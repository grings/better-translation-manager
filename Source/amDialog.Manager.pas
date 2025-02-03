unit amDialog.Manager;

(*
 * Copyright © 2019-2025 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

implementation

uses
  Generics.Collections,
  Classes,
  Forms,
  Controls,
  SysUtils,
  Windows,
  amDialog.Manager.API;

// -----------------------------------------------------------------------------
//
//      TDialogManagerFormWrapper
//
// -----------------------------------------------------------------------------
type
  TDialogManagerFormWrapper = class(TInterfacedObject, IVCLComObject)
  strict private type
    TComponentNotificationSink = class(TComponent)
    strict private
      FWrapper: TDialogManagerFormWrapper;
    protected
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    public
      constructor Create(AWrapper: TDialogManagerFormWrapper); reintroduce;
      destructor Destroy; override;
    end;
  private
    FDialogID: TGUID;
    FForm: TComponent;
    FNotificationSink: TComponentNotificationSink;
    FFormIsModal: boolean;
  protected
    property FormIsModal: boolean read FFormIsModal;
  protected
    // IVCLComObject
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
    procedure FreeOnRelease;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  public
    constructor Create(const ADialogID: TGUID; const AForm: TComponent; AFormIsModal: boolean = True);
    destructor Destroy; override;
  end;


// -----------------------------------------------------------------------------
//
//      TDialogManager
//
// -----------------------------------------------------------------------------
// Implements IDialogManager.
// -----------------------------------------------------------------------------
type
  TDialogManager = class(TInterfacedObject, IUnknown, IDialogManager)
  private
    FRegistry: TDictionary<TGUID, TComponentClass>;
    FInstances: TDictionary<TGUID, IUnknown>;
  private
    // IDialogManager
    procedure RegisterDialogClass(const DialogID: TGUID; DialogClass: TComponentClass);
    function CreateDialog(const DialogID: TGUID; AOwner: TComponent = nil; ARegisterDialog: boolean = False; AFormIsModal: boolean = True): IUnknown;
    procedure RegisterDialog(const DialogID: TGUID; Dialog: IUnknown);
    procedure UnregisterDialog(const DialogID: TGUID);
    function FindDialog(const DialogID: TGUID): IUnknown;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  FDialogManager: IDialogManager = nil;

// -----------------------------------------------------------------------------

constructor TDialogManager.Create;
begin
  inherited Create;
end;

destructor TDialogManager.Destroy;
begin
  FRegistry.Free;
  FInstances.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

function TDialogManager.FindDialog(const DialogID: TGUID): IUnknown;
begin
  if (FInstances = nil) or (not FInstances.TryGetValue(DialogID, Result)) then
    Result := nil;
end;

// -----------------------------------------------------------------------------

function TDialogManager.CreateDialog(const DialogID: TGUID; AOwner: TComponent; ARegisterDialog: boolean; AFormIsModal: boolean): IUnknown;
var
  DialogClass: TComponentClass;
  Form: TComponent;
  Wrapper: IVCLComObject;
begin
  if (FRegistry = nil) or (not FRegistry.TryGetValue(DialogID, DialogClass)) then
    raise Exception.CreateFmt('Dialog class not registered: %s', [DialogID.ToString]);

  Form := DialogClass.Create(AOwner);

  // Must create wrapper before retrieving interface or the wrapper
  // ref count will not be updated correctly.
  Wrapper := TDialogManagerFormWrapper.Create(DialogID, Form, AFormIsModal);

  if (not Supports(Form, DialogID, Result)) then
  begin
    if (not AFormIsModal) then
      // Must free explicitly as wrapper will not do it
      Form.Free;
    raise Exception.CreateFmt('Dialog class %s does not implement the %s interface', [DialogClass.ClassName, DialogID.ToString]);
  end;

  if (ARegisterDialog) then
    RegisterDialog(DialogID, Result);
end;

// -----------------------------------------------------------------------------

procedure TDialogManager.RegisterDialog(const DialogID: TGUID; Dialog: IUnknown);
begin
  if (FInstances = nil) then
    FInstances := TDictionary<TGUID, IUnknown>.Create;

  FInstances.Add(DialogID, Dialog);
end;

// -----------------------------------------------------------------------------

procedure TDialogManager.RegisterDialogClass(const DialogID: TGUID; DialogClass: TComponentClass);
begin
  if (FRegistry = nil) then
    FRegistry := TDictionary<TGUID, TComponentClass>.Create;
  FRegistry.Add(DialogID, DialogClass);
end;

// -----------------------------------------------------------------------------

procedure TDialogManager.UnregisterDialog(const DialogID: TGUID);
begin
  if (FInstances <> nil) then
    FInstances.Remove(DialogID);
end;


// -----------------------------------------------------------------------------
//
//      TDialogManagerFormWrapper
//
// -----------------------------------------------------------------------------
constructor TDialogManagerFormWrapper.Create(const ADialogID: TGUID; const AForm: TComponent; AFormIsModal: boolean);
begin
  inherited Create;
  FDialogID := ADialogID;
  FForm := AForm;
  FFormIsModal := AFormIsModal;

  // VCLComObject is a pointer so this will _AddRef the interface
  // but not _Release it when the pointer is nilled.
  FForm.VCLComObject := Self as IVCLComObject;

  // FNotificationSink is used to catch premature destruction of the wrapped form.
  // For example if the form is destroyed explicitly.
  FNotificationSink := TComponentNotificationSink.Create(Self);

  if (not FFormIsModal) then
    // Keep wrapper alive past last release. Wrapper is free'd when form is destroyed
    _AddRef;
end;

// -----------------------------------------------------------------------------

destructor TDialogManagerFormWrapper.Destroy;
begin
  FreeAndNil(FNotificationSink);

  if (FForm <> nil) then
  begin
    FForm.VCLComObject := nil;

    var SaveForm := FForm;
    FForm := nil;

    if (FDialogManager <> nil) then
      FDialogManager.UnregisterDialog(FDialogID);

    if (not(csDestroying in SaveForm.ComponentState)) then
      SaveForm.Free;
  end;

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TDialogManagerFormWrapper.FreeOnRelease;
begin
end;

function TDialogManagerFormWrapper.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := HResult($80000001);
end;

function TDialogManagerFormWrapper.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  Result := HResult($80000001);
end;

function TDialogManagerFormWrapper.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := HResult($80000001);
end;

function TDialogManagerFormWrapper.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
begin
  Result := HResult($80000001);
end;

function TDialogManagerFormWrapper.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;
begin
  Result := inherited;
end;

// -----------------------------------------------------------------------------

function TDialogManagerFormWrapper.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if (FForm = nil) then
    raise Exception.Create('Form wrapped by TDialogManagerFormWrapper has been prematurely destroyed');

  // Delegate interface casts to the form
  if FForm.GetInterface(IID, Obj) then
    Result := 0
  else
    Result := inherited;
end;


// -----------------------------------------------------------------------------
//
//      TDialogManagerFormWrapper.TComponentNotificationSink
//
// -----------------------------------------------------------------------------
constructor TDialogManagerFormWrapper.TComponentNotificationSink.Create(AWrapper: TDialogManagerFormWrapper);
begin
  inherited Create(nil);
  FWrapper := AWrapper;
  FWrapper.FForm.FreeNotification(Self);
end;

// -----------------------------------------------------------------------------

destructor TDialogManagerFormWrapper.TComponentNotificationSink.Destroy;
begin
  if (FWrapper.FForm <> nil) then
    FWrapper.FForm.RemoveFreeNotification(Self);
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TDialogManagerFormWrapper.TComponentNotificationSink.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FWrapper.FForm) and (Operation = opRemove) then
  begin
    FWrapper.FForm := nil;
    FDialogManager.UnregisterDialog(FWrapper.FDialogID);
    if (not FWrapper.FormIsModal) then
      // Make sure wrapper is destroyed when form is destroyed
      FWrapper._Release;
  end;
end;

// -----------------------------------------------------------------------------

initialization
  FDialogManager := TDialogManager.Create;
  RegisterDialogManager(FDialogManager);

finalization
  RegisterDialogManager(nil);
  FDialogManager := nil;

end.
