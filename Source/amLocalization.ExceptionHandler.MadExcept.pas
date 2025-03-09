unit amLocalization.ExceptionHandler.MadExcept;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

{$ifndef MADEXCEPT}
{$message Fatal 'This unit requires that the MADEXCEPT symbol is defined'}
{$endif}
{$WARN SYMBOL_PLATFORM OFF}

interface

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  System.SysUtils,
  System.Classes,
  Generics.Collections,
  Windows,
  Forms,
  StrUtils,
  IOUtils,

  madExcept,
  madListModules,

  amLocalization.ExceptionHandler.API;

var
  sBugreportMailAddress: string = 'anders@melander.dk'; // Only for test

resourcestring
  sBugreportSendButton = 'Send bug report';
  sBugreportShowButton = 'View details';
  sBugreportCloseButton = 'Quit application';
  sBugreportContinueButton = 'Continue application';
  sBugreportMessage = 'The application encountered a problem.'+#13#13+
    'To help us diagnose and correct the problem, you can send us a bug report.'+#13#13+
    'If you choose to continue the application, you should save your work as soon as possible and restart the application.'+#13;
  sBugreportMailSubject = 'Defect report - %s %s';
  sBugreportMailMessage = 'To help us locate and correct the cause of the problem please describe, in as much details as possible, what you were doing when the problem occurred:%lf%%lf%%lf%'+
    '-- %lf%'+
    'Error message: %exceptMsg%%lf%'+
    'The generated bug report has been attached to this email.';

// -----------------------------------------------------------------------------

function BoolToStr(Value: boolean): string;
begin
  if Value then
    Result := 'true'
  else
    Result := 'false';
end;

function StrToBool(const s: string): boolean;
begin
  // XML standard: 1, 0, true, false. However... MSXML represents True as -1.
  if (s = '1') or (s = '-1') or (AnsiSameText(s, 'true')) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------
//
//              TMadExceptInfoConsumer
//
// -----------------------------------------------------------------------------
type
  TMadExceptInfoConsumer = class(TInterfacedObject, IExceptionInfoConsumer)
  private
    FExceptIntf: IMEException;
  private
    // IExceptionInfoConsumer
    procedure AddExceptionInfo(const Section, Name, Value: string);
  public
    constructor Create(const AExceptIntf: IMEException);
  end;

constructor TMadExceptInfoConsumer.Create(const AExceptIntf: IMEException);
begin
  inherited Create;
  FExceptIntf := AExceptIntf;
end;

procedure TMadExceptInfoConsumer.AddExceptionInfo(const Section, Name, Value: string);
begin
  if (Section <> '') then
  begin
    var Index := FExceptIntf.BugReportSections.FindItem(Section);
    if (Index <> -1) then
    begin
      var s := FExceptIntf.BugReportSections.Contents[Section];
      if (s <> '') then
        s := s + #13#10;
      s := s + Name + #9 + Value;
      FExceptIntf.BugReportSections.Contents[Section] := s;
    end else
      FExceptIntf.BugReportSections.Add(Section, Name+#9+Value);
  end else
    FExceptIntf.BugReportHeader.Add(Name, Value);
end;

// -----------------------------------------------------------------------------
//
//              TMadExceptExceptionHandler
//
// -----------------------------------------------------------------------------
type
  TMadExceptExceptionHandler = class(TInterfacedObject, IExceptionHandler)
  private
    FExceptionCount: integer;
    FExceptionInfoProviders: TList<IExceptionInfoProvider>;

  private
    procedure Trace(const Topic, Msg: string);

  protected // "protected" to shut compiler up about unused private methods
    // Registered exception handlers
    procedure MadExceptGlobalHandler(const exceptIntf: IMEException; var handled: boolean);
    procedure MadExceptHandlerSynced(const exceptIntf: IMEException; var handled: boolean);
    procedure MadExceptHandler(const exceptIntf: IMEException; var handled: boolean);

  private
    // IExceptionHandler
    procedure ExceptionHandler(const ExceptIntf: IUnknown; var Handled: boolean);
    procedure RegisterExceptionInfoProvider(const ExceptionInfoProvider: IExceptionInfoProvider);
    procedure UnregisterExceptionInfoProvider(const ExceptionInfoProvider: IExceptionInfoProvider);

  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TMadExceptExceptionHandler.Create;
begin
  inherited Create;

   // Note: This is called both before and after resource strings has been localized ... huh?
  MESettings.SendBtnCaption := sBugreportSendButton;
  MESettings.ShowBtnCaption := sBugreportShowButton;
  MESettings.CloseBtnCaption := sBugreportCloseButton;
  MESettings.ContinueBtnCaption := sBugreportContinueButton;
  MESettings.ExceptMsg := sBugreportMessage;
  MESettings.MailSubject := sBugreportMailSubject;
  MESettings.MailBody := sBugreportMailMessage;
  MESettings.MailAddr := sBugreportMailAddress;
  MESettings.MailViaMapi := True;
  MESettings.MailViaMailto := True;

  RegisterExceptionHandler(MadExceptHandler, stDontSync);
end;

destructor TMadExceptExceptionHandler.Destroy;
begin
  UnregisterExceptionHandler(MadExceptHandler);
  inherited;
end;

procedure TMadExceptExceptionHandler.ExceptionHandler(const ExceptIntf: IInterface; var Handled: boolean);
begin
  MadExceptHandler(ExceptIntf as IMEException, Handled);
end;

procedure TMadExceptExceptionHandler.MadExceptGlobalHandler(const exceptIntf: IMEException; var handled: boolean);
begin
//  Trace('Exception (handled)', exceptIntf.ExceptClass+':'+exceptIntf.ExceptMessage);
  Handled := True;
end;

procedure TMadExceptExceptionHandler.MadExceptHandler(const ExceptIntf: IMEException; var Handled: boolean);

  function GetFileVersion(const AFileName: string): string;
  var
    FileName: string;
    InfoSize, Wnd: DWORD;
    VerBuf: Pointer;
    FI: PVSFixedFileInfo;
    VerSize: DWORD;
  begin
    // GetFileVersionInfo modifies the filename parameter data while parsing.
    // Copy the string const into a local variable to create a writeable copy.
    FileName := AFileName;
    UniqueString(FileName);

    InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);

    if (InfoSize <> 0) then
    begin
      GetMem(VerBuf, InfoSize);
      try
        if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
          if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
          begin
            Result := Format('%d.%d.%d.%d', [HiWord(FI.dwFileVersionMS), LoWord(FI.dwFileVersionMS), HiWord(FI.dwFileVersionLS), LoWord(FI.dwFileVersionLS)]);
            Exit;
          end;
      finally
        FreeMem(VerBuf);
      end;
    end;

    Result := '(unknown)';
  end;

  function DeletePrefix(const Prefix, Value: string): string;
  begin
    Result := Value;
    if (StartsStr(Prefix, Result)) then
      Delete(Result, 1, Length(Prefix));
  end;

var
  s: string;
{$ifdef MADEXCEPT_POSTBUGREPORT}
  ExceptionEx: IMEExceptionEx;
  SaveMessage, SaveSubject: string;
{$endif MADEXCEPT_POSTBUGREPORT}
  ExceptionSettings: IMESettings;
begin
  // Ignore frozen app when debugging
  if (ExceptIntf.ExceptType = etFrozen) and (DebugHook <> 0) then
  begin
    Handled := True;
    exit;
  end;

  if (FExceptionCount > 0) and (Supports(ExceptIntf, IMESettings, ExceptionSettings)) then
  begin
    // Only enable "Send" button the first time an exception occurs. We are not interested in secondary errors.
    ExceptionSettings.SendBtnVisible := False;
  end;
  Inc(FExceptionCount);

  (*
  ** Exception reporting strategy
  **
  ** 1) Post report via HTTP.
  **
  ** 2a) If #1 succeedes, hide "Send report" button and display dialog.
  **
  ** 2b) If #1 fails, display dialog, hope user clicks "Send report" button.
  **
  *)

  (*
  ** Email setup
  *)
  s := Application.Title;
  if (s = '') then
    s := TPath.GetFileName(ParamStr(0));
  ExceptIntf.MailSubject := Format(ExceptIntf.MailSubject, [s, GetFileVersion(ParamStr(0))]);

  Trace('Exception', ExceptIntf.ExceptClass+':'+ExceptIntf.ExceptMessage);

  if (FExceptionInfoProviders <> nil) and (FExceptionInfoProviders.Count > 0) then
  begin
    var ExceptionInfoConsumer: IExceptionInfoConsumer;
    ExceptionInfoConsumer := TMadExceptInfoConsumer.Create(ExceptIntf);

    for var ExceptionInfoProvider in FExceptionInfoProviders do
      ExceptionInfoProvider.GetExceptionInfo(ExceptIntf, ExceptionInfoConsumer);
  end;

  (*
  ** Attempt to post bug report directly via HTTP
  *)
{$ifdef MADEXCEPT_POSTBUGREPORT}
  if (Supports(ExceptIntf, IMESettings, ExceptionSettings)) then
  begin
    // Temporarily disable mail options
    ExceptionSettings.MailViaMapi := False;
    ExceptionSettings.MailViaMailto := False;
    ExceptionSettings.UploadToCustomScript := True;

    // Use a more focused message when posting since user is not involved.
    // This message is only seen in the final bug report system (unless it's altered on the way)
    SaveSubject := ExceptIntf.MailSubject;
    SaveMessage := ExceptIntf.MailBody;
    ExceptIntf.MailSubject := ExceptIntf.ExceptMessage;
    ExceptIntf.MailBody := SaveSubject + #13#10#13#10 + ExceptIntf.ExceptMessage + #13#10#13#10 + 'This is an automated incident report.';

    ExceptIntf.SendBugReport();

    if (Supports(ExceptIntf, IMEExceptionEx, ExceptionEx)) and (ExceptionEx.GetMailWasSent) then
    begin
      // Bug report was successfully sent. Remove the send button and alter the message presented to the user so
      // they're not prompted to send the report.
      ExceptionSettings.SendBtnVisible := False;
      ExceptionSettings.ExceptMsg := sBugreportMessage2;
    end else
    begin
      // Restore original mail. Now we would like the user to get involved.
      ExceptIntf.MailSubject := SaveSubject;
      ExceptIntf.MailBody := SaveMessage;

      // Display Send button
      ExceptionSettings.SendBtnVisible := True;

      // Enable mail options again
      ExceptionSettings.MailViaMapi := True;
      ExceptionSettings.MailViaMailto := True;
    end;
  end;
{$endif MADEXCEPT_POSTBUGREPORT}

(*
  Attachments := ExceptIntf.AdditionalAttachments;
  Fields := MadExcept.NewFields;
  Fields.Add("MailSubject", ExceptIntf.MailSubject);
  Fields.Add("MailBody", ExceptIntf.MailBody);
  Fields.Add("MailFrom", ExceptIntf.MailFrom);

  MadExcept.HttpUpload(MadExcept.MESettings().HttpServer, MadExcept.MESettings().HttpSsl, MadExcept.MESettings().HttpPort,
    MadExcept.MESettings().HttpAccount, MadExcept.MESettings().HttpPassword,
    Attachments, Fields);
*)
end;

procedure TMadExceptExceptionHandler.MadExceptHandlerSynced(const exceptIntf: IMEException; var handled: boolean);
begin

end;

procedure TMadExceptExceptionHandler.RegisterExceptionInfoProvider(const ExceptionInfoProvider: IExceptionInfoProvider);
begin
  if (FExceptionInfoProviders = nil) then
    FExceptionInfoProviders := TList<IExceptionInfoProvider>.Create;

  FExceptionInfoProviders.Add(ExceptionInfoProvider);
end;

procedure TMadExceptExceptionHandler.UnregisterExceptionInfoProvider(const ExceptionInfoProvider: IExceptionInfoProvider);
begin
  if (FExceptionInfoProviders <> nil) then
    FExceptionInfoProviders.Remove(ExceptionInfoProvider);
end;

procedure TMadExceptExceptionHandler.Trace(const Topic, Msg: string);
begin

end;

// -----------------------------------------------------------------------------

function CreateMadExceptExceptionHandler: IExceptionHandler;
begin
  Result := TMadExceptExceptionHandler.Create;
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

initialization
  RegisterExceptionHandlerFactory(CreateMadExceptExceptionHandler);
  // Only report leaks in debug builds
{$ifdef RELEASE}
  MadExcept.StopLeakChecking(False);
  MadExcept.ClearLeaks(False);
{$endif RELEASE}
end.
