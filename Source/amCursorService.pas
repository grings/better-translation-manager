unit amCursorService;

(*
 * Copyright © 2008 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  Controls,
  Classes;

//------------------------------------------------------------------------------
//
//      ICursorService
//
//------------------------------------------------------------------------------
type
  ICursorService = interface
    ['{9866F659-1FC2-4B35-86DE-8429B2F4142E}']
    function RegisterCursor(const AResourceID: UnicodeString; ADefault: TCursor = crArrow): TCursor;
  end;

function CursorService: ICursorService;


//------------------------------------------------------------------------------
//
//      ICursorRecall
//
//------------------------------------------------------------------------------
type
  ICursorRecall = interface
    procedure Store;
    procedure Forget;
  end;

  TSaveCursor = class abstract(TInterfacedObject, ICursorRecall)
  private
    procedure Store; virtual; abstract;
    procedure Forget; virtual; abstract;
  end;


//------------------------------------------------------------------------------
//
//      SaveCursor
//
//------------------------------------------------------------------------------
// Save current cursor and, optionally, set a new cursor.
// Returns a reference counted interface.
// Most common usage is to simply call SaveCursor, ignore the returned value,
// and let the automatic interface reference counting handle the cleanup:
//
//   begin
//     SaveCursor(crHourglass);
//     ...do some stuff...
//   end; // cursor is automatically restored here
//
//------------------------------------------------------------------------------
function SaveCursor(NewCursor: TCursor = crNone; ImmediateUpdate: boolean = False): ICursorRecall;
function SaveCursorScoped(NewCursor: TCursor = crNone; ImmediateUpdate: boolean = False): TSaveCursor;

// Use UpdateCursor to force immediate update of cursor
procedure UpdateCursor;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Generics.Collections,
  Windows,
  Forms,
  SysUtils;

//------------------------------------------------------------------------------

procedure UpdateCursor;
var
  p: TPoint;
begin
  GetCursorPos(p);
  SetCursorPos(p.X, p.Y);
end;

//------------------------------------------------------------------------------

type
  TCustomCursor = class
  public
    Name: string;
    Index: integer;
    Handle: HCursor;
  end;

var
  FCursorService: ICursorService = nil;

//------------------------------------------------------------------------------
//
//      TCursorRecall
//
//------------------------------------------------------------------------------
type
  TCursorRecall = class(TInterfacedObject, ICursorRecall)
  strict private
    class var [weak] FCurrentCursor: ICursorRecall; // Cursor stack head
  strict private
    FSavedCursor: TCursor;
    FPreviousCursor: ICursorRecall; // Cursor stack link to previous
  public
    constructor Create(NewCursor: TCursor = crNone; ImmediateUpdate: boolean = False);
    destructor Destroy; override;
    procedure Store;
    procedure Forget;
    property SavedCursor: TCursor read FSavedCursor;
  end;

constructor TCursorRecall.Create(NewCursor: TCursor; ImmediateUpdate: boolean);
begin
  inherited Create;

  // Maintain stack of cursors to ensure that we recall the cursors in the
  // correct order: The recall order should be the reverse of the store order.
  // Otherwise we can end up with an order like this:
  //   A.Store: crDefault->crHourGlass
  //   B.Store: crHourGlass->crHourGlass
  //   A.Recall: ->crDefault
  //   B.Recall: ->crHourGlass
  FPreviousCursor := FCurrentCursor;
  FCurrentCursor := Self;

  Store;

  if (NewCursor <> crNone) then
  begin
    // Reset Screen.Cursor if it has gone out of sync with Windows' cursor.
    // Otherwise setting Screen.Cursor might have no effect.
    if (Screen.Cursor = NewCursor) and (Windows.GetCursor <> Screen.Cursors[Screen.Cursor]) then
      Screen.Cursor := crNone;

    Screen.Cursor := NewCursor;
    if (ImmediateUpdate) then
      UpdateCursor;
  end;
end;

destructor TCursorRecall.Destroy;
begin
  if (FSavedCursor <> crNone) then
    Screen.Cursor := FSavedCursor;

  // Pop from stack if we are the last (i.e current) entry
  if (TCursorRecall(FCurrentCursor) = Self) then
    FCurrentCursor := FPreviousCursor;

  inherited Destroy;
end;

procedure TCursorRecall.Forget;
begin
  FSavedCursor := crNone;
end;

procedure TCursorRecall.Store;
begin
  FSavedCursor := Screen.Cursor;
end;

function SaveCursor(NewCursor: TCursor; ImmediateUpdate: boolean): ICursorRecall;
begin
  Result := TCursorRecall.Create(NewCursor, ImmediateUpdate);
end;

type
  TSaveCursorDelegate = class(TSaveCursor)
  private
    FCursorRecall: ICursorRecall;
    procedure Store; override;
    procedure Forget; override;
  public
    constructor Create(NewCursor: TCursor; ImmediateUpdate: boolean);
  end;

constructor TSaveCursorDelegate.Create(NewCursor: TCursor; ImmediateUpdate: boolean);
begin
  inherited Create;
  FCursorRecall := SaveCursor(NewCursor, ImmediateUpdate);
end;

procedure TSaveCursorDelegate.Store;
begin
  FCursorRecall.Store;
end;

procedure TSaveCursorDelegate.Forget;
begin
  FCursorRecall.Forget;
end;

function SaveCursorScoped(NewCursor: TCursor; ImmediateUpdate: boolean): TSaveCursor;
begin
  Result := TSaveCursorDelegate.Create(NewCursor, ImmediateUpdate);
end;


//------------------------------------------------------------------------------
//
//      TCursorService / ICursorService
//
//------------------------------------------------------------------------------
type
  TCursorService = class(TInterfacedObject, ICursorService)
  strict private
  private
    FCursors: TObjectList<TCustomCursor>;
  protected
    // ICursorService
    function RegisterCursor(const AResourceID: UnicodeString; ADefault: TCursor): TCursor;

    procedure ClearCursors;
  public
    constructor Create;
    destructor Destroy; override;
  end;

//------------------------------------------------------------------------------

procedure TCursorService.ClearCursors;
var
  i: integer;
  CustomCursor: TCustomCursor;
  DefaultCursor: HCursor;
begin
  DefaultCursor := Screen.Cursors[crDefault];

  for i := 0 to FCursors.Count-1 do
  begin
    CustomCursor := FCursors[i];

    if (CustomCursor.Handle <> 0) and (CustomCursor.Index <> 0) and
      (Screen.Cursors[CustomCursor.Index] = CustomCursor.Handle) then
      Screen.Cursors[CustomCursor.Index] := DefaultCursor;
  end;

  FCursors.Clear;
end;

//------------------------------------------------------------------------------

constructor TCursorService.Create;
begin
  inherited Create;
  FCursors := TObjectList<TCustomCursor>.Create(True);
end;

//------------------------------------------------------------------------------

destructor TCursorService.Destroy;
begin
  ClearCursors;

  FreeAndNil(FCursors);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TCursorService.RegisterCursor(const AResourceID: UnicodeString; ADefault: TCursor): TCursor;

  function ResourceIdentToString(Ident: PWideChar): UnicodeString;
  begin
    // Ordinal resource idents start with $FFFF. This is safe since no Unicode
    // character starts with $FFFF.
    if (PWord(Ident)^ = $FFFF) then
    begin
      Setlength(Result, 2);
      Result[1] := Ident^;
      Inc(Ident);
      Result[2] := Ident^;
    end else
      Result := Ident;
  end;

var
  i: integer;
  Ident: string;
  CustomCursor: TCustomCursor;
  DefaultCursor: HCursor;
  NewCursor: HCursor;
begin
  Ident := ResourceIdentToString(PWideChar(AResourceID));

  i := FCursors.Count-1;

  CustomCursor := nil; // To silence bogus compiler warning
  while (i >= 0) do
  begin
    CustomCursor := FCursors[i];
    if (SameText(CustomCursor.Name, Ident)) then
      break;
    dec(i);
  end;

  if (i = -1) then
  begin
    CustomCursor := TCustomCursor.Create;
    FCursors.Add(CustomCursor);
    CustomCursor.Name := Ident;
  end;

  if (CustomCursor.Handle = 0) or (CustomCursor.Index = 0) or
    (Screen.Cursors[CustomCursor.Index] <> CustomCursor.Handle) then
  begin
    DefaultCursor := Screen.Cursors[crDefault];
    CustomCursor.Index := 1;
    // Find first unused entry
    while (Screen.Cursors[CustomCursor.Index] <> DefaultCursor) do
      inc(CustomCursor.Index);
//    CustomCursor.Handle := LoadCursor(HInstance, AResourceID);
//    CustomCursor.Handle := LoadImageW(HInstance, PWideChar(AResourceID), IMAGE_CURSOR, 0, 0, LR_DEFAULTSIZE or LR_SHARED);
    NewCursor := LoadImageW(HInstance, PWideChar(AResourceID), IMAGE_CURSOR, 0, 0, LR_DEFAULTSIZE);

    if (NewCursor <> 0) then
    begin
      CustomCursor.Handle := NewCursor;
      Screen.Cursors[CustomCursor.Index] := NewCursor;
      Result := CustomCursor.Index;
    end else
    begin
      // Fall back to default cursor (crArrow by default)
      if (CustomCursor.Handle = 0) then
        FCursors.Remove(CustomCursor);
      Result := ADefault;
    end;
  end else
    Result := CustomCursor.Index;
end;

//------------------------------------------------------------------------------

function CursorService: ICursorService;
begin
  if (FCursorService = nil) then
    FCursorService := TCursorService.Create;
  Result := FCursorService;
end;

//------------------------------------------------------------------------------

initialization
finalization
  FCursorService := nil;
end.
