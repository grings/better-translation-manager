unit amDataCsvReader;

(*
 * Copyright © 2012 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  Classes,
  SysUtils,
  Types,
  amProgress.API;


// -----------------------------------------------------------------------------
//
// TCsvSettings
//
// -----------------------------------------------------------------------------
type
  TCsvSettings = record
    Codepage: integer;
    EscapeChar: Char;
    FirstRow: integer; // 1 based
    QuoteChar: Char;
    DelimiterChar: Char;
    DecimalSeparator: Char;

    class function Default: TCsvSettings; static;
  end;

// -----------------------------------------------------------------------------
//
// Exceptions
//
// -----------------------------------------------------------------------------
type
  ECsvReader = class(Exception);


// -----------------------------------------------------------------------------
//
//              TCsvReader
//
// -----------------------------------------------------------------------------
// CSV parsing functions.
// -----------------------------------------------------------------------------
// See:
// RFC4180
// Common Format and MIME Type for Comma-Separated Values (CSV) Files
// http://tools.ietf.org/html/rfc4180
// -----------------------------------------------------------------------------
type
  ICsvReaderRowTarget = interface
    ['{7ADFB41A-05A4-4E5A-BB96-89AE8D22F6AB}']
    // At the beginning of each row the PrepareRow method is called.
    function PrepareRow: boolean;
    // For each column value within a row the SaveValue method is called. The Column parameter is zero based.
    function SaveValue(Index: integer; const Value: string): boolean;
    // At the end of each row the FinalizeRow method is called.
    function FinalizeRow: boolean;
  end;

  TCsvReader = class abstract
  private
  protected
  public
    class procedure ReadFromStream(Stream: TStream; const Settings: TCsvSettings; const Target: ICsvReaderRowTarget; Progress: IProgress = nil; MaxRows: integer = -1);
    class procedure ReadFromFile(const Filename: string; const Settings: TCsvSettings; const Target: ICsvReaderRowTarget; Progress: IProgress = nil; MaxRows: integer = -1);

    // CSV parser
    class function ParseCsvRow(const Settings: TCsvSettings; Reader: TStreamReader; const Target: ICsvReaderRowTarget): boolean;
    class function ParseCsv(const Settings: TCsvSettings; Reader: TStreamReader; const Progress: IProgress; MaxRows: integer; const Target: ICsvReaderRowTarget): integer;
  end;


resourcestring
  sCsvReaderReadData = 'Import from delimited data';
  sCsvReaderReadFile = 'Import from delimited file';
  sCsvReaderFileOpen = 'Opening file';
  sCsvReaderPrepare = 'Analyzing data structure';
  sCsvReaderRead = 'Reading data';
  sCsvReaderImport = 'Importing data';


// -----------------------------------------------------------------------------
//
//              TCsvReaderRowTargetColumnCounter
//
// -----------------------------------------------------------------------------
// Count columns. Ignore actual values.
// -----------------------------------------------------------------------------
type
  TCsvReaderRowTargetColumnCounter = class(TInterfacedObject, ICsvReaderRowTarget)
  private
    FCount: integer;
  protected
    // ICsvReaderRowTarget
    function PrepareRow: boolean;
    function SaveValue(Index: integer; const Value: string): boolean;
    function FinalizeRow: boolean;
  public
    property Count: integer read FCount;
  end;

// -----------------------------------------------------------------------------
//
//              TCsvReaderStringArrayRowTarget
//
// -----------------------------------------------------------------------------
// Read row into array of strings
// -----------------------------------------------------------------------------
type
  TCsvReaderStringArrayRowTarget = class(TInterfacedObject, ICsvReaderRowTarget)
  private
    FValues: TStringDynArray;
    FCount: integer;
  protected
    // ICsvReaderRowTarget
    function PrepareRow: boolean;
    function SaveValue(Index: integer; const Value: string): boolean;
    function FinalizeRow: boolean;
  public
    property Values: TStringDynArray read FValues;
    property Count: integer read FCount;
  end;


// -----------------------------------------------------------------------------
//
//              TTextParserCSV
//
// -----------------------------------------------------------------------------
type
  TTextParserCSV = class
  private
    FReader: TStreamReader;
    FOwnedReader: TStreamReader;
    FOwnedStream: TStream;
    FTarget: ICsvReaderRowTarget;
    FSettings: TCsvSettings;
    FRowCount: integer;
    FNeedData: boolean;
    FHasData: boolean;
  protected
    procedure EnsureData;
    function GetEndOfData: boolean;
  public
    constructor Create(const ASettings: TCsvSettings; AReader: TStreamReader); overload;
    constructor Create(const ASettings: TCsvSettings; AStream: TStream; AEncoding: TEncoding = nil; AOwnsStream: boolean = False); overload;
    destructor Destroy; override;

    property Reader: TStreamReader read FReader;
    property Target: ICsvReaderRowTarget read FTarget;
    property Settings: TCsvSettings read FSettings;
    property EndOfData: boolean read GetEndOfData;
    property RowCount: integer read FRowCount;

    function ReadRow: TStringDynArray;
  end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------


implementation

uses
{$ifOPT D+}
  Diagnostics,
{$endif}
  StrUtils,
  Windows,
  Controls,
  amProgress.Stream,
  amCursorService;

const
  // Line break character.
  // #10, #13, and #13#10 in the source CSV is converted to this character
  CsvLineBreak: Char = #13;

// -----------------------------------------------------------------------------
//
//              TCsvSettings
//
// -----------------------------------------------------------------------------
class function TCsvSettings.Default: TCsvSettings;
begin
  Result.Codepage := GetACP;
  Result.EscapeChar := '\';
  Result.FirstRow := 1;
  Result.QuoteChar := '"';
  Result.DelimiterChar := ';';
  Result.DecimalSeparator := SysUtils.FormatSettings.DecimalSeparator;
end;

// -----------------------------------------------------------------------------
//
//              TCsvReader
//
// -----------------------------------------------------------------------------
class procedure TCsvReader.ReadFromFile(const Filename: string; const Settings: TCsvSettings; const Target: ICsvReaderRowTarget; Progress: IProgress; MaxRows: integer);
var
  Stream: TStream;
begin
  if (Progress = nil) then
  begin
    Progress := ShowProgress(sCsvReaderReadData);
    Progress.EnableAbort := True;
    Progress.RaiseOnAbort := True;
  end;

  Progress.Progress(psBegin, 0, 1, sCsvReaderFileOpen);

  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try

    ReadFromStream(Stream, Settings, Target, Progress, MaxRows);

  finally
    Stream.Free;
  end;
end;

// -----------------------------------------------------------------------------

class procedure TCsvReader.ReadFromStream(Stream: TStream; const Settings: TCsvSettings; const Target: ICsvReaderRowTarget; Progress: IProgress; MaxRows: integer);
var
  Encoding: TEncoding;
  Reader: TStreamReader;
  ProgressStream: TStream;
begin
  ASSERT(Stream <> nil);

  SaveCursor(crAppStart);

  if (Progress = nil) then
  begin
    Progress := ShowProgress(sCsvReaderReadData);
    Progress.EnableAbort := True;
    Progress.RaiseOnAbort := True;
  end;

  Progress.UpdateMessage(sCsvReaderPrepare);

  Encoding := TEncoding.GetEncoding(Settings.Codepage);
  try
    Stream.Position := 0;
    ProgressStream := TProgressStream.Create(Stream, Progress);
    try
      Reader := TStreamReader.Create(ProgressStream, Encoding, False);
      try
        try

          ParseCsv(Settings, Reader, Progress, MaxRows, Target);

        except
          // Swallow Abort so it doesn't propagate. It's done its job at this point.
          on E: EAbort do
            Exit;
        end;
      finally
        Reader.Free;
      end;
    finally
      ProgressStream.Free;
    end;
  finally
    Encoding.Free;
  end;
end;

// -----------------------------------------------------------------------------

class function TCsvReader.ParseCsv(const Settings: TCsvSettings; Reader: TStreamReader; const Progress: IProgress; MaxRows: integer; const Target: ICsvReaderRowTarget): integer;
var
  MaxProgress, CurrentProgress: integer;
begin
  if (MaxRows = -1) then
    MaxProgress := Reader.BaseStream.Size
  else
    MaxProgress := MaxRows;

  if (Progress <> nil) then
    Progress.UpdateMessage(sCsvReaderImport);

  // See ParseCsvRow for an explanation of why we MUST make a bogus call to EndOfStream.
  Reader.EndOfStream;

  // Iterate to load all rows
  Result := 1;
  while (not Reader.EndOfStream) do
  begin
    if (MaxRows = -1) then
      CurrentProgress := Reader.BaseStream.Position
    else
      CurrentProgress := Result;

    if (Progress <> nil) then
      Progress.Progress(psProgress, CurrentProgress, MaxProgress, sCsvReaderImport);

    // Load row
    if (Result >= Settings.FirstRow) then
    begin
      if (not ParseCsvRow(Settings, Reader, Target)) then
        break;
    end else
      // Skip line by parsing it instead of using Reader.ReadLine because we still need to handle different variations of CR/LF
      ParseCsvRow(Settings, Reader, nil);

    Inc(Result);
  end;
  Dec(Result);

  if (Progress <> nil) then
    Progress.Progress(psEnd, MaxProgress, MaxProgress, sCsvReaderImport);
end;

// -----------------------------------------------------------------------------

class function TCsvReader.ParseCsvRow(const Settings: TCsvSettings; Reader: TStreamReader; const Target: ICsvReaderRowTarget): boolean;
const
  GrowBuffer = 256;
var
  Buffer: string;
  Column: integer;
  Count: integer;
  Quoted: boolean;
  Escaped: boolean;
  BufChar: Char;
  EOL: boolean;
  EOF: boolean;

  procedure StuffChar(Value: Char);
  begin
    Inc(Count);
    if (Count >= Length(Buffer)) then
      SetLength(Buffer, Count + GrowBuffer);
    Buffer[Count] := Value;
  end;

begin
  Column := 0;
  Count := 0;
  EOL := False;
  EOF := False;

  if (Target <> nil) then
  begin
    Result := Target.PrepareRow;
    if (not Result) then
      exit;
  end;

  // If stream contains data, but the data cannot be decoded with the specified
  // encoding, the first call to EndOfStream will read all data but return False
  // (since data was actually read) with an empty buffer. The next call to
  // EndOfStream will return True since the buffer is empty and we have already
  // read all data.
  Reader.EndOfStream;

  // Iterate to build column values
  while (not Reader.EndOfStream) and (not EOL) and (not EOF) do
  begin
    Quoted := False;
    Escaped := False;

    // Iterate to build single value
    while (not Reader.EndOfStream) do
    begin
      BufChar := Char(Reader.Read);

      if (BufChar = #0) then
      begin
        // End row
        EOF := True;
        break;
      end;

      if (Escaped) then
      begin
        // Save current regardless of content
        StuffChar(BufChar);
        Escaped := False;
        continue;
      end;

      // Quote handling
      // Not in quote mode:
      // - A quote at start of field starts quote mode.
      // - A quote not at start of field is an error.
      // In quote mode:
      // - Double-quote emits a quote.
      // - Single-quote ends quote mode.
      if (BufChar = Settings.QuoteChar) and ((Count = 0) or (Quoted)) then
      begin
        // Two consecutive quote chars outputs a single quote char.
        // However, since double-quote is only allowed when the field is quoted,
        // a double-quote at the start is actually two single-quotes; I.e. an
        // empty, quoted field.
        if (Count > 0) and (Quoted) and (Char(Reader.Peek) = Settings.QuoteChar) then
        begin
          // Skip next quote char and save current
          StuffChar(BufChar);
          Reader.Read;
        end else
          // Toggle quote state and skip quote char
          Quoted := not Quoted;

        continue;
      end;

      // Even though it's illogical (IMO) the escape char must be processed even when quoted
      if (BufChar = Settings.EscapeChar) then
      begin
        // Enter escape state and skip escape char
        Escaped := True;
        continue;
      end;

      if (not Quoted) then
      begin
        if (BufChar = Settings.DelimiterChar) then
          break;
      end;

      if (CharInSet(BufChar, [#10, #13])) then
      begin
        // Ignore LF after CR
        if (BufChar = #13) and (Reader.Peek = 10) then
          Reader.Read;

        if (not Quoted) then
        begin
          // End row
          EOL := True;
          break;
        end else
          StuffChar(CsvLineBreak);
      end else
        StuffChar(BufChar);
    end;

    // Save value
    if (not EOF) or (Count > 0) then
    begin
      if (Target <> nil) and (not Target.SaveValue(Column, Copy(Buffer, 1, Count))) then
      begin
        Count := 0;
        break;
      end;
      Count := 0;
      inc(Column);
    end;
  end;

  ASSERT(Count = 0);

  if (Target <> nil) and (Column > 0) then
    Result := Target.FinalizeRow
  else
    Result := (Column > 0);
end;


// -----------------------------------------------------------------------------
//
//              TCsvReaderRowTargetColumnCounter
//
// -----------------------------------------------------------------------------
function TCsvReaderRowTargetColumnCounter.FinalizeRow: boolean;
begin
  Result := (FCount > 0);
end;

// -----------------------------------------------------------------------------

function TCsvReaderRowTargetColumnCounter.PrepareRow: boolean;
begin
  Result := (FCount = 0);
end;

// -----------------------------------------------------------------------------

function TCsvReaderRowTargetColumnCounter.SaveValue(Index: integer; const Value: string): boolean;
begin
  Result := True;
  ASSERT(FCount = Index);
  inc(FCount);
end;


// -----------------------------------------------------------------------------
//
//              TCsvReaderStringArrayRowTarget
//
// -----------------------------------------------------------------------------
function TCsvReaderStringArrayRowTarget.FinalizeRow: boolean;
begin
  Result := True;
end;

function TCsvReaderStringArrayRowTarget.PrepareRow: boolean;
var
  i: integer;
begin
  FCount := 0;

  for i := 0 to Length(FValues)-1 do
    FValues[i] := '';

  Result := True;
end;

function TCsvReaderStringArrayRowTarget.SaveValue(Index: integer; const Value: string): boolean;
begin
  if (Index >= FCount) then
    FCount := Index+1;

  if (Index >= Length(FValues)) then
    SetLength(FValues, Length(FValues)+8);

  FValues[Index] := Value;

  Result := True;
end;


// -----------------------------------------------------------------------------
//
//              TTextParserCSV
//
// -----------------------------------------------------------------------------
constructor TTextParserCSV.Create(const ASettings: TCsvSettings; AReader: TStreamReader);
begin
  inherited Create;

  FReader := AReader;
  FTarget := TCsvReaderStringArrayRowTarget.Create;
  FSettings := ASettings;

  // See TCsvReader.ParseCsvRow for an explanation of why we MUST make a bogus call to EndOfStream.
  FReader.EndOfStream;

  FNeedData := True;
end;

constructor TTextParserCSV.Create(const ASettings: TCsvSettings; AStream: TStream; AEncoding: TEncoding; AOwnsStream: boolean);
begin
  if (AOwnsStream) then
    FOwnedStream := AStream;

  if (AEncoding <> nil) then
    FOwnedReader := TStreamReader.Create(AStream, AEncoding)
  else
    FOwnedReader := TStreamReader.Create(AStream);

  Create(ASettings, FOwnedReader);
end;

destructor TTextParserCSV.Destroy;
begin
  FOwnedStream.Free;
  FOwnedReader.Free;
  FTarget := nil;
  inherited;
end;

procedure TTextParserCSV.EnsureData;
begin
  if (FNeedData) then
  begin
    FHasData := TCsvReader.ParseCsvRow(FSettings, FReader, FTarget);
    FNeedData := (not FHasData);
  end;
end;

function TTextParserCSV.GetEndOfData: boolean;
begin
  EnsureData;
  Result := (not FHasData);
end;

function TTextParserCSV.ReadRow: TStringDynArray;
begin
  EnsureData;
  if (FHasData) then
  begin
    Result := TCsvReaderStringArrayRowTarget(FTarget).Values;
    SetLength(Result, TCsvReaderStringArrayRowTarget(FTarget).Count);

    FNeedData := True;
    Inc(FRowCount);
  end else
    SetLength(Result, 0);
end;

// -----------------------------------------------------------------------------

end.
