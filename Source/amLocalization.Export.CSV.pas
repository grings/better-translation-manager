unit amLocalization.Export.CSV;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  Classes,
  amLocalization.Model;

// -----------------------------------------------------------------------------
//
// TLocalizerCsvWriter
//
// -----------------------------------------------------------------------------
// Aims to write IETF RFC4180 compliant CSV
// https://tools.ietf.org/html/rfc4180
// Exceptions:
// - Uses system default list separator instead of the required comma.
// -----------------------------------------------------------------------------
type
  TLocalizerCsvWriter = class
  private
    FWriter: TTextWriter;
    FOnlyTranslated: boolean;
    FLanguage: TTranslationLanguage;
    FDelimiter: string;
    FTerminator: string;
    FQuote: string;
    FQuoteRow: boolean;
    FAlwaysQuote: boolean;
    FHeader: boolean;
    FNeedHeader: boolean;
    FRowBuffer: TArray<string>;
    FFieldCount: integer;
  protected
    function NeedQuotes(const Value: string): boolean;
    function CsvEscape(const Value: string): string;
    procedure WriteField(const Value: string);
    procedure BeginRow(Project: TLocalizerProject);
    procedure EndRow;
    procedure WriteHeader(AProject: TLocalizerProject);
  public
    constructor Create(AWriter: TTextWriter; AOnlyTranslated: boolean = True; ALanguage: TTranslationLanguage = nil);
    destructor Destroy; override;

    procedure Write(AProject: TLocalizerProject); overload;
    procedure Write(AModule: TLocalizerModule); overload;
    procedure Write(AProp: TLocalizerProperty); overload;

    property Header: boolean read FHeader write FHeader;
    property Delimiter: string read FDelimiter write FDelimiter;
    property Terminator: string read FTerminator write FTerminator;
    property Quote: string read FQuote write FQuote;
    property AlwaysQuote: boolean read FAlwaysQuote write FAlwaysQuote;
  end;

implementation

uses
  SysUtils,
  Math,
  Windows,
  amLanguageInfo;

// -----------------------------------------------------------------------------
//
// TLocalizerCsvWriter
//
// -----------------------------------------------------------------------------
constructor TLocalizerCsvWriter.Create(AWriter: TTextWriter; AOnlyTranslated: boolean; ALanguage: TTranslationLanguage);
begin
  inherited Create;
  FWriter := AWriter;
  FOnlyTranslated := AOnlyTranslated;
  FLanguage := ALanguage;

  FDelimiter := FormatSettings.ListSeparator;
  FTerminator := sLineBreak;
  FQuote := '"';

  FHeader := True;
  FNeedHeader := True;

  SetLength(FRowBuffer, 8);
end;

destructor TLocalizerCsvWriter.Destroy;
begin

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerCsvWriter.BeginRow(Project: TLocalizerProject);
begin
  if (FHeader) and (FNeedHeader) then
    WriteHeader(Project);

  FQuoteRow := FAlwaysQuote;
  FFieldCount := 0;
end;

procedure TLocalizerCsvWriter.EndRow;
begin
(*
  RFC 4180       Common Format and MIME Type for CSV Files    October 2005

  4.  Within the header and each record, there may be one or more
      fields, separated by commas.  Each line should contain the same
      number of fields throughout the file.  Spaces are considered part
      of a field and should not be ignored.  The last field in the
      record must not be followed by a comma.  For example:

      aaa,bbb,ccc
*)

  for var i := 0 to FFieldCount-1 do
  begin
    if (i > 0) then
      FWriter.Write(FDelimiter);

    if (FQuoteRow) then
      FWriter.Write(FQuote);

    FWriter.Write(FRowBuffer[i]);

    if (FQuoteRow) then
      FWriter.Write(FQuote);
  end;

  FWriter.Write(FTerminator);
end;

// -----------------------------------------------------------------------------

procedure TLocalizerCsvWriter.WriteField(const Value: string);
begin
  // Make room in buffer for field
  if (FFieldCount > High(FRowBuffer)) then
    // Exponential growth to minimize reallocations
    SetLength(FRowBuffer, Max(FFieldCount + 1, Length(FRowBuffer) * 2));

  FQuoteRow := FQuoteRow or NeedQuotes(Value);

  FRowBuffer[FFieldCount] := CsvEscape(Value);
  Inc(FFieldCount);
end;

// -----------------------------------------------------------------------------

function TLocalizerCsvWriter.NeedQuotes(const Value: string): boolean;
begin
(*
  RFC 4180       Common Format and MIME Type for CSV Files    October 2005

  6.  Fields containing line breaks (CRLF), double quotes, and commas
      should be enclosed in double-quotes.  For example:

      "aaa","b CRLF
      bb","ccc" CRLF
      zzz,yyy,xxx
*)

  // We don't need to check for decimal separators since we only process strings
  Result := (FAlwaysQuote) or (Value.IndexOfAny([' ', #10, #13]) <> -1) or (Value.StartsWith(' ')) or (Value.EndsWith(' ')) or
    (Value.Contains(FQuote)) or (Value.Contains(FDelimiter));
end;

// -----------------------------------------------------------------------------

function TLocalizerCsvWriter.CsvEscape(const Value: string): string;
begin
(*
  RFC 4180       Common Format and MIME Type for CSV Files    October 2005

  7.  If double-quotes are used to enclose fields, then a double-quote
      appearing inside a field must be escaped by preceding it with
      another double quote.  For example:

      "aaa","b""bb","ccc"
*)

  Result := Value.Replace(FQuote, FQuote+FQuote, [rfReplaceAll]);
end;

// -----------------------------------------------------------------------------

procedure TLocalizerCsvWriter.Write(AProject: TLocalizerProject);
begin
  AProject.Traverse(
    function(Module: TLocalizerModule): boolean
    begin
      if (Module.Status = ItemStatusTranslate) and (not Module.IsUnused) then
        Write(Module);
      Result := True;
    end, True); // Must sort or output will be confusing for the user
end;

// -----------------------------------------------------------------------------

procedure TLocalizerCsvWriter.Write(AModule: TLocalizerModule);
begin
  AModule.Traverse(
    function(Prop: TLocalizerProperty): boolean
    begin
      if (Prop.Status = ItemStatusTranslate) and (not Prop.IsUnused) then
        Write(Prop);
      Result := True;
    end, True); // Must sort or output will be confusing for the user
end;

// -----------------------------------------------------------------------------

procedure TLocalizerCsvWriter.Write(AProp: TLocalizerProperty);

  procedure WriteTranslation(Language: TTranslationLanguage);
  var
    Translation: TLocalizerTranslation;
    Value: string;
  begin
    if (AProp.Translations.TryGetTranslation(Language, Translation)) and (Translation.IsTranslated) then
      Value := Translation.Value
    else
    if (not FOnlyTranslated) then
      Value := AProp.Value
    else
      Value := '';
    WriteField(Value);
  end;


var
  i: integer;
begin
  BeginRow(AProp.Item.Module.Project);

  WriteField(AProp.Item.Module.Name);
  WriteField(AProp.Item.Name);
  WriteField(AProp.Item.TypeName);
  WriteField(AProp.Name);
  WriteField(AProp.Value);
  if (FLanguage = nil) then
  begin
    for i := 0 to AProp.Item.Module.Project.TranslationLanguages.Count-1 do
      WriteTranslation(AProp.Item.Module.Project.TranslationLanguages[i])
  end else
    WriteTranslation(FLanguage);

  EndRow;
end;

// -----------------------------------------------------------------------------

procedure TLocalizerCsvWriter.WriteHeader(AProject: TLocalizerProject);

  procedure WriteLanguage(Language: TLanguageItem);
  begin
    WriteField(Language.LocaleName);
  end;

var
  i: integer;
begin
(*
  RFC 4180       Common Format and MIME Type for CSV Files    October 2005

  3.  There maybe an optional header line appearing as the first line
      of the file with the same format as normal record lines.  This
      header will contain names corresponding to the fields in the file
      and should contain the same number of fields as the records in
      the rest of the file (the presence or absence of the header line
      should be indicated via the optional "header" parameter of this
      MIME type).  For example:

      field_name,field_name,field_name CRLF
      aaa,bbb,ccc CRLF
      zzz,yyy,xxx CRLF
*)

  FNeedHeader := False;

  BeginRow(AProject);

  WriteField('Module');
  WriteField('Item');
  WriteField('ItemType');
  WriteField('Property');
  WriteLanguage(AProject.SourceLanguage);
  if (FLanguage = nil) then
  begin
    for i := 0 to AProject.TranslationLanguages.Count-1 do
      WriteLanguage(AProject.TranslationLanguages[i].Language)
  end else
    WriteLanguage(FLanguage.Language);

  EndRow;
end;

// -----------------------------------------------------------------------------

end.
