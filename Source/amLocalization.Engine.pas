unit amLocalization.Engine;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

// IGNORE_DUPLICATE_RESOURCESTRING
// Define to ignore duplicate resource string symbols declared in the DRC file
{$define IGNORE_DUPLICATE_RESOURCESTRING}

uses
  Classes,
  SysUtils,
  Windows,
  Generics.Collections,
  Generics.Defaults,
  amLocalization.Model,
  amLocalization.ResourceWriter;

// -----------------------------------------------------------------------------
//
// TTranslateProc
//
// -----------------------------------------------------------------------------
type
  TTranslateProc = reference to function(Language: TTranslationLanguage; Prop: TLocalizerProperty; var NewValue: string): boolean;


// -----------------------------------------------------------------------------
//
// TResourceStringSymbolMap
//
// -----------------------------------------------------------------------------
// Loads and parses Delphi DRC file.
// Contains map between resource string names and IDs.
// -----------------------------------------------------------------------------
type
  TResourceStringSymbolMap = class
  private
    FSymbols: TDictionary<string, Word>;
    FIDs: TDictionary<Word, string>;
    FConflicts: TList<string>;
  private
    function DemangleSymbolName(const Value: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const Filename: string);

    procedure Clear;
    procedure Add(const Symbol: string; ID: Word);

    function TryLookupSymbol(const Symbol: string; var ID: Word): boolean;
    function TryLookupID(ID: Word; var Symbol: string): boolean;

    function HasConflicts: boolean;
    procedure GetConflicts(ANames: TStrings);
  end;


// -----------------------------------------------------------------------------
//
// TProjectResourceProcessor
//
// -----------------------------------------------------------------------------
type
  TLocalizerImportAction = (
    liaUpdateSource,            // Update project from source
    liaUpdateTarget,            // Update translation from a target module
    liaTranslate                // Build target module
  );

type
  TTranslationCounts = record
    CountAdded: integer;
    CountUpdated: integer;
    CountSkipped: integer;
  end;

type
  TProjectResourceProcessor = class
  private
    FTranslationCount: TTranslationCounts;
    FIncludeVersionInfo: boolean;
    FConflicts: TStrings;
  private
    function GetConflicts: TStrings;
    function GetHasConflicts: boolean;
  protected
    class function DefaultTranslator(Language: TTranslationLanguage; Prop: TLocalizerProperty; var NewValue: string): boolean; static;
  public
    destructor Destroy; override;

    procedure Execute(Action: TLocalizerImportAction; Project: TLocalizerProject; const Filename: string; Language: TTranslationLanguage; const ResourceWriter: IResourceWriter; Translator: TTranslateProc = nil); overload;
    procedure Execute(Action: TLocalizerImportAction; Project: TLocalizerProject; Instance: HINST; Language: TTranslationLanguage; const ResourceWriter: IResourceWriter; Translator: TTranslateProc = nil); overload;

    procedure ScanProject(Project: TLocalizerProject; const Filename: string); overload;
    procedure ScanProject(Project: TLocalizerProject; Instance: HINST); overload;

    property TranslationCount: TTranslationCounts read FTranslationCount;
    property IncludeVersionInfo: boolean read FIncludeVersionInfo write FIncludeVersionInfo;

    property HasConflicts: boolean read GetHasConflicts;
    property Conflicts: TStrings read GetConflicts;
  end;

  EResourceProcessor = class(Exception);

resourcestring
  sErrorLoadingModuleTitle = 'Error loading module';

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  Math,
  IOUtils,
  StrUtils,
  RegularExpressions,
  amPath,
  amLocalization.Settings;


// -----------------------------------------------------------------------------
//
// TModuleResourceProcessor
//
// -----------------------------------------------------------------------------
constructor TResourceStringSymbolMap.Create;
begin
  inherited Create;

  FSymbols := TDictionary<string, Word>.Create(TIStringComparer.Ordinal);
  FIDs := TDictionary<Word, string>.Create;
end;

function TResourceStringSymbolMap.DemangleSymbolName(const Value: string): string;
begin
  // Issue 37: Handle C++ mangling
  // https://en.wikipedia.org/wiki/Name_mangling#Complex_example

  (*
    Demangles a mangled symbol on the form
    (from https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangling)

      <mangled-name> ::= _Z <encoding>

      <encoding> ::= <data name>

      <name> ::= <nested-name>

      <nested-name> ::= N [<CV-qualifiers>] [<ref-qualifier>] <prefix> <unqualified-name> E

      <prefix> ::= <unqualified-name>                 # global class or namespace
               ::= <prefix> <unqualified-name>        # nested class or namespace

      <unqualified-name> ::= <source-name>

      <source-name> ::= <positive length number> <identifier>

      <identifier> ::= <unqualified source code identifier>

    Like so:

      _ZN3Vcl7Comstrs13_sDateTimeMaxE -> Vcl_Comstrs_sDateTimeMax

  *)

  if (not StartsText('_ZN', Value)) then
    Exit(Value);

  SetLength(Result, Length(Value));

  var p := PChar(Result); // Target pointer
  var Count := 0; // Length of result
  var Index := 4; // Current source index. Skip leading "_ZN"
  while (Index <= Length(Value)) and (Value[Index] <> 'E') do
  begin
    // Calculate length of segment
    var Len := 0; // Length of segment
    while (Value[Index] >= '0') and (Value[Index] <= '9') do
    begin
      Len := Len * 10 + Ord(Value[Index]) - Ord('0');
      Inc(Index);
    end;
    Len := Min(Len, Length(Value)-Index+1);

    // Insert "_" between unit namespace segments
    if (Count > 0) and (Value[Index] <> '_') then
    begin
      p^ := '_';
      Inc(p);
      Inc(Count);
    end;

    // Copy segment
    Move(Value[Index], p^, Len * SizeOf(Char));

    Inc(Count, Len);
    Inc(Index, Len);
    Inc(p, Len);
  end;

  // Truncate result
  SetLength(Result, Count);
end;

destructor TResourceStringSymbolMap.Destroy;
begin
  FSymbols.Free;
  FIDs.Free;
  FConflicts.Free;

  inherited;
end;

procedure TResourceStringSymbolMap.GetConflicts(ANames: TStrings);
begin
  if (FConflicts <> nil) then
    for var Name in FConflicts do
      ANames.Add(Name);
end;

// -----------------------------------------------------------------------------

function TResourceStringSymbolMap.HasConflicts: boolean;
begin
  Result := (FConflicts <> nil) and (FConflicts.Count > 0);
end;

// -----------------------------------------------------------------------------

procedure TResourceStringSymbolMap.Clear;
begin
  FSymbols.Clear;
  FIDs.Clear;
  FreeAndNil(FConflicts);
end;

// -----------------------------------------------------------------------------

procedure TResourceStringSymbolMap.LoadFromFile(const Filename: string);
var
  Reader: TStreamReader;
  i: integer;
  s: string;
  Symbol: string;
  Value: string;
  ResourceID: integer;
begin
  Clear;

  // DRC-file being encoded in ANSI is clearly a bug in Delphi but we'll have to deal with it.
  // Luckily the part of the file we're using (#define) is almost always ASCII.
  Reader := TStreamReader.Create(Filename, TEncoding.ANSI);
  try
    while (not Reader.EndOfStream) do
    begin
      s := Reader.ReadLine;

      if (not s.StartsWith('#define ')) then
        continue;

      Delete(s, 1, Length('#define '));

      i := Pos(' ', s);
      if (i < 2) then
        continue;

      Symbol := DemangleSymbolName(Copy(s, 1, i-1));
      Value := Copy(s, i+1, MaxInt);

      if (TryStrToInt(Value, ResourceID)) then
        Add(Symbol, ResourceID);
    end;

  finally
    Reader.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TResourceStringSymbolMap.Add(const Symbol: string; ID: Word);
var
  DuplicateName: string;
  DuplicateValue: Word;
begin
  // Look for existing symbol
  if (FSymbols.TryGetValue(Symbol, DuplicateValue)) then
  begin
    // While the ID values in a DRC file are always unique, the symbols can
    // be duplicated in case of duplicate resourcestring names within a unit.
    // Since the symbol/ID pairs are actually C #defines, a later declaration
    // will always replace earlier ones, which we can then ignore.

    // Existing symbol found
    if (DuplicateValue <> ID) then
    begin
      // Existing is conflict.
      // Update existing symbol->id
      FSymbols.AddOrSetValue(Symbol, ID);

      // Get existing id->symbol
      DuplicateName := FIDs[DuplicateValue];

      // Replace existing id->symbol with new
      FIDs.Remove(DuplicateValue);
      FIDs.Add(ID, Symbol);

      // Save conflict
      if (FConflicts = nil) then
        FConflicts := TList<string>.Create;
      FConflicts.Add(DuplicateName);
    end;
  end else
  begin
    FSymbols.Add(Symbol, ID);

    // Look for existing ID
    if (FIDs.TryGetValue(ID, DuplicateName)) then
    begin
      // Existing ID found.
      if (DuplicateName <> Symbol) then
      begin
        // Existing is conflict.
        // Update existing id->symbol
        FIDs.AddOrSetValue(ID, Symbol);

        // Remove existing symbol->id
        FSymbols.Remove(DuplicateName);

        // Save conflict
        if (FConflicts = nil) then
          FConflicts := TList<string>.Create;
        FConflicts.Add(DuplicateName);
      end;
    end else
      FIDs.Add(ID, Symbol);
  end;
end;

// -----------------------------------------------------------------------------

function TResourceStringSymbolMap.TryLookupID(ID: Word; var Symbol: string): boolean;
begin
  Result := FIDs.TryGetValue(ID, Symbol);
end;

// -----------------------------------------------------------------------------

function TResourceStringSymbolMap.TryLookupSymbol(const Symbol: string; var ID: Word): boolean;
begin
  Result := FSymbols.TryGetValue(Symbol, ID);
end;


// -----------------------------------------------------------------------------
//
// TModuleResourceProcessor
//
// -----------------------------------------------------------------------------
type
  TModuleResourceProcessor = class abstract
  private
    FModule: TLocalizerModule;
    FInstance: HINST;
  protected
    FTranslationCounts: TTranslationCounts;
  protected
    function GetHasConflicts: boolean; virtual;
    procedure DoSetTranslation(Prop: TLocalizerProperty; Language: TTranslationLanguage; const Value: string);
    property Module: TLocalizerModule read FModule;
    property Instance: HINST read FInstance;
  public
    constructor Create(AModule: TLocalizerModule; AInstance: HINST);

    function Execute(Action: TLocalizerImportAction; ResourceWriter: IResourceWriter; Language: TTranslationLanguage; Translator: TTranslateProc): boolean; overload; virtual; abstract;
    function Execute(Action: TLocalizerImportAction; ResourceWriter: IResourceWriter; Language: TTranslationLanguage): boolean; overload;

    procedure GetConflicts(ANames: TStrings); virtual;

    property TranslationCount: TTranslationCounts read FTranslationCounts;

    property HasConflicts: boolean read GetHasConflicts;
  end;

// -----------------------------------------------------------------------------

constructor TModuleResourceProcessor.Create(AModule: TLocalizerModule; AInstance: HINST);
begin
  inherited Create;
  FModule := AModule;
  FInstance := AInstance;
end;

// -----------------------------------------------------------------------------

function TModuleResourceProcessor.Execute(Action: TLocalizerImportAction; ResourceWriter: IResourceWriter; Language: TTranslationLanguage): boolean;
begin
  Result := Execute(Action, ResourceWriter, Language, TProjectResourceProcessor.DefaultTranslator);
end;

// -----------------------------------------------------------------------------

procedure TModuleResourceProcessor.GetConflicts(ANames: TStrings);
begin
end;

function TModuleResourceProcessor.GetHasConflicts: boolean;
begin
  Result := False;
end;


// -----------------------------------------------------------------------------

procedure TModuleResourceProcessor.DoSetTranslation(Prop: TLocalizerProperty; Language: TTranslationLanguage; const Value: string);
var
  Translation: TLocalizerTranslation;
begin
  // We could have just added the translation with Prop.TranslatedValue[] but we need
  // to update the counts so we must do it the explicit way (same performance).
  if (Prop.Translations.TryGetTranslation(Language, Translation)) then
  begin
    if (Translation.Value <> Value) then
    begin
      // Update existing translation
      Translation.Value := Value;
      Inc(FTranslationCounts.CountUpdated);
    end else
      Inc(FTranslationCounts.CountSkipped);
  end else
  begin
    if (Prop.Value <> Value) then
    begin
      // Add new translation
      Prop.Translations.AddOrUpdateTranslation(Language, Value);
      Inc(FTranslationCounts.CountAdded);
    end else
      Inc(FTranslationCounts.CountSkipped);
  end;
end;


// -----------------------------------------------------------------------------
//
// TModuleDFMResourceProcessor
//
// -----------------------------------------------------------------------------
type
  TModuleDFMResourceProcessor = class(TModuleResourceProcessor)
  strict private type
    TRequiredPropertyRule = record
      RegExp: TRegEx;
      HasRegEx: boolean;
      TypeMask: string;
      PropertyName: string;
      PropertyValue: string;
    end;
  private
    FStream: TStream;
    FReader: TReader;
    FWriter: TWriter;
    FLastCopied: integer;
    FRequiredPropertyRules: array of TRequiredPropertyRule;
  protected
    procedure ReadCollection(Action: TLocalizerImportAction; Language: TTranslationLanguage; Translator: TTranslateProc; Item: TLocalizerItem; const Path: string);
    procedure ReadPropertyValue(Action: TLocalizerImportAction; Language: TTranslationLanguage; Translator: TTranslateProc; Item: TLocalizerItem; const Path, PropertyName: string);
    procedure ReadProperty(Action: TLocalizerImportAction; Language: TTranslationLanguage; Translator: TTranslateProc; Item: TLocalizerItem; const Path: string);
    procedure ReadProperties(Action: TLocalizerImportAction; Language: TTranslationLanguage; Translator: TTranslateProc; Item: TLocalizerItem; const Name: string);
    procedure ReadComponent(Action: TLocalizerImportAction; Language: TTranslationLanguage; Translator: TTranslateProc; const Path: string);

    procedure CopyToHere(CopyPos: int64);
  public
    destructor Destroy; override;

    function Execute(Action: TLocalizerImportAction; ResourceWriter: IResourceWriter; Language: TTranslationLanguage; Translator: TTranslateProc): boolean; override;
  end;

// -----------------------------------------------------------------------------

procedure TModuleDFMResourceProcessor.CopyToHere(CopyPos: int64);
var
  Buffer: TBytes;
  Count, Chunk: integer;
  SavePos: integer;
begin
  Assert(FLastCopied <= CopyPos);

  if (FWriter <> nil) then
  begin
    Count := CopyPos-FLastCopied;

    if (Count > 1024) then
      SetLength(Buffer, 1024)
    else
      SetLength(Buffer, Count);

    SavePos := FReader.Position;
    FReader.Position := FLastCopied;

    while (Count > 0) do
    begin
      if (Count > Length(Buffer)) then
        Chunk := Length(Buffer)
      else
        Chunk := Count;

      FReader.Read(Buffer, Chunk);
      FWriter.Write(Buffer, Chunk);

      Dec(Count, Chunk);
      Inc(FLastCopied, Chunk);
    end;

    FReader.Position := SavePos;

    Assert(FLastCopied = CopyPos);
  end else
    FLastCopied := CopyPos;
end;

// -----------------------------------------------------------------------------

destructor TModuleDFMResourceProcessor.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TModuleDFMResourceProcessor.Execute(Action: TLocalizerImportAction; ResourceWriter: IResourceWriter; Language: TTranslationLanguage; Translator: TTranslateProc): boolean;

  procedure SetupRequiredPropertyRules;
  begin
    if (not TranslationManagerSettings.Parser.Synthesize.Enabled) then
    begin
      SetLength(FRequiredPropertyRules, 0);
      exit;
    end;

    SetLength(FRequiredPropertyRules, TranslationManagerSettings.Parser.Synthesize.Rules.Count);
    var i := 0;
    for var Rule in TranslationManagerSettings.Parser.Synthesize.Rules do
    begin
      FRequiredPropertyRules[i].TypeMask := Rule.TypeMask;
      FRequiredPropertyRules[i].PropertyName := Rule.PropertyName;
      FRequiredPropertyRules[i].PropertyValue := Rule.PropertyValue;
      Inc(i);
    end;
  end;

const
  FilerSignature: UInt32 = $30465054; // ($54, $50, $46, $30) 'TPF0'
var
  SourceStream: TStream;
  TargetStream: TMemoryStream;
  Signature: UInt32;
begin
  Assert(Module.Kind = mkForm);

  if (FindResource(Instance, PChar(Module.Name), RT_RCDATA) = 0) then
  begin
    if (Action = liaUpdateSource) then
      Module.SetState(ItemStateUnused);
    Exit(False);
  end;

  SourceStream := TResourceStream.Create(Instance, Module.Name, RT_RCDATA);
  try

    if (SourceStream.Read(Signature, SizeOf(Signature)) <> SizeOf(Signature)) or (Signature <> FilerSignature) then
    begin
      if (Action in [liaUpdateSource, liaTranslate]) then
      begin
        Module.Kind := mkOther;
        Module.Status := ItemStatusDontTranslate;
      end;
      Exit(False);
    end;

    SourceStream.Position := 0;

    SetupRequiredPropertyRules;

    if (ResourceWriter <> nil) then
      TargetStream := TMemoryStream.Create
    else
      TargetStream := nil;
    try
      // When updating source we need to process the module even if it is marked as "don't translate".
      // Otherwise the items/properties will be left with ItemStateUnused state.
      if (Action = liaUpdateSource) or (Module.Status = ItemStatusTranslate) then
      begin
        FReader := TReader.Create(SourceStream, 1024);
        try
          if (TargetStream <> nil) then
            FWriter := TWriter.Create(TargetStream, 1024)
          else
            FWriter := nil;
          try

            FReader.ReadSignature;

            ReadComponent(Action, Language, Translator, '');

            CopyToHere(SourceStream.Size);

          finally
            FreeAndNil(FWriter);
          end;
        finally
          FreeAndNil(FReader);
        end;
      end else
      if (ResourceWriter <> nil) and (TargetStream <> nil) then
        TargetStream.CopyFrom(SourceStream, 0);

      if (ResourceWriter <> nil) and (TargetStream <> nil) then
      begin
        // Write translated resource data
        TargetStream.Position := 0;
        ResourceWriter.WriteModule(Module, PWideChar(Module.Name), TargetStream);
      end;
    finally
      TargetStream.Free;
    end;
  finally
    SourceStream.Free;
  end;

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TModuleDFMResourceProcessor.ReadCollection(Action: TLocalizerImportAction; Language: TTranslationLanguage; Translator: TTranslateProc; Item: TLocalizerItem; const Path: string);
var
  Index: integer;
  ChildItem: TLocalizerItem;
  Name: string;
begin
  // TReader.ReadCollection
  Index := 0;
  while (not FReader.EndOfList) do
  begin
    if (FReader.NextValue in [vaInt8, vaInt16, vaInt32]) then
      FReader.ReadInteger;

    Name := Format('%s[%d]', [Path, Index]);

    // Issue #27: If we are importing a target language then Item can be nil
    // if the parent didn't have any translatable properties.
    // We still need to create a child item so we can apply an imported
    // translation to it. In case there wasn't any then we will delete the item
    // again below.
    if (Item <> nil) or (Action = liaUpdateTarget) then
    begin
      if (Action <> liaUpdateTarget) then
        ChildItem := Module.AddItem(Name, Item.TypeName)
      else
        ChildItem := Module.FindItem(Name, True);
    end else
      ChildItem := nil;

    FReader.ReadListBegin;
    ReadProperties(Action, Language, Translator, ChildItem, Name);
    FReader.ReadListEnd;

    if (ChildItem <> nil) and (ItemStateNew in ChildItem.State) and (ChildItem.Properties.Count = 0) then
      ChildItem.Free;

    Inc(Index);
  end;
  FReader.ReadListEnd;
end;

// -----------------------------------------------------------------------------

procedure TModuleDFMResourceProcessor.ReadComponent(Action: TLocalizerImportAction; Language: TTranslationLanguage; Translator: TTranslateProc; const Path: string);
var
  Flags: TFilerFlags;
  Position: Integer;
  Name, ClassName: string;
  Item: TLocalizerItem;
begin
  // TReader.ReadComponent
  FReader.ReadPrefix(Flags, Position);

  ClassName := FReader.ReadStr;
  Name := FReader.ReadStr;

  if (Path <> '') then
    Name := Path + '.' + Name;

  if (Action <> liaUpdateTarget) then
    Item := Module.AddItem(Name, ClassName)
  else
    Item := Module.FindItem(Name, True);

  // Issue #27: Even if component wasn't found we still need to process its
  // properties in case we're importing a target language and the component has
  // children with translatable properties.

  if (Item = nil) or (Action <> liaUpdateSource) or (Item.Status <> ItemStatusDontTranslate) then
  begin
    // TReader.ReadDataInner
    ReadProperties(Action, Language, Translator, Item, Name);
    FReader.ReadListEnd;

    // Remove empty item
    if (Item <> nil) and (ItemStateNew in Item.State) and (Item.Properties.Count = 0) then
      Item.Free;
  end else
  begin
    while (not FReader.EndOfList) do
    begin
      FReader.ReadStr;
      FReader.SkipValue;
    end;
    FReader.ReadListEnd;
  end;

  while (not FReader.EndOfList) do
    ReadComponent(Action, Language, Translator, Name);
  FReader.ReadListEnd;
end;

// -----------------------------------------------------------------------------

procedure TModuleDFMResourceProcessor.ReadProperties(Action: TLocalizerImportAction; Language: TTranslationLanguage; Translator: TTranslateProc; Item: TLocalizerItem; const Name: string);
begin
  while (not FReader.EndOfList) do
    ReadProperty(Action, Language, Translator, Item, Name);

  if (Action = liaUpdateSource) then
  begin
    Assert(Item <> nil);

    // Post processing to apply "required properties" rules.

    // If a required property is missing from the element we create it and then mark it "Synthesized"
    for var i := 0 to High(FRequiredPropertyRules) do
    begin
      if (not FRequiredPropertyRules[i].HasRegEx) then
      begin
        FRequiredPropertyRules[i].RegExp := TRegEx.Create(FRequiredPropertyRules[i].TypeMask, [roCompiled, roSingleLine]);
        FRequiredPropertyRules[i].HasRegEx := True;
      end;

      // Look for required property element type
      if (not FRequiredPropertyRules[i].RegExp.IsMatch(Item.TypeName)) then
        continue;

      // Look for required property name

      // Special case:
      //
      // - Property previously had a value, but now doesn't appear in the DFM.
      //   For example:
      //   * Before: FieldName='Foo', DisplayLabel='Bar'
      //   * Now:    FieldName='Foo'
      //
      // - The property will exist in the project, but not in the DFM.
      //
      // - The property will have had the ItemStateUpdating flag set and the
      //   ReadProperty call above will not clear it (since it doesn't exist
      //   in the DFM).
      //
      // - Item.FindProperty will find the old property (from the project)
      //   and clear the ItemStateUpdating flag.
      //
      // - Since the property was found, it will not be synthesized, and will
      //   retain its old (now obsolete) value.
      //
      // To work around this we call FindProperty(UpdateState=False) so we can
      // determine if the returned property should be marked synthesized.
      //
      var Prop := Item.FindProperty(FRequiredPropertyRules[i].PropertyName, False, False);
      if (Prop <> nil) then
      begin
        // In case property was previously defined but should now be synthesized,
        // we clear the update flag and continue to have the property value
        // updated below.
        if (ItemStateUpdating in Prop.State) then
          Prop.ClearState(ItemStateUpdating)
        else
          continue;
      end;

      // Property not found. Get the value the new property should have.
      var Value: string;
      if (FRequiredPropertyRules[i].PropertyValue.StartsWith('@')) then
      begin
        // Get value from another named property
        var SourceProp := Item.FindProperty(Copy(FRequiredPropertyRules[i].PropertyValue, 2, MaxInt));
        if (SourceProp = nil) then
          continue;

        Value := SourceProp.Value;
      end else
        // Get literal value
        Value := FRequiredPropertyRules[i].PropertyValue;

      // Create new property (or find existing one), or update the one we found above
      if (Prop = nil) then
        Prop := Item.AddProperty(FRequiredPropertyRules[i].PropertyName, Value)
      else
        Prop.Value := Value;

      Prop.Synthesized := True;
    end;

  end else
  if (Action = liaTranslate) and (FWriter <> nil) and (Language <> nil) then
  begin
    Assert(Item <> nil);

    // If we're updating the target then we must inject any synthesized properties
    // into the output stream.

    // Flush deferred writes to writer
    CopyToHere(FReader.Position);

    for var Prop in Item.Properties.Values do
      if (Prop.Synthesized) and (Prop.HasTranslation(Language)) then
      begin
        // Property name
        FWriter.WriteStr(AnsiString(Prop.Name));
        // Translated value (typed as string)
        FWriter.WriteString(Prop.TranslatedValue[Language]);
      end;
  end;
end;

procedure TModuleDFMResourceProcessor.ReadProperty(Action: TLocalizerImportAction; Language: TTranslationLanguage; Translator: TTranslateProc; Item: TLocalizerItem; const Path: string);
var
  PropertyName: string;
begin
  // TReader.ReadProperty
  PropertyName := FReader.ReadStr;

  ReadPropertyValue(Action, Language, Translator, Item, Path, PropertyName);
end;

procedure TModuleDFMResourceProcessor.ReadPropertyValue(Action: TLocalizerImportAction; Language: TTranslationLanguage;
  Translator: TTranslateProc; Item: TLocalizerItem; const Path, PropertyName: string);

  function GetProperty(const PropertyName, Value: string): TLocalizerProperty;
  begin
    Result := nil;

    if (Item = nil) then
      Exit;

    if (Action in [liaUpdateSource, liaTranslate]) then
      Result := Item.AddProperty(PropertyName, Value)
    else
    if (Action = liaUpdateTarget) and (Language <> nil) then
    begin
      Result := Item.FindProperty(PropertyName, True);

      if (Result <> nil) then
        DoSetTranslation(Result, Language, Value);
    end;
  end;

  procedure HandleProperty(Prop: TLocalizerProperty; StartPos, EndPos: int64; StringList: boolean);
  var
    Value, NewValue: string;
    Start, n: integer;
  begin
    // Perform translation
    if (Action <> liaTranslate) or (Language = nil) or (not Assigned(Translator)) or (Prop = nil) then
      Exit;

    Value := Prop.Value;
    NewValue := Value;

    if (not Translator(Language, Prop, NewValue)) then
      Exit;

    if (NewValue = Value) or (FWriter = nil) then
      Exit;

    // Copy up until original value
    CopyToHere(StartPos);

    // Write new value
    if (StringList) then
    begin
      Assert(StringListHandling = slStringLines);

      FWriter.WriteListBegin;

      // Split CR/LF delimited string into individual strings
      Start := 1;
      while (Start < Length(NewValue)) do
      begin
        n := Pos(#13, NewValue, Start);
        if (n = 0) then
        begin
          FWriter.WriteString(Copy(NewValue, Start, MaxInt));
          break;
        end else
          FWriter.WriteString(Copy(NewValue, Start, n-Start));

        Inc(n);
        // Ignore LF
        while (NewValue[n] = #10) do
          Inc(n);

        Start := n;
      end;
      FWriter.WriteListEnd;

    end else
      FWriter.WriteString(NewValue);

    // Skip original value in source so it doesn't get copied
    FLastCopied := EndPos;
  end;

var
  ValueType: TValueType;
  StartPos, EndPos: int64;
  Value: string;
  Index: integer;
  Prop: TLocalizerProperty;
begin
  ValueType := FReader.NextValue;

  case ValueType of
  vaCollection:
    begin
      FReader.ReadValue;
      ReadCollection(Action, Language, Translator, Item, Path+'.'+PropertyName);
    end;

  vaWString, vaUTF8String, vaString, vaLString:
    begin
      StartPos := FReader.Position;

      // TReader.ReadPropValue
      Value := FReader.ReadString;

      EndPos := FReader.Position;

      Prop := GetProperty(PropertyName, Value);
      HandleProperty(Prop, StartPos, EndPos, False);
    end;

  vaList:
    begin
      // Attempt to handle TStrings
      // Properties of type TStrings are named <propertyname>.Strings - See TStrings.DefineProperties
      if (StringListHandling = slIgnore) or (not PropertyName.EndsWith('.Strings')) then
      begin
        FReader.SkipValue;
        Exit;
      end;

      StartPos := FReader.Position;
      FReader.ReadValue;

      // Validate list item data type
      // To be safe we read and validate the whole list and then rewind since it's too late to
      // rewind after HandleProperty has been called called.
      while (not FReader.EndOfList) do
      begin
        ValueType := FReader.NextValue;
        if (not (ValueType in [vaWString, vaUTF8String, vaString, vaLString])) then
        begin
          // Unexpected value type encountered - rewind and skip
          FReader.Position := StartPos;
          FReader.SkipValue; // Skip whole list
          Exit;
        end;
        FReader.SkipValue; // Skip string so we can test the next
      end;
      // Rewind and skip vaList
      FReader.Position := StartPos;
      FReader.ReadValue;

      if (StringListHandling = slStringLines) then
      begin
        (*
        ** The string list is a single property
        *)
        Value := '';
        while (not FReader.EndOfList) do
        begin
          // Append value to list
          if (Value <> '') then
            Value := Value + #13;
          Value := Value + FReader.ReadString;
        end;
        FReader.ReadListEnd;
        EndPos := FReader.Position;

        Prop := GetProperty(PropertyName, Value);
        HandleProperty(Prop, StartPos, EndPos, True);
      end else
      begin
        (*
        ** Each line in the string list is a single property
        *)
        Index := 0;
        while (not FReader.EndOfList) do
        begin
          // Recurse to handle string list line (we already know it's a string)
          ReadPropertyValue(Action, Language, Translator, Item, Path, Format('%s[%d]', [PropertyName, Index]));
          Inc(Index);
        end;
        FReader.ReadListEnd;
      end;
    end;

  else
    FReader.SkipValue;
  end;
end;


// -----------------------------------------------------------------------------
//
// TModuleStringResourceProcessor
//
// -----------------------------------------------------------------------------
type
  TResourceGroups = TList<Word>;

  TModuleStringResourceProcessor = class(TModuleResourceProcessor)
  private
    FSymbolMap: TResourceStringSymbolMap;
    FResourceGroups: TResourceGroups;
  protected
    function GetHasConflicts: boolean; override;
    procedure ExecuteGroup(Action: TLocalizerImportAction; ResourceGroupID: Word; ReadStream, WriteStream: TStream; Language: TTranslationLanguage; Translator: TTranslateProc);
  public
    constructor Create(AModule: TLocalizerModule; AInstance: HINST; AResourceGroups: TResourceGroups);
    destructor Destroy; override;

    function Execute(Action: TLocalizerImportAction; ResourceWriter: IResourceWriter; Language: TTranslationLanguage; Translator: TTranslateProc): boolean; override;

    procedure GetConflicts(ANames: TStrings); override;
  end;

// -----------------------------------------------------------------------------

constructor TModuleStringResourceProcessor.Create(AModule: TLocalizerModule; AInstance: HINST; AResourceGroups: TResourceGroups);
begin
  inherited Create(AModule, AInstance);

  FResourceGroups := AResourceGroups;
  FSymbolMap := TResourceStringSymbolMap.Create;

  if (TFile.Exists(Module.Project.StringSymbolFilename)) then
    FSymbolMap.LoadFromFile(Module.Project.StringSymbolFilename);
end;

// -----------------------------------------------------------------------------

destructor TModuleStringResourceProcessor.Destroy;
begin
  FSymbolMap.Free;

  inherited;
end;

function TModuleStringResourceProcessor.Execute(Action: TLocalizerImportAction; ResourceWriter: IResourceWriter; Language: TTranslationLanguage; Translator: TTranslateProc): boolean;
var
  SourceStream: TStream;
  TargetStream: TMemoryStream;
  ResourceID: Word;
  AnyFound: boolean;
begin
  Assert(Module.Kind = mkString);

  if (FResourceGroups.Count = 0) then
    Exit(False);

  AnyFound := False;

  for ResourceID in FResourceGroups do
  begin
    if (FindResource(Instance, PChar(ResourceID), RT_STRING) <> 0) then
    begin
      SourceStream := TResourceStream.CreateFromID(Instance, ResourceID, RT_STRING);
      AnyFound := True;
    end else
      SourceStream := nil;
    try

      if (Action = liaTranslate) and (ResourceWriter <> nil) and (SourceStream <> nil) then
        TargetStream := TMemoryStream.Create
      else
        TargetStream := nil;
      try

        if (Module.Status = ItemStatusTranslate) then
          ExecuteGroup(Action, ResourceID, SourceStream, TargetStream, Language, Translator)
        else
        if (Action = liaTranslate) and (ResourceWriter <> nil) and (SourceStream <> nil) and (TargetStream <> nil) then
          TargetStream.CopyFrom(SourceStream, 0);

        if (Action = liaTranslate) and (ResourceWriter <> nil) and (TargetStream <> nil) then
        begin
          // Write translated resource data
          TargetStream.Position := 0;
          ResourceWriter.WriteModule(Module, PWideChar(ResourceID), TargetStream);
        end;
      finally
        TargetStream.Free;
      end;
    finally
      SourceStream.Free;
    end;
  end;

  if (Action = liaUpdateSource) and (not AnyFound) then
    Module.SetState(ItemStateUnused);

  Result := True;
end;

procedure TModuleStringResourceProcessor.ExecuteGroup(Action: TLocalizerImportAction; ResourceGroupID: Word; ReadStream, WriteStream: TStream; Language: TTranslationLanguage; Translator: TTranslateProc);
var
  i: integer;
  Size: Word;
  Value: string;
  Item: TLocalizerItem;
  ResourceID: Word;
  Name: string;
  Prop: TLocalizerProperty;
begin
  i := 0;
  ResourceID := (ResourceGroupID-1) * 16;
  while (i < 16) do
  begin
    if (ReadStream <> nil) then
    begin
      if (ReadStream.Read(Size, SizeOf(Size)) <> SizeOf(Size)) then
        break; // Corrupt resource
      SetLength(Value, Size);

      if (Size > 0) then
        if (ReadStream.Read(PChar(Value)^, Size*SizeOf(Char)) <> Size*SizeOf(Char)) then
          break; // Corrupt resource

      // Skip string if value is empty and ID is unknown
      Name := '';
      if (FSymbolMap.TryLookupID(ResourceID, Name)) {$if not defined(IGNORE_DUPLICATE_RESOURCESTRING)} or (not Value.IsEmpty) {$ifend} then
      begin
        if (not Name.IsEmpty) then
        begin
          if (Action <> liaUpdateTarget) then
            Item := Module.AddItem(Name, '')
          else
            // Look for existing item - don't add new
            Item := Module.FindItem(Name, True);
        end else
        begin
          // TODO : This could be considered an error. Should flag item as unmapped.
          // Has been seen in some Delphi apps. E.g. sigma.exe - presumably the strings
          // are from explicitly included .RES files and are not resourcestrings.
          if (Action <> liaUpdateTarget) then
            Item := Module.AddItem(ResourceID, '')
          else
            // Look for existing item - don't add new
            Item := Module.FindItem(ResourceID);
        end;


        if (Item <> nil) and (Item.Status <> ItemStatusDontTranslate) then
        begin
          if (Action in [liaUpdateSource, liaTranslate]) then
            Prop := Item.AddProperty('', Value)
          else
          if (Action = liaUpdateTarget) and (Language <> nil) then
          begin
            // Look for existing property - don't add new
            Prop := Item.FindProperty('', True);

            if (Prop <> nil) then
              DoSetTranslation(Prop, Language, Value);
          end else
            Prop := nil;

          // Perform translation
          if (Action = liaTranslate) and (Language <> nil) and (Assigned(Translator)) and (Prop <> nil) then
          begin
            Value := Prop.Value;

            if (not Translator(Language, Prop, Value)) then
              Value := Prop.Value;
          end;
        end;
      end;

      if (Action = liaTranslate) and (WriteStream <> nil) then
      begin
        Size := Length(Value);
        WriteStream.Write(Size, SizeOf(Size));

        if (Size > 0) then
          WriteStream.Write(PChar(Value)^, Size*SizeOf(Char));
      end;
    end else
    if (Action = liaUpdateSource) then
    begin
      // Source item not found in resource - mark unused
      if (Module.Items.TryGetValue('', Item)) then
        Item.SetState(ItemStateUnused);
    end;

    Inc(i);
    Inc(ResourceID);
  end;
end;


procedure TModuleStringResourceProcessor.GetConflicts(ANames: TStrings);
begin
  inherited;
  FSymbolMap.GetConflicts(ANames);
end;

function TModuleStringResourceProcessor.GetHasConflicts: boolean;
begin
  Result := FSymbolMap.HasConflicts;
end;

// -----------------------------------------------------------------------------
//
// TProjectProcessorDFMResource
//
// -----------------------------------------------------------------------------

destructor TProjectResourceProcessor.Destroy;
begin
  FConflicts.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

class function TProjectResourceProcessor.DefaultTranslator(Language: TTranslationLanguage; Prop: TLocalizerProperty; var NewValue: string): boolean;
var
  Translation: TLocalizerTranslation;
begin
  Assert(Language <> nil);

  if (Prop.Status <> ItemStatusTranslate) then
    Exit(False);

  if (Prop.Translations.TryGetTranslation(Language, Translation)) and (Translation.IsTranslated) then
  begin
    NewValue := Translation.Value;
    Result := True;
  end else
    Result := False;
end;

// -----------------------------------------------------------------------------

procedure TProjectResourceProcessor.ScanProject(Project: TLocalizerProject; const Filename: string);
begin
  Execute(liaUpdateSource, Project, Filename, nil, nil);
end;

procedure TProjectResourceProcessor.ScanProject(Project: TLocalizerProject; Instance: HINST);
begin
  Execute(liaUpdateSource, Project, Instance, nil, nil);
end;

// -----------------------------------------------------------------------------

function ResourceIdentToOrdinal(Ident: PWideChar): WORD;
begin
  Result := DWORD(Ident) and $0000FFFF;
end;

function EnumResourceNamesDFMs(AModule: HMODULE; AResType, AResName: PChar; AProject: TLocalizerProject): BOOL; stdcall;
var
  Module: TLocalizerModule;
begin
  Assert(AResType = RT_RCDATA);

  Module := AProject.AddModule(AResName);
  Module.Kind := mkForm;

  Result := True;
end;

function EnumResourceNamesStrings(AModule: HMODULE; AResType, AResName: PChar; AResourceGroups: TResourceGroups): BOOL; stdcall;
var
  ResourceID: WORD;
begin
  Assert(AResType = RT_STRING);

  ResourceID := ResourceIdentToOrdinal(AResName);
  AResourceGroups.Add(ResourceID);

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TProjectResourceProcessor.Execute(Action: TLocalizerImportAction; Project: TLocalizerProject; const Filename: string; Language: TTranslationLanguage; const ResourceWriter: IResourceWriter; Translator: TTranslateProc);
var
  Module: HModule;
const
  LOAD_LIBRARY_AS_IMAGE_RESOURCE = $00000020;
begin
  Module := LoadLibraryEx(PChar(Filename), 0, LOAD_LIBRARY_AS_DATAFILE or LOAD_LIBRARY_AS_IMAGE_RESOURCE);
  if (Module = 0) then
  begin
    if (GetLastError = ERROR_BAD_EXE_FORMAT) then
      raise EResourceProcessor.CreateFmt('The specified file is not a valid PE image (EXE or DLL): %s', [Filename]);
    RaiseLastOSError;
  end;
  try

    Execute(Action, Project, Module, Language, ResourceWriter, Translator);

  finally
    FreeLibrary(Module);
  end;
end;

procedure TProjectResourceProcessor.Execute(Action: TLocalizerImportAction; Project: TLocalizerProject; Instance: HINST; Language: TTranslationLanguage; const ResourceWriter: IResourceWriter; Translator: TTranslateProc);

  procedure CopyVersionInfo;
  var
    SourceStream: TStream;
    TargetStream: TMemoryStream;
  begin
    if (FindResource(Instance, PWideChar(1), RT_VERSION) = 0) then
      Exit;

    TargetStream := TMemoryStream.Create;
    try
      SourceStream := TResourceStream.CreateFromID(Instance, 1, RT_VERSION);
      try
        TargetStream.CopyFrom(SourceStream, 0);
      finally
        SourceStream.Free;
      end;

      ResourceWriter.CopyVersionInfo(TargetStream);
    finally
      TargetStream.Free;
    end;
  end;

var
  Module: TLocalizerModule;
  StringsModule: TLocalizerModule;
  ResourceGroups: TResourceGroups;
  ModuleProcessor: TModuleResourceProcessor;
begin
  FillChar(FTranslationCount, SizeOf(FTranslationCount), 0);

  if (FConflicts <> nil) then
    FConflicts.Clear;

  Project.BeginUpdate;
  try
    Project.BeginLoad(Action = liaUpdateSource);
    try

      // Don't add modules if we're updating the target values
      if (Action <> liaUpdateTarget) then
        EnumResourceNames(Instance, RT_RCDATA, @EnumResourceNamesDFMs, IntPtr(Project));

      // Make sure we have a resourcestrings module
      StringsModule := Project.AddModule(sModuleNameResourcestrings);
      StringsModule.Kind := mkString;

      ResourceGroups := TResourceGroups.Create;
      try
        EnumResourceNames(Instance, RT_STRING, @EnumResourceNamesStrings, IntPtr(ResourceGroups));

        ResourceGroups.Sort;

        if (ResourceWriter <> nil) then
          ResourceWriter.BeginWrite;
        try

          if (Action = liaTranslate) and (not Assigned(Translator)) then
            Translator := TProjectResourceProcessor.DefaultTranslator;

          for Module in Project.Modules.Values.ToArray do // ToArray for stability since we delete from the list
          begin
            case Action of
              liaUpdateTarget:
                if (Module.IsUnused) then
                  continue;

              liaUpdateSource:
                if (Module.Status = ItemStatusDontTranslate) then
                  continue;
            end;

            case Module.Kind of
              mkForm:
                ModuleProcessor := TModuleDFMResourceProcessor.Create(Module, Instance);

              mkString:
                ModuleProcessor := TModuleStringResourceProcessor.Create(Module, Instance, ResourceGroups)
            else
              Module.Free;
              continue;
            end;

            try

              ModuleProcessor.Execute(Action, ResourceWriter, Language, Translator);

              Inc(FTranslationCount.CountAdded, ModuleProcessor.TranslationCount.CountAdded);
              Inc(FTranslationCount.CountUpdated, ModuleProcessor.TranslationCount.CountUpdated);
              Inc(FTranslationCount.CountSkipped, ModuleProcessor.TranslationCount.CountSkipped);

              if (ModuleProcessor.HasConflicts) then
                ModuleProcessor.GetConflicts(Conflicts);

            finally
              ModuleProcessor.Free;
            end;

            // Note: ModuleProcessor.Execute above can change the module Kind.
            // RT_RCDATA resources are created as mkForm by default and changed
            // in TModuleDFMResourceProcessor.Execute to mkOther if their
            // signature doesn't match that of a form.
            if (Module.Kind = mkOther) then
            begin
              if (ItemStateNew in Module.State) then
                Module.Free
              else
                Module.SetState(ItemStateUnused);
              continue;
            end;
          end;

          if (Action = liaTranslate) and (ResourceWriter <> nil) and (FIncludeVersionInfo) then
            CopyVersionInfo;

          if (ResourceWriter <> nil) then
            ResourceWriter.EndWrite(True);

        except
          if (ResourceWriter <> nil) then
            ResourceWriter.EndWrite(False);

          raise;
        end;
      finally
        ResourceGroups.Free;
      end;
    finally
      Project.EndLoad((Action = liaUpdateSource), (Action = liaUpdateSource));
    end;
  finally
    Project.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

function TProjectResourceProcessor.GetConflicts: TStrings;
begin
  if (FConflicts = nil) then
    FConflicts := TStringList.Create;
  Result := FConflicts;
end;

function TProjectResourceProcessor.GetHasConflicts: boolean;
begin
  Result := (FConflicts <> nil) and (FConflicts.Count > 0);
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------


end.
