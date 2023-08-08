unit amLocalization.CommandLine;

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
  amLocalization.Utils,
  amLocalization.ResourceWriter,
  amLocalization.Model;

type
  ICommandLineLogger = interface
    procedure Message(const Msg: string);
    procedure Error(const Msg: string);
    procedure Warning(const Msg: string);
  end;

type
  TLocalizationCommandLineTool = class
  private
    FLogger: ICommandLineLogger;
    FProject: TLocalizerProject;
    FVerbose: boolean;
    FModuleNameScheme: TModuleNameScheme;
    FOutputFolder: string;
    FIncludeVersionInfo: boolean;
  protected
    procedure Message(const Msg: string);
    procedure Error(const Msg: string);
    procedure Warning(const Msg: string);

    procedure DoBuild(TranslationLanguage: TTranslationLanguage);

    procedure LoadFromFile(const Filename: string);
    procedure Build(const Language: string);
    procedure Help;
    procedure Header;
  public
    constructor Create(ALogger: ICommandLineLogger);
    destructor Destroy; override;

    procedure Execute;

    class function ProjectFilename: string;
    class function OptionBuild: boolean;
    class function OptionSource(var Filename: string): boolean;
    class function OptionOutput(var Filename: string): boolean;
    class function OptionSymbols(var Filename: string): boolean;
    class function OptionPEStub(var Filename: string): boolean;
    class function OptionHelp: boolean;
    class function OptionVerbose: boolean;
    class function OptionIncludeVersionInfo: boolean;
    class function OptionName(ADefault: TModuleNameScheme): TModuleNameScheme;
    class function OptionLanguage(ADefault: string = ''): string;
  end;

type
  TCommandLineLogger = class(TInterfacedObject, ICommandLineLogger)
  private
    // ICommandLineLogger
    procedure Message(const Msg: string);
    procedure Error(const Msg: string);
    procedure Warning(const Msg: string);
  end;

resourcestring
  sCommandLineTitle = 'amTranslationManager resource module builder version %s';
  sCommandLineHelp = 'Usage: %s <projectfile> [options]'+#13#13+
    'Options:'+#13+
    '  -?                 Display help (this message)'+#13+
    '  -t:<language>      Only build for specified language'+#13+
    '  -b                 Build resource module(s)'+#13+
    '  -s:<source file>   Specify source file'+#13+
    '  -y:<symbol file>   Specify string symbols file'+#13+
    '  -o:<output folder> Specify resource module output folder'+#13+
    '  -x:<stub file>     Specify PE stub file'+#13+
    '  -v                 Display verbose messages'+#13+
    '  -i                 Include version info resource from source'+#13+
    '  -n:<scheme>        File name scheme:'+#13+
    '                     0: ISO 639-2 (e.g. ENU, DAN, DEU, etc)'+#13+
    '                     1: ISO 639-1 (e.g. EN, DA, DE, etc)'+#13+
    '                     2: RFC 4646 (e.g. en-US, da-DK, de-DE, etc)';

implementation

uses
  IOUtils,
  SysUtils,
  StrUtils,
  Windows,
  amPath,
  amVersionInfo,
  amLanguageInfo,
  amLocalization.Persistence,
  amLocalization.Engine;


{ TLocalizationCommandLine }

constructor TLocalizationCommandLineTool.Create(ALogger: ICommandLineLogger);
begin
  inherited Create;
  FLogger := ALogger;
  FProject := TLocalizerProject.Create('', LanguageInfo.DefaultLocale);
end;

destructor TLocalizationCommandLineTool.Destroy;
begin
  FProject.Free;
  inherited;
end;

procedure TLocalizationCommandLineTool.Message(const Msg: string);
begin
  if (FLogger <> nil) then
    FLogger.Message(Msg);
end;

procedure TLocalizationCommandLineTool.Error(const Msg: string);
begin
  if (FLogger <> nil) then
    FLogger.Error(Msg);
  Halt(1);
end;

procedure TLocalizationCommandLineTool.Warning(const Msg: string);
begin
  if (FLogger <> nil) then
    FLogger.Warning(Msg);
end;

class function TLocalizationCommandLineTool.OptionBuild: boolean;
begin
  Result := (FindCmdLineSwitch('build')) or (FindCmdLineSwitch('b'));
end;

class function TLocalizationCommandLineTool.OptionHelp: boolean;
begin
  Result := (FindCmdLineSwitch('help')) or (FindCmdLineSwitch('?')) or (FindCmdLineSwitch('h'));
end;

class function TLocalizationCommandLineTool.OptionIncludeVersionInfo: boolean;
begin
  Result := (FindCmdLineSwitch('versioninfo')) or (FindCmdLineSwitch('i'));
end;

class function TLocalizationCommandLineTool.OptionLanguage(ADefault: string): string;
begin
  if (not FindCmdLineSwitch('target', Result, True, [clstValueAppended])) and (not FindCmdLineSwitch('t', Result, True, [clstValueAppended])) then
    Result := ADefault;
end;

class function TLocalizationCommandLineTool.OptionName(ADefault: TModuleNameScheme): TModuleNameScheme;
begin
  var s: string;
  if (FindCmdLineSwitch('name', s, True, [clstValueAppended])) or (FindCmdLineSwitch('n', s, True, [clstValueAppended])) then
  begin
    var n := StrToIntDef(s, Ord(ADefault));
    if (n >= Ord(Low(TModuleNameScheme))) and (n <= Ord(High(TModuleNameScheme))) then
      Result := TModuleNameScheme(n)
    else
      Result := ADefault;
  end else
    Result := ADefault;
end;

class function TLocalizationCommandLineTool.OptionOutput(var Filename: string): boolean;
begin
  Result := False;
  var Value := '';
  if (FindCmdLineSwitch('output', Value, True, [clstValueAppended]) or FindCmdLineSwitch('o', Value, True, [clstValueAppended])) and
    (Value <> '') then
  begin
    // If command line specified a relative path then it must be relative to the "current folder"
    Filename := PathUtil.PathCombinePath(GetCurrentDir, Value);
    Result := True;
  end;
end;

class function TLocalizationCommandLineTool.OptionSource(var Filename: string): boolean;
begin
  Result := False;
  var Value := '';
  if (FindCmdLineSwitch('source', Value, True, [clstValueAppended]) or FindCmdLineSwitch('s', Value, True, [clstValueAppended])) and
    (Value <> '') then
  begin
    // If command line specified a relative path then it must be relative to the "current folder"
    Filename := PathUtil.PathCombinePath(GetCurrentDir, Value);
    Result := True;
  end;
end;

class function TLocalizationCommandLineTool.OptionSymbols(var Filename: string): boolean;
begin
  Result := False;
  var Value := '';
  if (FindCmdLineSwitch('symbols', Value, True, [clstValueAppended]) or FindCmdLineSwitch('y', Value, True, [clstValueAppended])) and
    (Value <> '') then
  begin
    // If command line specified a relative path then it must be relative to the "current folder"
    Filename := PathUtil.PathCombinePath(GetCurrentDir, Value);
    Result := True;
  end;
end;

class function TLocalizationCommandLineTool.OptionPEStub(var Filename: string): boolean;
begin
  Result := False;
  var Value := '';
  if (FindCmdLineSwitch('stub', Value, True, [clstValueAppended]) or FindCmdLineSwitch('x', Value, True, [clstValueAppended])) and
    (Value <> '') then
  begin
    // If command line specified a relative path then it must be relative to the "application folder"
    Filename := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), Value);
    Result := True;
  end;
end;

class function TLocalizationCommandLineTool.OptionVerbose: boolean;
begin
  Result := (FindCmdLineSwitch('verbose')) or (FindCmdLineSwitch('v'));
end;

class function TLocalizationCommandLineTool.ProjectFilename: string;
var
  i: integer;
begin
  // Filename should be first but look for it in all params anyway
  Result := '';
  for i := 1 to ParamCount do
    if (not CharInSet(ParamStr(1)[1], SwitchChars)) then
    begin
      Result := ParamStr(i);
      break;
    end;

  // If command line specified a relative path then it must be relative to the "current folder"
  if (Result <> '') then
    Result := PathUtil.PathCombinePath(GetCurrentDir, Result);
end;

procedure TLocalizationCommandLineTool.Execute;
var
  Filename: string;
  Language: string;
begin
  Header;

  if (ParamCount < 1) or OptionHelp then
  begin
    Help;
    Exit;
  end;

  Filename := ProjectFilename;

  if (not TFile.Exists(Filename)) then
    Error(Format('Project file not found: %s', [Filename]));

  FVerbose := OptionVerbose;
  FIncludeVersionInfo := OptionIncludeVersionInfo;

  LoadFromFile(Filename);

  Language := OptionLanguage;
  FModuleNameScheme := OptionName(Low(TModuleNameScheme));

  Filename := FProject.SourceFilename;
  if (OptionSource(Filename)) then
  begin
    if (FVerbose) then
      Message(Format('Custom source file: %s', [Filename]));
    FProject.SourceFilename := Filename;
  end;
  if (not TFile.Exists(Filename)) then
    Error(Format('Source file not found: %s', [Filename]));

  // Make symbol file absolute
  FProject.StringSymbolFilename := PathUtil.PathCombinePath(TPath.GetDirectoryName(FProject.SourceFilename), FProject.StringSymbolFilename);

  Filename := FProject.StringSymbolFilename;
  if (OptionSymbols(Filename)) then
  begin
    if (FVerbose) then
      Message(Format('Custom symbols file: %s', [Filename]));
    FProject.StringSymbolFilename := Filename;
  end;
  if (not TFile.Exists(Filename)) then
    Warning(Format('String symbols file not found: %s', [Filename]));

  FOutputFolder := TPath.GetDirectoryName(FProject.SourceFilename);
  if (OptionOutput(FOutputFolder)) then
  begin
    if (FVerbose) then
      Message(Format('Output folder: %s', [FOutputFolder]));
  end;
  if (not TDirectory.Exists(FOutputFolder)) then
    Warning(Format('Output folder does not exist: %s', [FOutputFolder]));

  if (OptionPEStub(sResourceModuleStub)) then
  begin
    if (FVerbose) then
      Message(Format('Custom PE stub: %s', [sResourceModuleStub]));

    if (not TFile.Exists(sResourceModuleStub)) then
      Warning(Format('Custom PE stub file does not exist: %s', [sResourceModuleStub]));
  end;


  if (OptionBuild) then
    Build(Language);

  Message('Done');
end;

procedure TLocalizationCommandLineTool.Header;
begin
  Message(Format(sCommandLineTitle, [TVersionInfo.FileVersionString(ParamStr(0))])+#13);
end;

procedure TLocalizationCommandLineTool.Help;
begin
  Message(Format(sCommandLineHelp, [TPath.GetFileNameWithoutExtension(ParamStr(0))]));
end;

procedure TLocalizationCommandLineTool.Build(const Language: string);
var
  LanguageItem: TLanguageItem;
  TranslationLanguage: TTranslationLanguage;
  i: integer;
begin
  if (Language <> '') then
  begin
    LanguageItem := LanguageInfo.FindLocale(Language);
    if (LanguageItem = nil) then
      LanguageItem := LanguageInfo.FindLanguageShortName(Language);
    if (LanguageItem = nil) then
      LanguageItem := LanguageInfo.FindISO639_1Name(Language);
    if (LanguageItem = nil) then
      LanguageItem := LanguageInfo.FindISO3166Name(Language);

    if (LanguageItem = nil) then
      Error(Format('Unknown target language: %s', [Language]));

    TranslationLanguage := FProject.TranslationLanguages.Find(LanguageItem);

    if (TranslationLanguage = nil) then
      Error(Format('Project does not contain any translations for the language: %s (%s)', [Language, LanguageItem.LanguageName]));

    DoBuild(TranslationLanguage);
  end else
  begin
    for i := 0 to FProject.TranslationLanguages.Count-1 do
      if (FProject.TranslationLanguages[i].Language <> FProject.SourceLanguage) then
        DoBuild(FProject.TranslationLanguages[i]);
  end;
end;

procedure TLocalizationCommandLineTool.DoBuild(TranslationLanguage: TTranslationLanguage);
var
  ProjectProcessor: TProjectResourceProcessor;
  ResourceWriter: IResourceWriter;
  LanguageItem: TLanguageItem;
  TargetFilename: string;
begin
  LanguageItem := TranslationLanguage.Language;

  TargetFilename := TPath.Combine(FOutputFolder, TPath.GetFileName(FProject.SourceFilename));
  TargetFilename := LocalizationTools.BuildModuleFilename(TargetFilename, LanguageItem, FModuleNameScheme);
  Message(Format('Building resource module for %s: %s...', [LanguageItem.LanguageName, TPath.GetFileName(TargetFilename)]));

  if (not TDirectory.Exists(FOutputFolder)) then
  begin
    if (FVerbose) then
      Message('Creating output folder...');
    TDirectory.CreateDirectory(FOutputFolder);
  end;

  ProjectProcessor := TProjectResourceProcessor.Create;
  try
    ResourceWriter := TResourceModuleWriter.Create(TargetFilename);
    try

      ProjectProcessor.IncludeVersionInfo := FIncludeVersionInfo;

      ProjectProcessor.Execute(liaTranslate, FProject, FProject.SourceFilename, TranslationLanguage, ResourceWriter);

    finally
      ResourceWriter := nil;
    end;
  finally
    ProjectProcessor.Free;
  end;
end;

procedure TLocalizationCommandLineTool.LoadFromFile(const Filename: string);
var
  LoadProperties: TLocalizationLoadProperties;
  LanguageItem: TLanguageItem;
  i: integer;
  CountItem, CountProperty: integer;
begin
  Message(Format('Loading project: %s...', [TPath.GetFileNameWithoutExtension(Filename)]));

  if (not TFile.Exists(Filename)) then
    Error(Format('Project file not found: %s', [Filename]));

  FProject.BeginUpdate;
  try

    TLocalizationProjectFiler.LoadFromFile(FProject, LoadProperties, Filename);

  finally
    FProject.EndUpdate;
  end;

  // Make source filename absolute
  FProject.SourceFilename := PathUtil.PathCombinePath(TPath.GetDirectoryName(Filename), FProject.SourceFilename);

  if (FVerbose) then
  begin
    LanguageItem := FProject.SourceLanguage;
    Message('Project information:');
    Message(Format('  Source file    : %s', [FProject.SourceFilename]));
    Message(Format('  Source Language: Locale=%s (%.4X), Name=%s', [LanguageItem.LocaleName, LanguageItem.LocaleID, LanguageItem.LanguageName]));
    Message(Format('  Symbol file    : %s', [FProject.StringSymbolFilename]));
    for i := 0 to FProject.TranslationLanguages.Count-1 do
    begin
      LanguageItem := FProject.TranslationLanguages[i].Language;
      Message(Format('  Target language: Translated=%6.0n, Locale=%s (%.4X), Name=%s', [FProject.TranslationLanguages[i].TranslatedCount*1.0, LanguageItem.LocaleName, LanguageItem.LocaleID, LanguageItem.LanguageName]));
    end;
    CountItem := 0;
    CountProperty := 0;
    FProject.Traverse(
      function(Item: TLocalizerItem): boolean
      begin
        Inc(CountItem);
        Item.Traverse(
          function(Prop: TLocalizerProperty): boolean
          begin
            Inc(CountProperty);
            Result := True;
          end);
        Result := True;
      end);
    Message(Format('  Modules        : %6.0n', [FProject.Modules.Count*1.0]));
    Message(Format('  Items          : %6.0n', [CountItem*1.0]));
    Message(Format('  Properties     : %6.0n', [CountProperty*1.0]));
  end;
end;

{ TCommandLineLogger }

procedure TCommandLineLogger.Error(const Msg: string);
var
  s: string;
begin
  for s in Format('Error: %s', [Msg]).Split([#13]) do
    Writeln(ErrOutput, s);
  Halt(1);
end;

procedure TCommandLineLogger.Message(const Msg: string);
var
  s: string;
begin
  for s in Msg.Split([#13]) do
    Writeln(Output, s);
end;

procedure TCommandLineLogger.Warning(const Msg: string);
var
  s: string;
begin
  for s in Format('Warning: %s', [Msg]).Split([#13]) do
    Writeln(ErrOutput, s);
end;

end.
