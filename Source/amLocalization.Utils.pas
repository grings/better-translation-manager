unit amLocalization.Utils;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  Windows,
  amLanguageInfo;

type
  TModuleNameScheme = (
    mnsISO639_2, // Actually, not ISO639-2 since the Delphi RTL uses LOCALE_SABBREVLANGNAME to generate the identifier
    mnsISO639_1,
    mnsRFC4646
  );

type
  LocalizationTools = record
    class function LoadResourceModule(LanguageItem: TLanguageItem): boolean; overload; static;
    class function LoadResourceModule(LocaleID: LCID): boolean; overload; static;
    class function LoadResourceModule(const LocaleName: string): boolean; overload; static;
    class function BuildModuleFilename(const BaseFilename: string; LanguageItem: TLanguageItem; ModuleNameScheme: TModuleNameScheme): string; overload; static;
    class function BuildModuleFilename(const BaseFilename: string; LocaleID: LCID; ModuleNameScheme: TModuleNameScheme): string; overload; static;
    class function BuildModuleFilename(const BaseFilename: string; const LocaleName: string; ModuleNameScheme: TModuleNameScheme): string; overload; static;
  end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  Math,
  UITypes,
  Dialogs,
  Controls,
  SysUtils,
  IOUtils,
  amVersionInfo;

// -----------------------------------------------------------------------------

class function LocalizationTools.BuildModuleFilename(const BaseFilename: string;
  LanguageItem: TLanguageItem; ModuleNameScheme: TModuleNameScheme): string;
begin
  case ModuleNameScheme of

    mnsISO639_2:
      Result := TPath.ChangeExtension(BaseFilename, '.'+LanguageItem.LanguageShortName);

    mnsISO639_1:
      Result := TPath.ChangeExtension(BaseFilename, '.'+LanguageItem.ISO639_1Name);

    mnsRFC4646:
      Result := TPath.ChangeExtension(BaseFilename, '.'+LanguageItem.LocaleName);

  end;
end;

class function LocalizationTools.BuildModuleFilename(const BaseFilename: string;
  LocaleID: LCID; ModuleNameScheme: TModuleNameScheme): string;
begin
  var LanguageItem := LanguageInfo.FindLCID(LocaleID);

  if (LanguageItem <> nil) then
    Result := BuildModuleFilename(BaseFilename, LanguageItem, ModuleNameScheme)
  else
    Result := '';
end;

class function LocalizationTools.BuildModuleFilename(const BaseFilename: string;
  const LocaleName: string; ModuleNameScheme: TModuleNameScheme): string;
begin
  var LanguageItem := LanguageInfo.FindLocaleName(LocaleName);

  if (LanguageItem <> nil) then
    Result := BuildModuleFilename(BaseFilename, LanguageItem, ModuleNameScheme)
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

class function LocalizationTools.LoadResourceModule(LanguageItem: TLanguageItem): boolean;
var
  ModuleFilename: string;
const
  // Do not localize - localizations has not yet been loaded
  sResourceModuleOutOfSync = 'The resource module for the current language (%s) appears to be out of sync with the application.'+#13#13+
    'Application version: %s'+#13+
    'Resource module version: %s';
  sResourceModuleTooOld = 'The resource module for the current language (%s) is older than the application (by %d days).';
  sResourceModuleFallback = #13#13+'The default language will be used instead.';
const
{$ifdef DEBUG}
  MaxAgeDifference = 1; // Allow module being a bit out of date during development
{$else DEBUG}
  MaxAgeDifference = 0;
{$endif DEBUG}
begin
  Result := False;

  // LanguageItem=nil means "Use default"
  if (LanguageItem <> nil) then
  begin
    var Module := LoadNewResourceModule(LanguageItem, ModuleFilename);

    Result := (Module <> 0) and (ModuleFilename <> '');

    // Verify VersionInfo
    if (Result) then
    begin
      var ApplicationVersion := TVersionInfo.FileVersionString(ParamStr(0));
      // Note: GetModuleFileName (used by GetModuleName) can not be used with modules loaded with LOAD_LIBRARY_AS_DATAFILE
      var ModuleVersion := TVersionInfo.FileVersionString(ModuleFilename);

      if (ApplicationVersion <> ModuleVersion) then
      begin
        Result := False;
        MessageDlg(Format(sResourceModuleOutOfSync, [LanguageItem.LanguageName, ApplicationVersion, ModuleVersion])+sResourceModuleFallback, mtWarning, [mbOK], 0);
      end;
    end;

    // Verify timestamp
    if (Result) then
    begin
      var AgeDifference := Ceil(TFile.GetLastWriteTime(ParamStr(0)) - TFile.GetLastWriteTime(ModuleFilename));

      if (AgeDifference > MaxAgeDifference) then
      begin
        Result := False;
        MessageDlg(Format(sResourceModuleTooOld, [LanguageItem.LanguageName, AgeDifference])+sResourceModuleFallback, mtWarning, [mbOK], 0);
      end;
    end;
  end;

  if (not Result) then
    // Use default application language if we failed to load a resource module
    LoadNewResourceModule(nil, ModuleFilename);
end;

class function LocalizationTools.LoadResourceModule(LocaleID: LCID): boolean;
const
  // Do not localize - localizations has not yet been loaded
  sResourceModuleUnknownLanguage = 'Unknown language ID: %d'+#13#13+
    'The default language will be used instead.';
begin
  var LanguageItem: TLanguageItem := nil;

  // LocaleID=0 means "Use default"
  if (LocaleID <> 0) then
  begin
    LanguageItem := LanguageInfo.FindLCID(LocaleID);

    if (LanguageItem = nil) then
      MessageDlg(Format(sResourceModuleUnknownLanguage, [LocaleID]), mtWarning, [mbOK], 0);
  end;

  Result := LoadResourceModule(LanguageItem);
end;

class function LocalizationTools.LoadResourceModule(const LocaleName: string): boolean;
const
  // Do not localize - localizations has not yet been loaded
  sResourceModuleUnknownLanguage = 'Unknown language: %s'+#13#13+
    'The default language will be used instead.';
begin
  var LanguageItem: TLanguageItem := nil;

  // LocaleName='' means "Use default"
  if (LocaleName <> '') then
  begin
    LanguageItem := LanguageInfo.FindLocaleName(LocaleName);

    if (LanguageItem = nil) then
      MessageDlg(Format(sResourceModuleUnknownLanguage, [LocaleName]), mtWarning, [mbOK], 0);
  end;

  Result := LoadResourceModule(LanguageItem);
end;

end.
