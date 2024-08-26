unit amLocalization.Portable;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  amLocalization.Settings;

procedure SavePortableSettingsFile(Settings: TTranslationManagerSettings);


implementation

uses
  UITypes,
  SysUtils,
  IOUtils,
  Dialogs,
  Controls,
  Classes,
  amFileUtils,
  amLocalization.Environment;


// -----------------------------------------------------------------------------

procedure SavePortableSettingsFile(Settings: TTranslationManagerSettings);
begin
  // Save registry branch to external file
  var Filename := TPath.ChangeExtension(ParamStr(0), '.portable');

  try
    SafeReplaceFile(Filename,
      function(const Filename: string): boolean
      var
        Stream: TStream;
      begin
        Stream := TFileStream.Create(Filename, fmCreate);
        try
          Settings.SaveRegistryToStream(Stream);
        finally
          Stream.Free;
        end;
        Result := True;
      end, False);

    // Enable this once we're confident that everything else works...
    (*
    if (Settings.System.PortablePurge) then
      Settings.Registry.EraseSection('');
    *)

  except
    on E: Exception do
      TaskMessageDlg('Portable Configuration', Format('Unable to save the portable configuration.'#13#13+
        'Error: %s', [E.Message]), mtWarning, [mbOK], 0);
  end;
end;

// -----------------------------------------------------------------------------

procedure OnSettingsCreatingHandler(Settings: TTranslationManagerSettings);
const
  sRegKeyPortable = 'Portable';
begin
  Settings.System.Portable := (FindCmdLineSwitch('portable', True)) or (TFile.Exists(TPath.GetDirectoryName(ParamStr(0)) + '\portable'));

  if (not Settings.System.Portable) then
    exit;

  // Redirect the App Data environment var to the Install folder
  EnvironmentVars['DATA'] := EnvironmentVars['INSTALL'];

  var PortableConfigFilename := TPath.ChangeExtension(ParamStr(0), '.portable');

  // Fall back to normal registry if the portable config file doesn't exist.
  // This can happen if we have just switched to portable mode and the settings
  // hasn't been saved yet (e.g. first run after restart) or if we have enabled
  // portable mode via the command line.
  if (not TFile.Exists(PortableConfigFilename)) then
    exit;

  // Switch to portable registry path
  Settings.RegistryPath := TranslationManagerPortableRegistryRoot;

  // Restore registry branch from external file
  try

    var Stream := TFileStream.Create(PortableConfigFilename, fmOpenRead);
    try
      Settings.LoadRegistryFromStream(Stream);
    finally
      Stream.Free;
    end;

  except
    on E: Exception do
    begin
      var Msg := E.Message;
      if (Settings.Registry.LastErrorMsg <> '') then
        Msg := Msg + '('+Settings.Registry.LastErrorMsg+')';

      var Res := TaskMessageDlg('Portable Configuration', Format('Unable to restore the portable configuration.'#13+
        'Would you like to reset the configuration?'#13#13+
        'Error: %s', [Msg]), mtWarning, [mbYes, mbNo, mbCancel], 0, mbNo);

      if (Res = mrCancel) then
        Halt(0);

      // Delete current settings to use default
      if (Res = mrYes) then
      begin
        // Save copy of registry tree
        SafeReplaceFile(TPath.ChangeExtension(PortableConfigFilename, '.settings'),
          function(const Filename: string): boolean
          begin
            var Stream := TFileStream.Create(Filename, fmCreate);
            try
              Settings.SaveRegistryToStream(Stream);
            finally
              Stream.Free;
            end;
            Result := True;
          end, False);

        Settings.Registry.EraseSection('');
        // Recreate root key so we can write to it.
        Settings.RegistryPath := Settings.RegistryPath;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure OnSettingsDestroyingHandler(Settings: TTranslationManagerSettings);
begin
  if (Settings.System.Portable) then
    SavePortableSettingsFile(Settings);
end;

// -----------------------------------------------------------------------------

initialization
  Assert(not TranslationManagerSettingsLoaded);

//  if (FindCmdLineSwitch('portable', True)) or (TFile.Exists(TPath.GetDirectoryName(ParamStr(0)) + '\portable')) then
  begin
    TTranslationManagerSettings.OnSettingsCreating := OnSettingsCreatingHandler;
    TTranslationManagerSettings.OnSettingsDestroying := OnSettingsDestroyingHandler;
  end;
end.
