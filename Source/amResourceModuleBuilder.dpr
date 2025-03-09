program amResourceModuleBuilder;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

{$APPTYPE CONSOLE}

{$R amTranslationManager.res}


{$R 'EmptyResourceModule.res' 'resources\EmptyResourceModule.rc'}

uses
  {$ifdef MADEXCEPT}
  madExcept,
  {$endif MADEXCEPT}
  System.SysUtils,
  amLocalization.CommandLine;

var
  CommandLineTool: TLocalizationCommandLineTool;
begin
  CommandLineTool := TLocalizationCommandLineTool.Create(TCommandLineLogger.Create);
  try

    CommandLineTool.Execute;

  finally
    CommandLineTool.Free;
  end;
end.
