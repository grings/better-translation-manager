unit amLocalization.Dialog.SelectModule.API;

interface

uses
  amLocalization.Model;

type
  IDialogSelectModule = interface
    ['{DA1CEC93-AFAD-4810-B4CB-715CFBD250FA}']

    function Execute(Project: TLocalizerProject; const Title: string = ''; const Prompt: string = ''): TLocalizerModule;

  end;

implementation

end.
