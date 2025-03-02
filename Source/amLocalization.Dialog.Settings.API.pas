unit amLocalization.Dialog.Settings.API;

interface

uses
  dxSpellChecker,
  dxRibbonSkins;

type
  IDialogSettings = interface
    ['{F68E307F-E314-460B-83B1-BA42CE30182D}']

    function Execute(ASpellChecker: TdxSpellChecker; ARibbonStyle: TdxRibbonStyle): boolean;

    function GetRestartRequired: boolean;
    property RestartRequired: boolean read GetRestartRequired;

  end;

implementation

end.
