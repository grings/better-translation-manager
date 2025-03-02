unit amLocalization.Dialog.Languages.API;

interface

uses
  amLanguageInfo;

type
  IDialogLanguages = interface
    ['{C68A8612-CD0A-43BF-8759-DF95EBF3FF88}']

    function Execute: boolean;

    function SelectTargetLanguage(Language: TLanguageItem): boolean;
    procedure ClearTargetLanguages;

    function GetApplyFilter: boolean;
    procedure SetApplyFilter(const Value: boolean);
    property ApplyFilter: boolean read GetApplyFilter write SetApplyFilter;

    function GetSourceLanguage: TLanguageItem;
    procedure SetSourceLanguage(const Value: TLanguageItem);
    property SourceLanguage: TLanguageItem read GetSourceLanguage write SetSourceLanguage;

    function GetTargetLanguageCount: integer;
    property TargetLanguageCount: integer read GetTargetLanguageCount;

    function GetTargetLanguage(Index: integer): TLanguageItem;
    property TargetLanguage[Index: integer]: TLanguageItem read GetTargetLanguage;

  end;

implementation

end.
