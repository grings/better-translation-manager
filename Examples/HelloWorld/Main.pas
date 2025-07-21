unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus;

type
  TFormMain = class(TForm)
    ListBoxValues: TListBox;
    PanelTop: TPanel;
    LabelValue: TLabel;
    EditValue: TEdit;
    ButtonAdd: TButton;
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemLanguage: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ButtonAddClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
  private
    procedure SelectLanguage(Sender: TObject);
    procedure PopulateLanguagesMenu;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  UITypes,
  IOUtils,
  Registry,
  amLanguageInfo,
  amVersionInfo,
  amLocalization.Utils;

procedure TFormMain.ButtonAddClick(Sender: TObject);
begin
  ListBoxValues.Items.Insert(0, EditValue.Text);
  EditValue.Text := '';
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  PopulateLanguagesMenu;
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
resourcestring
  sCloseQuery = 'Are you sure you want to exit the application?';
begin
  CanClose := (MessageDlg(sCloseQuery, mtConfirmation, [mbYes, mbNo], 0, mbNo) = mrYes);
end;

procedure TFormMain.PopulateLanguagesMenu;

  function IsLanguageModule(const Filename: string; LanguageItem: TLanguageItem; var LocaleName: string): boolean;
  var
    ModuleNameScheme: TModuleNameScheme;
    BaseFilename: string;
    ModuleFilename: string;
  begin
    Result := False;
    BaseFilename := TPath.GetFileName(Filename);
    for ModuleNameScheme := Low(TModuleNameScheme) to High(TModuleNameScheme) do
    begin
      ModuleFilename := LocalizationTools.BuildModuleFilename(BaseFilename, LanguageItem, ModuleNameScheme);

      if (SameText(BaseFilename, ModuleFilename)) then
      begin
        LocaleName := TPath.GetExtension(BaseFilename);
        Delete(LocaleName, 1, 1); // Remove the '.'
        Result := True;
        break;
      end;
    end;
  end;

  procedure AddLanguage(const Caption, LocaleName: string; Default: boolean); overload;
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := Caption;
    MenuItem.Hint := LocaleName;
    MenuItem.Default := Default;
    MenuItem.RadioItem := True;
    MenuItem.Checked := SameText(LocaleName, GetLocaleOverride(Application.ExeName));
    MenuItem.OnClick := SelectLanguage;

    MenuItemLanguage.Add(MenuItem);
  end;

  procedure AddLanguage(LanguageItem: TLanguageItem; const LocaleName: string; Default: boolean); overload;
  begin
    AddLanguage(LanguageItem.DisplayName, LocaleName, Default);
  end;

var
  FileMask: string;
  Filename: string;
  LocaleName: string;
  VersionInfo: TVersionInfo;
  DefaultLanguageItem: TLanguageItem;
  NativeLanguageItem: TLanguageItem;
  LanguageItem: TLanguageItem;
const
  sDefaultNativeLocale = 'en-US';  // LCID: $00000409
resourcestring
  sDefaultLanguage = 'Automatic: %s';
begin
  // Populate sub menu with available languages
  MenuItemLanguage.Clear;

  DefaultLanguageItem := LanguageInfo.FindLCID(GetThreadUILanguage);

  VersionInfo := TVersionInfo.Create(Application.ExeName);
  try
    if (VersionInfo.TranslationCount > 0) then
      // Get native language from VersionInfo
      NativeLanguageItem := LanguageInfo.FindLCID(VersionInfo.LanguageID[0])
    else
      // Fall back to en-US (or whatever)
      NativeLanguageItem := LanguageInfo.FindLocaleName(sDefaultNativeLocale);
  finally
    VersionInfo.Free;
  end;

  if (NativeLanguageItem <> nil) then
    AddLanguage(NativeLanguageItem, NativeLanguageItem.LocaleName, (NativeLanguageItem = DefaultLanguageItem));

  // Look for files with the same name as the application
  FileMask := TPath.ChangeExtension(TPath.GetFileName(Application.ExeName), '.*');
  for Filename in TDirectory.GetFiles(TPath.GetDirectoryName(Application.ExeName), FileMask) do
    // For each filename see if it matches a language module filename for any of the known locales
    for LanguageItem in LanguageInfo do
    begin
      if (LanguageItem = NativeLanguageItem) then
        continue; // We already have that one in the menu

      if (IsLanguageModule(Filename, LanguageItem, LocaleName)) then
      begin
        // Found a match. Add a menu item.
        AddLanguage(LanguageItem, LocaleName, (LanguageItem = DefaultLanguageItem));
        break;
      end;
    end;

  MenuItemLanguage.NewBottomLine;
  // User default language
  if (DefaultLanguageItem <> nil) then
    AddLanguage(Format(sDefaultLanguage, [DefaultLanguageItem.LanguageName]), '', False);
end;

procedure TFormMain.SelectLanguage(Sender: TObject);
var
  Reg: TRegIniFile;
  LocaleName: string;
resourcestring
  sRestart = 'Please restart the application to use the selected language module';
begin
  LocaleName := TMenuItem(Sender).Hint;

  Reg := TRegIniFile.Create('\Software\Embarcadero', KEY_ALL_ACCESS);
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if (LocaleName <> '') then
      // Use selected locale
      Reg.WriteString('Locales', Application.ExeName, LocaleName)
    else
      // Use system/user default locale
      Reg.DeleteKey('Locales', Application.ExeName);
  finally
    Reg.Free;
  end;

  TMenuItem(Sender).Checked := True;

  ShowMessage(sRestart);
end;

end.
