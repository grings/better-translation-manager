unit amLocalization.Dialog.TranslationMemory;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

{$WARN SYMBOL_PLATFORM OFF}

uses
  Generics.Collections,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.Actions, Vcl.ActnList, Vcl.ExtCtrls,
  Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, dxSkinsCore,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, dxDateRanges,
  cxDataControllerConditionalFormattingRulesManagerDialog, cxDBData, dxLayoutControlAdapters, dxLayoutContainer,
  cxButtons, dxLayoutControl, cxGridLevel, cxClasses, cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, cxEditRepositoryItems, dxLayoutcxEditAdapters, cxContainer, cxLabel,
  dxBarBuiltInMenu, cxGridCustomPopupMenu, cxGridPopupMenu, cxCheckBox, cxCheckComboBox, cxTextEdit, cxMaskEdit, cxDropDownEdit,

  amLanguageInfo,
  amLocalization.Normalization,
  amLocalization.Dialog,
  amLocalization.TranslationMemory;

type
  ITranslationMemoryFormTools = interface
    ['{D76E9F2C-9CF9-42CF-9092-625B809708F0}']
    function Locate(Language: TLanguageItem; const Value: string): boolean;
    function LocatePair(LanguageA: TLanguageItem; const ValueA: string; LanguageB: TLanguageItem; const ValueB: string): boolean;
  end;

// -----------------------------------------------------------------------------
//
//              TFormTranslationMemory
//
// -----------------------------------------------------------------------------
type
  TFormTranslationMemory = class;

  TFormTranslationMemoryCallback = reference to function(FormTranslationMemory: TFormTranslationMemory): boolean;

  TFormTranslationMemory = class(TFormDialog, ITranslationMemoryFormTools)
    GridTMDBTableView: TcxGridDBTableView;
    GridTMLevel: TcxGridLevel;
    GridTM: TcxGrid;
    dxLayoutItem2: TdxLayoutItem;
    OpenDialogTMX: TOpenDialog;
    SaveDialogTMX: TSaveDialog;
    dxLayoutItem1: TdxLayoutItem;
    ButtonClose: TcxButton;
    dxLayoutItem3: TdxLayoutItem;
    ButtonSaveAs: TcxButton;
    dxLayoutItem4: TdxLayoutItem;
    ButtonLoad: TcxButton;
    GridPopupMenu: TcxGridPopupMenu;
    PopupMenuHeader: TPopupMenu;
    MenuItemDeleteLanguage: TMenuItem;
    ActionDeleteLanguage: TAction;
    TaskDialogOpen: TTaskDialog;
    ActionExport: TAction;
    ActionImport: TAction;
    DataSourceTranslationMemory: TDataSource;
    ActionViewDuplicates: TAction;
    Findduplicates1: TMenuItem;
    N1: TMenuItem;
    dxLayoutItem5: TdxLayoutItem;
    ComboBoxLanguages: TcxComboBox;
    dxLayoutItem6: TdxLayoutItem;
    ComboBoxOptions: TcxCheckComboBox;
    LayoutGroupDuplicates: TdxLayoutGroup;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    PopupMenuGrid: TPopupMenu;
    Viewduplicates1: TMenuItem;
    ActionRowInsert: TAction;
    ActionRowDelete: TAction;
    N2: TMenuItem;
    Insertrow1: TMenuItem;
    Deleterows1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure GridTMDBTableViewCustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure ActionDeleteLanguageExecute(Sender: TObject);
    procedure ActionDeleteLanguageUpdate(Sender: TObject);
    procedure GridPopupMenuPopupMenus0Popup(ASenderMenu: TComponent; AHitTest: TcxCustomGridHitTest; X, Y: Integer);
    procedure ActionExportExecute(Sender: TObject);
    procedure ActionImportExecute(Sender: TObject);
    procedure GridTMDBTableViewInitEdit(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit);
    procedure ActionViewDuplicatesExecute(Sender: TObject);
    procedure ComboBoxLanguagesPropertiesChange(Sender: TObject);
    procedure ComboBoxOptionsPropertiesChange(Sender: TObject);
    procedure LayoutGroupDuplicatesButton0Click(Sender: TObject);
    procedure GridTMDBTableViewDataControllerFilterRecord(ADataController: TcxCustomDataController; ARecordIndex: Integer;
      var Accept: Boolean);
    procedure ActionImportUpdate(Sender: TObject);
    procedure ActionExportUpdate(Sender: TObject);
    procedure ActionViewDuplicatesUpdate(Sender: TObject);
    procedure ActionRowInsertExecute(Sender: TObject);
    procedure ActionRowInsertUpdate(Sender: TObject);
    procedure ActionRowDeleteExecute(Sender: TObject);
    procedure ActionRowDeleteUpdate(Sender: TObject);
    procedure GridTMDBTableViewColumnPosChanged(Sender: TcxGridTableView; AColumn: TcxGridColumn);
    procedure GridTMDBTableViewStylesGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
      AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
    procedure GridTMDBTableViewCellDblClick(Sender: TcxCustomGridTableView;
      ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
      AShift: TShiftState; var AHandled: Boolean);
  private
    FTranslationMemory: ITranslationMemory;
    FPoupMenuColumn: TcxGridColumn;
    FRightToLeft: array of boolean;
  private type
    TTranslationMemoryUpdate = (tmCreateColumns, tmUpdateRightToLeft, tmUpdateDuplicates);
    TTranslationMemoryUpdates = set of TTranslationMemoryUpdate;
  private
    FUpdateCount: integer;
    FUpdates: TTranslationMemoryUpdates;
  protected
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure NeedUpdate(Update: TTranslationMemoryUpdate);
  private
    FViewDuplicates: boolean;
    FDuplicateColumn: TcxGridDBColumn;
    FDuplicates: TTranslationMemoryRecordList; // [RecordIndex] = DuplicateIndex
    FDuplicateLanguageItem: TLanguageItem;
    FDuplicateSourceColumn: TcxGridColumn;
    FSanitizeRules: TSanitizeRules;
    FLookupIndex: ITranslationMemoryLookup;
  protected
    procedure GridTMDBTableViewColumnDuplicateGetDataText(Sender: TcxCustomGridTableItem; ARecordIndex: Integer; var AText: string);
    procedure DoUpdateDuplicates;
    procedure DoCreateColumns;
    procedure DoUpdateRightToLeft;
  protected
    procedure SaveLayout;
    procedure RestoreLayout;
  protected
    // ITranslationMemoryFormTools
    function Locate(Language: TLanguageItem; const Value: string): boolean;
    function LocatePair(LanguageA: TLanguageItem; const ValueA: string; LanguageB: TLanguageItem; const ValueB: string): boolean;
  public
    destructor Destroy; override;

    function Execute(ATranslationMemory: ITranslationMemory; CallBack: TFormTranslationMemoryCallback = nil): boolean;
  end;

resourcestring
  sTranslationMemoryAddCompleteTitle = 'Translation added';
  sTranslationMemoryUpdateCompleteTitle = 'Translation added';
  sTranslationMemoryMergeCompleteTitle = 'Merge completed';
  sTranslationMemoryMergeComplete = 'The translations were added to the Translation Memory.'#13#13+
    'Values added: %.0n'#13+
    'Values merged: %.0n'#13+
    'Values skipped: %.0n'#13+
    'Duplicate values: %.0n';
  sTranslationMemoryOpenCompleteTitle = 'Translation Memory opened';
  sTranslationMemoryOpenComplete = 'Values read: %.0n';

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  System.Generics.Defaults,
  IOUtils,
  UITypes,
  cxDrawTextUtils,
  dxCore,
  amCursorService,
  amProgress.API,
  amLocalization.Settings,
  amLocalization.Data.Main,
  amLocalization.Dialog.TranslationMemory.SelectFileFormat,
  amLocalization.TranslationMemory.Data,
  amLocalization.TranslationMemory.FileFormats,
  amLocalization.TranslationMemory.FileFormats.TMX;

// -----------------------------------------------------------------------------
//
//              TFormTranslationMemory
//
// -----------------------------------------------------------------------------
destructor TFormTranslationMemory.Destroy;
begin
  FDuplicates.Free;

  inherited;
end;

procedure TFormTranslationMemory.FormCreate(Sender: TObject);
begin
  OpenDialogTMX.Filter := TTranslationMemoryFileFormat.FileFormatFileFilters(ffcLoad) + OpenDialogTMX.Filter;
  SaveDialogTMX.Filter := TTranslationMemoryFileFormat.FileFormatFileFilters(ffcSave) + SaveDialogTMX.Filter;
  GridTMDBTableView.DataController.CreateAllItems(True);
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.BeginUpdate;
begin
  Inc(FUpdateCount);
  if (FUpdateCount = 1) then
    GridTMDBTableView.BeginUpdate;
end;

procedure TFormTranslationMemory.EndUpdate;
begin
  if (FUpdateCount = 1) then
  begin
    SaveCursor(crAppStart);

    if (tmUpdateDuplicates in FUpdates) then
      DoUpdateDuplicates;

    if (tmCreateColumns in FUpdates) then
      DoCreateColumns; // Must be before check for tmUpdateRightToLeft

    if (tmUpdateRightToLeft in FUpdates) then
      DoUpdateRightToLeft;

    if (DataSourceTranslationMemory.DataSet.RecordCount > 100*1024) then
      SaveCursor(crHourGlass, True);

    GridTMDBTableView.EndUpdate; // This takes ages if the dataset is very large (hence the SaveCursor above)
  end;
  Dec(FUpdateCount);
end;

procedure TFormTranslationMemory.NeedUpdate(Update: TTranslationMemoryUpdate);
begin
  BeginUpdate;
  Include(FUpdates, Update);
  EndUpdate;
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.RestoreLayout;
begin
  if (TranslationManagerSettings.Layout.TranslationMemory.Valid) then
    GridTMDBTableView.RestoreFromRegistry(TranslationManagerSettings.Layout.KeyPath, False, False, [gsoUseFilter], TranslationManagerSettings.Layout.TranslationMemory.Name);
end;

procedure TFormTranslationMemory.SaveLayout;
begin
  GridTMDBTableView.StoreToRegistry(TranslationManagerSettings.Layout.KeyPath, False, [gsoUseFilter], TranslationManagerSettings.Layout.TranslationMemory.Name);
  TranslationManagerSettings.Layout.TranslationMemory.Valid := True;
end;

// -----------------------------------------------------------------------------

function TFormTranslationMemory.Execute(ATranslationMemory: ITranslationMemory; CallBack: TFormTranslationMemoryCallback): boolean;
begin
  SaveCursor(crAppStart);

  FTranslationMemory := ATranslationMemory;

  try

    if (not FTranslationMemory.CheckLoaded(True)) then
      Exit(false);

  except
    on E: ETranslationMemory do
    begin
      if (MessageDlg(E.Message, mtWarning, [mbIgnore, mbAbort], 0, mbAbort) = mrAbort) then
        Exit(False)
    end;
  end;

  BeginUpdate;
  try
    DataSourceTranslationMemory.DataSet := FTranslationMemory.TranslationMemoryDataSet;

    NeedUpdate(tmCreateColumns);
  finally
    EndUpdate;
  end;

  BeginUpdate; // Do not merge with BeginUpdate/EndUpdate block above. We need the columns before we restore.
  try
    // Column order can change when we restore the layout so we need to rebuild the RTL array
    RestoreLayout;

    NeedUpdate(tmUpdateRightToLeft);
  finally
    EndUpdate;
  end;

  if (Assigned(CallBack)) and (not CallBack(Self)) then
    Exit(False);

  ShowModal;

  // Make sure duplicate column doesn't affect saved layout
  FreeAndNil(FDuplicateColumn); // Must use free and nil. Free causes repaint. Repaint references object.

  SaveLayout;

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.ActionDeleteLanguageExecute(Sender: TObject);
var
  LanguageItem: TLanguageItem;
  Field: TField;
  Clone: IInterface;
resourcestring
  sDeleteLanguage = 'Are you sure you want to delete the "%s" language from the Translation Memory?';
begin
  Field := TcxGridDBColumn(FPoupMenuColumn).DataBinding.Field;
  LanguageItem := FTranslationMemory.FieldToLanguage(Field);
  Assert(LanguageItem <> nil);

  if (MessageDlg(Format(sDeleteLanguage, [LanguageItem.LanguageName]), mtConfirmation, [mbYes, mbNo], 0, mbNo) <> mrYes) then
    Exit;

  SaveCursor(crHourGlass);

  BeginUpdate;
  try
    // Start by removing the column
    FPoupMenuColumn.Free;

    // Then remove the field
    if (FTranslationMemory.TranslationMemoryDataSet.Fields.Count > 1) then
    begin
      Clone := FTranslationMemory.SaveTableTranslationMemoryClone;
      try
        Field.Free;
      finally
        Clone := nil;
      end;
    end else
      Field.Free;
  finally
    EndUpdate;
  end;
end;

procedure TFormTranslationMemory.ActionDeleteLanguageUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not FViewDuplicates) and (FPoupMenuColumn <> nil);
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.ActionExportUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not FViewDuplicates);
end;

procedure TFormTranslationMemory.ActionExportExecute(Sender: TObject);
var
  TranslationMemoryFileFormatClass: TTranslationMemoryFileFormatClass;
  TranslationMemoryFileFormat: TTranslationMemoryFileFormat;
begin
  if (SaveDialogTMX.InitialDir = '') then
    SaveDialogTMX.InitialDir := TranslationManagerSettings.Folders.FolderDocuments;
  SaveDialogTMX.FileName := TPath.GetFileName(SaveDialogTMX.FileName);

  if (not SaveDialogTMX.Execute(Handle)) then
    Exit;

  SaveCursor(crHourGlass);

  TranslationMemoryFileFormatClass := TTranslationMemoryFileFormat.FindFileFormat(SaveDialogTMX.FileName, ffcSave, TTranslationMemoryFileFormatTMX);
  Assert(TranslationMemoryFileFormatClass <> nil);

  TranslationMemoryFileFormat := TranslationMemoryFileFormatClass.Create(FTranslationMemory);
  try

    TranslationMemoryFileFormat.SaveToFile(SaveDialogTMX.FileName);

  finally
    TranslationMemoryFileFormat.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.ActionViewDuplicatesUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FViewDuplicates;
end;

procedure TFormTranslationMemory.ActionViewDuplicatesExecute(Sender: TObject);
var
  Languages: TArray<TLanguageItem>;
  LanguageItem: TLanguageItem;
const
  SanitizeKinds: TSanitizeRules = [Low(TSanitizeRule)..High(TSanitizeRule)];
resourcestring
  sDuplicate = 'Duplicate';
begin
  FViewDuplicates := not FViewDuplicates;
  LayoutGroupDuplicates.Visible := FViewDuplicates;

  BeginUpdate;
  try
    if (not FViewDuplicates) then
    begin
      if (FDuplicateColumn <> nil) then
      begin
        FDuplicateColumn.SortOrder := soNone;
        FDuplicateColumn.Visible := False;
        FDuplicates.Clear;
      end;
      Exit;
    end;

    SaveCursor(crHourGlass);

    if (FDuplicateColumn = nil) then
    begin
      FDuplicateColumn := GridTMDBTableView.CreateColumn;
      FDuplicateColumn.DataBinding.ValueTypeClass := TcxStringValueType;
      FDuplicateColumn.VisibleForCustomization := False;
      FDuplicateColumn.Options.Editing := False;
      FDuplicateColumn.Options.Focusing := False;
      FDuplicateColumn.FixedKind := fkLeft;
      FDuplicateColumn.Caption := sDuplicate;
      FDuplicateColumn.Width := 150;
      FDuplicateColumn.BestFitMaxWidth := 300;
      //FDuplicateColumn.Styles.Content := StyleDuplicateOdd;
      FDuplicateColumn.OnGetDataText := GridTMDBTableViewColumnDuplicateGetDataText;

      // Column added. We must update the RTL array.
      NeedUpdate(tmUpdateRightToLeft);

      Languages := FTranslationMemory.GetLanguages;
      for LanguageItem in Languages do
        ComboBoxLanguages.Properties.Items.AddObject(LanguageItem.LanguageName, LanguageItem);

      FDuplicates := TTranslationMemoryRecordList.Create;
    end;
    FDuplicateColumn.Index := 0;

    ComboBoxLanguages.ItemIndex := 0;
    ComboBoxOptions.EditValue := Byte(SanitizeKinds);

    FDuplicateColumn.SortOrder := soAscending;
    FDuplicateColumn.SortIndex := 0;

    FDuplicateColumn.Visible := (FDuplicateSourceColumn <> nil);

    // Get data for duplicate filter
    NeedUpdate(tmUpdateDuplicates);

    // Update form so it looks nice while user wait for the rest to complete
    Update;
  finally
    EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.DoUpdateDuplicates;
var
  Values: TArray<string>;
  Value: string;
  RecNo: integer;
  List: TTranslationMemoryRecordList;
  Progress: IProgress;
  i, Index: integer;
begin
  FLookupIndex := nil;
  if (not FViewDuplicates) then
    Exit;

  Progress := ShowProgress('Finding duplicates');
  Progress.UpdateMessage('Loading values...');

  // Create an index of sanitized values
  FLookupIndex := FTranslationMemory.CreateLookup(FDuplicateLanguageItem, FSanitizeRules, Progress);

  Progress.UpdateMessage('Finding duplicates...');
  Progress.Marquee := True;

  // Get the values
  Values := FLookupIndex.GetValues;

  // Sort values so we can determine odd/even values directly from the group index
  TArray.Sort<string>(Values, TOrdinalIStringComparer.Create);

  // For each value get the record list (the duplicates)
  FDuplicates.Clear;
  FDuplicates.Count := GridTMDBTableView.DataController.RecordCount;

  for i := 0 to FDuplicates.Count-1 do
    FDuplicates[i] := -1;

  Index := 0;
  for Value in Values do
  begin
    Progress.AdvanceProgress;
    List := FLookupIndex.Lookup(Value);
    // Ignore values that does not have duplicates (i.e. less than two records per value)
    if (List = nil) or (List.Count <= 1) then
      continue;

    // Add records to duplicate list
    for RecNo in List do
      FDuplicates[RecNo-1] := Index;

    Inc(Index);
  end;

  Progress.UpdateMessage('Loading data...');

  // Must do this or the outer GridTMDBTableView.EndUpdate will take *forever*
//  DataSourceTranslationMemory.DataSet.DisableControls;
// turns out it doesn't make any difference. Something else is rotten...
  try

    GridTMDBTableView.DataController.RefreshExternalData;

  finally
//    DataSourceTranslationMemory.DataSet.EnableControls;
  end;

  Exclude(FUpdates, tmUpdateDuplicates);
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.ActionRowDeleteExecute(Sender: TObject);
var
  i: integer;
begin
  if (MessageDlg('Delete selected row(s)?', mtConfirmation, [mbYes, mbNo], 0, mbNo) <> mrYes) then
    Exit;

  SaveCursor(crAppStart);

  if (FViewDuplicates) then
  for i:= 0 to GridTMDBTableView.Controller.SelectedRecordCount-1 do
    FDuplicates.Delete(GridTMDBTableView.Controller.SelectedRecords[i].RecordIndex);

  BeginUpdate;
  try
    GridTMDBTableView.DataController.DeleteSelection;

//    NeedUpdate(tmUpdateDuplicates);
  finally
    EndUpdate;
  end;
end;

procedure TFormTranslationMemory.ActionRowDeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (GridTMDBTableView.Controller.SelectedRecordCount > 0) and (not GridTMDBTableView.DataController.IsEditing);
end;

procedure TFormTranslationMemory.ActionRowInsertExecute(Sender: TObject);
begin
  GridTMDBTableView.DataController.Insert;
end;

procedure TFormTranslationMemory.ActionRowInsertUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not FViewDuplicates) and (not GridTMDBTableView.DataController.IsEditing);
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.ActionImportUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not FViewDuplicates);
end;

procedure TFormTranslationMemory.ActionImportExecute(Sender: TObject);
var
  Merge: boolean;
  Res: Word;
  Stats, OneStats: TTranslationMemoryMergeStats;
  Filename: string;
  FileFormatClasses: TTranslationMemoryFileFormatClasses;
  FileFormatClass: TTranslationMemoryFileFormatClass;
  FileFormat: TTranslationMemoryFileFormat;
  FormSelectFileFormat: TFormSelectFileFormat;
  DuplicateAction: TTranslationMemoryDuplicateAction;
  Progress: IProgress;
  MergeFile: boolean;
resourcestring
  sLoadTranslationMemoryErrorTitle = 'Error importing Translation Memory?';
  sLoadTranslationMemoryError = 'The specified translation memory file could not be imported.'+#13#13+
      'File: %s'#13+
      'Error: %s';
  sLoadTranslationMemoryMergeTitle = 'Merge Translation Memory?';
  sLoadTranslationMemoryMerge = 'Do you want to merge the selected file(s) into your existing Translation Memory?'+#13#13+
      'If you answer No then your current Translation Memory will be emptied and replaced with the translations in the specified file.';
begin
  if (not FTranslationMemory.CheckSave) then
    Exit;

  if (OpenDialogTMX.InitialDir = '') then
    OpenDialogTMX.InitialDir := TranslationManagerSettings.Folders.FolderDocuments;

  if (not OpenDialogTMX.Execute(Handle)) then
    Exit;

  // Remember folder for next time
  OpenDialogTMX.InitialDir := TPath.GetDirectoryName(OpenDialogTMX.FileName);

  if (FTranslationMemory.TranslationMemoryDataSet.Active) and (FTranslationMemory.TranslationMemoryDataSet.RecordCount > 0) then
  begin
    Res := TaskMessageDlg(sLoadTranslationMemoryMergeTitle, sLoadTranslationMemoryMerge, mtConfirmation, [mbYes, mbNo, mbCancel], 0, mbCancel);

    if (Res = mrCancel) then
      Exit;

    Merge := (Res = mrYes);
  end else
    Merge := False;

  SaveCursor(crHourGlass);

  SaveLayout;

  BeginUpdate;
  try
    Stats := Default(TTranslationMemoryMergeStats);
    DuplicateAction := tmDupActionPrompt;
    if (OpenDialogTMX.Files.Count > 1) then
    begin
      Progress := ShowProgress(sTranslationMemoryLoading);
      Progress.EnableAbort := True;
      Progress.Progress(psBegin, 0, OpenDialogTMX.Files.Count);
      SaveCursor(crAppStart);
    end else
    begin
      Progress := nil;
      SaveCursor(crHourGlass);
    end;

    MergeFile := Merge;

    FileFormatClasses := TTranslationMemoryFileFormat.FindFileFormats(OpenDialogTMX.FileName, ffcLoad);

    if (Length(FileFormatClasses) = 0) then
      Exit;

    if (Length(FileFormatClasses) > 1) then
    begin
      FormSelectFileFormat := TFormSelectFileFormat.Create(nil);
      try
        FileFormatClass := FormSelectFileFormat.Execute(FileFormatClasses);
      finally
        FormSelectFileFormat.Free;
      end;
      if (FileFormatClass = nil) then
        Exit;
    end else
      FileFormatClass := FileFormatClasses[0];

    Assert(FileFormatClass <> nil);

    FileFormat := FileFormatClass.Create(FTranslationMemory);
    try
      for Filename in OpenDialogTMX.Files do
      begin
        if (Progress <> nil) then
          Progress.AdvanceProgress;

        if (not FileFormat.Prepare(Filename)) then
          break;

        try

          OneStats := FileFormat.LoadFromFile(Filename, DuplicateAction, MergeFile, Progress);

        except
          on E: ETranslationMemoryFileFormat do
          begin
            Res := TaskMessageDlg(sLoadTranslationMemoryErrorTitle, Format(sLoadTranslationMemoryError, [Filename, E.Message]), mtWarning, [mbAbort, mbIgnore], 0, mbAbort);
            if (Res = mrAbort) then
              break;
          end;

          on E: EAbort do
            break; // Do not propagate - just don't continue
        end;

        MergeFile := True; // Additional files will be merged regardless of choice

        Inc(Stats.Added, OneStats.Added);
        Inc(Stats.Merged, OneStats.Merged);
        Inc(Stats.Skipped, OneStats.Skipped);
        Inc(Stats.Duplicate, OneStats.Duplicate);

        if (DuplicateAction = tmDupActionAbort) or ((Progress <> nil) and (Progress.Aborted)) then
          break;
      end;
    finally
      FileFormat.Free;
    end;

    if (not Merge) then
      FTranslationMemory.SetLoaded;

    if (Progress <> nil) then
      Progress.Progress(psEnd, 1, 1);

    NeedUpdate(tmCreateColumns);

  finally
    EndUpdate;
  end;

  Progress := nil;

  RestoreLayout;

  if (Merge) then
  begin
    TaskMessageDlg(sTranslationMemoryMergeCompleteTitle,
      Format(sTranslationMemoryMergeComplete, [Stats.Added * 1.0, Stats.Merged * 1.0, Stats.Skipped * 1.0, Stats.Duplicate * 1.0]),
      mtInformation, [mbOK], 0);
  end else
  begin
    TaskMessageDlg(sTranslationMemoryOpenCompleteTitle,
      Format(sTranslationMemoryOpenComplete, [Stats.Added * 1.0]),
      mtInformation, [mbOK], 0);
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.ComboBoxLanguagesPropertiesChange(Sender: TObject);
var
  i: integer;
begin
  BeginUpdate;
  try
    if (FDuplicateColumn <> nil) then
      FDuplicateColumn.Visible := False;

    FDuplicateLanguageItem := TLanguageItem(ComboBoxLanguages.ItemObject);

    FDuplicateSourceColumn := nil;
    for i := 0 to GridTMDBTableView.ColumnCount-1 do
      if (GridTMDBTableView.Columns[i].DataBinding.Field <> nil) and
        (FTranslationMemory.FieldToLanguage(GridTMDBTableView.Columns[i].DataBinding.Field) = FDuplicateLanguageItem) then
      begin
        FDuplicateSourceColumn := GridTMDBTableView.Columns[i];
        break;
      end;

    if (FDuplicateColumn <> nil) then
      FDuplicateColumn.Visible := (FDuplicateSourceColumn <> nil);

    NeedUpdate(tmUpdateDuplicates);
  finally
    EndUpdate;
  end;
end;

procedure TFormTranslationMemory.ComboBoxOptionsPropertiesChange(Sender: TObject);
begin
  BeginUpdate;
  try
    FSanitizeRules := TSanitizeRules(Byte(ComboBoxOptions.EditValue));

    NeedUpdate(tmUpdateDuplicates);
  finally
    EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.DoCreateColumns;
var
  i: integer;
begin
  BeginUpdate;
  try
    GridTMDBTableView.ClearItems;
    GridTMDBTableView.DataController.CreateAllItems;

    for i := 0 to GridTMDBTableView.ColumnCount-1 do
    begin
      GridTMDBTableView.Columns[i].RepositoryItem := DataModuleMain.EditRepositoryTextItem;
      GridTMDBTableView.Columns[i].Width := 200;
      GridTMDBTableView.Columns[i].BestFitMaxWidth := 400;
    end;

    Exclude(FUpdates, tmCreateColumns);
    NeedUpdate(tmUpdateRightToLeft);
  finally
    EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.DoUpdateRightToLeft;
var
  i: integer;
  Column: TcxGridDBColumn;
  LanguageItem: TLanguageItem;
begin
  SetLength(FRightToLeft, GridTMDBTableView.ColumnCount);

  for i := 0 to GridTMDBTableView.ColumnCount-1 do
  begin
    Column := GridTMDBTableView.Columns[i];

    if (Column.DataBinding.Field <> nil) then
      LanguageItem := FTranslationMemory.FieldToLanguage(Column.DataBinding.Field)
    else
      LanguageItem := FDuplicateLanguageItem;

    if (LanguageItem <> nil) then
      FRightToLeft[i] := LanguageItem.IsRightToLeft
    else
      FRightToLeft[i] := IsRightToLeft;
  end;

  Exclude(FUpdates, tmUpdateRightToLeft);
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.GridPopupMenuPopupMenus0Popup(ASenderMenu: TComponent; AHitTest: TcxCustomGridHitTest; X,
  Y: Integer);
begin
  if (AHitTest is TcxGridColumnHeaderHitTest) then
  begin
    FPoupMenuColumn := TcxGridColumnHeaderHitTest(AHitTest).Column;

    // If we have an OnPopup event handler then the popupmenu isn't displayed...
    // Really intuitive logic DevExpress....
    // ...so we have to display is manually.

    PopupMenuHeader.PopupComponent := GridTMDBTableView;
    PopupMenuHeader.Popup(X, Y);
  end else
    FPoupMenuColumn := nil;
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.GridTMDBTableViewCellDblClick(
  Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo;
  AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
begin
  // Double click starts edit mode.
  // This works around DevExpress' unintuitive "very slow double click to edit"
  if (ACellViewInfo.Item.Options.Editing) then
  begin
    Sender.Controller.EditingController.ShowEdit;
    AHandled := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.GridTMDBTableViewColumnDuplicateGetDataText(Sender: TcxCustomGridTableItem; ARecordIndex: Integer; var AText: string);
var
  Value: string;
begin
  // Duplicate column is unbound. We supply data for it here.

  // Note that this method is called even if the column is hidden, so we cannot
  // assume that the conditions for its visibility have been satisfied.
  if (not Sender.Visible) then
    exit;

  if (FDuplicateSourceColumn = nil) then
    exit;

  Value := VarToStr(GridTMDBTableView.DataController.Values[ARecordIndex, FDuplicateSourceColumn.Index]);
  AText := AnsiLowerCase(SanitizeText(Value, FSanitizeRules));
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.GridTMDBTableViewColumnPosChanged(Sender: TcxGridTableView; AColumn: TcxGridColumn);
begin
  // Column has moved - need to update RTL array
  NeedUpdate(tmUpdateRightToLeft);
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.GridTMDBTableViewCustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
var
  Flags: DWORD;
begin

  if (FRightToLeft[AViewInfo.Item.Index] <> IsRightToLeft) then
  begin
    // Target language is Right-to-Left but rest of UI isn't - or vice versa
    Flags := TcxCustomTextEditViewInfo(AViewInfo.EditViewInfo).DrawTextFlags;
    if (FRightToLeft[AViewInfo.Item.Index]) then
    begin
      // Set RTL
      Flags := Flags or CXTO_RTLREADING;
      // Swap left/right alignment
      if (TranslationManagerSettings.Editor.EditBiDiMode) then
      begin
        if ((Flags and $0000000F) = CXTO_LEFT) then
          Flags := (Flags and $FFFFFFF0) or CXTO_RIGHT
        else
        if ((Flags and $0000000F) =  CXTO_RIGHT) then
          Flags := (Flags and $FFFFFFF0) or CXTO_LEFT;
      end;
    end else
      // Clear RTL
      Flags := Flags and (not CXTO_RTLREADING);

    TcxCustomTextEditViewInfo(AViewInfo.EditViewInfo).DrawTextFlags := Flags;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.GridTMDBTableViewDataControllerFilterRecord(ADataController: TcxCustomDataController;
  ARecordIndex: Integer; var Accept: Boolean);
begin
  if (FViewDuplicates) then
    // Only display row if record is in duplicate list
    Accept := (FDuplicates[ARecordIndex] <> -1);
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.GridTMDBTableViewInitEdit(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem;
  AEdit: TcxCustomEdit);
begin
  // Editor is reused between columns so we need to reset this for every column
  // in case one of the columns has changed it
  if (TranslationManagerSettings.Editor.EditBiDiMode) then
  begin
    if (FRightToLeft[AItem.Index]) then
      AEdit.BiDiMode := bdRightToLeft
    else
      AEdit.BiDiMode := bdLeftToRight;
  end;

  // Store language in tag so we can use it when opening the text editor.
  // See: TDataModuleMain.EditRepositoryTextItemPropertiesButtonClick
  AEdit.Tag := NativeInt(FTranslationMemory.FieldToLanguage(TcxGridDBColumn(AItem).DataBinding.Field));
end;

procedure TFormTranslationMemory.GridTMDBTableViewStylesGetContentStyle(Sender: TcxCustomGridTableView;
  ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
begin
  if (ARecord.Focused) and (AItem <> nil) and (AItem.Editing) then
    AStyle := DataModuleMain.StyleDefault
  else
  if (ARecord.Focused) and (AItem <> nil) and (AItem.Focused) then
    AStyle := DataModuleMain.StyleFocused
  else
  if (FViewDuplicates) and (FDuplicateColumn <> nil) then
  begin
(*
    if (AItem = FDuplicateColumn) then
      AStyle := StyleDuplicate
    else
*)
    if (FDuplicateColumn.SortIndex = 0) then
    begin
      if (Odd(FDuplicates[ARecord.RecordIndex])) then
        AStyle := DataModuleMain.StyleOdd
      else
        AStyle := DataModuleMain.StyleEven;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormTranslationMemory.LayoutGroupDuplicatesButton0Click(Sender: TObject);
begin
  ActionViewDuplicates.Execute;
end;

// -----------------------------------------------------------------------------

function TFormTranslationMemory.Locate(Language: TLanguageItem; const Value: string): boolean;
begin
  Result := False; // Maybe later...
end;

type
  TLocateMatch = (lmNone, lmEqualized, lmSanitized, lmSameSourceAndTarget, lmExactSourceOrTarget, lmExactSourceAndTarget);

function TFormTranslationMemory.LocatePair(LanguageA: TLanguageItem; const ValueA: string;
  LanguageB: TLanguageItem; const ValueB: string): boolean;

  function FindField(LanguageItem: TLanguageItem): TField;
  begin
    // First look for exact match
    for var i := 0 to GridTMDBTableView.ColumnCount-1 do
      if (GridTMDBTableView.Columns[i].DataBinding.Field <> nil) and (FTranslationMemory.FieldToLanguage(GridTMDBTableView.Columns[i].DataBinding.Field) = LanguageItem) then
        Exit(GridTMDBTableView.Columns[i].DataBinding.Field);

    // Then look for LCID match
    // TODO : We don't really use LCIDs anymore; Maybe retire this piece of code?
    for var i := 0 to GridTMDBTableView.ColumnCount-1 do
       if (GridTMDBTableView.Columns[i].DataBinding.Field <> nil) and (FTranslationMemory.FieldToLanguage(GridTMDBTableView.Columns[i].DataBinding.Field).LocaleID = LanguageItem.LocaleID) then
        Exit(GridTMDBTableView.Columns[i].DataBinding.Field);

    Result := nil;
  end;

var
  FieldA, FieldB: TField;
  SanitizedValueA, SanitizedValueB: string;
  Lookup: ITranslationMemoryLookup;
  List: TTranslationMemoryRecordList;
  RecNo: integer;
  DataSet: TDataSet;
  Value, FieldAValue, FieldBValue: string;
  PrevRecNo, MatchRecNo: integer;
  Matchness, LastMatchNess: TLocateMatch;
begin
  Result := False;

  FieldA := FindField(LanguageA);
  FieldB := FindField(LanguageB);
  if (FieldA = nil) or (FieldB = nil) then
    Exit;

  SanitizedValueA := SanitizeText(ValueA);
  SanitizedValueB := SanitizeText(ValueB);

  Lookup := FTranslationMemory.CreateLookup(LanguageA, TranslationManagerSettings.Editor.SanitizeRules);
  List := Lookup.Lookup(SanitizedValueA);

  if (List = nil) or (List.Count = 0) then
    Exit;

  DataSet := GridTMDBTableView.DataController.DataSet;
  DataSet.DisableControls;
  try
    Matchness := lmNone;
    MatchRecNo := -1;
    LastMatchNess := lmNone;
    PrevRecNo := -1;
    for RecNo in List do
    begin
      if (Matchness > LastMatchNess) then
        MatchRecNo := PrevRecNo;
      PrevRecNo := RecNo;
      LastMatchNess := Matchness;

      DataSet.RecNo := RecNo;
      FieldAValue := FieldA.AsString;
      FieldBValue := FieldB.AsString;

      // TODO : The algorithm below doesn't give priority to exact match on the A value

      if (FieldBValue = ValueB) then
      // Exact match on B
      begin
        if (FieldAValue = ValueA) then
        begin
          // Exact match on both values
          Matchness := lmExactSourceAndTarget;
          break;
        end;
        if (Matchness >= lmExactSourceOrTarget) then
          continue;
        if (AnsiSameText(FieldAValue, ValueA)) then
          // Exact match on B, same on A
          Matchness := lmExactSourceOrTarget
        else
        if (Matchness >= lmSameSourceAndTarget) then
          continue
        else
        if (AnsiSameText(FieldAValue, ValueA)) then
          // Exact match on B, same on A
          Matchness := lmSameSourceAndTarget
        else
          // Sanitized match on A and B
          Matchness := lmSanitized;
      end else
      if (Matchness >= lmExactSourceOrTarget) then
        continue
      else
      if (FieldAValue = ValueA) then
      // Exact match on A
      begin
        if (AnsiSameText(FieldBValue, ValueB)) then
          // Exact match on A, same on B
          Matchness := lmExactSourceOrTarget;
      end else
      if (Matchness >= lmSameSourceAndTarget) then
        continue
      else
      if (AnsiSameText(FieldAValue, ValueA)) and (AnsiSameText(FieldBValue, ValueB)) then
        // Same match on A and B
        Matchness := lmSameSourceAndTarget;

      // Don't look for sanitized match if we already have one
      if (Matchness >= lmSanitized) then
        continue;

      // Test for sanitized match
      Value := SanitizeText(FieldBValue);
      if (AnsiSameText(Value, SanitizedValueB)) then
      begin
        // Sanitized match on A and B
        Matchness := lmSanitized;
        continue;
      end;

      // Test for equalized match
      Value := MakeAlike(ValueA, FieldBValue);
      if (AnsiSameText(Value, ValueB)) then
        // Sanitized match on A, equalized on B
        Matchness := lmEqualized;
    end;

    if (Matchness > LastMatchNess) then
      MatchRecNo := PrevRecNo;

    if (Matchness > lmNone) then
    begin
      DataSet.RecNo := MatchRecNo;
      Result := True;
    end;
  finally
    DataSet.EnableControls;
  end;

  if (Result) then
    GridTMDBTableView.DataController.ChangeRowSelection(GridTMDBTableView.DataController.FocusedRowIndex, True);
end;

// -----------------------------------------------------------------------------

end.
