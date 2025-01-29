unit amLocalization.Dialog.ApplySameTranslation;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  Generics.Collections,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ToolWin, ActnMan, ActnCtrls, ActnList,
  StdActns, ImgList,
  Menus, ActnPopup, System.Actions, System.ImageList,

  cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxMemo, cxRichEdit, cxButtons, cxSplitter, cxImageList,
  dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxLabel, dxLayoutContainer, cxClasses, dxLayoutControl,
  cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxNavigator, dxDateRanges,
  dxScrollbarAnnotations, cxGridCustomTableView, cxGridTableView, cxGridCustomView, cxGridLevel, cxGrid,
  cxMaskEdit, cxDropDownEdit,

  amLanguageInfo,
  amLocalization.Dialog,
  amLocalization.Model,
  amLocalization.Settings;

//------------------------------------------------------------------------------
//
//      TFormApplySameTranslation
//
//------------------------------------------------------------------------------
type
  TPropertyTranslatedDelegate = reference to procedure(AProp: TLocalizerProperty);

type
  TFormApplySameTranslation = class(TFormDialog)
    GridLevel: TcxGridLevel;
    Grid: TcxGrid;
    LayoutItemGrid: TdxLayoutItem;
    GridView: TcxGridTableView;
    GridViewColumnModule: TcxGridColumn;
    GridViewColumnElement: TcxGridColumn;
    GridViewColumnProperty: TcxGridColumn;
    GridViewColumnValue: TcxGridColumn;
    GridViewColumnOld: TcxGridColumn;
    GridViewColumnNew: TcxGridColumn;
    dxLayoutLabeledItem1: TdxLayoutLabeledItem;
    ComboBoxNoPromptAction: TcxComboBox;
    LayoutItemRemaining: TdxLayoutItem;
    LayoutItemButtonSkip: TdxLayoutItem;
    ButtonSkip: TcxButton;
    ActionSkip: TAction;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    procedure GridViewStylesGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
      AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
    procedure GridViewColumnNewPropertiesEditValueChanged(Sender: TObject);
    procedure GridViewCustomDrawIndicatorCell(Sender: TcxGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxCustomGridIndicatorItemViewInfo; var ADone: Boolean);
    procedure GridViewStylesGetInactiveStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; AItem: TObject;
      var AStyle: TcxStyle);
    procedure GridViewStylesGetSelectionStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; AItem: TObject;
      var AStyle: TcxStyle);
    procedure GridViewCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo;
      AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
    procedure GridViewKeyPress(Sender: TObject; var Key: Char);
    procedure GridViewEditing(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem; var AAllow: Boolean);
    procedure GridViewCanSelectRecord(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; var AAllow: Boolean);
    procedure GridViewSelectionChanged(Sender: TcxCustomGridTableView);
    procedure ComboBoxNoPromptActionPropertiesEditValueChanged(Sender: TObject);
    procedure ActionEmptyExecute(Sender: TObject);
  private
    FTranslationLanguage: TTranslationLanguage;
    FProjectIndex: ILocalizerProjectPropertyLookup;
    FPromptOverride: TTranslationAutoApply;

  private
    property TranslationLanguage: TTranslationLanguage read FTranslationLanguage;
    property ProjectIndex: ILocalizerProjectPropertyLookup read FProjectIndex;

    function DoExecute(AProp: TLocalizerProperty; var UpdatedSame, UpdatedSimilar: integer;
      PropertyTranslatedDelegate: TPropertyTranslatedDelegate): boolean;
  public
    constructor Create(ATranslationLanguage: TTranslationLanguage; AProjectIndex: ILocalizerProjectPropertyLookup); reintroduce;

    function Execute(AProp: TLocalizerProperty; PropIndex, PropCount: integer; var PromptOverride: TTranslationAutoApply;
      var UpdatedSame, UpdatedSimilar: integer; PropertyTranslatedDelegate: TPropertyTranslatedDelegate = nil): boolean;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  amCursorService,
  amLocalization.Common,
  amLocalization.Normalization,
  amLocalization.Data.Main;

//------------------------------------------------------------------------------
//
//      TFormApplySameTranslation
//
//------------------------------------------------------------------------------
constructor TFormApplySameTranslation.Create(ATranslationLanguage: TTranslationLanguage; AProjectIndex: ILocalizerProjectPropertyLookup);
begin
  inherited Create(nil);
  FTranslationLanguage := ATranslationLanguage;
  FProjectIndex := AProjectIndex;
end;

//------------------------------------------------------------------------------

function TFormApplySameTranslation.Execute(AProp: TLocalizerProperty;
  PropIndex, PropCount: integer; var PromptOverride: TTranslationAutoApply;
  var UpdatedSame, UpdatedSimilar: integer;
  PropertyTranslatedDelegate: TPropertyTranslatedDelegate): boolean;
begin
  FPromptOverride := PromptOverride;
  ComboBoxNoPromptAction.ItemIndex := Ord(FPromptOverride);

  LayoutItemHeader.CaptionOptions.Text := Format(LayoutItemHeader.CaptionOptions.Text, [PropIndex+1, PropCount]);
  LayoutItemRemaining.CaptionOptions.Text := Format(LayoutItemRemaining.CaptionOptions.Text, [PropCount - PropIndex - 1]);
  // Hide the remaining choice if we are on the last property
  LayoutItemRemaining.Visible := (PropIndex < PropCount-1);

  Result := DoExecute(AProp, UpdatedSame, UpdatedSimilar, PropertyTranslatedDelegate);

  case ModalResult of
    mrCancel:
      PromptOverride := aaNever;
  else
    PromptOverride := FPromptOverride;
  end;
end;

function TFormApplySameTranslation.DoExecute(AProp: TLocalizerProperty; var UpdatedSame, UpdatedSimilar: integer;
  PropertyTranslatedDelegate: TPropertyTranslatedDelegate): boolean;

  procedure AddRow(RowIndex: integer; Prop: TLocalizerProperty; const OldTranslation, NewTranslation: string);
  begin
    GridView.DataController.Values[RowIndex, GridViewColumnModule.Index] := Prop.Item.Module.Name;
    GridView.DataController.Values[RowIndex, GridViewColumnElement.Index] := Prop.Item.Name;
    GridView.DataController.Values[RowIndex, GridViewColumnProperty.Index] := Prop.Name;
    GridView.DataController.Values[RowIndex, GridViewColumnValue.Index] := Prop.Value;
    GridView.DataController.Values[RowIndex, GridViewColumnOld.Index] := OldTranslation;
    GridView.DataController.Values[RowIndex, GridViewColumnNew.Index] := NewTranslation;
  end;

var
  Prop: TLocalizerProperty;
begin
  Result := False;

  (*
  ** TODO :
  ** 1. Check if translation doesn't exist in TM and
  **    a) Other identical translations of same term exist.
  **    b) Term exist in TM but with other translation.
  ** 2. Suggest to user that translation be added to TM:
  **    "The translation you just made appears to be common.
  **     It is suggested that you add this translation to the Translation Memory
  **     so it can be reused for future translations."
  **
  *)

  var AutoApplyTranslations := TranslationManagerSettings.Editor.AutoApplyTranslations;
  var AutoApplyTranslationsSimilar := TranslationManagerSettings.Editor.AutoApplyTranslationsSimilar;

  // If caller has specified that we shouldn't prompt, and has specified an action to take
  // then override the global settings, locally, with that action.
  if (FPromptOverride <> aaPrompt) then
  begin
    if (AutoApplyTranslations <> aaNever) then
      AutoApplyTranslations := FPromptOverride;

    if (AutoApplyTranslationsSimilar <> aaNever) then
      AutoApplyTranslationsSimilar := FPromptOverride;
  end;

  if (AutoApplyTranslations = aaNever) then
    // Do not apply translation to whole project
    Exit;

  if (not AProp.HasTranslation(TranslationLanguage)) then
    // Property wasn't translated - nothing to do
    Exit;

  var SourceValue := AProp.Value;
  var TranslatedValue := AProp.TranslatedValue[TranslationLanguage];

  // Ignore empty source or target
  if (SourceValue.Trim.IsEmpty) or (TranslatedValue.Trim.IsEmpty) then
    Exit;

  SaveCursor(crAppStart);

  // Get a list of properties with "similar" source values
  var PropertyList := ProjectIndex.Lookup(AProp);

  if (PropertyList = nil) then
    Exit;

  var DoPrompt := (AutoApplyTranslations = aaPrompt) or (AutoApplyTranslationsSimilar = aaPrompt);

  var PromptProperties := TLocalizerPropertyList.Create;
  try
    PromptProperties.Capacity := PropertyList.Count;

    var FixedRows := 0;

    var Properties := TLocalizerPropertyList.Create;
    try
      Properties.Capacity := PropertyList.Count;

      // Collect elegible properties
      for Prop in PropertyList do
      begin
        if (Prop = AProp) then
          continue;

        if (Prop.EffectiveStatus <> ItemStatusTranslate) then
          continue;

        var IsSame := (SourceValue = Prop.Value);
        if (AutoApplyTranslationsSimilar = aaNever) and (not IsSame) then
          continue;

        // Consider property if:
        // - User has chosen to apply to existing translations or...
        // - There is not existing translation
        if (not TranslationManagerSettings.Editor.AutoApplyTranslationsExisting) and (Prop.HasTranslation(TranslationLanguage)) then
          continue;

        Properties.Add(Prop);
      end;

      // Populate grid
      var RowIndex := 0;
      if (DoPrompt) then
      begin
        GridView.DataController.RecordCount := Properties.Count+1;

        AddRow(RowIndex, AProp, '', TranslatedValue);
        GridView.DataController.RowFixedState[RowIndex] := rfsFixedToTop;
        GridView.DataController.ChangeRowSelection(RowIndex, True);
        Inc(FixedRows);
      end;

      for Prop in Properties do
      begin

        var OldTranslation := '';
        var HasTranslation := Prop.TryGetTranslatedValue(TranslationLanguage, OldTranslation);

        var IsSame := (SourceValue = Prop.Value);

        var NewTranslation := TranslatedValue;
        if (not IsSame) then
        begin
          // TODO : We need to use SanitizeText in order to handle the case wher the
          // source translation contains "stuff" but the target shouldn't.
          // E.g. with out SanitizeText "(Hello)" -> "(Hej)) now produces "Hello"->"(Hej)".
          // This is a deficiency of MakeAlike which, for some cases, only add "stuff" and
          // doesn't remove it.
          // Accelerator, Ending & Space is handled by MakeAlike.
          NewTranslation := SanitizeText(NewTranslation, [skSurround]);

          NewTranslation := MakeAlike(Prop.Value, NewTranslation);
        end;

        // Apply without prompt?
        if (IsSame) then
        begin
          if (AutoApplyTranslations = aaAlways) then
          begin
            Prop.TranslatedValue[TranslationLanguage] := NewTranslation;

            Inc(UpdatedSame);
            Result := True;

            if (Assigned(PropertyTranslatedDelegate)) then
              PropertyTranslatedDelegate(Prop);

            continue;
          end;
        end else
        if (AutoApplyTranslationsSimilar = aaAlways) then
        begin
          Prop.TranslatedValue[TranslationLanguage] := NewTranslation;

          Inc(UpdatedSimilar);
          Result := True;

          if (Assigned(PropertyTranslatedDelegate)) then
            PropertyTranslatedDelegate(Prop);

          continue;
        end;

        if (DoPrompt) then
        begin
          var Apply := (not HasTranslation) or (OldTranslation <> NewTranslation);

          GridView.DataController.ChangeRowSelection(FixedRows+RowIndex, Apply);

          AddRow(FixedRows+RowIndex, Prop, OldTranslation, NewTranslation);
          PromptProperties.Add(Prop);

          Inc(RowIndex);
        end;
      end;
    finally
      Properties.Free;
    end;

    if (PromptProperties.IsEmpty) then
      exit;

    GridView.DataController.RecordCount := FixedRows+PromptProperties.Count; // Purge unused rows

    (*
    ** Prompt user; Display dialog
    *)
    if (ShowModal <> mrOK) then
      exit;

    for var i := 0 to PromptProperties.Count-1 do
    begin
      if (not GridView.DataController.IsRowSelected(FixedRows+i)) then
        continue;

      Prop := PromptProperties[i];

      // Apply translation
      var NewTranslation := GridView.DataController.Values[FixedRows+i, GridViewColumnNew.Index];

      var Translation: TLocalizerTranslation;
      if (not Prop.Translations.TryGetTranslation(TranslationLanguage, Translation)) or (Translation.Value <> NewTranslation) then
      begin
        Prop.TranslatedValue[TranslationLanguage] := NewTranslation;

        if (Prop.Value = SourceValue) then
          Inc(UpdatedSame)
        else
          Inc(UpdatedSimilar);

        Result := True;

        if (Assigned(PropertyTranslatedDelegate)) then
          PropertyTranslatedDelegate(Prop);
      end;
    end;
  finally
    PromptProperties.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TFormApplySameTranslation.ActionEmptyExecute(Sender: TObject);
begin
  //
end;

//------------------------------------------------------------------------------

procedure TFormApplySameTranslation.ComboBoxNoPromptActionPropertiesEditValueChanged(Sender: TObject);
begin
  FPromptOverride := TTranslationAutoApply(ComboBoxNoPromptAction.ItemIndex);
end;

//------------------------------------------------------------------------------

procedure TFormApplySameTranslation.GridViewCanSelectRecord(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  var AAllow: Boolean);
begin
(* Unfortunately this makes the "check all"/"uncheck all" function not work
  if (GridView.DataController.RowFixedState[ARecord.Index] <> rfsNotFixed) then
    AAllow := False;
*)
end;

procedure TFormApplySameTranslation.GridViewColumnNewPropertiesEditValueChanged(Sender: TObject);
begin
  if (GridView.Controller.FocusedRecord.Selected) then
    exit; // Already selected

  // Select row if "New" value <> "Old" value
  // Note that the GridCell values hasn't changed yet, so we need to test against the editor text value
  if (GridView.Controller.EditingController.Edit.EditValue <> GridView.Controller.FocusedRecord.Values[GridViewColumnOld.Index]) then
    GridView.Controller.FocusedRecord.Selected := True;
end;

procedure TFormApplySameTranslation.GridViewCellDblClick(Sender: TcxCustomGridTableView;
  ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
begin
  // Double-click toggles selection
  if (ACellViewInfo.GridRecord <> nil) then
    ACellViewInfo.GridRecord.Selected := not ACellViewInfo.GridRecord.Selected;
end;

procedure TFormApplySameTranslation.GridViewKeyPress(Sender: TObject; var Key: Char);
begin
  // Space-key toggles selection
  if (Key = #32) and (GridView.Controller.FocusedRecord <> nil) then
  begin
    if (GridView.Controller.FocusedItem <> nil) and (GridView.Controller.FocusedItem.Options.Editing) then
      exit;

    GridView.Controller.FocusedRecord.Selected := not GridView.Controller.FocusedRecord.Selected;
    Key := #0;
  end;
end;

procedure TFormApplySameTranslation.GridViewSelectionChanged(Sender: TcxCustomGridTableView);
begin
  // When selection changes, update the fixed row selection so it indicates if
  // any or none are selected.
  // In effect, this makes manual selection of the fixed row impossible, which is what we want.
  var HasChecked := False;
  for var i := GridView.DataController.FixedTopRowCount to GridView.DataController.RecordCount-1 do
    if (GridView.DataController.IsRowSelected(i)) then
    begin
      HasChecked := True;
      break;
    end;

  GridView.DataController.ChangeRowSelection(0, HasChecked);
end;

//------------------------------------------------------------------------------

procedure TFormApplySameTranslation.GridViewCustomDrawIndicatorCell(Sender: TcxGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxCustomGridIndicatorItemViewInfo; var ADone: Boolean);
begin
  // Checkbox is painted *unconditionally* after event handler returns; No way to hide it

  if AViewInfo is TcxGridIndicatorRowItemViewInfo then
  begin
    var IndicatorKind := TcxGridIndicatorRowItemViewInfo(AViewInfo).IndicatorKind;
    case IndicatorKind of
      // Focused, not selected
      ikArrow:
        IndicatorKind := ikNone; // Display unchecked checkbox

      // Focused, selected
      ikMultiArrow:
        IndicatorKind := ikMultiDot; // Display checked checkbox

      // Not focused, selected
      ikMultiDot,
      // Not focused, not selected
      ikNone:
        ;
    else
      exit;
    end;

    AViewInfo.LookAndFeelPainter.DrawScaledIndicatorItem(ACanvas, AViewInfo.Bounds, IndicatorKind, AViewInfo.Params.Color, AViewInfo.ScaleFactor, False);
    ADone := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TFormApplySameTranslation.GridViewEditing(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem;
  var AAllow: Boolean);
begin
  if (GridView.DataController.RowFixedState[GridView.DataController.FocusedRecordIndex] <> rfsNotFixed) then
    AAllow := False;
end;

//------------------------------------------------------------------------------

procedure TFormApplySameTranslation.GridViewStylesGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
begin
  if (ARecord <> nil) and (GridView.DataController.RowFixedState[ARecord.Index] <> rfsNotFixed) then
    // Fixed row is styled using the Even style of the Odd/Even colors
    AStyle := DataModuleMain.StyleEven
  else
  if (ARecord <> nil) and (ARecord.Focused) then
  begin
    // Focused row is styled as selected

    if (AItem <> nil) and (AItem.Editing) then
      AStyle := DataModuleMain.StyleSkinDefault
    else
    if (Sender.IsControlFocused) then
      AStyle := DataModuleMain.StyleSkinSelected
    else
      AStyle := DataModuleMain.StyleSkinSelectedInactive;
  end else
    AStyle := DataModuleMain.StyleSkinDefault;
end;

procedure TFormApplySameTranslation.GridViewStylesGetInactiveStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  AItem: TObject; var AStyle: TcxStyle);
begin
  if (ARecord <> nil) and (GridView.DataController.RowFixedState[ARecord.Index] <> rfsNotFixed) then
    AStyle := DataModuleMain.StyleEven
  else
  if (ARecord <> nil) and (ARecord.Focused) then
    AStyle := DataModuleMain.StyleSkinSelectedInactive
  else
    AStyle := DataModuleMain.StyleSkinDefault;
end;

procedure TFormApplySameTranslation.GridViewStylesGetSelectionStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  AItem: TObject; var AStyle: TcxStyle);
begin
  if (ARecord <> nil) and (GridView.DataController.RowFixedState[ARecord.Index] <> rfsNotFixed) then
    AStyle := DataModuleMain.StyleEven
  else
  if (ARecord <> nil) and (ARecord.Focused) then
    AStyle := DataModuleMain.StyleSkinSelected
  else
    AStyle := DataModuleMain.StyleSkinDefault;
end;

//------------------------------------------------------------------------------

end.
