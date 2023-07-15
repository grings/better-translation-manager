unit amLocalization.Dialog.Languages;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  System.Generics.Collections,
  System.Generics.Defaults,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, cxClasses,
  dxLayoutContainer, dxLayoutControl, cxContainer, cxEdit, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, cxCheckBox, cxCustomListBox, cxCheckListBox, cxGroupBox, dxCheckGroupBox, cxButtons,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxLookupEdit, cxDBLookupEdit, cxDBExtLookupComboBox, cxLabel,

  VirtualTrees,
  VirtualTrees.Types,

  amLanguageInfo,
  amLocalization.Dialog;

type
  TFormLanguages = class(TFormDialog)
    ComboBoxSourceLanguage: TcxExtLookupComboBox;
    LayoutItemSourceLanguage: TdxLayoutItem;
    dxLayoutGroup1: TdxLayoutGroup;
    LayoutItemTargetLanguage: TdxLayoutItem;
    CheckBoxApplyFilter: TcxCheckBox;
    dxLayoutItem5: TdxLayoutItem;
    ActionList1: TActionList;
    ActionApplyFilter: TAction;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    procedure ActionApplyFilterExecute(Sender: TObject);
    procedure ActionApplyFilterUpdate(Sender: TObject);
  private
    FTreeView: TVirtualStringTree;
    FNodes: TList<PVirtualNode>;
    procedure TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeViewIncrementalSearch(Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: string; var Result: Integer);
  protected
    function GetApplyFilter: boolean;
    procedure SetApplyFilter(const Value: boolean);
    function GetSourceLanguage: TLanguageItem;
    procedure SetSourceLanguage(const Value: TLanguageItem);
    function GetTargetLanguageCount: integer;
    function GetTargetLanguage(Index: integer): TLanguageItem;

    procedure LoadLanguages;
    function NodeToLanguageItem(Node: PVirtualNode): TLanguageItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: boolean;

    function SelectTargetLanguage(Language: TLanguageItem): boolean;
    procedure ClearTargetLanguages;

    property ApplyFilter: boolean read GetApplyFilter write SetApplyFilter;
    property SourceLanguage: TLanguageItem read GetSourceLanguage write SetSourceLanguage;
    property TargetLanguageCount: integer read GetTargetLanguageCount;
    property TargetLanguage[Index: integer]: TLanguageItem read GetTargetLanguage;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  Math,
  amLocalization.Data.Main;


//------------------------------------------------------------------------------

function ExtractLanguage(const Value: string): string;
var
  n: integer;
begin
  Result := Value;
  n := Pos('(', Result);
  if (n <> 0) then
  begin
    if (n > 1) and (Result[n-1] = ' ') then
      Dec(n);
    SetLength(Result, n-1);
  end;
end;

//------------------------------------------------------------------------------

constructor TFormLanguages.Create(AOwner: TComponent);
begin
  inherited;

  FNodes := TList<PVirtualNode>.Create;

  FTreeView := TVirtualStringTree.Create(Self);
  FTreeView.OnGetText := TreeViewGetText;
  FTreeView.OnIncrementalSearch := TreeViewIncrementalSearch;
  FTreeView.TreeOptions.MiscOptions := FTreeView.TreeOptions.MiscOptions + [toCheckSupport];
  FTreeView.TreeOptions.SelectionOptions := FTreeView.TreeOptions.SelectionOptions + [toFullRowSelect];
  FTreeView.IncrementalSearch := isAll;
  FTreeView.CheckImageKind := ckSystemDefault;

  LayoutItemTargetLanguage.Control := FTreeView;

  LoadLanguages;
end;

destructor TFormLanguages.Destroy;
begin
  FNodes.Free;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TFormLanguages.TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
begin
  if (Node = nil) or (Node = FTreeView.RootNode) then
    Exit;

  var LanguageItem := NodeToLanguageItem(Node);

  if (Node.Parent = FTreeView.RootNode) and (Node.ChildCount > 0) then
    // Top level parent node: Region invariant
    CellText := Format('%s - %s', [ExtractLanguage(LanguageItem.DisplayName), LanguageItem.LocaleName.ToUpper])
  else
    // Child nodes or top level nodes with out children: Language, Region
    CellText := Format('%s - %s', [LanguageItem.LanguageName, LanguageItem.LocaleName]);

(*
  if (LanguageItem.Invariant) then
    CellText := CellText + ' *';
*)
end;

procedure TFormLanguages.TreeViewIncrementalSearch(Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: string; var Result: Integer);
begin
  var Text: string;
  TreeViewGetText(Sender, Node, 0, ttNormal, Text);
  if (Text.ToLower.Contains(SearchText.ToLower)) then
    Result := 0
  else
    Result := 1;
end;

//------------------------------------------------------------------------------

function TFormLanguages.NodeToLanguageItem(Node: PVirtualNode): TLanguageItem;
begin
  Result := TLanguageItem(FTreeView.GetNodeData(Node)^);
end;

//------------------------------------------------------------------------------

procedure TFormLanguages.LoadLanguages;
var
  LanguageNode, LocaleNode: PVirtualNode;
  SortedLanguages: TList<TLanguageItem>;
  Languages: TDictionary<string, PVirtualNode>;
  LanguageItem: TLanguageItem;
  LanguageName: string;
begin
  SortedLanguages := TList<TLanguageItem>.Create;
  try
    for var Item in LanguageInfo do
      SortedLanguages.Add(Item);

    SortedLanguages.Sort(TComparer<TLanguageItem>.Construct(
      function(const Left, Right: TLanguageItem): Integer
      begin
        Result := CompareText(ExtractLanguage(Left.DisplayName), ExtractLanguage(Right.DisplayName));
        if (Result = 0) then
          Result := CompareText(Left.LanguageName, Right.LanguageName);
      end));

    Languages := TDictionary<string, PVirtualNode>.Create;
    try

      // Start by adding invariant locale nodes
      for LanguageItem in SortedLanguages do
      begin
        if (not LanguageItem.Invariant) then
          continue;

        LanguageName := LanguageItem.ISO639_1Name;

        if (not Languages.TryGetValue(LanguageName, LanguageNode)) then
        begin
          LanguageNode := FTreeView.AddChild(nil, LanguageItem);
          FNodes.Add(LanguageNode);
          LanguageNode.CheckType := ctCheckBox;
          LanguageNode.States := LanguageNode.States - [vsExpanded];
          Languages.Add(LanguageName, LanguageNode);
        end else
        begin
          // Invariant locales with the same language code as an existing locale
          // are added as child nodes under that.
          LanguageNode := FTreeView.AddChild(LanguageNode, LanguageItem);
          FNodes.Add(LanguageNode);
          LanguageNode.CheckType := ctCheckBox;
        end;
      end;


      // Then all the invariant locale nodes. These will become child nodes
      // under the invariant locale nodes.
      for LanguageItem in SortedLanguages do
      begin
        if (LanguageItem.Invariant) then
          continue;

        LanguageName := LanguageItem.ISO639_1Name;

        if (Languages.TryGetValue(LanguageName, LanguageNode)) then
        begin
          LocaleNode := FTreeView.AddChild(LanguageNode, LanguageItem);
          FNodes.Add(LocaleNode);
          LocaleNode.CheckType := ctCheckBox;
        end else
        begin
          // This shouldn't happen but don't bomb if it does.
          LanguageNode := FTreeView.AddChild(nil, LanguageItem);
          FNodes.Add(LanguageNode);
          LanguageNode.CheckType := ctCheckBox;
          Languages.Add(LanguageName, LanguageNode);
        end;
      end;

    finally
      Languages.Free;
    end;

  finally
    SortedLanguages.Free;
  end;
end;

//------------------------------------------------------------------------------

function TFormLanguages.Execute: boolean;
begin
  Result := (ShowModal = mrOK)
end;

//------------------------------------------------------------------------------

procedure TFormLanguages.ActionApplyFilterExecute(Sender: TObject);
begin
  CheckBoxApplyFilter.Checked := TAction(Sender).Checked;
end;

procedure TFormLanguages.ActionApplyFilterUpdate(Sender: TObject);
var
  i: integer;
  SourceEqualsTarget: boolean;
  CheckedCount: integer;
begin
  SourceEqualsTarget := False;
  CheckedCount := 0;
  for i := 0 to FNodes.Count-1 do
    if (FNodes[i].CheckState = csCheckedNormal) then
    begin
      if (NodeToLanguageItem(FNodes[i]) = SourceLanguage) then
        SourceEqualsTarget := True;
      Inc(CheckedCount);
    end;

  TAction(Sender).Enabled := (CheckedCount > 1) or ((CheckedCount = 1) and (not SourceEqualsTarget));
end;

//------------------------------------------------------------------------------

function TFormLanguages.GetApplyFilter: boolean;
begin
  Result := ActionApplyFilter.Checked;
end;

procedure TFormLanguages.SetApplyFilter(const Value: boolean);
begin
  ActionApplyFilter.Checked := Value;
end;

//------------------------------------------------------------------------------

procedure TFormLanguages.ClearTargetLanguages;
var
  i: integer;
begin
  for i := 0 to FNodes.Count-1 do
    FNodes[i].CheckState := csUncheckedNormal;
  FTreeView.Invalidate;
end;

function TFormLanguages.SelectTargetLanguage(Language: TLanguageItem): boolean;
var
  i: integer;
begin
  Result := False;

  for i := 0 to FNodes.Count-1 do
    if (NodeToLanguageItem(FNodes[i]) = Language) then
    begin
      FNodes[i].CheckState := csCheckedNormal;
      FTreeView.InvalidateNode(FNodes[i]);
      FTreeView.Expanded[FNodes[i].Parent] := True;
      Result := True;
      break;
    end;
end;

//------------------------------------------------------------------------------

function TFormLanguages.GetSourceLanguage: TLanguageItem;
begin
  Result := LanguageInfo.FindLocaleName(ComboBoxSourceLanguage.EditValue);
end;

procedure TFormLanguages.SetSourceLanguage(const Value: TLanguageItem);
begin
  ComboBoxSourceLanguage.EditValue := Value.LocaleName;
end;

//------------------------------------------------------------------------------

function TFormLanguages.GetTargetLanguage(Index: integer): TLanguageItem;
var
  i: integer;
begin
  Result := nil;

  i := 0;
  while (i < FNodes.Count) and (Index >= 0) do
  begin
    if (FNodes[i].CheckState = csCheckedNormal) then
    begin
      if (Index = 0) then
        Exit(NodeToLanguageItem(FNodes[i]));
      Dec(Index);
    end;

    Inc(i);
  end;
end;

function TFormLanguages.GetTargetLanguageCount: integer;
var
  i: integer;
begin
  Result := 0;

  for i := 0 to FNodes.Count-1 do
    if (FNodes[i].CheckState = csCheckedNormal) then
      Inc(Result);
end;

end.
