unit amLocalization.Dialog;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore,
  dxLayoutControlAdapters, dxLayoutContainer, cxClasses, cxButtons, dxLayoutControl,
  cxContainer, cxEdit, cxGroupBox, dxLayoutcxEditAdapters, cxLabel,
  dxForms;

type
  TFormDialog = class abstract(TdxForm)
    ActionList: TActionList;
    ActionOK: TAction;
    ActionCancel: TAction;
    LayoutControlButtons: TdxLayoutControl;
    ButtonOK: TcxButton;
    ButtonCancel: TcxButton;
    LayoutControlButtonsGroup_Root: TdxLayoutGroup;
    LayoutItemButtonOK: TdxLayoutItem;
    LayoutItemButtonCancel: TdxLayoutItem;
    LayoutGroupButtons: TdxLayoutGroup;
    LayoutControl: TdxLayoutControl;
    LayoutControlGroup_Root: TdxLayoutGroup;
    LayoutControlHeader: TdxLayoutControl;
    LayoutControlHeaderGroup_Root: TdxLayoutGroup;
    LayoutItemHeader: TdxLayoutLabeledItem;
    procedure ActionOKExecute(Sender: TObject);
    procedure ActionCancelExecute(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetHeader(const Value: string);
  end;

implementation

{$R *.dfm}

uses
  amLocalization.Data.Main;

procedure TFormDialog.ActionCancelExecute(Sender: TObject);
begin
//
end;

procedure TFormDialog.ActionOKExecute(Sender: TObject);
begin
//
end;

constructor TFormDialog.Create(AOwner: TComponent);
begin
  inherited;

  if (LayoutControlHeader.Visible) then
    SetHeader(LayoutItemHeader.CaptionOptions.Text);
end;

procedure TFormDialog.SetHeader(const Value: string);
begin
  LayoutItemHeader.CaptionOptions.Text := Value;
  LayoutControlHeader.Visible := (Value <> '');
end;

end.
