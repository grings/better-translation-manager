unit amDialog.Manager.API;

(*
 * Copyright © 2019-2025 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  Forms,
  Classes;

// -----------------------------------------------------------------------------
//
//      IDialogManager
//
// -----------------------------------------------------------------------------
type
  IDialogManager = interface
    ['{1A72D5D0-6A90-4B09-86EB-0A0AA4B84B4F}']

    procedure RegisterDialogClass(const DialogID: TGUID; DialogClass: TComponentClass);

    // Lifetime of dialog is assumed to be controlled by returned interface unless AFormIsModal=False
    // in which case the lifetime is controlled by the form.
    function CreateDialog(const DialogID: TGUID; AOwner: TComponent = nil; RegisterDialog: boolean = False; AFormIsModal: boolean = True): IUnknown;

    // Form instance management
    procedure RegisterDialog(const DialogID: TGUID; Dialog: IUnknown);
    procedure UnregisterDialog(const DialogID: TGUID);

    function FindDialog(const DialogID: TGUID): IUnknown;
  end;


// -----------------------------------------------------------------------------
//
//      DialogManager
//
// -----------------------------------------------------------------------------
procedure RegisterDialogManager(const ADialogManager: IDialogManager);

function DialogManager: IDialogManager;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

var
  FDialogManager: IDialogManager = nil;

procedure RegisterDialogManager(const ADialogManager: IDialogManager);
begin
  FDialogManager := ADialogManager;
end;

function DialogManager: IDialogManager;
begin
  Result := FDialogManager;
end;

// -----------------------------------------------------------------------------

end.
