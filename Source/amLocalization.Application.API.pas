unit amLocalization.Application.API;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

type
  TTranslationManagerApplicationNotification = (
    tmaStartup,
    tmaShutdown,
    // TODO : tmaProject* will become project level notifications
    tmaProjectLoading,
    tmaProjectLoaded,
    // tmaProjectModified,
    tmaProjectSaving,
    tmaProjectSaved,
    tmaSettingsChanged
  );

  ITranslationManagerApplicationSubscriber = interface
    ['{DA56E097-2BB2-4386-A980-10CF16315295}']
    procedure TranslationManagerApplicationNotification(Notification: TTranslationManagerApplicationNotification);
  end;

  ITranslationManagerApplication = interface
    ['{21ACB579-0BFF-442E-8C9C-2892230AE3F5}']
    procedure Subscribe(const Subscriber: ITranslationManagerApplicationSubscriber);
    procedure Unsubscribe(const Subscriber: ITranslationManagerApplicationSubscriber);
  end;

function TranslationManagerApplication: ITranslationManagerApplication;

function RegisterTranslationManagerApplication(const ATranslationManagerApplication: ITranslationManagerApplication): ITranslationManagerApplication;


implementation

var
  FTranslationManagerApplication: ITranslationManagerApplication;

function RegisterTranslationManagerApplication(const ATranslationManagerApplication: ITranslationManagerApplication): ITranslationManagerApplication;
begin
  Result := FTranslationManagerApplication;
  FTranslationManagerApplication := ATranslationManagerApplication;
end;

function TranslationManagerApplication: ITranslationManagerApplication;
begin
  Assert(FTranslationManagerApplication <> nil);
  Result := FTranslationManagerApplication;
end;

initialization
finalization
  FTranslationManagerApplication := nil;
end.
