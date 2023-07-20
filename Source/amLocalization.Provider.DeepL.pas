unit amLocalization.Provider.DeepL;

(*
 * Copyright © 2023 atiburzi
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  System.SysUtils, System.Classes,

  amLanguageInfo,
  amLocalization.Model,
  amLocalization.Provider;

type
  ITranslationProviderDeepL = interface
    ['{0C0FEE40-4DB1-4462-A3EA-9CE4D5F1B49B}']
    function ValidateAPIKey(const APIKey: string; Pro: boolean; var ErrorMessage: string): boolean;
  end;

// -----------------------------------------------------------------------------
//
// TTranslationProviderDeepL
//
// -----------------------------------------------------------------------------

type
  TTranslationProviderDeepL = class(TInterfacedObject, ITranslationProvider, ITranslationProviderDeepL)
  public const
    DeepLAPIAddressFree = 'https://api-free.deepl.com/v2/translate';
    DeepLAPIAddressPro = 'https://api.deepl.com/v2/translate';
  private
    function GetDeepLAPIAddress: string;
    function GetDeepLAPIKey: string;
    function TranslateText(const ASourceLang, ATargetLang, AText: string): string;
  protected
    // ITranslationProvider
    function BeginLookup(SourceLanguage, TargetLanguage: TLanguageItem): boolean;
    function Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLanguageItem; Translations: TStrings): boolean;
    procedure EndLookup;
    function GetProviderName: string;
  protected
    // ITranslationProviderDeepL
    function ValidateAPIKey(const APIKey: string; Pro: boolean; var ErrorMessage: string): boolean;
  public
    property DeepLAPIAddress: string read GetDeepLAPIAddress;
    property DeepLAPIKey: string read GetDeepLAPIKey;
  end;

implementation

uses
  Dialogs,
  System.StrUtils,
  System.Net.HttpClient,
  System.JSON,
  System.Generics.Collections,
  amLocalization.Normalization,
  amLocalization.Settings;

resourcestring
  sProviderNameDeepL = 'DeepL Translation Service';

const
  DeepLValidLanguages: array of string = ['BG', 'CS', 'DA', 'DE', 'EL', 'EN', 'ES',
    'ET', 'FI', 'FR', 'HU', 'ID', 'IT', 'JA', 'KO', 'LT', 'LV', 'NB', 'NL',
    'PL', 'PT', 'RO', 'RU', 'SK', 'SL', 'SV', 'TR', 'UK', 'ZH'];

type
  EDeepLLocalizationProvider = class(ELocalizationProvider);

resourcestring
  sDeepLErrorInvalidAPIKey = 'Invalid API Key. Please verify the DeepL configuration settings';
  sDeepLErrorInvalidSourceLang = 'Unsupported source language for the traslation (%s)';
  sDeepLErrorInvalidTargetLang = 'Unsupported target language for the traslation (%s)';
  sDeepLErrorMoreThanOneTranslation = 'DeepL server returned none or more than one translation';
  sDeepLErrorInvalidTranslation = 'DeepL server returned an invalid translation';
  sDeepLErrorInvalidResponse = 'DeepL server returned an invalid response';
  sDeepLErrorEmptyResponse = 'DeepL server returned an empty response';
  sDeepLErrorTooManyRequests = 'Too many requests for the DeepL server. Please wait and send your request again.';
  sDeepLErrorGeneralException = 'DeepL server invalid Response - Code: %s - %s';
  sDeepLErrorUnefinedResponse = 'Undefined response from DeepL server.';
  sDeepLErrorForbidden = 'DeepL server rejected the request (forbidden access), please check the validity or the version of the API Key';


// -----------------------------------------------------------------------------
//
// TTranslationProviderDeepL
//
// -----------------------------------------------------------------------------
function TTranslationProviderDeepL.ValidateAPIKey(const APIKey: string; Pro: boolean; var ErrorMessage: string): boolean;
begin
  Result := (RightStr(APIKey, 3) = ':fx');
  if (not Result) then
    ErrorMessage := sDeepLErrorInvalidAPIKey;
end;

function TTranslationProviderDeepL.TranslateText(const ASourceLang, ATargetLang, AText: string): string;
var
  Msg: string;
  RequestClient: THTTPClient;
  RequestParams: TStringList;
  HTTPResponse: IHTTPResponse;
  JSONResponse, JSONTranslationItem: TJSONObject;
  JSONResultArray: TJSONArray;
  TextResponse: string;
begin

  Result := '';

  // Verify the validity of the input parameters
  if (not ValidateAPIKey(DeepLAPIKey, TranslationManagerSettings.Providers.DeepL.ProVersion, Msg)) then
    raise Exception.Create(Msg);

  if not MatchText(ASourceLang, DeepLValidLanguages) then
    raise Exception.CreateFmt(sDeepLErrorInvalidSourceLang, [ASourceLang]);

  if not MatchText(ATargetLang, DeepLValidLanguages) then
    raise Exception.CreateFmt(sDeepLErrorInvalidTargetLang, [ASourceLang]);

  RequestClient := THTTPClient.Create;

  try

    // Setup the parameters to for the HTTP request

    RequestParams := TStringList.Create;
    try
      RequestParams.DefaultEncoding := TEncoding.UTF8;

      RequestParams.Add('auth_key=' + DeepLAPIKey);
      RequestParams.Add('source_lang=' + ASourceLang);
      RequestParams.Add('target_lang=' + ATargetLang);
      RequestParams.Add('text=' + AText);
      RequestParams.Add('split_sentences=0');
      RequestParams.Add('preserve_formatting=1');
      RequestParams.Add('formality=default');

      // Call web service
      HTTPResponse := RequestClient.Post(DeepLAPIAddress, RequestParams);
    finally
      RequestParams.Free;
    end;

    // Checks the possible exceptions on the result
    if (HTTPResponse = nil) then
      raise Exception.Create(sDeepLErrorUnefinedResponse);

    case HTTPResponse.StatusCode of
      200: // Response ok
        begin
          TextResponse := HTTPResponse.ContentAsString(TEncoding.UTF8);

          if TextResponse.IsEmpty then
            raise Exception.Create(sDeepLErrorEmptyResponse);

          JSONResponse := TJSONObject.ParseJSONValue(TextResponse) as TJSONObject;

          if (JSONResponse = nil) then
            raise Exception.Create(sDeepLErrorInvalidResponse);
          try
            JSONResultArray := JSONResponse.GetValue('translations') as TJSONArray;

            if (JSONResultArray = nil) then
              raise Exception.Create(sDeepLErrorInvalidResponse);

            if (JSONResultArray.Count <> 1) then
              raise Exception.Create(sDeepLErrorMoreThanOneTranslation);

            JSONTranslationItem := JSONResultArray[0] as TJSONObject;

            if (JSONTranslationItem = nil) then
              raise Exception.Create(sDeepLErrorInvalidTranslation);

            Result := (JSONTranslationItem.GetValue('text') as TJSONString).Value;
          finally
            JSONResponse.Free;
          end;
        end;

      429: // Other responses
        raise Exception.Create(sDeepLErrorTooManyRequests);

      403:
        raise Exception.Create(sDeepLErrorForbidden);

    else
      raise Exception.CreateFmt(sDeepLErrorGeneralException, [HTTPResponse.StatusCode.ToString, HTTPResponse.StatusText]);
    end;

  finally
    RequestClient.Free;
  end;
end;

function TTranslationProviderDeepL.BeginLookup(SourceLanguage, TargetLanguage: TLanguageItem): boolean;
begin
  Result := True;
end;

procedure TTranslationProviderDeepL.EndLookup;
begin
end;

function TTranslationProviderDeepL.GetDeepLAPIAddress: string;
begin
  if TranslationManagerSettings.Providers.DeepL.ProVersion then
    Result := DeepLAPIAddressPro
  else
    Result := DeepLAPIAddressFree;
end;

function TTranslationProviderDeepL.GetDeepLAPIKey: string;
begin
  if TranslationManagerSettings.Providers.DeepL.ProVersion then
    Result := TranslationManagerSettings.Providers.DeepL.APIKey
  else
    Result := '';
end;

function TTranslationProviderDeepL.GetProviderName: string;
begin
  Result := sProviderNameDeepL;
end;

function TTranslationProviderDeepL.Lookup(Prop: TLocalizerProperty;
  SourceLanguage, TargetLanguage: TLanguageItem;
  Translations: TStrings): boolean;
var
  SourceText: string;
  TranslatedText: string;
begin
  SourceText := Prop.Value;
  try

    TranslatedText := TranslateText(SourceLanguage.ISO639_1Name, TargetLanguage.ISO639_1Name, SourceText);

  except
    on E: Exception do
      raise EDeepLLocalizationProvider.Create(E.Message)
  end;

  Result := (TranslatedText <> '');
  if (Result) then
    Translations.Add(TranslatedText);

end;

var
  ProviderHandle: integer = -1;

initialization
  ProviderHandle := TranslationProviderRegistry.RegisterProvider(sProviderNameDeepL,
    function(): ITranslationProvider
    begin
      Result := TTranslationProviderDeepL.Create;
    end);

finalization

  TranslationProviderRegistry.UnregisterProvider(ProviderHandle);

end.
