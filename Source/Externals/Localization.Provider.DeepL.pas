unit Localization.Provider.DeepL;

interface

uses
  System.SysUtils, System.Classes,

  amLanguageInfo,
  amLocalization.Model,
  amLocalization.Provider;

const
  DeepLAPIAddressFree = 'https://api-free.deepl.com/v2/translate';
  DeepLAPIAddressPro = 'https://api.deepl.com/v2/translate';

// -----------------------------------------------------------------------------
//
// TTranslationProviderDeepL
//
// -----------------------------------------------------------------------------

type
  TTranslationProviderDeepL = class(TInterfacedObject, ITranslationProvider)
  protected
    // ITranslationProvider
    function BeginLookup(SourceLanguage, TargetLanguage: TLanguageItem)
      : boolean;
    function Lookup(Prop: TLocalizerProperty;
      SourceLanguage, TargetLanguage: TLanguageItem;
      Translations: TStrings): boolean;
    procedure EndLookup;
    function GetProviderName: string;
  public
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

var
  DeepLAPIAddress: string;

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
// DeepL TraslateText function
//
// -----------------------------------------------------------------------------


function DeepLTranslateText(const AAPIKey, ASourceLang, ATargetLang,
  AText: string): string;
var
  RequestClient: THTTPClient;
  RequestParams: TStringList;
  HTTPResponse: IHTTPResponse;
  JSONResponse, JSONTranslationItem: TJSONObject;
  JSONResultArray: TJSONArray;
  TextResponse: string;
begin

  Result := '';

  // Verify the validity of the input parameters
  if AAPIKey.IsEmpty or (RightStr(AAPIKey, 3) <> ':fx') then
    raise Exception.Create(sDeepLErrorInvalidAPIKey);

  if not MatchText(ASourceLang, DeepLValidLanguages) then
    raise Exception.CreateFmt(sDeepLErrorInvalidSourceLang, [ASourceLang]);

  if not MatchText(ATargetLang, DeepLValidLanguages) then
    raise Exception.CreateFmt(sDeepLErrorInvalidTargetLang, [ASourceLang]);

  RequestClient := THTTPClient.Create;

  try

    // Setup the parameters to for the HTTP request

    RequestParams := TStringList.Create;
    RequestParams.DefaultEncoding := TEncoding.UTF8;

    RequestParams.Add('auth_key=' + AAPIKey);
    RequestParams.Add('source_lang=' + ASourceLang);
    RequestParams.Add('target_lang=' + ATargetLang);
    RequestParams.Add('text=' + AText);
    RequestParams.Add('split_sentences=0');
    RequestParams.Add('preserve_formatting=1');
    RequestParams.Add('formality=default');

    // Call web service
    try
      HTTPResponse := RequestClient.Post(DeepLAPIAddress, RequestParams);
    finally
      RequestParams.Free;
    end;

    // Checks the possible exceptions on the result
    if not Assigned(HTTPResponse) then
      raise Exception.Create(sDeepLErrorUnefinedResponse)
    else if HTTPResponse.StatusCode = 200 then // Response ok
    begin
      TextResponse := HTTPResponse.ContentAsString(TEncoding.UTF8);
      if TextResponse.IsEmpty then
        raise Exception.Create(sDeepLErrorEmptyResponse);
      JSONResponse := TJSONObject.ParseJSONValue(TextResponse) as TJSONObject;
      if not Assigned(JSONResponse) then
        raise Exception.Create(sDeepLErrorInvalidResponse);
      try
        JSONResultArray := JSONResponse.GetValue('translations') as TJSONArray;
        if not Assigned(JSONResultArray) then
          raise Exception.Create(sDeepLErrorInvalidResponse);
        if JSONResultArray.Count <> 1 then
          raise Exception.Create(sDeepLErrorMoreThanOneTranslation);
        JSONTranslationItem := JSONResultArray[0] as TJSONObject;
        if not Assigned(JSONTranslationItem) then
          raise Exception.Create(sDeepLErrorInvalidTranslation);
        Result := (JSONTranslationItem.GetValue('text') as TJSONString).Value;
      finally
        JSONResponse.Free;
      end;
    end
    else if HTTPResponse.StatusCode = 429 then // Other responses
      raise Exception.Create(sDeepLErrorTooManyRequests)
    else if HTTPResponse.StatusCode = 403 then
      raise Exception.Create(sDeepLErrorForbidden)
    else
      raise Exception.CreateFmt(sDeepLErrorGeneralException,
        [HTTPResponse.StatusCode.ToString, HTTPResponse.StatusText]);

  finally
    RequestClient.Free;
  end;

end;

// -----------------------------------------------------------------------------
//
// TTranslationProviderDeepL
//
// -----------------------------------------------------------------------------

function TTranslationProviderDeepL.BeginLookup(SourceLanguage,
  TargetLanguage: TLanguageItem): boolean;
begin
  if TranslationManagerSettings.Providers.DeepL.ProVersion then
    DeepLAPIAddress := DeepLAPIAddressPro
  else
    DeepLAPIAddress := DeepLAPIAddressFree;
  Result := True;
end;

procedure TTranslationProviderDeepL.EndLookup;
begin
  DeepLAPIAddress := '';
end;

function TTranslationProviderDeepL.GetProviderName: string;
begin
  Result := sProviderNameDeepL;
end;

function TTranslationProviderDeepL.Lookup(Prop: TLocalizerProperty;
  SourceLanguage, TargetLanguage: TLanguageItem;
  Translations: TStrings): boolean;
var
  SourceText, TranslatedText, APIKey: string;
begin

  APIKey := TranslationManagerSettings.Providers.DeepL.APIKey;

  SourceText := Prop.Value;
  try
    TranslatedText := DeepLTranslateText(APIKey,
      SourceLanguage.ISO639_1Name, TargetLanguage.ISO639_1Name, SourceText);
  except
    on E: Exception do
      raise EDeepLLocalizationProvider.Create(E.Message)
  end;

  Result := False;
  if TranslatedText <> '' then
  begin
    Translations.Add(TranslatedText);
    Result := True;
  end;

end;

var
  ProviderHandle: integer = -1;

initialization
  ProviderHandle := TranslationProviderRegistry.RegisterProvider
    (sProviderNameDeepL,
    function(): ITranslationProvider
    begin
      Result := TTranslationProviderDeepL.Create;
    end);

finalization
  TranslationProviderRegistry.UnregisterProvider(ProviderHandle);

end.
