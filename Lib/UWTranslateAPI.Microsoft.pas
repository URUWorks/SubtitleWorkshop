unit UWTranslateAPI.Microsoft;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, IdHTTP, IdURI, laz2_XMLRead, laz2_DOM;

const
  MicrosoftTranslatorLocale: array[0..34] of String =
  (
    'ar',    // Arabic
    'bg',    // Bulgarian
    'zh-CHS', // Chinese Simplified
    'zh-CHT', // Chinese Traditional
    'cs',    // Czech
    'da',    // Danish
    'nl',    // Dutch
    'en',    // English
    'et',    // Estonian
    'fi',    // Finnish
    'fr',    // French
    'de',    // German
    'el',    // Greek
    'ht',    // Haitian Creole
    'he',    // Hebrew
    'hu',    // Hungarian
    'id',    // Indonesian
    'it',    // Italian
    'ja',    // Japanese
    'ko',    // Korean
    'lv',    // Latvian
    'lt',    // Lithuanian
    'no',    // Norwegian
    'pl',    // Polish
    'pt',    // Portuguese
    'ro',    // Romanian
    'ru',    // Russian
    'sk',    // Slovak
    'sl',    // Slovenian
    'es',    // Spanish
    'sv',    // Swedish
    'th',    // Thai
    'tr',    // Turkish
    'uk',    // Ukrainian
    'vi'     // Vietnamese
  );

  MicrosoftTranslatorName: array[0..34] of String =
  (
    'Arabic',
    'Bulgarian',
    'Chinese Simplified',
    'Chinese Traditional',
    'Czech',
    'Danish',
    'Dutch',
    'English',
    'Estonian',
    'Finnish',
    'French',
    'German',
    'Greek',
    'Haitian Creole',
    'Hebrew',
    'Hungarian',
    'Indonesian',
    'Italian',
    'Japanese',
    'Korean',
    'Latvian',
    'Lithuanian',
    'Norwegian',
    'Polish',
    'Portuguese',
    'Romanian',
    'Russian',
    'Slovak',
    'Slovenian',
    'Spanish',
    'Swedish',
    'Thai',
    'Turkish',
    'Ukrainian',
    'Vietnamese'
  );

function TranslateText(const AText, SourceLng, DestLng: String): String;
function DetectLanguage(const AText:string): String;

// -----------------------------------------------------------------------------

implementation

const
  MicrosoftTranslatorTranslateUri = 'http://api.microsofttranslator.com/v2/Http.svc/Translate?appId=%s&text=%s&from=%s&to=%s';
  MicrosoftTranslatorDetectUri    = 'http://api.microsofttranslator.com/v2/Http.svc/Detect?appId=%s&text=%s';
  MicrosoftTranslatorGetLngUri    = 'http://api.microsofttranslator.com/v2/Http.svc/GetLanguagesForTranslate?appId=%s';
  MicrosoftTranslatorGetSpkUri    = 'http://api.microsofttranslator.com/v2/Http.svc/GetLanguagesForSpeak?appId=%s';
  MicrosoftTranslatorSpeakUri     = 'http://api.microsofttranslator.com/v2/Http.svc/Speak?appId=%s&text=%s&language=%s';
  BingAppId                       = '73C8F474CA4D1202AD60747126813B731199ECEA';

// -----------------------------------------------------------------------------

function HTTPGet(const url: String): String;
var
  ResponseContent : TStringStream;
  HTTP            : TIdHTTP;
begin
  ResponseContent := TStringStream.Create('', TEncoding.UTF8);
  HTTP            := TIdHTTP.Create(NIL);
  try
    try
      HTTP.Request.ContentType     := 'text/xml'; //'text/xml; charset=utf-8';
      HTTP.Request.ContentEncoding := 'utf-8';
      HTTP.Request.CharSet         := 'utf-8';
      //HTTP.HTTPOptions             := [hoForceEncodeParams];
      HTTP.Get(TIdURI.ParamsEncode(url), ResponseContent);
      ResponseContent.Position := 0;
      Result := ResponseContent.ReadString(ResponseContent.Size);
    except
      Result := '';
    end;
  finally
    HTTP.Disconnect;
    HTTP.Free;
    ResponseContent.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TranslateText(const AText, SourceLng, DestLng: String): String;
var
  XmlDoc : TXMLDocument;
  Node   : TDOMNode;
begin
  Result := HttpGet(Format(MicrosoftTranslatorTranslateUri, [BingAppId, AText, SourceLng, DestLng]));
  if Result <> '' then
  begin
    XmlDoc := TXMLDocument.Create;
    try
      try
        ReadXMLfile(XmlDoc, Result); //  XmlDoc.LoadFromXML(Result);
//        XmlDoc.Active := True;
        Node          := XmlDoc.DocumentElement;
        Result        := Node.TextContent;
      except
        Result := '';
      end;
    finally
       Node   := NIL;
       XmlDoc := NIL;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function DetectLanguage(const AText:string): String;
var
  XmlDoc : TXMLDocument;
  Node   : TDOMNode;
begin
  Result := HttpGet(Format(MicrosoftTranslatorDetectUri, [BingAppId, AText]));
  if Result <> '' then
  begin
    XmlDoc := TXMLDocument.Create;
    try
      try
        ReadXMLfile(XmlDoc, Result); //  XmlDoc.LoadFromXML(Result);
//        XmlDoc.Active := True;
        Node          := XmlDoc.DocumentElement;
        Result        := Node.TextContent;
      finally
        Result := '';
      end;
    finally
       Node   := NIL;
       XmlDoc := NIL;
    end;
  end;
end;

// -----------------------------------------------------------------------------

end.
