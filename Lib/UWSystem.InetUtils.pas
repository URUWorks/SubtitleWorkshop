{*
 *  URUWorks InetUtils
 *
 *  The contents of this file are used with permission, subject to
 *  the Mozilla Public License Version 1.1 (the "License"); you may
 *  not use this file except in compliance with the License. You may
 *  obtain a copy of the License at
 *  http://www.mozilla.org/MPL/MPL-1.1.html
 *
 *  Software distributed under the License is distributed on an
 *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing
 *  rights and limitations under the License.
 *
 *  Copyright (C) 2001-2022 URUWorks, uruworks@gmail.com.
 *
 *}

unit UWSystem.InetUtils;

{$mode objfpc}{$H+}

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils;

type
  TOnProgress = procedure(Sender: TObject; Percent: Integer) of object;

  { TStreamAdapter }

   TStreamAdapter = class(TStream)
   strict private
     FOnProgress: TOnProgress;
     FPercent: integer;
     FStream: TStream;
   public
     constructor Create(AStream: TStream; ASize: int64);
     destructor Destroy; override;
     function Read(var Buffer; Count: longint): longint; override;
     function Write(const Buffer; Count: longint): longint; override;
     function Seek(Offset: longint; Origin: word): longint; override;
     procedure DoProgress; virtual;
   published
     property OnProgress: TOnProgress read FOnProgress write FOnProgress;
   end;

function IsInternetAlive: Boolean;
function GetURLAsString(const URL: String): String;
function DownloadFileToStream(const URL: String): TMemoryStream;
procedure DonwloadFile(OnProgress: TOnProgress; const cUrl, cFile: String);

// -----------------------------------------------------------------------------

implementation

uses IdHTTP, fphttpclient;

// -----------------------------------------------------------------------------

function IsInternetAlive: Boolean;
var
  http: TIdHTTP;
begin
  Result := False;
  try
    http := TIdHTTP.Create(NIL);
    try
      http.Head('http://www.google.com');
      // HTTP is OK, and HEAD is successful
      Result := True;
    finally
      http.Free;
    end;
  except
    // Indy uses different exception types for different things...
    on E: EIdHTTPProtocolException do
    begin
      // HTTP is OK, just HEAD failed on the server side...
      Result := True;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function GetURLAsString(const URL: String): String;
var
  HTTP : TIdHTTP;
begin
  HTTP := TIdHTTP.Create(NIL);
  try
    try
      HTTP.Request.ContentType     := 'text/xml'; //'text/xml; charset=utf-8';
      HTTP.Request.ContentEncoding := 'utf-8';
      HTTP.Request.CharSet         := 'utf-8';
      Result := HTTP.Get(URL);
    except
      Result := '';
    end;
  finally
    HTTP.Disconnect;
    HTTP.Free;
  end;
end;

// -----------------------------------------------------------------------------

function DownloadFileToStream(const URL: String): TMemoryStream;
var
  HTTP   : TIdHTTP;
begin
  HTTP   := TIdHTTP.Create(NIL);
  Result := TMemoryStream.Create;
  try
    try
      HTTP.Get(URL, Result);
    except
    end;
  finally
    HTTP.Free;
  end;
end;

// -----------------------------------------------------------------------------

{ TStreamAdapter }

constructor TStreamAdapter.Create(AStream: TStream; ASize: int64);
begin
  inherited Create;
  FStream := AStream;
  FStream.Size := ASize;
  FStream.Position := 0;
end;

// -----------------------------------------------------------------------------

destructor TStreamAdapter.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TStreamAdapter.Read(var Buffer; Count: longint): longint;
begin
  Result := FStream.Read(Buffer, Count);
  DoProgress;
end;

// -----------------------------------------------------------------------------

function TStreamAdapter.Write(const Buffer; Count: longint): longint;
begin
  Result := FStream.Write(Buffer, Count);
  DoProgress;
end;

// -----------------------------------------------------------------------------

function TStreamAdapter.Seek(Offset: longint; Origin: word): longint;
begin
  Result := FStream.Seek(Offset, Origin);
end;

// -----------------------------------------------------------------------------

procedure TStreamAdapter.DoProgress;
begin
  FPercent := Trunc((FStream.Position) / (FStream.Size) * 100);
  if Assigned(OnProgress) then OnProgress(Self, FPercent);
end;

// -----------------------------------------------------------------------------

procedure DonwloadFile(OnProgress: TOnProgress; const cUrl, cFile: String);
// cUrl  = 'https://uruworks.net/downloads/sw.zip';
// cFile = 'sw.zip';
var
  vStream: TStreamAdapter;
  vHTTP: TFPHTTPClient;
  vSize: int64 = 0;
  i: integer;
  s: string;
begin
  try
    vHTTP := TFPHTTPClient.Create(nil);
    vHTTP.HTTPMethod('HEAD', cUrl, nil, [200]);

    for i := 0 to pred(vHTTP.ResponseHeaders.Count) do
    begin
      s := UpperCase(vHTTP.ResponseHeaders[I]);
      if Pos('CONTENT-LENGTH:', s) > 0 then
      begin
        vSize := StrToIntDef(Copy(s, Pos(':', s) + 1, Length(s)), 0);
        Break;
      end;
    end;

    vStream := TStreamAdapter.Create(TFileStream.Create(cFile, fmCreate), vSize);
    vStream.OnProgress := OnProgress;

    vHTTP.HTTPMethod('GET', cUrl, vStream, [200]);
  finally
    vHTTP.Free;
    vStream.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
