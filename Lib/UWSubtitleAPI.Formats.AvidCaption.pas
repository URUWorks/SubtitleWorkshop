{*
 *  URUWorks Subtitle API
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

unit UWSubtitleAPI.Formats.AvidCaption;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.StrUtils,
  UWSubtitleAPI.Formats;

type

  { TUWAvidCaption }

  TUWAvidCaption = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTimeBased: Boolean; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
    function ToText(const Subtitles: TUWSubtitles): String; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Tags;

const
  ATimeFormat = 'hh:mm:ss:ff';

// -----------------------------------------------------------------------------

function TUWAvidCaption.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWAvidCaption.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfAvidCaption;
end;

// -----------------------------------------------------------------------------

function TUWAvidCaption.Extension: String;
begin
  Result := '*.txt';
end;

// -----------------------------------------------------------------------------

function TUWAvidCaption.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWAvidCaption.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWAvidCaption.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if Contains('@ This file ', SubtitleFile[Row]) or Contains('<begin subtitles>', SubtitleFile[Row]) or
    ((TimeInFormat(Copy(SubtitleFile[Row], 1, 11), ATimeFormat))) and
    (TimeInFormat(Copy(SubtitleFile[Row], 13, 11), ATimeFormat)) and
    (Copy(SubtitleFile[Row], 12, 1) = ' ')  then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWAvidCaption.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    i := 0;
    while i < SubtitleFile.Count do
    begin
      if (TimeInFormat(Copy(SubtitleFile[i], 1, 11), ATimeFormat)) and
         (TimeInFormat(Copy(SubtitleFile[i], 13, 11), ATimeFormat)) then
      begin
        InitialTime := HHMMSSFFTimeToMS(Copy(SubtitleFile[i], 1, 11), FPS);
        FinalTime   := HHMMSSFFTimeToMS(Copy(SubtitleFile[i], 13, 11), FPS);
        Text        := '';

        Inc(i);
        while (i < SubtitleFile.Count) and
              (not TimeInFormat(Copy(SubtitleFile[i], 1, 11), ATimeFormat)) and
              (not TimeInFormat(Copy(SubtitleFile[i], 13, 11), ATimeFormat)) do
        begin
          if Text <> '' then
            Text := Text + #13#10 + SubtitleFile[i]
          else
            Text := SubtitleFile[i];

          Inc(i);
        end;
        Dec(i);

        if (InitialTime > -1) and (FinalTime > -1) then
          Subtitles.Add(InitialTime, FinalTime, Text, '', NIL, False);
      end;
      Inc(i);
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function TUWAvidCaption.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  SubFile : TUWStringList;
  i       : Integer;
begin
  Result  := False;
  SubFile := TUWStringList.Create;
  try
    SubFile.Add('@ This file written with the Avid Caption plugin, version 1');
    SubFile.Add('', False);
    SubFile.Add('<begin subtitles>');

    for i := FromItem to ToItem do
    begin
      SubFile.Add(MSToHHMMSSFFMax(Subtitles.InitialTime[i], FPS) + ' ' +
        MSToHHMMSSFFMax(Subtitles.FinalTime[i], FPS));

      SubFile.Add(RemoveSWTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i])));
      SubFile.Add('', False);
    end;

    SubFile.Add('<end subtitles>');

    try
      SubFile.SaveToFile(FileName, Encoding);
      Result := True;
    except
    end;
  finally
    SubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWAvidCaption.ToText(const Subtitles: TUWSubtitles): String;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------

end.
