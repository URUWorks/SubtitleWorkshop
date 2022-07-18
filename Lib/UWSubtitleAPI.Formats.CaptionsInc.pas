{*
 *  URUWorks Subtitle API
 *
 *  Author  : URUWorks
 *  Website : uruworks.net
 *
 *  The contents of this file are used with permission, subject to
 *  the Mozilla Public License Version 2.0 (the "License"); you may
 *  not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/2.0.html
 *
 *  Software distributed under the License is distributed on an
 *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing
 *  rights and limitations under the License.
 *
 *  Copyright (C) 2001-2022 URUWorks, uruworks@gmail.com.
 *}

unit UWSubtitleAPI.Formats.CaptionsInc;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.StrUtils,
  UWSystem.SysUtils, UWSubtitleAPI.Formats, StrUtils;

type

  { TUWCaptionsInc }

  TUWCaptionsInc = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
    function ToText(const Subtitles: TUWSubtitles): String; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.ExtraInfo, UWSubtitleAPI.Tags;

// -----------------------------------------------------------------------------

function TUWCaptionsInc.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWCaptionsInc.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfCaptionsInc;
end;

// -----------------------------------------------------------------------------

function TUWCaptionsInc.Extension: String;
begin
  Result := '*.txt';
end;

// -----------------------------------------------------------------------------

function TUWCaptionsInc.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWCaptionsInc.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if SubtitleFile[Row].Contains('*Timecode type: ') or
     ((TimeInFormat(Copy(SubtitleFile[Row], 1, 11), 'hh:mm:ss:zz') and
     (TimeInFormat(Copy(SubtitleFile[Row], 13, 11), 'hh:mm:ss:zz') and
     (Length(SubtitleFile[Row]) = 23)))) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWCaptionsInc.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i, c        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to SubtitleFile.Count-1 do
    begin
      if ((TimeInFormat(Copy(SubtitleFile[i], 1, 11), 'hh:mm:ss:zz') and
       (TimeInFormat(Copy(SubtitleFile[i], 13, 11), 'hh:mm:ss:zz') and
       (Length(SubtitleFile[i]) = 23)))) and ((i+1) < SubtitleFile.Count-1) then
      begin
        InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 11));
        FinalTime   := StringToTime(Copy(SubtitleFile[i], 13, 11));
        Text        := '';

        c := i+1;
        while c < (SubtitleFile.Count-1) do
        begin
          if (Copy(SubtitleFile[c], 1, 1)='{') and (Copy(SubtitleFile[c], 4, 1)='[') then
          begin
            if Text = '' then
              Text := Copy(SubtitleFile[c], 7, Length(SubtitleFile[c]))
            else
              Text := Text + LineEnding + Copy(SubtitleFile[c], 7, Length(SubtitleFile[c]));

            Inc(c);
          end
          else
            Break;
        end;

        Subtitles.Add(InitialTime, FinalTime, Text, '', NIL, False);
      end;
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWCaptionsInc.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  SubFile : TUWStringList;
  i       : Integer;
  Text    : String;
begin
  Result  := False;
  SubFile := TUWStringList.Create;
  try
    SubFile.Add('*Timecode type: PAL/EBU', False);
    SubFile.Add('', False);

    for i := FromItem to ToItem do
    begin
      Text := RemoveSWTags(Subtitles.Text[i]);
      SubFile.Add(TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:zz') + ' ' + TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:zz'), False);
      SubFile.Add('{0 [1 ' + ReplaceEnters(Text, LineEnding, ' '), False);
      SubFile.Add('', False);
    end;

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

function TUWCaptionsInc.ToText(const Subtitles: TUWSubtitles): String;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------

end.
