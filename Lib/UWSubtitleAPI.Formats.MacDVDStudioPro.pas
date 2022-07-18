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

unit UWSubtitleAPI.Formats.MACDVDStudioPro;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.StrUtils,
  UWSystem.SysUtils, UWSubtitleAPI.Formats, StrUtils;

type

  { TUWMacDVDStudioPro }

  TUWMacDVDStudioPro = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTimeBased: Boolean; override;
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

function TUWMacDVDStudioPro.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWMacDVDStudioPro.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfMACDVDStudioPro;
end;

// -----------------------------------------------------------------------------

function TUWMacDVDStudioPro.Extension: String;
begin
  Result := '*.txt';
end;

// -----------------------------------------------------------------------------

function TUWMacDVDStudioPro.IsTimeBased: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWMacDVDStudioPro.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWMacDVDStudioPro.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  //00:00:03:00 00:00:08:00 Text<P>Text
  if (TimeInFormat(Copy(SubtitleFile[Row], 1, 11), 'hh:mm:ss:zz')) and
     (TimeInFormat(Copy(SubtitleFile[Row], 13, 11), 'hh:mm:ss:zz')) and
     (Pos(',', SubtitleFile[Row]) = 12) and
     (StringCount(',', SubtitleFile[Row]) = 2) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWMacDVDStudioPro.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to SubtitleFile.Count-1 do
    begin
      if not TimeInFormat(Copy(SubtitleFile[i], 1, 8), 'hh:mm:ss') then Continue;
      InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 8));
      FinalTime   := StringToTime(Copy(SubtitleFile[i], 13, 8));

      if IsInteger(Copy(SubtitleFile[i], 10, 2)) then
        InitialTime := InitialTime + FramesToTime(StrToInt(Copy(SubtitleFile[i], 10, 2)), FPS);
      if IsInteger(Copy(SubtitleFile[i], 22, 2)) then
        FinalTime := FinalTime + FramesToTime(StrToInt(Copy(SubtitleFile[i], 22, 2)), FPS);

      Text := ReplaceString(Copy(SubtitleFile[i], 25, Length(SubtitleFile[i])), '<P>', LineEnding);

      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime, FinalTime, Text, '');
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWMacDVDStudioPro.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  SubFile : TUWStringList;
  InitialTime : String;
  FinalTime   : String;
  i           : Integer;
begin
  Result  := False;
  SubFile := TUWStringList.Create;
  try
    for i := FromItem to ToItem do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i]);

      // Time format is hh:mm:ss:ff
      InitialTime := TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:') +
                     AddChar('0', IntToStr(GetMSecsInFrames(Subtitles[i].InitialTime, FPS)), 2);

      FinalTime := TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:') +
                   AddChar('0', IntToStr(GetMSecsInFrames(Subtitles[i].FinalTime, FPS)), 2);

      SubFile.Add(InitialTime + #9 + FinalTime + #9 + ReplaceString(Subtitles[i].Text, LineEnding, '<P>'));
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

function TUWMacDVDStudioPro.ToText(const Subtitles: TUWSubtitles): String;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------

end.
