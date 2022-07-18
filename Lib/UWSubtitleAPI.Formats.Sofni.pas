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

unit UWSubtitleAPI.Formats.Sofni;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.StrUtils,
  UWSystem.SysUtils, UWSubtitleAPI.Formats;

type

  { TUWSofni }

  TUWSofni = class(TUWSubtitleCustomFormat)
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

function TUWSofni.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWSofni.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfSofni;
end;

// -----------------------------------------------------------------------------

function TUWSofni.Extension: String;
begin
  Result := '*.sub';
end;

// -----------------------------------------------------------------------------

function TUWSofni.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWSofni.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (Pos('\', SubtitleFile[Row]) = 12) or SubtitleFile[Row].StartsWith('*PART 1*', True) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWSofni.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
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
      if SubtitleFile[i].StartsWith('*PART 1*', True) then Inc(i, 2)
      else if SubtitleFile[i].StartsWith('*END*', True) then Break;

      if Pos('\', SubtitleFile[i]) <> 12 then
      begin
        Text := '';
        while (i < SubtitleFile.Count) and (Pos('\', SubtitleFile[i]) <> 12) do
        begin
          if Text <> '' then
            Text := Text + LineEnding + SubtitleFile[i]
          else
            Text := SubtitleFile[i];

          Inc(i);
        end;
        InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 11));
        FinalTime   := StringToTime(Copy(SubtitleFile[i], 13, 11));

        Text := HTMLTagsToSW(Text);
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

function TUWSofni.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  SubFile : TUWStringList;
  i       : Integer;
  Text    : String;
begin
  Result  := False;
  SubFile := TUWStringList.Create;
  try
    SubFile.Add('*PART 1*');
    SubFile.Add('00:00:00.00\00:00:00.00');
    for i := FromItem to ToItem do
    begin
      Text := SWTagsToHTML(Subtitles.Text[i]);
      SubFile.Add(Text, False);
      SubFile.Add(TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss.ff', FPS) + '\' + TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss.ff', FPS), False);
    end;
    SubFile.Add('*END*');
    SubFile.Add('...........\...........');
    SubFile.Add('*CODE*');
    SubFile.Add('0000000000000000');
    SubFile.Add('*CAST*');
    SubFile.Add('*GENERATOR*');
    SubFile.Add('*FONTS*');
    SubFile.Add('*READ*');
    SubFile.Add('0.300 15.000 130.000 100.000 25.000');
    SubFile.Add('*TIMING*');
    SubFile.Add('1 30 1 1 1');
    SubFile.Add('*TIMED BACKUP NAME*');
    SubFile.Add('C:\');
    SubFile.Add('*FORMAT SAMPLE ÅåÉéÌìÕõÛûÿ*');
    SubFile.Add('*READ ADVANCED*');
    SubFile.Add('< > 1 1 0.300');
    SubFile.Add('*MARKERS*');

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

function TUWSofni.ToText(const Subtitles: TUWSubtitles): String;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------

end.
