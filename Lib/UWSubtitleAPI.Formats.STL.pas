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

unit UWSubtitleAPI.Formats.STL;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.StrUtils,
  UWSystem.SysUtils, UWSubtitleAPI.Formats;

type

  { TUWSTL }

  TUWSTL = class(TUWSubtitleCustomFormat)
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

function STLTagsToSW(const Text: String): String;

  function ReplaceTag(const sText, Tag: String): String;
  var
    StartTag : Boolean;
    sTag     : String;
  begin
    Result   := sText;
    StartTag := True;
    sTag     := '^' + Tag;
    while Result.Contains(sTag) do
    begin
      if StartTag then
        Result := Result.Replace(sTag, '{\' + Tag + '1}', [rfIgnoreCase])
      else
        Result := Result.Replace(sTag, '{\' + Tag + '0}', [rfIgnoreCase]);

      StartTag := not StartTag;
    end;
  end;

begin
  Result := ReplaceTag(Text, 'b');
  Result := ReplaceTag(Result, 'i');
  Result := ReplaceTag(Result, 'u');
end;

// -----------------------------------------------------------------------------

function SWTagsToSTL(const Text: String): String;
begin
  Result := Text;
  Result := Result.Replace('{\b0}', '^B', [rfReplaceAll]);
  Result := Result.Replace('{\b1}', '^B', [rfReplaceAll]);
  Result := Result.Replace('{\i0}', '^I', [rfReplaceAll]);
  Result := Result.Replace('{\i1}', '^I', [rfReplaceAll]);
  Result := Result.Replace('{\u0}', '^U', [rfReplaceAll]);
  Result := Result.Replace('{\u1}', '^U', [rfReplaceAll]);
  Result := RemoveSWTags(Result);
end;

// -----------------------------------------------------------------------------

function TUWSTL.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWSTL.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfSTL;
end;

// -----------------------------------------------------------------------------

function TUWSTL.Extension: String;
begin
  Result := '*.stl';
end;

// -----------------------------------------------------------------------------

function TUWSTL.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWSTL.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (LowerCase(ExtractFileExt(SubtitleFile.FileName)) = '.stl') and
     (((TimeInFormat(Copy(SubtitleFile[Row], 1, 11), 'hh:mm:ss:zz')) and
     (Pos(',', SubtitleFile[Row]) = 13) and
     (TimeInFormat(Copy(SubtitleFile[Row], 15, 11), 'hh:mm:ss:zz'))) or
     ((TimeInFormat(Copy(SubtitleFile[Row], 1, 11), 'hh:mm:ss:zz')) and
     (Pos(',', SubtitleFile[Row]) = 12) and
     (TimeInFormat(Copy(SubtitleFile[Row], 13, 11), 'hh:mm:ss:zz')))) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWSTL.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
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
      if (TimeInFormat(Copy(SubtitleFile[i], 1, 11), 'hh:mm:ss:zz')) and
         (Pos(',', SubtitleFile[i]) = 13) and
         (TimeInFormat(Copy(SubtitleFile[i], 15, 11), 'hh:mm:ss:zz')) then
      begin
        InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 11));
        FinalTime   := StringToTime(Copy(SubtitleFile[i], 15, 11));
        Text        := Copy(SubtitleFile[i], 29, Length(SubtitleFile[i])-28);
      end
      else if (TimeInFormat(Copy(SubtitleFile[i], 1, 11), 'hh:mm:ss:zz')) and
              (Pos(',', SubtitleFile[i]) = 12) and
              (TimeInFormat(Copy(SubtitleFile[i], 13, 11), 'hh:mm:ss:zz')) then
      begin
        InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 11));
        FinalTime   := StringToTime(Copy(SubtitleFile[i], 13, 11));
        Text        := Copy(SubtitleFile[i], 25, Length(SubtitleFile[i])-24);
      end
      else
      begin
        InitialTime := -1;
        FinalTime   := -1;
      end;

      if (InitialTime > -1) and (FinalTime > -1) then
      begin
        Text := STLTagsToSW(ReplaceEnters(Text, '|', LineEnding));
        Subtitles.Add(InitialTime, FinalTime, Text, '', NIL, False);
      end;
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSTL.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  SubFile : TUWStringList;
  i       : Integer;
  Text    : String;
begin
  Result  := False;
  SubFile := TUWStringList.Create;
  try
    SubFile.Add('//Font select and font size');
    SubFile.Add('$FontName       = Arial');
    SubFile.Add('$FontSize       = 30');
    SubFile.Add('', False);
    SubFile.Add('//Character attributes (global)');
    SubFile.Add('$Bold           = FALSE');
    SubFile.Add('$UnderLined     = FALSE');
    SubFile.Add('$Italic         = FALSE');
    SubFile.Add('', False);
    SubFile.Add('//Position Control');
    SubFile.Add('$HorzAlign      = Center');
    SubFile.Add('$VertAlign      = Bottom');
    SubFile.Add('$XOffset        = 0');
    SubFile.Add('$YOffset        = 0');
    SubFile.Add('', False);
    SubFile.Add('//Contrast Control');
    SubFile.Add('$TextContrast           = 15');
    SubFile.Add('$Outline1Contrast       = 8');
    SubFile.Add('$Outline2Contrast       = 15');
    SubFile.Add('$BackgroundContrast     = 0');
    SubFile.Add('', False);
    SubFile.Add('//Effects Control');
    SubFile.Add('$ForceDisplay   = FALSE');
    SubFile.Add('$FadeIn         = 0');
    SubFile.Add('$FadeOut        = 0');
    SubFile.Add('', False);
    SubFile.Add('//Other Controls');
    SubFile.Add('$TapeOffset          = FALSE');
    SubFile.Add('//$SetFilePathToken  = <<:>>');
    SubFile.Add('', False);
    SubFile.Add('//Colors');
    SubFile.Add('$ColorIndex1    = 0');
    SubFile.Add('$ColorIndex2    = 1');
    SubFile.Add('$ColorIndex3    = 2');
    SubFile.Add('$ColorIndex4    = 3');
    SubFile.Add('', False);
    SubFile.Add('//Subtitles');

    for i := FromItem to ToItem do
      SubFile.Add(TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:zz') + ' , ' + TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:zz') + ' , ' + SWTagsToSTL(ReplaceEnters(Subtitles.Text[i])));

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

function TUWSTL.ToText(const Subtitles: TUWSubtitles): String;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------

end.
