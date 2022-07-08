{*
 *  URUWorks Subtitle Workshop
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

unit UWSubtitles.Utils;

// -----------------------------------------------------------------------------

interface

uses SysUtils, StrUtils, Classes;

type

  TAutomaticDurationMode = (dmAlwaysNew, dmIfGreater, dmIfSmaller);

{ Timings }

function SetDurationLimits(const Duration, Min, Max: Cardinal): Cardinal;       // maximum duration and minimum duration
function SetDelay(const Time, Delay: Integer): Cardinal;                        // positive or negative, time or frames
function TimeExpander(const Text: String; const Duration, MSecsValue, CharsValue, MinMSecsDuration: Cardinal; const Expand: Boolean): Cardinal; // expand/reduce the final time of certain subtitles under certain conditions
function ExtendLength(const NextInitialTime: Cardinal): Cardinal;               // extend the length of selected subtitles to the start time of the next one
function AutomaticDurations(const Text: String; const Duration, msPerChar, msPerWord, msPerLine: Cardinal; const Mode: TAutomaticDurationMode): Cardinal; // calculate the duration of subtitles using a simple formula
procedure ShiftTime(const InitialTime, FinalTime, Value: Integer; out NewInitialTime, NewFinalTime: Cardinal); // Time to shift subtitle forwards/backwards

{ Texts }

function FixTags(const Text: String; const StartTag, EndTag: Char): String;
function RemoveUnnecessaryDots(Text: String): String;
function RemoveUnnecessarySpaces(const Text: String; const BreakChar: String = sLineBreak): String;
function HasProhibitedChars(Text, Chars: String): Boolean;
function HasTooLongLine(Text: String; const MaxChars: Integer = 42): Boolean;
function SmartLineAdjust(Text: String; const ChrsPerLine: Integer; const BreakChar: String = sLineBreak): String; // constrain subtitles bigger than three lines into two and adjust length of lines
function AutoBreakSubtitle(Text: String; const ChrsPerLine: Integer; const BreakChar: String = sLineBreak; const UnbreakBefore: Boolean = True): String;
function UnbreakSubtitles(const Text: String; const BreakChar: String = sLineBreak): String; // make subtitles be in one line
function FastDivideLines(Text: String; const InitialTime, FinalTime: Cardinal; const AddDots: Boolean = False; const ChrsPerLine: Integer = 43; const BreakChar: String = sLineBreak): String; // easily divide a subtitle with more than one line (or one big line) into two subtitles with proper time recalculation
function SetMaximumLineLength(const Text: String; const MaxChrs: Integer; const BreakChar: String = sLineBreak): String; // splits the subtitle in N number of lines so that each of the lines is shorter than a maximum specified length
function ReverseText(Text: String; const Enter: String = sLineBreak; const KeepLinesOrder: Boolean = True): String;
function FixRTLPunctuation(const Text: String; const Delimiter: String = sLineBreak): String;
function IsHearingImpaired(const Text: String): Boolean;
function FixHearingImpaired(const Text: String; const Enter: String = sLineBreak): String;
function HasRepeatedChar(const Text, RepeatableChars: String): Boolean;
function FixRepeatedChar(Text: String; const RepeatableChars: String): String;

// -----------------------------------------------------------------------------

implementation

uses UWSystem.SysUtils, UWSystem.StrUtils, RegExpr;

// -----------------------------------------------------------------------------

{ Timings }

// -----------------------------------------------------------------------------

function SetDurationLimits(const Duration, Min, Max: Cardinal): Cardinal;
begin
  Result := Range(Duration, Min, Max);
end;

// -----------------------------------------------------------------------------

function SetDelay(const Time, Delay: Integer): Cardinal;
begin
  if (Time + Delay) > 0 then
    Result := Time + Delay
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function TimeExpander(const Text: String; const Duration, MSecsValue, CharsValue,
  MinMSecsDuration: Cardinal; const Expand: Boolean): Cardinal;
var
  Apply : Boolean;
  pc    : Cardinal;
begin
  Result := Duration;
  Apply  := False;
  pc     := Length(Text);

  if (CharsValue > 0) and (pc > CharsValue) then Apply := True;

  if Expand then
  begin
    if (MinMSecsDuration > 0) and (Duration < MinMSecsDuration) then Apply := True;
    if Apply then Result := Duration + MSecsValue;
  end
  else
  begin
    if (MinMSecsDuration > 0) and (Duration > MinMSecsDuration) then Apply := True;
    if Apply then Result := Duration - MSecsValue;
  end;
end;

// -----------------------------------------------------------------------------

function ExtendLength(const NextInitialTime: Cardinal): Cardinal;
begin
  Result := NextInitialTime - 1;
end;

// -----------------------------------------------------------------------------

function AutomaticDurations(const Text: String; const Duration, msPerChar,
  msPerWord, msPerLine: Cardinal; const Mode: TAutomaticDurationMode): Cardinal;
var
  pc, pw,
  pl, nd: Cardinal;
begin
  Result := Duration;

  pc := Length(Text);
  pw := WordCount(Text);
  pl := LineCount(Text);
  nd := (pc * msPerChar) + (pw * msPerWord) + (pl * msPerLine);

  case Mode of
    dmAlwaysNew: Result := nd;
    dmIfGreater: if nd > Duration then Result := nd;
    dmIfSmaller: if nd < Duration then Result := nd;
  end;
end;

// -----------------------------------------------------------------------------

procedure ShiftTime(const InitialTime, FinalTime, Value: Integer;
  out NewInitialTime, NewFinalTime: Cardinal);
begin
  NewInitialTime := InitialTime + Value;
  NewFinalTime   := FinalTime   + Value;
end;

// -----------------------------------------------------------------------------

{ Texts }

// -----------------------------------------------------------------------------

function FixTags(const Text: String; const StartTag, EndTag: Char): String;

  function FixTag(const S: String; const Tag: Char): String;
  var
    sTag: String;
  begin
    Result := S;
    sTag   := Format('%s%s%s', [StartTag, Tag, EndTag]);
    Result := ReplaceString(Result, Format('%s %s %s', [StartTag, Tag, EndTag]), sTag);
    Result := ReplaceString(Result, Format('%s %s%s', [StartTag, Tag, EndTag]), sTag);
    Result := ReplaceString(Result, Format('%s%s %s', [StartTag, Tag, EndTag]), sTag);
    sTag   := Format('%s/%s%s', [StartTag, Tag, EndTag]);
    Result := ReplaceString(Result, Format('%s / %s %s', [StartTag, Tag, EndTag]), sTag);
    Result := ReplaceString(Result, Format('%s /%s%s', [StartTag, Tag, EndTag]), sTag);
    Result := ReplaceString(Result, Format('%s/%s %s', [StartTag, Tag, EndTag]), sTag);
    Result := ReplaceString(Result, Format('%s /%s %s', [StartTag, Tag, EndTag]), sTag);

    Result := ReplaceString(Result, Format('%s%s%s %s/%s%s', [StartTag, Tag, EndTag, StartTag, Tag, EndTag]), '');
    Result := ReplaceString(Result, Format('%s%s%s%s/%s%s', [StartTag, Tag, EndTag, StartTag, Tag, EndTag]), '');
  end;

begin
  Result := Text;
  Result := FixTag(Result, 'B'); Result := FixTag(Result, 'b');
  Result := FixTag(Result, 'I'); Result := FixTag(Result, 'i');
  Result := FixTag(Result, 'U'); Result := FixTag(Result, 'u');
  Result := FixTag(Result, 'S'); Result := FixTag(Result, 's');
end;

// -----------------------------------------------------------------------------

function RemoveUnnecessaryDots(Text: String): String;
begin
  while Pos('....', Text) > 0 do Delete(Text, Pos('....', Text), 1);
  Result := Text;
end;

// -----------------------------------------------------------------------------

function RemoveUnnecessarySpaces(const Text: String; const BreakChar: String = sLineBreak): String;
var
  v1, v2: String;
begin
  Result := Trim(Text);
  v1 := ' ' + BreakChar;
  v2 := BreakChar + ' ';

  while AnsiContainsText(Result, '  ') do Result := ReplaceString(Result, '  ', ' ');
  while AnsiContainsText(Result, v1) do Result := ReplaceString(Result, v1, BreakChar);
  while AnsiContainsText(Result, v2) do Result := ReplaceString(Result, v2, BreakChar);
  if AnsiEndsText(BreakChar, Result) then Result := Copy(Result, 1, Length(Result)-Length(BreakChar));
  if AnsiStartsText(BreakChar, Result) then Delete(Result, 1, 1);
end;

// -----------------------------------------------------------------------------

function HasProhibitedChars(Text, Chars: String): Boolean;
var
  split: TStringList;
  i: Integer;
begin
  Result := False;
  if (Text <> '') and (Chars <> '') then
  begin
    Text  := LowerCase(Text);
    Chars := LowerCase(Chars);
    split := TStringList.Create;
    try
      SplitRegExpr('\,', Chars, split);
      for i := 0 to split.Count-1 do
      begin
        if AnsiContainsText(Text, split[i]) then
        begin
          Result := True;
          Break;
        end;
      end;
    finally
      split.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function HasTooLongLine(Text: String; const MaxChars: Integer = 42): Boolean;
var
  PosEnter : Integer;
begin
  Result   := False;
  PosEnter := Pos(sLineBreak, Text);
  while PosEnter > 0 do
  begin
    if PosEnter-1 >= MaxChars then
    begin
      Result := True;
      Exit;
    end;
    Text     := Copy(Text, PosEnter + 2, Length(Text) - PosEnter);
    PosEnter := Pos(sLineBreak, Text);
  end;
  Result := Length(Text) >= MaxChars;
end;

// -----------------------------------------------------------------------------

function SmartLineAdjust(Text: String; const ChrsPerLine: Integer; const BreakChar: String = sLineBreak): String;
var
  l, l2, i : Integer;
  s        : TStringList;
begin
  Result := Text;
  l := Length(Text);
  if (Text = '') or ((l <= ChrsPerLine) and not AnsiContainsText(Text, '-')) then Exit;

  Text := RemoveUnnecessarySpaces(Text, BreakChar);

  // try to break dialogs
  if AnsiContainsText(Text, '-') then
  begin
    s := TStringList.Create;
    try
      SplitRegExpr('\-', Text, s);
      if s.Count > 0 then
      begin
        if Pos('-', Text) > 1 then
          Result := Copy(Text, 1, Pos('-', Text)-1) + BreakChar
        else
          Result := '';

        for i := 1 to s.Count - 1 do
        begin
          Result := Format('%s-%s%s', [Result, s[i], BreakChar]);
        end;
        Result := RemoveUnnecessarySpaces(Result, BreakChar);
      end;
    finally
      s.Free;
    end;
  end
  else
  begin
    l2 := l div 2;
    if l2 > (ChrsPerLine div 2) then l2 := ChrsPerLine;

    while (Result[l2] <> ' ') and (l2 < l) do Inc(l2);

    for i := l2-1 downto 1 do
      if Result[i] = ' ' then
      begin
        Insert(BreakChar, Result, i);
        Result := RemoveUnnecessarySpaces(Result, BreakChar);
        Break;
      end;
  end;
end;

// -----------------------------------------------------------------------------

function AutoBreakSubtitle(Text: String; const ChrsPerLine: Integer; const BreakChar: String = sLineBreak; const UnbreakBefore: Boolean = True): String;
begin
  if UnbreakBefore then Text := UnbreakSubtitles(Text, BreakChar);
  Result := SmartLineAdjust(Text, ChrsPerLine, BreakChar);
end;

// -----------------------------------------------------------------------------

function UnbreakSubtitles(const Text: String; const BreakChar: String = sLineBreak): String;
begin
  Result := RemoveUnnecessarySpaces(ReplaceString(Text, BreakChar, ' '), BreakChar);
end;

// -----------------------------------------------------------------------------

function FastDivideLines(Text: String; const InitialTime, FinalTime: Cardinal; const AddDots: Boolean = False; const ChrsPerLine: Integer = 43; const BreakChar: String = sLineBreak): String;
var
  s               : TStringList;
  i, ft, duration : Cardinal;
  dots            : Boolean;
  str             : String;
begin
  Result := '';
  dots   := False;
  s := TStringList.Create;
  try
    SplitRegExpr('\'+BreakChar, Text, s);
    if s.Count = 0 then
    begin
      Text := AutoBreakSubtitle(Text, ChrsPerLine, BreakChar, False);
      SplitRegExpr('\'+BreakChar, Text, s);
      if s.Count = 0 then Exit;
    end;

    duration := (FinalTime - InitialTime) div s.Count;
    ft       := InitialTime + duration;


    if AddDots and not AnsiEndsText(',', s[0]) and not AnsiEndsText('.', s[0]) then
    begin
      dots := True;
      str  := s[0]+'...';
    end
    else
      str := s[0];

    Result := Format('%d||%d||%s||', [InitialTime, ft, str]);

    for i := 1 to s.Count - 1 do
    begin
      if (i = 1) and dots and not AnsiStartsText('-', s[1]) then
        str := '...'+s[i]
      else
        str := s[i];

      Result := Format('%s%d||%d||%s||', [Result, ft + 1, ft + duration, str]);
      ft := ft + duration;
    end;
  finally
    s.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SetMaximumLineLength(const Text: String; const MaxChrs: Integer; const BreakChar: String = sLineBreak): String;
var
  l, l2,
  i, i2,
  p, c : Integer;
  max  : Integer;
begin
  Result := Text;
  l      := Length(Text);
  if (Text = '') or (l <= MaxChrs) then Exit;
  max   := l div MaxChrs;
  l2    := l div 2;
  p     := 1;
  i2    := 1;
  for c := 0 to max do
  begin
    for i := l2-1 downto p do
      if Result[i] = ' ' then
      begin
        Delete(Result, i, 1);
        Insert(BreakChar, Result, i);
        i2 := i;
        Break;
      end;
    p  := i2;
    l2 := l2*max;
  end;
end;

// -----------------------------------------------------------------------------

function ReverseText(Text: String; const Enter: String = sLineBreak; const KeepLinesOrder: Boolean = True): String;
var
  x, TotalLines, c   : Integer;
  PosEnter, NewEnter : Integer;
begin
  try
    if KeepLinesOrder then
    begin
      c := iff(Enter = #13#10, 1, 0);

      SetLength(Result, Length(Text));
      TotalLines := 0;
      repeat
        NewEnter := 0;
        PosEnter := Pos(Enter, Text);

        if PosEnter = 0 then
          PosEnter := Length(Text)+1
        else
          NewEnter := PosEnter;

        for x := 1 to PosEnter-1 do
          Result[TotalLines+PosEnter-x] := Text[x];

        if (NewEnter <> 0) then
        begin
          if (Enter = #13#10) then
          begin
            Result[TotalLines+NewEnter]   := #13;
            Result[TotalLines+NewEnter+1] := #10;
          end
          else
            Result[TotalLines+NewEnter]   := Enter[1];
        end;

        Delete(Text, 1, PosEnter+c);
        Inc(TotalLines, PosEnter+c);
      until Text = '';
    end
    else
    begin
      Result := '';
      for x := 1 to Length(Text) do Result := Text[x] + Result;

      if (Enter = #13#10) then
        Result := ReplaceString(Result, #10#13, #13#10);
    end;
  except
    Result := '';
  end;
end;

// -----------------------------------------------------------------------------

function FixRTLPunctuation(const Text: String; const Delimiter: String = sLineBreak): String;
const
  SpecialChars = '.,:;''()-?!+=*&$^%#@~`" /';

var
  Posit : Integer;
  A,B   : String;

  function FixSubString(const Sub: String): String;
  var
    Prefix : String;
    Suffix : String;
    Temp   : String;
    P,I    : Integer;
  begin
    Temp   := Sub;
    Prefix := '';
    Suffix := '';
    I      := 1;
    if Temp = '' then
    begin
      Result := '';
      exit;
    end;

    P := Pos(Temp[i], SpecialChars);
    while P <> 0 do
    begin
      Prefix := Prefix + Temp[i];
      Temp   := Copy(Temp, 2, Length(Temp)-1);
      if Temp = '' then
        P := 0
      else
        P := Pos(Temp[i], SpecialChars);
    end;
    if Suffix = ' -' then Suffix := '- ';

    I := Length(Temp);
    if Temp = '' then
      P := 0
    else
      P := Pos(Temp[i], SpecialChars);
    while P <> 0 do
    begin
      Suffix := Suffix + Temp[I];
      Temp   := Copy(Temp, 1, Length(Temp)-1);
      i      := Length(Temp);
      if Temp = '' then
        P := 0
      else
        P := Pos(Temp[i], SpecialChars);
      end;
    if Prefix = '- ' then Prefix := ' -';

    Result := Suffix + Temp + Prefix;
  end;
begin
  A := Text;
  B := '';
  Posit := Pos(Delimiter, A);
  while Posit > 0 do
  begin
    B     := B + FixSubString(Copy(A, 1, Posit-1)) + Delimiter;
    A     := Copy(A, Posit + Length(Delimiter), Length(A) - Posit);
    Posit := Pos(Delimiter, A);
  end;
  B := B + FixSubString(A);
  Result := B;
end;

// -----------------------------------------------------------------------------

function IsHearingImpaired(const Text: String): Boolean;
begin
  if ((Pos('(', Text) > 0) and (Pos(')', Text) > Pos('(', Text))) or
    ((Pos('[', Text) > 0) and (Pos(']', Text) > Pos('[', Text))) or
    ((Pos('<', Text) > 0) and (Pos('>', Text) > Pos('<', Text))) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function FixHearingImpaired(const Text: String; const Enter: String = sLineBreak): String;

  function RHearingImpaired(Line: String): String;
  begin
    while (Pos('(', Line) > 0) and (Pos(')', Line) > Pos('(', Line)) do
    begin
      if Copy(Line, Pos(')', Line) + 1, 1) = ':' then Delete(Line, Pos(')', Line) + 1, 1);
      Delete(Line, Pos('(', Line), Pos(')', Line) - Pos('(', Line) + 1);
    end;
    while (Pos('[', Line) > 0) and (Pos(']', Line) > Pos('[', Line)) do
    begin
      if Copy(Line, Pos(']', Line) + 1, 1) = ':' then Delete(Line, Pos(']', Line) + 1, 1);
      Delete(Line, Pos('[', Line), Pos(']', Line) - Pos('[', Line) + 1);
    end;

    Result := Line;
  end;

var
  PosEnter : Integer;
  A, B     : String;
begin
  Result := '';
  if Text <> '' then
  begin
    A := Text;
    B := '';
    PosEnter := Pos(Enter, A);
    while PosEnter > 0 do
    begin
      B        := B + RHearingImpaired(Copy(A, 1, PosEnter-1)) + Enter;
      A        := Copy(A, PosEnter + Length(Enter), Length(A) - PosEnter);
      PosEnter := Pos(Enter, A);
    end;
    B := RemoveUnnecessarySpaces(RHearingImpaired(B + RHearingImpaired(A)));

    if (Pos(Enter, B) = 0) and (Copy(B, 1, 1) = '-') then
    begin
      B := Copy(B, Length(Enter), Length(B));
      Result := RemoveUnnecessarySpaces(B);
    end
    else
      Result := B;
  end;
end;

// -----------------------------------------------------------------------------

function HasRepeatedChar(const Text, RepeatableChars: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(Text)-1 do
  begin
    if (Pos(Text[i], RepeatableChars) > 0) and (Text[i+1] = Text[i]) then
      if (Text[i] <> '/') or (Copy(Text, i-1, 3) <> '://') then
      begin
        Result := True;
        exit;
      end;
  end;
end;

// -----------------------------------------------------------------------------

function FixRepeatedChar(Text: String; const RepeatableChars: String): String;
var
  i: Integer;
begin
  for i := Length(Text) downto 2 do
  begin
    if (Pos(Text[i], RepeatableChars) > 0) and (Text[i-1] = Text[i]) then
      if (Text[i] <> '/') or (Copy(Text, i-2, 3) <> '://') then
        Delete(Text, i, 1);
  end;
  Result := Text;
end;

// -----------------------------------------------------------------------------

end.
