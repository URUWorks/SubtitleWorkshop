{*
 *  URUWorks Lazarus StrUtils
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

unit UWSystem.StrUtils;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, StrUtils, RegExpr, lazUTF8, Math;

// -----------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: String): String; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Char): Char; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Byte): Byte; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Integer): Integer; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Cardinal): Cardinal; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Int64): Int64; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Boolean): Boolean; overload;

function Contains(const AFind, ASource: String): Boolean;

function ReplaceString(const S, OldPattern, NewPattern: String; ReplaceAll: Boolean = True; IgnoreCase: Boolean = True): String;
function ReplaceEnters(const S: String; const OldPattern: String = sLineBreak; const NewPattern: String = '|'): String;
function StringCount(const AFindString, ASourceString: String): Integer;
function StringCounts(const ASourceString: String; const AFindString: Array of String): Integer;
function IsDelimeter(const C: Char): Boolean;
function WordCount(const Text: String): LongInt;
function LineCount(const Text: String; const Pattern: String = '|'): Cardinal;
function IsUpperCase(const Text: String): Boolean;
function PreserveCase(const Text, NewText: String): String;
function SentenceCase(const Text: String): String;
function InvertCase(const Text: String): String;
function TitleCase(const Text: String): String;
function PosRE(const ARExpr, ASource: String): Integer;
function PosCS(const SubStr, Str: String): Integer;
function LastPos(const SubStr, S: String): Integer;
function RemoveTagsFromText(const Text: String): String;
function ReplaceWordString(const Text, Word, NewWord: String; const ReplaceAll: Boolean = False): String;
procedure SplitStr(const Delimiter, Text: String; var Pieces: TStringList);
procedure AnsiStringToAnsiChar(var Dest: array of AnsiChar; const Source: AnsiString);

function StringSimilarityRatio(const Str1, Str2: String; const IgnoreCase: Boolean): Double;

// -----------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: String): String;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

// -----------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Char): Char;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

// -----------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Byte): Byte;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

// -----------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Integer): Integer;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

// -----------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Cardinal): Cardinal;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

// -----------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Int64): Int64;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

// -----------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Boolean): Boolean;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

// -----------------------------------------------------------------------------

function Contains(const AFind, ASource: String): Boolean;
begin
  Result := UTF8Pos(AFind, ASource) > 0;
end;

// -----------------------------------------------------------------------------

function ReplaceString(const S, OldPattern, NewPattern: String; ReplaceAll: Boolean = True; IgnoreCase: Boolean = True): String;
var
  Flags : TReplaceFlags;
begin
  Flags := [];
  if ReplaceAll then Flags := Flags + [rfReplaceAll];
  if IgnoreCase then Flags := Flags + [rfIgnoreCase];
  Result := UTF8StringReplace(S, OldPattern, NewPattern, Flags);
end;

// -----------------------------------------------------------------------------

function ReplaceEnters(const S: String; const OldPattern: String = sLineBreak; const NewPattern: String = '|'): String;
begin
  Result := ReplaceString(S, OldPattern, NewPattern);
end;

// -----------------------------------------------------------------------------

function StringCount(const AFindString, ASourceString: String): Integer;
var
  i: Integer;
begin
  Result := 0;
  i      := 1;
  repeat
    i := PosEx(AFindString, ASourceString, i);
    if i > 0 then
    begin
      Inc(i);
      Inc(Result);
    end;
  until i = 0;
end;

// -----------------------------------------------------------------------------

function StringCounts(const ASourceString: String; const AFindString: Array of String): Integer;
var
  i, c: Integer;
begin
  Result := 0;
  c      := 0;

  for i := Low(AFindString) to High(AFindString) do
    c := c + StringCount(AFindString[i], ASourceString);

  Result := c;
end;

// -----------------------------------------------------------------------------

function IsDelimeter(const C: Char): Boolean;
begin
  Result := CharInSet(C, [#0..#$1F, ' ', '.', ',', '¿', '?', ':', ';', '(', ')',
    '|', '/', '\', '¡', '!', '"', '-', '_', '<', '>']);
end;

// -----------------------------------------------------------------------------

function WordCount(const Text: String): LongInt;
var
  Ix        : Word;
  WorkCount : LongInt;
begin
  WorkCount := 0;
  Ix        := 1;
  while Ix <= UTF8Length(Text) do
  begin
    while (Ix <= UTF8Length(Text)) and IsDelimeter(Text[Ix]) do Inc(Ix);
    if Ix <= UTF8Length(Text) then
    begin
      Inc(WorkCount);
      while (Ix <= UTF8Length(Text)) and (not IsDelimeter(Text[Ix])) do Inc(Ix);
    end;
  end;
  Result := WorkCount;
end;

// -----------------------------------------------------------------------------

function LineCount(const Text: String; const Pattern: String = '|'): Cardinal;
begin
  Result := StringCount(Pattern, Text) + 1;
end;

// -----------------------------------------------------------------------------

function IsUpperCase(const Text: String): Boolean;
begin
  Result := Text = UTF8UpperCase(Text);
end;

// -----------------------------------------------------------------------------

function PreserveCase(const Text, NewText: String): String;
var
  i,
  l1,
  l2: Integer;
begin
  Result := NewText;
  l1     := UTF8Length(Text);
  l2     := UTF8Length(NewText);
  if l1 = l2 then
  begin
    for i := 1 to l1 do
      if IsUpperCase(Text[i]) then
        Result[i] := UTF8UpperCase(NewText)[i]
      else
        Result[i] := LowerCase(NewText[i]);
  end
  else if IsUpperCase(Text) then
    Result := UTF8UpperCase(NewText);
end;

// -----------------------------------------------------------------------------

function SentenceCase(const Text: String): String;
var
  s: String;
begin
  Result := UTF8LowerCase(Text);
  if Result = '' then Exit;
  s := UTF8UpperCase(Text);
  Result[1] := s[1];
end;

// -----------------------------------------------------------------------------

function InvertCase(const Text: String): String;
var
  sl,
  su : String;
  i  : Integer;
begin
  Result := Text;
  if Result = '' then Exit;
  sl := UTF8LowerCase(Text);
  su := UTF8UpperCase(Text);
  for i := 0 to UTF8Length(Text) - 1 do
    if Result[i] = sl[i] then
      Result[i] := su[i]
    else
      Result[i] := sl[i];
end;

// -----------------------------------------------------------------------------

function TitleCase(const Text: String): String;
var
  su : String;
  i  : Integer;
  up : Boolean;
begin
  Result := UTF8LowerCase(Text);
  if Result = '' then Exit;
  su := UTF8UpperCase(Text);
  up := True;
  i  := 1;
  while i <= UTF8Length(Text) do
  begin
    while (i <= UTF8Length(Text)) and IsDelimeter(Text[i]) do
    begin
      Inc(i);
      up := True;
    end;
    if up and not IsDelimeter(Text[i]) then
    begin
      Result[i] := su[i];
      up := False;
    end;
    Inc(i);
  end;
end;

// -----------------------------------------------------------------------------

function PosRE(const ARExpr, ASource: String): Integer;
begin
  with TRegExpr.Create do
  try
    Expression := ARExpr;
    Exec(ASource);
    Result := MatchPos[0]; // -1 = not found
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

function PosCS(const SubStr, Str: String): Integer;
begin
  Result := Pos(AnsiLowerCase(SubStr), AnsiLowerCase(Str));
end;

// -----------------------------------------------------------------------------

function LastPos(const SubStr, S: String): Integer;
var
  Found, Len, Pos: Integer;
begin
  Pos   := UTF8Length(S);
  Len   := UTF8Length(SubStr);
  Found := 0;
  while (Pos > 0) and (Found = 0) do
  begin
    if UTF8Copy(S, Pos, Len) = SubStr then Found := Pos;
    Dec(Pos);
  end;
  LastPos := Found;
end;

// -----------------------------------------------------------------------------

function RemoveTagsFromText(const Text: String): String;
var
  a, b: Integer;
begin
  Result := UTF8Trim(Text);
  repeat
    a := UTF8Pos('<', Result);
    b := UTF8Pos('>', Result);
    if (a > 0) and (b > 0) then
    begin
      UTF8Delete(Result, a, b);
    end;
  until (a <= 0) or (b <= 0);
end;

// -----------------------------------------------------------------------------

function ReplaceWordString(const Text, Word, NewWord: String; const ReplaceAll: Boolean = False): String;

  function IsValidToCut(const C: Char): Boolean;
  begin
    Result := not CharInSet(C, ['0'..'9', 'a'..'z', 'A'..'Z']);
  end;

var
  x: Integer;
begin
  Result := Text;

  x := UTF8Pos(Word, Result);
  if x > 0 then
  begin
    if Result = Word then
      Result := NewWord
    else if (x = 1) and ((x+UTF8Length(Word)) <= UTF8Length(Result)) then
    begin
      if IsValidToCut( Result[x+UTF8Length(Word)] ) then
      begin
        UTF8Insert(NewWord, Result, x);
        UTF8Delete(Result, x+UTF8Length(NewWord), UTF8Length(Word));

        if ReplaceAll then Result := ReplaceWordString(Result, Word, NewWord);
      end;
    end
    else if (x > 1) and ((x+UTF8Length(Word))-1 = UTF8Length(Result)) then
    begin
      if IsValidToCut( Result[x-1] ) then
      begin
        UTF8Delete(Result, x, UTF8Length(Word));
        Result := Result + NewWord;

        if ReplaceAll then Result := ReplaceWordString(Result, Word, NewWord);
      end;
    end
    else if (x > 1) and ((x+UTF8Length(Word))-1 < UTF8Length(Result)) then
    begin
      if IsValidToCut( Result[x-1] ) and IsValidToCut( Result[x+UTF8Length(Word)] ) then
      begin
        UTF8Insert(NewWord, Result, x);
        UTF8Delete(Result, x+UTF8Length(NewWord), UTF8Length(Word));

        if ReplaceAll then Result := ReplaceWordString(Result, Word, NewWord);
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure SplitStr(const Delimiter, Text: String; var Pieces: TStringList);
begin
  if Text <> '' then
  begin
    if (Pieces = NIL) then Pieces := TStringList.Create;
    Pieces.Text := UTF8StringReplace(Text, Delimiter, sLineBreak, [rfReplaceAll]);
  end;
end;

// -----------------------------------------------------------------------------

procedure AnsiStringToAnsiChar(var Dest: array of AnsiChar; const Source: AnsiString);
var
  i, MaxLen: Integer;
begin
  MaxLen := High(Dest);

  for i := 0 to MaxLen do
    Dest[i] := #0;

  if MaxLen > Length(Source) then MaxLen := Length(Source)-1;

  for i := 0 to MaxLen do
    Dest[i] := Source[i+1];
end;

// -----------------------------------------------------------------------------

function DamerauLevenshteinDistance(const Str1, Str2: String): Integer;
var
  LenStr1, LenStr2: Integer;
  I, J, T, Cost, PrevCost: Integer;
  pStr1, pStr2, S1, S2: PChar;
  D: PIntegerArray;
begin
  LenStr1 := Length(Str1);
  LenStr2 := Length(Str2);

  // to save some space, make sure the second index points to the shorter string
  if LenStr1 < LenStr2 then
  begin
    T := LenStr1;
    LenStr1 := LenStr2;
    LenStr2 := T;
    pStr1 := PChar(Str2);
    pStr2 := PChar(Str1);
  end
  else
  begin
    pStr1 := PChar(Str1);
    pStr2 := PChar(Str2);
  end;

  // to save some time and space, look for exact match
  while (LenStr2 <> 0) and (pStr1^ = pStr2^) do
  begin
    Inc(pStr1);
    Inc(pStr2);
    Dec(LenStr1);
    Dec(LenStr2);
  end;

  while (LenStr2 <> 0) and ((pStr1+LenStr1-1)^ = (pStr2+LenStr2-1)^) do
  begin
    Dec(LenStr1);
    Dec(LenStr2);
  end;

  if LenStr2 = 0 then
  begin
    Result := LenStr1;
    Exit;
  end;

  // calculate the edit distance
  T := LenStr2 + 1;

  GetMem(D, T * SizeOf(Integer));

  for I := 0 to T-1 do
    D^[I] := I;

  S1 := pStr1;
  for I := 1 to LenStr1 do
  begin
    PrevCost := I-1;
    Cost := I;
    S2 := pStr2;
    for J := 1 to LenStr2 do
    begin
      if (S1^ = S2^) or ((I > 1) and (J > 1) and (S1^ = (S2 - 1)^) and (S2^ = (S1 - 1)^)) then
        Cost := PrevCost
      else
        Cost := 1 + MinIntValue([Cost, PrevCost, D^[J]]);
      PrevCost := D^[J];
      D^[J] := Cost;
      Inc(S2);
    end;
    Inc(S1);
  end;
  Result := D^[LenStr2];
  FreeMem(D, T * SizeOf(Integer));
end;

// -----------------------------------------------------------------------------

function StringSimilarityRatio(const Str1, Str2: String; const IgnoreCase: Boolean): Double;
var
  MaxLen: Integer;
  Distance: Integer;
begin
  Result := 1.0;
  if Length(Str1) > Length(Str2) then
    MaxLen := Length(Str1)
  else
    MaxLen := Length(Str2);

  if MaxLen <> 0 then
  begin
    if IgnoreCase then
      Distance := DamerauLevenshteinDistance(AnsiUpperCase(Str1), AnsiUpperCase(Str2))
    else
      Distance := DamerauLevenshteinDistance(Str1, Str2);
    Result := Result - (Distance / MaxLen);
  end;
end;

// -----------------------------------------------------------------------------


end.
