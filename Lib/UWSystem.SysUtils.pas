{*
 *  URUWorks Lazarus SysUtils
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

unit UWSystem.SysUtils;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, Math, LCLIntf, Graphics;

// -----------------------------------------------------------------------------

function IsInteger(const Value: String): Boolean;
function IsFloat(Text: String; const FormatSettings: TFormatSettings): Boolean; overload;
function IsFloat(Text: String): Boolean; overload;

function StrToSingle(const S: String; const DefaultValue: Single = 0): Single;
function SingleToStr(const Value: Single; const Len: Byte = 3): String; overload;
function SingleToStr(const Value: Single; const Len: Byte; const FormatSettings: TFormatSettings): String; overload;
function Single2SmallInt(const Value: Single): SmallInt;
function BoolToStr(const Value: Boolean): String;
function StringToBoolean(const S: String; const Default: Boolean = False): Boolean;
function StringToByte(const S: String; const Default: Byte = 0): Byte;
function StringToShortInt(const S: String; const Default: ShortInt = 0): ShortInt;
function StringToSmallInt(const S: String; const Default: SmallInt = 0): SmallInt;
function StringToInt(const S: String; const Default: Integer = 0): Integer;

function HexToByte(const s: String): Byte;
function BaseToInt(const Number: String; const Base: Byte): Integer;
function HexStrToInt(S: String): Integer;
function IntToHexStr(const Color: Integer; const Invert: Boolean = False; const Prefix: String = ''): String;

function Range(const Value, Min, Max: Integer): Integer;
procedure Constrain(var Value: Single; const MinValue, MaxValue: Single); overload;
procedure Constrain(var Value: Integer; const MinValue, MaxValue: Integer); overload;
procedure Constrain(var Value: Int64; const MinValue, MaxValue: Int64); overload;
procedure Constrain(var Value: Cardinal; const MinValue, MaxValue: Cardinal); overload;
function MulDiv(nNumber, nNumerator, nDenominator: Single): Integer;
function RoundValue(const Value: Single; const Digits: Integer): Single;
function Rnd(r: Double): LongInt;
function LimitDecimals(Num: Real; Limit: Integer) : String;
procedure ArrayToTBytes(var DestArray: TBytes; const SourceArray: array of Byte; const StartIndex: Integer);
procedure TBytesToArray(var DestArray: array of Byte; const SourceArray: TBytes; const StartIndex: Integer);

function MixColors(C1, C2: TColor; Opacity: Integer = 70): TColor;

// -----------------------------------------------------------------------------

implementation

uses UWSystem.StrUtils;

//------------------------------------------------------------------------------

function IsInteger(const Value: String): Boolean;
var
  X, E: Integer;
begin
  Val(Value, X, E);
  Result := (E = 0);
end;

//------------------------------------------------------------------------------

function IsFloat(Text: String; const FormatSettings: TFormatSettings): Boolean;
var
  i: integer;
begin
  Text   := Trim(Text);
  Result := False;

  if Length(Text) = 0 then Exit;
  if not (Text[1] in ['0'..'9']) then Exit;

  for i := 1 to Length(Text) do
    if Pos(Text[i],'0123456789+-E' + FormatSettings.DecimalSeparator) = 0 then
      Exit;

  Result := True;
end;

// -----------------------------------------------------------------------------

function IsFloat(Text: String): Boolean;
begin
  IsFloat(Text, DefaultFormatSettings);
end;

// -----------------------------------------------------------------------------

function StrToSingle(const S: String; const DefaultValue: Single = 0): Single;
begin
  Result := StrToFloatDef(S, DefaultValue);
end;

// -----------------------------------------------------------------------------

function SingleToStr(const Value: Single; const Len: Byte = 3): String;
begin
  Result := SingleToStr(Value, Len, DefaultFormatSettings);
end;

// -----------------------------------------------------------------------------

function SingleToStr(const Value: Single; const Len: Byte; const FormatSettings: TFormatSettings): String;
begin
  Result := FloatToStr(Value, FormatSettings);
  if Length(Result) > 6 then
    Result := Format('%.' + IntToStr(Len) + 'f', [Value], FormatSettings);
end;

// -----------------------------------------------------------------------------

function Single2SmallInt(const Value: Single): SmallInt;
var
  i: Integer;
begin
  i := Round(Value * 32767);

  if (i > 32767)  then i := 32767;
  if (i < -32768) then i := -32768;

  Result := i;
end;

// -----------------------------------------------------------------------------

function BoolToStr(const Value: Boolean): String;
begin
  Result := IntToStr(Integer(Value));
end;

// -----------------------------------------------------------------------------

function StringToBoolean(const S: String; const Default: Boolean = False): Boolean;
begin
  if S = '' then
    Result := Default
  else if Length(S) = 1 then
    Result := Iff(S = '0', False, True)
  else
    Result := Iff(LowerCase(S) = 'false', False, True);
end;

// -----------------------------------------------------------------------------

function StringToByte(const S: String; const Default: Byte = 0): Byte;
begin
  Result := Byte(StringToInt(S, Default));
end;

// -----------------------------------------------------------------------------

function StringToShortInt(const S: String; const Default: ShortInt = 0): ShortInt;
begin
  Result := ShortInt(StringToInt(S, Default));
end;

// -----------------------------------------------------------------------------

function StringToSmallInt(const S: String; const Default: SmallInt = 0): SmallInt;
begin
  Result := SmallInt(StringToInt(S, Default));
end;

// -----------------------------------------------------------------------------

function StringToInt(const S: String; const Default: Integer = 0): Integer;
begin
  if S = '' then
    Result := Default
  else
    Result := StrToIntDef(S, Default);
end;

// -----------------------------------------------------------------------------

function HexToByte(const s: String): Byte;
const
  cs = '0123456789ABCDEF';

begin

  if (Length(s) = 2) and CharInSet(s[1], ['0'..'9','A'..'F']) and CharInSet(s[2], ['0'..'9','A'..'F']) then
    Result := ((Pos(s[1], cs)-1)*16) + (Pos(s[2], cs)-1)
  else
    Result := 0;
    //raise EConvertError.CreateFmt('%s is not a Hexformatstring', [s]);
end;

// -----------------------------------------------------------------------------

function BaseToInt(const Number: String; const Base: Byte): Integer;
const
  B36: PChar = ('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ');

var
  i: Byte;
begin
  Result := 0;

  for i := 1 to Length(Number) do
  begin
    if (Pos(Number[i], B36)-1) < Base then
      Result := Result * Base + (Pos(Number[i], B36)-1)
    else
    begin
      Result := 0;
      Break;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function HexStrToInt(S: String): Integer;
begin
  if (S = '') or (Length(S) < 6) then
    Result := 0
  else
  begin
    if S[1]='#' then Delete(S, 1, 1);

    Result := RGB(BaseToInt(System.Copy(S, 1, 2), 16),
                  BaseToInt(System.Copy(S, 3, 2), 16),
                  BaseToInt(System.Copy(S, 5, 2), 16));
  end;
end;

// -----------------------------------------------------------------------------

function IntToHexStr(const Color: Integer; const Invert: Boolean = False; const Prefix: String = ''): String;
var
  r, g, b: String;
begin
  r := IntToHex(GetRValue(Color), 2);
  g := IntToHex(GetGValue(Color), 2);
  b := IntToHex(GetBValue(Color), 2);

  if not Invert then
    Result := Format('%s%s%s%s', [Prefix, r, g, b])
  else
    Result := Format('%s%s%s%s', [Prefix, b, g, r]);
end;

// -----------------------------------------------------------------------------

function Range(const Value, Min, Max: Integer): Integer;
begin
  if Value < Min then
    Result := Min
  else if Value > Max then
    Result := Max
  else
    Result := Value;
end;

// -----------------------------------------------------------------------------

procedure Constrain(var Value: Single; const MinValue, MaxValue: Single);
begin
  if (MinValue <= MaxValue) then
  begin
    if (Value < MinValue) then
      Value := MinValue
    else if (Value > MaxValue) then
      Value := MaxValue;
  end;
end;

// -----------------------------------------------------------------------------

procedure Constrain(var Value: Integer; const MinValue, MaxValue: Integer);
begin
  if (MinValue <= MaxValue) then
  begin
    if (Value < MinValue) then
      Value := MinValue
    else if (Value > MaxValue) then
      Value := MaxValue;
  end;
end;

// -----------------------------------------------------------------------------

procedure Constrain(var Value: Int64; const MinValue, MaxValue: Int64);
begin
  if (MinValue <= MaxValue) then
  begin
    if (Value < MinValue) then
      Value := MinValue
    else if (Value > MaxValue) then
      Value := MaxValue;
  end;
end;

// -----------------------------------------------------------------------------

procedure Constrain(var Value: Cardinal; const MinValue, MaxValue: Cardinal);
begin
  if (MinValue <= MaxValue) then
  begin
    if (Value < MinValue) then
      Value := MinValue
    else if (Value > MaxValue) then
      Value := MaxValue;
  end;
end;

// -----------------------------------------------------------------------------

function MulDiv(nNumber, nNumerator, nDenominator: Single): Integer;
begin
  if nDenominator = 0 then
    Result := -1
  else
    Result := Round(nNumber * nNumerator / nDenominator);
end;

// -----------------------------------------------------------------------------

function RoundValue(const Value: Single; const Digits: Integer): Single;
begin
  Result := Trunc(Value * Power(10, Digits)) / Power(10, Digits);
end;

// -----------------------------------------------------------------------------

function Rnd(r: Double): LongInt;
begin
  Result := Trunc(r);
  if Abs(Frac(r)) >= 0.5 then
    if r > 0 then
      Result := Result + 1
    else
      Result := Result - 1;
end;

// -----------------------------------------------------------------------------

function LimitDecimals(Num: Real; Limit: Integer): String;
begin
  Result := FloatToStr(Round(Num * Power(10, Limit)) / Power(10, Limit));
end;

// -----------------------------------------------------------------------------

procedure ArrayToTBytes(var DestArray: TBytes; const SourceArray: array of Byte; const StartIndex: Integer);
begin
  SetLength(DestArray, Length(SourceArray));
  Move(SourceArray[0], DestArray[StartIndex], Length(SourceArray));
end;

// -----------------------------------------------------------------------------

procedure TBytesToArray(var DestArray: array of Byte; const SourceArray: TBytes; const StartIndex: Integer);
begin
  Move(SourceArray[0], DestArray[StartIndex], Length(SourceArray));
end;

// -----------------------------------------------------------------------------

function MixColors(C1, C2: TColor; Opacity: Integer = 70): TColor;
var
  R1, R2,
  G1, G2,
  B1, B2: Byte;
  p2: Integer;
begin
  C1 := ColorToRGB(C1);
  C2 := ColorToRGB(C2);
  Opacity := Range(Opacity, 0, 100);
  p2 := 100 - Opacity;
  R1 := GetRValue(C1);
  R2 := GetRValue(C2);
  G1 := GetGValue(C1);
  G2 := GetGValue(C2);
  B1 := GetBValue(C1);
  B2 := GetBValue(C2);

  Result := RGB(Round((R1*Opacity+R2*p2)/100), Round((G1*Opacity+G2*p2)/100), Round((B1*Opacity+B2*p2)/100));
end;

// -----------------------------------------------------------------------------

end.
