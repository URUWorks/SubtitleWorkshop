{*
 *  URUWorks HTMLUtils
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

unit UWSystem.HTMLUtils;

// -----------------------------------------------------------------------------

interface

uses Classes, SysUtils, Graphics, LCLIntf, LazUTF8, UWSubtitles.Utils;

procedure DrawHTMLTextEx(const ACanvas: TCanvas; const ARect: TRect; Text: String;
  const RTL: Boolean = False; const startTag: String = '<'; endTag: String = '>';
  boldTag: String = 'B'; italicTag: String = 'I'; underlineTag: String = 'U';
  strikeoutTag: String = 'S'; colorTag: String = 'C';
  const Enter: String = sLineBreak);

procedure DrawASSText(const ACanvas: TCanvas; const ARect: TRect; Text: String; const RTL: Boolean = False);

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

function HTMLHex2Color(Value: String): TColor;
var
  HexValue: String;
begin
  Result := 0;

  if Value <> '' then
  begin
    if Value[1] = '#' then
    begin
      HexValue := Copy(Value, 2, 6);

      Result := RGB(StrToInt('$'+Copy(HexValue, 1, 2)),
                    StrToInt('$'+Copy(HexValue, 3, 2)),
                    StrToInt('$'+Copy(HexValue, 5, 2)));
    end
    else
      Result := StrToIntDef(Value, 0);
  end;
end;

// -----------------------------------------------------------------------------

function BGRHex2Color(Value: String): TColor;
begin
  Result := 0;

  if Value <> '' then
    Result := LCLIntf.RGB(StrToInt('$'+Copy(Value, 5, 2)),
                  StrToInt('$'+Copy(Value, 3, 2)),
                  StrToInt('$'+Copy(Value, 1, 2)));
end;

// -----------------------------------------------------------------------------

procedure DrawHTMLTextEx(const ACanvas: TCanvas; const ARect: TRect; Text: String;
  const RTL: Boolean = False; const startTag: String = '<'; endTag: String = '>'; boldTag: String = 'B';
  italicTag: String = 'I'; underlineTag: String = 'U'; strikeoutTag: String = 'S';
  colorTag: String = 'C'; const Enter: String = sLineBreak);

  function CloseTag(const ATag: String): String;
  begin
    Result := Concat('/', ATag);
  end;

  function GetTagValue(const ATag: String): String;
  var
    p: Integer;
  begin
    p := Pos(':', ATag);

    if p = 0 then
      Result := ''
    else
      Result := Copy(ATag, p + 1, MaxInt);
  end;

var
  x, y, idx,
  CharWidth,
  MaxCharHeight  : Integer;
  CurrChar       : String;
  Tag, TagValue  : String;
  PrevFontColour : TColor;
  NeedBreak      : Boolean;
begin
    if RTL then Text := ReverseText(Text, Enter);

    NeedBreak        := False;
    PrevFontColour   := ACanvas.Font.Color;
    x                := ARect.Left;
    y                := ARect.Top;
    idx              := 1;

    MaxCharHeight := ACanvas.TextHeight('Aj');

    while idx <= UTF8Length(Text) do
    begin
      CurrChar := UTF8Copy(Text, idx, 1);
      // Is this a start tag?
      if UTF8CompareStr(CurrChar, startTag) = 0 then
      begin
        Tag := '';
        Inc(idx);

        // Find the end of the tag
        while (UTF8CompareStr(UTF8Copy(Text, idx, 1), endTag) <> 0) and (idx <= UTF8Length(Text)) do
        begin
          Tag := Concat(Tag, UTF8UpperCase((UTF8Copy(Text, idx, 1))));
          Inc(idx);
        end;
        // Simple tags
        if Tag = boldTag then
          ACanvas.Font.Style := ACanvas.Font.Style + [TFontStyle.fsBold]
        else if Tag = italicTag then
          ACanvas.Font.Style := ACanvas.Font.Style + [TFontStyle.fsItalic]
        else if Tag = underlineTag then
          ACanvas.Font.Style := ACanvas.Font.Style + [TFontStyle.fsUnderline]
        else if Tag = strikeoutTag then
          ACanvas.Font.Style := ACanvas.Font.Style + [TFontStyle.fsStrikeout]
        else
        // Closing tags
        if Tag = CloseTag(boldTag) then
          ACanvas.Font.Style := ACanvas.Font.Style - [TFontStyle.fsBold]
        else if Tag = CloseTag(italicTag) then
          ACanvas.Font.Style := ACanvas.Font.Style - [TFontStyle.fsItalic]
        else if Tag = CloseTag(UnderlineTag) then
          ACanvas.Font.Style := ACanvas.Font.Style - [TFontStyle.fsUnderline]
        else if Tag = CloseTag(strikeoutTag) then
          ACanvas.Font.Style := ACanvas.Font.Style - [TFontStyle.fsStrikeout]
        else if Tag = CloseTag(colorTag) then
          ACanvas.Font.Color := PrevFontColour
        else
        // Tags with values
        begin
          // Get the tag value (everything after ':')
          TagValue := GetTagValue(Tag);
          if TagValue <> '' then
          begin
            // Remove the value from the tag
            Tag := Copy(Tag, 1, Pos(':', Tag) - 1);
            if Tag = colorTag then
            begin
              PrevFontColour := ACanvas.Font.Color;
              try
                ACanvas.Font.Color := HTMLHex2Color(TagValue);
              except
              end;
            end;
          end;
        end;
      end
      else
      // Enter char?
      if (CurrChar = Enter) or (CurrChar = #13) then
      begin
        Inc(y, MaxCharHeight);
        x := ARect.Left;
      end
      // Draw the character if it's not a ctrl char
      else if (CurrChar >= #32) then
      begin
        CharWidth := ACanvas.TextWidth(CurrChar);

        if (x + CharWidth > ARect.Right) then // too long line!!
          NeedBreak := True;

        if not NeedBreak or (y + MaxCharHeight < ARect.Bottom) then
        begin
          ACanvas.Brush.Style := bsClear;
          ACanvas.TextOut(x, y, CurrChar);
        end;

        x := x + CharWidth;
        NeedBreak := False;
      end;
      Inc(idx);
    end;
end;

// -----------------------------------------------------------------------------

procedure DrawASSText(const ACanvas: TCanvas; const ARect: TRect; Text: String; const RTL: Boolean = False);

  function CloseTag(const ATag: String): String;
  begin
    Result := Concat('0', ATag);
  end;

  function GetTagValue(const ATag: String; out TagID: String): String;
  var
    p, px: Integer;
  begin
    Result := '';
    TagID  := '';

    // color
    p := Pos('&', ATag);
    px := Pos('&', ATag, p+1);
    if (p > 0) and (px > 0) then
    begin
      TagID := 'c';
      Result := Copy(ATag, p + 1, px - 1);
      Exit;
    end;
    // font name
    p := Pos('fn', ATag);
    if (p > 0) then
    begin
      TagID := 'fn';
      Result := Copy(ATag, p + 2, MaxInt);
      Exit;
    end;
    // font size
    p := Pos('fs', ATag);
    if (p > 0) then
    begin
      TagID := 'fs';
      Result := Copy(ATag, p + 2, MaxInt);
      Exit;
    end;
    // font encoding
    p := Pos('fe', ATag);
    if (p > 0) then
    begin
      TagID := 'fe';
      Result := Copy(ATag, p + 2, MaxInt);
      Exit;
    end;
  end;

const
  startTag: String = '{'; endTag: String = '}';
  boldTag: String = 'b'; italicTag: String = 'i';
  underlineTag: String = 'u'; strikeoutTag: String = 's';
  colorTag: String = 'c';

var
  x, y, idx,
  CharWidth,
  MaxCharHeight   : Integer;
  CurrChar        : String;
  Tag, TagValue   : String;
  TagTmp, TagTmpX : String;
  TagID           : String;
  PrevFontColour  : TColor;
  NeedBreak       : Boolean;
begin
    if RTL then Text := ReverseText(Text);

    NeedBreak        := False;
    PrevFontColour   := ACanvas.Font.Color;
    x                := ARect.Left;
    y                := ARect.Top;
    idx              := 1;

    MaxCharHeight := ACanvas.TextHeight('Aj');

    while idx <= UTF8Length(Text) do
    begin
      CurrChar := UTF8Copy(Text, idx, 1);
      // Is this a start tag?
      if UTF8CompareStr(CurrChar, startTag) = 0 then
      begin
        Tag := '';
        Inc(idx);

        // Find the end of the tag
        while (UTF8CompareStr(UTF8Copy(Text, idx, 1), endTag) <> 0) and (idx <= UTF8Length(Text)) do
        begin
          Tag := Concat(Tag, (UTF8Copy(Text, idx, 1)));
          Inc(idx);
        end;
        // Simple tags
        if UTF8Copy(Tag, 1, 1) = '\' then
        begin
          TagTmp  := UTF8Copy(Tag, 2, 1);
          TagTmpX := UTF8RightStr(Tag, 1);
          if Length(Tag) = 3 then
          begin
            // Starting tags
            if TagTmpX = '1' then
            begin
              if TagTmp = boldTag then
                ACanvas.Font.Style := ACanvas.Font.Style + [TFontStyle.fsBold]
              else if TagTmp = italicTag then
                ACanvas.Font.Style := ACanvas.Font.Style + [TFontStyle.fsItalic]
              else if TagTmp = underlineTag then
                ACanvas.Font.Style := ACanvas.Font.Style + [TFontStyle.fsUnderline]
              else if TagTmp = strikeoutTag then
                ACanvas.Font.Style := ACanvas.Font.Style + [TFontStyle.fsStrikeout]
            end;
            // Closing tags
            if TagTmpX = '0' then
            begin
              if TagTmp = boldTag then
                ACanvas.Font.Style := ACanvas.Font.Style - [TFontStyle.fsBold]
              else if TagTmp = italicTag then
                ACanvas.Font.Style := ACanvas.Font.Style - [TFontStyle.fsItalic]
              else if TagTmp = UnderlineTag then
                ACanvas.Font.Style := ACanvas.Font.Style - [TFontStyle.fsUnderline]
              else if TagTmp = strikeoutTag then
                ACanvas.Font.Style := ACanvas.Font.Style - [TFontStyle.fsStrikeout]
            end;
          end
          else
          // Tags with values
          begin
            if TagTmp = colorTag then
              ACanvas.Font.Color := PrevFontColour;

            // Get the tag value (everything after ':')
            TagValue := GetTagValue(Tag, TagID);
            if TagValue <> '' then
            begin
              if TagID = colorTag then
              begin
                PrevFontColour := ACanvas.Font.Color;
                try
                  ACanvas.Font.Color := BGRHex2Color(TagValue);
                except
                end;
              end;
            end;
          end;
        end;
      end
      else
      // Enter char?
      if (CurrChar = sLineBreak) or (CurrChar = #13) then
      begin
        Inc(y, MaxCharHeight);
        x := ARect.Left;
      end
      // Draw the character if it's not a ctrl char
      else if (CurrChar >= #32) then
      begin
        CharWidth := ACanvas.TextWidth(CurrChar);

        if (x + CharWidth > ARect.Right) then // too long line!!
          NeedBreak := True;

        if not NeedBreak and (y < ARect.Bottom) then
        begin
          ACanvas.Brush.Style := bsClear;
          ACanvas.TextOut(x, y, CurrChar);
        end;

        x := x + CharWidth;
        NeedBreak := False;
      end;
      Inc(idx);
    end;
end;

// -----------------------------------------------------------------------------

end.
