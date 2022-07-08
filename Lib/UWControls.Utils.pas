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

unit UWControls.Utils;

// -----------------------------------------------------------------------------

interface

uses
  StdCtrls, SysUtils, LazUTF8, character;

// TMemo
function Memo_GetWordUnderCaret(const Memo: TMemo; const SelectWord: Boolean = False): String;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

function Memo_GetWordUnderCaret(const Memo: TMemo; const SelectWord: Boolean = False): String;
var
   Line     : Integer;
   Pos, i   : Integer;
   LineText : String;
   InitPos  : Integer;
   EndPos   : Integer;
begin
  if Memo.SelText <> '' then
    Result := Memo.SelText
  else
  begin
     //Get the caret position
     Line := Memo.CaretPos.y;
     Pos  := Memo.CaretPos.x;
     //Validate the line number
     if Memo.Lines.Count-1 < Line then Exit;
     //Get the text of the line
     LineText := Memo.Lines[Line];
     //Inc(Pos);
     InitPos := Pos;
     //search the initial position using the space symbol as separator
     while (InitPos > 0) and TCharacter.IsLetter(LineText, InitPos) do Dec(InitPos);
     Inc(Pos);
     EndPos := Pos;
     //search the final position using the space symbol as separator
     while (EndPos <= UTF8Length(LineText)) and TCharacter.IsLetter(LineText, EndPos) do Inc(EndPos);
     //Get the text
     Inc(InitPos);
     Result := UTF8Copy(LineText, InitPos, EndPos - InitPos);
     Dec(InitPos);
     //Finally select the text in the Memo if needed
     if SelectWord then
     begin
       if Line > 0 then
         for i := 0 to Line-1 do InitPos := InitPos + UTF8Length(Memo.Lines[i]) + UTF8Length(sLineBreak);

       Memo.SelStart  := InitPos;
       Memo.SelLength := UTF8Length(Result);
     end;
  end;
end;

// -----------------------------------------------------------------------------

end.
