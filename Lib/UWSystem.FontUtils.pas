{*
 *  URUWorks Lazarus FontUtils
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

unit UWSystem.FontUtils;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils
  {$IFDEF MSWINDOWS}, Windows{$ENDIF};
//  {$IFDEF MACOS}, Macapi.Foundation, Macapi.AppKit{$ENDIF};

// -----------------------------------------------------------------------------

{$IFDEF MSWINDOWS}procedure GetSystemFonts(FontList: TStrings);{$ENDIF}

// -----------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function EnumFontsList(var LogFont: ENUMLOGFONTEX; var TextMetric: NEWTEXTMETRICEX;
  FontType: longint; Data: LPARAM): longint; stdcall;
var
  List  : TStrings;
  fName : String;
begin
  List  := TStrings(Data);  ;
  fName := LogFont.elfFullName;
  if (List.Count = 0) or (AnsiCompareText(List[List.Count-1], fName) <> 0) then
    List.Add(fName);

  Result := 1;
end;

// -----------------------------------------------------------------------------

procedure GetSystemFonts(FontList: TStrings);
var
  dContext : HDC;
  LFont    : TLogFont;
begin
  FontList.BeginUpdate;
  try
    dContext := GetDC(0);
    FillChar(LFont, SizeOf(LFont), 0);
    LFont.lfCharset := DEFAULT_CHARSET;
    EnumFontFamiliesEx(dContext, LFont, @EnumFontsList, Windows.LPARAM(FontList), 0);
    ReleaseDC(0, dContext);
  finally
    FontList.EndUpdate;
  end;
end;
{$ENDIF}//{$ELSE}
// -----------------------------------------------------------------------------

{procedure GetSystemFonts(FontList: TStrings);
var
  fManager : NSFontManager;
  list     : NSArray;
  lItem    : NSString;
  i        : Integer;
begin
  Screen.
  fManager := TNSFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);
  list     := fManager.availableFontFamilies;
  if (List <> NIL) and (List.count > 0) then
  begin
    FontList.BeginUpdate;
    try
      for i := 0 to List.Count-1 do
      begin
        lItem := TNSString.Wrap(List.objectAtIndex(i));
        FontList.Add(String(lItem.UTF8String))
      end;
    finally
      FontList.EndUpdate;
    end;
  end;
end;
{$ENDIF}
}
// -----------------------------------------------------------------------------

end.
