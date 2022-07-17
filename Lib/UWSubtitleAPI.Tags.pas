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

unit UWSubtitleAPI.Tags;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils;

{ Tags Functions }

function RemoveSWTags(const Text: String): String;
function SWTagsToHTML(const Text: String): String;
function HTMLTagsToSW(const Text: String): String;
function SWTagsToMicroDVD(const Text: String): String;
function MicroDVDTagsToSW(const Text: String): String;

// -----------------------------------------------------------------------------

implementation

uses UWSystem.StrUtils, RegExpr;

// -----------------------------------------------------------------------------

{ Tags functions }

//------------------------------------------------------------------------------

function RemoveSWTags(const Text: String): String;
begin
  //Result := Text;
  Result := ReplaceRegExpr('\{.*?\}', Text, '', True);
end;

//------------------------------------------------------------------------------

function SWTagsToHTML(const Text: String): String;
begin
  Result := Text;
  if Result = '' then Exit;

  Result := ReplaceRegExpr('\{c&(.*?|var)\&}', Result, '<font color=$1>', True);
  Result := ReplaceRegExpr('\{c\}', Result, '</font>', True);
//  Result := ReplaceRegExpr('\{(.*?|var)\}', Result, '<$1>', True);
  Result := ReplaceString(Result, '{\b1}', '<b>');
  Result := ReplaceString(Result, '{\i1}', '<i>');
  Result := ReplaceString(Result, '{\u1}', '<u>');
  Result := ReplaceString(Result, '{\s1}', '<s>');
  Result := ReplaceString(Result, '{\b0}', '</b>');
  Result := ReplaceString(Result, '{\i0}', '</i>');
  Result := ReplaceString(Result, '{\u0}', '</u>');
  Result := ReplaceString(Result, '{\s0}', '</s>');
end;

// -----------------------------------------------------------------------------

function HTMLTagsToSW(const Text: String): String;
begin
  Result := Text;
  if Result = '' then Exit;
  Result := ReplaceRegExpr('<font color=(.*?|var)>', Result, '{\c&$1&}', True);
  Result := ReplaceRegExpr('</font>', Result, '{\c}', True);
//  Result := ReplaceRegExpr('<(.*?|var)>', Result, '{$1}', True);
  Result := ReplaceString(Result, '<b>', '{\b1}');
  Result := ReplaceString(Result, '<i>', '{\i1}');
  Result := ReplaceString(Result, '<u>', '{\u1}');
  Result := ReplaceString(Result, '<s>', '{\s1}');
  Result := ReplaceString(Result, '</b>', '{\b0}');
  Result := ReplaceString(Result, '</i>', '{\i0}');
  Result := ReplaceString(Result, '</u>', '{\u0}');
  Result := ReplaceString(Result, '</s>', '{\s0}');
end;

// -----------------------------------------------------------------------------

function SWTagsToMicroDVD(const Text: String): String;
begin
  Result := Text;
  Result := ReplaceString(Result, '{\i1}', '{y:i}');
  Result := ReplaceString(Result, '{\i0}', '');
  Result := ReplaceString(Result, '{\b1}', '{y:b}');
  Result := ReplaceString(Result, '{\b0}', '');
  Result := ReplaceString(Result, '{\u1}', '{y:u}');
  Result := ReplaceString(Result, '{\u0}', '');
  Result := ReplaceString(Result, '{\s1}', '{y:s}');
  Result := ReplaceString(Result, '{\s0}', '');
end;

// -----------------------------------------------------------------------------

function MicroDVDTagsToSW(const Text: String): String;
begin
  Result := Text;
  Result := ReplaceString(Result, '{y:i}', '{\i1}');
  Result := ReplaceString(Result, '{y:b}', '{\b1}');
  Result := ReplaceString(Result, '{y:u}', '{\u1}');
  Result := ReplaceString(Result, '{y:s}', '{\s1}');
  Result := ReplaceRegExpr('{c:$(.*?|var)}', Result, '{\c&$1&}', True);
  Result := ReplaceRegExpr('{(.*?)}', Result, '', True);
end;

// -----------------------------------------------------------------------------

end.
