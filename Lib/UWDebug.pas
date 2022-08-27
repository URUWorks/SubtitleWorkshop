{*
 *  URUWorks DEBUG
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

unit UWDebug;

// -----------------------------------------------------------------------------

interface

uses LazLogger, SysUtils;

procedure DebugMsg(const AMsg: String);

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

procedure DebugMsg(const AMsg: String);
begin
  DebugLn(TimeToStr(Time) + '.' + AMsg);
end;

// -----------------------------------------------------------------------------

end.
