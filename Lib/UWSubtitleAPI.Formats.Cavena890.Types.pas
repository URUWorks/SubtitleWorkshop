{*
 *  URUWorks Cavena 890 Type
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

unit UWSubtitleAPI.Formats.Cavena890.Types;

//------------------------------------------------------------------------------

interface

uses
  StrUtils;

type

  { THeaderBlock }

  THeaderBlock = record
    UnknowCodes1      : array[0..1] of Byte;      // $00$00
    TapeNumber        : array[0..37] of AnsiChar; // end with $00$00
    TranslatedTitle   : array[0..27] of AnsiChar;
    Translator        : array[0..36] of AnsiChar;
    TranslatedEpisode : array[0..32] of AnsiChar;
    UnknowCodes2      : array[0..9] of Byte;
    Comments          : array[0..38] of AnsiChar;
    PrimaryFont       : array[0..6] of AnsiChar;
    PrimaryLanguage   : Byte;
    UnknowCodes3      : array[0..22] of Byte;
    OriginalTitle     : array[0..27] of AnsiChar;
    SecondaryFont     : array[0..6] of AnsiChar;
    SecondaryLanguage : Byte;
    UnknowCodes4      : array[0..1] of Byte;
    StartTime         : array[0..10] of AnsiChar; // 00:00:00:00
    UnknowCodes5      : array[0..25] of Byte;
    Producer          : array[0..37] of AnsiChar;
    EpisodeTitle      : array[0..56] of AnsiChar;
//    UnknowCodes6      : array[0..14] of Byte;
  end;

//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

end.
