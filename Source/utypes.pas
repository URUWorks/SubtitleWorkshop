{*
 *  URUWorks Subtitle Workshop
 *
 *  Author  : URUWorks
 *  Website : uruworks.net
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
 *  Copyright (C) 2001-2022 Aldo Lacavalla, URUWorks.
 *}

unit UTypes;

// -----------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Graphics, Types, UWSubtitleAPI, UWSubtitleAPI.Formats, UErrors,
  UUndo, UWFiles.MRU, UWSpellCheck.Hunspell, UWCustom.SWStyles, UWTMX;

const

  ProgramName       = 'Subtitle Workshop';
  ProgramWebsite    = 'https://uruworks.net';
  ProgramVer        = 07008; // First 4 digits / 1000 = version, last digit: 0 = Final, >0 = Beta

  DefTimeFormat     = 'hh:mm:ss.zzz';
  DefDurationFormat = 'mm:ss.zzz';
  DefFPS            = 23.976;
  DefFPSList        : array[0..7] of Single = (15, 20, 23.976, 23.978, 24, 25, 29.997, 30);

  swt_StartTag      = '{';
  swt_EndTag        = '}';
  swt_Bold          = 'b';
  swt_Italic        = 'i';
  swt_Underline     = 'u';
  swt_Strikeout     = 's';
  swt_Color         = 'c';

  swt_Sing          = '♪';

  TVideoExts : array[0..19] of String =
  (
    '.avi', '.mp4', '.mpg', '.mpeg', '.mkv', '.webm', '.flv', '.ogv', '.ogg',
    '.mov', '.qt', '.wmv', '.rm', '.rmvb', '.asf', '.m4v', '.m4p', '.mpv',
    '.mpe', '.nsv'
  );

  TAudioExts : array[0..1] of String =
  (
    '.wav', '.peak'
  );

type

  TWorkMode = (wmTime, wmFrames);

  TShowSubtitles = (ssText, ssTranslation);

  TFPS = packed record // used for conversions
    InputFPS : Single;
    FPS      : Single;
  end;

  TColors = packed record // used for mark errors
    Overlapping   : TColor;
    BadValues     : TColor;
    TimeTooShort  : TColor;
    TimeTooLong   : TColor;
    PauseTooShort : TColor;
    Untranslated  : TColor;
  end;

  TMPEText = packed record
    Color    : String;
    Position : String;
    Size     : Integer;
  end;

  TOptions = packed record
    Language             : String;
    AutoCheckErrors      : Boolean;
    ShowErrors           : Boolean;
    ErrOptions           : TSubtitleErrorTypeSet;
    ErrCfg               : TErrCfg; // UErrors
    Colors               : TColors;
    Marquee              : TMPEText;
    DrawTags             : Boolean;
    NewSubtitleMS        : Cardinal;
    DefSubtitlePauseMS   : Cardinal;
    DefChangePlayRate    : Byte;
    ShiftTimeMS          : Cardinal;
    DotsOnSplit          : Boolean;
    TextToFind           : String;
    ShowSubtitles        : TShowSubtitles;
    HunspellLang         : String;
    AutoStartPlaying     : Boolean;
    ShowWelcomeAtStartup : Boolean;
    CustomSearch         : String;
  end;

  TLangStrings = packed record
    Text,
    TextChars,
    TranslationChars,
    Translation,
    CPS,
    LineSelected,
    AllSupportedFiles,
    Selection,
    SaveDialog,
    WriteDenieded,
    ExtractAudio,
    ExtractLibError,
    libMPVError,
    LoadingVideo: String;
  end;

  TErrorStrings = packed record
    Marked,
    BadValues,
    TimeTooLong,
    TimeTooShort,
    PauseTooShort,
    MaxCPS,
    Overlapping,
    FixTags,
    Empty,
    UnnecessarySpaces,
    UnnecessaryDots,
    RepeatedChars,
    ProhibitedChars,
    HearingImpaired,
    BreakLongLines,
    RepeatedSubtitle,
    OCR,
    ExecuteScript,
    CompileScript: String;
  end;

  TLastSubtitle  = packed record
    Selected    : Integer;
    InitialTime : Cardinal;
    Color       : TColor;
    ShowIndex   : Integer; // used in GetSubtitleTextAtTime
  end;

  TSubtitleFileInfo = packed record
    FileName : String;
    Format   : TUWSubtitleFormats;
    Changed  : Boolean;
  end;

  TSubtitleFiles  = packed record
    Text,
    Translation : TSubtitleFileInfo;
  end;

  TAudioExtraction  = packed record
    FileName,
    Params  : String;
  end;

  { TMediaPlayMode }

  TMediaPlayMode = (mpmAll, mpmSelection, mpmFromSelection, mpmBeforeSelection, mpmAfterSelection);

  { VST Proc }

  TUWSubtitleDoLoopProc      = procedure(const Item: PUWSubtitleItem; const Index: Integer);
  TUWSubtitleDoLoopProcCB    = procedure(const CurrentItem, TotalCount: Integer; var Cancel: Boolean);
  TUWSubtitleDoLoopSelection = (dlAll, dlSelected, dlCurrentToLast, dlMarked);

var

  Subtitles       : TUWSubtitles;       // SubtitleAPI
  WorkMode        : TWorkMode = wmTime;
  TranslatorMode  : Boolean = True;
  VideoPreview    : Boolean = True;
  AudioPreview    : Boolean = True;
  TextSize        : TSize;              // used in VST DrawText
  FormatSettings  : TFormatSettings;
  FPS             : TFPS;
  Options         : TOptions;
  Strings         : TLangStrings;
  ErrorStrings    : TErrorStrings;
  LastSubtitle    : TLastSubtitle;
  SubtitleFile    : TSubtitleFiles;
  MediaPlayMode   : TMediaPlayMode = mpmAll;
  AudioExtraction : TAudioExtraction;
  Undo            : TUndo;
  LastUndoGroup   : Byte = 0;
  MRU             : TUWMRU;
  Hunspell        : TUWHunspell;
  Styles          : TSWStyles;
  TMX             : TUWTMX;

const
  CustomSearchWordReference = 'www.wordreference.com/redirect/translation.aspx?w=%s&dict=enen';

  {$IFDEF MSWINDOWS}
  VLC_EXE        = 'vlc.exe';
  VLC_Params     = '"%s" -I dummy --no-sout-video --sout-audio --audio-track=%d --no-sout-rtp-sap --no-sout-standard-sap --ttl=1 --sout-keep --sout "#transcode{acodec=s16l,channels=1}:std{access=file,mux=wav,dst=%s}" vlc://quit';
  FFMPEG_EXE     = 'ffmpeg.exe';
  FFMPEG_Params  = '-i "%s" -vn -ac 1 -ar 44100 -map 0:a:%d -acodec pcm_s16le "%s"';
  MPLAYER_EXE    = 'mplayer.exe';
  MPLAYER_Params = '"%s" -ao pcm:fast:file="%s" -vo null -vc null';
  {$ENDIF}
  {$IFDEF LINUX}
  VLC_EXE        = 'vlc';
  VLC_Params     = '%s -I dummy --no-sout-video --sout-audio --audio-track=%d --no-sout-rtp-sap --no-sout-standard-sap --ttl=1 --sout-keep --sout "#transcode{acodec=s16l,channels=1}:std{access=file,mux=wav,dst=%s}" vlc://quit';
  FFMPEG_EXE     = 'ffmpeg';
  FFMPEG_Params  = '-i %s -vn -ac 1 -ar 44100 -map 0:a:%d -acodec pcm_s16le %s';
  MPLAYER_EXE    = 'mplayer';
  MPLAYER_Params = '%s -ao pcm:fast:file=%s -vo null -vc null';
  {$ENDIF}
  {$IFDEF DARWIN}
  VLC_EXE        = 'VLC'; // '/Applications/VLC.app/Contents/MacOS';
  VLC_Params     = '%s -I dummy --no-sout-video --sout-audio --audio-track=%d --no-sout-rtp-sap --no-sout-standard-sap --ttl=1 --sout-keep --sout "#transcode{acodec=s16l,channels=1}:std{access=file,mux=wav,dst=%s}" vlc://quit';
  FFMPEG_EXE     = 'ffmpeg';
  FFMPEG_Params  = '-i %s -vn -ac 1 -ar 44100 -map 0:a:%d -acodec pcm_s16le %s';
  MPLAYER_EXE    = 'mplayer';
  MPLAYER_Params = '%s -ao pcm:fast:file=%s -vo null -vc null';
  {$ENDIF}

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

end.

