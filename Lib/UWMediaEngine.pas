{*
 *  URUWorks Media Engine
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
 *  Copyright (C) 2021-2022 URUWorks, uruworks@gmail.com.
 *
 *  Based on the great work of OvoM3U
 *  Copyright (C) 2020 Marco Caselli.
 *}

unit UWMediaEngine;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}
{$I UWMediaEngine.inc}

interface

uses
  Classes, Controls, SysUtils, LazFileUtils, ExtCtrls
  {$IFDEF USEOPENGL}, OpenGLContext{$ENDIF};

// -----------------------------------------------------------------------------

type

  { TUWMediaEngine Types }

  TUWMediaEngineSate      = (mesStop, mesPlay, mesPause, mesEnd);
  TUWMediaEngineOnEvent   = procedure(Sender: TObject; AParam: Integer = 0) of object;
  TUWMediaEngineTrackType = (trkVideo, trkAudio, trkSubtitle, trkUnknown);

  TUWMediaEngineTrackInfo = record
    Kind     : TUWMediaEngineTrackType;
    ID       : Integer;
    Decoder  : String;
    Info     : String;
    Selected : Boolean;
  end;

  TUWMediaEngineTrackList = array of TUWMediaEngineTrackInfo;

  { TUWMediaEngine }

  TUWMediaEngine = class(TObject)
  private
    FInitialized : Boolean;
    FParent      : TWinControl;
    FState       : TUWMediaEngineSate;
    FErrorCode   : Integer; // Latest error code
    {$IFDEF USETIMER}
    FTimer       : TTimer;
    {$ENDIF}
    {$IFDEF USEOPENGL}
    FOpenGlControl : TOpenGlControl;
    {$ENDIF}
    FFileName    : String;

    FOnStartFile: TNotifyEvent;        // Notification before playback start of a file (before the file is loaded).
    FOnEndFile: TNotifyEvent;          // Notification after playback end (after the file was unloaded).
    FOnFileLoaded: TNotifyEvent;       // Notification when the file has been loaded (headers were read etc.)
    FOnIdle: TNotifyEvent;             // Idle mode was entered.
    FOnVideoReconfig: TNotifyEvent;    // Happens after video changed in some way.
    FOnAudioReconfig: TNotifyEvent;    // Similar to VIDEO_RECONFIG.
    FOnSeek: TUWMediaEngineOnEvent;    // Happens when a seek was initiated.
    FOnPlaybackRestart: TNotifyEvent;  // Usually happens on start of playback and after seeking.

    FOnPlay: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FOnPause: TNotifyEvent;
    FOnTimeChanged: TUWMediaEngineOnEvent;
  protected
    function GetMediaPos: Integer; virtual; abstract;
    procedure SetMediaPos(const AValue: Integer); virtual; abstract;

    procedure LoadTracks; virtual; abstract;

    function DoPlay(const AFileName: String; const APos: Integer = 0): Boolean; virtual; abstract;
    procedure PushEvent(Sender: Pointer); virtual; abstract;
    procedure ReceivedEvent(Sender: TObject); virtual; abstract;

    function GetVolume: Integer; virtual; abstract;
    procedure SetVolume(const AValue: Integer); virtual; abstract;
  public
    TrackList: TUWMediaEngineTrackList;
    class function GetMediaEngineName: String; virtual; abstract;

    procedure Play(const AFileName: String; const APos: Integer = 0); virtual;
    procedure Pause; virtual;
    procedure Resume; virtual; abstract;
    procedure Stop; virtual;
    function Duration: Integer; virtual; abstract;
    procedure Seek(const MSecs: Integer; const SeekAbsolute: Boolean); virtual; abstract;
    procedure NextFrame; virtual; abstract;
    procedure PreviousFrame; virtual; abstract;
    procedure PlaybackRate(const AValue: Byte); virtual; abstract;
    procedure SetTrack(const TrackType: TUWMediaEngineTrackType; const ID: Integer); virtual; abstract;
    function IsPlaying: Boolean; virtual;
    function IsPaused: Boolean; virtual;
    procedure UnInitialize; virtual; abstract;
    function Initialize: Boolean; virtual; abstract;
    {$IFDEF USETIMER}
    procedure DoTimer(Sender: TObject);
    {$ENDIF}
    procedure ShowText(const AText: String); virtual; abstract;
    procedure SetTextColor(const AValue: String); virtual; abstract;
    procedure SetTextPosition(const AValue: String); virtual; abstract;
    procedure SetTextSize(const AValue: Integer); virtual; abstract;

    constructor Create(const AParent: TWinControl); virtual;
    destructor Destroy; override;

    {$IFDEF USEOPENGL}
    property OpenGlControl : TOpenGlControl read FOpenGlControl;
    {$ENDIF}

    property Initialized   : Boolean read FInitialized write FInitialized;
    property State         : TUWMediaEngineSate read FState write FState;
    property Parent        : TWinControl read FParent write FParent;
    property ErrorCode     : Integer read FErrorCode write FErrorCode;
    property Position      : Integer read GetMediaPos;
    property FileName      : String read FFileName write FFileName;
    {$IFDEF USETIMER}
    property Timer         : TTimer read FTimer;
    {$ENDIF}

    property OnStartFile: TNotifyEvent read FOnStartFile write FOnStartFile;
    property OnEndFile: TNotifyEvent read FOnEndFile write FOnEndFile;
    property OnFileLoaded: TNotifyEvent read FOnFileLoaded write FOnFileLoaded;
    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
    property OnVideoReconfig: TNotifyEvent read FOnVideoReconfig write FOnVideoReconfig;
    property OnAudioReconfig: TNotifyEvent read FOnAudioReconfig write FOnAudioReconfig;
    property OnSeek: TUWMediaEngineOnEvent read FOnSeek write FOnSeek;
    property OnPlaybackRestart: TNotifyEvent read FOnPlaybackRestart write FOnPlaybackRestart;

    property OnPlay : TNotifyEvent read FOnPlay  write FOnPlay;
    property OnStop : TNotifyEvent read FOnStop  write FOnStop;
    property OnPause: TNotifyEvent read FOnPause write FOnPause;
    property OnTimeChanged: TUWMediaEngineOnEvent read FOnTimeChanged write FOnTimeChanged;
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{ TUWMediaEngine }

// -----------------------------------------------------------------------------

constructor TUWMediaEngine.Create(const AParent: TWinControl);
begin
  inherited Create;

  {$IFDEF USETIMER}
  FTimer := TTimer.Create(NIL);
  FTimer.Enabled := False;
  FTimer.Interval := 75;
  FTimer.OnTimer := @DoTimer;
  {$ENDIF}

  FParent := AParent;
  FErrorCode := 0;
  FFileName := '';

  {$IFDEF USEOPENGL}
  FOpenGlControl := TOpenGlControl.Create(FParent);
  FOpenGlControl.DoubleBuffered := True;
  {$ENDIF}

  SetLength(TrackList, 0);
  FInitialized := False;
end;

// -----------------------------------------------------------------------------

destructor TUWMediaEngine.Destroy;
begin
  {$IFDEF USETIMER}
  FTimer.Free;
  {$ENDIF}
  {$IFDEF USEOPENGL}
  FOpenGlControl.Free;
  {$ENDIF}
  FParent := NIL;
  FInitialized := False;
  FFileName := '';
  SetLength(TrackList, 0);

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWMediaEngine.Play(const AFileName: String; const APos: Integer = 0);
begin
  FFileName := AFileName;

//  if (AFileName = '') or not FileExistsUTF8(AFileName) then Exit;
  DoPlay(AFileName, APos);
end;

// -----------------------------------------------------------------------------

procedure TUWMediaEngine.Pause;
begin
  {$IFDEF USETIMER}
  FTimer.Enabled := IsPlaying;
  {$ENDIF}
  if IsPlaying then
  begin
    if Assigned(OnPlay) then OnPlay(Self);
  end
  else
    if Assigned(OnPause) then OnPause(Self);
end;

// -----------------------------------------------------------------------------

procedure TUWMediaEngine.Stop;
begin
  {$IFDEF USETIMER}
  FTimer.Enabled := False;
  {$ENDIF}
  if Assigned(OnStop) then OnStop(Self);
end;

// -----------------------------------------------------------------------------

function TUWMediaEngine.IsPlaying: Boolean;
begin
  Result := (FState = mesPlay);
end;

// -----------------------------------------------------------------------------

function TUWMediaEngine.IsPaused: Boolean;
begin
  Result := (FState = mesPause);
end;

// -----------------------------------------------------------------------------

{$IFDEF USETIMER}
procedure TUWMediaEngine.DoTimer(Sender: TObject);
begin
  if Assigned(OnTimeChanged) then OnTimeChanged(Sender, GetMediaPos);
end;
{$ENDIF}

// -----------------------------------------------------------------------------

end.

