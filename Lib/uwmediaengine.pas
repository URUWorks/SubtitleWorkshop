{*
 *  URUWorks Media Engine
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
 *  Copyright (C) 2021-2022 URUWorks, uruworks@gmail.com.
 *}

unit UWMediaEngine;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}
{$DEFINE USETIMER}

interface

uses
  Classes, Controls, SysUtils, LazFileUtils, ExtCtrls;

// -----------------------------------------------------------------------------

type

  { TUWMediaEngine Types }

  TUWMediaEngineSate      = (mesStop, mesPlay, mesPause, mesEnd);
  TUWMediaEngineCommand   = (mecInvalid, mecLoading, mecLoaded, mecEnded, mecStop, mecPlay, mecPause, mecSeek, mecTimeChanged, mecTracks, mecUser);
  TUWMediaEngineOnCommand = procedure(Sender: TObject; ACommand: TUWMediaEngineCommand; AParam: Integer = 0) of object;
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
    FOnCommand   : TUWMediaEngineOnCommand;
    FFileName    : String;
  protected
    function GetMediaPos: Integer; virtual; abstract;
    procedure SetMediaPos(const AValue: Integer); virtual; abstract;

    procedure LoadTracks; virtual; abstract;

    function DoPlay(const AFileName: String; const APos: Integer = 0): Boolean; virtual; abstract;
    procedure PostCommand(const ACommand: TUWMediaEngineCommand; const AParam: Integer = 0); virtual; abstract;
    procedure ReceivedCommand(Sender: TObject; ACommand: TUWMediaEngineCommand; AParam: Integer = 0); virtual;

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

    constructor Create(const AParent: TWinControl; AOnCommand: TUWMediaEngineOnCommand); virtual;
    destructor Destroy; override;

    property Initialized   : Boolean read FInitialized write FInitialized;
    property State         : TUWMediaEngineSate read FState write FState;
    property Parent        : TWinControl read FParent write FParent;
    property ErrorCode     : Integer read FErrorCode write FErrorCode;
    property Position      : Integer read GetMediaPos;
    property FileName      : String read FFileName write FFileName;
    {$IFDEF USETIMER}
    property Timer         : TTimer read FTimer;
    {$ENDIF}
    property OnCommand     : TUWMediaEngineOnCommand read FOnCommand  write FOnCommand;
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{ TUWMediaEngine }

// -----------------------------------------------------------------------------

constructor TUWMediaEngine.Create(const AParent: TWinControl; AOnCommand: TUWMediaEngineOnCommand);
begin
  inherited Create;

  FOnCommand := AOnCommand;

  {$IFDEF USETIMER}
  FTimer := TTimer.Create(NIL);
  FTimer.Enabled := False;
  FTimer.Interval := 75;
  FTimer.OnTimer := @DoTimer;
  {$ENDIF}

  FParent := AParent;
  FErrorCode := 0;
  FFileName := '';

  SetLength(TrackList, 0);
  FInitialized := False;
end;

// -----------------------------------------------------------------------------

destructor TUWMediaEngine.Destroy;
begin
  {$IFDEF USETIMER}
  FTimer.Free;
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
  if Assigned(FOnCommand) then
    if IsPlaying then
      FOnCommand(Self, mecPlay, 0)
    else
      FOnCommand(Self, mecPause, 0);
end;

// -----------------------------------------------------------------------------

procedure TUWMediaEngine.Stop;
begin
  {$IFDEF USETIMER}
  FTimer.Enabled := False;
  {$ENDIF}
  if Assigned(FOnCommand) then FOnCommand(Self, mecStop, 0);
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

procedure TUWMediaEngine.ReceivedCommand(Sender: TObject; ACommand: TUWMediaEngineCommand; AParam: Integer = 0);
begin
  if Assigned(FOnCommand) then FOnCommand(Sender, ACommand, AParam);
end;

// -----------------------------------------------------------------------------

{$IFDEF USETIMER}
procedure TUWMediaEngine.DoTimer(Sender: TObject);
begin
  if Assigned(FOnCommand) then FOnCommand(Sender, mecTimeChanged, GetMediaPos);
end;
{$ENDIF}

// -----------------------------------------------------------------------------

end.

