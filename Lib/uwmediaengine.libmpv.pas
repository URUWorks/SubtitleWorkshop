{*
 *  URUWorks Media Engine libMPV
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
 *
 *}

unit UWMediaEngine.libMPV;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}
{$DEFINE USETIMER}

interface

uses
  Classes, Controls, SysUtils, UWMediaEngine, UWMediaEngine.Thread,
  UWImport.libmpv, UWSystem.TimeUtils, fpjson, jsonparser
  {$IFDEF LINUX}
  , gtk2, gdk2x
  {$ENDIF};

// -----------------------------------------------------------------------------

type
  { TUWMediaEngineLibMPV }
  TUWLibMPV = class(TUWMediaEngine)
  private
    FHandle: Pmpv_handle;
    FText: String;
    function GetBoolProperty(const APropertyName: String): Boolean;
    procedure SetBoolProperty(const APropertyName:string; const AValue: Boolean);
    procedure SetStringProperty(const APropertyName, AValue: String);
  protected
    FThread: TUWMediaEngineThread;
    function GetVolume: Integer; override;
    procedure SetVolume(const AValue: Integer); override;
    function GetMediaPos: Integer; override;
    procedure SetMediaPos(const AValue: Integer); override;
    procedure LoadTracks; override;
    function DoPlay(const AFileName: String; const APos: Integer = 0): Boolean; override;
  public
    class function GetMediaEngineName: String; override;
    procedure PostCommand(const ACommand: TUWMediaEngineCommand; const AParam: Integer = 0); override;
    procedure ReceivedCommand(Sender: TObject; ACommand: TUWMediaEngineCommand; AParam: Integer = 0); override;
    constructor Create(const AParent: TWinControl; AOnCommand: TUWMediaEngineOnCommand); override;
    destructor Destroy; override;
    procedure UnInitialize; override;
    function Initialize: Boolean; override;
    function Duration: Integer; override;
    procedure Pause; override;
    procedure Resume; override;
    procedure Seek(const MSecs: Integer; const SeekAbsolute: Boolean); override;
    procedure Stop; override;
    procedure NextFrame; override;
    procedure PreviousFrame; override;
    procedure PlaybackRate(const AValue: Byte); override;
    procedure SetTrack(const TrackType: TUWMediaEngineTrackType; const ID: Integer); override; overload;
    procedure SetTrack(const Index: Integer); overload;
    procedure ShowText(const AText: String); override;
    procedure SetTextColor(const AValue: String); override;
    procedure SetTextPosition(const AValue: String); override;
    procedure SetTextSize(const AValue: Integer); override;
  end;

// -----------------------------------------------------------------------------

implementation

const
   MPVMAXVOLUME = 100;

// -----------------------------------------------------------------------------

procedure LibMPVEvent(Data: Pointer); cdecl;
begin
  if (Data = NIL) then
    Exit
  else
    TUWLibMPV(Data).PostCommand(mecUser, 1);
end;

// -----------------------------------------------------------------------------

constructor TUWLibMPV.Create(const AParent: TWinControl; AOnCommand: TUWMediaEngineOnCommand);
begin
  inherited Create(AParent, AOnCommand);

  ErrorCode := Load_libMPV; //(UWImport.libmpv.External_library);
end;

// -----------------------------------------------------------------------------

destructor TUWLibMPV.Destroy;
begin
  UnInitialize;
  Free_libMPV;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

class function TUWLibMPV.GetMediaEngineName: String;
var
  ver: DWord;
begin
  Result := 'libmpv';
  if Assigned(mpv_client_api_version) then
  begin
    ver := mpv_client_api_version();
    Result := Format('%s %d.%d', [Result, ver shr 16, ver and $FF]);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.UnInitialize;
begin
  FText := '';
  if Assigned(mpv_set_wakeup_callback) and Assigned(FHandle) then mpv_set_wakeup_callback(FHandle^, NIL, Self);
  if Assigned(FThread) then FThread.Free;
  if Assigned(mpv_terminate_destroy) and Assigned(FHandle) then mpv_terminate_destroy(FHandle^);
end;

// -----------------------------------------------------------------------------

function TUWLibMPV.Initialize: Boolean;
var
  hwnd: {$IFDEF WINDOWS}THandle;{$ELSE}PtrUInt;{$ENDIF}
begin
  Result := False;
  FText  := '';

  if not Assigned(mpv_create) then Exit;
  FHandle := mpv_create();
  if not Assigned(FHandle) then Exit;

  //mpv_set_option_string(FHandle^, 'hwdec', 'auto'); // enable best hw decoder
  mpv_set_option_string(FHandle^, 'keep-open', 'always'); // don't auto close video
  mpv_set_option_string(FHandle^, 'sub', 'no'); // don't load subtitles

  mpv_set_option_string(FHandle^, 'pause', ''); // Start the player in paused state.

  {$IFDEF LINUX}
  hwnd := GDK_WINDOW_XWINDOW(PGtkWidget(PtrUInt(Parent.Handle))^.window);
  {$ELSE}
  hwnd := Parent.Handle;
  {$ENDIF}
  mpv_set_option(FHandle^, 'wid', MPV_FORMAT_INT64, @hwnd); // window parent

  //  mpv_set_option_string(FHandle^, 'osd-color', '#FF0000');
  mpv_set_option_string(FHandle^, 'osd-duration', '10000');
  mpv_set_option_string(FHandle^, 'osd-align-x', 'center');
  mpv_set_option_string(FHandle^, 'osd-align-y', 'bottom');
  //mpv_set_option_string(FHandle^, 'log-file', 'zmpv.log');

  {$IFNDEF USETIMER}
  mpv_observe_property(FHandle^, 0, 'playback-time', MPV_FORMAT_INT64);
  {$ENDIF}

  mpv_initialize(FHandle^);
  mpv_request_log_messages(FHandle^, 'no');

  FThread := TUWMediaEngineThread.Create;
  FThread.OnCommand := @ReceivedCommand;
  mpv_set_wakeup_callback(FHandle^, @LibMPVEvent, Self);

  Initialized := True;
  Result := Initialized;
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.LoadTracks;

  function jKeyExists(jData: TJSONObject; jKey: String): Boolean;
  begin
    try
      if not jData[jKey].IsNull then Result := True;
    except
      Result := False;
    end;
  end;

var
  sList: String;
  Data: TJSONArray;
  DataArrayItem: TJSONObject;
  i: Integer;
  sKind: String;
begin
  sList := mpv_get_property_string(FHandle^, 'track-list');
  Data := TJSONArray(GetJSON(sList));
  SetLength(TrackList, Data.Count);
  for i := 0 to Data.Count-1 do
  begin
    DataArrayItem := Data.Objects[i];

    TrackList[i].ID := DataArrayItem['id'].AsInteger;
    sKind :=  DataArrayItem['type'].AsString;
    TrackList[i].Selected := False;

    if sKind = 'audio' then
      TrackList[i].Kind := trkAudio
    else if sKind = 'video' then
      TrackList[i].Kind := trkVideo
    else if sKind = 'sub' then
      TrackList[i].Kind := trkSubtitle;

    if (TrackList[i].Kind = trkAudio) or (TrackList[i].Kind = trkVideo) then
    begin
      if jKeyExists(DataArrayItem, 'lang') then
        TrackList[i].Decoder := DataArrayItem['lang'].AsString
      else if jKeyExists(DataArrayItem, 'decoder-desc') then
        TrackList[i].Decoder := DataArrayItem['decoder-desc'].AsString
      else
        TrackList[i].Decoder := '';
    end;

    if (TrackList[i].Kind = trkAudio) then
    begin
      TrackList[i].Info := DataArrayItem['demux-channels'].AsString + ', ' + DataArrayItem['demux-samplerate'].AsString;
      TrackList[i].Selected := DataArrayItem['selected'].AsBoolean;
    end;

    if (TrackList[i].Kind = trkVideo) then
      TrackList[i].Info := DataArrayItem['demux-w'].AsString + 'x' + DataArrayItem['demux-h'].AsString + ' @ ' + DataArrayItem['demux-fps'].AsString;
  end;
end;

// -----------------------------------------------------------------------------

function TUWLibMPV.DoPlay(const AFileName: String; const APos: Integer = 0): Boolean;
var
  args: array of PChar;
begin
  if not Initialized then Exit;

  SetLength(args, 2);
  args[0]   := 'loadfile';
  args[1]   := PChar(AFileName);
//  args[2]   := 'start';
//  args[3]   := PChar(TimeToString(APos, 'hh:mm:ss.zzz'));
  args[2]   := NIL;
  ErrorCode := mpv_command(FHandle^, PPChar(@args[0]));

  Result := (ErrorCode = MPV_ERROR_SUCCESS);
  if Result and (APos <> 0) then
  begin
    Sleep(250);
    Seek(APos, True);
  end;
end;

// -----------------------------------------------------------------------------

function TUWLibMPV.Duration: Integer;
var
  dur: Double;
begin
  Result := 0;
  if not Initialized then Exit;

  Result := mpv_get_property(FHandle^, 'duration', MPV_FORMAT_DOUBLE, @dur);

  if Result = MPV_ERROR_SUCCESS then
    Result := Trunc(dur * 1000)
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function TUWLibMPV.GetMediaPos: Integer;
var
  pos: Double;
begin
  Result := 0;
  if not Initialized then Exit;

  Result := mpv_get_property(FHandle^, 'time-pos', MPV_FORMAT_DOUBLE, @pos);

  if Result = MPV_ERROR_SUCCESS then
    Result := Trunc(pos * 1000)
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.SetMediaPos(const AValue: Integer);
var
  pos: Double;
begin
  if not Initialized then Exit;

  pos := AValue / 1000;
  mpv_set_property(FHandle^, 'time-pos', MPV_FORMAT_DOUBLE, @pos);
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.PostCommand(const ACommand: TUWMediaEngineCommand; const AParam: Integer = 0);
begin
  FThread.SendCommand(ACommand, AParam);
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.ReceivedCommand(Sender: TObject; ACommand: TUWMediaEngineCommand; AParam: Integer = 0);
var
  Event: Pmpv_event;
begin
  if (ACommand = mecUser) and (AParam = 1) then
  begin
    Event := mpv_wait_event(FHandle^, 0);
    while Event^.event_id <> MPV_EVENT_NONE do
    begin
      case (Event^.event_id) of
        MPV_EVENT_START_FILE: begin
                                ReceivedCommand(Self, mecLoading, 0);
                                //Break;
                              end;

        MPV_EVENT_FILE_LOADED: begin
                                 LoadTracks;
                                 State := mesPause;
                                 {$IFDEF USETIMER}
                                 Timer.Enabled := True;
                                 {$ENDIF}
                                 ReceivedCommand(Self, mecLoaded, 0);
                                 //Break;
                               end;

        MPV_EVENT_SEEK: begin
                          ReceivedCommand(Self, mecSeek, GetMediaPos);
                          //Break;
                        end;

        MPV_EVENT_END_FILE: begin
                              State := mesEnd;
                              {$IFDEF USETIMER}
                              Timer.Enabled := False;
                              {$ENDIF}
                              ReceivedCommand(Self, mecEnded, 0);
                              //Break;
                            end;

        MPV_EVENT_AUDIO_RECONFIG: begin
                                    LoadTracks;
                                    ReceivedCommand(Self, mecTracks, 0);
                                    //Break;
                                  end;
        {$IFNDEF USETIMER}
        MPV_EVENT_PROPERTY_CHANGE: if (Pmpv_event_property(Event^.Data)^.Name = 'playback-time') and (Pmpv_event_property(Event^.Data)^.format = MPV_FORMAT_INT64) then
                                   begin
                                     //ReceivedCommand(Self, mecTimeChanged, Integer(Pmpv_event_property(Event^.Data)^.data));
                                     ReceivedCommand(Self, mecTimeChanged, GetMediaPos);
                                     //Break;
                                   end;
                                  {if ((Pmpv_event_property(Event^.Data)^.Name = 'aid') or
                                     (Pmpv_event_property(Event^.Data)^.Name = 'vid')) and
                                     (Pmpv_event_property(Event^.Data)^.format = MPV_FORMAT_INT64) then
                                   begin
                                     LoadTracks;
                                     ReceivedCommand(Self, mecTracks, 0);
                                     //Break;
                                   end;}
        {$ENDIF}
      end;
      Event := mpv_wait_event(FHandle^, 0);
    end;
  end
  else
    inherited ReceivedCommand(Sender, ACommand, AParam);
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.SetTrack(const TrackType: TUWMediaEngineTrackType; const ID: Integer);
var
  s   : String;
  Num : Int64;
begin
  case TrackType of
    trkAudio    : s := 'aid';
    trkVideo    : s := 'vid';
    trkSubtitle : s := 'sid';
  else
    Exit;
  end;
  Num := ID;
  mpv_set_property(FHandle^, PChar(s), MPV_FORMAT_INT64, @Num);
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.SetTrack(const Index: Integer);
begin
  SetTrack(TrackList[Index].Kind, TrackList[Index].ID);
end;

// -----------------------------------------------------------------------------

function TUWLibMPV.GetBoolProperty(const APropertyName: String): Boolean;
var
  p: Integer;
begin
  Result := False;
  if not Initialized then Exit;
  mpv_get_property(FHandle^, PChar(APropertyName), MPV_FORMAT_FLAG, @p);
  Result := Boolean(p);
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.SetBoolProperty(const APropertyName: String; const AValue: Boolean);
var
  p: Integer;
begin
  if not Initialized then Exit;

  if AValue then
    p := 1
  else
    p :=0;

  mpv_set_property(FHandle^, PChar(APropertyName), MPV_FORMAT_FLAG, @p);
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.SetStringProperty(const APropertyName, AValue: String);
var
  p: PChar;
begin
  if not Initialized then Exit;
  p := PChar(AValue);
  mpv_set_property_string(FHandle^, PChar(APropertyName), p);
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.Pause;
begin
  if State = mesPlay then
  begin
    SetBoolProperty('pause', True);
    State := mesPause;
  end
  else //if (State = mesPause) or (State = mesStop) then
  begin
    SetBoolProperty('pause', False);
    State := mesPlay;
  end;

  inherited Pause;
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.Resume;
begin
  if State = mesPause then
  begin
    SetBoolProperty('pause', False);
    State := mesPlay;
  end;

  inherited Pause;
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.Stop;
begin
  if State <> mesStop then
  begin
    SetBoolProperty('pause', True);
    SetMediaPos(0);
    State := mesStop;

    inherited Stop;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.Seek(const MSecs: Integer; const SeekAbsolute: Boolean);
var
  currpos: integer;
begin
  if SeekAbsolute then
    SetMediaPos(MSecs)
  else
  begin
    currpos := GetMediaPos;
    SetMediaPos(currpos + MSecs);
  end;
end;

// -----------------------------------------------------------------------------
procedure TUWLibMPV.NextFrame;
var
  args: array of PChar;
begin
  if not Initialized then Exit;
  SetLength(args, 2);
  args[0] := 'frame-step';
  args[1] := NIL;
  mpv_command(FHandle^, PPChar(@args[0]));
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.PreviousFrame;
var
  args: array of PChar;
begin
  if not Initialized then Exit;
  SetLength(args, 2);
  args[0] := 'frame-back-step';
  args[1] := NIL;
  mpv_command(FHandle^, PPChar(@args[0]));
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.PlaybackRate(const AValue: Byte);
var
  rate: Double;
begin
  if not Initialized then Exit;
  rate := AValue / 100.0;
  mpv_set_property(FHandle^, 'speed', MPV_FORMAT_DOUBLE, @rate);
end;

// -----------------------------------------------------------------------------

function TUWLibMPV.GetVolume: Integer;
var
  vol: Double;
begin
  Result := 0;
  if not Initialized then Exit;
  mpv_get_property(FHandle^, 'volume', MPV_FORMAT_DOUBLE, @vol);
  Result := Trunc(vol * (255 / MPVMAXVOLUME));
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.SetVolume(const AValue: Integer);
var
  vol: Double;
begin
  if not Initialized then Exit;
  vol := AValue * (MPVMAXVOLUME / 255);
  mpv_set_property(FHandle^, 'volume', MPV_FORMAT_DOUBLE, @vol);
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.ShowText(const AText: String);
var
  args: array of PChar;
begin
  if not Initialized or (AText = FText) then Exit;
  FText := AText;
  SetLength(args, 4);
  args[0] := 'expand-properties';
  args[1] := 'show-text';
  args[2] := PChar('${osd-ass-cc/0}' + FText);
  args[3] := NIL;
  mpv_command(FHandle^, PPChar(@args[0]));
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.SetTextColor(const AValue: String);
begin
  if not Initialized then Exit;
  mpv_set_option_string(FHandle^, 'osd-color', PChar(AValue));
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.SetTextPosition(const AValue: String);
begin
  if not Initialized then Exit;
  mpv_set_option_string(FHandle^, 'osd-align-y', PChar(AValue));
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.SetTextSize(const AValue: Integer);
var
  num: Int64;
begin
  if not Initialized then Exit;
  num := AValue;
  mpv_set_property(FHandle^, 'osd-font-size', MPV_FORMAT_INT64, @Num);
end;

// -----------------------------------------------------------------------------

end.

