{*
 *  URUWorks Media Engine (Thread)
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

unit UWMediaEngine.Thread;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}
{$I UWMediaEngine.inc}

interface

uses
  Classes, SysUtils
  {$IFDEF ENABLE_OPENGL}, UWlibMPV.Client, UWlibMPV.Render, UWlibMPV.Render_gl,
  gl, glext, OpenGLContext{$ENDIF};

// -----------------------------------------------------------------------------

type

  // used to handle mpv envents

  { TUWCustomEventThread }

  TUWMediaEngineEvent = class;
  TUWCustomEventThread = class(TThread)
  private
    procedure HandleEvent;
  public
    FOwner : TUWMediaEngineEvent;
    Event  : PRtlEvent;
    constructor Create(AOwner: TUWMediaEngineEvent);
    procedure Execute; override;
    destructor Destroy; override;
  end;

  { TUWMediaEngineEvent }

  TUWMediaEngineEvent = class
  private
    FThread  : TUWCustomEventThread;
    FOnEvent : TNotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PushEvent;
    property OnEvent: TNotifyEvent read FOnEvent write FOnEvent;
  end;

  {$IFDEF ENABLE_OPENGL}
  // used to handle mpv/opengl envents

  { TUWCustomGlRenderThread }

  TUWMediaEngineGlRender = class;
  TUWCustomGlRenderThread = class(TThread)
  private
    FOpenGlControl : TOpenGlControl;
    FRenderContext : pmpv_render_context;
    FMPVHandle     : Pmpv_handle;
    FCreateParams  : array of mpv_render_param;
    FRenderParams  : array of mpv_render_param;
    FOpenGLParams  : mpv_opengl_init_params;
    function Initialize_GL: Boolean;
  protected
    procedure TerminatedSet; override;
  public
    FOwner    : TUWMediaEngineGlRender;
    Event     : PRtlEvent;
    ErrorCode : Integer;
    constructor Create(AOwner: TUWMediaEngineGlRender; AControl: TOpenGlControl; AHandle: Pmpv_handle);
    procedure Execute; override;
    destructor Destroy; override;
  end;

  { TUWMediaEngineGlRender }

  TUWMediaEngineGlRender = class
  private
    FThread: TUWCustomGlRenderThread;
  public
    constructor Create(AControl: TOpenGlControl; AHandle: pmpv_handle);
    destructor Destroy; override;
    procedure Render;
    function ErrorCode: Integer;
  end;
  {$ENDIF}

// -----------------------------------------------------------------------------

implementation

{$IFDEF DEBUG}uses UWDebug;{$ENDIF}

{$IFDEF ENABLE_OPENGL}
const
  glFlip: Longint = 1;
{$ENDIF}

// -----------------------------------------------------------------------------

{ TUWCustomEventThread }

// -----------------------------------------------------------------------------

constructor TUWCustomEventThread.Create(AOwner: TUWMediaEngineEvent);
begin
  inherited Create(True);
  Priority := tpHigher;
  FOwner   := AOwner;
  Event    := RTLEventCreate;
end;

// -----------------------------------------------------------------------------

procedure TUWCustomEventThread.Execute;
begin
  while not Terminated do
  begin
    RTLEventWaitFor(Event);
    Queue(@HandleEvent); //Synchronize(@HandleEvent);
    RTLEventResetEvent(Event);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWCustomEventThread.HandleEvent;
begin
  if Assigned(FOwner.OnEvent) then FOwner.OnEvent(FOwner);
end;

// -----------------------------------------------------------------------------

destructor TUWCustomEventThread.Destroy;
begin
  RTLEventDestroy(Event);
  FOwner := NIL;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

{ TUWMediaEngineEvent }

// -----------------------------------------------------------------------------

constructor TUWMediaEngineEvent.Create;
begin
  FOnEvent := NIL;
  FThread := TUWCustomEventThread.Create(Self);
  FThread.Start;
end;

// -----------------------------------------------------------------------------

destructor TUWMediaEngineEvent.Destroy;
begin
  {$IFDEF DEBUG}DebugMsg('TUWMediaEngineEvent: destroy');{$ENDIF}
  FThread.Terminate;
  RTLEventSetEvent(FThread.Event);
  FThread.WaitFor;
  FThread.Free;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWMediaEngineEvent.PushEvent;
begin
  RTLEventSetEvent(FThread.Event);
end;

{$IFDEF ENABLE_OPENGL}
// -----------------------------------------------------------------------------

{ TUWCustomGlRenderThread }

// -----------------------------------------------------------------------------

function GetProcAddress_GL(ctx: Pointer; Name: PChar): Pointer; cdecl;
begin
  Result := GetProcAddress(LibGL, Name);
  if Result = NIL then Result := wglGetProcAddress(Name);
  {$IFDEF DEBUG}if Result = NIL then DebugMsg('GetProcAddress_GL: NIL!');{$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure Update_GL(cb_ctx: Pointer); cdecl;
begin
  if (cb_ctx <> NIL) then TUWMediaEngineGlRender(cb_ctx).Render;
end;

// -----------------------------------------------------------------------------

constructor TUWCustomGlRenderThread.Create(AOwner: TUWMediaEngineGlRender; AControl: TOpenGlControl; AHandle: Pmpv_handle);
begin
  inherited Create(True);

  FOwner         := AOwner;
  FOpenGlControl := AControl;
  FMPVHandle     := AHandle;
  ErrorCode      := 0;

  if FOpenGlControl.Visible then FOpenGlControl.Visible := False;
end;

// -----------------------------------------------------------------------------

function TUWCustomGlRenderThread.Initialize_GL: Boolean;
begin
  Result := False;
  {$IFDEF DEBUG}DebugMsg('CustomGlRenderThread: Initialize_GL');{$ENDIF}
  mpv_set_option_string(FMPVHandle^,'vd-lavc-dr', 'no');

  {$IFDEF DEBUG}DebugMsg('CustomGlRenderThread: CreateParams');{$ENDIF}
  FCreateParams := NIL;
  FOpenGLParams.get_proc_address := @GetProcAddress_GL;
  FOpenGLParams.get_proc_address_ctx := NIL;

  SetLength(FCreateParams, 4);
  FCreateParams[0]._type    := MPV_RENDER_PARAM_API_TYPE;
  FCreateParams[0].Data     := PChar(MPV_RENDER_API_TYPE_OPENGL);
  FCreateParams[1]._type    := MPV_RENDER_PARAM_OPENGL_INIT_PARAMS;
  FCreateParams[1].Data     := @FOpenGLParams;
  FCreateParams[2]._type    := MPV_RENDER_PARAM_ADVANCED_CONTROL;
  FCreateParams[2].Data     := @glFlip;
  FCreateParams[3]._type    := MPV_RENDER_PARAM_INVALID;
  FCreateParams[3].Data     := NIL;
  FOpenGlControl.MakeCurrent();
  {$IFDEF DEBUG}DebugMsg('CustomGlRenderThread: mpv_render_context_create');{$ENDIF}

  if Assigned(mpv_render_context_create) then
    ErrorCode := mpv_render_context_create(FRenderContext, FMPVHandle^, Pmpv_render_param(@FCreateParams[0]))
  else
    ErrorCode := -99;
  {$IFDEF DEBUG}DebugMsg('CustomGlRenderThread: mpv_render_context_create --> ' + IntToStr(ErrorCode));{$ENDIF}
  if (ErrorCode <> 0) then Exit; //raise Exception.Create('Failed to initialize mpv GL context');
  {$IFDEF DEBUG}DebugMsg('CustomGlRenderThread: CreateRenderParams');{$ENDIF}
  SetLength(FRenderParams, 3);
  FRenderParams[0]._type := MPV_RENDER_PARAM_OPENGL_FBO;
  FRenderParams[0].Data  := NIL;
  FRenderParams[1]._type := MPV_RENDER_PARAM_FLIP_Y;
  FRenderParams[1].Data  := @glFlip;
  FRenderParams[2]._type := MPV_RENDER_PARAM_INVALID;
  FRenderParams[2].Data  := NIL;

  Event := RTLEventCreate;
  FOpenGlControl.Visible := True;
  FOpenGlControl.MakeCurrent();
  mpv_render_context_set_update_callback(FRenderContext^, @Update_GL, FOwner);
  mpv_render_context_update(FRenderContext^);
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TUWCustomGlRenderThread.Execute;
var
  mpvfbo: mpv_opengl_fbo;
begin
  if not Initialize_GL then Exit;
  while not Terminated do
  begin
    mpvfbo.fbo             := 0;
    mpvfbo.internal_format := 0;
    FRenderParams[0].Data  := @mpvfbo;
    RTLEventWaitFor(Event);
    while ((mpv_render_context_update(FRenderContext^) and MPV_RENDER_UPDATE_FRAME) <> 0) and not Terminated do
    begin
      FOpenGlControl.MakeCurrent();
      mpvfbo.h               := FOpenGlControl.Height;
      mpvfbo.w               := FOpenGlControl.Width;
      mpv_render_context_render(FRenderContext^, Pmpv_render_param(@FRenderParams[0]));
      FOpenGlControl.SwapBuffers();
      mpv_render_context_report_swap(FRenderContext^);
    end;
    RTLeventResetEvent(Event);
  end;
end;

// -----------------------------------------------------------------------------

destructor TUWCustomGlRenderThread.Destroy;
begin
  if ErrorCode = 0 then
  begin
    mpv_render_context_set_update_callback(FRenderContext^, NIL, NIL);
    mpv_render_context_update(FRenderContext^);
    mpv_render_context_free(FRenderContext^);
  end;
  RTLeventDestroy(Event);

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWCustomGlRenderThread.TerminatedSet;
begin
  if Assigned(Event) then RTLeventSetEvent(Event);
  inherited TerminatedSet;
end;

// -----------------------------------------------------------------------------

{ TUWMediaEngineGlRender }

// -----------------------------------------------------------------------------

constructor TUWMediaEngineGlRender.Create(AControl: TOpenGlControl; AHandle: pmpv_handle);
begin
  FThread := TUWCustomGlRenderThread.Create(Self, AControl, AHandle);
  FThread.Start;
end;

// -----------------------------------------------------------------------------

destructor TUWMediaEngineGlRender.Destroy;
begin
  FThread.Terminate;
  FThread.WaitFor;
  FThread.Free;
  FThread := NIL;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWMediaEngineGlRender.Render;
begin
  RTLeventSetEvent(FThread.Event);
end;

// -----------------------------------------------------------------------------

function TUWMediaEngineGlRender.ErrorCode: Integer;
begin
  Result := FThread.ErrorCode;
end;

// -----------------------------------------------------------------------------

{$ENDIF}

// -----------------------------------------------------------------------------

end.

