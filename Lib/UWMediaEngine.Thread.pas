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
  {$IFDEF USEOPENGL}, UWlibMPV.Client, UWlibMPV.Render, UWlibMPV.Render_gl,
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

  {$IFDEF USEOPENGL}
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
    procedure Initialize_GL;
  public
    FOwner : TUWMediaEngineGlRender;
    Event  : PRtlEvent;
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
  end;
  {$ENDIF}

// -----------------------------------------------------------------------------

implementation

{$IFDEF USEOPENGL}
const
  glFlip: longint = 1;
{$ENDIF}

// -----------------------------------------------------------------------------

{ TUWCustomEventThread }

// -----------------------------------------------------------------------------

constructor TUWCustomEventThread.Create(AOwner: TUWMediaEngineEvent);
begin
  inherited Create(True);
  FOwner := AOwner;
  Event  := RTLEventCreate;
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
  FThread := TUWCustomEventThread.Create(Self);
  FThread.Start;
end;

// -----------------------------------------------------------------------------

destructor TUWMediaEngineEvent.Destroy;
begin
  FThread.Terminate;
  RTLEventSetEvent(FThread.Event);
  FThread.Free;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWMediaEngineEvent.PushEvent;
begin
  RTLEventSetEvent(FThread.Event);
end;

{$IFDEF USEOPENGL}
// -----------------------------------------------------------------------------

{ TUWCustomGlRenderThread }

// -----------------------------------------------------------------------------

function GetProcAddress_GL(ctx: Pointer; Name: PChar): Pointer; cdecl;
begin
  Result := GetProcAddress(LibGL, Name);
  if Result = NIL then Result := wglGetProcAddress(Name);
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
  FOpenGlControl.Visible := True;
end;

// -----------------------------------------------------------------------------

procedure TUWCustomGlRenderThread.Initialize_GL;
var
  res: Integer;
begin
  mpv_set_option_string(FMPVHandle^,'vd-lavc-dr','no');
  FCreateParams := NIL;
  SetLength(FCreateParams, 4);
  FCreateParams[0]._type    := MPV_RENDER_PARAM_API_TYPE;
  FCreateParams[0].Data     := PChar(MPV_RENDER_API_TYPE_OPENGL);
  FCreateParams[1]._type    := MPV_RENDER_PARAM_OPENGL_INIT_PARAMS;
  FOpenGLParams.get_proc_address := @GetProcAddress_GL;
  FCreateParams[1].Data     := @FOpenGLParams;
  FCreateParams[2]._type    := MPV_RENDER_PARAM_ADVANCED_CONTROL;
  FCreateParams[2].Data     := @glFlip;
  FCreateParams[3]._type    := MPV_RENDER_PARAM_INVALID;
  FCreateParams[3].Data     := NIL;
  FOpenGlControl.MakeCurrent();
  res := mpv_render_context_create(FRenderContext, FMPVHandle^, Pmpv_render_param(@FCreateParams[0]));
  if (res < 0) then raise Exception.Create('Failed to initialize mpv GL context');

  SetLength(FRenderParams, 3);
  FRenderParams[0]._type := MPV_RENDER_PARAM_OPENGL_FBO;
  FRenderParams[0].Data  := NIL;
  FRenderParams[1]._type := MPV_RENDER_PARAM_FLIP_Y;
  FRenderParams[1].Data  := @glFlip;
  FRenderParams[2]._type := MPV_RENDER_PARAM_INVALID;
  FRenderParams[3].Data  := NIL;
  Event := RTLEventCreate;
  FOpenGlControl.MakeCurrent();
  mpv_render_context_set_update_callback(FRenderContext^, @Update_GL, FOwner);
  mpv_render_context_update(FRenderContext^);
end;

// -----------------------------------------------------------------------------

procedure TUWCustomGlRenderThread.Execute;
var
  mpvfbo: mpv_opengl_fbo;
begin
  Initialize_GL;
  while not Terminated do
  begin
    RTLEventWaitFor(Event);
    begin
      while (mpv_render_context_update(FRenderContext^) and MPV_RENDER_UPDATE_FRAME) <> 0 do
      begin
        FOpenGlControl.MakeCurrent();
        mpvfbo.fbo             := 0;
        mpvfbo.h               := FOpenGlControl.Height;
        mpvfbo.w               := FOpenGlControl.Width;
        mpvfbo.internal_format := 0;
        FRenderParams[0].Data  := @mpvfbo;
        mpv_render_context_render(FRenderContext^, Pmpv_render_param(@FRenderParams[0]));
        FOpenGlControl.SwapBuffers();
        mpv_render_context_report_swap(FRenderContext^);
      end;
    end;
    RTLeventResetEvent(Event);
  end;
end;

// -----------------------------------------------------------------------------

destructor TUWCustomGlRenderThread.Destroy;
begin
  mpv_render_context_set_update_callback(FRenderContext^, NIL, NIL);
  mpv_render_context_update(FRenderContext^);
  mpv_render_context_free(FRenderContext^);
  RTLeventdestroy(Event);

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

{ TUWMediaEngineGlRender }

// -----------------------------------------------------------------------------

constructor TUWMediaEngineGlRender.Create(AControl: TOpenGlControl; AHandle: pmpv_handle);
begin
  FThread := TUWCustomGlRenderThread.Create(Self, AControl, AHandle);
  FThread.FreeOnTerminate := True;
  FThread.Start;
end;

// -----------------------------------------------------------------------------

destructor TUWMediaEngineGlRender.Destroy;
begin
  FThread.Terminate;
  Render;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWMediaEngineGlRender.Render;
begin
  RTLeventSetEvent(FThread.Event);
end;

// -----------------------------------------------------------------------------
{$ENDIF}

end.

