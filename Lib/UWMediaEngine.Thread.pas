{*
 *  URUWorks Subtitle Workshop
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

unit UWMediaEngine.Thread;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UWMediaEngine;

// -----------------------------------------------------------------------------

type

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

// -----------------------------------------------------------------------------

implementation

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
    RtlEventWaitFor(Event);
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

// -----------------------------------------------------------------------------

end.

