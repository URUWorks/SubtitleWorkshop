{*
 *  URUWorks Media Engine Thread
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

unit UWMediaEngine.Thread;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UWMediaEngine;

// -----------------------------------------------------------------------------

type

  { TUWCustomMediaEngineThread }

  TUWMediaEngineThread = class;
  TUWCustomMediaEngineThread = class(TThread)
  private
    procedure DoEvent;
  public
    Owner: TUWMediaEngineThread;
    Event: PRtlEvent;
    constructor Create;
    procedure Execute; override;
    destructor Destroy; override;
  end;

  { TUWMediaEngineThread }

  TUWMediaEngineThread = class
  private
    FThread    : TUWCustomMediaEngineThread;
    FOnCommand : TUWMediaEngineOnCommand;
    FCommand   : TUWMediaEngineCommand;
    FParam     : Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SendCommand(const ACommand: TUWMediaEngineCommand; const AParam: Integer);
    property OnCommand: TUWMediaEngineOnCommand read FOnCommand write FOnCommand;
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{ TUWCustomMediaEngineThread }

// -----------------------------------------------------------------------------

constructor TUWCustomMediaEngineThread.Create;
begin
  inherited Create(True);
  Event := RTLEventCreate;
end;

// -----------------------------------------------------------------------------

procedure TUWCustomMediaEngineThread.Execute;
begin
  while not Terminated do
  begin
    RtlEventWaitFor(Event);
    if Assigned(Owner.FOnCommand) and not Terminated then Synchronize(@DoEvent);
    RTLEventResetEvent(Event);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWCustomMediaEngineThread.DoEvent;
begin
  if Assigned(Owner.FOnCommand) then Owner.FOnCommand(Owner, Owner.FCommand, Owner.FParam);
end;

// -----------------------------------------------------------------------------

destructor TUWCustomMediaEngineThread.Destroy;
begin
  RTLEventDestroy(Event);
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

{ TUWMediaEngineThread }

// -----------------------------------------------------------------------------

constructor TUWMediaEngineThread.Create;
begin
  FThread := TUWCustomMediaEngineThread.Create;
  FThread.Owner := Self;
  FThread.Start;
end;

// -----------------------------------------------------------------------------

destructor TUWMediaEngineThread.Destroy;
begin
  FThread.Terminate;
  RTLEventSetEvent(FThread.Event);
  FThread.Free;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWMediaEngineThread.SendCommand(const ACommand: TUWMediaEngineCommand; const AParam: integer);
begin
  FCommand := ACommand;
  FParam   := AParam;

  RTLEventSetEvent(FThread.Event);
end;

// -----------------------------------------------------------------------------

end.

