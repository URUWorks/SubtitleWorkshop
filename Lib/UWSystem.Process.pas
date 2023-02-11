{*
 *  URUWorks Lazarus Process
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

unit UWSystem.Process;

// -----------------------------------------------------------------------------

interface

uses
  Forms, Classes, SysUtils, Process;

// -----------------------------------------------------------------------------

type
  TUWProcessCB = procedure(const TimeElapsed: Double; var Cancel: Boolean);

procedure ExecuteApp(const AApp, AParams: String; const AHidden: Boolean = True; const AWaitOnExit: Boolean = True; const ACB: TUWProcessCB = NIL);

// -----------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

procedure ExecuteApp(const AApp, AParams: String; const AHidden: Boolean = True; const AWaitOnExit: Boolean = True; const ACB: TUWProcessCB = NIL);
var
  AProcess: TProcess;
  ACancel: Boolean;
  ATimeElapsed: Double;
  AParamArray: TStringArray;
  i: Integer;
begin
  try
    AProcess := TProcess.Create(NIL);
    if AHidden then AProcess.ShowWindow := swoHide;
    AProcess.Executable := AApp;
    AParamArray := AParams.Split(' ');
    for i := 0 to High(AParamArray) do
      AProcess.Parameters.Add(AParamArray[i]);

    AProcess.Execute;

    if AWaitOnExit then  // wait for the launched application to finish
    begin
      ATimeElapsed := 0;
      while AProcess.Running do
      begin
        Application.ProcessMessages;
        Sleep(100);
        ATimeElapsed := ATimeElapsed + 0.1;
        if Assigned(ACB) then
        begin
          ACB(ATimeElapsed, ACancel);
          Application.ProcessMessages;
          if ACancel then AProcess.Terminate(-1);
        end;
      end;
    end;
  finally
    AProcess.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
