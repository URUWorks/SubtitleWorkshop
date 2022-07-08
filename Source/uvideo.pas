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
 *  Copyright (C) 2001-2022 URUWorks, uruworks@gmail.com.
 *}

unit UVideo;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, Controls, UMain;

type

  { TfrmVideo }

  TfrmVideo = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

var
  frmVideo: TfrmVideo;

  // -----------------------------------------------------------------------------

implementation

uses UTypes, UCommon;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmVideo }

// -----------------------------------------------------------------------------

procedure TfrmVideo.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  s: String;
  p: Integer;
begin
  s := frmMain.MPV.Engine.FileName;
  if s <> '' then
  begin
    if frmMain.MPV.Engine.IsPaused then frmMain.MPV.Tag := 1;
    p := frmMain.MPV.Engine.Position;
    frmMain.MPV.Engine.UnInitialize;
    frmMain.actDockVideoControls.Tag := -2;
  end;

  //frmMain.lyoVideo.Align   := alRight;
  frmMain.lyoVideo.Parent  := frmMain;
  frmMain.sptVideo.Visible := True;
  SetWorkspace(frmMain.actChangeWorkspace.Checked);

  if (s <> '') and frmMain.MPV.Engine.Initialize then
  begin
    frmMain.MPV.Engine.Play(s, p);
    frmMain.MPV.Engine.SetTextColor(Options.Marquee.Color);
    frmMain.MPV.Engine.SetTextPosition(Options.Marquee.Position);
    frmMain.MPV.Engine.SetTextSize(Options.Marquee.Size);
    frmMain.actMediaChangePlayRateExecute(NIL);
  end;
  frmMain.actDockVideoControls.Checked := True;

  CloseAction := caFree;
  frmVideo := NIL;
end;

// -----------------------------------------------------------------------------

end.

