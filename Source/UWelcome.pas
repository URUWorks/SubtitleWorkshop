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

unit UWelcome;

{$mode objfpc}{$H+}

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls, Buttons;

type

  { TfrmWelcome }

  TfrmWelcome = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    chkShowOnStartup: TCheckBox;
    imgSW: TImage;
    lblOpenFile: TLabel;
    lblNewProject: TLabel;
    lblRecentFiles: TLabel;
    lblNewSubtitle: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    spdNewSubtitle: TSpeedButton;
    spdOpenFile: TSpeedButton;
    spdNewProject: TSpeedButton;
    procedure FormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure spdNewProjectClick(Sender: TObject);
    procedure spdNewSubtitleClick(Sender: TObject);
    procedure spdOpenFileClick(Sender: TObject);
  private
    { private declarations }
    procedure MRUItemClick(Sender: TObject);
  public
    { public declarations }
  end;

var
  frmWelcome: TfrmWelcome;

// -----------------------------------------------------------------------------

implementation

uses UTypes, UCommon, UMain;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmWelcome }

// -----------------------------------------------------------------------------

procedure TfrmWelcome.FormCreate(Sender: TObject);
var
  i   : Integer;
  lbl : TLabel;
begin
  ReadLangForForm(LanguageFileName, Self);
  chkShowOnStartup.Checked := Options.ShowWelcomeAtStartup;

  if MRU.Items.Count > 0 then
    for i := 0 to MRU.Items.Count-1 do
    begin
      lbl            := TLabel.Create(Self);
      lbl.Parent     := Self;
      lbl.SetBounds(228, 46 + (i*22), 100, 15);
      lbl.Caption    := ExtractFileName(MRU.Items[i]);
      lbl.Hint       := ExtractFilePath(MRU.Items[i]);
      lbl.ShowHint   := True;
      lbl.Tag        := i;
      lbl.Font.Color := clNavy;
      if not FileExists(MRU.Items[i]) then
      begin
        lbl.Enabled := False; //lbl.Font.Color := clMaroon;
        lbl.Font.Style := lbl.Font.Style + [fsStrikeOut];
      end
      else
      begin
        lbl.Cursor  := crHandPoint;
        lbl.OnClick := @MRUItemClick;
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmWelcome.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Options.ShowWelcomeAtStartup := chkShowOnStartup.Checked;
  CloseAction := caFree;
  frmWelcome  := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmWelcome.FormClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmWelcome.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FormClick(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmWelcome.MRUItemClick(Sender: TObject);
begin
  frmMain.LoadSubtitle(MRU.Items[(Sender as TLabel).Tag]);
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmWelcome.spdNewSubtitleClick(Sender: TObject);
begin
  Hide;
  frmMain.actNewSubtitle.Execute;
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmWelcome.spdOpenFileClick(Sender: TObject);
begin
  Hide;
  frmMain.actLoadSubtitle.Execute;
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmWelcome.spdNewProjectClick(Sender: TObject);
begin
  Hide;
  frmMain.actNewProject.Execute;
  Close;
end;

// -----------------------------------------------------------------------------

end.

