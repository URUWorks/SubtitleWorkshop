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

unit UAudioExtraction;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  UWSystem.Process, UWSystem.TimeUtils;

type

  { TfrmAudioExtraction }

  TfrmAudioExtraction = class(TForm)
    btnClose: TButton;
    btnExtract: TButton;
    btnBrowse: TButton;
    cboTrack: TComboBox;
    edtApp: TEdit;
    edtParams: TEdit;
    lblTimeElapsed: TLabel;
    lblWait: TLabel;
    lblTrack: TLabel;
    lblApp: TLabel;
    lblParams: TLabel;
    lblStatus: TLabel;
    od: TOpenDialog;
    pagExtractor: TPageControl;
    prbProgress: TProgressBar;
    tabOptions: TTabSheet;
    tabApp: TTabSheet;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure ExtractAudioTrack;
  end;

var
  frmAudioExtraction: TfrmAudioExtraction;
  strExtracting, strGenerating: String;

// -----------------------------------------------------------------------------

implementation

uses UTypes, UCommon, UMain, UWMediaEngine;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmAudioExtraction }

// -----------------------------------------------------------------------------

procedure ProcessCB(const TimeElapsed: Double; var Cancel: Boolean);
begin
  Cancel := False;
  frmAudioExtraction.lblTimeElapsed.Caption := TimeToString(Trunc(TimeElapsed)*1000, 'mm:ss');
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioExtraction.FormCreate(Sender: TObject);
var
  i, tl: Integer;
begin
  ReadLangForForm(LanguageFileName, Self);

  if not FileExists(AudioExtraction.FileName) and AudioExtraction.FileName.EndsWith(VLC_EXE) then
  begin
    AudioExtraction.FileName := GetInstallFolderVLC + VLC_EXE;
  end;

  edtApp.Text    := AudioExtraction.FileName;
  edtParams.Text := AudioExtraction.Params;

  tl := Length(frmMain.MPV.Engine.TrackList);
  if tl > 0 then
  begin
    for i := 0 to tl-1 do
    begin
      if frmMain.MPV.Engine.TrackList[i].Kind = trkAudio then
      begin
        cboTrack.Items.Add(IntToStr(frmMain.MPV.Engine.TrackList[i].ID) + ': '
        + frmMain.MPV.Engine.TrackList[i].Decoder + ', '
        + frmMain.MPV.Engine.TrackList[i].Info);

        if cboTrack.Items.Count > 0 then cboTrack.ItemIndex := 0;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioExtraction.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmAudioExtraction := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioExtraction.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioExtraction.btnBrowseClick(Sender: TObject);
begin
  od.FileName := edtApp.Text;
  if od.Execute then edtApp.Text := od.FileName;
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioExtraction.btnExtractClick(Sender: TObject);
begin
  ExtractAudioTrack;
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioExtraction.ExtractAudioTrack;
var
//  AProcess: TProcess;
  s: String;
  p: Boolean;
begin
  if not FileExists(edtApp.Text) then
    ShowMessage(Format(Strings.ExtractLibError, [ExtractFileName(edtApp.Text)]))
  else if (edtParams.Text <> '') then
  begin
    btnExtract.Enabled := False;
    btnClose.Enabled := False;
    lblWait.Visible := True;
    lblStatus.Visible := True;
    lblTimeElapsed.Visible := True;
    lblStatus.Caption := strExtracting;

    Application.ProcessMessages;
    try
      if not DirectoryExists(WaveformsFolder) then
      begin
        //DirectoryIsWritable
        if not ForceDirectories(WaveformsFolder) then
        begin
          ShowMessage(Strings.WriteDenieded);
          Exit;
        end;
      end;
      AudioExtraction.FileName := edtApp.Text;
      AudioExtraction.Params   := edtParams.Text;
      s := WaveformsFolder + ChangeFileExt(ExtractFileName(frmMain.MPV.Engine.FileName), '.wav');

      p := frmMain.MPV.Engine.IsPlaying;
      if p then frmMain.MPV.Engine.Pause;

      ExecuteApp(AudioExtraction.FileName,
        Format(AudioExtraction.Params, [frmMain.MPV.Engine.FileName, cboTrack.ItemIndex+1, s]),
        True, True, @ProcessCB);

      if FileExists(s) then
        if frmMain.WAVE.LoadWaveFromFile(s) then
          if not frmMain.actAudioPreview.Checked then frmMain.actAudioPreview.Execute;

      if p then frmMain.MPV.Engine.Pause;
    finally
      lblWait.Visible := False;
      lblStatus.Visible := False;
      lblTimeElapsed.Visible := False;
      btnExtract.Enabled := True;
      btnClose.Enabled := True;
      btnClose.Click;
    end;
  end
end;

// -----------------------------------------------------------------------------

end.

