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

uses UTypes, UCommon, UMain, MPVPlayer;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmAudioExtraction }

// -----------------------------------------------------------------------------

{$IFDEF UNIX}
function GetInstallPath(const AFileName: String): String;
const
  {$IFDEF LINUX}
  pathLst : array[0..2] of string = (
    '/usr/bin',
    '/bin',
    '/usr/local/bin'
  );
  {$ELSE}
  pathLst : array[0..1] of string = (
    '/usr/local/bin',
    '/opt/local/bin'
    );
  {$ENDIF}
var
  pathIdx : Integer;
  pathStr : string;
begin
  for pathIdx := Low(pathLst) to High(pathLst) do
  begin
    pathStr := pathLst[pathIdx];
    if not DirectoryExists(pathStr) then continue;
    // look for bin
    if FileExists(pathStr + PathDelim + AFileName) then
    begin
      Result := pathStr + PathDelim + AFileName;
      Exit;
    end;
  end;

  {$IFDEF DARWIN}
  Result := '/Applications/' + AFileName + '.app/Contents/MacOS/' + AFileName;
  if FileExists(Result) then
    Exit
  else
  begin
    Result := '~' + Result;
    if FileExists(Result) then
      Exit;
  end;
  {$ENDIF}

  Result := '';
end;
{$ENDIF}

procedure ProcessCB(const TimeElapsed: Double; var Cancel: Boolean);
begin
  Cancel := False;
  frmAudioExtraction.lblTimeElapsed.Caption := TimeToString(Trunc(TimeElapsed)*1000, 'mm:ss');
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioExtraction.FormCreate(Sender: TObject);
var
  i, tl: Integer;
  s: String;
begin
  ReadLangForForm(LanguageFileName, Self);

  if not FileExists(AudioExtraction.FileName) and AudioExtraction.FileName.EndsWith(VLC_EXE) then
  begin
    AudioExtraction.FileName := GetInstallFolderVLC + VLC_EXE;
  end
  else if not FileExists(AudioExtraction.FileName) and AudioExtraction.FileName.EndsWith(FFMPEG_EXE) then
  begin
    {$IFDEF UNIX}AudioExtraction.FileName := GetInstallPath(FFMPEG_EXE);{$ENDIF}
  end;

  edtApp.Text    := AudioExtraction.FileName;
  edtParams.Text := AudioExtraction.Params;

  tl := Length(frmMain.MPV.TrackList);
  if tl > 0 then
  begin
    for i := 0 to tl-1 do
    begin
      if frmMain.MPV.TrackList[i].Kind = ttAudio then
      begin
        s := IntToStr(frmMain.MPV.TrackList[i].ID) + ': ';
        if frmMain.MPV.TrackList[i].Title <> '' then s := s + frmMain.MPV.TrackList[i].Title + ', ';
        if frmMain.MPV.TrackList[i].Lang <> '' then s := s + frmMain.MPV.TrackList[i].Lang + ', ';
        if frmMain.MPV.TrackList[i].Codec <> '' then s := s + '(' + frmMain.MPV.TrackList[i].Codec + ')';

        cboTrack.Items.Add(s);

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
        if not ForceDirectories(WaveformsFolder) then
        begin
          ShowMessage(Strings.WriteDenieded);
          Exit;
        end;
      end;
      AudioExtraction.FileName := edtApp.Text;
      AudioExtraction.Params   := edtParams.Text;
      s := WaveformsFolder + ChangeFileExt(ExtractFileName(frmMain.MPV.FileName), '.wav');

      p := frmMain.MPV.IsPlaying;
      if p then frmMain.MPV.Pause;

      ExecuteApp(AudioExtraction.FileName,
        Format(AudioExtraction.Params, [frmMain.MPV.FileName.Replace(' ', '*', [rfReplaceAll]) , cboTrack.ItemIndex, s.Replace(' ', '*', [rfReplaceAll])]),
        True, True, @ProcessCB);

      if FileExists(s) then
        if frmMain.WAVE.LoadWaveFromFile(s) then
          if not frmMain.actAudioPreview.Checked then frmMain.actAudioPreview.Execute;

      if p then frmMain.MPV.Pause;
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

