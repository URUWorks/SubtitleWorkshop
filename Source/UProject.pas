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

unit UProject;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  UCommon, UTypes, UMain;

type

  { TfrmProject }

  TfrmProject = class(TForm)
    btnLocation: TButton;
    btnFileSub: TButton;
    btnFileVideo: TButton;
    btnFileWave: TButton;
    btnTMFile: TButton;
    btnCancel: TButton;
    btnNew: TButton;
    btnTermFile: TButton;
    cboSourceLang: TComboBox;
    cboTransLang: TComboBox;
    edtProject: TEdit;
    edtLocation: TEdit;
    edtFileSub: TEdit;
    edtFileVideo: TEdit;
    edtFileWave: TEdit;
    edtTMFile: TEdit;
    edtTermFile: TEdit;
    grpFiles: TGroupBox;
    grpTM: TGroupBox;
    grpTerm: TGroupBox;
    lblProject: TLabel;
    lblTermFile: TLabel;
    lblLocation: TLabel;
    lblSourceLang: TLabel;
    lblTransLang: TLabel;
    lblFileSub: TLabel;
    lblFileVideo: TLabel;
    lblFileWave: TLabel;
    lblTMFile: TLabel;
    pagSettings: TPageControl;
    tabGeneral: TTabSheet;
    tabResources: TTabSheet;
    procedure btnCancelClick(Sender: TObject);
    procedure btnFileSubClick(Sender: TObject);
    procedure btnFileVideoClick(Sender: TObject);
    procedure btnFileWaveClick(Sender: TObject);
    procedure btnLocationClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnTermFileClick(Sender: TObject);
    procedure btnTMFileClick(Sender: TObject);
    procedure edtProjectKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmProject: TfrmProject;

// -----------------------------------------------------------------------------

implementation

uses
  UWSystem.Globalization;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmProject }

// -----------------------------------------------------------------------------

function MyOpenDialog(const Filter: String = '*.*'): String;
var
  OD: TOpenDialog;
begin
  Result := '';

  OD := TOpenDialog.Create(NIL);
  try
    OD.Filter := Filter;
    if OD.Execute then
      Result := OD.FileName;
  finally
    OD.Free;
  end;
end;

// -----------------------------------------------------------------------------

function MySelectFolder(const Root: String = ''): String;
var
  SD: TSelectDirectoryDialog;
begin
  Result := '';

  SD := TSelectDirectoryDialog.Create(NIL);
  try
    SD.FileName := Root;
    if SD.Execute then
      Result := SD.FileName;
  finally
    SD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmProject.FormCreate(Sender: TObject);
begin
  ReadLangForForm(LanguageFileName, Self);

  FillCultureTStrings(cboSourceLang.Items);
  cboTransLang.Items.Assign(cboSourceLang.Items);

  cboSourceLang.ItemIndex := 49;  // eng
  cboTransLang.ItemIndex  := 120; // spa

  edtLocation.Text := ProjectsFolder;
end;

// -----------------------------------------------------------------------------

procedure TfrmProject.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmProject  := NIL
end;

// -----------------------------------------------------------------------------

procedure TfrmProject.btnCancelClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmProject.btnFileSubClick(Sender: TObject);
begin
  edtFileSub.Text := MyOpenDialog(Subtitles.FillDialogFilter(Strings.AllSupportedFiles));
end;

// -----------------------------------------------------------------------------

procedure TfrmProject.btnFileVideoClick(Sender: TObject);
var
  i: Integer;
  s: String;
begin
  s := '';
  for i := 0 to Length(TVideoExts)-1 do s := s + '*' + TVideoExts[i] + ';';
  edtFileVideo.Text := MyOpenDialog(Strings.AllSupportedFiles + '|' + s);
end;

// -----------------------------------------------------------------------------

procedure TfrmProject.btnFileWaveClick(Sender: TObject);
var
  i: Integer;
  s: String;
begin
  s := '';
  for i := 0 to Length(TAudioExts)-1 do s := s + '*' + TAudioExts[i] + ';';
  edtFileWave.Text := MyOpenDialog(Strings.AllSupportedFiles + '|' + s);
end;

// -----------------------------------------------------------------------------

procedure TfrmProject.btnLocationClick(Sender: TObject);
begin
  edtLocation.Text := MySelectFolder(edtLocation.Text);
end;

// -----------------------------------------------------------------------------

procedure TfrmProject.btnNewClick(Sender: TObject);
begin
  // enable translation workarea
  if not TranslatorMode then
  begin
    SetTranslatorMode(True);
    frmMain.actTranslatorMode.Checked := True;
  end;
  frmMain.EnableWorkArea;
  // load subtitle source
  frmMain.LoadSubtitle(edtFileSub.Text, NIL, -1, (edtFileVideo.Text = ''));
  // close window
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmProject.btnTermFileClick(Sender: TObject);
begin
  edtTermFile.Text := MyOpenDialog('*.*');
end;

// -----------------------------------------------------------------------------

procedure TfrmProject.btnTMFileClick(Sender: TObject);
begin
  edtTMFile.Text := MyOpenDialog('*.*');
end;

// -----------------------------------------------------------------------------

procedure TfrmProject.edtProjectKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  edtLocation.Text := ProjectsFolder + edtProject.Text + '.swp';
end;

// -----------------------------------------------------------------------------

end.

