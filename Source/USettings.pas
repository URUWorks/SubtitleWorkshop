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

unit USettings;

// -----------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, SysUtils;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnClose: TButton;
    cboLanguage: TComboBox;
    lblLanguage: TLabel;
    pagSettings: TPageControl;
    tabGeneral: TTabSheet;
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure GetLanguageFiles;
  public
    { public declarations }
  end;

var
  frmSettings: TfrmSettings;

// -----------------------------------------------------------------------------

implementation

uses UTypes, UCommon, UWSystem.Globalization;

{$R *.lfm}

var
  Langs: TStringList;

// -----------------------------------------------------------------------------

{ TfrmSettings }

// -----------------------------------------------------------------------------

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  Langs := TStringList.Create;
  ReadLangForForm(LanguageFileName, Self);
  GetLanguageFiles;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmSettings := NIL;
  Langs.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnCloseClick(Sender: TObject);
begin
  Options.Language := Langs[cboLanguage.ItemIndex];
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.GetLanguageFiles;
var
  SearchRec : TSearchRec;
  s: String;
  i: Integer;
begin
  if SysUtils.FindFirst(LanguageFolder + '*.lng', faAnyFile, SearchRec) = 0 then
  try
    repeat
      s := ChangeFileExt(SearchRec.Name, '');
      Langs.Add(s);
      i := cboLanguage.Items.Add(GetCultureDisplayName(AnsiString(s)));
      if Options.Language = s then cboLanguage.ItemIndex := i;
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

// -----------------------------------------------------------------------------

end.

