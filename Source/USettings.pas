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
  Classes, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnClose: TButton;
    pagSettings: TPageControl;
    tabGeneral: TTabSheet;
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure FillComboWithLangs(const Combo: TCombobox; const Index: Integer = 0);
  public
    { public declarations }
  end;

var
  frmSettings: TfrmSettings;

// -----------------------------------------------------------------------------

implementation

uses UTypes, UCommon;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmSettings }

// -----------------------------------------------------------------------------

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  ReadLangForForm(LanguageFileName, Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmSettings := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.FillComboWithLangs(const Combo: TCombobox; const Index: Integer = 0);
//var
//  i: Integer;
begin
//  for i := 0 to Length(GoogleTranslateName)-1 do
//    Combo.Items.Add(GoogleTranslateName[i]);

//  Combo.ItemIndex := Index;
end;

// -----------------------------------------------------------------------------

end.

