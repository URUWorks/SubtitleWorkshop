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

unit UAbout;

// -----------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, StdCtrls, LCLIntf, SysUtils;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnClose: TButton;
    lblDonate: TLabel;
    lblInfo: TLabel;
    lblSW: TLabel;
    lblUW: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure lblDonateClick(Sender: TObject);
    procedure lblUWClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

// -----------------------------------------------------------------------------

implementation

uses UTypes, UCommon;

{$R *.lfm}

const
  cDonateURL = 'https://www.paypal.com/cgi-bin/webscr?cmd=_xclick&business=uruwo' +
               'rks%40gmail%2ecom&item_name=URUWorks%20Subtitle%20Workshop&no_shipping=0&no_note=1&tax' +
               '=0&currency_code=USD&lc=US&bn=PP%2dDonationsBF&charset=UTF%2d8'; //&amount=25

// -----------------------------------------------------------------------------

{ TfrmAbout }

// -----------------------------------------------------------------------------

procedure TfrmAbout.FormCreate(Sender: TObject);

  function BuildNumber: String;
  var
    v: Integer;
  begin
    v := ProgramVer - (ProgramVer div 10 * 10);
    if v > 0 then
      Result := 'r' + IntToStr(v)
    else
      Result := '';
  end;

const
  cInfo = 'Compiled at ' + {$I %TIME%} + ' on ' + {$I %DATE%} + sLineBreak +
          'Compiler version: ' + {$I %FPCVERSION%} + sLineBreak +
          'Target CPU: ' + {$I %FPCTARGET%};
  cUW   = 'uruworks.net' + sLineBreak +
          'Copyright © 2001-2022 URUWorks.' + sLineBreak +
          '© 2022 Bedazzle, CM630.';
begin
  lblSW.Caption := Format('%s %s %s', [ProgramName,
    Format('%.2f', [ProgramVer / 1000], FormatSettings), BuildNumber]);

  lblUW.Caption   := cUW;
  lblInfo.Caption := cInfo;

  ReadLangForForm(LanguageFileName, Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmAbout := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.lblDonateClick(Sender: TObject);
begin
  OpenURL(cDonateURL);
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.lblUWClick(Sender: TObject);
begin
  OpenURL(ProgramWebsite);
end;

// -----------------------------------------------------------------------------

end.
