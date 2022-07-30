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

unit UTexts;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Dialogs, UWControls,
  UWTranslateAPI.Google, UWSystem.InetUtils, LazUTF8;

type

  { TfrmTexts }

  TfrmTexts = class(TForm)
    Bevel1: TBevel;
    btnApply: TButton;
    btnMoreLess: TButton;
    btnClose: TButton;
    cboSourceLanguage: TComboBox;
    cboTranslationLanguage: TComboBox;
    cboInput: TComboBox;
    cboOutput: TComboBox;
    lblApplyIn: TLabel;
    lblInput: TLabel;
    lblOutput: TLabel;
    lblSourceLanguage: TLabel;
    lblTranslationLanguage: TLabel;
    pagTexts: TPageControl;
    prbTranslate: TProgressBar;
    rdoSentenceType: TRadioButton;
    rdoLowercase: TRadioButton;
    rdoUppercase: TRadioButton;
    rdoTitleType: TRadioButton;
    rdoInverseType: TRadioButton;
    rdoAllTheSubtitles: TRadioButton;
    rdoOnlySelectedSubtitles: TRadioButton;
    rdoFromTheSelectedSubtitle: TRadioButton;
    tabConvertCase: TTabSheet;
    tabTranslate: TTabSheet;
    lyoOptions: TUWLayout;
    procedure btnCloseClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnMoreLessClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure pagTextsChange(Sender: TObject);
  private
    { private declarations }
    procedure FillComboWithGLangs(const Combo: TCombobox; const Index: Integer = 0);
  public
    { public declarations }
  end;

var
  frmTexts: TfrmTexts;
  strLess, strMore, strOrig, strTrans, strCancel, strClose, strNoInternet: String;
  CancelTranslation: Boolean;

// -----------------------------------------------------------------------------

implementation

uses UTypes, UCommon, UMain, UWSubtitleAPI, UWSystem.StrUtils;

{$R *.lfm}

const
  _more = 311;
  _less = 208;

// -----------------------------------------------------------------------------

procedure ApplyConvertCase(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with frmTexts, Item^ do
  begin
    if rdoSentenceType.Checked then
      ApplyText(Item, Index, SentenceCase(Text), SentenceCase(Translation), False)
    else if rdoLowercase.Checked then
      ApplyText(Item, Index, UTF8LowerCase(Text), UTF8LowerCase(Translation), False)
    else if rdoUppercase.Checked then
      ApplyText(Item, Index, UTF8UpperCase(Text), UTF8UpperCase(Translation), False)
    else if rdoTitleType.Checked then
      ApplyText(Item, Index, TitleCase(Text), TitleCase(Translation), False)
    else
      ApplyText(Item, Index, InvertCase(Text), InvertCase(Translation), False);
  end;
end;

// -----------------------------------------------------------------------------

procedure ApplyGoogleTranslate(const Item: PUWSubtitleItem; const Index: Integer);
var
  s1, s2: String;
begin
  with frmTexts, Item^ do
  begin
    case cboInput.ItemIndex of
      0: s1 := Text;
      1: s1 := Translation;
    end;

    s2 := GoogleTranslateText(s1, GoogleTranslateLocale[cboSourceLanguage.ItemIndex],
      GoogleTranslateLocale[cboTranslationLanguage.ItemIndex]);

    case cboOutput.ItemIndex of
      0: Text        := s2;
      1: Translation := s2;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure CheckCBState(const CurrentItem, TotalCount: Integer; var Cancel: Boolean);
begin
  with frmTexts.prbTranslate do
  begin
    Max      := TotalCount;
    Position := CurrentItem;
  end;
  Cancel := CancelTranslation;
end;

// -----------------------------------------------------------------------------

{ TfrmTexts }

// -----------------------------------------------------------------------------

procedure TfrmTexts.FormCreate(Sender: TObject);
begin
  Height := _Less;

  if frmMain.VST.SelectedCount >= 1 then
    rdoOnlySelectedSubtitles.Checked := True
  else
    rdoAllTheSubtitles.Checked := True;

  FillComboWithGLangs(cboSourceLanguage);
  FillComboWithGLangs(cboTranslationLanguage, 45);
  ReadLangForForm(LanguageFileName, Self);

  cboInput.Items.Add('Original');
  cboInput.Items.Add('Translation');
  cboOutput.Items.Assign(cboInput.Items);
  cboInput.ItemIndex  := 0;
  cboOutput.ItemIndex := 1;

  strClose := btnClose.Caption;
end;

// -----------------------------------------------------------------------------

procedure TfrmTexts.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmTexts := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmTexts.pagTextsChange(Sender: TObject);

  procedure SetCtrlsToTab(const Parent: TWinControl; const OnlyCancel: Boolean = False);
  begin
    btnClose.Parent := Parent;
    if not OnlyCancel then
    begin
      btnMoreLess.Parent := Parent;
      lyoOptions.Parent  := Parent;
      btnApply.Parent    := Parent;

      if Self.Visible then
        case pagTexts.TabIndex of
          0: rdoSentenceType.SetFocus;
          1: cboSourceLanguage.SetFocus;
      end;
    end;
  end;

begin
  case pagTexts.TabIndex of
    0: SetCtrlsToTab(tabConvertCase);
    1: SetCtrlsToTab(tabTranslate);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTexts.btnCloseClick(Sender: TObject);
begin
  if btnApply.Enabled then
    Close
  else
    CancelTranslation := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmTexts.btnMoreLessClick(Sender: TObject);
begin
  with btnMoreLess do
    if Tag = 0 then
    begin
      Tag  := 1;
      Text := strLess;
      Self.Height := _more;
    end
    else
    begin
      Tag  := 0;
      Text := strMore;
      Self.Height := _less;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTexts.btnApplyClick(Sender: TObject);
var
  SelLoop: TUWSubtitleDoLoopSelection;
begin
  SelLoop := TUWSubtitleDoLoopSelection(iff(rdoAllTheSubtitles.Checked, 0, iff(rdoOnlySelectedSubtitles.Checked, 1, 2)));
  case pagTexts.TabIndex of
    0: VSTDoLoop(@ApplyConvertCase, SelLoop, True, True);
    else
    begin
      if IsInternetAlive then
      begin
        CancelTranslation := False;
        btnApply.Enabled  := False;
        btnClose.Caption  := strCancel;
        VSTDoLoop(@ApplyGoogleTranslate, SelLoop, True, True, @CheckCBState);
        btnClose.Caption  := strClose;
        btnApply.Enabled  := True;
      end
      else
        ShowMessage(strNoInternet);
    end;
  end;
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmTexts.FillComboWithGLangs(const Combo: TCombobox; const Index: Integer = 0);
var
  i: Integer;
begin
  for i := 0 to Length(GoogleTranslateName)-1 do
    Combo.Items.Add(GoogleTranslateName[i]);

  Combo.ItemIndex := Index;
end;

// -----------------------------------------------------------------------------

end.

