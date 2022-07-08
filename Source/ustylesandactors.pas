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

unit UStylesAndActors;

{$mode objfpc}{$H+}
//{$mode delphi}

interface

uses
  Classes, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Dialogs, LCLType,
  UWControls, UWCustom.SWStyles, SysUtils, Graphics;

type

  { TfrmStylesAndActors }

  TfrmStylesAndActors = class(TForm)
    btnClose: TButton;
    btnApply: TButton;
    btnOk: TButton;
    btnStyleAdd: TButton;
    btnStyleRemove: TButton;
    btnActorAdd: TButton;
    btnActorRemove: TButton;
    chkFontBold: TCheckBox;
    chkFontItalic: TCheckBox;
    chkFontUnderline: TCheckBox;
    chkFontStrikeout: TCheckBox;
    cbnPrimary: TColorButton;
    cbnSecondary: TColorButton;
    cbnOutline: TColorButton;
    cbnShadow: TColorButton;
    cboStyle: TComboBox;
    cboFonts: TComboBox;
    edtActor: TEdit;
    grpFont: TGroupBox;
    grpColors: TGroupBox;
    grpMargins: TGroupBox;
    grpAlign: TGroupBox;
    grpMisc: TGroupBox;
    grpPreview: TGroupBox;
    lblColorPrimary: TLabel;
    lblMiscRotation: TLabel;
    lblMiscSpacing: TLabel;
    lblMiscScaleY: TLabel;
    lblMiscShadow: TLabel;
    lblColorSecondary: TLabel;
    lblColorOutline: TLabel;
    lblColorShadow: TLabel;
    lblMarginLeft: TLabel;
    lblMarginRight: TLabel;
    lblMarginVertical: TLabel;
    lblMiscOutline: TLabel;
    lblMiscScaleX: TLabel;
    lstActors: TListBox;
    pagStylesAndActors: TPageControl;
    rdo7: TRadioButton;
    rdo8: TRadioButton;
    rdo9: TRadioButton;
    rdo4: TRadioButton;
    rdo1: TRadioButton;
    rdo5: TRadioButton;
    rdo2: TRadioButton;
    rdo6: TRadioButton;
    rdo3: TRadioButton;
    tabStyles: TTabSheet;
    tabActors: TTabSheet;
    numFontSize: TUWNumberBox;
    numMiscSpacing: TUWNumberBox;
    numMarginLeft: TUWNumberBox;
    numMarginRight: TUWNumberBox;
    numMarginVertical: TUWNumberBox;
    numMiscOutline: TUWNumberBox;
    numMiscScaleX: TUWNumberBox;
    numMiscRotation: TUWNumberBox;
    numMiscShadow: TUWNumberBox;
    numMiscScaleY: TUWNumberBox;
    procedure btnActorAddClick(Sender: TObject);
    procedure btnActorRemoveClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnStyleAddClick(Sender: TObject);
    procedure btnStyleRemoveClick(Sender: TObject);
    procedure cboFontsChange(Sender: TObject);
    procedure cboStyleSelect(Sender: TObject);
    procedure edtActorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    function GetAlignValue: Integer;
    procedure SetAlignValue(const Value: Integer);
    procedure UpdateStyleItem(const AIndex: Integer);
  public
    { public declarations }
  end;

var
  frmStylesAndActors: TfrmStylesAndActors;
  strNewStyle, strNewStyleName: String;

// -----------------------------------------------------------------------------

implementation

uses UTypes, UCommon, UMain;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmStylesAndActors }

// -----------------------------------------------------------------------------

procedure TfrmStylesAndActors.FormCreate(Sender: TObject);
begin
  cboFonts.Items.Assign(Screen.Fonts);
  cboFonts.ItemIndex := 0;

  cboStyle.Tag := 2;
  cboStyle.Items.Assign(frmMain.cboStyle.Items);
  cboStyle.Tag := 0;

  lstActors.Items.Assign(frmMain.cboActor.Items);

  ReadLangForForm(LanguageFileName, Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmStylesAndActors.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmStylesAndActors := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmStylesAndActors.btnCloseClick(Sender: TObject);
begin
  btnApply.Click;
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmStylesAndActors.btnOkClick(Sender: TObject);
begin
  btnApply.Click;
  btnClose.Click;
end;

// -----------------------------------------------------------------------------

procedure TfrmStylesAndActors.btnActorAddClick(Sender: TObject);
begin
  if (edtActor.Text <> '') and (lstActors.Items.IndexOf(edtActor.Text) < 0) then
  begin
    lstActors.Items.Add(edtActor.Text);
    edtActor.Text := '';
    edtActor.SetFocus;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmStylesAndActors.btnActorRemoveClick(Sender: TObject);
begin
  if lstActors.ItemIndex >= 0 then
    lstActors.Items.Delete(lstActors.ItemIndex);
end;

// -----------------------------------------------------------------------------

procedure TfrmStylesAndActors.btnApplyClick(Sender: TObject);
begin
  frmMain.cboStyle.Items.Assign(cboStyle.Items);
  frmMain.cboActor.Items.Assign(lstActors.Items);
  //Styles.SaveToFile;
  //btnClose.Click;
end;

// -----------------------------------------------------------------------------

procedure TfrmStylesAndActors.btnStyleAddClick(Sender: TObject);
var
  s: String;
  i: Integer;
  sItem: TStyleItem;
begin
  s := InputBox(strNewStyle, strNewStyleName, '');
  if (s <> '') and (cboStyle.Items.IndexOf(s) < 0) then
  begin
    // add new style
    cboStyle.Tag := 2;
    i := cboStyle.Items.Add(s);
    cboStyle.ItemIndex := i;
    Styles.ClearItem(sItem);
    Styles.ItemList.Add(sItem);
    cboStyle.Tag := 0;
    UpdateStyleItem(i);
    cboStyle.SetFocus;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmStylesAndActors.btnStyleRemoveClick(Sender: TObject);
begin
  if cboStyle.ItemIndex >= 0 then
  begin
    cboStyle.Items.Delete(cboStyle.ItemIndex);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmStylesAndActors.cboFontsChange(Sender: TObject);
begin
  if cboStyle.ItemIndex >= 0 then UpdateStyleItem(cboStyle.ItemIndex);
end;

// -----------------------------------------------------------------------------

procedure TfrmStylesAndActors.cboStyleSelect(Sender: TObject);
begin
  if cboStyle.Tag = 2 then Exit;

  if cboStyle.ItemIndex >= 0 then
  with Styles.Items[cboStyle.ItemIndex] do
  begin
    cboStyle.Tag := 2;
    cboFonts.ItemIndex := cboFonts.Items.IndexOf(FontName);
    numFontSize.Value := FontSize;
    chkFontBold.Checked := Bold;
    chkFontItalic.Checked := Italic;
    chkFontUnderline.Checked := Underline;
    chkFontStrikeout.Checked := Strikeout;
    cbnPrimary.ButtonColor := TColor(ColorPrimary);
    cbnSecondary.ButtonColor := TColor(ColorSec);
    cbnOutline.ButtonColor := TColor(ColorOutline);
    cbnShadow.ButtonColor := TColor(ColorShadow);
    numMiscOutline.Value := Outline;
    numMiscShadow.Value := Shadow;
    numMiscScaleX.Value := ScaleX;
    numMiscScaleY.Value := ScaleY;
    numMiscRotation.Value := Rotation;
    numMiscSpacing.Value := Spacing;
    numMarginLeft.Value := MarginLeft;
    numMarginRight.Value := MarginRight;
    numMarginVertical.Value := MarginVert;
    SetAlignValue(Align);
    cboStyle.Tag := 0;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmStylesAndActors.edtActorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    btnActorAdd.Click;
    Key := 0;
  end;
end;

// -----------------------------------------------------------------------------

function TfrmStylesAndActors.GetAlignValue: Integer;
begin
  if rdo1.Checked then
    Result := 1
  else if rdo2.Checked then
    Result := 2
  else if rdo3.Checked then
    Result := 3
  else if rdo4.Checked then
    Result := 4
  else if rdo5.Checked then
    Result := 5
  else if rdo6.Checked then
    Result := 6
  else if rdo7.Checked then
    Result := 7
  else if rdo8.Checked then
    Result := 8
  else
    Result := 9
end;

// -----------------------------------------------------------------------------

procedure TfrmStylesAndActors.SetAlignValue(const Value: Integer);
begin
  case Value of
    1: rdo1.Checked := True;
    3: rdo3.Checked := True;
    4: rdo4.Checked := True;
    5: rdo5.Checked := True;
    6: rdo6.Checked := True;
    7: rdo7.Checked := True;
    8: rdo8.Checked := True;
    9: rdo9.Checked := True;
  else
    rdo2.Checked := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmStylesAndActors.UpdateStyleItem(const AIndex: Integer);
var
  item: TStyleItem;
begin
  if cboStyle.Tag <> 0 then Exit;

  with item do
  begin
    Title := cboStyle.Text;
    FontName := cboFonts.Text;
    FontSize := numFontSize.Value;
    Bold := chkFontBold.Checked;
    Italic := chkFontItalic.Checked;
    Underline := chkFontUnderline.Checked;
    Strikeout := chkFontStrikeout.Checked;
    ColorPrimary := Integer(cbnPrimary.ButtonColor);
    ColorSec := Integer(cbnSecondary.ButtonColor);
    ColorOutline := Integer(cbnOutline.ButtonColor);
    ColorShadow := Integer(cbnShadow.ButtonColor);
    Outline := numMiscOutline.Value;
    Shadow := numMiscShadow.Value;
    ScaleX := numMiscScaleX.Value;
    ScaleY := numMiscScaleY.Value;
    Rotation := numMiscRotation.Value;
    Spacing := numMiscSpacing.Value;
    MarginLeft := numMarginLeft.Value;
    MarginRight := numMarginRight.Value;
    MarginVert := numMarginVertical.Value;
    Align := GetAlignValue;
  end;

  Styles[AIndex] := item;
end;

// -----------------------------------------------------------------------------

end.

