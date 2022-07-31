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

unit UTimings;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Dialogs, UWControls,
  UWSubtitles.Utils;

type

  { TfrmTimings }

  TfrmTimings = class(TForm)
    Bevel1: TBevel;
    btnApply: TButton;
    btnMoreLess: TButton;
    btnClose: TButton;
    cboExpand: TComboBox;
    cboDelay: TComboBox;
    chkMaxDuration: TCheckBox;
    chkOnlyIfSubLonger: TCheckBox;
    chkMinDuration: TCheckBox;
    cboApplyIf: TComboBox;
    chkOnlyIfDurationLonger: TCheckBox;
    lblPerChar: TLabel;
    lblApplyIf: TLabel;
    lblApplyIn: TLabel;
    lblTimeChar: TLabel;
    lblPerWord: TLabel;
    lblPerLine: TLabel;
    lblTimeMS: TLabel;
    numTimeChars: TUWNumberBox;
    numMaxDuration: TUWNumberBox;
    numPerChar: TUWNumberBox;
    numExpand: TUWNumberBox;
    numPerWord: TUWNumberBox;
    numPerLine: TUWNumberBox;
    numMinDuration: TUWNumberBox;
    numTimeMS: TUWNumberBox;
    pagTimings: TPageControl;
    rdoAllTheSubtitles: TRadioButton;
    rdoOnlySelectedSubtitles: TRadioButton;
    rdoFromTheSelectedSubtitle: TRadioButton;
    tabDurationLimits: TTabSheet;
    tabAutomaticDurations: TTabSheet;
    lyoOptions: TUWLayout;
    tabDelay: TTabSheet;
    tabTime: TTabSheet;
    tedDelay: TUWTimeEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnMoreLessClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure pagTimingsChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmTimings: TfrmTimings;
  strLess, strMore,
  strNewAll, strNewGreater, strNewSmaller,
  strExpand, strReduce: String;

// -----------------------------------------------------------------------------

implementation

uses UTypes, UCommon, UMain, UWSubtitleAPI, UWSystem.StrUtils;

{$R *.lfm}

const
  _more = 324;
  _less = 221;

// -----------------------------------------------------------------------------

procedure ApplyTimings(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^, frmTimings do
    case pagTimings.TabIndex of
      0: begin //Limits
           SetSubtitleTime(Index, InitialTime + SetDurationLimits(GetDurationTime(Index), numMaxDuration.Value, numMinDuration.Value),
           1, False, False);
         end;
      1: begin //Automatic
           SetSubtitleTime(Index, InitialTime + AutomaticDurations(Text, GetDurationTime(Index), numPerChar.Value, numPerWord.Value, numPerLine.Value, TAutomaticDurationMode(cboApplyIf.ItemIndex)),
           1, False, False);
         end;
      2: begin //Expander
           SetSubtitleTime(Index, InitialTime + TimeExpander(Text, GetDurationTime(Index), numExpand.Value, iff(chkOnlyIfSubLonger.Checked, numTimeChars.Value, 0), iff(chkOnlyIfDurationLonger.Checked, numTimeMS.Value, 0), cboExpand.ItemIndex = 0),
           1, False, False);
         end;
      3: begin //Delay
           SetSubtitleTime(Index, SetDelay(InitialTime, iff(cboDelay.ItemIndex = 0, tedDelay.Value, -tedDelay.Value)),
           0, False, False);
           SetSubtitleTime(Index, SetDelay(FinalTime, iff(cboDelay.ItemIndex = 0, tedDelay.Value, -tedDelay.Value)),
           1, False, False);
         end;
    end;
end;

// -----------------------------------------------------------------------------

{ TfrmTimings }

// -----------------------------------------------------------------------------

procedure TfrmTimings.FormCreate(Sender: TObject);
begin
  Height := _Less;

  if frmMain.VST.SelectedCount >= 1 then
    rdoOnlySelectedSubtitles.Checked := True
  else
    rdoAllTheSubtitles.Checked := True;

  ReadLangForForm(LanguageFileName, Self);

  cboApplyIf.Items.Add(strNewAll);
  cboApplyIf.Items.Add(strNewGreater);
  cboApplyIf.Items.Add(strNewSmaller);
  cboApplyIf.ItemIndex := 1;
  cboExpand.Items.Add(strExpand);
  cboExpand.Items.Add(strReduce);
  cboExpand.ItemIndex := 0;
  cboDelay.Items.Add('+');
  cboDelay.Items.Add('-');
  cboDelay.ItemIndex := 0;

  numMaxDuration.Value := Options.ErrCfg.MaxDuration;
  numMinDuration.Value := Options.ErrCfg.MinDuration;
  numPerChar.Value     := 60;
  numPerWord.Value     := 50;
  numPerLine.Value     := 50;
  numExpand.Value      := 1500;
  numTimeChars.Value   := 40;
  numTimeMS.Value      := 1000;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimings.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmTimings := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimings.pagTimingsChange(Sender: TObject);

  procedure SetCtrlsToTab(const Parent: TWinControl; const OnlyCancel: Boolean = False);
  begin
    btnClose.Parent := Parent;
    if not OnlyCancel then
    begin
      btnMoreLess.Parent := Parent;
      lyoOptions.Parent  := Parent;
      btnApply.Parent    := Parent;

      if Self.Visible then
        case pagTimings.TabIndex of
          0: numMaxDuration.SetFocus;
          1: cboApplyIf.SetFocus;
          2: numExpand.SetFocus;
          3: tedDelay.SetFocus;
      end;
    end;
  end;

begin
  case pagTimings.TabIndex of
    0: SetCtrlsToTab(tabDurationLimits);
    1: SetCtrlsToTab(tabAutomaticDurations);
    2: SetCtrlsToTab(tabTime);
    3: SetCtrlsToTab(tabDelay);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimings.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimings.btnMoreLessClick(Sender: TObject);
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

procedure TfrmTimings.btnApplyClick(Sender: TObject);
var
  SelLoop: TUWSubtitleDoLoopSelection;
begin
  SelLoop := TUWSubtitleDoLoopSelection(iff(rdoAllTheSubtitles.Checked, 0, iff(rdoOnlySelectedSubtitles.Checked, 1, 2)));
  VSTDoLoop(@ApplyTimings, SelLoop, True, True);
  IncrementUndoGroup;
  SubtitleChanged(True, True);
  frmMain.UpdateValues(True);
  Close;
end;

// -----------------------------------------------------------------------------

end.

