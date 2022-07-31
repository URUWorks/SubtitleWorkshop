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

unit UInfoAndErrors;

{$mode delphi}
//{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Dialogs, Grids,
  UWSubtitleAPI, LazUTF8, generics.collections, SysUtils, XMLConf;

type

  { TfrmInfoAndErrors }

  TfrmInfoAndErrors = class(TForm)
    btnApply: TButton;
    btnBack: TButton;
    btnClose: TButton;
    cboOCR: TComboBox;
    lblOCR: TLabel;
    pagInfo: TPageControl;
    sgdInfo: TStringGrid;
    sgdError: TStringGrid;
    tabInfo: TTabSheet;
    tabErrors: TTabSheet;
    procedure btnBackClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure pagInfoChange(Sender: TObject);
    procedure sgdInfoSetCheckboxState(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckboxState);
  private
    { private declarations }
    procedure FillInfoGridWithOptions;
    procedure SetRowCaption(const Index: Integer; const AText: String; const IsInfoOrError: Boolean = True);
    procedure LoadCfg;
    procedure SaveCfg;
  public
    { public declarations }
  end;

  TCustomErrorType = record
    Error   : TSubtitleErrorType;
    Enabled : Boolean;
    FixDesc : String;
    Example : String;
  end;

var
  frmInfoAndErrors: TfrmInfoAndErrors;
  ErrorsToFix: array [1..14] of TCustomErrorType;
  igdCheck, igdFix, igdDesc, fgdApply, fgdFunction, fgdBefore, fgdAfter,
  sNext, sApply: String;

// -----------------------------------------------------------------------------

implementation

uses
  UTypes, UCommon, UInfoAndErrorsTypes, UWSystem.TimeUtils, UWSystem.StrUtils,
  UMain;

var
  FixList: TUWSubtitleInfo;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmInfoAndErrors }

// -----------------------------------------------------------------------------

procedure TfrmInfoAndErrors.FormCreate(Sender: TObject);
var
  ts: TStrings;
  i: Integer;
begin
  FillInfoGridWithOptions;
  FillComboWithOCRScripts(cboOCR);
  ReadLangForForm(LanguageFileName, Self);
  btnApply.Caption := sNext;
  LoadCfg;

  SetRowCaption(0, igdCheck);
  SetRowCaption(1, igdFix);
  SetRowCaption(2, igdDesc);
  SetRowCaption(0, fgdApply, False);
  SetRowCaption(2, fgdFunction, False);
  SetRowCaption(3, fgdBefore, False);
  SetRowCaption(4, fgdAfter, False);

  ts := TStringList.Create;
  try
    for i := 1 to Length(ErrorsToFix) do
    begin
      ts.Clear;
      ts.Add(iff(ErrorsToFix[i].Enabled, '1', '0'));
      ts.Add(ErrorsToFix[i].FixDesc);
      ts.Add(ErrorsToFix[i].Example);
      sgdInfo.Rows[i] := ts;
    end;
  finally
    ts.Free;
  end;

  FixList := TUWSubtitleInfo.Create(Subtitles);

  tabErrors.TabVisible := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoAndErrors.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveCfg;
  FixList.Free;

  CloseAction := caFree;
  frmInfoAndErrors := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoAndErrors.pagInfoChange(Sender: TObject);

  procedure SetCtrlsToTab(const Parent: TWinControl; const OnlyCancel: Boolean = False);
  begin
    btnClose.Parent := Parent;
    if not OnlyCancel then
    begin
      btnApply.Parent := Parent;
      btnClose.Parent := Parent;

      case pagInfo.TabIndex of
        0: btnApply.Caption := sNext;
        1: btnApply.Caption := sApply;
      end;
    end;
  end;

begin
  case pagInfo.TabIndex of
    0: SetCtrlsToTab(tabInfo);
    1: SetCtrlsToTab(tabErrors);
  end;
end;

procedure TfrmInfoAndErrors.sgdInfoSetCheckboxState(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckboxState);

  procedure SetError(const AIndex: Integer; const AEnabled: Boolean = True);
  begin
    ErrorsToFix[AIndex].Enabled := AEnabled;
    with (Sender as TStringGrid) do
      Cells[0, AIndex] := iff(AEnabled, '1', '0');
  end;

begin
  if Value = cbChecked then
    SetError(ARow, True)
  else
    SetError(ARow, False)
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoAndErrors.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoAndErrors.btnBackClick(Sender: TObject);
begin
  tabInfo.TabVisible   := True;
  pagInfo.TabIndex     := 0;
  tabErrors.TabVisible := False;
  pagInfoChange(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoAndErrors.btnApplyClick(Sender: TObject);

  function GetErrorStr(const AError: TSubtitleErrorType): String;
  begin
    case AError of
      etUnnecessarySpaces: Result := ErrorsToFix[1].FixDesc;
      etUnnecessaryDots: Result := ErrorsToFix[2].FixDesc;
      etFixTags: Result := ErrorsToFix[3].FixDesc;
      etTimeTooShort: Result := ErrorsToFix[4].FixDesc;
      etTimeTooLong: Result := ErrorsToFix[5].FixDesc;
      etOverlapping: Result := ErrorsToFix[6].FixDesc;
      etBadValues: Result := ErrorsToFix[7].FixDesc;
      etBreakLongLines: Result := ErrorsToFix[8].FixDesc;
      etEmpty: Result := ErrorsToFix[9].FixDesc;
      etProhibitedChars: Result := ErrorsToFix[10].FixDesc;
      etHearingImpaired: Result := ErrorsToFix[11].FixDesc;
      etRepeatedSubtitle: Result := ErrorsToFix[12].FixDesc;
      etRepeatedChars: Result := ErrorsToFix[13].FixDesc;
      etOCR: Result := ErrorsToFix[14].FixDesc;
      //etPauseTooShort: Result := ErrorsToFix[15].FixDesc;
      //etMaxCPS: Result := ErrorsToFix[16].FixDesc;
    end;
  end;

  function IsTimeFixed(const Item: TSubtitleInfoItem): Boolean;
  begin
    with Item do
      if (ErrorsFixed = etOverlapping) or (ErrorsFixed = etBadValues) or
         (ErrorsFixed = etTimeTooShort) or (ErrorsFixed = etTimeTooLong) then
        Result := True
      else
        Result := False;
  end;

  function GetFixedText(const Item: TSubtitleInfoItem): String;
  begin
    with Item do
      if IsTimeFixed(Item) then
        Result := TimeToString(InitialTime, DefTimeFormat) + ' --> ' +
                  TimeToString(FinalTime, DefTimeFormat) // + ' ' + Text
      else
        Result := Text;
  end;

  function GetText(const Index: Integer; const Item: TSubtitleInfoItem): String;
  begin
    with Subtitles[Index] do
      if IsTimeFixed(Item) then
        Result := TimeToString(InitialTime, DefTimeFormat) + ' --> ' +
                  TimeToString(FinalTime, DefTimeFormat) // + ' ' + Text
      else
        Result := Text;
  end;

var
  i: Integer;
  numList: TList<Cardinal>;
  ts: TStrings;
begin
  if pagInfo.TabIndex = 0 then // 1/2
  begin
    tabErrors.TabVisible := True;
    pagInfo.TabIndex     := 1;
    tabInfo.TabVisible   := False;
    pagInfoChange(Sender);

    Application.ProcessMessages;

    FixList.Errors  := [];
    for i := 0 to Length(ErrorsToFix)-1 do
      if ErrorsToFix[i].Enabled then FixList.Errors := FixList.Errors + [ErrorsToFix[i].Error];

    FixList.FixErrors(OCRFolder + cboOCR.Items[cboOCR.ItemIndex] + '.ocr');

    sgdError.RowCount := FixList.Count;
    ts := TStringList.Create;
    try
      for i := 0 to FixList.Count-1 do
      begin
        ts.Clear;
        ts.Add('1'); // chk
        ts.Add(IntToStr(FixList[i].Index+1)); // idx
        ts.Add(GetErrorStr(FixList[i].ErrorsFixed) ); // func
        ts.Add(GetText(FixList[i].Index, FixList[i])); // before
        ts.Add(GetFixedText(FixList[i])); // after
        sgdError.Rows[i] := ts;
      end;
    finally
      ts.Free;
    end;
  end
  else // 2/2
  begin
    if FixList.Count > 0 then
    begin
      numList := TList<Cardinal>.Create;
      try
        numList.Clear;
        // Apply all text/times changes
        for i := 0 to FixList.Count-1 do
          with FixList[i] do
            if Apply then
            begin
              SetSubtitleTime(Index, InitialTime, 0, False, False);
              SetSubtitleTime(Index, FinalTime, 1, False, False);
              SetSubtitleText(Index, Text, True, False, False, False);
              if (ErrorsFixed = etEmpty) or (ErrorsFixed = etProhibitedChars) or (ErrorsFixed = etRepeatedSubtitle) then
                numList.Add(Index);
            end;

        // Remove empty/prohibted lines...
        if numList.Count > 0 then
        begin
          numList.Sort;
          for i := numList.Count-1 downto 0 do DeleteSubtitle(numList[i], False, False);
        end;

        frmMain.UpdateValues;
        IncrementUndoGroup;
      finally
        numList.Free;
      end;
    end;
    Close;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoAndErrors.FillInfoGridWithOptions;

  procedure SetInfoError(var AError: TCustomErrorType; const Error: TSubtitleErrorType; const Enabled: Boolean = True);
  begin
    AError.Error   := Error;
    AError.Enabled := Enabled;
    AError.Example := '';
    AError.FixDesc := '';
  end;

var
  i: Integer;
begin
  sgdInfo.RowCount := Length(ErrorsToFix)+1;
  for i := 1 to Length(ErrorsToFix) do sgdInfo.Cells[0, i] := '0';

  SetInfoError(ErrorsToFix[1], etUnnecessarySpaces);
  SetInfoError(ErrorsToFix[2], etUnnecessaryDots);
  SetInfoError(ErrorsToFix[3], etFixTags);
  SetInfoError(ErrorsToFix[4], etTimeTooShort);
  SetInfoError(ErrorsToFix[5], etTimeTooLong);
  SetInfoError(ErrorsToFix[6], etOverlapping);
  SetInfoError(ErrorsToFix[7], etBadValues);
  SetInfoError(ErrorsToFix[8], etBreakLongLines);
  SetInfoError(ErrorsToFix[9], etEmpty);
  SetInfoError(ErrorsToFix[10], etProhibitedChars);
  SetInfoError(ErrorsToFix[11], etHearingImpaired);
  SetInfoError(ErrorsToFix[12], etRepeatedSubtitle);
  SetInfoError(ErrorsToFix[13], etRepeatedChars);
  SetInfoError(ErrorsToFix[14], etOCR);
  //SetInfoError(ErrorsToFix[15], etPauseTooShort);
  //SetInfoError(ErrorsToFix[16], etMaxCPS);
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoAndErrors.SetRowCaption(const Index: Integer; const AText: String; const IsInfoOrError: Boolean = True);
begin
  if IsInfoOrError then
    sgdInfo.Columns[Index].Title.Caption := AText
  else
    sgdError.Columns[Index].Title.Caption := AText;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoAndErrors.LoadCfg;
var
  i: Integer;
begin
  with TXMLConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    OpenKey('InfoAndErrors');

    for i := 1 to Length(ErrorsToFix) do
      ErrorsToFix[i].Enabled := GetValue('Fix' + IntToStr(i), True);

    CloseKey;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoAndErrors.SaveCfg;
var
  i: Integer;
begin
  with TXMLConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    //Clear;
    //RootName := 'SW';
    OpenKey('InfoAndErrors');

    for i := 1 to Length(ErrorsToFix) do
      SetValue('Fix' + IntToStr(i), ErrorsToFix[i].Enabled);

    CloseKey;

    Flush;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

end.

