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

unit USpellCheck;

// -----------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, fgl, character, Dialogs, LazUTF8;

type

  { TFPGMaps }

  TWordMap        = specialize TFPGMap<Integer, String>;
  TWordReplaceMap = specialize TFPGMap<String, String>;

  { TfrmSpellCheck }

  TfrmSpellCheck = class(TForm)
    btnSkip: TButton;
    btnSkipAll: TButton;
    btnAdd: TButton;
    btnChange: TButton;
    btnChangeAll: TButton;
    btnClose: TButton;
    cboDictionary: TComboBox;
    edtCustom: TEdit;
    lblSuggestions: TLabel;
    lblNotFound: TLabel;
    lblDictionary: TLabel;
    lstSuggestions: TListBox;
    mmoNotFound: TMemo;
    procedure btnAddClick(Sender: TObject);
    procedure btnChangeAllClick(Sender: TObject);
    procedure btnChangeClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnSkipAllClick(Sender: TObject);
    procedure btnSkipClick(Sender: TObject);
    procedure cboDictionaryChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure lstSuggestionsSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
    procedure PopulateWordMap(const AText: String);
    procedure InitializeSpellAtLine(const ALine: Integer = 0);
    procedure DoSpell;
  public
    { public declarations }
  end;

var
  frmSpellCheck: TfrmSpellCheck;
  strFinished: String;

// -----------------------------------------------------------------------------

implementation

uses UCommon, UTypes, UWSystem.StrUtils, UMain;

{$R *.lfm}

var
  FWordMap    : TWordMap;
  FLastPos    : Integer;
  FLastLine   : Integer;
  FSkipList   : TStrings;
  FTempList   : TStrings;
  FChangeList : TWordReplaceMap;

// -----------------------------------------------------------------------------

{ TfrmSpellCheck }

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.FormCreate(Sender: TObject);
begin
  FWordMap    := TWordMap.Create;
  FSkipList   := TStringList.Create;
  FTempList   := TStringList.Create;
  FChangeList := TWordReplaceMap.Create;

  FillWithDictionaries(NIL, cboDictionary);
  ReadLangForForm(LanguageFileName, Self);

  InitializeSpellAtLine;
  DoSpell;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  i: Integer;
begin
  CloseAction := caFree;
  FWordMap.Free;
  FSkipList.Free;
  FTempList.Free;
  FChangeList.Free;

  for i := 0 to frmMain.mnuDictionary.Count-1 do
    if frmMain.mnuDictionary.Items[i].Caption = cboDictionary.Text then
      frmMain.mnuDictionary.Items[i].Checked := True
    else
      frmMain.mnuDictionary.Items[i].Checked := False;

  frmSpellCheck := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.PopulateWordMap(const AText: String);
var
  SL        : TStrings;
  LineText,
  s         : String;
  InitPos,
  EndPos, i : Integer;
  l         : Integer;
begin
  FWordMap.Clear;
  if AText = '' then Exit;

  SL := TStringList.Create;
  try
    SL.Text := AText;
    l := 0;
    for i := 0 to SL.Count-1 do
    begin
      LineText := SL[i];
      if (i >= 0) and (i < Pred(SL.Count)) then
        l := l + UTF8Length(LineText) + UTF8Length(sLineBreak);

      InitPos  := 1;
      EndPos   := 0;

      if LineText <> '' then
        while (EndPos < UTF8Length(LineText)) do
        begin
          Inc(EndPos);

          if not TCharacter.IsLetterOrDigit(LineText, EndPos) or (EndPos = UTF8Length(LineText)) then
          begin
            s := UTF8Trim(UTF8Copy(LineText, InitPos, EndPos-InitPos));

            if s <> '' then
            begin
              if (i > 0) and (i <= Pred(SL.Count)) then
                FWordMap.Add(InitPos + l, s)
              else
                FWordMap.Add(InitPos, s);
            end;
            InitPos := EndPos+1;
          end;
        end;
    end;
  finally
    SL.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.InitializeSpellAtLine(const ALine: Integer = 0);
begin
  btnChange.Enabled    := False;
  btnChangeAll.Enabled := False;

  FTempList.Clear;
  FLastPos  := 0;
  FLastLine := ALine;

  if ALine < (Subtitles.Count-1) then
  begin
    PopulateWordMap(Subtitles.Text[ALine]);
    mmoNotFound.Text := Subtitles.Text[ALine];
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.DoSpell;
var
  Suggests: TStrings;
  i, x: Integer;
begin
  if FLastLine < (Subtitles.Count-1) then
  begin
    Suggests := NIL;
    lstSuggestions.Clear;

    for i := 0 to FWordMap.Count-1 do
    begin
      if (FLastPos <> FWordMap.Keys[i]) and not Hunspell.Spell(FWordMap.Data[i]) and
        (FSkipList.IndexOf(FWordMap.Data[i]) < 0) and (FTempList.IndexOf(FWordMap.Data[i]) < 0) then
      begin
        x := FChangeList.IndexOf(FWordMap.Data[i]);
        if x >= 0 then
        begin
          SetSubtitleText(FLastLine, ReplaceString(Subtitles.Text[FLastLine], FChangeList.Keys[x], FChangeList.Data[x], True, False), True, False);
        end
        else
        begin
          FLastPos := FWordMap.Keys[i];
          mmoNotFound.SelStart  := FLastPos-1;
          mmoNotFound.SelLength := UTF8Length(FWordMap.Data[i]);

          if Hunspell.Suggest(FWordMap.Data[i], Suggests) then
          begin
            lstSuggestions.Items.Assign(Suggests);
            lstSuggestions.ItemIndex := 0;
          end;

          btnChange.Enabled    := lstSuggestions.Count > 0;
          btnChangeAll.Enabled := btnChange.Enabled;

          Exit;
        end;
      end;
    end;
    InitializeSpellAtLine(FLastLine + 1);
    DoSpell;
  end
  else
  begin
    edtCustom.Text := '';
    ShowMessage(strFinished);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.btnSkipClick(Sender: TObject);
begin
  FTempList.Add(mmoNotFound.SelText);
  DoSpell;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.btnSkipAllClick(Sender: TObject);
begin
  if (mmoNotFound.SelText <> '') and (FSkipList.IndexOf(mmoNotFound.SelText) < 0) then
    FSkipList.Add(mmoNotFound.SelText);

  DoSpell;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.btnAddClick(Sender: TObject);
begin
  if (mmoNotFound.SelText <> '') then
    Hunspell.Add(mmoNotFound.SelText);

  DoSpell;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.btnChangeClick(Sender: TObject);
begin
  if (lstSuggestions.Count > 0) and (mmoNotFound.SelText <> '') then
    SetSubtitleText(FLastLine, ReplaceString(Subtitles.Text[FLastLine], mmoNotFound.SelText, edtCustom.Text, True, False), True, False);

  DoSpell;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.btnChangeAllClick(Sender: TObject);
begin
  if (lstSuggestions.Count > 0) and (mmoNotFound.SelText <> '') then
  begin
    SetSubtitleText(FLastLine, ReplaceString(Subtitles.Text[FLastLine], mmoNotFound.SelText, edtCustom.Text, True, False), True, False);
    FChangeList.Add(mmoNotFound.SelText, edtCustom.Text);
  end;

  DoSpell;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.cboDictionaryChange(Sender: TObject);
begin
  if (cboDictionary.Items.Count > 0) then
  begin
    Options.HunspellLang := GetDictNameFromCaption(cboDictionary.Text);
    Hunspell.LoadDictionary(DictionariesFolder+Options.HunspellLang+'.aff', DictionariesFolder+Options.HunspellLang+'.dic');

    InitializeSpellAtLine;
    DoSpell;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.lstSuggestionsSelectionChange(Sender: TObject;
  User: boolean);
begin
  edtCustom.Text := lstSuggestions.Items[lstSuggestions.ItemIndex];
end;

// -----------------------------------------------------------------------------

end.

