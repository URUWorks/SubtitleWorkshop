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
 *  Copyright (C) 2001-2016 Aldo Lacavalla, URUWorks.
 *}

unit UCommon;

// -----------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Types, VirtualTrees, Graphics, StdCtrls, Forms, ActnList,
  Menus, LCLProc, fpjson, jsonparser, LCLIntf, XMLConf, UTypes, UWControls,
  UWSubtitleAPI, LazUTF8, FileUtil, Controls
  {$IFDEF WINDOWS}, registry{$ENDIF};

function GetCustomFolderPath(const SubFolder: String): String;
function GetCustomFilePath(const FileName: String): String;
function SettingsFileName: String;
function LanguageFolder: String;
function LanguageFileName: String;
function ShortCutFileName: String;
function OCRFolder: String;
function ExtensionsFolder: String;
function WaveformsFolder: String;
function MRUFileName: String;
function StylesFileName: String;
function ActorsFileName: String;
function DictionariesFolder: String;
function ProjectsFolder: String;
function TMFolder: String;
function TerminologyFolder: String;

function MediaFileExists(const FileName: String; const Exts: array of String): String;

procedure ReadLangForForm(const FileName: String; const Form: TForm);
procedure ReadShortCuts(const FileName: String);

procedure DefaultSettings;
procedure LoadSettings;
procedure SaveSettings;
procedure CommandLineProcess;

function MsgSaveSubtitle(const FileName: String): Integer;

function GetInitialTime(const Index: Integer): Integer;
function GetInitialTimeStr(const Index: Integer; const Trim: Boolean = False; const Format: String = DefTimeFormat): String;
function GetFinalTime(const Index: Integer): Integer;
function GetFinalTimeStr(const Index: Integer; const Trim: Boolean = False; const Format: String = DefTimeFormat): String;
function GetDurationTime(const Index: Integer): Integer;
function GetDurationTimeStr(const Index: Integer; const Trim: Boolean = False; const Format: String = DefTimeFormat): String;
function GetPauseTime(const Index: Integer): Integer;
function GetPauseTimeStr(const Index: Integer; const Trim: Boolean = False; const Format: String = DefTimeFormat): String;

function GetInputFPS: Single;
function GetFPS: Single;

function CorrectTime(const Time: Integer): Integer;
procedure SetSubtitleTime(const Index: Integer; const Time: Integer; const Tag: Integer; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);
procedure SetSubtitleText(const Index: Integer; const Text: String; const IsOriginal: Boolean = True; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True; const Resent: Boolean = True);

function InsertSubtitle(const Index: Integer; const Item: TUWSubtitleItem; const AutoIncrementUndo: Boolean = True; const AUpdate: Boolean = True): Integer; overload;
function InsertSubtitle(const Index: Integer; const InitialTime, FinalTime: Integer; const Text, Translation: String; const AutoIncrementUndo: Boolean = True; const AUpdate: Boolean = True): Integer; overload;
procedure DeleteSubtitle(const Index: Integer; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);

procedure SubtitleChanged(const AText, ATranslation: Boolean);

function GetSubtitleTextAtTime(const MSecs: Cardinal): String;

function GetLengthForEachLine(Text: String; const Separator: String = sLineBreak; const LastSeparator: String = sLineBreak): String;

procedure FocusMemo(const SelectText: Boolean = True);
procedure SelectSubtitleAndFocusMemo(const NextSibiling: Boolean);
function GetMemoFocused: TMemo;
function GetComboFocused: TComboBox;
procedure ComboCopyToClipboard(const Cut: Boolean = False);

procedure SetTranslatorMode(const Value: Boolean);
procedure SetTimeEditMode(const Mode: TUWTimeEditMode);
procedure SetWorkspace(const Alternative: Boolean);

procedure SetAlignTo(const Tag: Integer);
procedure SetActor;
procedure SetStyle;

procedure SetTextTag(const Tag: String);
procedure SetTextTagColor(const HexColor: String);
procedure SetTextCustomTag(const Tag: String);

procedure AddFPSToCombo(const FPS: Single; const Combo: TComboBox);
procedure FillComboWithFPS(const Combo: TComboBox);
procedure FillComboWithEncodings(const Combo: TComboBox);
procedure FillComboWithFormats(const Combo: TComboBox);
procedure FillComboWithOCRScripts(const ACombo: TComboBox; const AIndex: Integer = 0);
procedure FillComboWithStyles(const ACombo: TComboBox; const AIndex: Integer = -1);
procedure FillWithDictionaries(const AMenu: TMenuItem; const ACombo: TComboBox);
procedure FillMenuWithExtensions(const Folder: String; const AParent: TComponent);
procedure FillMenuWithPlayRate(const AParent: TComponent);
procedure FillMenuWithAudioStreams(const AParent: TComponent);

function GetDictNameFromCaption(const AText: String): String;

procedure IncrementUndoGroup;

procedure VLCPlay(const PlayMode: TMediaPlayMode = mpmAll);
procedure VLCPlayFromSelection(const Value: Integer = 0; const InitialOrFinal: Boolean = True);
procedure VLCSeekTo(const Forward: Boolean; const MSecsToSeek: Integer); overload;
procedure VLCSeekTo(const Value: Integer; const Play: Boolean = False); overload;
procedure VLCAlterPlayRate(const Value: Boolean);

procedure DoExtractAudioTrack;

procedure VSTUpdateCount(const VSTInvalidate: Boolean = False);
function VSTUpdating: Boolean;
procedure VSTBeginUpdate;
procedure VSTEndUpdate;
function VSTFocusedNode: Integer;
function VSTLastSelectedNode: Integer;
function VSTGetNodeAtIndex(const Index: Integer): PVirtualNode;
procedure VSTSelectNode(const Index: Integer; const AClear: Boolean); overload;
procedure VSTSelectNode(const Node: PVirtualNode; const AClear: Boolean); overload;
procedure VSTSelectNodes(const Idxs: TIntegerDynArray; const AClear: Boolean);
procedure VSTPaintCell(const TargetCanvas: TCanvas; const CellRect: TRect; const Color: TColor);
procedure VSTShowColumn(const Index: Integer; const Visible: Boolean);
procedure VSTInsertSubtitles(const Before: Boolean = False);
procedure VSTDeleteSubtitles;
procedure VSTMarkSubtitles(const Value: Boolean);
procedure VSTCopySubtitlesToClipboard(const Cut: Boolean = False);
procedure VSTPasteSubtitlesFromClipboard;
function VSTFind(const FindText: String; const CaseSensitive: Boolean; const FromBegining: Boolean; const Backward: Boolean = False; const Replace: Boolean = False; const NewText: String = ''; const ReplaceAll: Boolean = False; const CasePreserve: Boolean = False; const RE: Boolean = False): Boolean;
procedure VSTDoLoop(Proc: TUWSubtitleDoLoopProc; const Selection: TUWSubtitleDoLoopSelection = dlSelected; const Refresh: Boolean = True; const IncrementUndo: Boolean = False; const CBProc: TUWSubtitleDoLoopProcCB = NIL);
procedure DoFastDivideSubtitles;
procedure ApplyChangeFPS(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyCheckErrors(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyFixErrors(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetTimeInitialFromSpin(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetTimeFinalFromSpin(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetTimeDurationFromSpin(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetTimePauseFromSpin(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetTimeInitialFromVLC(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetTimeFinalFromVLC(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetTextFromEdit(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetTranslationFromEdit(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyShiftToPrevious(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyShiftToNext(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyAutomaticDuration(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyDefaultPause(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyAlign(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyActor(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyStyle(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyXY(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyText(const Item: PUWSubtitleItem; const Index: Integer; const NewSubtitleText, NewSubtitleTranslation: String; const Resent: Boolean = True);
procedure ApplyFontBold(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyFontItalic(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyFontStrikeOut(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyFontUnderline(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyFontColor(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyFontClear(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySingTag(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyUnbreakSubtitles(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyAutoBreakSubtitles(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplySetMaximumLineLength(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyReverseText(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyFixRTLPunctuation(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyExtendLength(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyShiftTimeMore(const Item: PUWSubtitleItem; const Index: Integer);
procedure ApplyShiftTimeLess(const Item: PUWSubtitleItem; const Index: Integer);

procedure GetTMatIndex(const Index: Integer);
function GetInstallFolderVLC: String;

// -----------------------------------------------------------------------------

implementation

uses UWSystem.SysUtils, UWSystem.StrUtils, UWSystem.TimeUtils, UWSystem.Encoding,
  UWSubtitleAPI.Tags, UWSubtitleAPI.Formats, UWSubtitles.Utils, RegExpr, Clipbrd,
  UMain, UUndo, UErrors, strUtils, Dialogs, UTexts, UFindAndReplace, USpellCheck,
  UTimings, UInfoAndErrors, UWSystem.Globalization, UStylesAndActors,
  UAudioExtraction, UGlossary, UWMediaEngine, UTM, jsonscanner;

// -----------------------------------------------------------------------------

function GetCustomFolderPath(const SubFolder: String): String;
begin
  Result := IncludeTrailingPathDelimiter(ConcatPaths([ExtractFilePath(ParamStr(0)){$IFDEF DARWIN}+ '/../'{$ENDIF}, SubFolder]));
end;

// -----------------------------------------------------------------------------

function GetCustomFilePath(const FileName: String): String;
begin
  Result := ConcatPaths([ExtractFilePath(ParamStr(0)){$IFDEF DARWIN}+ '/../'{$ENDIF}, FileName]);
end;

// -----------------------------------------------------------------------------

function SettingsFileName: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + 'sw.cfg';
  {$ELSE}
  Result := GetCustomFilePath('sw.cfg');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function LanguageFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetCustomFolderPath('Resources/Languages/');
  {$ELSE}
  Result := GetCustomFolderPath('Languages');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function LanguageFileName: String;
begin
  Result := LanguageFolder + Options.Language + '.lng';
end;

// -----------------------------------------------------------------------------

function ShortCutFileName: String;
begin
  {$IFDEF DARWIN}
  Result := GetCustomFolderPath('Resources/sw.key');
  {$ELSE}
  Result := GetCustomFilePath('sw.key');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function OCRFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetCustomFolderPath('Resources/OCR/');
  {$ELSE}
  Result := GetCustomFolderPath('OCR');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function ExtensionsFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetCustomFolderPath('Resources/Extensions/');
  {$ELSE}
  Result := GetCustomFolderPath('Extensions');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function WaveformsFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + 'SW/Waveforms/';
  {$ELSE}
  Result := GetCustomFolderPath('Waveforms');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function MRUFileName: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + 'SW/sw.mru';
  {$ELSE}
  Result := GetCustomFilePath('sw.mru');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function StylesFileName: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + 'SW/sw.sts';
  {$ELSE}
  Result := GetCustomFilePath('sw.sts');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function ActorsFileName: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + 'SW/sw.act';
  {$ELSE}
  Result := GetCustomFilePath('sw.act');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function DictionariesFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetCustomFolderPath('Resources/Dictionaries/');
  {$ELSE}
  Result := GetCustomFolderPath('Dictionaries');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function ProjectsFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + 'SW/Projects/';
  {$ELSE}
  Result := GetCustomFolderPath('Projects');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function TMFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + 'SW/TM/';
  {$ELSE}
  Result := GetCustomFolderPath('TM');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function TerminologyFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + 'SW/Terminology/';
  {$ELSE}
  Result := GetCustomFolderPath('Terminology');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function MediaFileExists(const FileName: String; const Exts: array of String): String;
var
  i, c   : Integer;
  s1, s2, s3 : String;
//  fl     : TStringList;
begin
  Result := '';

  s1 := FileName;
  while StringCount('.', s1) > 0 do
  begin
    Delete(s1, LastDelimiter('.', s1), (Length(s1)-LastDelimiter('.', s1))+1);
    for i := 0 to Length(Exts)-1 do
    begin
      s2 := s1 + Exts[i];
      s3 := WaveformsFolder + ExtractFileName(s1 + Exts[i]);
      if FileExists(s2) then
      begin
        Result := s2;
        Exit;
      end
      else if FileExists(s3) then
      begin
        Result := s3;
        Exit;
      end;
    end;
  end;

{  if Result = '' then
  begin
    s2 := '';
    for i := 0 to Length(Exts)-1 do s2 := s2 + '*' + Exts[i] + ';';
    fl := TStringList.Create;
    try
      FindAllFiles(fl, ExtractFilePath(FileName), s2, False);
      if fl.Count > 0 then
        for c := 0 to fl.Count-1 do
          if s1 = Copy(fl[c], 1, Length(s1)) then
          begin
            Result := fl[0];
            Exit;
          end;
    finally
      fl.Free;
    end;
  end;}
end;

// -----------------------------------------------------------------------------

procedure ReadLangForForm(const FileName: String; const Form: TForm);
var
  FileStream : TFileStream;
  Parser     : TJSONParser;
  Data       : TJSONData;
  joData,
  joItem     : TJSONObject;
  jaData     : TJSONArray;
  Comp       : TComponent;
  i          : Integer;
  s, desc, t : String;
  ZeroArray  : TJSONArray;
  rndTip     : Integer;
begin
  if not FileExists(FileName) or not Assigned(Form) then Exit;

  Randomize;
  rndTip := Random(3)+1;

  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Parser := TJSONParser.Create(FileStream, [joUTF8]);
    ZeroArray := TJSONArray.Create;
    try
      try
        Data := Parser.Parse;
      except
        Exit;
      end;
      ZeroArray.Clear;
      joData := TJSONObject(Data);
      // Specific strings
      if Form = frmMain then
      begin
        jaData := joData.Get('Strings', ZeroArray);
        for i := 0 to jaData.Count-1 do
        begin
          joItem := jaData.Objects[i];
          s      := joItem.Get('Name', '');
          desc   := joItem.Get('Text', '');
          if s = 'LineSelected' then Strings.LineSelected := desc;
          if s = 'Text' then Strings.Text := desc;
          if s = 'TextChars' then Strings.TextChars := desc;
          if s = 'Translation' then Strings.Translation := desc;
          if s = 'TranslationChars' then Strings.TranslationChars := desc;
          if s = 'AllSupportedFiles' then Strings.AllSupportedFiles := desc;
          if s = 'Selection' then Strings.Selection := desc;
          if s = 'SaveDialog' then Strings.SaveDialog := desc;
          if s = 'WriteDenieded' then Strings.WriteDenieded := desc;
          if s = 'ExtractAudio' then Strings.ExtractAudio := desc;
          if s = 'ExtractLibError' then Strings.ExtractLibError := desc;
          if s = 'libMPVError' then Strings.libMPVError := desc;
          if s = 'LoadingVideo' then Strings.LoadingVideo := desc;
        end;
        // Error strings
        jaData := joData.Get('ErrorStrings', ZeroArray);
        for i := 0 to jaData.Count-1 do
        begin
          joItem := jaData.Objects[i];
          s      := joItem.Get('Name', '');
          desc   := joItem.Get('Text', '');
          if s = 'Marked' then ErrorStrings.Marked := desc;
          if s = 'BadValues' then ErrorStrings.BadValues := desc;
          if s = 'TimeTooLong' then ErrorStrings.TimeTooLong := desc;
          if s = 'TimeTooShort' then ErrorStrings.TimeTooShort := desc;
          if s = 'PauseTooShort' then ErrorStrings.PauseTooShort := desc;
          if s = 'MaxCPS' then ErrorStrings.MaxCPS := desc;
          if s = 'Overlapping' then ErrorStrings.Overlapping := desc;
          if s = 'FixTags' then ErrorStrings.FixTags := desc;
          if s = 'Empty' then ErrorStrings.Empty := desc;
          if s = 'UnnecessarySpaces' then ErrorStrings.UnnecessarySpaces := desc;
          if s = 'UnnecessaryDots' then ErrorStrings.UnnecessaryDots := desc;
          if s = 'RepeatedChars' then ErrorStrings.RepeatedChars := desc;
          if s = 'ProhibitedChars' then ErrorStrings.ProhibitedChars := desc;
          if s = 'HearingImpaired' then ErrorStrings.HearingImpaired := desc;
          if s = 'BreakLongLines' then ErrorStrings.BreakLongLines := desc;
          if s = 'RepeatedSubtitle' then ErrorStrings.RepeatedSubtitle := desc;
          if s = 'OCR' then ErrorStrings.OCR := desc;
          if s = 'ExecuteScript' then ErrorStrings.ExecuteScript := desc;
          if s = 'CompileScript' then ErrorStrings.CompileScript := desc;
        end;
        // Tip strings
        jaData := joData.Get('Tips', ZeroArray);
        for i := 0 to jaData.Count-1 do
        begin
          joItem := jaData.Objects[i];
          s      := joItem.Get('Name', '');
          desc   := joItem.Get('Text', '');
          if s = 'Tip' + IntToStr(rndTip) then
          begin
            with frmMain do
              case rndTip of
                1: stbStatus.Panels[1].Text := Format(desc, [ShortCutToText(actPreviousSubtitle.ShortCut), ShortCutToText(actNextSubtitle.ShortCut)]);
                2: stbStatus.Panels[1].Text := Format(desc, [ShortCutToText(actCustomSearch.ShortCut)]);
                3: stbStatus.Panels[1].Text := Format(desc, [ShortCutToText(actDockVideoControls.ShortCut)]);
              end;
          end;
        end;
      end;
      // Form texts
      jaData := joData.Get(Form.Name, ZeroArray);
      for i := 0 to jaData.Count-1 do
      begin
        joItem := jaData.Objects[i];
        Comp   := Form.FindComponent(joItem.Get('Name', ''));
        if Comp <> NIL then
        begin
          s := joItem.Get('Text', '');
          desc := joItem.Get('Desc', '');
          if (Comp is TAction) then
          begin
            with (Comp as TAction) do
            begin
              if (Name = 'actShiftTimeMore') or (Name = 'actShiftTimeLess') then
                Caption := Format(s, [Options.ShiftTimeMS])
              else
                Caption := s;

              if desc <> '' then
                Hint := Caption + sLineBreak + sLineBreak + desc
              else
                Hint := Caption;
            end;
          end
          else if (Comp is TMenuItem) then
          begin
            with (Comp as TMenuItem) do
            begin
              Caption := s;
              if desc <> '' then
                Hint := s + sLineBreak + sLineBreak + desc
              else
                Hint := s;
            end;
          end
          else if (Comp is TLabel) then
          begin
            with (Comp as TLabel) do
            begin
              Caption := s;
              if desc <> '' then
                Hint := s + sLineBreak + sLineBreak + desc
              //else
                //Hint := s;
            end;
          end
          else if (Comp is TButton) then
          begin
            with (Comp as TButton) do
            begin
              Caption := s;
              if desc <> '' then
                Hint := s + sLineBreak + sLineBreak + desc
              //else
                //Hint := s;
            end;
          end
          else if (Comp is TCheckBox) then
          begin
            with (Comp as TCheckBox) do
            begin
              Caption := s;
              if desc <> '' then
                Hint := s + sLineBreak + sLineBreak + desc
              //else
                //Hint := s;
            end;
          end
          else if (Comp is TComboBox) then
          begin
            with (Comp as TComboBox) do
            begin
              if desc <> '' then
                Hint := s + sLineBreak + sLineBreak + desc
              else
                Hint := s;
            end;
          end
          else if (Comp is TUWNumberBox) then
          begin
            with (Comp as TUWNumberBox) do
            begin
              if desc <> '' then
                Hint := s + sLineBreak + sLineBreak + desc
              else
                Hint := s;
            end;
          end;
        end
        else
        begin
          s    := joItem.Get('Name', '');
          desc := joItem.Get('Text', '');
          t    := joItem.Get('Desc', '');

          if Form = frmMain then
          begin
            if s = 'hdrIndex' then
              frmMain.VST.Header.Columns[0].Text := desc
            else if s = 'hdrTimes' then
              frmMain.VST.Header.Columns[1].Text := desc
            else if s = 'hdrDuration' then
              frmMain.VST.Header.Columns[2].Text := desc
            else if s = 'hdrStyle' then
              frmMain.VST.Header.Columns[3].Text := desc
            else if s = 'hdrText' then
              frmMain.VST.Header.Columns[4].Text := desc
            else if s = 'hdrTranslation' then
              frmMain.VST.Header.Columns[5].Text := desc
            else if s = 'hdrCPS' then
              frmMain.VST.Header.Columns[6].Text := desc
            else if s = 'hdrWPM' then
              frmMain.VST.Header.Columns[7].Text := desc
            else if s = 'hdrCPL' then
              frmMain.VST.Header.Columns[8].Text := desc;
          end
          else if s = Form.Name then
            Form.Caption := desc;

          if Form.Name = 'frmTexts' then
          begin
            if s = 'strLess' then
              UTexts.strLess := desc
            else if s = 'strMore' then
              UTexts.strMore := desc
            else if s = 'strCancel' then
              UTexts.strCancel := desc
            else if s = 'strNoInternet' then
              UTexts.strNoInternet := desc
            else if s = 'strOrig' then
              UTexts.strOrig := desc
            else if s = 'strTrans' then
              UTexts.strTrans := desc;
          end;

          if Form.Name = 'frmTimings' then
          begin
            if s = 'strLess' then
              UTimings.strLess := desc
            else if s = 'strMore' then
              UTimings.strMore := desc
            else if s = 'strNewAll' then
              UTimings.strNewAll := desc
            else if s = 'strNewGreater' then
              UTimings.strNewGreater := desc
            else if s = 'strNewSmaller' then
              UTimings.strNewSmaller := desc
            else if s = 'strExpand' then
              UTimings.strExpand := desc
            else if s = 'strReduce' then
              UTimings.strReduce := desc;
          end;

          if Form.Name = 'frmFindAndReplace' then
          begin
            if s = 'strLess' then
              UFindAndReplace.strLess := desc
            else if s = 'strMore' then
              UFindAndReplace.strMore := desc;
          end;

          if Form.Name = 'frmStylesAndActors' then
          begin
            if s = 'strNewStyle' then
              UStylesAndActors.strNewStyle := desc
            else if s = 'strNewStyleName' then
              UStylesAndActors.strNewStyleName := desc;
          end;

          if Form.Name = 'frmSpellCheck' then
          begin
            if s = 'strFinished' then
              USpellCheck.strFinished := desc;
          end;

          if Form.Name = 'frmAudioExtraction' then
          begin
            if s = 'strExtracting' then
              UAudioExtraction.strExtracting := desc
            else if s = 'strGenerating' then
              UAudioExtraction.strGenerating := desc;
          end;

          if Form.Name = 'frmGlossary' then
          begin
            if s = 'hdrOriginal' then
              (Form as TfrmGlossary).VST.Header.Columns[0].Text := desc
            else if s = 'hdrTranslated' then
              (Form as TfrmGlossary).VST.Header.Columns[1].Text := desc
            else if s = 'hdrComments' then
              (Form as TfrmGlossary).VST.Header.Columns[2].Text := desc;
          end;

          if Form.Name = 'frmTM' then
          begin
            if s = 'hdrOriginal' then
              (Form as TfrmTM).VST.Header.Columns[0].Text := desc
            else if s = 'hdrTranslated' then
              (Form as TfrmTM).VST.Header.Columns[1].Text := desc
            else if s = 'hdrPercent' then
              (Form as TfrmTM).VST.Header.Columns[2].Text := desc;
          end;

          if Form.Name = 'frmInfoAndErrors' then
          begin
            if s = 'strNext' then
              UInfoAndErrors.sNext := desc
            else if s = 'strApply' then
              UInfoAndErrors.sApply := desc
            else if s = 'igdCheck' then
              UInfoAndErrors.igdCheck := desc
            else if s = 'igdFix' then
              UInfoAndErrors.igdFix := desc
            else if s = 'igdDesc' then
              UInfoAndErrors.igdDesc := desc
            else if s = 'fgdApply' then
              UInfoAndErrors.fgdApply := desc
            else if s = 'fgdFunction' then
              UInfoAndErrors.fgdFunction := desc
            else if s = 'fgdBefore' then
              UInfoAndErrors.fgdBefore := desc
            else if s = 'fgdAfter' then
              UInfoAndErrors.fgdAfter := desc
            else if s = 'Desc1' then
            begin
              UInfoAndErrors.ErrorsToFix[1].FixDesc := desc;
              UInfoAndErrors.ErrorsToFix[1].Example := t;
            end
            else if s = 'Desc2' then
            begin
              UInfoAndErrors.ErrorsToFix[2].FixDesc := desc;
              UInfoAndErrors.ErrorsToFix[2].Example := t;
            end
            else if s = 'Desc3' then
            begin
              UInfoAndErrors.ErrorsToFix[3].FixDesc := desc;
              UInfoAndErrors.ErrorsToFix[3].Example := t;
            end
            else if s = 'Desc4' then
            begin
              UInfoAndErrors.ErrorsToFix[4].FixDesc := desc;
              UInfoAndErrors.ErrorsToFix[4].Example := t;
            end
            else if s = 'Desc5' then
            begin
              UInfoAndErrors.ErrorsToFix[5].FixDesc := desc;
              UInfoAndErrors.ErrorsToFix[5].Example := t;
            end
            else if s = 'Desc6' then
            begin
              UInfoAndErrors.ErrorsToFix[6].FixDesc := desc;
              UInfoAndErrors.ErrorsToFix[6].Example := t;
            end
            else if s = 'Desc7' then
            begin
              UInfoAndErrors.ErrorsToFix[7].FixDesc := desc;
              UInfoAndErrors.ErrorsToFix[7].Example := t;
            end
            else if s = 'Desc8' then
            begin
              UInfoAndErrors.ErrorsToFix[8].FixDesc := desc;
              UInfoAndErrors.ErrorsToFix[8].Example := t;
            end
            else if s = 'Desc9' then
            begin
              UInfoAndErrors.ErrorsToFix[9].FixDesc := desc;
              UInfoAndErrors.ErrorsToFix[9].Example := t;
            end
            else if s = 'Desc10' then
            begin
              UInfoAndErrors.ErrorsToFix[10].FixDesc := Format(desc, [Options.ErrCfg.ProhibitedChars]);
              UInfoAndErrors.ErrorsToFix[10].Example := t;
            end
            else if s = 'Desc11' then
            begin
              UInfoAndErrors.ErrorsToFix[11].FixDesc := desc;
              UInfoAndErrors.ErrorsToFix[11].Example := t;
            end
            else if s = 'Desc12' then
            begin
              UInfoAndErrors.ErrorsToFix[12].FixDesc := desc;
              UInfoAndErrors.ErrorsToFix[12].Example := t;
            end
            else if s = 'Desc13' then
            begin
              UInfoAndErrors.ErrorsToFix[13].FixDesc := desc;
              UInfoAndErrors.ErrorsToFix[13].Example := t;
            end
            else if s = 'Desc14' then
            begin
              UInfoAndErrors.ErrorsToFix[14].FixDesc := desc;
              UInfoAndErrors.ErrorsToFix[14].Example := t;
            end
            {else if s = 'Desc15' then
            begin
              UInfoAndErrors.ErrorsToFix[15].FixDesc := desc;
              UInfoAndErrors.ErrorsToFix[16].Example := t;
            end
            else if s = 'Desc16' then
            begin
              UInfoAndErrors.ErrorsToFix[16].FixDesc := desc;
              UInfoAndErrors.ErrorsToFix[16].Example := t;
            end;}
          end;
        end;
      end;
    finally
      ZeroArray.Free;
      FreeAndNil(Parser);
    end;
  finally
    FileStream.Destroy;
  end;
end;

// -----------------------------------------------------------------------------

procedure ReadShortCuts(const FileName: String);
var
  FileStream : TFileStream;
  Parser     : TJSONParser;
  Data       : TJSONData;
  joData,
  joItem     : TJSONObject;
  jaData     : TJSONArray;
  Comp       : TComponent;
  i          : Integer;
  s          : String;
  ZeroArray  : TJSONArray;
begin
  if not FileExists(FileName) then Exit;

  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Parser := TJSONParser.Create(FileStream);
    ZeroArray := TJSONArray.Create;
    try
      try
        Data := Parser.Parse;
      except
        Exit;
      end;
      ZeroArray.Clear;
      joData := TJSONObject(Data);
      jaData := joData.Get('Shortcuts', ZeroArray);
      for i := 0 to jaData.Count-1 do
      begin
        joItem := jaData.Objects[i];
        Comp   := frmMain.FindComponent(joItem.Get('Action', ''));
        if Comp <> NIL then
        begin
          s := joItem.Get('Key', '');
          if (Comp is TAction) then
          begin
            with (Comp as TAction) do
              ShortCut := TextToShortCut(s);
          end;
        end;
      end;
    finally
      ZeroArray.Free;
      FreeAndNil(Parser);
    end;
  finally
    FileStream.Destroy;
  end;
end;

// -----------------------------------------------------------------------------

procedure DefaultSettings;
begin
  with Options do
  begin
    Language                 := 'en-US';
    AutoCheckErrors          := True;
    ShowErrors               := True;
    ErrCfg.RepeatableChars   := '-¡!¿?";\/_[]=';
    ErrCfg.ProhibitedChars   := '@,http,www,#,*';
    ErrCfg.MaxLineLength     := 43;
    ErrCfg.MaxDuration       := 8000;
    ErrCfg.MinDuration       := 1000;
    ErrCfg.MinPause          := 200;
    ErrCfg.MaxCPS            := 20;
    ErrCfg.RepeatedTolerance := 100;
    ErrOptions := [etBadValues, etTimeTooLong, etTimeTooShort,
      etPauseTooShort, etMaxCPS, etOverlapping, etFixTags, etEmpty,
      etUnnecessarySpaces, etUnnecessaryDots, etRepeatedChars, etProhibitedChars,
      etHearingImpaired, etBreakLongLines, etRepeatedSubtitle, etOCR];

    Colors.Overlapping   := clGray;
    Colors.BadValues     := clRed;
    Colors.TimeTooShort  := clPurple;
    Colors.TimeTooLong   := clFuchsia;
    Colors.PauseTooShort := clYellow;
    Colors.Untranslated  := clRed;

    Marquee.Color    := '#FFFF00';
    Marquee.Position := 'bottom';
    Marquee.Size     := 32;

    DrawTags := True;

    NewSubtitleMS := 1000;
    DefSubtitlePauseMS := 200;
    DefChangePlayRate := 50;
    ShiftTimeMS := 500;
    DotsOnSplit := False;

    TextToFind := '';

    ShowSubtitles := TShowSubtitles.ssText;

    HunspellLang := 'en_US';

    AutoStartPlaying := True;
    ShowWelcomeAtStartup := True;

    CustomSearch := CustomSearchWordReference;
  end;

  with LastSubtitle do
  begin
    Selected    := -1;
    InitialTime := 0;
    Color       := clBlack;
    ShowIndex   := 0;
  end;

  with AudioExtraction do
  begin
    FileName := VLC_EXE;
    Params   := VLC_Params;
  end;

  frmMain.actSingTag.Caption := swt_Sing;
end;

// -----------------------------------------------------------------------------

function HexToColor(const C: String): TColor;
begin
  Result := TColor(Hex2Dec(RightStr(C, 6)));
end;

function ColorToHex(const C: TColor): String;
begin
  Result := '$' + IntToHex(C, 6);
end;

// -----------------------------------------------------------------------------

procedure LoadSettings;
begin
  if not FileExists(SettingsFileName) then Exit;

  with TXMLConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    with Options do
    begin
      OpenKey('Settings');
      Language := GetValue('Language', Language);
      HunspellLang := GetValue('HunspellLang', HunspellLang);
      ShowWelcomeAtStartup := GetValue('ShowWelcomeAtStartup', ShowWelcomeAtStartup);
      AutoStartPlaying := GetValue('AutoStartPlaying', ShowWelcomeAtStartup);
      AutoCheckErrors := GetValue('AutoCheckErrors', AutoCheckErrors);
      ShowErrors := GetValue('ShowErrors', ShowErrors);
      DrawTags := GetValue('DrawTags', DrawTags);
      NewSubtitleMS := GetValue('NewSubtitleMS', NewSubtitleMS);
      DefSubtitlePauseMS := GetValue('DefSubtitlePauseMS', DefSubtitlePauseMS);
      DefChangePlayRate := GetValue('DefChangePlayRate', DefChangePlayRate);
      ShiftTimeMS := GetValue('ShiftTimeMS', ShiftTimeMS);
      DotsOnSplit := GetValue('DotsOnSplit', False);
      TextToFind := GetValue('TextToFind', TextToFind);
      frmMain.actTranslatorMode.Checked := GetValue('TranslatorMode', False);
      frmMain.actTranslatorModeExecute(NIL);
      frmMain.actVideoPreview.Checked := GetValue('VideoPreview', False);
      frmMain.actVideoPreviewExecute(NIL);
      frmMain.actAudioPreview.Checked := GetValue('AudioPreview', False);
      frmMain.actAudioPreviewExecute(NIL);
      frmMain.actSimpleMode.Checked := GetValue('SimpleMode', False);
      frmMain.actSimpleModeExecute(NIL);
      frmMain.actChangeWorkspace.Checked := GetValue('WorkspaceAlternative', False);
      frmMain.actChangeWorkspaceExecute(NIL);
      if not GetValue('DockVideoControls', True) then frmMain.actDockVideoControls.Tag := -1;
      WorkMode := TWorkMode(GetValue('WorkMode', Integer(wmTime)));
      if (WorkMode = wmFrames) and not frmMain.actModeFrames.Checked then frmMain.actModeFrames.Execute;
      Integer(ShowSubtitles) := GetValue('ShowSubtitles', Integer(ShowSubtitles));
      Integer(ErrOptions) := GetValue('ErrOptions', Integer(ErrOptions));
      frmMain.actMediaAutoScroll.Checked := GetValue('MediaAutoScroll', True);
      frmMain.actMediaChangePlayRate.Checked := GetValue('MediaChangePlayRate', False);
      CustomSearch := GetValue('CustomSearch', CustomSearchWordReference);
      CloseKey;

      OpenKey('VST');
      frmMain.actViewNumber.Checked   := GetValue('ColumnNumber', True);
      frmMain.actViewTimes.Checked    := GetValue('ColumnTimes', True);
      frmMain.actViewDuration.Checked := GetValue('ColumnDuration', True);
      frmMain.actViewStyle.Checked    := GetValue('ColumnStyle', False);
      frmMain.actViewCPS.Checked    := GetValue('ColumnCPS', False);
      frmMain.actViewWPM.Checked    := GetValue('ColumnWPM', False);
      frmMain.actViewCPL.Checked    := GetValue('ColumnCPL', False);
      VSTShowColumn(0, frmMain.actViewNumber.Checked);
      VSTShowColumn(1, frmMain.actViewTimes.Checked);
      VSTShowColumn(2, frmMain.actViewDuration.Checked);
      VSTShowColumn(3, frmMain.actViewStyle.Checked);
      VSTShowColumn(6, frmMain.actViewCPS.Checked);
      VSTShowColumn(7, frmMain.actViewWPM.Checked);
      VSTShowColumn(8, frmMain.actViewCPL.Checked);
      CloseKey;

      OpenKey('Toolbars');
      frmMain.actViewToolbarAdditional.Checked := GetValue('Additional', False);
      frmMain.actViewToolbarAdditionalExecute(frmMain.actViewToolbarAdditional);
      CloseKey;

      with Colors do
      begin
        OpenKey('Colors');
        Overlapping   := HexToColor(GetValue('Overlapping', ColorToHex(Overlapping)));
        BadValues     := HexToColor(GetValue('BadValues', ColorToHex(BadValues)));
        TimeTooShort  := HexToColor(GetValue('TimeTooShort', ColorToHex(TimeTooShort)));
        TimeTooLong   := HexToColor(GetValue('TimeTooLong', ColorToHex(TimeTooLong)));
        PauseTooShort := HexToColor(GetValue('PauseTooShort', ColorToHex(PauseTooShort)));
        Untranslated  := HexToColor(GetValue('Untranslated', ColorToHex(Untranslated)));

        frmMain.shpColor1.Brush.Color := HexToColor(GetValue('c1', ColorToHex(frmMain.shpColor1.Brush.Color)));
        frmMain.shpColor2.Brush.Color := HexToColor(GetValue('c2', ColorToHex(frmMain.shpColor2.Brush.Color)));
        frmMain.shpColor3.Brush.Color := HexToColor(GetValue('c3', ColorToHex(frmMain.shpColor3.Brush.Color)));
        frmMain.shpColor4.Brush.Color := HexToColor(GetValue('c4', ColorToHex(frmMain.shpColor4.Brush.Color)));
        frmMain.shpColor5.Brush.Color := HexToColor(GetValue('c5', ColorToHex(frmMain.shpColor5.Brush.Color)));
        frmMain.shpColor6.Brush.Color := HexToColor(GetValue('c6', ColorToHex(frmMain.shpColor6.Brush.Color)));
        frmMain.shpColor7.Brush.Color := HexToColor(GetValue('c7', ColorToHex(frmMain.shpColor7.Brush.Color)));
        frmMain.shpColor8.Brush.Color := HexToColor(GetValue('c8', ColorToHex(frmMain.shpColor8.Brush.Color)));
        CloseKey;
      end;

      with Marquee do
      begin
        OpenKey('Marquee');
        Color    := GetValue('Color', Color);
        Position := GetValue('Position', Position);
        Size     := GetValue('Size', Size);
        CloseKey;
      end;

      with ErrCfg do
      begin
        OpenKey('ErrCfg');
        RepeatableChars   := GetValue('RepeatableChars', RepeatableChars);
        ProhibitedChars   := GetValue('ProhibitedChars', ProhibitedChars);
        MaxLineLength     := GetValue('MaxLineLength', MaxLineLength);
        MaxDuration       := GetValue('MaxDuration', MaxDuration);
        MinDuration       := GetValue('MinDuration', MinDuration);
        MinPause          := GetValue('MinPause', MinPause);
        MaxCPS            := GetValue('MaxCPS', MaxCPS);
        RepeatedTolerance := GetValue('RepeatedTolerance', RepeatedTolerance);
        CloseKey;
      end;

      with AudioExtraction do
      begin
        OpenKey('AudioExtraction');
        FileName := GetValue('EXE', FileName);
        Params   := GetValue('Params', Params);
        CloseKey;
      end;
    end;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveSettings;
begin
  with TXMLConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    //Clear;
    RootName := 'SW';
    with Options do
    begin
      OpenKey('Settings');
      SetValue('Language', Language);
      SetValue('HunspellLang', HunspellLang);
      SetValue('ShowWelcomeAtStartup', ShowWelcomeAtStartup);
      SetValue('AutoStartPlaying', AutoStartPlaying);
      SetValue('AutoCheckErrors', AutoCheckErrors);
      SetValue('ShowErrors', ShowErrors);
      SetValue('DrawTags', DrawTags);
      SetValue('NewSubtitleMS', NewSubtitleMS);
      SetValue('DefSubtitlePauseMS', DefSubtitlePauseMS);
      SetValue('DefChangePlayRate', DefChangePlayRate);
      SetValue('DotsOnSplit', DotsOnSplit);
      SetValue('ShiftTimeMS', ShiftTimeMS);
      SetValue('TextToFind', TextToFind);
      SetValue('TranslatorMode', TranslatorMode);
      SetValue('VideoPreview', VideoPreview);
      SetValue('AudioPreview', AudioPreview);
      SetValue('SimpleMode', frmMain.actSimpleMode.Checked);
      SetValue('WorkspaceAlternative', frmMain.actChangeWorkspace.Checked);
      SetValue('DockVideoControls', frmMain.actDockVideoControls.Checked);
      SetValue('WorkMode', Integer(WorkMode));
      SetValue('ShowSubtitles', Integer(ShowSubtitles));
      SetValue('ErrOptions', Integer(ErrOptions));
      SetValue('MediaAutoScroll', frmMain.actMediaAutoScroll.Checked);
      SetValue('MediaChangePlayRate', frmMain.actMediaChangePlayRate.Checked);
      SetValue('CustomSearch', CustomSearch);
      CloseKey;

      OpenKey('VST');
      SetValue('ColumnNumber', frmMain.actViewNumber.Checked);
      SetValue('ColumnTimes', frmMain.actViewTimes.Checked);
      SetValue('ColumnDuration', frmMain.actViewDuration.Checked);
      SetValue('ColumnStyle', frmMain.actViewStyle.Checked);
      SetValue('ColumnCPS', frmMain.actViewCPS.Checked);
      SetValue('ColumnWPM', frmMain.actViewWPM.Checked);
      SetValue('ColumnCPL', frmMain.actViewCPL.Checked);
      CloseKey;

      OpenKey('Toolbars');
      SetValue('Additional', frmMain.actViewToolbarAdditional.Checked);
      CloseKey;

      with Colors do
      begin
        OpenKey('Colors');
        SetValue('Overlapping', ColorToHex(Overlapping));
        SetValue('BadValues', ColorToHex(BadValues));
        SetValue('TimeTooShort', ColorToHex(TimeTooShort));
        SetValue('TimeTooLong', ColorToHex(TimeTooLong));
        SetValue('PauseTooShort', ColorToHex(PauseTooShort));
        SetValue('Untranslated', ColorToHex(Untranslated));

        SetValue('c1', ColorToHex(frmMain.shpColor1.Brush.Color));
        SetValue('c2', ColorToHex(frmMain.shpColor2.Brush.Color));
        SetValue('c3', ColorToHex(frmMain.shpColor3.Brush.Color));
        SetValue('c4', ColorToHex(frmMain.shpColor4.Brush.Color));
        SetValue('c5', ColorToHex(frmMain.shpColor5.Brush.Color));
        SetValue('c6', ColorToHex(frmMain.shpColor6.Brush.Color));
        SetValue('c7', ColorToHex(frmMain.shpColor7.Brush.Color));
        SetValue('c8', ColorToHex(frmMain.shpColor8.Brush.Color));
        CloseKey;
      end;

      with Marquee do
      begin
        OpenKey('Marquee');
        SetValue('Color', Color);
        SetValue('Position', Position);
        SetValue('Size', Size);
        CloseKey;
      end;

      with ErrCfg do
      begin
        OpenKey('ErrCfg');
        SetValue('RepeatableChars', RepeatableChars);
        SetValue('ProhibitedChars', ProhibitedChars);
        SetValue('MaxLineLength', MaxLineLength);
        SetValue('MaxDuration', MaxDuration);
        SetValue('MinDuration', MinDuration);
        SetValue('MinPause', MinPause);
        SetValue('MaxCPS', MaxCPS);
        SetValue('RepeatedTolerance', RepeatedTolerance);
        CloseKey;
      end;

      with AudioExtraction do
      begin
        OpenKey('AudioExtraction');
        SetValue('EXE', FileName);
        SetValue('Params', Params);
        CloseKey;
      end;
    end;
    Flush;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure CommandLineProcess;
var
  i: Byte;
begin
  // Command line parameters reading
  if ParamCount > 0 then
  begin
    // Subtitle file
    if FileExists(ParamStr(1)) then
      frmMain.LoadSubtitle(ParamStr(1));

    // Others params
    for i := 1 to ParamCount do
    begin
      //ParamStr(i);
    end;
  end;
end;

// -----------------------------------------------------------------------------

function MsgSaveSubtitle(const FileName: String): Integer;
begin
  Result := MessageDlg(Format(Strings.SaveDialog, [FileName, sLineBreak]),
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel], 0);
end;

// -----------------------------------------------------------------------------

function GetInitialTime(const Index: Integer): Integer;
begin
  if WorkMode = wmTime then
    Result := Subtitles[Index].InitialTime
  else
    Result := Subtitles.InitialFrames[Index, GetInputFPS];
end;

// -----------------------------------------------------------------------------

function GetInitialTimeStr(const Index: Integer; const Trim: Boolean = False; const Format: String = DefTimeFormat): String;
begin
  if WorkMode = wmTime then
  begin
    Result := TimeToString(GetInitialTime(Index), Format);
    if Trim then Result := TrimTimeString(Result);
  end
  else
    Result := IntToStr(GetInitialTime(Index));
end;

// -----------------------------------------------------------------------------

function GetFinalTime(const Index: Integer): Integer;
begin
  if WorkMode = wmTime then
    Result := Subtitles[Index].FinalTime
  else
    Result := Subtitles.FinalFrames[Index, GetInputFPS];
end;

// -----------------------------------------------------------------------------

function GetFinalTimeStr(const Index: Integer; const Trim: Boolean = False; const Format: String = DefTimeFormat): String;
begin
  if WorkMode = wmTime then
  begin
    Result := TimeToString(GetFinalTime(Index), Format);
    if Trim then Result := TrimTimeString(Result);
  end
  else
    Result := IntToStr(GetFinalTime(Index));
end;

// -----------------------------------------------------------------------------

function GetDurationTime(const Index: Integer): Integer;
begin
  if WorkMode = wmTime then
    Result := Subtitles.Duration[Index]
  else
    Result := Subtitles.DurationFrames[Index, GetInputFPS];
end;

// -----------------------------------------------------------------------------

function GetDurationTimeStr(const Index: Integer; const Trim: Boolean = False; const Format: String = DefTimeFormat): String;
begin
  if WorkMode = wmTime then
  begin
    Result := TimeToString(GetDurationTime(Index), Format);
    if Trim then Result := TrimTimeString(Result);
  end
  else
    Result := IntToStr(GetDurationTime(Index));
end;

// -----------------------------------------------------------------------------

function GetPauseTime(const Index: Integer): Integer;
begin
  if WorkMode = wmTime then
    Result := Subtitles.Pause[Index]
  else
    Result := Subtitles.PauseFrames[Index, GetInputFPS];
end;

// -----------------------------------------------------------------------------

function GetPauseTimeStr(const Index: Integer; const Trim: Boolean = False; const Format: String = DefTimeFormat): String;
begin
  if WorkMode = wmTime then
  begin
    Result := TimeToString(GetPauseTime(Index), Format);
    if Trim then Result := TrimTimeString(Result);
  end
  else
    Result := IntToStr(GetPauseTime(Index));
end;

// -----------------------------------------------------------------------------

function GetInputFPS: Single;
begin
  with frmMain.cboInputFPS do
    Result := StrToFloatDef(Text, DefFPS, FormatSettings);
end;

// -----------------------------------------------------------------------------

function GetFPS: Single;
begin
  with frmMain.cboFPS do
    Result := StrToFloatDef(Text, DefFPS, FormatSettings);
end;

// -----------------------------------------------------------------------------

function CorrectTime(const Time: Integer): Integer;
begin
  if WorkMode = wmTime then
    Result := Time
  else
    Result := FramesToTime(Time, GetInputFPS);
end;

// -----------------------------------------------------------------------------

procedure SetSubtitleTime(const Index: Integer; const Time: Integer; const Tag: Integer; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);
begin
  Undo.AddUndo(utSubtitleChange, Index, Subtitles[Index], LastUndoGroup);

  case Tag of
    0  : Subtitles.InitialTime[Index] := Time;
    1  : begin
           Subtitles.FinalTime[Index] := Time;
           if Options.AutoCheckErrors and (Subtitles.ValidIndex(Index+1)) then
             Subtitles.ItemPointer[Index+1]^.ErrorType := CheckErrors(Subtitles, Index+1, Options.ErrOptions - [etOCR], Options.ErrCfg);
         end;
    2 : Subtitles.Duration[Index] := Time;
    3 : Subtitles.Pause[Index] := Time;
  end;

  SubtitleChanged(True, True);
  if AUpdate then
  begin
    if Options.AutoCheckErrors then Subtitles.ItemPointer[Index]^.ErrorType := CheckErrors(Subtitles, Index, Options.ErrOptions - [etOCR], Options.ErrCfg);
    frmMain.UpdateColorsInBoxes(Index);
    frmMain.UpdateCPSAndTexts;
    if frmMain.VST.SelectedCount = 1 then frmMain.VST.Invalidate;
  end;
  if AutoIncrementUndo then IncrementUndoGroup;
end;

// -----------------------------------------------------------------------------

procedure SetSubtitleText(const Index: Integer; const Text: String; const IsOriginal: Boolean = True; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True; const Resent: Boolean = True);
begin
  if Resent then
    Undo.AddUndoIfNotResent(utSubtitleChange, Index, Subtitles[Index], LastUndoGroup)
  else
    Undo.AddUndo(utSubtitleChange, Index, Subtitles[Index], LastUndoGroup);


  if IsOriginal then
  begin
    Subtitles.Text[Index] := Text;
    SubtitleChanged(True, False);
  end
  else
  begin
    Subtitles.Translation[Index] := Text;
    SubtitleChanged(False, True);
  end;

  if AUpdate then
  begin
    if Options.AutoCheckErrors then Subtitles.ItemPointer[Index]^.ErrorType := CheckErrors(Subtitles, VSTFocusedNode, Options.ErrOptions - [etOCR], Options.ErrCfg);
    frmMain.UpdateCPSAndTexts;
    frmMain.VST.Invalidate;
  end;
  if AutoIncrementUndo then IncrementUndoGroup;
end;

// -----------------------------------------------------------------------------

function InsertSubtitle(const Index: Integer; const Item: TUWSubtitleItem; const AutoIncrementUndo: Boolean = True; const AUpdate: Boolean = True): Integer;
begin
  Result := Index;
  if Subtitles.ValidIndex(Result) then
    Subtitles.Insert(Result, Item, NIL, False)
  else
    Result := Subtitles.Add(Item, NIL, False);

  SubtitleChanged(True, True);
  Undo.AddUndo(utInsertLine, Result, Item, LastUndoGroup);
  if AutoIncrementUndo then IncrementUndoGroup;

  frmMain.VST.RootNodeCount := Subtitles.Count;
  if AUpdate then frmMain.UpdateValues(True);
end;

// -----------------------------------------------------------------------------

function InsertSubtitle(const Index: Integer; const InitialTime, FinalTime: Integer; const Text, Translation: String; const AutoIncrementUndo: Boolean = True; const AUpdate: Boolean = True): Integer;
var
  Item: TUWSubtitleItem;
begin
  ClearSubtitleItem(Item);
  Item.InitialTime := InitialTime;
  Item.FinalTime   := FinalTime;
  Item.Text        := Text;
  Item.Translation := Translation;

  Result := InsertSubtitle(Index, Item, AutoIncrementUndo, AUpdate);
end;

// -----------------------------------------------------------------------------

procedure DeleteSubtitle(const Index: Integer; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);
begin
  if not Subtitles.ValidIndex(Index) then Exit;
  Undo.AddUndo(utDeleteLine, Index, Subtitles[Index], LastUndoGroup);

  Subtitles.Delete(Index);
  SubtitleChanged(True, True);
  frmMain.VST.RootNodeCount := Subtitles.Count;
  if AUpdate then frmMain.UpdateValues(True);
  if AutoIncrementUndo then IncrementUndoGroup;
end;

// -----------------------------------------------------------------------------

procedure SubtitleChanged(const AText, ATranslation: Boolean);
begin
  with SubtitleFile do
  begin
    Text.Changed        := AText;
    Translation.Changed := ATranslation;
  end;
end;

// -----------------------------------------------------------------------------

function GetSubtitleTextAtTime(const MSecs: Cardinal): String;
var
  Run: PVirtualNode;
begin
  Result := '';

  with frmMain.VST do
    if TotalCount > 0 then
    begin
      Run := GetFirst;
      while Assigned(Run) do
      begin
        with Subtitles[Run^.Index] do
          if (MSecs >= InitialTime) and (MSecs <= FinalTime) then
          begin
            case Options.ShowSubtitles of
              TShowSubtitles.ssText        : Result := Text;
              TShowSubtitles.ssTranslation : Result := Translation;
            end;
            Result := ReplaceString(Result, sLineBreak, #10) + #10 + ' ';
            if Subtitles[Run^.Index].Align <> 0 then
            begin
              case Subtitles[Run^.Index].Align of
                1: Result := '{\an1}' + Result + '{\an0}';
                2: Result := '{\an2}' + Result + '{\an0}';
                3: Result := '{\an3}' + Result + '{\an0}';
              end;
            end;

            LastSubtitle.ShowIndex := Run^.Index;
            if LastSubtitle.Selected <> LastSubtitle.ShowIndex then
            begin
              LastSubtitle.Selected := Run^.Index;
              if frmMain.actMediaAutoScroll.Checked then
                VSTSelectNode(Run, True);
            end;
            Break;
          end
          else if (MSecs < InitialTime) then
          begin
            LastSubtitle.ShowIndex := 0;
            Break;
          end;

        Run := GetNext(Run);
      end;
    end;
end;

// -----------------------------------------------------------------------------

function GetLengthForEachLine(Text: String; const Separator: String = sLineBreak; const LastSeparator: String = sLineBreak): String;
var
  TotLen  : Integer;
  PosEnter: Integer;
begin
  Result   := '';
  TotLen   := UTF8Length(Text) - StringCount(sLineBreak, Text) * 2;
  PosEnter := Pos(sLineBreak, Text);
  if PosEnter > 0 then
  begin
    while PosEnter > 0 do
    begin
      Result   := Result + IntToStr(UTF8Length(RemoveSWTags(Copy(Text, 1, PosEnter-1)))) + Separator;
      Text     := Copy(Text, PosEnter + 2, UTF8Length(Text));
      PosEnter := Pos(sLineBreak, Text);
    end;
    Result := Result + IntToStr(UTF8Length(RemoveSWTags(Text))) + LastSeparator + IntToStr(TotLen);
  end
  else
    Result := IntToStr(UTF8Length(RemoveSWTags(Text)));
end;

// -----------------------------------------------------------------------------

procedure FocusMemo(const SelectText: Boolean = True);
var
  Memo: TMemo;
begin
  with frmMain do
  begin
    if TranslatorMode then
      Memo := mmoTranslation
    else
      Memo := mmoText;

    if Memo.Visible then
    begin
      if SelectText then Memo.SelectAll;
      Memo.SetFocus;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure SelectSubtitleAndFocusMemo(const NextSibiling: Boolean);
begin
  if NextSibiling then
  begin
    if (VSTFocusedNode < Subtitles.Count) then VSTSelectNode(VSTFocusedNode+1, True);
  end
  else
  begin
    if (VSTFocusedNode > 0) then VSTSelectNode(VSTFocusedNode-1, True);
  end;

  FocusMemo;
end;

// -----------------------------------------------------------------------------

function GetMemoFocused: TMemo;
begin
  with frmMain do
    if mmoText.Focused then
      Result := mmoText
    else if mmoTranslation.Focused then
      Result := mmoTranslation
    else
      Result := NIL;
end;

// -----------------------------------------------------------------------------

function GetComboFocused: TComboBox;
begin
  with frmMain do
    if cboInputFPS.Focused then
      Result := cboInputFPS
    else if cboFPS.Focused then
      Result := cboFPS
    else
      Result := NIL;
end;

// -----------------------------------------------------------------------------

procedure ComboCopyToClipboard(const Cut: Boolean = False);
var
  Combo : TComboBox;
  Edit  : TEdit;
begin
  Combo := GetComboFocused;
  if Combo <> NIL then
  begin
    Edit := TEdit.Create(NIL);
    try
      //Edit.Parent    := Combo.Parent;
      Edit.Text      := Combo.Text;
      Edit.SelStart  := Combo.SelStart;
      Edit.SelLength := Combo.SelLength;
      //Edit.SelText   := Combo.SelText;
      if not Cut then
        Edit.CopyToClipboard
      else
        Edit.CutToClipboard;
      Combo.Text := Edit.Text;
    finally
      Edit.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetTranslatorMode(const Value: Boolean);
begin
  TranslatorMode := Value;
  VSTShowColumn(5, Value);
  frmMain.mmoTranslation.Visible := Value;
  frmMain.cpsTranslation.Visible := Value;
  frmMain.lblTranslation.Visible := Value;
  frmMain.tlbTM.Visible := Value;
  frmMain.FormResize(frmMain);
  frmMain.VSTResize(frmMain.VST);
end;

// -----------------------------------------------------------------------------

procedure SetTimeEditMode(const Mode: TUWTimeEditMode);
var
  i: Integer;
begin
  VSTBeginUpdate;
  try
    with frmMain do
      for i := 0 to ComponentCount-1 do
        if Components[i] is TUWTimeEdit then
          (Components[i] as TUWTimeEdit).TimeMode := Mode;

    frmMain.UpdateValues; // update times
  finally
    VSTEndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetWorkspace(const Alternative: Boolean);
begin
  with frmMain do
  if lyoVideo.Parent = frmMain then
    if actChangeWorkspace.Checked then
    begin
      lyoVideo.Align := alLeft;
      sptVideo.Align := alLeft;
      lyoVideo.Left := 0;
      //sptVideo.Left := lyoVideo.Width;
    end
    else
    begin
      lyoVideo.Align := alRight;
      sptVideo.Align := alRight;
      sptVideo.Left := 0;
    end;
end;

// -----------------------------------------------------------------------------

procedure SetAlignTo(const Tag: Integer);
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyAlign)
  else
    Subtitles.ItemPointer[VSTFocusedNode]^.Align := Tag;
end;

// -----------------------------------------------------------------------------

procedure SetActor;
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyActor)
  else
  begin
    Subtitles.ItemPointer[VSTFocusedNode]^.Actor := frmMain.cboActor.Text;
    frmMain.UpdateValues(True);
  end;
end;

// -----------------------------------------------------------------------------

procedure SetStyle;
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyStyle)
  else
  begin
    Subtitles.ItemPointer[VSTFocusedNode]^.Style := frmMain.cboStyle.Text;
    frmMain.UpdateValues(True);
  end;
end;

// -----------------------------------------------------------------------------

procedure SetTextTag(const Tag: String);
var
  Memo   : TMemo;
  i      : Integer;
  s1, s2 : String;
begin
  Memo := GetMemoFocused;
  if Memo = NIL then Exit;

  i  := Memo.SelStart;
  s1 := Copy(Memo.Text, 1, Memo.SelStart);
  if Memo.SelText <> '' then
    s2 := Copy(Memo.Text, Memo.SelStart+Memo.SelLength+1, Length(Memo.Text)-(Memo.SelStart+Memo.SelLength))
  else
    s2 := Copy(Memo.Text, Memo.SelStart+1, Length(Memo.Text)-Memo.SelStart);
  Memo.Text := Format('%s{\%s1}%s{\%s0}%s', [s1, Tag, Memo.SelText, Tag, s2]);
  Memo.SelStart := i+3;
end;

// -----------------------------------------------------------------------------

procedure SetTextTagColor(const HexColor: String);
var
  Memo   : TMemo;
  i      : Integer;
  s1, s2 : String;
begin
  Memo := GetMemoFocused;
  if Memo = NIL then Exit;

  i  := Memo.SelStart;
  s1 := Copy(Memo.Text, 1, Memo.SelStart);
  if Memo.SelText <> '' then
    s2 := Copy(Memo.Text, Memo.SelStart+Memo.SelLength+1, Length(Memo.Text)-(Memo.SelStart+Memo.SelLength))
  else
    s2 := Copy(Memo.Text, Memo.SelStart+1, Length(Memo.Text)-Memo.SelStart);
  Memo.Text := Format('%s{\%s&%s&}%s{\%s}%s', [s1, swt_Color, HexColor, Memo.SelText, swt_Color, s2]);
  Memo.SelStart := i+11;
end;

// -----------------------------------------------------------------------------

procedure SetTextCustomTag(const Tag: String);
var
  Memo   : TMemo;
  i      : Integer;
  s1, s2 : String;
begin
  Memo := GetMemoFocused;
  if Memo = NIL then Exit;

  i  := Memo.SelStart;
  s1 := Copy(Memo.Text, 1, Memo.SelStart);
  if Memo.SelText <> '' then
    s2 := Copy(Memo.Text, Memo.SelStart+Memo.SelLength+1, Length(Memo.Text)-(Memo.SelStart+Memo.SelLength))
  else
    s2 := Copy(Memo.Text, Memo.SelStart+1, Length(Memo.Text)-Memo.SelStart);
  Memo.Text := Format('%s%s %s %s%s', [s1, Tag, Memo.SelText, Tag, s2]);
  Memo.SelStart := i+2;
end;

// -----------------------------------------------------------------------------

procedure AddFPSToCombo(const FPS: Single; const Combo: TComboBox);
var
  FPSStr : String;
  Index  : Integer;
begin
  if FPS > 0 then
  begin
    FPSStr := SingleToStr(FPS, 3, FormatSettings); // FormatFloat('#.###', FPS, FormatSettings);
    Index := Combo.Items.IndexOf(FPSStr);
    if (Index = -1) and (FPSStr <> '') then
      Combo.ItemIndex := Combo.Items.Add(FPSStr);
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithFPS(const Combo: TComboBox);
var
  i: Byte;
begin
  with Combo do
  begin
    Items.BeginUpdate;
    Clear;
    for i := 0 to Length(DefFPSList)-1 do Items.Add(SingleToStr(DefFPSList[i], 3, FormatSettings));
    ItemIndex := Items.IndexOf(SingleToStr(DefFPS, 3, FormatSettings));
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithEncodings(const Combo: TComboBox);
var
  i: Byte;
begin
  with Combo do
  begin
    Items.BeginUpdate;
    Clear;
    for i := 0 to MaxEncodings-1 do Items.Add(
      Format('%d: %s', [Encodings[i].CPID, Encodings[i].CPName]));

    ItemIndex := Items.Count-1;
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithFormats(const Combo: TComboBox);
var
  Formats: TStrings;
begin
  with Combo do
  begin
    Items.BeginUpdate;
    Clear;
    Formats := NIL;
    AddFormatsToStrings(Formats);
    try
      Items.Assign(Formats);
    finally
      Formats.Free;
    end;
    ItemIndex := 0;
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithOCRScripts(const ACombo: TComboBox; const AIndex: Integer = 0);
var
  SearchRec : TSearchRec;
begin
  ACombo.Items.BeginUpdate;
  try
    if FindFirst(OCRFolder + '*.ocr', faAnyFile, SearchRec) = 0 then
    try
      repeat
          ACombo.Items.Add(ChangeFileExt(SearchRec.Name, ''));
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  finally
    if ACombo.Items.Count > 0 then ACombo.ItemIndex := AIndex;
    ACombo.Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithStyles(const ACombo: TComboBox; const AIndex: Integer = -1);
var
  i: Integer;
begin
  if Styles.ItemCount > 0 then
  begin
    ACombo.Clear;
    for i := 0 to Styles.ItemCount-1 do
      ACombo.Items.Add(Styles.Items[i].Title);

    if AIndex <> -1 then ACombo.ItemIndex := AIndex;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillWithDictionaries(const AMenu: TMenuItem; const ACombo: TComboBox);
var
  SearchRec : TSearchRec;
  mnu       : TMenuItem;
  x, i      : Integer;
  s         : String;
begin
  if FindFirst(DictionariesFolder + '*.dic', faAnyFile, SearchRec) = 0 then
  try
    x := 0;
    repeat
      s := ChangeFileExt(SearchRec.Name, '');
      if AMenu <> NIL then
      begin
        mnu         := TMenuItem.Create(AMenu);
        mnu.Name    := 'dic_' + IntToStr(x);
        mnu.Caption := Trim(GetCultureDisplayName(ReplaceString(s, '_', '-')) + ' [' + s + ']');
        mnu.OnClick := @frmMain.DictionaryItemClick;
        if s = Options.HunspellLang then
          mnu.Checked := True
        else
          mnu.Checked := False;
        AMenu.Insert(x, mnu);
      end
      else if ACombo <> NIL then
      begin
        i := ACombo.Items.Add(Trim(GetCultureDisplayName(ReplaceString(s, '_', '-')) + ' [' + s + ']'));
        if s = Options.HunspellLang then ACombo.ItemIndex := i;
      end;
     inc(x);
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

// -----------------------------------------------------------------------------

procedure FillMenuWithExtensions(const Folder: String; const AParent: TComponent);
var
  SearchRec : TSearchRec;
  Item      : TMenuItem;
begin
  if AParent = NIL then Exit;

  if FindFirst(ExtensionsFolder + Folder + '*', faAnyFile and faDirectory, SearchRec) = 0 then
  try
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        // if we find a folder
        if (SearchRec.Attr and faDirectory) <> 0 then
        begin
          Item := TMenuItem.Create(AParent);
          Item.Tag := 1;
          Item.Caption := SearchRec.Name;

          if AParent is TPopupMenu then
            with AParent as TPopupMenu do
              Items.Add(Item)
          else if AParent is TMenuItem then
            with AParent as TMenuItem do
              Add(Item);

          FillMenuWithExtensions(Folder + IncludeTrailingPathDelimiter(SearchRec.Name), Item);
        end
        // if we find a file
        else if LowerCase(ExtractFileExt(SearchRec.Name)) = '.pas' then
        begin
          Item := TMenuItem.Create(AParent);
          Item.Caption := ChangeFileExt(SearchRec.Name, '');
          Item.OnClick := @frmMain.ExtensionItemClick;
          if AParent is TPopupMenu then
            with AParent as TPopupMenu do
              Items.Add(Item)
          else if AParent is TMenuItem then
            with AParent as TMenuItem do
              Add(Item);
          end;
        end;
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

// -----------------------------------------------------------------------------

procedure FillMenuWithPlayRate(const AParent: TComponent);
var
  i: Integer;
  Item: TMenuItem;
begin
  if AParent = NIL then Exit;
  i := 30;
  while i < 260 do
  begin
    Item := TMenuItem.Create(AParent);
    Item.Caption := IntToStr(i) + '%';
    Item.OnClick := @frmMain.popPlayRatePopup;
    if AParent is TPopupMenu then
      with AParent as TPopupMenu do
        Items.Add(Item)
    else if AParent is TMenuItem then
      with AParent as TMenuItem do
        Add(Item);

    Inc(i, 10);
  end;
end;

// -----------------------------------------------------------------------------

procedure FillMenuWithAudioStreams(const AParent: TComponent);
var
  i, tl: Integer;
  Item: TMenuItem;
begin
  if AParent = NIL then Exit;

  (AParent as TMenuItem).Clear;

  tl := Length(frmMain.MPV.Engine.TrackList);
  if tl > 0 then
  begin
    for i := 0 to tl-1 do
    begin
      if frmMain.MPV.Engine.TrackList[i].Kind = trkAudio then
      begin
        Item := TMenuItem.Create(AParent);
        Item.Caption := IntToStr(frmMain.MPV.Engine.TrackList[i].ID) + ': '
                        + frmMain.MPV.Engine.TrackList[i].Decoder + ', '
                        + frmMain.MPV.Engine.TrackList[i].Info;
        Item.Tag := frmMain.MPV.Engine.TrackList[i].ID;
        Item.Checked := frmMain.MPV.Engine.TrackList[i].Selected;
        Item.OnClick := @frmMain.popAudioTrackSet;
        if AParent is TPopupMenu then
          with AParent as TPopupMenu do
            Items.Add(Item)
        else if AParent is TMenuItem then
          with AParent as TMenuItem do
            Add(Item);
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function GetDictNameFromCaption(const AText: String): String;
begin
  with TRegExpr.Create do
  try
    Expression := '\[(.*?)\]';
    Exec(AText);

    Result := Match[1];
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure IncrementUndoGroup;
begin
  if LastUndoGroup < 100 then
    Inc(LastUndoGroup)
  else
    LastUndoGroup := 0;
end;

// -----------------------------------------------------------------------------

procedure VLCPlay(const PlayMode: TMediaPlayMode = mpmAll);
begin
  MediaPlayMode := PlayMode;

  with frmMain do
  begin
    case PlayMode of
      mpmSelection,
      mpmFromSelection   : VLCPlayFromSelection;
      mpmBeforeSelection : VLCPlayFromSelection(-500);
      mpmAfterSelection  : VLCPlayFromSelection(+500, False);
    else
      MPV.Engine.Pause;
//      if VLC.IsPause() then
//        VLC.Resume()
//      else if VLC.IsPlay() then
//        VLC.Pause();
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure VLCPlayFromSelection(const Value: Integer = 0; const InitialOrFinal: Boolean = True);
begin
  with frmMain do
    if WAVE.IsPeakDataLoaded and (not WAVE.SelectionIsEmpty) then
      if InitialOrFinal then
        VLCSeekTo(WAVE.Selection.InitialTime + Value, True)
      else
        VLCSeekTo(WAVE.Selection.FinalTime + Value, True);
end;

// -----------------------------------------------------------------------------

procedure VLCSeekTo(const Forward: Boolean; const MSecsToSeek: Integer);
var
  ct, mt, tt: Integer;
begin
  with frmMain do
  begin
    ct := MPV.Engine.Position; //VLC.GetVideoPosInMs;
    tt := MPV.Engine.Duration; //VLC.GetVideoLenInMs;

    if Forward then
      mt := ct + MSecsToSeek
    else
      mt := ct - MSecsToSeek;

    if mt < 0 then
      mt := 0
    else if mt > tt then
      mt := tt;

    VLCSeekTo(mt);
  end;
end;

// -----------------------------------------------------------------------------

procedure VLCSeekTo(const Value: Integer; const Play: Boolean = False);
//var
//  IsPaused: Boolean;
begin
  with frmMain do
  begin
    //VLC.Pause(); //IsPaused := VLC.IsPause();
    MPV.Engine.Seek(Value, True); //VLC.SetVideoPosInMs(Value);
    //VLC.Resume(); //if not IsPaused then VLC.Resume();
    if Play then MPV.Engine.Resume;
  end;
end;

// -----------------------------------------------------------------------------

procedure VLCAlterPlayRate(const Value: Boolean);
begin
  with frmMain do
    if Value then
      MPV.Engine.PlaybackRate(Options.DefChangePlayRate) //VLC.SetPlayRate(Options.DefChangePlayRate)
    else
      MPV.Engine.PlaybackRate(100); //VLC.SetPlayRate(100);
end;

// -----------------------------------------------------------------------------

procedure DoExtractAudioTrack;
begin
//  if MessageDlg('', Strings.ExtractAudio, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
//    if FileExists(AudioExtraction.FileName) then
//    begin
      if frmAudioExtraction = NIL then
      begin
        frmAudioExtraction := TfrmAudioExtraction.Create(Application);
        frmAudioExtraction.ShowModal;
      end;
//    end
//    else
//      ShowMessage(Format(Strings.ExtractLibError, [AudioExtraction.FileName]));
end;

// -----------------------------------------------------------------------------

procedure VSTUpdateCount(const VSTInvalidate: Boolean = False);
begin
  with frmMain do
  begin
    VST.RootNodeCount := Subtitles.Count;
    if VSTInvalidate then VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

function VSTUpdating: Boolean;
begin
  Result := frmMain.VST.Tag > 0;
end;

// -----------------------------------------------------------------------------

procedure VSTBeginUpdate;
begin
  frmMain.VST.Tag := frmMain.VST.Tag + 1;
end;

// -----------------------------------------------------------------------------

procedure VSTEndUpdate;
begin
  frmMain.VST.Tag := frmMain.VST.Tag - 1;
end;

// -----------------------------------------------------------------------------

function VSTFocusedNode: Integer;
begin
  if Assigned(frmMain.VST.FocusedNode) then
    Result := frmMain.VST.FocusedNode^.Index
  else
    Result := -1;
end;

// -----------------------------------------------------------------------------

function VSTLastSelectedNode: Integer;
var
  Node: PVirtualNode;
begin
  Result := -1;
  Node   := frmMain.VST.GetFirstSelected;
  if Assigned(Node) then
  begin
    while Assigned(frmMain.VST.GetNextSelected(Node)) do
      Node := frmMain.VST.GetNextSelected(Node);

    Result := Node^.Index;
  end;
end;

// -----------------------------------------------------------------------------

function VSTGetNodeAtIndex(const Index: Integer): PVirtualNode;
var
  Run : PVirtualNode;
begin
  Result := NIL;

  with frmMain.VST do
    if TotalCount > 0 then
    begin
      Run := GetFirst;
      while Assigned(Run) do
      begin
        if Index = Run^.Index then
        begin
          Result := Run;
          Break;
        end;
        Run := GetNext(Run);
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure VSTSelectNode(const Index: Integer; const AClear: Boolean);
var
  Node: PVirtualNode;
begin
  Node := VSTGetNodeAtIndex(Index);
  VSTSelectNode(Node, AClear);
end;

// -----------------------------------------------------------------------------

procedure VSTSelectNode(const Node: PVirtualNode; const AClear: Boolean);
begin
  if Assigned(Node) then
    with frmMain.VST do
    begin
      if AClear then ClearSelection;
      FocusedNode    := Node;
      Selected[Node] := True;
      ScrollIntoView(Node, True);
    end;
end;

// -----------------------------------------------------------------------------

procedure VSTSelectNodes(const Idxs: TIntegerDynArray; const AClear: Boolean);
var
  Run : PVirtualNode;
  i   : Integer;
begin
  if Length(Idxs) = 0 then Exit;

  with frmMain.VST do
    if TotalCount > 0 then
    begin
      if AClear then ClearSelection;
      Run := GetFirst;
      while Assigned(Run) do
      begin
        for i := Low(Idxs) to High(Idxs) do
        if Idxs[i] = Run^.Index then
        begin
          Selected[Run] := True;
        end;
        Run := GetNext(Run);
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure VSTPaintCell(const TargetCanvas: TCanvas; const CellRect: TRect; const Color: TColor);
var
  PenStyle : TPenStyle;
  AColor   : TColor;
begin
  AColor   := TargetCanvas.Brush.Color;
  PenStyle := TargetCanvas.Pen.Style;
  TargetCanvas.Pen.Style   := psClear;
  TargetCanvas.Brush.Color := Color;
  TargetCanvas.FillRect(CellRect);
  TargetCanvas.Pen.Style   := PenStyle;
  TargetCanvas.Brush.Color := AColor;
end;

// -----------------------------------------------------------------------------

procedure VSTShowColumn(const Index: Integer; const Visible: Boolean);
begin
  with frmMain.VST.Header.Columns[Index] do
    if Visible then
      Options := Options + [coVisible]
    else
      Options := Options - [coVisible];
end;

// -----------------------------------------------------------------------------

procedure VSTInsertSubtitles(const Before: Boolean = False);
var
  Run       : PVirtualNode;
  Item      : PUWSubtitleItem;
  ti, tf    : Integer;
  pos, i    : Integer;
  SelArray  : TIntegerDynArray;
begin
  // Multiple subtitles selected
  if frmMain.VST.SelectedCount > 1 then
  begin
    i := 0;
    SetLength(SelArray, frmMain.VST.SelectedCount);
    try
      Run := frmMain.VST.GetFirstSelected;
      while Assigned(Run) do
      begin
        Item := Subtitles.ItemPointer[Run^.Index];
        if Assigned(Item) then
        begin
          ti  := (Item^.FinalTime + 1);
          tf  := ti + Options.NewSubtitleMS;
          SelArray[i] := InsertSubtitle(Run^.Index+1, ti, tf, '', '', False);
          Inc(i);
        end;
        Run := frmMain.VST.GetNextSelected(Run);
      end;
      VSTSelectNodes(SelArray, True);
    finally
      SetLength(SelArray, 0);
      IncrementUndoGroup;
    end;
  end
  else // only one
  begin
    pos := -1;
    i   := VSTFocusedNode;

    if i >= 0 then
    begin
      //if mpMedia.State = TMediaState.Unavailable then
        ti := Subtitles[i].FinalTime + 1;
      //else
     //   ti := MediaTimeToMS(mpMedia.CurrentTime); //(mpMedia.CurrentTime div MediaTimeScale) * 1000;

      if not before then
        pos := i+1
      else
        pos := i;
    end
    else
      ti := Subtitles.Items[Subtitles.Count].FinalTime + 1;

    //ti := ti + Options.FakeStart;
    tf := ti + Options.NewSubtitleMS;
    VSTSelectNode(InsertSubtitle(pos, ti, tf, '', ''), True);
  end;
end;

// -----------------------------------------------------------------------------

procedure VSTDeleteSubtitles;
var
  Nodes : TNodeArray;
  I     : Integer;
begin
  Nodes := NIL;
  if frmMain.VST.SelectedCount > 0 then
  begin
    frmMain.VST.BeginUpdate;
    try
      Nodes := frmMain.VST.GetSortedSelection(True);
      for I := High(Nodes) downto 1 do
      begin
        DeleteSubtitle(Nodes[I]^.Index, False, False);
      end;
      DeleteSubtitle(Nodes[0]^.Index, False, False);
    finally
      frmMain.VST.ClearSelection;
      frmMain.UpdateValues(True);
      IncrementUndoGroup;
      frmMain.VST.EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure VSTMarkSubtitles(const Value: Boolean);
var
  Run : PVirtualNode;
begin
  with frmMain.VST do
    if SelectedCount > 0 then
    begin
      Run := GetFirstSelected;
      while Assigned(Run) do
      begin
        Undo.AddUndo(utSubtitleChange, Run^.Index, Subtitles[Run^.Index], LastUndoGroup);
        Subtitles.ItemPointer[Run^.Index]^.Marked := Value;
        Run := GetNextSelected(Run);
      end;
      Invalidate;
      IncrementUndoGroup;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure VSTCopySubtitlesToClipboard(const Cut: Boolean = False);
var
  Nodes : TNodeArray;
  I     : Integer;
  s     : String;
begin
  Nodes := NIL;
  if frmMain.VST.SelectedCount > 0 then
  begin
    frmMain.VST.BeginUpdate;
    try
      Nodes := frmMain.VST.GetSortedSelection(True);
      for I := 0 to High(Nodes) do
        with Subtitles[Nodes[I]^.Index] do
        begin
          s := Format('%s%d||%d||%s||%s||', [s, InitialTime, FinalTime,
            ReplaceRegExpr('\'+sLineBreak, Text, '\~', False),
            ReplaceRegExpr('\'+sLineBreak, Translation, '\~', False)]);
        end;
    finally
      Clipboard.AsText := s;
      if Cut then VSTDeleteSubtitles;
      frmMain.UpdateValues(True);
      IncrementUndoGroup;
      frmMain.VST.EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure VSTPasteSubtitlesFromClipboard;
var
  s      : TStringList;
  i, x   : Integer;
  t1, t2 : Integer;
begin
  if frmMain.VST.SelectedCount > 0 then
  begin
    frmMain.VST.BeginUpdate;
    s := TStringList.Create;
    try
      x := VSTFocusedNode;
      frmMain.VST.ClearSelection;
      SplitRegExpr('\|\|', Clipboard.AsText, s);
      while s.Count > 4 do
      begin
        t1 := StrToIntDef(s[0], 0);
        t2 := StrToIntDef(s[1], 0);
        InsertSubtitle(x, t1, t2,
          ReplaceRegExpr('\\~', s[2], sLineBreak, False),
          ReplaceRegExpr('\\~', s[3], sLineBreak, False), False);
        frmMain.VST.Selected[VSTGetNodeAtIndex(x)] := True;
        inc(x);
        for i := 1 to 4 do s.Delete(0);
      end;
    finally
      s.Free;
      frmMain.UpdateValues(True);
      IncrementUndoGroup;
      frmMain.VST.EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function VSTFind(const FindText: String; const CaseSensitive: Boolean; const FromBegining: Boolean; const Backward: Boolean = False; const Replace: Boolean = False; const NewText: String = ''; const ReplaceAll: Boolean = False; const CasePreserve: Boolean = False; const RE: Boolean = False): Boolean;

  function FoundText(const S: String): Boolean;
  begin
    if not RE then
      Result := (CaseSensitive and (Pos(FindText, S) > 0)) or
                (not CaseSensitive and (PosCS(FindText, S) > 0))
    else
    begin
      if not CaseSensitive and not Contains('(?i)', FindText) then
        Result := PosRE('(?i)'+FindText, S) > -1
      else
        Result := PosRE(FindText, S) > -1;
    end;
  end;

var
  Item : PUWSubtitleItem;
  i, c : Integer;
  s    : String;
begin
  Result := False;
  Options.TextToFind := FindText;

  if Subtitles.Count > 0 then
  begin
    if Backward then
    begin
      for i := (VSTFocusedNode-1) downto 0 do
      begin
        Item := Subtitles.ItemPointer[i];
        if Assigned(Item) then
          with Item^ do
          begin
            if FoundText(Text) or FoundText(Translation) then
            begin
              Result := True;
              VSTSelectNode(i, True);
              Break;
            end;
          end;
      end;
      Exit;
    end;

    if FromBegining then
      c := 0
    else
      c := Range(VSTFocusedNode+1, 0, Subtitles.Count-1);

    for i := c to Subtitles.Count-1 do
    begin
      Item := Subtitles.ItemPointer[i];
      if Assigned(Item) then
        with Item^ do
        begin
          if FoundText(Text) or FoundText(Translation) then
          begin
            Result := True;
            VSTSelectNode(i, True);
            if not Replace then // Only Find...
              Break
            else
            begin // Find & Replace...
              if FoundText(Text) then
              begin
                if not RE then
                  s := ReplaceString(Text, FindText, iff(CasePreserve, PreserveCase(FindText, NewText), NewText), False)
                else
                  s := ReplaceRegExpr(FindText, Text, iff(CasePreserve, PreserveCase(FindText, NewText), NewText), False);

                SetSubtitleText(i, s, True, False);
                //SubtitleChanged(True, False);
              end;
              if FoundText(Translation) then
              begin
                if not RE then
                  s := ReplaceString(Translation, FindText, iff(CasePreserve, PreserveCase(FindText, NewText), NewText), False)
                else
                  s := ReplaceRegExpr(FindText, Translation, iff(CasePreserve, PreserveCase(FindText, NewText), NewText), False);

                SetSubtitleText(i, s, False, False);
                //SubtitleChanged(False, True);
              end;
              if not ReplaceAll then Break;
            end;
          end;
        end;
    end;
  end;
  if Replace then frmMain.UpdateValues(True);
end;

// -----------------------------------------------------------------------------

procedure VSTDoLoop(Proc: TUWSubtitleDoLoopProc; const Selection: TUWSubtitleDoLoopSelection = dlSelected; const Refresh: Boolean = True; const IncrementUndo: Boolean = False; const CBProc: TUWSubtitleDoLoopProcCB = NIL);
var
  Item   : PUWSubtitleItem;
  i, c   : Integer;
  Run    : PVirtualNode;
  Cancel : Boolean;
begin
  if Selection <> dlSelected then
  begin
    c := 0;

    if (Selection = dlSelected) or (Selection = dlCurrentToLast) then
      c := Range(VSTFocusedNode, 0, Subtitles.Count-1);

    for i := c to Subtitles.Count-1 do
    begin
      Item := Subtitles.ItemPointer[i];
      if Assigned(Item) then
      begin
        if (Selection <> dlMarked) or ((Selection = dlMarked) and Item^.Marked) then
        begin
          Proc(Item, i);
          if Assigned(CBProc) then
          begin
            CBProc(i, Subtitles.Count-1, Cancel);
            Application.ProcessMessages;
            if Cancel then Break;
          end;
        end;
      end;
    end;
  end
  else
  begin
    i := 0;
    Run := frmMain.VST.GetFirstSelected;
    while Assigned(Run) do
    begin
      Item := Subtitles.ItemPointer[Run^.Index];
      if Assigned(Item) then
      begin
        Proc(Item, Run^.Index);
        if Assigned(CBProc) then
        begin
          Inc(i);
          CBProc(i, frmMain.VST.SelectedCount-1, Cancel);
          Application.ProcessMessages;
          if Cancel then Break;
        end;
      end;
      Run := frmMain.VST.GetNextSelected(Run);
    end;
  end;

  if IncrementUndo then
  begin
    IncrementUndoGroup;
    SubtitleChanged(True, True);
  end;
  if Refresh then frmMain.UpdateValues(True);
end;

// -----------------------------------------------------------------------------

procedure DoFastDivideSubtitles;
var
  s    : TStringList;
  NodeArray : TNodeArray;
  Item : PUWSubtitleItem;
  i, c, x, t1,t2 : Integer;
begin
  if frmMain.VST.SelectedCount > 0 then
  begin
    s := TStringList.Create;
    try
      NodeArray := frmMain.VST.GetSortedSelection(False);
      if NodeArray = NIL then Exit;
      for i := High(NodeArray) downto Low(NodeArray) do
      begin
        x    := frmMain.VST.AbsoluteIndex(NodeArray[i]);
        Item := Subtitles.ItemPointer[x];
        if Assigned(Item) then
        with Item^, Options.ErrCfg do
        begin
          SplitRegExpr('\|\|', FastDivideLines(Text, InitialTime, FinalTime, Options.DotsOnSplit, MaxLineLength), s);
          DeleteSubtitle(x, False, False);

          while s.Count >= 3 do
          begin
            t1 := StrToIntDef(s[0], 0);
            t2 := StrToIntDef(s[1], 0);
            InsertSubtitle(x, t1, t2, s[2], '', False, False);
            for c := 1 to 3 do s.Delete(0);
            inc(x);
          end;
        end;
      end;
      NodeArray := NIL;
    finally
      s.Free;
      IncrementUndoGroup;
      SubtitleChanged(True, True);
      frmMain.UpdateValues(True);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure ApplyChangeFPS(const Item: PUWSubtitleItem; const Index: Integer);
var
  it, ft : Integer;
begin
  Undo.AddUndo(utSubtitleChange, Index, Item^, LastUndoGroup);
  with Item^ do
  begin
    it          := TimeToFrames(InitialTime, FPS.FPS);
    ft          := TimeToFrames(FinalTime, FPS.FPS);
    InitialTime := FramesToTime(it, GetFPS);
    FinalTime   := FramesToTime(ft, GetFPS);
  end;
end;

// -----------------------------------------------------------------------------

procedure ApplyCheckErrors(const Item: PUWSubtitleItem; const Index: Integer);
begin
  Item^.ErrorType := CheckErrors(Subtitles, Index, Options.ErrOptions, Options.ErrCfg);
end;

// -----------------------------------------------------------------------------

procedure ApplyFixErrors(const Item: PUWSubtitleItem; const Index: Integer);
begin
  Item^ := FixErrors(Subtitles, Index, Options.ErrOptions, Options.ErrCfg);
end;

// -----------------------------------------------------------------------------

procedure ApplySetTimeInitialFromSpin(const Item: PUWSubtitleItem; const Index: Integer);
begin
  SetSubtitleTime(Index, CorrectTime(frmMain.tedInitial.Value), frmMain.tedInitial.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplySetTimeFinalFromSpin(const Item: PUWSubtitleItem; const Index: Integer);
begin
  SetSubtitleTime(Index, CorrectTime(frmMain.tedFinal.Value), frmMain.tedFinal.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplySetTimeDurationFromSpin(const Item: PUWSubtitleItem; const Index: Integer);
begin
  SetSubtitleTime(Index, CorrectTime(frmMain.tedDuration.Value), frmMain.tedDuration.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplySetTimePauseFromSpin(const Item: PUWSubtitleItem; const Index: Integer);
begin
  SetSubtitleTime(Index, CorrectTime(frmMain.tedPause.Value), frmMain.tedPause.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplySetTimeInitialFromVLC(const Item: PUWSubtitleItem; const Index: Integer);
begin
  SetSubtitleTime(Index, CorrectTime(frmMain.MPV.Engine.Position), frmMain.tedInitial.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplySetTimeFinalFromVLC(const Item: PUWSubtitleItem; const Index: Integer);
begin
  SetSubtitleTime(Index, CorrectTime(frmMain.MPV.Engine.Position), frmMain.tedFinal.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplySetTextFromEdit(const Item: PUWSubtitleItem; const Index: Integer);
begin
  SetSubtitleText(Index, frmMain.mmoText.Text, True);
end;

// -----------------------------------------------------------------------------

procedure ApplySetTranslationFromEdit(const Item: PUWSubtitleItem; const Index: Integer);
begin
  SetSubtitleText(Index, frmMain.mmoTranslation.Text, False);
end;

// -----------------------------------------------------------------------------

procedure ApplyShiftToPrevious(const Item: PUWSubtitleItem; const Index: Integer);
var
  NewTime: Integer;
begin
  if Index > 0 then
    NewTime := Subtitles.FinalTime[Index-1] + Options.DefSubtitlePauseMS
  else
    NewTime := Subtitles.InitialTime[Index];

  if NewTime <> Subtitles.InitialTime[Index] then
    SetSubtitleTime(Index, CorrectTime(NewTime), frmMain.tedInitial.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplyShiftToNext(const Item: PUWSubtitleItem; const Index: Integer);
var
  NewTime: Integer;
begin
  if Index < (Subtitles.Count-1) then
    NewTime := Subtitles.InitialTime[Index+1] - Options.DefSubtitlePauseMS
  else
    NewTime := Subtitles.FinalTime[Index];

  if NewTime <> Subtitles.FinalTime[Index] then
    SetSubtitleTime(Index, CorrectTime(NewTime), frmMain.tedFinal.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplyAutomaticDuration(const Item: PUWSubtitleItem; const Index: Integer);
var
  NewTime: Integer;
begin
  with Subtitles[Index] do
    NewTime := InitialTime + AutomaticDurations(Text, Subtitles.Duration[Index], 60, 50, 50, TAutomaticDurationMode.dmAlwaysNew);

  if NewTime <> Subtitles.FinalTime[Index] then
    SetSubtitleTime(Index, CorrectTime(NewTime), frmMain.tedFinal.Tag);
end;

// -----------------------------------------------------------------------------

procedure ApplyDefaultPause(const Item: PUWSubtitleItem; const Index: Integer);
begin
  if Index > 0 then
  begin
    if Subtitles.Pause[Index] < Options.DefSubtitlePauseMS then
      SetSubtitleTime(Index, CorrectTime(Options.DefSubtitlePauseMS), frmMain.tedPause.Tag);
  end;
end;

// -----------------------------------------------------------------------------

procedure ApplyAlign(const Item: PUWSubtitleItem; const Index: Integer);
begin
  Undo.AddUndo(utSubtitleChange, Index, Subtitles[Index], LastUndoGroup);

  with frmMain do
    if actAlignToLeft.Checked then
      Item^.Align := 1
    else if actAlignToCenter.Checked then
      Item^.Align := 2
    else if actAlignToRight.Checked then
      Item^.Align := 3
    else
      Item^.Align := 0;

  IncrementUndoGroup;
end;

// -----------------------------------------------------------------------------

procedure ApplyXY(const Item: PUWSubtitleItem; const Index: Integer);
begin
  Undo.AddUndo(utSubtitleChange, Index, Subtitles[Index], LastUndoGroup);

  with frmMain do
    Item^.R := Rect(numLeft.Value, numTop.Value, numRight.Value, numBottom.Value);

  IncrementUndoGroup;
end;

// -----------------------------------------------------------------------------

procedure ApplyActor(const Item: PUWSubtitleItem; const Index: Integer);
begin
  Undo.AddUndo(utSubtitleChange, Index, Subtitles[Index], LastUndoGroup);

  with frmMain do
    Item^.Actor := cboActor.Text;

  IncrementUndoGroup;
end;

// -----------------------------------------------------------------------------

procedure ApplyStyle(const Item: PUWSubtitleItem; const Index: Integer);
begin
  Undo.AddUndo(utSubtitleChange, Index, Subtitles[Index], LastUndoGroup);

  with frmMain do
    Item^.Style := cboStyle.Text;

  IncrementUndoGroup;
end;

// -----------------------------------------------------------------------------

procedure ApplyText(const Item: PUWSubtitleItem; const Index: Integer; const NewSubtitleText, NewSubtitleTranslation: String; const Resent: Boolean = True);
begin
  SetSubtitleText(Index, NewSubtitleText, True, False, False, Resent);
  SetSubtitleText(Index, NewSubtitleTranslation, False, False, Resent);
end;

// -----------------------------------------------------------------------------

function ApplyTag(const Text: String; const Tag: Char): String;
var
  sTag, eTag: String;
begin
  if Text <> '' then
  begin
    sTag := swt_StartTag + '\' + Tag + '1' + swt_EndTag;
    eTag := swt_StartTag + '\' + Tag + '0' + swt_EndTag;

    if AnsiStartsText(sTag, Text) and AnsiEndsText(eTag, Text) then
      Result := Copy(Text, Length(sTag)+1, Length(Text) - (Length(sTag)+Length(eTag)))
    else
      Result := sTag + Text + eTag;
  end
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

function ApplyTagColor(const Text: String; const Color: String): String;
begin
  if Text <> '' then
    Result := Format('%s\%s&%s&%s%s%s\%s%s',
      [swt_StartTag, swt_Color, Color, swt_EndTag, Text, swt_StartTag, swt_Color, swt_EndTag])
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

function ApplyCustomTag(const Text: String; const Tag: String): String;
begin
  if Text <> '' then
    Result := Format('%s %s %s', [Tag, Text, Tag])
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

procedure ApplyFontBold(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, ApplyTag(Text, swt_Bold), ApplyTag(Translation, swt_Bold), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyFontItalic(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, ApplyTag(Text, swt_Italic), ApplyTag(Translation, swt_Italic), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyFontStrikeOut(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, ApplyTag(Text, swt_StrikeOut), ApplyTag(Translation, swt_StrikeOut), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyFontUnderline(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, ApplyTag(Text, swt_Underline), ApplyTag(Translation, swt_Underline), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyFontColor(const Item: PUWSubtitleItem; const Index: Integer);
var
  AColor: String;
begin
  with frmMain do
    AColor := Format('%s%s%s', [IntToHex(LCLIntf.GetBValue(LastSubtitle.Color), 2),
               IntToHex(LCLIntf.GetGValue(LastSubtitle.Color), 2),
               IntToHex(LCLIntf.GetRValue(LastSubtitle.Color), 2)]);

  with Item^ do
    ApplyText(Item, Index, ApplyTagColor(Text, AColor), ApplyTagColor(Translation, AColor), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyFontClear(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, RemoveSWTags(Text), RemoveSWTags(Translation), False);
end;

// -----------------------------------------------------------------------------

procedure ApplySingTag(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, ApplyCustomTag(Text, swt_Sing), ApplyCustomTag(Translation, swt_Sing), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyUnbreakSubtitles(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^, Options.ErrCfg do
    ApplyText(Item, Index, UnbreakSubtitles(Text), UnbreakSubtitles(Translation), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyAutoBreakSubtitles(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^, Options.ErrCfg do
    ApplyText(Item, Index, AutoBreakSubtitle(Text, MaxLineLength), AutoBreakSubtitle(Translation, MaxLineLength), False);
end;

// -----------------------------------------------------------------------------

procedure ApplySetMaximumLineLength(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^, Options.ErrCfg do
    ApplyText(Item, Index, SetMaximumLineLength(Text, MaxLineLength), SetMaximumLineLength(Translation, MaxLineLength), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyReverseText(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, ReverseText(Text), ReverseText(Translation), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyFixRTLPunctuation(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^ do
    ApplyText(Item, Index, FixRTLPunctuation(Text), FixRTLPunctuation(Translation), False);
end;

// -----------------------------------------------------------------------------

procedure ApplyExtendLength(const Item: PUWSubtitleItem; const Index: Integer);
var
  NewTime: Cardinal;
begin
  if Index < (Subtitles.Count-1) then
  begin
    NewTime := Subtitles.InitialTime[Index+1];
    SetSubtitleTime(Index, ExtendLength(NewTime), frmMain.tedFinal.Tag, False, False);
  end;
end;

// -----------------------------------------------------------------------------

procedure ApplyShiftTimeMore(const Item: PUWSubtitleItem; const Index: Integer);
var
  it, ft: Cardinal;
begin
  with Item^ do
  begin
    ShiftTime(InitialTime, FinalTime, Options.ShiftTimeMS, it, ft);
    SetSubtitleTime(Index, it, frmMain.tedInitial.Tag, False, False);
    SetSubtitleTime(Index, ft, frmMain.tedFinal.Tag, False, False);
  end;
end;

// -----------------------------------------------------------------------------

procedure ApplyShiftTimeLess(const Item: PUWSubtitleItem; const Index: Integer);
var
  it, ft: Cardinal;
begin
  with Item^ do
  begin
    ShiftTime(InitialTime, FinalTime, -Options.ShiftTimeMS, it, ft);
    SetSubtitleTime(Index, it, frmMain.tedInitial.Tag, False, False);
    SetSubtitleTime(Index, ft, frmMain.tedFinal.Tag, False, False);
  end;
end;

// -----------------------------------------------------------------------------

procedure GetTMatIndex(const Index: Integer);
var
  s: String;
begin
  if (frmTM = NIL) or (frmMain.VST.SelectedCount <> 1) or (not TranslatorMode) then Exit;
  s := frmTM.GetItemAtIndex(Index);
  if s <> '' then frmMain.mmoTranslation.Text := s;
end;

// -----------------------------------------------------------------------------

function GetInstallFolderVLC: String;
{$IFDEF WINDOWS}
var
  reg : TRegistry;
begin
  Result := '';
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKeyReadOnly('Software\VideoLAN\VLC');
    Result := IncludeTrailingPathDelimiter(reg.ReadString('InstallDir'));
  finally
    reg.Free;
  end;
{$ENDIF}

{$IFDEF DARWIN}
const
  pathLst : array[0..2] of string = (
    '~/Desktop/VLC.app/Contents/MacOS/VLC',
    '~/Applications/VLC.app/Contents/MacOS/VLC',
    '/Applications/VLC.app/Contents/MacOS/VLC'
    );
var
  pathIdx : Integer;
  pathStr : string;
 begin
  Result := '';
  for pathIdx := Low(pathLst) to High(pathLst) do
  begin
    pathStr := pathLst[pathIdx];

    if not DirectoryExists(pathStr) then Continue;
    if FileExists(IncludeTrailingPathDelimiter(pathStr + VLC_EXE)) then
    begin
      Result := IncludeTrailingPathDelimiter(pathStr);
      Exit;
    end;
  end;
{$ENDIF}

{$IFDEF LINUX}
 const
  pathLst : array[0..6] of string = (
    '/usr/lib',
    '/lib',
    '/usr/local/lib',
    '/lib64',
    '/usr/lib64',
    '/usr/lib/x86_64-linux-gnu',
    '/snap/vlc/current/usr/lib'
  );
var
  pathIdx : Integer;
  pathStr : string;
  sr : TSearchRec;
  re : Integer;
begin
  Result := '';
  for pathIdx := Low(pathLst) to High(pathLst) do
  begin
    pathStr := pathLst[pathIdx];
    if not DirectoryExists(pathStr) then continue;
    if FileExists(IncludeTrailingPathDelimiter(pathStr + VLC_EXE)) then
    begin
      Result := IncludeTrailingPathDelimiter(pathStr);
      Exit;
    end;
  end;
{$ENDIF}
end;

// -----------------------------------------------------------------------------

end.

