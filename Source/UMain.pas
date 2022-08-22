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

unit UMain;

// -----------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, LCLIntf, Graphics, Dialogs,
  ComCtrls, ActnList, Menus, StdCtrls, Buttons, ExtCtrls, LCLType, UWControls,
  VirtualTrees, UUndo, UWSubtitleAPI, UWSubtitleAPI.Formats, UWMediaEngine,
  UWControls.WaveformDisplay, uPSComponent, uPSCompiler, uPSRuntime, uPSUtils;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    aclActions: TActionList;
    actInsertSubtitle: TAction;
    actDeleteSubtitle: TAction;
    actInsertSubtitleBefore: TAction;
    actAudioPreview: TAction;
    actCut: TAction;
    actCopy: TAction;
    actAutomaticDuration: TAction;
    actDefaultPause: TAction;
    actAlignToNone: TAction;
    actAlignToLeft: TAction;
    actAlignToCenter: TAction;
    actAlignToRight: TAction;
    actFontBold: TAction;
    actFontItalic: TAction;
    actFontUnderline: TAction;
    actFontStrikeout: TAction;
    actFontClear: TAction;
    actFontColor: TAction;
    actFind: TAction;
    actFindNext: TAction;
    actActors: TAction;
    actExecuteExtension: TAction;
    actGoTo: TAction;
    actCloseSubtitle: TAction;
    actExit: TAction;
    actAbout: TAction;
    actConvertCase: TAction;
    actCloseVideo: TAction;
    actAutoBreakSubtitle: TAction;
    actFixRTLPunctuation: TAction;
    actFastDivideSubtitle: TAction;
    actDurationLimits: TAction;
    actAutomaticDurations: TAction;
    actExtendLength: TAction;
    actFontColorDlg: TAction;
    actInfoAndErrors: TAction;
    actDockVideoControls: TAction;
    actFindPrevious: TAction;
    actExtractWaveform: TAction;
    actCustomSearch: TAction;
    actGlossary: TAction;
    actChangeWorkspace: TAction;
    actSettings: TAction;
    actViewWPM: TAction;
    actTMValidate: TAction;
    actTM3: TAction;
    actTM2: TAction;
    actTM1: TAction;
    actViewCPS: TAction;
    actNewProject: TAction;
    actTM: TAction;
    actMediaPreviousFrame: TAction;
    actSubtitlePosBottom: TAction;
    actSubtitlePosTop: TAction;
    actOpenVideo: TAction;
    actSingTag: TAction;
    actShiftTimeLess: TAction;
    actShiftTimeMore: TAction;
    actSetDelay: TAction;
    actTimeExpander: TAction;
    actReverseText: TAction;
    actSetMaximumLineLength: TAction;
    actUnbreakSubtitle: TAction;
    actTranslate: TAction;
    actViewToolbarAdditional: TAction;
    actViewCPL: TAction;
    actViewStyle: TAction;
    actViewDuration: TAction;
    actViewTimes: TAction;
    actViewNumber: TAction;
    actSpellCheck: TAction;
    actSelectAll: TAction;
    actReplace: TAction;
    actQuickFind: TAction;
    actSubtitleDblClick: TAction;
    actStyles: TAction;
    actSimpleMode: TAction;
    actMediaAddSubtitle: TAction;
    actMediaPlayAfterSelection: TAction;
    actMediaPlayBeforeSelection: TAction;
    actMediaChangePlayRate: TAction;
    actMediaZoomSelection: TAction;
    actMediaZoomOut: TAction;
    actMediaZoomIn: TAction;
    actMediaPlayFromSelection: TAction;
    actMediaPlaySelection: TAction;
    actMediaEndSubtitle: TAction;
    actMediaStartSubtitle: TAction;
    actMediaSetFinalTime: TAction;
    actMediaSetInitialTime: TAction;
    actMediaForwardEx: TAction;
    actMediaRewindEx: TAction;
    actMediaNextFrame: TAction;
    actMediaForward: TAction;
    actMediaRewind: TAction;
    actShiftToNext: TAction;
    actShiftToPrevious: TAction;
    actNextSubtitle: TAction;
    actPreviousSubtitle: TAction;
    actMediaAutoScroll: TAction;
    actMediaStop: TAction;
    actMediaPlay: TAction;
    actNewSubtitle: TAction;
    actPaste: TAction;
    actSaveSubtitleAs: TAction;
    actLoadSubtitle: TAction;
    actVideoPreview: TAction;
    actUnMarkSubtitle: TAction;
    actMarkSubtitle: TAction;
    actRedo: TAction;
    actUndo: TAction;
    actTranslatorMode: TAction;
    actModeFrames: TAction;
    actModeTime: TAction;
    cboFind: TComboBox;
    cboStyle: TComboBox;
    cboActor: TComboBox;
    cboInputFPS: TComboBox;
    cboFPS: TComboBox;
    cboFormat: TComboBox;
    cboEncoding: TComboBox;
    cpsText: TUWCPSBar;
    cpsTranslation: TUWCPSBar;
    imlET: TImageList;
    imgFind: TImage;
    imlMain: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    lblMediaTime: TLabel;
    lblText: TLabel;
    lblTranslation: TLabel;
    MenuItem1: TMenuItem;
    MenuItem100: TMenuItem;
    MenuItem101: TMenuItem;
    MenuItem102: TMenuItem;
    MenuItem103: TMenuItem;
    MenuItem104: TMenuItem;
    MenuItem105: TMenuItem;
    MenuItem106: TMenuItem;
    MenuItem107: TMenuItem;
    MenuItem108: TMenuItem;
    MenuItem109: TMenuItem;
    MenuItem110: TMenuItem;
    MenuItem112: TMenuItem;
    MenuItem113: TMenuItem;
    MenuItem114: TMenuItem;
    MenuItem115: TMenuItem;
    MenuItem116: TMenuItem;
    MenuItem117: TMenuItem;
    MenuItem118: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem7: TMenuItem;
    mnuVST_Validate: TMenuItem;
    mnuWorkspace: TMenuItem;
    MenuItem111: TMenuItem;
    mnuVideoAudio: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    mnuExit: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem71: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem80: TMenuItem;
    MenuItem81: TMenuItem;
    MenuItem82: TMenuItem;
    MenuItem83: TMenuItem;
    MenuItem84: TMenuItem;
    MenuItem85: TMenuItem;
    MenuItem86: TMenuItem;
    MenuItem87: TMenuItem;
    MenuItem88: TMenuItem;
    MenuItem89: TMenuItem;
    MenuItem90: TMenuItem;
    MenuItem91: TMenuItem;
    MenuItem92: TMenuItem;
    MenuItem93: TMenuItem;
    MenuItem94: TMenuItem;
    MenuItem95: TMenuItem;
    MenuItem96: TMenuItem;
    MenuItem97: TMenuItem;
    MenuItem98: TMenuItem;
    MenuItem99: TMenuItem;
    mnuPopVideoTextPosition: TMenuItem;
    popPlayRate: TPopupMenu;
    popVSTHeader: TPopupMenu;
    popWAVE: TPopupMenu;
    Separator12: TMenuItem;
    mnuVideoTextPosition: TMenuItem;
    popVideo: TPopupMenu;
    Separator11: TMenuItem;
    Separator10: TMenuItem;
    mnuVideoSubtitles: TMenuItem;
    Separator13: TMenuItem;
    Separator14: TMenuItem;
    Separator15: TMenuItem;
    Separator16: TMenuItem;
    mnuVST_SepValidate: TMenuItem;
    Separator17: TMenuItem;
    Separator9: TMenuItem;
    Separator8: TMenuItem;
    mnuVideoPlayback: TMenuItem;
    Separator7: TMenuItem;
    mnuFormat: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    MenuItem76: TMenuItem;
    MenuItem77: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem79: TMenuItem;
    Separator6: TMenuItem;
    Separator5: TMenuItem;
    Separator4: TMenuItem;
    mnuTimings: TMenuItem;
    Separator3: TMenuItem;
    mnuSubtitles: TMenuItem;
    mnuRTL: TMenuItem;
    Separator2: TMenuItem;
    mnuTexts: TMenuItem;
    Separator1: TMenuItem;
    mnuToolbars: TMenuItem;
    mnuColumns: TMenuItem;
    mnuHelp: TMenuItem;
    mnuVideo: TMenuItem;
    mnuView: TMenuItem;
    mnuTools: TMenuItem;
    mnuFind: TMenuItem;
    mnuEdit: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    mnuDictionary: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnuMemoX: TMenuItem;
    mnuMain: TMainMenu;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mmoText: TMemo;
    mnuFile: TMenuItem;
    mmoTranslation: TMemo;
    numTop: TUWNumberBox;
    numRight: TUWNumberBox;
    numBottom: TUWNumberBox;
    popMRU: TPopupMenu;
    popExtensions: TPopupMenu;
    popMemo: TPopupMenu;
    popVST: TPopupMenu;
    psExtensions: TPSScript;
    sptWave: TSplitter;
    tlbVideoControls: TToolBar;
    tlbWaveControls: TToolBar;
    tlbTM: TToolBar;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    sbrSeek: TUWSeekBar;
    ToolButton23: TToolButton;
    ToolButton36: TToolButton;
    ToolButton37: TToolButton;
    ToolButton38: TToolButton;
    ToolButton39: TToolButton;
    ToolButton40: TToolButton;
    ToolButton41: TToolButton;
    ToolButton42: TToolButton;
    ToolButton43: TToolButton;
    ToolButton44: TToolButton;
    ToolButton45: TToolButton;
    tlbVLCControls: TToolButton;
    ToolButton46: TToolButton;
    ToolButton47: TToolButton;
    ToolButton48: TToolButton;
    ToolButton49: TToolButton;
    ToolButton50: TToolButton;
    ToolButton51: TToolButton;
    ToolButton52: TToolButton;
    tbnPlayRate: TToolButton;
    ToolButton53: TToolButton;
    ToolButton54: TToolButton;
    ToolButton55: TToolButton;
    ToolButton56: TToolButton;
    ToolButton57: TToolButton;
    ToolButton58: TToolButton;
    ttTimes: TUWTickTime;
    MPV: TUWMediaPlayer;
    lyoNotes: TUWLayout;
    WAVE: TUWWaveformDisplay;
    popMenuInsert: TPopupMenu;
    shpColor1: TShape;
    shpColor2: TShape;
    shpColor3: TShape;
    shpColor4: TShape;
    shpColor5: TShape;
    shpColor6: TShape;
    shpColor7: TShape;
    shpColor8: TShape;
    spdInitial: TSpeedButton;
    spdFinal: TSpeedButton;
    spdDuration: TSpeedButton;
    spdPause: TSpeedButton;
    spdStyle: TSpeedButton;
    spdActor: TSpeedButton;
    sptVideo: TSplitter;
    stbStatus: TStatusBar;
    tedFinal: TUWTimeEdit;
    tedDuration: TUWTimeEdit;
    tedPause: TUWTimeEdit;
    tlbEditorAlign: TToolBar;
    tlbMain: TToolBar;
    tlbEditor: TToolBar;
    tlbExtra: TToolBar;
    ToolButton1: TToolButton;
    lyoEditor: TUWLayout;
    lyoSubtitles: TUWLayout;
    lyoVideo: TUWLayout;
    lyoEditorLeftPanel: TUWLayout;
    lyoEditorTopPanel: TUWLayout;
    tedInitial: TUWTimeEdit;
    lyoEditorClient: TUWLayout;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton3: TToolButton;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    ToolButton33: TToolButton;
    ToolButton34: TToolButton;
    ToolButton35: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    numLeft: TUWNumberBox;
    lyoVideoControls: TUWLayout;
    VST: TVirtualStringTree;
    procedure actAboutExecute(Sender: TObject);
    procedure actActorsExecute(Sender: TObject);
    procedure actAlignToCenterExecute(Sender: TObject);
    procedure actAlignToLeftExecute(Sender: TObject);
    procedure actAlignToNoneExecute(Sender: TObject);
    procedure actAlignToRightExecute(Sender: TObject);
    procedure actAudioPreviewExecute(Sender: TObject);
    procedure actAutoBreakSubtitleExecute(Sender: TObject);
    procedure actAutomaticDurationExecute(Sender: TObject);
    procedure actAutomaticDurationsExecute(Sender: TObject);
    procedure actCloseSubtitleExecute(Sender: TObject);
    procedure actCloseVideoExecute(Sender: TObject);
    procedure actConvertCaseExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCustomSearchExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actDefaultPauseExecute(Sender: TObject);
    procedure actDeleteSubtitleExecute(Sender: TObject);
    procedure actDockVideoControlsExecute(Sender: TObject);
    procedure actDurationLimitsExecute(Sender: TObject);
    procedure actExecuteExtensionExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actExtendLengthExecute(Sender: TObject);
    procedure actExtractWaveformExecute(Sender: TObject);
    procedure actFastDivideSubtitleExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure actFindNextExecute(Sender: TObject);
    procedure actFindPreviousExecute(Sender: TObject);
    procedure actFixRTLPunctuationExecute(Sender: TObject);
    procedure actFontBoldExecute(Sender: TObject);
    procedure actFontClearExecute(Sender: TObject);
    procedure actFontColorDlgExecute(Sender: TObject);
    procedure actFontColorExecute(Sender: TObject);
    procedure actFontItalicExecute(Sender: TObject);
    procedure actFontStrikeoutExecute(Sender: TObject);
    procedure actFontUnderlineExecute(Sender: TObject);
    procedure actGlossaryExecute(Sender: TObject);
    procedure actGoToExecute(Sender: TObject);
    procedure actInfoAndErrorsExecute(Sender: TObject);
    procedure actInsertSubtitleBeforeExecute(Sender: TObject);
    procedure actInsertSubtitleExecute(Sender: TObject);
    procedure actLoadSubtitleExecute(Sender: TObject);
    procedure actMarkSubtitleExecute(Sender: TObject);
    procedure actMediaAddSubtitleExecute(Sender: TObject);
    procedure actMediaChangePlayRateExecute(Sender: TObject);
    procedure actMediaAutoScrollExecute(Sender: TObject);
    procedure actMediaEndSubtitleExecute(Sender: TObject);
    procedure actMediaForwardExecute(Sender: TObject);
    procedure actMediaForwardExExecute(Sender: TObject);
    procedure actMediaPlayAfterSelectionExecute(Sender: TObject);
    procedure actMediaPlayBeforeSelectionExecute(Sender: TObject);
    procedure actMediaPlayExecute(Sender: TObject);
    procedure actMediaPlayFromSelectionExecute(Sender: TObject);
    procedure actMediaPlaySelectionExecute(Sender: TObject);
    procedure actMediaPreviousFrameExecute(Sender: TObject);
    procedure actMediaRewindExecute(Sender: TObject);
    procedure actMediaNextFrameExecute(Sender: TObject);
    procedure actMediaRewindExExecute(Sender: TObject);
    procedure actMediaSetFinalTimeExecute(Sender: TObject);
    procedure actMediaSetInitialTimeExecute(Sender: TObject);
    procedure actMediaStartSubtitleExecute(Sender: TObject);
    procedure actMediaStopExecute(Sender: TObject);
    procedure actMediaZoomInExecute(Sender: TObject);
    procedure actMediaZoomOutExecute(Sender: TObject);
    procedure actMediaZoomSelectionExecute(Sender: TObject);
    procedure actModeFramesExecute(Sender: TObject);
    procedure actModeTimeExecute(Sender: TObject);
    procedure actNewProjectExecute(Sender: TObject);
    procedure actNewSubtitleExecute(Sender: TObject);
    procedure actNextSubtitleExecute(Sender: TObject);
    procedure actOpenVideoExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actPreviousSubtitleExecute(Sender: TObject);
    procedure actQuickFindExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actReverseTextExecute(Sender: TObject);
    procedure actSaveSubtitleAsExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSetDelayExecute(Sender: TObject);
    procedure actSetMaximumLineLengthExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actShiftTimeLessExecute(Sender: TObject);
    procedure actShiftTimeMoreExecute(Sender: TObject);
    procedure actShiftToNextExecute(Sender: TObject);
    procedure actShiftToPreviousExecute(Sender: TObject);
    procedure actSimpleModeExecute(Sender: TObject);
    procedure actSingTagExecute(Sender: TObject);
    procedure actSpellCheckExecute(Sender: TObject);
    procedure actStylesExecute(Sender: TObject);
    procedure actSubtitleDblClickExecute(Sender: TObject);
    procedure actSubtitlePosBottomExecute(Sender: TObject);
    procedure actSubtitlePosTopExecute(Sender: TObject);
    procedure actTimeExpanderExecute(Sender: TObject);
    procedure actTM1Execute(Sender: TObject);
    procedure actTM2Execute(Sender: TObject);
    procedure actTM3Execute(Sender: TObject);
    procedure actTMExecute(Sender: TObject);
    procedure actTMValidateExecute(Sender: TObject);
    procedure actTranslateExecute(Sender: TObject);
    procedure actTranslatorModeExecute(Sender: TObject);
    procedure actUnbreakSubtitleExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actUnMarkSubtitleExecute(Sender: TObject);
    procedure actVideoPreviewExecute(Sender: TObject);
    procedure actViewCPLExecute(Sender: TObject);
    procedure actViewCPSExecute(Sender: TObject);
    procedure actViewDurationExecute(Sender: TObject);
    procedure actViewNumberExecute(Sender: TObject);
    procedure actViewStyleExecute(Sender: TObject);
    procedure actViewTimesExecute(Sender: TObject);
    procedure actViewToolbarAdditionalExecute(Sender: TObject);
    procedure actChangeWorkspaceExecute(Sender: TObject);
    procedure actViewWPMExecute(Sender: TObject);
    procedure cboActorChange(Sender: TObject);
    procedure cboFindChange(Sender: TObject);
    procedure cboFPSSelect(Sender: TObject);
    procedure cboInputFPSKeyPress(Sender: TObject; var Key: char);
    procedure cboInputFPSSelect(Sender: TObject);
    procedure cboStyleChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mmoTextChange(Sender: TObject);
    procedure MPVAudioReconfig(Sender: TObject);
    procedure MPVClick(Sender: TObject);
    procedure MPVFileLoaded(Sender: TObject);
    procedure MPVPlay(Sender: TObject);
    procedure MPVStop(Sender: TObject);
    procedure MPVTimeChanged(Sender: TObject; AParam: Integer=0);
    procedure numLeftValueChange(Sender: TObject);
    procedure popMemoPopup(Sender: TObject);
    procedure popPlayRatePopup(Sender: TObject);
    procedure popAudioTrackSet(Sender: TObject);
    procedure popVSTHeaderPopup(Sender: TObject);
    procedure popVSTPopup(Sender: TObject);
    procedure psExtensionsCompile(Sender: TPSScript);
    procedure psExtensionsCompImport(Sender: TObject; x: TPSPascalCompiler);
    procedure psExtensionsExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter);
    function psExtensionsNeedFile(Sender: TObject;
      const OrginFileName: tbtstring; var FileName, Output: tbtstring): Boolean;
    procedure sbrSeekMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure shpColor1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tedInitialTimeChange(Sender: TObject; const NewTime: Cardinal);
    procedure UndoChanged(const ChangeType: UUndo.TUndoChangeType);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTDblClick(Sender: TObject);
    procedure VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const AText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VSTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure VSTHeaderMouseUp(Sender: TVTHeader; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VSTMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VSTResize(Sender: TObject);
    procedure WAVEPeakCreation(Sender: TObject;
      const EventType: TUWPeakCreationEventType; const Param: Integer);
    procedure WAVESelectedSubtitleItem(Sender: TObject; const Index: Integer;
      const SubtitleItem: TUWSubtitleItem; const IsDynamic: Boolean);
    procedure WAVESelectedSubtitleItemChange(Sender: TObject);
    procedure WAVESelectedSubtitleItemChanged(Sender: TObject;
      const Index: Integer; const OldInitialTime, OldFinalTime: Integer;
      const NeedSort: Boolean);
    procedure WAVESelectionChange(Sender: TObject);
  private
    { private declarations }
    procedure UpdateStatusBar;
    procedure MRUItemClick(Sender: TObject);
    procedure HunspellItemClick(Sender: TObject);
    procedure ShowFindAndReplace(const TabIndex: Integer = 0);
    procedure ShowTexts(const TabIndex: Integer = 0);
    procedure ShowTimings(const TabIndex: Integer = 0);
    procedure MediaUpdateProgress(const Time: Integer);
    procedure CheckTM;
  public
    { public declarations }
    procedure UpdateCPSAndTexts;
    procedure UpdateColorsInBoxes(const Index: Integer);
    procedure UpdateValues(const VSTInvalidate: Boolean = False);
    procedure EnableWorkArea(const Value: Boolean = True);
    function CloseSubtitle: Boolean;
    procedure LoadSubtitle(const FileName: String; const AEncoding: TEncoding = NIL; const AFPS: Single = -1; const AutoLoadVW: Boolean = True);
    procedure SaveSubtitle(const FileName: String; const Format: TUWSubtitleFormats; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
    procedure OpenVideo(const FileName: String; const Pos: Int64 = 0);
    procedure OpenAudio(const FileName: String);
    procedure DictionaryItemClick(Sender: TObject);
    procedure ExtensionItemClick(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

uses UWSystem.TimeUtils, UWSystem.StrUtils, UWSystem.SysUtils, UWFiles.MRU,
  UWSystem.Encoding, UWSpellCheck.Hunspell, UWSystem.HTMLUtils,
  UWSubtitleAPI.Tags, UTypes, UCommon, UExtensions, UFindAndReplace,
  UWControls.Utils, USpellCheck, UAbout, UWelcome, UTexts, UTimings,
  UWSubtitles.Utils, UInfoAndErrors, UVideo, UStylesAndActors,
  UWCustom.SWStyles, UAudioExtraction, UGlossary, UTM, UProject, UWTMX,
  USettings;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmMain }

// -----------------------------------------------------------------------------

procedure TfrmMain.FormCreate(Sender: TObject);
const
  mTed = 3;

var
  s: String;
begin
  Randomize; // used for tips
  DoubleBuffered := True;

  // Initialize SubtitleAPI
  Subtitles := TUWSubtitles.Create;
  // Undo engine
  Undo := TUndo.Create;
  Undo.OnChange := @UndoChanged;
  // VST Node/Header size
  TextSize  := Canvas.TextExtent('W');
  VST.Header.Columns[0].Width := (TextSize.cx*7);
  VST.Header.Columns[1].Width := (TextSize.cx*8);
  VST.Header.Columns[2].Width := (TextSize.cx*7);
  VST.Header.Columns[3].Width := (TextSize.cx*7);
  VST.Header.Columns[6].Width := (TextSize.cx*5);
  // Our FormatSettings
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := FormatSettings.DecimalSeparator;
  // Settings
  DefaultSettings;
  LoadSettings;
  // VST Cell Height
  if not Options.ShowErrors then
    VST.DefaultNodeHeight := (TextSize.cy*3)
  else
    VST.DefaultNodeHeight := (TextSize.cy*4);
  // Prepare combos and menus
  FillComboWithFPS(cboInputFPS);
  FillComboWithFPS(cboFPS);
  FillComboWithEncodings(cboEncoding);
  FillComboWithFormats(cboFormat);
  FillWithDictionaries(mnuDictionary, NIL);
  FillMenuWithExtensions('', popExtensions);
  FillMenuWithPlayRate(popPlayRate);
  // Get FPS
  FPS.InputFPS := GetInputFPS;
  FPS.FPS      := GetFPS;
  // Language & Shortcuts
  ReadShortCuts(ShortCutFileName);
  ReadLangForForm(LanguageFileName, Self);
  // MRU
  MRU := TUWMRU.Create(popMRU);
  MRU.OnMRUItemClick := @MRUItemClick;
  MRU.LoadFromJSON(MRUFileName);
  // Styles
  Styles := TSWStyles.Create(StylesFileName);
  FillComboWithStyles(cboStyle);
  // Hunspell
  {$IFDEF MSWINDOWS}
    {$IFDEF WIN32}
      s := 'libhunspellx86.dll';
    {$ELSE}
      s := 'libhunspellx64.dll';
    {$ENDIF}
  {$ELSE}
    {$IFDEF UNIX}
      {$IFDEF DARWIN} // mac
        {$IFDEF CPU32}
          s := 'libhunspellx86.dylib';
        {$ELSE}
          s := 'libhunspellx64.dylib';
        {$ENDIF}
      {$ELSE} // linux
        {$IFDEF CPU32}
          s := 'libhunspellx86.so';
        {$ELSE}
          s := 'libhunspellx64.so';
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF DARWIN}
  Hunspell := TUWHunspell.Create(ConcatPaths([GetCustomFolderPath('Resources'), s]));
  {$ELSE}
  Hunspell := TUWHunspell.Create(GetCustomFilePath(s));
  {$ENDIF}
  Hunspell.LoadDictionary(DictionariesFolder+Options.HunspellLang+'.aff', DictionariesFolder+Options.HunspellLang+'.dic');
  //
  Caption := ProgramName;

  EnableWorkArea(False);
  // Link subtitles to WaveformDisplay
  WAVE.Subtitles := Subtitles;

  // TM
  actGlossary.Enabled := False;
  actTM.Enabled := False;
  //TMX := TUWTMX.Create('c:\subtitles\tm.tmx');
  TMX := TUWTMX.Create('');

  // Initialize libMPV
  if MPV.Engine.ErrorCode = 0 then
  begin
    MPV.Engine.Initialize;
    MPV.Engine.SetTextColor(Options.Marquee.Color);
    MPV.Engine.SetTextPosition(Options.Marquee.Position);
    MPV.Engine.SetTextSize(Options.Marquee.Size);

    stbStatus.Panels[0].Text := MPV.Engine.GetMediaEngineName;
  end
  else
    Showmessage(Strings.libMPVError);

  //
  {$IFDEF DARWIN}
  VST.PopupMenu := popVST;
  VST.OnMouseUp := NIL;

  ForceDirectories(WaveformsFolder);
  ForceDirectories(TMFolder);
  ForceDirectories(TerminologyFolder);
  {$ENDIF}
  ForceDirectories(ProjectsFolder);

  // Prepare GUI
  {$IFDEF DARWIN}
  mnuExit.Visible := False;
  {$ENDIF}
  //tlbMain.Height := cboFind.Height;
  imgFind.Top := 0;
  cboFind.Top := 0;

  lyoEditorTopPanel.Height := (cboStyle.Height*2)+(mTed*3);
  {$IFDEF DARWIN}
  lyoEditor.Height := lyoEditorTopPanel.Height + (cboStyle.Height*4)+(mTed*6);
  {$ELSE}
  lyoEditor.Height := lyoEditorTopPanel.Height + (cboStyle.Height*4)+(mTed*2);
  {$ENDIF}

  cboStyle.Top := 0;
  cboStyle.Width := TextSize.cx*9;
  spdStyle.Top := cboStyle.Top;
  cboActor.Top := (cboStyle.Top + cboStyle.Height) + mTed;
  cboActor.Width := cboStyle.Width;
  spdActor.Top := cboActor.Top;
  tlbEditor.Top := cboStyle.Top;
  tlbEditor.Height := cboStyle.Height;
  tlbEditor.Left := cboStyle.Left+cboStyle.Width + mTed;
  shpColor1.Top := cboStyle.Top;
  shpColor1.Height := cboStyle.Height;
  shpColor1.Left := tlbEditor.Left+tlbEditor.Width + mTed;
  shpColor2.Top := cboStyle.Top;
  shpColor2.Height := cboStyle.Height;
  shpColor2.Left := shpColor1.Left+shpColor1.Width + mTed;
  shpColor3.Top := cboStyle.Top;
  shpColor3.Height := cboStyle.Height;
  shpColor3.Left := shpColor2.Left+shpColor2.Width + mTed;
  shpColor4.Top := cboStyle.Top;
  shpColor4.Height := cboStyle.Height;
  shpColor4.Left := shpColor3.Left+shpColor3.Width + mTed;
  shpColor5.Top := cboStyle.Top;
  shpColor5.Height := cboStyle.Height;
  shpColor5.Left := shpColor4.Left+shpColor4.Width + mTed;
  shpColor6.Top := cboStyle.Top;
  shpColor6.Height := cboStyle.Height;
  shpColor6.Left := shpColor5.Left+shpColor5.Width + mTed;
  shpColor7.Top := cboStyle.Top;
  shpColor7.Height := cboStyle.Height;
  shpColor7.Left := shpColor6.Left+shpColor6.Width + mTed;
  shpColor8.Top := cboStyle.Top;
  shpColor8.Height := cboStyle.Height;
  shpColor8.Left := shpColor7.Left+shpColor7.Width + mTed;
  tlbEditorAlign.Top := cboActor.Top;
  tlbEditorAlign.Left := tlbEditor.Left;
  cboActor.Height := cboStyle.Height;
  numLeft.Top  := cboActor.Top;
  numLeft.Left := tlbEditorAlign.Left+tlbEditorAlign.Width + mTed;
  numTop.Top := cboActor.Top;
  numTop.Left := numLeft.Left+numLeft.Width + mTed;
  numRight.Top := cboActor.Top;
  numRight.Left := numTop.Left+numTop.Width + mTed;
  numBottom.Top := cboActor.Top;
  numBottom.Left := numRight.Left+numRight.Width + mTed;

  lyoEditorLeftPanel.Width := cboStyle.Width+spdInitial.Width+(mTed*3);
  tedInitial.Left  := 25;
  tedInitial.Width := cboStyle.Width;
  tedInitial.Top   := 0;
  spdInitial.Top   := tedInitial.Top;
  tedFinal.Left  := tedInitial.Left;
  tedFinal.Width := cboStyle.Width;
  tedFinal.Top   := tedInitial.Top + tedInitial.Height + mTed;
  spdFinal.Top   := tedFinal.Top;
  tedDuration.Left  := tedInitial.Left;
  tedDuration.Width := cboStyle.Width;
  tedDuration.Top   := tedFinal.Top + tedInitial.Height + mTed;
  spdDuration.Top   := tedDuration.Top;
  tedPause.Left  := tedInitial.Left;
  tedPause.Width := cboStyle.Width;
  tedPause.Top   := tedDuration.Top + tedInitial.Height + mTed;
  spdPause.Top   := tedPause.Top;

  cpsText.Height := 6;
  cpsText.Top := mmoText.Top+mmoText.Height;
  mmoText.Height := (tedInitial.Height*3)-(cpsText.Height-mTed);
  mmoText.Top := (lyoEditorClient.Height-mmoText.Height)-(cpsText.Height+(mTed*2));
  lblText.Top := (mmoText.Top - lblText.Height)-(mTed*2)+1;
  cpsTranslation.Top := cpsText.Top;
  cpsTranslation.Height := cpsText.Height;
  mmoTranslation.Top := mmoText.Top;
  mmoTranslation.Height := mmoText.Height;
  lblTranslation.Top := lblText.Top;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CloseSubtitle;
  if CanClose then
  begin
    // Save settings
    SaveSettings;
    // TMX
    TMX.Free;
    // Hunspell
    if Assigned(Hunspell) then Hunspell.Free;
    // Styles
    //Styles.SaveToFile;
    Styles.Free;
    // MRU
    MRU.SaveToJSON(MRUFileName);
    MRU.Free;
    // VST
    VST.RootNodeCount := 0;
    // Free Undo
    Undo.Free;
    // Unlink Subtitles
    WAVE.Subtitles := NIL;
    // Free SubtitleAPI
    Subtitles.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FormShow(Sender: TObject);
begin
  CommandLineProcess;

  if Options.ShowWelcomeAtStartup and not VST.Enabled then
  begin
    if frmWelcome = NIL then
    begin
      frmWelcome := TfrmWelcome.Create(Application);
      frmWelcome.ShowModal;
    end;
  end;

  if actDockVideoControls.Tag = -1 then
  begin
    actDockVideoControlsExecute(NIL);
    actDockVideoControls.Tag := 0;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FormResize(Sender: TObject);
var
  Border, w: Integer;
begin
  Border := VST.Left;

  cboFind.Left := (tlbMain.Width - cboFind.Width) - 2;
  imgFind.Left := cboFind.Left - imgFind.Width;

  if lyoEditorLeftPanel.Visible then
    w  := (ClientWidth-lyoEditorLeftPanel.Width) - (Border*4)
  else
    w  := (ClientWidth)-(Border*4);

  tlbTM.Left := lyoEditorTopPanel.Width-tlbTM.Width;
  if TranslatorMode then
  begin
    mmoText.Width := (w div 2);
    cpsText.Width := mmoText.Width;
    mmoTranslation.Width := mmoText.Width;
    mmoTranslation.Left := mmoText.Width+(Border*3)-1;
    lblTranslation.Left := mmoTranslation.Left;
    cpsTranslation.Left := mmoTranslation.Left;
    cpsTranslation.Width := mmoTranslation.Width;
  end
  else
  begin
    mmoText.Width := w + (Border*2);
    cpsText.Width := mmoText.Width;
  end;

  stbStatus.Panels[1].Width := ClientWidth - stbStatus.Panels[0].Width - stbStatus.Panels[2].Width;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTResize(Sender: TObject);
var
  wCols: Integer;
begin
  wCols := 0;
  if (coVisible in VST.Header.Columns[0].Options) then wCols := wCols + VST.Header.Columns[0].Width;
  if (coVisible in VST.Header.Columns[1].Options) then wCols := wCols + VST.Header.Columns[1].Width;
  if (coVisible in VST.Header.Columns[2].Options) then wCols := wCols + VST.Header.Columns[2].Width;
  if (coVisible in VST.Header.Columns[3].Options) then wCols := wCols + VST.Header.Columns[3].Width;
  if (coVisible in VST.Header.Columns[6].Options) then wCols := wCols + VST.Header.Columns[6].Width;
  if (coVisible in VST.Header.Columns[7].Options) then wCols := wCols + VST.Header.Columns[7].Width;
  if (coVisible in VST.Header.Columns[8].Options) then wCols := wCols + VST.Header.Columns[8].Width;
  wCols := (VST.Width-wCols) - (GetSystemMetrics(SM_CXVSCROLL)+5);
  if TranslatorMode then wCols := wCols div 2;
  VST.Header.Columns[4].Width := wCols;
  VST.Header.Columns[5].Width := wCols;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVEPeakCreation(Sender: TObject;
  const EventType: TUWPeakCreationEventType; const Param: Integer);
begin
  if frmAudioExtraction <> NIL then
  begin
    case EventType of
      pcetStart    : begin
                       frmAudioExtraction.lblTimeElapsed.Visible := False;
                       frmAudioExtraction.prbProgress.Max := 100;
                       frmAudioExtraction.lblStatus.Caption := strGenerating;
                     end;
      pcetProgress : begin
                       frmAudioExtraction.prbProgress.Position := Param;
                       Application.ProcessMessages;
                     end;
      pcetStop     : begin
                       frmAudioExtraction.prbProgress.Position := 100;
                       frmAudioExtraction.lblStatus.Caption := '';
                     end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; const AText: String;
  const CellRect: TRect; var DefaultDraw: Boolean);

var
  TS : TTextStyle;
  R  : TRect;
  c  : Cardinal;

  procedure DrawDiffDuration;
  var
    dur,
    doptimal : Cardinal;
    s        : String;
  begin
    //TS.Alignment := taCenter;
    doptimal := CalculateOptimalDisplayMS(Subtitles[Node^.Index].Text);
    dur := Subtitles.Duration[Node^.Index];
    s   := '';
    if (doptimal > dur) then
    begin
      dur := doptimal - dur;
      if dur > 1000 then s := '-';
    end
    else if (doptimal < dur) then
    begin
      dur := dur - doptimal;
      if dur > 1000 then s := '+';
    end;

    if s <> '' then
    begin
      if not (vsSelected in Node^.States) then
      begin
        c := TargetCanvas.Font.Color;
        TargetCanvas.Font.Color := clRed;
      end;

      if WorkMode = wmTime then
        TargetCanvas.TextRect(R, R.Left, TextSize.cy, s+TrimTimeString(TimeToString(dur, 'ss.z')), TS)
      else
        TargetCanvas.TextRect(R, R.Left, TextSize.cy, s+IntToStr(TimeToFrames(dur, GetInputFPS)), TS);
      if not (vsSelected in Node^.States) then TargetCanvas.Font.Color := c;
    end;
  end;

  procedure DrawCPS(const Original: Boolean);
  var
    cps: Double;
  begin
    if Original then
      cps := Subtitles.TextCPS[Node^.Index]
    else
      cps := Subtitles.TranslationCPS[Node^.Index];

    {if not (vsSelected in Node^.States) then
    begin
      c := TargetCanvas.Font.Color;
      TargetCanvas.Font.Color := GetColorCPS(cps, c);
    end;}
    //if c <> TargetCanvas.Font.Color then
    //  TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold]
    //else
    //  TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
    //TargetCanvas.TextRect(R, R.Left, R.Top, Format(Strings.CPS, [cps], FormatSettings), TS);
    TargetCanvas.TextRect(R, R.Left, R.Top, Format('%.2f', [cps], FormatSettings), TS);
    //if not (vsSelected in Node^.States) then TargetCanvas.Font.Color := c;
  end;

  procedure DrawWPM(const Original: Boolean);
  var
    wpm: Double;
  begin
    if Original then
      wpm := Subtitles.TextWPM[Node^.Index]
    else
      wpm := Subtitles.TranslationWPM[Node^.Index];

    TargetCanvas.TextRect(R, R.Left, R.Top, Format('%.2f', [wpm], FormatSettings), TS);
  end;

begin
  DefaultDraw := False;
  TargetCanvas.Brush.Style := bsClear;
  if vsSelected in Node^.States then TargetCanvas.Font.Color := clWhite;
  FillByte(TS, SizeOf(TS), 0);
  TS.RightToLeft := Application.IsRightToLeft;
  R := CellRect;

  case Column of
    0: begin
         TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
         TS.Layout := tlTop;
         // !
         if Subtitles[Node^.Index].ErrorType <> [] then
         begin
           c := TargetCanvas.Font.Color;
           TargetCanvas.Font.Color := clRed;
           TS.Alignment := taLeftJustify;
           TargetCanvas.TextRect(R, R.Left, R.Top, '!', TS);
           TargetCanvas.Font.Color := c;
         end;
         // #
         TS.Alignment := taRightJustify;
         TargetCanvas.TextRect(R, R.Left, R.Top, '#' + IntToStr(Node^.Index+1), TS);
         // Pause
         R.Top := TextSize.cy;
         TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
         TargetCanvas.TextRect(R, R.Left, R.Top, GetPauseTimeStr(Node^.Index, True), TS);
       end;
    1: begin
         // Times
         TS.Alignment := taCenter;
         TS.Layout := tlTop;
         TargetCanvas.TextRect(R, R.Left, R.Top, GetInitialTimeStr(Node^.Index) + sLineBreak +
           GetFinalTimeStr(Node^.Index), TS);
       end;
    2: begin
         // Duration
         TS.Alignment := taCenter;
         TS.Layout := tlTop; // TS.Layout := tlBottom;
         TargetCanvas.TextRect(R, R.Left, R.Top, GetDurationTimeStr(Node^.Index, True), TS);
         DrawDiffDuration; // diff time duration
       end;
    3: begin // Style/Actor
         TS.Alignment := taCenter;
         TS.Layout := tlTop;
         TargetCanvas.TextRect(R, R.Left, R.Top, Subtitles[Node^.Index].Style, TS);
         R.Top := TextSize.cy;
         TargetCanvas.Font.Style := TargetCanvas.Font.Style + [TFontStyle.fsBold];
         TargetCanvas.TextRect(R, R.Left, R.Top, Subtitles[Node^.Index].Actor, TS);
         TargetCanvas.Font.Style := TargetCanvas.Font.Style - [TFontStyle.fsBold];
       end;
    4: begin
         // Text
         if Options.DrawTags then
           DrawASSText(TargetCanvas, R, Subtitles[Node^.Index].Text,
           Application.BidiMode <> bdLeftToRight)
//           DrawHTMLTextEx(TargetCanvas, R, Subtitles[Node^.Index].Text,
//           Application.BidiMode <> bdLeftToRight, swt_StartTag, swt_EndTag)
         else
         begin
           TS.Alignment := taLeftJustify;
           TS.Layout := tlTop;
           //TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
           TargetCanvas.TextRect(R, R.Left, R.Top, Subtitles[Node^.Index].Text, TS);
         end;
       end;
    5: begin
         // Translation
         R.Right := (R.Right - 16);
         if Options.DrawTags then
           DrawASSText(TargetCanvas, R, Subtitles[Node^.Index].Translation,
           Application.BidiMode <> bdLeftToRight)
//           DrawHTMLTextEx(TargetCanvas, R, Subtitles[Node^.Index].Translation,
//           Application.BidiMode <> bdLeftToRight, swt_StartTag, swt_EndTag)
         else
         begin
           TS.Alignment := taLeftJustify;
           TS.Layout := tlTop;
           //TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
           TargetCanvas.TextRect(R, R.Left, R.Top, Subtitles[Node^.Index].Translation, TS);
         end;
       end;
    6: begin
         // CPS
         TS.Alignment := taCenter;
         TS.Layout := tlTop;
         //R.Top := TextSize.cy*2; // text cps
         DrawCPS(True);
         if TranslatorMode then
         begin
           R.Top := TextSize.cy; //*3 translation cps
           DrawCPS(False);
         end;
       end;
    7: begin
         // WPM
         TS.Alignment := taCenter;
         TS.Layout := tlTop;
         DrawWPM(True);
         if TranslatorMode then
         begin
           R.Top := TextSize.cy; //*3 translation cps
           DrawWPM(False);
         end;
       end;
    8: begin
         // CPL
         TS.Alignment := taRightJustify;
         TS.Layout := tlTop;
         c := TargetCanvas.Font.Color;
         if (etBreakLongLines in Subtitles[Node^.Index].ErrorType) and not (vsSelected in Node^.States) then TargetCanvas.Font.Color := clRed;
         if TranslatorMode then
         begin
           TargetCanvas.TextRect(R, R.Left, R.Top, GetLengthForEachLine(Subtitles[Node^.Index].Translation), TS);
           TS.Alignment := taLeftJustify;
         end;
         TargetCanvas.TextRect(R, R.Left, R.Top, GetLengthForEachLine(Subtitles[Node^.Index].Text), TS);
         TargetCanvas.Font.Color := c;
       end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  SelColor: TColor;

  procedure DrawErrors;
  const
    AW: Integer = 18;
  var
    AX: Integer;
    i: TSubtitleErrorType;
  begin
    if Options.ShowErrors then
    begin
      AX := 4;
      for i in Subtitles[Node^.Index].ErrorType do
      begin
        imlET.Draw(TargetCanvas, CellRect.Left+AX, CellRect.Bottom-AW, Integer(i));
        Inc(AX, AW);
      end;
    end;
  end;

begin
  SelColor := TargetCanvas.Brush.Color;

  // row background
  if (Column = 5) or (Node^.Index mod 2 = 0) then
    VSTPaintCell(TargetCanvas, CellRect, $00E5E5E5);

  // selection
  if vsSelected in Node^.States then
  begin
    SelColor := VST.Colors.FocusedSelectionColor; //MixColors(VST.Colors.FocusedSelectionColor, SelColor, iff(VST.Focused, 70, 40));
    VSTPaintCell(TargetCanvas, CellRect, SelColor);
  end
  else
  begin
    // errors
    if (etOverlapping in Subtitles[Node^.Index].ErrorType) and (Column in [0..1]) then
      VSTPaintCell(TargetCanvas, CellRect, MixColors(SelColor, Options.Colors.Overlapping));
    if (etBadValues in Subtitles[Node^.Index].ErrorType) and (Column = 1) then
      VSTPaintCell(TargetCanvas, CellRect, MixColors(SelColor, Options.Colors.BadValues));
    if (etTimeTooShort in Subtitles[Node^.Index].ErrorType) and (Column = 2) then
      VSTPaintCell(TargetCanvas, CellRect, MixColors(SelColor, Options.Colors.TimeTooShort))
    else if (etTimeTooLong in Subtitles[Node^.Index].ErrorType) and (Column = 2) then
      VSTPaintCell(TargetCanvas, CellRect, MixColors(SelColor, Options.Colors.TimeTooLong));
    if (etPauseTooShort in Subtitles[Node^.Index].ErrorType) and (Column = 0) then
      VSTPaintCell(TargetCanvas, CellRect, MixColors(SelColor, Options.Colors.PauseTooShort));
  end;

  // untranslated text
  //if (Column = 5) and TranslatorMode and ((Subtitles[Node^.Index].Text = Subtitles[Node^.Index].Translation) or (Subtitles[Node^.Index].Translation = '')) then
  if (Column = 5) and TranslatorMode then
  begin
    if (Subtitles[Node^.Index].Translation = '') then
    begin
      VSTPaintCell(TargetCanvas, CellRect, MixColors(SelColor, Options.Colors.Untranslated));
      imlMain.Draw(TargetCanvas, CellRect.Right-17, 1, 60);
    end
    else
      imlMain.Draw(TargetCanvas, CellRect.Right-17, 1, 59);
  end;

  // marked
  if (Column = 0) and Subtitles[Node^.Index].Marked then
    VSTPaintCell(TargetCanvas, Rect(CellRect.Left, CellRect.Top, CellRect.Left+2, CellRect.Bottom), clBlue);

  // Icon errors
  if (Column = 0) then DrawErrors;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: String);

  procedure SetHintText(const S: String);
  begin
    if HintText = '' then
      HintText := S
    else
      HintText := HintText + sLineBreak + S;
  end;

begin
  LineBreakStyle := hlbForceSingleLine;
  HintText := '';
  with Subtitles[Node^.Index] do
    if ErrorType = [] then
    begin
      if Marked then SetHintText(ErrorStrings.Marked);
    end
    else
    begin
      if etBadValues in ErrorType then SetHintText(ErrorStrings.BadValues);
      if etTimeTooLong in ErrorType then SetHintText(ErrorStrings.TimeTooLong);
      if etTimeTooShort in ErrorType then SetHintText(ErrorStrings.TimeTooShort);
      if etPauseTooShort in ErrorType then SetHintText(ErrorStrings.PauseTooShort);
      if etMaxCPS in ErrorType then SetHintText(ErrorStrings.MaxCPS);
      if etOverlapping in ErrorType then SetHintText(ErrorStrings.Overlapping);
      if etFixTags in ErrorType then SetHintText(ErrorStrings.FixTags);
      if etEmpty in ErrorType then SetHintText(ErrorStrings.Empty);
      if etUnnecessarySpaces in ErrorType then SetHintText(ErrorStrings.UnnecessarySpaces);
      if etUnnecessaryDots in ErrorType then SetHintText(ErrorStrings.UnnecessaryDots);
      if etRepeatedChars in ErrorType then SetHintText(ErrorStrings.RepeatedChars);
      if etProhibitedChars in ErrorType then SetHintText(ErrorStrings.ProhibitedChars);
      if etHearingImpaired in ErrorType then SetHintText(ErrorStrings.HearingImpaired);
      if etBreakLongLines in ErrorType then SetHintText(ErrorStrings.BreakLongLines);
      if etRepeatedSubtitle in ErrorType then SetHintText(ErrorStrings.RepeatedSubtitle);
      if etOCR in ErrorType then SetHintText(ErrorStrings.OCR);
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTHeaderMouseUp(Sender: TVTHeader; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) then popVSTHeader.PopUp;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) and (popVSTHeader.Tag = 0) then
    popVST.PopUp
  else if popVSTHeader.Tag = 1 then
    popVSTHeader.Tag := 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  UpdateValues;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTDblClick(Sender: TObject);
begin
  if WAVE.IsPeakDataLoaded then
    WAVE.SelectSubtitle(VSTFocusedNode, True, True);

  MPV.Engine.Seek(Subtitles.ItemPointer[VSTFocusedNode]^.InitialTime, True);

  FocusMemo;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cboInputFPSKeyPress(Sender: TObject; var Key: char);
var
  CBO: TComboBox;
begin
  CBO := (Sender as TComboBox);
  if Key = ',' then Key := '.';
  if (Key = Chr(VK_RETURN)) and IsFloat(CBO.Text, FormatSettings) then
    AddFPSToCombo(SysUtils.StrToFloat(CBO.Text, FormatSettings), CBO)
  else if not CharInSet(Key, ['0'..'9', '.', Chr(VK_BACK)]) or
    (Key = '.') and (StringCount('.', CBO.Text) = 1) then
    Key := #0;
end;

procedure TfrmMain.cboInputFPSSelect(Sender: TObject);
begin
  VST.Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cboStyleChange(Sender: TObject);
begin
  if cboStyle.Tag = 0 then SetStyle;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cboFPSSelect(Sender: TObject);
begin
  VSTDoLoop(@ApplyChangeFPS, dlAll, False);
  FPS.FPS := GetFPS;
  VST.Invalidate;
  IncrementUndoGroup;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.tedInitialTimeChange(Sender: TObject; const NewTime: Cardinal);
begin
  if not VSTUpdating and (VSTFocusedNode > -1) then
    with (Sender as TUWTimeEdit) do
    begin
      if VST.SelectedCount = 1 then
        SetSubtitleTime(VSTFocusedNode, CorrectTime(NewTime), Tag)
      else
        case Tag of
          0 : VSTDoLoop(@ApplySetTimeInitialFromSpin, dlSelected, True);
          1 : VSTDoLoop(@ApplySetTimeFinalFromSpin, dlSelected, True);
          2 : VSTDoLoop(@ApplySetTimeDurationFromSpin, dlSelected, True);
          3 : VSTDoLoop(@ApplySetTimePauseFromSpin, dlSelected, True);
        end;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mmoTextChange(Sender: TObject);
begin
  if not VSTUpdating and (VSTFocusedNode > -1) then
    with (Sender as TMemo) do
      if VST.SelectedCount = 1 then
        SetSubtitleText(VSTFocusedNode, Text, Tag = 0)
      else
        case Tag of
          0 : VSTDoLoop(@ApplySetTextFromEdit, dlSelected, True);
          1 : VSTDoLoop(@ApplySetTranslationFromEdit, dlSelected, True);
        end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVAudioReconfig(Sender: TObject);
begin
  FillMenuWithAudioStreams(mnuVideoAudio);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVClick(Sender: TObject);
begin
  actMediaPlay.Execute;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVFileLoaded(Sender: TObject);
begin
  //stbStatus.Panels[1].Text := '';
  LastSubtitle.ShowIndex := 0;
  sbrSeek.Max := MPV.Engine.Duration;
  ttTimes.Duration := MSecsToRefTime(sbrSeek.Max);
  lblMediaTime.Caption := TimeToString(sbrSeek.Max, DefTimeFormat);
  actMediaPlay.ImageIndex := 29;
  actCloseVideo.Enabled := True;

  if not actVideoPreview.Checked then actVideoPreview.Execute;
  if Options.AutoStartPlaying and (MPV.Tag = 0) then
    actMediaPlay.Execute
  else if (MPV.Tag = 1) then
    MPV.Tag := 0;

  if actDockVideoControls.Tag <> -2 then
    OpenAudio(MediaFileExists(MPV.Engine.FileName, TAudioExts))
  else
    actDockVideoControls.Tag := 0;

  actExtractWaveform.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVPlay(Sender: TObject);
begin
  actMediaPlay.ImageIndex := 55;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVStop(Sender: TObject);
begin
  actMediaPlay.ImageIndex := 29;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MPVTimeChanged(Sender: TObject; AParam: Integer=0);
begin
  MediaUpdateProgress(AParam);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cboFindChange(Sender: TObject);
begin
  if MPV.Engine.IsPlaying and actMediaAutoScroll.Checked then actMediaAutoScroll.Checked := False;
  if not VSTFind(cboFind.Text, False, True) then VST.ClearSelection;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.numLeftValueChange(Sender: TObject);
begin
  if (VST.SelectedCount > 1) then
    VSTDoLoop(@ApplyXY)
  else
    ApplyXY(Subtitles.ItemPointer[VSTFocusedNode], VSTFocusedNode);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.sbrSeekMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  VLCSeekTo(sbrSeek.Position);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.shpColor1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton.mbLeft then
  begin
    LastSubtitle.Color := (Sender as TShape).Brush.Color;
    actFontColor.Execute;
  end
  else if Button = TMouseButton.mbRight then
    with TColorDialog.Create(Self) do
    try
      if Execute then
      begin
        LastSubtitle.Color := Color;
        (Sender as TShape).Brush.Color := Color;
        actFontColor.Execute;
      end;
    finally
      Free;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.UndoChanged(const ChangeType: UUndo.TUndoChangeType);
begin
  actUndo.Enabled := Undo.CanUndo;
  actRedo.Enabled := Undo.CanRedo;

  if ChangeType = uctReIndex then
  begin
    VST.RootNodeCount := Subtitles.Count;
    UpdateValues(True);
  end
  else if ChangeType = uctItems then UpdateValues(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.UpdateStatusBar;
var
  s: String;
begin
  s := '';
  if VST.Enabled then
  begin
    if VST.SelectedCount > 1 then
      s := Format(Strings.LineSelected, [VST.SelectedCount, VST.TotalCount])
    else if (VST.SelectedCount = 1) and (VSTFocusedNode >= 0) then
      s := Format('%d / %d', [VSTFocusedNode+1, VST.TotalCount])
    else
      s := IntToStr(VST.TotalCount);
  end;
  stbStatus.Panels[2].Text := s;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.UpdateCPSAndTexts;
begin
  if (VSTFocusedNode > -1) and (VST.SelectedCount = 1) then
  begin
    lblText.Caption := Format(Strings.TextChars, [GetLengthForEachLine(Subtitles[VSTFocusedNode].Text, '/', '='), Subtitles.TextCPS[VSTFocusedNode]], FormatSettings);
    lblTranslation.Caption := Format(Strings.TranslationChars, [GetLengthForEachLine(Subtitles[VSTFocusedNode].Translation, '/', '='), Subtitles.TranslationCPS[VSTFocusedNode]], FormatSettings);
    cpsText.SetCPS(Subtitles.TextCPS[VSTFocusedNode]);
    cpsTranslation.SetCPS(Subtitles.TranslationCPS[VSTFocusedNode]);
  end
  else
  begin
    lblText.Caption := Strings.Text;
    lblTranslation.Caption := Strings.Translation;
    cpsText.SetCPS(0);
    cpsTranslation.SetCPS(0);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.UpdateColorsInBoxes(const Index: Integer);
begin
  if VST.SelectedCount > 1 then // multiple selected? paint disabled colors
  begin
    tedInitial.Color     := clBtnFace;
    tedFinal.Color       := tedInitial.Color;
    tedDuration.Color    := tedInitial.Color;
    tedPause.Color       := tedInitial.Color;
    mmoText.Color        := tedInitial.Color;
    mmoTranslation.Color := tedInitial.Color;
  end
  else
    // error colors
    with Subtitles[Index] do
    begin
      mmoText.Color        := clDefault;
      mmoTranslation.Color := mmoText.Color;

      if (etOverlapping in ErrorType) then
        tedInitial.Color := MixColors(clWhite, Options.Colors.Overlapping)
      else if (etBadValues in ErrorType) then
      begin
        tedInitial.Color := MixColors(clWhite, Options.Colors.BadValues);
        tedFinal.Color   := tedInitial.Color;
      end
      else
      begin
        tedInitial.Color := clDefault;
        tedFinal.Color   := clDefault;
      end;

      if (etTimeTooShort in ErrorType) then
        tedDuration.Color := MixColors(clWhite, Options.Colors.TimeTooShort)
      else if (etTimeTooLong in ErrorType) then
        tedDuration.Color := MixColors(clWhite, Options.Colors.TimeTooLong)
      else
        tedDuration.Color := clDefault;

      if (etPauseTooShort in ErrorType) then
        tedPause.Color := MixColors(clWhite, Options.Colors.PauseTooShort)
      else
        tedPause.Color := clDefault;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.UpdateValues(const VSTInvalidate: Boolean = False);
var
  NodeIndex: Integer;
  LastIndex: Integer;
begin
  VSTBeginUpdate;
  try
    if VST.SelectedCount = 0 then // zero selected
    begin
      mmoText.Text        := '';
      mmoTranslation.Text := '';
      tedInitial.SetValueOnly(0);
      tedFinal.SetValueOnly(0);
      tedDuration.SetValueOnly(0);
      tedPause.SetValueOnly(0);
      actShiftToPrevious.Enabled   := False;
      actDefaultPause.Enabled      := False;
      actShiftToNext.Enabled       := False;
      actAutomaticDuration.Enabled := False;
      numLeft.SetValueOnly(0);
      numTop.SetValueOnly(0);
      numRight.SetValueOnly(0);
      numBottom.SetValueOnly(0);
      cboStyle.Tag := 1;
      cboStyle.ItemIndex := -1;
      cboStyle.Tag := 0;
      cboActor.Tag := 1;
      cboActor.Text := '';
      cboActor.Tag := 0;
    end
    else if Assigned(VST.GetFirstSelected) then // one or more selected
    begin
      LastIndex := -1;
      NodeIndex := VST.GetFirstSelected^.Index;
      if VST.SelectedCount > 1 then LastIndex := VSTLastSelectedNode;
      if not Subtitles.ValidIndex(LastIndex) then
        LastIndex := NodeIndex;

      tedInitial.SetValueOnly(GetInitialTime(NodeIndex));
      tedFinal.SetValueOnly(GetFinalTime(LastIndex));
      tedPause.SetValueOnly(GetPauseTime(NodeIndex));
      if VST.SelectedCount = 1 then
      begin
        tedDuration.SetValueOnly(GetDurationTime(NodeIndex));
        mmoText.Text        := Subtitles[NodeIndex].Text;
        mmoTranslation.Text := Subtitles[NodeIndex].Translation;
        case Subtitles[NodeIndex].Align of
          0: actAlignToNone.Checked   := True;
          1: actAlignToLeft.Checked   := True;
          2: actAlignToCenter.Checked := True;
          3: actAlignToRight.Checked  := True;
        end;
        actShiftToPrevious.Enabled := (NodeIndex > 0);
        actDefaultPause.Enabled    := actShiftToPrevious.Enabled;
        actShiftToNext.Enabled     := NodeIndex < Subtitles.Count-1;
        numLeft.SetValueOnly(Subtitles[NodeIndex].R.Left);
        numTop.SetValueOnly(Subtitles[NodeIndex].R.Top);
        numRight.SetValueOnly(Subtitles[NodeIndex].R.Right);
        numBottom.SetValueOnly(Subtitles[NodeIndex].R.Bottom);

        cboStyle.Tag := 1;
        cboStyle.ItemIndex := cboStyle.Items.IndexOf(Subtitles[NodeIndex].Style);
        cboStyle.Tag := 0;
        cboActor.Tag := 1;
        cboActor.Text := Subtitles[NodeIndex].Actor;
        cboActor.Tag := 0;
      end
      else
      begin
        tedDuration.SetValueOnly(Range(tedFinal.Value - tedInitial.Value, 0, tedFinal.Value));
        mmoText.Text        := '';
        mmoTranslation.Text := '';
        actAlignToNone.Checked   := False;
        actAlignToLeft.Checked   := False;
        actAlignToCenter.Checked := False;
        actAlignToRight.Checked  := False;

        actShiftToPrevious.Enabled := True;
        actDefaultPause.Enabled    := True;
        actShiftToNext.Enabled     := True;
        numLeft.SetValueOnly(0);
        numTop.SetValueOnly(0);
        numRight.SetValueOnly(0);
        numBottom.SetValueOnly(0);
      end;
      actAutomaticDuration.Enabled := True;

      UpdateCPSAndTexts;
      UpdateColorsInBoxes(NodeIndex);
    end;
  finally
    VSTEndUpdate;
  end;

  if VSTInvalidate then
  begin
    VST.Invalidate;
    WAVE.Invalidate;
  end;
  UpdateStatusBar;
  CheckTM;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.EnableWorkArea(const Value: Boolean = True);
var
  i: Integer;
begin
  // Components
  VST.Enabled := Value;
  cboFormat.Enabled := Value;
  cboEncoding.Enabled := Value;
  cboFPS.Enabled := Value;
  mmoText.Enabled := Value;
  mmoTranslation.Enabled := Value;
  spdInitial.Enabled := Value;
  spdFinal.Enabled := Value;
  spdDuration.Enabled := Value;
  spdPause.Enabled := Value;
  numLeft.Enabled := Value;
  numTop.Enabled := Value;
  numRight.Enabled := Value;
  numBottom.Enabled := Value;
  cboStyle.Enabled := Value;
  cboActor.Enabled := Value;
  tedInitial.Enabled := Value;
  tedFinal.Enabled := Value;
  tedDuration.Enabled := Value;
  tedPause.Enabled := Value;
  sbrSeek.Enabled := Value;
  //
  if VST.Enabled and mmoText.Enabled then
  begin
    VSTSelectNode(0, True);
    mmoText.SetFocus;
  end;
  // Actions
  actUndo.Enabled := False;
  actRedo.Enabled := False;
  actCloseVideo.Enabled := MPV.Engine.Duration > 0;
  actExtractWaveform.Enabled := actCloseVideo.Enabled;

  for i := 0 to aclActions.ActionCount -1 do
     with (aclActions[i] as TAction) do
       if Tag = 1 then Enabled := Value;
end;

// -----------------------------------------------------------------------------

function TfrmMain.CloseSubtitle: Boolean;
var
  r: Integer;
begin
  Result := False;

  if VST.Enabled then // ask to save if needed
  begin
    if SubtitleFile.Text.Changed then
    begin
      r := MsgSaveSubtitle(SubtitleFile.Text.FileName);
      case r of
        mrYes    : actSaveSubtitleAs.Execute;
        mrCancel : Abort;
      end;
    end;

    if actTranslatorMode.Checked then
    begin
      if SubtitleFile.Translation.Changed then
      begin
        r := MsgSaveSubtitle(SubtitleFile.Translation.FileName);
        case r of
          mrYes    : ;//actSubtitleSaveTranslationAs.Execute;
          mrCancel : Abort;
        end;
      end;
    end;
  end;

  actCloseVideo.Execute;
  SubtitleFile.Text.FileName := '';
  SubtitleFile.Translation.FileName := '';
  SubtitleChanged(False, False);
  WAVE.Close;
  VST.RootNodeCount := 0;
  Subtitles.Clear;
  EnableWorkArea(False);
  UpdateStatusBar;
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.LoadSubtitle(const FileName: String; const AEncoding: TEncoding = NIL; const AFPS: Single = -1; const AutoLoadVW: Boolean = True);
var
  _FPS: Single;
begin
  if not CloseSubtitle then Exit;

  _FPS := AFPS;
  if _FPS = -1 then _FPS := FPS.InputFPS;
  Subtitles.LoadFromFile(FileName, AEncoding, _FPS);
  VST.RootNodeCount := Subtitles.Count;
  cboEncoding.ItemIndex := GetEncodingIndex(Subtitles.CodePage);
  cboFormat.ItemIndex := Integer(Subtitles.Format)-1;

  if Options.AutoCheckErrors then
    VSTDoLoop(@ApplyCheckErrors, dlAll, False);

  SubtitleFile.Text.FileName := ExtractFileName(FileName);
  Caption := SubtitleFile.Text.FileName + ' - ' + ProgramName;
  EnableWorkArea;

  if AutoLoadVW then OpenVideo(MediaFileExists(FileName, TVideoExts));
  //OpenAudio(MediaFileExists(FileName, TAudioExts));

  MRU.Add(FileName);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.SaveSubtitle(const FileName: String; const Format: TUWSubtitleFormats; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
var
  _FPS      : Single;
  _Encoding : TEncoding;
begin
  _FPS := AFPS;
  if _FPS = -1 then _FPS := FPS.InputFPS;
  _Encoding := AEncoding;
  if _Encoding = NIL then  _Encoding := TEncoding.GetEncoding(Encodings[cboEncoding.ItemIndex].CPID);

  Subtitles.SaveToFile(FileName, _FPS, _Encoding, Format);
  MRU.Add(FileName);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.OpenVideo(const FileName: String; const Pos: Int64 = 0);
begin
  MPV.Engine.Play(FileName); //VLC.Play(WideString(FileName));
  actMediaChangePlayRateExecute(NIL);
  //actCloseVideo.Enabled := MPV.Engine.Duration > 0; //VLC.CanPlay();
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.OpenAudio(const FileName: String);
begin
  if FileName = '' then
    Exit
  else if FileExists(FileName) then
    WAVE.LoadWaveFromFile(FileName)
  else
  begin // no wave/peak file found
    WAVE.LoadWaveFromFile(MediaFileExists(WaveformsFolder + ExtractFileName(FileName), TAudioExts));
    //actExtractWaveform.Enabled := True;
    //if actCloseVideo.Enabled then
//    if MPV.Engine.Duration > 0 then
//      DoExtractAudioTrack;
  end;

  if (WAVE.IsPeakDataLoaded and not actAudioPreview.Checked) or
     (not WAVE.IsPeakDataLoaded and actAudioPreview.Checked) then
    actAudioPreview.Execute;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MRUItemClick(Sender: TObject);
begin
  LoadSubtitle((Sender as TMenuItem).Caption);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.popMemoPopup(Sender: TObject);
var
  Memo     : TMemo;
  Suggests : TStrings;
  i        : Integer;
  mnu      : TMenuItem;
  s        : String;
begin
  // check word under caret
  Memo     := GetMemoFocused;
  Suggests := NIL;

  if Memo = NIL then Exit;
  mnuMemoX.Visible := False;

  for i := popMemo.Items.Count-1 downto 0 do
    if Copy(popMemo.Items[i].Name, 1, 3) = 'hs_' then
      popMemo.Items[i].Free;

  s := Memo_GetWordUnderCaret(Memo);

  if Hunspell.Suggest(s, Suggests) then
  begin
    for i := 0 to Suggests.Count-1 do
    begin
      mnu         := TMenuItem.Create(popMemo);
      mnu.Name    := 'hs_' + IntToStr(i);
      mnu.Caption := Suggests[i];
      mnu.OnClick := @HunspellItemClick;
      popMemo.Items.Insert(i, mnu);
    end;
    mnuMemoX.Visible := True;
  end;

  if Suggests <> NIL then Suggests.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.popPlayRatePopup(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    Options.DefChangePlayRate := StrToInt(Copy(Caption, 1, Length(Caption)-1));
    actMediaChangePlayRate.Checked := True;
    tbnPlayRate.Down := actMediaChangePlayRate.Checked;
    actMediaChangePlayRateExecute(NIL);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.popAudioTrackSet(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    MPV.Engine.SetTrack(trkAudio, (Sender as TMenuItem).Tag);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.popVSTHeaderPopup(Sender: TObject);
begin
  popVSTHeader.Tag := 1;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.popVSTPopup(Sender: TObject);
begin
  mnuVST_SepValidate.Visible := TranslatorMode;
  mnuVST_Validate.Visible    := TranslatorMode;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.HunspellItemClick(Sender: TObject);
var
  Memo : TMemo;
begin
  // word suggest clicked
  Memo := GetMemoFocused;
  if Memo = NIL then Exit;

  if Memo.SelText = '' then Memo_GetWordUnderCaret(Memo, True);

  Memo.SelText   := (Sender as TMenuItem).Caption;
  Memo.SelLength := Length(Memo.SelText);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DictionaryItemClick(Sender: TObject);
var
  i: Integer;
begin
  // select dictionary
  if Options.HunspellLang <> (Sender as TMenuItem).Caption then
  begin
    for i := 0 to mnuDictionary.Count-1 do mnuDictionary.Items[i].Checked := False;

    (Sender as TMenuItem).Checked := True;
    Options.HunspellLang := GetDictNameFromCaption((Sender as TMenuItem).Caption);

    Hunspell.LoadDictionary(DictionariesFolder+Options.HunspellLang+'.aff', DictionariesFolder+Options.HunspellLang+'.dic');
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.ExtensionItemClick(Sender: TObject);

  procedure OutputMsg(s: String);
  var
    l          : LongInt;
    ErrorsText : String;
  begin
    ErrorsText := '';
    for l := 0 to psExtensions.CompilerMessageCount - 1 do
    begin
      ErrorsText := ErrorsText + sLineBreak + psExtensions.CompilerErrorToStr(l);
    end;
    ShowMessage(s + sLineBreak + sLineBreak + ErrorsText);
  end;

var
  Item: TMenuItem;
  s: String;
begin
  with (Sender as TMenuItem) do
  begin
    s    := Caption;
    Item := Parent;
  end;
  while Item <> NIL do
  begin
    s    := ConcatPaths([Item.Caption, s]);
    Item := Item.Parent;
  end;
  s := ConcatPaths([ExtensionsFolder, s]) + '.pas';

  with TUWStringList.Create(s) do
  try
    psExtensions.MainFileName := s;
    psExtensions.Script.Text  := Text;
  finally
    Free;
  end;

  if psExtensions.Compile then
  begin
    if not psExtensions.Execute then
    begin
      ShowMessage(ErrorStrings.ExecuteScript + sLineBreak + sLineBreak +
        IntToStr(psExtensions.Exec.ExceptionPos) + '. ' +
        psExtensions.ExecErrorToString);
    end
    else
    begin
      if Options.AutoCheckErrors then
        VSTDoLoop(@ApplyCheckErrors, dlAll, False);
    end;
  end
  else
    OutputMsg(ErrorStrings.CompileScript);

  UpdateValues(True);
  IncrementUndoGroup;
end;

// -----------------------------------------------------------------------------

{ EXTENSIONS }

// -----------------------------------------------------------------------------

procedure TfrmMain.psExtensionsCompile(Sender: TPSScript);
begin
  psCompile(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.psExtensionsCompImport(Sender: TObject; x: TPSPascalCompiler
  );
begin
  psCompImport(Sender, x);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.psExtensionsExecImport(Sender: TObject; se: TPSExec;
  x: TPSRuntimeClassImporter);
begin
  psExecImport(Sender, se, x);
end;

// -----------------------------------------------------------------------------

function TfrmMain.psExtensionsNeedFile(Sender: TObject;
  const OrginFileName: tbtstring; var FileName, Output: tbtstring): Boolean;
var
  FS: TFileStream;
begin
  Result := False;

  try
    FS := TFileStream.Create(ExtractFilePath(OrginFileName) + FileName,
      fmOpenRead or fmShareDenyWrite);
  except
    Exit;
  end;
  try
    SetLength(Output, FS.Size);
    FS.Read(Output[1], Length(Output));
  finally
    FS.Free;
  end;

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.ShowFindAndReplace(const TabIndex: Integer = 0);
begin
  if frmFindAndReplace = NIL then
  begin
    frmFindAndReplace := TfrmFindAndReplace.Create(Application);
    frmFindAndReplace.pagFindAndReplace.TabIndex := TabIndex;
    frmFindAndReplace.Show;
  end
  else
  begin
    frmFindAndReplace.BringToFront;
    frmFindAndReplace.pagFindAndReplace.TabIndex := TabIndex;
  end;
  frmFindAndReplace.pagFindAndReplaceChange(frmFindAndReplace);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.ShowTexts(const TabIndex: Integer = 0);
begin
  if frmTexts = NIL then
  begin
    frmTexts := TfrmTexts.Create(Application);
    frmTexts.pagTexts.TabIndex := TabIndex;
    frmTexts.Show;
  end
  else
  begin
    frmTexts.BringToFront;
    frmTexts.pagTexts.TabIndex := TabIndex;
  end;
  frmTexts.pagTextsChange(frmTexts);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.ShowTimings(const TabIndex: Integer = 0);
begin
  if frmTimings = NIL then
  begin
    frmTimings := TfrmTimings.Create(Application);
    frmTimings.pagTimings.TabIndex := TabIndex;
    frmTimings.Show;
  end
  else
  begin
    frmTimings.BringToFront;
    frmTimings.pagTimings.TabIndex := TabIndex;
  end;
  frmTimings.pagTimingsChange(frmTimings);
end;

// -----------------------------------------------------------------------------

{ WAVE }

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESelectedSubtitleItem(Sender: TObject;
  const Index: Integer; const SubtitleItem: TUWSubtitleItem;
  const IsDynamic: Boolean);
begin
  VSTSelectNode(Index, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESelectedSubtitleItemChange(Sender: TObject);
begin
  UpdateValues(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESelectedSubtitleItemChanged(Sender: TObject;
  const Index: Integer; const OldInitialTime, OldFinalTime: Integer;
  const NeedSort: Boolean);
var
  Item : TUWSubtitleItem;
begin
  Item := Subtitles[Index];
  Item.InitialTime := OldInitialTime;
  Item.FinalTime   := OldFinalTime;

  Undo.AddUndo(utSubtitleChange, Index, Item, LastUndoGroup);
  IncrementUndoGroup;
  UpdateValues(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESelectionChange(Sender: TObject);
begin
  if WAVE.SelectionIsEmpty then
    stbStatus.Panels[1].Text := ''
  else
    stbStatus.Panels[1].Text := Format(Strings.Selection,
      [TimeToString(WAVE.Selection.InitialTime, DefTimeFormat, FPS.FPS, True),
      TimeToString(WAVE.Selection.FinalTime, DefTimeFormat, FPS.FPS, True)]);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MediaUpdateProgress(const Time: Integer);
begin
  if not sbrSeek.MouseIsDown then sbrSeek.Position := Time;

  MPV.Engine.ShowText(GetSubtitleTextAtTime(Time));

  lblMediaTime.Caption := TimeToString(time, DefTimeFormat) + ' / ' + TimeToString(MPV.Engine.Duration, DefTimeFormat);

  // -- WaveDisplay
  if WAVE.IsPeakDataLoaded then
  begin
    WAVE.SetPlayCursorMS(time);
    if (MediaPlayMode = TMediaPlayMode.mpmSelection) and (not WAVE.SelectionIsEmpty)
      and (time >= WAVE.Selection.FinalTime) then
      begin
        MPV.Engine.Pause; // VLC.Pause();
        WAVE.SetPlayCursorMS(WAVE.Selection.FinalTime);
        //VLC.SetVideoPosInMs(Int64(WAVE.Selection.FinalTime));
      end;
  end;
end;

// -----------------------------------------------------------------------------

{ Actions }

// -----------------------------------------------------------------------------

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actExtendLengthExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyExtendLength, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actExtractWaveformExecute(Sender: TObject);
begin
  DoExtractAudioTrack;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFastDivideSubtitleExecute(Sender: TObject);
begin
  DoFastDivideSubtitles;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actModeTimeExecute(Sender: TObject);
begin
  if actModeTime.Checked then
    actModeFrames.Execute
  else
  begin
    actModeTime.Checked := True;
    WorkMode := wmTime;
    SetTimeEditMode(temTime);
    VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actNewProjectExecute(Sender: TObject);
begin
  if Self.Visible then
    if not CloseSubtitle then Exit;

  if frmProject = NIL then
  begin
    frmProject := TfrmProject.Create(Application);
    frmProject.ShowModal;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actModeFramesExecute(Sender: TObject);
begin
  if actModeFrames.Checked then
    actModeTime.Execute
  else
  begin
    actModeFrames.Checked := True;
    WorkMode := wmFrames;
    SetTimeEditMode(temFrames);
    VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTranslatorModeExecute(Sender: TObject);
begin
  SetTranslatorMode(actTranslatorMode.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnbreakSubtitleExecute(Sender: TObject);
var
  Memo: TMemo;
begin
  Memo := GetMemoFocused;
  if Memo = NIL then
    VSTDoLoop(@ApplyUnbreakSubtitles)
  else
  begin
    Memo.Text := UnbreakSubtitles(Memo.Text);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUndoExecute(Sender: TObject);
begin
  Undo.Undo;
  if WAVE.IsPeakDataLoaded then WAVE.ClearSelection;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actRedoExecute(Sender: TObject);
begin
  Undo.Redo;
  if WAVE.IsPeakDataLoaded then WAVE.ClearSelection;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actInsertSubtitleExecute(Sender: TObject);
begin
  VSTInsertSubtitles;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actInsertSubtitleBeforeExecute(Sender: TObject);
begin
  VSTInsertSubtitles(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDeleteSubtitleExecute(Sender: TObject);
begin
  VSTDeleteSubtitles;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDockVideoControlsExecute(Sender: TObject);
var
  s: String;
  p: Integer;
begin
  if frmVideo = NIL then
  begin
    s := MPV.Engine.FileName;
    if s <> '' then
    begin
      if MPV.Engine.IsPaused then MPV.Tag := 1;
      p := MPV.Engine.Position;
      MPV.Engine.UnInitialize;
      actDockVideoControls.Tag := -2;
    end;

    frmVideo := TfrmVideo.Create(Application);
    sptVideo.Visible := False;
    lyoVideo.Align   := alClient;
    lyoVideo.Parent  := frmVideo;
    frmVideo.Show;

    actDockVideoControls.Checked := False;
    if (s <> '') and MPV.Engine.Initialize then
    begin
      MPV.Engine.SetTextColor(Options.Marquee.Color);
      MPV.Engine.SetTextPosition(Options.Marquee.Position);
      MPV.Engine.SetTextSize(Options.Marquee.Size);
      actMediaChangePlayRateExecute(NIL);

      MPV.Engine.Play(s, p);
      frmVideo.Caption := ExtractFileName(s);
    end;
  end
  else
  begin
    frmVideo.Close;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDurationLimitsExecute(Sender: TObject);
begin
  ShowTimings;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMarkSubtitleExecute(Sender: TObject);
begin
  VSTMarkSubtitles(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnMarkSubtitleExecute(Sender: TObject);
begin
  VSTMarkSubtitles(False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSelectAllExecute(Sender: TObject);
var
  Memo: TMemo;
begin
  if VST.Focused then
    VST.SelectAll(False)
  else
  begin
    Memo := GetMemoFocused;
    if Memo <> NIL then Memo.SelectAll;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSetDelayExecute(Sender: TObject);
begin
  ShowTimings(3);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSetMaximumLineLengthExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplySetMaximumLineLength, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSettingsExecute(Sender: TObject);
var
  lng: String;
  i: Integer;
begin
  if frmSettings = NIL then
  begin
    lng := Options.Language;
    frmSettings := TfrmSettings.Create(Application);
    frmSettings.ShowModal;
    // reload lang if needed
    if lng <> Options.Language then
      for i := 0 to Screen.FormCount-1 do
        if Screen.Forms[i] <> NIL then
        begin
          ReadLangForForm(LanguageFileName, Screen.Forms[i]);
          UpdateValues;
        end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShiftTimeLessExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyShiftTimeLess, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShiftTimeMoreExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyShiftTimeMore, dlSelected, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSubtitleDblClickExecute(Sender: TObject);
begin
  VSTDblClick(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSubtitlePosBottomExecute(Sender: TObject);
begin
  Options.Marquee.Position := 'bottom';
  MPV.Engine.SetTextPosition(Options.Marquee.Position);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSubtitlePosTopExecute(Sender: TObject);
begin
  Options.Marquee.Position := 'top';
  MPV.Engine.SetTextPosition(Options.Marquee.Position);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTimeExpanderExecute(Sender: TObject);
begin
  ShowTimings(2);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTM1Execute(Sender: TObject);
begin
  // Copy TM Similarity at #1 to Translation
  GetTMatIndex(0);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTM2Execute(Sender: TObject);
begin
  // Copy TM Similarity at #2 to Translation
  GetTMatIndex(1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTM3Execute(Sender: TObject);
begin
  // Copy TM Similarity at #3 to Translation
  GetTMatIndex(2);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTMExecute(Sender: TObject);
begin
  if frmTM = NIL then
  begin
    frmTM := TfrmTM.Create(Application);
    frmTM.Show;
  end
  else
  begin
    frmTM.BringToFront;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTMValidateExecute(Sender: TObject);
begin
  // validate TM, add to tmx and go to next subtitle

end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTranslateExecute(Sender: TObject);
begin
  ShowTexts(1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVideoPreviewExecute(Sender: TObject);
begin
  VideoPreview := actVideoPreview.Checked;
  if frmVideo = NIL then
  begin
    sptVideo.Visible := VideoPreview;
    lyoVideo.Visible := VideoPreview;
    SetWorkspace(False);
  end
  else
  begin
    frmVideo.Visible := VideoPreview;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAudioPreviewExecute(Sender: TObject);
const
  _tlbSize = 31;
begin
  AudioPreview    := actAudioPreview.Checked;
  WAVE.Visible    := AudioPreview;
  sptWave.Visible := AudioPreview;
  tlbWaveControls.Visible := AudioPreview;
  if AudioPreview then
    lyoVideoControls.Height := (tlbVLCControls.Height + tlbWaveControls.Height) + _tlbSize //lyoVideoControls.Height + tlbWaveControls.Height  // 76
  else
    lyoVideoControls.Height := tlbVLCControls.Height + _tlbSize; //lyoVideoControls.Height - tlbWaveControls.Height; // 54;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAutoBreakSubtitleExecute(Sender: TObject);
var
  Memo: TMemo;
begin
  Memo := GetMemoFocused;
  if Memo = NIL then
    VSTDoLoop(@ApplyAutoBreakSubtitles)
  else
  begin
    Memo.Text := AutoBreakSubtitle(Memo.Text, Options.ErrCfg.MaxLineLength);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSimpleModeExecute(Sender: TObject);
begin
  tlbMain.Visible  := not actSimpleMode.Checked;
  tlbExtra.Visible := actViewToolbarAdditional.Checked and tlbMain.Visible;
  lyoEditorTopPanel.Visible  := tlbMain.Visible;
  lyoEditorLeftPanel.Visible := tlbMain.Visible;
  FormResize(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSingTagExecute(Sender: TObject);
begin
//  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplySingTag, dlSelected, True, True)
//  else
//    SetTextTag(swt_Sing);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFindExecute(Sender: TObject);
begin
  ShowFindAndReplace;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFindNextExecute(Sender: TObject);
begin
  with Options do
    if TextToFind <> '' then VSTFind(TextToFind, False, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFindPreviousExecute(Sender: TObject);
begin
  with Options do
    if TextToFind <> '' then VSTFind(TextToFind, False, False, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFixRTLPunctuationExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyFixRTLPunctuation);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actQuickFindExecute(Sender: TObject);
begin
  if not cboFind.Focused and tlbMain.Visible then cboFind.SetFocus;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actReplaceExecute(Sender: TObject);
begin
  ShowFindAndReplace(1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actReverseTextExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyReverseText);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actGoToExecute(Sender: TObject);
begin
  ShowFindAndReplace(2);
end;

procedure TfrmMain.actInfoAndErrorsExecute(Sender: TObject);
begin
  if frmInfoAndErrors = NIL then
  begin
    frmInfoAndErrors := TfrmInfoAndErrors.Create(Application);
    frmInfoAndErrors.ShowModal;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAlignToNoneExecute(Sender: TObject);
begin
  SetAlignTo(0);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAlignToLeftExecute(Sender: TObject);
begin
  SetAlignTo(1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAlignToCenterExecute(Sender: TObject);
begin
  SetAlignTo(2);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAlignToRightExecute(Sender: TObject);
begin
  SetAlignTo(3);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontBoldExecute(Sender: TObject);
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyFontBold, dlSelected, True, True)
  else
    SetTextTag(swt_Bold);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontItalicExecute(Sender: TObject);
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyFontItalic, dlSelected, True, True)
  else
    SetTextTag(swt_Italic);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontUnderlineExecute(Sender: TObject);
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyFontUnderline, dlSelected, True, True)
  else
    SetTextTag(swt_Underline);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actGlossaryExecute(Sender: TObject);
begin
  if frmGlossary = NIL then
  begin
    frmGlossary := TfrmGlossary.Create(Application);
    frmGlossary.Show;
  end
  else
  begin
    frmGlossary.BringToFront;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontStrikeoutExecute(Sender: TObject);
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyFontStrikeout, dlSelected, True, True)
  else
    SetTextTag(swt_Strikeout);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontClearExecute(Sender: TObject);
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyFontClear, dlSelected, True, True)
  else
    with GetMemoFocused do Text := RemoveSWTags(Text);
end;

procedure TfrmMain.actFontColorDlgExecute(Sender: TObject);
begin
  with TColorDialog.Create(Self) do
  try
    if Execute then
    begin
      LastSubtitle.Color := Color;
      actFontColor.Execute;
    end;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontColorExecute(Sender: TObject);
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyFontColor, dlSelected, True, True)
  else
    SetTextTagColor(Format('%s%s%s', [IntToHex(GetBValue(LastSubtitle.Color), 2),
      IntToHex(GetGValue(LastSubtitle.Color), 2),
      IntToHex(GetRValue(LastSubtitle.Color), 2)]));
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCopyExecute(Sender: TObject);
var
  Memo: TMemo;
begin
  if VST.Focused then
    VSTCopySubtitlesToClipboard
  else
  begin
    Memo := GetMemoFocused;
    if Memo <> NIL then
      Memo.CopyToClipboard
    else
      ComboCopyToClipboard;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCustomSearchExecute(Sender: TObject);
var
  Memo : TMemo;
  s: String;
begin
  Memo := GetMemoFocused;
  if Memo = NIL then Exit;

  s := Memo.SelText;
  if s = '' then s := Memo_GetWordUnderCaret(Memo);
  if s <> '' then OpenURL(Format(Options.CustomSearch, [s]));
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCutExecute(Sender: TObject);
var
  Memo: TMemo;
begin
  if VST.Focused then
    VSTCopySubtitlesToClipboard(True)
  else
  begin
    Memo := GetMemoFocused;
    if Memo <> NIL then
      Memo.CutToClipboard
    else
      ComboCopyToClipboard(True);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actPasteExecute(Sender: TObject);
var
  Memo: TMemo;
begin
  if VST.Focused then
    VSTPasteSubtitlesFromClipboard
  else
  begin
    Memo := GetMemoFocused;
    if Memo <> NIL then
      Memo.PasteFromClipboard;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actPreviousSubtitleExecute(Sender: TObject);
begin
  SelectSubtitleAndFocusMemo(False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actNextSubtitleExecute(Sender: TObject);
begin
  SelectSubtitleAndFocusMemo(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actOpenVideoExecute(Sender: TObject);
var
  OD : TOpenDialog;
  i  : Integer;
  s  : String;
begin
  OD := TOpenDialog.Create(Self);
  try
    s := '';
    for i := 0 to Length(TVideoExts)-1 do s := s + '*' + TVideoExts[i] + ';';
    OD.Filter := Strings.AllSupportedFiles + '|' + s;
    if OD.Execute then
    begin
      OpenVideo(OD.FileName);
    end;
  finally
    OD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actNewSubtitleExecute(Sender: TObject);
begin
  if CloseSubtitle then
  begin
    EnableWorkArea;
    VSTInsertSubtitles;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCloseSubtitleExecute(Sender: TObject);
begin
  CloseSubtitle;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCloseVideoExecute(Sender: TObject);
begin
  MPV.Engine.Play('');
//  MPV.Engine.UnInitialize;
  WAVE.Close;
  actCloseVideo.Enabled := False;
  actExtractWaveform.Enabled := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actConvertCaseExecute(Sender: TObject);
begin
  ShowTexts;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actLoadSubtitleExecute(Sender: TObject);
var
  OD : TOpenDialog;
begin
  OD := TOpenDialog.Create(Self);
  try
    OD.Filter := Subtitles.FillDialogFilter(Strings.AllSupportedFiles);
    if OD.Execute then
    begin
      LoadSubtitle(OD.FileName);
    end;
  finally
    OD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSaveSubtitleAsExecute(Sender: TObject);
var
  SD : TSaveDialog;
begin
  SD := TSaveDialog.Create(Self);
  try
    SD.Filter := Subtitles.FillDialogFilter('');
    SD.FilterIndex := cboFormat.ItemIndex+1;
    SD.FileName := ChangeFileExt(ExtractFileName(SubtitleFile.Text.FileName), '');
    if SD.Execute then
    begin
      SaveSubtitle(SD.FileName, TUWSubtitleFormats(SD.FilterIndex));
    end;
  finally
    SD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPlayExecute(Sender: TObject);
begin
  VLCPlay;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPlaySelectionExecute(Sender: TObject);
begin
  VLCPlay(mpmSelection);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPreviousFrameExecute(Sender: TObject);
begin
  MPV.Engine.PreviousFrame;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPlayFromSelectionExecute(Sender: TObject);
begin
  VLCPlay(mpmFromSelection);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPlayBeforeSelectionExecute(Sender: TObject);
begin
  VLCPlay(mpmBeforeSelection);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPlayAfterSelectionExecute(Sender: TObject);
begin
  VLCPlay(mpmAfterSelection);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaStopExecute(Sender: TObject);
begin
  MPV.Engine.Stop;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaZoomInExecute(Sender: TObject);
begin
  Wave.ZoomIn;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaZoomOutExecute(Sender: TObject);
begin
  Wave.ZoomOut;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaZoomSelectionExecute(Sender: TObject);
begin
  Wave.ZoomSelection;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaAutoScrollExecute(Sender: TObject);
begin
  ///
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaRewindExecute(Sender: TObject);
begin
  VLCSeekTo(False, 100);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaForwardExecute(Sender: TObject);
begin
  VLCSeekTo(True, 100);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaNextFrameExecute(Sender: TObject);
begin
  MPV.Engine.NextFrame;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaRewindExExecute(Sender: TObject);
begin
  VLCSeekTo(False, 5000);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaForwardExExecute(Sender: TObject);
begin
  VLCSeekTo(True, 5000);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaChangePlayRateExecute(Sender: TObject);
begin
  VLCAlterPlayRate(actMediaChangePlayRate.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaSetInitialTimeExecute(Sender: TObject);
begin
  if not (MPV.Engine.Duration > 0) or (VSTFocusedNode < 0) then Exit;
  VSTDoLoop(@ApplySetTimeInitialFromVLC);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaSetFinalTimeExecute(Sender: TObject);
begin
  if not (MPV.Engine.Duration > 0) or (VSTFocusedNode < 0) then Exit;
  VSTDoLoop(@ApplySetTimeFinalFromVLC);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaStartSubtitleExecute(Sender: TObject);
begin
  if not (MPV.Engine.Duration > 0) then Exit;
  LastSubtitle.InitialTime := MPV.Engine.Position;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaEndSubtitleExecute(Sender: TObject);
var
  i, l: Integer;
begin
  if not (MPV.Engine.Duration > 0) then Exit;

  if VSTFocusedNode >= 0 then
    i := VSTFocusedNode + 1
  else
    i := -1;

 l := MPV.Engine.Duration;
 if l >= LastSubtitle.InitialTime then
   InsertSubtitle(i, LastSubtitle.InitialTime, l, '', '');
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaAddSubtitleExecute(Sender: TObject);
var
  Item: TUWSubtitleItem;
begin
  if WAVE.IsPeakDataLoaded then
  begin
    if WAVE.SelectionIsEmpty and (WAVE.CursorPosMS > 0) then
    begin
      ClearSubtitleItem(Item);
      Item.InitialTime := WAVE.CursorPosMS;
      Item.FinalTime   := Item.InitialTime + Options.NewSubtitleMS;
      WAVE.SelectSubtitle(InsertSubtitle(Subtitles.FindInsertPos(Item), Item), True, False);
    end
    else if not WAVE.SelectionIsEmpty and WAVE.IsOnlySelection then
    begin
      WAVE.SelectSubtitle(InsertSubtitle(Subtitles.FindInsertPos(WAVE.Selection), WAVE.Selection), True, False);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShiftToPreviousExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyShiftToPrevious, dlSelected, True, True);
  //UpdateValues;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShiftToNextExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyShiftToNext, dlSelected, True, True);
  //UpdateValues;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAutomaticDurationExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyAutomaticDuration, dlSelected, True, True);
  //UpdateValues;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAutomaticDurationsExecute(Sender: TObject);
begin
  ShowTimings(1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDefaultPauseExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyDefaultPause, dlSelected, True, True);
  //UpdateValues;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actExecuteExtensionExecute(Sender: TObject);
begin
  ///
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSpellCheckExecute(Sender: TObject);
begin
  if frmSpellCheck = NIL then
  begin
    frmSpellCheck := TfrmSpellCheck.Create(Application);
    frmSpellCheck.ShowModal;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actStylesExecute(Sender: TObject);
begin
  if frmStylesAndActors = NIL then
  begin
    frmStylesAndActors := TfrmStylesAndActors.Create(Application);
    frmStylesAndActors.pagStylesAndActors.TabIndex := 0;
    frmStylesAndActors.Show;
  end
  else
  begin
    frmStylesAndActors.BringToFront;
    frmStylesAndActors.pagStylesAndActors.TabIndex := 0;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  if frmAbout = NIL then
  begin
    frmAbout := TfrmAbout.Create(Application);
    frmAbout.ShowModal;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actActorsExecute(Sender: TObject);
begin
  if frmStylesAndActors = NIL then
  begin
    frmStylesAndActors := TfrmStylesAndActors.Create(Application);
    frmStylesAndActors.pagStylesAndActors.TabIndex := 1;
    frmStylesAndActors.Show;
  end
  else
  begin
    frmStylesAndActors.BringToFront;
    frmStylesAndActors.pagStylesAndActors.TabIndex := 1;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actViewNumberExecute(Sender: TObject);
begin
  VSTShowColumn(0, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actViewTimesExecute(Sender: TObject);
begin
  VSTShowColumn(1, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actViewDurationExecute(Sender: TObject);
begin
  VSTShowColumn(2, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actViewStyleExecute(Sender: TObject);
begin
  VSTShowColumn(3, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actViewCPLExecute(Sender: TObject);
begin
  VSTShowColumn(8, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actViewCPSExecute(Sender: TObject);
begin
  VSTShowColumn(6, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actViewToolbarAdditionalExecute(Sender: TObject);
begin
  tlbExtra.Visible := (Sender as TAction).Checked;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actChangeWorkspaceExecute(Sender: TObject);
begin
  SetWorkspace(actChangeWorkspace.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actViewWPMExecute(Sender: TObject);
begin
  VSTShowColumn(7, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cboActorChange(Sender: TObject);
begin
  if cboActor.Tag = 0 then SetActor;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.CheckTM;
begin
  if not TranslatorMode or (frmTM = NIL) or (VST.SelectedCount <> 1) then Exit;
  if (VSTFocusedNode >= 0) and (Subtitles[VSTFocusedNode].Translation <> '') then Exit;

  frmTM.FindSimilary(RemoveSWTags(Subtitles[VST.GetFirstSelected^.Index].Text));
end;

// -----------------------------------------------------------------------------

end.

