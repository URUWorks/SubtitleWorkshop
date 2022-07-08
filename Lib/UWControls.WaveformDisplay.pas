{*
 *  URUWorks SW Waveform Display Control (custom modified version of great VSS)
 *}

// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2003 Christophe Paris
// -----------------------------------------------------------------------------
//  This Program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2, or (at your option)
//  any later version.
//
//  This Program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with GNU Make; see the file COPYING.  If not, write to
//  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
//  http://www.gnu.org/copyleft/gpl.html
// -----------------------------------------------------------------------------

unit UWControls.WaveformDisplay;

// -----------------------------------------------------------------------------

interface

uses
  Classes, Types, SysUtils, Controls, Graphics, Math, LazarusPackageIntf,
  UWParser.Wave, UWControls, UWSubtitleAPI, UWSubtitleAPI.Tags;

type

  { TUWPeak }

  PUWPeak = ^TUWPeak;
  TUWPeak = record
    Max : SmallInt;
    Min : Smallint;
  end;

  TUWPeakCreationEventType  = (pcetStart, pcetProgress, pcetStop);
  TUWPeakCreationEvent      = procedure (Sender: TObject; const EventType: TUWPeakCreationEventType; const Param: Integer) of object;

  { TUWWaveformDisplay }

  TUWCustomDrawSubtitleItem    = procedure(Sender: TObject; ACanvas: TCanvas; const Index: Integer; const SubtitleItem: TUWSubtitleItem; const Rect: TRect) of object;
  TUWSubtitleItemChangedEvent  = procedure(Sender: TObject; const Index: Integer; const OldInitialTime, OldFinalTime: Integer; const NeedSort: Boolean) of object;
  TUWSelectedSubtitleItemEvent = procedure(Sender: TObject; const Index: Integer; const SubtitleItem: TUWSubtitleItem; const IsDynamic: Boolean) of object;
  TUWSubtitleItemDblClickEvent = procedure(Sender: TObject; const SubtitleItem: TUWSubtitleItem) of object;

  TUWUpdateViewFlag  = (uvfCursor, uvfSelection, uvfSubtitle, uvfPosition, uvfPageSize, uvfPlayCursor);
  TUWUpdateViewFlags = set of TUWUpdateViewFlag;

  TUWDynamicEditMode = (demNone, demInitial, demFinal);

  TUWWaveformDisplay = class(TCustomControl)
  private
    FPeakTab        : array of TUWPeak;
    FPeakTabSize    : Cardinal;
    FSamplesPerPeak : Cardinal;
    FPeakDataLoaded : Boolean;

    FLengthMS   : Integer;
    FPositionMS : Integer;
    FPageSizeMS : Integer;

    FWaveFormat      : TWaveFormatEx;
    FSavePeakToFile  : Boolean;

    FVerticalScaling : Integer; // 1..800%
    FTimeLineHeight  : Byte;

    FSubtitles : TUWSubtitles;

    FDynamicEditMode : TUWDynamicEditMode;
    FDynamicSelSub   : PUWSubtitleItem;
    FDynamicEditTime : Integer;

    FSceneChangeList    : TIntegerDynArray;
    FSceneChangeEnabled : Boolean;

    FSelection          : TUWSubtitleItem;
    FSelectedSubtitle   : PUWSubtitleItem;

    FMouseIsDown     : Boolean;
    FDrawCursorTime  : Boolean;
    FSelectionX     : Integer;
    FNeedToSortList : Boolean;
    FCursorMS       : Integer;
    FPlayCursorMS   : Integer;

    FOldInitialTime : Integer;
    FOldFinalTime   : Integer;

    FBackBuffer     : TBitmap;
    FBackBufferWAVE : TBitmap;

    FScrollBar : TUWScrollBar;

    FTS : TTextStyle;

    FOnPeakCreation            : TUWPeakCreationEvent;
    FOnCustomDrawSubtitleItem  : TUWCustomDrawSubtitleItem;
    FOnItemChangedEvent        : TUWSubtitleItemChangedEvent;
    FOnCursorChange            : TNotifyEvent;
    FOnPlayCursorChange        : TNotifyEvent;
    FOnSelectionChange         : TNotifyEvent;
    FOnViewChange              : TNotifyEvent;
    FOnSelectedSubtitleItem       : TUWSelectedSubtitleItemEvent;
    FOnSelectedSubtitleItemChange : TNotifyEvent;
    FOnSubtitleItemStartDblClick  : TUWSubtitleItemDblClickEvent;
    FOnSubtitleItemStopDblClick   : TUWSubtitleItemDblClickEvent;

    function PixelToTime(const Pixel: Integer): Integer;
    function TimeToPixel(const Time: Integer): Integer;
    function GetWAVECanvasHeight: Integer;
    procedure DrawWave(const ABitmap: TBitmap);
    procedure DrawTimeLine(const ABitmap: TBitmap);
    procedure DrawItemsCanvas(const ABitmap: TBitmap);
    procedure DrawSubtitleItem(const ABitmap: TBitmap; const ATop, ABottom: Integer);
    procedure DrawSelection(const ABitmap: TBitmap);
    procedure DrawCursor(const ABitmap: TBitmap);
    procedure DrawPlayCursor(const ABitmap: TBitmap);
    procedure DrawSceneChange(const ABitmap: TBitmap; const ATop, ABottom: Integer);
    procedure SetSelectedSubtitleItem(const Value: PUWSubtitleItem; const UpdateDisplay: Boolean; const UpdatePosition: Boolean);
    function CheckSubtitleItemForDynamicSelection(const Subtitle: TUWSubtitleItem; const CursorPosMS, SubtitleSelWindow, X, Y: Integer): Boolean;
    procedure ClearPeakData;
    procedure CreatePeakTab(WAVFile: TUWWAVEFile);
    function NormalizePeakTab(NormFactor: Double): Boolean;
    procedure SetLengthMS(const LenghtMS: Integer);
    procedure SetPositionMS(NewPosition: Integer);
    procedure OnScrollBarChange(Sender: TObject);
    function GetPositionMS: Integer;
    procedure UpdateView(const UpdateViewFlags: TUWUpdateViewFlags; const DoRepaint: Boolean = True);
    procedure ZoomSubtitle(const Subtitle: TUWSubtitleItem); overload;
    procedure ZoomSubtitle(const Start, Stop: Integer); overload;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadWaveFromFile(const FileName: String): Boolean;
    procedure GenerateDummyPeakTab(const LengthMS: Cardinal);
    procedure Close;
    function IsPeakDataLoaded: Boolean;
    function IsOnlySelection: Boolean;
    function SelectionIsEmpty: Boolean;
    procedure ClearSelection;
    function GetPlayCursorMS: Integer;
    procedure SetPlayCursorMS(NewPosMS: Integer);
    procedure ZoomAll;
    procedure ZoomIn;
    procedure ZoomOut;
    procedure ZoomSelection;
    procedure VZoomRestore;
    procedure VZoomMore;
    procedure VZoomLess;
    procedure SelectSubtitle(const Index: Integer; const UpdateDisplay: Boolean; const UpdatePosition: Boolean);
    function GetSubtitleIdxAtCursorPos: Integer;
    procedure DoUpdate(const Complete: Boolean = True);
    procedure SetSceneChangeList(const SceneChangeList: TIntegerDynArray);
    procedure ClearSceneChange;
    property Selection: TUWSubtitleItem read FSelection;
    property SubTextStyle                  : TTextStyle                   read FTS                           write FTS;
  published
    property Subtitles                     : TUWSubtitles                 read FSubtitles                    write FSubtitles;
    property CursorPosMS                   : Integer                      read FCursorMS;
    property SceneChangeEnabled            : Boolean                      read FSceneChangeEnabled           write FSceneChangeEnabled;
    property SavePeakToFile                : Boolean                      read FSavePeakToFile               write FSavePeakToFile;
    property OnCustomDrawSubtitleItem      : TUWCustomDrawSubtitleItem    read FOnCustomDrawSubtitleItem     write FOnCustomDrawSubtitleItem;
    property OnCursorChange                : TNotifyEvent                 read FOnCursorChange               write FOnCursorChange;
    property OnPlayCursorChange            : TNotifyEvent                 read FOnPlayCursorChange           write FOnPlayCursorChange;
    property OnSelectionChange             : TNotifyEvent                 read FOnSelectionChange            write FOnSelectionChange;
    property OnViewChange                  : TNotifyEvent                 read FOnViewChange                 write FOnViewChange;
    property OnSelectedSubtitleItem        : TUWSelectedSubtitleItemEvent read FOnSelectedSubtitleItem       write FOnSelectedSubtitleItem;
    property OnSelectedSubtitleItemChange  : TNotifyEvent                 read FOnSelectedSubtitleItemChange write FOnSelectedSubtitleItemChange;
    property OnSelectedSubtitleItemChanged : TUWSubtitleItemChangedEvent  read FOnItemChangedEvent           write FOnItemChangedEvent;
    property OnPeakCreation                : TUWPeakCreationEvent         read FOnPeakCreation               write FOnPeakCreation;
    property OnSubtitleItemStartDblClick   : TUWSubtitleItemDblClickEvent read FOnSubtitleItemStartDblClick  write FOnSubtitleItemStartDblClick;
    property OnSubtitleItemStopDblClick    : TUWSubtitleItemDblClickEvent read FOnSubtitleItemStopDblClick   write FOnSubtitleItemStopDblClick;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Enabled;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Visible;
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

uses UWSystem.TimeUtils, UWSystem.SysUtils;

const
  WAV_COLOR                   : TColor = $00998877; //$00AC8439; //clLime; //$00A7F24A;
  WAV_BACK_COLOR              : TColor = $00111111; //$303030; //$1E1E1E; //clBlack;
  ZERO_LINE_COLOR             : TColor = $00998877; //clGreen;
  SUBTITLE_COLOR_1            : TColor = $55FF8000;
  SUBTITLE_COLOR_2            : TColor = $553333FF;
  RULER_BACK_COLOR            : TColor = $00998877;
  RULER_TOP_BOTTOM_LINE_COLOR : TColor = $00D3D3D3;
  RULER_TEXT_COLOR            : TColor = $00E0E0E0;
  CURSOR_COLOR                : TColor = clYellow;
  DISABLED_BACK_COLOR         : TColor = $00111111; //clBlack;

// -----------------------------------------------------------------------------

{ Helpers }

//------------------------------------------------------------------------------

function TimeMSToShortString(const TimeMS: Cardinal; const Precision : Cardinal): String;
var
  min, sec, ms: Cardinal;
begin
  ms  := TimeMS div 1000;
  min := ms div 60;
  sec := ms mod 60;
  ms  := (TimeMS - (min * 60 * 1000) - (sec * 1000)) div Precision;

  if (min > 0) then
  begin
    if (ms > 0) then
      Result := Format('%d:%.2d.%d', [min, sec, ms])
    else
      Result := Format('%d:%.2d', [min, sec]);
  end
  else
    Result := Format('%d.%d', [sec, ms]);
end;

// -----------------------------------------------------------------------------

function IsEqualItem(const I1, I2: TUWSubtitleItem): Boolean;
begin
  Result := (I1.InitialTime = I2.InitialTime) and (I1.FinalTime = I2.FinalTime);
end;

// -----------------------------------------------------------------------------

{ TUWWaveformDisplay }

// -----------------------------------------------------------------------------

constructor TUWWaveformDisplay.Create(AOwner: TComponent);
begin
  inherited;

  FLengthMS        := 0;
  FPositionMS      := 0;
  FPageSizeMS      := 5000;
  FPeakDataLoaded  := False;
  FSavePeakToFile  := True;
  FVerticalScaling := 85;
  FTimeLineHeight  := 18;

  FDynamicEditMode := demNone;
  FDynamicSelSub   := NIL;
  FDynamicEditTime := 0;

  FSceneChangeEnabled    := True;

  FSelection.InitialTime := 0;
  FSelection.FinalTime   := 0;

  FMouseIsDown           := False;
  FDrawCursorTime        := False;
  FNeedToSortList        := False;
  FSelectionX            := -1;
  FCursorMS              := 0;
  FPlayCursorMS          := 0;

  FOldInitialTime        := -1;
  FOldFinalTime          := -1;

  FScrollBar          := TUWScrollBar.Create(Self);
  FScrollBar.Parent   := Self;
  FScrollBar.Height   := 7;
  FScrollBar.Align    := alBottom;
  FScrollBar.Min      := 0;
  FScrollBar.Max      := FLengthMs;
  FScrollBar.Position := 0;
  FScrollBar.PageSize := FLengthMs;
  FScrollBar.OnChange := @OnScrollBarChange;

  FillByte(FTS, SizeOf(FTS), 0);
  FTS.Clipping    := True;
  FTS.SingleLine  := False;
  //FTS.EndEllipsis := True;

  FBackBufferWAVE     := TBitmap.Create;
  FBackBufferWAVE.PixelFormat := pf24bit;
  FBackBufferWAVE.SetSize(Width, Height);
  FBackBufferWAVE.Canvas.Brush.Color := DISABLED_BACK_COLOR;
  FBackBufferWAVE.Canvas.FillRect(FBackBufferWAVE.Canvas.ClipRect);
  FBackBuffer         := TBitmap.Create;
  FBackBuffer.PixelFormat := pf24bit;
  FBackBuffer.SetSize(Width, Height);
  FBackBuffer.Canvas.Brush.Color := DISABLED_BACK_COLOR;
  FBackBuffer.Canvas.FillRect(FBackBuffer.Canvas.ClipRect);
end;

// -----------------------------------------------------------------------------

destructor TUWWaveformDisplay.Destroy;
begin
  FPeakTab       := NIL;
  FSubtitles     := NIL;
  FDynamicSelSub := NIL;
  FBackBufferWAVE.Free;
  FBackBuffer.Free;
  FScrollBar.Free;

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TUWWaveformDisplay.Paint;
begin
  Canvas.Lock;
  try
    if (FBackBuffer.Width > 0) and (FBackBuffer.Height > 0) then
      Canvas.Draw(0, 0, FBackBuffer)
    else
    begin
      Canvas.Brush.Color := DISABLED_BACK_COLOR;
      Canvas.FillRect(ClientRect);
    end;
  finally
    Canvas.Unlock;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWWaveformDisplay.Resize;
begin
  inherited;
  UpdateView([uvfPageSize], False);
end;

// -----------------------------------------------------------------------------

function TUWWaveformDisplay.PixelToTime(const Pixel: Integer): Integer;
begin
  if FPageSizeMS = 0 then
    Result := 0
  else
    Result := Round(Pixel * (FPageSizeMS / Width));
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplay.TimeToPixel(const Time: Integer): Integer;
begin
  if FPageSizeMS = 0 then
    Result := 0
  else
    Result := Round(Time / (FPageSizeMs / Width));
end;

// -----------------------------------------------------------------------------

function TUWWaveformDisplay.GetWAVECanvasHeight: Integer;
begin
  Result := Trunc(Height - FScrollBar.Height) - FTimeLineHeight;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.DrawWave(const ABitmap: TBitmap);
var
  x, y1, y2            : Integer;
  x1_update, x2_update : Integer;
  x_scaled             : Cardinal;
  PeaksPerPixelScaled  : Double;
  StartPositionInPeaks : Double;
  Middle               : Integer;
  PeakMax, PeakMin     : Integer;
  i                    : Integer;
  Rect                 : TRect;
  RectHeight           : Integer;
begin
  FBackBufferWAVE.Canvas.Brush.Color := WAV_BACK_COLOR;
  FBackBufferWAVE.Canvas.FillRect(FBackBufferWAVE.Canvas.ClipRect);
  if not FPeakDataLoaded then Exit;

  with ABitmap do
  begin
    Canvas.Lock;
    try
      PeaksPerPixelScaled := (((FPageSizeMS / 1000.0) * FWaveFormat.nSamplesPerSec) / FSamplesPerPeak) / Width;
      StartPositionInPeaks := ((FPositionMS / 1000.0) * FWaveFormat.nSamplesPerSec) / FSamplesPerPeak;

      x1_update := 0;
      x2_update := Width;

      Rect        := ClientRect;
      Rect.Left   := x1_update;
      Rect.Right  := x2_update;
      Rect.Top    := Rect.Top + FTimeLineHeight;
      Rect.Bottom := Rect.Bottom - FScrollBar.Height;

      RectHeight := Rect.Bottom - Rect.Top;
      Middle     := Rect.Top + (RectHeight div 2);

      // Wave
      Canvas.Pen.Color := WAV_COLOR;

      for x := Trunc(x1_update) to Trunc(x2_update) do
      begin
        x_scaled := Round((x * PeaksPerPixelScaled) + StartPositionInPeaks);

        if (x_scaled >= FPeakTabSize) then x_scaled := FPeakTabSize - 1;

        // calculate peak from x_scaled to next x_scaled
        PeakMax := FPeakTab[x_scaled].Max;
        PeakMin := FPeakTab[x_scaled].Min;
        for i := x_scaled+1 to Min(Round(((x+1)*PeaksPerPixelScaled)+StartPositionInPeaks), FPeakTabSize)-1 do
        begin
          if FPeakTab[i].Max > PeakMax then
            PeakMax := FPeakTab[i].Max;

          if FPeakTab[i].Min < PeakMin then
            PeakMin := FPeakTab[i].Min;
        end;

        y1 := Round((((PeakMax * FVerticalScaling) / 100) * RectHeight) / 65536);
        y2 := Round((((PeakMin * FVerticalScaling) / 100) * RectHeight) / 65536);

        Canvas.Line(x, Middle-y1, x, Middle-y2);
      end;

      // zero line
      Canvas.Pen.Color := ZERO_LINE_COLOR;
      Canvas.Line(0, Middle, Width, Middle);
    finally
      Canvas.Unlock;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.DrawTimeLine(const ABitmap: TBitmap);
var
  PosRect         : TRect;
  PosString       : String;
  p, x, x1, x2,
  MaxPosStep,
  StepMs, StepLog : Integer;
begin
  if not FPeakDataLoaded then Exit;

  with ABitmap do
  begin
    Canvas.Lock;
    try
      PosRect        := ClientRect;
      PosRect.Bottom := PosRect.Top + FTimeLineHeight;

      // Draw background
      Canvas.Brush.Color := RULER_BACK_COLOR;
      Canvas.FillRect(PosRect);

      // Draw horizontal line at top and bottom
      Canvas.Pen.Width := 1;
      Canvas.Pen.Color := RULER_TOP_BOTTOM_LINE_COLOR;
      Canvas.Line(0, PosRect.Top, Width, PosRect.Top);

      // Set the text font
      Canvas.Pen.Color := RULER_TEXT_COLOR;
      Canvas.Font.Name := 'Time New Roman';
      Canvas.Font.Size := 8;

      // Do some little calculation to try to show "round" time
      MaxPosStep := Round(Width / (Canvas.TextWidth('000:00.0') * 2));
      StepMs     := Round(FPageSizeMS / MaxPosStep);
      if StepMs = 0 then StepMs := 1;
      StepLog    := Trunc(Power(10, Trunc(Log10(StepMs))));
      StepMs     := StepMs div StepLog * StepLog;

      p := (FPositionMS div StepMs * StepMs);
      while (p < FPositionMS + FPageSizeMS) do
      begin
        // Draw main division
        x := TimeToPixel(p - FPositionMS);
        Canvas.Line(x, PosRect.Bottom - 7, x, PosRect.Bottom - 1);
        PosString := TimeMSToShortString(p, StepLog);
        // Calculate text coordinate
        x1 := x - (Trunc(Canvas.TextWidth(PosString)) div 2);
        // Draw text
        Canvas.Font.Color := RULER_TEXT_COLOR;
        Canvas.TextOut(x1, PosRect.Top + 4, PosString);
        // Draw subdivision
        x2 := x + TimeToPixel(StepMs div 2);
        Canvas.Line(x2, PosRect.Bottom - 4, x2, PosRect.Bottom - 1);
        p := p + StepMs;
      end;
    finally
      Canvas.Unlock;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.DrawItemsCanvas(const ABitmap: TBitmap);
var
  y1, y2       : Integer;
  CanvasHeight : Double;
begin
  CanvasHeight := GetWAVECanvasHeight;
  ABitmap.Canvas.Font.Style := [TFontStyle.fsBold];

  DrawSelection(ABitmap);

  // Draw Subtitle Items
  y1 := FTimeLineHeight;
  y2 := Round(CanvasHeight) + FTimeLineHeight;
  DrawSceneChange(FBackBuffer, y1, y2);
  DrawSubtitleItem(FBackBuffer, y1, y2);

  DrawCursor(ABitmap);
  DrawPlayCursor(ABitmap);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.DrawSubtitleItem(const ABitmap: TBitmap; const ATop, ABottom: Integer);

  procedure _TextOut(const X, Y: Integer; const S: String);
  begin
    with ABitmap.Canvas do
    begin
      Font.Color := clBlack;
      TextOut(X, Y-1, S);
      TextOut(X, Y+1, S);
      TextOut(X-1, Y, S);
      TextOut(X+1, Y, S);
      Font.Color := clLtGray;
      TextOut(X, Y, S);
    end;
  end;

  procedure _TextOut(const R: TRect; const S: String);
  begin
    with ABitmap.Canvas do
    begin
      Font.Color := clBlack;
      TextRect(R, R.Left, R.Top-1, S, FTS);
      TextRect(R, R.Left, R.Top+1, S, FTS);
      TextRect(R, R.Left-1, R.Top, S, FTS);
      TextRect(R, R.Left+1, R.Top, S, FTS);
      Font.Color := clLtGray;
      TextRect(R, R.Left, R.Top, S, FTS);
    end;
  end;

var
  i         : Integer;
  r         : TUWSubtitleItem;
  x1, x2,
  y1, y2, z : Integer;
  ShowStart, ShowStop, FullHLines : Boolean;
  CustomDrawRect : TRect;
  sTime     : String;
begin
  ABitmap.Canvas.Lock;
  begin
    with ABitmap do
    try
      y1 := ATop    + 1;
      y2 := ABottom - 2;

      for i := 0 to FSubtitles.Count-1 do
      begin
        r  := FSubtitles[i];
        x1 := -1;
        x2 := -1;

        if (r.InitialTime >= FPositionMS) and (r.InitialTime <= FPositionMS + FPageSizeMS) then
          x1 := TimeToPixel(r.InitialTime - FPositionMS);

        if (r.FinalTime >= FPositionMS) and (r.FinalTime <= FPositionMS + FPageSizeMS) then
          x2 := TimeToPixel(r.FinalTime - FPositionMS);

        if x1 > ABitmap.Width then Break;

        ShowStart  := (x1 <> -1);
        ShowStop   := (x2 <> -1) and (x2 <> x1);
        FullHLines := (r.InitialTime < FPositionMS) and (r.FinalTime > FPositionMS + FPageSizeMS);

        if (ShowStart or ShowStop or FullHLines) then
        begin
          if (i mod 2) = 0 then
          begin
            Canvas.Pen.Color   := SUBTITLE_COLOR_1;
            //Canvas.Brush.Color := SUBTITLE_COLOR_1;
          end
          else
          begin
            Canvas.Pen.Color   := SUBTITLE_COLOR_2;
            //Canvas.Brush.Color := SUBTITLE_COLOR_2;
          end;
        end;

        Canvas.Brush.Color := Canvas.Pen.Color;
        Canvas.Font.Color  := Canvas.Pen.Color;
        Canvas.Brush.Style := bsClear;
        Canvas.Pen.Style   := psSolid;
        Canvas.Pen.Width   := 1; //2;
        // Paint start time
        if ShowStart then
        begin
          Canvas.Line(x1, y1, x1, y2);
          z := x2;
          if z = -1 then z := FBackBuffer.Width-1;
          if (z - x1) > 70 then
          begin
            sTime := TrimTimeString(TimeToString(r.InitialTime, 'h:mm:ss.zzz'));
            _TextOut(x1+3, ABottom-FTimeLineHeight, sTime);
          end;
        end;

        // Paint stop time
        if ShowStop then
        begin
          Canvas.Line(x2, y1, x2, y2);
          z := x1;
          if z = -1 then z := 0;
          if (x2 - z) > 140 then
          begin
            sTime := TrimTimeString(TimeToString(r.FinalTime, 'h:mm:ss.zzz'));
            _TextOut(x2-3-Canvas.TextWidth(sTime), ABottom-FTimeLineHeight, sTime);
          end;
        end;

        // Draw the top and bottom horizontal lines
        if FullHLines then
        begin
          x1 := 0;
          x2 := FBackBuffer.Width; //-1;
        end;

        if ((x1 <> -1) or (x2 <> -1)) then
        begin
          if (x1 = -1) then
            x1 := 0
          else if (x2 = -1) then
            x2 := FBackBuffer.Width; //-1;
          // Draw top h-line
          Canvas.Line(x1, y1, x2, y1);
          // Draw bottom h-line
          Canvas.Line(x1, y2, x2, y2);

          // Custom draw
          if ((x2 - x1) > 10) then
          begin
            CustomDrawRect.Top    := y1+5;
            CustomDrawRect.Left   := x1+5;
            CustomDrawRect.Right  := x2-5;
            CustomDrawRect.Bottom := y2-5;

            if Assigned(FOnCustomDrawSubtitleItem) then
              FOnCustomDrawSubtitleItem(Self, Canvas, i, r, CustomDrawRect)
            else
              _TextOut(CustomDrawRect, '#' + IntToStr(i+1) + ': ' + RemoveSWTags(Subtitles[i].Text));
          end;
        end;
      end;
    finally
      ABitmap.Canvas.UnLock;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.DrawSelection(const ABitmap: TBitmap);

  procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
  var
    X    : Integer;
    AM   : TAntialiasingMode;
    _Pen : TPen;
  begin
    _Pen := TPen.Create;
    try
      AM   := C.AntialiasingMode;
      _Pen.Assign(C.Pen);

      X := (R.Left+R.Right) div 2;
      C.Pen.Mode         := pmNotXor;
      C.Pen.Style        := psSolid;
      C.Pen.Color        := AColor;
      C.AntialiasingMode := amOff;
      C.Pen.EndCap       := pecFlat;
      C.Pen.Width        := R.Right-R.Left;

      C.MoveTo(X, R.Top);
      C.LineTo(X, R.Bottom);

      C.Pen.Assign(_Pen);
      C.AntialiasingMode := AM;
      C.Rectangle(0, 0, 0, 0); //apply pen
    finally
      _Pen.Free;
    end;
  end;

var
  x1, x2       : Integer;
  CanvasHeight : Integer;
  SelRect      : TRect;
begin
  CanvasHeight := GetWAVECanvasHeight;
  // Selection
  if (FSelection.FinalTime > 0) then
    with ABitmap do
    begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Pen.Style   := psDot;
      Canvas.Pen.Color   := clWhite;

      x1 := TimeToPixel(FSelection.InitialTime - FPositionMS);
      x2 := TimeToPixel(FSelection.FinalTime - FPositionMS);

      if (x1 = x2) then
      begin
        // points are on each other and in the display Subtitle
        if (FSelection.InitialTime >= FPositionMS) and
           (FSelection.InitialTime <= FPositionMS + FPageSizeMS) then
        begin
          Canvas.Line(x1, 0, x1, CanvasHeight);
        end;
      end
      else
      begin
        Constrain(x1, 0, Trunc(Width));
        Constrain(x2, 0, Trunc(Width));
        if (x1 <> x2) then
        begin
          SelRect           := ClientRect;
          SelRect.Left      := x1;
          SelRect.Right     := x2+1;
          SelRect.Top       := SelRect.Top + FTimeLineHeight;
          SelRect.Bottom    := SelRect.Bottom - FScrollBar.Height;
          //Canvas.FillRect(SelRect);
          CanvasInvertRect(Canvas, SelRect, clBlack);
        end;
      end;
    end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.DrawCursor(const ABitmap: TBitmap);
var
  x, t  : Integer;
  sTime : String;
begin
  // Cursor
  if (FCursorMS >= FPositionMS) and (FCursorMS <= FPositionMS + FPageSizeMS) then
    with ABitmap do
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style   := psDot;
      Canvas.Pen.Color   := CURSOR_COLOR;
      x := TimeToPixel(FCursorMS - FPositionMS);
      Canvas.Line(x, ClientRect.Top + FTimeLineHeight + 1, x, ClientRect.Bottom - FScrollBar.Height - 1);

      if FDrawCursorTime then
      begin
        Canvas.Font.Color  := clBlack;
        Canvas.Brush.Color := CURSOR_COLOR;
        sTime := TimeToString(FCursorMS, 'h:mm:ss.zzz');
        t     := (((ClientRect.Bottom - FScrollBar.Height) + (ClientRect.Top + FTimeLineHeight)) - Canvas.TextHeight(sTime)) div 2;

        if x < ((ClientRect.Right-ClientRect.Left) div 2) then
          Canvas.TextOut(x+2, t, sTime)
        else
          Canvas.TextOut(x-2-Canvas.TextWidth(sTime), t, sTime);
      end;
    end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.DrawPlayCursor(const ABitmap: TBitmap);
var
  x : Integer;
begin
  // Play Cursor
  if (FPlayCursorMS >= FPositionMS) and (FPlayCursorMS <= FPositionMS + FPageSizeMS) then
    with ABitmap do
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style   := psSolid; //psDot;
      Canvas.Pen.Color   := clLtGray;
      x := TimeToPixel(FPlayCursorMS - FPositionMS);
      Canvas.Line(x, ClientRect.Top + FTimeLineHeight + 1, x, ClientRect.Bottom - FScrollBar.Height - 1);
    end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.DrawSceneChange(const ABitmap: TBitmap; const ATop, ABottom: Integer);
var
  x, i, SceneChange : Integer;
begin
  if FSceneChangeEnabled then
    with ABitmap do
    begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Pen.Style   := psSolid;
      Canvas.Pen.Color   := clWhite;

      for i := Low(FSceneChangeList) to High(FSceneChangeList) do
      begin
        SceneChange := FSceneChangeList[i];
        if (SceneChange >= FPositionMS) and (SceneChange <= FPositionMs + FPageSizeMS) then
        begin
          x := TimeToPixel(SceneChange - FPositionMS);
          Canvas.Line(x, ATop, x, ABottom);
        end;
      end;
    end;
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplay.GetPlayCursorMS: Integer;
begin
  Result := FPlayCursorMS;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.SetPlayCursorMS(NewPosMS: Integer);
begin
  Constrain(NewPosMS, 0, FLengthMS);
  if FPlayCursorMS <> NewPosMS then
  begin
    FPlayCursorMS := NewPosMS;
    if Assigned(FOnPlayCursorChange) then FOnPlayCursorChange(Self);
    if  not FMouseIsDown and ((NewPosMS < FPositionMS) or (FPlayCursorMS > (FPositionMS + FPageSizeMS))) then
    begin
      // Keep 1/8 of the previous display
      SetPositionMS(NewPosMS - (FPageSizeMS div 8));
    end
    else
      UpdateView([uvfPlayCursor]);
  end;
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplay.IsOnlySelection: Boolean;
begin
  Result := (not SelectionIsEmpty) and (not Assigned(FSelectedSubtitle));
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplay.SelectionIsEmpty: Boolean;
begin
  Result := (FSelection.FinalTime = 0);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.ClearSelection;
begin
  SetSelectedSubtitleItem(NIL, False, False);
  if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
  UpdateView([uvfSelection, uvfSubtitle]);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.SetSelectedSubtitleItem(const Value: PUWSubtitleItem; const UpdateDisplay: Boolean; const UpdatePosition: Boolean);
begin
  // Make sure to reset selection if Value is nil
  if (Value = NIL) then
  begin
    FSelection.InitialTime := 0;
    FSelection.FinalTime   := 0;
  end;

  if (FSelectedSubtitle <> Value) then
  begin
    FSelectedSubtitle := Value;
    if (FSelectedSubtitle <> NIL) then
    begin
      FSelection.InitialTime := FSelectedSubtitle^.InitialTime;
      FSelection.FinalTime   := FSelectedSubtitle^.FinalTime;
    end;

    if UpdatePosition and (Value <> NIL) then
      SetPositionMS(Value^.InitialTime);

    if UpdateDisplay and not UpdatePosition then
      UpdateView([uvfSelection, uvfSubtitle]);

    if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
  end;
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplay.CheckSubtitleItemForDynamicSelection(const Subtitle: TUWSubtitleItem;
  const CursorPosMS, SubtitleSelWindow, X, Y: Integer): Boolean;
var
  NewDynamicEditMode : TUWDynamicEditMode;
  CanvasHeight       : Integer;
begin
  Result             := False;
  NewDynamicEditMode := demNone;

  if (((Subtitle.FinalTime - Subtitle.InitialTime) / SubtitleSelWindow) > 2) then
  begin
    CanvasHeight := GetWAVECanvasHeight;
    if (Y > 0) and (Y < CanvasHeight) then
    begin
      if (Abs(CursorPosMs - Subtitle.InitialTime) < SubtitleSelWindow) then
      begin
        NewDynamicEditMode := demInitial;
        FDynamicEditTime   := Subtitle.InitialTime;
      end
      else if (Abs(Subtitle.FinalTime - CursorPosMs) < SubtitleSelWindow) then
      begin
        NewDynamicEditMode := demFinal;
        FDynamicEditTime   := Subtitle.FinalTime;
      end;
    end;
  end;

  if (NewDynamicEditMode <> demNone) then
  begin
    Result           := True;
    Cursor           := crHSplit;

    FDynamicEditMode := NewDynamicEditMode;
    if not IsEqualItem(Subtitle, FSelection) then
      FDynamicSelSub := @Subtitle
    else if Assigned(FSelectedSubtitle) then
      FDynamicSelSub := FSelectedSubtitle
    else
      FDynamicSelSub := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  UpdateFlags  : TUWUpdateViewFlags;
  ACursorPosMs : Integer;
begin
  inherited;

  if (ssDouble in Shift) or (FLengthMs = 0) or (not InRange(X, 0, Width)) then Exit;

  UpdateFlags  := [];
  if (ssLeft in Shift) and not (ssCtrl in Shift) then
  begin
    FMouseIsDown := True;

    // Re-adjust mouse cursor position
    if (FDynamicEditMode = deminitial) or (FDynamicEditMode = demfinal) then
    begin
      X := TimeToPixel(FDynamicEditTime - FPositionMS);
      //Windows.SetCursorPos(X + ClientOrigin.X, Y + ClientOrigin.Y);
    end;
    ACursorPosMs := PixelToTime(Trunc(X)) + FPositionMS;

    if (ssShift in Shift) or (FDynamicEditMode = demInitial) or (FDynamicEditMode = demFinal) then
    begin
      if Assigned(FDynamicSelSub) and (FDynamicSelSub <> @FSelectedSubtitle) then
      begin
        SetSelectedSubtitleItem(FDynamicSelSub, False, False);
        Include(UpdateFlags, uvfSelection);
        if Assigned(FOnSelectedSubtitleItem) then FOnSelectedSubtitleItem(Self, FSubtitles.IndexOf(FSelectedSubtitle), FSelectedSubtitle^, True);
      end;

      // Selection modification using shift key
      if (ACursorPosMs > FSelection.InitialTime + ((FSelection.FinalTime - FSelection.InitialTime) div 2)) then
      begin
        // We are close to the end of the selection
        if SelectionIsEmpty then
        begin
          if (ACursorPosMs > FCursorMs) then
          begin
            FSelection.FinalTime   := ACursorPosMs;
            FSelection.InitialTime := FCursorMs;
          end
          else
          begin
            FSelection.FinalTime   := FCursorMs;
            FSelection.InitialTime := ACursorPosMs;
          end;
        end
        else
          FSelection.FinalTime := ACursorPosMs;

        FSelectionX := FSelection.InitialTime;
      end
      else
      begin
        // We are close to the start of the selection
        FSelection.InitialTime := ACursorPosMs;
        FSelectionX            := FSelection.FinalTime;
      end;

      if Assigned(FSelectedSubtitle) then
      begin
        FNeedToSortList := True;
        FOldInitialTime := FSelectedSubtitle^.InitialTime;
        FOldFinalTime   := FSelectedSubtitle^.FinalTime;
        FSelectedSubtitle^.InitialTime := FSelection.InitialTime;
        FSelectedSubtitle^.FinalTime   := FSelection.FinalTime;
        Include(UpdateFlags, uvfSubtitle);
        if Assigned(FOnSelectedSubtitleItem) and (FDynamicEditMode = demNone) then FOnSelectedSubtitleItemChange(Self);
      end;
      if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
      Include(UpdateFlags, uvfSelection);
    end
    else
    begin
      if (FSelection.InitialTime <> FSelection.FinalTime) then Include(UpdateFlags, uvfSelection); // clear selection
      FSelectionX := ACursorPosMs;
      SetSelectedSubtitleItem(NIL, False, False);
    end;

    if (FCursorMS <> ACursorPosMs) and (FDynamicEditMode = demNone) then
    begin
      FCursorMS := ACursorPosMs;
      if Assigned(FOnCursorChange) then FOnCursorChange(Self);
    end;

    UpdateView(UpdateFlags);
  end
  else if (ssLeft in Shift) and (ssCtrl in Shift) then
  begin
    FDrawCursorTime        := True;
    FSelection.InitialTime := 0;
    FSelection.FinalTime   := 0;
    FCursorMs := PixelToTime(Trunc(X)) + FPositionMS;
    UpdateView([uvfCursor]);
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ACursorPosMs      : Integer;
  SubtitleUnder     : PUWSubtitleItem;
  SubtitleSelWindow : Integer;
  UpdateFlags       : TUWUpdateViewFlags;
begin
  inherited;

  if (ssDouble in Shift) or (FLengthMs = 0) then Exit;

  UpdateFlags := [];

  if (FMouseIsDown) then
  begin
    if (ssLeft in Shift) then
    begin
      Constrain(X, 0, Width);
      ACursorPosMS := PixelToTime(Trunc(X)) + FPositionMS;

      if (FSelectionX <> -1) and (FSelectionX <> ACursorPosMS) then
      begin
        // Update selection
        if (ACursorPosMS > FSelectionX) then
        begin
            FSelection.InitialTime := FSelectionX;
            FSelection.FinalTime   := ACursorPosMS;
        end
        else
        begin
            FSelection.InitialTime := ACursorPosMS;
            FSelection.FinalTime   := FSelectionX;
        end;

        if Assigned(FSelectedSubtitle) then
        begin
          FNeedToSortList := True;
          if (FSelectedSubtitle^.InitialTime <> FSelection.InitialTime) or
             (FSelectedSubtitle^.FinalTime <> FSelection.FinalTime) then
          begin
            FSelectedSubtitle^.InitialTime := FSelection.InitialTime;
            FSelectedSubtitle^.FinalTime   := FSelection.FinalTime;
            Include(UpdateFlags, uvfSubtitle);
            if Assigned(FOnSelectedSubtitleItemChange) then FOnSelectedSubtitleItemChange(Self);
          end;
        end;
        if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
        Include(UpdateFlags, uvfSelection);
      end;

      if (FCursorMS <> ACursorPosMS) and (FDynamicEditMode = demNone) then
      begin
        FCursorMS := ACursorPosMS;
        if Assigned(FOnCursorChange) then FOnCursorChange(Self);
      end;

      UpdateView([uvfSelection, uvfSubtitle]);
    end;
  end
  else if not FDrawCursorTime then
  begin
    Constrain(X, 0, Width);
    ACursorPosMS := PixelToTime(Trunc(X)) + FPositionMS;

    // "Dynamic selection"
    if (Shift = []) then
    begin
      // Find a subtitle under the mouse
      SubtitleSelWindow := PixelToTime(4);
      if (SubtitleSelWindow < 1) then SubtitleSelWindow := 1;

      // First pass : check only inside sub
      SubtitleUnder := FSubtitles.FindFirstPointer(ACursorPosMS, 0);
      while Assigned(SubtitleUnder) do
      begin
        if CheckSubtitleItemForDynamicSelection(SubtitleUnder^, ACursorPosMS, SubtitleSelWindow, Trunc(X), Trunc(Y)) then Exit;
        SubtitleUnder := FSubtitles.FindNextPointer;
      end;

      // 2nd pass : Wider search
      SubtitleSelWindow := PixelToTime(2);
      if (SubtitleSelWindow < 1) then SubtitleSelWindow := 1;
      SubtitleUnder := FSubtitles.FindFirstPointer(ACursorPosMS, SubtitleSelWindow);
      while Assigned(SubtitleUnder) do
      begin
        if CheckSubtitleItemForDynamicSelection(SubtitleUnder^, ACursorPosMS, SubtitleSelWindow, Trunc(X), Trunc(Y)) then Exit;
        SubtitleUnder := FSubtitles.FindNextPointer;
      end;

      // Check selection
      if not SelectionIsEmpty then
      begin
        SubtitleSelWindow := PixelToTime(4);
        if (SubtitleSelWindow < 1) then SubtitleSelWindow := 1;
        if CheckSubtitleItemForDynamicSelection(FSelection, ACursorPosMS, SubtitleSelWindow, Trunc(X), Trunc(Y)) then Exit;
      end;

      Cursor           := crDefault;
      FDynamicEditMode := demNone;
      FDynamicSelSub := NIL;
    end
    else
    begin
      Cursor           := crDefault;
      FDynamicEditMode := demNone;
      FDynamicSelSub := NIL;
    end;
  end
  else if FDrawCursorTime then
  begin
    FCursorMS := PixelToTime(Trunc(X)) + FPositionMS;
    UpdateView([uvfCursor]);
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (FMouseIsDown) then
  begin
    if ((FSelection.FinalTime - FSelection.InitialTime) < 40) and
      (not (Assigned(FSelectedSubtitle) or Assigned(FDynamicSelSub))) then
      ClearSelection;

    // The selected sub has changed, we need to keep Subtitle list sorted
    if FNeedToSortList then
    begin
      if Assigned(FOnItemChangedEvent) and Assigned(FSelectedSubtitle) then
        FOnItemChangedEvent(Self, FSubtitles.IndexOf(FSelectedSubtitle), FOldInitialTime, FOldFinalTime, FNeedToSortList);
      // Make sure we keep the list sorted internally
      FSubtitles.Sort;
      FNeedToSortList := False;
    end;

    if FDynamicEditMode = demNone then Cursor := crDefault;
    FSelectionX      := -1;
    FDynamicEditMode := demNone;
    FMouseIsDown     := False;
    FDynamicSelSub := NIL;
    FCursorMS        := PixelToTime(Trunc(X)) + FPositionMS;

    UpdateView([uvfSelection, uvfSubtitle]);
  end
  else if FDrawCursorTime then
  begin
    FDrawCursorTime := False;
    FCursorMS       := PixelToTime(Trunc(X)) + FPositionMS;
    UpdateView([uvfCursor]);
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.DblClick;
var
  idx      : Integer;
  Subtitle : PUWSubtitleItem;
begin
  inherited;

  if (FLengthMS = 0) then Exit;

  // Detect double click on start or stop timestamp
  if Assigned(FDynamicSelSub) then
  begin
    case FDynamicEditMode of
      demInitial : if Assigned(FOnSubtitleItemStartDblClick) then
                   begin
                     FOnSubtitleItemStartDblClick(Self, FDynamicSelSub^);
                     Exit;
                   end;
      demFinal   : if Assigned(FOnSubtitleItemStopDblClick) then
                   begin
                     FOnSubtitleItemStopDblClick(Self, FDynamicSelSub^);
                     Exit;
                   end;
    end;
  end
  else
  begin
    idx := FSubtitles.FindFirst(FCursorMS);
    if idx = -1 then
      Subtitle := NIL
    else
      Subtitle := FSubtitles.ItemPointer[idx];

    // Full subtitle selection
    SetSelectedSubtitleItem(Subtitle, False, False);
    UpdateView([uvfSelection, uvfSubtitle]);
    if Assigned(FOnSelectedSubtitleItem) then FOnSelectedSubtitleItem(Self, FSubtitles.IndexOf(FSelectedSubtitle), FSelectedSubtitle^, False);
  end;
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplay.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  ScrollAmount : Integer;
begin
  Result := True;

  ScrollAmount := Round(FPageSizeMS / 4); // scroll amount = 1/4 of visible interval
  if (ScrollAmount = 0) then ScrollAmount := 1;

  if WheelDelta > 0 then
  begin
    if ssShift in Shift then
      ZoomIn
    else if ssCtrl in Shift then
      VZoomLess
    else
      SetPositionMS(FPositionMs + ScrollAmount);
  end
  else
  begin
    if ssShift in Shift then
      ZoomOut
    else if ssCtrl in Shift then
      VZoomMore
    else
      SetPositionMS(FPositionMs - ScrollAmount);
  end;

  UpdateView([uvfPageSize]);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.Close;
begin
  FPeakDataLoaded        := False;
  FPeakTab               := NIL;
  FPeakTabSize           := 0;
  FSamplesPerPeak        := 0;

  FPageSizeMS            := 0;
  FPositionMs            := 0;
  FSelection.InitialTime := 0;
  FSelection.FinalTime   := 0;
  FDynamicEditMode       := demNone;
  FDynamicSelSub         := NIL;
  FDynamicEditTime       := 0;
  FCursorMS              := 0;
  FPlayCursorMS          := 0;
  FLengthMS              := 0;
  FSelectionX            := -1;
  FOldInitialTime        := -1;
  FOldFinalTime          := -1;
  FMouseIsDown           := False;
  FDrawCursorTime        := False;
  FNeedToSortList        := False;

  FillByte(FWaveFormat, SizeOf(FWaveFormat), 0);

  FScrollBar.PageSize := 0;
  FScrollBar.Position := 0;
  FScrollBar.Min      := 0;
  FScrollBar.Max      := 0;

  ClearSceneChange;
  UpdateView([uvfPageSize]);

  //if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
  //if Assigned(FOnViewChange)      then FOnViewChange(Self);
  //if Assigned(FOnCursorChange)    then FOnCursorChange(Self);

  Cursor := crDefault;
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplay.LoadWaveFromFile(const FileName: String): Boolean;
var
  PeakFileName    : String;
  PeakFS          : TFileStream;
  PeakFileIDRead  : String;
  PeakFileVerRead : Cardinal;
  WAVFile         : TUWWAVEFile;
  HDRSize         : Integer;
  CreatePeakFile  : Boolean;
  Normalized      : Boolean;
  LengthMS        : Integer;

const
  PeakFileID  : AnsiString = 'PeakFile';
  PeakFileVer : Cardinal   = $0100;

  procedure SavePeakFile;
  begin
    PeakFS := TFileStream.Create(PeakFileName, fmCreate);
    try
      with PeakFS do
      begin
        WriteBuffer(PeakFileID[1], System.Length(PeakFileID));
        WriteBuffer(PeakFileVer, SizeOf(PeakFileVer));
        WriteBuffer(LengthMS, SizeOf(FLengthMS));
        WriteBuffer(FWaveFormat.nSamplesPerSec, SizeOf(FWaveFormat.nSamplesPerSec));
        WriteBuffer(FWaveFormat.nChannels, SizeOf(FWaveFormat.nChannels));
        WriteBuffer(FWaveFormat.wBitsPerSample, SizeOf(FWaveFormat.wBitsPerSample));
        WriteBuffer(FSamplesPerPeak, SizeOf(FSamplesPerPeak));
        WriteBuffer(FPeakTabSize, SizeOf(FPeakTabSize));
        WriteBuffer(FPeakTab[0], FPeakTabSize*SizeOf(TUWPeak));
      end;
    finally
      PeakFS.Free;
    end;
  end;

begin
  Result := False;
  ClearPeakData;
  FPeakDataLoaded := False;
  CreatePeakFile  := True;

  // Search for a "peak" file with the same name
  PeakFilename := ChangeFileExt(FileName, '.peak');
  if FileExists(PeakFileName) then
  begin
    // Load peak file
    PeakFS := TFileStream.Create(PeakFileName, fmOpenRead or fmShareDenyWrite);
    try
      // Check filesize, we need at least
      HDRSize := System.Length(PeakFileID) + SizeOf(PeakFileVerRead) + SizeOf(FLengthMs) +
        SizeOf(FWaveFormat.nSamplesPerSec) + SizeOf(FWaveFormat.nChannels) +
        SizeOf(FWaveFormat.wBitsPerSample) + SizeOf(FSamplesPerPeak) +
        SizeOf(FPeakTabSize);

      if (PeakFS.Size > HDRSize) then
      begin
        SetLength(PeakFileIDRead, System.Length(PeakFileID));
        PeakFS.ReadBuffer(PeakFileIDRead[1], System.Length(PeakFileID));
        PeakFS.ReadBuffer(PeakFileVerRead, SizeOf(PeakFileVerRead));
        PeakFS.ReadBuffer(LengthMS, SizeOf(LengthMS));
        PeakFS.ReadBuffer(FWaveFormat.nSamplesPerSec, SizeOf(FWaveFormat.nSamplesPerSec));
        PeakFS.ReadBuffer(FWaveFormat.nChannels, SizeOf(FWaveFormat.nChannels));
        PeakFS.ReadBuffer(FWaveFormat.wBitsPerSample, SizeOf(FWaveFormat.wBitsPerSample));
        PeakFS.ReadBuffer(FSamplesPerPeak, SizeOf(FSamplesPerPeak));
        PeakFS.ReadBuffer(FPeakTabSize, SizeOf(FPeakTabSize));
        FPeakTab := NIL;
        SetLength(FPeakTab, FPeakTabSize);
        PeakFS.Read(FPeakTab[0], FPeakTabSize * SizeOf(TUWPeak));

        Normalized := NormalizePeakTab(-1);
        if Normalized then
        begin
          // rewrite normalized data
          SavePeakFile;
        end;

        CreatePeakFile := False;
      end;
    finally
      PeakFS.Free;
    end;
  end;

  if CreatePeakFile then
  begin
    // No wave file
    if not FileExists(FileName) then Exit;

    WAVFile := TUWWAVEFile.Create;
    if not WAVFile.Open(FileName) then
    begin
      WAVFile.Free;
      Exit;
    end;

    LengthMS    := WAVFile.Duration;
    FWaveFormat := WAVFile.GetWaveFormatEx^;
    // Create the "peak" file
    CreatePeakTab(WAVFile);
    // Save it
    if FSavePeakToFile then SavePeakFile;

    WAVFile.Close;
    WAVFile.Free;
  end;

  FPageSizeMS := 5000;
  SetLengthMS(LengthMS);
  FPeakDataLoaded := True;

  Result := True;
  UpdateView([uvfPageSize]);
end;

// -----------------------------------------------------------------------------

procedure TUWWaveformDisplay.ClearPeakData;
begin
  FPeakDataLoaded := False;
  FPeakTab        := NIL;
  FPeakTabSize    := 0;
  FSamplesPerPeak := 0;
  FillByte(FWaveFormat, SizeOf(FWaveFormat), 0);
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplay.IsPeakDataLoaded: Boolean;
begin
  Result := FPeakDataLoaded;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.CreatePeakTab(WAVFile: TUWWAVEFile);
var
  Buffer8                : array of Byte; //TByteDynArray;
  Buffer16               : array Of SmallInt; //TSmallIntDynArray;
  Buffer32               : array of Single; //TSingleDynArray;
  i, j                   : Integer;
  PeakMax, PeakMin       : SmallInt;
  PeakMax8, PeakMin8     : Byte;
  PeakMax32, PeakMin32   : Single;
  PeakMaxMax, PeakMinMin : SmallInt;
  MaxAbsoluteValue       : Integer;
  NormFactor             : Double;
begin
  if Assigned(FOnPeakCreation) then FOnPeakCreation(Self, pcetStart, 0);

  // Get 1 peak value every ~10ms
  FSamplesPerPeak := WAVFile.SamplesPerSecond div 100;
  FPeakTabSize    := Ceil((WAVFile.SamplesCount / FSamplesPerPeak) / WAVFile.Channels);
  // Allocate the big peak tab
  FPeakTab := NIL;
  SetLength(FPeakTab, FPeakTabSize);

// WAV Data format  Maximum value    Minimum value	    Midpoint value
//     8-bit PCM	  255 (0xFF)       0	                128 (0x80)
//     16-bit PCM	  32,767 (0x7FFF)  - 32,768 (0x8000)	0

  PeakMaxMax := -32768;
  PeakMinMin := 32767;

  if (WAVFile.BitsPerSample = 8) then
  begin
    // Allocate the small buffer
    SetLength(Buffer8, FSamplesPerPeak * WAVFile.Channels);
    for i := 0 to FPeakTabSize-1 do
    begin
      FillByte(Buffer8[0], FSamplesPerPeak * SizeOf(Shortint) * WAVFile.Channels, 0);
      WAVFile.Read(@Buffer8[0], FSamplesPerPeak * SizeOf(Shortint) * WAVFile.Channels);
      PeakMax8 := 0;
      PeakMin8 := 255;
      for j := 0 to (FSamplesPerPeak * WAVFile.Channels)-1 do
      begin
        if Buffer8[j] > PeakMax8 then
          PeakMax8 := Buffer8[j];
        if Buffer8[j] < PeakMin8 then
          PeakMin8 := Buffer8[j];
      end;
      // Convert 8 bits to 16 bits
      PeakMax := ((PeakMax8 - 128) shl 8);
      PeakMin := ((PeakMin8 - 128) shl 8);

      FPeakTab[i].Max := PeakMax;
      FPeakTab[i].Min := PeakMin;

      if (PeakMax > PeakMaxMax) then
        PeakMaxMax := PeakMax;
      if (PeakMin < PeakMinMin) then
        PeakMinMin := PeakMin;

      if Assigned(FOnPeakCreation) then
        FOnPeakCreation(Self, pcetProgress, (i*100) div Integer(FPeakTabSize));
    end;
  end
  else if (WAVFile.BitsPerSample = 16) then
  begin
    // Allocate the small buffer
    SetLength(Buffer16, FSamplesPerPeak * WAVFile.Channels);
    for i := 0 to FPeakTabSize-1 do
    begin
      FillByte(Buffer16[0], FSamplesPerPeak * SizeOf(SmallInt) * WAVFile.Channels, 0);
      WAVFile.Read(@Buffer16[0], FSamplesPerPeak * SizeOf(SmallInt) * WAVFile.Channels);
      PeakMax := -32768;
      PeakMin := 32767;
      for j := 0 to (FSamplesPerPeak * WAVFile.Channels)-1 do
      begin
        if Buffer16[j] > PeakMax then
          PeakMax := Buffer16[j];
        if Buffer16[j] < PeakMin then
          PeakMin := Buffer16[j];
      end;
      FPeakTab[i].Max := PeakMax;
      FPeakTab[i].Min := PeakMin;

      if PeakMax > PeakMaxMax then
        PeakMaxMax := PeakMax;
      if PeakMin < PeakMinMin then
        PeakMinMin := PeakMin;

      if Assigned(FOnPeakCreation) then
        FOnPeakCreation(Self, pcetProgress, (i*100) div Integer(FPeakTabSize));
    end;
  end
  else if (WAVFile.BitsPerSample = 32) then
  begin
    // Allocate the small buffer
    SetLength(Buffer32, FSamplesPerPeak * WAVFile.Channels);
    for i := 0 to FPeakTabSize-1 do
    begin
      FillByte(Buffer32[0], FSamplesPerPeak * SizeOf(SmallInt) * WAVFile.Channels, 0);
      WAVFile.Read(@Buffer32[0], FSamplesPerPeak * SizeOf(Single) * WAVFile.Channels);
      PeakMax32 := -1.0;
      PeakMin32 := 1.0;
      for j:=0 to (FSamplesPerPeak * WAVFile.Channels)-1 do
      begin
        if Buffer32[j] > PeakMax32 then
          PeakMax32 := Buffer32[j];
        if Buffer32[j] < PeakMin32 then
          PeakMin32 := Buffer32[j];
      end;
      // Convert 32 bits float to 16 bits integer
      PeakMax := Single2SmallInt(PeakMax32);
      PeakMin := Single2SmallInt(PeakMin32);

      FPeakTab[i].Max := PeakMax;
      FPeakTab[i].Min := PeakMin;

      if PeakMax > PeakMaxMax then
        PeakMaxMax := PeakMax;
      if PeakMin < PeakMinMin then
        PeakMinMin := PeakMin;

      if Assigned(FOnPeakCreation) then
        FOnPeakCreation(Self, pcetProgress, (i*100) div Integer(FPeakTabSize));
    end;
  end;
  // Calc. normalize factor
  MaxAbsoluteValue := Max(Abs(PeakMaxMax), Abs(PeakMinMin));
  NormFactor := 32768.0 / MaxAbsoluteValue;
  // Normalize peak tab
  NormalizePeakTab(NormFactor);
  Buffer8  := NIL;
  Buffer16 := NIL;
  if Assigned(FOnPeakCreation) then FOnPeakCreation(Self, pcetStop, 0);
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplay.NormalizePeakTab(NormFactor: Double): Boolean;
var
  i, MaxAbsoluteValue, Value : Integer;
begin
  Result := False;

  if (NormFactor = -1) then
  begin
    MaxAbsoluteValue := 0;
    // First pass, calculate normalization factor
    for i := 0 to FPeakTabSize-1 do
    begin
      Value := FPeakTab[i].Max;
      if (Value > MaxAbsoluteValue) then
        MaxAbsoluteValue := Value;

      Value := -FPeakTab[i].Min;
      if (Value > MaxAbsoluteValue) then
        MaxAbsoluteValue := Value;
    end;
    NormFactor := 32768.0 / MaxAbsoluteValue;
  end;

  if (NormFactor > 1.1) then
  begin
    // Apply normalization factor
    for i := 0 to FPeakTabSize-1 do
    begin
      FPeakTab[i].Max := Round(FPeakTab[i].Max * NormFactor);
      FPeakTab[i].Min := Round(FPeakTab[i].Min * NormFactor);
    end;

    Result := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.GenerateDummyPeakTab(const LengthMS: Cardinal);
begin
  FSamplesPerPeak := 1;
  FPeakTabSize    := 1;
  FPeakTab        := NIL;
  SetLength(FPeakTab, FPeakTabSize);
  FillByte(FPeakTab[0], 0, 0);

  SetLengthMS(LengthMS);
  FPeakDataLoaded := True;

  UpdateView([uvfPageSize]);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.SetLengthMS(const LenghtMS: Integer);
begin
  if FPeakDataLoaded then Exit;

  if (FLengthMS <> LenghtMS) then
  begin
    FLengthMS := LenghtMS;
    if (FPositionMS + FPageSizeMS > LenghtMS) then
    begin
      if (FPageSizeMS < LenghtMS) then
        FPositionMS := LenghtMS - FPageSizeMS
      else
      begin
        FPositionMS := 0;
        FPageSizeMS := LenghtMS;
      end;
    end;

    FScrollBar.Max      := LenghtMs;
    FScrollBar.PageSize := FPageSizeMs;
    FScrollBar.Position := FPositionMs;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.SetPositionMS(NewPosition: Integer);
begin
  Constrain(NewPosition, 0, FLengthMS - FPageSizeMS);
  if NewPosition <> FPositionMS then
  begin
    FPositionMS         := NewPosition;
    FScrollBar.Position := FPositionMS;
    UpdateView([uvfPageSize, uvfPosition]);
    if Assigned(FOnViewChange) then FOnViewChange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.OnScrollBarChange(Sender: TObject);
begin
  FPositionMS := GetPositionMS;
  //UpdateView([uvfPosition]);
  UpdateView([uvfPageSize]);
  if Assigned(FOnViewChange) then FOnViewChange(Self);
end;

//------------------------------------------------------------------------------

// Called only internally when the scroll bar change
function TUWWaveformDisplay.GetPositionMS: Integer;
begin
  if (FScrollBar.Position + FPageSizeMS - 1) > FLengthMS then
    Result := (FLengthMS - FPageSizeMS)
  else
    Result := FScrollBar.Position;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.UpdateView(const UpdateViewFlags: TUWUpdateViewFlags; const DoRepaint: Boolean = True);
begin
  FBackBufferWAVE.SetSize(Width, Height);
  FBackBuffer.SetSize(FBackBufferWAVE.Width, FBackBufferWAVE.Height);

  if (FLengthMS = 0) then
  begin
    FBackBufferWAVE.Canvas.Brush.Color := DISABLED_BACK_COLOR;
    FBackBufferWAVE.Canvas.FillRect(FBackBufferWAVE.Canvas.ClipRect);
    FBackBuffer.Canvas.Brush.Color := DISABLED_BACK_COLOR;
    FBackBuffer.Canvas.FillRect(FBackBuffer.Canvas.ClipRect);
    Repaint;
    Exit;
  end;

  if (uvfPageSize in UpdateViewFlags) then
  begin
    DrawWave(FBackBufferWAVE);
    DrawTimeLine(FBackBufferWAVE);
  end;

  FBackBuffer.Canvas.Lock;
  try
    FBackBuffer.Canvas.Draw(0, 0, FBackBufferWAVE);

    DrawItemsCanvas(FBackBuffer);
  finally
    FBackBuffer.Canvas.UnLock;
  end;

  if DoRepaint then Repaint;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.ZoomSubtitle(const Subtitle: TUWSubtitleItem);
var
  NewPosition, NewPageSize : Integer;
  UpdateFlags              : TUWUpdateViewFlags;
begin
  if (Subtitle.InitialTime >= Subtitle.FinalTime) then Exit;

  NewPageSize := Subtitle.FinalTime - Subtitle.InitialTime;
  if (NewPageSize < 100) then
  begin
    NewPageSize := 100; // Zoom to 100ms max
    NewPosition := Subtitle.InitialTime + ((Subtitle.FinalTime - Subtitle.InitialTime - NewPageSize) div 2);
  end
  else
  begin
    Constrain(NewPageSize, 0, FLengthMs);
    NewPosition := Subtitle.InitialTime;
  end;

  Constrain(NewPosition, 0, FLengthMs - NewPageSize);
  FScrollBar.SetPositionAndPageSize(NewPosition, NewPageSize);

  UpdateFlags := [];
  if (NewPosition <> FPositionMs) or (NewPageSize <> FPageSizeMs) then
  begin
    if(NewPageSize <> FPageSizeMs) then Include(UpdateFlags, uvfPageSize);
    FPageSizeMs := NewPageSize;
    if(NewPosition <> FPositionMs) then Include(UpdateFlags, uvfPosition);
    FPositionMs := NewPosition;
    UpdateView(UpdateFlags);
    if Assigned(FOnViewChange) then FOnViewChange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.ZoomSubtitle(const Start, Stop: Integer);
var
  Subtitle : TUWSubtitleItem;
begin
  Subtitle.InitialTime := Start;
  Subtitle.FinalTime  := Stop;
  ZoomSubtitle(Subtitle);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.ZoomAll;
begin
  ZoomSubtitle(0, FLengthMs);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.ZoomIn;
begin
  ZoomSubtitle(FPositionMS + Round(FPageSizeMS / 3), FPositionMS + Round((FPageSizeMS / 3) * 2));
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.ZoomOut;
begin
  ZoomSubtitle(FPositionMS - FPageSizeMS, FPositionMS + (FPageSizeMS * 2));
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.ZoomSelection;
begin
  ZoomSubtitle(FSelection.InitialTime, FSelection.FinalTime);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.VZoomRestore;
begin
  FVerticalScaling := 100;
  //UpdateView([uvfPageSize]);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.VZoomMore;
begin
  if InRange(FVerticalScaling, 5, 400) then Dec(FVerticalScaling, 5);
  //UpdateView([uvfPageSize]);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.VZoomLess;
begin
  if InRange(FVerticalScaling, 0, 395) then Inc(FVerticalScaling, 5);
  //UpdateView([uvfPageSize]);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.SelectSubtitle(const Index: Integer; const UpdateDisplay: Boolean; const UpdatePosition: Boolean);
var
  ASubtitle: PUWSubtitleItem;
begin
  if (FSubtitles.Count > 0) and InRange(Index, 0, FSubtitles.Count) then
    ASubtitle := FSubtitles.ItemPointer[Index]
  else
    Exit;

  // Full subtitle selection
  SetSelectedSubtitleItem(ASubtitle, UpdateDisplay, UpdatePosition);
  //UpdateView([uvfSelection, uvfSubtitle]);
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplay.GetSubtitleIdxAtCursorPos: Integer;
begin
  Result := FSubtitles.FindFirst(FCursorMS);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.DoUpdate(const Complete: Boolean = True);
begin
  if Complete then
    UpdateView([uvfPageSize])
  else
    UpdateView([uvfSubtitle]);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.SetSceneChangeList(const SceneChangeList: TIntegerDynArray);
begin
  SetLength(FSceneChangeList, System.Length(SceneChangeList));
  if (System.Length(FSceneChangeList) > 0) then
  begin
    System.Move(SceneChangeList[0], FSceneChangeList[0],
      System.Length(SceneChangeList) * SizeOf(Integer));
  end;
  UpdateView([uvfSubtitle]);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplay.ClearSceneChange;
begin
  SetLength(FSceneChangeList, 0);
  UpdateView([uvfSubtitle]);
end;

//------------------------------------------------------------------------------

procedure RegisterUWControlsWaveformDisplay;
begin
  RegisterComponents('URUWorks', [TUWWaveformDisplay]);
end;

procedure Register;
begin
  RegisterUnit('UWControls.WaveformDisplay', @RegisterUWControlsWaveformDisplay);
end;

// -----------------------------------------------------------------------------

end.
