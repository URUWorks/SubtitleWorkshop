{*
 *  URUWorks Lazarus Controls
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
 *
 *}

unit UWControls;

// -----------------------------------------------------------------------------

interface

uses
  Classes, Controls, SysUtils, LCLType, Graphics, ExtCtrls, Buttons, LMessages,
  StdCtrls, Math, LazarusPackageIntf, UWMediaEngine, UWMediaEngine.libMPV;

type

  { TUWSpeedButton }

  TUWSpeedButtonState = set of (tbFocusRect, tbAllowTimer);

  TUWSpeedButton = class(TCustomSpeedButton)
  private
    FRepeatTimer : TTimer;
    FButtonState : TUWSpeedButtonState;
    procedure TimerEvent(Sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ButtonState: TUWSpeedButtonState read FButtonState write FButtonState;
  end;

  { TUWSpinButton }

  TUWSpinButton = class(TWinControl)
  private
    FUpButton      : TUWSpeedButton;
    FDownButton    : TUWSpeedButton;
    FFocusedButton : TUWSpeedButton;
    FFocusControl  : TWinControl;
    FOnUpClick     : TNotifyEvent;
    FOnDownClick   : TNotifyEvent;
    function CreateButton: TUWSpeedButton;
    function GetUpGlyph: TBitmap;
    function GetDownGlyph: TBitmap;
    procedure SetUpGlyph(Value: TBitmap);
    procedure SetDownGlyph(Value: TBitmap);
    function GetUpNumGlyphs: TNumGlyphs;
    function GetDownNumGlyphs: TNumGlyphs;
    procedure SetUpNumGlyphs(Value: TNumGlyphs);
    procedure SetDownNumGlyphs(Value: TNumGlyphs);
    procedure BtnClick(Sender: TObject);
    procedure BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetFocusBtn(Btn: TUWSpeedButton);
    procedure AdjustSpinSize(const W, H: Integer);
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property DownGlyph     : TBitmap read GetDownGlyph write SetDownGlyph;
    property DownNumGlyphs : TNumGlyphs read GetDownNumGlyphs write SetDownNumGlyphs default 1;
    property FocusControl  : TWinControl read FFocusControl write FFocusControl;
    property UpGlyph       : TBitmap read GetUpGlyph write SetUpGlyph;
    property UpNumGlyphs   : TNumGlyphs read GetUpNumGlyphs write SetUpNumGlyphs default 1;
    property Enabled;
    property Visible;
    property OnDownClick   : TNotifyEvent read FOnDownClick write FOnDownClick;
    property OnUpClick     : TNotifyEvent read FOnUpClick   write FOnUpClick;
  end;

  { TUWTimeEdit }

  TUWTimeEditMode        = (temTime, temFrames);
  TUWTimeEditChangeEvent = procedure(Sender: TObject; const NewTime: Cardinal) of object;

  TUWTimeEdit = class(TCustomEdit)
  private
    FButton      : TUWSpinButton;
    FTimeMode    : TUWTimeEditMode;
    FValue       : Integer;
    FChangeEvent : TUWTimeEditChangeEvent;
    procedure SetTimeMode(const ATimeMode: TUWTimeEditMode);
    procedure SetValueFromString(const S: String);
    procedure SetValue(const NewValue: Integer);
    procedure UpdateValue(const FireChangeEvent: Boolean = True);
    procedure SetSel(const Start, Len: Integer);
    procedure DoTimeDown;
    procedure DoTimeUp;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function ChildClassAllowed(ChildClass: TClass): Boolean; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseUpClick(Sender: TObject); virtual;
    procedure MouseDownClick(Sender: TObject); virtual;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetValueOnly(const NewValue: Integer);
  published
    property Align;
    property Alignment;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ParentBidiMode;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    //property PopupMenu;
    //property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property TextHint;
    //property TextHintFontColor;
    //property TextHintFontStyle;
    property Visible;
    {Custom}
    property Value        : Integer                read FValue       write SetValue;
    property TimeMode     : TUWTimeEditMode        read FTimeMode    write SetTimeMode;
    property OnTimeChange : TUWTimeEditChangeEvent read FChangeEvent write FChangeEvent;
  end;

  { TUWTickTime }

  TUWTickTime = class(TGraphicControl)
  private
    FDuration : Int64;
    FBuffer   : TBitmap;
    FLoaded   : Boolean;
    procedure SetDuration(const Value: Int64);
    procedure DrawSubTick(const X: Integer);
    procedure DrawTick(const X: LongInt; const Time: String);
    procedure DrawTicks;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure Resize; override;
  published
    property Duration : Int64 read FDuration write SetDuration;
    property Align;
    property Anchors;
    property Color;
    property Font;
    property ParentColor;
    property PopupMenu;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { TUWSeekBar }

  TUWSeekBarPaintEvent = procedure(Sender: TObject; const ACanvas: TCanvas; const R: TRect) of object;

  TUWSeekBar = class(TGraphicControl)
  private
    FMax                 : Integer;
    FPosition            : Integer;
    FPoints              : array of TPoint;
    FBackRect            : TRect;
    FFillRect            : TRect;
    FButtonRect          : TRect;
    FButtonSize          : TPoint;
    FMouseIsDown         : Boolean;
    FOnPaintSeekBarEvent : TUWSeekBarPaintEvent;
    FOnChangeEvent       : TNotifyEvent;
    procedure CalculateBackRect;
    procedure CalculateFillAndButtonRect;
    procedure DrawBackground(const ACanvas: TCanvas);
    procedure DrawButton(const ACanvas: TCanvas);
    procedure CheckNewPosition(const X: Integer);
    procedure SetMax(const AMax: Integer);
    procedure SetPosition(const APosition: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  published
    property Align;
    property Anchors;
    property Cursor;
    property Enabled;
    property Width;
    property Height;
    property PopupMenu;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {custom}
    property Max         : Integer              read FMax                 write SetMax;
    property Position    : Integer              read FPosition            write SetPosition;
    property MouseIsDown : Boolean              read FMouseIsDown;
    property OnPaintBkg  : TUWSeekBarPaintEvent read FOnPaintSeekBarEvent write FOnPaintSeekBarEvent;
    property OnChange    : TNotifyEvent         read FOnChangeEvent       write FOnChangeEvent;
  end;

  { TUWScrollBar }

  TUWScrollBarThumb = class(TCustomPanel);

  TUWScrollBar = class(TCustomPanel)
  private
    FThumb            : TUWScrollBarThumb;
    FTracking         : Boolean;
    FTrackStartPointX : Integer;
    FMax, FMin        : Integer;
    FPageSize         : Integer;
    FPosition         : Integer;
    FThumbMinSize     : Integer;
    FOnChange         : TNotifyEvent;
    FColorNormal      : TColor;
    FColorDown        : TColor;
    FRepeatTimer      : TTimer;
    FLastXOnMouse     : Integer;
    FOnBeginScrolling : TNotifyEvent;
    FOnEndScrolling   : TNotifyEvent;
    procedure OnThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnRepeatTimer(Sender: TObject);
    procedure UpdateThumb;
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPageSize(Value: Integer);
    procedure SetPosition(Value: Integer);
  protected
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetPositionAndPageSize(NewPosition, NewPageSize: Integer);
  published
    property Max              : Integer      read FMax      write SetMax;
    property Min              : Integer      read FMin      write SetMin;
    property PageSize         : Integer      read FPageSize write SetPageSize;
    property Position         : Integer      read FPosition write SetPosition;
    property OnChange         : TNotifyEvent read FOnChange write FOnChange;
    property OnBeginScrolling : TNotifyEvent read FOnChange write FOnChange;
    property OnEndScrolling   : TNotifyEvent read FOnChange write FOnChange;
  end;

  { TUWLayout }

  TUWLayout = class(TCustomControl)
  public
    constructor Create(TheOwner: TComponent); override;
  protected
    procedure Paint; override;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
  end;

  { TUWCPSBar }

  TUWCPSBar = class(TGraphicControl)
  private
    FBuffer : TBitmap;
    FLoaded : Boolean;
    FValue  : Double;
    FMin    : Integer;
    FMax    : Integer;
    procedure DrawBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetValues(const Min, Max: Integer);
    procedure SetCPS(const CPS: Double);
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure Resize; override;
  published
    property Anchors;
    property PopupMenu;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { TUWNumberBox }

  TUWNumberBox = class(TCustomEdit)
  private
    FValue: Integer;
    FMinValue: Integer;
    FMaxValue: Integer;
    FChangeEvent: TNotifyEvent;
    procedure SetValue(const NewValue: Integer);
    procedure UpdateValue(const FireChangeEvent: Boolean = True);
    procedure DoValueDown;
    procedure DoValueUp;
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetValueOnly(const NewValue: Integer);
  published
    property Align;
    property Alignment;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ParentBidiMode;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    //property PopupMenu;
    //property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property TextHint;
    //property TextHintFontColor;
    //property TextHintFontStyle;
    property Visible;
    {Custom}
    property Value         : Integer      read FValue       write SetValue;
    property Max           : Integer      read FMaxValue    write FMaxValue;
    property Min           : Integer      read FMinValue    write FMinValue;
    property OnValueChange : TNotifyEvent read FChangeEvent write FChangeEvent;
  end;

  { TUWMediaPlayer }

  TUWMediaPlayer = class(TPanel)
  private
    FMPV: TUWLibMPV;
    FOnCommand: TUWMediaEngineOnCommand;
  protected
    procedure SetOnCommand(const AValue: TUWMediaEngineOnCommand);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Engine: TUWLibMPV read FMPV;
    property OnCommand: TUWMediaEngineOnCommand read FOnCommand write SetOnCommand;
    //
    property Align;
    property Anchors;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBackground;
    property ParentBidiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation
{$R UWControls.res}

uses UWSystem.TimeUtils, UWSystem.SysUtils;//, UWSystem.Graphics;

const

  { TUWSpeedButton }
  sbRepeatInterval = 100; // pause before repeat timer (ms)

  { TUWTimeEdit }
  OneSecond = 1000;
  OneMinute = OneSecond * 60;
  OneHour   = OneMinute * 60;
  MaxTime   = 86399999; // 23:59:59.999

  { TUWTickTime }
  OneSecond64  = Int64(10000000);
  OneMinute64  = 60 * OneSecond64;
  OneHour64    = 60 * OneMinute64;

  { TUWSeekBar }
  BACK_COLOR       = $D3D3D3; //clLtGray;
  FILL_COLOR       = $D5965E;
  BUTTON_COLOR     = $696969; //clDkGray;
  BUTTONDOWN_COLOR = $998877; //clMedGray;

  { TUWCPSBar }
  MaxCPS = 48;

// -----------------------------------------------------------------------------

{ TUWSpeedButton }

// -----------------------------------------------------------------------------

constructor TUWSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Flat := True;
end;

// -----------------------------------------------------------------------------

destructor TUWSpeedButton.Destroy;
begin
  if FRepeatTimer <> NIL then FRepeatTimer.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);

  if tbAllowTimer in FButtonState then
  begin
    if FRepeatTimer = NIL then FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer  := @TimerEvent;
    FRepeatTimer.Interval := sbRepeatInterval;
    FRepeatTimer.Enabled  := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);

  if FRepeatTimer <> NIL then FRepeatTimer.Enabled := False;
end;

// -----------------------------------------------------------------------------

procedure TUWSpeedButton.TimerEvent(Sender: TObject);
begin
  if (FState = bsDown) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

// -----------------------------------------------------------------------------

{ TUWSpinButton }

// -----------------------------------------------------------------------------

constructor TUWSpinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle   := ControlStyle - [csAcceptsControls, csSetCaption] + [csFramed, csOpaque];

  FUpButton      := CreateButton;
  FDownButton    := CreateButton;
  UpGlyph        := NIL;
  DownGlyph      := NIL;
  Width          := 20;
  Height         := 23;
  FFocusedButton := FUpButton;
end;

// -----------------------------------------------------------------------------

function TUWSpinButton.CreateButton: TUWSpeedButton;
begin
  Result             := TUWSpeedButton.Create(Self);
  Result.OnClick     := @BtnClick;
  Result.OnMouseDown := @BtnMouseDown;
  Result.Visible     := True;
  Result.Enabled     := True;
  Result.ButtonState := [tbAllowTimer];
  Result.Parent      := Self;
end;

// -----------------------------------------------------------------------------

procedure TUWSpinButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then FFocusControl := NIL;
end;

// -----------------------------------------------------------------------------

procedure TUWSpinButton.AdjustSpinSize(const W, H: Integer);
var
  iH: Integer;
begin
  if (FUpButton = NIL) or (FDownButton = NIL) or (csLoading in ComponentState) then Exit;

  iH := (H div 2)+1;
  FUpButton.SetBounds(0, 0, W, iH);
  FDownButton.SetBounds(0, iH-1, W, iH);
end;

// -----------------------------------------------------------------------------

procedure TUWSpinButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  AdjustSpinSize(AWidth, AHeight);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

// -----------------------------------------------------------------------------

procedure TUWSpinButton.WMSize(var Message: TLMSize);
begin
  inherited;

  AdjustSpinSize(Width, Height);
  Message.Result := 0;
end;

// -----------------------------------------------------------------------------

procedure TUWSpinButton.WMSetFocus(var Message: TLMSetFocus);
begin
  FFocusedButton.ButtonState := FFocusedButton.ButtonState + [tbFocusRect];
  FFocusedButton.Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TUWSpinButton.WMKillFocus(var Message: TLMKillFocus);
begin
  FFocusedButton.ButtonState := FFocusedButton.ButtonState - [tbFocusRect];
  FFocusedButton.Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TUWSpinButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP    : begin
                 SetFocusBtn(FUpButton);
                 FUpButton.Click;
               end;
    VK_DOWN  : begin
                 SetFocusBtn(FDownButton);
                 FDownButton.Click;
               end;
    VK_SPACE : FFocusedButton.Click;
  end;

  Abort;
end;

// -----------------------------------------------------------------------------

procedure TUWSpinButton.BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then SetFocusBtn(TUWSpeedButton(Sender));
end;

// -----------------------------------------------------------------------------

procedure TUWSpinButton.BtnClick(Sender: TObject);
begin
  if (Sender = FUpButton) and Assigned(FOnUpClick) then FOnUpClick(Self);
  if (Sender = FDownButton) and Assigned(FOnDownClick) then FOnDownClick(Self);
end;

// -----------------------------------------------------------------------------

procedure TUWSpinButton.SetFocusBtn(Btn: TUWSpeedButton);
begin
  if TabStop and CanFocus and (Btn <> FFocusedButton) then
  begin
    FFocusedButton.ButtonState := FFocusedButton.ButtonState - [tbFocusRect];
    FFocusedButton := Btn;
    FFocusedButton.ButtonState := FFocusedButton.ButtonState + [tbFocusRect];
    Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSpinButton.GetUpGlyph: TBitmap;
begin
  Result := FUpButton.Glyph;
end;

// -----------------------------------------------------------------------------

procedure TUWSpinButton.SetUpGlyph(Value: TBitmap);
var
  Png: TPortableNetworkGraphic;
begin
  if Value <> NIL then
    FUpButton.Glyph := Value
  else
  begin
    Png := TPortableNetworkGraphic.Create;
    try
      Png.LoadFromResourceName(HInstance, 'SpinUp');
      FUpButton.Glyph.Assign(Png);
    finally
      Png.Free;
    end;
    FUpButton.NumGlyphs := 1;
    FUpButton.Margin    := -1;
    FUpButton.Invalidate;
    FUpButton.Layout    := blGlyphTop;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSpinButton.GetUpNumGlyphs: TNumGlyphs;
begin
  Result := FUpButton.NumGlyphs;
end;

// -----------------------------------------------------------------------------

procedure TUWSpinButton.SetUpNumGlyphs(Value: TNumGlyphs);
begin
  FUpButton.NumGlyphs := Value;
end;

// -----------------------------------------------------------------------------

function TUWSpinButton.GetDownGlyph: TBitmap;
begin
  Result := FDownButton.Glyph;
end;

// -----------------------------------------------------------------------------

procedure TUWSpinButton.SetDownGlyph(Value: TBitmap);
var
  Png: TPortableNetworkGraphic;
begin
  if Value <> NIL then
    FDownButton.Glyph := Value
  else
  begin
    Png := TPortableNetworkGraphic.Create;
    try
      Png.LoadFromResourceName(HInstance, 'SpinDown');
      FDownButton.Glyph.Assign(Png);
    finally
      Png.Free;
    end;
    FDownButton.NumGlyphs := 1;
    FDownButton.Margin    := -1;
    FDownButton.Invalidate;
    FDownButton.Layout    := blGlyphBottom;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSpinButton.GetDownNumGlyphs: TNumGlyphs;
begin
  Result := FDownButton.NumGlyphs;
end;

// -----------------------------------------------------------------------------

procedure TUWSpinButton.SetDownNumGlyphs(Value: TNumGlyphs);
begin
  FDownButton.NumGlyphs := Value;
end;

// -----------------------------------------------------------------------------

{ TUWTimeEdit }

// -----------------------------------------------------------------------------

constructor TUWTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Constraints.MinWidth  := 90;
  Constraints.MinHeight := 21;
  Width     := 90;
  Height    := 23;
  FValue    := 0;
  FTimeMode := TUWTimeEditMode.temTime;
  Text      := '00:00:00.000';

  Anchors     := [akTop, akLeft];
  ChildSizing.SetGridSpacing(0);
  NumbersOnly := True;
  AutoSelect  := False;
  AutoSize    := False;

  FButton := TUWSpinButton.Create(Self);
  with FButton do
  begin
    Parent       := Self;
    Align        := alRight;
    Anchors      := [akTop, akBottom];
    Width        := 16;
    Height       := 20;
    FocusControl := Self;
    Visible      := True;
    OnUpClick    := @MouseUpClick;
    OnDownClick  := @MouseDownClick;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

// -----------------------------------------------------------------------------

function TUWTimeEdit.ChildClassAllowed(ChildClass: TClass): Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True; // Turn off default right mouse click popup
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.SetSel(const Start, Len: Integer);
begin
  SelStart  := Start;
  SelLength := Len;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FTimeMode = TUWTimeEditMode.temTime then
  begin
    // 01 34 67 901
    //   2  5  8
    // 00:00:00,000
    // if clicked on colon or comma, then fix cursor position
    case SelStart of
       0..1  : SetSel(0, 2);
       3..4  : SetSel(3, 2);
       6..7  : SetSel(6, 2);
       9..12 : SetSel(9, 3);
       2, 5  : SetSel(SelStart + 1, 2);
          8  : SetSel(SelStart + 1, 3);
    end;

    Abort;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.KeyDown(var Key: Word; Shift: TShiftState);
  procedure AbortNow;
  begin
    Key := VK_UNKNOWN;
    Abort;
  end;

const
  MINMASK = '00:00:00.000';
  MAXMASK = '23:59:59.999';

var
  s       : String;
  SelSt   : Integer;
  KeyChar : Char;
begin
  if Key = VK_TAB then AbortNow;
  KeyChar := Chr(Key);

  case Key of
    VK_LEFT    : if (FTimeMode = TUWTimeEditMode.temTime) and (SelStart > 0) then
                 begin
                   if SelStart in [3, 6, 9] then
                      SelStart := SelStart - 3
                   else if SelStart in [1, 4, 7, 10] then
                     SelStart := SelStart - 1
                   else if SelStart in [11] then
                     SelStart := SelStart - 2
                   else
                     SelStart := 0;

                   if SelStart = 9 then
                     SelLength := 3
                   else
                     SelLength := 2;
                 end;
    VK_RIGHT   : if (FTimeMode = TUWTimeEditMode.temTime) and (SelStart < 11) then
                 begin
                   if SelStart in [0, 3, 6] then
                     SelStart := SelStart + 3
                   else if SelStart in [1, 4, 7] then
                     SelStart := SelStart + 2
                   else if SelStart in [10, 11] then
                     SelStart := 9;

                   if SelStart = 9 then
                     SelLength := 3
                   else
                     SelLength := 2;
                 end;
     VK_UP     : DoTimeUp;
     VK_DOWN   : DoTimeDown;
  else
     begin
       if (FTimeMode = TUWTimeEditMode.temTime) then
       begin
         SelSt := SelStart;
         s     := Text;

         if not CharInSet(KeyChar, [#48..#57]) then AbortNow;  // 0 - 9

         if KeyChar > MAXMASK[SelSt+1] then
           s[SelSt+1] := MAXMASK[SelSt+1]
         else if KeyChar < MINMASK[SelSt+1] then
           s[SelSt+1] := MINMASK[SelSt+1]
         else
           s[SelSt+1] := KeyChar;

         SetValueFromString(S);

         // put cursor to next position
         case SelSt of
            1, 4, 7 : SelStart := SelSt + 2;
           0,2,3,5,
           6,8,9,10 : SelStart := SelSt + 1;
           else
             SelStart := 0;
         end;

         SelLength := 1;
       end
       else
       begin
         if not CharInSet(KeyChar, [#48..#57]) then AbortNow; // 0 - 9
         FValue := StrToInt(Text);
       end;
     end;
  end;

  if Assigned(FChangeEvent) then FChangeEvent(Self, FValue);
  if FTimeMode = TUWTimeEditMode.temTime then AbortNow;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.MouseUpClick(Sender: TObject);
begin
  DoTimeUp;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.MouseDownClick(Sender: TObject);
begin
  DoTimeDown;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.DoTimeDown;

  procedure DecHour;
  begin
    if FValue >= OneHour then
    begin
      Value := FValue - OneHour;
      SetSel(0, 2);
    end
    else
      SetSel(3, 2);
  end;

  procedure DecMin;
  begin
    if FValue >= OneMinute then
    begin
      Value := FValue - OneMinute;
      SetSel(3, 2);
    end
    else
      SetSel(6, 2);
  end;

  procedure DecSec;
  begin
    if FValue >= OneSecond then
    begin
      Value := FValue - OneSecond;
      SetSel(6, 2);
    end
    else
      SetSel(9, 3);
  end;

  procedure DecMSec;
  begin
    if FValue > 10 then
      Value := FValue - 10
    else
      Value := 0;

    SetSel(9, 3);
  end;

begin
  if FValue > 0 then
  begin
    if FTimeMode = TUWTimeEditMode.temFrames then
      Value := FValue - 1
    else
    begin
      if SelLength <= 1 then SetSel(9, 3);

      case SelStart of
         0..1  : DecHour;
         3..4  : DecMin;
         6..7  : DecSec;
         9..11 : DecMSec;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.DoTimeUp;
begin
  if FTimeMode = TUWTimeEditMode.temFrames then
    Value := FValue + 1
  else
  begin
    if SelLength <= 1 then SetSel(9, 3);

    case SelStart of
       0..1  : begin
                 Value := FValue + OneHour;
                 SetSel(0, 2);
               end;
       3..4  : begin
                 Value := FValue + OneMinute;
                 SetSel(3, 2);
               end;
       6..7  : begin
                 Value := FValue + OneSecond;
                 SetSel(6, 2);
               end;
       9..11 : begin
                 Value := FValue + 10;
                 SetSel(9, 3);
               end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.SetValue(const NewValue: Integer);
begin
  if FValue <> NewValue then
  begin
    FValue := NewValue;
    Constrain(FValue, 0, MaxTime);
    UpdateValue;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.SetValueOnly(const NewValue: Integer);
begin
  if FValue <> NewValue then
  begin
    FValue := NewValue;
    Constrain(FValue, 0, MaxTime);
    UpdateValue(False);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.UpdateValue(const FireChangeEvent: Boolean = True);
begin
  if FTimeMode = TUWTimeEditMode.temFrames then
    Text := IntToStr(FValue)
  else
    Text := TimeToString(FValue, 'hh:mm:ss.zzz');

  if FireChangeEvent and Assigned(FChangeEvent) then FChangeEvent(Self, FValue);
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.SetTimeMode(const ATimeMode: TUWTimeEditMode);
begin
  FTimeMode := ATimeMode;
  UpdateValue;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.SetValueFromString(const S: String);
begin
  FValue := StringToTime(S);
  if FTimeMode = TUWTimeEditMode.temTime then
    Text := S
  else
    Text := IntToStr(FValue);
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.CMEnabledChanged(var Message: TLMessage);
begin
  inherited;
end;

// -----------------------------------------------------------------------------

function TUWTimeEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  if WheelDelta > 0 then
    DoTimeUp
  else
    DoTimeDown;

  Result := True;
end;

// -----------------------------------------------------------------------------

{ TUWTickTime }

// -----------------------------------------------------------------------------

constructor TUWTickTime.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];
  Color        := clBtnFace;
  Cursor       := crArrow;
  FDuration    := 1;
  FLoaded      := False;

  FBuffer := TBitmap.Create;
  FBuffer.PixelFormat := pf32bit;
  FBuffer.Canvas.Brush.Color := clDefault;
  with FBuffer.Canvas.Font do
  begin
    Name  := 'Verdana';
    Size  := 5;
    Color := clBlack;
  end;
end;

// -----------------------------------------------------------------------------

destructor TUWTickTime.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWTickTime.Loaded;
begin
  inherited Loaded;
  FLoaded := True;
end;

// -----------------------------------------------------------------------------

procedure TUWTickTime.Paint;
begin
  if FLoaded and Assigned(FBuffer) then
    Canvas.Draw(0, 0, FBuffer);
end;

// -----------------------------------------------------------------------------

procedure TUWTickTime.Resize;
begin
  inherited;

  if FLoaded and ([csLoading, csDestroying]*ComponentState = []) then
    DrawTicks;
end;

// -----------------------------------------------------------------------------

procedure TUWTickTime.SetDuration(const Value: Int64);
begin
  FDuration := Value;

  DrawTicks;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TUWTickTime.DrawSubTick(const X: Integer);
begin
  FBuffer.Canvas.Line(X, Height-1, X, Height);
end;

// -----------------------------------------------------------------------------

procedure TUWTickTime.DrawTick(const X: LongInt; const Time: String);
var
  r: TRect;
begin
  r := Rect(X - ((Length(Time)*2)-1), ClientRect.Top, ClientRect.Right, ClientRect.Bottom);
//  r := Rect(X - 13, ClientRect.Top, ClientRect.Right, ClientRect.Bottom);
  FBuffer.Canvas.Line(X, Height-5, X, Height);
  FBuffer.Canvas.Textout(r.Left, r.Top, Time);
end;

// -----------------------------------------------------------------------------

procedure TUWTickTime.DrawTicks;
const
  _Margin = 14;
  _Width  = 24;

var
  BarSize, Ticks,
  i, j : LongInt;
  TickTime, TickPos,
  SubTickPos : Int64;
begin
  FBuffer.SetSize(Width, Height);
  with FBuffer.Canvas do
  begin
    Brush.Style := bsSolid;
    if ParentColor then
      Brush.Color := TCustomControl(Parent).Color
    else
      Brush.Color := Color;
    FillRect(Rect(0,0, Width, Height));
    Brush.Style := bsClear;
  end;

  if FDuration < 1 then Exit;
  DrawTick(Width - _Margin, '');

  BarSize := Width - 2 * _Margin;
  Ticks   := (BarSize div (_Width + 10)) - 1;
  if (Ticks > 0) then
  begin
    TickTime := FDuration div Ticks;
    if (TickTime < 1*OneSecond64)                                       then TickTime := 1*OneSecond64
    else if (TickTime > 1*OneSecond64)  and (TickTime < 5*OneSecond64)  then TickTime := 5*OneSecond64
    else if (TickTime > 5*OneSecond64)  and (TickTime < 10*OneSecond64) then TickTime := 10*OneSecond64
    else if (TickTime > 10*OneSecond64) and (TickTime < 15*OneSecond64) then TickTime := 15*OneSecond64
    else if (TickTime > 15*OneSecond64) and (TickTime < 30*OneSecond64) then TickTime := 30*OneSecond64
    else if (TickTime > 30*OneSecond64) and (TickTime < 1*OneMinute64)  then TickTime := 1*OneMinute64
    else if (TickTime > 1*OneMinute64)  and (TickTime < 5*OneMinute64)  then TickTime := 5*OneMinute64
    else if (TickTime > 5*OneMinute64)  and (TickTime < 10*OneMinute64) then TickTime := 10*OneMinute64
    else if (TickTime > 10*OneMinute64) and (TickTime < 15*OneMinute64) then TickTime := 15*OneMinute64
    else if (TickTime > 15*OneMinute64) and (TickTime < 30*OneMinute64) then TickTime := 30*OneMinute64
    else if (TickTime > 30*OneMinute64) and (TickTime < 1*OneHour64)    then TickTime := 1*OneHour64
    else if (TickTime > 1*OneHour64)    and (TickTime < 2*OneHour64)    then TickTime := 2*OneHour64
    else if (TickTime > 2*OneHour64)    and (TickTime < 5*OneHour64)    then TickTime := 5*OneHour64;
  end
  else
    TickTime := FDuration;

  Ticks := (FDuration div TickTime);
  for i := 0 to Ticks do
  begin
    TickPos := _Margin+(i * BarSize * TickTime) div FDuration;

    for j := 1 to 4 do
    begin
      SubTickPos := i * TickTime + (TickTime * j div 5);
      if (SubTickPos < FDuration) then
      begin
        SubTickPos := _Margin+(BarSize * SubTickPos) div FDuration;
        DrawSubTick(SubTickPos);
      end;
    end;
    DrawTick(TickPos, TimeToString(RefTimeToMSecs(i * TickTime), 'h:mm:ss', 25, True));
  end;
end;

// -----------------------------------------------------------------------------

{ TUWSeekBar }

// -----------------------------------------------------------------------------

constructor TUWSeekBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMax          := 1000;
  FPosition     := 0;
  FButtonSize.X := 8;
  FButtonSize.Y := 18;
  SetLength(FPoints, 6);
  FMouseIsDown  := False;

  Constraints.MinWidth  := 10;
  Constraints.MinHeight := 8;
  Constraints.MaxHeight := 60;
end;

// -----------------------------------------------------------------------------

destructor TUWSeekBar.Destroy;
begin
  Finalize(FPoints);

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TUWSeekBar.Paint;
begin
  Canvas.Lock;
  try
    Canvas.Brush.Color := TCustomControl(Parent).Color;
    Canvas.FillRect(ClientRect);

    DrawBackground(Canvas);
    DrawButton(Canvas);
  finally
    Canvas.Unlock;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWSeekBar.Resize;
begin
  if [csLoading, csDestroying]*ComponentState = [] then
  begin
    CalculateBackRect;
    CalculateFillAndButtonRect;
  end;

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TUWSeekBar.CalculateBackRect;
begin
  FBackRect        := ClientRect;
  FBackRect.Left   := FButtonSize.X div 2;
  FBackRect.Right  := Width - FBackRect.Left;
  FBackRect.Top    := 0;
  FBackRect.Bottom := (Height div 2) + 1;
end;

// -----------------------------------------------------------------------------

procedure TUWSeekBar.CalculateFillAndButtonRect;
var
  Half  : Integer;
  HalfH : Integer;
begin
  FFillRect   := FBackRect;
  FButtonRect := FBackRect;

  if FMax <> 0 then
    FFillRect.Left := Trunc((FPosition * 100 / FMax) * (Width-FButtonSize.X) / 100) //FFillRect.Left := (FPosition * 100 div FMax) * (Width-FButtonSize.X) div 100
  else
    FFillRect.Left := 0;

  FFillRect.Right    := FFillRect.Left + FButtonSize.X div 2;
  FButtonRect.Left   := FFillRect.Left;
  FFillRect.Left     := FButtonSize.X div 2;

  FButtonRect.Top    := Height div 4;

  FButtonRect.Right  := FButtonRect.Left + FButtonSize.X;
  FButtonRect.Bottom := FButtonRect.Top  + FButtonSize.Y;
  Constrain(FButtonRect.Bottom, FButtonRect.Top, Height-1);

  Half  := FButtonRect.Left + (FButtonRect.Right-FButtonRect.Left) div 2;
  HalfH := (FButtonRect.Bottom div 2) + 1;
  FPoints[0] := Point(FButtonRect.Left, FButtonRect.Bottom);
  FPoints[1] := Point(FButtonRect.Left, HalfH);
  FPoints[2] := Point(half, FButtonRect.Top);
  FPoints[3] := Point(FButtonRect.Right, HalfH);
  FPoints[4] := Point(FButtonRect.Right, FButtonRect.Bottom);
  FPoints[5] := Point(FButtonRect.Left, FButtonRect.Bottom);
end;

// -----------------------------------------------------------------------------

procedure TUWSeekBar.DrawBackground(const ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := BACK_COLOR;
  ACanvas.FillRect(FBackRect);

  if Assigned(FOnPaintSeekBarEvent) then FOnPaintSeekBarEvent(Self, ACanvas, FBackRect);
end;

//------------------------------------------------------------------------------

procedure TUWSeekBar.DrawButton(const ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := FILL_COLOR;
  ACanvas.FillRect(FFillRect);

  if FMouseIsDown then
    ACanvas.Brush.Color := BUTTONDOWN_COLOR
  else
    ACanvas.Brush.Color := BUTTON_COLOR;

  ACanvas.Pen.Color := ACanvas.Brush.Color;
  ACanvas.Polygon(FPoints);
end;

//------------------------------------------------------------------------------

procedure TUWSeekBar.CheckNewPosition(const X: Integer);
begin
  FPosition := ((X - (FButtonSize.X div 2)) * FMax) div (Width-(FButtonSize.X));
  if FPosition < 0 then FPosition := 0;
  if FPosition > (FMax) then FPosition := FMax;
  CalculateFillAndButtonRect;
  Repaint;
end;

//------------------------------------------------------------------------------

procedure TUWSeekBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then
  begin
    FMouseIsDown := True;
    CheckNewPosition(X);
  end;
end;

//------------------------------------------------------------------------------

procedure TUWSeekBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FMouseIsDown then CheckNewPosition(X);
end;

//------------------------------------------------------------------------------

procedure TUWSeekBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FMouseIsDown then
  begin
    FMouseIsDown := False;
    CheckNewPosition(X);

    if Assigned(FOnChangeEvent) then FOnChangeEvent(Self);
    inherited OnMouseUp(Self, Button, Shift, X, Y);
  end;
end;

//------------------------------------------------------------------------------

procedure TUWSeekBar.SetMax(const AMax: Integer);
begin
  if FMax <> AMax then
  begin
    FMax := AMax;
    CalculateFillAndButtonRect;
    Repaint;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWSeekBar.SetPosition(const APosition: Integer);
begin
  if not FMouseIsDown and (FPosition <> APosition) then
  begin
    FPosition := APosition;
    Constrain(FPosition, 0, FMax);
    CalculateFillAndButtonRect;
    Repaint;

    if Assigned(FOnChangeEvent) then FOnChangeEvent(Self);
  end;
end;

// -----------------------------------------------------------------------------

{ TUWScrollBar }

// -----------------------------------------------------------------------------

constructor TUWScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMax                  := 100;
  FMin                  := 0;
  FPosition             := 10;
  FPageSize             := 10;
  FThumbMinSize         := 5;
  FColorNormal          := $00A56E3A;
  FColorDown            := $00D5965E;
  Self.Color            := clGray;
  Self.BevelOuter       := bvNone;

  FThumb                := TUWScrollBarThumb.Create(Self);
  FThumb.Parent         := Self;
  FThumb.Color          := FColorNormal;
  FThumb.BevelOuter     := bvNone;
  FThumb.OnMouseDown    := @OnThumbMouseDown;
  FThumb.OnMouseMove    := @OnThumbMouseMove;
  FThumb.OnMouseUp      := @OnThumbMouseUp;

  FRepeatTimer          := TTimer.Create(Self);
  FRepeatTimer.Enabled  := False;
  FRepeatTimer.Interval := 150;
  FRepeatTimer.OnTimer  := @OnRepeatTimer;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.WMSize(var Message: TLMSize);
begin
  inherited;
  FThumb.Height := Height;
  UpdateThumb;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.OnRepeatTimer(Sender: TObject);
begin
  // Are we on the thumb ?
  if (FLastXOnMouse >= FThumb.Left) and
     (FLastXOnMouse <= FThumb.Left+FThumb.Width) then
  begin
    FRepeatTimer.Enabled := False;
    Exit;
  end;

  if FLastXOnMouse > FThumb.Left then
    Position := Position + PageSize
  else if FLastXOnMouse < FThumb.Left then
    Position := Position - PageSize;

  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (FMax = FMin) and (FMax = 0) then  Exit;

  if Assigned(FOnBeginScrolling) then FOnBeginScrolling(Self);

  if X > FThumb.Left then
    Position := Position + PageSize
  else if X < FThumb.Left then
    Position := Position - PageSize;

  if Assigned(FOnChange) then FOnChange(Self);

  FLastXOnMouse        := X;
  FRepeatTimer.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FLastXOnMouse := X;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FRepeatTimer.Enabled and Assigned(FOnEndScrolling) then FOnEndScrolling(Self);
  FRepeatTimer.Enabled := False;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.OnThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (FMax = FMin) and (FMax = 0) then  Exit;

  if ssLeft in Shift then
  begin
    FTracking           := True;
    FTrackStartPointX   := X;
    FThumb.Color        := FColorDown;
    FThumb.MouseCapture := True;
    if Assigned(FOnBeginScrolling) then FOnBeginScrolling(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  NewX: Integer;
begin
  if (FMax = FMin) and (FMax = 0) then Exit;

  if FTracking then
  begin
    NewX := X - FTrackStartPointX + FThumb.Left;
    Constrain(NewX, 0, Width - FThumb.Width);

    if FThumb.Left <> NewX then
    begin
      FThumb.Left  := NewX;
      FThumb.Color := FColorDown;
      // Update FPosition
      FPosition := MulDiv(NewX, (FMax - FMin + 1)-Math.Max(1,FPageSize),
        Width - FThumb.Width)+FMin;

      //Assert(FPosition >= FMin);
      //Assert(FPosition <= FMax);

      Constrain(FPosition, FMin, FMax - Math.Max(FPageSize-1,0));
      if Assigned(FOnChange) then FOnChange(Self);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.OnThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (FMax = FMin) and (FMax = 0) then Exit;

  FThumb.MouseCapture := False;
  FTracking           := False;
  FThumb.Color        := clBlue;
  FThumb.Color        := FColorNormal;

  if Assigned(FOnEndScrolling) then FOnEndScrolling(Self);
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.SetMax(Value: Integer);
begin
  if Value <> FMax then
  begin
    FMax           := Value;
    FThumb.Visible := (FMax <> FMin);
    UpdateThumb;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.SetMin(Value: Integer);
begin
  if Value <> FMin then
  begin
    FMin           := Value;
    FThumb.Visible := (FMax <> FMin);
    UpdateThumb;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.SetPageSize(Value: Integer);
begin
  Constrain(Value, 0, FMax - FMin + 1);
  if Value <> FPageSize then
  begin
    FPageSize := Value;
    UpdateThumb;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.SetPosition(Value: Integer);
begin
  Constrain(Value, FMin, FMax - Math.Max(FPageSize-1, 0));
  if Value <> FPosition then
  begin
    FPosition := Value;
    UpdateThumb;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.SetPositionAndPageSize(NewPosition, NewPageSize: Integer);
begin
  Constrain(NewPageSize, 0, FMax - FMin + 1);
  Constrain(NewPosition, FMin, FMax - Math.Max(NewPageSize-1, 0));
  if (NewPageSize <> FPageSize) or (NewPosition <> FPosition) then
  begin
    FPageSize := NewPageSize;
    FPosition := NewPosition;
    UpdateThumb;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.UpdateThumb;
var
  NewThumbWidth, NewPosition, MaxMin : Integer;
begin
  MaxMin := (FMax - FMin) + 1;
  if FPageSize = 0 then
    NewThumbWidth := FThumbMinSize
  else
  begin
    NewThumbWidth := MulDiv(FPageSize, Width, MaxMin);
    if NewThumbWidth < FThumbMinSize then NewThumbWidth := FThumbMinSize;
  end;

  NewPosition := MulDiv(FPosition - FMin, Width - NewThumbWidth, MaxMin - Math.Max(1, FPageSize));
  Constrain(NewPosition, 0, Width - NewThumbWidth);

  FThumb.SetBounds(NewPosition, FThumb.Top, NewThumbWidth, FThumb.Height);
end;

//------------------------------------------------------------------------------

{ TUWLayout }

//------------------------------------------------------------------------------

constructor TUWLayout.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FCompStyle   := csPanel;
  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
    csClickEvents, csSetCaption, csDoubleClicks, csReplicatable,
    csNoFocus, csAutoSize0x0, csParentBackground]
    - [csOpaque]; // we need the default background
  //ControlStyle := ControlStyle + [csNoFocus, csAcceptsControls] - [csOpaque];
  //Color := {$ifdef UseCLDefault}clDefault{$else}clBtnFace{$endif};

  UseDockManager := True;
  DoubleBuffered := True;
end;

//------------------------------------------------------------------------------

procedure TUWLayout.Paint;
begin
  if (csDesigning in ComponentState) then
    Canvas.DrawFocusRect(Rect(0, 0, Width, Height));

  inherited Paint;
end;

// -----------------------------------------------------------------------------

{ TUWCPSBar }

// -----------------------------------------------------------------------------

constructor TUWCPSBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];
  Color        := clDefault;
  Cursor       := crArrow;
  FLoaded      := False;

  FValue := 0;
  FMin   := 13; // Min acceptable value
  FMax   := 27; // Max acceptable value

  FBuffer := TBitmap.Create;
  FBuffer.PixelFormat := pf32bit;
  FBuffer.Canvas.Brush.Color := clDefault;
end;

// -----------------------------------------------------------------------------

destructor TUWCPSBar.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWCPSBar.Loaded;
begin
  inherited Loaded;
  FLoaded := True;
end;

// -----------------------------------------------------------------------------

procedure TUWCPSBar.Paint;
begin
  if (csDesigning in ComponentState) then
    Canvas.DrawFocusRect(Rect(0, 0, Width, Height));
//  if FLoaded and Assigned(FBuffer) then
  if Visible then
    Canvas.Draw(0, 0, FBuffer);
end;

// -----------------------------------------------------------------------------

procedure TUWCPSBar.Resize;
begin
  inherited;

  if FLoaded and ([csLoading, csDestroying]*ComponentState = []) then
    DrawBuffer;
end;

// -----------------------------------------------------------------------------

procedure TUWCPSBar.SetValues(const Min, Max: Integer);
begin
  FMin := Min;
  FMax := Max;
  DrawBuffer;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TUWCPSBar.SetCPS(const CPS: Double);
begin
  FValue := CPS;
  DrawBuffer;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TUWCPSBar.DrawBuffer;
var
  iSize     : Integer;
  lG, //lR, lB,
  lP, iC    : Integer;
begin
  FBuffer.SetSize(Width, Height);
  with FBuffer.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBtnFace; //Parent.Color; //clBlack;
    FillRect(Rect(0, 0, Width, Height));

    iSize := Width div (MaxCPS-1);

    {lR := iSize * FMin;
    Brush.Color := clRed;
    FillRect(Rect(0, 0, lR, Height));
    //GradientFill(Rect(0, 0, lR, Height), clRed, clLime, gdHorizontal);
    //FillGradient(FBuffer.Canvas, Rect(0, 0, lR, Height), clRed, clLime, 3);

    lG := iSize * FMax;
    Brush.Color := clLime;
    FillRect(Rect(lR, 0, lG, Height));

    lB := Width; //iSize * FMax;
    Brush.Color := $FF5000;
    FillRect(Rect(lG, 0, lB, Height));
    //GradientFill(Rect(lG, 0, lB, Height), clLime, clBlue, gdHorizontal);
    //FillGradient(FBuffer.Canvas, Rect(lG, 0, lB, Height), clLime, clBlue, 3);
    }

    if FValue < 1 then
      lP := 1
    else if FValue > MaxCPS then
      lP := Width
    else
      lP := Round(iSize * FValue);

    //lR := iSize * FMin;
    lG := iSize * FMax;
    //lB := Width;

    {if lP <= lR then
      iC := clRed
    else if lP <= lG then
      iC := clGreen
    else
      iC := $FF5000;}
    if lP <= lG then
      iC := clGreen
    else
      iC := clRed;

    // line cps
    //iSize := (Height div 2) + 1;
    Brush.Color := iC; //clBlack;
    FillRect(Rect(0, 0, lP, Height));

    // border
    Brush.Style := bsClear;
    Pen.Style := psSolid;
    Pen.Color := clLtGray; //clGrayText;
    Rectangle(Rect(0, 0, Width, Height));
    // max start line
    Line(Rect(lG, 0, lG, Height));
  end;
end;

// -----------------------------------------------------------------------------

{ TUWNumberBox }

// -----------------------------------------------------------------------------

constructor TUWNumberBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Anchors     := [akTop, akLeft];
  NumbersOnly := True;
  AutoSelect  := False;
  AutoSize    := False;
  Text        := '0';
  FValue      := 0;
  FMinValue   := 0;
  FMaxValue   := 100;
end;

// -----------------------------------------------------------------------------

procedure TUWNumberBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  case Key of
     VK_UP   : DoValueUp;
     VK_DOWN : DoValueDown;
  else
    FValue := StrToIntDef(Text, FMinValue);
    if FValue > FMaxValue then
    begin
      Key := 0;
      Constrain(FValue, FMinValue, FMaxValue);
      UpdateValue;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWNumberBox.DoValueDown;
begin
  Dec(FValue);
  Constrain(FValue, FMinValue, FMaxValue);
  UpdateValue;
end;

// -----------------------------------------------------------------------------

procedure TUWNumberBox.DoValueUp;
begin
  Inc(FValue);
  Constrain(FValue, FMinValue, FMaxValue);
  UpdateValue;
end;

// -----------------------------------------------------------------------------

procedure TUWNumberBox.SetValue(const NewValue: Integer);
begin
  if FValue <> NewValue then
  begin
    FValue := NewValue;
    Constrain(FValue, FMinValue, FMaxValue);
    UpdateValue;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWNumberBox.SetValueOnly(const NewValue: Integer);
begin
  if FValue <> NewValue then
  begin
    FValue := NewValue;
    Constrain(FValue, FMinValue, FMaxValue);
    UpdateValue(False);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWNumberBox.UpdateValue(const FireChangeEvent: Boolean = True);
begin
  Text := IntToStr(FValue);
  if FireChangeEvent and Assigned(FChangeEvent) then FChangeEvent(Self);
end;

// -----------------------------------------------------------------------------

function TUWNumberBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  if WheelDelta > 0 then
    DoValueUp
  else
    DoValueDown;

  Result := True;
end;

// -----------------------------------------------------------------------------

{ TUWMediaPlayer }

// -----------------------------------------------------------------------------

constructor TUWMediaPlayer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption := '';
  ParentBackground := False;
  Color       := clBlack;
  BevelInner  := bvNone;
  BevelOuter  := bvNone;
  BorderStyle := bsNone;

  FMPV := TUWLibMPV.Create(Self, FOnCommand);
end;

// -----------------------------------------------------------------------------

destructor TUWMediaPlayer.Destroy;
begin
  FMPV.Free;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWMediaPlayer.SetOnCommand(const AValue: TUWMediaEngineOnCommand);
begin
  FOnCommand     := AValue;
  FMPV.OnCommand := FOnCommand;
end;

// -----------------------------------------------------------------------------

procedure RegisterUWControlsUnit;
begin
  RegisterComponents('URUWorks', [TUWTimeEdit, TUWTickTime, TUWSeekBar,
    TUWLayout, TUWCPSBar, TUWNumberBox, TUWMediaPlayer]);
end;

procedure Register;
begin
  RegisterUnit('UWControls', @RegisterUWControlsUnit);
end;

// -----------------------------------------------------------------------------

end.
