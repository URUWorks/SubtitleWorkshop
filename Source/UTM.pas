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

unit UTM;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, VirtualTrees,
  UWTMX, UCommon, UTypes, Clipbrd, lclintf, LCLType;

type

  { TfrmTM }

  TfrmTM = class(TForm)
    btnCopy: TButton;
    btnClose: TButton;
    btnUse: TButton;
    VST: TVirtualStringTree;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnUseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTResize(Sender: TObject);
  private

  public
    procedure FindSimilary(const AText: String);
    function GetItemAtIndex(const Index: Integer): String;
  end;

var
  frmTM: TfrmTM;

// -----------------------------------------------------------------------------

implementation

uses UMain;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmTM }

// -----------------------------------------------------------------------------

procedure TfrmTM.FormCreate(Sender: TObject);
begin
  ReadLangForForm(LanguageFileName, Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmTM.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if (Column = 2) and (TMX.Map.Keys[Node^.Index] = 1) then
    VSTPaintCell(TargetCanvas, CellRect, clLime);
end;

// -----------------------------------------------------------------------------

procedure TfrmTM.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmTM.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
begin
  case Column of
    0: CellText := TMX.ItemFromMap(Node^.Index).Original; //TMX.Items[ TMX.Map.Data[Node^.Index] ]^.Original;
    1: CellText := TMX.ItemFromMap(Node^.Index).Translated; //TMX.Items[ TMX.Map.Data[Node^.Index] ]^.Translated;
    2: CellText := FloatToStrF(TMX.Map.Keys[Node^.Index]*100, ffFixed, 0, 0);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTM.VSTResize(Sender: TObject);
var
  wCols: Integer;
begin
  wCols := (VST.Width-VST.Header.Columns[2].Width) - (GetSystemMetrics(SM_CXVSCROLL)+5);
  wCols := wCols div 2;
  VST.Header.Columns[0].Width := wCols;
  VST.Header.Columns[1].Width := wCols;
end;

// -----------------------------------------------------------------------------

procedure TfrmTM.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction := caFree;
  frmTM := NIL
end;

// -----------------------------------------------------------------------------

procedure TfrmTM.btnCopyClick(Sender: TObject);
//var
//  h: TUWTMXHeader;
begin
//  h.creationtool := ProgramName;
//  h.creationtoolversion:= Format('%.2f', [ProgramVer / 1000], FormatSettings);
//  h.otmf := h.creationtool;

//  TMX.Header := h;
//  TMX.SaveToFile('c:\subtitles\z.tmx');
  if Assigned(VST.FocusedNode) then
    Clipboard.AsText := GetItemAtIndex(VST.FocusedNode^.Index);
end;

// -----------------------------------------------------------------------------

procedure TfrmTM.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmTM.btnUseClick(Sender: TObject);
begin
  if Assigned(VST.FocusedNode) then
    frmMain.mmoTranslation.Text := GetItemAtIndex(VST.FocusedNode^.Index);
end;

// -----------------------------------------------------------------------------

procedure TfrmTM.FindSimilary(const AText: String);
begin
  VST.RootNodeCount := TMX.FindSimilary(AText);
  VST.Invalidate;
end;

// -----------------------------------------------------------------------------

function TfrmTM.GetItemAtIndex(const Index: Integer): String;
begin
//  Result := '';
//  if (TMX.Map.Count > 0) and InRange(Index, 0, TMX.Map.Count-1) then
    Result := TMX.ItemFromMap(Index).Translated; // Result := TMX.Items[TMX.Map.Data[Index]]^.Translated;
end;

// -----------------------------------------------------------------------------

end.

