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

unit UGlossary;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, VirtualTrees,
  UWGlossary, UCommon;

type

  { TfrmGlossary }

  TfrmGlossary = class(TForm)
    btnAdd: TButton;
    edtCopy: TButton;
    cboFile: TComboBox;
    edtFind: TEdit;
    edtOriginal: TEdit;
    edtTranslated: TEdit;
    grpNew: TGroupBox;
    lblFind: TLabel;
    lblOriginal: TLabel;
    lblTranslated: TLabel;
    lblComments: TLabel;
    lblFile: TLabel;
    mmoComments: TMemo;
    VST: TVirtualStringTree;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
    FList: TUWGlossary;
  public

  end;

var
  frmGlossary: TfrmGlossary;

// -----------------------------------------------------------------------------

implementation

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmGlossary }

// -----------------------------------------------------------------------------

procedure TfrmGlossary.FormCreate(Sender: TObject);
begin
 // FList := TUWGlossary.Create('C:\Subtitles\terminology.po');
  FList := TUWGlossary.Create('');

  if FList.Items.Count > 0 then
  begin
    VST.RootNodeCount := FList.Items.Count;
  end;

  ReadLangForForm(LanguageFileName, Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmGlossary.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmGlossary.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
begin
  case Column of
    0: CellText := FList.Items[Node^.Index]^.Original;
    1: CellText := FList.Items[Node^.Index]^.Translated;
    2: CellText := FList.Items[Node^.Index]^.Comments;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmGlossary.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  FList.Free;
  CloseAction := caFree;
  frmGlossary := NIL
end;

// -----------------------------------------------------------------------------

end.

