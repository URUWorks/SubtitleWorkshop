{*
 *  URUWorks SW Styles
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

{$mode objfpc}{$H+}
//{$mode delphi}

unit UWCustom.SWStyles;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, generics.collections, RegExpr, Graphics;

type

  { TStyleItem }

  PStyleItem = ^TStyleItem;
  TStyleItem = packed record
    Title, FontName: String;
    FontSize: Integer;
    Bold, Italic, Underline, Strikeout: Boolean;
    ColorPrimary, ColorSec, ColorOutline, ColorShadow: Integer;
    Outline, Shadow, ScaleX, ScaleY, Rotation, Spacing: Integer;
    MarginLeft, MarginRight, MarginVert: Integer;
    Align: Byte;
  end;

  { TStyleList }

  TStyleList = specialize TList<TStyleItem>;

  { TSWStyles }

  TSWStyles = class
  private
    { Private declarations }
    FFileName : String;
    FStyles   : TStyleList;
    function ValidIndex(const Index: Integer): Boolean;
    function GetCount: Integer;
    function GetItem(Index: Integer): TStyleItem;
    procedure PutItem(Index: Integer; const Item: TStyleItem);
  public
    { Public declarations }
    constructor Create(const FileName: String);
    destructor Destroy; override;
    procedure SaveToFile;
    procedure LoadFromFile;
    procedure ClearItem(var AItem: TStyleItem);
    property FileName : String  read FFileName;
    property ItemCount: Integer read GetCount;
    property Items[Index: Integer]: TStyleItem read GetItem write PutItem; default;
    property ItemList: TStyleList read FStyles write FStyles;
  end;


// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{ TSWStyles }

// -----------------------------------------------------------------------------

constructor TSWStyles.Create(const FileName: String);
begin
  FFileName := FileName;
  FStyles   := TStyleList.Create;
  LoadFromFile;
end;

// -----------------------------------------------------------------------------

destructor TSWStyles.Destroy;
begin
  SaveToFile;
  FStyles.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TSWStyles.SaveToFile;
var
  s: TStringList;
  i: Integer;
begin
  if (FFileName <> '') and (FStyles.Count > 0) then
  begin
    s := TStringList.Create;
    try
      for i := 0 to FStyles.Count-1 do
        with FStyles[i] do
        begin
          s.Add(
            Format('%s,%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d',
              [Title, FontName, FontSize, Integer(Bold), Integer(Italic),
              Integer(Underline), Integer(Strikeout), ColorPrimary, ColorSec,
              ColorOutline, ColorShadow, Outline, Shadow, ScaleX, ScaleY,
              Rotation, Spacing, MarginLeft, MarginRight, MarginVert, Align])
            );
        end;
      s.SaveToFile(FFileName);
    finally
      s.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TSWStyles.LoadFromFile;
var
  s, split : TStringList;
  i        : Integer;
  item     : TStyleItem;
begin
  if (FFileName <> '') and FileExists(FFileName) then
  begin
    s := TStringList.Create;
    try
      s.LoadFromFile(FFileName);
      split := TStringList.Create;
      try
        for i := 0 to s.Count-1 do
        begin
          SplitRegExpr('\,', s[i], split);
          with item do
          begin
            Title        := split[0];
            FontName     := split[1];
            FontSize     := StrToInt(split[2]);
            Bold         := Boolean(StrToInt(split[3]));
            Italic       := Boolean(StrToInt(split[4]));
            Underline    := Boolean(StrToInt(split[5]));
            Strikeout    := Boolean(StrToInt(split[6]));
            ColorPrimary := TColor(StrToInt(split[7]));
            ColorSec     := TColor(StrToInt(split[8]));
            ColorOutline := TColor(StrToInt(split[9]));
            ColorShadow  := TColor(StrToInt(split[10]));
            Outline      := StrToInt(split[11]);
            Shadow       := StrToInt(split[12]);
            ScaleX       := StrToInt(split[13]);
            ScaleY       := StrToInt(split[14]);
            Rotation     := StrToInt(split[15]);
            Spacing      := StrToInt(split[16]);
            MarginLeft   := StrToInt(split[17]);
            MarginRight  := StrToInt(split[18]);
            MarginVert   := StrToInt(split[19]);
            Align        := StrToInt(split[20]);
          end;
          FStyles.Add(item);
        end;
      finally
        split.Free;
      end;
    finally
      s.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TSWStyles.ValidIndex(const Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < FStyles.Count);
end;

// -----------------------------------------------------------------------------

function TSWStyles.GetCount: Integer;
begin
  Result := FStyles.Count;
end;

// -----------------------------------------------------------------------------

function TSWStyles.GetItem(Index: Integer): TStyleItem;
begin
  if ValidIndex(Index) then
    with FStyles[Index] do
    begin
      Result.Title := Title;
      Result.FontName := FontName;
      Result.FontSize := FontSize;
      Result.Bold := Bold;
      Result.Italic := Italic;
      Result.Underline := Underline;
      Result.Strikeout := Strikeout;
      Result.ColorPrimary := ColorPrimary;
      Result.ColorSec := ColorSec;
      Result.ColorOutline := ColorOutline;
      Result.ColorShadow := ColorShadow;
      Result.Outline := Outline;
      Result.Shadow := Shadow;
      Result.ScaleX := ScaleX;
      Result.ScaleY := ScaleY;
      Result.Rotation := Rotation;
      Result.Spacing := Spacing;
      Result.MarginLeft := MarginLeft;
      Result.MarginRight := MarginRight;
      Result.MarginVert := MarginVert;
      Result.Align := Align;
    end;
end;

// -----------------------------------------------------------------------------

procedure TSWStyles.PutItem(Index: Integer; const Item: TStyleItem);
var
  nitem: TStyleItem;
begin
  if ValidIndex(Index) then
  begin
    with nitem do
    begin
      Title := Item.Title;
      FontName := Item.FontName;
      FontSize := Item.FontSize;
      Bold := Item.Bold;
      Italic := Item.Italic;
      Underline := Item.Underline;
      Strikeout := Item.Strikeout;
      ColorPrimary := Item.ColorPrimary;
      ColorSec := Item.ColorSec;
      ColorOutline := Item.ColorOutline;
      ColorShadow := Item.ColorShadow;
      Outline := Item.Outline;
      Shadow := Item.Shadow;
      ScaleX := Item.ScaleX;
      ScaleY := Item.ScaleY;
      Rotation := Item.Rotation;
      Spacing := Item.Spacing;
      MarginLeft := Item.MarginLeft;
      MarginRight := Item.MarginRight;
      MarginVert := Item.MarginVert;
      Align := Item.Align;
    end;
    FStyles[Index] := nitem;
  end;
end;

// -----------------------------------------------------------------------------

procedure TSWStyles.ClearItem(var AItem: TStyleItem);
begin
  with AItem do
  begin
    Title := '';
    FontName := '';
    FontSize := 0;
    Bold := False;
    Italic := False;
    Underline := False;
    Strikeout := False;
    ColorPrimary := 0;
    ColorSec := 0;
    ColorOutline := 0;
    ColorShadow := 0;
    Outline := 0;
    Shadow := 0;
    ScaleX := 0;
    ScaleY := 0;
    Rotation := 0;
    Spacing := 0;
    MarginLeft := 0;
    MarginRight := 0;
    MarginVert := 0;
    Align := 0;
  end;
end;

// -----------------------------------------------------------------------------

end.
