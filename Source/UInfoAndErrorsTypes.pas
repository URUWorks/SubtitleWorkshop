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

unit UInfoAndErrorsTypes;

// -----------------------------------------------------------------------------

{$mode delphi}
//{$mode objfpc}{$H+}

interface

uses generics.collections, UTypes, UWSubtitleAPI;

type

  { TSubtitleInfoItem }

  PSubtitleInfoItem = ^TSubtitleInfoItem;
  TSubtitleInfoItem = record
    Apply       : Boolean;
    Index       : Integer;
    InitialTime,
    FinalTime   : Integer;
    Text,
    Translation : String;
    ErrorsFixed : TSubtitleErrorType;
  end;

  { TUWSubtitleInfo }

  TSubtitleInfoList = TList<TSubtitleInfoItem>;

  TUWSubtitleInfo = class(TSubtitleInfoList)
  private
    FErrors    : TSubtitleErrorTypeSet;
    FSubtitles : TUWSubtitles;
  public
    constructor Create(const Subtitles: TUWSubtitles = NIL);
    destructor Destroy; override;
    property Errors: TSubtitleErrorTypeSet read FErrors write FErrors;
    procedure FixErrors(const OCRFile: String = '');
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitles.OCR, UWSubtitles.Utils;

// -----------------------------------------------------------------------------

function GetItemDuration(const Item: TSubtitleInfoItem): Cardinal;
begin
  Result := Item.FinalTime - Item.InitialTime;
end;

// -----------------------------------------------------------------------------

{function GetItemCPS(const Item: TSubtitleInfoItem): Double;
begin
  with Item do
    Result := GetSubtitleCPS(Text, InitialTime, FinalTime);
end;}

// -----------------------------------------------------------------------------

function IsEqualSubtitle(const Item1: TSubtitleInfoItem; const Item2: TUWSubtitleItem): Boolean;
begin
  if (Item1.InitialTime = Item2.InitialTime) and
     (Item1.FinalTime = Item2.FinalTime) and
     (Item1.Text = Item2.Text) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

constructor TUWSubtitleInfo.Create(const Subtitles: TUWSubtitles = NIL);
begin
  FSubtitles := Subtitles;
  FErrors    := [];

  inherited Create;
end;

// -----------------------------------------------------------------------------

destructor TUWSubtitleInfo.Destroy;
begin
  if FSubtitles <> NIL then FSubtitles := NIL;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitleInfo.FixErrors(const OCRFile: String = '');
var
  Item      : TUWSubtitleItem;
  FixedItem : TSubtitleInfoItem;
  i, x      : Integer;
  tmp       : String;
  ocr       : TUWOCRScript;

  procedure ClearItem(const Index: Integer);
  begin
    FixedItem.Index       := Index;
    FixedItem.Apply       := True;
    FixedItem.InitialTime := FSubtitles[Index].InitialTime;
    FixedItem.FinalTime   := FSubtitles[Index].FinalTime;
    FixedItem.Text        := FSubtitles[Index].Text;
    FixedItem.ErrorsFixed := etNone;
  end;

begin
  if FSubtitles = NIL then Exit;

  Clear;
  ocr := TUWOCRScript.Create(OCRFile);
  try
    for i := 0 to FSubtitles.Count-1 do
    begin
      Item := FSubtitles[i];

      ClearItem(i);
      // Repeated subtitle
      if etRepeatedSubtitle in FErrors then
      begin
        if (i > 0) and IsEqualSubtitle(FixedItem, FSubtitles[i-1]) then
        begin
          FixedItem.ErrorsFixed := etRepeatedSubtitle;
          Add(FixedItem);
        end;
      end;

      ClearItem(i);
      // Unnecessary Spaces
      if etUnnecessarySpaces in FErrors then
      begin
        tmp := RemoveUnnecessarySpaces(FixedItem.Text);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := etUnnecessarySpaces;
          Add(FixedItem);
        end;
      end;

      ClearItem(i);
      // Unnecessary Dots
      if etUnnecessaryDots in FErrors then
      begin
        tmp := RemoveUnnecessaryDots(FixedItem.Text);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := etUnnecessaryDots;
          Add(FixedItem);
        end;
      end;

      ClearItem(i);
      // Empty subtitle
      if etEmpty in FErrors then
      begin
        if FixedItem.Text = '' then
        begin
          FixedItem.ErrorsFixed := etEmpty;
          Add(FixedItem);
        end;
      end;

      ClearItem(i);
      // Fix Tags
      if etFixTags in FErrors then
      begin
        tmp := FixTags(FixedItem.Text, swt_StartTag, swt_EndTag);
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := etFixTags;
          Add(FixedItem);
        end;
        //
        tmp := FixTags(FixedItem.Text, '<', '>');
        if FixedItem.Text <> tmp then
        begin
          FixedItem.Text := tmp;
          FixedItem.ErrorsFixed := etFixTags;
          Add(FixedItem);
        end;
      end;

      ClearItem(i);
      // Prohibited chars
      if etProhibitedChars in FErrors then
      begin
        if HasProhibitedChars(Item.Text, Options.ErrCfg.ProhibitedChars) then
        begin
          FixedItem.ErrorsFixed := etProhibitedChars;
          Add(FixedItem);
        end;
      end;

      ClearItem(i);
      // Break long lines
      if etBreakLongLines in FErrors then
      begin
        if (Length(FixedItem.Text) > Options.ErrCfg.MaxLineLength) and (Pos(sLineBreak, FixedItem.Text) = 0) then
        begin
          tmp := AutoBreakSubtitle(FixedItem.Text, Options.ErrCfg.MaxLineLength, sLineBreak, False);
          if FixedItem.Text <> tmp then
          begin
            FixedItem.Text := tmp;
            FixedItem.ErrorsFixed := etBreakLongLines;
            Add(FixedItem);
          end;
        end;
      end;

      ClearItem(i);
      // Hearing impaired
      if etHearingImpaired in FErrors then
      begin
        if IsHearingImpaired(FixedItem.Text) then
        begin
          tmp := FixHearingImpaired(FixedItem.Text, sLineBreak);
          if FixedItem.Text <> tmp then
          begin
            FixedItem.Text        := tmp;
            FixedItem.ErrorsFixed := etHearingImpaired;
            Add(FixedItem);
          end;
        end;
      end;

      ClearItem(i);
      // Repeated chars
      if etRepeatedChars in FErrors then
      begin
        if HasRepeatedChar(Item.Text, Options.ErrCfg.RepeatableChars) then
        begin
          tmp := FixRepeatedChar(FixedItem.Text, Options.ErrCfg.RepeatableChars);
          if FixedItem.Text <> tmp then
          begin
            FixedItem.Text        := tmp;
            FixedItem.ErrorsFixed := etRepeatedChars;
            Add(FixedItem);
          end;
        end;
      end;

      ClearItem(i);
      // OCR
      if etOCR in FErrors then
      begin
        if ocr.HasErrors(FixedItem.Text) then
        begin
          tmp := ocr.Fix(FixedItem.Text);
          if FixedItem.Text <> tmp then
          begin
            FixedItem.Text        := tmp;
            FixedItem.ErrorsFixed := etOCR;
            Add(FixedItem);
          end;
        end;
      end;

      ClearItem(i);
      // Time too short
      if etTimeTooShort in FErrors then
      begin
        if GetItemDuration(FixedItem) < Options.ErrCfg.MinDuration then
        begin
          FixedItem.FinalTime   := FixedItem.InitialTime + Options.ErrCfg.MinDuration;
          FixedItem.ErrorsFixed := etTimeTooShort;
          Add(FixedItem);
        end;
      end;

      ClearItem(i);
      // Time too long
      if etTimeTooLong in FErrors then
      begin
        if GetItemDuration(FixedItem) > Options.ErrCfg.MaxDuration then
        begin
          FixedItem.FinalTime   := FixedItem.InitialTime + Options.ErrCfg.MaxDuration;
          FixedItem.ErrorsFixed := etTimeTooLong;
          Add(FixedItem);
        end;
      end;

      ClearItem(i);
      // Bad Values
      if etBadValues in FErrors then
      begin
        if FixedItem.InitialTime > FixedItem.FinalTime then
        begin
          x := FixedItem.InitialTime;
          FixedItem.InitialTime := FixedItem.FinalTime;
          FixedItem.FinalTime   := x;
          FixedItem.ErrorsFixed := etBadValues;
          Add(FixedItem);
        end;
      end;

      ClearItem(i);
      // Overlapping
      if etOverlapping in FErrors then
      begin
        if i > 0 then
        begin
          if FixedItem.InitialTime <= FSubtitles[i-1].FinalTime then
          begin
            FixedItem.InitialTime := FSubtitles[i-1].FinalTime + Options.ErrCfg.MinPause;
            FixedItem.ErrorsFixed := etOverlapping;
            Add(FixedItem);
          end;
        end
        else if (i < (FSubtitles.Count-1)) and (FixedItem.FinalTime >= FSubtitles[i+1].InitialTime) then
        begin
          x := FSubtitles[i+1].InitialTime - 1;
          if x < (FixedItem.InitialTime + Options.ErrCfg.MinDuration) then
          begin
            FixedItem.FinalTime := FSubtitles[i+1].InitialTime;
            FSubtitles.ItemPointer[i+1].InitialTime := FSubtitles[i+1].InitialTime + 1;
          end
          else
            FixedItem.FinalTime := x;

          FixedItem.ErrorsFixed := etOverlapping;
          Add(FixedItem);
        end;
      end;
    end;
  finally
    ocr.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
