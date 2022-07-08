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

unit UUndo;

{$mode objfpc}{$H+}

interface

uses
  UWSubtitleAPI, FGL, LCLIntf;

type

  { TUndoItem }

  TUndoType = (utInsertLine, utDeleteLine, utSubtitleChange);

  PUndoItem = ^TUndoItem;
  TUndoItem = packed record
    UndoType : TUndoType;
    Index    : Integer;
    Subtitle : TUWSubtitleItem;
    Group    : Byte;
  end;

  { TUndoList }

  TUndoList = specialize TFPGList<PUndoItem>;

  { TUndo }

  TUndoChangeType = (uctCount, uctItems, uctReIndex);
  TUndoChangeEvent = procedure(const ChangeType: TUndoChangeType) of object;

  TUndo = class
  private
    FUndoList      : TUndoList;
    FRedoList      : TUndoList;
    FMax           : Byte;
    FLastTickCount : Cardinal;
    FOnChange : TUndoChangeEvent;
    procedure Changed(const ChangeType: TUndoChangeType);
  public
    constructor Create;
    destructor Destroy; override;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    procedure AddUndo(const UndoItem: TUndoItem; const ClearRedo: Boolean = True); overload;
    procedure AddUndo(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const Group: Byte; const ClearRedo: Boolean = True); overload;
    procedure AddUndoIfNotResent(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const Group: Byte; const ClearRedo: Boolean = True);
    procedure AddRedo(const UndoItem: TUndoItem); overload;
    procedure AddRedo(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const Group: Byte; const ClearRedo: Boolean = True); overload;
    procedure Undo;
    procedure Redo;
    property Max: Byte read FMax write FMax;
    property OnChange: TUndoChangeEvent read FOnChange write FOnChange;
  end;

// -----------------------------------------------------------------------------

implementation

uses UTypes, UMain, UCommon;

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function TUndoToPUndo(const UndoItem: TUndoItem): PUndoItem;
begin
  New(Result);
  with Result^ do
  begin
    Result^.UndoType := UndoItem.UndoType;
    Result^.Index    := UndoItem.Index;
    Result^.Subtitle := UndoItem.Subtitle;
    Result^.Group    := UndoItem.Group;
  end;
end;

// -----------------------------------------------------------------------------

procedure ClearList(var List: TUndoList);
var
  i: Integer;
begin
  if not Assigned(List) then Exit;

  if List.Count > 0 then
  begin
    for i := List.Count-1 downto 0 do
    begin
      Dispose(PUndoItem(List.Items[i]));
      List.Items[i] := NIL;
      List.Delete(i);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure DeleteLast(var List: TUndoList);
var
  i: Integer;
begin
  if not Assigned(List) then Exit;

  i := List.Count-1;
  Dispose(PUndoItem(List.Items[i]));
  List.Items[i] := NIL;
  List.Delete(i);
end;

// -----------------------------------------------------------------------------

procedure CheckList(var FromList, ToList: TUndoList);

  function MakeUndoItem(const UndoType: TUndoType; const Index: Integer; const Group: Byte): PUndoItem;
  var
    UndoItem: TUndoItem;
  begin
    UndoItem.UndoType := UndoType;
    UndoItem.Index    := Index;
    UndoItem.Subtitle := Subtitles.Items[Index];
    UndoItem.Group    := Group;
    Result := TUndoToPUndo(UndoItem);
  end;

var
  i: Integer;
  group: Byte;
begin
  if not Assigned(FromList) or not Assigned(ToList) then Exit;

  frmMain.VST.ClearSelection;

  group := FromList.Last^.Group;
  if (FromList.Last^.UndoType = utSubtitleChange) and Subtitles.ValidIndex(FromList.Last^.Index) then
  begin
    ToList.Add(MakeUndoItem(utSubtitleChange, FromList.Last^.Index, FromList.Last^.Group));
    Subtitles.Items[FromList.Last^.Index] := FromList.Last^.Subtitle;
    VSTSelectNode(FromList.Last^.Index, False);
  end
  else if (FromList.Last^.UndoType = utInsertLine) and Subtitles.ValidIndex(FromList.Last^.Index) then
  begin
    ToList.Add(MakeUndoItem(utDeleteLine, FromList.Last^.Index, FromList.Last^.Group));
    Subtitles.Delete(FromList.Last^.Index);
  end
  else if (FromList.Last^.UndoType = utDeleteLine) then
  begin
    ToList.Add(MakeUndoItem(utInsertLine, FromList.Last^.Index, FromList.Last^.Group));
    Subtitles.Insert(FromList.Last^.Index, FromList.Last^.Subtitle, NIL, False);
    VSTSelectNode(FromList.Last^.Index, False);
  end;

  DeleteLast(FromList);
  for i := FromList.Count-1 downto 0 do
  begin
    if group = FromList[i]^.Group then
    begin
      if (FromList.Last^.UndoType = utSubtitleChange) and Subtitles.ValidIndex(FromList[i]^.Index) then
      begin
        ToList.Add(MakeUndoItem(utSubtitleChange, FromList[i]^.Index, FromList[i]^.Group));
        Subtitles.Items[FromList[i]^.Index] := FromList[i]^.Subtitle;
        VSTSelectNode(FromList.Last^.Index, False);
      end
      else if (FromList.Last^.UndoType = utInsertLine) and Subtitles.ValidIndex(FromList[i]^.Index) then
      begin
        ToList.Add(MakeUndoItem(utDeleteLine, FromList[i]^.Index, FromList[i]^.Group));
        Subtitles.Delete(FromList[i]^.Index);
      end
      else if FromList.Last^.UndoType = utDeleteLine then
      begin
        ToList.Add(MakeUndoItem(utInsertLine, FromList[i]^.Index, FromList[i]^.Group));
        Subtitles.Insert(FromList[i]^.Index, FromList[i]^.Subtitle, NIL, False);
        VSTSelectNode(FromList[i]^.Index, False);
      end;
      DeleteLast(FromList);
    end
    else
      Break;
  end;
end;

// -----------------------------------------------------------------------------

{ TUndo }

// -----------------------------------------------------------------------------

constructor TUndo.Create;
begin
  FUndoList      := TUndoList.Create;
  FRedoList      := TUndoList.Create;
  FMax           := 50;
  FLastTickCount := GetTickCount;
end;

// -----------------------------------------------------------------------------

destructor TUndo.Destroy;
begin
  ClearList(FUndoList);
  ClearList(FRedoList);
  FUndoList.Free;
  FRedoList.Free;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TUndo.CanUndo: Boolean;
begin
  Result := FUndoList.Count > 0;
end;

// -----------------------------------------------------------------------------

function TUndo.CanRedo: Boolean;
begin
  Result := FRedoList.Count > 0;
end;

// -----------------------------------------------------------------------------

procedure TUndo.Changed(const ChangeType: TUndoChangeType);
begin
  if Assigned(FOnChange) then FOnChange(ChangeType);
end;

// -----------------------------------------------------------------------------

procedure TUndo.AddUndo(const UndoItem: TUndoItem; const ClearRedo: Boolean = True);
begin
  if FUndoList.Count >= FMax then FUndoList.Delete(0);

  FUndoList.Add(TUndoToPUndo(UndoItem));
  if ClearRedo then ClearList(FRedoList);
  Changed(uctCount);
end;

// -----------------------------------------------------------------------------

procedure TUndo.AddUndo(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const Group: Byte; const ClearRedo: Boolean = True);
var
  UndoItem: TUndoItem;
begin
  UndoItem.UndoType := UndoType;
  UndoItem.Index    := Index;
  UndoItem.Subtitle := Subtitle;
  UndoItem.Group    := Group;
  AddUndo(UndoItem, ClearRedo);
end;

// -----------------------------------------------------------------------------

procedure TUndo.AddUndoIfNotResent(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const Group: Byte; const ClearRedo: Boolean = True);
begin
  if ((GetTickCount - FLastTickCount) > 500) then // add undo only if > 500ms
  begin
    AddUndo(UndoType, Index, Subtitle, Group, ClearRedo);
    FLastTickCount := GetTickCount;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUndo.AddRedo(const UndoItem: TUndoItem);
begin
  FRedoList.Add(TUndoToPUndo(UndoItem));
  Changed(uctCount);
end;

// -----------------------------------------------------------------------------

procedure TUndo.AddRedo(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const Group: Byte; const ClearRedo: Boolean = True);
var
  UndoItem: TUndoItem;
begin
  UndoItem.UndoType := UndoType;
  UndoItem.Index    := Index;
  UndoItem.Subtitle := Subtitle;
  UndoItem.Group    := Group;
  AddRedo(UndoItem);
end;

// -----------------------------------------------------------------------------

procedure TUndo.Undo;
begin
  if not CanUndo then Exit;

  case FUndoList.Last^.UndoType of
    utInsertLine: begin // then delete!
                    if (Subtitles.Count > 0) and (FUndoList.Last^.Index <= Subtitles.Count) then
                    begin
                      CheckList(FUndoList, FRedoList);
                      Changed(uctReIndex);
                    end;
                  end;
    utDeleteLine: begin // then insert!
                    CheckList(FUndoList, FRedoList);
                    Changed(uctReIndex);
                  end;
    else // utSubtitleChange then Undo!
      if (Subtitles.Count > 0) and (FUndoList.Count > 0) and (FUndoList.Last^.Index <= Subtitles.Count) then
      begin
        CheckList(FUndoList, FRedoList);
        Changed(uctItems);
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure TUndo.Redo;
begin
  if not CanRedo then Exit;

  case FRedoList.Last^.UndoType of
    utInsertLine: begin // then Insert!
                    if (Subtitles.Count > 0) and (FRedoList.Last^.Index <= Subtitles.Count) then
                    begin
                      CheckList(FRedoList, FUndoList);
                      Changed(uctReIndex);
                    end;
                  end;
    utDeleteLine: begin // then Delete!
                    CheckList(FRedoList, FUndoList);
                    Changed(uctReIndex);
                  end;
    else // utSubtitleChange then Redo!
      if (Subtitles.Count > 0) and (FRedoList.Count > 0) and (FRedoList.Last^.Index <= Subtitles.Count) then
      begin
        CheckList(FRedoList, FUndoList);
        Changed(uctItems);
      end;
    end;
end;

// -----------------------------------------------------------------------------

end.

