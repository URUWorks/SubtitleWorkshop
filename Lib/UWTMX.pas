{*
 *  URUWorks Translation Memory Exchange (TMX)
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

unit UWTMX;

// -----------------------------------------------------------------------------

interface

uses Classes, SysUtils, FGL, laz2_XMLRead, laz2_DOM, laz2_XMLWrite, Math,
  UWSystem.StrUtils;

type

  { TUWTMXHeader }

  PUWTMXHeader = ^TUWTMXHeader;
  TUWTMXHeader = packed record
    creationtool,        // "Subtitle Workshop"
    creationtoolversion, // "7.0"
    datatype,            // "tbx"
    segtype,             // "block", "sentence"
    adminlang,           // "en-us"
    srclang,             // "en"
    otmf: String;        // ""
  end;

  { TUWTMXItem }

  PUWTMXItem = ^TUWTMXItem;
  TUWTMXItem = packed record
    Original,
    Translated,
    Notes: String;
  end;

  { TUWTMXItemInfo }

  PUWTMXItemInfo = ^TUWTMXItemInfo;
  TUWTMXItemInfo = packed record
    SrcLang,         // "en"
    DstLang: String; // "es"
  end;

  { TUWTMXList }

  TUWTMXList = specialize TFPGList<PUWTMXItem>;

  { TUWTMXMap }

  TUWTMXMap = specialize TFPGMap<Double, Integer>; // Ratio, Index

  { TUWTMX }

  TUWTMX = class
  private
    FHeader : TUWTMXHeader;
    FLangs  : TUWTMXItemInfo;
    FList   : TUWTMXList;
    FMap    : TUWTMXMap;
  public
    constructor Create(const FileName: String);
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const FileName: String);
    function SaveToFile(const FileName: String): Boolean;
    procedure AddItem(const Original, Translated, Notes: String; const AllowDuplicate: Boolean = False);
    function FindSimilary(const AText: String): Integer; // fill Map with results
    function ItemFromMap(const AIndex: Integer): TUWTMXItem;
    property Items  : TUWTMXList     read FList;
    property Map    : TUWTMXMap      read FMap;
    property Langs  : TUWTMXItemInfo read FLangs  write FLangs;
    property Header : TUWTMXHeader   read FHeader write FHeader;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSystem.XmlUtils;

// -----------------------------------------------------------------------------

function ListCompare(const Item1, Item2: PUWTMXItem): Integer;
begin
  if Item1^.Original < Item2^.Original then
    Result := -1
  else if Item1^.Original > Item2^.Original then
    Result := 1
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function KeyMapCompare(const Key1, Key2: Double): Integer;
begin
  if Key1 < Key2 then
    Result := 1
  else if Key1 > Key2 then
    Result := -1
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

constructor TUWTMX.Create(const FileName: String);
begin
  FillByte(FHeader, SizeOf(TUWTMXHeader), 0);
  FLangs.SrcLang := 'en';
  FLangs.DstLang := 'es';

  FList := TUWTMXList.Create;

  FMap := TUWTMXMap.Create;
  FMap.OnKeyCompare := @KeyMapCompare;
  FMap.Sorted := True;

  if FileName <> '' then LoadFromFile(FileName);
end;

// -----------------------------------------------------------------------------

destructor TUWTMX.Destroy;
var
  i: Integer;
begin
  //save before free?
  //...

  Clear;
  FList.Free;
  FMap.Clear;
  FMap.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWTMX.Clear;
var
  i: Integer;
begin
  if FList.Count > 0 then
  begin
    for i := FList.Count-1 downto 0 do
    begin
      Dispose(PUWTMXItem(FList.Items[i]));
      FList.Items[i] := NIL;
      FList.Delete(i);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWTMX.LoadFromFile(const FileName: String);
var
  XmlDoc: TXMLDocument;
  NodeTU, NodeTUV,
  NodeID, NodeText: TDOMNode;
  Item: PUWTMXItem;
begin
  if not FileExists(FileName) then Exit;

  XmlDoc := NIL;
  ReadXMLFile(XmlDoc, FileName);
  if Assigned(XmlDoc) then
  try
    // Header
    NodeID := XMLFindNodeByName(XmlDoc, 'header');
    if NodeID <> NIL then
      with FHeader, NodeID.Attributes do
      begin
        creationtool        := GetNamedItem('creationtool').NodeValue;
        creationtoolversion := GetNamedItem('creationtoolversion').NodeValue;
        datatype            := GetNamedItem('datatype').NodeValue;
        segtype             := GetNamedItem('segtype').NodeValue;
        adminlang           := GetNamedItem('adminlang').NodeValue;
        srclang             := GetNamedItem('srclang').NodeValue;
        otmf                := GetNamedItem('o-tmf').NodeValue;
      end;

    if FHeader.srclang <> '' then FLangs.SrcLang := FHeader.srclang;

    // TMs
    Item := NIL;
    NodeTU := XMLFindNodeByName(XmlDoc, 'tu'); //XmlDoc.DocumentElement.FindNode('tu');
    while NodeTU <> NIL do
    begin
      New(Item);
      FillByte(Item[0], SizeOf(PUWTMXItem), 0);

      NodeTUV := NodeTU.FirstChild;
      while NodeTUV <> NIL do
      begin
        NodeID := NodeTUV.Attributes.GetNamedItem('xml:lang');
        if NodeID <> NIL then
        begin
          if (NodeID.NodeValue = FLangs.SrcLang) then
          begin // src
            NodeText := NodeTUV.FirstChild;
            while NodeText <> NIL do
            begin
              if NodeText.NodeName = 'seg' then
                Item^.Original := NodeText.TextContent;

              NodeText := NodeText.NextSibling;
            end;
          end
          else if (NodeID.NodeValue = FLangs.DstLang) then
          begin // dst
            NodeText := NodeTUV.FirstChild;
            while NodeText <> NIL do
            begin
              if NodeText.NodeName = 'seg' then
                Item^.Translated := NodeText.TextContent
              else if NodeText.NodeName = 'note' then
                Item^.Notes := NodeText.TextContent;

              NodeText := NodeText.NextSibling;
            end;
          end;
        end;
        NodeTUV := NodeTUV.NextSibling;
      end;
      NodeTU := NodeTU.NextSibling;

      if (Item^.Original <> '') and (Item^.Translated <> '') then
        FList.Add(Item)
      else
        Dispose(Item);
    end;
  finally
    XmlDoc.Free;
  end;
  // Sort for fast searching
  if FList.Count > 0 then FList.Sort(@ListCompare);
end;

// -----------------------------------------------------------------------------

function TUWTMX.SaveToFile(const FileName: String): Boolean;
var
  XmlDoc : TXMLDocument;
  Root, Element, Node, SubNode : TDOMNode;
  i : Integer;
begin
  Result := False;
  if FList.Count = 0 then exit;

  XmlDoc := TXMLDocument.Create;
  try
    Root := XmlDoc.CreateElement('tmx');
      TDOMElement(Root).SetAttribute('version', '1.4');
      XmlDoc.Appendchild(Root);
    Root := XmlDoc.DocumentElement;

    Element := XmlDoc.CreateElement('header');
      with TDOMElement(Element), FHeader do
      begin
        SetAttribute('creationtool', creationtool);
        SetAttribute('creationtoolversion', creationtoolversion);
        SetAttribute('datatype', datatype);
        SetAttribute('segtype', segtype);
        SetAttribute('adminlang', adminlang);
        SetAttribute('srclang', srclang);
        SetAttribute('o-tmf', otmf);

      end;
    Root.AppendChild(Element);

    Element := XmlDoc.CreateElement('body');
    Root.AppendChild(Element);

    Root := Element; // body

    for i := 0 to FList.Count-1 do
    begin
      Node := XmlDoc.CreateElement('tu');
      Root.AppendChild(Node);

      // original
      Element := XmlDoc.CreateElement('tuv');
      TDOMElement(Element).SetAttribute('xml:lang', FLangs.SrcLang);
        SubNode := XmlDoc.CreateElement('seg');
        SubNode.TextContent := FList[i]^.Original;
      Element.AppendChild(SubNode); // tuv
      Node.AppendChild(Element); // tu

      // translated
      Element := XmlDoc.CreateElement('tuv');
      TDOMElement(Element).SetAttribute('xml:lang', FLangs.DstLang);
        SubNode := XmlDoc.CreateElement('seg');
        SubNode.TextContent := FList[i]^.Translated;
        Element.AppendChild(SubNode); // tuv
        if FList[i]^.Notes <> '' then
        begin
          SubNode := XmlDoc.CreateElement('note');
          SubNode.TextContent := FList[i]^.Notes;
          Element.AppendChild(SubNode); // tuv
        end;
      Node.AppendChild(Element); // tu
    end;

    try
      WriteXMLFile(XmlDoc, FileName);
      Result := True;
    except
    end;
  finally
    XmlDoc.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWTMX.AddItem(const Original, Translated, Notes: String; const AllowDuplicate: Boolean = False);
var
  Item: PUWTMXItem;
  i: Integer;
begin
  if (Original = '') or (Translated = '') then Exit;

  if not AllowDuplicate and (FList.Count > 0) then
    for i := 0 to FList.Count-1 do
      if FList.Items[i]^.Original = Original then Exit;

  New(Item);
  Item^.Original   := Original;
  Item^.Translated := Translated;
  Item^.Notes      := Notes;
  FList.Add(Item)
end;

// -----------------------------------------------------------------------------

function TUWTMX.FindSimilary(const AText: String): Integer;
var
  i: Integer;
  Percent: Double;
begin
  Result := 0;
  FMap.Clear;

  if (FList.Count > 0) and (AText <> '') then
  begin
    for i := 0 to FList.Count-1 do
    begin
      Percent := StringSimilarityRatio(AText, FList.Items[i]^.Original, False);
      if Percent > 0.69 then
      begin
        FMap.Add(Percent, i);
      end;
    end;

    Result := FMap.Count;
  end;
end;

// -----------------------------------------------------------------------------

function TUWTMX.ItemFromMap(const AIndex: Integer): TUWTMXItem;
begin
  FillByte(Result, SizeOf(TUWTMXItem), 0);
  if (FMap.Count > 0) and InRange(AIndex, 0, FMap.Count-1) then
    Result := FList[ FMap.Data[AIndex] ]^;
end;

// -----------------------------------------------------------------------------

end.
