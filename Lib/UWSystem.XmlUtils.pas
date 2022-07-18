{*
 *  URUWorks XmlUtils
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

unit UWSystem.XmlUtils;

// -----------------------------------------------------------------------------

interface

uses
  laz2_XMLRead, laz2_DOM;

function XMLFindNodeByName(const XmlDoc: TXMLDocument; const NodeName: String): TDOMNode;
function XMLGetAttrValue(const ANode: TDOMNode; const AAttrName: String): String;
function XMLHasAttribute(const ANode: TDOMNode; const AAttrName: String): Boolean;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

function XMLFindNodeByName(const XmlDoc: TXMLDocument; const NodeName: String): TDOMNode;

  function FindNode(ANode: TDOMNode): TDOMNode;
  var
    i: Integer;
  begin
    Result := NIL;
    if Assigned(ANode) then
    begin
      if ANode.NodeName = NodeName then
        Result := ANode
      else
        for i := 0 to ANode.ChildNodes.Count-1 do
        begin
          Result := FindNode(ANode.ChildNodes[i]);
          if Assigned(Result) then Break;
        end;
    end;
  end;

var
  i: Integer;
begin
  Result := NIL;
  if Assigned(XmlDoc) and (XmlDoc.ChildNodes.Count > 0) then
  begin
    for i := 0 to XmlDoc.ChildNodes.Count-1 do
    begin
      Result := FindNode(XmlDoc.ChildNodes[i]);
      if Assigned(Result) then Break;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function XMLGetAttrValue(const ANode: TDOMNode; const AAttrName: String): String;
var
  i: LongWord;
  Found: Boolean;
begin
  Result := '';
  if (ANode = NIL) or (ANode.Attributes = NIL) then Exit;

  Found := False;
  i := 0;
  while not Found and (i < ANode.Attributes.Length) do
  begin
    if ANode.Attributes.Item[i].NodeName = AAttrName then
    begin
      Found := True;
      Result := ANode.Attributes.Item[i].NodeValue;
    end;
    Inc(i);
  end;
end;

// -----------------------------------------------------------------------------

function XMLHasAttribute(const ANode: TDOMNode; const AAttrName: String): Boolean;
begin
  Result := XMLGetAttrValue(ANode, AAttrName) <> '';
end;

// -----------------------------------------------------------------------------

end.
