{*
 *  URUWorks Subtitle Workshop
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

unit UWGlossary;

// -----------------------------------------------------------------------------

interface

uses Classes, SysUtils, FGL;

type

  { TUWGlossaryHeader }

  PUWGlossaryHeader = ^TUWGlossaryHeader;
  TUWGlossaryHeader = packed record
    Author,                   //# Aldo <yo@email.com>, 2022.
    msgid,                    //msgid ""
    msgstr,                   //msgstr ""
    ProjectID,                //"Project-Id-Version: PACKAGE VERSION\n"
    Report,                   //"Report-Msgid-Bugs-To: \n"
    CreationDate,             //"POT-Creation-Date: 2022-04-18 13:45-0300\n"
    RevisionDate,             //"PO-Revision-Date: 2022-04-18 17:16-0300\n"
    LastTranslator,           //"Last-Translator: Aldo <text@test.com>\n"
    LanguageTeam,             //"Language-Team: Equipo\n"
    Language,                 //"Language: es\n"
    MIMEVersion,              //"MIME-Version: 1.0\n"
    ContentType,              //"Content-Type: text/plain; charset=utf-8\n"
    ContentTransfer,          //"Content-Transfer-Encoding: 8bit\n"
    PluralForms,              //"Plural-Forms: nplurals=2; plural=(n != 1);\n"
    XGenerator     : String;  //"X-Generator: Subtitle Workshop 7.0\n"
  end;

  { TUWGlossaryItem }

  PUWGlossaryItem = ^TUWGlossaryItem;
  TUWGlossaryItem = packed record
    Original   : String;
    Translated : String;
    Comments   : String;
  end;

  { TUWGlossaryList }

  TUWGlossaryList = specialize TFPGList<PUWGlossaryItem>;

  { TUWGlossary }

  TUWGlossary = class
  private
    FHeader : TUWGlossaryHeader;
    FList   : TUWGlossaryList;
  public
    constructor Create(const FileName: String);
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: String);
    property Items: TUWGlossaryList read FList;
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

constructor TUWGlossary.Create(const FileName: String);
begin
  FillByte(FHeader, SizeOf(TUWGlossaryHeader), 0);
  FList := TUWGlossaryList.Create;
  if FileName <> '' then LoadFromFile(FileName);
end;

// -----------------------------------------------------------------------------

destructor TUWGlossary.Destroy;
var
  i: Integer;
begin
  if FList.Count > 0 then
  begin
    for i := FList.Count-1 downto 0 do
    begin
      Dispose(PUWGlossaryItem(FList.Items[i]));
      FList.Items[i] := NIL;
      FList.Delete(i);
    end;
  end;

  FList.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWGlossary.LoadFromFile(const FileName: String);
var
  sl   : TStrings;
  i    : Integer;
  Item : PUWGlossaryItem;
begin
  if not FileExists(FileName) then Exit;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    FList.Clear;

    if sl.Count > 15 then
    begin
      with FHeader do
      begin
        Author          := sl[0];
        msgid           := sl[1];
        msgstr          := sl[2];
        ProjectID       := sl[3];
        Report          := sl[4];
        CreationDate    := sl[5];
        RevisionDate    := sl[6];
        LastTranslator  := sl[7];
        LanguageTeam    := sl[8];
        Language        := sl[9];
        MIMEVersion     := sl[10];
        ContentType     := sl[11];
        ContentTransfer := sl[12];
        PluralForms     := sl[13];
        XGenerator      := sl[14];
      end;
      for i := 0 to 14 do sl.Delete(0);
    end;

    if sl.Count > 0 then
    begin
      i := 0;
      while i < sl.Count do
      begin
        if sl[i] = '' then Inc(i);

        New(Item);
        //FillByte(Item, SizeOf(PUWGlossaryItem), 0);
        Item^.Comments := '';
        while (i < sl.Count) and (sl[i] <> '') do
        begin
          with Item^ do
          begin
            if sl[i].StartsWith('#') then
              if Comments = '' then
                Comments := Copy(sl[i], 3, Length(sl[i])-2)
              else
                Comments := Comments + LineEnding + Copy(sl[i], 3, Length(sl[i])-2);

            if sl[i].StartsWith('msgid') then Original := Copy(sl[i], 8, Length(sl[i])-8)
            else if sl[i].StartsWith('msgstr') then Translated := Copy(sl[i], 9, Length(sl[i])-9);
          end;
          inc(i);
        end;
        FList.Add(Item);
      end;
    end;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
