{*
 *  URUWorks Lazarus Waveform Display Control
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
 *  Based on VisualSubSync, Copyright (C) 2003 Christophe Paris.
 *  Copyright (C) 2003-2022 URUWorks.
 *
 *}

unit UWParser.Wave;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, Types, Math;

type
  TChunkType = array [1..4] of AnsiChar;

  PChunk = ^TChunk;
  TChunk = packed record
    cType  : TChunkType;
    dwSize : Cardinal;
    pData  : Pointer;
  end;

  { TUWWaveFileParser }

  TUWWaveFileParser = class
  private
    FFileName : String;
    Chunks    : TList;
  protected
    procedure SetFilename(const FileName: String); virtual;
    function GetChunksCount: Integer; virtual;
    function GetChunk(Index: Integer): PChunk; virtual;
    procedure ProcessFile; virtual;
    procedure ClearChunks; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetChunkByType(const ChunkType: TChunkType): PChunk; virtual;
    property FileName: String read FFileName write SetFileName;
    property ChunksCount: Integer read GetChunksCount;
    property Chunk[Index: Integer]: PChunk read GetChunk;
  end;

  { TWAVEFormatEx }

  PWAVEFormatEx = ^TWAVEFormatEx;
  TWAVEFormatEx = record
    wFormatTag      : Word;  { format type }
    nChannels       : Word;  { number of channels (i.e. mono, stereo, etc.) }
    nSamplesPerSec  : DWORD; { sample rate }
    nAvgBytesPerSec : DWORD; { for buffer estimation }
    nBlockAlign     : Word;  { block size of data }
    wBitsPerSample  : Word;  { number of bits per sample of mono data }
    cbSize          : Word;  { the count in bytes of the size of }
  end;

  { TUWWAVEFile }

  TUWWAVEFile = class
  private
    FFS         : TFileStream;
    FHeaderSize : Integer;
    FDataSize   : Integer;
    FWFX        : TWAVEFormatEx;
    FIsOpen     : Boolean;
    function GetSamplesPerSecond: Cardinal;
    function GetChannels: Cardinal;
    function GetBitsPerSample: Cardinal;
    function GetDurationMs: Cardinal;
    function GetNbsSamples: Cardinal;
  public
    destructor Destroy; override;
    function Open(const FileName: String): Boolean;
    function Read(Buffer: Pointer; const Count: Integer): Integer;
    procedure Close;
    function GetWaveFormatEx: PWAVEFormatEx;
    procedure SeekMS(const PosMS: Cardinal);
    function ExtractToStream(const StartMS, StopMS: Integer; const Output: TStream): Integer;
  published
    property SamplesPerSecond : Cardinal read GetSamplesPerSecond;
    property Channels         : Cardinal read GetChannels;
    property BitsPerSample    : Cardinal read GetBitsPerSample;
    property Duration         : Cardinal read GetDurationMs;
    property SamplesCount     : Cardinal read GetNbsSamples;
    property IsOpen           : Boolean  read FIsOpen;
  end;

// -----------------------------------------------------------------------------

implementation

const
  RIFF_SIGNATURE = 'RIFF';
  WAVE_SIGNATURE = 'WAVE';

type
  TRIFFHeader = packed record
    cSignature : TChunkType;
    dwSize     : Cardinal;
    cType      : TChunkType;
  end;

//------------------------------------------------------------------------------

{ TUWWaveFileParser }

// -----------------------------------------------------------------------------

constructor TUWWaveFileParser.Create;
begin
  inherited Create;
  FFileName := '';
  Chunks    := TList.Create;
end;

// -----------------------------------------------------------------------------

destructor TUWWaveFileParser.Destroy;
begin
  ClearChunks;
  FreeAndNil(Chunks);
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWWaveFileParser.SetFileName(const Filename: String);
begin
  if FFilename <> FileName then
  begin
    ClearChunks;
    FFilename := FileName;
    ProcessFile;
  end; // if
end;

// -----------------------------------------------------------------------------

function TUWWaveFileParser.GetChunksCount: Integer;
begin
  Result := Chunks.Count;
end;

// -----------------------------------------------------------------------------

function TUWWaveFileParser.GetChunk(Index: Integer): PChunk;
begin
  Result := NIL;
  if (Index >= 0) and (Index < Chunks.Count) then Result := Chunks[Index];
end;

// -----------------------------------------------------------------------------

procedure TUWWaveFileParser.ProcessFile;
var
  WaveFile : TFileStream;
  Header   : TRIFFHeader;
  FChunk   : PChunk;
begin
  WaveFile := TFileStream.Create(FFileName, fmOpenRead + fmShareDenyWrite);
  try
    WaveFile.Read(Header, SizeOf(Header));
    if (AnsiCompareText(Header.cSignature, RIFF_SIGNATURE) = 0) and
       (AnsiCompareText(Header.cType, WAVE_SIGNATURE) = 0) then
    begin
      while WaveFile.Position < WaveFile.Size do
      begin
        FChunk := AllocMem(sizeof(TChunk));
        with FChunk^ do
        begin
          WaveFile.Read(cType, SizeOf(cType));
          WaveFile.Read(dwSize, SizeOf(dwSize));
          pData := AllocMem(dwSize);
          WaveFile.Read(pData^, dwSize);
        end; // with
        Chunks.Add(FChunk);
      end; // while
    end; // if
  finally
    FreeAndNil(WaveFile);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWWaveFileParser.ClearChunks;
var
  FChunk: PChunk;
begin
  while Chunks.Count > 0 do
  begin
    FChunk := Chunks[0];
    Chunks.Delete(0);
    if Assigned(FChunk^.pData) then FreeMem(FChunk^.pData);
    Dispose(PChunk(FChunk));
  end; // while
end;

// -----------------------------------------------------------------------------

function TUWWaveFileParser.GetChunkByType(const ChunkType: TChunkType): PChunk;
var
  iIndex: Integer;
begin
  Result := NIL;
  iIndex := 0;
  while iIndex < Chunks.Count do
    if AnsiCompareText(PChunk(Chunks[iIndex])^.cType, ChunkType) = 0 then
    begin
      Result := Chunks[iIndex];
      Break;
    end
    else
      iIndex := iIndex + 1;
end;

// -----------------------------------------------------------------------------

{ TUWWAVEFile }

// -----------------------------------------------------------------------------

destructor TUWWAVEFile.Destroy;
begin
  if Assigned(FFS) then
  begin
    FFS.Free;
    FFS := NIL;
  end;

  inherited;
end;

// -----------------------------------------------------------------------------

function TUWWAVEFile.Open(const FileName: String): Boolean;
var
  Buff  : array[0..3] of AnsiChar;
  Size,
  Size2 : Integer;
begin
  Result := False;

  if FIsOpen then Close;

  if not FileExists(FileName) then Exit;

  try
    FFS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
    Exit;
  end;

  // -- RIFF Header
  if not (FFS.Read(Buff, 4) = 4) or not (StrLComp(Buff,'RIFF', 4) = 0) then Exit;

  // Read size
  if not (FFS.Read(Size, 4) = 4) then Exit;

  if (Size + 8) <> FFS.Size then
  begin
    // File is damaged, loading may not work correctly...
  end;

  if not (FFS.Read(Buff, 4) = 4) or not (StrLComp(Buff, 'WAVE', 4) = 0) then Exit;

  // -- FORMAT Header
  if not (FFS.Read(Buff, 4) = 4) or not (StrLComp(Buff, 'fmt ', 4) = 0) then Exit;

  // Read WAVFORMATEX struct size
  if not FFS.Read(Size, 4) = 4 then Exit;

  // Read the WAVFORMATEX struct (discard any extra data)
  Size2 := Size - SizeOf(FWFX);
  Size  := Min(SizeOf(FWFX), Size);
  FillByte(FWFX, SizeOf(FWFX), 0);
  //ZeroMemory(@FWFX, SizeOf(FWFX));
  if not (FFS.Read(FWFX, Size) = Size) then Exit;

  // Skip extra data
  if Size2 > 0 then FFS.Seek(Size2, soFromCurrent);

  while FFS.Position < FFS.Size do
  begin
    // DATA Header?
    if not (FFS.Read(Buff, 4) = 4) then Exit;
    // Read data size
    if not (FFS.Read(FDataSize, 4) = 4) then Exit;
    // DATA FOUND!! break!
    if (StrLComp(Buff, 'data', 4) = 0) then Break;
    // Skip extra data
    if Size > 0 then FFS.Seek(FDataSize, soFromCurrent);
  end;

  FHeaderSize := FFS.Position;

  Result  := True;
  FIsOpen := True;
end;

// -----------------------------------------------------------------------------

function TUWWAVEFile.Read(Buffer: Pointer; const Count: Integer): Integer;
begin
  Result := FFS.Read(Buffer^, Count);
end;

// -----------------------------------------------------------------------------

procedure TUWWAVEFile.Close;
begin
  if Assigned(FFS) then
  begin
    FFS.Free;
    FFS := NIL;
  end;

  FillByte(FWFX, SizeOf(FWFX), 0);
  //ZeroMemory(@FWFX, SizeOf(FWFX));
  FHeaderSize := 0;
  FDataSize   := 0;
  FIsOpen     := False;
end;

// -----------------------------------------------------------------------------

function TUWWAVEFile.GetNbsSamples: Cardinal;
begin
  Result := FDataSize div (FWFX.wBitsPerSample div 8);
end;

// -----------------------------------------------------------------------------

function TUWWAVEFile.GetBitsPerSample: Cardinal;
begin
  Result := FWFX.wBitsPerSample;
end;

// -----------------------------------------------------------------------------

function TUWWAVEFile.GetChannels: Cardinal;
begin
  Result := FWFX.nChannels;
end;

// -----------------------------------------------------------------------------

function TUWWAVEFile.GetSamplesPerSecond: Cardinal;
begin
  Result := FWFX.nSamplesPerSec;
end;

// -----------------------------------------------------------------------------

function TUWWAVEFile.GetWaveFormatEx: PWaveFormatEx;
begin
  Result := @FWFX;
end;

// -----------------------------------------------------------------------------

function TUWWAVEFile.GetDurationMS: Cardinal;
var
  Tmp: Extended;
begin
  if IsOpen then
  begin
    Tmp    := (FDataSize / (FWFX.wBitsPerSample div 8) / FWFX.nChannels) * 1000;
    Result := Round(Tmp / FWFX.nSamplesPerSec);
  end
  else
    Result := 0
end;

// -----------------------------------------------------------------------------

procedure TUWWAVEFile.SeekMS(const PosMS: Cardinal);
var
  StartOffsetBytes: Cardinal;
begin
  StartOffsetBytes := Round(PosMS / 1000 * FWFX.nAvgBytesPerSec);
  // Make sure we are aligned on samples
  StartOffsetBytes := StartOffsetBytes - (StartOffsetBytes mod FWFX.nBlockAlign);
  FFS.Seek(Cardinal(FHeaderSize) + StartOffsetBytes,soFromBeginning);
end;

// -----------------------------------------------------------------------------

function TUWWAVEFile.ExtractToStream(const StartMS, StopMS: Integer; const Output : TStream): Integer;
var
  StartOffsetBytes, StopOffsetBytes, LengthBytes, TotalLen : Cardinal;
  WFXLen : Cardinal;
begin
  StartOffsetBytes := Round(StartMs / 1000 * FWFX.nAvgBytesPerSec);
  StartOffsetBytes := StartOffsetBytes - (StartOffsetBytes mod FWFX.nBlockAlign);
  StopOffsetBytes  := Round(StopMs / 1000 * FWFX.nAvgBytesPerSec);
  StopOffsetBytes  := StopOffsetBytes - (StopOffsetBytes mod FWFX.nBlockAlign);
  LengthBytes      := StopOffsetBytes - StartOffsetBytes;

  TotalLen := Length('RIFF') + SizeOf(TotalLen) + Length('WAVEfmt ') +
    SizeOf(WFXLen) + SizeOf(FWFX) + Length('data') + SizeOf(LengthBytes) +
    LengthBytes;

  Result := TotalLen;

  // Write WAV header
  Output.Write('RIFF', 4);
  TotalLen := TotalLen - 8; // don't include 'RIFF'+Size in the total size
  Output.Write(TotalLen, SizeOf(TotalLen));

  Output.Write('WAVEfmt ', 8);
  WFXLen := SizeOf(FWFX);
  Output.Write(WFXLen, SizeOf(WFXLen));
  Output.Write(FWFX, SizeOf(FWFX));
  Output.Write('data', 4);
  Output.Write(LengthBytes, SizeOf(LengthBytes));

  // Write PCM data
  FFS.Seek(Cardinal(FHeaderSize) + StartOffsetBytes, soFromBeginning);
  Output.CopyFrom(FFS, LengthBytes);
end;

// -----------------------------------------------------------------------------

end.
