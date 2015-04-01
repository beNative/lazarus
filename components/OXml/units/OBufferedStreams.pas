unit OBufferedStreams;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OBufferedStreams.pas

  TOBufferedReadStream -> read streams with buffer.
    Can be used e.g. for reading data from a file stream.
    Supports seeking within the temp buffer range and also outside the temp buffer.

  TOBufferedWriteStream -> write data to a destination stream with buffer.
    E.g. to a file stream.
}

{$I OXml.inc}

{$IFDEF O_DELPHI_XE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$BOOLEVAL OFF}

interface

uses
  {$IFDEF O_NAMESPACES}
  System.SysUtils, System.Classes,
  {$ELSE}
  SysUtils, Classes,
  {$ENDIF}

  OWideSupp;

const
  OBUFFEREDSTREAMS_DEFBUFFERSIZE = 16*1024;//16 KB
  OBUFFEREDSTREAMS_DEFCHARBUFFERSIZE = OBUFFEREDSTREAMS_DEFBUFFERSIZE div SizeOf(OWideChar);//16 KB

type

  TOBufferedWriteStream = class(TStream)
  private
    fStream: TStream;
    fOwnsStream: Boolean;
    fStreamPosition: OStreamInt;
    fStreamSize: OStreamInt;

    fTempBuffer: TBytes;
    fTempBufferUsedLength: Integer;
    fBufferSize: Integer;
  protected
    function GetSize: OStreamInt; {$IFNDEF O_DELPHI_6_DOWN}override;{$ENDIF}
  public
    constructor Create(const aStream: TStream; const aOwnsStream: Boolean = False;
      const aBufferSize: Integer = OBUFFEREDSTREAMS_DEFBUFFERSIZE);
    destructor Destroy; override;

    function Write(const Buffer; Count: LongInt): LongInt; override;
    {$IFDEF O_DELPHI_XE3_UP}
    function Write(const Buffer: TBytes; Offset, Count: LongInt): LongInt; override;
    {$ENDIF}

    function Read(var {%H-}Buffer; {%H-}Count: LongInt): LongInt; override;
    {$IFDEF O_DELPHI_XE3_UP}
    function Read(Buffer: TBytes; Offset, Count: LongInt): LongInt; override;
    {$ENDIF}

    {$IFNDEF O_DELPHI_5_DOWN}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    {$ENDIF}
  public
    //write the whole temporary buffer to the destination stream
    procedure EnsureTempBufferWritten;
  end;

  TOBufferedReadStream = class(TStream)
  private
    fStream: TStream;
    fOwnsStream: Boolean;
    fStreamPosition: OStreamInt;
    fStreamSize: OStreamInt;
    fTempBuffer: TBytes;
    fTempBufferPosition: Integer;
    fTempBufferUsedLength: Integer;
    fBlockFlushTempBuffer: Integer;
    fBufferSize: Integer;

    procedure CheckTempBuffer;
  protected
    function GetSize: OStreamInt; {$IFNDEF O_DELPHI_6_DOWN}override;{$ENDIF}
  public
    constructor Create(const aStream: TStream; const aOwnsStream: Boolean = False;
      const aBufferSize: Integer = OBUFFEREDSTREAMS_DEFBUFFERSIZE);
    destructor Destroy; override;

    function Write(const {%H-}Buffer; {%H-}Count: LongInt): LongInt; override;
    {$IFDEF O_DELPHI_XE3_UP}
    function Write(const Buffer: TBytes; Offset, Count: LongInt): LongInt; override;
    {$ENDIF}

    function Read(var Buffer; Count: LongInt): LongInt; override;
    {$IFDEF O_DELPHI_XE3_UP}
    function Read(Buffer: TBytes; Offset, Count: LongInt): LongInt; override;
    {$ENDIF}

    {$IFNDEF O_DELPHI_5_DOWN}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    {$ENDIF}
  public
    //disallow clearing temporary buffer -> use if you need to seek back within the temp buffer.
    //use if you read from original streams that do not support seeking (e.g. a zip stream).
    procedure BlockFlushTempBuffer;
    procedure UnblockFlushTempBuffer;
  end;

  EOBufferedWriteStream = class(Exception);
  EOBufferedReadStream = class(Exception);

implementation

var
  OBufferedStreams_NilStream: OWideString = 'The aStream parameter must be assigned when creating a buffered stream.';
  OBufferedStreams_ReadingNotPossible: OWideString = 'You can''t read from TOBufferedWriteStream';
  OBufferedStreams_SeekingNotPossible: OWideString = 'You can''t use seek in TOBufferedWriteStream';
  OBufferedStreams_WritingNotPossible: OWideString = 'You can''t write to TOBufferedReadStream';

{ TOBufferedWriteStream }

constructor TOBufferedWriteStream.Create(const aStream: TStream;
  const aOwnsStream: Boolean; const aBufferSize: Integer);
begin
  inherited Create;

  if not Assigned(aStream) then
    raise EOBufferedWriteStream.Create(OBufferedStreams_NilStream);

  fStream := aStream;
  fStreamPosition := fStream.Position;
  fStreamSize := fStream.Size;
  fOwnsStream := aOwnsStream;

  fBufferSize := aBufferSize;
  SetLength(fTempBuffer, fBufferSize);
end;

destructor TOBufferedWriteStream.Destroy;
begin
  EnsureTempBufferWritten;

  inherited;
end;

procedure TOBufferedWriteStream.EnsureTempBufferWritten;
begin
  if fTempBufferUsedLength > 0 then
  begin
    fStream.WriteBuffer(fTempBuffer[0], fTempBufferUsedLength);
    fStreamSize := fStreamSize + fTempBufferUsedLength;
    fStreamPosition := fStreamPosition + fTempBufferUsedLength;
    fTempBufferUsedLength := 0;
  end;
end;

function TOBufferedWriteStream.GetSize: OStreamInt;
begin
  Result {%H-}:= fStreamSize + fTempBufferUsedLength;
end;

function TOBufferedWriteStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  {$IFDEF FPC}
  Result := 0;//JUST TO OMIT WARNING MESSAGES
  {$ENDIF}
  raise EOBufferedWriteStream.Create(OBufferedStreams_ReadingNotPossible);
end;

{$IFNDEF O_DELPHI_5_DOWN}
function TOBufferedWriteStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TOBufferedWriteStream.Seek(Offset: Integer; Origin: Word): LongInt;
{$ENDIF}
begin
  if (Origin = soCurrent) and (Offset = 0) then
  begin
    Result := fStreamPosition + fTempBufferUsedLength;
  {$IFDEF O_DELPHI_6_DOWN}
  //BECAUSE OF GetSize function!!!
  end else
  if (Origin = soEnd) and (Offset = 0) then
  begin
    Result := GetSize;
  end else
  if (Origin = soBeginning) and (Offset = fStreamPosition + fTempBufferUsedLength) then
  begin//CURRENT POSITION
    Result := fStreamPosition + fTempBufferUsedLength;
  {$ENDIF}
  end else
  begin
    raise EOBufferedWriteStream.Create(OBufferedStreams_SeekingNotPossible);
  end;
end;

{$IFDEF O_DELPHI_XE3_UP}
function TOBufferedWriteStream.Read(Buffer: TBytes; Offset,
  Count: LongInt): LongInt;
begin
  Result := Self.Read(Buffer[Offset], Count);
end;
{$ENDIF}

function TOBufferedWriteStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result := Count;
  if fTempBufferUsedLength+Result > fBufferSize then
    EnsureTempBufferWritten;//WRITE TEMP BUFFER

  if Result > fBufferSize then
  begin
    //count to write bigger then buffer -> write directly
    fStream.WriteBuffer(Buffer, Result);
  end else if Result > 0 then
  begin
    //store to temp!
    if Result > fBufferSize-fTempBufferUsedLength then//store only what has space in buffer
      Result := fBufferSize-fTempBufferUsedLength;

    Move(Buffer, fTempBuffer[fTempBufferUsedLength], Result);
    fTempBufferUsedLength := fTempBufferUsedLength + Result;
  end;

  {$IFNDEF O_DELPHI_XE2_UP}
  //older delphi versions need to return Result = Count !!!
  if (0 < Result) and (Result < Count) then
  begin
    Result := Write({%H-}Pointer({%H-}ONativeUInt(@Buffer)+ONativeUInt(Result))^, Count-Result) + Result;
  end;
  {$ENDIF}
end;

{$IFDEF O_DELPHI_XE3_UP}
function TOBufferedWriteStream.Write(const Buffer: TBytes; Offset,
  Count: LongInt): LongInt;
begin
  Result := Self.Write(Buffer[Offset], Count);
end;
{$ENDIF}

{ TOBufferedReadStream }

procedure TOBufferedReadStream.BlockFlushTempBuffer;
begin
  Inc(fBlockFlushTempBuffer);
end;

procedure TOBufferedReadStream.CheckTempBuffer;
var
  xReadBytes, xRemainingTempLength, xNewLength: ONativeInt;
begin
  if fTempBufferPosition = fTempBufferUsedLength then
  begin
    //we reached end of the tempbuffer, clear or grow and read from stream
    if fBlockFlushTempBuffer = 0 then
    begin
      fTempBufferPosition := 0;
      fTempBufferUsedLength := 0;
    end;

    xReadBytes := fBufferSize;
    if xReadBytes > fStreamSize-fStreamPosition then//don't read from stream more than possible
      xReadBytes := fStreamSize-fStreamPosition;

    //CHECK THAT WE HAVE ALL NECESSARY BYTES IN TEMP BUFFER
    xRemainingTempLength := Length(fTempBuffer)-fTempBufferPosition;
    if xRemainingTempLength < xReadBytes then
    begin
      //tempbuffer has to grow
      xNewLength := Length(fTempBuffer) + xReadBytes - xRemainingTempLength;
      SetLength(fTempBuffer, xNewLength);
    end;

    fStream.ReadBuffer(fTempBuffer[fTempBufferPosition], xReadBytes);
    fStreamPosition := fStreamPosition + xReadBytes;
    fTempBufferUsedLength := fTempBufferUsedLength + xReadBytes;
  end;
end;

constructor TOBufferedReadStream.Create(const aStream: TStream;
  const aOwnsStream: Boolean; const aBufferSize: Integer);
begin
  inherited Create;

  if not Assigned(aStream) then
    raise EOBufferedReadStream.Create(OBufferedStreams_NilStream);

  fStream := aStream;
  fStreamPosition := fStream.Position;
  fStreamSize := fStream.Size;
  fOwnsStream := aOwnsStream;

  fBufferSize := aBufferSize;

  SetLength(fTempBuffer, fBufferSize);
end;

destructor TOBufferedReadStream.Destroy;
begin
  inherited Destroy;

  if fOwnsStream then
    fStream.Free;
end;

function TOBufferedReadStream.GetSize: OStreamInt;
begin
  Result := fStreamSize;
end;

{$IFDEF O_DELPHI_XE3_UP}
function TOBufferedReadStream.Read(Buffer: TBytes; Offset,
  Count: LongInt): LongInt;
begin
  Result := Self.Read(Buffer[Offset], Count);
end;
{$ENDIF}

function TOBufferedReadStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  if Count < 0 then
  begin
    Result := 0;
    Exit;
  end;

  CheckTempBuffer;

  Result := fTempBufferUsedLength - fTempBufferPosition;
  if Result > Count then
    Result := Count;

  if Result > 0 then
  begin
    Move(fTempBuffer[fTempBufferPosition], Buffer, Result);
    fTempBufferPosition := fTempBufferPosition + Result;
  end;

  {$IFNDEF O_DELPHI_XE2_UP}
  //older delphi versions need to return Result = Count !!!
  if (0 < Result) and (Result < Count) then
  begin
    Result := Read({%H-}Pointer({%H-}ONativeUInt(@Buffer)+ONativeUInt(Result))^, Count-Result) + Result;
  end;
  {$ENDIF}
end;

{$IFNDEF O_DELPHI_5_DOWN}
function TOBufferedReadStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TOBufferedReadStream.Seek(Offset: Integer; Origin: Word): LongInt;
{$ENDIF}
var
  xAbsolutePosition: OStreamInt;
begin
  if (Origin = soCurrent) and (Offset = 0) then
  begin
    //CURRENT POSITION
    Result := fStreamPosition - fTempBufferUsedLength + fTempBufferPosition;
  end else
  begin
    //SEEK TO POSITION AND CLEAR TEMP STREAM

    case Origin of
      soCurrent: xAbsolutePosition := fStreamPosition - fTempBufferUsedLength + fTempBufferPosition + Offset;
      soEnd: xAbsolutePosition := fStreamSize + Offset;
    else
      //soFromBeginning
      xAbsolutePosition := Offset;
    end;

    if (xAbsolutePosition >= (fStreamPosition - fTempBufferUsedLength))
    then begin
      //WITHIN TEMP RANGE
      fTempBufferPosition := xAbsolutePosition - (fStreamPosition - fTempBufferUsedLength);
      Result := fStreamPosition - fTempBufferUsedLength + fTempBufferPosition;
    end else
    begin
      //OUTSIDE TEMP RANGE, CLEAR TEMP STREAM
      Result := fStream.Seek(soFromBeginning, xAbsolutePosition);
      fStreamPosition := Result;
      fTempBufferUsedLength := 0;
      fTempBufferPosition := 0;
    end;
  end;
end;

procedure TOBufferedReadStream.UnblockFlushTempBuffer;
begin
  if fBlockFlushTempBuffer > 0 then
    Dec(fBlockFlushTempBuffer);
end;

{$IFDEF O_DELPHI_XE3_UP}
function TOBufferedReadStream.Write(const Buffer: TBytes; Offset,
  Count: LongInt): LongInt;
begin
  Result := Self.Write(Buffer[Offset], Count);
end;
{$ENDIF}

function TOBufferedReadStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  {$IFDEF FPC}
  Result := 0;//JUST TO OMIT WARNING MESSAGES
  {$ENDIF}
  raise EOBufferedReadStream.Create(OBufferedStreams_WritingNotPossible);
end;

end.
