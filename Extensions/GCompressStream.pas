unit GCompressStream;

interface

uses
  SysUtils, Classes;

const
  COMPRESS_MAGIC : Word = $0001;

procedure CompressStream(InStream, OutStream: TStream; InSize: LongInt);
procedure UnCompressStream(InStream, OutStream: TStream);

implementation

uses
  LZRW1KH;

{ TCompressStream }

procedure CompressStream(InStream, OutStream: TStream; InSize: LongInt);
var
  InBuffer, OutBuffer: BufferArray;
  CompressedSize, BytesRead, FinalPos, SizePos, TotalSize: LongInt;
begin
  TotalSize := 0;
  OutStream.WriteBuffer(COMPRESS_MAGIC, SizeOf(Word));
  SizePos := OutStream.Position;
  OutStream.WriteBuffer(TotalSize, SizeOf(TotalSize));
  while InSize > 0 do
  begin
    BytesRead := InStream.Read(InBuffer, SizeOf(InBuffer));
    CompressedSize := Compression(@InBuffer, @OutBuffer, BytesRead);
    OutStream.WriteBuffer(CompressedSize, SizeOf(CompressedSize));
    OutStream.WriteBuffer(OutBuffer, CompressedSize);
    TotalSize := TotalSize + CompressedSize + SizeOf(CompressedSize);
    InSize := InSize - BytesRead;
  end;
  FinalPos := OutStream.Position;
  OutStream.Position := SizePos;
  OutStream.WriteBuffer(TotalSize, SizeOf(TotalSize));
  OutStream.Position := FinalPos;
end;

procedure UnCompressStream(InStream, OutStream: TStream);
var
  InBuffer, OutBuffer: BufferArray;
  CompressedSize, UnCompressedSize, InSize: LongInt;
  Sig: Word;
begin
  InStream.ReadBuffer(Sig, SizeOf(Word));
  if Sig <> COMPRESS_MAGIC then
    raise Exception.Create('Compression Type error');
  InStream.ReadBuffer(InSize, SizeOf(InSize));
  while InSize > 0 do
  begin
    InStream.ReadBuffer(CompressedSize, SizeOf(CompressedSize));
    InStream.ReadBuffer(InBuffer, CompressedSize);
    UnCompressedSize := DeCompression(@InBuffer, @OutBuffer, CompressedSize);
    OutStream.WriteBuffer(OutBuffer, UnCompressedSize);
    InSize := InSize - CompressedSize - SizeOf(CompressedSize);
  end;
end;

end.
