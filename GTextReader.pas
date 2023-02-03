{-------------------------------------------------------------------------
 Module:    TGlobe Text file reader

 Comment:   Buffers and reads a text file line by line

 Classes:   TGlobeTextReader

 Author:    Graham Knight
 Email:     tglobe@tglobe.com
-------------------------------------------------------------------------}
{$I GLOBE5.INC}
unit GTextReader;

interface

uses
  SysUtils, Classes, GSysUtils;

const
  TEXTFILE_BUF_SIZE = 4096;

type
  //-------------------------------
  //Description
  //Buffered reader for text files.
  //-------------------------------
  TGTextFileReader = class(TGRoot)
  private
    FbEOT : Boolean;
    FTextBufferSize : integer;
    FDataStream: TStream;
    FSize : integer;
    BufEnd, BufPos: integer;
    FiPosition : integer;
    FiLineStartPosition : integer;

    function GetSize : integer;
    procedure SetPosition(iPosition: integer);
    procedure SetTextBufferSize( Value : integer );
  public
    //-------------------------------------------------------------
    //Description
    //The IO buffer used to read blocks of data from the text file.
    //-------------------------------------------------------------
    TextBuffer: PChar;
    constructor Create( aStream : TStream );
    destructor Destroy; override;

    procedure ReadLn(var Line: String);

    //----------------------------------------
    //Description
    //The underlying stream for the text file.
    //----------------------------------------
    property DataStream: TStream read FDataStream;
    //-----------------------------------------------------------
    //Description
    //\Returns true if the End Of the Text file has been reached.
    //-----------------------------------------------------------
    property EOT: Boolean read FbEOT;
    //-----------------------------------------------------------------
    //Description
    //The offset of the start of the current line within the text file.
    //-----------------------------------------------------------------
    property LineStartPosition: integer read FiLineStartPosition;
    //---------------------------------------------
    //Description
    //The current position inside of the text file.
    //---------------------------------------------
    property Position: integer read FiPosition write SetPosition;
    //-----------------------------------
    //Description
    //\Returns the size of the text file.
    //-----------------------------------
    property Size : integer read GetSize;
    //-------------------------------------------------------------------------
    //Description
    //The size of the IO buffer used to read blocks of data from the text file.
    //-------------------------------------------------------------------------
    property TextBufferSize : integer read FTextBufferSize write SetTextBufferSize;
  end;

implementation


//---------------------------------------------------------------------------
//Description
//Creates the buffered text file reader, a open stream containing the data is
//passed to the object.
//
//Parameters
//aStream :  The stream to read from.                                        
//---------------------------------------------------------------------------
constructor TGTextFileReader.Create( aStream : TStream );
begin
  inherited Create;

  FSize := -1;

  FDataStream := aStream;

  SetTextBufferSize( TEXTFILE_BUF_SIZE );
end;

{------------------------------------------------------------------------------
  TGlobeTextReader.SetTextBufferSize
------------------------------------------------------------------------------}
{**
  @Param Value Size of the buffer to use when reading from the Stream.
} 
procedure TGTextFileReader.SetTextBufferSize(Value: integer);
begin
  if FTextBufferSize <> Value then
  begin
    FTextBufferSize := Value;

    FreeMem( TextBuffer );
    TextBuffer := AllocMem( FTextBufferSize + 1 );
    FiPosition := MaxInt;
    SetPosition( 0 );
  end;
end;


destructor TGTextFileReader.Destroy;
begin
  FDataStream := nil;

  FreeMem( TextBuffer );

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TGlobeTextReader.GetSize
------------------------------------------------------------------------------}
{**
  @Result The size of the input stream.
} 
function TGTextFileReader.GetSize : integer;
begin
  if FSize < 0 then
    FSize := DataStream.Size;
  Result := FSize;
end;

{------------------------------------------------------------------------------
  TGlobeTextReader.SetPosition
------------------------------------------------------------------------------}
{**
  @Param iPosition Position in the stream to start reading from.
}
procedure TGTextFileReader.SetPosition(iPosition: integer);
begin
  if ( FiPosition div FTextBufferSize ) <> ( iPosition div FTextBufferSize ) then
  begin
    DataStream.Position := ( iPosition div FTextBufferSize ) * FTextBufferSize;

    BufEnd := DataStream.Read(TextBuffer^, FTextBufferSize);
  end;

  BufPos := iPosition mod FTextBufferSize;
  FbEOT := BufEnd < BufPos;
  FiPosition := iPosition;
end;


//--------------------------------------------------------------------------
//Description
//Reads a single CR,LF terminated line from the data stream, the terminating
//charaters for the line are stripped from the string.                      
//--------------------------------------------------------------------------
procedure TGTextFileReader.ReadLn(var Line: String);
var
  P, Start: PChar;
  S: String;
begin
  Line := '';

  FiLineStartPosition := FiPosition; // remember start of this line
  repeat
    P := TextBuffer;
    Inc(P, BufPos);
    Start := P;

    while not (P^ in [#0, #10, #13]) do
      Inc(P);

    SetString(S, Start, P - Start);

    if P^ = #13 then
      Inc(P);
    Inc( FiPosition, P - Start );

    if P^ <> #0 then // if we have not hit the end of the Buffer
      Break;

    Line := Line + S;
    BufEnd := DataStream.Read(TextBuffer^, FTextBufferSize);
    TextBuffer[BufEnd] := #0;
    FbEOT := BufEnd = 0;
    BufPos := 0;
  until FbEOT;

  if P^ = #10 then
  begin
//    Inc(P);
    Position := Position + 1;
  end;
//  Inc( BufPos, P - Start );

  Line := Line + S;
end;

end.

