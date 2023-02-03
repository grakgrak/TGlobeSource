//-----------------------------------------------------------------------
// Summary
//	TGlobe DBF file reader
//
// Description
// 	Reads records from a DBF format file
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
{$I GLOBE5.INC}
unit GDBFReader;

interface

uses
  SysUtils, Classes;


type
  EDBFException = class(Exception);

  TDBFField = record
    FName : array [0..10] of char;
    FType : char;
    FData : PChar;
    FLen  : Byte;
    FCount : Byte;
    FReserved : array [0..13] of char;
  end;
  TDBFFieldArray = array [0..255] of TDBFField;
  PTDBFFieldArray = ^TDBFFieldArray;

  
  TGDBFFileReader = class
  private
    iRecordID : integer;
    iHeaderLen : SmallInt;
    iRecordLen : SmallInt;
    DBFstream : TStream;
    Data : PChar;
    function GetFieldName( iField : integer ) : String;
    function GetFieldType( iField : integer ) : Char;
    function GetFieldLength( iField : integer ) : integer;
    function GetString( iField : integer ) : String;
    procedure SetID( iRecord : integer );
  public
    
    iRecordCount : integer;
    
    iFieldCount : integer;
    
    Fields : PTDBFFieldArray;

    constructor Create( const DBFfilename : TFilename );
    destructor Destroy; override;
    function First : Boolean;
    function Next : Boolean;

    function ColumnNames : String;
    function RowCSL : String;

    
    property FieldName[iField : integer] : String read GetFieldName;
    
    property FieldType[iField : integer] : char read GetFieldType;
    
    property FieldLength[iField : integer] : integer read GetFieldLength;

    
    property AsString[iField : integer] : String read GetString;
    
    property RecordID : integer read iRecordID write SetID;
  end;

{-------------------------------------------------------------------------}
implementation


constructor TGDBFFileReader.Create( const DBFfilename : TFilename );
var
  cTmp : Byte;
  idx, iTmp : integer;
begin
  DBFstream := TFileStream.Create( DBFfilename, fmOpenRead or fmShareDenyNone );
  DBFstream.Position := 0;
  DBFstream.Read( cTmp, 1 );
  if ( cTmp <> $03 ) and ( cTmp <> $83 ) then
  begin
    DBFStream.Free;
    Raise EDBFException.Create( 'Unsupported DBase III version in Stream' );
  end;

  for idx := 1 to 3 do
    DBFstream.Read( cTmp, 1 );

  DBFstream.Read( iRecordCount, SizeOf( integer ));
  DBFstream.Read( iHeaderLen, SizeOf( SmallInt ));
  DBFstream.Read( iRecordLen, SizeOf( SmallInt ));
  GetMem( Data, iRecordLen );

  for idx := 1 to 20 do
    DBFstream.Read( cTmp, 1 );

  GetMem( Fields, iHeaderLen - 32 );
  DBFstream.Read( Fields^, iHeaderLen - 32 );

  iFieldCount := 0;
  iTmp := 1;
  repeat
    Fields^[iFieldCount].FData := Data + iTmp;
    Inc( iTmp, Fields^[iFieldCount].FLen );
    Inc( iFieldCount );
  until Ord( Fields^[iFieldCount].FName[0] ) in [$0, $0D];

  First;
end;


destructor TGDBFFileReader.Destroy;
begin
  if Data <> nil then
    FreeMem( Data, iRecordLen );
  if Fields <> nil then
    FreeMem( Fields, iHeaderLen - 32 );
  FreeAndNil( DBFstream );
  
  inherited Destroy;
end;


function TGDBFFileReader.First : Boolean;
begin
  iRecordID := 1;
  Result := DBFstream <> nil;
  if Result then
    DBFstream.Position := iHeaderLen;
end;


function TGDBFFileReader.Next : Boolean;
begin
  Result := False;
  if DBFstream <> nil then
  begin
    repeat
      Inc( iRecordID );
      if DBFstream.Read( Data^, iRecordLen ) <> iRecordLen then
        Exit;
    until Data[0] <> '*';
    Result := True;
  end;
end;


function TGDBFFileReader.GetFieldName( iField : integer ) : String;
begin
  Result := StrPas( Fields^[iField].FName );
end;


function TGDBFFileReader.GetFieldType( iField : integer ) : Char;
begin
  Result := Fields^[iField].FType;
end;


function TGDBFFileReader.GetFieldLength( iField : integer ) : integer;
begin
  Result := Fields^[iField].FLen;
end;


function TGDBFFileReader.GetString( iField : integer ) : string;
begin
  if iField < iFieldCount then
    Result := Trim( Copy( StrPas( Fields^[iField].FData ), 1, Fields^[iField].FLen ))
  else
    Result := '';
end;


procedure TGDBFFileReader.SetID( iRecord : integer );
begin
  if iRecord > 0 then
  begin
    Dec( iRecord );
    DBFstream.Position := iHeaderLen + iRecordLen * iRecord;
    DBFstream.Read( Data^, iRecordLen );
    iRecordID := iRecord;
  end;
end;


function TGDBFFileReader.ColumnNames: String;
var
  idx : integer;
  sl : TStringList;
begin
  sl := TStringList.Create;

  for idx := 0 to iFieldCount - 1 do
    sl.Add( FieldName[idx]);

  Result := sl.CommaText;

  sl.Free;
end;


function TGDBFFileReader.RowCSL: String;
var
  idx : integer;
  sl : TStringList;
begin
  sl := TStringList.Create;

  for idx := 0 to iFieldCount - 1 do
    sl.Add( AsString[idx] );

  Result := sl.CommaText;

  sl.Free;
end;

end.
