//-----------------------------------------------------------------------
// Summary
//	TGlobe SpatialDatabase key index classes
//
// Description
// 	Provides a simple key based index for layer metadata.
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
{$I GLOBE5.INC}

unit GSDBKeyIndex;

interface

Uses  Classes, Windows, SysUtils, GSDBCache, GSysUtils, GSDBTables, GXML;

type
  //----------------------------------------------------------------------------
  //Description
  //\Internal Spatial Database class which provides a simple key based index for
  //layer metadata properties in the spatial database.                          
  //----------------------------------------------------------------------------
  TGSDBKeyIndex = class( TGSDBTable )
  private
    FKeyCache : TStringList;
  protected
    function Locate( const key : String; memStream : TMemoryStream ) : TGRowID;
  public
    destructor Destroy; override;
    procedure InsertKey( const key : String; rowID : TGRowID );
    procedure DeleteKey( const key : String );
    procedure UpdateKey( const key : String; rowID : TGRowID );
    function FindKey( const key : String ) : TGRowID;
  end;

implementation

uses GLogging;



//-----------------------------
//Description
//Deletes a key from the index.
//-----------------------------
procedure TGSDBKeyIndex.DeleteKey(const key: String);
var
  memStream : TMemoryStream;
  rowID : TGRowID;
begin
  memStream := TMemoryStream.Create;
  try
    rowID := Locate( key, memStream );
    if rowID <> 0 then
    begin
      DeleteRow(rowID);
      FreeAndNil( FKeyCache );
    end;
  finally
    memStream.Free;
  end;
end;

destructor TGSDBKeyIndex.Destroy;
begin
  FreeAndNil( FKeyCache );
  inherited;
end;

//------------------------------------------------------------------------------
//Description
//Finds a key in the index and returns the RowID associated with the key or 0 if
//the key is not found.                                                         
//------------------------------------------------------------------------------
function TGSDBKeyIndex.FindKey( const key : String ): TGRowID;
var
  memStream : TMemoryStream;
  rowID : TGRowID;
begin
  Result := 0;
  memStream := TMemoryStream.Create;
  try
    rowID := Locate( key, memStream );
    if rowID <> 0 then
    begin
      memStream.Position := Length( PChar( memStream.Memory )) + 1;
      memStream.Read( Result, SizeOf( TGRowID ));
    end;
  finally
    memStream.Free;
  end;
end;

//-------------------------------------------------------
//Description
//Inserts a key and it's associated rowID into the index.
//-------------------------------------------------------
procedure TGSDBKeyIndex.InsertKey(const key: String; rowID: TGRowID);
var
  memStream : TMemoryStream;
begin
  Assert( FindKey( key ) = 0 );

  memStream := TMemoryStream.Create;
  try
    memStream.Write( PChar(key)^, Length( key ) + 1);
    memStream.Write( rowID, SizeOf(TGRowID));
    AppendStream(memStream);
    
    FreeAndNil( FKeyCache );
  finally
    memStream.Free;
  end;
end;

function TGSDBKeyIndex.Locate(const key: String; memStream: TMemoryStream): TGRowID;
var
  idx : integer;
begin
  if FKeyCache = nil then
  begin
    FKeyCache := TStringList.Create;
    FKeyCache.Sorted := true;
    Result := First;
    while Result <> 0 do
    begin
      ReadStream(Result,memStream);
      FKeyCache.AddObject( PChar( memStream.Memory ), TObject( Result ));
      Result := Next( Result );
    end;
  end;

  idx := FKeyCache.IndexOf(key);
  if idx < 0 then
    Result := 0
  else
  begin
    Result := TGRowID( FKeyCache.Objects[idx] );
    ReadStream(Result,memStream);
  end;
end;

//-------------------------------------------------------
//Description
//Updates the RowID associated with the key in the index.
//-------------------------------------------------------
procedure TGSDBKeyIndex.UpdateKey(const key: String; rowID: TGRowID);
var
  memStream : TMemoryStream;
  locID : TGRowID;
begin
  memStream := TMemoryStream.Create;
  try
    locID := Locate( key, memStream );
    if locID <> 0 then
    begin
      memStream.Position := Length( PChar( memStream.Memory )) + 1;
      memStream.Write( rowID, SizeOf( TGRowID ));

      UpdateStream(locID, memStream );
      FreeAndNil( FKeyCache );
    end
    else
      InsertKey( key, rowID );
  finally
    memStream.Free;
  end;
end;

initialization
  RegisterSDBClass( TGSDBKeyIndex, '' );
  RegisterSDBClass( TGSDBKeyIndex, 'TGKeyIndex' );
end.
