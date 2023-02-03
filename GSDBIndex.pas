//-----------------------------------------------------------------------
// Summary
//	TGlobe SpatialDatabase index base classes
//
// Description
// 	Provides a base class for spatial indexing.
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
{$I GLOBE5.INC}
unit GSDBIndex;

interface

Uses Classes, GSDBCache, GSDBTables, GClasses, GSysUtils;

type
  
  TGSpatialIndex = class( TGSDBObject )
  public
    function Count : Cardinal; virtual; abstract;
    function IndexMER : TGMER; virtual; abstract;

    procedure Insert( const mer : TGMER; objPK : TGPrimaryKey ); virtual; abstract;
    procedure Delete( const mer : TGMER; objPK : TGPrimaryKey ); virtual; abstract;

    procedure Search( const searchMER : TGMER; minSize: integer; resultList : TList ); overload; virtual; abstract;
    procedure Search( const searchMER : TGMER; minSize: integer; callback : TGSearchCallback ); overload; virtual; abstract;
  end;


  TGMemoryIndexRec = record
    MER : TGMER;
    objPK : TGPrimaryKey;
  end;

  TGMemoryIndex = class( TGSpatialIndex )
  private
    FCount : integer;
    FIndexArray : array of TGMemoryIndexRec;
  protected
    procedure InternalLoad( aStream : TMemoryStream ); override;
    procedure InternalSave( aStream : TMemoryStream ); override;
  public
    function Count : Cardinal; override;
    function IndexMER : TGMER; override;

    procedure Clear; override;
    procedure Insert( const mer : TGMER; objPK : TGPrimaryKey ); override;
    procedure Delete( const mer : TGMER; objPK : TGPrimaryKey ); override;

    procedure Search( const searchMER : TGMER; minSize: integer; resultList : TList ); override;
    procedure Search( const searchMER : TGMER; minSize: integer; callback : TGSearchCallback ); override;
  end;

implementation


function TGMemoryIndex.Count: Cardinal;
begin
  Result := FCount;
end;


procedure TGMemoryIndex.Clear;
begin
  FCount := 0;
  SetLength( FIndexArray, 0 );
end;

procedure TGMemoryIndex.Delete(const mer: TGMER; objPK: TGPrimaryKey);
var
  idx : integer;
begin
  for idx := 0 to FCount - 1 do
    if FIndexArray[idx].objPK = objPK then
    begin
      if ( FCount - idx ) > 1 then
        Move( FIndexArray[idx + 1], FIndexArray[idx], SizeOf(TGMemoryIndexRec) * (FCount - idx - 1 ));
      Dec( FCount );
      Exit;
    end;
end;

function TGMemoryIndex.IndexMER: TGMER;
var
  idx : integer;
begin
  MER_Empty( Result );

  for idx := 0 to FCount - 1 do
    Result := MER_Union(Result, FIndexArray[idx].MER);
end;

procedure TGMemoryIndex.Insert(const mer: TGMER; objPK: TGPrimaryKey);
begin
  if FCount = Length( FIndexArray ) then
    SetLength( FIndexArray, Length( FIndexArray ) + 256 );

  FIndexArray[FCount].MER := mer;
  FIndexArray[FCount].objPK := objPK;
  Inc( FCount );
end;

procedure TGMemoryIndex.Search( const searchMER: TGMER; minSize: integer; resultList: TList);
var
  idx : integer;
begin
  resultList.Clear;

  for idx := 0 to FCount - 1 do
    if MER_Intersect( searchMER, FIndexArray[idx].MER ) then
      resultList.Add( Pointer(FIndexArray[idx].objPK) );
end;

procedure TGMemoryIndex.InternalLoad(aStream: TMemoryStream);
begin
  // Does nothing
end;

procedure TGMemoryIndex.InternalSave(aStream: TMemoryStream);
begin
  // Does nothing
end;

procedure TGMemoryIndex.Search( const searchMER: TGMER; minSize: integer;
  callback: TGSearchCallback);
var
  idx : integer;
  abort : Boolean;
begin
  abort := false;
  for idx := 0 to FCount - 1 do
    if MER_Intersect( searchMER, FIndexArray[idx].MER ) then
    begin
      callback( FIndexArray[idx].objPK, abort );
      if abort then
        Exit;
    end;
end;

end.
