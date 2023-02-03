{------------------------------------------------------------------------------
 Module:		TGSHPReader.pas

 Comment:   Reads ESRI .SHP file

 Classes:   TGSHPFileReader

 Author:	  Graham Knight
 Email:		  tglobe@tglobe.com
------------------------------------------------------------------------------}
{$I GLOBE5.INC}
unit GSHPReader;

interface

uses
  Windows, SysUtils, Classes, GDBFReader, Globe5, GSysUtils, GMapObjects,
  GDataReaders, GClasses, GXML;

const
  TG_SHP_READERVERSION = $0103;

type

  //------------------------------------------------------------------------
  //Description
  //Reads SHP files into a layer and converts the point data to Globe Units.
  //------------------------------------------------------------------------
  TGSHPFileReader = class(TGFileDataReader)
  private
    DBF : TGDBFFileReader;
    FSHPStream : TStream;
    FStreamSize : integer;

    FLongXDivisor : Double;
    FLatYDivisor : Double;
    FLongXShift : Double;
    FLatYShift : Double;
    FiDBFRecordID : integer;
    FMeasureValues : boolean;


    function CorrectX( X: Extended ): Extended;
    function CorrectY( Y: Extended ): Extended;

    function ReadBigEndianInt : integer;
    function ReadLittleEndianInt : integer;

    function ReadPointObject( hasZvalues : Boolean ) : TGMapPoint;
    function ReadMultiPointObject( hasZvalues : Boolean ) : TGMapPoint;
    function ReadPolyObject( bPolygon, hasZvalues : Boolean ) : TGMapPoly;
  protected
    function InternalOpen : Boolean; override;
    function InternalClose : Boolean; override;

    function ReadHeader : Boolean; override;
    function ReadObject : IGMapPoint; override;

    procedure LoadFromXMD( el : TGXML_Element ); override;
  public
    constructor Create( parentGlobe : TGCustomGlobe5 ); override;
    destructor Destroy; override;

    function ProgressPercent: Double; override;
  published
    //----------------------------------------------------------------------------
    //Description
    //Divisor for shape files that contain points which are not specified in
    //geographic lat,long units. This value is used convert the coordinates to lat
    //\long value.
    //----------------------------------------------------------------------------
    property LongXDivisor : Double read FLongXDivisor write FLongXDivisor;
    //----------------------------------------------------------------------------
    //Description
    //Divisor for shape files that contain points which are not specified in
    //geographic lat,long units. This value is used convert the coordinates to lat
    //\long value.
    //----------------------------------------------------------------------------
    property LatYDivisor : Double read FLatYDivisor write FLatYDivisor;
    //---------------------------------------------------------------------------
    //Description
    //Used to shift data data location if the coordinates are relative to a point
    //\other than 0,0.
    //---------------------------------------------------------------------------
    property LongXShift : Double read FLongXShift write FLongXShift;
    //---------------------------------------------------------------------------
    //Description
    //Used to shift data data location if the coordinates are relative to a point
    //\other than 0,0.
    //---------------------------------------------------------------------------
    property LatYShift : Double read FLatYShift write FLatYShift;
  end;

implementation

Uses GResource;

{-------------------------------------------------------------------------}
procedure SwapWord( iLength : integer; wordP : PChar );
var
	idx : integer;
	cTmp : char;
begin
	for idx := 0 to iLength div 2 - 1 do
	begin
		cTmp := wordP[idx];
		wordP[idx] := wordP[iLength - idx - 1];
		wordP[iLength - idx - 1] := cTmp;
	end;
end;


constructor TGSHPFileReader.Create( parentGlobe : TGCustomGlobe5 );
begin
  inherited;

  LongXDivisor := 1.0;
  LatYDivisor := 1.0;
  LongXShift := 0.0;
  LatYShift := 0.0;
end;

function TGSHPFileReader.CorrectX( X: Extended ): Extended;
begin
  Result := LongXShift + X / LongXDivisor;
end; // CorrectX

function TGSHPFileReader.CorrectY( Y: Extended ): Extended;
begin
  Result := LatYShift + Y / LatYDivisor;
end;


destructor TGSHPFileReader.Destroy;
begin
  FreeAndNil( DBF );
  FreeAndNil( FSHPStream );

  inherited;
end;


function TGSHPFileReader.ProgressPercent: Double;
begin
  if FSHPStream <> nil then
    Result := ( FSHPStream.Position / FStreamSize ) * 100.0
  else
    Result := 0.0;
end;

function TGSHPFileReader.ReadBigEndianInt : integer;
begin
	FSHPStream.Read( Result, SizeOf( integer ));
	SwapWord( 4, PChar( @Result ));
end;

function TGSHPFileReader.ReadLittleEndianInt : integer;
begin
	FSHPStream.Read( Result, SizeOf( integer ));
end;

function TGSHPFileReader.ReadPolyObject( bPolygon, hasZvalues : Boolean ) : TGMapPoly;
var
	idx, iParts, iChain : integer;
	eTmp, eX, eY : double;
	iPoints : integer;
  offsets : array of integer;
begin
  Result := TGMapPoly.Create;

  if bPolygon then
    Result.PresenterID := DEFAULT_POLYGON_PRESENTERID
  else
    Result.PresenterID := DEFAULT_POLYLINE_PRESENTERID;

	for idx := 0 to 3 do
		FSHPStream.Read( eTmp, SizeOf( double ));	{ read the box for the object }

	iParts := ReadLittleEndianInt;	{ Number of parts }
	iPoints := ReadLittleEndianInt;	{ Number of points }

  SetLength( offsets, iParts + 1 );
  offsets[iParts] := iPoints;

  // Read all the offsets
  for idx := 0 to iParts - 1 do
    offsets[idx] := ReadLittleEndianInt;

  Result.Chains.Count := iParts;
	for idx := 0 to iParts - 1  do
    Result.Chains[idx].Count := offsets[idx+1] - offsets[idx];

  iChain := 0;
	for idx := 0 to iPoints - 1 do
	begin
    if idx = offsets[iChain + 1] then
      Inc( iChain );
		FSHPStream.Read( eX, SizeOf( double ));	{ X }
		FSHPStream.Read( eY, SizeOf( double ));	{ Y }
    Result.Chains[iChain].AsLL[idx - offsets[iChain]] := DDToPointLL( CorrectX( eX ), CorrectY( eY ));
  end;

  Result.Closed := bPolygon;

  if hasZvalues then
  begin
    FSHPStream.Read( eTmp, SizeOf( Double )); // bounding Z range
    FSHPStream.Read( eTmp, SizeOf( Double ));
    for idx := 1 to iPoints do
      FSHPStream.Read( eTmp, SizeOf( Double )); // Z Values
  end;

  if FMeasureValues then
  begin
    FSHPStream.Read( eTmp, SizeOf( Double )); // bounding M range
    FSHPStream.Read( eTmp, SizeOf( Double ));
    for idx := 1 to iPoints do
      FSHPStream.Read( eTmp, SizeOf( Double )); // Measure Values
  end;
end;

function TGSHPFileReader.ReadPointObject( hasZvalues : Boolean ) : TGMapPoint;
var
  eX, eY, eTmp : Double;
begin
  Result := TGMapPoint.Create;

  Result.PresenterID := DEFAULT_POINT_PRESENTERID;

  FSHPStream.Read( eX, SizeOf( double ));	{ X }
  FSHPStream.Read( eY, SizeOf( double ));	{ Y }

  if hasZvalues then
    FSHPStream.Read( eTmp, SizeOf( Double )); // Z Values

  if FMeasureValues then
    FSHPStream.Read( eTmp, SizeOf( Double )); // Measure Values

  Result.Centroid := DDToPointLL( CorrectX( eX ), CorrectY( eY ));
end;

function TGSHPFileReader.ReadMultiPointObject( hasZvalues : Boolean ) : TGMapPoint;
var
	idx, iPoints : integer;
	eTmp, eX, eY : double;
begin
	for idx := 0 to 3 do
		FSHPStream.Read( eTmp, SizeOf( double ));	{ read the box for the object }
	iPoints := ReadLittleEndianInt;	{ Number of points }

  if iPoints = 1 then
  begin
    Result := TGMapPoint.Create;
    Result.PresenterID := DEFAULT_POINT_PRESENTERID;

    FSHPStream.Read( eX, SizeOf( Double ));	{ X }
    FSHPStream.Read( eY, SizeOf( Double ));	{ Y }
    Result.Centroid := DDToPointLL( CorrectX( eX ), CorrectY( eY ));
  end
  else
  begin
    Result := TGMapPoly.Create;
    Result.PresenterID := DEFAULT_POINT_PRESENTERID;

    for idx := 1 to iPoints do
    begin
      FSHPStream.Read( eX, SizeOf( Double ));	{ X }
      FSHPStream.Read( eY, SizeOf( Double ));	{ Y }
      TGMapPoly(Result).Chains[0].AddDD( CorrectX( eX ), CorrectY( eY ));
    end;
  end;

  if hasZvalues then
  begin
    FSHPStream.Read( eTmp, SizeOf( Double )); // bounding Z range
    FSHPStream.Read( eTmp, SizeOf( Double ));
    for idx := 1 to iPoints do
      FSHPStream.Read( eTmp, SizeOf( Double )); // Z Values
  end;

  if FMeasureValues then
  begin
    FSHPStream.Read( eTmp, SizeOf( Double )); // bounding M range
    FSHPStream.Read( eTmp, SizeOf( Double ));
    for idx := 1 to iPoints do
      FSHPStream.Read( eTmp, SizeOf( Double )); // Measure Values
  end;
end;


function TGSHPFileReader.InternalOpen : Boolean;
begin
  inherited InternalOpen;

  // Create the data stream
  Result := FileExists( Filename );
  if Result then
  begin
    Name := ExtractFilename( Filename );

    FSHPStream := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite );
    FStreamSize := FSHPStream.Size;

    if FileExists( ChangeFileExt( Filename, '.DBF' )) then
      DBF := TGDBFFileReader.Create(ChangeFileExt( Filename, '.DBF' ));

    if DBF <> nil then
      MetaData.Values['ColumnNames'] := DBF.ColumnNames;
      
    FiDBFRecordID := 1;
  end;
end;


function TGSHPFileReader.InternalClose;
begin
  inherited InternalClose;
  
  FreeAndNil( DBF );
  FreeAndNil( FSHPStream );

  Result := True;
end;

{------------------------------------------------------------------------------
  TGSHPFileReader.ReadHeader
------------------------------------------------------------------------------}
{**
  @Param aStream The data stream to read from.
  @Result True if the Header was correctly read
}
function TGSHPFileReader.ReadHeader: Boolean;
var
	idx : integer;
  eTmp, eTmp1 : Double;
begin
  FMeasureValues := true;

	if ReadBigEndianInt <> 9994 then
    raise EGException.CreateFmt( 'Invalid SHP file %s', [Filename] );

  for idx := 0 to 4 do
    ReadLittleEndianInt;

  ReadBigEndianInt; // Shape file size

  if ReadLittleEndianInt <> 1000 then	{ Version }
    raise EGException.CreateFmt( 'Invalid SHP file version in %s', [Filename] );

  ReadLittleEndianInt;	{ Shape File Type }

  for idx := 0 to 3 do  // read the bounding box
    FSHPStream.Read( eTmp, SizeOf( Double ));

  FSHPStream.Read( eTmp, SizeOf( double ));	// Z bounds
  FSHPStream.Read( eTmp1, SizeOf( double ));	// Z bounds

  FSHPStream.Read( eTmp, SizeOf( double ));	// M bounds
  FSHPStream.Read( eTmp1, SizeOf( double ));	// M bounds

  if not ( IsNan( eTmp ) or IsNan( eTmp1 )) then
  if ( eTmp <= -10e+38 ) or ( eTmp1 <= -10e+38 ) then
    FMeasureValues := false
  else
    if ( eTmp - eTmp1 ) = 0 then
      FMeasureValues := false;

  Result := True;
end;

{------------------------------------------------------------------------------
  TGSHPFileReader.ReadObject
------------------------------------------------------------------------------}
function TGSHPFileReader.ReadObject: IGMapPoint;
var
  objType : integer;
  iPosition : integer;
  iLen : integer;
begin
  inherited ReadObject;

  Result := nil;

  iPosition := FSHPStream.Position;
  while (Result = nil ) and ( iPosition < FStreamSize ) do
  begin
		ReadBigEndianInt;	{ record number }
    iLen := ReadBigEndianInt; { content length }

    objType := ReadLittleEndianInt;
		case objType of	{ ShapeType }
    0 : // nul
      Result := nil;  // Do nothing for NULL objects
    1 :	{ Point }
      Result := ReadPointObject( False );
		3 :	{ Arc }
			Result := ReadPolyObject( False, false );
		5 :	{ Polygon }
			Result := ReadPolyObject( True, false );
		8 : { MultiPoint }
			Result := ReadMultiPointObject( false );
    11 :	{ PointZ }
      Result := ReadPointObject( True );
		13 :	{ PolylineZ }
			Result := ReadPolyObject( False, true );
		15 :	{ PolygonZ }
			Result := ReadPolyObject( True, true );
    18 : // MultiPointZ
			Result := ReadMultiPointObject( true );
    21 : // PointM
      Result := ReadPointObject( False );
    23 : // PolylineM
			Result := ReadPolyObject( False, False );
    25 : // PolygonM
			Result := ReadPolyObject( True, False );
    28 : // MultiPointM
			Result := ReadMultiPointObject( False );
		else
      raise EGException.CreateFmt( 'Unhandled object type %d in SHP file %s', [objType, Filename] );
		end;

    if DBF <> nil then
    begin
      DBF.RecordID := FiDBFRecordID;

      if Result <> nil then
        Result.ObjectDataCSL := DBF.RowCSL;

      Inc( FiDBFRecordID );
    end;
    iPosition := iPosition + ( 4 + iLen ) * 2;
    FShpStream.Position := iPosition;
	end;
end;

procedure TGSHPFileReader.LoadFromXMD(el: TGXML_Element);
begin
  inherited;

  if el <> nil then
  begin
    LongXDivisor := el.Attribute('LongXDivisor', 1.0 );
    LatYDivisor := el.Attribute('LatYDivisor', 1.0 );
    LongXShift := el.Attribute('LongXShift', 0.0 );
    LatYShift := el.Attribute('LatYShift', 0.0 );
  end;
end;

initialization
  RegisterGlobeClass( TGSHPFileReader, 'SHP File Reader' );
end.
