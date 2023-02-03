//-----------------------------------------------------------------------
// Summary
//	TGlobe .LYR Format file reader
//
// Description
// 	Reads data objects from an LYR format file
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
{$I GLOBE5.INC}
unit GLYRReader;

interface

uses
  Windows, SysUtils, Classes, Graphics, GPresenters, GSysUtils, GMapObjects,
  GClasses, GDataReaders, GXML;

type
  TGLYRFileReader = class;

  TGReadHeaderMethod = function(reader:TGLYRFileReader) : TGPresenter;
  TGReadDataMethod = function(reader:TGLYRFileReader) : TGMapPoint;


  TGLYRFileReader = class(TGFileDataReader)
  private
    FFileVersion : Word; { Helper global for Load routines }

    FLYRStream : TStream;
    FReader : TGStreamReader;

    FObjCount : integer;
    FHeaderMethod: TGReadHeaderMethod;
    FDataMethod: TGReadDataMethod;

    function ReadObjectProperties( obj : TGMapPoint ) : TGMapObjectFlagSet;
    procedure ReadPolyObject( obj : TGMapPoly; ObjectFlags : TGMapObjectFlagSet );
    procedure ReadPoints( PointStore : TGPointStore; Reader : TGStreamReader );

    procedure ReadPresenter( aPresenter : TGPresenter );
    procedure ReadPen( aPen : TGPen );
    procedure ReadBrush( aBrush : TGBrush );
    procedure ReadFont( aFont : TGFont );

  protected
    function ReadHeader : Boolean; override;
    function ReadObject : IGMapPoint; override;
    function InternalOpen : Boolean; override;
    function InternalClose : Boolean; override;
  public
    function ProgressPercent : Double; override;
  published
    function ReadDataTGMapPoint : TGMapPoint;
    function ReadDataTGMapObject : TGMapPoint;
    function ReadDataTGMapPoly : TGMapPoint;

    function ReadDataTGObject : TGMapPoint;
    function ReadDataTGPolyObject : TGMapPoint;

    function ReadTGPointPresenter : TGPresenter;
    function ReadTGPolyPresenter : TGPresenter;
    function ReadTGPolygonPresenter : TGPresenter;
    function ReadTGPolylinePresenter : TGPresenter;

    
    function ReadDataTGeoDataObject : TGMapPoint;
    function ReadTPointPresenter : TGPresenter;
    function ReadTPolyPresenter : TGPresenter;
    function ReadTPolygonPresenter : TGPresenter;
    function ReadTPolylinePresenter : TGPresenter;
  end;

implementation

Uses GResource, GLYRWriter;

function TGLYRFileReader.InternalClose: Boolean;
begin
  inherited InternalClose;

  FreeAndNil( FReader );
  FreeAndNil( FLYRStream );
  Result := true;
end;


function TGLYRFileReader.InternalOpen: Boolean;
begin
  inherited InternalOpen;

  Result := FileExists( Filename );
  if Result then
  begin
    FLYRStream := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite );

    FReader := TGStreamReader.Create( FLYRStream );
  end;
end;


function TGLYRFileReader.ProgressPercent: Double;
begin
  if FLYRStream <> nil then
    Result := ( FLYRStream.Position / FLYRStream.Size ) * 100.0
  else
    Result := 0.0;
end;



function TGLYRFileReader.ReadHeader: Boolean;
var
  idx : Integer;
  sClassName : ShortString;
begin
  Result := true;
  FFileVersion := FReader.ReadSmallInt; { read the version of globe data file }

  // check that the LYR file version is not newer than we can handle
  if FFileVersion > TG_FILEVERSION then
    raise EGException.CreateFmt( rsEDataFileVersionMsg, [FFileVersion, Filename] );

  if FFileVersion < TG_FILEVERSION300 then
    raise EGException.CreateFmt( rsEDataFileVersionMsg, [FFileVersion, Filename] );

  { read in the bounding rectangle }
  if FFileVersion < TG_FILEVERSION500 then
    FReader.ReadRect;

  FObjCount := FReader.ReadInteger;
  while ( FObjCount > 0 ) and ( FLYRStream.Position < FLYRStream.Size ) do
  begin
    sClassName := FReader.ReadShortString; { Get class name }

    if sClassName = 'MetaData' then
    begin
      MetaData.CommaText := FReader.ReadLongString;
      Presenters.AsXMLString := MetaData.Values['Presenters'];
    end
    else
    if sClassName = 'PresenterXML' then
    begin
      Presenters.AsXMLString := '<Presenters>' + FReader.ReadLongString + '</Presenters>';
    end
    else
    if sClassName = 'ColumnNames' then
    begin
      MetaData.Values['ColumnNames'] := FReader.ReadLongString;
    end
    else
    begin
      // Stop if we hit the TGlobeObject data
      @FHeaderMethod := MethodAddress( 'Read' + sClassName );
      if not Assigned( FHeaderMethod ) then
      begin
        @FDataMethod := MethodAddress( 'ReadData' + sClassName );
        Assert(Assigned( FDataMethod ), 'No Read method for class ' + sClassName );
        Exit;
      end;

      // Read in the Presenter objects
      for idx := 0 to FObjCount - 1 do
      begin
        { Create new instance of class the Presenter }
        Presenters.Add( FHeaderMethod(self) );
      end;
    end;
    FObjCount := FReader.ReadInteger;
  end;
end;


function TGLYRFileReader.ReadObject: IGMapPoint;
var
  sClassName : String;
//  obj : TGObject;
begin
  inherited ReadObject;

  if FObjCount = 0 then
  begin
    Result := nil;
    FObjCount := FReader.ReadInteger;
    if FObjCount = 0 then
      Exit;
    sClassName := FReader.ReadShortString; { Get class name }

    @FDataMethod := MethodAddress( 'ReadData' + sClassName );
    if not Assigned( FDataMethod ) then
      raise EGException.Create('No Read method for class: ' + sClassName );
  end;

  Dec( FObjCount );

  if Assigned( FDataMethod ) then
    Result:= FDataMethod(Self)
  else
    Result := nil;
end;

function TGLYRFileReader.ReadObjectProperties(obj: TGMapPoint) : TGMapObjectFlagSet;
var
  csl : AnsiString;
begin
  Result := TGMapObjectFlagSet(Ord(FReader.ReadWord));

  csl := '';
  if ofTitle in Result then
  begin
    if FFileVersion >= TG_FILEVERSION500 then
      csl := FReader.ReadLongString { Read Title if present }
    else
      csl := FReader.ReadShortString; { Read Title if present }

    if ( csl <> '' ) and ( csl[1] <> '"' ) then
      csl := '"' + csl + '"';
  end;

  obj.ObjectDataCSL := csl;

  if ofID in Result then
    FReader.ReadInteger;  // Object ID not used

  if ofUserID in Result then
    obj.UserID := FReader.ReadShortString; { Read Object GUID if present }

  if ofZoomMinMax in Result then
  begin
    //ToDo:
    FReader.ReadInteger; { Read ZoomMin if present }
    FReader.ReadInteger; { Read ZoomMax if present }
  end;

  obj.PresenterID := FReader.ReadSmallInt;
end;

procedure TGLYRFileReader.ReadPoints(PointStore: TGPointStore; Reader: TGStreamReader);
var
  dX, dY, dH, idx, iMultiplier : integer;
  iLastX, iLastY, iLastH : integer;

  {---------------------------------------------------}
  function ReadPointDiff( Flags : TGPointStoreFlagsSet ) : Boolean;
  begin
    if ps16Bit in Flags then
    begin
      dX := Reader.ReadShortInt;
      Result := dX <> 127;
    end
    else
    begin
      dX := Reader.ReadSmallInt;
      Result := dX <> 32767;
    end;

    if Result then
    begin
      if ps16Bit in Flags then
        dY := Reader.ReadShortInt
      else
        dY := Reader.ReadSmallInt;

      dH := 0;
      if psHeight in Flags then
        if ps16Bit in Flags then
          dH := Reader.ReadShortInt
        else
          dH := Reader.ReadSmallInt;
    end;
  end;

begin
  with PointStore do
  begin
    if psLongCount in Flags then
      Count := Reader.ReadInteger { get number of points in ring }
    else
      Count := Reader.ReadWord; { get number of points in ring }

    AsLL[0] := Reader.ReadPointLL( psHeight in Flags, ps16Bit in Flags ); { read first point }

    if ps16Bit in Flags then
      iMultiplier := GU_MINUTE
    else
      iMultiplier := 1;

    if not( psCompressed in Flags ) then { read un-compressed data }
      for idx := 1 to Count - 1 do
        AsLL[idx] := Reader.ReadPointLL( psHeight in Flags, ps16Bit in Flags )
    else
    begin { read compressed data }
      with AsLL[0] do
      begin
        iLastX := iLongX div iMultiplier;
        iLastY := iLatY div iMultiplier;
        iLastH := iHeightZ div iMultiplier;
      end;

      for idx := 1 to Count - 1 do
        if ReadPointDiff( Flags ) then
        begin
          iLastX := iLastX + dX;
          iLastY := iLastY + dY;
          iLastH := iLastH + dH;
          AsLL[idx] := PointLLH( iLastX * iMultiplier, iLastY * iMultiplier, iLastH * iMultiplier );
        end
        else
        begin
          AsLL[idx] := Reader.ReadPointLL( psHeight in Flags, ps16Bit in FLags );
          with AsLL[idx] do
          begin
            iLastX := iLongX div iMultiplier;
            iLastY := iLatY div iMultiplier;
            iLastH := iHeightZ div iMultiplier;
          end;
        end;
    end;
  end;
end;

procedure TGLYRFileReader.ReadPolyObject(obj: TGMapPoly; ObjectFlags : TGMapObjectFlagSet);
var
  chainFlags : TGMapObjectFlagSet;
  iChain : integer;
  pointFlags: TGPointStoreFlagsSet;
  csFlags: TGChainStoreFlagSet;
begin
  if FFileVersion >= TG_FILEVERSION500 then
  begin
    csFlags := [];

    if ofMultiRing in ObjectFlags then
      Include( csFlags, cfMultiRing );
    if ofHeight in ObjectFlags then
      Include( csFlags, cfHeight );

    obj.Chains.ReadChains( FReader, csFlags );
  end
  else
  begin
    if ofMultiRing in ObjectFlags then
      obj.Chains.Count := FReader.ReadWord
    else
      obj.Chains.Count := 1;

    chainFlags := ObjectFlags;
    // Load each Point Store from the Reader
    for iChain := 0 to obj.Chains.Count - 1 do
    begin
      pointFlags := [];
      obj.Chains[iChain].StoreHeight := ofHeight in chainFlags;

      if of16Bit in chainFlags then
        Include(pointFlags, ps16Bit);

      if ofCompressed in chainFlags then
        Include(pointFlags, psCompressed);

      if ofLongCount in chainFlags then
        Include(pointFlags, psLongCount);

      obj.Chains[iChain].Flags := pointFlags;
      ReadPoints( obj.Chains[iChain], FReader);

      if iChain < obj.Chains.Count - 1 then  // if more rings then get a new flag set
        chainFlags := TGMapObjectFlagSet( Ord( FReader.ReadWord ));
    end;
  end;
end;

function TGLYRFileReader.ReadTGPointPresenter: TGPresenter;
var
  PointType : TGPointPresenterType;
  PointUnit : TGUnitTypes;
  sTmp : String;
begin
  Result := TGPointPresenter.Create( ParentGlobe );
  ReadPresenter( Result );

  FReader.ReadBuffer( PointType, SizeOf( TGPointPresenterType ) );
  TGPointPresenter( Result ).PointType := PointType;
  FReader.ReadBuffer( PointUnit, SizeOf( TGUnitTypes ) );
  TGPointPresenter( Result ).PointUnit := pointUnit;
  TGPointPresenter( Result ).PointSize := FReader.ReadInteger;

  if TGPointPresenter( Result ).PointSize < 0 then
  begin
    TGPointPresenter( Result ).PointSize := -TGPointPresenter( Result ).PointSize;
    TGPointPresenter( Result ).MaxPointSize := FReader.ReadInteger;
  end;
  TGPointPresenter( Result ).SymbolIndex := FReader.ReadSmallInt;
  sTmp := FReader.ReadShortString;
  if Length( sTmp ) > 0 then
    TGPointPresenter( Result ).ImageName := sTmp;

  ReadPen( TGPointPresenter( Result ).PointPen );
  ReadBrush( TGPointPresenter( Result ).PointBrush );
  ReadFont( TGPointPresenter( Result ).PointFont );
end;

function TGLYRFileReader.ReadTGPolygonPresenter: TGPresenter;
begin
  Result := TGPolygonPresenter.Create(ParentGlobe);
  ReadPresenter( Result );
  ReadPen( TGPolygonPresenter( Result ).PolyPen );
  ReadBrush( TGPolygonPresenter( Result ).PolyBrush );
end;

function TGLYRFileReader.ReadTGPolylinePresenter: TGPresenter;
begin
  Result := TGPolylinePresenter.Create(ParentGlobe);
  ReadPresenter( Result );
  ReadPen( TGPolylinePresenter( Result ).PolyPen );
end;

function TGLYRFileReader.ReadTGPolyPresenter: TGPresenter;
begin
  Result := TGPolyPresenter.Create(ParentGlobe);
  ReadPresenter( Result );
  ReadPen( TGPolyPresenter( Result ).PolyPen );
  ReadBrush( TGPolyPresenter( Result ).PolyBrush );
end;

procedure TGLYRFileReader.ReadBrush( aBrush : TGBrush );
var
  BrushColor : TColor;
  BrushBkColor: TColor;
  BrushStyle : TGBrushStyle;
begin
  FReader.ReadBuffer(BrushColor, SizeOf(TColor));

  if FFileVersion >= TG_FILEVERSION405 then
    FReader.ReadBuffer(BrushBkColor, SizeOf(TColor))
  else
    BrushBkColor := BrushColor;

  FReader.ReadBuffer(BrushStyle, SizeOf(TGBrushStyle));

  aBrush.Color := BrushColor;
  aBrush.BkColor := BrushColor;
  aBrush.Style := BrushStyle;
end;

procedure TGLYRFileReader.ReadFont( aFont : TGFont );
var
  FontUnit : TGUnitTypes;
  FontColor : TColor;
  Style : Byte;
begin
  if FFileVersion >= TG_FILEVERSION500 then
  begin
    FReader.ReadBuffer(FontColor, SizeOf(TColor));
    aFont.BkColor := FontColor;
  end;

  FReader.ReadBuffer(FontColor, SizeOf(TColor));
  aFont.Color := FontColor;
  FReader.ReadBuffer(FontUnit, SizeOf(TGUnitTypes));
  aFont.SizeUnit := FontUnit;
  aFont.Size := FReader.ReadSmallInt;
  aFont.Angle := FReader.ReadSmallInt;
  FReader.ReadBuffer(Style, SizeOf(Byte));
  aFont.Style := TGFontStyles( Style );
  aFont.Name := FReader.ReadShortString;
end;

procedure TGLYRFileReader.ReadPen( aPen : TGPen );
var
  PenColor : TColor;
  PenStyle : TGPenStyle;
  PenWidth : Byte;
  PenUnit : TGUnitTypes;
begin
  FReader.ReadBuffer(PenColor, SizeOf(TColor));
  FReader.ReadBuffer(PenStyle, SizeOf(TPenStyle));
  aPen.Color := PenColor;
  aPen.Style := PenStyle;

  if FFileVersion >= TG_FILEVERSION400 then
  begin
    aPen.Width := FReader.ReadInteger;
    FReader.ReadBuffer(PenUnit, SizeOf(TGUnitTypes));
    aPen.WidthUnit := PenUnit;
  end
  else
  begin
    FReader.ReadBuffer(PenWidth, SizeOf(Byte));
    aPen.Width :=  PenWidth;
    aPen.WidthUnit := guPixel;
  end;
end;

procedure TGLYRFileReader.ReadPresenter(aPresenter: TGPresenter);
var
  TitleAlignment : TGTitleAlignment;
  ZoomUnit : TGUnitTypes;
begin
  if FFileVersion >= TG_FILEVERSION500 then
  begin
    aPresenter.AlphaBlend := FReader.ReadByte;
    aPresenter.TitleColumn := FReader.ReadSmallInt;
    aPresenter.NextPresenterID := FReader.ReadInteger;
    aPresenter.TitleZoomMax := FReader.ReadInteger;
    aPresenter.TitleZoomMin := FReader.ReadInteger;
    ZoomUnit := aPresenter.TitleZoomUnits;
    FReader.ReadBuffer( ZoomUnit, SizeOf( TGUnitTypes ));
  end;

  aPresenter.PresenterID := FReader.ReadInteger;

  if FFileVersion >= TG_FILEVERSION400 then
    aPresenter.TitleOffset := FReader.ReadInteger
  else
    if FFileVersion >= TG_FILEVERSION301 then
      aPresenter.TitleOffset := FReader.ReadShortInt;

  aPresenter.Name := FReader.ReadShortString; // Presenter name

  FReader.ReadBuffer(TitleAlignment, 1);
  aPresenter.TitleAlignment := TGTitleAlignment(TitleAlignment);
  ReadFont(aPresenter.TitleFont);
end;

function TGLYRFileReader.ReadDataTGMapObject: TGMapPoint;
var
  ObjectFlags : TGMapObjectFlagSet;
begin
  Result := TGMapPoint.Create;

  ObjectFlags := ReadObjectProperties( Result );

  Result.Centroid := FReader.ReadPointLL(ofHeight in ObjectFlags, of16Bit in ObjectFlags)  // read first point
end;

function TGLYRFileReader.ReadDataTGMapPoint : TGMapPoint;
begin
  Result := ReadDataTGMapObject;
end;

function TGLYRFileReader.ReadDataTGObject: TGMapPoint;
begin
  Result := ReadDataTGMapObject;
end;

function TGLYRFileReader.ReadDataTGMapPoly: TGMapPoint;
var
  ObjectFlags : TGMapObjectFlagSet;
begin
  Result := TGMapPoly.Create;

  ObjectFlags := ReadObjectProperties( Result );

  if ofClosed in ObjectFlags then
    TGMapPoly( Result ).Closed := true;

  if ofSingle in ObjectFlags then
    Result.Centroid := FReader.ReadPointLL(ofHeight in ObjectFlags, of16Bit in ObjectFlags)  // read Centroid only
  else
    ReadPolyObject( TGMapPoly( Result), ObjectFlags );
end;

function TGLYRFileReader.ReadDataTGeoDataObject: TGMapPoint;
begin
  Result := ReadDataTGMapPoly;
end;

function TGLYRFileReader.ReadTPointPresenter: TGPresenter;
begin
  Result := ReadTGPointPresenter;
end;

function TGLYRFileReader.ReadTPolyPresenter: TGPresenter;
begin
  Result := ReadTGPolyPresenter
end;

function TGLYRFileReader.ReadTPolygonPresenter: TGPresenter;
begin
  Result := ReadTGPolygonPresenter;
end;

function TGLYRFileReader.ReadTPolylinePresenter: TGPresenter;
begin
  Result := ReadTGPolylinePresenter
end;

function TGLYRFileReader.ReadDataTGPolyObject: TGMapPoint;
begin
  Result := ReadDataTGMapPoly;
end;

initialization
  RegisterGlobeClass(TGLYRFileReader, 'LYR File Reader' );
end.
