unit GVirtualEarthLayer;

interface

uses Windows, Classes, SysUtils, Graphics, GClasses, GSysUtils, GBitmap,
    GProjections, GXML;

const
  MAX_ZOOM_LEVELS = 19;

type
  TGVEMapStyle = ( vemsRoad, vemsAerial, vemsHybrid );

  TGVirtualEarthLayer = class( TGLayer )
  private
    FImageTileSize : integer;
    FImageWidth : integer;
    FImageHeight : integer;
    FMapStyle: TGVEMapStyle;

    function CalcTileLatitude(y, z: integer): integer;

    function CalcLevel(sf: Extended): integer;
    function CalcTileMER( x, y, z : integer ) : TGMER;
    function TileURL( x, y, z : integer ) : string;
    procedure RenderTile( bmp : TGBitmap; tileMER : TGMER; y, z : integer );
    procedure RenderTiles( x, y, z, targetZ : integer );
    procedure SetMapStyle(const Value: TGVEMapStyle);
  public
    constructor Create(ParentGlobe : TGCustomGlobe5); override;
    procedure Render( firstPass : Boolean; const viewMER : TGMER ); override;
    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;
    function ZoomLevelToScaleFactor( level : integer ) : Extended;
    function ScaleFactorToZoomLevel( sf : Extended ) : integer;
  published
    property MapStyle : TGVEMapStyle read FMapStyle write SetMapStyle;
  end;

implementation

{ TGVirtualEarthLayer }

uses Globe5, Globe5Utils, Math;

function TGVirtualEarthLayer.CalcLevel(sf: Extended): integer;
var
  vLen : Double;
  tileSz : Double;
begin
  Result := 0;
  tileSz := FImageTileSize / sf * 1.5;
  while Result < MAX_ZOOM_LEVELS do
  begin
    vLen := GU_180_DEGREE / ( 2 shl Result );

    if ( tileSz >= vLen * 1.25 ) then   // Adjustment factor to improve tile images
      Break;
    Inc( Result );
  end;
end;

function TGVirtualEarthLayer.CalcTileLatitude(y, z: integer): integer;
var
  eY : Double;
begin
  eY := GU_180_DEGREE - ( y * ( GU_360_DEGREE / ( 2 shl z )));

  Result := Trunc(( HalfPi - 2.0 * arctan( Exp( -eY * GU_TORADIANS )) ) * GU_FROMRADIANS);
end;

function TGVirtualEarthLayer.CalcTileMER(x, y, z: integer): TGMER;
var
  tileX : Extended;
begin
  tileX := GU_360_DEGREE / ( 2 shl z );

  Result.iLongX := Round( -GU_180_DEGREE + x * tileX ) + 1;
  Result.iLatY := CalcTileLatitude( y + 1, z );
  Result.iWidthX := Round( tileX ) - 1;
  Result.iHeightY := CalcTileLatitude( y, z ) - Result.iLatY;
end;

constructor TGVirtualEarthLayer.Create(ParentGlobe: TGCustomGlobe5);
begin
  inherited;
  FImageTileSize := 256;
  FImageWidth := FImageTileSize * (2 shl MAX_ZOOM_LEVELS);
  FImageHeight := FImageTileSize * (2 shl MAX_ZOOM_LEVELS);
  FMapStyle := vemsRoad;
end;

procedure TGVirtualEarthLayer.LoadFromXML(Element: TGXML_Element);
begin
  inherited;

  if Element <> nil then
    FMapStyle := TGVEMapStyle( Element.Attribute( 'MapStyle', integer( vemsRoad )));
end;

procedure TGVirtualEarthLayer.Render(firstPass: Boolean; const viewMER: TGMER);
begin
  if not Visible then
    Exit;
  // recursively search for triangles that are visible at all levels
  // from 0 downto the actual calculated level

  if firstPass then
    RenderTiles( 0, 0, -1, CalcLevel( ParentGlobe.Projector.ScaleFactor ));

  inherited;
end;

procedure TGVirtualEarthLayer.RenderTile( bmp : TGBitmap; tileMER : TGMER; y, z : integer );
var
  idx, jdx : integer;
  srcTri : TGTriangle;
  destTri : TGTriangleLL;

  XSteps, YSteps : integer;
  s_dx, s_dy, i_dx, i_dy : Extended;
  sx, sy, ix, iy : Extended;
  eY : Extended;

  function Latitude( tileY : Extended ) : integer;
  begin
    Result := Trunc(( HalfPi - 2.0 * arctan( Exp( -tileY * GU_TORADIANS )) ) * GU_FROMRADIANS);
  end;
begin
  bmp.AlphaBlend := MulDiv( ObjectSource.Layer.AdjustedAlphaBlend, AlphaBlend, 255 );

  // calculate the interpolation steps
  with tileMER do
  begin
    XSteps := Trunc( iWidthX / ( GU_DEGREE * 5 )) + 1;
    YSteps := Trunc( iHeightY / ( GU_DEGREE * 5 )) + 1;

    s_dx := FImageTileSize / XSteps;
    s_dy := FImageTileSize / YSteps;
    i_dx := ( iWidthX + 0.5 ) / XSteps; // correction to remove vertical stitch lines
  end;

  // calc un-projected start of tile
  eY := GU_180_DEGREE - (( y + 1 ) * ( GU_360_DEGREE / ( 2 shl z )));
  i_dY := 2.5 + ( GU_360_DEGREE / ( 2 shl z )) / YSteps;

  for idx := 0 to XSteps - 1 do
  begin
    sx := s_dx * idx;
    ix := tileMER.iLongX + i_dx * idx;

    for jdx := 1 to YSteps do
    begin
      sy := ( FImageTileSize ) - s_dy * jdx;
      iy := i_dy * ( jdx - 1 );

      srcTri[0] := Point(Trunc( sx ), Trunc( sy ));
      srcTri[1] := Point(Trunc( sx ), Trunc( sy + s_dy ));
      srcTri[2] := Point(Trunc( sx + s_dx ), Trunc( sy ));
      destTri[0] := PointLL(Trunc( ix ), Latitude( eY + iy + i_dy ));
      destTri[1] := PointLL(Trunc( ix ), Latitude( eY + iy ));
      destTri[2] := PointLL(Trunc( ix + i_dx ), Latitude( eY + iy + i_dy ));
      TGlobe5(ParentGlobe).RenderTextureTriangle( srcTri, destTri, bmp, false );

      srcTri[0] := Point(Trunc( sx + s_dx ), Trunc( sy + s_dy ));
      destTri[0] := PointLL(Trunc( ix + i_dx ), Latitude( eY + iy ));
      TGlobe5(ParentGlobe).RenderTextureTriangle( srcTri, destTri, bmp, false );
    end;
  end;
end;

procedure TGVirtualEarthLayer.RenderTiles(x, y, z, targetZ: integer);
var
  tileMER : TGMER;
  State : TGRenderStateSet;
  bmp : TGBitmap;
begin
  if z >= MAX_ZOOM_LEVELS then
    Exit;

  if z >= 0 then
  begin
    tileMER := CalcTileMER( x, y, z );

{    if z = 2 then
    begin
      DrawProjectedMER(TGlobe5(ParentGlobe), CalcTileMER( x, y, z ));
      TGlobe5(ParentGlobe).RenderTextOut(MER_CenterLL(tileMER), Format( '%d,%d', [x,y]));
    end;
}
    if ParentGlobe.Projector.ProjectionModel.VisibleMER( tileMER, State ) = false then
      Exit;
  end;
  // render the quarter tiles of this tile
  if z < targetZ then
  begin
    x := x * 2;
    y := y * 2;
    RenderTiles( x, y, z + 1, targetZ );
    RenderTiles( x + 1, y, z + 1, targetZ );
    RenderTiles( x, y + 1, z + 1, targetZ );
    RenderTiles( x + 1, y + 1, z + 1, targetZ );
  end
  else
  begin // we are at the required level and the tile is visible so render it
    if Assigned( TGlobe5(ParentGlobe).ImageTileCache ) then
      bmp := TGlobe5(ParentGlobe).ImageTileCache.FetchURLTile( ParentGlobe, TileURL(x,y,z))
    else
      bmp := nil;

    if bmp <> nil then
    begin
      RenderTile( bmp, tileMER, y, z );
//      DrawProjectedMER(TGlobe5(ParentGlobe), tileMER);  // show the outline of the tile
    end;
  end;
end;


procedure TGVirtualEarthLayer.SaveToXML(Element: TGXML_Element);
begin
  inherited;

  Element.AddAttribute( 'MapStyle', integer( FMapStyle ), integer( vemsRoad ));
end;

function TGVirtualEarthLayer.ScaleFactorToZoomLevel(sf: Extended): integer;
var
  vLen : Double;
  tileSz : Double;
begin
  Result := 0;
  tileSz := FImageTileSize / sf;
  while Result < MAX_ZOOM_LEVELS do
  begin
    vLen := GU_180_DEGREE / ( 2 shl Result );

    if tileSz >= vLen then
      Break;
    Inc( Result );
  end;
end;

procedure TGVirtualEarthLayer.SetMapStyle(const Value: TGVEMapStyle);
begin
  FMapStyle := Value;
end;

function TGVirtualEarthLayer.TileURL(x, y, z: integer): string;
var
  idx : integer;
  mask : integer;
  val : integer;
  ms : string;
begin
  // generate the quad key for the tile
  Result := '';
  if z < 0 then
    Result := '0';

  for idx := z downto 0 do
  begin
    mask := 1 shl idx;
    val := 0;

    if (X and mask) <> 0 then
      val := 1;

    if ( Y and mask ) <> 0 then
       Inc( val, 2 );

    Result := Result + chr( Ord('0') + val );
  end;

//  Result := '216.38.192.50/tiles/r' + Result + '.png';
//  exit;

  case FMapStyle of
  vemsRoad :    ms := 'r';
  vemsAerial :  ms := 'a';
  vemsHybrid :  ms := 'h';
  end;

  Result := ms + Result[Length(Result)] + '.ortho.tiles.virtualearth.net/tiles/' + ms + Result;

  case FMapStyle of
  vemsRoad :    Result := Result + '.png?g=22';
  vemsAerial, vemsHybrid :  Result := Result + '.jpg?g=22';
  end;
end;

function TGVirtualEarthLayer.ZoomLevelToScaleFactor(level: integer): Extended;
begin
  Result := (( 2 shl level ) * FImageTileSize ) / GU_180_DEGREE;
end;

initialization
  RegisterGlobeClass( TGVirtualEarthLayer, 'VirtualEarthLayer' );
end.
