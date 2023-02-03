{------------------------------------------------------------------------------
 Module:    Globe5Projections.pas

 Comment:   TGlobe Suplemental projections

 Classes:   TGAlbersEqualAreaConicPrj
            TGAzimuthal2EquidistantPrj
            TGAzimuthalEquidistantPrj
            TGBehrmannCylEqualAreaPrj
            TGBonnePrj
            TGCylEqualAreaPrj
            TGEquidistantPrj
            TGPetersPrj
            TGSinusoidalPrj
            TGGnomonicPrj

 Author:    Graham Knight
 Email:     tglobe@tglobe.com

 These projections can be used by creating and assigning to the
 Globe.Projection.ProjectionModel property i.e.

 with Globe do
   Projection.ProjectionModel := TGAlbersEqualAreaConicPrj.Create( Projection );

 You do not need to free the existing projection as this is done
 automatically when you assign the new projection.

------------------------------------------------------------------------------}
unit Globe5Projections;

interface

uses WinTypes, WinProcs, SysUtils, Classes, GSysUtils, Globe5, GClasses,
  GProjections;

type
  {------------------------ TTGGnomonicPrjPrj -------------------------}
  TGGnomonicPrj = class( TGCartesianPrj )
  public
    function PointLLToXY( const ptLL : TGPointLL; iIndex : Integer ) : Boolean; override;
    function XYToLL( iX, iY, iIndex : Integer ) : Boolean; override;
    function IsContinuous : Boolean; override;
  end;

  {------------------------ TGAzimuthal2EquidistantPrj -------------------------}
  TGAzimuthal2EquidistantPrj = class( TGCartesianPrj )
  public
    constructor Create(ParentGlobe : TGCustomGlobe5); override;

    function IsContinuous : Boolean; override;
    function PointLLToXY( const ptLL : TGPointLL; iIndex : Integer ) : Boolean; override;
    function XYToLL( iX, iY, iIndex : Integer ) : Boolean; override;
    procedure PaintSurface; override;
  end;

  {------------------------ TGAzimuthalEquidistantPrj --------------------------}
  TGAzimuthalEquidistantPrj = class( TGAzimuthal2EquidistantPrj )
  end;

  {---------------------------- TGEquidistantPrj -------------------------------}
  TGEquidistantPrj = class( TGCartesianPrj )
  public
    function PointLLToXY( const ptLL : TGPointLL; iIndex : Integer ) : Boolean; override;
    function XYToLL( iX, iY, iIndex : Integer ) : Boolean; override;
    procedure PaintSurface; override;
    function IsContinuous : Boolean; override;
  end;

  {---------------------------- TGCylEqualAreaPrj ------------------------------}
  TGCylEqualAreaPrj = class( TGCartesianPrj )
  public
    function PointLLToXY( const ptLL : TGPointLL; iIndex : Integer ) : Boolean; override;
    function XYToLL( iX, iY, iIndex : Integer ) : Boolean; override;
  end;

  {---------------------------- TGBehrmannCylEqualAreaPrj ----------------------}
  TGBehrmannCylEqualAreaPrj = class( TGCylEqualAreaPrj )
  public
    constructor Create(ParentGlobe : TGCustomGlobe5); override;
  end;

  {---------------------------- TGPetersPrj ------------------------------------}
  TGPetersPrj = class( TGCylEqualAreaPrj )
  public
    constructor Create(ParentGlobe : TGCustomGlobe5); override;
  end;

  {---------------------------- TGAlbersEqualAreaConicPrj ----------------------}
  TGAlbersEqualAreaConicPrj = class( TGCartesianPrj )
  private
    n, Rho0, C : Extended;
  public
    constructor Create(ParentGlobe : TGCustomGlobe5); override;

    procedure PropertyChanged( ProjectionProperty : TGProjectionProperty ); override;
    function PointLLToXY( const ptLL : TGPointLL; iIndex : Integer ) : Boolean; override;
    function XYToLL( iX, iY, iIndex : Integer ) : Boolean; override;
  end;

  {---------------------------- TGBonnePrj -------------------------------------}
  TGBonnePrj = class( TGCartesianPrj )
  private
    cotSP : Extended;
  public
    constructor Create(ParentGlobe : TGCustomGlobe5); override;

    procedure PropertyChanged( ProjectionProperty : TGProjectionProperty ); override;
    function PointLLToXY( const ptLL : TGPointLL; iIndex : Integer ) : Boolean; override;
    function XYToLL( iX, iY, iIndex : Integer ) : Boolean; override;
  end;

  {---------------------------- TGSinusoidalPrj -------------------------------------}
  TGSinusoidalPrj = class( TGCartesianPrj )
  public
    function PointLLToXY( const ptLL : TGPointLL; iIndex : Integer ) : Boolean; override;
    function XYToLL( iX, iY, iIndex : Integer ) : Boolean; override;
  end;

implementation



{-------------------------------------------------------------------------
 TGAzimuthal2EquidistantPrj.Create
-------------------------------------------------------------------------}
{**
  @Param  ParentGlobe Globe to associate the Projection with.
}
constructor TGAzimuthal2EquidistantPrj.Create(ParentGlobe : TGCustomGlobe5);
begin
  inherited Create( ParentGlobe );

  TGlobe5(ParentGlobe).Graticule.LongitudeStep := 10;
end;

{-------------------------------------------------------------------------
 TGAzimuthal2EquidistantPrj.IsContinuous
-------------------------------------------------------------------------}
function TGAzimuthal2EquidistantPrj.IsContinuous: Boolean;
begin
  Result := true;
end;

{-------------------------------------------------------------------------
 TGAzimuthal2EquidistantPrj.XYtoLL()
-------------------------------------------------------------------------}
{**
  @Param iX Screen X coordinate to convert.
  @Param iY Screen Y coordinate to convert.
  @Param iIndex Position in the gaPoints global array to store the result.
  @Result True if the Point is on the visible surface of the globe.
}
function TGAzimuthal2EquidistantPrj.XYToLL( iX, iY, iIndex : Integer ) : Boolean;
var x, y,
    c, sinc, cosc,
    sinhlat, coshlat,
    lat2, lon2 : extended;
begin
  with ParentGlobe.Projector do
  begin
    x := ( iX - XOrigin ) / ScaleFactor * GU_TORADIANS;
    y := -( iY - YOrigin ) / ScaleFactor * GU_TORADIANS;
    c := Hypot( x, y );
    if c <= TG_EPSILON then begin
      lat2 := 0;
      lon2 := 0;
      Result := True;
    end else begin
      SinCos( c, sinc, cosc );
      SinCos(CentralParallel * DD_TORADIANS,sinhlat,coshlat);
      lat2 := ArcSin( cosc*sinhlat + y*sinc*coshlat/c );
      lon2 := ArcTan2( x*sinc,(c*coshlat*cosc-y*sinhlat*sinc) );
      { check to see if the coords are outside the globe }
      Result := ( abs( lon2 ) < LocalPi ) and ( abs( lat2 ) < HalfPi );
    end;

    if Result then
      ParentGlobe.Renderer.Points[iIndex] := Point(
        Mod180( Round( lon2 * GU_FROMRADIANS + CentralMeridian * GU_DEGREE)),
        Round( lat2 * GU_FROMRADIANS ));
  end;
end;

{-------------------------------------------------------------------------
 TGAzimuthal2EquidistantPrj.LLToXY
-------------------------------------------------------------------------}
{**
  @Param ptLL TGPointLL object to convert to XY screen Coordinates.
  @Param iIndex Position in the gaPoints global array to store the result.
  @Result True if the Point is on the screen.
}
function TGAzimuthal2EquidistantPrj.PointLLToXY( const ptLL : TGPointLL; iIndex : Integer ) : Boolean;
var lat2, lon2, plat, plon, cll, c, sinll, cosll, sinc, cosc, kprime,
      HLat, HLon, sinHLat, sinLat2, cosHLat, cosLat2 : Extended;
begin
  with ptLL do
  begin    { check to see if the coords are outside the globe }
    with ParentGlobe.Projector.ExtentsLL do
      Result := ( iLongX > Left ) and ( iLongX <= Right ) and ( iLatY >= Top ) and ( iLatY <= Bottom );

    if Result then
      with ParentGlobe.Projector do
      begin
        Hlat := CentralParallel * DD_TORADIANS; //CentralParallel must be in degrees (decimal)
        Hlon := CentralMeridian * DD_TORADIANS; //CentralMeridian must be in degrees (decimal)
        SinCos(HLat, sinHLat, cosHlat);
        lat2 := iLatY  * GU_TORADIANS;
        lon2 := iLongX * GU_TORADIANS;
        SinCos(Lat2, sinLat2, cosLat2);
        SinCos(lon2-HLon,sinll,cosll);
        cll :=  CosLat2 * Cosll;
        cosc := SinHlat * SinLat2 + CosHlat * cll;
        If cosc > 0.9999999999999999999 Then
        begin
          ParentGlobe.Renderer.Points[iIndex] := Point(XOrigin,YOrigin);
          Exit;
        end;
        If cosc < -0.9999999999999999999999 Then
        begin
          ParentGlobe.Renderer.Points[iIndex] := Point(XOrigin,YOrigin);
          Exit;
        end;
        c := arccos(cosc);
        SinCos(c,sinc,cosc);
        kprime := c/sinc;
        plon := kprime*CosLat2*sinll*GU_FROMRADIANS;
        plat := kprime*(CosHLat*SinLat2-SinHLat*cll)*GU_FROMRADIANS;
        ParentGlobe.Renderer.Points[iIndex] := Point(
            XOrigin + Round( plon * ScaleFactor ),
            YOrigin - Round( plat * ScaleFactor ) );
      end;
  end;
end;

{-------------------------------------------------------------------------
 TGAzimuthal2EquidistantPrj.PaintSurface
-------------------------------------------------------------------------}
procedure TGAzimuthal2EquidistantPrj.PaintSurface;
const
  SEGMENTS = 90;
var
  idx : Integer;
  eLastX, eLastY, eX, eY : Extended;
  eCosAngle, eSinAngle : Extended;
begin
  with ParentGlobe do
  begin
    eY := 0;
    eX := GU_EARTHRADIUS * Projector.ScaleFactor * LocalPi;

    Renderer.Points[0] := Point( Round( Projector.XOrigin + eX ), Round( Projector.YOrigin + eY ) );
    SinCos( ( GU_360_DEGREE / SEGMENTS ) * GU_TORADIANS, eSinAngle, eCosAngle );

    idx := 1;
    while idx < SEGMENTS do
    begin
      eLastX := eX;
      eLastY := eY;
      eX := eLastX * eCosAngle - eLastY * eSinAngle;
      eY := eLastX * eSinAngle + eLastY * eCosAngle;
      Renderer.Points[idx] := Point( Round( Projector.XOrigin + eX ), Round( Projector.YOrigin + eY ) );
      Inc( idx );
    end;

    Renderer.BeginDrawPoly( True );
    Renderer.AddClippedPoly( SEGMENTS, 0 );
    Renderer.EndDrawPoly;
  end;
end;

{-------------------------------------------------------------------------
 TGEquidistantPrj.IsContinuous
-------------------------------------------------------------------------}
function TGEquidistantPrj.IsContinuous : Boolean;
begin
  Result := true;
end;

{-------------------------------------------------------------------------
 TGEquidistantPrj.XYtoLL()
-------------------------------------------------------------------------}
{**
  @Param iX Screen X coordinate to convert.
  @Param iY Screen Y coordinate to convert.
  @Param iIndex Position in the gaPoints global array to store the result.
  @Result True if the Point is on the visible surface of the globe.
}
function TGEquidistantPrj.XYToLL( iX, iY, iIndex : Integer ) : Boolean;
var
  x, y, D, sinD, cosD : Extended;
  lat, lon : Extended;
begin
  with ParentGlobe.Projector do
  begin
    x := ( iX - XOrigin ) / ScaleFactor * GU_TORADIANS;
    y := -( iY - YOrigin ) / ScaleFactor * GU_TORADIANS;
    D := Hypot( x, y );
    if D <= TG_EPSILON then
    begin
      lat := 0;
      lon := 0;
      Result := True;
    end
    else
    begin
      SinCos( D, sinD, cosD );
      lat := ArcSin( ( y * sinD ) / D );
      lon := ArcCos( cosD / Cos( lat ) );

    { check to see if the coords are outside the globe }
      Result := ( abs( lon ) < LocalPi ) and ( abs( lat ) < HalfPi );
    end;

    if Result then
      ParentGlobe.Renderer.Points[iIndex] := Point(
        Mod180( Round( lon * GU_FROMRADIANS + CentralMeridian * GU_DEGREE )),
        Round( lat * GU_FROMRADIANS ) );
  end;
end;

{-------------------------------------------------------------------------
 TGEquidistantPrj.LLToXY
-------------------------------------------------------------------------}
{**
  @Param ptLL TGPointLL object to convert to XY screen Coordinates.
  @Param iIndex Position in the gaPoints global array to store the result.
  @Result True if the Point is on the screen.
} 
function TGEquidistantPrj.PointLLToXY( const ptLL : TGPointLL; iIndex : Integer ) : Boolean;
var
  sinlat2, coslat2,
    d, tc, lat2, lon2,
    xcoord, ycoord,
    sinx, cosx : extended;
begin
  with ptLL, ParentGlobe.Projector do
  begin
  { check to see if the coords are outside the globe }
    with ExtentsLL do
      Result := ( iLongX > Left ) and ( iLongX <= Right ) and ( iLatY >= Top ) and ( iLatY <= Bottom );

    if Result then
    begin
      lat2 := iLatY * GU_TORADIANS;
      lon2 := Mod180( iLongX - Round( CentralMeridian * GU_DEGREE)) * GU_TORADIANS;

      SinCos( lat2, sinlat2, coslat2 );
      d := arccos( coslat2 * cos( lon2 ) );
      SinCos( d, sinx, cosx );
      if Abs( sinx ) < 10E-6 then
        tc := 0.0
      else
      begin
        tc := arccos( sinlat2 / sinx );
        if sin( lon2 ) < 0.0 then
          tc := 2.0 * pi - tc;
      end;
      SinCos( tc, sinx, cosx );
      xcoord := Sinx * d * GU_FROMRADIANS; //-pi .. pi
      ycoord := Cosx * d * GU_FROMRADIANS; //-pi .. pi

      ParentGlobe.Renderer.Points[iIndex] := Point(
        XOrigin + Round( xcoord * ScaleFactor ),
        YOrigin - Round( ycoord * ScaleFactor ) );
    end;
  end;
end;

{-------------------------------------------------------------------------
 TGEquidistantPrj.PaintSurface
-------------------------------------------------------------------------}
procedure TGEquidistantPrj.PaintSurface;
const
  SEGMENTS = 90;
var
  idx : Integer;
  eLastX, eLastY, eX, eY : Extended;
  eCosAngle, eSinAngle : Extended;
begin
  with ParentGlobe do
  begin
    eY := 0;
    eX := GU_EARTHRADIUS * Projector.ScaleFactor * LocalPi;

    Renderer.Points[0] := Point( Round( Projector.XOrigin + eX ), Round( Projector.YOrigin + eY ) );
    SinCos( ( GU_360_DEGREE / SEGMENTS ) * GU_TORADIANS, eSinAngle, eCosAngle );

    idx := 1;
    while idx < SEGMENTS do
    begin
      eLastX := eX;
      eLastY := eY;
      eX := eLastX * eCosAngle - eLastY * eSinAngle;
      eY := eLastX * eSinAngle + eLastY * eCosAngle;
      Renderer.Points[idx] := Point( Round( Projector.XOrigin + eX ), Round( Projector.YOrigin + eY ) );
      Inc( idx );
    end;

    Renderer.BeginDrawPoly( True );
    Renderer.AddClippedPoly( SEGMENTS, 0 );
    Renderer.EndDrawPoly;
  end;
end;

{-------------------------------------------------------------------------
 TGCylEqualAreaPrj.XYtoLL()
-------------------------------------------------------------------------}
{**
  @Param iX Screen X coordinate to convert.
  @Param iY Screen Y coordinate to convert.
  @Param iIndex Position in the gaPoints global array to store the result.
  @Result True if the Point is on the visible surface of the globe.
} 
function TGCylEqualAreaPrj.XYToLL( iX, iY, iIndex : Integer ) : Boolean;
var
  x, y, lat, long, eCos : Extended;
begin
  with ParentGlobe.Projector do
  begin
    x := ( iX - XOrigin ) / ScaleFactor * GU_TORADIANS;
    y := -( iY - YOrigin ) / ScaleFactor * GU_TORADIANS;

    begin
      eCos := Cos( FirstParallel * DD_TORADIANS);
      lat := ArcSin( y * eCos );
      Long := ( x / eCos );

    { check to see if the coords are outside the globe }
      Result := ( abs( long ) < LocalPi ) and ( abs( lat ) < HalfPi );
    end;

    if Result then
      ParentGlobe.Renderer.Points[iIndex] := Point(
        Mod180( Round( long * GU_FROMRADIANS + CentralMeridian * GU_DEGREE )),
        Round( lat * GU_FROMRADIANS ) );
  end;
end;

{-------------------------------------------------------------------------
 TGCylEqualAreaPrj.PointLLToXY
-------------------------------------------------------------------------}
{**
  @Param ptLL TGPointLL object to convert to XY screen Coordinates.
  @Param iIndex Position in the gaPoints global array to store the result.
  @Result True if the Point is on the screen.
} 
function TGCylEqualAreaPrj.PointLLToXY( const ptLL : TGPointLL; iIndex : Integer ) : Boolean;
var
  lat, long, eCos,
    xcoord, ycoord : extended;
begin
  with ptLL, ParentGlobe.Projector do
  begin
  { check to see if the coords are outside the globe }
    with ExtentsLL do
      Result := ( iLongX > Left ) and ( iLongX <= Right ) and ( iLatY >= Top ) and ( iLatY <= Bottom );

//    if Result then
    begin
      lat := iLatY * GU_TORADIANS;
      long := Mod180( iLongX - Round( CentralMeridian * GU_DEGREE )) * GU_TORADIANS;

      eCos := Cos( FirstParallel * DD_TORADIANS);
      xcoord := long * eCos * GU_FROMRADIANS;
      ycoord := Sin( lat ) / eCos * GU_FROMRADIANS;

      ParentGlobe.Renderer.Points[iIndex] := Point(
        XOrigin + Round( xcoord * ScaleFactor ),
        YOrigin - Round( ycoord * ScaleFactor ) );
    end;
  end;
end;

{------------------------------------------------------------------------------
 TGBehrmannCylEqualAreaPrj.Create
------------------------------------------------------------------------------}
{**
  @Param  ParentGlobe Globe to associate the Projection with.
} 
constructor TGBehrmannCylEqualAreaPrj.Create( ParentGlobe : TGCustomGlobe5 );
begin
  inherited Create( ParentGlobe );
  ParentGlobe.Projector.FirstParallel := 30; { For Behrmann Projection }
end;

{------------------------------------------------------------------------------
 TGPetersPrj.Create
------------------------------------------------------------------------------}
{**
  @Param  ParentGlobe Globe to associate the Projection with.
}
constructor TGPetersPrj.Create( ParentGlobe : TGCustomGlobe5 );
begin
  inherited Create( ParentGlobe );
  ParentGlobe.Projector.FirstParallel := 45; { For Peters Projection }
end;

{------------------------------------------------------------------------------
 TGAlbersEqualAreaConicPrj.Create
------------------------------------------------------------------------------}
{**
  @Param  ParentGlobe Globe to associate the Projection with.
} 
constructor TGAlbersEqualAreaConicPrj.Create( ParentGlobe : TGCustomGlobe5 );
begin
  inherited Create( ParentGlobe );

  ParentGlobe.Projector.FirstParallel := 45;
  ParentGlobe.Projector.SecondParallel := 0;
end;

{------------------------------------------------------------------------------
 TGAlbersEqualAreaConicPrj.SetProperty
------------------------------------------------------------------------------}
procedure TGAlbersEqualAreaConicPrj.PropertyChanged( ProjectionProperty : TGProjectionProperty );
var
  eCos, eSin : Extended;
begin
 { if either the first or second parallels changed }
  if ( ProjectionProperty = ppFirstParallel ) or ( ProjectionProperty = ppSecondParallel ) then
    with ParentGlobe.Projector do
    begin
      SinCos( FirstParallel * DD_TORADIANS, eSin, eCos );
      n := 0.5 * ( eSin + Sin( SecondParallel * DD_TORADIANS ) );

      if n = 0 then n := 1;

      C := Sqr( eCos ) + 2 * n * eSin;
      Rho0 := Sqrt( C ) / n;
    end;
end;

{------------------------------------------------------------------------------
 TGAlbersEqualAreaConicPrj.PointLLToXY
------------------------------------------------------------------------------}
{**
  @Param ptLL TGPointLL object to convert to XY screen Coordinates.
  @Param iIndex Position in the gaPoints global array to store the result.
  @Result True if the Point is on the screen.
}
function TGAlbersEqualAreaConicPrj.PointLLToXY( const ptLL : TGPointLL; iIndex : Integer ) : Boolean;
var
  lat, long, eCos, eSin : Extended;
  xcoord, ycoord : Extended;
  Rho, Theta : Extended;
begin
  with ptLL, ParentGlobe.Projector do
  begin
  { check to see if the coords are outside the globe }
    with ExtentsLL do
      Result := ( iLongX > Left ) and ( iLongX <= Right ) and ( iLatY >= Top ) and ( iLatY <= Bottom );

    if Result then
    begin
      lat := iLatY * GU_TORADIANS;
      long := Mod180( iLongX - Round( CentralMeridian * GU_DEGREE )) * GU_TORADIANS;

      theta := n * ( long );
      Rho := Sqrt( C - 2 * n * Sin( lat ) ) / n;

      SinCos( Theta, eSin, eCos );

      xcoord := Rho * eSin * GU_FROMRADIANS;
      ycoord := ( Rho0 - Rho * eCos ) * GU_FROMRADIANS;

      ParentGlobe.Renderer.Points[iIndex] := Point(
        XOrigin + Round( xcoord * ScaleFactor ),
        YOrigin - Round( ycoord * ScaleFactor ) );
    end;
  end;
end;

{------------------------------------------------------------------------------
 TGAlbersEqualAreaConicPrj.XYToLL
------------------------------------------------------------------------------}
{**
  @Param iX Screen X coordinate to convert.
  @Param iY Screen Y coordinate to convert.
  @Param iIndex Position in the gaPoints global array to store the result.
  @Result True if the Point is on the visible surface of the globe.
} 
function TGAlbersEqualAreaConicPrj.XYToLL( iX, iY, iIndex : Integer ) : Boolean;
var
  x, y, lat, long, Rho, Theta : Extended;
begin
  with ParentGlobe.Projector do
  begin
    x := ( iX - XOrigin ) / ScaleFactor * GU_TORADIANS;
    y := -( iY - YOrigin ) / ScaleFactor * GU_TORADIANS;

    Rho := Sqrt( x * x + Sqr( Rho0 - y ) );
    Theta := ArcTan( x / ( Rho0 - y ) );

    begin
      lat := ArcSin( ( C - Rho * Rho * n * n ) / ( n + n ) );
      Long := Theta / n;

    { check to see if the coords are outside the globe }
      Result := ( abs( long ) < LocalPi ) and ( abs( lat ) < HalfPi );
    end;

    if Result then
      ParentGlobe.Renderer.Points[iIndex] := Point(
        Mod180( Round( long * GU_FROMRADIANS + CentralMeridian * GU_DEGREE)), Round( lat * GU_FROMRADIANS ) );
  end;
end;

{------------------------------------------------------------------------------
 TGBonnePrj.Create
------------------------------------------------------------------------------}
{**
  @Param  ParentGlobe Globe to associate the Projection with.
}
constructor TGBonnePrj.Create( ParentGlobe : TGCustomGlobe5 );
begin
  inherited Create( ParentGlobe );

  ParentGlobe.Projector.FirstParallel := 90;
end;

{------------------------------------------------------------------------------
 TGBonnePrj.SetProperty
------------------------------------------------------------------------------}
procedure TGBonnePrj.PropertyChanged( ProjectionProperty : TGProjectionProperty );
begin
  inherited;
  
 { if the first parallel changes }
  if ProjectionProperty = ppFirstParallel then
    with ParentGlobe.Projector do
      if FirstParallel = 0 then
        cotSP := LocalPI
      else
        cotSP := 1 / Tan( FirstParallel * DD_TORADIANS);
end;

{------------------------------------------------------------------------------
 TGBonnePrj.PointLLToXY
------------------------------------------------------------------------------}
{**
  @Param ptLL TGPointLL object to convert to XY screen Coordinates.
  @Param iIndex Position in the gaPoints global array to store the result.
  @Result True if the Point is on the screen.
}
function TGBonnePrj.PointLLToXY( const ptLL : TGPointLL; iIndex : Integer ) : Boolean;
var
  lat, long : Extended;
  xcoord, ycoord : Extended;
  Rho, E : Extended;
begin
  with ptLL, ParentGlobe.Projector do
  begin
  { check to see if the coords are outside the globe }
    with ExtentsLL do
      Result := ( iLongX > Left ) and ( iLongX <= Right ) and ( iLatY >= Top ) and ( iLatY <= Bottom );

    if Result then
    try
      lat := iLatY * GU_TORADIANS;
      long := Mod180( iLongX - Round( CentralMeridian * GU_DEGREE )) * GU_TORADIANS;

      Rho := ( cotSP + FirstParallel * DD_TORADIANS - lat );
      E := long * ( Cos( lat ) / Rho );

      xcoord := Rho * Sin( E );
      ycoord := ( cotSP - Rho * Cos( E ) );

      ParentGlobe.Renderer.Points[iIndex] := Point(
        XOrigin + Round( xcoord * ScaleFactor * GU_FROMRADIANS ),
        YOrigin - Round( ycoord * ScaleFactor * GU_FROMRADIANS ) );
    except
    end;
  end;
end;

{------------------------------------------------------------------------------
 TGBonnePrj.XYToLL
------------------------------------------------------------------------------}
{**
  @Param iX Screen X coordinate to convert.
  @Param iY Screen Y coordinate to convert.
  @Param iIndex Position in the gaPoints global array to store the result.
  @Result True if the Point is on the visible surface of the globe.
} 
function TGBonnePrj.XYToLL( iX, iY, iIndex : Integer ) : Boolean;
var
  x, y, lat, long, Rho : Extended;
begin
  with ParentGlobe.Projector do
  begin
    x := ( iX - XOrigin ) / ScaleFactor * GU_TORADIANS;
    y := -( iY - YOrigin ) / ScaleFactor * GU_TORADIANS;

    Rho := Sign( FirstParallel ) * Sqrt( x * x + Sqr( cotSP - y ) );

    lat := cotSP + FirstParallel * DD_TORADIANS - Rho;
    Long := ( Rho / Cos( lat ) ) * ArcTan2( x, cotSP - y );

   { check to see if the coords are outside the globe }
    Result := ( abs( long ) < LocalPi ) and ( abs( lat ) < HalfPi );

    if Result then
      ParentGlobe.Renderer.Points[iIndex] := Point(
        Mod180( Round( CentralMeridian * GU_DEGREE + long * GU_FROMRADIANS )),
        Round( lat * GU_FROMRADIANS ));
  end;
end;

{------------------------------------------------------------------------------
 TGSinusoidalPrj.PointLLToXY
------------------------------------------------------------------------------}
{**
  @Param ptLL TGPointLL object to convert to XY screen Coordinates.
  @Param iIndex Position in the gaPoints global array to store the result.
  @Result True if the Point is on the screen.
}
function TGSinusoidalPrj.PointLLToXY( const ptLL : TGPointLL; iIndex : Integer ) : Boolean;
var
  xcoord, ycoord : Extended;
begin
  with ptLL, ParentGlobe.Projector do
  begin
  { check to see if the coords are outside the globe }
    with ExtentsLL do
      Result := ( iLongX > Left ) and ( iLongX <= Right ) and ( iLatY >= Top ) and ( iLatY <= Bottom );

    if Result then
    begin
      ycoord := iLatY * GU_TORADIANS;
      xcoord := Mod180( iLongX - Round( CentralMeridian * GU_DEGREE )) * GU_TORADIANS * Cos( ycoord );

      ParentGlobe.Renderer.Points[iIndex] := Point(
        XOrigin + Round( xcoord * GU_FROMRADIANS * ScaleFactor ),
        YOrigin - Round( ycoord * GU_FROMRADIANS * ScaleFactor ) );
    end;
  end;
end;

{------------------------------------------------------------------------------
 TGSinusoidalPrj.XYToLL
------------------------------------------------------------------------------}
{**
  @Param iX Screen X coordinate to convert.
  @Param iY Screen Y coordinate to convert.
  @Param iIndex Position in the gaPoints global array to store the result.
  @Result True if the Point is on the visible surface of the globe.
}
function TGSinusoidalPrj.XYToLL( iX, iY, iIndex : Integer ) : Boolean;
var
  x, y, lat, long : Extended;
begin
  with ParentGlobe.Projector do
  begin
    x := ( iX - XOrigin ) / ScaleFactor * GU_TORADIANS;
    y := -( iY - YOrigin ) / ScaleFactor * GU_TORADIANS;

    lat := y;
    Long := x / Cos( lat );

   { check to see if the coords are outside the globe }
    Result := ( abs( long ) < LocalPi ) and ( abs( lat ) < HalfPi );

    if Result then
      ParentGlobe.Renderer.Points[iIndex] := Point(
        Mod180( Round( CentralMeridian * GU_DEGREE + long * GU_FROMRADIANS ) ),
        Round( lat * GU_FROMRADIANS ) );
  end;
end;

{------------------------------------------------------------------------------
  TGGnomonicPrj.IsContinuous
------------------------------------------------------------------------------}
function TGGnomonicPrj.IsContinuous : Boolean;
begin
  Result := true;
end;

{------------------------------------------------------------------------------
  TGGnomonicPrj.PointLLToXY
------------------------------------------------------------------------------}
function TGGnomonicPrj.PointLLToXY(const ptLL: TGPointLL; iIndex: Integer): Boolean;
var
  CosC, k : Extended;
begin
  with ptLL, ParentGlobe.Projector do
  begin

    cosC := Sin( CentralParallel * DD_TORADIANS )
      * Sin( iLatY * GU_TORADIANS )
      + Cos( CentralParallel * DD_TORADIANS )
      * Cos( iLatY * GU_TORADIANS )
      * Cos( iLongX * GU_TORADIANS - ( CentralMeridian * DD_TORADIANS ));

    Result := CosC > 0.0;

    if not Result then
      Exit;

    k := 1 / CosC;

    ParentGlobe.Renderer.Points[iIndex] := Point(
      XOrigin + Round( GU_EARTHRADIUS * GU_TORADIANS * k * Cos( iLatY * GU_TORADIANS )
        * Sin( iLongX * GU_TORADIANS - ( CentralMeridian * DD_TORADIANS ))
        * GU_FROMRADIANS * ScaleFactor),
      YOrigin + Round( GU_EARTHRADIUS * GU_TORADIANS * k * ( Cos( CentralParallel * DD_TORADIANS )
        * Sin( iLatY * GU_TORADIANS )
        - Sin( CentralParallel * DD_TORADIANS )
        * Cos( iLatY * GU_TORADIANS )
        * Cos( iLongX * GU_TORADIANS - ( CentralMeridian * DD_TORADIANS )))
        * GU_FROMRADIANS * ScaleFactor));
  end;
end;

function TGGnomonicPrj.XYToLL(iX, iY, iIndex: Integer): Boolean;
begin
  // ToDO: Not implemented yet
  Result := False;
end;

initialization
  RegisterGlobeClass( TGAlbersEqualAreaConicPrj, 'AlbersEqualAreaConic Projection' );
  RegisterGlobeClass( TGAzimuthal2EquidistantPrj, 'Azimuthal2Equidistant Projection' );
  RegisterGlobeClass( TGAzimuthalEquidistantPrj, 'AzimuthalEquidistant Projection' );
  RegisterGlobeClass( TGBehrmannCylEqualAreaPrj, 'BehrmannCylEqualArea Projection' );
  RegisterGlobeClass( TGBonnePrj, 'Bonne Projection' );
  RegisterGlobeClass( TGCylEqualAreaPrj, 'CylEqualArea Projection' );
  RegisterGlobeClass( TGEquidistantPrj, 'Equidistant Projection' );
  RegisterGlobeClass( TGPetersPrj, 'Peters Projection' );
  RegisterGlobeClass( TGGnomonicPrj, 'Gnomonic Projection' );
  RegisterGlobeClass( TGSinusoidalPrj, 'Sinusoidal Projection' );
end.

