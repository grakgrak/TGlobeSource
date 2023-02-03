//-------------------------------------------------
//Summary
//TGlobe Utility Functions
//
//Description
//Useful functions for working with TGlobe objects.
//
//Author
//Graham Knight (tglobe@tglobe.com)
//-------------------------------------------------
unit Globe5Utils;

interface

uses WinTypes, WinProcs, Classes, SysUtils, Graphics, Globe5, GSysUtils,
  GMapObjects, GClasses, Math;

type
  //------------------------------------------------------------------------------
  //Description
  //Used to calculate Quantiles for a layer. All the objects in the layer are
  //sorted by the requested numeric attribute and then the ordered list is divided
  //into equal numbered classes.
  //
  //This object can be used as part of a Thematic Presenter to provide a
  //segmentation of the value space on a layer based on Quantiles.
  //------------------------------------------------------------------------------
  TGQuantile = class( TObject )
  private
    FLayer : TGLayer;
    FValueColumn: integer;
    FNoDataValue : Double;
    FDataValues : array of Double;
    FColorArray : array of TColor;
  protected
    procedure SortValues( L, R : Integer );
  public
    constructor Create( aLayer : TGLayer; valueColumn : integer; noDataValue : Double );

    procedure Colors( classColors : array of TColor );
    procedure ColorGradient( classColors : array of TColor; classCount : integer );

    function ClassifyToIndex( value : Double ) : integer;
    function ClassifyToColor( value : Double ) : TColor;

    function ClassColor( index : integer ) : TColor;
    function ClassValue( index : integer ) : Double;

    function ValueColumn : integer;
    function ColorCount : integer;
  end;


//-------------------------------------------------------------------------------
//Description
//Returns a point on the great circle defined by a given point,
//a distance and an angle from that point. An angle of 0 is due North.
//
//Parameters
//ptLL :      The point to start from.
//iAngle :    The angle in GlobeUnits to travel in.
//iDistance : The distance in GlobeUnits along the Great Circle line to travel.
//-------------------------------------------------------------------------------
function AngleDistanceToGCPoint(const ptLL : TGPointLL; iAngle, iDistance : Integer) : TGPointLL;
function DistanceLLToLL(const FromLL, ToLL : TGPointLL; Spheroid : TGSpheroid = WGS84) : Integer;
function AngleLLToLL(const FromLL, ToLL : TGPointLL) : Integer;
function GreatCirclePoint(const FromLL, ToLL : TGPointLL; alpha : Extended) : TGPointLL;

procedure DrawProjectedMER(Globe : TGlobe5; const mer : TGMER);

function SplitPolyObject(APoly : TGMapPoly; ALayer : TGLayer) : integer;
function CombinePolyObjects(const Polys : array of TGMapPoly; ALayer : TGLayer) : TGMapPoly;
procedure ConcatPoints(ptStore1, ptStore2 : TGPointStore);

procedure ZoomToLayers(Layers : TGLayerStore);
procedure ZoomToLayer(aLayer : TGLayer);
procedure ZoomToMER(Globe : TGlobe5; MER : TGMER);
procedure ZoomToObject(Globe : TGlobe5; Obj : IGMapPoint);
procedure ZoomToSelectedObjects(Globe : TGlobe5);

function IsMERVisible(Globe : TGCustomGlobe5; MER : TGMER) : Boolean;
function IsObjectVisible(Globe : TGCustomGlobe5; Obj : IGMapPoint) : Boolean;
function IsObjectVisibleInRect(const Globe : TGCustomGlobe5; const Obj : IGMapPoint;
  const aRect : TRect) : boolean;
function SamePixelPos(Projector: TGProjector; const ptLL1, ptLL2 : TGPointLL): Boolean;

function GetVersionString( const ExeName, VerStr : String ) : String;
function GetMachineName : String;

function ColorGradient( startColor, endColor : TColor; alpha : Double ): TColor;
function AlphaBlend( source, target : TColor; alpha : Double ) : TColor;
procedure LayerStats( aLayer : TGLayer; column : integer; const noDataValue : Double;
  var minValue : Double; var maxValue : Double; var meanValue : Double; var sumValue : Double );
function ValueSegment( value, minValue, maxValue, noDataValue : Double; breaks : integer ): Double;

function RenderSunShadow(Globe : TGlobe5; GMTDateTime : TDateTime; ColorNight: TColor;
  TwilightDegrees : integer = 12; MaxAlpha : Byte = 160) : TGPointLL;
{------------------------------------------------------------------------------}
implementation


{ TGQuantile }

var
  SortQuantile : TGQuantile;

function SortByValue(Item1, Item2: Pointer ): integer;
var
  obj1, obj2 : IGMapPoint;
  val1, val2 : Double;
begin
  obj1 := SortQuantile.FLayer.ObjectSource.ByPrimaryKey(TGPrimaryKey(Item1));
  obj2 := SortQuantile.FLayer.ObjectSource.ByPrimaryKey(TGPrimaryKey(Item2));

  val1 := obj1.Value[SortQuantile.FValueColumn];
  val2 := obj2.Value[SortQuantile.FValueColumn];

  if val1 = val2 then
    Result := 0
  else
    if val1 < val2 then
      Result := -1
    else
      Result := 1;
end;

//--------------------------------------------------------------------------
//Description
//\Returns the Color assigned to a specified classification of the Quantile.
//--------------------------------------------------------------------------
function TGQuantile.ClassColor(index: integer): TColor;
begin
  if ( index >= 0 ) and ( index < Length( FColorArray )) then
    Result := FColorArray[index]
  else
    Result := clNone;
end;

//--------------------------------------------------------------------------
//Description
//\Returns the index of the class that the value belongs to in the Quantile.
//--------------------------------------------------------------------------
function TGQuantile.ClassifyToColor(value: Double): TColor;
begin
  Result := ClassColor( ClassifyToIndex( value ));
end;

//-----------------------------------------------------------------------
//Description
//\Returns the index of the quantile in which the supplied value belongs.
//-----------------------------------------------------------------------
function TGQuantile.ClassifyToIndex( value : Double ) : integer;
var
  idx : integer;
begin
  Result := clNone;
  if ( Length( FColorArray ) > 0 ) and ( Length( FDataValues ) < Length( FColorArray )) then
    Exit;

  if IsNan( value ) or ( value = FNoDataValue ) then
    Exit;

  for idx := 1 to High( FColorArray ) do
    if value < ClassValue(idx) then
    begin
      Result := idx - 1;
      Exit;
    end;

  Result := High( FColorArray );
end;

//------------------------------------------------------------------------
//Description
//\Returns the maximum value in the class specifed by the index parameter.
//------------------------------------------------------------------------
function TGQuantile.ClassValue(index: integer): Double;
var
  step : Double;
  idx : integer;
begin
  step := Length( FDataValues ) / Length( FColorArray );

  idx := Trunc(index * step);
  if idx < Length( FDataValues ) then
    Result := FDataValues[idx]
  else
    Result := NaN;
end;

//-----------------------------------------------------------------------------------
//Description
//Creates the classification colors for the Quantile. A gradient of colors is
//automatically generated between the supplied colors.
//
//Parameters
//classColors :  an array of colors to use to generated the classes. 2 or more colors
//               must be provided.
//classCount :   The number of classes to generate. This must be equal to or greater
//               than the number of colors supplied.                                 
//-----------------------------------------------------------------------------------
function TGQuantile.ColorCount: integer;
begin
  Result := Length( FColorArray );
end;

//---------------------------------------------------------------------------
//Description
//Allows the colors for the quantile classes to be defined as an interpolated
//gradient of colors between 2 or more supplied colors.
//
//Parameters
//classColors :  2 or more colors to use to define the color gradient.
//classCount :   The number of classes to generate from the supplied colors. 
//---------------------------------------------------------------------------
procedure TGQuantile.ColorGradient( classColors : array of TColor; classCount : integer );
var
  idx, jdx : integer;
  segCount : Double;
begin
  if classCount <= Length( classColors ) then
  begin
    Colors( classColors );
    Exit;
  end;

  SetLength( FColorArray, classCount );

  segCount := ( classCount - 1 ) / High( classColors );

  for idx := 0 to classCount - 2 do
  begin
    jdx := Trunc( idx / segCount );

    FColorArray[idx]:= AlphaBlend( classColors[jdx + 1], classColors[jdx],
      (idx - jdx * segCount ) / segCount);
  end;
  FColorArray[High(FColorArray)] := classColors[High(classColors)];
end;

//----------------------------------------------------------------------
//Description
//Generates the same number of classes as the number of colors supplied.
//
//Parameters
//classColors :  an array of colors to define the classes.              
//----------------------------------------------------------------------
procedure TGQuantile.Colors(classColors: array of TColor);
var
  idx : integer;
begin
  SetLength( FColorArray, Length( classColors ));

  for idx := 0 to High( classColors ) do
    FColorArray[idx] := classColors[idx];
end;

//---------------------------------------------------------------------------
//Description
//Scans the layer and internally orders the objects by the supplied attribute
//column.
//
//Parameters
//aLayer :       The layer to scan.
//valueColumn :  The attribute column to use to order the objects.
//noDataValue :  Any matching objects with this value are ignored.
//---------------------------------------------------------------------------
constructor TGQuantile.Create( aLayer : TGLayer; valueColumn : integer; noDataValue : Double );
var
  obj : IGMapPoint;
  val : double;
  idx, count : integer;
begin
  FLayer := aLayer;
  FValueColumn := valueColumn;
  FNoDataValue := noDataValue;

  // build list of data objects
  obj := FLayer.ObjectSource.FirstObject;
  count := 0;
  SetLength( FDataValues, FLayer.ObjectSource.ObjectCount );
  while obj <> nil do
  begin
    val := obj.Value[valueColumn];
    if not isNan( val ) and ( val <> FNoDataValue ) then
    begin
      FDataValues[count] := val;
      Inc( count );
    end;
    obj := FLayer.ObjectSource.NextObject( obj );
  end;

  if count > 1 then
  begin
    SortValues( 0, count - 1);

    // remove duplicates from the list
    for idx := count - 1 downto 1 do
      if FDataValues[idx] = FDataValues[idx - 1] then
      begin
        Move( FDataValues[idx], FDataValues[idx - 1], Sizeof( Double ) * (count - idx));
        Dec( count );
      end;
  end;

  SetLength( FDataValues, count );
end;

procedure TGQuantile.SortValues( L, R : Integer );
var
  I, J: Integer;
  P, T: Double;
begin
  repeat
    I := L;
    J := R;
    P := FDataValues[(L + R) shr 1];
    repeat
      while ( FDataValues[I] - P ) < 0.0 do
        Inc(I);
      while ( FDataValues[J] - P ) > 0.0 do
        Dec(J);
      if I <= J then
      begin
        T := FDataValues[I];
        FDataValues[I] := FDataValues[J];
        FDataValues[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      SortValues(L, J);
    L := I;
  until I >= R;
end;

//-----------------------------------------------------------------------------
//Description
//The column from the layers attributes to use as the value for calculating the
//quantile.                                                                    
//-----------------------------------------------------------------------------
function TGQuantile.ValueColumn: integer;
begin
  Result := FValueColumn;
end;

//-------------------------------------------------------------------------------
//Description
//Returns a point on the great circle defined by a given point,
//a distance and an angle from that point. An angle of 0 is due North.
//
//Parameters
//ptLL :      The point to start from.
//iAngle :    The angle in GlobeUnits to travel in.
//iDistance : The distance in GlobeUnits along the Great Circle line to travel.
//-------------------------------------------------------------------------------
function AngleDistanceToGCPoint(const ptLL : TGPointLL; iAngle, iDistance : integer) : TGPointLL;
var
  sinlat1, coslat1, sind, cosd, sintc, costc : Extended;
  dlon, tlat, tlon : Extended;
begin
  with PtLL do
  begin
    SinCos(ilatY * GU_TORADIANS, sinlat1, coslat1);
    SinCos(iDistance / GU_EARTHRADIUS, sind, cosd);
    SinCos(iAngle * GU_TORADIANS, sintc, costc);
    tlat := arcsin(sinlat1 * cosd + coslat1 * sind * costc);
    dlon := arctan2(sintc * sind * coslat1, cosd - sinlat1 * sin(tlat));
    tlon := SphericalMod(ilongX * GU_TORADIANS - dlon + LocalPI) - LocalPI;
  end;
  Result := PointLL(Round(tlon * GU_FROMRADIANS), Round(tlat * GU_FROMRADIANS));
end;

//-----------------------------------------------------------------------------
//Description
//Calculates the position of a point that lies on the great circle line between
//the two supplied points.
//
//If alpha is 0.0 then the FromLL is returned. If alpha is 1.0 then the ToLL is
//returned.
//
//Parameters
//FromLL :  Starting point
//ToLL :    Endsing point
//alpha :   The location of the destination point.                             
//-----------------------------------------------------------------------------
function GreatCirclePoint(const FromLL, ToLL : TGPointLL; alpha : Extended) : TGPointLL;
var
  a, b : TGVec3D;
  mat : TGMatrix;
  quat : TGQuaternion;
begin
  a := V3D(FromLL);
  b := V3D(ToLL);

  Quat_FromAxisAngle(quat, Vec3D_Cross(a,b),-ArcCos(Vec3D_Dot(a,b)) * alpha);
  Quat_ToMatrix(quat, mat);

  Result := Vec3D_ToPointLL( Vec3D_MatrixMultiply(a, mat));
end;

//-------------------------------------------------------------------------------
//Description
//Calculates the Angle between the supplied points. The angle is returned in
//Globeunits with 0 being due north. To convert the result to Radians multiply by
//the GU_TORADIANS constant.
//
//Parameters
//FromLL :  The starting point
//ToLL :    The ending point.
//-------------------------------------------------------------------------------
function AngleLLToLL(const FromLL, ToLL : TGPointLL) : integer;
var
  SLat1, SLat2, CLat1, CLat2 : Extended;
  SLonDiff, CLonDiff : Extended;
begin
  SinCos(FromLL.iLatY * GU_TORADIANS, SLat1, CLat1);
  SinCos(ToLL.iLatY * GU_TORADIANS, SLat2, CLat2);
  SinCos(LongDiff(ToLL.iLongX, FromLL.iLongX) * GU_TORADIANS, SLonDiff, CLonDiff);

  Result := Round(SphericalMod(ArcTan2(SLonDiff * CLat2,
    CLat1 * SLat2 - SLat1 * CLat2 * CLonDiff)) * GU_FROMRADIANS);
end;


//-------------------------------------------------------------------------------
//Description
//Calculates the Distance between the supplied points. This method uses an
//Ellipsoid model of the earth to calculate more acurate distances.
//
//Parameters
//FromLL :  The starting point.
//ToLL :    The ending point.
//Spheroid : The Ellipsoid to use to calculate the distance over.
//-------------------------------------------------------------------------------
function DistanceLLToLL(const FromLL, ToLL : TGPointLL; Spheroid : TGSpheroid) : Integer;
var
  C, c_value_1, c_value_2, c2a, cy, cz : Extended;
  D, E, r_value : Extended;
  S, s_value_1, sA, sy : Extended;
  tangent_1, tangent_2, X, Y : Extended;
  Heading_FromTo, Heading_ToFrom : Extended;
  term1, term2 : Extended;
  Flattening : Extended;
begin
  Result := 0;

  if (FromLL.iLongX - ToLL.iLongX = 0) and (FromLL.iLatY - ToLL.iLatY = 0) then
    Exit;

  Flattening := SpheroidData[Ord(Spheroid)].f;

  r_value := 1.0 - Flattening;
  tangent_1 := (r_value * Sin(FromLL.iLatY * GU_TORADIANS)) / Cos(FromLL.iLatY * GU_TORADIANS);
  tangent_2 := (r_value * Sin(ToLL.iLatY * GU_TORADIANS)) / Cos(ToLL.iLatY * GU_TORADIANS);
  c_value_1 := 1.0 / Sqrt((tangent_1 * tangent_1) + 1.0);
  s_value_1 := c_value_1 * tangent_1;
  c_value_2 := 1.0 / Sqrt((tangent_2 * tangent_2) + 1.0);
  S := c_value_1 * c_value_2;

  Heading_ToFrom := S * tangent_2; { backward_azimuth }
  Heading_FromTo := Heading_ToFrom * tangent_1;

  X := ToLL.iLongX * GU_TORADIANS - FromLL.iLongX * GU_TORADIANS;

  repeat
    tangent_1 := c_value_2 * Sin(X);
    tangent_2 := Heading_ToFrom - (s_value_1 * c_value_2 * Cos(X));
    sy := Sqrt((tangent_1 * tangent_1) + (tangent_2 * tangent_2));
    cy := (S * Cos(X)) + Heading_FromTo;
    Y := ArcTan2(sy, cy);
    sA := (S * Sin(X)) / sy;
    c2a := (-sA * sA) + 1.0;
    cz := Heading_FromTo + Heading_FromTo;

    if c2a > 0.0 then
      cz := (-cz / c2a) + cy;

    E := (cz * cz * 2.0) - 1.0;
    C := (((((-3.0 * c2a) + 4.0) * Flattening) + 4.0) * c2a * Flattening) / 16.0;
    D := X;
    X := ((((E * cy * C) + cz) * sy * C) + Y) * sA;
    X := ((1.0 - C) * X * Flattening) + ToLL.iLongX * GU_TORADIANS - FromLL.iLongX * GU_TORADIANS;
  until Abs(D - X) < 5.0E20;

  X := Sqrt((((1.0 / r_value / r_value) - 1) * c2a) + 1.0) + 1.0;
  X := (X - 2.0) / X;
  C := 1.0 - X;
  C := (((X * X) * 0.25) + 1.0) / C;
  D := ((0.375 * (X * X)) - 1.0) * X;
  X := X * cy;

  S := (1.0 - E) - E;

  term1 := ((sy * sy * 4.0) - 3.0) * (((S * cz * D) / 6.0) - X);
  term2 := ((((term1 * D) * 0.25) + cz) * sy * D) + Y;

  Result := Round(term2 * C * SpheroidData[Ord(Spheroid)].r * r_value);
end;

//----------------------------------------------------------------------------
//Description
//Splits the chains in a poly object into individual objects
//
//Parameters
//APoly :   An object to split.
//ALayer :  ALayer The Layer on which to create the individual objects from APoly.
//----------------------------------------------------------------------------
function SplitPolyObject(APoly : TGMapPoly; ALayer : TGLayer) : integer;
var
  idx : integer;
begin
  Result := APoly.Chains.Count;
  for idx := 0 to Result - 1 do
    with TGMapPoly.Create do
    begin
      ObjectDataCSL := APoly.ObjectDataCSL;
      Chains.Count := 1;
      Chains[0] := APoly.Chains[idx].Clone;
      APoly.Chains[idx] := nil;
    end;
  APoly.Chains.Count := 0;
end;

//----------------------------------------------------------------------------
//Description
//Combines an array of poly objects into a single object. The objects are just
//appended to a single chainstore in the resultant object.
//
//Parameters
//Polys :   An array of Poly objects
//ALayer :  The layer to store the resultant object onto
//----------------------------------------------------------------------------
function CombinePolyObjects(const Polys : array of TGMapPoly; ALayer : TGLayer) : TGMapPoly;
var
  idx, jdx, iChains : integer;
begin
  Result := TGMapPoly.Create;
//ToDo:  Result.Title := 'New Object';

  iChains := 0;
  for idx := 0 to High(Polys) do
    Inc(iChains, Polys[idx].Chains.Count);

  Result.Chains.Count := iChains;

  iChains := 0;
  for idx := 0 to High(Polys) do
  begin
    for jdx := 0 to Polys[idx].Chains.Count - 1 do
    begin
      Result.Chains[iChains] := Polys[idx].Chains[jdx].Clone;
      //      Polys[idx].Chains[jdx] := nil;
      Inc(iChains);
    end;
  end;
end;

//--------------------------------------------------------------------
//Description
//Concatonates two sets of points together into a single TGPointStore.
//
//Parameters
//ptStore1 :  The target pointstore
//ptStore2 :  The pointstore to append to the target pointstore
//--------------------------------------------------------------------
procedure ConcatPoints(ptStore1, ptStore2 : TGPointStore);
var
  idx : integer;
begin
  for idx := 0 to ptStore2.Count - 1 do
    ptStore1.Add(ptStore2[idx]);
end;

//-----------------------------------------------------------------------------
//Description
//Renders a projected MER onto the Globe's surface. This should only be callued
//during the OnRender or OnPaint events.
//
//Parameters
//Globe :  The Globe component to render to.
//mer :    The MER to render.
//-----------------------------------------------------------------------------
procedure DrawProjectedMER(Globe : TGlobe5; const mer : TGMER);
var
  startLong : integer;
begin
  // display the bounding rectangle
  startLong := mer.iLongX + 1;

  // draw lines of Longitude in 90 degree steps to avoid the 180 degree direction problem
  while startLong + GU_90_DEGREE < MER_iRightX(mer) do
  begin
    globe.RenderLine( PointLL( startLong, mer.iLatY + 1 ), PointLL( startLong + GU_90_DEGREE, mer.iLatY + 1 ), 16);
    globe.RenderLine( PointLL( startLong, MER_iTopY(mer) - 1), PointLL( startLong + GU_90_DEGREE, MER_iTopY(mer) - 1), 16);
    startLong := startLong + GU_90_DEGREE;
  end;
  globe.RenderLine( PointLL( startLong, mer.iLatY + 1 ), PointLL( MER_iRightX(mer) - 1, mer.iLatY + 1 ), 16);
  globe.RenderLine( PointLL( startLong, MER_iTopY(mer) - 1), PointLL( MER_iRightX(mer) - 1, MER_iTopY(mer) - 1), 16);

  // Draw lines of latitude
  globe.RenderLine(PointLL(MER_iRightX(mer) - 1, mer.iLatY + 1), PointLL( MER_iRightX(mer) - 1, MER_iTopY(mer) - 1), 32);
  globe.RenderLine(PointLL(mer.iLongX + 1, mer.iLatY + 1), PointLL(mer.iLongX + 1, MER_iTopY(mer) - 1), 32);
end;

//----------------------------------------------------------------------
//Description
//Zooms and positions the globe so that the supplied MER is just visible
//
//Parameters
//Globe :  The Globe to adjust.
//MER :    The MER to zoom and position the globe to.
//----------------------------------------------------------------------
procedure ZoomToMER(Globe : TGlobe5; MER : TGMER);
begin
  Globe.Projector.CenterXY := Point(Globe.Width div 2, Globe.Height div 2);
  with MER do
  begin
    if (iWidthX <> 0) and (iHeightY <> 0) then
      Globe.ViewRect := Rect(0, 0, iWidthX, iHeightY);

    TGlobe5(Globe).LocateToLL(iLongX + iWidthX div 2, iLatY + iHeightY div 2);
  end;
end;

//----------------------------------------------------------------------------
//Description
//Adjusts the zoom level so that all of the selected objects are just visible.
//----------------------------------------------------------------------------
procedure ZoomToSelectedObjects(Globe : TGlobe5);
var
  MER : TGMER;
  obj : IGMapPoint;
begin
  obj := Globe.Selection.First;
  if obj <> nil then
  begin
    MER := obj.ObjectMER;

    while obj <> nil do
    begin
      MER := MER_Union(MER, obj.ObjectMER);
      obj := Globe.Selection.Next;
    end;

    ZoomToMER(Globe, MER);
  end;
end;

//------------------------------------------------------------------------------
//Description
//Adjusts the Globe's Zoom and position so that all the the supplied layers
//are just visible.
//
//Parameters
//Layers :  A LayerStore to Zoom to.
//------------------------------------------------------------------------------
procedure ZoomToLayers(Layers : TGLayerStore);
var
  idx : Integer;
  MER : TGMER;
begin
  if Layers.Count = 0 then Exit;

  MER := Layers[0].LayerMER;

  for idx := 1 to Layers.Count - 1 do
    MER := MER_Union( MER, Layers[idx].LayerMER);

  ZoomToMER(TGLobe5(Layers.ParentGlobe), MER);
end;

//------------------------------------------------------------------------------
//Description
//Adjusts the Globe's Zoom and position so that the supplied layer is just visible.
//
//Parameters
//aLayer :  Layer to Zoom to.
//------------------------------------------------------------------------------
procedure ZoomToLayer(aLayer : TGLayer);
begin
  ZoomToMER(TGlobe5(aLayer.ParentGlobe), aLayer.LayerMER);
end;

//------------------------------------------------------------------------------
//Description
//Adjusts the zoom and location of the globe so that the supplied object is just
//visible.
//
//Parameters
//Globe :  The Globe component to zoom
//Obj :    The object to zoom and position to
//------------------------------------------------------------------------------
procedure ZoomToObject(Globe : TGlobe5; Obj : IGMapPoint);
begin
  ZoomToMER(Globe, Obj.ObjectMER);
end;

//------------------------------------------------------------------------------
//Description
//Checks to see if the supplied MER is visible on the Globe component.
//
//Parameters
//Globe : TGlobe object check against.
//MER :   The MER to check.
//------------------------------------------------------------------------------
function IsMERVisible(Globe : TGCustomGlobe5; MER : TGMER) : Boolean;
var
  aState : TGRenderStateSet;
begin
  Result := Globe.Projector.ProjectionModel.VisibleMER(MER, aState);
end;

//------------------------------------------------------------------------------
//Description
//Checks to see if the supplied object is visible on the Globe component.
//
//Parameters
//Globe :  TGlobe object check against.
//Obj :    Object to Check if Visible.
//------------------------------------------------------------------------------
function IsObjectVisible(Globe : TGCustomGlobe5; Obj : IGMapPoint) : Boolean;
begin
  Result := IsObjectVisibleInRect( Globe, Obj, Globe.ClientRect );
end;

//------------------------------------------------------------------------------
//Description
//Tests to see if an object intersects the supplied device rectangle.
//
//Parameters
//Globe :  Globe object that is displaying the map object
//Obj :    Map object to test
//aRect :  Rectange in device coordinates to check against.
//------------------------------------------------------------------------------
function IsObjectVisibleInRect(const Globe : TGCustomGlobe5; const Obj : IGMapPoint;
  const aRect : TRect) : boolean;
var
  iChain, idx : integer;
  ptXY : TPoint;
  ptLL : TGPointLL;
begin
  Result := IsMERVisible(Globe, Obj.ObjectMER);

  if Result then
  begin
    if (Supports( Obj, IGMapPoly)) and (IGMapPoly(Obj).Chains.Count > 0) then
    begin
      // Check all the points in a TGMapPoly
      with Obj as IGMapPoly do
        for iChain := 0 to Chains.count - 1 do
          for idx := 0 to Chains[iChain].Count - 1 do
            if Globe.Projector.PointLLToXY(Chains[iChain][idx], ptXY) then
              if (ptXY.X >= aRect.Left) and (ptXY.X < aRect.Right) then
                if (ptXY.Y >= aRect.Top) and (ptXY.Y < aRect.Bottom) then
                  Exit;
    end
    else // Check the objects Centroid
      if Globe.Projector.PointLLToXY(Obj.Centroid, ptXY) then
        if (ptXY.X >= aRect.Left) and (ptXY.X < aRect.Right) then
          if (ptXY.Y >= aRect.Top) and (ptXY.Y < aRect.Bottom) then
            Exit;

    with Globe.Projector do
      DeviceXYToLL((aRect.Left + aRect.Right) div 2, (aRect.Top + aRect.Bottom) div 2, ptLL);

    if Obj.LLInObject(ptLL, 0) then
      Exit;
  end;
  Result := False;
end;

//------------------------------------------------------------------------------
//Description
//Compares Two points to see if they occupy the same screen pixel.
//
//Parameters
//Projector : Current Globe Projector.
//ptLL1 :     first point to compare.
//ptLL2 :     second point to compare.
//------------------------------------------------------------------------------
function SamePixelPos(Projector: TGProjector; const ptLL1, ptLL2 : TGPointLL): Boolean;
var
  ptPixel1, ptPixel2: TPoint;
begin
  Projector.LLToDeviceXY(ptLL1.iLongX, ptLL1.iLatY, ptPixel1);
  Projector.LLToDeviceXY(ptLL2.iLongX, ptLL2.iLatY, ptPixel2);
  Result:= (ptPixel1.x=ptPixel2.x) and (ptPixel1.y=ptPixel2.y);
end;

//----------------------------------------------------------------------------
//Description
//Returns a version string from the Application Version info
//
//Parameters
//ExeName :  Name of Executable to get version info from
//VerStr :   Name of version string to fetch
//----------------------------------------------------------------------------
function GetVersionString( const ExeName, VerStr : String ) : String;
var
  Buf: array[0..2048] of Char;
  str: String;
  len: DWORD;
  ptr: Pointer;
  pt: TSmallPoint;
begin
  Result := '';

  GetFileVersionInfo(PChar(ExeName), 0, Length(Buf), @Buf);

  str := '\\VarFileInfo\\Translation';
  if (VerQueryValue(@Buf, PChar(str), ptr, len)) then
  begin
    pt.x := TSmallPoint(ptr^).y;
    pt.y := TSmallPoint(ptr^).x;
    str := format('%.8x', [integer(pt)]);
    str := '\\StringFileInfo\\' + str + '\\' + VerStr;

    if VerQueryValue(@Buf, PChar(str), ptr, len) then
      Result := PChar( ptr );
  end;
end;

//----------------------------------------------------------------------------
//Description
//Returns the name of the computer.
//----------------------------------------------------------------------------
function GetMachineName : String;
var
  buf : array [0..MAX_COMPUTERNAME_LENGTH] of char;
  len : Cardinal;
begin
  len := SizeOf( buf );
  GetComputerName( @buf, len );
  Result := buf;
end;

//----------------------------------------------------------------------------
//Description
//Returns a calculated color on the color gradient defined by the supplied
// colors. The alpha value should be between 0 and 1.0.
//----------------------------------------------------------------------------
function ColorGradient( startColor, endColor: TColor; alpha : Double ): TColor;
var
  sR,sG,sB : integer;
  eR,eG,eB : integer;
begin
  sR := GetRValue( startColor );
  sG := GetGValue( startColor );
  sB := GetBValue( startColor );
  eR := GetRValue( endColor );
  eG := GetGValue( endColor );
  eB := GetBValue( endColor );
  Result := RGB(
    Round(sR + ( eR - sR ) * alpha),
    Round(sG + ( eG - sG ) * alpha),
    Round(sB + ( eB - sB ) * alpha));
end;

//----------------------------------------------------------------------------
//Description
//Returns a calculated blend of the Source color and the Target color
// colors. The alpha value should be between 0 and 1.0.
//----------------------------------------------------------------------------
function AlphaBlend( source, target : TColor; alpha : Double ) : TColor;
var
  blend, R, G, B : integer;
begin
  blend := Round( 255 * alpha );

  // Alpha blend the target color into the source colour
  R := (GetRValue( source ) * blend + GetRValue( target ) * (255 - blend)) shr 8;
  G := (GetGValue( source ) * blend + GetGValue( target ) * (255 - blend)) shr 8;
  B := (GetBValue( source ) * blend + GetBValue( target ) * (255 - blend)) shr 8;

  Result := RGB( R, G, B );
end;

//----------------------------------------------------------------------------
//Description
//Scans all the objects on a layer and calculates the min, max, mean and sum
//of the specified attribute value. The noDataValue is used to specify objects
//that should be ignored.
//----------------------------------------------------------------------------
procedure LayerStats( aLayer : TGLayer; column : integer; const noDataValue : Double;
  var minValue : Double; var maxValue : Double; var meanValue : Double; var sumValue : Double );
var
  obj : IGMapPoint;
  iDataCount : integer;
  eTmp : Double;
begin
	maxValue := MinDouble;
	minValue := MaxDouble;
	sumValue := 0;

	iDataCount := 0;
  obj := aLayer.ObjectSource.FirstObject;
  while obj <> nil do
  begin
    // scan the objects to find the range of values

    eTmp := obj.Value[column];

    // update the range values
    if not IsNan( eTmp ) and ( eTmp <> noDataValue ) then
    begin
      if eTmp < minValue then
        minValue := eTmp;
      if eTmp > maxValue then
        maxValue := eTmp;
      SumValue := SumValue + eTmp;
      Inc(iDataCount);
    end;
    obj := aLayer.ObjectSource.NextObject(obj);
  end;

	if iDataCount > 0 then
		meanValue := sumValue / iDataCount
  else
  begin
    minValue := 0;
    maxValue := 0;
    meanValue := 0;
  end;
end;

//----------------------------------------------------------------------------
//Description
//Returns a value between 0 and 1.0 in discrete steps. The result represents
//the segment that the value falls into between the minValue and maxValue.
//Returns -1.0 if the value is out of range or is equal to noDataValue.
//----------------------------------------------------------------------------
function ValueSegment( value, minValue, maxValue, noDataValue : Double; breaks : integer ): Double;
var
  range : Double;
begin
  Result := -1.0;

  if not IsNan( value ) and ( value >= minValue ) and ( value <= maxValue ) and (value <> noDataValue ) then
  begin
    range := maxValue - minValue;
    if range > 0.0 then
    begin
      Result := ( value - minvalue ) / range;
      Result := ( 1.0 / ( breaks - 1 )) * Int( Result * breaks );
    end;
  end;
end;


//-----------------------------------------------------------------------------
//Description
// Renders the shadow of the sun on the globe at the supplied date and time.
// Returns The location of the earth directly under the sun.
//-----------------------------------------------------------------------------
function RenderSunShadow(Globe : TGlobe5; GMTDateTime : TDateTime; ColorNight: TColor;
  TwilightDegrees : integer; MaxAlpha : Byte) : TGPointLL;
var
	TimeZone : TTimeZoneInformation;
	iBias : integer;
  Year, Tmp : word;
  iState, iRotation, iDays, iSecs : integer;
  iCol, iRow, iTmp, idx : integer;
  eAngle : Double;
  sunVec : TGVec3D;

  function DoPixel(iX, iY : integer) : integer;
  var
    ptLL : TGPointLL;
    eTmp : Double;
    alpha : Byte;
  begin
    Result := 0;  // not on the earth
    if iX < 0 then
      Exit;
    if Globe.Projector.DeviceXYToLL(iX, iY, ptLL) then
    begin
      Inc(Result);  // In Sun zone
      eTmp := Vec3D_Dot( V3D(ptLL), sunVec);

      if eTmp <= 0 then  // Twilight or Night zone
      begin
        Inc(Result, 1 + Ord(eTmp >= -TwilightDegrees * GU_DEGREE));
        if Result > 2 then
          alpha := Round( MaxAlpha * Abs(eTmp / ( TwilightDegrees * GU_DEGREE )))  // Twilight zone
        else
          alpha := MaxAlpha;  // Night zone

        Globe.Renderer.DrawPixel(iX, iY, ColorNight, alpha );
      end;
    end;
  end;
begin
	case GetTimeZoneInformation( TimeZone ) of
	1 :		iBias := TimeZone.Bias + TimeZone.StandardBias;
	2 :		iBias := TimeZone.Bias + TimeZone.DayLightBias;
	else
    iBias := 0;
	end;
	GMTDateTime := GMTDateTime + iBias / 1440;

  DecodeDate(GMTDateTime, Year, Tmp, Tmp);
  iDays := Trunc(GMTDateTime) - Trunc(EncodeDate(Year - 1, 12, 21));
  { 0.4333823 = Tan( 23.452294 ) = Tan( Axial tilt ) }
  eAngle := 180 * ArcTan(0.4333823 * Cos(DoublePi * iDays / 365.24225)) / LocalPi;

  iSecs := Round(Frac(GMTDateTime) * 86400);
  iRotation := (Round(iSecs * (21600 / 86400)) mod 21600) - 10800;

  Result := PointLL( -iRotation * GU_MINUTE, -Round(eAngle * GU_DEGREE));
  sunVec := Vec3D_Scale(V3D(Result), GU_EARTHRADIUS);

  for iRow := 0 to Globe.Height - 1 do
  begin
    iCol := 0;
    iState := DoPixel(iCol, iRow);

    while iCol - 16 < Globe.Width do
    begin
      Inc(iCol, 16);
      iTmp := DoPixel(iCol, iRow);

      if ( iTmp <> iState ) or ( iTmp = 3 ) then  // if Crossing a boundary or in Twilight zone
        for idx := 1 to 15 do
          DoPixel(iCol - idx, iRow)
      else
        if iState = 2 then  // render night
          for idx := 1 to 15 do
            Globe.Renderer.DrawPixel(iCol - idx, iRow, ColorNight, MaxAlpha);

      iState := iTmp;
    end;
  end;
end;
end.

