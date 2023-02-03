//-----------------------------------------------------------------------
// Summary
//	Built in projections
//
// Description
//   Provides the implementation of the builtin Projection Models
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
unit GProjections;

interface

Uses Windows, Classes, Graphics, SysUtils, GSysUtils, GClasses, GXML, GBitmap;

const
  MAX_MERCATOR_LATITUDE = GU_90_DEGREE - GU_DEGREE;

type

  //-------------------------------------------------------------------------------
  //Description
  //Base class that represents a Low level projection model class that provides the
  //final coordinate conversion between world and device coordinates.
  //
  //To add a new map projection to the system you would derive from this class or
  //\one of it's descendants.                                                      
  //-------------------------------------------------------------------------------
  TGProjectionModel = class(TGCustomProjectionModel)
  protected
    FTransform : TGMatrix;
    FInvTransform : TGMatrix;
  public
    procedure LoadFromXML( Element : TGXML_Element ); override;
    procedure SaveToXML( Element : TGXML_Element ); override;

    function IsContinuous : Boolean; override;

    function PointStoreToXY(Points : TGPointStore; iStart : integer) : Integer; override;

    function LLToXY(iLong, iLat, iIndex : Integer) : Boolean; override;
    function LLHToXY(iLong, iLat, iHeight, iIndex : Integer) : Boolean; override;

    function VisibleMER(const MER : TGMER; var State : TGRenderStateSet) : Boolean; override;
    function ExtentsLL : TRect; override;

    //-------------------------------------------------------------------------------
    //Description
    //Matrix that provides an inverse transformation from world coordinates to device
    //coordinates.
    //-------------------------------------------------------------------------------
    property Transform : TGMatrix read FTransform write FTransform;
    //-------------------------------------------------------------------------------
    //Description
    //Matrix that provides an inverse transformation from device coordinates to world
    //coordinates.
    //-------------------------------------------------------------------------------
    property InvTransform : TGMatrix read FInvTransform write FInvTransform;
  end;


  //---------------------------------------------
  //Description
  //Provides a spherical projection of the earth.
  //---------------------------------------------
  TGSphericalPrj = class(TGProjectionModel)
  private
    FGlobeRadius, FGlobeRadiusSquared : Extended;
  public
    procedure PropertyChanged( ProjectionProperty : TGProjectionProperty ); override;
    function PointStoreToXY(Points : TGPointStore; iStart : integer ) : Integer; override;

    procedure Assign( AModel : TPersistent ); override;
    procedure GetPOZ(var POZ : TGPOZ); override;
    procedure SetPOZ(const POZ : TGPOZ); override;

    function PointLLToXY(const ptLL: TGPointLL; iIndex : Integer) : Boolean; override;
    function XYToLL(iX, iY, iIndex : Integer) : Boolean; override;
    function ExtentsLL : TRect; override;
    function IsContinuous : Boolean; override;
    procedure PaintSurfaceImage( image : TGBitmap ); override;
    procedure PaintSurface; override;
  end;


  //------------------------------------------------------------------------------
  //Description
  //Provides a cartesian plane projection from world coordinates in globe units to
  //device coordinates in pixels.
  //
  //------------------------------------------------------------------------------
  TGCartesianPrj = class(TGProjectionModel)
  private
    LastXrotation : Extended;
    LastYrotation : Extended;
  public
    procedure PropertyChanged( ProjectionProperty : TGProjectionProperty ); override;
    procedure GetPOZ(var POZ : TGPOZ); override;
    procedure SetPOZ(const POZ : TGPOZ); override;

    function PointLLToXY(const ptLL: TGPointLL; iIndex : Integer) : Boolean; override;
    function XYToLL(iX, iY, iIndex : Integer) : Boolean; override;
    procedure PaintSurfaceImage( image : TGBitmap ); override;
    procedure PaintSurface; override;
  end;


  //---------------------------------------------------------------------------
  //Description
  //Provides a projection model to convert between World coordinates and screen
  //coordinates to produce a Mercator view of the globe.
  //---------------------------------------------------------------------------
  TGMercatorPrj = class(TGCartesianPrj)
  public
    function PointLLToXY(const ptLL: TGPointLL; iIndex : Integer) : Boolean; override;
    function XYToLL(iX, iY, iIndex : Integer) : Boolean; override;
  end;

implementation

Uses Globe5;

function TGProjectionModel.ExtentsLL: TRect;
begin
  Result := Rect( -GU_180_DEGREE + 1, -GU_90_DEGREE, GU_180_DEGREE, GU_90_DEGREE )
end;


function TGProjectionModel.IsContinuous: Boolean;
begin
  Result := false;
end;

function TGProjectionModel.LLHToXY(iLong, iLat, iHeight,
  iIndex: Integer): Boolean;
var
  ptLL : TGPointLL;
begin
  ptLL.iLongX := iLong;
  ptLL.iLatY := iLat;
  ptLL.iHeightZ := iHeight;
  Result := PointLLToXY( ptLL, iIndex );
end;

function TGProjectionModel.LLToXY(iLong, iLat, iIndex: Integer): Boolean;
var
  ptLL : TGPointLL;
begin
  ptLL.iLongX := iLong;
  ptLL.iLatY := iLat;
  ptLL.iHeightZ := 0;
  Result := PointLLToXY( ptLL, iIndex );
end;

procedure TGProjectionModel.LoadFromXML( Element : TGXML_Element );
begin
  // Does nothing
end;


function TGProjectionModel.PointStoreToXY( Points : TGPointStore; iStart : integer ) : Integer;
var
  idx : Integer;
begin
  Result := iStart;
  for idx := 0 to Points.Count - 1 do
    if PointLLToXY( Points.AsLL[idx], Result ) then
      Inc( Result );
end;


procedure TGProjectionModel.SaveToXML( Element : TGXML_Element );
begin
  Element.AddAttribute( 'Class', ClassName, '' );
end;


function TGProjectionModel.VisibleMER(const MER : TGMER; var State : TGRenderStateSet) : Boolean;
var
  idx, iCount, iFace : integer;

  function TestRect(iLeft, iTop, iRight, iBottom : Integer) : Boolean;
  begin
    Result := False;
    iFace := 0;

    Inc(iFace, Ord(LLToXY(iLeft, iTop, 0)));
    Inc(iFace, Ord(LLToXY(iLeft, iBottom, 1)));
    Inc(iFace, Ord(LLToXY(iRight, iBottom, 2)));
    Inc(iFace, Ord(LLToXY(iRight, iTop, 3)));
    // add in points around the edge of the rectangle if dealing with a large object
    iCount := 4;
    idx := iLeft + GU_DEGREE * 5;
    while idx < iRight do
    begin
      Inc(iFace, Ord(LLToXY(idx, iTop, iCount)));
      Inc(iCount);
      Inc(iFace, Ord(LLToXY(idx, iBottom, iCount)));
      Inc(iCount);
      idx := idx + GU_DEGREE * 5;
    end;

    idx := iTop + GU_DEGREE * 5;
    while idx < iBottom do
    begin
      Inc(iFace, Ord(LLToXY(iLeft, idx, iCount)));
      Inc(iCount);
      Inc(iFace, Ord(LLToXY(iRight, idx, iCount)));
      Inc(iCount);
      idx := idx + GU_DEGREE * 5;
    end;

    if iFace > 0 then
    begin
      Result := ParentGlobe.Renderer.XYListVisible( ParentGlobe.Renderer.Points, iCount);

      if Result then
      begin
{        if ParentGlobe.Projector.RenderBackFace then
        begin
          if iFace <> iCount then
            Include(State, rsFace);
          if iFace > 0 then
            Include(State, rsBackFace);
        end
        else}
        begin
          if iFace <> iCount then
            Include(State, rsBackFace);
          if iFace > 0 then
            Include(State, rsFace);
        end;
      end;
    end;
  end;
begin
  Exclude( State, rsFace );
  Exclude( State, rsBackFace );

  with MER do
  try
    if iWidthX >= GU_360_DEGREE then
    begin
      Result := True;
      Exit;
    end;

    if ( iWidthX > GU_180_DEGREE ) or ( iLongX + iWidthX > GU_180_DEGREE ) then
    begin
      Result := TestRect(iLongX, iLatY, GU_180_DEGREE, iLatY + iHeightY) or
        TestRect(-GU_180_DEGREE, iLatY, -GU_180_DEGREE + iWidthX - (GU_180_DEGREE - iLongX), iLatY + iHeightY);
    end
    else
      Result := TestRect(iLongX, iLatY, iLongX + iWidthX, iLatY + iHeightY);
  except
    Result := true;
  end;
end;


procedure TGSphericalPrj.Assign( AModel : TPersistent );
begin
  inherited Assign( AModel );

  if AModel is TGSphericalPrj then
  begin
    Transform := TGSphericalPrj( AModel ).Transform;
    InvTransform := TGSphericalPrj( AModel ).InvTransform;
    FGlobeRadius := TGSphericalPrj( AModel ).FGlobeRadius;
    FGlobeRadiusSquared := TGSphericalPrj( AModel ).FGlobeRadiusSquared;
  end;
end;


function TGSphericalPrj.ExtentsLL: TRect;
begin
  Result := Rect( -Round( GU_EARTHRADIUS ), -Round( GU_EARTHRADIUS ),
    Round( GU_EARTHRADIUS ), Round( GU_EARTHRADIUS ) )
end;

procedure TGSphericalPrj.GetPOZ(var POZ : TGPOZ);
begin
  with ParentGlobe.Projector do
  begin
    POZ.eSf := ScaleFactor;
    POZ.CenterXY := CenterXY;
    POZ.iRotX := Round(XRotation * GU_DEGREE);
    POZ.iRotY := Round(YRotation * GU_DEGREE);
    POZ.iRotZ := Round(ZRotation * GU_DEGREE);
    POZ.iWidth := ParentGlobe.ClientWidth;
    POZ.iHeight := ParentGlobe.ClientHeight;
  end;
end;

function TGSphericalPrj.IsContinuous: Boolean;
begin
  Result := true;
end;


procedure TGSphericalPrj.PaintSurface;
const
  SEGMENTS = 180;
var
  eLastX, eLastY, eX, eY : Extended;
  eCosAngle, eSinAngle : Extended;
  idx : integer;
begin
  eY := 0;
  eX := FGlobeRadius - 1.0;

  with ParentGlobe do
  begin
    Renderer.Points[0] := Point( Projector.XOrigin + Round( eX ), Projector.YOrigin + Round( eY ));

    SinCos(( GU_360_DEGREE / SEGMENTS ) * GU_TORADIANS, eSinAngle, eCosAngle );

    for idx := 1 to SEGMENTS - 1 do
    begin
      eLastX := eX;
      eLastY := eY;
      eX := eLastX * eCosAngle - eLastY * eSinAngle;
      eY := eLastX * eSinAngle + eLastY * eCosAngle;
      Renderer.Points[idx] := Point( Projector.XOrigin + Round( eX ), Projector.YOrigin + Round( eY ));
    end;

    Renderer.BeginDrawPoly( True );
    Renderer.AddClippedPoly( SEGMENTS, 0 );
    Renderer.EndDrawPoly;
  end;
end;

procedure TGSphericalPrj.PaintSurfaceImage( image : TGBitmap );
const
  octohedron : array[0..23] of TGVec3D = (
    ( x: 1.0; y: 0.0; z: 0.0 ),( x: 0.0; y: 0.0; z: 1.0 ),( x: 0.0; y: 1.0; z: 0.0 ),
    ( x: 1.0; y: 0.0; z: 0.0 ),( x: 0.0; y: 1.0; z: 0.0 ),( x: 0.0; y: 0.0; z:-1.0 ),
    ( x: 0.0; y:-1.0; z: 0.0 ),( x: 0.0; y: 0.0; z: 1.0 ),( x: 1.0; y: 0.0; z: 0.0 ),
    ( x: 0.0; y:-1.0; z: 0.0 ),( x: 1.0; y: 0.0; z: 0.0 ),( x: 0.0; y: 0.0; z:-1.0 ),
    ( x: 0.0; y: 1.0; z: 0.0 ),( x:-1.0; y: 0.0; z: 0.0 ),( x: 0.0; y: 0.0; z:-1.0 ),
    ( x: 0.0; y: 1.0; z: 0.0 ),( x: 0.0; y: 0.0; z: 1.0 ),( x:-1.0; y: 0.0; z: 0.0 ),
    ( x:-1.0; y: 0.0; z: 0.0 ),( x: 0.0; y:-1.0; z: 0.0 ),( x: 0.0; y: 0.0; z:-1.0 ),
    ( x:-1.0; y: 0.0; z: 0.0 ),( x: 0.0; y: 0.0; z: 1.0 ),( x: 0.0; y:-1.0; z: 0.0 )
  );
var
  idx, iLevel : Integer;
  WorldTri : TGTriangleLL;
  SrcTri : TGTriangle;
  bWesternHemisphere : Boolean;

 {---------------------------------------------------------------------------}
  procedure DivideTriangle( const A, B, C : TGVec3D; const iLevel : integer );
  var
    v1, v2, v3 : TGVec3D;

    //------------------------------------------------------
    function ToWorld( const v : TGVec3D ) : TGPointLL;
    var
      Long : Extended;
    begin
      {Convert to Polar coords taking hemisphere into account }
      Long := ArcTan2( v.x, v.z ) * GU_FROMRADIANS - 1.0;

      if bWesternHemisphere then  // to fix triangles that lie on the 180 meridian
        Long := -abs( Long );

      Result := PointLL( Round( Long ), Round( ArcSin( v.y ) * GU_FROMRADIANS));
    end;

    //------------------------------------------------------
    function ToTexture( const ptLL : TGPointLL ) : TPoint;
    begin
      with image do
        Result := Point(
          Round((( ptLL.iLongX + GU_180_DEGREE) / GU_360_DEGREE ) * (Width - 1)),
          Height - Round((( ptLL.iLatY + GU_90_DEGREE ) / GU_180_DEGREE ) * (Height - 1)));
    end;
  begin
    if iLevel = 0 then
    begin
      WorldTri[0] := ToWorld( A );
      WorldTri[1] := ToWorld( B );
      WorldTri[2] := ToWorld( C );

      SrcTri[0] := ToTexture( WorldTri[0] );
      SrcTri[1] := ToTexture( WorldTri[1] );
      SrcTri[2] := ToTexture( WorldTri[2] );

      TGlobe5(ParentGlobe).RenderTextureTriangle( SrcTri, WorldTri, image, false );
    end
    else
      if PointLLToXY( ToWorld( A ), 0 ) or PointLLToXY( ToWorld( B ), 1 ) or PointLLToXY( ToWorld( C ), 2 ) then
      begin
        v1 := Vec3D_Plus( A, B );
        v2 := Vec3D_Plus( A, C );
        v3 := Vec3D_Plus( B, C );

        DivideTriangle( A, v2, v1, iLevel - 1 );
        DivideTriangle( B, v1, v3, iLevel - 1 );
        DivideTriangle( C, v3, v2, iLevel - 1 );
        DivideTriangle( v1, v2, v3, iLevel - 1 );
      end;
  end;
begin
  with ParentGlobe.Projector do
  begin
    // Adjust sub division level to suit scalefactor - performance improvement
    iLevel := 4;
    if ScaleFactor > 0.0000002 then
      Inc( iLevel );
    if ScaleFactor > 0.000002 then
      Inc( iLevel );
    if ScaleFactor > 0.00002 then
      Inc( iLevel );

    for idx := 0 to 7 do
    begin
      bWesternHemisphere := idx >= 4;
      DivideTriangle( octohedron[idx * 3], octohedron[idx * 3 + 1], octohedron[idx * 3 + 2], iLevel );
    end;
  end;
end;

function TGSphericalPrj.PointLLToXY( const ptLL: TGPointLL; iIndex : Integer ) : Boolean;
var
  eTmpX, eTmpY : Extended;
  bFlag : Boolean;
begin
  with Point3D(ptLL) do
  begin
    { convert to XY }
    eTmpX := Transform.terms[0] * X + Transform.terms[1] * Y + Transform.terms[2] * Z;
    eTmpY := Transform.terms[4] * X + Transform.terms[5] * Y + Transform.terms[6] * Z;
    bFlag := ( Transform.terms[8] * X + Transform.terms[9] * Y + Transform.terms[10] * Z ) < 0.0;

    if ( not bFlag ) and ( PtLL.iHeightZ > 0 ) then
      bFlag := Hypot( eTmpX, eTmpY ) > FGlobeRadius;

//    Result := bFlag <> ParentGlobe.Projector.RenderBackFace;
    Result := bFlag;

    with ParentGlobe do
      Renderer.Points[iIndex] := Point( Projector.XOrigin + Round( eTmpX ), Projector.YOrigin + Round( eTmpY ));
  end;
end;


function TGSphericalPrj.PointStoreToXY( Points : TGPointStore; iStart : integer ) : Integer;
var
  idx, iXorg, iYorg : Integer;
  iLastX, iLastY, iX, iY : Integer;
  eTmpX, eTmpY, eTmp : Double;
  array3D : TGPoint3DArray;
begin
  Result := 0;

  iXorg := ParentGlobe.Projector.XOrigin;
  iYorg := ParentGlobe.Projector.YOrigin;
  iLastX := GU_180_DEGREE;
  iLastY := GU_180_DEGREE;

  array3D := Points.Point3DArray;

  for idx := 0 to High( array3D ) do
  begin
    // convert to XY
    with array3D[idx], Transform do
    begin
      eTmpX := terms[0] * X + terms[1] * Y + terms[2] * Z;
      eTmpY := terms[4] * X + terms[5] * Y + terms[6] * Z;

      if terms[8] * X + terms[9] * Y + terms[10] * Z > 0.0 then
      begin // adjust point to lie on the horizon
        eTmp := FGlobeRadius / Hypot( eTmpX, eTmpY );
        eTmpX := eTmpX * eTmp;
        eTmpY := eTmpY * eTmp;
      end;
    end;

    iX := Round( eTmpX );
    iY := Round( eTmpY );

    if (( iLastX - iX ) <> 0 ) or (( iLastY - iY ) <> 0 ) then
    begin
      iLastX := iX;
      iLastY := iY;
      ParentGlobe.Renderer.Points[Result] := Point( iXorg + iX, iYorg + iY );
      Inc( Result );
    end;
  end;
end;


procedure TGSphericalPrj.PropertyChanged( ProjectionProperty : TGProjectionProperty );
var
  quat : TGQuaternion;
begin
  with ParentGlobe.Projector do
  begin
    case ProjectionProperty of
      ppXRotation :
        XRotation := MinFloat( MaxFloat( XRotation, -90 ), 90 );
      ppYRotation :
        YRotation := Mod180Float( YRotation );
      ppZRotation :
        ZRotation := Mod180Float( ZRotation );
      ppScaleFactor :
      begin
        FGlobeRadius := GU_EARTHRADIUS * ScaleFactor;
        FGlobeRadiusSquared := FGlobeRadius * FGlobeRadius;
      end;
      else
        Exit;
    end;

//    CentralMeridian := 0.0;

    Quat_FromEuler(quat, XRotation, YRotation, ZRotation);
    Quat_ToMatrix(quat, FTransform);

    TransposeMatrix( FTransform, FInvTransform );

    ScaleMatrix( FTransform, ScaleFactor );
    ScaleMatrix( FInvTransform, 1.0 / ScaleFactor );
  end;
end;

procedure TGSphericalPrj.SetPOZ(const POZ : TGPOZ);
var
  ratio : Extended;
begin
  with ParentGlobe.Projector do
  try
    // Calc the ratio of the window sizes
    ratio := Hypot( ParentGlobe.ClientWidth, ParentGlobe.ClientHeight ) / Hypot( POZ.iWidth, POZ.iHeight );

    ScaleFactor := ratio * POZ.eSF;

    XRotation := POZ.iRotX / GU_DEGREE;
    YRotation := POZ.iRotY / GU_DEGREE;
    ZRotation := POZ.iRotZ / GU_DEGREE;

    CenterXY := Point( Round(ratio * POZ.CenterXY.X ), Round(ratio * POZ.CenterXY.Y ));
  except
    // Ignore errors
  end;
end;


function TGSphericalPrj.XYToLL( iX, iY, iIndex : Integer ) : Boolean;
var
  eTmpX, eTmpY, eTmpZ : Extended;
  eX, eY, eZ : Extended;
begin
  { XY to World }
  eTmpX := iX - ParentGlobe.Projector.XOrigin;
  eTmpY := iY - ParentGlobe.Projector.YOrigin;
  eTmpZ := FGlobeRadiusSquared - eTmpX * eTmpX - eTmpY * eTmpY;

  { check to see if the coords are outside the globe }
  Result := eTmpZ >= 0;

  if Result then
  begin
    { World To LL }
    eTmpZ := -Sqrt( eTmpZ );

    eX := ( InvTransform.terms[0] * eTmpX ) + ( InvTransform.terms[1] * eTmpY ) + ( InvTransform.terms[2] * eTmpZ );
    eY := ( InvTransform.terms[4] * eTmpX ) + ( InvTransform.terms[5] * eTmpY ) + ( InvTransform.terms[6] * eTmpZ );
    eZ := ( InvTransform.terms[8] * eTmpX ) + ( InvTransform.terms[9] * eTmpY ) + ( InvTransform.terms[10] * eTmpZ );

    ParentGlobe.Renderer.Points[iIndex] := Point( Round( ArcTan2( eX, eZ ) * GU_FROMRADIANS ),
      Round( ArcSin( eY * ( 1.0 / GU_EARTHRADIUS )) * GU_FROMRADIANS ));
  end;
end;

procedure TGCartesianPrj.GetPOZ(var POZ : TGPOZ);
begin
  with ParentGlobe.Projector do
  begin
    POZ.eSf := ScaleFactor;
    POZ.iRotY := Round( CenterLongitude * GU_DEGREE );
    POZ.iRotX := Round( CenterLatitude * GU_DEGREE );
    POZ.iRotZ := 0;
    POZ.CenterXY := CenterXY;
    POZ.iWidth := ParentGlobe.ClientWidth;
    POZ.iHeight := ParentGlobe.ClientHeight;
  end;
end;

//---------------------------------------------------------
//Description
//Called when a projection model property has been changed.
//---------------------------------------------------------
procedure TGCartesianPrj.PropertyChanged( ProjectionProperty : TGProjectionProperty );
begin
  with ParentGlobe.Projector do
    case ProjectionProperty of
      ppXRotation :
        begin
          case ParentGlobe.Projector.XAxisChanges of
          xaCentralParallel :
            if ( CentralParallel > -GU_90_DEGREE ) and ( CentralParallel < GU_90_DEGREE ) then
              CentralParallel := Mod180Float(( XRotation - LastXRotation ) + CentralParallel );
          xaYOrigin :
            begin
              LLToXY( Round( YRotation * GU_DEGREE ), -Round(( XRotation - LastXRotation ) * GU_DEGREE ), 0 );
              YOrigin := ParentGlobe.Renderer.Points[0].Y;
            end;
          end;
          LastXRotation := XRotation;
        end;
      ppYRotation :
        begin
          case ParentGlobe.Projector.YAxisChanges of
          yaCentralMeridian :
            CentralMeridian := Mod180Float(( YRotation - LastYRotation ) + CentralMeridian );
          yaXOrigin :
            begin
              LLToXY( -Round(( YRotation - LastYRotation ) * GU_DEGREE), -Round( XRotation * GU_DEGREE ), 0 );
              XOrigin := ParentGlobe.Renderer.Points[0].X;
            end;
          end;
          LastYRotation := YRotation;
        end;
    end;
end;

//-----------------------------------------------------------------------------
//Description
//Converts a point in globe units to XY coordinates. Returns false if the point
//lies on the back side of the globe.
//
//Parameters
//ptLL :    The point to convert
//iIndex :  the index in the Renderer.Points array to store the resulting XY
//          coordinate.
//-----------------------------------------------------------------------------
function TGCartesianPrj.PointLLToXY( const ptLL: TGPointLL; iIndex : Integer ) : Boolean;
begin
 { check to see if the coords are outside the globe }
  with ParentGlobe.Projector, ptLL do
  begin
    with ExtentsLL do
      Result := ( iLongX >= Left ) and ( iLongX <= Right ) and ( iLatY >= Top ) and ( iLatY <= Bottom );

    ParentGlobe.Renderer.Points[iIndex] := Point(
      XOrigin + Round( Mod180( iLongX - Round( CentralMeridian * GU_DEGREE )) * ScaleFactor ),
      YOrigin - Round( iLatY * ScaleFactor ) );
  end;
end;

procedure TGCartesianPrj.PaintSurface;
var
  idx : integer;
  CM : Extended;
begin
  with ParentGlobe.Projector do
  begin
    CM := CentralMeridian;
    CentralMeridian := 0;
    with ExtentsLL do
    begin
      for idx := 0 to 35 do
        LLToXY( Left + idx * GU_10_DEGREE, Top, idx );
      for idx := 0 to 17 do
        LLToXY( Right - 1, Top + idx * GU_10_DEGREE, 36 + idx );
      for idx := 0 to 35 do
        LLToXY( Right - 1 - idx * GU_10_DEGREE, Bottom, 54 + idx );
      for idx := 0 to 17 do
        LLToXY( Left, Bottom - idx * GU_10_DEGREE, 90 + idx );
    end;
    CentralMeridian := CM;

    ParentGlobe.Renderer.BeginDrawPoly( True );
    ParentGlobe.Renderer.AddClippedPoly( 108, 0 );
    ParentGlobe.Renderer.EndDrawPoly;
  end;
end;

procedure TGCartesianPrj.PaintSurfaceImage( image : TGBitmap );
var
  idx, jdx : integer;
  stepX, stepY : Extended;
  x, y : Extended;
  long, lat : integer;
  srcTri : TGTriangle;
  destTri : TGTriangleLL;
begin
  stepX := image.Width / 36;
  stepY := image.Height / 18;

  for idx := 0 to 35 do
    for jdx := 0 to 17 do
    begin
      x := idx * stepX;
      y := jdx * stepY;

      srcTri[0] := Point( Round(x), Round(image.Height - y));
      srcTri[1] := Point( Round(x + stepX), Round(image.Height - y));
      srcTri[2] := Point( Round(x), Round(image.Height - ( y + stepY )));

      long := -GU_180_DEGREE + idx * GU_10_DEGREE;
      lat := -GU_90_DEGREE + jdx * GU_10_DEGREE;

      destTri[0] := PointLL( long + 1, lat );
      destTri[1] := PointLL( long + GU_10_DEGREE - 1, lat);
      destTri[2] := PointLL( long + 1, lat + GU_10_DEGREE );
      TGlobe5(ParentGlobe).RenderTextureTriangle(srcTri, destTri, image, false );

      srcTri[0] := Point( Round(x + stepX), Round(image.Height - (y + stepY)));
      destTri[0] := PointLL( long + GU_10_DEGREE - 1, lat + GU_10_DEGREE);
      TGlobe5(ParentGlobe).RenderTextureTriangle(srcTri, destTri, image, false );
    end;
end;

procedure TGCartesianPrj.SetPOZ(const POZ : TGPOZ);
var
  ratio : Extended;
begin
  with ParentGlobe.Projector do
  try
    // Calc the ratio of the window sizes
    ratio := Hypot( ParentGlobe.ClientWidth, ParentGlobe.ClientHeight ) / Hypot( POZ.iWidth, POZ.iHeight );

    ScaleFactor := ratio * POZ.eSF;

    CenterLongitude := Mod180(POZ.iRotY) / GU_DEGREE;
    CenterLatitude := POZ.iRotX / GU_DEGREE;
    ZRotation := 0;
  except
    // Ignore errors
  end;
end;

//-------------------------------------------------------------------------
//Description
//Converts an XY coordinate to a point in globeunits on the earths surface.
//\Returns false is the XY coordinate is not on the earths surface.
//-------------------------------------------------------------------------
function TGCartesianPrj.XYToLL( iX, iY, iIndex : Integer ) : Boolean;
var
  long, lat, eRecipSF : Extended;
begin
  { XY to World }
  with ParentGlobe.Projector do
  try
    eRecipSF := 1.0 / ScaleFactor;
    long := ( iX - XOrigin ) * eRecipSF;
    lat := -(( iY - YOrigin ) * eRecipSF );

    { check to see if the coords are outside the globe }
    with ExtentsLL do
      Result := ( long >= Left ) and ( long <= Right ) and ( lat >= Top ) and ( lat <= Bottom );

    if Result then
      ParentGlobe.Renderer.Points[iIndex] := Point( Mod180( Round( long  + CentralMeridian * GU_DEGREE )), Round( lat ));
  except
    Result := False;
  end;
end;

function TGMercatorPrj.XYToLL( iX, iY, iIndex : Integer ) : Boolean;
var
  long, lat, eY, eRecipSF : Extended;
begin
  { XY to World }
  with ParentGlobe.Projector do
  begin
    eRecipSF := 1 / ScaleFactor;
    long := ( iX - XOrigin ) * eRecipSF;

    eY := ( iY - YOrigin ) * eRecipSF;
    lat := ( 2.0 * arctan( Exp( -eY * GU_TORADIANS )) - HalfPI ) * GU_FROMRADIANS;

    { check to see if the coords are outside the globe }
    with ExtentsLL do
      Result := ( long >= Left ) and ( long <= Right ) and ( lat >= Top ) and ( lat <= Bottom );

    if Result then
      ParentGlobe.Renderer.Points[iIndex] := Point( Mod180( Round( long + CentralMeridian * GU_DEGREE )), Round( lat ));
  end;
end;


function TGMercatorPrj.PointLLToXY( const ptLL: TGPointLL; iIndex : Integer ) : Boolean;
var
  eY, eSLat, eCLat : Extended;
  iLatY : integer;
begin
  iLatY := ptLL.iLatY;
  with ParentGlobe.Projector do
  begin
    with ExtentsLL, ptLL do
      Result := ( iLongX >= Left ) and ( iLongX <= Right ) and ( iLatY >= Top ) and ( iLatY <= Bottom );

    // Clip the data to +-89 degrees
    if iLatY >= MAX_MERCATOR_LATITUDE then
      iLatY := MAX_MERCATOR_LATITUDE;
    if iLatY <= -MAX_MERCATOR_LATITUDE then
      iLatY := -MAX_MERCATOR_LATITUDE;

    SinCos(( iLatY * GU_TORADIANS ) * 0.5 + QuarterPI, eSLat, eCLat );

    eY := Ln( eSLat / eCLat ) * GU_FROMRADIANS;

    ParentGlobe.Renderer.Points[iIndex] := Point(
      XOrigin + Round( Mod180( ptLL.iLongX - Round( CentralMeridian * GU_DEGREE)) * ScaleFactor ),
      YOrigin - Round( eY * ScaleFactor ));
  end;
end;

initialization
  RegisterGlobeClass( TGCartesianPrj, 'Cartesian Projection' );
  RegisterGlobeClass( TGMercatorPrj, 'Mercator Projection' );
  RegisterGlobeClass( TGSphericalPrj, 'Spherical Projection' );
end.


