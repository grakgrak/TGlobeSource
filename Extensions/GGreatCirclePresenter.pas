unit GGreatCirclePresenter;

interface

Uses Windows, Classes, Graphics, SysUtils,
  GClasses, GMapObjects, GSysUtils, GXML, GResource, GPresenters, Globe5Utils;

type
  TGGreatCirclePresenter = class(TGPolyPresenter)
  private
    procedure GenerateGCLine(GCPoints : TGPointStore; const ptFromLL, ptToLL: TGPointLL; stepAngle : integer);
  public
    procedure RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet); override;

    function LLInObject(ptLL : TGPointLL; mapObj : IGMapPoint; iTolerance : integer) : Boolean; override;
  end;
implementation

uses Globe5;

{ TGGreatCirclePresenter }

procedure TGGreatCirclePresenter.GenerateGCLine( GCPoints : TGPointStore;
  const ptFromLL, ptToLL : TGPointLL; stepAngle : integer);
var
  a, b : TGVec3D;
  angle : Extended;
  iSteps, idx : integer;
  mat : TGMatrix;
  quat : TGQuaternion;
begin
  a := V3D( ptFromLL );
  b := V3D( ptToLL );
  angle := ArcCos( Vec3D_Dot( a, b ));

  iSteps := Trunc( angle / (stepAngle * GU_TORADIANS));
  if iSteps > 0 then
  begin
    Quat_FromAxisAngle( quat, Vec3D_Cross( a, b ), -angle / iSteps);
    Quat_ToMatrix( quat, mat );

    for idx := 0 to iSteps do
    begin
      GCPoints.Add( Vec3D_ToPointLL( a ));
      a := Vec3D_MatrixMultiply( a, mat );
    end;
  end
  else
  begin
    GCPoints.Add( ptFromLL );
    GCPoints.Add( ptToLL );
  end;
end;

function TGGreatCirclePresenter.LLInObject(ptLL: TGPointLL;
  mapObj: IGMapPoint; iTolerance: integer): Boolean;
var
  GCPoints : TGPointStore;
  iChain, idx : integer;
begin
  Result := True;

  if Supports( mapObj, IGMapPoly ) then
  begin
    GCPoints := TGPointStore.Create;
    try
      with mapObj as IGMapPoly do
        for iChain := 0 to Chains.Count - 1 do
          for idx := 1 to Chains[iChain].Count - 1 do
          begin
            GenerateGCLine( GCPoints, Chains[iChain].AsLL[idx - 1], Chains[iChain].AsLL[idx], GU_MINUTE * 5 );

            if GCPoints.PointOnEdge( ptLL.iLongX, ptLL.iLatY, iTolerance, false ) >= 0 then
              Exit;
            GCPoints.Count := 0;
          end;
    finally
      GCPoints.Free;
    end;
  end;
  Result := False;
end;

procedure TGGreatCirclePresenter.RenderObject(mapObj: IGMapPoint;
  State: TGRenderStateSet);
var
  iChain : integer;
  idx : integer;
  GCChains : TGChainStore;
begin
  if Supports( mapObj, IGMapPoly ) then
  begin
    Exclude( State, rsClosed );

    GCChains := TGChainStore.Create;
    try
      if rsSelected in State then
        Renderer.Pen := Renderer.SelectedPen
      else
      begin
        Renderer.AlphaBlend := AdjustedAlphaBlend;
        Renderer.Pen := PolyPen;
      end;

      with mapObj as IGMapPoly do
        for iChain := 0 to Chains.Count - 1 do
          for idx := 1 to Chains[iChain].Count - 1 do
            GenerateGCLine( GCChains[iChain], Chains[iChain].AsLL[idx - 1], Chains[iChain].AsLL[idx], GU_MINUTE * 20 );

      // Actually render the lines.
      TGlobe5(ParentGlobe).RenderChainStore( GCChains, State );
    finally
      GCChains.Free;
    end;
  end;
end;

initialization
  RegisterGlobeClass( TGGreatCirclePresenter, 'GreatCircle Presenter' );
end.
