//-----------------------------------------------------------------------
//Summary
//Presenter to render smoothed contour lines
//
//Description
//Renders a poly as a smoothed line.
//
//Author
//Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------

{$I GLOBE5.INC}
unit GSmoothPolyPresenter;

interface

Uses Windows, Classes, Forms, Graphics, SysUtils, GPResenters,
  Globe5, GClasses, GMapObjects, GSysUtils, GXML, GResource;

const
  DEFAULT_STEPS = 6;

type
  TGSmoothPolyPresenter = class(TGPolyPresenter)
  private
    FSmoothingSteps: integer;
  public
    constructor Create( parentGlobe: TGCustomGlobe5 ); override;

    procedure Assign(Source : TPersistent); override;
    function Equals( aPresenter : TGPresenter ) : Boolean; override;

//    function LLInObject(ptLL : TGPointLL; mapObj : IGMapPoint; iTolerance : integer) : Boolean; override;
//    procedure RenderLegend( Canvas : TCanvas; rect : TRect ); override;


    procedure RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet); override;

    procedure LoadFromXML( Element : TGXML_Element ); override;
    procedure SaveToXML( Element : TGXML_Element ); override;
  published
    property SmoothingSteps : integer read FSmoothingSteps write FSmoothingSteps;
  end;

implementation


procedure GCatmullRom( Controls, points: TGPointStore; iSteps : integer; closed : Boolean );
var
  idx, iCtrl: integer;
  a, b, c, d: Extended;

  procedure Coefficients(u: Extended);
  begin
    a := -0.5 * u * u * u + u * u - 0.5 * u;
    b := 1.5 * u * u * u - 2.5 * u * u + 1;
    c := -1.5 * u * u * u + 2.0 * u * u + 0.5 * u;
    d := 0.5 * u * u * u - 0.5 * u * u;
  end;

  function Generate(iStart: integer): TGPointLL;
  var
    iPrior, iNext, iLast: integer;
  begin
    if closed then
    begin
      iPrior := ( iStart + Controls.Count - 1 )  mod Controls.Count;
      iNext := ( iStart + 1 ) mod Controls.Count;
      iLast := ( iStart + 2 ) mod Controls.Count;
    end
    else
    begin
      iPrior := MaxVal( 0, iStart - 1 );
      iNext := iStart + 1;
      iLast := MinVal( Controls.Count - 1, iStart + 2 );
    end;

    Result.iLongX := Round( Controls[iPrior].iLongX * a + Controls[iStart].iLongX * b +
      Controls[iNext].iLongX * c + Controls[iLast].iLongX * d );
    Result.iLatY := Round( Controls[iPrior].iLatY * a + Controls[iStart].iLatY * b +
      Controls[iNext].iLatY * c + Controls[iLast].iLatY * d );
  end;
begin
  if Controls.Count < 4 then // degenerate case
    Exit;

  if closed then
  begin
    points.Count := iSteps * Controls.Count;

    for idx := 0 to iSteps - 1 do
    begin
      Coefficients(idx / iSteps);
      for iCtrl := 0 to Controls.Count - 1 do
        points[iCtrl * iSteps + idx] := Generate(iCtrl);
    end;
  end
  else
  begin
    points.Count := iSteps * (Controls.Count - 1) + 1;

    for idx := 0 to iSteps - 1 do
    begin
      Coefficients(idx / iSteps);
      for iCtrl := 0 to Controls.Count - 2 do
        points[iCtrl * iSteps + idx] := Generate(iCtrl);
    end;
    // Add in the last control point
    points[points.Count - 1] := Controls[Controls.Count - 1];
  end;
end;



procedure TGSmoothPolyPresenter.Assign(Source: TPersistent);
begin
  inherited;

  if Source.InheritsFrom( TGSmoothPolyPresenter ) then
    SmoothingSteps := TGSmoothPolyPresenter( Source ).SmoothingSteps;
end;

constructor TGSmoothPolyPresenter.Create(parentGlobe: TGCustomGlobe5);
begin
  inherited;

  Name := 'Smooth PolyPresenter';

  FSmoothingSteps := DEFAULT_STEPS;
end;

function TGSmoothPolyPresenter.Equals(aPresenter: TGPresenter): Boolean;
begin
  Result := inherited Equals(aPresenter);

  if Result then
    Result := SmoothingSteps = TGSmoothPolyPresenter(aPresenter).SmoothingSteps
end;

procedure TGSmoothPolyPresenter.LoadFromXML(Element: TGXML_Element);
begin
  inherited;

  if Element <> nil then
    SmoothingSteps := Element.Attribute('Steps', DEFAULT_STEPS );
end;

procedure TGSmoothPolyPresenter.RenderObject( mapObj: IGMapPoint; State: TGRenderStateSet);
var
  idx : integer;
  ctrls : TGPointStore;
  tmp : TGChainStore;
begin
  if Supports( mapObj, IGMapPoly ) then
  begin
    if IsClosed( mapObj ) then
      Include( State, rsClosed )
    else
      Exclude( State, rsClosed );

    Renderer.AlphaBlend := AdjustedAlphaBlend;

    if mapObj.Selected then
      Renderer.Pen := Renderer.SelectedPen
    else
      Renderer.Pen := PolyPen;

    if rsClosed in State then
    begin
      if mapObj.Selected then
        Renderer.Brush := Renderer.SelectedBrush
      else
        Renderer.Brush := PolyBrush;
    end;

    with mapObj as IGMapPoly do
    begin
      tmp := TGChainStore.Create;

      if MER_Crosses180(ObjectMER) then
      begin
        for idx := 0 to Chains.Count -1 do
        begin
          ctrls := Chains[idx].Clone;
          ctrls.Translate( GU_180_DEGREE, 0 ,0 );
          GCatmullRom( ctrls, tmp[idx], SmoothingSteps, IsClosed( mapObj ));
          ctrls.Free;
          tmp[idx].Translate( GU_180_DEGREE, 0, 0 );
        end;
      end
      else
        for idx := 0 to Chains.Count -1 do
          GCatmullRom( Chains[idx], tmp[idx], SmoothingSteps, IsClosed( mapObj ));

      TGlobe5(ParentGlobe).RenderChainStore( tmp, State );

      tmp.Free;
    end;
  end;
end;

procedure TGSmoothPolyPresenter.SaveToXML(Element: TGXML_Element);
begin
  inherited;

  Element.AddAttribute('Steps', SmoothingSteps, DEFAULT_STEPS );
end;

initialization
  RegisterGlobeClass( TGSmoothPolyPresenter, 'Smooth Presenter' );
end.
