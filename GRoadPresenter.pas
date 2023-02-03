//----------------------------------------------------------------------------
//Summary
//Presenter to display polylines as Roads
//
//Description
//Renders polylines as a road line bounded with edges. Uses 2 passes to render
//the network so that the roads intersect rather than overlap.
//
//Author
//Graham Knight (tglobe@tglobe.com)
//----------------------------------------------------------------------------
{$I GLOBE5.INC}
unit GRoadPresenter;

interface

Uses Windows, Classes, Forms, Graphics, SysUtils, 
  Globe5, GClasses, GMapObjects, GSysUtils, GXML, GResource;

type
  TGRoadPresenter = class(TGPresenter)
  private
    FEdgePen : TGPen;
    FRoadPen : TGPen;
    FEdgePixels: integer;
    FEdgeColor: TColor;
    FRoadPenWidth : integer;
    FRenderPass : integer;

  protected
    procedure BeginLayerRender( aLayer : TGLayer ); override;
    procedure EndLayerRender( aLayer : TGLayer ); override;
  public
    constructor Create( parentGlobe: TGCustomGlobe5 ); override;
    destructor Destroy; override;

    class function FindMatch( Presenters : TGPresenterStore;
      edgeColor : TColor; edgePixels : integer; roadPen : TGPen; ATitleFont : TGFont) : integer;

    procedure Assign(Source : TPersistent); override;
    function LLInObject(ptLL : TGPointLL; mapObj : IGMapPoint; iTolerance : integer) : Boolean; override;

    procedure RenderLegend( Canvas : TCanvas; rect : TRect ); override;
    procedure RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet); override;

    procedure LoadFromXML( Element : TGXML_Element ); override;
    procedure SaveToXML( Element : TGXML_Element ); override;
  published
    property EdgeColor : TColor read FEdgeColor write FEdgeColor;
    property EdgePixels : integer read FEdgePixels write FEdgePixels;
    property RoadPen : TGPen read FRoadPen write FRoadPen;
  end;


implementation




procedure TGRoadPresenter.Assign(Source: TPersistent);
begin
  inherited Assign( Source );

  if Source is TGRoadPresenter then
  begin
    EdgeColor := TGRoadPresenter( Source ).EdgeColor;
    EdgePixels := TGRoadPresenter( Source ).EdgePixels;
    RoadPen.Assign( TGRoadPresenter( Source ).RoadPen );
  end;
end;

procedure TGRoadPresenter.BeginLayerRender(aLayer: TGLayer);
begin
  inherited;

  FRenderPass := aLayer.RenderPass;
  FRoadPenWidth := TGlobe5(ParentGlobe).ScaleUnitsToDevice( FRoadPen.Width, FRoadPen.WidthUnit );
end;

constructor TGRoadPresenter.Create(parentGlobe: TGCustomGlobe5);
begin
  inherited;

  FRoadPen := TGPen.Create;
  FRoadPen.Color := clWhite;
  FRoadPen.Width := 8;  // Default to 8 meter wide road
  FRoadPen.WidthUnit := guMeter;

  FEdgePen := TGPen.Create;
  FEdgePen.WidthUnit := guPixel;
  
  FEdgePixels := 1;
  FEdgeColor := clGray;
end;

destructor TGRoadPresenter.Destroy;
begin
  FreeAndNil( FEdgePen );
  FreeAndNil( FRoadPen );

  inherited;
end;

procedure TGRoadPresenter.EndLayerRender(aLayer: TGLayer);
begin
  inherited;

  if ( aLayer.RenderPass = 0 ) and ( FRoadPenWidth > 0 ) then
    aLayer.ReRenderLayer := true;
end;

class function TGRoadPresenter.FindMatch( Presenters: TGPresenterStore;
  edgeColor : TColor; edgePixels : integer; roadPen: TGPen; ATitleFont: TGFont): integer;
var
  idx : integer;
  presenter : TGRoadPresenter;
begin
  for idx := 0 to Presenters.Count - 1 do
    if Presenters[idx] is TGRoadPresenter then
    begin
      presenter := TGRoadPresenter(Presenters[idx]);

      Result := presenter.PresenterID;

      if edgeColor <> presenter.EdgeColor then
        continue;
      if edgePixels <> presenter.edgePixels then
        continue;
      if ( roadPen <> nil ) and not presenter.RoadPen.Equals(roadPen) then
        continue;

      Exit; // we have found a match
    end;
  Result := 0;
end;

function TGRoadPresenter.LLInObject(ptLL: TGPointLL; mapObj: IGMapPoint;
  iTolerance: integer): Boolean;
begin
  Result := (mapObj as IGMapPoly).LLOnEdge(ptLL,iTolerance, false);
end;

procedure TGRoadPresenter.LoadFromXML(Element: TGXML_Element);
begin
  inherited;

  if Element <> nil then
  begin
    EdgeColor := StringToColor(Element.Attribute( 'EdgeColor', ColorToString( clGray )));
    EdgePixels := Element.Attribute('EdgePixels', 1 );

    FRoadPen.LoadFromXML( Element.Element('RoadPen'));
  end;
end;

procedure TGRoadPresenter.RenderLegend(Canvas: TCanvas; rect : TRect);
begin
  inherited;

  SaveDC( Canvas.Handle );

  Canvas.Refresh;

  Canvas.Pen.Assign( FEdgePen );
  Canvas.Pen.Color := EdgeColor;
  Canvas.Pen.Width := 2 + EdgePixels * 2;
  with rect do
    Canvas.Polyline([TopLeft, BottomRight]);

  Canvas.Pen.Assign( RoadPen );
  Canvas.Pen.Width := 2;
  with rect do
    Canvas.Polyline([TopLeft, BottomRight]);

  RestoreDC( Canvas.Handle, -1 );
  Canvas.Refresh;
end;

procedure TGRoadPresenter.RenderObject( mapObj: IGMapPoint; State: TGRenderStateSet);
begin
  if Supports( mapObj, IGMapPoly ) then
  begin
    Exclude( State, rsClosed );

    if ( FRenderPass > 0 ) or ( EdgePixels = 0 ) then
      Renderer.Pen := FRoadPen
    else
    begin
      Renderer.Pen := FEdgePen;
      if mapObj.Selected then
        Renderer.Pen.Color := Renderer.SelectedPen.Color
      else
        Renderer.Pen.Color := EdgeColor;

      if FRoadPenWidth > 0 then
        Renderer.Pen.Width := FRoadPenWidth + EdgePixels * 2
      else
        Renderer.Pen.Width := 1;
    end;

    TGlobe5(ParentGlobe).RenderChainStore((mapObj as IGMapPoly).Chains, State );
  end;
end;

procedure TGRoadPresenter.SaveToXML(Element: TGXML_Element);
begin
  inherited;

  Element.AddAttribute( 'EdgeColor', ColorToString( EdgeColor ), ColorToString( clGray ));
  Element.AddAttribute( 'EdgePixels', EdgePixels, 1 );

  FRoadPen.SaveToXML( Element.AddElement('RoadPen'));
end;

initialization
  RegisterGlobeClass( TGRoadPresenter, 'Road Presenter' );
end.
