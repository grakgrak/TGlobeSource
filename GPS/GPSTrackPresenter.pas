unit GPSTrackPresenter;

interface

Uses Windows, Classes, Forms, Graphics, SysUtils, GPresenters, 
  Globe5, GClasses, GMapObjects, GSysUtils, GXML, GResource;

type
  TGGPSTrackPresenter = class(TGPresenter)
  private
    FLocationColor: TColor;
    FShowTrack: Boolean;
    FLocPen : TGPen;
    FLocBrush : TGBrush;
    FTrackPen : TGPen;
    procedure SetLocationColor(const Value: TColor);
  public
    constructor Create( parentGlobe: TGCustomGlobe5 ); override;
    destructor Destroy; override;

    procedure Assign(Source : TPersistent); override;

    function LLInObject(ptLL : TGPointLL; mapObj : IGMapPoint; iTolerance : integer) : Boolean; override;

    procedure RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet); override;

    procedure LoadFromXML( Element : TGXML_Element ); override;
    procedure SaveToXML( Element : TGXML_Element ); override;
  published
    property LocationColor : TColor read FLocationColor write SetLocationColor;
    property ShowTrack : Boolean read FShowTrack write FShowTrack;
  end;

implementation

{ TGGPSTrackPresenter }

procedure TGGPSTrackPresenter.Assign(Source: TPersistent);
begin
  inherited;

  if Source.InheritsFrom( TGGPSTrackPresenter ) then
  begin
    LocationColor := TGGPSTrackPresenter(Source).LocationColor;
    ShowTrack := TGGPSTrackPresenter(Source).ShowTrack;
  end;
end;

constructor TGGPSTrackPresenter.Create(parentGlobe: TGCustomGlobe5);
begin
  inherited;
  FTrackPen := TGPen.Create;
  FTrackPen.Style := gpsSolid;
  FTrackPen.Color := clSilver;

  FLocPen := TGPen.Create;

  FLocBrush := TGBrush.Create;
  FLocBrush.Style := gbsClear;

  LocationColor := clRed;
  FShowTrack := true;
end;

destructor TGGPSTrackPresenter.Destroy;
begin
  FreeAndNil( FLocPen );
  FreeAndNil( FTrackPen );
  FreeAndNil( FLocBrush );
  inherited;
end;

function TGGPSTrackPresenter.LLInObject(ptLL: TGPointLL;
  mapObj: IGMapPoint; iTolerance: integer): Boolean;
begin
  Result := mapObj.LLInObject(ptLL, iTolerance);
end;

procedure TGGPSTrackPresenter.LoadFromXML(Element: TGXML_Element);
begin
  inherited;

  LocationColor := StringToColor( Element.Attribute( 'LocColor', ColorToString( clRed )));
  ShowTrack := Element.Attribute('ShowTrack', true);
end;

procedure TGGPSTrackPresenter.RenderObject( mapObj: IGMapPoint; State: TGRenderStateSet);
var
  ptXY : TPoint;
  ptLL : TGPointLL;
  idx : integer;
begin
  with TGlobe5(ParentGlobe) do
  begin
    if Supports( mapObj, IGMapPoly ) then
    begin
      with (mapObj as IGMapPoly) do
      begin
        if Chains[0].Count = 0 then
          Exit;

        Renderer.Pen := FTrackPen;
        RenderChainStore( Chains, [] );

        for idx := 0 to Chains[0].Count - 1 do
          with Chains[0][idx] do
            if Projector.LLToDeviceXY( iLongX, iLatY, ptXY) then
              with Renderer.Canvas do
              begin
                Pixels[ptXY.X, ptXY.Y] := clRed;
                Pixels[ptXY.X + 1, ptXY.Y] := clRed;
                Pixels[ptXY.X + 1, ptXY.Y + 1] := clRed;
                Pixels[ptXY.X, ptXY.Y + 1] := clRed;
              end;

        ptLL := Chains[0].AsLL[Chains[0].Count - 1];
      end;
    end
    else
      ptLL := mapObj.Centroid;

    if Projector.LLToDeviceXY( ptLL.iLongX, ptLL.iLatY, ptXY) then
    begin
      Renderer.Pen := FLocPen;
      Renderer.Brush := FLocBrush;

      Renderer.Canvas.Ellipse( ptXY.X - 8, ptXY.Y - 8, ptXY.X + 8, ptXY.Y + 8 );
      Renderer.Canvas.Ellipse( ptXY.X - 3, ptXY.Y - 3, ptXY.X + 3, ptXY.Y + 3 );
    end;
  end;
end;

procedure TGGPSTrackPresenter.SaveToXML(Element: TGXML_Element);
begin
  inherited;

  Element.AddAttribute('LocColor', ColorToString( LocationColor ), ColorToString( clRed ));
  Element.AddAttribute('ShowTrack', ShowTrack, true);
end;

procedure TGGPSTrackPresenter.SetLocationColor(const Value: TColor);
begin
  FLocationColor := Value;
  FLocPen.Color := FLocationColor;
end;

initialization
  RegisterGlobeClass( TGGPSTrackPresenter, 'GPS Track Presenter' );
end.
