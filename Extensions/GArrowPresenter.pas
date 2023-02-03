
{$I GLOBE5.INC}
unit GArrowPresenter;

interface

Uses Windows, Classes, Forms, Graphics, SysUtils,
  Globe5, GClasses, GMapObjects, GSysUtils, GXML, GResource;

type
  TGArrowPresenter = class(TGPresenter)
  private
    FArrowBrush: TGBrush;
    FArrowPen: TGPen;
    FArrowLength: integer;
    FArrowSize: integer;
    FArrowInnerLength: integer;
  public
    constructor Create( parentGlobe: TGCustomGlobe5 ); override;
    destructor Destroy; override;

    procedure Assign(Source : TPersistent); override;
    function Equals( aPresenter : TGPresenter ) : Boolean; override;

    function LLInObject(ptLL : TGPointLL; mapObj : IGMapPoint; iTolerance : integer) : Boolean; override;
//    procedure RenderLegend( Canvas : TCanvas; rect : TRect ); override;
    procedure RenderObject( mapObj: IGMapPoint; State: TGRenderStateSet); override;

    procedure LoadFromXML( Element : TGXML_Element ); override;
    procedure SaveToXML( Element : TGXML_Element ); override;
  published
    property ArrowPen : TGPen read FArrowPen write FArrowPen;
    property ArrowBrush : TGBrush read FArrowBrush write FArrowBrush;
    property ArrowInnerLength : integer read FArrowInnerLength write FArrowInnerLength;
    property ArrowLength : integer read FArrowLength write FArrowLength;
    property ArrowSize : integer read FArrowSize write FArrowSize;
  end;

implementation


{ TGArrowPresenter }

procedure TGArrowPresenter.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TGArrowPresenter.Create(parentGlobe: TGCustomGlobe5);
begin
  inherited;

  Name := 'Arrow Presenter';
  ArrowInnerLength := 10;
  ArrowLength := 10;
  ArrowSize := 6;
  ArrowPen := TGPen.Create;
  ArrowBrush := TGBrush.Create;
end;

destructor TGArrowPresenter.Destroy;
begin
  ArrowPen.Free;
  ArrowBrush.Free;

  inherited;
end;

function TGArrowPresenter.Equals(aPresenter: TGPresenter): Boolean;
begin
  Result := inherited Equals(aPresenter);
end;

function TGArrowPresenter.LLInObject(ptLL: TGPointLL; mapObj: IGMapPoint;
  iTolerance: integer): Boolean;
begin
  Result := mapObj.LLInObject(ptLL,iTolerance);
end;

procedure TGArrowPresenter.LoadFromXML(Element: TGXML_Element);
begin
  inherited;

  if Element <> nil then
  begin
    ArrowPen.LoadFromXML( Element.Element('Pen'));
    ArrowBrush.LoadFromXML( Element.Element('Brush'));

    ArrowInnerLength := Element.Attribute( 'InnerLength', 10 );
    ArrowLength := Element.Attribute( 'Length', 10 );
    ArrowSize := Element.Attribute( 'Size', 6 );
  end;
end;

procedure TGArrowPresenter.RenderObject( mapObj: IGMapPoint; State: TGRenderStateSet);
var
  XDiff, YDiff: integer;
  ptLeft, ptRight, ptBack: TPoint;
  Radius, Len, Hypotenuse: Extended;
  V2, ptTip: TPoint;
  idx : integer;
begin
  if Supports( mapObj, IGMapPoly ) then
    with mapObj as IGMapPoly, TGlobe5(ParentGlobe) do
    begin
      Closed := false;
      Renderer.Pen := ArrowPen;
      RenderChainStore(Chains, State);

      idx := Chains[0].Count;
      if idx > 1 then
      begin
        TGlobe5(ParentGlobe).Projector.PointLLToXY(Chains[0].AsLL[idx - 1], ptTip);
        TGlobe5(ParentGlobe).Projector.PointLLToXY(Chains[0].AsLL[idx - 2], V2);

        XDiff := ptTip.X - V2.X;
        YDiff := ptTip.Y - V2.Y;

        try
          Hypotenuse := Hypot(XDiff, YDiff);
          if Hypotenuse = 0 then
            Exit;
          Radius := ArrowSize / Hypotenuse;
          Len := ArrowLength / ArrowSize;

          ptLeft.X := ptTip.X + Round((-YDiff - Len * XDiff) * Radius);
          ptLeft.Y := ptTip.Y + Round((XDiff - Len * YDiff) * Radius);

          ptRight.X := ptTip.X + Round((YDiff - Len * XDiff) * Radius);
          ptRight.Y := ptTip.Y + Round((-XDiff - Len * YDiff) * Radius);

          ptBack.X := ptTip.X - Round(ArrowInnerLength / Hypotenuse * (XDiff));
          ptBack.Y := ptTip.Y - Round(ArrowInnerLength / Hypotenuse * (YDiff));

          Renderer.Brush := ArrowBrush;
          TGlobe5(ParentGlobe).Renderer.DrawPoly([ptTip, ptLeft, ptBack, ptRight], true);
        except
          // Do Nothing
        end;
      end;
    end;
end;

procedure TGArrowPresenter.SaveToXML(Element: TGXML_Element);
begin
  inherited;

  ArrowPen.SaveToXML( Element.AddElement('Pen'));
  ArrowBrush.SaveToXML( Element.AddElement('Brush'));

  Element.AddAttribute( 'InnerLength', ArrowInnerLength, 10 );
  Element.AddAttribute( 'Length', ArrowLength, 10 );
  Element.AddAttribute( 'Size', ArrowSize, 6 );
end;

initialization
  RegisterGlobeClass( TGArrowPresenter, 'Arrow Presenter' );
end.
