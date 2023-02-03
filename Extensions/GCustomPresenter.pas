unit GCustomPresenter;

interface

Uses Classes, SysUtils,
  GSysUtils, GClasses, GPresenterInterpreter, GXML;

type
  TGCustomPresenter = class( TGPresenter )
  private
    FInterpreter : TGPresenterInterpreter;
    FCurrentObject : IGMapPoint;
    FExpressionCSL: String;
    FRenderPass : integer;
  protected
    procedure RenderPoly( mapObj : IGMapPoint; State : TGRenderStateSet ); virtual;
    procedure BeginLayerRender( aLayer : TGlayer ); override;
  public
    constructor Create(parentGlobe: TGCustomGlobe5); override;
    destructor Destroy; override;

    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;

    property CurrentObject : IGMapPoint read FCurrentObject;
    property RenderPass : integer read FRenderPass;
  published
    property ExpressionCSL : String read FExpressionCSL write FExpressionCSL;
  end;

implementation

Uses Globe5, GMapObjects;

{ TGCustomPresenter }

procedure TGCustomPresenter.BeginLayerRender( aLayer : TGlayer );
var
  sl : TStringList;
  idx : integer;
begin
  inherited;

  FRenderPass := aLayer.RenderPass;
  if FInterpreter <> nil then
  begin
    FInterpreter.Clear;

    sl := TStringList.Create;
    sl.CommaText := ExpressionCSL;

    for idx := 0 to sl.Count - 1 do
      FInterpreter.AddExpression( sl[idx]);

    sl.Free;
  end;
end;

constructor TGCustomPresenter.Create(parentGlobe: TGCustomGlobe5);
begin
  inherited;
  FInterpreter := TGPresenterInterpreter.Create(Self);
end;

destructor TGCustomPresenter.Destroy;
begin
  FreeAndNil( FInterpreter );
  inherited;
end;

procedure TGCustomPresenter.LoadFromXML(Element: TGXML_Element);
begin
  inherited;

  if Element <> nil then
    FExpressionCSL := Element.Attribute('ExprCSL', '');
end;

procedure TGCustomPresenter.RenderPoly(mapObj: IGMapPoint; State: TGRenderStateSet);
begin
  if FInterpreter <> nil then
  begin
    FCurrentObject := mapObj;
    FInterpreter.Execute;
  end;
  TGlobe5(ParentGlobe).RenderChainStore((mapObj as IGMapPoly).Chains, State );
end;

procedure TGCustomPresenter.SaveToXML(Element: TGXML_Element);
begin
  inherited;
  Element.AddAttribute('ExprCSL', ExpressionCSL, '');
end;

end.
