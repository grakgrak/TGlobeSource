//-----------------------------------------------------------------------
//Summary
//TGlobe Thematic Presenter Classes
//
//Description
//Provides the basic Thematic Presenters for TGlobe map objects.
//
//Author
//Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------

{$I GLOBE5.INC}
unit GThematicPresenters;

interface

Uses Windows, Classes, Forms, Graphics, SysUtils,
  GClasses, GMapObjects, GSysUtils, GBitmap, GXML, GResource, GPresenters,
  Globe5Utils;

type
  TColorArray = array of TColor;

  TGThematicPointPresenter = class(TGPointPresenter)
  private
    FQuantile : TGQuantile;
    FValueColumn : integer;
    FNoDataValue : Double;
    procedure SetValueColumn(const Value: integer);
    procedure SetNoDataValue(const Value: Double);
  protected
    procedure BeginLayerRender( aLayer : TGLayer ); override;
  public
    constructor Create( parentGlobe: TGCustomGlobe5 ); override;
    destructor Destroy; override;

    function Equals( aPresenter : TGPresenter ) : Boolean; override;

    procedure RenderLegend( Canvas : TCanvas; rect : TRect ); override;
    procedure RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet); override;

    procedure LoadFromXML( Element : TGXML_Element ); override;
    procedure SaveToXML( Element : TGXML_Element ); override;
  published
    property ValueColumn : integer read FValueColumn write SetValueColumn;
    property NoDataValue : Double read FNoDataValue write SetNoDataValue;
  end;


  TGThematicPolyPresenter = class(TGPolyPresenter)
  private
    FQuantile : TGQuantile;
    FValueColumn: integer;
    FNoDataValue: Double;
    FClassCount: integer;
    FColors : TColorArray;
    FColorCount: integer;
    procedure SetValueColumn(const Value: integer);
    procedure SetNoDataValue(const Value: Double);
    procedure SetClassCount(const Value: integer);
    function GetColor(index: integer): TColor;
    procedure SetColor(index: integer; const Value: TColor);
    procedure SetColorCount(const Value: integer);
  protected
    procedure BeginLayerRender( aLayer : TGLayer ); override;
  public
    constructor Create( parentGlobe: TGCustomGlobe5 ); override;
    destructor Destroy; override;

    function Equals( aPresenter : TGPresenter ) : Boolean; override;

    procedure RenderLegend( Canvas : TCanvas; rect : TRect ); override;
    procedure RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet); override;

    procedure LoadFromXML( Element : TGXML_Element ); override;
    procedure SaveToXML( Element : TGXML_Element ); override;

    property Colors[index:integer] : TColor read GetColor write SetColor;
  published
    property ClassCount : integer read FClassCount write SetClassCount;
    property ColorCount : integer read FColorCount write SetColorCount;
    property NoDataValue : Double read FNoDataValue write SetNoDataValue;
    property ValueColumn : integer read FValueColumn write SetValueColumn;
  end;


implementation

Uses Globe5;

procedure TGThematicPointPresenter.BeginLayerRender(aLayer: TGLayer);
begin
  inherited;

  if FQuantile = nil then
    FQuantile := TGQuantile.Create(aLayer, FValueColumn, FNoDataValue );
end;


constructor TGThematicPointPresenter.Create( parentGlobe: TGCustomGlobe5 );
begin
  inherited;
  Name := 'Thematic Point Presenter';
end;


destructor TGThematicPointPresenter.Destroy;
begin
  FreeAndNil( FQuantile );
  inherited Destroy;
end;

function TGThematicPointPresenter.Equals( aPresenter : TGPresenter ) : Boolean;
begin
  Result := aPresenter is TGThematicPointPresenter;

  if not Result then
    Exit;

  Result := inherited Equals( aPresenter );

  if Result then
    with TGThematicPointPresenter( aPresenter ) do
      Result := ( OverrideColumn = Self.OverrideColumn )
          and ( PointType = Self.PointType )
          and ( PointUnit = Self.PointUnit )
          and ( PointSize = Self.PointSize )
          and ( SymbolIndex = Self.SymbolIndex )
          and ( ImageName = Self.ImageName )
          and ( MaxPointSize = Self.MaxPointSize )
          and PointPen.Equals( Self.PointPen )
          and PointBrush.Equals( Self.PointBrush )
          and PointFont.Equals( Self.PointFont );
end;


procedure TGThematicPointPresenter.LoadFromXML( Element : TGXML_Element );
begin
  inherited LoadFromXML( Element );

  if Element <> nil then
  begin
    ValueColumn := Element.Attribute( 'ValueColumn', -1);
    NoDataValue := Element.Attribute( 'NoDataValue', 0.0);
  end;
end;


procedure TGThematicPointPresenter.RenderLegend(Canvas: TCanvas; rect : TRect);
var
  iSize : integer;
begin
  inherited;

  SaveDC( Canvas.Handle );

  Canvas.Pen.Assign( PointPen );
  Canvas.Brush.Assign( PointBrush );

  Renderer := ParentGlobe.Renderer.Clone;
  Renderer.DoubleBuffer := false;
  Renderer.Canvas.Handle := Canvas.Handle;

  iSize := MinVal( rect.Right - rect.Left, rect.Bottom - rect.Top );
  RenderPoint( Rect.Left + iSize div 2, rect.Top + iSize div 2, iSize, [] );

  Renderer.Free;
  Renderer := nil;

  RestoreDC( Canvas.Handle, -1 );
  Canvas.Refresh;
end;


procedure TGThematicPointPresenter.RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet );
begin
end;


procedure TGThematicPointPresenter.SaveToXML( Element : TGXML_Element );
begin
  inherited SaveToXML( Element );

  Element.AddAttribute( 'ValueColumn', ValueColumn, -1);
  Element.AddAttribute( 'NoDataValue', NoDataValue, 0);
end;

procedure TGThematicPointPresenter.SetNoDataValue(const Value: Double);
begin
  if FNoDataValue <> Value then
  begin
    FreeAndNil( FQuantile );
    FNoDataValue := Value;
  end;
end;

procedure TGThematicPointPresenter.SetValueColumn(const Value: integer);
begin
  if FValueColumn <> Value then
  begin
    FreeAndNil( FQuantile );
    FValueColumn := Value;
  end;
end;


procedure TGThematicPolyPresenter.BeginLayerRender(aLayer: TGLayer);
begin
  inherited;

  if FQuantile = nil then
    FQuantile := TGQuantile.Create(aLayer, FValueColumn, FNoDataValue );

  if ClassCount > ColorCount then
    FQuantile.ColorGradient(FColors, FClassCount )
  else
    FQuantile.Colors( FColors );
end;

constructor TGThematicPolyPresenter.Create( parentGlobe: TGCustomGlobe5 );
begin
  inherited;

  Name := 'Thematic Poly Presenter';

  FColorCount := 2;
  SetLength( FColors, 2 );
  FColors[0] := clRed;
  FColors[1] := clBlue;

  FClassCount := 2;
end;


destructor TGThematicPolyPresenter.Destroy;
begin
  FreeAndNil( FQuantile );
  inherited Destroy;
end;


function TGThematicPolyPresenter.Equals( aPresenter : TGPresenter ) : Boolean;
begin
  Result := aPresenter.InheritsFrom( TGThematicPolyPresenter );

  if not Result then
    Exit;

  Result := inherited Equals( aPresenter );

  if Result then
    with TGThematicPolyPresenter( aPresenter ) do
      Result := ( PolyRenderMode = Self.PolyRenderMode )
            and PolyPen.Equals( Self.PolyPen )
            and PolyBrush.Equals( Self.PolyBrush );
end;

function TGThematicPolyPresenter.GetColor(index: integer): TColor;
begin
  Result := FColors[index];
end;

procedure TGThematicPolyPresenter.LoadFromXML( Element : TGXML_Element );
var
  idx : integer;
begin
  inherited LoadFromXML( Element );

  if Element <> nil then
  begin
    ValueColumn := Element.Attribute( 'ValueColumn', -1);
    NoDataValue := Element.Attribute( 'NoDataValue', 0);
    ClassCount := Element.Attribute( 'ClassCount', 2);
    ColorCount := Element.Attribute( 'ColorCount', 2);

    for idx := 0 to ColorCount - 1 do
      Colors[idx] := StringToColor( Element.Element( 'Color', idx ).BodyText );
  end;
end;


procedure TGThematicPolyPresenter.RenderLegend( Canvas : TCanvas; rect : TRect );
begin
  inherited;

  SaveDC( Canvas.Handle );

  Canvas.Refresh;
  Canvas.Pen.Assign( PolyPen );
  Canvas.Brush.Assign( PolyBrush );

  with rect do
    if IsClosed( nil ) then
      Canvas.Polygon([Point(Left,Top),Point(Right, Top), Point(Right,Bottom), Point(Left, Bottom)])
    else
      Canvas.Polyline([Point(Left,Top),Point(Right, Top), Point(Right,Bottom), Point(Left, Bottom)]);

  RestoreDC( Canvas.Handle, -1 );
  Canvas.Refresh;
end;

procedure TGThematicPolyPresenter.RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet );
begin
  if Supports( mapObj, IGMapPoly ) then
  begin
    if IsClosed( mapObj ) then
      Include( State, rsClosed )
    else
      Exclude( State, rsClosed );

    if rsSelected in State then
    begin
      Renderer.Brush := Renderer.SelectedBrush;
      Renderer.Pen := Renderer.SelectedPen;
    end
    else
    begin
      Renderer.AlphaBlend := AdjustedAlphaBlend;

      Renderer.Pen := PolyPen;

      if rsClosed in State then
        Renderer.Brush := PolyBrush;

      if ValueColumn >= 0 then
        Renderer.Brush.Color := FQuantile.ClassifyToColor(mapObj.Value[ValueColumn]);
    end;

    TGlobe5(ParentGlobe).RenderChainStore((mapObj as IGMapPoly).Chains, State );
  end;
end;

procedure TGThematicPolyPresenter.SaveToXML( Element : TGXML_Element );
var
  idx : integer;
begin
  inherited SaveToXML( Element );

  Element.AddAttribute( 'ValueColumn', ValueColumn, -1);
  Element.AddAttribute( 'NoDataValue', NoDataValue, 0);
  Element.AddAttribute( 'ClassCount', ClassCount, 2);
  Element.AddAttribute( 'ColorCount', ColorCount, 2);

  // Save the colors for the Presenter
  for idx := 0 to ColorCount - 1 do
    Element.AddElement( 'Color' ).BodyText := ColorToString( Colors[idx] );
end;


procedure TGThematicPolyPresenter.SetClassCount(const Value: integer);
begin
  if ( FClassCount <> Value ) and ( Value >= 2 ) then
  begin
    FreeAndNil( FQuantile );
    FClassCount := Value;
  end;
end;

procedure TGThematicPolyPresenter.SetColor(index: integer; const Value: TColor);
begin
  FColors[index] := Value;
end;

procedure TGThematicPolyPresenter.SetColorCount(const Value: integer);
begin
  if Value >= 2 then
  begin
    FColorCount := Value;
    Setlength( FColors, Value );
    FreeAndNil( FQuantile );
  end;
end;

procedure TGThematicPolyPresenter.SetNoDataValue(const Value: Double);
begin
  if FNoDataValue <> Value then
  begin
    FreeAndNil( FQuantile );
    FNoDataValue := Value;
  end;
end;

procedure TGThematicPolyPresenter.SetValueColumn(const Value: integer);
begin
  if FValueColumn <> Value then
  begin
    FreeAndNil( FQuantile );
    FValueColumn := Value;
  end;
end;

initialization
  RegisterGlobeClass( TGThematicPointPresenter, 'Thematic Point Presenter' );
  RegisterGlobeClass( TGThematicPolyPresenter, 'Thematic Poly Presenter' );
end.
