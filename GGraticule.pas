//-------------------------------------------
//Summary
//TGlobe Graticule Rendering class
//
//Description
//Displays a graticule on the globes surface.
//
//Author
//Graham Knight (tglobe@tglobe.com)          
//-------------------------------------------
unit GGraticule;

interface

uses Classes, Graphics, SysUtils, GClasses, GSysUtils, GXML;


type
  TGGraticuleDraw = (gdNone, gdBehind, gdInFront);

  TGGraticule = class(TGComponent)
  private
    FiLongitudeStep: Integer;
    FiLatitudeStep: Integer;
    FOnChanged: TNotifyEvent;
    FDrawMode: TGGraticuleDraw;
    FGraticulePen: TGPen;
    procedure SetLatitudeStep(const Value: Integer);
    procedure SetLongitudeStep(const Value: Integer);
    procedure SetDrawMode(const Value: TGGraticuleDraw);
    procedure SetGraticulePen(const Value: TGPen);
  protected
    procedure DoChanged(Sender: TObject);
  public
    constructor Create( parentGlobe : TGCustomGlobe5 ); override;
    destructor Destroy; override;

    procedure Render; virtual;
    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;

    property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;
  published
    property DrawMode : TGGraticuleDraw read FDrawMode write SetDrawMode default gdBehind;
    property GraticulePen : TGPen read FGraticulePen write SetGraticulePen;
    property LatitudeStep : Integer read FiLatitudeStep write SetLatitudeStep default 15;
    property LongitudeStep : Integer read FiLongitudeStep write SetLongitudeStep default 15;
  end;


implementation

Uses Globe5;

constructor TGGraticule.Create;
begin
  inherited;

  FDrawMode := gdBehind;
  FiLongitudeStep := 10;
  FiLatitudeStep := 10;

  FGraticulePen := TGPen.Create;
  FGraticulePen.Define( clSilver, gpsSolid, 1, guPixel );
  FGraticulePen.OnChange := DoChanged;
end;

destructor TGGraticule.Destroy;
begin
  FreeAndNil( FGraticulePen );
  inherited;
end;

procedure TGGraticule.DoChanged(Sender: TObject);
begin
  if Assigned( FOnChanged ) then
    FOnChanged(Self);
end;

procedure TGGraticule.LoadFromXML(Element: TGXML_Element);
begin
  inherited;
  if Element <> nil then
  begin
    FiLongitudeStep := Element.Attribute( 'LongStep', 0 );
    FiLatitudeStep := Element.Attribute( 'LatStep', 0 );
    FDrawMode := TGGraticuleDraw( Element.Attribute( 'DrawMode', Ord(gdBehind)));
    GraticulePen.LoadFromXML( Element.Element( 'Pen' ));
    DoChanged(Self);
  end;
end;

procedure TGGraticule.Render;
var
  idx : Integer;
  ptTmp : TGPointLL;
begin
{  if ParentGlobe.Projector.RenderBackFace then
    ParentGlobe.Renderer.Pen := ParentGlobe.DefaultAttributes.TransparentPen
  else}
    ParentGlobe.Renderer.Pen := GraticulePen;

  // Draw lines of Longitude
  idx := -GU_180_DEGREE + 1;
  if LongitudeStep > 0 then
    repeat
      TGlobe5(ParentGlobe).RenderLine( PointLL( idx, -GU_90_DEGREE + GU_DEGREE ),
        PointLL( idx, GU_90_DEGREE - GU_DEGREE ), TG_GRATICULE_LONG_STEPS );
      Inc( idx, LongitudeStep * GU_DEGREE );
    until idx >= GU_180_DEGREE;

  // Draw lines of Latitude
  idx := 0;
  if LatitudeStep > 0 then
    repeat
      ptTmp := PointLL( 0, idx );
      TGlobe5(ParentGlobe).RenderLine( PointLL( GU_180_DEGREE - 1, idx ), ptTmp, TG_GRATICULE_LAT_STEPS );
      TGlobe5(ParentGlobe).RenderLine( ptTmp, PointLL( -GU_180_DEGREE + 1, idx ), TG_GRATICULE_LAT_STEPS );

      ptTmp := PointLL( 0, -idx );
      TGlobe5(ParentGlobe).RenderLine( PointLL( GU_180_DEGREE - 1, -idx ), ptTmp, TG_GRATICULE_LAT_STEPS );
      TGlobe5(ParentGlobe).RenderLine( ptTmp, PointLL( -GU_180_DEGREE + 1, -idx ), TG_GRATICULE_LAT_STEPS );

      Inc( idx, LatitudeStep * GU_DEGREE );
    until idx >= 85 * GU_DEGREE;
end;

procedure TGGraticule.SaveToXML(Element : TGXML_Element);
begin
  inherited;
  
  Element.AddAttribute( 'LongStep', LongitudeStep, 0 );
  Element.AddAttribute( 'LatStep', LatitudeStep, 0 );
  Element.AddAttribute( 'DrawMode', Ord(DrawMode), Ord(gdBehind));
  GraticulePen.SaveToXML( Element.AddElement( 'Pen' ));
end;

procedure TGGraticule.SetDrawMode(const Value: TGGraticuleDraw);
begin
  if FDrawMode <> Value then
  begin
    FDrawMode := Value;
    DoChanged(Self);
  end;
end;

procedure TGGraticule.SetGraticulePen(const Value: TGPen);
begin
  FGraticulePen.Assign( Value );
  FGraticulePen.OnChange := DoChanged;
  DoChanged(Self);
end;

procedure TGGraticule.SetLatitudeStep(const Value: Integer);
begin
  if FiLatitudeStep <> Value then
  begin
    FiLatitudeStep := Value;
    DoChanged(Self);
  end;
end;

procedure TGGraticule.SetLongitudeStep(const Value: Integer);
begin
  if FiLongitudeStep <> Value then
  begin
    FiLongitudeStep := Value;
    DoChanged(Self);
  end;
end;

initialization
  RegisterGlobeClass( TGGraticule, 'Graticule' );
end.
