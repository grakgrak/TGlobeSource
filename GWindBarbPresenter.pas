//-----------------------------------------------------------------------
//Summary
//TGlobe WindBarb Presenter
//
//Description
//Provides a presenter that is used to display wind direction and strength.
//
//The presenter expects that the Wind direction, Wind Strength and Temperature
//values are stored in the map objects attributes.
//Set the DirectionColumn, StrengthColumn and	TempColumn properties to the
//correct attribute columns. Currently the TempColumn is not used.
//The values in the DirectionColumn should be in decimal degrees.
//
//Author
//Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------

unit GWindBarbPresenter;

interface

Uses WinTypes, WinProcs, SysUtils, Classes, Graphics, Globe5, GSysUtils,
  GClasses, GXML, Globe5Utils;

type
	TGWindBarbPresenter = class( TGPresenter )
	private
		FBarbColor : TColor;
    FBarbFillColor: TColor;
		FBarbLength : integer;
		FBarbUnit : TGUnitTypes;
		FDirectionColumn : integer;
		FStrengthColumn : integer;
		FTempColumn : integer;
    procedure RenderArrow( const ptFrom, ptTo : TPoint; const Speed : double );
	public
    constructor Create( parentGlobe: TGCustomGlobe5 ); override;
    procedure RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet ); override;

    procedure Assign(Source : TPersistent); override;
    function Equals( aPresenter : TGPresenter ) : Boolean; override;

    procedure LoadFromXML( Element : TGXML_Element ); override;
    procedure SaveToXML( Element : TGXML_Element ); override;
	published
		property BarbColor : TColor read FBarbColor write FBarbColor;
		property BarbFillColor : TColor read FBarbFillColor write FBarbFillColor;
		property BarbLength : integer read FBarbLength write FBarbLength;
		property BarbUnit : TGUnitTypes read FBarbUnit write FBarbUnit;

    property DirectionColumn : integer read FDirectionColumn write FDirectionColumn;
		property StrengthColumn : integer read FStrengthColumn write FStrengthColumn;
		property TempColumn : integer read FTempColumn write FTempColumn;
	end;

implementation

{------------------------------------------------------------------------------
  TGWindBarbPresenter.Create
------------------------------------------------------------------------------}
procedure TGWindBarbPresenter.Assign(Source: TPersistent);
begin
  inherited Assign( Source );

  if Source is TGWindBarbPresenter then
    with TGWindBarbPresenter(Source) do
    begin
      Self.BarbColor := BarbColor;
      Self.BarbFillColor := BarbFillColor;
      Self.BarbLength := BarbLength;
      Self.BarbUnit := BarbUnit;

      Self.DirectionColumn := DirectionColumn;
      Self.StrengthColumn := StrengthColumn;
      Self.TempColumn := TempColumn;
    end;
end;

constructor TGWindBarbPresenter.Create( parentGlobe : TGCustomGlobe5 );
begin
	inherited;

	FBarbColor := clBlack;
	FBarbFillColor := clWhite;
	FBarbUnit := guPixel;
	FBarbLength := 20;
	FDirectionColumn := -1;
	FStrengthColumn := -1;
	FTempColumn := -1;
end;

function TGWindBarbPresenter.Equals(aPresenter: TGPresenter): Boolean;
begin
  Result := aPresenter is TGWindBarbPresenter;

  if not Result then
    Exit;

  Result := inherited Equals( aPresenter );

  if Result then
    with TGWindBarbPresenter( aPresenter ) do
      Result := (Self.BarbColor = BarbColor)
        and (Self.BarbFillColor = BarbFillColor)
        and (Self.BarbLength = BarbLength)
        and (Self.BarbUnit = BarbUnit)
        and (Self.DirectionColumn = DirectionColumn)
        and (Self.StrengthColumn = StrengthColumn)
        and (Self.TempColumn = TempColumn);

end;

procedure TGWindBarbPresenter.LoadFromXML(Element: TGXML_Element);
begin
  inherited;

  if Element <> nil then
  begin
    BarbColor := Element.Attribute( 'BarbColor', clBlack );
    BarbFillColor := Element.Attribute( 'BarbFillColor', clWhite );
    BarbLength := Element.Attribute( 'BarbLength', 20 );
    BarbUnit := TGUnitTypes(Element.Attribute( 'BarbUnit', Ord(guPixel)));

    DirectionColumn := Element.Attribute( 'DirectionColumn', -1 );
    StrengthColumn := Element.Attribute( 'StrengthColumn', -1 );
    TempColumn := Element.Attribute( 'TempColumn', -1 );
  end;
end;

procedure TGWindBarbPresenter.RenderArrow(const ptFrom, ptTo: TPoint; const Speed : double);
var
  xDiff, yDiff : integer;
  Len, Radius, Hypotenuse : double;
  ptTip, ptLeft, ptRight, ptBack : TPoint;
  tmp, ArrowSize, ArrowLength, ArrowInnerLength : double;
begin
  with TGlobe5( ParentGlobe ) do
    tmp := GlobeUnitsTo( GlobeUnitsFrom( Speed / 3, BarbUnit ), guPixel );

  ArrowSize := tmp / 2;
  ArrowLength := tmp;
  ArrowInnerLength := tmp;

  XDiff := ptTo.X - ptFrom.X;
  YDiff := ptTo.Y - ptFrom.Y;

  Hypotenuse := Hypot(XDiff, YDiff);
  if Hypotenuse = 0 then
    Exit;

  ptTip.X := ptTo.X + Round(ArrowInnerLength / Hypotenuse * (XDiff));
  ptTip.Y := ptTo.Y + Round(ArrowInnerLength / Hypotenuse * (YDiff));

  Radius := ArrowSize / Hypotenuse;
  Len := ArrowLength / ArrowSize;

  ptLeft.X := ptTip.X + Round((-YDiff - Len * XDiff) * Radius);
  ptLeft.Y := ptTip.Y + Round((XDiff - Len * YDiff) * Radius);

  ptRight.X := ptTip.X + Round((YDiff - Len * XDiff) * Radius);
  ptRight.Y := ptTip.Y + Round((-XDiff - Len * YDiff) * Radius);

  ptBack.X := ptTip.X - Round(ArrowInnerLength / Hypotenuse * (XDiff));
  ptBack.Y := ptTip.Y - Round(ArrowInnerLength / Hypotenuse * (YDiff));

  Renderer.Pen.Color := BarbColor;
  Renderer.Brush.Color := BarbFillColor;
  Renderer.RealizeAttributes;
  
  TGlobe5(ParentGlobe).Renderer.DrawPoly([ptTip, ptLeft, ptBack, ptRight], true);
  Renderer.Canvas.MoveTo( ptFrom.X, ptFrom.Y);
  Renderer.Canvas.LineTo( ptTo.X, ptTo.Y);
end;

procedure TGWindBarbPresenter.RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet );
var
	CalmRadius : integer;
	FlagLen    : single;
	LineLen    : single;
	LineSpace  : single;
	FlagWidth  : single;
	LineLean   : single;
	offset     : single;		// amount of stem left
	anyFlags   : Boolean;
	Hemisphere : integer;

	val, eSin, eCos   : Extended;
	startRadius : integer;
	iTmp, iX, iY : integer;
	Speed : integer;
  tmpPoint : TGPointLL;
  pt : TPoint;
  dir : Extended;
begin
	if ( DirectionColumn < 0 ) or ( StrengthColumn < 0 ) then
  begin
    mapObj.Render(ParentGlobe); // Just let the object render itself.
    Exit;
  end;

  with TGlobe5( ParentGlobe ) do
  begin
    iTmp := Round( GlobeUnitsTo( GlobeUnitsFrom( BarbLength, BarbUnit ), guPixel ));

    CalmRadius := iTmp div 7;
    startRadius := 0;
    offset := iTmp;
    flagwidth := iTmp div 7;
    linelean := iTmp div 10;
    linespace := iTmp div 10;

		if Projector.ProjectionModel.PointLLToXY( mapObj.Centroid, 0 ) then
    begin
      if mapObj.Centroid.iLatY < 0 then	{ if in southern hemisphere }
        Hemisphere := -1
      else
        Hemisphere := +1;

      flaglen := iTmp div 2 * Hemisphere;
      Linelen := iTmp div 2 * Hemisphere;

      val := mapObj.Value[StrengthColumn];
      if IsNan( val ) then
        val := 0;

      Speed := Round(val);

      iX := Renderer.Points[0].X;
      iY := Renderer.Points[0].Y;

      val := mapObj.Value[DirectionColumn];
      if IsNan( val ) then
        val := 0;


      tmpPoint:=AngleDistanceToGCPoint(mapObj.Centroid,0, Round(10/Projector.ScaleFactor ));
      Projector.ProjectionModel.LLToXY(tmpPoint.iLongX,tmpPoint.iLatY,0);
      pt := Renderer.Points[0];
      if (pt.X<>iX) or (pt.Y<>iY) then
      begin
        dir:=ArcTan2((pt.X-iX),(iY-pt.Y))*180/LocalPi;
        SinCos( (dir*10+val) * LocalPi / 1800, eSin, eCos );
      end
      else
        SinCos( val * LocalPi / 1800, eSin, eCos );

      Renderer.Pen.Color := BarbColor;
      Renderer.Brush.Color := BarbFillColor;
      Renderer.AlphaBlend := AdjustedAlphaBlend;
      Renderer.RealizeAttributes;

      if Speed = 0 then
        Renderer.Canvas.Ellipse( iX - CalmRadius, iY - CalmRadius, iX + CalmRadius + 1, iY + CalmRadius + 1)
      else
      begin
        RenderArrow(
          Point( Round(iX + startRadius * eSin), Round(iY - startRadius * eCos)),
          Point( Round(iX + offset * eSin), Round(iY - offset * eCos)),
          Speed / 2);
        exit;


        Renderer.Canvas.MoveTo( Round(iX + startRadius * eSin), Round(iY - startRadius * eCos));
        Renderer.Canvas.LineTo( Round(iX + offset * eSin), Round(iY - offset * eCos));

        if Speed > 2 then 	//needs barbs
        begin
          anyFlags := False;

          while Speed >= 48 do
          begin
            anyFlags := True;
            Renderer.Canvas.Polygon( [
              Point( Round(iX + offset * eSin), Round(iY - offset * eCos)),
              Point( Round(iX + (offset - FlagWidth) * eSin), Round(iY - (offset - FlagWidth) * eCos)),
              Point( Round(iX + ((offset - FlagWidth/2) * eSin) + (FlagLen * eCos)),
                    Round(iY - ((offset - FlagWidth/2) * eCos) + (FlagLen * eSin)))
              ] );

            Speed  := Speed - 50;
            offset := offset - FlagWidth;
          end; //while

          if AnyFlags then
            offset := offset - LineSpace;

          while Speed >= 8 do
          begin
            Renderer.Canvas.MoveTo( Round(iX + offset * eSin), Round(iY - offset * eCos));
            Renderer.Canvas.LineTo( Round(iX + ((offset + LineLean) * eSin) + (LineLen * eCos)),
                    Round(iY - ((offset + LineLean) * eCos) + (LineLen * eSin)));
            anyFlags := true;
            offset := offset - LineSpace;
            Speed  := Speed - 10;
          end;//while

          while Speed >= 3 do
          begin
            if not AnyFlags then
              offset := offset - LineSpace;

            Speed  := Speed - 5;
            Renderer.Canvas.MoveTo( Round(iX + offset * eSin), Round(iY - offset * eCos));
            Renderer.Canvas.LineTo( Round(iX + ((offset + LineLean/2) * eSin) + ((LineLen / 2) * eCos)),
                    Round(iY - ((offset + LineLean/2) * eCos) + ((LineLen / 2) * eSin)));
          end;//while
        end;
      end;
    end;
  end;
end;

procedure TGWindBarbPresenter.SaveToXML(Element: TGXML_Element);
begin
  inherited SaveToXML( Element );

  Element.AddAttribute( 'BarbColor', BarbColor, clBlack );
  Element.AddAttribute( 'BarbfillColor', BarbFillColor, clWhite );
  Element.AddAttribute( 'BarbLength', BarbLength, 20 );
  Element.AddAttribute( 'BarbUnit', Ord(BarbUnit), Ord(guPixel));

  Element.AddAttribute( 'DirectionColumn', DirectionColumn, -1 );
  Element.AddAttribute( 'StrengthColumn', StrengthColumn, -1 );
  Element.AddAttribute( 'TempColumn', TempColumn, -1 );
end;

initialization
  RegisterGlobeClass( TGWindBarbPresenter, 'WindBarb Presenter' );
end.
