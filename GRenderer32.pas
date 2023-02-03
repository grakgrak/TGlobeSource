{-------------------------------------------------------------------------
 Module:    TGlobe Renderer using Graphics32

 Comment:   New Renderer using Graphics32 pacakage

            Graphics32 is Copyright (c) 2001-2003 Alex A. Denisov. All rights reserved

            This unit is built against Graphics32 1.5.1

            Antialising enhancements thanks to William Roberts.

 Classes:   TGRenderer32

 Author:    Graham Knight
  Email:    tglobe@tglobe.com
-------------------------------------------------------------------------}
{$R-}
{$Q-}
{$I GLOBE5.INC}

unit GRenderer32;

interface

Uses Windows, Classes, SysUtils, Graphics, GClasses, GSysUtils, GBitmap,
  GR32, GR32_Polygons, GXML;

type
  TGRenderer32 = class( TGRenderer )
  private
    FBackBuffer : TBitmap32;
    FAntialiased: Boolean;
  protected
    function GetCanvas: TCanvas; override;
    procedure DrawPolyPoly( const Points : array of GClasses.TPointArray; closed : Boolean); override;
  public
    constructor Create(ParentGlobe : TGCustomGlobe5); override;
    destructor Destroy; override;

    procedure RefreshDC; override;
    procedure SetSize( newWidth, newHeight : integer ); override;
    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;

    procedure DrawTextRect( const aRect : TRect; const text : String ); override;
    procedure DrawPixel( X, Y : integer; color: TColor; alpha : Byte ); override;
    procedure DrawScanLine(Y, x1, x2: integer; u1, u2, v1, v2: Double; const image: TGBitmap); override;
  published
    property Antialiased : Boolean read FAntialiased write FAntialiased;
  end;

implementation

Uses Globe5;

{ TGRenderer32 }

constructor TGRenderer32.Create(ParentGlobe: TGCustomGlobe5);
begin
  inherited;

  FAntialiased := false;
  FBackBuffer := TBitmap32.Create;
  SetSize( ParentGlobe.Width, ParentGlobe.Height );
  RefreshDC;
end;

destructor TGRenderer32.Destroy;
begin
  FBackBuffer.Canvas.Refresh;
  FreeAndNil( FBackBuffer );

  inherited;
end;

procedure TGRenderer32.DrawPolyPoly( const Points : array of GClasses.TPointArray; closed : Boolean);
var
  idx, jdx : integer;
  polygon : TPolygon32;

  polyPolyPoints : GClasses.TPointArray;
  polyCounts : array of integer;
begin
  if ( closed and ( Brush.Style = gbsSolid )) or ( not closed and ( Pen.Style = gpsSolid ) and (Pen.Width = 1 )) then
  begin
    polygon := TPolygon32.Create;
    polygon.Antialiased := Antialiased;
    polygon.AntialiasMode := am4times; // use lowest/fastest level
    polygon.Closed := closed;

    try
      // copy over the points
      for jdx := 0 to High( Points ) do
        if High( Points[jdx] ) >= 1 then
        begin
          for idx := 0 to High( Points[jdx] ) do
            polygon.Add( FixedPoint(Points[jdx][idx]));
          polygon.NewLine;
        end;

      if not closed then
      begin
        polygon.DrawEdge(FBackBuffer, SetAlpha(Color32(Pen.Color), AlphaBlend));
        Exit;
      end;

      // Alpha blended fill.
      if Pen.Style = gpsSolid then
      begin
        polygon.Draw(FBackBuffer, SetAlpha(Color32(Pen.Color), AlphaBlend), SetAlpha(Color32(Brush.Color), AlphaBlend));
        exit;
      end;

      polygon.DrawFill(FBackBuffer, SetAlpha(Color32(Brush.Color), AlphaBlend));
    finally
      FreeAndNil( polygon );
    end;
  end;

  // Prepare to use window to render
  if ( Pen.Style <> gpsClear ) or ( closed and ( Brush.Style <> gbsSolid )) then
  begin
    // Use windows to draw lines so that we can have wide and dashed lines
    jdx := 0;
    SetLength( polyCounts, Length( Points ));
    for idx := 0 to High( Points ) do
    begin
      polyCounts[idx] := Length( Points[idx] );
      SetLength( polyPolyPoints, jdx + polyCounts[idx] );
      Move( Points[idx][0], polyPolyPoints[jdx], polyCounts[idx] * SizeOf( TPoint ));
      Inc( jdx, polyCounts[idx] );
    end;
  end;

  if closed and ( Brush.Style <> gbsSolid ) then
  begin
    RendererState([csPenValid, csBrushValid]);
    Windows.PolyPolygon(Canvas.Handle, polyPolyPoints[0], polyCounts[0], Length( polyCounts ));
  end
  else
    if Pen.Style <> gpsClear then
    begin
      RendererState([csPenValid]);
      Windows.PolyPolyline(Canvas.Handle, polyPolyPoints[0], polyCounts[0], Length( polyCounts ));
    end;
end;

procedure TGRenderer32.DrawPixel(X, Y: integer; color: TColor; alpha: Byte);
begin
  if FBackBuffer <> nil then
    FBackBuffer.SetPixelTS(X,Y, SetAlpha( Color32(color), alpha));
end;

procedure TGRenderer32.DrawScanLine(Y, x1, x2 : integer; u1, u2, v1, v2 : Double; const image: TGBitmap);
var
  tmp, du, dv : Extended;
  Xwidth, jdx, srcMax : integer;
  srcColor, transparentColor : TGColor;
  alpha : Byte;
begin
  if x2 < x1 then
  begin // swap coordinates so we can draw left-to-right
    jdx := x1;  x1 := x2;  x2 := jdx;
    tmp := u1;  u1 := u2;  u2 := tmp;
    tmp := v1;  v1 := v2;  v2 := tmp;
  end;

  Xwidth := x2 - x1;
  if (Xwidth <= 0) or (x2 < 0) or (x1 > Width) then
    Exit;

  // calc interpolants
  du := (u2 - u1) / Xwidth;
  dv := (v2 - v1) / Xwidth;

  // clamp destination
  if x1 < 0 then
  begin
    v1 := v1 - x1 * dv;
    u1 := u1 - x1 * du;
    x1 := 0;
  end;
  XWidth := FBackBuffer.Width;
  if x2 > XWidth then
    x2 := XWidth;

  Inc( x1, Y * XWidth );
  Inc( x2, Y * XWidth );

  XWidth := image.Width;
  srcMax := XWidth * image.Height;

  alpha := image.AlphaBlend;
  if image.Transparent then
    transparentColor := ToGColor( image.TransparentColor ) and $00FFFFFF // strip off the Alpha channel
  else
    transparentColor := $FFFFFFFF;

  // draw the scanline
  while x1 < x2 do
  begin
    if v1 < 0 then  v1 := 0;
    if u1 < 0 then  u1 := 0;

    jdx := Trunc(u1) + Trunc(v1) * XWidth;
    if ( jdx < srcMax ) and ( jdx >= 0 ) then
    begin
//      srcColor := image.PixelF[u1, v1 * XWidth];
      srcColor := image.Bits[jdx];
      if srcColor <> transparentColor then
      begin
        if alpha < 255 then
          ColorBlend( TGColor(FBackBuffer.Bits[x1]), srcColor, alpha )
        else
          FBackBuffer.Bits[x1] := srcColor;
      end;
    end;

    v1 := v1 + dv;
    u1 := u1 + du;
    Inc( x1 );
  end;
end;

procedure TGRenderer32.DrawTextRect( const aRect : TRect; const text : String );
var
  poly : TPolygon32;
  iBKMode : integer;
  tmpRect : TRect;
begin
  RendererState([csFontValid]);

  if gfsOpaque in Font.Style then
  begin
    poly := TPolygon32.Create;
    poly.Antialiased := Antialiased;

    poly.Add( FixedPoint(aRect.Left - 1,aRect.Top - 1));
    poly.Add( FixedPoint(aRect.Right,aRect.Top - 1));
    poly.Add( FixedPoint(aRect.Right,aRect.Bottom));
    poly.Add( FixedPoint(aRect.Left - 1,aRect.Bottom));
    poly.Draw(FBackBuffer, SetAlpha( Color32( Font.BkColor ), Font.BkAlphaBlend ), SetAlpha(Color32(Font.BkColor), Font.BkAlphaBlend));
    poly.Free;
  end;

  tmpRect := aRect;
  if gfsHalo in Font.Style then
  begin
    BeginPath( Canvas.Handle );
    Windows.DrawText( Canvas.Handle, PChar(text), Length(text), tmpRect, DT_NOCLIP + DT_NOPREFIX );
    EndPath( Canvas.Handle );

    Pen.Define( Font.BkColor, gpsSolid, 3, guPixel);

    RendererState([csPenValid]);
    StrokePath( Canvas.Handle );
  end;

  iBKMode := SetBkMode( Canvas.Handle, TRANSPARENT);

  Windows.DrawText(Canvas.Handle, PChar(text), Length(text), tmpRect, DT_NOCLIP + DT_NOPREFIX);

  if iBKMode <> TRANSPARENT then
    SetBkMode( Canvas.Handle, iBKMode);
end;

function TGRenderer32.GetCanvas: TCanvas;
begin
  if DoubleBuffer and ( FBackBuffer <> nil ) then
    Result := FBackBuffer.Canvas
  else
    Result := TGlobe5(ParentGlobe).Canvas;
end;

procedure TGRenderer32.RefreshDC;
begin
  inherited;

  if FBackBuffer <> nil then
  begin
    FBackBuffer.Canvas.Refresh;

    SetTextAlign(FBackBuffer.Canvas.Handle,TA_TOP or TA_LEFT);
    SetBkMode(FBackBuffer.Canvas.Handle,TRANSPARENT);
  end;
end;

procedure TGRenderer32.SetSize(newWidth, newHeight: integer);
begin
  if FBackBuffer <> nil then
    FBackBuffer.SetSize( newWidth, newHeight );

  inherited;
end;

procedure TGRenderer32.LoadFromXML(Element: TGXML_Element);
begin
  inherited;

  if Element <> nil then
    Antialiased := Element.Attribute( 'AntiAlias', false );
end;

procedure TGRenderer32.SaveToXML(Element: TGXML_Element);
begin
  inherited;
  
  Element.AddAttribute( 'AntiAlias', Antialiased, false );
end;

end.
