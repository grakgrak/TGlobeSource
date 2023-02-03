//-----------------------------------------------------------------------
//Summary
//TGlobe Default renderer class
//
//Description
//Provides a default renderer that renders to the windows device context.
//
//Author
//Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
{$R-}
unit GRenderer;

interface

Uses  Windows, Classes, Graphics, SysUtils, GClasses, GSysUtils, GBitmap ;

type
  //------------------------------------------------------------------------
  //Description
  //This is an implementation of the TGRenderer class.
  //
  //This renderer renders objects using the standard TCanvas and windows GDI
  //routines.
  //
  //This is the default renderer created when the TGlobe component is first
  //created.                                                                
  //------------------------------------------------------------------------
  TGDefaultRenderer = class(TGRenderer)
  private
    FBackBuffer : TGBitmap;
  protected
    function GetCanvas: TCanvas; override;
    procedure DrawPolyPoly( const Points : array of TPointArray; closed: Boolean); override;
  public
    constructor Create(ParentGlobe : TGCustomGlobe5); override;
    destructor Destroy; override;

    procedure RefreshDC; override;
    procedure SetSize( newWidth, newHeight : integer ); override;

    procedure DrawPixel( X, Y : integer; color: TColor; alpha : Byte ); override;
    procedure DrawTextRect( const aRect : TRect; const text : String ); override;

    procedure DrawScanLine(Y, x1, x2: integer; u1, u2, v1, v2: Double; const image: TGBitmap); override;
  end;

implementation

Uses Globe5;

constructor TGDefaultRenderer.Create(ParentGlobe: TGCustomGlobe5);
begin
  inherited;
  FBackBuffer := TGBitmap.Create;
end;

destructor TGDefaultRenderer.Destroy;
begin
  FBackBuffer.Canvas.Refresh;

  FreeAndNil( FBackBuffer );

  inherited;
end;

procedure TGDefaultRenderer.DrawPixel(X, Y: integer; color: TColor; alpha: Byte);
begin
  if FBackBuffer <> nil then
    FBackBuffer.SetPixelA(X, Y, color, alpha);
end;

procedure TGDefaultRenderer.DrawPolyPoly( const Points: array of TPointArray;
  closed: Boolean);
var
  total, idx : integer;
  polyPolyPoints : TPointArray;
  polyCounts : array of integer;
begin
  total := 0;
  SetLength( polyCounts, Length( Points ));
  for idx := 0 to High( Points ) do
  begin
    polyCounts[idx] := Length( Points[idx] );
    SetLength( polyPolyPoints, total + polyCounts[idx] );
    Move( Points[idx][0], polyPolyPoints[total], polyCounts[idx] * SizeOf( TPoint ));
    Inc( total, polyCounts[idx] );
  end;

  if closed then
  begin
    RendererState([csPenValid, csBrushValid]);
    Windows.PolyPolygon(Canvas.Handle, polyPolyPoints[0], polyCounts[0], Length( polyCounts ));
  end
  else
  begin
    RendererState([csPenValid]);
    Windows.PolyPolyline(Canvas.Handle, polyPolyPoints[0], polyCounts[0], Length( polyCounts ));
  end;
end;

procedure TGDefaultRenderer.DrawScanLine(Y, x1, x2: integer; u1, u2, v1, v2: Double;
  const image: TGBitmap);
var
  du, dv, tmp, srcW, srcH : Single;
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

  srcH := image.Height;
  srcW := image.Width;

  alpha := image.AlphaBlend;
  if image.Transparent then
    transparentColor := image.TransparentColor and $00FFFFFF // strip off the Alpha channel
  else
    transparentColor := $FFFFFFFF;

  // draw the scanline
  if ( alpha = 255 ) and ( transparentColor = $FFFFFFFF ) then
    while x1 < x2 do
    begin
      if ( u1 < srcW ) and ( v1 < srcH ) and ( u1 >= 0.0 ) and ( v1 >= 0.0 ) then
        FBackBuffer.Bits[x1] := image.BilinearPixel( u1, v1 );

      v1 := v1 + dv;
      u1 := u1 + du;
      Inc( x1 );
    end
  else
    while x1 < x2 do
    begin
      if v1 < 0 then  v1 := 0;
      if u1 < 0 then  u1 := 0;

      jdx := Trunc(u1) + Trunc(v1) * XWidth;
      if ( jdx < srcMax ) and ( jdx >= 0 ) then
      begin
        srcColor := image.Bits[jdx];
        if srcColor <> transparentColor then
        begin
          if alpha < 255 then
            ColorBlend( FBackBuffer.Bits[x1], srcColor, alpha )
          else
            FBackBuffer.Bits[x1] := srcColor;
        end;
      end;

      v1 := v1 + dv;
      u1 := u1 + du;
      Inc( x1 );
    end;
end;

procedure TGDefaultRenderer.DrawTextRect( const aRect : TRect; const text : String );
var
  gdiObj, brush : HGDIOBJ;
  iBKMode : integer;
  tmpRect : TRect;
begin
  RendererState([csFontValid]);

  iBKMode := SetBkMode( Canvas.Handle, TRANSPARENT);

  if gfsOpaque in Font.Style then
    with aRect do
    begin
      brush := CreateSolidBrush(Font.BkColor);
      gdiObj := SelectObject(Canvas.Handle, brush);
      Windows.FillRect( Canvas.Handle, Rect(Left - 1, Top - 1, Right + 1, Bottom + 1), brush);
      DeleteObject( SelectObject(Canvas.Handle, gdiObj));
    end;

  tmpRect := aRect;
  if gfsHalo in Font.Style then
  begin
    BeginPath( Canvas.Handle );
    Windows.DrawText( Canvas.Handle, PChar(text), Length(text), tmpRect, DT_NOCLIP + DT_NOPREFIX );
    EndPath( Canvas.Handle );

    Pen.Define(Font.BkColor, gpsSolid, 3, guPixel);

    RendererState([csPenValid]);
    StrokePath( Canvas.Handle );
  end;

  Windows.DrawText(Canvas.Handle, PChar(text), Length(text), tmpRect, DT_NOCLIP + DT_NOPREFIX);

  if iBKMode <> TRANSPARENT then
    SetBkMode( Canvas.Handle, iBKMode);
end;

function TGDefaultRenderer.GetCanvas: TCanvas;
begin
  if DoubleBuffer and ( FBackBuffer <> nil ) then
    Result := FBackBuffer.Canvas
  else
    Result := TGlobe5(ParentGlobe).Canvas;
end;

procedure TGDefaultRenderer.RefreshDC;
begin
  inherited;

  if FBackBuffer <> nil then
  begin
    FBackBuffer.Canvas.Refresh;

    SetTextAlign(FBackBuffer.Canvas.Handle,TA_TOP or TA_LEFT);
    SetBkMode(FBackBuffer.Canvas.Handle,TRANSPARENT);
  end;
end;

procedure TGDefaultRenderer.SetSize(newWidth, newHeight: integer);
begin
  FBackBuffer.SetSize( newWidth, newHeight );

  inherited;
end;

end.

