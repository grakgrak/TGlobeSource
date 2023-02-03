//-----------------------------------------------------------------------
//Summary
//TGlobe Default Presenter Classes
//
//Description
//Provides the basic Presenters for TGlobe map objects.
//
//Author
//Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------

{$I GLOBE5.INC}
unit GPresenters;

interface

Uses Windows, Classes, Forms, Graphics, SysUtils, 
  GClasses, GMapObjects, GSysUtils, GBitmap, GXML, GResource;

type
  TGPolyRenderMode = ( prmNormal, prmClosed, prmOpen );


  TGPointPresenterType = (ppNone, ppFontSymbol, ppImage,
    ppDot, ppCircle, ppSquare, ppDiamond,
    ppTriangleUp, ppTriangleDown, ppTriangleLeft, ppTriangleRight);

  TGPointPresenter = class(TGPresenter)
  private
    FPointPen : TGPen;
    FPointBrush : TGBrush;
    FPointFont : TGFont;
    FiSymbolIndex : SmallInt;
    FiSymbolOverride : integer;
    FPointType : TGPointPresenterType;
    FPointUnit : TGUnitTypes;
    FiPointSize : integer;
    FiMaxPointSize : integer;
    FImage : TGBitmap;
    FsImageName : TFilename;
    FOverrideColumn: integer;
    FToleranceAdjust : integer;
    procedure SetPointSize(Value : integer);
    procedure SetPointUnit(Value : TGUnitTypes);
    procedure SetImageName(const sName : TFilename);
    procedure SetMaxPointSize(Value : integer);
    function GetImage: TGBitmap;
  protected
    procedure AdjustTolerance( var tolerance : integer ); override;
    procedure BeginLayerRender( aLayer : TGLayer ); override;
    procedure RenderPoint( X, Y, iSize : integer; State : TGRenderStateSet ); virtual;
  public
    constructor Create( parentGlobe: TGCustomGlobe5 ); override;
    destructor Destroy; override;

    function Equals( aPresenter : TGPresenter ) : Boolean; override;

    procedure RenderLegend( Canvas : TCanvas; rect : TRect ); override;
    procedure RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet); override;

    procedure LoadFromXML( Element : TGXML_Element ); override;
    procedure SaveToXML( Element : TGXML_Element ); override;

    function LLInObject( ptLL : TGPointLL; mapObj : IGMapPoint; iTolerance : integer ) : Boolean; override;
  published
    property OverrideColumn : integer read FOverrideColumn write FOverrideColumn;

    property PointType : TGPointPresenterType read FPointType write FPointType;
    property PointUnit : TGUnitTypes read FPointUnit write SetPointUnit;
    property PointSize : integer read FiPointSize write SetPointSize;
    property SymbolIndex : SmallInt read FiSymbolIndex write FiSymbolIndex;
    property Image : TGBitmap read GetImage;
    property ImageName : TFilename read FsImageName write SetImageName;
    property MaxPointSize : integer read FiMaxPointSize write SetMaxPointSize;

    property PointPen : TGPen read FPointPen write FPointPen;
    property PointBrush : TGBrush read FPointBrush write FPointBrush;
    property PointFont : TGFont read FPointFont write FPointFont;
  end;


  TGPolyPresenter = class(TGPresenter)
  private
    FPolyPen : TGPen;
    FPolyBrush : TGBrush;
    FPolyRenderMode: TGPolyRenderMode;
  protected
    function IsClosed( mapObj : IGMapPoint ) : Boolean;
  public
    constructor Create( parentGlobe: TGCustomGlobe5 ); override;
    destructor Destroy; override;

    function Equals( aPresenter : TGPresenter ) : Boolean; override;

    function LLInObject(ptLL : TGPointLL; mapObj : IGMapPoint; iTolerance : integer) : Boolean; override;
    procedure RenderLegend( Canvas : TCanvas; rect : TRect ); override;
    procedure RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet); override;

    procedure LoadFromXML( Element : TGXML_Element ); override;
    procedure SaveToXML( Element : TGXML_Element ); override;
  published
    property PolyPen : TGPen read FPolyPen write FPolyPen;
    property PolyBrush : TGBrush read FPolyBrush write FPolyBrush;
    property PolyRenderMode : TGPolyRenderMode read FPolyRenderMode write FPolyRenderMode;
  end;


  TGPolygonPresenter = class(TGPolyPresenter)
  public
    constructor Create( parentGlobe: TGCustomGlobe5 ); override;
  end;


  TGPolylinePresenter = class(TGPolyPresenter)
  public
    constructor Create( parentGlobe: TGCustomGlobe5 ); override;
  end;

implementation

Uses Globe5;

procedure TGPointPresenter.AdjustTolerance( var tolerance : integer );
begin
  if FToleranceAdjust > tolerance then
    tolerance := FToleranceAdjust;
end;


procedure TGPointPresenter.BeginLayerRender(aLayer: TGLayer);
begin
  inherited;

  // Slightly larger that the point size to ease Detection and Selection
  FToleranceAdjust := TGlobe5(ParentGlobe).GlobeUnitsFrom( PointSize + 1, PointUnit ) div 2;

  if MaxPointSize <> 0 then
    FToleranceAdjust := MinVal( FToleranceAdjust, Round( MaxPointSize / ParentGlobe.Projector.Scalefactor ));
end;


constructor TGPointPresenter.Create( parentGlobe: TGCustomGlobe5 );
begin
  inherited;

  Name := rsPointPresenterName;

  FPointPen := TGPen.Create;
  FPointBrush := TGBrush.Create;
  FPointFont := TGFont.Create;
  FPointFont.Name := 'WingDings';

  FPointType := ppCircle;
  FPointUnit := guPixel;
  FiPointSize := 6;
  FiMaxPointSize := 0;
  FiSymbolOverride := -1;
  FOverrideColumn := -1;

  SetImageName( '' );
end;


destructor TGPointPresenter.Destroy;
begin
  if FsImageName <> '' then
    FreeAndNil( FImage );

  FreeAndNil( FPointPen );
  FreeAndNil( FPointBrush );
  FreeAndNil( FPointFont );

  inherited Destroy;
end;

function TGPointPresenter.Equals( aPresenter : TGPresenter ) : Boolean;
begin
  Result := aPresenter is TGPointPresenter;

  if not Result then
    Exit;

  Result := inherited Equals( aPresenter );

  if Result then
    with TGPointPresenter( aPresenter ) do
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


function TGPointPresenter.GetImage: TGBitmap;
var
  aLayer : TGLayer;
begin
  if FImage = nil then
    FImage := TGBitmap.Create;

  if FImage.Empty and ( FsImageName <> '' ) then
  begin
    // Find the layer that holds this presenter
    aLayer := TGlobe5( ParentGlobe ).GlobeLayer.LayerStore.LayerFromPresenter(Self);
//    aLayer := TGlobe5( ParentGlobe ).LayerStore.LayerFromPresenter(Self);
    if aLayer <> nil then
      aLayer.LoadImage( FImage, FsImageName )
    else
      TGlobe5( ParentGlobe ).LoadImage( FImage, FsImageName );
  end;

  Result := FImage;
end;

procedure TGPointPresenter.SetPointUnit( Value : TGUnitTypes );
begin
  if Value <> FPointUnit then
  begin
    FPointUnit := Value;
    if PointType = ppFontSymbol then
    begin
      PointFont.SizeUnit := Value;
      PointFont.Modified := True;
    end;

//ToDo:    Layer.Notify( osnRedraw, nil );
  end;
end;


procedure TGPointPresenter.SetMaxPointSize( Value : integer );
begin
  if Value <> FiMaxPointSize then
  begin
    FiMaxPointSize := Value;
//ToDo:    Layer.Notify( osnRedraw, nil );
  end;
end;


procedure TGPointPresenter.SetPointSize( Value : integer );
begin
  if Value <> FiPointSize then
  begin
    FiPointSize := Value;
    if PointType = ppFontSymbol then
    begin
      PointFont.Size := Value;
      PointFont.Modified := True;
    end;

//ToDo:    Layer.Notify( osnRedraw, nil );
  end;
end;


procedure TGPointPresenter.SetImageName( const sName : TFilename );
begin
  FsImageName := sName;

  FreeAndNil( FImage );
end;


function TGPointPresenter.LLInObject( ptLL : TGPointLL; mapObj : IGMapPoint; iTolerance : integer ) : Boolean;
begin
  with mapObj.Centroid do
    Result := MER_ContainsLL( MER( iLongX - FToleranceAdjust, iLatY - FToleranceAdjust, FToleranceAdjust * 2, FToleranceAdjust * 2 ), ptLL);
end;


procedure TGPointPresenter.RenderLegend(Canvas: TCanvas; rect : TRect);
var
  iSize : integer;
begin
  inherited;

  SaveDC( Canvas.Handle );

  Canvas.Refresh;
//  Canvas.Pen.Assign( PointPen );
//  Canvas.Brush.Assign( PointBrush );

  Renderer := ParentGlobe.Renderer.Clone;
  Renderer.DoubleBuffer := false;
  Renderer.Canvas.Handle := Canvas.Handle;
  Renderer.Brush.Assign(PointBrush);
  Renderer.Pen.Assign(PointPen);
  Renderer.Font.Assign(PointFont);

  iSize := MinVal( rect.Right - rect.Left, rect.Bottom - rect.Top );
  RenderPoint( Rect.Left + iSize div 2, rect.Top + iSize div 2, iSize, [] );

  Renderer.Free;
  Renderer := nil;

  RestoreDC( Canvas.Handle, -1 );
  Canvas.Refresh;
end;


procedure TGPointPresenter.RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet );
var
  ptXY : TPoint;
  iMaxHeight, iSize, iChain, idx : integer;
  bPointRendered : Boolean;
begin
  iSize := TGlobe5(ParentGlobe).ScaleUnitsToDevice( PointSize, PointUnit );

  if MaxPointSize <> 0 then
    iSize := MinVal( iSize, Round( MaxPointSize * Renderer.PixelsPerInch / Screen.PixelsPerInch));

  // Override the MaxText height for PointFonts
  iMaxHeight := Renderer.MaxTextHeight;
  Renderer.MaxTextHeight := 0;

  case PointType of
    ppFontSymbol : { display a font symbol }
    begin
      if OverrideColumn >= 0 then
        FiSymbolOverride := StrToIntDef(mapObj.Attribute[OverrideColumn], SymbolIndex)
      else
        FiSymbolOverride := -1;

      PointFont.Size := PointSize;
      PointFont.SizeUnit := PointUnit;
      Renderer.Font := PointFont;
      Renderer.AlphaBlend := PointFont.BkAlphaBlend;

      if rsSelected in State then
        Renderer.Font.Color := Renderer.SelectedPen.Color;
    end
    else
    begin
      if rsSelected in State then
      begin
        Renderer.Brush := Renderer.SelectedBrush;
        Renderer.Pen := Renderer.SelectedPen;
      end
      else
      begin
        Renderer.AlphaBlend := AdjustedAlphaBlend;
        Renderer.Pen := PointPen;
        Renderer.Brush := PointBrush;
      end;
    end;
  end;

  bPointRendered := false;
  if Supports( mapObj, IGMapPoly ) then
    with mapObj as IGMapPoly do
      for iChain := 0 to Chains.Count - 1 do
        with Chains[iChain] do
          if not Hidden then
            for idx := 0 to Count - 1 do
              if ParentGlobe.Projector.PointLLToXY( AsLL[idx], ptXY ) then
              begin
                RenderPoint( ptXY.X, ptXY.Y, iSize, State );
                bPointRendered := true;
              end;

  if not bPointRendered then
    if ParentGlobe.Projector.PointLLToXY( mapObj.Centroid, ptXY ) then
      RenderPoint( ptXY.X, ptXY.Y, iSize, State );

  FiSymbolOverride := -1;
  Renderer.MaxTextHeight := iMaxHeight;
end;


procedure TGPointPresenter.RenderPoint( X, Y, iSize : integer; State : TGRenderStateSet );
var
  iTmp, tmpX, tmpY : integer;
  sTmp : String;
  srcTri, destTri : TGTriangle;
begin
  iSize := iSize div 2;
  if iSize <= 0 then
    Exit;

  Renderer.AlphaBlend := AdjustedAlphaBlend;

  case PointType of
    ppFontSymbol : { display a font symbol }
      begin
        Renderer.Font.Size := iSize * 2;
        Renderer.Font.SizeUnit := guPixel;

        if FiSymbolOverride >= 0 then
          iTmp := FiSymbolOverride
        else
          iTmp := FiSymbolIndex;

        sTmp := Chr( iTmp );

        Renderer.DrawText( X, Y, sTmp, taCenter );
      end;
    ppImage : { display a Bitmap or Icon }
      if ( Image <> nil ) and not Image.Empty then
        with Image do
        begin
          tmpX := Round( Width * ( iSize / Width ));
          tmpY := Round( Height * ( iSize / Height ));

          AlphaBlend := Self.AdjustedAlphaBlend;
          if TransparentColor = clNone then
            TransparentColor := Image.Pixel[0,Height - 1];

          srcTri := GTriangle( 0, 0, Width, 0, 0, Height );
          destTri := GTriangle( X - tmpX, Y - tmpY, X + tmpX, Y - tmpY, X - tmpX, Y + tmpY);
          Renderer.DrawTextureTriangle( srcTri, destTri, Image );

          srcTri[0] := Point( Width, Height);
          destTri[0] := Point( X + tmpX, Y + tmpY );
          Renderer.DrawTextureTriangle( srcTri, destTri, Image );

          if rsSelected in State then
            Renderer.DrawPoly([
              Point( X - iSize, Y - iSize ), Point( X - iSize, Y + iSize -1 ),
              Point( X + iSize - 1, Y + iSize - 1 ), Point( X + iSize - 1, Y - iSize ),
              Point( X - iSize, Y - iSize )], False );
        end;
    ppDot : { display a single pixel }
      begin
        Renderer.DrawPixel(X,Y, PointPen.Color, AdjustedAlphaBlend );
      end;
    ppCircle : { display a circle }
      begin
        Renderer.DrawEllipse(Rect( X - iSize, Y - iSize, X + iSize, Y + iSize ));
      end;
    ppTriangleUp : { display an upwards pointing triangle }
      begin
        Renderer.DrawPoly([ Point( X + iSize, Y + iSize ), Point( X, Y - iSize ),
          Point( X - iSize, Y + iSize )], True );
      end;
    ppTriangleDown : { display an downwards pointing triangle }
      begin
        Renderer.DrawPoly([ Point( X - iSize, Y - iSize ), Point( X, Y + iSize ),
          Point( X + iSize, Y - iSize )], True );
      end;
    ppTriangleRight : { display an rightwards pointing triangle }
      begin
        Renderer.DrawPoly([ Point( X - iSize, Y - iSize ), Point( X + iSize, Y ),
          Point( X - iSize, Y + iSize )], True );
      end;
    ppTriangleLeft : { display an leftwards pointing triangle }
      begin
        Renderer.DrawPoly([ Point( X + iSize, Y + iSize ),
          Point( X - iSize, Y ), Point( X + iSize, Y - iSize )], True );
      end;
    ppSquare : { display a square }
      begin
        Renderer.DrawPoly([ Point( X - iSize, Y - iSize ),
          Point( X - iSize, Y + iSize ), Point( X + iSize, Y + iSize ),
          Point( X + iSize, Y - iSize )], True );
      end;
    ppDiamond : { display a diamond }
      begin
        Renderer.DrawPoly([ Point( X, Y - iSize ), Point( X + iSize, Y ),
          Point( X, Y + iSize ), Point( X - iSize, Y )], True );
      end;
  end;
end;


procedure TGPointPresenter.LoadFromXML( Element : TGXML_Element );
begin
  inherited LoadFromXML( Element );

  if Element <> nil then
    with Element do
    begin
      FPointType := TGPointPresenterType( Attribute( 'Type', Ord( ppCircle )));
      FPointUnit := StrToUnits( Attribute( 'Unit', UnitsToStr( guPixel )));
      FiPointSize := Attribute( 'Size', 6 );
      FiMaxPointSize := Attribute( 'MaxSize', 0 );
      FiSymbolIndex := Attribute( 'SymbolIndex', 0 );
      ImageName := Attribute( 'ImageName', '' );
      FOverrideColumn := Attribute( 'OverrideCol', -1 );

      PointPen.LoadFromXML( Element( 'Pen', 0 ));
      PointBrush.LoadFromXML( Element( 'Brush', 0 ));
      PointFont.LoadFromXML( Element( 'Font', 0 ));
    end;
end;


procedure TGPointPresenter.SaveToXML( Element : TGXML_Element );
begin
  inherited SaveToXML( Element );

  Element.AddAttribute( 'Type', Ord( PointType ), Ord( ppCircle ));
  Element.AddAttribute( 'Unit', UnitsToStr( PointUnit ), UnitsToStr( guPixel ));
  Element.AddAttribute( 'Size', PointSize, 6 );
  Element.AddAttribute( 'MaxSize', MaxPointSize, 0 );
  Element.AddAttribute( 'SymbolIndex', SymbolIndex, 0 );
  Element.AddAttribute( 'ImageName', ImageName, '' );
  Element.AddAttribute( 'OverrideCol', OverrideColumn, -1 );

  PointPen.SaveToXML( Element.AddElement( 'Pen' ));
  PointBrush.SaveToXML( Element.AddElement( 'Brush' ));
  PointFont.SaveToXML( Element.AddElement( 'Font' ));
end;


constructor TGPolyPresenter.Create( parentGlobe: TGCustomGlobe5 );
begin
  inherited;

  Name := rsPolyPresenterName;

  FPolyPen := TGPen.Create;
  FPolyBrush := TGBrush.Create;
  FPolyBrush.Color := clWhite;
  FPolyBrush.BkColor := clWhite;
end;


destructor TGPolyPresenter.Destroy;
begin
  FreeAndNil( FPolyPen );
  FreeAndNil( FPolyBrush );

  inherited Destroy;
end;


function TGPolyPresenter.Equals( aPresenter : TGPresenter ) : Boolean;
begin
  Result := aPresenter.InheritsFrom( TGPolyPresenter );

  if not Result then
    Exit;

  Result := inherited Equals( aPresenter );

  if Result then
    with TGPolyPresenter( aPresenter ) do
      Result := ( PolyRenderMode = Self.PolyRenderMode )
            and PolyPen.Equals( Self.PolyPen )
            and PolyBrush.Equals( Self.PolyBrush );
end;

function TGPolyPresenter.IsClosed( mapObj : IGMapPoint ) : Boolean;
begin
  case PolyRenderMode of
    prmClosed: Result := true;
    prmOpen: Result := false
    else
      if mapObj <> nil then
        Result := (mapObj as IGMapPoly).Closed
      else
        Result := True;
  end;
end;


function TGPolyPresenter.LLInObject(ptLL : TGPointLL; mapObj : IGMapPoint; iTolerance : integer) : Boolean;
begin
  if not Supports( mapObj, IGMapPoly ) then
    Result := false
  else
    if IsClosed(mapObj) then
      Result := (mapObj as IGMapPoly).LLInPolygon(ptLL, iTolerance)
    else
      Result := (mapObj as IGMapPoly).LLOnEdge(ptLL, iTolerance, (mapObj as IGMapPoly).Closed);
end;


procedure TGPolyPresenter.LoadFromXML( Element : TGXML_Element );
begin
  inherited LoadFromXML( Element );

  if Element <> nil then
    with Element do
    begin
      PolyRenderMode := TGPolyRenderMode( Attribute( 'RenderMode', Ord(prmNormal)));
      PolyPen.LoadFromXML( Element( 'Pen', 0 ));
      PolyBrush.LoadFromXML( Element( 'Brush', 0 ));
    end;
end;


procedure TGPolyPresenter.RenderLegend( Canvas : TCanvas; rect : TRect );
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
      Canvas.Polyline([Point(Left,Top), Point(Left, Bottom), Point(Right, Top), Point(Right,Bottom)]);

  RestoreDC( Canvas.Handle, -1 );
  Canvas.Refresh;
end;

procedure TGPolyPresenter.RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet );
begin
  if Supports( mapObj, IGMapPoly ) then
  begin
    DoBeginRender( mapObj );

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
    end;

    TGlobe5(ParentGlobe).RenderChainStore((mapObj as IGMapPoly).Chains, State );

    DoEndRender( mapObj );
  end;
end;

procedure TGPolyPresenter.SaveToXML( Element : TGXML_Element );
begin
  inherited SaveToXML( Element );

  Element.AddAttribute( 'RenderMode', Ord(PolyRenderMode), Ord(prmNormal));
  PolyPen.SaveToXML( Element.AddElement( 'Pen' ));
  PolyBrush.SaveToXML( Element.AddElement( 'Brush' ));
end;


constructor TGPolygonPresenter.Create( parentGlobe: TGCustomGlobe5 );
begin
  inherited;

  Name := rsPolygonPresenterName;
  FPolyRenderMode := prmClosed;
end;


constructor TGPolylinePresenter.Create( parentGlobe: TGCustomGlobe5 );
begin
  inherited;
  Name := rsPolylinePresenterName;
  FPolyRenderMode := prmOpen;
end;

initialization
  RegisterGlobeClass( TGPointPresenter, 'Point Presenter' );
  RegisterGlobeClass( TGPolyPresenter, 'Poly Presenter' );
  RegisterGlobeClass( TGPolygonPresenter, 'Polygon Presenter' );
  RegisterGlobeClass( TGPolylinePresenter, 'Polyline Presenter' );
end.
