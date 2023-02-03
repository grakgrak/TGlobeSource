//-----------------------------------------------------------------------
//Summary
//TGlobe Component
//
//Description
//The main Globe component used to render the map to the screen.
//
//Author
//Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------

{$I GLOBE5.INC}
{$J+}
unit Globe5;

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, StdCtrls,
  Printers, Forms, Dialogs,
{$IFDEF TG_PROPERTY_EDITORS}
{$IFDEF TG_DELPHI6}
  DesignIntf,
  DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
{$ENDIF}
  GLabelRenderer, GGraticule,
  GSysUtils, GClasses, GXML, GRenderer, GBitmap, GSpatialDatabase, GImageTileCache;

const
  TGLOBE_VERSION = '5.1.2';
  GLOBAL_POINT_PRESENTER = -1;
  GLOBAL_POLY_PRESENTER = -2;
//---------------------------- Types and Classes -------------------------------
type
  TGlobe5 = class;

  TGlobeNotification = ( gnDestroying, gnPaint, gnRender, gnLayerTreeUpdated );

  IGlobeSubscriber = Interface
  ['{93118164-923E-4FDE-BEC0-FE51FA73CF6E}']
    procedure GlobeNotification( Sender : TGlobe5; notification : TGlobeNotification );
  end;

  TGlobeOptions = (goMultiSelect, goConstantScaleOnResize, goFixedCenterXYOnResize, goTransparentBackground);
  TGlobeOptionsSet = set of TGlobeOptions;


  TGGreatCircle = (gcShortest, gcLongest, gcCircle);
  TGMouseButton = (mzbLeft, mzbRight, mzbMiddle, mzbNone);

  TGLoadImageEvent = procedure(Sender : TGlobe5; const image : TGBitmap; const sourceName : String; var done : Boolean) of object;
  TGMissingImageEvent = procedure(Sender : TGlobe5; var sourceName : String; var retry : Boolean) of object;
  TGMissingLayerEvent = procedure(Sender : TGlobe5; const layerName, sourceName : String; userElement : TGXML_Element; var retry : Boolean) of object;
  TGLayerRenderEvent = procedure(Sender : TGlobe5; GLayer : TGLayer) of object;
  TGObjectRenderEvent = procedure(Sender : TGlobe5; GLayer : TGLayer; mapObj : IGMapPoint; State : TGRenderStateSet) of object;
  TGObjectNotifyEvent = procedure(Sender : TGlobe5; GObject : IGMapPoint) of object;
  TGLayerNotifyEvent = procedure(Sender : TGlobe5; GLayer : TGLayer) of object;
  TGEnvironmentEvent = procedure(Sender : TGlobe5; Document : TGXML_Document) of object;


  TGlobe5 = class(TGCustomGlobe5, IGSDBObserver)
  private
    FGlobeOptions : TGlobeOptionsSet;
    FBackgroundImage : TBitmap;
    FBackgroundColor : TColor;

    FMouseZoomButton : TGMouseButton;
    FMouseSelectButton : TGMouseButton;
    FMouseRotateButton : TGMouseButton;

    FOnPaintGraticule : TNotifyEvent;
    FOnPaint : TNotifyEvent;
    FOnProgress: TGProgressEvent;
    FOnMouseEnter : TNotifyEvent;
    FOnMouseLeave : TNotifyEvent;

    FOnObjectRender : TGObjectRenderEvent;
    FOnObjectClick : TGObjectNotifyEvent;

    FOnSaveToENV : TGEnvironmentEvent;
    FOnLoadFromENV : TGEnvironmentEvent;

    FOnSelectionChange : TNotifyEvent;
    FOnPanned : TNotifyEvent;
    FOnRotated : TNotifyEvent;
    FOnZoomed : TNotifyEvent;
    FOnRender : TNotifyEvent;

    FOnLoadImage: TGLoadImageEvent;
    FOnMissingImage: TGMissingImageEvent;
    FOnMissingLayer: TGMissingLayerEvent;
    FOnAfterLayerRender: TGLayerRenderEvent;
    FOnBeforeLayerRender: TGLayerRenderEvent;
    FOnBeforeRender: TNotifyEvent;
    FOnAfterRender: TNotifyEvent;
    FOnPeriodicUpdate: TNotifyEvent;

    FbRotating : Boolean;
    FbZooming : Boolean;
    FbDrawFocusRect : Boolean;
    FbPainting : Boolean;
    FbUpdateTreeRequired : Boolean;
    FFocusRect : TRect;
    FUpdateCount : integer;
    FPeriodicUpdateTime : integer;
    FRenderUpdateTime : DWord;
    FbRestartRender : Boolean;
    FbRenderAborted : Boolean;
    FbRenderToCanvas : Boolean;
    FbRedrawLayers : Boolean;

    FCullObjectPixels: Double;
    FMinObjectPixels: Double;
    FScrollBars : TScrollStyle;
    FEnvironmentFile : TFilename;
    FSurfaceBitmap : TGBitmap;
    FSurfaceTextureName : TFilename;

    FSpatialDatabase: TGSpatialDatabase;
    FGraticule: TGGraticule;
    FSelection : TGGlobeSelection;

    FCurrentPath : TGChainStore;
    FSurfaceColor: TColor;
    FSubscribers : TInterfaceList;
    FLabelRenderer: TGLabelRenderer;
    FGlobeLayer: TGLayer;
    FMinObjectColor: TColor;
    ptRotate : TGPointLL;
    FOnLayersChanged: TNotifyEvent;
    FImageTileCache: TGImageTileCache;

    procedure SetBackgroundBitmap(NewBitmap : TBitmap);
    procedure SetBackgroundColor(Value : TColor);
    procedure SetSpatialDatabase(const Value: TGSpatialDatabase);
    procedure SetSurfaceColor(const Value: TColor);
    procedure SetSurfaceTextureName(sName : TFilename);

    procedure CMSysColorChange(var Message : TMessage); message CM_SYSCOLORCHANGE;
    procedure WMHScroll(var Msg : TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg : TWMVScroll); message WM_VSCROLL;
    procedure WMEraseBkgnd(var Msg : TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;

    procedure OnSDBNotification( Sender: TGSpatialDatabase; notification : TGSDBNotification );
    procedure UpdateLayerTree;
    procedure RemoveOrphanedLayers( layerStore : TGLayerStore );
    procedure SetImageTileCache(const Value: TGImageTileCache);
  protected
    procedure CreateParams(var Params : TCreateParams); override;

    procedure SetCullObjectPixels(const Value: Double);
    procedure SetGlobeOptions(Value : TGlobeOptionsSet);
    procedure SetGraticule(const Value: TGGraticule);
    procedure SetMinObjectColor(const Value: TColor);
    procedure SetMinObjectPixels(const Value: Double);
    procedure SetProjector(const Value: TGProjector); override;
    procedure SetRenderer(const Value: TGRenderer); override;
    procedure SetScrollBars(Value : TScrollStyle);
    procedure SetViewRect(const sRect : TRect); override;

    procedure PaintWindowBackground;
    procedure PaintSurface;
    procedure PaintGraticule;
    procedure RenderLayers;
    procedure GlobePropertyChanged(Sender : TObject);

    function GetClientRect: TRect; override;
    function ResizeProjection(iWidth, iHeight : Integer) : Boolean;
    procedure DrawVectorMatrixLine(vec : TGVec3D; const mat : TGMatrix; iHeight, iSteps : integer);

    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GlobeNotify( notification : TGlobeNotification );
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure SaveToXML(Element : TGXML_Element);
    procedure LoadFromXML(Element : TGXML_Element);
    procedure SaveEnvironmentToFile(const sFilename : TFilename);
    procedure LoadEnvironmentFromFile(const sFilename : TFilename);

    function GlobeUnitsFrom(const eValue : Extended; Units : TGUnitTypes) : Integer;
    function GlobeUnitsTo(iValue : Integer; Units : TGUnitTypes) : Extended;
    function ScaleUnitsToDevice(Value : Integer; Units : TGUnitTypes) : Integer;


    function GetMetaFile(iWidth, iHeight : integer; bEnhanced : Boolean) : TMetafile;
    function GetMetaFileMM(iWidthMM, iHeightMM : integer; bEnhanced : Boolean) : TMetafile;
    function GetBitmap(iWidth, iHeight : integer) : TBitmap;


    procedure PanViewXY(iX, iY : Integer);
    procedure LocateToLL(ptLL : TGPointLL); overload;
    procedure LocateToLL(iLong, iLat : Integer); overload;
    procedure LocateToDD(const long: TGDecimalDegree; const lat: TGDecimalDegree );
    procedure LocateToObject(Obj : IGMapPoint);

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure BeginPath( chains : TGChainStore );
    procedure EndPath;

    procedure GetPOZ(var POZ : TGPOZ);
    procedure SetPOZ(const POZ : TGPOZ);

    procedure Clear;
    procedure AbortRender;
    procedure NotifyLayerTreeChanged;
    procedure PeriodicUpdateRender;
    procedure RestartRender;
    procedure RedrawLayers;

    function IsBusy : Boolean;
    function IsRenderRestarted : Boolean;
    function IsRenderAborted : Boolean;
    function RectToMER( rect : TRect ) : TGMER;
    function SelectObjects( aMER : TGMER; firstOnly, state : Boolean ) : integer;
    function ClearSelected : Boolean;


    function RenderShadow(GMTDateTime : TDateTime; ColorNight : TColor; TwilightDegrees : integer = 12; MaxAlpha : Byte = 160) : TGPointLL;

    procedure RenderChainStore(Chains : TGChainStore; State : TGRenderStateSet );
    procedure RenderCircle(const ptLL : TGPointLL; iRadius : integer; radiusUnit : TGUnitTypes; iSteps : integer);
    procedure RenderGreatCircleLine(const ptFromLL, ptToLL : TGPointLL; CircleType : TGGreatCircle);
    procedure RenderLine(const ptFromLL, ptToLL : TGPointLL; iSteps : Integer);
    procedure RenderPolygon(const Points : array of TGPointLL );
    procedure RenderPolyLine(const Points : array of TGPointLL);
    procedure RenderTextOut(const ptLL : TGPointLL; const sText : String);
    procedure RenderTextureTriangle(const srcTri: TGTriangle; const destTriLL: TGTriangleLL; const image : TGBitmap; selected : Boolean ); virtual;
    procedure RenderToCanvas(ACanvas : TCanvas; ARect : TRect; bAsImage : Boolean);

    procedure DoOnObjectRender( GLayer : TGLayer; mapObj : IGMapPoint; state : TGRenderStateSet );
    procedure DoOnAfterLayerRender( GLayer : TGLayer );
    procedure DoOnBeforeLayerRender( GLayer : TGLayer );
    procedure DoOnLoadImage( const image : TGBitmap; const imageName : string; var done : Boolean );
    procedure DoOnMissingImage( var sourceName : String; var retry : Boolean );
    procedure DoOnMissingLayer( const layerName, sourceName : String; userElement : TGXML_Element; var retry : Boolean );
    procedure DoOnPaint;
    procedure DoOnProgress( progressType : TGProgressType; count : integer; percentage : double );
    procedure DoOnRender;
    procedure DoOnAfterRender;
    procedure DoOnBeforeRender;
    procedure DoOnSelectionChange;
    procedure DoOnPeriodicUpdate;
    procedure DoOnPanned;
    procedure DoOnRotated;
    procedure DoOnZoomed;
    procedure DoOnLayersChanged;


    procedure BeginFocusRect( X, Y : integer );
    function EndFocusRect : TRect;
    function LoadImage( image : TGBitmap; var imageName : TFilename ) : Boolean;
    procedure Paint; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Subscribe( subscriber : IGlobeSubscriber );
    procedure UnSubscribe( subscriber : IGlobeSubscriber );
    procedure UpdateScrollBars;


    property GlobeLayer : TGLayer read FGlobeLayer;
    property LabelRenderer : TGLabelRenderer read FLabelRenderer write FLabelRenderer;
    property SurfaceBitmap : TGBitmap read FSurfaceBitmap;
    property Selection : TGGlobeSelection read FSelection;
    property ViewRect;
    property MouseCapture;
    property Canvas;

  published
    property Graticule : TGGraticule read FGraticule write SetGraticule;
    property SpatialDatabase : TGSpatialDatabase read FSpatialDatabase write SetSpatialDatabase;
    property ImageTileCache : TGImageTileCache read FImageTileCache write SetImageTileCache;

    property OnLayersChanged : TNotifyEvent read FOnLayersChanged write FOnLayersChanged;
    property OnLoadImage : TGLoadImageEvent read FOnLoadImage write FOnLoadImage;
    property OnMissingLayer : TGMissingLayerEvent read FOnMissingLayer write FOnMissingLayer;
    property OnMissingImage : TGMissingImageEvent read FOnMissingImage write FOnMissingImage;
    property OnPanned : TNotifyEvent read FOnPanned write FOnPanned;
    property OnPeriodicUpdate : TNotifyEvent read FOnPeriodicUpdate write FOnPeriodicUpdate;
    property OnProgress : TGProgressEvent read FOnProgress write FOnProgress;
    property OnRender : TNotifyEvent read FOnRender write FOnRender;
    property OnRotated : TNotifyEvent read FOnRotated write FOnRotated;
    property OnSelectionChange : TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnZoomed : TNotifyEvent read FOnZoomed write FOnZoomed;

    property OnPaintGraticule : TNotifyEvent read FOnPaintGraticule write FOnPaintGraticule;
    property OnPaint : TNotifyEvent read FOnPaint write FOnPaint;
    property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;

    property OnAfterRender : TNotifyEvent read FOnAfterRender write FOnAfterRender;
    property OnBeforeRender : TNotifyEvent read FOnBeforeRender write FOnBeforeRender;
    property OnAfterLayerRender : TGLayerRenderEvent read FOnAfterLayerRender write FOnAfterLayerRender;
    property OnBeforeLayerRender : TGLayerRenderEvent read FOnBeforeLayerRender write FOnBeforeLayerRender;

    property OnObjectRender : TGObjectRenderEvent read FOnObjectRender write FOnObjectRender;
    property OnObjectClick : TGObjectNotifyEvent read FOnObjectClick write FOnObjectClick;
    property OnSaveEnvironment : TGEnvironmentEvent read FOnSaveToENV write FOnSaveToENV;
    property OnLoadEnvironment : TGEnvironmentEvent read FOnLoadFromENV write FOnLoadFromENV;

    property BackgroundImage : TBitmap read FBackgroundImage write SetBackgroundBitmap;
    property Color : TColor read FBackgroundColor write SetBackgroundColor default clBlack;
    property CullObjectPixels : Double read FCullObjectPixels write SetCullObjectPixels;
    property GlobeOptions : TGlobeOptionsSet read FGlobeOptions write SetGlobeOptions;
    property MinObjectColor : TColor read FMinObjectColor write SetMinObjectColor;
    property MinObjectPixels : Double read FMinObjectPixels write SetMinObjectPixels;

    property MouseZoomButton : TGMouseButton read FMouseZoomButton write FMouseZoomButton default mzbRight;
    property MouseSelectButton : TGMouseButton read FMouseSelectButton write FMouseSelectButton default mzbLeft;
    property MouseRotateButton : TGMouseButton read FMouseRotateButton write FMouseRotateButton default mzbLeft;
    property PeriodicUpdateTime : Integer read FPeriodicUpdateTime write FPeriodicUpdateTime default 0;
    property EnvironmentFile : TFilename read FEnvironmentFile write LoadEnvironmentFromFile;
    property ScrollBars : TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property SurfaceTextureName : TFilename read FSurfaceTextureName write SetSurfaceTextureName;
    property SurfaceColor : TColor read FSurfaceColor write SetSurfaceColor;

{$IFDEF TG_DELPHI5_UP}
    property Anchors;
{$ENDIF}
    property Projector;
    property Renderer;

    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnCanResize;
    property OnConstrainedResize;
  end;

procedure Register;

{------------------------------------------------------------------------------}

implementation

{$R *.DCR}

uses GMapObjects, GPresenters, GSDBLayers, GSDBObjectSource, Globe5Utils;


const
  TGLOBE_COPYRIGHT = 'Copyright Graham Knight 1998-2005';
  TGLOBE_EMAIL = 'Email: tglobe@tglobe.com';
  TGLOBE_WWW = 'http://www.tglobe.com/';
  SHAREMSG = 'TGlobe ' + TGLOBE_VERSION + ' ' + TGLOBE_COPYRIGHT + ' ' + TGLOBE_EMAIL;

procedure DisplaySharewareMessage( Canvas : TCanvas );
begin
  with Canvas do
  begin
    SetBkMode( Handle, TRANSPARENT );
//    Brush.Color := clWhite;
//    Brush.Style := Graphics.bsSolid;
    Font.Name := 'Arial';
    Font.Size := 10;
    Font.Color := clDkGray;
    TextOut(1, 1, SHAREMSG);
    TextOut(1, 2, SHAREMSG);
    TextOut(1, 3, SHAREMSG);

    TextOut(2, 1, SHAREMSG);
    TextOut(2, 3, SHAREMSG);

    TextOut(3, 1, SHAREMSG);
    TextOut(3, 2, SHAREMSG);
    TextOut(3, 3, SHAREMSG);

    Font.Color := clWhite;
    TextOut(2, 2, SHAREMSG);
  end;
end;

procedure TGlobe5.AbortRender;
begin
  FbRestartRender := true;
  FbRenderAborted := true;
end;

procedure TGlobe5.BeginFocusRect( X, Y : integer );
begin
  FbDrawFocusRect := true;
  FFocusRect := Rect(X, Y, X, Y);
  MouseCapture := True;
end;

procedure TGlobe5.BeginPath(chains: TGChainStore);
begin
  if FCurrentPath <> nil then
    raise EGException.Create('BeginPath: Path is already open');

  if chains = nil then
    raise EGException.Create('BeginPath: chains parameter is nil' );

  FCurrentPath := chains;
end;

procedure TGlobe5.BeginUpdate;
begin
  Inc( FUpdateCount );
end;

procedure TGlobe5.Clear;
var
  aPresenter : TGPresenter;
begin
  GlobeLayer.Clear;

  // Add back the in 2 default presenters
  aPresenter := TGPointPresenter.Create(Self);

  aPresenter.PresenterID := GLOBAL_POINT_PRESENTER;
  TGPointPresenter( aPresenter ).PointUnit := guPixel;
  GlobeLayer.Presenters.Add(aPresenter);

  aPresenter := TGPolyPresenter.Create(Self);
  aPresenter.PresenterID := GLOBAL_POLY_PRESENTER;
  GlobeLayer.Presenters.Add(aPresenter);

  // reset the environment file as well
//  if EnvironmentFile <> '' then
//    SaveEnvironmentToFile(EnvironmentFile);
end;

function TGlobe5.ClearSelected: Boolean;
begin
  Result := GlobeLayer.ClearSelected;

  if Result then
    DoOnSelectionChange;
end;

procedure TGlobe5.CMMouseEnter(var Message : TMessage);
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TGlobe5.CMMouseLeave(var Message : TMessage);
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

procedure TGlobe5.CMSysColorChange(var Message : TMessage);
begin
  inherited;
  RedrawLayers;
end;

constructor TGlobe5.Create(AOwner : TComponent);
begin
  ControlStyle := ControlStyle + [csOpaque,csAcceptsControls];

  inherited Create(AOwner);

  TabStop := true;
  Width := 150;
  Height := 150;

  FSubscribers := TInterfaceList.Create;

  FSurfaceBitmap := TGBitmap.Create;
  FBackgroundImage := TBitmap.Create;
  FSelection := TGGlobeSelection.Create(Self);
  FGraticule := TGGraticule.Create(Self);
  FGlobeLayer := TGLayer.Create(Self);

  FRenderer := TGDefaultRenderer.Create(Self);
  FRenderer.SetSize(Width, Height);

  FProjector := TGProjector.Create(Self);
  FProjector.ProjectionClass := 'TGSphericalPrj';
  FProjector.ScaleFactor := 0.0;
  FProjector.CenterXY := Point( Width div 2, Height div 2 );

  FLabelRenderer := TGLabelRenderer.Create(Self);

  // hook in the onChange handlers
  FGraticule.OnChanged := GlobePropertyChanged;
  FBackgroundImage.OnChange := GlobePropertyChanged;

  FScrollBars := ssBoth;
  FMouseZoomButton := mzbRight;
  FMouseSelectButton := mzbLeft;
  FMouseRotateButton := mzbLeft;

  FGlobeOptions := [];

  FCullObjectPixels := 0.75;
  FMinObjectPixels := 1.5;

  FPeriodicUpdateTime := 0;

  Color := clBlack;
  SurfaceColor := clAqua;
  MinObjectColor := clGray;

  Font.Name := 'Arial';
  Font.Color := clBlack;
  Font.Size := 9;

  Clear;
end;

procedure TGlobe5.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);

  case FScrollBars of
    ssHorizontal :
      Params.Style := Params.Style or WS_HSCROLL;
    ssVertical :
      Params.Style := Params.Style or WS_VSCROLL;
    ssBoth :
      Params.Style := Params.Style or WS_HSCROLL or WS_VSCROLL;
  end;
  if goTransparentBackground in GlobeOptions then
    Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
    
//  Params.ExStyle := Params.ExStyle or  WS_EX_CONTROLPARENT;
end;

destructor TGlobe5.Destroy;
begin
  GlobeNotify( gnDestroying );
  FreeAndNil( FSubscribers );

  FreeAndNil( FGlobeLayer );

  FreeAndNil( FSelection );
  FreeAndNil( FGraticule );
  FreeAndNil( FBackgroundImage );
  FreeAndNil( FSurfaceBitmap );
  FreeAndNil( FProjector );
  FreeAndNil( FRenderer );
  FreeAndNil( FLabelRenderer );

  SpatialDatabase := nil;

  inherited Destroy;
end;


procedure TGlobe5.DoOnAfterLayerRender(GLayer: TGLayer);
begin
  if Assigned( FOnAfterLayerRender ) then
  begin
    Renderer.RefreshDC;
    FOnAfterLayerRender( Self, GLayer );
  end;
end;

procedure TGlobe5.DoOnAfterRender;
begin
  if Assigned( FOnAfterRender ) then
    FOnAfterRender( Self );
end;

procedure TGlobe5.DoOnBeforeLayerRender(GLayer: TGLayer);
begin
  if Assigned( FOnBeforeLayerRender ) then
  begin
    Renderer.RefreshDC;
    FOnBeforeLayerRender( Self, GLayer );
  end;
end;

procedure TGlobe5.DoOnBeforeRender;
begin
  if Assigned( FOnBeforeRender ) then
  begin
    Renderer.RefreshDC;
    FOnBeforeRender( Self );
  end;
end;

procedure TGlobe5.DoOnLayersChanged;
begin
  if Assigned( FOnLayersChanged ) then
    FOnLayersChanged( Self );
end;

procedure TGlobe5.DoOnLoadImage( const image : TGBitmap; const imageName : string; var done : Boolean );
begin
  if Assigned( FOnLoadImage ) then
    FOnLoadImage( Self, image, imageName, done );
end;

procedure TGlobe5.DoOnMissingImage( var sourceName : String; var retry : Boolean );
begin
  if Assigned( FOnMissingImage ) then
    FOnMissingImage( Self, sourceName, retry );
end;

procedure TGlobe5.DoOnMissingLayer( const layerName, sourceName : String; userElement : TGXML_Element; var retry : Boolean );
begin
  if Assigned( FOnMissingLayer ) then
    FOnMissingLayer( Self, layerName, sourceName, userElement, retry );
end;

procedure TGlobe5.DoOnObjectRender( GLayer : TGLayer; mapObj : IGMapPoint; state : TGRenderStateSet );
begin
  Renderer.RefreshDC;
  if Assigned( FOnObjectRender ) then
  begin
    SaveDC( Canvas.Handle );
    FOnObjectRender( self, GLayer, mapObj, state );
    RestoreDC( Canvas.Handle, -1 );
  end;
  GlobeNotify(gnRender);
end;

procedure TGlobe5.DoOnPaint;
var
  flag : Boolean;
begin
  if Assigned(FOnPaint) then
  begin
    Renderer.RefreshDC;

    // Disable Double Buffer so that Renderer.Canvas returns the Component Canvas
    flag := Renderer.DoubleBuffer;
    Renderer.DoubleBuffer := false;

    FOnPaint(Self);

    Renderer.DoubleBuffer := flag;
  end;
  GlobeNotify(gnPaint);
end;

procedure TGlobe5.DoOnPanned;
begin
  if Assigned(FOnPanned) and not (csLoading in ComponentState) then
    FOnPanned(Self);
end;

procedure TGlobe5.DoOnPeriodicUpdate;
begin
  if Assigned( FOnPeriodicUpdate ) then
    FOnPeriodicUpdate( Self );
end;

procedure TGlobe5.DoOnProgress( progressType : TGProgressType; count : integer; percentage : double );
begin
  if Assigned( FOnProgress ) then
    FOnProgress( Self, progressType, count, percentage );
end;

procedure TGlobe5.DoOnRender;
begin
  if Assigned(FOnRender) then
  begin
    FOnRender(Self);
    Renderer.RefreshDC;
  end;

  GlobeNotify(gnRender);
end;

procedure TGlobe5.DoOnRotated;
begin
  if Assigned(FOnRotated) and not (csLoading in ComponentState) then
    FOnRotated(Self)
  else
    DoOnPanned;
end;

procedure TGlobe5.DoOnSelectionChange;
begin
  if Assigned( FOnSelectionChange ) then
    FOnSelectionChange( Self );
  RedrawLayers;
end;

procedure TGlobe5.DoOnZoomed;
begin
  if Assigned(FOnZoomed) and not (csLoading in ComponentState) then
    FOnZoomed(Self);
end;

procedure TGlobe5.DrawVectorMatrixLine(vec : TGVec3D; const mat : TGMatrix; iHeight, iSteps : integer);
var
  idx : integer;
  Chains : TGChainStore;
  pt : TGPointLL;
begin
  Chains := TGChainStore.Create;
  Chains.StoreHeight := iHeight > 0;

  for idx := 0 to iSteps do
  begin
    pt := Vec3D_ToPointLL(vec);
    pt.iHeightZ := iHeight;
    Chains[0].Add(pt);
    vec := Vec3D_MatrixMultiply(vec, mat);
  end;
  RenderChainStore(Chains,[rsFace, rsBackFace]);
  Chains.Free;
end;

function TGlobe5.EndFocusRect: TRect;
begin
  if FbDrawFocusRect then
  begin
    MouseCapture := False;
    Result := FFocusRect;
    FbDrawFocusRect := false;
    Invalidate;
  end
  else
    SetRectEmpty( Result );
end;

procedure TGlobe5.EndPath;
begin
  if FCurrentPath = nil then
    raise EGException.Create('EndPath: No Path is open');

  FCurrentPath := nil;
end;

procedure TGlobe5.EndUpdate;
begin
  Dec( FUpdateCount );
  Assert( FUpdateCount >= 0 );

  if FUpdateCount = 0 then
  begin
    UpdateLayerTree;
    RedrawLayers;
  end;
end;

function TGlobe5.GetBitmap(iWidth, iHeight : integer) : TBitmap;
begin
  Result := TBitmap.Create;
  Result.Width := iWidth;
  Result.Height := iHeight;

  RenderToCanvas(Result.Canvas, Rect(0, 0, iWidth, iHeight), True);
end;

function TGlobe5.GetClientRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;

  if ( ScrollBars = ssBoth ) or ( ScrollBars = ssVertical ) then
    Result.Right := Width - GetSystemMetrics(SM_CXVSCROLL)
  else
    Result.Right := Width;

  if ( ScrollBars = ssBoth ) or ( ScrollBars = ssHorizontal ) then
    Result.Bottom := Height - GetSystemMetrics(SM_CYHSCROLL)
  else
    Result.Bottom := Height;
end;

function TGlobe5.GetMetaFile(iWidth, iHeight : integer; bEnhanced : Boolean) : TMetafile;
var
  MetaCanvas : TMetafileCanvas;
begin
  Result := TMetafile.Create;
  Result.Width := iWidth;
  Result.MMWidth := iWidth * 26;
  Result.Height := iHeight;
  Result.MMHeight := iHeight * 26;
  Result.Enhanced := bEnhanced;

  MetaCanvas := TMetafileCanvas.CreateWithComment(Result, Canvas.Handle, TGLOBE_VERSION, '');
  try
    RenderToCanvas(MetaCanvas, Rect(0, 0, iWidth, iHeight), False);
  finally
    MetaCanvas.Free;
  end;
end;

function TGlobe5.GetMetaFileMM(iWidthMM, iHeightMM : integer; bEnhanced : Boolean) : TMetafile;
begin
  Result := GetMetaFile(( iWidthMM * 100 ) div 26, (iHeightMM * 100 ) div 26, bEnhanced );
end;

procedure TGlobe5.GetPOZ(var POZ : TGPOZ);
begin
  Projector.ProjectionModel.GetPOZ(POZ);
end;

procedure TGlobe5.GlobeNotify(notification: TGlobeNotification);
var
  idx : integer;
begin
  for idx := 0 to FSubscribers.Count - 1 do
    (FSubScribers[idx] as IGlobeSubscriber).GlobeNotification(self, notification);
end;

procedure TGlobe5.GlobePropertyChanged(Sender : TObject);
begin
  if not (( csLoading in ComponentState ) or ( csDestroying in ComponentState )) then
    RedrawLayers;
end;

function TGlobe5.GlobeUnitsFrom(const eValue : Extended; Units : TGUnitTypes) : Integer;
begin
  try
    if Units = guPixel then
      Result := Round(eValue / Projector.Scalefactor)
    else
      Result := Round(eValue * GU_Conversions[Ord(Units)]);
  except
    Result := Round(eValue);
  end;
end;

function TGlobe5.GlobeUnitsTo(iValue : Integer; Units : TGUnitTypes) : Extended;
begin
  try
    if Units = guPixel then
      Result := iValue * Projector.Scalefactor
    else
      Result := iValue / GU_Conversions[Ord(Units)];
  except
    Result := iValue;
  end;
end;

function TGlobe5.IsBusy: Boolean;
begin
  Result := FbPainting or ( FUpdateCount > 0 );
end;

function TGlobe5.IsRenderAborted : Boolean;
begin
  Result := FbRenderAborted;
end;

function TGlobe5.IsRenderRestarted : Boolean;
begin
  Result := FbRestartRender;
end;

procedure TGlobe5.LoadEnvironmentFromFile(const sFilename : TFilename);
var
  doc : TGXML_Document;
begin
  doc := TGXML_Document.Create('');
  try
    FEnvironmentFile := sFilename;

    if FileExists( sFilename ) then
    begin
      doc.LoadFromFile(EnvironmentFile);

      LoadFromXML(doc.Document);

      if Assigned(FOnLoadFromENV) then
        FOnLoadFromENV(Self, Doc);
    end;
  finally
    doc.Free;
  end;
end;

procedure TGlobe5.LoadFromXML(Element : TGXML_Element);
var
  iW, iH : integer;
  pt : TPoint;
  ratio : Extended;
begin
  BeginUpdate;
  Clear; // Clear the Globe
  if Element <> nil then
    with Element do
    begin
      if Assigned( ImageTileCache ) then
        ImageTileCache.LoadFromXML( Element('ImageTileCache'));

      SurfaceTextureName := Attribute('SurfaceTextureName', SurfaceTextureName);
      SurfaceColor := StringToColor(Attribute('SurfaceColor', ColorToString(clAqua)));
      MinObjectColor := StringToColor(Attribute('MinObjectColor', ColorToString(clGray)));

      iW := Attribute('Width', ClientWidth);
      iH := Attribute('Height', ClientHeight);

      Graticule := TGGraticule( TGComponent.CreateFromXML(Self, Element('Graticule', 0), TGGraticule ));
      Projector.LoadFromXML( Element('Projector', 0));
      Renderer.LoadFromXML( Element('Renderer', 0));

      with Projector do
      begin
        pt := CenterXY; // Save the center point before setting the SF
        ratio := Hypot( ClientWidth, ClientHeight ) / Hypot( iW, iH );
        ScaleFactor := ScaleFactor * ratio;
        CenterXY := Point( Round(ratio * pt.X), Round(ratio * pt.Y));
      end;

      // for some backwards compatability with older version of the .ENV xml.
      if Element('LayerStore', 0) <> nil then
      begin
        GlobeLayer.Presenters.LoadFromXML(Element('Presenters', 0));
        GlobeLayer.LayerStore.LoadFromXML( Element('LayerStore', 0));
      end
      else
        GlobeLayer.LoadFromXML(Element('GlobeLayer', 0));

      FbUpdateTreeRequired := true;
      UpdateLayerTree;

      UpdateScrollBars;
    end;
  EndUpdate;
end;

function TGlobe5.LoadImage(image: TGBitmap; var imageName : TFilename ) : Boolean;
var
  retry : Boolean;
  filename : String;
begin
  Result := false;
  Assert( image <> nil, 'TGlobe5.LoadImage: Passed nil image object' );

  DoOnLoadImage( image, imageName, Result );

  if Result = false then
  begin
    if not FileExists( imageName ) then
    begin
      filename := imageName;

      while not FileExists( filename ) do
      begin
        retry := false;
        DoOnMissingImage( filename, retry );
        if retry = false then
          Exit;
      end;
    end
    else
      filename := imageName;

    image.LoadFromFile(filename);
    Result := true;
  end;
end;


procedure TGlobe5.LocateToLL(ptLL : TGPointLL);
begin
  LocateToLL( ptlL.iLongX, ptLL.iLatY );
end;


procedure TGlobe5.LocateToDD(const long: TGDecimalDegree; const lat: TGDecimalDegree );
begin
  LocateToLL( DDToPointLL(Long, Lat));
end;

procedure TGlobe5.LocateToLL(iLong, iLat : Integer);
begin
  with Projector do
  begin
    CenterLongitude := Mod180(iLong) / GU_DEGREE;
    CenterLatitude := iLat / GU_DEGREE;
  end;
end;


procedure TGlobe5.LocateToObject(Obj : IGMapPoint);
begin
  if Obj <> nil then
    with Obj.ObjectMER do
      LocateToLL(iLongX + iWidthX div 2, iLatY + iHeightY div 2);
end;

procedure TGlobe5.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  SetFocus;

  if FbDrawFocusRect then
  begin
    EndFocusRect;
    Invalidate;
    Exit;
  end;

  inherited MouseDown(Button, Shift, X, Y);

  if (Shift * [ssShift, ssAlt, ssCtrl, ssDouble]) = [] then
  begin
    if Button = TMouseButton(FMouseZoomButton) then
    begin
      FbZooming := True;
      BeginFocusRect( X, Y );
      MouseCapture := True;
    end;

    if Button = TMouseButton(FMouseRotateButton) then
      if Projector.DeviceXYToLL(X, Y, ptRotate) then
      begin
        FbRotating := True;
        MouseCapture := True;
      end;
  end;
end;

procedure TGlobe5.MouseMove(Shift : TShiftState; X, Y : Integer);
var
  pTmp : TGPointLL;
begin
  if FbDrawFocusRect then
  begin
    Renderer.DrawFocusRect( FFocusRect );

    FFocusRect.BottomRight := Point(X, Y);

    Renderer.DrawFocusRect( FFocusRect );
  end;

  if FbRotating then
    with Projector do
      if DeviceXYToLL(X, Y, pTmp) then
      begin
        XRotation := XRotation + (ptRotate.iLatY - pTmp.iLatY) / GU_DEGREE;
        YRotation := YRotation + Mod180(ptRotate.iLongX - pTmp.iLongX) / GU_DEGREE;

        if PeriodicUpdateTime > 0 then
          RestartRender;
      end;

  inherited MouseMove(Shift, X, Y);
end;

procedure TGlobe5.MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
  Obj : IGMapPoint;
  rect, efr : TRect;
  SF : Extended;
begin
  MouseCapture := False;

  if FbZooming then
  begin
    FbZooming := False;

    efr := EndFocusRect;
    with efr do
      if (Abs( Right - Left ) < 10) or ( Abs( Bottom - Top ) < 10) then
        Invalidate { re-draw the globe to remove focus rect }
      else
      begin
        SF := Projector.ScaleFactor;
        // Convert to GlobeUnits from Device Points
        with AbsRect( efr ) do
        begin
          rect.Left := Round(Left / SF);
          rect.Right := Round(Right / SF) - rect.Left;
          rect.Top := Round(Top / SF);
          rect.Bottom := Round(Bottom / SF) - rect.Top;
        end;
        if ( Left > Right ) or ( Bottom < top ) then
          Projector.ScaleFactor := SF * 0.5
        else
          ViewRect := rect;
        RedrawLayers;
      end;
  end;

  FbRotating := False;

  Obj := GlobeLayer.ObjectAtXY(X, Y);

  if Obj = nil then
    ClearSelected
  else
  begin
//ToDo: Check the operation of the mouse select
//    if (goMultiSelect in GlobeOptions) and (ssCtrl in Shift) and (Button = TMouseButton(FMouseSelectButton)) then
    if not (goMultiSelect in GlobeOptions) and (Button = TMouseButton(FMouseSelectButton)) then
    begin
      ClearSelected;
      Obj.Selected := not Obj.Selected;
      DoOnSelectionChange;
    end;
  end;

  if Assigned(FOnObjectClick) and ( Button = mbLeft ) then
    FOnObjectClick(Self, Obj);

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TGlobe5.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = SpatialDatabase then
      FSpatialDatabase := nil;

    if AComponent = ImageTileCache then
      FImageTileCache := nil;
  end;
end;

procedure TGlobe5.NotifyLayerTreeChanged;
begin
  FbUpdateTreeRequired := true;

  if FUpdateCount = 0 then
    UpdateLayerTree;
end;

procedure TGlobe5.OnSDBNotification(Sender: TGSpatialDatabase; notification: TGSDBNotification);
//var
//  idx : integer;
//  layer : TGLayer;
begin
  case notification of
  gsdbAfterOpen, gsdbChanged:
    begin
      NotifyLayerTreeChanged;
    end;
  gsdbBeforeClose:
    begin
      // remove any SDB layers
      Clear;
//      for idx := GlobeLayer.LayerStore.Count - 1 downto 0 do
////        if not LayerStore[idx].ObjectSource.IsTemporary then
//        begin
//          layer := LayerStore[idx];
//          LayerStore.Remove( layer );
//          layer.Free;
//        end;
    end;
  end;
end;

procedure TGlobe5.Paint;
begin
  if ( csDestroying in ComponentState ) or ( Parent = nil ) or FbPainting or Printer.Printing then
    Exit;

  // To Stop and AV when the canvas becomes invalid
  if ( Renderer.Width <= 0 ) or ( Renderer.Height <= 0 ) then
    Exit;

  DoOnBeforeRender;

  FbRenderAborted := False;
  FbPainting := True;
  try
    repeat
      FbRestartRender := False;

      if (( FUpdateCount = 0 ) and FbRedrawLayers ) or not Renderer.DoubleBuffer then
        RenderLayers;

{$IFDEF TG_SHAREWARE}
    DisplaySharewareMessage( Renderer.Canvas );
{$ENDIF}

      if not IsRenderRestarted then
      begin
        { Paint the BMP onto the canvas and release }
        if Parent.HandleAllocated then
          if Renderer.DoubleBuffer then
            BitBlt(Canvas.Handle, 0, 0, Renderer.Width, Renderer.Height, Renderer.Canvas.Handle, 0, 0, SRCCOPY);

        if FbDrawFocusRect then
          Renderer.DrawFocusRect( FFocusRect );

        DoOnPaint;

        FbRedrawLayers := False;
      end;
    until not IsRenderRestarted or IsRenderAborted;
  finally
    FbPainting := False;

    DoOnAfterRender;
  end;
end;

procedure TGlobe5.PaintGraticule;
begin
  if Assigned(FOnPaintGraticule) then
  begin
    Renderer.RefreshDC;
    FOnPaintGraticule(Self);
  end
  else
    Graticule.Render;
end;

procedure TGlobe5.PaintSurface;
begin
  //Draw the Globe surface background
  if SurfaceColor <> clNone then
  begin
    Renderer.Pen.Style := gpsClear;
    Renderer.Brush.Color := SurfaceColor;
    Renderer.Brush.Style := gbsSolid;

    Projector.ProjectionModel.PaintSurface;
  end;

  // if a surface image is defined
  if not SurfaceBitmap.Empty then
    Projector.ProjectionModel.PaintSurfaceImage( FSurfaceBitmap );
end;

procedure TGlobe5.PaintWindowBackground;
var
  ifx, ify, iX, iY : Integer;
begin
  { Make sure background color is correct }
  Brush.Color := FBackgroundColor;
  Brush.Style := Graphics.bsSolid;

  if goTransparentBackground in GlobeOptions then { if no bitmap specified }
    if BackgroundImage.Empty then
    begin
      BackgroundImage.HandleType := bmDIB;
      BackgroundImage.PixelFormat := pf24bit;
      BackgroundImage.Width := Renderer.Width;
      BackgroundImage.Height := Renderer.Height;
      if Parent.HandleAllocated then
        BitBlt(BackgroundImage.Canvas.Handle, 0, 0, Renderer.Width, Renderer.Height, Self.Canvas.Handle, 0, 0, SRCCOPY);
    end;

  if BackgroundImage.Empty then { if no bitmap specified }
    Renderer.Clear
  else
    with BackgroundImage do { tile bitmap }
    begin
      ifx := (Renderer.Width div Width) * Width;
      ify := (Renderer.Height div Height) * Height;

      iX := 0;
      while iX <= ifx do
      begin
        iY := 0;
        while iY <= ify do
        begin
          { Draw the bitmap }
          Renderer.Canvas.Draw(iX, iY, BackgroundImage);
          Inc(iY, Height);
        end;
        Inc(iX, Width);
      end;
    end;
end;

procedure TGlobe5.PanViewXY(iX, iY : Integer);
begin
  if (iX <> 0) or (iY <> 0) then
    with Projector do
    begin
      XOrigin := XOrigin - Round(iX * ScaleFactor);
      YOrigin := YOrigin - Round(iY * ScaleFactor);

      DoOnPanned;
    end;

  UpdateScrollBars;
  RedrawLayers;
end;

procedure TGlobe5.PeriodicUpdateRender;
begin
  if ( not FbRenderToCanvas ) and FbPainting and ( PeriodicUpdateTime > 0 ) then
    if GetTickCount > FRenderUpdateTime then
    begin
      if Parent.HandleAllocated then
        BitBlt(Canvas.Handle, 0, 0, Renderer.Width, Renderer.Height, Renderer.Canvas.Handle, 0, 0, SRCCOPY);

      FRenderUpdateTime := GetTickCount + DWord( PeriodicUpdateTime );
      DoOnPeriodicUpdate;
    end;
end;

function TGlobe5.RectToMER( rect : TRect ) : TGMER;
var
  ptStore : TGPointStore;
  ptLL : TGPointLL;
  idx, jdx, steps : integer;
  mid, X, Y, widthStep, heightStep : Extended;
  valid, state : Boolean;
begin
  // Get the visible rectangle
  ptStore := TGPointStore.Create;

  steps := 64;

  with AbsRect( rect ) do
  begin
    widthStep := ( Right - Left ) / steps;
    heightStep := ( Bottom - Top ) / steps;

    for idx := 0 to steps do
    begin
      Y := Top + idx * heightStep;

      state := Projector.DeviceXYToLL( Left, Trunc( Y ), ptLL );

      for jdx := 0 to steps do
      begin
        X := Left + jdx * widthStep;

        valid := Projector.DeviceXYToLL( Trunc( X ), Trunc( Y ), ptLL );
        if valid then
          ptStore.Add(ptLL);

        if valid <> state then  // we have crossed an edge
        begin
          mid := widthStep / 2;
          X := X - mid;
          while mid >= 1.0 do
          begin
            mid := mid / 2;
            valid := Projector.DeviceXYToLL( Trunc( X ), Trunc( Y ), ptLL );
            if valid then
              ptStore.Add(ptLL);
            if valid = state then
              X := X + mid
            else
              X := X - mid;
          end;
        end;
        state := valid;
      end;
    end;
  end;

  if ptStore.Count > 10 then
  begin
    Result := ptStore.PointStoreMER;
    if Result.iWidthX > ( GU_DEGREE * 330 ) then  // if a very wide MER
      Result := GlobeMER;
  end
  else
    MER_Empty(Result);

  ptStore.Free;
end;

procedure TGlobe5.RedrawLayers;
begin
  if not FbRedrawLayers then
  begin
    FbRedrawLayers := True;
    Invalidate;
  end;
end;

procedure TGlobe5.RemoveOrphanedLayers( layerStore : TGLayerStore );
var
  idx : integer;
  layer : TGLayer;
begin
  for idx := layerStore.Count - 1 downto 0 do
  begin
    layer := LayerStore[idx];
    RemoveOrphanedLayers(layer.LayerStore); // process sub-layers

    if not layer.ObjectSource.IsTemporary then
      if ( SpatialDatabase <> nil ) and ( SpatialDatabase.SDBLayers.LayerByID( layer.LayerID ) = nil ) then
      begin
        LayerStore.Remove( layer );
        layer.Free;
      end;
  end;
end;

procedure TGlobe5.RenderChainStore(Chains : TGChainStore; State : TGRenderStateSet);
var
  iEdge, iCM, jdx, idx, iPoints, iLastPoint : integer;
  iPenWidth, iChain, iPoly, iZone, iLastZone : Integer;
  PolyChain : TGChainStore;
  PointStore : TGPointStore;
  ptLL, tmpLL, lastLL : TGPointLL;
begin
  if FCurrentPath <> nil then
  begin
    for idx := 0 to Chains.Count - 1 do
      FCurrentPath.Add(Chains[idx].Clone);
    Exit;
  end;

  iPenWidth := ScaleUnitsToDevice( Renderer.Pen.Width, Renderer.Pen.WidthUnit );

//ToDO:  RequiredState([csHandleValid, csPenValid, csBrushValid, csFontValid]);

{  if Projector.RenderBackFace then
  begin
    SelectObject(Renderer.Canvas.Handle, DefaultAttributes.TransparentBrush.AttributeHandle(Renderer));
    SelectObject(Renderer.Canvas.Handle, DefaultAttributes.TransparentPen.AttributeHandle(Renderer));
  end;
}
  with Projector do
    if IsContinuous and (rsClosed in State) then
    begin
      Renderer.BeginDrawPoly(rsClosed in State);
      for iChain := 0 to Chains.Count - 1 do
        if not Chains[iChain].Hidden then
        begin
          iPoints := ProjectionModel.PointStoreToXY(Chains[iChain], 0);
          Renderer.AddClippedPoly(iPoints, iPenWidth);
        end;
      Renderer.EndDrawPoly;
      Exit;
    end;

  // Render the poly in parts
  iCM := Round(Projector.CentralMeridian * GU_DEGREE);
  iEdge := Mod180(GU_180_DEGREE + iCM);

  PolyChain := TGChainStore.Create;
  PolyChain.StoreHeight := Chains.StoreHeight;
  try
    Renderer.BeginDrawPoly(rsClosed in State);
    for iChain := 0 to Chains.Count - 1 do
      if not Chains[iChain].Hidden then
      begin
        PolyChain.Count := 0;
        iPoly := 0;

        iPoints := Chains[iChain].Count - 1;
        if iPoints < 1 then
          Continue;

        // set the height flag
  //      PolyChain[iPoly].StoreHeight := Chains[iChain].StoreHeight;

        if rsClosed in State then // adjust last point for polygons
          iLastPoint := iPoints
        else
          iLastPoint := 0;
        iLastZone := Mod180(Chains[iChain].AsLL[iLastPoint].iLongX - iCM) div GU_90_DEGREE;

        // split into chains
        for idx := 0 to iPoints do
        begin
          ptLL := Chains[iChain].AsLL[idx];
          iZone := Mod180(ptLL.iLongX - iCM) div GU_90_DEGREE;
          if ((iZone < 0) and (iLastZone > 0)) or ((iZone > 0) and (iLastZone < 0)) then
          begin
            tmpLL := ptLL;

            if idx > 0 then
              iLastPoint := idx - 1;
            lastLL := Chains[iChain].AsLL[iLastPoint];

            // find crossover point for edge
            with tmpLL do
              iLatY := iLatY + Round((lastLL.iLatY - iLatY) *
                (LongDiff(iEdge, iLongX) / LongDiff(lastLL.iLongX, iLongX)));

            tmpLL.iLongX := Mod180( iEdge - iLastZone * 2 );
            PolyChain[iPoly].Add(tmpLL); // last point on current chain

            Inc(iPoly);
            tmpLL.iLongX := Mod180( iEdge + iLastZone * 2 );
            PolyChain[iPoly].Add(tmpLL); // first point on new chain
          end;
          PolyChain[iPoly].Add(ptLL);
          iLastZone := iZone;
        end;

        if rsClosed in State then // render alternate chains as the polygons
          with Projector.ProjectionModel do
          begin
            iPoints := 0;
            idx := 0;
            while idx < PolyChain.Count do
            begin
              iPoints := PointStoreToXY(PolyChain[idx], iPoints);
              Inc(idx, 2);
            end;

            if idx <> PolyChain.Count then
            begin
              Renderer.AddClippedPoly(iPoints, iPenWidth);
              iPoints := 0;
            end
            else
            begin // connect to first chain
              PointStore := TGPointStore.Create;
              PointStore.StoreHeight := Chains.StoreHeight;

              with PolyChain[idx - 2] do
                tmpLL := AsLL[Count - 1];
              ptLL := tmpLL;

              while tmpLL.iLatY > -( GU_90_DEGREE - GU_MINUTE ) do
              begin
                tmpLL.iLatY := tmpLL.iLatY - GU_MINUTE;
                PointStore.Add(tmpLL);
              end;
              tmpLL.iLatY := -GU_90_DEGREE + 1;
              PointStore.Add(tmpLL);

              tmpLL.iLongX := iEdge + 2;
              ptLL.iLongX := tmpLL.iLongX;

              while tmpLL.iLatY < ptLL.iLatY do
              begin
                tmpLL.iLatY := tmpLL.iLatY + GU_MINUTE;
                PointStore.Add(tmpLL);
              end;
              PointStore.Add(ptLL);
              iPoints := PointStoreToXY(PointStore, iPoints);
              PointStore.Free;
            end;

            idx := 1;
            while idx < PolyChain.Count do
            begin
              iPoints := PointStoreToXY(PolyChain[idx], iPoints);
              Inc(idx, 2);
            end;
            Renderer.AddClippedPoly(iPoints, iPenWidth);
          end
        else // render as polyline
          with Projector.ProjectionModel do
            for idx := 0 to PolyChain.Count - 1 do
            begin
              iPoints := 0;
              for jdx := 0 to PolyChain[idx].Count - 1 do
                if PointLLToXY(PolyChain[idx][jdx], iPoints) then
                  Inc(iPoints)
                else
                begin
                  if iPoints > 1 then
                    Renderer.AddClippedPoly(iPoints, iPenWidth);
                  iPoints := 0;
                end;
              Renderer.AddClippedPoly(iPoints, iPenWidth);
            end;
      end;
  finally
    Renderer.EndDrawPoly;
    PolyChain.Free;
  end;
end;

procedure TGlobe5.RenderCircle(const ptLL : TGPointLL; iRadius : integer;
  radiusUnit : TGUnitTypes; iSteps : integer);
var
  mat : TGMatrix;
  quat : TGQuaternion;
  longX : integer;
  tmpLL : TGPointLL;
begin
  //  Renderer.RefreshDC;
  { use 4 Degree steps to draw circle }
  if (iSteps > 360) or (iSteps < 3) then
    iSteps := 36;

  Quat_FromAxisAngle(quat, V3D(ptLL), (360 / iSteps) * GU_DEGREE * GU_TORADIANS);
  Quat_ToMatrix(quat, mat);

  // minor hack to avoid a divide by zero problem when the circle is centered on multiple of 90 degrees
  longX := ptLL.iLongX;
  if ( longX mod GU_90_DEGREE ) = 0 then
    Inc( longX );

  tmpLL := PointLLH(longX, ptLL.iLatY + GlobeUnitsFrom( iRadius, radiusUnit ), ptLL.iHeightZ );
  DrawVectorMatrixLine( V3D( tmpLL ), mat, ptLL.iHeightZ, iSteps);
end;

procedure TGlobe5.RenderGreatCircleLine(const ptFromLL, ptToLL : TGPointLL;
  CircleType : TGGreatCircle);
var
  a, b : TGVec3D;
  angle : Extended;
  iSteps, penWidth : integer;
  mat : TGMatrix;
  quat : TGQuaternion;
begin
  penWidth := ScaleUnitsToDevice( Renderer.Pen.Width, Renderer.Pen.WidthUnit );

  //  Renderer.RefreshDC;
  a := V3D(ptFromLL);
  b := V3D(ptToLL);

  case CircleType of
    gcShortest :
      angle := ArcCos(Vec3D_Dot(a,b));
    gcLongest :
      angle := DoublePi - ArcCos(Vec3D_Dot(a,b));
  else
    angle := DoublePi; { gcCircle }
  end;

  iSteps := Trunc(angle / (GU_DEGREE * GU_TORADIANS));
  if iSteps > 0 then
  begin
    Quat_FromAxisAngle(quat, Vec3D_Cross(a,b), -angle / iSteps);
    Quat_ToMatrix(quat, mat);

    if CircleType = gcLongest then
      DrawVectorMatrixLine(b, mat, 0, iSteps)
    else
      DrawVectorMatrixLine(a, mat, 0, iSteps);
  end
  else
    RenderLine(ptFromLL, ptToLL, penWidth);
end;

procedure TGlobe5.RenderLayers;
var
  viewMER : TGMER;
begin
  Renderer.RefreshDC;
  FRenderUpdateTime := GetTickCount + DWord( PeriodicUpdateTime ); // for periodic updating

  // get the MER for the current view
  viewMER := RectToMER( Rect( 0, 0, Width, Height ));
  if MER_IsEmpty( viewMER) then
    viewMER := MERFromDD( -180.0, -90.0, 360.0, 180.0 );

  PaintWindowBackground;
  PaintSurface;

  { Draw the Layers }
{  if goTransparentGlobe in GlobeOptions then
  begin
    Projector.RenderBackFace := True;

    GlobeLayer.Render( 0, ViewMER ); // Render the layers for the back face

    if IsRenderRestarted then
      Exit;

    // Draw the Graticule if required
    if Graticule.DrawMode <> gdNone then
      PaintGraticule;

    Projector.RenderBackFace := False;
  end;
}
  // Draw the Graticule if required
  if Graticule.DrawMode = gdBehind then
    PaintGraticule;

  LabelRenderer.Clear;  // Reset the label renderer.

  // Render the layers
  GlobeLayer.Render( true, viewMER ); // Render the layers

  if not IsRenderRestarted then
    DoOnRender;

  { Draw the Graticule if required }
  if Graticule.DrawMode = gdInFront then
    PaintGraticule;

  // Render the Labels
  if not IsRenderRestarted then
    LabelRenderer.Render;
end;

procedure TGlobe5.RenderLine(const ptFromLL, ptToLL : TGPointLL; iSteps : Integer);
var
  idx, iXDist, iYDist : Integer;
  Chains : TGChainStore;
  State : TGRenderStateSet;
begin
  //  Renderer.RefreshDC;
  if iSteps <= 0 then iSteps := 1;

  State := [rsFace, rsBackFace];
  Chains := TGChainStore.Create;
  Chains.StoreHeight := (ptFromLL.iHeightZ <> 0) or (ptToLL.iHeightZ <> 0);

  iYDist := ptToLL.iLatY - ptFromLL.iLatY;
  iXDist := ptToLL.iLongX - ptFromLL.iLongX;

  // take the shortest route between the points
  if Abs( iXDist ) >= GU_180_DEGREE then
  begin
    if iXDist < 0 then
      iXDist := iXDist + GU_360_DEGREE
    else
      iXDist := iXDist - GU_360_DEGREE;
  end;

  Chains[0].Count := iSteps + 1;
  for idx := 0 to iSteps do
    Chains[0].AsLL[idx] := PointLL(
      ptFromLL.iLongX + MulDiv(iXDist, idx, iSteps),
      ptFromLL.iLatY + MulDiv(iYDist, idx, iSteps));

  with Renderer do
    if Projector.ProjectionModel.VisibleMER(Chains.ChainStoreMER, State) then
      RenderChainStore(Chains, State);
  Chains.Free;
end;

procedure TGlobe5.RenderPolygon(const Points : array of TGPointLL);
var
  idx : Integer;
  Chains : TGChainStore;
begin
  Chains := TGChainStore.Create;
  Chains.StoreHeight := True;
  try
    for idx := 0 to High(Points) do
      Chains[0].Add(Points[idx]);

    RenderChainStore( Chains, [rsClosed] );
  finally
    Chains.Free;
  end;
end;

procedure TGlobe5.RenderPolyLine(const Points : array of TGPointLL);
var
  idx : Integer;
  Chains : TGChainStore;
begin
  Chains := TGChainStore.Create;
  Chains.StoreHeight := True;
  try
    for idx := 0 to High(Points) do
      Chains[0].Add(Points[idx]);

    RenderChainStore(Chains, []);
  finally
    Chains.Free;
  end;
end;

function TGlobe5.RenderShadow(GMTDateTime : TDateTime; ColorNight: TColor;
  TwilightDegrees : integer; MaxAlpha : Byte) : TGPointLL;
begin
  RenderSunShadow( Self, GMTDateTime, ColorNight, TwilightDegrees, MaxAlpha ); 
end;

procedure TGlobe5.RenderTextOut(const ptLL : TGPointLL; const sText : String);
begin
  if Projector.ProjectionModel.LLToXY(ptLL.iLongX, ptLL.iLatY, 0) then
    with Renderer do
      if FontRenderSize( Font.Size, Font.SizeUnit ) >= GetMinTextSize then
      begin
        SetBkMode(Renderer.Canvas.Handle, TRANSPARENT);

        with Renderer.Points[0] do
          Renderer.DrawText( X, Y, sText );
      end;
end;

procedure TGlobe5.RenderTextureTriangle(const srcTri : TGTriangle;
  const destTriLL: TGTriangleLL; const image: TGBitmap; selected : Boolean);
var
  jdx, idx, kdx, edge, edgeOffset, lhsCount : integer;
  screenTri, txtTri : TGTriangle;
  lhs : array [0..2] of Boolean;
  splitLat : array [0..2] of integer;
  splitY : array [0..2] of TPoint;
  t : Extended;
  range : integer;

  procedure showSelected( pt : TPoint );
  begin
    with Renderer do
    begin
      Pen := SelectedPen;
      RealizeAttributes;

      Canvas.PolyLine([Point( pt.X - 2, pt.Y - 2), Point(pt.X + 3, pt.Y + 3)]);
      Canvas.PolyLine([Point( pt.X - 2, pt.Y + 2), Point(pt.X + 3, pt.Y - 3)]);
    end;
  end;

  procedure DrawTri( destLL0, destLL1, destLL2 : TGPointLL; ptXY0, ptXY1, ptXY2 : TPoint );
  begin
    with Projector do
    begin
      PointLLToXY( destLL0, screenTri[0] );
      PointLLToXY( destLL1, screenTri[1] );
      PointLLToXY( destLL2, screenTri[2] );
    end;

    txtTri[0] := ptXY0;
    txtTri[1] := ptXY1;
    txtTri[2] := ptXY2;

    Renderer.DrawTextureTriangle( txtTri, screenTri, Image );
    if selected then
      ShowSelected( screenTri[0] );
  end;
begin
  if not Projector.TriangleLLToXY( destTriLL, screenTri ) then
    Exit;

  with Projector do
    if IsContinuous then
    begin
      Renderer.DrawTextureTriangle( srcTri, screenTri, Image );
      if selected then
        ShowSelected( screenTri[0] );

      Exit;
    end;

  edge := Mod180( GU_180_DEGREE + Round( Projector.CentralMeridian * GU_DEGREE ));

  // work out which points are on the LHS of the edge.
  lhs[0] := ( destTriLL[0].iLongX < edge );
  lhs[1] := ( destTriLL[1].iLongX < edge );
  lhs[2] := ( destTriLL[2].iLongX < edge );
  lhsCount := Ord(lhs[0]) + Ord(lhs[1]) + Ord(lhs[2]);

  // check for triangles that are not split on the LHS or RHS
  if ( lhsCount = 0 ) or ( lhsCount = 3 ) then
    with Projector do
    begin
      Renderer.DrawTextureTriangle(srcTri, screenTri, Image );
      if selected then
        ShowSelected( screenTri[0] );
      Exit;
    end;

  // calculate the points at which the triangle intersects the edge
  for idx := 0 to 2 do
  begin
    jdx := ( idx + 1 ) mod 3;
    range := ( destTriLL[jdx].iLongX - destTriLL[idx].iLongX );

    if range = 0 then
    begin
      splitLat[idx] := destTriLL[idx].iLatY;
      splitY[idx] := srcTri[idx];
    end
    else   // if the line crosses the edge
      if lhs[idx] xor lhs[jdx] then
      begin
        t := ( edge - destTriLL[idx].iLongX ) / range;
        splitLat[idx] := destTriLL[idx].iLatY + Round( t * ( destTriLL[jdx].iLatY - destTriLL[idx].iLatY ));

        splitY[idx] := Point(
          srcTri[idx].X + Round( t * ( srcTri[jdx].X - srcTri[idx].X )),
          srcTri[idx].Y + Round( t * ( srcTri[jdx].Y - srcTri[idx].Y )));
      end
  end;


  if lhs[0] = ( lhsCount = 1 ) then
  begin
    idx := 1;
    jdx := 2;
    kdx := 0;
  end
  else if lhs[1] = ( lhsCount = 1 ) then
  begin
    idx := 2;
    jdx := 0;
    kdx := 1;
  end
  else
  begin
    idx := 0;
    jdx := 1;
    kdx := 2;
  end;

  if lhsCount = 1 then
    edgeOffset := 1
  else
    edgeOffset := -1;


  // Draw the single triangle
  DrawTri( destTriLL[kdx], PointLL(edge - edgeOffset, splitLat[kdx]), PointLL(edge - edgeOffset, splitLat[jdx]),
      srcTri[kdx], splitY[kdx], splitY[jdx] );

  // Draw the pair of triangles
  DrawTri( destTriLL[jdx], destTriLL[idx], PointLL(edge + edgeOffset, splitLat[kdx]),
    srcTri[jdx], srcTri[idx], splitY[kdx] );

  DrawTri( destTriLL[jdx], PointLL(edge + edgeOffset, splitLat[jdx]), PointLL(edge + edgeOffset, splitLat[kdx]),
    srcTri[jdx], splitY[jdx], splitY[kdx] );

end;

procedure TGlobe5.RenderToCanvas(ACanvas : TCanvas; ARect : TRect; bAsImage : Boolean);
var
  ptOrg : TPoint;
  SavedRenderer : TGRenderer;
  SavedProjection : TGProjector;
  SavedCanvasHandle : HDC;
begin
  if FbRenderToCanvas then Exit;
  FbRenderToCanvas := True;

  SavedProjection := Projector;
  FProjector := Projector.Clone;

  SavedRenderer := Renderer;
  FRenderer := Renderer.Clone;

  // Adjust the view port origin
  GetViewPortOrgEx(ACanvas.Handle, ptOrg);
  SetViewPortOrgEx(ACanvas.Handle, ptOrg.x + ARect.Left, ptOrg.y + ARect.Top, nil);

{$IFDEF TG_BCB5}
  SavedCanvasHandle := Canvas.Handle;
{$ELSE}
  {$IFDEF TG_DELPHI5}
    SavedCanvasHandle := Canvas.Handle;
  {$ELSE}
    if Canvas.HandleAllocated then
      SavedCanvasHandle := Canvas.Handle
    else
      SavedCanvasHandle := 0;
  {$ENDIF}
{$ENDIF}

  if not bAsImage then
    Canvas.Handle := ACanvas.Handle;

  Renderer.DoubleBuffer := bAsImage;
  Renderer.SetSize(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
  Renderer.PixelsPerInch := GetDeviceCaps(ACanvas.Handle, LOGPIXELSX);
  Renderer.RefreshDC;

  ResizeProjection(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);

  DoOnBeforeRender;
  try
    RenderLayers;

    DoOnPaint;

{$IFDEF TG_SHAREWARE}
    DisplaySharewareMessage( Renderer.Canvas );
{$ENDIF}

    if bAsImage then
      BitBlt(ACanvas.Handle, ARect.Left, ARect.Top,
        ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, Renderer.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    // restore the view port origin
    SetViewPortOrgEx(ACanvas.Handle, ptOrg.x, ptOrg.y, nil);

    FRenderer.Free;
    FRenderer := SavedRenderer;

    FProjector.Free;
    FProjector := SavedProjection;

    if not bAsImage and ( SavedCanvasHandle <> 0 )then
      Canvas.Handle := SavedCanvasHandle;

    FbRenderToCanvas := False;
    DoOnAfterRender;
  end;
end;

function TGlobe5.ResizeProjection(iWidth, iHeight: Integer): Boolean;
var
  eRatio, eSF : Extended;
  iXOrg, iYorg : integer;
  ptCenter : TGPointLL;
  ptXY : TPoint;
  ValidPoint : Boolean;
  OrgX, OrgY : Extended;
begin
  Result := (iWidth > 0) and (iHeight > 0);

  if Result then
    with Projector do
    begin
      ValidPoint := DeviceXYToLL(Width div 2, Height div 2, ptCenter);
      { Use the window diagonal to control the sizing ratio }
      eRatio := Hypot(iWidth, iHeight) / Hypot(Width, Height);

      iXOrg := MulDiv( XOrigin, iWidth, Width);
      iYOrg := MulDiv( YOrigin, iHeight, Height);

      if not (goConstantScaleOnResize in TGlobe5(ParentGlobe).GlobeOptions) then
      begin
        eSF := ScaleFactor * eRatio;

        if eSF > TG_MAXSCALEFACTOR then
          eSF := TG_MAXSCALEFACTOR;
        if MaxVal(iWidth, iHeight) / eSF > Maxint then
          eSF := MaxVal(iWidth, iHeight) / Maxint;

        // Adjust the origin and the scalefactor
        OrgX := ( XOrigin - Width / 2) / ScaleFactor;
        XOrigin := Round( iWidth / 2 + OrgX * eSF);
        OrgY := ( YOrigin - Height / 2) / ScaleFactor;
        YOrigin := Round(iHeight / 2 + OrgY * eSF);

        FViewRect := Rect(0, 0, Round(iWidth / eSF), Round(iHeight / eSF));
        Projector.ScaleFactor := eSF;

        ProjectionModel.PropertyChanged(ppScaleFactor);
      end;

      if not (goFixedCenterXYOnResize in TGlobe5(ParentGlobe).GlobeOptions) then
      begin
        if ValidPoint then
        begin
          PointLLToXY(ptCenter, ptXY);
          iXOrg := XOrigin + ((iWidth div 2 ) - ptXY.X);
          iYOrg := YOrigin + ((iHeight div 2 ) - ptXY.Y);
        end;
        XOrigin := Round( iXOrg );
        YOrigin := Round( iYOrg );
      end;
//      Width := iWidth;
//      Height := iHeight;
      //      Globe.ViewRect := Rect( 0, 0, Round( FiCanvasWidth / eSF ), Round( FiCanvasHeight / eSF ) );
    end;
end;

procedure TGlobe5.RestartRender;
begin
  FbRestartRender := true;
end;

procedure TGlobe5.SaveEnvironmentToFile(const sFilename : TFilename);
var
  doc : TGXML_Document;
begin
  doc := TGXML_Document.Create( 'TGlobe' );
  try
    SaveToXML( doc.Document );

    if Assigned(FOnSaveToENV) then
      FOnSaveToENV(Self, doc);

    doc.SaveToFile(sFilename);
  finally
    doc.Free;
  end;
end;

procedure TGlobe5.SaveToXML(Element : TGXML_Element);
begin
  Element.AddAttribute('SurfaceTextureName', SurfaceTextureName, '');
  Element.AddAttribute('SurfaceColor', ColorToString(SurfaceColor), ColorToString(clAqua));
  Element.AddAttribute('MinObjectColor', ColorToString(MinObjectColor), ColorToString(clGray));
  Element.AddAttribute('Width', ClientWidth, -1);
  Element.AddAttribute('Height', ClientHeight, -1);

  Graticule.SaveToXML( Element.AddElement('Graticule'));
  Projector.SaveToXML( Element.AddElement('Projector'));
  Renderer.SaveToXML( Element.AddElement('Renderer'));

  GlobeLayer.SaveToXML( Element.AddElement('GlobeLayer'));

  if Assigned( ImageTileCache ) then
    ImageTileCache.SaveToXML( Element.AddElement('ImageTileCache'));
end;

function TGlobe5.ScaleUnitsToDevice(Value: Integer; Units: TGUnitTypes): Integer;
begin
  if Units = guPixel then
    Result := MulDiv(Value, Renderer.PixelsPerInch, Screen.PixelsPerInch)
  else
    Result := Round(GlobeUnitsFrom(Value, Units) * Projector.ScaleFactor);
end;

function TGlobe5.SelectObjects( aMER : TGMER; firstOnly, state : Boolean ) : integer;
begin
  Result := GlobeLayer.SelectObjects(aMER, true, firstOnly, state);

  if Result > 0 then
    DoOnSelectionChange;
end;

procedure TGlobe5.SetBackgroundBitmap(NewBitmap : TBitmap);
begin
  BackgroundImage.Assign(NewBitmap);
  ReDrawLayers;
end;

procedure TGlobe5.SetBackgroundColor(Value : TColor);
begin
  FBackgroundColor := Value;
  ReDrawLayers;
end;

procedure TGlobe5.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  iW, iH : integer;
  pt : TPoint;
  ratio : Extended;
begin
  inherited;

  if Renderer = nil then
    Exit;

  iW := Renderer.Width;
  iH := Renderer.Height;

  if ( iW <> ClientWidth ) or ( iH <> ClientHeight ) then
  begin
    Renderer.SetSize(ClientWidth, ClientHeight);
    Renderer.PixelsPerInch := Screen.PixelsPerInch;

    if ( csDesigning in ComponentState ) or ( iW = 0 ) or ( iH = 0 ) then
      Projector.CenterXY := Point( ClientWidth div 2, ClientHeight div 2 )
    else
      with Projector do
      begin
        pt := CenterXY; // remember the center point before setting the SF
        ratio := Hypot( ClientWidth, ClientHeight ) / Hypot( iW, iH );
        ScaleFactor := ScaleFactor * ratio;
        CenterXY := Point( Round( ratio * pt.X ), Round(ratio * pt.Y ));
      end;

    RedrawLayers;
  end;
end;

procedure TGlobe5.SetCullObjectPixels(const Value: Double);
begin
  if Value <> FCullObjectPixels then
  begin
    FCullObjectPixels := Value;
    RedrawLayers;
  end;
end;

procedure TGlobe5.SetGlobeOptions(Value : TGlobeOptionsSet);
begin
  // if Transparent background setting has changed
  if (goTransparentBackground in FGlobeOptions) <> (goTransparentBackground in Value) then
  begin
    if goTransparentBackground in FGlobeOptions then
    begin
      BackgroundImage.Width := 0;
      BackgroundImage.Height := 0;
    end;
    RecreateWnd;
  end;

  {  if (goTransparentBackground in FGlobeOptions ) <> (goTransparentBackground in Value ) then
    begin
      BackgroundImage.Free;
      BackgroundImage := TBitmap.Create;
      BackgroundImage.OnChange := GlobePropertyChanged;

      RecreateWnd;
    end;
  }
  FGlobeOptions := Value;
  UpdateScrollBars;
  RedrawLayers;
end;

procedure TGlobe5.SetGraticule(const Value: TGGraticule);
begin
  if FGraticule <> Value then
  begin
    FreeAndNil( FGraticule );
    FGraticule := Value;
    FGraticule.OnChanged := GlobePropertyChanged;
  end;
end;

procedure TGlobe5.SetImageTileCache(const Value: TGImageTileCache);
begin
  FImageTileCache := Value;
end;

procedure TGlobe5.SetMinObjectColor(const Value: TColor);
begin
  if Value <> FMinObjectColor then
  begin
    FMinObjectColor := Value;
    RedrawLayers;
  end;
end;


procedure TGlobe5.SetMinObjectPixels(const Value: Double);
begin
  if Value <> FMinObjectPixels then
  begin
    FMinObjectPixels := Value;
    RedrawLayers;
  end;
end;

procedure TGlobe5.SetPOZ(const POZ : TGPOZ);
begin
  Projector.ProjectionModel.SetPOZ(POZ);
end;

procedure TGlobe5.SetProjector(const Value: TGProjector);
begin
  if FProjector <> Value then
  begin
    FreeAndNil( FProjector );
    if Value <> nil then
      FProjector  := Value
    else
      FProjector := TGProjector.Create(Self);
  end;
end;

procedure TGlobe5.SetRenderer(const Value: TGRenderer);
begin
  if FRenderer <> Value then
  begin
    FreeAndNil( FRenderer );

    if Value <> nil then
      FRenderer := Value
    else
      FRenderer := TGDefaultRenderer.Create(Self); // always have the default renderer

    Renderer.SetSize(ClientWidth, ClientHeight);
    Renderer.PixelsPerInch := Screen.PixelsPerInch;
  end;
end;

procedure TGlobe5.SetScrollBars(Value : TScrollStyle);
begin
  if Value <> FScrollBars then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TGlobe5.SetSpatialDatabase(const Value: TGSpatialDatabase);
begin
  if FSpatialDatabase <> Value then
  begin
    if SpatialDatabase <> nil then
      SpatialDatabase.UnSubscribe( self );

    FSpatialDatabase := Value;

    if SpatialDatabase <> nil then
    begin
      SpatialDatabase.Subscribe( self );

      if SpatialDatabase.Active then
        OnSDBNotification(SpatialDatabase, gsdbAfterOpen);
    end;
  end;
end;

procedure TGlobe5.SetSurfaceColor(const Value: TColor);
begin
  FSurfaceColor := Value;
  RedrawLayers;
end;

procedure TGlobe5.SetSurfaceTextureName(sName : TFilename);
begin
  FSurfaceTextureName := Trim( sName );

  if FSurfaceTextureName = '' then
    SurfaceBitmap.Delete
  else
    SurfaceBitmap.LoadFromFile(FSurfaceTextureName);

  if SurfaceBitmap.Empty then
    FSurfaceTextureName := '';

  RedrawLayers;
end;

procedure TGlobe5.SetViewRect(const sRect : TRect);
var
  eSF : Extended;
  iXOrg, iYOrg : integer;
begin
  if (sRect.Right = 0) or (sRect.Bottom = 0) then
  begin
    Projector.ScaleFactor := 0.0;
    Exit;
  end;

  with Renderer, Projector do
  begin
    with sRect do
    begin
      eSF := minFloat(Width / Right, Height / Bottom);
      if eSF > TG_MAXSCALEFACTOR then
        eSF := TG_MAXSCALEFACTOR;

      // Check an limit the zoom.
      if MaxZoom > MinZoom then
      begin
        if eSF < MinZoom then
          eSF := MinZoom;
        if eSF > MaxZoom then
          eSF := MaxZoom;
      end;

      // Calculate the new Origin
      iXOrg := Round((XOrigin / ScaleFactor - (Left + Right div 2)) * eSF) + Width div 2;
      iYOrg := Round((YOrigin / ScaleFactor - (Top + Bottom div 2)) * eSF) + Height div 2;
    end;

    if MaxVal(Width, Height) / eSF > Maxint then
      eSF := MaxVal(Width, Height) / Maxint;

    if eSF <> ScaleFactor then
    try
      FViewRect := Rect(0, 0, Round(Width / eSF), Round(Height / eSF));
      ScaleFactor := eSF;

      XOrigin := iXOrg;
      YOrigin := iYOrg;

      DoOnZoomed;
    except
    end;
  end;
  UpdateScrollBars;
end;

procedure TGlobe5.Subscribe(subscriber: IGlobeSubscriber);
begin
  FSubscribers.Add(subscriber);
end;

procedure TGlobe5.UpdateLayerTree;
var
  idx : integer;
  layer : TGLayer;
  tmpLayer : TGLayer;
begin
  if not FbUpdateTreeRequired then
    Exit;

  FbUpdateTreeRequired := false;

  BeginUpdate;

  if ( SpatialDatabase <> nil ) and SpatialDatabase.Active then
  begin
    DoOnProgress(pProgressStart, SpatialDatabase.SDBLayers.Count, 0.0);

    // Load any SDB layers
    tmpLayer := nil;
    for idx := 0 to SpatialDatabase.SDBLayers.Count - 1 do
    begin
      if tmpLayer = nil then
        tmpLayer := GlobeLayer;

      layer := tmpLayer.LayerStore.LayerByID( SpatialDatabase.SDBLayers[idx].LayerID );
      // restart from the top if we did not find the layer
      if ( layer = nil ) and ( tmpLayer <> GlobeLayer ) then
        layer := GlobeLayer.LayerStore.LayerByID( SpatialDatabase.SDBLayers[idx].LayerID );

      if layer = nil then // still did not find a layer so create a new object
      begin
        layer := TGLayer.Create( self );
        layer.Name := SpatialDatabase.SDBLayers[idx].Name;
        GlobeLayer.LayerStore.Add( layer );

        // Attach an object source from the SDB
        layer.ObjectSource := TGSDBObjectSource.Create( Self );
        layer.ObjectSource.Name := SpatialDatabase.SDBLayers[idx].Name;
        layer.ObjectSource.UniqueID := SpatialDatabase.SDBLayers[idx].LayerID;
      end;
      tmpLayer := layer.ParentLayer;

      if ( idx mod 50 ) = 0 then
        DoOnProgress(pProgress, idx, (idx / SpatialDatabase.SDBLayers.Count) * 100);
    end;

    // remove any orphaned SDB layers
    RemoveOrphanedLayers( GlobeLayer.LayerStore );

    DoOnProgress(pProgressEnd, 0, 100.0);
  end;

  FbUpdateTreeRequired := false;
  EndUpdate;

  GlobeNotify(gnLayerTreeUpdated);
  DoOnLayersChanged;
end;

procedure TGlobe5.UpdateScrollBars;
var
  ScrollInfo : TScrollInfo;
  eTmp, HalfPage, MinLimit, MaxLimit : Extended;
begin
  if FbRenderToCanvas or (Parent = nil) then
    Exit;

  ScrollInfo.cbSize := SizeOf(TScrollInfo);
  ScrollInfo.fMask := SIF_RANGE or SIF_POS or SIF_PAGE or SIF_DISABLENOSCROLL;

  with Projector do // Vertical
  begin
    if UseMinMaxLimits then
    begin
      MinLimit := MaxVal(Round(MinLatitude * GU_DEGREE), ExtentsLL.Top);
      MaxLimit := MinVal(Round(MaxLatitude * GU_DEGREE), ExtentsLL.Bottom);
    end
    else
    begin
      MinLimit := ExtentsLL.Top;
      MaxLimit := ExtentsLL.Bottom;
    end;

    ScrollInfo.nPage := MinVal(GU_180_DEGREE, FViewRect.Bottom - FViewRect.Top);
    HalfPage := ScrollInfo.nPage / 2;

    if MinLimit < -GU_90_DEGREE then
      ScrollInfo.nMin := -GU_90_DEGREE
    else
      ScrollInfo.nMin := Sign(MinLimit) * Round(MinFloat(MaxInt div 2, abs(MinLimit)));

    if MaxLimit > GU_90_DEGREE then
      ScrollInfo.nMax := GU_90_DEGREE
    else
      ScrollInfo.nMax := Sign(MaxLimit) * Round(MinFloat(MaxInt div 2, abs(MaxLimit)));

    eTmp := -(Renderer.Height div 2 - YOrigin) / ScaleFactor;
    eTmp := LimitFloat(eTmp, ScrollInfo.nMin + HalfPage, ScrollInfo.nMax - HalfPage);

    if UseMinMaxLimits then
      with ScrollInfo do
      begin
        if integer(nPage) > nMax - nMin then
        begin
          ScaleFactor := Renderer.Height / (nMax - nMin);
          Exit;
        end;

        YOrigin := (Renderer.Height div 2 + Round(eTmp * ScaleFactor));
      end;

      with ScrollInfo do
        nPos := nMax + nMin - Round(HalfPage + eTmp);

    if (FScrollBars = ssBoth) or (FScrollBars = ssVertical) then
      SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  end;

  with Projector do // Horizontal
  begin
    MinLimit := ExtentsLL.Left;
    MaxLimit := ExtentsLL.Right;

    if UseMinMaxLimits then
    begin
      MinLimit := MaxFloat(Round(MinLongitude * GU_DEGREE), MinLimit);
      MaxLimit := MinFloat(Round(MaxLongitude * GU_DEGREE), MaxLimit);
    end;

    ScrollInfo.nPage := MinVal(GU_360_DEGREE, FViewRect.Right - FViewRect.Left);
    HalfPage := ScrollInfo.nPage / 2;

    if MinLimit < -GU_180_DEGREE then
      ScrollInfo.nMin := -GU_180_DEGREE
    else
      ScrollInfo.nMin := Sign(MinLimit) * Round(MinFloat(MaxInt div 2, abs(MinLimit)));

    if MaxLimit > GU_180_DEGREE then
      ScrollInfo.nMax := GU_180_DEGREE
    else
      ScrollInfo.nMax := Sign(MaxLimit) * Round(MinFloat(MaxInt div 2, abs(MaxLimit)));

    // limit movement
    eTmp := (Renderer.Width div 2 - XOrigin) / ScaleFactor;
    eTmp := LimitFloat(eTmp, ScrollInfo.nMin + HalfPage, ScrollInfo.nMax - HalfPage);

    if UseMinMaxLimits then
      with ScrollInfo do
      begin
        if integer(nPage) > nMax - nMin then
        begin
          ScaleFactor := Renderer.Width / (nMax - nMin);
          Exit;
        end;

        XOrigin := Renderer.Width div 2 - Round(eTmp * ScaleFactor);
      end;

    ScrollInfo.nPos := -Round(HalfPage - eTmp);

    if (FScrollBars = ssBoth) or (FScrollBars = ssHorizontal) then
      SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
  end;
end;

procedure TGlobe5.UnSubscribe(subscriber: IGlobeSubscriber);
begin
  FSubscribers.Remove(subscriber);
end;

procedure TGlobe5.WMEraseBkgnd(var Msg : TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TGlobe5.WMHScroll(var Msg : TWMHScroll);
var
  ScrollInfo : TScrollInfo;
begin
  if Msg.ScrollBar = 0 then
  begin
    ScrollInfo.cbSize := SizeOf(TScrollInfo);
    ScrollInfo.fMask := SIF_ALL;
    GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
    with ScrollInfo do
      case Msg.ScrollCode of
        SB_LINEUP : PanViewXY(-integer(nPage div 20), 0);
        SB_LINEDOWN : PanViewXY(nPage div 20, 0);
        SB_PAGEUP : PanViewXY(-integer(nPage div 2), 0);
        SB_PAGEDOWN : PanViewXY(nPage div 2, 0);
        SB_THUMBTRACK : PanViewXY(nTrackPos - nPos, 0);
      end;
    if PeriodicUpdateTime > 0 then
      RestartRender;
  end
  else
    inherited;
end;

procedure TGlobe5.WMVScroll(var Msg : TWMVScroll);
var
  ScrollInfo : TScrollInfo;
begin
  if Msg.ScrollBar = 0 then
  begin
    ScrollInfo.cbSize := SizeOf(TScrollInfo);
    ScrollInfo.fMask := SIF_ALL;
    GetScrollInfo(Handle, SB_VERT, ScrollInfo);
    with ScrollInfo do
      case Msg.ScrollCode of
        SB_LINEUP : PanViewXY(0, -integer(nPage div 20));
        SB_LINEDOWN : PanViewXY(0, nPage div 20);
        SB_PAGEUP : PanViewXY(0, -integer(nPage div 2));
        SB_PAGEDOWN : PanViewXY(0, nPage div 2);
        SB_THUMBTRACK : PanViewXY(0, nTrackPos - nPos);
      end;
    if PeriodicUpdateTime > 0 then
      RestartRender;
  end
  else
    inherited;
end;

{$IFDEF TG_PROPERTY_EDITORS}
type
  TBMPFilenameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes : TPropertyAttributes; override;
  end;
  TEnvFilenameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes : TPropertyAttributes; override;
  end;
  TProjectionClassProperty = class(TStringProperty)
  public
    function GetAttributes : TPropertyAttributes; override;
    procedure GetValues(Proc : TGetStrProc); override;
  end;

procedure TBMPFilenameProperty.Edit;
var
  FileOpen : TOpenDialog;
begin
  FileOpen := TOpenDialog.Create(Application);
  FileOpen.Filename := GetValue;
  FileOpen.Filter := 'Bitmap files (*.bmp)|*.bmp|All files (*.*)|*.*';
  FileOpen.Options := FileOpen.Options + [ofShowHelp, ofPathMustExist,
    ofFileMustExist];
  try
    if FileOpen.Execute then SetValue(FileOpen.Filename);
  finally
    FileOpen.Free;
  end;
end;

function TBMPFilenameProperty.GetAttributes : TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

procedure TEnvFilenameProperty.Edit;
var
  FileOpen : TOpenDialog;
begin
  FileOpen := TOpenDialog.Create(Application);
  FileOpen.Filename := GetValue;
  FileOpen.Filter := 'Environment files (*.env)|*.env|All files (*.*)|*.*';
  FileOpen.Options := FileOpen.Options + [ofShowHelp, ofPathMustExist,
    ofFileMustExist];
  try
    if FileOpen.Execute then SetValue(FileOpen.Filename);
  finally
    FileOpen.Free;
  end;
end;

function TEnvFilenameProperty.GetAttributes : TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

function TProjectionClassProperty.GetAttributes : TPropertyAttributes;
begin
  Result := [paValueList, paRevertable];
end;

procedure TProjectionClassProperty.GetValues(Proc : TGetStrProc);
begin
  inherited;
  Proc('TGSphericalPrj');
  Proc('TGMercatorPrj');
  Proc('TGCartesianPrj');
end;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('TGlobe', [TGlobe5]);

{$IFDEF TG_PROPERTY_EDITORS}
  RegisterPropertyEditor(TypeInfo(TFileName), TGlobe5, 'EnvironmentFile', TEnvFileNameProperty);
  RegisterPropertyEditor(TypeInfo(TFileName), TGlobe5, 'SurfaceTextureName', TBMPFileNameProperty);
  RegisterPropertyEditor(TypeInfo(TProjectionClass), TGlobeProjection, 'ProjectionClass', TProjectionClassProperty);
{$ENDIF}
end;

end.

