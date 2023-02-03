//------------------------------------------------------------------------------
//Summary
//TGlobe Editors for map Objects
//
//Description
//Provides an implementation of map object editors.
//
//Author
//Graham Knight (tglobe@tglobe.com)
//------------------------------------------------------------------------------
unit GMapObjectEditors;

interface

Uses Windows, Controls, Classes, Graphics, SysUtils, Menus,
  Globe5, GClasses, GSysUtils, GMapObjects, Globe5Utils;

const
  HANDLE_RADIUS = 3;  // radius of editing nodes in pixels

type
  TGObjectEditor = class(TGRoot)
  private
    FOnMouseUp: TMouseEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
  protected
    FGlobe : TGlobe5;
    FMapObject: IGMapPoint;
    FCopyObject: IGMapPoint;

    FOldSelectedState : Boolean;
    FOldMouseDown : TMouseEvent;
    FOldMouseMove : TMouseMoveEvent;
    FOldMouseUp : TMouseEvent;
    FOldObjectRender : TGObjectRenderEvent;


    function NodeRect( const ptXY : TPoint; radius : integer ) : TRect;
    procedure AdjustCentroid( const ptXY : TPoint; const ptLL : TGPointLL );

    procedure BeginEdit( mapObject : IGMapPoint );
    procedure EndEdit;

    procedure ObjectRender(Sender: TGlobe5; GLayer: TGLayer; mapObject: IGMapPoint; State: TGRenderStateSet); virtual; abstract;
  public
    constructor Create( globe : TGlobe5 ); virtual;
    destructor Destroy; override;

    function EditObject( mapObject : IGMapPoint ) : Boolean; virtual; abstract;
    procedure CancelEdit;

    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    property Globe : TGlobe5 read FGlobe write FGlobe;
    property OnMouseDown : TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove : TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp : TMouseEvent read FOnMouseUp write FOnMouseUp;
  end;
  TGObjectEditorClass = class of TGObjectEditor;

  TGMapPointEditor = class(TGObjectEditor)
  private
    FMousePt : TPoint;
    FMoveObject : Boolean;
  protected
    procedure ObjectRender(Sender: TGlobe5; GLayer: TGLayer; mapObject: IGMapPoint; State: TGRenderStateSet); override;
  public
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function EditObject( mapObject : IGMapPoint ) : Boolean; override;
  end;

  TGMapPolyEditor = class(TGObjectEditor)
  private
    chainIndex : integer;
    nodeIndex : integer;
    FMousePt : TPoint;
    FMoveObject : Boolean;
    menu : TPopupMenu;
  protected
    procedure ObjectRender(Sender: TGlobe5; GLayer: TGLayer; mapObject: IGMapPoint; State: TGRenderStateSet); override;

    procedure PopupLocalMenu;
    procedure AddNode(Sender:TObject);
    procedure DeleteNode(Sender:TObject);
    function NodeHitAt( X, Y : integer ): Boolean;
  public
    constructor Create( globe : TGlobe5 ); override;
    destructor Destroy; override;

    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function EditObject( mapObject : IGMapPoint ) : Boolean; override;
  end;

  TGRasterEditorState = (reNone, reMoveImage, reTransparentColor,
    reSizeTL, reSizeTR, reSizeBR, reSizeBL );

  TGMapRasterEditor = class(TGObjectEditor)
  private
    FControlPairIndex : integer;
    FNodeIndex : integer;
    FMousePt : TPoint;
    FState : TGRasterEditorState;

    FAddControls : integer;
    startLL : TGPointLL;
    menu : TPopupMenu;
    procedure AddControlLineClick( Sender: TObject );
    procedure RemoveControlLinesClick( Sender: TObject );
    procedure SelectTransparentClick( Sender: TObject );
  protected
    procedure PopupLocalMenu;
    procedure ObjectRender(Sender: TGlobe5; GLayer: TGLayer; mapObject: IGMapPoint; State: TGRenderStateSet); override;
  public
    constructor Create( globe : TGlobe5 ); override;
    destructor Destroy; override;

    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function EditObject( mapObject : IGMapPoint ) : Boolean; override;
  end;

  // dummy class to share the Raster editor
  TGMapMRRasterEditor = class(TGMapRasterEditor)
  end;

function CreateMapObjectEditor( globe : TGlobe5; mapObject : IGMapPoint ) : TGObjectEditor;

implementation

uses GMorphing;

function CreateMapObjectEditor( globe : TGlobe5; mapObject : IGMapPoint ) : TGObjectEditor;
var
  editorClass : TGObjectEditorClass;
begin
  Result := nil;

  editorClass := TGObjectEditorClass( GetClass( mapObject.ObjectClassName + 'Editor' ));

  if editorClass <> nil then
  begin
    Result := editorClass.Create( Globe );

    if not Result.EditObject( mapObject ) then
      FreeAndNil( Result );
  end;
end;


procedure TGObjectEditor.AdjustCentroid(const ptXY : TPoint; const ptLL: TGPointLL);
var
  oldPtLL : TGPointLL;
begin
  FGlobe.Projector.DeviceXYToLL(ptXY.X, ptXY.Y, oldPtLL );

  with FMapObject do
    Centroid := PointLL(
        Centroid.iLongX + LongDiff( oldPtLL.iLongX, ptLL.iLongX ),
        Centroid.iLatY + (ptLL.iLatY - oldPtLL.iLatY ));

  FMapObject.Invalidate;
  FGlobe.RedrawLayers;
end;

procedure TGObjectEditor.BeginEdit( mapObject : IGMapPoint );
begin
  Assert( mapObject <> nil, 'BeginEdit passed nil object' );

  FOldSelectedState := mapObject.Selected;
  FCopyObject := mapObject.Clone; // Copy the object

  FMapObject := mapObject;
  FMapObject.Edit;
  FMapObject.Selected := false;

  // save the mouse events and hook in the local mouse handlers
  FOldMouseDown := FGlobe.OnMouseDown;
  FOldMouseMove := FGlobe.OnMouseMove;
  FOldMouseUp := FGlobe.OnMouseUp;
  FOldObjectRender := FGlobe.OnObjectRender;

  FGlobe.OnMouseDown := MouseDown;
  FGlobe.OnMouseMove := MouseMove;
  FGlobe.OnMouseUp := MouseUp;
  FGlobe.OnObjectRender := ObjectRender;

  FGlobe.RedrawLayers;
end;

procedure TGObjectEditor.CancelEdit;
begin
  // restore the object to it's original state
  FMapObject.CancelEdit;
  FMapObject.Assign( FCopyObject );
  FMapObject.Selected := FOldSelectedState;

  FCopyObject := nil;
end;

constructor TGObjectEditor.Create(globe: TGlobe5);
begin
  inherited Create;

  FGlobe := globe;
end;

destructor TGObjectEditor.Destroy;
begin
  EndEdit;

  inherited;
end;

procedure TGObjectEditor.EndEdit;
begin
  if FMapObject <> nil then
  begin
    FMapObject.Post; // Save the changes
    FMapObject.Selected := FOldSelectedState;
  end;

  FMapObject := nil;
  FCopyObject := nil;

  // restore the mouse events to the globe.
  FGlobe.OnMouseDown := FOldMouseDown;
  FGlobe.OnMouseMove := FOldMouseMove;
  FGlobe.OnMouseUp := FOldMouseUp;
  FGlobe.OnObjectRender := FOldObjectRender;

  FGlobe.RedrawLayers;
end;

procedure TGObjectEditor.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned( FOnMouseDown ) then
    FOnMouseDown(Sender, Button, Shift, X, Y );
end;

procedure TGObjectEditor.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned( FOnMouseMove ) then
    FOnMouseMove(Sender, Shift, X, Y );
end;

procedure TGObjectEditor.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned( FOnMouseUp ) then
    FOnMouseUp(Sender, Button, Shift, X, Y );
end;

function TGObjectEditor.NodeRect( const ptXY: TPoint; radius : integer): TRect;
begin
  Result := Rect( ptXY.X - radius, ptXY.Y - radius, ptXY.X + radius + 1, ptXY.Y + radius + 1 );
end;

{ TGMapPolyEditor }

procedure TGMapPolyEditor.AddNode(Sender: TObject);
var
  ptLL : TGPointLL;
  idx, jdx : integer;
begin
  if FGlobe.Projector.DeviceXYToLL( FMousePt.X, FMousePt.Y, ptLL ) then
    with (FMapObject as IGMapPoly) do
      for idx := 0 to Chains.Count - 1 do
        if not Chains[idx].Hidden then
        begin
          jdx := Chains[idx].PointOnEdge(ptLL.iLongX, ptLL.iLatY, Round(4/FGlobe.Projector.ScaleFactor), Closed);
          if jdx >= 0 then
          begin
            Chains[idx].Insert(jdx,ptLL);
            FMapObject.Invalidate;
            FGlobe.RedrawLayers;
            Exit;
          end;
        end;
end;

constructor TGMapPolyEditor.Create( globe : TGlobe5 );
var
  item : TMenuItem;
begin
  inherited;

  menu := TPopupMenu.Create(nil);

  item := TMenuItem.Create(menu);
  item.Caption := 'Add Node';
  item.OnClick := AddNode;
  menu.Items.Add(item);

  item := TMenuItem.Create(menu);
  item.Caption := 'Delete Node';
  item.OnClick := DeleteNode;
  menu.Items.Add(item);
end;

procedure TGMapPolyEditor.DeleteNode(Sender: TObject);
begin
  with (FMapObject as IGMapPoly) do
    Chains[chainIndex].Delete(nodeIndex);
    
  chainIndex := -1;
  nodeIndex := -1;
  
  FMapObject.Invalidate;
  FGlobe.RedrawLayers;
end;

destructor TGMapPolyEditor.Destroy;
begin
  FreeAndNil( menu );
  
  inherited;
end;

function TGMapPolyEditor.EditObject(mapObject: IGMapPoint): Boolean;
begin
  chainIndex := -1;

  Result := ( mapObject <> nil ) and Supports( mapObject, IGMapPoly );

  if Result then
    BeginEdit( mapObject );
end;

procedure TGMapPolyEditor.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not ( ssCtrl in Shift ) then
    Exit;
  if Button <> mbLeft then
    Exit;

  if NodeHitAt( X, Y ) then
    Exit;

  if FGlobe.GlobeLayer.ObjectAtXY(X,Y) = FMapObject then
  begin
    FMoveObject := true;
    FMousePt := Point( X, Y );
  end;
end;

procedure TGMapPolyEditor.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  ptLL : TGPointLL;
begin
  inherited;
  if not ( ssCtrl in Shift ) then
    Exit;

  if not FGlobe.Projector.DeviceXYToLL(X,Y, ptLL) then
    Exit;

  if chainIndex >= 0 then
  begin
    with FMapObject as IGMapPoly do
      Chains[chainIndex].AsLL[nodeIndex] := ptLL;
    FMapObject.Invalidate;
    FGlobe.RedrawLayers;
  end;

  if FMoveObject then
  begin
    AdjustCentroid( FMousePt, ptLL );
    FMousePt := Point( X, Y );
  end;
end;

procedure TGMapPolyEditor.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ptLL : TGPointLL;
begin
  inherited;
  chainIndex := -1;
  FMoveObject := false;
  FMousePt := Point(X,Y);

  if ( Button = mbRight ) and ( ssCtrl in Shift ) then
    if FGlobe.Projector.DeviceXYToLL(X, Y, ptLL) then
      if (FMapObject as IGMapPoly).LLOnEdge(ptll, FGlobe.Projector.PixelTolerance, (FMapObject as IGMapPoly).Closed ) then
        PopupLocalMenu;
end;

function TGMapPolyEditor.NodeHitAt(X, Y: integer): Boolean;
var
  ptXY : TPoint;
  idx, jdx : integer;
begin
  with FMapObject as IGMapPoly do
    for idx := 0 to Chains.Count - 1 do
      if not Chains[idx].Hidden then
        for jdx := 0 to Chains[idx].Count - 1 do
          with Chains[idx].AsLL[jdx] do
            if FGlobe.Projector.LLToDeviceXY(iLongX, iLatY, ptXY ) then
              if PtInRect( NodeRect(ptXY,HANDLE_RADIUS), Point(X,Y)) then
              begin
                chainIndex := idx;
                nodeIndex := jdx;
                Result := true;
                Exit;
              end;

  chainIndex := -1;
  nodeIndex := -1;
  Result := false;
end;

procedure TGMapPolyEditor.ObjectRender(Sender: TGlobe5; GLayer: TGLayer;
  mapObject: IGMapPoint; State: TGRenderStateSet);
var
  idx, jdx : integer;
  ptXY : TPoint;
begin
  if mapObject <> FMapObject then
    Exit;

  FGlobe.Renderer.Canvas.Pen.Color := clRed;

  with FMapObject as IGMapPoly do
    for idx := 0 to Chains.Count - 1 do
      if not Chains[idx].Hidden then
        for jdx := 0 to Chains[idx].Count - 1 do
          with Chains[idx].AsLL[jdx] do
            if FGlobe.Projector.LLToDeviceXY(iLongX, iLatY, ptXY ) then
              FGlobe.Renderer.Canvas.Ellipse( NodeRect( ptXY, HANDLE_RADIUS ));

  Globe.Renderer.Pen.Color := clRed;
  DrawProjectedMER(FGlobe, FMapObject.ObjectMER);
end;

procedure TGMapPolyEditor.PopupLocalMenu;
var
  item : TMenuItem;
  ptLL : TGPointLL;
begin
  if not FGlobe.Projector.DeviceXYToLL( FMousePt.X, FMousePt.Y, ptLL ) then
    Exit;

  item := menu.Items.Find('Add Node');
  item.Enabled := (FMapObject as IGMapPoly).LLOnEdge( ptLL, FGlobe.Projector.PixelTolerance, (FMapObject as IGMapPoly).Closed );

  item := menu.Items.Find('Delete Node');
  item.Enabled := NodeHitAt( FMousePt.X, FMousePt.Y );

  with FGlobe.ClientToScreen(FMousePt) do
    menu.Popup(X,Y);
end;


{ TGMapRasterEditor }

procedure TGMapRasterEditor.AddControlLineClick(Sender: TObject);
var
  idx : integer;
begin

  with ( FMapObject as IGMapRaster).GetMorph do
    for idx := 0 to Count - 1 do
    begin
      ControlPairs[idx].WorldStart := XYToWorld(ControlPairs[idx].ImageStart);
      ControlPairs[idx].WorldEnd := XYToWorld(ControlPairs[idx].ImageEnd);
    end;

  FAddControls := 1;  // stage 1 in adding controls
  FGlobe.RedrawLayers;
end;

constructor TGMapRasterEditor.Create(globe: TGlobe5);
var
  item : TMenuItem;
begin
  inherited;

  menu := TPopupMenu.Create(nil);

  item := TMenuItem.Create(menu);
  item.Caption := 'Add Control Line';
  item.OnClick := AddControlLineClick;
  menu.Items.Add(item);

  item := TMenuItem.Create(menu);
  item.Caption := 'Remove Control Lines';
  item.OnClick := RemoveControlLinesClick;
  menu.Items.Add(item);

  item := TMenuItem.Create(menu);
  item.Caption := 'Select Transparent Color';
  item.OnClick := SelectTransparentClick;
  menu.Items.Add(item);
end;

destructor TGMapRasterEditor.Destroy;
begin
  FreeAndNil( menu );

  inherited;
end;

function TGMapRasterEditor.EditObject(mapObject: IGMapPoint) : Boolean;
begin
  Result := ( mapObject <> nil ) and Supports( mapObject, IGMapRaster );

  if Result then
    BeginEdit( mapObject );
end;

procedure TGMapRasterEditor.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  idx : integer;
  ptLL : TGPointLL;
  tolerance : double;
begin
  inherited;
  if not (( ssCtrl in Shift ) and (ssLeft in Shift ))then
    Exit;

  if FState = reTransparentColor then
  begin
   (FMapObject as IGMapRaster).TransparentColor := Globe.Renderer.Canvas.Pixels[X,Y];
   FGlobe.RedrawLayers;
   Exit;
  end;

  if FAddControls > 0 then
  begin
    if FGlobe.Projector.DeviceXYToLL(X, Y, ptLL) then
      Inc( FAddControls );

    if FAddControls = 2 then
    begin
      startLL := ptLL;
      FGlobe.RedrawLayers;
    end
    else
    begin
      idx := (FMapObject as IGMapRaster).GetMorph.Add;

      with (FMapObject as IGMapRaster).GetMorph.ControlPairs[idx] do
      begin
        WorldStart := startLL;
        WorldEnd := ptLL;
        ImageStart := (FMapObject as IGMapRaster).GetMorph.WorldToXY(startLL);
        ImageEnd := (FMapObject as IGMapRaster).GetMorph.WorldToXY(ptLL);
      end;
      FAddControls := 0;
      FGlobe.RedrawLayers;
    end;
  end;

  if FGlobe.Projector.DeviceXYToLL(X, Y, ptLL) then
  begin
    tolerance := HANDLE_RADIUS / FGlobe.Projector.ScaleFactor;

    for idx := 0 to (FMapObject as IGMapRaster).GetMorph.Count - 1 do
      with (FMapObject as IGMapRaster).GetMorph.ControlPairs[idx] do
      begin
        if Abs( Hypot( WorldStart.iLongX - ptLL.iLongX, WorldStart.iLatY - ptLL.iLatY )) <= tolerance then
        begin
          FControlPairIndex := idx;
          FNodeIndex := 3;
          Exit;
        end;

        if Abs( Hypot( WorldEnd.iLongX - ptLL.iLongX, WorldEnd.iLatY - ptLL.iLatY )) <= tolerance then
        begin
          FControlPairIndex := idx;
          FNodeIndex := 4;
          Exit;
        end;
      end;

    // Check for hit on a re-size handle
    if ( FMapObject as IGMapRaster).GetMorph.Count = 0 then
      with FMapObject do
      begin
//        if Abs( Hypot( iLongX - ptLL.iLongX, iTopY - ptLL.iLatY )) <= tolerance then
//          FState := reSizeTL;
        if Abs( Hypot( MER_iRightX(ObjectMER) - ptLL.iLongX, MER_iTopY(ObjectMER) - ptLL.iLatY )) <= tolerance then
          FState := reSizeTR;
//        if Abs( Hypot( iRightX - ptLL.iLongX, iLatY - ptLL.iLatY )) <= tolerance then
//          FState := reSizeBR;
//        if Abs( Hypot( iLongX - ptLL.iLongX, iLatY - ptLL.iLatY )) <= tolerance then
//          FState := reSizeBL;
      end;

    // move the whole object
    if FMapObject.ObjectSource.Layer.ObjectAtXY(X,Y) = FMapObject then
      FState := reMoveImage;

    FMousePt := Point( X, Y );
  end;
end;

procedure TGMapRasterEditor.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  ptLL : TGPointLL;
  imageW, imageH : double;
  scaleX, scaleY : Extended;
begin
  inherited;
  if not (( ssCtrl in Shift ) and (ssLeft in Shift ))then
    Exit;

  if not FGlobe.Projector.DeviceXYToLL(X,Y, ptLL) then
    Exit;

  if FNodeIndex <> 0 then
    with (FMapObject as IGMapRaster).GetMorph.ControlPairs[FControlPairIndex] do
    begin
      if FNodeIndex = 3 then
        WorldStart := ptLL
      else
        WorldEnd := ptLL;

      FMapObject.Invalidate;
      FGlobe.RedrawLayers;
      Exit;
    end;

  if FState = reMoveImage then
  begin
    AdjustCentroid( FMousePt, ptLL );

    FMousePt := Point( X, Y );
    Exit;
  end;

  with (FMapObject as IGMapRaster) do
  begin
//    scaleX := GetMorph.ScaleX;
//    scaleY := GetMorph.ScaleY;
    imageW := ImageWidth;
    imageH := ImageHeight;
  end;

  case FState of
  reSizeTL :
    begin
      scaleX := LongDiff( ptLL.iLongX, MER_iRightX(FMapObject.ObjectMER)) / imageW;
      scaleY := ( ptLL.iLatY - FMapObject.ObjectMER.iLatY ) / imageH;
      AdjustCentroid( FMousePt, ptLL );
    end;
  reSizeTR :
    begin
      scaleX := LongDiff( FMapObject.ObjectMER.iLongX, ptLL.iLongX ) / imageW;
      scaleY := ( ptLL.iLatY - FMapObject.ObjectMER.iLatY ) / imageH;
    end;
  reSizeBR :
    Exit;
  reSizeBL :
    Exit;
  else
    Exit;
  end;

  with (FMapObject as IGMapRaster) do
  begin
    GetMorph.ScaleX := scaleX;
    GetMorph.ScaleY := scaleY;
  end;

  FGlobe.RedrawLayers;
  FMousePt := Point( X, Y );
end;

procedure TGMapRasterEditor.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FNodeIndex := 0;
  FState := reNone;
  FMousePt := Point(X,Y);

  if ( Button = mbRight ) and ( ssCtrl in Shift ) then
    if FMapObject.ObjectSource.Layer.ObjectAtXY(X,Y) = FMapObject then
      PopupLocalMenu;
end;


procedure TGMapRasterEditor.ObjectRender(Sender: TGlobe5;
  GLayer: TGLayer; mapObject: IGMapPoint; State: TGRenderStateSet);
var
  idx : integer;
  startXY, endXY : TPoint;
begin
  if mapObject <> FMapObject then
    Exit;

  with FGlobe.Renderer.Canvas do
  begin
    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Brush.Color := clYellow;
    Refresh;

//    if FGlobe.Projector.LLToDeviceXY(FMapObject.Centroid.iLongX, FMapObject.Centroid.iLatY, startXY ) then
//      Ellipse( NodeRect( startXY, 5 ));

    if FAddControls = 2 then
      if FGlobe.Projector.LLToDeviceXY(startLL.iLongX, startLL.iLatY, startXY ) then
        Ellipse( NodeRect( startXY, HANDLE_RADIUS ));

    // Draw the control lines on the object
    with ( FMapObject as IGMapRaster).GetMorph do
      for idx := 0 to Count - 1 do
        with ControlPairs[idx] do
          if FGlobe.Projector.LLToDeviceXY(WorldStart.iLongX, WorldStart.iLatY, startXY ) then
            if FGlobe.Projector.LLToDeviceXY(WorldEnd.iLongX, WorldEnd.iLatY, endXY ) then
            begin
              Pen.Style := psDot;
              MoveTo( startXY.X, startXY.Y );
              LineTo( endXY.X, endXY.Y );
              Pen.Style := psSolid;
              Ellipse( NodeRect( startXY, HANDLE_RADIUS ));
              Ellipse( NodeRect( endXY, HANDLE_RADIUS ));
            end;

    // if no control lines defined allow the image to be sized.
    if ( FMapObject as IGMapRaster).GetMorph.Count = 0 then
      with FMapObject do
      begin
//        if FGlobe.Projector.LLToDeviceXY(iLongX, iTopY, startXY ) then
//          Ellipse( NodeRect( startXY, HANDLE_RADIUS ));
        if FGlobe.Projector.LLToDeviceXY(MER_iRightX(ObjectMER), MER_iTopY(ObjectMER), startXY ) then
          Ellipse( NodeRect( startXY, 4 ));
//        if FGlobe.Projector.LLToDeviceXY(iRightX, iLatY, startXY ) then
//          Ellipse( NodeRect( startXY, HANDLE_RADIUS ));
//        if FGlobe.Projector.LLToDeviceXY(iLongX, iLatY, startXY ) then
//          Ellipse( NodeRect( startXY, HANDLE_RADIUS ));
      end;
  end;
end;

procedure TGMapRasterEditor.PopupLocalMenu;
begin
  with FGlobe.ClientToScreen(FMousePt) do
    menu.Popup(X,Y);
end;

procedure TGMapRasterEditor.RemoveControlLinesClick(Sender: TObject);
begin
  ( FMapObject as IGMapRaster).GetMorph.Clear;
  FGlobe.RedrawLayers;
end;

procedure TGMapRasterEditor.SelectTransparentClick(Sender: TObject);
begin
  FState := reTransparentColor;
end;



function TGMapPointEditor.EditObject(mapObject: IGMapPoint): Boolean;
begin
  Result := mapObject <> nil;

  if Result then
    BeginEdit( mapObject );
end;

procedure TGMapPointEditor.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not ( ssCtrl in Shift ) then
    Exit;
  if Button <> mbLeft then
    Exit;

  if FGlobe.GlobeLayer.ObjectAtXY(X,Y) = FMapObject then
  begin
    FMoveObject := true;
    FMousePt := Point( X, Y );
  end;
end;

procedure TGMapPointEditor.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  ptLL : TGPointLL;
begin
  inherited;

  if not ( ssCtrl in Shift ) then
    Exit;

  if not FGlobe.Projector.DeviceXYToLL(X,Y, ptLL) then
    Exit;

  if FMoveObject then
  begin
    AdjustCentroid( FMousePt, ptLL );
    FMousePt := Point( X, Y );
  end;
end;

procedure TGMapPointEditor.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMoveObject := false;
end;

procedure TGMapPointEditor.ObjectRender(Sender: TGlobe5; GLayer: TGLayer;
  mapObject: IGMapPoint; State: TGRenderStateSet);
var
  ptXY : TPoint;
begin
  if mapObject <> FMapObject then
    Exit;

  FGlobe.Renderer.Canvas.Pen.Color := clRed;

  with mapObject.Centroid do
    if FGlobe.Projector.LLToDeviceXY(iLongX, iLatY, ptXY ) then
      FGlobe.Renderer.Canvas.Ellipse( NodeRect( ptXY, HANDLE_RADIUS ));
end;

initialization
  RegisterGlobeClass( TGMapPointEditor, 'Point Editor' );
  RegisterGlobeClass( TGMapPolyEditor, 'Poly Editor' );
  RegisterGlobeClass( TGMapRasterEditor, 'Raster Editor' );
  RegisterGlobeClass( TGMapMRRasterEditor, 'Raster Editor' );
end.
