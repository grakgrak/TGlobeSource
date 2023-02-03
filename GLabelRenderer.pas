//-----------------------------------------------------------------------
//Summary
//TGlobe Label Rendering class
//
//Description
//Manages the set of labels for all layers which need to be rendered to
//the globe. The labels are rendered after the layer data has been rendered.
//
//Author
//Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------

unit GLabelRenderer;

interface

Uses Windows, Classes, Forms, SysUtils, Graphics, GSysUtils, GClasses;

type
  TGTitleData = record
    objectPK : TGPrimaryKey;
    centroid : TGPointLL;
    selected : Boolean;
    title : String;
    titleRect : TRect;
    titleFont : TGFont;
  end;

  TGLayerTitles = record
    layer : TGLayer;
    titleData : array of TGTitleData;
    titleCount : integer;
  end;

  TGLabelRenderer = class(TGComponent)
  private
    FLayerTitles : array of TGLayerTitles;
  public
    procedure Clear; virtual;
    procedure AddLayer( aLayer : TGLayer ); virtual;
    procedure AddLabel( mapObj : IGMapPoint; const text : AnsiString;
      aFont : TGFont; alignment : TGTitleAlignment; offset : integer; offsetUnits : TGUnitTypes );
    procedure Render; virtual;
    function ObjectAtLL( ptLL : TGPointLL ) : IGMapPoint;
    function SelectObjects( aMER : TGMER; firstOnly, state : Boolean ) : integer;
  end;
implementation

Uses Globe5;


procedure TGLabelRenderer.AddLabel(mapObj: IGMapPoint;  const text: AnsiString;
  aFont : TGFont; alignment : TGTitleAlignment; offset : integer; offsetUnits : TGUnitTypes );
var
  ptXY : TPoint;
begin
  Assert( Length(FLayerTitles) > 0, 'No Layers added to LabelRenderer' );

  if ( text = '' ) or ( alignment = taNone ) then
    Exit;

  with ParentGlobe.Renderer do
    if FontRenderSize(aFont.Size, aFont.SizeUnit) < MaxVal(2, GetMinTextSize) then
      Exit;

  if ParentGlobe.Projector.LLToDeviceXY(mapObj.Centroid.iLongX, mapObj.Centroid.iLatY, ptXY) then
    with FLayerTitles[High(FLayerTitles)] do
    begin
      if titleCount >= High(titleData) then
        SetLength( titleData, Length( titleData ) + 512 );

      with titleData[titleCount] do
      begin
        objectPK := mapObj.PrimaryKey;
        centroid := mapObj.Centroid;
        selected := mapObj.Selected;
        title := StringReplace( text, '\n', #13#10, [rfReplaceAll]);
        titleFont := aFont;

        ParentGlobe.Renderer.Font := titleFont;

        titleRect := ParentGlobe.Renderer.GetTextRect(
          ptXY.X, ptXY.Y,
          title,
          alignment,
          TGlobe5(ParentGlobe).ScaleUnitsToDevice( offset, offsetUnits ));
      end;

      Inc( titleCount );
    end;
end;


procedure TGLabelRenderer.AddLayer(aLayer: TGLayer);
begin
  // ignore the layer if there are no title on it.
  if ( Length( FLayerTitles ) = 0 ) or (FLayerTitles[High( FLayerTitles )].titleCount > 0) then
    SetLength( FLayerTitles, Length(FLayerTitles) + 1 );
    
  FLayerTitles[High( FLayerTitles )].layer := aLayer;
end;

procedure TGLabelRenderer.Clear;
var
  idx : integer;
begin
  for idx := 0 to High( FLayerTitles ) do
    SetLength( FLayerTitles[idx].titleData, 0 );
    
  SetLength( FLayerTitles, 0 );
end;

function TGLabelRenderer.ObjectAtLL(ptLL : TGPointLL): IGMapPoint;
var
  idx, jdx : integer;
  ptXY : TPoint;
begin
  if ParentGlobe.Projector.LLToDeviceXY(ptLL.iLongX,ptLL.iLatY, ptXY ) then
    for idx := 0 to High( FLayerTitles ) do
      with FLayerTitles[idx] do
        if layer.Visible and layer.Enabled then
          for jdx := 0 to titleCount - 1 do
            with titleData[jdx] do
              if PtInRect( titleRect, ptXY ) then
              begin
                Result := FLayerTitles[idx].layer.ObjectSource.ByPrimaryKey(objectPK);
                Exit;
              end;
  Result := nil;
end;

procedure TGLabelRenderer.Render;
var
  count, idx, jdx : integer;
begin
  count := 0;

  for idx := 0 to High( FLayerTitles ) do
    with FLayerTitles[idx] do
      for jdx := 0 to titleCount - 1 do
        with titleData[jdx] do
        begin
          with ParentGlobe.Renderer do
          begin
            Font := titleFont;
            if selected then
              Font.Color := SelectedPen.Color;

            DrawTextRect( titleRect, title );
          end;

          Inc( count );
          if ( count mod 256 ) = 0 then
          begin
            TGlobe5(ParentGlobe).PeriodicUpdateRender;

            if TGlobe5(ParentGlobe).IsRenderRestarted or Application.Terminated then
              Exit;
          end;
        end;
end;

function TGLabelRenderer.SelectObjects(aMER: TGMER; firstOnly, state: Boolean): integer;
var
  obj : IGMapPoint;
begin
  Result := 0;

  // Todo: Should search for all titles that intersect the MER
  obj := ObjectAtLL(MER_CenterLL(aMER));
  if obj <> nil then
  begin
    obj.Selected := state;
    Inc( Result );
  end;
end;

end.
