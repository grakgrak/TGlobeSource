unit GLayerTreeView;

interface

uses
  Windows, Messages, Menus, SysUtils, Classes, Controls, ComCtrls,
  Globe5, GClasses;

const
  tiGlobe = 0;
  tiPresenterStore = 1;
  tiLayerStore = 2;
  tiLayer = 3;
  tiPresenter = 4;
  tiObjectSource = 5;

type
  TGLayerTreeOption = ( toHideLayerPresenters, toHideObjectSource, toHideObjectSourcePresenters, toHideGlobe );
  TGLayerTreeOptionSet = set of TGLayerTreeOption;

  TGLayerTreeView = class(TTreeView, IGlobeSubscriber)
  private
    { Private declarations }
    FGlobe: TGlobe5;
    FOptions: TGLayerTreeOptionSet;

    procedure SetGlobe(const Value: TGlobe5);
    procedure SetOptions(const Value: TGLayerTreeOptionSet);
  protected
    { Protected declarations }
    procedure GlobeNotification( Sender : TGlobe5; notification : TGlobeNotification );
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CanExpand(Node: TTreeNode): Boolean; override;
  public
    { Public declarations }
    destructor Destroy; override;
    procedure BuildTree;
    procedure RebuildNode( Node : TTreeNode );
    function NodeToLayerStore( Node : TTreeNode ) : TGLayerStore; 
  published
    { Published declarations }
    property Globe : TGlobe5 read FGlobe write SetGlobe;
    property Options : TGLayerTreeOptionSet read FOptions write SetOptions;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TGlobe', [TGLayerTreeView]);
end;

{ TGLayerTreeView }

procedure TGLayerTreeView.BuildTree;
var
  globeNode, node : TTreeNode;
begin
  if csDestroying in ComponentState then
    Exit;
    
  with Items do
  begin
    BeginUpdate;
    Clear;

    if Globe <> nil then
    begin
      // Add in the Globe node
      if not ( toHideGlobe in Options ) then
      begin
        globeNode := AddChildObject( nil, 'Globe', Globe);
        globeNode.ImageIndex := tiGlobe;
        globeNode.SelectedIndex := tiGlobe;
      end
      else
        globeNode := nil;

      // Add the GlobalLayer Presenters Key
      if not ( toHideLayerPresenters in Options ) then
      begin
        node := AddChildObject( globeNode, 'Presenters', Globe.GlobeLayer.Presenters);
        node.HasChildren := true;
        node.ImageIndex := tiPresenterStore;
        node.SelectedIndex := tiPresenterStore;
      end;

      // Add the Layer Store Key
      node := AddChildObject( globeNode, 'Layers', Globe.GlobeLayer.LayerStore );
      node.HasChildren := true;
      node.ImageIndex := tiLayerStore;
      node.SelectedIndex := tiLayerStore;

      node.Expand( False );
      node.Selected := true;
    end;

    EndUpdate;
  end;
end;

function TGLayerTreeView.CanExpand(Node: TTreeNode): Boolean;
begin
  if Node.Count = 0 then
    RebuildNode( Node );
    
  Result := inherited CanExpand( Node );
end;

destructor TGLayerTreeView.Destroy;
begin
  Globe := nil;

  inherited;
end;

procedure TGLayerTreeView.GlobeNotification(Sender: TGlobe5;
  notification: TGlobeNotification);
begin
  case notification of
    gnLayerTreeUpdated :
      BuildTree;
    gnDestroying :
      FGlobe := nil;
  end;
end;

function TGLayerTreeView.NodeToLayerStore(Node: TTreeNode): TGLayerStore;
begin
  Result := nil;

  if ( node <> nil ) then
  begin
    if ( TObject( node.Data ) is TGLayerStore ) then
      Result := TGLayerStore( node.Data )
    else
      if ( TObject( node.Data ) is TGLayer ) then
        Result := TGLayer( node.Data ).ParentLayer.LayerStore;
  end;
end;

procedure TGLayerTreeView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if ( AComponent = FGlobe ) and ( Operation = opRemove ) then
    FGlobe := nil;
end;

procedure TGLayerTreeView.RebuildNode( Node: TTreeNode );
var
  iTop, idx : integer;
  SubNode : TTreeNode;
  expanded : Boolean;
begin
  if Node = nil then
    Exit;
    
  if TopItem <> nil then
    iTop := TopItem.AbsoluteIndex
  else
    iTop := -1;

  expanded := Node.Expanded;
  Items.BeginUpdate;
  Node.DeleteChildren;

  if TObject( Node.Data ) is TGLayerStore then
    with TGLayerStore( Node.Data ) do
      for idx := 0 to Count - 1 do
      begin
        SubNode := Items.AddChildObject( Node, Layers[idx].Name, Layers[idx]);
        SubNode.HasChildren := True;
        SubNode.ImageIndex := tiLayer;
        SubNode.SelectedIndex := tiLayer;
      end;

  if TObject( Node.Data ) is TGLayer then
    with TGLayer( Node.Data ) do
    begin
      if not ( toHideLayerPresenters in Options ) then
      begin
        SubNode := Items.AddChildObject( Node, 'Presenters', Presenters );
        SubNode.HasChildren := True;
        SubNode.ImageIndex := tiPresenterStore;
        SubNode.SelectedIndex := tiPresenterStore;
      end;

      if not ( toHideObjectSource in Options ) then
      begin
        SubNode := Items.AddChildObject( Node, 'ObjectSource', ObjectSource);
        SubNode.HasChildren := True;
        SubNode.ImageIndex := tiObjectSource;
        SubNode.SelectedIndex := tiObjectSource;
      end;

      SubNode := Items.AddChildObject( Node, 'Layers', LayerStore );
      SubNode.HasChildren := True;
      SubNode.ImageIndex := tiLayerStore;
      SubNode.SelectedIndex := tiLayerStore;
    end;

  if TObject( Node.Data ) is TGObjectSource then
    if not ( toHideObjectSourcePresenters in Options ) then
      with TGObjectSource( Node.Data ) do
      begin
        SubNode := Items.AddChildObject( Node, 'Presenters', Presenters );
        SubNode.HasChildren := True;
        SubNode.ImageIndex := tiPresenterStore;
        SubNode.SelectedIndex := tiPresenterStore;
      end;

  if TObject( Node.Data ) is TGPresenterStore then
    with TGPresenterStore( Node.Data ) do
      for idx := 0 to Count - 1 do
      begin
        SubNode := Items.AddChildObject( Node, Presenters[idx].Name, Presenters[idx] );
        SubNode.ImageIndex := tiPresenter;
        SubNode.SelectedIndex := tiPresenter;
      end;

  Node.Expanded := expanded;

  if expanded then
    while Node.Parent <> nil do
    begin
      Node := Node.Parent;
      Node.Expanded := true;
    end;

  Items.EndUpdate;

  if ( iTop >= 0 ) and ( iTop < Items.Count ) then
    TopItem := Items.Item[iTop];
end;

procedure TGLayerTreeView.SetGlobe(const Value: TGlobe5);
begin
  if FGlobe <> Value then
  begin
    if FGlobe <> nil then
      FGlobe.Unsubscribe( Self );

    FGlobe := Value;

    if FGlobe <> nil then
      FGlobe.Subscribe( Self );

    BuildTree;
  end;
end;

procedure TGLayerTreeView.SetOptions(const Value: TGLayerTreeOptionSet);
begin
  FOptions := Value;
  BuildTree;
end;


end.
