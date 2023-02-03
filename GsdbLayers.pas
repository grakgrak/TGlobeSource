//-----------------------------------------------------------------------
// Summary
//	TGlobe Spatial Database Layer classes
//
// Description
// 	Provides contains for persisted layer in the spatial database.
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
{$I GLOBE5.INC}
unit GSDBLayers;

interface

Uses  ActiveX, Windows, Classes, SysUtils, Contnrs, GSDBCache, GSDBTables, GSDBBtree,
  GSysUtils, GXML, GSDBIndex, GSDBKeyIndex;

const
  SDBLAYER_MAGIC = $4C520000;

type
  TGSDBLayerList = class;

  //---------------------------------------------------------------------
  //Description
  //Interface for observers which are interested in changes to SDBLayers.
  //---------------------------------------------------------------------
  IGLayersObserver = interface
    procedure OnLayersNotification( Sender: TGSDBLayerList );
  end;

  {---------------------------- TGSDBLayer ------------------------------------}
  // The LayerTable contains the following record type
  TGSDBLayerHeader = packed record
    Magic : DWORD;
    LayerID : TGUID;
    Spare : Cardinal;
  end;


  TGSDBLayer = class( TGSDBObject )
  private
    FHeader : TGSDBLayerHeader;
    FMetaDataModified : Boolean;
    FMetaData : TStringList;
    FLayerList: TGSDBLayerList;
    FBlobIndex : TStringList;

    FAttributeTable : TGSDBTable;
    FBlobTable : TGSDBTable;
    FDataTable : TGSDBTable;
    FObjectTable : TGSDBTable;

    FSpatialIndex : TGSpatialIndex;
    FBTreeIndex : TGBtreeIndex;
    FBulkLoad: Boolean;

    function GetPageCache: TGPageCache;
    function GetMetaData: TStringList;
    function GetLayerID: TGUID;
    function GetObjectCount: Cardinal;
    procedure MetaDataChanged( Sender : TObject );
    procedure SetLayerID(const Value: TGUID);
  protected
    procedure InternalLoad( aStream : TMemoryStream ); override;
    procedure InternalSave( aStream : TMemoryStream ); override;
  public
    destructor Destroy; override;

    procedure Flush; override;
    procedure Clear; override;

    procedure AssignProperties( DBLayer : TGSDBLayer );
    procedure SaveToXML( el : TGXML_Element ); override;

    function AttributeTable : TGSDBTable;
    function BlobTable : TGSDBTable;
    function DataTable : TGSDBTable;
    function ObjectTable : TGSDBTable;

    function BlobIndex : TStringList;
    function BTreeIndex : TGBtreeIndex;
    function SpatialIndex : TGSpatialIndex;
    procedure RebuildSpatialIndex;

    property BulkLoad : Boolean read FBulkLoad write FBulkLoad;
    property LayerID : TGUID read GetLayerID write SetLayerID;
    property LayerList : TGSDBLayerList read FLayerList write FLayerList;
    property MetaData : TStringList read GetMetaData;
    property ObjectCount : Cardinal read GetObjectCount;
    property PageCache : TGPageCache read GetPageCache;
  end;

  TGSDBLayerList = class(TGNoRefCountObject,IGPageCacheObserver)
  private
    FSysRoot : TGSDBSysRoot;
    FSDBLayers : TObjectList;
    FLayerPropertyIndex: TGSDBKeyIndex;
    FSubscribers : TInterfaceList;
    FUpdateCount : integer;
    function GetSDBLayer(index: integer): TGSDBLayer;
    function GetCount: integer;
    procedure OnPageCacheNotification( Sender : TGPageCache; notification : TGPageCacheNotification );
    procedure Notify;
  public
    constructor Create( SysRoot : TGSDBSysRoot );
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    procedure Subscribe( observer : IGLayersObserver );
    procedure UnSubscribe( observer : IGLayersObserver );

    procedure Flush;
    procedure Clear;
    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;

    procedure SaveToXML(el: TGXML_Element);

    procedure AddLayer( sdbLayer : TGSDBLayer );
    procedure DeleteLayer( sdbLayer : TGSDBLayer );
    procedure CopyLayer( sdbLayer : TGSDBLayer; const sName : String );
    function NewLayer( const layerName : String ) : TGSDBLayer; overload;
    function NewLayer( const layerName : String; const ID : TGUID ) : TGSDBLayer; overload;
    function LayerByName( const layerName : String ) : TGSDBLayer;
    function LayerByID( const ID : TGUID ) : TGSDBLayer;
    function LayerPropertyIndex: TGSDBKeyIndex;

    property Count : integer read GetCount;
    property SDBLayer[index:integer] : TGSDBLayer read GetSDBLayer; default;
    property SysRoot : TGSDBSysRoot read FSysRoot;
  end;

implementation

Uses GSDBDataLink, GLogging, GSDBRTree;



procedure TGSDBLayer.AssignProperties(DBLayer: TGSDBLayer);
begin
  Name := DBLayer.Name;
  MetaData.CommaText := DBLayer.MetaData.CommaText;
  FMetaDataModified := true;
end;

function TGSDBLayer.AttributeTable: TGSDBTable;
begin
  if FAttributeTable = nil then
  begin
    FAttributeTable := SysRoot.CreateObjectByName( TGSDBTable, GUIDToString(LayerID) + '.Data' ) as TGSDBTable;

    if FAttributeTable = nil then
    begin
      FAttributeTable := TGSDBTable.Create( SysRoot, GUIDToString(LayerID) + '.Data', '' );
      SysRoot.SaveObject( FAttributeTable );
    end;
  end;

  Result := FAttributeTable;
end;

function TGSDBLayer.BlobIndex: TStringList;
var
  rowID : TGRowID;
  blobName : String;
begin
  if FBlobIndex = nil then
  begin
    FBlobIndex := TStringList.Create;

    rowID := BlobTable.First;
    while rowID <> 0 do
    begin
      blobName := BlobTable.ReadString(rowID);  // get the name of the Image Blob

      rowID := BlobTable.Next( rowID ); // Get the RowID of the Image Blob
      FBlobIndex.AddObject( blobName, TObject( rowID ));

      rowID := BlobTable.Next( rowID ); // Step onto the next name,image pair
    end;
  end;

  Result := FBlobIndex;
end;

function TGSDBLayer.BlobTable: TGSDBTable;
begin
  if FBlobTable = nil then
  begin
    FBlobTable := SysRoot.CreateObjectByName( TGSDBTable, GUIDToString(LayerID) + '.Blob' ) as TGSDBTable;

    if FBlobTable = nil then
    begin
      FBlobTable := TGSDBTable.Create( SysRoot, GUIDToString(LayerID) + '.Blob', '' );
      SysRoot.SaveObject( FBlobTable );
    end;
  end;

  Result := FBlobTable;
end;

function TGSDBLayer.BTreeIndex: TGBTreeIndex;
begin
  if FBTreeIndex = nil then
  begin
    FBTreeIndex := SysRoot.CreateObjectByName( TGBTreeIndex, GUIDToString(LayerID) + '.BTree' ) as TGBTreeIndex;
    if FBTreeIndex = nil then
    begin
      FBTreeIndex := TGBTreeIndex.Create( SysRoot, GUIDToString(LayerID) + '.BTree' );
      SysRoot.SaveObject( FBTreeIndex );
    end;
  end;

  Result := FBTreeIndex;
end;

procedure TGSDBLayer.Clear;
begin
  if LayerList <> nil then
    LayerList.LayerPropertyIndex.DeleteKey( GUIDToString(LayerID));

  SysRoot.DeleteObject( AttributeTable );
  FreeAndNil( FAttributeTable );

  SysRoot.DeleteObject( BlobTable );
  FreeAndNil( FBlobTable );

  SysRoot.DeleteObject( DataTable );
  FreeAndNil( FDataTable );

  SysRoot.DeleteObject( ObjectTable );
  FreeAndNil( FObjectTable );

  SysRoot.DeleteObject( BTreeIndex );
  FreeAndNil( FBTreeIndex );

  SysRoot.DeleteObject( FSpatialIndex );
  FreeAndNil( FSpatialIndex );
end;

function TGSDBLayer.DataTable: TGSDBTable;
begin
  if FDataTable = nil then
  begin
    FDataTable := SysRoot.CreateObjectByName( TGSDBTable, GUIDToString(LayerID) + '.Chains' ) as TGSDBTable;

    if FDataTable = nil then
    begin
      FDataTable := TGSDBTable.Create( SysRoot, GUIDToString(LayerID) + '.Chains', '' );
      SysRoot.SaveObject( FDataTable );
    end;
  end;

  Result := FDataTable;
end;

destructor TGSDBLayer.Destroy;
begin
  Flush;
  FreeAndNil( FMetaData );

  FreeAndNil( FAttributeTable );
  FreeAndNil( FBlobTable );
  FreeAndNil( FDataTable );
  FreeAndNil( FSpatialIndex );
  FreeAndNil( FBTreeIndex );
  FreeAndNil( FObjectTable );
  FreeAndNil( FBlobIndex );
  inherited;
end;

procedure TGSDBLayer.Flush;
var
  rowID : TGRowID;
begin
  if FMetaDataModified then
  begin
    if LayerList <> nil then
      with LayerList.LayerPropertyIndex do
      begin
        rowID := FindKey( GUIDToString(LayerID));
        if rowID <> 0 then
          AttributeTable.DeleteRow(rowID);
        rowID := AttributeTable.AppendString( MetaData.CommaText );
        UpdateKey(GUIDToString(LayerID), rowID ); // appends the key if not found
      end;
    FMetaDataModified := false;
  end;

  if FAttributeTable <> nil then
    FAttributeTable.Flush;
  if FBlobTable <> nil then
    FBlobTable.Flush;
  if FDataTable <> nil then
    FDataTable.Flush;
  if FObjectTable <> nil then
    FObjectTable.Flush;
    
  if FSpatialIndex <> nil then
    FSpatialIndex.Flush;
  if FBTreeIndex <> nil then
    FBTreeIndex.Flush;

  inherited;
end;

function TGSDBLayer.GetMetaData: TStringList;
var
  rowID : TGRowID;
begin
  if FMetaData = nil then
  begin
    FMetaData := TStringList.Create;

    if LayerList <> nil then
      with LayerList.LayerPropertyIndex do
      begin
        rowID := FindKey( GUIDToString(LayerID));
        if rowID <> 0 then
          FMetaData.CommaText := AttributeTable.ReadString(rowID);
      end;

    FMetaData.OnChange := MetaDataChanged;
  end;
  Result := FMetaData;
end;

function TGSDBLayer.GetPageCache: TGPageCache;
begin
  Result := SysRoot.PageCache;
end;

procedure TGSDBLayer.InternalLoad(aStream: TMemoryStream);
begin
  aStream.Read( FHeader, SizeOf( TGSDBLayerHeader ));
  Assert( FHeader.Magic = SDBLAYER_MAGIC, 'SDBLayer Magic check failed' );
end;

procedure TGSDBLayer.InternalSave(aStream: TMemoryStream);
begin
  // Ensure all the Layer objects have been created before saving
//  DataTable;
//  ObjectTable;
//  AttributeTable;
//  SpatialIndex;

  FHeader.Magic := SDBLAYER_MAGIC;
  aStream.Write( FHeader, SizeOf( TGSDBLayerHeader ));
end;

procedure TGSDBLayer.MetaDataChanged(Sender: TObject);
begin
  FMetaDataModified := true
end;

function TGSDBLayer.ObjectTable: TGSDBTable;
begin
  if FObjectTable = nil then
  begin
    FObjectTable := SysRoot.CreateObjectByName( TGSDBTable, GUIDToString(LayerID) + '.Objects' ) as TGSDBTable;

    if FObjectTable = nil then
    begin
      FObjectTable := TGSDBTable.Create( SysRoot, GUIDToString(LayerID) + '.Objects', '' );
      SysRoot.SaveObject( FObjectTable );
    end;
  end;

  Result := FObjectTable;
end;

procedure TGSDBLayer.RebuildSpatialIndex;
var
  dbObject : TGSDBDataLink;
  rowID : TGRowID;
begin
  SpatialIndex.Clear;

  rowID := ObjectTable.First;
  while rowID <> 0 do
  begin
    dbObject := TGSDBDataLink.Create( Self, rowID );

    SpatialIndex.Insert(dbObject.MER, dbObject.PrimaryKey);

    dbObject.Free;
    rowID := ObjectTable.Next( rowID );
  end;

  SpatialIndex.Flush;
end;

procedure TGSDBLayer.SaveToXML(el: TGXML_Element);
var
  idx : integer;
  elProp: TGXML_Element;
  rowID : TGRowID;
  DBLink : TGSDBDataLink;
begin
  inherited;

  el.AddAttribute( 'LayerID', GUIDToString( FHeader.LayerID ), '' );

  with el.AddElement( 'MetaData' ) do
    for idx := 0 to MetaData.Count - 1 do
    begin
      elProp := AddElement( 'Data' );
      elProp.AddAttribute('Name', MetaData.Names[idx], '');
      elProp.BodyText := MetaData.Values[MetaData.Names[idx]];
    end;

  rowID := ObjectTable.First;
  while rowID <> 0 do
  begin
    DBLink := TGSDBDataLink.Create( Self, rowID);
    DBLink.SaveToXML(el.AddElement('OBJ'));
    DBLink.Free;

    rowID := ObjectTable.Next( rowID );
  end;
end;

procedure TGSDBLayer.SetLayerID(const Value: TGUID);
begin
  if not IsEqualGUID( LayerID, Value ) then
  begin
    FHeader.LayerID := Value;
    Modified := true;
  end;
end;

function TGSDBLayer.SpatialIndex: TGSpatialIndex;
begin
  if FSpatialIndex = nil then
  begin
    FSpatialIndex := SysRoot.CreateObjectByName( TGRTreeIndex, GUIDToString(LayerID) + '.RTree' ) as TGSpatialIndex;

    if FSpatialIndex = nil then
    begin
      FSpatialIndex := TGRtreeIndex.Create( SysRoot, GUIDToString(LayerID) + '.RTree' );
      SysRoot.SaveObject( FSpatialIndex );
    end;
  end;
  Result := FSpatialIndex;
end;




procedure TGSDBLayerList.AddLayer(sdbLayer: TGSDBLayer);
begin
  Assert( sdbLayer <> nil, 'nil Layer passed in' );
  Assert( TGLog.LogTrace( gllOne, 'AddLayer: ' + sdbLayer.Name ));

  // Check to see if this layer belongs to this SDB
  if sdbLayer.SysRoot <> SysRoot then
    CopyLayer( sdbLayer, sdbLayer.Name )
  else
    FSDBLayers.Add( sdbLayer );

  Notify;
end;

procedure TGSDBLayerList.BeforeDestruction;
begin
  FSysRoot.PageCache.UnSubscribe( self );
  inherited;
end;

procedure TGSDBLayerList.BeginUpdate;
begin
  Inc( FUpdateCount );
end;

procedure TGSDBLayerList.CancelUpdate;
begin
  FUpdateCount := 0;
end;

procedure TGSDBLayerList.Clear;
var
  idx : integer;
begin
  if FSysRoot.PageCache.ReadOnly then
    Exit;

  BeginUpdate;

  for idx := Count - 1 downto 0 do
    DeleteLayer( SDBLayer[idx] );
  FSDBLayers.Clear;

  FreeAndNil( FLayerPropertyIndex );

  EndUpdate;
end;

procedure TGSDBLayerList.CopyLayer(sdbLayer: TGSDBLayer; const sName : String );
var
  newDBLayer : TGSDBLayer;
  newDBObject : TGSDBDataLink;
  dbObject : TGSDBDataLink;
  rowID : TGRowID;
  ID : TGUID;
  blobname : AnsiString;
  memStream : TMemoryStream;
begin
  if FSysRoot.PageCache.ReadOnly then
    Exit;

  Assert( TGLog.LogTrace( gllOne, 'CopyLayer: ' + sdbLayer.Name + ' to ' + sName ));

  // if the layer comes from a different SDB then preserve LayerID
  if sdbLayer.SysRoot <> SysRoot then
    ID := sdbLayer.LayerID
  else
    CreateGUID( ID );

  newDBLayer := NewLayer( sName, ID);
  newDBLayer.AssignProperties( sdbLayer );
  newDBLayer.Name := sName;

  newDBLayer.BulkLoad := true;

  // copy over all the objects
  rowID := sdbLayer.ObjectTable.First;
  while rowID <> 0 do
  begin
    dbObject := TGSDBDataLink.Create( sdbLayer, rowID );

    newDBObject := TGSDBDataLink.Create( newDBLayer, 0 );
    newDBObject.Assign( dbObject );
    newDBObject.Post;
    newDBObject.Free;

    dbObject.Free;
    rowID := sdbLayer.ObjectTable.Next( rowID );
  end;

  // Copy over all the Blobs
  memStream := TMemoryStream.Create;
  rowID := sdbLayer.BlobTable.First;
  while rowID <> 0 do
  begin
    blobName := sdbLayer.BlobTable.ReadString(rowID);  // get the name of the Image Blob
    newDBLayer.BlobTable.AppendString( blobName );

    rowID := sdbLayer.BlobTable.Next( rowID ); // Step onto the next name,image pair

    sdbLayer.BlobTable.ReadStream(rowID,memStream);
    newDBLayer.BlobTable.AppendStream(memStream);

    rowID := sdbLayer.BlobTable.Next( rowID ); // Step onto the next name,image pair
  end;
  memStream.Free;

  newDBLayer.BulkLoad := false;
  newDBLayer.RebuildSpatialIndex;
end;

constructor TGSDBLayerList.Create( SysRoot : TGSDBSysRoot );
begin
  FSubscribers := TInterfaceList.Create;
  FSDBLayers := TObjectList.Create( True );

  Assert( SysRoot <> nil, 'TGSDBLayerList: Passed nil SysRoot' );

  FSysRoot := SysRoot;
  FSysRoot.PageCache.Subscribe( self ); // Causes RefCount to increment
end;

procedure TGSDBLayerList.DeleteLayer(sdbLayer: TGSDBLayer);
begin
  if FSysRoot.PageCache.ReadOnly or ( sdbLayer = nil ) then
    Exit;
  if sdbLayer.SysRoot <> SysRoot then
    raise EGException.Create( 'Cannot delete layer. Layer does not belong to this SDB' );

  Assert( TGLog.LogTrace( gllOne, 'DeleteLayer: ' + sdbLayer.Name ));

  SysRoot.DeleteObject( sdbLayer );
  FSDBLayers.Remove( sdbLayer );
  SysRoot.Flush;
  Notify;
end;

destructor TGSDBLayerList.Destroy;
begin
  FreeAndNil( FLayerPropertyIndex );
  FreeAndNil( FSubscribers );
  FreeAndNil( FSDBLayers );
  inherited;
end;

procedure TGSDBLayerList.EndUpdate;
begin
  Dec( FUpdateCount );

  if FUpdateCount = 0 then
    Notify;
end;

procedure TGSDBLayerList.Flush;
var
  idx : integer;
begin
  for idx := 0 to Count - 1 do
    SDBLayer[idx].Flush; // Save the layer objects to the db

  FreeAndNil( FLayerPropertyIndex );
end;

function TGSDBLayerList.GetCount: integer;
begin
  Result := FSDBLayers.Count;
end;

function TGSDBLayer.GetLayerID: TGUID;
begin
  Result := FHeader.LayerID;
end;

function TGSDBLayer.GetObjectCount: Cardinal;
begin
  Result := ObjectTable.Header.RowCount;
end;

function TGSDBLayerList.GetSDBLayer( index: integer ): TGSDBLayer;
begin
  Result := TGSDBLayer( FSDBLayers[index] );
  Result.LayerList := Self;
end;

function TGSDBLayerList.LayerByID(const ID: TGUID): TGSDBLayer;
var
  idx : integer;
begin
  for idx := 0 to Count - 1 do
  begin
    Result := SDBLayer[idx];
    if IsEqualGUID( Result.LayerID, ID ) then
      Exit;
  end;
  Result := nil;
end;

function TGSDBLayerList.LayerByName( const layerName: String): TGSDBLayer;
var
  idx : integer;
begin
  for idx := 0 to Count - 1 do
  begin
    Result := SDBLayer[idx];
    if Result.Name = layerName then
      Exit;
  end;
  Result := nil;
end;

function TGSDBLayerList.LayerPropertyIndex: TGSDBKeyIndex;
begin
  if FLayerPropertyIndex = nil then
  begin
    FLayerPropertyIndex := SysRoot.CreateObjectByName( TGSDBKeyIndex, 'LayerPropIndex' ) as TGSDBKeyIndex;
    if FLayerPropertyIndex = nil then
    begin
      FLayerPropertyIndex := TGSDBKeyIndex.Create( SysRoot, 'LayerPropIndex', '' );
      SysRoot.SaveObject( FLayerPropertyIndex );
    end;
  end;

  Result := FLayerPropertyIndex;
end;

function TGSDBLayerList.NewLayer( const layerName : String ) : TGSDBLayer;
var
  ID : TGUID;
begin
  if FSysRoot.PageCache.ReadOnly then
    Result := nil
  else
  begin
    CreateGUID( ID );
    Result := NewLayer( layerName, ID );
  end;
end;

function TGSDBLayerList.NewLayer( const layerName : String; const ID : TGUID ) : TGSDBLayer;
begin
  if FSysRoot.PageCache.ReadOnly then
    Result := nil
  else
  begin
    Assert( TGLog.LogTrace( gllOne, 'CreateLayer: ' + layerName + GUIDToString(ID)));

    Result := TGSDBLayer.Create( SysRoot, layerName );
    Result.LayerID := ID;
    Result.LayerList := Self;
    SysRoot.SaveObject( Result );

    Result.Modified := True;

    AddLayer( Result );
  end;
end;

procedure TGSDBLayerList.Notify;
var
  idx : integer;
begin
  if FUpdateCount > 0 then
    Exit;

  // Notify all of the subscribers
  for idx := FSubscribers.Count - 1 downto 0 do
    IGLayersObserver(FSubscribers[idx]).OnLayersNotification( self );
end;

procedure TGSDBLayerList.OnPageCacheNotification(Sender: TGPageCache; notification : TGPageCacheNotification);
var
  rowID : TGRowID;
  sdbLayer : TGSDBLayer;
  layerTable : TGSDBTable;
begin
  Assert( TGLog.LogDebug( gllOne, 'LayerList.OnNotified: ' + IntToStr( integer(notification ))));

  case notification of
  tgpcFlush:
    Flush;
  tgpcBeforeClose:
    begin
      Flush;
      FSDBLayers.Clear;
    end;
  tgpcAfterOpen, tgpcRollback:
    begin
      // Load all of the layers into the list
      BeginUpdate;
      FSDBLayers.Clear;
      layerTable := SysRoot.ObjectTable( TGSDBLayer.ClassName );  // this is cached by SysRoot - do not free

      rowID := layerTable.First;
      while rowID <> 0 do
      begin
        sdbLayer := SysRoot.CreateObject(TGSDBLayer, rowID ) as TGSDBLayer;
        AddLayer( sdbLayer );
        rowID := layerTable.Next( rowID );
      end;

      CancelUpdate;
    end;
  end;
end;


procedure TGSDBLayerList.SaveToXML(el: TGXML_Element);
var
  idx : integer;
begin
  for idx := 0 to Count - 1 do
    SDBLayer[idx].SaveToXML(el.AddElement('SDBLayer'));
end;

procedure TGSDBLayerList.Subscribe(observer: IGLayersObserver);
begin
  if FSubscribers.IndexOf( observer ) < 0 then
    FSubscribers.Add( observer );
end;

procedure TGSDBLayerList.UnSubscribe(observer: IGLayersObserver);
begin
  FSubscribers.Remove( observer );
end;

initialization
  RegisterSDBClass( TGSDBLayer, '' );
end.
