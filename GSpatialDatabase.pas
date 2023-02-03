//------------------------------------------------------------------------------
//Summary
//  Spatial Database Management Component
//
//Description
//Provides an implemention a the Spatial Database
//
//Author
//Graham Knight (tglobe@tglobe.com)
//------------------------------------------------------------------------------
{$I GLOBE5.INC}
unit GSpatialDatabase;

interface

uses
  ActiveX, Windows, Forms, SysUtils, Classes, GClasses, GSysUtils,
  GSDBCache, GDataReaders, GDataWriters,
  GSDBLayers, GSDBTables, GDataLink, GLogging, GXML, GBitmap;

const
  ONE_SECOND = 1.0 / ( 24.0 * 60.0 * 60.0 );
  OBJECTCACHE_BUCKETS = 17;
  OBJECTCACHE_SIZE = 128;

type
  TGSpatialDatabase = class;

  TGSDBNotification = ( gsdbBeforeOpen, gsdbAfterOpen, gsdbBeforeClose, gsdbAfterClose, gsdbChanged );

  IGSDBObserver = interface
    procedure OnSDBNotification( Sender: TGSpatialDatabase; notification : TGSDBNotification );
  end;

  TGObjectRec = record
    RowID : TGRowID;
    obj : IGMapPoint;
    MRU : Cardinal;
  end;
  PTGObjectRec = ^TGObjectRec;

  TGMapObjectCache = class(TGRoot)
  private
    function GetCacheSize: integer;
    procedure SetCacheSize(const Value: integer);
  protected
    FCache : array of TGObjectRec;
    FBuckets : array of TList;
    FMRU : Cardinal;
    FAllocatedMemory : integer;
    FObjectCount : integer;
    FNextFree : PTGObjectRec;
    function MakeCacheSpace : PTGObjectRec;
    procedure Drop( bucket, item : integer );
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; overload;
    procedure Clear( sdbLayer : TGSDBLayer ); overload;

    procedure Add(rowID : TGRowID; obj : IGMapPoint);
    function DataMER( objSource : TGObjectSource ) : TGMER;
    function Find(rowID: TGPageID): IGMapPoint;
    procedure Remove(rowID: TGPageID);

    procedure ForEach( objSource: TGObjectSource; const searchMER : TGMER; minSize: integer; callback : TGSearchCallback );
    procedure FilterIterator( objSource: TGObjectSource; const aMER: TGMER; iterator : TGListIterator);

    property CacheSize : integer read GetCacheSize write SetCacheSize;
    property ObjectCount : integer read FObjectCount;
  end;



  TGSpatialDatabase = class(TComponent, IGLayersObserver)
  private
    { Private declarations }
    FReadOnly: boolean;
    FPageCache : TGFilePageCache;
    FBlobCache : TGMapObjectCache;
    FEditCache : TGMapObjectCache;
    FDataSourceName: TFileName;
    FLayers: TGSDBLayerList;
    FOnChanged: TNotifyEvent;
    FUpdateCount : integer;
    FSubscribers : TInterfaceList;
    FSysRoot: TGSDBSysRoot;
    FOnAfterClose: TNotifyEvent;
    FOnBeforeOpen: TNotifyEvent;
    FOnBeforeClose: TNotifyEvent;
    FOnAfterOpen: TNotifyEvent;
    FEncryptionKey: String;
    FOnImportProgress: TGProgressEvent;
    procedure SetActive(Value: boolean);
    procedure SetDataSourceName(const Value: TFileName);
    function GetPageCacheSize: integer;
    procedure SetPageCacheSize(const Value: integer);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    function GetActive: boolean;
    function GetDatabaseSize: Int64;
    function GetBlobCacheSize: integer;
    procedure SetBlobCacheSize(const Value: integer);
  protected
    procedure OnLayersNotification( Sender: TGSDBLayerList ); virtual;
    procedure Notify(notification: TGSDBNotification); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Flush;
    procedure Clear;
    function Compact : Int64;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure NotifyChanged;

    function InTransaction : Boolean;
    procedure BeginTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;

    function CreateLayer( const layerName : String ) : TGUID;
    procedure DeleteLayer( const LayerID : TGUID );
    function ImportLayer( dataReader : TGDataReader ) : TGUID;

    procedure SaveToXML( el : TGXML_Element );

    procedure Subscribe( observer : IGSDBObserver );
    procedure UnSubscribe( observer : IGSDBObserver );

    property DatabaseSize : Int64 read GetDatabaseSize;
    property SDBLayers : TGSDBLayerList read FLayers;
    property BlobCache : TGMapObjectCache read FBlobCache;
    property EditCache : TGMapObjectCache read FEditCache;
    property PageCache : TGFilePageCache read FPageCache;
    property SysRoot : TGSDBSysRoot read FSysRoot;
  published
    property Active : boolean read GetActive write SetActive;
    property BlobCacheSize : integer read GetBlobCacheSize write SetBlobCacheSize;
    property DataSourceName : TFileName read FDataSourceName write SetDataSourceName;
    property EncryptionKey : String read FEncryptionKey write FEncryptionKey;
    property PageCacheSize : integer read GetPageCacheSize write SetPageCacheSize;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;

    property OnAfterClose : TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnAfterOpen : TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnBeforeClose : TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnBeforeOpen : TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;
    property OnImportProgress : TGProgressEvent read FOnImportProgress write FOnImportProgress;
  end;

procedure Register;

implementation

Uses GMapObjects, GSDBObjectSource, GSDBDataLink, GLYRReader, Globe5;

{$R *.dcr}

procedure Register;
begin
  RegisterComponents('TGlobe', [TGSpatialDatabase]);
end;



procedure TGSpatialDatabase.BeginTransaction;
begin
  BlobCache.Clear;
  EditCache.Clear;
  PageCache.BeginTransaction;
end;

procedure TGSpatialDatabase.BeginUpdate;
begin
  Inc( FUpdateCount );
end;

procedure TGSpatialDatabase.Clear;
var
  activeState : Boolean;
begin
//  if ReadOnly then
//    raise EGException.Create( 'Clear: SDB is ReadOnly' );

  activeState := Active;

  Active := False;
  FPageCache.Clear;
  FSysRoot.Clear;

  BlobCache.Clear;
  EditCache.Clear;
//  FLayers.Clear;

  Active := activeState;
end;

procedure TGSpatialDatabase.CommitTransaction;
begin
  PageCache.CommitTransaction;
end;

function TGSpatialDatabase.Compact : Int64;
var
  tmpSDB: TGSpatialDatabase;
  idx : integer;
begin
  Assert( TGLog.LogTrace( gllOne, 'Start Compacting' ));

  Active := False;  // Make sure all data is commited

  Result := DatabaseSize; // get the old size of the DB

  Active := True;

  BeginUpdate;

  // Create a temp DB to copy the data to.
  tmpSDB := TGSpatialDatabase.Create( Self );
  tmpSDB.PageCacheSize := 2048;
  tmpSDB.DataSourceName := DataSourceName + '.tmp';
  tmpSDB.Clear;
  tmpSDB.EncryptionKey := EncryptionKey;
  tmpSDB.Active := true;

  // Copy the layers to the tmp Database
  for idx := 0 to SDBLayers.Count - 1 do
    tmpSDB.SDBLayers.CopyLayer( SDBLayers[idx], SDBLayers[idx].Name );
    
  tmpSDB.Active := False;

  EndUpdate;
  NotifyChanged;
  
  Active := False;

  if tmpSDB.DatabaseSize < Result then
  begin
    DeleteFile(DataSourceName);
    RenameFile( DataSourceName + '.tmp', DataSourceName );
  end
  else
    DeleteFile(DataSourceName + '.tmp' ); // Compacted database is larger than original

  tmpSDB.Free;

  Active := True;

  Assert( TGLog.LogTrace( gllone, 'End Compacting' ));
end;

constructor TGSpatialDatabase.Create(AOwner: TComponent);
begin
  inherited;
  FSubscribers := TInterfaceList.Create;
  FPageCache := TGFilePageCache.Create;
  FBlobCache := TGMapObjectCache.Create;
  FEditCache := TGMapObjectCache.Create;

  FSysRoot := TGSDBSysRoot.Create( FPageCache );
  FLayers := TGSDBLayerList.Create( FSysRoot );
  FLayers.Subscribe(self);
end;

function TGSpatialDatabase.CreateLayer( const layerName: String): TGUID;
begin
  if ReadOnly then
    raise EGException.Create( 'CreateLayer: SDB is ReadOnly' );

  CreateGUID( Result );
  SDBLayers.NewLayer(layerName, Result);
end;

procedure TGSpatialDatabase.DeleteLayer(const LayerID: TGUID);
var
  sdbLayer : TGSDBLayer;
begin
  if ReadOnly then
    raise EGException.Create( 'DeleteLayer: SDB is ReadOnly' );

  sdbLayer := SDBLayers.LayerByID(LayerID);
  if sdbLayer <> nil then
  begin
    BlobCache.Clear( sdbLayer );
    EditCache.Clear( sdbLayer );
    SDBLayers.DeleteLayer(sdbLayer);
  end;
end;

destructor TGSpatialDatabase.Destroy;
begin
  Flush;
  Active := false;

  SDBLayers.UnSubscribe(self);
  FreeAndNil( FLayers );
  FreeAndNil( FSysRoot );
  FreeAndNil( FSubscribers );
  FreeAndNil( FBlobCache );
  FreeAndNil( FEditCache );
  FreeAndNil( FPageCache );

  inherited;
end;

procedure TGSpatialDatabase.EndUpdate;
begin
  Dec( FUpdateCount );
  Assert( FUpdateCount >= 0, 'Unbalanced EndUpdate called without BeginUpdate' );
end;

procedure TGSpatialDatabase.Flush;
begin
  if Active and not ReadOnly then
  begin
    SDBLayers.Flush;
    SysRoot.Flush;
    BlobCache.Clear;
    EditCache.Clear;
  end;
end;

function TGSpatialDatabase.GetActive: boolean;
begin
  Result := PageCache.Active;
end;

function TGSpatialDatabase.GetBlobCacheSize: integer;
begin
  Result := FBlobCache.CacheSize;
end;

function TGSpatialDatabase.GetDatabaseSize: Int64;
var
  F : TSearchRec;
begin
  Result := 0;

  PageCache.Flush;
  if FindFirst( DataSourceName, 0, F ) = 0 then
    Result := F.Size;
  FindClose(F);
end;

function TGSpatialDatabase.GetPageCacheSize: integer;
begin
  Result := PageCache.CacheSize;
end;

function TGSpatialDatabase.GetReadOnly: Boolean;
begin
  Result := FReadOnly or ( Active and PageCache.ReadOnly );
end;

function TGSpatialDatabase.ImportLayer(dataReader: TGDataReader) : TGUID;
var
  obj : IGMapPoint;
  idx : integer;
  sdbLayer : TGSDBLayer;
  objectSource : TGSDBObjectSource;
begin
  Result := NULL_GUID;
  Assert( TGLog.LogTrace( gllone, 'Start ImportLayer' ));

  if ReadOnly then
    raise EGException.Create( 'ImportLayer: SDB is ReadOnly' );

  objectSource := nil;
  if dataReader <> nil then
  try
    if not Assigned( dataReader.OnProgress ) then
      dataReader.OnProgress := FOnImportProgress; 

    BeginUpdate;

    sdbLayer := SDBLayers.NewLayer( dataReader.Name );

    objectSource := TGSDBObjectSource.Create(nil);
    objectSource.SDB := Self;
    objectSource.SDBLayer := sdbLayer;

    // needed by LR5Reader to import image data
    dataReader.ObjectSource := objectSource;

    sdbLayer.BulkLoad := true;

    obj := dataReader.First;
    while obj <> nil do
    begin
      obj.ObjectSource := objectSource;
      obj.DataLink := TGSDBDataLink.Create( sdbLayer, 0 );  // Create a new object
      obj.Post;
      obj := dataReader.Next;
    end;

    sdbLayer.BulkLoad := false;
//    sdbLayer.RebuildSpatialIndex;

    // copy over any metadata to the sdb layer
    with dataReader.MetaData do
      for idx := 0 to Count - 1 do
        sdbLayer.MetaData.Values[Names[idx]] := Values[Names[idx]];

    //Copy over the presenters for the layer
    sdbLayer.MetaData.Values['PresenterXML'] := dataReader.Presenters.AsXMLString;

    Result := sdbLayer.LayerID;  // return the ID of the new layer

    EndUpdate;
    NotifyChanged;
  finally
    objectSource.Free;
  end;

  Flush;
  Assert( TGLog.LogTrace( gllone, 'End ImportLayer' ));
end;

function TGSpatialDatabase.InTransaction: Boolean;
begin
  Result := PageCache.InTransaction;
end;

procedure TGSpatialDatabase.Notify(notification: TGSDBNotification);
var
  idx : integer;
begin
  if FUpdateCount = 0 then
  begin
    // Notify all of the subscribers
    if not ( csDestroying in ComponentState ) then
      for idx := FSubscribers.Count - 1 downto 0 do
        IGSDBObserver(FSubscribers[idx]).OnSDBNotification( self, notification );

    if notification = gsdbChanged then
    begin
      if not ( csDestroying in ComponentState ) then
        if Assigned( FOnChanged ) then
          FOnChanged( self );
    end;
  end;
end;

procedure TGSpatialDatabase.NotifyChanged;
begin
  if FUpdateCount = 0 then
    Notify( gsdbChanged )
end;

procedure TGSpatialDatabase.OnLayersNotification( Sender: TGSDBLayerList );
begin
  NotifyChanged;
end;

procedure TGSpatialDatabase.RollbackTransaction;
begin
  BlobCache.Clear;
  EditCache.Clear;
  Flush;
  PageCache.RollbackTransaction;
  NotifyChanged;
end;

procedure TGSpatialDatabase.SaveToXML(el: TGXML_Element);
begin
  SDBLayers.SaveToXML( el.AddElement( 'Layers' ));
end;

procedure TGSpatialDatabase.SetActive(Value: boolean);
begin
  if Trim( DataSourceName ) = '' then
    Value := false;

  if Active = Value then
    Exit;

  if Value then
  begin
    Assert( TGLog.LogTrace( gllOne, 'SpatialDatabase Open' ));

    Notify( gsdbBeforeOpen );

    if Assigned( FOnBeforeOpen ) then
      FOnBeforeOpen( self );

    FPageCache.EncryptionKey := EncryptionKey;
    FPageCache.Active := Value;

    Notify( gsdbAfterOpen );

    if Assigned( FOnAfterOpen ) then
      FOnAfterOpen( self );
  end
  else
  begin
    if Assigned( FOnBeforeClose ) then
      FOnBeforeClose( self );

    Notify( gsdbBeforeClose );

    Flush;

    FPageCache.Active := Value;

    Notify( gsdbAfterClose );

    if Assigned( FOnAfterClose ) then
      FOnAfterClose( self );
    Assert( TGLog.LogTrace( gllOne, 'SpatialDatabase Closed' ));
  end;
end;

procedure TGSpatialDatabase.SetBlobCacheSize(const Value: integer);
begin
  FBlobCache.CacheSize := Value;
end;

procedure TGSpatialDatabase.SetDataSourceName(const Value: TFileName);
var
  activeState : boolean;
begin
  if FDataSourceName <> Trim( Value ) then
  begin
    activeState := Active;
    Active := false;

    FDataSourceName := Trim( Value );
    FPageCache.DataFile := FDataSourceName;

    Active := activeState;
  end;
end;

procedure TGSpatialDatabase.SetPageCacheSize(const Value: integer);
begin
  FPageCache.CacheSize := Value;
end;

procedure TGSpatialDatabase.SetReadOnly(const Value: Boolean);
begin
  if Active then
  begin
    PageCache.ReadOnly := Value;
    FReadOnly := PageCache.ReadOnly;  // incase PageCache blocks the change
  end
  else
    FReadOnly := Value;
end;

procedure TGSpatialDatabase.Subscribe(observer: IGSDBObserver);
begin
  if FSubscribers.IndexOf( observer ) < 0 then
    FSubscribers.Add( observer );
end;

procedure TGSpatialDatabase.UnSubscribe(observer: IGSDBObserver);
begin
  FSubscribers.Remove( observer );
end;



procedure TGMapObjectCache.Add(rowID : TGRowID; obj : IGMapPoint);
var
  objRec : PTGObjectRec;
begin
  if CacheSize <= 1 then
    Exit;

  if Find( obj.PrimaryKey ) <> nil then
    raise EGException.Create('Already in cache');


  if FAllocatedMemory > CacheSize * 1024 then
    objRec := MakeCacheSpace  // Free up some space
  else
    objRec := FNextFree;

  // place the object into the cache
  if objRec = nil then
    objRec := MakeCacheSpace;  // Free up some space

  Assert( objRec <> nil, 'Failed to make space in Object cache' );

  FNextFree := PTGObjectRec( objRec.RowID );

  objRec.obj := obj;
  objRec.RowID := rowID;
  objRec.MRU := FMRU;
  Inc( FMRU );
  Inc( FObjectCount );
  Inc( FAllocatedMemory, obj.ObjectSize );

  FBuckets[rowID mod OBJECTCACHE_BUCKETS].Add(objRec);
end;

procedure TGMapObjectCache.Clear;
var
  idx : integer;
begin
  FMRU := 0;
  FNextFree := nil;
  for idx := 1 to High(FCache) do
  begin
    FCache[idx].obj := nil;
    FCache[idx].RowID := integer( FNextFree ); // Link into to the Free item chain
    FNextFree := @FCache[idx];
  end;

  for idx := 0 to High(FBuckets) do
    FBuckets[idx].Clear;

  FObjectCount := 0;
  FAllocatedMemory := 0;
end;

procedure TGMapObjectCache.Clear( sdbLayer : TGSDBLayer );
var
  idx : integer;
begin
  // delete all objects that belong to this sdb layer from the cache
  for idx := 1 to High(FCache) do
    with FCache[idx] do
      if ( obj <> nil ) and ( obj.ObjectSource <> nil ) then
        if TGSDBObjectSource( obj.ObjectSource ).SDBLayer = sdbLayer then
          Remove( RowID );
end;

constructor TGMapObjectCache.Create;
var
  idx : integer;
begin
  SetLength( FBuckets, OBJECTCACHE_BUCKETS );
  for idx := 0 to High(FBuckets) do
    FBuckets[idx] := TList.Create;

  CacheSize := OBJECTCACHE_SIZE;
end;

function TGMapObjectCache.DataMER(objSource: TGObjectSource): TGMER;
var
  idx : integer;
  mapObject : IGMapPoint;
begin
  MER_Empty( Result );
  // search all of the cached objects
  for idx := 1 to High(FCache) do
    if FCache[idx].obj <> nil then
    begin
      mapObject := FCache[idx].obj;
      if mapObject.ObjectSource = objSource then
        Result := MER_Union(Result, mapObject.ObjectMER);
    end;
end;

destructor TGMapObjectCache.Destroy;
var
  idx : integer;
begin
  Clear;
  for idx := 0 to High(FBuckets) do
    FBuckets[idx].Free;
  inherited;
end;

procedure TGMapObjectCache.Drop(bucket, item: integer);
var
  objRec : PTGObjectRec;
begin
  objRec := PTGObjectRec( FBuckets[bucket][item] );

  if objRec.obj <> nil then
    Dec( FAllocatedMemory, objRec.obj.ObjectSize );

  objRec.obj := nil;
  objRec.RowID := integer( FNextFree );
  FNextFree := objRec;
  FBuckets[bucket].Delete(item);
  Dec( FObjectCount );
end;

procedure TGMapObjectCache.FilterIterator( objSource: TGObjectSource;
  const aMER: TGMER; iterator: TGListIterator);
var
  idx : integer;
  mapObject : IGMapPoint;
begin
  // search all of the cached objects
  for idx := 1 to High(FCache) do
    if FCache[idx].obj <> nil then
    begin
      mapObject := FCache[idx].obj;

      // if the object belongs to the correct layer then check for intersection
      if mapObject.ObjectSource = objSource then
      begin
        // check to see if the cached object is already in the list
        if iterator.PrimaryKeyList.IndexOf(Pointer(mapObject.PrimaryKey)) >= 0 then
        begin
          if not mapObject.IntersectMER(aMER) then
            iterator.PrimaryKeyList.Remove(Pointer(mapObject.PrimaryKey))
        end
        else
          if mapObject.IntersectMER(aMER) then
            iterator.AddPrimaryKey(mapObject.PrimaryKey);
      end;
    end;
end;

function TGMapObjectCache.Find(rowID: TGPageID): IGMapPoint;
var
  idx : integer;
begin
  if CacheSize > 1 then
    with FBuckets[rowID mod OBJECTCACHE_BUCKETS] do
      for idx := 0 to Count - 1 do
        if PTGObjectRec(Items[idx]).RowID = rowID then
        begin
          Result := PTGObjectRec(Items[idx]).obj;
          PTGObjectRec(Items[idx]).MRU := FMRU;
          Inc( FMRU );
          Exit;
        end;

  Result := nil;
end;

procedure TGMapObjectCache.ForEach(objSource: TGObjectSource; const searchMER: TGMER;
  minSize: integer; callback: TGSearchCallback);
var
  idx : integer;
  mapObject : IGMapPoint;
  abort : Boolean;
begin
  abort := false;

  for idx := 1 to High(FCache) do
    if FCache[idx].obj <> nil then
    begin
      mapObject := FCache[idx].obj;

      // if the object belongs to the correct layer then check for intersection
      if mapObject.IntersectMER(searchMER) then
      begin
        if IsEqualGUID( mapObject.ObjectSource.UniqueID, objSource.UniqueID ) then
        begin
          mapObject.ObjectSource := objSource;
          callBack( mapObject.PrimaryKey, abort );
        end;

        if abort then
          Exit;
      end;
    end;
end;

function TGMapObjectCache.GetCacheSize: integer;
begin
  Result := Length( FCache );
end;

function SortByMRU( Item1, Item2 : Pointer ) : integer;
begin
  Result := PTGObjectRec( Item2 ).MRU - PTGObjectRec( Item1 ).MRU
end;

function TGMapObjectCache.MakeCacheSpace : PTGObjectRec;
var
  idx : integer;
begin
  for idx := 0 to High(FBuckets) do
    with FBuckets[idx] do
      if Count > 0 then
      begin
        Sort( SortByMRU );
        Drop( idx, Count - 1 ); // Drop the last item from each bucket

        Pack; // remove the nil pointers
      end;
  Result := FNextFree;
end;

procedure TGMapObjectCache.Remove(rowID: TGPageID);
var
  idx : integer;
begin
  with FBuckets[rowID mod OBJECTCACHE_BUCKETS] do
    for idx := 0 to Count - 1 do
      if PTGObjectRec(Items[idx]).RowID = rowID then
      begin
        Drop(rowID mod OBJECTCACHE_BUCKETS, idx );
        Exit;
      end;
end;

procedure TGMapObjectCache.SetCacheSize(const Value: integer);
begin
  Clear;  // empty any existing items in the cache
  SetLength( FCache, Value );
  Clear;  // Initialises the free index chain
end;

end.
