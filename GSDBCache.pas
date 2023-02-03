//-----------------------------------------------------------------------
// Summary
//	Spatial Database Page Caching classes
//
// Description
// 	Provides the management of page based storage of data in the Spatial Database.
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------

{$I GLOBE5.INC}

unit GSDBCache;

interface

Uses Windows, Classes, SysUtils, GSysUtils, GSDBCrypt, GXML, Contnrs;

const
  TG_PAGECACHE_VERSION = $0004;
  CACHE_DEFAULT_COUNT = 1024;
  HASH_BUCKET_DIVISOR = 251;  // or 701
  CACHE_HEADER_SIZE = 1024 * 4;
  CACHE_PAGE_SIZE = 1024 * 8;
  CACHE_DROP_FACTOR = 5;  // n/CACHE_DROP_FACTOR entries are removed when space is needed
  READ_AHEAD_BUFFERS = 16;
  FREE_CACHE_ID = $FFFFFFFF;

type
  TGPageCache = class;
  TGCachedPage = class;

  TGPageCacheNotification = ( tgpcCreated, tgpcAfterOpen, tgpcBeforeClose, tgpcFlush, tgpcRollback );

  IGPageCacheObserver = interface
    procedure OnPageCacheNotification( Sender: TGPageCache; notification : TGPageCacheNotification );
  end;

  TGPageID = Cardinal;
  TGRowID = Cardinal;
  PTGRowID = ^TGRowID;
  TGPageIDArray = array of TGPageID;

  // A pointer to an in memory Cached Page of data
  PTGCachedPage = ^TGCachedPage;


  TGCachedPage = class(TGRoot)
  private
    FPageCache: TGPageCache;
    FPageID : TGPageID;
    FBatchWritePageID : TGPageID;
    FModified : boolean;
    FLockCount : integer;
    FIndex : integer;
    procedure SetModified(const Value: boolean);
  public
    MRU : array [0..2] of Cardinal;
    PageData : array of Byte;

    constructor Create( pageCache: TGPageCache; pageID : TGPageID ); virtual;
    destructor Destroy; override;

    procedure Lock;
    procedure UnLock;
    function IsLocked : boolean;

    property Modified : boolean read FModified write SetModified;
    property PageCache: TGPageCache read FPageCache;
    property PageID : TGPageID read FPageID write FPageID;
    property BatchWritePageID : TGPageID read FBatchWritePageID write FBatchWritePageID;
    property Index : integer read FIndex write FIndex;
  end;


  TGPage = class(TGRoot)
  private
    FPage : TGCachedPage;

    function GetModified: boolean;
    function GetPageID: TGPageID;
    procedure SetModified(const Value: boolean);
  public
    constructor Create( pageCache: TGPageCache; pageID : TGPageID ); virtual;
    destructor Destroy; override;

    function DataPtr : PChar;

    property Modified : boolean read GetModified write SetModified;
    property PageID : TGPageID read GetPageID;
  end;



  TGCacheBucketList = class(TGRoot)
  protected
    FBuckets : array of TObjectList;
  public
    constructor Create( bucketCount : integer );
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AData: TGCachedPage);
    function Remove(pageID: TGPageID): TGCachedPage;
    function Find(pageID: TGPageID): TGCachedPage;
  end;

  TGPageIDBucketList = class(TGRoot)
  protected
    FBuckets : array of TObjectList;
  public
    constructor Create( bucketCount : integer );
    destructor Destroy; override;

    procedure Clear;
    procedure Add(pageID: TGPageID);
    function Find(pageID: TGPageID): Boolean;
  end;


  TGPageCacheHeader = packed record
    Version : Word;
    PageSize : Cardinal;
    AllocatedPageCount : Cardinal;
    FreePageCount : Cardinal;
    PageMapBits : Cardinal;
    UniqueID : Cardinal;  // Counter used to generate Unique IDs

    // System table management
    SysFirstPageID : TGPageID;
    SysLastPageID : TGPageID;
    SysRowCount : integer;
    EncryptionType : Byte;
    EncryptionChecksum : Cardinal;
  end;
  PTGPageCacheHeader = ^TGPageCacheHeader;


  TGPageCache = class(TGRoot)
  private
    FHeader : TGPageCacheHeader;
    FNextPageID : TGPageID;
    FActive : boolean;
    FCache : array of TGCachedPage;
    FFreeCacheList : TList;
    FSubscribers : TInterfaceList;
    FCacheMisses: Cardinal;
    FCacheHits: Cardinal;
    FModified: Boolean;
    FCacheReadOnly: Boolean;
    FReadOnly: Boolean;
    FEncryptionKey: String;

    function GetCacheSize: integer;

    function GetSysFirstPageID: TGRowID;
    function GetSysLastPageID: TGRowID;
    function GetSysRowCount: integer;
    procedure SetSysFirstPageID(const Value: TGRowID);
    procedure SetSysLastPageID(const Value: TGRowID);
    procedure SetSysRowCount(const Value: integer);
    procedure SetEncryptionKey(const Value: String);
    function GetReadOnly: Boolean;
  protected
    FHashList : TGCacheBucketList;
    FCrypt : TGSDBCrypt;

    procedure SetActive(const Value: boolean); virtual;
    procedure SetCacheSize(const Value: integer); virtual;

    procedure ClearCache( saveModified : Boolean ); virtual;

    procedure DropCachePage( pageID : TGPageID );
    procedure MakeCacheSpace;
    function FindCachePage( pageID : TGPageID ) : TGCachedPage;
    function NewCachePage( pageID : TGPageID ) : TGCachedPage;
    function NextPageID : TGPageID; virtual;

    function GetEncryptionType : Byte;

    procedure InternalClose; virtual; abstract;
    procedure InternalOpen; virtual; abstract;

    procedure InternalRead( var page : TGCachedPage ); virtual; abstract;
    procedure InternalWrite( const page : TGCachedPage ); virtual; abstract;
    procedure InternalDelete( pageID : TGPageID ); virtual; abstract;

    procedure BeginBatchWrite; virtual; abstract;
    procedure EndBatchWrite; virtual; abstract;

    procedure ReadHeader; virtual; abstract;
    procedure WriteHeader; virtual; abstract;

    procedure Notify( notification : TGPageCacheNotification );
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function NewPage : TGPageID;
    procedure DeletePage( pageID : TGPageID);

    function AcquirePage( pageID : TGPageID ) : TGCachedPage;
    procedure ReleasePage( aPage : TGCachedPage ); overload; virtual;

    procedure Subscribe( observer : IGPageCacheObserver );
    procedure UnSubscribe( observer : IGPageCacheObserver );

    function LockedPageCount : integer;
    function NextUniqueID : integer;

    procedure Clear; virtual;
    procedure Flush; virtual;
    procedure Open; virtual;
    procedure Close; virtual;

    procedure BeginTransaction; virtual; abstract;
    procedure CommitTransaction; virtual; abstract;
    procedure RollbackTransaction; virtual; abstract;
    function InTransaction : boolean; virtual; abstract;

    procedure SaveToXML( el : TGXML_Element ); virtual;

    property Active : boolean read FActive write SetActive;
    property CacheHits : Cardinal read FCacheHits write FCacheHits;
    property CacheMisses : Cardinal read FCacheMisses write FCacheMisses;
    property CacheSize: integer read GetCacheSize write SetCacheSize default CACHE_DEFAULT_COUNT;
    property EncryptionKey : String read FEncryptionKey write SetEncryptionKey;
    property ReadOnly : Boolean read GetReadOnly write FReadOnly;
    property SysFirstPageID : TGRowID read GetSysFirstPageID write SetSysFirstPageID;
    property SysLastPageID : TGRowID read GetSysLastPageID write SetSysLastPageID;
    property SysRowCount : integer read GetSysRowCount write SetSysRowCount;

    property Header : TGPageCacheHeader read FHeader;
  end;


  TGFilePageCache = class(TGPageCache)
  private
    FVirtualBuffer : Pointer; // File IO buffer
    FDataFileHandle : THandle;  // Data file handle
    FReadAheadPageID : Cardinal;
    FPageBuffers : array [0..READ_AHEAD_BUFFERS] of array [0..CACHE_PAGE_SIZE] of Byte;
    FPageBufferIDs : array[0..READ_AHEAD_BUFFERS] of TGPageID;

    FDataFile : TFileName;
    FDataFileAndPath : String;
    FDataFileSize : Int64;

    // Amount of space left in Bitmap Pages
    FMapPageSpace : array of Cardinal;
    FBatchWriteList : TObjectList;
    FBatchWriteCounter : integer;

    FTransactionPages : TGPageIDBucketList;
    FTransactionCounter : integer;

    procedure SetBit( page : TGCachedPage; index : integer );
    procedure ClearBit( page : TGCachedPage; index : integer );
    function FindFreeBit( page : TGCachedPage ) : Cardinal;
    function CountUsedBits( page : TGCachedPage ) : Cardinal;

    function GetDataFileSize : Int64;
    procedure CreateNewFileCache;
    procedure InitialiseComponents;
    procedure ClearReadAhead( pageID : TGPageID );

    function PageOffset( pageID: TGPageID ) : Int64;
    function FileSeek64( handle: Integer; const offset: Int64; origin: Integer): Int64;

    procedure WriteTransactionPage( pageID: TGPageID );
    procedure RollbackTransactionFile;
  protected
    procedure SetDataFile(const Value: TFilename); virtual;

    function NextPageID : TGPageID; override;

    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalRead( var page : TGCachedPage ); override;
    procedure InternalWrite( const page : TGCachedPage ); override;
    procedure InternalDelete( pageID : TGPageID ); override;

    procedure BeginBatchWrite; override;
    procedure EndBatchWrite; override;

    procedure ReadHeader; override;
    procedure WriteHeader; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure SaveToXML( el : TGXML_Element ); override;

    procedure BeginTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;
    function InTransaction : boolean; override;

  published
    property DataFile : TFilename read FDataFile write SetDataFile;
  end;

function MakeRowID( pageID : TGPageID; itemID : Byte ) : TGRowID;
function RowPageID( rowID : TGRowID ) : Cardinal;
function RowItemID( rowID : TGRowID ) : integer;
function RowIDToStr( rowID : TGRowID ) : String;

implementation

Uses GLogging, GSDBTables, GSDBRTree, GSDBBTree;

var
  SortTickCount : Cardinal;


function MakeRowID( pageID : TGPageID; itemID : Byte ) : TGRowID;
begin
  Result := Cardinal( itemID and $00FFFFFF ) shl 24 + pageID;
end;

function RowPageID( rowID : TGRowID ) : Cardinal;
begin
  Result := rowID and $00FFFFFF;
end;

function RowItemID( rowID : TGRowID ) : integer;
begin
  Result := ( rowID shr 24 ) and $000000FF;
end;

function RowIDToStr( rowID : TGRowID ) : String;
begin
  Result := IntToStr( RowPageID( rowID )) + '.' + IntToStr( RowItemID( rowID ));
end;



procedure TGCacheBucketList.Add(AData: TGCachedPage);
var
  idx, jdx : integer;
  L, H: Integer;
begin
  idx := AData.PageID mod Cardinal( Length( FBuckets ));

  L := 0;
  H := FBuckets[idx].Count - 1;
  while L <= H do
  begin
    jdx := (L + H) shr 1;

    // assumes that the page does not already exist in the list
    if AData.PageID < TGCachedPage( FBuckets[idx][jdx] ).PageID then
      L := jdx + 1
    else
      H := jdx - 1;
  end;

  FBuckets[idx].Insert(L, AData);
end;

procedure TGCacheBucketList.Clear;
var
  idx : integer;
begin
  for idx := 0 to High( FBuckets ) do
    FBuckets[idx].Clear;
end;

constructor TGCacheBucketList.Create( bucketCount : integer );
var
  idx : integer;
begin
  SetLength( FBuckets, bucketCount );
  for idx := 0 to High( FBuckets ) do
    FBuckets[idx] := TObjectList.Create( False );
end;

destructor TGCacheBucketList.Destroy;
var
  idx : integer;
begin
  for idx := 0 to High( FBuckets ) do
    FBuckets[idx].Free;
end;

function TGCacheBucketList.Find( pageID: TGPageID ) : TGCachedPage;
var
  idx, jdx : integer;
  L, H: Integer;
begin
  idx := pageID mod Cardinal( Length( FBuckets ));

  L := 0;
  H := FBuckets[idx].Count - 1;
  while L <= H do
  begin
    jdx := (L + H) shr 1;

    Result := TGCachedPage( FBuckets[idx][jdx] );
    if pageID = Result.PageID then
      Exit;
    if pageID < Result.PageID then
      L := jdx + 1
    else
      H := jdx - 1;
  end;

  Result := nil;
end;

function TGCacheBucketList.Remove( pageID: TGPageID ): TGCachedPage;
var
  idx, jdx : integer;
  L, H: Integer;
begin
  idx := pageID mod Cardinal( Length( FBuckets ));

  L := 0;
  H := FBuckets[idx].Count - 1;
  while L <= H do
  begin
    jdx := (L + H) shr 1;

    Result := TGCachedPage( FBuckets[idx][jdx] );
    if pageID = Result.PageID then
    begin
      FBuckets[idx].Delete(jdx);
      Exit;
    end;

    if pageID < Result.PageID then
      L := jdx + 1
    else
      H := jdx - 1;
  end;

  Result := nil;
end;




constructor TGCachedPage.Create(pageCache: TGPageCache; pageID: TGPageID);
begin
  FPageCache := pageCache;
  FPageID := pageID;
  SetLength( PageData, PageCache.FHeader.PageSize );
end;

destructor TGCachedPage.Destroy;
begin
  inherited;
end;

function TGCachedPage.IsLocked: boolean;
begin
  Result := FLockCount > 0;
end;

procedure TGCachedPage.Lock;
begin
  Inc( FLockCount );
end;

procedure TGCachedPage.SetModified(const Value: boolean);
begin
  FModified := Value;
end;

procedure TGCachedPage.UnLock;
begin
  Assert( FLockCount > 0, 'Page UnLock Underflow' );
  Dec( FLockCount );
end;

//-----------------------------------------------------------------------------
//Description
//Locks a page in the cache memory so that it cannot be swapped out while it is
//being accesed.
//
//Release must be called for this page when it is no longer needed.
//
//Parameters
//pageID :  The id of the page to lock in memory.
//-----------------------------------------------------------------------------
function TGPageCache.AcquirePage(pageID: TGPageID): TGCachedPage;
begin
  Result := FindCachePage( pageID );

  if Result <> nil then
    Result.Lock;  // Hold in memory while Acquired
end;

procedure TGPageCache.Clear;
begin
  if Active then
    ClearCache( True );
end;

procedure TGPageCache.ClearCache( saveModified : Boolean );
var
  idx : integer;
  lockedPages : integer;
begin
  lockedPages := 0;

  FHashList.Clear;
  FFreeCacheList.Clear;

  if saveModified and not ReadOnly then
  begin
//    BeginBatchWrite;
    for idx := 0 to High( FCache ) do
      if FCache[idx].Modified then
        InternalWrite( FCache[idx] );
//    EndBatchWrite;
  end;

  for idx := 0 to High( FCache ) do
  begin
    FFreeCacheList.Add( Pointer(idx));  // add all of the pages to the free list

    if FCache[idx].PageID <> FREE_CACHE_ID then
    begin
      if FCache[idx].IsLocked then
        Inc( lockedPages );

      FCache[idx].PageID := FREE_CACHE_ID;
    end;
  end;

  if saveModified and not ReadOnly and FModified then
    WriteHeader;

  FModified := false;
  Assert( lockedPages = 0, 'ClearCache(): Locked Pages still in the PageCache' );
end;

procedure TGPageCache.Close;
begin
  Active := false;
end;

constructor TGPageCache.Create;
begin
  FSubscribers := TInterfaceList.Create;
  FFreeCacheList := TList.Create;
  FHashList := TGCacheBucketList.Create(1);

  CacheSize := CACHE_DEFAULT_COUNT;

  FCacheHits := 1;
end;

procedure TGPageCache.DeletePage(pageID: TGPageID);
begin
  Assert( Active, 'PageCache is not Active' );

  DropCachePage( pageID );
  InternalDelete( pageID );
end;

destructor TGPageCache.Destroy;
var
  idx : integer;
begin
  Active := false;
  FreeAndNil( FSubscribers );
  FreeAndNil( FHashList );
  FreeAndNil( FFreeCacheList );
  for idx := 0 to High( FCache ) do
    FCache[idx].Free;
end;

procedure TGPageCache.DropCachePage(pageID: TGPageID);
var
  cache : TGCachedPage;
begin
  cache := FHashList.Remove(pageID);
  if cache <> nil then
  begin
    if cache.Modified then
      InternalWrite( cache );
      
    FCache[cache.Index].PageID := FREE_CACHE_ID;
    FFreeCacheList.Add(Pointer(cache.Index));
  end;
end;

function TGPageCache.FindCachePage(pageID: TGPageID): TGCachedPage;
var
  TickCount, diff : Cardinal;
begin
  Result := FHashList.Find(PageID);

  // If not already in the cache
  if Result = nil then
  begin
    Result := NewCachePage( pageID );
    try
      InternalRead( Result );

      // Add the cache item into the hash array
      FHashList.Add( Result );
      Inc( FCacheMisses );
    except
      Result := nil;
      Exit;
    end;
  end
  else
    Inc( FCacheHits );

  tickCount := GetTickCount();
  diff := tickCount - Result.MRU[0];
  if diff <> 0 then
  begin
    Result.MRU[2] := Result.MRU[1] + diff;
    Result.MRU[1] := Result.MRU[0] + diff;
    Result.MRU[0] := tickCount; // Update the MRU counter
  end;
end;

procedure TGPageCache.Flush;
var
  idx : integer;
begin
  if Active and not ReadOnly then
  begin
  //  BeginBatchWrite;
    for idx := 0 to High( FCache ) do
      if FCache[idx].Modified then
        InternalWrite( FCache[idx] );
  //  EndBatchWrite;

    if FModified then
      WriteHeader;

    FModified := false;
  end;
end;

function TGPageCache.GetCacheSize: integer;
begin
  Result := Length( FCache );
end;

function TGPageCache.GetEncryptionType: Byte;
var
  tmp : String;
begin
  Result := 0;

  if EncryptionKey <> '' then
  begin
    tmp := Trim( Copy( EncryptionKey, 1, Pos( ':', EncryptionKey )));

    // Check for TEA encryption
    if CompareText( 'TEA:', tmp ) = 0 then
      Result := 1;
  end;
end;

function TGPageCache.GetReadOnly: Boolean;
begin
  if Active then
    Result := FReadOnly or FCacheReadOnly  // Check the cache state as well
  else
    Result := FReadOnly;
end;

function TGPageCache.GetSysFirstPageID: TGRowID;
begin
  if FHeader.SysFirstPageID = 0 then
  begin
    FHeader.SysFirstPageID := NewPage;
    FHeader.SysLastPageID := FHeader.SysFirstPageID;
    FModified := true;
  end;

  Result := FHeader.SysFirstPageID;
end;

function TGPageCache.GetSysLastPageID: TGRowID;
begin
  Result := FHeader.SysLastPageID;
end;

function TGPageCache.GetSysRowCount: integer;
begin
  Result := FHeader.SysRowCount;
end;

function TGPageCache.LockedPageCount: integer;
var
  idx : integer;
begin
  Result := 0;
  for idx := 0 to High( FCache ) do
    if ( FCache[idx].PageID <> FREE_CACHE_ID ) and FCache[idx].IsLocked then
      Inc( Result );
end;

function SortByMRU(Item1, Item2: Pointer) : integer;
var
  val1, val2 : Cardinal;
begin
  with PTGCachedPage( Item1 )^ do
    val1 := (( SortTickCount - MRU[0] ) shr 2 ) + (( MRU[0] - MRU[1] ) shr 1) + ( MRU[1] - MRU[2] );

  with PTGCachedPage( Item2 )^ do
    val2 := (( SortTickCount - MRU[0] ) shr 2 ) + (( MRU[0] - MRU[1] ) shr 1) + ( MRU[1] - MRU[2] );

  if val1 < val2 then
    Result := 1
  else
    if val1 > val2 then
      Result := -1
    else
      Result := 0;
end;

procedure TGPageCache.MakeCacheSpace;
var
  idx : integer;
  sortList : TList;
begin
  // Make some space in the Cache table
  sortList := TList.Create;
  sortList.Capacity := Length( FCache );

  // add in the unLocked pages that are not PageMap pages
  for idx := 0 to High( FCache ) do
    if ( not FCache[idx].IsLocked ) and (( FCache[idx].PageID mod FHeader.PageMapBits) <> 0 ) then
      sortList.Add( @FCache[idx] );

  if sortList.Count = 0 then
    raise EGException.Create( 'MakeCacheSpace: Out of Cache Table Space' );

  SortTickCount := GetTickCount();
  sortList.Sort( SortByMRU );

  // drop a selection of the available pages
  BeginBatchWrite;
  for idx := 0 to sortList.Count div CACHE_DROP_FACTOR do
    DropCachePage( PTGCachedPage( sortList[idx] )^.PageID );
  EndBatchWrite;

  sortList.Free;
end;

function TGPageCache.NewCachePage(pageID: TGPageID): TGCachedPage;
var
  idx : integer;
begin
  if FFreeCacheList.Count = 0 then
    MakeCacheSpace;

  idx := integer(FFreeCacheList.Last);
  FFreeCacheList.Remove(FFreeCacheList.Last);

  Assert( FCache[idx].PageID = FREE_CACHE_ID, 'Cache Slot already assigned' );

  Result := FCache[idx];
  Result.PageID := pageID;
  Result.Index := idx;
  Result.Modified := false;
  Result.MRU[0] := GetTickCount;
  Result.MRU[1] := 0;
  Result.MRU[2] := 0;
end;

function TGPageCache.NewPage: TGPageID;
var
  page : TGPage;
begin
  Assert( Active, 'PageCache is not Active' );
  Result := NextPageID;

  page := TGPage.Create( self, Result );

  FillChar(page.DataPtr[0], FHeader.PageSize, 0);
  page.Modified := True;
  page.Free;
end;

function TGPageCache.NextPageID: TGPageID;
begin
  Result := FNextPageID;
  Inc( FNextPageID );
end;

function TGPageCache.NextUniqueID: integer;
begin
  Inc( FHeader.UniqueID );
  Result := FHeader.UniqueID;
  FModified := True;
end;

procedure TGPageCache.Notify( notification : TGPageCacheNotification );
var
  idx : integer;
begin
  // Notify all of the subscribers
  for idx := FSubscribers.Count - 1 downto 0 do
    IGPageCacheObserver(FSubscribers[idx]).OnPageCacheNotification( self, notification );
end;

procedure TGPageCache.Open;
begin
  Active := True;
end;

procedure TGPageCache.ReleasePage(aPage: TGCachedPage);
begin
  Assert( aPage <> nil, 'ReleasePage passed nil Page' );
  aPage.UnLock;
end;


procedure TGPageCache.SaveToXML(el: TGXML_Element);
begin
  el.AddAttribute( 'PageSize', FHeader.PageSize, -1 );
  el.AddAttribute( 'PageCount', FHeader.AllocatedPageCount, -1 );
  el.AddAttribute( 'FreeCount', FHeader.FreePageCount, -1 );
end;

procedure TGPageCache.SetActive(const Value: boolean);
begin
  if FActive <> Value then
  begin
    if FActive then
    try
      Notify( tgpcFlush );
      Notify( tgpcBeforeClose );
      while InTransaction do
        CommitTransaction;
      ClearCache( True );
      Assert( TGLog.LogTrace( gllTwo, 'Close PageCache' ));
      InternalClose;
    except
      FActive := false;
      raise;
    end;

    FActive := Value;

    if FActive then
    try
      InternalOpen;
      Assert( TGLog.LogTrace( gllTwo, 'Open PageCache' ));
      Notify( tgpcAfterOpen );
    except
      FActive := false;
      raise;
    end;
  end;
end;

procedure TGPageCache.SetCacheSize(const Value: integer);
var
  idx : integer;
begin
  Assert( TGLog.LogTrace( gllTwo, 'PageCache.SetCacheSize: ' + IntToStr( Value )));

  ClearCache( True );

  FHashList.Free;
  FHashList := TGCacheBucketList.Create( HASH_BUCKET_DIVISOR );

  for idx := 0 to High( FCache ) do
    FCache[idx].Free;

  SetLength( FCache, Value );

  for idx := 0 to High( FCache ) do
    FCache[idx] := TGCachedPage.Create(Self, FREE_CACHE_ID );

  ClearCache( True ); // reset the FreeCache list
end;

procedure TGPageCache.SetEncryptionKey(const Value: String);
begin
  if FEncryptionKey <> Value then
  begin
    FEncryptionKey := Value;

    FreeAndNil( FCrypt );

    case GetEncryptionType of
    1 : //  TEA encryption specified
      FCrypt := TGSDBTeaCrypt.Create( Copy( EncryptionKey, Pos( ':', EncryptionKey ) + 1, Length(EncryptionKey)));
    end;
  end;
end;

procedure TGPageCache.SetSysFirstPageID(const Value: TGRowID);
begin
  if FHeader.SysFirstPageID <> Value then
  begin
    FHeader.SysFirstPageID := Value;
    FModified := True;
  end;
end;

procedure TGPageCache.SetSysLastPageID(const Value: TGRowID);
begin
  if FHeader.SysLastPageID <> Value then
  begin
    FHeader.SysLastPageID := Value;
    FModified := True;
  end;
end;

procedure TGPageCache.SetSysRowCount(const Value: integer);
begin
  if FHeader.SysRowCount <> Value then
  begin
    FHeader.SysRowCount := Value;
    FModified := True;
  end;
end;

procedure TGPageCache.Subscribe(observer : IGPageCacheObserver );
begin
  if FSubscribers.IndexOf( observer ) < 0 then
    FSubscribers.Add( observer );
end;

procedure TGPageCache.UnSubscribe(observer : IGPageCacheObserver );
begin
  FSubscribers.Remove( observer );
end;

procedure TGFilePageCache.BeginBatchWrite;
begin
  if FBatchWriteList = nil then
    FBatchWriteList := TObjectList.Create(false);

  Inc( FBatchWriteCounter );
end;

procedure TGFilePageCache.BeginTransaction;
begin
  if FTransactionCounter > 0 then
    Inc( FTransactionCounter )
  else
  begin
    Flush;  // flush all pending writes
    FTransactionPages.Clear;
    Inc( FTransactionCounter );
    WriteHeader;
  end;
end;

procedure TGFilePageCache.Clear;
var
  activeState : Boolean;
begin
  inherited;

  activeState := Active;
  Active := False;

  if FileExists( FDataFileAndPath ) then
    DeleteFile( FDataFileAndPath );

  Active := activeState;
end;

procedure TGFilePageCache.ClearBit(page: TGCachedPage; index: integer);
begin
  Assert(( index >= 0 ) and (Cardinal( index ) < FHeader.PageMapBits ), 'ClearBit: Invalid Index' );

  page.PageData[index div 8] := page.PageData[index div 8] and not ( 1 shl (index mod 8 ));

  page.Modified := true;
end;

procedure TGFilePageCache.ClearReadAhead(pageID: TGPageID);
var
  idx : integer;
begin
  FReadAheadPageID := 0;
  for idx := 0 to READ_AHEAD_BUFFERS do
    if ( pageID = FREE_CACHE_ID ) or ( FPageBufferIDs[idx] = pageID ) then
      FPageBufferIDs[idx] := FREE_CACHE_ID;
end;

procedure TGFilePageCache.CommitTransaction;
begin
  if FTransactionCounter > 0 then
  begin
    Dec( FTransactionCounter );

    if FTransactionCounter = 0 then
    begin
      //ToDo: Commit the data.
      DeleteFile( FDataFileAndPath + '.TRN' );
      FTransactionPages.Clear;
    end;
  end;
end;

function TGFilePageCache.CountUsedBits(page: TGCachedPage): Cardinal;
var
  idx : integer;
  flags : Byte;
begin
  Result := 0;

  for idx := 0 to FHeader.PageSize - 1 do
  begin
    flags := page.PageData[idx];
    while flags <> 0 do
    begin
      Inc( Result);
      flags := flags and (flags - 1);
    end;
  end;
end;

constructor TGFilePageCache.Create;
begin
  inherited;

  FTransactionPages := TGPageIDBucketList.Create( HASH_BUCKET_DIVISOR );
end;

procedure TGFilePageCache.CreateNewFileCache;
var
  mapPage : TGCachedPage;
  aStream : TFileStream;
begin
  aStream := TFileStream.Create( FDataFileAndPath, fmCreate or fmShareDenyWrite );
  Assert( aStream <> nil );
  aStream.Size := CACHE_HEADER_SIZE;  // Allocate space for the page file header

  FillChar( FHeader, SizeOf( TGPageCacheHeader ), 0 );
  FHeader.Version := TG_PAGECACHE_VERSION;
  FHeader.PageSize := CACHE_PAGE_SIZE;
  FHeader.PageMapBits := CACHE_PAGE_SIZE * 8;  // Number of bits in a PageMap
  FHeader.AllocatedPageCount := 1;

  // Calculate the checksum for Header
  FHeader.EncryptionType := GetEncryptionType;   // Set the type of encryption to use on the SDB
  if FCrypt <> nil then
    FHeader.EncryptionChecksum := FCrypt.CheckSum( @FHeader, SizeOf( TGPageCacheHeader ));

  aStream.Position := 0;
  aStream.Write( FHeader, SizeOf( TGPageCacheHeader ));

  mapPage := TGCachedPage.Create(Self, 0);
  SetBit( mapPage, 0 ); // Flag page 0 as allocated

  // Set the free space counter for this page
  SetLength( FMapPageSpace, 1 );
  FMapPageSpace[0] := FHeader.PageMapBits - 1;

  if FCrypt <> nil then
    FCrypt.Encrypt( mapPage.PageData, FHeader.PageSize );  // Encrypt

  aStream.Position := PageOffset( 0 );
  aStream.Write( mapPage.PageData[0], FHeader.PageSize);
  mapPage.Free;

  aStream.Free;
end;

function SortByPageID(Item1, Item2: Pointer) : integer;
begin
  Result := integer( TGCachedPage( Item1 ).BatchWritePageID) - integer(TGCachedPage( Item2 ).BatchWritePageID);
end;

destructor TGFilePageCache.Destroy;
begin
  FreeAndNil( FTransactionPages );
  inherited;
end;

procedure TGFilePageCache.EndBatchWrite;
var
  idx : integer;

  procedure WriteBatchedPage(page : TGCachedPage );
  var
    oldPageID : TGPageID;
  begin
    oldPageID := page.PageID;
    page.PageID := page.BatchWritePageID;
    InternalWrite( page );
    page.PageID := oldPageID;
  end;
begin
  Dec( FBatchWriteCounter );

  if FBatchWriteCounter = 0 then
  begin
    if FBatchWriteList.Count > 0 then
    begin
      FBatchWriteList.Sort(SortByPageID);  // sort by ascending pageID

      // write the last page in case the datafile needs expanding
      WriteBatchedPage( TGCachedPage( FBatchWriteList.Last ));

      // write the rest of the pages
      for idx := 0 to FBatchWriteList.Count - 2 do
        WriteBatchedPage( TGCachedPage( FBatchWriteList[idx] ));
    end;

    FreeAndNil( FBatchWriteList );
  end;
end;

function TGFilePageCache.FileSeek64( handle: Integer; const offset: Int64; origin: Integer): Int64;
begin
  Result := offset;

  Int64Rec(Result).Lo := SetFilePointer(THandle(handle), Int64Rec(Result).Lo,
    @Int64Rec(Result).Hi, origin);
end;

function TGFilePageCache.FindFreeBit(page: TGCachedPage): Cardinal;
var
  idx, jdx : integer;
  flags : Byte;
begin
  for idx := 0 to FHeader.PageSize - 1 do
    if page.PageData[idx] <> $FF then
    begin
      flags := page.PageData[idx];
      for jdx := 0 to 7 do
        if ( flags and ( 1 shl jdx )) = 0 then
        begin
          Result := idx * 8 + jdx;
          Exit;
        end;
    end;
  Result := 0;
end;

function TGFilePageCache.GetDataFileSize: Int64;
begin
  Result := FileSeek64( FDataFileHandle, 0, FILE_END);
end;

procedure TGFilePageCache.InitialiseComponents;
begin
  // Make sure the RTree is configured for this PageCache
  InitialiseRTree( FHeader.PageSize );
  InitialiseBTree( FHeader.PageSize );
  InitialiseDBTables( FHeader.PageSize );
end;

procedure TGFilePageCache.InternalClose;
begin
  ClearReadAhead(FREE_CACHE_ID);

  CloseHandle(FDataFileHandle);
  VirtualFree(FVirtualBuffer,0,MEM_RELEASE);
end;

procedure TGFilePageCache.InternalDelete(pageID: TGPageID);
var
  mapPage : TGCachedPage;
  idx : Cardinal;
begin
  idx := RowPageID( pageID ) div FHeader.PageMapBits;

  mapPage := AcquirePage( idx ); //get the Page Map for this page
  ClearBit( mapPage, RowPageID( pageID ) - idx * FHeader.PageMapBits);
  FMapPageSpace[idx] := FHeader.PageMapBits - CountUsedBits(mapPage);
  ReleasePage( mapPage );

  Inc( FHeader.FreePageCount );

  ClearReadAhead(pageID);

  FModified := True;
end;

procedure TGFilePageCache.InternalOpen;
var
  createNewFile : Boolean;
begin
  ClearReadAhead(FREE_CACHE_ID);

  // reserve enough space to fit 2 full sized pages for read ahead
  FVirtualBuffer := VirtualAlloc( nil, CACHE_PAGE_SIZE * 2, MEM_RESERVE + MEM_COMMIT, PAGE_READWRITE);

  if FDataFileAndPath <> '' then
  begin
    createNewFile := not FileExists( FDataFileAndPath );
    if createNewFile then
      CreateNewFileCache; // build an initial file

    // Attempt to open the file in ReadWrite mode
    FDataFileHandle := CreateFile( PChar(FDataFileAndPath),
      GENERIC_READ + GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_EXISTING,
      FILE_FLAG_RANDOM_ACCESS {+ FILE_FLAG_NO_BUFFERING}, 0);

    FCacheReadOnly := FDataFileHandle = INVALID_HANDLE_VALUE;  // check that the open succeded

    if FCacheReadOnly then // Open for read only
      FDataFileHandle := CreateFile( PChar(FDataFileAndPath),
        GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,
        FILE_FLAG_RANDOM_ACCESS {+ FILE_FLAG_NO_BUFFERING}, 0);

    Assert( FDataFileHandle <> INVALID_HANDLE_VALUE );

    if not FCacheReadOnly then
      RollbackTransactionFile;

    ReadHeader;

    if createNewFile then
      Notify( tgpcCreated );  // tell the world that we created a new file
  end;

  Assert( FHeader.Version = TG_PAGECACHE_VERSION, 'SDB Version Error' );
end;

procedure TGFilePageCache.InternalRead(var page: TGCachedPage);
var
  pos : Int64;
  transfered : DWord;
  idx : integer;
  pageID : TGPageID;
begin
  page.Modified := false;
  pageID := RowPageID( page.PageID );
  pos := PageOffset( pageID );

  // if Reading a page beyond the end of the file
  if pos >= FDataFileSize then
    FillChar( page.PageData[0], FHeader.PageSize, 0 )
  else
  begin
    // if requesting the Read Ahead page.
    if ( FReadAheadPageID <> 0 ) and ( pageID = FReadAheadPageID ) then
    begin
      Move(( PChar(FVirtualBuffer) + FHeader.PageSize )^, page.PageData[0], FHeader.PageSize );
      Inc( FCacheHits );
//      Dec( FCacheMisses );

      FReadAheadPageID := 0;
      Exit;
    end;

    // search the RA cache for the page
    for idx := 0 to READ_AHEAD_BUFFERS do
      if FPageBufferIDs[idx] = pageID then
      begin
        Move( FPageBuffers[idx], page.PageData[0], FHeader.PageSize );
        FPageBufferIDs[idx] := FREE_CACHE_ID;

        Inc( FCacheHits );
//        Dec( FCacheMisses );
        Exit;
      end;

    // Save the RA page only if it is not already loaded in the main cache
    if ( FReadAheadPageID <> 0 ) and ( FHashList.Find(pageID) = nil ) then
    begin
      idx := 0;
      repeat  // search for an empty slot
        if FPageBufferIDs[idx] = FREE_CACHE_ID then
          break;
        Inc( idx );
      until idx > READ_AHEAD_BUFFERS;

      if idx > READ_AHEAD_BUFFERS then  // just pick a slot at random
        idx := Random( READ_AHEAD_BUFFERS + 1 );

      FPageBufferIDs[idx] := FReadAheadPageID;
      Move(( PChar(FVirtualBuffer) + FHeader.PageSize )^, FPageBuffers[idx], FHeader.PageSize );
    end;

    // read 2 pages from the file
    FReadAheadPageID := 0;
    FileSeek64( FDataFileHandle, pos, FILE_BEGIN );
    ReadFile(FDataFileHandle, FVirtualBuffer^, FHeader.PageSize * 2, transfered,nil);

    // decrypt the page
    if FCrypt <> nil then
    begin
      FCrypt.Decrypt( FVirtualBuffer, FHeader.PageSize );
      FCrypt.Decrypt( PChar(FVirtualBuffer) + FHeader.PageSize, FHeader.PageSize );
    end;

    Move( FVirtualBuffer^, page.PageData[0], FHeader.PageSize );

    if transfered = FHeader.PageSize * 2 then
      FReadAheadPageID := pageID + 1;
  end;
end;

procedure TGFilePageCache.InternalWrite(const page: TGCachedPage);
var
  transfered : DWord;
  fileOffset : Int64;
begin
  if ReadOnly then
  begin
//    raise EGException.Create( 'TGFilePageCache.Write: File is ReadOnly - Page ' + RowIDAsString( page.PageID ));
    page.Modified := false;
    Exit;
  end;

  if FBatchWriteCounter > 0 then
  begin
    page.BatchWritePageID := page.PageID; // make a safe copy of the PageID
    FBatchWriteList.Add(page);
    Exit;
  end;

  Assert( page.PageID <> FREE_CACHE_ID, 'Internal Write passed Free Page' );

  if InTransaction then
    WriteTransactionPage( page.PageID );

  fileOffset := PageOffset( page.PageID );
  FileSeek64( FDataFileHandle, fileOffset, FILE_BEGIN);

  Move( page.PageData[0], FVirtualBuffer^, FHeader.PageSize );

  if FCrypt <> nil then
    FCrypt.Encrypt( FVirtualBuffer, FHeader.PageSize );

  WriteFile(FDataFileHandle, FVirtualBuffer^, FHeader.PageSize, transfered, nil);

  if fileOffset >= FDataFileSize then
    FDataFileSize := GetDataFileSize;  // Update the size of the data stream

  page.Modified := false;

  ClearReadAhead( page.PageID );
end;

function TGFilePageCache.InTransaction: boolean;
begin
  Result := FTransactionCounter > 0;
end;

function TGFilePageCache.NextPageID: TGPageID;
var
  mapPage : TGCachedPage;
  idx : integer;
begin
  mapPage := nil;

  for idx := 0 to High(FMapPageSpace) do
    if FMapPageSpace[idx] <> 0 then
    begin
      mapPage := AcquirePage( Cardinal( idx ) * FHeader.PageMapBits );
      Break;
    end;

  if mapPage = nil then // allocate a new mappage
  begin
    SetLength( FMapPageSpace, Length(FMapPageSpace) + 1 );
    mapPage := AcquirePage( Cardinal( High(FMapPageSpace)) * FHeader.PageMapBits );
    SetBit(mapPage, 0); // make sure that the first bit is marked as inuse
    Inc( FHeader.AllocatedPageCount )
  end;

  Result := FindFreeBit( mapPage );

  if Result = 0 then
  Assert( Result <> 0,'FilePageCache has Run out of space' );

  SetBit(mapPage, Result);
  FMapPageSpace[mapPage.PageID div (FHeader.PageMapBits)] := FHeader.PageMapBits - CountUsedBits(mapPage);
  ReleasePage( mapPage );

  Inc( Result, mapPage.PageID );  // Adjust for the mapPage base ID

  if Result >= FHeader.AllocatedPageCount then
    Inc( FHeader.AllocatedPageCount )
  else
    Dec( FHeader.FreePageCount );

  FModified := True;
end;

function TGFilePageCache.PageOffset(pageID: TGPageID): Int64;
begin
  Result := Int64( RowPageID( pageID )) * FHeader.PageSize + CACHE_HEADER_SIZE;
end;

procedure TGFilePageCache.ReadHeader;
var
  transfered : DWord;
  chksum : Cardinal;
  bitmapPages : integer;
  idx : Cardinal;
  page : TGCachedPage;
begin
  FDataFileSize := GetDataFileSize;  // Update the size of the data file

  FileSeek64( FDataFileHandle, 0, FILE_BEGIN);
  ReadFile(FDataFileHandle, FVirtualBuffer^, CACHE_HEADER_SIZE, transfered, nil);
  Move( FVirtualBuffer^, FHeader, SizeOf( TGPageCacheHeader ));

  if FHeader.PageMapBits = 0 then
    FHeader.PageMapBits := FHeader.PageSize * 8;

  if FHeader.EncryptionType > 2 then
    FHeader.EncryptionType := 0;

  // if sdb not encrypted but an encryption key was supplied
  if ( FCrypt <> nil ) and ( FHeader.EncryptionType = 0 ) then
    raise EGCryptException.Create( 'Incorrect Encryption Key supplied' );

  if FHeader.EncryptionType <> 0 then
  begin
    chksum := FHeader.EncryptionChecksum;
    FHeader.EncryptionChecksum := 0;

    if ( FCrypt = nil ) or ( chksum <> FCrypt.CheckSum( @FHeader, SizeOf( TGPageCacheHeader ))) then
      raise EGCryptException.Create('Encryption Key missing or Incorrect');
  end;

  FModified := False;

  InitialiseComponents;

  SetCacheSize( CacheSize ); // Re-Init the Cache

  // Set the free space counters for the data file
  bitmapPages := ( FDataFileSize - CACHE_HEADER_SIZE ) div ( FHeader.PageSize * FHeader.PageMapBits );

  SetLength( FMapPageSpace, bitmapPages + 1 );
  for idx := 0 to bitmapPages do
  begin
    page := AcquirePage( idx * FHeader.PageMapBits );
    FMapPageSpace[idx] := FHeader.PageMapBits - CountUsedBits(page);
    ReleasePage(page);
  end;
end;

procedure TGFilePageCache.RollbackTransaction;
begin
  if FTransactionCounter > 0 then
  begin
    FTransactionCounter := 0;
    FTransactionPages.Clear;

    RollbackTransactionFile;
    Notify(tgpcRollback);
  end;
end;

procedure TGFilePageCache.RollbackTransactionFile;
var
  handle : THandle;
  transfered : DWord;
  pageID : TGPageID;
begin
  handle := CreateFile( PChar(FDataFileAndPath + '.TRN'),
    GENERIC_READ, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

  if handle <> INVALID_HANDLE_VALUE then
  try
    ClearCache(false);
    ClearReadAhead(FREE_CACHE_ID);

    // read the Header
    FileSeek64( handle, 0, FILE_BEGIN );
    ReadFile(handle, FVirtualBuffer^, CACHE_HEADER_SIZE, transfered, nil);
    Move( FVirtualBuffer^, FHeader, SizeOf( TGPageCacheHeader ));

    FileSeek64( FDataFileHandle, 0, FILE_BEGIN );
    WriteFile( FDataFileHandle, FVirtualBuffer^, CACHE_HEADER_SIZE, transfered, nil);

    repeat
      ReadFile(handle, pageID, SizeOf( TGPageID ), transfered, nil);
      if transfered <> SizeOf( TGPageID ) then
        Exit;

      // save back to the data file.
      ReadFile(handle, FVirtualBuffer^, FHeader.PageSize, transfered, nil);
      FileSeek64( FDataFileHandle, PageOffset( pageID ), FILE_BEGIN );
      WriteFile( FDataFileHandle, FVirtualBuffer^, FHeader.PageSize, transfered, nil);
    until false;

    ReadHeader; // re-load the header
  finally
    CloseHandle(handle);
    // Clean up
    DeleteFile( FDataFileAndPath + '.TRN' );
  end;
end;

procedure TGFilePageCache.SaveToXML(el: TGXML_Element);
begin
  inherited;

  el.AddAttribute( 'Filename', FDataFile, '' );
end;

procedure TGFilePageCache.SetBit(page: TGCachedPage; index: integer);
begin
  Assert(( index >= 0 ) and (Cardinal( index ) < FHeader.PageMapBits ), 'SetBit: Invalid Index' );

  page.PageData[index div 8] := page.PageData[index div 8] or ( 1 shl ( index mod 8 ));

  page.Modified := true;
end;

procedure TGFilePageCache.SetDataFile(const Value: TFilename);
var
  ActiveState : boolean;
begin
  ActiveState := Active;
  Active := false;

  FDataFile := Value;
  FDataFileAndPath := ExpandFileName( FDataFile );

  Active := ActiveState;

  Assert( TGLog.LogTrace( gllTwo, 'FilePageCache.SetDataFile: ' + FDataFileAndPath ));
end;

procedure TGFilePageCache.WriteHeader;
var
  transfered : DWord;
begin
  if FHeader.EncryptionType <> 0 then
  begin
    Assert( FCrypt <> nil, 'Encryption Key Needed' );
    FHeader.EncryptionChecksum := 0;  // Zero as it is part of the checksum
    FHeader.EncryptionChecksum := FCrypt.CheckSum( @FHeader, SizeOf( TGPageCacheHeader ));
  end;

  FileSeek64( FDataFileHandle, 0, FILE_BEGIN);

  // Clear out the header
  FillMemory( FVirtualBuffer, CACHE_HEADER_SIZE, 0 );
  Move( FHeader, FVirtualBuffer^, SizeOf( TGPageCacheHeader ));

  WriteFile(FDataFileHandle, FVirtualBuffer^, CACHE_HEADER_SIZE, transfered, nil);

  Assert( transfered = CACHE_HEADER_SIZE, 'TGFilePageCache.WriteHeader: Failed' );

  if InTransaction then
    WriteTransactionPage( FREE_CACHE_ID );

  FModified := False;

  InitialiseComponents;
end;

procedure TGFilePageCache.WriteTransactionPage(pageID: TGPageID);
var
  handle : THandle;
  pos : Int64;
  transfered : DWord;
begin
  // Check to see if the page has already been written
  if FTransactionPages.Find(pageID) then
    Exit;

  pos := PageOffset( pageID );
  if pageID <> FREE_CACHE_ID then // Always write the header
    if pos >= FDataFileSize then  // nothing to do if Reading a page beyond the end of the file
      Exit;

  handle := CreateFile( PChar(FDataFileAndPath + '.TRN'),
      GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

  if handle = INVALID_HANDLE_VALUE then
    handle := CreateFile( PChar(FDataFileAndPath + '.TRN'),
        GENERIC_WRITE, 0, nil, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);

  Assert( handle <> INVALID_HANDLE_VALUE, 'TGFilePageCache.WriteTransactionPage: Failed' );

  try
    if pageID = FREE_CACHE_ID then  // if writing the header page
    begin
      FillMemory( FVirtualBuffer, CACHE_HEADER_SIZE, 0 );
      Move( FHeader, FVirtualBuffer^, SizeOf( TGPageCacheHeader ));
      FileSeek64( handle, 0, FILE_BEGIN );
      WriteFile(handle, FVirtualBuffer^, CACHE_HEADER_SIZE, transfered, nil);
      Exit;
    end;

    // Read in the old page
    FileSeek64( FDataFileHandle, pos, FILE_BEGIN );
    ReadFile(FDataFileHandle, FVirtualBuffer^, FHeader.PageSize, transfered, nil);

    // save it to the transaction file.
    FileSeek64( handle, 0, FILE_END );
    WriteFile(handle, pageID, SizeOf( TGPageID ), transfered, nil);
    WriteFile(handle, FVirtualBuffer^, FHeader.PageSize, transfered, nil);
  finally
    CloseHandle(handle);
    FTransactionPages.Add(pageID);
  end;
end;



constructor TGPage.Create(pageCache: TGPageCache; pageID: TGPageID);
begin
  pageID := RowPageID( pageID );
  Assert( pageID <> 0, 'TGPage: Passed 0 pageID' );

//  Assert( pageCache <> nil, 'TGPage: PageCache = nil');

  FPage := pageCache.AcquirePage(pageID);
//  Assert( FPage <> nil, 'TGPage: Bad PageID ' + RowIDToStr( pageID ));
end;

function TGPage.DataPtr: PChar;
begin
  Result := PChar(FPage.PageData);
end;

destructor TGPage.Destroy;
begin
  if FPage <> nil then
    FPage.PageCache.ReleasePage( FPage );

  inherited;
end;

function TGPage.GetModified: boolean;
begin
  Result := FPage.Modified;
end;

function TGPage.GetPageID: TGPageID;
begin
  Result := FPage.PageID;
end;

procedure TGPage.SetModified(const Value: boolean);
begin
  FPage.Modified := Value;
end;

{ TGPageIDBucketList }

procedure TGPageIDBucketList.Add(pageID: TGPageID);
var
  idx, jdx : integer;
  L, H: Integer;
begin
  idx := pageID mod Cardinal( Length( FBuckets ));

  L := 0;
  H := FBuckets[idx].Count - 1;
  while L <= H do
  begin
    jdx := (L + H) shr 1;

    // assumes that the page does not already exist in the list
    if pageID < TGPageID( FBuckets[idx][jdx] )then
      L := jdx + 1
    else
      H := jdx - 1;
  end;

  FBuckets[idx].Insert(L, TObject( pageID ));
end;

procedure TGPageIDBucketList.Clear;
var
  idx : integer;
begin
  for idx := 0 to High( FBuckets ) do
    FBuckets[idx].Clear;
end;

constructor TGPageIDBucketList.Create(bucketCount: integer);
var
  idx : integer;
begin
  SetLength( FBuckets, bucketCount );
  for idx := 0 to High( FBuckets ) do
    FBuckets[idx] := TObjectList.Create( False );
end;

destructor TGPageIDBucketList.Destroy;
var
  idx : integer;
begin
  for idx := 0 to High( FBuckets ) do
    FBuckets[idx].Free;
end;

function TGPageIDBucketList.Find(pageID: TGPageID): Boolean;
var
  idx, jdx : integer;
  L, H: Integer;
  tmpID: TGPageID;
begin
  Result := true;
  idx := pageID mod Cardinal( Length( FBuckets ));

  L := 0;
  H := FBuckets[idx].Count - 1;
  while L <= H do
  begin
    jdx := (L + H) shr 1;

    tmpID := TGPageID( FBuckets[idx][jdx] );
    if pageID = tmpID then
      Exit;
    if pageID < tmpID then
      L := jdx + 1
    else
      H := jdx - 1;
  end;
  Result := false;
end;

end.
