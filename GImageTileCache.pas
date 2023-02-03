//------------------------------------------------------------------------------
//Summary
//  TGlobe Tile cache component
//
//Description
//Provides cached image tiles for TGlobe
//
//Author
//Graham Knight (tglobe@tglobe.com)
//------------------------------------------------------------------------------

//Todo:fix stitch lines on images.
//ToDo: improve Disk cache tile removal strategy

unit GImageTileCache;

interface

uses
  Windows, Messages, SysUtils, Classes, GClasses, GXML, Graphics, GBitmap,
  SyncObjs, ScktComp, Forms;

const
 TILE_CACHE_SZ = 64;
 MAX_JOB_THREADS = 7; // has to be 2 or more
 MAX_QUEUE_LEN = MAX_JOB_THREADS;
 MAX_DISK_MB = 5;

type
  TGImageTileCache = class;
  TGWorkerThreadPool = class;
  TGCustomJob = class;

  TGJobThread = class( TThread )
  private
    FJobID : string;
    FJobData : TGCustomJob;
    FThreadPool: TGWorkerThreadPool;
  protected
    procedure Execute; override;
  public
    constructor Create( threadPool : TGWorkerThreadPool );
    procedure RunJob( jobID : string; data : TGCustomJob );
  end;

  TGWorkerThreadPool = class
  private
    FQueue : TStringList;
    FRunning : TStringList;
    FThreads : array of TGJobThread;
    FActiveJobs : integer;
    FThreadLock : TCriticalSection;
    FQueueLock : TCriticalSection;
    function StartJob( const jobID : string; data : TGCustomJob ) : TGJobThread;
  public
    constructor Create;
    destructor Destroy; override;

    function GetNextQueuedJob( jobThread : TGJobThread ) : Boolean;
    function IsJobInQueue( const jobID : string ) : Boolean;
    procedure ProcessQueue;
    procedure CancelJob( const jobID : string );
    procedure JobDone( const jobID : string );
    procedure QueueJob( const jobID : string; data : TGCustomJob );
    property ActiveJobs : integer read FActiveJobs;
  end;


  // Thread jobs
  TGCustomJob = class
  private
    FTerminated: Boolean;
  protected
    procedure Execute( jobThread : TGJobThread ); virtual; abstract;
    property Terminated : Boolean read FTerminated write FTerminated;
  end;

  TGURLTileJob = class(TGCustomJob)
  private
    FGlobe : TGCustomGlobe5;
    FKey : string;
    FTileURL : string;
    FTileStream : TMemoryStream;
    FCache : TGImageTileCache;

    procedure DownloadFile;
    procedure UpdateGlobe;
  public
    constructor Create( cache : TGImageTileCache );
    procedure Execute( jobThread : TGJobThread ); override;

    property Globe : TGCustomGlobe5 read FGlobe write FGlobe;
    property TileKey : string read FKey write FKey;
    property TileURL : string read FTileURL write FTileURL;
    property TileStream : TMemoryStream read FTileStream;
  end;

  TGDiskCacheJob = class(TGCustomJob)
  private
    FCache : TGImageTileCache;
  public
    constructor Create( cache : TGImageTileCache );
    procedure Execute( jobThread : TGJobThread ); override;
  end;

  TGImageTileCache = class(TComponent)
  private
    { Private declarations }
    FTileCache : TStringList;
    FTileCacheSize: integer;
    FTileCacheCS : TCriticalSection;

    FThreadPool : TGWorkerThreadPool;
    FDiskCachePath: TFilename;
    FDiskCacheMB: integer;

    procedure SetTileCacheSize(const Value: integer);
    procedure SetDiskCachePath(const Value: TFilename);
    procedure SetDiskCacheMB(const Value: integer);

    function KeyToFilename( const key : string ) : string;
  protected
    { Protected declarations }
    function FetchFromCache( const key : string ) : TGBitmap;
    function FetchFromDisk( const key : string ) : TGBitmap;
    function StreamToBitmap( aStream : TStream ) : TGBitmap;

    function LoadTileFromDMR( elMRI, elMosaic, elRow, elCol : TGXML_Element ) : TGBitmap;
    procedure SaveTileToCache( const key : string; img : TGBitmap );
    procedure SaveStreamToDisk( const key : string; aStream : TStream );
    procedure ReduceDiskCache();
  public
    { Public declarations }
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;

    function FetchMRITile( globe : TGCustomGlobe5; elMRI : TGXML_Element; level, row, col: integer ) : TGBitmap;
    function FetchURLTile( globe : TGCustomGlobe5; url : string; download : Boolean = true ) : TGBitmap;

    procedure LoadFromXML(Element : TGXML_Element);
    procedure SaveToXML(Element : TGXML_Element);
  published
    { Published declarations }
    property TileCacheSize : integer read FTileCacheSize write SetTileCacheSize;
    property DiskCachePath : TFilename read FDiskCachePath write SetDiskCachePath;
    property DiskCacheMB : integer read FDiskCacheMB write SetDiskCacheMB;
  end;


procedure Register;

implementation

uses Globe5, jpeg, {$ifdef USE_PNG}pngimage,{$endif} GLogging;

procedure Register;
begin
  RegisterComponents('TGlobe', [TGImageTileCache]);
end;

{ TGURLTileJob }

constructor TGURLTileJob.Create(cache: TGImageTileCache);
begin
  inherited Create;
  FCache := cache;
end;

procedure TGURLTileJob.DownloadFile;
var
  rc: integer;
  s: string;
  szBuffer: array[0..4095] of Char;
begin
  FTileStream := TMemoryStream.Create;

  with TClientSocket.Create( nil ) do
  try
    Host := Copy( FTileURL, 1, Pos( '/', FTileURL ) - 1);
    ClientType := ctBlocking;

    Port := 80;

    try
      Open;

      // Send the request for the data
      s := 'GET http://' + FTileURL + ' HTTP/1.0'#13#10 +
           'Host: ' + Host + #13#10#13#10;

//      s := 'GET /' + Copy( FTileURL, Pos( '/', FTileURL ) + 1, 255) + ' HTTP/1.1'#13#10 +
//           'Host: ' + Host + #13#10#13#10;

      Assert( TGLog.LogTrace( gllOne, 'Downloading: ' + FTileURL ));

      rc := Socket.SendBuf( Pointer(s)^, Length(s));

      if rc <= 0 then
        Exit;

      while not Terminated and ( rc > 0 ) do
      begin
        rc := Socket.ReceiveBuf(szBuffer, SizeOf(szBuffer));

        if rc > 0 then
          FTileStream.Write(szBuffer, rc);
      end;

      if not Terminated then
      begin
        // skip over the http response header
        rc := integer( StrPos( PChar( FTileStream.Memory ), #13#10#13#10 ) + 4 - PChar( FTileStream.Memory ));

        if ( rc > 0 ) and ( rc < FTileStream.Size ) then
        begin
          FTileStream.Position := rc;
          FCache.SaveTileToCache( TileKey, FCache.StreamToBitmap( FTileStream ));

          FTileStream.Position := rc;
          FCache.SaveStreamToDisk( TileKey, FTileStream );
        end;
      end;
    except
      FTileStream.Clear;
    end;
  finally
    Close;
    Free;

    FTileStream.Free;
  end;
end;

procedure TGURLTileJob.Execute( jobThread : TGJobThread );
begin
  DownloadFile;

  if not Terminated then
    jobThread.Synchronize( UpdateGlobe );
end;

procedure TGURLTileJob.UpdateGlobe;
begin
  TGlobe5(Globe).RedrawLayers;
end;

{ TGImageTileCache }

constructor TGImageTileCache.Create(AOwner: TComponent);
var
  jobThread : TGJobThread;
begin
  inherited;

  TClientSocket.Create( self ); // to pre-load the tcpip dlls and hold them in memory

  FDiskCacheMB := MAX_DISK_MB;
  FTileCacheSize := TILE_CACHE_SZ;
  FTileCache := TStringList.Create;
  FTileCacheCS := TCriticalSection.Create;

  FThreadPool := TGWorkerThreadPool.Create;

  jobThread := FThreadPool.StartJob( '_DiskCacheJob_', TGDiskCacheJob.Create(Self));

  if jobThread <> nil then
    jobThread.Priority := tpLowest;
end;

destructor TGImageTileCache.Destroy;
begin
  FreeAndNil( FThreadPool );

  while FTileCache.Count > 0 do
  begin
    FTileCache.Objects[0].Free;
    FTileCache.Delete(0);
  end;
  FreeAndNil( FTileCache );

  FTileCacheCS.Free;
  inherited;
end;

function TGImageTileCache.FetchFromCache(const key: string): TGBitmap;
var
  idx : integer;
begin
  FTileCacheCS.Acquire;
  idx := FTileCache.IndexOf(key);

  if idx >= 0 then
  begin
    // fetch tile from cache
    Result := TGBitmap( FTileCache.Objects[idx] );
    FTileCache.Move(idx, 0);
  end
  else
    Result := nil;

  FTileCacheCS.Release;
end;

function TGImageTileCache.FetchFromDisk(const key: string ): TGBitmap;
var
  filename : string;
  aStream : TFileStream;
begin
  Result := nil;

  if DiskCachePath = '' then
    Exit;

  FTileCacheCS.Acquire;

  filename := KeyToFilename(key);
  if FileExists( filename ) then
  begin
    aStream := TFileStream.Create( filename, fmOpenRead );
    Result := StreamToBitmap( aStream );
    aStream.Free;

    if Result = nil then  // if the cached image is bad
      DeleteFile(filename);
  end;
  FTileCacheCS.Release;
end;

function TGImageTileCache.FetchMRITile( globe : TGCustomGlobe5; elMRI : TGXML_Element;
  level, row, col: integer): TGBitmap;
var
  filename, imageKey : string;
  elMosaic, elRow, elCol : TGXML_Element;
begin
  filename := elMRI.Attribute('XMLFilename', '' );

  imageKey := filename + ':' + IntToStr(level) + ':' + IntToStr(row) + ':' + IntToStr(col);

  Result := FetchFromCache( imageKey );

  if Result = nil then
  begin
    elMosaic := elMRI.Element( 'Mosaic', level );
    if elMosaic = nil then
      Exit;

    elRow := elMosaic.Element(row);
    if elRow = nil then
      Exit;

    elCol := elRow.Element(col);
    if elCol = nil then
      Exit;

    // pull in the tile
    Result := LoadTileFromDMR( elMRI, elMosaic, elRow, elCol );

    SaveTileToCache(imageKey, Result);
  end;

  // Checks Queue to see if there are more jobs to be run.
  FThreadPool.ProcessQueue;
end;

function TGImageTileCache.FetchURLTile(globe: TGCustomGlobe5; url: string; download : Boolean ): TGBitmap;
var
  job : TGURLTileJob;
begin
  Result := FetchFromCache( url );

  if Result = nil then
  begin
    Result := FetchFromDisk( url );

    if Result <> nil then
      SaveTileToCache(url, Result);
  end;

  if Result = nil then
    if FThreadPool.IsJobInQueue( url ) = false then
    begin
      job := TGURLTileJob.Create( self );
      job.Globe := globe;
      job.TileKey := url;
      job.TileURL := url;

      FThreadPool.QueueJob(url, job);

//      SaveTileToCache( url, nil );
    end;

  // Checks Queue to see if there are more jobs to be run.
  FThreadPool.ProcessQueue;
end;

function TGImageTileCache.KeyToFilename(const key: string): string;
begin
  Result := key;

  Result := StringReplace( Result, '\', '_', [rfReplaceAll]);
  Result := StringReplace( Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace( Result, '?', '_', [rfReplaceAll]);

  Result := Trim( DiskCachePath ) + '\' + Result;
end;

procedure TGImageTileCache.LoadFromXML(Element: TGXML_Element);
begin
  if Element <> nil then
  begin
    TileCacheSize := Element.Attribute('TileCacheSize', TILE_CACHE_SZ);
    DiskCacheMB := Element.Attribute('DiskCacheMB', MAX_DISK_MB);
  end;
end;

function TGImageTileCache.LoadTileFromDMR( elMRI, elMosaic, elRow, elCol: TGXML_Element): TGBitmap;
var
  FDatFile : TFileStream;
  len : integer;
  filename : string;
  memStream : TMemoryStream;
begin
  Result := nil;
  filename := elMRI.Attribute('XMLFilename', '' );

  if not FileExists( ChangeFileExt( filename, '.dmr' )) then
    Exit;

  FDatFile := TFileStream.Create( ChangeFileExt( filename, '.dmr' ), fmOpenRead or fmShareDenyWrite );

  FDatFile.Position := StrToInt64Def( elCol.BodyText, 0 );
  FDatFile.Read( len, SizeOf(len)); // get the length of the data to read

  memStream := TMemoryStream.Create;
  memStream.CopyFrom( FDatFile, len );
  memStream.Position := 0;

  FDatFile.Free;

  Result := StreamToBitmap( memStream );
end;

procedure TGImageTileCache.ReduceDiskCache;
var
  rSearch : TSearchRec;
  sl : TStringList;
  idx, jdx : integer;
begin
  if DiskCachePath <> '' then
  begin
    FTileCacheCS.Acquire;

    sl := TStringList.Create;

    if FindFirst(DiskCachePath + '\*.*', 0, rSearch ) = 0 then
    repeat
      sl.Add(rSearch.Name);
    until FindNext(rSearch) <> 0;

    // remove 10% of the cached tiles
    for idx := 0 to sl.Count div 10 do
    begin
      jdx := Random(sl.Count);
      if sl[jdx] <> '' then
        DeleteFile(DiskCachePath + '\' + sl[jdx]);
      sl[jdx] := '';
    end;

    sl.Free;
    FTileCacheCS.Release;
  end;
end;

procedure TGImageTileCache.SaveStreamToDisk(const key: string; aStream: TStream);
var
  fileStream : TFileStream;
begin
  FTileCacheCS.Acquire;

  if DiskCachePath <> '' then
    if ForceDirectories(DiskCachePath) then
    begin
      fileStream := TFileStream.Create( KeyToFilename( key ), fmCreate );

      fileStream.CopyFrom( aStream, aStream.Size - aStream.Position );
      fileStream.Free;
    end;

  FTileCacheCS.Release;
end;

procedure TGImageTileCache.SaveTileToCache(const key: string; img: TGBitmap);
var
  idx : integer;
begin
  if img = nil then
    Exit;

  FTileCacheCS.Acquire;

  idx := FTileCache.IndexOf(key);
  if idx > 0 then
  begin
    FTileCache.Objects[idx].Free;
    FTileCache.Delete(idx);
  end;

  FTileCache.InsertObject(0, key, img);

  while FTileCache.Count > FTileCacheSize do
  begin
    FThreadPool.CancelJob( FTileCache[FTileCacheSize] );

    FTileCache.Objects[FTileCacheSize].Free;
    FTileCache.Delete(FTileCacheSize);
  end;

  FTileCacheCS.Release;
end;

procedure TGImageTileCache.SaveToXML(Element: TGXML_Element);
begin
  Element.AddAttribute('TileCacheSize', TileCacheSize, TILE_CACHE_SZ);
  Element.AddAttribute('DiskCacheMB', DiskCacheMB, MAX_DISK_MB);
end;

procedure TGImageTileCache.SetDiskCacheMB(const Value: integer);
begin
  FDiskCacheMB := Value;
end;

procedure TGImageTileCache.SetDiskCachePath(const Value: TFilename);
begin
  FDiskCachePath := Trim( Value );
end;

procedure TGImageTileCache.SetTileCacheSize(const Value: integer);
begin
  while Value < FTileCache.Count do
  begin
    FTileCache.Objects[FTileCache.Count - 1].Free;
    FTileCache.Delete(FTileCache.Count - 1);
  end;
  FTileCacheSize := Value;
end;

function TGImageTileCache.StreamToBitmap( aStream: TStream ): TGBitmap;
var
  img : TGraphic;
  buf : array [0..3] of char;
begin
  Result := nil;

  if aStream <> nil then
  try
    Result := TGBitmap.Create;
    Result.Canvas.Lock;

    aStream.Read(buf, 4);
    aStream.Position := aStream.Position - 4; // reset the stream

{$ifdef USE_PNG}
    // if a PNG image
    if ( buf[1] = 'P' ) and ( buf[2] = 'N' ) and ( buf[3] = 'G' ) then
    begin
      img := TPNGObject.Create;
      try
        img.LoadFromStream(aStream);
        Result.Width := TPNGObject(img).Width;
        Result.Height := TPNGObject(img).Height;
        TPNGObject(img).DrawUsingPixelInformation(Result.Canvas, Point(0,0));
      finally
        img.Free;
      end;
    end
    else
{$endif}
      if ( buf[0] = 'B' ) and ( buf[1] = 'M' ) then // load in a bitmap
      begin
        img := TBitmap.Create;
        try
          img.LoadFromStream(aStream);
          Result.Canvas.Draw(0,0,img);
        finally
          img.Free;
        end;
      end
      else
      begin // assume a jpg
        img := TJPEGImage.Create;
        try
          img.LoadFromStream(aStream);
          Result.Width := TJPEGImage(img).Width;
          Result.Height := TJPEGImage(img).Height;
          TJPEGImage(img).Smoothing := false;
          TJPEGImage(img).ProgressiveDisplay := false;
          TJPEGImage(img).DIBNeeded;
          Result.Canvas.Draw(0,0,img);
        finally
          img.Free;
        end;
      end;

    Result.Canvas.UnLock;
  except
    Result.Free;
    Result := nil;
  end;
end;

{ TGWorkerThreadPool }

procedure TGWorkerThreadPool.CancelJob(const jobID: string);
var
  idx : integer;
begin
  FQueueLock.Acquire;
  idx := FQueue.IndexOf( jobID );
  if idx >= 0 then
  begin
    FQueue.Objects[idx].Free;
    FQueue.Delete(idx);
  end;
  FQueueLock.Release;
end;

constructor TGWorkerThreadPool.Create;
var
  idx : integer;
begin
  FThreadLock := TCriticalSection.Create;
  FQueueLock := TCriticalSection.Create;
  FQueue := TStringList.Create;
  FRunning := TStringList.Create;

  SetLength( FThreads, MAX_JOB_THREADS );

  for idx := 0 to High( FThreads ) do
    FThreads[idx] := TGJobThread.Create(self);
end;

destructor TGWorkerThreadPool.Destroy;
var
  idx : integer;
begin
  FThreadLock.Acquire;
  for idx := 0 to High( FThreads ) do
  begin
    if not FThreads[idx].Suspended then
      FThreads[idx].FJobData.Terminated := true;

    FThreads[idx].Terminate;
    Sleep(0);
  end;

  FThreadLock.Release;

  Sleep( 1000 ); // give the threads a chance to terminate

  FreeAndNil( FRunning );
  FreeAndNil( FQueue );
  FreeAndNil( FThreadLock );
  FreeAndNil( FQueueLock );

  inherited;
end;

function TGWorkerThreadPool.GetNextQueuedJob( jobThread: TGJobThread ): Boolean;
begin
  FQueueLock.Acquire;

  Result := FQueue.Count > 0;

  if Result then
  begin
    jobThread.FJobID := FQueue[0];
    jobThread.FJobData := FQueue.Objects[0] as TGCustomJob;
    FQueue.Delete(0);

    FRunning.Add(jobThread.FJobID); // add to the running job list
  end;

  FQueueLock.Release;
end;

function TGWorkerThreadPool.IsJobInQueue(const jobID: string): Boolean;
var
  jobIdx : integer;
begin
  FQueueLock.Acquire;

  // check to see if the job is already running
  Result := FRunning.IndexOf(jobID) >= 0;

  if not Result then
  begin
    jobIdx := FQueue.IndexOf(jobID);
    Result := jobIdx >= 0;

    if Result then
      FQueue.Move( jobIdx, 0 ); // pull job to front of queue
  end;

  FQueueLock.Release;
end;

procedure TGWorkerThreadPool.JobDone(const jobID: string);
var
  idx : integer;
begin
  FQueueLock.Acquire;

  idx := FRunning.IndexOf(jobID);
  if idx >= 0 then
    FRunning.Delete(idx);

  FQueueLock.Release;
end;

procedure TGWorkerThreadPool.ProcessQueue;
var
  idx : integer;
begin
  if FQueue.Count > 0 then
    if ActiveJobs < MAX_JOB_THREADS then
    begin
      FThreadLock.Acquire;

      for idx := 0 to High( FThreads ) do
        if FThreads[idx].Suspended then
        begin
          GetNextQueuedJob( FThreads[idx] );
          FThreads[idx].Resume;
          Break;
        end;

      FThreadLock.Release;
    end;
end;

procedure TGWorkerThreadPool.QueueJob(const jobID : string; data: TGCustomJob);
var
  jobIdx : integer;
begin
//  FThreadLock.Acquire;
  if ActiveJobs < High( FThreads ) then
  begin
    if data <> nil then
      StartJob( jobID, data );
  end
  else
  begin
    FQueueLock.Acquire;

    jobIdx := FQueue.IndexOf(jobID);

    if jobIdx < 0 then
    begin
      if data <> nil then
        FQueue.InsertObject( 0, jobID, data );
      if FQueue.Count > MAX_QUEUE_LEN then
        FQueue.Delete(FQueue.Count - 1);
    end
    else
      FQueue.Move( jobIdx, 0 );

    FQueueLock.Release;
  end;
//  FThreadLock.Release;
end;

function TGWorkerThreadPool.StartJob(const jobID : string; data: TGCustomJob) : TGJobThread;
var
  idx : integer;
begin
  FThreadLock.Acquire;
  try
    Result := nil;

    for idx := 0 to High( FThreads ) do
      if FThreads[idx].Suspended then
      begin
        FQueueLock.Acquire;
        FRunning.Add(jobID);
        FQueueLock.Release;

        Result := FThreads[idx];

        Result.RunJob( jobID, data );
        Exit;
      end;

    raise EInvalidOperation.Create('Failed to find available thread');
  finally
    FThreadLock.Release;
  end;
end;

{ TGJobThread }

constructor TGJobThread.Create(threadPool: TGWorkerThreadPool);
begin
  inherited Create( true );

  FThreadPool := threadPool;
  FreeOnTerminate := True;

end;

procedure TGJobThread.Execute;
begin
  while not Terminated do
  begin
    assert( FJobData <> nil );

    FJobData.Terminated := false;
    FJobData.Execute( self );
    FreeAndNil( FJobData );

    if not Terminated then
    begin
      // remove from the running list
      FThreadPool.JobDone(FJobID);

      // check to see if there is a job on the queue to run
      if FThreadPool.GetNextQueuedJob( self ) = false then
      begin
        InterlockedDecrement(FThreadPool.FActiveJobs);
        Suspend;
      end;
    end;
  end;
end;

procedure TGJobThread.RunJob(jobID : string; data: TGCustomJob);
begin
  FJobID := jobID;
  FJobData := data;
  InterlockedIncrement(FThreadPool.FActiveJobs);
  Resume;
end;

{ TGDiskCacheJob }

constructor TGDiskCacheJob.Create(cache: TGImageTileCache);
begin
  inherited Create;
  FCache := cache;
end;

procedure TGDiskCacheJob.Execute(jobThread: TGJobThread);
var
  rSearch : TSearchRec;
  totalSize : integer;
begin
  // periodically check for something to do.
  while not Terminated do
  begin
    Sleep( 5000 );  // sleep for 5 seconds

    if FCache.DiskCachePath <> '' then
      if FCache.DiskCacheMB > 0 then
      begin
        totalSize := 0;
        
        if FindFirst(FCache.DiskCachePath + '\*.*', 0, rSearch ) = 0 then
        repeat
          totalSize := totalSize + rSearch.Size;
        until Terminated or ( FindNext(rSearch) <> 0 );

        FindClose( rSearch );

        // if using more disk than allowed
        if not Terminated then
          if totalSize / ( 1024.0 * 1024.0 ) > FCache.DiskCacheMB then
            FCache.ReduceDiskCache();
      end;
  end;
end;

end.
