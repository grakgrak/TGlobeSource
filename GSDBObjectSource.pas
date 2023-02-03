//------------------------------------------------------------------------------
//Summary
//  Spatial Database Object source component
//
//Description
//Provides an object connection to the Spatial Database
//
//Author
//Graham Knight (tglobe@tglobe.com)
//------------------------------------------------------------------------------
{$I GLOBE5.INC}
unit GSDBObjectSource;

interface

Uses ActiveX, Classes, SysUtils, GClasses, GSysUtils, GSDBCache, GSDBLayers, GSpatialDatabase,
  GBitmap, GXML;

type
  TGSDBObjectSource = class( TGObjectSource )
  private
    FSDBLayer: TGSDBLayer;
    FSDB: TGSpatialDatabase;
    IgnoreIfCached : Boolean;
    procedure SetSourceName(const Value: String);
    function GetSourceName: String;
    procedure SetSDBLayer(const Value: TGSDBLayer);
  protected
    procedure SetUniqueID( Value : TGUID); override;
    function GetObjectCount : Cardinal; override;
  public
    constructor Create( parentGlobe: TGCustomGlobe5 ); override;
    destructor Destroy; override;
    function Clone : TGObjectSource; override;

    procedure EditObject( mapObject : IGMapPoint ); override;
    procedure EndEditObject( mapObject : IGMapPoint ); override;

    procedure Post; override;
    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;

    function First : TGPrimaryKey; override;
    function Next( objPK : TGPrimaryKey ) : TGPrimaryKey; override;
    function ByPrimaryKey( objPK : TGPrimaryKey ) : IGMapPoint; override;

    function NewIterator( aMER : TGMER ) : TGIterator; override;
    procedure ForEach( const aMER :TGMER; callback : TGObjectCallback); override;

    function ImageByName( const Image: TGBitmap; var ImageName : TFilename ) : Boolean; override;
    function ImportImage( const ImageName : String ) : String; override;
    procedure ImportImageStream( const ImageName : String; imageStream : TMemoryStream ); override;
    procedure ExportImageStream( const ImageName : String; imageStream : TStream ); override;

    procedure Add( obj : IGMapPoint ); override;
    procedure Remove( obj : IGMapPoint ); override;
    procedure Clear; override;
    procedure Delete; override;

    function DataMER : TGMER; override;
    function IsValid : Boolean; override;
    function IsTemporary : Boolean; override;

    property SDBLayer : TGSDBLayer read FSDBLayer write SetSDBLayer;
    
    property SDB : TGSpatialDatabase read FSDB write FSDB;
  published
    property SourceName : String read GetSourceName write SetSourceName;
  end;


implementation

Uses Globe5, GDataLink, GSDBDataLink, GMapObjects;



procedure TGSDBObjectSource.Add(obj: IGMapPoint);
var
  sdbLink : TGSDBDataLink;
begin
  inherited;

  sdbLink := TGSDBDataLink.Create( SDBLayer, 0);  // Create a new object

  // obj will take ownership of DBObject
  obj.WriteToDataLink(sdbLink);  // Save the object's properties to the DBObject

  sdbLink.Post;  // persist the data back to the DBLayer

  obj.PrimaryKey := sdbLink.PrimaryKey;  // Update the primary key of the object
end;

function TGSDBObjectSource.ByPrimaryKey( objPK: TGPrimaryKey ): IGMapPoint;
var
  sdbLink : TGSDBDataLink;
  obj : TGMapPoint;
begin
  Result := nil;
  if objPK = 0 then
    Exit;

  Result := SDB.EditCache.Find(objPK);
  if Result = nil then
    Result := SDB.BlobCache.Find(objPK);

  if Result <> nil then
  begin
    if IgnoreIfCached then
      Result := nil
    else
      Result.ObjectSource := Self;
  end
  else
  begin
    sdbLink := TGSDBDataLink.Create( SDBLayer, TGRowID( objPK ));

    case sdbLink.Kind of
    tgokPoint :
      obj := TGMapPoint.Create;
    tgokPoly :
      obj := TGMapPoly.Create;
    tgokRaster :
      obj := TGMapRaster.Create;
    else
      obj := nil;
    end;
    Assert( obj <> nil, 'Unknown Object Kind' );

    obj.ObjectSource := Self;
    obj.PrimaryKey := objPK;

    // The MapObject will now own the instance of the DataLink
    obj.ReadFromDataLink( sdbLink );

    Result := obj as IGMapPoint;

    if Result.CachableObject then
      SDB.BlobCache.Add(objPK, Result);
  end;
end;

procedure TGSDBObjectSource.Clear;
begin
  inherited;
  if SDBLayer <> nil then
    SDBLayer.Clear;
end;

function TGSDBObjectSource.Clone : TGObjectSource;
begin
  Result := TGObjectSourceClass(Self.ClassType).Create(ParentGlobe);

  SDB.SDBLayers.BeginUpdate;  // Stop notifications while cloning

  Result.UniqueID := SDB.CreateLayer('Clone of ' + Name);
  Result.CopyFrom(Self);

  SDB.SDBLayers.CancelUpdate; // We don't want a notification to fire from a clone
end;

constructor TGSDBObjectSource.Create( parentGlobe: TGCustomGlobe5 );
begin
  inherited;

  if parentGlobe <> nil then
    FSDB := TGlobe5( parentGlobe ).SpatialDatabase;
end;

function TGSDBObjectSource.DataMER: TGMER;
begin
  if SDBLayer <> nil then
    Result := SDBLayer.SpatialIndex.IndexMER
  else
    MER_Empty( Result );

  Result := MER_Union(Result, SDB.EditCache.DataMER(Self));
  Result := MER_Union(Result, SDB.BlobCache.DataMER(Self));
end;

procedure TGSDBObjectSource.Delete;
begin
  if SDBLayer <> nil then
    SDB.DeleteLayer( SDBLayer.LayerID );
  SDBLayer := nil;
end;

destructor TGSDBObjectSource.Destroy;
begin
  Name := '';

  inherited;
end;


procedure TGSDBObjectSource.EditObject(mapObject: IGMapPoint);
begin
  // make sure that the object is held in the cache while it is being edited
  if SDB.EditCache.Find(mapObject.PrimaryKey) = nil then
    SDB.EditCache.Add(mapObject.PrimaryKey, mapObject);
end;


procedure TGSDBObjectSource.EndEditObject( mapObject : IGMapPoint );
begin
  // Remove from the cache
  if SDB.EditCache.Find(mapObject.PrimaryKey) <> nil then
    SDB.EditCache.Remove(mapObject.PrimaryKey);
end;

procedure TGSDBObjectSource.ExportImageStream(const ImageName: String; imageStream: TStream);
var
  idx : integer;
  rowID : TGRowID;
  memStream : TMemoryStream;
  fileSize : Cardinal;
begin
  Assert( SDBLayer <> nil );

  idx := SDBLayer.BlobIndex.IndexOf( ExtractFileName( ImageName ));
  if idx >= 0 then
  begin
    // get the ID of the image blob
    rowID := TGRowID( SDBLayer.BlobIndex.Objects[idx] );

    memStream := TMemoryStream.Create;
    SDBLayer.BlobTable.ReadStream(rowID,memStream);

    fileSize := memStream.Size;
    imageStream.Write( fileSize, SizeOf( Cardinal ));
    imageStream.CopyFrom(memStream, 0); // copies the entire contents of fileStream over

    memStream.Free;
  end
  else
    if FileExists( imageName ) then
    begin
      memStream := TMemoryStream.Create;
      memStream.LoadFromFile(imageName);

      fileSize := memStream.Size;
      imageStream.Write( fileSize, SizeOf( Cardinal ));
      imageStream.CopyFrom(memStream, 0); // copies the entire contents of fileStream over

      memStream.Free;
    end;
end;

function TGSDBObjectSource.First: TGPrimaryKey;
begin
  if ( SDBLayer <> nil ) and SDB.Active then
    Result := SDBLayer.ObjectTable.First
  else
    Result := 0;
end;

procedure TGSDBObjectSource.ForEach(const aMER: TGMER; callback: TGObjectCallback);
begin
  if not SDB.Active then
    Exit;
  IgnoreIfCached := true;

  if SDBLayer <> nil then
    SDBLayer.SpatialIndex.Search( aMER, CullObjectSize, callback );

  IgnoreIfCached := false;

  SDB.EditCache.ForEach( Self, aMER,CullObjectSize, callback );
  SDB.BlobCache.ForEach( Self, aMER, CullObjectSize, callback );
end;

function TGSDBObjectSource.GetObjectCount: Cardinal;
begin
  if SDBLayer <> nil then
    Result := SDBLayer.ObjectCount
  else
    Result := 0;
end;

function TGSDBObjectSource.GetSourceName: String;
begin
  Result := MetaData.Values['SourceName'];
end;

function TGSDBObjectSource.ImageByName( const Image: TGBitmap; var ImageName : TFilename ) : Boolean;
var
  filename : TFilename;
  idx : integer;
  rowID : TGRowID;
  fileStream : TFileStream;
begin
  Assert( SDBLayer <> nil );

  idx := SDBLayer.BlobIndex.IndexOf( ExtractFileName( ImageName ));
  Result := ( idx >= 0 );

  if Result then
  begin
    // get the ID of the image blob
    rowID := TGRowID( SDBLayer.BlobIndex.Objects[idx] );

    //ToDo: Have to find a better way to load images into the TGBitmap
    filename := ExtractFilePath(SDB.DataSourceName) + '_TG_TMP_' + ExtractFileName( ImageName );
    DeleteFile( filename );

    fileStream := TFileStream.Create(filename, fmCreate );
    SDBLayer.BlobTable.ReadStream(rowID,fileStream);
    fileStream.Free;

    Image.LoadFromFile( filename );
    DeleteFile( filename );
  end;
end;

function TGSDBObjectSource.ImportImage( const ImageName : String ) : String;
var
  memStream : TMemoryStream;
begin
  Assert( SDBLayer <> nil );

  Result := ExtractFilename( ImageName );

  if SDBLayer.BlobIndex.IndexOf( Result ) < 0 then
    if FileExists( ImageName ) then
    begin
      memStream := TMemoryStream.Create;
      memStream.LoadFromFile( ImageName );
      ImportImageStream( Result, memStream );

      memStream.Free;
    end;
end;

procedure TGSDBObjectSource.ImportImageStream(const ImageName: String;
  imageStream: TMemoryStream);
var
  rowID : TGRowID;
begin
  Assert( SDBLayer <> nil );

  SDBLayer.BlobTable.AppendString(ImageName); // Save the Name of the Image Blob
  
  rowID := SDBLayer.BlobTable.AppendStream(imageStream);

  // Update the index;
  SDBLayer.BlobIndex.AddObject( ImageName, TObject( rowID ));
end;

function TGSDBObjectSource.IsTemporary: Boolean;
begin
  Result := false;  // This is a permanent data storeage
end;

function TGSDBObjectSource.IsValid: Boolean;
begin
  Result := SDBLayer <> nil;
end;

procedure TGSDBObjectSource.LoadFromXML(Element: TGXML_Element);
begin
  // Make sure SourceName is pre-loaded
  if Element <> nil then
    SourceName := Element.Attribute('SourceName', '' );

  inherited;
end;


function TGSDBObjectSource.NewIterator(aMER: TGMER): TGIterator;
begin
  Result := TGListIterator.Create( Self );

  if not SDB.Active then
    Exit;

  if SDBLayer <> nil then
    SDBLayer.SpatialIndex.Search( aMER, CullObjectSize, TGListIterator(Result).PrimaryKeyList );

  FSDB.EditCache.FilterIterator( Self, aMER, TGListIterator(Result));
  FSDB.BlobCache.FilterIterator( Self, aMER, TGListIterator(Result));
end;

function TGSDBObjectSource.Next(objPK: TGPrimaryKey): TGPrimaryKey;
begin
  if ( SDBLayer <> nil ) and ( objPK <> 0 ) then
    Result := SDBLayer.ObjectTable.Next( objPK )
  else
    Result := 0;
end;

procedure TGSDBObjectSource.Post;
begin
  inherited;

  // Save any existing data for this layer
  if ( SDBLayer <> nil ) and ( not SDB.ReadOnly ) then
  begin
    // Check to see if the metadata has changed
    if MetaData.CommaText <> SDBLayer.MetaData.CommaText then
    begin
      SDBLayer.MetaData.Assign( MetaData );
      SDBLayer.Flush;
    end;
  end;
end;

procedure TGSDBObjectSource.Remove(obj: IGMapPoint);
begin
  SDB.BlobCache.Remove( obj.PrimaryKey );
  Layer.Selection.Remove( obj.PrimaryKey );  // remove from selection
  obj.ObjectSource := nil;
end;

procedure TGSDBObjectSource.SaveToXML(Element: TGXML_Element);
begin
  inherited;

  Element.AddAttribute('SourceName', SourceName, '');
end;

procedure TGSDBObjectSource.SetSDBLayer(const Value: TGSDBLayer);
begin
  if ( FSDBLayer <> nil ) and ( Value <> nil ) then
    if IsEqualGUID( FSDBLayer.LayerID, Value.LayerID ) then
      Exit;

  FSDBLayer := Value;

  if FSDBLayer <> nil then
  begin
    MetaData.Assign( FSDBLayer.MetaData ); // Load the metadata from the Database

    Presenters.AsXMLString := MetaData.Values['PresenterXML'];
    Presenters.Modified := false;

    Name := FSDBLayer.Name;

    FUniqueID := FSDBLayer.LayerID;
  end
  else
    FUniqueID := NULL_GUID;
end;

procedure TGSDBObjectSource.SetSourceName(const Value: String);
begin
  MetaData.Values['SourceName'] := Value;
end;

procedure TGSDBObjectSource.SetUniqueID(Value: TGUID);
var
  retry : Boolean;
  dbLayer : TGSDBLayer;
begin
  inherited;

  if SDB = nil then
    Exit;

  if IsEqualGUID( FUniqueID, NULL_GUID ) then  // Save the ObjectSource if necessary
    Post;

  dbLayer := SDB.SDBLayers.LayerByID(Value);

  while dbLayer = nil do // Try to locate the layer by Name
  begin
    dbLayer := SDB.SDBLayers.LayerByName( Name );
    //ToDo: Need to re-think how this works
    // We should get the layerID of the newly imported layer returned from
    // DoOnMissingLayer so that we are not Name dependant.
    retry := false;
    if dbLayer = nil then  // if we failed to find the layer
    begin
      SDB.BeginUpdate;
      TGlobe5( ParentGlobe ).DoOnMissingLayer( Name, SourceName, UserXML, retry );
      SDB.EndUpdate;
      SDB.NotifyChanged;
    end;
    if retry = false then
      Break;
  end;

  SDBLayer := dbLayer;
end;

initialization
  RegisterGlobeClass( TGSDBObjectSource, 'SDB Object Source');
end.

