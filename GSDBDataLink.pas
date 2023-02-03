//------------------------------------------------------------------------------
//Summary
//TGlobe Spatial Database data link class
//
//Description
//Provides a data read write link between the TGlobe map objects and the spatial
//database.
//
//Author
//Graham Knight (tglobe@tglobe.com)                                             
//------------------------------------------------------------------------------
unit GSDBDataLink;

interface

Uses Windows, SysUtils, Classes, GSDBCache, GDataLink, GSDBLayers, GSysUtils,
  GBitmap, GXML;

type
  TGDBObjectRec = packed record
    Kind : TGDBObjectKind;
    State : TGMapObjectStateSet;
    DateTime : TDateTime;
    PresenterID: SmallInt;
    MER: TGMER;
    Centroid : TGPointLL;

     // Links to associated object data
    DataID : TGRowID;  // points to a chain store
    AttributeID : TGRowID; // points to a CSL of strings
    UserID : TGRowID; // points to a string holding the UserID value
    SpareID : TGRowID; // For future use
  end;

  //---------------------------------------------
  //Description
  //This is a wrapper class for the TGDBObjectRec
  //---------------------------------------------
  TGSDBDataLink = class( TGDataLink )
  private
    FDBObjectRec : TGDBObjectRec;
    FDBLayer : TGSDBLayer;
    FPrimaryKey: TGRowID;
  protected
    function GetCentroid: TGPointLL; override;
    function GetDateTime: TDateTime; override;
    function GetMER: TGMER; override;
    function GetPresenterID: SmallInt; override;
    function GetAttributeID: TGRowID; override;
    function GetPrimaryKey: TGPrimaryKey; override;
    function GetKind: TGDBObjectKind; override;
    function GetDataID: TGRowID; override;
    function GetObjectDataCSL: String; override;
    function GetState: TGMapObjectStateSet; override;
    function GetUserID: String; override;

    procedure SetCentroid(const Value: TGPointLL); override;
    procedure SetDateTime(const Value: TDateTime); override;
    procedure SetMER(const Value: TGMER); override;
    procedure SetPresenterID(const Value: SmallInt); override;
    procedure SetAttributeID(const Value: TGRowID); override;
    procedure SetKind(const Value: TGDBObjectKind); override;
    procedure SetDataID(const Value: TGRowID); override;
    procedure SetObjectDataCSL(const Value: String); override;
    procedure SetState(const Value: TGMapObjectStateSet); override;
    procedure SetUserID(const Value: String); override;

    procedure WriteObjectBase; override;
    procedure ReadObjectBase; override;
  public
    constructor Create( layer : TGSDBLayer; rootID : TGRowID );
    procedure Assign( DBObject : TGSDBDataLink ); reintroduce;

    procedure SaveToXML( el : TGXML_ELement );

    procedure Clear; override;
    procedure Delete; override;
    procedure ReadDataStream( memStream : TMemoryStream ); override;
    procedure WriteDataStream( memStream : TMemoryStream ); override;

    property DBLayer : TGSDBLayer read FDBLayer;
  end;

implementation

Uses GLogging;

{ TGSDBDataLink }

procedure TGSDBDataLink.Assign(DBObject: TGSDBDataLink);
var
  memStream : TMemoryStream;
begin
  State := DBObject.GetState;
  Kind := DBObject.Kind;
  MER := DBObject.MER;
  Centroid := DBObject.Centroid;
  PresenterID := DBObject.PresenterID;
  ObjectDataCSL := DBObject.ObjectDataCSL;
  UserID := DBObject.UserID;
  DateTime := DBObject.DateTime;

  // copy over the object data
  memStream := TMemoryStream.Create;
  DBObject.ReadDataStream(memStream);
  WriteDataStream(memStream);
  memStream.Free;
end;

procedure TGSDBDataLink.Clear;
begin
  inherited;
  FillChar( FDBObjectRec, SizeOf( FDBObjectRec), 0 );
  FPrimaryKey := 0;
end;

constructor TGSDBDataLink.Create( layer: TGSDBLayer; rootID : TGRowID );
begin
  FDBLayer := layer;
  FPrimaryKey := rootID;

  if rootID <> 0 then
  try
    ReadObjectBase;
  except
    TGLog.LogError( 'TGSDBDataLink.Create: Failed to read object ' + IntToStr( rootID ));
  end;
end;

procedure TGSDBDataLink.Delete;
begin
  if FPrimaryKey <> 0 then
  begin
    if DataID <> 0 then
      DBLayer.DataTable.DeleteRow(DataID);

    if AttributeID <> 0 then
      DBLayer.AttributeTable.DeleteRow(AttributeID);

    if FDBObjectRec.UserID <> 0 then
      DBLayer.AttributeTable.DeleteRow(FDBObjectRec.UserID);

    DBLayer.SpatialIndex.Delete( MER, PrimaryKey );
    DBLayer.ObjectTable.DeleteRow( PrimaryKey );

    FPrimaryKey := 0;
    Modified := false;
  end;
end;

function TGSDBDataLink.GetCentroid: TGPointLL;
begin
  Result := FDBObjectRec.Centroid;
end;

function TGSDBDataLink.GetDataID: TGRowID;
begin
  Result := FDBObjectRec.DataID;
end;

function TGSDBDataLink.GetDateTime: TDateTime;
begin
  Result := FDBObjectRec.DateTime;
end;

function TGSDBDataLink.GetKind: TGDBObjectKind;
begin
  Result := FDBObjectRec.Kind;
end;

function TGSDBDataLink.GetMER: TGMER;
begin
  if FDBObjectRec.MER.iWidthX = 0 then
    FDBObjectRec.MER.iWidthX := 1;
  if FDBObjectRec.MER.iHeightY = 0 then
    FDBObjectRec.MER.iHeightY := 1;

  Result := FDBObjectRec.MER;
end;

function TGSDBDataLink.GetAttributeID: TGRowID;
begin
  Result := FDBObjectRec.AttributeID;
end;

function TGSDBDataLink.GetObjectDataCSL: String;
begin
  if AttributeID <> 0 then
    Result := DBLayer.AttributeTable.ReadString( AttributeID )
  else
    Result := '';
end;

function TGSDBDataLink.GetPrimaryKey: TGPrimaryKey;
begin
  Result := FPrimaryKey;
end;

function TGSDBDataLink.GetPresenterID: SmallInt;
begin
  Result := FDBObjectRec.PresenterID;
end;

function TGSDBDataLink.GetState: TGMapObjectStateSet;
begin
  Result := FDBObjectRec.State;
end;

function TGSDBDataLink.GetUserID: String;
begin
  if FDBObjectRec.UserID <> 0 then
    Result := DBLayer.AttributeTable.ReadString( FDBObjectRec.UserID )
  else
    Result := '';
end;

procedure TGSDBDataLink.ReadObjectBase;
begin
  DBLayer.ObjectTable.ReadBuffer(FPrimaryKey, @FDBObjectRec, SizeOf( TGDBObjectRec ), 0, SizeOf( TGDBObjectRec ));

  Modified := false;
end;

procedure TGSDBDataLink.SetDataID(const Value: TGRowID);
begin
  FDBObjectRec.DataID := Value;
  Modified := True;
end;

procedure TGSDBDataLink.SetDateTime(const Value: TDateTime);
begin
  FDBObjectRec.DateTime := Value;
  Modified := True;
end;

procedure TGSDBDataLink.SetKind(const Value: TGDBObjectKind);
begin
  FDBObjectRec.Kind := Value;
  Modified := True;
end;

procedure TGSDBDataLink.SetMER(const Value: TGMER);
begin
  FDBObjectRec.MER := Value;

(*  if Value.WidthX = 0 then
    FDBObjectRec.MER.WidthX := 1.0 / GU_DEGREE;
  if Value.HeightY = 0 then
    FDBObjectRec.MER.HeightY := 1.0 / GU_DEGREE;
*)

  Modified := True;
end;

procedure TGSDBDataLink.SetAttributeID(const Value: TGRowID);
begin
  FDBObjectRec.AttributeID := Value;
  Modified := True;
end;

procedure TGSDBDataLink.SetObjectDataCSL(const Value: String);
begin
  if AttributeID <> 0 then
    DBLayer.AttributeTable.DeleteRow(AttributeID);

  if Trim( Value ) <> '' then
    AttributeID := DBLayer.AttributeTable.AppendString( Trim( Value ))
  else
    AttributeID := 0;
end;

procedure TGSDBDataLink.SetPresenterID(const Value: SmallInt);
begin
  FDBObjectRec.PresenterID := Value;
  Modified := True;
end;

procedure TGSDBDataLink.SetState(const Value: TGMapObjectStateSet);
begin
  FDBObjectRec.State := Value;
  Modified := True;
end;

procedure TGSDBDataLink.WriteObjectBase;
var
  oldObjectRec : TGDBObjectRec;
begin
  if FPrimaryKey = 0 then
    FPrimaryKey := DBLayer.ObjectTable.AppendBuffer(@FDBObjectRec, SizeOf( TGDBObjectRec ))
  else
    if not DBLayer.BulkLoad then
    begin
      DBLayer.ObjectTable.ReadBuffer(FPrimaryKey, @oldObjectRec, SizeOf( TGDBObjectRec ), 0, SizeOf( TGDBObjectRec ));
      DBLayer.ObjectTable.UpdateBuffer(FPrimaryKey, @FDBObjectRec, SizeOf( TGDBObjectRec ));

      if oldObjectRec.MER.iWidthX = 0 then
        oldObjectRec.MER.iWidthX := 1;
      if oldObjectRec.MER.iHeightY = 0 then
        oldObjectRec.MER.iHeightY := 1;

      DBLayer.SpatialIndex.Delete(oldObjectRec.MER, FPrimaryKey);
    end;

//  if not DBLayer.BulkLoad then
    DBLayer.SpatialIndex.Insert(MER, FPrimaryKey);  // build the index while loading data
end;

procedure TGSDBDataLink.SaveToXML(el: TGXML_ELement);
var
  chainStore: TGChainStore;
  memStream : TMemoryStream;
begin
  el.AddAttribute( 'Centroid', PointLLToStr( Centroid ), '' );
  el.AddAttribute( 'Kind', Ord( Kind ), -1 );
  el.AddAttribute( 'State', Byte(State), 0 );
  el.AddAttribute( 'DateTime', DateTime, 0.0 );
  el.AddAttribute( 'PresenterID', PresenterID, 0 );
  el.AddAttribute( 'UserID', UserID, '' );
  el.AddAttribute( 'MER', MERToStr(MER), '' );

  if AttributeID <> 0 then
    el.AddElement('Data').BodyText := GetObjectDataCSL;

  if DataID <> 0 then
  begin
    chainStore := TGChainStore.Create;
    memStream := TMemoryStream.Create;

    ReadDataStream(MemStream);
    chainStore.ReadFromStream( MemStream );
    chainStore.SaveToXML(el.AddElement( 'Chains' ));

    memStream.Free;
    chainstore.Free;
  end;
end;

procedure TGSDBDataLink.SetCentroid(const Value: TGPointLL);
begin
  FDBObjectRec.Centroid := Value;
  Modified := True;
end;

procedure TGSDBDataLink.SetUserID(const Value: String);
begin
  if FDBObjectRec.UserID <> 0 then
    DBLayer.AttributeTable.DeleteRow(FDBObjectRec.UserID);

  if Trim( Value ) <> '' then
    FDBObjectRec.UserID := DBLayer.AttributeTable.AppendString( Trim( Value ))
  else
    FDBObjectRec.UserID := 0;
end;

procedure TGSDBDataLink.ReadDataStream(memStream: TMemoryStream);
begin
//  memStream.Clear;
  memStream.Position := 0;
  
  if DataID <> 0 then
    DBLayer.DataTable.ReadStream( DataID, memStream );
end;

procedure TGSDBDataLink.WriteDataStream(memStream: TMemoryStream);
begin
  if DataID <> 0 then
    DBLayer.DataTable.DeleteRow(DataID);

  DataID := DBLayer.DataTable.AppendStream( memStream );
end;

end.
