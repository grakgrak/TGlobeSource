{-------------------------------------------------------------------------
 Module:    Spatial Database Data Storage classes

 Comment:

 Classes:   TGDataPage
            TGSDBSysRoot
            TGSDBTable

 Author:    Graham Knight
 Email:     tglobe@tglobe.com
-------------------------------------------------------------------------}
{$I GLOBE5.INC}

unit GSDBTables;

interface

Uses  Classes, Windows, SysUtils, GSDBCache, GSysUtils, GXML;

const
  SDBTABLE_MAGIC = $54420000;
  SYSTABLE_ROOTID = $FFFFFFFF;
  
var
  ITEM_SIZE : integer = 0;

type
  TGDataPageRec = packed record
    NextID : TGPageID;
    PriorID : TGPageID;
    UsedPara : SmallInt;
    // this record uses the first paragraph of the page.
    // therefore there are 6 bytes free in this first paragraph
  end;
  PTGDataPageRec = ^TGDataPageRec;

  TGSDBTable = class;
  TGSDBSysRoot = class;

  TGRowStates = ( tgrsStart, tgrsEnd, tgrsChain, tgrsObject, tgrsCompressed );
  TGDataRowRec = packed record
    State : set of TGRowStates;
    ParaCount : Byte;   // number of paragraphs used by the row in the current page
    RowLength : integer;  // total length of the data row (this is omitted for chained rows)
  end;
  PTGDataRowRec = ^TGDataRowRec;

  TGDataPage = class( TGPage )
  private
    FDataTable: TGSDBTable;

    function GetNextID: TGPageID;
    function GetPriorID: TGPageID;
    function GetUsedPara: SmallInt;

    procedure SetNextID(const Value: TGPageID);
    procedure SetPriorID(const Value: TGPageID);
    procedure SetUsedPara(const Value: SmallInt);

    function DataItem( rowID : TGRowID ) : PTGDataRowRec;
    
    procedure DeleteNext;
    procedure ReadNext( data : TStream; start, bytes : integer );
    procedure UpdateNext( data : TStream; bytes : integer );
    procedure WriteNext( data : TStream );
  public
    constructor Create( dataTable : TGSDBTable; pageID : TGPageID ); reintroduce;

    procedure Delete( rowID : TGRowID );
    procedure Read( rowID : TGRowID; data : TStream; start, bytes : integer );
    procedure Update( rowID : TGRowID; data : TStream );
    function Write( data : TStream; asObject : Boolean = false ) : TGRowID;

    function RowLength( rowID : TGRowID ) : integer;
    function IsPageFull : boolean;

    property NextID : TGPageID read GetNextID write SetNextID;
    property PriorID : TGPageID read GetPriorID write SetPriorID;
    property UsedPara : SmallInt read GetUsedPara write SetUsedPara;
  end;

  TGSDBObject = class(TGRoot)
  private
    FSysRoot : TGSDBSysRoot;
    FName : String;
    FRootID : TGRowID;
    FModified: Boolean;
  protected
    procedure InternalLoad( aStream : TMemoryStream ); virtual; abstract;
    procedure InternalSave( aStream : TMemoryStream ); virtual; abstract;
  public
    constructor Create( SysRoot : TGSDBSysRoot; const name : String ); virtual;
    destructor Destroy; override;

    procedure Clear; virtual; abstract;
    procedure Flush; virtual;
    procedure SaveToXML( el : TGXML_Element ); virtual;

    property Name : String read FName write FName;
    property RootID : TGRowID read FRootID write FRootID;
    property SysRoot : TGSDBSysRoot read FSysRoot;
    property Modified: Boolean read FModified write FModified;
  end;
  TGSDBObjectClass = class of TGSDBObject;

  // The Generic data storage table header record
  TGSDBTableHeader = packed record
    Magic : DWORD;
    FirstPageID : TGPageID;
    LastPageID : TGPageID;
    PageCount : Cardinal;
    RowCount : Cardinal;
    DeletedCount : Cardinal;
    ObjectClass : string[32];
  end;

  TGSDBTable = class( TGSDBObject )
  private
    FHeader : TGSDBTableHeader;
    function NewPage : TGPageID;
    function GetReadOnly: Boolean;
    function GetObjectClass: String;
    procedure SetObjectClass(const Value: String);
  protected
    procedure InternalLoad( aStream : TMemoryStream ); override;
    procedure InternalSave( aStream : TMemoryStream ); override;
  public
    constructor Create( SysRoot : TGSDBSysRoot; const name, sdbClass : String ); reintroduce;

    procedure ReadBuffer( rowID : TGRowID; buffer : Pointer; bufLen, start, bytes : integer ); virtual;
    procedure ReadStream( rowID: TGRowID; aStream : TStream );
    function ReadString( rowID : TGRowID ) : String;

    function AppendBuffer( buffer : Pointer; bufLen : integer; asObject : Boolean = false ) : TGRowID;  virtual;
    function AppendStream( aStream : TStream; asObject : Boolean = false ) : TGRowID;
    function AppendString( const str : String ) : TGRowID;

    procedure UpdateBuffer( rowID : TGRowID; buffer : Pointer; bufLen : integer );  virtual;
    procedure UpdateStream( rowID: TGRowID; aStream : TStream );

    function RowLength( rowID : TGRowID ) : integer; virtual;
    procedure DeleteRow( rowID : TGRowID ); virtual;

    function First : TGRowID;
    function Next( rowID : TGRowID ) : TGRowID;

    procedure Clear; override;

    procedure SaveToXML( el : TGXML_Element ); override;

    property ObjectClass : String read GetObjectClass write SetObjectClass;
    property ReadOnly : Boolean read GetReadOnly;

    property Header : TGSDBTableHeader read FHeader;
  end;

  TGSDBObjectTable = class( TGSDBTable )
  private
    FObjectClass : string[32];
    function GetObjectClass: String;
    procedure SetObjectClass(const Value: String);
  protected
    procedure InternalLoad( aStream : TMemoryStream ); override;
    procedure InternalSave( aStream : TMemoryStream ); override;
  public
    constructor Create( SysRoot : TGSDBSysRoot; const name, sdbClass : String ); reintroduce;

    procedure SaveToXML( el : TGXML_Element ); override;

    property ObjectClass : String read GetObjectClass write SetObjectClass;
  end;

  
  TGSDBSysRoot = class( TGNoRefCountObject, IGPageCacheObserver )
  private
    FPageCache : TGPageCache;
    FRootTable : TGSDBTable;
    FRootTableCache : TStringList;

    procedure OnPageCacheNotification( Sender: TGPageCache; notification : TGPageCacheNotification );
  protected
    function GetRootTable : TGSDBTable;
  public
    constructor Create( pageCache: TGPageCache );
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    procedure Flush;
    procedure Clear;
    procedure SaveToXML( el : TGXML_Element );

    function CreateObject( sdbClass : TGSDBObjectClass; rowID : TGRowID ) : TGSDBObject;
    function CreateObjectByName( sdbClass : TGSDBObjectClass; const name : String ) : TGSDBObject;
    function ObjectTable( const name : String ) : TGSDBTable;
    procedure DeleteObject( sdbObject : TGSDBObject );
    procedure SaveObject( sdbObject : TGSDBObject );

    property PageCache : TGPageCache read FPageCache;
    property RootTable : TGSDBTable read GetRootTable;
  end;

procedure RegisterSDBClass(sdbClass : TGSDBObjectClass; const alias : string );
function FindSDBClass(const sdbClassName : String ) : TGSDBObjectClass;
procedure InitialiseDBTables( pageSize : integer );

implementation

uses GSDBRtree, GSDBBTree, GSDBLayers, GLogging, GSDBKeyindex;

var
  SDBClassList : TStringList;

//-----------------------------------------------------------------------------
//Description
//Called when the spatial database is first opened, it gives the DBTables class
//an oportunity to set it's data structures to the same size as used by the
//spatial database.                                                            
//-----------------------------------------------------------------------------
procedure InitialiseDBTables( pageSize : integer );
begin
  ITEM_SIZE := pageSize div 256;
end;

function FindSDBClass(const sdbClassName : String ) : TGSDBObjectClass;
var
  idx : integer;
begin
  if SDBClassList = nil then
    SDBClassList := TStringList.Create;

  idx := SDBClassList.IndexOf( sdbClassName );

  if idx >= 0 then
    Result := TGSDBObjectClass( SDBClassList.Objects[idx] )
  else
    Result := nil;
end;

procedure RegisterSDBClass(sdbClass : TGSDBObjectClass; const alias : string );
var
  className : string;
begin
  if SDBClassList = nil then
    SDBClassList := TStringList.Create;

  if alias = '' then
    className := sdbClass.ClassName
  else
    className := alias;
    
  if SDBClassList.IndexOf( className ) < 0 then
    SDBClassList.AddObject( className, TObject( sdbClass ));
end;

constructor TGSDBObject.Create(SysRoot: TGSDBSysRoot; const name : String);
begin
  Assert( SysRoot <> nil, 'TGSDBObject passed nil SysRoot' );

  FSysRoot := SysRoot;
  FRootID := 0;
  FName := name;
  FModified := false;
end;

destructor TGSDBObject.Destroy;
begin
  Flush;
  inherited;
end;

procedure TGSDBObject.Flush;
begin
  if Modified then
  begin
    SysRoot.SaveObject( Self );
    Modified := false;
  end;
end;

procedure TGSDBObject.SaveToXML(el: TGXML_Element);
begin
  el.AddAttribute( 'Name', Name, '' );
end;



function TGSDBTable.AppendStream(aStream: TStream; asObject : Boolean): TGRowID;
var
  page : TGDataPage;
begin
  page := TGDataPage.Create( self, FHeader.LastPageID );
  if page.IsPageFull then
  begin
    page.Free;
    page := TGDataPage.Create( self, NewPage );
  end;

  Result := page.Write( aStream, asObject );

  page.Free;

  Inc( FHeader.RowCount );
  Modified := true;
end;

function TGSDBTable.AppendBuffer( buffer : Pointer; bufLen: integer; asObject : Boolean ): TGRowID;
var
  bufStream : TGBufferStream;
begin
  bufStream := TGBufferStream.Create( buffer, bufLen );

  Result := AppendStream( bufStream, asObject );

  bufStream.Free;
end;

function TGSDBTable.AppendString(const str: String): TGRowID;
var
  ss : TStringStream;
begin
  ss := TStringStream.Create(str);
  Result := AppendStream( ss );
  ss.Free;
end;

procedure TGSDBTable.Clear;
var
  page : TGDataPage;
  rowID, nextID : TGPageID;
begin
  rowID := FHeader.FirstPageID;

  while rowID <> 0 do
  begin
    page := TGDataPage.Create( self, rowID );
    nextID := page.GetNextID;
    page.Free;
    FSysRoot.PageCache.DeletePage( rowID );
    rowID := nextID;
  end;
  FHeader.FirstPageID := 0;
  FHeader.LastPageID := 0;
  FHeader.RowCount := 0;
  FHeader.DeletedCount := 0;
  Modified := true;
  Flush;
end;

constructor TGSDBTable.Create(SysRoot: TGSDBSysRoot; const name, sdbClass: String);
begin
  inherited Create( SysRoot, name );

  ObjectClass := sdbClass;
end;

procedure TGSDBTable.DeleteRow(rowID: TGRowID);
var
  page : TGDataPage;
begin
  //ToDo: Delete should release empty pages to the cache

  page := TGDataPage.Create( self, rowID );

  page.Delete( rowID );

  Dec( FHeader.RowCount );
  Inc( FHeader.DeletedCount );

  page.Free;
  
  Modified := true;
end;

function TGSDBTable.First: TGRowID;
begin
  if FHeader.RowCount > 0 then
    Result := Next( FHeader.FirstPageID )
  else
    Result := 0;
end;

function TGSDBTable.GetObjectClass: String;
begin
  Result := FHeader.ObjectClass;
end;

function TGSDBTable.GetReadOnly: Boolean;
begin
  Result := SysRoot.PageCache.ReadOnly;
end;

procedure TGSDBTable.InternalLoad(aStream: TMemoryStream);
begin
  aStream.Read( FHeader, SizeOf( TGSDBTableHeader ));
  Assert( FHeader.Magic = SDBTABLE_MAGIC, 'SDBTable Magic Failed' );
end;

procedure TGSDBTable.InternalSave(aStream: TMemoryStream);
begin
  if FHeader.FirstPageID = 0 then
  begin
    // Create the first page for the table
    FHeader.FirstPageID := NewPage;
    FHeader.LastPageID := FHeader.FirstPageID;
    Modified := true;
  end;

  FHeader.Magic := SDBTABLE_MAGIC;
  aStream.Write( FHeader, SizeOf( TGSDBTableHeader ));
end;

function TGSDBTable.NewPage : TGPageID;
var
  page : TGDataPage;
begin
  Result := FSysRoot.PageCache.NewPage;

  Inc( FHeader.PageCount );

  if FHeader.LastPageID <> 0 then
  begin
    page := TGDataPage.Create( self, FHeader.LastPageID );
    page.NextID := Result;  // Chain the table pages together
    page.Free;
  end;

  page := TGDataPage.Create( self, Result );
  page.PriorID := FHeader.LastPageID;  // Chain the table pages together
  page.Free;

  FHeader.LastPageID := Result;
  Modified := True;
end;

function TGSDBTable.Next(rowID: TGRowID): TGRowID;
var
  page : TGDataPage;
  idx : integer;
  itemPtr : PTGDataRowRec;
begin
  Result := 0;
  repeat
    page := TGDataPage.Create( self, RowPageID( rowID ));

    idx := RowItemID(rowID);
    if idx = 0 then // before the start of the page - so get the first item
    begin
      rowID := MakeRowID( page.PageID, 1 );
      itemPtr := page.DataItem( rowID );
    end
    else
    begin
      itemPtr := page.DataItem( rowID );
      idx := idx + itemPtr.ParaCount; // move to the start of the next item

      // if at the end of the page
      if idx >= page.UsedPara then
      begin
        rowID := MakeRowID( page.NextID, 0 ); // step to before the start of the next page
        itemPtr := nil;
      end
      else
      begin
        rowID := MakeRowID( page.PageID, idx );
        itemPtr := page.DataItem( rowID ); // pointer to the start of the nxt item
      end;
    end;

    // if at the start of a record
    if ( itemPtr <> nil ) and ( tgrsStart in itemPtr.State ) then
      Result := rowID;

    page.Free;
  until ( rowID = 0 ) or ( Result <> 0 );
end;

procedure TGSDBTable.ReadStream(rowID: TGRowID; aStream: TStream);
var
  page : TGDataPage;
begin
  page := TGDataPage.Create( self, RowPageID( rowID ));
  page.Read( rowID, aStream, 0, 0 );
  aStream.Position := 0;
  page.Free;
end;

procedure TGSDBTable.ReadBuffer(rowID: TGRowID; buffer : Pointer; bufLen, start, bytes : integer);
var
  page : TGDataPage;
  bufStream : TGBufferStream;
begin
  page := TGDataPage.Create( self, RowPageID( rowID ));

  bufStream := TGBufferStream.Create(buffer, bufLen);
  page.Read( rowID, bufStream, 0, bytes );
  bufStream.Free;

  page.Free;
end;

function TGSDBTable.ReadString(rowID: TGRowID): String;
var
  page : TGDataPage;
  ss : TStringStream;
begin
  page := TGDataPage.Create( self, RowPageID( rowID ));

  ss := TStringStream.Create('');
  page.Read( rowID, ss, 0, 0 );

  Result := ss.DataString;

  ss.Free;
  page.Free;
end;

function TGSDBTable.RowLength(rowID: TGRowID): integer;
var
  page : TGDataPage;
begin
  page := TGDataPage.Create( self, RowPageID( rowID ));

  Result := page.RowLength( rowID );

  page.Free;
end;

procedure TGSDBTable.SaveToXML(el: TGXML_Element);
var
  rowID : TGRowID;
  memStream : TMemoryStream;
  sdbObject : TGSDBObject;
  sdbClass : TGSDBObjectClass;
begin
  inherited;

  el.AddAttribute('Ver', FHeader.Magic, -1 );
  el.AddAttribute('Class', ObjectClass, '' );
  el.AddAttribute('PageCount', Header.PageCount, -1 );
  el.AddAttribute('Rows', Header.RowCount, -1 );
  el.AddAttribute('Deleted', Header.DeletedCount, -1 );

  sdbClass := FindSDBClass( ObjectClass );
  memStream := TMemoryStream.Create;
  rowID := First;
  while rowID <> 0 do
  begin
    memStream.Position := 0;
    ReadStream( rowID, memStream );
    if sdbClass <> nil then
    begin
      sdbObject := SysRoot.CreateObject( sdbClass, rowID );
      sdbObject.SaveToXML( el.AddElement( Name ));
      sdbObject.Free;
    end
    else
    begin
//      rowEl := tableEl.AddBinaryElement('Row', memStream.Pointer, memStream.Size);
//      rowEl.AddAttribute('ID', RowIDAsString( rowID ));
    end;

    rowID := Next( rowID );
  end;
  memStream.Free;
end;

procedure TGSDBTable.SetObjectClass(const Value: String);
begin
  FHeader.ObjectClass := Value;
end;

procedure TGSDBTable.UpdateStream(rowID: TGRowID; aStream: TStream);
var
  page : TGDataPage;
begin
  page := TGDataPage.Create( self, RowPageID( rowID ));
  page.Update( rowID, aStream );
  page.Free;
end;

procedure TGSDBTable.UpdateBuffer(rowID: TGRowID; buffer: Pointer; bufLen : integer);
var
  bufStream : TGBufferStream;
begin
  bufStream := TGBufferStream.Create( buffer, bufLen );
  UpdateStream( rowID, bufStream );
  bufStream.Free;
end;

{ TGDataPage }

constructor TGDataPage.Create(dataTable: TGSDBTable; pageID: TGPageID);
begin
  inherited Create( dataTable.SysRoot.PageCache, pageID );

  FDataTable := dataTable;

  if UsedPara = 0 then  // ensure that the page is properly initialised
    UsedPara := 1;
end;

function TGDataPage.DataItem(rowID: TGRowID): PTGDataRowRec;
begin
  Result := PTGDataRowRec( DataPtr + RowItemID(rowID) * ITEM_SIZE );
end;

procedure TGDataPage.Delete(rowID: TGRowID);
var
  itemLen : integer;
  itemPtr : PTGDataRowRec;
begin
  itemPtr := DataItem(rowID);

  itemPtr.State := [];  // indicate item has been deleted
  Modified := true;

  // get the amount of data space in the item
  itemLen := itemPtr.ParaCount * ITEM_SIZE - SizeOf( TGDataRowRec );
  if itemPtr.RowLength > itemLen then
    DeleteNext;
end;

procedure TGDataPage.DeleteNext;
var
  rowID : TGRowID;
  page : TGDataPage;
  itemPtr : PTGDataRowRec;
begin
  rowID := MakeRowID(NextID, 1 );

  while rowID <> 0 do
  begin
    page := TGDataPage.Create( FDataTable, rowID );
    itemPtr := page.DataItem(rowID);

    if tgrsEnd in itemPtr.State then  // Check for end of data row
      rowID := 0
    else
      rowID := MakeRowID( page.NextID, 1 );  // get the rowID for next part of the data

    itemPtr.State := []; // flag as deleted
    page.Modified := true;
    page.Free;
  end;
end;

function TGDataPage.GetNextID: TGPageID;
begin
  Result := PTGDataPageRec(DataPtr).NextID;
end;

function TGDataPage.GetPriorID: TGPageID;
begin
  Result := PTGDataPageRec(DataPtr).PriorID;
end;

function TGDataPage.GetUsedPara: SmallInt;
begin
  Result := PTGDataPageRec(DataPtr).UsedPara;
end;

function TGDataPage.IsPageFull: boolean;
begin
  Result := ( UsedPara = 256 );
end;

procedure TGDataPage.Read(rowID: TGRowID; data : TStream; start, bytes : integer );
var
  itemLen : integer;
  itemPtr : PTGDataRowRec;
  srcPtr : PChar;
begin
  itemPtr := DataItem(rowID);

  Assert( itemPtr.ParaCount > 0, 'Invalid Item at Page:' + RowIDToStr( rowID ));

  // get total data length
  if bytes = 0 then
    bytes := itemPtr.RowLength;

  data.Size := bytes;
  data.Position := 0;

  itemLen := MinVal( bytes, itemPtr.ParaCount * ITEM_SIZE - SizeOf( TGDataRowRec ));

  // Move source pointer to start of data area
  if start > itemLen then
    ReadNext( data, start - itemLen, bytes )
  else
  begin
    srcPtr := PChar( itemPtr ) + SizeOf( TGDataRowRec ) + start;

    data.Write( srcPtr^, MinVal( bytes, itemLen - start ));

    Dec( bytes, itemLen - start );

    ReadNext( data, 0, bytes );
  end;
end;

procedure TGDataPage.ReadNext( data : TStream; start, bytes : integer );
var
  page : TGDataPage;
  itemLen : integer;
  srcPtr : PChar;
  rowID : TGRowID;
begin
  rowID := MakeRowID( NextID, 1 );

  while ( bytes > 0 ) and ( rowID <> 0 ) do
  begin
    page := TGDataPage.Create( FDataTable, rowID );

    // limit itemLen to the maximum amount of space in a data block or the data length
    itemLen := MinVal( 255 * ITEM_SIZE, bytes + 2 );

    // Get a pointer to the start of the data
    if start < itemLen then
    begin
      srcPtr := PChar( page.DataItem(rowID));
      Inc( srcPtr, 2 + start ); // skip over the item header
      Dec( itemLen, 2 );

      data.Write(srcPtr^, MinVal( bytes, itemLen - start));

      Dec( bytes, itemLen - start );
      start := 0;
    end
    else
      Dec( start, itemLen );

    // if there is more to read
    if bytes > 0 then
      rowID := MakeRowID( page.NextID, 1 );  // get the rowID for next part of the data

    page.Free;
  end;
end;

function TGDataPage.RowLength(rowID: TGRowID): integer;
begin
  Result := DataItem(rowID).RowLength;
end;

procedure TGDataPage.SetNextID(const Value: TGPageID);
begin
  PTGDataPageRec(DataPtr).NextID := Value;
  Modified := true;
end;

procedure TGDataPage.SetPriorID(const Value: TGPageID);
begin
  PTGDataPageRec(DataPtr).PriorID := Value;
  Modified := true;
end;

procedure TGDataPage.SetUsedPara(const Value: SmallInt);
begin
  PTGDataPageRec(DataPtr).UsedPara := Value;
  Modified := true;
end;

procedure TGDataPage.Update(rowID: TGRowID; data : TStream );
var
  itemLen, dataLen : integer;
  itemPtr : PTGDataRowRec;
  destPtr : PChar;
begin
  itemPtr := DataItem(rowID);

  dataLen := itemPtr.RowLength; // get total data length

  Assert( dataLen = data.Size, 'Update: Length has changed cannot update.' );

  data.Position := 0;

  itemLen := itemPtr.ParaCount * ITEM_SIZE - SizeOf( TGDataRowRec );

  // Move source pointer to start of data area
  destPtr := PChar( itemPtr ) + SizeOf( TGDataRowRec );
  data.Read( destPtr^, itemLen);
  Modified := true;

  if dataLen > itemLen then
    UpdateNext( data, dataLen - itemLen );
end;

procedure TGDataPage.UpdateNext( data : TStream; bytes : integer );
var
  page : TGDataPage;
  itemLen : integer;
  destPtr : PChar;
  rowID : TGRowID;
begin
  rowID := MakeRowID( NextID, 1 );
  while rowID <> 0 do
  begin
    page := TGDataPage.Create( FDataTable, rowID );

    // Get a pointer to the start of the data
    destPtr := PChar( page.DataItem(rowID));

    // limit itemLen to the maximum amount of space in a data block or the data length
    itemLen := MinVal( 255 * ITEM_SIZE, bytes + 2);
    Inc( destPtr, 2 );  // skip over the item header
    Dec( itemLen, 2 );

    data.Read( destPtr^, itemLen );

    page.Modified := true;

    // if there is more to write
    if bytes > itemLen then
    begin
      Dec( bytes, ItemLen );
      rowID := MakeRowID( page.NextID, 1 );  // get the rowID for next part of the data
    end
    else
      rowID := 0; // no more to write

    page.Free;
  end;
end;

function TGDataPage.Write(data : TStream; asObject : Boolean): TGRowID;
var
  itemPtr : PTGDataRowRec;
  destPtr : PChar;
  paras, count : integer;
begin
  if not (UsedPara < 256 ) then
    Assert( UsedPara < 256, 'Write: Page Full' );

  Result := MakeRowID( PageID, UsedPara );

  data.Position := 0;

  // calculate the number of paras that need to be written to this page
  paras := MinVal( 256 - UsedPara, (data.Size + SizeOf( TGDataRowRec )) div ITEM_SIZE + 1);
  count := MinVal( data.Size, paras * ITEM_SIZE - SizeOf( TGDataRowRec ));

  Assert( paras > 0, 'Invalid Write Item at Page:' + RowIDToStr( PageID ));

  itemPtr := DataItem(Result);
  Include( itemPtr.State, tgrsStart ); // Flag the start of the data
  if asObject then
    Include( itemPtr.State, tgrsObject ); // Flag the row holds an object

  itemPtr.RowLength := data.Size;
  itemPtr.ParaCount := paras;

  destPtr := PChar(itemPtr) + SizeOf( TGDataRowRec );
  data.Read(destPtr^, count);

  UsedPara := UsedPara + paras;
  Modified := true;

  if count < data.Size then
    WriteNext( data )
  else
    Include( itemPtr.State, tgrsEnd ); // Flag the End of the data
end;

procedure TGDataPage.WriteNext( data : TStream);
var
  page : TGDataPage;
  destPtr : PChar;
  paras, count, dataLen : integer;
begin
  dataLen := data.Size - data.Position;
  repeat
    // append a new page to the store
    page := TGDataPage.Create( FDataTable, FDataTable.NewPage );

    Inc( dataLen, 2 );
    paras := MinVal( 256 - page.UsedPara, dataLen div ITEM_SIZE + 1);
    count := MinVal( dataLen, paras * ITEM_SIZE);

    destPtr := PChar( page.DataPtr + ITEM_SIZE );

    // if we are at the end of the data
    if count = dataLen then
      Include( PTGDataRowRec( destPtr ).State, tgrsEnd )
    else
      Include( PTGDataRowRec( destPtr ).State, tgrsChain );

    PTGDataRowRec( destPtr ).ParaCount := paras;

    Inc( destPtr, 2 );
    Dec( count, 2 );
    Dec( dataLen, 2 );

    data.Read(destPtr^, count);

    page.UsedPara := page.UsedPara + paras;
    page.Modified := true;
    page.Free;

    // adjust the data length
    Dec( dataLen, count );
  until dataLen <= 0;
end;



procedure TGSDBSysRoot.BeforeDestruction;
begin
  FPageCache.Unsubscribe(self);

  inherited;
end;

procedure TGSDBSysRoot.Clear;
var
  idx : integer;
begin
  for idx := 0 to FRootTableCache.Count - 1 do
    TGSDBTable( FRootTableCache.Objects[idx] ).Free;

  FRootTableCache.Clear;
//  FreeAndNil( FRootTable );
end;

constructor TGSDBSysRoot.Create(pageCache: TGPageCache);
begin
  Assert( pageCache <> nil, 'TGSDBSysRoot: Passed nil PageCache' );
  Assert( TGLog.LogTrace( gllTwo, 'Create SysRoot' ));

  FPageCache := pageCache;
  pageCache.Subscribe(self);
  FRootTableCache := TStringList.Create;
end;

function TGSDBSysRoot.CreateObject(sdbClass: TGSDBObjectClass; rowID: TGRowID): TGSDBObject;
var
  memStream : TMemoryStream;
  objTable : TGSDBTable;
begin
  objTable := ObjectTable( sdbClass.ClassName );

  memStream := TMemoryStream.Create;
  try
    objTable.ReadStream( rowID, memStream);

    Result := sdbClass.Create( Self, PChar( memStream.Memory ));
    Result.RootID := rowID;

    // step over the object name
    memStream.Position := Length( PChar( memStream.Memory )) + 1;
    Result.InternalLoad( memStream );
  finally
    memStream.Free;
  end;
end;

function TGSDBSysRoot.CreateObjectByName(sdbClass: TGSDBObjectClass; const name: String): TGSDBObject;
var
  memStream : TMemoryStream;
  rowID : TGRowID;
  objTable : TGSDBTable;
begin
  objTable := ObjectTable( sdbClass.ClassName );

  memStream := TMemoryStream.Create;
  try
    rowID := objTable.First;
    while rowID <> 0 do
    begin
      memStream.Position := 0;
      objTable.ReadStream( rowID, memStream);

      // look for the requested object
      if StrIComp( PChar( memStream.Memory ), PChar( name )) = 0 then
      begin
        Result := sdbClass.Create( Self, PChar( memStream.Memory ));
        Result.RootID := rowID;

        // step over the object name
        memStream.Position := Length( PChar( memStream.Memory )) + 1;
        Result.InternalLoad( memStream );
        Exit;
      end;
      rowID := objTable.Next( rowID );
    end;
  finally
    memStream.Free;
  end;
  Result := nil;
end;

procedure TGSDBSysRoot.DeleteObject(sdbObject: TGSDBObject);
begin
  if PageCache.ReadOnly or ( sdbObject = nil ) then
    Exit;

  sdbObject.Clear;

  if sdbObject.RootID <> 0 then
    ObjectTable( sdbObject.ClassName ).DeleteRow( sdbObject.RootID );
end;

destructor TGSDBSysRoot.Destroy;
var
  idx : integer;
begin
  Flush;

  for idx := 0 to FRootTableCache.Count - 1 do
    TGSDBTable( FRootTableCache.Objects[idx] ).Free;
  FreeAndNil( FRootTableCache );

  FreeAndNil( FRootTable );
  Assert( TGLog.LogTrace( gllTwo, 'Destroy SysRoot' ));
  inherited;
end;

procedure TGSDBSysRoot.Flush;
var
  idx : integer;
begin
  for idx := 0 to FRootTableCache.Count - 1 do
    TGSDBTable( FRootTableCache.Objects[idx] ).Flush;
  Clear;

  if FRootTable <> nil then
    RootTable.Flush;

  FPageCache.Flush;
end;

function TGSDBSysRoot.GetRootTable: TGSDBTable;
begin
  if FRootTable = nil then
  begin
    FRootTable := TGSDBTable.Create(self, 'Root', 'TGSDBTable');
    FRootTable.RootID := SYSTABLE_ROOTID;
    FRootTable.FHeader.FirstPageID := PageCache.SysFirstPageID;
    FRootTable.FHeader.LastPageID := PageCache.SysLastPageID;
    FRootTable.FHeader.RowCount := PageCache.SysRowCount;
  end;
  Result := FRootTable;
end;

function TGSDBSysRoot.ObjectTable( const name : String ): TGSDBTable;
var
  memStream : TMemoryStream;
  rowID : TGRowID;
  idx : integer;
begin
  idx := FRootTableCache.IndexOf( name );

  if idx >= 0 then
    Result := TGSDBTable( FRootTableCache.Objects[idx] )
  else
  begin
    memStream := TMemoryStream.Create;
    try
      rowID := RootTable.First;
      while rowID <> 0 do
      begin
        memStream.Position := 0;
        RootTable.ReadStream( rowID, memStream);

        // look for the requested table
        if StrIComp( PChar( memStream.Memory ), PChar( name )) = 0 then
        begin
          Result := TGSDBTable.Create( Self, name, name );
          Result.RootID := rowID;

          FRootTableCache.AddObject( name, Result );  // Add to the table list

          // step over the object name
          memStream.Position := Length( PChar( memStream.Memory )) + 1;
          Result.InternalLoad( memStream );
          Exit;
        end;
        rowID := RootTable.Next( rowID );
      end;

      // Create a new instance of the object
      Result := TGSDBTable.Create( Self, name, name );
      FRootTableCache.AddObject( name, Result );  // Add to the table list

      memStream.Clear;
      memStream.Write( PChar( name )^, Length( name ) + 1 );
      Result.InternalSave(memStream);

      Result.RootID := RootTable.AppendStream( memStream, True ); // Save stream to master table
    finally
      memStream.Free;
    end;
  end;
end;

procedure TGSDBSysRoot.OnPageCacheNotification(Sender: TGPageCache; notification : TGPageCacheNotification );
var
  idx : integer;
begin
  case notification of
  tgpcFlush  :
    Flush;
  tgpcCreated :
    begin
      // Make sure all the registered classes have their object tables
      for idx := 0 to SDBClassList.Count - 1 do
        ObjectTable( SDBClassList[idx] );
    end;
  tgpcAfterOpen :
    begin
      // Ensure that the internal tables are cleared
//      FreeAndNil( FRootTable );
    end;
  tgpcBeforeClose :
    begin
      // Ensure that the internal tables are cleared
      Flush;
//      FreeAndNil( FRootTable );
    end;
  end;
end;

procedure TGSDBSysRoot.SaveObject(sdbObject: TGSDBObject);
var
  memStream : TMemoryStream;
  objTable : TGSDBTable;
begin
  if PageCache.ReadOnly or ( sdbObject = nil ) then
    Exit;
  Assert( sdbObject.Name <> '', 'SaveObject: SDBObject name not set' );
  Assert( TGLog.LogTrace( gllthree, 'SaveObject: ' + sdbObject.Name + ' RowID: ' + RowIDToStr( sdbObject.RootID )));

  memStream := TMemoryStream.Create;

  memStream.Write( PChar(sdbObject.Name)^, Length( sdbObject.Name ) + 1 );
  sdbObject.InternalSave(memStream);

  objTable := ObjectTable( sdbObject.ClassName );

  if sdbObject.RootID = 0 then
    sdbObject.RootID := objTable.AppendStream( memStream, True ) // Save stream to master table
  else
  begin
    if sdbObject.RootID = SYSTABLE_ROOTID then  // this is the system table itself
    begin
      PageCache.SysFirstPageID := RootTable.FHeader.FirstPageID;
      PageCache.SysLastPageID := RootTable.FHeader.LastPageID;
      PageCache.SysRowCount := RootTable.FHeader.RowCount;
    end
    else
    begin
      // Update an existing object
      if objTable.RowLength(sdbObject.RootID) = memStream.Size then
        objTable.UpdateStream( sdbObject.RootID, memStream ) // Save stream to master table
      else
      begin
        objTable.DeleteRow(sdbObject.RootID);
        sdbObject.RootID := objTable.AppendStream( memStream, True ) // Save stream to master table
      end;
    end;
  end;
  memStream.Free;
end;

procedure TGSDBSysRoot.SaveToXML(el: TGXML_Element);
begin
  PageCache.SaveToXML( el.AddElement( 'PageCache' ));
  RootTable.SaveToXML( el.AddElement( 'SDBRoot' ));
end;



constructor TGSDBObjectTable.Create(SysRoot: TGSDBSysRoot; const name, sdbClass: String);
begin

end;

function TGSDBObjectTable.GetObjectClass: String;
begin
  Result := FObjectClass;
end;

procedure TGSDBObjectTable.InternalLoad(aStream: TMemoryStream);
begin
  inherited;
  aStream.Read( FObjectClass, SizeOf( FObjectClass ));
end;

procedure TGSDBObjectTable.InternalSave(aStream: TMemoryStream);
begin
  inherited;
  aStream.Write( FObjectClass, SizeOf( FObjectClass ));
end;

procedure TGSDBObjectTable.SaveToXML(el: TGXML_Element);
var
  rowID : TGRowID;
  sdbObject : TGSDBObject;
  sdbClass : TGSDBObjectClass;
begin
  inherited;

  el.AddAttribute('Ver', FHeader.Magic, -1 );
  el.AddAttribute('Class', ObjectClass, '' );
  el.AddAttribute('PageCount', Header.PageCount, -1 );
  el.AddAttribute('Rows', Header.RowCount, -1 );
  el.AddAttribute('Deleted', Header.DeletedCount, -1 );

  sdbClass := FindSDBClass( ObjectClass );
  if sdbClass = nil then
    raise EGException.Create( 'Unknown Class: ' + ObjectClass );

  rowID := First;
  while rowID <> 0 do
  begin
    sdbObject := SysRoot.CreateObject( sdbClass, rowID );
    sdbObject.SaveToXML( el.AddElement( Name ));
    sdbObject.Free;

    rowID := Next( rowID );
  end;
end;

procedure TGSDBObjectTable.SetObjectClass(const Value: String);
begin
  if FObjectClass <> Value then
  begin
    FObjectClass := Value;
    Modified := True;
  end;
end;

initialization
  RegisterSDBClass( TGSDBTable, '' );
finalization
  FreeAndNil( SDBClassList );
end.
