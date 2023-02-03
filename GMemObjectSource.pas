//---------------------------------------------------------------------------
//Summary
//TGlobe Memory object source class.
//
//Description
//Provides an object source for tyemporary layers that are held in memory.
//
//Author
//Graham Knight (tglobe@tglobe.com)
//---------------------------------------------------------------------------
unit GMemObjectSource;

interface

Uses
  Classes, SysUtils, GClasses, GSysUtils, GBitmap, GDataReaders, GXML;

type

  TGMemoryObjectSource = class(TGObjectSource)
  private
    FObjectList : TInterfaceList;
    FNullObjects : integer;
    FDataReader: TGDataReader;
    procedure SetDataReader(const Value: TGDataReader);
  protected
    function GetObjectCount : Cardinal; override;
  public
    constructor Create( parentGlobe: TGCustomGlobe5 ); override;
    destructor Destroy; override;

    function First : TGPrimaryKey; override;
    function Next( objPK : TGPrimaryKey ) : TGPrimaryKey; override;
    function ByPrimaryKey( objPK : TGPrimaryKey ): IGMapPoint; override;

    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;

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
    function IsTemporary : Boolean; override;

    property DataReader : TGDataReader read FDataReader write SetDataReader;
  end;

implementation

Uses Globe5;


procedure TGMemoryObjectSource.Add(obj: IGMapPoint);
var
  idx : integer;
begin
  if obj = nil then
    Exit;
    
  inherited;

  if FNullObjects = 0 then
    obj.PrimaryKey := FObjectList.Add( obj ) + 1  // One counting
  else
    for idx := 0 to FObjectList.Count - 1 do
      if FObjectList[idx] = nil then
      begin
        FObjectList[idx] := obj;
        obj.PrimaryKey := idx + 1;  // one counting
        Dec( FNullObjects );
        Break;
      end;
end;

function TGMemoryObjectSource.ByPrimaryKey( objPK: TGPrimaryKey ): IGMapPoint;
begin
  // objPK's are one counting
  if ( objPK > 0 ) and ( objPK <= TGPrimaryKey(FObjectList.Count)) then
  begin
    Result := FObjectList[objPK - 1] as IGMapPoint;
    if Result <> nil then
    begin
      Result.ObjectSource := Self;
      Result.PrimaryKey := objPK;
    end;
  end
  else
    Result := nil;
end;

procedure TGMemoryObjectSource.Clear;
begin
  inherited;
  FObjectList.Clear;
  FNullObjects := 0;
end;

constructor TGMemoryObjectSource.Create( parentGlobe: TGCustomGlobe5 );
begin
  inherited;

  FObjectList := TInterfaceList.Create;

  CreateGUID( FUniqueID );
end;

procedure TGMemoryObjectSource.Delete;
begin
  Clear;
end;

destructor TGMemoryObjectSource.Destroy;
begin
  Clear;
  FreeAndNil( FObjectList );
  FreeAndNil( FDataReader );
  inherited;
end;

procedure TGMemoryObjectSource.ExportImageStream(const ImageName: String; imageStream: TStream);
var
  fileStream : TFileStream;
  fileSize : int64;
begin
  if FileExists(imageName) then
  begin
    fileStream := TFileStream.Create(imageName, fmOpenRead);
    fileSize := fileStream.Size;

    imageStream.Write( fileSize, SizeOf( Cardinal ));
    imageStream.CopyFrom(fileStream, 0); // copies the entire contents of fileStream over

    fileStream.Free;
  end;
end;

function TGMemoryObjectSource.First: TGPrimaryKey;
var
  idx : integer;
begin
  for idx := 0 to FObjectList.Count - 1 do
    if FObjectList[idx] <> nil then
    begin
      Result := idx + 1;
      Exit;
    end;
  Result := 0;
end;

procedure TGMemoryObjectSource.ForEach(const aMER: TGMER; callback: TGObjectCallback);
var
  objIterator : TGIterator;
  obj : IGMapPoint;
  abort : Boolean;
begin
  objIterator := NewIterator( aMER );
  try
    abort := false;
    obj := objIterator.First;
    while ( obj <> nil ) and ( not abort ) do
    begin
      callback( obj.PrimaryKey, abort );
      obj := objIterator.Next;
    end;
  finally
    objIterator.Free;
  end;
end;

function TGMemoryObjectSource.GetObjectCount: Cardinal;
begin
  Result := FObjectList.Count - FNullObjects;
end;

function TGMemoryObjectSource.ImageByName( const Image: TGBitmap; var ImageName: TFilename) : Boolean;
begin
  Result := TGlobe5(ParentGlobe).LoadImage(Image, ImageName );
end;

function TGMemoryObjectSource.ImportImage( const ImageName : String ) : String;
begin
  Result := ImageName;
end;

procedure TGMemoryObjectSource.ImportImageStream(
  const ImageName: String; imageStream: TMemoryStream);
begin
  //ToDO:
end;

function TGMemoryObjectSource.IsTemporary: Boolean;
begin
  Result := true;
end;

procedure TGMemoryObjectSource.LoadFromXML(Element: TGXML_Element);
begin
  inherited;
  if Element <> nil then
    if Element.ElementExists( 'DataReader' ) then
      DataReader := TGDataReader( CreateFromXML( ParentGlobe, Element.Element( 'DataReader' ), nil));
end;

function TGMemoryObjectSource.NewIterator(aMER: TGMER): TGIterator;
var
  idx : integer;
  obj : IGMapPoint;
begin
  Result := TGListIterator.Create( Self );

  for idx := 0 to FObjectList.Count - 1 do
  begin
    obj := ByPrimaryKey( idx + 1 );

    if ( obj <> nil ) then
//    if ( obj <> nil ) and MER_Intersect( obj.ObjectMER, aMER ) then
        TGListIterator(Result).AddPrimaryKey( idx + 1 );
  end;
end;

function TGMemoryObjectSource.Next(objPK: TGPrimaryKey): TGPrimaryKey;
var
  idx : Cardinal;
begin
  if objPK <> 0 then
    for idx := Cardinal( objPK ) to FObjectList.Count - 1 do
      if FObjectList[idx] <> nil then
      begin
        Result := idx + 1;
        Exit;
      end;
  Result := 0;
end;

procedure TGMemoryObjectSource.Remove(obj: IGMapPoint);
var
  idx : integer;
begin
  idx := FObjectList.IndexOf(obj);

  if idx >= 0 then
  begin
    Layer.Selection.Remove( obj.PrimaryKey );  // remove from selection
    FObjectList[idx] := nil;
    Inc( FNullObjects );
  end;
end;

procedure TGMemoryObjectSource.SaveToXML(Element: TGXML_Element);
begin
  inherited;

  if FDataReader <> nil then
    FDataReader.SaveToXML( Element.AddElement( 'DataReader' ));
end;

procedure TGMemoryObjectSource.SetDataReader(const Value: TGDataReader);
var
  obj : IGMapPoint;
begin
  FreeAndNil( FDataReader );

  FDataReader := Value;

  if FDataReader <> nil then
  begin
    Clear;
    Name := FDataReader.Name;
    obj := FDataReader.First;
    while obj <> nil do
    begin
      Add( obj );
      obj := FDataReader.Next;
    end;
    Presenters.Assign(FDataReader.Presenters);
    MetaData.Assign(FDataReader.MetaData);
  end;
end;

initialization
  RegisterGlobeClass( TGMemoryObjectSource, 'Memory Object Source' );
end.
