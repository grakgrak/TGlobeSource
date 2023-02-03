//-----------------------------------------------------------------------
//Summary
//TGlobe map object classes
//
//Description
//Provides the basic TGlobe map objects.
//
//Author
//Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------

{$I GLOBE5.INC}
unit GMapObjects;

interface

Uses Windows, Classes, SysUtils, Graphics, GSysUtils, GClasses,
    GDataLink, GBitmap, GMorphing, GXML, jpeg, Math;

const
 IMAGE_CACHE_SZ = 256;

type
  TGMapRaster = class;

  TGMapPoint = class(TGInterfacedPersistent, IGMapPoint, IGUnderlyingObject)
  private
    FUserXML: TGXML_Element;
  protected
    FModified : Boolean;

    FPrimaryKey : TGPrimaryKey;
    FPresenterID : integer;
    FUserID: String;
    FObjectDataCSL: String;
    FDataLink: TGDataLink;
    FObjectSource: TGObjectSource;
    FObjectMER : TGMER;
    FCentroid : TGPointLL;
    FObjectState : TGMapObjectStateSet;

    function GetUnderlyingObject : TPersistent;

    function GetCentroid : TGPointLL; virtual;
    procedure SetCentroid(const ptLL : TGPointLL); virtual;
    procedure SetHidden(Value : Boolean); virtual;
    function GetHidden : Boolean; virtual;
    procedure SetPrimaryKey(Value : TGPrimaryKey); virtual;
    function GetPrimaryKey : TGPrimaryKey; virtual;
    procedure SetObjectDataCSL(const Value : String); virtual;
    function GetObjectDataCSL: String; virtual;
    procedure SetPresenterID(const Value : integer); virtual;
    function GetPresenterID : integer; virtual;
    procedure SetSelected(Value : Boolean); virtual;
    function GetSelected : Boolean; virtual;
    procedure SetModified(Value : Boolean); virtual;
    function GetModified : Boolean; virtual;
    function GetObjectMER : TGMER; virtual;
    procedure SetObjectMER( Value : TGMER ); virtual;
    function GetObjectSource : TGObjectSource; virtual;
    procedure SetObjectSource( Value : TGObjectSource ); virtual;
    function GetAttribute( index : integer ) : String; virtual;
    procedure SetAttribute( index : integer; const Value : String ); virtual;
    function GetValue( index : integer ) : Double; virtual;
    procedure SetValue( index : integer; const Value : Double ); virtual;
    function GetUserID: String;
    procedure SetUserID(const Value: String);
    function GetDataLink: TGDataLink;
    procedure SetDataLink(const Value: TGDataLink);
    function GetXML: String;
    procedure SetXML(const Value: String);
  public
    ObjectFlags : TGMapObjectFlagSet;

    constructor Create; virtual;
    destructor Destroy; override;

    procedure Assign(Source : IGMapPoint); reintroduce; virtual;
    function ObjectClassName : String;
    function Clone : IGMapPoint; virtual;
    function Layer : TGLayer; virtual;
    procedure Delete;
    procedure Invalidate;
    function ObjectSize : integer; virtual;
    function CachableObject: Boolean; virtual;

    procedure CancelEdit; virtual;
    procedure Edit; virtual;
    procedure Post; virtual;

    // Data Persistance
    procedure ReadFromDataLink( aDataLink : TGDataLink ); virtual;
    procedure WriteToDataLink( aDataLink : TGDataLink ); virtual;
    procedure LoadFromXML( Element : TGXML_Element ); virtual;
    procedure SaveToXML( Element : TGXML_Element ); virtual;

    procedure Render(Globe : TGCustomGlobe5); virtual;

    function LLInObject(const ptLL : TGPointLL; iTolerance : integer) : Boolean; virtual;
    function LLInObjectMER( const ptLL : TGPointLL; iTolerance : integer) : TGMERHitZone; virtual;
    function IntersectMER( const aMER : TGMER ) : Boolean; virtual;

    property DataLink : TGDataLink read GetDataLink write SetDataLink;
    property Modified : Boolean read GetModified write SetModified;
    property ObjectMER : TGMER read GetObjectMER write SetObjectMER;
    property ObjectSource : TGObjectSource read GetObjectSource write SetObjectSource;
    property Attribute[index:integer] : String read GetAttribute write SetAttribute;
    property Value[index:integer] : Double read GetValue write SetValue;
    property Centroid : TGPointLL read GetCentroid write SetCentroid;
    property PrimaryKey : TGPrimaryKey read GetPrimaryKey write SetPrimaryKey;
    property UserXML : TGXML_Element read FUserXML write FUserXML;
  published
    property Hidden : Boolean read GetHidden write SetHidden;
    property ObjectDataCSL : String read GetObjectDataCSL write SetObjectDataCSL;
    property PresenterID : integer read GetPresenterID write SetPresenterID;
    property Selected : Boolean read GetSelected write SetSelected;
    property UserID : String read GetUserID write SetUserID;
    property XML : String read GetXML write SetXML;
  end;
  TGMapObjectClass = class of TGMapPoint;


  IGMapPoly = interface(IGMapPoint)
    ['{96E080A8-E607-4ADE-A0A1-A20D2B8CAD87}']
    function GetChains: TGChainStore;
    procedure SetChains( const Value: TGChainStore );
    procedure SetClosed(Value : Boolean);
    function GetClosed : Boolean;
    function LLOnEdge( const ptLL : TGPointLL; iTolerance : integer; bClosed: Boolean) : Boolean;
    function LLInPolygon( const ptLL : TGPointLL; iTolerance : integer ) : Boolean;

    property Closed : Boolean read GetClosed write SetClosed;
    property Chains : TGChainStore read GetChains write SetChains;
  end;

  TGMapPoly = class(TGMapPoint, IGMapPoly)
  private
    FChains : TGChainStore;
    function GetChains: TGChainStore;
    procedure SetChains(const Value: TGChainStore);
    procedure SetClosed(Value : Boolean); virtual;
    function GetClosed : Boolean; virtual;
  protected
    procedure SetCentroid( const ptLL : TGPointLL ); override;
    function GetCentroid : TGPointLL; override;
  public
    destructor Destroy; override;

    function Clone : IGMapPoint; override;
    procedure Assign(Source : IGMapPoint); override;
    function ObjectSize : integer; override;

    procedure WriteToDataLink( aDataLink : TGDataLink ); override;

    procedure LoadFromXML( Element : TGXML_Element ); override;
    procedure SaveToXML( Element : TGXML_Element ); override;

    procedure Render(Globe : TGCustomGlobe5); override;

    function GetObjectMER : TGMER; override;
    function LLOnEdge( const ptLL : TGPointLL; iTolerance : integer; bClosed: Boolean) : Boolean; virtual;
    function LLInPolygon( const ptLL : TGPointLL; iTolerance : integer ) : Boolean;  virtual;
    function LLInObject( const ptLL : TGPointLL; iTolerance : integer ) : Boolean; override;
    function IntersectMER( const aMER : TGMER ): Boolean; override;
  published
    property Closed : Boolean read GetClosed write SetClosed;
    property Chains : TGChainStore read GetChains write SetChains;
  end;
  TGMapPolyClass = class of TGMapPoly;


  IGRasterMorph = interface
  ['{B5C1EAEA-440B-40FC-964B-B03A72E5AFAE}']
    function GetCount : integer;
    function GetControlPair(index: integer): TGControlPair;

    function Add : integer;
    procedure Delete( index : integer );

    property Count : integer read GetCount;
    property ControlPairs[index:integer] : TGControlPair read GetControlPair;
  end;

  IGMapRaster = interface(IGMapPoint)
    ['{98FC4ACC-F58E-4416-AF38-BA3EC948E3CD}']
    function GetImageName: TFilename;
    procedure SetImageName(const Value: TFilename);
    function GetMorph: TGBeierNeelyMorph;
    function GetAlphaBlend : Byte;
    procedure SetAlphaBlend( const Value : Byte );
    function GetTransparentColor : TColor;
    procedure SetTransparentColor( const Value : TColor );
    function ImageWidth : integer;
    function ImageHeight : integer;

    property AlphaBlend : Byte read GetAlphaBlend write SetAlphaBlend;
    property ImageName : TFilename read GetImageName write SetImageName;
    property TransparentColor : TColor read GetTransparentColor write SetTransparentColor;
  end;


  TGRasterData = class(TGRoot)
  protected
    FImageName : TFilename;
    FMapRaster : TGMapRaster;
    procedure SetImageName(const Value: TFilename); virtual;
    procedure LoadImageData; virtual; abstract;
  public
    constructor Create( mapRaster : TGMapRaster );

    procedure Render(Globe : TGCustomGlobe5 ); virtual; abstract;

    function ImageWidth : integer; virtual; abstract;
    function ImageHeight : integer; virtual; abstract;

    property ImageName : TFilename read FImageName write SetImageName;
  end;

  TGImageDataType = ( idtJPG, idtPNG, idtUnknown );

  TGRasterXMRData = class( TGRasterData )
  private
    FImageLevels : integer;
    FImageWidth : integer;
    FImageHeight : integer;
    FImageTileSize : integer;
    FImageScalePower : integer;
    FImageDataType : TGImageDataType;

    FDatFile : TFileStream;
    FImageXML : TGXML_Document;
    function CalcLevel( sf : Double ) : integer;
    function VirtualTileSize( level : integer ) : integer;
    function FetchTile( elMosaic, elTile : TGXML_Element ) : TGBitmap;
    function LoadTileStream( const source : string ) : TMemoryStream;
    procedure LoadXMR( xmlStream : TStream  );
    function ImageDataType( const dataType : string ) : TGImageDataType;
  protected
    procedure LoadImageData; override;
  public
    constructor Create( mapRaster : TGMapRaster );
    destructor Destroy; override;

    procedure Render(Globe : TGCustomGlobe5); override;

    function ImageWidth : integer; override;
    function ImageHeight : integer; override;
  end;


  TGRasterBitmapData = class( TGRasterData )
  private
    FImage : TGBitmap;
  protected
    procedure LoadImageData; override;
  public
    constructor Create( mapRaster : TGMapRaster );
    destructor Destroy; override;
    procedure Render(Globe : TGCustomGlobe5); override;

    function ImageWidth : integer; override;
    function ImageHeight : integer; override;
  end;

  TGMapRaster = class(TGMapPoint, IGMapRaster)
  private
    FMorph : TGBeierNeelyMorph;
    FRasterData : TGRasterData;

    FAlphaBlend : Byte;
    FTransparentColor : TColor;
    FImageFilename: TFilename;
    procedure MorphChanged( Sender : TObject );
    procedure SetImageFilename(const Value: TFilename);
  protected
    function GetAlphaBlend: Byte;  virtual;
    function GetCentroid : TGPointLL; override;
    function GetImageName: TFilename;  virtual;
    function GetMorph: TGBeierNeelyMorph;  virtual;
    function GetTransparentColor: TColor;  virtual;

    procedure SetAlphaBlend(const Value: Byte);  virtual;
    procedure SetCentroid( const ptLL : TGPointLL ); override;
    procedure SetImageName(const Value: TFilename);  virtual;
    procedure SetTransparentColor(const Value: TColor);  virtual;

    procedure RenderTextureQuad(Globe : TGCustomGlobe5; bmp : TGBitmap;
      srcRect, imageRect : TRect); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Clone : IGMapPoint; override;
    procedure Assign(Source : IGMapPoint); override;

    function GetObjectMER : TGMER; override;
    function LLInObject(const ptLL : TGPointLL; iTolerance : integer ) : Boolean; override;
    function IntersectMER( const aMER : TGMER ) : Boolean; override;

    function CachableObject: Boolean; override;
    procedure Render(Globe : TGCustomGlobe5); override;

    procedure WriteToDataLink( aDataLink : TGDataLink ); override;
    procedure ReadFromDataLink( aDataLink : TGDataLink ); override;

    procedure LoadFromXML( Element : TGXML_Element ); override;
    procedure SaveToXML( Element : TGXML_Element ); override;

    function ImageWidth : integer; virtual;
    function ImageHeight : integer; virtual;
    function ImageEmpty : Boolean; virtual;

    property Morph : TGBeierNeelyMorph read GetMorph;
    property ImageFilename : TFilename read FImageFilename write SetImageFilename;
  published
    property ImageName : TFilename read GetImageName write SetImageName;
    property TransparentColor : TColor read GetTransparentColor write SetTransparentColor;
    property AlphaBlend : Byte read GetAlphaBlend write SetAlphaBlend;
  end;
  TGMapRasterClass = class of TGMapRaster;


  TGMRGraphic = class(TGraphic)
  protected
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
  end;

function MapObjectToXML( mapObj : IGMapPoint ) : String;
function XMLToMapObject( xml : String ) : IGMapPoint;

var
  gTileCache : TStringList;

implementation

Uses Globe5, GResource, Controls;

function MapObjectToXML( mapObj : IGMapPoint ) : String;
var
  doc : TGXML_Document;
begin
  doc := TGXML_Document.Create('MapObject');
  try
    mapObj.SaveToXML(doc.Document);

    Result := doc.AsString;
  finally
    doc.Free;
  end;
end;

function XMLToMapObject( xml : String ) : IGMapPoint;
var
  doc : TGXML_Document;
  className : String;
begin
  doc := TGXML_Document.Create('MapObject');
  try
    doc.AsString := xml;

    className := doc.Document.Attribute('Class', '');

    Result := TGMapObjectClass(FindClass(className)).Create as IGMapPoint;

    Result.LoadFromXML(doc.Document);
    Result.Invalidate;
  finally
    doc.Free;
  end;
end;

procedure TGMapPoint.Assign(Source: IGMapPoint);
begin
  with Source do
  begin
    Self.ObjectMER := ObjectMER;
    Self.Centroid := Centroid;
//    Self.PrimaryKey := PrimaryKey;
    Self.Hidden := Hidden;
    Self.ObjectDataCSL := ObjectDataCSL;
    Self.PresenterID := PresenterID;
    Self.UserID := UserID;
  end;
  Invalidate;
end;

function TGMapPoint.CachableObject: Boolean;
begin
  Result := false;  // By default do not cache objects
end;

procedure TGMapPoint.CancelEdit;
begin
  FModified := false;
  ObjectSource.EndEditObject(Self);
end;

function TGMapPoint.Clone: IGMapPoint;
begin
  Result := TGMapObjectClass(Self.ClassType).Create as IGMapPoint;
  Result.Assign(Self);
end;

constructor TGMapPoint.Create;
begin
  FCentroid := PointLLH(0,0,-MaxInt);
  FObjectDataCSL := #9;
end;

procedure TGMapPoint.Delete;
begin
  Selected := false;
  
  if FDataLink <> nil then
    FDataLink.Delete;

  ObjectSource.Remove(Self);
end;

destructor TGMapPoint.Destroy;
begin
  DataLink := nil;
  FreeAndNil( FUserXML );
  inherited;
end;

procedure TGMapPoint.Edit;
begin
  ObjectSource.EditObject(Self);
  FModified := true;
end;

function TGMapPoint.GetAttribute(index: integer): String;
begin
  Result := StringReplace( ItemFromCSL( ObjectDataCSL, index ), '#2c;', ',', [rfReplaceAll] );
end;

function TGMapPoint.GetCentroid: TGPointLL;
begin
  if FCentroid.iHeightZ = -MaxInt then
    FCentroid.iHeightZ := 0;

  Result := FCentroid;
end;

function TGMapPoint.GetDataLink: TGDataLink;
begin
  Result := FDataLink;
end;

function TGMapPoint.GetHidden: Boolean;
begin
  Result := osHidden in FObjectState;
end;

function TGMapPoint.GetModified: Boolean;
begin
  Result := FModified;
end;

function TGMapPoint.GetPrimaryKey: TGPrimaryKey;
begin
  Result := FPrimaryKey;
end;

function TGMapPoint.GetObjectDataCSL: String;
begin
  // Demand load the Object data CSL
  if FObjectDataCSL = #9 then
  begin
    if FDataLink <> nil then
      FObjectDataCSL := FDataLink.ObjectDataCSL
    else
      FObjectDataCSL := '';
  end;

  Result := FObjectDataCSL;
end;

function TGMapPoint.GetObjectMER: TGMER;
begin
  if MER_IsEmpty(FObjectMER) then
    FObjectMER := MER( Centroid.iLongX, Centroid.iLatY, 1, 1 );

  Result := FObjectMER;
end;

function TGMapPoint.GetObjectSource: TGObjectSource;
begin
  Result := FObjectSource;
end;

function TGMapPoint.GetPresenterID: integer;
begin
  Result := FPresenterID;
end;

function TGMapPoint.GetSelected: Boolean;
begin
  Result := ObjectSource.Layer.Selection.Contains(Self.PrimaryKey);
end;

function TGMapPoint.GetUnderlyingObject: TPersistent;
begin
  Result := Self;
end;

function TGMapPoint.GetUserID: String;
begin
  Result := FUserID;
end;

function TGMapPoint.GetValue(index: integer): Double;
begin
  Result := StrToExtendedDef( ItemFromCSL( ObjectDataCSL, index ), NaN );
end;

function TGMapPoint.GetXML: String;
var
  doc : TGXML_Document;
begin
  doc := TGXML_Document.Create('MapObject');

  SaveToXML( doc.Document );
  Result := doc.AsString;

  doc.Free;
end;

function TGMapPoint.IntersectMER(const aMER: TGMER): Boolean;
begin
  Result := MER_ContainsLL( aMER, Centroid );
end;

procedure TGMapPoint.Invalidate;
begin
  if ClassType <> TGMapPoint then
    FCentroid.iHeightZ := -MaxInt;  // only reset the centroid if not a point object

  MER_Empty( FObjectMER );
  FModified := True;
end;

function TGMapPoint.Layer : TGLayer;
begin
  if ObjectSource <> nil then
    Result := ObjectSource.Layer
  else
    Result := nil;
end;

function TGMapPoint.LLInObject(const ptLL: TGPointLL; iTolerance: integer): Boolean;
begin
  Result := LLInObjectMER( ptLL, iTolerance ) <> hzNone;
end;

function TGMapPoint.LLInObjectMER( const ptLL: TGPointLL; iTolerance: integer): TGMERHitZone;
var
  aMER : TGMER;
begin
  Result := hzNone;

  if not Hidden then
  begin
    aMER := MER_Inflate( ObjectMER, iTolerance, iTolerance );
    Result := MER_ContainsLLEx(aMER, ptLL);
  end;
end;

procedure TGMapPoint.LoadFromXML(Element: TGXML_Element);
begin
  if Element <> nil then
  begin
    Centroid := StrToPointLL( Element.Attribute( 'Centroid', ''));
    Hidden := Element.Attribute( 'Hidden', false );
    ObjectDataCSL := Element.Attribute( 'CSL', '' );
    PresenterID := Element.Attribute( 'PresenterID', 0 );
    UserID := Element.Attribute( 'UserID', '' );
    if Element.Element('UserXML') <> nil then
    begin
      UserXML := TGXML_Element.Create('UserXML');
      UserXML.Assign(Element.Element('UserXML'));
    end;
  end;
end;

function TGMapPoint.ObjectClassName: String;
begin
  Result := ClassName;
end;

function TGMapPoint.ObjectSize: integer;
begin
  Result := InstanceSize;
end;

procedure TGMapPoint.Post;
begin
  if FModified and ( DataLink <> nil ) then
  begin
    Invalidate; //make sure that the MER is correctly calculated
    WriteToDataLink(DataLink);
    DataLink.Post;
    ObjectSource.EndEditObject(Self);
  end;
  FModified := false;

end;

procedure TGMapPoint.ReadFromDataLink(aDataLink: TGDataLink);
begin
  DataLink := aDataLink;

  if aDataLink <> nil then
  begin
    FPresenterID := aDataLink.PresenterID;
    FObjectState := aDataLink.State;
    FPrimaryKey := aDataLink.PrimaryKey;
    FObjectMER := aDataLink.MER;
    FCentroid := aDataLink.Centroid;
    FUserID := aDataLink.UserID;
  end;
end;

procedure TGMapPoint.Render(Globe: TGCustomGlobe5);
begin
  with TGlobe5( Globe ) do
    if Projector.ProjectionModel.PointLLToXY(Centroid, 0) then
      with Renderer.Points[0] do
        Renderer.DrawEllipse(Rect(X - 2, Y - 2, X + 2, Y + 2));
end;

procedure TGMapPoint.SaveToXML(element: TGXML_Element);
begin
  Element.AddAttribute( 'Class', ObjectClassName, '');
  Element.AddAttribute( 'Centroid', PointLLToStr( Centroid ), '');
  Element.AddAttribute( 'Hidden', Hidden, false );
  Element.AddAttribute( 'CSL', ObjectDataCSL, '' );
  Element.AddAttribute( 'PresenterID', PresenterID, 0 );
  Element.AddAttribute( 'UserID', UserID, '' );

  if UserXML <> nil then
    Element.AddElement( 'UserXML' ).Assign(UserXML);
end;

procedure TGMapPoint.SetAttribute(index: integer; const Value: String);
begin
  ObjectDataCSL := ItemToCSL( ObjectDataCSL, StringReplace( Value, ',', '#2c;', [rfReplaceAll] ), index );
  FModified := true;
end;

procedure TGMapPoint.SetCentroid(const ptLL: TGPointLL);
begin
  FCentroid := ptLL;  // just update the Centroid without a redraw
  FObjectMER := MER( ptLL.iLongX, ptLL.iLatY, 1, 1 );
  FModified := true;
end;

procedure TGMapPoint.SetDataLink(const Value: TGDataLink);
begin
  if FDataLink <> Value then
  begin
    FDataLink.Free;
    FDataLink := Value;
  end;
end;

procedure TGMapPoint.SetHidden(Value: Boolean);
begin
  if Hidden <> Value then
  begin
    if Value then
      Include(FObjectState, osHidden)
    else
      Exclude(FObjectState, osHidden);

    FModified := true;
  end;
end;

procedure TGMapPoint.SetModified(Value: Boolean);
begin
  FModified := Value;
end;

procedure TGMapPoint.SetObjectDataCSL(const Value: String);
begin
  if FObjectDataCSL <> Value then
  begin
    FObjectDataCSL := Value;
    FModified := true;
  end;
end;

procedure TGMapPoint.SetObjectMER(Value: TGMER);
begin
  FObjectMER := Value;
  FModified := true;
end;

procedure TGMapPoint.SetObjectSource(Value: TGObjectSource);
begin
  FObjectSource := Value;
end;

procedure TGMapPoint.SetPresenterID(const Value: integer);
begin
  if FPresenterID <> Value then
  begin
    FPresenterID := Value;
    FModified := true;
  end;
end;

procedure TGMapPoint.SetPrimaryKey(Value: TGPrimaryKey);
begin
  FPrimaryKey := Value;
end;

procedure TGMapPoint.SetSelected(Value: Boolean);
begin
  if value then
    ObjectSource.Layer.Selection.Add( Self.PrimaryKey )
  else
    ObjectSource.Layer.Selection.Remove( Self.PrimaryKey );
end;

procedure TGMapPoint.SetUserID(const Value: String);
begin
  if FUserID <> Value then
  begin
    FUserID := Value;
    FModified := true;
  end;
end;

procedure TGMapPoint.SetValue(index: integer; const Value: Double);
begin
  ObjectDataCSL := ItemToCSL( ObjectDataCSL, ExtendedToStr( Value ), index );
  FModified := true;
end;

procedure TGMapPoint.SetXML(const Value: String);
var
  doc : TGXML_Document;
begin
  doc := TGXML_Document.Create('MapObject');

  doc.AsString := Value;
  LoadFromXML( doc.Document );
  doc.Free;

  Invalidate; // to re-calculate the object MER
end;

procedure TGMapPoint.WriteToDataLink( aDataLink: TGDataLink);
begin
  DataLink := aDataLink;

  if aDataLink <> nil then
  begin
    aDataLink.UserID := UserID;
    aDataLink.Centroid := Centroid;
    aDataLink.MER := ObjectMER;
    aDataLink.PresenterID := PresenterID;

    aDataLink.State := FObjectState;
    aDataLink.ObjectDataCSL := ObjectDataCSL;
    aDataLink.Kind := tgokPoint;
  end;
end;



procedure TGMapPoly.Assign(Source : IGMapPoint);
var
  idx : integer;
begin
  inherited Assign(Source);

  if ( Source as IGMapPoly ) <> nil then
  begin
    Closed := (Source as IGMapPoly).Closed;
    Chains.Count := 0;  // To release any existing chains
    Chains.Count := (Source as IGMapPoly).Chains.Count;
    for idx := 0 to Chains.Count - 1 do
      Chains[idx] := (Source as IGMapPoly).Chains[idx].Clone;
  end;
end;


function TGMapPoly.Clone : IGMapPoint;
begin
  Result := TGMapPolyClass(Self.ClassType).Create;
  Result.Assign( Self as IGMapPoint );
end;


destructor TGMapPoly.Destroy;
begin
  FreeAndNil( FChains );

  inherited Destroy;
end;


function TGMapPoly.GetCentroid : TGPointLL;
begin
  if FCentroid.iHeightZ = -MaxInt then
  begin
    if Chains.Count > 0 then
      FCentroid := Chains.Centroid
    else
      FCentroid.iHeightZ := 0;
  end;

  Result := FCentroid;
end;


function TGMapPoly.GetChains: TGChainStore;
var
  memStream : TMemoryStream;
begin
  // Demand load the object chainstore
  if FChains = nil then
  begin
    FChains := TGChainStore.Create;
    if DataLink <> nil then
    begin
      memStream := TMemoryStream.Create;
      DataLink.ReadDataStream(memStream);
      FChains.ReadFromStream( memStream );
      memStream.Free;
    end;
  end;
  Result := FChains;
end;


function TGMapPoly.GetClosed: Boolean;
begin
  Result := osClosed in FObjectState;
end;


function TGMapPoly.GetObjectMER : TGMER;
begin
  if MER_IsEmpty( FObjectMER ) then
  begin
    if (Chains.Count > 0 ) and (Chains[0].Count > 0 ) then
      FObjectMER := Chains.ChainStoreMER
    else
      with Centroid do
        FObjectMER := MER(iLongX, iLatY, 1, 1);
  end;

  Result := FObjectMER;
end;


function TGMapPoly.IntersectMER( const aMER: TGMER): Boolean;
begin
  Result := false;
  if Closed then  // Check to see if the MER is inside the polygon
    Result := LLInObject(MER_CenterLL( aMER ), 0);

  if not Result then
    if Chains.Count > 0 then
      Result := Chains.IntersectMER( aMER )
    else
      Result := MER_ContainsLL( aMER, Centroid );
end;


function TGMapPoly.LLInObject( const ptLL : TGPointLL; iTolerance : integer ) : Boolean;
begin
  if Closed then
    Result := LLInPolygon( ptLL, iTolerance )
  else
    Result := LLOnEdge( ptLL, iTolerance, Closed );
end;

function TGMapPoly.LLInPolygon( const ptLL : TGPointLL; iTolerance : integer ) : Boolean;
var
  iChain : integer;
  hitZone : TGMERHitZone;
begin
  Result := False;

  hitZone := LLInObjectMER( ptLL, iTolerance );
  if hitZone = hzNone then
    Exit;

  if Chains.Count > 0 then
    for iChain := 0 to Chains.Count - 1 do
      with Chains[iChain] do
        if not Hidden then
        begin
          Result := PointInPolygon( ptLL.iLongX, ptLL.iLatY, hitZone );
          if Result then
            Exit;
        end;
end;

function TGMapPoly.LLOnEdge( const ptLL: TGPointLL; iTolerance: integer;
  bClosed: Boolean): Boolean;
var
  iChain : integer;
  hitZone : TGMERHitZone;
begin
  Result := False;

  hitZone := LLInObjectMER( ptLL, iTolerance );
  if hitZone = hzNone then
    Exit;

  if Chains.Count > 0 then
    for iChain := 0 to Chains.Count - 1 do
      with Chains[iChain] do
        if not Hidden then
        begin
          Result := PointOnEdge( ptLL.iLongX, ptLL.iLatY, iTolerance, bClosed ) >= 0;
          if Result then
            Exit;
        end;
end;

procedure TGMapPoly.LoadFromXML(Element: TGXML_Element);
begin
  inherited;
  if Element <> nil then
  begin
    Closed := Element.Attribute( 'Closed', false );
    Chains.LoadFromXML( Element.Element('Chains'));
  end;
end;

function TGMapPoly.ObjectSize : integer;
begin
  Result := inherited ObjectSize;

  if FChains <> nil then
    Result := Result + FChains.ObjectSize;
end;

procedure TGMapPoly.Render(Globe : TGCustomGlobe5);
begin
  if ( Chains = nil ) or ( Chains.Count = 0 ) then
    inherited Render( Globe )  // default render of the object
  else
    with TGlobe5(Globe) do
    begin
      if Closed then
        RenderChainStore( Chains, [rsClosed] )
      else
        RenderChainStore( Chains, [] );
    end;
end;

procedure TGMapPoly.SaveToXML(element: TGXML_Element);
begin
  inherited;
  Element.AddAttribute( 'Closed', Closed, false );
  Chains.SaveToXML( Element.AddElement('Chains'));
end;

procedure TGMapPoly.SetCentroid( const ptLL : TGPointLL );
var
  iChain : integer;
  dx, dy, dz : integer;
begin
  dx := ptLL.iLongX - Centroid.iLongX;
  dy := ptLL.iLatY - Centroid.iLatY;
  dz := ptLL.iHeightZ - Centroid.iHeightZ;

  for iChain := 0 to Chains.Count - 1 do
    with Chains[iChain] do
      Translate( dx, dy, dz );

  inherited SetCentroid( ptLL );
end;


procedure TGMapPoly.SetChains(const Value: TGChainStore);
begin
  FChains := Value;
end;


procedure TGMapPoly.SetClosed(Value: Boolean);
begin
  if Closed <> Value then
  begin
    if Value then
      Include(FObjectState, osClosed)
    else
      Exclude(FObjectState, osClosed);

    FModified := true;
  end;
end;


procedure TGMapPoly.WriteToDataLink( aDataLink: TGDataLink);
var
  memStream : TMemoryStream;
begin
  inherited;

  aDataLink.Kind := tgokPoly;

  if Chains.Count > 0 then
  begin
    memStream := TMemoryStream.Create;

    Chains.WriteToStream( memStream );
    aDataLink.WriteDataStream(memStream);

    memStream.Free;
  end;
end;


procedure TGMapRaster.Assign(Source: IGMapPoint);
begin
  inherited Assign(Source);

  if Source as IGMapRaster <> nil then
  begin
    ImageName := (Source as IGMapRaster).ImageName;
    TransparentColor := (Source as IGMapRaster).TransparentColor;
    AlphaBlend := (Source as IGMapRaster).AlphaBlend;
    Morph.Assign((Source as IGMapRaster).GetMorph);
  end;
end;

function TGMapRaster.CachableObject: Boolean;
begin
  Result := true; // can cache Raster objects
end;

function TGMapRaster.Clone: IGMapPoint;
begin
  Result := TGMapRasterClass(Self.ClassType).Create;
  Result.Assign( Self as IGMapPoint );
end;

constructor TGMapRaster.Create;
begin
  inherited;

  FAlphaBlend := 255;
  FTransparentColor := clNone;
end;

destructor TGMapRaster.Destroy;
begin
  FreeAndNil( FRasterData );
  FreeAndNil( FMorph );
  inherited;
end;

function TGMapRaster.GetAlphaBlend: Byte;
begin
  Result := FAlphaBlend;
end;

function TGMapRaster.GetCentroid: TGPointLL;
begin
  if FCentroid.iHeightZ = -MaxInt then
    FCentroid := PointLL( Morph.OffsetX, Morph.OffsetY );

  Result := FCentroid;
end;

function TGMapRaster.GetImageName: TFilename;
begin
  Result := FRasterData.ImageName;
end;

function TGMapRaster.GetMorph: TGBeierNeelyMorph;
begin
  if FMorph = nil then
  begin
    FMorph := TGBeierNeelyMorph.Create;
    FMorph.OnChanged := MorphChanged;
  end;

  Result := FMorph;
end;

function TGMapRaster.GetObjectMER: TGMER;
const
  STEPCOUNT = 4;
var
  ptStore : TGPointStore;
  idx : integer;
  step : Double;
begin
  if MER_IsEmpty( FObjectMER ) and not ImageEmpty then
  begin
    ptStore := TGPointStore.Create;

    // generate points all about the image
    step := ImageWidth / STEPCOUNT;
    for idx:= 0 to STEPCOUNT do
    begin
      ptStore.Add( Morph.XYToWorld( Round( step * idx ), 0 ));
      ptStore.Add( Morph.XYToWorld( Round( step * idx ), ImageHeight ));
    end;

    step := ImageHeight / STEPCOUNT;
    for idx:= 1 to STEPCOUNT - 1 do
    begin
      ptStore.Add( Morph.XYToWorld( 0, Round( step * idx )));
      ptStore.Add( Morph.XYToWorld( ImageWidth, Round( step * idx )));
    end;

    FObjectMER := ptStore.PointStoreMER;
    ptStore.Free;
  end;

  Result := FObjectMER;
end;

function TGMapRaster.GetTransparentColor: TColor;
begin
  Result := FTransparentColor;
end;

function TGMapRaster.ImageEmpty: Boolean;
begin
  Result := ( FRasterData <> nil ) and ( FRasterData.ImageHeight = 0 ) and ( FRasterData.ImageWidth = 0 );
end;

function TGMapRaster.ImageHeight: integer;
begin
  Result := FRasterData.ImageHeight;
end;

function TGMapRaster.Imagewidth: integer;
begin
  Result := FRasterData.ImageWidth;
end;

function TGMapRaster.IntersectMER( const aMER: TGMER): Boolean;
begin
  Result := MER_Intersect(GetObjectMER, aMER);
end;

function TGMapRaster.LLInObject( const ptLL: TGPointLL; iTolerance: integer): Boolean;
const STEPCOUNT = 10;
var
  ptStore : TGPointStore;
  idx : integer;
  step : Double;
begin
  ptStore := TGPointStore.Create;

  // generate points all about the image
  step := ImageWidth / STEPCOUNT;
  for idx:= 0 to STEPCOUNT do
    ptStore.Add( Morph.XYToWorld( Round( step * idx ), 0 ));

  step := ImageHeight / STEPCOUNT;
  for idx:= 1 to STEPCOUNT - 1 do
    ptStore.Add( Morph.XYToWorld( ImageWidth, Round( step * idx )));

  step := ImageWidth / STEPCOUNT;
  for idx:= STEPCOUNT downto 0 do
    ptStore.Add( Morph.XYToWorld( Round( step * idx ), ImageHeight ));

  step := ImageHeight / STEPCOUNT;
  for idx:= STEPCOUNT - 1 downto 0 do
    ptStore.Add( Morph.XYToWorld( 0, Round( step * idx )));

  Result := ptStore.PointInPolygon( ptLL.iLongX, ptLL.iLatY, LLInObjectMER(ptLL, 0));
  ptStore.Free;
end;

procedure TGMapRaster.LoadFromXML( Element: TGXML_Element);
begin
  inherited;

  if Element <> nil then
  begin
    ImageName := Element.Attribute( 'ImageName', '' );
    TransparentColor := Element.Attribute( 'TransparentColor', clNone );
    AlphaBlend := Element.Attribute( 'Alpha', 255 );
    Morph.LoadFromXML( Element.Element( 'Morph' ));
  end;
end;

procedure TGMapRaster.MorphChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TGMapRaster.ReadFromDataLink(aDataLink: TGDataLink);
var
  memStream : TMemoryStream;
  tmp : string;
begin
  inherited;

  memStream := TMemoryStream.Create;
  aDataLink.ReadDataStream(memStream);
  memStream.Position := 0;

  Morph.ReadFromStream( memStream );

  with TGStreamReader.Create(memStream) do
  begin
    tmp := ReadString;  // read in the image name.
    AlphaBlend := ReadByte;
    TransparentColor := TColor( ReadInteger );

    FImageFilename := ReadString;  // read full image name
    Free;
  end;
  memStream.Free;

  ImageName := tmp; // finally assign the image name to create the RasterData object
end;

procedure TGMapRaster.Render(Globe: TGCustomGlobe5);
begin
  FRasterData.Render(Globe);
end;

procedure TGMapRaster.RenderTextureQuad(Globe : TGCustomGlobe5; bmp : TGBitmap;
  srcRect, imageRect : TRect);
var
  idx, jdx : integer;
  srcTri : TGTriangle;
  destTri : TGTriangleLL;
  selectFlag : Boolean;

  XSteps, YSteps, w, h : integer;
  s_dx, s_dy, i_dx, i_dy : Double;
  sx, sy, ix, iy : Double;
begin
  bmp.AlphaBlend := MulDiv( ObjectSource.Layer.AdjustedAlphaBlend, AlphaBlend, 255 );
  bmp.TransparentColor := TransparentColor;

  // calculate the interpolation steps
  destTri[0] := Morph.XYToWorld( imageRect.Left, imageRect.Top );
  destTri[1] := Morph.XYToWorld( imageRect.Right, imageRect.Bottom );

  XSteps := Trunc( Abs( LongDiff( destTri[1].iLongX, destTri[0].iLongX )) / ( GU_DEGREE * 5 )) + 1;
  YSteps := Trunc( Abs( destTri[1].iLatY - destTri[0].iLatY ) / ( GU_DEGREE * 5 )) + 1;

  // Fix to deal with very small bitmaps
  w := srcRect.Right - srcRect.Left;
  h := srcRect.Bottom - srcRect.Top;
  if (w = 0 ) or ( h = 0 ) then
    Exit;

  s_dx := ( w + 0.5 ) / XSteps;  // + 0.5 so that Trunc hits last pixel
  s_dy := ( h + 0.5 ) / YSteps;  // + 0.5 so that Trunc hits last pixel
  i_dx := ( imageRect.Right - imageRect.Left ) / XSteps;
  i_dy := ( imageRect.Bottom - imageRect.Top ) / YSteps;

  selectFlag := Selected;

  for idx := 0 to XSteps - 1 do
  begin
    sx := srcRect.Left + s_dx * idx;
    ix := imageRect.Left + i_dx * idx;

    for jdx := 0 to YSteps - 1 do
    begin
      sy := srcRect.Top + s_dy * jdx;
      iy := imageRect.Top + i_dy * jdx;

      srcTri[0] := Point( Trunc( sx ), Trunc( sy ));
      srcTri[1] := Point( Trunc( sx + s_dx ), Trunc( sy ));
      srcTri[2] := Point( Trunc( sx ), Trunc( sy + s_dy ));

      destTri[0] := Morph.XYToWorld( ix, iy );
      destTri[1] := Morph.XYToWorld( ix + i_dx, iy );
      destTri[2] := Morph.XYToWorld( ix, iy + i_dy );
      TGlobe5(Globe).RenderTextureTriangle( srcTri, destTri, bmp, selectFlag );

      srcTri[0] := Point( Trunc( sx + s_dx ), Trunc( sy + s_dy ));
      destTri[0] := Morph.XYToWorld( ix + i_dx, iy + i_dy );
      TGlobe5(Globe).RenderTextureTriangle( srcTri, destTri, bmp, selectFlag );
    end;
  end;
end;

procedure TGMapRaster.SaveToXML(element: TGXML_Element);
begin
  inherited;

  Element.AddAttribute( 'ImageName', ImageName, '' );
  Element.AddAttribute( 'TransparentColor', TransparentColor, clNone );
  Element.AddAttribute( 'Alpha', AlphaBlend, 255 );

  Morph.SaveToXML( Element.AddElement( 'Morph' ));
end;

procedure TGMapRaster.SetAlphaBlend(const Value: Byte);
begin
  FAlphaBlend := Value;
end;

procedure TGMapRaster.SetCentroid( const ptLL: TGPointLL);
begin
  inherited;

  Morph.SetXYOffsets( Centroid.iLongX, Centroid.iLatY );
end;

procedure TGMapRaster.SetImageFilename(const Value: TFilename);
begin
  FImageFilename := Value;
  ImageName := Value;
end;

procedure TGMapRaster.SetImageName(const Value: TFilename);
begin
  if ( FRasterData = nil ) or ( ImageName <> Value ) then
  begin
    if FImageFilename = '' then
      FImageFilename := Value;

    FRasterData.Free;

    if CompareText( ExtractFileExt(Value), '.XMR' ) = 0 then
      FRasterData := TGRasterXMRData.Create( self )
    else
      FRasterData := TGRasterBitmapData.Create( self );

    FRasterData.ImageName := Value;
    FRasterData.LoadImageData;
  end;
end;

procedure TGMapRaster.SetTransparentColor(const Value: TColor);
begin
  FTransparentColor := Value;
end;

procedure TGMapRaster.WriteToDataLink( aDataLink: TGDataLink );
var
  memStream : TMemoryStream;
begin
  inherited;
  aDataLink.Kind := tgokRaster; // indicate the type of object

  memStream := TMemoryStream.Create;

  Morph.WriteToStream( memStream );
  with TGStreamWriter.Create(memStream) do
  begin
    WriteString( ObjectSource.ImportImage( ImageName ));
    WriteByte( AlphaBlend );
    WriteInteger( TransparentColor );

    WriteString( ImageFilename );

    Free;
  end;

  aDataLink.WriteDataStream(memStream); // save to the data store
  memStream.Free;
end;

{ TGRasterData }

constructor TGRasterData.Create(mapRaster: TGMapRaster);
begin
  FMapRaster := mapRaster;
end;

procedure TGRasterData.SetImageName(const Value: TFilename);
begin
  FImageName := Value;
end;



function TGRasterXMRData.CalcLevel(sf: Double): integer;
var
  vLen : double;
begin
  Result := 0;
  while Result < FImageLevels - 1 do
  begin
    vLen := ( sf * FImageWidth * FMapRaster.Morph.ScaleX ) / ( FImageWidth / VirtualTileSize( Result ));

    if ( FImageTileSize / vLen ) <= ( 1.0 + FImageScalePower / 4 ) then
      Break;
    Inc( Result );
  end;
end;

constructor TGRasterXMRData.Create( mapRaster : TGMapRaster );
begin
  inherited;

//  TileCache := TStringList.Create;
  FImageXML := TGXML_Document.Create( 'MRI' );
  FImageDataType := idtJPG; // default to JPG image data
end;

destructor TGRasterXMRData.Destroy;
begin
  FDatFile.Free;
//  TileCache.Free;
  FImageXML.Free;
  inherited;
end;

function TGRasterXMRData.FetchTile(elMosaic, elTile: TGXML_Element): TGBitmap;
var
  idx : integer;
  imageName : string;
  img : TGraphic;
  memStream : TMemoryStream;
begin
  imageName := elMosaic.Attribute('Source', '' ) + '\' + elTile.BodyText;

  idx := gTileCache.IndexOf(imageName);

  if idx >= 0 then
  begin
    Result := TGBitmap( gTileCache.Objects[idx] );
    gTileCache.Move(idx, 0);
  end
  else
  begin
    Result := TGBitmap.Create;
    Result.Width := FImageTileSize;
    Result.Height := FImageTileSize;

    memStream := LoadTileStream(imageName);

    if memStream <> nil then
    begin
{      if FImageDataType = idtPNG then
      begin
        img := TPNGGraphic.Create
        try
          img.LoadFromStream(memStream);
          Result.Canvas.Draw(0,0,img);
        finally
          img.Free;
        end;
      end
      else
}      begin
        img := TJPEGImage.Create;
        try
          img.LoadFromStream(memStream);
          TJPEGImage(img).Smoothing := false;
          TJPEGImage(img).ProgressiveDisplay := false;
          TJPEGImage(img).DIBNeeded;
          Result.Canvas.Draw(0,0,img);
        finally
          img.Free;
        end;
      end;

      memStream.Free;
    end
    else
    begin // Just blank the bitmap to indicate no data
      Result.Canvas.Brush.Color := clWhite;
      Result.Canvas.FillRect(Result.BoundsRect);
      Result.Canvas.TextOut(Result.Width div 4, Result.Height div 2, 'No Image Data' );
    end;

    gTileCache.InsertObject(0, imageName, Result);

    while gTileCache.Count > IMAGE_CACHE_SZ do
    begin
      gTileCache.Objects[IMAGE_CACHE_SZ].Free;
      gTileCache.Delete(IMAGE_CACHE_SZ);
    end;
  end;
end;

function TGRasterXMRData.ImageDataType(const dataType: string): TGImageDataType;
begin
  Result := idtUnknown;

  if CompareText( dataType, 'JPG' ) = 0 then
    Result := idtJPG;

  if CompareText( dataType, 'PNG' ) = 0 then
    Result := idtPNG;
end;

function TGRasterXMRData.ImageHeight: integer;
begin
  Result := FImageHeight;
end;

function TGRasterXMRData.ImageWidth: integer;
begin
  Result := FImageWidth;
end;

procedure TGRasterXMRData.LoadImageData;
var
  ms : TMemoryStream;
  filename : string;
begin
  if FileExists( ImageName ) then
    filename := ImageName
  else
    if FileExists( FMapRaster.ImageFilename ) then
      filename := FMapRaster.ImageFilename;

  if FileExists( filename ) then
  begin
    ms := TMemoryStream.Create();
    ms.LoadFromFile( filename );

    LoadXMR(ms);
    ms.Free;

    // record the name of the source xml file into the XML DOM
    FImageXML.Document.AddAttribute('XMLFilename', filename, '' );

    FreeAndNil( FDatFile );
    if FileExists( ChangeFileExt( filename, '.dmr')) then
      FDatFile := TFileStream.Create( ChangeFileExt( filename, '.dmr'), fmOpenRead or fmShareDenyWrite );
  end;
end;

function TGRasterXMRData.LoadTileStream(const source: string): TMemoryStream;
var
  len : integer;
begin
  Result := nil;
  if FDatFile <> nil then
  try
    FDatFile.Position := StrToInt64Def( ExtractFileName( source ), 0 );
    FDatFile.Read( len, SizeOf(len));

    Result := TMemoryStream.Create;
    Result.CopyFrom(FDatFile, len);
    Result.Position := 0;
  except
    // ignore errors and return nil
    FreeAndNil( Result );
  end;
end;


procedure TGRasterXMRData.LoadXMR( xmlStream : TStream );
begin
  FImageXML.LoadFromStream( xmlStream );

  // Get the number of tile mosaics in the document, this represents the zoom levels
  FImageLevels := FImageXML.Document.ElementCount('Mosaic');
  FImageWidth := FImageXML.Document.Attribute('Width', 0);
  FImageHeight := FImageXML.Document.Attribute('Height', 0);
  FImageTileSize := FImageXML.Document.Attribute('TileSize', 0);
  FImageScalePower := FImageXML.Document.Attribute('ScalePower', 2);
  FImageDataType := ImageDataType( FImageXML.Document.Attribute('DataType', 'JPG'));
end;


procedure TGRasterXMRData.Render(Globe: TGCustomGlobe5);
var
  row, col, level, rowCount, x, y : integer;
  worldRight, worldBottom, tileRight, tileBottom : integer;
  b, r, vLen : Double;
  elMosaic, elRow : TGXML_Element;
  worldLL : array [0..3] of TGPointLL;
  bmp : TGBitmap;
begin
  level := CalcLevel( Globe.Projector.ScaleFactor );

  elMosaic := FImageXML.Document.Element( 'Mosaic', level );

  if elMosaic = nil then
    Exit;

  vLen := VirtualTileSize( level );

  rowCount := elMosaic.ElementCount;
  for row := 0 to rowCount - 1 do
  begin
    elRow := elMosaic.Element(row);
    y := Trunc( row * vLen );
    if elRow <> nil then
      for col := 0 to elRow.ElementCount - 1 do
      begin
        x := Trunc( col * vLen );
        //clip the tile to the Image dimenions
        r := MinValue( [1.0, ( FImageWidth - col * vLen ) / vLen] );
        b := MinValue( [1.0, ( FImageHeight - row * vLen ) / vLen] );

        tileRight := Trunc( r * ( FImageTileSize - 1 ));
        tileBottom := Trunc( b * ( FImageTileSize - 1 ));

        worldRight := x + Trunc( r * vLen );
        worldBottom := y + Trunc( b * vLen );

        worldLL[0] := FMapRaster.Morph.XYToWorld( x, FImageHeight - y );
        worldLL[1] := FMapRaster.Morph.XYToWorld( worldRight, FImageHeight - y );
        worldLL[2] := FMapRaster.Morph.XYToWorld( worldRight, FImageHeight - worldBottom );
        worldLL[3] := FMapRaster.Morph.XYToWorld( x, FImageHeight - worldBottom );

        if Globe.Renderer.TriangleLLVisible( worldLL[0], worldLL[1], worldLL[3] ) or
          Globe.Renderer.TriangleLLVisible( worldLL[2], worldLL[1], worldLL[3] ) then
        begin
          if Assigned( TGlobe5(Globe).ImageTileCache ) then
            bmp := TGlobe5(Globe).ImageTileCache.FetchMRITile( Globe, FImageXML.Document, level, row, col)
          else
            bmp := FetchTile( elMosaic, elRow.Element(col));

          if bmp <> nil then
            FMapRaster.RenderTextureQuad( Globe, bmp,
              Rect( 0, 0, tileRight, tileBottom ),
              Rect( x, FImageHeight - y, worldRight, FImageHeight - worldBottom));
        end;
      end;
  end;
end;


function TGRasterXMRData.VirtualTileSize(level: integer): integer;
begin
  Result := 1;
  while level > 0 do
  begin
    Result := Result * FImageScalePower;
    Dec( level );
  end;
  Result := Result * FImageTileSize;
end;

{ TGRasterBitmapData }

constructor TGRasterBitmapData.Create(mapRaster: TGMapRaster);
begin
  inherited;

  FImage := TGBitmap.Create;
end;

destructor TGRasterBitmapData.Destroy;
begin
  FImage.Free;
  inherited;
end;

function TGRasterBitmapData.ImageHeight: integer;
begin
  Result := FImage.Height;
end;

function TGRasterBitmapData.ImageWidth: integer;
begin
  Result := FImage.Width;
end;

procedure TGRasterBitmapData.LoadImageData;
begin
  if FileExists(ImageName) then
    FImage.LoadFromFile(ImageName)
  else
    if ( ImageName <> '' ) and ( FMapRaster.ObjectSource <> nil ) then
      FMapRaster.ObjectSource.ImageByName( FImage, FImageName );
end;

procedure TGRasterBitmapData.Render(Globe: TGCustomGlobe5);
begin
  FMapRaster.RenderTextureQuad( Globe, FImage,
    Rect( 0, 0, ImageWidth, ImageHeight ),
    Rect( 0, ImageHeight, ImageWidth, 0 ));
end;


{ TGMRGraphic }

procedure TGMRGraphic.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
end;

function TGMRGraphic.GetEmpty: Boolean;
begin
  Result := false;
end;

function TGMRGraphic.GetHeight: Integer;
begin
  Result := 0;
end;

function TGMRGraphic.GetWidth: Integer;
begin
  Result := 0;
end;

procedure TGMRGraphic.LoadFromClipboardFormat(AFormat: Word;
  AData: THandle; APalette: HPALETTE);
begin
end;

procedure TGMRGraphic.LoadFromStream(Stream: TStream);
begin
end;

procedure TGMRGraphic.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
end;

procedure TGMRGraphic.SaveToStream(Stream: TStream);
begin
end;

procedure TGMRGraphic.SetHeight(Value: Integer);
begin
end;

procedure TGMRGraphic.SetWidth(Value: Integer);
begin
end;

initialization
  gTileCache := TStringList.Create;

  RegisterGlobeClass( TGMapPoint, 'Map Point' );
  RegisterGlobeClass( TGMapPoly, 'Map Poly' );
  RegisterGlobeClass( TGMapRaster, 'Map Raster' );

  TPicture.RegisterFileFormat( 'XMR', 'Multi-Resolution Images', TGMRGraphic);

finalization
  while gTileCache.Count > 0 do
  begin
    gTileCache.Objects[0].Free;
    gTileCache.Delete(0);
  end;
  gTileCache.Free;
  TPicture.UnregisterGraphicClass( TGMRGraphic );
end.
