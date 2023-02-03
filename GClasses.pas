//-----------------------------------------------------------------------
// Summary
//	TGlobe Component General Classes
//
// Description 
// 	Common classes for TGlobe
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------

{$R-}
{$Q-}
{$I GLOBE5.INC}

unit GClasses;

interface

uses ActiveX, Forms, Windows, Classes, Graphics, Controls, SysUtils, StdCtrls,
  Printers, GSysUtils, GXML, Contnrs, GDataLink, GBitmap;

const
  TG_DEFAULT_MAXPOINTS = 256 * 1024;
  TG_PIXEL_TOLERANCE = 2;
  TG_GRATICULE_LONG_STEPS = 180;
  TG_GRATICULE_LAT_STEPS = 85;

type
  {---------------------------- Forward Class Declarations --------------------}
  TGCustomGlobe5 = class;
  TGCustomDataReader = class;
  TGProjector = class;
  TGLayer = class;
  TGPresenterStore = class;
  TGObjectSource = class;
  TGRenderer = class;
  IGMapPoint = interface;

  TPointArray = array of TPoint;
  TGFontName = String;
  TGProjectionClass = String;
  TGRenderClass = String;
  
  TPointData = record  // used by the polygon clipper
    Point : TPoint;
    Code : integer;
  end;

  TGPenStyle = (gpsSolid, gpsDash, gpsDot, gpsDashDot, gpsDashDotDot, gpsClear, gpsInsideFrame);

  TGBrushStyle = (gbsSolid, gbsClear, gbsHorizontal, gbsVertical, gbsFDiagonal,
    gbsBDiagonal, gbsCross, gbsDiagCross,
    gbsPattern1, gbsPattern2, gbsPattern3, gbsPattern4, gbsPattern5, gbsPattern6,
    gbsPattern7, gbsPattern8, gbsPattern9, gbsPattern10, gbsPattern11, gbsPattern12,
    gbsPattern13, gbsPattern14, gbsPattern15, gbsPattern16, gbsPattern17, gbsPattern18,
    gbsPattern19, gbsPattern20, gbsPattern21, gbsPattern22, gbsPattern23, gbsPattern24,
    gbsPattern25, gbsPattern26, gbsPattern27, gbsPattern28, gbsPattern29, gbsPattern30,
    gbsPattern31, gbsPattern32, gbsPattern33, gbsPattern34, gbsPattern35, gbsPattern36,
    gbsPattern37, gbsPattern38, gbsPattern39, gbsPattern40, gbsPattern41, gbsPattern42,
    gbsPattern43, gbsPattern44, gbsPattern45, gbsPattern46, gbsPattern47, gbsPattern48,
    gbsPattern49, gbsPattern50);


  TGFontStyle = (gfsBold, gfsItalic, gfsUnderline, gfsStrikeOut, gfsOpaque, gfsHalo);
  TGFontStyles = set of TGFontStyle;


  TGTitleAlignment = (taNone, taCenter, taLeft, taRight,
    taTopLeft, taTop, taTopRight, taBottomLeft, taBottom, taBottomRight);

  TGMapObjectFlags = (ofTitle, ofID, ofHeight, of16Bit, ofCompressed,
    ofSingle, ofLongCount, ofZoomMinMax, ofClosed, ofGreatCircle, ofMultiRing,
    ofUserID);
  TGMapObjectFlagSet = set of TGMapObjectFlags;

  TGObjectSourceNotification = (osnOpen, osnClose, osnAdd, osnDelete,
    osnRedraw, osnSelectedChanged, osnCreate, osnFree);

  TGObjectCallback = procedure(objPK : TGPrimaryKey; var bAbort : Boolean ) of object;


  TGXAxisChanges = (xaYOrigin, xaCentralParallel, xaNoAction);
  TGYAxisChanges = (yaXOrigin, yaCentralMeridian, yaNoAction);

  TGProjectionProperty = (ppXRotation, ppYRotation, ppZRotation, ppScaleFactor,
    ppXOrigin, ppYOrigin, ppCentralMeridian, ppCentralParallel,
    ppFirstParallel, ppSecondParallel, ppSpheroid, ppXAxisChanges,
    ppYAxisChanges, ppMaxLatitude, ppMinLatitude, ppMaxLongitude, ppMinLongitude,
    ppMaxZoom, ppMinZoom);

  IGUnderlyingObject = Interface
    ['{3B20F42E-9508-46D4-8D51-BD82169F9E1E}']
    function GetUnderlyingObject : TPersistent;
  end;

  IGPresenterEvents = Interface
    ['{38F15E88-6004-4466-AE1F-5F084A405779}']
    procedure OnBeginRender( mapPoint : IGMapPoint );
    procedure OnEndRender( mapPoint : IGMapPoint );
    procedure OnBeginLayer( aLayer : TGLayer );
    procedure OnEndLayer( aLayer : TGLayer );
  end;

  IGMapPoint = Interface
    ['{3E168D16-28C0-4348-9F68-0413A088DA75}']

    function GetCentroid : TGPointLL;
    procedure SetCentroid(const ptLL : TGPointLL);
    procedure SetHidden(Value : Boolean);
    function GetHidden : Boolean;
    procedure SetModified(Value : Boolean);
    function GetModified : Boolean;
    procedure SetPrimaryKey( Value : TGPrimaryKey );
    function GetPrimaryKey : TGPrimaryKey;
    procedure SetObjectDataCSL(const Value : String);
    function GetObjectDataCSL: String;
    procedure SetPresenterID(const Value : integer);
    function GetPresenterID : integer;
    procedure SetSelected(Value : Boolean);
    function GetSelected : Boolean;
    function GetObjectMER : TGMER;
    procedure SetObjectMER( Value : TGMER );
    function GetObjectSource : TGObjectSource;
    procedure SetObjectSource( Value : TGObjectSource );
    function GetAttribute( index : integer ) : String;
    procedure SetAttribute( index : integer; const Value : String );
    function GetValue( index : integer ) : Double;
    procedure SetValue( index : integer; const Value : Double );
    function GetUserID : String;
    procedure SetUserID( const Value : String );
    function GetDataLink : TGDataLink;
    procedure SetDataLink( const Value : TGDataLink );
    procedure SetXML(const Value : String);
    function GetXML: String;


    procedure Assign(Source : IGMapPoint);
    function ObjectClassName : String;
    function Clone : IGMapPoint;
    function Layer : TGLayer;
    procedure Delete;
    function ObjectSize: integer;
    function CachableObject: Boolean;
    procedure ReadFromDataLink( aDataLink : TGDataLink );
    procedure WriteToDataLink( aDataLink : TGDataLink );
    procedure LoadFromXML( Element : TGXML_Element );
    procedure SaveToXML( Element : TGXML_Element );

    procedure Post;
    procedure Edit;
    procedure CancelEdit;

    procedure Render(Globe : TGCustomGlobe5);

    function LLInObject( const ptLL : TGPointLL; iTolerance : integer) : Boolean;
    function LLInObjectMER( const ptLL : TGPointLL; iTolerance : integer) : TGMERHitZone;
    function IntersectMER( const aMER : TGMER ) : Boolean;
    procedure Invalidate;


    property Centroid : TGPointLL read GetCentroid write SetCentroid;
    property DataLink : TGDataLink read GetDataLink write SetDataLink;
    property ObjectMER : TGMER read GetObjectMER write SetObjectMER;
    property ObjectSource : TGObjectSource read GetObjectSource write SetObjectSource;
    property Attribute[index:integer] : String read GetAttribute write SetAttribute;
    property Value[index:integer] : Double read GetValue write SetValue;

    property Hidden : Boolean read GetHidden write SetHidden;
    property Modified : Boolean read GetModified write SetModified;
    property PrimaryKey : TGPrimaryKey read GetPrimaryKey write SetPrimaryKey;
    property ObjectDataCSL : String read GetObjectDataCSL write SetObjectDataCSL;
    property PresenterID : integer read GetPresenterID write SetPresenterID;
    property Selected : Boolean read GetSelected write SetSelected;
    property UserID : String read GetUserID write SetUserID;
    property XML : String read GetXML write SetXML;
  end;

  
  TGComponentClass = class of TGComponent;
  TGComponent = class(TGENVPersistent)
  protected
    FParentGlobe : TGCustomGlobe5;
    function GetXML: String;
    procedure SetXML(const Value: String);
  public
    constructor Create( parentGlobe: TGCustomGlobe5 ); virtual;
    class function CreateFromXML(parentGlobe: TGCustomGlobe5; Element : TGXML_Element;
      defaultClass : TGComponentClass) : TGComponent;

    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;

    property ParentGlobe : TGCustomGlobe5 read FParentGlobe;
    property XML : String read GetXML write SetXML;
  end;

  
  TGAttribute = class(TGENVPersistent)
  private
    FHashValue : Cardinal;
    FRenderer : TGRenderer;
    FAttributeHandle : Cardinal;
    FModified : Boolean;
    FOnChange : TNotifyEvent;
    procedure SetModified(const Value: Boolean);
  protected
    procedure DoChange;
    procedure CreateHandle( renderer : TGRenderer ); virtual; abstract;
    procedure DestroyHandle( renderer : TGRenderer ); virtual;

    property HashValue : Cardinal read FHashValue write FHashValue;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function AttributeHandle( renderer : TGRenderer ): Cardinal; virtual;
    function Equals(Attr : TGAttribute) : Boolean; virtual; abstract;

    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;

    property Modified : Boolean read FModified write SetModified;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;
  TGlobeAttributeClass = class of TGAttribute;



  TGPen = class(TGAttribute)
  private
    FPenAltitude : Cardinal;
    FColor : TColor;
    FStyle : TGPenStyle;
    FWidth : integer;
    FWidthUnit : TGUnitTypes;
    procedure SetColor(Value : TColor);
    procedure SetStyle(Value : TGPenStyle);
    procedure SetWidth(Value : integer);
    procedure SetUnit(Value : TGUnitTypes);
  protected
    procedure CreateHandle( renderer : TGRenderer ); override;
  public
    constructor Create; override;

    procedure Define(color : TColor; style : TGPenStyle; width : integer;
      widthUnit : TGUnitTypes);

    procedure Assign(Attr : TPersistent); override;
    procedure AssignTo(Attr : TPersistent); override;
    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;

    function AttributeHandle( renderer : TGRenderer ): Cardinal; override;

    function Clone : TGPen;
    function Equals(Attr : TGAttribute) : Boolean; override;
  published
    property Color : TColor read FColor write SetColor;
    property Style : TGPenStyle read FStyle write SetStyle;
    property Width : integer read FWidth write SetWidth;
    property WidthUnit : TGUnitTypes read FWidthUnit write SetUnit;
  end;
  TGlobePenClass = class of TGPen;



  TGBrush = class(TGAttribute)
  private
    FColor : TColor;
    FStyle : TGBrushStyle;
    FBkColor: TColor;
    procedure SetColor(Value : TColor);
    procedure SetStyle(Value : TGBrushStyle);
    procedure SetBkColor(const Value: TColor);
  protected
    procedure CreateHandle( renderer : TGRenderer ); override;
  public
    constructor Create; override;

    procedure Define(color, bkColor : TColor; style : TGBrushStyle);

    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;

    function AttributeHandle( renderer : TGRenderer ): Cardinal; override;

    procedure Assign(Attr : TPersistent); override;
    procedure AssignTo(Attr : TPersistent); override;
    function Clone : TGBrush;
    function Equals(Attr : TGAttribute) : Boolean; override;
  published
    property BkColor : TColor read FBkColor write SetBkColor;
    property Color : TColor read FColor write SetColor;
    property Style : TGBrushStyle read FStyle write SetStyle;
  end;
  TGlobeBrushClass = class of TGBrush;



  TGFont = class(TGAttribute)
  private
    FontAltitude : Cardinal;
    FSize : integer;
    FSizeUnit : TGUnitTypes;
    FColor : TColor;
    FAngle : integer;
    FStyle : Byte;
    FName : TGFontName;
    FBkColor: TColor;
    FBkAlphaBlend: Byte;
    function GetStyle : TGFontStyles;
    procedure SetStyle(Value : TGFontStyles);
    procedure SetSizeUnit(Value : TGUnitTypes);
    procedure SetColor(Value : TColor);
    procedure SetAngle(Value : integer);
    procedure SetSize(Value : integer);
    procedure SetFontName(Value : TGFontName);
    procedure SetBkColor(const Value: TColor);
    procedure SetBkAlphaBlend(const Value: Byte);
  protected
    procedure CreateHandle( renderer : TGRenderer ); override;
  public
    constructor Create; override;

    procedure Define(const name : TGFontName; color, bkColor : TColor;
      size : integer; sizeUnit : TGUnitTypes; angle : integer; style : TGFontStyles;
      bkAlpha : Byte = 255);

    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;

    function AttributeHandle( renderer : TGRenderer ): Cardinal; override;

    procedure Assign(Attr : TPersistent); override;
    procedure AssignTo(Attr : TPersistent); override;
    function Clone : TGFont;
    function Equals(Attr : TGAttribute) : Boolean; override;
  published
    property Angle : integer read FAngle write SetAngle;
    property Color : TColor read FColor write SetColor;
    property BkColor : TColor read FBkColor write SetBkColor;
    property BkAlphaBlend : Byte read FBkAlphaBlend write SetBkAlphaBlend;
    property Name : TGFontName read FName write SetFontName;
    property Size : integer read FSize write SetSize;
    property Style : TGFontStyles read GetStyle write SetStyle;
    property SizeUnit : TGUnitTypes read FSizeUnit write SetSizeUnit;
  end;
  TGlobeFontClass = class of TGFont;

  TGPresenter = class(TGComponent)
  private
    FAlphaBlend: Byte;
    FHidden: Boolean;
    FName: String;
    FNextPresenterID: integer;
    FPresenterID: integer;

    FTitleColumn: integer;
    FTitleFont : TGFont;
    FTitleAlignment : TGTitleAlignment;
    FTitleOffset : integer;
    FTitleOffsetUnits : TGUnitTypes;
    FTitleVisible : TGTriState;
    FiTitleZoomMin: integer;
    FiTitleZoomMax: integer;
    FTitleZoomUnits: TGUnitTypes;

    FAltitude : Cardinal;
    FRenderer: TGRenderer;
    FAdjustedAlphaBlend: Byte;
    FPresenterEvents: IGPresenterEvents;
    function GetRenderer: TGRenderer;
  protected
    procedure AdjustTolerance( var tolerance : integer ); virtual;
    procedure BeginLayerRender( aLayer : TGLayer ); virtual;
    procedure EndLayerRender( aLayer : TGLayer ); virtual;

    procedure DoBeginRender( mapObj : IGMapPoint );
    procedure DoEndRender( mapObj : IGMapPoint );
    procedure DoBeginLayer( aLayer : TGLayer );
    procedure DoEndLayer( aLayer : TGLayer );
  public
    constructor Create(parentGlobe: TGCustomGlobe5); override;
    destructor Destroy; override;

    function LLInObject( ptLL : TGPointLL; mapObj : IGMapPoint; iTolerance : integer) : Boolean; virtual;

    procedure Assign(Source : TPersistent); override;
    function Clone : TGPresenter; virtual;
    function Equals( aPresenter : TGPresenter ): Boolean; virtual;

    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;

    procedure RenderLegend( Canvas : TCanvas; rect : TRect ); virtual;
    procedure RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet); virtual;
    procedure RenderTitle( mapObj : IGMapPoint;  State : TGRenderStateSet ); virtual;
    function IsTitleVisible : Boolean; virtual;

    property AdjustedAlphaBlend : Byte read FAdjustedAlphaBlend;
    property Renderer : TGRenderer read GetRenderer write FRenderer;
    property PresenterEvents : IGPresenterEvents read FPresenterEvents write FPresenterEvents;
  published
    property AlphaBlend : Byte read FAlphaBlend write FAlphaBlend;
    property Hidden : Boolean read FHidden write FHidden;
    property Name : String read FName write FName;
    property PresenterID : integer read FPresenterID write FPresenterID;
    property NextPresenterID : integer read FNextPresenterID write FNextPresenterID;

    property TitleAlignment : TGTitleAlignment read FTitleAlignment write FTitleAlignment;
    property TitleColumn : integer read FTitleColumn write FTitleColumn;
    property TitleFont : TGFont read FTitleFont write FTitleFont;
    property TitleOffset : integer read FTitleOffset write FTitleOffset;
    property TitleOffsetUnits : TGUnitTypes read FTitleOffsetUnits write FTitleOffsetUnits;
    property TitleZoomMax : Integer read FiTitleZoomMax write FiTitleZoomMax;
    property TitleZoomMin : Integer read FiTitleZoomMin write FiTitleZoomMin;
    property TitleZoomUnits : TGUnitTypes read FTitleZoomUnits write FTitleZoomUnits;
  end;
  TGPresenterClass = class of TGPresenter;

  TGLayerStore = class(TGComponent)
  private
    FParentLayer : TGLayer;
    FLayers : TObjectList;

    function GetCount: integer;
  protected
    function GetLayer(iIndex : integer) : TGLayer;
  public
    constructor Create(Globe : TGCustomGlobe5); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function Add(ALayer : TGLayer) : integer;
    procedure Remove(ALayer : TGLayer);
    procedure Delete(ALayer : TGLayer);
    procedure Move(iFrom, iTo : integer);
    procedure Clear;

    function LayerByID( ID : TGUID ) : TGLayer;
    function LayerByName(const sName : String) : TGLayer;
    function LayerFromPresenter( aPresenter : TGPresenter ) : TGLayer;
    function IndexOf(ALayer : TGLayer) : integer;
    function IndexByName(const sName : String) : integer;

    function CreateLayerFromXML(Element : TGXML_Element): TGLayer;
    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;

    function ObjectAtLL(ptLL : TGPointLL) : IGMapPoint;
    function ObjectAtXY(iX, iY : Integer) : IGMapPoint;
    function ObjectByTitle(const sTitle : String ) : IGMapPoint;
    function ObjectByUserID(const sUserID : String ) : IGMapPoint;

    procedure Render( const viewMER : TGMER );

    property Count : integer read GetCount;
    property Layers[iIndex : Integer] : TGLayer read GetLayer; default;
    property ParentLayer : TGLayer read FParentLayer write FParentLayer;
  end;


  TGPresenterStore = class(TGComponent)
  private
    FPresenters : TObjectList;
    FModified: Boolean;
    function GetAsXMLString: String;
    procedure SetAsXMLString(const Value: String);
  protected
    function GetPresenter(iIndex : integer) : TGPresenter;
    function GetCount : integer;
    procedure AdjustTolerance( var tolerance : integer ); virtual;
  public
    constructor Create( parentGlobe : TGCustomGlobe5 ); override;
    destructor Destroy; override;

    procedure Assign(Source : TPersistent); override;

    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;

    function Add(aPresenter : TGPresenter) : integer;
    procedure AddPresenters( presenterStore : TGPresenterStore );
    function ByID(iPresenterID : integer) : TGPresenter;
    function ByName( const PresenterName : String) : TGPresenter;
    function Contains(aPresenter : TGPresenter) : Boolean;
    procedure Delete(aPresenter : TGPresenter);
    procedure Clear;
    function FindMatch( aPresenter : TGPresenter ) : integer;

    property AsXMLString : String read GetAsXMLString write SetAsXMLString;
    property Count : integer read GetCount;
    property Modified : Boolean read FModified write FModified;
    property Presenters[iIndex : integer] : TGPresenter read GetPresenter; default;
  end;


  TGIterator = class(TGRoot)
  private
    FObjectSource : TGObjectSource;
    FCurrentPK : TGPrimaryKey;
  public
    constructor Create( objSource : TGObjectSource ); virtual;

    function First : IGMapPoint; virtual;
    function Next : IGMapPoint; virtual;
    function Last : IGMapPoint; virtual;
    function Prior : IGMapPoint; virtual;

    function Count : integer; virtual;

    property ObjectSource : TGObjectSource read FObjectSource;
  end;


  TGListIterator = class( TGIterator )
  private
    FPrimaryKeyList : TList;
    FPosition : integer;
  public
    constructor Create( objSource : TGObjectSource ); override;
    destructor Destroy; override;

    function First : IGMapPoint; override;
    function Next : IGMapPoint; override;
    function Last : IGMapPoint; override;
    function Prior : IGMapPoint; override;
    function Count : integer; override;

    function Contains( objPK : TGPrimaryKey ) : Boolean; virtual;
    procedure AddPrimaryKey( objPK : TGPrimaryKey ); virtual;

    property PrimaryKeyList : TList read FPrimaryKeyList;
  end;


  TGObjectSource = class(TGComponent)
  private
    FLayer : TGLayer;
    FPresenters: TGPresenterStore;
    FName: String;
    FCullObjectSize: integer;
    FMetaData : TStringList;
    FUserXML: TGXML_Element;
    FSaveDataInXML: Boolean;
  protected
    FUniqueID : TGUID;
    function GetColumnNames: String;
    function GetName: String; virtual;
    function GetObjectCount : Cardinal; virtual; abstract;
    function GetPresenters: TGPresenterStore; virtual;
    procedure SetColumnNames(const Value: String);
    procedure SetName(const Value: String); virtual;
    procedure SetUniqueID( Value : TGUID); virtual;
  public
    constructor Create( parentGlobe: TGCustomGlobe5 ); override;
    destructor Destroy; override;

    function Clone : TGObjectSource; virtual;
    procedure CopyFrom( objectSource : TGObjectSource ); virtual;

    procedure EditObject( mapObject : IGMapPoint ); virtual;
    procedure EndEditObject( mapObject : IGMapPoint ); virtual;
    procedure Post; virtual;

    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;

    procedure LoadDataFromXML(Element : TGXML_Element); virtual;
    procedure SaveDataToXML(Element : TGXML_Element); virtual;

    function FirstObject : IGMapPoint; virtual;
    function NextObject( mapObject : IGMapPoint ) : IGMapPoint; virtual;

    function First : TGPrimaryKey; virtual; abstract;
    function Next( objPK : TGPrimaryKey ) : TGPrimaryKey; virtual; abstract;
    function ByPrimaryKey( objPK : TGPrimaryKey ): IGMapPoint; virtual; abstract;

    function NewIterator : TGIterator; overload; virtual;
    function NewIterator( aMER : TGMER ) : TGIterator; overload; virtual; abstract;

    procedure ForEach( const aMER :TGMER; callback : TGObjectCallback); virtual; abstract;

    procedure Clear; virtual;
    procedure Delete; virtual; abstract;

    procedure Add( obj : IGMapPoint ); virtual;
    procedure Remove( obj : IGMapPoint ); virtual; abstract;

    function DataMER : TGMER; virtual;
    function IsValid : Boolean; virtual;
    function IsTemporary : Boolean; virtual; abstract;

    function ImageByName( const Image: TGBitmap; var ImageName : TFilename ) : Boolean; virtual; abstract;
    function ImportImage( const ImageName : String ) : String; virtual; abstract;
    procedure ImportImageStream( const ImageName : String; imageStream : TMemoryStream ); virtual; abstract;
    procedure ExportImageStream( const ImageName : String; imageStream : TStream ); virtual; abstract;
    procedure ImportData( reader : TGCustomDataReader ); virtual;

    property MetaData : TStringList read FMetaData;

    property CullObjectSize : integer read FCullObjectSize write FCullObjectSize;
    property Layer : TGLayer read FLayer write FLayer;
    property Presenters : TGPresenterStore read GetPresenters;
    property SaveDataInXML : Boolean read FSaveDataInXML write FSaveDataInXML;
    property UserXML : TGXML_Element read FUserXML;
{$IFDEF BCB}
    property UniqueID : TGUID read FUniqueID write SetUniqueID;
{$ENDIF}
  published
    property ColumnNames : String read GetColumnNames write SetColumnNames;
    property Name : String read GetName write SetName;
    property ObjectCount : Cardinal read GetObjectCount;
{$IFNDEF BCB}
    property UniqueID : TGUID read FUniqueID write SetUniqueID;
{$ENDIF}
  end;
  TGObjectSourceClass = class of TGObjectSource;

  TGSelection = class(TGRoot)
  public
    function First : IGMapPoint; virtual; abstract;
    function Next : IGMapPoint; virtual; abstract;
    function Last : IGMapPoint; virtual; abstract;
    function Prior : IGMapPoint; virtual; abstract;
    function Count : integer; virtual; abstract;
    function Clear : Boolean; virtual; abstract;
  end;

  TGGlobeSelection = class( TGSelection )
  private
    FParentGlobe : TGCustomGlobe5;
    FLayerStack : TList;
    FPosition : integer;
    FStartLayer : Boolean;
    procedure BuildLayerStack(aLayer: TGLayer);
  public
    constructor Create( parentGlobe: TGCustomGlobe5 );
    destructor Destroy; override;

    function First : IGMapPoint; override;
    function Next : IGMapPoint; override;
    function Last : IGMapPoint; override;
    function Prior : IGMapPoint; override;
    function Count : integer; override;
    function Clear : Boolean; override;
  end;

  TGLayerSelection = class( TGSelection )
  private
    FPosition : integer;
    FParentLayer : TGLayer;
    FBuckets : array of TList;
    FCount : integer;

    function Hash( key : TGPrimaryKey ) : integer;
    function GetKey(index: integer): TGPrimaryKey;
  public
    constructor Create( parentLayer : TGLayer );
    destructor Destroy; override;

    procedure Add( key : TGPrimaryKey );
    function Contains( key : TGPrimaryKey ) : Boolean;
    procedure Remove( key : TGPrimaryKey );

    function First : IGMapPoint; override;
    function Next : IGMapPoint; override;
    function Last : IGMapPoint; override;
    function Prior : IGMapPoint; override;
    function Count : integer; override;
    function Clear : Boolean; override;
  end;

  TGLayer = class(TGComponent)
  private
    FParentLayer : TGLayer;
    FLayerStore : TGLayerStore;

    FsLayerName : String;
    FPresenters : TGPresenterStore;
    FPresenterCache : TList;
    FCurrentPresenter : TGPresenter;

    FSelection: TGLayerSelection;

    FbEnabled : Boolean;
    FbSelectable : Boolean;
    FbShowTitles : Boolean;
    FbVisible : Boolean;
    FbNoInterrupt : Boolean;

    FUpdateCounter : integer;
    FRenderObjectCount : integer;
    FRenderPass : integer;
    FZoomMin : Cardinal;
    FZoomMax : Cardinal;
    FMinObjectSize: integer;

    FZoomUnits : TGUnitTypes;
    FModified: Boolean;
    FObjectSource: TGObjectSource;

    FTitleColumn: integer;

    FSurfaceBitmap : TGBitmap;
    FSurfaceImageName: TFilename;
    FSurfaceAlpha: Byte;
    FSurfaceTransparentColor: TColor;
    FReRenderLayer: Boolean;
    FUserXML: TGXML_Element;
    FRenderAllObjects: Boolean;
    FAlphaBlend: Byte;

    function GetLayerID: TGUID;
    procedure SetSurfaceImageName(const Value: TFilename);
    function GetTitleColumnName: String;
    procedure SetTitleColumnName(const Value: String);
    procedure SetAlphaBlend(const Value: Byte);
  protected
    function GetIndex : integer;
    function GetLayerName : String;
    function GetObjectSource: TGObjectSource;
    function GetProjection : TGProjector;

    procedure SetIndex(iIndex : integer);
    procedure SetLayerName(const Value: String);
    procedure SetLayerBoolean(iIndex : Integer; bValue : Boolean);
    procedure SetObjectSource(const Value: TGObjectSource);
    procedure SetZoomUnits(Value : TGUnitTypes);
    procedure SetTitleColumn(const Value: integer);
    procedure SetZoomMax(const Value: Cardinal);
    procedure SetZoomMin(const Value: Cardinal);

    function IsRenderAborted : Boolean;
    function PeriodicUpdate : Boolean;
    function MaxTolerance : integer;
    procedure RenderObject(objPK : TGPrimaryKey; var bAbort : Boolean );
  public
    constructor Create(ParentGlobe : TGCustomGlobe5); override;
    destructor Destroy; override;

    procedure Clear;
    procedure Delete;
    procedure Post;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Assign(Source: TPersistent); override;
    function Clone: TGLayer;

    function ClearSelected : Boolean;
    function SelectObjects( aMER : TGMER; includeSubLayers, firstOnly, state : Boolean ) : integer;

    function FindPresenter(iPresenterID : integer) : TGPresenter;
    function IsShowing : Boolean;
    function IsValid : Boolean;
    function LayerMER : TGMER;
    function LayerDepth : integer;
    function AdjustedAlphaBlend : Byte;

    function ObjectAtLL(ptLL : TGPointLL) : IGMapPoint;
    function ObjectAtXY(iX, iY : Integer) : IGMapPoint;
    function ObjectByTitle(const sTitle : String ) : IGMapPoint;
    function ObjectByUserID(const sUserID : String ) : IGMapPoint;

    procedure Render( firstPass : Boolean; const viewMER : TGMER ); virtual;

    function TitleColumnToIndex( const columnName : String ) : integer;
    function TitleColumnToName( index : integer ) : String;
    function LoadImage( image : TGBitmap; var imageName : TFilename ) : Boolean;
    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;

    property Modified : Boolean read FModified write FModified;
    property ParentLayer : TGLayer read FParentLayer;
    property Presenters : TGPresenterStore read FPresenters;
    property Projector : TGProjector read GetProjection;
    property RenderObjectCount : integer read FRenderObjectCount;
    property RenderPass : integer read FRenderPass;
    property LayerStore : TGLayerStore read FLayerStore;
    property LayerID : TGUID read GetLayerID;
    property ReRenderLayer : Boolean read FReRenderLayer write FReRenderLayer;
    property Selection : TGLayerSelection read FSelection write FSelection;
    property UserXML : TGXML_Element read FUserXML;
  published
    property AlphaBlend : Byte read FAlphaBlend write SetAlphaBlend;
    property Enabled : Boolean index 5 read FbEnabled write SetLayerBoolean;
    property Index : integer read GetIndex write SetIndex;
    property Name : String read GetLayerName write SetLayerName;
    property NoInterrupt : Boolean index 4 read FbNoInterrupt write SetLayerBoolean;
    property ObjectSource : TGObjectSource read GetObjectSource write SetObjectSource;
    property RenderAllObjects : Boolean read FRenderAllObjects write FRenderAllObjects;
    property Selectable : Boolean index 0 read FbSelectable write SetLayerBoolean;
    property ShowTitles : Boolean index 3 read FbShowTitles write SetLayerBoolean;
    property TitleColumn : integer read FTitleColumn write SetTitleColumn;
    property TitleColumnName : String read GetTitleColumnName write SetTitleColumnName;
    property Visible : Boolean index 1 read FbVisible write SetLayerBoolean;
    property ZoomMax : Cardinal read FZoomMax write SetZoomMax;
    property ZoomMin : Cardinal read FZoomMin write SetZoomMin;
    property ZoomUnits : TGUnitTypes read FZoomUnits write SetZoomUnits;
    property SurfaceImageName : TFilename read FSurfaceImageName write SetSurfaceImageName;
    property SurfaceAlpha : Byte read FSurfaceAlpha write FSurfaceAlpha;
    property SurfaceTransparentColor : TColor read FSurfaceTransparentColor write FSurfaceTransparentColor;
  end;
  TGLayerClass = class of TGLayer;



  TGCustomProjectionModel = class(TGComponent)
  public
    function PointStoreToXY(Points : TGPointStore; iStart : integer) : Integer; virtual; abstract;
    function PointLLToXY(const ptLL : TGPointLL; iIndex : Integer) : Boolean; virtual; abstract;

    function LLToXY(iLong, iLat, iIndex : Integer) : Boolean; virtual; abstract;
    function LLHToXY(iLong, iLat, iHeight, iIndex : Integer) : Boolean; virtual; abstract;
    function XYToLL(iX, iY, iIndex : Integer) : Boolean; virtual; abstract;
    function ExtentsLL : TRect; virtual; abstract;
    procedure GetPOZ(var POZ : TGPOZ); virtual; abstract;
    procedure SetPOZ(const POZ : TGPOZ); virtual; abstract;

    function VisibleMER(const MER : TGMER; var State : TGRenderStateSet) : Boolean; virtual; abstract;

    procedure PropertyChanged(ProjectionProperty : TGProjectionProperty); virtual; abstract;

    procedure PaintSurfaceImage( image : TGBitmap ); virtual; abstract;
    procedure PaintSurface; virtual; abstract;
    function Clone(projector : TGProjector) : TGCustomProjectionModel; virtual;
    procedure Assign(AModel : TPersistent); override;

    function IsContinuous : Boolean; virtual; abstract;
  end;
  TGCustomProjectionModelClass = class of TGCustomProjectionModel;


  
  TGProjector = class(TGComponent)
  private
    FiXOrigin : Integer;
    FiYOrigin : Integer;
    FiAltitude : Cardinal;
    FXRotation : TGDecimalDegree;
    FYRotation : TGDecimalDegree;
    FZRotation : TGDecimalDegree;

    FMaxLatitude : TGDecimalDegree;
    FMinLatitude : TGDecimalDegree;
    FMaxLongitude : TGDecimalDegree;
    FMinLongitude : TGDecimalDegree;

    FMaxZoom : TGDecimalDegree;
    FMinZoom : TGDecimalDegree;

    FScaleFactor : Extended;

    FCentralMeridian : TGDecimalDegree;
    FCentralParallel : TGDecimalDegree;
    FFirstParallel : TGDecimalDegree;
    FSecondParallel : TGDecimalDegree;

    FSpheroid : TGSpheroid;
    FXAxisChanges : TGXAxisChanges;
    FYAxisChanges : TGYAxisChanges;
    FProjectionModel : TGCustomProjectionModel;

    FUseMinMaxLimits: Boolean;
    FFieldOfViewAngle: TGDecimalDegree;
    function GetProjectionClass : String;
    procedure SetProjectionClass(projectionClassName : String);

    function GetGlobeCenterXY : TPoint;
    procedure SetGlobeCenterXY(PT : TPoint);
    function GetUnitsPerInch : Integer;
    procedure SetUnitsPerInch(UPI : Integer);
    function GetUnitsPerPixel: Integer;
    procedure SetUnitsPerPixel(const Value: Integer);
  protected
    procedure SetIntegerProp(ProjectionProperty : integer; Value : Integer);
    procedure SetFloatProp(ProjectionProperty : integer; Value : TGDecimalDegree);
    procedure SetProjectionModel(Value : TGCustomProjectionModel);
    procedure SetUseMinMaxLimits(const Value: Boolean);
    procedure SetXAxisChanges(Value : TGXAxisChanges);
    procedure SetYAxisChanges(Value : TGYAxisChanges);
    procedure SetScaleFactor(eSF : Extended);
    procedure SetCenterLat(Value : TGDecimalDegree);
    procedure SetCenterLong(Value : TGDecimalDegree);
    function GetCenterLat : TGDecimalDegree;
    function GetCenterLong : TGDecimalDegree;
  public
    constructor Create(ParentGlobe : TGCustomGlobe5); override;
    destructor Destroy; override;

    procedure SaveToXML(Element : TGXML_Element); override;
    procedure LoadFromXML(Element : TGXML_Element); override;

    procedure Assign(Source : TPersistent); override;
    function Clone : TGProjector; virtual;

    function DeviceXYToLL(iX, iY : Integer; var ptLL : TGPointLL) : Boolean;
    function DeviceXYToDD(iX, iY : Integer; var ptDD : TGPointDD) : Boolean;
    function LLToDeviceXY(iLong, iLat : Integer; var ptXY : TPoint) : Boolean;
    function DDToDeviceXY(Long, Lat : TGDecimalDegree; var ptXY : TPoint) : Boolean;
    function PointLLToXY(const ptLL : TGPointLL; var ptXY : TPoint) : Boolean;
    function TriangleLLToXY( worldTri : TGTriangleLL; var screenTri : TGTriangle ) : Boolean;
    function IsContinuous : Boolean;
    function ExtentsLL : TRect;
    function PixelTolerance : integer;

    procedure AdjustScaleFactor( ptXY : TPoint; delta : Double );

    property ProjectionModel : TGCustomProjectionModel read FProjectionModel write SetProjectionModel;
    property GUPerInch : Integer read GetUnitsPerInch write SetUnitsPerInch;
    property GUPerPixel : Integer read GetUnitsPerPixel write SetUnitsPerPixel;
    property CenterXY : TPoint read GetGlobeCenterXY write SetGlobeCenterXY;
    property Altitude : Cardinal read FiAltitude;
  published
    property CentralMeridian : TGDecimalDegree index Ord(ppCentralMeridian)read FCentralMeridian write SetFloatProp;
    property CentralParallel : TGDecimalDegree index Ord(ppCentralParallel)read FCentralParallel write SetFloatProp;
    property FirstParallel : TGDecimalDegree index Ord(ppFirstParallel)read FFirstParallel write SetFloatProp;
    property SecondParallel : TGDecimalDegree index Ord(ppSecondParallel)read FSecondParallel write SetFloatProp;

    property CenterLatitude : TGDecimalDegree read GetCenterLat write SetCenterLat;
    property CenterLongitude : TGDecimalDegree read GetCenterLong write SetCenterLong;
    property FieldOfViewAngle : TGDecimalDegree read FFieldOfViewAngle write FFieldOfViewAngle;
    property MaxLatitude : TGDecimalDegree index Ord(ppMaxLatitude)read FMaxLatitude write SetFloatProp;
    property MaxLongitude : TGDecimalDegree index Ord(ppMaxLongitude)read FMaxLongitude write SetFloatProp;
    property MaxZoom : TGDecimalDegree index Ord(ppMaxZoom)read FMaxZoom write SetFloatProp;
    property MinLatitude : TGDecimalDegree index Ord(ppMinLatitude)read FMinLatitude write SetFloatProp;
    property MinLongitude : TGDecimalDegree index Ord(ppMinLongitude)read FMinLongitude write SetFloatProp;
    property MinZoom : TGDecimalDegree index Ord(ppMinZoom)read FMinZoom write SetFloatProp;
    property ProjectionClass : TGProjectionClass read GetProjectionClass write SetProjectionClass;
    property ScaleFactor : Extended read FScaleFactor write SetScaleFactor;
    property Spheroid : TGSpheroid read FSpheroid write FSpheroid default WGS84;
    property UseMinMaxLimits : Boolean read FUseMinMaxLimits write SetUseMinMaxLimits;
    property XRotation : TGDecimalDegree index Ord(ppXRotation)read FXRotation write SetFloatProp;
    property YRotation : TGDecimalDegree index Ord(ppYRotation)read FYRotation write SetFloatProp;
    property ZRotation : TGDecimalDegree index Ord(ppZRotation)read FZRotation write SetFloatProp;
    property XOrigin : Integer index Ord(ppXOrigin)read FiXOrigin write SetIntegerProp;
    property YOrigin : Integer index Ord(ppYOrigin)read FiYOrigin write SetIntegerProp;
    property XAxisChanges : TGXAxisChanges read FXAxisChanges write SetXAxisChanges;
    property YAxisChanges : TGYAxisChanges read FYAxisChanges write SetYAxisChanges;
  end;
  TGlobeProjectionClass = class of TGProjector;


  TGRenderer = class(TGComponent)
  private
    FHeight : integer;
    FWidth : integer;
    FAltitude : Cardinal;
    FPixelsPerInch : integer;
    FDoubleBuffer: Boolean;
    FMinTextHeight: Integer;
    FMaxTextHeight: Integer;

    CpStart : TPointData;
    CpStartCode: integer;
    CpEnd : TPointData;
    CpEndCode : integer;

    FState : TCanvasState;
    FBrush: TGBrush;
    FFont: TGFont;
    FPen: TGPen;
    FSelectedBrush: TGBrush;
    FSelectedPen: TGPen;
    FAlphaBlend: Byte;

    procedure SetHeight( Value : integer );
    procedure SetWidth( Value : integer );

    function ClipCode( const pt : TPoint ) : integer;
    function StartClip: Boolean;
    function EndClip( const pt : TPoint ) : integer;
    function GetAltitude: Cardinal;

    procedure SetPen(const Value: TGPen);
    procedure SetBrush(const Value: TGBrush);
    procedure SetFont(const Value: TGFont);
    procedure BrushChanged(Sender : TObject);
    procedure FontChanged(Sender : TObject);
    procedure PenChanged(Sender : TObject);
    procedure SetSelectedBrush(const Value: TGBrush);
    procedure SetSelectedPen(const Value: TGPen);
  protected
    CpRegion : array [0..3] of TPoint;
    FPoints : TPointArray;
    FClippedPoints : TPointArray;
    FPolyPolyPoints : array of TPointArray;
    FDrawPolyClosed: Boolean;

    function ClipPoly(iCount : Integer; closed : Boolean; iPenWidth : integer) : Integer;
    procedure DrawPolyPoly(const Points : array of TPointArray; closed: Boolean); virtual; abstract;
    function GetCanvas: TCanvas; virtual; abstract;
    procedure RendererState(ReqState: TCanvasState);
    procedure SetDoubleBuffer(const Value: Boolean); virtual;
    procedure UpdateClipRect( offset : integer ); virtual;
  public
    constructor Create(ParentGlobe : TGCustomGlobe5); override;
    destructor Destroy; override;

    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;

    procedure Assign(Source : TPersistent); override;
    function Clone : TGRenderer;
    procedure Clear; virtual;
    procedure RefreshDC; virtual;
    procedure RealizeAttributes; virtual;

    function FontRenderSize(FontSize : integer; FontUnits : TGUnitTypes) : integer;
    function XYListVisible(Points : TPointArray; iCount : Integer) : Boolean;
    function TriangleLLVisible(const ptLL1, ptLL2, ptLL3 : TGPointLL): Boolean;
    procedure ColorBlend( var target: TGColor; source: TGColor; alpha: Byte);

    procedure BeginDrawPoly(closed: Boolean); virtual;
    procedure AddClippedPoly(iPoints : integer; iPenWidth : integer); virtual;
    procedure EndDrawPoly; virtual;

    procedure DrawEllipse( rectangle : TRect ); virtual;
    procedure DrawFocusRect( rectangle : TRect ); virtual;
    procedure DrawRectangle( rectangle : TRect ); virtual;
    procedure DrawPixel( X, Y : integer; color: TColor; alpha : Byte ); virtual;
    procedure DrawPoly( const points : array of TPoint; closed : Boolean ); virtual;
    procedure DrawScanLine(Y, x1, x2 : integer; u1, u2, v1, v2: Double; const image: TGBitmap); virtual;
    procedure DrawTextRect( const aRect : TRect; const text : String ); virtual;
    procedure DrawText( X, Y : integer; const text : String; alignment : TGTitleAlignment = taBottomRight; offset : integer = 0 ); virtual;
    procedure DrawTextureTriangle( const srcTri, destTri: TGTriangle; const image : TGBitmap ); virtual;

    function GetTextRect(X, Y : integer; const text : String; alignment : TGTitleAlignment = taBottomRight; offset : integer = 0 ): TRect; virtual;
    function GetMaxTextSize: Integer;
    function GetMinTextSize: Integer;

    procedure SetSize( newWidth, newHeight : integer ); virtual;

    property AlphaBlend : Byte read FAlphaBlend write FAlphaBlend;
    property Altitude : Cardinal read GetAltitude;
    property Canvas : TCanvas read GetCanvas;
    property Height : integer read FHeight write SetHeight;
    property Width : integer read FWidth write SetWidth;
    property PixelsPerInch : integer read FPixelsPerInch write FPixelsPerInch;
    property Points : TPointArray read FPoints;
    property Brush : TGBrush read FBrush write SetBrush;
    property Font : TGFont read FFont write SetFont;
    property Pen : TGPen read FPen write SetPen;
  published
    property DoubleBuffer : Boolean read FDoubleBuffer write SetDoubleBuffer;
    property MaxTextHeight : Integer read FMaxTextHeight write FMaxTextHeight;
    property MinTextHeight : Integer read FMinTextHeight write FMinTextHeight;
    property SelectedBrush : TGBrush read FSelectedBrush write SetSelectedBrush;
    property SelectedPen : TGPen read FSelectedPen write SetSelectedPen;
  end;
  TGRendererClass = class of TGRenderer;

  TGCustomDataReader = class(TGComponent)
  private
    FObjectSource: TGObjectSource;
    FName: String;
    FOnProgress: TGProgressEvent;
    FPresenters: TGPresenterStore;
    FMetaData: TStringList;
  protected
    function ReadHeader : Boolean; virtual; abstract;
    function ReadTrailer : Boolean; virtual; abstract;
    function ReadObject : IGMapPoint; virtual; abstract;

    function InternalClose : Boolean; virtual; abstract;
    function InternalOpen : Boolean; virtual; abstract;
  public
    function ProgressPercent : Double; virtual; abstract;

    function First : IGMapPoint; virtual; abstract;
    function Next : IGMapPoint; virtual; abstract;

    property MetaData : TStringList read FMetaData write FMetaData;
    property ObjectSource : TGObjectSource read FObjectSource write FObjectSource;
    property Presenters : TGPresenterStore read FPresenters write FPresenters;
  published
    property Name : String read FName write FName;
    property OnProgress : TGProgressEvent read FOnProgress write FOnProgress;
  end;

  TGCustomGlobe5 = class(TCustomControl)
  protected
    FProjector : TGProjector;
    FRenderer : TGRenderer;
    FViewRect : TRect;

    procedure SetProjector(const Value: TGProjector); virtual; abstract;
    procedure SetRenderer(const Value: TGRenderer); virtual; abstract;
    procedure SetViewRect(const sRect : TRect); virtual; abstract;
  public
    property ViewRect : TRect read FViewRect write SetViewRect;
  published
    property Projector : TGProjector read FProjector write SetProjector;
    property Renderer : TGRenderer read FRenderer write SetRenderer;
  end;

implementation

{$R GBRUSHES.RES}  // Include the Brush bitmap resource

uses GSDBCache, GResource, GProjections, GMapObjects, GPresenters, Globe5Utils,
  GSDBTables, GLogging, GMemObjectSource, Globe5;

const // used by the polygon clipper
  TG_TWOBITS = $0100;
  TG_NOSEGM = 0;
  TG_SEGM = 1;
  TG_CLIP = 2;
  CpTcc : array[0..15] of integer = ( 0,-3,-6, 1, 3, 0, 1, 0, 6, 1, 0, 0, 1, 0, 0, 0);
  CpCra : array[0..15] of integer = (-1,-1,-1, 3,-1,-1, 2,-1,-1, 1,-1,-1, 0,-1,-1,-1);

var
  AttributeHashValue : Cardinal = 0;

function TGAttribute.AttributeHandle( renderer : TGRenderer ): Cardinal;
begin
  if ( FAttributeHandle = 0 ) or ( renderer <> FRenderer ) then
    CreateHandle( renderer );

  Result := FAttributeHandle;
end;

constructor TGAttribute.Create;
begin
  // Does nothing.
end;

destructor TGAttribute.Destroy;
begin
  DestroyHandle( FRenderer );
  inherited;
end;

procedure TGAttribute.DestroyHandle( renderer : TGRenderer );
begin
  if FAttributeHandle <> 0 then
  begin
    DeleteObject(FAttributeHandle);
    FAttributeHandle := 0;
  end;
end;

procedure TGAttribute.DoChange;
begin
  if Assigned( FOnChange ) then
    FOnChange( Self );
end;


procedure TGAttribute.LoadFromXML(Element : TGXML_Element);
begin
  // Does nothing
end;

procedure TGAttribute.SaveToXML(Element : TGXML_Element);
begin
  // Does nothing
end;

procedure TGAttribute.SetModified(const Value: Boolean);
begin
  FModified := Value;

  if Value then
  begin
    Inc( AttributeHashValue );
    FHashValue := AttributeHashValue;

    DestroyHandle( FRenderer );
    DoChange;
  end;
end;

procedure TGProjector.AdjustScaleFactor(ptXY: TPoint; delta: Double);
var
  eSF, OrgX, OrgY : Extended;
begin
  if TGlobe5(ParentGlobe).PeriodicUpdateTime > 0 then
    TGlobe5(ParentGlobe).RestartRender;

  eSF := ScaleFactor * delta;

  // Limit the zoom range to avoid integer overflow
  if eSF > TG_MAXSCALEFACTOR then
    eSF := TG_MAXSCALEFACTOR;
  if eSF < TG_MINSCALEFACTOR then
    eSF := TG_MINSCALEFACTOR;
  if MaxVal(ParentGlobe.Renderer.Width, ParentGlobe.Renderer.Height) / eSF > Maxint then
    eSF := MaxVal(ParentGlobe.Renderer.Width, ParentGlobe.Renderer.Height) / Maxint;

  if delta <> 0.0 then
  try
    OrgX := (XOrigin - ptXY.X) / ScaleFactor;
    XOrigin := Round( ptXY.X + OrgX * eSF);

    OrgY := (YOrigin - ptXY.Y) / ScaleFactor;
    YOrigin := Round(ptXY.Y + OrgY * eSF);

    FiAltitude := Round((ParentGlobe.Renderer.Width / eSF) / Tan((FFieldOfViewAngle) * DD_TORADIANS));

    ParentGlobe.FViewRect := Rect(0, 0, Round(ParentGlobe.Renderer.Width / eSF), Round(ParentGlobe.Renderer.Height / eSF));
    FScaleFactor := eSF;

    ProjectionModel.PropertyChanged(ppScaleFactor);

    TGlobe5(ParentGlobe).RedrawLayers;
    TGlobe5(ParentGlobe).DoOnZoomed;
    TGlobe5(ParentGlobe).UpdateScrollBars;
  finally
    // swallow exception
  end;
end;

procedure TGProjector.Assign(Source : TPersistent);
var
  Prj : TGProjector;
begin
  if Source is TGProjector then
  begin
    Prj := TGProjector(Source);

    FiXOrigin := Prj.XOrigin;
    FiYOrigin := Prj.YOrigin;
    FXRotation := Prj.XRotation;
    FYRotation := Prj.YRotation;
    FZRotation := Prj.ZRotation;

    FMaxLatitude := Prj.MaxLatitude;
    FMinLatitude := Prj.MinLatitude;
    FMaxLongitude := Prj.MaxLongitude;
    FMinLongitude := Prj.MinLongitude;

    FMaxZoom := Prj.MaxZoom;
    FMinZoom := Prj.MinZoom;

    FiAltitude := Prj.Altitude;
    FScaleFactor := Prj.ScaleFactor;
    FFieldOfViewAngle := Prj.FieldOfViewAngle;

    FCentralMeridian := Prj.CentralMeridian;
    FCentralParallel := Prj.CentralParallel;
    FFirstParallel := Prj.FirstParallel;
    FSecondParallel := Prj.SecondParallel;

    FSpheroid := Prj.Spheroid;
    FXAxisChanges := Prj.XAxisChanges;
    FYAxisChanges := Prj.YAxisChanges;

//    FRenderBackFace := Prj.RenderBackFace;

    FProjectionModel.Free;
    FProjectionModel := Prj.ProjectionModel.Clone(Self);
  end
  else
    inherited Assign(Source);
end;

function TGProjector.Clone : TGProjector;
begin
  Result := TGlobeProjectionClass(Self.ClassType).Create(ParentGlobe);
  Result.Assign(Self);
end;

constructor TGProjector.Create(ParentGlobe : TGCustomGlobe5);
begin
  inherited;

  Assert(ParentGlobe <> nil, 'TGProjector.Create passed nil Globe parameter' );

  FProjectionModel := nil;

  FYAxisChanges := yaCentralMeridian;
  FXAxisChanges := xaYOrigin;
  FSpheroid := WGS84;

  FMinLongitude := -180.0;
  FMinLatitude := -90.0;
  FMaxLongitude := 180.0;
  FMaxLatitude := 90.0;

  FXRotation := 23.5;
  FYRotation := 0;
  FZRotation := 0;

  FCentralMeridian := 0;
  FFirstParallel := 0;
  FSecondParallel := 0;
  FFieldOfViewAngle := TG_FOV_ANGLE;

  FiXOrigin := ParentGlobe.Renderer.Width div 2;
  FiYOrigin := ParentGlobe.Renderer.Height div 2;
  FScaleFactor := minFloat(ParentGlobe.Renderer.Width / GU_180_DEGREE, ParentGlobe.Renderer.Height / GU_180_DEGREE);

  if ProjectionModel <> nil then
    with ExtentsLL do
    begin
      FScaleFactor := minFloat(ParentGlobe.Renderer.Width / Abs(Right - Left), ParentGlobe.Renderer.Height / Abs(Bottom - Top));
      if FScaleFactor > TG_MAXSCALEFACTOR then
        FScaleFactor := TG_MAXSCALEFACTOR;
      if FScaleFactor < TG_EPSILON then
        FScaleFactor := TG_EPSILON;

      FiXOrigin := ParentGlobe.Renderer.Width div 2;
      FiYOrigin := ParentGlobe.Renderer.Height div 2;
    end;
end;

destructor TGProjector.Destroy;
begin
  FreeAndNil( FProjectionModel );

  inherited Destroy;
end;

function TGProjector.DeviceXYToDD(iX, iY : Integer; var ptDD : TGPointDD) : Boolean;
begin
  Result := ProjectionModel.XYToLL(iX, iY, 0);

  if Result then
  begin
    ptDD.LongX := GUToDD(ParentGlobe.Renderer.Points[0].X);
    ptDD.LatY := GUToDD(ParentGlobe.Renderer.Points[0].Y);
  end
  else
  begin
    ptDD.LongX := GUToDD(GU_180_DEGREE);
    ptDD.LatY := GUToDD(GU_180_DEGREE);
  end;

  ptDD.HeightZ := 0;
end;

function TGProjector.DeviceXYToLL(iX, iY : Integer; var ptLL : TGPointLL) : Boolean;
begin
  Result := ProjectionModel.XYToLL(iX, iY, 0);
  if Result then
    ptLL := PointLL(ParentGlobe.Renderer.Points[0].X, ParentGlobe.Renderer.Points[0].Y)
  else
    ptLL := PointLL(GU_180_DEGREE, GU_180_DEGREE);
end;

function TGProjector.GetProjectionClass : String;
begin
  Result := ProjectionModel.ClassName;
end;

procedure TGProjector.LoadFromXML(Element : TGXML_Element);
begin
  if Element <> nil then
    with Element do
    begin
      ProjectionModel := TGCustomProjectionModel( CreateFromXML(ParentGlobe, Element('ProjectionModel', 0), TGSphericalPrj));

      Spheroid := TGSpheroid(Attribute('Spheroid', Ord(WGS84)));
      YAxisChanges := TGYAxisChanges(Attribute('YAxisChanges', Ord(yaCentralMeridian)));
      XAxisChanges := TGXAxisChanges(Attribute('XAxisChanges', Ord(xaYOrigin)));
      ScaleFactor := Attribute('ScaleFactor', 0.0);

      MaxLatitude := Attribute('MaxLatitude', 90.0);
      MaxLongitude := Attribute('MaxLongitude', 180.0);
      MaxZoom := Attribute('MaxZoom', 0.0);
      MinLatitude := Attribute('MinLatitude', -90.0);
      MinLongitude := Attribute('MinLongitude', -180.0);
      MinZoom := Attribute('MinZoom', 0.0);
      UseMinMaxLimits := Attribute('UseMinMax', false);
      FieldOfViewAngle := Attribute('FOV', TG_FOV_ANGLE);

      XRotation := Attribute('XRotation', 0.0);
      YRotation := Attribute('YRotation', 0.0);
      ZRotation := Attribute('ZRotation', 0.0);
      XOrigin := Attribute('XOrigin', 0);
      YOrigin := Attribute('YOrigin', 0);

      CentralMeridian := Attribute('CentralMeridian', 0.0);
      CentralParallel := Attribute('CentralParallel', 0.0);
      FirstParallel := Attribute('FirstParallel', 0.0);
      SecondParallel := Attribute('SecondParallel', 0.0);
    end;
end;

procedure TGProjector.SaveToXML(Element : TGXML_Element);
begin
  inherited;

  Element.AddAttribute('ScaleFactor', ScaleFactor, 0.0);

  Element.AddAttribute('CentralMeridian', CentralMeridian, 0.0);
  Element.AddAttribute('CentralParallel', CentralParallel, 0.0);
  Element.AddAttribute('FirstParallel', FirstParallel, 0.0);
  Element.AddAttribute('SecondParallel', SecondParallel, 0.0);

  Element.AddAttribute('MaxLatitude', MaxLatitude, 90.0);
  Element.AddAttribute('MaxLongitude', MaxLongitude, 180.0);
  Element.AddAttribute('MaxZoom', MaxZoom, 0.0);
  Element.AddAttribute('MinLatitude', MinLatitude, -90.0 );
  Element.AddAttribute('MinLongitude', MinLongitude, -180.0);
  Element.AddAttribute('MinZoom', MinZoom, 0.0);
  Element.AddAttribute('UseMinMax', UseMinMaxLimits, false);
  Element.AddAttribute('FOV', FieldOfViewAngle, TG_FOV_ANGLE);

  Element.AddAttribute('Spheroid', Ord(Spheroid), Ord(WGS84));
  Element.AddAttribute('XRotation', XRotation, 0.0);
  Element.AddAttribute('YRotation', YRotation, 0.0);
  Element.AddAttribute('ZRotation', ZRotation, 0.0);
  Element.AddAttribute('XOrigin', XOrigin, 0);
  Element.AddAttribute('YOrigin', YOrigin, 0);
  Element.AddAttribute('XAxisChanges', Ord(XAxisChanges), Ord(xaYOrigin));
  Element.AddAttribute('YAxisChanges', Ord(YAxisChanges), Ord(yaCentralMeridian));

  ProjectionModel.SaveToXML( Element.AddElement('ProjectionModel'));
end;

procedure TGProjector.SetFloatProp(ProjectionProperty : integer; Value : TGDecimalDegree);
begin
  case TGProjectionProperty(ProjectionProperty) of
    ppXRotation :
      if Value <> XRotation then
      begin
        FXRotation := Value;
        TGlobe5(ParentGlobe).DoOnRotated;
      end
      else
        Exit;
    ppYRotation :
      if Value <> YRotation then
      begin
        FYRotation := Mod180Float(Value);
        TGlobe5(ParentGlobe).DoOnRotated;
      end
      else
        Exit;
    ppZRotation :
      if Value <> ZRotation then
      begin
        FZRotation := Value;
        TGlobe5(ParentGlobe).DoOnRotated;
      end
      else
        Exit;
    ppMaxLatitude :
      if Value <> MaxLatitude then
        FMaxLatitude := MinFloat( Value, 90.0 )
      else
        Exit;
    ppMinLatitude :
      if Value <> MinLatitude then
        FMinLatitude := MaxFloat( Value, -90.0 )
      else
        Exit;
    ppMaxLongitude :
      if Value <> MaxLongitude then
        FMaxLongitude := MinFloat( Value, 180.0 )
      else
        Exit;
    ppMinLongitude :
      if Value <> MinLongitude then
        FMinLongitude := MaxFloat( Value, -180.0 )
      else
        Exit;
    ppMaxZoom :
      if Value <> MaxZoom then
        FMaxZoom := MinFloat( Value, TG_MAXSCALEFACTOR )
      else
        Exit;
    ppMinZoom :
      if Value <> MinZoom then
        FMinZoom := MaxFloat( Value, TG_MINSCALEFACTOR )
      else
        Exit;
    ppCentralMeridian :
      if Value <> CentralMeridian then
        FCentralMeridian := Value
      else
        Exit;
    ppCentralParallel :
      if Value <> CentralParallel then
        FCentralParallel := Value
      else
        Exit;
    ppFirstParallel :
      if Value <> FirstParallel then
        FFirstParallel := Value
      else
        Exit;
    ppSecondParallel :
      if Value <> SecondParallel then
        FSecondParallel := Value
      else
        Exit;
  end;

  ProjectionModel.PropertyChanged(TGProjectionProperty(ProjectionProperty));
  TGlobe5(ParentGlobe).UpdateScrollBars;
  TGlobe5(ParentGlobe).RedrawLayers;
end;

procedure TGProjector.SetIntegerProp(ProjectionProperty : integer; Value : Integer);
begin
  case TGProjectionProperty(ProjectionProperty) of
    ppXOrigin :
      if Value <> XOrigin then
      begin
        FiXOrigin := Value;
        TGlobe5(ParentGlobe).DoOnPanned;
      end
      else
        Exit;
    ppYOrigin :
      if Value <> YOrigin then
      begin
        FiYOrigin := Value;
        TGlobe5(ParentGlobe).DoOnPanned;
      end
      else
        Exit;
  end;

  ProjectionModel.PropertyChanged(TGProjectionProperty(ProjectionProperty));
  TGlobe5(ParentGlobe).RedrawLayers;
end;

procedure TGProjector.SetProjectionModel(Value : TGCustomProjectionModel);
var
  bValid : Boolean;
  iProp : TGProjectionProperty;
  iLong, iLat : integer;
begin
  bValid := False;

  { locate the center of the screen in LL }
  if ProjectionModel <> nil then
    if ProjectionModel.ClassType <> Value.ClassType then
      bValid := ProjectionModel.XYToLL(ParentGlobe.ClientWidth div 2, ParentGlobe.ClientHeight div 2, 0);

  if bValid then
  begin
    iLong := ParentGlobe.Renderer.Points[0].X;
    iLat := ParentGlobe.Renderer.Points[0].Y;
  end
  else
  begin
    iLong := 0;
    iLat := 0;
  end;

  FreeAndNil( FProjectionModel );
  if Value = nil then
    Value := TGSphericalPrj.Create(ParentGlobe);

  FProjectionModel := Value;

  CentralMeridian := 0;
  // Update all of the Projections properties
  for iProp := Low(TGProjectionProperty) to High(TGProjectionProperty) do
    Value.PropertyChanged(iProp);

  { adjust the new Projector to display the same map }
  if bValid then
    with TGlobe5(ParentGlobe) do
    begin
      SetViewRect(ViewRect);
      Projector.CenterXY := Point(ClientWidth div 2, ClientHeight div 2);
      LocateToLL(iLong, iLat);
    end;

  TGlobe5(ParentGlobe).RedrawLayers;
end;

procedure TGProjector.SetScaleFactor(eSF : Extended);
var
  iCanvasWidth, iCanvasHeight : integer;
begin
  if ( eSF < 0.0 ) or ( abs( eSF - ScaleFactor ) < TG_EPSILON ) then
    Exit;

  // check that the scalefactor is within limits.
  if FMaxZoom > FMinZoom then
  begin
    if eSF < FMinZoom then
      eSF := FMinZoom;
    if eSF > FMaxZoom then
      eSF := FMaxZoom;
  end;

  iCanvasWidth := ParentGlobe.Renderer.Width;
  iCanvasHeight := ParentGlobe.Renderer.Height;

  //  if ( eSF <> 0 ) and ( csLoading in ComponentState ) then
  //  begin
  //     FAltitude := Round(( GlobeCanvas.CanvasWidth / eSF ) / Tan(FOV_ANGLE * GU_TORADIANS ));
  //     ProjectionModel.ScaleFactor := eSF;
  //     Redraw;
  //     Exit;
  //  end;

  if eSF = 0.0 then // Zoom to the world extents
    with ProjectionModel do
    begin
      XOrigin := Round( iCanvasWidth / 2 );
      YOrigin := Round( iCanvasHeight / 2 );
      with ExtentsLL do
        eSF := minFloat(iCanvasWidth / Abs(Right - Left), iCanvasHeight / Abs(Bottom - Top));
      if eSF = 0.0 then
        eSF := 1.0;
    end;

  AdjustScaleFactor( Point(iCanvasWidth div 2, iCanvasHeight div 2),  eSF / ScaleFactor );
end;

procedure TGProjector.SetUseMinMaxLimits(const Value: Boolean);
begin
  FUseMinMaxLimits := Value;

  if Value then
  begin
    YAxisChanges := yaXOrigin;
    XAxisChanges := xaYOrigin;
    CentralMeridian := 0;
    CentralParallel := 0;
    ProjectionClass := 'TGCartesianPrj';
  end;
end;

procedure TGProjector.SetProjectionClass(projectionClassName : String);
var
  prjClass : TGCustomProjectionModelClass;
begin
  if ProjectionModel <> nil then
    if CompareText(ProjectionModel.ClassName, projectionClassName) = 0 then
      Exit;

  // FindClass raises an exception if the classname is not found
  prjClass := TGCustomProjectionModelClass(FindClass(projectionClassName));

  if prjClass.InheritsFrom(TGCustomProjectionModel) then
    ProjectionModel := prjClass.Create(ParentGlobe)
  else
    raise EGException.CreateFmt(rsEBadProjectionClassMsg, [projectionClassName]);

  // Check if UseMinMaxLimits set - only works in the Cartesian Projector
  if UseMinMaxLimits and ( CompareText( ProjectionClassName, 'TGCartesianPrj' ) = 0 ) then
    UseMinMaxLimits := False;
end;

procedure TGProjector.SetUnitsPerInch(UPI : Integer);
begin
  ScaleFactor := Screen.PixelsPerInch / UPI;
end;

procedure TGProjector.SetUnitsPerPixel(const Value: Integer);
begin
  ScaleFactor := 1 / Value;
end;

function TGProjector.ExtentsLL : TRect;
begin
  Result := ProjectionModel.ExtentsLL;
end;

function TGProjector.GetUnitsPerInch : Integer;
begin
  Result := Round(ParentGlobe.Renderer.PixelsPerInch / ScaleFactor);
end;

function TGProjector.GetUnitsPerPixel: Integer;
begin
  Result := Round(1 / ScaleFactor);
end;

procedure TGProjector.SetGlobeCenterXY(pt : TPoint);
begin
  if (pt.X <> XOrigin) or (pt.Y <> YOrigin) then
  begin
    XOrigin := pt.X;
    YOrigin := pt.Y;
    //Globe.DoOnPanned;

    TGlobe5(ParentGlobe).UpdateScrollBars;
    TGlobe5(ParentGlobe).RedrawLayers;
  end;
end;

function TGProjector.GetGlobeCenterXY : TPoint;
begin
  Result := Point(XOrigin, YOrigin);
end;

procedure TGProjector.SetXAxisChanges(Value : TGXAxisChanges);
begin
  if FXAxisChanges <> Value then
  begin
    FXAxisChanges := Value;
    ProjectionModel.PropertyChanged(ppXAxisChanges);
  end;
end;

procedure TGProjector.SetYAxisChanges(Value : TGYAxisChanges);
begin
  if FYAxisChanges <> Value then
  begin
    FYAxisChanges := Value;
    ProjectionModel.PropertyChanged(ppYAxisChanges);
  end;
end;

procedure TGProjector.SetCenterLat(Value : TGDecimalDegree);
var
  ptLL : TGPointLL;
begin
//  XOrigin := Globe.Renderer.Width div 2;
  YOrigin := ParentGlobe.Renderer.Height div 2;
  DeviceXYToLL(XOrigin, YOrigin, ptLL);

  XRotation := XRotation - (ptLL.iLatY / GU_DEGREE - Value);
end;

procedure TGProjector.SetCenterLong(Value : TGDecimalDegree);
var
  ptLL : TGPointLL;
begin
  XOrigin := ParentGlobe.Renderer.Width div 2;

//  YRotation := Value;
  DeviceXYToLL(XOrigin, YOrigin, ptLL);

  YRotation := YRotation - (ptLL.iLongX / GU_DEGREE - Value);
end;

function TGProjector.GetCenterLat : TGDecimalDegree;
var
  ptLL : TGPointLL;
begin
  if DeviceXYToLL(ParentGlobe.Renderer.Width div 2, ParentGlobe.Renderer.Height div 2, ptLL) then
    Result := ptLL.iLatY / GU_DEGREE
  else
    Result := MaxInt;
end;

function TGProjector.GetCenterLong : TGDecimalDegree;
var
  ptLL : TGPointLL;
begin
  if DeviceXYToLL(ParentGlobe.Renderer.Width div 2, ParentGlobe.Renderer.Height div 2, ptLL) then
    Result := ptLL.iLongX / GU_DEGREE
  else
    Result := MaxInt;
end;

function TGProjector.IsContinuous: Boolean;
begin
  Result := ProjectionModel.IsContinuous;
end;


function TGProjector.DDToDeviceXY(Long, Lat : TGDecimalDegree; var ptXY : TPoint) : Boolean;
begin
  Result := ProjectionModel.LLToXY( DDToGU(Long), DDToGU(Lat), 0);
  ptXY := ParentGlobe.Renderer.Points[0];
end;

function TGProjector.LLToDeviceXY(iLong, iLat : Integer; var ptXY : TPoint) : Boolean;
begin
  Result := ProjectionModel.LLToXY(iLong, iLat, 0);
  ptXY := ParentGlobe.Renderer.Points[0];
end;


function TGProjector.PixelTolerance: integer;
begin
  Result := Round( TG_PIXEL_TOLERANCE / ScaleFactor );
end;


function TGProjector.PointLLToXY(const ptLL : TGPointLL; var ptXY : TPoint) : Boolean;
begin
  Result := ProjectionModel.PointLLToXY(ptLL, 0);
  ptXY := ParentGlobe.Renderer.Points[0];
end;

function TGProjector.TriangleLLToXY( worldTri : TGTriangleLL; var screenTri : TGTriangle ) : Boolean;
begin
  Result := false;

  if ProjectionModel.PointLLToXY(worldTri[0], 0) then
    if ProjectionModel.PointLLToXY(worldTri[1], 1) then
      if ProjectionModel.PointLLToXY(worldTri[2], 2) then
        with ParentGlobe.Renderer do
        begin
          screenTri[0] := Points[0];
          screenTri[1] := Points[1];
          screenTri[2] := Points[2];

          Result := true;
        end;
end;

procedure TGPen.Assign(Attr : TPersistent);
begin
  if Attr is TGPen then
    with TGPen( Attr ) do
      Self.Define( Color, Style, Width, WidthUnit )
    else
      inherited Assign( Attr );
end;


procedure TGPen.AssignTo(Attr : TPersistent);
begin
  if Attr is TPen then
  begin
    TPen( Attr ).Color := Color;
    TPen( Attr ).Style := TPenStyle( Style );
    TPen( Attr ).Width := Width;
  end
  else
    inherited;
end;


function TGPen.Clone : TGPen;
begin
  Result := TGlobePenClass(Self.ClassType).Create;
  Result.Assign(Self);
end;


constructor TGPen.Create;
begin
  inherited;

  FColor := clBlack;
  FStyle := gpsSolid;
  FWidthUnit := guPixel;
  FWidth := 1;
end;


procedure TGPen.Define(color : TColor; style : TGPenStyle; width : integer;
  widthUnit : TGUnitTypes);
begin
  FColor := color;
  FStyle := style;
  FWidth := width;
  FWidthUnit := widthUnit;

  Modified := true;
end;


function TGPen.Equals(Attr : TGAttribute) : Boolean;
begin
  Result := Attr is TGPen;

  if Result then
    with TGPen(Attr) do
      Result := (Self.Color = Color)
            and (Self.Style = Style)
            and (Self.Width = Width)
            and (Self.WidthUnit = WidthUnit);
end;

function TGPen.AttributeHandle( renderer : TGRenderer ): Cardinal;
begin
  if ( FAttributeHandle = 0 ) or ( renderer <> FRenderer ) then
    CreateHandle( renderer )
  else
    if WidthUnit <> guPixel then
      if FPenAltitude <> renderer.Altitude then
        CreateHandle( renderer );

  Result := FAttributeHandle;
end;

procedure TGPen.CreateHandle( renderer : TGRenderer );
var
  iPenWidth : integer;
  lb : TLogBrush;
  ps: DWord;
begin
  DestroyHandle( renderer );
  FRenderer := renderer;
  FPenAltitude := renderer.Altitude;

  iPenWidth := TGlobe5( renderer.ParentGlobe ).ScaleUnitsToDevice( Width, WidthUnit );

  if iPenWidth > 1 then
  begin
    lb.lbStyle := BS_SOLID;
    lb.lbColor := FColor;

    case FStyle of
    gpsSolid:       ps := PS_SOLID;
    gpsDash:        ps := PS_DASH;
    gpsDot:         ps := PS_DOT;
    gpsDashDot:     ps := PS_DASHDOT;
    gpsDashDotDot:  ps := PS_DASHDOTDOT;
    gpsClear:       ps := PS_NULL;
    gpsInsideFrame: ps := PS_INSIDEFRAME;
    else
      ps := PS_SOLID;
    end;

    FAttributeHandle := ExtCreatePen( ps or PS_GEOMETRIC or PS_ENDCAP_ROUND, iPenWidth, lb, 0, nil)
  end
  else
    FAttributeHandle := CreatePen(Ord(FStyle), 1, FColor );

  Modified := False;
end;

procedure TGPen.LoadFromXML(Element : TGXML_Element);
begin
  if Element <> nil then
    with Element do
    begin
      Color := StringToColor(Attribute('Color', ColorToString(clBlack)));
      Style := TGPenStyle(Attribute('Style', Ord(gpsSolid)));
      Width := Attribute('Width', 1);
      WidthUnit := StrToUnits(Attribute('Unit', UnitsToStr(guPixel)));
    end;
end;

procedure TGPen.SaveToXML(Element : TGXML_Element);
begin
  inherited;

  Element.AddAttribute('Color', ColorToString(Color), ColorToString(clBlack));
  Element.AddAttribute('Style', Ord(Style), Ord(psSolid));
  Element.AddAttribute('Width', Width, 1);
  Element.AddAttribute('Unit', UnitsToStr(WidthUnit), UnitsToStr(guPixel));
end;



procedure TGPen.SetColor(Value : TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Modified := True;
  end;
end;


procedure TGPen.SetStyle(Value : TGPenStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Modified := True;
  end;
end;


procedure TGPen.SetUnit(Value : TGUnitTypes);
begin
  if FWidthUnit <> Value then
  begin
    FWidthUnit := Value;
    Modified := True;
  end;
end;


procedure TGPen.SetWidth(Value : integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Modified := True;
  end;
end;


procedure TGBrush.Assign(Attr : TPersistent);
begin
  if Attr is TGBrush then
    with TGBrush( Attr ) do
      Self.Define(Color, BkColor, Style)
    else
      inherited Assign( Attr );
end;

procedure TGBrush.AssignTo(Attr : TPersistent);
begin
  if Attr is TBrush then
  begin
    TBrush(Attr).Color := Color;
    TBrush(Attr).Style := TBrushStyle( Style );
  end
  else
    inherited;
end;

function TGBrush.Clone : TGBrush;
begin
  Result := TGlobeBrushClass(Self.ClassType).Create;
  Result.Assign(Self);
end;

constructor TGBrush.Create;
begin
  inherited;

  FBkColor := clWhite;
  FColor := clWhite;
  FStyle := gbsSolid;
end;

procedure TGBrush.Define(color, bkColor : TColor; style : TGBrushStyle);
begin
  FColor := color;
  FBkColor := bkColor;
  FStyle := style;

  Modified := true;
end;

function TGBrush.Equals(Attr : TGAttribute) : Boolean;
begin
  Result := Attr is TGBrush;
  if Result then
    with TGBrush(Attr) do
      Result := (Self.Color = Color)
        and (Self.BkColor = BkColor)
        and (Self.Style = Style);
end;

procedure TGBrush.SetBkColor(const Value: TColor);
begin
  if FBkColor <> Value then
  begin
    FBkColor := Value;
    Modified := True;
  end;
end;

procedure TGBrush.SetColor(Value : TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Modified := True;
  end;
end;

procedure TGBrush.SetStyle(Value : TGBrushStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Modified := True;
  end;
end;

function TGBrush.AttributeHandle( renderer : TGRenderer ): Cardinal;
begin
  if ( FAttributeHandle = 0 ) or ( renderer <> FRenderer ) then
    CreateHandle( renderer );

  Result := FAttributeHandle;
end;

procedure TGBrush.CreateHandle( renderer : TGRenderer );
var
  LogBrush : TLogBrush;
  buf: array[0..32] of Char;
  BitmapHandle : HBitmap;
begin
  DestroyHandle( renderer );
  FRenderer := renderer;

  // try to load a bitmap for the brush
  BitmapHandle := 0;
  if FStyle > gbsDiagCross then
    BitmapHandle:= LoadBitmap(HInstance, StrPCopy( buf, 'BRUSH_' + IntToStr(Ord(FStyle))));

  if BitmapHandle <> 0 then
  begin
    FAttributeHandle := CreatePatternBrush(BitmapHandle);
    DeleteObject(BitmapHandle);
  end
  else
    if FStyle <= gbsDiagCross then
    begin
      LogBrush.lbHatch := 0;
      case TGBrushStyle( FStyle ) of
        gbsSolid : LogBrush.lbStyle := BS_SOLID;
        gbsClear : LogBrush.lbStyle := BS_HOLLOW;
      else
        LogBrush.lbStyle := BS_HATCHED;
        LogBrush.lbHatch := Ord(FStyle) - Ord(bsHorizontal);
      end;
      LogBrush.lbColor := ColorToRGB( FColor );
      FAttributeHandle := CreateBrushIndirect(LogBrush);
    end;

  Modified := False;
end;

procedure TGBrush.LoadFromXML(Element : TGXML_Element);
begin
  if Element <> nil then
    with Element do
    begin
      Color := StringToColor(Attribute('Color', ColorToString(clWhite)));
      BkColor := StringToColor(Attribute('BkColor', ColorToString(clWhite)));
      Style := TGBrushStyle(Attribute('Style', Ord(bsSolid)));
    end;
end;

procedure TGBrush.SaveToXML(Element : TGXML_Element);
begin
  inherited;

  Element.AddAttribute('Color', ColorToString(Color), ColorToString(clWhite));
  Element.AddAttribute('BkColor', ColorToString(BkColor), ColorToString(clWhite));
  Element.AddAttribute('Style', Ord(Style), Ord(bsSolid));
end;

procedure TGFont.Assign(Attr : TPersistent);
begin
  if Attr is TGFont then
    with TGFont( Attr ) do
      Self.Define( Name, Color, BkColor, Size, SizeUnit, Angle, Style, BkAlphaBlend)
    else
      inherited Assign( Attr );
end;

procedure TGFont.AssignTo(Attr : TPersistent);
begin
  if Attr is TFont then
  begin
    TFont( Attr ).Name := Name;
    TFont( Attr ).Size := Size;
    TFont( Attr ).Style := TFontStyles( Byte(Style));
    TFont( Attr ).Color := Color;
  end
  else
    inherited;
end;

function TGFont.Clone : TGFont;
begin
  Result := TGlobeFontClass(Self.ClassType).Create;
  Result.Assign(Self);
end;

constructor TGFont.Create;
begin
  inherited;

  FName := 'Arial';
  FSizeUnit := guPixel;
  FSize := 9;
  FColor := clBlack;
  FBkColor := clWhite;
  FBkAlphaBlend := 255;
end;

procedure TGFont.Define(const name : TGFontName; color, bkColor : TColor;
  size : integer; sizeUnit : TGUnitTypes; angle : integer; style : TGFontStyles;
  bkAlpha : Byte);
begin
  FName := name;
  FColor := color;
  FBkColor := bkColor;
  FSize := size;
  FSizeUnit := sizeUnit;
  FAngle := angle;
  FStyle := Byte(style);
  FBkAlphaBlend := bkAlpha;

  Modified := true;
end;


function TGFont.Equals(Attr : TGAttribute) : Boolean;
begin
  Result := Attr is TGFont;
  if Result then
    with TGFont(Attr) do
      Result := (CompareText(Self.Name, Name) = 0)
        and (Self.Color = Color)
        and (Self.BkColor = BkColor)
        and (Self.Style = Style)
        and (Self.SizeUnit = SizeUnit)
        and (Self.Size = Size)
        and (Self.Angle = Angle);
end;

procedure TGFont.SetSizeUnit(Value : TGUnitTypes);
begin
  if FSizeUnit <> Value then
  begin
    FSizeUnit := Value;
    Modified := True;
  end;
end;

procedure TGFont.SetColor(Value : TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Modified := True;
  end;
end;

procedure TGFont.SetAngle(Value : integer);
begin
  if FAngle <> Value then
  begin
    FAngle := Value;
    Modified := True;
  end;
end;

procedure TGFont.SetBkAlphaBlend(const Value: Byte);
begin
  if FBkAlphaBlend <> Value then
  begin
    FBkAlphaBlend := Value;
    Modified := True;
  end;
end;

procedure TGFont.SetBkColor(const Value: TColor);
begin
  if FBkColor <> Value then
  begin
    FBkColor := Value;
    Modified := True;
  end;
end;

procedure TGFont.SetSize(Value : integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Modified := True;
  end;
end;

procedure TGFont.SetFontName(Value : TGFontName);
begin
  if FName <> Value then
  begin
    FName := Value;
    Modified := True;
  end;
end;

function TGFont.GetStyle : TGFontStyles;
begin
  Result := TGFontStyles(FStyle);
end;

procedure TGFont.SetStyle(Value : TGFontStyles);
begin
  if FStyle <> Byte(Value) then
  begin
    FStyle := Byte(Value);
    Modified := True;
  end;
end;

function TGFont.AttributeHandle( renderer : TGRenderer ): Cardinal;
begin
  if ( FAttributeHandle = 0 ) or ( renderer <> FRenderer ) then
    CreateHandle( renderer )
  else
    if SizeUnit <> guPixel then
      if FontAltitude <> renderer.Altitude then
        CreateHandle( renderer );

  Result := FAttributeHandle;
end;

procedure TGFont.CreateHandle( renderer : TGRenderer );
var
  LogFont : TLogFont;
  iHeight : integer;
begin
  DestroyHandle( renderer );
  FRenderer := renderer;

  if renderer <> nil then
  begin
    FontAltitude := renderer.Altitude;

    iHeight := renderer.FontRenderSize(Size, SizeUnit);
//    if (renderer.MaxTextHeight > 0) and (iHeight > renderer.MaxTextHeight) then
    if (renderer.MaxTextHeight > 0) and (iHeight > renderer.GetMaxTextSize) then
      iHeight := renderer.GetMaxTextSize;

    with LogFont do
    begin
      lfHeight := -iHeight;
      lfWidth := 0; { have font Reader choose }
      lfEscapement := Angle;
      lfOrientation := Angle; { angle in tenths of a degree }
      if gfsBold in Style then
        lfWeight := FW_BOLD
      else
        lfWeight := FW_NORMAL;
      lfItalic := Ord(gfsItalic in Style);
      lfUnderline := Ord(gfsUnderline in Style);
      lfStrikeOut := Ord(gfsStrikeOut in Style);
      lfCharSet := DEFAULT_CHARSET;
      StrPCopy(lfFaceName, Name);
      lfQuality := DEFAULT_QUALITY;
      lfOutPrecision := OUT_DEFAULT_PRECIS;
      lfClipPrecision := CLIP_DEFAULT_PRECIS;
      lfPitchAndFamily := DEFAULT_PITCH;
    end;

    FAttributeHandle := CreateFontIndirect(LogFont);
    Modified := False;
  end
  else
    FAttributeHandle := 0;
end;

procedure TGFont.LoadFromXML(Element : TGXML_Element);
begin
  if Element <> nil then
    with Element do
    begin
      Name := Attribute('Name', 'Arial');
      BkColor := StringToColor(Attribute('BkColor', ColorToString(clWhite)));
      Color := StringToColor(Attribute('Color', ColorToString(clBlack)));
      FStyle := Byte(Attribute('Style', Ord(0)));
      Size := Attribute('Size', 9);
      Angle := Attribute('Angle', 0);
      SizeUnit := StrToUnits(Attribute('Unit', UnitsToStr(guPixel)));
      BkAlphaBlend := Attribute('BkAlpha', 255 );
    end;
end;


procedure TGFont.SaveToXML(Element : TGXML_Element);
begin
  inherited;

  Element.AddAttribute('Name', Name, 'Arial');
  Element.AddAttribute('BkColor', ColorToString(BkColor), ColorToString(clWhite));
  Element.AddAttribute('Color', ColorToString(Color), ColorToString(clBlack));
  Element.AddAttribute('Style', FStyle, 0);
  Element.AddAttribute('Size', Size, 9);
  Element.AddAttribute('Angle', Angle, 0);
  Element.AddAttribute('Unit', UnitsToStr(SizeUnit), UnitsToStr(guPixel));
  Element.AddAttribute('BkAlpha', BkAlphaBlend, 255);
end;


function TGLayer.AdjustedAlphaBlend: Byte;
begin
  if ParentLayer <> nil then
  begin
    Result := ParentLayer.AdjustedAlphaBlend;
    if AlphaBlend <> 255 then
      Result := Byte( MulDiv( Result, AlphaBlend, 255 ));
  end
  else
    Result := AlphaBlend;
end;

procedure TGLayer.Assign(Source: TPersistent);
begin
  if Source is TGLayer then
    with TGLayer( Source ) do
    begin
      Self.Clear;
      Self.AlphaBlend := AlphaBlend;
      Self.Enabled := Enabled;
      Self.ObjectSource := ObjectSource.Clone;
      Self.Presenters.Assign( Presenters );
      Self.Name := Name;
      Self.NoInterrupt := NoInterrupt;
      Self.RenderAllObjects := RenderAllObjects;
      Self.Selectable := Selectable;
      Self.ShowTitles := ShowTitles;
      Self.SurfaceImageName := SurfaceImageName;
      Self.SurfaceAlpha := SurfaceAlpha;
      Self.SurfaceTransparentColor := SurfaceTransparentColor;
      Self.TitleColumn := TitleColumn;
      Self.Visible := Visible;
      Self.ZoomMax := ZoomMax;
      Self.ZoomMin := ZoomMin;
      Self.ZoomUnits := ZoomUnits;
      Self.UserXML.Assign(UserXML);
    end
    else
      inherited Assign(Source);
end;


procedure TGLayer.BeginUpdate;
begin
  Inc( FUpdateCounter );
end;


procedure TGLayer.Clear;
begin
  FbSelectable := True;
  FbVisible := True;
  FbEnabled := True;
  FbShowTitles := True;
  FModified := False;

  FZoomMin := 0;
  FZoomMax := 0;
  FZoomUnits := guKilometer;

  Selection.Clear;
  Presenters.Clear;
  ObjectSource.Clear;
  LayerStore.Clear;

  TGlobe5(ParentGlobe).RedrawLayers;
end;

function TGLayer.ClearSelected : Boolean;
var
  idx : integer;
begin
  Result := Selection.Count > 0;
  Selection.Clear;

  for idx := 0 to LayerStore.Count - 1 do
    if LayerStore[idx].ClearSelected then
      Result := true;
end;

function TGLayer.Clone: TGLayer;
begin
  Result := TGLayerClass(Self.ClassType).Create(ParentGlobe);
  Result.Assign(Self);
end;


constructor TGLayer.Create(parentGlobe : TGCustomGlobe5);
begin
  inherited;

  FRenderAllObjects := false;

  FAlphaBlend := 255;
  FUpdateCounter := 0;
  FPresenters := TGPresenterStore.Create(ParentGlobe);
  FPresenterCache := TList.Create;

  FLayerStore := TGLayerStore.Create(ParentGlobe);
  FLayerStore.ParentLayer := Self;
  FSelection := TGLayerSelection.Create( Self );
  FSurfaceAlpha := 255;
  FSurfaceTransparentColor := clNone;

  FUserXML := TGXML_Element.Create('User');
  Clear;
end;


procedure TGLayer.Delete;
begin
  Clear;
  ObjectSource.Delete;
end;


destructor TGLayer.Destroy;
begin
  if ParentLayer <> nil then
    ParentLayer.LayerStore.Remove(self);
  FParentLayer := nil;

  FreeAndNil( FSelection );
  FreeAndNil( FObjectSource );
  FreeAndNil( FPresenterCache );
  FreeAndNil( FPresenters );
  FreeAndNil( FLayerStore );
  FreeAndNil( FSurfaceBitmap );
  FreeAndNil( FUserXML );

  inherited Destroy;
end;


procedure TGLayer.EndUpdate;
begin
  Dec( FUpdateCounter );
  Assert( FUpdateCounter >= 0, 'Mismatched BeginUpdate/EndUpdate' );

  if FUpdateCounter = 0 then
    Post;
end;


function TGLayer.FindPresenter(iPresenterID : integer) : TGPresenter;
var
  aLayer : TGLayer;
begin
  Result := nil;
  if iPresenterID = 0 then
    Exit;

  // search for local Layer Presenter Store then the Global Presenter Store
  Result := Presenters.ByID(iPresenterID);
  if Result = nil then
  begin
    // check up the sub-layer tree
    aLayer := ParentLayer;

    while ( aLayer <> nil ) and ( Result = nil ) do
    begin
      Result := aLayer.Presenters.ByID(iPresenterID);
      aLayer := aLayer.ParentLayer;
    end;
  end;

  // finally check for a Presenter associated with the DB Layer
  if Result = nil then
    Result := ObjectSource.Presenters.ByID(iPresenterID);
end;


function TGLayer.GetIndex : integer;
begin
  if ParentLayer <> nil then
    Result := ParentLayer.LayerStore.IndexOf(Self)
  else
    Result := 0;
end;


function TGLayer.GetLayerID: TGUID;
begin
  Result := ObjectSource.UniqueID;
end;


function TGLayer.GetLayerName : String;
begin
  Result := FsLayerName;
  if Result = '' then
    Result := ObjectSource.Name;
end;


function TGLayer.GetObjectSource: TGObjectSource;
begin
  if FObjectSource = nil then
  begin
    FObjectSource := TGMemoryObjectSource.Create( ParentGlobe );
    FObjectSource.Layer := Self;
  end;

  Result := FObjectSource;
end;


function TGLayer.GetProjection : TGProjector;
begin
  Result := ParentGlobe.Projector;
end;


function TGLayer.GetTitleColumnName: String;
begin
  Result := TitleColumnToName( TitleColumn );
end;

function TGLayer.IsShowing : Boolean;
begin
  Result := Visible;

  if Result and ( ZoomMin <> 0 ) then
    Result := Projector.Altitude >= Cardinal(GUFrom( ZoomMin, ZoomUnits ));

  if Result and ( ZoomMax <> 0 ) then
    Result := Projector.Altitude <= Cardinal(GUFrom( ZoomMax, ZoomUnits ));
end;


function TGLayer.IsValid: Boolean;
begin
  Result := ( ObjectSource <> nil ) and ObjectSource.IsValid;
end;


function TGLayer.IsRenderAborted: Boolean;
begin
  Result := ( not NoInterrupt and TGlobe5(ParentGlobe).IsRenderRestarted ) or Application.Terminated;
end;


function TGLayer.LayerDepth : integer;
var
  tmpLayer : TGLayer;
begin
  Result := 0;
  tmpLayer := ParentLayer;
  while tmpLayer <> nil do
  begin
    Inc( Result );
    tmpLayer := tmpLayer.ParentLayer;
  end;
end;


function TGLayer.LayerMER: TGMER;
var
  idx : integer;
begin
  Result := ObjectSource.DataMER;
  for idx := 0 to LayerStore.Count - 1 do
    Result := MER_Union( Result, LayerStore[idx].LayerMER );
end;


procedure TGLayer.LoadFromXML(Element : TGXML_Element);
var
  sClass : String;
  elObjectSource : TGXML_Element;
begin
//  Clear; // Clear the Layer first;

  if Element <> nil then
    with Element do
    begin
      Name := Attribute('Name', '');
      AlphaBlend := Attribute('Alpha', 255 );
      Enabled := Attribute('Enabled', true);
      NoInterrupt := Attribute('NoInterrupt', false);
      RenderAllObjects := Attribute('RenderAll', false);
      Selectable := Attribute('Selectable', true);
      ShowTitles := Attribute('ShowTitles', true);
      TitleColumn := Attribute('TitleColumn', 0);
      Visible := Attribute('Visible', true);
      ZoomUnits := StrToUnits(Attribute('ZoomUnits', UnitsToStr(guKilometer)));
      ZoomMin := Attribute('ZoomMin', 0);
      ZoomMax := Attribute('ZoomMax', 0);
      SurfaceImageName := Attribute('Image', '' );
      SurfaceAlpha := Attribute('ImageAlpha', 255 );
      SurfaceTransparentColor := Attribute('ImageColor', clNone );

      // Load any sub layers
      LayerStore.LoadFromXML(Element('Layers'));

      // Here we are checking to see if the class of the objectsource has changed.
      // If so we re-create it, if not then we just update the properties and
      // any objects on the ObjectSource will not be deleted.
      elObjectSource := Element('ObjectSource');
      if elObjectSource <> nil then
      begin
        // if we are changing the class type of the ObjectSource
        sClass := elObjectSource.Attribute('Class', '' );
        if sClass = '' then
          sClass := ObjectSource.ClassName;

        if ObjectSource.ClassName <> sClass then
          ObjectSource := TGObjectSource( CreateFromXML( ParentGlobe, elObjectSource, TGMemoryObjectSource ))
        else
          ObjectSource.LoadFromXML( elObjectSource );
      end;

      // Load the Layer Presenters object
      Presenters.LoadFromXML( Element('Presenters'));

      UserXML.Assign( Element('User'));  // Load in any User XML for the Layer

      Index := Attribute('Index', Index);  // Set the position of the layer

      Self.Modified:= false;
    end;
end;

function TGLayer.LoadImage( image : TGBitmap; var imageName : TFilename ) : Boolean;
begin
  Result := ObjectSource.ImageByName(image, imageName);
  
  if not Result then
    Result := TGlobe5( ParentGlobe ).LoadImage(image, imageName);
end;

function TGLayer.MaxTolerance : integer;
begin
  Result := Projector.PixelTolerance;

  if Presenters.Count = 0 then
    ObjectSource.Presenters.AdjustTolerance(Result)
  else
    Presenters.AdjustTolerance(Result);
end;

function TGLayer.ObjectAtLL(ptLL : TGPointLL) : IGMapPoint;
var
  aPresenter : TGPresenter;
  iTolerance : integer;
  objIterator: TGIterator;
begin
  Result := nil;

  if not Visible or not Enabled then
    Exit;

  // if this is the GlobeLayer then first check the label renderer
  if ParentLayer = nil then
  begin
    Result := TGlobe5( ParentGlobe ).LabelRenderer.ObjectAtLL(ptLL);
    if Result <> nil then
      Exit;
  end;

  // Check the sub-layers first
  Result := LayerStore.ObjectAtLL(ptLL);
  if Result <> nil then
    Exit;

  // calculate the size of the MER to search for objects
  iTolerance := MaxTolerance;

  if not MER_ContainsLL( MER_Inflate( LayerMER, iTolerance, iTolerance ), ptLL) then
    Exit;

  objIterator := ObjectSource.NewIterator(
    MER( ptLL.iLongX - iTolerance * 4, ptLL.iLatY - iTolerance * 4, iTolerance * 8 + 1, iTolerance * 8 + 1 ));

  try
    Result := objIterator.Last; // In reverse order
    if Result <> nil then
    begin
      aPresenter := nil;
      iTolerance := Round(2 / Projector.ScaleFactor);

      repeat
        if not Result.Hidden then // Ignore hidden objects
        begin
          // find the presenter for the object
          if ( aPresenter = nil ) or ( Result.PresenterID <> aPresenter.PresenterID ) then
            aPresenter := FindPresenter(Result.PresenterID);

          if aPresenter <> nil then
          begin
            if aPresenter.LLInObject( ptLL, Result, iTolerance) then
              Break;
          end
          else
            if Result.LLInObject(ptLL, iTolerance) then
              Break;
        end;

        Result := objIterator.Prior;
      until Result = nil;
    end;
  finally
    objIterator.Free;
  end;
end;

{------------------------------------------------------------------------------
  TGLayer.ObjectAtXY
------------------------------------------------------------------------------}
{**
  @Param iX X device location.
  @Param iY Y device location.
  @Param tollerance the number of pixels wide to make the search MER
  @Return Object at specifed point or nil if no object found.
}
function TGLayer.ObjectAtXY(iX, iY: integer) : IGMapPoint;
var
  ptLL : TGPointLL;
begin
  Result := nil;

  if Enabled and IsShowing then
  begin
    if Projector.DeviceXYToLL(iX, iY, ptLL) then
      Result := ObjectAtLL( ptLL )
  end;
end;


function TGLayer.ObjectByTitle(const sTitle : String ) : IGMapPoint;
var
  objIterator : TGIterator;
begin
  Result := nil;

  objIterator := ObjectSource.NewIterator;
  try
    Result := objIterator.First;
    while Result <> nil do
    begin
      if Result.Attribute[TitleColumn] = sTitle then
        Break;
      Result := objIterator.Next;
    end;
  finally
    objIterator.Free;
  end;
end;


function TGLayer.ObjectByUserID(const sUserID : String ) : IGMapPoint;
var
  objIterator : TGIterator;
begin
  Result := nil;

  objIterator := ObjectSource.NewIterator;
  try
    Result := objIterator.First;
    while Result <> nil do
    begin
      if Result.UserID = sUserID then
        Break;
      Result := objIterator.Next;
    end;
  finally
    objIterator.Free;
  end;
end;


function TGLayer.PeriodicUpdate : Boolean;
begin
  Result := False;

  if NoInterrupt or ( TGlobe5(ParentGlobe).PeriodicUpdateTime = 0 ) then
    Exit;

  TGlobe5(ParentGlobe).PeriodicUpdateRender;

  Result := IsRenderAborted;
end;


procedure TGLayer.Post;
begin
  ObjectSource.Post;
end;


procedure TGLayer.Render( firstPass : Boolean; const viewMER : TGMER );
var
  idx : integer;
begin
  FRenderObjectCount := 0;
  ReRenderLayer := false;

  if FUpdateCounter <> 0 then
    Exit;

  if IsRenderAborted then
    Exit;

  if not IsShowing then
    Exit;

  if ( FSurfaceBitmap <> nil ) and not FSurfaceBitmap.Empty then
  begin
    FSurfaceBitmap.AlphaBlend := SurfaceAlpha;
    FSurfaceBitmap.TransparentColor := SurfaceTransparentColor;
    ParentGlobe.Projector.ProjectionModel.PaintSurfaceImage(FSurfaceBitmap);
  end;

//  if Projector.RenderBackFace then
//    viewMER := GlobeMER;

  // Objects smaller than this value are ignored
  if RenderAllObjects then
    ObjectSource.CullObjectSize := 0
  else
    ObjectSource.CullObjectSize := Round( TGlobe5(ParentGlobe).CullObjectPixels / ParentGlobe.Projector.ScaleFactor );

  // objects smaller than this size are rendered as a single point
  if RenderAllObjects then
    FMinObjectSize := 0
  else
    FMinObjectSize := Round( TGlobe5(ParentGlobe).MinObjectPixels / ParentGlobe.Projector.ScaleFactor );

  if firstPass then
  begin
    FRenderPass := 0;
    TGlobe5(ParentGlobe).DoOnBeforeLayerRender(Self);
  end
  else
    Inc( FRenderPass );

  FPresenterCache.Clear;
  FCurrentPresenter := nil;

  ObjectSource.ForEach( viewMER, RenderObject );  // render the objects on the layer

  if IsRenderAborted then
    Exit;

  // Tell the Presenters that the layer has finished rendering
  for idx := 0 to FPresenterCache.Count -1 do
    TGPresenter( FPresenterCache[idx] ).EndLayerRender( Self );

  if not ReRenderLayer then // If we have finished rendering the layer
    LayerStore.Render( viewMER );   // render sub layers

  if IsRenderAborted then
    Exit;

  if not ReRenderLayer then
    TGlobe5(ParentGlobe).DoOnAfterLayerRender(Self);  // layer is finally finished
end;


procedure TGLayer.RenderObject( objPK : TGPrimaryKey; var bAbort : Boolean );
var
  State : TGRenderStateSet;
  ptXY : TPoint;
  objMER : TGMER;
  obj : IGMapPoint;

  procedure RenderWithPresenter;
  var
    presenterID : integer;
    idx : integer;
  begin
    if obj.Selected then
      Include( State, rsSelected );

    presenterID := obj.PresenterID;

    if presenterID = 0 then // No Presenter assigned to the object
    begin
      with ParentGlobe.Renderer do
        if rsSelected in State then
        begin
          Brush := SelectedBrush;
          Pen := SelectedPen;
        end
        else
        begin
          Brush := nil;
          Pen := nil;
          AlphaBlend := AdjustedAlphaBlend;
        end;

      obj.Render(ParentGlobe);
    end
    else
      while presenterID <> 0 do
      begin
        // Look for a locally cached presenter
        if ( FCurrentPresenter = nil ) or ( FCurrentPresenter.PresenterID <> presenterID ) then
        begin
          FCurrentPresenter := nil;
          for idx := 0 to FPresenterCache.Count - 1 do
            if TGPresenter( FPresenterCache[idx] ).PresenterID = presenterID then
            begin
              FCurrentPresenter := TGPresenter( FPresenterCache[idx] );
              break;
            end;
        end;

        if FCurrentPresenter = nil then
        begin
          FCurrentPresenter := FindPresenter( presenterID ) as TGPresenter;

          if FCurrentPresenter = nil then // Cannot find a presenter for the object so default render
          begin
            with ParentGlobe.Renderer do
              if rsSelected in State then
              begin
                Brush := SelectedBrush;
                Pen := SelectedPen;
              end
              else
              begin
                Brush := nil;
                Pen := nil;
                AlphaBlend := AdjustedAlphaBlend;
              end;
            obj.Render(ParentGlobe);
            Exit;
          end;

          FCurrentPresenter.BeginLayerRender(Self);
          FPresenterCache.Add( FCurrentPresenter )  // Add into the local presenter cache
        end;

        if FCurrentPresenter.Hidden then // If Hidden flag set then don't display the object
          Exit;

        FCurrentPresenter.RenderObject( obj, State );

        if ShowTitles and ( RenderPass = 0 ) then
          FCurrentPresenter.RenderTitle( obj, State );

        // Step to the next presenter in the chain
        presenterID := FCurrentPresenter.NextPresenterID;
      end;
  end;
begin
  obj := ObjectSource.ByPrimaryKey( objPK );

  if ( obj = nil ) or obj.Hidden then
    Exit;

  objMER := obj.ObjectMER;

  State := [];

  with objMER do
    if ( iWidthX = 1 ) and ( iHeightY = 1 ) then // Check for point objects
    begin
      if ParentGlobe.Projector.LLToDeviceXY( iLongX, iLatY, ptXY) then
        RenderWithPresenter
      else
        obj := nil;
    end
    else
      if ( iWidthX <= FMinObjectSize ) and ( iHeightY <= FMinObjectSize ) then
      begin
        // Render tiny objects as a single point
        if ParentGlobe.Projector.LLToDeviceXY( iLongX, iLatY, ptXY) then
          ParentGlobe.Renderer.Canvas.Pixels[ptXY.X, ptXY.Y] := TGlobe5( ParentGlobe ).MinObjectColor
        else
          obj := nil;
      end
      else
        if Projector.ProjectionModel.VisibleMER( objMER, State) then
          RenderWithPresenter
        else
          obj := nil;

  // let the user act on the object
  if ( obj <> nil ) and Assigned( TGlobe5(ParentGlobe).OnObjectRender ) then
    TGlobe5(ParentGlobe).DoOnObjectRender( Self, obj, State );

  Inc(FRenderObjectCount);
  if (FRenderObjectCount mod 128) = 0 then
    bAbort := PeriodicUpdate;
end;


procedure TGLayer.SaveToXML(Element : TGXML_Element);
begin
  inherited;

  FModified:= False;

//  Element.AddAttribute('ID', GUIDToString( LayerID ), '');
  Element.AddAttribute('Name', Name, '');
  Element.AddAttribute('Alpha', AlphaBlend, 255 );
  Element.AddAttribute('Enabled', Enabled, true);
  Element.AddAttribute('NoInterrupt', NoInterrupt, false);
  Element.AddAttribute('RenderAll', RenderAllObjects, false);
  Element.AddAttribute('Selectable', Selectable, true);
  Element.AddAttribute('ShowTitles', ShowTitles, true);
  Element.AddAttribute('TitleColumn', TitleColumn, 0 );
  Element.AddAttribute('Visible', Visible, true);
  Element.AddAttribute('ZoomUnits', UnitsToStr(ZoomUnits), UnitsToStr(guKilometer));
  Element.AddAttribute('ZoomMin', ZoomMin, 0);
  Element.AddAttribute('ZoomMax', ZoomMax, 0);
  Element.AddAttribute('Index', Index, -1);
  Element.AddAttribute('Image', SurfaceImageName, '' );
  Element.AddAttribute('ImageAlpha', SurfaceAlpha, 255 );
  Element.AddAttribute('ImageColor', SurfaceTransparentColor, clNone );

  if Presenters.Count > 0 then
    Presenters.SaveToXML( Element.AddElement('Presenters'));

  ObjectSource.SaveToXML( Element.AddElement('ObjectSource'));

  if LayerStore.Count > 0 then
    LayerStore.SaveToXML(Element.AddElement('Layers'));

  Element.AddElement('User').Assign(UserXML);
end;


function TGLayer.SelectObjects( aMER : TGMER; includeSubLayers, firstOnly, state : Boolean ) : integer;
var
  obj : IGMapPoint;
  flag : Boolean;
  idx, iTolerance : integer;
  objIterator : TGIterator;

  function TestRect( X, Y, W, H : integer; aPresenter : TGPresenter ) : Boolean;
  var
    centerLL : TGPointLL;
  begin
    Result := true;
    // find the center of the search rect
    centerLL := PointLL( X + W div 2, Y + H div 2);

    // Check for a hit on the object
    if aPresenter <> nil then
    begin
      if aPresenter.LLInObject( centerLL, obj, iTolerance) then
        Exit;
    end
    else
      if obj.LLInObject(centerLL, iTolerance) then
        Exit;

    // sub - divide the search rect and try again recursively
    W := W div 2;
    H := H div 2;
    if ( W > iTolerance ) and ( H > iTolerance ) then
    begin
      if TestRect( X, Y, W, H, aPresenter ) then
        Exit;
      if TestRect( X + W, Y, W, H, aPresenter ) then
        Exit;
      if TestRect( X, Y + H, W, H, aPresenter ) then
        Exit;
      if TestRect( X + W, Y + H, W, H, aPresenter ) then
        Exit;
    end;
    Result := False;
  end;

begin
  Result := 0;

  // if this is the GlobeLayer then first check the label renderer
  if ParentLayer = nil then
  begin
    Result := TGlobe5( ParentGlobe ).LabelRenderer.SelectObjects(aMER, firstOnly, state);
    if firstOnly and ( Result <> 0 ) then
      Exit;
  end;

  if IsShowing and Selectable then
  begin
    // Select objects in sub-layers if requested
    if includeSubLayers then
      for idx := LayerStore.Count - 1 downto 0 do
      begin
        Inc( Result, LayerStore[idx].SelectObjects( aMER, true, firstOnly, state ));
        if firstOnly and ( Result <> 0 ) then
          Exit;
      end;

    // calculate the size of the MER to search for objects
    iTolerance := MaxTolerance;

    ObjectSource.CullObjectSize := 0;
    // allow a large selection rectangle to get point objects
    objIterator := ObjectSource.NewIterator( MER_Inflate( aMER, iTolerance * 4, iTolerance * 4 ));
    try
      obj := objIterator.Last;
      while obj <> nil do
      begin
        if not obj.Hidden then  // ignore hidden objects
        begin
          flag := MER_Contains( aMER, obj.ObjectMER ); // if the object is contained in the selection mer

          if not flag then  // Allow the Presenter a chance to test for the object
            flag := TestRect( aMER.iLongX, aMER.iLatY, aMER.iWidthX, aMER.iHeightY, FindPresenter( obj.PresenterID ) as TGPresenter);

          if flag then
          begin
            obj.Selected := state;
            Inc( Result );
            if firstOnly then
              Exit;
          end;
        end;
        obj := objIterator.Prior;
      end;
    finally
      objIterator.Free;
    end;
  end;
end;

procedure TGLayer.SetAlphaBlend(const Value: Byte);
begin
  if FAlphaBlend <> Value then
  begin
    FAlphaBlend := Value;
    TGlobe5(ParentGlobe).RedrawLayers;
  end;
end;

procedure TGLayer.SetSurfaceImageName(const Value: TFilename);
begin
  if FSurfaceImageName <> Value then
  begin
    FSurfaceImageName := Value;

    if FSurfaceImageName <> '' then
    begin
      if FSurfaceBitmap = nil then
        FSurfaceBitmap := TGBitmap.Create;

      ObjectSource.ImportImage( FSurfaceImageName );
      ObjectSource.ImageByName( FSurfaceBitmap, FSurfaceImageName );
    end
    else
      FreeAndNil( FSurfaceBitmap );

    Modified := True;
    TGlobe5(ParentGlobe).RedrawLayers;
  end;
end;


procedure TGLayer.SetTitleColumn(const Value: integer);
begin
  if FTitleColumn <> Value then
  begin
    FTitleColumn := Value;
    Modified := True;
    TGlobe5(ParentGlobe).RedrawLayers;
  end;
end;

procedure TGLayer.SetTitleColumnName(const Value: String);
var
  idx : integer;
begin
  idx := TitleColumnToIndex(Value);
  if idx >= 0 then
    FTitleColumn := idx;
end;


procedure TGLayer.SetZoomMax(const Value: Cardinal);
begin
  if FZoomMax <> Value then
  begin
    FZoomMax := Value;
    Modified := True;
    TGlobe5(ParentGlobe).RedrawLayers;
  end;
end;


procedure TGLayer.SetZoomMin(const Value: Cardinal);
begin
  if FZoomMin <> Value then
  begin
    FZoomMin := Value;
    Modified := True;
    TGlobe5(ParentGlobe).RedrawLayers;
  end;
end;


procedure TGLayer.SetIndex(iIndex : integer);
begin
  if ParentLayer <> nil then
    ParentLayer.LayerStore.Move(Index, iIndex);
end;


procedure TGLayer.SetLayerBoolean(iIndex : Integer; bValue : Boolean);
begin
  case iIndex of
  0 :
    if FbSelectable <> bValue then
      FbSelectable := bValue
    else
      Exit;
  1 :
    if FbVisible <> bValue then
      FbVisible := bValue
    else
      Exit;
  3 :
    if FbShowTitles <> bValue then
      FbShowTitles := bValue
    else
      Exit;
  4 :
    if FbNoInterrupt <> bValue then
      FbNoInterrupt := bValue
    else
      Exit;
  5 :
    if FbEnabled <> bValue then
      FbEnabled := bValue
    else
      Exit;
  end;

  if not NoInterrupt and ( TGlobe5(ParentGlobe).PeriodicUpdateTime > 0 ) then
    TGlobe5(ParentGlobe).RestartRender;

  Modified := True;
  TGlobe5(ParentGlobe).RedrawLayers;
end;


procedure TGLayer.SetLayerName(const Value: String);
begin
  if Name <> Trim( Value ) then
  begin
    FsLayerName := Trim( Value );
    Modified := True;
  end;
end;



procedure TGLayer.SetObjectSource(const Value: TGObjectSource);
begin
  if FObjectSource <> Value then
  begin
    FreeAndNil( FObjectSource );

    FObjectSource := Value;

    if FObjectSource <> nil then
      FObjectSource.Layer := Self;
  end;
end;


procedure TGLayer.SetZoomUnits(Value : TGUnitTypes);
begin
  if Value <> FZoomUnits then
  begin
    FZoomMin := Round(GUTo(GUFrom(FZoomMin, FZoomUnits), Value));
    FZoomMax := Round(GUTo(GUFrom(FZoomMax, FZoomUnits), Value));

    FZoomUnits := Value;
    Modified := True;
  end;
end;


function TGLayer.TitleColumnToIndex( const columnName : String ) : integer;
var
  sl : TStringList;
begin
  sl := TStringList.Create;

  sl.CommaText := Uppercase( ObjectSource.ColumnNames );
  Result := sl.IndexOf( Uppercase( columnName ));

  sl.Free;
end;


function TGLayer.TitleColumnToName( index : integer ) : String;
var
  sl : TStringList;
begin
  sl := TStringList.Create;

  sl.CommaText := Uppercase( ObjectSource.ColumnNames );
  if index < sl.Count then
    Result := sl[index]
  else
    Result := '';

  sl.Free;
end;


function TGLayerStore.Add(ALayer : TGLayer) : integer;
begin
  Assert( ALayer <> nil, 'TGLayerStore.Add passed nil layer' );

  TGlobe5(ParentGlobe).BeginUpdate;
  try
    if ALayer.ParentLayer <> nil then
    begin
      // If moving from a different layer
      if ALayer.ParentLayer = ParentLayer then
      begin
        Result := ALayer.Index;
        Exit;
      end;
      ALayer.ParentLayer.LayerStore.Remove(ALayer);
    end;

    ALayer.FParentLayer := ParentLayer;
    Result := FLayers.Add( ALayer );

    TGlobe5(ParentGlobe).NotifyLayerTreeChanged;
  finally
    TGlobe5(ParentGlobe).EndUpdate;
  end;
end;


procedure TGLayerStore.Assign(Source: TPersistent);
var
  idx: Integer;
begin
  if Source is TGLayerStore then
  begin
    Clear;
    for idx := 0 to TGLayerStore(Source).Count - 1 do
      Add( TGLayerStore(Source)[idx].Clone );
  end
  else
    if Source is TGLayer then
      Add( TGLayer(Source).Clone );
end;


procedure TGLayerStore.Clear;
var
  idx : integer;
begin
  for idx := Count - 1 downto 0 do
  begin
    Layers[idx].FParentLayer := nil;
    Layers[idx].Free;
  end;

  FLayers.Clear();
end;


constructor TGLayerStore.Create(Globe : TGCustomGlobe5);
begin
  inherited;

  FLayers := TObjectList.Create( False );
end;


function TGLayerStore.CreateLayerFromXML(Element : TGXML_Element) : TGLayer;
var
  sClassName : String;
  layerID : TGUID;
begin
  Result := nil;
  if Element = nil then
    Exit;

  TGlobe5( ParentGlobe ).BeginUpdate;

  try
    layerID := StringToGUID( Element.Element('ObjectSource').Attribute('ID', ''));
    Result := TGlobe5(ParentGlobe).GlobeLayer.LayerStore.LayerByID(layerID)  // Find a layer with the same ID
  except
    Result := TGlobe5(ParentGlobe).GlobeLayer.LayerStore.LayerByName(Element.Attribute('Name', ''));  // Find a layer with the same name
  end;

  if Result = nil then
  begin
    // Get the ClassName of the Layer
    sClassName := Element.Attribute('Class', '');

    // Create the Layer Object
    Result := TGLayerClass(FindClass(sClassName)).Create(ParentGlobe);
    Assert( Result <> nil );
  end;

  Add( Result );  // Make sure the Layer is part of this layerstore

  Result.LoadFromXML(Element);

  if not Result.IsValid then
  begin
    Remove(Result);
    FreeAndNil( Result );
  end;
  TGlobe5( ParentGlobe ).EndUpdate;
end;


procedure TGLayerStore.Delete(ALayer : TGLayer);
begin
  Remove( ALayer );
  ALayer.Delete;
  ALayer.Free;
end;


destructor TGLayerStore.Destroy;
begin
  Clear;

  FreeAndNil( FLayers );

  inherited Destroy;
end;


function TGLayerStore.GetCount: integer;
begin
  Result := FLayers.Count;
end;


function TGLayerStore.GetLayer(iIndex : integer) : TGLayer;
begin
  try
    Result := TGLayer( FLayers[iIndex] );
  except
    raise EGException.CreateFmt(rsEBadLayerIndexMsg, [iIndex]);
  end;
end;


function TGLayerStore.IndexByName(const sName : String) : integer;
begin
  for Result := 0 to Count - 1 do
    if CompareText( Layers[Result].Name, sName ) = 0 then
      Exit;
  Result := -1;
end;



function TGLayerStore.IndexOf(ALayer : TGLayer) : integer;
begin
  Result := FLayers.IndexOf( ALayer );
end;


function TGLayerStore.LayerByID( ID : TGUID ) : TGLayer;
var
  idx : integer;
begin
  // Breadth first search
  for idx := 0 to Count - 1 do
  begin
    Result := Layers[idx];
    if IsEqualGUID( Result.ObjectSource.UniqueID, ID ) then
      Exit;
  end;

  // Now Depth
  for idx := 0 to Count - 1 do
  begin
    Result := Layers[idx].LayerStore.LayerByID(ID);
    if Result <> nil then
      Exit;
  end;

  Result := nil;
end;


function TGLayerStore.LayerByName(const sName : String) : TGLayer;
var
  idx : integer;
begin
  idx := IndexByName( sName );
  if idx >= 0 then
    Result := Layers[idx]
  else
    Result := nil;
end;

function TGLayerStore.LayerFromPresenter( aPresenter : TGPresenter ) : TGLayer;
var
  idx : integer;
begin
  for idx := 0 to Count - 1 do
  begin
    Result := Layers[idx];
    if Result.Presenters.Contains(aPresenter) then
      Exit;

    Result := Result.LayerStore.LayerFromPresenter(aPresenter);
    if Result <> nil then
      Exit;
  end;
  Result := nil;
end;

procedure TGLayerStore.LoadFromXML(Element : TGXML_Element);
var
  idx : integer;
begin
  if Element <> nil then
    with Element do
    begin
      // Load in all the Layers
      for idx := 0 to ElementCount('Layer') - 1 do
        CreateLayerFromXML( Element('Layer', idx));
    end;
end;


procedure TGLayerStore.Move(iFrom, iTo : integer);
begin
  if iFrom <> iTo then
  try
    if ( iFrom >= 0 ) and ( iFrom < FLayers.Count ) and ( iTo >= 0  )and ( iTo < FLayers.Count ) then
      FLayers.Move(iFrom, iTo);
    TGlobe5(ParentGlobe).NotifyLayerTreeChanged;
  except
    // Ignore exceptions
  end;
end;


function TGLayerStore.ObjectAtLL(ptLL : TGPointLL) : IGMapPoint;
var
  idx : Integer;
begin
  Result := nil;
  for idx := Count - 1 downto 0 do
    if Layers[idx].Enabled then
    begin
      Result := Layers[idx].ObjectAtLL(ptLL);
      if Result <> nil then
        Break;
    end;
end;



function TGLayerStore.ObjectAtXY(iX, iY : Integer) : IGMapPoint;
var
  idx : Integer;
begin
  for idx := Count - 1 downto 0 do
    if Layers[idx].Enabled then
    begin
      Result := Layers[idx].ObjectAtXY(iX, iY);
      if Result <> nil then
        Exit;
    end;
  Result := nil;
end;


function TGLayerStore.ObjectByTitle(const sTitle : String ) : IGMapPoint;
var
  idx : Integer;
begin
  for idx := 0 to Count - 1 do
    if Layers[idx].Enabled then
    begin
      Result := Layers[idx].ObjectByTitle(sTitle);
      if Result <> nil then
        Exit;
    end;
  Result := nil;
end;


function TGLayerStore.ObjectByUserID( const sUserID: String): IGMapPoint;
var
  idx : Integer;
begin
  for idx := 0 to Count - 1 do
    if Layers[idx].Enabled then
    begin
      Result := Layers[idx].ObjectByUserID(sUserID);
      if Result <> nil then
        Exit;
    end;
  Result := nil;
end;


procedure TGLayerStore.Remove(ALayer : TGLayer);
begin
  if ALayer <> nil then
  begin
    FLayers.Remove( ALayer );
    ALayer.FParentLayer := nil;

    TGlobe5(ParentGlobe).NotifyLayerTreeChanged;
  end;
end;


procedure TGLayerStore.Render( const viewMER : TGMER );
var
  idx : integer;
  renderPass : integer;
begin
  // give all the layers a chance to render
  for idx := 0 to Count - 1 do
  begin
    renderPass := 0;
    repeat
      if renderPass = 0 then
        TGlobe5( ParentGlobe ).LabelRenderer.AddLayer(Layers[idx]);

      Layers[idx].Render( renderPass = 0, viewMER ); // render the layer

      if TGlobe5(ParentGlobe).IsRenderRestarted or Application.Terminated then
        Exit;

      Inc( renderPass );
    until not Layers[idx].ReRenderLayer;
  end;
end;


procedure TGLayerStore.SaveToXML(Element : TGXML_Element);
var
  idx : integer;
begin
  inherited;

  for idx := 0 to Count - 1 do
    if Layers[idx].Name <> '' then  // Do not save layers without a name
      Layers[idx].SaveToXML( Element.AddElement('Layer'));
end;


procedure TGCustomProjectionModel.Assign(AModel : TPersistent);
begin
  if not (AModel is TGCustomProjectionModel) then
    inherited Assign(AModel);
end;


function TGCustomProjectionModel.Clone(projector : TGProjector) : TGCustomProjectionModel;
begin
  Result := TGCustomProjectionModelClass(Self.ClassType).Create(projector.ParentGlobe);
  Result.Assign(Self);
end;



procedure TGPresenter.AdjustTolerance( var tolerance : integer );
begin
  // Does nothing by default
end;

procedure TGPresenter.Assign(Source : TPersistent);
var
  doc : TGXML_Document;
begin
  if Source is TGPresenter then
  begin
    doc := TGXML_Document.Create('Presenter');
    TGPresenter(Source).SaveToXML(doc.Document);
    LoadFromXML( doc.Document );
    doc.Free;
  end
  else
    inherited Assign(Source);

{  if Source is TGPresenter then
    with TGPresenter(Source) do
    begin
      Self.AlphaBlend := AlphaBlend;
      Self.PresenterID := PresenterID;
      Self.NextPresenterID := NextPresenterID;
      Self.TitleAlignment := TitleAlignment;
      Self.TitleColumn := TitleColumn;
      Self.TitleOffset := TitleOffset;
      Self.TitleOffsetUnits := TitleOffsetUnits;
      Self.TitleZoomMax := TitleZoomMax;
      Self.TitleZoomMin := TitleZoomMin;
      Self.TitleZoomUnits := TitleZoomUnits;

      if TitleFont <> nil then
      begin
        Self.TitleFont.Free;
        Self.TitleFont := TitleFont.Clone;
      end;
      Self.Name := Name;
      Self.Hidden := Hidden;
    end
    else
      inherited Assign(Source);
}
end;

procedure TGPresenter.BeginLayerRender(aLayer: TGLayer);
begin
  inherited;
  FRenderer := nil;
  FAltitude := ParentGlobe.Projector.Altitude;
  FTitleVisible := tsUnknown;

  FAdjustedAlphaBlend := MulDiv( aLayer.AdjustedAlphaBlend, AlphaBlend, 255 );

  DoBeginLayer( aLayer );
end;

function TGPresenter.Clone : TGPresenter;
begin
  Result := TGPresenterClass(Self.ClassType).Create(ParentGlobe);
  Result.Assign(Self);
end;

constructor TGPresenter.Create(parentGlobe: TGCustomGlobe5);
begin
  inherited;

  Name := ClassName;
  PresenterID := 0;

  FAlphaBlend := 255;
  FTitleFont := TGFont.Create;
  FTitleColumn := 0;
  FTitleAlignment := taNone;
  FTitleZoomUnits := guKilometer;
  FTitleOffset := 0;
  FTitleOffsetUnits := guPixel;
  FTitleVisible := tsUnknown;
end;


destructor TGPresenter.Destroy;
begin
  FreeAndNil( FTitleFont );
  inherited Destroy;
end;

procedure TGPresenter.DoBeginLayer( aLayer : TGLayer );
begin
  if FPresenterEvents <> nil then
    FPresenterEvents.OnBeginLayer( aLayer );
end;

procedure TGPresenter.DoBeginRender( mapObj : IGMapPoint );
begin
  if FPresenterEvents <> nil then
    FPresenterEvents.OnBeginRender( mapObj );
end;

procedure TGPresenter.DoEndLayer( aLayer : TGLayer );
begin
  if FPresenterEvents <> nil then
    FPresenterEvents.OnEndLayer( aLayer );
end;

procedure TGPresenter.DoEndRender( mapObj : IGMapPoint );
begin
  if FPresenterEvents <> nil then
    FPresenterEvents.OnEndRender( mapObj );
end;

procedure TGPresenter.EndLayerRender(aLayer: TGLayer);
begin
  DoEndLayer( aLayer );
end;

function TGPresenter.Equals( aPresenter : TGPresenter ): Boolean;
begin
  with aPresenter as TGPresenter do
    Result := ( Self.AlphaBlend = AlphaBlend )
        and ( Self.TitleAlignment = TitleAlignment )
        and ( Self.TitleColumn = TitleColumn )
        and ( Self.TitleFont.Equals( TitleFont ))
        and ( Self.TitleOffset = TitleOffset )
        and ( Self.TitleOffsetUnits = TitleOffsetUnits )
        and ( Self.TitleZoomMax = TitleZoomMax )
        and ( Self.TitleZoomMin = TitleZoomMin )
        and ( Self.TitleZoomUnits = TitleZoomUnits );
end;

function TGPresenter.GetRenderer: TGRenderer;
begin
  if FRenderer = nil then
    FRenderer := ParentGlobe.Renderer;
  Result := FRenderer;
end;

function TGPresenter.IsTitleVisible: Boolean;
var
  ZoomMin, ZoomMax : Cardinal;
begin
  if FTitleVisible = tsUnknown then
  begin
    FTitleVisible := tsFalse;

    if not Hidden and ( TitleAlignment <> taNone ) then
    begin
      ZoomMin := GUFrom( TitleZoomMin, TitleZoomUnits );
      ZoomMax := GUFrom( TitleZoomMax, TitleZoomUnits );

      if (ZoomMin = ZoomMax) or ((FAltitude > ZoomMin) and (FAltitude < ZoomMax)) then
        FTitleVisible := tsTrue;
    end;
  end;
  Result := ( FTitleVisible = tsTrue );
end;

function TGPresenter.LLInObject( ptLL : TGPointLL; mapObj : IGMapPoint; iTolerance : integer ) : Boolean;
begin
  with mapObj.ObjectMER do
    Result := MER_ContainsLL( MER( iLongX - iTolerance, iLatY - iTolerance, iWidthX + iTolerance, iHeightY + iTolerance ), ptLL);
end;

procedure TGPresenter.LoadFromXML(Element : TGXML_Element);
begin
  if Element <> nil then
    with Element do
    begin
      FName := Attribute('Name', '');
      FPresenterID := Attribute('PresenterID', 0);
      FNextPresenterID := Attribute('NextID', 0);
      FTitleAlignment := TGTitleAlignment(Attribute('Alignment', Ord(taCenter)));
      FTitleOffset := Attribute('Offset', 0);
      FTitleOffsetUnits := StrToUnits(Attribute('OffsetUnit', UnitsToStr(guPixel)));
      FTitleColumn := Attribute('Column', 0);
      FHidden := Attribute('Hidden', false);
      FAlphaBlend := Attribute('Alpha', 255);

      FiTitleZoomMax := Attribute('ZoomMax', 0);
      FiTitleZoomMin := Attribute('ZoomMin', 0);
      FTitleZoomUnits := StrToUnits(Attribute('ZoomUnit', UnitsToStr(guKilometer)));

      TitleFont.LoadFromXML(Element('TitleFont', 0));
    end;
end;


procedure TGPresenter.SaveToXML(Element : TGXML_Element);
begin
  Element.AddAttribute('Class', ClassName, '');

  Element.AddAttribute('Name', Name, '');
  Element.AddAttribute('PresenterID', PresenterID, 0);
  Element.AddAttribute('NextID', NextPresenterID, 0);
  Element.AddAttribute('Alignment', Ord(TitleAlignment), Ord(taCenter));
  Element.AddAttribute('Offset', TitleOffset, 0);
  Element.AddAttribute('OffsetUnit', UnitsToStr(TitleOffsetUnits), UnitsToStr(guPixel));
  Element.AddAttribute('Column', TitleColumn, 0);
  Element.AddAttribute('Hidden', Hidden, false);
  Element.AddAttribute('Alpha', FAlphaBlend, 255);

  Element.AddAttribute('ZoomMax', TitleZoomMax, 0);
  Element.AddAttribute('ZoomMin', TitleZoomMin, 0);
  if ( TitleZoomMax <> 0 ) or ( TitleZoomMin <> 0 ) then
    Element.AddAttribute('ZoomUnit', UnitsToStr(TitleZoomUnits), UnitsToStr(guKilometer));

  TitleFont.SaveToXML( Element.AddElement('TitleFont'));
end;


procedure TGPresenter.RenderLegend( Canvas : TCanvas; rect : TRect );
begin
  // Implemented by descendant Presenters
end;

procedure TGPresenter.RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet);
begin
  // Implemented by descendant Presenters
  DoBeginRender( mapObj );

  DoEndRender( mapObj );
end;

procedure TGPresenter.RenderTitle( mapObj : IGMapPoint;  State : TGRenderStateSet );
begin
  if IsTitleVisible then
    TGlobe5(ParentGlobe).LabelRenderer.AddLabel(
      mapObj,
      mapObj.Attribute[TitleColumn],
      TitleFont,
      TitleAlignment,
      TitleOffset,
      TitleOffsetUnits );
end;


function TGPresenterStore.Add(aPresenter : TGPresenter) : integer;
var
  idx : integer;
  tmpPresenter : TGPresenter;
begin
  Result := 0;
  if aPresenter <> nil then
  begin
    Result := aPresenter.PresenterID;

    if Result = 0 then
    begin
      // Find the highest presenter ID
      for idx := 0 to FPresenters.Count - 1 do
        if Presenters[idx].PresenterID > Result then
          Result := Presenters[idx].PresenterID;

      Inc(Result);
      aPresenter.PresenterID := Result;
    end
    else
    begin
      tmpPresenter := ByID(Result);
      if tmpPresenter <> nil then // remove existing from list 
        Delete( tmpPresenter );
    end;

    FPresenters.Add( aPresenter );
    Modified := True;
  end;
end;


procedure TGPresenterStore.AddPresenters( presenterStore : TGPresenterStore );
var
  idx : integer;
begin
  for idx := 0 to presenterStore.Count - 1 do
    Add( presenterStore[idx].Clone ); // Add a copy of the presenters
end;


procedure TGPresenterStore.AdjustTolerance( var tolerance : integer );
var
  idx : integer;
begin
  for idx := 0 to Count - 1 do
    Presenters[idx].AdjustTolerance(tolerance);
end;


procedure TGPresenterStore.Assign(Source: TPersistent);
var
  idx: Integer;
begin
  if Source is TGPresenterStore then
  begin
    Clear;
    for idx:= 0 to TGPresenterStore(Source).Count - 1 do
      Add( TGPresenter(TGPresenterStore(Source)[idx]).Clone );
  end
  else
    if Source is TGPresenter then
      Add(TGPresenter(Source).Clone);
end;

function TGPresenterStore.ByID(iPresenterID : integer) : TGPresenter;
var
  idx : integer;
begin
  { Search for the requested Presenter on this Layer }
  for idx := 0 to Count - 1 do
  begin
    Result := Presenters[idx];

    if iPresenterID = Result.PresenterID then
      Exit;
  end;
  Result := nil;
end;


function TGPresenterStore.ByName( const PresenterName : String) : TGPresenter;
var
  idx : integer;
begin
  { Search for the requested Presenter on this Layer }
  for idx := 0 to Count - 1 do
  begin
    Result := Presenters[idx];

    if PresenterName = Result.Name then
      Exit;
  end;
  Result := nil;
end;


procedure TGPresenterStore.Clear;
var
  idx : integer;
begin
  for idx := Count - 1 downto 0 do
    Presenters[idx].Free;

  FPresenters.Clear;
  Modified := False;
end;

function TGPresenterStore.Contains(aPresenter : TGPresenter) : Boolean;
begin
  Result := FPresenters.IndexOf(aPresenter) >= 0;
end;

constructor TGPresenterStore.Create( parentGlobe : TGCustomGlobe5 );
begin
  inherited;

  FPresenters := TObjectList.Create( False );

  FModified := False;
end;


procedure TGPresenterStore.Delete(aPresenter : TGPresenter);
begin
  if aPresenter <> nil then
  begin
    FPresenters.Remove( aPresenter );
    aPresenter.Free;
    Modified := True;
  end;
end;


destructor TGPresenterStore.Destroy;
begin
  Clear;
  FreeAndNil(FPresenters);

  inherited Destroy;
end;

function TGPresenterStore.FindMatch( aPresenter : TGPresenter ) : integer;
var
  idx : integer;
begin
  for idx := 0 to Count - 1 do
    if Presenters[idx].Equals(aPresenter) then
    begin
      Result := Presenters[idx].PresenterID;
      Exit;
    end;
  Result := 0;
end;

function TGPresenterStore.GetAsXMLString: String;
var
  doc : TGXML_Document;
begin
  doc := TGXML_Document.Create( 'Presenters' );
  SaveToXML( doc.Document );
  Result := doc.AsString;
  doc.Free;
  Modified := false;
end;


function TGPresenterStore.GetCount : integer;
begin
  Result := FPresenters.Count;
end;


function TGPresenterStore.GetPresenter(iIndex : integer) : TGPresenter;
begin
  Result := TGPresenter(FPresenters[iIndex]);
end;


procedure TGPresenterStore.LoadFromXML(Element : TGXML_Element);
var
  idx : integer;

  function CreatePresenter(iOccourance : integer) : TGPresenter;
  var
    sClassName : String;
    presenterElement : TGXML_Element;
  begin
    Result := nil;
    presenterElement := Element.Element('Presenter', iOccourance);

    if presenterElement <> nil then
    begin
      // Get the ClassName of the Layer
      sClassName := presenterElement.Attribute('Class', '');

      // convert older presenter names to new presenter names
      if Copy( sClassName, 1, 2 ) = 'TP' then
        sClassName := 'TGP' + Copy( sClassName, 3, 255 );

      try
        // Create the Object
        Result := TGPresenterClass(FindClass(sClassName)).Create(ParentGlobe);
        // Load the properties
        Result.LoadFromXML(presenterElement);
        Add(Result);
      except
        Result := nil;
      end;
    end;
  end;
begin
  if Element <> nil then
  begin
    Clear; // Clear the Presenter Store First;
    with Element do
    begin
      // Load in all the Presenter
      idx := 0;
      while CreatePresenter(idx) <> nil do
        Inc(idx);
    end;
  end;
end;

procedure TGPresenterStore.SetAsXMLString(const Value: String);
var
  doc : TGXML_Document;
begin
  doc := TGXML_Document.Create( 'Presenters' );
  doc.AsString := Value;
  LoadFromXML( doc.Document );
  doc.Free;
  Modified := false;
end;



procedure TGPresenterStore.SaveToXML(Element : TGXML_Element);
var
  idx : integer;
begin
  for idx := 0 to Count - 1 do
    Presenters[idx].SaveToXML( Element.AddElement('Presenter'));
end;


procedure TGObjectSource.Add(obj: IGMapPoint);
var
  aPresenter : TGPresenter;
begin
  if obj = nil then
    Exit;
  // copy over the object's Presenter if necessary
  if obj.ObjectSource <> nil then
  begin
    if Presenters.ByID(obj.PresenterID) = nil then
    begin
      // get the objects Presenter does an indepth search for the presenter
      aPresenter := obj.ObjectSource.Layer.FindPresenter( obj.PresenterID ) as TGPresenter;
      if aPresenter <> nil then
        Presenters.Add(aPresenter.Clone);
    end;
  end;
  obj.ObjectSource := Self;
end;

procedure TGObjectSource.Clear;
begin
  FPresenters.Clear;
  FMetaData.Clear;
  FUserXML.Clear;
end;

function TGObjectSource.Clone : TGObjectSource;
begin
  Result := TGObjectSourceClass(Self.ClassType).Create(ParentGlobe);
  Result.CopyFrom(Self);
end;

procedure TGObjectSource.CopyFrom(objectSource: TGObjectSource);
var
  objIterator : TGIterator;
  obj : IGMapPoint;
begin
  if objectSource = Self then
    Exit;

  Clear;

  MetaData.Assign(objectSource.MetaData);
  Presenters.Assign(objectSource.Presenters);
  FUserXML.Assign(objectSource.UserXML);

  objIterator := objectSource.NewIterator;
  try
    obj := objIterator.First;
    while obj <> nil do
    begin
      Add( obj.Clone );
      obj := objIterator.Next;
    end;
  finally
    objIterator.Free;
  end;
end;

constructor TGObjectSource.Create( parentGlobe: TGCustomGlobe5 );
begin
  inherited;
  FUniqueID := NULL_GUID;
  FCullObjectSize := 0;
  FPresenters := TGPresenterStore.Create(ParentGlobe);
  FMetaData := TStringList.Create;
  FUserXML := TGXML_Element.Create( 'User' );
end;

function TGObjectSource.DataMER: TGMER;
var
  objIterator : TGIterator;
  obj : IGMapPoint;
begin
  MER_Empty( Result );
  objIterator := NewIterator;
  try
    obj := objIterator.First;
    while obj <> nil do
    begin
      Result := MER_Union(Result, obj.ObjectMER);
      obj := objIterator.Next;
    end;
  finally
    objIterator.Free;
  end;
end;

destructor TGObjectSource.Destroy;
begin
  FreeAndNil( FPresenters );
  FreeAndNil( FMetaData );
  FreeAndNil( FUserXML );
  inherited;
end;

procedure TGObjectSource.EditObject( mapObject : IGMapPoint );
begin
end;

procedure TGObjectSource.EndEditObject( mapObject : IGMapPoint );
begin
end;

function TGObjectSource.FirstObject : IGMapPoint;
begin
  Result := ByPrimaryKey( First );
end;

function TGObjectSource.GetColumnNames: String;
begin
  Result := MetaData.Values['ColumnNames'];
end;

function TGObjectSource.GetName: String;
begin
  Result := FName;
end;

function TGObjectSource.GetPresenters: TGPresenterStore;
begin
  Result := FPresenters;
end;

procedure TGObjectSource.ImportData( reader : TGCustomDataReader );
var
  obj : IGMapPoint;
begin
  if reader <> nil then
  begin
    Name := reader.Name;
    obj := reader.First;
    while obj <> nil do
    begin
      Add( obj );
      obj := reader.Next;
    end;

    Presenters.Assign( reader.Presenters );
    MetaData.Assign( reader.MetaData );
  end;
end;

function TGObjectSource.IsValid: Boolean;
begin
  Result := true;
end;

procedure TGObjectSource.LoadDataFromXML(Element: TGXML_Element);
var
  idx : integer;
  objectsElement: TGXML_Element;

  function CreateMapObject(objElement : TGXML_Element ) : IGMapPoint;
  var
    className : String;
  begin
    className := objElement.Attribute('Class', '');
    try
      Result := TGMapObjectClass(FindClass(className)).Create as IGMapPoint;

      Result.LoadFromXML(objElement);
      Result.Invalidate;
    except
      Result := nil;
    end;
  end;
begin
  if Element <> nil then
  try
    Presenters.LoadFromXML( Element.Element('Presenters'));

    objectsElement := Element.Element('Objects');
    if objectsElement <> nil then
    begin
      Clear;

      for idx := 0 to objectsElement.ElementCount - 1 do
        Add( CreateMapObject( objectsElement.Element(idx)));

      Post;
    end;
  except
    // ignore errors
  end;
end;

procedure TGObjectSource.LoadFromXML(Element: TGXML_Element);
begin
  if Element <> nil then
  try
    Name := Element.Attribute( 'Name', Name );

    UniqueID := StringToGUID( Element.Attribute( 'ID', GUIDToString(UniqueID) ));

    ColumnNames := Element.Attribute('ColumnNames', ColumnNames);

    if Element.ElementExists('User', 0) then
      UserXML.Assign(Element.Element('User', 0));

    LoadDataFromXML( Element );
  except
    // ignore errors
  end;
end;


function TGObjectSource.NewIterator: TGIterator;
begin
  Result := TGIterator.Create( Self );
end;

function TGObjectSource.NextObject( mapObject : IGMapPoint ) : IGMapPoint;
begin
  if mapObject = nil then
    Result := nil
  else
    Result := ByPrimaryKey(Next(mapObject.PrimaryKey));
end;

procedure TGObjectSource.Post;
begin
  // save the changes to the presenters
  MetaData.Values['PresenterXML'] := Presenters.AsXMLString;
end;


procedure TGObjectSource.SaveDataToXML(Element: TGXML_Element);
var
  objIterator : TGIterator;
  obj : IGMapPoint;
  objElement: TGXML_Element;
begin
  Presenters.SaveToXML( Element.AddElement('Presenters'));

  objElement := Element.AddElement('Objects');

  objIterator := NewIterator;
  try
    obj := objIterator.First;
    while obj <> nil do
    begin
      obj.SaveToXML( objElement.AddElement('Obj'));
      obj := objIterator.Next;
    end;
  finally
    objIterator.Free;
  end;
end;

procedure TGObjectSource.SaveToXML(Element: TGXML_Element);
begin
  inherited;
  Element.AddAttribute('ID', GUIDToString(UniqueID), '' );
  Element.AddAttribute('Name', Name, '' );
  Element.AddAttribute('ColumnNames', ColumnNames, '');
  Element.AddElement('User').Assign( UserXML );

  if SaveDataInXML then
    SaveDataToXML( Element );
end;

procedure TGObjectSource.SetColumnNames(const Value: String);
begin
  MetaData.Values['ColumnNames'] := Value;
end;

procedure TGObjectSource.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TGObjectSource.SetUniqueID(Value: TGUID);
begin
  FUniqueID := Value;
end;


procedure TGRenderer.Assign(Source: TPersistent);
begin
  with Source as TGRenderer do
  begin
    Self.AlphaBlend := AlphaBlend;
    Self.FAltitude := Altitude;
    Self.Height := Height;
    Self.Width := Width;
    Self.PixelsPerInch := PixelsPerInch;
    Self.Brush.Assign( Brush );
    Self.Font.Assign( Font );
    Self.Pen.Assign( Pen );
    Self.SelectedBrush.Assign(SelectedBrush);
    Self.SelectedPen.Assign(SelectedPen);
    Self.DoubleBuffer := DoubleBuffer;
    Self.MaxTextHeight := MaxTextHeight;
    Self.MinTextHeight := MinTextHeight;
  end;
end;

procedure TGRenderer.BeginDrawPoly(closed: Boolean);
begin
  if Length( FPolyPolyPoints ) > 0 then
    EndDrawPoly;
//  Assert( Length( FPolyPolyPoints ) = 0, 'TGRenderer.BeginDrawPoly: Nested calls not supported.' );

  FDrawPolyClosed := closed;
end;

procedure TGRenderer.ColorBlend( var target: TGColor; source: TGColor; alpha: Byte);
var
  R,G,B : integer;
begin
  // Alpha blend the source color into the target colour
  R := (GetRValue( source ) * alpha + GetRValue( target ) * (255 - alpha)) shr 8;
  G := (GetGValue( source ) * alpha + GetGValue( target ) * (255 - alpha)) shr 8;
  B := (GetBValue( source ) * alpha + GetBValue( target ) * (255 - alpha)) shr 8;

  target := RGB( R, G, B );
end;

procedure TGRenderer.EndDrawPoly;
var
  idx : integer;
begin
  if Length( FPolyPolyPoints ) > 0 then
  try
    DrawPolyPoly(FPolyPolyPoints, FDrawPolyClosed);

    // Reset the Poly Poly array
    for idx := 0 to High( FPolyPolyPoints ) do
      SetLength( FPolyPolyPoints[idx], 0 );
    SetLength( FPolyPolyPoints, 0 );
  except
    // Ignore exceptions for now.
  end;
end;

procedure TGRenderer.UpdateClipRect( offset : integer );
begin
  CpRegion[0] := Point( -offset, FHeight + offset );
  CpRegion[1] := Point( FWidth + offset, FHeight + offset );
  CpRegion[2] := Point( -offset, -offset );
  CpRegion[3] := Point( FWidth + offset, -offset );
end;

function TGRenderer.ClipCode( const pt : TPoint ) : integer;
var
  x, y : integer;
begin
  x := pt.X;
  y := pt.Y;

  if x < CpRegion[0].x then
  begin
    if y < CpRegion[3].y then
      Result := 6 or TG_TWOBITS
    else
      if y > CpRegion[0].y then
        Result := 12 or TG_TWOBITS
      else
        Result := 4;
    Exit;
  end;

  if x > CpRegion[3].x then
  begin
    if y < CpRegion[3].y then
      Result := 3 or TG_TWOBITS
    else
      if y > CpRegion[0].y then
        Result := 9 or TG_TWOBITS
      else
        Result := 1;
    Exit;
  end;

  if y < CpRegion[3].y then
    Result := 2
  else
    if y > CpRegion[0].y then
      Result := 8
    else
      Result := 0;
end;

function TGRenderer.StartClip: Boolean;
begin
  CpStart.Point := FPoints[0];
  CpStartCode := ClipCode( CpStart.Point );
  CpStart.Code := CpStartCode;
  Result := CpStartCode = 0;
end;

function TGRenderer.EndClip( const pt : TPoint ) : integer;
var
  OutCode : integer;
  X, Y : double;
  startPt, endPt : TPoint;
begin
  // Sutherland Cohen clipping
  startPt := CpStart.Point;
  CpStartCode := ClipCode( startPt );
  CpStart.Code := CpStartCode;

  endPt := pt;
  CpEnd.Point := endPt;
  CpEndCode := ClipCode( endPt );

  if ( CpStartCode = 0 ) and ( CpEndCode = 0 ) then
    Result := TG_SEGM
  else
  begin
    Result := TG_NOSEGM;

    CpEnd.Code := CpEndCode;
    while ( CpStart.Code <> 0 ) or ( CpEnd.Code <> 0 ) do
    begin
      if (( CpStart.Code and not TG_TWOBITS ) and ( CpEnd.Code and not TG_TWOBITS )) <> 0 then
        Exit;

      if CpStart.Code <> 0 then
        OutCode := CpStart.Code
      else
        OutCode := CpEnd.Code;

      if ( OutCode and 1 ) <> 0 then // Clip to Right
      begin
        X := CpRegion[3].x;
        Y := startPt.Y + Round(( X - startPt.x ) * ( endPt.Y - startPt.y ) / (endPt.X - startPt.X));
      end
      else if ( OutCode and 4 ) <> 0 then // Clip to Left
      begin
        X := CpRegion[0].x;
        Y := startPt.Y + Round(( X - startPt.x ) *  ( endPt.Y - startPt.y ) / ( endPt.X - startPt.X));
      end
      else if ( OutCode and 2 ) <> 0 then // Clip to Top
      begin
        Y := CpRegion[3].y;
        X := startPt.X + Round(( Y - startPt.Y ) * ( endPt.X - startPt.X ) / (endPt.Y - startPt.Y));
      end
      else if ( OutCode and 8 ) <> 0 then // Clip to bottom
      begin
        Y := CpRegion[0].y;
        X := startPt.X + Round(( Y - startPt.Y ) * (endPt.X - startPt.X) / (endPt.Y - startPt.Y));
      end
      else
      begin
        X := 0; // to stop the compiler from giving warnings
        Y := 0;
      end;

      if CpStart.Code <> 0 then
      begin
        startPt := Point(Round(X),Round(Y));
        CpStart.Point := startPt;
        CpStart.Code := ClipCode( startPt );
      end
      else
      begin
        endPt := Point(Round(X),Round(Y));
        CpEnd.Point := endPt;
        CpEnd.Code := ClipCode( endPt );
      end;
    end;
    Result := TG_SEGM or TG_CLIP;
  end;
end;

function TGRenderer.ClipPoly(iCount: Integer; closed : Boolean; iPenWidth: integer): Integer;
var
  Cp_t_start : TPoint;
  Cp_t_end : TPoint;
  Cp_A_point : TPoint;
  A_code : integer;

  idx : integer;
  tmp : integer;
  iClippedCount : integer;

  procedure AddClippedPoint( const pt : TPoint );
  begin
//    if ( iClippedCount > 0 ) and CompareMem( @FClippedPoints[iClippedCount - 1], @pt, SizeOf(TPoint)) then
    if ( iClippedCount > 0 ) and ( FClippedPoints[iClippedCount - 1].X = pt.X ) and ( FClippedPoints[iClippedCount - 1].Y = pt.Y ) then
      Exit;
    FClippedPoints[iClippedCount] := pt;
    Inc( iClippedCount );
  end;
begin
  iClippedCount := 0;

  if iPenWidth = 0 then
    iPenWidth := 1;
  UpdateClipRect( iPenWidth * 2 );

  // Make sure the polygon is closed
  if closed then
  begin
    FPoints[iCount] := FPoints[0];
    Inc( iCount );
  end;

  // Get the status of the first point
  if StartClip then
    AddClippedPoint( CpStart.Point );

  for idx := 1 to iCount - 1 do
  begin
    tmp := EndClip( FPoints[idx] );

    // If the line is visible
    CpStart.Code := CpEndCode;
    if ( tmp and TG_SEGM ) <> 0 then
    begin
      if ( tmp and TG_CLIP ) <> 0 then
        AddClippedPoint( CpStart.Point );
      AddClippedPoint( CpEnd.Point );
    end
    else  // The line has been rejected
    begin
      // if end point has 2 bits
      if ( CpEndCode and TG_TWOBITS ) <> 0 then
      begin
        //
        if (( CpStartCode and CpEndCode ) and not TG_TWOBITS ) = 0 then
        begin
          // if start point is a 2 bit then do mid point subdivision
          if ( CpStartCode and TG_TWOBITS ) <> 0 then
          begin
            tmp := 1;
            A_code := 0;
            Cp_t_start := CpStart.Point;
            Cp_t_end := CpEnd.Point;
            while tmp > 0 do
            begin
              // get mid point
              Cp_A_point.x := ( Cp_t_start.x + Cp_t_end.x ) div 2;
              Cp_A_point.y := ( Cp_t_start.y + Cp_t_end.y ) div 2;

              A_code := ClipCode( Cp_A_point );

              if ( A_code and TG_TWOBITS ) <> 0 then
              begin
                if A_code = CpEndCode then
                  Cp_t_end := Cp_A_point
                else
                  if A_code = CpStartCode then
                    Cp_t_start := Cp_A_point
                  else
                    tmp := 0;
              end
              else
              begin
                if ( A_code and CpEndCode ) <> 0 then
                  A_code := CpStartCode + CpTcc[A_code and not TG_TWOBITS]
                else
                  A_code := CpEndCode + CpTcc[A_code and not TG_TWOBITS];
                tmp := 0;
              end;
            end;  // while
          end
          else  // 1 bit start 2 bit end
            A_code := CpEndCode + CpTcc[CpStartCode];

          AddClippedPoint( CpRegion[CpCra[A_code and not TG_TWOBITS]] );
        end;
      end
      else
      begin
        // 1 bit end point
        if ( CpStartCode and TG_TWOBITS ) <> 0 then
        begin
          if ( CpStartCode and CpEndCode ) = 0 then
            CpEndCode := CpStartCode + CpTcc[CpEndCode];
        end
        else
        begin
          CpEndCode := CpEndCode or CpStartCode;
          if CpTcc[CpEndCode] = 1 then
            CpEndCode := CpEndCode or TG_TWOBITS;
        end;
      end;
    end;

    // basic turning point test
    if ( CpEndCode and TG_TWOBITS ) <> 0 then
      AddClippedPoint( CpRegion[CpCra[CpEndCode and not TG_TWOBITS]]);

    // copy the current point as the next starting point
    CpStart.Point := FPoints[idx];
  end;  // for

  Result := iClippedCount;
end;

procedure TGRenderer.Clear;
begin
  Windows.FillRect(Canvas.Handle, Rect(0, 0, Width, Height), ParentGlobe.Brush.Handle)
end;


function TGRenderer.Clone: TGRenderer;
begin
  Result := TGRendererClass(Self.ClassType).Create( ParentGlobe );
  Result.Assign(Self);
end;

constructor TGRenderer.Create( ParentGlobe : TGCustomGlobe5 );
begin
  inherited;

  FBrush := TGBrush.Create;
  FFont := TGFont.Create;
  FPen := TGPen.Create;
  FSelectedBrush := TGBrush.Create;
  FSelectedPen := TGPen.Create;

  FSelectedBrush.Define(clYellow, clYellow, gbsSolid);
  FSelectedPen.Define(clRed, gpsSolid, 1, guPixel);

  FBrush.OnChange := BrushChanged;
  FFont.OnChange := FontChanged;
  FPen.OnChange := PenChanged;

  FMaxTextHeight := 50;
  FMinTextHeight := 5;
  FPixelsPerInch := Screen.PixelsPerInch;

  FDoubleBuffer := True;

  SetLength( FPoints, TG_DEFAULT_MAXPOINTS );
  SetLength( FClippedPoints, TG_DEFAULT_MAXPOINTS );
end;

destructor TGRenderer.Destroy;
begin
  SetLength(FPoints, 0); // Free the working point array
  SetLength(FClippedPoints, 0); // Free the working point array

  FreeAndNil( FBrush );
  FreeAndNil( FFont );
  FreeAndNil( FPen );
  FreeAndNil( FSelectedBrush );
  FreeAndNil( FSelectedPen );
  inherited;
end;

procedure TGRenderer.AddClippedPoly(iPoints: integer; iPenWidth: integer);
var
  len : integer;
begin
  if iPoints > 1 then
    iPoints := ClipPoly(iPoints, FDrawPolyClosed, iPenWidth);

  if iPoints > 1 then
  begin
    len := Length( FPolyPolyPoints );
    SetLength( FPolyPolyPoints, len + 1 );

    FPolyPolyPoints[len] := Copy( FClippedPoints, 0, iPoints );
  end;
end;

procedure TGRenderer.DrawEllipse( rectangle : TRect );
begin
  RendererState([csPenValid, csBrushValid]);

  Canvas.Ellipse(rectangle);
end;

procedure TGRenderer.DrawFocusRect( rectangle : TRect );
begin
  Windows.DrawFocusRect(ParentGlobe.Canvas.Handle, AbsRect( rectangle ));
end;

procedure TGRenderer.DrawPixel(X, Y: integer; color: TColor; alpha: Byte);
begin
  Canvas.Pixels[X,Y] := color;
end;

procedure TGRenderer.DrawPoly( const points : array of TPoint; closed : Boolean );
var
  idx : integer;
begin
  BeginDrawPoly(closed);

  for idx := 0 to High( points ) do
    Self.Points[idx] := points[idx];

  AddClippedPoly(Length( Points ), 1);

  EndDrawPoly;
end;

procedure TGRenderer.DrawRectangle( rectangle : TRect );
begin
  RendererState([csPenValid, csBrushValid]);

  Canvas.Rectangle(rectangle);
end;

procedure TGRenderer.DrawScanLine(Y, x1, x2 : integer; u1, u2, v1, v2: Double; const image: TGBitmap);
begin
end;

procedure TGRenderer.DrawText( X, Y : integer; const text : String; alignment : TGTitleAlignment; offset : integer );
begin
  DrawTextRect( GetTextRect( X, Y, text, alignment, offset ), text );
end;

procedure TGRenderer.DrawTextRect(const aRect: TRect; const text: String);
begin
  Canvas.TextRect( aRect, aRect.Left, aRect.Top, text );
end;

procedure TGRenderer.DrawTextureTriangle(const srcTri, destTri: TGTriangle; const image: TGBitmap);
var
  idx, top, a, b, Y : integer;
  Height_A, Height_B : integer;
  xA, xB, uA, uB, vA, vB : Extended;
  dx_A, du_A, dv_A, dx_B, du_B, dv_B : Extended;

  {---------------------------------------------------}
  procedure InterpolantsA(iA, iB : integer);
  begin
    xA := destTri[iB].X;
    uA := srcTri[iB].X;
    vA := srcTri[iB].Y;
    if Height_A > 0 then
    begin
      dx_A := (destTri[iA].X - xA) / Height_A;
      du_A := (srcTri[iA].X - uA) / Height_A;
      dv_A := (srcTri[iA].Y - vA) / Height_A;
    end;
  end;

  {---------------------------------------------------}
  procedure InterpolantsB(iA, iB : integer);
  begin
    xB := destTri[iB].X;
    uB := srcTri[iB].X;
    vB := srcTri[iB].Y;
    if Height_B > 0 then
    begin
      dx_B := (destTri[iA].X - xB) / Height_B;
      du_B := (srcTri[iA].X - uB) / Height_B;
      dv_B := (srcTri[iA].Y - vB) / Height_B;
    end;
  end;
begin
  Points[0] := destTri[0];
  Points[1] := destTri[1];
  Points[2] := destTri[2];

  if not XYListVisible(Points, 3) then
    Exit;

  // find the topmost point of the triangle
  top := 0;
  if destTri[1].Y < destTri[0].Y then
    top := 1;
  if destTri[2].Y < destTri[top].Y then
    top := 2;

  Y := destTri[top].Y;
  a := ((top + 1) mod 3);
  b := ((top + 2) mod 3);

  Height_A := destTri[a].Y - Y;
  Height_B := destTri[b].Y - Y;

  InterpolantsA(a, top);
  InterpolantsB(b, top);

  try
    idx := 2;
    while idx > 0 do
    begin
      while (Height_A <> 0) and (Height_B <> 0) do
      begin
        if (Y >= 0) and (Y < Height) then
          DrawScanLine(Y, Trunc(xA), Trunc(xB), uA, uB, vA, vB, image);
        Inc(Y);
        Dec(Height_A);
        Dec(Height_B);
        xA := xA + dx_A; // add the interpolants
        xB := xB + dx_B;
        uA := uA + du_A;
        uB := uB + du_B;
        vA := vA + dv_A;
        vB := vB + dv_B;
      end;

      if Height_A = 0 then
      begin
        Height_A := destTri[b].y - destTri[a].y;
        // recalculate the interpolants for the new edge
        InterpolantsA(b, a);
        Dec(idx); // one less vertex
        a := b;
      end;

      if Height_B = 0 then
      begin
        Height_B := destTri[a].y - destTri[b].y;
        // recalculate the interpolants for the new edge
        InterpolantsB(a, b);
        Dec(idx); // one less vertex
        b := a;
      end;
    end;
  except
    // ignore exceptions.
  end;
end;

function TGRenderer.FontRenderSize(FontSize: integer; FontUnits: TGUnitTypes): integer;
begin
  if FontUnits = guPixel then
    Result := MulDiv(FontSize, PixelsPerInch, Screen.PixelsPerInch)
  else
    Result := Round(FontSize * TGlobe5(ParentGlobe).GlobeUnitsFrom(1, TGUnitTypes(FontUnits)) * ParentGlobe.Projector.ScaleFactor);
end;

function TGRenderer.GetAltitude: Cardinal;
begin
  if FAltitude <> 0 then
    Result := FAltitude
  else
    Result := ParentGlobe.Projector.Altitude;
end;

function TGRenderer.GetMaxTextSize: Integer;
begin
  Result := MulDiv(FMaxTextHeight, PixelsPerInch, Screen.PixelsPerInch);
end;

function TGRenderer.GetMinTextSize: Integer;
begin
  Result := MulDiv(FMinTextHeight, PixelsPerInch, Screen.PixelsPerInch);
end;

function TGRenderer.GetTextRect(X, Y : integer; const text : String; alignment : TGTitleAlignment; offset : integer ): TRect;
var
  iW, iH : integer;
  eSin, eCos : Extended;
begin
  RendererState([csFontValid]);

  Result := Rect( X, Y, X + 1, Y + 1);
  Windows.DrawText(Canvas.Handle, PChar(text), Length(text), Result, DT_CALCRECT + DT_NOCLIP + DT_NOPREFIX);

  iW := Result.Right - Result.Left;
  iH := Result.Bottom - Result.Top;

  case alignment of
    taTopLeft :    OffsetRect(Result, -(iW + offset), -(iH + offset));
    taLeft :       OffsetRect(Result, -(iW + offset), -iH div 2 );
    taBottomLeft : OffsetRect(Result, -(iW + offset), offset );
    taTop :        OffsetRect(Result, -iW div 2, -(iH + offset));
    taCenter :     OffsetRect(Result, -iW div 2, -iH div 2 );
    taBottom :     OffsetRect(Result, -iW div 2, offset );
    taTopRight :   OffsetRect(Result, offset, -(iH + offset));
    taRight :      OffsetRect(Result, offset, -iH div 2 );
    taBottomRight: OffsetRect(Result, offset, offset );
  end;

  // Adjust the rectangle center to allow for the rotated text
  if Font.Angle <> 0 then
  begin
    iW := iW div 2;
    iH := iH div 2;

    SinCos(( -Font.Angle / 10 ) * LocalPI / 180.0, eSin, eCos );
    OffsetRect(Result, iW - Trunc( iW * eCos - iH * eSin ), iH - Trunc( iW * eSin + iH * eCos ));
  end;
end;

procedure TGRenderer.LoadFromXML(Element: TGXML_Element);
begin
  inherited;
  //ToDo: TGRenderer.LoadFromXML

end;

procedure TGRenderer.BrushChanged(Sender : TObject);
begin
  if csBrushValid in FState then
  begin
    Exclude(FState, csBrushValid);
    SelectObject(Canvas.Handle, GetStockObject(WHITE_BRUSH));
  end;
end;

procedure TGRenderer.FontChanged(Sender : TObject);
begin
  if csFontValid in FState then
  begin
    Exclude(FState, csFontValid);
    SelectObject(Canvas.Handle, GetStockObject(SYSTEM_FONT));
  end;
end;

procedure TGRenderer.PenChanged(Sender : TObject);
begin
  if csPenValid in FState then
  begin
    Exclude(FState, csPenValid);
    SelectObject(Canvas.Handle, GetStockObject(BLACK_PEN));
  end;
end;

procedure TGRenderer.RealizeAttributes;
begin
  RendererState( [csPenValid, csBrushValid, csFontValid]);
end;

procedure TGRenderer.RefreshDC;
begin
  AlphaBlend := 255;
  Brush := nil;
  Font := nil;
  Pen := nil;
end;

procedure TGRenderer.RendererState(ReqState: TCanvasState);
var
  NeededState: TCanvasState;
begin
  NeededState := ReqState - FState;

  if NeededState <> [] then
  begin
    // Set up the font
    if csFontValid in NeededState then
    begin
      SelectObject( Canvas.Handle, Font.AttributeHandle(Self));
      SetTextColor( Canvas.Handle, ColorToRGB(Font.Color));
      SetTextAlign( Canvas.Handle, TA_TOP + TA_LEFT);

      SetBkMode( Canvas.Handle, TRANSPARENT);
    end;

    // Set up the pen
    if csPenValid in NeededState then
    begin
      SelectObject( Canvas.Handle, Pen.AttributeHandle(Self));
    end;

    // Set up the brush
    if csBrushValid in NeededState then
    begin
//      if Brush.Style > gbsSolid then  // set the brush origin for patterned brushes
//        with TGlobe5(ParentGlobe).Projector do
//          SetBrushOrgEx( Canvas.Handle, XOrigin mod 8, YOrigin mod 8, nil );

      SelectObject( Canvas.Handle, Brush.AttributeHandle(Self));

      if Brush.Style <> gbsClear then
      begin
        if Brush.BkColor = Brush.Color then
          SetBkMode( Canvas.Handle, TRANSPARENT)
        else
        begin
          SetBkMode( Canvas.Handle, OPAQUE);
          SetBkColor( Canvas.Handle, ColorToRGB( Brush.BkColor ));
        end;

        if Brush.Style > gbsDiagCross then  // For bitmap brushes
        begin
          SetBkColor( Canvas.Handle, ColorToRGB( Brush.BkColor ));
          SetTextColor( Canvas.Handle, ColorToRGB( Brush.Color ));
        end;
      end;
    end;
    FState := FState + NeededState;
  end;
end;

procedure TGRenderer.SaveToXML(Element : TGXML_Element);
begin
  inherited;
  //ToDo: TGRenderer.SaveToXML
end;

procedure TGRenderer.SetDoubleBuffer(const Value: Boolean);
begin
  FDoubleBuffer := Value;
end;

procedure TGRenderer.SetHeight(Value: integer);
begin
  SetSize( Width, Value );
end;

procedure TGRenderer.SetBrush(const Value: TGBrush);
begin
  if Value = nil then
  begin
    SelectObject( Canvas.Handle, GetStockObject(WHITE_BRUSH));
    Exclude( FState, csBrushValid );
    FBrush.Define( clWhite, clBlack, gbsSolid );
  end
  else
  begin
    if FBrush.HashValue <> Value.HashValue then
    begin
      FBrush.Assign( Value ); // Copy over the properties
      FBrush.HashValue := Value.HashValue;
    end;
  end;
end;

procedure TGRenderer.SetFont(const Value: TGFont);
begin
  if Value = nil then
  begin
    SelectObject( Canvas.Handle, GetStockObject(SYSTEM_FONT));
    Exclude( FState, csFontValid );
    FFont.Define( 'Arial', clWhite, clBlack, 9, guPixel, 0, [], 255 );
  end
  else
  begin
    if FFont.HashValue <> Value.HashValue then
    begin
      FFont.Assign( Value ); // Copy over the properties
      FFont.HashValue := Value.HashValue;
    end;
  end;
end;

procedure TGRenderer.SetPen(const Value: TGPen);
begin
  if Value = nil then
  begin
    SelectObject( Canvas.Handle, GetStockObject(BLACK_PEN));
    Exclude( FState, csPenValid );
    FPen.Define(clBlack, gpsSolid, 1, guPixel);
  end
  else
  begin
    if FPen.HashValue <> Value.HashValue then
    begin
      FPen.Assign( Value ); // Copy over the properties
      FPen.HashValue := Value.HashValue;
    end;
  end;
end;

procedure TGRenderer.SetSelectedBrush(const Value: TGBrush);
begin
  FSelectedBrush := Value;
end;

procedure TGRenderer.SetSelectedPen(const Value: TGPen);
begin
  FSelectedPen := Value;
end;

procedure TGRenderer.SetSize(newWidth, newHeight : integer);
begin
  FWidth := newWidth;
  FHeight := newHeight;
  UpdateClipRect( 1 );
  FState := [];
end;

procedure TGRenderer.SetWidth(Value: integer);
begin
  SetSize( Value, Height );
end;

function TGRenderer.TriangleLLVisible(const ptLL1, ptLL2, ptLL3 : TGPointLL) : Boolean;
begin
  Result := false;

  with ParentGlobe.Projector.ProjectionModel do
    if PointLLToXY( ptLL1, 0) then
      if PointLLToXY( ptLL2, 1) then
        if PointLLToXY( ptLL3, 2) then
          Result := XYListVisible( Points, 3 );
end;

function TGRenderer.XYListVisible(Points : TPointArray; iCount : Integer) : Boolean;
var
  idx, iLeft, iTop, iRight, iBottom : Integer;
  A, B, C, D, TL, BR : Byte;
begin
  Result := False;

  if iCount > 1 then
  begin
    iLeft := Maxint;
    iTop := Maxint;
    iRight := -Maxint;
    iBottom := -Maxint;
    for idx := 0 to iCount - 1 do
      with Points[idx] do
      begin
        if X < iLeft then iLeft := X;
        if X > iRight then iRight := X;
        if Y < iTop then iTop := Y;
        if Y > iBottom then iBottom := Y;
      end;

    A := (Ord(iTop < 0) shl 2) or (Ord(iTop > FHeight) shl 3);
    B := Ord(iLeft < 0) or (Ord(iLeft > FWidth) shl 1);

    C := (Ord(iBottom < 0) shl 2) or (Ord(iBottom > FHeight) shl 3);
    D := Ord(iRight < 0) or (Ord(iRight > FWidth) shl 1);

    TL := A or B;
    BR := C or D;

    if (TL and BR) <> 0 then
      Result := not ((A = 8) or (B = 2) or (C = 4) or (D = 1))
    else
      Result := True;
  end;
end;


constructor TGComponent.Create(parentGlobe: TGCustomGlobe5);
begin
  inherited Create;
  FParentGlobe := parentGlobe;
end;

class function TGComponent.CreateFromXML(parentGlobe: TGCustomGlobe5; Element: TGXML_Element;
  defaultClass : TGComponentClass): TGComponent;
var
  sClassName : String;
begin
  Result := nil;

  if Element <> nil then
  begin
    sClassName := Element.Attribute('Class', '');
    // Create the Layer Object
    if sClassName <> '' then
    try
      Result := TGComponentClass(FindClass(sClassName)).Create( parentGlobe );
    except
      // Ignore Exceptions
      Result := nil;
    end;
  end;

  // Build a default object instead
  if ( Result = nil ) and ( defaultClass <> nil ) then
    Result := defaultClass.Create(ParentGlobe);

  if Result <> nil then
    Result.LoadFromXML( Element );
end;

function TGComponent.GetXML: String;
var
  doc : TGXML_Document;
begin
  doc := TGXML_Document.Create('Component');

  SaveToXML( doc.Document );
  Result := doc.AsString;

  doc.Free;
end;

procedure TGComponent.LoadFromXML(Element: TGXML_Element);
begin
  // Does nothing
end;

procedure TGComponent.SaveToXML(Element: TGXML_Element);
begin
  Element.AddAttribute('Class', ClassName, '');
end;

procedure TGComponent.SetXML(const Value: String);
var
  doc : TGXML_Document;
begin
  doc := TGXML_Document.Create('Component');

  doc.AsString := Value;
  LoadFromXML( doc.Document );
  doc.Free;
end;

function TGIterator.Count: integer;
begin
  Result := 0;
  if First <> nil then
  repeat
    Inc( Result );
  until Next = nil;
end;

constructor TGIterator.Create(objSource: TGObjectSource);
begin
  FObjectSource := objSource;
end;

function TGIterator.First: IGMapPoint;
begin
  FCurrentPK := ObjectSource.First;
  Result := ObjectSource.ByPrimaryKey(FCurrentPK);
end;

function TGIterator.Last: IGMapPoint;
begin
  Result := nil;
end;

function TGIterator.Next: IGMapPoint;
begin
  FCurrentPK := ObjectSource.Next(FCurrentPK);
  Result := ObjectSource.ByPrimaryKey(FCurrentPK);
end;

function TGIterator.Prior: IGMapPoint;
begin
  Result := nil;
end;


procedure TGListIterator.AddPrimaryKey( objPK: TGPrimaryKey);
begin
  FPrimaryKeyList.Add(Pointer(objPK));
end;

function TGListIterator.Contains( objPK : TGPrimaryKey ) : Boolean;
begin
  Result := FPrimaryKeyList.IndexOf(Pointer( objPK )) >= 0;
end;

function TGListIterator.Count: integer;
begin
  Result := FPrimaryKeyList.Count;
end;

constructor TGListIterator.Create( objSource : TGObjectSource );
begin
  inherited;

  FPrimaryKeyList := TList.Create;
end;

destructor TGListIterator.Destroy;
begin
  FreeAndNil( FPrimaryKeyList );
  inherited;
end;

function TGListIterator.First: IGMapPoint;
begin
  FPosition := 0;
  Result := Next;
end;

function TGListIterator.Last: IGMapPoint;
begin
  FPosition := FPrimaryKeyList.Count - 1;
  Result := Prior;
end;

function TGListIterator.Next: IGMapPoint;
begin
  if FPosition < FPrimaryKeyList.Count then
  begin
    Result := ObjectSource.ByPrimaryKey( TGPrimaryKey( FPrimaryKeyList[FPosition] ));
    Inc( FPosition );
  end
  else
    Result := nil;
end;

function TGListIterator.Prior: IGMapPoint;
begin
  if FPosition >= 0 then
  begin
    Result := ObjectSource.ByPrimaryKey( TGPrimaryKey( FPrimaryKeyList[FPosition] ));
    Dec( FPosition );
  end
  else
    Result := nil;
end;

{ TGLayerSelection }

procedure TGLayerSelection.Add(key: TGPrimaryKey);
var
  idx : integer;
begin
  if Contains( key ) then // stop duplicate additions
    Exit;

  idx := Hash( key );

  if FBuckets[idx] = nil then
    FBuckets[idx] := TList.Create;

  FBuckets[idx].Add(Pointer( key ));  // add the key into the hash
  Inc( FCount );
end;

function TGLayerSelection.Clear: Boolean;
var
  idx : integer;
begin
  Result := FCount > 0;

  if Result then
  begin
    for idx:= 0 to High( FBuckets ) do
      FreeAndNil( FBuckets[idx] );
    FCount := 0;
  end;
end;

function TGLayerSelection.Contains(key: TGPrimaryKey): Boolean;
var
  idx : integer;
begin
  Result := false;

  idx := Hash( key );

  if FBuckets[idx] <> nil then
    Result := FBuckets[idx].IndexOf(Pointer( key )) >= 0;
end;

function TGLayerSelection.Count: integer;
begin
  Result := FCount;
end;

constructor TGLayerSelection.Create(parentLayer: TGLayer);
begin
  SetLength( FBuckets, 67 );

  FCount := 0;
  FParentLayer := parentLayer;
  FPosition := 0;
end;

destructor TGLayerSelection.Destroy;
begin
  Clear;

  inherited;
end;

function TGLayerSelection.First: IGMapPoint;
begin
  FPosition := 0;
  Result := Next;
end;

function TGLayerSelection.GetKey(index: integer): TGPrimaryKey;
var
  idx : integer;
begin
  for idx:= 0 to High( FBuckets ) do
    if FBuckets[idx] <> nil then
    begin
      Dec( index, FBuckets[idx].Count );
      if index < 0 then
      begin
        Result := TGPrimaryKey( FBuckets[idx][FBuckets[idx].Count + index] );
        Exit;
      end;
    end;
  Result := 0;
end;

function TGLayerSelection.Hash(key: TGPrimaryKey): integer;
begin
  Result := key mod Cardinal(Length( FBuckets ));
end;

function TGLayerSelection.Last: IGMapPoint;
begin
  FPosition := Count;
  Result := Prior;
end;

function TGLayerSelection.Next: IGMapPoint;
begin
  if FPosition < Count then
  begin
    Result := FParentLayer.ObjectSource.ByPrimaryKey( GetKey(FPosition) );
    Inc( FPosition );
  end
  else
    Result := nil;
end;

function TGLayerSelection.Prior: IGMapPoint;
begin
  if FPosition > 0 then
  begin
    Dec( FPosition );
    Result := FParentLayer.ObjectSource.ByPrimaryKey( GetKey(FPosition) );
  end
  else
    Result := nil;
end;

procedure TGLayerSelection.Remove(key: TGPrimaryKey);
var
  idx : integer;
begin
  idx := Hash( key );

  if FBuckets[idx] <> nil then
    if FBuckets[idx].Remove(Pointer( key )) >= 0 then
      Dec( FCount );
end;

{ TGGlobeSelection }

procedure TGGlobeSelection.BuildLayerStack(aLayer: TGLayer);
var
  idx : integer;
begin
  FLayerStack.Add( Pointer(aLayer) );

  for idx := 0 to aLayer.LayerStore.Count - 1 do
    BuildLayerStack( aLayer.LayerStore[idx] );
end;

function TGGlobeSelection.Clear: Boolean;
begin
  Result := TGlobe5(FParentGlobe).GlobeLayer.ClearSelected;
end;

function TGGlobeSelection.Count: integer;
  function SubLayerCount( aLayer : TGLayer ) : integer;
  var
    idx : integer;
  begin
    Result := aLayer.Selection.Count;
    for idx := 0 to aLayer.LayerStore.Count - 1 do
      Inc( Result, SubLayerCount( aLayer.LayerStore[idx] ));
  end;
begin
  Result := SubLayerCount( TGlobe5(FParentGlobe).GlobeLayer );
end;

constructor TGGlobeSelection.Create(parentGlobe: TGCustomGlobe5);
begin
  FLayerStack := TList.Create;
  FParentGlobe := parentGlobe;
end;

destructor TGGlobeSelection.Destroy;
begin
  FreeAndNil( FLayerStack );
  inherited;
end;

function TGGlobeSelection.First: IGMapPoint;
begin
  FLayerStack.Clear;
  BuildLayerStack( TGlobe5(FParentGlobe).GlobeLayer );

  FPosition := 0;
  FStartLayer := true;
  Result := Next;
end;

function TGGlobeSelection.Last: IGMapPoint;
begin
  FLayerStack.Clear;
  BuildLayerStack( TGlobe5(FParentGlobe).GlobeLayer );

  FPosition := FLayerStack.Count - 1;
  FStartLayer := true;
  Result := Prior;
end;

function TGGlobeSelection.Next: IGMapPoint;
begin
  while FPosition < FLayerStack.Count do
  begin
    if FStartLayer then
      Result := TGLayer( FLayerStack[FPosition] ).Selection.First
    else
      Result := TGLayer( FLayerStack[FPosition] ).Selection.Next;

    FStartLayer := false;

    if Result <> nil then
      Exit;

    Inc( FPosition );
    FStartLayer := true;
  end;
  Result := nil;
end;

function TGGlobeSelection.Prior: IGMapPoint;
begin
  while FPosition > 0 do
  begin
    if FStartLayer then
      Result := TGLayer( FLayerStack[FPosition] ).Selection.Last
    else
      Result := TGLayer( FLayerStack[FPosition] ).Selection.Prior;

    FStartLayer := false;

    if Result <> nil then
      Exit;

    Dec( FPosition );
    FStartLayer := true;
  end;
  Result := nil;
end;


initialization
  RegisterGlobeClass( TGPen, 'Globe Pen' );
  RegisterGlobeClass( TGBrush, 'Globe Brush' );
  RegisterGlobeClass( TGFont, 'Globe Font' );
  RegisterGlobeClass( TGLayer, 'Layer' );
end.

