//------------------------------------------------------------------------------
//Summary
//  TGlobe System Utility Unit
//
//Description
//Provides general constants, utility functions and classes for TGlobe
//
//Author
//Graham Knight (tglobe@tglobe.com)
//------------------------------------------------------------------------------
{$I GLOBE5.INC}

{$IOCHECKS OFF}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

unit GSysUtils;

interface

uses ActiveX, Windows, Classes, Graphics, SysUtils, GXML;

(*$HPPEMIT 'static const Extended NAN = 0.0 / 0.0;' *)
(*$HPPEMIT 'static const Extended INF = 1.0 / 0.0;' *)

const
  Infinity    =  1.0 / 0.0;
  NegInfinity = -1.0 / 0.0;
  NaN = 0.0 / 0.0;

  LocalPI = 3.14159265358979323846;
  DoublePI = (LocalPi * 2.0);
  HalfPI = (LocalPI / 2.0);
  QuarterPi = (LocalPI / 4.0);
  MinDouble   =  5.0e-324;
  MaxDouble   =  1.7e+308;
  MinExtended =  3.4e-4932;
  MaxExtended =  1.1e+4932;


  TG_MAXSCALEFACTOR = 1.0;
  TG_MINSCALEFACTOR = 0.00000005;


  GU_RESOLUTION = (360 * 60 * 60 * 1500);
  GU_DEGREE = (GU_RESOLUTION div 360);
  GU_MINUTE = (GU_DEGREE div 60);
  GU_MINUTE_THOUSANTH = (GU_DEGREE div 60000);
  GU_SECOND = (GU_DEGREE div 3600);
  GU_THIRD = (GU_DEGREE div 21600);
  GU_HUNDRETH = (GU_DEGREE div 360000);
  GU_THOUSANTH = (GU_DEGREE div 3600000);

  GU_KILOMETER = (GU_MINUTE / 1.853184);
  GU_METER = (GU_MINUTE / 1852.0);
  GU_CENTIMETER = (GU_MINUTE / 185318.4);

  GU_NAUTICALMILE = GU_MINUTE;
  GU_MILE = (GU_MINUTE / 1.15151515);
  GU_YARD = (GU_MILE / 1760);
  GU_FATHOM = (GU_YARD * 2);
  GU_FOOT = (GU_YARD / 3);
  GU_INCH = (GU_FOOT / 12);

  GU_Conversions: array[0..16] of Double = (
    GU_DEGREE, GU_MINUTE, GU_NAUTICALMILE, GU_KILOMETER, GU_MILE,
    GU_SECOND, GU_THIRD, GU_HUNDRETH, GU_THOUSANTH,
    GU_METER, GU_CENTIMETER, GU_FATHOM, GU_YARD, GU_FOOT, GU_INCH, 1, 0);


//  GU_EARTHRADIUS = (3437.747 * GU_NAUTICALMILE);
  GU_EARTHRADIUS = (6378137 * GU_METER);
  GU_EARTHCIRCUM = (40075016.685578488 * GU_METER);

  GU_10_DEGREE = (10 * GU_DEGREE);
  GU_15_DEGREE = (15 * GU_DEGREE);
  GU_30_DEGREE = (30 * GU_DEGREE);
  GU_90_DEGREE = (90 * GU_DEGREE);
  GU_180_DEGREE = (180 * GU_DEGREE);
  GU_360_DEGREE = (360 * GU_DEGREE);


  GU_TORADIANS = (LocalPI / GU_180_DEGREE);
  DD_TORADIANS = (LocalPI / 180.0);

  GU_FROMRADIANS = (GU_180_DEGREE / LocalPI);
  DD_FROMRADIANS = (180.0 / LocalPI);

  TG_FOV_ANGLE = 45.0;
  TG_EPSILON = 1E-15;

type
{$IFDEF VER130}
  IInterface = IUnknown;
  IntegerArray = array [0..3] of integer;
  PIntegerArray = ^IntegerArray;
{$ENDIF}

{$IFDEF BCB}
  TGPrimaryKey = Cardinal;
  TGDecimalDegree = Double;
{$ELSE}
  TGPrimaryKey = type Cardinal;
  TGDecimalDegree = type Extended;
{$ENDIF}

  EGException = class(Exception);

  TGProgressType = ( pProgressStart, pProgressEnd, pProgressError, pImporting, pExporting, pIndexing, pCompacting, pProgress );
  TGProgressEvent = procedure( Sender: TObject; progressType : TGProgressType; count : integer; percentage : double ) of object;
  TGSearchCallback = procedure( rowID : TGPrimaryKey; var bAbort : boolean ) of object;

  TGSpheroid = (Airy1830, Australian1965, Bessel1841,
    Clarke1866, Clarke1880, Everest1830, GRS1967, GRS1980,
    Helmert1906, Hough, International1920, Krassovsky1940,
    SouthAmerican1969, WGS60, WGS66, WGS72, WGS84);

  TGMERHitZone = ( hzNone, hzHit, hzPlus180, hzMinus180 );

  TGSpheroidData = packed record
    r: Integer;
    f: Double;
  end;

  TGPointLL = packed record
    iLongX, iLatY, iHeightZ: Integer;
  end;

  TGPointDD = packed record
    LongX, LatY, HeightZ: TGDecimalDegree;
  end;

  TGPoint3D = packed record
    X, Y, Z: Extended;
  end;
  TGPoint3DArray = array of TGPoint3D;

  TGMER = packed record
    iLongX, iLatY, iWidthX, iHeightY : integer;
  end;

  TGVec = packed record
    X, Y : Extended;
  end;

  TGTriangle = array[0..2] of TPoint;
  TGTriangleLL = array[0..2] of TGPointLL;

  TGTextureTriangle = packed record
    worldTri : TGTriangleLL;
    srcTri : TGTriangle;
    destTri : TGTriangle;
  end;

  TGMatrix = packed record
    terms : array[0..15] of Double;
  end;

  TGVec3D = packed record
    X, Y, Z: Extended;
  end;


  TGQuaternion = packed record
    X, Y, Z, W: Double;
  end;

  TGTriState = ( tsUnknown, tsFalse, tsTrue );

  TGUnitTypes = (guDegree, guMinute, guNauticalMile, guKilometer, guMile,
    guSecond, guThird, guHundreth, guThousanth, guMeter, guCentimeter, guFathom,
    guYard, guFeet, guInch, guGlobeUnit, guPixel);


  TGPOZ = packed record
    eSF: Extended;
    CenterXY: TPoint;
    iRotX, iRotY, iRotZ: Integer;
    iWidth, iHeight: Integer;
  end;

  TGMapObjectState = ( osClosed, osHidden );
  TGMapObjectStateSet = set of TGMapObjectState;


  TGRenderState = ( rsClosed, rsSelected, rsFace, rsBackFace );
  TGRenderStateSet = set of TGRenderState;


  TGNoRefCountObject = class( TObject, IUnknown )
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;


  TGRoot = class(TPersistent)
  end;
  TGRootClass = class of TGRoot;


  TGENVPersistent = class(TGRoot)
  public
    procedure LoadFromXML(Element : TGXML_Element); virtual; abstract;
    procedure SaveToXML(Element : TGXML_Element); virtual; abstract;
  end;


  TGInterfacedPersistent = class(TGRoot, IInterface)
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;


  TGBufferStream = class(TCustomMemoryStream)
  public
    constructor Create(buffer : Pointer; len : Cardinal);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;


  TGStreamReader = class(TGRoot)
  private
    FDataStream : TStream;
  protected
    procedure _Read(var Buffer; iSize : integer); virtual;
  public
    constructor Create(aStream : TStream);
    destructor Destroy; override;

    procedure ReadBuffer(var Buffer; iSize : integer);
    function ReadBoolean : Boolean;
    function ReadByte : Byte;
    function ReadDouble : Double;
    function ReadWord : Word;
    function ReadShortInt : ShortInt;
    function ReadSmallInt : SmallInt;
    function ReadInteger : integer;
    function ReadLongString : String;
    function ReadShortString : ShortString;
    function ReadString : String;
    function ReadPoint : TPoint;
    function ReadPointLL(bHeight, b16Bit : Boolean) : TGPointLL;
    function ReadRect : TRect;
    function ReadMER : TGMER;
    property DataStream : TStream read FDataStream write FDataStream;
  end;


  TGStreamWriter = class(TGRoot)
  private
    FDataStream : TStream;
  protected
    procedure _Write(const Buffer; iSize : integer); virtual;
  public
    constructor Create(aStream : TStream);
    destructor Destroy; override;

    procedure WriteBuffer(const Buffer; iSize : integer);
    procedure WriteBoolean(bVal : Boolean);
    procedure WriteByte(Val : Byte);
    procedure WriteDouble(Val : Double);
    procedure WriteWord(iVal : Word);
    procedure WriteShortInt(iVal : ShortInt);
    procedure WriteSmallInt(iVal : SmallInt);
    procedure WriteInteger(iVal : Integer);
    procedure WriteLongString(const sValue : String);
    procedure WriteShortString(const sValue : ShortString);
    procedure WriteString(const sValue : String);
    procedure WriteRect(const ARect : TRect);
    procedure WritePoint(const pt : TPoint);
    procedure WritePointLL(const ptLL : TGPointLL; bHeight, b16Bit : Boolean);
    procedure WriteMER(const MER : TGMER);

    property DataStream : TStream read FDataStream write FDataStream;
  end;



  TGDynArrayRecord = record
    Count: integer;
    Itemsize: integer;
    RefCount: Word;
    Capacity: Word;
  end;
  TGDynArray = ^TGDynArrayRecord;
  PGDynArray = ^TGDynArray;

  TGPointStoreFlags = (psLongCount, ps16Bit, psCompressed, psHeight, psHidden);
  TGPointStoreFlagsSet = set of TGPointStoreFlags;
  PTGPointStore = ^TGPointStore;

  
  TGPointStore = class(TGRoot)
  private
    FRefreshPending : Boolean;
    function GetCount: integer;
    function GetHeightFlag: Boolean;
    function GetHidden: Boolean;
    procedure SetCount(iNewCount: Integer);
    procedure SetHidden(const Value: Boolean);
    procedure SetHeightFlag(Value: Boolean);
  protected
    FPointArray: TGDynArray;
    FPoint3DArray: TGPoint3DArray;
    function GetLL(iIndex: integer): TGPointLL;
    function GetDD(iIndex: integer): TGPointDD;
    procedure PutDD(iIndex: integer; const pt: TGPointDD);
    procedure PutLL(iIndex: integer; const pt: TGPointLL);
  public
    Flags: TGPointStoreFlagsSet;

    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function Clone: TGPointStore;
    procedure Insert(iIndex: Integer; const ptLL: TGPointLL);
    procedure Delete(iIndex: Integer);
    function Add( const ptLL: TGPointLL ): integer;
    function AddDD( const Long, Lat : TGDecimalDegree ): integer;

    function PointStoreMER: TGMER;
    function IntersectMER( const aMER : TGMER ) : Boolean;
    function ObjectSize: integer;
    function PointInPolygon(iLong, iLat : integer; hitZone : TGMERHitZone ): Boolean;
    function PointOnEdge(iLong, iLat, iTolerance: Integer; bClosed : Boolean): integer;
    function Centroid(var Area: Extended): TGPointLL;
    procedure Translate(dx, dy, dz: integer);

    procedure ReadPoints( reader: TGStreamReader );
    procedure WritePoints( writer: TGStreamWriter );
    procedure SaveToXML( el : TGXML_ELement );
    procedure LoadFromXML( el : TGXML_ELement );

    function Point3DArray: TGPoint3DArray;

    property AsDD[iIndex: Integer]: TGPointDD read GetDD write PutDD;
    property AsLL[iIndex: Integer]: TGPointLL read GetLL write PutLL; default;
  published
    property Count: Integer read GetCount write SetCount;
    property Hidden : Boolean read GetHidden write SetHidden;
    property StoreHeight: Boolean read GetHeightFlag write SetHeightFlag;
  end;
  TGPointStoreClass = class of TGPointStore;



  TGChainStoreFlag = (cfHeight,cfMultiRing);
  TGChainStoreFlagSet = set of TGChainStoreFlag;

  TGChainStore = class(TGRoot)
  private
    FChains: TGDynArray;
    FStoreHeight: Boolean;
    function GetChain(iIndex: integer): TGPointStore;
    function GetCount: integer;
    procedure SetCount(iNewCount: integer);
    procedure SetChain(iIndex: integer; Value: TGPointStore);
    procedure SetStoreHeight(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Insert(iIndex: Integer; PointStore: TGPointStore);
    procedure Delete(iIndex: Integer);
    procedure Add(PointStore: TGPointStore);
    procedure Move(iFrom, iTo: integer);
    function Clone: TGChainStore;
    function Centroid: TGPointLL;
    function ChainStoreMER: TGMER;
    function IntersectMER( const aMER : TGMER ) : Boolean;
    function ObjectSize: integer;
    function Last : TGPointStore;

    procedure ReadChains( reader: TGStreamReader; chainFlags : TGChainStoreFlagSet );
    procedure WriteChains( writer: TGStreamWriter );
    procedure ReadFromStream( aStream: TStream );
    procedure WriteToStream( aStream: TStream );

    procedure SaveToXML( el : TGXML_ELement );
    procedure LoadFromXML( el : TGXML_ELement );


    property Chain[iIndex: integer]: TGPointStore read GetChain write SetChain; default;
  published
    
    property Count: Integer read GetCount write SetCount;
    property StoreHeight: Boolean read FStoreHeight write SetStoreHeight;
  end;
  TGChainStoreClass = class of TGChainStore;



function DynArrayCreate(const iItemSize, iCount: integer): TGDynArray;
function DynArrayClone(const aDynArray: TGDynArray): TGDynArray;
function DynArraySizeOf(const aDynArray: TGDynArray) : integer;
procedure DynArrayInsert(var aDynArray: TGDynArray; iIndex: integer);
procedure DynArrayDelete(var aDynArray: TGDynArray; iIndex: integer);
procedure DynArrayMove(var aDynArray: TGDynArray; iFrom, iTo: integer);
procedure DynArrayFree(var aDynArray: TGDynArray);
procedure DynArraySetLength(var aDynArray: TGDynArray; iCount: integer);

function DynArrayPtr(const aDynArray: TGDynArray; iIndex: integer): Pointer;
function DynArrayCount(const aDynArray: TGDynArray): integer;
function DynArrayAsObject(const aDynArray: TGDynArray; iIndex: integer): TObject;
function DynArrayAsInteger(const aDynArray: TGDynArray; iIndex: integer): integer;
function DynArrayIndexOfObject(const aDynArray: TGDynArray; Obj: TObject): integer;
function DynArraySetAsObject(const aDynArray: TGDynArray; iIndex: integer; Obj: TObject): TObject;
function DynArraySetAsInteger(const aDynArray: TGDynArray; iIndex: integer; Value: integer): integer;
procedure DynArrayIncReference(const aDynArray: TGDynArray);
procedure DynArrayDecReference(const aDynArray: TGDynArray);


function PointLL(iLongX, iLatY: integer): TGPointLL;
function PointLLH(iLongX, iLatY, iHeightZ: integer): TGPointLL;
function PointLLToStr( const ptLL : TGPointLL ): String;
function PointToStr( const pt : TPoint ): String;
function PointLLToDD( const ptLL : TGPointLL ) : TGPointDD;
function PointDDToStr( const ptDD : TGPointDD ): String;
function DDToStr( const Long: TGDecimalDegree; const Lat: TGDecimalDegree ): String;
function DDToPointLL( const Long: TGDecimalDegree; const Lat: TGDecimalDegree ) : TGPointLL;
function StrToPointDD( const str : String ) : TGPointDD;
function StrToPointLL( const str : String ) : TGPointLL;
function StrToPoint( const str : String ) : TPoint;

function Point3D(ptLL : TGPointLL): TGPoint3D; overload;
function Point3D(Long, Lat, Height: TGDecimalDegree): TGPoint3D; overload;

function GTriangle( const pt0, pt1, pt2 : TPoint ) : TGTriangle; overload;
function GTriangle( x0, y0, x1, y1, x2, y2 : integer ) : TGTriangle; overload;
function GTriangleLL( const ptLL0, ptLL1, ptLL2 : TGPointLL ) : TGTriangleLL;

function GlobeMER : TGMER;
function MER(aLongX, aLatY, aWidthX, aHeightY: integer): TGMER;
function MERFromDD(aLongX, aLatY, aWidthX, aHeightY: TGDecimalDegree): TGMER;
function MERFromLLArray( ptLLArray: array of TGPointLL): TGMER;
function MERToStr(const mer : TGMER) : String;
function MER_iRightX(const mer : TGMER) : integer;
function MER_iTopY(const mer : TGMER) : integer;

function MER_Long(const mer : TGMER) : TGDecimalDegree;
function MER_Lat(const mer : TGMER) : TGDecimalDegree;
function MER_Width(const mer : TGMER) : TGDecimalDegree;
function MER_Height(const mer : TGMER) : TGDecimalDegree;
function MER_Right(const mer : TGMER) : TGDecimalDegree;
function MER_Top(const mer : TGMER) : TGDecimalDegree;

function MER_LongLat(const mer : TGMER) : TGPointLL;
function MER_TopRight(const mer : TGMER) : TGPointLL;
function MER_CenterLL(const mer : TGMER) : TGPointLL;

function MER_Area(const mer: TGMER ): Extended;
function MER_Crosses180(const mer : TGMER) : Boolean;
function MER_Contains(const mer : TGMER; const aMER: TGMER ): Boolean;
function MER_ContainsLL(const mer : TGMER;  const ptLL: TGPointLL ): Boolean;
function MER_ContainsLLEx(const mer : TGMER;  const ptLL: TGPointLL ): TGMERHitZone;
function MER_Inflate(const mer : TGMER; dx, dy : integer ): TGMER;
function MER_Intersect( const srcMER : TGMER; const aMER: TGMER ): Boolean;
function MER_IsEmpty(const mer : TGMER) : Boolean;
function MER_IsEqual(const srcMER : TGMER; const aMER : TGMER ) : Boolean;
function MER_Union(const srcMER : TGMER; const aMER: TGMER ): TGMER;
procedure MER_Empty(var mer : TGMER);
function StrToMER( const str : String ): TGMER;

procedure POZ_SaveToXML( const poz : TGPOZ; Element : TGXML_Element );
procedure POZ_LoadFromXML( var poz : TGPOZ; Element : TGXML_Element );
procedure POZ_Normalise( var poz : TGPOZ; width, height : integer );

procedure Quat_Normalise( var q : TGQuaternion );
procedure Quat_FromAxisAngle(var q : TGQuaternion; const axis: TGVec3D; const angle: Double);
procedure Quat_FromEuler( var q : TGQuaternion; const RotX, RotY, RotZ: TGDecimalDegree);
procedure Quat_FromVectors(var q : TGQuaternion; const vec1, vec2: TGVec3D);
procedure Quat_ToAxisAngle( const q : TGQuaternion; var Axis: TGVec3D; var angle: Double);
procedure Quat_ToMatrix( const q : TGQuaternion; var mat: TGMatrix);
function Quat_Dot(const q1 : TGQuaternion; const q2: TGQuaternion): Double;
function Quat_Multiply(const q1 : TGQuaternion; const q2 : TGQuaternion): TGQuaternion;

function Vec_FromPointLL( ptLL : TGPointLL ): TGVec;
function Vec_FromPoint( pt : TPoint ): TGVec;
function Vec_FromXY( eX, eY : Double ): TGVec;
function Vec_Dot( const v1 : TGVec; const v2 : TGVec ): Double;
function Vec_Length(const v : TGVec): Double;
function Vec_Minus( const v1 : TGVec; const v2 : TGVec ): TGVec;
function Vec_Perpendicular(const v : TGVec): TGVec;
function Vec_Plus( const v1 : TGVec; const v2 : TGVec ): TGVec;
function Vec_Scale( const v: TGVec; s : Double ): TGVec;

procedure Vec3D_Normalise( var v: TGVec3D );
function Vec3D_ToPointLL( const v: TGVec3D ): TGPointLL;
function Vec3D_Cross(const v1: TGVec3D; v2: TGVec3D): TGVec3D;
function Vec3D_Dot(const v1: TGVec3D; v2: TGVec3D): Double;
function Vec3D_Plus(const v1: TGVec3D; v2: TGVec3D): TGVec3D;
function Vec3D_Minus(const v1: TGVec3D; v2: TGVec3D): TGVec3D;
function Vec3D_Multiply(const v1: TGVec3D; v2: TGVec3D): TGVec3D;
function Vec3D_MatrixMultiply(const v: TGVec3D; mat: TGMatrix ): TGVec3D;
function Vec3D_Scale( const v: TGVec3D; s : Double ): TGVec3D;

function PointsToRect(ptArray: array of TPoint): TRect;
function AbsRect( const rect : TRect ): TRect;
function RectToStr( const rect : TRect ) : String;
function StrToRect( const str : String ) : TRect;

function ItemToCSL( const csl : String; const item : String; index : integer ) : String;
function ItemFromCSL( const csl : String; index : integer ) : String;
function sIFE(bCond: Boolean; const sLeft, sRight: String): String;
function StrToExtended(const Text: String): Extended;
function StrToExtendedDef(const Text: String; Default: Extended): Extended;
function ExtendedToStr( Value: Extended ): String;

function PointOnLine(X, Y, ax, ay, bx, by, iTolerance: Integer): Boolean;

function UnitsToStr(Units: TGUnitTypes): String;
function StrToUnits(const sUnits: String): TGUnitTypes;
function StrToGU(sValue: String; const sFmt: String): Integer;
function LLToStr(iLong, iLat: Integer): String;


function DDToGU(const Value: TGDecimalDegree): Integer;
function DDFrom(const Value: Double; Units: TGUnitTypes): TGDecimalDegree;

function GUToStr( iValue: Integer; const sFmt: String): String;
function GUToDD(const Value: Double): TGDecimalDegree;
function GUFrom(const Value: Double; Units: TGUnitTypes): Integer;
function GUTo(const Value: Double; Units: TGUnitTypes): Double;


function V3D(const ptLL : TGPointLL): TGVec3D; overload;
function V3D(const eX, eY, eZ: Double): TGVec3D; overload;
function V3DLerp(const lo, hi: TGVec3D; const alpha: Double): TGVec3D;


function Quat(const X, Y, Z, W: Double): TGQuaternion;
function QuatSlerp(const qLo, qHi: TGQuaternion; const alpha: Double): TGQuaternion;


procedure TransposeMatrix(var FromMat, ToMat: TGMatrix);
procedure ScaleMatrix(var mat: TGMatrix; const SF: Extended);
procedure IdentityMatrix(var mat: TGMatrix);
function PointLLMatrixMul(const mat: TGMatrix; const ptLL: TGPointLL): TGPointLL;


function Tan(const X: Extended): Extended;
function ArcSin(const X: Extended): Extended;
function ArcCos(const X: Extended): Extended;
function ArcTan2(const Y, X: Extended): Extended;
procedure SinCos(const Theta: Extended; var eSin, eCos: Extended);
function Hypot(const X, Y: Extended): Extended;
function SphericalMod(const X: TGDecimalDegree): TGDecimalDegree;
function Sign(const Value: Extended): integer;
function IntPower(const Base: Extended; Exponent: Integer): Extended;
function Power(const Base, Exponent: Extended): Extended;
function LimitFloat(const eValue, eMin, eMax: Double): Double;
function LimitVal(iValue, iMin, iMax: Integer): Integer;
function MinFloat(const eLeft, eRight: Double): Double;
function MaxFloat(const eLeft, eRight: Double): Double;
function MinVal(iLeft, iRight: integer): Integer;
function MaxVal(iLeft, iRight: integer): Integer;
function AngleToRadians(iAngle: Integer): Double;
function RadiansToAngle(const eRad: Double): Integer;
function Cross180(iLong1, iLong2: Integer): Boolean;
function IsNan(const AValue: Double): Boolean;
function IsInfinite(const AValue: Double): Boolean;


function Round180( const Value: Double ): integer;
function Mod180(Value: integer): Integer;
function Mod180Float(const Value: Double): Double;
function LongDiff(iLong1, iLong2: integer ): integer;


function DirectoryExists(const Name: String): Boolean;
procedure RegisterGlobeClass(AClass: TGRootClass; const displayName : String);
function GlobeDisplayName( obj : TObject ): String;

{$IFDEF VER130}
function CreateGUID(out Guid: TGUID): HResult;
function IsEqualGUID(const guid1, guid2: TGUID): Boolean;
function GUIDToString(const GUID: TGUID): string;
function StringToGUID(const S: string): TGUID;

//function Supports(const Instance: IInterface; const IID: TGUID; out Intf): Boolean; overload;
//function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean; overload;
function Supports(const Instance: IInterface; const IID: TGUID): Boolean; overload;
function Supports(const Instance: TObject; const IID: TGUID): Boolean; overload;
function Supports(const AClass: TClass; const IID: TGUID): Boolean; overload;
{$ENDIF}

var
  GlobeClassList: TStringList;
  NULL_GUID: TGUID;

  GUnitStrings : array[0..16] of string = ('D', 'MN', 'NM', 'KM', 'MI',
    'S', 'T', 'H', 'T', 'M', 'CM', 'F', 'YD', 'FT', 'IN', 'GU', 'PI');

  SpheroidData: array[0..Ord(High(TGSpheroid))] of TGSpheroidData = (
    (r: Round((6377563.396 * GU_MINUTE) / 1852.0); f: 1 / 299.3249646), {Airy 1830}
    (r: Round((6378160.0 * GU_MINUTE) / 1852.0); f: 1 / 298.25), {Australian National 1965}
    (r: Round((6377397.155 * GU_MINUTE) / 1852.0); f: 1 / 299.1528128), {Bessel 1841}
    (r: Round((6378206.4 * GU_MINUTE) / 1852.0); f: 1 / 294.9786982), {Clarke 1866}
    (r: Round((6378249.145 * GU_MINUTE) / 1852.0); f: 1 / 293.465), {Clarke 1880}
    (r: Round((6377276.345 * GU_MINUTE) / 1852.0); f: 1 / 300.8017), {Everest 1830}
    (r: Round((6378160.0 * GU_MINUTE) / 1852.0); f: 1 / 298.247167427), {GRS 1967}
    (r: Round((6378137.0 * GU_MINUTE) / 1852.0); f: 1 / 298.257222101), {GRS 1980}
    (r: Round((6378200.0 * GU_MINUTE) / 1852.0); f: 1 / 298.3), {Helmert 1906}
    (r: Round((6378270.0 * GU_MINUTE) / 1852.0); f: 1 / 297.0), {Hough}
    (r: Round((6378388.0 * GU_MINUTE) / 1852.0); f: 1 / 297.0), {International 1920}
    (r: Round((6378245.0 * GU_MINUTE) / 1852.0); f: 1 / 298.3), {Krassovsky 1940}
    (r: Round((6378160.0 * GU_MINUTE) / 1852.0); f: 1 / 298.25), {South American 1969}
    (r: Round((6378165.0 * GU_MINUTE) / 1852.0); f: 1 / 298.3), {WGS-60}
    (r: Round((6378145.0 * GU_MINUTE) / 1852.0); f: 1 / 298.25), {WGS-66}
    (r: Round((6378135.0 * GU_MINUTE) / 1852.0); f: 1 / 298.26), {WGS-72}
    (r: Round((6378137.0 * GU_MINUTE) / 1852.0); f: 1 / 298.257223563) {WGS-84}
    );


implementation

uses GResource;
{$IFDEF VER130}
//function CLSIDFromString(psz: PWideChar; out clsid: TGUID): HResult; stdcall; external 'ole32.dll' name 'CLSIDFromString';
//function CoCreateGuid(out guid: TGUID): HResult; stdcall; external 'ole32.dll' name 'CoCreateGuid';
//function IsEqualGUID; external 'ole32.dll' name 'IsEqualGUID';
function IsEqualGUID(const guid1, guid2: TGUID): Boolean;
var
  a, b: PIntegerArray;
begin
  a := PIntegerArray(@guid1);
  b := PIntegerArray(@guid2);
  Result := (a^[0] = b^[0]) and (a^[1] = b^[1]) and (a^[2] = b^[2]) and (a^[3] = b^[3]);
end;


function CreateGUID(out Guid: TGUID): HResult;
begin
  Result := CoCreateGuid(Guid);
end;

function GUIDToString(const GUID: TGUID): string;
begin
  SetLength(Result, 38);
  StrLFmt(PChar(Result), 38,'{%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x}',   // do not localize
    [GUID.D1, GUID.D2, GUID.D3, GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
    GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]);
end;

function StringToGUID(const S: string): TGUID;
begin
  if not Succeeded(CLSIDFromString(PWideChar(WideString(S)), Result)) then
    raise EConvertError.CreateFmt('Invalid GUID String', [s]);
end;

{function Supports(const Instance: IInterface; const IID: TGUID; out Intf): Boolean;
begin
  Result := (Instance <> nil) and (Instance.QueryInterface(IID, Intf) = 0);
end;

{function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean;
var
  LUnknown: IUnknown;
begin
  Result := (Instance <> nil) and
            ((Instance.GetInterface(IUnknown, LUnknown) and Supports(LUnknown, IID, Intf)) or
             Instance.GetInterface(IID, Intf));
end;
}
function Supports(const Instance: IInterface; const IID: TGUID): Boolean;
var
  Temp: IInterface;
begin
  Result := (Instance <> nil) and (Instance.QueryInterface(IID, Temp) = 0);
end;

function Supports(const Instance: TObject; const IID: TGUID): Boolean;
var
  Temp: IInterface;
begin
  Result := Supports(Instance, IID, Temp);
end;

function Supports(const AClass: TClass; const IID: TGUID): Boolean;
begin
  Result := AClass.GetInterfaceEntry(IID) <> nil;
end;

{$ENDIF}

function GlobeDisplayName( obj : TObject ): String;
var
  idx : integer;
begin
  for idx := 0 to GlobeClassList.Count - 1 do
    if GlobeClassList.Objects[idx] = TObject( obj.ClassType ) then
    begin
      Result := GlobeClassList[idx];
      Exit;
    end;
  Result := obj.ClassName;
end;

procedure RegisterGlobeClass(AClass: TGRootClass; const displayName : String);
begin
  if GlobeClassList = nil then
    GlobeClassList := TStringList.Create;

  GlobeClassList.AddObject(displayName, TObject(AClass));

  RegisterClass(AClass);  // Register with Borland RTTI
end;


function DirectoryExists(const Name: String): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;


function PointLL(iLongX, iLatY: integer): TGPointLL;
begin
  Result.iLongX := Mod180(iLongX);
  Result.iLatY := iLatY;
  Result.iHeightZ := 0;
end;


function PointLLH(iLongX, iLatY, iHeightZ: integer): TGPointLL;
begin
  Result.iLongX := Mod180(iLongX);
  Result.iLatY := iLatY;
  Result.iHeightZ := iHeightZ;
end;


function Point3D(ptLL : TGPointLL): TGPoint3D;
begin
  with ptLL do
    Result := Point3D( iLongX, iLatY, iHeightZ );
end;


function Point3D(Long, Lat, Height: TGDecimalDegree): TGPoint3D;
var
  eTmp, eHeight, eSLong, eClong, eSLat, eCLat: Extended;
begin
  with Result do
  begin
    { Convert to World }
    SinCos(Long * GU_TORADIANS, eSLong, eClong);
    SinCos(Lat * GU_TORADIANS, eSLat, eCLat);
    eHeight := GU_EARTHRADIUS + Height;
    eTmp := eHeight * eCLat;
    X := eTmp * eSlong;
    Z := eTmp * eCLong;
    Y := eHeight * eSLat;
  end;
end;


function MER(aLongX, aLatY, aWidthX, aHeightY: integer): TGMER;
begin
  with Result do
  begin
    iLongX := aLongX;
    iLatY := aLatY;
    iWidthX := aWidthX;
    iHeightY := aHeightY;
  end;
end;


function MERFromDD( aLongX, aLatY, aWidthX, aHeightY: TGDecimalDegree): TGMER;
begin
  with Result do
  begin
    iLongX := Round( aLongX * GU_DEGREE );
    iLatY := Round( aLatY * GU_DEGREE );
    iWidthX := Round( aWidthX * GU_DEGREE );
    iHeightY := Round( aHeightY * GU_DEGREE );
  end;
end;

function GTriangle( const pt0, pt1, pt2 : TPoint ) : TGTriangle;
begin
  Result[0] := pt0;
  Result[1] := pt1;
  Result[2] := pt2;
end;

function GTriangle( x0, y0, x1, y1, x2, y2 : integer ) : TGTriangle;
begin
  Result[0].x := x0;
  Result[0].y := y0;
  Result[1].x := x1;
  Result[1].y := y1;
  Result[2].x := x2;
  Result[2].y := y2;
end;

function GTriangleLL( const ptLL0, ptLL1, ptLL2 : TGPointLL ) : TGTriangleLL;
begin
  Result[0] := ptLL0;
  Result[1] := ptLL1;
  Result[2] := ptLL2;
end;


function PointLLToDD( const ptLL : TGPointLL ) : TGPointDD;
begin
  Result.LongX := ptLL.iLongX / GU_DEGREE;
  Result.LatY := ptLL.iLatY / GU_DEGREE;
  Result.HeightZ := ptLL.iHeightZ / GU_DEGREE;
end;

function DDToPointLL( const Long: TGDecimalDegree; const Lat: TGDecimalDegree ) : TGPointLL;
begin
  Result.iLongX := LimitVal( Round( Long * GU_DEGREE ), -(GU_180_DEGREE - 1), GU_180_DEGREE - 1);
  Result.iLatY := LimitVal( Round( Lat * GU_DEGREE ), -(GU_90_DEGREE - 1), GU_90_DEGREE - 1 );
  Result.iHeightZ := 0;
end;

function MER_Area(const mer: TGMER ): Extended;
begin
  Result := mer.iWidthX * mer.iHeightY;
end;

function MER_CenterLL(const mer : TGMER): TGPointLL;
begin
  with mer do
  begin
    Result.iLongX := iLongX + iWidthX div 2;
    Result.iLatY := iLatY + iHeightY div 2;
  end;
end;

function MER_Contains( const mer : TGMER; const aMER: TGMER ): Boolean;
begin
  with mer do
    Result := ( aMER.iLongX >= iLongX )
      and ( aMER.iLongX + aMER.iWidthX < iLongX + iWidthX )
      and ( aMER.iLatY >= iLatY )
      and ( aMER.iLatY + aMER.iHeightY < iLatY + iHeightY );
end;

function MER_ContainsLL(const mer : TGMER; const ptLL: TGPointLL): Boolean;
begin
  Result := MER_ContainsLLEx(mer, ptLL ) <> hzNone;

{  Result := (ptLL.iLatY > iLatY) and (ptLL.iLatY < iLatY + iHeightY);
  if Result then
  begin
    Result := (ptLL.iLongX > iLongX) and (ptLL.iLongX < iLongX + iWidthX);
    // check for crossing of 180 meridan
    if not Result and (iLongX + iWidthX > GU_180_DEGREE) then
      Result := (ptLL.iLongX + GU_360_DEGREE > iLongX) and (ptLL.iLongX + GU_360_DEGREE < iLongX + iWidthX);
  end;
}
end;

function MER_ContainsLLEx(const mer : TGMER; const ptLL: TGPointLL): TGMERHitZone;
begin
  Result := hzNone;

  with mer do
    if (ptLL.iLatY >= iLatY ) and (ptLL.iLatY <= iLatY + iHeightY ) then
      if iLongX + iWidthX <= GU_180_DEGREE then
      begin
        if (ptLL.iLongX >= iLongX) and (ptLL.iLongX <= iLongX + iWidthX) then
          Result := hzHit;
      end
      else { split across the 180 meridian }
        if (ptLL.iLongX >= iLongX) and (ptLL.iLongX < GU_180_DEGREE) then
          Result := hzPlus180
        else
          if (ptLL.iLongX >= -GU_180_DEGREE) and (ptLL.iLongX <= Mod180(iLongX + iWidthX)) then
            Result := hzMinus180;
end;

function MER_Crosses180(const mer : TGMER): Boolean;
begin
  with mer do
    Result := iLongX + iWidthX > GU_180_DEGREE;
end;

procedure MER_Empty(var mer : TGMER);
begin
  with mer do
  begin
    iLongX := 0;
    iLatY := 0;
    iWidthX := 0;
    iHeightY := 0;
  end;
end;

function MERFromLLArray(ptLLArray: array of TGPointLL): TGMER;
var
  idx, tmp: integer;
begin
  Assert(High(ptLLArray) >= 0, 'FromLLArray called with zero points');

  Result.iLongX := ptLLArray[0].iLongX;
  Result.iLatY := ptLLArray[0].iLatY;
  Result.iWidthX := 0;
  Result.iHeightY := 0;

  for idx := 1 to High(ptLLArray) do
  begin
    tmp := ptLLArray[idx].iLongX;

    if tmp < Result.iLongX then
    begin
      Inc( Result.iWidthX, Result.iLongX - tmp);
      Result.iLongX := tmp;
    end;

    if tmp > Result.iLongX + Result.iWidthX then
      Result.iWidthX := tmp - Result.iLongX;

    tmp := ptLLArray[idx].iLatY;
    if tmp < Result.iLatY then
    begin
      Inc( Result.iHeightY, Result.iLatY - tmp);
      Result.iLatY := tmp;
    end;

    if tmp > Result.iLatY + Result.iHeightY then
      Result.iHeightY := tmp - Result.iLatY;
  end;
end;

function StrToMER(const str: String): TGMER;
begin
  Result.iLongX := StrToIntDef( ItemFromCSL( str, 0 ), 0 );
  Result.iLatY := StrToIntDef( ItemFromCSL( str, 1 ), 0 );
  Result.iWidthX := StrToIntDef( ItemFromCSL( str, 2 ), 0 );
  Result.iHeightY := StrToIntDef( ItemFromCSL( str, 3 ), 0 );
end;

function MERToStr(const mer : TGMER): String;
begin
  with mer do
    Result := IntToStr( iLongX ) + ',' + IntToStr( iLatY )
      + ',' + IntToStr( iWidthX ) + ',' + IntToStr( iHeightY );
end;

function MER_Height(const mer : TGMER): TGDecimalDegree;
begin
  Result := mer.iHeightY * ( 1.0 / GU_DEGREE );
end;

function MER_Inflate( const mer : TGMER; dx, dy : integer ) : TGMER;
begin
  with mer do
  begin
    Result.iLongX := iLongX - dx;
    Result.iLatY := iLatY - dy;
    Result.iWidthX := iWidthX + dx + dx;
    Result.iHeightY := iHeightY + dy + dy;
  end;
end;

function MER_Intersect(const srcMER : TGMER; const aMER: TGMER ): Boolean;
var
  iTmp, iMERRight : integer;
begin
  with srcMER do
  begin
    Result := not (( MER_iTopY(srcMER) < aMER.iLatY ) or ( iLatY > MER_iTopY(aMER)));

    if not Result then
      Exit;

    iMERRight := iLongX + iWidthX;
    if iMERRight > GU_180_DEGREE then  // if Source MER crosses 180 Meridian
    begin
      iTmp := GU_180_DEGREE - iLongX;

      Result := MER_Intersect( MER( iLongX, iLatY, iTmp, iHeightY ), aMER );
      if not Result then
        Result := MER_Intersect( MER( -GU_180_DEGREE, iLatY, iWidthX - iTmp, iHeightY ), aMER);
    end
    else
    begin
      iMERRight := aMER.iLongX + aMER.iWidthX;

      if iMERRight > GU_180_DEGREE then  // if target MER crosses 180 Meridian
      begin
        iTmp := GU_180_DEGREE - aMER.iLongX;

        Result := MER_Intersect( srcMER, MER( aMER.iLongX, aMER.iLatY, iTmp, aMER.iHeightY ));
        if not Result then
          Result := MER_Intersect( srcMER, MER( -GU_180_DEGREE, aMER.iLatY, aMER.iWidthX - iTmp, aMER.iHeightY ));
      end
      else
      begin
        if ( iLongX > iMERRight ) or ( MER_iRightX(srcMER) < aMER.iLongX ) then
          Result := false
        else
          Result := not (( MER_iTopY(srcMER) < aMER.iLatY ) or ( iLatY > MER_iTopY(aMER)));
      end;
    end;
  end;
end;

function MER_iRightX(const mer : TGMER): integer;
begin
  with mer do
    Result := iLongX + iWidthX;
end;

function MER_IsEmpty(const mer : TGMER): Boolean;
begin
  with mer do
    Result := (iWidthX = 0) and (iHeightY = 0);
end;

function MER_IsEqual( const srcMER : TGMER; const aMER: TGMER): Boolean;
begin
  with srcMER do
    Result := ( iLongX = aMER.iLongX )
      and ( iLatY = aMER.iLatY )
      and ( iWidthX = aMER.iWidthX )
      and ( iHeightY = aMER.iHeightY );
end;

function MER_iTopY(const mer : TGMER): integer;
begin
  with mer do
    Result := iLatY + iHeightY;
end;

function MER_Lat(const mer : TGMER): TGDecimalDegree;
begin
  Result := mer.iLatY * ( 1.0 / GU_DEGREE );
end;

function MER_Long(const mer : TGMER): TGDecimalDegree;
begin
  Result := mer.iLongX * ( 1.0 / GU_DEGREE );
end;

function MER_LongLat(const mer : TGMER): TGPointLL;
begin
  with mer do
    Result := PointLL( iLongX, iLatY );
end;

function MER_Right(const mer : TGMER): TGDecimalDegree;
begin
  with mer do
    Result := (iLongX + iWidthX)  * ( 1.0 / GU_DEGREE );
end;

function MER_Top(const mer : TGMER): TGDecimalDegree;
begin
  with mer do
    Result := ( iLatY + iHeightY ) * ( 1.0 / GU_DEGREE );
end;

function MER_TopRight(const mer : TGMER): TGPointLL;
begin
  with mer do
    Result := PointLL( iLongX + iWidthX, iLatY + iHeightY );
end;

function MER_Union(const srcMER : TGMER; const aMER: TGMER): TGMER;
var
  sL, mL : integer;
begin
  // return the Union of the 2 MERs
  if MER_IsEmpty( srcMER ) then
    Result := aMER
  else
    if MER_IsEmpty( aMER ) then
      Result := srcMER
    else
      with srcMER do
      begin
        // get the Union of the Y and height values
        Result.iLatY := MinVal(iLatY, aMER.iLatY);
        if MER_iTopY( srcMER ) > MER_iTopY( aMER ) then
          Result.iHeightY := MER_iTopY( srcMER ) - Result.iLatY
        else
          Result.iHeightY := MER_iTopY( aMER ) - Result.iLatY;

        sl := srcMER.iLongX;
        ml := aMER.iLongX;

        if sl < ml then // source is to the left of aMer
        begin
          Result.iWidthX := MaxVal( srcMER.iWidthX, ( ml + aMER.iWidthX ) - sl );
          Result.iLongX := sl;
        end
        else
        begin
          Result.iWidthX := MaxVal( aMER.iWidthX, ( sl + srcMER.iWidthX ) - ml );
          Result.iLongX := ml;
        end;

      end;
end;

function xMER_Union(const srcMER : TGMER; const aMER: TGMER): TGMER;
var
  sL, mL : integer;
  w, w180 : Extended;
begin
  // return the Union of the 2 MERs
  if MER_IsEmpty( srcMER ) then
    Result := aMER
  else
    if MER_IsEmpty( aMER ) then
      Result := srcMER
    else
      with srcMER do
      begin
        // get the Union of the Y and height values
        Result.iLatY := MinVal(iLatY, aMER.iLatY);
        if MER_iTopY( srcMER ) > MER_iTopY( aMER ) then
          Result.iHeightY := MER_iTopY( srcMER ) - Result.iLatY
        else
          Result.iHeightY := MER_iTopY( aMER ) - Result.iLatY;

        sl := srcMER.iLongX;
        ml := aMER.iLongX;

        // if we have crossed the 180 or prime meridians
        if Sign( sl ) <> Sign( ml ) then
        begin
          if sl < 0 then  // sl is to the left of ml since the signs are different
          begin
            w := ( ml + aMER.iWidthX ) - sl;
            w180 := ( GU_360_DEGREE + sl + srcMER.iWidthX ) - ml;

            if ( Abs( w ) < Abs( w180 )) or ( iWidthX > GU_90_DEGREE ) then
            begin
              Result.iWidthX := MaxVal( srcMER.iWidthX, Round(w) );
              Result.iLongX := sl;
            end
            else
            begin
              Result.iWidthX := MaxVal( aMER.iWidthX, Round(w180) );
              Result.iLongX := ml;
            end;
          end
          else
          begin
            w := ( sl + srcMER.iWidthX ) - ml;
            w180 := ( GU_360_DEGREE + ml + aMER.iWidthX ) - sl;

            if ( Abs( w ) < Abs( w180 )) or ( aMER.iWidthX > GU_90_DEGREE ) then
            begin
              Result.iWidthX := MaxVal( aMER.iWidthX, Round(w) );
              Result.iLongX := ml;
            end
            else
            begin
              Result.iWidthX := MaxVal( srcMER.iWidthX, Round(w180) );
              Result.iLongX := sl;
            end;
          end;
        end
        else  // both mer's start in the same hemisphere
        begin
          if sl < ml then // source is to the left of aMer
          begin
            Result.iWidthX := MaxVal( srcMER.iWidthX, ( ml + aMER.iWidthX ) - sl );
            Result.iLongX := sl;
          end
          else
          begin
            Result.iWidthX := MaxVal( aMER.iWidthX, ( sl + srcMER.iWidthX ) - ml );
            Result.iLongX := ml;
          end;
        end;
      end;
end;

function MER_Width(const mer : TGMER): TGDecimalDegree;
begin
  Result := mer.iWidthX * ( 1.0 / GU_DEGREE );
end;

function ItemToCSL( const csl : String; const item : String; index : integer ) : String;
var
  sl : TStringList;
  s, p: PChar;
  idx : integer;
begin
  sl := TStringList.Create;

  p := PChar(csl);
  s := p;

  if p^ <> #0 then
  begin
    while p^ <> #0 do
    begin
      if p^ in [',', #9] then
      begin
        SetString(Result, s, p - s);
        sl.Add(Result);
        s := p + 1;
      end;
      Inc(p);
    end;
    SetString(Result, s, p - s);
    sl.Add(Result);
  end;

  while index >= sl.Count do
    sl.Add('');

  sl[index] := item;

  Result := '';
  for idx := 0 to sl.Count - 2 do
    Result := Result + sl[idx] + ',';
  Result := Result + sl[sl.Count - 1];

  sl.Free;
end;

function ItemFromCSL( const csl : String; index : integer ) : String;
var
  s, p: PChar;
begin
  Result := '';

  p := PChar(csl);
  s := p;

  while p^ <> #0 do
  begin
    if p^ in [',', #9] then
    begin
      Dec( index );
      if index < 0 then
        break;
      s := p + 1;
    end;
    Inc(p);
  end;

  if index <= 0 then
    SetString(Result, s, p - s);
end;


function sIFE(bCond: Boolean; const sLeft, sRight: String): String;
begin
  if bCond then
    Result := sLeft
  else
    Result := sRight;
end;


function PointOnLine( X, Y, ax, ay, bx, by, iTolerance: Integer): Boolean;
var
  u, s, A, B, C, D, L: Double;
begin
  Result := False;

  A := bx - ax;
  B := by - ay;
  L := (A * A + B * B);
  C := ay;
  D := ax;

  if L = 0 then
    Exit;

  u := ((C - Y) * (C - by) - (D - X) * A) / L;
  if (u >= 0) and (u <= 1.0) then
  begin
    s := abs((C - Y) * A - (D - X) * B) / L;
    Result := ( s * sqrt(L)) <= iTolerance;
  end;
end;


function GlobeMER : TGMER;
begin
  Result := MER( -GU_180_DEGREE, -GU_90_DEGREE, GU_360_DEGREE, GU_180_DEGREE );
end;


function PointsToRect(ptArray: array of TPoint): TRect;
var
  idx: integer;
begin
  Result.TopLeft := ptArray[0];
  Result.BottomRight := ptArray[0];

  for idx := 1 to High(ptArray) do
    with ptArray[idx] do
    begin
      if X < Result.Left then
        Result.Left := X;
      if X > Result.Right then
        Result.Right := X;

      if Y < Result.Top then
        Result.Top := Y;
      if Y > Result.Bottom then
        Result.Bottom := Y;
    end;
end;

function AbsRect( const rect : TRect ): TRect;
begin
  Result := rect;
  
  with rect do
  begin
    if Left > Right then
    begin
      Result.Left := Right;
      Result.Right := Left;
    end;
    if Bottom < Top then
    begin
      Result.Top := Bottom;
      Result.Bottom := Top;
    end;
  end;
end;


function StrToGU(sValue: String; const sFmt: String): Integer;
var
  iDeg, iMin, iMinTh, iSec, iThird, iUnits: Integer;
  idx, jdx, iHund: Integer;
  bSign: Boolean;
begin
  iDeg := 0;
  iMin := 0;
  iMinTh := 0;
  iSec := 0;
  iThird := 0;
  iHund := 0;
  iUnits := 0;
  bSign := True;

  idx := 1;
  jdx := 1;

  while (jdx <= Length(sValue)) and (idx <= Length(sFmt)) do
  begin
    if jdx > 1 then
      sValue := Copy(sValue, jdx, 255);
    jdx := 2;
    if sFmt[idx] = '%' then
    begin
      Inc(idx);
      case UpCase(sFmt[idx]) of
        'D': Val(sValue, iDeg, jdx);
        'M': Val(sValue, iMin, jdx);
        'I': Val(sValue, iMinTh, jdx);
        'S': Val(sValue, iSec, jdx);
        'H': Val(sValue, iHund, jdx);
        'T': Val(sValue, iThird, jdx);
        'U': Val(sValue, iUnits, jdx);
        'E': bSign := UpCase(sValue[1]) = 'E';
        'N': bSign := UpCase(sValue[1]) = 'N';
      else
        raise EConvertError.Create(rsStrToLLMsg);
      end;
    end
    else
      if sValue[1] <> sFmt[idx] then
        break;
    Inc(idx);
  end;

  Result := iDeg * GU_DEGREE + iMin * GU_MINUTE + iMinTh * GU_MINUTE_THOUSANTH
    + iSec * GU_SECOND + iThird * GU_THIRD + iHund * GU_HUNDRETH + iUnits;

  if not bSign then
    Result := -Result;
end;


function GUToStr( iValue: Integer; const sFmt: String): String;
var
  iDeg, iMin, iSec, iHund, iThird, iSecTh: Integer;
  idx: Integer;
  bNegative, bSign: Boolean;
  iMinTh: Integer;
begin
  bNegative := iValue < 0;
  iValue := Abs(iValue);

  bSign := bNegative and (Pos('%N', Uppercase(sFmt)) = 0) and (Pos('%E', Uppercase(sFmt)) = 0);

  iDeg := iValue div GU_DEGREE;
  Dec(iValue, iDeg * GU_DEGREE);
  iMin := iValue div GU_MINUTE;
  Dec(iValue, iMin * GU_MINUTE);
  iMinTh := iValue div GU_MINUTE_THOUSANTH;
  iSec := iValue div GU_SECOND;
  Dec(iValue, iSec * GU_SECOND);

  iHund := iValue div (GU_SECOND div 100);  // Hundreth of a second
  iSecTh := iValue div (GU_SECOND div 1000);  // thousanth of a second
  iThird := iValue div GU_THIRD;        // sixtyeth of a second

  Result := '';
  idx := 1;
  while idx <= Length(sFmt) do
  begin
    if sFmt[idx] = '%' then
    begin
      if bSign then
        Result := Result + '-';
      bSign := False;
      Inc(idx);
      case UpCase(sFmt[idx]) of
        'D':
          Result := Result + IntToStr(iDeg);
        'M':
          Result := Result + Format('%2.2d', [iMin]);
        'I':
          Result := Result + Format('%3.3d', [iMinTh]);
        'S':
          Result := Result + Format('%2.2d', [iSec]);
        'H':
          Result := Result + Format('%2.2d', [iHund]);
        'U':
          Result := Result + Format('%3.3d', [iSecTh]);
        'T':
          Result := Result + Format('%2.2d', [iThird]);
        'E':
          Result := Result + sIFE(bNegative, 'W', 'E');
        'N':
          Result := Result + sIFE(bNegative, 'S', 'N');
      else
        Result := Result + sFmt[idx];
      end;
    end
    else
      Result := Result + sFmt[idx];
    Inc(idx);
  end;
end;


function LLToStr(iLong, iLat: Integer): String;
begin
  Result := GUToStr(iLong, '%d.%m.%s.%t%E ') + GUToStr(iLat, '%d.%m.%s.%t%N');
end;


function DDToGU(const Value: TGDecimalDegree): Integer;
begin
  Result := Round( Value * GU_DEGREE );
end;


function GUToDD(const Value: Double): TGDecimalDegree;
begin
  Result := Value * (1.0 / GU_DEGREE);
end;


function GUTo(const Value: Double; Units: TGUnitTypes): Double;
begin
  try
    Result := Value / GU_Conversions[Ord(Units)];
  except
    Result := Value;
  end;
end;


function GUFrom(const Value: Double; Units: TGUnitTypes): Integer;
begin
  Result := Round(Value * GU_Conversions[Ord(Units)]);
end;


function DDFrom(const Value: Double; Units: TGUnitTypes): TGDecimalDegree;
begin
  Result := Value * GU_Conversions[Ord(Units)] * ( 1.0 / GU_DEGREE );
end;


function StrToUnits(const sUnits: String): TGUnitTypes;
var
  idx: integer;
begin
  for idx := 0 to High(GUnitStrings) do
    if CompareText(sUnits, GUnitStrings[idx]) = 0 then
    begin
      Result := TGUnitTypes(idx);
      Exit;
    end;
  raise EGException.CreateFmt(rsEStrToUnitsMsg, [sUnits]);
end;


function UnitsToStr(Units: TGUnitTypes): String;
begin
  Result := GUnitStrings[Ord(Units)];
end;


function Mod180(Value: integer): Integer;
begin
  Result := Value;

  if Result > GU_180_DEGREE then
    Dec( Result, GU_360_DEGREE )
  else
    if Result <= -GU_180_DEGREE then
      Inc( Result, GU_360_DEGREE )
end;


function Round180( const Value: Double ): integer;
begin
  Result := Round( LimitFloat( Value, -GU_180_DEGREE, GU_180_DEGREE ));
end;


function LimitFloat(const eValue, eMin, eMax: Double): Double;
begin
  if eValue < eMin then
    Result := eMin
  else
    if eValue > eMax then
      Result := eMax
    else
      Result := eValue;
end;

function LimitVal(iValue, iMin, iMax: Integer): Integer;
begin
  if iValue < iMin then
    Result := iMin
  else
    if iValue > iMax then
      Result := iMax
    else
      Result := iValue;
end;

function MinFloat(const eLeft, eRight: Double): Double;
begin
  if eLeft < eRight then
    Result := eLeft
  else
    Result := eRight;
end;


function MaxFloat(const eLeft, eRight: Double): Double;
begin
  if eLeft > eRight then
    Result := eLeft
  else
    Result := eRight;
end;


function LongDiff(iLong1, iLong2: integer): integer;
begin
  Result := iLong2 - iLong1;

  // If a result larger than 180
  if Abs( Result ) > GU_180_DEGREE then
    if Result < 0 then
      Result := Result + GU_360_DEGREE
    else
      Result := Result - GU_360_DEGREE;

  if Result = 0 then { avoid divide by zero problems }
    Inc(Result);
end;

function Mod180Float(const Value: Double): Double;
begin
  if Value >= 180.0 then
    Result := Value - 360.0
  else
    if Value < -180.0 then
      Result := Value + 360.0
    else
      Result := Value;
end;


function MinVal(iLeft, iRight: integer): Integer;
begin
  if iLeft < iRight then
    Result := iLeft
  else
    Result := iRight;
end;


function MaxVal(iLeft, iRight: integer): Integer;
begin
  if iLeft > iRight then
    Result := iLeft
  else
    Result := iRight;
end;


function ExtendedToStr( Value: Extended ): String;
var
  sepChar : Char;
begin
  sepChar := DecimalSeparator;
  DecimalSeparator := '.';  // Make sure we are using the correct decimal format for float values.

  Result := FloatToStr(Value);

  DecimalSeparator := sepChar;
end;


function StrToExtendedDef(const Text: String; Default: Extended): Extended;
var
  Code: integer;
  sepChar : Char;
begin
  sepChar := DecimalSeparator;
  DecimalSeparator := '.';

  Val(Text, Result, Code);
  if Code <> 0 then
    Result := Default;

  DecimalSeparator := sepChar;
end;


function StrToExtended(const Text: String): Extended;
var
  Code: integer;
begin
  Val(Text, Result, Code);
  if Code <> 0 then
    raise EGException.CreateFmt(rsEConversionError, [Text]);
end;


function RectToStr( const rect : TRect ) : String;
begin
  Result := IntToStr( rect.Left ) + ',' + IntToStr( rect.Top ) + ','
    + IntToStr( rect.Right ) + ',' + IntToStr( rect.Bottom );
end;


function StrToRect( const str : String ) : TRect;
begin
  Result.Left := StrToIntDef( ItemFromCSL( str, 0 ), 0 );
  Result.Top := StrToIntDef( ItemFromCSL( str, 1 ), 0 );
  Result.Right := StrToIntDef( ItemFromCSL( str, 2 ), 0 );
  Result.Bottom := StrToIntDef( ItemFromCSL( str, 3 ), 0 );
end;

function PointLLToStr( const ptLL : TGPointLL ): String;
begin
  Result := IntToStr( ptLL.iLongX ) + ',' + IntToStr( ptLL.iLatY );

  if ptLL.iHeightZ <> 0 then
    Result := Result + ',' + IntToStr( ptLL.iHeightZ );
end;

function StrToPointDD( const str : String ) : TGPointDD;
begin
  Result.LongX := StrToExtendedDef( ItemFromCSL(str, 0), 0.0 );
  Result.LatY := StrToExtendedDef( ItemFromCSL(str, 1), 0.0 );
end;

function StrToPointLL( const str : String ) : TGPointLL;
begin
  Result.iLongX := StrToIntDef( ItemFromCSL(str, 0), 0 );
  Result.iLatY := StrToIntDef( ItemFromCSL(str, 1), 0 );
  Result.iHeightZ := StrToIntDef( ItemFromCSL(str, 2), 0 );
end;

function PointDDToStr( const ptDD : TGPointDD ): String;
begin
  Result := FloatToStr( ptDD.LongX ) + ',' + FloatToStr( ptDD.LatY );
end;

function DDToStr( const Long: TGDecimalDegree; const Lat: TGDecimalDegree ): String;
begin
  Result := FloatToStr( Long ) + ',' + FloatToStr( Lat );
end;

function PointToStr( const pt : TPoint ): String;
begin
  Result := IntToStr( pt.X ) + ',' + IntToStr( pt.Y );
end;

function StrToPoint( const str : String ) : TPoint;
begin
  Result.X := StrToIntDef( ItemFromCSL(str, 0), 0 );
  Result.Y := StrToIntDef( ItemFromCSL(str, 1), 0 );
end;

function AngleToRadians(iAngle: Integer): Double;
begin
  Result := iAngle * GU_TORADIANS;
end;


function RadiansToAngle(const eRad: Double): Integer;
begin
  Result := Round(eRad * GU_FROMRADIANS);
end;


function Cross180(iLong1, iLong2: Integer): Boolean;
var
  iZone1, iZone2 : integer;
begin
  iZone1 := iLong1 div GU_90_DEGREE;
  iZone2 := iLong2 div GU_90_DEGREE;

  Result := (( iZone1 < 0 ) and ( iZone2 > 0 )) or (( iZone1 > 0 ) and ( iZone2 < 0 ));
end;


function Tan(const X: Extended): Extended;
asm
        FLD    X
        FPTAN
        FSTP   ST(0)
        FWAIT
end;


function IntPower(const Base: Extended; Exponent: Integer): Extended;
asm
        mov     ecx, eax
        cdq
        fld1                      { Result := 1 }
        xor     eax, edx
        sub     eax, edx          { eax := Abs(Exponent) }
        jz      @@3
        fld     Base
        jmp     @@2
@@1:    fmul    ST, ST            { X := Base * Base }
@@2:    shr     eax,1
        jnc     @@1
        fmul    ST(1),ST          { Result := Result * X }
        jnz     @@1
        fstp    st                { pop X from FPU stack }
        cmp     ecx, 0
        jge     @@3
        fld1
        fdivrp                    { Result := 1 / Result }
@@3:
        fwait
end;


function Power(const Base, Exponent: Extended): Extended;
begin
  if Exponent = 0.0 then
    Result := 1.0 { n**0 = 1 }
  else
    if (Base = 0.0) and (Exponent > 0.0) then
      Result := 0.0 { 0**n = 0, n > 0 }
    else
      if (Frac(Exponent) = 0.0) and (Abs(Exponent) <= MaxInt) then
        Result := IntPower(Base, Integer(Trunc(Exponent)))
      else
        Result := Exp(Exponent * Ln(Base))
end;


function IsNan(const AValue: Double): Boolean;
begin
  Result := ((PInt64(@AValue)^ and $7FF0000000000000)  = $7FF0000000000000) and
            ((PInt64(@AValue)^ and $000FFFFFFFFFFFFF) <> $0000000000000000)
end;

function IsInfinite(const AValue: Double): Boolean;
begin
  Result := ((PInt64(@AValue)^ and $7FF0000000000000) = $7FF0000000000000) and
            ((PInt64(@AValue)^ and $000FFFFFFFFFFFFF) = $0000000000000000)
end;

function Sign(const Value: Extended): integer;
begin
  Result := 1;
  if Value < 0.0 then
    Result := -1;
end;


function SphericalMod(const X: TGDecimalDegree): TGDecimalDegree;
begin
  Result := X;
  if X < -LocalPi then
    Result := X + DoublePi
  else
    if X > LocalPi then
      Result := X - DoublePi;
end;


procedure SinCos(const Theta: Extended; var eSin, eCos: Extended);
asm
        FLD     Theta
        FSINCOS
        FSTP    tbyte ptr [edx]    // Cos
        FSTP    tbyte ptr [eax]    // Sin
        FWAIT
end;


function Hypot(const X, Y: Extended): Extended;
var
  eX, eY, eTemp: Extended;
begin
  eX := Abs(X);
  eY := Abs(Y);
  if eX > eY then
  begin
    eTemp := eX;
    eX := eY;
    eY := eTemp;
  end;
  if eX = 0 then
    Result := eY
  else { Y > X, X <> 0, so Y > 0}
    Result := eY * Sqrt(1 + Sqr(eX / eY));
end;


function ArcTan2(const Y, X: Extended): Extended;
asm
        FLD     Y
        FLD     X
        FPATAN
        FWAIT
end;


function ArcSin(const X: Extended): Extended;
begin
  if Abs(X) < 1.0 then
    Result := ArcTan2(X, Sqrt(1 - X * X))
  else
    Result := Sign(X) * HalfPi;
end;


function ArcCos(const X: Extended): Extended;
var
  eTmp: Extended;
begin
  {  Result := 0;
    if (Abs(X - 1.0)) > 0.0001 then
   Result := ArcTan2(Sqrt(1 - X * X), X);
   }
  eTmp := 1.0 - X * X;
  if eTmp > 0 then
    Result := ArcTan2(Sqrt(eTmp), X)
  else
    if X < 0 then
      Result := LocalPI
    else
      Result := 0;
end;


function V3D(const eX, eY, eZ: Double): TGVec3D;
begin
  Result.X := eX;
  Result.Y := eY;
  Result.Z := eZ;
  Vec3D_Normalise( Result );
end;


function V3D(const ptLL : TGPointLL): TGVec3D;
begin
  with Point3D( ptLL ) do
    Result := V3D( X, Y, Z );
end;




function V3DLerp(const lo, hi: TGVec3D; const alpha: Double): TGVec3D;
  function LERP(a, l, h: Double): Double;
  begin
    Result := l + ((h - l) * a);
  end;
begin
  Result.x := LERP(alpha, lo.x, hi.x);
  Result.y := LERP(alpha, lo.y, hi.y);
  Result.z := LERP(alpha, lo.z, hi.z);
  { V3DNormalise( Result );}
end;


function PointLLMatrixMul(const mat: TGMatrix; const ptLL: TGPointLL): TGPointLL;
begin
  Result := Vec3D_ToPointLL( Vec3D_MatrixMultiply(V3D( ptLL ), mat));
end;


function Quat(const X, Y, Z, W: Double): TGQuaternion;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
  Quat_Normalise( Result );
end;

{------------------------------------------------------------------------------
 AxisAngleToQuat
------------------------------------------------------------------------------}
{**
  @Param axis Normalised Vector to represent the Axis.
  @Param angle Angle in radians to set the Quaternion to.
  @Return Converted Vector and angle of rotation to Quaternion.
}
function AxisAngleToQuat(const axis: TGVec3D; const angle: Double): TGQuaternion;
var
  eCos, eSin: Extended;
begin
  SinCos(angle * 0.5, eSin, eCos);
  with Result do
  begin
    x := axis.x * eSin;
    y := axis.y * eSin;
    z := axis.z * eSin;
    w := eCos;
  end;
end;




function QuatSlerp(const qLo, qHi: TGQuaternion; const Alpha: Double): TGQuaternion;
var
  hiX, hiY, hiZ, hiW: Double;
  CosAngle, t, Sint, ScaleLo, ScaleHi: Double;
begin
  CosAngle := Quat_Dot(qLo, qHi);

  { adjust signs (if necessary) }
  with qHi do
    if CosAngle < 0.0 then
    begin
      CosAngle := -CosAngle;
      hiX := -x;
      hiY := -y;
      hiZ := -z;
      hiW := -w;
    end
    else
    begin
      hiX := x;
      hiY := y;
      hiZ := z;
      hiW := w;
    end;

  { quaternions are very close so do a linear interpolation }
  ScaleLo := 1.0 - Alpha;
  ScaleHi := Alpha;

  { calculate coefficients }
  if ScaleLo > TG_EPSILON then
  begin
    { standard case (slerp)}
    t := ArcCos(CosAngle);
    Sint := Sin(t);
    ScaleLo := Sin(ScaleLo * t) / Sint;
    ScaleHi := Sin(ScaleHi * t) / Sint;
  end;

  with qLo do
    Result := Quat(
      ScaleLo * X + ScaleHi * hiX, ScaleLo * Y + ScaleHi * hiY,
      ScaleLo * Z + ScaleHi * hiZ, ScaleLo * W + ScaleHi * hiW);
end;


procedure IdentityMatrix(var mat: TGMatrix);
var
  idx : integer;
begin
  for idx := 0 to 15 do
    mat.terms[idx] := Ord( idx mod 5 = 0 );
end;


procedure ScaleMatrix(var mat: TGMatrix; const SF: Extended);
var
  idx: Integer;
begin
  for idx := 0 to 15 do
    mat.terms[idx] := mat.terms[idx] * SF;
end;


procedure TransposeMatrix(var FromMat, ToMat: TGMatrix);
begin
//  ToMat[0] := FromMat[0];
//  ToMat[3] := FromMat[1];
//  ToMat[6] := FromMat[2];
//  ToMat[1] := FromMat[3];
//  ToMat[4] := FromMat[4];
//  ToMat[7] := FromMat[5];
//  ToMat[2] := FromMat[6];
//  ToMat[5] := FromMat[7];
//  ToMat[8] := FromMat[8];

  ToMat.terms[0] := FromMat.terms[0];
  ToMat.terms[4] := FromMat.terms[1];
  ToMat.terms[8] := FromMat.terms[2];
  ToMat.terms[1] := FromMat.terms[4];
  ToMat.terms[5] := FromMat.terms[5];
  ToMat.terms[9] := FromMat.terms[6];
  ToMat.terms[2] := FromMat.terms[8];
  ToMat.terms[6] := FromMat.terms[9];
  ToMat.terms[10] := FromMat.terms[10];
end;


function DynArrayPtr(const aDynArray: TGDynArray; iIndex: integer): Pointer;
asm
  IMUL  EDX, TGDynArrayRecord([EAX]).ItemSize
  ADD   EAX, EDX
  ADD   EAX, TYPE TGDynArrayRecord    { SizeOf( TGDynArrayRecord ) }
end;


function DynArrayAsObject(const aDynArray: TGDynArray; iIndex: integer): TObject;
asm
  IMUL  EDX, TGDynArrayRecord([EAX]).ItemSize
  ADD   EDX, EAX
  ADD   EDX, TYPE TGDynArrayRecord    { SizeOf( TGDynArrayRecord ) }
  MOV   EAX, [EDX]
end;


function DynArraySetAsObject(const aDynArray: TGDynArray; iIndex: integer; Obj: TObject): TObject;
asm
  IMUL  EDX, TGDynArrayRecord([EAX]).ItemSize
  ADD   EDX, EAX
  ADD   EDX, TYPE TGDynArrayRecord    { SizeOf( TGDynArrayRecord ) }
  MOV   EAX, [EDX]
  MOV   [EDX], Obj
end;


function DynArrayAsInteger(const aDynArray: TGDynArray; iIndex: integer): integer;
asm
  IMUL  EDX, TGDynArrayRecord([EAX]).ItemSize
  ADD   EDX, EAX
  ADD   EDX, TYPE TGDynArrayRecord    { SizeOf( TGDynArrayRecord ) }
  MOV   EAX, [EDX]
end;


function DynArraySetAsInteger(const aDynArray: TGDynArray; iIndex: integer; Value: integer): integer;
asm
  IMUL  EDX, TGDynArrayRecord([EAX]).ItemSize
  ADD   EDX, EAX
  ADD   EDX, TYPE TGDynArrayRecord    { SizeOf( TGDynArrayRecord ) }
  MOV   EAX, [EDX]
  MOV   [EDX], Value
end;


procedure DynArrayIncReference(const aDynArray: TGDynArray);
begin
  Inc(aDynArray^.RefCount);
end;


procedure DynArrayDecReference(const aDynArray: TGDynArray);
begin
  Dec(aDynArray^.RefCount);
end;


function DynArrayCount(const aDynArray: TGDynArray): integer;
begin
  Result := aDynArray^.Count;
end;


function DynArrayIndexOfObject(const aDynArray: TGDynArray; Obj: TObject): integer;
begin
  if aDynArray^.Itemsize = SizeOf(TObject) then
    for Result := 0 to aDynArray^.Count - 1 do
      if DynArrayAsObject(aDynArray, Result) = Obj then
        Exit;
  Result := -1;
end;


procedure DynArrayMove(var aDynArray: TGDynArray; iFrom, iTo: integer);
var
  tmpPtr: Pointer;
begin
  if aDynArray = nil then
    raise EGException.Create('DynArrayMove: Invalid Parameter');

  with aDynArray^ do
    if (iFrom <> iTo) and (iFrom < Count) and (iTo < Count) and ( iTo >= 0 ) then
    begin
      tmpPtr := AllocMem(ItemSize);

      Windows.CopyMemory(tmpPtr, DynArrayPtr(aDynArray, iFrom), ItemSize);
      DynArrayDelete(aDynArray, iFrom);
      DynArrayInsert(aDynArray, iTo);
      Windows.CopyMemory(DynArrayPtr(aDynArray, iTo), tmpPtr, ItemSize);

      FreeMem(tmpPtr);
    end;
end;


procedure DynArrayInsert(var aDynArray: TGDynArray; iIndex: integer);
begin
  DynArraySetLength(aDynArray, aDynArray^.Count + 1);
  with aDynArray^ do
    if iIndex < Count then
      Windows.MoveMemory(DynArrayPtr(aDynArray, iIndex + 1), DynArrayPtr(aDynArray, iIndex), (Count - 1 - iIndex) * ItemSize);
end;


procedure DynArrayDelete(var aDynArray: TGDynArray; iIndex: integer);
begin
  with aDynArray^ do
    if (iIndex < Count) and (iIndex >= 0) then
    begin
      if iIndex <> Count-1 then
        Windows.MoveMemory(DynArrayPtr(aDynArray, iIndex), DynArrayPtr(aDynArray, iIndex + 1), (Count - iIndex - 1) * ItemSize);
      DynArraySetLength(aDynArray, aDynArray^.Count - 1);
    end;
end;


function DynArrayCreate(const iItemSize, iCount: integer): TGDynArray;
var
  Capacity: integer;
begin
  Capacity := iCount div 16 + 1;
  Result := AllocMem(Sizeof(TGDynArrayRecord) + iItemSize * Capacity * 16);
  Result^.Capacity := Capacity;
  Result^.Count := iCount;
  Result^.ItemSize := iItemSize;
end;


function DynArrayClone(const aDynArray: TGDynArray): TGDynArray;
var
  iSize: integer;
begin
  if aDynArray <> nil then
  begin
    iSize := Sizeof(TGDynArrayRecord) + aDynArray^.ItemSize * aDynArray^.Capacity * 16;
    Result := AllocMem(iSize);
    Move(aDynArray^, Result^, iSize);
    Result^.RefCount := 0;
  end
  else
    Result := nil;
end;


function DynArraySizeOf(const aDynArray: TGDynArray) : integer;
begin
  if aDynArray = nil then
    Result := 0
  else
    Result := aDynArray^.Count * aDynArray^.Itemsize + Sizeof(TGDynArrayRecord);
end;

procedure DynArrayFree(var aDynArray: TGDynArray);
begin
  if aDynArray <> nil then
  begin
    if aDynArray^.RefCount = 0 then
      ReallocMem(aDynArray, 0)
    else
    begin
      DynArrayDecReference(aDynArray);
      aDynArray := nil;
    end;
  end;
end;


procedure DynArraySetLength(var aDynArray: TGDynArray; iCount: integer);
var
  CharPtr: PChar;
  NewCapacity: integer;
begin
  if aDynArray = nil then
    raise EGException.Create('DynArraySetLength: Invalid Parameter');

  if iCount <> aDynArray^.Count then
  begin
    NewCapacity := ( iCount div 16 ) + 1;
    if NewCapacity <> aDynArray^.Capacity then
    begin
      ReallocMem(aDynArray, Sizeof(TGDynArrayRecord) + aDynArray^.ItemSize * ( NewCapacity * 16 ));
      aDynArray^.Capacity := NewCapacity;
    end;

    if iCount > aDynArray^.Count then
    begin
      CharPtr := DynArrayPtr(aDynArray, aDynArray^.Count);
      FillChar(CharPtr^, aDynArray^.ItemSize * (iCount - aDynArray^.Count), 0);
    end;
    aDynArray^.Count := iCount;
  end;
end;



procedure TGChainStore.Add(PointStore: TGPointStore);
begin
  SetCount(Count + 1);
  PTGPointStore(DynArrayPtr(FChains, Count - 1))^ := PointStore;
end;


function TGChainStore.Centroid: TGPointLL;
var
  Area: extended;
  SumArea: extended;
  xSum: extended;
  ySum: extended;
  i: integer;
begin
  if Count = 0 then
    Result := PointLL(0,0)
  else
    try
      Result := Chain[0].Centroid(Area);
      if Count > 1 then
      begin
        xSum := Result.iLongX * Area;
        ySum := Result.iLatY * Area;
        SumArea := Area;
        for i := 1 to Count - 1 do
        begin
          Result := Chain[i].Centroid(Area);
          xSum := xSum + Result.iLongX * Area;
          ySum := ySum + Result.iLatY * Area;
          SumArea := SumArea + Area;
        end;
        if SumArea <> 0.0 then
        begin
          Result.iLongX := Round(xSum / SumArea);
          Result.iLatY := Round(ySum / SumArea);
        end;
      end;
    except
      Result := Chain[0][0];
    end;
end;

//--------------------------------------------------
//Description
//Calculates the MER of the chain store.
//
//Returns
//The MER of all the points held in the chain store.
//--------------------------------------------------


function TGChainStore.ChainStoreMER: TGMER;
var
  idx: integer;
begin
  if Count > 0 then
  begin
    Result := Chain[0].PointStoreMER;
    for idx := 1 to Count - 1 do
      if Chain[idx].Count > 0 then
        Result := MER_Union( Result, Chain[idx].PointStoreMER );
  end
  else
    Result := MER(0, 0, 0, 0);
end;


function TGChainStore.Clone: TGChainStore;
begin
  Result := TGChainStoreClass(Self.ClassType).Create;
  Result.FChains := DynArrayClone(FChains);
end;


constructor TGChainStore.Create;
begin
  inherited Create;
  FChains := DynArrayCreate(SizeOf(TGPointStore), 0);
end;


procedure TGChainStore.Delete(iIndex: Integer);
begin
  SetChain(iIndex, nil);
  DynArrayDelete(FChains, iIndex);
end;


destructor TGChainStore.Destroy;
var
  idx: integer;
begin
  for idx := 0 to FChains.Count - 1 do
    Chain[idx].Free;

  DynArrayFree(FChains);

  inherited Destroy;
end;


function TGChainStore.GetChain(iIndex: integer): TGPointStore;
begin
  if iIndex >= Count then
    Count := iIndex + 1;
  Result := TGPointStore(DynArrayAsObject(FChains, iIndex));

  if Result = nil then
  begin
    Result := TGPointStore.Create;
    Result.StoreHeight := FStoreHeight; // Set the default storeheight
    DynArraySetAsObject(FChains, iIndex, Result);
  end;
end;


function TGChainStore.GetCount: integer;
begin
  Result := FChains.Count;
end;


procedure TGChainStore.Insert(iIndex: Integer; PointStore: TGPointStore);
begin
  DynArrayInsert(FChains, iIndex);
  PTGPointStore(DynArrayPtr(FChains, iIndex))^ := PointStore;
end;


function TGChainStore.IntersectMER(const aMER: TGMER): Boolean;
var
  idx: integer;
begin
  Result := True;

  for idx := 0 to Count - 1 do
    if Chain[idx].IntersectMER( aMER ) then
      Exit;

  Result := False;
end;


function TGChainStore.Last: TGPointStore;
begin
  if Count = 0 then
    Result := Chain[0] // create the first object
  else
    Result := Chain[Count - 1];
end;


procedure TGChainStore.LoadFromXML(el: TGXML_ELement);
var
  idx : integer;
begin
  if el <> nil then
  begin
    StoreHeight := el.Attribute('StoreHeight', false);

    Count := el.ElementCount( 'Chain' );
    for idx := 0 to Count - 1 do
      Chain[idx].LoadFromXML( el.Element( 'Chain', idx ));
  end;
end;

procedure TGChainStore.Move(iFrom, iTo: integer);
begin
  DynArrayMove(FChains, iFrom, iTo);
end;


function TGChainStore.ObjectSize: integer;
var
  idx : integer;
begin
  Result := InstanceSize;

  for idx := 0 to Count - 1 do
    Result := Result + Chain[idx].ObjectSize;
end;


procedure TGChainStore.ReadChains( reader: TGStreamReader; chainFlags : TGChainStoreFlagSet );
var
  idx : integer;
begin
  StoreHeight := cfHeight in chainFlags;

  if cfMultiRing in ChainFlags then
    Count := reader.ReadSmallInt
  else
    Count := 1;

  for idx := 0 to Count - 1 do
    Chain[idx].ReadPoints( reader );
end;

procedure TGChainStore.ReadFromStream(aStream: TStream);
var
  reader: TGStreamReader;
  chainFlags : TGChainStoreFlagSet;
begin
  if aStream.Size > 0 then
  begin
    reader := TGStreamReader.Create( aStream );

    chainFlags := TGChainStoreFlagSet( Ord( reader.ReadByte ));
    ReadChains( reader, chainFlags );

    reader.Free;
  end;
end;


procedure TGChainStore.SaveToXML(el: TGXML_ELement);
var
  idx : integer;
begin
  el.AddAttribute('StoreHeight', StoreHeight, false);
  for idx := 0 to Count - 1 do
    Chain[idx].SaveToXML( el.AddElement( 'Chain' ));
end;


procedure TGChainStore.SetChain(iIndex: integer; Value: TGPointStore);
var
  Tmp: TGDynArray;
begin
  Tmp := PGDynArray(DynArrayPtr(FChains, iIndex))^;
  DynArrayFree(Tmp);
  PTGPointStore(DynArrayPtr(FChains, iIndex))^ := Value;
end;


procedure TGChainStore.SetCount(iNewCount: integer);
var
  idx: integer;
begin
  // free up the chains if reducing the count
  idx := FChains.Count;
  while idx > iNewCount do
  begin
    Dec(idx);
    DynArrayAsObject(FChains, idx).Free;
  end;

  DynArraySetLength(FChains, iNewCount);

  while idx < iNewCount do
  begin
    DynArraySetAsObject(FChains, idx, nil);
    Inc(idx);
  end;
end;


procedure TGChainStore.SetStoreHeight(const Value: Boolean);
var
  idx: integer;
begin
  if Value <> FStoreHeight then
  begin
    FStoreHeight := Value;

    for idx := 0 to Count - 1 do
      Chain[idx].StoreHeight := Value;
  end;
end;


procedure TGChainStore.WriteChains( writer: TGStreamWriter );
var
  idx : integer;
begin
  if Count > 1 then
    writer.WriteSmallInt( Count );

  for idx := 0 to Count - 1 do
    Chain[idx].WritePoints( writer );
end;

procedure TGChainStore.WriteToStream(aStream: TStream);
var
  writer: TGStreamWriter;
  chainFlags : TGChainStoreFlagSet;
begin
  writer := TGStreamWriter.Create( aStream );

  chainFlags := [];
  if StoreHeight then
    Include( chainFlags, cfHeight );

  if Count > 1 then
    Include( chainFlags, cfMultiRing );

  writer.WriteByte( Byte( chainFlags ));  // Save the flags to the stream
  WriteChains( writer );  // Save the chains to the stream

  writer.Free;
end;




function TGPointStore.Add(const ptLL: TGPointLL): integer;
begin
  FRefreshPending := true;

  Result := Count;
  SetCount(Result + 1);
  PutLL(Result, ptLL);
end;

function TGPointStore.AddDD( const Long, Lat : TGDecimalDegree ): integer;
begin
  FRefreshPending := true;

  Result := Count;
  SetCount(Result + 1);
  PutLL(Result, DDToPointLL(Long, Lat));
end;

procedure TGPointStore.Assign(Source: TPersistent);
begin
  if Source is TGPointStore then
  begin
    Flags := TGPointStore(Source).Flags;
    FPointArray := DynArrayClone(TGPointStore(Source).FPointArray);
  end
  else
    inherited;
end;


function TGPointStore.Centroid(var Area: Extended): TGPointLL;
var
  DangerZone: boolean;

  function GetPoint(i: integer): TGPointDD;
  var
    P: TGPointLL;
  begin
    if i = Count then
      P := asLL[0]
    else
      P := AsLL[i];
    with Result do
    begin
      LatY := P.iLatY * GU_TORADIANS;
      LongX := P.iLongX * GU_TORADIANS;
      if DangerZone and (LongX <= -HalfPi) then
        LongX := LongX + DoublePi;
    end;
  end;

var
  aSum: extended;
  i: integer;
  Ai: extended;
  xSum: extended;
  ySum: extended;
  PI: TGPointDD;
  PJ: TGPointDD;
begin
  Area := 0.0;
  case Count of
    0:
      begin
        Result.iLongX := 0;
        Result.iLatY := 0;
      end;
    1:
      Result := asLL[0];
    2:
      begin
        Result.iLongX := (asLL[0].iLongX + asLL[1].iLongX) div 2;
        Result.iLatY := (asLL[0].iLatY + asLL[1].iLatY) div 2;
      end;
  else
    begin
      with PointStoreMER do
        DangerZone := (iLongX + iWidthX) > GU_180_DEGREE;
      aSum := 0.0;
      xSum := 0.0;
      ySum := 0.0;
      PJ := GetPoint(0);
      for i := 0 to Count - 1 do
      begin
        PI := PJ;
        PJ := GetPoint(i + 1);
        Ai := PI.LongX * PJ.LatY - PJ.LongX * PI.LatY;
        aSum := aSum + Ai;
        xSum := xSum + (PJ.LongX + PI.LongX) * Ai;
        ySum := ySum + (PJ.LatY + PI.LatY) * Ai
      end;
      Area := 0.5 * aSum; // in square radians!!

      if aSum <> 0.0 then
      begin
        xSum := xSum / (3.0 * aSum);
        if xSum >= LocalPi then
          xSum := xSum - DoublePi;
        ySum := ySum / (3.0 * aSum);
        if ySum < -HalfPi then
          ySum := ySum + HalfPi;
      end;
      Result.iLongX := Round(xSum * GU_FROMRADIANS);
      Result.iLatY := Round(ySum * GU_FROMRADIANS);
    end;
  end; //case
  Result.iHeightZ := 0;
end;




function TGPointStore.Clone: TGPointStore;
begin
  Result := TGPointStoreClass(Self.ClassType).Create;
  Result.Flags := Flags;
  Result.FPointArray := DynArrayClone(FPointArray);
end;


procedure TGPointStore.Delete(iIndex: Integer);
begin
  FRefreshPending := true;
  DynArrayDelete(FPointArray, iIndex);
end;


destructor TGPointStore.Destroy;
begin
  DynArrayFree(FPointArray);
  FPoint3DArray := nil;
  inherited Destroy;
end;


function TGPointStore.GetCount: Integer;
begin
  if FPointArray = nil then
    Result := 0
  else
    Result := FPointArray.Count
end;


function TGPointStore.GetDD(iIndex: integer): TGPointDD;
begin
  with AsLL[iIndex] do
  begin
    Result.LongX := iLongX * (1.0 / GU_DEGREE);
    Result.LatY := iLatY * (1.0 / GU_DEGREE);
    Result.HeightZ := iHeightZ * (1.0 / GU_DEGREE);
  end;
end;


function TGPointStore.GetHeightFlag: Boolean;
begin
  Result := (psHeight in Flags);
end;


function TGPointStore.GetHidden: Boolean;
begin
  Result := (psHidden in Flags);
end;

function TGPointStore.GetLL(iIndex: integer): TGPointLL;
var
  iPtr: PInteger;
begin
  iPtr := PInteger(DynArrayPtr(FPointArray, iIndex));

  Result.iLongX := iPtr^;
  Inc(iPtr);
  Result.iLatY := iPtr^;

  if psHeight in Flags then
  begin
    Inc(iPtr);
    Result.iHeightZ := iPtr^;
  end
  else
    Result.iHeightZ := 0;
end;


procedure TGPointStore.Insert(iIndex: Integer; const ptLL: TGPointLL);
begin
  FRefreshPending := true;
  DynArrayInsert(FPointArray, iIndex);
  PutLL(iIndex, ptLL);
end;


function TGPointStore.IntersectMER( const aMER: TGMER): Boolean;
var
  idx : integer;
begin
  Result := True;

  for idx := 0 to Count - 1 do
    if MER_ContainsLL( aMER, AsLL[idx] ) then
      Exit;

  Result := False;
end;


procedure TGPointStore.LoadFromXML(el: TGXML_ELement);
var
  idx : integer;
begin
  Count := 0; // to clear out any existing data
  if el <> nil then
  begin
    Count := el.ElementCount( 'LL' );
    for idx := 0 to Count - 1 do
      AsLL[idx] := StrToPointLL( el.Element( 'LL', idx ).BodyText );
  end;
end;

function TGPointStore.ObjectSize: integer;
begin
  Result := InstanceSize;
  Inc( Result,  DynArraySizeOf( FPointArray ));
  Inc( Result, Length( FPoint3DArray ) * SizeOf( TGPoint3D ));
end;

function TGPointStore.Point3DArray: TGPoint3DArray;
var
  idx: integer;
  eSLong, eClong, eSLat, eCLat, eHeight, eTmp: Extended;
begin
  if FRefreshPending or ( Length( FPoint3DArray ) = 0 ) then
  begin
    FRefreshPending := false;
    SetLength( FPoint3DArray, Count );

    for idx := 0 to High( FPoint3DArray ) do
    begin
      with GetLL(idx) do
      begin
        SinCos(iLongX * GU_TORADIANS, eSLong, eClong);
        SinCos(iLatY * GU_TORADIANS, eSLat, eCLat);

        eHeight := GU_EARTHRADIUS + iHeightZ;
        eTmp := eHeight * eCLat;
      end;
      with FPoint3DArray[idx] do
      begin
        X := eTmp * eSlong;
        Y := eHeight * eSLat;
        Z := eTmp * eCLong;
      end;
    end;
  end;
  Result := FPoint3DArray;
end;

function TGPointStore.PointInPolygon(iLong, iLat: integer; hitZone : TGMERHitZone): Boolean;
var
  idx, jdx: integer;
  iX1, iX2, iY1, iY2: integer;
  iPtr: Pinteger;
begin
  Result := False;

  if hitZone = hzNone then
    Exit;

  iX2 := 0;
  iY2 := 0;
  for jdx := Count - 1 downto 0 do
  begin
    iPtr := PInteger(DynArrayPtr(FPointArray, jdx));
    iX2 := iPtr^;
    Inc(iPtr);
    iY2 := iPtr^;
    if (hitZone = hzMinus180 ) and (iX2 >= 0) then
      continue;
    if (hitZone = hzPlus180 ) and (iX2 <= 0) then
      continue;
    Break;
  end;

  for idx := 0 to Count - 1 do
  begin
    iPtr := PInteger(DynArrayPtr(FPointArray, idx));
    iX1 := iPtr^;
    Inc(iPtr);
    iY1 := iPtr^;

    if hitZone <> hzHit then
    begin
      if (hitZone = hzMinus180) and (iX1 > 0) then
        continue;
      if (hitZone = hzPlus180) and (iX1 < 0) then
        continue;
    end;

    if ( ((iY1 <= iLat) and (iLat < iY2)) or ((iY2 <= iLat) and (iLat < iY1)) )
      and ( iLong < MulDiv(iX2 - iX1, iLat - iY1, iY2 - iY1) + iX1 ) then
      Result := not Result;

    iX2 := iX1;
    iY2 := iY1;
  end;
end;

function TGPointStore.PointOnEdge(iLong, iLat, iTolerance: Integer; bClosed : Boolean): integer;
var
  ptLast, ptNew: TGPointLL;
  edge, latY : integer;
begin
  if Count > 0 then
  begin
    ptLast := AsLL[0];
    Result := 1;
    while Result < Count do
    begin
      ptNew := AsLL[Result];

      if Cross180( ptLast.iLongX, ptNew.iLongX ) then
      begin
        edge := GU_180_DEGREE - 1;
        if ptLast.iLongX < 0 then
          edge := -edge;

        latY := ptLast.iLatY + Round( (ptLast.iLatY - ptNew.iLatY)
          * (( edge - ptLast.iLongX ) / LongDiff(ptLast.iLongX, ptNew.iLongX)));

        if PointOnLine(iLong, iLat, edge, latY, ptLast.iLongX, ptLast.iLatY, iTolerance) then
          Exit;
        if PointOnLine(iLong, iLat, -edge, latY, ptNew.iLongX, ptNew.iLatY, iTolerance) then
          Exit;
      end
      else
        if PointOnLine(iLong, iLat, ptLast.iLongX, ptLast.iLatY, ptNew.iLongX, ptNew.iLatY, iTolerance) then
          Exit;

      ptLast := ptNew;
      Inc( Result );
    end;

    if bClosed then
      if PointOnLine(iLong, iLat, ptLast.iLongX, ptLast.iLatY, AsLL[0].iLongX, AsLL[0].iLatY, iTolerance) then
        Exit;
  end;
  Result := -1;
end;


function TGPointStore.PointStoreMER: TGMER;
var
  iMaxX, iMinX, iMaxY, iMinY, iMinPlus, iMaxMinus: Integer;
  idx: Integer;
  iLongX, iLatY: Integer;
  iPtr: PInteger;
  tmp : Extended;
//  iLastZone, iCross180Count: Integer;

{  procedure Cross180(iLong: Integer);
  var
    iZone: integer;
  begin
    iZone := iLong div GU_90_DEGREE;
    if ((iZone < 0) and (iLastZone > 0)) or ((iZone > 0) and (iLastZone < 0)) then
      Inc(iCross180Count);
    iLastZone := iZone;
  end;
}
begin
  if Count = 0 then
  begin
    MER_Empty( Result );
    Exit;
  end;

  iMinY := MaxInt;
  iMaxY := -MaxInt;

  iMinX := MaxInt;
  iMaxX := -MaxInt;

  iMinPlus := MaxInt;
  iMaxMinus := -MaxInt;


  for idx := 0 to Count - 1 do
  begin
    iPtr := PInteger(DynArrayPtr(FPointArray, idx));
    iLongX := iPtr^;

    if iLongX >= 0 then
    begin
      if iLongX < iMinPlus then
        iMinPlus := iLongX;
    end
    else
      if iLongX > iMaxMinus then
        iMaxMinus := iLongX;

    if iLongX < iMinX then iMinX := iLongX;
    if iLongX > iMaxX then iMaxX := iLongX;

    Inc(iPtr);
    iLatY := iPtr^;

    if iLatY < iMinY then iMinY := iLatY;
    if iLatY > iMaxY then iMaxY := iLatY;
  end;

  if ( iMinPlus < MaxInt ) and ( iMaxMinus > -MaxInt ) then  // crosses 180 or prime meridian
  begin
    tmp := iMaxMinus + GU_180_DEGREE;
    tmp := tmp + GU_180_DEGREE - iMinPlus;

    if (iMaxX - iMinX) > tmp then
      Result := MER(iMinPlus, iMinY, Round( tmp ) + 1, ( iMaxY - iMinY ) + 1)
    else
      Result := MER(iMinX, iMinY, (iMaxX - iMinX) + 1, (iMaxY - iMinY) + 1);
  end
  else
    Result := MER(iMinX, iMinY, (iMaxX - iMinX) + 1, (iMaxY - iMinY) + 1);


{
  Cross180(AsLL[Count - 1].iLongX); // initialise Cross180
  iCross180Count := 0;

  for idx := 0 to Count - 1 do
  begin
    iPtr := PInteger(DynArrayPtr(FPointArray, idx));
    iLongX := iPtr^;
    Inc(iPtr);
    iLatY := iPtr^;

    Cross180(iLongX);

    if (iLongX >= 0) and (iLongX < iMinPlus) then iMinPlus := iLongX;
    if (iLongX < 0) and (iLongX > iMaxMinus) then iMaxMinus := iLongX;

    if iLongX < iMinX then iMinX := iLongX;
    if iLongX > iMaxX then iMaxX := iLongX;

    if iLatY < iMinY then iMinY := iLatY;
    if iLatY > iMaxY then iMaxY := iLatY;
  end;

  if (iCross180Count mod 2) <> 0 then
    Result := MER(-GU_180_DEGREE + 1, iMinY, GU_360_DEGREE - 2, iMaxY - iMinY)
  else
    if iCross180Count > 0 then
    begin
      tmp := (GU_180_DEGREE - iMinPlus);
      tmp := tmp + (iMaxMinus + GU_180_DEGREE);
      Result := MER(iMinPlus, iMinY,  Round( tmp ) + 1, ( iMaxY - iMinY ) + 1);
    end
    else
      Result := MER(iMinX, iMinY, (iMaxX - iMinX ) + 1, (iMaxY - iMinY) + 1);
}
end;


procedure TGPointStore.PutDD(iIndex: integer; const pt: TGPointDD);
begin
  FRefreshPending := true;

  AsLL[iIndex] := PointLLH(
    Round( pt.LongX * GU_DEGREE ),
    Round( pt.LatY  * GU_DEGREE ),
    Round( pt.HeightZ * GU_DEGREE ));
end;


procedure TGPointStore.PutLL(iIndex: integer; const pt: TGPointLL);
var
  iPtr: PInteger;
begin
  FRefreshPending := true;

  iPtr := PInteger(DynArrayPtr(FPointArray, iIndex));

  iPtr^ := pt.iLongX;
  Inc(iPtr);
  iPtr^ := pt.iLatY;

  if psHeight in Flags then
  begin
    Inc(iPtr);
    iPtr^ := pt.iHeightZ;
  end;
end;


procedure TGPointStore.ReadPoints( reader : TGStreamReader );
var
  dX, dY, dH, idx : integer;
  iLastX, iLastY, iLastH : integer;
  bHeight : Boolean;

  {---------------------------------------------------}
  function ReadPoint16BitDiff : Boolean;
  begin
    dX := reader.ReadShortInt;
    Result := dX <> 127;
    if Result then
    begin
      dY := reader.ReadShortInt;
      if bHeight then
        dH := reader.ReadShortInt
      else
        dH := 0;
    end;
  end;

  {---------------------------------------------------}
  function ReadPoint32BitDiff : Boolean;
  begin
    dX := reader.ReadSmallInt;
    Result := dX <> 32767;
    if Result then
    begin
      dY := reader.ReadSmallInt;
      if bHeight then
        dH := reader.ReadSmallInt
      else
        dH := 0;
    end;
  end;

begin
  FPoint3DArray := nil;
  DynArrayFree(FPointArray);

  reader.ReadBuffer( Flags, SizeOf(TGPointStoreFlagsSet));  // Get the flags for the pointstore

  if psLongCount in Flags then
    Count := reader.ReadInteger // get number of points in ring
  else
    Count := reader.ReadWord; // get number of points in ring

  if Count > 0 then
  begin
    bHeight := psHeight in Flags;
    AsLL[0] := reader.ReadPointLL( bHeight, ps16Bit in Flags ); // read first point
    with AsLL[0] do
    begin
      iLastX := iLongX;
      iLastY := iLatY;
      iLastH := iHeightZ;
    end;

    if ps16Bit in Flags then
    begin
      iLastX := iLastX div GU_MINUTE;
      iLastY := iLastY div GU_MINUTE;
      iLastH := iLastH div GU_MINUTE;

      for idx := 1 to Count - 1 do
        if ReadPoint16BitDiff then
        begin
          Inc( iLastX, dX );
          Inc( iLastY, dY );
          Inc( iLastH, dH );
          AsLL[idx] := PointLLH( iLastX * GU_MINUTE, iLastY * GU_MINUTE, iLastH * GU_MINUTE );
        end
        else
        begin
          AsLL[idx] := reader.ReadPointLL( bHeight, True );
          with AsLL[idx] do
          begin
            iLastX := iLongX div GU_MINUTE;
            iLastY := iLatY div GU_MINUTE;
            iLastH := iHeightZ div GU_MINUTE;
          end;
        end;
    end
    else
    begin
      for idx := 1 to Count - 1 do
        if ReadPoint32BitDiff then
        begin
          Inc( iLastX, dX );
          Inc( iLastY, dY );
          Inc( iLastH, dH );
          AsLL[idx] := PointLLH( iLastX, iLastY, iLastH );
        end
        else
        begin
          AsLL[idx] := reader.ReadPointLL( bHeight, False );
          with AsLL[idx] do
          begin
            iLastX := iLongX;
            iLastY := iLatY;
            iLastH := iHeightZ;
          end;
        end;
    end;
  end;
end;


procedure TGPointStore.SaveToXML(el: TGXML_ELement);
var
  idx : integer;
begin
  for idx := 0 to Count - 1 do
    el.AddElement( 'LL' ).BodyText := PointLLToStr( AsLL[idx] );
end;


procedure TGPointStore.SetCount(iNewCount: Integer);
begin
  FRefreshPending := true;

  if FPointArray = nil then
  begin
    if psHeight in Flags then
      FPointArray := DynArrayCreate(SizeOf(TGPointLL), iNewCount)
    else
      FPointArray := DynArrayCreate(SizeOf(TPoint), iNewCount);
  end
  else
    DynArraySetLength(FPointArray, iNewCount);
end;


procedure TGPointStore.SetHeightFlag(Value: Boolean);
var
  tmpStore: TGPointStore;
  idx: integer;
begin
  if Value <> (psHeight in Flags) then
  begin
    tmpStore := Clone; // Save the points then copy them all back in with the new Height state

    if Value then
      Include(Flags, psHeight)
    else
      Exclude(Flags, psHeight);

    DynArrayFree(FPointArray); // Delete all existing point in the PointStore
    Count := tmpStore.Count;
    for idx := 0 to Count - 1 do
      asLL[idx] := tmpStore.asLL[idx];
    tmpStore.Free;
  end;
end;


procedure TGPointStore.SetHidden(const Value: Boolean);
begin
  if Value then
    Include( Flags, psHidden )
  else
    Exclude( Flags, psHidden );
end;

procedure TGPointStore.Translate(dx, dy, dz: integer);
var
  idx: integer;
  iPtr: Pinteger;
begin
  iPtr := PInteger(DynArrayPtr(FPointArray, 0));

  if psHeight in Flags then
  begin
    for idx := 0 to Count - 1 do
    begin
      iPtr^ := Mod180(iPtr^ + dx);
      Inc(iPtr);
      iPtr^ := iPtr^ + dy;
      Inc(iPtr);
      iPtr^ := iPtr^ + dz;
      Inc(iPtr);
    end;
  end
  else
    for idx := 0 to Count - 1 do
    begin
      iPtr^ := Mod180(iPtr^ + dx);
      Inc(iPtr);
      iPtr^ := iPtr^ + dy;
      Inc(iPtr);
    end;

  FRefreshPending := true;
end;


procedure TGPointStore.WritePoints( writer : TGStreamWriter );
var
  ptLast, ptLL : TGPointLL;
  dX, dY, dH, idx, iLimit : Integer;
  iDivisor : Integer;

 {---------------------------------------------------}
  procedure WritePointDiff( Flags : TGPointStoreFlagsSet );
  begin
    if ps16Bit in Flags then
    begin
      writer.WriteShortInt( dX );
      writer.WriteShortInt( dY );
      if psHeight in Flags then
        writer.WriteShortInt( dH );
    end
    else
    begin
      writer.WriteSmallInt( dX );
      writer.WriteSmallInt( dY );
      if psHeight in Flags then
        writer.WriteSmallInt( dH );
    end;
  end;

begin
  if Count > 1024 * 64 then
    Include( Flags, psLongCount )
  else
    Exclude( Flags, psLongCount );

  writer.WriteBuffer( Flags, SizeOf( TGPointStoreFlagsSet )); // write pointstore flags

  // Set the delta limit size
  if ps16Bit in Flags then
  begin
    iLimit := 127;
    iDivisor := GU_MINUTE;
  end
  else
  begin
    iLimit := 32767;
    iDivisor := 1;
  end;

  if psLongCount in Flags then
    writer.WriteInteger( Count ) // write number of points in ring
  else
    writer.WriteWord( Count ); // write number of points in ring

  if Count > 0 then
  begin
    ptLast := AsLL[0];

    writer.WritePointLL( ptLast, psHeight in Flags, ps16bit in Flags );

    for idx := 1 to Count - 1 do
    begin
      ptLL := AsLL[idx];

      dX := ( ptLL.iLongX - ptLast.iLongX ) div iDivisor;
      dY := ( ptLL.iLatY - ptLast.iLatY ) div iDivisor;
      dH := ( ptLL.iHeightZ - ptLast.iHeightZ ) div iDivisor;

      if ( Abs( dX ) >= iLimit ) or ( Abs( dY ) >= iLimit ) or ( Abs( dH ) >= iLimit ) then
      begin
        if ps16Bit in Flags then
          writer.WriteShortInt( iLimit )
        else
          writer.WriteSmallInt( iLimit );
        writer.WritePointLL( ptLL, psHeight in Flags, ps16bit in Flags );
      end
      else
        WritePointDiff( Flags );

      ptLast := ptLL;
    end;
  end;
end;



function TGNoRefCountObject._AddRef: Integer;
begin
  Result := -1;   // -1 indicates no reference counting is taking place
end;

function TGNoRefCountObject._Release: Integer;
begin
  Result := -1;   // -1 indicates no reference counting is taking place
end;

function TGNoRefCountObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

constructor TGBufferStream.Create(buffer: Pointer; len : Cardinal);
begin
  SetPointer(buffer, len);
end;

function TGBufferStream.Write(const Buffer; Count: Integer): Longint;
var
  Pos: Longint;
begin
  if (Position >= 0) and (Count >= 0) then
  begin
    Pos := Position + Count;
    if Pos > 0 then
    begin
      if Pos > Size then
        raise EGException.Create('TGBufferStream.Write: Buffer overflow');
      System.Move(Buffer, Pointer(Longint(Memory) + Position)^, Count);
      Position := Pos;
      Result := Count;
      Exit;
    end;
  end;
  Result := 0;
end;


procedure TGStreamReader._Read(var Buffer; iSize : integer);
begin
  DataStream.Read(Buffer, iSize);
end;


constructor TGStreamReader.Create(aStream : TStream);
begin
  inherited Create;
  DataStream := aStream;
end;


destructor TGStreamReader.Destroy;
begin
  DataStream := nil;
  inherited Destroy;
end;


procedure TGStreamReader.ReadBuffer(var Buffer; iSize : integer);
begin
  DataStream.Read(Buffer, iSize);
//  _Read(Buffer, iSize);
end;


function TGStreamReader.ReadBoolean : Boolean;
var
  Chr : Char;
begin
  _Read( Chr, 1);
  Result := Chr = 'T';
end;


function TGStreamReader.ReadByte: Byte;
begin
  _Read(Result, SizeOf(Byte));
end;


function TGStreamReader.ReadDouble : Double;
begin
  _Read(Result, SizeOf(Double));
end;


function TGStreamReader.ReadWord : Word;
begin
  DataStream.Read(Result, SizeOf(Word));
//  _Read(Result, SizeOf(Word));
end;


function TGStreamReader.ReadShortInt : ShortInt;
begin
  _Read(Result, SizeOf(ShortInt));
end;


function TGStreamReader.ReadSmallInt : Smallint;
begin
  DataStream.Read(Result, SizeOf(Smallint));
//  _Read(Result, SizeOf(Smallint));
end;


function TGStreamReader.ReadInteger : Integer;
begin
  DataStream.Read(Result, SizeOf(Integer));
//  _Read(Result, SizeOf(Integer));
end;


function TGStreamReader.ReadLongString : String;
begin
  SetLength( Result, ReadInteger );
  _Read( Pointer(Result)^, Length( Result ));
end;

function TGStreamReader.ReadShortString : ShortString;
begin
  _Read(Result[0], 1);
  _Read(Result[1], Ord(Result[0]));
end;


function TGStreamReader.ReadString : String;
begin
  SetLength( Result, ReadWord );
  _Read( Pointer(Result)^, Length( Result ));
end;


function TGStreamReader.ReadPoint : TPoint;
begin
  Result.X := ReadInteger;
  Result.Y := ReadInteger;
end;


function TGStreamReader.ReadPointLL(bHeight, b16Bit : Boolean) : TGPointLL;
var
  iTmpX, iTmpY, iTmpH : Integer;
begin
  iTmpH := 0;
  if b16Bit then
  begin
    iTmpX := ReadSmallInt * GU_MINUTE;
    iTmpY := ReadSmallInt * GU_MINUTE;
    if bHeight then
      iTmpH := ReadSmallInt * GU_MINUTE;
  end
  else
  begin
    iTmpX := ReadInteger;
    iTmpY := ReadInteger;
    if bHeight then
      iTmpH := ReadInteger;
  end;
  Result := PointLLH(iTmpX, iTmpY, iTmpH);
end;


function TGStreamReader.ReadRect : TRect;
begin
  Result.Left := ReadInteger;
  Result.Top := ReadInteger;
  Result.Right := ReadInteger;
  Result.Bottom := ReadInteger;
end;


function TGStreamReader.ReadMER : TGMER;
begin
  Result.iLongX := ReadInteger;
  Result.iLatY := ReadInteger;
  Result.iWidthX := ReadInteger;
  Result.iHeightY := ReadInteger;
end;



procedure TGStreamWriter._Write(const Buffer; iSize : integer);
begin
  DataStream.Write(Buffer, iSize);
end;


constructor TGStreamWriter.Create(aStream : TStream);
begin
  inherited Create;
  DataStream := aStream;
end;


destructor TGStreamWriter.Destroy;
begin
  DataStream := nil;
  inherited Destroy;
end;


procedure TGStreamWriter.WriteBuffer(const Buffer; iSize : integer);
begin
  _Write(Buffer, iSize);
end;


procedure TGStreamWriter.WriteBoolean(bVal : Boolean);
var
  Chr : Char;
begin
  Chr := 'FT'[Ord(bVal)+1];
  _Write( Chr, 1 );
end;


procedure TGStreamWriter.WriteByte(Val : Byte);
begin
  _Write(Val, SizeOf(Byte));
end;


procedure TGStreamWriter.WriteDouble(Val : Double);
begin
  _Write(Val, SizeOf(Double));
end;


procedure TGStreamWriter.WriteWord(iVal : Word);
begin
  _Write(iVal, SizeOf(Word));
end;


procedure TGStreamWriter.WriteShortInt(iVal : ShortInt);
begin
  _Write(iVal, SizeOf(Shortint));
end;

{------------------------------------------------------------------------------
  TGStreamWriter.WriteSmallInt
------------------------------------------------------------------------------}
{**
  @Param iVal Smallint to write.
}
procedure TGStreamWriter.WriteSmallInt(iVal : Smallint);
begin
  _Write(iVal, SizeOf(Smallint));
end;


procedure TGStreamWriter.WriteInteger(iVal : Integer);
begin
  _Write(iVal, SizeOf(Integer));
end;

procedure TGStreamWriter.WriteLongString(const sValue : String);
begin
  WriteInteger( Length(sValue));
  _Write( Pointer(sValue)^, Length( sValue ));
end;

procedure TGStreamWriter.WriteShortString(const sValue : ShortString);
begin
  _Write(sValue, Length(sValue) + 1);
end;


procedure TGStreamWriter.WriteString(const sValue : String);
begin
  WriteWord( Length(sValue));
  _Write( Pointer(sValue)^, Length( sValue ));
end;


procedure TGStreamWriter.WritePoint(const pt : TPoint);
begin
  WriteInteger(pt.X);
  WriteInteger(pt.Y);
end;


procedure TGStreamWriter.WritePointLL(const ptLL : TGPointLL; bHeight, b16Bit : Boolean);
begin
  if b16Bit then
  begin
    WriteSmallInt(ptLL.iLongX div GU_MINUTE);
    WriteSmallInt(ptLL.iLatY div GU_MINUTE);
    if bHeight then
      WriteSmallInt(ptLL.iHeightZ div GU_MINUTE);
  end
  else
  begin
    WriteInteger(ptLL.iLongX);
    WriteInteger(ptLL.iLatY);
    if bHeight then
      WriteInteger(ptLL.iHeightZ);
  end;
end;


procedure TGStreamWriter.WriteRect(const ARect : TRect);
begin
  with ARect do
  begin
    WriteInteger(Left);
    WriteInteger(Top);
    WriteInteger(Right);
    WriteInteger(Bottom);
  end;
end;


procedure TGStreamWriter.WriteMER(const MER : TGMER);
begin
  with MER do
  begin
    WriteInteger(iLongX);
    WriteInteger(iLatY);
    WriteInteger(iWidthX);
    WriteInteger(iHeightY);
  end;
end;



function TGInterfacedPersistent._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TGInterfacedPersistent._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

procedure TGInterfacedPersistent.AfterConstruction;
begin
// Release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

procedure TGInterfacedPersistent.BeforeDestruction;
begin
  if RefCount <> 0 then
    raise EGException.Create( 'TGInterfacedPersistent: Invalid Pointer' );
end;

class function TGInterfacedPersistent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TGInterfacedPersistent(Result).FRefCount := 1;
end;

function TGInterfacedPersistent.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;



procedure POZ_LoadFromXML(var poz : TGPOZ; Element: TGXML_Element);
begin
  if Element <> nil then
    with poz do
    begin
      eSF := Element.Attribute('SF', 0.0);
      CenterXY.X := Round( Element.Attribute('CenterX', 0));
      CenterXY.Y := Round( Element.Attribute('CenterY', 0));
      iRotX := Element.Attribute('RotX', 0);
      iRotY := Element.Attribute('RotY', 0);
      iRotZ := Element.Attribute('RotZ', 0);
      iWidth := Element.Attribute('Width', 0);
      iHeight := Element.Attribute('Height', 0);
    end;
end;

procedure POZ_Normalise(var poz : TGPOZ; width, height: integer);
var
  ratio : Extended;
begin
  with poz do
  begin
    if ( iWidth = 0 ) or ( iHeight = 0 ) then
    begin
      iWidth := width;
      iHeight := height;
      CenterXY := Point( width div 2, height div 2 );
    end;
    
    ratio := Hypot( width, height ) / Hypot( iWidth, iHeight );

    eSF := ratio * eSF;
    CenterXY := Point( Round( ratio * CenterXY.X ), Round( ratio * CenterXY.Y ));
  //  CenterXY := Point( Round(( width / iWidth) * CenterXY.X ), Round(( height / iHeight ) * CenterXY.Y ));

    iWidth := width;
    iHeight := height;
  end;
end;

procedure POZ_SaveToXML(const poz : TGPOZ; Element: TGXML_Element);
begin
  if Element <> nil then
    with poz do
    begin
      Element.AddAttribute('SF', eSF, 0.0);
      Element.AddAttribute('CenterX', CenterXY.X, 0);
      Element.AddAttribute('CenterY', CenterXY.Y, 0);
      Element.AddAttribute('RotX', iRotX, 0);
      Element.AddAttribute('RotY', iRotY, 0);
      Element.AddAttribute('RotZ', iRotZ, 0);
      Element.AddAttribute('Width', iWidth, 0);
      Element.AddAttribute('Height', iHeight, 0);
    end;
end;



function Quat_Dot(const q1 : TGQuaternion; const q2: TGQuaternion): Double;
begin
  with q1 do
    Result := X * q2.X + Y * q2.Y + Z * q2.Z + W * q2.W;
end;

procedure Quat_FromAxisAngle(var q : TGQuaternion; const axis: TGVec3D; const angle: Double);
var
  eCos, eSin: Extended;
begin
  SinCos(angle * 0.5, eSin, eCos);

  with q do
  begin
    x := axis.x * eSin;
    y := axis.y * eSin;
    z := axis.z * eSin;
    w := eCos;
  end;
end;

procedure Quat_FromEuler( var q : TGQuaternion; const RotX, RotY, RotZ: TGDecimalDegree);
var
  eSX, eCX, eSY, eCY, eSZ, eCZ: Extended;
begin
  SinCos((LocalPI - RotX * DD_TORADIANS) * 0.5, eSX, eCX);
  SinCos(RotY * DD_TORADIANS * 0.5, eSY, eCY);
  SinCos(RotZ * DD_TORADIANS * 0.5, eSZ, eCZ);

  with q do
  begin
    X := (eCY * eSX * eCZ) - (eSY * eCX * eSZ);
    Y := (eCY * eSX * eSZ) + (eSY * eCX * eCZ);
    Z := (eCY * eCX * eSZ) - (eSY * eSX * eCZ);
    W := (eCY * eCX * eCZ) + (eSY * eSX * eSZ);
  end;
end;

procedure Quat_FromVectors(var q : TGQuaternion; const vec1, vec2: TGVec3D);
var
  vec: TGVec3D;
  CosAngle, eTmp: Double;
begin
  CosAngle := Vec3D_Dot(vec1, vec2);

  { check if parallel }
  with q do
    if CosAngle > 0.99999 then
    begin
      X := 0.0;
      Y := 0.0;
      Z := 0.0;
      W := 1.0;
    end
    else
      if CosAngle < -0.99999 then { check if opposite }
      begin
        { check if we can use cross product of from vector with [1, 0, 0] }
        vec := V3D(0, vec1.x, -vec1.y);
        with vec do
          eTmp := sqrt(y * y + z * z); { get length of vector }

        if eTmp < TG_EPSILON then { we need cross product of from vector with [0, 1, 0] }
          vec := V3D(-vec1.z, 0, vec1.x);

        X := vec.X;
        Y := vec.Y;
        Z := vec.Z;
        W := 0.0;
      end
      else
      begin
        eTmp := Sqrt(0.5 * (1.0 - CosAngle));
        with Vec3D_Cross(vec1, vec2) do
        begin
          q.X := X * eTmp;
          q.Y := Y * eTmp;
          q.Z := Z * eTmp;
          q.W := sqrt(0.5 * (1.0 + CosAngle));
        end;
      end;
end;

function Quat_Multiply(const q1 : TGQuaternion; const q2 : TGQuaternion): TGQuaternion;
begin
  with q1 do
  begin
    Result.X := q2.W * X + q2.X * W + q2.Y * Z - q2.Z * Y;
    Result.Y := q2.W * Y + q2.Y * W + q2.Z * X - q2.X * Z;
    Result.Z := q2.W * Z + q2.Z * W + q2.X * Y - q2.Y * X;
    Result.W := q2.W * W - q2.X * X - q2.Y * Y - q2.Z * Z;
  end;
end;

procedure Quat_Normalise( var q : TGQuaternion );
var
  eLen: Extended;
begin
  with q do
  begin
    eLen := x * x + y * y + z * z + w * w;
    if eLen > 0.0 then
      eLen := 1.0 / Sqrt(eLen)
    else
      eLen := 1.0;

    x := x * eLen;
    y := y * eLen;
    z := z * eLen;
    w := w * eLen;
  end;
end;

procedure Quat_ToAxisAngle( const q : TGQuaternion; var Axis: TGVec3D; var Angle: Double);
var
  eLen: Extended;
begin
  with q do
  begin
    eLen := x * x + y * y + z * z;

    if eLen > TG_EPSILON then
    begin
      eLen := 1.0 / eLen;
      Axis := V3D(x * eLen, y * eLen, z * eLen);
      Angle := 2.0 * ArcCos(w);
    end
    else
    begin
      Axis := V3D(0.0, 0.0, 1.0);
      Angle := 0.0;
    end;
  end;
end;

procedure Quat_ToMatrix( const q : TGQuaternion; var mat: TGMatrix);
var
  x2, y2, z2, xx, xy, xz, yy, yz, zz, wx, wy, wz: Double;
begin
  with q do
  begin
    x2 := x + x;
    y2 := y + y;
    z2 := z + z;
    xx := x * x2;
    xy := x * y2;
    xz := x * z2;
    yy := y * y2;
    yz := y * z2;
    zz := z * z2;
    wx := w * x2;
    wy := w * y2;
    wz := w * z2;
  end;
  
  mat.terms[0] := 1.0 - (yy + zz);
  mat.terms[1] := xy + wz;
  mat.terms[2] := xz - wy;
  mat.terms[4] := xy - wz;
  mat.terms[5] := 1.0 - (xx + zz);
  mat.terms[6] := yz + wx;
  mat.terms[8] := xz + wy;
  mat.terms[9] := yz - wx;
  mat.terms[10] := 1.0 - (xx + yy);
end;



function Vec3D_Cross(const v1: TGVec3D; v2: TGVec3D): TGVec3D;
begin
  with v1 do
  begin
    Result.x := y * v2.z - z * v2.y;
    Result.y := z * v2.x - x * v2.z;
    Result.z := x * v2.y - y * v2.x;
    Vec3D_Normalise( Result );
  end;
end;

function Vec3D_Dot(const v1: TGVec3D; v2: TGVec3D): Double;
begin
  with v1 do
    Result := x * v2.x + y * v2.y + z * v2.z;
end;

function Vec3D_MatrixMultiply(const v: TGVec3D; mat: TGMatrix ): TGVec3D;
begin
  with v do
  begin
    Result.X := mat.terms[0] * X + mat.terms[1] * Y + mat.terms[2] * Z;
    Result.Y := mat.terms[4] * X + mat.terms[5] * Y + mat.terms[6] * Z;
    Result.Z := mat.terms[8] * X + mat.terms[9] * Y + mat.terms[10] * Z;
    Vec3D_Normalise(Result);
  end;
end;

function Vec3D_Minus(const v1: TGVec3D; v2: TGVec3D): TGVec3D;
begin
  with v1 do
  begin
    Result.x := x - v2.x;
    Result.y := y - v2.y;
    Result.z := z - v2.z;
    Vec3D_Normalise(Result);
  end;
end;

function Vec3D_Multiply(const v1: TGVec3D; v2: TGVec3D): TGVec3D;
begin
  with v1 do
  begin
    Result.x := x * v2.x;
    Result.y := y * v2.y;
    Result.z := z * v2.z;
    Vec3D_Normalise(Result);
  end;
end;

procedure Vec3D_Normalise( var v: TGVec3D );
var
  eLen: Extended;
begin
  with v do
  begin
    eLen := x * x + y * y + z * z;

    if eLen > TG_EPSILON then
    begin
      eLen := 1.0 / sqrt(eLen);
      x := x * eLen;
      y := y * eLen;
      z := z * eLen;
    end;
  end;
end;

function Vec3D_Plus(const v1: TGVec3D; v2: TGVec3D): TGVec3D;
begin
  with v1 do
  begin
    Result.x := x + v2.x;
    Result.y := y + v2.y;
    Result.z := z + v2.z;
    Vec3D_Normalise(Result);
  end;
end;

function Vec3D_Scale( const v: TGVec3D; s : Double ): TGVec3D;
begin
  with v do
  begin
    Result.x := x * s;
    Result.y := y * s;
    Result.z := z * s;
  end;
end;

function Vec3D_ToPointLL( const v: TGVec3D ): TGPointLL;
begin
  with v do
  begin
    if z <> 0 then
      Result.iLongX := Trunc(ArcTan2(x, z) * GU_FROMRADIANS);

    Result.iLatY := Trunc(ArcSin(y) * GU_FROMRADIANS);
    Result.iHeightZ := 0;
  end;
end;



function Vec_Dot( const v1 : TGVec; const v2 : TGVec ): Double;
begin
  Result := v1.X * v2.X + v1.Y * v2.Y;
end;

function Vec_FromPoint( pt : TPoint ): TGVec;
begin
  Result.X := pt.X;
  Result.Y := pt.Y;
end;

function Vec_FromPointLL( ptLL : TGPointLL ): TGVec;
begin
  Result.X := ptLL.iLongX;
  Result.Y := ptLL.iLatY;
end;

function Vec_FromXY( eX, eY : Double ): TGVec;
begin
  Result.X := eX;
  Result.Y := eY;
end;

function Vec_Length(const v : TGVec): Double;
begin
  Result := Hypot( v.X, v.Y );
end;

function Vec_Minus( const v1 : TGVec; const v2 : TGVec ): TGVec;
begin
  Result.X := v1.X - v2.X;
  Result.Y := v1.Y - v2.Y;
end;

function Vec_Perpendicular(const v : TGVec): TGVec;
begin
  Result.X := -v.Y;
  Result.Y := v.X;
end;

function Vec_Plus( const v1 : TGVec; const v2 : TGVec ): TGVec;
begin
  Result.X := v1.X + v2.X;
  Result.Y := v1.Y + v2.Y;
end;

function Vec_Scale( const v: TGVec; s : Double ): TGVec;
begin
  Result.X := v.X * s;
  Result.Y := v.Y * s;
end;

initialization
  GlobeClassList := nil;
  
finalization
  GlobeClassList.Free;
end.

