//-----------------------------------------------------------------------
// Summary
//	TGlobe data linking base classes
//
// Description
// 	Spatial Database Layer object link base class
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
{$I GLOBE5.INC}
unit GDataLink;

interface

Uses Windows, SysUtils, Classes, GSDBCache, GSysUtils, GBitmap, GXML;

type
  TGDBObjectKind = ( tgokPoint, tgokPoly, tgokRaster );

  // Abstract base class that all DataLink objects share
  TGDataLink = class( TGRoot )
  private
    FModified: boolean;
  protected
    function GetCentroid: TGPointLL; virtual; abstract;
    function GetDateTime: TDateTime; virtual; abstract;
    function GetMER: TGMER; virtual; abstract;
    function GetPrimaryKey: TGPrimaryKey; virtual; abstract;
    function GetKind: TGDBObjectKind; virtual; abstract;
    procedure SetCentroid(const Value: TGPointLL); virtual; abstract;
    procedure SetDateTime(const Value: TDateTime); virtual; abstract;
    procedure SetMER(const Value: TGMER); virtual; abstract;
    procedure SetKind(const Value: TGDBObjectKind); virtual; abstract;

    function GetPresenterID: SmallInt; virtual; abstract;
    function GetAttributeID: TGRowID; virtual; abstract;
    function GetDataID: TGRowID; virtual; abstract;
    function GetObjectDataCSL: String; virtual; abstract;
    function GetState: TGMapObjectStateSet; virtual; abstract;
    function GetUserID: String; virtual; abstract;
    procedure SetPresenterID(const Value: SmallInt); virtual; abstract;
    procedure SetAttributeID(const Value: TGRowID); virtual; abstract;
    procedure SetDataID(const Value: TGRowID); virtual; abstract;
    procedure SetObjectDataCSL(const Value: String); virtual; abstract;
    procedure SetState(const Value: TGMapObjectStateSet); virtual; abstract;
    procedure SetUserID(const Value: String); virtual; abstract;

    procedure WriteObjectBase; virtual; abstract;
    procedure ReadObjectBase; virtual; abstract;
  public
    destructor Destroy; override;

    procedure Clear; virtual;
    procedure Delete; virtual; abstract;
    procedure Post; virtual;
    procedure ReadDataStream( memStream : TMemoryStream ); virtual; abstract;
    procedure WriteDataStream( memStream : TMemoryStream ); virtual; abstract;

    property DateTime : TDateTime read GetDateTime write SetDateTime;
    property Kind : TGDBObjectKind read GetKind write SetKind;
    property Centroid : TGPointLL read GetCentroid write SetCentroid;
    property MER : TGMER read GetMER write SetMER;
    property Modified : boolean read FModified write FModified;
    property PrimaryKey : TGPrimaryKey read GetPrimaryKey;

    property DataID : TGRowID read GetDataID write SetDataID;
    property AttributeID : TGRowID read GetAttributeID write SetAttributeID;
    property ObjectDataCSL : String read GetObjectDataCSL write SetObjectDataCSL;
    property PresenterID : SmallInt read GetPresenterID write SetPresenterID;
    property State : TGMapObjectStateSet read GetState write SetState;
    property UserID : String read GetUserID write SetUserID;
  end;

implementation

{ TGDataLink }

procedure TGDataLink.Clear;
begin
  FModified := false;
end;

destructor TGDataLink.Destroy;
begin
//  Post;
  inherited;
end;

procedure TGDataLink.Post;
begin
  if Modified then
    WriteObjectBase;
  Modified := false;
end;


end.
