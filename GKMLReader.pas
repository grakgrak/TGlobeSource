{------------------------------------------------------------------------------
 Module:		TGKMLReader.pas

 Comment:   Reads Google KML files

 Classes:   TGKMLFileReader

 Author:	  Graham Knight
 Email:		  tglobe@tglobe.com
------------------------------------------------------------------------------}
{$I GLOBE5.INC}
unit GKMLReader;

interface

uses
  Windows, SysUtils, Classes, GDBFReader, Globe5, GSysUtils, GMapObjects,
  GDataReaders, GClasses, GXML;

type

  //------------------------------------------------------------------------
  //Description
  //Reads SHP files into a layer and converts the point data to Globe Units.
  //------------------------------------------------------------------------
  TGKMLFileReader = class(TGFileDataReader)
  private
    FXMLDocument : TGXML_Document;
  protected
    function InternalOpen : Boolean; override;
    function InternalClose : Boolean; override;

    function ReadHeader : Boolean; override;
    function ReadObject : IGMapPoint; override;

    procedure LoadFromXMD( el : TGXML_Element ); override;
  public
    constructor Create( parentGlobe : TGCustomGlobe5 ); override;
    destructor Destroy; override;

    function ProgressPercent: Double; override;
  published
  end;

implementation

Uses GResource;

constructor TGKMLFileReader.Create( parentGlobe : TGCustomGlobe5 );
begin
  inherited;
end;

destructor TGKMLFileReader.Destroy;
begin
  inherited;
end;


function TGKMLFileReader.ProgressPercent: Double;
begin
  Result := 0.0;
end;

function TGKMLFileReader.InternalOpen : Boolean;
begin
  inherited InternalOpen;

  FXMLDocument := TGXML_Document.Create('KML');
  FXMLDocument.LoadFromFile(Filename);

  Result := true;
end;


function TGKMLFileReader.InternalClose;
begin
  inherited InternalClose;

  FreeAndNil( FXMLDocument );

  Result := True;
end;

function TGKMLFileReader.ReadHeader: Boolean;
begin
  Result := True;
end;

function TGKMLFileReader.ReadObject: IGMapPoint;
begin
  inherited ReadObject;

  Result := nil;
end;

procedure TGKMLFileReader.LoadFromXMD(el: TGXML_Element);
begin
  inherited;

  if el <> nil then
  begin
  end;
end;

initialization
  RegisterGlobeClass( TGKMLFileReader, 'KML File Reader' );
end.
