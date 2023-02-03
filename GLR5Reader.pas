//-----------------------------------------------------------------------
// Summary
//	TGlobe .LR5 Format file reader
//
// Description
// 	Reads data objects from an LR5 format file
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
{$I GLOBE5.INC}
unit GLR5Reader;

interface

uses
  Windows, SysUtils, Classes, Graphics, GPresenters, GSysUtils, GMapObjects,
  GClasses, GDataReaders, GXML;

type
  TGLR5FileReader = class;

  TGReadDataMethod = function(reader:TGLR5FileReader) : TGMapPoint;


  //----------------------------------------------------------------------------
  //Description
  //Import manager class used to read data from a TGlobe .LR5 format file. This
  //format is prefered over the .LYR format as it supports the storing of images
  //and is more extensible.                                                     
  //----------------------------------------------------------------------------
  TGLR5FileReader = class(TGFileDataReader)
  private
    FLYRStream : TStream;
    FReader : TGStreamReader;
    FObjectClasses : TStringList;

  protected
    function ReadObjectProperties( obj : TGMapPoint ) : TGMapObjectFlagSet; virtual;
    procedure ReadPolyObject( obj : TGMapPoly; ObjectFlags : TGMapObjectFlagSet ); virtual;

    function ReadHeader : Boolean; override;
    function ReadObject : IGMapPoint; override;

    function InternalOpen : Boolean; override;
    function InternalClose : Boolean; override;
  public
    constructor Create( parentGlobe : TGCustomGlobe5 ); override;
    destructor Destroy; override;
    function ProgressPercent : Double; override;
  published
    function ReadTGMapObject : TGMapPoint;
    function ReadTGMapPoint : TGMapPoint;
    function ReadTGMapPoly : TGMapPoint;
    function ReadTGMapRaster : TGMapPoint;
    // To Add a new object type create a descendant class and add a Read....
    // method for the class in the published section.
  end;

implementation

Uses GSpatialDatabase, GResource, GLR5Writer;


constructor TGLR5FileReader.Create(parentGlobe: TGCustomGlobe5);
begin
  inherited;

  FObjectClasses := TStringList.Create;
end;

destructor TGLR5FileReader.Destroy;
begin
  FreeAndNil( FObjectClasses );
  inherited;
end;

function TGLR5FileReader.ReadHeader: Boolean;
var
  idx : integer;
  section : ShortString;
  imageName : TFilename;
  fileVersion : Word;
  dataReader : TGReadDataMethod;
  memStream : TMemoryStream;
begin
  Result := false;
  fileVersion := FReader.ReadSmallInt; { read the version of globe data file }

  // check that the LR5 file version is not older than we can handle
  if fileVersion < TG_LR5_VERSION then
    raise EGException.CreateFmt( rsEDataFileVersionMsg, [fileVersion, Filename] );

  // check that the LR5 file version is not newer than we can handle
  if fileVersion > TG_LR5_VERSION then
    raise EGException.CreateFmt( rsEDataFileVersionMsg, [fileVersion, Filename] );

  while FLYRStream.Position < FLYRStream.Size do
  begin
    section := FReader.ReadShortString;

    if section = 'End' then
      Exit;

    if section = 'MetaData' then
    begin
      MetaData.CommaText := FReader.ReadString;
      Presenters.AsXMLString := MetaData.Values['Presenters'];
    end
    else
      if section = 'Images' then
      begin
        // Load in the images for the Presenters
        imageName := FReader.ReadString;
        while imageName <> '' do
        begin
          memStream := TMemoryStream.Create;

          memStream.CopyFrom(FReader.DataStream, FReader.ReadInteger);
          ObjectSource.ImportImageStream(imageName, memStream);
          memStream.Free;

          imageName := FReader.ReadString;
        end;
      end
      else
        if section = 'Classes' then
        begin
          // Load in the list of classes to read.
          FObjectClasses.CommaText := FReader.ReadString;
          for idx := 0 to FObjectClasses.Count - 1 do
          begin
            @dataReader := MethodAddress( 'Read' + FObjectClasses[idx] );
            Assert( Assigned( dataReader ), 'No read method for class ' + FObjectClasses[idx] );
            FObjectClasses.Objects[idx] := TObject(@dataReader);
          end;
        end
        else
          if section = 'Data' then
            break;
  end;
  Result := true;
end;


function TGLR5FileReader.ReadObject: IGMapPoint;
var
  objType : Byte;
  dataMethod: TGReadDataMethod;
begin
  inherited ReadObject;

  Result := nil;

  objType := FReader.ReadByte;

  if objType = 0 then // reached the end of the section
    Exit;

  @dataMethod := Pointer( FObjectClasses.Objects[objType - 1] );
//  if not Assigned( dataMethod ) then
//    raise EGException.Create('No Read method for class: ' + FObjectClasses[objType - 1] );

  Result:= dataMethod(Self);
end;


function TGLR5FileReader.InternalClose: Boolean;
begin
  inherited InternalClose;

  FreeAndNil( FReader );
  FreeAndNil( FLYRStream );
  Result := true;
end;


function TGLR5FileReader.InternalOpen: Boolean;
begin
  inherited InternalOpen;

  Result := FileExists( Filename );
  if Result then
  begin
    FLYRStream := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite );

    FReader := TGStreamReader.Create( FLYRStream );
  end;
end;


function TGLR5FileReader.ProgressPercent: Double;
begin
  if FLYRStream <> nil then
    Result := ( FLYRStream.Position / FLYRStream.Size ) * 100.0
  else
    Result := 0.0;
end;


function TGLR5FileReader.ReadObjectProperties(obj: TGMapPoint) : TGMapObjectFlagSet;
begin
  Result := TGMapObjectFlagSet(Ord(FReader.ReadWord));

  if ofTitle in Result then
    obj.ObjectDataCSL := FReader.ReadString; { Read Title if present }

  if ofID in Result then
    FReader.ReadInteger;  // Object ID not used

  if ofUserID in Result then
    obj.UserID := FReader.ReadShortString; { Read Object GUID if present }

  obj.PresenterID := FReader.ReadSmallInt;
end;

procedure TGLR5FileReader.ReadPolyObject(obj: TGMapPoly; ObjectFlags : TGMapObjectFlagSet);
var
  csFlags: TGChainStoreFlagSet;
begin
  csFlags := [];

  if ofMultiRing in ObjectFlags then
    Include( csFlags, cfMultiRing );
  if ofHeight in ObjectFlags then
    Include( csFlags, cfHeight );
  if ofClosed in ObjectFlags then
    obj.Closed := true;


  obj.Chains.ReadChains( FReader, csFlags );
end;

//-----------------------------------------
//Description
//Reads a TGMapObject from the data stream.
//-----------------------------------------
function TGLR5FileReader.ReadTGMapObject : TGMapPoint;
begin
  Result := ReadTGMapPoint;
end;

//-----------------------------------------
//Description
//Reads a TGMapPoint from the data stream.
//-----------------------------------------
function TGLR5FileReader.ReadTGMapPoint: TGMapPoint;
var
  ObjectFlags : TGMapObjectFlagSet;
begin
  Result := TGMapPoint.Create;

  ObjectFlags := ReadObjectProperties( Result );

  Result.Centroid := FReader.ReadPointLL(ofHeight in ObjectFlags, of16Bit in ObjectFlags)  // read first point
end;

//---------------------------------------
//Description
//Reads a TGMapPoly from the data stream.
//---------------------------------------
function TGLR5FileReader.ReadTGMapPoly: TGMapPoint;
var
  ObjectFlags : TGMapObjectFlagSet;
begin
  Result := TGMapPoly.Create;

  ObjectFlags := ReadObjectProperties( Result );

  if ofSingle in ObjectFlags then
    Result.Centroid := FReader.ReadPointLL(ofHeight in ObjectFlags, of16Bit in ObjectFlags)  // read Centroid only
  else
    ReadPolyObject( TGMapPoly( Result), ObjectFlags );
end;

//-----------------------------------------
//Description
//Reads a TGMapRaster from the data stream.
//-----------------------------------------
function TGLR5FileReader.ReadTGMapRaster: TGMapPoint;
var
  doc : TGXML_Document;
  memStream : TMemoryStream;
begin
  Result := TGMapRaster.Create;

  ReadObjectProperties(Result);

  with Result as TGMapRaster do
  begin
    ImageName := FReader.ReadString;
    AlphaBlend := FReader.ReadByte;
    TransparentColor := TColor( FReader.ReadInteger );

    doc := TGXML_Document.Create('Morph');
    doc.AsString := FReader.ReadString;
    Morph.LoadFromXML(doc.Document);
    doc.Free;

    memStream := TMemoryStream.Create;
    memStream.CopyFrom(FReader.DataStream, FReader.ReadInteger);

    Self.ObjectSource.ImportImageStream(ImageName, memStream);

    memStream.Free;
  end;
end;

initialization
  RegisterGlobeClass( TGLR5FileReader, 'LR5 File Reader');
end.
