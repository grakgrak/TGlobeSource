//-----------------------------------------------------------------------
// Summary
//	TGlobe .LR5 Format file writer
//
// Description
// 	Writes data objects to an LR5 format file
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
{$I GLOBE5.INC}
unit GLR5Writer;

interface

uses
  Windows, FOrms, SysUtils, Classes, Graphics, GPresenters, GSysUtils, GMapObjects,
  GClasses, GDataWriters, GXML;

const
  TG_LR5_VERSION = $0100;

type
  TGLR5FileWriter = class;

  TGWriteDataMethod = procedure(writer:TGLR5FileWriter; obj : TGMapPoint );

  //----------------------------------------------------------------------------
  //Description
  //Export manager class used to write data to a TGlobe .LR5 format file. This
  //format is prefered over the .LYR format as it supports the storing of images
  //and is more extensible.                                                     
  //----------------------------------------------------------------------------
  TGLR5FileWriter = class(TGFileDataWriter)
  private
    FWriter : TGStreamWriter;
    FTotalObjectCount : integer;
  protected
    FObjectClasses : TStringList;

    function InternalOpen : Boolean; override;
    procedure WriteHeader; override;
    procedure WriteBody; override;
    procedure WriteTrailer; override;
    procedure InternalClose; override;
    procedure WriteObjectProperties( obj : TGMapPoint );
  public
    constructor Create( parentGlobe : TGCustomGlobe5 ); override;
    destructor Destroy; override;
  published
    procedure WriteTGMapPoint( obj : TGMapPoint );
    procedure WriteTGMapPoly( obj : TGMapPoint );
    procedure WriteTGMapRaster( obj : TGMapPoint );
    // To Add a new object type create a descendant class and add a Write....
    // method for the class in the published section.
    // Add the method to the FObjectClasses list in the constructor
  end;

implementation

Uses Globe5, GResource, GBitmap;

constructor TGLR5FileWriter.Create(parentGlobe : TGCustomGlobe5);
begin
  inherited;

  FObjectClasses := TStringList.Create;
  FObjectClasses.AddObject('TGMapPoint', TObject(MethodAddress( 'WriteTGMapPoint' )));
  FObjectClasses.AddObject('TGMapPoly', TObject(MethodAddress( 'WriteTGMapPoly' )));
  FObjectClasses.AddObject('TGMapRaster', TObject(MethodAddress( 'WriteTGMapRaster' )));
end;

destructor TGLR5FileWriter.Destroy;
begin
  FreeAndNil( FWriter );
  FreeAndNil( FObjectClasses );

  inherited;
end;

procedure TGLR5FileWriter.InternalClose;
begin
  FWriter.DataStream.Free;
  FreeAndNil( FWriter );

  DoProgress( 0, 0.0, true ); // flag Export has finished
end;

function TGLR5FileWriter.InternalOpen: Boolean;
begin
  DoProgress( 0, 0.0, true );

  FWriter := TGStreamWriter.Create( TFileStream.Create( Filename, fmCreate ));

  Result := true;
end;

procedure TGLR5FileWriter.WriteBody;
var
  idx, iTotalCount, iWritten : integer;
  sClassName : string;
  objIterator : TGIterator;
  obj : IGMapPoint;
  mapObj : TGMapPoint;
  dataMethod: TGWriteDataMethod;
begin
  // Save the object class data
  objIterator := Layer.ObjectSource.NewIterator;
  iTotalCount := objIterator.Count;
  iWritten := 0;
  try
    obj := objIterator.First;
    while obj <> nil do
    begin
      mapObj := ( obj as IGUnderlyingObject ).GetUnderlyingObject as TGMapPoint;

      sClassName := obj.ObjectClassName;

      idx := FObjectClasses.IndexOf( sClassName );
      Assert(idx >= 0, 'No write method for class ' + sClassName );

      @dataMethod := Pointer( FObjectClasses.Objects[idx] );

      // Flag the object type
      FWriter.WriteByte( idx + 1 );  // One Counting
      dataMethod(Self, mapObj);  // write the data to the file

      Inc( iWritten );
      if ( iWritten mod 50 ) = 0 then
      begin
        DoProgress( iWritten, ( iWritten / iTotalCount ) * 100.0, false);
        if Application.Terminated then
          Break;
      end;

      obj := objIterator.Next;
    end;
    FWriter.WriteByte( 0 ); // flag the end of the data
  finally
    objIterator.Free;
  end;
end;

procedure TGLR5FileWriter.WriteHeader;
var
  presenters : TGPresenterStore;
  idx : integer;
  imageNames : TStringList;
begin
  FWriter.WriteSmallInt( TG_LR5_VERSION ); { write the version of globe data file }

  // gather all the presenters for the layer together.
  presenters := TGPresenterStore.Create(ParentGlobe);
  presenters.AddPresenters(Layer.ObjectSource.Presenters);
  presenters.AddPresenters(Layer.Presenters);

  imageNames := TStringList.Create;

  // collect up all the names of the images in the presenters
  for idx := 0 to presenters.Count - 1 do
    if presenters[idx] is TGPointPresenter then
      if TGPointPresenter( presenters[idx] ).ImageName <> '' then
        imageNames.Add(TGPointPresenter( presenters[idx] ).ImageName);

  // save into the metadata string.
  Layer.ObjectSource.MetaData.Values['Presenters'] := presenters.AsXMLString;
  presenters.Free;

  FWriter.WriteShortString( 'MetaData' ); // start of the metadata section
  FWriter.WriteString( Layer.ObjectSource.MetaData.CommaText );

  // Save any image files for the Presenters
  if imageNames.Count > 0 then
  begin
    FWriter.WriteShortString( 'Images' ); // start of the image section

    for idx := 0 to imageNames.Count - 1 do
    begin
      FWriter.WriteString(ExtractFileName( imageNames[idx] ));
      Layer.ObjectSource.ExportImageStream( imageNames[idx], FWriter.DataStream );
    end;
    FWriter.WriteString('');  // Sentinal to mark the end of the images
  end;
  imageNames.Free;

  FWriter.WriteShortString( 'Classes' ); // start of the class name section
  FWriter.WriteString( FObjectClasses.CommaText );

  FWriter.WriteShortString( 'Data' ); // start of the data section
end;

//-----------------------------------------------------------------------
//Description
//\Internal routine to write a map objects properties to the output file.
//-----------------------------------------------------------------------
procedure TGLR5FileWriter.WriteObjectProperties(obj: TGMapPoint);
begin
  if obj.ObjectDataCSL <> '' then
    Include( obj.ObjectFlags, ofTitle );
  if obj.UserID <> '' then
    Include( obj.ObjectFlags, ofUserID );
  if obj.Centroid.iHeightZ <> 0 then
    Include( obj.ObjectFlags, ofHeight );

  FWriter.WriteWord(Word( obj.ObjectFlags));

  if ofTitle in obj.ObjectFlags then
    FWriter.WriteString( obj.ObjectDataCSL );

  if ofUserID in obj.ObjectFlags then
    FWriter.WriteShortString( obj.UserID );

  FWriter.WriteSmallInt( obj.PresenterID );
end;

//-----------------------------------------------------------
//Description
//\Internal routine to write a map object to the output file.
//-----------------------------------------------------------
procedure TGLR5FileWriter.WriteTGMapPoint(obj: TGMapPoint);
begin
  obj.ObjectFlags := [ofSingle];

  WriteObjectProperties(obj); // Write the basic properties of the object

  FWriter.WritePointLL( obj.Centroid, ofHeight in obj.ObjectFlags, false ); // write centroid
end;

//----------------------------------------------------------------
//Description
//\Internal routine to write a map Poly object to the output file.
//----------------------------------------------------------------
procedure TGLR5FileWriter.WriteTGMapPoly(obj: TGMapPoint);
begin
  if TGMapPoly( obj ).Chains.Count = 0 then
    obj.ObjectFlags := [ofSingle];

  if TGMapPoly( obj ).Chains.Count > 1 then
    obj.ObjectFlags := [ofMultiRing];

  if TGMapPoly( obj ).Closed then
    Include( obj.ObjectFlags, ofClosed );

  WriteObjectProperties(obj); // Write the basic properties of the object

  if ofSingle in obj.ObjectFlags then
    FWriter.WritePointLL( obj.Centroid, ofHeight in obj.ObjectFlags, false ) // write centroid
  else
    TGMapPoly( obj ).Chains.WriteChains( FWriter );
end;

//------------------------------------------------------------------
//Description
//\Internal routine to write a map Raster object to the output file.
//------------------------------------------------------------------
procedure TGLR5FileWriter.WriteTGMapRaster(obj: TGMapPoint);
var
  doc : TGXML_Document;
begin
  WriteObjectProperties(obj); // Write the basic properties of the object

  with obj as TGMapRaster do
  begin
    FWriter.WriteString(ExtractFileName(ImageName));
    FWriter.WriteByte(AlphaBlend);
    FWriter.WriteInteger(TransparentColor);

    doc := TGXML_Document.Create('Morph');
    Morph.SaveToXML(doc.Document);
    FWriter.WriteString(doc.AsString);
    doc.Free;

    Layer.ObjectSource.ExportImageStream( ImageName, FWriter.DataStream );
  end;
end;

procedure TGLR5FileWriter.WriteTrailer;
begin
  FWriter.WriteShortString( 'End' );  // sentinal at the end of the data
  DoProgress( FTotalObjectCount, 100.0, true );
end;

end.
