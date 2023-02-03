//-----------------------------------------------------------------------
// Summary
//	TGlobe .LYR Format file writer
//
// Description
// 	Writes data objects to a LYR format file
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
{$I GLOBE5.INC}
unit GLYRWriter;

interface

uses
  Windows, FOrms, SysUtils, Classes, Graphics, GPresenters, GSysUtils, GMapObjects,
  GClasses, GDataWriters, GXML;

const
  TG_FILEVERSION500 = $0500;
  TG_FILEVERSION405 = $0405;
  TG_FILEVERSION400 = $0400;
  TG_FILEVERSION300 = $0300;
  TG_FILEVERSION301 = $0301;
  TG_FILEVERSION = TG_FILEVERSION500;

type
  TGLYRFileWriter = class;

  TGWriteDataMethod = procedure(writer:TGLYRFileWriter; obj : TGMapPoint );
  
  TGLYRFileWriter = class(TGFileDataWriter)
  private
    FWriter : TGStreamWriter;
    FObjectClasses : TStringList;
    FPresenterClasses : TStringList;

    FDataMethod: TGWriteDataMethod;

    FTotalObjectCount : integer;
  protected
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
  end;

implementation

Uses GResource;

constructor TGLYRFileWriter.Create(parentGlobe : TGCustomGlobe5);
begin
  inherited;

  FObjectClasses := TStringList.Create;
  FPresenterClasses := TStringList.Create;
end;

destructor TGLYRFileWriter.Destroy;
begin
  FreeAndNil( FWriter );

  FreeAndNil( FObjectClasses );
  FreeAndNil( FPresenterClasses );

  inherited;
end;

procedure TGLYRFileWriter.InternalClose;
begin
  FWriter.DataStream.Free;
  FreeAndNil( FWriter );

  DoProgress( 0, 0.0, true ); // flag Export has finished
end;

function TGLYRFileWriter.InternalOpen: Boolean;
var
  objIterator : TGIterator;
  obj : IGMapPoint;
  idx : integer;
begin
  DoProgress( 0, 0.0, true );

  FWriter := TGStreamWriter.Create( TFileStream.Create( Filename, fmCreate ));

  // Build a list of all object classes on the layer
  objIterator := Layer.ObjectSource.NewIterator;
  try
    obj := objIterator.First;
    while obj <> nil do
    begin
      Inc( FTotalObjectCount );

      idx := FObjectClasses.IndexOf( obj.ObjectClassName );
      if idx = -1 then
        idx := FObjectClasses.AddObject( obj.ObjectClassName, TObject( 0 ));

      // Increment count instances for this count
      FObjectClasses.Objects[idx] := TObject(integer(FObjectClasses.Objects[idx]) + 1 );

      obj := objIterator.Next;
    end;
    Result := true;
  finally
    objIterator.Free;
  end;
end;

procedure TGLYRFileWriter.WriteBody;
var
  idx, iCount : integer;
  iWritten : integer;
  sClassName : string;
  objIterator : TGIterator;
  obj : IGMapPoint;
  mapObj : TGMapPoint;
begin
  if FTotalObjectCount = 0 then
    Exit;

  iWritten := 0;
  for idx := 0 to FObjectClasses.Count - 1 do
  begin
    iCount := integer( FObjectClasses.Objects[idx] );
    sClassName := FObjectClasses[idx];

    // get the writer method for this class of object
    @FDataMethod := MethodAddress( 'Write' + sClassName );
    Assert(Assigned( FDataMethod ), 'No write method for class ' + sClassName );

    if iCount > 0 then
    begin
      FWriter.WriteInteger( iCount );
      FWriter.WriteShortString( sClassName );

      // Save the object class data
      objIterator := Layer.ObjectSource.NewIterator;
      try
        obj := objIterator.First;
        while obj <> nil do
        begin
          mapObj := ( obj as IGUnderlyingObject ).GetUnderlyingObject as TGMapPoint;

          if sClassName = obj.ObjectClassName then
          begin
            FDataMethod(Self, mapObj);  // write the data to the file

            Inc( iWritten );
            if ( iWritten mod 50 ) = 0 then
            begin
              DoProgress( iWritten, ( iWritten / FTotalObjectCount ) * 100.0, false);
              if Application.Terminated then
                Break;
            end;
          end;
          obj := objIterator.Next;
        end;
      finally
        objIterator.Free;
      end;
    end;
  end;
end;

procedure TGLYRFileWriter.WriteHeader;
var
  presenters : TGPresenterStore;
begin
  FWriter.WriteSmallInt( TG_FILEVERSION ); { write the version of globe data file }

  // gather all the presenters for the layer together.
  presenters := TGPresenterStore.Create(ParentGlobe);
  presenters.AddPresenters(Layer.ObjectSource.Presenters);
  presenters.AddPresenters(Layer.Presenters);

  // save into the metadata string.
  Layer.ObjectSource.MetaData.Values['Presenters'] := presenters.AsXMLString;
  presenters.Free;

  FWriter.WriteInteger( 1 );
  FWriter.WriteShortString( 'MetaData' ); // As a classname
  FWriter.WriteLongString( Layer.ObjectSource.MetaData.CommaText );
end;

//-----------------------------------------------------------------------
//Description
//\Internal routine to write a map objects properties to the output file.
//-----------------------------------------------------------------------
procedure TGLYRFileWriter.WriteObjectProperties(obj: TGMapPoint);
begin
  if obj.ObjectDataCSL <> '' then
    Include( obj.ObjectFlags, ofTitle );
  if obj.UserID <> '' then
    Include( obj.ObjectFlags, ofUserID );

  FWriter.WriteWord(Word( obj.ObjectFlags));

  if ofTitle in obj.ObjectFlags then
    FWriter.WriteLongString( obj.ObjectDataCSL );

  if ofUserID in obj.ObjectFlags then
    FWriter.WriteShortString( obj.UserID );

  FWriter.WriteSmallInt( obj.PresenterID );
end;

//-----------------------------------------------------------
//Description
//\Internal routine to write a map object to the output file.
//-----------------------------------------------------------
procedure TGLYRFileWriter.WriteTGMapPoint(obj: TGMapPoint);
begin
  obj.ObjectFlags := [ofSingle];

  if obj.Centroid.iHeightZ <> 0 then
    Include( obj.ObjectFlags, ofHeight );

  WriteObjectProperties(obj);
  FWriter.WritePointLL( obj.Centroid, ofHeight in obj.ObjectFlags, false ); // write centroid
end;

//----------------------------------------------------------------
//Description
//\Internal routine to write a map Poly object to the output file.
//----------------------------------------------------------------
procedure TGLYRFileWriter.WriteTGMapPoly(obj: TGMapPoint);
begin
  if TGMapPoly( obj ).Chains.Count = 0 then
    obj.ObjectFlags := [ofSingle];

  if TGMapPoly( obj ).Chains.Count > 1 then
    obj.ObjectFlags := [ofMultiRing];

  if obj.Centroid.iHeightZ <> 0 then
    Include( obj.ObjectFlags, ofHeight );

  if TGMapPoly( obj ).Closed then
    Include( obj.ObjectFlags, ofClosed );

  WriteObjectProperties(obj);

  if ofSingle in obj.ObjectFlags then
    FWriter.WritePointLL( obj.Centroid, ofHeight in obj.ObjectFlags, false ) // write centroid
  else
    TGMapPoly( obj ).Chains.WriteChains( FWriter );
end;

procedure TGLYRFileWriter.WriteTrailer;
begin
  FWriter.WriteInteger( 0 );  // sentinal at the end of the data
  DoProgress( FTotalObjectCount, 100.0, true );
end;

end.
