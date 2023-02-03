//-----------------------------------------------------------------------
// Summary
//	TGlobe data importing base classes
//
// Description
// 	Spatial Database importer base classes
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
unit GDataReaders;

interface

Uses Classes, SysUtils, GClasses, GSysUtils, GXML;

const
  ONE_SECOND = 1.0 / ( 24.0 * 60.0 * 60.0 );
  DEFAULT_POINT_PRESENTERID = 1;
  DEFAULT_POLYLINE_PRESENTERID = 2;
  DEFAULT_POLYGON_PRESENTERID = 3;

type
  TGReadObjectEvent = procedure( Sender: TGComponent; obj : IGMapPoint ) of object;

  TGDataReader = class(TGCustomDataReader)
  private
    FCurrentObject : IGMapPoint;
    FOnReadObject: TGReadObjectEvent;
    FOpen : Boolean;
    FReadCount : integer;
    FProgressTimeout : TDateTime;
  protected
    function ReadHeader : Boolean; override;
    function ReadTrailer : Boolean; override;
    function ReadObject : IGMapPoint; override;

    function InternalClose : Boolean; override;
    function InternalOpen : Boolean; override;

    procedure DoOnReadObject;
    procedure DoOnProgress( progressType : TGProgressType );
  public
    constructor Create( parentGlobe : TGCustomGlobe5 ); override;
    destructor Destroy; override;

    function First : IGMapPoint; override;
    function Next : IGMapPoint; override;
  published
    property OnReadObject : TGReadObjectEvent read FOnReadObject write FOnReadObject;
  end;
  TGDataReaderClass = class of TGDataReader;


  TGFileDataReader = class(TGDataReader)
  private
    FFileName : TFileName;
    procedure SetFileName(const Value: TFileName);
  protected
    procedure LoadFromXMD( el : TGXML_Element ); virtual;
    function InternalOpen : Boolean; override;
    function ReadTrailer : Boolean; override;
  public
    procedure LoadFromXML(Element : TGXML_Element); override;
    procedure SaveToXML(Element : TGXML_Element); override;
  published
    property FileName : TFileName read FFileName write SetFileName;
  end;
  TGFileDataReaderClass = class of TGFileDataReader;

function CreateFileDataReader( globe : TGCustomGlobe5; const filename : String ): TGFileDataReader;

implementation

Uses Globe5;


function CreateFileDataReader( globe : TGCustomGlobe5; const filename : String ): TGFileDataReader;
var
  ext : String;
  fileReaderClass : TGFileDataReaderClass;
begin
  Result := nil;

  if FileExists( filename ) then
  begin
    ext := ExtractFileExt( filename );
    if ext[1] = '.' then
      ext := Copy( ext, 2, Length(ext));

    try
      fileReaderClass := TGFileDataReaderClass( FindClass( 'TG' + ext + 'FileReader' ));

      Result := fileReaderClass.Create(globe);
      Result.FileName := filename;
    except
      // Ignore if the file reader class cannot be found
    end;
  end;
end;


constructor TGDataReader.Create( parentGlobe : TGCustomGlobe5 );
begin
  inherited;

  Assert( parentGlobe <> nil, 'Data readers must be constructed with a parent globe' );

  Presenters := TGPresenterStore.Create(parentGlobe);
  MetaData := TStringList.Create;
  MetaData.Values['ColumnNames'] := 'Title';  // default the column names property
end;

destructor TGDataReader.Destroy;
begin
  InternalClose;

  FCurrentObject := nil;
  Presenters.Free;
  MetaData.Free;

  inherited;
end;


procedure TGDataReader.DoOnProgress(progressType : TGProgressType );
begin
  if Assigned( OnProgress ) then
    if ( progressType in [pProgressStart, pProgressEnd, pProgressError]) or (Now > FProgressTimeout) then
    begin
      OnProgress( Self, progressType, FReadCount, ProgressPercent );

      if progressType in [pProgressStart, pProgressEnd, pProgressError] then
        FProgressTimeout := 0.0
      else
        FProgressTimeout := Now + ONE_SECOND;
    end;
end;


procedure TGDataReader.DoOnReadObject;
begin
  if Assigned( FOnReadObject ) then
    if FCurrentObject <> nil then
      FOnReadObject( Self, FCurrentObject );
end;


function TGDataReader.First: IGMapPoint;
begin
  Result := nil;

  if FOpen then
    InternalClose;

  FOpen := InternalOpen;
  if FOpen then
    if ReadHeader then
      Result := Next;
end;

function TGDataReader.InternalClose : Boolean;
begin
  DoOnProgress(pProgressEnd);
  Result := true;
end;

function TGDataReader.InternalOpen : Boolean;
begin
  DoOnProgress(pProgressStart);
  FReadCount := 0;
  Result := true;
end;

function TGDataReader.Next: IGMapPoint;
begin
  if FOpen then
  begin
    FCurrentObject := ReadObject;
    Result := FCurrentObject;

    if Result = nil then
    begin
      ReadTrailer;
      InternalClose;
      FOpen := false;
    end
    else
      DoOnReadObject;
  end
  else
    Result := nil;
end;



function TGDataReader.ReadHeader : Boolean;
begin
  Result := true;
end;

function TGDataReader.ReadObject : IGMapPoint;
begin
  Inc( FReadCount );
  if ( FReadCount mod 100 ) = 0 then
    DoOnProgress(pProgress);

  Result := nil;
end;


function TGDataReader.ReadTrailer : Boolean;
begin
  Result := true;
end;

function TGFileDataReader.InternalOpen: Boolean;
var
  doc : TGXML_Document;
  tmp : string;
begin
  doc := nil;

  tmp := ChangeFileExt(Filename, '.xmd');
  if not FileExists(tmp) then
  begin
    tmp := ExtractFilePath(Filename) + Copy( ExtractFileExt(Filename), 2, 64 ) + '.xmd';

    if not FileExists(tmp) then
      tmp := '';
  end;

  if tmp <> '' then
  try
    doc := TGXML_Document.Create('XMD');

    doc.LoadFromFile(tmp);

    if doc.Document <> nil then
      LoadFromXMD( doc.Document );
  finally
    doc.Free;
  end;

  Result := inherited InternalOpen;
end;

procedure TGFileDataReader.LoadFromXMD(el: TGXML_Element);
begin
  // Does nothing
end;

procedure TGFileDataReader.LoadFromXML(Element: TGXML_Element);
begin
  inherited;
  if Element <> nil then
    FileName := Element.Attribute('Filename', '');
end;

function TGFileDataReader.ReadTrailer: Boolean;
begin
  Result := inherited ReadTrailer;
  
  MetaData.Values['SourceName'] := Filename;
end;

procedure TGFileDataReader.SaveToXML(Element: TGXML_Element);
begin
  inherited;
  Element.AddAttribute('Filename', FileName, '');
end;

procedure TGFileDataReader.SetFileName(const Value: TFileName);
begin
  FFilename := Value;

  // Get the name of the layer from the file name, remove the extention
  Name := ChangeFileExt( ExtractFileName( Value ), '' );
end;

end.
