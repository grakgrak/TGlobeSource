//-----------------------------------------------------------------------
// Summary
//	TGlobe FWV file reader
//
// Description
// 	Simple Fixed Width Value file reader - very much under development
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
{$I GLOBE5.INC}
unit GFWVReader;

interface

uses
  Windows, Forms, Graphics, Classes, SysUtils, Globe5, GSysUtils, GDataReaders,
  GMapObjects, GPresenters, GTextReader, GClasses, GXML;

const
  TEXT_BUFFER_SIZE = 64 * 1024;

type

  //-----------------------------------------------------------------------------
  //Description
  //This is a basic Fixed Width Value data file reader. It can only read point
  //data and associated attributes from a file and assumes that the first line of
  //the file contains the column names, and the lat, long values are specified in
  //decimal degrees.
  //
  //Either an XML metadata file called '.FWV.XMD' or with the same name as the
  //FWV file but with a .XMD extension can be created (see below) which gives
  //some information on the format of the CSV data. If both files exist then the
  //filename.XMD file has precedence over the .FWV.XMD file.
  //
  //
  //
  //This reader is very much still under construction.
  //
  //
  //
  //Example
  //\Example of the XML description metadata file:
  //
  //
  //
  //\<XMD Seperator=","\>
  //
  //\<Centroid LongCol="0" LatCol="1"/\>
  //
  //\</XMD\>
  //
  //
  //
  //The above specifies the comma character as the column seperator, column 0 as
  //the longitude column and column 1 as the latitude column.
  //-----------------------------------------------------------------------------
  TGFWVFileReader = class(TGFileDataReader)
  private
    FCSVStream : TFileStream;
    FCSVReader : TGTextFileReader;
    FColumnNames : TStringList;
    FCSVWords : TStringList;
    FCurrentLine : String;
    FCentroidLongCol : integer;
    FCentroidLatCol : integer;
    FFirstLineTitles : Boolean;
    FSeperator : String;
  protected
    function GetCSVWord(iIndex : integer) : String;
    function ReadLine : Boolean;
    function ReadCentroid : TGPointLL;

    function InternalOpen : Boolean; override;
    function InternalClose : Boolean; override;

    function ReadHeader : Boolean; override;
    function ReadObject : IGMapPoint; override;

    function ReadPointObject : TGMapPoint;

    procedure LoadFromXMD( el : TGXML_Element ); override;
    procedure SetCurrentLine(const Value: String);
  public
    destructor Destroy; override;
    function ProgressPercent: Double;  override;

    //--------------------------------------------------
    //The names of the columns contained in the CSV file
    //--------------------------------------------------
    property ColumnNames : TStringList read FColumnNames write FColumnNames;
    //-----------------------------------------------------------------------------
    //Description
    //Specifes the character to use as the sperator between data columns in the CSV
    //file. Defaults to a comma.                                                   
    //-----------------------------------------------------------------------------
    property Seperator : String read FSeperator write FSeperator;
    //------------------------------------------------------------------------
    //Description
    //The column that contains the longitude value for the centroid of the new
    //\object.                                                                
    //------------------------------------------------------------------------
    property CentroidLongCol : integer read FCentroidLongCol write FCentroidLongCol;
    //-------------------------------------------------------------------------------
    //Description
    //The column that contains the latitude value for the centroid of the new object.
    //-------------------------------------------------------------------------------
    property CentroidLatCol : integer read FCentroidLatCol write FCentroidLatCol;
  end;

implementation

uses GResource;

function TGFWVFileReader.GetCSVWord(iIndex : integer) : String;
begin
  if iIndex < FCSVWords.Count then
    Result := FCSVWords[iIndex]
  else
    Result := '';
end;

function TGFWVFileReader.ReadLine : Boolean;
var
  sTmp : String;
begin
  repeat
    FCSVReader.ReadLn(sTmp);
    Result := Length(sTmp) > 0;
  until FCSVReader.EOT or Result;

  if Result then
    SetCurrentLine(sTmp)
  else
    SetCurrentLine('');
end;

function TGFWVFileReader.ReadCentroid : TGPointLL;
var
  Long, Lat : Extended;
begin
  Long := StrToExtended(FCSVWords[FCentroidLongCol]);
  Lat := StrToExtended(FCSVWords[FCentroidLatCol]);

  Result := DDToPointLL(Long, Lat);
end;

function TGFWVFileReader.ReadPointObject : TGMapPoint;
begin
  Result := TGMapPoint.Create;
  Result.Centroid := ReadCentroid;
  Result.PresenterID := 1;
end;


function TGFWVFileReader.InternalOpen : Boolean;
begin
  FColumnNames := TStringList.Create;
  FCSVWords := TStringList.Create;

  inherited InternalOpen;

  FFirstLineTitles := true;
  FSeperator := ',';

  FCentroidLongCol := -1;
  FCentroidLatCol := -1;

  Result := FileExists(Filename);
  if Result then
  begin
    Name := ExtractFilename(Filename);

    // Create the MIF data stream
    FCSVStream := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite );
    FCSVReader := TGTextFileReader.Create(FCSVStream);
    FCSVReader.TextBufferSize := TEXT_BUFFER_SIZE;

    Result := True;
  end;
end;

{------------------------------------------------------------------------------
  TGFWVFileReader.InternalClose
------------------------------------------------------------------------------}
function TGFWVFileReader.InternalClose : Boolean;
begin
  inherited InternalClose;

  FreeAndNil( FColumnNames );
  FreeAndNil( FCSVWords );

  FreeAndNil( FCSVReader );
  FreeAndNil( FCSVStream );

  Result := True;
end;

function TGFWVFileReader.ProgressPercent: Double;
begin
  if FCSVReader <> nil then
    Result := ( FCSVReader.Position / FCSVReader.Size ) * 100.0
  else
    Result := 0.0;
end;

function TGFWVFileReader.ReadHeader : Boolean;
begin
  // load up the titles from the data file
  if FFirstLineTitles and ReadLine then
    ColumnNames.Assign(FCSVWords);

  MetaData.Values['ColumnNames'] := ColumnNames.CommaText;

  // have a guess at the locations for the lat long columns
  if FCentroidLongCol = -1 then
    FCentroidLongCol := ColumnNames.IndexOf( 'Longitude' );
  if FCentroidLongCol = -1 then
    FCentroidLongCol := ColumnNames.IndexOf( 'Long' );
  if FCentroidLongCol = -1 then
    FCentroidLongCol := ColumnNames.IndexOf( 'Lon' );

  if FCentroidLatCol = -1 then
    FCentroidLatCol := ColumnNames.IndexOf( 'Latitude' );
  if FCentroidLatCol = -1 then
    FCentroidLatCol := ColumnNames.IndexOf( 'Lat' );

  Result := ( FCentroidLatCol <> -1 ) and ( FCentroidLongCol <> -1 );
end;

function TGFWVFileReader.ReadObject : IGMapPoint;
begin
  inherited ReadObject;

  Result := nil;

  ReadLine;
  if not FCSVReader.EOT then
  begin
    Result := ReadPointObject;

    if Result <> nil then
      Result.ObjectDataCSL := FCSVWords.CommaText;
  end;
end;

procedure TGFWVFileReader.SetCurrentLine(const Value: String);
begin
  FCurrentLine := Value;

  FCSVWords.CommaText := Value;
end;

destructor TGFWVFileReader.Destroy;
begin
  FreeAndNil( FColumnNames );
  FreeAndNil( FCSVWords );

  inherited;
end;

procedure TGFWVFileReader.LoadFromXMD(el: TGXML_Element);
begin
  inherited;

  if el <> nil then
  begin
    FSeperator := el.Attribute('Seperator', ',');

    //ToDo: Load in centroid units, projection etc.
    el := el.Element('Centroid');
    if el <> nil then
    begin
      FCentroidLongCol := el.Attribute('LongCol', -1);
      FCentroidLatCol := el.Attribute('LatCol', -1);
    end;
  end;
end;

initialization
  RegisterGlobeClass( TGFWVFileReader, 'FWV File Reader' );
end.

