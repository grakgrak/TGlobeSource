//-----------------------------------------------------------------------
// Summary
//	TGlobe CSV file reader
//
// Description
// 	Simple CSL file reader - very much under development
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
{$I GLOBE5.INC}
unit GCSVReader;

interface

uses
  Windows, Forms, Graphics, Classes, SysUtils, Globe5, GSysUtils, GDataReaders,
  GMapObjects, GPresenters, GTextReader, GClasses, GXML;

const
  TEXT_BUFFER_SIZE = 64 * 1024;

type

  //-----------------------------------------------------------------------------
  //Description
  //This is a basic Comma Separated Value data file reader. It can only read point 
  //data and associated attributes from a file and assumes that the first line of 
  //the file contains the column names, and the lat, long values are specified in 
  //decimal degrees.
  //
  //Either an XML metadata file called '.CSV.XMD' or with the same name as the
  //CSV file but with a .XMD extension can be created (see below) which gives
  //some information on the format of the CSV data. If both files exist then the
  //filename.XMD file has precedence over the .CSV.XMD file.
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
  //\<XMD Separator=","\>
  //
  //\<Centroid LongColumn="0" LatColumn="1"/\>
  //
  //\</XMD\>
  //
  //
  //
  //The above specifies the comma character as the column separator, column 0 as
  //the longitude column and column 1 as the latitude column.
  //-----------------------------------------------------------------------------
  TGCSVFileReader = class(TGFileDataReader)
  private
    FCSLStream : TFileStream;
    FCSLReader : TGTextFileReader;
    FColumnNames : TStringList;
    FCSLWords : TStringList;
    FCurrentLine : String;
    FCentroidLongCol : integer;
    FCentroidLatCol : integer;
    FFirstLineTitles : Boolean;
    FSeparator : String;
    FSkipLines : integer;
  protected
    function GetCSLWord(iIndex : integer) : String;
    function ReadLine : Boolean;
    function ReadCentroid : TGPointLL;

    function InternalOpen : Boolean; override;
    function InternalClose : Boolean; override;

    function ReadHeader : Boolean; override;
    function ReadObject : IGMapPoint; override;

    function ReadPointObject : TGMapPoint;

    procedure LoadFromXMD( el : TGXML_Element ); override;
    procedure ParseLine(const Value: String);
  public
    destructor Destroy; override;
    function ProgressPercent: Double;  override;

    //--------------------------------------------------
    //The names of the columns contained in the CSL file
    //--------------------------------------------------
    property ColumnNames : TStringList read FColumnNames write FColumnNames;

    //-----------------------------------------------------------------------------
    //Description
    //Specifes the character to use as the sperator between data columns in the CSL
    //file. Defaults to a comma.
    //-----------------------------------------------------------------------------
    property Separator : String read FSeparator write FSeparator;

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

function TGCSVFileReader.GetCSLWord(iIndex : integer) : String;
begin
  if iIndex < FCSLWords.Count then
    Result := FCSLWords[iIndex]
  else
    Result := '';
end;

function TGCSVFileReader.ReadLine : Boolean;
var
  sTmp : String;
begin
  repeat
    FCSLReader.ReadLn(sTmp);
    Result := Length(sTmp) > 0;
  until FCSLReader.EOT or Result;

  FCSLWords.BeginUpdate;
  try
    FCSLWords.Clear;
    if Result then
      ParseLine(sTmp)
    else
      ParseLine('');
  finally
    FCSLWords.EndUpdate;
  end;
end;

function TGCSVFileReader.ReadCentroid : TGPointLL;
var
  Long, Lat : Extended;
begin
  Long := StrToExtended(FCSLWords[FCentroidLongCol]);
  Lat := StrToExtended(FCSLWords[FCentroidLatCol]);

  Result := DDToPointLL(Long, Lat);
end;

function TGCSVFileReader.ReadPointObject : TGMapPoint;
begin
  Result := TGMapPoint.Create;
  Result.Centroid := ReadCentroid;
  Result.PresenterID := 1;
end;


function TGCSVFileReader.InternalOpen : Boolean;
begin
  FColumnNames := TStringList.Create;
  FCSLWords := TStringList.Create;

  FFirstLineTitles := true;
  FSeparator := ',';

  FCentroidLongCol := -1;
  FCentroidLatCol := -1;

  inherited InternalOpen;

  Result := FileExists(Filename);
  if Result then
  begin
    Name := ExtractFilename(Filename);

    // Create the MIF data stream
    FCSLStream := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite );
    FCSLReader := TGTextFileReader.Create(FCSLStream);
    FCSLReader.TextBufferSize := TEXT_BUFFER_SIZE;

    Result := True;
  end;
end;


function TGCSVFileReader.InternalClose : Boolean;
begin
  inherited InternalClose;

  FreeAndNil( FColumnNames );
  FreeAndNil( FCSLWords );

  FreeAndNil( FCSLReader );
  FreeAndNil( FCSLStream );

  Result := True;
end;

{
procedure TGCSVFileReader.ParseLine(const Value: String);
var
  idx : integer;
  tmp : string;
begin
  FCurrentLine := Value;

//  FCSLWords.CommaText := Value;

  //todo: improve the reading of data items from the line
  idx := Pos( FSeparator, FCurrentLine );
  while idx > 0 do
  begin
    tmp := Copy( FCurrentLine, 1, idx - 1 );
    FCSLWords.Add(tmp);
    FCurrentLine := Copy( FCurrentLine, idx + 1, Length( FCurrentLine ));
    idx := Pos( FSeparator, FCurrentLine );
  end;
end;
}
{procedure TGCSVFileReader.ParseLine(const Value: String);
var
  P, P1: PChar;
  S: string;
begin
  FCurrentLine := Value;

  P := PChar(Value);

  while P^ <> #0 do
  begin
    P1 := P;
    if ( P1^ = '"' ) or ( P1^ = '''' ) then // if we are starting with a quote char
    begin
      repeat
        Inc( P );
      until ( P^ = #0 ) or ( P^ = P1^ );

      if P^ <> #0 then
      begin
        SetString(S, P1 + 1, ( P - P1 ) - 1); // strip off the quote chars
        Inc(P);
      end;
    end
    else
    begin
      while (P^ <> #0) and (P^ <> Separator) do // find the end of the item
        Inc(P);
      SetString(S, P1, P - P1);
    end;

    FCSLWords.Add(Trim(S));

    if P^ = Separator then
    begin
      Inc(P);
      P1 := P;
      if P1^ = #0 then
        FCSLWords.Add('');
    end;
  end;
end;
}

procedure TGCSVFileReader.ParseLine( const Value: string );
var
  iState:  cardinal;
  i:       cardinal;
  iLength: cardinal;
  sField:  string;
begin
  FCurrentLine := Value;

  iLength := Length(Value);
  if iLength = 0 then
    Exit;

  // state machine
  iState := 0;
  for i := 1 to iLength do
    case iState of
    0: // Unknown state
      begin
        sField := '';
        if Value[i] = '"' then  // start of embedded quotes or commas
          iState := 2
        else
          if Value[i] = Separator then  // empty field
          begin
            FCSLWords.Add(sField);
            if i = iLength then // EOL
              FCSLWords.Add('');
          end
          else
          begin  // start of regular field
            sField := Value[i];
            iState := 1;
            if i = iLength then // EOL
              FCSLWords.Add(sField);
          end;
      end; // state 0
    1: // continuation of regular field
      begin
        if Value[i] = Separator then  // end of regular field
        begin
          FCSLWords.Add(sField);

          if i = iLength then // if end of input, then we know there remains a "null" field
            FCSLWords.Add('')
          else
            iState := 0;
        end
        else  // concatenate current char
        begin
          sField := sField + Value[i];
          if i = iLength then // EOL
            FCSLWords.Add(sField);
        end; // if Value[i] =
      end; // state 1
    2: // continuation of embedded quotes or commas
      begin
        if Value[i] = '"' then // end of embedded comma field or beginning of embedded quote
        begin
          if i < iLength then // NotTheEndPos
          begin
            if Value[i+1] = Separator then
              iState := 1 // end of embedded comma field
            else
              iState := 3;
          end
          else  // end of field since end of line
            FCSLWords.Add(sField);
        end
        else // concatenate current char
          sField := sField + Value[i];
      end; // state 2
    3: // beginning of embedded quote
      begin
        if Value[i] = '"' then
        begin
          sField := sField + Value[i];
          iState := 2;
        end;
      end; // state 3
    end; // case iState
end;

procedure TGCSVFileReader.LoadFromXMD(el: TGXML_Element);
begin
  inherited;

  if el <> nil then
  begin
    FSeparator := el.Attribute('Separator', ',');
    FSkipLines := el.Attribute('SkipLines', 0);

    //ToDo: Load in centroid units, projection etc.
    el := el.Element('Centroid');
    if el <> nil then
    begin
      FCentroidLongCol := el.Attribute('LongColumn', -1);
      FCentroidLatCol := el.Attribute('LatColumn', -1);
    end;

  end;
end;

function TGCSVFileReader.ProgressPercent: Double;
begin
  if FCSLReader <> nil then
    Result := ( FCSLReader.Position / FCSLReader.Size ) * 100.0
  else
    Result := 0.0;
end;

function TGCSVFileReader.ReadHeader : Boolean;
var
  idx : integer;
begin
  // load up the titles from the data file
  for idx := 1 to FSkipLines do
    ReadLine;

  if FFirstLineTitles and ReadLine then
    ColumnNames.Assign(FCSLWords);

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

function TGCSVFileReader.ReadObject : IGMapPoint;
begin
  inherited ReadObject;

  Result := nil;

  ReadLine;
  if not FCSLReader.EOT then
  begin
    Result := ReadPointObject;

    if Result <> nil then
      Result.ObjectDataCSL := FCSLWords.CommaText;
  end;
end;

destructor TGCSVFileReader.Destroy;
begin
  FreeAndNil( FColumnNames );
  FreeAndNil( FCSLWords );

  inherited;
end;

initialization
  RegisterGlobeClass( TGCSVFileReader, 'CSL File Reader' );
end.

