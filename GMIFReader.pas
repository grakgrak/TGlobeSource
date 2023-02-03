//-----------------------------------------------------------------------
// Summary
//	TGlobe .MIF format file reader
//
// Description
// 	Reads data objects from a MapInfo .MIF file
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
{$I GLOBE5.INC}
unit GMIFReader;

interface

uses
  Windows, Forms, Graphics, Classes, SysUtils, Globe5, GSysUtils, GDataReaders,
  GMapObjects, GPresenters, GTextReader, GClasses, GXML;

const
  TEXT_BUFFER_SIZE = 64 * 1024;

  DefaultPenColor = clBlack;
  DefaultPenStyle = gpsSolid;
  DefaultPenWidth = 1;
  DefaultPenUnit = guPixel;
  DefaultBrushColor = clBlack;
  DefaultBrushStyle = gbsSolid;
  DefaultFontName = 'Arial';
  DefaultFontSize = 9;
  DefaultFontBkColor = clWhite;
  DefaultFontColor = clBlack;
  DefaultFontUnit = guPixel;
  DefaultFontAngle = 0;
  DefaultFontStyle = [];

type
  
  //----------------------------------------------------------------------------
  //Description
  //Reads the data from a MapInfo MIF/MID file pair and creates a TGlobe spatial
  //database layer with presenters.                                             
  //----------------------------------------------------------------------------
  TGMIFFileReader = class(TGFileDataReader)
  private
    FMIFStream : TFileStream;
    FMIFReader : TGTextFileReader;

    FMIDStream : TFileStream;
    FMIDReader : TGTextFileReader;

    FMIFCoordSys : String;
    FMIDDelimiter : String;
    FMIDColumnNames : TStringList;

    FMIFWords : TStringList;
    FCurrentMIFLine: String;
    FCurrentMIDLine: String;

    procedure SetCurrentMIFLine(const Value: String);
  protected
    //------------------------------------
    //Description
    //Records the version of the MIF file.
    //------------------------------------
    iMIFVersion : integer;

    aPen : TGPen;
    aBrush : TGBrush;
    aFont : TGFont;
    aTitleFont : TGFont;

    function MIF2DelphiBrush(iStyle : Integer) : TGBrushStyle;
    function MIF2DelphiPen(iStyle : Integer) : TGPenStyle;
    function MIF2DelphiColor(iColor : Integer) : Integer;
    function ReadLLPointFromLine : TGPointLL;
    function ReadLLPoint(iLongPos, iLatPos : integer) : TGPointLL;
    function ReadPenObject : Boolean;
    function ReadBrushObject : Boolean;
    function ReadFontObject : Boolean;
    function ReadLineObject : TGMapPoly;
    function ReadPLineObject : TGMapPoly;
    function ReadPointObject : TGMapPoint;
    function ReadRegionObject : TGMapPoly;
    function ReadTextObject : TGMapPoly;

    function ReadMIFLine : Boolean;
    function ReadMIDLine : Boolean;

    function GetMIFWord(iIndex : integer) : String;

    function ReplaceStr(const sOrigin, sToReplace, sReplacer : String) : String;

    function InternalOpen : Boolean; override;
    function InternalClose : Boolean; override;

    function ReadHeader : Boolean; override;
    function ReadObject : IGMapPoint; override;

    property CurrentMIFLine : String read FCurrentMIFLine write SetCurrentMIFLine;
  public
    destructor Destroy; override;
    function ProgressPercent: Double;  override;

    //------------------------------------------------------------------------------
    //Description
    //Records the coordinate system used when specifing the coordinates in the file.
    //------------------------------------------------------------------------------
    property MIFCoordSys : String read FMIFCoordSys write FMIFCoordSys;
    //------------------------------------------------------------------
    //Description
    //Records the delimeter used to seperate the items in the .MID file.
    //------------------------------------------------------------------
    property MIDDelimiter : String read FMIDDelimiter write FMIDDelimiter;
    //----------------------------------------------------------------------------
    //Description
    //Contains a comma seperated list of the column names stored in the .MID file.
    //----------------------------------------------------------------------------
    property MIDColumnNames : TStringList read FMIDColumnNames write FMIDColumnNames;
  end;

implementation

uses GResource;

function TGMIFFileReader.GetMIFWord(iIndex : integer) : String;
begin
  if iIndex < FMIFWords.Count then
    Result := FMIFWords[iIndex]
  else
    Result := '';
end;

function TGMIFFileReader.ReplaceStr(const sOrigin, sToReplace, sReplacer : String) : String;
{ ex: sNewString := ReplaceStr('Hej hopp', ' ', '_');
  results in sNewString := 'Hej_hopp');}
var
  sToReturn : String;
  iCurPos : Integer;
  sTmpRet, sTmpStr : String;
begin
  iCurPos := Pos(sToReplace, sOrigin);
  if iCurPos > 0 then
  begin
    sTmpRet := Copy(sOrigin, 1, Length(sOrigin));
    sToReturn := '';
    while iCurPos > 0 do
    begin
      sTmpStr := Copy(sTmpRet, 1, iCurPos - 1);
      sToReturn := sToReturn + sTmpStr + sReplacer;
      sTmpRet := Copy(sTmpRet, iCurPos + Length(sToReplace), Length(sTmpRet));
      iCurPos := Pos(sToReplace, sTmpRet);
    end;
    sToReturn := sToReturn + sTmpRet;
    Result := sToReturn;
  end
  else
    Result := sOrigin;
end;

function TGMIFFileReader.MIF2DelphiBrush(iStyle : Integer) : TGBrushStyle;
const
  gBrushStyle : array[1..68] of TGBrushStyle = (
    gbsClear, gbsSolid, gbsHorizontal, gbsVertical, gbsBDiagonal, gbsFDiagonal, gbsCross, gbsDiagCross,
    gbsPattern1, gbsPattern32, gbsPattern26, gbsPattern29, gbsPattern33, gbsPattern1, gbsPattern35, gbsHorizontal,
    gbsPattern3, gbsPattern4, gbsPattern5, gbsPattern6, gbsVertical, gbsPattern8, gbsPattern9, gbsPattern10,
    gbsPattern11, gbsFDiagonal, gbsPattern13, gbsPattern14, gbsPattern15, gbsPattern16, gbsBDiagonal, gbsPattern18,
    gbsPattern19, gbsPattern20, gbsPattern21, gbsCross, gbsPattern23, gbsPattern24, gbsPattern25, gbsPattern26,
    gbsPattern27, gbsPattern28, gbsPattern29, gbsPattern30, gbsPattern31, gbsPattern32, gbsPattern33, gbsPattern34,
    gbsPattern35, gbsPattern36, gbsPattern37, gbsPattern38, gbsPattern39, gbsPattern40, gbsPattern41, gbsPattern42,
    gbsPattern43, gbsPattern44, gbsPattern45, gbsPattern46, gbsPattern47, gbsPattern48, gbsPattern49, gbsPattern50,
    gbsPattern46, gbsPattern45, gbsPattern44, gbsPattern50
    );
begin
  if iStyle > High(gBrushStyle) then
    Result := gbsSolid
  else
    Result := gBrushStyle[iStyle];
end;

function TGMIFFileReader.MIF2DelphiPen(iStyle : Integer) : TGPenStyle;
begin
  case iStyle of
    1 : Result := gpsClear;
    3..9 : Result := gpsDash;
    10..13 : Result := gpsDot;
    14..16 : Result := gpsDashDot;
    20..25 : Result := gpsDashDotDot;
    71..77 : Result := gpsInsideFrame;
  else
    Result := gpsSolid;
  end;
end;

function TGMIFFileReader.MIF2DelphiColor(iColor : Integer) : Integer;
var
  sTmp : String;
begin
  sTmp := IntToHex(iColor, 6);
  Result := StrToInt('$' + Copy(sTmp, 5, 2) + Copy(sTmp, 3, 2) + Copy(sTmp, 1, 2));
end;

function TGMIFFileReader.ReadMIFLine : Boolean;
var
  sTmp : String;
begin
  repeat
    FMIFReader.ReadLn(sTmp);
    Result := Length(sTmp) > 0;
  until FMIFReader.EOT or Result;

  if Result then
    CurrentMIFLine := sTmp
  else
    CurrentMIFLine := '';
end;

function TGMIFFileReader.ReadMIDLine : Boolean;
begin
  Result := (FMIDReader <> nil) and not FMIDReader.EOT;

  if Result then
  begin
    FMIDReader.ReadLn(FCurrentMIDLine);
  end;
end;

function TGMIFFileReader.ReadLLPoint(iLongPos, iLatPos : integer) : TGPointLL;
var
  Long, Lat : Extended;
begin
  Long := StrToExtended(FMIFWords[iLongPos]);
  Lat := StrToExtended(FMIFWords[iLatPos]);

  {ToDo:  if Assigned( OnProjectCoords ) then
      OnProjectCoords( Self, Long, Lat );}

  Result := DDToPointLL(Long, Lat);
end;

function TGMIFFileReader.ReadLLPointFromLine : TGPointLL;
begin
  ReadMIFLine;
  Result := ReadLLPoint(0, 1);
end;

function TGMIFFileReader.ReadPointObject : TGMapPoint;
var
  ptLL : TGPointLL;
  aPresenter : TGPointPresenter;
begin
  ptLL := ReadLLPoint(1, 2);

  Result := TGMapPoint.Create;
  Result.Centroid := ptLL;

  ReadMIFLine;

  aPresenter := TGPointPresenter.Create(ParentGlobe);
  aPresenter.PointPen.Assign(aPen);
  aPresenter.PointBrush.Assign(aBrush);
  aPresenter.PointFont.Assign(aFont);
  aPresenter.TitleFont.Assign(aTitleFont);
  aPresenter.PointType := ppCircle;
  aPresenter.PointUnit := guPixel;
  aPresenter.PointSize := 6;


  Result.PresenterID := Presenters.FindMatch(aPresenter);

  if Result.PresenterID = 0 then
    Result.PresenterID := Presenters.Add(aPresenter)
  else
    aPresenter.Free;
end;

function TGMIFFileReader.ReadPenObject : Boolean;
var
  sTmp : String;
  slPen : TStrings;
begin
  Result := CompareText(GetMIFWord(0), 'Pen') = 0;

  if not Result then
    Exit;

  sTmp := GetMIFWord(1);
  slPen := TStringList.Create;
  slPen.CommaText := Copy(sTmp, 2, Length(sTmp) - 2);

  try
    aPen.Define(
      MIF2DelphiColor(StrToInt(slPen[2])),
      MIF2DelphiPen(StrToInt(slPen[1])),
      StrToIntDef(slPen[0], 1), guPixel);
  finally
    slPen.Free;
  end;

  ReadMIFLine;
end;

function TGMIFFileReader.ReadBrushObject : Boolean;
var
  sTmp : String;
  slBrush : TStrings;
begin
  Result := CompareText(GetMIFWord(0), 'Brush') = 0;

  if not Result then
    Exit;

  sTmp := GetMIFWord(1);
  slBrush := TStringList.Create;
  slBrush.CommaText := Copy(sTmp, 2, Length(sTmp) - 2);
  with aBrush do
  try
    Style := MIF2DelphiBrush(StrToInt(slBrush[0]));
    Color := MIF2DelphiColor(StrToInt(slBrush[1]));
    if (slBrush.Count > 2 ) then
      BkColor := MIF2DelphiColor(StrToInt(slBrush[2]));
  finally
    slBrush.Free;
  end;

  ReadMIFLine;
end;

function TGMIFFileReader.ReadFontObject : Boolean;
var
  sTmp : String;
  slFont : TStrings;
begin
  Result := CompareText(GetMIFWord(0), 'Font') = 0;

  if not Result then
    Exit;

  slFont := TStringList.Create;
  sTmp := Trim(CurrentMIFLine);
  slFont.CommaText := Copy(sTmp, 7, Length(sTmp) - 7);

  try
    aTitleFont.Name := ReplaceStr(slFont[0], '"', '');
    aTitleFont.Style := [];
    aTitleFont.Size := StrToInt(slFont[2]);
    aTitleFont.Color := MIF2DelphiColor(StrToInt(slFont[3]));
    aTitleFont.Angle := 0;

    while ReadMIFLine do
    begin
      slFont.CommaText := ReplaceStr(Trim(CurrentMIFLine), ' ', ',');
//      slFont.Delimiter := ' ';
//      slFont.DelimitedText := Trim(CurrentMIFLine);

      if CompareText(slFont[0], 'Angle') = 0 then
      begin
        aTitleFont.Angle := Round(StrToExtended(slFont[1])) * 10;
        continue;
      end;
      if CompareText(slFont[0], 'Spacing') = 0 then
        continue;
      if CompareText(slFont[0], 'justify') = 0 then
        continue;
      if CompareText(slFont[0], 'Label') = 0 then
        continue;
      break;
    end;
  finally
    slFont.free;
  end;
end;

function TGMIFFileReader.ReadRegionObject : TGMapPoly;
var
  idx, iParts, iPoints : integer;
  aPresenter : TGPolygonPresenter;
begin
  Result := TGMapPoly.Create;
  Result.Closed := True;

  Result.Chains.Count := StrToInt(GetMIFWord(1)); { get number of parts to region }
  for iParts := 0 to Result.Chains.Count - 1 do
  begin
    ReadMIFLine;
    iPoints := StrToInt(GetMIFWord(0)); // get number of points in this region
    Result.Chains[iParts].Count := iPoints;
    for idx := 0 to iPoints - 1 do
      Result.Chains[iParts].AsLL[idx] := ReadLLPointFromLine;
  end;

  // read attributes
  ReadMIFLine;
  while True do
    if not ReadPenObject then
      if not ReadBrushObject then
        if CompareText(GetMIFWord(0), 'Center') <> 0 then
          break
        else
          if not ReadMIFLine then
            break;

  aPresenter := TGPolygonPresenter.Create(ParentGlobe);
  aPresenter.PolyPen.Assign(aPen);
  aPresenter.PolyBrush.Assign(aBrush);

  Result.PresenterID := Presenters.FindMatch(aPresenter);

  if Result.PresenterID = 0 then
    Result.PresenterID := Presenters.Add(aPresenter)
  else
    aPresenter.Free;
end;

function TGMIFFileReader.ReadPLineObject : TGMapPoly;
var
  idx, iParts, iPoints : integer;
  sTmp : String;
  aPresenter : TGPolylinePresenter;
  MultipleParts : Boolean;
begin
  Result := TGMapPoly.Create;

  MultipleParts := False;
  iPoints := 0;
  Result.Chains.Count := 1;

  if iMIFVersion >= 300 then
  begin
    sTmp := GetMIFWord(1);
    MultipleParts := CompareText(sTmp, 'Multiple') = 0;
    if MultipleParts then
      Result.Chains.Count := StrToInt(GetMIFWord(2)) // get number of parts to region
    else
    begin
      iPoints := StrToIntDef( sTmp, 0 ); // get number of points in this region

      if iPoints = 0 then
      begin
        ReadMIFLine;
        iPoints := StrToInt(GetMIFWord(0)); { get number of points in this region }
      end;
    end;
  end
  else
    if iMIFVersion = 1 then
      iPoints := StrToInt(GetMIFWord(1)) { get number of points in this region }
    else
    begin
      Result.Chains.Count := StrToInt(GetMIFWord(1)); { get number of parts to region }
      MultipleParts := True; { region has multiple parts }
    end;

  for iParts := 0 to Result.Chains.Count - 1 do
  begin
    if MultipleParts then
    begin
      ReadMIFLine;
      iPoints := StrToInt(GetMIFWord(0)); { get number of points in this region }
    end;

    Result.Chains[iParts].Count := iPoints;
    for idx := 0 to iPoints - 1 do
      Result.Chains[iParts].AsLL[idx] := ReadLLPointFromLine;
  end;

  { read attributes }
  ReadMIFLine;
  while True do
    if not ReadPenObject then
      if CompareText(GetMIFWord(0), 'Smooth') <> 0 then
        break
      else
        if not ReadMIFLine then
          break;

  aBrush.Define(DefaultBrushColor, DefaultBrushColor, DefaultBrushStyle);

  aPresenter := TGPolylinePresenter.Create(ParentGlobe);
  aPresenter.PolyPen.Assign(aPen);
  aPresenter.PolyBrush.Assign(aBrush);

  Result.PresenterID := Presenters.FindMatch(aPresenter);

  if Result.PresenterID = 0 then
    Result.PresenterID := Presenters.Add(aPresenter)
  else
    aPresenter.Free;
end;

function TGMIFFileReader.ReadLineObject : TGMapPoly;
var
  aPresenter : TGPolylinePresenter;
begin
  Result := TGMapPoly.Create;

  Result.Chains[0].Add(ReadLLPoint(1, 2));
  Result.Chains[0].Add(ReadLLPoint(3, 4));

  ReadMIFLine;
  ReadPenObject;

  aPresenter := TGPolylinePresenter.Create(ParentGlobe);
  aPresenter.PolyPen.Assign(aPen);
  aPresenter.PolyBrush.Assign(aBrush);

  Result.PresenterID := Presenters.FindMatch(aPresenter);

  if Result.PresenterID = 0 then
    Result.PresenterID := Presenters.Add(aPresenter)
  else
    aPresenter.Free;
end;

function TGMIFFileReader.ReadTextObject : TGMapPoly;
var
  sText : String;
  ptLL1, ptLL2 : TGPointLL;
  aPresenter : TGPointPresenter;
begin
  ReadMIFLine;
  sText := CurrentMIFLine;
  sText := ReplaceStr(ReplaceStr(Trim(sText), '"', ''), '\n', ' ');

  ReadMIFLine;
  ptLL1 := ReadLLPoint(0, 1);
  ptLL2 := ReadLLPoint(2, 3);

  Result := TGMapPoly.Create;

  with Result do
  begin
    Centroid := ptLL1;

    ReadMIFLine;
    ReadFontObject;

    aTitleFont.SizeUnit := guGlobeUnit;
    with ptLL2 do
//      aTitleFont.Size := Abs(ptLL1.iLatY - ptLL2.iLatY);
      aTitleFont.Size :=  MinVal( Abs(ptLL1.iLatY - ptLL2.iLatY),Abs(LongDiff(ptLL1.iLongX, ptLL2.iLongX)));

    aPen.Define(DefaultPenColor, DefaultPenStyle, DefaultPenWidth, DefaultPenUnit);
    aBrush.Define(DefaultBrushColor, DefaultBrushColor, DefaultBrushStyle);
    aFont.Define(DefaultFontName, DefaultFontColor, DefaultFontBkColor, DefaultFontSize, DefaultFontUnit, DefaultFontAngle, DefaultFontStyle);

    aPresenter := TGPointPresenter.Create(ParentGlobe);
    aPresenter.PointPen.Assign(aPen);
    aPresenter.PointBrush.Assign(aBrush);
    aPresenter.PointFont.Assign(aFont);
    aPresenter.TitleFont.Assign(aTitleFont);
    aPresenter.TitleAlignment := taCenter;
    aPresenter.PointType := ppNone;
    aPresenter.PointUnit := guPixel;
    aPresenter.PointSize := 6;

    Result.PresenterID := Presenters.FindMatch(aPresenter);

    if Result.PresenterID = 0 then
      Result.PresenterID := Presenters.Add(aPresenter)
    else
      aPresenter.Free;
  end;
end;

function TGMIFFileReader.InternalOpen : Boolean;
begin
  inherited InternalOpen;
  
  Result := FileExists(Filename);
  if Result then
  begin
    aPen := TGPen.Create;
    aBrush := TGBrush.Create;
    aFont := TGFont.Create;
    aTitleFont := TGFont.Create;

    FMIDColumnNames := TStringList.Create;
    FMIFWords := TStringList.Create;

    Name := ExtractFilename(Filename);

    // Create the MIF data stream
    FMIFStream := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite );
    FMIFReader := TGTextFileReader.Create(FMIFStream);
    FMIFReader.TextBufferSize := TEXT_BUFFER_SIZE;

    if FileExists(ChangeFileExt(Filename, '.MID')) then
    begin
      FMIDStream := TFileStream.Create( ChangeFileExt(Filename, '.MID'), fmOpenRead or fmShareDenyWrite );
      FMIDReader := TGTextFileReader.Create(FMIDStream);
      FMIDReader.TextBufferSize := TEXT_BUFFER_SIZE;
    end;

    Result := True;
  end;
end;

function TGMIFFileReader.InternalClose : Boolean;
begin
  inherited InternalClose;
  
  FreeAndNil( FMIDColumnNames );
  FreeAndNil( FMIFWords );

  FreeAndNil( FMIFReader );
  FreeAndNil( FMIDReader );

  FreeAndNil( FMIFStream );
  FreeAndNil( FMIDStream );

  FreeAndNil( aPen );
  FreeAndNil( aBrush );
  FreeAndNil( aFont );
  FreeAndNil( aTitleFont );

  Result := True;
end;

function TGMIFFileReader.ProgressPercent: Double;
begin
  if FMIFReader <> nil then
    Result := ( FMIFReader.Position / FMIFReader.Size ) * 100.0
  else
    Result := 0.0;
end;

function TGMIFFileReader.ReadHeader : Boolean;
var
  sTmp : String;
  bColumns : Boolean;
begin
  MIDDelimiter := #9;

  bColumns := False;
  while ReadMIFLine do
  begin
    sTmp := GetMIFWord(0);

    if CompareText(sTmp, 'Data') = 0 then
      Break;

    if CompareText(sTmp, 'Version') = 0 then
      iMIFVersion := StrToInt(GetMIFWord(1));

    if CompareText(sTmp, 'Delimiter') = 0 then
      MIDDelimiter := GetMIFWord(1)[2];

    if CompareText(sTmp, 'CoordSys') = 0 then
      MIFCoordSys := CurrentMIFLine;

    if bColumns then
      MIDColumnNames.Add(sTmp);

    if CompareText(sTmp, 'Columns') = 0 then
      bColumns := True;
  end;

  MetaData.Values['ColumnNames'] := MIDColumnNames.CommaText;

  // Check the Projection Model used.
  CurrentMIFLine := MIFCoordSys;
  if not ((CompareText(GetMIFWord(1), 'Earth') = 0)
    and (CompareText(GetMIFWord(2), 'Projection') = 0)
    and (CompareText(GetMIFWord(3), '1,') = 0)) then
    raise EGException.Create(rsMIFImportError);

  Result := True;
end;

function TGMIFFileReader.ReadObject : IGMapPoint;
var
  sTmp : String;
begin
  inherited ReadObject;

  Result := nil;

  while not FMIFReader.EOT do
  begin
    sTmp := lowercase(GetMIFWord(0)); // Check the current line

    if (sTmp = 'point') or (sTmp = 'region') or (sTmp = 'line')
        or (sTmp = 'pline') or (sTmp = 'text') then
    begin
      if sTmp = 'point' then
        Result := ReadPointObject
      else
        if sTmp = 'region' then
          Result := ReadRegionObject
        else
          if sTmp = 'line' then
            Result := ReadLineObject
          else
            if sTmp = 'pline' then
              Result := ReadPlineObject
            else
              if sTmp = 'text' then
                Result := ReadTextObject;

      ReadMIDLine;

      if Result <> nil then
        Result.ObjectDataCSL := FCurrentMIDLine;  // Save the MID CSL

      Exit;
    end;

    ReadMIFLine;
  end;
end;

procedure TGMIFFileReader.SetCurrentMIFLine(const Value: String);
var
  iLen : integer;
  P, S : PChar;
  bSpace : Boolean;
  sWord : String;
begin
  FCurrentMIFLine := Value;

  FMIFWords.Clear;

  iLen := Length(CurrentMIFLine);
  if iLen = 0 then
    Exit;
  P := Pointer(@CurrentMIFLine[1]);

  bSpace := True;
  S := P;
  repeat
    // skip blank space or words
    while (iLen > 0) and (bSpace = (P^ in [#9, ' '])) do
    begin
      Inc(P);
      Dec(iLen);
    end;
    bSpace := not bSpace;

    if not bSpace then
      S := P
    else
      if integer( P - S ) > 0 then
      begin
        SetString(sWord, S, P - S);
        FMIFWords.Add(sWord);
      end;
  until iLen = 0;
end;

destructor TGMIFFileReader.Destroy;
begin
  FreeAndNil( FMIFReader );
  FreeAndNil( FMIDReader );
  FreeAndNil( FMIDColumnNames );
  FreeAndNil( FMIFWords );

  inherited;
end;

initialization
  RegisterGlobeClass( TGMIFFileReader, 'MIF File Reader' );
end.

