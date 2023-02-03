//-----------------------------------------------------------------------
// Summary
//	TGlobe ESRI E00 Format file reader
//
// Description
// 	Reads data objects from an E00 format file
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
unit GE00Reader;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Globe5, GClasses, GSysUtils, GMapObjects, GTextReader,
  GDataReaders, Globe5Utils, GConversions;

type
  
  //-------------------------------------------------------------
  //Description
  //Import class used to read ESRI .E00 interchange format files.
  //-------------------------------------------------------------
  TGE00FileReader = class(TGFileDataReader)
  private
    FE00Stream : TStream;
    FE00Reader : TGTextFileReader;

    FConversion : TGCoordConversion;

    FiCurrentChain : integer;

    FFoundPALData : Boolean;
    FiPolygonNumber : integer;
    FARCChains : TGChainStore;

    FARCDoublePrecision : Boolean;
    FPALDoublePrecision : Boolean;

    procedure AddNewDPPoint(const sTmp : string);
    procedure AddNewPoint(const sTmp : string; iLongStart, iLatStart : integer);
    function CopyPoints(APolygon : TGMapPoly; const sTmp : string; iNodeStart : integer) : Boolean;

    procedure ReadARCData;
    procedure ReadCNTData;
    procedure ReadTX7Data;
    procedure ReadPRJData;
    procedure ReadPALHeader;
    function ReadPALObject : TGMapPoly;
    function ReadARCObject : TGMapPoly;
  protected
    function ReadHeader : Boolean; override;
    function ReadObject : IGMapPoint; override;
    function InternalOpen : Boolean; override;
    function InternalClose : Boolean; override;
  public
    function ProgressPercent : Double; override;
  end;

implementation

{-------------------------------------------------------------------------}
procedure TGE00FileReader.AddNewDPPoint(const sTmp : string);
var
  Long, Lat : Extended;
begin
  Long := StrToExtended(Copy(sTmp, 1, 21));
  Lat := StrToExtended(Copy(sTmp, 22, 21));

  FARCChains[FiCurrentChain].Add( FConversion.ToPointLL(Long, Lat));
end;

{-------------------------------------------------------------------------}
procedure TGE00FileReader.AddNewPoint(const sTmp : string; iLongStart, iLatStart : integer);
var
  Long, Lat : Extended;
begin
  Long := StrToExtended(Copy(sTmp, iLongStart, 14));
  Lat := StrToExtended(Copy(sTmp, iLatStart, 14));

  FARCChains[FiCurrentChain].Add( FConversion.ToPointLL(Long, Lat));
end;

{-------------------------------------------------------------------------}
function TGE00FileReader.CopyPoints(APolygon : TGMapPoly; const sTmp : string; iNodeStart : integer) : Boolean;
var
  iArcNum, idx, iCount : integer;
begin
  iArcNum := StrToInt(Trim(Copy(sTmp, iNodeStart, 10)));
  Result := iArcNum = 0;
  if not Result then
  begin
    iCount := FARCChains[Abs(iArcNum) - 1].Count - 1;

    with FARCChains[Abs(iArcNum) - 1] do
    begin
      if iArcNum > 0 then
        for idx := 0 to iCount do
          APolygon.Chains.Last.Add(AsLL[idx])
      else
        for idx := iCount downto 0 do
          APolygon.Chains.Last.Add(AsLL[idx])
    end;
  end;
end;

{-------------------------------------------------------------------------}
procedure TGE00FileReader.ReadARCData;
var
  sTmp : string;
  iIndex, iPoints : integer;
begin
  FiCurrentChain := -1;
  repeat
    FE00Reader.ReadLn(sTmp);

    iIndex := StrToInt(Copy(sTmp, 1, 10));
    iPoints := StrToInt(Copy(sTmp, 61, 10));

    if iIndex = -1 then
      Break;

    if iPoints > 0 then
    begin
      Inc(FiCurrentChain);
      if FiCurrentChain >= FARCChains.Count then
        FARCChains.Count := FARCChains.Count + 128;
    end;

    if FARCDoublePrecision then
    begin
      while iPoints > 0 do
      begin
        FE00Reader.ReadLn(sTmp);
        AddNewDPPoint(sTmp);
        Dec(iPoints);
      end;
    end
    else
    begin
      while iPoints > 1 do
      begin
        FE00Reader.ReadLn(sTmp);
        AddNewPoint(sTmp, 1, 15);
        AddNewPoint(sTmp, 29, 43);
        Dec(iPoints, 2);
      end;

      if iPoints > 0 then
      begin
        FE00Reader.ReadLn(sTmp);
        AddNewPoint(sTmp, 1, 15);
      end;
    end;
  until FE00Reader.EOT;

  FARCChains.Count := FiCurrentChain + 1;
end;

{-------------------------------------------------------------------------}
procedure TGE00FileReader.ReadCNTData;
begin
//  gGlobe.ProgressMessage(pmMESSAGE, Format('%s: Reading CNT data...', [gLayer.Name]));
end;

{-------------------------------------------------------------------------}
procedure TGE00FileReader.ReadPALHeader;
var
  idx, iArcs : integer;
  sTmp : string;
begin
  { Process the PAL file }
  FE00Reader.ReadLn(sTmp);
  iArcs := StrToInt(Copy(sTmp, 1, 10));

  if FPALDoublePrecision then
    FE00Reader.ReadLn(sTmp);

  { Skip the universal polygon }
  idx := (iArcs div 2) + (iArcs mod 2);
  while idx > 0 do
  begin
    FE00Reader.ReadLn(sTmp);
    Dec(idx);
  end;
end;

{-------------------------------------------------------------------------}
function TGE00FileReader.ReadARCObject : TGMapPoly;
begin
  if FiPolygonNumber < FARCChains.Count then
  begin
    Result := TGMapPoly.Create;
    Result.PresenterID := DEFAULT_POLYLINE_PRESENTERID;
    Result.Attribute[0] := IntToStr(FiPolygonNumber + 1);

    Result.Chains[0].Assign(FARCChains[FiPolygonNumber]);
    Inc(FiPolygonNumber);
  end
  else
    Result := nil;
end;

{-------------------------------------------------------------------------}
function TGE00FileReader.ReadPALObject : TGMapPoly;
var
  iArcs : integer;
  sTmp : string;
begin
  Result := nil;

  { Process the PAL file }

  { read in the other polygons }
  FE00Reader.ReadLn(sTmp);
  iArcs := StrToInt(Copy(sTmp, 1, 10));
  if iArcs = -1 then
    Exit;

  if FPALDoublePrecision then
    FE00Reader.ReadLn(sTmp);

  Result := TGMapPoly.Create;
  Result.PresenterID := DEFAULT_POLYGON_PRESENTERID;
  Result.Closed := true;
  Result.Attribute[0] := IntToStr(FiPolygonNumber + 1);

  Inc(FiPolygonNumber);

  while iArcs > 0 do
  begin
    FE00Reader.ReadLn(sTmp);

    if CopyPoints(Result, sTmp, 1) then
      with Result do
        if Chains.Last.Count > 0 then
          Chains.Count := Chains.Count + 1;
    Dec(iArcs);

    if iArcs > 0 then
    begin
      if CopyPoints(Result, sTmp, 31) then
        with Result do
          if Chains.Last.Count > 0 then
            Chains.Count := Chains.Count + 1;
      Dec(iArcs);
    end;
  end;
end;

{-------------------------------------------------------------------------}
procedure TGE00FileReader.ReadTX7Data;
var
  idx, iPoints : integer;
  sTmp, sText : string;
  Long, Lat : Extended;
  aObj : TGMapPoint;
begin
  { Process the TX7 file }
  FE00Reader.ReadLn(sTmp);
  while not FE00Reader.EOT do
  begin
    FE00Reader.ReadLn(sTmp);
    if StrToInt(Copy(sTmp, 1, 10)) = -1 then
      Break;

    iPoints := StrToInt(Copy(sTmp, 21, 10));
    for idx := 0 to 8 do
      FE00Reader.ReadLn(sTmp);

    Long := StrToExtended(Copy(sTmp, 1, 14));
    Lat := StrToExtended(Copy(sTmp, 15, 14));

    for idx := 0 to iPoints - 1 do
      FE00Reader.ReadLn(sTmp);
    sText := Trim(sTmp);

    if sText <> '' then
    begin
      aObj := TGMapPoint.Create;
      aObj.PresenterID := DEFAULT_POINT_PRESENTERID;
      aObj.Attribute[0] := sText;
      aObj.Centroid := FConversion.ToPointLL(Long, Lat);
//ToDO:      FLayer.ObjectSource.Add( aPoly );
    end;
  end;
end;

{-------------------------------------------------------------------------}
procedure TGE00FileReader.ReadPRJData;
var
  sTmp, sProj : string;
  iParameter : integer;
  sParams : array[0..8] of string;
  convClass : TPersistentClass;
begin
  FreeAndNil( FConversion );

  sParams[4] := '000000.000000';
  sParams[5] := '000000.000000';

  iParameter := -1;

  repeat
    FE00Reader.ReadLn(sTmp);
    sTmp := Uppercase(sTmp);
    if Trim(sTmp) = 'EOP' then
      Break;

    if Trim(Copy(sTmp, 1, 14)) = 'PROJECTION' then
    begin
      sProj := Uppercase(Trim(Copy(sTmp, 15, 255)));

      // get the registered coordinate conversion class
      convClass := GetClass( 'TG' + sProj + 'CoordConversion' );

      if ( convClass = nil ) or ( not convClass.InheritsFrom( TGCoordConversion )) then
        FConversion := TGCoordConversion.Create
      else
        FConversion := convClass.Create as TGCoordConversion;

      Assert( FConversion <> nil, 'Failed to create CoordConversion' );
    end;

    if Trim(Copy(sTmp, 1, 14)) = 'UNITS' then
    begin
      if Trim(Copy(sTmp, 15, 255)) = 'METERS' then
        FConversion.InputUnits  := guMeter;
      if Trim(Copy(sTmp, 15, 255)) = 'FEET' then
        FConversion.InputUnits := guFeet;
    end;

    if Trim(Copy(sTmp, 1, 14)) = 'SPHEROID' then
    begin
      if Trim(Copy(sTmp, 15, 255)) = 'CLARKE1866' then
        FConversion.Spheroid := Clarke1866;
      if Trim(Copy(sTmp, 15, 255)) = 'WGS84' then
        FConversion.Spheroid := WGS84;
    end;

    if (Trim(sTmp) <> '~') and (iParameter >= 0) and (iParameter <= 8) then
    begin
      sParams[iParameter] := sTmp;
      Inc(iParameter);
    end;

    if Trim(Copy(sTmp, 1, 14)) = 'PARAMETERS' then
      iParameter := 0;
  until FE00Reader.EOT;

  FConversion.ReadE00_PARAMS( sParams );
end;

function TGE00FileReader.ReadHeader : Boolean;
var
  sTmp : string;
  ARCOffset : Cardinal;
  CNTOffset : Cardinal;
  PRJOffset : Cardinal;
  TX7Offset : Cardinal;
  PALOffset : Cardinal;
begin
  ARCOffset := 0;
  CNTOffset := 0;
  PRJOffset := 0;
  TX7Offset := 0;
  PALOffset := 0;

  DecimalSeparator := '.';
  FARCChains := TGChainStore.Create;

  while not FE00Reader.EOT do
  begin
    FE00Reader.ReadLn(sTmp);

    if Length( sTmp ) = 6 then
    begin
      if (CompareText(sTmp, 'PRJ  2') = 0) or (CompareText(sTmp, 'PRJ  3') = 0) then
      begin
        PRJOffset := FE00Reader.Position;
      end
      else if (CompareText(sTmp, 'ARC  2') = 0) or (CompareText(sTmp, 'ARC  3') = 0) then
      begin
        ARCOffset := FE00Reader.Position;
        FArcDoublePrecision := sTmp[Length(sTmp)] = '3';
      end
      else if (CompareText(sTmp, 'CNT  2') = 0) or (CompareText(sTmp, 'CNT  3') = 0) then
      begin
        CNTOffset := FE00Reader.Position;
      end
      else if (CompareText(sTmp, 'TX7  2') = 0) or (CompareText(sTmp, 'TX7  3') = 0) then
      begin
        TX7Offset := FE00Reader.Position;
      end
      else if (CompareText(sTmp, 'PAL  2') = 0) or (CompareText(sTmp, 'PAL  3') = 0) then
      begin
        PALOffset := FE00Reader.Position;
        FPALDoublePrecision := sTmp[Length(sTmp)] = '3';
      end;
    end;
  end;

  if PRJOffset <> 0 then
  begin
    FE00Reader.Position := PRJOffset;
    ReadPRJData;
  end;
  if ARCOffset <> 0 then
  begin
    FE00Reader.Position := ARCOffset;
    ReadARCData;
  end;
  if CNTOffset <> 0 then
  begin
    FE00Reader.Position := CNTOffset;
    ReadCNTData;
  end;
  if TX7Offset <> 0 then
  begin
//    FE00Reader.Position := TX7Offset;
//    ReadTX7Data;
  end;
  if PALOffset <> 0 then
  begin
    FE00Reader.Position := PALOffset;
    ReadPALHeader;
    FFoundPALData := True;
  end;

  FiPolygonNumber := 0;
  Result := true;
end;

function TGE00FileReader.ReadObject : IGMapPoint;
begin
  inherited ReadObject;

  if FFoundPALData then
    Result := ReadPALObject as IGMapPoint
  else
    Result := ReadARCObject as IGMapPoint;
end;

function TGE00FileReader.InternalOpen : Boolean;
begin
  inherited InternalOpen;

  Result := FileExists( Filename );
  if Result then
  begin
    FE00Stream := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite );
    FE00Reader := TGTextFileReader.Create( FE00Stream );
  end;

  FConversion := TGCoordConversion.Create;
end;

function TGE00FileReader.InternalClose : Boolean;
begin
  inherited InternalClose;

  FreeAndNil( FConversion );
  FreeAndNil( FARCChains );
  FreeAndNil( FE00Reader );
  FreeAndNil( FE00Stream );
  Result := true;
end;

function TGE00FileReader.ProgressPercent : Double;
begin
  if FE00Reader <> nil then
    Result := ( FE00Reader.Position / FE00Reader.Size ) * 100.0
  else
    Result := 0.0;
end;

initialization
  RegisterGlobeClass( TGE00FileReader, 'E00 File Reader' );
end.

