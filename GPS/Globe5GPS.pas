{-------------------------------------------------------------------------
  Module: TGlobe GPS Component

  Comment: Provides a NMEA 0183 parsing component

  Classes: TGlobeGPS

  Author:    Graham Knight
  Email:     tglobe@tglobe.com
-------------------------------------------------------------------------}
unit Globe5GPS;

interface

uses
  SysUtils, Classes, SyncObjs;

const
  GPS_ONE_MINUTE = 1.0 / 60.0;
  GPS_INACTIVE_SECS = 5.0;  // Inactivity window

type
  EGPSException = class( Exception );

  TGSatInView = record
    PRN : String;    // ID number for satelite
    Elevation : integer;  // 90 maximum can be negative
    Azimuth : integer;    // 0 - 359 from true north
    SNR : integer;    // 0 - 99 db (or nul if not tracking) Signal noise ratio
  end;

  TGActiveSats = record
    Valid : Boolean;
    ModeType : String;
    Mode : String;
    Channel : array [0..11] of String;
    PDOP : Double;  // Position Dilution of Precision
    HDOP : Double;  // Horizontal Dilution of Precision
    VDOP : Double;  // Vertical Dilution of Precision
  end;

  TGGeoPosition = record
    Valid : Boolean;
    Latitude : Double;  // + is North - is South
    Longitude : Double; // + is East - is West
    UTCTime : TDateTime;
  end;

  TGGPSFixData = record
    Valid : Boolean;
    Latitude : Double;  // + is North - is South
    Longitude : Double; // + is East - is West
    UTCTime : TDateTime;
    Quality : integer;  // 0 = Invalid, 1 = GPS Fix, 2 - DGPS fix, 6 = Estimated fix
    SatsInView : integer;
    HDOP : Double;  // Horizontal dilution of Precision
    Altitude : Double;  // Meters above sea level
    GeoidHeight : Double; // Height of Geoid above WGS84
    DGPSAgeInSecs : integer;
    DGPSStationID : integer;
  end;

  TGBearing = record
    Valid : Boolean;
    TrueBearing : Double;   // From Start to Dest
    MagneticBearing : Double;   // From Start to Dest
    DestWaypointID : String;
    StartWaypointID : String;
  end;

  TGBearingDist = record
    Valid : Boolean;
    UTCTime : TDateTime;
    LatitudeNextWP : Double;  // + is North - is South
    LongitudeNextWP : Double; // + is East - is West
    TrueBearing : Double;   // From Start to Dest
    MagneticBearing : Double;   // From Start to Dest
    Distance : Double;
    DistanceUnit : String;  // N = nautical Miles
    WaypointID : String;
  end;

  TGCourseOverGround = record
    Valid : Boolean;
    TrueCourse : Double;
    MagneticCourse : Double;
    Speed : Double;
    SpeedUnits : String;  // N = knots
    Speed2 : Double;
    SpeedUnits2 : String;  // K = Kilometers
    Mode : String;  // optional: A = Autonomous, D=Diferential, E=Estimated, N=Not valid
  end;

  TGWPLocation = record
    Valid : Boolean;
    Latitude : Double;  // + is North - is South
    Longitude : Double; // + is East - is West
    WaypointID : String;
  end;


  TGActiveSatsEvent = procedure( Sender : TObject; const ActiveSats : array of TGActiveSats ) of object;
  TGBearingEvent = procedure( Sender : TObject; const BearingToWP : TGBearing ) of object;
  TGBearingDistEvent = procedure( Sender : TObject; const BearingDistToWP : TGBearingDist ) of object;
  TGCourseOverGroundEvent = procedure( Sender : TObject; const CourseOverGround : TGCourseOverGround ) of object;
  TGGeoPositionEvent = procedure( Sender : TObject; const GeoPosition : TGGeoPosition ) of object;
  TGGPSFixDataEvent = procedure( Sender : TObject; const GPSFixData : TGGPSFixData ) of object;
  TGSentenceEvent = procedure( Sender : TObject; sentence : String ) of object;
  TGWPLocationEvent = procedure( Sender : TObject; const WPLocation : TGWPLocation ) of object;
  TGSatInViewEvent = procedure( Sender : TObject; const SatInView : array of TGSatInView ) of object;

  TGlobeGPS = class(TComponent)
  private
    { Private declarations }
    FMsgQueue : TStringList;
    FPartialMsg : String;
    FOnSatInView: TGSatInViewEvent;
    FOnGPSFixData: TGGPSFixDataEvent;
    FOnGeoPosition: TGGeoPositionEvent;
    FOnUnknownSentence: TGSentenceEvent;
    FOnKnownSentence: TGSentenceEvent;
    FOnWPLocation: TGWPLocationEvent;
    FOnBearingDistToWP: TGBearingDistEvent;
    FOnBearingToWP: TGBearingEvent;
    FOnCourseOverGround: TGCourseOverGroundEvent;
    FOnActiveSats: TGActiveSatsEvent;
    FGPSActivity : TDateTime;
    FCriticalSection : TCriticalSection;
    function GetGPSActive: Boolean;
  protected
    { Protected declarations }
    function ReadLatitude( const lat, ns : String ) : Double;
    function ReadLongitude( const long, ew : String ) : Double;
    function ReadUTCTime( const time : String ) : TDateTime;

    procedure ProcessMsgQueue;

    procedure ProcessActiveSats( sl : TStringList );
    procedure ProcessBearing( sl : TStringList );
    procedure ProcessBearingDist( sl : TStringList );
    procedure ProcessCourseOverGround( sl : TStringList );
    procedure ProcessGeoLatLong( sl : TStringList );
    procedure ProcessGPSFixData( sl : TStringList );
    procedure ProcessSateliteInView( sl : TStringList );
    procedure ProcessWPLocation( sl : TStringList );
  public
    { Public declarations }
    ActiveSats : TGActiveSats;
    BearingToWP : TGBearing;
    BearingDistToWP : TGBearingDist;
    CourseOverGround : TGCourseOverGround;
    GeoPosition : TGGeoPosition;
    GPSFixData : TGGPSFixData;
    SatInView : array of TGSatInView;
    WPLocation : TGWPLocation;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Reset;

    procedure ReceiveData(Buffer: Pointer; BufferLength: Word);

    property GPSActive : Boolean read GetGPSActive;
  published
    { Published declarations }
    property OnTGActiveSats : TGActiveSatsEvent read FOnActiveSats write FOnActiveSats;
    property OnBearingToWP : TGBearingEvent read FOnBearingToWP write FOnBearingToWP;
    property OnBearingDistToWP : TGBearingDistEvent read FOnBearingDistToWP write FOnBearingDistToWP;
    property OnCourseOverGround : TGCourseOverGroundEvent read FOnCourseOverGround write FOnCourseOverGround;
    property OnGeoPosition : TGGeoPositionEvent read FOnGeoPosition write FOnGeoPosition;
    property OnGPSFixData : TGGPSFixDataEvent read FOnGPSFixData write FOnGPSFixData;
    property OnSatInView : TGSatInViewEvent read FOnSatInView write FOnSatInView;
    property OnWPLocation : TGWPLocationEvent read FOnWPLocation write FOnWPLocation;


    property OnKnownSentence : TGSentenceEvent read FOnKnownSentence write FOnKnownSentence;
    property OnUnknownSentence : TGSentenceEvent read FOnUnknownSentence write FOnUnknownSentence;
  end;

procedure Register;

implementation

Uses GSysUtils;

procedure Register;
begin
  RegisterComponents('TGlobe', [TGlobeGPS]);
end;



constructor TGlobeGPS.Create(AOwner: TComponent);
begin
  inherited;

  FMsgQueue := TStringList.Create;
  FCriticalSection := TCriticalSection.Create;

end;

destructor TGlobeGPS.Destroy;
begin
  FMsgQueue.Free;
  FCriticalSection.Free;

  inherited;
end;

function TGlobeGPS.GetGPSActive: Boolean;
begin
  // Returns true if the GPS has done something within the last GPS_INACTIVE_SECS seconds.
  Result := ( Now - FGPSActivity ) < GPS_INACTIVE_SECS / ( 24 * 60 * 60);
end;

procedure TGlobeGPS.ProcessActiveSats(sl: TStringList);
var
  idx : integer;
begin
  ActiveSats.Valid := false;
  try
    ActiveSats.ModeType := sl[1];
    ActiveSats.Mode := sl[2];
    for idx := 0 to 11 do
      ActiveSats.Channel[idx] := sl[3 + idx];
    ActiveSats.PDOP := StrToExtendedDef( sl[15], 0.0 );
    ActiveSats.HDOP := StrToExtendedDef( sl[16], 0.0 );
    ActiveSats.VDOP := StrToExtendedDef( sl[17], 0.0 );

    ActiveSats.Valid := true;
  finally
    if Assigned( FOnActiveSats ) then
      FOnActiveSats( self, ActiveSats );
  end;
end;

procedure TGlobeGPS.ProcessBearing(sl: TStringList);
begin
  BearingToWP.Valid := false;
  try
    BearingToWP.TrueBearing := StrToExtendedDef(sl[1], 0.0);
    BearingToWP.MagneticBearing := StrToExtendedDef(sl[3], 0.0);
    BearingToWP.DestWaypointID := sl[5];
    BearingToWP.StartWaypointID := sl[6];
    BearingToWP.Valid := true;
  finally
    if Assigned( FOnBearingToWP ) then
      FOnBearingToWP( self, BearingToWP );
  end;
end;

procedure TGlobeGPS.ProcessBearingDist(sl: TStringList);
begin
  try
    BearingDistToWP.UTCTime := ReadUTCTime(sl[1]);
    BearingDistToWP.LatitudeNextWP := ReadLatitude(sl[2], sl[3]);
    BearingDistToWP.LongitudeNextWP := ReadLongitude(sl[4], sl[5]);
    BearingDistToWP.TrueBearing := StrToExtendedDef( sl[6], 0.0 );
    BearingDistToWP.MagneticBearing := StrToExtendedDef( sl[8], 0.0 );
    BearingDistToWP.Distance := StrToExtendedDef( sl[10], 0.0 );
    BearingDistToWP.DistanceUnit := sl[11];
    BearingDistToWP.WaypointID := sl[12];
    BearingDistToWP.Valid := true;
  except
    BearingDistToWP.Valid := false;
  end;

  if Assigned( FOnBearingDistToWP ) then
    FOnBearingDistToWP( self, BearingDistToWP );
end;

procedure TGlobeGPS.ProcessCourseOverGround(sl: TStringList);
begin
  CourseOverGround.Valid := false;
  try
    CourseOverGround.TrueCourse := StrToExtendedDef( sl[1], 0.0 );
    CourseOverGround.MagneticCourse := StrToExtendedDef( sl[3], 0.0 );
    CourseOverGround.Speed := StrToExtendedDef( sl[5], 0.0 );
    CourseOverGround.SpeedUnits := sl[6];
    CourseOverGround.Speed2 := StrToExtendedDef( sl[7], 0.0 );
    CourseOverGround.SpeedUnits2 := sl[8];

    CourseOverGround.Valid := true;
  finally
    if Assigned( FOnCourseOverGround ) then
      FOnCourseOverGround( self, CourseOverGround );
  end;
end;

procedure TGlobeGPS.ProcessGeoLatLong( sl : TStringList );
begin
  try
    GeoPosition.Latitude := ReadLatitude( sl[1], sl[2] );
    GeoPosition.Longitude := ReadLongitude( sl[3], sl[4] );
    GeoPosition.UTCTime := ReadUTCTime( sl[5] );
    GeoPosition.Valid := true;
  except
    GeoPosition.Valid := false;
  end;
  
  if Assigned( FOnGeoPosition ) then
    FOnGeoPosition( self, GeoPosition );
end;

procedure TGlobeGPS.ProcessGPSFixData(sl: TStringList);
begin
  try
    GPSFixData.UTCTime := ReadUTCTime( sl[1] );
    GPSFixData.Latitude := ReadLatitude( sl[2], sl[3] );
    GPSFixData.Longitude := ReadLongitude( sl[4], sl[5] );

    GPSFixData.Quality := StrToIntDef( sl[6], 0 );
    GPSFixData.SatsInView := StrToIntDef( sl[7], 0 );
    GPSFixData.HDOP := StrToExtendedDef( sl[8], 0.0 );
    GPSFixData.Altitude := StrToExtendedDef( sl[9], 0.0 );
    GPSFixData.GeoidHeight := StrToExtendedDef( sl[10], 0.0 );
    GPSFixData.DGPSAgeInSecs := StrToIntDef( sl[12], 0 );
    GPSFixData.DGPSStationID := StrToIntDef( sl[12], 0 );

    GPSFixData.Valid := true;
  except
    GPSFixData.Valid := False;  // Indicate
  end;
  if Assigned( FOnGPSFixData ) then
    FOnGPSFixData( self, GPSFixData );
end;

procedure TGlobeGPS.ProcessMsgQueue;
var
  sl : TStringList;
  idx : integer;
  tmp, tid, sid : String; //Sentance ID
begin
  sl := TStringlist.Create;

  for idx := 0 to FMsgQueue.Count -1 do
  try
    sl.CommaText := Trim(FMsgQueue[idx]);  // Break a NMEA sentence into components

    if sl.Count < 1 then
      continue;

    // get the talker ID
    tid := Uppercase( Copy( sl[0], 1, 3 ));

    // tids from : GPS receiver, Loran-C receiver, Omega Navigation receiver, Integrated Instrumentation
    if ( tid = '$GP' ) or ( tid = '$LC' ) or ( tid = '$OM' ) or ( tid = '$II' ) then
    begin
      // strip off the checksum from the last element
      tmp := sl[sl.Count - 1];
      tmp := Copy( tmp, 1, Pos( '*', tmp ) - 1 );
      sl[sl.Count - 1] := tmp;

      // get the sentance ID
      sid := Uppercase( Copy( sl[0], 4, 3 ));

      if sid = 'GSV' then  // Satelite in view
        ProcessSateliteInView( sl )
      else
        if sid = 'GLL' then  // Position Lat Long
          ProcessGeoLatLong( sl )
        else
          if sid = 'GGA' then  // Fix Data
            ProcessGPSFixData( sl )
          else
            if sid = 'BOD' then  // Bearing
              ProcessBearing( sl )
            else
              if sid = 'BWC' then  // Bearing and distance
                ProcessBearingDist( sl )
              else
                if sid = 'WPL' then  // Waypoint Location
                  ProcessWPLocation( sl )
                else
                  if sid = 'VTG' then  // Course over ground
                    ProcessCourseOverGround( sl )
                  else
                    if sid = 'GSA' then  // Active Satelites
                      ProcessActiveSats( sl )
                    else
                    begin // unknown sentence
                      if Assigned( FOnUnknownSentence ) then
                        FOnUnknownSentence( self, FMsgQueue[idx] );
                      continue;
                    end;
    end;

    FGPSActivity := Now;
    if Assigned( FOnKnownSentence ) then
      FOnKnownSentence( self, FMsgQueue[idx] );
  except
    // Just ignore exceptions
    if Assigned( FOnKnownSentence ) then
      FOnKnownSentence( self, 'Error:' + FMsgQueue[idx] );
  end;

  sl.free;

  FMsgQueue.Clear;
end;

procedure TGlobeGPS.ProcessSateliteInView( sl : TStringList );
var
  row, jdx : integer;
  allData : boolean;
begin
  row := StrToIntDef( sl[2], 0 );
  if row = 0 then
    Exit;

  if row = 1 then // first record
    SetLength( SatInView, StrToIntDef( sl[3], 0 ));  // set the number of satelites

  allData := ( row = StrToIntDef( sl[1], 0 )); // last record

  Dec( row ); // zero counting
  // add data into the SatInView array
  for jdx := 0 to 3 do
    if row * 4 + jdx < Length( SatInView ) then
      with SatInView[row * 4 + jdx] do
      begin
        PRN := sl[4 * jdx + 4];
        Elevation := StrToIntDef( sl[4 * jdx + 5],0 );
        Azimuth := StrToIntDef( sl[4 * jdx + 6],0 );
        SNR := StrToIntDef( sl[4 * jdx + 7],0 );
      end;

  if allData then
    if Assigned( FOnSatInView ) then
      FOnSatInView(self, SatInView );
end;

procedure TGlobeGPS.ProcessWPLocation(sl: TStringList);
begin
  try
    WPLocation.Latitude := ReadLatitude( sl[1], sl[2] );
    WPLocation.Longitude := ReadLongitude( sl[3], sl[4] );
    WPLocation.WaypointID := sl[5];
    WPLocation.Valid := true;
  except
    WPLocation.Valid := False;  // Indicate
  end;

  if Assigned( FOnWPLocation ) then
    FOnWPLocation( self, WPLocation );
end;

{ Summary
  Reads a Latitude value from the sentance
  
  Description
  Converts a latitude value in the form ddmm.ttt to decimal
  degrees
  
  Returns
  Latitude in decimal degrees or raises <LINK EGPSException> if
  invalid
                                                                }
function TGlobeGPS.ReadLatitude(const lat, ns: String): Double;
var
  dpPos : integer;
begin
  if Length( lat ) < 8 then
    raise EGPSException.Create( 'Invalid Latitude' );

  dpPos := Pos( '.', lat );
  if dpPos < 3 then
    raise EGPSException.Create( 'Invalid Latitude' );

  // Converts ddmm.mmm to Decimal Degrees
  Result := StrToExtendedDef( Copy( lat, 1, dpPos - 3 ), 0);
  Result := Result + GPS_ONE_MINUTE * StrToExtendedDef( Copy( lat, dpPos - 2, 10 ), 0);

  if ns = 'S' then
    Result := -Result;
end;

function TGlobeGPS.ReadLongitude(const long, ew: String): Double;
var
  dpPos : integer;
begin
  if Length( long ) < 9 then
    raise EGPSException.Create( 'Invalid Longitude' );

  dpPos := Pos( '.', long );
  if dpPos < 3 then
    raise EGPSException.Create( 'Invalid Longitude' );

  // Converts dddmm.mmm to Decimal Degrees
  Result := StrToExtendedDef( Copy( long, 1, dpPos - 3 ),0);
  Result := Result + GPS_ONE_MINUTE * StrToExtendedDef( Copy( long, dpPos - 2, 10 ),0);

  if ew = 'W' then
    Result := -Result;
end;

function TGlobeGPS.ReadUTCTime(const time: String): TDateTime;
var
  dp, ms : integer;
begin
  Result := 0.0;
  dp := Pos( '.', time );
  if dp > 0 then
    ms := StrToIntDef( Copy( time, dp + 1, 3 ), 0)
  else
    ms := 0;

  if Length(time) >= 6 then
  begin
    Result := EncodeTime(
      StrToIntDef( Copy( time, 1, 2 ), 0),
      StrToIntDef( Copy( time, 3, 2 ), 0),
      StrToIntDef( Copy( time, 5, 2 ), 0),
      ms );
  end;
end;

procedure TGlobeGPS.ReceiveData(Buffer: Pointer; BufferLength: Word);
var
  sl : TStringList;
  tmp : String;
  sol, eol : integer;
begin
  FCriticalSection.Acquire;
  FPartialMsg := FPartialMsg + Copy( PChar(Buffer ), 0, BufferLength);

  sol := Pos( '$', FPartialMsg );
  eol := LastDelimiter(#10#13, FPartialMsg );
  if ( sol > 0 ) and ( eol > 0 ) then
  begin
    tmp := Copy( FPartialMsg, sol, eol - sol );
    FPartialMsg := Copy( FPartialMsg, eol + 1, Length(FPartialMsg));

    sl := TStringlist.Create;
    sl.Text := tmp;

    FMsgQueue.AddStrings(sl);

    sl.Free;

    ProcessMsgQueue;
  end;
  FCriticalSection.Release;
end;

procedure TGlobeGPS.Reset;
begin
  FGPSActivity := 0.0;

  FMsgQueue.Clear;
  FPartialMsg := '';
  
  ActiveSats.Valid := false;
  BearingToWP.Valid := false;
  BearingDistToWP.Valid := false;
  CourseOverGround.Valid := false;
  GeoPosition.Valid := false;
  GPSFixData.Valid := false;
  WPLocation.Valid := false;
end;

end.

