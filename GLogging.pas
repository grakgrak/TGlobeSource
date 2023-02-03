{-------------------------------------------------------------------------
 Module:    Globe Logging classes

 Comment:   Scans command line for:
            -Name:Debug=255
            -Name:Trace=255
            -Name:Error=255
            Where Name is the name of the component and 255 is the value of the
            specified flag set

 Classes:   TGCustomLog
            TGDebugLog
            TGFileLog
            TGLogManager
            TGUserLog

 Author:    Graham Knight
 Email:     tglobe@tglobe.com
-------------------------------------------------------------------------}
unit GLogging;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Contnrs;

type
  TGLogType = ( gltDebug, gltTrace, gltError );
  TGLogLevel = ( gllOne, gllTwo, gllThree, gllFour, gllFive, gllSix, gllSeven, gllEight );
  TGLogLevelSet = set of TGLogLevel;

  TGLogOption = ( gloDate, gloTime, gloTimeMS, gloType, gloLevel );
  TGLogOptionSet = set of TGLogOption;

  //----------------------------------------------------------------------------

  TGCustomLog = class( TComponent )
  private
    { Private declarations }
    FActive: Boolean;
    FOptions : TGLogOptionSet; 
    FTraceLevel: TGLogLevelSet;
    FErrorLevel: TGLogLevelSet;
    FDebugLevel: TGLogLevelSet;
  protected
    { Protected declarations }
    procedure ScanCommandLine; virtual;
    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;

    procedure Log( const logMessage : String ); virtual; abstract;
  published
    { Published declarations }
    property Active : Boolean read FActive write FActive;

    property Options : TGLogOptionSet read FOptions write FOptions;

    property DebugLevel : TGLogLevelSet read FDebugLevel write FDebugLevel;
    property TraceLevel : TGLogLevelSet read FTraceLevel write FTraceLevel;
  end;

  //----------------------------------------------------------------------------
  TGFileLog = class( TGCustomLog )
  private
    { Private declarations }
    FLogFilename: TFilename;
    procedure SetLogFilename(const Value: TFilename);
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Log( const logMessage : String ); override;
  published
    { Published declarations }
    property LogFilename : TFilename read FLogFilename write SetLogFilename;
  end;

  //----------------------------------------------------------------------------
  TGDebugLog = class( TGCustomLog )
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Log( const logMessage : String ); override;
  end;

  //----------------------------------------------------------------------------
  TGUserLog = class( TGCustomLog )
  private
    { Private declarations }
    FLogMessage: String;
    FOnLog: TNotifyEvent;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Log( const logMessage : String ); override;
  published
    property OnLog : TNotifyEvent read FOnLog write FOnLog;
    property LogMessage : String read FLogMessage;
  end;

  //----------------------------------------------------------------------------
  TGLogManager = class
  private
    { Private declarations }
    FLoggers: TObjectList;

    function DoLog( logType : TGLogType; level : TGLogLevel; const logMessage : String ) : Boolean;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;

    function LogDebug( level : TGLogLevel; const logMessage : String ) : Boolean;
    function LogTrace( level : TGLogLevel; const logMessage : String ) : Boolean;
    function LogError( const logMessage : String ) : Boolean;

    procedure RegisterLogger( logger : TGCustomLog );
    procedure UnRegisterLogger( logger : TGCustomLog );
  end;


function TGLog : TGLogManager;

procedure Register;

implementation
var
  GLogger : TGLogManager;

function TGLog : TGLogManager;
begin
  if GLogger = nil then
    GLogger := TGLogManager.Create;
  Result := GLogger;
end;

procedure Register;
begin
//  RegisterComponents('TGlobe', [TGFileLog, TGDebugLog, TGUserLog]);
end;

{ TGLogManager }

constructor TGLogManager.Create;
begin
  FLoggers := TObjectList.Create( False );
end;

destructor TGLogManager.Destroy;
begin
  FreeAndNil( FLoggers );

  inherited;
end;

function TGLogManager.DoLog(logType: TGLogType; level: TGLogLevel; const logMessage: String) : Boolean;
var
  prefix, logTypeStr, dateTimeStr : String;
  idx : integer;
  levelnum : integer;
  logger : TGCustomLog;
begin
  for idx := 0 to FLoggers.Count - 1 do
  begin
    logger := TGCustomLog( FLoggers[idx] );

    if logger.Active then
    begin
      logTypeStr := '';
      case logType of
        gltDebug:
          if level in logger.DebugLevel then
            logTypeStr := 'Debug';
        gltTrace:
          if level in logger.TraceLevel then
            logTypeStr := 'Trace';
        gltError:
          logTypeStr := 'Error';
      end;

      if logTypeStr <> '' then
      begin
        dateTimeStr := '';
        if gloDate in logger.Options then
          dateTimeStr := dateTimeStr + 'yyyy-mm-dd ';
        if gloTime in logger.Options then
          dateTimeStr := dateTimeStr + 'hh:nn:ss';
        if gloTimeMS in logger.Options then
          dateTimeStr := dateTimeStr + '.zzz';

        if dateTimestr <> '' then
          prefix := FormatDateTime( dateTimeStr + '  ', Now );

        if gloType in logger.Options then
          prefix := prefix + logTypeStr;

        if gloLevel in logger.Options then
        begin
          case level of
            gllOne: levelnum := 1;
            gllTwo: levelnum := 2;
            gllThree: levelnum := 3;
            gllFour: levelnum := 4;
            gllFive: levelnum := 5;
            gllSix: levelnum := 6;
            gllSeven: levelnum := 7;
            gllEight: levelnum := 8;
            else
              levelnum := 0;
          end;
          prefix := prefix + '[' + IntToStr( levelNum ) + '] ';
        end;

        logger.Log( prefix + logMessage );
      end;
    end;
  end;

  Result := True;
end;

function TGLogManager.LogDebug(level: TGLogLevel; const logMessage: String) : Boolean;
begin
  Result := DoLog( gltDebug, level, logMessage );
end;

function TGLogManager.LogError( const logMessage: String) : Boolean;
begin
  Result := DoLog( gltError, gllOne, logMessage );
end;

function TGLogManager.LogTrace(level: TGLogLevel; const logMessage: String) : Boolean;
begin
  Result := DoLog( gltTrace, level, logMessage );
end;

procedure TGLogManager.RegisterLogger(logger: TGCustomLog);
begin
  if FLoggers.IndexOf(logger) = -1 then
    FLoggers.Add( logger );
end;

procedure TGLogManager.UnRegisterLogger(logger: TGCustomLog);
var
  idx : integer;
begin
  idx := FLoggers.IndexOf( logger );
  if idx >= 0 then
    Floggers.Remove( logger ); 
end;

{ TGFileLog }

procedure TGFileLog.Log(const logMessage: String);
var
  FText : TextFile;
begin
  if Active and ( LogFilename <> '' ) then
  try
    AssignFile(FText, LogFilename);
    if FileExists( LogFilename ) then
      Append( FText )
    else
      ReWrite( FText );
    Write( FText, logMessage + #13#10 );

    CloseFile( FText );
  except
    Active := false;
  end;
end;

procedure TGFileLog.SetLogFilename(const Value: TFilename);
begin
  FLogFilename := Value;
end;


{ TGCustomLog }

constructor TGCustomLog.Create(AOwner: TComponent);
begin
  inherited;
  TGLog.RegisterLogger( self );

  FOptions := [gloDate, gloTime, gloTimeMS, gloType, gloLevel];
  FDebugLevel := [gllOne, gllTwo, gllThree, gllFour, gllFive, gllSix, gllSeven, gllEight];
  FErrorLevel := [gllOne, gllTwo, gllThree, gllFour, gllFive, gllSix, gllSeven, gllEight];
  FTraceLevel := [gllOne, gllTwo, gllThree, gllFour, gllFive, gllSix, gllSeven, gllEight];

  FActive := True;
end;

destructor TGCustomLog.Destroy;
begin
  inherited;

  TGLog.UnRegisterLogger( self );
end;

procedure TGCustomLog.Loaded;
begin
  inherited;
  ScanCommandLine;
end;

procedure TGCustomLog.ScanCommandLine;
var
  idx : integer;
  flags : Byte;
  cmd : String;
begin
  // look for parameters for this component
  for idx := 1 to ParamCount do
    if Pos( '-' + Uppercase( Name ) + ':', Uppercase( ParamStr(idx))) = 1 then
    begin
      cmd := Uppercase( Copy( ParamStr(idx), Length( Name ) + 3, Length( ParamStr(idx))));

      if Pos( 'DEBUG=', cmd ) = 1 then
      begin
        flags := StrToIntDef( Copy( cmd, 7, Length( cmd )), Byte( FDebugLevel ));
        FDebugLevel := TGLogLevelSet( flags );
        FActive := True;
      end;

      if Pos( 'ERROR=', cmd ) = 1 then
      begin
        flags := StrToIntDef( Copy( cmd, 7, Length( cmd )), Byte( FErrorLevel ));
        FErrorLevel := TGLogLevelSet( flags );
        FActive := True;
      end;

      if Pos( 'TRACE=', cmd ) = 1 then
      begin
        flags := StrToIntDef( Copy( cmd, 7, Length( cmd )), Byte( FTraceLevel ));
        FTraceLevel := TGLogLevelSet( flags );
        FActive := True;
      end;
    end;
end;

{ TGDebugLog }


procedure TGDebugLog.Log(const logMessage: String);
begin
  OutputDebugString( PChar( logMessage ));
end;

{ TGUserLog }

procedure TGUserLog.Log(const logMessage: String);
begin
  if Active and Assigned( FOnLog ) then
  begin
    FLogMessage := logMessage;
    FOnLog( self );
  end;
end;

initialization
  GLogger := nil;
finalization
  FreeAndNil( GLogger );
end.
