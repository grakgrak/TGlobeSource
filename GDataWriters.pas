//-----------------------------------------------------------------------
// Summary
//	TGlobe base classes for data writers
//
// Description
// 	Data Writers output data to a data source e.g. .LR5 files
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
unit GDataWriters;

interface

Uses Forms, Classes, SysUtils, GClasses, GSysUtils;

type
  
  //-----------------------------------------------------------------------
  //Description
  //This is a base class for objects that can write a layer data held in an
  //ObjectSource to another file format. e.g. TGlobe .LR5 format.          
  //-----------------------------------------------------------------------
  TGDataWriter = class(TGComponent)
  private
    FLayer: TGLayer;
    FProgressTime : TDateTime;

    FOnProgress: TGProgressEvent;
  protected
    //---------------------------------------------
    //Description
    //Actually opens the output file or connection.
    //---------------------------------------------
    function InternalOpen : Boolean; virtual; abstract;
    //------------------------------------------
    //Description
    //Writes the header to the data destination.
    //------------------------------------------
    procedure WriteHeader; virtual; abstract;
    //------------------------------------------------
    //Description
    //Writes the map objects to the data destination. 
    //------------------------------------------------
    procedure WriteBody; virtual; abstract;
    //-------------------------------------------
    //Description
    //Writes the trailer to the data destination.
    //-------------------------------------------
    procedure WriteTrailer; virtual; abstract;
    //----------------------------------------------
    //Description
    //Actually closes the output file or connection.
    //----------------------------------------------
    procedure InternalClose; virtual; abstract;

    procedure DoProgress( count : integer; percentage : double; force : Boolean );
  public
    function Run : Boolean; virtual;

    //--------------------------------------
    //Description
    //Source layer to write map object from.
    //--------------------------------------
    property Layer : TGLayer read FLayer write FLayer;
  published
    //-----------------------------------------------------------------------------
    //Description
    //Is raised periodically to indicate the progress of writing map objects to the
    //data destination.                                                            
    //-----------------------------------------------------------------------------
    property OnProgress : TGProgressEvent read FOnProgress write FOnProgress;
  end;

  
  //---------------------------------------------------------
  //Description
  //Base class for layer export classes that write to a file.
  //---------------------------------------------------------
  TGFileDataWriter = class(TGDataWriter)
  private
    FFileName : TFileName;
    procedure SetFileName(const Value: TFileName);
  public
    //---------------------------------
    //Description
    //The name of the file to write to.
    //---------------------------------
    property FileName : TFileName read FFileName write SetFileName;
  end;

implementation

const
 ONE_SECOND = 1.0 / ( 24.0 * 60.0 * 60.0 );



procedure TGDataWriter.DoProgress(count: integer; percentage: double; force : Boolean);
begin
  if Assigned( FOnProgress ) then
    if force or ( Now > FProgressTime ) then    // Send a progress message no more than once per second
    begin
      FOnProgress( Self, pExporting, count, percentage );
      FProgressTime := Now + ONE_SECOND;
    end;
end;

//------------------------------------------------------------
//Description
//Calls all of the writer sections in turn to output the data.
//------------------------------------------------------------
function TGDataWriter.Run: Boolean;
begin
  if FLayer = nil then
    Raise EGException.Create( 'Run: Layer not assigned');

  if Assigned( FOnProgress ) then
    FOnProgress( Self, pProgressStart, 0, 0.0 );

  Result := InternalOpen;
  if Result then
  begin
    WriteHeader;
    WriteBody;
    WriteTrailer;
    InternalClose;
  end;

  if Assigned( FOnProgress ) then
    FOnProgress( Self, pProgressEnd, 0, 100.0 );
end;

{ TGFileDataWriter }

procedure TGFileDataWriter.SetFileName(const Value: TFileName);
begin
  FFilename := Value;
end;

end.
