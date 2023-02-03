//-----------------------------------------------------------------------
// Summary
//	TGlobe Morphing base classes
//
// Description
// 	Provides morphing routines for image georeferencing
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
{$I GLOBE5.INC}
unit GMorphing;

interface

Uses Windows, Messages, SysUtils, Classes, GSysUtils, GXML, GBitmap;

type
  TGMorph = class( TGRoot )
  private
    FOnChanged: TNotifyEvent;
    procedure SetOnChanged(const Value: TNotifyEvent);
  protected
    procedure DoChanged;
  public
    procedure WriteToStream( stream : TStream ); virtual; abstract;
    procedure ReadFromStream( stream : TStream ); virtual; abstract;

    procedure LoadFromXML( Element : TGXML_Element ); virtual; abstract;
    procedure SaveToXML( Element : TGXML_Element ); virtual; abstract;

    function WorldToXY( ptLL : TGPointLL ): TPoint; virtual; abstract;
    function XYToWorld( X, Y : Double ): TGPointLL; overload; virtual; abstract;
    function XYToWorld( pt : TPoint ): TGPointLL; overload; virtual;

    property OnChanged : TNotifyEvent read FOnChanged write SetOnChanged;
  end;


  TGLinearMorph = class( TGMorph )
  private
    FOffsetY: integer;
    FOffsetX: integer;
    FScaleY: Double;
    FScaleX: Double;
    procedure SetOffsetX(const Value: integer);
    procedure SetOffsetY(const Value: integer);
    procedure SetScaleX(const Value: Double);
    procedure SetScaleY(const Value: Double);
  public
    constructor Create;

    procedure WriteToStream( stream : TStream ); override;
    procedure ReadFromStream( stream : TStream ); override;

    procedure LoadFromXML( Element : TGXML_Element ); override;
    procedure SaveToXML( Element : TGXML_Element ); override;

    function WorldToXY( ptLL : TGPointLL ): TPoint; override;
    function XYToWorld( X, Y : Double ): TGPointLL; override;

    procedure ScaleAbout( sfX, sfY, X, Y : Double );
    procedure SetXYOffsets( X, Y : integer ); virtual;
    procedure SetXYScaleFactor( sfX, sfY : Double ); virtual;
    procedure Translate( dX, dY : integer );

    property OffsetX : integer read FOffsetX write SetOffsetX;
    property OffsetY : integer read FOffsetY write SetOffsetY;
    property ScaleX : Double read FScaleX write SetScaleX;
    property ScaleY : Double read FScaleY write SetScaleY;
  end;


  TGControlPair = record
    ImageStart : TPoint;
    ImageEnd : TPoint;
    WorldStart : TGPointLL;
    WorldEnd : TGPointLL;
  end;
  TGPairArray = array of TGControlPair;


  TGBeierNeelyMorph = class( TGLinearMorph )
  private
    FControlPairs : TGPairArray;
    a, b : Double;
  public
    constructor Create;

    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function Count : integer;
    function Add : integer;
    procedure Delete( index : integer );

    procedure WriteToStream( stream : TStream ); override;
    procedure ReadFromStream( stream : TStream ); override;

    procedure LoadFromXML( Element : TGXML_Element ); override;
    procedure SaveToXML( Element : TGXML_Element ); override;

    function XYToWorld( X, Y : Double ): TGPointLL; override;

    procedure SetXYOffsets( X, Y : integer ); override;

    property ControlPairs : TGPairArray read FControlPairs;
  end;

implementation



procedure TGMorph.DoChanged;
begin
  if Assigned( FOnChanged ) then
    FOnChanged( self );
end;

procedure TGMorph.SetOnChanged(const Value: TNotifyEvent);
begin
  FOnChanged := Value;
end;

function TGMorph.XYToWorld( pt : TPoint ): TGPointLL;
begin
  Result := XYToWorld( pt.X, pt.Y );
end;


constructor TGLinearMorph.Create;
begin
  inherited;

  FScaleX := 1.0;
  FScaleY := 1.0;
  FOffsetX := 0;
  FOffsetY := 0;
end;


procedure TGLinearMorph.ScaleAbout(sfX, sfY, X, Y: Double);
var
  tmp : Double;
begin
  if ( sfX < 0.01 ) or ( sfX > 1000.0 ) then
    Exit;

  tmp := ( OffsetX - X ) / FScaleX;
  FOffsetX := Round( X + tmp * sfX );

  tmp := ( OffsetY - Y ) / FScaleY;
  FOffsetY := Round( Y + tmp * sfY );

  FScaleX := sfX;
  FScaleY := sfY;

  DoChanged;
end;

procedure TGLinearMorph.SetOffsetX(const Value: integer);
begin
  SetXYOffsets( Value, OffsetY );
end;

procedure TGLinearMorph.SetOffsetY(const Value: integer);
begin
  SetXYOffsets( OffsetX, Value );
end;

procedure TGLinearMorph.SetScaleX(const Value: Double);
begin
  SetXYScaleFactor(Value, FScaleY);
end;

procedure TGLinearMorph.SetScaleY(const Value: Double);
begin
  SetXYScaleFactor(FScaleX, Value);
end;

procedure TGLinearMorph.SetXYOffsets(X, Y: integer);
begin
  FOffsetX := X;
  FOffsetY := Y;
  DoChanged;
end;

procedure TGLinearMorph.SetXYScaleFactor(sfX, sfY: Double);
begin
  FScaleX := sfX;
  FScaleY := sfY;
  DoChanged;
end;

procedure TGLinearMorph.Translate(dX, dY: integer);
begin
  SetXYOffsets( FOffsetX + dX, FOffsetY + dY );
end;

function TGLinearMorph.XYToWorld( X, Y : Double ): TGPointLL;
begin
  Result.iLongX := OffsetX + Round(X * FScaleX);
  Result.iLatY := OffsetY + Round(Y * FScaleY);
  Result.iHeightZ := 0;
end;

function TGLinearMorph.WorldToXY(ptLL: TGPointLL): TPoint;
begin
  Result.X := Round( ( ptLL.iLongX - OffsetX ) / FScaleX );
  Result.Y := Round( ( ptLL.iLatY - OffsetY ) / FScaleY );
end;

procedure TGLinearMorph.ReadFromStream(stream: TStream);
begin
  inherited;

  with TGStreamReader.Create(stream) do
  begin
    OffsetX := ReadInteger;
    OffsetY := ReadInteger;
    ScaleX := ReadDouble;
    ScaleY := ReadDouble;

    Free;
  end;
end;

procedure TGLinearMorph.WriteToStream(stream: TStream);
begin
  inherited;

  with TGStreamWriter.Create(stream) do
  begin
    WriteInteger( OffsetX );
    WriteInteger( OffsetY );
    WriteDouble( ScaleX );
    WriteDouble( ScaleY );
    Free;
  end;
end;

procedure TGLinearMorph.LoadFromXML(Element: TGXML_Element);
begin
  if Element <> nil then
  begin
    OffsetX := Element.Attribute( 'OffsetX', 0 );
    OffsetY := Element.Attribute( 'OffsetY', 0 );
    ScaleX := Element.Attribute( 'ScaleX', 1.0 );
    ScaleY := Element.Attribute( 'ScaleY', 1.0 );
  end;
end;

procedure TGLinearMorph.SaveToXML(Element: TGXML_Element);
begin
  Element.AddAttribute( 'OffsetX', OffsetX, 0 );
  Element.AddAttribute( 'OffsetY', OffsetY, 0 );
  Element.AddAttribute( 'ScaleX', ScaleX, 1.0 );
  Element.AddAttribute( 'ScaleY', ScaleY, 1.0 );
end;




function TGBeierNeelyMorph.Add : integer;
begin
  Result := Length( FControlPairs );
  SetLength( FControlPairs, Result + 1 );
end;


procedure TGBeierNeelyMorph.Assign(Source: TPersistent);
var
  doc : TGXML_Document;
begin
  if Source is TGBeierNeelyMorph then
  begin
    doc := TGXML_Document.Create('Morph');

    ( Source as TGBeierNeelyMorph ).SaveToXML(doc.Document);
    LoadFromXML(doc.Document);

    doc.Free;
  end
  else
    inherited;
end;


procedure TGBeierNeelyMorph.Clear;
begin
  SetLength( FControlPairs, 0 );
  DoChanged;
end;


function TGBeierNeelyMorph.Count: integer;
begin
  Result := Length( FControlPairs );
end;

constructor TGBeierNeelyMorph.Create;
begin
  inherited;

  a := 0.2;
  b := 1.5;
//  b := 0.0;
  FScaleX := 1.0;
  FScaleY := 1.0;
end;


procedure TGBeierNeelyMorph.Delete(index: integer);
begin
  if ( index >= 0 ) and ( index < Count ) then
  begin
    if index < High( FControlPairs ) then
      Move( FControlPairs[index + 1], FControlPairs[index], ( Count - index ) * SizeOf(TGControlPair));
    SetLength( FControlPairs, High( FControlPairs ));
  end;
end;


procedure TGBeierNeelyMorph.LoadFromXML(Element: TGXML_Element);
var
  idx : integer;
begin
  inherited;

  Clear;
  if Element <> nil then
    for idx := 0 to Element.ElementCount( 'Ctrl' ) - 1 do
      with Element.Element( 'Ctrl', idx ) do
      begin
        Self.Add;  // Add a control Pair

        ControlPairs[idx].ImageStart := StrToPoint( Attribute( 'ImageStart', '' ));
        ControlPairs[idx].ImageEnd := StrToPoint( Attribute( 'ImageEnd', '' ));
        ControlPairs[idx].WorldStart := StrToPointLL( Attribute( 'WorldStart', '' ));
        ControlPairs[idx].WorldEnd := StrToPointLL( Attribute( 'WorldEnd', '' ));
      end;
  DoChanged;
end;


procedure TGBeierNeelyMorph.ReadFromStream(stream: TStream);
var
  idx : integer;
begin
  inherited;

  with TGStreamReader.Create(stream) do
  begin
    SetLength(FControlPairs, ReadInteger );

    for idx := 0 to High( FControlPairs ) do
      with FControlPairs[idx] do
      begin
        ImageStart := ReadPoint;
        ImageEnd := ReadPoint;
        WorldStart := ReadPointLL(false, false);
        WorldEnd := ReadPointLL(false, false);
      end;

    Free;
  end;
end;


procedure TGBeierNeelyMorph.SaveToXML(Element: TGXML_Element);
var
  idx : integer;
begin
  inherited;

  for idx := 0 to High( ControlPairs ) do
    with Element.AddElement( 'Ctrl' ) do
    begin
      AddAttribute( 'ImageStart', PointToStr( ControlPairs[idx].ImageStart ), '');
      AddAttribute( 'ImageEnd', PointToStr( ControlPairs[idx].ImageEnd ), '');
      AddAttribute( 'WorldStart', PointLLToStr( ControlPairs[idx].WorldStart ), '');
      AddAttribute( 'WorldEnd', PointLLToStr( ControlPairs[idx].WorldEnd ), '');
    end;
end;

procedure TGBeierNeelyMorph.SetXYOffsets(X, Y: integer);
var
  idx, dx, dy : integer;
begin
  dx := Round( X - FOffsetX );
  dy := Round( Y - FOffsetY );

  if ( dy <> 0 ) or ( dx <> 0 ) then
  begin
    for idx := 0 to High( FControlPairs ) do
      with FControlPairs[idx] do
      begin
        WorldStart.iLongX := WorldStart.iLongX + dx;
        WorldStart.iLatY := WorldStart.iLatY + dy;

        WorldEnd.iLongX := WorldEnd.iLongX + dx;
        WorldEnd.iLatY := WorldEnd.iLatY + dy;
      end;

    FOffsetX := X;
    FOffsetY := Y;
    DoChanged;
  end;
end;



procedure TGBeierNeelyMorph.WriteToStream(stream: TStream);
var
  idx : integer;
begin
  inherited;

  with TGStreamWriter.Create(stream) do
  begin
    WriteInteger( Length( FControlPairs ));

    for idx := 0 to High( FControlPairs ) do
      with FControlPairs[idx] do
      begin
        WritePoint(ImageStart);
        WritePoint(ImageEnd);
        WritePointLL(WorldStart, false, false);
        WritePointLL(WorldEnd, false, false);
      end;

    Free;
  end;
end;


function TGBeierNeelyMorph.XYToWorld( X, Y : Double ) : TGPointLL;
var
  idx : integer;
  vQ, vP, vX, vD, vDSum : TGVec;
  u, v, weight, weightsum, dist : Double;
  res : array of TGVec;
  vXP, vQP : TGVec;
begin
  vX.X := X * ScaleX + OffsetX;
  vX.Y := Y * ScaleY + OffsetY;

  if Count > 0 then
  begin
    SetLength( res, Count );

    vDSum := Vec_FromXY( 0, 0 );
    weightsum := 0;
    for idx := 0 to Count - 1 do
    begin
      //The Q-P stuff can be pre-calculated for the source and destination lines
      with FControlPairs[idx].ImageStart do
        vQ := Vec_FromXY( X * ScaleX + OffsetX, Y * ScaleY + OffsetY );
      with FControlPairs[idx].ImageEnd do
        vP := Vec_FromXY( X * ScaleX + OffsetX, Y * ScaleY + OffsetY );
      vXP := Vec_Minus(vX, vP);
      vQP := Vec_Minus(vQ, vP);
      dist := Vec_Length(vQP);
      if dist = 0.0 then
        continue;

      u := Vec_dot(vXP,vQP) / Sqr(dist);
      v := Vec_dot(vXP,Vec_Perpendicular(vQP)) / dist;

      vQ := Vec_FromPointLL( FControlPairs[idx].WorldStart );
      vP := Vec_FromPointLL( FControlPairs[idx].WorldEnd );
      vQP := Vec_Minus(vQ, vP);
      dist := Vec_Length(vQP);
      if dist = 0 then
        continue;

      res[idx] := Vec_Plus( Vec_Plus( vP, Vec_Scale(vQP,u)), Vec_Scale(Vec_Scale(Vec_Perpendicular(vQP),v), 1.0 / dist));

    	if u >= 1.0 then
  		  dist := Vec_Length(Vec_Minus(vQ, res[idx]))
  		else
        if u <= 0.0 then
    		  dist := Vec_Length(Vec_Minus(vP, res[idx]))
    		else
		      dist := Abs(v);

  	  weight := Power( Vec_Length(vQP), 0.5 ) / (a + dist);
//  	  weight := Power( QP.Length(), 1.0 ) / (a + dist);
//  	  weight := vQP.Length() / (a + dist);
//  	  weight := Power( vQP.Length(), 1.0 ) / (a + dist);
	  	weight := Power(weight, b);

      vD := Vec_Minus(res[idx], vX);
  		vDSum := Vec_Plus( vDSum, Vec_Scale( vD, weight ));
	  	weightsum := weightsum + weight;
    end;

    if weightsum > 0.0 then
      vX := Vec_Plus(vX, Vec_Scale( vDSum, 1.0 / weightsum ));
  end;

  Result.iLongX := Trunc( vX.X );
  Result.iLatY := Trunc( vX.Y );
  Result.iHeightZ := 0;
end;



end.
