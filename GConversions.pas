// Summary
//	Geographic coordinate conversion Classes
//
// Description
// 	TGCoordConversion
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//
{$I GLOBE5.INC}
unit GConversions;

interface

uses GSysUtils;

type
  TGCoordConversion = class( TGRoot )
  private
    FScaleFactor: Extended;
    FLatitudeShift: TGDecimalDegree;
    FFirstParallel: TGDecimalDegree;
    FLongitudeShift: TGDecimalDegree;
    FSecondParallel: TGDecimalDegree;
    FSpheroid: TGSpheroid;
    FInputUnits: TGUnitTypes;
    FModified: Boolean;
    FCentralMeridian: TGDecimalDegree;

    procedure SetFirstParallel(const Value: TGDecimalDegree);
    procedure SetInputUnits(const Value: TGUnitTypes);
    procedure SetLatitudeShift(const Value: TGDecimalDegree);
    procedure SetLongitudeShift(const Value: TGDecimalDegree);
    procedure SetScaleFactor(const Value: Extended);
    procedure SetSecondParallel(const Value: TGDecimalDegree);
    procedure SetSpheroid(const Value: TGSpheroid);
    procedure SetCentralMeridian(const Value: TGDecimalDegree);
  protected
    FEquitorialRadius, FEccentricity, FEccentricitySquared : Extended;
  public
    constructor Create; virtual;

    procedure ReadE00_PARAMS( const params : array of string ); virtual;

    function ToPointLL( long, lat : Extended ) : TGPointLL; virtual;

    property CentralMeridian : TGDecimalDegree read FCentralMeridian write SetCentralMeridian;
    property LongitudeShift : TGDecimalDegree read FLongitudeShift write SetLongitudeShift;
    property LatitudeShift : TGDecimalDegree read FLatitudeShift write SetLatitudeShift;
    property FirstParallel : TGDecimalDegree read FFirstParallel write SetFirstParallel;
    property SecondParallel : TGDecimalDegree read FSecondParallel write SetSecondParallel;
    property ScaleFactor : Extended read FScaleFactor write SetScaleFactor;
    property InputUnits : TGUnitTypes read FInputUnits write SetInputUnits;
    property Spheroid : TGSpheroid read FSpheroid write SetSpheroid;
    property Modified : Boolean read FModified write FModified;
  end;

  TGPolarCoordConversion = class( TGCoordConversion )
  private
    FPhi1, FLambda0, FX1, FM1 : Extended;
  protected
    procedure Initialise;
  public
    procedure ReadE00_PARAMS( const params : array of string ); override;

    function ToPointLL( long, lat : Extended ) : TGPointLL; override;
  end;

  TGLambertCoordConversion = class( TGCoordConversion )
  private
    FPhi0, FPhi1, FPhi2, FLambda0, FN, FF, FRho0 : Extended;
  protected
    procedure Initialise;
  public
    procedure ReadE00_PARAMS( const params : array of string ); override;

    function ToPointLL( long, lat : Extended ) : TGPointLL; override;
  end;

  TGTransverseCoordConversion = class( TGCoordConversion )
  protected
    procedure Initialise;
  public
    procedure ReadE00_PARAMS( const params : array of string ); override;

    function ToPointLL( long, lat : Extended ) : TGPointLL; override;
  end;

implementation

uses SysUtils, Classes;

{ TGCoordConversion }

constructor TGCoordConversion.Create;
begin
  ScaleFactor := 1.0;
  InputUnits := guDegree;
  Spheroid := WGS84;
end;

procedure TGCoordConversion.ReadE00_PARAMS(const params: array of string);
begin
  // Does nothing.
end;

procedure TGCoordConversion.SetCentralMeridian(
  const Value: TGDecimalDegree);
begin
  FCentralMeridian := Value;
  Modified := true;
end;

procedure TGCoordConversion.SetFirstParallel(const Value: TGDecimalDegree);
begin
  FFirstParallel := Value;
  Modified := true;
end;

procedure TGCoordConversion.SetInputUnits(const Value: TGUnitTypes);
begin
  FInputUnits := Value;
  Modified := true;
end;

procedure TGCoordConversion.SetLatitudeShift(const Value: TGDecimalDegree);
begin
  FLatitudeShift := Value;
  Modified := true;
end;

procedure TGCoordConversion.SetLongitudeShift( const Value: TGDecimalDegree);
begin
  FLongitudeShift := Value;
  Modified := true;
end;

procedure TGCoordConversion.SetScaleFactor(const Value: Extended);
begin
  FScaleFactor := Value;
  Modified := true;
end;

procedure TGCoordConversion.SetSecondParallel( const Value: TGDecimalDegree);
begin
  FSecondParallel := Value;
  Modified := true;
end;

procedure TGCoordConversion.SetSpheroid(const Value: TGSpheroid);
begin
  FSpheroid := Value;

  FEquitorialRadius := SpheroidData[Ord(Value)].r * GU_TORADIANS;
  FEccentricitySquared := 2 * SpheroidData[Ord(Spheroid)].f - Sqr(SpheroidData[Ord(Spheroid)].f);
  FEccentricity := Sqrt(FEccentricitySquared);

  Modified := true;
end;

function TGCoordConversion.ToPointLL(long, lat: Extended): TGPointLL;
begin
  Result.iLongX := GUFrom(long * ScaleFactor, InputUnits) + Round( LongitudeShift * GU_DEGREE );
  Result.iLatY := GUFrom(lat * ScaleFactor, InputUnits) + Round( LatitudeShift * GU_DEGREE );
  Result.iHeightZ := 0;
end;

{ TGPolarCoordConversion }

procedure TGPolarCoordConversion.Initialise;
begin
  FPhi1 := LatitudeShift * DD_TORADIANS;
  FLambda0 := LongitudeShift * DD_TORADIANS;

  FM1 := Cos(FPhi1) / Sqrt(1 - FEccentricitySquared * Sqr(Sin(FPhi1)));

  FX1 := 2 * ArcTan(Tan(QuarterPi + FPhi1 / 2) * Power((1 - FEccentricity * Sin(FPhi1))
    / (1 + FEccentricity * Sin(FPhi1)), FEccentricity / 2)) - HalfPi;

  Modified := false;
end;

procedure TGPolarCoordConversion.ReadE00_PARAMS( const params: array of string);
begin
  LongitudeShift := StrToExtendedDef( Trim(Copy(params[0], 1, 4)), 0.0);
  LatitudeShift := -90;
end;

function TGPolarCoordConversion.ToPointLL(long, lat: Extended): TGPointLL;
var
  X, Y, Rho, Phi, C, cappa, Tmp : Extended;
begin
  if Modified then
    Initialise;

  X := GUFrom(long, InputUnits) * GU_TORADIANS;
  Y := GUFrom(lat, InputUnits) * GU_TORADIANS;

  Rho := Sqrt(X * X + Y * Y);
  if Rho = 0 then
  begin
    Result.iLongX := Round(FPhi1 * GU_FROMRADIANS);
    Result.iLatY := Round(FLambda0 * GU_FROMRADIANS);
  end
  else
  begin
    C := 2 * ArcTan2(Tan(Rho * Cos(FX1)), (2 * FEquitorialRadius * ScaleFactor * FM1));
    cappa := ArcSin(Cos(C) * Sin(FX1) + (Y * Sin(C) * Cos(FX1) / Rho));

    Phi := cappa;
    repeat
      Tmp := Phi;
      Phi := 2 * ArcTan(Tan(QuarterPi + cappa / 2) *
        Power((1 + FEccentricity * Sin(Phi)) / (1 - FEccentricity * Sin(Phi)),
        FEccentricity * 0.5)) - HalfPi;
    until Abs(Phi - Tmp) < 0.00000005;

    Result.iLongX := Round((FLambda0 + ArcTan2(X * Sin(C), (Rho * Cos(FX1)
      * Cos(C) - Y * Sin(FX1) * Sin(C)))) * GU_FROMRADIANS);
    Result.iLatY := Round(Phi * GU_FROMRADIANS);
  end;
end;

{ TGLambertCoordConversion }

procedure TGLambertCoordConversion.Initialise;
var
  m1, m2, t1, t2, t0 : Extended;

  function Tx(Phi : Extended) : Extended;
  begin
    Result := Tan(QuarterPi - Phi * 0.5) /
      Power(
        (1 - FEccentricity * Sin(Phi)) / (1 + FEccentricity * Sin(Phi)),
        FEccentricity * 0.5);
  end;

begin
  FPhi1 := FirstParallel * DD_TORADIANS;
  FPhi2 := SecondParallel * DD_TORADIANS;
  FPhi0 := LatitudeShift * DD_TORADIANS;
  FLambda0 := LongitudeShift * DD_TORADIANS;

  m1 := Cos(FPhi1) / Sqrt(1 - FEccentricitySquared * Sqr(Sin(FPhi1)));
  m2 := Cos(FPhi2) / Sqrt(1 - FEccentricitySquared * Sqr(Sin(FPhi2)));

  t0 := Tx(FPhi0);
  t1 := Tx(FPhi1);
  t2 := Tx(FPhi2);

  FN := Ln(m1 / m2) / ln(t1 / t2);
  FF := m1 / (FN * Power(t1, FN));
  FRho0 := FEquitorialRadius * FF * Power(t0, FN);

  Modified := false;
end;

function TGLambertCoordConversion.ToPointLL(long, lat: Extended): TGPointLL;
var
  Tmp, Phi, Rho, Theta, t : Extended;
  X, Y : Extended;
begin
  if Modified then
    Initialise;

  X := GUFrom(long, InputUnits) * GU_TORADIANS;
  Y := GUFrom(lat, InputUnits) * GU_TORADIANS;

  Rho := Sign(FN) * Sqrt(X * X + Sqr(FRho0 - Y));

  Theta := ArcTan2(X, (FRho0 - Y));

  t := Power(Rho / (FEquitorialRadius * FF), 1 / FN);

  Phi := HalfPi - 2 * ArcTan(t);
  repeat
    Tmp := Phi;
    Phi := HalfPi - 2 * ArcTan(t * Power((1 - FEccentricity * Sin(Tmp))
      / (1 + FEccentricity * Sin(Tmp)), FEccentricity * 0.5));
  until Abs(Phi - tmp) < 0.00000005;

  Result.iLongX := Round((Theta / FN + FLambda0) * GU_FROMRADIANS) + Round( CentralMeridian * GU_DEGREE );
  Result.iLatY := Round(Phi * GU_FROMRADIANS) + Round( LatitudeShift * GU_DEGREE );
end;

procedure TGLambertCoordConversion.ReadE00_PARAMS( const params : array of string );
begin
  FirstParallel := StrToExtendedDef(Trim(Copy(params[0], 1, 3)), 0.0);
  SecondParallel := StrToExtendedDef(Trim(Copy(params[1], 1, 3)), 0.0);
  CentralMeridian := StrToExtendedDef(Trim(Copy(params[2], 1, 4)), 0.0);
end;

{ TGTransverseCoordConversion }

procedure TGTransverseCoordConversion.Initialise;
begin

end;

procedure TGTransverseCoordConversion.ReadE00_PARAMS( const params: array of string);
begin
end;

function TGTransverseCoordConversion.ToPointLL(long, lat: Extended): TGPointLL;
begin
  if Modified then
    Initialise;

end;

initialization
  // Register the Coord Conversion class so it can be created as needed
  RegisterClasses( [TGPolarCoordConversion, TGLambertCoordConversion, TGTransverseCoordConversion] );
end.
