//-----------------------------------------------------------------------
// Summary
//	TGlobe Encryption and Decryption routines
//
// Description
// 	Used to Encrypt and Decrypt Data blocks written to the SDB
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
unit GSDBCrypt;

interface

Uses Classes, SysUtils, GSysUtils;

const
  Delta: Cardinal = $9e3779b9;

type
  EGCryptException = class(EGException);

  TCardinalArray = array of Cardinal;

  TGSDBCrypt = class(TGRoot)
  public
    constructor Create( const key : String ); virtual; abstract;

    function CheckSum( blockPtr : Pointer; blockSize : integer ) : Cardinal; virtual; abstract;
    procedure Decrypt( blockPtr : Pointer; blockSize : integer ); virtual; abstract;
    procedure Encrypt( blockPtr : Pointer; blockSize : integer ); virtual; abstract;
  end;

  TGSDBTeaCrypt = class(TGSDBCrypt)
  private
    FKey : array[0..3] of Cardinal;
  public
    constructor Create( const key : String ); override;

    function CheckSum( blockPtr : Pointer; blockSize : integer ) : Cardinal; override;
    procedure Decrypt( blockPtr : Pointer; blockSize : integer ); override;
    procedure Encrypt( blockPtr : Pointer; blockSize : integer ); override;
  end;


implementation



function TGSDBTeaCrypt.CheckSum(blockPtr: Pointer; blockSize: integer): Cardinal;
var
  q, n, y, sum, e, p: Cardinal;
begin
  n := blockSize div SizeOf( Cardinal );
  q := 6 + 52 div n;
  Result := TCardinalArray( blockPtr )[n-1];
  sum := 0;
  repeat
    Inc(sum,Delta);
    e := (sum shr 2) and 3;
    for p := 0 to n-1 do
    begin
      y := TCardinalArray( blockPtr )[p];
      Inc(y, (((Result shl 4) xor (Result shr 5)) + Result) xor (FKey[(p and 3) xor e] + sum));
      Result := y;
    end;
    Dec(q);
  until q = 0;
end;

constructor TGSDBTeaCrypt.Create(const key: String);
var
  sl : TStringList;
begin
  sl := TStringList.Create;
  try
    try
      sl.CommaText := key;
      FKey[0] := StrToInt( sl[0] );
      FKey[1] := StrToInt( sl[1] );
      FKey[2] := StrToInt( sl[2] );
      FKey[3] := StrToInt( sl[3] );
    except
      raise EGException.Create( 'TGSDBTeaCrypt: Passed invalid Key' );
    end;
  finally
    sl.Free;
  end;
end;

procedure TGSDBTeaCrypt.Decrypt(blockPtr: Pointer; blockSize: integer);
var
  n, z, y, sum, e, p, q: Cardinal;
begin
  n := blockSize div SizeOf( Cardinal );
  q := 6 + 52 div n;
  sum := q * Delta;
  while sum <> 0 do
  begin
    e := (sum shr 2) and 3;
    for p := n-1 downto 1 do
    begin
      z := TCardinalArray( blockPtr )[p-1];
      y := TCardinalArray( blockPtr )[p];
      Dec(y,(((z shl 4) xor (z shr 5)) + z) xor (FKey[(p and 3) xor e] + sum));
      TCardinalArray( blockPtr )[p] := y;
    end;
    z := TCardinalArray( blockPtr )[n-1];
    y := TCardinalArray( blockPtr )[0];
    Dec(y,(((z shl 4) xor (z shr 5)) + z) xor (FKey[e] + sum));
    TCardinalArray( blockPtr )[0] := y;
    Dec(sum,Delta);
  end;
end;

procedure TGSDBTeaCrypt.Encrypt(blockPtr: Pointer; blockSize: integer);
var
  q, n, z, y, sum, e, p: Cardinal;
begin
  n := blockSize div SizeOf( Cardinal );
  q := 6 + 52 div n;
  z := TCardinalArray( blockPtr )[n-1];
  sum := 0;
  repeat
    Inc(sum,Delta);
    e := (sum shr 2) and 3;
    for p := 0 to n-1 do
    begin
      y := TCardinalArray( blockPtr )[p];
      Inc(y, (((z shl 4) xor (z shr 5)) + z) xor (FKey[(p and 3) xor e] + sum));
      TCardinalArray( blockPtr )[p] := y;
      z := y;
    end;
    Dec(q);
  until q = 0;
end;

end.
