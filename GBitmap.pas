//-------------------------------------------------
//Summary
//TGlobe Bitmap class
//
//Description
//32 bit bitmap image canvas used for rendering
//
//Author
//Graham Knight (tglobe@tglobe.com)
//-------------------------------------------------
unit GBitmap;

interface

uses
  Windows, Forms, Classes, SysUtils, Messages, Controls, Graphics, GSysUtils;

type
  TGColor = type Cardinal;
  PTGColorArray = ^TGColorArray;
  TGColorArray = array [0..0] of TGColor;
  TGraphicClass = class(TGraphic);

  TGBitmap = class(TGraphic)
  private
    FWidth: integer;
    FHeight: integer;
    FBitmapInfo: TBitmapInfo;
    FBits: PTGColorArray;
    FCanvas: TCanvas;
    FControlCanvas : TCanvas;
    FHandle: HBITMAP;
    FHDC: HDC;
    FPixelsPerInch: integer;
    FTransparentColor: TColor;
    FAlphaBlend: Byte;
    function  GetCanvas: TCanvas;
    function  GetPixel(X, Y: Integer): TColor;
    procedure SetPixel(X, Y: Integer; Value: TColor);
    procedure SetTransparentColor(const Value: TColor);
  protected
    procedure ChangeSize(var oldWidth, oldHeight: Integer; newWidth, newHeight: Integer); virtual;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetHeight: integer; override;
    function GetWidth: integer; override;
    function  GetEmpty: Boolean; override;
    procedure SetHeight(Value: integer); override;
    procedure SetWidth(Value: integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function  BoundsRect: TRect;
    procedure SetSize( newWidth, newHeight : integer );
    procedure Clear; overload;
    procedure Clear(FillColor: TColor); overload;
    procedure Delete;

    procedure LoadFromFile( const filename : string ); override;
    procedure SaveToFile( const filename : string ); override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;

    procedure SetPixelA(X, Y: Integer; Value: TColor; alpha : Byte);
    function BilinearPixel( const u, v: Double ): TGColor;

    property Pixel[X, Y: Integer]: TColor read GetPixel write SetPixel; default;
    property Canvas: TCanvas read GetCanvas;
    property BitmapHandle: HBITMAP read FHandle;
    property BitmapInfo: TBitmapInfo read FBitmapInfo;
    property Bits: PTGColorArray read FBits;
    property Handle: HDC read FHDC;
  published
    property AlphaBlend : Byte read FAlphaBlend write FAlphaBlend;
    property TransparentColor : TColor read FTransparentColor write SetTransparentColor;
    property Width : integer read FWidth write SetWidth;
    property Height : integer read FHeight write SetHeight;
  end;

function ToGColor( color : TColor ) : TGColor;
function ToColor( gColor : TGColor ) : TGColor;

implementation

function ToGColor( color : TColor ) : TGColor;
asm
      BSWAP  EAX
      MOV    AL,$FF
      ROR    EAX,8
      MOV    Result,EAX
end;

function ToColor( gColor : TGColor ) : TGColor;
asm
        ROL    EAX,8
        XOR    AL,AL
        BSWAP  EAX
end;

procedure FillDWord(var Buffer; Count: Integer; Value: DWOrd);
asm
// EAX = Buffer
// EDX = Count
// ECX = Value
        PUSH    EDI

        MOV     EDI,EAX  // Point EDI to destination
        MOV     EAX,ECX
        MOV     ECX,EDX
        TEST    ECX,ECX
        JS      @exit

        REP     STOSD    // Fill count dwords
@exit:
        POP     EDI
end;



procedure TGBitmap.Assign(Source: TPersistent);
var
  Canvas: TCanvas;

  procedure AssignFromBitmap(src: TBitmap);
  begin
    SetSize(src.Width, src.Height);
    if Empty then
      Exit;
    BitBlt(Handle, 0, 0, Width, Height, src.Canvas.Handle, 0, 0, SRCCOPY);
  end;

begin
  if Source = nil then
    SetSize(0, 0)
  else
    if Source is TGBitmap then
    begin
      SetSize(TGBitmap(Source).Width, TGBitmap(Source).Height);
      if Empty then
        Exit;
      BitBlt(Handle, 0, 0, Width, Height, TGBitmap(Source).Handle, 0, 0, SRCCOPY);
    end
    else
      if Source is TBitmap then
        AssignFromBitmap(TBitmap(Source))
      else
        if Source is TGraphic then
        begin
          SetSize(TGraphic(Source).Width, TGraphic(Source).Height);
          if Empty then
            Exit;
          Canvas := TCanvas.Create;
          try
            Canvas.Handle := Self.Handle;
            TGraphicClass(Source).Draw(Canvas, Rect(0, 0, Width, Height));
          finally
            Canvas.Free;
          end;
        end
        else
          if Source is TPicture then
          begin
            with TPicture(Source) do
              if TPicture(Source).Graphic is TBitmap then
                AssignFromBitmap(TBitmap(TPicture(Source).Graphic))
              else
              begin
                // icons, metafiles etc...
                if TPicture(Source).Graphic is TIcon then
                  TIcon( TPicture(Source).Graphic ).Handle;  // Make sure the image is properly realised before getting the Width or Height

                SetSize(TPicture(Source).Graphic.Width, TPicture(Source).Graphic.Height);
                if Empty then
                  Exit;
                Canvas := TCanvas.Create;
                try
                  Canvas.Handle := Self.Handle;
                  TGraphicClass(Graphic).Draw(Canvas, Rect(0, 0, Width, Height));
                finally
                  Canvas.Free;
                end;
              end;
          end
          else
            inherited; // default handler
end;

function TGBitmap.BilinearPixel( const u, v: Double ): TGColor;
var
  x, y, tmp, tmp2 : integer;
  u_ratio, v_ratio, u_inverse, v_inverse : Double;
  w1, w2, w3, w4 : Double;
  c00, c01, c10, c11 : TGColor;
begin
  x := Trunc(u);
  y := Trunc(v);

  u_ratio := u - x;
  v_ratio := v - y;
  u_inverse := 1.0 - u_ratio;
  v_inverse := 1.0 - v_ratio;

  w1 := u_inverse * v_inverse;
  w2 := u_ratio * v_inverse;
  w3 := u_inverse * v_ratio;
  w4 := u_ratio * v_ratio;

  if x < Width - 1 then Inc( x );
  if y < Height - 1 then Inc( y );

  tmp := x + y * Width;
  tmp2 := tmp - Width;
  c11 := Bits[tmp];
  c01 := Bits[tmp - 1];
  c10 := Bits[tmp2];
  c00 := Bits[tmp2 - 1];

  Result := RGB(
    Trunc( w1 * Byte(c00)        + w2 * Byte(c10)        + w3 * Byte(c01)        + w4 * Byte(c11)),
    Trunc( w1 * Byte(c00 shr 8)  + w2 * Byte(c10 shr 8)  + w3 * Byte(c01 shr 8)  + w4 * Byte(c11 shr 8)),
    Trunc( w1 * Byte(c00 shr 16) + w2 * Byte(c10 shr 16) + w3 * Byte(c01 shr 16) + w4 * Byte(c11 shr 16))
  );
end;

function TGBitmap.BoundsRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

procedure TGBitmap.ChangeSize(var oldWidth, oldHeight: Integer; newWidth, newHeight: Integer);
begin
  FreeAndNil( FCanvas );

  if FHDC <> 0 then
    DeleteDC(FHDC);
  if FHandle <> 0 then
    DeleteObject(FHandle);

  FHDC := 0;
  FHandle := 0;
  FBits := nil;
  oldWidth := 0;
  oldHeight := 0;

  if (newWidth > 0) and (newHeight > 0) then
  begin
    with FBitmapInfo.bmiHeader do
    begin
      biWidth := NewWidth;
      biHeight := -NewHeight;
    end;

    FHandle := CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS, Pointer(FBits), 0, 0);
    if FBits = nil then
      raise Exception.Create('Failed to allocate DIB Handle');

    FHDC := CreateCompatibleDC(0);
    if FHDC = 0 then
    begin
      DeleteObject(FHandle);
      FHandle := 0;
      FBits := nil;
      raise Exception.Create('Failed to Create compatible DC');
    end;

    if SelectObject(FHDC, FHandle) = 0 then
    begin
      DeleteDC(FHDC);
      DeleteObject(FHandle);
      FHDC := 0;
      FHandle := 0;
      FBits := nil;
      raise Exception.Create('Failed to select object into DC');
    end;
  end;

  oldWidth := NewWidth;
  oldHeight := NewHeight;
end;

procedure TGBitmap.Clear;
begin
  Clear(clBlack);
end;

procedure TGBitmap.Clear(FillColor: TColor);
begin
  if Empty then
    Exit;

  FillDWord(Bits[0], Width * Height, ToGColor( FillColor ));
end;

constructor TGBitmap.Create;
begin
  inherited;

  FillChar(FBitmapInfo, SizeOf(TBitmapInfo), 0);

  FAlphaBlend := 255;
  TransparentColor := clNone; // Alpha channel is set to 0 to indicate no transparent color
  FPixelsPerInch := Screen.PixelsPerInch;

  with FBitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biCompression := BI_RGB;
    biPlanes := 1;
    biBitCount := 32;
  end;

  FControlCanvas := TCanvas.Create;

  SetSize( 0, 0 );
end;

procedure TGBitmap.Delete;
begin
  SetSize(0, 0);
end;

destructor TGBitmap.Destroy;
begin
  if FCanvas <> nil then
  begin
    FCanvas.Handle := 0;
    FreeAndNil( FCanvas );
  end;
  FControlCanvas.Free;
  SetSize(0, 0);
  inherited;
end;

procedure TGBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  if Empty then
    Exit;

  with Rect do
    StretchBlt(ACanvas.Handle, Left, Top, Right - Left, Bottom - Top,
      Handle, 0, 0, Width, Height, ACanvas.CopyMode);
end;

function TGBitmap.GetCanvas: TCanvas;
begin
  if FCanvas = nil then
  begin
    FCanvas := TCanvas.Create;
    FCanvas.Handle := Handle;
  end;
  Result := FCanvas;
end;

function TGBitmap.GetEmpty: Boolean;
begin
  Result := (FHandle = 0) or (Width = 0) or (Height = 0);
end;

function TGBitmap.GetHeight: integer;
begin
  Result := FHeight;
end;

function TGBitmap.GetPixel(X, Y: Integer): TColor;
begin
  if ( X < Width ) and ( Y < Height ) then
    Result := ToColor( Bits[X + Y * Width] )
  else
    Result := clNone;
end;

function TGBitmap.GetWidth: integer;
begin
  Result := FWidth;
end;

procedure TGBitmap.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  raise EGException.Create( 'TGBitmap.LoadFromClipboardFormat: Not implemented' );
end;

procedure TGBitmap.LoadFromFile(const filename: string);
var
  Pict: TPicture;
begin
  if FileExists( filename ) then
  begin
    Pict := TPicture.Create;
    try
      Pict.LoadFromFile(fileName);
      Assign(Pict);
    finally
      Pict.Free;
    end;
  end;
end;

procedure TGBitmap.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  raise EGException.Create( 'TGBitmap.SaveToClipboardFormat: Not implemented' );
end;

procedure TGBitmap.SaveToFile(const filename: string);
begin
  raise EGException.Create( 'TGBitmap.SaveToFile: Not implemented' );
end;

procedure TGBitmap.SetHeight(Value: integer);
begin
  SetSize(Width, Value);
end;

procedure TGBitmap.SetSize(newWidth, newHeight: integer);
begin
  if newWidth < 0 then
    newWidth := 0;
  if newHeight < 0 then
    newHeight := 0;

  if (newWidth <> FWidth) or (newHeight <> FHeight) then
    ChangeSize(FWidth, FHeight, newWidth, newHeight);
end;

procedure TGBitmap.SetPixel(X, Y: Integer; Value: TColor);
begin
  Bits[X + Y * Width] := ToGColor( Value );
end;

procedure TGBitmap.SetWidth(Value: integer);
begin
  SetSize(Value, Height);
end;

procedure TGBitmap.SetPixelA(X, Y: Integer; Value: TColor; alpha: Byte);
var
  R,G,B : Byte;
  src, target : TGColor;
begin
  if (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
  begin
    if alpha < 255 then
    begin
      target := Bits[X + Y * Width];

      src := ToGColor( Value );

      R := (GetRValue( src ) * alpha + GetRValue( target ) * (255 - alpha)) shr 8;
      G := (GetGValue( src ) * alpha + GetGValue( target ) * (255 - alpha)) shr 8;
      B := (GetBValue( src ) * alpha + GetBValue( target ) * (255 - alpha)) shr 8;

      Value := RGB( R, G, B );
    end;

    Bits[X + Y * Width] := Value;
  end;
end;

procedure TGBitmap.SetTransparentColor(const Value: TColor);
begin
  FTransparentColor := Value;
  Transparent := ( Value <> clNone ); 
end;

procedure TGBitmap.LoadFromStream(Stream: TStream);
begin
  raise EGException.Create( 'TGBitmap.LoadFromStream: Not implemented' );
end;

procedure TGBitmap.SaveToStream(Stream: TStream);
begin
  raise EGException.Create( 'TGBitmap.SaveToStream: Not implemented' );
end;

end.
