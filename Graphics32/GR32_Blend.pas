unit GR32_Blend;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2006
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *  Mattias Andersson
 *      - 2004/07/07 - MMX Blendmodes
 *      - 2004/12/10 - _MergeReg, M_MergeReg
 *
 *  Michael Hansen <dyster_tid@hotmail.com>
 *      - 2004/07/07 - Pascal Blendmodes, function setup
 *      - 2005/08/19 - New merge table concept and reference implementations
 *
 *  Bob Voigt
 *      - 2004/08/25 - ColorDiv
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  GR32, SysUtils;

var
  MMX_ACTIVE: Boolean;

procedure EMMS;

type
{ Function Prototypes }
  TCombineReg  = function(X, Y, W: TColor32): TColor32;
  TCombineMem  = procedure(F: TColor32; var B: TColor32; W: TColor32);
  TBlendReg    = function(F, B: TColor32): TColor32;
  TBlendMem    = procedure(F: TColor32; var B: TColor32);
  TBlendRegEx  = function(F, B, M: TColor32): TColor32;
  TBlendMemEx  = procedure(F: TColor32; var B: TColor32; M: TColor32);
  TBlendLine   = procedure(Src, Dst: PColor32; Count: Integer);
  TBlendLineEx = procedure(Src, Dst: PColor32; Count: Integer; M: TColor32);
  TCombineLine = procedure(Src, Dst: PColor32; Count: Integer; W: TColor32);

var
{ Function Variables }
  CombineReg: TCombineReg;
  CombineMem: TCombineMem;

  BlendReg: TBlendReg;
  BlendMem: TBlendMem;

  BlendRegEx: TBlendRegEx;
  BlendMemEx: TBlendMemEx;

  BlendLine: TBlendLine;
  BlendLineEx: TBlendLineEx;

  CombineLine: TCombineLine;

  MergeReg: TBlendReg;
  MergeMem: TBlendMem;

  MergeRegEx: TBlendRegEx;
  MergeMemEx: TBlendMemEx;

  MergeLine: TBlendLine;
  MergeLineEx: TBlendLineEx;

{ Access to alpha composite functions corresponding to a combine mode }
  BLEND_REG: array[TCombineMode] of TBlendReg;
  BLEND_MEM: array[TCombineMode] of TBlendMem;
  BLEND_REG_EX: array[TCombineMode] of TBlendRegEx;
  BLEND_MEM_EX: array[TCombineMode] of TBlendMemEx;
  BLEND_LINE: array[TCombineMode] of TBlendLine;
  BLEND_LINE_EX: array[TCombineMode] of TBlendLineEx;

{ Color algebra functions }
  ColorAdd: TBlendReg;
  ColorSub: TBlendReg;
  ColorDiv: TBlendReg;
  ColorModulate: TBlendReg;
  ColorMax: TBlendReg;
  ColorMin: TBlendReg;
  ColorDifference: TBlendReg;
  ColorAverage: TBlendReg;
  ColorExclusion: TBlendReg;
  ColorScale: TBlendReg;

{ Special LUT pointers }
  AlphaTable: Pointer;
  bias_ptr: Pointer;
  alpha_ptr: Pointer;


{ Misc stuff }
function Lighten(C: TColor32; Amount: Integer): TColor32;


implementation

uses Math, GR32_System, GR32_LowLevel, GR32_Math;

var
  RcTable: array [Byte, Byte] of Byte;
  DivTable: array [Byte, Byte] of Byte;

{ Merge }

function _MergeReg(F, B: TColor32): TColor32;
asm
  // EAX <- F
  // EDX <- B

  // GR32_Blend.pas.156: if F.A = 0 then
    test eax,$ff000000
    jz   @exit0

  // GR32_Blend.pas.160: else if B.A = 255 then
    cmp     edx,$ff000000
    jnc     @blend

  // GR32_Blend.pas.158: else if F.A = 255 then
    cmp     eax,$ff000000
    jnc     @exit

  // else if B.A = 0 then
    test    edx,$ff000000
    jz      @exit

@4:
    push ebx
    push esi
    push edi
    add  esp,-$0c
    mov  [esp+$04],edx
    mov  [esp],eax

  // AH <- F.A
  // DL, CL <- B.A
    shr eax,16
    and eax,$0000ff00
    shr edx,24
    mov cl,dl
    nop
    nop
    nop

  // EDI <- PF
  // EDX <- PB
  // ESI <- PR

  // GR32_Blend.pas.164: PF := @DivTable[F.A];
    lea edi,[eax+DivTable]
  // GR32_Blend.pas.165: PB := @DivTable[B.A];
    shl edx,$08
    lea edx,[edx+DivTable]
  // GR32_Blend.pas.166: Result.A := B.A + F.A - PB[F.A];
    shr eax,8
    //add cl,al
    add ecx,eax
    //sub cl,[edx+eax]
    sub ecx,[edx+eax]
    mov [esp+$0b],cl
  // GR32_Blend.pas.167: PR := @RcTable[Result.A];
    shl ecx,$08
    and ecx,$0000ffff
    lea esi,[ecx+RcTable]

  { Red component }

  // GR32_Blend.pas.169: Result.R := PB[B.R];
    xor eax,eax
    mov al,[esp+$06]
    mov cl,[edx+eax]
    mov [esp+$0a],cl
  // GR32_Blend.pas.170: X := F.R - Result.R;
    mov al,[esp+$02]
    xor ebx,ebx
    mov bl,cl
    sub eax,ebx
  // GR32_Blend.pas.171: if X >= 0 then
    jl @5
  // GR32_Blend.pas.172: Result.R := PR[PF[X] + Result.R]
    movzx eax,byte ptr[edi+eax]
    and ecx,$000000ff
    add eax,ecx
    mov al,[esi+eax]
    mov [esp+$0a],al
    jmp @6
@5:
  // GR32_Blend.pas.252: Result.R := PR[Result.R - PF[-X]];
    neg eax
    movzx eax,byte ptr[edi+eax]
    xor ecx,ecx
    mov cl,[esp+$0a]
    sub ecx,eax
    mov al,[esi+ecx]
    mov [esp+$0a],al


  { Green component }

@6:
  // GR32_Blend.pas.176: Result.G := PB[B.G];
    xor eax,eax
    mov al,[esp+$05]
    mov cl,[edx+eax]
    mov [esp+$09],cl
  // GR32_Blend.pas.177: X := F.G - Result.G;
    mov al,[esp+$01]
    xor ebx,ebx
    mov bl,cl
    sub eax,ebx
  // GR32_Blend.pas.178: if X >= 0 then
    jl @7
  // GR32_Blend.pas.179: Result.G := PR[PF[X] + Result.G]
    movzx eax,byte ptr[edi+eax]
    and ecx,$000000ff
    add eax,ecx
    mov al,[esi+eax]
    mov [esp+$09],al
    jmp @8
@7:
  // GR32_Blend.pas.259: Result.G := PR[Result.G - PF[-X]];
    neg eax
    movzx eax,byte ptr[edi+eax]
    xor ecx,ecx
    mov cl,[esp+$09]
    sub ecx,eax
    mov al,[esi+ecx]
    mov [esp+$09],al


  { Blue component }

@8:
  // GR32_Blend.pas.183: Result.B := PB[B.B];
    xor eax,eax
    mov al,[esp+$04]
    mov cl,[edx+eax]
    mov [esp+$08],cl
  // GR32_Blend.pas.184: X := F.B - Result.B;
    mov al,[esp]
    xor edx,edx
    mov dl,cl
    sub eax,edx
  // GR32_Blend.pas.185: if X >= 0 then
    jl @9
  // GR32_Blend.pas.186: Result.B := PR[PF[X] + Result.B]
    movzx eax,byte ptr[edi+eax]
    xor edx,edx
    mov dl,cl
    add eax,edx
    mov al,[esi+eax]
    mov [esp+$08],al
    jmp @10
@9:
  // GR32_Blend.pas.266: Result.B := PR[Result.B - PF[-X]];
    neg eax
    movzx eax,byte ptr[edi+eax]
    xor edx,edx
    mov dl,cl
    sub edx,eax
    mov al,[esi+edx]
    mov [esp+$08],al

@10:
  // EAX <- Result
    mov eax,[esp+$08]

  // GR32_Blend.pas.190: end;
    add esp,$0c
    pop edi
    pop esi
    pop ebx
    ret
@blend:
    call dword ptr [BlendReg]
    or   eax,$ff000000
    ret
@exit0:
    mov eax,edx
@exit:
end;

function _MergeRegEx(F, B, M: TColor32): TColor32;
begin
  Result := _MergeReg(DivTable[M, F shr 24] shl 24 or F and $00FFFFFF, B);
end;

procedure _MergeMem(F: TColor32; var B: TColor32);
begin
  B := _MergeReg(F, B);
end;

procedure _MergeMemEx(F: TColor32; var B: TColor32; M: TColor32);
begin
  B := _MergeReg(DivTable[M, F shr 24] shl 24 or F and $00FFFFFF, B);
end;

procedure _MergeLine(Src, Dst: PColor32; Count: Integer);
begin
  while Count > 0 do
  begin
    Dst^ := _MergeReg(Src^, Dst^);
    Inc(Src);
    Inc(Dst);
    Dec(Count);
  end;
end;

procedure _MergeLineEx(Src, Dst: PColor32; Count: Integer; M: TColor32);
var
  PM: PByteArray absolute M;
begin
  PM := @DivTable[M];
  while Count > 0 do
  begin
    Dst^ := _MergeReg(PM[Src^ shr 24] shl 24 or Src^ and $00FFFFFF, Dst^);
    Inc(Src);
    Inc(Dst);
    Dec(Count);
  end;
end;

{ Non-MMX versions }

const bias = $00800080;

function _CombineReg(X, Y, W: TColor32): TColor32;
asm
  // combine RGBA channels of colors X and Y with the weight of X given in W
  // Result Z = W * X + (1 - W) * Y (all channels are combined, including alpha)
  // EAX <- X
  // EDX <- Y
  // ECX <- W

  // W = 0 or $FF?
        JCXZ    @1              // CX = 0 ?  => Result := EDX
        CMP     ECX,$FF         // CX = $FF ?  => Result := EDX
        JE      @2

        PUSH    EBX

  // P = W * X
        MOV     EBX,EAX         // EBX  <-  Xa Xr Xg Xb
        AND     EAX,$00FF00FF   // EAX  <-  00 Xr 00 Xb
        AND     EBX,$FF00FF00   // EBX  <-  Xa 00 Xg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 Xa 00 Xg
        IMUL    EBX,ECX         // EBX  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pa 00 Pg 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * Y
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     EBX,EDX         // EBX  <-  Ya Yr Yg Yb
        AND     EDX,$00FF00FF   // EDX  <-  00 Yr 00 Yb
        AND     EBX,$FF00FF00   // EBX  <-  Ya 00 Yg 00
        IMUL    EDX,ECX         // EDX  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 Ya 00 Yg
        IMUL    EBX,ECX         // EBX  <-  Qa ** Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX,EDX         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  Za Zr Zg Zb

        POP     EBX
        RET

@1:     MOV     EAX,EDX
@2:     RET
end;

procedure _CombineMem(F: TColor32; var B: TColor32; W: TColor32);
asm
  // EAX <- F
  // [EDX] <- B
  // ECX <- W

  // Check W
        JCXZ    @1              // W = 0 ?  => write nothing
        CMP     ECX,$FF         // W = 255? => write F
        JZ      @2

        PUSH    EBX
        PUSH    ESI

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  ** Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$FF00FF00   // EBX  <-  Fa 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 Fa 00 Fg
        IMUL    EBX,ECX         // EBX  <-  00 00 Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr 00 Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  00 Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     ESI,[EDX]
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     EBX,ESI         // EBX  <-  Ba Br Bg Bb
        AND     ESI,$00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     EBX,$FF00FF00   // EBX  <-  Ba 00 Bg 00
        IMUL    ESI,ECX         // ESI  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 Ba 00 Bg
        IMUL    EBX,ECX         // EBX  <-  Qa 00 Qg **
        ADD     ESI,bias
        AND     ESI,$FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     ESI,8           // ESI  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX,ESI         // EBX  <-  00 Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  00 Zr Zg Zb

        MOV     [EDX],EAX

        POP     ESI
        POP     EBX
@1:     RET

@2:     MOV     [EDX],EAX
        RET
end;

function _BlendReg(F, B: TColor32): TColor32;
asm
  // blend foregrownd color (F) to a background color (B),
  // using alpha channel value of F
  // Result Z = Fa * Frgb + (1 - Fa) * Brgb
  // EAX <- F
  // EDX <- B

  // Test Fa = 255 ?
        CMP     EAX,$FF000000   // Fa = 255 ? => Result = EAX
        JNC     @2

  // Test Fa = 0 ?
        TEST    EAX,$FF000000   // Fa = 0 ?   => Result = EDX
        JZ      @1

  // Get weight W = Fa * M
        MOV     ECX,EAX         // ECX  <-  Fa Fr Fg Fb
        SHR     ECX,24          // ECX  <-  00 00 00 Fa

        PUSH    EBX

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  Fa Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$FF00FF00   // EBX  <-  Fa 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 Fa 00 Fg
        IMUL    EBX,ECX         // EBX  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr ** Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * B
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     EBX,EDX         // EBX  <-  Ba Br Bg Bb
        AND     EDX,$00FF00FF   // EDX  <-  00 Br 00 Bb
        AND     EBX,$FF00FF00   // EBX  <-  Ba 00 Bg 00
        IMUL    EDX,ECX         // EDX  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 Ba 00 Bg
        IMUL    EBX,ECX         // EBX  <-  Qa ** Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX,EDX         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  Za Zr Zg Zb

        POP     EBX
        RET

@1:     MOV     EAX,EDX
@2:     RET
end;

procedure _BlendMem(F: TColor32; var B: TColor32);
asm
  // EAX <- F
  // [EDX] <- B


  // Test Fa = 0 ?
        TEST    EAX,$FF000000   // Fa = 0 ?   => do not write
        JZ      @2

  // Get weight W = Fa * M
        MOV     ECX,EAX         // ECX  <-  Fa Fr Fg Fb
        SHR     ECX,24          // ECX  <-  00 00 00 Fa

  // Test Fa = 255 ?
        CMP     ECX,$FF
        JZ      @1

        PUSH EBX
        PUSH ESI

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  Fa Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$FF00FF00   // EBX  <-  Fa 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 Fa 00 Fg
        IMUL    EBX,ECX         // EBX  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr ** Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     ESI,[EDX]
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     EBX,ESI         // EBX  <-  Ba Br Bg Bb
        AND     ESI,$00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     EBX,$FF00FF00   // EBX  <-  Ba 00 Bg 00
        IMUL    ESI,ECX         // ESI  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 Ba 00 Bg
        IMUL    EBX,ECX         // EBX  <-  Qa ** Qg **
        ADD     ESI,bias
        AND     ESI,$FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     ESI,8           // ESI  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX,ESI         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  Za Zr Zg Zb
        MOV     [EDX],EAX

        POP     ESI
        POP     EBX
        RET

@1:     MOV     [EDX],EAX
@2:     RET
end;

function _BlendRegEx(F, B, M: TColor32): TColor32;
asm
  // blend foregrownd color (F) to a background color (B),
  // using alpha channel value of F multiplied by master alpha (M)
  // no checking for M = $FF, if this is the case when Graphics32 uses BlendReg
  // Result Z = Fa * M * Frgb + (1 - Fa * M) * Brgb
  // EAX <- F
  // EDX <- B
  // ECX <- M

  // Check Fa > 0 ?
        TEST    EAX,$FF000000   // Fa = 0? => Result := EDX
        JZ      @2

        PUSH    EBX

  // Get weight W = Fa * M
        MOV     EBX,EAX         // EBX  <-  Fa Fr Fg Fb
        INC     ECX             // 255:256 range bias
        SHR     EBX,24          // EBX  <-  00 00 00 Fa
        IMUL    ECX,EBX         // ECX  <-  00 00  W **
        SHR     ECX,8           // ECX  <-  00 00 00  W
        JZ      @1              // W = 0 ?  => Result := EDX

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  ** Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$0000FF00   // EBX  <-  00 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 00 00 Fg
        IMUL    EBX,ECX         // EBX  <-  00 00 Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr ** Pb
        ADD     EBX,bias
        AND     EBX,$0000FF00   // EBX  <-  00 00 Pg 00
        OR      EAX,EBX         // EAX  <-  00 Pr Pg Pb

  // W = 1 - W; Q = W * B
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     EBX,EDX         // EBX  <-  00 Br Bg Bb
        AND     EDX,$00FF00FF   // EDX  <-  00 Br 00 Bb
        AND     EBX,$0000FF00   // EBX  <-  00 00 Bg 00
        IMUL    EDX,ECX         // EDX  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 00 00 Bg
        IMUL    EBX,ECX         // EBX  <-  00 00 Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // EDX  <-  Qr 00 Qb 00
        SHR     EDX,8           // EDX  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$0000FF00   // EBX  <-  00 00 Qg 00
        OR      EBX,EDX         // EBX  <-  00 Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  00 Zr Zg Zb

        POP     EBX
        RET
        
@1:     POP     EBX
@2:     MOV     EAX,EDX
        RET
end;

procedure _BlendMemEx(F: TColor32; var B: TColor32; M: TColor32);
asm
  // EAX <- F
  // [EDX] <- B
  // ECX <- M

  // Check Fa > 0 ?
        TEST    EAX,$FF000000   // Fa = 0? => write nothing
        JZ      @2

        PUSH    EBX

  // Get weight W = Fa * M
        MOV     EBX,EAX         // EBX  <-  Fa Fr Fg Fb
        INC     ECX             // 255:256 range bias
        SHR     EBX,24          // EBX  <-  00 00 00 Fa
        IMUL    ECX,EBX         // ECX  <-  00 00  W **
        SHR     ECX,8           // ECX  <-  00 00 00  W
        JZ      @1              // W = 0 ?  => write nothing

        PUSH    ESI

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  ** Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$0000FF00   // EBX  <-  00 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 00 00 Fg
        IMUL    EBX,ECX         // EBX  <-  00 00 Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr ** Pb
        ADD     EBX,bias
        AND     EBX,$0000FF00   // EBX  <-  00 00 Pg 00
        OR      EAX,EBX         // EAX  <-  00 Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     ESI,[EDX]
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     EBX,ESI         // EBX  <-  00 Br Bg Bb
        AND     ESI,$00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     EBX,$0000FF00   // EBX  <-  00 00 Bg 00
        IMUL    ESI,ECX         // ESI  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 00 00 Bg
        IMUL    EBX,ECX         // EBX  <-  00 00 Qg **
        ADD     ESI,bias
        AND     ESI,$FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     ESI,8           // ESI  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$0000FF00   // EBX  <-  00 00 Qg 00
        OR      EBX,ESI         // EBX  <-  00 Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  00 Zr Zg Zb

        MOV     [EDX],EAX
        POP     ESI

@1:     POP     EBX
@2:     RET
end;

procedure _BlendLine(Src, Dst: PColor32; Count: Integer);
asm
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // test the counter for zero or negativity
        TEST    ECX,ECX
        JS      @4

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX         // ESI <- Src
        MOV     EDI,EDX         // EDI <- Dst

  // loop start
@1:     MOV     EAX,[ESI]
        TEST    EAX,$FF000000
        JZ      @3              // complete transparency, proceed to next point

        PUSH    ECX             // store counter

  // Get weight W = Fa * M
        MOV     ECX,EAX         // ECX  <-  Fa Fr Fg Fb
        SHR     ECX,24          // ECX  <-  00 00 00 Fa

  // Test Fa = 255 ?
        CMP     ECX,$FF
        JZ      @2

  // P = W * F
        MOV     EBX,EAX         // EBX  <-  Fa Fr Fg Fb
        AND     EAX,$00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX,$FF00FF00   // EBX  <-  Fa 00 Fg 00
        IMUL    EAX,ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX,8           // EBX  <-  00 Fa 00 Fg
        IMUL    EBX,ECX         // EBX  <-  Pa ** Pg **
        ADD     EAX,bias
        AND     EAX,$FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX,8           // EAX  <-  00 Pr ** Pb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX,EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     EDX,[EDI]
        XOR     ECX,$000000FF   // ECX  <-  1 - ECX
        MOV     EBX,EDX         // EBX  <-  Ba Br Bg Bb
        AND     EDX,$00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     EBX,$FF00FF00   // EBX  <-  Ba 00 Bg 00
        IMUL    EDX,ECX         // ESI  <-  Qr ** Qb **
        SHR     EBX,8           // EBX  <-  00 Ba 00 Bg
        IMUL    EBX,ECX         // EBX  <-  Qa ** Qg **
        ADD     EDX,bias
        AND     EDX,$FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     EDX,8           // ESI  <-  00 Qr ** Qb
        ADD     EBX,bias
        AND     EBX,$FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX,EDX         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX,EBX         // EAX  <-  Za Zr Zg Zb
@2:     MOV     [EDI],EAX

        POP     ECX             // restore counter

@3:     ADD     ESI,4
        ADD     EDI,4

  // loop end
        DEC     ECX
        JNZ     @1

        POP     EDI
        POP     ESI
        POP     EBX

@4:     RET
end;

procedure _BlendLineEx(Src, Dst: PColor32; Count: Integer; M: TColor32);
begin
  while Count > 0 do
  begin
    _BlendMemEx(Src^, Dst^, M);
    Inc(Src);
    Inc(Dst);
    Dec(Count);
  end;
end;

procedure _CombineLine(Src, Dst: PColor32; Count: Integer; W: TColor32);
begin
  while Count > 0 do
  begin
    _CombineMem(Src^, Dst^, W);
    Inc(Src);
    Inc(Dst);
    Dec(Count);
  end;
end;


{ MMX versions }


procedure GenAlphaTable;
var
  I: Integer;
  L: Longword;
  P: ^Longword;
begin
  GetMem(AlphaTable, 257 * 8);
  alpha_ptr := Pointer(Integer(AlphaTable) and $FFFFFFF8);
  if Integer(alpha_ptr) < Integer(AlphaTable) then
    alpha_ptr := Pointer(Integer(alpha_ptr) + 8);
  P := alpha_ptr;
  for I := 0 to 255 do
  begin
    L := I + I shl 16;
    P^ := L;
    Inc(P);
    P^ := L;
    Inc(P);
  end;
  bias_ptr := Pointer(Integer(alpha_ptr) + $80 * 8);
end;

procedure FreeAlphaTable;
begin
  FreeMem(AlphaTable);
end;

procedure EMMS;
begin
  if MMX_ACTIVE then
  asm
    db $0F,$77               /// EMMS
  end;
end;

function M_CombineReg(X, Y, W: TColor32): TColor32;
asm
  // EAX - Color X
  // EDX - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        db $0F,$6E,$C8           /// MOVD      MM1,EAX
        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        SHL       ECX,3
        db $0F,$6E,$D2           /// MOVD      MM2,EDX
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0
        ADD       ECX,alpha_ptr
        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$09           /// PMULLW    MM1,[ECX]
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        MOV       ECX,bias_ptr
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$C8           /// MOVD      EAX,MM1
end;

procedure M_CombineMem(F: TColor32; var B: TColor32; W: TColor32);
asm
  // EAX - Color X
  // [EDX] - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        JCXZ      @1
        CMP       ECX,$FF
        JZ        @2

        db $0F,$6E,$C8           /// MOVD      MM1,EAX
        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        SHL       ECX,3
        db $0F,$6E,$12           /// MOVD      MM2,[EDX]
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0
        ADD       ECX,alpha_ptr
        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$09           /// PMULLW    MM1,[ECX]
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        MOV       ECX,bias_ptr
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$0A           /// MOVD      [EDX],MM1
@1:     RET

@2:     MOV       [EDX],EAX
end;

function M_BlendReg(F, B: TColor32): TColor32;
asm
  // blend foregrownd color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // EDX <- B
  // Result := Fa * (Frgb - Brgb) + Brgb
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$EF,$DB           /// PXOR      MM3,MM3
        db $0F,$6E,$D2           /// MOVD      MM2,EDX
        db $0F,$60,$C3           /// PUNPCKLBW MM0,MM3
        MOV     ECX,bias_ptr
        db $0F,$60,$D3           /// PUNPCKLBW MM2,MM3
        db $0F,$6F,$C8           /// MOVQ      MM1,MM0
        db $0F,$69,$C9           /// PUNPCKHWD MM1,MM1
        db $0F,$F9,$C2           /// PSUBW     MM0,MM2
        db $0F,$6A,$C9           /// PUNPCKHDQ MM1,MM1
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        db $0F,$D5,$C1           /// PMULLW    MM0,MM1
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$D0           /// PADDW     MM2,MM0
        db $0F,$71,$D2,$08       /// PSRLW     MM2,8
        db $0F,$67,$D3           /// PACKUSWB  MM2,MM3
        db $0F,$7E,$D0           /// MOVD      EAX,MM2
end;

procedure M_BlendMem(F: TColor32; var B: TColor32);
asm
  // EAX - Color X
  // [EDX] - Color Y
  // Result := W * (X - Y) + Y

        TEST      EAX,$FF000000
        JZ        @1
        CMP       EAX,$FF000000
        JNC       @2

        db $0F,$EF,$DB           /// PXOR      MM3,MM3
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$12           /// MOVD      MM2,[EDX]
        db $0F,$60,$C3           /// PUNPCKLBW MM0,MM3
        MOV       ECX,bias_ptr
        db $0F,$60,$D3           /// PUNPCKLBW MM2,MM3
        db $0F,$6F,$C8           /// MOVQ      MM1,MM0
        db $0F,$69,$C9           /// PUNPCKHWD MM1,MM1
        db $0F,$F9,$C2           /// PSUBW     MM0,MM2
        db $0F,$6A,$C9           /// PUNPCKHDQ MM1,MM1
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        db $0F,$D5,$C1           /// PMULLW    MM0,MM1
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$D0           /// PADDW     MM2,MM0
        db $0F,$71,$D2,$08       /// PSRLW     MM2,8
        db $0F,$67,$D3           /// PACKUSWB  MM2,MM3
        db $0F,$7E,$12           /// MOVD      [EDX],MM2
@1:     RET

@2:     MOV       [EDX],EAX
end;

function M_BlendRegEx(F, B, M: TColor32): TColor32;
asm
  // blend foregrownd color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // EDX <- B
  // ECX <- M
  // Result := M * Fa * (Frgb - Brgb) + Brgb
        PUSH      EBX
        MOV       EBX,EAX
        SHR       EBX,24
        INC       ECX             // 255:256 range bias
        IMUL      ECX,EBX
        SHR       ECX,8
        JZ        @1

        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        db $0F,$6E,$C8           /// MOVD      MM1,EAX
        SHL       ECX,3
        db $0F,$6E,$D2           /// MOVD      MM2,EDX
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0
        ADD       ECX,alpha_ptr
        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$09           /// PMULLW    MM1,[ECX]
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        MOV       ECX,bias_ptr
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$C8           /// MOVD      EAX,MM1

        POP       EBX
        RET

@1:     MOV       EAX,EDX
        POP       EBX
end;

procedure M_BlendMemEx(F: TColor32; var B:TColor32; M: TColor32);
asm
  // blend foregrownd color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // [EDX] <- B
  // ECX <- M
  // Result := M * Fa * (Frgb - Brgb) + Brgb
        TEST      EAX,$FF000000
        JZ        @2

        PUSH      EBX
        MOV       EBX,EAX
        SHR       EBX,24
        INC       ECX             // 255:256 range bias
        IMUL      ECX,EBX
        SHR       ECX,8
        JZ        @1

        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        db $0F,$6E,$C8           /// MOVD      MM1,EAX
        SHL       ECX,3
        db $0F,$6E,$12           /// MOVD      MM2,[EDX]
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0
        ADD       ECX,alpha_ptr
        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$09           /// PMULLW    MM1,[ECX]
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        MOV       ECX,bias_ptr
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$0A           /// MOVD      [EDX],MM1
@1:     POP       EBX
@2:
end;

procedure M_BlendLine(Src, Dst: PColor32; Count: Integer);
asm
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // test the counter for zero or negativity
        TEST      ECX,ECX
        JS        @4

        PUSH      ESI
        PUSH      EDI

        MOV       ESI,EAX         // ESI <- Src
        MOV       EDI,EDX         // EDI <- Dst

  // loop start
@1:     MOV       EAX,[ESI]
        TEST      EAX,$FF000000
        JZ        @3              // complete transparency, proceed to next point
        CMP       EAX,$FF000000
        JNC       @2              // opaque pixel, copy without blending

  // blend
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$EF,$DB           /// PXOR      MM3,MM3
        db $0F,$6E,$17           /// MOVD      MM2,[EDI]
        db $0F,$60,$C3           /// PUNPCKLBW MM0,MM3
        MOV       EAX,bias_ptr
        db $0F,$60,$D3           /// PUNPCKLBW MM2,MM3
        db $0F,$6F,$C8           /// MOVQ      MM1,MM0
        db $0F,$69,$C9           /// PUNPCKHWD MM1,MM1
        db $0F,$F9,$C2           /// PSUBW     MM0,MM2
        db $0F,$6A,$C9           /// PUNPCKHDQ MM1,MM1
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        db $0F,$D5,$C1           /// PMULLW    MM0,MM1
        db $0F,$FD,$10           /// PADDW     MM2,[EAX]
        db $0F,$FD,$D0           /// PADDW     MM2,MM0
        db $0F,$71,$D2,$08       /// PSRLW     MM2,8
        db $0F,$67,$D3           /// PACKUSWB  MM2,MM3
        db $0F,$7E,$D0           /// MOVD      EAX,MM2

@2:     MOV       [EDI],EAX

@3:     ADD       ESI,4
        ADD       EDI,4

  // loop end
        DEC       ECX
        JNZ       @1

        POP       EDI
        POP       ESI

@4:     RET
end;

procedure M_BlendLineEx(Src, Dst: PColor32; Count: Integer; M: TColor32);
asm
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // test the counter for zero or negativity
        TEST      ECX,ECX
        JS        @4

        PUSH      ESI
        PUSH      EDI
        PUSH      EBX

        MOV       ESI,EAX         // ESI <- Src
        MOV       EDI,EDX         // EDI <- Dst
        MOV       EDX,M           // EDX <- Master Alpha

  // loop start
@1:     MOV       EAX,[ESI]
        TEST      EAX,$FF000000
        JZ        @3             // complete transparency, proceed to next point
        MOV       EBX,EAX
        SHR       EBX,24
        INC       EBX            // 255:256 range bias
        IMUL      EBX,EDX
        SHR       EBX,8
        JZ        @3              // complete transparency, proceed to next point

  // blend
        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        db $0F,$6E,$C8           /// MOVD      MM1,EAX
        SHL       EBX,3
        db $0F,$6E,$17           /// MOVD      MM2,[EDI]
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0
        ADD       EBX,alpha_ptr
        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$0B           /// PMULLW    MM1,[EBX]
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        MOV       EBX,bias_ptr
        db $0F,$FD,$13           /// PADDW     MM2,[EBX]
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$C8           /// MOVD      EAX,MM1

@2:     MOV       [EDI],EAX

@3:     ADD       ESI,4
        ADD       EDI,4

  // loop end
        DEC       ECX
        JNZ       @1

        POP       EBX
        POP       EDI
        POP       ESI
@4:
end;

procedure M_CombineLine(Src, Dst: PColor32; Count: Integer; W: TColor32);
asm
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // Result := W * (X - Y) + Y

        TEST      ECX,ECX
        JS        @3

        PUSH      EBX
        MOV       EBX,W

        TEST      EBX,EBX
        JZ        @2              // weight is zero

        CMP       EDX,$FF
        JZ        @4              // weight = 255  =>  copy src to dst

        SHL       EBX,3
        ADD       EBX,alpha_ptr
        db $0F,$6F,$1B           /// MOVQ      MM3,[EBX]
        MOV       EBX,bias_ptr
        db $0F,$6F,$23           /// MOVQ      MM4,[EBX]

   // loop start
@1:     db $0F,$6E,$08           /// MOVD      MM1,[EAX]
        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        db $0F,$6E,$12           /// MOVD      MM2,[EDX]
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0

        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$CB           /// PMULLW    MM1,MM3
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8

        db $0F,$FD,$D4           /// PADDW     MM2,MM4
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$0A           /// MOVD      [EDX],MM1

        ADD       EAX,4
        ADD       EDX,4

        DEC       ECX
        JNZ       @1
@2:     POP       EBX
        POP       EBP
@3:     RET       $0004

@4:     CALL      GR32_LowLevel.MoveLongword
        POP       EBX
end;

{ Non-MMX Color algebra versions }

function _ColorAdd(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: Integer;
  r2, g2, b2, a2: Integer;
begin
  a1 := C1 shr 24;
  r1 := C1 and $00FF0000;
  g1 := C1 and $0000FF00;
  b1 := C1 and $000000FF;

  a2 := C2 shr 24;
  r2 := C2 and $00FF0000;
  g2 := C2 and $0000FF00;
  b2 := C2 and $000000FF;

  a1 := a1 + a2;
  r1 := r1 + r2;
  g1 := g1 + g2;
  b1 := b1 + b2;

  if a1 > $FF then a1 := $FF;
  if r1 > $FF0000 then r1 := $FF0000;
  if g1 > $FF00 then g1 := $FF00;
  if b1 > $FF then b1 := $FF;

  Result := a1 shl 24 + r1 + g1 + b1;
end;

function _ColorSub(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: Integer;
  r2, g2, b2, a2: Integer;
begin
  a1 := C1 shr 24;
  r1 := C1 and $00FF0000;
  g1 := C1 and $0000FF00;
  b1 := C1 and $000000FF;

  r1 := r1 shr 16;
  g1 := g1 shr 8;

  a2 := C2 shr 24;
  r2 := C2 and $00FF0000;
  g2 := C2 and $0000FF00;
  b2 := C2 and $000000FF;

  r2 := r2 shr 16;
  g2 := g2 shr 8;

  a1 := a1 - a2;
  r1 := r1 - r2;
  g1 := g1 - g2;
  b1 := b1 - b2;

  if a1 < 0 then a1 := 0;
  if r1 < 0 then r1 := 0;
  if g1 < 0 then g1 := 0;
  if b1 < 0 then b1 := 0;

  Result := a1 shl 24 + r1 shl 16 + g1 shl 8 + b1;
end;

function _ColorDiv(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: Integer;
  r2, g2, b2, a2: Integer;
begin
  a1 := C1 shr 24;
  r1 := (C1 and $00FF0000) shr 16;
  g1 := (C1 and $0000FF00) shr 8;
  b1 := C1 and $000000FF;

  a2 := C2 shr 24;
  r2 := (C2 and $00FF0000) shr 16;
  g2 := (C2 and $0000FF00) shr 8;
  b2 := C2 and $000000FF;

  if a1 = 0 then a1:=$FF
  else a1 := (a2 shl 8) div a1;
  if r1 = 0 then r1:=$FF
  else r1 := (r2 shl 8) div r1;
  if g1 = 0 then g1:=$FF
  else g1 := (g2 shl 8) div g1;
  if b1 = 0 then b1:=$FF
  else b1 := (b2 shl 8) div b1;

  if a1 > $FF then a1 := $FF;
  if r1 > $FF then r1 := $FF;
  if g1 > $FF then g1 := $FF;
  if b1 > $FF then b1 := $FF;

  Result := a1 shl 24 + r1 shl 16 + g1 shl 8 + b1;
end;

function _ColorModulate(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: Integer;
  r2, g2, b2, a2: Integer;
begin
  a1 := C1 shr 24;
  r1 := C1 and $00FF0000;
  g1 := C1 and $0000FF00;
  b1 := C1 and $000000FF;

  r1 := r1 shr 16;
  g1 := g1 shr 8;

  a2 := C2 shr 24;
  r2 := C2 and $00FF0000;
  g2 := C2 and $0000FF00;
  b2 := C2 and $000000FF;

  r2 := r2 shr 16;
  g2 := g2 shr 8;

  a1 := a1 * a2 shr 8;
  r1 := r1 * r2 shr 8;
  g1 := g1 * g2 shr 8;
  b1 := b1 * b2 shr 8;

  if a1 > 255 then a1 := 255;
  if r1 > 255 then r1 := 255;
  if g1 > 255 then g1 := 255;
  if b1 > 255 then b1 := 255;

  Result := a1 shl 24 + r1 shl 16 + g1 shl 8 + b1;
end;

function _ColorMax(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: TColor32;
  r2, g2, b2, a2: TColor32;
begin
  a1 := C1 shr 24;
  r1 := C1 and $00FF0000;
  g1 := C1 and $0000FF00;
  b1 := C1 and $000000FF;

  a2 := C2 shr 24;
  r2 := C2 and $00FF0000;
  g2 := C2 and $0000FF00;
  b2 := C2 and $000000FF;

  if a2 > a1 then a1 := a2;
  if r2 > r1 then r1 := r2;
  if g2 > g1 then g1 := g2;
  if b2 > b1 then b1 := b2;

  Result := a1 shl 24 + r1 + g1 + b1;
end;

function _ColorMin(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: TColor32;
  r2, g2, b2, a2: TColor32;
begin
  a1 := C1 shr 24;
  r1 := C1 and $00FF0000;
  g1 := C1 and $0000FF00;
  b1 := C1 and $000000FF;

  a2 := C2 shr 24;
  r2 := C2 and $00FF0000;
  g2 := C2 and $0000FF00;
  b2 := C2 and $000000FF;

  if a2 < a1 then a1 := a2;
  if r2 < r1 then r1 := r2;
  if g2 < g1 then g1 := g2;
  if b2 < b1 then b1 := b2;

  Result := a1 shl 24 + r1 + g1 + b1;
end;

function _ColorDifference(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: TColor32;
  r2, g2, b2, a2: TColor32;
begin
  a1 := C1 shr 24;
  r1 := C1 and $00FF0000;
  g1 := C1 and $0000FF00;
  b1 := C1 and $000000FF;

  r1 := r1 shr 16;
  g1 := g1 shr 8;

  a2 := C2 shr 24;
  r2 := C2 and $00FF0000;
  g2 := C2 and $0000FF00;
  b2 := C2 and $000000FF;

  r2 := r2 shr 16;
  g2 := g2 shr 8;

  a1 := abs(a2 - a1);
  r1 := abs(r2 - r1);
  g1 := abs(g2 - g1);
  b1 := abs(b2 - b1);

  Result := a1 shl 24 + r1 shl 16 + g1 shl 8 + b1;
end;

function _ColorExclusion(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: TColor32;
  r2, g2, b2, a2: TColor32;
begin
  a1 := C1 shr 24;
  r1 := C1 and $00FF0000;
  g1 := C1 and $0000FF00;
  b1 := C1 and $000000FF;

  r1 := r1 shr 16;
  g1 := g1 shr 8;

  a2 := C2 shr 24;
  r2 := C2 and $00FF0000;
  g2 := C2 and $0000FF00;
  b2 := C2 and $000000FF;

  r2 := r2 shr 16;
  g2 := g2 shr 8;

  a1 := a1 + a2 - (a1 * a2 shr 7);
  r1 := r1 + r2 - (r1 * r2 shr 7);
  g1 := g1 + g2 - (g1 * g2 shr 7);
  b1 := b1 + b2 - (b1 * b2 shr 7);

  Result := a1 shl 24 + r1 shl 16 + g1 shl 8 + b1;
end;

function _ColorAverage(C1, C2: TColor32): TColor32;
var
  r1, g1, b1, a1: TColor32;
  r2, g2, b2, a2: TColor32;
begin
  a1 := C1 shr 24;
  r1 := C1 and $00FF0000;
  g1 := C1 and $0000FF00;
  b1 := C1 and $000000FF;

  a2 := C2 shr 24;
  r2 := C2 and $00FF0000;
  g2 := C2 and $0000FF00;
  b2 := C2 and $000000FF;

  a1 := (a1 + a2) div 2;
  r1 := (r1 + r2) div 2;
  g1 := (g1 + g2) div 2;
  b1 := (b1 + b2) div 2;

  Result := a1 shl 24 + r1 + g1 + b1;
end;

function _ColorScale(C, W: TColor32): TColor32;
var
  r1, g1, b1, a1: Cardinal;
begin
  a1 := C shr 24;
  r1 := C and $00FF0000;
  g1 := C and $0000FF00;
  b1 := C and $000000FF;

  r1 := r1 shr 16;
  g1 := g1 shr 8;

  a1 := a1 * W shr 8;
  r1 := r1 * W shr 8;
  g1 := g1 * W shr 8;
  b1 := b1 * W shr 8;

  if a1 > 255 then a1 := 255;
  if r1 > 255 then r1 := 255;
  if g1 > 255 then g1 := 255;
  if b1 > 255 then b1 := 255;

  Result := a1 shl 24 + r1 shl 16 + g1 shl 8 + b1;
end;

{ MMX Color algebra versions }

function M_ColorAdd(C1, C2: TColor32): TColor32;
asm
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$DC,$C1           /// PADDUSB   MM0,MM1
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

function M_ColorSub(C1, C2: TColor32): TColor32;
asm
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$D8,$C1           /// PSUBUSB   MM0,MM1
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

function M_ColorModulate(C1, C2: TColor32): TColor32;
asm
        db $0F,$EF,$D2           /// PXOR      MM2,MM2
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$60,$C2           /// PUNPCKLBW MM0,MM2
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$60,$CA           /// PUNPCKLBW MM1,MM2
        db $0F,$D5,$C1           /// PMULLW    MM0,MM1
        db $0F,$71,$D0,$08       /// PSRLW     MM0,8
        db $0F,$67,$C2           /// PACKUSWB  MM0,MM2
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

function M_ColorMax(C1, C2: TColor32): TColor32;
asm
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$DE,$C1           /// PMAXUB    MM0,MM1
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

function M_ColorMin(C1, C2: TColor32): TColor32;
asm
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$DA,$C1           /// PMINUB    MM0,MM1
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;


function M_ColorDifference(C1, C2: TColor32): TColor32;
asm
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$6F,$D0           /// MOVQ      MM2,MM0
        db $0F,$D8,$C1           /// PSUBUSB   MM0,MM1
        db $0F,$D8,$CA           /// PSUBUSB   MM1,MM2
        db $0F,$EB,$C1           /// POR       MM0,MM1
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

function M_ColorExclusion(C1, C2: TColor32): TColor32;
asm
        db $0F,$EF,$D2           /// PXOR      MM2,MM2
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$60,$C2           /// PUNPCKLBW MM0,MM2
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$60,$CA           /// PUNPCKLBW MM1,MM2
        db $0F,$6F,$D8           /// MOVQ      MM3,MM0
        db $0F,$FD,$C1           /// PADDW     MM0,MM1
        db $0F,$D5,$CB           /// PMULLW    MM1,MM3
        db $0F,$71,$D1,$07       /// PSRLW     MM1,7
        db $0F,$D9,$C1           /// PSUBUSW   MM0,MM1
        db $0F,$67,$C2           /// PACKUSWB  MM0,MM2
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;


function M_ColorAverage(C1, C2: TColor32): TColor32;
asm
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$CA           /// MOVD      MM1,EDX
        db $0F,$E0,$C1           /// PAVGB     MM0,MM1
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;

function M_ColorScale(C, W: TColor32): TColor32;
asm
        db $0F,$EF,$D2           /// PXOR      MM2,MM2
        SHL       EDX,3
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$60,$C2           /// PUNPCKLBW MM0,MM2
        ADD       EDX,alpha_ptr
        db $0F,$D5,$02           /// PMULLW    MM0,[EDX]
        db $0F,$71,$D0,$08       /// PSRLW     MM0,8
        db $0F,$67,$C2           /// PACKUSWB  MM0,MM2
        db $0F,$7E,$C0           /// MOVD      EAX,MM0
end;


{ Misc stuff }

function Lighten(C: TColor32; Amount: Integer): TColor32;
var
  r, g, b, a: Integer;
begin
  a := C shr 24;
  r := C and $00FF0000;
  g := C and $0000FF00;
  b := C and $000000FF;

  r := r shr 16;
  g := g shr 8;

  Inc(r, Amount);
  Inc(g, Amount);
  Inc(b, Amount);

  if r > 255 then r := 255 else if r < 0 then r := 0;
  if g > 255 then g := 255 else if g < 0 then g := 0;
  if b > 255 then b := 255 else if b < 0 then b := 0;

  Result := a shl 24 + r shl 16 + g shl 8 + b;
end;

procedure MakeMergeTables;
var
  I, J: Integer;
begin
  for J := 0 to 255 do
    for I := 0 to 255 do
    begin
      DivTable[I, J] := Round(I * J / 255);
      if I > 0 then
        RcTable[I, J] := Round(J * 255 / I)
      else
        RcTable[I, J] := 0;
    end;
end;

{ MMX Detection and linking }

procedure SetupFunctions;
begin
  MMX_ACTIVE := HasMMX;

  MergeReg := _MergeReg;
  MergeMem := _MergeMem;
  MergeRegEx := _MergeRegEx;
  MergeMemEx := _MergeMemEx;
  MergeLine := _MergeLine;
  MergeLineEx := _MergeLineEx;

  BLEND_MEM[cmMerge] := _MergeMem;
  BLEND_REG[cmMerge] := _MergeReg;
  BLEND_MEM_EX[cmMerge] := _MergeMemEx;
  BLEND_REG_EX[cmMerge] := _MergeRegEx;
  BLEND_LINE[cmMerge] := _MergeLine;
  BLEND_LINE_EX[cmMerge] := _MergeLineEx;

{$IFNDEF DISABLE_MMX}
  if MMX_ACTIVE then
  begin
    // link MMX functions
    CombineReg := M_CombineReg;
    CombineMem := M_CombineMem;
    BlendReg := M_BlendReg;
    BlendMem := M_BlendMem;
    BlendRegEx := M_BlendRegEx;
    BlendMemEx := M_BlendMemEx;
    BlendLine := M_BlendLine;
    BlendLineEx := M_BlendLineEx;
    CombineLine := M_CombineLine;

    BLEND_MEM[cmBlend] := M_BlendMem;
    BLEND_REG[cmBlend] := M_BlendReg;
    BLEND_MEM_EX[cmBlend] := M_BlendMemEx;
    BLEND_REG_EX[cmBlend] := M_BlendRegEx;
    BLEND_LINE[cmBlend] := M_BlendLine;
    BLEND_LINE_EX[cmBlend] := M_BlendLineEx;

    ColorAdd := M_ColorAdd;
    ColorSub := M_ColorSub;
    ColorDiv := _ColorDiv;
    ColorModulate := M_ColorModulate;
    ColorMax := M_ColorMax;
    ColorMin := M_ColorMin;
    ColorDifference := M_ColorDifference;
    ColorExclusion := M_ColorExclusion;
    ColorAverage := M_ColorAverage;
    ColorScale := M_ColorScale;
  end
  else
  begin
{$ENDIF}
    // link non-MMX functions
    CombineReg := _CombineReg;
    CombineMem := _CombineMem;
    BlendReg := _BlendReg;
    BlendMem := _BlendMem;
    BlendRegEx := _BlendRegEx;
    BlendMemEx := _BlendMemEx;
    BlendLine := _BlendLine;
    BlendLineEx := _BlendLineEx;
    CombineLine := _CombineLine;

    BLEND_MEM[cmBlend] := _BlendMem;
    BLEND_REG[cmBlend] := _BlendReg;
    BLEND_MEM_EX[cmBlend] := _BlendMemEx;
    BLEND_REG_EX[cmBlend] := _BlendRegEx;
    BLEND_LINE[cmBlend] := _BlendLine;
    BLEND_LINE_EX[cmBlend] := _BlendLineEx;

    ColorAdd := _ColorAdd;
    ColorSub := _ColorSub;
    ColorDiv := _ColorDiv;
    ColorModulate := _ColorModulate;
    ColorMax := _ColorMax;
    ColorMin := _ColorMin;
    ColorDifference := _ColorDifference;
    ColorExclusion := _ColorExclusion;
    ColorAverage := _ColorAverage;
    ColorScale := _ColorScale;    
{$IFNDEF DISABLE_MMX}
  end;
{$ENDIF}
end;

initialization
  MakeMergeTables;
  SetupFunctions;
  if MMX_ACTIVE then GenAlphaTable;

finalization
  if MMX_ACTIVE then FreeAlphaTable;

end.



