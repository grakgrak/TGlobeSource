// Summary
//	TGlobe Expression Parser Classes
//
// Description
// 	Provides an interpreter for expressions for TGlobe Presenters
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//

unit GExpressionParser;

interface

uses Classes, SysUtils, Contnrs, GSysUtils;

type
  TGInterpreter = class;

  TGTokenType = (ttChar, ttString, ttOperator, ttSymbol, ttResolved, ttNumber, ttNewLine, ttError, ttEOF );

  TGToken = object
    TokType : TGTokenType;
    TextValue : String;
    Value : double;
    Ptr : Pointer;
    procedure Define( ttype : TGTokenType; text: String ); overload;
    procedure Define( ttype : TGTokenType; val : double ); overload;
    function AsValue( defValue : Double ) : Double;
    function AsText : String;
  end;
  TGTokenPtr = ^TGToken;
  TGTokenArray = array of TGToken;

  TGTokenStack = class
    FTokens : array of TGToken;
    FStackTop : integer;

    constructor Create;
    function IsEmpty : Boolean;
    function Pop : TGToken;
    procedure Push( tokPtr: TGTokenPtr );
    function Top : TGTokenPtr;
  end;

  TGLexer = class
  private
    FSource : String;
    SrcPtr : PChar;

    function StateSymbol( bufPtr : PChar ) : TGTokenType;
    function StateNumeric( bufPtr : PChar ) : TGTokenType;
    function StateString( bufPtr : PChar ) : TGTokenType;
    function StateOperator( bufPtr : PChar ) : TGTokenType;
    function StateChar( bufPtr : PChar ) : TGTokenType;
  public
    constructor Create( const source : string );

    function NextToken( var token : TGToken ) : TGTokenType;
    property Source : String read FSource;
  end;

  TGExpression = class
  private
    FSource : String;
    FMethods : TStringList;

    procedure Emit( var token : TGToken );
    function Precedence( const token : TGToken ) : integer;
    procedure ProcessOperator( const token : TGToken; var opStack : TGTokenStack );
  public
    CompiledTokens : TGTokenArray;
    Compiled : Boolean;
    Invalid : Boolean;   // Indicates if the expression is Invalid
    Running : Boolean;   // Used to block recursion

    constructor Create( const source : String );
    destructor Destroy; override;

    procedure AddMethod( const name : String; argCount : integer );
    function Compile : Boolean;
  end;

  TInterpMethod = procedure( Interp : TGInterpreter; tokPtr : TGTokenPtr; var stack : TGTokenStack );
  TGInterpSymbolEvent = function( Interp : TGInterpreter; tokPtr : TGTokenPtr; var stack : TGTokenStack ): Boolean of object;

  TGInterpreter = class( TGRoot )
  private
    FOnSymbol: TGInterpSymbolEvent;
    FExpressionList : TObjectList;
    FSymbols: TStringList;
    FExitExecute : boolean;
    FBreakExecute : boolean;
  protected
    function DoSymbolHandler(symbol : TGTokenPtr; var stack : TGTokenStack): Boolean; virtual;
  public
    Result : TGToken;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure AddExpression( const source : String );

    function Execute : Boolean;
    function ExecuteExpression( expr : TGExpression ) : Boolean;

    property Symbols : TStringList read FSymbols;
  published
    // Basic infix operators
    procedure Func_Add(symbol : TGTokenPtr; var stack : TGTokenStack);
    procedure Func_Div(symbol : TGTokenPtr; var stack : TGTokenStack);
    procedure Func_Mul(symbol : TGTokenPtr; var stack : TGTokenStack);
    procedure Func_Sub(symbol : TGTokenPtr; var stack : TGTokenStack);
    procedure Func_Eq(symbol : TGTokenPtr; var stack : TGTokenStack);
    procedure Func_Neq(symbol : TGTokenPtr; var stack : TGTokenStack);
    procedure Func_Lt(symbol : TGTokenPtr; var stack : TGTokenStack);
    procedure Func_Lte(symbol : TGTokenPtr; var stack : TGTokenStack);
    procedure Func_Gt(symbol : TGTokenPtr; var stack : TGTokenStack);
    procedure Func_Gte(symbol : TGTokenPtr; var stack : TGTokenStack);
    procedure Func_And(symbol : TGTokenPtr; var stack : TGTokenStack);

    // General functions
    procedure FuncIF(symbol : TGTokenPtr; var stack : TGTokenStack);
    procedure FuncExit(symbol : TGTokenPtr; var stack : TGTokenStack);
    procedure FuncBreak(symbol : TGTokenPtr; var stack : TGTokenStack);

    property OnSymbol : TGInterpSymbolEvent read FOnSymbol write FOnSymbol;
  end;

implementation
type
  TGLexerState = ( ssWhiteSpace, ssAlpha, ssNumeric, ssChar );

function GetState( chr : Char ) : TGLexerState;
begin
  case chr of
  #1..' ':
    Result := ssWhiteSpace;
  '_','$','.':
    Result := ssAlpha;
  'a'..'z':
    Result := ssAlpha;
  'A'..'Z':
    Result := ssAlpha;
  '0'..'9':
    Result := ssNumeric;
  else
    Result := ssChar;
  end;
end;

{ TGToken }

procedure TGToken.Define(ttype: TGTokenType; text: String);
begin
  TokType := ttype;
  TextValue := text;
  if TokType = ttNumber then
  begin
    Value := StrToExtendedDef( text, NaN );
    if IsNan( Value ) then
      TokType := ttString;
  end;
end;

procedure TGToken.Define(ttype: TGTokenType; val: double);
begin
  TokType := ttype;
  Value := val;
end;

function TGToken.AsValue( defValue : Double ): Double;
begin
  if TokType = ttNumber then
    Result := Value
  else
    Result := StrToExtendedDef( TextValue, defValue )
end;

function TGToken.AsText: String;
begin
  if TokType = ttNumber then
    Result := FloatToStr(Value)
  else
    Result := TextValue;
end;

{ TGTokenStack }

constructor TGTokenStack.Create;
begin
  FStackTop := -1;
end;

function TGTokenStack.IsEmpty: Boolean;
begin
  Result := FStackTop < 0;
end;

function TGTokenStack.Pop : TGToken;
begin
  if IsEmpty then
    raise EGException.Create( 'Stack Underflow' );
    
  Result := FTokens[FStackTop];
  Dec( FStackTop );
end;

procedure TGTokenStack.Push(tokPtr: TGTokenPtr);
begin
  Inc( FStackTop );

  if FStackTop > High( FTokens ) then
    SetLength( FTokens, Length( FTokens ) + 16 );

  FTokens[FStackTop] := tokPtr^;
end;

function TGTokenStack.Top: TGTokenPtr;
begin
  if not IsEmpty then
    Result := @FTokens[FStackTop]
  else
    Result := nil;
end;

{ TGLexer }

constructor TGLexer.Create(const source: string);
begin
  FSource := source;
  SrcPtr := PChar( FSource );
end;

function TGLexer.NextToken(var token: TGToken): TGTokenType;
var
  buf : array [0..2048] of char;
  bufPtr : PChar;
begin
  // init the Token
  token.TokType := ttEOF;
  token.Value := 0.0;

  bufPtr := buf;

  while SrcPtr^ <> #0 do
  begin
    case GetState( SrcPtr^ ) of
    ssWhiteSpace:
      begin
      end;
    ssAlpha:
      begin
        token.TokType := StateSymbol( bufPtr );
        break;  // break out of loop
      end;
    ssNumeric:
      begin
        token.TokType := StateNumeric( bufPtr );  // Must be done like this so that buf is correct
        token.Define( token.TokType, buf );
        break;  // break out of loop
      end;
    ssChar:
      begin
        token.TokType := StateChar( bufPtr );
        break;  // break out of loop
      end;
    end;
    Inc( SrcPtr );
  end;

  token.TextValue := buf;

  Result := token.TokType;
end;

function TGLexer.StateChar(bufPtr: PChar): TGTokenType;
begin
  // check for the start of a string
  if SrcPtr^ = '"' then
    Result := StateString( bufPtr )
  else
    Result := StateOperator( bufPtr );
end;

function TGLexer.StateNumeric(bufPtr: PChar): TGTokenType;
begin
  while SrcPtr^ <> #0 do
  begin
    bufPtr^ := SrcPtr^;
    Inc( bufPtr );
    Inc( SrcPtr );

    case GetState( SrcPtr^ ) of
    ssNumeric:
      begin
      end;
    ssChar:
      begin
        if SrcPtr^ <> '.' then // allow floating point
          break;
      end;
    ssWhiteSpace, ssAlpha:
      begin
        if SrcPtr^ <> '$' then // allow hex values
          break;
      end;
    end;
  end;
  bufPtr^ := #0;
  Result := ttNumber;
end;

function TGLexer.StateOperator(bufPtr: PChar): TGTokenType;
begin
  Result := ttChar;
  bufPtr^ := SrcPtr^;
  Inc( SrcPtr );

  case bufPtr^ of
  ':','<', '>':
    begin
      if ( SrcPtr^ = '=' ) or ( SrcPtr^ = '>' ) then
      begin
        Inc( bufPtr );
        bufPtr^ := SrcPtr^;
        Inc( SrcPtr );
      end;
      Result := ttOperator;
    end;
  ',', '(', ')', '[', ']','+', '-', '*', '/', '=', '&', '|' :
    Result := ttOperator;
  end;

  Inc( bufPtr );
  bufPtr^ := #0;
end;

function TGLexer.StateString(bufPtr: PChar): TGTokenType;
begin
  while SrcPtr^ <> #0 do
  begin
    if SrcPtr^ = '\' then  // the escape char
    begin
      Inc( SrcPtr );
      if SrcPtr^ = #0 then
        break;
      Inc( SrcPtr );  // skip the character after the escape char
      continue;
    end;

    if SrcPtr^ = '"' then
      Inc( SrcPtr )
    else
    begin
      bufPtr^ := SrcPtr^;
      Inc( bufPtr );
      Inc( SrcPtr );
    end;

    case GetState( SrcPtr^ ) of
    ssChar:
      if SrcPtr^ = '"' then
      begin
        Inc( SrcPtr );
        bufPtr^ := #0;
        Result := ttString;
        Exit;
      end;
    ssWhiteSpace, ssAlpha, ssNumeric:
      begin
      end;
    end;
  end;

  Result := ttError;
end;

function TGLexer.StateSymbol(bufPtr: PChar): TGTokenType;
begin
  while SrcPtr^ <> #0 do
  begin
    bufPtr^ := SrcPtr^;
    Inc( bufPtr );
    Inc( SrcPtr );

    case GetState( SrcPtr^ ) of
    ssChar, ssWhiteSpace:
      break;
    end;
  end;
  bufPtr^ := #0;
  Result := ttSymbol;
end;

{ TGExpression }

function TGExpression.Compile : Boolean;
var
  token : TGToken;
  opStack : TGTokenStack;
  bracketCount : integer;
  lexer : TGLexer;
begin
  if not Compiled then
  begin
    lexer := TGLexer.Create( FSource );
    opStack := TGTokenStack.Create;

    bracketCount := 0;
    try
      try
        while lexer.NextToken( token ) <> ttEOF do
        begin
          case token.TokType of
          ttSymbol:
            begin
              token.Value := 0;
              ProcessOperator( token, opStack );
            end;
          ttOperator:
            begin
              if token.TextValue = ':=' then
              begin
                token.TextValue := '(';
                Inc( bracketCount );
                ProcessOperator( token, opStack );
              end;

              case token.TextValue[1] of
              ',':
                begin // assumes commas only used for function args
                  token.TextValue := ')';
                  ProcessOperator( token, opStack );  // Close first param
                  token.TextValue := '('; // Open next param
                end;
              '(','[':
                begin
                  Inc( bracketCount );
                  token.TextValue := '(';
                end;
              ')',']':
                begin
                  Dec( bracketCount );
                  token.TextValue := ')';
                end;
              end;

              if bracketCount < 0 then
                raise EGException.Create( 'Mismatched Brackets in ' + lexer.Source );

              ProcessOperator( token, opStack );  // Close first param
            end;
          ttChar, ttString, ttNumber, ttError:
            Emit( token );
          end;
        end;

        // Called to flush the operator stack
        ProcessOperator( token, opStack );

        while bracketCount > 0 do // Auto close open brackets
        begin
          Dec( bracketCount );
          token.Define(ttOperator,')');
          ProcessOperator( token, opStack );
        end;
      except
        Invalid := true;
      end;
    finally
      opStack.Free;
      lexer.Free;
      Compiled := true;
    end;
  end;

  Result := not Invalid;
end;

procedure TGExpression.AddMethod( const name : String; argCount : integer );
begin
  FMethods.AddObject( Lowercase(name), TObject( argCount ));
end;

constructor TGExpression.Create( const source : String );
begin
  FMethods := TStringList.Create;
  FSource := source;
end;

procedure TGExpression.Emit( var token : TGToken );
var
  idx : integer;
begin
  token.Ptr := nil;

  if token.TokType = ttOperator then
  begin
    token.Value := 2;  // Default the number of arguments for the operator

    if token.TextValue = '+' then
      token.TextValue := '_add'
    else if token.TextValue = '-' then
      token.TextValue := '_sub'
    else if token.TextValue = '*' then
      token.TextValue := '_mul'
    else if token.TextValue = '/' then
      token.TextValue := '_div'
    else if token.TextValue = '=' then
      token.TextValue := '_eq'
    else if token.TextValue = '<' then
      token.TextValue := '_lt'
    else if token.TextValue = '<=' then
      token.TextValue := '_lte'
    else if token.TextValue = '>' then
      token.TextValue := '_gt'
    else if token.TextValue = '>=' then
      token.TextValue := '_gte'
    else if token.TextValue = '<>' then
      token.TextValue := '_neq'
    else if token.TextValue = '&' then
      token.TextValue := '_and'
    else if token.TextValue = '|' then
      token.TextValue := '_or';
  end;

  if token.TokType = ttSymbol then  // check the argument count for known methods
  begin
    idx := FMethods.IndexOf( Lowercase( token.TextValue ));
    if idx >= 0 then
    begin
      if token.Value < integer( FMethods.Objects[idx] ) then
        raise EGException.Create('Method ' + token.TextValue + ' called with too few arguments.' );
      if token.Value > integer( FMethods.Objects[idx] ) then
        raise EGException.Create('Method ' + token.TextValue + ' called with too many arguments.' );
    end;
  end;

  SetLength( CompiledTokens, Length( CompiledTokens ) + 1 );
  CompiledTokens[High(CompiledTokens)] := token;
end;

function TGExpression.Precedence( const token : TGToken ) : integer;
var
  text : String;
begin
  case token.TokType of
    ttSymbol, ttResolved:
      Result := 7;
    else
      begin
        text := Token.AsText;

        if ( text = '*' ) or ( text = '/' ) then
          Result := 6
        else if ( text = '+' ) or ( text = '-' ) then
          Result := 5
        else if ( text = '<' ) or ( text = '>' ) or ( text = '<=' ) or ( text = '>=' ) or ( text = '<>' ) or ( text = '=' ) then
          Result := 4
        else if ( text = '&' ) or ( text = '|' ) then
          Result := 3
        else if ( text = '(' ) or ( text = ')' ) or ( text = '[' ) or ( text = ']' ) then
          Result := 1
        else if ( text = ':=' ) then
          Result := 2
        else
          Result := 0;
      end;
  end;
end;

procedure TGExpression.ProcessOperator( const token : TGToken; var opStack : TGTokenStack );
var
  top : TGToken;
begin
  if token.TextValue[1] <> '(' then
    while not opStack.IsEmpty do
    begin
      top := opStack.Top^;
      // Evaluate higher precedence first
      if Precedence( token ) > Precedence( top ) then
        break;

      opStack.Pop;
      if top.TextValue = '(' then  // reached end of bracketed ops
      begin
        // Update the argument count for function calls
        if ( not opStack.IsEmpty ) and ( opStack.Top.TokType = ttSymbol ) then
          opStack.Top.Value := opStack.Top.Value + 1;
        Exit;
      end;
      Emit( top );  // output the token
    end;

  if token.TextValue <> ')' then  // place operator on stack if not a ")"
    opStack.Push( @token );
end;

destructor TGExpression.Destroy;
begin
  FreeAndNil( FMethods );

  inherited;
end;

{ TGInterpreter }

procedure TGInterpreter.AddExpression(const source: String);
var
  expr : TGExpression;
begin
  if Trim( Source ) <> '' then
  begin
    expr := TGExpression.Create(source);

    expr.AddMethod( 'if', 3 );

    FExpressionList.Add(expr);
  end;
end;

procedure TGInterpreter.Clear;
begin
  FExpressionList.Clear;
end;

constructor TGInterpreter.Create;
begin
  FExpressionList := TObjectList.Create(true);
  FSymbols := TStringList.Create;
  FSymbols.Sorted := true;
end;

destructor TGInterpreter.Destroy;
begin
  FreeAndNil( FExpressionList );
  FreeAndNil( FSymbols );
  inherited;
end;

function TGInterpreter.DoSymbolHandler(symbol : TGTokenPtr; var stack : TGTokenStack): Boolean;
var
  methP : TInterpMethod;
begin
  Result := true;

  @methP := MethodAddress('Func' + symbol.TextValue);

  if Assigned( methP ) then
  begin
    symbol.TokType := ttResolved;
    symbol.Ptr := @methP;

    methP( Self, symbol, stack );
    Exit;
  end;

  if Assigned( FOnSymbol ) then
    if FOnSymbol( Self, symbol, stack ) then
      Exit;

  Result := false;
end;

function TGInterpreter.Execute: Boolean;
var
  idx : integer;
begin
  try
    for idx := 0 to FExpressionList.Count - 1 do
    begin
      ExecuteExpression( TGExpression( FExpressionList[idx] ));
      if FExitExecute then
        Break;
    end;

    Result := true;
  except
    Result := false;
  end;
end;

function TGInterpreter.ExecuteExpression(expr: TGExpression): Boolean;
var
  codePtr : TGTokenPtr;
  stack : TGTokenStack;
  idx : integer;
  methP : TInterpMethod;
begin
  Result := false;
  FExitExecute := false;
  FBreakExecute := false;

  if not expr.Compile then
    Exit;

  if expr.Invalid then
    Exit;

  if expr.Running then
  begin
    expr.Invalid := true;
    Self.Result.Define( ttString, 'RECURSIVE EXPRESSION');
    raise EGException.Create( 'Recursive Expressions are not supported.' );
  end;

  stack := TGTokenStack.Create;
  expr.Running := true;  // Used to detect recursive calls

  try
    for idx := 0 to High( expr.CompiledTokens ) do
      begin
        codePtr := @expr.CompiledTokens[idx];

        case codePtr.TokType of
        ttEOF, ttString, ttNumber:
          stack.Push( codePtr );
        ttResolved: // Call a known function
          if codePtr.Ptr <> nil then
          begin
            methP := codePtr.Ptr;
            methP( Self, codePtr, stack );
          end;
        ttOperator, ttSymbol: // call the external symbol handler
          if not DoSymbolHandler( codePtr, stack ) then
          begin
            codePtr.TokType := ttString;
            stack.Push(codePtr); // Push the symbol as a string literal
          end;
        end;
        if FExitExecute or FBreakExecute then
          Break;
      end;
    if stack.IsEmpty() then
      Self.Result.Define( ttString, 'NULL' )
    else
      Self.Result := stack.Top^;
    expr.Running := false;
    Result := true;
  except
    expr.Running := false;
    expr.Invalid := true;
    Self.Result.Define( ttString, '## EXPR? ##');
    raise EGException.Create( 'Expression Error' );
  end;
  stack.Free;
end;

procedure TGInterpreter.Func_Add(symbol : TGTokenPtr; var stack: TGTokenStack);
var
  right : TGToken;
  left : TGTokenPtr;
begin
  right := stack.Pop;
  left := stack.Top;  // Leave on the stack and use for result

  case left.TokType of
    ttNumber:
      left.Value := left.Value + right.AsValue(0.0);
    ttString:
      left.TextValue := left.AsText + right.AsText;
    else
      raise EGException.Create( 'Syntax Error: Cannot Add the supplied types' );
  end;
end;

procedure TGInterpreter.Func_Div(symbol: TGTokenPtr; var stack: TGTokenStack);
var
  right : TGToken;
  left : TGTokenPtr;
begin
  right := stack.Pop;
  left := stack.Top;  // Leave on the stack and use for result

  if left.TokType = ttNumber then
    left.Value := left.Value / right.AsValue(1.0)
  else
    raise EGException.Create( 'Syntax Error: Cannot Divide the supplied types' );
end;

procedure TGInterpreter.Func_Mul(symbol: TGTokenPtr; var stack: TGTokenStack);
var
  right : TGToken;
  left : TGTokenPtr;
begin
  right := stack.Pop;
  left := stack.Top;  // Leave on the stack and use for result

  if left.TokType = ttNumber then
    left.Value := left.Value * right.AsValue(0.0)
  else
    raise EGException.Create( 'Syntax Error: Cannot Multiply the supplied types' );
end;

procedure TGInterpreter.Func_Sub(symbol : TGTokenPtr; var stack: TGTokenStack);
var
  right : TGToken;
  left : TGTokenPtr;
begin
  right := stack.Pop;
  left := stack.Top;  // Leave on the stack and use for result

  if left.TokType = ttNumber then
    left.Value := left.Value - right.AsValue(0.0)
  else
    raise EGException.Create( 'Syntax Error: Cannot Subtract the supplied types' );
end;

procedure TGInterpreter.Func_Eq(symbol: TGTokenPtr; var stack: TGTokenStack);
var
  right : TGToken;
  left : TGTokenPtr;
begin
  right := stack.Pop;
  left := stack.Top;  // Leave on the stack and use for result

  if left.TokType = ttNumber then
    left.Define( ttNumber, Ord( left.AsValue(0.0) = right.AsValue(0.0)))
  else
    left.Define( ttNumber, Ord( left.AsText = right.AsText ));
end;

procedure TGInterpreter.Func_Neq(symbol: TGTokenPtr; var stack: TGTokenStack);
var
  right : TGToken;
  left : TGTokenPtr;
begin
  right := stack.Pop;
  left := stack.Top;  // Leave on the stack and use for result

  if left.TokType = ttNumber then
    left.Define( ttNumber, Ord( left.AsValue(0.0) <> right.AsValue(0.0)))
  else
    left.Define( ttNumber, Ord( left.AsText <> right.AsText));
end;

procedure TGInterpreter.Func_Gt(symbol: TGTokenPtr; var stack: TGTokenStack);
var
  right : TGToken;
  left : TGTokenPtr;
begin
  right := stack.Pop;
  left := stack.Top;  // Leave on the stack and use for result

  if left.TokType = ttNumber then
    left.Define( ttNumber, Ord( left.AsValue(0.0) > right.AsValue(0.0)))
  else
    left.Define( ttNumber, Ord( left.AsText > right.AsText));
end;

procedure TGInterpreter.Func_Gte(symbol: TGTokenPtr; var stack: TGTokenStack);
var
  right : TGToken;
  left : TGTokenPtr;
begin
  right := stack.Pop;
  left := stack.Top;  // Leave on the stack and use for result

  if left.TokType = ttNumber then
    left.Define( ttNumber, Ord( left.AsValue(0.0) >= right.AsValue(0.0)))
  else
    left.Define( ttNumber, Ord( left.AsText >= right.AsText));
end;

procedure TGInterpreter.Func_Lt(symbol: TGTokenPtr; var stack: TGTokenStack);
var
  right : TGToken;
  left : TGTokenPtr;
begin
  right := stack.Pop;
  left := stack.Top;  // Leave on the stack and use for result

  if left.TokType = ttNumber then
    left.Define( ttNumber, Ord( left.AsValue(0.0) < right.AsValue(0.0)))
  else
    left.Define( ttNumber, Ord( left.AsText < right.AsText));
end;

procedure TGInterpreter.Func_Lte(symbol: TGTokenPtr; var stack: TGTokenStack);
var
  right : TGToken;
  left : TGTokenPtr;
begin
  right := stack.Pop;
  left := stack.Top;  // Leave on the stack and use for result

  if left.TokType = ttNumber then
    left.Define( ttNumber, Ord( left.AsValue(0.0) <= right.AsValue(0.0)))
  else
    left.Define( ttNumber, Ord( left.AsText <= right.AsText));
end;

procedure TGInterpreter.Func_And(symbol : TGTokenPtr; var stack : TGTokenStack);
var
  right : TGToken;
  left : TGTokenPtr;
begin
  right := stack.Pop;
  left := stack.Top;  // Leave on the stack and use for result

  left.Define( ttNumber, Ord((left.AsValue(0.0) <> 0.0 ) and ( right.AsValue(0.0) <> 0.0 )));
end;

procedure TGInterpreter.FuncBreak(symbol : TGTokenPtr; var stack : TGTokenStack);
begin
  FBreakExecute := true;
end;

procedure TGInterpreter.FuncExit(symbol : TGTokenPtr; var stack : TGTokenStack);
begin
  FExitExecute := true;
end;

procedure TGInterpreter.FuncIF(symbol: TGTokenPtr; var stack: TGTokenStack);
var
  trueVal, elseVal : TGToken;
  test : TGTokenPtr;
begin
  elseVal := stack.Pop;
  trueVal := stack.Pop;

  test := stack.Top;  // Leave on the stack and use for result

  if test.AsValue(0.0) <> 0.0 then
    test^ := trueVal
  else
    test^ := elseVal;
end;

end.
