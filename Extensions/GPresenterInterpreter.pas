unit GPresenterInterpreter;

interface

Uses Graphics, SysUtils, GSysUtils, GClasses, GExpressionParser;

type
  TGPresenterInterpreter = class(TGInterpreter)
  private
    FPresenter : TGPresenter;
  protected
    function DoSymbolHandler(symbol : TGTokenPtr; var stack : TGTokenStack): Boolean; override;
  public
    constructor Create( aPresenter : TGPresenter ); virtual;
  end;

implementation

Uses GCustomPresenter, GPresenters;

{ TGPresenterInterpreter }

constructor TGPresenterInterpreter.Create( aPresenter : TGPresenter );
begin
  inherited Create;

  FPresenter := aPresenter;

  Symbols.AddObject('brush.color', TObject(1));
  Symbols.AddObject('pen.color', TObject(2));
  Symbols.AddObject('obj.attribute', TObject(3));
  Symbols.AddObject('obj.hidden', TObject(4));
  Symbols.AddObject('renderpass', TObject(5));
end;

function TGPresenterInterpreter.DoSymbolHandler(symbol: TGTokenPtr; var stack: TGTokenStack): Boolean;
var
  tok : TGToken;
  idx : integer;
  currentObj : IGMapPoint;
begin
  Result := Inherited DoSymbolHandler( symbol, stack );

  if not Result then
  begin
    idx := integer(symbol.Ptr); // check to see if the symbol has already been resolved

    if idx = 0 then
    begin
      idx := Symbols.IndexOf( lowercase( symbol.TextValue ));
      symbol.Ptr := Pointer( idx );
    end;

    Result := idx >= 0;
    if Result then
    begin
      currentObj := (FPresenter as TGCustomPresenter).CurrentObject;
      case integer( Symbols.Objects[idx]) of
      1 : // brush.color
        if symbol.Value = 1 then  // Write to the property
        begin
          tok := stack.Pop;
          FPresenter.ParentGlobe.Renderer.Brush.Color := StringToColor( tok.TextValue );
        end
        else
          tok.Define(ttString, ColorToString( FPresenter.ParentGlobe.Renderer.Brush.Color ));
      2 : // pen.color
        if symbol.Value = 1 then  // Write to the property
        begin
          tok := stack.Pop;
          FPresenter.ParentGlobe.Renderer.Pen.Color := StringToColor( tok.TextValue )
        end
        else
          tok.Define(ttString, ColorToString( FPresenter.ParentGlobe.Renderer.Pen.Color ));
      3 : // obj.Attribute[]
        if symbol.Value = 2 then  // Write to the property
        begin
          tok := stack.Pop; // Get value to write
          idx := Trunc(stack.Pop.AsValue(0.0)); // get indexer
          currentObj.Attribute[idx] := tok.AsText;
        end
        else
        begin
          idx := Trunc(stack.Pop.AsValue(0.0)); // get indexer
          tok.Define(ttString, currentObj.Attribute[idx]);
        end;
      4 : // obj.hidden
        if symbol.Value = 1 then  // Write to the property
        begin
          tok := stack.Pop;
          currentObj.Hidden := tok.AsValue(0.0) <> 0.0;
        end
        else
          tok.Define(ttNumber, Ord( currentObj.Hidden));
      5 : // RenderPass
        tok.Define(ttNumber, Ord( TGCustomPresenter( FPresenter ).RenderPass ));
      end;
      stack.Push( @tok );
    end;
  end;
end;

initialization
  RegisterGlobeClass( TGPresenterInterpreter, 'Presenter Interpreter' );
end.
