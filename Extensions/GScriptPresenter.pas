//-----------------------------------------------------------------------
// Summary
//	TGlobe Script driven Presenter
//
// Description
// 	Allows a script to be written which acts as a Presenter
//
// Author
// 	Graham Knight (tglobe@tglobe.com)
//-----------------------------------------------------------------------
{$I GLOBE5.INC}
unit GScriptPresenter;

interface

Uses Windows, Classes, Graphics, SysUtils, Dialogs, Variants,
  Globe5, GClasses, GMapObjects, GSysUtils, GXML, GResource, GPresenters,
  BASE_PARSER, PaxScripter, PaxPascal, PaxBasic, PaxC;

type
  TGScript = type String;

  TGScriptLanguage = ( slBasic, slC, slPascal );

  {---------------------------- TGPointPresenter -------------------------------}
  TGScriptPresenter = class(TGPresenter)
  private
    FScript: TGScript;
    FPaxScripter : TPaxScripter;
    FPaxLanguage : TPaxLanguage;
    FScriptError : Boolean;
    FScriptCompiled : Boolean;
    FScriptLanguage: TGScriptLanguage;

    FBeginLayerRenderID : integer;
    FEndLayerRenderID : integer;
    FRenderObjectID : integer;
    FLLInObjectID : integer;
    FAdjustToleranceID : integer;

    procedure SetScript(const Value: TGScript);
    procedure SetScriptLanguage(const Value: TGScriptLanguage);
  protected
    procedure AdjustTolerance( var tolerance : integer ); override;
    procedure BeginLayerRender( aLayer : TGLayer ); override;
    procedure EndLayerRender( aLayer : TGLayer ); override;

    function CallScriptMethod( var id : integer; const Params: array of const ): Variant;
  public
    constructor Create(parentGlobe: TGCustomGlobe5); override;
    destructor Destroy; override;

    procedure Assign(Source : TPersistent); override;

    procedure RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet); override;
    function LLInObject( ptLL : TGPointLL; mapObj : IGMapPoint; iTolerance : integer ) : Boolean; override;

    procedure LoadFromXML( Element : TGXML_Element ); override;
    procedure SaveToXML( Element : TGXML_Element ); override;
  published
    property Script : TGScript read FScript write SetScript;
    property ScriptLanguage : TGScriptLanguage read FScriptLanguage write SetScriptLanguage;
  end;

implementation

Uses
  IMP_Classes, IMP_SysUtils, IMP_Graphics, IMP_Contnrs, IMP_Dialogs,
  IMP_GSysUtils, IMP_GClasses, IMP_Globe5, IMP_GMapObjects, IMP_GLabelRenderer,
  IMP_GPresenters, IMP_GRenderer, IMP_GRenderer32, IMP_Globe5Utils;

constructor TGScriptPresenter.Create( parentGlobe: TGCustomGlobe5);
begin
  inherited;

  Name := 'TGScriptPresenter';
  FScript := '';
  ScriptLanguage := slPascal;
end;

destructor TGScriptPresenter.Destroy;
begin
  FreeAndNil(FPaxScripter);
  FreeAndNil(FPaxLanguage);

  inherited Destroy;
end;

procedure TGScriptPresenter.Assign( Source : TPersistent );
begin
  inherited Assign( Source );

  if Source.InheritsFrom( TGScriptPresenter ) then
  begin
    Script := TGScriptPresenter( Source ).Script;
  end;
end;

procedure TGScriptPresenter.BeginLayerRender( aLayer : TGLayer );
begin
  inherited;

  ParentGlobe.Renderer.RefreshDC;

  if not FScriptError and not FScriptCompiled then
  begin
    FBeginLayerRenderID := 0;
    FEndLayerRenderID := 0;
    FRenderObjectID := 0;
    FLLInObjectID := 0;
    FAdjustToleranceID := 0;

    FPaxScripter.ResetScripter;

    case FScriptLanguage of
    slBasic :
      begin
        FPaxScripter.AddModule('Main','paxBasic');
        FPaxScripter.AddCode('Main', 'Imports Dialogs, Classes, Graphics, SysUtils, GSysUtils, GClasses, GMapObjects, GPresenters, GRenderer32, Globe5, Globe5Utils' );
      end;
    slC :
      begin
        FPaxScripter.AddModule('Main','paxC');
        FPaxScripter.AddCode('Main', 'using Dialogs, Classes, Graphics, SysUtils, GSysUtils, GClasses, GMapObjects, GPresenters, GRenderer32, Globe5, Globe5Utils;' );
      end;
    slPascal :
      begin
        FPaxScripter.AddModule('Main','paxPascal');
        FPaxScripter.AddCode('Main', 'Uses Dialogs, Classes, Graphics, SysUtils, GSysUtils, GClasses, GMapObjects, GPresenters, GRenderer32, Globe5, Globe5Utils;' );
      end;
    end;

    FPaxScripter.RegisterObject('Globe', TGlobe5(ParentGlobe));
    FPaxScripter.RegisterObject('Presenter', Self);

    FPaxScripter.AddCode('Main', FScript );
    FPaxScripter.Compile;

    FScriptError := FPaxScripter.IsError;

    if not FScriptError then
    begin
      FScriptCompiled := true;

      FBeginLayerRenderID := FPaxScripter.GetMemberID( 'BeginLayerRender');
      FEndLayerRenderID := FPaxScripter.GetMemberID( 'EndLayerRender');

      FRenderObjectID := FPaxScripter.GetMemberID( 'RenderObject');
      FLLInObjectID := FPaxScripter.GetMemberID( 'LLInObject');
      FAdjustToleranceID := FPaxScripter.GetMemberID( 'AdjustTolerance');
    end;
  end;

  CallScriptMethod( FBeginLayerRenderID, [aLayer] );
end;

procedure TGScriptPresenter.RenderObject( mapObj : IGMapPoint; State : TGRenderStateSet );
var
  obj : TGMapPoint;
begin
  if FRenderObjectID <> 0 then
  begin
    obj := TGMapPoint((mapObj as IGUnderlyingObject).GetUnderlyingObject );
    CallScriptMethod( FRenderObjectID, [obj, rsSelected in State] );
  end
  else
    mapObj.Render(ParentGlobe);
end;

procedure TGScriptPresenter.LoadFromXML( Element : TGXML_Element );
begin
  inherited;

  ScriptLanguage := TGScriptLanguage( Element.Attribute('Language', Ord( slPascal )));

  if Element.Element('Script') <> nil then
    Script := Element.Element('Script').BodyText;
end;

procedure TGScriptPresenter.SaveToXML( Element : TGXML_Element );
begin
  inherited;

  Element.AddElement('Script').BodyText := Script;
  Element.AddAttribute('Language', integer( FScriptLanguage ), Ord( slPascal ));
end;

procedure TGScriptPresenter.SetScript(const Value: TGScript);
begin
  FScript := Value;
  FScriptError := false;
  FScriptCompiled := false;
end;

function TGScriptPresenter.LLInObject(
  ptLL: TGPointLL; mapObj: IGMapPoint; iTolerance: integer): Boolean;
var
  obj : TGMapPoint;
  v : Variant;
begin
  obj := TGMapPoint((mapObj as IGUnderlyingObject).GetUnderlyingObject );

  if FLLInObjectID <> 0 then
  begin
    v := CallScriptMethod( FLLInObjectID, [ptLL.iLongX, ptLL.iLatY, obj] );

    Result := not ( VarIsEmpty(v) or ( v = false ))
  end
  else
    Result := obj.LLInObject(ptLL, iTolerance);
end;

function TGScriptPresenter.CallScriptMethod( var id: integer;
  const Params: array of const): Variant;
begin
  Result := Unassigned;

  if not FScriptError and ( id <> 0 ) then
  try
    Result := FPaxScripter.CallFunctionByID( id, Params );

    FScriptError := FPaxScripter.IsError;
    if FScriptError then
    begin
      id := 0;
      FPaxScripter.DiscardError;
    end;
  except
    id := 0;
  end;
end;

procedure TGScriptPresenter.AdjustTolerance(var tolerance: integer);
var
  v : Variant;
begin
  inherited;

  v := CallScriptMethod( FAdjustToleranceID, [tolerance] );

  if not VarIsEmpty(v) then
    tolerance := v;
end;

procedure TGScriptPresenter.SetScriptLanguage( const Value: TGScriptLanguage );
begin
  FreeAndNil(FPaxScripter);
  FreeAndNil(FPaxLanguage);

  FPaxScripter := TPaxScripter.Create(nil);

  FScriptLanguage := Value;

  case FScriptLanguage of
  slBasic :
    FPaxLanguage := TPaxBasic.Create(nil);
  slC :
    FPaxLanguage := TPaxC.Create(nil);
  slPascal :
    FPaxLanguage := TPaxPascal.Create(nil);
  end;

  FPaxScripter.RegisterLanguage(FPaxLanguage);
end;

procedure TGScriptPresenter.EndLayerRender( aLayer : TGLayer );
begin
  inherited;

  CallScriptMethod( FEndLayerRenderID, [aLayer] );
end;

initialization
  RegisterGlobeClass( TGScriptPresenter, 'Script Presenter' );
end.
