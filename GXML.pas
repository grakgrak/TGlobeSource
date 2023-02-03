{-------------------------------------------------------------------------
 Module:    GXML Classes

 Comment:   XML parsing and management classes

 Classes:   TGXML_Document
            TGXML_Element

 Author:    Graham Knight
 Email:     tglobe@tglobe.com
-------------------------------------------------------------------------}
{$I GLOBE5.INC}

unit GXML;

interface

uses
  SysUtils, Classes;

type
  TGXML_Document = class;

  //-------------------------------------------------------------------------------
  //Description
  //Represents an XML element. This can store XML attributes, other XML elements
  //and a single bodytext for the element.
  //
  //
  //
  //Attributes are stored as Ident=Value strings and the Object point to Self.
  //
  //Elements are stored as Ident and the Object pointing to a new Element.
  //
  //BodyText is Stored as Text with the Object set to nil.
  //-------------------------------------------------------------------------------
  TGXML_Element = class( TPersistent )
  private
    FAttributes: TStringList;
    FItems: TStringList;
    FElementTag: String;

    function AttributePosition(Index: integer): integer;
    function GetBodyText: String;
    procedure SetBodyText(const Value: String);
  public
    constructor Create(const sTag: String);
    destructor Destroy; override;
    procedure Assign( Source : TPersistent ); override;

    procedure Clear;
    procedure ClearAttributes;
    procedure ClearElements;

    procedure AddAttribute(const Tag, Value, defaultValue: String; SaveBlankValue: Boolean = False ); overload;
    procedure AddAttribute(const Tag : String; Value: integer; defaultValue: integer); overload;
    procedure AddAttribute(const Tag : String; Value: Extended; defaultValue: Extended); overload;
    procedure AddAttribute(const Tag : String; Value: Boolean; defaultValue: Boolean); overload;
    procedure DeleteAttribute( const Tag: String );

    function Attribute(iInstance: integer): String; overload;
    function Attribute(const Tag: String; const DefaultValue: String): String; overload;
    function Attribute(const Tag: String; const DefaultValue: integer): integer; overload;
    function Attribute(const Tag: String; const DefaultValue: Extended): Extended; overload;
    function Attribute(const Tag: String; const DefaultValue: Boolean): Boolean; overload;

    function AttributeCount: integer;
    function AttributeName(iInstance: integer): String;

    function AddElement(const Tag: String): TGXML_Element;
    procedure DeleteElement(iInstance: integer); overload;
    procedure DeleteElement(el : TGXML_Element); overload;
    procedure MoveElement( el, elFrom, elTo : TGXML_Element );

    function Element(iInstance: integer): TGXML_Element; overload;
    function Element(const Tag: String; iInstance: integer = 0; autoCreate: Boolean = false): TGXML_Element; overload;
    function ElementCount: integer; overload;
    function ElementCount(const Tag: String): integer; overload;

    function ElementExists(const Tag: String; iInstance: integer = 0): Boolean;
    function ElementByID(const Tag, IDValue: String; const IDTag: String = 'ID'): TGXML_Element;
    function ElementByKey( const sKey : String ) : TGXML_Element;

    function XMLString( doc : TGXML_Document; iLevel: integer): String;

    property BodyText: String read GetBodyText write SetBodyText;
    property ElementTag: String read FElementTag;
  end;

  TGXML_Document = class
  private
    FDocument: TGXML_Element;
    FDocumentTag : String;
    FFormatted: Boolean;
    FCompressed: Boolean;
    FTagTable : TStringList;

    function GetXMLString: String;
    procedure SetXMLString(const XML: String);

    procedure SetDocument(doc: TGXML_Element);
    function GetDocument: TGXML_Element;
  protected
    procedure BuildTagTable;
    function CompressTag( const tag : String ) : String;
    function ExpandTag( const tag : String ) : String;
  public
    constructor Create( const documentTag : String );
    destructor Destroy; override;
    procedure Clear;

    procedure LoadFromFile(const sFilename: String);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const sFilename: String);
    procedure SaveToStream(Stream: TStream);

    property Compressed: Boolean read FCompressed write FCompressed;
    property Formatted: Boolean read FFormatted write FFormatted;
    property AsString: String read GetXMLstring write SetXMLstring;
    property Document: TGXML_Element read GetDocument write SetDocument;
  end;

function StrToXMLText(const value: String; attribute : Boolean): String;
function XMLTextToStr(const value: String): String;

implementation

Uses GSysUtils;

function StrToXMLText(const value: String; attribute : Boolean): String;
var
  iStart, idx : integer;
begin
  Result := '';
  iStart := 1;
  idx := 1;
  while idx <= Length( value ) do
  begin
    if ( value[idx] in ['&','<','>',#13,#10] ) or ( attribute and ( value[idx] in ['''','"'] )) then
    begin
      if idx - iStart > 0 then
        Result := Result + Copy( value, iStart, idx - iStart );
      iStart := idx + 1;
      case value[idx] of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '''': Result := Result + '&apos;';
      '"': Result := Result + '&#34;';
      #13: Result := Result + '&#13;';
      #10: Result := Result + '&#10;';
      end;
    end;
    Inc( idx );
  end;

  if idx - iStart > 0 then
    Result := Result + Copy( value, iStart, idx - iStart );

  Result := Trim(Result);
end;


function XMLTextToStr(const value: String): String;
begin
  Result := Trim(Value);

  // check to see if there is something to do first
  if Pos( '&', Result ) > 0 then
  begin
    {Standard XML named entities - added 8 Aug 2001, John R}
    Result := StringReplace(Result, '&lt;',   '<',  [rfReplaceAll]);
    Result := StringReplace(Result, '&gt;',   '>',  [rfReplaceAll]);
    Result := StringReplace(Result, '&apos;', '''', [rfReplaceAll]);
    Result := StringReplace(Result, '&quot;', '"',  [rfReplaceAll]);

    Result := StringReplace(Result, '&#10;', #10, [rfReplaceAll]);
    Result := StringReplace(Result, '&#13;', #13, [rfReplaceAll]);
    Result := StringReplace(Result, '&#39;', '''', [rfReplaceAll]);
    Result := StringReplace(Result, '&#34;', '"', [rfReplaceAll]);
    Result := StringReplace(Result, '&#60;', '<', [rfReplaceAll]);
    Result := StringReplace(Result, '&#62;', '>', [rfReplaceAll]);

    {Do the ampersands last, and do them in such a way that the result of the ampersand
    replacement does not, itself, result in any additional replacements.  E.g.
    '&#38;amp;' becomes '&amp;' in the final result, NOT '&'. }
    Result := StringReplace(Result, '&#38;',  '&amp;', [rfReplaceAll]);
    Result := StringReplace(Result, '&amp;',  '&',     [rfReplaceAll]);
  end;
end;


function TGXML_Element.AttributePosition(Index: integer): integer;
begin
  for Result := 0 to FAttributes.Count - 1 do
  begin
    if Index = 0 then
      Exit;
    Dec(Index);
  end;
  Result := -1;
  Assert(Result <> -1, 'No Attribute at Index ' + IntToStr(Index));
end;


procedure TGXML_Element.AddAttribute(const Tag, Value, defaultValue: String; SaveBlankValue: Boolean = False);
var
  idx : integer;
begin
  if Value = defaultValue then  // ignore if the value is the default Value
    Exit;

  Assert(Pos('=', Tag) = 0, 'Illegal character in Attribute Tag');

  idx := FAttributes.IndexOfName(Trim(Tag));
  if idx >= 0 then
    FAttributes.Delete(idx);

  if (Value <> '') or SaveBlankValue then
    FAttributes.AddObject(Trim(Tag) + '=' + Value, Self);
end;


procedure TGXML_Element.AddAttribute(const Tag: String; Value: integer; defaultValue: integer);
begin
  AddAttribute( Tag, IntToStr(Value), IntToStr(defaultValue),false);
end;


procedure TGXML_Element.AddAttribute(const Tag : String; Value: Extended; defaultValue: Extended);
begin
  AddAttribute( Tag, ExtendedToStr(Value), ExtendedToStr(defaultValue), false);
end;


procedure TGXML_Element.AddAttribute(const Tag : String; Value: Boolean; defaultValue: Boolean);
begin
  AddAttribute( Tag, 'FT'[Ord(Value) + 1], 'FT'[Ord(defaultValue) + 1] );
end;

function TGXML_Element.AddElement(const Tag: String): TGXML_Element;
begin
  Result := TGXML_Element.Create(Tag);
  FItems.AddObject(Trim(Tag), Result);
end;


destructor TGXML_Element.Destroy;
var
  idx: integer;
begin
  for idx := 0 to FItems.Count - 1 do
    FItems.Objects[idx].Free;

  FreeAndNil( FItems );
  FreeAndNil( FAttributes );
  inherited;
end;


function TGXML_Element.GetBodyText: String;
var
  idx: integer;
begin
  Result := '';
  for idx := 0 to FAttributes.Count - 1 do
    if FAttributes.Objects[idx] = nil then
      Result := Result + FAttributes.Strings[idx];
end;

procedure TGXML_Element.SetBodyText(const Value: String);
var
  idx: integer;
begin
  // delete any existing text
  for idx := FAttributes.Count - 1 downto 0 do
    if FAttributes.Objects[idx] = nil then
      FAttributes.Delete(idx);

  FAttributes.Add(Value);
end;


function TGXML_Element.AttributeCount: integer;
var
  idx: integer;
begin
  Result := 0;
  for idx := 0 to FAttributes.Count - 1 do
    if FAttributes.Objects[idx] <> nil then
      Inc(Result);
end;


procedure TGXML_Element.DeleteAttribute(const Tag: String);
var
  idx : integer;
begin
  idx := FAttributes.IndexOfName(Trim(Tag));

  if idx >= 0 then
    FAttributes.Delete(idx);
end;


function TGXML_Element.AttributeName(iInstance: integer): String;
begin
  Result := FAttributes.Names[AttributePosition(iInstance)];
end;


function TGXML_Element.ElementCount: integer;
begin
  Result := FItems.Count;
end;


function TGXML_Element.ElementCount(const Tag: String): integer;
var
  idx: integer;
begin
  Result := 0;
  for idx := 0 to FItems.Count - 1 do
    if AnsiCompareText(FItems.Strings[idx], Tag) = 0 then
      Inc(Result);
end;

procedure TGXML_Element.DeleteElement(iInstance: integer);
begin
  if ( iInstance >= 0 ) and ( iInstance < FItems.Count ) then
  begin
    FItems.Objects[iInstance].Free;
    FItems.Delete(iInstance);
  end;
end;

procedure TGXML_Element.DeleteElement(el : TGXML_Element);
begin
  if el <> nil then
    DeleteElement( FItems.IndexOfObject(el));
end;

procedure TGXML_Element.MoveElement( el, elFrom, elTo : TGXML_Element );
var
  idx : integer;
begin
  idx := elFrom.FItems.IndexOfObject(el);
  if idx < 0 then
    Exit;

  elFrom.FItems.Delete(idx);
  elTo.FItems.AddObject( el.ElementTag, el );
end;

function TGXML_Element.Attribute(iInstance: integer): String;
begin
  Result := FAttributes.Values[FAttributes.Names[AttributePosition(iInstance)]];
end;


function TGXML_Element.Attribute(const Tag: String; const DefaultValue: String ): String;
begin
  if FAttributes.IndexOfName(Trim(Tag)) < 0 then
    Result := DefaultValue
  else
    Result := FAttributes.Values[Trim(Tag)];
end;


function TGXML_Element.Attribute(const Tag: String; const DefaultValue: integer): integer;
begin
  Result := StrToIntDef( Attribute( Tag, '' ), DefaultValue );
end;


function TGXML_Element.Attribute(const Tag: String; const DefaultValue: Extended): Extended;
begin
  Result := StrToExtendedDef( Attribute( Tag, ''), DefaultValue);
end;


function TGXML_Element.Attribute(const Tag: String; const DefaultValue: Boolean): Boolean;
var
  attr : String;
begin
  attr := Uppercase( Trim( Attribute( Tag, '' )));
  if ( attr = 'T' ) or ( attr = '1' ) then
    Result := true
  else
    if ( attr = 'F' ) or ( attr = '0' ) then
      Result := false
    else
      Result := DefaultValue;
end;


procedure TGXML_Element.Assign( Source : TPersistent );
var
  idx : integer;
  el : TGXML_Element;
begin
  if ( Source <> nil ) and ( Source is TGXML_Element ) then
  begin
    Self.FAttributes.Clear;
    Self.FItems.Clear;
    Self.FElementTag := '';

    with TGXML_Element( Source ) do
    begin
      Self.FElementTag := ElementTag;

      // add attributes
      for idx := 0 to FAttributes.Count - 1 do
        if FAttributes.Objects[idx] = nil then
          Self.FAttributes.AddObject( FAttributes.Strings[idx], nil )
        else
          Self.FAttributes.AddObject( FAttributes.Strings[idx], Self );

      // add sub elements
      for idx := 0 to FItems.Count - 1 do
      begin
        el := Self.AddElement( FItems.Strings[idx] );
        el.Assign(TPersistent( FItems.Objects[idx] ));
      end;
    end;
  end;
end;

procedure TGXML_Element.Clear;
begin
  ClearAttributes;
  ClearElements;  // ensures that all sub-elements are freed

  FAttributes.Clear;
  FItems.Clear;
end;

procedure TGXML_Element.ClearAttributes;
var
  idx: integer;
begin
  for idx := FAttributes.Count - 1 downto 0 do
    if FAttributes.Objects[idx] <> nil then
      FAttributes.Delete(idx);
end;

procedure TGXML_Element.ClearElements;
var
  idx: integer;
begin
  for idx := FItems.Count - 1 downto 0 do
  begin
    FItems.Objects[idx].Free;
    FItems.Delete(idx);
  end;
end;

constructor TGXML_Element.Create( const sTag : String );
begin
  inherited Create;

  FElementTag := sTag;
  FItems := TStringList.Create;
  FAttributes := TStringList.Create;
end;


function TGXML_Element.Element(iInstance: integer): TGXML_Element;
begin
  Result := TGXML_Element(FItems.Objects[iInstance]);
end;


function TGXML_Element.Element(const Tag: String; iInstance: integer = 0;
  autoCreate: Boolean = false): TGXML_Element;
var
  idx: integer;
begin
  for idx := 0 to FItems.Count - 1 do
    if AnsiCompareText(FItems.Strings[idx], Tag) = 0 then
    begin
      if iInstance = 0 then
      begin
        Result := TGXML_Element(FItems.Objects[idx]);
        Exit;
      end;
      Dec(iInstance);
    end;

  if not autoCreate then
    Result := nil
  else
    repeat
      Result := AddElement(Tag);
      Dec( iInstance );
    until iInstance < 0;
end;


function TGXML_Element.ElementByID(const Tag, IDValue: String; const IDTag: String = 'ID'): TGXML_Element;
var
  idx: integer;
begin
  for idx := 0 to FItems.Count - 1 do
  begin
    Result := TGXML_Element(FItems.Objects[idx]);
    if AnsiCompareText(FItems.Strings[idx], Tag) = 0 then
      if AnsiCompareText(Result.Attribute(IDTag, ''), IDValue) = 0 then
        Exit;
  end;
  Result := nil;
end;

function TGXML_Element.ElementByKey(const sKey: String): TGXML_Element;
var
  idx : integer;
begin
  idx := Pos( '.', sKey );

  if idx > 0 then
  begin
    Result := Element( Copy( sKey, 1, idx - 1 ));
    if Result <> nil then
      Result := Result.ElementByKey( Copy( sKey, idx + 1, Length( sKey )));
  end
  else
    Result := Element( sKey );
end;

function TGXML_Element.ElementExists(const Tag: String; iInstance: integer = 0): Boolean;
begin
  Result := Element(Tag, iInstance) <> nil;
end;

//------------------------------------------------------------------------------
//Description
//Converts an XML Element into a string representation
//
//Parameters
//doc :        The document object that owns the elements.
//iLevel :     The current depth that the element is at in the document. Used to
//             insert spaces into the ouptut if Formatted is true.
//------------------------------------------------------------------------------
function TGXML_Element.XMLString( doc : TGXML_Document; iLevel: integer): String;
var
  idx: integer;
  sBodyText: string;

  function Attributes: String;
  var
    idx, iPos: integer;
    sAttr: String;
  begin
    Result := ' ';
    for idx := 0 to FAttributes.Count - 1 do
      if FAttributes.Objects[idx] <> nil then
      begin
        sAttr := FAttributes.Strings[idx];
        iPos := Pos('=', sAttr);
        Result := Result + doc.CompressTag( Copy(sAttr, 1, iPos - 1)) + '='''
          + StrToXMLText(Copy(sAttr, iPos + 1, Length(sAttr)), true) + ''' ';
      end;

    // if there are no sub elements and no body text
    if ( FItems.Count = 0 ) and ( BodyText = '' )  then
      Result := Result + '/';
  end;

begin
  Result := '';
  if ( FItems.Count = 0 ) and ( FAttributes.Count = 0 ) then // do not process empty elements
    Exit;

  if doc.Formatted and ( iLevel > 0 ) then
    Result := Result + #13#10 + StringOfChar(#9, iLevel);

  if ElementTag <> '' then
    Result := Result + '<' + Trim(doc.CompressTag(ElementTag) + Attributes ) + '>';

  if (ElementTag = '') or (Result[Length(Result) - 1] <> '/') then
  begin
    for idx := 0 to FItems.Count - 1 do
      Result := Result + TGXML_Element(FItems.Objects[idx]).XMLString( doc, iLevel + 1);

    sBodyText := GetBodyText;
    if sBodyText <> '' then
      Result := Result + StrToXMLText(sBodyText, false);

    if ( sBodyText = '' ) and doc.Formatted and (iLevel > 0) then
      Result := Result + #13#10 + StringOfChar(#9, iLevel);

    if ElementTag <> '' then
      Result := Result + '</' + doc.CompressTag(ElementTag) + '>'; // Terminating tag
  end;
end;

function SortByCount(List: TStringList; Index1, Index2: Integer): integer;
begin
  Result := Integer( List.Objects[Index2] ) - Integer( List.Objects[Index1] );
end;

procedure TGXML_Document.BuildTagTable;
  procedure AddTag( const tag : String );
  var
    pos : integer;
  begin
    pos := FTagTable.Add(tag);
    FTagTable.Objects[pos] := TObject( Integer( FTagTable.Objects[pos] ) + 1 );
  end;

  procedure AddTags( el : TGXML_Element );
  var
    idx : integer;
  begin
    AddTag(el.ElementTag);

    for idx := 0 to el.AttributeCount - 1 do
      AddTag( el.AttributeName(idx));

    for idx := 0 to el.ElementCount - 1 do
      AddTags( el.Element(idx) );
  end;
begin
  FTagTable.Clear;

{$IFNDEF VER130}
  FTagTable.CaseSensitive := False;
{$ENDIF}
  FTagTable.Duplicates := dupIgnore;
  FTagTable.Sorted := True;

  AddTags( Document );

  FTagTable.Sorted := false;
  FTagTable.CustomSort(SortByCount);  // sort by frequency
end;

procedure TGXML_Document.Clear;
begin
  FreeAndNil( FDocument );
  FTagTable.Clear;
end;


constructor TGXML_Document.Create( const documentTag : String );
begin
  inherited Create;

  Compressed := False;
  Formatted := False;
  FDocumentTag := documentTag;
  FTagTable := TStringList.Create;
end;


destructor TGXML_Document.Destroy;
begin
  Clear;
  FTagTable.Free;
  inherited;
end;


function TGXML_Document.GetDocument: TGXML_Element;
begin
  if FDocument = nil then
    FDocument := TGXML_Element.Create(FDocumentTag);
  Result := FDocument;
end;

function TGXML_Document.GetXMLString: String;
begin
  if Compressed then
  begin
    BuildTagTable;
    Result := '<DICT>' + FTagTable.CommaText + '</DICT>' + Document.XMLString( Self, 0 );
  end
  else
    Result := Document.XMLString( Self, 0 );

  FTagTable.Clear;
end;


procedure TGXML_Document.LoadFromFile(const sFilename: String);
var
  Stream: TFileStream;
begin
  Stream := nil;
  if FileExists(sFileName) then
  try
    Stream := TFileStream.Create(sFileName, fmOpenRead or fmShareDenyWrite);
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;


procedure TGXML_Document.LoadFromStream(Stream: TStream);
var
  Count: integer;
  XML: String;
begin
  Stream.Position := 0;
  Count := Stream.Size;
  SetLength(XML, Count);
  if Count <> 0 then
    Stream.ReadBuffer(XML[1], Count);
  AsString := XML;
end;


procedure TGXML_Document.SaveToFile(const sFilename: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(sFileName, fmCreate or fmShareDenyNone);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;


procedure TGXML_Document.SaveToStream(Stream: TStream);
var
  XML: String;
begin
  XML := AsString;

  Stream.Write(XML[1], Length(XML));
end;



procedure TGXML_Document.SetDocument(doc: TGXML_Element);
begin
  if doc <> Document then
  begin
    FDocument.Free;
    FDocument := doc;
  end;
end;


procedure TGXML_Document.SetXMLString(const XML: String);
var
  iOffset, iEOS: integer;

  //---------------------------------------------------------------------------

  procedure ParseBody(el: TGXML_Element);
  var
    sBody: String;
    start : integer;
  begin
    sBody := '';

    // strip leading space
    while ( iOffset <= iEOS ) and ( XML[iOffset] <= ' ' ) do
      Inc(iOffset);

    // check for and process a CDATA section
    if Copy( XML, iOffset, 9 ) = '<![CDATA[' then
    begin
      start := iOffset + 9;
      while (iOffset <= iEOS - 3) and (XML[iOffset] <> ']') and (XML[iOffset + 1] <> ']') and (XML[iOffset + 2] <> '>') do
        Inc( iOffset );

      if el <> nil then
        el.FAttributes.Add( Copy( XML, start, ( iOffset - start ) + 1 ));
      Inc( iOffset, 5 );
    end
    else  // process a normal bodytext
    begin
      while (iOffset <= iEOS) and  (XML[iOffset] <> '<') do
      begin
        sBody := sBody + XML[iOffset];
        Inc(iOffset);
      end;
      Inc(iOffset);

      if el <> nil then
        el.FAttributes.Add(XMLTextToStr( Trim(sBody)));
    end;
  end;

  //---------------------------------------------------------------------------

  procedure ParseAttributes(el: TGXML_Element; sAttr: String);
  var
    iPos: integer;
    sIdent, sValue: String;
  begin
    iPos := Pos('="', sAttr);
    while iPos > 0 do
    begin
      sIdent := ExpandTag( Copy(sAttr, 1, iPos - 1));
      sAttr := Copy(sAttr, iPos + 2, Length(sAttr));
      iPos := Pos('"', sAttr);
      sValue := Copy(sAttr, 1, iPos - 1);
      sAttr := Copy(sAttr, iPos + 1, Length(sAttr));

      el.AddAttribute(sIdent, XMLTextToStr( sValue ), '');
      iPos := Pos('="', sAttr);
    end;

    iPos := Pos('=''', sAttr);
    while iPos > 0 do
    begin
      sIdent := ExpandTag( Copy(sAttr, 1, iPos - 1));
      sAttr := Copy(sAttr, iPos + 2, Length(sAttr));
      iPos := Pos('''', sAttr);
      sValue := Copy(sAttr, 1, iPos - 1);
      sAttr := Copy(sAttr, iPos + 1, Length(sAttr));

      el.AddAttribute(sIdent, XMLTextToStr( sValue ), '');
      iPos := Pos('=''', sAttr);
    end;
  end;

  //---------------------------------------------------------------------------
  function ParseTagAttributes(el: TGXML_Element): Boolean;
  var
    sAttr: String;
  begin
    sAttr := '';
    while (iOffset <= iEOS) and (not (XML[iOffset] in ['>']))  do
    begin
      sAttr := sAttr + XML[iOffset];
      Inc(iOffset);
    end;

    Result := False;
    sAttr := Trim(sAttr);
    Inc(iOffset);
    if sAttr <> '' then
    begin
      ParseAttributes(el, sAttr);
      Result := sAttr[Length(sAttr)] in ['?', '/'];
    end;
  end;

  //---------------------------------------------------------------------------
  function ParseTagIdent(var sIdent: String; el: TGXML_Element): Boolean;
  begin
    sIdent := '';

    Result := XML[iOffset] = '/';
    if Result then
      Inc( iOffset );

    while (iOffset <= iEOS) and (not (XML[iOffset] in ['>', ' ', '/', #13, #10])) do
    begin
      sIdent := sIdent + XML[iOffset];
      Inc(iOffset);
    end;

    if sIdent = '' then
      Result := true;

    if Result then
      Inc(iOffset);
  end;

  //---------------------------------------------------------------------------

  procedure ParseElement(el: TGXML_Element);
  var
    sIdent: String;
    sub: TGXML_Element;
  begin
    while iOffset <= iEOS do
    begin
      ParseBody(el);

      if ParseTagIdent(sIdent, el) then
        Break;

      if el = nil then
      begin
        // on the second time around
        if FDocumentTag = 'DICT' then
        begin
          FTagTable.CommaText := Document.BodyText;
          Document.Clear;
        end;

        FDocumentTag := ExpandTag( sIdent );
        sub := Document;
      end
      else
        sub := el.AddElement(ExpandTag( sIdent ));

      if not ParseTagAttributes(sub) then
        ParseElement(sub);
    end;
  end;

begin
  Clear;
  iOffset := 1;
  iEOS := Length(XML);
  ParseElement( nil );
end;

function TGXML_Document.CompressTag(const tag: String): String;
var
  idx : integer;
begin
  Result := tag;

  if FTagTable.Count > 0 then
  begin
    idx := FTagTable.IndexOf(Result);

    if idx >= 0 then
      Result := IntToStr(idx);
  end;
end;

function TGXML_Document.ExpandTag(const tag: String): String;
var
  idx : integer;
begin
  Result := tag;

  if FTagTable.Count > 0 then
  begin
    idx := StrToIntDef( tag, -1 );

    if ( idx >= 0 ) and ( idx < FTagTable.Count ) then
      Result := FTagTable[idx];
  end;
end;

end.

