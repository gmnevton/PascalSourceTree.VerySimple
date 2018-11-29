{
  PascalSourceTree.VerySimple v1.0
  
  A lightweight, one-unit, cross-platform Pascal Source Tree reader[/writer]
  for Delphi 2010-XE10.3

  Author:
    (c) Copyrights 2018; Grzegorz Molenda; gmnevton@o2.pl
    
  Version history:
    v.1.0 - 2018.10.30 - GM - initial version
    
    
  This unit is free and can be used for any needs. The introduction of
  any changes and the use of those changed library is permitted without
  limitations with only requirement:
  This header must be present without changes in all modifications of library.

  * The contents of this file, used with permission, are subject to   *
  * the Mozilla Public License Version 1.1 (the "License"); you may   *
  * not use this file except in compliance with the License. You may  *
  * obtain a copy of the License at                                   *
  * http:  www.mozilla.org/MPL/MPL-1.1.html                           *
  *                                                                   *
  * Software distributed under the License is distributed on an       *
  * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or    *
  * implied. See the License for the specific language governing      *
  * rights and limitations under the License.                         *
}
unit PascalSourceTree.VerySimple;

interface

uses
  SysUtils,
  Classes,
  IniFiles,
  Generics.Defaults,
  Generics.Collections;

const
  TSourceSpaces = #$20 + #$0D + #$0A + #9;

type
  TSourceNode = class;
  TSourceNodeList = class;
  TSourceTree = class;

  TSourceNodeType = (stUnit, stKeyWord, stIdentifier, stLambda, stComment);
  TSourceNodeTypes = set of TSourceNodeType;
  TSourceNodeSubType = (sstUnspecified, sstType, sstConst, sstVariable, sstDeclaration, sstInOut, sstOut, sstProcedure, sstFunction, sstCommentBlock, sstCommentLine, sstClass, sstClassMethod, sstClassProperty);
  TSourceNodeSubTypes = set of TSourceNodeSubType;
  TSourceNodeSearchType = (ssRecursive);
  TSourceNodeSearchTypes = set of TSourceNodeSearchType;
  TSourceOption = (soNodeAutoIndent, soCompact, soCompactWithBreakes, soCaseInsensitive, soWriteBOM);
  TSourceOptions = set of TSourceOption;
  TSourceExtractTextOption = (jetDeleteToStopChar, jetDeleteWithStopChar, jetStopString);
  TSourceExtractTextOptions = set of TSourceExtractTextOption;

  ESourceParseException = class(Exception);

  TSourceNodeAttributeType = (atSingle, atType);
  TSourceNodeLambdaType = (sltProcedure, sltFunction);

  TSourceNodeAttribute = class(TObject)
  private
    FName: String;
    FValue: String;
    FAttributeType: TSourceNodeAttributeType;
    procedure SetValue(const Value: String);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //
    function AsString: String;
    property Name: String read FName write FName;
    property Value: String read FValue write SetValue;
    property AttributeType: TSourceNodeAttributeType read FAttributeType write FAttributeType;
  end;

  TSourceNodeAttributeList = class(TObjectList<TSourceNodeAttribute>)
  public
    [Weak] Document: TSourceTree;
    function Add(const Name: String): TSourceNodeAttribute; overload; virtual;
    function Find(const Name: String): TSourceNodeAttribute; virtual;
    procedure Delete(const Name: String); overload; virtual;
    function HasAttribute(const AttrName: String): Boolean; virtual;
    function AsString: String; virtual;
    function AsStrings: TStrings; virtual;
  end;

  TSourceNodeCallBack = reference to procedure(const Node: TSourceNode; var BreakLoop: Boolean); // now BreakLoop = True can break the loop

  TSourceNode = class(TObject)
  private
    FName: String;
    FType: TSourceNodeType;
    FSubType: TSourceNodeSubType;
    FValueType: String;
    FValue: String;
    FLine: Cardinal;
    FPos: Cardinal;
    ///
    FLevel: Cardinal; // node level in tree structure
    FIndex: Cardinal; // node index in nodes list structure
    FPrevSibling,           // link to the node's previous sibling or nil if it is the first node
    FNextSibling: TSourceNode; // link to the node's next sibling or nil if it is the last node
  protected
    [Weak] FSourceTree: TSourceTree;
  protected
    function IsSame(const Value1, Value2: String): Boolean; virtual;
    function FindNodeRecursive(const Name: String; NodeTypes: TSourceNodeTypes = []): TSourceNode; overload; virtual;
    function FindNodeRecursive(const Name: String; NodeTypes: TSourceNodeTypes = []; NodeSubTypes: TSourceNodeSubTypes = []): TSourceNode; overload; virtual;
    procedure AddAttr(const AttrName: String; const AttrValue: String); virtual;
    procedure SetAttr(const AttrName: String; const AttrValue: String); virtual;
    procedure SetSourceTree(const Value: TSourceTree); virtual;
    procedure SetName(const Value: String); virtual;
    procedure SetType(const Value: TSourceNodeType); virtual;
    procedure SetSubType(const Value: TSourceNodeSubType); virtual;
    function GetAttr(const AttrName: String): String; virtual;
    function GetName: String; virtual;
  public
    AttributeList: TSourceNodeAttributeList;
    ChildNodes: TSourceNodeList;
    [Weak] ParentNode: TSourceNode;
  public
    constructor Create(ANodeType: TSourceNodeType; ANodeSubType: TSourceNodeSubType); virtual;
    destructor Destroy; override;
    //
    procedure Clear; virtual;
    function FindNode(const Name: String; NodeTypes: TSourceNodeTypes = []; const SearchOptions: TSourceNodeSearchTypes = []): TSourceNode; overload; virtual;
    function FindNode(const Name: String; NodeTypes: TSourceNodeTypes = []; NodeSubTypes: TSourceNodeSubTypes = []; const SearchOptions: TSourceNodeSearchTypes = []): TSourceNode; overload; virtual;
    function FindNodes(const Name: String; NodeTypes: TSourceNodeTypes = []): TSourceNodeList; virtual;
    procedure ScanNodes(Name: String; CallBack: TSourceNodeCallBack);
    function HasChild(const Name: String; NodeTypes: TSourceNodeTypes = []): Boolean; virtual;
    function AddChild(const AName: String; ANodeType: TSourceNodeType; ANodeSubType: TSourceNodeSubType = sstUnspecified): TSourceNode; virtual;
    function RemoveChild(const Node: TSourceNode): Integer; virtual;
    function MoveChild(const FromNode, ToNode: TSourceNode): TSourceNode; virtual;
    procedure AddNodes(const RootNode: TSourceNode; const AddRootNode: Boolean = False); virtual;
    function InsertChild(const Name: String; Position: Integer; NodeType: TSourceNodeType): TSourceNode; virtual;
    function SetAttribute(const AttrName, AttrValue: String): TSourceNode; virtual;
    function FirstChild: TSourceNode; virtual;
    function LastChild: TSourceNode; virtual;
    function PreviousSibling: TSourceNode; overload; virtual;
    function NextSibling: TSourceNode; overload; virtual;
    function HasChildNodes: Boolean; virtual;
    property Attributes[const AttrName: String]: String read GetAttr write SetAttr;
    property Document: TSourceTree read FSourceTree write SetSourceTree;
    property NodeName: String read GetName write SetName;
    property NodeType: TSourceNodeType read FType write SetType;
    property NodeSubType: TSourceNodeSubType read FSubType write SetSubType;
    property NodeValueType: String read FValueType write FValueType;
    property NodeValue: String read FValue write FValue;
    property Level: Cardinal read FLevel;
    property Index: Cardinal read FIndex;
    property SourceLine: Cardinal read FLine write FLine;
    property LinePos: Cardinal read FPos write FPos;
  end;

  TSourceNodeList = class(TObjectList<TSourceNode>)
  private
    function IsSame(const Value1, Value2: String): Boolean;
  public
    [Weak] Document: TSourceTree;
    [Weak] Parent: TSourceNode;
    function Add(Value: TSourceNode): Integer; overload; virtual;
    function Add(NodeType: TSourceNodeType): TSourceNode; overload; virtual;
    function Add(const Name: String; NodeType: TSourceNodeType): TSourceNode; overload; virtual;
    function Add(const Name: String; NodeType: TSourceNodeType; NodeSubType: TSourceNodeSubType): TSourceNode; overload; virtual;
    function Insert(const Name: String; Position: Integer; NodeType: TSourceNodeType): TSourceNode; overload; virtual;
    procedure Remove(Index: Integer); overload; virtual;
    function FindNode(const Name: String; NodeTypes: TSourceNodeTypes = []): TSourceNode; overload; virtual;
    function FindNode(const Name: String; NodeTypes: TSourceNodeTypes = []; NodeSubTypes: TSourceNodeSubTypes = []): TSourceNode; overload; virtual;
    function FindNodes(const Name: String; NodeTypes: TSourceNodeTypes = []): TSourceNodeList; virtual;
    function HasNode(const Name: String; NodeTypes: TSourceNodeTypes = []): Boolean; virtual;
    function FirstChild: TSourceNode; virtual;
    function PreviousSibling(Node: TSourceNode): TSourceNode; virtual;
    function NextSibling(Node: TSourceNode): TSourceNode; virtual;
    function Get(Index: Integer): TSourceNode; virtual;
    function CountNames(const Name: String; var NodeList: TSourceNodeList): Integer; virtual;
  end;

  TSourceTokenizer = class;
  TSourceReader = TSourceTokenizer;
  TSourceReaderClass = class of TSourceReader;

  TSourceTree = class(TObject)
  private
    FText: String;
    FRoot: TSourceNode;
    FEncoding: String;
    FInternalEncodingClass: TEncoding;
    //FSkipIndent: Boolean;
    //FReaderClass: TSourceReaderClass;
  protected
    function  GetReaderClass: TSourceReaderClass; virtual;
    procedure Parse(Reader: TSourceReader); virtual;
    procedure SetText(const Value: String); virtual;
    //function  GetText: String; virtual;
    procedure SetEncoding(const Value: String); virtual;
    function  GetChildNodes: TSourceNodeList; virtual;
    procedure SetNodeAutoIndent(const Value: Boolean);
    function  GetNodeAutoIndent: Boolean;
    function  IsSame(const Value1, Value2: String): Boolean; virtual;
  public
    NodeIndentStr: String;
    LineBreak: String;
    Options: TSourceOptions;
  public
    constructor Create(const Name: String);
    destructor Destroy; override;
    //
    procedure Clear; virtual;
    function AddChild(const Name: String; NodeType: TSourceNodeType): TSourceNode; virtual;
    function RemoveChild(const Node: TSourceNode): Integer; virtual;
    function MoveChild(const FromNode, ToNode: TSourceNode): TSourceNode; virtual;
    function CreateNode(const Name: String; NodeType: TSourceNodeType): TSourceNode; virtual;
    function LoadFromFile(const FileName: String; BufferSize: Integer = 8192): TSourceTree; virtual;
    function LoadFromStream(const Stream: TStream; BufferSize: Integer = 8192): TSourceTree; virtual;
    //function SaveToFile(const FileName: String): TSourceTree; virtual;
    //function SaveToStream(const Stream: TStream): TSourceTree; virtual;
    property ChildNodes: TSourceNodeList read GetChildNodes;
    property Document: TSourceNode read FRoot;
    property Encoding: String read FEncoding write SetEncoding;
    property NodeAutoIndent: Boolean read GetNodeAutoIndent write SetNodeAutoIndent;
    property Text: String read {GetText}FText write SetText;
    property ReaderClass: TSourceReaderClass read GetReaderClass;
  end;

  ESourceTokenizer = class(Exception)
  private
    FLine: Cardinal;
    FLinePos: Cardinal;
  public
    constructor Create(const Msg: String; const Line: Cardinal; const LinePos: Cardinal);
    property Line: Cardinal read FLine;
    property LinePos: Cardinal read FLinePos;
  end;

  TSourceTokenizer = class(TObject)
  private
    FStream: TStream;
    FOrigin: Cardinal;
    FSaveChar: Byte;
  private
    procedure ReadBuffer;
    procedure SkipBlanks;
  protected type
    TLexerTokenType = (ltEOF, ltUnknown, ltControlChar, ltSymbol, ltKeyWord, ltVariable, ltString, ltLambda, ltInteger, ltFloat, ltComment, ltCommentBlock, ltCommentLine);
    TLexerCharType = (lcUnknown, lcLetter, lcNumber, lcSpace, lcQuote, lcDoubleQuote, lcHash, lcDollar, lcPercent, lcAmpersand,
                      lcLeftParenthesis, lcRightParenthesis, lcAsterisk, lcPlus, lcMinus, lcComma, lcPeriod, lcSlash, lcColon, lcSemicolon,
                      lcLessThan, lcEquals, lcGreaterThan, lcAt, lcBackSlash, lcAccent, lcLeftSquareBracket, lcRightSquareBracket,
                      lcLeftBracket, lcRightBracket, lcOther);
  protected
    FBufferSize: Cardinal;
    FBuffer: TBytes;
    FBufPtr: Cardinal;
    FBufEnd: Cardinal;
    FSourcePtr: Cardinal;
    FStoredPtr: Cardinal;
    FIsStoredPtr: Boolean;
    FCanMoveToNext: Boolean;
    FSourceEnd: Cardinal;
    FSourceLine: Cardinal;
    FLineStart: Cardinal;
    FToken: TLexerTokenType;
    FTokenPtr: Cardinal;
    FFloatType: AnsiChar;
    FWideStr: UnicodeString;
    FEncoding: TEncoding;
    FFormat: TFormatSettings;
  protected
    procedure Error(const Msg: String);
    function GetSourcePos: Cardinal; virtual;
    function GetLinePos: Integer; virtual;
    function GetBufferChar: AnsiChar; overload; virtual;
    function GetBufferChar(const Offset: Cardinal): AnsiChar; overload; virtual;
    function NextToken: Boolean; virtual; abstract; // ABSTRACT
    procedure NextChar(const Force: Boolean = False); virtual;
    procedure PreviousChar; virtual;
    procedure StoreSourcePtr; virtual;
    procedure StoreTokenPtr; virtual;
    function TokenFloat: Extended; virtual;
    function TokenInt: Int64; virtual;
    function TokenString: String; virtual;
    function TokenLine(const IncludeToken: Boolean = False): String; overload; virtual;
    function TokenLine(const ToChar: Array of AnsiChar; const IncludeToken: Boolean = False): String; overload; virtual;
    function TokenIsKeyWord(const S: String): Boolean; virtual;
    function CharType(var ABufPos: Cardinal): TLexerCharType; virtual;
    procedure CheckToken(T: TLexerTokenType); overload; virtual;
    procedure CheckToken(T: Array of TLexerTokenType; const ErrorMsg: String); overload; virtual;
    function CheckToken(T: TLexerTokenType; C: AnsiChar): Boolean; overload; virtual;
    function CheckLastSymbol(C: AnsiChar; ShowError: Boolean = False): Boolean; virtual;
    procedure CheckAlphaNumeric(const StartPtr, EndPtr: Cardinal; var Result: TLexerTokenType); overload; virtual;
    function CheckAlphaNumeric(AlphaNumeric: String): TLexerTokenType; overload; virtual;
  public
    constructor Create(const Stream: TStream; const Encoding: TEncoding; const DetectBOM: Boolean = True; const BufferSize: Cardinal = 8192); virtual;
    destructor Destroy; override;
    property SourceLine: Cardinal read FSourceLine;
    property SourcePos: Cardinal read GetSourcePos;
    property LinePos: Integer read GetLinePos;
    property Format: TFormatSettings read FFormat write FFormat;
    property FloatType: AnsiChar read FFloatType;
    property Token: TLexerTokenType read FToken;
    property BufferChar: AnsiChar read GetBufferChar;
  end;

  TPascalTokenizer = class(TSourceTokenizer)
  private const
    PascalKeyWords = 'absolute'#13#10'abstract'#13#10'and'#13#10'array'#13#10'as'#13#10'asm'#13#10'assembler'#13#10'at'#13#10'automated'#13#10+
                     'begin'#13#10'case'#13#10'cdecl'#13#10'class'#13#10'const'#13#10'constructor'#13#10'contains'#13#10'default'#13#10+
                     'destructor'#13#10'dispid'#13#10'dispinterface'#13#10'div'#13#10'do'#13#10'downto'#13#10'dynamic'#13#10'else'#13#10+
                     'end'#13#10'except'#13#10'exports'#13#10'external'#13#10'file'#13#10'finalization'#13#10'finally'#13#10'for'#13#10 +
                     'forward'#13#10'function'#13#10'goto'#13#10'if'#13#10'implementation'#13#10'in'#13#10'index'#13#10'inherited'#13#10+
                     'initialization'#13#10'inline'#13#10'interface'#13#10'is'#13#10'label'#13#10'library'#13#10'message'#13#10'mod'#13#10+
                     'nil'#13#10'nodefault'#13#10'not'#13#10'object'#13#10'of'#13#10'on'#13#10'or'#13#10'override'#13#10'packed'#13#10+
                     'pascal'#13#10'private'#13#10'procedure'#13#10'program'#13#10'property'#13#10'protected'#13#10'public'#13#10'published'#13#10+
                     'raise'#13#10'read'#13#10'record'#13#10'register'#13#10'repeat'#13#10'requires'#13#10'resident'#13#10'safecall'#13#10+
                     'set'#13#10'shl'#13#10'shr'#13#10'stdcall'#13#10'stored'#13#10'string'#13#10'then'#13#10'threadvar'#13#10'to'#13#10+
                     'try'#13#10'type'#13#10'unit'#13#10'until'#13#10'uses'#13#10'var'#13#10'virtual'#13#10'while'#13#10'with'#13#10'write'#13#10'xor';
  private
    FKeyWords: THashedStringList;
  protected
    function NextToken: Boolean; override;
    function TokenString: String; override;
    function TokenIsKeyWord(const S: String): Boolean; override;
    function CheckAlphaNumeric(AlphaNumeric: String): TSourceTokenizer.TLexerTokenType; override;
  public
    constructor Create(const Stream: TStream; const Encoding: TEncoding; const DetectBOM: Boolean = True; const BufferSize: Cardinal = 8192); override;
    destructor Destroy; override;
  end;

  TPascalSourceTree = class(TSourceTree)
  private
    FInsideLambda: Integer;
  protected
    function  GetReaderClass: TSourceReaderClass; override;
    procedure Parse(Reader: TSourceReader); override;
    procedure ParseKeyWord(Reader: TSourceReader; Parent: TSourceNode);
    procedure ParseSection(Reader: TSourceReader; Parent: TSourceNode);
    procedure ParseUses(Reader: TSourceReader; Parent: TSourceNode);
    procedure ParseVariable(Reader: TSourceReader; Parent: TSourceNode);
    procedure ParseConst(Reader: TSourceReader; Parent: TSourceNode);
    procedure ParseType(Reader: TSourceReader; Parent: TSourceNode);
    procedure ParseString(Reader: TSourceReader; Parent: TSourceNode);
    procedure ParseComment(Reader: TSourceReader; Parent: TSourceNode);
    procedure ParseInteger(Reader: TSourceReader; Parent: TSourceNode);
    procedure ParseFloat(Reader: TSourceReader; Parent: TSourceNode);
    procedure ParseMainBlock(Reader: TSourceReader; Parent: TSourceNode);
    procedure ParseLambda(Reader: TSourceReader; Parent: TSourceNode; LambdaType: TSourceNodeLambdaType);
    procedure ParseLambdaHeader(Reader: TSourceReader; Parent: TSourceNode; LambdaType: TSourceNodeLambdaType);
    procedure ParseLambdaParams(Reader: TSourceReader; Parent: TSourceNode; LambdaType: TSourceNodeLambdaType);
    procedure ParseLambdaBody(Reader: TSourceReader; Parent: TSourceNode; LambdaType: TSourceNodeLambdaType);
  end;

implementation

uses
  StrUtils,
  Character;

const
  sExpected             = 'expected!';
  sLinePos              = #13#10'Line: %d, Pos: %d';

resourcestring
  sExpectedButFound     = 'Expected %s, but %s found at ''%s''.';
  sExpectedButNotFound  = 'Expected %s, but nothing found!';
  sUnknownIdentifier    = 'Unknown identifier!' + sLinePos;
  sUnexpectedIdentifier = 'Unexpected identifier!' + sLinePos;
  sSymbolExpected       = 'Symbol ''%s'' ' + sExpected;
  sKeyWordExpected      = 'Keyword ' + sExpected;
  sVariableExpected     = 'Variable ' + sExpected;
  sTypeExpected         = 'Type ' + sExpected;
  sStringExpected       = 'String ' + sExpected;
  sCommentExpected      = 'Comment ' + sExpected;
  sLambdaExpected       = 'Procedure or function ' + sExpected;
  sNumberExpected       = 'Number ' + sExpected;

{$REGION 'Helpers'}
type
  TStreamWriterHelper = class helper for TStreamWriter
  public
    constructor Create(Stream: TStream; Encoding: TEncoding; WritePreamble: Boolean = True; BufferSize: Integer = 1024); overload;
    constructor Create(Filename: string; Append: Boolean; Encoding: TEncoding; WritePreamble: Boolean = True; BufferSize: Integer = 1024); overload;
  end;

const
{$IF CompilerVersion >= 24} // Delphi XE3+ can use Low(), High() and TEncoding.ANSI
  LowStr = Low(String); // Get string index base, may be 0 (NextGen compiler) or 1 (standard compiler)
{$ELSE} // For any previous Delphi version overwrite High() function and use 1 as string index base
  LowStr = 1;  // Use 1 as string index base

function High(const Value: String): Integer; inline;
begin
  Result:=Length(Value);
end;

//Delphi XE3 added PosEx as an overloaded Pos function, so we need to wrap it in every other Delphi version
function Pos(const SubStr, S: String; Offset: Integer): Integer; overload; Inline;
begin
  Result:=PosEx(SubStr, S, Offset);
end;
{$IFEND}

{$IF CompilerVersion < 23}  //Delphi XE2 added ANSI as Encoding, in every other Delphi version use TEncoding.Default
type
  TEncodingHelper = class helper for TEncoding
    class function GetANSI: TEncoding; static;
    class property ANSI: TEncoding read GetANSI;
  end;

class function TEncodingHelper.GetANSI: TEncoding;
begin
  Result:=TEncoding.Default;
end;
{$IFEND}

{ TStreamWriterHelper }

constructor TStreamWriterHelper.Create(Stream: TStream; Encoding: TEncoding; WritePreamble: Boolean; BufferSize: Integer);
begin
  Create(Stream, Encoding, BufferSize);
  if not WritePreamble then begin
    Self.BaseStream.Position:=0;
    Self.BaseStream.Size:=0;
  end;
end;

constructor TStreamWriterHelper.Create(Filename: string; Append: Boolean; Encoding: TEncoding; WritePreamble: Boolean; BufferSize: Integer);
begin
  Create(Filename, Append, Encoding, BufferSize);
  if not WritePreamble then begin
    Self.BaseStream.Position:=0;
    Self.BaseStream.Size:=0;
  end;
end;

{$ENDREGION}

{$REGION 'Implementation'}
{ TSourceNodeAttribute }

constructor TSourceNodeAttribute.Create;
begin
  FName:='';
  FValue:='';
  FAttributeType:=atSingle;
end;

destructor TSourceNodeAttribute.Destroy;
begin
  FName:='';
  FValue:='';
  inherited;
end;

procedure TSourceNodeAttribute.SetValue(const Value: String);
begin
  FValue:=Value;
  FAttributeType:=atType;
end;

function TSourceNodeAttribute.AsString: String;
begin
  Result:=FName;
  if AttributeType = atSingle then
    Exit;
  Result:=Result + ': ' + Value;
end;

{ TSourceNodeAttributeList }

function TSourceNodeAttributeList.Add(const Name: String): TSourceNodeAttribute;
begin
  Result:=TSourceNodeAttribute.Create;
  Result.Name:=Name;
  try
    Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TSourceNodeAttributeList.AsString: String;
var
  Attribute: TSourceNodeAttribute;
begin
  Result:='';
  for Attribute in Self do begin
    if Result <> '' then
      Result:=Result + ';';
    Result:=Result + ' ' + Attribute.AsString;
  end;
  Result:=Trim(Result);
end;

function TSourceNodeAttributeList.AsStrings: TStrings;
var
  Attribute: TSourceNodeAttribute;
begin
  Result:=TStringList.Create;
  for Attribute in Self do
    Result.Add(Attribute.AsString);
end;

procedure TSourceNodeAttributeList.Delete(const Name: String);
var
  Attribute: TSourceNodeAttribute;
begin
  Attribute:=Find(Name);
  if Assigned(Attribute) then
    Remove(Attribute);
end;

function TSourceNodeAttributeList.Find(const Name: String): TSourceNodeAttribute;
var
  Attribute: TSourceNodeAttribute;
begin
  Result:=NIL;
  for Attribute in Self do
    if ((Assigned(Document) and Document.IsSame(Attribute.Name, Name)) or // use the documents text comparison
       ((not Assigned(Document)) and (Attribute.Name = Name))) then begin // or if not Assigned then compare names case sensitive
      Result:=Attribute;
      Break;
    end;
end;

function TSourceNodeAttributeList.HasAttribute(const AttrName: String): Boolean;
begin
  Result:=Assigned(Find(AttrName));
end;

{ TSourceNode }

constructor TSourceNode.Create(ANodeType: TSourceNodeType; ANodeSubType: TSourceNodeSubType);
begin
  AttributeList:=TSourceNodeAttributeList.Create;
  ChildNodes:=TSourceNodeList.Create;
  ChildNodes.Parent:=Self;
  NodeName:='';
  NodeType:=ANodeType;
  NodeSubType:=ANodeSubType;
  FLevel:=0;
  FIndex:=0;
end;

destructor TSourceNode.Destroy;
begin
  Clear;
  ChildNodes.Free;
  AttributeList.Free;
  inherited;
end;

function TSourceNode.IsSame(const Value1, Value2: String): Boolean;
begin
  Result:=((Assigned(Document) and Document.IsSame(Value1, Value2)) or // use the documents text comparison
           (not Assigned(Document) and (CompareText(Value1, Value2) = 0))); // or if not Assigned then compare names case sensitive
end;

function TSourceNode.FindNodeRecursive(const Name: String; NodeTypes: TSourceNodeTypes): TSourceNode;
begin
  Result:=FindNodeRecursive(Name, NodeTypes, []);
end;

function TSourceNode.FindNodeRecursive(const Name: String; NodeTypes: TSourceNodeTypes; NodeSubTypes: TSourceNodeSubTypes): TSourceNode;
var
  Node: TSourceNode;
begin
  Result:=Nil;
  for Node in ChildNodes do begin
    if ((NodeTypes = []) or (Node.NodeType in NodeTypes)) and // if no type specified or node type in types
       ((NodeSubTypes = []) or (Node.NodeSubType in NodeSubTypes)) and
       ((Name = '') or ((Name <> '') and IsSame(Node.NodeName, Name))) then begin
      Result:=Node;
      Exit;
    end;
    if Node.HasChildNodes then begin
      Result:=Node.FindNodeRecursive(Name, NodeTypes, NodeSubTypes);
      if Result <> Nil then
        Exit;
    end;
  end;
end;

procedure TSourceNode.AddAttr(const AttrName, AttrValue: String);
var
  Attribute: TSourceNodeAttribute;
begin
  Attribute:=AttributeList.Add(AttrName);
  Attribute.AttributeType:=atType;
  Attribute.Name:=AttrName; // this allows rewriting of the attribute name (lower/upper case)
  Attribute.Value:=AttrValue;
end;

procedure TSourceNode.SetAttr(const AttrName, AttrValue: String);
begin
  SetAttribute(AttrName, AttrValue);
end;

function TSourceNode.SetAttribute(const AttrName, AttrValue: String): TSourceNode;
var
  Attribute: TSourceNodeAttribute;
begin
  Attribute:=AttributeList.Find(AttrName); // Search for given name
  if not Assigned(Attribute) then // If attribute is not found, create one
    Attribute:=AttributeList.Add(AttrName);
  Attribute.AttributeType:=atType;
  Attribute.Name:=AttrName; // this allows rewriting of the attribute name (lower/upper case)
  Attribute.Value:=AttrValue;
  Result:=Self;
end;

procedure TSourceNode.SetSourceTree(const Value: TSourceTree);
begin
  FSourceTree:=Value;
  ChildNodes.Document:=Value;
end;

procedure TSourceNode.SetName(const Value: String);
begin
  FName:=Value;
end;

procedure TSourceNode.SetType(const Value: TSourceNodeType);
begin
  FType:=Value;
end;

procedure TSourceNode.SetSubType(const Value: TSourceNodeSubType);
begin
  FSubType:=Value;
end;

function TSourceNode.GetAttr(const AttrName: String): String;
var
  Attribute: TSourceNodeAttribute;
begin
  Result:='';
  Attribute:=AttributeList.Find(AttrName);
  if Assigned(Attribute) then
    Result:=Attribute.Value;
end;

function TSourceNode.GetName: String;
begin
  Result:=FName;
end;

procedure TSourceNode.Clear;
begin
  NodeName:='';
  NodeValueType:='';
  NodeValue:='';
  ChildNodes.Clear;
end;

function TSourceNode.AddChild(const AName: String; ANodeType: TSourceNodeType; ANodeSubType: TSourceNodeSubType = sstUnspecified): TSourceNode;
var
  Last: TSourceNode;
begin
  Last:=Nil;
  try
    if ChildNodes.Count > 0 then
      Last:=ChildNodes.Last;
  except
    Last:=Nil;
  end;
  Result:=ChildNodes.Add(AName, ANodeType, ANodeSubType);
  Result.FPrevSibling:=Nil;
  Result.FNextSibling:=Nil;
  if Last <> Nil then begin
    Result.FPrevSibling:=Last;
    Last.FNextSibling:=Result;
  end;
end;

function TSourceNode.RemoveChild(const Node: TSourceNode): Integer;
begin
  Result:=Node.Index;
  if Node.NextSibling <> Nil then
    Node.NextSibling.FPrevSibling:=Node.PreviousSibling
  else if Node.PreviousSibling <> Nil then // last node, so delete reference within previous node to this, which is about to be deleted
    Node.PreviousSibling.FNextSibling:=Nil;
  ChildNodes.Remove(Result);
end;

function TSourceNode.MoveChild(const FromNode, ToNode: TSourceNode): TSourceNode;
begin
  Result:=Nil;
  if (ToNode <> Nil) and (FromNode <> Nil) then begin
    ToNode.AddNodes(FromNode, True);
    FromNode.ParentNode.RemoveChild(FromNode);
    Result:=ToNode;
  end;
end;

procedure TSourceNode.AddNodes(const RootNode: TSourceNode; const AddRootNode: Boolean);
var
  Child, Node: TSourceNode;
begin
  Child:=Self;
  if AddRootNode then begin
    Child:=AddChild(RootNode.NodeName, RootNode.NodeType);
    Child.NodeSubType:=RootNode.NodeSubType;
  end;
  for Node in RootNode.ChildNodes do // add all root node child nodes to child node
    Child.AddNodes(Node, True);
end;

function TSourceNode.FindNode(const Name: String; NodeTypes: TSourceNodeTypes; const SearchOptions: TSourceNodeSearchTypes): TSourceNode;
begin
  Result:=FindNode(Name, NodeTypes, [], SearchOptions);
end;

function TSourceNode.FindNode(const Name: String; NodeTypes: TSourceNodeTypes; NodeSubTypes: TSourceNodeSubTypes; const SearchOptions: TSourceNodeSearchTypes): TSourceNode;
begin
  if ((NodeTypes = []) or (Self.NodeType in NodeTypes)) and
     ((NodeSubTypes = []) or (Self.NodeSubType in NodeSubTypes)) and
     ((Name = '') or ((Name <> '') and IsSame(Self.NodeName, Name))) then begin
    Result:=Self;
    Exit;
  end;
  Result:=ChildNodes.FindNode(Name, NodeTypes, NodeSubTypes);
  if (Result = Nil) and (ssRecursive in SearchOptions) then
    Result:=FindNodeRecursive(Name, NodeTypes, NodeSubTypes);
end;

function TSourceNode.FindNodes(const Name: String; NodeTypes: TSourceNodeTypes): TSourceNodeList;
begin
  Result:=ChildNodes.FindNodes(Name, NodeTypes);
end;

procedure TSourceNode.ScanNodes(Name: String; CallBack: TSourceNodeCallBack);
var
  Node: TSourceNode;
  CanBreak: Boolean;
begin
  Name:=LowerCase(Name);
  for Node in ChildNodes do
    if (Name = '') or ((Name <> '') and (CompareText(Node.NodeName, Name) = 0)) then begin
      CanBreak:=False;
      CallBack(Node, CanBreak);
      if CanBreak then // break the loop if Result is False
        Break;
    end;
end;

function TSourceNode.FirstChild: TSourceNode;
begin
  Result:=ChildNodes.First;
end;

function TSourceNode.HasChild(const Name: String; NodeTypes: TSourceNodeTypes): Boolean;
begin
  Result:=ChildNodes.HasNode(Name, NodeTypes);
end;

function TSourceNode.HasChildNodes: Boolean;
begin
  Result:=(ChildNodes.Count > 0);
end;

function TSourceNode.InsertChild(const Name: String; Position: Integer; NodeType: TSourceNodeType): TSourceNode;
begin
  Result:=ChildNodes.Insert(Name, Position, NodeType);
  if Assigned(Result) then
    Result.ParentNode:=Self;
end;

function TSourceNode.LastChild: TSourceNode;
begin
  if ChildNodes.Count > 0 then
    Result:=ChildNodes.Last
  else
    Result:=NIL;
end;

function TSourceNode.PreviousSibling: TSourceNode;
begin
  Result:=FPrevSibling;
end;

function TSourceNode.NextSibling: TSourceNode;
begin
  Result:=FNextSibling;
end;

{ TSourceNodeList }

function TSourceNodeList.Add(Value: TSourceNode): Integer;
var
  Index: Integer;
begin
  Index:=-1;
  try
    if Count > 0 then
      Index:=Last.Index;
  except
    Index:=-1;
  end;
  Result:=inherited Add(Value);
  Value.ParentNode:=Parent;
  Value.FLevel:=Parent.Level + 1;
  Value.FIndex:=Index + 1;
end;

function TSourceNodeList.Add(NodeType: TSourceNodeType): TSourceNode;
begin
  Result:=TSourceNode.Create(NodeType, sstUnspecified);
  try
    Result.Document:=Document;
    Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TSourceNodeList.Add(const Name: String; NodeType: TSourceNodeType): TSourceNode;
begin
  Result:=Add(NodeType);
  Result.NodeName:=Name;
end;

function TSourceNodeList.Add(const Name: String; NodeType: TSourceNodeType; NodeSubType: TSourceNodeSubType): TSourceNode;
begin
  Result:=Add(NodeType);
  Result.NodeName:=Name;
  Result.NodeSubType:=NodeSubType;
end;

function TSourceNodeList.CountNames(const Name: String; var NodeList: TSourceNodeList): Integer;
begin
  NodeList:=FindNodes(Name, []);
  Result:=NodeList.Count;
end;

function TSourceNodeList.FindNode(const Name: String; NodeTypes: TSourceNodeTypes): TSourceNode;
begin
  Result:=FindNode(Name, NodeTypes, []);
end;

function TSourceNodeList.FindNode(const Name: String; NodeTypes: TSourceNodeTypes; NodeSubTypes: TSourceNodeSubTypes): TSourceNode;
var
  Node: TSourceNode;
begin
  Result:=NIL;
  for Node in Self do
    if ((NodeTypes = []) or (Node.NodeType in NodeTypes)) and // if no type specified or node type in types
       ((NodeSubTypes = []) or (Node.NodeSubType in NodeSubTypes)) and
       ((Name = '') or ((Name <> '') and IsSame(Node.NodeName, Name))) then begin
      Result:=Node;
      Break;
    end;
end;

function TSourceNodeList.FindNodes(const Name: String; NodeTypes: TSourceNodeTypes): TSourceNodeList;
var
  Node: TSourceNode;
begin
  Result:=TSourceNodeList.Create(False);
  Result.Document:=Document;
  try
    for Node in Self do
      if ((NodeTypes = []) or (Node.NodeType in NodeTypes)) and IsSame(Node.NodeName, Name) then begin
        Result.Parent:=Node.ParentNode;
        Result.Add(Node);
      end;
    Result.Parent:=NIL;
  except
    Result.Free;
    raise;
  end;
end;

function TSourceNodeList.FirstChild: TSourceNode;
begin
  Result:=First;
end;

function TSourceNodeList.Get(Index: Integer): TSourceNode;
begin
  Result:=Items[Index];
end;

function TSourceNodeList.HasNode(const Name: String; NodeTypes: TSourceNodeTypes): Boolean;
begin
  Result:=Assigned(FindNode(Name, NodeTypes, []));
end;

function TSourceNodeList.Insert(const Name: String; Position: Integer; NodeType: TSourceNodeType): TSourceNode;
var
  Node, NodeBefore: TSourceNode;
  Index: Integer;
begin
  try
    Node:=Get(Position);
  except
    Node:=Nil;
  end;
  Index:=0;
  if Node <> Nil then
    Index:=Node.Index;
  Result:=TSourceNode.Create(NodeType, sstUnspecified);
  Result.NodeName:=Name;
  Result.FLevel:=Parent.Level + 1;
  Result.Document:=Document;
  try
    Insert(Position, Result);
    Result.FIndex:=Index;
    if Position > 0 then try
      NodeBefore:=Get(Position - 1);
      Result.FPrevSibling:=NodeBefore;
      NodeBefore.FNextSibling:=Result;
    except
      // discard this
    end;
    if Node <> Nil then begin
      Result.FNextSibling:=Node;
      Node.FPrevSibling:=Result;
    end;
    // reindex nodes
    while Node <> Nil do begin
      Node.FIndex:=Index + 1;
      Inc(Index);
      Node:=Node.NextSibling;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TSourceNodeList.Remove(Index: Integer);
var
  Node: TSourceNode;
begin
  if Index >= 0 then begin
    try
      Node:=Get(Index);
    except
      Node:=Nil;
    end;
    if Node <> Nil then
      Node:=Node.NextSibling;
    Delete(Index);
    // reindex nodes
    while Node <> Nil do begin
      Node.FIndex:=Index;
      Inc(Index);
      Node:=Node.NextSibling;
    end;
  end;
end;

function TSourceNodeList.IsSame(const Value1, Value2: String): Boolean;
begin
  Result := ((Assigned(Document) and Document.IsSame(Value1, Value2)) or // use the documents text comparison
             (not Assigned(Document) and (Value1 = Value2))); // or if not Assigned then compare names case sensitive
end;

function TSourceNodeList.PreviousSibling(Node: TSourceNode): TSourceNode;
begin
  Result:=Node.PreviousSibling;
end;

function TSourceNodeList.NextSibling(Node: TSourceNode): TSourceNode;
begin
  Result:=Node.NextSibling;
end;

{ TSourceTree }

constructor TSourceTree.Create(const Name: String);
begin
  inherited Create;
  FRoot:=TSourceNode.Create(stUnit, sstUnspecified);
  FRoot.NodeName:=Name;
  FRoot.FLevel:=0;
//  Root.FIndex:=0;
  FRoot.ParentNode:=FRoot;
  FRoot.Document:=Self;
  Encoding:='utf-8';
  NodeIndentStr:='  ';
  Options:=[soNodeAutoIndent];
  LineBreak:=sLineBreak;
end;

destructor TSourceTree.Destroy;
begin
  FRoot.ParentNode:=NIL;
  FRoot.Clear;
  FRoot.Free;
  inherited;
end;

procedure TSourceTree.Parse(Reader: TSourceReader);
begin
//
end;

procedure TSourceTree.SetText(const Value: String);
var
  Stream: TStringStream;
begin
  FText:=Value;
  Stream:=TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.WriteString(Value);
    Stream.Position:=0;
    Encoding:='utf-8';
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

{
function TSourceTree.GetText: String;
var
  Stream: TStringStream;
begin
  if CompareText(Encoding, 'utf-8') = 0 then
    Stream:=TStringStream.Create('', TEncoding.UTF8)
  else
    Stream:=TStringStream.Create('', TEncoding.ANSI);
  try
    SaveToStream(Stream);
    Result:=Stream.DataString;
  finally
    Stream.Free;
  end;
end;
}

procedure TSourceTree.SetEncoding(const Value: String);
begin
  FEncoding:=Value;
  if CompareText(FEncoding, 'utf-8') = 0 then
    FInternalEncodingClass:=TEncoding.UTF8
  else if CompareText(Encoding, 'windows-1250') = 0 then
    FInternalEncodingClass:=TEncoding.GetEncoding(1250)
  else
    FInternalEncodingClass:=TEncoding.ANSI;
end;

function TSourceTree.GetChildNodes: TSourceNodeList;
begin
  Result:=FRoot.ChildNodes;
end;

procedure TSourceTree.SetNodeAutoIndent(const Value: Boolean);
begin
  if Value then
    Options:=Options + [soNodeAutoIndent]
  else
    Options:=Options - [soNodeAutoIndent];
end;

function TSourceTree.GetNodeAutoIndent: Boolean;
begin
  Result:=(soNodeAutoIndent in Options);
end;

function TSourceTree.IsSame(const Value1, Value2: String): Boolean;
begin
  if soCaseInsensitive in Options then
    Result:=(CompareText(Value1, Value2) = 0)
  else
    Result:=(Value1 = Value2);
end;

function TSourceTree.GetReaderClass: TSourceReaderClass;
begin
  Result:=TSourceReader;
end;

procedure TSourceTree.Clear;
begin
  FRoot.Clear;
end;

function TSourceTree.AddChild(const Name: String; NodeType: TSourceNodeType): TSourceNode;
begin
  Result:=Nil; // satisfy compiler
  try
    Result:=FRoot.AddChild(Name, NodeType);
    Result.Document:=Self;
  except
    Result.Free;
    raise;
  end;
end;

function TSourceTree.RemoveChild(const Node: TSourceNode): Integer;
begin
  Result:=-1;
  if Node <> Nil then begin
    if FRoot.ChildNodes.IndexOf(Node) > -1 then begin
      Result:=Node.Index;
      FRoot.ChildNodes.Remove(Result);
      Node.Clear;
    end;
  end;
end;

function TSourceTree.MoveChild(const FromNode, ToNode: TSourceNode): TSourceNode;
begin
  Result:=ToNode;
  if (ToNode <> Nil) and (FromNode <> Nil) then begin
    ToNode.AddNodes(FromNode, True);
    FromNode.ParentNode.RemoveChild(FromNode);
  end;
end;

function TSourceTree.CreateNode(const Name: String; NodeType: TSourceNodeType): TSourceNode;
begin
  Result:=TSourceNode.Create(NodeType, sstUnspecified);
  Result.NodeName:=Name;
  Result.Document:=Self;
end;

function TSourceTree.LoadFromFile(const FileName: String; BufferSize: Integer): TSourceTree;
var
  Stream: TFileStream;
begin
  Result:=Self;
  Stream:=TFileStream.Create(FileName, fmOpenRead + fmShareDenyWrite);
  try
    LoadFromStream(Stream, BufferSize);
  finally
    Stream.Free;
  end;
end;

function TSourceTree.LoadFromStream(const Stream: TStream; BufferSize: Integer): TSourceTree;
var
  Reader: TSourceReader;
begin
  Result:=Self;
  if Encoding = '' then // none specified then use UTF8 with DetectBom
    Reader:=ReaderClass.Create(Stream, TEncoding.UTF8, True, BufferSize)
  else if CompareText(Encoding, 'utf-8') = 0 then
    Reader:=ReaderClass.Create(Stream, TEncoding.UTF8, False, BufferSize)
  else if CompareText(Encoding, 'windows-1250') = 0 then
    Reader:=ReaderClass.Create(Stream, TEncoding.GetEncoding(1250), False, BufferSize)
  else
    Reader:=ReaderClass.Create(Stream, TEncoding.ANSI, False, BufferSize);
  try
    Parse(Reader);
  finally
    Reader.Free;
  end;
end;

{
function TSourceTree.SaveToFile(const FileName: String): TSourceTree;
var
  Stream: TFileStream;
begin
  Result:=Self;
  Stream:=TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TSourceTree.SaveToStream(const Stream: TStream): TSourceTree;
var
  Writer: TStreamWriter;
begin
  Result:=Self;
  if CompareText(Self.Encoding, 'utf-8') = 0 then
    Writer:=TStreamWriter.Create(Stream, TEncoding.UTF8, (soWriteBOM in Options))
  else if CompareText(Encoding, 'windows-1250') = 0 then
    Writer:=TStreamWriter.Create(Stream, TEncoding.GetEncoding(1250), (soWriteBOM in Options))
  else
    Writer:=TStreamWriter.Create(Stream, TEncoding.ANSI, (soWriteBOM in Options));
  try
    Compose(Writer);
  finally
    Writer.Free;
  end;
end;
}

function UTF8Len(B: Byte): Integer; inline;
begin
  case B of
    $00..$7F: Result := 1; //
    $C2..$DF: Result := 2; // 110x xxxx C0 - DF
    $E0..$EF: Result := 3; // 1110 xxxx E0 - EF
    $F0..$F7: Result := 4; // 1111 0xxx F0 - F7 // outside traditional UNICODE
  else
    Result := 0; // Illegal leading character.
  end;
end;

function UTF8ToCategory(const ABuffer: TBytes; var ABufPos: Cardinal): TUnicodeCategory;
var
  CharSize: Cardinal;
  C: UCS4Char;
  L: Cardinal;
begin
  CharSize:=UTF8Len(ABuffer[ABufPos]);
  L:=Length(ABuffer);
  Assert((CharSize > 0) and (CharSize + ABufPos < L), 'Invalid UTF8 Character'); // do not localize
  case CharSize of
    1: C:=UCS4Char(ABuffer[ABufPos]);
    2: C:=UCS4Char(((ABuffer[ABufPos] and $1F) shl 6 ) or (ABuffer[ABufPos + 1] and $3F));
    3: C:=UCS4Char(((ABuffer[ABufPos] and $0F) shl 12) or ((ABuffer[ABufPos + 1] and $3F) shl 6 ) or (ABuffer[ABufPos + 2] and $3F));
    4: C:=UCS4Char(((ABuffer[ABufPos] and $07) shl 18) or ((ABuffer[ABufPos + 1] and $3F) shl 12) or ((ABuffer[ABufPos + 2] and $3F) shl 6) or (ABuffer[ABufPos + 2] and $3F));
  else
    C:=0;
  end;
  Inc(ABufPos, CharSize);
  if C > $0000FFFF then
    Result:=TCharacter.GetUnicodeCategory(TCharacter.ConvertFromUtf32(C), 1)
  else
    Result:=TCharacter.GetUnicodeCategory(Char(C));
end;

{ ESourceTokenizer }

constructor ESourceTokenizer.Create(const Msg: String; const Line, LinePos: Cardinal);
begin
  FLine:=Line;
  FLinePos:=LinePos;
  inherited CreateFmt(Msg + #13#10'Line: %d, Pos: %d', [FLine, FLinePos]);
end;

{ TSourceTokenizer }

constructor TSourceTokenizer.Create(const Stream: TStream; const Encoding: TEncoding; const DetectBOM: Boolean = True; const BufferSize: Cardinal = 8192);
begin
  inherited Create;
  GetLocaleFormatSettings(0, FFormat);
  FStream:=Stream;
  FBufferSize:=BufferSize;
  SetLength(FBuffer, BufferSize);
  FillChar(FBuffer[0], FBufferSize, 0);
  FBufPtr:=0;
  FBufEnd:=FBufferSize - 1;
  FSourcePtr:=0;
  FStoredPtr:=0;
  FIsStoredPtr:=False;
  FCanMoveToNext:=False;
  FSourceEnd:=0;
  FTokenPtr:=0;
  FSourceLine:=1;
  FLineStart:=0;
  ReadBuffer;
  FEncoding:=Nil;
  if DetectBOM then
    FSourcePtr:=FSourcePtr + Cardinal(TEncoding.GetBufferEncoding(FBuffer, FEncoding));
  if (FEncoding = Nil) or ((FEncoding <> TEncoding.Default) and (FEncoding <> TEncoding.UTF8)) then begin
    if Encoding = Nil then
      FEncoding:=TEncoding.UTF8
    else
      FEncoding:=Encoding;
  end;
end;

destructor TSourceTokenizer.Destroy;
begin
  if Length(FBuffer) > 0 then
    FStream.Seek(Int64(FTokenPtr) - Int64(FBufPtr), TSeekOrigin.soCurrent);
  inherited Destroy;
end;

procedure TSourceTokenizer.Error(const Msg: String);
begin
  raise ESourceTokenizer.CreateFmt(Msg + sLinePos, [SourceLine, LinePos]);
end;

function TSourceTokenizer.GetLinePos: Integer;
begin
  Result:=FTokenPtr - FLineStart + 1;
end;

function TSourceTokenizer.GetSourcePos: Cardinal;
begin
  Result:=FOrigin + FTokenPtr;
end;

function TSourceTokenizer.GetBufferChar: AnsiChar;
begin
  Result:=GetBufferChar(0);
end;

function TSourceTokenizer.GetBufferChar(const Offset: Cardinal): AnsiChar;
begin
  if FSourcePtr + Offset <= FSourceEnd then
    Result:=AnsiChar(FBuffer[FSourcePtr + Offset])
  else
    Result:=#0;
end;

procedure TSourceTokenizer.NextChar(const Force: Boolean = False);
begin
  if FCanMoveToNext or Force then begin
    FCanMoveToNext:=False;
    Inc(FSourcePtr);
  end;
end;

procedure TSourceTokenizer.PreviousChar;
begin
  Dec(FSourcePtr);
  FCanMoveToNext:=True;
end;

procedure TSourceTokenizer.ReadBuffer;
var
  Count: Integer;
begin
  Inc(FOrigin, FSourcePtr);
  FBuffer[FSourceEnd]:=FSaveChar;
  Count:=FBufPtr - FSourcePtr;
  if Count <> 0 then
    Move(FBuffer[FSourcePtr], FBuffer[0], Count);
  FBufPtr:=Count;
  Inc(FBufPtr, FStream.Read(FBuffer[FBufPtr], FBufEnd - FBufPtr));
  FSourcePtr:=0;
  FSourceEnd:=FBufPtr;
  if FSourceEnd = FBufEnd then begin
    FSourceEnd:=LineStart(FBuffer, FSourceEnd - 1);
    if FSourceEnd = 0 then
      Error('Line too long!');
  end;
  FSaveChar:=FBuffer[FSourceEnd];
  FBuffer[FSourceEnd]:=0;
end;

procedure TSourceTokenizer.SkipBlanks;
begin
  NextChar;
  while True do begin
    case FBuffer[FSourcePtr] of
      0: begin
        ReadBuffer;
        if FBuffer[FSourcePtr] = 0 then
          Exit;
        Continue;
      end;
      10: begin
        Inc(FSourceLine);
        //Inc(FSourcePtr);
        FCanMoveToNext:=True;
        FLineStart:=FSourcePtr + 1;
        Break;
      end;
      33..255: Exit;
    else
      Inc(FSourcePtr);
    end;
  end;
end;

procedure TSourceTokenizer.StoreSourcePtr;
begin
  if not FIsStoredPtr then begin
    FStoredPtr:=FSourcePtr;
    FIsStoredPtr:=True;
  end;
end;

procedure TSourceTokenizer.StoreTokenPtr;
begin
  if not FIsStoredPtr then begin
    FStoredPtr:=FTokenPtr;
    FIsStoredPtr:=True;
  end;
end;

function TSourceTokenizer.TokenFloat: Extended;
begin
  if FFloatType <> #0 then
    Dec(FSourcePtr);
  Result:=StrToFloat(TokenString, FFormat);
  if FFloatType <> #0 then
    Inc(FSourcePtr);
end;

function TSourceTokenizer.TokenInt: Int64;
begin
  Result:=StrToInt64(TokenString);
end;

function TSourceTokenizer.TokenString: String;
begin
  Result:='';
end;

function TSourceTokenizer.TokenLine(const IncludeToken: Boolean = False): String;
begin
  if IncludeToken then
    StoreTokenPtr
  else
    StoreSourcePtr;
  while NextToken and not CheckToken(ltControlChar, #10) do begin
    if (Token in [ltControlChar, ltSymbol]) and (BufferChar in [#9, #13, #32]) then
      Continue;

    if not (Token in [ltSymbol..ltCommentLine]) then
      raise ESourceParseException.CreateFmt(sUnexpectedIdentifier, [SourceLine, LinePos]);
  end;
  Result:=TokenString;
end;

function TSourceTokenizer.TokenLine(const ToChar: Array of AnsiChar; const IncludeToken: Boolean): String;
var
  StopChar: AnsiChar;
  Found: Boolean;
begin
  if IncludeToken then
    StoreTokenPtr
  else
    StoreSourcePtr;
  while NextToken and not CheckToken(ltControlChar, #10) do begin
    if (Token in [ltControlChar, ltSymbol]) and (BufferChar in [#9, #13, #32]) then
      Continue;

    Found:=False;
    for StopChar in ToChar do
      if CheckToken(ltSymbol, StopChar) then
        Found:=True;
    if Found then
      Break;

    if not (Token in [ltSymbol..ltCommentLine]) then
      raise ESourceParseException.CreateFmt(sUnexpectedIdentifier, [SourceLine, LinePos]);
  end;
  Result:=TokenString;
end;

function TSourceTokenizer.TokenIsKeyWord(const S: String): Boolean;
begin
  Result:=False;
end;

function TSourceTokenizer.CharType(var ABufPos: Cardinal): TLexerCharType;
begin
  case AnsiChar(FBuffer[ABufPos]) of
    'A'..'Z',
    'a'..'z',
    '_'     : Result:=lcLetter;
    '0'..'9': Result:=lcNumber;
    ' '     : Result:=lcSpace;
    ''''    : Result:=lcQuote;
    '"'     : Result:=lcDoubleQuote;
    '#'     : Result:=lcHash;
    '$'     : Result:=lcDollar;
    '%'     : Result:=lcPercent;
    '&'     : Result:=lcAmpersand;
    '('     : Result:=lcLeftParenthesis;
    ')'     : Result:=lcRightParenthesis;
    '*'     : Result:=lcAsterisk;
    '+'     : Result:=lcPlus;
    '-'     : Result:=lcMinus;
    ','     : Result:=lcComma;
    '.'     : Result:=lcPeriod;
    '/'     : Result:=lcSlash;
    ':'     : Result:=lcColon;
    ';'     : Result:=lcSemicolon;
    '<'     : Result:=lcLessThan;
    '='     : Result:=lcEquals;
    '>'     : Result:=lcGreaterThan;
    '@'     : Result:=lcAt;
    '\'     : Result:=lcBackSlash;
    '^'     : Result:=lcAccent;
    '['     : Result:=lcLeftSquareBracket;
    ']'     : Result:=lcRightSquareBracket;
    '{'     : Result:=lcLeftBracket;
    '}'     : Result:=lcRightBracket;
  else
    if FBuffer[ABufPos] < 128 then begin
      if FBuffer[ABufPos] < 32 then begin
        Result:=lcUnknown;
        FCanMoveToNext:=True;
      end
      else
        Result:=lcOther;
    end
    else begin
      if (FEncoding = TEncoding.UTF8) and (FBuffer[ABufPos] > 127) then begin
        case UTF8ToCategory(FBuffer, ABufPos) of
          TUnicodeCategory.ucLowercaseLetter,
          TUnicodeCategory.ucModifierLetter,
          TUnicodeCategory.ucOtherLetter,
          TUnicodeCategory.ucTitlecaseLetter,
          TUnicodeCategory.ucUppercaseLetter,
          TUnicodeCategory.ucLetterNumber: Result:=lcLetter;

          TUnicodeCategory.ucCombiningMark,
          TUnicodeCategory.ucNonSpacingMark,
          TUnicodeCategory.ucConnectPunctuation,
          TUnicodeCategory.ucFormat,
          TUnicodeCategory.ucDecimalNumber: Result:=lcNumber;
        else
          Result:=lcOther;
        end;
      end
      else begin
        Result:=lcUnknown;
        FCanMoveToNext:=True;
      end;
    end;
  end;
end;

procedure TSourceTokenizer.CheckToken(T: TLexerTokenType);
begin
  if Token <> T then begin
    case T of
      ltKeyWord : Error(sKeyWordExpected);
      ltVariable: Error(sVariableExpected);
      ltString  : Error(sStringExpected);
      ltComment : Error(sCommentExpected);
      ltLambda  : Error(sLambdaExpected);
      ltInteger,
      ltFloat   : Error(sNumberExpected);
    end;
  end;
end;

procedure TSourceTokenizer.CheckToken(T: Array of TLexerTokenType; const ErrorMsg: String);
var
  L: TLexerTokenType;
  Result: Boolean;
begin
  Result:=False;
  for L in T do begin
    if Token = L then
      Result:=True;
  end;
  if not Result then
    Error(ErrorMsg);
end;

function TSourceTokenizer.CheckToken(T: TLexerTokenType; C: AnsiChar): Boolean;
begin
  Result:=(Token = T) and ((C = #0) or ((C <> #0) and (BufferChar = C)));
end;

function TSourceTokenizer.CheckLastSymbol(C: AnsiChar; ShowError: Boolean = False): Boolean;
begin
  Result:=(Token = ltSymbol) and ((C <> #0) and (BufferChar = C));
  if not Result and ShowError then
     Error(SysUtils.Format(sSymbolExpected, [C]));
end;

procedure TSourceTokenizer.CheckAlphaNumeric(const StartPtr, EndPtr: Cardinal; var Result: TLexerTokenType);
var
  L: Integer;
  S: String;
begin
  L:=EndPtr - StartPtr;
  S:=FEncoding.GetString(FBuffer, StartPtr, L);
  Result:=CheckAlphaNumeric(S);
  S:='';
end;

function TSourceTokenizer.CheckAlphaNumeric(AlphaNumeric: String): TLexerTokenType;
begin
  Result:=ltUnknown;
end;

{ TPascalTokenizer }

constructor TPascalTokenizer.Create(const Stream: TStream; const Encoding: TEncoding; const DetectBOM: Boolean = True; const BufferSize: Cardinal = 8192);
begin
  inherited Create(Stream, Encoding, DetectBOM, BufferSize);
  FKeyWords:=THashedStringList.Create;
  FKeyWords.LineBreak:=#13#10;
  FKeyWords.Text:=PascalKeyWords;
end;

destructor TPascalTokenizer.Destroy;
begin
  FKeyWords.Free;
  inherited;
end;

function TPascalTokenizer.NextToken: Boolean;
//var
//  I, J: Integer;
//  IsWideStr: Boolean;
//  P, Q, S: Integer;
begin
  Result:=False;
  SkipBlanks;
  if FBuffer[FSourcePtr] = 0 then
    Exit;
  FTokenPtr:=FSourcePtr;
  case CharType(FSourcePtr) of
    lcLetter: begin
      NextChar(True);
      while CharType(FSourcePtr) in [lcLetter, lcNumber] do
        NextChar(True);
      CheckAlphaNumeric(FTokenPtr, FSourcePtr, FToken);
      Result:=True;
    end;
    lcRightParenthesis: begin
      FToken:=ltSymbol;
      Result:=True;
      NextChar(True);
      if CharType(FSourcePtr) = lcAsterisk then begin // its a comment, type '(*'
        NextChar(True);
        FToken:=ltComment;
      end
      else
        PreviousChar;
    end;
    lcSlash: begin
      FToken:=ltSymbol;
      Result:=True;
      NextChar(True);
      if CharType(FSourcePtr) = lcSlash then begin
        NextChar(True);
        FToken:=ltCommentLine;
      end
      else
        PreviousChar;
    end;
{    lcHash, lcQuote: begin
      IsWideStr:=False;
      J:=0;
      S:=P;
      while True do begin
        case AnsiChar(FBuffer[P]) of
          '#': begin
            Inc(P);
            I:=0;
            while AnsiChar(FBuffer[P]) in ['0'..'9'] do begin
              I:=I * 10 + (FBuffer[P] - Ord('0'));
              Inc(P);
            end;
            if (I > 127) then
              IsWideStr:=True;
            Inc(J);
          end;
          '''': begin
            Inc(P);
            while True do begin
              case AnsiChar(FBuffer[P]) of
                #0, #10, #13: begin
                  //Break;
                  Error('Invalid String!');
                end;
                '''': begin
                  Inc(P);
                  if AnsiChar(FBuffer[P]) <> '''' then
                    Break;
                end;
              end;
              Inc(J);
              Inc(P);
            end;
          end;
        else
          Break;
        end;
      end;
      P:=S;
      if IsWideStr then
        SetLength(FWideStr, J);
      J:=1;
      while True do begin
        case AnsiChar(FBuffer[P]) of
          '#': begin
            Inc(P);
            I:=0;
            while AnsiChar(FBuffer[P]) in ['0'..'9'] do begin
              I:=I * 10 + (FBuffer[P] - Ord('0'));
              Inc(P);
            end;
            if IsWideStr then begin
              FWideStr[J]:=WideChar(SmallInt(I));
              Inc(J);
            end
            else begin
              FBuffer[S]:=I;
              Inc(S);
            end;
          end;
          '''': begin
            Inc(P);
            while True do begin
              case AnsiChar(FBuffer[P]) of
                #0, #10, #13: begin
                  //Break;
                  Error('Invalid String!');
                end;
                '''': begin
                  Inc(P);
                  if AnsiChar(FBuffer[P]) <> '''' then
                    Break;
                end;
              end;
              if IsWideStr then begin
                FWideStr[J]:=WideChar(FBuffer[P]);
                Inc(J);
              end
              else begin
                FBuffer[S]:=FBuffer[P];
                Inc(S);
              end;
              Inc(P);
            end;
          end;
        else
          Break;
        end;
      end;
//      FStringPtr:=S;
      Result:=True;
      FToken:=ltString;
    end;}
    lcDollar: begin
      Result:=True;
      FToken:=ltSymbol;
      if GetBufferChar(1) in ['0'..'9', 'A'..'F', 'a'..'f'] then begin
        NextChar(True);
        while BufferChar in ['0'..'9', 'A'..'F', 'a'..'f'] do
          NextChar(True);
        Result:=True;
        FToken:=ltInteger;
      end;
    end;
    lcMinus, lcNumber: begin
      Result:=True;
      FToken:=ltSymbol;
      FFloatType:=#0;
      if (BufferChar = '-') and not (GetBufferChar(1) in ['0'..'9']) then begin
        FCanMoveToNext:=True;
        Exit;
      end;
      NextChar(True);
      while BufferChar in ['0'..'9'] do
        NextChar(True);
      FToken:=ltInteger;
      while BufferChar in ['0'..'9', '.', 'e', 'E', '+', '-'] do begin
        NextChar(True);
        FToken:=ltFloat;
      end;
      if BufferChar in ['c', 'C', 'd', 'D', 's', 'S', 'f', 'F'] then begin
        FFloatType:=BufferChar;
        NextChar(True);
        FToken:=ltFloat;
      end;
    end;
  else
    Result:=(BufferChar <> #0) and (BufferChar > #31);
    if Result then begin
      FToken:=ltSymbol;
      FCanMoveToNext:=True;
    end
    else if BufferChar = #0 then
      FToken:=ltEOF
    else if (BufferChar > #0) and (BufferChar < #32) then begin
      Result:=True;
      FToken:=ltControlChar;
    end;
  end;
end;

function TPascalTokenizer.TokenString: String;
var
  L: Cardinal;
  LTokenPtr: Cardinal;
begin
  L:=0;
  LTokenPtr:=FTokenPtr;
  if FIsStoredPtr then begin
    LTokenPtr:=FStoredPtr;
    L:=FSourcePtr - LTokenPtr;
    FIsStoredPtr:=False;
  end
  else begin
    if FToken in [ltKeyWord, ltVariable, ltString, ltComment, ltLambda] then
      L:=FSourcePtr - LTokenPtr;
  end;
  Result:=FEncoding.GetString(FBuffer, LTokenPtr, L);
end;

function TPascalTokenizer.TokenIsKeyWord(const S: String): Boolean;
begin
  Result:=(Token = ltKeyWord) and SameText(S, TokenString);
end;

function TPascalTokenizer.CheckAlphaNumeric(AlphaNumeric: String): TSourceTokenizer.TLexerTokenType;
var
  idx: Integer;
begin
  AlphaNumeric:=LowerCase(AlphaNumeric);
  idx:=FKeyWords.IndexOf(AlphaNumeric);
  if idx > -1 then
    Result:=ltKeyWord
  else begin
    if Length(AlphaNumeric) > 1 then
      Result:=ltString
    else
      Result:=ltSymbol;
  end;
end;

{ TPascalSourceTree }

function TPascalSourceTree.GetReaderClass: TSourceReaderClass;
begin
  Result:=TPascalTokenizer;
end;

procedure TPascalSourceTree.Parse(Reader: TSourceReader);
var
  Parent: TSourceNode;
begin
  Clear;
  FInsideLambda:=0;
  Parent:=Document;
  while Reader.NextToken do begin
    if (Reader.Token in [ltControlChar, ltSymbol]) and (Reader.BufferChar in [#9, #10, #13, #32]) then
      Continue;
    if Reader.Token = ltKeyWord then
      ParseKeyWord(Reader, Parent)
    else if Reader.Token = ltString then
      ParseString(Reader, Parent)
//    else if Reader.Token = ltLambda then
//      ParseLambda(Reader, Parent)
    else if Reader.Token = ltInteger then
      ParseInteger(Reader, Parent)
    else if Reader.Token = ltFloat then
      ParseFloat(Reader, Parent)
    else if Reader.Token in [ltComment..ltCommentLine] then
      ParseComment(Reader, Parent)
    else if Reader.Token <> ltControlChar then
      raise ESourceParseException.CreateFmt(sUnknownIdentifier, [Reader.SourceLine, Reader.LinePos]);
  end;
end;

procedure TPascalSourceTree.ParseKeyWord(Reader: TSourceReader; Parent: TSourceNode);
var
  LKeyWord: String;
  Node: TSourceNode;
begin
  LKeyWord:=LowerCase(Reader.TokenString);
  try
    Node:=Parent.AddChild(LKeyWord, stKeyWord);
    Node.SourceLine:=Reader.SourceLine;
    Node.LinePos:=Reader.LinePos;
    if Reader.TokenIsKeyWord('unit') then begin
      Node.Free;
      FRoot.SourceLine:=Reader.SourceLine;
      FRoot.LinePos:=Reader.LinePos;
      FRoot.NodeName:=Trim(Reader.TokenLine([';']));
      Reader.CheckLastSymbol(';', True);
      Reader.NextChar(True);
    end
    else if Reader.TokenIsKeyWord('interface') or Reader.TokenIsKeyWord('implementation') then
      ParseSection(Reader, Node)
    else if Reader.TokenIsKeyWord('uses') then
      ParseUses(Reader, Node)
    else if Reader.TokenIsKeyWord('var') then
      ParseVariable(Reader, Node)
    else if Reader.TokenIsKeyWord('const') then
      ParseConst(Reader, Node)
    else if Reader.TokenIsKeyWord('type') then
      ParseType(Reader, Node)
    else if Reader.TokenIsKeyWord('begin') then
      ParseMainBlock(Reader, Node)
    else if Reader.TokenIsKeyWord('procedure') then
      ParseLambda(Reader, Node, sltProcedure)
    else if Reader.TokenIsKeyWord('function') then
      ParseLambda(Reader, Node, sltFunction);
  finally
    LKeyWord:='';
  end;
end;

procedure TPascalSourceTree.ParseSection(Reader: TSourceReader; Parent: TSourceNode);
begin
  while Reader.NextToken do begin
    if (Reader.Token in [ltControlChar, ltSymbol]) and (Reader.BufferChar in [#9, #10, #13, #32]) then
      Continue;
    if Reader.Token = ltKeyWord then begin
      if Reader.TokenIsKeyWord('implementation') then
        Parent:=Parent.ParentNode;
      ParseKeyWord(Reader, Parent);
    end
    else if Reader.Token = ltString then
      ParseString(Reader, Parent)
    else if Reader.Token = ltInteger then
      ParseInteger(Reader, Parent)
    else if Reader.Token = ltFloat then
      ParseFloat(Reader, Parent)
    else if Reader.Token in [ltComment..ltCommentLine] then
      ParseComment(Reader, Parent)
    else if Reader.Token <> ltControlChar then
      raise ESourceParseException.CreateFmt(sUnknownIdentifier, [Reader.SourceLine, Reader.LinePos]);
  end;
end;

procedure TPascalSourceTree.ParseUses(Reader: TSourceReader; Parent: TSourceNode);
var
  Node: TSourceNode;
begin
  Node:=Parent;
  while Reader.NextToken do begin
    if (Reader.Token in [ltControlChar, ltSymbol]) and (Reader.BufferChar in [#9, #10, #13, #32]) then
      Continue;
    if (Reader.Token = ltSymbol) and (Reader.BufferChar = ';') then begin
      Reader.NextChar(True);
      Exit;
    end
    else begin
      Node.AddAttr('include', Trim(Reader.TokenLine([',', ';'], True))); // read to line end
      if Reader.CheckToken(ltSymbol, ';') then begin
        Reader.NextChar(True);
        Exit;
      end
      else begin
        Reader.CheckLastSymbol(',', True);
        Reader.NextChar(True);
      end;
    end;
  end;
end;

procedure TPascalSourceTree.ParseVariable(Reader: TSourceReader; Parent: TSourceNode);
var
  LVarName, LVarType: String;
  Node: TSourceNode;
begin
  while Reader.NextToken do begin
    if (Reader.Token in [ltControlChar, ltSymbol]) and (Reader.BufferChar in [#9, #10, #13, #32]) then
      Continue;
    if Reader.Token = ltKeyWord then begin
      if Reader.TokenIsKeyWord('begin') and (FInsideLambda > 0) then
        Exit;
      ParseKeyWord(Reader, Parent.ParentNode);
      Exit;
    end
    else if Reader.Token in [ltComment..ltCommentLine] then
      ParseComment(Reader, Parent)
    else begin
      Node:=Parent.AddChild('', stIdentifier, sstVariable);
      Node.SourceLine:=Reader.SourceLine;
      Node.LinePos:=Reader.LinePos;
      try
        Reader.CheckToken(ltString);
        LVarName:=Reader.TokenString;
        Reader.NextToken;
        Reader.CheckLastSymbol(':', True);
        Reader.NextToken;
        Reader.CheckToken([ltKeyWord, ltString], sTypeExpected);
        LVarType:=Reader.TokenString;
        Reader.NextToken;
        Reader.CheckLastSymbol(';', True);
        Node.NodeName:=LVarName;
        Node.NodeValue:=LVarType;
      except
        Node.Free;
        raise;
      end;
    end;
  end;
end;

procedure TPascalSourceTree.ParseConst(Reader: TSourceReader; Parent: TSourceNode);
var
  LVarName, LVarType, LVarValue: String;
  Node: TSourceNode;
begin
  while Reader.NextToken do begin
    if (Reader.Token in [ltControlChar, ltSymbol]) and (Reader.BufferChar in [#9, #10, #13, #32]) then
      Continue;
    if Reader.Token = ltKeyWord then begin
      if Reader.TokenIsKeyWord('begin') and (FInsideLambda > 0) then
        Exit;
      ParseKeyWord(Reader, Parent.ParentNode);
      Exit;
    end
    else if Reader.Token in [ltComment..ltCommentLine] then
      ParseComment(Reader, Parent)
    else begin
      LVarName:='';
      LVarType:='';
      LVarValue:='';
      Node:=Parent.AddChild('', stIdentifier, sstConst);
      Node.SourceLine:=Reader.SourceLine;
      Node.LinePos:=Reader.LinePos;
      try
        try
          Reader.CheckToken(ltString);
          LVarName:=Reader.TokenString;
          Reader.NextToken;
          if Reader.CheckToken(ltSymbol, ':') then begin // type declared
            Reader.NextChar(True);
            Reader.NextToken;
            Reader.CheckToken([ltKeyWord, ltString], sTypeExpected);
            LVarType:=Trim(Reader.TokenString);
            Reader.NextToken;
            if Reader.CheckToken(ltSymbol, '=') then begin // initial value set
              Reader.NextChar(True);
              Reader.NextToken;
              //Reader.CheckToken([ltKeyWord, ltString], sTypeExpected);
              LVarValue:=Trim(Reader.TokenLine([';']));
            end;
          end
          else if Reader.CheckToken(ltSymbol, '=') then begin // initial value set
            Reader.NextChar(True);
            Reader.NextToken;
            //Reader.CheckToken([ltKeyWord, ltString], sTypeExpected);
            LVarValue:=Trim(Reader.TokenLine([';']));
          end;
          Reader.CheckLastSymbol(';', True);
          Node.NodeName:=LVarName;
          Node.NodeValueType:=LVarType;
          Node.NodeValue:=LVarValue;
        except
          Node.Free;
          raise;
        end;
      finally
        LVarName:='';
        LVarType:='';
        LVarValue:='';
      end;
    end;
  end;
end;

procedure TPascalSourceTree.ParseType(Reader: TSourceReader; Parent: TSourceNode);
var
  LTypeName, LTypeType: String;
  LTypeIsPointerTo: Boolean;
  Node: TSourceNode;
begin
  while Reader.NextToken do begin
    if (Reader.Token in [ltControlChar, ltSymbol]) and (Reader.BufferChar in [#9, #10, #13, #32]) then
      Continue;
    if Reader.Token = ltKeyWord then begin
      if Reader.TokenIsKeyWord('begin') and (FInsideLambda > 0) then
        Exit;
      ParseKeyWord(Reader, Parent.ParentNode);
      Exit;
    end
    else if Reader.Token in [ltComment..ltCommentLine] then
      ParseComment(Reader, Parent)
    else begin
      LTypeName:='';
      LTypeType:='';
      Node:=Parent.AddChild('', stIdentifier, sstType);
      Node.SourceLine:=Reader.SourceLine;
      Node.LinePos:=Reader.LinePos;
      try
        try
          Reader.CheckToken(ltString);
          LTypeName:=Reader.TokenString;
          Node.NodeName:=LTypeName;
          Reader.NextToken;
          Reader.CheckLastSymbol('=', True);
          Reader.NextToken; // class, record, set, enum, array, pointer to something (^....)
          LTypeIsPointerTo:=False;
          if Reader.CheckToken(ltSymbol, '^') then begin
            LTypeIsPointerTo:=True;
            Reader.NextChar(True);
            Reader.NextToken;
          end;
          if Reader.TokenIsKeyWord('class') then begin
            Node.NodeSubType:=sstClass;
            Reader.NextToken;
            if Reader.CheckToken(ltSymbol, '(') then begin
              Reader.NextToken;
              LTypeType:=Reader.TokenLine([')']);
              Node.NodeValueType:=LTypeType;
              Reader.CheckLastSymbol(')', True);
              Reader.NextChar(True);
            end
            else if Reader.CheckLastSymbol(';') then begin
              Reader.NextChar(True);
              Continue;
            end;
            Reader.NextToken;
            // read class definition
          end
          else if Reader.TokenIsKeyWord('record') then begin
          end
          else if Reader.TokenIsKeyWord('set') then begin
          end
          else if Reader.TokenIsKeyWord('array') then begin
          end
          else if Reader.CheckLastSymbol('(') then begin // enum
          end
          else if Reader.CheckToken(ltKeyWord, #0) then begin
          end;
        except
          Node.Free;
          raise;
        end;
      finally
        LTypeName:='';
        LTypeType:='';
      end;
    end;
  end;
end;

procedure TPascalSourceTree.ParseString(Reader: TSourceReader; Parent: TSourceNode);
begin

end;

procedure TPascalSourceTree.ParseComment(Reader: TSourceReader; Parent: TSourceNode);
var
  Node: TSourceNode;
begin
  case Reader.Token of
    ltComment: begin // (*.....*)

    end;
    ltCommentBlock: begin // {.....}

    end;
    ltCommentLine: begin // //.....
      Node:=Parent.AddChild('', stComment, sstCommentLine);
      Node.SourceLine:=Reader.SourceLine;
      Node.LinePos:=Reader.LinePos;
      Node.NodeValue:=TrimRight(Reader.TokenLine); // read to line end
    end;
  end;
end;

procedure TPascalSourceTree.ParseInteger(Reader: TSourceReader; Parent: TSourceNode);
begin

end;

procedure TPascalSourceTree.ParseFloat(Reader: TSourceReader; Parent: TSourceNode);
begin

end;

procedure TPascalSourceTree.ParseMainBlock(Reader: TSourceReader; Parent: TSourceNode);
var
  Node: TSourceNode;
begin
  Node:=Parent;
  while Reader.NextToken do begin
    if (Reader.Token in [ltControlChar, ltSymbol]) and (Reader.BufferChar in [#9, #10, #13, #32]) then
      Continue;
    if (Reader.Token = ltKeyWord) and (Reader.TokenString = 'end') and (Reader.BufferChar = '.') then begin
      Reader.NextChar(True);
      Exit;
    end
    else begin
      Node.AddAttr('line', Trim(Reader.TokenLine(True))); // read to line end
    end;
  end;
end;

procedure TPascalSourceTree.ParseLambda(Reader: TSourceReader; Parent: TSourceNode; LambdaType: TSourceNodeLambdaType);
begin
  Inc(FInsideLambda);
  try
    Parent.NodeType:=stLambda;
    if LambdaType = sltProcedure then
      Parent.NodeSubType:=sstProcedure
    else
      Parent.NodeSubType:=sstFunction;
    ParseLambdaHeader(Reader, Parent, LambdaType);
    ParseLambdaBody(Reader, Parent, LambdaType);
  finally
    Dec(FInsideLambda);
  end;
end;

procedure TPascalSourceTree.ParseLambdaHeader(Reader: TSourceReader; Parent: TSourceNode; LambdaType: TSourceNodeLambdaType);
var
  LLambdaName: String;
  Node: TSourceNode;
begin
  Node:=Parent;
  Reader.NextToken;
  Reader.CheckToken(ltString);
  LLambdaName:=Reader.TokenString;
  Node.NodeName:=LLambdaName;
  LLambdaName:='';
  ParseLambdaParams(Reader, Parent, LambdaType);
end;

procedure TPascalSourceTree.ParseLambdaParams(Reader: TSourceReader; Parent: TSourceNode; LambdaType: TSourceNodeLambdaType);
begin
  Reader.NextToken;
  if Reader.CheckToken(ltSymbol, '(') then begin
    Reader.NextChar(True);
    Reader.StoreSourcePtr;
    while Reader.NextToken and not Reader.CheckToken(ltSymbol, ')') and not Reader.CheckToken(ltSymbol, ';') do begin
      if (Reader.Token in [ltControlChar, ltSymbol]) and (Reader.BufferChar in [#9, #10, #13, #32]) then
        Continue;
      Reader.StoreTokenPtr;
      while Reader.NextToken and not Reader.CheckToken(ltSymbol, ')') and not Reader.CheckToken(ltSymbol, ';') do begin
        if (Reader.Token in [ltControlChar, ltSymbol]) and (Reader.BufferChar in [#9, #13, #32]) then
          Continue;

        if not (Reader.Token in [ltSymbol..ltCommentLine]) then
          raise ESourceParseException.CreateFmt(sUnexpectedIdentifier, [Reader.SourceLine, Reader.LinePos]);
      end;
      Parent.AddAttr('param', Trim(Reader.TokenString));
      if Reader.CheckLastSymbol(')') then
        Break;
    end;
    Reader.NextToken;
  end;
  if LambdaType = sltFunction then begin
    Reader.CheckLastSymbol(':', True);
    Reader.NextChar(True);
    Reader.StoreSourcePtr;
    while Reader.NextToken and not Reader.CheckToken(ltSymbol, ';') do begin
      if (Reader.Token in [ltControlChar, ltSymbol]) and (Reader.BufferChar in [#9, #13, #32]) then
        Continue;

      if not (Reader.Token in [ltSymbol..ltCommentLine]) then
        raise ESourceParseException.CreateFmt(sUnexpectedIdentifier, [Reader.SourceLine, Reader.LinePos]);
    end;
    Parent.SetAttr('return', Trim(Reader.TokenString));
  end;
  Reader.CheckLastSymbol(';', True);
end;

procedure TPascalSourceTree.ParseLambdaBody(Reader: TSourceReader; Parent: TSourceNode; LambdaType: TSourceNodeLambdaType);
var
  Node: TSourceNode;
  level: Integer;
  LKeyWord: String;
label
  jump_to;
begin
  level:=0;
  Node:=Nil;
  while Reader.NextToken do begin
    if (Reader.Token in [ltControlChar, ltSymbol]) and (Reader.BufferChar in [#9, #10, #13, #32]) then
      Continue;
    if Reader.TokenIsKeyWord('begin') then begin
      jump_to:
      Inc(level);
      if (level = 1) and (Node = Nil) then begin
        LKeyWord:=LowerCase(Reader.TokenString);
        Node:=Parent.AddChild(LKeyWord, stKeyWord);
        Node.SourceLine:=Reader.SourceLine;
        Node.LinePos:=Reader.LinePos;
        LKeyWord:='';
        Parent:=Node;
      end;
    end
    else if Reader.TokenIsKeyWord('end') then begin
      Dec(level);
      Reader.NextToken;
      Reader.CheckLastSymbol(';', True);
      if level = 0 then
        Exit;
      if level < 0 then
        level:=0;
    end
    else if (Reader.Token = ltKeyWord) and (Node = Nil) then begin
      ParseKeyWord(Reader, Parent);
      if Reader.TokenIsKeyWord('begin') then
        goto jump_to;
    end
    else if Reader.Token in [ltKeyWord..ltCommentLine] then begin
      if (Node <> Nil) and (level > 0) then begin
        Node.AddAttr('line', Trim(Reader.TokenLine(True)));
      end;
    end;
  end;
end;

{$ENDREGION}

end.
