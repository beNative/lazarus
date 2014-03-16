unit OXmlReadWrite;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    MPL 1.1 / GPLv2 / LGPLv2 / FPC modified LGPLv2
    Please see the /license.txt file for more information.

}

{
  OXmlReadWrite.pas

  Basic XML reader/writer. OXmlIntfDOM.pas, OXmlPDOM.pas and OXmlSAX.pas use
  this unit to read and write XML.

  TXMLWriter
    - fast sequential XML writer
    - no real DOM validity checking - the programmer has to know what he is doing
    - supports escaping of text - you should pass unescaped text to every function
      (if not stated differently) and the writer takes care of valid XML escaping
    - all line breaks (#10, #13, #13#10) are automatically changed to LineBreak
      -> default value for LineBreak is your OS line break (sLineBreak)
      -> if you don't want to process them, set LineBreak to lbDoNotProcess
    - supports automatic indentation of XML
    - don't use it directly. If performance is crucial for you, use SAX
      which has the same performance.


  TXMLReader
    - fast sequential XML reader/parser
    - the nodes are returned as they are found in the document
    - absolutely no whitespace handling - the document is parsed exactly 1:1
      -> white space is preserved also in the very beginning of the document
      -> you have to care for white space handling in end-level
    - only line breaks (#10, #13, #13#10) are automatically changed to LineBreak
      -> default value for LineBreak is your OS line break (sLineBreak)
      -> if you don't want to process them, set LineBreak to lbDoNotProcess

}

{$I OXml.inc}

{$IFDEF O_DELPHI_XE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$BOOLEVAL OFF}

interface

uses
  {$IFDEF O_NAMESPACES}
  System.SysUtils, System.Classes,
  {$ELSE}
  SysUtils, Classes,
  {$ENDIF}

  {$IFDEF O_GENERICS}
    {$IFDEF O_NAMESPACES}
    System.Generics.Collections,
    {$ELSE}
    Generics.Collections,
    {$ENDIF}
  {$ENDIF}

  OWideSupp, OXmlUtils, OTextReadWrite, OEncoding;

type
  TXMLWriter = class;

  TXMLWriterElementMode = (
    stOpenOnly,    //"<node"
    stFinish,       //"<node>"
    stFinishClose); //"<node/>"

  PXMLWriterElement = ^TXMLWriterElement;
  TXMLWriterElement = {$IFDEF O_EXTRECORDS}record{$ELSE}object{$ENDIF}
  private
    fOwner: TXMLWriter;
    fElementName: OWideString;
    fOpenElementFinished: Boolean;
    fChildrenWritten: Boolean;

    procedure FinishOpenElement;
  public
    // <aElementName ... >, <aElementName ... /> etc.
    procedure OpenElement(const aElementName: OWideString; const aMode: TXMLWriterElementMode = stOpenOnly);
    procedure OpenElementR(const aElementName: OWideString;
      var outElement: TXMLWriterElement;
      const aMode: TXMLWriterElementMode = stOpenOnly); overload;
    function OpenElementR(const aElementName: OWideString;
      const aMode: TXMLWriterElementMode = stOpenOnly): TXMLWriterElement; overload;

    // </fElementName> or "/>" if no children were written
    procedure CloseElement;

    // write attribute of an element or declaration
    procedure Attribute(const aAttrName, aAttrValue: OWideString);

    // <![CDATA[aText]]>
    procedure CData(const aText: OWideString);
    // <!--aText-->
    procedure Comment(const aText: OWideString);
    // <?aTarget aContent?>
    procedure ProcessingInstruction(const aTarget, aContent: OWideString);

    // write escaped text, do not escape quotes
    procedure Text(const aText: OWideString);
  public
    property ElementName: OWideString read fElementName;
  end;

  TXMLWriterSettings = class(TPersistent)
  private
    fIndentString: OWideString;
    fIndentType: TXmlIndentType;
    fLineBreak: TXmlLineBreak;
    fStrictXML: Boolean;
    fWriteBOM: Boolean;
    fOnSetWriteBOM: TNotifyEvent;
  protected
    procedure SetWriteBOM(const aWriteBOM: Boolean);

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
  public
    //write BOM (if applicable)
    property WriteBOM: Boolean read fWriteBOM write SetWriteBOM;

    //process line breaks (#10, #13, #13#10) to the LineBreak character
    //  set to lbDoNotProcess if you don't want any processing
    //  default is your OS line break (XmlDefaultLineBreak)
    property LineBreak: TXmlLineBreak read fLineBreak write fLineBreak;
    //StrictXML: document must be valid XML
    //   = true: element names & values checking
    //   = false: no element names & values checking
    property StrictXML: Boolean read fStrictXML write fStrictXML;

    //indent type - default is none (no indent)
    property IndentType: TXmlIndentType read fIndentType write fIndentType;
    //indent string - default are two space characters (#32#32)
    property IndentString: OWideString read fIndentString write fIndentString;
  end;

  TXMLWriter = class(TObject)
  private
    fWriter: TOTextWriter;
    fWriterSettings: TXMLWriterSettings;
    fDefaultIndentLevel: Integer;
    fWritten: Boolean;

    fIndentLevel: Integer;

    function GetEncoding: TEncoding;
    function GetOwnsEncoding: Boolean;
    procedure SetEncoding(const aEncoding: TEncoding);
    procedure SetOwnsEncoding(const aOwnsEncoding: Boolean);
    procedure SetDefaultIndentLevel(const aDefaultIndentLevel: Integer);
    procedure OnSetWriteBOM(Sender: TObject);
  protected
    //manual indentation support - you can use Indent+IncIndentLevel+DecIndentLevel
    //  manually if you want to. Set IndentType to itNone in this case.
    procedure Indent;
    procedure IncIndentLevel;
    procedure DecIndentLevel;
  protected
    procedure DoCreate;
    procedure DoInit; virtual;
  public
    //create
    constructor Create; overload;
    //create and init
    constructor Create(const aStream: TStream); overload;

    destructor Destroy; override;
  public
    //The Init* procedures initialize a document for writing.
    // Please note that the file/stream/... is locked until you destroy
    // TXMLWriter or call ReleaseDocument!

    procedure InitFile(const aFileName: String);
    procedure InitStream(const aStream: TStream);

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument;
  public
    // default <?xml ?> declaration
    procedure XMLDeclaration(const aEncodingAttribute: Boolean = True;
      const aVersion: OWideString = '1.0';
      const aStandAlone: OWideString = '');
    // <?xml
    procedure OpenXMLDeclaration;
    // ?>
    procedure FinishOpenXMLDeclaration;

    // <aElementName ... >, <aElementName ... /> etc.
    procedure OpenElement(const aElementName: OWideString;
      const aMode: TXMLWriterElementMode = stOpenOnly);
    procedure OpenElementR(const aElementName: OWideString;
      var outElement: TXMLWriterElement;
      const aMode: TXMLWriterElementMode = stOpenOnly); overload;
    function OpenElementR(const aElementName: OWideString;
      const aMode: TXMLWriterElementMode = stOpenOnly): TXMLWriterElement; overload;
    // >
    procedure FinishOpenElement(const {%H-}aElementName: OWideString = '');//you may pass a ElementName just to make it clear for you which element you want to close
    // />
    procedure FinishOpenElementClose(const {%H-}aElementName: OWideString = '');//you may pass a ElementName just to make it clear for you which element you want to close
    // </aElementName>, decide if you want to indent
    procedure CloseElement(const aElementName: OWideString; const aIndent: Boolean = True);

    // write attribute of an element or declaration
    procedure Attribute(const aAttrName, aAttrValue: OWideString);
    // <![CDATA[aText]]>
    procedure CData(const aText: OWideString);
    // <!--aText-->
    procedure Comment(const aText: OWideString);
    // <?aTarget aContent?>
    procedure ProcessingInstruction(const aTarget, aContent: OWideString);
    // <!DOCTYPE aDocTypeRawText> - aDocTypeRawText must be escaped, it won't be processed
    procedure DocType(const aDocTypeRawText: OWideString);

    // write escaped text, escape also a quote if aQuoteChar specified
    procedure Text(const aText: OWideString; const aQuoteChar: OWideChar); overload;
    // write escaped text, decide if you want to indent
    procedure Text(const aText: OWideString; const aIndent: Boolean = True); overload;
    // write raw text, do not process it
    procedure RawText(const aText: OWideString);
    procedure RawChar(const aChar: OWideChar);
  public
    //encoding of the text writer
    property Encoding: TEncoding read GetEncoding write SetEncoding;
    property OwnsEncoding: Boolean read GetOwnsEncoding write SetOwnsEncoding;

    //indentation level - you can change it only if nothing has been written yet
    //  and after the Init* call
    property DefaultIndentLevel: Integer read fDefaultIndentLevel write SetDefaultIndentLevel;

    property WriterSettings: TXMLWriterSettings read fWriterSettings;
  end;

  TXMLReaderTokenType = (
    rtDocumentStart,//start of reading
    rtOpenXMLDeclaration,//xml declaration open element: <?xml
    rtXMLDeclarationAttribute,//attribute in an xml declaration: name="value"
    rtFinishXMLDeclarationClose,//xml declaration element finished and closed: ?>
    rtOpenElement,//open element: <name
    rtAttribute,//attribute: name="value"
    rtFinishOpenElement,//open element finished but not closed: <node ... ">"
    rtFinishOpenElementClose,//open element finished and closed: <node ... "/>"
    rtCloseElement,//close element: "</node>"
    rtText,//text: value
    rtCData,//cdata: <![CDATA[value]]>
    rtComment,//comment: <!--value-->
    rtProcessingInstruction,//custom processing instruction: <?target content?>
    rtDocType//doctype: <!DOCTYPE value> -> value is not unescaped by reader!!!
    );

  TXMLReaderToken = record
    TokenName: OWideString;
    TokenValue: OWideString;
    TokenType: TXMLReaderTokenType;
  end;
  PXMLReaderToken = ^TXMLReaderToken;
  TXMLReaderTokenList = class(TObject)
  private
    {$IFDEF O_GENERICS}
    fReaderTokens: TList<PXMLReaderToken>;
    {$ELSE}
    fReaderTokens: TList;
    {$ENDIF}

    fCount: Integer;

    function GetToken(const aIndex: Integer): PXMLReaderToken;

    procedure AddLast;
    function CreateNew: PXMLReaderToken;
  public
    procedure Clear;
    procedure DeleteLast;

    function IndexOf(const aToken: PXMLReaderToken): Integer;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property Count: Integer read fCount;
    property Items[const aIndex: Integer]: PXMLReaderToken read GetToken; default;
  end;

  TXMLReader = class;
  TXMLReaderSettings = class;

  TXMLReaderSettings = class(TPersistent)
  private
    fBreakReading: TXmlBreakReading;
    fLineBreak: TXmlLineBreak;
    fStrictXML: Boolean;
    fEntityList: TXMLReaderEntityList;
    fRecognizeXMLDeclaration: Boolean;
    fErrorHandling: TXMLReaderErrorHandling;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
  public
    //process known entities. add user-defined entities here
    property EntityList: TXMLReaderEntityList read fEntityList;
    //decide if you want to read the document after the root element has been closed
    property BreakReading: TXMLBreakReading read fBreakReading write fBreakReading;
    //process line breaks (#10, #13, #13#10) to the LineBreak character
    //  set to lbDoNotProcess if you don't want any processing
    //  default is your OS line break (XmlDefaultLineBreak)
    property LineBreak: TXMLLineBreak read fLineBreak write fLineBreak;
    //StrictXML: document must be valid XML
    //   = true: raise Exceptions when document is not valid
    //   = false: try to fix and go over document errors.
    property StrictXML: Boolean read fStrictXML write fStrictXML;
    //RecognizeXMLDeclaration
    //  if set to true the processing instruction "<?xml ... ?>" will be detected as XMLDeclaration
    //   and following element types will be fired:
    //   rtOpenXMLDeclaration, rtXMLDeclarationAttribute, rtFinishXMLDeclarationClose
    //  if set to false, it will be handled as a normal processing instruction
    //   rtProcessingInstruction
    property RecognizeXMLDeclaration: Boolean read fRecognizeXMLDeclaration write fRecognizeXMLDeclaration;

    //ErrorHandling
    //  determine if Exception should be raised
    //  ehSilent: do not raise any exceptions (check TXMLReader.ParseError for an error
    //    -> this is the standard behaviour of MSXML
    //  ehRaiseAndEat: an exception will be raised but it will be eaten automatically
    //    so that the user doesn't see it it appears only when debugging and it is available to error handling/logging
    //    -> this is the standard behaviour of OmniXML
    //    -> this is also the default behaviour of OXml
    //  ehRaise: an exception will be raised and the end-user sees it as well
    //    -> this is the standard behaviour of Delphi's own TXmlDocument
    //  ! if an error was found, the TXMLReader.ParseError will contain it in any case !
    property ErrorHandling: TXMLReaderErrorHandling read fErrorHandling write fErrorHandling;
  end;

  IXMLParseError = interface;
  TXMLParseError = class;
  TXMLParseErrorClass = class of TXMLParseError;

  TXMLReader = class(TObject)
  private
    fReaderSettings: TXMLReaderSettings;

    fAttributeTokens: TXMLReaderTokenList;//do not destroy!!!
    fOpenElementTokens: TXMLReaderTokenList;//destroy

    fReader: TOTextReader;
    fAllowSetEncodingFromFile: Boolean;//internal, for external use ForceEncoding

    fLastTokenType: TXMLReaderTokenType;//must be used because fReaderToken can be replaced! -> do not replace it with fReaderToken.TokenType!!!
    fLastTokenLinePosition: Integer;
    fLastTokenCharPosition: Integer;
    fDocumentElementFound: Boolean;
    fForceEncoding: Boolean;

    fElementsToClose: Integer;
    fReaderToken: PXMLReaderToken;//current reader
    fOwnsReaderToken: Boolean;

    fMainBuffer: TOTextBuffer;
    fEntityBuffer: TOTextBuffer;

    fParseError: IXMLParseError;

    function GetEncoding: TEncoding;
    procedure SetEncoding(const aEncoding: TEncoding);
    function GetOwnsEncoding: Boolean;
    procedure SetOwnsEncoding(const aSetOwnsEncoding: Boolean);
    function GetApproxStreamPosition: ONativeInt;
    function GetStreamSize: ONativeInt;
  private
    function GetNodePath(const aIndex: Integer): OWideString;
    function GetNodePathCount: Integer;
  private
    procedure ProcessEntity;
    procedure ProcessNewLineChar(const aLastChar: OWideChar);
    procedure ChangeEncoding(const aEncodingAlias: OWideString);
    procedure OpenElement;
    procedure Attribute;
    procedure FinishOpenElement;
    procedure FinishOpenElementClose;
    procedure CloseElement;
    procedure Text(const aClearCustomBuffer: Boolean = True);

    procedure ExclamationNode(const aTokenType: TXMLReaderTokenType;
      const aBeginTag, aEndTag: OWideString; const aWhiteSpaceAfterBeginTag: Boolean);
    procedure CData;
    procedure Comment;
    procedure DocType;
    procedure ProcessingInstruction;
    function GetFilePosition: Integer;
    function GetLinePosition: Integer;
    function GetLine: Integer;
    procedure DoRaiseException;
    procedure RaiseException(const aErrorClass: TXMLParseErrorClass;
      const aReason: String);
    procedure RaiseExceptionFmt(const aErrorClass: TXMLParseErrorClass;
      const aReason: String; const aArgs: array of const);
  protected
    procedure DoCreate;
    procedure DoDestroy;
    procedure DoInit(const aForceEncoding: TEncoding);
  protected
    procedure RemoveLastFromNodePath(const aCheckPath: Boolean);
    function NodePathIsEmpty: Boolean;
  public
    //create
    constructor Create; overload;
    //create and init
    constructor Create(const aStream: TStream; const aForceEncoding: TEncoding = nil); overload;

    destructor Destroy; override;
  public
    //The Init* procedures initialize a XML document for parsing.
    // Please note that the file/stream/... is locked until the end of the
    // document is reached, you destroy TXMLReader or you call ReleaseDocument!

    //init document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitFile(const aFileName: String; const aForceEncoding: TEncoding = nil);
    //init document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitStream(const aStream: TStream; const aForceEncoding: TEncoding = nil);
    //init XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    procedure InitXML(const aXML: OWideString);
    {$IFDEF O_RAWBYTESTRING}
    procedure InitXML_UTF8(const aXML: ORawByteString);
    {$ENDIF}
    {$IFDEF O_GENERICBYTES}
    //init document from TBytes buffer
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil);
    {$ENDIF}

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument;
  public
    //use ReadNextToken for reading next XML token
    function ReadNextToken(var outToken: PXMLReaderToken): Boolean;

    //create
    procedure SetAttributeTokens(const aAttributeTokens: TXMLReaderTokenList);
  public
    //following are functions to work with the current path in the XML document
    function NodePathMatch(const aNodePath: OWideString): Boolean; overload;
    function NodePathMatch(const aNodePath: TOWideStringList): Boolean; overload;
    function NodePathMatch(const aNodePath: Array of OWideString): Boolean; overload;
    function RefIsChildOfNodePath(const aRefNodePath: TOWideStringList): Boolean; overload;
    function RefIsChildOfNodePath(const aRefNodePath: Array of OWideString): Boolean; overload;
    function RefIsParentOfNodePath(const aRefNodePath: TOWideStringList): Boolean; overload;
    function RefIsParentOfNodePath(const aRefNodePath: Array of OWideString): Boolean; overload;
    procedure NodePathAssignTo(const aNodePath: TOWideStringList);
    function NodePathAsString: OWideString;

    //current path in XML document
    property NodePath[const aIndex: Integer]: OWideString read GetNodePath;
    //count of elements in path
    property NodePathCount: Integer read GetNodePathCount;
  public
    //encoding of the text file, when set, the file will be read again from the start
    property Encoding: TEncoding read GetEncoding write SetEncoding;
    property OwnsEncoding: Boolean read GetOwnsEncoding write SetOwnsEncoding;
    //if set to true, the encoding will not be changed automatically when
    //  <?xml encoding="..."?> is found
    property ForceEncoding: Boolean read fForceEncoding write fForceEncoding;

    //Reader Settings
    property ReaderSettings: TXMLReaderSettings read fReaderSettings;
  public
    //Approximate position in original read stream
    //  exact position cannot be determined because of variable UTF-8 character lengths
    property ApproxStreamPosition: ONativeInt read GetApproxStreamPosition;
    //size of original stream
    property StreamSize: ONativeInt read GetStreamSize;

    //Character position in text
    //  -> in Lazarus, the position is always in UTF-8 characters (no way to go around that since Lazarus uses UTF-8).
    //  -> in Delphi the position is always correct
    property FilePosition: Integer read GetFilePosition;//absolute character position in file (in character units, not bytes), 1-based
    property LinePosition: Integer read GetLinePosition;//current character in line, 1-based
    property Line: Integer read GetLine;//current line, 1-based
    //Position where last token was read
    property LastTokenLinePosition: Integer read fLastTokenLinePosition;
    property LastTokenCharPosition: Integer read fLastTokenCharPosition;

    property ParseError: IXMLParseError read fParseError;
  end;

  EXmlWriterException = class(Exception);
  EXmlWriterInvalidString = class(EXmlWriterException);
  EXmlReaderException = class(Exception)
  private
    fReason: String;
    fFilePos: Integer;
    fLinePos: Integer;
    fLine: Integer;
    fLastTokenLinePos: Integer;
    fLastTokenCharPos: Integer;
    fSrcText: OWideString;
    fSrcTextPos: Integer;
  protected
    procedure DoCreate(const aError: TXMLParseError; const aReason: string);
  public
    constructor Create(const aError: TXMLParseError; const aReason: string);
  public
    //formatted error text
    function GetFormattedErrorText: OWideString;
    //error code
    class function GetErrorCode: Integer; virtual;
  public
    //error code
    property ErrorCode: Integer read GetErrorCode;
    //reason
    property Reason: String read fReason;
    //Character position in text (when error was detected)
    //  -> in Lazarus, the position is always in UTF-8 characters (no way to go around that since Lazarus uses UTF-8).
    //  -> in Delphi the position is always correct
    property FilePos: Integer read fFilePos;//absolute character position in file (in character units, not bytes), 1-based
    property LinePos: Integer read fLinePos;//current character in line, 1-based
    property Line: Integer read fLine;//current line, 1-based
    //Position where last token was read (before the error)
    property LastTokenCharPos: Integer read fLastTokenCharPos;//1-based
    property LastTokenLinePos: Integer read fLastTokenLinePos;//1-based
    //Source code stub
    property SrcText: OWideString read fSrcText;
    //position of error in SrcText, 1-based
    property SrcTextPos: Integer read fSrcTextPos;
  end;
  EXmlReaderExceptionClass = class of EXmlReaderException;
  EXmlReaderInvalidCharacter = class(EXmlReaderException)
  public
    class function GetErrorCode: Integer; override;
  end;
  EXmlReaderInvalidStructure = class(EXmlReaderException)
  public
    class function GetErrorCode: Integer; override;
  end;

  IXMLParseError = interface
    ['{B2008FD1-65E2-44BE-97DA-38D42E44326C}']

    function GetExceptionClass: EXmlReaderExceptionClass;
    function GetErrorCode: Integer;
    function GetURL: OWideString;
    function GetReason: OWideString;
    function GetSrcText: OWideString;
    function GetSrcTextPos: Integer;
    function GetLine: Integer;
    function GetLinePos: Integer;
    function GetFilePos: Integer;

    procedure RaiseException;
    procedure RaiseAndEatException;

    property ErrorCode: Integer read GetErrorCode;
    property FilePos: Integer read GetFilePos;
    property Line: Integer read GetLine;
    property LinePos: Integer read GetLinePos;
    property Reason: OWideString read GetReason;
    property SrcText: OWideString read GetSrcText;
    property SrcTextPos: Integer read GetSrcTextPos;
    property URL: OWideString read GetURL;
  end;

  TXMLParseError = class(TInterfacedObject, IXMLParseError)
  private
    fURL: string;
    fReason: OWideString;
    fSrcText: OWideString;
    fSrcTextPos: Integer;
    fLine: Integer;
    fLinePos: Integer;
    fFilePos: Integer;
    fLastTokenLinePos: Integer;
    fLastTokenCharPos: Integer;
  protected
    function GetExceptionClass: EXmlReaderExceptionClass; virtual; abstract;
    function GetErrorCode: Integer;
    function GetURL: OWideString;
    function GetReason: OWideString;
    function GetSrcText: OWideString;
    function GetSrcTextPos: Integer;
    function GetLine: Integer;
    function GetLinePos: Integer;
    function GetFilePos: Integer;

    procedure DoCreate(const aReader: TXMLReader; const aReason: string);
  public
    constructor Create(const aReader: TXMLReader; const aReason: string);
    constructor CreateFmt(const aReader: TXMLReader; const aReason: string;
      const aArgs: array of const);
  public
    procedure RaiseException;
    procedure RaiseAndEatException;
  public
    property URL: OWideString read GetURL;
    property ErrorCode: Integer read GetErrorCode;
    property FilePos: Integer read GetFilePos;
    property Line: Integer read GetLine;
    property LinePos: Integer read GetLinePos;
    property Reason: OWideString read GetReason;
    property SrcText: OWideString read GetSrcText;
    property SrcTextPos: Integer read GetSrcTextPos;
    property LastTokenCharPos: Integer read fLastTokenCharPos;
    property LastTokenLinePos: Integer read fLastTokenLinePos;
  end;

  TXMLParseErrorInvalidCharacter = class(TXMLParseError)
    function GetExceptionClass: EXmlReaderExceptionClass; override;
  end;

  TXMLParseErrorInvalidStructure = class(TXMLParseError)
    function GetExceptionClass: EXmlReaderExceptionClass; override;
  end;

implementation

uses OXmlLng;

{ TXMLReader }

procedure TXMLReader.FinishOpenElement;
begin
  fReaderToken := fOpenElementTokens.GetToken(fOpenElementTokens.Count-1);//has TokenName already set
  fReaderToken.TokenValue := '';
  if fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute] then
    fReaderToken.TokenType := rtFinishXMLDeclarationClose
  else
    fReaderToken.TokenType := rtFinishOpenElement;
end;

procedure TXMLReader.FinishOpenElementClose;
var
  xC: OWideChar;
  x: PXMLReaderToken;
begin
  //opened after a '?' for PI or '/' for an element.

  fReader.ReadNextChar({%H-}xC);//must be '>'
  if xC <> '>' then begin
    if fReaderSettings.fStrictXML then begin
      if fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute] then
      begin
        RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
          OXmlLng_InvalidCharacterInElement, ['?']);
        Exit;
      end else begin
        RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
          OXmlLng_InvalidCharacterInElement, ['/']);
        Exit;
      end;
    end else begin
      //let's be generous and go over this invalid character
      fReader.UndoRead;
      ReadNextToken({%H-}x);
      Exit;
    end;
  end;

  fReaderToken := fOpenElementTokens.GetToken(fOpenElementTokens.Count-1);//has TokenName already set
  fReaderToken.TokenValue := '';
  if fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute] then begin
    fReaderToken.TokenType := rtFinishXMLDeclarationClose;
  end else begin
    fReaderToken.TokenType := rtFinishOpenElementClose;
  end;
  RemoveLastFromNodePath(False);
end;

procedure TXMLReader.Text(const aClearCustomBuffer: Boolean);
var
  xC: OWideChar;
begin
  if aClearCustomBuffer then
    fMainBuffer.Clear(False);

  fReader.ReadNextChar({%H-}xC);
  while not Assigned(fParseError) and not fReader.EOF do begin
    case OXmlCharKind(xC) of
      ckNewLine10, ckNewLine13: ProcessNewLineChar(xC);
      ckAmpersand: ProcessEntity;
      ckLowerThan: Break;
      ckCharacter, ckSingleQuote, ckDoubleQuote: fMainBuffer.WriteChar(xC);
    else
      if not fReaderSettings.fStrictXML then
        fMainBuffer.WriteChar(xC)
      else
        RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
          OXmlLng_InvalidCharacterInText, [IntToHex(Ord(xC), 4)]);
    end;
    fReader.ReadNextChar(xC);
  end;(**)

  if not fReader.EOF then
    fReader.UndoRead;
  fReaderToken.TokenType := rtText;
  fReaderToken.TokenName := '';
  fMainBuffer.GetBuffer(fReaderToken.TokenValue);
end;

procedure TXMLReader.Attribute;
var
  xC: OWideChar;
  xQuotationMark: OWideChar;
begin
  if Assigned(fAttributeTokens) then
  begin
    fReaderToken := fAttributeTokens.CreateNew;
    fAttributeTokens.AddLast;
  end;

  fMainBuffer.Clear(False);
  fReader.ReadNextChar({%H-}xC);

  if fReaderSettings.fStrictXML and not OXmlIsNameStartChar(xC) then
  begin
    RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
      OXmlLng_InvalidAttributeStartChar, [xC]);
    Exit;
  end;

  if not fReaderSettings.fStrictXML then begin
    //not StrictXML

    repeat//read attribute name
      fMainBuffer.WriteChar(xC);
      fReader.ReadNextChar(xC);
    until OXmlIsBreakChar(xC);
  end else begin
    //StrictXML
    while OXmlIsNameChar(xC) do begin//read attribute name
      fMainBuffer.WriteChar(xC);
      fReader.ReadNextChar(xC);
    end;
  end;
  fMainBuffer.GetBuffer(fReaderToken.TokenName);

  while OXmlIsWhiteSpaceChar(xC) do//jump over spaces "attr ="
    fReader.ReadNextChar(xC);

  if xC <> '=' then begin
    //let's be generous and allow attributes without values - even if they are not allowed by xml spec
    if fReaderSettings.fStrictXML then begin
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_EqualSignMustFollowAttribute, [fReaderToken.TokenName]);
      Exit;
    end else begin
      fReaderToken.TokenValue := '';
      fReader.UndoRead;
    end;
  end else begin
    fReader.ReadNextChar(xC);
    while OXmlIsWhiteSpaceChar(xC) do//jump over spaces "= value"
      fReader.ReadNextChar(xC);

    xQuotationMark := xC;
    if (xQuotationMark = '''') or (xQuotationMark = '"') then begin
      //read attribute value in quotation marks
      fMainBuffer.Clear(False);
      fReader.ReadNextChar(xC);
      while not (xC = xQuotationMark) and not Assigned(fParseError) and not fReader.EOF do
      begin
        case OXmlCharKind(xC) of
          ckNewLine10, ckNewLine13: ProcessNewLineChar(xC);
          ckAmpersand: ProcessEntity;
          ckCharacter, ckSingleQuote, ckDoubleQuote: fMainBuffer.WriteChar(xC);
        else
          if not fReaderSettings.fStrictXML then
            fMainBuffer.WriteChar(xC)
          else
            RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
              OXmlLng_InvalidCharacterInAttribute, [IntToHex(Ord(xC), 4)]);
        end;
        fReader.ReadNextChar(xC);
      end;
    end else begin
      if fReaderSettings.fStrictXML then begin
        RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
          OXmlLng_AttributeValueMustBeEnclosed, [fReaderToken.TokenName]);
        Exit;
      end else begin
        //let's be generous and allow attribute values that are not enclosed in quotes
        fMainBuffer.Clear(False);
        while not OXmlIsBreakChar(xC) and not Assigned(fParseError) and not fReader.EOF do
        begin
          case OXmlCharKind(xC) of
            ckNewLine10, ckNewLine13: ProcessNewLineChar(xC);
            ckAmpersand: ProcessEntity;
            ckCharacter, ckSingleQuote, ckDoubleQuote: fMainBuffer.WriteChar(xC);
          else
            if not fReaderSettings.fStrictXML then
              fMainBuffer.WriteChar(xC)
            else
              RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
                OXmlLng_InvalidCharacterInText, [IntToHex(Ord(xC), 4)]);
          end;
          fReader.ReadNextChar(xC);
        end;
      end;
    end;

    fMainBuffer.GetBuffer(fReaderToken.TokenValue);
  end;

  if fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute] then begin
    fReaderToken.TokenType := rtXMLDeclarationAttribute;

    if not fForceEncoding and fAllowSetEncodingFromFile and
      (fReaderToken.TokenName = 'encoding')
    then
      ChangeEncoding(fReaderToken.TokenValue);
  end else begin
    fReaderToken.TokenType := rtAttribute;
  end;
end;

procedure TXMLReader.CData;
begin
  ExclamationNode(rtCData, '<![CDATA[', ']]>', False);
end;

procedure TXMLReader.ChangeEncoding(const aEncodingAlias: OWideString);
var
  xLastName: OWideString;
  xEncoding: TEncoding;
  xInXMLDeclaration: Boolean;
  xReaderToken: PXMLReaderToken;
begin
  if
    TEncoding.EncodingFromAlias(aEncodingAlias, {%H-}xEncoding) and
    (fReader.Encoding <> xEncoding)
  then begin
    //reload document with new encoding
    fReader.Encoding := xEncoding;
    if fAllowSetEncodingFromFile then begin
      fAllowSetEncodingFromFile := False;
      fReader.UnblockFlushTempBuffer;//was blocked in TXMLReader.Create
    end;
    //go back to current position
    xInXMLDeclaration := False;
    xLastName := fReaderToken.TokenName;
    fLastTokenType := rtDocumentStart;
    fOpenElementTokens.Clear;
    //parse from beginning back to the encoding attribute
    while ReadNextToken({%H-}xReaderToken) do begin
      case xReaderToken.TokenType of
        rtOpenXMLDeclaration: xInXMLDeclaration := True;
        rtOpenElement: xInXMLDeclaration := False;
        rtXMLDeclarationAttribute:
        if xInXMLDeclaration and (xReaderToken.TokenName = xLastName) then begin
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TXMLReader.Comment;
begin
  ExclamationNode(rtComment, '<!--', '-->', False);
end;

constructor TXMLReader.Create;
begin
  inherited Create;

  DoCreate;
end;

constructor TXMLReader.Create(const aStream: TStream;
  const aForceEncoding: TEncoding = nil);
begin
  inherited Create;

  DoCreate;

  InitStream(aStream, aForceEncoding);
end;

procedure TXMLReader.DoInit(const aForceEncoding: TEncoding);
begin
  fAllowSetEncodingFromFile := not fReader.BOMFound;
  fDocumentElementFound := False;
  fElementsToClose := 0;
  fLastTokenType := rtDocumentStart;
  fOpenElementTokens.Clear;
  fReaderToken := fOpenElementTokens.CreateNew;
  if Assigned(fAttributeTokens) then
    fAttributeTokens.Clear;

  fReader.BlockFlushTempBuffer;//will be unblocked when fAllowSetEncodingFromFile is set to false
  if Assigned(aForceEncoding) then
    Self.Encoding := aForceEncoding
  else
    Self.fForceEncoding := False;
end;

procedure TXMLReader.DoRaiseException;
begin
  case fReaderSettings.ErrorHandling of
    ehRaiseAndEat: fParseError.RaiseAndEatException;
    ehRaise: fParseError.RaiseException;
  end;
end;

destructor TXMLReader.Destroy;
begin
  DoDestroy;

  inherited;
end;

procedure TXMLReader.DoCreate;
begin
  fReader := TOTextReader.Create;
  fOwnsReaderToken := True;
  fReaderSettings := TXMLReaderSettings.Create;

  fOpenElementTokens := TXMLReaderTokenList.Create;
  fReaderToken := fOpenElementTokens.CreateNew;

  fMainBuffer := TOTextBuffer.Create;
  fEntityBuffer := TOTextBuffer.Create(16);
end;

procedure TXMLReader.DocType;
begin
  ExclamationNode(rtDocType, '<!DOCTYPE', '>', True);
end;

procedure TXMLReader.DoDestroy;
begin
  fReader.Free;
  fReaderSettings.Free;
  fOpenElementTokens.Free;

  fMainBuffer.Free;
  fEntityBuffer.Free;
  //do not destroy fAttributeTokens here!!!
end;

procedure TXMLReader.SetAttributeTokens(
  const aAttributeTokens: TXMLReaderTokenList);
begin
  fAttributeTokens := aAttributeTokens;
  fReaderToken := fOpenElementTokens.CreateNew;//in case fReaderToken was in old fAttributeTokens
end;

procedure TXMLReader.SetEncoding(const aEncoding: TEncoding);
begin
  fReader.Encoding := aEncoding;
  fForceEncoding := True;
end;

procedure TXMLReader.SetOwnsEncoding(const aSetOwnsEncoding: Boolean);
begin
  fReader.OwnsEncoding := aSetOwnsEncoding;
end;

procedure TXMLReader.ExclamationNode(
  const aTokenType: TXMLReaderTokenType; const aBeginTag, aEndTag: OWideString;
  const aWhiteSpaceAfterBeginTag: Boolean);
var
  I: Integer;
  xC: OWideChar;
  xPreviousC: OWideString;
  xResult: Boolean;
begin
  fMainBuffer.Clear(False);
  fMainBuffer.WriteChar('<');
  fMainBuffer.WriteChar('!');
  xResult := True;
  for I := 3 to Length(aBeginTag) do begin
    fReader.ReadNextChar({%H-}xC);
    if aBeginTag[I] <> UpperCase(xC) then begin
      xResult := False;
      fReader.UndoRead;
      Break;
    end;
    fMainBuffer.WriteChar(xC);
  end;

  if aWhiteSpaceAfterBeginTag and xResult then begin
    //must be followed by a whitespace character
    fReader.ReadNextChar(xC);
    fMainBuffer.WriteChar(xC);
    xResult := OXmlIsWhiteSpaceChar(xC);
  end;

  if not xResult then begin
    //header not found
    if fReaderSettings.fStrictXML then begin
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidCharacterInText, ['<']);
      Exit;
    end else begin
      //output as text
      if xC <> '<' then begin
        Text(False);
      end else begin
        fReaderToken.TokenType := rtText;
        fMainBuffer.GetBuffer(fReaderToken.TokenValue);
        fReaderToken.TokenName := '';
      end;
      Exit;
    end;
  end else begin
    fMainBuffer.Clear(False);
    SetLength(xPreviousC, Length(aEndTag));
    for I := 1 to Length(xPreviousC) do
      xPreviousC[I] := #0;

    repeat
      if Length(xPreviousC) > 1 then
        Move(xPreviousC[2], xPreviousC[1], (Length(xPreviousC)-1)*SizeOf(OWideChar));
      fReader.ReadNextChar(xC);

      if not fReaderSettings.fStrictXML or OXmlIsChar(xC) then
      begin
        xPreviousC[Length(xPreviousC)] := xC;
        fMainBuffer.WriteChar(xC);
      end else
      begin
        RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
          OXmlLng_InvalidCharacterInText, [IntToHex(Ord(xC), 4)]);
      end;
    until ((xPreviousC = aEndTag) or Assigned(fParseError) or fReader.EOF);

    for I := 1 to Length(aEndTag) do
      fMainBuffer.RemoveLastChar;

    fReaderToken.TokenType := aTokenType;
    fReaderToken.TokenName := '';
    fMainBuffer.GetBuffer(fReaderToken.TokenValue);
  end;
end;

procedure TXMLReader.OpenElement;
var
  xC: OWideChar;
begin
  fMainBuffer.Clear(False);
  fReader.ReadNextChar({%H-}xC);

  case xC of
    '!': begin
      //comment or cdata
      fReader.ReadNextChar(xC);
      fReader.UndoRead;
      case xC of
        '[': CData;
        '-': Comment;
        'D', 'd': DocType;
      else
        if fReaderSettings.fStrictXML then begin
          RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
            OXmlLng_InvalidCharacterInText, ['<']);
          Exit;
        end else begin
          fMainBuffer.WriteChar('<');
          fMainBuffer.WriteChar('!');
          if xC <> '<' then begin
            Text(False);
          end else begin
            fReaderToken.TokenType := rtText;
            fMainBuffer.GetBuffer(fReaderToken.TokenValue);
            fReaderToken.TokenName := '';
          end;
          Exit;
        end;
      end;
      Exit;
    end;
    '/': begin
      //close element
      CloseElement;
      Exit;
    end;
    '?': begin
      //processing instruction
      ProcessingInstruction;
      Exit;
    end;
  end;

  if fReaderSettings.fStrictXML then begin
    if not OXmlIsNameStartChar(xC) then
    begin
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidCharacterInText, ['<']);
      Exit;
    end;

    while OXmlIsNameChar(xC) do begin
      fMainBuffer.WriteChar(xC);
      fReader.ReadNextChar(xC);
    end;

    if (xC = '/') or (xC = '>') then
      fReader.UndoRead
    else if not OXmlIsWhiteSpaceChar(xC) then
    begin
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidElementName, [fMainBuffer.GetBuffer+xC]);
      Exit;
    end;
  end else begin
    if not OXmlIsNameChar(xC) then begin
      fMainBuffer.WriteChar('<');
      fReader.UndoRead;
      if xC <> '<' then begin
        Text(False);
      end else begin
        fReaderToken.TokenType := rtText;
        fMainBuffer.GetBuffer(fReaderToken.TokenValue);
        fReaderToken.TokenName := '';
      end;
      Exit;
    end else begin
      while not OXmlIsBreakChar(xC) do begin
        fMainBuffer.WriteChar(xC);
        fReader.ReadNextChar(xC);
      end;

      if (xC = '/') or (xC = '>') then
        fReader.UndoRead
    end;
  end;

  if fAllowSetEncodingFromFile then begin
    fAllowSetEncodingFromFile := False;// -> first Node found, encoding change is not allowed any more
    fReader.UnblockFlushTempBuffer;//was blocked in TXMLReader.Create
  end;

  fDocumentElementFound := True;
  fMainBuffer.GetBuffer(fReaderToken.TokenName);
  fReaderToken.TokenValue := '';
  fReaderToken.TokenType := rtOpenElement;
  fOpenElementTokens.AddLast;
  if Assigned(fAttributeTokens) then
    fAttributeTokens.Clear;
end;

procedure TXMLReader.RaiseException(const aErrorClass: TXMLParseErrorClass;
  const aReason: String);
begin
  fParseError := aErrorClass.Create(Self, aReason);
  DoRaiseException;
end;

procedure TXMLReader.RaiseExceptionFmt(const aErrorClass: TXMLParseErrorClass;
  const aReason: String; const aArgs: array of const);
begin
  fParseError := aErrorClass.CreateFmt(Self, aReason, aArgs);
  DoRaiseException;
end;

function TXMLReader.ReadNextToken(var outToken: PXMLReaderToken): Boolean;
var
  xC: OWideChar;
begin
  Result := True;

  fLastTokenLinePosition := fReader.Line;
  fLastTokenCharPosition := fReader.LinePosition;

  fParseError := nil;

  if fElementsToClose > 0 then begin
    //close elements
    fReaderToken := fOpenElementTokens.GetToken(fOpenElementTokens.Count-1);//has TokenName already set
    fReaderToken.TokenValue := '';
    fReaderToken.TokenType := rtCloseElement;
    RemoveLastFromNodePath(False);
    fLastTokenType := fReaderToken.TokenType;
    outToken := fReaderToken;
    Dec(fElementsToClose);
    Exit;
  end;

  if (fReaderSettings.fBreakReading = brAfterDocumentElement) and
     (fDocumentElementFound) and
     NodePathIsEmpty
  then begin
    //end of root element is reached, but do not release document!!!
    Result := False;
    outToken := fReaderToken;
    Exit;
  end;

  fReaderToken := fOpenElementTokens.CreateNew;//must be here and not in the beggining of the function -> due to attributes and open elements and sequential parser

  if not fReader.ReadNextChar({%H-}xC) then
  begin
    //end of document
    Result := False;
    ReleaseDocument;
    if fReaderSettings.fStrictXML and (NodePathCount > 0) then
      RaiseExceptionFmt(TXMLParseErrorInvalidStructure,
        OXmlLng_UnclosedElementsInTheEnd, [xC]);
    Exit;
  end;

  case xC of
    '<': begin
      if fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute, rtOpenElement, rtAttribute] then
      begin
        if fReaderSettings.fStrictXML then begin
          Result := False;
          RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
            OXmlLng_InvalidCharacterInElement, [xC]);
          Exit;
        end else begin
          fReader.UndoRead;
          Attribute;
        end;
      end else begin
        OpenElement;
      end;
    end;
    '?': begin
      if fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute] then
      begin
        FinishOpenElementClose
      end
      else
      begin
        //text
        fReader.UndoRead;
        Text;
      end;
    end;
    '/': begin
      if fLastTokenType in [rtOpenElement, rtAttribute] then
      begin
        FinishOpenElementClose
      end
      else
      begin
        //text
        fReader.UndoRead;
        Text;
      end;
    end;
    '>': begin
      if fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute, rtOpenElement, rtAttribute] then
      begin
        FinishOpenElement;
      end else begin
        if fReaderSettings.fStrictXML then begin
          Result := False;
          RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
            OXmlLng_InvalidCharacterInText, [xC]);
          Exit;
        end else begin
          //text
          fReader.UndoRead;
          Text;
        end;
      end;
    end;
  else//case
    if fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute, rtOpenElement, rtAttribute]
    then begin
      while OXmlIsWhiteSpaceChar(xC) do
        fReader.ReadNextChar(xC);

      if ((xC = '/') and (fLastTokenType in [rtOpenElement, rtAttribute])) or
         ((xC = '?') and (fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute]))
      then begin
        FinishOpenElementClose;
      end else if ((xC = '>') and (fLastTokenType in [rtOpenElement, rtAttribute])) then begin
        FinishOpenElement;
      end else begin
        fReader.UndoRead;
        Attribute;
      end;
    end else begin
      //text
      fReader.UndoRead;
      Text;
    end;
  end;

  outToken := fReaderToken;
  fLastTokenType := fReaderToken.TokenType;

  Result := not Assigned(fParseError);
end;

procedure TXMLReader.CloseElement;
var
  xC: OWideChar;
begin
  fMainBuffer.Clear(False);
  fReader.ReadNextChar({%H-}xC);

  if fReaderSettings.fStrictXML then begin
    //strict
    if not OXmlIsNameStartChar(xC) then
    begin
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidStringInText, ['</'+xC]);
      Exit;
    end;

    while OXmlIsNameChar(xC) do begin
      fMainBuffer.WriteChar(xC);
      fReader.ReadNextChar(xC);
    end;
    while OXmlIsWhiteSpaceChar(xC) do begin
      fReader.ReadNextChar(xC);
    end;
    if xC <> '>' then
    begin
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidStringInText, ['</'+fMainBuffer.GetBuffer]);
      Exit;
    end;
  end else begin
    //not strict
    if not OXmlIsNameChar(xC) then begin
      fMainBuffer.WriteChar('<');
      fMainBuffer.WriteChar('/');
      fReader.UndoRead;
      if xC <> '<' then begin
        Text(False);
      end else begin
        fReaderToken.TokenType := rtText;
        fMainBuffer.GetBuffer(fReaderToken.TokenValue);
        fReaderToken.TokenName := '';
      end;
      Exit;
    end else begin
      while not OXmlIsBreakChar(xC) do begin
        fMainBuffer.WriteChar(xC);
        fReader.ReadNextChar(xC);
      end;
      while not((xC = '>') or fReader.EOF) do begin
        fReader.ReadNextChar(xC);
      end;
    end;
  end;

  fMainBuffer.GetBuffer(fReaderToken.TokenName);
  fReaderToken.TokenValue := '';
  fReaderToken.TokenType := rtCloseElement;

  RemoveLastFromNodePath(True);
end;

function TXMLReader.GetApproxStreamPosition: ONativeInt;
begin
  Result := fReader.ApproxStreamPosition;
end;

function TXMLReader.GetEncoding: TEncoding;
begin
  Result := fReader.Encoding;
end;

function TXMLReader.GetOwnsEncoding: Boolean;
begin
  Result := fReader.OwnsEncoding;
end;

function TXMLReader.GetStreamSize: ONativeInt;
begin
  Result := fReader.StreamSize;
end;

function TXMLReader.GetLinePosition: Integer;
begin
  Result := fReader.LinePosition;
end;

function TXMLReader.GetLine: Integer;
begin
  Result := fReader.Line;
end;

function TXMLReader.GetFilePosition: Integer;
begin
  Result := fReader.FilePosition;
end;

{$IFDEF O_GENERICBYTES}
procedure TXMLReader.InitBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding);
begin
  fReader.InitBuffer(aBuffer);
  DoInit(aForceEncoding);
end;
{$ENDIF}

procedure TXMLReader.InitFile(const aFileName: String;
  const aForceEncoding: TEncoding);
begin
  fReader.InitFile(aFileName);
  DoInit(aForceEncoding);
end;

procedure TXMLReader.InitStream(const aStream: TStream;
  const aForceEncoding: TEncoding);
begin
  fReader.InitStream(aStream);
  DoInit(aForceEncoding);
end;

procedure TXMLReader.InitXML(const aXML: OWideString);
begin
  fReader.InitString(aXML);
  DoInit(TEncoding.OWideStringEncoding);
end;

function TXMLReader.NodePathIsEmpty: Boolean;
begin
  Result := fOpenElementTokens.Count = 0;
end;

{$IFDEF O_RAWBYTESTRING}
procedure TXMLReader.InitXML_UTF8(const aXML: ORawByteString);
begin
  fReader.InitString_UTF8(aXML);
  DoInit(TEncoding.UTF8);
end;
{$ENDIF}

procedure TXMLReader.ProcessEntity;
const
  cEntityPrefix: Array[0..2] of OWideString = ('&', '&#', '&#x');
  cEntityText = 0;
  cEntityDec = 1;
  cEntityHex = 2;
var
  xEntityType: Byte;

  procedure _EntityError(bC: OWideChar; bCustomEntityString: OWideString = '');
  begin
    if fReaderSettings.fStrictXML then begin
      if bCustomEntityString = '' then begin
        fEntityBuffer.WriteChar(bC);
        fEntityBuffer.GetBuffer(bCustomEntityString);
      end;
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidEntity, [cEntityPrefix[xEntityType]+bCustomEntityString]);
      Exit;
    end else begin
      if bCustomEntityString = '' then
        fEntityBuffer.GetBuffer(bCustomEntityString);

      fReader.UndoRead;
      fMainBuffer.WriteString(cEntityPrefix[xEntityType]+bCustomEntityString);
    end;
  end;
var
  xC: OWideChar;
  xEntityString: OWideString;
  xOutputChar: Integer;
  xIsHex: Boolean;
  xOutputEntity: OWideString;
begin
  xOutputEntity := '';
  xOutputChar := 0;
  fEntityBuffer.Clear(False);

  fReader.ReadNextChar({%H-}xC);
  if xC = '#' then begin
    fReader.ReadNextChar(xC);
    xIsHex := (xC = 'x');
    if xIsHex then begin
      xEntityType := cEntityHex;

      fReader.ReadNextChar(xC);
      while OXmlIsHexadecimalChar(xC) do begin
        fEntityBuffer.WriteChar(xC);
        fReader.ReadNextChar(xC);
      end;

      if xC <> ';' then begin
        _EntityError(xC);
        Exit;
      end;

      fEntityBuffer.GetBuffer({%H-}xEntityString);
      if not TryStrToInt('$'+xEntityString, xOutputChar)
      then begin
        _EntityError(xC, xEntityString+';');
        Exit;
      end;
    end else begin
      xEntityType := cEntityDec;

      while OXmlIsDecimalChar(xC) do begin
        fEntityBuffer.WriteChar(xC);
        fReader.ReadNextChar(xC);
      end;

      if xC <> ';' then begin
        _EntityError(xC);
        Exit;
      end;

      fEntityBuffer.GetBuffer(xEntityString);
      if not TryStrToInt(xEntityString, xOutputChar) then begin
        _EntityError(xC, xEntityString+';');
        Exit;
      end;
    end;

  end else begin
    xEntityType := cEntityText;

    if not OXmlIsNameStartChar(xC) then begin
      _EntityError(xC);
      Exit;
    end;

    while OXmlIsNameChar(xC) do begin
      fEntityBuffer.WriteChar(xC);
      fReader.ReadNextChar(xC);
    end;

    if xC <> ';' then begin
      _EntityError(xC);
      Exit;
    end;

    fEntityBuffer.GetBuffer(xEntityString);
    if not fReaderSettings.fEntityList.TryGetValue(xEntityString, xOutputEntity) then begin
      _EntityError(xC, xEntityString+';');
      Exit;
    end;
  end;

  if (xOutputChar > 0) and (xOutputEntity = '') then begin
    if not OXmlIsChar(xOutputChar) then begin
      _EntityError(xC, xEntityString+';');
      Exit;
    end else begin
      {$IFDEF FPC}
      xOutputEntity := UTF8Encode(WideString(WideChar(xOutputChar)));//MUST BE WideString + WideChar (convert utf16 to utf8)
      {$ELSE}
      xOutputEntity := OWideString(OWideChar(xOutputChar));
      {$ENDIF}
    end;
  end;

  if xOutputEntity <> '' then
    fMainBuffer.WriteString(xOutputEntity);
end;

procedure TXMLReader.ProcessingInstruction;
var
  xC: OWideChar;
  xPreviousC: OWideChar;
begin
  fMainBuffer.Clear(False);
  fReader.ReadNextChar({%H-}xC);

  if fReaderSettings.fStrictXML then begin
    if not OXmlIsNameStartChar(xC) then
    begin
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidStringInText, ['<?'+xC]);
      Exit;
    end;

    while OXmlIsNameChar(xC) do begin
      fMainBuffer.WriteChar(xC);
      fReader.ReadNextChar(xC);
    end;

    if not OXmlIsWhiteSpaceChar(xC) then begin
      //must be followed by a whitespace character
      fMainBuffer.WriteChar(xC);
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidStringInText, ['<?'+fMainBuffer.GetBuffer]);
      Exit;
    end;
  end else begin
    while not OXmlIsBreakChar(xC) do begin
      fMainBuffer.WriteChar(xC);
      fReader.ReadNextChar(xC);
    end;
  end;

  fMainBuffer.GetBuffer(fReaderToken.TokenName);
  if
    not fDocumentElementFound and
    fReaderSettings.fRecognizeXMLDeclaration and
    SameText(fReaderToken.TokenName, 'xml')
  then begin
    //xml declaration: <?xml attr="value"?>
    fReaderToken.TokenType := rtOpenXMLDeclaration;
    fReaderToken.TokenValue := '';
    fOpenElementTokens.AddLast;
    Exit;
  end;

  //custom processing instruction
  fReaderToken.TokenType := rtProcessingInstruction;
  fMainBuffer.Clear(False);
  if not fReaderSettings.fStrictXML and (fReaderToken.TokenName = '') then
    fReader.UndoRead;

  xPreviousC := #0;
  fReader.ReadNextChar(xC);
  while
    not((xPreviousC = '?') and (xC = '>')) and
    not fReader.EOF
  do begin
    fMainBuffer.WriteChar(xC);
    xPreviousC := xC;
    fReader.ReadNextChar(xC);
  end;
  fMainBuffer.RemoveLastChar;

  fMainBuffer.GetBuffer(fReaderToken.TokenValue);
end;

procedure TXMLReader.ProcessNewLineChar(const aLastChar: OWideChar);
var
  xC: OWideChar;
begin
  if fReaderSettings.fLineBreak <> lbDoNotProcess then begin
    if aLastChar = #13 then begin
      fReader.ReadNextChar({%H-}xC);
      if xC <> #10 then
        fReader.UndoRead;
    end;

    fMainBuffer.WriteString(XmlLineBreak[fReaderSettings.fLineBreak]);
  end else begin
    fMainBuffer.WriteChar(aLastChar);
  end;
end;

procedure TXMLReader.ReleaseDocument;
begin
  fReader.ReleaseDocument;
end;

procedure TXMLReader.RemoveLastFromNodePath(const aCheckPath: Boolean);
var
  I: Integer;
begin
  if (fOpenElementTokens.Count = 0) then begin
    //there is no open element
    if fReaderSettings.fStrictXML then
    begin
      RaiseException(TXMLParseErrorInvalidStructure,
        OXmlLng_TooManyElementsClosed);
      Exit;
    end;
  end else begin
    if aCheckPath and
       (fReaderToken.TokenName <> '') and
       (fOpenElementTokens.GetToken(fOpenElementTokens.Count-1).TokenName <> fReaderToken.TokenName)
    then begin
      //element names do not match
      if fReaderSettings.fStrictXML then
      begin
        RaiseExceptionFmt(TXMLParseErrorInvalidStructure,
          OXmlLng_WrongElementClosed, [fReaderToken.TokenName,
            fOpenElementTokens.GetToken(fOpenElementTokens.Count-1).TokenName]);
        Exit;
      end else begin
        //trying to close parent element
        for I := fOpenElementTokens.Count-2 downto 0 do
        if (fOpenElementTokens.GetToken(I).TokenName = fReaderToken.TokenName) then begin
          //parent element with the same name found, we have to close more elements in the future!!!
          fElementsToClose := (fOpenElementTokens.Count - I - 1);
          Break;
        end;

        //delete the last one from fNodePath
        //  + rename the element if names differ
        if fReaderToken.TokenName <> '' then
          fReaderToken.TokenName := fOpenElementTokens.GetToken(fOpenElementTokens.Count-1).TokenName;
        fOpenElementTokens.DeleteLast;
      end;
    end else begin
      //everything is fine -> delete last from fNodePath
      fOpenElementTokens.DeleteLast;
    end;
  end;
end;

function TXMLReader.GetNodePath(
  const aIndex: Integer): OWideString;
begin
  Result := fOpenElementTokens.GetToken(aIndex).TokenName;
end;

function TXMLReader.GetNodePathCount: Integer;
begin
  Result := fOpenElementTokens.Count;
end;

procedure TXMLReader.NodePathAssignTo(
  const aNodePath: TOWideStringList);
var
  I: Integer;
begin
  aNodePath.Clear;
  for I := 0 to fOpenElementTokens.Count-1 do
    aNodePath.Add(fOpenElementTokens.GetToken(I).TokenName);
end;

function TXMLReader.NodePathAsString: OWideString;
var
  I: Integer;
begin
  if fOpenElementTokens.Count = 0 then
  begin
    Result := '';
    Exit;
  end;

  Result := fOpenElementTokens.GetToken(0).TokenName;
  for I := 1 to fOpenElementTokens.Count-1 do
    Result := Result + '/' + fOpenElementTokens.GetToken(0).TokenName;
end;

function TXMLReader.NodePathMatch(
  const aNodePath: array of OWideString): Boolean;
var
  I: Integer;
begin
  Result := Length(aNodePath) = fOpenElementTokens.Count;
  if not Result then
    Exit;

  for I := 0 to Length(aNodePath)-1 do
  if aNodePath[I] <> fOpenElementTokens.GetToken(I).TokenName then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TXMLReader.NodePathMatch(
  const aNodePath: TOWideStringList): Boolean;
var
  I: Integer;
begin
  Result := aNodePath.Count = fOpenElementTokens.Count;
  if not Result then
    Exit;

  for I := 0 to aNodePath.Count-1 do
  if aNodePath[I] <> fOpenElementTokens.GetToken(I).TokenName then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TXMLReader.NodePathMatch(
  const aNodePath: OWideString): Boolean;
var
  xNodePath: TOWideStringList;
begin
  xNodePath := TOWideStringList.Create;
  try
    OExplode(aNodePath, '/', xNodePath);

    Result := NodePathMatch(xNodePath);
  finally
    xNodePath.Free;
  end;
end;

function TXMLReader.RefIsChildOfNodePath(
  const aRefNodePath: array of OWideString): Boolean;
var
  I: Integer;
begin
  Result := Length(aRefNodePath) = fOpenElementTokens.Count-1;
  if not Result then
    Exit;

  for I := 0 to Length(aRefNodePath)-1 do
  if aRefNodePath[I] <> fOpenElementTokens.GetToken(I).TokenName then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TXMLReader.RefIsChildOfNodePath(
  const aRefNodePath: TOWideStringList): Boolean;
var
  I: Integer;
begin
  Result := aRefNodePath.Count = fOpenElementTokens.Count-1;
  if not Result then
    Exit;

  for I := 0 to aRefNodePath.Count-1 do
  if aRefNodePath[I] <> fOpenElementTokens.GetToken(I).TokenName then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TXMLReader.RefIsParentOfNodePath(
  const aRefNodePath: array of OWideString): Boolean;
var
  I: Integer;
begin
  Result := Length(aRefNodePath) = fOpenElementTokens.Count+1;
  if not Result then
    Exit;

  for I := 0 to fOpenElementTokens.Count-1 do
  if aRefNodePath[I] <> fOpenElementTokens.GetToken(I).TokenName then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TXMLReader.RefIsParentOfNodePath(
  const aRefNodePath: TOWideStringList): Boolean;
var
  I: Integer;
begin
  Result := aRefNodePath.Count = fOpenElementTokens.Count+1;
  if not Result then
    Exit;

  for I := 0 to fOpenElementTokens.Count-1 do
  if aRefNodePath[I] <> fOpenElementTokens.GetToken(I).TokenName then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

{ TXMLWriter }

procedure TXMLWriter.CData(const aText: OWideString);
begin
  if fWriterSettings.fStrictXML and not OXmlValidCData(aText) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidCData, [aText]);

  Indent;

  RawText('<![CDATA[');
  RawText(aText);//MUST BE RAWTEXT - must contain unescaped characters
  RawChar(']');
  RawChar(']');
  RawChar('>');
end;

procedure TXMLWriter.Comment(const aText: OWideString);
begin
  if fWriterSettings.fStrictXML and not OXmlValidComment(aText) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidComment, [aText]);

  Indent;

  RawChar('<');
  RawChar('!');
  RawChar('-');
  RawChar('-');
  RawText(aText);//MUST BE RAWTEXT - must contain unescaped characters
  RawChar('-');
  RawChar('-');
  RawChar('>');
end;

constructor TXMLWriter.Create(const aStream: TStream);
begin
  inherited Create;

  DoCreate;

  InitStream(aStream);
end;

constructor TXMLWriter.Create;
begin
  inherited Create;

  DoCreate;
end;

procedure TXMLWriter.DecIndentLevel;
begin
  Dec(fIndentLevel);
end;

destructor TXMLWriter.Destroy;
begin
  fWriter.Free;
  fWriterSettings.Free;

  inherited;
end;

procedure TXMLWriter.DoInit;
begin
  fWritten := False;
  fIndentLevel := fDefaultIndentLevel;
end;

procedure TXMLWriter.DoCreate;
begin
  fWriter := TOTextWriter.Create;

  Encoding := TEncoding.UTF8;
  fWriterSettings := TXMLWriterSettings.Create;
  fWriterSettings.WriteBOM := True;
  fWriterSettings.fOnSetWriteBOM := OnSetWriteBOM;
end;

procedure TXMLWriter.DocType(const aDocTypeRawText: OWideString);
begin
  Indent;

  RawText('<!DOCTYPE ');
  RawText(aDocTypeRawText);//MUST BE RAW ESCAPED TEXT - the programmer has to be sure that aDocTypeRawText is valid
  RawChar('>');
end;

procedure TXMLWriter.CloseElement(const aElementName: OWideString; const aIndent: Boolean);
begin
  DecIndentLevel;
  if aIndent then
    Indent;
  RawChar('<');
  RawChar('/');
  RawText(aElementName);//can be rawtext, because validated (in OpenElement)!
  RawChar('>');
end;

function TXMLWriter.GetEncoding: TEncoding;
begin
  Result := fWriter.Encoding;
end;

function TXMLWriter.GetOwnsEncoding: Boolean;
begin
  Result := fWriter.OwnsEncoding;
end;

procedure TXMLWriter.IncIndentLevel;
begin
  Inc(fIndentLevel);
end;

procedure TXMLWriter.Indent;
var I: Integer;
begin
  if (fWriterSettings.fIndentType in [itFlat, itIndent]) and
    fWritten//do not indent at the very beginning of the document
  then
    RawText(XmlLineBreak[fWriterSettings.fLineBreak]);

  if fWriterSettings.fIndentType = itIndent then
  for I := 1 to fIndentLevel do
    RawText(fWriterSettings.fIndentString);
end;

procedure TXMLWriter.InitFile(const aFileName: String);
begin
  fWriter.InitFile(aFileName);

  DoInit;
end;

procedure TXMLWriter.InitStream(const aStream: TStream);
begin
  fWriter.InitStream(aStream);

  DoInit;
end;

procedure TXMLWriter.ProcessingInstruction(const aTarget,
  aContent: OWideString);
begin
  if fWriterSettings.fStrictXML and not OXmlValidName(aTarget) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidPITarget, [aTarget]);

  if fWriterSettings.fStrictXML and not OXmlValidPIContent(aContent) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidPIContent, [aContent]);

  Indent;

  RawChar('<');
  RawChar('?');
  RawText(aTarget);
  if aTarget <> '' then
    RawChar(' ');
  RawText(aContent);//MUST BE RAWTEXT - must contain unescaped characters
  RawChar('?');
  RawChar('>');
end;

procedure TXMLWriter.RawChar(const aChar: OWideChar);
begin
  fWritten := True;

  fWriter.WriteChar(aChar);
end;

procedure TXMLWriter.RawText(const aText: OWideString);
begin
  fWritten := True;

  fWriter.WriteString(aText);
end;

procedure TXMLWriter.ReleaseDocument;
begin
  fWriter.ReleaseDocument;
end;

procedure TXMLWriter.Attribute(const aAttrName, aAttrValue: OWideString);
begin
  if fWriterSettings.fStrictXML and not OXmlValidName(aAttrName) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidAttributeName, [aAttrName]);

  RawChar(' ');
  RawText(aAttrName);//can be rawtext, because validated!
  RawChar('=');
  RawChar('"');
  Text(aAttrValue, OWideChar('"'));
  RawChar('"');
end;

procedure TXMLWriter.SetEncoding(const aEncoding: TEncoding);
begin
  fWriter.Encoding := aEncoding;
end;

procedure TXMLWriter.SetDefaultIndentLevel(const aDefaultIndentLevel: Integer);
begin
  if fWritten then
    raise EXmlWriterException.Create(OXmlLng_CannotSetIndentLevelAfterWrite);
  fDefaultIndentLevel := aDefaultIndentLevel;
  fIndentLevel := fDefaultIndentLevel;
end;

procedure TXMLWriter.SetOwnsEncoding(const aOwnsEncoding: Boolean);
begin
  fWriter.OwnsEncoding := aOwnsEncoding;
end;

procedure TXMLWriter.OnSetWriteBOM(Sender: TObject);
begin
  if Assigned(fWriter) then
    fWriter.WriteBOM := fWriterSettings.fWriteBOM;
end;

procedure TXMLWriter.Text(const aText: OWideString; const aIndent: Boolean);
begin
  if aIndent then
    Indent;

  Text(aText, OWideChar(#0));
end;

procedure TXMLWriter.OpenXMLDeclaration;
begin
  Indent;
  RawText('<?xml');
end;

procedure TXMLWriter.FinishOpenXMLDeclaration;
begin
  RawText('?>');
end;

procedure TXMLWriter.OpenElement(const aElementName: OWideString; const aMode: TXMLWriterElementMode);
begin
  if fWriterSettings.fStrictXML and not OXmlValidName(aElementName) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidElementName, [aElementName]);

  Indent;

  RawChar('<');
  RawText(aElementName);
  case aMode of
    stOpenOnly: IncIndentLevel;
    stFinish: begin
      RawChar('>');
      IncIndentLevel;
    end;
    stFinishClose: begin
      RawChar('/');
      RawChar('>');
    end;
  end;
end;

procedure TXMLWriter.OpenElementR(const aElementName: OWideString;
  var outElement: TXMLWriterElement; const aMode: TXMLWriterElementMode);
begin
  OpenElement(aElementName, aMode);

  if aMode = stFinishClose then begin
    outElement.fOwner := nil;//do not use after close
  end else begin
    outElement.fOwner := Self;
    outElement.fElementName := aElementName;
    outElement.fOpenElementFinished := (aMode = stFinish);
    outElement.fChildrenWritten := False;
  end;
end;

procedure TXMLWriter.FinishOpenElement(const aElementName: OWideString);
begin
  RawChar('>');
end;

procedure TXMLWriter.FinishOpenElementClose(const aElementName: OWideString);
begin
  DecIndentLevel;
  RawChar('/');
  RawChar('>');
end;

function TXMLWriter.OpenElementR(const aElementName: OWideString;
  const aMode: TXMLWriterElementMode): TXMLWriterElement;
begin
  OpenElementR(aElementName, {%H-}Result, aMode);
end;

procedure TXMLWriter.Text(const aText: OWideString; const aQuoteChar: OWideChar);
var
  xC: OWideChar;
  I, xLength: Integer;
begin
  xLength := Length(aText);
  if xLength = 0 then
    Exit;

  for I := 1 to xLength do begin
    xC := aText[I];
    case OXmlCharKind(xC) of
      ckAmpersand: RawText('&amp;');
      ckLowerThan: RawText('&lt;');
      ckGreaterThan: RawText('&gt;');
      ckDoubleQuote:
        if aQuoteChar = '"' then
          RawText('&quot;')
        else
          RawChar(xC);
      ckSingleQuote:
        if aQuoteChar = '''' then
          RawText('&apos;')
        else
          RawChar(xC);
      ckNewLine10:
        if (fWriterSettings.fLineBreak = lbDoNotProcess) then//no line break handling
          RawChar(xC)
        else if ((I = 1) or (aText[I-1] <> #13)) then//previous character is not #13 (i.e. this is a simple #10 not #13#10) -> write fLineBreak
          RawText(XmlLineBreak[fWriterSettings.fLineBreak]);
      ckNewLine13:
        if fWriterSettings.fLineBreak = lbDoNotProcess then
          RawChar(xC)
        else
          RawText(XmlLineBreak[fWriterSettings.fLineBreak]);
      ckCharacter: RawChar(xC);
    else
      //invalid
      if not fWriterSettings.fStrictXML then
        RawChar(xC)
      else
        raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidText, [aText]);
    end;
  end;
end;

procedure TXMLWriter.XMLDeclaration(const aEncodingAttribute: Boolean;
  const aVersion, aStandAlone: OWideString);
begin
  OpenXMLDeclaration;

  if aVersion <> '' then
    Attribute('version', aVersion);
  if aEncodingAttribute then
    Attribute('encoding', fWriter.Encoding.EncodingAlias);
  if aStandAlone <> '' then
    Attribute('standalone', aStandAlone);

  FinishOpenXMLDeclaration;
end;

{ TXMLWriterElement }

procedure TXMLWriterElement.Attribute(const aAttrName, aAttrValue: OWideString);
begin
  if fOpenElementFinished then
    raise EXmlDOMException.CreateFmt(OXmlLng_CannotWriteAttributesWhenFinished, [aAttrName, aAttrValue, fElementName])
  else
    fOwner.Attribute(aAttrName, aAttrValue);
end;

procedure TXMLWriterElement.CData(const aText: OWideString);
begin
  FinishOpenElement;
  fChildrenWritten := True;
  fOwner.CData(aText);
end;

procedure TXMLWriterElement.Comment(const aText: OWideString);
begin
  FinishOpenElement;
  fChildrenWritten := True;
  fOwner.Comment(aText);
end;

procedure TXMLWriterElement.CloseElement;
begin
  if fChildrenWritten then
    fOwner.CloseElement(fElementName, True)
  else
    fOwner.FinishOpenElementClose;

  //DO NOT USE THIS RECORD ANY MORE
  fOwner := nil;
  fElementName := '';
end;

procedure TXMLWriterElement.ProcessingInstruction(const aTarget,
  aContent: OWideString);
begin
  FinishOpenElement;
  fChildrenWritten := True;
  fOwner.ProcessingInstruction(aTarget, aContent);
end;

procedure TXMLWriterElement.OpenElement(const aElementName: OWideString;
  const aMode: TXMLWriterElementMode);
begin
  FinishOpenElement;
  fChildrenWritten := True;
  fOwner.OpenElement(aElementName, aMode);
end;

procedure TXMLWriterElement.OpenElementR(const aElementName: OWideString;
  var outElement: TXMLWriterElement; const aMode: TXMLWriterElementMode);
begin
  FinishOpenElement;
  fChildrenWritten := True;
  fOwner.OpenElementR(aElementName, outElement, aMode);
end;

procedure TXMLWriterElement.FinishOpenElement;
begin
  if not fOpenElementFinished then begin
    fOwner.FinishOpenElement;
    fOpenElementFinished := True;
  end;
end;

function TXMLWriterElement.OpenElementR(const aElementName: OWideString;
  const aMode: TXMLWriterElementMode): TXMLWriterElement;
begin
  OpenElementR(aElementName, {%H-}Result, aMode);
end;

procedure TXMLWriterElement.Text(const aText: OWideString);
begin
  FinishOpenElement;
  fChildrenWritten := True;
  fOwner.Text(aText);
end;

{ EXmlReaderException }

constructor EXmlReaderException.Create(const aError: TXMLParseError;
  const aReason: string);
begin
  DoCreate(aError, aReason);

  inherited Create(
    aReason+
    sLineBreak+sLineBreak+
    GetFormattedErrorText);
end;

procedure EXmlReaderException.DoCreate(const aError: TXMLParseError;
  const aReason: string);
begin
  fReason := aReason;
  fLinePos := aError.LinePos;
  fLastTokenLinePos := aError.LastTokenLinePos;
  fLine := aError.Line;
  fLastTokenCharPos := aError.LastTokenCharPos;
  fSrcText := aError.SrcText;
  fSrcTextPos := aError.SrcTextPos;
end;

class function EXmlReaderException.GetErrorCode: Integer;
begin
  Result := 0;
end;

function EXmlReaderException.GetFormattedErrorText: OWideString;
begin
  Result :=
    Format(OXmlLng_ReadingAt, [
      Line,
      LinePos,
      LastTokenLinePos,
      LastTokenCharPos,
      SrcTextPos,
      SrcText]);
end;

{ TXMLWriterSettings }

procedure TXMLWriterSettings.AssignTo(Dest: TPersistent);
var
  xDest: TXMLWriterSettings;
begin
  if Dest is TXMLWriterSettings then begin
    xDest := TXMLWriterSettings(Dest);

    xDest.IndentString := Self.IndentString;
    xDest.IndentType := Self.IndentType;
    xDest.LineBreak := Self.LineBreak;
    xDest.StrictXML := Self.StrictXML;
    xDest.WriteBOM := Self.WriteBOM;
  end else
    inherited;
end;

constructor TXMLWriterSettings.Create;
begin
  inherited Create;

  fLineBreak := XmlDefaultLineBreak;
  fStrictXML := True;
  fWriteBOM := True;
  fIndentType := itNone;
  fIndentString := #32#32;
end;

procedure TXMLWriterSettings.SetWriteBOM(const aWriteBOM: Boolean);
begin
  fWriteBOM := aWriteBOM;
  if Assigned(fOnSetWriteBOM) then
    fOnSetWriteBOM(Self);
end;

{ TXMLReaderSettings }

procedure TXMLReaderSettings.AssignTo(Dest: TPersistent);
var
  xDest: TXMLReaderSettings;
{$IFDEF O_GENERICS}
  xEntity: TPair<OWideString,OWideString>;
{$ENDIF}
begin
  if Dest is TXMLReaderSettings then begin
    xDest := TXMLReaderSettings(Dest);

    xDest.BreakReading := Self.BreakReading;
    xDest.LineBreak := Self.LineBreak;
    xDest.StrictXML := Self.StrictXML;
    xDest.RecognizeXMLDeclaration := Self.RecognizeXMLDeclaration;
    xDest.ErrorHandling := Self.ErrorHandling;
    {$IFDEF O_GENERICS}
    xDest.EntityList.Clear;
    for xEntity in Self.EntityList do
      xDest.EntityList.Add(xEntity.Key, xEntity.Value);
    {$ELSE}
    xDest.EntityList.Assign(Self.EntityList);
    {$ENDIF}
  end else
    inherited;
end;

constructor TXMLReaderSettings.Create;
begin
  inherited Create;

  fEntityList := TXMLReaderEntityList.Create;
  fEntityList.Add('quot', '"');
  fEntityList.Add('amp', '&');
  fEntityList.Add('apos', '''');
  fEntityList.Add('lt', '<');
  fEntityList.Add('gt', '>');

  fBreakReading := brAfterDocumentElement;
  fLineBreak := XmlDefaultLineBreak;
  fStrictXML := True;
  fRecognizeXMLDeclaration := True;
  fErrorHandling := ehRaiseAndEat;
end;

destructor TXMLReaderSettings.Destroy;
begin
  fEntityList.Free;

  inherited Destroy;
end;

{ TXMLReaderTokenList }

procedure TXMLReaderTokenList.AddLast;
begin
  Inc(fCount);
end;

procedure TXMLReaderTokenList.Clear;
begin
  fCount := 0;
end;

constructor TXMLReaderTokenList.Create;
begin
  inherited Create;

  {$IFDEF O_GENERICS}
  fReaderTokens := TList<PXMLReaderToken>.Create;
  {$ELSE}
  fReaderTokens := TList.Create;
  {$ENDIF}
end;

function TXMLReaderTokenList.CreateNew: PXMLReaderToken;
begin
  if fCount = fReaderTokens.Count then begin
    New(Result);
    fReaderTokens.Add(Result);
  end else
    Result := GetToken(fCount);
end;

procedure TXMLReaderTokenList.DeleteLast;
begin
  Dec(fCount);
  if fCount < 0 then
    fCount := 0;
end;

destructor TXMLReaderTokenList.Destroy;
var
  I: Integer;
begin
  for I := 0 to fReaderTokens.Count-1 do
    Dispose(PXMLReaderToken(fReaderTokens[I]));
  fReaderTokens.Free;

  inherited;
end;

function TXMLReaderTokenList.GetToken(const aIndex: Integer): PXMLReaderToken;
begin
  Result := PXMLReaderToken(fReaderTokens[aIndex]);
end;

function TXMLReaderTokenList.IndexOf(const aToken: PXMLReaderToken): Integer;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
  if fReaderTokens[I] = aToken then begin
    Result := I;
    Exit;
  end;
  Result := -1;
end;

{ TXMLParseError }

constructor TXMLParseError.Create(const aReader: TXMLReader;
  const aReason: string);
begin
  inherited Create;

  DoCreate(aReader, aReason);
end;

constructor TXMLParseError.CreateFmt(const aReader: TXMLReader;
  const aReason: string; const aArgs: array of const);
var
  xReason: string;
begin
  inherited Create;

  xReason := Format(aReason, aArgs);
  DoCreate(aReader, xReason);
end;

procedure TXMLParseError.DoCreate(const aReader: TXMLReader;
  const aReason: string);
begin
  fReason := aReason;
  fLinePos := aReader.LinePosition;
  fLine := aReader.Line;
  fLastTokenLinePos := aReader.LastTokenLinePosition;
  fLastTokenCharPos := aReader.LastTokenCharPosition;
  fSrcText := aReader.fReader.ReadPreviousString(30, True);
  fSrcTextPos := Length(fSrcText);
  fSrcText := fSrcText + aReader.fReader.ReadString(10, True);
end;

function TXMLParseError.GetErrorCode: Integer;
begin
  Result := GetExceptionClass.GetErrorCode;
end;

function TXMLParseError.GetFilePos: Integer;
begin
  Result := fFilePos;
end;

function TXMLParseError.GetLine: Integer;
begin
  Result := fLine;
end;

function TXMLParseError.GetLinePos: Integer;
begin
  Result := fLinePos;
end;

function TXMLParseError.GetReason: OWideString;
begin
  Result := fReason;
end;

function TXMLParseError.GetSrcText: OWideString;
begin
  Result := fSrcText;
end;

function TXMLParseError.GetSrcTextPos: Integer;
begin
  Result := fSrcTextPos;
end;

function TXMLParseError.GetURL: OWideString;
begin
  Result := fURL;
end;

procedure TXMLParseError.RaiseAndEatException;
begin
  try
    RaiseException;
  except
    on EXmlReaderException do
    begin
      //eat EXmlReaderException
    end;
  end;
end;

procedure TXMLParseError.RaiseException;
begin
  raise GetExceptionClass.Create(Self, fReason);
end;

{ TXMLParseErrorInvalidCharacter }

function TXMLParseErrorInvalidCharacter.GetExceptionClass: EXmlReaderExceptionClass;
begin
  Result := EXmlReaderInvalidCharacter;
end;

{ TXMLParseErrorInvalidStructure }

function TXMLParseErrorInvalidStructure.GetExceptionClass: EXmlReaderExceptionClass;
begin
  Result := EXmlReaderInvalidStructure;
end;

{ EXmlReaderInvalidCharacter }

class function EXmlReaderInvalidCharacter.GetErrorCode: Integer;
begin
  Result := INVALID_CHARACTER_ERR;
end;

{ EXmlReaderInvalidStructure }

class function EXmlReaderInvalidStructure.GetErrorCode: Integer;
begin
  Result := HIERARCHY_REQUEST_ERR;
end;

end.
