unit OXmlUnitTests;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{$IFNDEF FPC}
  {$IF CompilerVersion >= 25}
    {$ZEROBASEDSTRINGS OFF}
    {$LEGACYIFEND ON}
  {$IFEND}
{$ENDIF}

interface

uses Classes, SysUtils, OWideSupp, OXmlUtils, OXmlReadWrite, OXmlPDOM, OXmlCDOM,
  OHashedStrings, OXmlSAX, OXmlSeq;

const
  cTestCount = 25;
  
type
  TObjFunc = function(): Boolean of object;

  TOXmlUnitTest = class(TObject)
  private
    fPassNameIfFalse: TStringList;
    fPassedCount: Integer;

    function GetAllTestCount: Integer;
    procedure ExecuteFunction(const aFunction: TObjFunc; const aFunctionName: String);
  private
    //OXmlReadWrite.pas
    function Test_TXMLReader_FinishOpenElementClose_NodeName_Empty: Boolean;
    function Test_TXMLReader_InvalidDocument1: Boolean;
  private
    //OXmlPDOM.pas
    function Test_OXmlPDOM_TXMLNode_SelectNodeCreate_Attribute: Boolean;
    function Test_OXmlPDOM_TXMLNode_Clone: Boolean;
    function Test_OXmlPDOM_TXMLNode_Normalize: Boolean;
    function Test_OXmlPDOM_TXMLDocument_InvalidDocument1: Boolean;
    function Test_OXmlPDOM_TXMLDocument_WhiteSpaceHandling: Boolean;
    function Test_OXmlPDOM_TXMLDocument_AttributeIndex: Boolean;
    function Test_OXmlPDOM_TXMLDocument_WrongDocument1: Boolean;
    function Test_OXmlPDOM_TXMLDocument_WrongDocument2: Boolean;
    function Test_OXmlPDOM_TXMLDocument_WrongDocument3: Boolean;
  private
    //OXmlCDOM.pas
    function Test_OXmlCDOM_TXMLNode_SelectNodeCreate_Attribute: Boolean;
    function Test_OXmlCDOM_TXMLNode_Clone: Boolean;
    function Test_OXmlCDOM_TXMLNode_Normalize: Boolean;
    function Test_OXmlCDOM_TXMLDocument_InvalidDocument1: Boolean;
    function Test_OXmlCDOM_TXMLDocument_WhiteSpaceHandling: Boolean;
    function Test_OXmlCDOM_TXMLDocument_AttributeIndex: Boolean;
    function Test_OXmlCDOM_TXMLDocument_WrongDocument1: Boolean;
    function Test_OXmlCDOM_TXMLDocument_WrongDocument2: Boolean;
    function Test_OXmlCDOM_TXMLDocument_WrongDocument3: Boolean;
  private
    //OWideSupp.pas
    function Test_TOTextBuffer: Boolean;
  private
    //OHashedStrings.pas
    function Test_TOHashedStrings_Grow: Boolean;
  private
    //OXmlSAX.pas
    procedure Test_TSAXParser_HashIndex_SAXStartElement({%H-}aSaxParser: TSAXParser;
      const {%H-}aName: OWideString; const aAttributes: TSAXAttributes);
    function Test_TSAXParser_HashIndex: Boolean;
  private
    //OXmlSeq.pas
    function Test_TXMLSeqParser_Test1: Boolean;
  private
    //OXmlXPath.pas
    function Test_OXmlXPath_Test1: Boolean;
  public
    procedure OXmlTestAll(const aStrList: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation


{ TOXmlUnitTest }

constructor TOXmlUnitTest.Create;
begin
  inherited Create;

  fPassNameIfFalse := TStringList.Create;
end;

destructor TOXmlUnitTest.Destroy;
begin
  fPassNameIfFalse.Free;

  inherited;
end;

procedure TOXmlUnitTest.ExecuteFunction(const aFunction: TObjFunc;
  const aFunctionName: String);
begin
  if not aFunction() then
    fPassNameIfFalse.Add(aFunctionName)
  else
    Inc(fPassedCount);
end;

function TOXmlUnitTest.GetAllTestCount: Integer;
begin
  Result := fPassNameIfFalse.Count + fPassedCount;
end;

procedure TOXmlUnitTest.OXmlTestAll(const aStrList: TStrings);
var
  I: Integer;
begin
  //because this tests are supposed to run in D7 and Lazarus too,
  //we cannot use RTTI to call all test functions automatically
  // -> call here all functions manually

  ExecuteFunction(Test_TXMLReader_FinishOpenElementClose_NodeName_Empty, 'Test_TXMLReader_FinishOpenElementClose_NodeName_Empty');
  ExecuteFunction(Test_TXMLReader_InvalidDocument1, 'Test_TXMLReader_InvalidDocument1');
  ExecuteFunction(Test_OXmlPDOM_TXMLNode_SelectNodeCreate_Attribute, 'Test_OXmlPDOM_TXMLNode_SelectNodeCreate_Attribute');
  ExecuteFunction(Test_OXmlPDOM_TXMLNode_Clone, 'Test_OXmlPDOM_TXMLNode_Clone');
  ExecuteFunction(Test_OXmlPDOM_TXMLNode_Normalize, 'Test_OXmlPDOM_TXMLNode_Normalize');
  ExecuteFunction(Test_OXmlPDOM_TXMLDocument_InvalidDocument1, 'Test_OXmlPDOM_TXMLDocument_InvalidDocument1');
  ExecuteFunction(Test_OXmlPDOM_TXMLDocument_WhiteSpaceHandling, 'Test_OXmlPDOM_TXMLDocument_WhiteSpaceHandling');
  ExecuteFunction(Test_OXmlPDOM_TXMLDocument_AttributeIndex, 'Test_OXmlPDOM_TXMLDocument_AttributeIndex');
  ExecuteFunction(Test_OXmlPDOM_TXMLDocument_WrongDocument1, 'Test_OXmlPDOM_TXMLDocument_WrongDocument1');
  ExecuteFunction(Test_OXmlPDOM_TXMLDocument_WrongDocument2, 'Test_OXmlPDOM_TXMLDocument_WrongDocument2');
  ExecuteFunction(Test_OXmlPDOM_TXMLDocument_WrongDocument3, 'Test_TXMLDocument_WrongDocument3');
  ExecuteFunction(Test_OXmlCDOM_TXMLNode_SelectNodeCreate_Attribute, 'Test_OXmlCDOM_TXMLNode_SelectNodeCreate_Attribute');
  ExecuteFunction(Test_OXmlCDOM_TXMLNode_Clone, 'Test_OXmlCDOM_TXMLNode_Clone');
  ExecuteFunction(Test_OXmlCDOM_TXMLNode_Normalize, 'Test_OXmlCDOM_TXMLNode_Normalize');
  ExecuteFunction(Test_OXmlCDOM_TXMLDocument_InvalidDocument1, 'Test_OXmlCDOM_TXMLDocument_InvalidDocument1');
  ExecuteFunction(Test_OXmlCDOM_TXMLDocument_WhiteSpaceHandling, 'Test_OXmlCDOM_TXMLDocument_WhiteSpaceHandling');
  ExecuteFunction(Test_OXmlCDOM_TXMLDocument_AttributeIndex, 'Test_OXmlCDOM_TXMLDocument_AttributeIndex');
  ExecuteFunction(Test_OXmlCDOM_TXMLDocument_WrongDocument1, 'Test_OXmlCDOM_TXMLDocument_WrongDocument1');
  ExecuteFunction(Test_OXmlCDOM_TXMLDocument_WrongDocument2, 'Test_OXmlCDOM_TXMLDocument_WrongDocument2');
  ExecuteFunction(Test_OXmlCDOM_TXMLDocument_WrongDocument3, 'Test_TXMLDocument_WrongDocument3');
  ExecuteFunction(Test_TOTextBuffer, 'Test_TOTextBuffer');
  ExecuteFunction(Test_TOHashedStrings_Grow, 'Test_TOHashedStrings_Grow');
  ExecuteFunction(Test_TSAXParser_HashIndex, 'Test_TSAXParser_HashIndex');
  ExecuteFunction(Test_TXMLSeqParser_Test1, 'Test_TXMLSeqParser_Test1');
  ExecuteFunction(Test_OXmlXPath_Test1, 'Test_OXmlXPath_Test1');

  aStrList.Clear;

  if fPassNameIfFalse.Count = 0 then
    aStrList.Add(Format('OXml: all tests from %d passed.', [GetAllTestCount]))
  else
  begin
    aStrList.Add('');
    aStrList.Add(Format('ERROR OXml: %d from %d test(s) not passed:', [fPassNameIfFalse.Count, GetAllTestCount]));

    for I := 0 to fPassNameIfFalse.Count-1 do
      aStrList.Add(fPassNameIfFalse[I]);
  end;

  if (GetAllTestCount <> cTestCount) then
  begin
    aStrList.Add('');
    aStrList.Add('ERROR OXmlUnitTest: test count is invalid.');
    aStrList.Add(Format('tests runned: %d, tests expected: %d',
      [GetAllTestCount, cTestCount]));
  end;

end;

function TOXmlUnitTest.Test_OXmlXPath_Test1: Boolean;
const
  cXML: OWideString =
    //'  '+sLineBreak+'  '+
    '<?xml version="1.0" encoding="utf-8" ?>'+
    '<root description="test xml">'+
      '<boss name="Max Muster">'+
        '<person name="boss person"/>'+
        '<person name="boss person 2">'+
          '<person name="boss person/2.1"/>'+
          '<dog name="boss dog 2.2" type="fight" />'+
        '</person>'+
      '</boss>'+
      '<!-- comment -->'+
      '<person name="Paul Caster">this text is in person tag</person>'+
      '<![CDATA[some test info]]>'+
      '<?pi processing instruction ?>'+
    '</root>';

var
  xXml: OXmlPDOM.IXMLDocument;

  function _TestXPathElements(const aStartNode: OXmlPDOM.PXMLNode; const aXPath, aResult: OWideString): Boolean;
  var
    xList: OXmlPDOM.IXMLNodeList;
    xElement: OXmlPDOM.PXMLNode;
    xStr: OWideString;
    I: Integer;
  begin
    if aStartNode.SelectNodes(aXPath, {%H-}xList) then begin
      xStr := '';
      for I := 0 to xList.Count-1 do begin
        xElement := xList[I];

        if xStr <> '' then
        if xStr <> '' then
          xStr := xStr+sLineBreak;
        case xElement.NodeType of
          ntElement: xStr := xStr+xElement.NodeName+'='+xElement.Attributes['name'];
          ntAttribute: xStr := xStr+xElement.ParentNode.NodeName+':'+xElement.NodeName+'='+xElement.NodeValue;
          ntText, ntCData: xStr := xStr+xElement.NodeValue;
        end;
      end;

      Result := (xStr = aResult);
    end else begin
      Result := (aResult = '');//nothing selected
    end;
  end;
begin
  xXml := OXmlPDOM.CreateXMLDoc;
  xXml.LoadFromXML(cXML);

  Result := True;

  Result := Result and _TestXPathElements(xXml.DocumentElement, '.', 'root=');
  Result := Result and _TestXPathElements(xXml.DocumentElement, '../root', 'root=');
  Result := Result and _TestXPathElements(xXml.DocumentElement, '../root|../root', 'root=');
  Result := Result and _TestXPathElements(xXml.DocumentElement, '../root/.', 'root=');
  Result := Result and _TestXPathElements(xXml.DocumentElement, '../root/boss/..', 'root=');
  Result := Result and _TestXPathElements(xXml.DocumentElement, '../root/person', 'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.DocumentElement, '..//person[@name="boss person/2.1"]', 'person=boss person/2.1');
  Result := Result and _TestXPathElements(xXml.DocumentElement, '//person[@name="boss person/2.1"]', 'person=boss person/2.1');
  Result := Result and _TestXPathElements(xXml.Node, '//person[@name]', 'person=boss person'+sLineBreak+'person=boss person 2'+sLineBreak+'person=boss person/2.1'+sLineBreak+'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, '//root//person/*', 'person=boss person/2.1'+sLineBreak+'dog=boss dog 2.2');
  Result := Result and _TestXPathElements(xXml.Node, '//person/../../boss', 'boss=Max Muster');
  Result := Result and _TestXPathElements(xXml.Node, '//person', 'person=boss person'+sLineBreak+'person=boss person 2'+sLineBreak+'person=boss person/2.1'+sLineBreak+'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, 'root//person', 'person=boss person'+sLineBreak+'person=boss person 2'+sLineBreak+'person=boss person/2.1'+sLineBreak+'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, 'root//boss/person', 'person=boss person'+sLineBreak+'person=boss person 2');
  Result := Result and _TestXPathElements(xXml.Node, 'root//*', 'boss=Max Muster'+sLineBreak+'person=boss person'+sLineBreak+'person=boss person 2'+sLineBreak+'person=boss person/2.1'+sLineBreak+'dog=boss dog 2.2'+sLineBreak+'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, 'root/*', 'boss=Max Muster'+sLineBreak+'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, '/root/boss/person', 'person=boss person'+sLineBreak+'person=boss person 2');
  Result := Result and _TestXPathElements(xXml.Node, 'root/boss', 'boss=Max Muster');
  Result := Result and _TestXPathElements(xXml.Node, 'root/person|root/boss', 'boss=Max Muster'+sLineBreak+'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, 'root', 'root=');
  Result := Result and _TestXPathElements(xXml.Node, 'root/boss/person[2]/*', 'person=boss person/2.1'+sLineBreak+'dog=boss dog 2.2');
  Result := Result and _TestXPathElements(xXml.Node, 'root/person[1]', 'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, 'root/person[last()]', 'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, '/root/*[last()-1]/person[last()]/*', 'person=boss person/2.1'+sLineBreak+'dog=boss dog 2.2');
  Result := Result and _TestXPathElements(xXml.Node, '//text()', 'this text is in person tag'+sLineBreak+'some test info');
  Result := Result and _TestXPathElements(xXml.Node, 'root/node()', 'root:description=test xml'+sLineBreak+'boss=Max Muster'+sLineBreak+'person=Paul Caster'+sLineBreak+'some test info');


  Result := Result and _TestXPathElements(xXml.Node, 'root//@*', 'root:description=test xml'+sLineBreak+'boss:name=Max Muster'+sLineBreak+'person:name=boss person'+sLineBreak+'person:name=boss person 2'+sLineBreak+'person:name=boss person/2.1'+sLineBreak+'dog:name=boss dog 2.2'+sLineBreak+'dog:type=fight'+sLineBreak+'person:name=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, 'root//@name', 'boss:name=Max Muster'+sLineBreak+'person:name=boss person'+sLineBreak+'person:name=boss person 2'+sLineBreak+'person:name=boss person/2.1'+sLineBreak+'dog:name=boss dog 2.2'+sLineBreak+'person:name=Paul Caster');
end;

function TOXmlUnitTest.Test_TOHashedStrings_Grow: Boolean;
var
  xHS: TOHashedStrings;
  I: Integer;
begin
  xHS := TOHashedStrings.Create;
  try
    for I := 1 to 35 do
      xHS.Add(IntToStr(I));

    //36 is the limit when GrowBuckets is called and new hashes are generated
    xHS.Add('x');
    //x must be found in created list by a new hash!
    xHS.Add('x');

    Result := xHS.Count = 36;
  finally
    xHS.Free;
  end;
end;

function TOXmlUnitTest.Test_TOTextBuffer: Boolean;
var
  xC: OWideString;
  xBuf: TOTextBuffer;
  I, L: Integer;
begin
  xBuf := TOTextBuffer.Create;
  try
    for L := 1 to 2 do begin
      for I := 0 to 10*1000 - 1 do
        xBuf.WriteChar(OWideChar(IntToStr(I mod 10)[1]));

      Result := xBuf.UsedLength = (10*1000);
      if not Result then Exit;

      for I := 0 to xBuf.UsedLength-1 do begin
        xBuf.GetBuffer({%H-}xC, I+1, 1);
        Result := xC = IntToStr(I mod 10);
        if not Result then
          Exit;
      end;

      xBuf.Clear(True);
    end;
  finally
    xBuf.Free;
  end;
end;

function TOXmlUnitTest.Test_TSAXParser_HashIndex: Boolean;
var
  xStream: TMemoryStream;
  xWriter: TXMLWriter;
  xSAXParser: TSAXParser;
  I: Integer;
  xAttr: OWideString;
begin
  xStream := nil;
  xWriter := nil;
  xSAXParser := nil;
  try
    xStream := TMemoryStream.Create;
    xWriter := TXMLWriter.Create(xStream);
    xWriter.OpenElement('root', stFinish);

    xWriter.OpenElement('ten');//under the hash index limit
    for I := 1 to 10 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('ten');

    xWriter.OpenElement('thousand');//above the hash index limit
    for I := 1 to 1000 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('thousand');

    xWriter.OpenElement('tenthousand');//above the hash index limit
    for I := 1 to 10*1000 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('tenthousand');

    xWriter.CloseElement('root');
    xWriter.Free;
    xWriter := nil;

    xStream.Position := 0;

    xSAXParser := TSAXParser.Create;
    xSAXParser.OnStartElement := Test_TSAXParser_HashIndex_SAXStartElement;
    xSAXParser.ParseStream(xStream);

  finally
    xWriter.Free;
    xStream.Free;
    xSAXParser.Free;
  end;

  Result := True;//always true -> check for assertions in Test_TSAXParser_HashIndex_SAXStartElement
end;

procedure TOXmlUnitTest.Test_TSAXParser_HashIndex_SAXStartElement(
  aSaxParser: TSAXParser; const aName: OWideString;
  const aAttributes: TSAXAttributes);
var
  I: Integer;
  xAttrName, xAttrValue: OWideString;
begin
  for I := 1 to aAttributes.Count do
  begin
    xAttrName := 'a'+IntToStr(I);
    aAttributes.Find(xAttrName, {%H-}xAttrValue);
    Assert(xAttrName = xAttrValue);
  end;
end;

function TOXmlUnitTest.Test_TXMLReader_FinishOpenElementClose_NodeName_Empty: Boolean;
var
  xReader: TXMLReader;
  xReaderToken: PXMLReaderToken;
  xResult: OWideString;
begin
  xReader := TXMLReader.Create;
  try
    xReader.InitXML('<root attribute="1" />');

    xResult := '';
    while xReader.ReadNextToken({%H-}xReaderToken) do
    begin
      xResult := xResult + Format('%d:%s:%s;', [Ord(xReaderToken.TokenType), xReaderToken.TokenName, xReaderToken.TokenValue]);
    end;

    Result := xResult = '4:root:;5:attribute:1;7:root:;';
  finally
    xReader.Free;
  end;
end;

function TOXmlUnitTest.Test_TXMLReader_InvalidDocument1: Boolean;
const
  inXML: OWideString = '<root><b>TEXT</i><p><t><aaa/></p></root>';
var
  xXMLReader: TXMLReader;
  xToken: PXMLReaderToken;

  procedure CheckNextToken(aTokenType: TXMLReaderTokenType; const aTokenName, aTokenValue: OWideString);
  begin
    Result := Result and xXMLReader.ReadNextToken(xToken) and
      ((xToken.TokenType = aTokenType) and (xToken.TokenName = aTokenName) and (xToken.TokenValue = aTokenValue));
  end;
begin
  Result := True;

  xXMLReader := TXMLReader.Create;
  try
    xXMLReader.ReaderSettings.StrictXML := False;

    xXMLReader.InitXML(inXML);

    CheckNextToken(rtOpenElement, 'root', '');
    CheckNextToken(rtFinishOpenElement, 'root', '');
    CheckNextToken(rtOpenElement, 'b', '');
    CheckNextToken(rtFinishOpenElement, 'b', '');
    CheckNextToken(rtText, '', 'TEXT');
    CheckNextToken(rtCloseElement, 'b', '');
    CheckNextToken(rtOpenElement, 'p', '');
    CheckNextToken(rtFinishOpenElement, 'p', '');
    CheckNextToken(rtOpenElement, 't', '');
    CheckNextToken(rtFinishOpenElement, 't', '');
    CheckNextToken(rtOpenElement, 'aaa', '');
    CheckNextToken(rtFinishOpenElementClose, 'aaa', '');
    CheckNextToken(rtCloseElement, 't', '');
    CheckNextToken(rtCloseElement, 'p', '');
    CheckNextToken(rtCloseElement, 'root', '');

  finally
    xXMLReader.Free;
  end;
end;

function TOXmlUnitTest.Test_TXMLSeqParser_Test1: Boolean;
const
  inXml: OWideString =
    '<?xml version="1.0" encoding="UTF-8"?>'+sLineBreak+
    '<teryt>'+sLineBreak+
    '  <catalog name="ULIC">'+sLineBreak+
    '    <row name="row1">'+sLineBreak+
    '      <col name="WOJ">04</col>'+sLineBreak+
    '      <col name="POW">10</col>'+sLineBreak+
    '    </row>'+sLineBreak+
    '    <row name="row2">'+sLineBreak+
    '      <col name="ABC">09</col>'+sLineBreak+
    '      <col name="CDE">11</col>'+sLineBreak+
    '    </row>'+sLineBreak+
    '    <row name="row3">'+sLineBreak+
    '      <col name="REW">00</col>'+sLineBreak+
    '      <col name="OLD">99</col>'+sLineBreak+
    '    </row>'+sLineBreak+
    '  </catalog>'+sLineBreak+
    '</teryt>'+sLineBreak;
  outStr: OWideString = 'WOJ:04;POW:10;ABC:09;CDE:11;REW:00;OLD:99;';
var
  xXMLSeq: TXMLSeqParser;
  xNode: OXmlPDOM.PXMLNode;
  xColNode: OXmlPDOM.PXMLNode;
  xName, xValue:String;
  xOpened: Boolean;
  xStr: OWideString;
begin
  Result := False;

  xXMLSeq := TXMLSeqParser.Create;
  try
    xXMLSeq.InitXML(inXml);
    xXMLSeq.WhiteSpaceHandling := wsTrim;

    if not(xXMLSeq.GoToPath('/teryt/catalog')) then
      Exit;

    if not((xXMLSeq.ReadNextChildElementHeader({%H-}xNode, {%H-}xOpened)) and xOpened) then
      Exit;

    xStr := '';
    while xXMLSeq.ReadNextChildNode(xNode) do
    begin
      if(xNode.NodeType = ntElement) and (xNode.NodeName = 'row') then
      begin
        xColNode := nil;
        while xNode.GetNextChild(xColNode) do
        begin
          xName := xColNode.GetAttribute('name');
          xValue := xColNode.Text;

          xStr := xStr + xName+':'+xValue+';'
        end;
      end;
    end;

    Result := (xStr = outStr);
  finally
    xXMLSeq.Free;
  end;
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLDocument_AttributeIndex: Boolean;
  procedure _TestNode(const bNode: OXmlPDOM.PXMLNode);
  var
    I: Integer;
    xAttr: OWideString;
  begin
    for I := 1 to bNode.AttributeCount do
    begin
      xAttr := 'a'+IntToStr(I);
      Result := (bNode.GetAttribute(xAttr) = xAttr);
      if not Result then
        Exit;
    end;
  end;
var
  xStream: TMemoryStream;
  xWriter: TXMLWriter;
  xXML: OXmlPDOM.IXMLDocument;
  I: Integer;
  xAttr: OWideString;
  xNode: OXmlPDOM.PXMLNode;
begin
  xStream := nil;
  xWriter := nil;
  try
    xStream := TMemoryStream.Create;
    xWriter := TXMLWriter.Create(xStream);
    xWriter.OpenElement('root', stFinish);

    xWriter.OpenElement('ten');//under the hash index limit
    for I := 1 to 10 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('ten');

    xWriter.OpenElement('thousand');//above the hash index limit
    for I := 1 to 1000 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('thousand');

    xWriter.OpenElement('tenthousand');//above the hash index limit
    for I := 1 to 10*1000 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('tenthousand');

    xWriter.CloseElement('root');
    xWriter.Free;
    xWriter := nil;

    xStream.Position := 0;

    xXML := OXmlPDOM.CreateXMLDoc;
    xXML.LoadFromStream(xStream);

    xNode := xXML.Node.SelectNode('root/ten');
    Result := xNode.AttributeCount = 10;
    if not Result then Exit;
    _TestNode(xNode);

    xNode := xXML.Node.SelectNode('root/thousand');
    Result := xNode.AttributeCount = 1000;
    if not Result then Exit;
    _TestNode(xNode);

    xNode := xXML.Node.SelectNode('root/tenthousand');
    Result := xNode.AttributeCount = 10*1000;
    if not Result then Exit;
    _TestNode(xNode);

  finally
    xWriter.Free;
    xStream.Free;
  end;

  Result := True;//always true -> check for assertions in Test_TSAXParser_HashIndex_SAXStartElement
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLDocument_InvalidDocument1: Boolean;
const
  inXML: OWideString = '<root><b>TEXT</i><p><t><aaa/></p></root>';
  outXML: OWideString = '<root><b>TEXT</b><p><t><aaa/></t></p></root>';
var
  xXML: OXmlPDOM.IXMLDocument;
begin
  xXML := OXmlPDOM.CreateXMLDoc;
  xXML.ReaderSettings.StrictXML := False;
  xXML.WriterSettings.StrictXML := False;

  xXML.WhiteSpaceHandling := wsPreserveAll;
  xXML.LoadFromXML(inXML);

  Result := (xXML.XML = outXML);
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLDocument_WhiteSpaceHandling: Boolean;
const
  inXML: OWideString =  '<root xml:space="preserve">'+sLineBreak+'<text xml:space="default"> default <p xml:space="preserve"> text <b> hello <br/> </b>  my text'+sLineBreak+'</p>  </text>  </root>';
  outXML: OWideString = '<root xml:space="preserve">'+sLineBreak+'<text xml:space="default">default<p xml:space="preserve"> text <b> hello <br/> </b>  my text'+sLineBreak+'</p></text>  </root>';
var
  xXML: OXmlPDOM.IXMLDocument;
begin
  xXML := OXmlPDOM.CreateXMLDoc;

  xXML.WhiteSpaceHandling := wsAutoTag;
  xXML.LoadFromXML(inXML);

  Result := (xXML.XML = outXML);
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLDocument_WrongDocument1: Boolean;
const
  inXML: OWideString =
    '<Test>'+sLineBreak+
    '  <T1>'#0'</T1> {Chr(0)}'+sLineBreak+
    '</Test>'+sLineBreak;
var
  xXML: OXmlPDOM.IXMLDocument;
begin
  xXML := OXmlPDOM.CreateXMLDoc;

  xXML.ReaderSettings.ErrorHandling := ehSilent;

  Result :=
    not xXML.LoadFromXML(inXML);
  Result := Result and
    (xXML.ParseError.Line = 2) and
    (xXML.ParseError.LinePos = 7) and
    (xXML.ParseError.ErrorCode = INVALID_CHARACTER_ERR);

  if not Result then
    Exit;

  //now check XML read in not strict mode
  xXML.ReaderSettings.StrictXML := False;
  Result :=
    xXML.LoadFromXML(inXML);

  Result := Result and
    (xXML.Node.SelectNode('/Test/T1').Text = #0);
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLDocument_WrongDocument2: Boolean;
const
  inXML: OWideString =
    '<Test>'+sLineBreak+
    '  <T1>0</T1> {Chr(0)}';
var
  xXML: OXmlPDOM.IXMLDocument;
begin
  xXML := OXmlPDOM.CreateXMLDoc;

  xXML.ReaderSettings.ErrorHandling := ehSilent;

  Result :=
    not xXML.LoadFromXML(inXML);
  Result := Result and
    (xXML.ParseError.Line = 2) and
    (xXML.ParseError.LinePos = 21) and
    (xXML.ParseError.ErrorCode = HIERARCHY_REQUEST_ERR);

  if not Result then
    Exit;

  //now check XML read in not strict mode
  xXML.ReaderSettings.StrictXML := False;
  Result :=
    xXML.LoadFromXML(inXML);

  Result := Result and
    (xXML.Node.SelectNode('/Test/T1').Text = '0');
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLDocument_WrongDocument3: Boolean;
const
  inXML: OWideString =
    '<Test> /> </Test>';
var
  xXML: OXmlPDOM.IXMLDocument;
begin
  xXML := OXmlPDOM.CreateXMLDoc;

  xXML.ReaderSettings.ErrorHandling := ehSilent;

  Result :=
    not xXML.LoadFromXML(inXML);
  Result := Result and
    (xXML.ParseError.Line = 1) and
    (xXML.ParseError.LinePos = 9) and
    (xXML.ParseError.ErrorCode = INVALID_CHARACTER_ERR);

  if not Result then
    Exit;

  //now check XML read in not strict mode
  xXML.ReaderSettings.StrictXML := False;
  xXML.WhiteSpaceHandling := wsPreserveAll;
  Result :=
    xXML.LoadFromXML(inXML);

  Result := Result and
    (xXML.Node.SelectNode('/Test').Text = ' /> ');
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLNode_Clone: Boolean;
const
  inXML: OWideString = '<root><clone attr="value"><n>text</n><m/></clone></root>';
  outXML: OWideString = '<root><clone attr="value"><n>text</n><m/></clone><clone attr="value"/><clone attr="value"><n>text</n><m/></clone></root>';
var
  xXML: OXmlPDOM.IXMLDocument;
  xCloneNode: OXmlPDOM.PXMLNode;
begin
  xXML := OXmlPDOM.CreateXMLDoc;
  xXML.LoadFromXML(inXML);
  xCloneNode := xXML.DocumentElement.SelectNode('clone');
  xXML.DocumentElement.AppendChild(xCloneNode.CloneNode(False));
  xXML.DocumentElement.AppendChild(xCloneNode.CloneNode(True));

  Result := xXML.XML = outXML;
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLNode_Normalize: Boolean;
const
  outXML: OWideString = '<root><test/>my  text<b>hello<clone/></b></root>';
var
  xXML: OXmlPDOM.IXMLDocument;
  xDocElement, xNodeB: OXmlPDOM.PXMLNode;
begin
  xXML := OXmlPDOM.CreateXMLDoc('root');
  xXML.WhiteSpaceHandling := wsPreserveAll;
  xDocElement := xXML.DocumentElement;
  xDocElement.AddText(sLineBreak+'   '+sLineBreak+#9);
  xDocElement.AddChild('test');
  xDocElement.AddText(#9'my  text '+sLineBreak);
  xDocElement.AddText(sLineBreak);
  xNodeB := xDocElement.AddChild('b');
  xNodeB.AddText('  ');
  xNodeB.AddText('hello');
  xNodeB.AddText(sLineBreak);
  xNodeB.AddText('  ');
  xNodeB.AddChild('clone');
  xNodeB.AddText('  ');

  xDocElement.Normalize;

  Result := xXML.XML = outXML;
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLNode_SelectNodeCreate_Attribute: Boolean;
var
  xXML: OXmlPDOM.IXMLDocument;
  xAttribute: OXmlPDOM.PXMLNode;
begin
  xXML := OXmlPDOM.CreateXMLDoc('root', False);

  xAttribute := xXML.DocumentElement.SelectNodeCreate('@attr');
  xAttribute.NodeValue := 'value';

  Result := (xXML.XML = '<root attr="value"/>');
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLDocument_AttributeIndex: Boolean;
  procedure _TestNode(const bNode: OXmlCDOM.TXMLNode);
  var
    I: Integer;
    xAttr: OWideString;
  begin
    for I := 1 to bNode.AttributeCount do
    begin
      xAttr := 'a'+IntToStr(I);
      Result := (bNode.GetAttribute(xAttr) = xAttr);
      if not Result then
        Exit;
    end;
  end;
var
  xStream: TMemoryStream;
  xWriter: TXMLWriter;
  xXML: OXmlCDOM.IXMLDocument;
  I: Integer;
  xAttr: OWideString;
  xNode: OXmlCDOM.TXMLNode;
begin
  xStream := nil;
  xWriter := nil;
  try
    xStream := TMemoryStream.Create;
    xWriter := TXMLWriter.Create(xStream);
    xWriter.OpenElement('root', stFinish);

    xWriter.OpenElement('ten');//under the hash index limit
    for I := 1 to 10 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('ten');

    xWriter.OpenElement('thousand');//above the hash index limit
    for I := 1 to 1000 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('thousand');

    xWriter.OpenElement('tenthousand');//above the hash index limit
    for I := 1 to 10*1000 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('tenthousand');

    xWriter.CloseElement('root');
    xWriter.Free;
    xWriter := nil;

    xStream.Position := 0;

    xXML := OXmlCDOM.CreateXMLDoc;
    xXML.LoadFromStream(xStream);

    xNode := xXML.Node.SelectNode('root/ten');
    Result := xNode.AttributeCount = 10;
    if not Result then Exit;
    _TestNode(xNode);

    xNode := xXML.Node.SelectNode('root/thousand');
    Result := xNode.AttributeCount = 1000;
    if not Result then Exit;
    _TestNode(xNode);

    xNode := xXML.Node.SelectNode('root/tenthousand');
    Result := xNode.AttributeCount = 10*1000;
    if not Result then Exit;
    _TestNode(xNode);

  finally
    xWriter.Free;
    xStream.Free;
  end;

  Result := True;//always true -> check for assertions in Test_TSAXParser_HashIndex_SAXStartElement
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLDocument_InvalidDocument1: Boolean;
const
  inXML: OWideString = '<root><b>TEXT</i><p><t><aaa/></p></root>';
  outXML: OWideString = '<root><b>TEXT</b><p><t><aaa/></t></p></root>';
var
  xXML: OXmlCDOM.IXMLDocument;
begin
  xXML := OXmlCDOM.CreateXMLDoc;
  xXML.ReaderSettings.StrictXML := False;
  xXML.WriterSettings.StrictXML := False;

  xXML.WhiteSpaceHandling := wsPreserveAll;
  xXML.LoadFromXML(inXML);

  Result := (xXML.XML = outXML);
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLDocument_WhiteSpaceHandling: Boolean;
const
  inXML: OWideString =  '<root xml:space="preserve">'+sLineBreak+'<text xml:space="default"> default <p xml:space="preserve"> text <b> hello <br/> </b>  my text'+sLineBreak+'</p>  </text>  </root>';
  outXML: OWideString = '<root xml:space="preserve">'+sLineBreak+'<text xml:space="default">default<p xml:space="preserve"> text <b> hello <br/> </b>  my text'+sLineBreak+'</p></text>  </root>';
var
  xXML: OXmlCDOM.IXMLDocument;
begin
  xXML := OXmlCDOM.CreateXMLDoc;

  xXML.WhiteSpaceHandling := wsAutoTag;
  xXML.LoadFromXML(inXML);

  Result := (xXML.XML = outXML);
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLDocument_WrongDocument1: Boolean;
const
  inXML: OWideString =
    '<Test>'+sLineBreak+
    '  <T1>'#0'</T1> {Chr(0)}'+sLineBreak+
    '</Test>'+sLineBreak;
var
  xXML: OXmlCDOM.IXMLDocument;
begin
  xXML := OXmlCDOM.CreateXMLDoc;

  xXML.ReaderSettings.ErrorHandling := ehSilent;

  Result :=
    not xXML.LoadFromXML(inXML);
  Result := Result and
    (xXML.ParseError.Line = 2) and
    (xXML.ParseError.LinePos = 7) and
    (xXML.ParseError.ErrorCode = INVALID_CHARACTER_ERR);

  if not Result then
    Exit;

  //now check XML read in not strict mode
  xXML.ReaderSettings.StrictXML := False;
  Result :=
    xXML.LoadFromXML(inXML);

  Result := Result and
    (xXML.Node.SelectNode('/Test/T1').Text = #0);
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLDocument_WrongDocument2: Boolean;
const
  inXML: OWideString =
    '<Test>'+sLineBreak+
    '  <T1>0</T1> {Chr(0)}';
var
  xXML: OXmlCDOM.IXMLDocument;
begin
  xXML := OXmlCDOM.CreateXMLDoc;

  xXML.ReaderSettings.ErrorHandling := ehSilent;

  Result :=
    not xXML.LoadFromXML(inXML);
  Result := Result and
    (xXML.ParseError.Line = 2) and
    (xXML.ParseError.LinePos = 21) and
    (xXML.ParseError.ErrorCode = HIERARCHY_REQUEST_ERR);

  if not Result then
    Exit;

  //now check XML read in not strict mode
  xXML.ReaderSettings.StrictXML := False;
  Result :=
    xXML.LoadFromXML(inXML);

  Result := Result and
    (xXML.Node.SelectNode('/Test/T1').Text = '0');
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLDocument_WrongDocument3: Boolean;
const
  inXML: OWideString =
    '<Test> /> </Test>';
var
  xXML: OXmlCDOM.IXMLDocument;
begin
  xXML := OXmlCDOM.CreateXMLDoc;

  xXML.ReaderSettings.ErrorHandling := ehSilent;

  Result :=
    not xXML.LoadFromXML(inXML);
  Result := Result and
    (xXML.ParseError.Line = 1) and
    (xXML.ParseError.LinePos = 9) and
    (xXML.ParseError.ErrorCode = INVALID_CHARACTER_ERR);

  if not Result then
    Exit;

  //now check XML read in not strict mode
  xXML.ReaderSettings.StrictXML := False;
  xXML.WhiteSpaceHandling := wsPreserveAll;
  Result :=
    xXML.LoadFromXML(inXML);

  Result := Result and
    (xXML.Node.SelectNode('/Test').Text = ' /> ');
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLNode_Clone: Boolean;
const
  inXML: OWideString = '<root><clone attr="value"><n>text</n><m/></clone></root>';
  outXML: OWideString = '<root><clone attr="value"><n>text</n><m/></clone><clone attr="value"/><clone attr="value"><n>text</n><m/></clone></root>';
var
  xXML: OXmlCDOM.IXMLDocument;
  xCloneNode: OXmlCDOM.TXMLNode;
begin
  xXML := OXmlCDOM.CreateXMLDoc;
  xXML.LoadFromXML(inXML);
  xCloneNode := xXML.DocumentElement.SelectNode('clone');
  xXML.DocumentElement.AppendChild(xCloneNode.CloneNode(False));
  xXML.DocumentElement.AppendChild(xCloneNode.CloneNode(True));

  Result := xXML.XML = outXML;
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLNode_Normalize: Boolean;
const
  outXML: OWideString = '<root><test/>my  text<b>hello<clone/></b></root>';
var
  xXML: OXmlCDOM.IXMLDocument;
  xDocElement, xNodeB: OXmlCDOM.TXMLNode;
begin
  xXML := OXmlCDOM.CreateXMLDoc('root');
  xXML.WhiteSpaceHandling := wsPreserveAll;
  xDocElement := xXML.DocumentElement;
  xDocElement.AddText(sLineBreak+'   '+sLineBreak+#9);
  xDocElement.AddChild('test');
  xDocElement.AddText(#9'my  text '+sLineBreak);
  xDocElement.AddText(sLineBreak);
  xNodeB := xDocElement.AddChild('b');
  xNodeB.AddText('  ');
  xNodeB.AddText('hello');
  xNodeB.AddText(sLineBreak);
  xNodeB.AddText('  ');
  xNodeB.AddChild('clone');
  xNodeB.AddText('  ');

  xDocElement.Normalize;

  Result := xXML.XML = outXML;
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLNode_SelectNodeCreate_Attribute: Boolean;
var
  xXML: OXmlCDOM.IXMLDocument;
  xAttribute: OXmlCDOM.TXMLNode;
begin
  xXML := OXmlCDOM.CreateXMLDoc('root', False);

  xAttribute := xXML.DocumentElement.SelectNodeCreate('@attr');
  xAttribute.NodeValue := 'value';

  Result := (xXML.XML = '<root attr="value"/>');
end;

end.

