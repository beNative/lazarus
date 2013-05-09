unit NativeXmlXPath;

interface

{$mode delphi}

uses
  Classes,
  NativeXml;

type
  TXmlNodeHelper = class helper for TXmlNode
  private
    function ProcessXPath(const StartNode: TXmlNode; XPath: string;
                          const ResultNodes: TList; const StopWhenFound: Boolean): Integer;
  public
    function SelectNode(const XPath: string): TXmlNode;
    function SelectNodes(XPath: string; const Nodes: TList): Integer;
  end;



implementation

uses
  SysUtils;

{ ================================================================================================ }
{ TXmlNodeHelper }

{ ------------------------------------------------------------------------------------------------ }
function TXmlNodeHelper.ProcessXPath(const StartNode: TXmlNode; XPath: string;
                                     const ResultNodes: TList; const StopWhenFound: Boolean): Integer;
var
  Recursive: boolean;
  SlashPos: Integer;
  NodeName: string;
  i: Integer;
  Child: TXmlNode;
  NodesToSearch: TList;
label
  FindFirstSlash;
begin
  Result := 0;
  if not Assigned(ResultNodes) then Exit;

  Recursive := False;
FindFirstSlash:
  SlashPos := Pos('/', XPath);
  case SlashPos of
    0: begin // no slash present
      NodeName := XPath;
      XPath := '';
    end;
    1: begin // starting with a slash; this was '//'
      Recursive := True;
      XPath := Copy(XPath, 2, Length(XPath));
      goto FindFirstSlash;
    end;
    else begin
      NodeName := Copy(XPath, 1, SlashPos - 1);
      XPath := Copy(XPath, SlashPos + 1, Length(XPath));
    end;
  end;

  if (NodeName = '') and (XPath = '') then begin
    ResultNodes.Add(StartNode);
    Result := 1;
    Exit;
  end else if NodeName = '.' then begin
    Assert(not Recursive, 'The expression "//." is not supported.');
    Result := Result + ProcessXPath(StartNode, XPath, ResultNodes, StopWhenFound);
    if StopWhenFound and (Result > 0) then
      Exit;
  end else if NodeName = '..' then begin
    Assert(not Recursive, 'The expression "//.." is not supported.');
    Result := Result + ProcessXPath(StartNode, XPath, ResultNodes, StopWhenFound);
    if StopWhenFound and (Result > 0) then
      Exit;
  end else if NodeName = '*' then begin
    Assert(not Recursive, 'The expression "//*" is not supported.');
    NodeName := '';
  end;

  if Recursive then begin
    NodesToSearch := TList.Create;
    try
      StartNode.FindNodes(UTF8String(NodeName), NodesToSearch);
      for i := 0 to NodesToSearch.Count - 1 do begin
        Child := NodesToSearch[i];
        Result := Result + ProcessXPath(Child, XPath, ResultNodes, StopWhenFound);
        if StopWhenFound and (Result > 0) then
          Exit;
      end;
    finally
      NodesToSearch.Free;
    end;
  end else begin
    for i := 0 to StartNode.NodeCount - 1 do begin
      Child := StartNode.Nodes[i];
      if (NodeName = '') or (string(Child.Name) = NodeName) then begin
        Result := Result + ProcessXPath(Child, XPath, ResultNodes, StopWhenFound);
        if StopWhenFound and (Result > 0) then
          Exit;
      end;
    end;
  end;

end {TXmlNodeHelper.ProcessXPath};

{ ------------------------------------------------------------------------------------------------ }
function TXmlNodeHelper.SelectNode(const XPath: string): TXmlNode;
var
  Nodes: TList;
begin
  Nodes := TList.Create;
  try
    if Copy(XPath, 1, 1) = '/' then begin
      ProcessXPath(Self.Document.Root, Copy(XPath, 2, 2), Nodes, True);
    end else begin
      ProcessXPath(Self, XPath, Nodes, True);
    end;
    if Nodes.Count > 0 then
      Result := TXmlNode(Nodes[0])
    else
      Result := nil;
  finally
    Nodes.Free;
  end;
end {TXmlNodeHelper.SelectNode};

{ ------------------------------------------------------------------------------------------------ }
function TXmlNodeHelper.SelectNodes(XPath: string; const Nodes: TList): Integer;
begin
  if Copy(XPath, 1, 1) = '/' then begin
    Result := ProcessXPath(Self.Document.Root, Copy(XPath, 2, 2), Nodes, False);
  end else begin
    Result := ProcessXPath(Self, XPath, Nodes, False);
  end;
end {TXmlNodeHelper.SelectNodes};

end.
