unit ts_editor_commentstripper;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils;

type
  TSourceTokenKind = (
    skUndefined,
    skCode,
    skBlockComment,
    skLineComment,
    skQuoteString,
    skDittoString,
    skDirective,
    skTodoList,
    skToReserve
  );

  TStripOption = (coAll, coExAscii);

type
  TSourceStripper = class(TComponent)
  private
    FCurTokenKind: TSourceTokenKind;
    FCurChar: AnsiChar;

    FStripTodoList: Boolean;
    FStripDirective: Boolean;
    FStripOption: TStripOption;
    FInStream: TStream;
    FOutStream: TStream;
    FReserve: Boolean;
    FReserveItems: TStringList;
    procedure SetInStream(const Value: TStream);
    procedure SetOutStream(const Value: TStream);
    procedure SetReserveItems(const Value: TStringList);

  protected
    procedure DoParse; virtual; abstract;
    procedure ProcessToBlockEnd; virtual; abstract;

    function IsTodoList: Boolean;
    function IsReserved: Boolean;
    function IsBlank(AChar: AnsiChar): Boolean;

    function GetCurChar: AnsiChar;
    function NextChar(Value: Integer = 1): AnsiChar;
    procedure WriteChar(Value: AnsiChar);

    procedure ProcessToLineEnd;
    procedure DoDefaultProcess;
    procedure DoBlockEndProcess;

  public
    procedure Parse;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property InStream: TStream
      read FInStream write SetInStream;
    property OutStream: TStream
      read FOutStream write SetOutStream;
    property CropOption: TStripOption
      read FStripOption write FStripOption;
    property CropDirective: Boolean
      read FStripDirective write FStripDirective;
    property CropTodoList: Boolean
      read FStripTodoList write FStripTodoList;
    property Reserve: Boolean
      read FReserve write FReserve;
    property ReserveItems: TStringList
      read FReserveItems write SetReserveItems;

  end;

type
  TPasCommentStripper = class(TSourceStripper)
  protected
    procedure DoParse; override;
    procedure ProcessToBlockEnd; override;
    procedure ProcessToBracketBlockEnd;

  end;

type
  TCPPCommentStripper = class(TSourceStripper)
  protected
    procedure DoParse; override;
    procedure ProcessToBlockEnd; override;

  end;

implementation

{ TSourceStripper }

const
  SToDo = 'TODO';
  SToDoDone = 'DONE';

constructor TSourceStripper.Create(AOwner: TComponent);
begin
  inherited;
  FReserveItems := TStringList.Create;
end;

destructor TSourceStripper.Destroy;
begin
  FInStream := nil;
  FOutStream := nil;
  FReserveItems.Free;
  inherited;
end;

procedure TSourceStripper.DoBlockEndProcess;
begin
  case FCurTokenKind of
  skBlockComment:
    if (FStripOption = coExAscii) and (FCurChar < #128) then
      WriteChar(FCurChar);
  skDirective:
    if not CropDirective or
      ((FStripOption = coExAscii) and (FCurChar < #128)) then
      WriteChar(FCurChar);
  skTodoList:
     if not CropTodoList or
      ((FStripOption = coExAscii) and (FCurChar < #128)) then
      WriteChar(FCurChar);
  skToReserve:
    if FReserve then
      WriteChar(FCurChar);
  else
    DoDefaultProcess;
  end;
end;

procedure TSourceStripper.DoDefaultProcess;
begin
  if (FStripOption = coAll) or (FCurChar < #128) then
    WriteChar(FCurChar);
end;

function TSourceStripper.GetCurChar: AnsiChar;
begin
  Result := #0;
  if Assigned(FInStream) then
  begin
    try
      FInStream.Read(Result, SizeOf(AnsiChar));
    except
      Exit;
    end;
  end;
end;

function TSourceStripper.IsBlank(AChar: AnsiChar): Boolean;
begin
  Result := AChar in [' ', #13, #10, #7, #9];
end;

function TSourceStripper.IsReserved: Boolean;
var
  i: Integer;
  OldChar: AnsiChar;
  OldPos: Integer;
  MaxLen: Integer;
  PBuf: PChar;
  SToCompare: String;
begin
  Result := False;
  if FInStream = nil then Exit;

  PBuf := nil;
  OldChar := FCurChar;
  OldPos := FInStream.Position;

  MaxLen := 0;
  for i := Self.FReserveItems.Count - 1 downto 0 do
  begin
    if MaxLen < Length(Self.FReserveItems.Strings[i]) then
      MaxLen := Length(Self.FReserveItems.Strings[i]);
    if Self.FReserveItems.Strings[i] = '' then
      Self.FReserveItems.Delete(i);
  end;

  if (FCurChar = '/') or (FCurChar = '(') then
  begin
    FCurChar := GetCurChar;
    if FCurChar <> '*' then
      Exit;
  end;

  try
    PBuf := StrAlloc(MaxLen + 1);
    FillChar(PBuf^, Length(PBuf), 0);
    FInStream.Read(PBuf^, MaxLen);

    for i := 0 to Self.FReserveItems.Count - 1 do
    begin
      SToCompare := Copy(StrPas(PBuf), 1, Length(Self.FReserveItems.Strings[i]));
      if SToCompare = Self.FReserveItems.Strings[i] then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    FCurChar := OldChar;
    FInStream.Position := OldPos;
    if PBuf <> nil then
      StrDispose(PBuf);
  end;
end;

function TSourceStripper.IsTodoList: Boolean;
var
  OldPos: Integer;
  OldChar: AnsiChar;
  PTodo: PChar;
  STodo: String;
begin
  Result := False;
  if FInStream = nil then Exit;

  PTodo := nil;
  OldChar := FCurChar;
  OldPos := FInStream.Position;
  try
    if (FCurChar = '/') or (FCurChar = '(') then
    begin
      FCurChar := GetCurChar;
      if (FCurChar <> '*') and (FCurChar <> '/') then
        Exit;
    end;
    while IsBlank(NextChar) do
      FCurChar := GetCurChar;

    PTodo := StrAlloc(Length(SToDo) + 1);
    FillChar(PTodo^, Length(PTodo), 0);
    FInStream.Read(PTodo^, Length(SToDo));
    STodo := Copy(UpperCase(StrPas(PTodo)), 1, 4);

    if (STodo = SToDo) or (STodo = SToDoDone) then
    begin
      while IsBlank(NextChar) do
        FCurChar := GetCurChar;

      if NextChar = ':' then
      begin
        Result := True;
        Exit;
      end
    end;

  finally
    FCurChar := OldChar;
    FInStream.Position := OldPos;
    if PTodo <> nil then
      StrDispose(PTodo);
  end;
end;

function TSourceStripper.NextChar(Value: Integer): AnsiChar;
begin
  Result := #0;
  if Assigned(FInStream) then
  begin
    try
      FInStream.Seek(Value - 1, soFromCurrent);
      FInStream.Read(Result, SizeOf(AnsiChar));
      FInStream.Seek(-Value, soFromCurrent);
    except
      Exit;
    end;
  end;
end;

procedure TSourceStripper.Parse;
begin
  if (FInStream <> nil) and (FOutStream <> nil) then
  begin
    if (FInStream.Size > 0) then
    begin
      FInStream.Position := 0;
      FCurTokenKind := skUndefined;
      DoParse;
    end;
  end;
end;

procedure TSourceStripper.ProcessToLineEnd;
begin
  while not (FCurChar in [#0, #13]) do
  begin
    if ((FStripOption = coExAscii) and (FCurChar < #128))
      or (FCurTokenKind = skTodoList) then
        WriteChar(FCurChar);
    FCurChar := GetCurChar;
  end;

  if FCurChar = #13 then
    repeat
      WriteChar(FCurChar);
      FCurChar := GetCurChar;
    until FCurChar in [#0, #10];

  if FCurChar = #10 then
    WriteChar(FCurChar);

  FCurTokenKind := skUndefined;
end;

procedure TSourceStripper.SetInStream(const Value: TStream);
begin
  FInStream := Value;
end;

procedure TSourceStripper.SetOutStream(const Value: TStream);
begin
  FOutStream := Value;
end;

procedure TSourceStripper.SetReserveItems(const Value: TStringList);
begin
  if Value <> nil then
    FReserveItems.Assign(Value);
end;

procedure TSourceStripper.WriteChar(Value: AnsiChar);
begin
  if Assigned(FOutStream) then
  begin
    try
      OutStream.Write(Value, SizeOf(Value));
    except
      Exit;
    end;
  end;
end;

{ TCPPCommentStripper }

procedure TCPPCommentStripper.DoParse;
begin
  FCurChar := GetCurChar;
  while FCurChar <> #0 do
  begin
    case FCurChar of
    '/':
      begin
        if (FCurTokenKind in [skCode, skUndefined]) and (NextChar = '/') then
        begin
          if IsTodoList then
            FCurTokenKind := skTodoList
          else
            FCurTokenKind := skLineComment;
          ProcessToLineEnd;
        end
        else
        if (FCurTokenKind in [skCode, skUndefined]) and (NextChar = '*') then
        begin
          if IsTodoList then
            FCurTokenKind := skTodoList
          else if FReserve and IsReserved then
            FCurTokenKind := skToReserve
          else
            FCurTokenKind := skBlockComment;
          ProcessToBlockEnd;
        end
        else
          DoDefaultProcess;
      end;
    '''':
      begin
        if FCurTokenKind in [skCode, skUndefined] then
          FCurTokenKind := skQuoteString
        else if FCurTokenKind = skQuoteString then
           FCurTokenKind := skCode;

        DoDefaultProcess;
      end;
    '"':
      begin
        if FCurTokenKind in [skCode, skUndefined] then
          FCurTokenKind := skDittoString
        else if FCurTokenKind = skDittoString then
           FCurTokenKind := skCode;

        DoDefaultProcess;
      end;
    else
      DoDefaultProcess;
    end;

    FCurChar := GetCurChar;
  end;
end;

procedure TCPPCommentStripper.ProcessToBlockEnd;
begin
  while ((FCurChar <> '*') or (NextChar <> '/')) and (FCurChar <> #0) do
  begin
    DoBlockEndProcess;
    FCurChar := GetCurChar;
  end;

  if FCurChar = '*' then
  begin
    DoBlockEndProcess;
    FCurChar := GetCurChar;
    DoBlockEndProcess;
  end;

  FCurTokenKind := skUndefined;
end;

{ TPasCommentStripper }

procedure TPasCommentStripper.DoParse;
begin
  FCurChar := GetCurChar;
  while FCurChar <> #0 do
  begin
    case FCurChar of
    '/':
      begin
        if (FCurTokenKind in [skCode, skUndefined]) and (NextChar = '/') then
        begin
          if IsTodoList then
            FCurTokenKind := skTodoList
          else
            FCurTokenKind := skLineComment;
          ProcessToLineEnd;
        end
        else
          DoDefaultProcess;
      end;
    '{':
      begin
        if FCurTokenKind in [skCode, skUndefined] then
        begin
          if NextChar <> '$' then
          begin
            if IsTodoList then
              FCurTokenKind := skTodoList
            else if FReserve and IsReserved then
              FCurTokenKind := skToReserve
            else
              FCurTokenKind := skBlockComment
          end
          else
            FCurTokenKind := skDirective;
          ProcessToBlockEnd;
        end
        else
          DoDefaultProcess;
      end;
    '(':
      begin
        if (FCurTokenKind in [skCode, skUndefined]) and (NextChar = '*') then
        begin
          if IsTodoList then
            FCurTokenKind := skTodoList
          else if NextChar(2) = '$' then
            FCurTokenKind := skDirective
          else
            FCurTokenKind := skBlockComment;
          ProcessToBracketBlockEnd;
        end
        else
          DoDefaultProcess;
      end;
    '''':
      begin
        if FCurTokenKind in [skCode, skUndefined] then
          FCurTokenKind := skQuoteString
        else if FCurTokenKind = skQuoteString then
           FCurTokenKind := skCode;

        DoDefaultProcess;
      end;
    else
      DoDefaultProcess;
    end;

    FCurChar := GetCurChar;
  end;
end;

procedure TPasCommentStripper.ProcessToBlockEnd;
begin
  while not (FCurChar in [#0, '}']) do
  begin
    DoBlockEndProcess;
    FCurChar := GetCurChar;
  end;

  DoBlockEndProcess;
  FCurTokenKind := skUndefined;
end;

procedure TPasCommentStripper.ProcessToBracketBlockEnd;
begin
  while ((FCurChar <> '*') or (NextChar <> ')')) and (FCurChar <> #0) do
  begin
    DoBlockEndProcess;
    FCurChar := GetCurChar;
  end;

  if FCurChar = '*' then
  begin
    DoBlockEndProcess;
    FCurChar := GetCurChar;
    DoBlockEndProcess;
  end;

  FCurTokenKind := skUndefined;
end;

end.
