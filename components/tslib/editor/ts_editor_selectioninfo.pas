unit ts_Editor_SelectionInfo;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes,

  SynEdit, SynEditTypes;

type
  TSelectionInfo = class
  private
    FBlockBegin    : TPoint;
    FBlockEnd      : TPoint;
    FLines         : TStrings;
    FSelectionMode : TSynSelectionMode;
    FLockUpdates   : Boolean;
    FStripLastLine : Boolean;
    FCaretXY       : TPoint;
    FSynEdit       : TSynEdit;

    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetCaretXY: TPoint;
    function GetLines: TStrings;
    function GetSelectionMode: TSynSelectionMode;
    function GetText: string;
    procedure SetBlockBegin(AValue: TPoint);
    procedure SetBlockEnd(AValue: TPoint);
    procedure SetCaretXY(AValue: TPoint);
    procedure SetSelectionMode(AValue: TSynSelectionMode);
    procedure SetText(AValue: string);

  public
    constructor Create(ASynEdit: TSynEdit); reintroduce; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Clear;
    procedure Store(
      const ACaretXY               : TPoint;
      const ABlockBegin            : TPoint;
      const ABlockEnd              : TPoint;
      const AText                  : string;
            ASelectionMode         : TSynSelectionMode;
            ALockUpdates           : Boolean;
            AAutoExcludeEmptyLines : Boolean
    );
    procedure Restore;
    procedure Ignore;

    {
    FStoredBlockBegin       : TPoint;
    FStoredBlockEnd         : TPoint;
    FStoredBlockLines       : TStringList;
    FStoredStripLastLine    : Boolean;
    FStoredBlockLockUpdates : Boolean;
    FStoredSelectionMode    : TSynSelectionMode;
    }

    property BlockBegin: TPoint
      read GetBlockBegin write SetBlockBegin;

    property BlockEnd: TPoint
      read GetBlockEnd write SetBlockEnd;

    property CaretXY: TPoint
      read GetCaretXY write SetCaretXY;

    property SelectionMode: TSynSelectionMode
      read GetSelectionMode write SetSelectionMode;

    property Lines: TStrings
      read GetLines;

    property Text: string
      read GetText write SetText;

    property LockUpdates: Boolean
      read FLockUpdates write FLockUpdates;

    property StripLastLine: Boolean
      read FStripLastLine write FStripLastLine;

  end;

//*****************************************************************************

implementation

uses
  ts_Editor_Utils;

{$region 'construction and destruction' /fold}
//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

constructor TSelectionInfo.Create(ASynEdit: TSynEdit);
begin
  inherited Create;
  FSynEdit := ASynEdit;
end;

procedure TSelectionInfo.AfterConstruction;
begin
  inherited AfterConstruction;
  FLines := TStringList.Create;
end;

procedure TSelectionInfo.BeforeDestruction;
begin
  FLines.Free;
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************
{$endregion}

{$region 'property access mehods' /fold}
//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TSelectionInfo.GetBlockBegin: TPoint;
begin
  Result := FBlockBegin;
end;

procedure TSelectionInfo.SetBlockBegin(AValue: TPoint);
begin
  FBlockBegin := AValue;
end;

function TSelectionInfo.GetBlockEnd: TPoint;
begin
  Result := FBlockEnd;
end;

function TSelectionInfo.GetCaretXY: TPoint;
begin
  Result := FCaretXY;
end;

procedure TSelectionInfo.SetCaretXY(AValue: TPoint);
begin
  FCaretXY := AValue;
end;

function TSelectionInfo.GetLines: TStrings;
begin
  Result := FLines;
end;

procedure TSelectionInfo.SetBlockEnd(AValue: TPoint);
begin
  FBlockEnd := AValue;
end;

function TSelectionInfo.GetSelectionMode: TSynSelectionMode;
begin
  Result := FSelectionMode;
end;

function TSelectionInfo.GetText: string;
begin
  // The Text property of a stringlist always returns a line ending at the end
  // of the string which needs to be removed to avoid side effects.
  Result := StripLastLineEnding(FLines.Text);
end;

procedure TSelectionInfo.SetText(AValue: string);
begin
  FLines.Text := AValue;
end;

procedure TSelectionInfo.SetSelectionMode(AValue: TSynSelectionMode);
begin
  FSelectionMode := AValue;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************
{$endregion}

{$region 'public methods' /fold}
//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

procedure TSelectionInfo.Clear;
begin
  FLines.Clear;
  FBlockBegin.X  := 0;
  FBlockBegin.Y  := 0;
  FBlockEnd.X    := 0;
  FBlockEnd.Y    := 0;
  FSelectionMode := smNormal;
end;

procedure TSelectionInfo.Store(const ACaretXY: TPoint;
  const ABlockBegin: TPoint; const ABlockEnd: TPoint; const AText: string;
  ASelectionMode: TSynSelectionMode; ALockUpdates: Boolean;
  AAutoExcludeEmptyLines: Boolean);
begin
  FBlockBegin    := ABlockBegin;
  FBlockEnd      := ABlockEnd;
  FSelectionMode := ASelectionMode;
  FLines.Text    := AText;
  FLockUpdates   := ALockUpdates;
  if FLockUpdates then
    FSynEdit.BeginUpdate;

  if AAutoExcludeEmptyLines then
  begin
    // Are multiple lines selected and is the last line in selection empty?
    // => adjust selected block to exclude this line
    if (FBlockEnd.X = 1)
      and (FBlockEnd.Y > FBlockBegin.Y)
      and not (FSelectionMode in [smLine, smColumn]) then
    begin
      FBlockEnd.Y := FBlockEnd.Y - 1;
      FStripLastLine := True;
    end
    else
      FStripLastLine := False;
  end
  else
    FStripLastLine := True;
end;

procedure TSelectionInfo.Restore;
begin
  if StripLastLine then // adjust block selection bounds
  begin
    case SelectionMode of
    smNormal:
      begin
        FBlockEnd.X := Length(FLines[FLines.Count - 1]) + 2;
        FBlockEnd.Y := FBlockBegin.Y + FLines.Count - 1;
      end;
    smColumn:
      begin
        FBlockEnd.X := Length(FLines[FLines.Count - 1]) + 2;
        FBlockEnd.Y := FBlockBegin.Y + FLines.Count - 1;
      end;
    smLine:
      begin

      end;
    end;
  end;
  FSynEdit.SetTextBetweenPoints(
    BlockBegin,
    BlockEnd,
    Text,
    [setSelect],
    scamIgnore,
    smaKeep,
    SelectionMode
  );
  if FLockUpdates then
    FSynEdit.EndUpdate;
end;

procedure TSelectionInfo.Ignore;
begin
  if FLockUpdates then
    FSynEdit.EndUpdate;
  Clear;
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************
{$endregion}

end.

