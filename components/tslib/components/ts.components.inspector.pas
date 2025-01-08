{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

{
  The Original Code is part of 'Extended components library, Version 1.3.16'.
  The Initial Developer of the Original Code (Ex_Grid.pas) is Roman M. Mochalov
  (roman@tersy.ru). Portions created by the Initial Developer are Copyright
  (C) 1997-2007. All Rights Reserved. You may obtain a copy of the original code
  at http://www.tersy.ru/~roman/download/

  Changes by Tim Sinaeve:
   - Ported to FPC/Lazarus
   - OnGetCellColors was not triggered anymore
}

unit ts.Components.Inspector;

{$MODE DELPHI}

interface

uses
  Classes, Controls, Graphics, Forms, Math,

  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}

  LCLIntf, LCLType,

  ts.Components.GridView;

type

{ TInspectorEdit }

  TInspectorEdit = class(TGridEdit)
  protected
    procedure UpdateBounds(ScrollCaret: Boolean); override;
    procedure UpdateColors; override;

  public
    procedure Invalidate; override;

  end;

{ TCustomInspector }
  {
    Base class for creating the inspectors of properties on the means
    ObjectInspector in Delphi.
    Overlap the methods of class TCustomGridView for creating the exterior view
    (color of the text of columns, the "embedded" focus and t.p.). It always contains two
    column - column of the names of properties and the column of values. It is always located in
    the state of editing.
    Dopolnitel'nyem the methods:
    IsCategoryRow - is the line indicated line with the name
                    category. It is not sketched for the lines with the name of category
                    the separating strip of columns (as into Delphi 5.0),
                    text is sketched to entire width of table by color clPurple.
                    ATTENTION! The text of the line of category determines the text
                    the cell of left (index 0) column.
    Additional properties:
    NameFont -      type of the names properties.
    ValueFont -     type of values.
    CategoryFont  - type of text in the cells of category.
  }

  TInspectorCategoryRowEvent = procedure(
        Sender  : TObject;
        Row     : Longint;
    var Category: Boolean
  ) of object;

  TCustomInspector = class(TCustomGridView)
  private
    FNameFont: TFont;
    FValueFont: TFont;
    FCategoryFont: TFont;
    FHitTest: TPoint;
    FColUpdate: Integer;
    FOnGetCategoryRow: TInspectorCategoryRowEvent;
    procedure SetCategoryFont(Value: TFont);
    procedure SetNameFont(Value: TFont);
    procedure SetValueFont(Value: TFont);
    {$IFDEF WINDOWS}
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    {$ENDIF}
  protected
    procedure FontChanged(Sender: TObject); override;
    procedure ChangeColumns; override;
    function ColResizeAllowed(X, Y: Integer): Boolean;
    procedure ColumnResizing(Column: Integer; var Width: Integer); override;
    function EditCanShow(Cell: TGridCell): Boolean; override;
    procedure GetCellColors(Cell: TGridCell; Canvas: TCanvas); override;
    function GetCellHintRect(Cell: TGridCell): TRect; override;
    function GetCellText(Cell: TGridCell): string; override;
    function GetCellTextIndent(Cell: TGridCell): TPoint; override;
    function GetEditClass(Cell: TGridCell): TGridEditClass; override;
    procedure GetEditListBounds(Cell: TGridCell; var Rect: TRect); override;
    function GetTipsRect(Cell: TGridCell): TRect; override;
    procedure HideCursor; override;
    procedure HideFocus; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintCell(Cell: TGridCell; Rect: TRect); override;
    procedure PaintFocus; override;
    procedure Resize; override;
    procedure ShowCursor; override;
    procedure ShowFocus; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetColumnAt(X, Y: Integer): Integer; override;
    function GetEditRect(Cell: TGridCell): TRect; override;
    function GetFocusRect: TRect; override;
    function GetRowAt(X, Y: Integer): Integer; override;
    function IsCategoryRow(Row: Integer): Boolean; virtual;
    procedure UpdateColumnsSize; virtual;
    procedure UpdateScrollBars; override;

    property CategoryFont: TFont
      read FCategoryFont write SetCategoryFont;

    property NameFont: TFont
      read FNameFont write SetNameFont;

    property ValueFont: TFont
      read FValueFont write SetValueFont;

    property OnGetCategoryRow: TInspectorCategoryRowEvent
      read FOnGetCategoryRow write FOnGetCategoryRow;
  end;

{ TInspector }

  TExInspector = class(TCustomInspector)
  published
    property Align;
    property AllowEdit default True;
    property AlwaysEdit default True;
    property AlwaysSelected;
    property Anchors;
    property BorderStyle;
    property CursorKeys;
    property CategoryFont;
    property Color default clBtnFace;
    property Constraints;
    property ColumnsFullDrag default True;
    property DoubleBuffered default True;
    property Enabled;
    property EndEllipsis default False;
    property FlatBorder;
    property Font;
    property NameFont;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCellTips;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property ValueFont;
    property Visible;
    property OnCellClick;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnCellAcceptCursor;
    property OnEditAcceptKey;
    property OnEditCanModify;
    property OnEditButtonPress;
    property OnEditCanceled;
    property OnEditChange;
    property OnEditCloseUp;
    property OnEditSelectNext;
    property OnEnter;
    property OnExit;
    property OnGetCheckAlignment;
    property OnGetCheckImage;
    property OnGetCheckIndent;
    property OnGetCheckKind;
    property OnGetCheckState;    
    property OnGetCategoryRow;
    property OnGetCellText;
    property OnGetCellColors;
    property OnGetCellReadOnly;
    property OnGetEditList;
    property OnGetEditStyle;
    property OnGetEditText;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnKeyPress;
    property OnKeyUp;
    property OnResize;
    property OnSetEditText;
    property OnStartDrag;
  end;

implementation

uses
  Types;

{ TInspectorEdit }

procedure TInspectorEdit.UpdateBounds(ScrollCaret: Boolean);
begin
  inherited;
  ButtonWidth := Height;
end;

procedure TInspectorEdit.UpdateColors;
begin
  inherited;
  Color := clWindow;
  Font.Color := clBtnText;
end;

procedure TInspectorEdit.Invalidate;
begin
  inherited;
  if Grid <> nil then
    Grid.InvalidateFocus;
end;

{ TCustomInspector }

constructor TCustomInspector.Create(AOwner: TComponent);
begin
  inherited;
  {color}
  Color := clBtnFace;
  {editing}
  RowSelect := False;
  AllowEdit := True;
  AlwaysEdit := True;
  {column}
  with Columns do
  begin
    {the column of properties}
    with Add do
    begin
      Caption := 'Property';
      FixedSize := True;
      WordWrap := False;
      WantReturns := False;
      ReadOnly := True;
      TabStop := False;
    end;
    {the column of values}
    with Add do
    begin
      Caption := 'Value';
      WordWrap := False;
      WantReturns := False;
      FixedSize := True;
    end;
  end;
  {we synchronize and hide title}
  Header.Synchronized := True;
  ShowHeader := False;
  {the height of lines}
  Rows.AutoHeight := False;
  Rows.Height := 16;
  {the strip of warming up}
  HorzScrollBar.Visible := False;
  {exterior view}
  ColumnsFullDrag := True;
  DoubleBuffered := True;
  CheckBoxes := True;
  EndEllipsis := False;
  GridLines := False;
  TextTopIndent := 1;
  TextRightIndent := 1;
  {the key for the isolation of line}
  CursorKeys := CursorKeys + [gkMouseMove];
  {the color of text}
  FNameFont := TFont.Create;
  FNameFont.Assign(Font);
  FNameFont.Color := clBtnText;
  FNameFont.OnChange := FontChanged;
  FValueFont := TFont.Create;
  FValueFont.Assign(Font);
  FValueFont.Color := clNavy;
  FValueFont.OnChange := FontChanged;
  FCategoryFont := TFont.Create;
  FCategoryFont.Assign(Font);
  FCategoryFont.Color := clPurple;
  FCategoryFont.Style := FValueFont.Style + [fsBold];
  FCategoryFont.OnChange := FontChanged;
end;

destructor TCustomInspector.Destroy;
begin
  FCategoryFont.Free;
  FValueFont.Free;
  FNameFont.Free;
  inherited;
end;

procedure TCustomInspector.FontChanged(Sender: TObject);
begin
  inherited;
  InvalidateGrid;
end;

procedure TCustomInspector.SetCategoryFont(Value: TFont);
begin
  FCategoryFont.Assign(Value);
end;

procedure TCustomInspector.SetNameFont(Value: TFont);
begin
  FNameFont.Assign(Value);
end;

procedure TCustomInspector.SetValueFont(Value: TFont);
begin
  FValueFont.Assign(Value);
end;

{$IFDEF WINDOWS}
procedure TCustomInspector.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
//  FHitTest := ScreenToClient(SmallPointToPoint(Message.Pos));
end;

procedure TCustomInspector.WMSetCursor(var Message: TWMSetCursor);
begin
  with Message, FHitTest do
    if (HitTest = HTCLIENT) and not (csDesigning in ComponentState) then
      if ColResizeAllowed(X, Y) then
      begin
        Windows.SetCursor(Screen.Cursors[crHSplit]);
        Exit;
      end;
  inherited;
end;
{$ENDIF}

procedure TCustomInspector.ChangeColumns;
begin
  inherited;
  {we correct the sizes of columns}
  UpdateColumnsSize;
end;

function TCustomInspector.ColResizeAllowed(X, Y: Integer): Boolean;
begin
  Result := (Columns.Count > 0) and (X >= Columns[0].Width - 4) and (X <= Columns[0].Width);
end;

procedure TCustomInspector.ColumnResizing(Column: Integer; var Width: Integer);
begin
  if Width > ClientWidth - 35 then Width := ClientWidth - 35;
  if Width < 35 then Width := 35;
end;

function TCustomInspector.EditCanShow(Cell: TGridCell): Boolean;
begin
  {for the cells of category the line of introduction we do not show}
  Result := (not IsCategoryRow(Cell.Row)) and inherited EditCanShow(Cell);
end;

procedure TCustomInspector.GetCellColors(Cell: TGridCell; Canvas: TCanvas);
begin
  Canvas.Brush.Color := Self.Color;
  if IsCategoryRow(Cell.Row) then
    Canvas.Font := CategoryFont
  else if Cell.Col = 1 then
    Canvas.Font := ValueFont
  else if Cell.Col = 0 then
    Canvas.Font := NameFont
  else
    Canvas.Font := Self.Font;

  if Assigned(OnGetCellColors) then
    OnGetCellColors(Self, Cell, Canvas);
end;

function TCustomInspector.GetCellHintRect(Cell: TGridCell): TRect;
begin
  Result := inherited GetCellHintRect(Cell);
  {for the lines of categories the rectangle of prompt - entire line }
  if IsCategoryRow(Cell.Row) then
  begin
    Result.Left := GetColumnRect(0).Left;
    Result.Right := GetColumnRect(1).Right;
  end;
end;

function TCustomInspector.GetCellText(Cell: TGridCell): string;
begin
  {for the lines of categories the second column will be the same as the first}
  if (Cell.Col <> 0) and IsCategoryRow(Cell.Row) then Cell.Col := 0;
  Result := inherited GetCellText(Cell);
end;

function TCustomInspector.GetCellTextIndent(Cell: TGridCell): TPoint;
begin
  Result.X := 2 - Cell.Col;
  Result.Y := TextTopIndent;
end;

function TCustomInspector.GetEditClass(Cell: TGridCell): TGridEditClass;
begin
  Result := TInspectorEdit;
end;

procedure TCustomInspector.GetEditListBounds(Cell: TGridCell; var Rect: TRect);
begin
  {we consider vertical line}
  Dec(Rect.Left, 2);
  {working on silence}
  inherited;
end;

function TCustomInspector.GetTipsRect(Cell: TGridCell): TRect;
var
  DX: Integer;
begin
  Result := inherited GetTipsRect(Cell);
  {for 2 columns of the line of category we displace prompt on beginning 1 of column}
  if (Cell.Col <> 0) and IsCategoryRow(Cell.Row) then
  begin
    DX := GetColumnRect(0).Left - Result.Left;
    OffsetRect(Result, DX, 0);
  end;
end;

procedure TCustomInspector.HideCursor;
begin
  {for the cells of category the line of introduction does not show; therefore
    dissipated the framework of focus must be to very}
  if IsCategoryRow(CellFocused.Row) then
  begin
    InvalidateFocus;
    Exit;
  end;
  inherited;
end;

procedure TCustomInspector.HideFocus;
begin
  {on silence TCustomGridView sketches the rectangle of the focus by the function
    DrawFocusRect. In the consequence of the special feature of funktsiii DrawFocusRect,
    the broken rectangle of focus must be dissipated (to sketch repeatedly)
    everyone with the least copying, a change in the dimensions of column and t.p.,
    otherwise will remain "rubbish". Since in the inspector focus is sketched by the framework,
    that the procedure of the extinction of focus can be ignored}
end;

procedure TCustomInspector.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  {we establish focus on itself}
  if not AcquireFocus then
  begin
    MouseCapture := False;
    Exit;
  end;
  {left key}
  if Button = mbLeft then
  begin
    {the attempt to begin changes in the size of column}
    if ColResizeAllowed(X, Y) then
    begin
      {we begin a change in the size}
      StartColResize(Header.Sections[0], X, Y);
      {we do not release further}
      Exit;
    end;
  end;
  {processor for silence}
  inherited;
end;

procedure TCustomInspector.Paint;
begin
  inherited;
  {we sketch the framework around the focus}
  PaintFocus;
end;

procedure TCustomInspector.PaintCell(Cell: TGridCell; Rect: TRect);
var
  R: TRect;
begin
  {the cells of category we sketch to entire line}
  if IsCategoryRow(Cell.Row) then
  begin
    Rect.Left := GetGridRect.Left;
    Rect.Right := GetGridRect.Right;
    {the column of the values of the cells of category we do not sketch generally}
    if Cell.Col <> 0 then Exit;
  end;                                                     
  {we sketch cell}
  inherited;
  {for the cells without the focus we sketch the broken separating strip from below}
  if Cell.Row <> CellFocused.Row then
  begin
    R := Rect;
    R.Top := R.Bottom - 1;
    with Canvas do
    begin
      {the color of the points of line}
      Brush.Color := clGray xor clSilver;
      Font.Color := clBlack;
      Refresh;
      {line}
      PaintResizeRectDC(Handle, R);
    end;
  end;
  {for the cells without the name category we sketch the dual separating
    strip to the right}
  if (Cell.Col = 0) and (not IsCategoryRow(Cell.Row)) then
    with Canvas do
    begin
      Pen.Color := clBtnShadow;
      Pen.Width := 1;
      MoveTo(Rect.Right - 2, Rect.Top - 1);
      LineTo(Rect.Right - 2, Rect.Bottom);
      Pen.Color := clBtnHighlight;
      MoveTo(Rect.Right - 1, Rect.Bottom - 1);
      LineTo(Rect.Right - 1, Rect.Top - 1);
    end;
  {we sketch the framework of focus}
  if Cell.Row = CellFocused.Row then
    {the lower line of the framework only for the cells with the focus}
    with Canvas do
      DrawEdge(Handle, Rect, BDR_SUNKENOUTER, BF_BOTTOM)
  else
    { the upper line of the framework of focus only for the cells above the focus}
    if Cell.Row = CellFocused.Row - 1 then
    begin
      R := Rect;
      R.Top := R.Bottom - 2;
      with Canvas do
      begin
        DrawEdge(Handle, R, BDR_SUNKENOUTER, BF_TOP);
        InflateRect(R, 0, -1);
        DrawEdge(Handle, R, BDR_SUNKENINNER, BF_TOP);
      end;
    end;
end;

procedure TCustomInspector.PaintFocus;
begin
  {the framework focus it is sketched with otrisovke of cell}
end;

procedure TCustomInspector.Resize;
begin
  { we correct columns}
  UpdateColumnsSize;
  { working on silence}
  inherited;
end;


procedure TCustomInspector.ShowCursor;
begin
  if IsCategoryRow(CellFocused.Row) then
  begin
    InvalidateFocus;
    Exit;
  end;
  inherited;
end;

procedure TCustomInspector.ShowFocus;
begin
  {}
end;

function TCustomInspector.GetColumnAt(X, Y: Integer): Integer;
var
  C1, C2: TGridCell;
  R: TRect;
begin
  { we obtain the visible cells}
  C1 := GridCell(0, VisOrigin.Row);
  C2 := GridCell(1, VisOrigin.Row + VisSize.Row - 1);
  { we obtain the rectangle of the visible cells}
  R := GetCellsRect(C1, C2);
  {if point to the left of the cells - we return the first column}
  if X < R.Left then
  begin
    Result := C1.Col;
    Exit;
  end;
  {if point to the right of cells - we return posledyuyu column}
  if X >= R.Right then
  begin
    Result := C2.Col;
    Exit;
  end;
  {the search on silence}
  Result := inherited GetColumnAt(X, Y);
end;

function TCustomInspector.GetEditRect(Cell: TGridCell): TRect;
begin
  { we obtain the rectangle of line}
  Result := inherited GetEditRect(Cell);
  { we consider border}
  Dec(Result.Bottom, 1);
end;


function TCustomInspector.GetFocusRect: TRect;
begin
  { we obtain the rectangle of line, we consider border}
  Result := GetRowRect(CellFocused.Row);
  Dec(Result.Top, 2);
end;

function TCustomInspector.GetRowAt(X, Y: Integer): Integer;
var
  C1, C2: TGridCell;
  R: TRect;
begin
  { we obtain the visible cells}
  C1 := GridCell(0, VisOrigin.Row);
  C2 := GridCell(1, VisOrigin.Row + VisSize.Row - 1);
  { we obtain the rectangle of the visible cells}
  R := GetCellsRect(C1, C2);
  {if point to vserkhu from the cells - we return the first line}
  if Y < R.Top then
  begin
    Result := MaxIntValue([C1.Row - 1, 0]);
    Exit;
  end;
  {if point from below from the cells - we return posledyuyu to period}
  if Y >= R.Bottom then
  begin
    Result := MinIntValue([C2.Row + 1, Rows.Count - 1]);
    Exit;
  end;
  {the search on silence}
  Result := inherited GetRowAt(X, Y);
end;

function TCustomInspector.IsCategoryRow(Row: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnGetCategoryRow) then FOnGetCategoryRow(Self, Row, Result);
end;

procedure TCustomInspector.UpdateColumnsSize;
begin
  {but do not occur now changes}
  if (FColUpdate = 0) and (Columns.Count = 2) then
  begin
    Inc(FColUpdate);
    try
      {we correct the width of the column of values}
      Columns[1].Width := ClientWidth - Columns[0].Width;
      {column cannot be less than 35 pixels}
      if Columns[1].Width < 35 then
      begin
        Columns[1].Width := 35;
        Columns[0].Width := ClientWidth - 35;
      end;
      {we correct the width of the column of names}
      if Columns[0].Width < 35 then
      begin
        Columns[0].Width := 35;
        Columns[1].Width := ClientWidth - 35;
      end;
    finally
      Dec(FColUpdate);
    end;
  end;
end;

procedure TCustomInspector.UpdateScrollBars;
begin
  inherited;
  {when are dissipated scrollers it increases the visible part table, but
    Resize is not caused - it is necessary to touch up the sizes of columns}
  UpdateColumnsSize;
end;

end.
