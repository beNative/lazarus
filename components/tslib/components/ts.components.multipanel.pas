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

unit ts.Components.MultiPanel;

// original name: OMultipanel

{$MODE DELPHI}

{
    OMultiPanel

    Version 1.1

    Copyright (C) 2012 Ondrej Pokorny
      http://www.kluug.net


  *** BEGIN LICENSE BLOCK *****

  OMultiPanel is licensed under MPL 1.1/GPL 3/LGPL 3 tri-license.

  MPL: http://www.mozilla.org/MPL/1.1/
  GPL: http://www.gnu.org/licenses/gpl-3.0.html
  LGPL: http://www.gnu.org/licenses/lgpl-3.0.de.html

  ***** END LICENSE BLOCK *****

}

interface

{$IFNDEF FPC}
  {$IF CompilerVersion < 18.0}
    {$DEFINE OMP_DELPHI7_DOWN}
  {$IFEND}
  {$IF CompilerVersion >= 15}
    {$DEFINE OMP_DELPHI7_UP}
  {$IFEND}
  {$IF CompilerVersion < 15}
    {$DEFINE OMP_DELPHI6_DOWN}
  {$IFEND}
  {$IF CompilerVersion >= 18.0}
    {$DEFINE OMP_DELPHI2006_UP}
  {$IFEND}
{$ENDIF}

{$IFNDEF OMP_DELPHI6_DOWN}
  {$DEFINE OMP_THEMES}
{$ENDIF}


uses
  {$IFNDEF FPC}Windows, {$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Dialogs, ExtCtrls, Types, Registry, IniFiles, Contnrs
  {$IFNDEF FPC}, StdCtrls{$ENDIF}
  {$IFDEF FPC}, lmessages, lcltype, lclintf{$ENDIF}
  ;

type

  TPanelType = (ptHorizontal, ptVertical);

  TOCustomMultiPanel = class;

  TOnPaintSizingBar = procedure(Sender: TOCustomMultiPanel; aCanvas: TCanvas; aBarRect: TRect; aHover: Boolean) of Object;
  TOnPaintSizingBarMethod = procedure(Sender: TOCustomMultiPanel; aCanvas: TCanvas; aBarRect: TRect; aHover: Boolean);

  TSplitterListItem = class(TObject)
  private
    fPxPosition: Integer;
    fPanelIndex: Integer;
  public
    constructor CreateAdd(aAddToList: TList);
  public
    property PxPosition: Integer read fPxPosition;
    property PanelIndex: Integer read fPanelIndex;
  end;

  TOMultiPanelCollection = class;
  TOMultiPanelItem = class(TCollectionItem)
  private
    fPosition: Double;
    fControl: TControl;
    fVisible: Boolean;
    procedure SetControl(const Value: TControl);
    procedure SetPosition(const Value: Double);
    procedure SetPositionOnlyWithCheck(const Value: Double);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure SetIndex(Value: Integer); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure CheckPosition(var Value: Double); virtual;
  public
    constructor Create(ACollection: TCollection); override;
  public
    function MPOwner: TOCustomMultiPanel;
    function MPCollection: TOMultiPanelCollection;
  published
    property Control: TControl read fControl write SetControl;
    property Position: Double read fPosition write SetPosition;
    property Visible: Boolean read fVisible write SetVisible;

    property Index;
  end;

  TOMultiPanelCollection = class(TOwnedCollection)
  protected
    function GetAttrCount: Integer; override;
    function GetItem(Index: Integer): TOMultiPanelItem;
    procedure SetItem(Index: Integer; Value: TOMultiPanelItem);
    procedure Update(Item: TCollectionItem); override;

    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    constructor Create(AOwner: TPersistent);
  public
    function MPOwner: TOCustomMultiPanel;

    function Add: TOMultiPanelItem;

    procedure AddControl(AControl: TControl; AIndex: Integer = -1);
    procedure RemoveControl(AControl: TControl);
    function IndexOf(AControl: TControl): Integer;

    procedure SetDefaultPositions;

    property Items[Index: Integer]: TOMultiPanelItem read GetItem write SetItem; default;
  end;

  {$IFNDEF FPC}
  TConstraintSize = Integer;
  {$ENDIF}

  TOCustomMultiPanel = class(TCustomPanel)
  private
    fOnSplitterMoved: TNotifyEvent;
    fOnPaintSizingBar: TOnPaintSizingBar;
    
    fSplitterColor: TColor;
    fSplitterHoverColor: TColor;

    fHover: Boolean;
    fHoverIndex: Integer;
    fSizing: Boolean;
    fSizingIndex: Integer;

    fLastSizingLinePx: Integer;
    fPrevBrush: HBrush;
    {$IFDEF FPC}
    fSplitterWindow: HWND;
    {$ELSE}
    fHitTest: TPoint;
    fLineDC: HDC;
    fBrush: TBrush;
    {$ENDIF}
    fInResizeControls: Boolean;
    fDoubleBufferedBitmap: TBitmap;

    fMinPosition: Double;
    fSplitterSize: Integer;

    fPanelCollection: TOMultiPanelCollection;
    fSplittersList: TObjectList;
    fPanelType: TPanelType;

    procedure SetPanelType(const Value: TPanelType);

    procedure SetMinPosition(const Value: Double);
    procedure SetSplitterSize(const Value: Integer);

    procedure AllocateLineDC;
    procedure ReleaseLineDC;

    procedure DrawSizingLine({%H-}X, {%H-}Y: Integer);
    {$IFNDEF FPC}
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    {$ENDIF}

    procedure SetPanelCollection(const Value: TOMultiPanelCollection);
    function GetSplitterColor: TColor;
    procedure SetSplitterColor(const Value: TColor);
    function GetSplitterHoverColor: TColor;
    procedure SetSplitterHoverColor(const Value: TColor);
    function GetSplitterPosition(Index: Integer): Integer;
    function MinPositionStored: Boolean;
  {$IFDEF FPC}
  protected
  {$ELSE}
  private
  {$ENDIF}
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
  protected

    procedure CalcSizing(X, Y: Integer; var aSizing: Boolean; var aSizingIndex: Integer);
    function GetSizingRect(SplitterIndex: Integer): TRect;

    procedure Paint; override;
    procedure DoPaintSizingBar(aCanvas: TCanvas; aIndex: Integer; aRect: TRect); virtual;
    procedure InvalidateSplitter(aIndex: Integer);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    procedure SetParent(AParent: TWinControl); override;
    procedure AlignControls({%H-}AControl: TControl; var {%H-}Rect: TRect); override;
    procedure Loaded; override;
    procedure Resize; override;

    procedure ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight: TConstraintSize); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure InvalidateSplitters;
    procedure ResizeControls; virtual;

    procedure LoadPositionsFromRegistry(aReg: TRegIniFile); overload;
    procedure SavePositionsToRegistry(aReg: TRegIniFile); overload;
    procedure LoadPositionsFromRegistry(aReg: TRegIniFile; const aSection, aIdent: String); overload; virtual;
    procedure SavePositionsToRegistry(aReg: TRegIniFile; const aSection, aIdent: String); overload; virtual;

    procedure LoadPositionsFromIniFile(aIni: TCustomIniFile); overload;
    procedure SavePositionsToIniFile(aIni: TCustomIniFile); overload;
    procedure LoadPositionsFromIniFile(aIni: TCustomIniFile; const aSection, aIdent: String); overload; virtual;
    procedure SavePositionsToIniFile(aIni: TCustomIniFile; const aSection, aIdent: String); overload; virtual;

    procedure DoSplitterMoved;

    function FindPanel(aControl: TControl): TOMultiPanelItem;

    property SplitterPosition[Index: Integer]: Integer read GetSplitterPosition;

    property PanelCollection: TOMultiPanelCollection read fPanelCollection write SetPanelCollection;
    property PanelType: TPanelType read fPanelType write SetPanelType default ptHorizontal;
    property MinPosition: Double read fMinPosition write SetMinPosition stored MinPositionStored;
    property SplitterSize: Integer read fSplitterSize write SetSplitterSize default 3;
    property SplitterColor: TColor read GetSplitterColor write SetSplitterColor default clNone;
    property SplitterHoverColor: TColor read GetSplitterHoverColor write SetSplitterHoverColor default clNone;

    property OnSplitterMoved: TNotifyEvent read fOnSplitterMoved write fOnSplitterMoved;
    property OnPaintSizingBar: TOnPaintSizingBar read fOnPaintSizingBar write fOnPaintSizingBar;

    property BevelOuter default bvNone;
    property BevelInner default bvNone;
    property ParentColor default True;
  end;

  TOMultiPanel = class(TOCustomMultiPanel)
  published
    property PanelType;
    property PanelCollection;
    property MinPosition;
    property SplitterSize;
    property SplitterColor;
    property SplitterHoverColor;

    property OnSplitterMoved;
    property OnPaintSizingBar;

    property Align;
    property Anchors;
    property AutoSize;
    {$IFNDEF FPC}
    property BevelEdges;
    property BevelKind;
    property Ctl3D;
    property ParentCtl3D;
    {$IFDEF OMP_DELPHI7_UP}
    property ParentBackground;
    {$ENDIF}
    {$IFDEF OMP_DELPHI2006_UP}
    property OnAlignInsertBefore;
    property OnAlignPosition;
    {$ENDIF}
    property OnCanResize;
    {$IFDEF OMP_DELPHI2006_UP}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    {$ENDIF}
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property Constraints;
    property Color;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    //property Padding;//does not have any effect
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

//DEFAULTS
var
  OMP_SplitterColor: TColor = clNone;
  OMP_SplitterHoverColor: TColor = clNone;
  OMP_SplitterSize: Integer = 3;
  OMP_OnPaintSizingBar: TOnPaintSizingBarMethod = nil;

implementation

uses Math
  {$IFDEF OMP_THEMES}, Themes{$ENDIF}
  {$IFNDEF FPC}, ShellAPI{$ENDIF};

{ TOCustomMultiPanel }

procedure TOCustomMultiPanel.AllocateLineDC;
begin
{$IFDEF FPC}
  fPrevBrush := GetStockObject(BLACK_BRUSH);//ThemeServices.DottedBrush;
  fSplitterWindow := CreateRubberband(Rect(0, 0, 0, 0), fPrevBrush);
{$ELSE}
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
  //if ResizeStyle = rsPattern then
  begin
    if FBrush = nil then
    begin
      FBrush := TBrush.Create;

      FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
    end;
    fPrevBrush := SelectObject(FLineDC, FBrush.Handle);
  end;
{$ENDIF}
end;

procedure TOCustomMultiPanel.CalcSizing(X, Y: Integer; var aSizing: Boolean;
  var aSizingIndex: Integer);

  function NextTo(Pos1, Pos2: Double): Boolean;
  begin
    result := (Abs(Pos1 - Pos2) < SplitterSize);
  end;

var
  I: Integer;
begin
  aSizing := False;
  aSizingIndex := -1;

  if csDesigning in ComponentState then
    exit;

  for I := 0 to fSplittersList.Count-1 do
  if PtInRect(GetSizingRect(I), Point(X, Y)) then begin
    aSizing := true;
    aSizingIndex := TSplitterListItem(fSplittersList[I]).PanelIndex;
    break;
  end;

  {for I := 0 to fPanelCollection.Count-2 do
  if (fPanelType = ptHorizontal) and
   NextTo(TOMultiPanelItem(fPanelCollection.Items[I]).Position*ClientWidth, X) then begin
    aSizing := true;
    aSizingIndex := I;
    break;
  end else
  if (fPanelType = ptVertical) and
   NextTo(TOMultiPanelItem(fPanelCollection.Items[I]).Position*ClientHeight, Y) then begin
    aSizing := true;
    aSizingIndex := I;
    break;
  end;}
end;

procedure TOCustomMultiPanel.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
  if not (csLoading in ComponentState) then
    if Message.Inserting and (Message.Control.Parent = Self) then
    begin
      DisableAlign;
      try
        Message.Control.Anchors := [];
        fPanelCollection.AddControl(Message.Control);
      finally
        EnableAlign;
      end;
    end else
      fPanelCollection.RemoveControl(Message.Control);
end;

procedure TOCustomMultiPanel.CMMouseEnter(var Message: TMessage);
{$IF DEFINED(OMP_DELPHI7_DOWN) OR DEFINED(FPC)}
var
  xP: TPoint;
{$IFEND}
begin
  inherited;

  {$IF DEFINED(OMP_DELPHI7_DOWN) OR DEFINED(FPC)}
  //DELPHI 7
  xP := ScreenToClient(Mouse.CursorPos);
  MouseMove([], xP.X, xP.Y);
  {$IFEND}
end;

procedure TOCustomMultiPanel.CMMouseLeave(var Message: TMessage);
var
  xHover: Boolean;
  {$IFDEF FPC}
  xP: TPoint;
  {$ENDIF}
begin
  xHover := fHover;
  fHover := False;

  inherited;

  if (xHover <> fHover) and (fHoverIndex >= 0) then
    InvalidateSplitter(fHoverIndex);

  {$IFDEF FPC}
  xP := ScreenToClient(Mouse.CursorPos);
  MouseMove([], xP.X, xP.Y);
  {$ENDIF}
end;

type
  TMyControl = class(TControl)
  public
    //procedure ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer); override;
  end;

procedure TOCustomMultiPanel.ConstrainedResize(var MinWidth, MinHeight, MaxWidth,
  MaxHeight: TConstraintSize);
  function GetControlsConstrainedHeight: Integer;
  var nW, nH, xW, xH: TConstraintSize;
    I: Integer;
  begin
    Result := 0;
    nW := 0; xW := 0; xH := 0;
    for I := 0 to ControlCount-1 do begin
      nH := Controls[I].Constraints.MinHeight;
      TMyControl(Controls[I]).ConstrainedResize(nW, nH, xW, xH);
      Inc(Result, nH);
    end;
  end;
begin
  inherited ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);

  if fPanelType = ptHorizontal then begin
    MinWidth := Max(MinWidth, fPanelCollection.Count*(fSplitterSize*2) + GetControlsConstrainedHeight);
  end else begin
    MinHeight := Max(MinHeight, fPanelCollection.Count*(fSplitterSize*2) + GetControlsConstrainedHeight);
  end;
end;

constructor TOCustomMultiPanel.Create(AOwner: TComponent);
begin
  fPanelCollection := TOMultiPanelCollection.Create(Self);
  fSplittersList := TObjectList.Create(True);
  fDoubleBufferedBitmap := TBitmap.Create;

  inherited;

  Caption := '';
  BevelOuter := bvNone;
  BevelInner := bvNone;
  ParentColor := True;

  fHover := True;
  fHoverIndex := -1;
  fSizing := False;
  fSizingIndex := -1;

  fPanelType := ptHorizontal;
  fMinPosition := 0.02;
  fOnSplitterMoved := nil;

  fSplitterColor := OMP_SplitterColor;
  fSplitterHoverColor := OMP_SplitterHoverColor;
  fSplitterSize := OMP_SplitterSize;

  {with PanelCollection.Add do begin
    Control := nil;
    Position := 0.5;
  end;
  with PanelCollection.Add do begin
    Control := nil;
    Position := 1;
  end;}
end;

destructor TOCustomMultiPanel.Destroy;
begin
  fPanelCollection.Free;
  fSplittersList.Free;
  fDoubleBufferedBitmap.Free;

  inherited;
end;

procedure TOCustomMultiPanel.DoPaintSizingBar(aCanvas: TCanvas; aIndex: Integer; aRect: TRect);
var
  xColor: TColor;
begin
  if Assigned(fOnPaintSizingBar) then begin
    fOnPaintSizingBar(Self, aCanvas, aRect, fHover and (fHoverIndex = aIndex));
  end else if Assigned(OMP_OnPaintSizingBar) then begin
    OMP_OnPaintSizingBar(Self, aCanvas, aRect, fHover and (fHoverIndex = aIndex));
  end else begin
    aCanvas.Brush.Style := bsSolid;
    if fHover and (fHoverIndex = aIndex) then
      xColor := SplitterHoverColor
    else
      xColor := SplitterColor;
    if xColor <> clNone then begin
      aCanvas.Brush.Color := xColor;
      aCanvas.FillRect(aRect);
    end;
  end;
end;

procedure TOCustomMultiPanel.DoSplitterMoved;
begin
  if Assigned(fOnSplitterMoved) then
    fOnSplitterMoved(Self);
end;

procedure TOCustomMultiPanel.DrawSizingLine(X, Y: Integer);
var
  xRect: TRect;
begin
  if fSizing and (fPanelType = ptHorizontal) then begin
    xRect := Rect(Left + X-fSplitterSize div 2, Top, fSplitterSize, Height);
    fLastSizingLinePx := X;
  end else if fSizing and (fPanelType = ptVertical) then begin
    xRect := Rect(Left, Top + Y-fSplitterSize div 2, Width, fSplitterSize);
    fLastSizingLinePx := Y;
  end;
  {$IFDEF FPC}
  xRect.TopLeft := Parent.ClientToScreen(xRect.TopLeft);
  xRect.Right:=xRect.Left+xRect.Right;
  xRect.Bottom:=xRect.Top+xRect.Bottom;
  SetRubberBandRect(fSplitterWindow, xRect);
  {$ELSE}
  PatBlt(FLineDC, xRect.Left, xRect.Top, xRect.Right, xRect.Bottom, PATINVERT);
  {$ENDIF}
end;

function TOCustomMultiPanel.FindPanel(aControl: TControl): TOMultiPanelItem;
var
  I: Integer;
begin
  for I := 0 to fPanelCollection.Count-1 do
  if fPanelCollection[I].Control = aControl then begin
    Result := fPanelCollection[I];
    exit;
  end;
  Result := nil;
end;

function TOCustomMultiPanel.GetSplitterHoverColor: TColor;
begin
  Result := fSplitterHoverColor;
end;

function TOCustomMultiPanel.GetSizingRect(SplitterIndex: Integer): TRect;
var Px: Integer;
begin
  Px := TSplitterListItem(fSplittersList[SplitterIndex]).PxPosition;
  if fPanelType = ptHorizontal then
    Result := Rect(Px, 0, Px+fSplitterSize, ClientHeight)
  else
    Result := Rect(0, Px, ClientWidth, Px+fSplitterSize);
end;

function TOCustomMultiPanel.GetSplitterColor: TColor;
begin
  Result := fSplitterColor;
end;

function TOCustomMultiPanel.GetSplitterPosition(Index: Integer): Integer;
begin
  if Index < fSplittersList.Count then begin
    Result := TSplitterListItem(fSplittersList[Index]).PxPosition;
  end else begin
    Result := 0;
  end;
end;

{$IFNDEF FPC}
type
  TAccessWinControl = class(TWinControl);
{$ENDIF}

procedure TOCustomMultiPanel.InvalidateSplitter(aIndex: Integer);
  procedure InvalidateWithParent(bControl: TWinControl; bRect: TRect);
  begin
    if not bControl.HandleAllocated then
      exit;

    if {$IF NOT DEFINED(FPC) AND NOT DEFINED(OMP_DELPHI6_DOWN)}TAccessWinControl(bControl).ParentBackground and{$IFEND}
       (bControl.Parent <> nil)
    then begin
      OffsetRect(bRect, bControl.Left, bControl.Top);
      InvalidateWithParent(bControl.Parent, bRect);
      OffsetRect(bRect, -bControl.Left, -bControl.Top);
    end;
    {$IFDEF MSWINDOWS}
    InvalidateRect(bControl.Handle, @bRect, True);
    {$ELSE}
    bControl.Invalidate;
    {$ENDIF}
  end;
var xR: TRect;
begin
  if aIndex < 0 then
    exit;

  {$WARNINGS OFF}
  if HandleAllocated then begin
    xR := GetSizingRect(aIndex);
    {$IFDEF OMP_THEMES}
    if ThemeServices.ThemesEnabled then
    {$ENDIF}
    begin
      //Repaint parent under splitter if splitter is hidden (clNone etc)!!!
      InvalidateWithParent(Self, xR);
    {$IFDEF OMP_THEMES}
    end else begin
      {$IFDEF MSWINDOWS}
      InvalidateRect(Handle, @xR, False);
      {$ELSE}
      Self.Invalidate;
      {$ENDIF}
    {$ENDIF}
    end;
  end;
  {$WARNINGS ON}
end;

procedure TOCustomMultiPanel.InvalidateSplitters;
var
  I: Integer;
begin
  for I := 0 to fSplittersList.Count-1 do
    InvalidateSplitter(I);
end;

procedure TOCustomMultiPanel.Loaded;
begin
  inherited;

  ResizeControls;
end;

procedure TOCustomMultiPanel.LoadPositionsFromIniFile(aIni: TCustomIniFile;
  const aSection, aIdent: String);
var I: Integer;
begin
  for I := 0 to PanelCollection.Count-2 do
  with TOMultiPanelItem(PanelCollection.Items[I]) do
    Position := StrToFloatDef(aIni.ReadString(aSection, aIdent+'.Position'+IntToStr(I), ''), Position);
end;

procedure TOCustomMultiPanel.LoadPositionsFromIniFile(aIni: TCustomIniFile);
begin
  LoadPositionsFromIniFile(aIni, Self.Owner.ClassName, Self.Name);
end;

procedure TOCustomMultiPanel.LoadPositionsFromRegistry(aReg: TRegIniFile;
  const aSection, aIdent: String);
var I: Integer;
begin
  for I := 0 to PanelCollection.Count-2 do
  with TOMultiPanelItem(PanelCollection.Items[I]) do
    Position := StrToFloatDef(aReg.ReadString(aSection, aIdent+'.Position'+IntToStr(I), ''), Position);
end;

procedure TOCustomMultiPanel.LoadPositionsFromRegistry(aReg: TRegIniFile);
begin
  LoadPositionsFromRegistry(aReg, Self.Owner.ClassName, Self.Name);
end;

function TOCustomMultiPanel.MinPositionStored: Boolean;
begin
  Result := MinPosition <> 0.02;
end;

procedure TOCustomMultiPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and not (ssDouble in Shift) then begin
    CalcSizing(X, Y, fSizing, fSizingIndex);

    if fSizing then begin
      AllocateLineDC;
    end;
    DrawSizingLine(X, Y);
  end else
    inherited;
end;

procedure TOCustomMultiPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Pos: Double;
  xHover: Boolean;
  xHoverIndex: Integer;
begin
  inherited;

  if fSizing and (fSizingIndex >= 0) and (ClientWidth > 0) and (ClientHeight > 0) then begin
    DrawSizingLine(fLastSizingLinePx, fLastSizingLinePx);

    if PanelType = ptHorizontal then
      Pos := X/ClientWidth
    else
      Pos := Y/ClientHeight;

    TOMultiPanelItem(PanelCollection.Items[fSizingIndex]).CheckPosition(Pos);

    if PanelType = ptHorizontal then
      Pos := Pos*ClientWidth
    else
      Pos := Pos*ClientHeight;

    DrawSizingLine(Round(Pos), Round(Pos));
  end else begin
    CalcSizing(X, Y, {%H-}xHover, {%H-}xHoverIndex);
    if (xHoverIndex <> fHoverIndex) or (xHover <> fHover) then begin
      fHover := xHover;
      if fHoverIndex <> xHoverIndex then
        InvalidateSplitter(fHoverIndex);
      fHoverIndex := xHoverIndex;
      InvalidateSplitter(fHoverIndex);
    end;
    {$IFDEF FPC}
    if fSizing or xHover then begin
      if (fPanelType = ptHorizontal) then
        Cursor := crSizeWE
      else if (fPanelType = ptVertical) then
        Cursor := crSizeNS;
    end else begin
      Cursor := crDefault;
    end;
    {$ENDIF}
  end;
end;

procedure TOCustomMultiPanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  try
    if (fSizing) and (fSizingIndex >= 0) and (ClientWidth > 0) and (ClientHeight > 0) then begin
      Dec(X, fSplitterSize div 2);
      Dec(Y, fSplitterSize div 2);

      DrawSizingLine(fLastSizingLinePx, fLastSizingLinePx);
      {$IFDEF MSWINDOWS}
      SendMessage(Handle, WM_SetRedraw, 0, 0);
      {$ENDIF}
      try

        if PanelType = ptHorizontal then
          TOMultiPanelItem(PanelCollection.Items[fSizingIndex]).Position := X/ClientWidth
        else
          TOMultiPanelItem(PanelCollection.Items[fSizingIndex]).Position := Y/ClientHeight;
      finally
        ReleaseLineDC;
        {$IFDEF MSWINDOWS}
        SendMessage(Handle, WM_SetRedraw, 1, 0);
        //RecreateWnd; //MUST BE HERE - BUG WORKAROUND (if width/height changed, it won't change!
        RedrawWindow(Handle, nil, 0,
          RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
        {$ELSE}
        Self.Invalidate;
        {$ENDIF}
      end;
    end;
  finally
    fSizing := False;
    CalcSizing(X, Y, fHover, fHoverIndex);
    InvalidateSplitter(fHoverIndex);
  end;
end;

procedure TOCustomMultiPanel.Paint;
var
  I: Integer;
  xR: TRect;
  xC: TCanvas;
begin
  {$WARNINGS OFF}
  if not Assigned(fDoubleBufferedBitmap) then begin
    inherited;
    exit;
  end;

  {$IFDEF OMP_THEMES}
  if not ThemeServices.ThemesEnabled then
  {$ENDIF}
  begin
    {$IFDEF OMP_DELPHI2006_UP}
    //DELPHI 2007+
    fDoubleBufferedBitmap.SetSize(Width, Height);
    {$ELSE}
    //DELPHI 7
    fDoubleBufferedBitmap.Width := Width;
    fDoubleBufferedBitmap.Height := Height;
    {$ENDIF}

    xC := fDoubleBufferedBitmap.Canvas;
    if Color = clDefault then
      xC.Brush.Color := clBtnFace
    else
      xC.Brush.Color := Color;
    xC.FillRect(Rect(0, 0, Width, Height));
  {$IFDEF OMP_THEMES}
  end else begin
    xC := Canvas;
  {$ENDIF}
  end;

  xC.Lock;

  if (csDesigning in ComponentState) and (fPanelCollection.Count = 0) then begin
    xC.Pen.Color := clBlue;
    xC.Brush.Style := bsClear;
    xC.Rectangle(0, 0, ClientWidth, ClientHeight);
    xC.MoveTo(0, 0);
    xC.LineTo(ClientWidth, ClientHeight);
    xC.MoveTo(ClientWidth, 0);
    xC.LineTo(0, ClientHeight);
    if Name <> '' then begin
      xR := Rect(0, 0, xC.TextWidth(Name) + 4, xC.TextHeight(Name) + 2);
      OffsetRect(xR, (Width - xR.Right) div 2, (Height - xR.Bottom) div 2);
      xC.Brush.Color := clWindow;
      xC.Font.Color := clWindowText;
      xC.Rectangle(xR);
      xC.TextOut(xR.Left+2, xR.Top+1, Name);
    end;
  end;

  for I := 0 to fSplittersList.Count-1 do begin
    xR := GetSizingRect(I);
    DoPaintSizingBar(xC, I, xR);
  end;

  xC.Unlock;

  {$IFDEF OMP_THEMES}
  if not ThemeServices.ThemesEnabled then
  {$ENDIF}
  begin
    Canvas.Draw(0, 0, fDoubleBufferedBitmap);

    Brush.Style := bsClear;//Disable flicker
  end;
  {$WARNINGS ON}
end;

procedure TOCustomMultiPanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
  ResizeControls;

  //inherited;

end;

procedure TOCustomMultiPanel.ReleaseLineDC;
begin
  {$IFDEF FPC}
  DestroyRubberBand(fSplitterWindow);
  {$ELSE}
  if FPrevBrush <> 0 then
    SelectObject(FLineDC, FPrevBrush);
  ReleaseDC(Parent.Handle, FLineDC);
  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
  {$ENDIF}
end;

procedure TOCustomMultiPanel.Resize;
begin
  inherited;

  DoSplitterMoved;

  //Invalidate;
end;

procedure TOCustomMultiPanel.ResizeControls;
var
  I, L: Integer;
  cPosAbs, DefaultSize: Integer;
begin
  if fInResizeControls then
    exit;

  fInResizeControls := True;
  try
    if not(
       not (csLoading in ComponentState) and
       not (csUpdating in ComponentState) and
       not (csDestroying in ComponentState)) then
    begin
      exit;
    end;

    fSplittersList.Clear;
    if csLoading in ComponentState then
      exit;

    cPosAbs := 0;
    if (PanelCollection.Count > 0) then
      PanelCollection[PanelCollection.Count-1].FPosition := 1;//use fPosition => do not check!!

    for I := 0 to PanelCollection.Count-1 do
    with TOMultiPanelItem(PanelCollection.Items[I]) do begin
      if not Assigned(Control) then
        continue;
      if Control.Parent <> Self then begin
        Control := nil;
        continue;
      end;

      //CHECK TAB STOP!!! -> BEFORE visible check
      if Control is TWinControl then
      with TWinControl(Control) do begin
        TabOrder := I;
      end;

      if (not Visible and not (csDesigning in ComponentState)) then begin
        Control.Visible := false;
        continue;
      end;

      if PanelType = ptHorizontal then begin
        DefaultSize := Round(Position*ClientWidth)-cPosAbs;//MUST NOT BE CLIENT WIDTH!!!
        if not(csDesigning in ComponentState) then
        for L := I+1 to PanelCollection.Count-1 do
        if not TOMultiPanelItem(PanelCollection.Items[L]).Visible then
          DefaultSize := Round(TOMultiPanelItem(PanelCollection.Items[L]).Position*ClientWidth)-cPosAbs;

        Control.SetBounds(cPosAbs, 0, DefaultSize, ClientHeight);
        if Control.Width <> DefaultSize then begin
          SetPositionOnlyWithCheck((cPosAbs+Control.Width)/Max(ClientWidth, 1));//ONDRA 08-09-29  <- Causes Stact Ovreflow when Position := ...; used
        end;
        Inc(cPosAbs, Control.Width);
        if cPosAbs < ClientWidth then
        with TSplitterListItem.CreateAdd(fSplittersList) do begin
          fPxPosition := cPosAbs;
          fPanelIndex := I;
        end;
      end else begin
        DefaultSize := Round(Position*ClientHeight)-cPosAbs;
        if not(csDesigning in ComponentState) then
        for L := I+1 to PanelCollection.Count-1 do
        if not TOMultiPanelItem(PanelCollection.Items[L]).Visible then
          DefaultSize := Round(TOMultiPanelItem(PanelCollection.Items[L]).Position*ClientHeight)-cPosAbs;

        Control.SetBounds(0, cPosAbs, ClientWidth, DefaultSize);
        if Control.Height <> DefaultSize then begin
          SetPositionOnlyWithCheck((cPosAbs+Control.Height)/Max(ClientHeight, 1))//ONDRA 12-02-24  <- Causes Stact Ovreflow when Position := ...; used
        end;
        Inc(cPosAbs, Control.Height);
        if cPosAbs < ClientHeight then
        with TSplitterListItem.CreateAdd(fSplittersList) do begin
          fPxPosition := cPosAbs;
          fPanelIndex := I;
        end;
      end;
      Inc(cPosAbs, SplitterSize);
    end;
  finally
    fInResizeControls := False;
  end;
end;

procedure TOCustomMultiPanel.SavePositionsToRegistry(aReg: TRegIniFile);
begin
  SavePositionsToRegistry(aReg, Self.Owner.ClassName, Self.Name);
end;

procedure TOCustomMultiPanel.SavePositionsToIniFile(aIni: TCustomIniFile;
  const aSection, aIdent: String);
var I: Integer;
begin
  for I := 0 to PanelCollection.Count-2 do
  with TOMultiPanelItem(PanelCollection.Items[I]) do
    aIni.WriteString(aSection, aIdent+'.Position'+IntToStr(I), FloatToStr(Position));
end;

procedure TOCustomMultiPanel.SavePositionsToIniFile(aIni: TCustomIniFile);
begin
  SavePositionsToIniFile(aIni, Self.Owner.ClassName, Self.Name);
end;

procedure TOCustomMultiPanel.SavePositionsToRegistry(aReg: TRegIniFile;
  const aSection, aIdent: String);
var I: Integer;
begin
  for I := 0 to PanelCollection.Count-2 do
  with TOMultiPanelItem(PanelCollection.Items[I]) do
    aReg.WriteString(aSection, aIdent+'.Position'+IntToStr(I), FloatToStr(Position));
end;

procedure TOCustomMultiPanel.SetSplitterHoverColor(const Value: TColor);
begin
  if fSplitterHoverColor <> Value then begin
    fSplitterHoverColor := Value;
    Repaint;
  end;
end;

procedure TOCustomMultiPanel.SetMinPosition(const Value: Double);
begin
  if fMinPosition <> Value then begin
    fMinPosition := Value;
    if (fMinPosition < 0) then
     fMinPosition := 0;
    if (fMinPosition > 0.4) then
      fMinPosition := 0.4;
  end;
end;

procedure TOCustomMultiPanel.SetPanelCollection(
  const Value: TOMultiPanelCollection);
begin
  fPanelCollection.Assign(Value);
end;

procedure TOCustomMultiPanel.SetPanelType(const Value: TPanelType);
begin
  if fPanelType <> Value then begin
    fPanelType := Value;
    ResizeControls;
  end;
end;

procedure TOCustomMultiPanel.SetParent(AParent: TWinControl);
begin
  inherited;
end;

procedure TOCustomMultiPanel.SetSplitterColor(const Value: TColor);
begin
  if fSplitterColor <> Value then begin
    fSplitterColor := Value;
    Repaint;
  end;
end;

procedure TOCustomMultiPanel.SetSplitterSize(const Value: Integer);
begin
  if fSplitterSize <> Value then begin
    fSplitterSize := Value;
    ResizeControls;
  end;
end;

{$IFNDEF FPC}
procedure TOCustomMultiPanel.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  DefaultHandler(Msg);
  FHitTest := ScreenToClient(SmallPointToPoint(Msg.Pos));
end;

procedure TOCustomMultiPanel.WMSetCursor(var Message: TWMSetCursor);
var
  Cur: HCURSOR;
  SizeState: Boolean;
  I: Integer;
begin
  Cur := 0;
  with Message do
  if (CursorWnd = Handle) and not (csDesigning in ComponentState) and (HitTest = HTCLIENT) then
  begin
    if not fSizing then
      //if fPanelType = ptHorizontal then
        CalcSizing(Self.FHitTest.X, Self.FHitTest.Y, {%H-}SizeState, {%H-}I);

    if fSizing or SizeState then begin
      {$WARNINGS OFF}
      if (fPanelType = ptHorizontal) then begin
        Cur := Windows.LoadCursor(0, IDC_SIZEWE);
      end else if (fPanelType = ptVertical) then begin
        Cur := Windows.LoadCursor(0, IDC_SIZENS);
      end;
      {$WARNINGS ON}
    end;

  end;
  if Cur <> 0 then begin
    Windows.SetCursor(Cur);
    Message.Result := 1;
  end else begin
    inherited;
  end;
end;
{$ENDIF}

{ TOMultiPanelItem }

procedure TOMultiPanelItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TOMultiPanelItem then
    with TOMultiPanelItem(Dest) do
    begin
      fPosition := Self.fPosition;
      fControl := Self.fControl;
      fVisible := Self.Visible;
    end;
end;

procedure TOMultiPanelItem.CheckPosition(var Value: Double);
begin
  if not(
     not (csLoading in MPOwner.ComponentState) and
     not (csUpdating in MPOwner.ComponentState) and
     not (csDestroying in MPOwner.ComponentState)) then
  begin
    exit;
  end;

  if (MPOwner = nil) or (csLoading in MPOwner.ComponentState) then//Component is Loading
    exit;

  if (Index = Collection.Count-1) then
    Value := 1;

  if (Index = 0) and (Value < MPOwner.MinPosition) then
    Value := MPOwner.MinPosition;
  if (Index = Collection.Count-2) and (Value > 1-MPOwner.MinPosition) then
    Value := 1-MPOwner.MinPosition;

  if(Index > 0) and (TOMultiPanelItem(Collection.Items[Index-1]).Position+MPOwner.MinPosition > Value) then
    Value := TOMultiPanelItem(Collection.Items[Index-1]).Position + MPOwner.MinPosition;
  if(Index < Collection.Count-1) and (TOMultiPanelItem(Collection.Items[Index+1]).Position-MPOwner.MinPosition < Value) then
    Value := TOMultiPanelItem(Collection.Items[Index+1]).Position - MPOwner.MinPosition;
end;

constructor TOMultiPanelItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  MPCollection.SetDefaultPositions;

  fControl := nil;
  fVisible := true;
end;

function TOMultiPanelItem.MPCollection: TOMultiPanelCollection;
begin
  if Collection is TOMultiPanelCollection then
    Result := TOMultiPanelCollection(Collection)
  else
    Result := nil;
end;

function TOMultiPanelItem.MPOwner: TOCustomMultiPanel;
begin
  Result := MPCollection.MPOwner;
end;

procedure TOMultiPanelItem.SetControl(const Value: TControl);
begin
  {if Assigned(Value) and (MPOwner <> nil) and (Value.Parent <> nil) and (Value.Parent <> MPOwner) then begin//COMPARE NAMES, OTHERWISE THERE ARE ERRORS
    MessageDlg('The control''s parent must be the MultiPanel. Otherwise it won''t be added!', mtError, [mbOK], 0);
    ShowMessage('Value.Parent: '+Value.Parent.ClassName+#13#10+'MPOwner: '+MPOwner.ClassName);
    exit;
  end;}

  fControl := Value;
  (*if Assigned(Value) then begin
    {if (MPOwner <> nil) then
      Value.Parent := MPOwner;}
    Value.Align := alNone;
  end;*)
  
  if not (csLoading in MPOwner.ComponentState) and
     not (csUpdating in MPOwner.ComponentState) and
     not (csDestroying in MPOwner.ComponentState) then
  begin
    if Assigned(fControl) then begin
      fControl.Align := alNone;
    end;

    MPOwner.ResizeControls;
  end;
end;

procedure TOMultiPanelItem.SetIndex(Value: Integer);
begin
  inherited SetIndex(Value);

  if not (csLoading in MPOwner.ComponentState) and
     not (csUpdating in MPOwner.ComponentState) and
     not (csDestroying in MPOwner.ComponentState) then
  begin
    MPCollection.SetDefaultPositions;
    MPOwner.ResizeControls;
  end;
end;

procedure TOMultiPanelItem.SetPosition(const Value: Double);
begin
  if fPosition <> Value then begin
    fPosition  := Value;

    if not (csLoading in MPOwner.ComponentState) and
       not (csUpdating in MPOwner.ComponentState) and
       not (csDestroying in MPOwner.ComponentState) then
    begin
      CheckPosition(fPosition);

      MPOwner.ResizeControls;
      MPOwner.DoSplitterMoved;
    end;
  end;
end;

procedure TOMultiPanelItem.SetPositionOnlyWithCheck(const Value: Double);
begin
  if fPosition <> Value then begin
    fPosition  := Value;

    if not (csLoading in MPOwner.ComponentState) and
       not (csUpdating in MPOwner.ComponentState) and
       not (csDestroying in MPOwner.ComponentState) then
    begin
      CheckPosition(fPosition);
    end;
  end;
end;

procedure TOMultiPanelItem.SetVisible(const Value: Boolean);
begin
  if fVisible <> Value then begin
    fVisible := Value;

    if not (csLoading in MPOwner.ComponentState) and
       not (csUpdating in MPOwner.ComponentState) and
       not (csDestroying in MPOwner.ComponentState) then
    begin
      if Assigned(fControl) then
        fControl.Visible := fVisible;

      MPOwner.ResizeControls;
      MPOwner.DoSplitterMoved;
    end;
  end;
end;

{ TOMultiPanelCollection }

function TOMultiPanelCollection.Add: TOMultiPanelItem;
begin
  Result := TOMultiPanelItem(inherited Add);
end;

procedure TOMultiPanelCollection.AddControl(AControl: TControl; AIndex: Integer);
var I: Integer;
  Item: TOMultiPanelItem;
begin
  if IndexOf(AControl) >= 0 then
    exit;

  if AIndex = -1 then
  for I := 0 to Count-1 do
  if not Assigned(Items[I].Control) then begin
    AIndex := I;
    break;
  end;
  if AIndex = -1 then
    Item := Add
  else
    Item := Items[AIndex];

  Item.Control := AControl;
end;

constructor TOMultiPanelCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOMultiPanelItem);
end;

function TOMultiPanelCollection.GetAttrCount: Integer;
begin
  Result := 0;
end;

function TOMultiPanelCollection.GetItem(Index: Integer): TOMultiPanelItem;
begin
  Result := TOMultiPanelItem(inherited GetItem(Index));
end;

function TOMultiPanelCollection.IndexOf(AControl: TControl): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Control = AControl then
      Exit;
  Result := -1;
end;

procedure TOMultiPanelCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;

end;

procedure TOMultiPanelCollection.RemoveControl(AControl: TControl);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Items[I].Control = AControl then
    begin
      Items[I].Control := nil;
      Delete(I);
      Exit;
    end;
end;

function TOMultiPanelCollection.MPOwner: TOCustomMultiPanel;
begin
  Result := GetOwner as TOCustomMultiPanel;
end;

procedure TOMultiPanelCollection.SetDefaultPositions;
var I: Integer;
begin
  if (MPOwner <> nil) and not (csLoading in MPOwner.ComponentState) then
  for I := 0 to Count-1 do
    TOMultiPanelItem(Items[I]).FPosition := (I+1)/Count;
end;

procedure TOMultiPanelCollection.SetItem(Index: Integer; Value: TOMultiPanelItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TOMultiPanelCollection.Update(Item: TCollectionItem);
begin
  inherited;
  exit;
  if MPOwner <> nil then
    with MPOwner do
    begin
      ResizeControls;
    end;
end;

{ TSplitterListItem }

constructor TSplitterListItem.CreateAdd(aAddToList: TList);
begin
  inherited Create;

  fPxPosition := -1;
  fPanelIndex := -1;

  aAddToList.Add(Self);
end;

{ TMyControl }

{procedure TMyControl.ConstrainedResize(var MinWidth, MinHeight, MaxWidth,
  MaxHeight: Integer);
begin
  inherited ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);
end;}

end.

