{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit ts_Editor_ToolView_Filter;

{$MODE Delphi}

{
  This is an attempt to make a reusable filter view using a virtual treeview and
  a presenter.

  sequence that we need to follow:

  - create VST (Owner, Parent)
  - assign VST events
  - create TVP (Owner)
  - create TVP datatemplate (takes columndefinitions as argument)
  - assign TVP.ItemTemplate
  - add columndefinitions for TVP
  - assign TVP events (filter event)
  - assign TVP.ItemsSource property
  - assign TVP.Treeview property

  }


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Contnrs,

  VirtualTrees,

  ts.Core.TreeviewPresenter, ts.Core.ColumnDefinitions, ts.Core.DataTemplates,

  ts_Editor_ToolView_Base;

type

  { TfrmFilter }

  TfrmFilter = class(TCustomEditorToolView)
    edtFilter: TEdit;
    pnlView: TPanel;
    pnlHeader: TPanel;
    pnlMain: TPanel;
    sbrMain: TStatusBar;

    function CCustomDraw(Sender: TObject; ColumnDefinition: TColumnDefinition;
      Item: TObject; TargetCanvas: TCanvas; CellRect: TRect;
      ImageList: TCustomImageList; DrawMode: TDrawMode;
      Selected: Boolean): Boolean;
    procedure edtFilterChange(Sender: TObject);
    procedure edtFilterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtFilterKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FTVPFilter(Item: TObject; var Accepted: Boolean);
    procedure FVSTKeyPress(Sender: TObject; var Key: char);

  strict private
    FVST : TVirtualStringTree;
    FTVP : TTreeViewPresenter;

    FVKPressed : Boolean;
    FTextStyle : TTextStyle;

    function GetColumnDefinitions: TColumnDefinitions;
    function GetFilter: string;
    function GetItemsSource: TObjectList;
    function GetItemTemplate: IDataTemplate;
    procedure SetFilter(AValue: string);
    procedure SetItemsSource(AValue: TObjectList);
    procedure SetItemTemplate(AValue: IDataTemplate);

    function IsMatch(const AString : string): Boolean; overload; inline;
    function IsMatch(
      const AString : string; // Search string
        var AMatch  : string; // Matching string
        var APos    : Integer // Character position where the match starts
    ): Boolean; overload; inline;
    procedure CalcMatchRect(
      const ASource           : string;   // String to search for a match
      const AMatch            : string;   // The found match
      const AOffset           : Integer;  // Character offset in Source to Match
            ACanvas           : TCanvas;
            AColumnDefinition : TColumnDefinition;
        var ARect             : TRect
    ); inline;
    procedure DrawMatchRect(
            ACanvas : TCanvas;
      const ARect   : TRect
    ); inline;

    procedure InitializeComponents;

  protected
    procedure SetVisible(Value: boolean); override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Filter: string
      read GetFilter write SetFilter;

    property ColumnDefinitions: TColumnDefinitions
      read GetColumnDefinitions;

    { TObjectlist holding the objects we want to display. If no data template
      is used the published properties will be shown. }
    property ItemsSource: TObjectList
      read GetItemsSource write SetItemsSource;

    { If no ItemTemplate is specified, a default  one will be created that
      returns the values of all published properties of the objects in the ItemsSource.}
    property ItemTemplate: IDataTemplate
      read GetItemTemplate write SetItemTemplate;
  end;

implementation

{$R *.lfm}

uses
  Variants,

  LMessages, GraphUtil,

  Windows,

  ts.Core.Utils,
  ts.Editor.Utils, ts.Core.Helpers, ts.Core.ColumnDefinitionsDataTemplate;

type
  TVKSet = set of Byte;

var
  VK_EDIT_KEYS : TVKSet = [
    VK_DELETE,
    VK_BACK,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END,
    VK_SHIFT,
    VK_CONTROL,
    VK_SPACE,
    VK_0..VK_Z,
    VK_OEM_1..VK_OEM_102,
    VK_MULTIPLY..VK_DIVIDE
  ];

  VK_CTRL_EDIT_KEYS : TVKSet = [
    VK_INSERT,
    VK_DELETE,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END,
    VK_C,
    VK_X,
    VK_V,
    VK_Z
  ];

  VK_SHIFT_EDIT_KEYS : TVKSet = [
    VK_INSERT,
    VK_DELETE,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END
  ];

{ TfrmFilter }

{$region 'construction and destruction' /fold}
procedure TfrmFilter.AfterConstruction;
begin
  inherited AfterConstruction;
  FVST := VST.Create(Self, pnlView);
  FVST.OnKeyPress := FVSTKeyPress;
  FTVP := CreateTVP(Self, FVST);

  FTextStyle.SingleLine := True;
  FTextStyle.Opaque     := False;
  FTextStyle.ExpandTabs := False;
  FTextStyle.Wordbreak  := False;
  FTextStyle.ShowPrefix := True;
  FTextStyle.Clipping   := False;
  FTextStyle.SystemFont := False;
  FTextStyle.Alignment  := taLeftJustify;
  FTextStyle.Layout     := tlCenter;
end;

procedure TfrmFilter.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'event handlers' /fold}
procedure TfrmFilter.edtFilterChange(Sender: TObject);
begin
  FTVP.ApplyFilter;
end;

function TfrmFilter.CCustomDraw(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
var
  Match  : string;
  Offset : Integer;
  R      : TRect;
  S      : string;
begin
  if DrawMode = dmBeforeCellPaint then
  begin
    S := ItemTemplate.GetText(Item, ColumnDefinition.Index);
    if IsMatch(S, Match, Offset) then
    begin
      R := CellRect;
      CalcMatchRect(S, Match, Offset, TargetCanvas, ColumnDefinition, R);
      DrawMatchRect(TargetCanvas, R);
    end;
  end;
end;

procedure TfrmFilter.edtFilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  A : Boolean;
  B : Boolean;
  C : Boolean;
  D : Boolean;
begin
  A := (ssAlt in Shift) or (ssShift in Shift);
  B := (Key in VK_EDIT_KEYS) and (Shift = []);
  C := (Key in VK_CTRL_EDIT_KEYS) and (Shift = [ssCtrl]);
  D := (Key in VK_SHIFT_EDIT_KEYS) and (Shift = [ssShift]);
  if not (A or B or C or D) then
  begin
    FVKPressed := True;
    Key := 0;
  end;
end;

procedure TfrmFilter.edtFilterKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FVKPressed and FVST.Enabled then
  begin
{$IFDEF Windows}
    PostMessage(FVST.Handle, WM_KEYDOWN, Key, 0);
{$ENDIF}
    if Visible and FVST.CanFocus then
      FVST.SetFocus;
  end;
  FVKPressed := False;
end;

procedure TfrmFilter.FTVPFilter(Item: TObject; var Accepted: Boolean);
var
  B : Boolean;
  S : string;
  I : Integer;
  C : TColumnDefinition;
begin
  B := False;
  for I := 0 to ColumnDefinitions.Count - 1 do
  begin
    C := ColumnDefinitions[I];
    S := GetPropValue(Item, C.Name, True);
    B := B or IsMatch(S);
  end;
  Accepted := B;
end;

procedure TfrmFilter.FVSTKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = VK_RETURN then
  begin
    Close;
  end
  else if Ord(Key) = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Close;
  end
  else if not edtFilter.Focused then
  begin
    edtFilter.SetFocus;
    PostMessage(edtFilter.Handle, LM_CHAR, Ord(Key), 0);
    edtFilter.SelStart := Length(Filter);
    // required to prevent the invocation of accelerator keys!
    Key := #0;
  end;
end;
{$endregion}

{$region 'property access mehods' /fold}
function TfrmFilter.GetFilter: string;
begin
  Result := edtFilter.Text;
end;

function TfrmFilter.GetItemsSource: TObjectList;
begin
  Result := FTVP.ItemsSource;
end;

procedure TfrmFilter.SetItemsSource(AValue: TObjectList);
begin
  FTVP.ItemsSource := AValue;
end;

function TfrmFilter.GetItemTemplate: IDataTemplate;
begin
  Result := FTVP.ItemTemplate;
end;

procedure TfrmFilter.SetItemTemplate(AValue: IDataTemplate);
begin
  FTVP.ItemTemplate := AValue;
end;

procedure TfrmFilter.SetFilter(AValue: string);
begin
  if AValue <> Filter then
  begin
    edtFilter.Text := AValue;
  end;
end;

function TfrmFilter.GetColumnDefinitions: TColumnDefinitions;
begin
  Result := FTVP.ColumnDefinitions;
end;

procedure TfrmFilter.SetVisible(Value: boolean);
begin
  if Value then
  begin
    // check properties
    if ItemsSource = nil then
      Exception.Create('ItemSource property not assigned');
    InitializeComponents;
  end;
  inherited SetVisible(Value);
end;
{$endregion}

{$region 'private methods' /fold}
function TfrmFilter.IsMatch(const AString: string): Boolean;
begin
  if Filter = '' then
    Result := True
  else
    Result := StrPos(Filter, AString, False) > 0;
end;

function TfrmFilter.IsMatch(const AString: string; var AMatch: string;
  var APos: Integer): Boolean;
begin
  APos   := -1;
  AMatch := '';
  Result := False;
  if Filter <> '' then
  begin
    APos   := StrPos(Filter, AString, False);
    AMatch := System.Copy(AString, APos, Length(Filter));
    Result := APos > 0;
  end;
end;

procedure TfrmFilter.InitializeComponents;
var
  C : TColumnDefinition;
  I : Integer;
begin
  // connect custom draw events
  for I := 0 to ColumnDefinitions.Count - 1 do
  begin
    C := ColumnDefinitions[I];
    C.OnCustomDraw := CCustomDraw;
  end;

  FTVP.OnFilter := FTVPFilter;
  FTVP.TreeView := FVST;
end;

procedure TfrmFilter.CalcMatchRect(const ASource: string; const AMatch: string;
  const AOffset: Integer; ACanvas: TCanvas;
  AColumnDefinition: TColumnDefinition; var ARect: TRect);
var
  Margin: Integer;
begin
  // calculate the rectangle to draw around the matching text
  Margin := AColumnDefinition.Margin + AColumnDefinition.Spacing;
  ARect.Left := ARect.Left + Margin +
    ACanvas.TextWidth(System.Copy(ASource, 1, AOffset - 1));
  ARect.Right := ARect.Left + ACanvas.TextWidth(AMatch);
end;

procedure TfrmFilter.DrawMatchRect(ACanvas: TCanvas; const ARect: TRect);
var
  C : TColor;
  B : TBrush;
  P : TPen;
begin
  B := TBrush.Create;
  try
    B.Assign(ACanvas.Brush); // copy original brush
    P := TPen.Create;
    try
      P.Assign(ACanvas.Pen); // copy original pen
      C := ColorToRGB(ACanvas.Brush.Color);
      if C <> clWhite then
      begin
        C := MixColors(
          C,
          Manager.Settings.HighlightAllColor.Background,
          Manager.Settings.HighlightAllColor.BackAlpha
        )
      end
      else
      begin
        C := ColorAdjustLuma(
          Manager.Settings.HighlightAllColor.Background,
          70,
          False
        );
      end;
      ACanvas.Pen.Color := Manager.Settings.HighlightAllColor.FrameColor;
      ACanvas.Pen.Width := 1;
      ACanvas.Brush.Color := C;
      ACanvas.Rectangle(ARect);
      ACanvas.Pen.Assign(P); // restore original pen
      ACanvas.Brush.Assign(B); // restore original brush
    finally
      P.Free;
    end;
  finally
    B.Free;
  end;
end;
{$endregion}

end.

