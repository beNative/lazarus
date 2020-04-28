{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.filter.ToolView;

{$MODE DELPHI}

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
  ComCtrls, StdCtrls, ActnList, Contnrs,

  VirtualTrees,

  ts.Core.TreeviewPresenter, ts.Core.ColumnDefinitions, ts.Core.DataTemplates,

  ts.Editor.ToolView.Base;

type
  TfrmFilter = class(TCustomEditorToolView)
    aclMain            : TActionList;
    actFocusFilterText : TAction;
    edtFilter          : TEdit;
    pnlView            : TPanel;
    pnlHeader          : TPanel;
    pnlMain            : TPanel;
    sbrMain            : TStatusBar;

    function CCustomDraw(Sender: TObject; ColumnDefinition: TColumnDefinition;
      Item: TObject; TargetCanvas: TCanvas; CellRect: TRect;
      ImageList: TCustomImageList; DrawMode: TDrawMode;
      Selected: Boolean): Boolean;

    procedure actFocusFilterTextExecute(Sender: TObject);

    procedure edtFilterChange(Sender: TObject);
    procedure edtFilterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtFilterKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure FTVPFilter(Item: TObject; var Accepted: Boolean);
    procedure FVSTKeyPress(Sender: TObject; var Key: char);
    procedure FVSTKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

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
      var AMatch    : string; // Matching string
      var APos      : Integer // Character position where the match starts
    ): Boolean; overload; inline;
    procedure CalcMatchRect(
      const ASource     : string;   // String to search for a match
      const AMatch      : string;   // The found match
      const AOffset     : Integer;  // Character offset in Source to Match
      ACanvas           : TCanvas;
      AColumnDefinition : TColumnDefinition;
      var ARect         : TRect
    ); inline;
    procedure DrawMatchRect(
      ACanvas     : TCanvas;
      const ARect : TRect
    ); inline;

    procedure InitializeComponents;

  protected
    procedure SetVisible(Value: boolean); override;

    procedure UpdateActions; override;

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

    { If no ItemTemplate is specified, a default one will be created that
      returns the values of all published properties of the objects in the
      ItemsSource.}
    property ItemTemplate: IDataTemplate
      read GetItemTemplate write SetItemTemplate;
  end;

implementation

{$R *.lfm}

uses
  Variants, TypInfo,
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}

  LCLProc, LCLType, LMessages, GraphUtil,

  ts.Core.SharedLogger, ts.Core.Utils, ts.Core.Helpers,
  ts.Core.ColumnDefinitionsDataTemplate,

  ts.Editor.Utils;

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
    VK_SPACE,
    VK_0..VK_Z,
    VK_OEM_1..VK_OEM_102,
    VK_NUMPAD0..VK_DIVIDE
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
    VK_CONTROL,
    VK_SHIFT,
    VK_INSERT,
    VK_DELETE,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END
  ];

{$REGION 'construction and destruction'}
procedure TfrmFilter.AfterConstruction;
begin
  inherited AfterConstruction;
  FVST := VST.Create(Self, pnlView);
  FVST.OnKeyPress := FVSTKeyPress;
  FVST.OnKeyUp := FVSTKeyUp;
  FTVP := CreateTVP(Self, FVST);
  FTVP.MultiLine := False;

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
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmFilter.edtFilterChange(Sender: TObject);
begin
  FTVP.ApplyFilter;
end;

function TfrmFilter.CCustomDraw(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
var
  Match  : string = '';
  Offset : Integer = 0;
  R      : TRect;
  S      : string;
begin
  Result := False;
  if DrawMode = dmBeforeCellPaint then
  begin
    S := ItemTemplate.GetText(Item, ColumnDefinition.Index);
    if IsMatch(S, Match, Offset) then
    begin
      R := CellRect;
      CalcMatchRect(S, Match, Offset, TargetCanvas, ColumnDefinition, R);
      DrawMatchRect(TargetCanvas, R);
      Result := True;
    end;
  end;
end;

procedure TfrmFilter.actFocusFilterTextExecute(Sender: TObject);
begin
  edtFilter.SetFocus;
  edtFilter.SelectAll;
end;

procedure TfrmFilter.edtFilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  A : Boolean;
  B : Boolean;
  C : Boolean;
  D : Boolean;
  E : Boolean;
  F : Boolean;
  G : Boolean;
begin
  //Logger.Send('KeyDown ActiveControl:', ActiveControl.ClassName);
  //Logger.Send('Keys : %s', [KeyAndShiftStateToKeyString(Key, Shift)]);
  // SHIFTED and ALTED keycombinations
  A := (ssAlt in Shift) or (ssShift in Shift);
  { Single keys that need to be handled by the edit control like all displayable
    characters but also HOME and END }
  B := (Key in VK_EDIT_KEYS) and (Shift = []);
  { CTRL-keycombinations that need to be handled by the edit control like
    CTRL-C for clipboard copy. }
  C := (Key in VK_CTRL_EDIT_KEYS) and (Shift = [ssCtrlOS]);
  { SHIFT-keycombinations that need to be handled by the edit control for
    uppercase characters but also eg. SHIFT-HOME for selections. }
  D := (Key in VK_SHIFT_EDIT_KEYS) and (Shift = [ssShift]);
  { Only CTRL key is pressed. }
  E := (Key = VK_CONTROL) and (Shift = [ssCtrlOS]);
  { Only SHIFT key is pressed. }
  F := (Key = VK_SHIFT) and (Shift = [ssShift]);
  { Only (left) ALT key is pressed. }
  G := (Key = VK_MENU) and (Shift = [ssAlt]);
  if not (A or B or C or D or E or F or G) then
  begin
    FVKPressed := True;
    Key := 0;
  end
  { Prevents jumping to the application's main menu which happens by default
    if ALT is pressed. }
  else if G then
  begin
    Key := 0;
  end;
end;

procedure TfrmFilter.edtFilterKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FVKPressed and FVST.Enabled then
  begin
    FVST.Perform(LM_KEYDOWN, Key, 0);
    if Visible and FVST.CanFocus then
      FVST.SetFocus;
  end;
  FVKPressed := False;
end;

procedure TfrmFilter.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  Filter := '';
end;

procedure TfrmFilter.FormShow(Sender: TObject);
begin
  FTVP.ApplyFilter;
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
    S := TypInfo.GetPropValue(Item, C.Name, True);
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
{$IFDEF WINDOWS}
    PostMessage(edtFilter.Handle, WM_CHAR, Ord(Key), 0);
{$ENDIF}
    edtFilter.SelStart := Length(Filter);
    // required to prevent the invocation of accelerator keys!
    Key := #0;
  end;
end;

procedure TfrmFilter.FVSTKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_MENU) and (Shift = []) then // only ALT pressed
  begin
    Key := 0;
  end;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmFilter.GetFilter: string;
begin
  Result := edtFilter.Text;
end;

procedure TfrmFilter.SetFilter(AValue: string);
begin
  if AValue <> Filter then
  begin
    edtFilter.Text := AValue;
  end;
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
{$ENDREGION}

{$REGION 'private methods'}
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
          Manager.Settings.Colors.HighlightAllColor.Background,
          Manager.Settings.Colors.HighlightAllColor.BackAlpha
        )
      end
      else
      begin
        C := ColorAdjustLuma(
          Manager.Settings.Colors.HighlightAllColor.Background,
          70,
          False
        );
      end;
      ACanvas.Pen.Color := Manager.Settings.Colors.HighlightAllColor.FrameColor;
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
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmFilter.UpdateActions;
begin
  inherited UpdateActions;
end;
{$ENDREGION}
end.

