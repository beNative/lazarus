{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.CodeFilter.ToolView;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls,
  ActnList, Buttons, Contnrs, Menus, ComCtrls,

  LCLType, GraphUtil,

  VirtualTrees, RegExpr,

  ts.Core.TreeViewPresenter, ts.Core.ColumnDefinitions, ts.Core.DataTemplates,

  ts.Editor.Interfaces, ts.Editor.Utils, ts.Editor.CodeFilter.Settings;

type
  TfrmCodeFilterDialog = class(TForm)
    {$REGION 'designer controls'}
    aclMain              : TActionList;
    actApplyFilter       : TAction;
    actCopyToNewView     : TAction;
    actFocusSearchFilter : TAction;
    actCopy              : TAction;
    actMatchCase         : TAction;
    actRegularExpression : TAction;
    actSelectAll         : TAction;
    btnMatchCase         : TSpeedButton;
    btnRegularExpression : TSpeedButton;
    btnApply             : TSpeedButton;
    edtFilter            : TEdit;
    grpOutput            : TGroupBox;
    Image1               : TImage;
    imlMain              : TImageList;
    lblSearch            : TLabel;
    MenuItem1            : TMenuItem;
    MenuItem2            : TMenuItem;
    MenuItem3            : TMenuItem;
    mniCopy              : TMenuItem;
    mniCopyToNewView     : TMenuItem;
    mniSelectAll         : TMenuItem;
    pnlVST               : TPanel;
    ppmMain              : TPopupMenu;
    sbrMain              : TStatusBar;
    tmrUpdate            : TTimer;
    tlbMain              : TToolBar;
    ToolButton1          : TToolButton;
    ToolButton3          : TToolButton;
    ToolButton4          : TToolButton;
    {$ENDREGION}

    procedure actApplyFilterExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCopyToNewViewExecute(Sender: TObject);
    procedure actFocusSearchFilterExecute(Sender: TObject);
    procedure actMatchCaseExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);

    procedure EditorSettingsChanged(Sender: TObject);
    procedure EditorActiveViewChanged(Sender: TObject);
    procedure EditorChange(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure edtFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtFilterKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function FTVPColumnDefinitionsItemsCustomDraw(Sender: TObject;
      ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas:
      TCanvas; CellRect: TRect; ImageList: TCustomImageList;
      DrawMode: TDrawMode; Selected: Boolean): Boolean;
    procedure FTVPFilter(Item: TObject; var Accepted: Boolean);
    procedure FTVPSelectionChanged(Sender: TObject);
    procedure FVSTKeyPress(Sender: TObject; var Key: Char);
    procedure FVSTKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tmrUpdateTimer(Sender: TObject);

  private
    FTVP              : TTreeViewPresenter;
    FVST              : TVirtualStringTree;
    FLines            : TObjectList;
    FUpdate           : Boolean;
    FUpdateEditorView : Boolean;
    FVKPressed        : Boolean;
    FTextStyle        : TTextStyle;
    FRegExpr          : TRegExpr;
    FIsCompiled       : Boolean;

    function GetManager: IEditorManager;
    function GetFilter: string;
    function GetSettings: TCodeFilterSettings;
    procedure SetFilter(AValue: string);
    function GetMatchCase: Boolean;
    function GetRegEx: Boolean;
    function GetView: IEditorView;
    function GetForm: TForm;
    function GetName: string;

  protected
    procedure Modified;

    { IEditorToolView }
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean); override;
    procedure UpdateStatusDisplay;
    procedure UpdateView;

    property Visible: Boolean
      read GetVisible write SetVisible;

    property Name: string
      read GetName;

    property Form: TForm
      read GetForm;

    property Filter: string
      read GetFilter write SetFilter;

    property View: IEditorView
      read GetView;

    property Manager: IEditorManager
      read GetManager;

    property RegEx: Boolean
      read GetRegEx;

    property MatchCase: Boolean
      read GetMatchCase;

    property Settings : TCodeFilterSettings
      read GetSettings;

    procedure UpdateActions; override;
    procedure ApplyFilter;
    procedure FillList(AStrings: TStrings);
    function IsMatch(
      const AString : string;
        var AMatch  : string;
        var APos    : Integer
    ): Boolean; overload; inline;
    function IsMatch(const AString : string): Boolean; overload; inline;

  public
     procedure AfterConstruction; override;
     procedure BeforeDestruction; override;

  end;

implementation

{$R *.lfm}

uses
  Clipbrd, StrUtils,
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}
  LCLIntf, LMessages,

  SynEditHighlighter,

  ts.Core.ColumnDefinitionsDataTemplate, ts.Core.Helpers, ts.Core.Utils,

  ts.Editor.Resources,

  ts.Editor.CodeFilter.Data;

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

const
  MAX_LINES = 100000; // max. amount of lines for automatic filtering

resourcestring
  SFilteredCode = '<Filtered code>';
  SLineIndex    = 'Line';
  SLineText     = 'Text';

{$REGION 'construction and destruction'}
procedure TfrmCodeFilterDialog.AfterConstruction;
begin
  inherited AfterConstruction;
  Manager.Settings.AddEditorSettingsChangedHandler(EditorSettingsChanged);
  Manager.Events.AddOnActiveViewChangeHandler(EditorActiveViewChanged);
  Manager.Events.AddOnChangeHandler(EditorChange);
  FTextStyle.SingleLine := True;
  FTextStyle.Opaque := False;
  FTextStyle.ExpandTabs := False;
  FTextStyle.Wordbreak := False;
  FTextStyle.ShowPrefix := True;
  FTextStyle.Clipping := False;
  FTextStyle.SystemFont := False;
  FTextStyle.Alignment := taLeftJustify;
  FTextStyle.Layout := tlCenter;
  FVST := VST.Create(Self, pnlVST);
  FVST.Font.Assign(Manager.Settings.EditorFont);
  FVST.Font.Size := 8;
  FVST.TreeOptions.PaintOptions := FVST.TreeOptions.PaintOptions - [toShowHorzGridLines, toShowVertGridLines];
  FVST.Colors.FocusedSelectionColor := clSilver;
  FVST.Colors.FocusedSelectionBorderColor := clSilver;
  FVST.Colors.SelectionRectangleBorderColor := clSilver;
  FVST.Colors.SelectionRectangleBlendColor := clSilver;
  FVST.OnKeyPress := FVSTKeyPress;
  FVST.OnKeyUp := FVSTKeyUp;
  FVST.Header.MainColumn := 1;
  FVST.DefaultNodeHeight := 16;
  FTVP := TTreeViewPresenter.Create(Self);
  FTVP.MultiSelect := True;
  FTVP.SelectionMode := smMulti;
  FTVP.MultiLine := False;
  FTVP.ItemTemplate := TColumnDefinitionsDataTemplate.Create(FTVP.ColumnDefinitions);
  FTVP.ColumnDefinitions.AddColumn('Index', SLineIndex, dtNumeric, 50);
  FTVP.ColumnDefinitions.AddColumn('Text', SLineText, dtString, 600, 500, 4000);
  FTVP.ColumnDefinitions.Items[0].Fixed := False;
  FTVP.ColumnDefinitions.Items[0].MaxWidth := 50;
  FTVP.ColumnDefinitions.Items[0].OnCustomDraw := FTVPColumnDefinitionsItemsCustomDraw;
  FTVP.ColumnDefinitions.Items[1].OnCustomDraw := FTVPColumnDefinitionsItemsCustomDraw;
  FTVP.TreeView := FVST;
  FLines := TObjectList.Create(True);
  FTVP.ItemsSource := FLines;
  FTVP.OnFilter := FTVPFilter;
  FTVP.OnSelectionChanged := FTVPSelectionChanged;
  FRegExpr := TRegExpr.Create;
  Settings.FormSettings.AssignTo(Self);
end;

procedure TfrmCodeFilterDialog.BeforeDestruction;
begin
  FLines.Free;
  FTVP.TreeView := nil;
  FVST.Free;
  FRegExpr.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmCodeFilterDialog.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

procedure TfrmCodeFilterDialog.SetVisible(AValue: Boolean);
begin
  inherited SetVisible(AValue);
end;

function TfrmCodeFilterDialog.GetForm: TForm;
begin
  Result := Self;
end;

function TfrmCodeFilterDialog.GetName: string;
begin
  Result := inherited Name;
end;

function TfrmCodeFilterDialog.GetMatchCase: Boolean;
begin
  Result := actMatchCase.Checked;
end;

function TfrmCodeFilterDialog.GetRegEx: Boolean;
begin
  Result := actRegularExpression.Checked;
end;

function TfrmCodeFilterDialog.GetFilter: string;
begin
  Result := edtFilter.Text;
end;

procedure TfrmCodeFilterDialog.SetFilter(AValue: string);
begin
  if AValue <> Filter then
  begin
    edtFilter.Text := AValue;
  end;
end;

function TfrmCodeFilterDialog.GetSettings: TCodeFilterSettings;
begin
  Result := (Manager as IEditorSettings)
    .ToolSettings.ItemsByClass[TCodeFilterSettings] as TCodeFilterSettings;
end;

function TfrmCodeFilterDialog.GetView: IEditorView;
begin
  Result := Owner as IEditorView;
end;

function TfrmCodeFilterDialog.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmCodeFilterDialog.EditorSettingsChanged(Sender: TObject);
begin
  FVST.Font.Assign(Manager.Settings.EditorFont);
  FVST.Font.Size := 8;
end;

procedure TfrmCodeFilterDialog.EditorActiveViewChanged(Sender: TObject);
begin
  Modified;
  UpdateView;
end;

procedure TfrmCodeFilterDialog.EditorChange(Sender: TObject);
begin
  Modified;
  UpdateView;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmCodeFilterDialog.actApplyFilterExecute(Sender: TObject);
begin
  Modified;
end;

procedure TfrmCodeFilterDialog.actCopyExecute(Sender: TObject);
var
  I  : Integer;
  SL : TStringList;
  L  : TLine;
begin
  SL := TStringList.Create;
  try
    for I := 0 to FTVP.SelectedItems.Count - 1 do
    begin
      L := TLine(FTVP.SelectedItems[I]);
      SL.Add(L.Text);
    end;
    Clipboard.AsText := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TfrmCodeFilterDialog.actCopyToNewViewExecute(Sender: TObject);
var
  I  : Integer;
  SL : TStringList;
  L  : TLine;
  S  : string;
begin
  SL := TStringList.Create;
  try
    for I := 0 to FTVP.SelectedItems.Count - 1 do
    begin
      L := TLine(FTVP.SelectedItems[I]);
      SL.Add(L.Text);
    end;
    S := View.HighlighterName;
    with Manager.Views.Add do
    begin
      HighlighterName := S;
      FileName := SFilteredCode;
      Text := SL.Text;
    end;
  finally
    SL.Free;
  end;
end;

procedure TfrmCodeFilterDialog.actFocusSearchFilterExecute(Sender: TObject);
begin
  edtFilter.SetFocus;
end;

procedure TfrmCodeFilterDialog.actMatchCaseExecute(Sender: TObject);
begin
  Modified;
end;

procedure TfrmCodeFilterDialog.actSelectAllExecute(Sender: TObject);
begin
  FTVP.SelectAll;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmCodeFilterDialog.edtFilterChange(Sender: TObject);
begin
  if not RegEx and (FLines.Count < MAX_LINES) then
    Modified;
end;

procedure TfrmCodeFilterDialog.edtFilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  A : Boolean;
  B : Boolean;
  C : Boolean;
  D : Boolean;
  E : Boolean;
  F : Boolean;
  G : Boolean;
  H : Boolean;
begin
  { SHIFTed and ALTed keycombinations }
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
  { ESCAPE }
  H := Key = VK_ESCAPE;
  if not (A or B or C or D or E or F or G or H) then
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

procedure TfrmCodeFilterDialog.edtFilterKeyUp(Sender: TObject; var Key: Word;
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

procedure TfrmCodeFilterDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  Filter := '';
end;

procedure TfrmCodeFilterDialog.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
  end
  else
    inherited;
end;

procedure TfrmCodeFilterDialog.FormResize(Sender: TObject);
begin
  Settings.FormSettings.Assign(Self);
end;

procedure TfrmCodeFilterDialog.FormShow(Sender: TObject);
begin
  edtFilter.SetFocus;
end;

function TfrmCodeFilterDialog.FTVPColumnDefinitionsItemsCustomDraw(
  Sender: TObject; ColumnDefinition: TColumnDefinition; Item: TObject;
  TargetCanvas: TCanvas; CellRect: TRect; ImageList: TCustomImageList;
  DrawMode: TDrawMode; Selected: Boolean): Boolean;
var
  L      : TLine;
  S      : string;
  Line   : string;
  P      : TPoint;
  A      : TSynHighlighterAttributes;
  R      : TRect;
  I      : Integer;
  Offset : Integer;
  Match  : string;
  C      : TColor;
  TS     : string;
begin
  Offset := 0;
  Match  := '';
  Result := True;
  if DrawMode = dmAfterCellPaint then
  begin
    L := TLine(Item);
    if ColumnDefinition.Name = 'Index' then
    begin
      S := IntToStr(L.Index + 1);
      R := CellRect;
      TargetCanvas.FillRect(R);
      FTextStyle.Alignment := taRightJustify;
      TargetCanvas.Font.Color := clGray;
      TargetCanvas.Font.Name := Manager.Settings.EditorFont.Name;
      R.Right := R.Right - 8;
      TargetCanvas.TextRect(R, R.Left, R.Top, S, FTextStyle);
    end
    else
    begin
      FTextStyle.Alignment := taLeftJustify;
      P.Y := L.Index + 1;
      R := CellRect;
      TargetCanvas.FillRect(R);
      // TODO: does not have any effect as tabs are reduced
      TS := '';
      TS := AddChar(' ', TS, Manager.Settings.EditorOptions.TabWidth);
      Line := StringReplace(L.Text, #9, TS, [rfReplaceAll]);
      if IsMatch(L.Text, Match, Offset) then
      begin
        R.Left := R.Left + TargetCanvas.TextWidth(System.Copy(L.Text, 1, Offset - 1));
        R.Right := R.Left + TargetCanvas.TextWidth(Match);
        TargetCanvas.Pen.Color := Manager.Settings.Colors.HighlightAllColor.FrameColor;
        TargetCanvas.Pen.Width := 1;
        C := ColorToRGB(TargetCanvas.Brush.Color);
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
            50,
            False
          );
        end;
        TargetCanvas.Brush.Color := C;
        TargetCanvas.Rectangle(R);
        R := CellRect;
      end;
      I := 0;
      while I <= Length(Line) do
      begin
        P.X := I;
        if Assigned(View.HighlighterItem.SynHighlighter) then
        begin
          if View.Editor.GetHighlighterAttriAtRowCol(P, S, A) then
          begin
            TargetCanvas.Brush.Color := A.Background;
            if A.Foreground <> clNone then // clNone will be painted as white
              TargetCanvas.Font.Color := A.Foreground
            else
              TargetCanvas.Font.Color := clBlack;
            TargetCanvas.Font.Style := A.Style;
          end
        end
        else  // A is not assigned => no highlighter
        begin
          S := Line;
          TargetCanvas.Brush.Color := clBlack;
          TargetCanvas.Font.Color  := clBlack;
          TargetCanvas.Font.Style := [];
          I := I + Length(S);
        end;
        if S = '' then
          I := I + 1
        else
        begin
          //TS := AddChar(' ', TS, Manager.Settings.TabWidth);
          TS := ' ';
          S := StringReplace(S, #9, TS, [rfReplaceAll]);
          TargetCanvas.TextRect(R, R.Left, R.Top, S, FTextStyle);
          R.Left := R.Left + TargetCanvas.TextWidth(S);
          I := I + Length(S);
        end;
      end;
    end;
  end;
end;

procedure TfrmCodeFilterDialog.FTVPFilter(Item: TObject; var Accepted: Boolean);
var
  L: TLine;
begin
  L := TLine(Item);
  Accepted := IsMatch(L.Text);
end;

procedure TfrmCodeFilterDialog.FTVPSelectionChanged(Sender: TObject);
begin
  tmrUpdate.Enabled := True;
end;

procedure TfrmCodeFilterDialog.FVSTKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
  begin
    Close;
  end
  else if Ord(Key) = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
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

procedure TfrmCodeFilterDialog.FVSTKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_MENU) and (Shift = []) then // only ALT pressed
  begin
    Key := 0;
  end;
end;

procedure TfrmCodeFilterDialog.tmrUpdateTimer(Sender: TObject);
begin
  FUpdateEditorView := True;
  tmrUpdate.Enabled := False;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmCodeFilterDialog.FillList(AStrings: TStrings);
var
  I: Integer;
begin
  FLines.Clear;
  for I := 0 to AStrings.Count - 1 do
  begin
    FLines.Add(TLine.Create(I, AStrings[I]));
  end;
end;

function TfrmCodeFilterDialog.IsMatch(const AString: string; var AMatch: string;
  var APos: Integer): Boolean;
begin
  Result := False;
  if Filter <> '' then
  begin
    if RegEx and FIsCompiled then
    begin
      Result := FRegExpr.Exec(AString);
      if Result then
      begin
        AMatch := FRegExpr.Match[0];
        APos := FRegExpr.MatchPos[0];
      end;
    end
    else
    begin
      APos := StrPos(Filter, AString, MatchCase);
      AMatch := System.Copy(AString, APos, Length(Filter));
      Result := APos > 0;
    end;
  end;
end;

function TfrmCodeFilterDialog.IsMatch(const AString: string): Boolean;
begin
  Result := False;
  if Filter <> '' then
  begin
    if RegEx and FIsCompiled then
    begin
      Result := FRegExpr.Exec(AString);
    end
    else
    begin
      Result := StrPos(Filter, AString, MatchCase) > 0;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmCodeFilterDialog.Modified;
begin
  FUpdate := True;
  FIsCompiled := False;
end;

procedure TfrmCodeFilterDialog.UpdateActions;
var
  L: TLine;
begin
  inherited UpdateActions;
  actApplyFilter.Enabled := RegEx;
  if FUpdate then
  begin
    UpdateView;
    ApplyFilter;
    FUpdate := False;
  end;
  if FUpdateEditorView then // update position in the editorview
  begin
    L := TLine(FTVP.SelectedItem);
    if Assigned(L) then
      View.SearchAndSelectLine(L.Index, L.Text);
    FUpdateEditorView := False;
  end;
end;

procedure TfrmCodeFilterDialog.UpdateStatusDisplay;
var
  N : Cardinal;
begin
  N := FTVP.VisibleCount;
  if N = 1 then
    sbrMain.SimpleText := SOneLineWithMatchFound
  else
    sbrMain.SimpleText := Format(SLinesWithMatchFound, [N]);
end;

procedure TfrmCodeFilterDialog.ApplyFilter;
var
  B: Boolean;
begin
  B := True;
  if RegEx and (Filter <> '') then
  begin
    try
      FRegExpr.Expression := Filter;
      FRegExpr.ModifierI := not MatchCase;
      FRegExpr.ModifierM := True;
      FRegExpr.Compile;
      FIsCompiled := True;
    except
      B := False;
      sbrMain.SimpleText := FRegExpr.ErrorMsg(FRegExpr.LastError);
    end;
  end;
  if B then
  begin
    sbrMain.SimpleText := '';
    FTVP.ApplyFilter;
    UpdateStatusDisplay;
  end;
end;

procedure TfrmCodeFilterDialog.UpdateView;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    { Remove trailing empty lines first. If we don't do this the last line is
      for an unknown reason shown twice in the grid after multiple filter
      operations. }
    SL.Text := TrimRight(View.Text);
    FillList(SL);
    FTVP.Refresh;
    UpdateStatusDisplay;
  finally
    SL.Free;
  end;
end;
{$ENDREGION}

end.

{
regular expressions to parse Pascal procedure and functions

rproc1 = "(?<!\w)procedure\s+[\w\s.]+;"
rproc2 = "(?<!\w)procedure\s+[\w\s.]+\([\w\s,.=':;$/*()]*?\)\s*;"

rfunc1 = "(?<!\w)function\s+[\w\s.]+:\s*\w+\s*;"
^function\s+[\w\s.]+:\s*\w+\s*;
rfunc2 = "(?<!\w)function\s+[\w\s.]+\([\w\s,.=':;$/*()]*?\)\s*:\s*\w+\s*;"


}
