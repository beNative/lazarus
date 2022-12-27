{
  Copyright (C) 2013-2023 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.ActionList.ToolView;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ActnList, ComCtrls, StdCtrls,
  Contnrs, ImgList, Graphics,

  VirtualTrees,

  ts.Core.TreeViewPresenter, ts.Core.ColumnDefinitions, ts.Core.DataTemplates,

  ts.Editor.Interfaces;

type

  { TfrmActionListView }

  TfrmActionListView = class(TForm, IEditorToolView)
    edtFilterActions : TEdit;
    pnlActions       : TPanel;
    pgcMain          : TPageControl;
    pnlEditorList    : TPanel;
    tsMouseActions   : TTabSheet;
    tsCommands       : TTabSheet;
    tsActions        : TTabSheet;

    procedure edtFilterActionsChange(Sender: TObject);
    procedure edtFilterActionsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtFilterActionsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    function FTVPActionsCustomDraw(Sender: TObject;
      ColumnDefinition: TColumnDefinition; Item: TObject;
      TargetCanvas: TCanvas; CellRect: TRect; ImageList: TCustomImageList;
      DrawMode: TDrawMode; Selected: Boolean): Boolean;
    procedure FTVPActionsFilter(Item: TObject; var Accepted: Boolean);
    procedure FVSTActionsKeyPress(Sender: TObject; var Key: char);

  private
    FVSTActions      : TVirtualStringTree;
    FVSTCommands     : TVirtualStringTree;
    FVSTMouseActions : TVirtualStringTree;
    FTVPActions      : TTreeViewPresenter;
    FTVPCommands     : TTreeViewPresenter;
    FTVPMouseActions : TTreeViewPresenter;
    FActionItems     : TObjectList;
    FKeyStrokeItems  : TObjectList;
    FMouseItems      : TObjectList;
    FVKPressed       : Boolean;
    FTextStyle       : TTextStyle;

    function GetFilter: string;
    function GetForm: TForm;
    function GetManager: IEditorManager;
    function GetName: string;
    procedure SetFilter(AValue: string);

  protected
    procedure CreateActionsView;
    procedure CreateCommandsView;
    procedure CreateMouseActionsView;
    procedure UpdateLists;

    property Manager: IEditorManager
      read GetManager;

    function IsMatch(const AString : string): Boolean; overload; inline;
    function IsMatch(
      const AString : string;
        var AMatch  : string;
        var APos    : Integer
    ): Boolean; overload; inline;

    { IEditorToolView }
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean); override;
    procedure UpdateView;

    property Visible: Boolean
      read GetVisible write SetVisible;

    property Name: string
      read GetName;

    property Form: TForm
      read GetForm;

    property Filter: string
      read GetFilter write SetFilter;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.lfm}

uses
  TypInfo, StrUtils, Variants,

  GraphUtil, LCLType,

  SynEditKeyCmds, SynEditMouseCmds,

  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}

  LCLProc, LCLIntf, LMessages,

  ts.Core.Value, ts.Core.ColumnDefinitionsDataTemplate, ts.Core.Utils,
  ts.Core.Helpers,

  ts.Editor.Utils;

resourcestring
  SName       = 'Name';
  SCategory   = 'Category';
  SCaption    = 'Caption';
  SShortcut   = 'Shortcut';
  SShortcut2  = 'Shortcut2';
  SHint       = 'Hint';
  SVisible    = 'Visible';
  SEnabled    = 'Enabled';
  SCommand    = 'Command';
  SButton     = 'Button';
  SShift      = 'Shift';
  SShiftMask  = 'ShiftMask';
  SClickCount = 'ClickCount';
  SClickDir   = 'ClickDir';
  SMoveCaret  = 'MoveCaret';

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
    VK_INSERT,
    VK_DELETE,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END
  ];

{$REGION 'TActionListTemplate'}
type

  { TActionListTemplate }

  TActionListTemplate = class(TColumnDefinitionsDataTemplate)
    function GetImageIndex(
      const Item        : TObject;
      const ColumnIndex : Integer
    ): Integer; override;
    function GetValueForProperty(
      const Item          : TObject;
      const APropertyName : string
    ): TValue; override;
    procedure SetValueForProperty(
      const Item: TObject;
      const APropertyName: string;
      const AValue: TValue
    ); override;
    procedure SetValue(const Item: TObject; const ColumnIndex: Integer;
      const Value: TValue); override;
  end;

function TActionListTemplate.GetImageIndex(const Item: TObject;
  const ColumnIndex: Integer): Integer;
var
  CD: TColumnDefinition;
begin
  Result := -1;
  if Assigned(Item) and Assigned(ColumnDefinitions)
    and (ColumnIndex < ColumnDefinitions.Count) and (ColumnIndex > -1) then
  begin
    CD := TColumnDefinition(ColumnDefinitions[ColumnIndex]);
    if CD.Caption = '' then
    begin
      if IsPublishedProp(Item, 'ImageIndex') then
        Result := GetPropValue(Item, 'ImageIndex')
      else
        Result := -1;
      end
    end;
end;

function TActionListTemplate.GetValueForProperty(const Item: TObject;
  const APropertyName: string): TValue;
begin
  Result := inherited GetValueForProperty(Item, APropertyName);
  if SameText(APropertyName, 'Shortcut') then
  begin
    if not Result.IsEmpty then
    begin
      Result := ShortCutToText(TShortCut(Result.AsInteger));
    end;
  end;
end;

procedure TActionListTemplate.SetValueForProperty(const Item: TObject;
  const APropertyName: string; const AValue: TValue);
begin
  inherited SetValueForProperty(Item, APropertyName, AValue);
end;

procedure TActionListTemplate.SetValue(const Item: TObject;
  const ColumnIndex: Integer; const Value: TValue);
begin
  inherited SetValue(Item, ColumnIndex, Value);
end;
{$ENDREGION}

{$REGION 'TKeyStrokeTemplate'}
type
  TKeyStrokeTemplate = class(TColumnDefinitionsDataTemplate)
    function GetValueForProperty(
      const Item          : TObject;
      const APropertyName : string
    ): TValue; override;
  end;

function TKeyStrokeTemplate.GetValueForProperty(const Item: TObject;
  const APropertyName: string): TValue;
var
  C: TSynEditorCommand;
  S : string;
begin
  Result := inherited GetValueForProperty(Item, APropertyName);
  if SameText(APropertyName, 'Command') then
  begin
    C := TSynEditorCommand(Integer(Result));
    S := EditorCommandToCodeString(C);
    if Length(S) > 0 then
      S[1] := LowerCase(S[1]);
    Result := S;
  end
  else if SameText(APropertyName, 'Hint') then
  begin
    C := TSynEditorCommand(Integer((Item as TSynEditKeyStroke).Command));
    Result := EditorCommandToDescrString(C);
  end
  else if AnsiMatchText(APropertyName, ['Shortcut', 'Shortcut2']) then
  begin
    Result := ShortCutToText(TShortCut(Result.AsInteger));
  end;
end;
{$ENDREGION}

{$REGION 'TMouseActionTemplate'}
type
  TMouseActionTemplate = class(TColumnDefinitionsDataTemplate)
    function GetValueForProperty(
      const Item          : TObject;
      const APropertyName : string
    ): TValue; override;
  end;

function TMouseActionTemplate.GetValueForProperty(const Item: TObject;
  const APropertyName: string): TValue;
var
  C: TSynEditorMouseCommand;
begin
  Result := inherited GetValueForProperty(Item, APropertyName);
  if SameText(APropertyName, 'Command') then
  begin
    C := TSynEditorMouseCommand(Integer(Result));
    Result := MouseCommandName(C);
  end;
end;
{$ENDREGION}

{$REGION 'TfrmActionListView'}
{$REGION 'construction and destruction'}
procedure TfrmActionListView.AfterConstruction;
begin
  inherited AfterConstruction;

  FActionItems    := TObjectList.Create(False);
  FKeyStrokeItems := TObjectList.Create(False);
  FMouseItems     := TObjectList.Create(False);
  CreateActionsView;
  CreateCommandsView;
  CreateMouseActionsView;

  FTextStyle.SingleLine := True;
  FTextStyle.Opaque := False;
  FTextStyle.ExpandTabs := False;
  FTextStyle.Wordbreak := False;
  FTextStyle.ShowPrefix := True;
  FTextStyle.Clipping := False;
  FTextStyle.SystemFont := False;
  FTextStyle.Alignment := taLeftJustify;
  FTextStyle.Layout := tlCenter;
end;

procedure TfrmActionListView.BeforeDestruction;
begin
  FreeAndNil(FActionItems);
  FreeAndNil(FKeyStrokeItems);
  FreeAndNil(FMouseItems);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmActionListView.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TfrmActionListView.GetForm: TForm;
begin
  Result := Self;
end;

function TfrmActionListView.GetFilter: string;
begin
  Result := edtFilterActions.Text;
end;

function TfrmActionListView.GetName: string;
begin
  Result := inherited Name;
end;

procedure TfrmActionListView.SetFilter(AValue: string);
begin
  if AValue <> Filter then
  begin
    edtFilterActions.Text := AValue;
  end;
end;

function TfrmActionListView.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

procedure TfrmActionListView.SetVisible(AValue: Boolean);
begin
  inherited SetVisible(AValue);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmActionListView.FormShow(Sender: TObject);
begin
  UpdateLists;
  edtFilterActions.SetFocus;
end;

function TfrmActionListView.FTVPActionsCustomDraw(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
var
  A      : TAction;
  Match  : string = '';
  Margin : Integer;
  Offset : Integer = 0;
  R      : TRect;
  S      : string;
  C      : TColor;
  OC     : TColor;
begin
  S := '';
  Result := True;
  if DrawMode = dmAfterCellPaint then
  begin
    A := TAction(Item);
    if ColumnDefinition.Name = 'Name' then
    begin
      S := A.Name;
    end
    else if ColumnDefinition.Name = 'Caption' then
    begin
      S := A.Caption;
    end;
    Margin := 4;
    Result := False;
    R := CellRect;
    TargetCanvas.FillRect(R);
    if IsMatch(S, Match, Offset) then
    begin
      R.Left := Margin + R.Left + TargetCanvas.TextWidth(System.Copy(S, 1, Offset - 1));
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
        );
      end
      else
      begin
        C := ColorAdjustLuma(
          Manager.Settings.Colors.HighlightAllColor.Background,
          50,
          False
        );
      end;
      OC := TargetCanvas.Brush.Color;
      TargetCanvas.Brush.Color := C;
      TargetCanvas.Rectangle(R);
      TargetCanvas.Brush.Color := OC;
    end;
    R := CellRect;
    TargetCanvas.TextRect(R, R.Left + Margin, R.Top, S, FTextStyle);
  end;
end;

procedure TfrmActionListView.FTVPActionsFilter(Item: TObject;
  var Accepted: Boolean);
var
  A: TAction;
begin
  A := TAction(Item);
  Accepted := IsMatch(A.Name) or IsMatch(A.Caption);
end;

procedure TfrmActionListView.edtFilterActionsChange(Sender: TObject);
begin
  FTVPActions.ApplyFilter;
end;

procedure TfrmActionListView.edtFilterActionsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
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

procedure TfrmActionListView.edtFilterActionsKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if FVKPressed and FVSTActions.Enabled then
  begin
    FVSTActions.Perform(LM_KEYDOWN, Key, 0);
    if Visible and FVSTActions.CanFocus then
      FVSTActions.SetFocus;
  end;
  FVKPressed := False;
end;

{ Handled when KeyPreview = True. }

procedure TfrmActionListView.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
  end
  else
    inherited;
end;

procedure TfrmActionListView.FVSTActionsKeyPress(Sender: TObject; var Key: char
  );
begin
  if Ord(Key) = VK_RETURN then
  begin
    ModalResult := mrOK;
  end
  else if Ord(Key) = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
  end
  else if not edtFilterActions.Focused then
  begin
    edtFilterActions.SetFocus;
    PostMessage(edtFilterActions.Handle, LM_CHAR, Ord(Key), 0);
    edtFilterActions.SelStart := Length(Filter);
    // required to prevent the invocation of accelerator keys!
    Key := #0;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmActionListView.CreateActionsView;
var
  CD : TColumnDefinitions;
begin
  FVSTActions := VST.Create(Self, pnlActions);
  FVSTActions.OnKeyPress := FVSTActionsKeyPress;

  FTVPActions := TTreeViewPresenter.Create(Self);
  FTVPActions.ListMode := True;
  FTVPActions.ListMode := False;
  FTVPActions.AllowMove := False;
  FTVPActions.SyncMode := True;
  FTVPActions.ImageList := Manager.Actions.ActionList.Images as TImageList;

  CD := FTVPActions.ColumnDefinitions;
  FTVPActions.ItemTemplate := TActionListTemplate.Create(CD);
  CD.AddColumn('Category', SCategory, dtString, 100);
  with CD.AddColumn('Name', SName, dtString, 150, 150, 200) do
  begin
    OnCustomDraw := FTVPActionsCustomDraw;
  end;
  CD.AddColumn('', dtString, 24);
  with CD.AddColumn('Caption', SCaption, dtString, 120, 100, 200) do
  begin
    OnCustomDraw := FTVPActionsCustomDraw;
  end;
  with CD.AddColumn('Shortcut', SShortcut, dtString, 100) do
  begin
    AllowEdit := False;
  end;
  with CD.AddColumn('Hint', SHint, dtString, 200, 200, 400) do
  begin
    AllowEdit := True;
  end;
  with CD.AddColumn('Visible', SVisible, dtString, 50) do
  begin
    ColumnType := TColumnType.ctCheckBox;
    AllowEdit := True;
  end;
  with CD.AddColumn('Enabled', SEnabled, dtString, 55) do
  begin
    ColumnType := TColumnType.ctCheckBox;
    AllowEdit := True;
  end;
  FTVPActions.OnFilter    := FTVPActionsFilter;
  FTVPActions.ItemsSource := FActionItems;
  FTVPActions.TreeView    := FVSTActions;
end;

procedure TfrmActionListView.CreateCommandsView;
var
  CD : TColumnDefinitions;
begin
  FVSTCommands := VST.Create(Self, tsCommands);
  FTVPCommands := TTreeViewPresenter.Create(Self);
  FTVPCommands.ListMode := True;
  CD := FTVPCommands.ColumnDefinitions;
  FTVPCommands.ItemTemplate := TKeyStrokeTemplate.Create(CD);
  CD.AddColumn('Command', SCommand, dtString, 200, 100, 400);
  CD.AddColumn('Shortcut', SShortcut, dtString, 120);
  CD.AddColumn('Shortcut2', SShortcut2, dtString, 120);
  CD.AddColumn('Hint', SHint, dtString, 200, 100, 600);
  FTVPCommands.ItemsSource := FKeyStrokeItems;
  FTVPCommands.TreeView    := FVSTCommands;
end;

procedure TfrmActionListView.CreateMouseActionsView;
var
  CD: TColumnDefinitions;
begin
  FVSTMouseActions := VST.Create(Self, tsMouseActions);
  FTVPMouseActions := TTreeViewPresenter.Create(Self);
  FTVPMouseActions.ListMode := True;
  CD := FTVPMouseActions.ColumnDefinitions;
  FTVPMouseActions.ItemTemplate := TMouseActionTemplate.Create(CD);
  CD.AddColumn('Command', SCommand, dtString, 200, 100, 400);
  CD.AddColumn('Button', SButton, dtString, 120);
  CD.AddColumn('Shift', SShift, dtString, 120);
  CD.AddColumn('ShiftMask', SShiftMask, dtString, 120);
  CD.AddColumn('ClickCount', SClickCount, dtNumeric, 100);
  CD.AddColumn('ClickDir', SClickDir, dtString, 100);
  with CD.AddColumn('MoveCaret', SMoveCaret, dtString, 100) do
  begin
    ColumnType := TColumnType.ctCheckBox;
  end;
  FTVPMouseActions.ItemsSource := FMouseItems;
  FTVPMouseActions.TreeView    := FVSTMouseActions;
end;

procedure TfrmActionListView.UpdateView;
begin
  FVSTActions.Invalidate;
  FVSTCommands.Invalidate;
  FVSTMouseActions.Invalidate;
end;

procedure TfrmActionListView.UpdateLists;
var
  K: TCollectionItem;
  A: TContainedAction;
begin
  FActionItems.Clear;
  FKeyStrokeItems.Clear;
  FMouseItems.Clear;
  for K in Manager.ActiveView.Editor.MouseActions do
    FMouseItems.Add(K);
  for K in Manager.ActiveView.Editor.Keystrokes do
    FKeyStrokeItems.Add(K);
  for A in Manager.Actions.ActionList do
    FActionItems.Add(A);
  FTVPActions.Refresh;
  FTVPCommands.Refresh;
  FTVPMouseActions.Refresh;
end;

function TfrmActionListView.IsMatch(const AString: string): Boolean;
begin
  if Filter = '' then
    Result := True
  else
    Result := StrPos(Filter, AString, False) > 0;
end;

function TfrmActionListView.IsMatch(const AString: string; var AMatch: string;
  var APos: Integer): Boolean;
var
  S : string;
begin
  APos   := 0;
  AMatch := '';
  Result := False;
  if Filter <> '' then
  begin
    // remove accelerator token
    S := StringReplace(AString, '&', '', [rfReplaceAll]);
    APos   := StrPos(Filter, S, False);
    AMatch := System.Copy(S, APos, Length(Filter));
    Result := APos > 0;
  end;
end;
{$ENDREGION}
{$ENDREGION}
end.

