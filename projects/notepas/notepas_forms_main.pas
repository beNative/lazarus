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

unit Notepas_Forms_Main;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ActnList, ExtCtrls, Menus,
  Buttons, StdCtrls,

  LResources,

  ts_components_docking, ts_components_docking_storage,
  // for debugging
  sharedloggerlcl, ipcchannel,

  SynEdit,

  ts_Editor_Interfaces;

{
  KNOWN PROBLEMS
    - Close all but current tab does not work in all cases
    - The highlighter of the selected view does not show correctly in the
      popup menu
    - idem for the folding level
    - Fold level does not work for XML
    - encoding support needs to be implemented
    - support for alternative line endings needs to be implemented
    - auto guess highlighter (make it a setting)
    - comment selection should use comment style of the currently selected
      highlighter
    - saving loading in different encodings
    - memory management in combination with the anchor docking

  TODO
    - Dequote lines in code shaper
    - cheat panel with shortcut/button overview for all supported actions

  IDEAS
    - surround with function for selected block (as in Notepad2)
    - duplicate lines
    - draw tables
    - status bar builder
}

//=============================================================================

type
  TfrmMain = class(TForm)
    {$region 'designer controls' /fold}
    aclMain               : TActionList;
    actClose              : TAction;
    actAbout              : TAction;
    actCloseAllOtherPages : TAction;
    actInspect            : TAction;
    actToggleMaximized    : TAction;
    btnEncoding           : TSpeedButton;
    btnFileName           : TSpeedButton;
    btnHighlighter        : TSpeedButton;
    btnLineBreakStyle     : TSpeedButton;
    btnSelectionMode      : TSpeedButton;
    imlMain               : TImageList;
    lblHeader             : TLabel;
    mnuMain               : TMainMenu;
    pnlSelectionMode      : TPanel;
    pnlTop                : TPanel;
    pnlTool               : TPanel;
    pnlLineBreakStyle     : TPanel;
    pnlFileName           : TPanel;
    pnlHighlighter        : TPanel;
    pnlEncoding           : TPanel;
    pnlModified           : TPanel;
    pnlViewerCount        : TPanel;
    pnlSize               : TPanel;
    pnlPosition           : TPanel;
    pnlInsertMode         : TPanel;
    pnlStatusBar          : TPanel;
    Shape1                : TShape;
    btnCloseToolView      : TSpeedButton;
    splVertical           : TSplitter;
    tlbMain               : TToolBar;
    {$endregion}

    {$region 'action handlers' /fold}
    procedure actAboutExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actToggleMaximizedExecute(Sender: TObject);
    {$endregion}

    {$region 'event handlers' /fold}
    procedure ActionListExecute(AAction: TBasicAction; var Handled: Boolean);
    procedure AHSActivateSite(Sender: TObject);
    procedure btnEncodingClick(Sender: TObject);
    procedure btnFileNameClick(Sender: TObject);
    procedure btnHighlighterClick(Sender: TObject);
    procedure btnLineBreakStyleClick(Sender: TObject);
    procedure btnSelectionModeClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShow(Sender: TObject);
    procedure frmMainActiveViewChange(Sender: TObject);
    procedure btnCloseToolViewClick(Sender: TObject);
    procedure TAnchorDockPageControlChanging(Sender: TObject; var AllowChange: Boolean);
    {$endregion}
  private
    {$region 'property access methods' /fold}
    function GetActions: IEditorActions;
    function GetEditor: IEditorView;
    function GetManager: IEditorManager;
    function GetMenus: IEditorMenus;
    function GetSettings: IEditorSettings;
    function GetViews: IEditorViews;
    {$endregion}

    procedure InitDebugAction(const AActionName: string);

    // event handlers
    procedure ENewFile(Sender: TObject; var AFileName: string;
      const AText: string);
    procedure EStatusChange(Sender: TObject; Changes: TSynStatusChanges);

  protected
    procedure AddDockingMenuItems;
    procedure AssignEvents;
    procedure ConfigureAvailableActions;
    procedure UpdateStatusBar;
    procedure UpdateCaptions;

    procedure DisplayToolForm(
            AAction   : TAction;
      const AFormName : string;
            ASetFocus : Boolean = False
    );

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure UpdateActions; override;

    function AddEditor(const AFileName: string): IEditorView;

    property Manager: IEditorManager
      read GetManager;

    property Editor: IEditorView
      read GetEditor;

    property Actions: IEditorActions
      read GetActions;

    property Views: IEditorViews
      read GetViews;

    property Menus: IEditorMenus
      read GetMenus;

    property Settings: IEditorSettings
      read GetSettings;
  end;

var
  frmMain: TfrmMain;

//*****************************************************************************

implementation

{$R *.lfm}

uses
  StrUtils, Windows, FileUtil, TypInfo,

  SynEditTypes,

  ts_Core_Utils, ts_Core_VersionInfo, ts_Core_Helpers,

  ts_Editor_Manager, ts_Editor_AboutDialog, ts_Editor_Helpers;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmMain.AfterConstruction;
var
  I : Integer;
  EV: IEditorEvents;
  V : IEditorView;
begin
  inherited AfterConstruction;
  Manager.PersistSettings := True;
  ConfigureAvailableActions;
  DockMaster.MakeDockSite(Self, [akTop, akBottom, akRight, akLeft], admrpChild);
  AddDockingMenuItems;
  AddStandardEditorToolbarButtons(tlbMain);
  AddStandardEditorMenus(mnuMain);
  if Settings.DebugMode then
  begin
    // for debugging
    Logger.MaxStackCount := 5; // more than 5 give problems when exception is raised when stackinfo is not available
    AddEditorDebugMenu(mnuMain);
  end;
  pnlViewerCount.Visible := Settings.DebugMode;

  if ParamCount > 0 then
  begin
    for I := 1 to Paramcount do
    begin
      if I = 1 then
        V := AddEditor(ParamStr(I))
      else
        AddEditor(ParamStr(I));
    end;
  end
  else
  begin
    V := AddEditor('<new>');
  end;
  EV := Manager.Events;
  Manager.OnActiveViewChange  := frmMainActiveViewChange;
  EV.OnStatusChange := EStatusChange;
  EV.OnNewFile := ENewFile;
  tlbMain.Parent := Self;
  pnlStatusBar.Parent := Self;
  pnlHighlighter.PopupMenu    := Menus.HighlighterPopupMenu;
  btnHighlighter.PopupMenu    := Menus.HighlighterPopupMenu;
  btnEncoding.PopupMenu       := Menus.EncodingPopupMenu;
  btnLineBreakStyle.PopupMenu := Menus.LineBreakStylePopupMenu;
  btnSelectionMode.PopupMenu  := Menus.SelectionModePopupMenu;
  Manager.Actions.ActionList.OnExecute  := ActionListExecute;

  SetWindowSizeGrip(pnlStatusBar.Handle, True);
  Manager.ActiveView := V;
  DoubleBuffered := True;
end;

procedure TfrmMain.BeforeDestruction;
begin
  Settings.FormSettings.Assign(Self);
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TfrmMain.GetManager: IEditorManager;
begin
  Result := EditorManager;
end;

function TfrmMain.GetActions: IEditorActions;
begin
  Result := Manager.Actions;
end;

function TfrmMain.GetEditor: IEditorView;
begin
  Result := Manager.ActiveView;
end;

function TfrmMain.GetMenus: IEditorMenus;
begin
  Result := Manager.Menus;
end;

function TfrmMain.GetSettings: IEditorSettings;
begin
  Result := Manager.Settings;
end;

function TfrmMain.GetViews: IEditorViews;
begin
  Result := Manager.Views;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// action handlers                                                       BEGIN
//*****************************************************************************

procedure TfrmMain.actToggleMaximizedExecute(Sender: TObject);
begin
  if WindowState = wsMaximized then
    WindowState := wsNormal
  else
    WindowState := wsMaximized;
end;

procedure TfrmMain.actCloseExecute(Sender: TObject);
begin
  if Settings.CloseWithESC then
    Close;
end;

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  ShowAboutDialog;
end;

//*****************************************************************************
// action handlers                                                         END
//*****************************************************************************

//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

{ TODO: needs to be refactored. }

procedure TfrmMain.ActionListExecute(AAction: TBasicAction; var Handled: Boolean);
var
  A : TAction;
  S : string;
begin
  A := TAction(AAction);
  S := A.Name;
  if S = 'actShapeCode' then
    DisplayToolForm(A, 'frmCodeShaper')
  else if S = 'actFind' then
    DisplayToolForm(A, 'frmSearchForm', True)
  else if S = 'actShowPreview' then
    DisplayToolForm(A, 'frmPreview')
  else if S = 'actTestForm' then
    DisplayToolForm(A, 'frmTest')
  else if S = 'actAlignSelection' then
    DisplayToolForm(A, 'frmAlignLines')
  else if S = 'actXMLTree' then
    DisplayToolForm(A, 'frmXmlTree');
{$region 'docking support' /fold}
/// below works to support docking toolforms!
{
  below works to support docking toolforms!
  ADHS: TAnchorDockHostSite;

  if AAction.Name = 'actFindNext' then
  begin
    Actions.FindReplaceDialog;
    DockMaster.MakeDockable(Actions.FindReplaceDialog);
    DockMaster.ManualDock(DockMaster.GetAnchorSite(Actions.FindReplaceDialog),Self, alRight);
  end;
  DockMaster.BeginUpdate;
  DockMaster.MakeDockable(CodeShaperForm.CodeShaperForm);
  ADHS := DockMaster.GetAnchorSite(CodeShaperForm.CodeShaperForm);
  ADHS.BeginUpdateLayout;
  DockMaster.ManualDock(ADHS, FToolForm, alLeft);
  ADHS.EndUpdateLayout;
  DockMaster.EndUpdate;
}
{$endregion}
end;

procedure TfrmMain.AHSActivateSite(Sender: TObject);
var
  AHS : TAnchorDockHostSite;
  C   : TControl;
begin
  AHS := TAnchorDockHostSite(Sender);
  if AHS.SiteType = adhstOneControl then
  begin
    C := DockMaster.GetControl(AHS);
    if C is IEditorView then
      (C as IEditorView).Activate;
  end;
end;

procedure TfrmMain.ENewFile(Sender: TObject; var AFileName: string;
  const AText: string);
begin
  if FileExists(AFileName) then
  begin
    AddEditor(AFileName);
  end
  else
  begin
    AddEditor('<new>').Text := AText;
    Editor.SetFocus;
  end;
end;

procedure TfrmMain.EStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if scModified in Changes then
  begin
    // TODO: draw icon in tabsheet indicating that there was a change
  end;
end;

procedure TfrmMain.btnEncodingClick(Sender: TObject);
begin
  btnEncoding.PopupMenu.PopUp;
end;

procedure TfrmMain.btnFileNameClick(Sender: TObject);
begin
  ExploreFile(Editor.FileName);
end;

procedure TfrmMain.btnHighlighterClick(Sender: TObject);
begin
  btnHighlighter.PopupMenu.PopUp;
end;

procedure TfrmMain.btnLineBreakStyleClick(Sender: TObject);
begin
  btnLineBreakStyle.PopupMenu.PopUp;
end;

procedure TfrmMain.btnSelectionModeClick(Sender: TObject);
begin
  btnSelectionMode.PopupMenu.PopUp;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := Actions['actExit'].Execute;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
var
  S : string;
  V : IEditorView;
  I : Integer;
begin
  if Assigned(Editor) then
    V := Editor;
  DisableAutoSizing;
  try
    for I := Low(FileNames) to High(FileNames) do
    begin
      S := FileNames[I];
      if I = Low(FileNames) then
        V := AddEditor(S)
      else
        AddEditor(S)
    end;
  finally
    EnableAutoSizing;
  end;
  V.Activate;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  BeginAutoSizing;
  Settings.FormSettings.AssignTo(Self);
  EndAutoSizing;
end;

procedure TfrmMain.frmMainActiveViewChange(Sender: TObject);
begin
  if Assigned(Editor) then
    DockMaster.MakeVisible(Editor.Form, True);
end;

procedure TfrmMain.btnCloseToolViewClick(Sender: TObject);
var
  I: Integer;
begin
  pnlTool.Visible := False;
  splVertical.Visible := False;
  for I := 0 to Manager.ToolViews.Count - 1 do
  begin
    Manager.ToolViews.Views[I].Visible := False;
  end;
end;

procedure TfrmMain.TAnchorDockPageControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  (Sender as TAnchorDockPageControl).GetActiveSite.Show;
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************

//*****************************************************************************
// private methods                                                       BEGIN
//*****************************************************************************

procedure TfrmMain.InitDebugAction(const AActionName: string);
begin
  Actions[AActionName].Enabled := Settings.DebugMode;
  Actions[AActionName].Visible := Settings.DebugMode;
end;

//*****************************************************************************
// private methods                                                         END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TfrmMain.AddDockingMenuItems;
var
  MI: TMenuItem;
  PPM: TPopupMenu;
begin
  PPM := DockMaster.GetPopupMenu;
  MI := TMenuItem.Create(PPM);
  MI.Action := Actions['actCloseOthers'];
  PPM.Items.Add(MI);
end;

procedure TfrmMain.AssignEvents;
var
  C: TComponent;
  I: Integer;
begin
  for I := 0 to DockMaster.ComponentCount - 1 do
  begin
    C := DockMaster.Components[I];
    if C is TAnchorDockPageControl then
      TAnchorDockPageControl(C).OnChanging := TAnchorDockPageControlChanging;
  end;
end;

{ Hide actions that are not (fully) implemented or supported. }

procedure TfrmMain.ConfigureAvailableActions;
begin
  // TODO: maintain a list with Available actions in the manager instance !!!
  InitDebugAction('actMonitorChanges');
  InitDebugAction('actShowActions');
  InitDebugAction('actLoadHighlighterFromFile');
  InitDebugAction('actInspect');
  InitDebugAction('actShapeCode');
  InitDebugAction('actSortSelection');
  InitDebugAction('actPrint');
  InitDebugAction('actPrintPreview');
  InitDebugAction('actPageSetup');
  //InitDebugAction('actExportToWiki');
  //InitDebugAction('actExportToHTML');
  //InitDebugAction('actExportToRTF');
  InitDebugAction('actInsertCharacterFromMap');

  //InitDebugAction('actCopyHTMLToClipboard');
  //InitDebugAction('actCopyWikiToClipboard');
  //InitDebugAction('actCopyRTFToClipboard');
  //InitDebugAction('actCopyHTMLTextToClipboard');
  //InitDebugAction('actCopyWikiTextToClipboard');
  //InitDebugAction('actCopyRTFTextToClipboard');
end;

procedure TfrmMain.UpdateStatusBar;
var
  S: string;
begin
  pnlPosition.Caption :=
    Format('%1d:%1d / %1d | %1d', [
      Editor.CaretX,
      Editor.CaretY,
      Editor.Lines.Count,
      Editor.SelStart
    ]);

  pnlViewerCount.Caption := IntToStr(Views.Count);
  pnlSize.Caption := FormatByteText(Editor.TextSize);

  if Assigned(Editor.HighlighterItem) then
    pnlHighlighter.Caption := Editor.HighlighterItem.Description;
  if Editor.InsertMode then
    pnlInsertMode.Caption := 'INS'
  else
    pnlInsertMode.Caption := 'OVR';
//  pnlSelectionMode.Caption := GetEnumName(TypeInfo(TSynSelectionMode), Ord(Editor.SelectionMode));
  btnFileName.Caption := Editor.FileName;
  btnFileName.Hint := Editor.FileName;
  pnlFileName.Caption := Editor.FileName;
  pnlEncoding.Caption := UpperCase(Editor.Encoding);
  pnlLineBreakStyle.Caption := Editor.LineBreakStyle;
  S := GetEnumName(TypeInfo(TSynSelectionMode), Ord(Editor.SelectionMode));
  S := System.Copy(S, 3, Length(S));
  pnlSelectionMode.Caption := S;
  pnlModified.Caption := IfThen(Editor.Modified, 'Modified', '');
  OptimizeWidth(pnlViewerCount);
  OptimizeWidth(pnlPosition);
  OptimizeWidth(pnlSize);
  OptimizeWidth(pnlHighlighter);
  OptimizeWidth(pnlEncoding);
  OptimizeWidth(pnlInsertMode);
  OptimizeWidth(pnlSelectionMode);
  OptimizeWidth(pnlFileName);
  OptimizeWidth(pnlLineBreakStyle);
  OptimizeWidth(pnlModified);
end;

procedure TfrmMain.UpdateCaptions;
var
  V: IEditorView;
  I: Integer;
begin
  for I := 0 to Views.Count - 1 do
  begin
    V := Views[I];
    V.Form.Caption := ExtractFileName(V.FileName);
  end;
end;

procedure TfrmMain.DisplayToolForm(AAction: TAction; const AFormName: string;
  ASetFocus: Boolean);
var
  TV : IEditorToolView;
begin
  TV := EditorManager.ToolViews[AFormName];
  if Assigned(TV) then
  begin
    lblHeader.Caption := TV.Form.Caption;
    AssignFormParent(TV.Form, pnlTool);
    TV.Visible := AAction.Checked;
    pnlTool.Visible := AAction.Checked;
    splVertical.Visible := AAction.Checked;
    if TV.Visible then
      TV.UpdateView;
    if ASetFocus and TV.Form.CanFocus then
      TV.Form.SetFocus;
  end
  else
    raise Exception.CreateFmt('Toolform with name %s does not exist!', [AFormName]);
end;

procedure TfrmMain.UpdateActions;
begin
  inherited UpdateActions;
  if Assigned(Editor) then
  begin
    UpdateStatusBar;
  end;
end;

{ Creates a new IEditorView instance for the given file. }

function TfrmMain.AddEditor(const AFileName: string): IEditorView;
var
  V: IEditorView;
  AHS: TAnchorDockHostSite;
begin
  DisableAutoSizing;
  try
    V := Views.Add('', AFileName);
    V.Form.DisableAutoSizing;
    try
      DockMaster.MakeDockable(V.Form);
      AHS := DockMaster.GetAnchorSite(V.Form);
      DockMaster.ManualDock(AHS, Self, alClient);
      AHS.Header.Visible := Views.Count > 1;
      AHS.Header.HeaderPosition := adlhpTop;
      AHS.OnActivateSite := AHSActivateSite;
      if FileExists(AFileName) then
      begin
        V.LoadFromFile(AFileName);
      end;
      V.OnDropFiles := FormDropFiles;
      V.Editor.PopupMenu := Menus.EditorPopupMenu;
      UpdateCaptions;
    finally
      V.Form.EnableAutoSizing;
    end;
  finally
    EnableAutoSizing;
  end;
  Result := V;
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

initialization
{$I notepas_forms_main.lrs}

  Logger.Channels.Add(TIPCChannel.Create);

end.

