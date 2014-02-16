{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

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

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ActnList, ExtCtrls, Menus,
  Buttons, StdCtrls,

  SynEdit,

  DefaultTranslator,

  ts.Components.UniqueInstance,

  // for debugging
  ts.Core.SharedLogger,

  ts.Components.Docking, ts.Components.Docking.Storage,

  ts.Editor.Interfaces, UExceptionLogger;

{
  KNOWN PROBLEMS
    - Close all but current tab does not work in all cases
    - saving loading in different encodings

  TODO
    - status bar factory for editor
}

type

  { TfrmMain }

  TfrmMain = class(TForm)
    {$region 'designer controls' /fold}
    aclMain               : TActionList;
    actAbout              : TAction;
    actCloseToolview: TAction;
    actCheckForNewVersion: TAction;
    btnEncoding           : TSpeedButton;
    btnFileName           : TSpeedButton;
    btnHighlighter        : TSpeedButton;
    btnLineBreakStyle     : TSpeedButton;
    btnSelectionMode      : TSpeedButton;
    btnCurrentChar        : TSpeedButton;
    ExceptionLogger: TExceptionLogger;
    imlMain               : TImageList;
    lblHeader             : TLabel;
    pnlToolClient         : TPanel;
    pnlSelectionMode      : TPanel;
    pnlCurrentChar        : TPanel;
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
    btnCloseToolView      : TSpeedButton;
    splVertical           : TSplitter;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    {$endregion}

    {$region 'action handlers' /fold}
    procedure actAboutExecute(Sender: TObject);
    procedure actCheckForNewVersionExecute(Sender: TObject);
    procedure actCloseToolviewExecute(Sender: TObject);
    {$endregion}

    {$region 'event handlers' /fold}
    procedure AHSActivateSite(Sender: TObject);
    procedure AHSShowModalFinished(Sender: TObject; AResult: Integer);
    procedure btnEncodingClick(Sender: TObject);
    procedure btnFileNameClick(Sender: TObject);
    procedure btnHighlighterClick(Sender: TObject);
    procedure btnLineBreakStyleClick(Sender: TObject);
    procedure btnSelectionModeClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormWindowStateChange(Sender: TObject);
    procedure UniqueInstanceOtherInstance(Sender: TObject; ParamCount: Integer;
      Parameters: array of string);
    procedure UniqueInstanceTerminateInstance(Sender: TObject);
    {$endregion}
  private
    FSettings : IEditorSettings;
    FManager  : IEditorManager;
    FMainToolbar      : TToolbar;
    FSelectionToolbar : TToolbar;
    FRightToolbar     : TToolbar;
    FMainMenu         : TMainMenu;
    FToolbarHostPanel : TPanel;
    FUniqueInstance   : TUniqueInstance;

    {$region 'property access methods' /fold}
    function GetActions: IEditorActions;
    function GetEditor: IEditorView;
    function GetEvents: IEditorEvents;
    function GetManager: IEditorManager;
    function GetMenus: IEditorMenus;
    function GetSettings: IEditorSettings;
    function GetViews: IEditorViews;
    {$endregion}

    procedure InitializeEvents;
    procedure InitializeControls;
    procedure InitDebugAction(const AActionName: string);

    // event handlers
    procedure EditorEventsHideEditorToolView(Sender: TObject; AEditorToolView: IEditorToolView);
    procedure EditorEventsShowEditorToolView(Sender: TObject; AToolView: IEditorToolView);
    procedure EditorEventsActiveViewChange(Sender: TObject);
    procedure EditorEventsAddEditorView(Sender: TObject; AEditorView: IEditorView);
    procedure EditorEventsStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure EditorEventsOpenOtherInstance(Sender: TObject; const AParams: array of string);
    procedure EditorSettingsChangedHandler(Sender: TObject);

  protected
    procedure AddDockingMenuItems;
    procedure UpdateCaptions;
    procedure UpdateControls;
    procedure UpdateStatusBar;
    procedure UpdateEditorViewCaptions;
    procedure AddToolButton(const AParent: TToolBar; const AAction : TContainedAction);

//    procedure CheckForNewVersion;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure UpdateActions; override;

    { This instance controls the editor views. }
    property Manager: IEditorManager
      read GetManager;

    { Shortcut to the manager's active editorview. A manager instance can only
      have one active view. }
    property Editor: IEditorView
      read GetEditor;

    { The list of all supported actions that can be executed on a editor view. }
    property Actions: IEditorActions
      read GetActions;

    { The list of available editor views that are maintained by the editor
      manager. }
    property Views: IEditorViews
      read GetViews;

    { Menu components to use in the user interface. }
    property Menus: IEditorMenus
      read GetMenus;

    { Collection of all persistable settings for a editor manager instance. }

    property Settings: IEditorSettings
      read GetSettings;

    { Set of events where the user interface can respond to. }
    property Events: IEditorEvents
      read GetEvents;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  StrUtils, FileUtil, TypInfo, Dialogs,

  SynEditTypes,

  httpsend,

  ts.Core.VersionInfo,

  ts.Core.Utils, ts.Core.Helpers,

  ts_Editor_AboutDialog,

  ts_Editor_Resources, ts.Editor.Factories.Settings, ts.Editor.Factories;

resourcestring
  SModified = 'Modified';
  SSize     = 'Size';
  SLines    = 'Lines';
  SLn       = 'Ln';
  SCol      = 'Col';
  SSel      = 'Sel';

{$region 'construction and destruction' /fold}
procedure TfrmMain.AfterConstruction;
var
  I  : Integer;
  V  : IEditorView;
  S  : string;
begin
  inherited AfterConstruction;
  FUniqueInstance                     := TUniqueInstance.Create(Self);
  FUniqueInstance.Identifier          := ApplicationName;
  FUniqueInstance.OnOtherInstance     := UniqueInstanceOtherInstance;
  FUniqueInstance.OnTerminateInstance := UniqueInstanceTerminateInstance;
  FSettings := TEditorFactories.CreateSettings(Self);
  FSettings.FileName := 'settings.xml';
  FSettings.Load;
  TEditorSettingsFactory.InitializeFoldHighlighters(FSettings.Highlighters);
  SetDefaultLang(FSettings.LanguageCode);
  Logger.Send('SetDefaultLang to ' + FSettings.LanguageCode);
  FManager := TEditorFactories.CreateManager(
    Self,
    FSettings
  );
  FManager.PersistSettings := True;
  FUniqueInstance.Enabled := Settings.SingleInstance;
  FMainMenu := TEditorFactories.CreateMainMenu(Self, Actions, Menus);
  FMainToolbar :=
    TEditorFactories.CreateMainToolbar(Self, Self, Actions, Menus);
  FSelectionToolbar :=
    TEditorFactories.CreateSelectionToolbar(Self, nil, Actions, Menus);
  FToolbarHostPanel := TPanel.Create(Self);
  FToolbarHostPanel.Parent := FMainToolbar;
  FRightToolbar :=
    TEditorFactories.CreateTopRightToolbar(Self, FToolbarHostPanel, Actions, Menus);
  InitializeControls;
  InitializeEvents;
  actCheckForNewVersion.ActionList := Manager.Actions.GetActionList;
  AddToolButton(FMainToolbar, actCheckForNewVersion);
  if ParamCount > 0 then
  begin
    for I := 1 to Paramcount do
    begin
      S := ParamStr(I);
      if I = 1 then
        V := Manager.OpenFile(S)
      else
        Manager.OpenFile(S);
    end;
  end;
  if (ParamCount = 0) or (Manager.Views.Count=0) then  //if no exists views, create one
  begin
    V := Manager.NewFile(SNewEditorViewFileName);
  end;
  Manager.ActiveView := V;
end;

procedure TfrmMain.BeforeDestruction;
begin
  Settings.FormSettings.Assign(Self);
  FSettings := nil;
  FManager  := nil;
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'property access mehods' /fold}
function TfrmMain.GetManager: IEditorManager;
begin
  Result :=  FManager;
end;

function TfrmMain.GetActions: IEditorActions;
begin
  Result := Manager.Actions;
end;

function TfrmMain.GetEditor: IEditorView;
begin
  Result := Manager.ActiveView;
end;

function TfrmMain.GetEvents: IEditorEvents;
begin
  Result := Manager.Events;
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
{$endregion}

{$region 'action handlers' /fold}
procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  ShowAboutDialog;  // not shown -> manager shows about dialog for the moment
end;

procedure TfrmMain.actCheckForNewVersionExecute(Sender: TObject);
begin
//  CheckForNewVersion;
end;

procedure TfrmMain.actCloseToolviewExecute(Sender: TObject);
var
  TV: IEditorToolView;
begin
  pnlTool.Visible     := False;
  splVertical.Visible := False;
  for TV in Manager.ToolViews do
    TV.Visible := False;
  Manager.ActiveView.SetFocus;
end;
{$endregion}

{$region 'event handlers' /fold}
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
procedure TfrmMain.AHSActivateSite(Sender: TObject);
var
  AHS : TAnchorDockHostSite;
  C   : TControl;
  EV  : IEditorView;
begin
  AHS := TAnchorDockHostSite(Sender);
  if AHS.SiteType = adhstOneControl then
  begin
    C := DockMaster.GetControl(AHS);
    if C is IEditorView then
    begin
      EV := C as IEditorView;
      EV.Activate;
      EV.SetFocus;
    end;
  end;
end;

procedure TfrmMain.AHSShowModalFinished(Sender: TObject; AResult: Integer);
begin
  Editor.SetFocus;
end;

procedure TfrmMain.EditorEventsStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if scModified in Changes then
  begin
    // TODO: draw icon in tabsheet indicating that there was a change
  end;
  if (scTopLine in Changes) and FSelectionToolbar.Visible then
    FSelectionToolbar.Invalidate;
end;

procedure TfrmMain.btnEncodingClick(Sender: TObject);
begin
  btnEncoding.PopupMenu.PopUp;
end;

procedure TfrmMain.btnFileNameClick(Sender: TObject);
begin
{ TODO -oTS : Make this work in a cross platform fashion }
{$IFDEF Windows}
  ExploreFile(Editor.FileName);
{$ENDIF}
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

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  Logger.Send('Form OnActivate');
end;

procedure TfrmMain.EditorEventsActiveViewChange(Sender: TObject);
begin
  if Assigned(Editor) then
    DockMaster.MakeVisible(Editor.Form, True);
end;

procedure TfrmMain.EditorEventsOpenOtherInstance(Sender: TObject;
  const AParams: array of string);
var
  I : Integer;
  S : string;
  V : IEditorView;
begin
  for I := Low(AParams) to High(AParams) do
  begin
    S := AParams[I];
    if I = Low(AParams) then
      V := Manager.OpenFile(S)
    else
      Manager.OpenFile(S);
    V.Activate;
  end;
end;

procedure TfrmMain.EditorSettingsChangedHandler(Sender: TObject);
begin
  WindowState := Settings.FormSettings.WindowState;
  FormStyle   := Settings.FormSettings.FormStyle;
  UpdateControls;
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
  {$IFDEF DARWIN}
  if (Manager.Views.Count=1) then  //for macos because mac open file parameter by DropFile event
  begin
      if (Manager.Views[0].FileName=SNewEditorViewFileName) and
         (Manager.Views[0].TextSize=0) then
         Manager.Views[0].Close;
  end;
  {$ENDIF}
  try
    for I := Low(FileNames) to High(FileNames) do
    begin
      S := FileNames[I];
      if I = Low(FileNames) then
        V := Manager.OpenFile(S)
      else
        Manager.OpenFile(S)
    end;
  finally
    EnableAutoSizing;
  end;
  V.Activate;
  V.SetFocus;
end;

procedure TfrmMain.FormWindowStateChange(Sender: TObject);
begin
  Settings.FormSettings.WindowState := WindowState;
end;

procedure TfrmMain.UniqueInstanceOtherInstance(Sender: TObject;
  ParamCount: Integer; Parameters: array of string);
begin
  Events.DoOpenOtherInstance(Parameters);
end;

procedure TfrmMain.UniqueInstanceTerminateInstance(Sender: TObject);
begin
  // prevents that the second instance overwrites settings.
  Manager.PersistSettings := False;
end;

procedure TfrmMain.EditorEventsHideEditorToolView(Sender: TObject;
  AEditorToolView: IEditorToolView);
begin
  pnlTool.Visible     := False;
  splVertical.Visible := False;
end;

procedure TfrmMain.EditorEventsShowEditorToolView(Sender: TObject;
  AToolView: IEditorToolView);
begin
  pnlTool.Visible     := False;
  lblHeader.Caption   := AToolView.Form.Caption;
  pnlTool.Width       := AToolView.Form.Width;
  splVertical.Visible := True;
  AssignFormParent(AToolView.Form, pnlToolClient);
  pnlTool.Visible := True;
end;

procedure TfrmMain.EditorEventsAddEditorView(Sender: TObject; AEditorView: IEditorView);
var
  V   : IEditorView;
  AHS : TAnchorDockHostSite;
begin
  DisableAutoSizing;
  try
    V := AEditorView;
    V.IsFile := True;
    V.Form.DisableAutoSizing;
    try
      DockMaster.MakeDockable(V.Form);
      AHS := DockMaster.GetAnchorSite(V.Form);
      if Assigned(AEditorView.MasterView) then
      begin
        V.Form.Width := Self.Width div 2;
        DockMaster.ManualDock(
          AHS,
          DockMaster.GetAnchorSite(V.MasterView.Form),
          alRight,
          AEditorView.MasterView.Form
        );
        AEditorView.MasterView.Form.Width := Self.Width div 2;
      end
      else
      begin
        DockMaster.ManualDock(AHS, Self, alClient);
        AHS.Header.Visible := Views.Count > 1;
        AHS.Header.HeaderPosition := adlhpTop;
      end;
      AHS.OnActivateSite := AHSActivateSite;
      AHS.OnShowModalFinished := AHSShowModalFinished;
      V.OnDropFiles      := FormDropFiles;
      V.Editor.PopupMenu := Menus.EditorPopupMenu;
      UpdateEditorViewCaptions;
    finally
      V.Form.EnableAutoSizing;
    end;
  finally
    EnableAutoSizing;
  end;
end;
{$endregion}

{$region 'private methods' /fold}
procedure TfrmMain.InitializeEvents;
begin
  Settings.AddEditorSettingsChangedHandler(EditorSettingsChangedHandler);
  Events.AddOnActiveViewChangeHandler(EditorEventsActiveViewChange);
  Events.OnStatusChange       := EditorEventsStatusChange;
  Events.OnOpenOtherInstance  := EditorEventsOpenOtherInstance;
  Events.OnAddEditorView      := EditorEventsAddEditorView;
  Events.OnShowEditorToolView := EditorEventsShowEditorToolView;
  Events.OnHideEditorToolView := EditorEventsHideEditorToolView;
  OnDropFiles := FormDropFiles;
end;

procedure TfrmMain.InitializeControls;
begin
  DockMaster.MakeDockSite(Self, [akTop, akBottom, akRight, akLeft], admrpChild);
  AddDockingMenuItems;
  FMainToolbar.ShowHint := True;
  FMainToolbar.Transparent := True;
  FMainToolbar.AutoSize := True;
  FMainToolbar.DoubleBuffered := True;

  FToolbarHostPanel.Parent := FMainToolbar;
  FToolbarHostPanel.Align := alRight;
  FToolbarHostPanel.BevelInner := bvNone;
  FToolbarHostPanel.BevelOuter := bvNone;
  FToolbarHostPanel.Caption := '';
  FToolbarHostPanel.Width := FRightToolbar.ButtonCount * FRightToolbar.ButtonWidth;

  FRightToolbar.ShowHint := True;
  FRightToolbar.Align := alTop;
  FRightToolbar.AutoSize := False;
  FRightToolbar.Transparent := True;
  FRightToolbar.Wrapable := False;
  FRightToolbar.DoubleBuffered := True;

  FSelectionToolbar.Align := alRight;
  FSelectionToolbar.Visible := False;
  FSelectionToolbar.AutoSize := True;
  FSelectionToolbar.Transparent := True;
  FSelectionToolbar.DoubleBuffered := True;

  Settings.FormSettings.AssignTo(Self);
  UpdateControls;
  pnlHighlighter.PopupMenu    := Menus.HighlighterPopupMenu;
  btnHighlighter.PopupMenu    := Menus.HighlighterPopupMenu;
  btnEncoding.PopupMenu       := Menus.EncodingPopupMenu;
  btnLineBreakStyle.PopupMenu := Menus.LineBreakStylePopupMenu;
  btnSelectionMode.PopupMenu  := Menus.SelectionModePopupMenu;
  DoubleBuffered := True;
end;

procedure TfrmMain.InitDebugAction(const AActionName: string);
begin
  Actions[AActionName].Enabled := Settings.DebugMode;
  Actions[AActionName].Visible := Settings.DebugMode;
end;
{$endregion}

{$region 'protected methods' /fold}
procedure TfrmMain.AddDockingMenuItems;
var
  MI  : TMenuItem;
  PPM : TPopupMenu;
begin
  PPM := DockMaster.GetPopupMenu;
  MI := TMenuItem.Create(PPM);
  MI.Action := Actions['actCloseOthers'];
  PPM.Items.Add(MI);
end;

procedure TfrmMain.UpdateCaptions;
var
  S : string;
begin
  S := Application.Title;
  if FileExists(Editor.FileName) then
    Caption := Format('%s - %s',  [Editor.FileName, S])
  else
    Caption := S;
end;

procedure TfrmMain.UpdateControls;
begin
  pnlViewerCount.Visible := Settings.DebugMode;
  InitDebugAction('actMonitorChanges');
  InitDebugAction('actShowActions');
  InitDebugAction('actInspect');
  InitDebugAction('actSortSelection');
  InitDebugAction('actPrint');
  InitDebugAction('actPrintPreview');
  InitDebugAction('actPageSetup');
  InitDebugAction('actNewSharedView');
  InitDebugAction('actShowPreview');
  InitDebugAction('actShowHTMLViewer');
  InitDebugAction('actShowStructureViewer');
  InitDebugAction('actShowHexEditor');
  InitDebugAction('actShowMiniMap');
  InitDebugAction('actShowScriptEditor');
  //InitDebugAction('actExecuteScriptOnSelection');
end;

procedure TfrmMain.UpdateStatusBar;
var
  S : string;
  SelText: TStringList;
begin
  SelText:= TStringList.Create;
  try
    SelText.Text:=Editor.SelText;
    pnlPosition.Caption := SLn+': '+Format('%1d', [Editor.CaretY])+'   '+
                           SCol+': '+Format('%1d', [Editor.CaretX])+'   '+
                           SSel+': '+ Format('%1d|%1d', [length(Editor.SelText),
         (SelText.Count)]);

    pnlViewerCount.Caption := IntToStr(Views.Count);
    pnlSize.Caption := SSize+': '+FormatByteText(Editor.TextSize)+'   '+SLines+': '+
                       Format('%1d', [Editor.Lines.Count]);

    if Assigned(Editor.HighlighterItem) then
      btnHighlighter.Caption := Editor.HighlighterItem.Description;
    if Editor.InsertMode then
      pnlInsertMode.Caption := 'INS'
    else
      pnlInsertMode.Caption := 'OVR';
    btnFileName.Caption := Editor.FileName;
    btnFileName.Hint := Editor.FileName;
    btnEncoding.Caption := UpperCase(Editor.Encoding);
    btnLineBreakStyle.Caption := Editor.LineBreakStyle;
    S := GetEnumName(TypeInfo(TSynSelectionMode), Ord(Editor.SelectionMode));
    S := System.Copy(S, 3, Length(S));
    btnSelectionMode.Caption := S;
    btnCurrentChar.Caption := HexDisplayPrefix + IntToHex(Ord(Editor.CurrentChar), 4);
    pnlModified.Caption := IfThen(Editor.Modified, SModified, '');
    OptimizeWidth(pnlViewerCount);
    OptimizeWidth(pnlPosition);
    OptimizeWidth(pnlSize);
    OptimizeWidth(pnlInsertMode);
    OptimizeWidth(pnlModified);
    pnlCurrentChar.Width :=
     GetTextWidth(btnCurrentChar.Caption, btnCurrentChar.Font) + 10;
    pnlHighlighter.Width :=
      GetTextWidth(btnHighlighter.Caption, btnHighlighter.Font) + 10;
    pnlEncoding.Width := GetTextWidth(btnEncoding.Caption, btnEncoding.Font) + 10;
    pnlSelectionMode.Width :=
      GetTextWidth(btnSelectionMode.Caption, btnSelectionMode.Font) + 10;
    pnlFileName.Width := GetTextWidth(btnFileName.Caption, btnFileName.Font) + 10;
    pnlLineBreakStyle.Width :=
      GetTextWidth(btnLineBreakStyle.Caption, btnLineBreakStyle.Font) + 10;
  finally
    SelText.Free;
  end;
end;

procedure TfrmMain.UpdateEditorViewCaptions;
var
  V : IEditorView;
  I : Integer;
begin
  for I := 0 to Views.Count - 1 do
  begin
    V := Views[I];
    V.Form.Caption := ExtractFileName(V.FileName);
  end;
end;

procedure TfrmMain.AddToolButton(const AParent: TToolBar;
  const AAction: TContainedAction);
var
  TB: TToolButton;
begin
  TB := TToolButton.Create(AParent);
  TB.Parent := AParent;
  begin
    TB.Action := AAction;
  end;
end;

//procedure TfrmMain.CheckForNewVersion;
//var
//  S  : string;
//  SL : TStringList;
//begin
//  SL := TStringList.Create;
//  try
//    S := 'http://notepas.googlecode.com/svn/releases/version-i386-win32-win32.txt';
//    if HttpGetText(S, SL) then
//    begin
//      if VersionInfo.ProductVersion <> Trim(SL.Text) then
//        ShowMessage('New version released')
//      else
//        ShowMessage('Nothing released');
//    end
//    else
//      ShowMessage('Checkversion failed.');
//  finally
//    FreeAndNil(SL);
//  end;
//end;

procedure TfrmMain.UpdateActions;
begin
  inherited UpdateActions;
  if Assigned(Manager) and Assigned(Manager.ActiveView) then
  begin
    UpdateCaptions;
    UpdateStatusBar;
    FSelectionToolbar.Parent  := Editor.Editor;
    FSelectionToolbar.Visible := Editor.SelAvail;

    { TODO -oTS : For some unknown reason the form is sometimes focused when
      multiple views are closed. This is a temporary work-around till the real
      nature of the problem is identified. The anchordocking control might be
      responsible for this behaviour. }
    if Screen.ActiveControl = Self then
      Editor.SetFocus;
  end;
end;
{$endregion}

end.
