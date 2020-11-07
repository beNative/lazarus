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

unit NotePas.Main.Form;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ActnList, ExtCtrls, Menus,
  Buttons, StdCtrls, Graphics,

  SynEdit, SynMacroRecorder,

  DefaultTranslator,

  UExceptionLogger,

  // for debugging
  ts.Core.Logger,

  ts.Components.Docking, ts.Components.Docking.Storage,
  ts.Components.UniqueInstance,

  ts.Editor.Interfaces;

{
  KNOWN PROBLEMS
    - saving loading in different encodings

  TODO
    - status bar factory for editor
}

type

  { TfrmMain }

  TfrmMain = class(TForm)
    {$REGION 'designer controls'}
    aclMain               : TActionList;
    actAbout              : TAction;
    actCloseToolview      : TAction;
    actCheckForNewVersion : TAction;
    btnEncoding           : TSpeedButton;
    btnFileName           : TSpeedButton;
    btnHighlighter        : TSpeedButton;
    btnLineBreakStyle     : TSpeedButton;
    btnMacro              : TSpeedButton;
    btnSelectionMode      : TSpeedButton;
    btnCurrentChar        : TSpeedButton;
    ExceptionLogger       : TExceptionLogger;
    imlMain               : TImageList;
    lblHeader             : TLabel;
    pnlMacro              : TPanel;
    pnlClientStatusBar    : TPanel;
    pnlToolBar            : TPanel;
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
    shpLine               : TShape;
    splVertical           : TSplitter;
    ToolBar2              : TToolBar;
    ToolButton1           : TToolButton;
    ToolButton2           : TToolButton;
    ToolButton3           : TToolButton;
    ToolButton4           : TToolButton;
    ToolButton5           : TToolButton;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actAboutExecute(Sender: TObject);
    procedure actCheckForNewVersionExecute(Sender: TObject);
    procedure actCloseToolviewExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure AHSActivateSite(Sender: TObject);
    procedure AHSClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure AHSShowModalFinished(Sender: TObject; AResult: Integer);
    procedure btnEncodingClick(Sender: TObject);
    procedure btnFileNameClick(Sender: TObject);
    procedure btnHighlighterClick(Sender: TObject);
    procedure btnLineBreakStyleClick(Sender: TObject);
    procedure btnSelectionModeClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormWindowStateChange(Sender: TObject);
    procedure UniqueInstanceOtherInstance(Sender: TObject; ParamCount: Integer;
      Parameters: array of string);
    procedure UniqueInstanceTerminateInstance(Sender: TObject);
    {$ENDREGION}
  private
    FSettings         : IEditorSettings;
    FManager          : IEditorManager;
    FMainToolbar      : TToolbar;
    FSelectionToolbar : TToolbar;
    FRightToolbar     : TToolbar;
    FMainMenu         : TMainMenu;
    FToolbarHostPanel : TPanel;
    FUniqueInstance   : TUniqueInstance;

    {$REGION 'property access methods'}
    function GetActions: IEditorActions;
    function GetEditor: IEditorView;
    function GetEvents: IEditorEvents;
    function GetManager: IEditorManager;
    function GetMenus: IEditorMenus;
    function GetSettings: IEditorSettings;
    function GetViews: IEditorViews;
    {$ENDREGION}

    procedure InitializeEvents;
    procedure InitializeControls;
    procedure InitDebugAction(const AActionName: string);

    // event handlers
    procedure EditorEventsHideEditorToolView(
      Sender          : TObject;
      AEditorToolView : IEditorToolView
    );
    procedure EditorEventsShowEditorToolView(
      Sender    : TObject;
      AToolView : IEditorToolView
    );
    procedure EditorEventsAddEditorView(
      Sender      : TObject;
      AEditorView : IEditorView
    );
    procedure EditorEventsOpenOtherInstance(
      Sender        : TObject;
      const AParams : array of string
    );
    procedure EditorEventsActiveViewChange(Sender: TObject);
    procedure EditorEventsStatusChange(
      Sender  : TObject;
      Changes : TSynStatusChanges
    );
    procedure EditorSettingsChangedHandler(Sender: TObject);
    procedure EditorEventsMacroStateChange(
      Sender : TObject;
      AState : TSynMacroState
    );

  protected
    procedure AddDockingMenuItems;
    procedure UpdateCaptions;
    procedure UpdateControls;
    procedure UpdateStatusBar;
    procedure UpdateEditorViewCaptions;

    procedure CheckForNewVersion;

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
  StrUtils, TypInfo, Dialogs,

  FileUtil,

  SynEditTypes,

  LCLTranslator,

  httpsend,

  ts.Core.VersionInfo, ts.Core.Utils, ts.Core.Helpers,

  ts.Editor.AboutDialog, ts.Editor.Resources, ts.Editor.Factories.Settings,
  ts.Editor.Factories,

  NotePas.Resources;

{$REGION 'construction and destruction'}
procedure TfrmMain.AfterConstruction;
var
  I : Integer;
  V : IEditorView;
  S : string;
begin
  inherited AfterConstruction;
  FUniqueInstance                     := TUniqueInstance.Create(Self);
  FUniqueInstance.Identifier          := ApplicationName;
  FUniqueInstance.OnOtherInstance     := UniqueInstanceOtherInstance;
  FUniqueInstance.OnTerminateInstance := UniqueInstanceTerminateInstance;

  FSettings := TEditorFactories.CreateSettings(Self);
  FSettings.FileName := 'notepas.xml';
  FSettings.Load;

  TEditorSettingsFactory.InitializeFoldHighlighters(FSettings.Highlighters);

  SetDefaultLang(FSettings.LanguageCode);

  FManager := TEditorFactories.CreateManager(
    Self,
    FSettings
  );
  FManager.PersistSettings := True;

  FUniqueInstance.Enabled := Settings.SingleInstance;

  FMainMenu := TEditorFactories.CreateMainMenu(Self, Actions, Menus);
  FMainToolbar := TEditorFactories.CreateMainToolbar(
    Self,
    pnlToolBar,
    Actions,
    Menus
  );
  FSelectionToolbar := TEditorFactories.CreateSelectionToolbar(
    Self,
    nil,
    Actions,
    Menus
  );
  FToolbarHostPanel := TPanel.Create(Self);
  FRightToolbar := TEditorFactories.CreateTopRightToolbar(
    Self,
    FToolbarHostPanel,
    Actions,
    Menus
  );
  InitializeControls;
  InitializeEvents;

  actCheckForNewVersion.ActionList := Manager.Actions.GetActionList;
  //AddToolButton(FMainToolbar, actCheckForNewVersion);

  if ParamCount > 0 then
  begin
    for I := 1 to Paramcount do
    begin
      S := ParamStr(I);
      //Logger.Send('ParamStr(%d)=%s', [I, S]);
      if I = 1 then
        V := Manager.OpenFile(S)
      else
        Manager.OpenFile(S);
    end;
  end;
  // if no view exists, create one
  if Manager.Views.Count = 0 then
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
{$ENDREGION}

{$REGION 'property access mehods'}
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
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  ShowAboutDialog;  // not shown -> manager shows about dialog for the moment
end;

procedure TfrmMain.actCheckForNewVersionExecute(Sender: TObject);
begin
  CheckForNewVersion;
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
{$ENDREGION}

{$REGION 'event handlers'}
{$REGION 'docking support'}
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
{$ENDREGION}
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

procedure TfrmMain.AHSClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
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

procedure TfrmMain.EditorEventsMacroStateChange(Sender: TObject;
  AState: TSynMacroState);
begin
  btnMacro.Glyph.Clear;
  case AState of
    msStopped:
      btnMacro.Action := Actions['actPlaybackMacro'];
    msRecording:
      btnMacro.Action := Actions['actRecordMacro'];
    msPlaying:
      btnMacro.Action := Actions['actRecordMacro'];
    msPaused:
      btnMacro.Action := Actions['actPlaybackMacro'];
  end;
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
  if Manager.Views.Count = 1 then  //for macos because mac open file parameter by DropFile event
  begin
    if (Manager.Views[0].FileName = SNewEditorViewFileName) and
       (Manager.Views[0].TextSize = 0) then
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
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmMain.InitializeEvents;
begin
  Settings.AddEditorSettingsChangedHandler(EditorSettingsChangedHandler);
  Events.AddOnActiveViewChangeHandler(EditorEventsActiveViewChange);
  Events.OnStatusChange       := EditorEventsStatusChange;
  Events.OnOpenOtherInstance  := EditorEventsOpenOtherInstance;
  Events.OnAddEditorView      := EditorEventsAddEditorView;
  Events.OnShowEditorToolView := EditorEventsShowEditorToolView;
  Events.OnHideEditorToolView := EditorEventsHideEditorToolView;
  Events.OnMacroStateChange   := EditorEventsMacroStateChange;
  OnDropFiles := FormDropFiles;
end;

procedure TfrmMain.InitializeControls;
begin
  DockMaster.MakeDockSite(Self, [akTop, akBottom, akRight, akLeft], admrpChild);
  AddDockingMenuItems;
  FMainToolbar.ShowHint       := True;
  FMainToolbar.Transparent    := True;
  FMainToolbar.Align          := alLeft;
  FMainToolbar.AutoSize       := True;
  FMainToolbar.DoubleBuffered := True;

  FToolbarHostPanel.Align      := alRight;
  FToolbarHostPanel.Parent     := pnlToolBar;
  FToolbarHostPanel.BevelInner := bvNone;
  FToolbarHostPanel.BevelOuter := bvNone;
  FToolbarHostPanel.Caption    := '';
  FToolbarHostPanel.Width      := FRightToolbar.ButtonCount * FRightToolbar.ButtonWidth;
  FToolbarHostPanel.Anchors    := [akRight, akTop];

  FRightToolbar.ShowHint       := True;
  FRightToolbar.Align          := alClient;
  FRightToolbar.AutoSize       := False;
  FRightToolbar.Transparent    := True;
  FRightToolbar.Wrapable       := False;
  FRightToolbar.DoubleBuffered := True;

  FMainToolbar.Align := alClient;

  FSelectionToolbar.Align          := alRight;
  FSelectionToolbar.Visible        := False;
  FSelectionToolbar.AutoSize       := True;
  FSelectionToolbar.Transparent    := True;
  FSelectionToolbar.DoubleBuffered := True;

  Settings.FormSettings.AssignTo(Self);
  UpdateControls;
  pnlHighlighter.PopupMenu    := Menus.HighlighterPopupMenu;
  btnHighlighter.PopupMenu    := Menus.HighlighterPopupMenu;
  btnEncoding.PopupMenu       := Menus.EncodingPopupMenu;
  btnLineBreakStyle.PopupMenu := Menus.LineBreakStylePopupMenu;
  btnSelectionMode.PopupMenu  := Menus.SelectionModePopupMenu;

  btnMacro.Action := Actions['actRecordMacro'];
  Actions['actRecordMacro'].Enabled := False;
  Actions['actRecordMacro'].Visible := False;
  DoubleBuffered := True;
end;

procedure TfrmMain.InitDebugAction(const AActionName: string);
begin
  Actions[AActionName].Enabled := Settings.DebugMode;
  Actions[AActionName].Visible := Settings.DebugMode;
end;
{$ENDREGION}

{$REGION 'protected methods'}
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
//  InitDebugAction('actShowPreview');
//  InitDebugAction('actShowStructureViewer');
//  InitDebugAction('actShowHexEditor');
//  InitDebugAction('actShowMiniMap');
//  InitDebugAction('actShowScriptEditor');
//  InitDebugAction('actRecordMacro');
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
    pnlPosition.Caption :=
      SLn + ': ' + Format('%1d', [Editor.CaretY]) + '   ' +
      SCol + ': ' + Format('%1d', [Editor.CaretX]) + '   ' +
      SSel + ': '+ Format('%1d|%1d', [Length(Editor.SelText),
      (SelText.Count)]);
    pnlViewerCount.Caption := IntToStr(Views.Count);
    pnlSize.Caption := SSize+': '+ FormatByteText(Editor.TextSize) + '   ' +
      SLines+': '+ Format('%1d', [Editor.Lines.Count]);
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
    //OptimizeWidth(pnlModified);
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

//procedure TfrmMain.AddToolButton(const AParent: TToolBar;
//  const AAction: TContainedAction);
//var
//  TB: TToolButton;
//begin
//  TB := TToolButton.Create(AParent);
//  TB.Parent := AParent;
//  begin
//    TB.Action := AAction;
//  end;
//end;

{ Just a proof of concept. Implementation pending. }

procedure TfrmMain.CheckForNewVersion;
var
  S  : string;
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    S := 'http://notepas.googlecode.com/svn/releases/version-i386-win32-win32.txt';
    if HttpGetText(S, SL) then
    begin
      if VersionInfo.ProductVersion <> Trim(SL.Text) then
        ShowMessage('New version released')
      else
        ShowMessage('Nothing released');
    end
    else
      ShowMessage('Checkversion failed.');
  finally
    FreeAndNil(SL);
  end;
end;

procedure TfrmMain.UpdateActions;
begin
  inherited UpdateActions;
  if Assigned(Manager) and Assigned(Manager.ActiveView) then
  begin
    UpdateCaptions;
    UpdateStatusBar;
    FSelectionToolbar.Parent  := Editor.Editor;
    FSelectionToolbar.Visible := Editor.SelAvail;
  end;
end;
{$ENDREGION}

end.
