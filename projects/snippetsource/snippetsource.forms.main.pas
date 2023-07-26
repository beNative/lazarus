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

unit SnippetSource.Forms.Main;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, ComCtrls, ActnList,
  StdCtrls, Menus, Buttons, DB,
  LazFileUtils, FileUtil,

  VirtualTrees, MenuButton,

  ts.Core.VersionInfo,
  ts.Editor.Interfaces, ts.Editor.Highlighters, ts.Editor.Factories,
  ts.RichEditor.Interfaces, ts.HtmlEditor.Interfaces,

  SnippetSource.Forms.Lookup, SnippetSource.Forms.VirtualDBTree,
  SnippetSource.Interfaces, SnippetSource.Settings, SnippetSource.Forms.Busy;

type
  TfrmMain = class(TForm)
    {$REGION 'designer controls'}
    aclMain                         : TActionList;
    actAbout                        : TAction;
    actCloseRichEditorToolView      : TAction;
    actConsole                      : TAction;
    actExecute                      : TAction;
    actCloseEditorToolView          : TAction;
    actHtmlEditor                   : TAction;
    actTextEditor                   : TAction;
    actRtfEditor                    : TAction;
    actLookup                       : TAction;
    actSettings                     : TAction;
    actShowGridForm                 : TAction;
    actSQLEditor                    : TAction;
    actToggleFullScreen             : TAction;
    actToggleStayOnTop              : TAction;
    btnCloseEditorToolView          : TSpeedButton;
    btnCloseRichEditorToolView      : TSpeedButton;
    btnHighlighter                  : TMenuButton;
    btnImage                        : TSpeedButton;
    btnLineBreakStyle               : TSpeedButton;
    dscMain                         : TDatasource;
    edtTitle                        : TEdit;
    imlMain                         : TImageList;
    imlNodes                        : TImageList;
    lblEditorToolViewHeader         : TLabel;
    lblRichEditorToolViewHeader     : TLabel;
    lblWelcome                      : TLabel;
    nbRight                         : TNotebook;
    pnlToolBarHost                  : TPanel;
    pnlHtmlEditorToolBar            : TPanel;
    pnlHtmlEditor                   : TPanel;
    pgTextEditor                    : TPage;
    pgRichEditor                    : TPage;
    pgHtmlEditor                    : TPage;
    pnlCapsLock                     : TPanel;
    pnlDateCreated                  : TPanel;
    pnlDateModified                 : TPanel;
    pnlEditMode                     : TPanel;
    pnlEditor                       : TPanel;
    pnlEditorToolBar                : TPanel;
    pnlEditorToolViewHost           : TPanel;
    pnlEditorToolViewHostHeader     : TPanel;
    pnlRichEditorToolBar            : TPanel;
    pnlId                           : TPanel;
    pnlLeft                         : TPanel;
    pnlLineBreakStyle               : TPanel;
    pnlNumLock                      : TPanel;
    pnlPosition                     : TPanel;
    pnlRichEditor                   : TPanel;
    pnlRichEditorToolViewHost       : TPanel;
    pnlRichEditorToolViewHostHeader : TPanel;
    pnlRight                        : TPanel;
    pnlSize                         : TPanel;
    pnlSnippetCount                 : TPanel;
    pnlStatusBar                    : TPanel;
    pnlStatusBarCenter              : TPanel;
    pnlTitle                        : TPanel;
    pnlWelcome                      : TPanel;
    shpLine1                        : TShape;
    shpTextEditorToolBarLine        : TShape;
    shpHtmlEditorToolBarLine        : TShape;
    shpRichEditorToolBarLine        : TShape;
    splEditorVertical               : TSplitter;
    splRichEditorVertical           : TSplitter;
    splVertical                     : TSplitter;
    tlbApplication                  : TToolBar;
    tlbEditorView                   : TToolBar;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actAboutExecute(Sender: TObject);
    procedure actCloseEditorToolViewExecute(Sender: TObject);
    procedure actCloseRichEditorToolViewExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure actHtmlEditorExecute(Sender: TObject);
    procedure actRtfEditorExecute(Sender: TObject);
    procedure actSQLEditorExecute(Sender: TObject);
    procedure actLookupExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actShowGridFormExecute(Sender: TObject);
    procedure actTextEditorExecute(Sender: TObject);
    procedure actToggleFullScreenExecute(Sender: TObject);
    procedure actToggleStayOnTopExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure btnHighlighterMouseEnter(Sender: TObject);
    procedure btnHighlighterMouseLeave(Sender: TObject);
    procedure btnLineBreakStyleClick(Sender: TObject);
    procedure dscMainDataChange(Sender: TObject; Field: TField);
    procedure edtTitleChange(Sender: TObject);
    procedure edtTitleEditingDone(Sender: TObject);
    procedure edtTitleEnter(Sender: TObject);
    procedure edtTitleExit(Sender: TObject);
    procedure edtTitleMouseEnter(Sender: TObject);
    procedure edtTitleMouseLeave(Sender: TObject);
    procedure FileSearcherDirectoryFound(FileIterator: TFileIterator);
    procedure FileSearcherFileFound(FileIterator: TFileIterator);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    {$ENDREGION}

  private
    FTree               : TfrmVirtualDBTree;
    FBusyForm           : TfrmBusy;
    FLastId             : Integer;
    FVersionInfo        : TVersionInfo;
    FFileSearcher       : TFileSearcher;
    FParentId           : Integer;
    FCommonPath         : string;
    FData               : IDataSet;
    FEditorManager      : IEditorManager;
    FEditorSettings     : IEditorSettings;
    FEditor             : IEditorView;
    FRichEditorManager  : IRichEditorManager;
    FRichEditor         : IRichEditorView;
    FHtmlEditorManager  : IHtmlEditorManager;
    FHtmlEditor         : IHtmlEditorView;
    FSettings           : TSettings;
    FRichEditorToolBar  : TToolBar;
    FHtmlEditorToolBar  : TToolBar;
    FUpdate             : Boolean;
    FRtfStream          : TStringStream;
    FHtmlStream         : TStringStream;
    FUpdateRichTextView : Boolean;

    {$REGION 'event handlers'}
    procedure FEditorChange(Sender: TObject);
    procedure FEditorFormEnter(Sender: TObject);
    procedure FEditorHighlighterChange(Sender: TObject);
    procedure FEditorBeforeSave(
      Sender           : TObject;
      var AStorageName : string
    );
    procedure FEditorNew(
      Sender        : TObject;
      var AFileName : string;
      const AText   : string
    );
    procedure FEditorHideToolView(
      Sender    : TObject;
      AToolView : IEditorToolView
    );
    procedure FEditorShowToolView(
      Sender    : TObject;
      AToolView : IEditorToolView
    );
    procedure FHtmlEditorAfterCreated(Sender: TObject);
    procedure FHtmlEditorChange(Sender: TObject);
    procedure FHtmlEditorContentLoaded(Sender: TObject);
    procedure FHtmlEditorNavigationCompleted(Sender: TObject);
    procedure FRichEditorChange(Sender: TObject);
    procedure FRichEditorDropFiles(
      Sender           : TObject;
      const AFileNames : array of string
    );
    procedure FRichEditorFormEnter(Sender: TObject);
    procedure FRichEditorHideToolView(
      Sender    : TObject;
      AToolView : IRichEditorToolView
    );
    procedure FRichEditorShowToolView(
      Sender    : TObject;
      AToolView : IRichEditorToolView
    );

    procedure FTreeDropFiles(
      Sender      : TBaseVirtualTree;
      AFiles      : TStrings;
      AAttachMode : TVTNodeAttachMode
    );
    procedure FTreeDuplicateSelectedNodes(Sender: TObject);
    procedure FTreeNewFolderNode(Sender: TObject);
    procedure FTreeNewItemNode(Sender: TObject);
    procedure FTreeDeleteSelectedNodes(Sender: TObject);
    procedure FTreeCopyNodeData(Sender: TObject);
    {$ENDREGION}

    {$REGION 'property access methods'}
    function GetConnection: IConnection;
    function GetDataSet: IDataSet;
    function GetEditor: IEditorView;
    function GetHtmlEditor: IHtmlEditorView;
    function GetQuery: IQuery;
    function GetRichEditor: IRichEditorView;
    function GetSnippet: ISnippet;
    {$ENDREGION}

    procedure AssignTextEditorChanges;
    procedure BuildTextEditorToolBar;
    procedure BuildApplicationToolBar;
    procedure UpdateApplicationToolBar;
    procedure HideAction(const AActionName: string);
    procedure InitTextEditorActions;

    procedure SwitchView;

    procedure CreateTreeview;
    procedure CreateTextEditor;
    procedure CreateRichEditor;
    procedure CreateHtmlEditor;

    procedure InitializeLogger;

    procedure Modified;

    function FileExtensionToHighlighter(const AFileExtension: string): string;

    procedure LoadRtfData;
    procedure SaveRtfData;

    procedure LoadHtmlData;
    procedure SaveHtmlData;

    procedure AddPathNode(
      const APath       : string;
      const ACommonPath : string;
      ATree             : TBaseVirtualTree
    );
    procedure AddDirectoryNode(
      const APath       : string;
      const ACommonPath : string;
      ATree             : TBaseVirtualTree
    );
    // not in use?
    procedure ExportNode;

  protected
    procedure UpdateActions; override;
    procedure UpdateStatusBar;
    procedure UpdateViews;
    procedure UpdateToolBars;

  public
     procedure AfterConstruction; override;
     destructor Destroy; override;

     property Connection : IConnection
       read GetConnection;

     property Snippet : ISnippet
       read GetSnippet;

     property DataSet : IDataSet
       read GetDataSet;

     property Query: IQuery
       read GetQuery;

     property Editor : IEditorView
       read GetEditor;

     property RichEditor : IRichEditorView
       read GetRichEditor;

     property HtmlEditor : IHtmlEditorView
       read GetHtmlEditor;
  end; 

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  StrUtils, Base64, Dialogs,
  LclIntf, LclType,

  ts.Core.Utils, ts.Core.Logger, ts.Core.Helpers,
  ts.Core.Logger.Channel.IPC,

  ts.Editor.AboutDialog,

  ts.Richeditor.Factories, ts.HtmlEditor.Factories,

  SnippetSource.Forms.SettingsDialog, SnippetSource.Modules.Data,
  SnippetSource.Forms.Query, SnippetSource.Forms.Grid, SnippetSource.Resources,
  SnippetSource.Modules.Terminal;

{$REGION 'construction and destruction'}
procedure TfrmMain.AfterConstruction;
begin
  inherited AfterConstruction;
  FSettings := TSettings.Create(Self);
  FSettings.Load;

  InitializeLogger;

  FData := TdmSnippetSource.Create(Self, FSettings);
  FBusyForm := TfrmBusy.Create(Self);
  FBusyForm.Visible := False;

  pnlId.Font.Color           := clRed;
  pnlDateCreated.Font.Color  := clBlue;
  pnlDateModified.Font.Color := clGreen;

  CreateTextEditor;
  CreateRichEditor;
  // TODO page must be active to create the HtmlEditor correctly!
  nbRight.PageIndex := 2;
  CreateHtmlEditor;
  CreateTreeview;
  FRtfStream := TStringStream.Create;
  FHtmlStream := TStringStream.Create;

  dscMain.DataSet := DataSet.DataSet;
  if FSettings.LastFocusedId > 0 then
  begin
    try
      DataSet.DataSet.Locate('Id', FSettings.LastFocusedId, []);
    except
      ShowMessage(SLastFocusedRecordError);
    end;
  end;
  FHtmlEditor.SourceVisible := FSettings.HtmlSourceVisible;
  FHtmlEditor.EditMode      := FSettings.HtmlEditMode;

  FFileSearcher := TFileSearcher.Create;
  FFileSearcher.OnDirectoryFound := FileSearcherDirectoryFound;
  FFileSearcher.OnFileFound      := FileSearcherFileFound;

  BuildApplicationToolBar;
  btnLineBreakStyle.PopupMenu := FEditorManager.Menus.LineBreakStylePopupMenu;

  FVersionInfo := TVersionInfo.Create(Self);
  Caption := Format('%s %s', [ApplicationName, FVersionInfo.ProductVersion]);
end;

destructor TfrmMain.Destroy;
begin
  Logger.Enter(Self, 'Destroy');
  FBusyForm.Free;
  FSettings.LastFocusedId     := Snippet.Id;
  FSettings.HtmlEditMode      := FHtmlEditor.EditMode;
  FSettings.HtmlSourceVisible := FHtmlEditor.SourceVisible;
  FSettings.Save;
  FEditorSettings.Save;
  FData              := nil;
  FRichEditorManager := nil;
  FEditorManager     := nil;
  FHtmlEditorManager := nil;
  FEditorSettings    := nil;
  FEditor            := nil;
  FRichEditor        := nil;
  FHtmlEditor        := nil;
  FreeAndNil(FFileSearcher);
  FreeAndNil(FRtfStream);
  FreeAndNil(FHtmlStream);
  inherited Destroy;
  Logger.Leave(Self, 'Destroy');
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmMain.GetRichEditor: IRichEditorView;
begin
  Result := FRichEditor;
end;

function TfrmMain.GetEditor: IEditorView;
begin
  Result := FEditor;
end;

function TfrmMain.GetHtmlEditor: IHtmlEditorView;
begin
  Result := FHtmlEditor;
end;

function TfrmMain.GetQuery: IQuery;
begin
  Result := FData as IQuery;
end;

function TfrmMain.GetConnection: IConnection;
begin
  Result := FData as IConnection;
end;

function TfrmMain.GetDataSet: IDataSet;
begin
  Result := FData as IDataSet;
end;

function TfrmMain.GetSnippet: ISnippet;
begin
  Result := FData as ISnippet;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmMain.actLookupExecute(Sender: TObject);
begin
  Lookup(Editor, FData as ILookup);
end;

procedure TfrmMain.actSettingsExecute(Sender: TObject);
begin
  ExecuteSettingsDialog(FData, FSettings);
  FData.DataSet.Refresh;
  UpdateApplicationToolBar;
  Modified;
end;

procedure TfrmMain.actShowGridFormExecute(Sender: TObject);
begin
  ShowGridForm(DataSet.DataSet);
end;

procedure TfrmMain.actTextEditorExecute(Sender: TObject);
begin
  nbRight.PageIndex := pgTextEditor.PageIndex;
end;

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  ShowAboutDialog;
end;

procedure TfrmMain.actCloseEditorToolViewExecute(Sender: TObject);
var
  TV : IEditorToolView;
begin
  pnlEditorToolViewHost.Visible     := False;
  splEditorVertical.Visible := False;
  for TV in FEditorManager.ToolViews do
    TV.Visible := False;
  FEditorManager.ActiveView.SetFocus;
end;

procedure TfrmMain.actCloseRichEditorToolViewExecute(Sender: TObject);
var
  TV : IRichEditorToolView;
begin
  pnlRichEditorToolViewHost.Visible     := False;
  splRichEditorVertical.Visible := False;
  for TV in FRichEditorManager.ToolViews do
    TV.Visible := False;
  FRichEditorManager.ActiveView.SetFocus;
end;

procedure TfrmMain.actExecuteExecute(Sender: TObject);
var
  FS        : TFileStream;
  LFileName : string;
const
  SCRIPT_FILE = 'SnippetSource.%s';
  COMMAND    = '.\%s\Scripts\activate.bat & py "SnippetSource.PY"';
begin
  LFileName := Format(SCRIPT_FILE, [Snippet.Highlighter]);
  Logger.Send('FileName', LFileName);
  FS := TFileStream.Create(LFileName, fmCreate);
  try
    Editor.SaveToStream(FS);
  finally
    FS.Free;
  end;
  if Snippet.Highlighter = 'BAT' then
  begin
    dmTerminal.Execute(LFileName);
  end
  else if Snippet.Highlighter = 'PY' then
  begin
    dmTerminal.Execute(Format(COMMAND, [FSettings.PythonVirtualEnvironmentName]));
  end;
end;

procedure TfrmMain.actHtmlEditorExecute(Sender: TObject);
begin
  nbRight.PageIndex := pgHtmlEditor.PageIndex;
end;

procedure TfrmMain.actRtfEditorExecute(Sender: TObject);
begin
  nbRight.PageIndex := pgRichEditor.PageIndex;
end;

procedure TfrmMain.actSQLEditorExecute(Sender: TObject);
var
  F : TfrmQuery;
begin
  F := TfrmQuery.Create(Self, FEditorManager, FData as IQuery);
  try
    F.Showmodal;
    DataSet.DataSet.Refresh;
  finally
    F.Free;
  end;
end;

procedure TfrmMain.actToggleFullScreenExecute(Sender: TObject);
begin
  if WindowState = wsMaximized then
    WindowState := wsNormal
  else
    WindowState := wsMaximized;
end;

procedure TfrmMain.actToggleStayOnTopExecute(Sender: TObject);
begin
  if (Sender as TAction).Checked then
    FormStyle := fsSystemStayOnTop
  else
    FormStyle := fsNormal;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMain.btnHighlighterMouseEnter(Sender: TObject);
begin
  btnHighlighter.Transparent := False;
end;

procedure TfrmMain.btnHighlighterMouseLeave(Sender: TObject);
begin
  btnHighlighter.Transparent := True;
end;

procedure TfrmMain.btnLineBreakStyleClick(Sender: TObject);
begin
  btnLineBreakStyle.PopupMenu.PopUp;
end;

procedure TfrmMain.dscMainDataChange(Sender: TObject; Field: TField);
var
  //LStream : TMemoryStream;
  LBitmap : TBitmap;
begin
  LBitmap := TBitmap.Create;
  if Field = nil then // browse record
  begin
    if Assigned(DataSet.DataSet) then
    begin
      if DataSet.DataSet.State = dsBrowse then
      begin
        FParentId := Snippet.ID;
        Editor.BeginUpdate; // avoid FEditorChange getting triggered
        try
          Editor.Text := Snippet.Text;
          if Snippet.Highlighter <> '' then
          begin
            Editor.HighlighterName := Snippet.Highlighter;
          end;
        finally
          Editor.EndUpdate;
        end;
        edtTitle.Text := Snippet.NodeName;
        edtTitle.Hint := Snippet.NodeName;
        (FData as IGlyphs).ImageList.GetBitmap(Snippet.ImageIndex, LBitmap);
        btnImage.Glyph.Assign(LBitmap);
        //LStream := TMemoryStream.Create;
        //(FData.DataSet.FieldByName('Image') as TBlobField).SaveToStream(LStream);
        //LStream.Position := 0;
     //btnImage.Glyph.LoadFromStream(LStream);
      //  LStream.Free;
        btnHighlighter.Caption := Snippet.Highlighter;
        //FTextEditorVisible := False;
        //FRichEditorVisible := False;
        LoadRtfData;
        LoadHtmlData;
        Modified;
        SwitchView;
      end;
    end;
  end;
  LBitmap.Free;
end;

{$REGION 'FEditor'}
procedure TfrmMain.FEditorChange(Sender: TObject);
begin
  Logger.Enter(Self, 'EChange');
  DataSet.Edit;
  AssignTextEditorChanges;
  Logger.Leave(Self, 'EChange');
end;

procedure TfrmMain.FEditorFormEnter(Sender: TObject);
begin
  Editor.Actions.ActionList.State := asNormal;
end;

procedure TfrmMain.FEditorBeforeSave(Sender: TObject; var AStorageName: string);
var
  FS : TFileStream;
begin
  DataSet.Post;
  FS := TFileStream.Create(Snippet.NodeName, fmCreate);
  try
    Editor.SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TfrmMain.FEditorHighlighterChange(Sender: TObject);
begin
  Logger.Enter(Self, 'EHighlighterChange');
  if Snippet.Highlighter <> Editor.HighlighterName then
  begin
    DataSet.Edit;
    AssignTextEditorChanges;
  end;
  Logger.Leave(Self, 'EHighlighterChange');
end;

procedure TfrmMain.FEditorNew(Sender: TObject; var AFileName: string; const AText: string);
begin
  Logger.Enter(Self, 'ENew');
  FTree.NewSubItemNode;
  Editor.Text := AText;
  AssignTextEditorChanges;
  Logger.Leave(Self, 'ENew');
end;
{$ENDREGION}

{$REGION 'edtTitle'}
procedure TfrmMain.edtTitleChange(Sender: TObject);
var
  LEdit : TEdit absolute Sender;
begin
  if Snippet.NodeName <> LEdit.Text then
    DataSet.Edit;
end;

procedure TfrmMain.edtTitleEditingDone(Sender: TObject);
var
  LEdit : TEdit absolute Sender;
begin
  DataSet.Edit;
  Snippet.NodeName := LEdit.Text;
  LEdit.Hint    := LEdit.Text;
end;

procedure TfrmMain.edtTitleEnter(Sender: TObject);
begin
  DataSet.Edit;
end;

procedure TfrmMain.edtTitleMouseEnter(Sender: TObject);
var
  LEdit : TEdit absolute Sender;
begin
  LEdit.Color      := clWhite;
  LEdit.Font.Color := clBlack;
end;

procedure TfrmMain.edtTitleExit(Sender: TObject);
var
  LEdit : TEdit absolute Sender;
begin
  LEdit.Color      := clForm;
  LEdit.Font.Color := clDkGray;
end;

procedure TfrmMain.edtTitleMouseLeave(Sender: TObject);
var
  LEdit : TEdit absolute Sender;
begin
  if not LEdit.Focused then
  begin
    LEdit.Color      := clForm;
    LEdit.Font.Color := clDkGray;
    Editor.SetFocus;
  end;
end;
{$ENDREGION}

{$REGION 'FileSearcher'}
procedure TfrmMain.FileSearcherDirectoryFound(FileIterator: TFileIterator);
begin
  AddDirectoryNode(FileIterator.FileName, FCommonPath, FTree.TreeView);
end;

procedure TfrmMain.FileSearcherFileFound(FileIterator: TFileIterator);
begin
  AddPathNode(FileIterator.FileName, FCommonPath, FTree.TreeView);
end;
{$ENDREGION}

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  dmTerminal.prcTerminal.Active := False;
end;

{$REGION 'FTree'}
procedure TfrmMain.FTreeDeleteSelectedNodes(Sender: TObject);
begin
  DataSet.ApplyUpdates;
end;

procedure TfrmMain.FTreeCopyNodeData(Sender: TObject);
begin
  Editor.CopyAllToClipboard;
end;

procedure TfrmMain.FTreeDropFiles(Sender: TBaseVirtualTree;
  AFiles: TStrings; AAttachMode: TVTNodeAttachMode);
var
  I     : Integer;
  T     : Integer;
  LFile : string;
begin
  FBusyForm.Visible := True;
  FBusyForm.Repaint;
  Cursor := crHourGlass;
  FCommonPath := GetCommonPath(AFiles);
  Logger.Watch('FCommonPath', FCommonPath);
  FLastId := Query.LastId;
  T := Snippet.NodeTypeId;
  if T = 1 then // FOLDER
  begin
    FParentId := Snippet.Id;
  end
  else
  begin
    FParentId := Snippet.ParentId;
  end;
  DataSet.DisableControls;
  Connection.BeginBulkInserts;
  try
    for I := 0 to AFiles.Count - 1 do
    begin
      LFile := AFiles[I];
      Logger.Send('LFile', LFile);
      if DirectoryExists(LFile) then // add files in directory
      begin
        AddDirectoryNode(LFile, FCommonPath, Sender);
        FFileSearcher.Search(LFile);
      end
      else
      begin
        AddPathNode(LFile, FCommonPath, Sender);
      end;
    end;
  finally
    Connection.EndBulkInserts;
    DataSet.DataSet.Refresh;
    DataSet.EnableControls;
    Sender.Refresh;
  end;
  Cursor := crDefault;
  FBusyForm.Visible := False;
end;

procedure TfrmMain.FTreeDuplicateSelectedNodes(Sender: TObject);
var
  LValues : TStrings;
begin
  LValues := TStringList.Create;
  try
    FTree.RetrieveSelectedNodeIds(LValues);
    Snippet.DuplicateRecords(LValues);
  finally
    LValues.Free;
  end;
end;

procedure TfrmMain.FTreeNewFolderNode(Sender: TObject);
begin
  DataSet.Edit;
  Snippet.ImageIndex := 1;
  edtTitle.SelectAll;
  edtTitle.SetFocus;
end;

procedure TfrmMain.FTreeNewItemNode(Sender: TObject);
begin
  DataSet.Edit;
  Snippet.ImageIndex := 2;
  edtTitle.SelectAll;
  edtTitle.SetFocus;
end;
{$ENDREGION}

{$REGION 'FRichEditor'}
procedure TfrmMain.FRichEditorChange(Sender: TObject);
begin
  if DataSet.RecordCount > 0 then
  begin
    SaveRtfData;
  end;
end;

procedure TfrmMain.FRichEditorDropFiles(Sender: TObject;
  const AFileNames: array of string);
var
  S : string;
begin
  for S in AFileNames do
  begin
    RichEditor.InsertImageFile(S);
    Logger.IncCounter('DroppedCount');
  end;
  SaveRtfData;
end;

procedure TfrmMain.FRichEditorFormEnter(Sender: TObject);
begin
  FEditor.Actions.ActionList.State := asSuspended;
end;

procedure TfrmMain.FRichEditorHideToolView(Sender: TObject;
  AToolView: IRichEditorToolView);
begin
//
end;

procedure TfrmMain.FRichEditorShowToolView(Sender: TObject;
  AToolView: IRichEditorToolView);
begin
  pnlRichEditorToolViewHost.Visible   := False;
  pnlRichEditorToolViewHost.Width     := AToolView.Form.Width;
  lblRichEditorToolViewHeader.Caption := AToolView.Form.Caption;
  splRichEditorVertical.Visible       := True;
  AssignFormParent(AToolView.Form, pnlRichEditorToolViewHost);
  pnlRichEditorToolViewHost.Visible   := True;
end;

procedure TfrmMain.FEditorHideToolView(Sender: TObject;
  AToolView: IEditorToolView);
begin
  //
end;

procedure TfrmMain.FEditorShowToolView(Sender: TObject;
  AToolView: IEditorToolView);
begin
  //pnlEditorToolViewHost.Visible   := False;
  //pnlEditorToolViewHost.Width     := AToolView.Form.Width;
  //lblEditorToolViewHeader.Caption := AToolView.Form.Caption;
  //splEditorVertical.Visible       := True;
  //AssignFormParent(AToolView.Form, pnlEditorToolViewHost);
  //pnlEditorToolViewHost.Visible   := True;
end;

procedure TfrmMain.FHtmlEditorAfterCreated(Sender: TObject);
begin
  nbRight.PageIndex := 0;
end;

procedure TfrmMain.FHtmlEditorChange(Sender: TObject);
begin
  if DataSet.RecordCount > 0 then
  begin
    SaveHtmlData;
  end;
end;

procedure TfrmMain.FHtmlEditorContentLoaded(Sender: TObject);
begin
  //if FUpdate then

    SwitchView;
end;

procedure TfrmMain.FHtmlEditorNavigationCompleted(Sender: TObject);
begin
  //if FUpdate then
  SwitchView;
  if (Snippet.NodeName.IsEmpty) or (Snippet.NodeName = 'New') then
  begin
    DataSet.Edit;
    Snippet.NodeName := FHtmlEditor.DocumentTitle;
    edtTitle.Text    := Snippet.NodeName;
  end;
end;

{$ENDREGION}
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmMain.AssignTextEditorChanges;
begin
  Snippet.Text        := Editor.Text;
  Snippet.FoldLevel   := Editor.FoldLevel;
  Snippet.Highlighter := Editor.HighlighterName;
  Logger.Send('Editor.HighlighterName', Editor.HighlighterName);
  if Snippet.Highlighter = '' then
    Snippet.Highlighter := 'TXT';
end;

procedure TfrmMain.BuildTextEditorToolBar;
var
  AL : TActionList;
begin
  AL := FEditorManager.Actions.ActionList;
  tlbEditorView.Images  := AL.Images;
  AddToolBarButton(tlbEditorView, AL, 'actSave');
  AddToolBarButton(tlbEditorView, AL, 'actSaveAs');
  AddToolBarButton(tlbEditorView);
  AddToolBarButton(tlbEditorView, AL, 'actCut');
  AddToolBarButton(tlbEditorView, AL, 'actCopy');
  AddToolBarButton(tlbEditorView, AL, 'actPaste');
  AddToolBarButton(tlbEditorView);
  AddToolBarButton(tlbEditorView, AL, 'actUndo');
  AddToolBarButton(tlbEditorView, AL, 'actRedo');
  AddToolBarButton(tlbEditorView);
  AddToolBarButton(tlbEditorView, AL, 'actSearch');
  AddToolBarButton(tlbEditorView, AL, 'actSearchReplace');
  AddToolBarButton(tlbEditorView);
  AddToolBarButton(tlbEditorView, AL, 'actShowCodeShaper');
  AddToolBarButton(tlbEditorView, AL, 'actShowCodeFilter');
  AddToolBarButton(tlbEditorView, AL, 'actShowCharacterMap');
  AddToolBarButton(tlbEditorView);
  AddToolBarButton(tlbEditorView, AL, 'actShowSpecialCharacters');
  AddToolBarButton(tlbEditorView, AL, 'actSortSelection');
  AddToolBarButton(tlbEditorView);
  AddToolBarButton(tlbEditorView, AL, 'actSelectAll');
  AddToolBarButton(tlbEditorView, AL, 'actCopyAllToClipboard');
  AddToolBarButton(tlbEditorView, AL, 'actClear');
  AddToolBarButton(tlbEditorView);
  AddToolBarButton(
    tlbEditorView,
    AL,
    'actToggleFoldLevel',
    False,
    FEditorManager.Menus.FoldPopupMenu
  );
  AddToolBarButton(
    tlbEditorView,
    AL,
    'actToggleHighlighter',
    False,
    FEditorManager.Menus.HighlighterPopupMenu
  );
  AddToolBarButton(tlbEditorView);
  AddToolBarButton(tlbEditorView, AL, 'actSettings');
end;

procedure TfrmMain.BuildApplicationToolBar;
begin
  tlbApplication.Images := imlMain;
  AddToolBarButton(tlbApplication, actTextEditor, True);
  AddToolBarButton(tlbApplication, actRtfEditor, True);
  AddToolBarButton(tlbApplication, actHtmlEditor, True);
  AddToolBarButton(tlbApplication);
  AddToolBarButton(tlbApplication, actLookup);
  if FSettings.DebugMode then
  begin
    AddToolBarButton(tlbApplication, actSQLEditor);
    AddToolBarButton(tlbApplication, actShowGridForm);
  end;
  AddToolBarButton(tlbApplication, actExecute);
  AddToolBarButton(tlbApplication, actSettings);
  AddToolBarButton(tlbApplication, actAbout);
  AddToolBarButton(tlbApplication, actToggleStayOnTop);
end;

procedure TfrmMain.UpdateApplicationToolBar;
var
  LWidth  : Integer;
  LButton : TToolButton;
  I       : Integer;
begin
  LWidth := 2;
  for I := 0 to tlbApplication.ButtonCount - 1 do
  begin
    LButton := tlbApplication.Buttons[I];
    if LButton.Visible then
      LWidth := LWidth + LButton.Width;
  end;
  pnlToolBarHost.Width := LWidth;
end;

procedure TfrmMain.HideAction(const AActionName: string);
var
  A : TCustomAction;
begin
  A := FEditorManager.Actions.ActionList.ActionByName(
    AActionName
  ) as TCustomAction;
  if Assigned(A) then
  begin
    A.Enabled := False;
    A.Visible := False;
  end;
end;

procedure TfrmMain.InitTextEditorActions;
begin
  HideAction('actAutoGuessHighlighter');
  HideAction('actClose');
  HideAction('actCreateDesktopLink');
  HideAction('actFormat');
  HideAction('actInsertCharacterFromMap');
  HideAction('actInsertColorValue');
  HideAction('actMonitorChanges');
  HideAction('actOpenFileAtCursor');
  HideAction('actShowActions');
  HideAction('actShowHexEditor');
  HideAction('actShowMiniMap');
  HideAction('actShowPreview');
  HideAction('actShowScriptEditor');
  HideAction('actShowStructureViewer');
  HideAction('actShowViews');
  HideAction('actSmartSelect');
end;

procedure TfrmMain.SwitchView;
begin
  if FHtmlEditor.IsInitialized then
  begin
    if (not FHtmlEditor.IsEmpty) or (not FHtmlEditor.Source.IsEmpty) then
    begin
      actHtmlEditor.Checked := True;
      nbRight.PageIndex := pgHtmlEditor.PageIndex;
    end
    else if not FEditor.Text.IsEmpty then
    begin
      actTextEditor.Checked := True;
      nbRight.PageIndex := pgTextEditor.PageIndex;
    end
    else if not FRichEditor.IsEmpty then
    begin
      actRtfEditor.Checked := True;
      nbRight.PageIndex := pgRichEditor.PageIndex;
    end
    else
    begin
      actTextEditor.Checked := True;
      nbRight.PageIndex := pgTextEditor.PageIndex;
    end;
  end;
end;

procedure TfrmMain.CreateTreeview;
begin
  FTree                          := TfrmVirtualDBTree.Create(Self);
  FTree.DoubleBuffered           := True;
  FTree.Parent                   := pnlLeft;
  FTree.BorderStyle              := bsNone;
  FTree.Align                    := alClient;
  FTree.Visible                  := True;
  FTree.DataSet                  := DataSet.DataSet;
  FTree.ImageList                := (FData as IGlyphs).ImageList;
  FTree.OnDropFiles              := FTreeDropFiles;
  FTree.OnNewFolderNode          := FTreeNewFolderNode;
  FTree.OnNewItemNode            := FTreeNewItemNode;
  FTree.OnDeleteSelectedNodes    := FTreeDeleteSelectedNodes;
  FTree.OnDuplicateSelectedNodes := FTreeDuplicateSelectedNodes;
  FTree.OnCopyNodeData           := FTreeCopyNodeData;
end;

procedure TfrmMain.CreateTextEditor;
var
  LEvents : IEditorEvents;
begin
  FEditorSettings := TEditorFactories.CreateSettings(Self);
  FEditorSettings.FileName := EDITOR_SETTINGS_FILE;
  FEditorSettings.Load;
  FEditorManager := TEditorFactories.CreateManager(Self, FEditorSettings);
  FEditor := TEditorFactories.CreateView(pnlEditor, FEditorManager, 'Editor');
  FEditor.IsFile := False;
  FEditor.Form.OnEnter := FEditorFormEnter;
  FEditor.Editor.PopupMenu := FEditorManager.Menus.EditorPopupMenu;
  btnHighlighter.Menu := FEditorManager.Menus.HighlighterPopupMenu;
  FEditorManager.Settings.AutoFormatXML := False;
  FEditorManager.Settings.AutoGuessHighlighterType := False;
  LEvents := FEditorManager.Events;
  LEvents.OnNew        := FEditorNew;
  LEvents.OnBeforeSave := FEditorBeforeSave;
  LEvents.AddOnChangeHandler(FEditorChange);
  LEvents.AddOnHighlighterChangeHandler(FEditorHighlighterChange);
  LEvents.OnShowEditorToolView := FEditorShowToolView;
  LEvents.OnHideEditorToolView := FEditorHideToolView;
  BuildTextEditorToolBar;
  InitTextEditorActions;
end;

procedure TfrmMain.CreateRichEditor;
begin
  FRichEditorManager := TRichEditorFactories.CreateManager(Self);
  FRichEditor := TRichEditorFactories.CreateView(
    pnlRichEditor,
    FRichEditorManager,
    'Comment'
  );
  FRichEditor.IsFile       := False;
  FRichEditor.Form.OnEnter := FRichEditorFormEnter;
  FRichEditor.OnChange     := FRichEditorChange;
  FRichEditor.OnDropFiles  := FRichEditorDropFiles;
  FRichEditor.PopupMenu    := FRichEditorManager.EditorPopupMenu;
  FRichEditorManager.Events.OnShowRichEditorToolView := FRichEditorShowToolView;
  FRichEditorManager.Events.OnHideRichEditorToolView := FRichEditorHideToolView;
  FRichEditorToolBar := TRichEditorFactories.CreateMainToolbar(
    Self,
    pnlRichEditorToolBar,
    FRichEditorManager as IRichEditorActions
  );
  FRichEditorToolBar.Height := 23;
end;

procedure TfrmMain.CreateHtmlEditor;
begin
  FHtmlEditorManager := THtmlEditorFactories.CreateManager(Self);
  FHtmlEditorToolBar := THtmlEditorFactories.CreateMainToolbar(
    Self,
    pnlHtmlEditorToolBar,
    FHtmlEditorManager as IHtmlEditorActions
  );
  FHtmlEditor := THtmlEditorFactories.CreateView(
    pnlHtmlEditor,
    FHtmlEditorManager
  );
  FHtmlEditorManager.Events.AddOnContentLoadedHandler(FHtmlEditorContentLoaded);
  FHtmlEditorManager.Events.AddOnNavigationCompletedHandler(FHtmlEditorNavigationCompleted);
  FHtmlEditorToolBar.Height := 23;
  FHtmlEditor.SetFocus;
  FHtmlEditor.OnAfterCreated := FHtmlEditorAfterCreated;
  FHtmlEditor.OnChange       := FHtmlEditorChange;
end;

procedure TfrmMain.InitializeLogger;
begin
  if FSettings.EmitLogMessages then
  begin
    Logger.Channels.Add(TIpcChannel.Create);
    Logger.Clear; // first few messages can be lost by receiver instance, so
    Logger.Clear; // we send some dummy ones.
    Logger.Clear;
    Logger.Info('SnippetsSource started.');
  end;
end;

procedure TfrmMain.Modified;
begin
  FUpdate := True;
end;

function TfrmMain.FileExtensionToHighlighter(const AFileExtension: string
  ): string;
var
  I  : Integer = 0;
  B  : Boolean = False;
  HL : THighlighters;
begin
  Result := 'TXT';
  HL := FEditorManager.Highlighters;
  while (I < HL.Count) and not B do
  begin
    B := HL[I].FileExtensions.Contains(AFileExtension.ToLower);
    if not B then
      Inc(I);
  end;
  if B then
    Result := HL[I].Highlighter;
end;

procedure TfrmMain.LoadRtfData;
var
  S : string;
begin
  Logger.Enter(Self, 'LoadRtfData');
  if Snippet.RtfData <> '' then
  begin
    S := DecodeStringBase64(Snippet.RtfData);
    if S <> FRtfStream.DataString then
    begin
      FRtfStream.Clear;
      FRtfStream.WriteString(S);
      FRtfStream.Position := 0;
      FUpdateRichTextView := True;
    end
  end
  else
  begin
    FRtfStream.Clear;
    FUpdateRichTextView := True;
  end;
  Logger.Leave(Self, 'LoadRtfData');
end;

procedure TfrmMain.SaveRtfData;
var
  SS : TStringStream;
  S  : string;
begin
  Logger.Enter(Self, 'SaveRtfData');
  SS := TStringStream.Create('');
  try
    if Assigned(DataSet) then
      DataSet.Edit;
    if not RichEditor.IsEmpty then
    begin
      RichEditor.BeginUpdate;
      RichEditor.SaveToStream(SS);
      RichEditor.EndUpdate;
      S := EncodeStringBase64(SS.DataString);
      Snippet.RtfData := S;
      Snippet.RtfText := RichEditor.Text;
    end
    else
    begin
      Snippet.RtfData := '';
      Snippet.RtfText := '';
    end;
  finally
    FreeAndNil(SS);
  end;
  Logger.Leave(Self, 'SaveRtfData');
end;

procedure TfrmMain.LoadHtmlData;
var
  S : string;
begin
  Logger.Enter(Self, 'LoadHtmlData');
  try
    Logger.Send('Snippet.Source', Snippet.Source);
    Logger.Send('FHtmlEditor.Source', FHtmlEditor.Source);
    if Snippet.Source.IsEmpty then
    begin
      S := DecodeStringBase64(Snippet.HtmlData);
      FHtmlEditor.HtmlText := S;
      FHtmlEditor.Source   := '';
    end
    else
    begin
      FHtmlEditor.Source := Snippet.Source;
    end;
  except
    Logger.Error('DecodeStringBase64 failed during LoadHtmlData.');
  end;
  Logger.Leave(Self, 'LoadHtmlData');
end;

procedure TfrmMain.SaveHtmlData;
begin
  Logger.Enter(Self, 'SaveHtmlData');
  if Assigned(DataSet) then
    DataSet.Edit;
  if HtmlEditor.Source.IsEmpty then
  begin
    Snippet.Source   := '';
    Snippet.HtmlData := EncodeStringBase64(HtmlEditor.HtmlText)
  end
  else
  begin
    Snippet.HtmlData := '';
    Snippet.Source   := HtmlEditor.Source;
  end;
  Logger.Leave(Self, 'SaveHtmlData');
end;

procedure TfrmMain.AddPathNode(const APath: string; const ACommonPath: string;
  ATree: TBaseVirtualTree);
var
  LIsDir      : Boolean;
  LIsTextFile : Boolean;
  LIsImage    : Boolean;
  LIsReadable : Boolean;
  LParentId   : Variant;
  LRelPath    : string;
  LParentPath : string;
  LFileName   : string;
begin
  Logger.Enter(Self, 'AddPathNode');
  Logger.Send('APath', APath);
  Connection.BeginBulkInserts;
  LIsTextFile := False;
  LIsImage    := False;
  LIsDir      := False;
  if DirectoryExists(APath) then
  begin
    LIsDir := True;
  end
  else
  begin
    LIsTextFile := FileIsText(APath, LIsReadable);
    if not LIsTextFile then
    begin
      LIsImage := TPicture.FindGraphicClassWithFileExt(
        ExtractFileExt(APath), False
      ) <> nil;
    end;
  end;

  if LIsDir or (LIsTextFile and LIsReadable) or LIsImage then
  begin
    LRelPath    := CreateRelativePath(APath, ACommonPath);
    LParentPath := ChompPathDelim(GetParentDir(LRelPath));
    if LParentPath <> '' then
    begin
      LParentId := Query.QueryValue(Format(SQL_PARENT_ID, [FLastId, LParentPath]));
      if LParentId <> Null then
        FParentId := LParentId;
    end;
  end;
  LFileName := Trim(ExtractFileName(APath));
  if LFileName <> '' then
  begin
    if FilenameExtIn(LFileName, ['html', 'htm', 'mhtml', 'svg', 'pdf']) then
    begin
      HtmlEditor.LoadFromFile(LFileName);
    end;
    if LIsTextFile then
    begin
      DataSet.Append;
      Snippet.Text        := ReadFileToString(APath);
      Snippet.Highlighter := FileExtensionToHighlighter(
        DelChars(UpperCase(ExtractFileExt(APath)), '.')
      );
      Snippet.NodeName    := LFileName;
      Snippet.NodeTypeId  := 2;
      Snippet.ImageIndex  := 2;
      Snippet.ParentId    := FParentId;
      DataSet.Post;
    end
    else if LIsImage then
    begin
      DataSet.Append;
      Snippet.NodeName    := LFileName;
      Snippet.NodeTypeId  := 2;
      Snippet.ImageIndex  := 2;
      Snippet.ParentId    := FParentId;
      RichEditor.Clear;
      RichEditor.InsertImageFile(APath);
      SaveRtfData;
      DataSet.Post;
    end
    else if LIsDir then
    begin
      DataSet.Append;
      Snippet.NodeName   := LFileName;
      Snippet.NodeTypeId := 1;
      Snippet.ImageIndex := 1;
      Snippet.NodePath   := LRelPath;
      Snippet.ParentID   := FParentId;
      DataSet.Post;
    end;
    Logger.Send('NodeName', Snippet.NodeName);
    if LIsDir then
      FParentId := Snippet.Id
    else if LIsTextFile then
      FParentId := Snippet.ParentId;
    Logger.Send('FParentId', FParentId);
  end;
  Logger.Leave(Self, 'AddPathNode');
end;

procedure TfrmMain.AddDirectoryNode(const APath: string;
  const ACommonPath: string; ATree: TBaseVirtualTree);
var
  V           : Variant;
  LRelPath    : string;
  LParentPath : string;
  LFileName   : string;
begin
  Logger.Enter(Self, 'AddDirectoryNode');
  Connection.EndBulkInserts;
  Logger.Send('APath', APath);
  Logger.Send('ACommonPath', ACommonPath);
  LRelPath    := CreateRelativePath(APath, ACommonPath);
  LParentPath := ChompPathDelim(GetParentDir(LRelPath));
  Logger.Send('LRelPath', LRelPath);
  Logger.Send('LParentPath', LParentPath);
  if LParentPath <> '' then
  begin
    V := Query.QueryValue(Format(SQL_PARENT_ID, [FLastId, LParentPath]));
    if V <> Null then
      FParentId := V;
  end;
  LFileName := Trim(ExtractFileName(APath));
  if LFileName <> '' then
  begin
    DataSet.Append;
    Snippet.NodeName   := LFileName;
    Snippet.NodeTypeId := 1;
    Snippet.ImageIndex := 1;
    Snippet.NodePath   := LRelPath;
    Snippet.ParentId   := FParentId;
    DataSet.Post;
  end;
  FParentId := Snippet.Id;
  Logger.Leave(Self, 'AddDirectoryNode');
end;

procedure TfrmMain.ExportNode;
begin
  if Snippet.NodeTypeId = 2 then
    Editor.Save(Snippet.NodeName);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMain.UpdateActions;
begin
  inherited UpdateActions;
  btnHighlighter.Caption := Editor.HighlighterName;
  pnlEditor.Visible := (DataSet.RecordCount > 0) and (FTree.SelectionCount < 2);
  UpdateToolBars;
  UpdateViews;
  UpdateStatusBar;
  UpdateApplicationToolBar;
  //if not edtTitle.Focused then
  //begin
  //  edtTitle.Color      := clForm;
  //  edtTitle.Font.Color := clDkGray;
  //end;

  pnlWelcome.Visible := DataSet.DataSet.Active and DataSet.DataSet.IsEmpty;
end;

procedure TfrmMain.UpdateStatusBar;
begin
  pnlPosition.Caption :=
    Format('%1d:%1d / %1d | %1d', [
      Editor.CaretX,
      Editor.CaretY,
      Editor.Lines.Count,
      Editor.SelStart
    ]);
  pnlSnippetCount.Caption := Format('%d records.', [DataSet.RecordCount]);
  pnlSize.Caption := FormatByteText(Editor.TextSize + RichEditor.ContentSize
  + HtmlEditor.ContentSize);
  if DataSet.RecordCount > 0 then
  begin
    pnlId.Caption           := Format(SId, [Snippet.Id]);
    pnlDateCreated.Caption  := Format(
      SDateCreated, [DateTimeToStr(Snippet.DateCreated)]
    );
    pnlDateModified.Caption := Format(
      SDateModified, [DateTimeToStr(Snippet.DateModified)]
    );
  end;
  if Editor.Editor.InsertMode then
    pnlEditMode.Caption := 'INS'
  else
    pnlEditMode.Caption := 'OVR';
  pnlLineBreakStyle.Caption := Editor.LineBreakStyle;

  if Odd(GetKeyState(VK_NUMLOCK)) then
    pnlNumLock.Caption := 'NUM '
  else
    pnlNumLock.Caption := '';

  if Odd(GetKeyState(VK_CAPITAL)) then
    pnlCapsLock.Caption := 'CAPS'
  else
    pnlCapsLock.Caption := '';
  OptimizeWidth(pnlSnippetCount);
  OptimizeWidth(pnlPosition);
  OptimizeWidth(pnlSize);
  OptimizeWidth(pnlId);
  OptimizeWidth(pnlDateCreated);
  OptimizeWidth(pnlDateModified);
end;

procedure TfrmMain.UpdateViews;
begin
  if FUpdate then
  begin
    if FUpdateRichTextView then
    begin
      RichEditor.LoadFromStream(FRtfStream);
      FUpdateRichTextView := False;
    end;
    if RichEditor.IsEmpty and (FSettings.DefaultRichEditorFontName <> '') then
    begin
      RichEditor.Font.Name := FSettings.DefaultRichEditorFontName;
    end;
    FUpdate := False;
  end;
end;

procedure TfrmMain.UpdateToolBars;
begin
  if FSettings.AutoHideEditorToolBar then
    pnlEditorToolBar.Visible := Editor.Focused
  else
    pnlEditorToolBar.Visible := True;
  if FSettings.AutoHideRichEditorToolBar then
    FRichEditorToolBar.Visible := RichEditor.Focused
  else
  FRichEditorToolBar.Visible := True;
  //actHtmlEditor.Checked := nbRight.ActivePageComponent = pgHtmlEditor;
  //actRtfEditor.Checked  := nbRight.ActivePageComponent = pgRichEditor;
  //actTextEditor.Checked := nbRight.ActivePageComponent = pgTextEditor;
end;
{$ENDREGION}

end.
