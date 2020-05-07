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

unit SnippetSource.Forms.Main;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, ComCtrls, ActnList,
  StdCtrls, Menus, Buttons,
  DB,
  LazFileUtils, FileUtil,

  SynEdit, VirtualTrees, MenuButton,

  ts.Core.VersionInfo,
  ts.Editor.Interfaces, ts.Editor.Highlighters, ts.Editor.Factories,
  ts.RichEditor.Interfaces,

  SnippetSource.Forms.Lookup, SnippetSource.Forms.VirtualDBTree,
  SnippetSource.Interfaces, SnippetSource.Settings, SnippetSource.Forms.Console;

{
  REMARKS:
    - RTF text is stored in Base64 encoding.
      To make the RTF text searchable a copy with only the flat text is stored.
}

const
  EDITOR_SETTINGS_FILE = 'settings.xml';

type

  { TfrmMain }

  TfrmMain = class(TForm)
    {$REGION 'designer controls'}
    aclMain             : TActionList;
    actAbout            : TAction;
    actExecute          : TAction;
    actConsole          : TAction;
    actShowGridForm     : TAction;
    actLookup           : TAction;
    actSettings         : TAction;
    actToggleFullScreen : TAction;
    actToggleStayOnTop  : TAction;
    btnHighlighter      : TMenuButton;
    btnLineBreakStyle   : TSpeedButton;
    btnLookup           : TToolButton;
    btnSettingsDialog   : TToolButton;
    dscMain             : TDatasource;
    edtTitle            : TEdit;
    imlMain             : TImageList;
    imlNodes            : TImageList;
    pnlComments         : TPanel;
    pnlEditMode         : TPanel;
    pnlEditor           : TPanel;
    pnlDateCreated      : TPanel;
    pnlDateModified     : TPanel;
    pnlId               : TPanel;
    pnlLeft             : TPanel;
    pnlLineBreakStyle   : TPanel;
    pnlPosition         : TPanel;
    pnlRight            : TPanel;
    pnlDateModifie      : TPanel;
    pnlSize             : TPanel;
    pnlSnippetCount     : TPanel;
    pnlStatusBar        : TPanel;
    pnlTitle            : TPanel;
    splHorizontal       : TSplitter;
    splVertical         : TSplitter;
    tlbApplication      : TToolBar;
    tlbEditorView       : TToolBar;
    btnConsole          : TToolButton;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actAboutExecute(Sender: TObject);
    procedure actConsoleExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure actLookupExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actShowGridFormExecute(Sender: TObject);
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
    procedure EChange(Sender: TObject);
    procedure EHighlighterChange(Sender: TObject);
    procedure EBeforeSave(
      Sender           : TObject;
      var AStorageName : string
    );
    procedure ENew(
      Sender        : TObject;
      var AFileName : string;
      const AText   : string
    );
    procedure RVChange(Sender: TObject);
    procedure FTreeDropFiles(
      Sender      : TBaseVirtualTree;
      AFiles      : TStrings;
      AAttachMode : TVTNodeAttachMode
    );
    procedure FTreeNewFolderNode(Sender: TObject);
    procedure FTreeNewItemNode(Sender: TObject);
    procedure FTreeDeleteSelectedNodes(Sender: TObject);
    {$ENDREGION}

  private
    FTree              : TfrmVirtualDBTree;
    FVersionInfo       : TVersionInfo;
    FFileSearcher      : TFileSearcher;
    FParentId          : Integer;
    FCommonPath        : string;
    FData              : IDataSet;
    FEditorManager     : IEditorManager;
    FEditorSettings    : IEditorSettings;
    FRichEditorManager : IRichEditorManager;
    FSettings          : TSettings;
    FConsole           : TfrmConsole;
    FBrowsing          : Boolean;

    function GetConnection: IConnection;
    function GetDataSet: IDataSet;
    function GetEditor: IEditorView;
    function GetRichEditor: IRichEditorView;
    function GetSnippet: ISnippet;

    procedure CreateTreeview;
    procedure CreateEditor;
    procedure CreateRichEditor;

    function FileExtensionToHighlighter(const AFileExtension: string): string;

    procedure LoadRichText;
    procedure SaveRichText;

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
    procedure ExportNode;

  protected
    procedure AddButton(
      const AActionName : string;
      APopupMenu        : TPopupMenu = nil
    ); overload;
    procedure AddButton(AAction: TBasicAction); overload;
    procedure AssignEditorChanges;
    procedure BuildToolBar;
    procedure HideAction(const AActionName: string);
    procedure InitActions;
    procedure UpdateActions; override;
    procedure UpdateStatusBar;

  public
     procedure AfterConstruction; override;
     destructor Destroy; override;

     property Connection: IConnection
       read GetConnection;

     property Snippet: ISnippet
       read GetSnippet;

     property DataSet: IDataSet
       read GetDataSet;

     property Editor: IEditorView
       read GetEditor;

     property RichEditor: IRichEditorView
       read GetRichEditor;
  end; 

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  StrUtils, Base64, Dialogs,

  ts.Core.Utils, ts.Core.Logger,

  ts.Editor.AboutDialog,

  ts.Richeditor.Factories,

  SnippetSource.Forms.SettingsDialog, SnippetSource.Modules.Data,
  SnippetSource.Forms.Grid, SnippetSource.Resources;

{$REGION 'construction and destruction'}
procedure TfrmMain.AfterConstruction;
begin
  inherited AfterConstruction;
  FSettings := TSettings.Create(Self);
  FSettings.Load;
  FData := TdmSnippetSource.Create(Self, FSettings);

  CreateEditor;
  CreateRichEditor;
  CreateTreeview;

  dscMain.DataSet := DataSet.DataSet;

  FFileSearcher := TFileSearcher.Create;
  FFileSearcher.OnDirectoryFound := FileSearcherDirectoryFound;
  FFileSearcher.OnFileFound      := FileSearcherFileFound;

  BuildToolBar;
  InitActions;
  btnLineBreakStyle.PopupMenu := FEditorManager.Menus.LineBreakStylePopupMenu;

  TRichEditorFactories.CreateMainToolbar(
    Self,
    pnlComments,
    FRichEditorManager as IRichEditorActions
  );
  FVersionInfo := TVersionInfo.Create(Self);
  Caption := Format('%s %s', [ApplicationName, FVersionInfo.FileVersion]);

  Logger.Info('SnippetSource started');
end;

destructor TfrmMain.Destroy;
begin
  FSettings.Save;
  FEditorSettings.Save;
  FData           := nil;
  FEditorManager  := nil;
  FEditorSettings := nil;
  FreeAndNil(FFileSearcher);
  inherited Destroy;
  Logger.Info('SnippetSource Stopped');
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmMain.GetRichEditor: IRichEditorView;
begin
  Result := FRichEditorManager.ViewByName['Comment'];
end;

function TfrmMain.GetEditor: IEditorView;
begin
  Result := FEditorManager.Views.ViewByName['Editor'];
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
  ExecuteSettingsDialog(FData);
end;

procedure TfrmMain.actShowGridFormExecute(Sender: TObject);
begin
  ShowGridForm(DataSet.DataSet);
end;

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  ShowAboutDialog;
end;

procedure TfrmMain.actConsoleExecute(Sender: TObject);
begin
  if not Assigned(FConsole) then
  begin
    FConsole := TfrmConsole.Create(Self);
  end;
  FConsole.Show;
end;

procedure TfrmMain.actExecuteExecute(Sender: TObject);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create('SnippetSource.bat', fmCreate);
  try
    Editor.SaveToStream(FS);
    if not Assigned(FConsole) then
    begin
      FConsole := TfrmConsole.Create(Self);
    end;
    FConsole.Show;
    FConsole.Execute('SnippetSource.bat');
  finally
    FS.Free;
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
    FormStyle := fsStayOnTop
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

procedure TfrmMain.ENew(Sender: TObject; var AFileName: string; const AText: string);
begin
  FTree.NewSubItemNode;
  Editor.Text := AText;
  AssignEditorChanges;
end;

procedure TfrmMain.FTreeNewFolderNode(Sender: TObject);
begin
  DataSet.Edit;
  Snippet.ImageIndex := 1;
end;

procedure TfrmMain.FTreeNewItemNode(Sender: TObject);
begin
  DataSet.Edit;
  Snippet.ImageIndex := 2;
end;

procedure TfrmMain.FTreeDeleteSelectedNodes(Sender: TObject);
begin
  DataSet.ApplyUpdates;
end;

procedure TfrmMain.EChange(Sender: TObject);
begin
  DataSet.Edit;
  AssignEditorChanges;
end;

procedure TfrmMain.EHighlighterChange(Sender: TObject);
begin
  Logger.Info('EHighlighterChange');
  if Snippet.Highlighter <> Editor.HighlighterName then
  begin
    DataSet.Edit;
    AssignEditorChanges;
  end;
end;

procedure TfrmMain.EBeforeSave(Sender: TObject; var AStorageName: string);
begin
  DataSet.Post;
end;

procedure TfrmMain.dscMainDataChange(Sender: TObject; Field: TField);
var
  DS : TDataSet;
begin
  if Field = nil then // browse record
  begin
    if Assigned(DataSet.DataSet) then
    begin
      DS := DataSet.DataSet;
      if DS.State = dsBrowse then
      begin
        Editor.BeginUpdate;
        FParentId := Snippet.ID;
        Editor.Text := Snippet.Text;
        if Snippet.Highlighter <> '' then
        begin
          Editor.HighlighterName := Snippet.Highlighter;
        end;
        edtTitle.Text := Snippet.NodeName;
        btnHighlighter.Caption := Snippet.Highlighter;
        LoadRichText;
        Editor.EndUpdate;
      end;
    end;
  end;
end;

{$REGION 'edtTitle'}
procedure TfrmMain.edtTitleChange(Sender: TObject);
begin
  if Snippet.NodeName <> edtTitle.Text then
    DataSet.Edit;
end;

procedure TfrmMain.edtTitleEditingDone(Sender: TObject);
begin
  Snippet.NodeName := edtTitle.Text;
end;

procedure TfrmMain.edtTitleEnter(Sender: TObject);
begin
  DataSet.Edit;
end;

procedure TfrmMain.edtTitleMouseEnter(Sender: TObject);
begin
  edtTitle.Color      := clWhite;
  edtTitle.Font.Color := clBlack;
end;

procedure TfrmMain.edtTitleExit(Sender: TObject);
begin
  edtTitle.Color      := clForm;
  edtTitle.Font.Color := clDkGray;
end;

procedure TfrmMain.edtTitleMouseLeave(Sender: TObject);
begin
  if not edtTitle.Focused then
  begin
    edtTitle.Color      := clForm;
    edtTitle.Font.Color := clDkGray;
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

{$REGION 'FTree'}
procedure TfrmMain.FTreeDropFiles(Sender: TBaseVirtualTree;
  AFiles: TStrings; AAttachMode: TVTNodeAttachMode);
var
  I     : Integer;
  T     : Integer;
  LFile : string;
begin
  FCommonPath := GetCommonPath(AFiles);
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
  DataSet.BeginBulkInserts;
  try
    for I := 0 to AFiles.Count - 1 do
    begin
      LFile := AFiles[I];
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
    DataSet.EndBulkInserts;
    DataSet.DataSet.Refresh;
    DataSet.EnableControls;
    Sender.Refresh;
  end;
end;
{$ENDREGION}

{$REGION 'RV'}
procedure TfrmMain.RVChange(Sender: TObject);
begin
  if not RichEditor.IsEmpty  then
  begin
    DataSet.Edit;
    AssignEditorChanges;
  end;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmMain.CreateTreeview;
begin
  FTree                       := TfrmVirtualDBTree.Create(Self);
  FTree.DoubleBuffered        := True;
  FTree.Parent                := pnlLeft;
  FTree.BorderStyle           := bsNone;
  FTree.Align                 := alClient;
  FTree.Visible               := True;
  FTree.DataSet               := DataSet.DataSet;
  FTree.ImageList             := (FData as IGlyphs).ImageList;
  FTree.OnDropFiles           := FTreeDropFiles;
  FTree.OnNewFolderNode       := FTreeNewFolderNode;
  FTree.OnNewItemNode         := FTreeNewItemNode;
  FTree.OnDeleteSelectedNodes := FTreeDeleteSelectedNodes;
end;

procedure TfrmMain.CreateEditor;
var
  EV : IEditorEvents;
  V  : IEditorView;
begin
  FEditorSettings := TEditorFactories.CreateSettings(Self);
  FEditorSettings.FileName := EDITOR_SETTINGS_FILE;
  FEditorSettings.Load;
  FEditorManager := TEditorFactories.CreateManager(Self, FEditorSettings);
  V := TEditorFactories.CreateView(pnlEditor, FEditorManager, 'Editor');
  V.IsFile := False;
  V.Editor.PopupMenu  := FEditorManager.Menus.EditorPopupMenu;
  btnHighlighter.Menu := FEditorManager.Menus.HighlighterPopupMenu;
  FEditorManager.Settings.AutoFormatXML := False;
  FEditorManager.Settings.AutoGuessHighlighterType := False;
  EV := FEditorManager.Events;
  EV.OnNew        := ENew;
  EV.OnBeforeSave := EBeforeSave;
  EV.AddOnChangeHandler(EChange);
  EV.AddOnHighlighterChangeHandler(EHighlighterChange);
end;

procedure TfrmMain.CreateRichEditor;
var
  RV : IRichEditorView;
begin
  FRichEditorManager := TRichEditorFactories.CreateManager(Self);
  RV := TRichEditorFactories.CreateView(
    pnlComments,
    FRichEditorManager,
    'Comment'
  );
  RV.OnChange  := RVChange;
  RV.PopupMenu := FRichEditorManager.EditorPopupMenu;
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

procedure TfrmMain.LoadRichText;
var
  S  : string;
  SS : TStringStream;
begin
  if Snippet.CommentRTF <> '' then
  begin
    S := DecodeStringBase64(Snippet.CommentRTF);
    SS := TStringStream.Create(S);
    try
      SS.Position := 0;
      if SS.Size > 0 then
      begin
        RichEditor.BeginUpdate;
        RichEditor.Clear;
        RichEditor.LoadFromStream(SS);
        RichEditor.EndUpdate;
      end;
    finally
      SS.Free;
    end;
  end
  else
  begin
    RichEditor.BeginUpdate;
    RichEditor.Clear;
    RichEditor.EndUpdate;
  end;
end;

procedure TfrmMain.SaveRichText;
var
  SS : TStringStream;
  S  : string;
begin
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
      Snippet.CommentRTF := S;
      Snippet.Comment    := RichEditor.Text;
    end
    else
    begin
      Snippet.CommentRTF := '';
      Snippet.Comment    := '';
    end;
  finally
    FreeAndNil(SS);
  end;
end;

procedure TfrmMain.AddPathNode(const APath: string; const ACommonPath: string;
  ATree: TBaseVirtualTree);
var
  LIsDir      : Boolean;
  LIsTextFile : Boolean;
  LIsReadable : Boolean;
  LParentId   : Variant;
  LRelPath    : string;
  LParentPath : string;
  LFileName   : string;
begin
  DataSet.BeginBulkInserts;
  LIsTextFile := False;
  LIsDir      := False;
  if DirectoryExists(APath) then
  begin
    LIsDir := True;
  end
  else
  begin
    LIsTextFile := FileIsText(APath, LIsReadable);
  end;

  if LIsDir or (LIsTextFile and LIsReadable) then
  begin
    LRelPath    := CreateRelativePath(APath, ACommonPath);
    LParentPath := ChompPathDelim(GetParentDir(LRelPath));
    if LParentPath <> '' then
    begin
      LParentId := DataSet.DataSet.Lookup('NodePath', LParentPath, 'Id');
      if LParentId <> Null then
        FParentId := LParentId;
    end;
  end;
  LFileName := Trim(ExtractFileName(APath));
  if LFileName <> '' then
  begin
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
    if LIsDir then
      FParentId := Snippet.Id
    else if LIsTextFile then
      FParentId := Snippet.ParentId;
  end;
end;

procedure TfrmMain.AddDirectoryNode(const APath: string;
  const ACommonPath: string; ATree: TBaseVirtualTree);
var
  V           : Variant;
  LRelPath    : string;
  LParentPath : string;
  LFileName   : string;
begin
  DataSet.EndBulkInserts;
  LRelPath    := CreateRelativePath(APath, ACommonPath);
  LParentPath := ChompPathDelim(GetParentDir(LRelPath));
  if LParentPath <> '' then
  begin
    V := DataSet.DataSet.Lookup('NodePath', LParentPath, 'Id');
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
    Snippet.ParentID   := FParentId;
    DataSet.Post;
  end;
  FParentId := Snippet.Id;
end;

procedure TfrmMain.ExportNode;
begin
  if Snippet.NodeTypeID = 2 then
    Editor.Save(Snippet.NodeName);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMain.AddButton(const AActionName: string; APopupMenu: TPopupMenu);
var
  TB : TToolButton;
begin
  TB := TToolButton.Create(Self);
  TB.Parent := tlbEditorView;
  if Assigned(APopupMenu) then
  begin
    TB.Style := tbsDropDown;
    TB.DropdownMenu := APopupMenu;
    TB.Action := FEditorManager.Actions[AActionName];
  end
  else
  begin
    if AActionName = '' then
    begin
      TB.Style := tbsDivider;
    end
    else
      TB.Action := FEditorManager.Actions[AActionName];
  end;
end;

procedure TfrmMain.AddButton(AAction: TBasicAction);
var
  TB : TToolButton;
begin
  TB := TToolButton.Create(Self);
  TB.Parent := tlbApplication;
  TB.Action := AAction;
end;

procedure TfrmMain.AssignEditorChanges;
begin
  Snippet.Text        := Editor.Text;
  Snippet.FoldLevel   := Editor.FoldLevel;
  Snippet.Highlighter := Editor.HighlighterName;
  if Snippet.Highlighter = '' then
    Snippet.Highlighter := 'TXT';
  SaveRichText;
end;

procedure TfrmMain.BuildToolBar;
begin
  tlbEditorView.Images := FEditorManager.Actions.ActionList.Images;
  tlbApplication.Images := imlMain;
  AddButton('actSave');
  AddButton('actSaveAs');
  AddButton('');
  AddButton('actCut');
  AddButton('actCopy');
  AddButton('actPaste');
  AddButton('');
  AddButton('actSelectAll');
  AddButton('actClear');
  AddButton('');
  AddButton('actUndo');
  AddButton('actRedo');
  AddButton('');
  AddButton('actSearch');
  AddButton('actSearchReplace');
  AddButton('actFindNext');
  AddButton('actFindPrevious');
  AddButton('');
  AddButton('actToggleFoldLevel', FEditorManager.Menus.FoldPopupMenu);
  AddButton('actToggleHighlighter', FEditorManager.Menus.HighlighterPopupMenu);
  AddButton('');

  AddButton('');
  AddButton('actSettings');
  AddButton('actAbout');
end;

procedure TfrmMain.HideAction(const AActionName: string);
var
  A : TCustomAction;
begin
  A := FEditorManager.Actions.ActionList.ActionByName(AActionName) as TCustomAction;
  if Assigned(A) then
  begin
    A.Enabled := False;
    A.Visible := False;
  end;
end;

procedure TfrmMain.InitActions;
begin
  HideAction('actAlignSelection');
  HideAction('actAutoGuessHighlighter');
  HideAction('actClose');
  HideAction('actCreateDesktopLink');
  HideAction('actFilterCode');
  HideAction('actFindAllOccurences');
  HideAction('actFindNext');
  HideAction('actFindPrevious');
  HideAction('actFormat');
  HideAction('actInsertCharacterFromMap');
  HideAction('actInsertColorValue');
  HideAction('actMonitorChanges');
  HideAction('actOpenFileAtCursor');
  HideAction('actSearch');
  HideAction('actSearchReplace');
  HideAction('actShapeCode');
  HideAction('actShowActions');
  HideAction('actShowHexEditor');
  HideAction('actShowHTMLViewer');
  HideAction('actShowMiniMap');
  HideAction('actShowPreview');
  HideAction('actShowScriptEditor');
  HideAction('actShowStructureViewer');
  HideAction('actShowViews');
  HideAction('actSmartSelect');
  HideAction('actSortSelection');
end;

procedure TfrmMain.UpdateActions;
begin
  inherited UpdateActions;
  btnHighlighter.Caption := Editor.HighlighterName;
  pnlEditor.Visible := DataSet.RecordCount > 0;
  UpdateStatusBar;
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
  pnlSize.Caption         := FormatByteText(Editor.TextSize);
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
  OptimizeWidth(pnlSnippetCount);
  OptimizeWidth(pnlPosition);
  OptimizeWidth(pnlSize);
  OptimizeWidth(pnlEditMode);
  OptimizeWidth(pnlLineBreakStyle);
  OptimizeWidth(pnlId);
  OptimizeWidth(pnlDateCreated);
  OptimizeWidth(pnlDateModified);
end;
{$ENDREGION}

end.
