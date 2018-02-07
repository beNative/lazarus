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

unit SnippetSource.Forms.Main;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, DB, FileUtil, Forms, Controls, Graphics, ExtCtrls,
  ComCtrls, ActnList, StdCtrls, Menus, Buttons,
  LazFileUtils,

  SynEdit, VirtualTrees, MenuButton,

  ts.Core.VersionInfo,

  ts.Editor.Interfaces, ts.Editor.Highlighters, ts.Editor.Factories,

  ts.RichEditor.Helpers, ts.RichEditor.Interfaces,

  SnippetSource.Forms.Lookup, SnippetSource.Forms.VirtualDBTree,
  SnippetSource.Interfaces;

{
  REMARKS:
    - RTF text is stored in Base64 encoding.
      To make the RTF text searchable a copy with only the flat text is stored.
}

const
  SETTINGS_FILE = 'settings.xml';

type

  { TfrmMain }

  TfrmMain = class(TForm)
    {$REGION 'designer controls'}
    aclMain                : TActionList;
    actAbout               : TAction;
    actExecute             : TAction;
    actApplicationExplorer : TAction;
    actLookup              : TAction;
    actSettings            : TAction;
    actToggleFullScreen    : TAction;
    actToggleStayOnTop     : TAction;
    btnHighlighter         : TMenuButton;
    btnHighlighterSB       : TSpeedButton;
    btnLineBreakStyle      : TSpeedButton;
    btnLookup              : TToolButton;
    btnSettingsDialog      : TToolButton;
    dscMain                : TDatasource;
    edtSearch              : TEdit;
    edtTitle               : TEdit;
    imlMain                : TImageList;
    imlNodes               : TImageList;
    pnlComments            : TPanel;
    pnlEditMode            : TPanel;
    pnlEditor              : TPanel;
    pnlHighlighter         : TPanel;
    pnlID                  : TPanel;
    pnlLeft                : TPanel;
    pnlLineBreakStyle      : TPanel;
    pnlPosition            : TPanel;
    pnlProgress            : TPanel;
    pnlRight               : TPanel;
    pnlSearch              : TPanel;
    pnlSize                : TPanel;
    pnlSnippetcount        : TPanel;
    pnlStatusBar           : TPanel;
    pnlTitle               : TPanel;
    splHorizontal          : TSplitter;
    splVertical            : TSplitter;
    tlbApplication         : TToolBar;
    tlbEditorView          : TToolBar;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actAboutExecute(Sender: TObject);
    procedure actApplicationExplorerExecute(Sender: TObject);
    procedure actLookupExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actToggleFullScreenExecute(Sender: TObject);
    procedure actToggleStayOnTopExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure btnHighlighterSBClick(Sender: TObject);
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
    procedure FTreeDropFiles(
      Sender      : TBaseVirtualTree;
      AFiles      : TStrings;
      AAttachMode : TVTNodeAttachMode
    );
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
    procedure RVEditingDone(Sender: TObject);
    procedure RVSelectionChange(Sender: TObject);
    procedure FTreeNewFolderNode(Sender: TObject);
    procedure FTreeNewItemNode(Sender: TObject);
    procedure tlbRichEditorViewClick(Sender: TObject);
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

    function GetConnection: IConnection;
    function GetDataSet: IDataSet;
    function GetEditor: IEditorView;
    function GetRichEditor: IRichEditorView;
    function GetSnippet: ISnippet;

    procedure CreateTreeview;
    procedure CreateEditor;
    procedure CreateRichEditor;

    procedure AddPathNode(
      const APath       : string;
      const ACommonPath : string;
      ATree             : TBaseVirtualTree
    );
    procedure ExportNode;

  protected
    procedure HideAction(const AActionName: string);
    procedure AssignEditorChanges;
    procedure AddButton(
      const AActionName : string;
      APopupMenu        : TPopupMenu = nil
    ); overload;
    procedure AddButton(AAction: TBasicAction); overload;
    procedure InitActions;
    procedure UpdateStatusBar;
    procedure UpdateActions; override;
    procedure BuildToolBar;

  public
     procedure AfterConstruction; override;
     procedure BeforeDestruction; override;

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

  AppExploreFrm,

  ts.Core.Utils, ts.Core.SharedLogger,

  ts.Editor.AboutDialog,

  ts.Richeditor.Factories,

  SnippetSource.Forms.SettingsDialog, SnippetSource.Modules.Data,
  SnippetSource.Forms.Grid;

{$REGION 'construction and destruction'}
procedure TfrmMain.AfterConstruction;
begin
  inherited AfterConstruction;
  FData := TdmSnippetSource.Create(Self);

  CreateEditor;

  FFileSearcher := TFileSearcher.Create;
  FFileSearcher.OnDirectoryFound := FileSearcherDirectoryFound;
  FFileSearcher.OnFileFound      := FileSearcherFileFound;

  CreateRichEditor;

  BuildToolBar;
  InitActions;

  CreateTreeview;

  dscMain.DataSet := DataSet.DataSet;
  ShowGridForm(DataSet.DataSet);


  TRichEditorFactories.CreateMainToolbar(Self, pnlComments, FRichEditorManager as IRichEditorActions );
  //BuildStandardRichEditorToolbar(tlbRichEditorView);

  pnlHighlighter.PopupMenu    := FEditorManager.Menus.HighlighterPopupMenu;
  btnHighlighterSB.PopupMenu  := FEditorManager.Menus.HighlighterPopupMenu;
  btnLineBreakStyle.PopupMenu := FEditorManager.Menus.LineBreakStylePopupMenu;

  FVersionInfo := TVersionInfo.Create(Self);
  Caption := Format('%s %s', [ApplicationName, FVersionInfo.FileVersion]);
end;

procedure TfrmMain.BeforeDestruction;
begin
  FEditorSettings.Save;
  FData     := nil;
  FEditorManager  := nil;
  FEditorSettings := nil;
  FreeAndNil(FFileSearcher);
  inherited BeforeDestruction;
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

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  ShowAboutDialog;
end;

procedure TfrmMain.actApplicationExplorerExecute(Sender: TObject);
begin
  ShowAppExplorer;
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
procedure TfrmMain.btnHighlighterSBClick(Sender: TObject);
begin
  btnHighlighterSB.PopupMenu.PopUp;
end;

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

procedure TfrmMain.EChange(Sender: TObject);
begin
  Logger.Info('EChange');
  DataSet.Edit;
  AssignEditorChanges;
end;

procedure TfrmMain.EHighlighterChange(Sender: TObject);
begin
  //if Snippet.Highlighter <> Editor.HighlighterName then
  //begin
  //  DataSet.Edit;
  //  AssignEditorChanges;
  //end;
end;

procedure TfrmMain.EBeforeSave(Sender: TObject; var AStorageName: string);
begin
  DataSet.Post;
end;

procedure TfrmMain.dscMainDataChange(Sender: TObject; Field: TField);
var
  DS : TDataSet;
  SS : TStringStream;
  S  : string;
begin
  if Field = nil then // browse record
  begin
    if Assigned(DataSet.DataSet) then
    begin
      DS := DataSet.DataSet;
      if DS.State = dsBrowse then
      begin
        FParentId := Snippet.ID;
        Editor.Text := Snippet.Text;
        if Snippet.Highlighter <> '' then
        begin
          Editor.HighlighterName := Snippet.Highlighter;
        end;
        edtTitle.Text := Snippet.NodeName;
        btnHighlighter.Caption := Snippet.Highlighter;
        if Snippet.CommentRTF <> '' then
        begin
          S := DecodeStringBase64(Snippet.CommentRTF);
          SS := TStringStream.Create(S);
          try
            SS.Position := 0;
            if SS.Size > 0 then
            begin
              RichEditor.LoadFromStream(SS);
            end;
          finally
            SS.Free;
          end;
        end
        else
          RichEditor.Clear;
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
  AddPathNode(FileIterator.FileName, FCommonPath, FTree.TreeView);
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
  sFile : string;
begin
  DataSet.DisableControls;
  try
    FCommonPath := GetCommonPath(AFiles);
    T := Snippet.NodeTypeID;
    if T = 1 then // FOLDER
    begin
      FParentId := Snippet.Id;
    end
    else
    begin
      FParentId := Snippet.ParentID;
    end;
    for I := 0 to AFiles.Count - 1 do
    begin
      sFile := AFiles[I];
      if DirectoryExists(sFile) then // add files in directory
      begin
        AddPathNode(sFile, FCommonPath, Sender);
        FFileSearcher.Search(sFile);
      end
      else
      begin
        AddPathNode(sFile, FCommonPath, Sender);
      end;
    end;
  finally
    Connection.Commit;
    DataSet.Active := True;
    DataSet.EnableControls;
    Sender.Refresh;
  end;
end;
{$ENDREGION}

{$REGION 'RV'}
procedure TfrmMain.RVChange(Sender: TObject);
begin
  Logger.Warn('RVChange');
  DataSet.Edit;
  AssignEditorChanges;
end;

procedure TfrmMain.RVEditingDone(Sender: TObject);
var
  SS : TStringStream;
  S   : string;
begin
  SS := TStringStream.Create('');
  try
    RichEditor.SaveToStream(SS);
    S := EncodeStringBase64(SS.DataString);
    if Assigned(DataSet) then
      DataSet.Edit;
    if Assigned(Snippet) then
    begin
      Snippet.CommentRTF := S;
      //Snippet.Comment := RichEditor.Editor.Lines.Text;
    end;
  finally
    FreeAndNil(SS);
  end;
end;

procedure TfrmMain.RVSelectionChange(Sender: TObject);
begin
  DataSet.Edit;
end;

procedure TfrmMain.tlbRichEditorViewClick(Sender: TObject);
begin

end;

{$ENDREGION}
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmMain.AddPathNode(const APath: string; const ACommonPath: string;
  ATree: TBaseVirtualTree);
var
  LIsDir      : Boolean;
  LIsTextFile : Boolean;
  V           : Variant;
  LRelPath    : string;
  LParentPath : string;
  LFileName   : string;
begin
  pnlProgress.Caption := Format('Loading file: %s', [APath]);
  LIsTextFile := False;
  LIsDir      := False;
  if DirectoryExists(APath) then
  begin
    LIsDir := True;
  end
  else
  begin
    LIsTextFile := True;
  end;

  if LIsDir or LIsTextFile then
  begin
    LRelPath    := CreateRelativePath(APath, ACommonPath);
    LParentPath := ChompPathDelim(GetParentDir(LRelPath));
    if LParentPath <> '' then
    begin
      V := DataSet.DataSet.Lookup('NodePath', LParentPath, 'Id');
      if V <> Null then
        FParentId := V;
    end;
  end;
  LFileName := Trim(ExtractFileName(APath));
  if LFileName <> '' then
  begin
    if LIsTextFile then
    begin
      DataSet.Append;
      Snippet.Text        := ReadFileToString(APath);
      Snippet.Highlighter := DelChars(UpperCase(ExtractFileExt(APath)), '.');
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
  pnlProgress.Caption := '';
end;

procedure TfrmMain.ExportNode;
begin
  if Snippet.NodeTypeID = 2 then
    Editor.Save(Snippet.NodeName);
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
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMain.AssignEditorChanges;
//var
//  LId: Integer;
begin
  //LId := Snippet.Id;
  Snippet.Text      := Editor.Text;
  Snippet.FoldLevel := Editor.FoldLevel;
  if Assigned(Editor.HighlighterItem) then
  begin
    Snippet.Highlighter := Editor.HighlighterItem.Name;
  end;
  RVEditingDone(Self);
  Logger.Send('Id', Snippet.Id);
  //Snippet.Id := LId;
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
  pnlID.Caption := IntToStr(Snippet.ID);
  pnlSnippetcount.Caption := Format('%d records.', [DataSet.RecordCount]);
  pnlSize.Caption := FormatByteText(Editor.TextSize);

  if Assigned(Editor.HighlighterItem) then
    pnlHighlighter.Caption := Editor.HighlighterItem.Description;
  if Editor.Editor.InsertMode then
    pnlEditMode.Caption := 'INS'
  else
    pnlEditMode.Caption := 'OVR';
  pnlLineBreakStyle.Caption := Editor.LineBreakStyle;

  OptimizeWidth(pnlPosition);
  OptimizeWidth(pnlSize);
  OptimizeWidth(pnlHighlighter);
  OptimizeWidth(pnlSnippetcount);
  OptimizeWidth(pnlEditMode);
  OptimizeWidth(pnlLineBreakStyle);
  OptimizeWidth(pnlID);
end;

procedure TfrmMain.UpdateActions;
begin
  inherited UpdateActions;
  btnHighlighter.Caption := Editor.HighlighterName;
  UpdateStatusBar;
end;

procedure TfrmMain.AddButton(const AActionName: string; APopupMenu: TPopupMenu);
var
  TB: TToolButton;
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
  TB: TToolButton;
begin
  TB := TToolButton.Create(Self);
  TB.Parent := tlbApplication;
  TB.Action := AAction;
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
  AddButton('actSettings');
  AddButton('actAbout');
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

procedure TfrmMain.CreateTreeview;
begin
  FTree                 := TfrmVirtualDBTree.Create(Self);
  FTree.DoubleBuffered  := True;
  FTree.Parent          := pnlLeft;
  FTree.BorderStyle     := bsNone;
  FTree.Align           := alClient;
  FTree.Visible         := True;
  FTree.DataSet         := DataSet.DataSet;
  FTree.ImageList       := (FData as IGlyphs).ImageList;
  FTree.OnDropFiles     := FTreeDropFiles;
  FTree.OnNewFolderNode := FTreeNewFolderNode;
  FTree.OnNewItemNode   := FTreeNewItemNode;
end;

procedure TfrmMain.CreateEditor;
var
  EV : IEditorEvents;
  V  : IEditorView;
begin
  FEditorSettings := TEditorFactories.CreateSettings(Self);
  FEditorSettings.FileName := SETTINGS_FILE;
  FEditorSettings.Load;
  FEditorManager := TEditorFactories.CreateManager(Self, FEditorSettings);
  V := TEditorFactories.CreateView(pnlEditor, FEditorManager, 'Editor');
  V.IsFile := False;
  V.Editor.PopupMenu  := FEditorManager.Menus.EditorPopupMenu;
  btnHighlighter.Menu := FEditorManager.Menus.HighlighterPopupMenu;
  FEditorManager.Settings.AutoFormatXML := False;
  FEditorManager.Settings.AutoGuessHighlighterType := False;
  EV := FEditorManager.Events;
  EV.OnNew := ENew;
  EV.OnBeforeSave := EBeforeSave;
  EV.AddOnChangeHandler(EChange);
  EV.AddOnHighlighterChangeHandler(EHighlighterChange);
end;

procedure TfrmMain.CreateRichEditor;
var
  RV: IRichEditorView;
begin
  FRichEditorManager := TRichEditorFactories.CreateManager(Self);
  RV := TRichEditorFactories.CreateView(
    pnlComments,
    FRichEditorManager,
    'Comment'
  );
  RV.OnEditingDone     := RVEditingDone;
  RV.OnChange          := RVChange;
  RV.OnSelectionChange := RVSelectionChange;
  RV.PopupMenu         := FRichEditorManager.EditorPopupMenu;
end;
{$ENDREGION}

end.
