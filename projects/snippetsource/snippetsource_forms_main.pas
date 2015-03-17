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

unit SnippetSource_Forms_Main;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, DB, BufDataset, FileUtil, Forms, Controls, Graphics,
  ExtCtrls, ComCtrls, ActnList, StdCtrls, Menus, Buttons,

  SynEdit,

  VirtualTrees,

  ts.Core.VersionInfo,

  ts.Editor.Interfaces, ts.Editor.Highlighters, ts_Editor_View,
  ts.Editor.Factories,

  ts.RichEditor.Helpers, ts.RichEditor.Interfaces,

  MenuButton, // Lui Controls

  SnippetSource_Forms_Lookup, SnippetSource_Forms_VirtualDBTree,

  SnippetSource.Interfaces;

{
  REMARKS:
    - RTF text is stored in Base64 encoding. Maybe needs to be changed...
      To make the RTF text searchable a copy with only the flat text is stored.
}

type

  { TfrmMain }

  TfrmMain = class(TForm)
    {$region 'designer controls' /fold}
    aclMain            : TActionList;
    actExecute         : TAction;
    actAbout           : TAction;
    actSettings        : TAction;
    actToggleMaximize  : TAction;
    actToggleStayOnTop : TAction;
    actLookup          : TAction;
    btnHighlighterSB   : TSpeedButton;
    btnLineBreakStyle  : TSpeedButton;
    edtSearch          : TEdit;
    edtTitle           : TEdit;
    dscMain            : TDatasource;
    imgNode            : TImage;
    imlMain            : TImageList;
    imlNodes           : TImageList;
    btnHighlighter     : TMenuButton;
    pnlComments        : TPanel;
    pnlEditMode        : TPanel;
    pnlEditor          : TPanel;
    pnlHighlighter     : TPanel;
    pnlID              : TPanel;
    pnlProgress        : TPanel;
    pnlSnippetcount    : TPanel;
    pnlLeft            : TPanel;
    pnlLineBreakStyle  : TPanel;
    pnlPosition        : TPanel;
    pnlRight           : TPanel;
    pnlSearch          : TPanel;
    pnlSize            : TPanel;
    pnlStatusBar       : TPanel;
    pnlTitle           : TPanel;
    shpRect            : TShape;
    splHorizontal      : TSplitter;
    splVertical        : TSplitter;
    tlbEditorView      : TToolBar;
    tlbRichEditorView  : TToolBar;
    tlbApplication     : TToolBar;
    ToolButton1: TToolButton;
    {$endregion}

    procedure actAboutExecute(Sender: TObject);
    procedure actLookupExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actToggleMaximizeExecute(Sender: TObject);
    procedure actToggleStayOnTopExecute(Sender: TObject);
    procedure btnHighlighterSBClick(Sender: TObject);
    procedure btnHighlighterMouseEnter(Sender: TObject);
    procedure btnHighlighterMouseLeave(Sender: TObject);
    procedure btnLineBreakStyleClick(Sender: TObject);
    procedure dscMainDataChange(Sender: TObject; Field: TField);

    procedure edtTitleEditingDone(Sender: TObject);
    procedure edtTitleMouseEnter(Sender: TObject);
    procedure edtTitleMouseLeave(Sender: TObject);
    procedure FileSearcherDirectoryFound(FileIterator: TFileIterator);
    procedure FileSearcherFileFound(FileIterator: TFileIterator);
    procedure OnTreeDropFiles(
      Sender      : TBaseVirtualTree;
      AFiles      : TStrings;
      AAttachMode : TVTNodeAttachMode
    );

    procedure EChange(Sender: TObject);
    procedure EHighlighterChange(Sender: TObject);
    procedure EModified(Sender: TObject);
    procedure EBeforeSave(
          Sender       : TObject;
      var AStorageName : string
    );
    procedure ENew(
            Sender    : TObject;
      var   AFileName : string;
      const AText     : string
    );

    procedure RVChange(Sender: TObject);
    procedure RVEditingDone(Sender: TObject);

  private
    FLoggingEnabled : Boolean;
    FTree           : TfrmVirtualDBTree;
    FVersionInfo    : TVersionInfo;
    FFileSearcher   : TFileSearcher;
    FParentId       : Integer;
    FCommonPath     : string;
    FData           : IInterface;
    FManager        : IEditorManager;
    FSettings       : IEditorSettings;

    function GetConnection: IConnection;
    function GetDataSet: IDataSet;
    function GetEditor: IEditorView;
    function GetRichEditor: IRichEditorView;
    function GetSnippet: ISnippet;

    procedure AddPathNode(
      const APath       : string;
      const ACommonPath : string;
            ATree       : TBaseVirtualTree
    );
    procedure ExportNode;

  protected
    procedure HideAction(const AActionName: string);
    procedure AssignEditorChanges;
    procedure UpdateStatusBar;
    procedure UpdateActions; override;
    procedure AddButton(
      const AActionName : string;
            APopupMenu  : TPopupMenu = nil
    ); overload;
    procedure AddButton(AAction: TBasicAction); overload;
    procedure BuildToolBar;
    procedure InitActions;

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

  SynEditTypes,

  ts.Core.Utils, ts.Core.SharedLogger,

  ts_Editor_Manager, ts_Editor_AboutDialog,

  ts_Richeditor_Manager,

  SnippetSource_Forms_SQLLog, SnippetSource_Forms_SettingsDialog,
  SnippetSource_Modules_Data;

{$region 'construction and destruction' /fold}
procedure TfrmMain.AfterConstruction;
var
  EV : IEditorEvents;
  V  : IEditorView;
  RV : IRichEditorView;
begin
  inherited AfterConstruction;

  FData := TdmMain.Create(Self);
  FSettings := TEditorFactories.CreateSettings(Self);
  FSettings.FileName := 'settings.xml';
  FSettings.Load;
  FManager := TEditorFactories.CreateManager(Self, FSettings);

  FFileSearcher := TFileSearcher.Create;
  FFileSearcher.OnDirectoryFound := FileSearcherDirectoryFound;
  FFileSearcher.OnFileFound      := FileSearcherFileFound;

  V := TEditorFactories.CreateView(pnlEditor, FManager, 'Editor');
  V.IsFile := False;
  V.Editor.PopupMenu  := FManager.Menus.EditorPopupMenu;
  btnHighlighter.Menu := FManager.Menus.HighlighterPopupMenu;
  FManager.Settings.AutoFormatXML := False;
  FManager.Settings.AutoGuessHighlighterType := False;

  RV := CreateRichEditorView(pnlComments, 'Comment');
  RV.OnEditingDone  := RVEditingDone;
  RV.OnChange  := RVChange;
  RV.PopupMenu := RichEditorActions.EditorPopupMenu;

  EV := FManager.Events;
  EV.OnNew := ENew;
  EV.OnBeforeSave := EBeforeSave;
  EV.AddOnChangeHandler(EChange);
  EV.AddOnModifiedHandler(EModified);
  EV.AddOnHighlighterChangeHandler(EHighlighterChange);

  BuildToolBar;
  InitActions;
  dscMain.DataSet := DataSet.DataSet;

  FTree := TfrmVirtualDBTree.Create(Self);
  FTree.DoubleBuffered := True;
  FTree.Parent := pnlLeft;
  FTree.BorderStyle := bsNone;
  FTree.Align := alClient;
  FTree.Visible := True;
  FTree.DataSet := DataSet.DataSet;
  DataSet.Active := True;
  FTree.ImageList := (FData as IGlyphs).ImageList;
  FTree.OnDropFiles  := OnTreeDropFiles;

  BuildStandardRichEditorToolbar(tlbRichEditorView);

  pnlHighlighter.PopupMenu    := FManager.Menus.HighlighterPopupMenu;
  btnHighlighterSB.PopupMenu  := FManager.Menus.HighlighterPopupMenu;
  btnLineBreakStyle.PopupMenu := FManager.Menus.LineBreakStylePopupMenu;

  FVersionInfo := TVersionInfo.Create(Self);
  Caption := Format('%s %s', [ApplicationName, FVersionInfo.FileVersion]);

  SetWindowSizeGrip(pnlStatusBar.Handle, True);
end;

procedure TfrmMain.BeforeDestruction;
begin
  FSettings.Save;
  FData := nil;
  FManager := nil;
  FSettings := nil;
  FreeAndNil(FFileSearcher);
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'property access mehods' /fold}
function TfrmMain.GetRichEditor: IRichEditorView;
begin
  Result := RichEditorActions.ViewByName['Comment'];
end;

function TfrmMain.GetEditor: IEditorView;
begin
  Result := FManager.Views.ViewByName['Editor'];
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
{$endregion}

{$region 'action handlers' /fold}
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

procedure TfrmMain.actToggleMaximizeExecute(Sender: TObject);
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
{$endregion}

{$region 'event handlers' /fold}
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

procedure TfrmMain.EChange(Sender: TObject);
begin
  Logger.Send('EChange');
  DataSet.Edit;
  AssignEditorChanges;
end;

procedure TfrmMain.EHighlighterChange(Sender: TObject);
begin
  DataSet.Edit;
  AssignEditorChanges;
  //Snippet.Highlighter := Editor.HighlighterName;
  //DataSet.Post;
end;

procedure TfrmMain.EModified(Sender: TObject);
begin
  Logger.Send('EModified');
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
  if Field = nil then
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

       //imlNodes.GetBitmap(DS.FieldByName('NodeTypeID').AsInteger,imgNode.Picture.Bitmap);

        if Snippet.CommentRTF <> '' then
        begin
          S := DecodeStringBase64(Snippet.CommentRTF);
          SS := TStringStream.Create(S);
          try
            SS.Position := 0;
            try
              if SS.Size > 0 then
                RichEditor.LoadFromStream(SS);
            except
              RichEditor.Clear;
              RichEditor.Editor.Text := 'Error loading RTF stream';
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

procedure TfrmMain.edtTitleEditingDone(Sender: TObject);
begin
  Snippet.NodeName := edtTitle.Text;
end;

procedure TfrmMain.edtTitleMouseEnter(Sender: TObject);
begin
  edtTitle.Color := clWhite;
end;

procedure TfrmMain.edtTitleMouseLeave(Sender: TObject);
begin
  edtTitle.Color := clInfoBk;
  Editor.SetFocus;
end;

procedure TfrmMain.FileSearcherDirectoryFound(FileIterator: TFileIterator);
begin
  AddPathNode(FileIterator.FileName, FCommonPath, FTree.TreeView);
end;

procedure TfrmMain.FileSearcherFileFound(FileIterator: TFileIterator);
begin
  AddPathNode(FileIterator.FileName, FCommonPath, FTree.TreeView);
end;

procedure TfrmMain.OnTreeDropFiles(Sender: TBaseVirtualTree;
  AFiles: TStrings; AAttachMode: TVTNodeAttachMode);
var
  I     : Integer;
  T     : Integer;
  sFile : string;
begin
  DataSet.DisableControls;
  FCommonPath := GetCommonPath(AFiles);
  T := Snippet.NodeTypeID;
  if T = 1 then // FOLDER
  begin
    AAttachMode := amAddChildLast;
    FParentId := Snippet.Id;
  end
  else
  begin
    AAttachMode := amInsertAfter;
    FParentId := Snippet.ParentID;
  end;
  for I := 0 to AFiles.Count - 1 do
  begin
    sFile := AFiles[I];
    if DirPathExists(sFile) then // add files in directory
    begin
      AddPathNode(sFile, FCommonPath, Sender);
      FFileSearcher.Search(sFile);
    end
    else
    begin
      AddPathNode(sFile, FCommonPath, Sender);
    end;
  end;
  DataSet.EnableControls;
  Sender.Refresh;
end;

procedure TfrmMain.RVChange(Sender: TObject);
begin
  DataSet.Edit;
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
    Snippet.CommentRTF := S;
    Snippet.Comment := RichEditor.Editor.Lines.Text;
  finally
    FreeAndNil(SS);
  end;
end;
{$endregion}

{$region 'private methods' /fold}
procedure TfrmMain.AddPathNode(const APath: string; const ACommonPath: string;
  ATree: TBaseVirtualTree);
var
  IsDir       : Boolean;
  IsTextFile  : Boolean;
  AttachMode  : TVTNodeAttachMode;
  V           : Variant;
  sRelPath    : string;
  sParentPath : string;
  sFileName   : string;
  bReadable   : Boolean;
begin
  pnlProgress.Caption := Format('Loading file: %s', [APath]);
  Application.ProcessMessages;
  IsTextFile := False;
  IsDir      := False;
  if DirPathExists(APath) then
  begin
    IsDir := True;
  end
  else if FileIsText(APath, bReadable) and bReadable then
  begin
    IsTextFile := True;
  end;

  if IsDir or IsTextFile then
  begin
    sRelPath := CreateRelativePath(APath, ACommonPath);
    sParentPath := ChompPathDelim(GetParentDir(sRelPath));
    if sParentPath <> '' then
    begin
      V := DataSet.DataSet.Lookup('NodePath', sParentPath, 'Id');
      if V <> Null then
        FParentId := V;
    end;
  end;
  try
    sFileName := Trim(ExtractFileName(APath));
    if sFileName <> '' then
    begin
      if IsTextFile then
      begin
        DataSet.Append;
        Snippet.Text        := ReadFileToString(APath);
        Snippet.Highlighter := DelChars(UpperCase(ExtractFileExt(APath)), '.');
        Snippet.NodeName    := sFileName;
        Snippet.NodeTypeID  := 2;
        Snippet.ParentID    := FParentId;
        DataSet.Post;
      end
      else if IsDir then
      begin
        DataSet.Append;
        Snippet.NodeName   := sFileName;
        Snippet.NodeTypeID := 1;
        Snippet.NodePath   := sRelPath;
        Snippet.ParentID   := FParentId;
        DataSet.Post;
      end;
      if IsDir then
        FParentId := Snippet.ID
      else if IsTextFile then
        FParentId := Snippet.ParentID;
    end;
  except
  end;
  pnlProgress.Caption := '';
  Application.ProcessMessages;
end;

procedure TfrmMain.ExportNode;
begin
  if Snippet.NodeTypeID = 2 then
    Editor.Save(Snippet.NodeName);
end;

procedure TfrmMain.HideAction(const AActionName: string);
begin
  //FManager.Actions[AActionName].Enabled := False;
  //FManager.Actions[AActionName].Visible := False;
end;
{$endregion}

{$region 'protected methods' /fold}
procedure TfrmMain.AssignEditorChanges;
begin
  Snippet.Text := Editor.Text;
  Snippet.FoldLevel := Editor.FoldLevel;
  if Assigned(Editor.HighlighterItem) then
  begin
    Snippet.Highlighter := Editor.HighlighterItem.Name;
    btnHighlighter.Caption := Editor.HighlighterItem.Name;
  end;
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
    TB.Action := FManager.Actions[AActionName];
  end
  else
  begin
    if AActionName = '' then
    begin
      TB.Style := tbsDivider;
    end
    else
      TB.Action := FManager.Actions[AActionName];
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
  tlbEditorView.Images := FManager.Actions.ActionList.Images;
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
  AddButton('actToggleFoldLevel', FManager.Menus.FoldPopupMenu);
  AddButton('actToggleHighlighter', FManager.Menus.HighlighterPopupMenu);
  AddButton('');
  AddButton('actAbout');
end;

procedure TfrmMain.InitActions;
begin
  HideAction('actAlignSelection');
  HideAction('actSortSelection');
  HideAction('actInsertCharacterFromMap');
  HideAction('actInsertColorValue');
  HideAction('actSearch');
  HideAction('actFindAllOccurences');
  HideAction('actSearchReplace');
  HideAction('actFindNext');
  HideAction('actFindPrevious');
  HideAction('actShowViews');
  HideAction('actShowActions');
  HideAction('actShowPreview');
  HideAction('actShowMiniMap');
  HideAction('actShowHTMLViewer');
  HideAction('actShowStructureViewer');
  HideAction('actShowHexEditor');
  HideAction('actShowScriptEditor');
  HideAction('actShapeCode');
  HideAction('actFilterCode');
  HideAction('actSmartSelect');
  HideAction('actFormat');
  HideAction('actAutoGuessHighlighter');
  HideAction('actCreateDesktopLink');
  HideAction('actMonitorChanges');
end;
{$endregion}

end.
