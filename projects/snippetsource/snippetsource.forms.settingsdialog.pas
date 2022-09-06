{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

unit SnippetSource.Forms.SettingsDialog;

{$MODE DELPHI}

{ Manages settings for the SnippetSource application. }

interface

uses
  Classes, SysUtils, DB, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ActnList,

  RTTICtrls, DBGrids, ExtCtrls, Buttons, EditBtn, Grids,

  VirtualTrees,

  SnippetSource.Interfaces;

type

  { TfrmSettingsDialog }

  TfrmSettingsDialog = class(TForm)
    {$REGION 'designer controls'}
    aclMain                          : TActionList;
    actCreateNewDatabase             : TAction;
    actDeleteDatabase                : TAction;
    actClose                         : TAction;
    actDatabaseIntegrityCheck        : TAction;
    actDatabaseVacuum                : TAction;
    actDataBaseShrinkMemory          : TAction;
    actCreateDatabaseIndexes         : TAction;
    actCreateDatabaseTables          : TAction;
    actCreateDatabaseTriggers        : TAction;
    actBackupDatabase                : TAction;
    actFontDialog                    : TAction;
    actCleanupHistory: TAction;
    actReloadConfigurationData       : TAction;
    actOpenDatabase                  : TAction;
    actAddGlyphs                     : TAction;
    actRefreshGlyphs                 : TAction;
    btnBackupDatabase                : TBitBtn;
    btnCreateNewDatabase             : TBitBtn;
    btnCreateDatabaseIndexes         : TBitBtn;
    btnCreateDatabaseTables          : TBitBtn;
    btnCreateDatabaseTriggers        : TBitBtn;
    btnDatabaseIntegrityCheck        : TBitBtn;
    btnClose                         : TBitBtn;
    btnDatabaseVacuum                : TBitBtn;
    btnOpenDatabase                  : TBitBtn;
    btnOpenGlyphs                    : TButton;
    btnRefresh                       : TButton;
    Button1: TButton;
    cbxImageList                     : TComboBox;
    chkTrackHistory: TCheckBox;
    chkAutoHideEditorToolBar         : TCheckBox;
    chkAutoHideRichEditor            : TCheckBox;
    chkAutoHideEditor                : TCheckBox;
    chkAutoHideRichEditorToolBar     : TCheckBox;
    chkEmitLogMessages               : TCheckBox;
    dlgOpen                          : TOpenDialog;
    dscGlyph                         : TDatasource;
    dscHighlighter                   : TDatasource;
    edtFontName                      : TEditButton;
    edtDatabaseFile                  : TFileNameEdit;
    dlgFont                          : TFontDialog;
    grdGlyph                         : TDBGrid;
    grdHighlighters                  : TDBGrid;
    grdDBInfo                        : TStringGrid;
    GroupBox1                        : TGroupBox;
    GroupBox2: TGroupBox;
    grpDiagnostics                   : TGroupBox;
    grpLayout                        : TGroupBox;
    grpDatabaseInfo                  : TGroupBox;
    Highlighters                     : TTabSheet;
    imlMain                          : TImageList;
    lblApplicationNeedsToBeRestarted : TLabel;
    lblFontName                      : TLabel;
    lblDataBaseFile                  : TLabel;
    pnlBottom                        : TPanel;
    pgcMain                          : TPageControl;
    tsApplication                    : TTabSheet;
    tsDataBase                       : TTabSheet;
    tsImages                         : TTabSheet;
    vstImageList                     : TVirtualStringTree;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actBackupDatabaseExecute(Sender: TObject);
    procedure actCleanupHistoryExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actCreateDatabaseIndexesExecute(Sender: TObject);
    procedure actCreateDatabaseTablesExecute(Sender: TObject);
    procedure actCreateNewDatabaseExecute(Sender: TObject);
    procedure actDatabaseIntegrityCheckExecute(Sender: TObject);
    procedure actDataBaseShrinkMemoryExecute(Sender: TObject);
    procedure actDatabaseVacuumExecute(Sender: TObject);
    procedure actDeleteDatabaseExecute(Sender: TObject);
    procedure actFontDialogExecute(Sender: TObject);
    procedure actOpenDatabaseExecute(Sender: TObject);
    procedure actAddGlyphsExecute(Sender: TObject);
    procedure actRefreshGlyphsExecute(Sender: TObject);
    procedure actReloadConfigurationDataExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure cbxImageListDrawItem(
      Control : TWinControl;
      Index   : Integer;
      ARect   : TRect;
      State   : TOwnerDrawState
    );
    procedure chkAutoHideEditorClick(Sender: TObject);
    procedure chkAutoHideEditorToolBarClick(Sender: TObject);
    procedure chkAutoHideRichEditorClick(Sender: TObject);
    procedure chkAutoHideRichEditorToolBarClick(Sender: TObject);
    procedure chkEmitLogMessagesClick(Sender: TObject);
    procedure chkTrackHistoryClick(Sender: TObject);
    procedure dscGlyphStateChange(Sender: TObject);
    procedure dscGlyphUpdateData(Sender: TObject);
    procedure edtDatabaseFileAcceptFileName(Sender: TObject; var Value: String);
    procedure edtFontNameButtonClick(Sender: TObject);
    procedure grdGlyphDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure grdGlyphPrepareCanvas(sender: TObject; DataCol: Integer;
      Column: TColumn; AState: TGridDrawState);
    procedure grdHighlightersDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure pgcMainChange(Sender: TObject);

    procedure vstImageListAfterCellPaint(
      Sender         : TBaseVirtualTree;
      TargetCanvas   : TCanvas;
      Node           : PVirtualNode;
      Column         : TColumnIndex;
      const CellRect : TRect
    );
    procedure vstImageListGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );
    {$ENDREGION}

  private
    FData     : IInterface;
    FSettings : ISettings;

    {$REGION 'property access methods'}
    function GetConnection: IConnection;
    function GetDataSet: IDataSet;
    function GetGlyphDS: TDataSet;
    function GetHighlighterDataSet: TDataSet;
    function GetSQLite: ISQLite;
    {$ENDREGION}

    procedure LoadImage(const AFileName, AFieldName: string);
    procedure UpdateDataBaseInfo;

  public
    constructor Create(
      AOwner          : TComponent;
      const AData     : IInterface;
      const ASettings : ISettings
    ); reintroduce; virtual;

    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure UpdateActions; override;

    property Connection: IConnection
      read GetConnection;

    property GlyphDS: TDataSet
      read GetGlyphDS;

    property HighlighterDataSet: TDataSet
      read GetHighlighterDataSet;

    property SQLite: ISQLite
      read GetSQLite;

    property DataSet: IDataSet
      read GetDataSet;

  end;

procedure ExecuteSettingsDialog(
  const AData     : IInterface;
  const ASettings : ISettings
);

implementation

{$R *.lfm}

uses
  ts.Core.Utils, ts.Core.Logger,
  SnippetSource.Resources;

var
  FSettingsDialog : TfrmSettingsDialog;

procedure ExecuteSettingsDialog(const AData: IInterface;
  const ASettings: ISettings);
begin
  if not Assigned(FSettingsDialog) then
    FSettingsDialog := TfrmSettingsDialog.Create(Application, AData, ASettings);
  FSettingsDialog.ShowModal;
end;

{$REGION 'construction and destruction'}
constructor TfrmSettingsDialog.Create(AOwner: TComponent;
  const AData: IInterface; const ASettings: ISettings);
begin
  inherited Create(AOwner);
  FData     := AData;
  FSettings := ASettings;
  dscGlyph.DataSet := (FData as IGlyphs).GlyphDataSet;
  (FData as IGlyphs).GlyphDataSet.Active := True;
  dscHighlighter.DataSet := (FData as IHighlighters).HighlighterDataSet;
  //dscHighlighter.DataSet.Active := True;
end;

procedure TfrmSettingsDialog.AfterConstruction;
//var
//  I : Integer;
begin
  inherited AfterConstruction;
  edtDatabaseFile.FileName := Connection.FileName;
  pgcMain.ActivePageIndex := 0;
  chkAutoHideEditorToolBar.Checked     := FSettings.AutoHideEditorToolBar;
  chkAutoHideRichEditorToolBar.Checked := FSettings.AutoHideRichEditorToolBar;
  chkAutoHideRichEditor.Checked        := FSettings.AutoHideRichEditor;
  chkEmitLogMessages.Checked           := FSettings.EmitLogMessages;
  chkTrackHistory.Checked              := FSettings.TrackHistory;
  pgcMain.ActivePage                   := tsApplication;
  edtFontName.Text                     := FSettings.DefaultRichEditorFontName;
  //vstImageList.RootNodeCount :=  (FData as IGlyphs).ImageList.Count;
  //cbxImageList.Clear;
  //for I := 0 to (FData as IGlyphs).ImageList.Count - 1 do
  //begin
  //  cbxImageList.AddItem('', nil);
  //end;
  UpdateDataBaseInfo;
end;

destructor TfrmSettingsDialog.Destroy;
begin
  (FData as IGlyphs).GlyphDataSet.Active := False;
  FData     := nil;
  FSettings := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmSettingsDialog.GetConnection: IConnection;
begin
  Result := FData as IConnection;
end;

function TfrmSettingsDialog.GetDataSet: IDataSet;
begin
  Result := FData as IDataSet;
end;

function TfrmSettingsDialog.GetGlyphDS: TDataSet;
begin
  Result := (FData as IGlyphs).GlyphDataSet;
end;

function TfrmSettingsDialog.GetHighlighterDataSet: TDataSet;
begin
  Result := (FData as IHighlighters).HighlighterDataSet;
end;

function TfrmSettingsDialog.GetSQLite: ISQLite;
begin
  Result := FData as ISQLite;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmSettingsDialog.actOpenDatabaseExecute(Sender: TObject);
begin
  FSettings.Database := edtDatabaseFile.FileName;
end;

procedure TfrmSettingsDialog.actAddGlyphsExecute(Sender: TObject);
var
  LFile: string;
  LExt : string;
begin
  GlyphDS.Active := True;
  if dlgOpen.Execute then
  begin
    Connection.BeginBulkInserts;
    GlyphDS.DisableControls;
    for LFile in dlgOpen.Files do
    begin
      if FileExists(LFile) then
      begin
        if not (GlyphDS.State in dsEditModes) then
          GlyphDS.Append;
        LoadImage(LFile, 'Image');
        LExt := ExtractFileExt(LFile);
        LExt := LowerCase(StringReplace(LExt, '.', '', []));
        GlyphDS.FieldByName('Name').AsString := ExtractFileName(LFile);
        GlyphDS.Post;
      end;
    end;
    GlyphDS.EnableControls;
    Connection.EndBulkInserts;
  end;
end;

procedure TfrmSettingsDialog.actRefreshGlyphsExecute(Sender: TObject);
begin
  GlyphDS.Refresh;
  //(FData as IGlyphs).LoadGlyphs;
  //vstImageList.RootNodeCount :=  (FData as IGlyphs).ImageList.Count;
end;

procedure TfrmSettingsDialog.actReloadConfigurationDataExecute(Sender: TObject);
begin
  Connection.SetupConfigurationData;
end;

procedure TfrmSettingsDialog.actCreateNewDatabaseExecute(Sender: TObject);
begin
  Connection.FileName := edtDatabaseFile.FileName;
  Connection.CreateNewDatabase;
end;

procedure TfrmSettingsDialog.actDatabaseIntegrityCheckExecute(Sender: TObject);
begin
  if SQLite.IntegrityCheck then
    ShowMessage(SDatabaseIntegrityCheckSuccessful)
  else
    ShowMessage(SDatabaseIntegrityCheckFailed);
  UpdateDataBaseInfo;
end;

procedure TfrmSettingsDialog.actDataBaseShrinkMemoryExecute(Sender: TObject);
begin
  SQLite.ShrinkMemory;
end;

procedure TfrmSettingsDialog.actDatabaseVacuumExecute(Sender: TObject);
var
  LStartSize : Int64;
  S          : string;
begin
  LStartSize := SQLite.Size;
  SQLite.Vacuum;
  S := FormatByteText(LStartSize - SQLite.Size);
  ShowMessageFmt(SDatabaseSizeHasBeenReduced, [S]);
  UpdateDataBaseInfo;
end;

procedure TfrmSettingsDialog.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmSettingsDialog.actBackupDatabaseExecute(Sender: TObject);
var
  S : string;
begin
  S := Connection.BackupDatabase;
  ShowMessageFmt(SDatabaseBackupCreated, [S]);
end;

procedure TfrmSettingsDialog.actCleanupHistoryExecute(Sender: TObject);
begin
   Connection.CleanupHistory;
end;

procedure TfrmSettingsDialog.actCreateDatabaseIndexesExecute(Sender: TObject);
begin
  Connection.CreateDatabaseIndexes;
  ShowMessage(SDatabaseIndexesRebuilt)
end;

procedure TfrmSettingsDialog.actCreateDatabaseTablesExecute(Sender: TObject);
begin
  if AskConfirmation(SAskRecreateTables) then
  begin
    Connection.CreateDatabaseTables;
  end;
end;

procedure TfrmSettingsDialog.actDeleteDatabaseExecute(Sender: TObject);
begin
  if FileExists(edtDatabaseFile.FileName) then
    DeleteFile(edtDatabaseFile.FileName);
end;

procedure TfrmSettingsDialog.actFontDialogExecute(Sender: TObject);
begin
  if dlgFont.Execute then
  begin
    edtFontName.Text := dlgFont.Font.Name;
    FSettings.DefaultRichEditorFontName := dlgFont.Font.Name;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmSettingsDialog.cbxImageListDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  //Pos : Integer;
  C   : TComboBox;
begin
  C := Control as TComboBox;
  // This ensures the correct highlight color is used
  C.Canvas.FillRect(ARect);
  //Pos := ARect.Left + ((ARect.Right - ARect.Left) div 2) - 8;
  // This line draws the actual bitmap
  //(FData as IGlyphs).ImageList.Draw(C.Canvas , Pos, ARect.Top, Index);

  //  This line writes the text after the bitmap*)
  //combobox1.canvas.textout(rect.left+imlMain.width+2,rect.top,
  //                        combobox1.items[index]);



  //Pos := ARect.Left + ((ARect.Right - ARect.Left) div 2) - 8;

end;

procedure TfrmSettingsDialog.chkAutoHideEditorClick(Sender: TObject);
begin
  FSettings.AutoHideEditor := (Sender as TCheckBox).Checked;
end;

procedure TfrmSettingsDialog.chkAutoHideEditorToolBarClick(Sender: TObject);
begin
  FSettings.AutoHideEditorToolBar := (Sender as TCheckBox).Checked;
end;

procedure TfrmSettingsDialog.chkAutoHideRichEditorClick(Sender: TObject);
begin
  FSettings.AutoHideRichEditor := (Sender as TCheckBox).Checked;
end;

procedure TfrmSettingsDialog.chkAutoHideRichEditorToolBarClick(Sender: TObject);
begin
  FSettings.AutoHideRichEditorToolBar := (Sender as TCheckBox).Checked;
end;

procedure TfrmSettingsDialog.chkEmitLogMessagesClick(Sender: TObject);
begin
  FSettings.EmitLogMessages := (Sender as TCheckBox).Checked;
end;

procedure TfrmSettingsDialog.chkTrackHistoryClick(Sender: TObject);
begin
  FSettings.TrackHistory := (Sender as TCheckBox).Checked;
end;

procedure TfrmSettingsDialog.dscGlyphStateChange(Sender: TObject);
begin
  //(FData as IGlyphs).LoadGlyphs;
  //vstImageList.RootNodeCount :=  (FData as IGlyphs).ImageList.Count;
end;

procedure TfrmSettingsDialog.dscGlyphUpdateData(Sender: TObject);
begin
  //(FData as IGlyphs).LoadGlyphs;
  //vstImageList.RootNodeCount :=  (FData as IGlyphs).ImageList.Count;
end;

procedure TfrmSettingsDialog.edtDatabaseFileAcceptFileName(Sender: TObject;
  var Value: String);
begin
  if FileExists(Value) then
    FSettings.Database := Value;
end;

procedure TfrmSettingsDialog.edtFontNameButtonClick(Sender: TObject);
begin
  actFontDialog.Execute;
end;

procedure TfrmSettingsDialog.grdGlyphDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  P  : TPicture;
  F  : TBlobField;
  MS : TMemoryStream;
begin
  if Column.FieldName = 'Image' then
  begin
    with Sender as TDBGrid do
    begin
      F  := TBlobField(GlyphDS.FieldByName('Image'));
      if not F.IsNull then
      begin
        P := TPicture.Create;
        try
          MS := TMemoryStream.Create;
          try
            F.SaveToStream(MS);
            MS.Position := 0;
            P.LoadFromStream(MS);
            Canvas.FillRect(Rect);
            Canvas.Draw(Rect.Left + (Rect.Width div 2) - 8 , Rect.Top, P.Graphic);
          finally
            FreeAndNil(MS);
          end;
        finally
          FreeAndNil(P);
        end;
      end;
    end;
  end;
end;

procedure TfrmSettingsDialog.grdGlyphPrepareCanvas(sender: TObject;
  DataCol: Integer; Column: TColumn; AState: TGridDrawState);
begin

end;

procedure TfrmSettingsDialog.grdHighlightersDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  P  : TPicture;
  F  : TBlobField;
  MS : TMemoryStream;
begin
  if Column.FieldName = 'Image' then
  begin
    with Sender as TDBGrid do
    begin
      F := TBlobField(HighlighterDataSet.FieldByName('Image'));
      Canvas.FillRect(Rect);
      if not F.IsNull then
      begin
        P := TPicture.Create;
        try
          MS := TMemoryStream.Create;
          try
            F.SaveToStream(MS);
            MS.Position := 0;
            P.LoadFromStream(MS);
            Canvas.Draw(Rect.Left + (Rect.Width div 2) - 8 , Rect.Top, P.Graphic);
          finally
            FreeAndNil(MS);
          end;
        finally
          FreeAndNil(P);
        end;
      end
    end;
  end;
end;

procedure TfrmSettingsDialog.pgcMainChange(Sender: TObject);
begin
  UpdateDataBaseInfo;
end;

//procedure TfrmSettingsDialog.grdGlyphAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const CellRect: TRect);
//var
//  Pos: Integer;
//begin
//  if Column = 3 then
//  begin
//    Pos := CellRect.Left + ((CellRect.Right - CellRect.Left) div 2) - 8;
//    (FData as IGlyphs).GlyphList.Draw(TargetCanvas, Pos, CellRect.Top, Node^.Index);
//  end;
//end;

//procedure TfrmSettingsDialog.grdGlyphGetRecordCount(Sender: TCustomVirtualDBGrid; var RecordCount: longint);
//begin
//    vstImageList.RootNodeCount :=  (FData as IGlyphs).ImageList.Count;
//end;

//procedure TfrmSettingsDialog.grdHighlightersAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const CellRect: TRect);
//var
//  Pos: Integer;
//begin
//  if Column = 3 then
//  begin
//    Pos := CellRect.Left + ((CellRect.Right - CellRect.Left) div 2) - 8;
//    (FData as IGlyphs).ImageList.Draw(TargetCanvas, Pos, CellRect.Top, StrToIntDef(grdHighlighters.Text[Node, Column-1], 0));
//  end;
//end;
//
procedure TfrmSettingsDialog.vstImageListAfterCellPaint(Sender:
  TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column:
  TColumnIndex; const CellRect: TRect);
//var
//  Pos: Integer;
begin
  //if Column = 1 then
  //begin
  //  Pos := CellRect.Left + ((CellRect.Right - CellRect.Left) div 2) - 8;
  //  (FData as IGlyphs).ImageList.Draw(TargetCanvas, Pos, CellRect.Top, Node^.Index);
  //end;
end;

procedure TfrmSettingsDialog.vstImageListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  if Column = 0 then
    CellText := IntToStr(Node^.Index)
  else
    CellText := '';
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmSettingsDialog.LoadImage(const AFileName, AFieldName: string);
var
  F  : TBlobField;
  MS : TMemoryStream;
  P  : TPicture;
begin
  if FileExists(AFileName) then
  begin
    F := TBlobField(GlyphDS.FieldByName(AFieldName));
    MS := TMemoryStream.Create;
    try
      P := TPicture.Create;
      try
        P.LoadFromFile(AFileName);
        P.Graphic.SaveToStream(MS);
        MS.Position := 0;
        F.LoadFromStream(MS);
      finally
        FreeAndNil(P);
      end;
    finally
      FreeAndNil(MS);
    end;
  end;
end;

procedure TfrmSettingsDialog.UpdateDataBaseInfo;
begin
  grdDBInfo.Cells[1, 0] := SQLite.DBVersion;
  grdDBInfo.Cells[1, 1] := FormatByteText(SQLite.Size);
  grdDBInfo.Cells[1, 2] :=
    DateTimeToStr(GetFileCreationTime(Connection.FileName));
  grdDBInfo.Cells[1, 3] :=
    DateTimeToStr(FileDateToDateTime(FileAge(Connection.FileName)));
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmSettingsDialog.UpdateActions;
var
  S : string;
  B : Boolean;
begin
  inherited UpdateActions;
  S := edtDatabaseFile.FileName;
  B := FileExists(S);
  actOpenDatabase.Enabled      := B and (Connection.FileName <> S);
  actCreateNewDatabase.Enabled := not B;
  actDeleteDatabase.Enabled    := B;
end;
{$ENDREGION}

end.

