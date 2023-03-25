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

unit ts.RichEditor.Manager;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, FileUtil, ActnList, Dialogs, Menus, Contnrs, Forms,
  Controls,

  KMemoDlgTextStyle, KMemoDlgHyperlink, KMemoDlgImage, KMemoDlgNumbering,
  KMemoDlgContainer, KMemoDlgParaStyle,

  ts.RichEditor.Interfaces;

type
  TRichEditorViewList = TComponentList;

  TdmRichEditorManager = class(
    TDataModule, IRichEditorManager, IRichEditorActions, IRichEditorEvents
  )
    {$REGION 'designer controls'}
    aclActions               : TActionList;
    actAddParagraph          : TAction;
    actAlignCenter           : TAction;
    actAlignJustify          : TAction;
    actAlignLeft             : TAction;
    actAlignRight            : TAction;
    actBold                  : TAction;
    actBulletList            : TAction;
    actClear                 : TAction;
    actClipboardMenu         : TAction;
    actCopy                  : TAction;
    actCut                   : TAction;
    actDecFontSize           : TAction;
    actDecIndent             : TAction;
    actDeleteColumn          : TAction;
    actDeleteRow             : TAction;
    actEditParagraphStyle    : TAction;
    actEditSelectedItem      : TAction;
    actEditTextStyle         : TAction;
    actFileMenu              : TAction;
    actIncFontSize           : TAction;
    actIncIndent             : TAction;
    actInsertBulletList      : TAction;
    actInsertColumnAfter     : TAction;
    actInsertColumnBefore    : TAction;
    actInsertHyperLink       : TAction;
    actInsertImage           : TAction;
    actInsertMenu            : TAction;
    actInsertRowAfter        : TAction;
    actInsertRowBefore       : TAction;
    actInsertTable           : TAction;
    actShowStructureViewer   : TAction;
    actItalic                : TAction;
    actNumberedList          : TAction;
    actOpen                  : TAction;
    actPaste                 : TAction;
    actRedo                  : TAction;
    actSave                  : TAction;
    actSaveAs                : TAction;
    actSelectAll             : TAction;
    actSelectionMenu         : TAction;
    actSelectMenu            : TAction;
    actSelectTable           : TAction;
    actSetBackgroundColor    : TAction;
    actSetFont               : TAction;
    actSetFontColor          : TAction;
    actSettingsMenu          : TAction;
    actShowPreview           : TAction;
    actShowSpecialCharacters : TAction;
    actStrikeThrough         : TAction;
    actTableMenu             : TAction;
    actToggleWordWrap        : TAction;
    actUnderline             : TAction;
    actUndo                  : TAction;
    dlgColor                 : TColorDialog;
    dlgFont                  : TFontDialog;
    dlgOpen                  : TOpenDialog;
    dlgSave                  : TSaveDialog;
    imlMain                  : TImageList;
    ppmClipboard             : TPopupMenu;
    ppmFile                  : TPopupMenu;
    ppmInsert                : TPopupMenu;
    ppmRichEditor            : TPopupMenu;
    ppmSelect                : TPopupMenu;
    ppmSelection             : TPopupMenu;
    ppmSettings              : TPopupMenu;
    ppmTable                 : TPopupMenu;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure aclActionsExecute(AAction: TBasicAction; var Handled: Boolean);
    procedure actAddParagraphExecute(Sender: TObject);
    procedure actAlignCenterExecute(Sender: TObject);
    procedure actAlignJustifyExecute(Sender: TObject);
    procedure actAlignLeftExecute(Sender: TObject);
    procedure actAlignRightExecute(Sender: TObject);
    procedure actBoldExecute(Sender: TObject);
    procedure actBulletListExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actDecFontSizeExecute(Sender: TObject);
    procedure actDecIndentExecute(Sender: TObject);
    procedure actDeleteColumnExecute(Sender: TObject);
    procedure actDeleteRowExecute(Sender: TObject);
    procedure actEditParagraphStyleExecute(Sender: TObject);
    procedure actEditSelectedItemExecute(Sender: TObject);
    procedure actEditTextStyleExecute(Sender: TObject);
    procedure actIncFontSizeExecute(Sender: TObject);
    procedure actIncIndentExecute(Sender: TObject);
    procedure actInsertBulletListExecute(Sender: TObject);
    procedure actInsertColumnAfterExecute(Sender: TObject);
    procedure actInsertColumnBeforeExecute(Sender: TObject);
    procedure actInsertHyperLinkExecute(Sender: TObject);
    procedure actInsertImageExecute(Sender: TObject);
    procedure actInsertRowAfterExecute(Sender: TObject);
    procedure actInsertRowBeforeExecute(Sender: TObject);
    procedure actInsertTableExecute(Sender: TObject);
    procedure actItalicExecute(Sender: TObject);
    procedure actNumberedListExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSelectTableExecute(Sender: TObject);
    procedure actSetBackgroundColorExecute(Sender: TObject);
    procedure actSetFontColorExecute(Sender: TObject);
    procedure actSetFontExecute(Sender: TObject);
    procedure actShowPreviewExecute(Sender: TObject);
    procedure actShowSpecialCharactersExecute(Sender: TObject);
    procedure actShowStructureViewerExecute(Sender: TObject);
    procedure actStrikeThroughExecute(Sender: TObject);
    procedure actToggleWordWrapExecute(Sender: TObject);
    procedure actUnderlineExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    {$ENDREGION}

  private
    FViews      : TRichEditorViewList;
    FActiveView : IRichEditorView;
    FEvents     : IRichEditorEvents;
    FToolViews  : IRichEditorToolViews;

  protected
    {$REGION 'property access mehods'}
    function GetActionList: TActionList;
    function GetActions: IRichEditorActions;
    function GetToolViews: IRichEditorToolViews;
    //function GetActions: TActionList;
    function GetActiveView: IRichEditorView;
    function GetClipboardPopupMenu: TPopupMenu;
    function GetEditorPopupMenu: TPopupMenu;
    function GetEvents: IRichEditorEvents;
    function GetFilePopupMenu: TPopupMenu;
    function GetInsertPopupMenu: TPopupMenu;
    function GetItem(AName: string): TContainedAction;
    function GetSelectionPopupMenu: TPopupMenu;
    function GetSelectPopupMenu: TPopupMenu;
    function GetSettingsPopupMenu: TPopupMenu;
    function GetTablePopupMenu: TPopupMenu;
    function GetView(AIndex: Integer): IRichEditorView;
    function GetViewByName(AName: string): IRichEditorView;
    function GetViewCount: Integer;
    procedure SetActiveView(const AValue: IRichEditorView);
    {$ENDREGION}

    procedure UpdateActions;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    function AddView(
      const AName     : string = '';
      const AFileName : string = ''
    ): IRichEditorView;
    function DeleteView(AIndex: Integer): Boolean;
    procedure ClearViews;

    procedure InitializePopupMenus;
    procedure RegisterToolViews;
    procedure ShowToolView(
      const AName : string;
      AShowModal  : Boolean;
      ASetFocus   : Boolean
    );

    procedure BuildRichEditorPopupMenu;
    procedure BuildInsertPopupMenu;
    procedure BuildTablePopupMenu;
    procedure BuildSelectionPopupMenu;
    procedure BuildSelectPopupMenu;
    procedure BuildFilePopupMenu;
    procedure BuildClipboardPopupMenu;
    procedure BuildSettingsPopupMenu;

    { Delegates the implementation of IEditorEvents to an internal object. }
    property Events: IRichEditorEvents
      read GetEvents implements IRichEditorEvents;

    property ActionList: TActionList
      read GetActionList;

    property Actions: IRichEditorActions
      read GetActions;

    property ActiveView: IRichEditorView
      read GetActiveView write SetActiveView;

    property ToolViews: IRichEditorToolViews
      read GetToolViews;

    property Items[AName: string]: TContainedAction
      read GetItem; default;

    property Views[AIndex: Integer]: IRichEditorView
      read GetView;

    property ViewByName[AName: string]: IRichEditorView
      read GetViewByName;

    property ViewCount: Integer
      read GetViewCount;

    property ClipboardPopupMenu: TPopupMenu
      read GetClipboardPopupMenu;

    property EditorPopupMenu: TPopupMenu
      read GetEditorPopupMenu;

    property FilePopupMenu: TPopupMenu
      read GetFilePopupMenu;

    property TablePopupMenu: TPopupMenu
      read GetTablePopupMenu;

    property InsertPopupMenu: TPopupMenu
      read GetInsertPopupMenu;

    property SelectPopupMenu: TPopupMenu
      read GetSelectPopupMenu;

    property SelectionPopupMenu: TPopupMenu
      read GetSelectionPopupMenu;

    property SettingsPopupMenu: TPopupMenu
      read GetSettingsPopupMenu;

  end;

implementation

{$R *.lfm}

uses
  Graphics,

  ts.Core.Utils, ts.Core.Logger,

  ts.RichEditor.ToolViews, ts.RichEditor.Events, ts.RichEditor.View.KMemo,

  ts.RichEditor.Structure.ToolView;

{$REGION 'construction and destruction'}
procedure TdmRichEditorManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FViews     := TRichEditorViewList.Create(False);
  FEvents    := TRichEditorEvents.Create(Self);
  FToolViews := TRichEditorToolViews.Create(Self);
  actAlignJustify.Visible := False; // not supported yet by KMemo
  actUndo.Visible         := False; // not supported yet by KMemo
  actRedo.Visible         := False; // not supported yet by KMemo
  InitializePopupMenus;
  RegisterToolViews;
end;

destructor TdmRichEditorManager.Destroy;
begin
  FActiveView := nil;
  FEvents     := nil;
  FToolViews  := nil;
  FreeAndNil(FViews);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
//function TdmRichEditorManager.GetActions: TActionList;
//  Result := aclActions;
//begin
//end;

function TdmRichEditorManager.GetActiveView: IRichEditorView;
begin
  if not Assigned(FActiveView) then
    raise Exception.Create('No active view assigned!');
  Result := FActiveView;
end;

procedure TdmRichEditorManager.SetActiveView(const AValue: IRichEditorView);
begin
  if AValue <> FActiveView then
  begin
    FActiveView := AValue;
  end;
end;

function TdmRichEditorManager.GetEditorPopupMenu: TPopupMenu;
begin
  Result := ppmRichEditor;
end;

function TdmRichEditorManager.GetItem(AName: string): TContainedAction;
begin
  Result := aclActions.ActionByName(AName);
end;

function TdmRichEditorManager.GetView(AIndex: Integer): IRichEditorView;
begin
  if (AIndex > -1) and (AIndex < FViews.Count) then
  begin
    Result := FViews[AIndex] as IRichEditorView;
  end
  else
    Result := nil;
end;

function TdmRichEditorManager.GetViewByName(AName: string): IRichEditorView;
var
  I : Integer;
  B : Boolean;
begin
  I := 0;
  B := False;
  while (I < FViews.Count) and not B do
  begin
    B := FViews[I].Name = AName;
    if not B then
      Inc(I);
  end;
  if B then
    Result := FViews[I] as IRichEditorView
  else
    Result := nil;
end;

function TdmRichEditorManager.GetViewCount: Integer;
begin
  Result := FViews.Count;
end;

function TdmRichEditorManager.GetEvents: IRichEditorEvents;
begin
  Result := FEvents;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TdmRichEditorManager.actOpenExecute(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    ActiveView.LoadFromFile(dlgOpen.FileName);
    ActiveView.FileName := dlgOpen.FileName;
  end;
end;

procedure TdmRichEditorManager.actPasteExecute(Sender: TObject);
begin
  ActiveView.Paste;
end;

procedure TdmRichEditorManager.actRedoExecute(Sender: TObject);
begin
  ActiveView.Redo;
end;

procedure TdmRichEditorManager.actSaveAsExecute(Sender: TObject);
begin
  if dlgSave.Execute then
  begin
    ActiveView.SaveToFile(dlgSave.FileName);
    ActiveView.FileName := dlgSave.FileName;
  end;
end;

procedure TdmRichEditorManager.actSaveExecute(Sender: TObject);
begin
  ActiveView.SaveToFile(ActiveView.FileName);
end;

procedure TdmRichEditorManager.actShowPreviewExecute(Sender: TObject);
begin
  ActiveView.ShowPreview;
end;

procedure TdmRichEditorManager.actShowSpecialCharactersExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    actShowSpecialCharacters.Checked := not actShowSpecialCharacters.Checked;
    ActiveView.ShowSpecialChars := actShowSpecialCharacters.Checked;
  end;
end;

procedure TdmRichEditorManager.actShowStructureViewerExecute(Sender: TObject);
begin
  ShowToolView('Structure', False, False);
end;

procedure TdmRichEditorManager.actStrikeThroughExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    actStrikeThrough.Checked := not actStrikeThrough.Checked;
    ActiveView.Font.StrikeThrough := actStrikeThrough.Checked;
  end;
end;

procedure TdmRichEditorManager.actBoldExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    actBold.Checked := not actBold.Checked;
    ActiveView.Font.Bold := actBold.Checked;
  end;
end;

procedure TdmRichEditorManager.actBulletListExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    actBulletList.Checked := not actBulletList.Checked;
    ActiveView.Bullets := actBulletList.Checked;
  end;
end;

procedure TdmRichEditorManager.actClearExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.Clear;
  end;
end;

procedure TdmRichEditorManager.actAlignRightExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.AlignRight := True;
  end;
end;

procedure TdmRichEditorManager.actDeleteColumnExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.DeleteColumn;
  end;
end;

procedure TdmRichEditorManager.actDeleteRowExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.DeleteRow;
  end;
end;

procedure TdmRichEditorManager.actEditParagraphStyleExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.EditParagraphStyle;
  end;
end;

procedure TdmRichEditorManager.actEditTextStyleExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.EditTextStyle;
  end;
end;

procedure TdmRichEditorManager.actInsertColumnAfterExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.InsertColumnAfter;
  end;
end;

procedure TdmRichEditorManager.actInsertColumnBeforeExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.InsertColumnBefore;
  end;
end;

procedure TdmRichEditorManager.actInsertRowAfterExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.InsertRowAfter;
  end;
end;

procedure TdmRichEditorManager.actInsertRowBeforeExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.InsertRowBefore;
  end;
end;

procedure TdmRichEditorManager.actInsertTableExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.CreateTable(2, 2);
  end;
end;

procedure TdmRichEditorManager.actSelectTableExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.SelectTable;
  end;
end;

procedure TdmRichEditorManager.actSetBackgroundColorExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    dlgColor.Width := 300;
    dlgColor.Handle := Application.MainForm.Handle;
    if dlgColor.Execute then
    begin
      ActiveView.BackgroundColor := dlgColor.Color;
    end;
  end;
end;

procedure TdmRichEditorManager.actAlignLeftExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.AlignLeft := True;
  end;
end;

procedure TdmRichEditorManager.actAlignCenterExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.AlignCenter := True;
  end;
end;

// Not supported yet by TKMemo

procedure TdmRichEditorManager.actAlignJustifyExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.AlignJustify := True;
  end;
end;

procedure TdmRichEditorManager.aclActionsExecute(AAction: TBasicAction;
  var Handled: Boolean);
begin
  Logger.Action(AAction);
end;

procedure TdmRichEditorManager.actAddParagraphExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.AddParagraph;
  end;
end;

procedure TdmRichEditorManager.actEditSelectedItemExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.EditSelectedItem;
  end;
end;

procedure TdmRichEditorManager.actSetFontColorExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    dlgColor.Width  := 300;
    dlgColor.Handle := Application.MainForm.Handle;
    if dlgColor.Execute then
    begin;
      ActiveView.Font.Color := dlgColor.Color;
    end;
  end;
end;

procedure TdmRichEditorManager.actCopyExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.Copy;
  end;
end;

procedure TdmRichEditorManager.actCutExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.Cut;
  end;
end;

procedure TdmRichEditorManager.actDecFontSizeExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    if ActiveView.Font.Size > 0 then
      ActiveView.Font.Size := ActiveView.Font.Size - 1;
  end;
end;

procedure TdmRichEditorManager.actDecIndentExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.DecIndent;
  end;
end;

procedure TdmRichEditorManager.actSetFontExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    if dlgFont.Execute then
    begin
      ActiveView.Font.Assign(dlgFont.Font);
    end;
  end;
end;

procedure TdmRichEditorManager.actIncFontSizeExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.Font.Size := ActiveView.Font.Size + 1;
  end;
end;

procedure TdmRichEditorManager.actIncIndentExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.IncIndent;
  end;
end;

procedure TdmRichEditorManager.actInsertBulletListExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.CreateBulletList;
  end;
end;

procedure TdmRichEditorManager.actInsertHyperLinkExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.InsertHyperlink;
  end;
end;

procedure TdmRichEditorManager.actInsertImageExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.InsertImage;
  end;
end;

procedure TdmRichEditorManager.actItalicExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    actItalic.Checked := not actItalic.Checked;
    ActiveView.Font.Italic := actItalic.Checked;
  end;
end;

procedure TdmRichEditorManager.actNumberedListExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    //
  end;
end;

procedure TdmRichEditorManager.actUnderlineExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    actUnderline.Checked := not actUnderline.Checked;
    ActiveView.Font.Underline := actUnderline.Checked;
  end;
end;

procedure TdmRichEditorManager.actUndoExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.Undo;
  end;
end;

function TdmRichEditorManager.GetActionList: TActionList;
begin
  Result := aclActions;
end;

function TdmRichEditorManager.GetActions: IRichEditorActions;
begin
  Result := Self as IRichEditorActions;
end;

function TdmRichEditorManager.GetToolViews: IRichEditorToolViews;
begin
  Result := FToolViews;
end;

function TdmRichEditorManager.GetInsertPopupMenu: TPopupMenu;
begin
  Result := ppmInsert;
end;

function TdmRichEditorManager.GetClipboardPopupMenu: TPopupMenu;
begin
  Result := ppmClipboard;
end;

function TdmRichEditorManager.GetFilePopupMenu: TPopupMenu;
begin
  Result := ppmFile;
end;

function TdmRichEditorManager.GetSelectionPopupMenu: TPopupMenu;
begin
  Result := ppmSelection;
end;

function TdmRichEditorManager.GetSelectPopupMenu: TPopupMenu;
begin
  Result := ppmSelect;
end;

function TdmRichEditorManager.GetSettingsPopupMenu: TPopupMenu;
begin
   Result := ppmSettings;
end;

function TdmRichEditorManager.GetTablePopupMenu: TPopupMenu;
begin
  Result := ppmTable;
end;

procedure TdmRichEditorManager.actToggleWordWrapExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    actToggleWordWrap.Checked := not actToggleWordWrap.Checked;
    ActiveView.WordWrap := actToggleWordWrap.Checked;
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TdmRichEditorManager.BuildRichEditorPopupMenu;
var
  MI : TMenuItem;
begin
  MI := ppmRichEditor.Items;
  MI.Clear;
  AddMenuItem(MI, actEditSelectedItem);
  AddMenuItem(MI, actEditParagraphStyle);
  AddMenuItem(MI, actEditTextStyle);
  AddMenuItem(MI);
  AddMenuItem(MI, actCut);
  AddMenuItem(MI, actCopy);
  AddMenuItem(MI, actPaste);
  AddMenuItem(MI);
  AddMenuItem(MI, TablePopupMenu);
  AddMenuItem(MI, SelectionPopupMenu);
  AddMenuItem(MI, FilePopupMenu);
  AddMenuItem(MI, InsertPopupMenu);
//  AddMenuItem(MI, SelectPopupMenu);
//  AddMenuItem(MI, ClipboardPopupMenu);
  AddMenuItem(MI, SettingsPopupMenu);
  //AddMenuItem(MI);
  //AddMenuItem(MI, actUndo); // not supported yet
  //AddMenuItem(MI, actRedo); // not supported yet
end;

procedure TdmRichEditorManager.BuildInsertPopupMenu;
var
  MI : TMenuItem;
begin
  MI := InsertPopupMenu.Items;
  MI.Clear;
  MI.Action := actInsertMenu;
  AddMenuItem(MI, actInsertBulletList);
  AddMenuItem(MI, actInsertTable);
  AddMenuItem(MI, actInsertImage);
  AddMenuItem(MI, actInsertHyperLink);
  AddMenuItem(MI, actAddParagraph);
end;

procedure TdmRichEditorManager.BuildTablePopupMenu;
var
  MI : TMenuItem;
begin
  MI := TablePopupMenu.Items;
  MI.Clear;
  MI.Action := actTableMenu;
  AddMenuItem(MI, actInsertColumnBefore);
  AddMenuItem(MI, actInsertColumnAfter);
  AddMenuItem(MI, actInsertRowBefore);
  AddMenuItem(MI, actInsertRowAfter);
  AddMenuItem(MI);
  AddMenuItem(MI, actDeleteRow);
  AddMenuItem(MI, actDeleteColumn);
  AddMenuItem(MI);
  AddMenuItem(MI, actSelectTable);
end;

procedure TdmRichEditorManager.BuildSelectionPopupMenu;
var
  MI : TMenuItem;
begin
  MI := SelectionPopupMenu.Items;
  MI.Clear;
  MI.Action := actSelectionMenu;
  AddMenuItem(MI, actBold);
  AddMenuItem(MI, actItalic);
  AddMenuItem(MI, actUnderline);
  AddMenuItem(MI, actStrikeThrough);
  AddMenuItem(MI);
  AddMenuItem(MI, actAlignLeft);
  AddMenuItem(MI, actAlignCenter);
  AddMenuItem(MI, actAlignRight);
  //AddMenuItem(MI, actAlignJustify); // not supported yet
  AddMenuItem(MI);
  AddMenuItem(MI, actIncIndent);
  AddMenuItem(MI, actDecIndent);
  AddMenuItem(MI);
  AddMenuItem(MI, actBulletList);
  AddMenuItem(MI, actNumberedList);
  AddMenuItem(MI);
  AddMenuItem(MI, actSetFontColor);
  AddMenuItem(MI, actSetBackgroundColor);
  AddMenuItem(MI, actSetFont);
  AddMenuItem(MI);
  AddMenuItem(MI, actToggleWordWrap);
end;

procedure TdmRichEditorManager.BuildSelectPopupMenu;
var
  MI : TMenuItem;
begin
  MI := SelectPopupMenu.Items;
  MI.Clear;
  MI.Action := actSelectMenu;
  AddMenuItem(MI, actClear);
end;

procedure TdmRichEditorManager.BuildFilePopupMenu;
var
  MI : TMenuItem;
begin
  MI := FilePopupMenu.Items;
  MI.Clear;
  MI.Action := actFileMenu;
  AddMenuItem(MI, actOpen);
  AddMenuItem(MI, actSave);
  AddMenuItem(MI, actSaveAs);
  AddMenuItem(MI);
  AddMenuItem(MI, actShowPreview);
end;

procedure TdmRichEditorManager.BuildClipboardPopupMenu;
var
  MI : TMenuItem;
begin
  MI := ClipboardPopupMenu.Items;
  MI.Clear;
  MI.Action := actClipboardMenu;
end;

procedure TdmRichEditorManager.BuildSettingsPopupMenu;
var
  MI : TMenuItem;
begin
  MI := SettingsPopupMenu.Items;
  MI.Clear;
  MI.Action := actSettingsMenu;
  AddMenuItem(MI, actShowSpecialCharacters);
end;

procedure TdmRichEditorManager.InitializePopupMenus;
begin
  BuildSelectionPopupMenu;
  BuildSelectPopupMenu;
  BuildFilePopupMenu;
  BuildInsertPopupMenu;
  BuildTablePopupMenu;
  BuildClipboardPopupMenu;
  BuildSettingsPopupMenu;
  BuildRichEditorPopupMenu;
end;

procedure TdmRichEditorManager.RegisterToolViews;
begin
  ToolViews.Register(TStructureToolView, nil, 'Structure');
end;

procedure TdmRichEditorManager.ShowToolView(const AName: string;
  AShowModal: Boolean; ASetFocus: Boolean);
var
  ETV : IRichEditorToolView;
  TV  : IRichEditorToolView;
begin
  ETV := ToolViews[AName];
  for TV in ToolViews do
  begin
    if TV <> ETV then
      TV.Visible := False;
  end;
  if not ETV.Visible then
  begin
    if not AShowModal then
    begin
      { This for example can allow the owner to dock the toolview in the main
        application workspace. }
      Events.DoShowToolView(ETV);
      ETV.Visible := True;
    end
    else
    begin
      ETV.Form.ShowModal;
    end;
  end;
  ETV.UpdateView;
  if ASetFocus then
    ETV.SetFocus;
end;
{$ENDREGION}

{$REGION 'protected methods'}
{ Gets called by the active view. }

procedure TdmRichEditorManager.UpdateActions;
var
  B : Boolean;
begin
  B := Assigned(ActiveView);
  actUndo.Enabled                  := B and ActiveView.CanUndo;
  actRedo.Enabled                  := B and ActiveView.CanRedo;
  actCut.Enabled                   := B and ActiveView.SelAvail;
  actCopy.Enabled                  := B and ActiveView.SelAvail;
  actPaste.Enabled                 := B and ActiveView.CanPaste;
  //actAlignJustify.Enabled          := B;
  actAddParagraph.Enabled          := B;
  actAlignCenter.Enabled           := B;
  actAlignLeft.Enabled             := B;
  actAlignRight.Enabled            := B;
  actClear.Enabled                 := B;
  actDecFontSize.Enabled           := B;
  actDecIndent.Enabled             := B;
  actEditParagraphStyle.Enabled    := B;
  actEditSelectedItem.Enabled      := B;
  actEditTextStyle.Enabled         := B;
  actIncFontSize.Enabled           := B;
  actIncIndent.Enabled             := B;
  actInsertBulletList.Enabled      := B;
  actInsertHyperLink.Enabled       := B;
  actInsertImage.Enabled           := B;
  actItalic.Enabled                := B;
  actOpen.Enabled                  := B;
  actSave.Enabled                  := B;
  actSaveAs.Enabled                := B;
  actSelectAll.Enabled             := B;
  actSetBackgroundColor.Enabled    := B;
  actSetFont.Enabled               := B;
  actSetFontColor.Enabled          := B;
  actShowSpecialCharacters.Enabled := B;
  actStrikeThrough.Enabled         := B;
  actToggleWordWrap.Enabled        := B;
  actUnderline.Enabled             := B;

  actBold.Checked           := B and ActiveView.Font.Bold;
  actUnderline.Checked      := B and ActiveView.Font.Underline;
  actItalic.Checked         := B and ActiveView.Font.Italic;
  actStrikeThrough.Checked  := B and ActiveView.Font.StrikeThrough;
  actAlignCenter.Checked    := B and ActiveView.AlignCenter;
  actAlignLeft.Checked      := B and ActiveView.AlignLeft;
  actAlignRight.Checked     := B and ActiveView.AlignRight;
  //actAlignJustify.Checked   := B and ActiveView.AlignJustify;
  actToggleWordWrap.Checked := B and ActiveView.WordWrap;

  actBulletList.Checked := B and ActiveView.Bullets;

  B := B and ActiveView.IsInsideOfTable;
  actInsertColumnAfter.Visible  := B;
  actInsertColumnBefore.Visible := B;
  actInsertRowAfter.Visible     := B;
  actInsertRowBefore.Visible    := B;
  actDeleteColumn.Visible       := B;
  actDeleteRow.Visible          := B;
  actSelectTable.Visible        := B;
  actTableMenu.Visible          := B;
end;

function TdmRichEditorManager.AddView(const AName: string;
 const AFileName: string): IRichEditorView;
var
  V : TRichEditorViewKMemo;
begin
  V := TRichEditorViewKMemo.Create(Self);
  // if no name is provided, the view will get an automatically generated one.
  if AName <> '' then
    V.Name := AName;
  V.FileName := AFileName;
  V.Caption := '';
  FViews.Add(V);
  Result := V as IRichEditorView;
  FActiveView := V;
end;

function TdmRichEditorManager.DeleteView(AIndex: Integer): Boolean;
begin
  { TODO -oTS : Needs implementation }
  Result := False;
end;

procedure TdmRichEditorManager.ClearViews;
begin
  FViews.Clear;
end;
{$ENDREGION}

end.
