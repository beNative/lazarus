{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

  { TdmRichEditorManager }

  TdmRichEditorManager = class(
    TDataModule, IRichEditorManager, IRichEditorActions, IRichEditorEvents
  )
    {$REGION 'designer controls'}
    aclActions               : TActionList;
    actAlignCenter           : TAction;
    actAlignJustify          : TAction;
    actAlignLeft             : TAction;
    actAlignRight            : TAction;
    actBkColor               : TAction;
    actBold                  : TAction;
    actColor                 : TAction;
    actCopy                  : TAction;
    actCut                   : TAction;
    actDecFontSize           : TAction;
    actFont                  : TAction;
    actIncFontSize           : TAction;
    actInsertHyperLink       : TAction;
    actInsertImage           : TAction;
    actInsertBulletList      : TAction;
    actIncIndent             : TAction;
    actDecIndent             : TAction;
    actAdjustParagraphStyle  : TAction;
    actInsertTextBox         : TAction;
    actClear                 : TAction;
    actShowSpecialCharacters : TAction;
    actItalic                : TAction;
    actOpen                  : TAction;
    actPaste                 : TAction;
    actRedo                  : TAction;
    actSave                  : TAction;
    actSaveAs                : TAction;
    actSelectAll             : TAction;
    actStrikeThrough         : TAction;
    actToggleWordWrap        : TAction;
    actUnderline             : TAction;
    actUndo                  : TAction;
    dlgColor                 : TColorDialog;
    dlgFont                  : TFontDialog;
    dlgOpen                  : TOpenDialog;
    dlgSave                  : TSaveDialog;
    imlMain                  : TImageList;
    ppmRichEditor            : TPopupMenu;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure aclActionsExecute(AAction: TBasicAction; var Handled: Boolean);
    procedure actAlignCenterExecute(Sender: TObject);
    procedure actAlignJustifyExecute(Sender: TObject);
    procedure actAlignLeftExecute(Sender: TObject);
    procedure actAlignRightExecute(Sender: TObject);
    procedure actBkColorExecute(Sender: TObject);
    procedure actBoldExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actColorExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actDecFontSizeExecute(Sender: TObject);
    procedure actDecIndentExecute(Sender: TObject);
    procedure actFontExecute(Sender: TObject);
    procedure actIncFontSizeExecute(Sender: TObject);
    procedure actIncIndentExecute(Sender: TObject);
    procedure actInsertBulletListExecute(Sender: TObject);
    procedure actInsertHyperLinkExecute(Sender: TObject);
    procedure actInsertImageExecute(Sender: TObject);
    procedure actInsertTextBoxExecute(Sender: TObject);
    procedure actItalicExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actAdjustParagraphStyleExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actShowSpecialCharactersExecute(Sender: TObject);
    procedure actStrikeThroughExecute(Sender: TObject);
    procedure actUnderlineExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actToggleWordWrapExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    {$ENDREGION}

  private
    FViews      : TRichEditorViewList;
    FActiveView : IRichEditorView;
    FEvents     : IRichEditorEvents;

  protected
    {$REGION 'property access mehods'}
    function GetActions: TActionList;
    function GetActiveView: IRichEditorView;
    function GetEditorPopupMenu: TPopupMenu;
    function GetEvents: IRichEditorEvents;
    function GetItem(AName: string): TContainedAction;
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

    procedure BuildRichEditorPopupMenu;

    { Delegates the implementation of IEditorEvents to an internal object. }
    property Events: IRichEditorEvents
      read GetEvents implements IRichEditorEvents;

    property Actions: TActionList
      read GetActions;

    property ActiveView: IRichEditorView
      read GetActiveView write SetActiveView;

    property Items[AName: string]: TContainedAction
      read GetItem; default;

    property Views[AIndex: Integer]: IRichEditorView
      read GetView;

    property ViewByName[AName: string]: IRichEditorView
      read GetViewByName;

    property ViewCount: Integer
      read GetViewCount;

    property EditorPopupMenu: TPopupMenu
      read GetEditorPopupMenu;

  end;

implementation

{$R *.lfm}

uses
  Graphics,

  ts.Core.Utils, ts.Core.Logger,

  ts.RichEditor.Events, ts.RichEditor.View.KMemo;

{$REGION 'construction and destruction'}
procedure TdmRichEditorManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FViews  := TRichEditorViewList.Create(False);
  FEvents := TRichEditorEvents.Create(Self);
  FActiveView := nil;
  BuildRichEditorPopupMenu;
end;

destructor TdmRichEditorManager.Destroy;
begin
  FActiveView := nil;
  FEvents     := nil;
  FreeAndNil(FViews);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TdmRichEditorManager.GetActions: TActionList;
begin
  Result := aclActions;
end;

function TdmRichEditorManager.GetActiveView: IRichEditorView;
begin
  if not Assigned(FActiveView) then
    raise Exception.Create('No active view!');
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

procedure TdmRichEditorManager.actAdjustParagraphStyleExecute(Sender: TObject);
begin
  ActiveView.AdjustParagraphStyle;
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

procedure TdmRichEditorManager.actShowSpecialCharactersExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    actShowSpecialCharacters.Checked := not actShowSpecialCharacters.Checked;
    ActiveView.ShowSpecialChars := actShowSpecialCharacters.Checked;
  end;
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

procedure TdmRichEditorManager.actClearExecute(Sender: TObject);
begin
  ActiveView.Clear;
end;

procedure TdmRichEditorManager.actAlignRightExecute(Sender: TObject);
begin
  ActiveView.AlignRight := True;
end;

procedure TdmRichEditorManager.actBkColorExecute(Sender: TObject);
begin
  dlgColor.Width := 300;
  dlgColor.Handle := Application.MainForm.Handle;
  if dlgColor.Execute then
  begin;
    //ActiveView.TextAttributes.HasBkColor := False;
    //ActiveView.TextAttributes.HasBkColor := True;
    //ActiveView.TextAttributes.BkColor := dlgColor.Color;
  end;
end;

procedure TdmRichEditorManager.actAlignLeftExecute(Sender: TObject);
begin
  ActiveView.AlignLeft := True;
end;

procedure TdmRichEditorManager.actAlignCenterExecute(Sender: TObject);
begin
  ActiveView.AlignCenter := True;
end;

procedure TdmRichEditorManager.aclActionsExecute(AAction: TBasicAction;
  var Handled: Boolean);
begin
  Logger.Action(AAction);
end;

procedure TdmRichEditorManager.actAlignJustifyExecute(Sender: TObject);
begin
  ActiveView.AlignJustify := True;
end;

procedure TdmRichEditorManager.actColorExecute(Sender: TObject);
begin
  dlgColor.Width  := 300;
  dlgColor.Handle := Application.MainForm.Handle;
  if dlgColor.Execute then
  begin;
    ActiveView.Font.Color := dlgColor.Color;
  end;
end;

procedure TdmRichEditorManager.actCopyExecute(Sender: TObject);
begin
  ActiveView.Copy;
end;

procedure TdmRichEditorManager.actCutExecute(Sender: TObject);
begin
  ActiveView.Cut;
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
  ActiveView.DecIndent;
end;

procedure TdmRichEditorManager.actFontExecute(Sender: TObject);
begin
  if dlgFont.Execute then
  begin
    ActiveView.Font.Assign(dlgFont.Font);
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
  ActiveView.IncIndent;
end;

procedure TdmRichEditorManager.actInsertBulletListExecute(Sender: TObject);
begin
  ActiveView.InsertBulletList;
end;

procedure TdmRichEditorManager.actInsertHyperLinkExecute(Sender: TObject);
begin
  ActiveView.InsertHyperlink;
end;

procedure TdmRichEditorManager.actInsertImageExecute(Sender: TObject);
begin
  ActiveView.InsertImage;
end;

procedure TdmRichEditorManager.actInsertTextBoxExecute(Sender: TObject);
begin
  ActiveView.InsertTextBox;
end;

procedure TdmRichEditorManager.actItalicExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    actItalic.Checked := not actItalic.Checked;
    ActiveView.Font.Italic := actItalic.Checked;
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
  ActiveView.Undo;
end;

procedure TdmRichEditorManager.actToggleWordWrapExecute(Sender: TObject);
begin
  actToggleWordWrap.Checked := not actToggleWordWrap.Checked;
  ActiveView.WordWrap := actToggleWordWrap.Checked;
end;

procedure TdmRichEditorManager.DataModuleCreate(Sender: TObject);
begin

end;

{$ENDREGION}

{$REGION 'private methods'}
procedure TdmRichEditorManager.BuildRichEditorPopupMenu;
var
  MI : TMenuItem;
begin
  MI := ppmRichEditor.Items;
  MI.Clear;
  AddMenuItem(MI, actCut);
  AddMenuItem(MI, actCopy);
  AddMenuItem(MI, actPaste);
  AddMenuItem(MI);
  AddMenuItem(MI, actUndo);
  AddMenuItem(MI, actRedo);
  AddMenuItem(MI);
  AddMenuItem(MI, actBold);
  AddMenuItem(MI, actItalic);
  AddMenuItem(MI, actUnderline);
  AddMenuItem(MI);
  AddMenuItem(MI, actAlignLeft);
  AddMenuItem(MI, actAlignCenter);
  AddMenuItem(MI, actAlignRight);
  AddMenuItem(MI, actAlignJustify);
  AddMenuItem(MI);
  AddMenuItem(MI, actIncIndent);
  AddMenuItem(MI, actDecIndent);
  AddMenuItem(MI);
  AddMenuItem(MI, actToggleWordWrap);
  AddMenuItem(MI, actShowSpecialCharacters);
  AddMenuItem(MI);
  AddMenuItem(MI, actClear);
  AddMenuItem(MI);
  AddMenuItem(MI, actOpen);
  AddMenuItem(MI, actSave);
  AddMenuItem(MI, actSaveAs);

  //AddMenuItem(MI, FilePopupMenu);
  //AddMenuItem(MI, SettingsPopupMenu);
  //AddMenuItem(MI, SearchPopupMenu);
  //AddMenuItem(MI, SelectPopupMenu);
  //AddMenuItem(MI, SelectionPopupMenu);
  //AddMenuItem(MI, InsertPopupMenu);
  //AddMenuItem(MI, ClipboardPopupMenu);
  //AddMenuItem(MI, ExportPopupMenu);
  //AddMenuItem(MI, HighlighterPopupMenu);
  //AddMenuItem(MI, FoldPopupMenu);
  //AddMenuItem(MI);
  //AddMenuItem(MI, actClose);
  //AddMenuItem(MI, actCloseOthers);
end;
{$ENDREGION}

{$REGION 'protected methods'}
{ Gets called by the active view. }

procedure TdmRichEditorManager.UpdateActions;
begin
  if Assigned(ActiveView) then
  begin
    actBold.Checked           := ActiveView.Font.Bold;
    actUnderline.Checked      := ActiveView.Font.Underline;
    actItalic.Checked         := ActiveView.Font.Italic;
    actStrikeThrough.Checked  := ActiveView.Font.StrikeThrough;
    //actUndo.Enabled           := ActiveView.CanUndo;
    //actRedo.Enabled           := ActiveView.CanRedo;
    actUndo.Enabled           := True;
    actRedo.Enabled           := True;
    actCopy.Enabled           := ActiveView.SelAvail;
    actCut.Enabled            := ActiveView.SelAvail;
    actPaste.Enabled          := ActiveView.CanPaste;
    actAlignCenter.Checked    := ActiveView.AlignCenter;
    actAlignLeft.Checked      := ActiveView.AlignLeft;
    actAlignRight.Checked     := ActiveView.AlignRight;
    actAlignJustify.Checked   := ActiveView.AlignJustify;
    actToggleWordWrap.Checked := ActiveView.WordWrap;
  end;
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
