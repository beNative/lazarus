{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.RichEditor.Manager;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, FileUtil, ActnList, Dialogs, Menus, Contnrs, Forms,
  Controls,

  ts.RichEditor.Interfaces;

type
  TRichEditorViewList = TComponentList;

  { TdmRichEditorManager }

  TdmRichEditorManager = class(TDataModule, IRichEditorManager,
                                            IRichEditorActions,
                                            IRichEditorEvents

  )
    {$REGION 'designer controls'}
    aclActions             : TActionList;
    actBold                : TAction;
    actColor               : TAction;
    actAlignRight          : TAction;
    actAlignLeft           : TAction;
    actAlignCenter         : TAction;
    actFont                : TAction;
    actIncFontSize         : TAction;
    actDecFontSize         : TAction;
    actCut                 : TAction;
    actCopy                : TAction;
    actAlignJustify        : TAction;
    actBkColor             : TAction;
    actInsertImage         : TAction;
    actInsertHyperLink     : TAction;
    actStrikeThrough       : TAction;
    actRedo                : TAction;
    actToggleWordWrap      : TAction;
    actUndo                : TAction;
    actSelectAll           : TAction;
    actPaste               : TAction;
    actSaveAs              : TAction;
    actSave                : TAction;
    actOpen                : TAction;
    actUnderline           : TAction;
    actItalic              : TAction;
    dlgColor               : TColorDialog;
    dlgFont                : TFontDialog;
    dlgOpen                : TOpenDialog;
    dlgSave                : TSaveDialog;
    imlMain                : TImageList;
    MenuItem1              : TMenuItem;
    MenuItem2              : TMenuItem;
    MenuItem3              : TMenuItem;
    MenuItem4              : TMenuItem;
    MenuItem5              : TMenuItem;
    N1                     : TMenuItem;
    mniBold                : TMenuItem;
    mniItalic              : TMenuItem;
    mniUnderline           : TMenuItem;
    mniOpen                : TMenuItem;
    mniSave                : TMenuItem;
    mniSaveAs              : TMenuItem;
    ppmRichEditor          : TPopupMenu;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actAlignCenterExecute(Sender: TObject);
    procedure actAlignJustifyExecute(Sender: TObject);
    procedure actAlignLeftExecute(Sender: TObject);
    procedure actAlignRightExecute(Sender: TObject);
    procedure actBkColorExecute(Sender: TObject);
    procedure actBoldExecute(Sender: TObject);
    procedure actColorExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actDecFontSizeExecute(Sender: TObject);
    procedure actFontExecute(Sender: TObject);
    procedure actIncFontSizeExecute(Sender: TObject);
    procedure actInsertHyperLinkExecute(Sender: TObject);
    procedure actInsertImageExecute(Sender: TObject);
    procedure actItalicExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actStrikeThroughExecute(Sender: TObject);
    procedure actUnderlineExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actToggleWordWrapExecute(Sender: TObject);
    {$ENDREGION}

  private
    FViews      : TRichEditorViewList;
    FActiveView : IRichEditorView;
    FEvents     : IRichEditorEvents;
    function GetEvents: IRichEditorEvents;

  protected
    {$REGION 'property access mehods'}
    function GetActions: TActionList;
    function GetActiveView: IRichEditorView;
    function GetEditorPopupMenu: TPopupMenu;
    function GetItem(AName: string): TContainedAction;
    function GetView(AIndex: Integer): IRichEditorView;
    function GetViewByName(AName: string): IRichEditorView;
    function GetViewCount: Integer;
    procedure SetActiveView(const AValue: IRichEditorView);
    {$ENDREGION}

    procedure UpdateActions;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function AddView(
      const AName     : string = '';
      const AFileName : string = ''
    ): IRichEditorView;
    function DeleteView(AIndex: Integer): Boolean;
    procedure ClearViews;

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

  ts.RichEditor.Events, ts.RichEditor.View.KMemo;

{$REGION 'construction and destruction'}
procedure TdmRichEditorManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FViews  := TRichEditorViewList.Create(False);
  FEvents := TRichEditorEvents.Create(Self);
  FActiveView := nil;
end;

procedure TdmRichEditorManager.BeforeDestruction;
begin
  FActiveView := nil;
  FEvents     := nil;
  FreeAndNil(FViews);
  inherited BeforeDestruction;
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
    //ActiveViewUpdated;
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
  I: Integer;
  B: Boolean;
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
// File

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

procedure TdmRichEditorManager.actStrikeThroughExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.Font.StrikeThrough := actStrikeThrough.Checked;
  end;
end;

// Style

procedure TdmRichEditorManager.actBoldExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.Font.Bold := actBold.Checked;
  end;
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

procedure TdmRichEditorManager.actAlignJustifyExecute(Sender: TObject);
begin
  ActiveView.AlignJustify := True;
end;

procedure TdmRichEditorManager.actColorExecute(Sender: TObject);
begin
  dlgColor.Width := 300;
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

procedure TdmRichEditorManager.actInsertHyperLinkExecute(Sender: TObject);
begin
  ActiveView.InsertHyperlink;
end;

procedure TdmRichEditorManager.actInsertImageExecute(Sender: TObject);
begin
  ActiveView.InsertImage;
end;

procedure TdmRichEditorManager.actItalicExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.Font.Italic := actItalic.Checked;
  end;
end;

procedure TdmRichEditorManager.actUnderlineExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.Font.Underline := actUnderline.Checked;
  end;
end;

procedure TdmRichEditorManager.actUndoExecute(Sender: TObject);
begin
  ActiveView.Undo;
end;

procedure TdmRichEditorManager.actToggleWordWrapExecute(Sender: TObject);
begin
  ActiveView.WordWrap := actToggleWordWrap.Checked;
end;
{$ENDREGION}

{$REGION 'protected methods'}
{ Gets called by the active view. }

procedure TdmRichEditorManager.UpdateActions;
begin
  if Assigned(ActiveView) then
  begin
    actBold.Checked          := ActiveView.Font.Bold;
    actUnderline.Checked     := ActiveView.Font.Underline;
    actItalic.Checked        := ActiveView.Font.Italic;
    actStrikeThrough.Checked := ActiveView.Font.StrikeThrough;
    actUndo.Enabled          := ActiveView.CanUndo;
    actRedo.Enabled          := ActiveView.CanRedo;
    actUndo.Enabled          := True;
    actRedo.Enabled          := True;
    actCopy.Enabled          := ActiveView.SelAvail;
    actCut.Enabled           := ActiveView.SelAvail;
    actPaste.Enabled         := ActiveView.CanPaste;
    actAlignCenter.Checked   := ActiveView.AlignCenter;
    actAlignLeft.Checked     := ActiveView.AlignLeft;
    actAlignRight.Checked    := ActiveView.AlignRight;
    actAlignJustify.Checked  := ActiveView.AlignJustify;
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

