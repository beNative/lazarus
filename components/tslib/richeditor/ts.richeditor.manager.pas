{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

  { TdmRichEditorActions }

  TdmRichEditorActions = class(TDataModule, IRichEditorActions)
    {$REGION 'designer controls'}
    aclActions    : TActionList;
    actBold       : TAction;
    actColor      : TAction;
    actAlignRight : TAction;
    actAlignLeft  : TAction;
    actAlignCenter: TAction;
    actFont       : TAction;
    actIncFontSize: TAction;
    actDecFontSize: TAction;
    actCut        : TAction;
    actCopy       : TAction;
    actAlignJustify: TAction;
    actBkColor: TAction;
    actWordWrap   : TAction;
    actUndo       : TAction;
    actSelectAll  : TAction;
    actPaste      : TAction;
    actSaveAs     : TAction;
    actSave       : TAction;
    actOpen       : TAction;
    actUnderline  : TAction;
    actItalic     : TAction;
    dlgColor      : TColorDialog;
    dlgFont       : TFontDialog;
    dlgOpen       : TOpenDialog;
    dlgSave       : TSaveDialog;
    imlMain       : TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    N1            : TMenuItem;
    mniBold       : TMenuItem;
    mniItalic     : TMenuItem;
    mniUnderline  : TMenuItem;
    mniOpen       : TMenuItem;
    mniSave       : TMenuItem;
    mniSaveAs     : TMenuItem;
    ppmRichEditor : TPopupMenu;
    {$ENDREGION}

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
    procedure actItalicExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actUnderlineExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actWordWrapExecute(Sender: TObject);

  private
    FViews      : TRichEditorViewList;
    FActiveView : IRichEditorView;

    function GetActions: TActionList;
    function GetActiveView: IRichEditorView;
    function GetEditorPopupMenu: TPopupMenu;
    function GetItem(AName: string): TContainedAction;
    function GetView(AIndex: Integer): IRichEditorView;
    function GetViewByName(AName: string): IRichEditorView;
    function GetViewCount: Integer;
    procedure SetActiveView(const AValue: IRichEditorView);

    procedure UpdateActions;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure OpenFileAtCursor;

    function SaveFile: Boolean;
    procedure LoadFile;

    function AddView(const AName: string = ''; const AFileName: string = '')
      : IRichEditorView;
    function DeleteView(AIndex: Integer): Boolean;
    procedure ClearViews;

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

function RichEditorActions : IRichEditorActions;

implementation

{$R *.lfm}

uses
  Graphics,

  RichMemo,

  ts.RichEditor.View;

var
  dmRichEditorActions : TdmRichEditorActions;

function RichEditorActions: IRichEditorActions;
begin
  Result := dmRichEditorActions;
end;

{$REGION 'construction and destruction'}
procedure TdmRichEditorActions.AfterConstruction;
begin
  inherited AfterConstruction;
  FViews := TRichEditorViewList.Create(False);
  FActiveView := nil;
end;

procedure TdmRichEditorActions.BeforeDestruction;
begin
  FActiveView := nil;
  FreeAndNil(FViews);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TdmRichEditorActions.GetActions: TActionList;
begin
  Result := aclActions;
end;

function TdmRichEditorActions.GetActiveView: IRichEditorView;
begin
  if not Assigned(FActiveView) then
    raise Exception.Create('No active view!');
  Result := FActiveView;
end;

procedure TdmRichEditorActions.SetActiveView(const AValue: IRichEditorView);
begin
  if AValue <> FActiveView then
  begin
    FActiveView := AValue;
    //ActiveViewUpdated;
  end;
end;

function TdmRichEditorActions.GetEditorPopupMenu: TPopupMenu;
begin
  Result := ppmRichEditor;
end;

function TdmRichEditorActions.GetItem(AName: string): TContainedAction;
begin
  Result := aclActions.ActionByName(AName);
end;

function TdmRichEditorActions.GetView(AIndex: Integer): IRichEditorView;
begin
  if (AIndex > -1) and (AIndex < FViews.Count) then
  begin
    Result := TRichEditorView(FViews[AIndex]) as IRichEditorView;
  end
  else
    Result := nil;
end;

function TdmRichEditorActions.GetViewByName(AName: string): IRichEditorView;
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

function TdmRichEditorActions.GetViewCount: Integer;
begin
  Result := FViews.Count;
end;
{$ENDREGION}

{$REGION 'action handlers'}
// File

procedure TdmRichEditorActions.actOpenExecute(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    ActiveView.LoadFromFile(dlgOpen.FileName);
    ActiveView.FileName := dlgOpen.FileName;
  end;
end;

procedure TdmRichEditorActions.actPasteExecute(Sender: TObject);
begin
  ActiveView.Paste;
end;

procedure TdmRichEditorActions.actSaveAsExecute(Sender: TObject);
begin
  if dlgSave.Execute then
  begin
    ActiveView.SaveToFile(dlgSave.FileName);
    ActiveView.FileName := dlgSave.FileName;
  end;
end;

procedure TdmRichEditorActions.actSaveExecute(Sender: TObject);
begin
  ActiveView.SaveToFile(ActiveView.FileName);
end;

// Style

procedure TdmRichEditorActions.actBoldExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.TextAttributes.BeginUpdate;
    ActiveView.TextAttributes.Bold := actBold.Checked;
    ActiveView.TextAttributes.EndUpdate;
  end;
end;

procedure TdmRichEditorActions.actAlignRightExecute(Sender: TObject);
begin
  ActiveView.TextAttributes.Alignment := paRight;
end;

procedure TdmRichEditorActions.actBkColorExecute(Sender: TObject);
begin
  dlgColor.Width := 300;
    dlgColor.Handle := Application.MainForm.Handle;
    if dlgColor.Execute then
    begin;
      ActiveView.TextAttributes.HasBkColor := False;
      ActiveView.TextAttributes.HasBkColor := True;
      ActiveView.TextAttributes.BkColor := dlgColor.Color;
    end;
end;

procedure TdmRichEditorActions.actAlignLeftExecute(Sender: TObject);
begin
  ActiveView.TextAttributes.Alignment := paLeft;
end;

procedure TdmRichEditorActions.actAlignCenterExecute(Sender: TObject);
begin
  ActiveView.TextAttributes.Alignment := paCenter;
end;

procedure TdmRichEditorActions.actAlignJustifyExecute(Sender: TObject);
begin
  ActiveView.TextAttributes.Alignment := paJustify;
end;

procedure TdmRichEditorActions.actColorExecute(Sender: TObject);
begin
  dlgColor.Width := 300;
  dlgColor.Handle := Application.MainForm.Handle;
  if dlgColor.Execute then
  begin;
    ActiveView.TextAttributes.Color := dlgColor.Color;
  end;
end;

procedure TdmRichEditorActions.actCopyExecute(Sender: TObject);
begin
  ActiveView.Copy;
end;

procedure TdmRichEditorActions.actCutExecute(Sender: TObject);
begin
  ActiveView.Cut;
end;

procedure TdmRichEditorActions.actDecFontSizeExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    if ActiveView.TextAttributes.Size > 0 then
      ActiveView.TextAttributes.Size := ActiveView.TextAttributes.Size - 1;
  end;
end;

procedure TdmRichEditorActions.actFontExecute(Sender: TObject);
begin
  if dlgFont.Execute then
  begin
    ActiveView.TextAttributes.BeginUpdate;
    ActiveView.TextAttributes.FontName := dlgFont.Font.Name;
    ActiveView.TextAttributes.Size     := dlgFont.Font.Size;
    ActiveView.TextAttributes.EndUpdate;
  end;
end;

procedure TdmRichEditorActions.actIncFontSizeExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.TextAttributes.Size := ActiveView.TextAttributes.Size + 1;
  end;
end;

procedure TdmRichEditorActions.actItalicExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.TextAttributes.Italic := actItalic.Checked;
  end;
end;

procedure TdmRichEditorActions.actUnderlineExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.TextAttributes.Underline := actUnderline.Checked;
  end;
end;

procedure TdmRichEditorActions.actUndoExecute(Sender: TObject);
begin
  ActiveView.Editor.Undo;
end;

procedure TdmRichEditorActions.actWordWrapExecute(Sender: TObject);
begin
  ActiveView.WordWrap := actWordWrap.Checked;
end;
{$ENDREGION}

procedure TdmRichEditorActions.UpdateActions;
begin
  if Assigned(ActiveView) then
  begin
    actBold.Checked      := ActiveView.TextAttributes.Bold;
    actUnderline.Checked := ActiveView.TextAttributes.Underline;
    actItalic.Checked    := ActiveView.TextAttributes.Italic;
    actUndo.Enabled      := ActiveView.CanUndo;
    actCopy.Enabled      := ActiveView.SelAvail;
    actCut.Enabled       := ActiveView.SelAvail;
    actPaste.Enabled     := ActiveView.CanPaste;
    case ActiveView.TextAttributes.Alignment of
      paLeft    : actAlignLeft.Checked    := True;
      paCenter  : actAlignCenter.Checked  := True;
      paRight   : actAlignRight.Checked   := True;
      paJustify : actAlignJustify.Checked := True;
    end;
  end;
end;

procedure TdmRichEditorActions.OpenFileAtCursor;
begin
//
end;

function TdmRichEditorActions.SaveFile: Boolean;
begin
  Result := False;
end;

procedure TdmRichEditorActions.LoadFile;
begin
//
end;

function TdmRichEditorActions.AddView(const AName: string; const AFileName: string): IRichEditorView;
var
  V : TRichEditorView;
begin
  V := TRichEditorView.Create(Self);
  // if no name is provided, the view will get an automatically generated one.
  if AName <> '' then
    V.Name := AName;
  V.FileName := AFileName;
  V.Caption := '';
  FViews.Add(V);
  Result := V as IRichEditorView;
  FActiveView := V;
end;

function TdmRichEditorActions.DeleteView(AIndex: Integer): Boolean;
begin
  { TODO -oTS : Needs implementation }
  Result := False;
end;

procedure TdmRichEditorActions.ClearViews;
begin
  FViews.Clear;
end;

initialization
  dmRichEditorActions := TdmRichEditorActions.Create(Application);

end.

