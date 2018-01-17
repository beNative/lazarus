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

unit ts.RichEditor.Helpers;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Menus,

  ts.RichEditor.Interfaces;

procedure AddActionButton(AParent: TToolBar; AAction: TBasicAction = nil);

procedure AddActionMenuItem(AParent: TMenuItem; AAction: TBasicAction = nil);

procedure AddEditorMenuItem(AParent: TMenuItem; const AActionName: string = '');

procedure AddEditorButton(AParent: TToolBar; const AActionName: string = '');

function CreateRichEditorView(AParent: TWinControl; const AName: string): IRichEditorView;

procedure BuildStandardRichEditorToolbar(AToolBar: TToolbar);

procedure BuildStandardRichEditorMainMenu(AMainMenu : TMainMenu);

implementation

uses
  ActnList,

  ts.RichEditor.Manager;

function CreateRichEditorView(AParent: TWinControl; const AName: string): IRichEditorView;
var
  V: IRichEditorView;
begin
  V := RichEditorActions.AddView(AName);
  V.BeginUpdate;
  V.Form.BorderStyle := bsNone;
  V.Form.Align := alClient;
  V.Form.Parent := AParent;
  //V.PopupMenu := RichEditorActions.EditorPopupMenu;
  V.Form.Visible := True;
  V.EndUpdate;
  Result := V;
end;

procedure AddActionButton(AParent: TToolBar; AAction: TBasicAction);
var
  TB: TToolButton;
begin
  TB := TToolButton.Create(AParent.Owner);
  TB.Parent := AParent;
  if not Assigned(AAction) then
    TB.Style := tbsDivider
  else
    TB.Action := AAction;
end;

procedure AddDropDownButton(AParent: TToolBar; AAction: TBasicAction;
  APopupMenu: TPopupMenu);
var
  TB: TToolButton;
begin
  TB := TToolButton.Create(AParent.Owner);
  TB.Parent := AParent;
  TB.Style := tbsDropDown;
  TB.DropdownMenu := APopupMenu;
  TB.Action := AAction;
end;

procedure AddActionMenuItem(AParent: TMenuItem; AAction: TBasicAction);
var
  MI: TMenuItem;
begin
  if not Assigned(AAction) then
    AParent.AddSeparator
  else
  begin
    MI := TMenuItem.Create(AParent.Owner);
    MI.Action := AAction;
    if (AAction is TAction) and (TAction(AAction).GroupIndex > 0) then
      MI.RadioItem := True;
    AParent.Add(MI);
  end;
end;

procedure AddEditorMenuItem(AParent: TMenuItem; const AActionName: string);
begin
  if AActionName = '' then
    AddActionMenuItem(AParent, nil)
  else
    AddActionMenuItem(AParent, RichEditorActions[AActionName]);
end;

procedure AddEditorButton(AParent: TToolBar; const AActionName: string);
begin
  if AActionName = '' then
    AddActionButton(AParent)
  else
    AddActionButton(AParent, RichEditorActions[AActionName]);
end;

procedure AddEditorDropDownButton(AParent: TToolBar; const AActionName: string;
  APopupMenu: TPopupMenu);
begin
  AddDropDownButton(AParent, RichEditorActions[AActionName], APopupMenu);
end;

procedure BuildStandardRichEditorToolbar(AToolBar: TToolbar);
  procedure AddButton(const AActionName: string);
  begin
    AddEditorButton(AToolBar, AActionName);
  end;

  procedure AddDropDownButton(const AActionName: string; APopupMenu: TPopupMenu);
  begin
    AddEditorDropDownButton(AToolBar, AActionName, APopupMenu);
  end;

begin
  AToolBar.DoubleBuffered := True;
  AToolBar.Images := RichEditorActions.Actions.Images;
  AddButton('actBold');
  AddButton('actItalic');
  AddButton('actUnderline');
  AddButton('');
  AddButton('actCut');
  AddButton('actCopy');
  AddButton('actPaste');
  AddButton('');
  AddButton('actUndo');
  AddButton('actRedo');
  AddButton('');
  AddButton('actAlignLeft');
  AddButton('actAlignCenter');
  AddButton('actAlignRight');
  AddButton('actAlignJustify');
  AddButton('');
  AddButton('actFont');
  AddButton('actColor');
  AddButton('actBkColor');
  AddButton('');
  AddButton('actIncFontSize');
  AddButton('actDecFontSize');
end;

procedure BuildStandardRichEditorMainMenu(AMainMenu: TMainMenu);
var
  MI : TMenuItem;
begin
  AMainMenu.Images := RichEditorActions.Actions.Images;
  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := '&File';
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actNew');
  AddEditorMenuItem(MI, 'actOpen');
  AddEditorMenuItem(MI, 'actSave');
  AddEditorMenuItem(MI, 'actSaveAs');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actReload');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actClose');
  AddEditorMenuItem(MI, 'actCloseOthers');

  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := '&Edit';
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actUndo');
  AddEditorMenuItem(MI, 'actRedo');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actCut');
  AddEditorMenuItem(MI, 'actCopy');
  AddEditorMenuItem(MI, 'actPaste');
  AddEditorMenuItem(MI, 'actSelectAll');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actCopyFileName');
  AddEditorMenuItem(MI, 'actCopyFilePath');
  AddEditorMenuItem(MI, 'actCopyFullPath');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actSettings');

  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := 'Se&lection';
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actAlignSelection');
  AddEditorMenuItem(MI, 'actSortSelection');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actLowerCaseSelection');
  AddEditorMenuItem(MI, 'actUpperCaseSelection');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actToggleCommentSelection');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actOpenSelectionInNewEditor');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actQuoteSelection');
  AddEditorMenuItem(MI, 'actDeQuoteSelection');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actQuoteLines');
  AddEditorMenuItem(MI, 'actQuoteLinesAndDelimit');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actPascalStringOfSelection');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actStripFirstChar');
  AddEditorMenuItem(MI, 'actStripLastChar');

  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := '&Search';
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actFind');
  AddEditorMenuItem(MI, 'actReplace');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actFindNext');
  AddEditorMenuItem(MI, 'actFindPrevious');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actFindNextWord');
  AddEditorMenuItem(MI, 'actFindPrevWord');

  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := '&View';
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actIncFontSize');
  AddEditorMenuItem(MI, 'actDecFontSize');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actShowControlCharacters');
  AddEditorMenuItem(MI, 'actShowPreview');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actAutoGuessHighlighter');

  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := '&Tools';
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actInsertCharacterFromMap');
  AddEditorMenuItem(MI, 'actInsertColorValue');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actShapeCode');
  AddEditorMenuItem(MI, 'actFilterCode');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actSmartSelect');
  AddEditorMenuItem(MI, 'actFormat');
  AddEditorMenuItem(MI);
  AddEditorMenuItem(MI, 'actMonitorChanges');

  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := '&Highlighters';
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actHighlighterPAS');
  AddEditorMenuItem(MI, 'actHighlighterSQL');
  AddEditorMenuItem(MI, 'actHighlighterLFM');
  AddEditorMenuItem(MI, 'actHighlighterTXT');
  AddEditorMenuItem(MI, 'actHighlighterBAT');
  AddEditorMenuItem(MI, 'actHighlighterINI');
  AddEditorMenuItem(MI, 'actHighlighterRES');
  AddEditorMenuItem(MI, 'actHighlighterRTF');
  AddEditorMenuItem(MI, 'actHighlighterXML');
  AddEditorMenuItem(MI, 'actHighlighterUNI');

  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := '&Help';
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actHelp');
  AddEditorMenuItem(MI, 'actAbout');

  MI := TMenuItem.Create(AMainMenu.Owner);
  MI.Caption := '&Debug';
  AMainMenu.Items.Add(MI);
  AddEditorMenuItem(MI, 'actInspect');
  AddEditorMenuItem(MI, 'actShowActions');
//
//  AMainMenu.Items.Add();
//
//  MI := TMenuItem.Create(nil);
//  MI.Action := ;
//  s.Caption := 'test';
//  MainMenu1.Items[0].Add(s);
//  mmMain: TMainMenu;
//  mniFile1: TMenuItem;
//  mniEdit1: TMenuItem;
//  mniItem11: TMenuItem;
//  mniItem21: TMenuItem;
//  mniN2: TMenuItem;
//
//  mmMain := TMainMenu.Create(Self);
//  mniFile1 := TMenuItem.Create(Self);
//  mniEdit1 := TMenuItem.Create(Self);
//  mniItem11 := TMenuItem.Create(Self);
//  mniItem21 := TMenuItem.Create(Self);
//  mniN2 := TMenuItem.Create(Self);
//
//  mmMain.Name := 'mmMain';
//  mniFile1.Name := 'mniFile1';
//  mniEdit1.Name := 'mniEdit1';
//  mniItem11.Name := 'mniItem11';
//  mniItem21.Name := 'mniItem21';
//  mniN2.Name := 'mniN2';
end;

end.

