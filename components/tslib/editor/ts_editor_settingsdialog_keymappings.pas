{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

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

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: CustomizeDialog.pas

The Initial Developer of the Original Code is: AD <adsoft@nm.ru>
Copyright (c) 2005 ADSoft          
All Rights Reserved.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: CustomizeDialog.pas,v 1.4 2006/11/22 14:13:45 adsoft Exp $
-------------------------------------------------------------------------------}

unit ts_Editor_SettingsDialog_KeyMappings;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ComCtrls, ActnList, ImgList,

  LCLIntf, LCLType, LMessages;

type

  { TfrmKeyMappings }

  TfrmKeyMappings = class(TForm)
    pcCustomize: TPageControl;
    tsToolbars: TTabSheet;
    clbToolbars: TCheckListBox;
    bnOK: TButton;
    tsKeyboard: TTabSheet;
    gbActions: TGroupBox;
    lblActions: TLabel;
    lblCategory: TLabel;
    lbCategory: TListBox;
    gbSC: TGroupBox;
    //hkSC: THotKey;
    lblNewSC: TLabel;
    lblCurValue: TLabel;
    lbSC: TListBox;
    gbDescr: TGroupBox;
    bnAdd: TButton;
    bnDel: TButton;
    lblDescr: TLabel;
    AL: TActionList;
    lvActions: TListView;
    acAdd: TAction;
    acDel: TAction;
    tsOptions: TTabSheet;
    chkLargeIcons: TCheckBox;
    chkShowHint: TCheckBox;
    bnCancel: TButton;
    alTmp: TActionList;
    procedure FormCreate(Sender: TObject);
    procedure lbCategoryClick(Sender: TObject);
    procedure lvActionsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure acAddExecute(Sender: TObject);
    procedure acAddUpdate(Sender: TObject);
    procedure acDelExecute(Sender: TObject);
    procedure acDelUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lbSCKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FCurrentCategory: string;
    FCurrentAction: TAction;
    FSCList: TShortCutList;

    procedure SetCurrentCategory(const Value: string);
    procedure SelectCategory;
    procedure SelectAction;
    procedure SetCurrentAction(const Value: TAction);
  public
    { Public declarations }
    property CurrentCategory: string read FCurrentCategory write SetCurrentCategory;
    property CurrentAction: TAction read FCurrentAction write SetCurrentAction;
  end;


implementation

uses
  Menus;

{$R *.lfm}

procedure TfrmKeyMappings.FormCreate(Sender: TObject);
var
  i, j: integer;
  S: string;
begin
  //for i := 0 to frmMain.alMain.ActionCount - 1 do
  //  with TAction.Create(Self) do
  //  begin
  //    Name := TAction(frmMain.alMain.Actions[i]).Name;
  //    Caption := TAction(frmMain.alMain.Actions[i]).Caption;
  //    ShortCut := TAction(frmMain.alMain.Actions[i]).ShortCut;
  //    SecondaryShortcuts.Assign(TAction(frmMain.alMain.Actions[i]).SecondaryShortCuts);
  //    ImageIndex := TAction(frmMain.alMain.Actions[i]).ImageIndex;
  //    Hint := TAction(frmMain.alMain.Actions[i]).Hint;
  //    Category := TAction(frmMain.alMain.Actions[i]).Category;
  //    ActionList := alTmp;
  //  end;
//  alTmp.Images := dmCom.ilMain;

  FSCList := TShortCutList.Create;

  //chkLargeIcons.Checked := Options.Customize.LargeIcons;
  //chkShowHint.Checked := Options.Customize.ShowHints;
  
  //for i := 0 to frmMain.ComponentCount - 1 do
  //  if frmMain.Components[i] is TTBXToolbar then
  //    with TTBXToolbar(frmMain.Components[i]) do
  //      if not MenuBar then
  //      begin
  //        clbToolbars.AddItem(StringReplace(TTBXToolbar(frmMain.Components[i]).Caption,
  //          '&', '', [rfReplaceAll]), TTBXToolbar(frmMain.Components[i]));
  //        clbToolbars.Checked[clbToolbars.Count - 1] := Visible;
  //      end;
  //for i := 0 to alTmp.ActionCount - 1 do
  //  with alTmp do begin
  //    S := Actions[i].Category;
  //
  //    if TAction(Actions[i]).ShortCut <> 0 then
  //      FSCList.Add(ShortCutToText(TAction(Actions[i]).ShortCut));
  //    for j := 0 to TAction(Actions[i]).SecondaryShortCuts.Count - 1 do
  //      FSCList.Add(TAction(Actions[i]).SecondaryShortCuts.Strings[j]);
  //
  //    if lbCategory.Items.IndexOf(S) = -1 then
  //      lbCategory.AddItem(S, alTmp.Actions[i]);
  //  end;
  //lbCategory.Sorted := True;

  //CurrentCategory := lbCategory.Items[0];
  //lvActions.Items[0].Selected := True;
  //lvActionsSelectItem(Self, lvActions.Items[0], True);
end;

procedure TfrmKeyMappings.lbCategoryClick(Sender: TObject);
begin
  CurrentCategory := lbCategory.Items[lbCategory.ItemIndex];
end;

procedure TfrmKeyMappings.SelectCategory;
var
  i: integer;
  S: ShortString;
begin
  lvActions.Clear;
  S := TAction(lbCategory.Items.Objects[lbCategory.ItemIndex]).Category;
  for i := 0 to alTmp.ActionCount - 1 do
    if (alTmp.Actions[i].Category = S) and
      (Pos('Menu', alTmp.Actions[i].Name) = 0) and
      (Pos('Virtual', alTmp.Actions[i].Name) = 0) then
      with lvActions.Items.Add do
      begin
        Caption := StringReplace(TAction(alTmp.Actions[i]).Caption,
          '&', '', [rfReplaceAll]);
        Data := TAction(alTmp.Actions[i]);
        ImageIndex := TAction(alTmp.Actions[i]).ImageIndex;
      end;
  lvActions.AlphaSort;
end;

procedure TfrmKeyMappings.SetCurrentCategory(const Value: string);
begin
  FCurrentCategory := Value;
  lbCategory.ItemIndex := lbCategory.Items.IndexOf(Value);
  SelectCategory;
end;

procedure TfrmKeyMappings.lvActionsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if not Assigned(Item) then
    Exit;
  CurrentAction := TAction(Item.Data);
end;

procedure TfrmKeyMappings.SetCurrentAction(const Value: TAction);
begin
  FCurrentAction := Value;
  SelectAction;
end;

procedure TfrmKeyMappings.SelectAction;
var
  i: integer;
begin
  //lbSC.Clear;
  //with FCurrentAction do
  //begin
  //  if ShortCut <> 0 then
  //    lbSC.AddItem(ShortCutToText(ShortCut), nil);
  //  for i := 0 to SecondaryShortCuts.Count - 1 do
  //    lbSC.AddItem(SecondaryShortCuts.Strings[i], nil);
  //  if Hint <> '' then
  //    lblDescr.Caption := Hint
  //  else
  //    lblDescr.Caption := Caption;
  //end;
end;

procedure TfrmKeyMappings.acAddExecute(Sender: TObject);
var
  j: integer;
  A: TAction;
begin
  //A := GetActionByShortCut(alTmp, hkSC.HotKey);
  //if Assigned(A) then
  //  if Application.MessageBox(PChar(
  //    Format(GetLangStr('SShortCutExist'), [A.Category,
  //      StringReplace(A.Caption, '&', '', [rfReplaceAll])])), PChar(SAppName), MBStdQuery) = IDYES then
  //  begin
  //    with alTmp do
  //    begin
  //      if A.ShortCut = hkSC.HotKey then
  //        A.ShortCut := 0;
  //      j := A.SecondaryShortCuts.IndexOfShortCut(hkSC.HotKey);
  //      if j <> -1 then
  //        A.SecondaryShortCuts.Delete(j);
  //    end;
  //  end else
  //    Exit;
  //if lbSC.Count = 0 then
  //  FCurrentAction.ShortCut := hkSC.HotKey
  //else
  //  FCurrentAction.SecondaryShortCuts.Add(
  //    ShortCutToText(hkSC.HotKey));
  //FSCList.Add(ShortCutToText(hkSC.HotKey));
  //SelectAction;
end;

procedure TfrmKeyMappings.acDelExecute(Sender: TObject);
begin
  //if lbSC.ItemIndex = 0 then
  //begin
  //  FCurrentAction.ShortCut := 0;
  //  if lbSC.Count > 1 then
  //  begin
  //    FCurrentAction.ShortCut := TextToShortCut(lbSC.Items[1]);
  //    FCurrentAction.SecondaryShortCuts.Delete(0);
  //  end;
  //end
  //else
  //  FCurrentAction.SecondaryShortCuts.Delete(
  //    lbSC.ItemIndex - 1);
  //SelectAction;
end;

procedure TfrmKeyMappings.acAddUpdate(Sender: TObject);
begin
  //acAdd.Enabled := hkSC.HotKey > 0;
end;

procedure TfrmKeyMappings.acDelUpdate(Sender: TObject);
begin
  //acDel.Enabled := lbSC.ItemIndex <> -1;
end;

procedure TfrmKeyMappings.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  i: integer;
begin
  if ModalResult <> mrOK then
    Exit;

  //for i := 0 to alTmp.ActionCount - 1 do
  //  with TAction(frmMain.alMain.Actions[i]) do
  //  begin
  //    ShortCut := TAction(alTmp.Actions[i]).ShortCut;
  //    SecondaryShortcuts.Assign(TAction(alTmp.Actions[i]).SecondaryShortCuts);
  //  end;
  //
  //for i := 0 to clbToolbars.Count - 1 do
  //  TTBXToolbar(clbToolbars.Items.Objects[i]).Visible := clbToolbars.Checked[i];
  //dmCom.SaveShortCuts;

  //Options.Customize.LargeIcons := chkLargeIcons.Checked;
  //Options.Customize.ShowHints := chkShowHint.Checked;

  FSCList.Free;
end;

procedure TfrmKeyMappings.lbSCKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    acDel.Execute;
end;

end.
