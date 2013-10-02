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

The Original Code is: OptionsAssociateForm.pas

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

$Id: OptionsAssociateForm.pas,v 1.4 2006/11/09 08:39:12 adsoft Exp $
-------------------------------------------------------------------------------}

// based on OptionsAssociateForm.pas

unit ts_Editor_SettingsDialog_FileAssociations;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ActnList, ExtCtrls,

  LCLIntf, LCLType, LMessages,

  ts.Core.FileAssociations;

type
  TfrmOptionsAssociate = class(TForm)
    lblExt: TLabel;
    lvExt: TListView;
    btnAdd: TButton;
    btnEdit: TButton;
    btnDel: TButton;
    btnSelectAll: TButton;
    btnDeselectAll: TButton;
    gbxOptions: TGroupBox;
    lblMenuCaption: TLabel;
    edMenuText: TEdit;
    gbxOther: TGroupBox;
    chkAddToMenu: TCheckBox;
    chkAssocToUnknown: TCheckBox;
    alAssociate: TActionList;
    actAdd: TAction;
    actEdit: TAction;
    actDelete: TAction;
    actSelectAll: TAction;
    actDeselectAll: TAction;
    pnlAccess: TPanel;
    chkAllUsers: TCheckBox;
    lblBackup: TLabel;

    procedure actEditUpdate(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actDeselectAllExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvExtDblClick(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure chkAllUsersClick(Sender: TObject);

    procedure DoChange(Sender: TObject);
    procedure lvExtChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lnkBackupClick(Sender: TObject);

  private
    FMod: Boolean;
    FA: TFileAssociate;
    function GetCurrentItem: TAssociateItem;
    procedure SetCurrentItem(const Value: TAssociateItem);

    procedure DoLoadAssoc;
    procedure AddEditExt(const AEdit: Boolean);

    property CurrentItem: TAssociateItem
      read GetCurrentItem write SetCurrentItem;

  public
    procedure LoadOptions;
    procedure SaveOptions;
  end;

implementation

{$R *.lfm}

uses
  ADRegINI;

  {Utilits,
  , ADUtils}

var
  gbAppHK: HKEY;

const
  SAppKey = 'Software\';

procedure TfrmOptionsAssociate.actAddExecute(Sender: TObject);
begin
  AddEditExt(False);
end;

procedure TfrmOptionsAssociate.actEditExecute(Sender: TObject);
begin
  AddEditExt(True);
end;

procedure TfrmOptionsAssociate.actEditUpdate(Sender: TObject);
begin
  actEdit.Enabled := lvExt.SelCount = 1;
end;

procedure TfrmOptionsAssociate.actDeleteExecute(Sender: TObject);
var
  I: integer;
begin
  I := 0;
  while I <= lvExt.Items.Count - 1 do
    if lvExt.Items[I].Selected then
      FA.DeleteItem(I)
    else
      Inc(I);
end;

procedure TfrmOptionsAssociate.actDeleteUpdate(Sender: TObject);
begin
  actDelete.Enabled := lvExt.SelCount > 0;
end;

procedure TfrmOptionsAssociate.actSelectAllExecute(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to lvExt.Items.Count - 1 do
    lvExt.Items[I].Checked := True;
end;

procedure TfrmOptionsAssociate.actDeselectAllExecute(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to lvExt.Items.Count - 1 do
    lvExt.Items[I].Checked := False;
end;

procedure TfrmOptionsAssociate.LoadOptions;
begin
  //if not ADRegCheckAccess(HKCU, SClassesKey,
  //  KEY_READ or KEY_WRITE or KEY_CREATE_SUB_KEY) then
  //begin
  //  pnlAccess.Align := alClient;
  //  pnlAccess.BringToFront;
  //  pnlAccess.Visible := True;
  //end
  //else
  //  pnlAccess.Visible := False;
  //
  //chkAllUsers.Enabled := ADRegCheckAccess(HKCR, '', KEY_READ or KEY_WRITE or KEY_CREATE_SUB_KEY);
  //FA.AllUsers := ADRegKeyExist(  HKLM, SAppKey{, KEY_READ or KEY_WRITE or KEY_CREATE_SUB_KEY});
  //chkAllUsers.Checked := FA.AllUsers;
  //
  //FMod := False;
 DoLoadAssoc;
end;

procedure TfrmOptionsAssociate.SaveOptions;
//var
//  i: integer;
begin
  //FA.MenuCaption := edMenuText.Text;
  //FA.IsAssociateToContext := chkAddToMenu.Checked;
  //FA.IsAssociateToUnknown := chkAssocToUnknown.Checked;
  //
  //for i := 0 to lvExt.Items.Count - 1 do
  //  FA.Items[i].IsAssociate := lvExt.Items[i].Checked;
  //
  //FA.SaveAssociates;
end;

procedure TfrmOptionsAssociate.FormCreate(Sender: TObject);
begin
  FA := TFileAssociate.Create(Self);
  FA.RootKey := gbAppHK;
  FA.AppKey := SAppKey + Application.Name;
  FA.AppDescription := Application.Name;
  FA.ListView := lvExt;
end;

function TfrmOptionsAssociate.GetCurrentItem: TAssociateItem;
begin
  Result := FA.Items[lvExt.ItemIndex];
end;

procedure TfrmOptionsAssociate.SetCurrentItem(const Value: TAssociateItem);
begin
  lvExt.ItemIndex := Value.Index;
end;

procedure TfrmOptionsAssociate.lvExtDblClick(Sender: TObject);
begin
  if Assigned(lvExt.Selected) then
    actEdit.Execute;
end;

procedure TfrmOptionsAssociate.AddEditExt(const AEdit: Boolean);
begin
  //with TdlgExt.Create(Self) do
  //try
  //  Associate := FA;
  //  EditMode := AEdit;
  //  if AEdit then
  //  begin
  //    Ext := CurrentItem.Ext;
  //    IconName := CurrentItem.Icon;
  //    Description := CurrentItem.Descr;
  //  end else
  //  begin
  //    Ext := '';
  //    IconName := '';
  //    Description := '';
  //  end;
  //  if ShowModal = mrOK then
  //  begin
  //    if AEdit then
  //    begin
  //      CurrentItem.Ext := Ext;
  //      CurrentItem.Icon := IconName;
  //      CurrentItem.Descr := Description;
  //    end else
  //    begin
  //      lvExt.ClearSelection;
  //      lvExt.Selected := lvExt.Items[lvExt.Items.Count - 1];
  //      lvExt.Selected.Checked := True;
  //      CurrentItem.Ext := Ext;
  //      CurrentItem.Icon := IconName;
  //      CurrentItem.Descr := Description;
  //    end;
  //  end;
  //finally
  //  Free;
  //end;
end;

procedure TfrmOptionsAssociate.chkAllUsersClick(Sender: TObject);
begin
  DoLoadAssoc;
end;

procedure TfrmOptionsAssociate.DoLoadAssoc;
begin
  //if FMod then
  //  if MessageBox(Handle, PChar(GetLangStr('SSaveAssocQuery')), PChar(SAppName), MBStdQuery) = IDYES then
  //    SaveOptions;
  lvExt.Clear;
  FA.AllUsers := chkAllUsers.Checked;
  //if FA.AllUsers <> chkAllUsers.Checked then
  //  MessageBox(Handle, PChar(pnlAccess.Caption), PChar(SAppName), MBOK);
  chkAllUsers.Checked := FA.AllUsers;
  FA.LoadAssociates;
  chkAddToMenu.Checked := FA.IsAssociateToContext;
  chkAssocToUnknown.Checked := FA.IsAssociateToUnknown;
  //edMenuText.Text := FA.MenuCaption;
//  if FA.AllUsers then
//    lblExt.Caption := Format(GetLangStr('SAssocUser'), [GetLangStr('SAllUsers')])
//  else
//    lblExt.Caption := Format(GetLangStr('SAssocUser'), [GetLangStr('SCurrentUser')]);
//  FMod := False;
end;

procedure TfrmOptionsAssociate.DoChange(Sender: TObject);
begin
  FMod := True;
end;

procedure TfrmOptionsAssociate.lvExtChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  DoChange(Self);
end;

procedure TfrmOptionsAssociate.lnkBackupClick(Sender: TObject);
var
  Param: string;
  Res: integer;
begin
  with TSaveDialog.Create(Self) do
    try
      DefaultExt := 'reg';
      //Filter := GetLangStr('SRegFilter');
      if Execute then
      begin
        //if FA.AllUsers then
        //  Param := Format(SBackupRegTemp, [Filename, SHKCR])
        //else
        //  Param := Format(SBackupRegTemp, [Filename, SHKCU + '\' + SClassesKey]);
        //Res :=  OpenDocument(PChar(SRegEdit)); { *Converted from ShellExecute* }
        //if Res <= 32 then
        //  Application.MessageBox(PChar(GetErrorStr(Res)), PChar(SAppName), MBOK);
      end;
    finally
      Free;
    end;
end;

end.
