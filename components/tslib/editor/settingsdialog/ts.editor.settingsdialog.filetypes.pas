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

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: OptionsFileTypeForm.pas

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

$Id: OptionsFileTypeForm.pas,v 1.3 2006/10/29 08:09:07 adsoft Exp $
-------------------------------------------------------------------------------}

unit ts.Editor.SettingsDialog.FileTypes;

{$MODE DELPHI}

interface

uses
  SysUtils, Classes, Forms, StdCtrls, ComCtrls,

  ActnList;

{
object tmpFT: TADFileTypes
  Items = <
    item
      Name = 'Default'
      AutoIndent = True
      UseTab = True
      SmartTab = False
      TabWidth = 4
      Highlighter = 'General'
      Filter = 'Any Files (*.*)|*.*'
      Comment = 'rem '
    end>
  Left = 112
  Top = 8
end
}

type
  TfrmFileTypeSettings = class(TForm)
    lbFT: TLabel;
    cbFT: TComboBox;
    bnFTAdd: TButton;
    bnFTDel: TButton;
    pcFT: TPageControl;
    tsFTMain: TTabSheet;
    lbFilter: TLabel;
    gbOptions: TGroupBox;
    lblTabWidth: TLabel;
    lblHighlighter: TLabel;
    lblComment: TLabel;
    chkAutoI: TCheckBox;
    chkUseTab: TCheckBox;
    chkSmartTab: TCheckBox;
    cbH: TComboBox;
    cbFilter: TComboBox;
    tsFTAC: TTabSheet;
    tsFTRun: TTabSheet;
    lblRunCMD: TLabel;
    lblRunParam: TLabel;
    lblRunDir: TLabel;
    lblRunOutput: TLabel;
    cbRunCMD: TComboBox;
    cbRunParam: TComboBox;
    cbRunDir: TComboBox;
    cbRunOutput: TComboBox;
    bnRunCMD: TButton;
    bnRunParam: TButton;
    bnRunDirBrowse: TButton;
    bnRunDirTags: TButton;
    bnRunOutputExp: TButton;
    //tmpFT: TADFileTypes;
    cbTabWidth: TComboBox;
    cbComment: TComboBox;
    lblFTAutocomplete: TLabel;
    bnAC: TButton;
    bnACEdit: TButton;
    cbAC: TComboBox;
    bnRunOutputL: TButton;
    AL: TActionList;
    acAdd: TAction;
    acDel: TAction;
    tsFTHelp: TTabSheet;
    cbHelp: TComboBox;
    lblHelp: TLabel;
    bnHelp: TButton;
    lnkFilters: TLabel;
    procedure cbFTSelect(Sender: TObject);
    procedure bnACEditClick(Sender: TObject);
    procedure bnACClick(Sender: TObject);
    procedure cbACChange(Sender: TObject);
    procedure pcFTChange(Sender: TObject);
    procedure bnRunParamClick(Sender: TObject);
    procedure OptMenuClick(Sender: TObject);
    procedure OutputMenuClick(Sender: TObject);
    procedure bnRunDirBrowseClick(Sender: TObject);
    procedure bnRunCMDClick(Sender: TObject);
    procedure dlgRunBeforeExecute(Sender: TObject);
    procedure dlgRunAfterExecute(Sender: TObject);
    procedure dlgOpenSelectFile(Sender: TObject; const AFilename: String;
      const ALines: TStrings);
    procedure acAddExecute(Sender: TObject);
    procedure acDelExecute(Sender: TObject);
    procedure acDelUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bnHelpClick(Sender: TObject);
    procedure lnkFiltersClick(Sender: TObject);
  private
    function GetCurrentType: integer;
    procedure SetCurrentType(const Value: integer);

    procedure DoAddFTCheck(Sender: TForm; var AError: boolean; var AErrorStr: string);

    procedure DoParamClick(Sender: TObject);
    procedure DoDirClick(Sender: TObject);
    procedure DoOutputClick(Sender: TObject);
    procedure DoOutputLineClick(Sender: TObject);
  private
    { Private declarations }
    //AC: TSynAutoComplete;

    procedure UpdateFilterCombo;

    procedure DoEditAutocomplete;

    procedure SelectFT(const AIndex: integer);

    procedure SaveCurrentType;

    property CurrentType: integer read GetCurrentType write SetCurrentType;
  public
    { Public declarations }
    procedure LoadOptions;
    procedure SaveOptions;
  end;

implementation


{$R *.lfm}

procedure TfrmFileTypeSettings.LoadOptions;
//var
//  i: integer;
begin
  //AC := TSynAutoComplete.Create(Self);

  //LoadMRU(Self);

  //for i := 0 to dmCom.HL.Items.Count - 1 do
  //  cbH.AddItem(dmCom.HL.Items.Strings[i], nil);
  //cbH.Items[0] := SDef;

//  dmCom.LoadFileTypes(tmpFT);
  //UpdateFilterCombo;
end;

procedure TfrmFileTypeSettings.SaveOptions;
begin
  //SaveMRU(Self);
  SaveCurrentType;
  //dmCom.SaveFileTypes(tmpFT);
  //dmCom.LoadFileTypes(dmCom.FT);
  //if Assigned(GI_Editor) then
  //begin
  //  dmCom.FT.Current := dmCom.FT.FindByFilename(GI_Editor.Filename);
  //  GI_Editor.UpdateFileType;
  //end;
end;

procedure TfrmFileTypeSettings.SelectFT(const AIndex: integer);
begin
  //if Assigned(tmpFT.Current) then
  //  SaveCurrentType;

  //cbFT.ItemIndex := AIndex;
  //tmpFT.Current := tmpFT.Items[AIndex];
  //with tmpFT.Current do begin
  //  chkAutoI.Checked := AutoIndent;
  //  chkUseTab.Checked := UseTab;
  //  chkSmartTab.Checked := SmartTab;
  //  cbTabWidth.Text := IntToStr(TabWidth);
  //
  //  cbH.ItemIndex := cbH.Items.IndexOf(Highlighter);
  //  if cbH.ItemIndex = -1 then
  //    cbH.ItemIndex := 0;
  //
  //  cbComment.Text := Comment;
  //
  //  cbFilter.Text := Filter;
  //
  //  cbRunCMD.Text := CMD;
  //  cbRunParam.Text := Param;
  //  cbRunDir.Text := Dir;
  //  cbRunOutput.Text := Output;
  //
  //  cbAC.Text := Autocomplete;

//    cbHelp.Text := Help;
  //end;
end;

procedure TfrmFileTypeSettings.SaveCurrentType;
begin
  //with tmpFT.Current do begin
  //  SmartTab := chkSmartTab.Checked;
  //  UseTab := chkUseTab.Checked;
  //  AutoIndent := chkAutoI.Checked;
  //  Filter := cbFilter.Text;
  //  TabWidth := StrToInt(cbTabWidth.Text);
  //  Highlighter := cbH.Text;
  //  CMD := cbRunCMD.Text;
  //  Param := cbRunParam.Text;
  //  Dir := cbRunDir.Text;
  //  Output := cbRunOutput.Text;
  //  Comment := cbComment.Text;
  //  Autocomplete := cbAC.Text;
//    Help := cbHelp.Text;
  //end;
end;

function TfrmFileTypeSettings.GetCurrentType: integer;
begin
  Result := cbFT.ItemIndex;
end;

procedure TfrmFileTypeSettings.SetCurrentType(const Value: integer);
begin
  cbFT.ItemIndex := Value;
  if Value >= 0 then
    SelectFT(Value);
end;

procedure TfrmFileTypeSettings.cbFTSelect(Sender: TObject);
begin
  CurrentType := cbFT.Items.IndexOf(cbFT.Text);
end;

procedure TfrmFileTypeSettings.DoAddFTCheck(Sender: TForm; var AError: boolean; var AErrorStr: string);
begin
  //with Sender as TdlgInput do begin
  //  AError := tmpFT.IndexOf(Text) <> -1;
  //  if AError then
  //    AErrorStr := Format(GetLangStr('SRecAlreadyExist'), [Text]);
  //end;
end;

procedure TfrmFileTypeSettings.bnACEditClick(Sender: TObject);
begin
  DoEditAutocomplete;
end;

procedure TfrmFileTypeSettings.DoEditAutocomplete;
//var
//  FN: string;
begin
  //with TdlgEditAutocomplete.Create(Self) do
  //try
  //  FN := AppPath + SAutocompleteDir + cbAC.Text;
  //  if not IsValidFilename(FN) then
  //  begin
  //    TT.Items[TT.Items.IndexOf(cbAC)].Text := GetLangStr('SFileCreateError');
  //    TT.ShowToolTip(cbAC);
  //    Exit;
  //  end;
  //  SaveCurrentType;
  //  Item := tmpFT.Current;
//    dlgExp.Filter := dlgOpen.Filter;
//    dlgExp.DefaultExt := dlgOpen.DefaultExt;
//    dlgImp.Filter := dlgOpen.Filter;
//    dlgImp.DefaultExt := dlgOpen.DefaultExt;
  //  ShowModal;
  //finally
  //  Free;
  //end;
end;

procedure TfrmFileTypeSettings.bnACClick(Sender: TObject);
begin
//  TT.HideAllToolTips;
//  with dlgOpen do begin
//    Filename := cbAC.Text;
//    InitialDir := AppPath + SAutocompleteDir;
//    Filter := GetLangStr('SDCIFilter');
//    DefaultExt := SAutocompleteExt;
//    if Execute then
//      cbAC.Text := ExtractFilename(Filename);
//  end;
end;

procedure TfrmFileTypeSettings.cbACChange(Sender: TObject);
begin
  //if TT.Items[TT.Items.IndexOf(cbAC)].Showing then
  //  TT.Items[TT.Items.IndexOf(cbAC)].Activate(False);
end;

procedure TfrmFileTypeSettings.pcFTChange(Sender: TObject);
begin
  //TT.HideAllToolTips;
end;

procedure TfrmFileTypeSettings.UpdateFilterCombo;
//var
//  i: integer;
begin
  cbFT.Clear;
  //for i := 0 to tmpFT.Items.Count - 1 do
  //  cbFT.AddItem(tmpFT.Items[i].Name, nil);

  cbFT.Repaint;
  //cbFT.ItemIndex := cbFT.Items.IndexOf(dmCom.FT.Current.Name);
  cbFTSelect(Self);
end;

procedure TfrmFileTypeSettings.bnRunParamClick(Sender: TObject);
begin
  //ShowPopup(Sender);
end;

procedure TfrmFileTypeSettings.OutputMenuClick(Sender: TObject);
begin
  //case (Sender as TMenuItem).Tag of
  //  0: cbRunOutput.SelText := TagsArray[cTagFullname];
  //  1: cbRunOutput.SelText := TagsArray[cTagLine];
  //  2: cbRunOutput.SelText := TagsArray[cTagCol];
  //end;
end;

procedure TfrmFileTypeSettings.bnRunDirBrowseClick(Sender: TObject);
//var
//  S: string;
begin
  //S := ExtractFileDir(ProcessTags(cbRunDir.Text));
  //if SelectDirectory(GetLangStr('SDirToFind'), '', S) then
  //  cbRunDir.Text := S;
end;

procedure TfrmFileTypeSettings.bnRunCMDClick(Sender: TObject);
begin
//  dlgOpen.InitialDir := ExtractFileDir(ExpandFileName(cbRunCMD.Text));
//  dlgOpen.FileName := cbRunCMD.Text;
//  dlgOpen.Filter := GetLangStr('SCMDFilter');
//  dlgOpen.DefaultExt := SCMDExt;
//
//  if dlgOpen.Execute then
//    cbRunCMD.Text := dlgOpen.FileName;
end;

procedure TfrmFileTypeSettings.OptMenuClick(Sender: TObject);
begin
  //case (Sender as TMenuItem).Tag of
  //  0: cbRunParam.SelText := TagsArray[cTagFullName];
  //  1: cbRunParam.SelText := TagsArray[cTagShortName];
  //  2: cbRunParam.SelText := TagsArray[cTagExtension];
  //  3: cbRunParam.SelText := TagsArray[cTagFilePath];
  //  4: cbRunParam.SelText := TagsArray[cTagLine];
  //  5: cbRunParam.SelText := TagsArray[cTagCol];
  //end;
end;

procedure TfrmFileTypeSettings.dlgRunBeforeExecute(Sender: TObject);
begin
  //ShowDialog(Sender);
end;

procedure TfrmFileTypeSettings.dlgRunAfterExecute(Sender: TObject);
begin
  //CloseDialog(Sender);
end;

procedure TfrmFileTypeSettings.dlgOpenSelectFile(Sender: TObject;
  const AFilename: String; const ALines: TStrings);
begin
  ALines.LoadFromFile(AFilename);
end;

procedure TfrmFileTypeSettings.acAddExecute(Sender: TObject);
//var
//  S: string;
begin
  //with TdlgInput.Create(Self) do
  //try
  //  Name := SFileTypeDialogName;
  //  OnCheckValue := DoAddFTCheck;
  //  Caption := GetLangStr('SAddFTCaption');
  //  Prompt := GetLangStr('SAddFTPrompt');
  //  Text := '';
  //  if ShowModal = mrOK then begin
  //    with tmpFT.Items.Add do begin
  //      S := Text;
  //      Name := S;
  //      SmartTab := False;
  //      UseTab := False;
  //      AutoIndent := True;
  //      TabWidth := 4;
  //      Highlighter := dmCom.HL.DefHighlighter.LanguageName;
  //      Autocomplete := '';
  //      Filter := Format(SNewFilterFormat, [S]);
  //
  //      cbFT.Items.Add(S);
  //      cbFT.ItemIndex := cbFT.Items.Count - 1;
  //      cbFTSelect(Self);
  //    end;
  //  end;
  //finally
  //  Free;
  //end;
end;

procedure TfrmFileTypeSettings.acDelExecute(Sender: TObject);
//var
//  i: integer;
begin
  //i := CurrentType;
  //tmpFT.Items.Delete(CurrentType);
  //tmpFT.Current := nil;
  //cbFT.Items.Delete(CurrentType);
  //if i < cbFT.Items.Count then
  //  CurrentType := i
  //else
  //  CurrentType := i - 1;
end;

procedure TfrmFileTypeSettings.acDelUpdate(Sender: TObject);
begin
  bnFTDel.Enabled := cbFT.Items.IndexOf(cbFT.Text) > 0;
end;

procedure TfrmFileTypeSettings.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to bnRunParam.PopupMenu.Items.Count - 1 do
    bnRunParam.PopupMenu.Items[i].OnClick := DoParamClick;

  for i := 0 to bnRunDirTags.PopupMenu.Items.Count - 1 do
    bnRunDirTags.PopupMenu.Items[i].OnClick := DoDirClick;

  for i := 0 to bnRunOutputExp.PopupMenu.Items.Count - 1 do
    bnRunOutputExp.PopupMenu.Items[i].OnClick := DoOutputClick;

  for i := 0 to bnRunOutputL.PopupMenu.Items.Count - 1 do
    bnRunOutputL.PopupMenu.Items[i].OnClick := DoOutputLineClick;
end;

procedure TfrmFileTypeSettings.DoDirClick(Sender: TObject);
begin
  //InsertStrInCombo(cbRunDir, TMenuItem(Sender).Hint);
end;

procedure TfrmFileTypeSettings.DoOutputClick(Sender: TObject);
begin
  //InsertStrInCombo(cbRunOutput, TMenuItem(Sender).Hint);
end;

procedure TfrmFileTypeSettings.DoOutputLineClick(Sender: TObject);
begin
  //InsertStrInCombo(cbRunOutput, TMenuItem(Sender).Hint);
end;

procedure TfrmFileTypeSettings.DoParamClick(Sender: TObject);
begin
  //InsertStrInCombo(cbRunParam, TMenuItem(Sender).Hint);
end;

procedure TfrmFileTypeSettings.bnHelpClick(Sender: TObject);
begin
//  dlgOpen.InitialDir := ExtractFileDir(ExpandFileName(cbHelp.Text));
//  dlgOpen.FileName := cbHelp.Text;
//  dlgOpen.Filter := GetLangStr('SHelpFilter');
//  dlgOpen.DefaultExt := SHelpExt;
//  if dlgOpen.Execute then
//    cbHelp.Text := dlgOpen.FileName;
end;

procedure TfrmFileTypeSettings.lnkFiltersClick(Sender: TObject);
begin
  //with TdlgSetupFilters.Create(Self) do
  //try
  //  FT := tmpFT;
  //  if ShowModal = mrOK then
  //    UpdateFilterCombo;
  //finally
  //  Free;
  //end;
end;

end.

