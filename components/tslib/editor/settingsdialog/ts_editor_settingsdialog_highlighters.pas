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

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: OptionsThemesForm.pas

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

$Id: OptionsThemesForm.pas,v 1.4 2006/11/22 14:13:45 adsoft Exp $
-------------------------------------------------------------------------------}

unit ts_Editor_SettingsDialog_Highlighters;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  {ADSynEditTheme,}
  FileUtil, Spin,
  {ADExtDialogs,}

  LCLIntf, LCLType, LMessages;

type
  TfrmOptionsThemes = class(TForm)
    lbTheme: TLabel;
    pcTheme: TPageControl;
    tsThemeMain: TTabSheet;
    gbDColors: TGroupBox;
    lbDFG: TLabel;
    lbDBG: TLabel;
    gbDSelColors: TGroupBox;
    lbDSelFG: TLabel;
    lbDSelBG: TLabel;
    gbDFont: TGroupBox;
    pnDFont: TPanel;
    tsThemeHighlight: TTabSheet;
    lblHighlight: TLabel;
    cbHighlighter: TComboBox;
    gbHiAttrs: TGroupBox;
    lbHiAttrs: TListBox;
    tbHiStyle: TGroupBox;
    chkHiStyleBold: TCheckBox;
    chkHiStyleItalic: TCheckBox;
    chkHiStyleUnderline: TCheckBox;
    gbHiColor: TGroupBox;
    lblHiFGColor: TLabel;
    lblHiBGColor: TLabel;
    tsThemeGutter: TTabSheet;
    lblGColor: TLabel;
    lblGWidth: TLabel;
    chkGVisible: TCheckBox;
    tsThemeOther: TTabSheet;
    gbDOther: TGroupBox;
    lblHCLColor: TLabel;
    lblHBColor: TLabel;
    chkHighlightCurrentLine: TCheckBox;
    chkHighlightBrackets: TCheckBox;
    gbREdge: TGroupBox;
    lblREdgeAt: TLabel;
    lbREdgeColor: TLabel;
    cbTheme: TComboBox;
    bnThemeAdd: TButton;
    bnThemeDel: TButton;
    dlgFont: TFontDialog;
    dlgGFont: TFontDialog;
    tsThemeReadMode: TTabSheet;
    gbRMSize: TGroupBox;
    gbRMFont: TGroupBox;
    pnRMFont: TPanel;
    spnREdgeAt: TSpinEdit;
    spnRMFontSize: TSpinEdit;
    spnGWidth: TSpinEdit;
    gbGShowNumbers: TGroupBox;
    chkGShowNumbers: TCheckBox;
    chkGAutosize: TCheckBox;
    chkGLeadingZeros: TCheckBox;
    chkGZeroStart: TCheckBox;
    lblGDigitsCount: TLabel;
    spnGDigitsCount: TSpinEdit;
    gbGGrad: TGroupBox;
    chkGUseGrad: TCheckBox;
    lblGStartColor: TLabel;
    lblGEndColor: TLabel;
    lblGSteps: TLabel;
    spnGSteps: TSpinEdit;
    lblGFontColor: TLabel;
    gbURL: TGroupBox;
    chkURL: TCheckBox;
    gbURLStyle: TGroupBox;
    chkURLBold: TCheckBox;
    chkURLItalic: TCheckBox;
    chkURLUnderline: TCheckBox;
    lblURLColor: TLabel;
    lblBorder: TLabel;
    gbCaret: TGroupBox;
    cbInsCaret: TComboBox;
    cbOverCaret: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    pnAccess: TPanel;
    dlgTWFont: TFontDialog;
    bnChangeFont: TButton;
    chkAlwaysShowURL: TCheckBox;
    procedure cbThemeSelect(Sender: TObject);
    procedure bnDFGSelectColor(Sender: TObject; const AColor: TColor);
    procedure bnDBGSelectColor(Sender: TObject; const AColor: TColor);
    procedure chkHighlightCurrentLineClick(Sender: TObject);
    procedure chkHighlightBracketsClick(Sender: TObject);
    procedure cbHighlighterSelect(Sender: TObject);
    procedure lbHiAttrsClick(Sender: TObject);
    procedure chkHiStyleBoldClick(Sender: TObject);
    procedure chkHiStyleItalicClick(Sender: TObject);
    procedure chkHiStyleUnderlineClick(Sender: TObject);
    procedure bnHiFGSelectColor(Sender: TObject; const AColor: TColor);
    procedure bnHiBGSelectColor(Sender: TObject; const AColor: TColor);
    procedure SEMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure bnThemeDelClick(Sender: TObject);
    procedure bnThemeAddClick(Sender: TObject);
    procedure bnHighlightCurrentLineColorSelectColor(Sender: TObject;
      const AColor: TColor);
    procedure spnREdgeAtChange(Sender: TObject);
    procedure bnREdgeColorSelectColor(Sender: TObject; const AColor: TColor);
    procedure spnRMFontSizeChange(Sender: TObject);
    procedure chkGVisibleClick(Sender: TObject);
    procedure chkGShowNumbersClick(Sender: TObject);
    procedure chkGUseGradClick(Sender: TObject);
    procedure bnGColorSelectColor(Sender: TObject; const AColor: TColor);
    procedure spnGWidthChange(Sender: TObject);
    procedure chkGAutosizeClick(Sender: TObject);
    procedure chkGLeadingZerosClick(Sender: TObject);
    procedure chkGZeroStartClick(Sender: TObject);
    procedure spnGDigitsCountChange(Sender: TObject);
    procedure bnGStartColorSelectColor(Sender: TObject;
      const AColor: TColor);
    procedure bnGEndColorSelectColor(Sender: TObject;
      const AColor: TColor);
    procedure spnGStepsChange(Sender: TObject);
    procedure bnGFontColorSelectColor(Sender: TObject;
      const AColor: TColor);
    procedure chkURLClick(Sender: TObject);
    procedure bnURLColorSelectColor(Sender: TObject; const AColor: TColor);
    procedure chkURLBoldClick(Sender: TObject);
    procedure chkURLItalicClick(Sender: TObject);
    procedure chkURLUnderlineClick(Sender: TObject);
    procedure bnBorderColorSelectColor(Sender: TObject;
      const AColor: TColor);
    procedure bnTWBGSelectColor(Sender: TObject; const AColor: TColor);
    procedure FormCreate(Sender: TObject);
    procedure bnChangeFontClick(Sender: TObject);
    procedure bnTWFontClick(Sender: TObject);
  private
    { Private declarations }

    procedure DoAddThemeCheck(Sender: TForm; var AError: boolean; var AErrorStr: string);

    function DoInserTheme(const AFilename: string;
      const ARec: TSearchRec; const AParam: integer): boolean;
    function GetThemeFilename(const AName: string): string;
    function GetTmpThemeFilename(const AName: string): string;
    procedure SaveCurrentTheme;
    procedure SelectTheme(const AIndex: integer; const ASave: boolean = True);
    procedure SelectHighlighter(const AIndex: integer);
    procedure SelectAttribute(const AIndex: integer);
    procedure SetMainFont(const AFont: TFont);

    function CheckSpinEdit(Sender: TObject): boolean;

    procedure UpdateTheme;
    procedure InitThemeControls;

    procedure EnableThemeControls(const AEnable: boolean);
  public
    { Public declarations }
    procedure LoadOptions;
    procedure SaveOptions;
  end;

implementation

{$R *.lfm}

uses
  SynEditTypes
  {InputDialog,}
  {uThemes};

{ TfrmOptionsThemes }

function TfrmOptionsThemes.DoInserTheme(const AFilename: string;
  const ARec: TSearchRec; const AParam: integer): boolean;
begin
  Result := True;
  //if IsFileCanReadWrite(AFilename) then
  //begin
  //  if not CopyFile(PChar(AFilename), PChar(ChangeFileExt(AFilename, SBackExt)), False) then
  //    Exit;
  //  cbTheme.AddItem(ChangeFileExt(ARec.Name, ''), nil);
  //end;
end;

procedure TfrmOptionsThemes.LoadOptions;
//var
//  i: integer;
begin
  //FOldTheme := dmCom.TH.ThemeName;

  cbOverCaret.Items.Assign(cbInsCaret.Items);

  //dlgFont.MinFontSize := cMinFontSize;
  //dlgFont.MaxFontSize := cMaxFontSize;
  //dlgGFont.MinFontSize := cMinFontSize;
  //dlgGFont.MaxFontSize := cMaxFontSize;

  //spnGDigitsCount.MinValue := cMinGutterDigit;
  //spnGDigitsCount.MaxValue := cMaxGutterDigit;
  //
  //spnRMFontSize.MinValue := cMinFontSize;
  //spnRMFontSize.MaxValue := cMaxFontSize;

  //CommandModule.InitHighligtersList(Self, TH.Highlighters);
  //TH.Highlighters.DefHighlighter := SynGeneralSyn;
  //TH.Highlighters.SortList;

  //for i := 0 to TH.Highlighters.Items.Count - 1 do
  //  cbHighlighter.AddItem(TH.Highlighters.Items.Strings[i], nil);
  //
  //lbHiAttrs.Items.Clear;
  //for i := 0 to TH.Highlighters.Attributes.Count - 1 do
  //  lbHiAttrs.Items.Add(TH.Highlighters.Attributes[i].Name);
  //
  //TH.Highlighters.UpdateAttributes;
  //
  //ScanDir(AppPath + SThemesPath + SCfgMask, faAnyFile, DoInserTheme, 0);
  //cbTheme.Items.Insert(0, SDefTheme);
  //SelectTheme(cbTheme.Items.IndexOf(dmCom.TH.ThemeName), False);
end;

procedure TfrmOptionsThemes.SaveOptions;
var
  i: integer;
  S: string;
begin
  SaveCurrentTheme;

  for i := 0 to cbTheme.Items.Count - 1 do
  begin
    S := cbTheme.Items[i];
    CopyFile(PChar(GetTmpThemeFilename(S)), PChar(GetThemeFilename(S)), False);
  end;

  //if cbTheme.Items.IndexOf(FOldTheme) = -1 then
  //  LoadThemes(FOldTheme)
  //else
  //  LoadThemes(TH.ThemeName);
end;

procedure TfrmOptionsThemes.cbThemeSelect(Sender: TObject);
begin
  SelectTheme(cbTheme.ItemIndex);
end;

function TfrmOptionsThemes.GetThemeFilename(const AName: string): string;
begin
  Result := '';
  //Result := AppPath + SThemesPath + ChangeFileExt(AName, SCfgExt);
end;

function TfrmOptionsThemes.GetTmpThemeFilename(
  const AName: string): string;
begin
  Result := '';
  //Result := ChangeFileExt(GetThemeFilename(AName), SBackExt);
end;

procedure TfrmOptionsThemes.SelectTheme(const AIndex: integer; const ASave: boolean = True);
begin
  cbTheme.ItemIndex := AIndex;
  bnThemeDel.Enabled := cbTheme.ItemIndex > 0;

  if ASave then
    SaveCurrentTheme;
  try
    if cbTheme.ItemIndex = 0 then
    begin
      EnableThemeControls(False);
      Abort;
    end
    else
    begin
      //TH.Filename := GetTmpThemeFilename(cbTheme.Items[cbTheme.ItemIndex]);
      //TH.LoadTheme;
      //EnableThemeControls(not FileIsReadOnlyUTF8(TH.Filename) { *Converted from FileIsReadOnly* });
    end;
  except
    //TH.SetDefaultTheme;
  end;

  InitThemeControls;
end;

procedure TfrmOptionsThemes.SaveCurrentTheme;
begin
//  if (cbTheme.ItemIndex = 0) or (TH.ThemeName = SDefTheme) then
//    Exit;
//
//  // Main - Font
//  TH.Font.Assign(pnDFont.Font);
//  // Main - Colors
//  TH.Font.Color := bnDFG.Selected;
//  TH.Color := bnDBG.Selected;
//  // Main - SelColors
//  TH.SelectedColor.Foreground := bnDSelFG.Selected;
//  TH.SelectedColor.Background := bnDSelBG.Selected;
//  // ReadMode
//  TH.ReadFontSize := spnRMFontSize.Value;
//
//  // Othen
//  if chkHighlightCurrentLine.Checked then
//    TH.ActiveLineColor := bnHighlightCurrentLineColor.Selected
//  else
//    TH.ActiveLineColor := clNone;
//  if chkHighlightBrackets.Checked then
//    TH.HighlightBracketsColor := bnHighlightBracketsColor.Selected
//  else
//    TH.HighlightBracketsColor := clNone;
//
//  // Gutter
//  TH.Gutter.Visible := chkGVisible.Checked;
//  TH.Gutter.Color := bnGColor.Selected;
//  TH.Gutter.BorderColor := bnBorderColor.Selected;
//  TH.Gutter.Width := spnGWidth.Value;
//
//  // Gutter - Line Num
//  TH.Gutter.ShowLineNumbers := chkGShowNumbers.Checked;
//  TH.Gutter.AutoSize := chkGAutosize.Checked;
//  TH.Gutter.LeadingZeros := chkGLeadingZeros.Checked;
//  TH.Gutter.ZeroStart := chkGZeroStart.Checked;
//  TH.Gutter.DigitCount := spnGDigitsCount.Value;
//
//  // Gutter - Grad
//  TH.Gutter.Gradient := chkGUseGrad.Checked;
//  TH.Gutter.GradientStartColor := bnGStartColor.Selected;
//  TH.Gutter.GradientEndColor := bnGEndColor.Selected;
//  TH.Gutter.GradientSteps := spnGSteps.Value;
//
//  // Gutter - FontColor
//  TH.Gutter.Font.Color := bnGFontColor.Selected;
//
//  // Caret
////  TH.InsertCaret := TSynEditCaretType(cbInsCaret.ItemIndex);
////  TH.OverwriteCaret := TSynEditCaretType(cbOverCaret.ItemIndex);
////
////  // Tool Windows
////  TH.ToolFont.Assign(pnTWFont.Font);
////  TH.ToolColor := bnTWBG.Selected;
////  TH.ToolActiveLineColor := bnTWAL.Selected;
//  
//  // URL
//  if chkURL.Checked then
//    TH.URLColor := bnURLColor.Selected
//  else
//    TH.URLColor := clNone;
////  TH.AlwaysShowURL := chkAlwaysShowURL.Checked;
//  try
//    TH.SaveTheme;
//  except
//  end;
end;

procedure TfrmOptionsThemes.SetMainFont(const AFont: TFont);
begin
  //pnDFont.Font.Assign(AFont);
  //pnRMFont.Font.Assign(AFont);
  //pnRMFont.Font.Size := spnRMFontSize.Value;
  //dlgFont.Font.Assign(AFont);
  //bnDFG.Selected := AFont.Color;
  //TH.Font.Assign(AFont);
  //with TH.Gutter.Font do
  //begin
  //  Charset := AFont.Charset;
  //  Name := AFont.Name;
  //  Size := AFont.Size;
  //  Style := AFont.Style;
  //end;
  //UpdateTheme;
end;

procedure TfrmOptionsThemes.bnDFGSelectColor(Sender: TObject;
  const AColor: TColor);
begin
  //TH.Font.Color := AColor;
  //SetMainFont(TH.Font);
end;

procedure TfrmOptionsThemes.bnDBGSelectColor(Sender: TObject;
  const AColor: TColor);
begin
  //TH.Color := AColor;
  //UpdateTheme;
  //pnDFont.Color := AColor;
  //pnRMFont.Color := AColor;
end;

procedure TfrmOptionsThemes.chkHighlightCurrentLineClick(Sender: TObject);
begin
  //EnableControls([lblHCLColor, bnHighlightCurrentLineColor], chkHighlightCurrentLine.Checked);
  //if chkHighlightCurrentLine.Checked then
  //  TH.ActiveLineColor := bnHighlightCurrentLineColor.Selected
  //else
  //  TH.ActiveLineColor := clNone;
  //UpdateTheme;
end;

procedure TfrmOptionsThemes.chkHighlightBracketsClick(Sender: TObject);
begin
  //EnableControls([lblHBColor, bnHighlightBracketsColor], chkHighlightBrackets.Checked);
end;

procedure TfrmOptionsThemes.cbHighlighterSelect(Sender: TObject);
begin
  SelectHighlighter(cbHighlighter.ItemIndex);
end;

procedure TfrmOptionsThemes.SelectHighlighter(const AIndex: integer);
begin
  //cbHighlighter.ItemIndex := AIndex;
  //SE.Highlighter := TH.Highlighters.FindByName(cbHighlighter.Text);
  //SE.Text := SE.Highlighter.SampleSource;
  //
  //SelectAttribute(0);
end;

procedure TfrmOptionsThemes.SelectAttribute(const AIndex: integer);
begin
  //lbHiAttrs.ItemIndex := AIndex;
  //with TH.Highlighters.Attributes[AIndex] do begin
  //  chkHiStyleBold.Checked := fsBold in Style;
  //  chkHiStyleItalic.Checked := fsItalic in Style;
  //  chkHiStyleUnderline.Checked := fsUnderline in Style;
  //  bnHiFG.Selected := Foreground;
  //  bnHiBG.Selected := Background;
  //end;
end;

procedure TfrmOptionsThemes.lbHiAttrsClick(Sender: TObject);
begin
  //SelectAttribute(lbHiAttrs.ItemIndex);
end;

procedure TfrmOptionsThemes.chkHiStyleBoldClick(Sender: TObject);
begin
  //if chkHiStyleBold.Checked then
  //  TH.Highlighters.Attributes[lbHIAttrs.ItemIndex].Style :=
  //    TH.Highlighters.Attributes[lbHIAttrs.ItemIndex].Style + [fsBold]
  //else
  //  TH.Highlighters.Attributes[lbHIAttrs.ItemIndex].Style :=
  //    TH.Highlighters.Attributes[lbHIAttrs.ItemIndex].Style - [fsBold];
  //UpdateTheme;
end;

procedure TfrmOptionsThemes.chkHiStyleItalicClick(Sender: TObject);
begin
  //if chkHiStyleItalic.Checked then
  //  TH.Highlighters.Attributes[lbHIAttrs.ItemIndex].Style :=
  //    TH.Highlighters.Attributes[lbHIAttrs.ItemIndex].Style + [fsItalic]
  //else
  //  TH.Highlighters.Attributes[lbHIAttrs.ItemIndex].Style :=
  //    TH.Highlighters.Attributes[lbHIAttrs.ItemIndex].Style - [fsItalic];
  //UpdateTheme;
end;

procedure TfrmOptionsThemes.chkHiStyleUnderlineClick(Sender: TObject);
begin
  //if chkHiStyleUnderline.Checked then
  //  TH.Highlighters.Attributes[lbHIAttrs.ItemIndex].Style :=
  //    TH.Highlighters.Attributes[lbHIAttrs.ItemIndex].Style + [fsUnderline]
  //else
  //  TH.Highlighters.Attributes[lbHIAttrs.ItemIndex].Style :=
  //    TH.Highlighters.Attributes[lbHIAttrs.ItemIndex].Style - [fsUnderline];
  //UpdateTheme;
end;

procedure TfrmOptionsThemes.bnHiFGSelectColor(Sender: TObject;
  const AColor: TColor);
begin
  //TH.Highlighters.Attributes[lbHIAttrs.ItemIndex].Foreground := AColor;
  //UpdateTheme;
end;

procedure TfrmOptionsThemes.bnHiBGSelectColor(Sender: TObject;
  const AColor: TColor);
begin
  //TH.Highlighters.Attributes[lbHIAttrs.ItemIndex].Background := AColor;
  //UpdateTheme;
end;

procedure TfrmOptionsThemes.SEMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//var
  //B: TBufferCoord;
//  S: string;
  //A: TSynHighlighterAttributes;
begin
  //B := SE.DisplayToBufferPos(SE.PixelsToRowColumn(X, Y));
  //SE.GetHighlighterAttriAtRowCol(B, S, A);
  //if Assigned(A) then
  //  SelectAttribute(lbHiAttrs.Items.IndexOf(A.Name));
end;

procedure TfrmOptionsThemes.FormDestroy(Sender: TObject);
//var
//  i: integer;
begin
  //for i := 0 to cbTheme.Items.Count - 1 do
  //  DeleteFileUTF8(GetTmpThemeFilename(cbTheme.Items[i])); { *Converted from DeleteFile* }
end;

procedure TfrmOptionsThemes.bnThemeDelClick(Sender: TObject);
var
  S: string;
  i: integer;
begin
  i := cbTheme.ItemIndex;
  S := cbTheme.Items[i];
  DeleteFileUTF8(GetTmpThemeFilename(S)); { *Converted from DeleteFile* }
  DeleteFileUTF8(GetThemeFilename(S)); { *Converted from DeleteFile* }
  cbTheme.Items.Delete(i);
  SelectTheme(i - 1);
end;

procedure TfrmOptionsThemes.DoAddThemeCheck(Sender: TForm;
  var AError: boolean; var AErrorStr: string);
//var
//  S: string;
begin
  //AError := True;
  //with Sender as TdlgInput do begin
  //  if cbTheme.Items.IndexOf(Text) <> -1 then
  //    AErrorStr := Format(GetLangStr('SThemeAlreadyExist'), [Text])
  //  else
  //  begin
  //    S := GetTmpThemeFilename(Text);
  //    try
  //      TH.SetDefaultTheme;
  //      TH.Filename := S;
  //      TH.SaveTheme;
  //      AError := False;
  //    except
  //      AErrorStr := GetLangStr('SFileCreateError');
  //    end;
  //  end;
  //end;
end;

procedure TfrmOptionsThemes.bnThemeAddClick(Sender: TObject);
begin
  //with TdlgInput.Create(Self) do
  //try
  //  Name := SThemeDialogName;
  //  Caption := GetLangStr('SAddTheme');
  //  Prompt := GetLangStr('SName');
  //  OnCheckValue := DoAddThemeCheck;
  //  if ShowModal = mrOK then
  //  begin
  //    cbTheme.AddItem(Text, nil);
  //    SelectTheme(cbTheme.Items.Count - 1);
  //  end;
  //finally
  //  Free;
  //end;
end;

procedure TfrmOptionsThemes.InitThemeControls;
begin
  // Main - Font
  //SetMainFont(TH.Font);
  // Main - Colors
//  bnDFG.Selected := TH.Font.Color;
//  bnDBG.Selected := TH.Color;
//  bnDBGSelectColor(bnDBG, bnDBG.Selected);
//
//  // Main - SelColors
//  bnDSelFG.Selected := TH.SelectedColor.Foreground;
//  bnDSelBG.Selected := TH.SelectedColor.Background;
//  // ReadMode
//  spnRMFontSize.Value := TH.ReadFontSize;
//
//  // Othen
//  bnHighlightCurrentLineColor.Selected := TH.ActiveLineColor;
//  chkHighlightCurrentLine.Checked := TH.ActiveLineColor <> clNone;
//  chkHighlightBrackets.Checked := TH.HighlightBracketsColor <> clNone;
//  bnHighlightBracketsColor.Selected := TH.HighlightBracketsColor;
//  spnREdgeAt.Value := TH.RightEdgeAt;
//  bnREdgeColor.Selected := TH.RightEdgeColor;
//
//  // Gutter - Line Num
//  chkGShowNumbers.Checked := TH.Gutter.ShowLineNumbers;
//  chkGAutosize.Checked := TH.Gutter.AutoSize;
//  chkGLeadingZeros.Checked := TH.Gutter.LeadingZeros;
//  chkGZeroStart.Checked := TH.Gutter.ZeroStart;
//  spnGDigitsCount.Value := TH.Gutter.DigitCount;
//
//  // Gutter - Grad
//  chkGUseGrad.Checked := TH.Gutter.Gradient;
//  bnGStartColor.Selected := TH.Gutter.GradientStartColor;
//  bnGEndColor.Selected := TH.Gutter.GradientEndColor;
//  spnGSteps.Value := TH.Gutter.GradientSteps;
//
//  // Gutter - FontColor
//  bnGFontColor.Selected := TH.Gutter.Font.Color;
//  bnGFontColorSelectColor(bnGFontColor, bnGFontColor.Selected);
//
//  // Gutter
//  chkGVisible.Checked := TH.Gutter.Visible;
//  bnGColor.Selected := TH.Gutter.Color;
//  bnBorderColor.Selected := TH.Gutter.BorderColor;
//  spnGWidth.Value := TH.Gutter.Width;
//
//  // Caret
////  cbInsCaret.ItemIndex := integer(TH.InsertCaret);
////  cbOverCaret.ItemIndex := integer(TH.OverwriteCaret);
//
//  // URL
//  chkURL.Checked := TH.URLColor <> clNone;
//  bnURLColor.Selected := TH.URLColor;
//  chkURLBold.Checked := fsBold in TH.URLStyle;
//  chkURLItalic.Checked := fsItalic in TH.URLStyle;
//  chkURLUnderline.Checked := fsUnderline in TH.URLStyle;
////  chkAlwaysShowURL.Checked := TH.AlwaysShowURL;
//
//  chkGUseGradClick(Self);
//  chkGVisibleClick(Self);
//  chkURLClick(Self);
//  chkGShowNumbersClick(Self);
//  chkHighlightCurrentLineClick(Self);
//  chkHighlightBracketsClick(Self);
//
//  SelectHighlighter(cbHighlighter.Items.IndexOf(
//    TH.Highlighters.FindByName(dmCom.FT.Current.Highlighter).LanguageName));
//
//  // Tool Windows
////  dlgTWFont.Font.Assign(TH.ToolFont);
////  pnTWFont.Font.Assign(TH.ToolFont);
////  bnTWBG.Selected := TH.ToolColor;
////  bnTWAL.Selected := TH.ToolActiveLineColor;
////  bnTWBGSelectColor(nil, TH.ToolColor);
//
//  UpdateTheme;
end;

procedure TfrmOptionsThemes.bnHighlightCurrentLineColorSelectColor(
  Sender: TObject; const AColor: TColor);
begin
  chkHighlightCurrentLineClick(Self);
end;

procedure TfrmOptionsThemes.spnREdgeAtChange(Sender: TObject);
begin
  //TH.RightEdgeAt := spnREdgeAt.Value;
  UpdateTheme;
end;

procedure TfrmOptionsThemes.UpdateTheme;
begin
  //TH.Highlighters.UpdateAttributes;
  //TH.SetTheme(SE);
end;

procedure TfrmOptionsThemes.bnREdgeColorSelectColor(Sender: TObject;
  const AColor: TColor);
begin
  //TH.RightEdgeColor := AColor;
  UpdateTheme;
end;

procedure TfrmOptionsThemes.spnRMFontSizeChange(Sender: TObject);
begin
  if not CheckSpinEdit(Sender) then
    Exit;
  //TH.ReadFontSize := spnRMFontSize.Value;
  //SetMainFont(TH.Font);
end;

function TfrmOptionsThemes.CheckSpinEdit(Sender: TObject): boolean;
begin
  with Sender as TSpinEdit do
    Result := (Value >= MinValue) or (Value <= MaxValue);
end;

procedure TfrmOptionsThemes.chkGVisibleClick(Sender: TObject);
begin
  //EnableControls([
  //  bnGColor,
  //  spnGWidth,
  //  chkGShowNumbers,
  //  chkGAutosize,
  //  chkGLeadingZeros,
  //  chkGZeroStart,
  //  spnGDigitsCount,
  //  chkGUseGrad,
  //  bnGStartColor,
  //  bnGEndColor,
  //  spnGSteps,
  //  lblGColor,
  //  lblGWidth,
  //  lblGDigitsCount,
  //  lblGStartColor,
  //  lblGEndColor,
  //  lblGSteps,
  //  lblGFontColor,
  //  bnGFontColor,
  //  lblBorder,
  //  bnBorderColor
  //  ],
  //  chkGVisible.Checked);
  //chkGShowNumbersClick(Self);
  //chkGUseGradClick(Self);
  //TH.Gutter.Visible := chkGVisible.Checked;
  //UpdateTheme;
end;

procedure TfrmOptionsThemes.chkGShowNumbersClick(Sender: TObject);
begin
  //EnableControls([
  //  lblGDigitsCount,
  //  chkGAutosize,
  //  chkGLeadingZeros,
  //  chkGZeroStart,
  //  spnGDigitsCount],
  //  chkGShowNumbers.Checked and chkGVisible.Checked);
  //TH.Gutter.ShowLineNumbers := chkGShowNumbers.Checked;
  UpdateTheme;
end;

procedure TfrmOptionsThemes.chkGUseGradClick(Sender: TObject);
begin
  //EnableControls([
  //  lblGStartColor,
  //  lblGEndColor,
  //  lblGSteps,
  //  bnGStartColor,
  //  bnGEndColor,
  //  spnGSteps],
  //  chkGUseGrad.Checked and chkGVisible.Checked);
  //TH.Gutter.Gradient := chkGUseGrad.Checked;
  UpdateTheme;
end;

procedure TfrmOptionsThemes.bnGColorSelectColor(Sender: TObject;
  const AColor: TColor);
begin
  //TH.Gutter.Color := AColor;
  UpdateTheme;
end;

procedure TfrmOptionsThemes.spnGWidthChange(Sender: TObject);
begin
  //TH.Gutter.Width := spnGWidth.Value;
  UpdateTheme;
end;

procedure TfrmOptionsThemes.chkGAutosizeClick(Sender: TObject);
begin
  //TH.Gutter.AutoSize := chkGAutosize.Checked;
  UpdateTheme;
end;

procedure TfrmOptionsThemes.chkGLeadingZerosClick(Sender: TObject);
begin
  //TH.Gutter.LeadingZeros := chkGLeadingZeros.Checked;
  UpdateTheme;
end;

procedure TfrmOptionsThemes.chkGZeroStartClick(Sender: TObject);
begin
  //TH.Gutter.ZeroStart := chkGZeroStart.Checked;
  UpdateTheme;  
end;

procedure TfrmOptionsThemes.spnGDigitsCountChange(Sender: TObject);
begin
  if not CheckSpinEdit(Sender) then
    Exit;
  //TH.Gutter.DigitCount := spnGDigitsCount.Value;
  UpdateTheme;
end;

procedure TfrmOptionsThemes.bnGStartColorSelectColor(Sender: TObject;
  const AColor: TColor);
begin
  //TH.Gutter.GradientStartColor := AColor;
  UpdateTheme;
end;

procedure TfrmOptionsThemes.bnGEndColorSelectColor(Sender: TObject;
  const AColor: TColor);
begin
  //TH.Gutter.GradientEndColor := AColor;
  UpdateTheme;
end;

procedure TfrmOptionsThemes.spnGStepsChange(Sender: TObject);
begin
  //TH.Gutter.GradientSteps := spnGSteps.Value;
  UpdateTheme;
end;

procedure TfrmOptionsThemes.bnGFontColorSelectColor(Sender: TObject;
  const AColor: TColor);
begin
  //TH.Gutter.Font.Color := AColor;
  UpdateTheme;
end;

procedure TfrmOptionsThemes.chkURLClick(Sender: TObject);
begin
  //EnableControls([
  //  lblURLColor,
  //  bnURLColor,
  //  gbURLStyle,
  //  chkURLBold,
  //  chkURLItalic,
  //  chkURLUnderline,
  //  chkAlwaysShowURL],
  //  chkURL.Checked);
end;

procedure TfrmOptionsThemes.bnURLColorSelectColor(Sender: TObject;
  const AColor: TColor);
begin
  //TH.URLColor := AColor;
end;

procedure TfrmOptionsThemes.chkURLBoldClick(Sender: TObject);
begin
  //if chkURLBold.Checked then
  //  TH.URLStyle := TH.URLStyle + [fsBold]
  //else
  //  TH.URLStyle := TH.URLStyle - [fsBold];
end;

procedure TfrmOptionsThemes.chkURLItalicClick(Sender: TObject);
begin
  //if chkURLItalic.Checked then
  //  TH.URLStyle := TH.URLStyle + [fsItalic]
  //else
  //  TH.URLStyle := TH.URLStyle - [fsItalic];
end;

procedure TfrmOptionsThemes.chkURLUnderlineClick(Sender: TObject);
begin
  //if chkURLUnderline.Checked then
  //  TH.URLStyle := TH.URLStyle + [fsUnderline]
  //else
  //  TH.URLStyle := TH.URLStyle - [fsUnderline];
end;

procedure TfrmOptionsThemes.bnBorderColorSelectColor(Sender: TObject;
  const AColor: TColor);
begin
  //TH.Gutter.BorderColor := AColor;
  //UpdateTheme;
end;

procedure TfrmOptionsThemes.EnableThemeControls(const AEnable: boolean);
begin
  pcTheme.Enabled := AEnable;
  if not AEnable then
  begin
    pnAccess.Align := alClient;
    pnAccess.BringToFront;
    pnAccess.Visible := True;
  end
  else
    pnAccess.Visible := False;
end;

procedure TfrmOptionsThemes.bnTWBGSelectColor(Sender: TObject;
  const AColor: TColor);
begin

end;

procedure TfrmOptionsThemes.FormCreate(Sender: TObject);
begin
// Disable themes for font panels
{$IFDEF COMPILER9_UP}
  pnDFont.ParentBackground := False;
  pnRMFont.ParentBackground := False;
  pnTWFont.ParentBackground := False;
{$ENDIF}
end;

procedure TfrmOptionsThemes.bnChangeFontClick(Sender: TObject);
begin
  if dlgFont.Execute then
    SetMainFont(dlgFont.Font);
end;

procedure TfrmOptionsThemes.bnTWFontClick(Sender: TObject);
begin

end;

end.
