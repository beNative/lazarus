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

unit ts_Editor_CodeShaper_ToolView;

{$MODE Delphi}

{
  TODO:
    - Work in progress...
    - CodeShaper profiles (collection to save with settings)

    - Hint of each button should describe the operation reflecting the choosen
      settings.

    - Use IEditorCommands
}

interface

uses
  SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Spin, ActnList, Grids, Buttons,
  Menus, Classes,

  SynEditTypes,

  ts.Editor.Interfaces, ts.Editor.CodeShaper.Settings, ts_Editor_ToolView_Base;

type
  TTokenSide = (
    tsBefore,
    tsAfter,
    tsBoth
  );

type

  { TfrmCodeShaper }

  TfrmCodeShaper = class(TCustomEditorToolView, IEditorToolView)
    {$region 'designer controls' /fold}
    aclMain                         : TActionList;
    actInsertBreaks                 : TAction;
    actDequoteLines                 : TAction;
    actApplyConsistentCase          : TAction;
    actURLDecode                    : TAction;
    actURLEncode                    : TAction;
    actRemoveDoubles                : TAction;
    actStripLastChar                : TAction;
    actStripFirstChar               : TAction;
    actMakePascalString             : TAction;
    actToggleBreakSide              : TAction;
    actBreakBeforeToken             : TAction;
    actBreakAfterToken              : TAction;
    actQuoteLines                   : TAction;
    actReplace                      : TAction;
    actTrim                         : TAction;
    actRedo                         : TAction;
    actRemoveBreaks                 : TAction;
    actUndo                         : TAction;
    btnAlign1                       : TBitBtn;
    btnDequote                       : TBitBtn;
    btnInsertBreaks                 : TBitBtn;
    btnPascalString                 : TButton;
    btnRemoveBreaks                 : TBitBtn;
    btnReplace: TBitBtn;
    btnStripFirstChar               : TButton;
    btnStripLastChar                : TButton;
    btnStripLastChar1               : TButton;
    btnStripLastChar2               : TButton;
    btnTrim                         : TBitBtn;
    btnURLDecode                    : TButton;
    btnURLEncode                    : TButton;
    chkBreakLines                   : TCheckBox;
    chkBreakLinesTrimSpace          : TCheckBox;
    chkBreakLinesWrap               : TCheckBox;
    chkQuoteLinesDelimitLines       : TCheckBox;
    chkQuoteLinesTrimSpace          : TCheckBox;
    chkDeQuoteLinesTrimSpace        : TCheckBox;
    chkReplaceStringsCaseSensitive  : TCheckBox;
    chkReplaceStringsWholeWordsOnly : TCheckBox;
    chkTrimCompressSpace            : TCheckBox;
    chkTrimLinesLeft                : TCheckBox;
    chkTrimLinesRight               : TCheckBox;
    chkUnBreakLinesWrap             : TCheckBox;
    cbxInsertBreaksTokenSide: TComboBox;
    edtBreakLinesToken              : TLabeledEdit;
    edtBreakLinesWrapPosition       : TSpinEdit;
    edtDequoteLinesQuoteChar        : TLabeledEdit;
    edtQuotedLinesDelimiter         : TLabeledEdit;
    edtQuoteLinesQuoteChar          : TLabeledEdit;
    edtTrimLinesIndent              : TSpinEdit;
    edtUnBreakLinesWrapPosition     : TSpinEdit;
    grdReplaceStrings               : TStringGrid;
    grpDequoteLines                 : TGroupBox;
    grpInsertBreaks                 : TGroupBox;
    grpMisc                         : TGroupBox;
    grpQuoteLines                   : TGroupBox;
    grpRemoveBreaks                 : TGroupBox;
    grpReplaceStrings               : TGroupBox;
    grpTrim                         : TGroupBox;
    imlMain                         : TImageList;
    lblTrimLinesIndent              : TLabel;
    lblWrapLinesPosition            : TLabel;
    lblWrapLinesPosition1           : TLabel;
    mniBreakAfterToken                       : TMenuItem;
    mniBreakBeforeToken                       : TMenuItem;
    pnlOperations                   : TPanel;
    ppmAfterBefore                  : TPopupMenu;
    sbrMain                         : TScrollBox;
    {$endregion}

    {$region 'action handlers' /fold}
    procedure actApplyConsistentCaseExecute(Sender: TObject);
    procedure actBreakAfterTokenExecute(Sender: TObject);
    procedure actBreakBeforeTokenExecute(Sender: TObject);
    procedure actDequoteLinesExecute(Sender: TObject);
    procedure actInsertBreaksExecute(Sender: TObject);
    procedure actMakePascalStringExecute(Sender: TObject);
    procedure actQuoteLinesExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actRemoveBreaksExecute(Sender: TObject);
    procedure actRemoveDoublesExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actStripFirstCharExecute(Sender: TObject);
    procedure actStripLastCharExecute(Sender: TObject);
    procedure actToggleBreakSideExecute(Sender: TObject);
    procedure actTrimExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actURLDecodeExecute(Sender: TObject);
    procedure actURLEncodeExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    {$endregion}

  strict private
    FBreakTokenSide : TTokenSide;

    function GetSettings: TCodeShaperSettings;
    function GetText: string;

  protected
    procedure AssignText(const AText: string);
    procedure InitializeControls;
    procedure InitializeBreakLinesControls;
    procedure InitializeQuoteLinesControls;
    procedure UpdateBreakLinesControls;
    procedure UpdateReplaceStringsControls;

    procedure ReplaceStrings;
    function ReplaceString(
      const ASource  : string;
      const AStrFrom : string;
      const AStrTo   : string;
            AOptions : TSynSearchOptions
    ): string;
    procedure UpdateQuoteLinesControls;
    procedure UpdateActions; override;
    procedure UpdateView; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    property Text: string
      read GetText;

    property Settings: TCodeShaperSettings
      read GetSettings;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.lfm}

uses
  LCLIntf,

  ts.Core.Utils,

  ts.Editor.Utils;

{$region 'construction and destruction' /fold}
procedure TfrmCodeShaper.AfterConstruction;
begin
  inherited AfterConstruction;
  Width := Settings.Width;
end;

procedure TfrmCodeShaper.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'property access mehods' /fold}
function TfrmCodeShaper.GetText: string;
begin
  if View.SelAvail then
    Result := View.SelText
  else
    Result := View.Text;
end;

function TfrmCodeShaper.GetSettings: TCodeShaperSettings;
begin
  Result := inherited Settings
    .ToolSettings.ItemsByClass[TCodeShaperSettings] as TCodeShaperSettings;
end;
{$endregion}

{$region 'event handlers' /fold}
procedure TfrmCodeShaper.FormResize(Sender: TObject);
begin
  Settings.Width := Width;
end;
{$endregion}

{$region 'action handlers' /fold}
procedure TfrmCodeShaper.actRedoExecute(Sender: TObject);
begin
  View.Redo;
end;

procedure TfrmCodeShaper.actRemoveBreaksExecute(Sender: TObject);
var
  S: string;
begin
  S := Text;
  S := JoinLines(S);
  if chkUnBreakLinesWrap.Checked then
    S := ts.Editor.Utils.WrapText(S, edtUnBreakLinesWrapPosition.Value);
  AssignText(S);
end;

procedure TfrmCodeShaper.actRemoveDoublesExecute(Sender: TObject);
var
  S: string;
begin
  S := Text;
  S := RemoveDoubles(S);
  AssignText(S);
end;

procedure TfrmCodeShaper.actReplaceExecute(Sender: TObject);
begin
  ReplaceStrings;
end;

procedure TfrmCodeShaper.actStripFirstCharExecute(Sender: TObject);
var
  S: string;
begin
  S := Text;
  S := StripChars(S, True, False);
  AssignText(S);
end;

procedure TfrmCodeShaper.actStripLastCharExecute(Sender: TObject);
var
  S: string;
begin
  S := Text;
  S := StripChars(S, False, False);
  AssignText(S);
end;

procedure TfrmCodeShaper.actToggleBreakSideExecute(Sender: TObject);
begin
  FBreakTokenSide := TTokenSide((Ord(FBreakTokenSide) + 1) mod 2);
end;

procedure TfrmCodeShaper.actTrimExecute(Sender: TObject);
var
  S: string;
begin
  S := Text;
  if chkTrimLinesLeft.Checked or chkTrimLinesRight.Checked then
  begin
    S := TrimLines(
      S,
      chkTrimLinesLeft.Checked,
      chkTrimLinesRight.Checked,
      edtTrimLinesIndent.Value
    );
  end;
  if chkTrimCompressSpace.Checked then
  begin
    S := CompressLines(S);
  end;
  AssignText(S);
end;

procedure TfrmCodeShaper.actApplyConsistentCaseExecute(Sender: TObject);
var
  S : string;
  W : string;
begin
  S := Text;
  W := View.GetWordAtPosition(View.LogicalCaretXY);
  S := ReplaceString(S, W, W, [ssoWholeWord, ssoEntireScope, ssoReplaceAll]);
  AssignText(S);
end;

procedure TfrmCodeShaper.actBreakAfterTokenExecute(Sender: TObject);
begin
  FBreakTokenSide := tsAfter;
end;

procedure TfrmCodeShaper.actBreakBeforeTokenExecute(Sender: TObject);
begin
  FBreakTokenSide := tsBefore;
end;

procedure TfrmCodeShaper.actDequoteLinesExecute(Sender: TObject);
var
  S: string;
begin
  S := Text;
  S := DequoteLines(
    S,
    edtDequoteLinesQuoteChar.Text[1],
    chkDeQuoteLinesTrimSpace.Checked
  );
  AssignText(S);
end;

procedure TfrmCodeShaper.actInsertBreaksExecute(Sender: TObject);
var
  S: string;
begin
  S := Text;
  if chkBreakLines.Checked then
  begin
    S := BreakLines(
      S,
      edtBreakLinesToken.Text,
      actBreakBeforeToken.Checked,
      0,//edtTrimLinesIndent.Value,
      chkBreakLinesTrimSpace.Checked
    );
  end;
  if chkBreakLinesWrap.Checked then
  begin
    S := ts.Editor.Utils.WrapText(S, edtBreakLinesWrapPosition.Value);
  end;
  AssignText(S);
end;

procedure TfrmCodeShaper.actMakePascalStringExecute(Sender: TObject);
var
  S: string;
begin
  S := Text;
  S := PascalStringOf(S);
  AssignText(S);
end;

procedure TfrmCodeShaper.actQuoteLinesExecute(Sender: TObject);
var
  S : string;
begin
  S := Text;
  if not View.SelAvail then
  begin

  end;
  if chkQuoteLinesDelimitLines.Checked then
  begin
    S := QuoteLinesAndDelimit(
      S,
      edtQuoteLinesQuoteChar.Text[1],
      edtQuotedLinesDelimiter.Text,
      chkQuoteLinesTrimSpace.Checked
    );
  end
  else
  begin
    S := QuoteLines(
      S,
      edtQuoteLinesQuoteChar.Text[1],
      chkQuoteLinesTrimSpace.Checked
    );
  end;
  AssignText(S);
end;

procedure TfrmCodeShaper.actUndoExecute(Sender: TObject);
begin
  View.Undo;
end;

procedure TfrmCodeShaper.actURLDecodeExecute(Sender: TObject);
var
  S : string;
begin
  S := URLDecode(Text);
  AssignText(S);
end;

procedure TfrmCodeShaper.actURLEncodeExecute(Sender: TObject);
var
  S : string;
begin
  S := URLEncode(Text);
  AssignText(S);
end;
{$endregion}

{$region 'protected methods' /fold}
{ Updates the editor with the given text with undo/redo support. }

procedure TfrmCodeShaper.AssignText(const AText: string);
begin
  BeginUpdate;
  if View.Editor.SelAvail then
    View.Selection.Text := AText
  else
    View.Selection.Text := AText;
  EndUpdate;
end;

procedure TfrmCodeShaper.InitializeBreakLinesControls;
begin
  chkBreakLines.Checked := False;
  edtBreakLinesToken.Text := ',';
  edtTrimLinesIndent.Value := 2;
  chkBreakLinesTrimSpace.Checked := False;
end;

procedure TfrmCodeShaper.InitializeQuoteLinesControls;
begin
  chkQuoteLinesTrimSpace.Checked := False;
  chkQuoteLinesDelimitLines.Checked := False;
  edtQuotedLinesDelimiter.Text := ',';
  edtQuoteLinesQuoteChar.Text := '''';
end;

procedure TfrmCodeShaper.UpdateBreakLinesControls;
var
  B : Boolean;
begin
  B := chkBreakLines.Checked;
  chkBreakLinesTrimSpace.Enabled := B;
  lblTrimLinesIndent.Enabled := B;
  edtTrimLinesIndent.Enabled := B;
  chkBreakLines.Enabled := B;
  edtBreakLinesToken.Enabled := B and chkBreakLines.Checked;
  chkBreakLinesWrap.Enabled := B;
  edtBreakLinesWrapPosition.Enabled := B and chkBreakLinesWrap.Checked;
  lblWrapLinesPosition.Enabled := B and chkBreakLinesWrap.Checked;
end;

procedure TfrmCodeShaper.UpdateReplaceStringsControls;
var
  B: Boolean;
begin
  B := True;
  chkReplaceStringsCaseSensitive.Enabled := B;
  chkReplaceStringsWholeWordsOnly.Enabled := B;
  grdReplaceStrings.Enabled := B;
end;

procedure TfrmCodeShaper.ReplaceStrings;
var
  I  : Integer;
  S  : string;
  S1 : string;
  S2 : string;
  O  : TSynSearchOptions;
begin
  O := [ssoEntireScope, ssoReplaceAll];
  if chkReplaceStringsCaseSensitive.Checked then
    Include(O, ssoMatchCase);
  if chkReplaceStringsWholeWordsOnly.Checked then
    Include(O, ssoWholeWord);
  S := Text;
  I := 1;
  S2 := grdReplaceStrings.Cells[1, I];
  while (I < grdReplaceStrings.RowCount) and (S2 <> '') do
  begin
    S1 := grdReplaceStrings.Cells[0, I];
    if S1 <> '' then
      S := ReplaceString(S, S1, S2, O);
    Inc(I);
    S2 := grdReplaceStrings.Cells[1, I];
  end;
  AssignText(S);
end;

function TfrmCodeShaper.ReplaceString(const ASource: string;
  const AStrFrom: string; const AStrTo: string; AOptions: TSynSearchOptions): string;
begin
  View.Editor.SearchReplace(AStrFrom, AStrTo, AOptions);
  Result := Text;
end;

procedure TfrmCodeShaper.UpdateQuoteLinesControls;
var
  B: Boolean;
begin
  B := True;
  edtQuoteLinesQuoteChar.Enabled := B;
  chkQuoteLinesDelimitLines.Enabled := B;
  edtQuotedLinesDelimiter.Enabled := B and chkQuoteLinesDelimitLines.Checked;
  chkQuoteLinesTrimSpace.Enabled := B;
end;

procedure TfrmCodeShaper.BeginUpdate;
begin
  View.Selection.Store(True, True);
end;

procedure TfrmCodeShaper.EndUpdate;
begin
  View.Selection.Restore;
end;

procedure TfrmCodeShaper.UpdateView;
begin
  //
end;

procedure TfrmCodeShaper.InitializeControls;
begin
  InitializeBreakLinesControls;
  InitializeQuoteLinesControls;
end;

procedure TfrmCodeShaper.UpdateActions;
var
  A : TAction;
  M : TMenuItem;
begin
  inherited UpdateActions;
  actUndo.Enabled := View.CanUndo;
  actRedo.Enabled := View.CanRedo;

  edtTrimLinesIndent.Enabled :=
    chkTrimLinesRight.Checked or chkTrimLinesLeft.Checked;

  case FBreakTokenSide of
    tsBefore: M := mniBreakBeforeToken;
    tsAfter:  M := mniBreakAfterToken;
  end;
  A := M.Action as TAction;
  A.Checked := True;
end;
{$endregion}

end.


