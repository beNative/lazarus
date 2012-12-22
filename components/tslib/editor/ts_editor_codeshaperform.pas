{
  Copyright (C) 2012 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts_Editor_CodeShaperForm;

{$mode delphi}

{
  TODO:
    - Work in progress...
    - CodeShaper profiles (collection to save with settings)
}

//*****************************************************************************

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, ActnList, Grids, Buttons, Menus,

  LCLType, FileUtil, LResources,

  SynEdit, SynEditTypes,

  MenuButton,

  ts_Editor_Interfaces;

type
  TTokenSide = (
    tsBefore,
    tsAfter,
    tsBoth
  );

type
  TfrmCodeShaper = class(TForm, IEditorToolView, IClipboardCommands)
    {$region 'designer controls' /fold}
    aclMain                         : TActionList;
    actInsertBreaks                 : TAction;
    actAlignLines                   : TAction;
    actAlignInsertSpaceAfterToken   : TAction;
    actAlignInsertSpaceBeforeToken  : TAction;
    actAlignInsertSpaceOnBothSides  : TAction;
    actDequoteLines                 : TAction;
    actApplyConsistentCase          : TAction;
    actURLDecode                    : TAction;
    actURLEncode                    : TAction;
    actRemoveDoubles                : TAction;
    actStripLastChar                : TAction;
    actStripFirstChar               : TAction;
    actMakePascalString             : TAction;
    actToggleBreakSide              : TAction;
    actToggleAlignInsertSpace       : TAction;
    actBreakBeforeToken             : TAction;
    actBreakAfterToken              : TAction;
    actRevert                       : TAction;
    actQuoteLines                   : TAction;
    actReplace                      : TAction;
    actTrim                         : TAction;
    actRedo                         : TAction;
    actRemoveBreaks                 : TAction;
    actUndo                         : TAction;
    btnAlign                        : TBitBtn;
    btnAlign1                       : TBitBtn;
    btnAlign2                       : TBitBtn;
    btnAlign3                       : TBitBtn;
    btnAlignInsertSpace             : TMenuButton;
    btnAlignInsertSpace1            : TMenuButton;
    btnBreakAfterBefore             : TMenuButton;
    btnInsertBreaks                 : TBitBtn;
    btnPascalString                 : TButton;
    btnRemoveBreaks                 : TBitBtn;
    btnRevert                       : TButton;
    btnStripFirstChar               : TButton;
    btnStripLastChar                : TButton;
    btnStripLastChar1               : TButton;
    btnStripLastChar2               : TButton;
    btnTrim                         : TBitBtn;
    btnURLDecode                    : TButton;
    btnURLEncode                    : TButton;
    chkAlignLinesInParagraphs       : TCheckBox;
    chkAlignLinesInsertSpace        : TCheckBox;
    chkAlignLinesRemoveWhitespace   : TCheckBox;
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
    edtAlignLinesToken              : TLabeledEdit;
    edtBreakLinesToken              : TLabeledEdit;
    edtBreakLinesWrapPosition       : TSpinEdit;
    edtDequoteLinesQuoteChar        : TLabeledEdit;
    edtQuotedLinesDelimiter         : TLabeledEdit;
    edtQuoteLinesQuoteChar          : TLabeledEdit;
    edtTrimLinesIndent              : TSpinEdit;
    edtUnBreakLinesWrapPosition     : TSpinEdit;
    grdReplaceStrings               : TStringGrid;
    grpAlign                        : TGroupBox;
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
    MenuItem1                       : TMenuItem;
    MenuItem2                       : TMenuItem;
    mniAlignInsertSpaceAfterToken   : TMenuItem;
    mniAlignInsertSpaceBeforeToken  : TMenuItem;
    mniAlignInsertSpaceOnBothSides  : TMenuItem;
    pnlOperations                   : TPanel;
    ppmAfterBefore                  : TPopupMenu;
    ppmSpaces                       : TPopupMenu;
    sbrMain                         : TScrollBox;
    {$endregion}

    {$region 'action handlers' /fold}
    procedure actAlignInsertSpaceAfterTokenExecute(Sender: TObject);
    procedure actAlignInsertSpaceBeforeTokenExecute(Sender: TObject);
    procedure actAlignInsertSpaceOnBothSidesExecute(Sender: TObject);
    procedure actAlignLinesExecute(Sender: TObject);
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
    procedure actToggleAlignInsertSpaceExecute(Sender: TObject);
    procedure actToggleBreakSideExecute(Sender: TObject);
    procedure actTrimExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actURLDecodeExecute(Sender: TObject);
    procedure actURLEncodeExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    {$endregion}

  private
    FAlignTokenSide : TTokenSide;
    FBreakTokenSide : TTokenSide;

    function GetForm: TForm;
    function GetName: string;
    function GetText: string;
    function GetView: IEditorView;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

  protected
    { IClipboardCommands }
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;
    procedure Redo;

    procedure AssignText(const AText: string; ALockUpdates: Boolean = True);
    procedure InitializeControls;
    procedure InitializeAlignLinesControls;
    procedure InitializeBreakLinesControls;
    procedure InitializeQuoteLinesControls;
    procedure UpdateAlignLinesControls;
    procedure UpdateBreakLinesControls;
    procedure UpdateReplaceStringsControls;

    procedure ReplaceStrings;
    function ReplaceString(const ASource  : string;
                           const AStrFrom : string;
                           const AStrTo   : string;
                                 AOptions : TSynSearchOptions): string;
    procedure UpdateQuoteLinesControls;
    procedure UpdateActions; override;

    { IEditorToolView }
    procedure UpdateView;

    procedure BeginUpdate;
    procedure EndUpdate;

    property View: IEditorView
      read GetView;

    property Visible: Boolean
      read GetVisible write SetVisible;

    property Name: string
      read GetName;

    property Form: TForm
      read GetForm;

    property Text: string
      read GetText;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

//*****************************************************************************

implementation

{$R *.lfm}

uses
  Windows,

  ts_Core_Utils,

  ts_Editor_Utils;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmCodeShaper.AfterConstruction;
begin
  inherited AfterConstruction;
end;

procedure TfrmCodeShaper.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TfrmCodeShaper.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

procedure TfrmCodeShaper.SetVisible(AValue: Boolean);
begin
  inherited SetVisible(AValue);
end;

function TfrmCodeShaper.GetName: string;
begin
  Result := inherited Name;
end;

function TfrmCodeShaper.GetText: string;
begin
  if View.SelAvail then
    Result := View.SelText
  else
    Result := View.Text;
end;

function TfrmCodeShaper.GetForm: TForm;
begin
  Result := Self;
end;

function TfrmCodeShaper.GetView: IEditorView;
begin
  Result := Owner as IEditorView;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************

//*****************************************************************************
// action handlers                                                       BEGIN
//*****************************************************************************

procedure TfrmCodeShaper.actRedoExecute(Sender: TObject);
begin
  View.Redo;
end;

procedure TfrmCodeShaper.actRemoveBreaksExecute(Sender: TObject);
var
  S: string;
begin
  S := Text;
  S := UnBreakLines(S);
  if chkUnBreakLinesWrap.Checked then
    S := WrapText(S, edtUnBreakLinesWrapPosition.Value);
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

procedure TfrmCodeShaper.actToggleAlignInsertSpaceExecute(Sender: TObject);
begin
  FAlignTokenSide := TTokenSide((Ord(FAlignTokenSide) + 1) mod 3);
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

procedure TfrmCodeShaper.actAlignLinesExecute(Sender: TObject);
var
  S: string;
begin
  S := Text;
  S := AlignLines(
    S,
    edtAlignLinesToken.Text,
    chkAlignLinesRemoveWhitespace.Checked,
    chkAlignLinesInsertSpace.Checked and (FAlignTokenSide in [tsBefore, tsBoth]),
    chkAlignLinesInsertSpace.Checked and (FAlignTokenSide in [tsAfter, tsBoth]),
    chkAlignLinesInParagraphs.Checked
  );
  AssignText(S);
end;

procedure TfrmCodeShaper.actApplyConsistentCaseExecute(Sender: TObject);
var
  S: string;
  W: string;
begin
  S := Text;
  W := View.GetWordAtPosition(View.LogicalCaretXY);
  S := ReplaceString(S, W, W, [ssoWholeWord, ssoEntireScope, ssoReplaceAll]);
  AssignText(S, False);
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

procedure TfrmCodeShaper.actAlignInsertSpaceAfterTokenExecute(Sender: TObject);
begin
  FAlignTokenSide := tsAfter;
end;

procedure TfrmCodeShaper.actAlignInsertSpaceBeforeTokenExecute(Sender: TObject);
begin
  FAlignTokenSide := tsBefore;
end;

procedure TfrmCodeShaper.actAlignInsertSpaceOnBothSidesExecute(Sender: TObject);
begin
  FAlignTokenSide := tsBoth;
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
    S := WrapText(S, edtBreakLinesWrapPosition.Value);
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
  S: string;
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
  S: string;
begin
  S := URLDecode(Text);
  AssignText(S);
end;

procedure TfrmCodeShaper.actURLEncodeExecute(Sender: TObject);
var
  S: string;
begin
  S := URLEncode(Text);
  AssignText(S);
end;

procedure TfrmCodeShaper.Button1Click(Sender: TObject);
begin
  BeginUpdate;
end;

procedure TfrmCodeShaper.Button2Click(Sender: TObject);
begin
  EndUpdate;
end;

//*****************************************************************************
// action handlers                                                         END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

{ Updates the editor with the given text with undo/redo support. }

procedure TfrmCodeShaper.AssignText(const AText: string;
  ALockUpdates: Boolean);
var
  N: Integer;
begin
  if ALockUpdates then
    BeginUpdate;
  if View.Editor.SelAvail then
    View.Editor.TextBetweenPointsEx[View.BlockBegin, View.BlockEnd, scamEnd]
      := AText
  else
    View.Text := AText;

  N := View.Lines.Count - 1;
  // delete last empty line if added by TextBetweenPointsEx
  if View.Lines[N] = '' then
    View.Lines.Delete(N);
  if ALockUpdates then
    EndUpdate;
end;

procedure TfrmCodeShaper.InitializeAlignLinesControls;
begin
  edtAlignLinesToken.Text := ':';
  chkAlignLinesRemoveWhitespace.Checked := True;
  chkAlignLinesInsertSpace.Checked := True;
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

procedure TfrmCodeShaper.UpdateAlignLinesControls;
var
  B: Boolean;
begin
  B := True;
  edtAlignLinesToken.Enabled := B;
  chkAlignLinesRemoveWhitespace.Enabled := B;
  chkAlignLinesInsertSpace.Enabled := B;
  chkAlignLinesInParagraphs.Enabled := B;
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
  if B then
  begin
    //grdReplaceStrings.Columns[0].Title.Font.Color := clBlack;
    //grdReplaceStrings.Columns[1].Title.Font.Color := clBlack;
    //grdReplaceStrings.Color := clNone;
  end
  else
  begin
    //grdReplaceStrings.Columns[0].Title.Font.Color := clDisabledButtonText;
    //grdReplaceStrings.Columns[1].Title.Font.Color := clDisabledButtonText;
    //grdReplaceStrings.Color := clDisabledBackground;

  end;
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
  View.BeginUpdate;
end;

procedure TfrmCodeShaper.EndUpdate;
begin
  View.EndUpdate;
end;

procedure TfrmCodeShaper.UpdateView;
begin
  //
end;

procedure TfrmCodeShaper.InitializeControls;
begin
  InitializeBreakLinesControls;
  InitializeAlignLinesControls;
  InitializeQuoteLinesControls;
end;

procedure TfrmCodeShaper.UpdateActions;
var
  A : TAction;
begin
  inherited UpdateActions;
  actUndo.Enabled := View.CanUndo;
  actRedo.Enabled := View.CanRedo;

  edtTrimLinesIndent.Enabled :=
    chkTrimLinesRight.Checked or chkTrimLinesLeft.Checked;

  case FAlignTokenSide of
    tsBoth: A := actAlignInsertSpaceOnBothSides;
    tsBefore: A := actAlignInsertSpaceBeforeToken;
    tsAfter: A := actAlignInsertSpaceAfterToken;
  end;
  A.Checked := True;
  actToggleAlignInsertSpace.Caption := A.Caption;

  case FBreakTokenSide of
    tsBefore: A := actBreakBeforeToken;
    tsAfter:  A := actBreakAfterToken;
  end;
  A.Checked := True;
  actToggleBreakSide.Caption := A.Caption;
end;

procedure TfrmCodeShaper.Cut;
begin
  PostMessage(GetFocus, WM_CUT, 0, 0);
end;

procedure TfrmCodeShaper.Copy;
begin
  PostMessage(GetFocus, WM_COPY, 0, 0);
end;

procedure TfrmCodeShaper.Paste;
begin
  PostMessage(GetFocus, WM_PASTE, 0, 0);
end;

procedure TfrmCodeShaper.Undo;
begin
  PostMessage(GetFocus, WM_UNDO, 0, 0);
end;

procedure TfrmCodeShaper.Redo;
begin
  PostMessage(GetFocus, WM_UNDO, 1, 0);
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

end.


