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

unit ts_Editor_SearchForm;

{ Searchform to use in combination with an EditorManager instance. This toolform
  provides search/replace functionality for the active view or all views that
  are managed by the EditorManager. }


{
  TODO: copy all matches to a stringlist when regular expressions are used
}

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, Buttons, ExtCtrls, Dialogs,
  Graphics, Grids, ComCtrls, ActnList, StdActns,

  LCLProc, LCLType, LResources,

  SynEditTypes, SynRegExpr, SynEdit,

  VirtualTrees,

  ts_Core_TreeViewPresenter, ts_Core_ColumnDefinitionsDataTemplate,

  ts_Editor_Interfaces;

//=============================================================================

type
  TfrmSearchForm = class(TForm, IEditorToolView, IClipboardCommands)
    {$region 'designer controls' /fold}
    btnNext                         : TBitBtn;
    btnPrevious                     : TBitBtn;
    btnFind                         : TBitBtn;
    btnReplace                      : TBitBtn;
    btnReplaceAll                   : TBitBtn;
    chkCaseSensitive                : TCheckBox;
    chkMultiLine                    : TCheckBox;
    chkRegularExpressions           : TCheckBox;
    chkSearchAllViews               : TCheckBox;
    edtSearchText                   : TEdit;
    pnlButtons                      : TPanel;
    pnlResultList                   : TPanel;
    rbtBackward                     : TRadioButton;
    chkReplaceStringsCaseSensitive  : TCheckBox;
    chkReplaceStringsWholeWordsOnly : TCheckBox;
    DirectionGroupBox               : TGroupBox;
    rbtEntireScope                  : TRadioButton;
    rbtForward                      : TRadioButton;
    rbtFromCursor                   : TRadioButton;
    rbtGlobal                       : TRadioButton;
    grdReplaceStrings               : TStringGrid;
    grpOrigin                       : TGroupBox;
    grpDirection                    : TGroupBox;
    grpSearchText                   : TGroupBox;
    grpMisc                         : TGroupBox;
    grpScope                        : TGroupBox;
    grpReplaceWith                  : TGroupBox;
    grpOptions                      : TGroupBox;
    pnlOperations                   : TPanel;
    rbtSelected                     : TRadioButton;
    cbxReplaceWith                  : TComboBox;
    chkReplaceWith                  : TCheckBox;
    cbxSearchText                   : TComboBox;
    lblSearchText                   : TLabel;
    chkWholeWordsOnly               : TCheckBox;
    sbrMain                         : TStatusBar;
    {$endregion}

    procedure btnFindClick(Sender: TObject);
    procedure btnReplaceAllClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure cbxSearchTextChange(Sender: TObject);
    procedure chkCaseSensitiveClick(Sender: TObject);
    procedure chkMultiLineClick(Sender: TObject);
    procedure chkRegularExpressionsChange(Sender: TObject);
    procedure chkReplaceWithChange(Sender: TObject);
    procedure chkSearchAllViewsChange(Sender: TObject);
    procedure chkWholeWordsOnlyClick(Sender: TObject);
    procedure DoOnSelectionChanged(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    FTVP      : TTreeViewPresenter;
    FVST      : TVirtualStringTree;
    FUpdate   : Boolean;
    FCaretPos : Integer;

    function GetForm: TForm;
    function GetManager: IEditorManager;
    function GetName: string;
    function GetSearchEngine: IEditorSearchEngine;
    procedure SetOptions(AValue: TSynSearchOptions);
    function GetOptions: TSynSearchOptions;
    function GetSearchText: string;
    procedure SetSearchText(const AValue: string);
    function GetReplaceText: string;
    procedure SetReplaceText(const AValue: string);
    procedure UpdateView;

  protected
    procedure Execute;
    procedure Modified;

    { IClipboardCommands }
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;
    procedure Redo;

    property SearchEngine: IEditorSearchEngine
      read GetSearchEngine;

    property Manager: IEditorManager
      read GetManager;

    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    property Visible: Boolean
      read GetVisible write SetVisible;

    property Name: string
      read GetName;

    property Form: TForm
      read GetForm;

  public
    procedure AfterConstruction; override;

    procedure UpdateActions; override;

    property Options : TSynSearchOptions
      read GetOptions write SetOptions;

    property SearchText : string
      read GetSearchText write SetSearchText;

    property ReplaceText : string
      read GetReplaceText write SetReplaceText;
  end;

//*****************************************************************************

implementation

uses
  Messages, Windows,

  ts_Core_ColumnDefinitions, ts_Core_Helpers, ts_Core_ComponentInspector,

  ts_Editor_SearchEngine, ts_Editor_Utils;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmSearchForm.AfterConstruction;
begin
  inherited AfterConstruction;
  FVST := CreateVST(Self, pnlResultList);
  //FTVP := CreateTVP();   // TODO
  FTVP := TTreeViewPresenter.Create(Self);
  FTVP.MultiSelect := False;
  FTVP.ColumnDefinitions.AddColumn('Index', '#', dtNumeric, 40, 20, 60);
  FTVP.ColumnDefinitions.AddColumn('FileName', dtString, 160, 120, 400);
  FTVP.ColumnDefinitions.AddColumn('Column', dtNumeric, 60, 60, 80);
  FTVP.ColumnDefinitions.AddColumn('Line', dtNumeric, 40, 40, 80);
  FTVP.ItemsSource := SearchEngine.ItemList;
  FTVP.TreeView := FVST;
  FTVP.OnSelectionChanged := DoOnSelectionChanged;
  btnNext.Action      := Manager.Actions['actFindNext'];
  btnPrevious.Action  := Manager.Actions['actFindPrevious'];
  btnNext.Caption     := '';
  btnPrevious.Caption := '';
  cbxSearchText.Text  := '';
  cbxReplaceWith.Text := '';
  Modified;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TfrmSearchForm.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TfrmSearchForm.GetName: string;
begin
  Result := inherited Name;
end;

function TfrmSearchForm.GetSearchEngine: IEditorSearchEngine;
begin
  Result := Owner as IEditorSearchEngine;
end;

procedure TfrmSearchForm.SetOptions(AValue: TSynSearchOptions);
begin
  chkCaseSensitive.Checked      := ssoMatchCase in AValue;
  chkWholeWordsOnly.Checked     := ssoWholeWord in AValue;
  chkRegularExpressions.Checked := ssoRegExpr in AValue;
  chkMultiLine.Checked          := ssoRegExprMultiLine in AValue;

  if ssoEntireScope in AValue then
    rbtEntireScope.Checked := True
  else
    rbtFromCursor.Checked  := True;
  if ssoSelectedOnly in AValue then
    rbtSelected.Checked := True
  else
    rbtGlobal.Checked   := True;
  if ssoBackwards in AValue then
    rbtBackward.Checked := True
  else
    rbtForward.Checked  := True;

  Modified;
end;

function TfrmSearchForm.GetOptions: TSynSearchOptions;
begin
  Result := [];
  if chkCaseSensitive.Checked then
    Include(Result, ssoMatchCase);
  if chkWholeWordsOnly.Checked then
    Include(Result, ssoWholeWord);
  if chkRegularExpressions.Checked then
    Include(Result, ssoRegExpr);
  if chkMultiLine.Checked then
    Include(Result, ssoRegExprMultiLine);
  if rbtEntireScope.Checked then
    Include(Result, ssoEntireScope);
  if rbtSelected.Checked then
    Include(Result, ssoSelectedOnly);
  if rbtBackward.Checked then
    Include(Result, ssoBackwards);
end;

function TfrmSearchForm.GetSearchText: string;
begin
  Result := cbxSearchText.Text;
end;

procedure TfrmSearchForm.SetSearchText(const AValue: string);
begin
  cbxSearchText.Text := AValue;
  cbxSearchText.SelectAll;
end;

function TfrmSearchForm.GetReplaceText: string;
begin
  Result := cbxReplaceWith.Text;
end;

procedure TfrmSearchForm.SetReplaceText(const AValue: string);
begin
  cbxReplaceWith.Text := AValue;
end;

function TfrmSearchForm.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

procedure TfrmSearchForm.SetVisible(AValue: Boolean);
begin
  inherited SetVisible(AValue);
end;

function TfrmSearchForm.GetForm: TForm;
begin
  Result := Self;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

procedure TfrmSearchForm.btnFindClick(Sender: TObject);
begin
  Execute;
end;

procedure TfrmSearchForm.btnReplaceAllClick(Sender: TObject);
begin
  SearchEngine.ReplaceAll;
  Execute;
end;

procedure TfrmSearchForm.btnReplaceClick(Sender: TObject);
begin
  SearchEngine.Replace;
  Execute;
end;

procedure TfrmSearchForm.cbxSearchTextChange(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.chkCaseSensitiveClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.chkMultiLineClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.chkRegularExpressionsChange(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.chkReplaceWithChange(Sender: TObject);
begin
  if not chkReplaceWith.Checked then
    cbxReplaceWith.Text := '';
  Modified;
end;

procedure TfrmSearchForm.chkSearchAllViewsChange(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.chkWholeWordsOnlyClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.FormHide(Sender: TObject);
begin
  Manager.ClearHighlightSearch;
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TfrmSearchForm.Execute;
var
  S : string;
begin
  Manager.ClearHighlightSearch;
  SearchEngine.SearchText := cbxSearchText.Text;
  cbxSearchText.AddHistoryItem(SearchText, 30, True, True);
  SearchEngine.Options := Options;
  SearchEngine.SearchAllViews := chkSearchAllViews.Checked;
  Modified;
  // TODO: For some bizarre reason columms are not resized correctly when there
  // were records in the list for the last execution.
  // BEGIN workaround
  SearchEngine.ItemList.Clear;
  FTVP.Refresh;
  FVST.Header.AutoFitColumns(False);
  // END workaround
  SearchEngine.Execute;
  FTVP.Refresh;
  FVST.Header.AutoFitColumns(False);
  if FTVP.ItemsSource.Count = 1 then
    S := '%d search match found.'
  else
    S := '%d search matches found.';
  sbrMain.Panels[0].Text := Format(S, [FTVP.ItemsSource.Count]);
  Manager.ActiveView.SetHighlightSearch(SearchText, Options);
end;

procedure TfrmSearchForm.Modified;
begin
  FUpdate := True;
end;

procedure TfrmSearchForm.Cut;
begin
  PostMessage(GetFocus, WM_CUT, 0, 0);
end;

procedure TfrmSearchForm.Copy;
begin
  PostMessage(GetFocus, WM_COPY, 0, 0);
end;

procedure TfrmSearchForm.Paste;
begin
  PostMessage(GetFocus, WM_PASTE, 0, 0);
end;

procedure TfrmSearchForm.Undo;
begin
  PostMessage(GetFocus, WM_UNDO, 0, 0);
end;

procedure TfrmSearchForm.Redo;
begin
  PostMessage(GetFocus, WM_UNDO, 1, 0);
end;

procedure TfrmSearchForm.UpdateView;
begin
  //
end;

{ Updates activeview and selects SearchText }

procedure TfrmSearchForm.DoOnSelectionChanged(Sender: TObject);
var
  SR : TSearchResult;
  V  : IEditorView;
begin
  SR := (FTVP.CurrentItem as TSearchResult);
  if Assigned(SR) then
  begin
    Manager.ActivateView(SR.ViewName);
    Manager.ActiveView.SelStart := SR.StartPos;
    Manager.ActiveView.SelEnd := PointToPos(Manager.ActiveView.Lines, SR.BlockEnd);
    Modified;
    SearchEngine.CurrentIndex := SR.Index - 1;
  end;
end;

procedure TfrmSearchForm.UpdateActions;
var
  B: Boolean;
begin
  inherited UpdateActions;
  if Assigned(FTVP.CurrentItem)
    and (TSearchResult(FTVP.CurrentItem).Index <> SearchEngine.CurrentIndex + 1) then
  begin
    FTVP.CurrentItem := SearchEngine.ItemList[SearchEngine.CurrentIndex];
  end;
  btnReplace.Visible         := chkReplaceWith.Checked;
  btnReplaceAll.Visible      := chkReplaceWith.Checked;
  cbxReplaceWith.Enabled     := chkReplaceWith.Checked;
  B := not chkSearchAllViews.Checked;
  grpOrigin.Enabled    := B;
  grpScope.Enabled     := B;
  grpDirection.Enabled := B;
  grpOrigin.Visible    := B;
  grpScope.Visible     := B;
  grpDirection.Visible := B;
  if FUpdate then
  begin
    SearchEngine.Options := Options;
    SearchEngine.SearchText := SearchText;
    SearchEngine.ReplaceText := ReplaceText;
  end;
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

initialization
  {$I ts_editor_searchform.lrs}

end.



