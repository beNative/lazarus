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
  TODO:
    - only show function keys when focused.
    - copy all matches to a stringlist when regular expressions are used
}

{$mode delphi}

//*****************************************************************************

interface

uses
  SysUtils, StdCtrls, Forms, Buttons, ExtCtrls, Grids, ComCtrls, ActnList,
  Classes,

  LResources,

  SynEditTypes,

  VirtualTrees,

  ts_Core_TreeViewPresenter,

  ts_Editor_Interfaces, ts_Editor_CustomToolView;

//=============================================================================

type
  TfrmSearchForm = class(TCustomEditorToolView, IEditorToolView,
                                                IClipboardCommands)
    {$region 'designer controls' /fold}
    aclMain                         : TActionList;
    actFocusSearchText              : TAction;
    actReplace                      : TAction;
    actReplaceAll                   : TAction;
    actReplaceWith                  : TAction;
    actFind                         : TAction;
    btnFind                         : TBitBtn;
    btnReplace                      : TBitBtn;
    btnReplaceAll                   : TBitBtn;
    cbxReplaceWith                  : TComboBox;
    cbxSearchText                   : TComboBox;
    chkCaseSensitive                : TCheckBox;
    chkMultiLine                    : TCheckBox;
    chkRegularExpressions          : TCheckBox;
    chkReplaceStringsCaseSensitive  : TCheckBox;
    chkReplaceStringsWholeWordsOnly : TCheckBox;
    chkSearchInAllViews              : TCheckBox;
    chkWholeWordsOnly               : TCheckBox;
    DirectionGroupBox               : TGroupBox;
    grdReplaceStrings               : TStringGrid;
    grpDirection                    : TGroupBox;
    grpMisc                         : TGroupBox;
    grpOptions                      : TGroupBox;
    grpOrigin                       : TGroupBox;
    grpScope                        : TGroupBox;
    grpReplaceWith                  : TGroupBox;
    grpSearchText                   : TGroupBox;
    Image1                          : TImage;
    Image2                          : TImage;
    pnlButtons                      : TPanel;
    pnlOperations                   : TPanel;
    pnlResultList                   : TPanel;
    rbBackward                      : TRadioButton;
    rbEntireScope                   : TRadioButton;
    rbForward                       : TRadioButton;
    rbFromCursor                    : TRadioButton;
    rbGlobal                        : TRadioButton;
    rbSelected                      : TRadioButton;
    pnlStatus                       : TPanel;
    {$endregion}

    {$region 'action handlers' /fold}
    procedure actFocusSearchTextExecute(Sender: TObject);
    procedure actReplaceAllExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actReplaceWithExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    {$endregion}

    procedure cbxSearchTextChange(Sender: TObject);
    procedure chkClick(Sender: TObject);
    procedure DoOnSelectionChanged(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbBackwardChange(Sender: TObject);
    procedure rbEntireScopeClick(Sender: TObject);
    procedure rbForwardClick(Sender: TObject);
    procedure rbFromCursorClick(Sender: TObject);
    procedure rbGlobalClick(Sender: TObject);
    procedure rbSelectedClick(Sender: TObject);

  private
    FTVP      : TTreeViewPresenter;
    FVST      : TVirtualStringTree;

    procedure EditorSettingsChanged(Sender: TObject);

    function GetSearchEngine: IEditorSearchEngine;
    function GetOptions: TSynSearchOptions;
    function GetSearchText: string;
    procedure SetOptions(AValue: TSynSearchOptions);
    procedure SetSearchText(const AValue: string);
    function GetReplaceText: string;
    procedure SetReplaceText(const AValue: string);

  strict protected

    procedure SettingsChanged; override;
    procedure Execute;

    property SearchEngine: IEditorSearchEngine
      read GetSearchEngine;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure UpdateActions; override;
    procedure UpdateView; override;

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
{$ifdef windows}
  Windows,
{$endif}
  LCLIntf, LMessages,

  ts_Core_ColumnDefinitions, ts_Core_Helpers,

  ts_Editor_SearchEngine, ts_Editor_Utils;

{$region 'construction and destruction' /fold}
//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmSearchForm.AfterConstruction;
begin
  inherited AfterConstruction;
  FVST := CreateVST(Self, pnlResultList);
  FTVP := TTreeViewPresenter.Create(Self);
  FTVP.MultiSelect := False;
  FTVP.ColumnDefinitions.AddColumn('Index', '#', dtNumeric, 50, 50, 80);
  FTVP.ColumnDefinitions.AddColumn('FileName', dtString, 160, 120, 400);
  FTVP.ColumnDefinitions.AddColumn('Column', dtNumeric, 60, 60, 80);
  FTVP.ColumnDefinitions.AddColumn('Line', dtNumeric, 40, 40, 80);
  FVST.Header.MainColumn := 1;
  FTVP.ItemsSource := SearchEngine.ItemList;
  FTVP.TreeView := FVST;
  FTVP.OnSelectionChanged := DoOnSelectionChanged;
  cbxSearchText.Text  := '';
  cbxReplaceWith.Text := '';
end;

procedure TfrmSearchForm.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************
{$endregion}

{$region 'property access mehods' /fold}
//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TfrmSearchForm.GetSearchEngine: IEditorSearchEngine;
begin
  Result := Owner as IEditorSearchEngine;
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
  if rbEntireScope.Checked then
    Include(Result, ssoEntireScope);
  if rbSelected.Checked then
    Include(Result, ssoSelectedOnly);
  if rbBackward.Checked then
    Include(Result, ssoBackwards);
end;

procedure TfrmSearchForm.SetOptions(AValue: TSynSearchOptions);
begin
  chkCaseSensitive.Checked      := ssoMatchCase in AValue;
  chkWholeWordsOnly.Checked     := ssoWholeWord in AValue;
  chkRegularExpressions.Checked := ssoRegExpr in AValue;
  chkMultiLine.Checked          := ssoRegExprMultiLine in AValue;

  if ssoEntireScope in AValue then
    rbEntireScope.Checked := True
  else
    rbFromCursor.Checked  := True;
  if ssoSelectedOnly in AValue then
    rbSelected.Checked := True
  else
    rbGlobal.Checked   := True;
  if ssoBackwards in AValue then
    rbBackward.Checked := True
  else
    rbForward.Checked  := True;
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

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************
{$endregion}

{$region 'action handlers' /fold}
//*****************************************************************************
// action handlers                                                       BEGIN
//*****************************************************************************

procedure TfrmSearchForm.actFocusSearchTextExecute(Sender: TObject);
begin
  cbxSearchText.SetFocus;
end;

procedure TfrmSearchForm.actReplaceAllExecute(Sender: TObject);
begin
  SearchEngine.ReplaceAll;
  Execute;
end;

procedure TfrmSearchForm.actReplaceExecute(Sender: TObject);
begin
  SearchEngine.Replace;
  Execute;
end;

procedure TfrmSearchForm.actReplaceWithExecute(Sender: TObject);
begin
  Manager.Actions['actSearchReplace'].Checked := actReplaceWith.Checked;
  if not actReplaceWith.Checked then
    cbxReplaceWith.Text := ''
  else
  begin
    cbxReplaceWith.Enabled := True;
    cbxReplaceWith.SetFocus;
  end;
  Modified;
end;

procedure TfrmSearchForm.actFindExecute(Sender: TObject);
begin
  Execute;
end;

//*****************************************************************************
// action handlers                                                         END
//*****************************************************************************
{$endregion}

{$region 'event handlers' /fold}
//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

procedure TfrmSearchForm.cbxSearchTextChange(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.chkClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.FormHide(Sender: TObject);
begin
  Manager.ClearHighlightSearch;
end;

procedure TfrmSearchForm.FormShow(Sender: TObject);
begin
  Options := SearchEngine.Options;
  chkSearchInAllViews.Checked := SearchEngine.SearchAllViews;
end;

procedure TfrmSearchForm.rbBackwardChange(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.rbEntireScopeClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.rbForwardClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.rbFromCursorClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.rbGlobalClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.rbSelectedClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.EditorSettingsChanged(Sender: TObject);
begin

end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************
{$endregion}

{$region 'protected methods' /fold}
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
  SearchEngine.SearchAllViews := chkSearchInAllViews.Checked;
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
  FVST.Header.AutoFitColumns(False, smaAllColumns, 0);
  if FTVP.ItemsSource.Count = 1 then
    S := '%d search match found.'
  else
    S := '%d search matches found.';
  pnlStatus.Caption := Format(S, [FTVP.ItemsSource.Count]);
  Manager.ActiveView.SetHighlightSearch(SearchText, Options);
  FVST.SetFocus;
end;

procedure TfrmSearchForm.UpdateView;
begin
  if chkSearchInAllViews.Checked then
  begin
    Manager.ActiveView.BeginUpdate;
    Manager.ActiveView.SetHighlightSearch(
      SearchEngine.SearchText,
      SearchEngine.Options
    );
    Manager.ActiveView.EndUpdate;
  end;
  actReplaceWith.Checked := Manager.Actions['actSearchReplace'].Checked;
end;

procedure TfrmSearchForm.SettingsChanged;
begin
  inherited SettingsChanged;
  Options := SearchEngine.Options;
  chkSearchInAllViews.Checked := SearchEngine.SearchAllViews;
end;

{ Updates activeview and selects SearchText }

procedure TfrmSearchForm.DoOnSelectionChanged(Sender: TObject);
var
  SR : TSearchResult;
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
    and (TSearchResult(FTVP.CurrentItem).Index
      <> SearchEngine.CurrentIndex + 1) then
  begin
    FTVP.CurrentItem := SearchEngine.ItemList[SearchEngine.CurrentIndex];
  end;
  B := (SearchEngine.ItemList.Count > 0) and actReplaceWith.Checked;
  btnReplace.Visible     := B;
  btnReplaceAll.Visible  := B;
  cbxReplaceWith.Enabled := B;
  B := not chkSearchInAllViews.Checked;
  grpOrigin.Enabled    := B;
  grpScope.Enabled     := B;
  grpDirection.Enabled := B;
  grpOrigin.Visible    := B;
  grpScope.Visible     := B;
  grpDirection.Visible := B;
  if Update then
  begin
    SearchEngine.ReplaceText := ReplaceText;
    if (SearchEngine.SearchText <> SearchText)
      or (SearchEngine.Options <> Options) then
    begin
      SearchEngine.Options := Options;
      SearchEngine.SearchText := SearchText;
      pnlStatus.Caption := '';
      Manager.ClearHighlightSearch;
      // BEGIN workaround
      SearchEngine.ItemList.Clear;
      FTVP.Refresh;
      FVST.Header.AutoFitColumns(False);
      // END workaround
    end;
    Updated;
  end;
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************
{$endregion}

initialization
  {$I ts_editor_searchform.lrs}

end.
