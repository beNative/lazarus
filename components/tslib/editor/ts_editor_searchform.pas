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

  LResources,

  SynEditTypes,

  VirtualTrees,

  ts_Core_TreeViewPresenter,

  ts_Editor_Interfaces;

//=============================================================================

type
  TfrmSearchForm = class(TForm, IEditorToolView,
                                IClipboardCommands)
    {$region 'designer controls' /fold}
    aclMain                         : TActionList;
    actBackward                     : TAction;
    actCaseSensitive                : TAction;
    actEntireScope                  : TAction;
    actFocusSearchText              : TAction;
    actForward                      : TAction;
    actFromCursor                   : TAction;
    actGlobal                       : TAction;
    actMultiline                    : TAction;
    actRegularExpressions           : TAction;
    actReplace                      : TAction;
    actReplaceAll                   : TAction;
    actReplaceWith                  : TAction;
    actSearch                       : TAction;
    actSearchInAllViews             : TAction;
    actSelected                     : TAction;
    actToggleDirection              : TAction;
    actToggleOrigin                 : TAction;
    actToggleScope                  : TAction;
    actWholeWordsOnly               : TAction;
    btnFind                         : TBitBtn;
    btnReplace                      : TBitBtn;
    btnReplaceAll                   : TBitBtn;
    cbxReplaceWith                  : TComboBox;
    cbxSearchText                   : TComboBox;
    chkCaseSensitive                : TSpeedButton;
    chkMultiLine                    : TSpeedButton;
    chkRegularExpressions1          : TSpeedButton;
    chkReplaceStringsCaseSensitive  : TCheckBox;
    chkReplaceStringsWholeWordsOnly : TCheckBox;
    chkReplaceWith                  : TSpeedButton;
    chkSearchAllViews1              : TSpeedButton;
    chkWholeWordsOnly               : TSpeedButton;
    DirectionGroupBox               : TGroupBox;
    grdReplaceStrings               : TStringGrid;
    grpDirection                    : TGroupBox;
    grpMisc                         : TGroupBox;
    grpOptions                      : TGroupBox;
    grpOrigin                       : TGroupBox;
    grpReplaceWith                  : TGroupBox;
    grpScope                        : TGroupBox;
    grpSearchText                   : TGroupBox;
    Image1                          : TImage;
    Image2                          : TImage;
    Image3                          : TImage;
    Image4                          : TImage;
    lblSearchText                   : TLabel;
    pnlButtons                      : TPanel;
    pnlDirection                    : TPanel;
    pnlDirectionShortcut            : TPanel;
    pnlOperations                   : TPanel;
    pnlOrigin                       : TPanel;
    pnlOriginShortcut               : TPanel;
    pnlResultList                   : TPanel;
    pnlScope                        : TPanel;
    pnlScopeShortcut                : TPanel;
    rbtBackward                     : TSpeedButton;
    rbtEntireScope                  : TSpeedButton;
    rbtForward                      : TSpeedButton;
    rbtFromCursor1                  : TSpeedButton;
    rbtGlobal                       : TSpeedButton;
    rbtSelected                     : TSpeedButton;
    sbrMain                         : TStatusBar;
    {$endregion}

    {$region 'action handlers' /fold}
    procedure actCaseSensitiveExecute(Sender: TObject);
    procedure actFocusSearchTextExecute(Sender: TObject);
    procedure actMultilineExecute(Sender: TObject);
    procedure actRegularExpressionsExecute(Sender: TObject);
    procedure actReplaceAllExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actReplaceWithExecute(Sender: TObject);
    procedure actSearchExecute(Sender: TObject);
    procedure actSearchInAllViewsExecute(Sender: TObject);
    procedure actToggleDirectionExecute(Sender: TObject);
    procedure actToggleOriginExecute(Sender: TObject);
    procedure actToggleScopeExecute(Sender: TObject);
    procedure actWholeWordsOnlyExecute(Sender: TObject);
    {$endregion}

    procedure cbxSearchTextChange(Sender: TObject);
    procedure DoOnSelectionChanged(Sender: TObject);
    procedure FormHide(Sender: TObject);

  private
    FTVP      : TTreeViewPresenter;
    FVST      : TVirtualStringTree;
    FUpdate   : Boolean;

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
    procedure SetVisible(AValue: Boolean); override;

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
  Modified;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************
{$endregion}

{$region 'property access mehods' /fold}
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
  actCaseSensitive.Checked      := ssoMatchCase in AValue;
  actWholeWordsOnly.Checked     := ssoWholeWord in AValue;
  actRegularExpressions.Checked := ssoRegExpr in AValue;
  actMultiLine.Checked          := ssoRegExprMultiLine in AValue;

  if ssoEntireScope in AValue then
    actEntireScope.Checked := True
  else
    actFromCursor.Checked  := True;
  if ssoSelectedOnly in AValue then
    actSelected.Checked := True
  else
    actGlobal.Checked   := True;
  if ssoBackwards in AValue then
    actBackward.Checked := True
  else
    actForward.Checked  := True;

  Modified;
end;

function TfrmSearchForm.GetOptions: TSynSearchOptions;
begin
  Result := [];
  if actCaseSensitive.Checked then
    Include(Result, ssoMatchCase);
  if actWholeWordsOnly.Checked then
    Include(Result, ssoWholeWord);
  if actRegularExpressions.Checked then
    Include(Result, ssoRegExpr);
  if actMultiLine.Checked then
    Include(Result, ssoRegExprMultiLine);
  if actEntireScope.Checked then
    Include(Result, ssoEntireScope);
  if actSelected.Checked then
    Include(Result, ssoSelectedOnly);
  if actBackward.Checked then
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
{$endregion}

{$region 'action handlers' /fold}
//*****************************************************************************
// action handlers                                                       BEGIN
//*****************************************************************************

procedure TfrmSearchForm.actFocusSearchTextExecute(Sender: TObject);
begin
  cbxSearchText.SetFocus;
end;

procedure TfrmSearchForm.actCaseSensitiveExecute(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.actMultilineExecute(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.actRegularExpressionsExecute(Sender: TObject);
begin
  Modified;
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

procedure TfrmSearchForm.actSearchExecute(Sender: TObject);
begin
  Execute;
end;

procedure TfrmSearchForm.actSearchInAllViewsExecute(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.actToggleDirectionExecute(Sender: TObject);
begin
  if actForward.Checked then
    actBackward.Execute
  else
    actForward.Execute;
end;

procedure TfrmSearchForm.actToggleOriginExecute(Sender: TObject);
begin
  if actFromCursor.Checked then
  begin
    actEntireScope.Execute;
  end
  else
  begin
    actFromCursor.Execute;
  end;
end;

procedure TfrmSearchForm.actToggleScopeExecute(Sender: TObject);
begin
  if actSelected.Checked then
    actGlobal.Execute
  else
    actSelected.Execute;
end;

procedure TfrmSearchForm.actWholeWordsOnlyExecute(Sender: TObject);
begin
  Modified;
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

procedure TfrmSearchForm.FormHide(Sender: TObject);
begin
  Manager.ClearHighlightSearch;
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
  SearchEngine.SearchAllViews := actSearchInAllViews.Checked;
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
  sbrMain.Panels[0].Text := Format(S, [FTVP.ItemsSource.Count]);
  Manager.ActiveView.SetHighlightSearch(SearchText, Options);
  FVST.SetFocus;
end;

procedure TfrmSearchForm.Modified;
begin
  FUpdate := True;
end;

procedure TfrmSearchForm.Cut;
begin
  PostMessage(GetFocus, LM_CUT, 0, 0);
end;

procedure TfrmSearchForm.Copy;
begin
  PostMessage(GetFocus, LM_COPY, 0, 0);
end;

procedure TfrmSearchForm.Paste;
begin
  PostMessage(GetFocus, LM_PASTE, 0, 0);
end;

procedure TfrmSearchForm.Undo;
begin
{$ifdef windows}
  PostMessage(GetFocus, WM_UNDO, 0, 0);
{$endif}
end;

procedure TfrmSearchForm.Redo;
begin
{$ifdef windows}
  PostMessage(GetFocus, WM_UNDO, 1, 0);
{$endif}
end;

procedure TfrmSearchForm.UpdateView;
begin
  if actSearchInAllViews.Checked then
  begin
    Manager.ActiveView.BeginUpdate;
    Manager.ActiveView.SetHighlightSearch(SearchEngine.SearchText, SearchEngine.Options);
    Manager.ActiveView.EndUpdate;
  end;
  actReplaceWith.Checked := Manager.Actions['actSearchReplace'].Checked;
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
    and (TSearchResult(FTVP.CurrentItem).Index <> SearchEngine.CurrentIndex + 1) then
  begin
    FTVP.CurrentItem := SearchEngine.ItemList[SearchEngine.CurrentIndex];
  end;
  B := (SearchEngine.ItemList.Count > 0) and actReplaceWith.Checked;
  btnReplace.Visible         := B;
  btnReplaceAll.Visible      := B;
  cbxReplaceWith.Enabled     := B;
  B := not actSearchInAllViews.Checked;
  grpOrigin.Enabled    := B;
  grpScope.Enabled     := B;
  grpDirection.Enabled := B;
  grpOrigin.Visible    := B;
  grpScope.Visible     := B;
  grpDirection.Visible := B;
  if FUpdate then
  begin
    SearchEngine.ReplaceText := ReplaceText;
    if (SearchEngine.SearchText <> SearchText)
      or (SearchEngine.Options <> Options) then
    begin
      Manager.ClearHighlightSearch;
      SearchEngine.Options := Options;
      SearchEngine.SearchText := SearchText;
      // BEGIN workaround
      SearchEngine.ItemList.Clear;
      FTVP.Refresh;
      sbrMain.Panels[0].Text := '';
      FVST.Header.AutoFitColumns(False);
      // END workaround
    end;
    FUpdate := False;
  end;

  //B := Assigned(Screen.ActiveControl) and (Screen.ActiveControl.Owner = Self);
  //Image4.Visible := B;

end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************
{$endregion}

initialization
  {$I ts_editor_searchform.lrs}

end.
