{
  Copyright (C) 2013-2024 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.Search.ToolView;

{ Searchform to use in combination with an EditorManager instance. This toolform
  provides search/replace functionality for the active view or all views that
  are managed by the EditorManager. }

{
  TODO:
    - only show function keys when focused.
    - copy all matches to a stringlist when regular expressions are used
}

{$MODE DELPHI}

interface

uses
  SysUtils, StdCtrls, Forms, Buttons, ExtCtrls, Grids, ActnList, Classes,
  Contnrs,

  LResources,

  SynEditTypes,

  VirtualTrees,

  ts.Core.TreeViewPresenter, ts.Core.ColumnDefinitions,
  ts.Core.ColumnDefinitionsDataTemplate,
  ts.Editor.Interfaces, ts.Editor.ToolView.Base,
  ts.Core.Logger;

type
  TfrmSearchForm = class(TCustomEditorToolView, IEditorToolView)
    {$REGION 'designer controls'}
    aclMain                          : TActionList;
    actFocusSearchText               : TAction;
    actReplace                       : TAction;
    actReplaceAll                    : TAction;
    actFind                          : TAction;
    btnFind                          : TBitBtn;
    btnReplace                       : TBitBtn;
    btnReplaceAll                    : TBitBtn;
    cbxReplaceWith                   : TComboBox;
    cbxSearchText                    : TComboBox;
    chkCaseSensitive                 : TCheckBox;
    chkMultiLine                     : TCheckBox;
    chkRegularExpressions            : TCheckBox;
    chkReplaceStringsCaseSensitive   : TCheckBox;
    chkReplaceStringsWholeWordsOnly  : TCheckBox;
    chkWholeWordsOnly                : TCheckBox;
    grdReplaceStrings                : TStringGrid;
    grpDirection                     : TGroupBox;
    grpMisc                          : TGroupBox;
    grpOptions                       : TGroupBox;
    grpOrigin                        : TGroupBox;
    grpReplaceWith                   : TGroupBox;
    grpScope                         : TGroupBox;
    grpSearchText                    : TGroupBox;
    pnlButtons                       : TPanel;
    pnlClient                        : TPanel;
    pnlOperations                    : TPanel;
    pnlResultList                    : TPanel;
    pnlStatus                        : TPanel;
    rbtActiveView                    : TRadioButton;
    rbtAllViews                      : TRadioButton;
    rbtBackward                      : TRadioButton;
    rbtEntireScope                   : TRadioButton;
    rbtForward                       : TRadioButton;
    rbtFromCursor                    : TRadioButton;
    rbtSelection                     : TRadioButton;
    sbrMain                          : TScrollBox;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actFocusSearchTextExecute(Sender: TObject);
    procedure actReplaceAllExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure cbxSearchTextChange(Sender: TObject);
    procedure chkClick(Sender: TObject);
    procedure FTVPSelectionChanged(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbtBackwardChange(Sender: TObject);
    procedure rbtEntireScopeClick(Sender: TObject);
    procedure rbtForwardClick(Sender: TObject);
    procedure rbtFromCursorClick(Sender: TObject);
    procedure rbtActiveViewClick(Sender: TObject);
    procedure rbtSelectionClick(Sender: TObject);
    {$ENDREGION}

  private
    FTVP : TTreeViewPresenter;
    FVST : TVirtualStringTree;

    {$REGION 'property access mehods'}
    function GetOptions: TSynSearchOptions;
    function GetReplaceText: string;
    function GetSearchEngine: IEditorSearchEngine;
    function GetSearchText: string;
    procedure SetOptions(AValue: TSynSearchOptions);
    procedure SetReplaceText(const AValue: string);
    procedure SetSearchText(const AValue: string);
    {$ENDREGION}

    procedure SearchEngineExecute(Sender: TObject);
    procedure SearchEngineChange(Sender: TObject);
    procedure ActionExecute(
      Sender       : TObject;
      AAction      : TBasicAction;
      var AHandled : Boolean
    );

  protected
    procedure EditorSettingsChanged(Sender: TObject); override;

    procedure SettingsChanged; override;
    procedure Execute;

    property SearchEngine: IEditorSearchEngine
      read GetSearchEngine;

  public
    procedure AfterConstruction; override;

    procedure UpdateActions; override;
    procedure UpdateView; override;

    property Options : TSynSearchOptions
      read GetOptions write SetOptions;

    property SearchText : string
      read GetSearchText write SetSearchText;

    property ReplaceText : string
      read GetReplaceText write SetReplaceText;
  end;

implementation

{$R *.lfm}

uses
  Controls, Graphics,

  LCLIntf,

  ts.Core.Helpers,

  ts.Editor.Utils,

  ts.Editor.Search.Data, ts.Editor.Search.Templates;

resourcestring
  SFileName     = 'FileName';
  SMatchFound   = '%d search match found.';
  SMatchesFound = '%d search matches found.';

{$REGION 'construction and destruction'}
procedure TfrmSearchForm.AfterConstruction;
begin
  inherited AfterConstruction;
  FVST := VST.Create(Self, pnlResultList);
  FVST.TreeOptions.AutoOptions :=
    FVST.TreeOptions.AutoOptions + [toAutoSpanColumns];
  FVST.Header.MainColumn := 1;
  FVST.BorderStyle := bsNone;
  FTVP := TTreeViewPresenter.Create(Self);
  FTVP.MultiSelect := False;
  FTVP.ShowHeader  := False;
  FTVP.ListMode    := False;
  FTVP.ColumnDefinitions.AddColumn('Text', SFileName, dtString, 70, 60, 400);
  FTVP.ItemsSource := SearchEngine.ItemGroups;
  FTVP.TreeView    := FVST;
  FTVP.ItemTemplate := TSearchResultGroupTemplate.Create(FTVP.ColumnDefinitions);
  FTVP.OnSelectionChanged := FTVPSelectionChanged;
  cbxSearchText.Text  := '';
  cbxReplaceWith.Text := '';

  SearchEngine.AddOnExecuteHandler(SearchEngineExecute);
  SearchEngine.AddOnChangeHandler(SearchEngineChange);
  Manager.Events.AddOnActionExecuteEvent(ActionExecute);
end;
{$ENDREGION}

{$REGION 'property access mehods'}
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
  if rbtEntireScope.Checked then
    Include(Result, ssoEntireScope);
  if rbtSelection.Checked then
    Include(Result, ssoSelectedOnly);
  if rbtBackward.Checked then
    Include(Result, ssoBackwards);
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
    rbtSelection.Checked := True
  else
    rbtActiveView.Checked   := True;
  if ssoBackwards in AValue then
    rbtBackward.Checked := True
  else
    rbtForward.Checked  := True;
end;

function TfrmSearchForm.GetSearchText: string;
begin
  Result := cbxSearchText.Text;
end;

procedure TfrmSearchForm.SetSearchText(const AValue: string);
begin
  if AValue <> SearchText then
  begin
    cbxSearchText.Text := AValue;
  end;
end;

function TfrmSearchForm.GetReplaceText: string;
begin
  Result := cbxReplaceWith.Text;
end;

procedure TfrmSearchForm.SetReplaceText(const AValue: string);
begin
  if AValue <> ReplaceText then
  begin
    cbxReplaceWith.Text := AValue;
  end;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmSearchForm.actFocusSearchTextExecute(Sender: TObject);
begin
  cbxSearchText.SetFocus;
end;

procedure TfrmSearchForm.actReplaceAllExecute(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    SearchEngine.ReplaceAll;
    Execute;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmSearchForm.actReplaceExecute(Sender: TObject);
begin
  SearchEngine.Replace;
  Execute;
end;

procedure TfrmSearchForm.actFindExecute(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    Execute;
  finally
    Screen.Cursor := crDefault;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
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
  SearchEngine.ItemList.Clear;
  SearchEngine.ItemGroups.Clear;
  FTVP.Refresh;
  SearchEngine.SearchText := '';
  SearchEngine.ReplaceText := '';
  Manager.ClearHighlightSearch;
end;

procedure TfrmSearchForm.FormShow(Sender: TObject);
begin
  Options := SearchEngine.Options;
  rbtAllViews.Checked  := SearchEngine.SearchAllViews;
  cbxSearchText.Text  := SearchEngine.SearchText;
  cbxReplaceWith.Text := SearchEngine.ReplaceText;
  {$IFDEF DARWIN}//THE FORM IN MACOS HAVE SOME PROBLEMS WITH FOCUS, THIS IS TEMPORARY FIX
  cbxSearchText.SetFocus;
  {$ENDIF}
end;

procedure TfrmSearchForm.rbtBackwardChange(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.rbtEntireScopeClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.rbtForwardClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.rbtFromCursorClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.rbtActiveViewClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.rbtSelectionClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.EditorSettingsChanged(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.SearchEngineExecute(Sender: TObject);
var
  S : string;
begin
  FTVP.Refresh;
  FVST.Header.AutoFitColumns(False, smaAllColumns, 0);
  if SearchEngine.ItemList.Count = 1 then
    S := SMatchFound
  else
    S := SMatchesFound;
  pnlStatus.Caption := Format(S, [SearchEngine.ItemList.Count]);
  if Visible then
  begin
    if SearchEngine.ItemList.Count > 0 then
    begin
      pnlStatus.Font.Color := clGreen;
      FVST.SetFocus;
      FTVP.SelectedItem := FTVP.ItemsSource[0];
    end
    else
    begin
      pnlStatus.Font.Color := clRed;
      cbxSearchText.SetFocus;
      cbxSearchText.SelectAll;
    end;
  end;
end;

procedure TfrmSearchForm.SearchEngineChange(Sender: TObject);
begin
  SearchText  := SearchEngine.SearchText;
  ReplaceText := SearchEngine.ReplaceText;
  if Assigned(FTVP.CurrentItem)
    and (TSearchResult(FTVP.CurrentItem).Index <> SearchEngine.CurrentIndex + 1)
    then
  begin
    FTVP.CurrentItem := SearchEngine.ItemList[SearchEngine.CurrentIndex];
  end;
end;

procedure TfrmSearchForm.ActionExecute(Sender: TObject; AAction: TBasicAction;
  var AHandled: Boolean);
begin
  Logger.Info('Executed %s', [AAction.Name]);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmSearchForm.Execute;
begin
  SearchEngine.SearchText := cbxSearchText.Text;
  cbxSearchText.AddHistoryItem(SearchText, 30, True, True);
  SearchEngine.SearchAllViews := rbtAllViews.Checked;
  SearchEngine.Options        := Options;
  SearchEngine.Execute;
end;

procedure TfrmSearchForm.UpdateView;
begin
  if rbtAllViews.Checked then
  begin
    View.BeginUpdate;
    View.SetHighlightSearch(
      SearchEngine.SearchText,
      SearchEngine.Options
    );
    View.EndUpdate;
  end;
end;

procedure TfrmSearchForm.SettingsChanged;
begin
  inherited SettingsChanged;
  Options            := SearchEngine.Options;
  rbtAllViews.Checked := SearchEngine.SearchAllViews;
end;

{ Updates the active editorview and selects SearchText }

procedure TfrmSearchForm.FTVPSelectionChanged(Sender: TObject);
var
  SR  : TSearchResult;
  SRL : TSearchResultLine;
  SRG : TSearchResultGroup;
begin
  if FTVP.CurrentItem is TSearchResult then
  begin
    SR := FTVP.CurrentItem as TSearchResult;
    Manager.ActivateView(SR.ViewName);
    View.SelStart := SR.StartPos;
    View.SelEnd   := PointToPos(View.Lines, SR.BlockEnd);
    Modified;
    SearchEngine.CurrentIndex := SearchEngine.ItemList.IndexOf(SR);
  end
  else if FTVP.CurrentItem is TSearchResultLine then
  begin
    SRL := FTVP.CurrentItem as TSearchResultLine;
    SR  := SRL.List[0] as TSearchResult;
    Manager.ActivateView(SR.ViewName);
    View.SelStart := SR.StartPos;
    View.SelEnd   := PointToPos(View.Lines, SR.BlockEnd);
    Modified;
    SearchEngine.CurrentIndex := SearchEngine.ItemList.IndexOf(SR);
  end
  else if FTVP.CurrentItem is TSearchResultGroup then
  begin
    SRG := (FTVP.CurrentItem as TSearchResultGroup);
    SRL := SRG.Lines[0] as TSearchResultLine;
    SR  := SRL.List[0] as TSearchResult;
    Manager.ActivateView(SRG.ViewName);
    View.SelStart := SR.StartPos;
    View.SelEnd   := PointToPos(View.Lines, SR.BlockEnd);
    Modified;
    SearchEngine.CurrentIndex := SearchEngine.ItemList.IndexOf(SR);
  end;
end;

procedure TfrmSearchForm.UpdateActions;
var
  B: Boolean;
begin
  inherited UpdateActions;
  { Focus the corresponding search result in the list when we do find next/
    find previous from the editor view. }
  B := (SearchEngine.ItemList.Count > 0) and (ReplaceText <> '');
  btnReplace.Visible     := B;
  btnReplaceAll.Visible  := B;
  B := not rbtAllViews.Checked;
  grpOrigin.Enabled    := B;
  grpDirection.Enabled := B;
  grpOrigin.Visible    := B;
  grpDirection.Visible := B;
  if Update then
  begin
    SearchEngine.ReplaceText := ReplaceText;
    if (SearchEngine.SearchText <> SearchText)
      or (SearchEngine.Options <> Options) then
    begin
      SearchEngine.Options    := Options;
      SearchEngine.SearchText := SearchText;
      pnlStatus.Caption       := '';
      Manager.ClearHighlightSearch;
      SearchEngine.ItemGroups.Clear;
      SearchEngine.ItemList.Clear;
      FTVP.Refresh;
    end;
    Updated;
  end;
end;
{$ENDREGION}

end.
