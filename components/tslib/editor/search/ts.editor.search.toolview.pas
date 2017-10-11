{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

  ts.Core.Value, ts.Core.TreeViewPresenter, ts.Core.ColumnDefinitions,
  ts.Core.DataTemplates, ts.Core.ColumnDefinitionsDataTemplate,

  ts.Editor.Interfaces, ts.Editor.Types, ts.Editor.ToolView.Base,

  ts.Core.SharedLogger, ts.Core.Utils;

type

  { TfrmSearchForm }

  TfrmSearchForm = class(TCustomEditorToolView, IEditorToolView)
    {$REGION 'designer controls' /FOLD}
    aclMain                         : TActionList;
    actFocusSearchText              : TAction;
    actReplace                      : TAction;
    actReplaceAll                   : TAction;
    actFind                         : TAction;
    btnFind: TBitBtn;
    btnReplace: TBitBtn;
    btnReplaceAll: TBitBtn;
    cbxReplaceWith                  : TComboBox;
    cbxSearchText                   : TComboBox;
    chkCaseSensitive                : TCheckBox;
    chkMultiLine: TCheckBox;
    chkRegularExpressions: TCheckBox;
    chkReplaceStringsCaseSensitive  : TCheckBox;
    chkReplaceStringsWholeWordsOnly : TCheckBox;
    chkWholeWordsOnly               : TCheckBox;
    DirectionGroupBox               : TGroupBox;
    grdReplaceStrings               : TStringGrid;
    grpDirection: TGroupBox;
    grpMisc                         : TGroupBox;
    grpOptions                      : TGroupBox;
    grpOrigin: TGroupBox;
    grpReplaceWith                  : TGroupBox;
    grpScope: TGroupBox;
    grpSearchText                   : TGroupBox;
    imgF2Key                          : TImage;
    pnlButtons: TPanel;
    pnlOperations                   : TPanel;
    pnlResultList                   : TPanel;
    rbAllViews: TRadioButton;
    rbBackward: TRadioButton;
    rbEntireScope: TRadioButton;
    pnlStatus                       : TPanel;
    rbForward: TRadioButton;
    rbFromCursor: TRadioButton;
    rbActiveView: TRadioButton;
    rbSelection: TRadioButton;
    {$ENDREGION}

    {$REGION 'action handlers' /FOLD}
    procedure actFocusSearchTextExecute(Sender: TObject);
    procedure actReplaceAllExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    {$ENDREGION}

    procedure cbxSearchTextChange(Sender: TObject);
    procedure chkClick(Sender: TObject);
    procedure DoOnSelectionChanged(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbBackwardChange(Sender: TObject);
    procedure rbEntireScopeClick(Sender: TObject);
    procedure rbForwardClick(Sender: TObject);
    procedure rbFromCursorClick(Sender: TObject);
    procedure rbActiveViewClick(Sender: TObject);
    procedure rbSelectionClick(Sender: TObject);

  private
    FTVP : TTreeViewPresenter;
    FVST : TVirtualStringTree;

    procedure SearchEngineExecute(Sender: TObject);
    procedure SearchEngineChange(Sender: TObject);
    procedure ActionExecute(
          Sender   : TObject;
          AAction  : TBasicAction;
      var AHandled : Boolean
    );

    function GetSearchEngine: IEditorSearchEngine;
    function GetOptions: TSynSearchOptions;
    function GetSearchText: string;
    procedure SetOptions(AValue: TSynSearchOptions);
    procedure SetSearchText(const AValue: string);
    function GetReplaceText: string;
    procedure SetReplaceText(const AValue: string);

  strict protected
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

  ts.Editor.Search.Engine, ts.Editor.Search.Data, ts.Editor.Search.Templates;

resourcestring
  SFileName     = 'FileName';
  SMatchFound   = '%d search match found.';
  SMatchesFound = '%d search matches found.';

{$REGION 'construction and destruction' /FOLD}
procedure TfrmSearchForm.AfterConstruction;
begin
  inherited AfterConstruction;
  FVST := VST.Create(Self, pnlResultList);
  FVST.TreeOptions.AutoOptions :=
    FVST.TreeOptions.AutoOptions + [toAutoSpanColumns] ;
  FVST.Header.MainColumn := 1;

  FTVP := TTreeViewPresenter.Create(Self);
  FTVP.MultiSelect := False;
  FTVP.ShowHeader  := False;
  FTVP.ListMode    := False;
  FTVP.ColumnDefinitions.AddColumn('Text', SFileName, dtString, 70, 60, 400);
  FTVP.ItemsSource := SearchEngine.ItemGroups;
  FTVP.TreeView    := FVST;
  FTVP.ItemTemplate := TSearchResultGroupTemplate.Create(FTVP.ColumnDefinitions);
  FTVP.OnSelectionChanged := DoOnSelectionChanged;
  cbxSearchText.Text  := '';
  cbxReplaceWith.Text := '';

  SearchEngine.AddOnExecuteHandler(SearchEngineExecute);
  SearchEngine.AddOnChangeHandler(SearchEngineChange);
  Manager.Events.AddOnActionExecuteEvent(ActionExecute);
end;
{$ENDREGION}

{$REGION 'property access mehods' /FOLD}
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
  if rbSelection.Checked then
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
    rbSelection.Checked := True
  else
    rbActiveView.Checked   := True;
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

{$REGION 'action handlers' /FOLD}
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

{$REGION 'event handlers' /FOLD}
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
  rbAllViews.Checked  := SearchEngine.SearchAllViews;
  cbxSearchText.Text  := SearchEngine.SearchText;
  cbxReplaceWith.Text := SearchEngine.ReplaceText;
  {$IFDEF DARWIN}//THE FORM IN MACOS HAVE SOME PROBLEMS WITH FOCUS, THIS IS TEMPORARY FIX
  cbxSearchText.setFocus;
  {$ENDIF}
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

procedure TfrmSearchForm.rbActiveViewClick(Sender: TObject);
begin
  Modified;
end;

procedure TfrmSearchForm.rbSelectionClick(Sender: TObject);
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
  Logger.Send('Executed', AAction.Name);
end;
{$ENDREGION}

{$REGION 'protected methods' /FOLD}
procedure TfrmSearchForm.Execute;
begin
  SearchEngine.SearchText := cbxSearchText.Text;
  cbxSearchText.AddHistoryItem(SearchText, 30, True, True);
  SearchEngine.SearchAllViews := rbAllViews.Checked;
  SearchEngine.Options        := Options;
  Logger.Send(
    'SearchOptions',
    SetToString(TypeInfo(TSynSearchOptions),
    SearchEngine.Options)
  );
  SearchEngine.Execute;
end;

procedure TfrmSearchForm.UpdateView;
begin
  if rbAllViews.Checked then
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
  rbAllViews.Checked := SearchEngine.SearchAllViews;
end;

{ Updates the active editorview and selects SearchText }

procedure TfrmSearchForm.DoOnSelectionChanged(Sender: TObject);
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
  B := not rbAllViews.Checked;
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

  imgF2Key.Visible := GetFirstParentForm(Screen.ActiveControl) = Self;
end;
{$ENDREGION}

end.
