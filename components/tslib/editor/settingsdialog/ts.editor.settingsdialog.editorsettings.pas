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

unit ts.Editor.SettingsDialog.EditorSettings;

{$MODE DELPHI}

interface

uses
  RTTICtrls, StdCtrls, ExtCtrls,

  ts.Editor.SettingsDialog.Base;

type
  TfrmEditorSettingsDialog = class(TCustomSettingsDialog)
    chkAlwaysVisibleCaret    : TTICheckBox;
    chkAutoHideCursor        : TTICheckBox;
    chkAutoHideCursor1       : TTICheckBox;
    chkAutoIndent            : TTICheckBox;
    chkAutoIndentOnPaste     : TTICheckBox;
    chkBracketHighlight      : TTICheckBox;
    chkCaretSkipsSelection   : TTICheckBox;
    chkCaretSkipsTab         : TTICheckBox;
    chkDragDropEditing       : TTICheckBox;
    chkEnhanceEndKey         : TTICheckBox;
    chkEnhanceHomeKey        : TTICheckBox;
    chkFoldedCopyPaste       : TTICheckBox;
    chkOverwriteBlock        : TTICheckBox;
    chkPersistentBlock       : TTICheckBox;
    chkShowRightEdge         : TTICheckBox;
    chkShowSpecialCharacters : TTICheckBox;
    chkSmartTabs             : TTICheckBox;
    chkTabIndent             : TTICheckBox;
    chkTabsToSpaces          : TTICheckBox;
    chkTrimTrailingSpaces    : TTICheckBox;
    chkWantTabs              : TTICheckBox;
    edtBlockIndent           : TTISpinEdit;
    edtBlockTabIndent        : TTISpinEdit;
    edtExtraCharSpacing      : TTISpinEdit;
    edtExtraLineSpacing      : TTISpinEdit;
    edtRightEdge             : TTISpinEdit;
    edtTabWidth              : TTISpinEdit;
    chkBoolean               : TGroupBox;
    grpNumeric               : TGroupBox;
    grpTabs                  : TGroupBox;
    grpKeys                  : TGroupBox;
    grpWhitespace            : TGroupBox;
    grpSelection             : TGroupBox;
    grpBehaviour             : TGroupBox;
    grpIndentation           : TGroupBox;
    grpAppearance            : TGroupBox;
    lblBlockIndent           : TLabel;
    lblBlockTabIndent        : TLabel;
    Label6                   : TLabel;
    lblExtraCharSpacing      : TLabel;
    lblExtraLineSpacing      : TLabel;
    lblRightEdge             : TLabel;
    lblTabWidth              : TLabel;
    pnlMain                  : TPanel;
    trbBlockIndent           : TTITrackBar;
    trbBlockTabIndent        : TTITrackBar;
    trbExtraCharSpacing      : TTITrackBar;
    trbExtraLineSpacing      : TTITrackBar;
    trbRightEdge             : TTITrackBar;
    trbTabWidth              : TTITrackBar;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.lfm}

{$REGION 'construction and destruction'}
procedure TfrmEditorSettingsDialog.AfterConstruction;
begin
  inherited AfterConstruction;
  AutoLinkChildControls(Settings.EditorOptions, Self);
end;
{$ENDREGION}

end.

