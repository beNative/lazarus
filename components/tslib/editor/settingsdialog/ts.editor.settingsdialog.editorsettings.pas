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

unit ts.Editor.SettingsDialog.EditorSettings;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls,

  ts.Editor.SettingsDialog.Base;

type

  { TfrmEditorSettingsDialog }

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

{ TfrmEditorSettingsDialog }

{$REGION 'construction and destruction' /FOLD}
procedure TfrmEditorSettingsDialog.AfterConstruction;
begin
  inherited AfterConstruction;
  AutoLinkChildControls(Settings.EditorOptions, Self);
end;
{$ENDREGION}

end.

