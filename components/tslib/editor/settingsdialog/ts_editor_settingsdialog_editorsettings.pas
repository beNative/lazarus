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

unit ts_Editor_SettingsDialog_EditorSettings;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  StdCtrls,

  ts_Editor_SettingsDialog_Base;

type

  { TfrmEditorSettingsDialog }

  TfrmEditorSettingsDialog = class(TCustomSettingsDialog)
    chkWantTabs: TTICheckBox;
    edtBlockIndent: TTISpinEdit;
    edtBlockTabIndent: TTISpinEdit;
    edtRightEdge: TTISpinEdit;
    edtExtraLineSpacing: TTISpinEdit;
    edtExtraCharSpacing: TTISpinEdit;
    chkReadOnly: TTICheckBox;
    btnRightEdgeColor: TTIColorButton;
    edtTabWidth: TTISpinEdit;
    lblExtraLineSpacing: TLabel;
    lblExtraCharSpacing: TLabel;
    lblBlockIndent: TLabel;
    lblBlockTabIndent: TLabel;
    lblRightEdge: TLabel;
    Label6: TLabel;
    trbBlockTabIndent: TTITrackBar;
    trbRightEdge: TTITrackBar;
    trbExtraCharSpacing: TTITrackBar;
    trbBlockIndent: TTITrackBar;
    trbTabWidth: TTITrackBar;
    trbExtraLineSpacing: TTITrackBar;
  strict private

  public
  procedure AfterConstruction; override;

  end;

implementation

{$R *.lfm}

{ TfrmEditorSettingsDialog }

procedure TfrmEditorSettingsDialog.AfterConstruction;
begin
  //LinkProperty(Settings, edtBlockIndent.Link);
  //LinkProperty(Settings, edtBlockTabIndent.Link);
  //LinkProperty(Settings, edtExtraCharSpacing.Link);
  //LinkProperty(Settings, edtExtraLineSpacing.Link);
  //LinkProperty(Settings, chkReadOnly.Link);
  //LinkProperty(Settings, edtRightEdge.Link);
  //LinkProperty(Settings, btnRightEdgeColor.Link);
  //LinkProperty(Settings, chkWantTabs.Link);
  //LinkProperty(Settings, edtTabWidth.Link);
  //LinkProperty(Settings, trbTabWidth.Link);
  inherited AfterConstruction;
  AutoLinkChildControls(Settings, Self);
end;

end.

