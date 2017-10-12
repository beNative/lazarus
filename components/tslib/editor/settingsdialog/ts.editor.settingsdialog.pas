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

unit ts.Editor.SettingsDialog;

{$MODE DELPHI}

interface

uses
  SysUtils, Forms, ExtCtrls, StdCtrls,

  ts.Components.VirtualPages,

  ts.Editor.Interfaces;

type
  TEditorSettingsDialog = class(TForm, IEditorManager)
    lbxPages    : TListBox;
    pnlLeft     : TPanel;
    pnlRight    : TPanel;
    splVertical : TSplitter;

    procedure lbxPagesClick(Sender: TObject);

  private
    FVPM : TVirtualPageManager;
    function GetManager: IEditorManager;

  public
    procedure AfterConstruction; override;

    property Manager: IEditorManager
      read GetManager implements IEditorManager;
  end;

implementation

{$R *.lfm}

uses
  ts.Editor.SettingsDialog.FileAssociations,
  ts.Editor.SettingsDialog.FileTypes,
  ts.Editor.SettingsDialog.KeyMappings,
  ts.Editor.SettingsDialog.Highlighters,
  ts.Editor.SettingsDialog.ApplicationSettings,
  ts.Editor.SettingsDialog.EditorSettings;

{$REGION 'construction and destruction'}
procedure TEditorSettingsDialog.AfterConstruction;
begin
  inherited AfterConstruction;
  FVPM := TVirtualPageManager.Create(Self);
  FVPM.DisplayOptions.Parent := pnlRight;
  FVPM.Pages.Add(
    'ApplicationSettings',
    'Application settings',
    TfrmApplicationSettings,
    []
  );
  FVPM.Pages.Add(
    'EditorSettings',
    'Editor settings',
    TfrmEditorSettingsDialog,
    []
    );
  FVPM.Pages.Add(
    'FileAssociations',
    'File associations',
    TfrmOptionsAssociate,
    []
  );
  FVPM.Pages.Add(
    'FileTypes',
    'File types',
    TfrmFileTypeSettings,
    []
  );
  FVPM.Pages.Add(
    'KeyMappings',
    'Key mappings',
    TfrmKeyMappings,
    []
  );
  FVPM.Pages.Add(
    'Highlighters',
    'Highlighters',
    TfrmOptionsThemes,
    []
  );
  //TfrmOptionsThemes
  FVPM.Pages.AssignTo(lbxPages.Items);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TEditorSettingsDialog.lbxPagesClick(Sender: TObject);
begin
  FVPM.PageIndex := lbxPages.ItemIndex;
end;

function TEditorSettingsDialog.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;
{$ENDREGION}

end.

