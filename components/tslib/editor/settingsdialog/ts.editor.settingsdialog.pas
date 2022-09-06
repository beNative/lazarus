{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

