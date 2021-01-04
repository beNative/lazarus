{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.ScriptEditor.ToolView;

{$MODE DELPHI}

interface

uses
  Controls, ExtCtrls, ActnList, ComCtrls,

  ts.Editor.ToolView.Base, ts.Editor.Interfaces;

type
  TfrmScriptEditor = class(TCustomEditorToolView, IEditorToolView)
    aclMain    : TActionList;
    actExecute : TAction;
    btnExecute : TToolButton;
    imlMain    : TImageList;
    pnlBottom  : TPanel;
    pnlLeft    : TPanel;
    pnlMain    : TPanel;
    pnlRight   : TPanel;
    tlbMain    : TToolBar;

    procedure actExecuteExecute(Sender: TObject);

    procedure FormShow(Sender: TObject);

  private
    FScriptEditor        : IEditorView;
    FScriptEditorManager : IEditorManager;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.lfm}

uses
  LazFileUtils,

  ts.Editor.Factories;

{$REGION 'construction and destruction'}
procedure TfrmScriptEditor.AfterConstruction;
begin
  inherited AfterConstruction;
  // The script editor uses a dedicated editor manager
  FScriptEditorManager :=
    TEditorFactories.CreateManager(Self, Manager.Settings);
end;

procedure TfrmScriptEditor.BeforeDestruction;
begin
  FScriptEditor := nil;
  FScriptEditorManager := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmScriptEditor.FormShow(Sender: TObject);
begin
  if not Assigned(FScriptEditor) then
  begin
    FScriptEditor := TEditorFactories.CreateView(
      pnlLeft,
      FScriptEditorManager,
      'ScriptEditor',
      '',
      'PAS'
    );
  end;

  if FileExistsUTF8('notepas.dws') then
    FScriptEditor.Load('notepas.dws');
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmScriptEditor.actExecuteExecute(Sender: TObject);
begin
  // TODO
end;
{$ENDREGION}

end.

