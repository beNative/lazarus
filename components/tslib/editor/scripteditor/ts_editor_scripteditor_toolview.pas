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

unit ts_Editor_ScriptEditor_ToolView;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ActnList, ComCtrls,

  ts_Editor_ToolView_Base, ts.Editor.Interfaces;

type

  { TfrmScriptEditor }

  TfrmScriptEditor = class(TCustomEditorToolView, IEditorToolView)
    aclMain: TActionList;
    actExecute: TAction;
    imlMain: TImageList;
    pnlLeft   : TPanel;
    pnlRight  : TPanel;
    pnlBottom : TPanel;
    pnlMain   : TPanel;
    ToolBar1: TToolBar;
    btnExecute: TToolButton;

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
  ts.Editor.Factories;

{ TfrmScriptEditor }

{$region 'construction and destruction' /fold}
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
{$endregion}

{$region 'event handlers' /fold}
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

  if FileExists('notepas.dws') then
    FScriptEditor.Load('notepas.dws');
end;

procedure TfrmScriptEditor.actExecuteExecute(Sender: TObject);
begin
  //
end;

{$endregion}

end.

