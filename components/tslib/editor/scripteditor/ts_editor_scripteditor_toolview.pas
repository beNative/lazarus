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

  ts_Editor_ToolView_Base, ts.Editor.Interfaces;

type

  { TfrmScriptEditor }

  TfrmScriptEditor = class(TCustomEditorToolView, IEditorToolView)
    pnlLeft   : TPanel;
    pnlRight  : TPanel;
    pnlBottom : TPanel;
    pnlMain   : TPanel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FEditor  : IEditorView;
    FManager : IEditorManager;
  public
    { public declarations }
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{$R *.lfm}

uses
  ts.Editor.Helpers;

{ TfrmScriptEditor }

{$region 'construction and destruction' /fold}
procedure TfrmScriptEditor.AfterConstruction;
begin
  inherited AfterConstruction;
  //FEditor := CreateEditorView(pnlLeft, 'ScriptEditor', '', 'PAS');
  FManager := CreateEditorManager(Self);
end;

procedure TfrmScriptEditor.BeforeDestruction;
begin
  FEditor := nil;
  FManager := nil;
  inherited BeforeDestruction;
end;
{$endregion}

procedure TfrmScriptEditor.FormShow(Sender: TObject);
begin
  if not Assigned(FEditor) then
    FEditor := CreateEditorView(pnlLeft, FManager, 'ScriptEditor', '', 'PAS');

  if FileExists('notepas.dws') then
    FEditor.Load('notepas.dws');
end;

end.

