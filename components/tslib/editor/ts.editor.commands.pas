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

unit ts.Editor.Commands;

{$MODE Delphi}

{ Implements IEditorCommands which represents all commands that can be executed
  on the active editor view. }
{ TODO: Intended to implement IEditorCommands in the future. }

interface

uses
  Classes, SysUtils,

  ts.Editor.Interfaces;

type

  { TEditorCommands }

  TEditorCommands = class(TComponent)
  private
    function GetManager: IEditorManager;
    function GetSelection: IEditorSelection;
    function GetView: IEditorView;

  protected
    property Selection: IEditorSelection
      read GetSelection;

    property View: IEditorView
      read GetView;

    property Manager: IEditorManager
      read GetManager;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$region 'construction and destruction' /fold}
procedure TEditorCommands.AfterConstruction;
begin
  inherited AfterConstruction;
end;
{$endregion}

{$region 'property access mehods' /fold}
function TEditorCommands.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TEditorCommands.GetSelection: IEditorSelection;
begin
  Result := Manager.ActiveView.Selection;
end;

function TEditorCommands.GetView: IEditorView;
begin
  Result := Manager.ActiveView;
end;
{$endregion}

end.

