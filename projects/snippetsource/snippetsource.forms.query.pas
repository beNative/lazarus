{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit SnippetSource.Forms.Query;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,

  ts.Editor.Interfaces;

type

  { TfrmQuery }

  TfrmQuery = class(TForm)
    pnlQueryEditor : TPanel;
    pnlResultSet   : TPanel;
    splHorizontal  : TSplitter;
  private
      FManager : IEditorManager;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.lfm}

uses
  ts.Editor.Factories;

procedure TfrmQuery.AfterConstruction;
begin
  inherited AfterConstruction;
  FManager := TEditorFactories.CreateManager(Self, nil);
  V := TEditorFactories.CreateView(pnlQueryEditor, FManager, 'QueryEditor');
  V.HighlighterName := 'SQL';
  V.Editor.PopupMenu := FManager.Menus.EditorPopupMenu;
end;

procedure TfrmQuery.BeforeDestruction;
begin
  FManager := nil;
  inherited BeforeDestruction;
end;

end.

