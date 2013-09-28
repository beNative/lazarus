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

unit ts.Editor.SettingsDialog.Colors;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,

  VirtualTrees,

  ts.Core.TreeViewPresenter, ts.Core.DataTemplates;

type

  { TfrmColorSettings }

  TfrmColorSettings = class(TForm)
    pnlMain: TPanel;
  private
    FTVP: TTreeViewPresenter;
    FVST: TVirtualStringTree;
  public
    procedure AfterConstruction; override;
  end;

implementation

{$R *.lfm}

uses
  ts.Core.Helpers, ts.Core.Value, ts.Core.ColumnDefinitions,
  ts.Core.ColumnDefinitionsDataTemplate;

type

  { THighlighterAttributesTemplate }

  THighlighterAttributesTemplate = class(TColumnDefinitionsDataTemplate)
    function GetValueForProperty(
      const Item          : TObject;
      const APropertyName : string
    ): TValue; override;
  end;

{ THighlighterAttributesTemplate }

function THighlighterAttributesTemplate.GetValueForProperty(
  const Item: TObject; const APropertyName: string): TValue;
begin
  Result := inherited GetValueForProperty(Item, APropertyName);
end;

{ TfrmColorSettings }

procedure TfrmColorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FVST := CreateVST(Self, pnlMain);
  FTVP := CreateTVP(Self, FVST);
  //
end;

end.

