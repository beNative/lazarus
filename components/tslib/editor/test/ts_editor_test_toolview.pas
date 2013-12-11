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

unit ts_Editor_Test_ToolView;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls,

  ts.Components.GridView, ts.Components.Inspector,

  ts_Editor_ToolView_Base;

type

  { TfrmTest }

  TfrmTest = class(TCustomEditorToolView)
    procedure FGridViewGetCellText(Sender: TObject; Cell: TGridCell;
      var Value: string);
    procedure FInspectorGetCellText(Sender: TObject; Cell: TGridCell;
      var Value: string);
  strict private
    //FInspector : TExInspector;
    //FGridView  : TGridView;
  public
    procedure AfterConstruction; override;
  end;

implementation

{$R *.lfm}

uses
  ts_Core_ComponentInspector;

{ TfrmTest }

procedure TfrmTest.FGridViewGetCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
begin
  Value := 'Test';
end;

procedure TfrmTest.FInspectorGetCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
begin
  Value := 'Test';
end;

procedure TfrmTest.AfterConstruction;
begin
  inherited AfterConstruction;
  //FInspector := TExInspector.Create(Self);
  //FInspector.Parent := Self;
  //FInspector.Align := alClient;
  //FInspector.OnGetCellText := FInspectorGetCellText;

  //FGridView := TGridView.Create(Self);
  //FGridView.Parent := Self;
  //FGridView.Align := alClient;
  //FGridView.OnGetCellText := FGridViewGetCellText;

  //FInspector.Columns.Add;
  //FInspector.Columns.Add;

  //FInspector.Rows.Count := 1;
  //InspectComponent(FGridView);

end;

end.

