{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.Test.ToolView;

{$MODE DELPHI}

interface

uses
  ExtCtrls, ComCtrls,

  ts.Components.GridView,

  ts.Editor.ToolView.Base;

type
  TfrmTest = class(TCustomEditorToolView)
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
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

