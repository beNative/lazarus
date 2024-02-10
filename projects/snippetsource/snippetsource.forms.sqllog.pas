{
  Copyright (C) 2013-2024 Tim Sinaeve tim.sinaeve@gmail.com

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

unit SnippetSource.Forms.SQLLog;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,

  ts.Editor.Interfaces;

type
  TfrmSQLLog = class(TForm)
    dscMain : TDatasource;
    grdMain : TDBGrid;

  private
    FManager : IEditorManager;

    function GetDataSet: TDataSet;
    procedure SetDataSet(AValue: TDataSet);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property DataSet: TDataSet
      read GetDataSet write SetDataSet;
  end; 

implementation

uses
  ts.Editor.Factories;

{$R *.lfm}

{$REGION 'construction and destruction'}
procedure TfrmSQLLog.AfterConstruction;
var
  V: IEditorView;
begin
  inherited AfterConstruction;
  FManager := TEditorFactories.CreateManager(Self, nil);
  V := TEditorFactories.CreateView(Self, FManager, 'Editor2');
  V.HighlighterName := 'SQL';
  V.Editor.PopupMenu := FManager.Menus.EditorPopupMenu;
end;

procedure TfrmSQLLog.BeforeDestruction;
begin
  FManager := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmSQLLog.GetDataSet: TDataSet;
begin
  Result := dscMain.DataSet;
end;

procedure TfrmSQLLog.SetDataSet(AValue: TDataSet);
begin
  dscMain.DataSet := AValue;
end;
{$ENDREGION}

end.

