{
  Copyright (C) 2013-2023 Tim Sinaeve tim.sinaeve@gmail.com

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

unit SnippetSource.Forms.Query;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBGrids,
  ActnList,

  ts.Editor.Interfaces,

  SnippetSource.Interfaces;

type
  TfrmQuery = class(TForm)
    actExecute     : TAction;
    alMain         : TActionList;
    dscMain        : TDataSource;
    grdMain        : TDBGrid;
    pnlQueryEditor : TPanel;
    pnlResultSet   : TPanel;
    splHorizontal  : TSplitter;

    procedure actExecuteExecute(Sender: TObject);

  private
    FManager : IEditorManager;
    FEditor  : IEditorView;
    FQuery   : IQuery;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    constructor Create(
      AOwner         : TComponent;
      AEditorManager : IEditorManager;
      AQuery         : IQuery
    ); reintroduce;

  end;

implementation

{$R *.lfm}

uses
  ts.Editor.Factories;

{$REGION 'construction and destruction'}
constructor TfrmQuery.Create(AOwner: TComponent; AEditorManager: IEditorManager;
  AQuery: IQuery);
begin
  inherited Create(AOwner);
  FManager := AEditorManager;
  FQuery   := AQuery;
end;

procedure TfrmQuery.AfterConstruction;
begin
  inherited AfterConstruction;
  FEditor := TEditorFactories.CreateView(pnlQueryEditor, FManager, 'QueryEditor');
  FEditor.HighlighterName := 'SQL';
  FEditor.Editor.PopupMenu := FManager.Menus.EditorPopupMenu;
end;

destructor TfrmQuery.Destroy;
begin
  FManager.Views.Delete(FEditor);
  FEditor  := nil;
  FManager := nil;
  FQuery   := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmQuery.actExecuteExecute(Sender: TObject);
begin
  FQuery.Execute(FEditor.Text);
  dscMain.DataSet := FQuery.Query;
end;
{$ENDREGION}

end.

