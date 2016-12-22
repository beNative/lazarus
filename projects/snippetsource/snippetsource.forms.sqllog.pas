{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit SnippetSource.Forms.SQLLog;

{$MODE Delphi}

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
    //procedure LogEvent(Event: TZLoggingEvent);

    property DataSet: TDataSet
      read GetDataSet write SetDataSet;
  end; 

implementation

uses
  ts.Editor.Factories;

{$R *.lfm}

{$region 'construction and destruction' /fold}
procedure TfrmSQLLog.AfterConstruction;
var
  V: IEditorView;
begin
  inherited AfterConstruction;
  FManager := TEditorFactories.CreateManager(Self, nil);
  V := TEditorFactories.CreateView(Self, FManager, 'Editor2');
  V.HighlighterName := 'SQL';
//  DriverManager.AddLoggingListener(Self);
  V.Editor.PopupMenu := FManager.Menus.EditorPopupMenu;
end;

procedure TfrmSQLLog.BeforeDestruction;
begin
  FManager := nil;
  //DriverManager.RemoveLoggingListener(Self);
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'property access mehods' /fold}
function TfrmSQLLog.GetDataSet: TDataSet;
begin
  Result := dscMain.DataSet;
end;

procedure TfrmSQLLog.SetDataSet(AValue: TDataSet);
begin
  dscMain.DataSet := AValue;
end;
{$endregion}

{$region 'public methods' /fold}
//procedure TfrmSQLLog.LogEvent(Event: TZLoggingEvent);
//begin
//  FManager.Views.ViewByName['Editor2'].Text :=
//    FManager.Views.ViewByName['Editor2'].Text + Event.AsString + #13#10;
//end;
{$endregion}

end.

