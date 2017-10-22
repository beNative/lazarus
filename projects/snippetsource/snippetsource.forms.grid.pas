{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit SnippetSource.Forms.Grid;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  DbCtrls, ExtCtrls, ComCtrls, ActnList, sqldb,

  ts.Components.DBGridView, ts.Core.ComponentInspector, Grids;

type
  TfrmGrid = class(TForm)
    aclMain           : TActionList;
    actApplyUpdates   : TAction;
    actCommit         : TAction;
    actInspectDataSet : TAction;
    actInspectDBGrid  : TAction;
    actInspectFields  : TAction;
    dscMain           : TDataSource;
    grdMain           : TDBGrid;
    navMain           : TDBNavigator;
    pnlState: TPanel;
    pnlUpdateStatus: TPanel;
    pnlStatusBar: TPanel;
    pnlDBGrid         : TPanel;
    pnlChangeCount: TPanel;
    pnlUpdateMode: TPanel;
    ToolBar1          : TToolBar;
    btnApplyUpdates   : TToolButton;
    btnCommit         : TToolButton;
    btnInspectDataSet : TToolButton;
    btnInspectDBGrid  : TToolButton;
    btnInspectFields  : TToolButton;

    procedure actApplyUpdatesExecute(Sender: TObject);
    procedure actCommitExecute(Sender: TObject);
    procedure actInspectDataSetExecute(Sender: TObject);
    procedure actInspectDBGridExecute(Sender: TObject);
    procedure actInspectFieldsExecute(Sender: TObject);


  private
    function GetDataSet: TSQLQuery;
    //FGrid: TDBGridView;

  public
    constructor Create(AOwner: TComponent; ADataSet: TDataSet); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure UpdateActions; override;

    procedure UpdateStatusBar;

    property DataSet: TSQLQuery
      read GetDataSet;

  end;

procedure ShowGridForm(ADataSet: TDataSet);

implementation

uses
  TypInfo,

  ts.Core.Utils, ts.Core.SharedLogger;

var
  GridForm : TfrmGrid;

procedure ShowGridForm(ADataSet: TDataSet);
begin
  if not Assigned(GridForm) then
    GridForm := TfrmGrid.Create(Application, ADataSet);
  GridForm.Show;
end;

{$R *.lfm}

{$REGION 'construction and destruction'}
constructor TfrmGrid.Create(AOwner: TComponent; ADataSet: TDataSet);
begin
  inherited Create(AOwner);
  dscMain.DataSet := ADataSet;
  grdMain.AutoAdjustColumns;
end;

procedure TfrmGrid.AfterConstruction;
begin
  inherited AfterConstruction;



  //InspectComponent(FGrid);
end;

procedure TfrmGrid.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;

procedure TfrmGrid.UpdateActions;
begin
  inherited UpdateActions;
  UpdateStatusBar;
end;

procedure TfrmGrid.UpdateStatusBar;
var
  S : string;
begin
  S := GetEnumName(TypeInfo(TDataSetState), Ord(DataSet.State));
//  S := System.Copy(S, 3, Length(S));
//  Logger.Watch('State', S);
  pnlState.Caption := Format('State = %s', [S]);

  S := GetEnumName(TypeInfo(TUpdateStatus), Ord(DataSet.UpdateStatus));
//  S := System.Copy(S, 3, Length(S));
  pnlUpdateStatus.Caption := Format('UpdateStatus = %s', [S]);
//  Logger.Watch('UpdateStatus', S);

  S := GetEnumName(TypeInfo(TUpdateMode), Ord(DataSet.UpdateMode));
//  S := System.Copy(S, 3, Length(S));
//  Logger.Watch('UpdateMode', S);
  pnlUpdateMode.Caption := Format('UpdateMode = %s', [S]);

  pnlChangeCount.Caption := Format('ChangeCount = %d', [DataSet.ChangeCount]);

  OptimizeWidth(pnlState);
  OptimizeWidth(pnlUpdateStatus);
  OptimizeWidth(pnlUpdateMode);
  OptimizeWidth(pnlChangeCount);
end;

{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmGrid.GetDataSet: TSQLQuery;
begin
  Result := dscMain.DataSet as TSQLQuery;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmGrid.actApplyUpdatesExecute(Sender: TObject);
begin
  DataSet.ApplyUpdates;
end;

procedure TfrmGrid.actCommitExecute(Sender: TObject);
begin
  DataSet.SQLTransaction.Commit;
end;

procedure TfrmGrid.actInspectDataSetExecute(Sender: TObject);
begin
  InspectComponent(DataSet);
end;

procedure TfrmGrid.actInspectDBGridExecute(Sender: TObject);
begin
  InspectComponent(grdMain);
end;

procedure TfrmGrid.actInspectFieldsExecute(Sender: TObject);
var
  Fields : array of TComponent;
  I      : Integer;
begin
  SetLength(Fields, DataSet.FieldCount);
  for I := 0 to DataSet.FieldCount - 1 do
  begin
    Fields[I] := DataSet.Fields[I];
  end;
  InspectComponents(Fields);
end;
{$ENDREGION}

end.

