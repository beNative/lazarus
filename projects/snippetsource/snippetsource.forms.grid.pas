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

unit SnippetSource.Forms.Grid;

{$MODE DELPHI}

interface

{ Used internally for debugging. }

uses
  Classes, SysUtils, db, ExtendedNotebook, Forms, Controls, Graphics, Dialogs,
  DBGrids, DbCtrls, ExtCtrls, ComCtrls, ActnList, StdCtrls, SQLDB,

  ts.Core.ComponentInspector;

type
  TfrmGrid = class(TForm)
    {$REGION 'designer controls'}
    aclMain               : TActionList;
    actApplyUpdates       : TAction;
    actCommit             : TAction;
    actInspectConnection  : TAction;
    actInspectDataSet     : TAction;
    actInspectDBGrid      : TAction;
    actInspectFields      : TAction;
    actInspectTransaction : TAction;
    btnApplyUpdates       : TToolButton;
    btnCommit             : TToolButton;
    btnInspectConnection  : TToolButton;
    btnInspectDataSet     : TToolButton;
    btnInspectDBGrid      : TToolButton;
    btnInspectFields      : TToolButton;
    btnInspectTransaction : TToolButton;
    dscMain               : TDataSource;
    enbMain               : TExtendedNotebook;
    grdMain               : TDBGrid;
    navMain               : TDBNavigator;
    pnlChangeCount        : TPanel;
    pnlMain               : TPanel;
    pnlState              : TPanel;
    pnlStatusBar          : TPanel;
    pnlUpdateMode         : TPanel;
    pnlUpdateStatus       : TPanel;
    tbrMain               : TToolBar;
    tsGrid                : TTabSheet;
    tsSettings            : TTabSheet;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actApplyUpdatesExecute(Sender: TObject);
    procedure actCommitExecute(Sender: TObject);
    procedure actInspectConnectionExecute(Sender: TObject);
    procedure actInspectDataSetExecute(Sender: TObject);
    procedure actInspectDBGridExecute(Sender: TObject);
    procedure actInspectFieldsExecute(Sender: TObject);
    procedure actInspectTransactionExecute(Sender: TObject);
    {$ENDREGION}

  private
    function GetDataSet: TSQLQuery;

  public
    constructor Create(AOwner: TComponent; ADataSet: TDataSet); reintroduce;
    procedure UpdateActions; override;

    procedure UpdateStatusBar;

    property DataSet: TSQLQuery
      read GetDataSet;

  end;

procedure ShowGridForm(ADataSet: TDataSet);

implementation

{$R *.lfm}

uses
  TypInfo,

  ts.Core.Utils;

var
  GridForm : TfrmGrid;

{$REGION 'interfaced routines'}
procedure ShowGridForm(ADataSet: TDataSet);
begin
  if not Assigned(GridForm) then
    GridForm := TfrmGrid.Create(Application, ADataSet);
  GridForm.Show;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
constructor TfrmGrid.Create(AOwner: TComponent; ADataSet: TDataSet);
begin
  inherited Create(AOwner);
  dscMain.DataSet := ADataSet;
  grdMain.AutoAdjustColumns;
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

procedure TfrmGrid.actInspectConnectionExecute(Sender: TObject);
begin
  InspectComponent(DataSet.SQLConnection);
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
  LFields : array of TComponent;
  I       : Integer;
begin
  SetLength(LFields, DataSet.FieldCount);
  for I := 0 to DataSet.FieldCount - 1 do
  begin
    LFields[I] := DataSet.Fields[I];
  end;
  InspectComponents(LFields);
end;

procedure TfrmGrid.actInspectTransactionExecute(Sender: TObject);
begin
  InspectComponent(DataSet.SQLTransaction);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmGrid.UpdateActions;
begin
  inherited UpdateActions;
  UpdateStatusBar;
end;

procedure TfrmGrid.UpdateStatusBar;
var
  S : string;
begin
  if DataSet.Active then
  begin
    S := GetEnumName(TypeInfo(TDataSetState), Ord(DataSet.State));
    pnlState.Caption := Format('State = %s', [S]);
    S := GetEnumName(TypeInfo(TUpdateStatus), Ord(DataSet.UpdateStatus));
    pnlUpdateStatus.Caption := Format('UpdateStatus = %s', [S]);
    S := GetEnumName(TypeInfo(TUpdateMode), Ord(DataSet.UpdateMode));
    pnlUpdateMode.Caption := Format('UpdateMode = %s', [S]);
    pnlChangeCount.Caption := Format('ChangeCount = %d', [DataSet.ChangeCount]);
  end
  else
  begin
    pnlState.Caption        := 'Closed';
    pnlUpdateStatus.Caption := '';
    pnlUpdateMode.Caption   := '';
    pnlChangeCount.Caption  := '';
  end;
  OptimizeWidth(pnlState);
  OptimizeWidth(pnlUpdateStatus);
  OptimizeWidth(pnlUpdateMode);
  OptimizeWidth(pnlChangeCount);
end;
{$ENDREGION}

end.

