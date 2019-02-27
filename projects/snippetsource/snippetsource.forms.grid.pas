{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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
  Classes, SysUtils, db, FileUtil, ExtendedTabControls, ExtendedNotebook,
  RTTICtrls, Forms, Controls, Graphics, Dialogs, DBGrids,
  DbCtrls, ExtCtrls, ComCtrls, ActnList, StdCtrls,
  sqldb,

  SnippetSource.Interfaces,

  ts.Components.DBGridView, ts.Core.ComponentInspector;

type
  TfrmGrid = class(TForm)
    {$REGION 'designer controls'}
    aclMain               : TActionList;
    actApplyUpdates       : TAction;
    actCommit             : TAction;
    actInspectDataSet     : TAction;
    actInspectDBGrid      : TAction;
    actInspectFields      : TAction;
    actInspectTransaction : TAction;
    actInspectConnection  : TAction;
    dscMain               : TDataSource;
    enbMain               : TExtendedNotebook;
    grdMain               : TDBGrid;
    navMain               : TDBNavigator;
    pnlState              : TPanel;
    pnlUpdateStatus       : TPanel;
    pnlStatusBar          : TPanel;
    pnlMain               : TPanel;
    pnlChangeCount        : TPanel;
    pnlUpdateMode         : TPanel;
    btnInspectTransaction : TToolButton;
    btnInspectConnection  : TToolButton;
    tsGrid                : TTabSheet;
    tsSettings            : TTabSheet;
    tbrMain               : TToolBar;
    btnApplyUpdates       : TToolButton;
    btnCommit             : TToolButton;
    btnInspectDataSet     : TToolButton;
    btnInspectDBGrid      : TToolButton;
    btnInspectFields      : TToolButton;
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

  ts.Core.Utils, ts.Core.SharedLogger;

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

