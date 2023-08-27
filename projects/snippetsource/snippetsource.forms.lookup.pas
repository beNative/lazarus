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

unit SnippetSource.Forms.Lookup;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, DB, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, DBGrids, ExtCtrls,

  ts.Editor.Interfaces,

  SnippetSource.Interfaces;

type

  { TfrmLookup }

  TfrmLookup = class(TForm)
    aclMain   : TActionList;
    actSearch : TAction;
    dscMain   : TDatasource;
    edtLookup : TEdit;
    grdLookup : TDBGrid;
    pnlTop    : TPanel;
    sbrMain   : TStatusBar;

    {$REGION 'action handlers'}
    procedure actSearchExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure dscMainDataChange(Sender: TObject; Field: TField);
    procedure edtLookupKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtLookupKeyPress(Sender: TObject; var Key: Char);
    procedure edtLookupKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure grdLookupKeyPress(Sender: TObject; var Key: Char);
    procedure grdLookupKeyUp(
      Sender  : TObject;
      var Key : Word;
      Shift   : TShiftState
    );
    procedure grdLookupMouseUp(
      Sender : TObject;
      Button : TMouseButton;
      Shift  : TShiftState;
      X, Y   : Integer
    );
    {$ENDREGION}

  private
    FVKPressed : Boolean;
    FEditor    : IEditorView;
    FData      : ISearch;
    FUpdate    : Boolean;

    function GetDataSet: TDataSet;
    function GetSearchDataSet: TDataSet;

    procedure Modified;

  public
    constructor Create(
      AOwner  : TComponent;
      AEditor : IEditorView;
      ALookup : ISearch
    ); reintroduce; virtual;
    destructor Destroy; override;

    procedure UpdateActions; override;

    procedure Execute;

    property DataSet: TDataSet
      read GetDataSet;

    property SearchDataSet: TDataSet
      read GetSearchDataSet;
  end; 

procedure Lookup(AEditor: IEditorView; ALookup: ISearch);

implementation

{$R *.lfm}

uses
  LCLType, Windows,

  ts.Core.Logger;

var
  FLookupForm : TfrmLookup;

procedure Lookup(AEditor: IEditorView; ALookup: ISearch);
begin
  if not Assigned(FLookupForm) then
    FLookupForm := TfrmLookup.Create(Application, AEditor, ALookup);
  FLookupForm.Show;
end;

{$REGION 'construction and destruction'}
constructor TfrmLookup.Create(AOwner: TComponent; AEditor: IEditorView;
  ALookup: ISearch);
begin
  inherited Create(AOwner);
  FData           := ALookup;
  FEditor         := AEditor;
  dscMain.DataSet := ALookup.SearchDataSet;
end;

destructor TfrmLookup.Destroy;
begin
  FEditor := nil;
  FData   := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmLookup.GetDataSet: TDataSet;
begin
  Result := (FData as IDataSet).DataSet;
end;

function TfrmLookup.GetSearchDataSet: TDataSet;
begin
  Result := dscMain.DataSet;
end;

{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmLookup.actSearchExecute(Sender: TObject);
begin
  Execute;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmLookup.dscMainDataChange(Sender: TObject; Field: TField);
begin
  if not Assigned(Field) then
  begin
    DataSet.Locate('Id' , DataSet.FieldByName('Id').AsInteger,[]);
    //if not FData.SearchDataSet.IsEmpty then
    //  FEditor.SearchAndSelectText(edtLookup.Text);
  end;
end;

procedure TfrmLookup.edtLookupKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Logger.Enter(Self, 'edtLookupKeyDown');
  if not (
    (ssAlt in Shift) or
    (ssShift in Shift) or
    (Key in [VK_DELETE, VK_LEFT, VK_RIGHT, VK_HOME, VK_END, VK_SHIFT, VK_CONTROL]) or
    ((Key in [VK_INSERT, VK_DELETE, VK_LEFT, VK_RIGHT, VK_HOME, VK_END]) and
    ((ssShift in Shift) or (ssCtrl in Shift)))
  ) then
  begin
    FVKPressed := True;
    //Key := 0;
  end;

  //if DataSet.IsEmpty then
  //  Exit;

    if Key = VK_Down then
    begin
      //if (LV.Items.IndexOf(LV.ItemFocused) + 1) < LV.Items.Count then
      //  LV.ItemFocused := LV.Items[(LV.Items.IndexOf(LV.ItemFocused) + 1)];
    end
    else if Key = VK_Up then
    begin
      //if (LV.Items.IndexOf(LV.ItemFocused) - 1) >= 0 then
      //  LV.ItemFocused := LV.Items[(LV.Items.IndexOf(LV.ItemFocused) - 1)];
    end
    else if Key = VK_Home then
    begin
  //    LV.ItemFocused := LV.Items[0];
    end
    else if Key = VK_End then
    begin
  //    LV.ItemFocused := LV.Items[LV.Items.Count - 1];
    end;

    //if LV.ItemFocused<>nil then
    //begin
    //  LV.Selected := LV.ItemFocused;
    //  if Assigned(LV.Selected) then
    //    LV.Selected.MakeVisible(True);
    //end;
    Logger.Leave(Self, 'edtLookupKeyDown');
end;

procedure TfrmLookup.edtLookupKeyPress(Sender: TObject; var Key: Char);
begin
  Logger.Enter(Self, 'edtLookupKeyPress');
  case Ord(Key) of
    VK_RETURN:
      begin
        //JumpToSelection;
        Key := #0;
      end;
    VK_ESCAPE:
      begin
        ModalResult := mrCancel;
        Key := #0;
      end;
    end;
  Logger.Leave(Self, 'edtLookupKeyPress');
end;

procedure TfrmLookup.edtLookupKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Logger.Enter(Self, 'edtLookupKeyUp');
  if FVKPressed and grdLookup.Enabled then
  begin
    PostMessage(grdLookup.Handle, WM_KEYDOWN, Key, 0);
    grdLookup.SetFocus;
  end;
  FVKPressed := False;
  Modified;
  Logger.Leave(Self, 'edtLookupKeyUp');
end;

procedure TfrmLookup.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Logger.Enter(Self, 'FormKeyUp');
  if Key = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Close;
  end;
  Logger.Leave(Self, 'FormKeyUp');
end;

procedure TfrmLookup.FormShow(Sender: TObject);
begin
  edtLookup.SetFocus;
end;

procedure TfrmLookup.grdLookupKeyPress(Sender: TObject; var Key: Char);
begin
  Logger.Enter(Self, 'grdLookupKeyPress');
  if Ord(Key) = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    CloseQuery;
  end
  else if not edtLookup.Focused then
  begin
    edtLookup.SetFocus;
    PostMessage(edtLookup.Handle, WM_CHAR, Ord(Key), 0);
    edtLookup.SelStart := Length(edtLookup.Text);
    // required to prevent the invocation of accelerator keys!
    //Key := #0;
  end;
  Logger.Leave(Self, 'grdLookupKeyPress');
end;

procedure TfrmLookup.grdLookupKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Modified;
end;

procedure TfrmLookup.grdLookupMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Modified;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmLookup.Modified;
begin
  FUpdate := True;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmLookup.Execute;
begin
  Logger.Enter(Self, 'Execute');
  SearchDataSet.DisableControls;
  try
    (FData as ISearch).Search(
      edtLookup.Text,
      True,
      True,
      True
    );
    //if not FData.SearchDataSet.IsEmpty then
    //  /FEditor.SearchAndSelectText(edtLookup.Text);
    finally
      SearchDataSet.EnableControls;
   //   grdLookup.AutoAdjustColumns;
    end;
  Logger.Leave(Self, 'Execute');
end;

procedure TfrmLookup.UpdateActions;
begin
  inherited UpdateActions;
  if FUpdate then
  begin
    if Visible then
    begin
      Execute;
//      FEditor.SearchAndSelectText(edtLookup.Text);
    end;
    FUpdate := False;
  end;
  if SearchDataSet.Active then
  begin
    sbrMain.SimpleText := Format('%d record(s)', [SearchDataSet.RecordCount]);
  end;
end;
{$ENDREGION}

end.

