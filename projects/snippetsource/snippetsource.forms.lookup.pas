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

unit SnippetSource.Forms.Lookup;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, DB, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, DBGrids,

  ts.Editor.Interfaces,

  SnippetSource.Interfaces;

type

  { TfrmLookup }

  TfrmLookup = class(TForm)
    aclMain    : TActionList;
    actSearch  : TAction;
    btnSearch  : TButton;
    chkComment : TCheckBox;
    chkName    : TCheckBox;
    chkText    : TCheckBox;
    grdLookup  : TDBGrid;
    dscMain    : TDatasource;
    edtLookup  : TEdit;
    sbrMain    : TStatusBar;

    procedure actSearchExecute(Sender: TObject);

    procedure chkNameChange(Sender: TObject);
    procedure dscMainDataChange(Sender: TObject; Field: TField);
    procedure edtLookupKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtLookupKeyPress(Sender: TObject; var Key: char);
    procedure edtLookupKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure grdLookupKeyPress(Sender: TObject; var Key: char);
    procedure grdLookupKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure grdLookupMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  private
    FVKPressed : Boolean;
    FEditor    : IEditorView;
    FData      : ILookup;
    FUpdate    : Boolean;

    function GetDataSet: TDataSet;
    procedure Changed;

  public
    constructor Create(
      AOwner  : TComponent;
      AEditor : IEditorView;
      ALookup : ILookup
    ); reintroduce; virtual;
    destructor Destroy; override;

    procedure UpdateActions; override;

    procedure Execute;

    property DataSet: TDataSet
      read GetDataSet;
  end; 

procedure Lookup(AEditor: IEditorView; ALookup: ILookup);

implementation

{$R *.lfm}

uses
  LCLType, Windows;

var
  FLookupForm: TfrmLookup;

procedure Lookup(AEditor: IEditorView; ALookup: ILookup);
begin
  if not Assigned(FLookupForm) then
    FLookupForm := TfrmLookup.Create(Application, AEditor, ALookup);
  FLookupForm.Show;
end;

{$REGION 'construction and destruction'}
constructor TfrmLookup.Create(AOwner: TComponent; AEditor: IEditorView; ALookup: ILookup);
begin
  inherited Create(AOwner);
  FData           := ALookup;
  FEditor         := AEditor;
  dscMain.DataSet := ALookup.LookupDataSet;
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
procedure TfrmLookup.chkNameChange(Sender: TObject);
begin
  Execute;
end;

procedure TfrmLookup.dscMainDataChange(Sender: TObject; Field: TField);
begin
  if not Assigned(Field) then
  begin
     (FData as IDataSet).DataSet .Locate('Id' , DataSet.FieldByName('Id').AsInteger,[]);
    if not FData.LookupDataSet.IsEmpty then
      FEditor.SearchAndSelectText(edtLookup.Text);
  end;
end;

procedure TfrmLookup.edtLookupKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //if not (
  //  (ssAlt in Shift) or
  //  (ssShift in Shift) or
  //  (Key in [VK_DELETE, VK_LEFT, VK_RIGHT, VK_HOME, VK_END, VK_SHIFT, VK_CONTROL]) or
  //  ((Key in [VK_INSERT, VK_DELETE, VK_LEFT, VK_RIGHT, VK_HOME, VK_END]) and
  //  ((ssShift in Shift) or (ssCtrl in Shift)))
  //) then
  //begin
  //  FVKPressed := True;
  //  Key := 0;
  //end;
  //
  //if LV.Items.Count = 0 then
  //  Exit;
  //
  //if Key = VK_Down then
  //begin
  //  if (LV.Items.IndexOf(LV.ItemFocused) + 1) < LV.Items.Count then
  //    LV.ItemFocused := LV.Items[(LV.Items.IndexOf(LV.ItemFocused) + 1)];
  //end
  //else if Key = VK_Up then
  //begin
  //  if (LV.Items.IndexOf(LV.ItemFocused) - 1) >= 0 then
  //    LV.ItemFocused := LV.Items[(LV.Items.IndexOf(LV.ItemFocused) - 1)];
  //end
  //else if Key = VK_Home then
  //begin
  //  LV.ItemFocused := LV.Items[0];
  //end
  //else if Key = VK_End then
  //begin
  //  LV.ItemFocused := LV.Items[LV.Items.Count - 1];
  //end;
  //
  //if LV.ItemFocused<>nil then
  //begin
  //  LV.Selected := LV.ItemFocused;
  //  if Assigned(LV.Selected) then
  //    LV.Selected.MakeVisible(True);
  //end;

end;

procedure TfrmLookup.edtLookupKeyPress(Sender: TObject; var Key: char);
begin
  case Ord(Key) of
    VK_RETURN:
      begin
        //JumpToSelection;
        Key := #0;
      end;
    VK_ESCAPE:
      begin
        self.ModalResult := mrCancel;
        Key := #0;
      end;
  end;
end;

procedure TfrmLookup.edtLookupKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FVKPressed and grdLookup.Enabled then
  begin
    PostMessage(grdLookup.Handle, WM_KEYDOWN, Key, 0);
    grdLookup.SetFocus;
  end;
  FVKPressed := False;
end;

procedure TfrmLookup.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Close;
  end
  else
    inherited;
end;

procedure TfrmLookup.grdLookupKeyPress(Sender: TObject; var Key: char);
begin
  //if Ord(Key) = VK_ESCAPE then
  //begin
  //  ModalResult := mrCancel;
  //  CloseQuery;
  //end
  //else if not edtLookup.Focused then
  //begin
  //  edtLookup.SetFocus;
  //  PostMessage(edtLookup.Handle, WM_CHAR, Ord(Key), 0);
  //  edtLookup.SelStart := Length(edtLookup.Text);
  //  // required to prevent the invocation of accelerator keys!
  //  Key := #0;
  //end;
end;

procedure TfrmLookup.grdLookupKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
///  Changed;
end;

procedure TfrmLookup.grdLookupMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //Changed;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmLookup.Changed;
begin
  FUpdate := True;
end;

procedure TfrmLookup.UpdateActions;
begin
  inherited UpdateActions;
  //if FUpdate then
  //begin
  //  if Visible then
  //  begin
  //    (FData as IDataSet).DataSet.DisableControls;
  //    (FData as IDataSet).DataSet.Locate('Id', DataSet.FieldByName('Id').AsInteger, []);
  //    (FData as IDataSet).DataSet.EnableControls;
  //    FEditor.SearchAndSelectText(edtLookup.Text);
  //  end;
  //  FUpdate := False;
  //end;
  if DataSet.Active then
  begin
    sbrMain.SimpleText := Format('%d record(s)', [DataSet.RecordCount]);
  end;
end;

procedure TfrmLookup.Execute;
begin
  FData.LookupDataSet.DisableControls;
  try
    (FData as ILookup).Lookup(
      edtLookup.Text,
      chkText.Checked,
      chkName.Checked,
      chkComment.Checked
    );
    if not FData.LookupDataSet.IsEmpty then
      FEditor.SearchAndSelectText(edtLookup.Text);
  finally
    FData.LookupDataSet.EnableControls;
    grdLookup.AutoAdjustColumns;
  end;
end;
{$ENDREGION}

end.

