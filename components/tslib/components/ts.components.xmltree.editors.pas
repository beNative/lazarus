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

unit ts.Components.XMLTree.Editors;

{$MODE DELPHI}

interface

uses
  Messages, SysUtils, Classes, Controls, Forms, StdCtrls,

  LCLIntf, LCLType,

  VirtualTrees;

type
  // Node data record for the the document properties treeview.
  PPropertyData = ^TPropertyData;
  TPropertyData = record
    Value: String;      // This value can actually be a date or a number too.
    Changed: Boolean;
  end;

  // Our own edit link to implement several different node editors.

  { TXMLEditLink }

  //  IVTEditLink = interface
  //  ['{2BE3EAFA-5ACB-45B4-9D9A-B58BCC496E17}']
  //  function BeginEdit: Boolean; stdcall;                  // Called when editing actually starts.
  //  function CancelEdit: Boolean; stdcall;                 // Called when editing has been cancelled by the tree.
  //  function EndEdit: Boolean; stdcall;                    // Called when editing has been finished by the tree.
  //  function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
  //                                                         // Called after creation to allow a setup.
  //  function GetBounds: TRect; stdcall;                    // Called to get the current size of the edit window
  //                                                         // (only important if the edit resizes itself).
  //  procedure ProcessMessage(var Message: TLMessage); stdcall;
  //                                                         // Used to forward messages to the edit window(s)-
  //  procedure SetBounds(R: TRect); stdcall;                // Called to place the editor.
  //end;


  TXMLEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FEdit: TEdit;        // One of the property editor classes.
    FTree: TCustomVirtualStringTree;
    FNode: PVirtualNode;       // The node being edited.
    FColumn: Integer;          // The column of the node being edited.
  protected
    procedure EditExit(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    destructor Destroy; override;

    { IVTEditLink }
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

type
  TPropertyTextKind = (
    ptkText,
    ptkHint
  );

type
  PGridData = ^TGridData;
  TGridData = record
    ValueType: array[0..3] of TValueType; // one for each column
    Value: array[0..3] of Variant;
    Changed: Boolean;
  end;

implementation

uses
  ts.Components.XMLTree;

// This implementation is used in VST3 to make a connection beween the tree
// and the actual edit window which might be a simple edit, a combobox
// or a memo etc.

destructor TXMLEditLink.Destroy;
begin
  Application.ReleaseComponent(FEdit);
  inherited;
end;

procedure TXMLEditLink.EditExit(Sender: TObject);
begin
  FTree.EndEditNode;
end;

procedure TXMLEditLink.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  CanAdvance: Boolean;
begin
  CanAdvance := True;
  case Key of
    VK_ESCAPE:
      if CanAdvance then
      begin
        FTree.CancelEditNode;
        Key := 0;
      end;
    VK_RETURN:
      if CanAdvance then
      begin
        FTree.EndEditNode;
        Key := 0;
      end;
    VK_UP,
    VK_DOWN:
      begin
        // Consider special cases before finishing edit mode.
        CanAdvance := Shift = [];
        //if FEdit is TComboBox then
        //  CanAdvance := CanAdvance and not TComboBox(FEdit).DroppedDown;
        //todo: there's no way to know if date is being edited in LCL
        //if FEdit is TDateEdit then
        //  CanAdvance := CanAdvance and not TDateEdit(FEdit).DroppedDown;

        if CanAdvance then
        begin
          // Forward the keypress to the tree. It will asynchronously change the focused node.
          PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
          Key := 0;
        end;
      end;
  end;
end;

function TXMLEditLink.BeginEdit: Boolean; stdcall;
begin
  Result := True;
  FEdit.Show;
  FEdit.SetFocus;
end;

function TXMLEditLink.CancelEdit: Boolean; stdcall;
begin
  Result := True;
  FEdit.Hide;
end;

function TXMLEditLink.EndEdit: Boolean; stdcall;
var
  S: string;
begin
  Result := True;
  S := FEdit.Text;
  if S <> FTree.Text[FNode, FColumn] then
  begin
    FTree.Text[FNode, FColumn] := S;
  end;
  FTree.InvalidateNode(FNode);
  FEdit.Hide;
end;

function TXMLEditLink.GetBounds: TRect; stdcall;
begin
  Result := FEdit.BoundsRect;
end;

function TXMLEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex): Boolean; stdcall;
begin
  Result := True;
  FTree := Tree as TCustomVirtualStringTree;
  FNode := Node;
  FColumn := Column;
  FreeAndNil(FEdit);
  FEdit := TEdit.Create(nil);
  FEdit.BorderStyle := bsNone;
  FEdit.AutoSize := False;
  FEdit.Visible := False;
  FEdit.Font.Name := Tree.Font.Name;
    //FEdit.Font.Height := FTree.ComputeNodeHeight(FTree.Canvas, Node, Column);
  FEdit.Font.Size :=  8;
  //FTree.Canvas.Font.Size;
  //NodeHeight[Node];
  FEdit.Parent := Tree;
  FEdit.Text := FTree.Text[Node, FColumn];
  FEdit.SelectAll;
  FEdit.OnKeyDown := EditKeyDown;
end;

procedure TXMLEditLink.ProcessMessage(var Message: TMessage); stdcall;
begin
  FEdit.WindowProc(Message);
end;

procedure TXMLEditLink.SetBounds(R: TRect); stdcall;
//var
//  Dummy: Integer;
begin
  // Since we don't want to activate grid extensions in the tree (this would influence how the selection is drawn)
  // we have to set the edit's width explicitly to the width of the column.
//  TXMLTree(FTree).Header.Columns.GetColumnBounds(FColumn, Dummy, R.Right);
  FEdit.BoundsRect := R;
end;


end.
