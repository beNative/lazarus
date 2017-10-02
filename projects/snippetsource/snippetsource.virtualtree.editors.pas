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

unit SnippetSource.VirtualTree.Editors;

{$MODE DELPHI}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Buttons, ExtCtrls, ComCtrls,

  LCLIntf, LCLType,

  delphicompat,

  VirtualTrees,

  ts.Components.VirtualDBTreeEx;

type
  // Node data record for the the document properties treeview.
  PPropertyData = ^TPropertyData;
  TPropertyData = record
    ValueType: TValueType;
    Value: string;      // This value can actually be a date or a number too.
    Changed: Boolean;
  end;

  // Our own edit link to implement several different node editors.
  TPropertyEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FEdit: TEdit;        // One of the property editor classes.
    FTree: TCheckVirtualDBTreeEx;
    FNode: PVirtualNode;       // The node being edited.
    FColumn: Integer;          // The column of the node being edited.
  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    destructor Destroy; override;

    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

  // Our own edit link to implement several different node editors.

  { TGridEditLink }

  TGridEditLink = class(TPropertyEditLink, IVTEditLink)
  public
    function EndEdit: Boolean; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
  end;

implementation

destructor TPropertyEditLink.Destroy;
begin
  FreeAndNil(FEdit);
  FEdit.Free;
  inherited;
end;

procedure TPropertyEditLink.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  CanAdvance: Boolean;

begin
  CanAdvance := true;
  
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

        if CanAdvance then
        begin
          // Forward the keypress to the tree. It will asynchronously change the focused node.
          PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
          Key := 0;
        end;
      end;
  end;
end;

function TPropertyEditLink.BeginEdit: Boolean;
begin
  Result := True;
  FEdit.Show;
  FEdit.SetFocus;
end;

function TPropertyEditLink.CancelEdit: Boolean;
begin
  Result := True;
  FEdit.Hide;
end;

function TPropertyEditLink.EndEdit: Boolean;
var
  S: String;
begin
  Result := True;

  S := FEdit.Text;
  if S <> FTree.NodeText[FNode] then
  begin
    FTree.NodeText[FNode] := S;
    FTree.InvalidateNode(FNode);
  end;
  FEdit.Hide;
  FTree.SetFocus;
end;

function TPropertyEditLink.GetBounds: TRect;
begin
  Result := FEdit.BoundsRect;
end;

function TPropertyEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Result := True;
  FTree := Tree as TCheckVirtualDBTreeEx;
  FNode := Node;
  FColumn := Column;
  FreeAndNil(FEdit);
  FEdit := TEdit.Create(nil);
  FEdit.Visible := False;
  FEdit.Parent := Tree;
  FEdit.Text := FTree.NodeText[Node];
  FEdit.OnKeyDown := EditKeyDown;
end;

procedure TPropertyEditLink.ProcessMessage(var Message: TMessage);
begin
  FEdit.WindowProc(Message);
end;

procedure TPropertyEditLink.SetBounds(R: TRect);
var
  Dummy: Integer;
begin
  // Since we don't want to activate grid extensions in the tree (this would influence how the selection is drawn)
  // we have to set the edit's width explicitly to the width of the column.
  FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, R.Right);
  FEdit.BoundsRect := R;
end;

function TGridEditLink.EndEdit: Boolean;
var
  S: string;
begin
  Result := True;
  S := FEdit.Text;
  if S <> FTree.NodeText[FNode] then
  begin
    FTree.NodeText[FNode] := S;
  end;
  FTree.InvalidateNode(FNode);
  FEdit.Hide;
end;

function TGridEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Result := True;
  FTree := Tree as TCheckVirtualDBTreeEx;
  FNode := Node;
  FColumn := Column;

  // Determine what edit type actually is needed.
  FEdit.Free;
  FEdit := nil;
  FEdit := TEdit.Create(nil);
  FEdit.Visible := False;
  FEdit.Parent := Tree;
  FEdit.Text := FTree.NodeText[FNode];
  FEdit.OnKeyDown := EditKeyDown;
end;

end.
