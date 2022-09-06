{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

unit SnippetSource.VirtualTree.Editors;

{$MODE DELPHI}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Buttons, ExtCtrls, ComCtrls,

  LCLIntf, LCLType,

  VirtualTrees,

  ts.Components.VirtualDBTreeEx;

type
  // Node data record for the the document properties treeview.
  PPropertyData = ^TPropertyData;
  TPropertyData = record
    ValueType : TValueType;
    Value     : string;  // This value can actually be a date or a number too.
    Changed   : Boolean;
  end;

{$REGION 'TPropertyEditLink'}
  // Our own edit link to implement several different node editors.
  TPropertyEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FEdit   : TEdit;
    FTree   : TCheckVirtualDBTreeEx;
    FNode   : PVirtualNode;
    FColumn : Integer;

  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  public
    destructor Destroy; override;

    {$REGION 'IVTEditLink'}
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
    function PrepareEdit(
      Tree   : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    ): Boolean; stdcall;
    {$ENDREGION}
  end;
{$ENDREGION}

{$REGION 'TGridEditLink'}
  // Our own edit link to implement several different node editors.
  TGridEditLink = class(TPropertyEditLink, IVTEditLink)
  public
    function EndEdit: Boolean; stdcall;
    function PrepareEdit(
      Tree   : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    ): Boolean; stdcall;
  end;
{$ENDREGION}

implementation

{$REGION 'TPropertyEditLink'}
{$REGION 'construction and destruction'}
destructor TPropertyEditLink.Destroy;
begin
  FreeAndNil(FEdit);
  FEdit.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TPropertyEditLink.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  LCanAdvance: Boolean;
begin
  LCanAdvance := true;

  case Key of
    VK_ESCAPE:
      if LCanAdvance then
      begin
        FTree.CancelEditNode;
        Key := 0;
      end;
    VK_RETURN:
      if LCanAdvance then
      begin
        FTree.EndEditNode;
        Key := 0;
      end;

    VK_UP,
    VK_DOWN:
      begin
        // Consider special cases before finishing edit mode.
        LCanAdvance := Shift = [];

        if LCanAdvance then
        begin
          // Forward the keypress to the tree. It will asynchronously change the focused node.
          PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
          Key := 0;
        end;
      end;
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
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
  S : string;
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

function TPropertyEditLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): Boolean;
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
  LDummy: Integer;
begin
  // Since we don't want to activate grid extensions in the tree (this would
  // influence how the selection is drawn)
  // we have to set the edit's width explicitly to the width of the column.
  FTree.Header.Columns.GetColumnBounds(FColumn, LDummy, R.Right);
  FEdit.BoundsRect := R;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TGridEditLink'}
function TGridEditLink.EndEdit: Boolean;
var
  S : string;
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

function TGridEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex): Boolean;
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
{$ENDREGION}

end.
