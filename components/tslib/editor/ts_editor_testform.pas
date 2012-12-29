{
  Copyright (C) 2012 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts_Editor_Testform;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls,

  XMLTree,

  ts_Editor_Interfaces;

//=============================================================================

type
  TfrmTest = class(TForm, IEditorToolView)
    btnExecute: TButton;
    edtInput: TEdit;
    pnlXML: TPanel;
  private
    FXMLTree: TXMLTree;

    function GetForm: TForm;
    function GetManager: IEditorManager;
    function GetName: string;
    function GetView: IEditorView;

  protected

    procedure UpdateView;

    property Manager: IEditorManager
      read GetManager;

    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    property Visible: Boolean
      read GetVisible write SetVisible;

    property Name: string
      read GetName;

    property Form: TForm
      read GetForm;

    property View: IEditorView
      read GetView;

  public
    procedure AfterConstruction; override;
  end; 

//*****************************************************************************

implementation

{$R *.lfm}

uses
  ts_Core_Helpers;

function TfrmTest.GetForm: TForm;
begin
  Result := Self;
end;

function TfrmTest.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TfrmTest.GetName: string;
begin
  Result := inherited Name;
end;

function TfrmTest.GetView: IEditorView;
begin
  Result := Owner as IEditorView;
end;

procedure TfrmTest.UpdateView;
begin
  try
    FXMLTree.XML := View.Text;
  except
  end;
end;

function TfrmTest.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

procedure TfrmTest.SetVisible(AValue: Boolean);
begin
  inherited SetVisible(AValue);
end;

procedure TfrmTest.AfterConstruction;
begin
  inherited AfterConstruction;
  FXMLTree := CreateXMLTree(Self, pnlXML);
end;

end.

