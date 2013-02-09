{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts_Editor_XmlTreeForm;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,

  ts_Components_XMLTree,

  ts_Editor_Interfaces;

//=============================================================================

type
  TfrmXmlTree = class(TForm, IEditorToolView)
    pnlXmlTree: TPanel;

  strict private
    FXMLTree: TXMLTree;

    function GetForm: TForm;
    function GetName: string;
    function GetView: IEditorView;
    function GetVisible: Boolean;
  public
    { Lets the view respond to changes. }
    procedure UpdateView;

    procedure AfterConstruction; override;

    property View: IEditorView
      read GetView;

  end;

//*****************************************************************************

implementation

{$R *.lfm}

uses
  ts_Core_Helpers;

{$region 'construction and destruction' /fold}
//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmXmlTree.AfterConstruction;
begin
  inherited AfterConstruction;
  FXMLTree := CreateXMLTree(Self, pnlXmlTree);
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************
{$endregion}

{$region 'property access mehods' /fold}
//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TfrmXmlTree.GetForm: TForm;
begin
  Result := Self;
end;

function TfrmXmlTree.GetName: string;
begin
  Result := inherited Name;
end;

function TfrmXmlTree.GetView: IEditorView;
begin
  Result := Owner as IEditorView;
end;

function TfrmXmlTree.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************
{$endregion}

procedure TfrmXmlTree.UpdateView;
begin
  try
    FXMLTree.XML := View.Text;
  except
  end;
end;

end.

