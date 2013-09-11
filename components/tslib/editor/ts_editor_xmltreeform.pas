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
  StdCtrls,

  ts_Components_XMLTree,

  ts_Editor_Interfaces, ts_Editor_CustomToolView;

//=============================================================================

type
  TfrmXmlTree = class(TCustomEditorToolView, IEditorToolView)
    btnInspect : TButton;
    btnExpand: TButton;
    btnCollapse: TButton;
    pnlHeader: TPanel;
    pnlXmlTree: TPanel;

    procedure btnCollapseClick(Sender: TObject);
    procedure btnExpandClick(Sender: TObject);
    procedure btnInspectClick(Sender: TObject);

  strict private
    FXMLTree: TXMLTree;

  public
    procedure UpdateView; override;

    procedure AfterConstruction; override;
  end;

//*****************************************************************************

implementation

{$R *.lfm}

uses
  ts_Core_Helpers, ts_Core_ComponentInspector;

{$region 'construction and destruction' /fold}

procedure TfrmXmlTree.AfterConstruction;
begin
  inherited AfterConstruction;
  FXMLTree := CreateXMLTree(Self, pnlXmlTree);
end;

{$endregion}

{$region 'property access mehods' /fold}

{$endregion}

{$region 'action handlers' /fold}

procedure TfrmXmlTree.btnInspectClick(Sender: TObject);
begin
  InspectComponent(FXMLTree);
end;

procedure TfrmXmlTree.btnExpandClick(Sender: TObject);
begin
  FXMLTree.FullExpand;
end;

procedure TfrmXmlTree.btnCollapseClick(Sender: TObject);
begin
  FXMLTree.FullCollapse;
end;

{$endregion}

{$region 'protected methods' /fold}

procedure TfrmXmlTree.UpdateView;
begin
  try
    FXMLTree.XML := View.Text;
  except
  end;
end;

{$endregion}

end.

