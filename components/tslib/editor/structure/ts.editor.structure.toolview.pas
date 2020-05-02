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

unit ts.Editor.Structure.ToolView;

{$MODE DELPHI}

interface

uses
  ExtCtrls, StdCtrls,

  ts.Components.XMLTree,

  ts.Editor.Interfaces, ts.Editor.ToolView.Base;

type
  TfrmStructure = class(TCustomEditorToolView, IEditorToolView)
    btnInspect  : TButton;
    btnExpand   : TButton;
    btnCollapse : TButton;
    pnlHeader   : TPanel;
    pnlXmlTree  : TPanel;

    procedure btnCollapseClick(Sender: TObject);
    procedure btnExpandClick(Sender: TObject);
    procedure btnInspectClick(Sender: TObject);

  strict private
    FXMLTree: TXMLTree;

  public
    procedure UpdateView; override;

    procedure AfterConstruction; override;
  end;

implementation

{$R *.lfm}

uses
  ts.Core.ComponentInspector;

{$REGION 'construction and destruction'}
procedure TfrmStructure.AfterConstruction;
begin
  inherited AfterConstruction;
//  FXMLTree := CreateXMLTree(Self, pnlXmlTree);
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmStructure.btnInspectClick(Sender: TObject);
begin
  InspectComponent(FXMLTree);
end;

procedure TfrmStructure.btnExpandClick(Sender: TObject);
begin
  FXMLTree.FullExpand;
end;

procedure TfrmStructure.btnCollapseClick(Sender: TObject);
begin
  FXMLTree.FullCollapse;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmStructure.UpdateView;
begin
  try
    FXMLTree.XML := View.Text;
  except
    // ignore parsing errors
  end;
end;
{$ENDREGION}

end.

