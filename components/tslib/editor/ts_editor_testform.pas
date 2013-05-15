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

unit ts_Editor_Testform;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, RTTICtrls, RTTIGrids, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls,

//  CodeToolManager, CodeCache, CustomCodeTool,

  //XMLTree,

  ts_Editor_Interfaces, ts_Editor_CustomToolView, IpHtml, vsVisualSynapse,
  FZCommon, FZDB, TplResStoreUnit, TplMemStreamUnit, vd_system, TplMemINIUnit,
  TplFileSearchUnit, uCmdBox, ATBinHex, KHexEditor;

//=============================================================================

type
  TfrmTest = class(TCustomEditorToolView, IEditorToolView)
    ATBinHex1: TATBinHex;
    btnExecute : TButton;
    CmdBox1: TCmdBox;
    edtInput   : TEdit;
    FZSQLQuery1: TFZSQLQuery;
    KHexEditor1: TKHexEditor;
    plFileSearch1: TplFileSearch;
    plMemIni1: TplMemIni;
    plMemStream1: TplMemStream;
    plResStore1: TplResStore;
    pnlTest     : TPanel;
    vsSystem1: TvsSystem;
    procedure btnExecuteClick(Sender: TObject);
  protected
    procedure UpdateView; override;

  public
    procedure AfterConstruction; override;
  end; 

//*****************************************************************************

implementation

{$R *.lfm}

uses
  ts_Core_ComponentInspector;

//uses
//  IdentCompletionTool,
//
//  Clipbrd;

procedure TfrmTest.btnExecuteClick(Sender: TObject);
//var
//  CB : TCodeBuffer;
//  S  : string;
//  CC : TCodeContextInfo;
begin
   //CC := TCodeContextInfo.Create;
  //CB := CodeToolBoss.LoadFile(View.FileName, True, False);
  //if not Assigned(CB) then
  //  raise Exception.Create('failed loading ' + View.FileName);
  //
  ////S := CodeToolBoss.ExtractCodeWithoutComments(CB);
  //S := CodeToolBoss.FindSmartHint(CB, View.CaretX, View.CaretY);
  //edtInput.Text := S;
  //Clipboard.AsText := S;
  //if CodeToolBoss.FindCodeContext(CB, View.CaretX, View.CaretY, CC) then
  //begin
  //  Clipboard.AsText := CC.ProcName;
  //  edtInput.Text := CC.ProcName;
  //end;
  //
  // CC.Free;

  //if CodeToolBoss.JumpToMethod(CodeBuf,X,Y,NewCode,NewX,NewY,NewTopLine,
  //                             RevertableJump)
  InspectComponents([ATBinHex1, KHexEditor1]);
end;

procedure TfrmTest.UpdateView;
var
  S: TStream;
begin
  S := TMemoryStream.Create;
  try
    View.SaveToStream(S);
    S.Position := 0;
    KHexEditor1.LoadFromStream(S);
    S.Position := 0;
    ATBinHex1.OpenStream(S);
  finally
    S.Free;
  end;
end;

procedure TfrmTest.AfterConstruction;
begin
  inherited AfterConstruction;
  //FXMLTree := CreateXMLTree(Self, pnlTest);
  //CodeToolBoss.L;


end;

end.

