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

unit ts_Editor_HTMLViewForm;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, FramView, Forms, Controls, Graphics, Dialogs,

  ts_Editor_Interfaces, ts_Editor_CustomToolView, HTMLSubs, HtmlGlobals;

type
  TfrmHTMLView = class(TCustomEditorToolView, IEditorToolView)
    FrameViewer1: TFrameViewer;
    procedure FrameViewer1FileRequest(Sender: TObject; const SRC: ThtString;
      var NewName: ThtString);
  private
    //FHTMLViewer: TFrameViewer;
  public
    procedure AfterConstruction; override;

    procedure UpdateView; override;
  end;

//*****************************************************************************

implementation

{$R *.lfm}

procedure TfrmHTMLView.FrameViewer1FileRequest(Sender: TObject;
  const SRC: ThtString; var NewName: ThtString);
begin
  //Manager.OpenFile(NewName);
end;

procedure TfrmHTMLView.AfterConstruction;
begin
  inherited AfterConstruction;
  //FHTMLViewer := TFrameViewer.Create(Self);
  //FHTMLViewer.Parent := Self;
  //FHTMLViewer.Align := alClient;
  //FHTMLViewer.fvOptions := FHTMLViewer.fvOptions + [fvNoBorder];
end;

procedure TfrmHTMLView.UpdateView;
begin
  inherited UpdateView;
  //FHTMLViewer.Load(View.Text);
  //FrameViewer1.LoadFromString(View.Text);
  if FileExistsUTF8(View.FileName) then
    FrameViewer1.LoadFromFile(View.FileName)
  else
    FrameViewer1.LoadFromString(View.Text);

end;

end.

