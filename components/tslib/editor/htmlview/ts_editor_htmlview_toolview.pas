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

unit ts_Editor_HTMLView_ToolView;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,

  {$IFDEF Windows}
  FramView, HtmlGlobals,
  {$ENDIF}

  ts.Editor.Interfaces, ts_Editor_ToolView_Base;

type
  TfrmHTMLView = class(TCustomEditorToolView, IEditorToolView)
  private
    {$IFDEF Windows}
    FHTMLViewer: TFrameViewer;
    {$ENDIF}

  strict protected
    procedure UpdateView; override;

  public
    procedure AfterConstruction; override;


  end;

implementation

{$R *.lfm}

{$region 'construction and destruction' /fold}
procedure TfrmHTMLView.AfterConstruction;
begin
  inherited AfterConstruction;
{$IFDEF Windows}
  FHTMLViewer := TFrameViewer.Create(Self);
  FHTMLViewer.Parent := Self;
  FHTMLViewer.Align := alClient;
  FHTMLViewer.fvOptions := FHTMLViewer.fvOptions + [fvNoBorder];
{$ENDIF}
end;
{$endregion}

{$region 'protected methods' /fold}
procedure TfrmHTMLView.UpdateView;
begin
  inherited UpdateView;
{$IFDEF Windows}
  if FileExistsUTF8(View.FileName) then
    FHTMLViewer.LoadFromFile(WideString(View.FileName))
  else
    FHTMLViewer.LoadFromString(WideString(View.Text));
{$ENDIF}
end;
{$endregion}

end.

