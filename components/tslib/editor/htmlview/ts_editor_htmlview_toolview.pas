{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

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


  FramView,


  ts.Editor.Interfaces, ts_Editor_ToolView_Base, ts.Editor.HTMLView.Settings;

type

  { TfrmHTMLView }

  TfrmHTMLView = class(TCustomEditorToolView, IEditorToolView)
  private

    FHTMLViewer: TFrameViewer;
    function GetSettings: THTMLViewSettings;

  strict protected
    procedure UpdateView; override;

    property Settings : THTMLViewSettings
      read GetSettings;


  public
    procedure AfterConstruction; override;
  end;

implementation

{$R *.lfm}

{$region 'construction and destruction' /fold}
procedure TfrmHTMLView.AfterConstruction;
begin
  inherited AfterConstruction;

  FHTMLViewer := TFrameViewer.Create(Self);
  FHTMLViewer.Parent := Self;
  FHTMLViewer.Align := alClient;
  FHTMLViewer.fvOptions := FHTMLViewer.fvOptions + [fvNoBorder];

end;
{$endregion}

{$region 'protected methods' /fold}

function TfrmHTMLView.GetSettings: THTMLViewSettings;
begin
  Result := inherited Settings
    .ToolSettings.ItemsByClass[THTMLViewSettings] as THTMLViewSettings;
end;


procedure TfrmHTMLView.UpdateView;
begin
  inherited UpdateView;

  if FileExistsUTF8(View.FileName) then
    FHTMLViewer.LoadFromFile(View.FileName)
  else
    FHTMLViewer.Load(View.Text);

end;
{$endregion}

end.
