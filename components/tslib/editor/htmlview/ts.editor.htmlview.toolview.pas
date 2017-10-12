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

unit ts.Editor.htmlview.ToolView;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,

  FramView,

  ts.Editor.Interfaces, ts.Editor.ToolView.Base, ts.Editor.HTMLView.Settings;

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

{$REGION 'construction and destruction'}
procedure TfrmHTMLView.AfterConstruction;
begin
  inherited AfterConstruction;

  FHTMLViewer := TFrameViewer.Create(Self);
  FHTMLViewer.Parent := Self;
  FHTMLViewer.Align := alClient;
  FHTMLViewer.fvOptions := FHTMLViewer.fvOptions + [fvNoBorder];

end;
{$ENDREGION}

{$REGION 'protected methods'}

function TfrmHTMLView.GetSettings: THTMLViewSettings;
begin
  Result := inherited Settings
    .ToolSettings.ItemsByClass[THTMLViewSettings] as THTMLViewSettings;
end;

procedure TfrmHTMLView.UpdateView;
begin
  inherited UpdateView;

  if FileExists(View.FileName) then
    FHTMLViewer.LoadFromFile(View.FileName)
  else
    FHTMLViewer.Load(View.Text);
end;
{$ENDREGION}

end.
