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

unit ts.Editor.ToolView.HexEditor;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,

  KHexEditor,

  ts.Editor.Interfaces, ts.Editor.ToolView.Base;

type
  TfrmHexEditor = class(TCustomEditorToolView, IEditorToolView)
    HexEditor: TKHexEditor;

    procedure HexEditorChange(Sender: TObject);

  strict protected
    procedure UpdateView; override;
  end;

implementation

{$R *.lfm}

{$region 'event handlers' /fold}
procedure TfrmHexEditor.HexEditorChange(Sender: TObject);
var
  S: TStream;
begin
  if HexEditor.Focused then
  begin
    S := TMemoryStream.Create;
    try
      HexEditor.SaveToStream(S);
      S.Position := 0;
      View.LoadFromStream(S);
    finally
      S.Free;
    end;
  end;
end;
{$endregion}

{$region 'protected methods' /fold}
procedure TfrmHexEditor.UpdateView;
var
  S: TStream;
begin
  S := TMemoryStream.Create;
  try
    View.SaveToStream(S);
    S.Position := 0;
    HexEditor.LoadFromStream(S);
  finally
    S.Free;
  end;
end;
{$endregion}

end.

