{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.HexEditor.ToolView;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils,

  KHexEditor,

  ts.Editor.Interfaces, ts.Editor.HexEditor.Settings, ts.Editor.ToolView.Base;

type
  TfrmHexEditor = class(TCustomEditorToolView, IEditorToolView)
    HexEditor: TKHexEditor;

    procedure FormResize(Sender: TObject);
    procedure HexEditorChange(Sender: TObject);

  private
    function GetSettings: THexEditorSettings;
  strict protected
    procedure UpdateView; override;

    property Settings: THexEditorSettings
      read GetSettings;

  public
    procedure AfterConstruction; override;
  end;

implementation

{$R *.lfm}

{$REGION 'construction and destruction'}
procedure TfrmHexEditor.AfterConstruction;
begin
  inherited AfterConstruction;
  Width := Settings.Width;
end;
{$ENDREGION}

{$REGION 'event handlers'}
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

procedure TfrmHexEditor.FormResize(Sender: TObject);
begin
  Settings.Width := Width;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmHexEditor.GetSettings: THexEditorSettings;
begin
  Result := inherited Settings
    .ToolSettings.ItemsByClass[THexEditorSettings] as THexEditorSettings;
end;
{$ENDREGION}

{$REGION 'protected methods'}
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
{$ENDREGION}

end.

