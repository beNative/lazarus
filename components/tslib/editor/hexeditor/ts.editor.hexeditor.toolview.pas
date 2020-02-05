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

