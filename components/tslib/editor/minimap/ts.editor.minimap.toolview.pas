{
  Copyright (C) 2013-2023 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.MiniMap.ToolView;

{$MODE DELPHI}

interface

uses
  SysUtils, Controls,

  ts.Components.SynMiniMap,

  ts.Editor.ToolView.Base, ts.Editor.MiniMap.Settings;

type
  TfrmMiniMap = class(TCustomEditorToolView)
    procedure FMiniMapClick(Sender: TObject; Data: PSynMiniMapEventData);
    procedure FormResize(Sender: TObject);

  private
    FMiniMap : TSynMiniMap;
    function GetSettings: TMiniMapSettings;

  protected
    property Settings : TMiniMapSettings
      read GetSettings;

  public
    procedure AfterConstruction; override;

    procedure UpdateView; override;
  end;

implementation

{$R *.lfm}

{$REGION 'construction and destruction'}
procedure TfrmMiniMap.AfterConstruction;
begin
  inherited AfterConstruction;
  FMiniMap := TSynMiniMap.Create(Self);
  FMiniMap.Parent := Self;
  FMiniMap.Align := alClient;
  FMiniMap.Options.AllowScroll := True;
  FMiniMap.FontFactor := 3;
  FMiniMap.Options.TabWidth := Manager.Settings.EditorOptions.TabWidth;
  FMiniMap.Options.TabWidthOverride := False;
  FMiniMap.OnClick := FMiniMapClick;
  Width := Settings.Width;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmMiniMap.GetSettings: TMiniMapSettings;
begin
  Result := inherited Settings.
    ToolSettings.ItemsByClass[TMiniMapSettings] as TMiniMapSettings;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMiniMap.FMiniMapClick(Sender: TObject; Data: PSynMiniMapEventData
  );
begin
  // TODO
end;

procedure TfrmMiniMap.FormResize(Sender: TObject);
begin
  Settings.Width := Width;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmMiniMap.UpdateView;
begin
  inherited UpdateView;
  FMiniMap.Editor := View.Editor;
  FMiniMap.Render;
end;
{$ENDREGION}

end.

