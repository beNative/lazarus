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

unit ts.Editor.minimap.ToolView;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,

  ts.Components.SynMiniMap,

  ts.Editor.ToolView.Base, ts.Editor.MiniMap.Settings;

type

  { TfrmMiniMap }

  TfrmMiniMap = class(TCustomEditorToolView)
    procedure FMiniMapClick(Sender: TObject; Data: PSynMiniMapEventData);
    procedure FormResize(Sender: TObject);
  private
    FMiniMap : TSynMiniMap;
    function GetSettings: TMiniMapSettings;

  strict protected
    property Settings : TMiniMapSettings
      read GetSettings;

  public
    procedure AfterConstruction; override;

    procedure UpdateView; override;
  end;

implementation

{$R *.lfm}

{$REGION 'construction and destruction' /FOLD}
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

{$REGION 'property access mehods' /FOLD}
function TfrmMiniMap.GetSettings: TMiniMapSettings;
begin
  Result := inherited Settings.
    ToolSettings.ItemsByClass[TMiniMapSettings] as TMiniMapSettings;
end;
{$ENDREGION}

{$REGION 'event handlers' /FOLD}
procedure TfrmMiniMap.FMiniMapClick(Sender: TObject; Data: PSynMiniMapEventData
  );
begin
  //
end;

procedure TfrmMiniMap.FormResize(Sender: TObject);
begin
  Settings.Width := Width;
end;
{$ENDREGION}

{$REGION 'public methods' /FOLD}
procedure TfrmMiniMap.UpdateView;
begin
  inherited UpdateView;
  FMiniMap.Editor := View.Editor;
  FMiniMap.Render;
end;
{$ENDREGION}

end.

