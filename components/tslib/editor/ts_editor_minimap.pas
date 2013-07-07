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

unit ts_Editor_Minimap;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,

  ts_Components_SynMiniMap,

  ts_Editor_CustomToolView;

type
  TfrmMiniMap = class(TCustomEditorToolView)
    procedure FMiniMapClick(Sender: TObject; Data: PSynMiniMapEventData);
  private
    FMiniMap : TSynMiniMap;

  public
    procedure AfterConstruction; override;

    procedure UpdateView; override;
  end;

//*****************************************************************************

implementation

{$R *.lfm}

procedure TfrmMiniMap.FMiniMapClick(Sender: TObject; Data: PSynMiniMapEventData
  );
begin

  //
end;

procedure TfrmMiniMap.AfterConstruction;
begin
  inherited AfterConstruction;
  FMiniMap := TSynMiniMap.Create(Self);
  FMiniMap.Parent := Self;
  FMiniMap.Align := alClient;
  FMiniMap.Options.AllowScroll := True;
  FMiniMap.FontFactor := 3;
  FMiniMap.Options.TabWidth := Manager.Settings.TabWidth;
  FMiniMap.Options.TabWidthOverride := False;

  FMiniMap.OnClick := FMiniMapClick;

end;

procedure TfrmMiniMap.UpdateView;
begin
  inherited UpdateView;
  FMiniMap.Editor := View.Editor;
  FMiniMap.Render;
end;

end.

