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

unit ts_Editor_PreviewForm;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,

  RichMemo,

  ts_Editor_Interfaces, ts_Editor_CustomToolView,

  ts_Components_ExportRTF;

type
  TfrmPreview = class(TCustomEditorToolView, IEditorToolView)
    mniSelectAll                : TMenuItem;
    mniOpenSelectionInNewEditor : TMenuItem;
    mmoPreview                  : TRichMemo;
    ppmPreview                  : TPopupMenu;

  strict private
     FSynExporterRTF: TSynExporterRTF;

  strict protected
    procedure UpdateView; override;

  public
    procedure AfterConstruction; override;

  end;

//*****************************************************************************

implementation

{$R *.lfm}

{$region 'construction and destruction' /fold}

procedure TfrmPreview.AfterConstruction;
begin
  inherited AfterConstruction;
  FSynExporterRTF := TSynExporterRTF.Create(Self);
  mmoPreview.DoubleBuffered := True;
end;

{$endregion}

{$region 'property access mehods' /fold}



{$endregion}

{$region 'protected methods' /fold}

procedure TfrmPreview.UpdateView;
var
  SS : TStringStream;
  S  : string;
  SL : TStringList;
begin
  BeginFormUpdate;
  try
    mmoPreview.Clear;
    S := View.PreviewText;
    if (S <> '') and (View.Editor.Highlighter <> nil) then
    begin
      SL := TStringList.Create;
      try
        SL.Text := S;
        S := '';
        SS := TStringStream.Create(S);
        try
          SL.BeginUpdate;
          FSynExporterRTF.UseBackground := True;
          FSynExporterRTF.Font := View.Editor.Font;
          FSynExporterRTF.Highlighter := View.Editor.Highlighter;
          FSynExporterRTF.ExportAsText := True;
          FSynExporterRTF.ExportAll(SL);
          FSynExporterRTF.SaveToStream(SS);
          SS.Position := 0;
          mmoPreview.LoadRichText(SS);
        finally
          SL.EndUpdate;
          FreeAndNil(SS);
        end;
      finally
        FreeAndNil(SL);
      end;
    end;
  finally
    EndFormUpdate;
  end;
end;

{$endregion}

end.
