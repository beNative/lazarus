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

  SynExportRTF, RichMemo,

  ts_Editor_Interfaces;

type
  TfrmPreview = class(TForm, IEditorToolView)
    mniSelectAll                : TMenuItem;
    mniOpenSelectionInNewEditor : TMenuItem;
    mmoPreview                  : TRichMemo;
    ppmPreview                  : TPopupMenu;
    SynExporterRTF              : TSynExporterRTF;

  private
    function GetForm: TForm;
    function GetName: string;
    function GetSettings: IEditorSettings;
    function GetView: IEditorView;

  protected
    property Settings: IEditorSettings
      read GetSettings;

    property View: IEditorView
      read GetView;

    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    property Visible: Boolean
      read GetVisible write SetVisible;

    property Name: string
      read GetName;

    property Form: TForm
      read GetForm;

  public
    procedure AfterConstruction; override;
    procedure UpdateView;
  end;

//*****************************************************************************

implementation

{$R *.lfm}

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmPreview.AfterConstruction;
begin
  inherited AfterConstruction;
  mmoPreview.DoubleBuffered := True;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TfrmPreview.GetForm: TForm;
begin
  Result := Self;
end;

function TfrmPreview.GetName: string;
begin
  Result := inherited Name;
end;

function TfrmPreview.GetSettings: IEditorSettings;
begin
  Result := Owner as IEditorSettings;
end;

function TfrmPreview.GetView: IEditorView;
begin
  Result := Owner as IEditorView;
end;

function TfrmPreview.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

procedure TfrmPreview.SetVisible(AValue: Boolean);
begin
  inherited SetVisible(AValue);
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

procedure TfrmPreview.UpdateView;
var
  SS : TStringStream;
  S  : string;
  SL : TStringList;
begin
  if Settings.PreviewVisible then
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
            SynExporterRTF.UseBackground := True;
            SynExporterRTF.Font := View.Editor.Font;
            SynExporterRTF.Highlighter := View.Editor.Highlighter;
            SynExporterRTF.ExportAsText := True;
            SynExporterRTF.ExportAll(SL);
            SynExporterRTF.SaveToStream(SS);
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
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

end.
