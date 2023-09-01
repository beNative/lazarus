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

unit ts.Editor.Preview.ToolView;

{$MODE DELPHI}

interface

uses
  SysUtils, Menus,

  ts.Components.ExportRTF,

  ts.Editor.Interfaces, ts.Editor.ToolView.Base;

type
  TfrmPreview = class(TCustomEditorToolView, IEditorToolView)
    mniSelectAll                : TMenuItem;
    mniOpenSelectionInNewEditor : TMenuItem;
    ppmPreview                  : TPopupMenu;

  private
     FSynExporterRTF : TSynExporterRTF;

  protected
    procedure EditorCaretPositionChange(Sender: TObject; X, Y: Integer); override;

    procedure UpdateView; override;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.lfm}

{$REGION 'construction and destruction'}
procedure TfrmPreview.AfterConstruction;
begin
  inherited AfterConstruction;
  FSynExporterRTF := TSynExporterRTF.Create(Self);
  //mmoPreview.DoubleBuffered := True;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmPreview.EditorCaretPositionChange(Sender: TObject; X, Y: Integer);
begin
  UpdateView;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmPreview.UpdateView;
//var
//  SS : TStringStream;
//  S  : string;
//  SL : TStringList;
begin
  //BeginFormUpdate;
  //try
  //  mmoPreview.Clear;
  //  S := View.PreviewText;
  //  if (S <> '') and (View.Editor.Highlighter <> nil) then
  //  begin
  //    SL := TStringList.Create;
  //    try
  //      SL.Text := S;
  //      S := '';
  //      SS := TStringStream.Create(S);
  //      try
  //        SL.BeginUpdate;
  //        FSynExporterRTF.UseBackground := True;
  //        FSynExporterRTF.Font := View.Editor.Font;
  //        FSynExporterRTF.Highlighter := View.Editor.Highlighter;
  //        FSynExporterRTF.ExportAsText := True;
  //        FSynExporterRTF.ExportAll(SL);
  //        FSynExporterRTF.SaveToStream(SS);
  //        SS.Position := 0;
  //        mmoPreview.LoadRichText(SS);
  //      finally
  //        SL.EndUpdate;
  //        FreeAndNil(SS);
  //      end;
  //    finally
  //      FreeAndNil(SL);
  //    end;
  //  end;
  //finally
  //  EndFormUpdate;
  //end;
end;
{$ENDREGION}

end.
