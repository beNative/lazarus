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

unit ts.Editor.ViewList.Data;

{$MODE Delphi}

interface

uses
  Classes,

  ts.Editor.Interfaces;

type
  TEditorViewInfo = class
  private
    FView: TComponent; // TS: no interface reference here!

    function GetFileName: string;
    function GetHighlighter: string;
    function GetModified: Boolean;
    function GetPath: string;
    function GetView: IEditorView;

  public
    constructor Create(AView: IEditorView);
    procedure BeforeDestruction; override;

    property View: IEditorView
      read GetView;

  published
    property FileName: string
      read GetFileName;

    property Path: string
      read GetPath;

    property Highlighter: string
      read GetHighlighter;

    property Modified: Boolean
      read GetModified;
  end;

implementation

{$region 'construction and destruction' /fold}
constructor TEditorViewInfo.Create(AView: IEditorView);
begin
  FView := AView.Form;
end;

procedure TEditorViewInfo.BeforeDestruction;
begin
  FView := nil;
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'property access mehods' /fold}
function TEditorViewInfo.GetFileName: string;
begin
  Result := View.Form.Caption;
end;

function TEditorViewInfo.GetHighlighter: string;
begin
  Result := View.HighlighterItem.Name;
end;

function TEditorViewInfo.GetModified: Boolean;
begin
  Result := View.Modified;
end;

function TEditorViewInfo.GetPath: string;
begin
  Result := View.FileName;
end;

function TEditorViewInfo.GetView: IEditorView;
begin
  Result := FView as IEditorView;
end;
{$endregion}

end.

