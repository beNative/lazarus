{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

(*
  Copyright (c) 2011, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

{
  Modifications by Tim Sinaeve:
  - GetValueForProperty and SetValueForProperty
  - cleanup accelerator chars.
}

unit ts.Core.ColumnDefinitionsDataTemplate;

{$MODE Delphi}

interface

uses
  Graphics,

  ts.Core.Value, ts.Core.DataTemplates, ts.Core.ColumnDefinitions;

type

  { TColumnDefinitionsDataTemplate }

  TColumnDefinitionsDataTemplate = class(TDataTemplate, IDataTemplate)
  protected
    FColumnDefinitions: TColumnDefinitions;
  public
    constructor Create(AColumnDefinitions: TColumnDefinitions);
    procedure BeforeDestruction; override;

    function CustomDraw(const Item: TObject; const ColumnIndex: Integer;
      TargetCanvas: TCanvas; CellRect: TRect; ImageList: TCustomImageList;
      DrawMode: TDrawMode; IsSelected: Boolean): Boolean; override;

    function GetText(const Item: TObject; const ColumnIndex: Integer): string; override;
    function GetValue(const Item: TObject; const ColumnIndex: Integer
       ): TValue; override;
    procedure SetText(const Item: TObject; const ColumnIndex: Integer;
       const Value: string); override;
    procedure SetValue(const Item: TObject; const ColumnIndex: Integer;
       const Value: TValue); override;
    function GetValueForProperty(const Item: TObject; const APropertyName: string): TValue; virtual;
    procedure SetValueForProperty(const Item: TObject; const APropertyName: string;
      const AValue: TValue); virtual;

    property ColumnDefinitions: TColumnDefinitions
      read FColumnDefinitions;
  end;

implementation

uses
  TypInfo, Variants, SysUtils,

  LCLProc,

  ts.Core.Utils;

{ TColumnDefinitionsDataTemplate }

constructor TColumnDefinitionsDataTemplate.Create(
  AColumnDefinitions: TColumnDefinitions);
begin
  inherited Create;
  FColumnDefinitions := AColumnDefinitions;
end;

procedure TColumnDefinitionsDataTemplate.BeforeDestruction;
begin
  FColumnDefinitions := nil;
  inherited BeforeDestruction;
end;

function TColumnDefinitionsDataTemplate.CustomDraw(const Item: TObject;
  const ColumnIndex: Integer; TargetCanvas: TCanvas; CellRect: TRect;
  ImageList: TCustomImageList; DrawMode: TDrawMode; IsSelected: Boolean
  ): Boolean;
var
  CD: TColumnDefinition;
begin
  if Assigned(Item) and Assigned(FColumnDefinitions)
    and (ColumnIndex < FColumnDefinitions.Count) and (ColumnIndex > -1) then
  begin
    CD := TColumnDefinition(FColumnDefinitions[ColumnIndex]);
    if Assigned(CD.OnCustomDraw) then
    begin
      Result := CD.OnCustomDraw(FColumnDefinitions.Owner, CD, Item,
        TargetCanvas, CellRect, ImageList, DrawMode, IsSelected);
    end
    else
    begin
      Result := inherited;
    end;
  end
  else
  begin
    Result := inherited;
  end;
end;

function TColumnDefinitionsDataTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  if Assigned(Item) and Assigned(FColumnDefinitions)
    and (ColumnIndex < FColumnDefinitions.Count) and (ColumnIndex > -1) then
  begin
    Result := GetValue(Item, ColumnIndex).AsString;
  end
  else
  begin
    Result := inherited;
  end;
end;

function TColumnDefinitionsDataTemplate.GetValue(const Item: TObject;
  const ColumnIndex: Integer): TValue;
var
  CD: TColumnDefinition;
begin
  if Assigned(Item) and Assigned(FColumnDefinitions)
    and (ColumnIndex < FColumnDefinitions.Count) and (ColumnIndex > -1) then
  begin
    CD := TColumnDefinition(FColumnDefinitions[ColumnIndex]);
    if Assigned(CD) then
    begin
      if Assigned(CD.OnGetText) then
      begin
        Result := CD.OnGetText(
          FColumnDefinitions.Owner,
          FColumnDefinitions[ColumnIndex],
          Item
        );
      end
      else
      begin
        Result := GetValueForProperty(Item, CD.Name);
      end;
    end;
  end
  else
  begin
    Result := inherited;
  end;
end;

procedure TColumnDefinitionsDataTemplate.SetText(const Item: TObject;
  const ColumnIndex: Integer; const Value: string);
var
  CD: TColumnDefinition;
begin
  if Assigned(Item) and Assigned(FColumnDefinitions)
    and (ColumnIndex < FColumnDefinitions.Count) and (ColumnIndex > -1) then
  begin
    CD := TColumnDefinition(FColumnDefinitions[ColumnIndex]);
    if Assigned(CD) then
    begin
      if Assigned(CD.OnSetText) then
      begin
        CD.OnSetText(FColumnDefinitions.Owner, CD, Item, Value);
      end
      else
      begin
        SetValueForProperty(Item, CD.Name, Value);
      end;
    end;
  end
  else
    inherited SetText(Item, ColumnIndex, Value);
end;

procedure TColumnDefinitionsDataTemplate.SetValue(const Item: TObject;
  const ColumnIndex: Integer; const Value: TValue);
var
  CD: TColumnDefinition;
begin
  if Assigned(Item) and Assigned(FColumnDefinitions)
    and (ColumnIndex < FColumnDefinitions.Count) and (ColumnIndex > -1) then
  begin
    CD := TColumnDefinition(FColumnDefinitions[ColumnIndex]);
    if Assigned(CD) then
    begin
      if Assigned(CD.OnSetText) then
      begin
        CD.OnSetText(FColumnDefinitions.Owner, CD, Item, Value.AsString);
      end
      else
      begin
        SetValueForProperty(Item, CD.Name, Value);
      end;
    end;
  end
  else
    inherited SetValue(Item, ColumnIndex, Value);
end;

function TColumnDefinitionsDataTemplate.GetValueForProperty(
  const Item: TObject; const APropertyName: string): TValue;
var
  V: Variant;
  S : string;
begin
  if Assigned(Item) and IsPublishedProp(Item, APropertyName) then
  begin
    V := GetPropValue(Item, APropertyName, False);
    S := VarToStrDef(V, '');
    if APropertyName = 'Caption' then // cleanup accelerator tokens
      DeleteAmpersands(S);
    Result := S;
  end
  else
    Result := TValue.Null;
end;

procedure TColumnDefinitionsDataTemplate.SetValueForProperty(
  const Item: TObject; const APropertyName: string; const AValue: TValue);
begin
  if Assigned(Item) and IsPublishedProp(Item, APropertyName) then
    SetPropValue(Item, APropertyName, AValue.AsString);
end;

end.
