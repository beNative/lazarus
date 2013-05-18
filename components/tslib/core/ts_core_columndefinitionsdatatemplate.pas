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

{ Added by Tim Sinaeve: GetValueForProperty }

unit ts_Core_ColumnDefinitionsDataTemplate;

{$mode delphi}

interface

uses
  ts_Core_DataTemplates, ts_Core_ColumnDefinitions;

type
  TColumnDefinitionsDataTemplate = class(TDataTemplate)
  protected
    FColumnDefinitions: TColumnDefinitions;
  public
    constructor Create(AColumnDefinitions: TColumnDefinitions);

    function CustomDraw(const Item: TObject; const ColumnIndex: Integer;
      TargetCanvas: TCanvas; CellRect: TRect; ImageList: TCustomImageList;
      DrawMode: TDrawMode): Boolean; override;

    function GetText(const Item: TObject; const ColumnIndex: Integer): string; override;
    function GetValueForProperty(const Item: TObject; const APropertyName: string): Variant; virtual;

    property ColumnDefinitions: TColumnDefinitions
      read FColumnDefinitions;
  end;

implementation

uses
  TypInfo, Variants;

{ TColumnDefinitionsDataTemplate }

constructor TColumnDefinitionsDataTemplate.Create(
  AColumnDefinitions: TColumnDefinitions);
begin
  inherited Create;
  FColumnDefinitions := AColumnDefinitions;
end;

function TColumnDefinitionsDataTemplate.CustomDraw(const Item: TObject;
  const ColumnIndex: Integer; TargetCanvas: TCanvas; CellRect: TRect;
  ImageList: TCustomImageList; DrawMode: TDrawMode): Boolean;
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
        TargetCanvas, CellRect, ImageList, DrawMode, True);
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
          FColumnDefinitions.Owner, FColumnDefinitions[ColumnIndex], Item);
      end
      else
      begin
        Result := VarToStrDef(GetValueForProperty(Item, CD.Name), '');
      end;
    end;
  end
  else
  begin
    Result := inherited;
  end;
end;

function TColumnDefinitionsDataTemplate.GetValueForProperty(const Item: TObject;
  const APropertyName: string): Variant;
begin
  if Assigned(Item) and IsPublishedProp(Item, APropertyName) then
    Result := GetPropValue(Item, APropertyName, True)
  else
    Result := '';
end;

end.
