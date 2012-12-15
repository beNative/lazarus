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

unit ts_Core_DataTemplates;

{$mode delphi}

interface

uses
  Graphics, ImgList, Types, Contnrs;

type
  TCanvas          = Graphics.TCanvas;
  TCustomImageList = ImgList.TCustomImageList;
  TRect            = Types.TRect;
  TDrawMode = (
    dmBeforeCellPaint,
    dmAfterCellPaint,
    dmPaintText
  );

  IDataTemplate = interface
    // methods to display items
    function CustomDraw(const Item: TObject; const ColumnIndex: Integer;
      TargetCanvas: TCanvas; CellRect: TRect; ImageList: TCustomImageList;
      DrawMode: TDrawMode): Boolean;
    function GetImageIndex(const Item: TObject; const ColumnIndex: Integer): Integer;
    function GetText(const Item: TObject; const ColumnIndex: Integer): string;

    // methods to edit items
    procedure SetText(const Item: TObject; const ColumnIndex: Integer;
      const Value: string);

    // methods to build the tree structure
    function GetItem(const Item: TObject; const Index: Integer): TObject;
    function GetItemCount(const Item: TObject): Integer;
    function GetItems(const Item: TObject): TObjectList;
    function GetItemTemplate(const Item: TObject): IDataTemplate;

    // methods to manage the template "binding"
    function GetTemplateDataClass: TClass;
    procedure RegisterDataTemplate(const DataTemplate: IDataTemplate);
  end;

  TDataTemplate = class(TInterfacedObject, IDataTemplate)
  private
    FTemplates: TObjectList;
  protected
    property Templates: TObjectList read FTemplates;
  public
    constructor Create;
    destructor Destroy; override;

    function CustomDraw(const Item: TObject; const ColumnIndex: Integer;
      TargetCanvas: TCanvas; CellRect: TRect; ImageList: TCustomImageList;
      DrawMode: TDrawMode): Boolean; virtual;
    function GetImageIndex(const Item: TObject;
      const ColumnIndex: Integer): Integer; virtual;
    function GetText(const Item: TObject;
      const ColumnIndex: Integer): string; virtual;

    procedure SetText(const Item: TObject; const ColumnIndex: Integer;
      const Value: string); virtual;

    function GetItem(const Item: TObject;
      const Index: Integer): TObject; virtual;
    function GetItemCount(const Item: TObject): Integer; virtual;
    function GetItems(const Item: TObject): TObjectList; virtual;
    function GetItemTemplate(const Item: TObject): IDataTemplate; virtual;

    function GetTemplateDataClass: TClass; virtual;
    procedure RegisterDataTemplate(const DataTemplate: IDataTemplate);
  end;

implementation

constructor TDataTemplate.Create;
begin
  FTemplates := TObjectList.Create();
end;

destructor TDataTemplate.Destroy;
begin
  FTemplates.Free();
  inherited;
end;

function TDataTemplate.CustomDraw(const Item: TObject;
  const ColumnIndex: Integer; TargetCanvas: TCanvas; CellRect: TRect;
  ImageList: TCustomImageList; DrawMode: TDrawMode): Boolean;
begin
  Result := False;
end;

function TDataTemplate.GetImageIndex(const Item: TObject;
  const ColumnIndex: Integer): Integer;
begin
  Result := -1;
end;

function TDataTemplate.GetItem(const Item: TObject;
  const Index: Integer): TObject;
begin
  Result := nil;
  if Item is TObjectList then
  begin;
    if (TObjectList(Item).Count > Index) then
    begin
      Result := TObjectList(Item).Items[Index];
    end;
  end;
end;

function TDataTemplate.GetItemCount(const Item: TObject): Integer;
begin
  Result := 0;

  if Assigned(Item) then
  begin
    Result := TObjectList(Item).Count;
  end;
end;

function TDataTemplate.GetItems(const Item: TObject): TObjectList;
begin
  Result := nil;
end;

function TDataTemplate.GetItemTemplate(const Item: TObject): IDataTemplate;
var
  I: Integer;
  LTemplate: IDataTemplate;
begin
  Result := Self;
  for I := 0 to FTemplates.Count - 1 do
  begin
    LTemplate := IDataTemplate(TDataTemplate(FTemplates[I]));
    if Assigned(Item) and (Item.InheritsFrom(LTemplate.GetTemplateDataClass)) then
    begin
      Result := LTemplate.GetItemTemplate(Item);
      Break;
    end;
  end;
end;

function TDataTemplate.GetTemplateDataClass: TClass;
begin
  Result := TObjectList;
end;

function TDataTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  Result := '(not available)';
end;

procedure TDataTemplate.RegisterDataTemplate(const DataTemplate: IDataTemplate);
begin
  FTemplates.Add(TDataTemplate(DataTemplate));
end;

procedure TDataTemplate.SetText(const Item: TObject; const ColumnIndex: Integer;
  const Value: string);
begin

end;

end.
