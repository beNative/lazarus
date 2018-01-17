{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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
  Changes by Tim Sinaeve:
    - Ported to support FPC
    - Added TDataType = (dtNumeric, dtDateTime, dtString) to support column
      sorting based on the datatype.
    - Added Name property. So Caption can be different from the propertyname.
    - removed customfilter (no binding support)
    - column options added
    - MinWidth and MaxWidth
    - Added Margin
    - Fixed
}

unit ts.Core.ColumnDefinitions;

{$MODE DELPHI}

interface

uses
  Classes, Graphics, Controls, SysUtils, TypInfo,

  VirtualTrees,

  ts.Core.DataTemplates;

type
  TDataType = (
    dtNumeric,
    dtDateTime,
    dtString,
    dtBoolean
  );

const
  CDefaultWidth = 100;
  CDefaultSpacing = 4;
  CDefaultMargin  = 4;
  CDefaultMinWidth = 0;
  CDefaultMaxWidth = 1000;
  CDefaultDataType = dtString;

type
  TCanvas = Graphics.TCanvas;

  TColumnDefinition = class;
  TColumnDefinitions = class;

  TCustomDrawEvent = function(Sender: TObject; ColumnDefinition: TColumnDefinition;
    Item: TObject; TargetCanvas: TCanvas; CellRect: TRect;
    ImageList: TCustomImageList; DrawMode: TDrawMode; Selected: Boolean): Boolean of object;
  TGetHintEvent = function(Sender: TObject; ColumnDefinition: TColumnDefinition;
    Item: TObject): string of object;
  TGetImageIndexEvent = function(Sender: TObject; ColumnDefinition: TColumnDefinition;
    Item: TObject): Integer of object;
  TGetTextEvent = function(Sender: TObject; ColumnDefinition: TColumnDefinition;
    Item: TObject): string of object;
  TSetTextEvent = procedure(Sender: TObject; ColumnDefinition: TColumnDefinition;
    Item: TObject; const Value: string) of object;

  TColumnOption = (coResizable, coSortable, coDraggable);
  TColumnOptions = set of TColumnOption;
  TColumnType = (ctText, ctCheckBox, ctProgressBar, ctImage);
  TSortingDirection = (sdNone, sdAscending, sdDescending);
  TToggleMode = (tmNone, tmClick, tmDoubleClick);

  TColumnDefinition = class(TCollectionItem)
  private
    FAllowEdit: Boolean;
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FCaption: string;
    FColumnOptions: TColumnOptions;
    FColumnType: TColumnType;
    FDataType: TDataType;
    FFixed: Boolean;
    FMaxWidth: Integer;
    FMinWidth: Integer;
    FName: string;
    FOnCustomDraw: TCustomDrawEvent;
    FOnGetHint: TGetHintEvent;
    FOnGetImageIndex: TGetImageIndexEvent;
    FOnGetText: TGetTextEvent;
    FOnSetText: TSetTextEvent;
    FSortingDirection: TSortingDirection;
    FToggleMode: TToggleMode;
    FSpacing: Integer;
    FMargin: Integer;
    FVisible: Boolean;
    FWidth: Integer;

    function GetCollection: TColumnDefinitions;
    procedure SetAutoSize(AValue: Boolean);
    procedure SetCollection(const Value: TColumnDefinitions); reintroduce;
    procedure SetCaption(const Value: string);
    procedure SetMinWidth(AValue: Integer);
    procedure SetSortingDirection(AValue: TSortingDirection);

  protected
    function GetDisplayName: string; override;

    property Collection: TColumnDefinitions
      read GetCollection write SetCollection;

  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;

  published
    property AllowEdit: Boolean
      read FAllowEdit write FAllowEdit;

    property Alignment: TAlignment
      read FAlignment write FAlignment;

    property AutoSize: Boolean
      read FAutoSize write SetAutoSize default False;

    property Caption: string
      read FCaption write SetCaption;

   property ColumnOptions: TColumnOptions
     read FColumnOptions write FColumnOptions default [coResizable, coSortable, coDraggable];

    property ColumnType: TColumnType
      read FColumnType write FColumnType default ctText;

    property OnCustomDraw: TCustomDrawEvent
      read FOnCustomDraw write FOnCustomDraw;

    property OnGetHint: TGetHintEvent
      read FOnGetHint write FOnGetHint;

    property OnGetImageIndex: TGetImageIndexEvent
      read FOnGetImageIndex write FOnGetImageIndex;

    property OnGetText: TGetTextEvent
      read FOnGetText write FOnGetText;
	  
    property OnSetText: TSetTextEvent
      read FOnSetText write FOnSetText;

    property MinWidth: Integer
      read FMinWidth write FMinWidth default CDefaultMinWidth;

    property MaxWidth: Integer
      read FMaxWidth write FMaxWidth default CDefaultMaxWidth;

    property Spacing: Integer
      read FSpacing write FSpacing default CDefaultSpacing;

    property Margin: Integer
      read FMargin write FMargin default CDefaultMargin;

    property DataType: TDataType
      read FDataType write FDataType default CDefaultDataType;

    property Name: string
      read FName write FName;

    property Fixed: Boolean
      read FFixed write FFixed;

    property SortingDirection: TSortingDirection
      read FSortingDirection write SetSortingDirection default sdNone;

    property ToggleMode: TToggleMode
      read FToggleMode write FToggleMode default tmNone;

    property Visible: Boolean
      read FVisible write FVisible default True;

    property Width: Integer
      read FWidth write FWidth default CDefaultWidth;
  end;

  TColumnDefinitions = class(TOwnedCollection)
  private
    FMainColumnIndex: Integer;

    function GetItem(Index: Integer): TColumnDefinition;
    procedure SetItem(Index: Integer; AValue: TColumnDefinition);
  public
    function AddColumn(
      const AName     : string;
      const ADataType : TDataType = dtString;
            AWidth    : Integer = -1;
            AMinWidth : Integer = -1;
            AMaxWidth : Integer = -1
    ): TColumnDefinition; overload;
    function AddColumn(
      const AName     : string;
      const ACaption  : string;
      const ADataType : TDataType = dtString;
            AWidth    : Integer = -1;
            AMinWidth : Integer = -1;
            AMaxWidth : Integer = -1
    ): TColumnDefinition; overload;
    property Items[Index: Integer]: TColumnDefinition
      read GetItem write SetItem; default;

    property MainColumnIndex: Integer
      read FMainColumnIndex write FMainColumnIndex;

  end;

implementation

{ TColumnDefinition }

constructor TColumnDefinition.Create(ACollection: TCollection);
begin
  inherited;
  FColumnOptions := [coResizable, coSortable, coDraggable];
  FDataType := CDefaultDataType;
  FSpacing := CDefaultSpacing;
  FMargin := CDefaultMargin;
  FVisible := True;
  FMinWidth := CDefaultMinWidth;
  FMaxWidth := CDefaultMaxWidth;
  FWidth := CDefaultWidth;
end;

procedure TColumnDefinition.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

function TColumnDefinition.GetCollection: TColumnDefinitions;
begin
  Result := TColumnDefinitions(inherited Collection);
end;

function TColumnDefinition.GetDisplayName: string;
begin
  Result := FCaption;
end;

procedure TColumnDefinition.SetMinWidth(AValue: Integer);
begin
  if FMinWidth = AValue then exit;
  FMinWidth := AValue;
end;

procedure TColumnDefinition.Assign(Source: TPersistent);
var
  LSource: TColumnDefinition;
begin
  if Source is TColumnDefinition then
  begin
    LSource := TColumnDefinition(Source);

    AutoSize := LSource.AutoSize;
    Alignment := LSource.Alignment;
    Caption := LSource.Caption;
    DataType := LSource.DataType;
    AllowEdit := LSource.AllowEdit;
    ColumnOptions := LSource.ColumnOptions;
    ColumnType := LSource.ColumnType;
    MinWidth := LSource.MinWidth;
    MaxWidth := LSource.MaxWidth;
    Spacing := LSource.Spacing;
    Margin := LSource.Margin;
    Fixed := LSource.Fixed;
    OnCustomDraw := LSource.OnCustomDraw;
    OnGetText := LSource.OnGetText;
    SortingDirection := LSource.SortingDirection;
    ToggleMode := LSource.ToggleMode;
    Visible := LSource.Visible;
    Width := LSource.Width;
  end
  else
  begin
    inherited;
  end;
end;

procedure TColumnDefinition.SetAutoSize(AValue: Boolean);
var
  i: Integer;
begin
  FAutoSize := AValue;

  if FAutoSize then
  begin
    for i := 0 to Collection.Count - 1 do
    begin
      if Collection[i].AutoSize and (Collection[i] <> Self) then
      begin
        Collection[i].AutoSize := False;
      end;
    end;
  end;
end;

procedure TColumnDefinition.SetCollection(const Value: TColumnDefinitions);
begin
  inherited Collection := Value;
end;

procedure TColumnDefinition.SetSortingDirection(AValue: TSortingDirection);
var
  i: Integer;
begin
  FSortingDirection := AValue;
  if FSortingDirection <> sdNone then
  begin
    for i := 0 to Collection.Count - 1 do
    begin
      if (TColumnDefinition(Collection.Items[i]).SortingDirection <> sdNone)
        and (Collection.Items[i] <> Self) then
      begin
        TColumnDefinition(Collection.Items[i]).SortingDirection := sdNone;
      end;
    end;
  end;
end;

{ TColumnDefinitions }

function TColumnDefinitions.GetItem(Index: Integer): TColumnDefinition;
begin
  Result := (inherited Items[Index]) as TColumnDefinition;
end;

procedure TColumnDefinitions.SetItem(Index: Integer; AValue: TColumnDefinition);
begin
  inherited Items[Index] := AValue;
end;

function TColumnDefinitions.AddColumn(const AName: string;
  const ADataType: TDataType; AWidth: Integer; AMinWidth: Integer;
  AMaxWidth: Integer): TColumnDefinition;
begin
  Result := TColumnDefinition.Create(Self);
  Result.DataType := ADataType;
  Result.Name := AName;
  Result.Caption := AName;
  if AWidth >= 0 then
  begin
    Result.Width := AWidth;
    if AMinWidth = -1 then
    begin
      Result.MinWidth := AWidth;
    end
    else
    begin
      Result.MinWidth := AMinWidth;
    end;
    if AMaxWidth = -1 then
    begin
      Result.MaxWidth := AWidth;
    end
    else
    begin
      Result.MaxWidth := AMaxWidth;
    end;
  end;
end;

function TColumnDefinitions.AddColumn(const AName: string;
  const ACaption: string; const ADataType: TDataType; AWidth: Integer;
  AMinWidth: Integer; AMaxWidth: Integer): TColumnDefinition;
begin
  Result := TColumnDefinition.Create(Self);
  Result.DataType := ADataType;
  Result.Name := AName;
  Result.Caption := ACaption;
  if AWidth >= 0 then
  begin
    if AMinWidth = -1 then
    begin
      Result.MinWidth := AWidth;
    end
    else
    begin
      Result.MinWidth := AMinWidth;
    end;
    if AMaxWidth = -1 then
    begin
      Result.MaxWidth := AWidth;
    end
    else
    begin
      Result.MaxWidth := AMaxWidth;
    end;
  end;
end;

end.
