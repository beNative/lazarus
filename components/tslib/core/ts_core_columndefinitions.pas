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
}

unit ts_Core_ColumnDefinitions;

{$mode delphi}

interface

uses
  Classes, Graphics, Controls, SysUtils, TypInfo,

  VirtualTrees,

  ts_Core_DataTemplates;

type
  TDataType = (dtNumeric, dtDateTime, dtString);

const
  CDefaultWidth = 100;
  CDefaultSpacing = 4;
  CDefaultMinWidth = 0;
  CDefaultMaxWidth = 1000;
  CDefaultDataType = dtString;

type
  TColumnDefinition = class;

  TCustomDrawEvent = function(Sender: TObject; ColumnDefinition: TColumnDefinition;
    Item: TObject; TargetCanvas: TCanvas; CellRect: TRect;
    ImageList: TCustomImageList; DrawMode: TDrawMode): Boolean of object;
  TGetTextEvent = function(Sender: TObject; ColumnDefinition: TColumnDefinition;
    Item: TObject): string of object;

  TColumnDefinition = class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FCaption: string;
    FCheckBox: Boolean;
    FDataType: TDataType;
    FFixed: Boolean;
    FMaxWidth: Integer;
    FMinWidth: Integer;
    FName: string;
    FOnCustomDraw: TCustomDrawEvent;
    FOnGetText: TGetTextEvent;
    FSpacing: Integer;
    FWidth: Integer;

    procedure SetCaption(const Value: string);
    procedure SetMinWidth(AValue: Integer);

  public
    constructor Create(ACollection: TCollection); override;

  published
    property Caption: string
      read FCaption write SetCaption;
    property OnCustomDraw: TCustomDrawEvent
      read FOnCustomDraw write FOnCustomDraw;
    property OnGetText: TGetTextEvent
      read FOnGetText write FOnGetText;
    property Width: Integer
      read FWidth write FWidth default CDefaultWidth;
    property MinWidth: Integer
      read FMinWidth write FMinWidth default CDefaultMinWidth;
    property MaxWidth: Integer
      read FMaxWidth write FMaxWidth default CDefaultMaxWidth;
    property CheckBox: Boolean
      read FCheckBox write FCheckBox;
    property Alignment: TAlignment
      read FAlignment write FAlignment;
    property Spacing: Integer
      read FSpacing write FSpacing default CDefaultSpacing;
    property DataType: TDataType
      read FDataType write FDataType default CDefaultDataType;
    property Name: string
      read FName write FName;
    property Fixed: Boolean
      read FFixed write FFixed;
  end;

  TColumnDefinitions = class(TOwnedCollection)
  private
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

  end;

implementation

{ TColumnDefinition }

constructor TColumnDefinition.Create(ACollection: TCollection);
begin
  inherited;
  FDataType := CDefaultDataType;
  FWidth := CDefaultWidth;
  FSpacing := CDefaultSpacing;
  FMinWidth := CDefaultMinWidth;
  FMaxWidth := CDefaultMaxWidth;
end;

procedure TColumnDefinition.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TColumnDefinition.SetMinWidth(AValue: Integer);
begin
  if FMinWidth = AValue then exit;
  FMinWidth := AValue;
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

function TColumnDefinitions.AddColumn(const AName: string; const ADataType: TDataType; AWidth: Integer; AMinWidth: Integer; AMaxWidth: Integer): TColumnDefinition;
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
  if (AMinWidth = -1) and (AMaxWidth = -1) then
  begin
    Result.Fixed := True;
  end;
end;

function TColumnDefinitions.AddColumn(const AName: string; const ACaption: string; const ADataType: TDataType; AWidth: Integer; AMinWidth: Integer; AMaxWidth: Integer): TColumnDefinition;
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
    if (AMinWidth = -1) and (AMaxWidth = -1) then
    begin
      Result.Fixed := True;
    end;
  end;
end;


end.
