{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.RichEditor.TextAttributes;

{$MODE Delphi}

{ Text attributes for RichMemo }

interface

uses
  Classes, SysUtils, Graphics,

  RichMemo;

type

  { TTextAttributes }

  TTextAttributes = class(TPersistent)
  private
    FFontParams    : TFontParams;
    FParaMetric    : TParaMetric;
    FParaAlignment : TParaAlignment;
    FParaNumbering : TParaNumbering;

    FUpdateLock : Integer;
    FOnUpdate   : TNotifyEvent;
    FEditor     : TRichMemo;
    FSelStart   : Integer;
    FSelLength  : Integer;

    procedure Changed;
    procedure DoUpdate;

    function GetAlignment: TParaAlignment;
    function GetBkColor: TColor;
    function GetBold: Boolean;
    function GetColor: TColor;
    function GetHasBkColor: Boolean;
    function GetItalic: Boolean;
    function GetFontName: string;
    function GetNumberingStyle: TParaNumStyle;
    function GetSize: Integer;
    function GetUnderline: Boolean;
    procedure SetAlignment(AValue: TParaAlignment);
    procedure SetBkColor(AValue: TColor);
    procedure SetBold(const AValue: Boolean);
    procedure SetColor(const AValue: TColor);
    procedure SetHasBkColor(AValue: Boolean);
    procedure SetItalic(const AValue: Boolean);
    procedure SetFontName(const AValue: string);
    procedure SetNumberingStyle(AValue: TParaNumStyle);
    procedure SetSize(const AValue: Integer);
    procedure SetUnderline(const AValue: Boolean);

    function PositionChanged: Boolean;

  public
    constructor Create(AEditor: TRichMemo); reintroduce;
    procedure UpdateAttributes;
    procedure BeginUpdate;
    procedure EndUpdate;

    property OnUpdate: TNotifyEvent
      read FOnUpdate write FOnUpdate;

    property FontName: string
      read GetFontName write SetFontName;

    property Color: TColor
      read GetColor write SetColor;

    property BkColor: TColor
      read GetBkColor write SetBkColor;

    property HasBkColor: Boolean
      read GetHasBkColor write SetHasBkColor;

    property Size: Integer
      read GetSize write SetSize;

    property Bold: Boolean
      read GetBold write SetBold;

    property Italic: Boolean
      read GetItalic write SetItalic;

    property Underline: Boolean
      read GetUnderline write SetUnderline;

    property Alignment: TParaAlignment
      read GetAlignment write SetAlignment;

    property NumberingStyle: TParaNumStyle
      read GetNumberingStyle write SetNumberingStyle;
  end;

implementation

uses
  ts.Core.Logger, ts.Core.SharedLogger;

{$region 'construction and destruction' /fold}
constructor TTextAttributes.Create(AEditor: TRichMemo);
begin
  inherited Create;
  FEditor := AEditor;
end;
{$endregion}

{$region 'property access mehods' /fold}
function TTextAttributes.GetBold: Boolean;
begin
  Result := fsBold in FFontParams.Style;
end;

procedure TTextAttributes.SetBold(const AValue: Boolean);
begin
  if AValue <> Bold then
  begin
    if AValue then
      FFontParams.Style := FFontParams.Style + [fsBold]
    else
      FFontParams.Style := FFontParams.Style - [fsBold];
    Changed;
  end;
end;

function TTextAttributes.GetHasBkColor: Boolean;
begin
  Result := FFontParams.HasBkClr;
end;

procedure TTextAttributes.SetHasBkColor(AValue: Boolean);
begin
  if AValue <> HasBkColor then
  begin
    FFontParams.HasBkClr := AValue;
    Changed;
  end;
end;

function TTextAttributes.GetColor: TColor;
begin
  Result := FFontParams.Color;
end;

procedure TTextAttributes.SetColor(const AValue: TColor);
begin
  if AValue <> Color then
  begin
    FFontParams.Color := AValue;
    Changed;
  end;
end;

function TTextAttributes.GetItalic: Boolean;
begin
  Result := fsItalic in FFontParams.Style;
end;

procedure TTextAttributes.SetItalic(const AValue: Boolean);
begin
  if AValue <> Italic then
  begin
    if AValue then
      FFontParams.Style := FFontParams.Style + [fsItalic]
    else
      FFontParams.Style := FFontParams.Style - [fsItalic];
    Changed;
  end;
end;

function TTextAttributes.GetFontName: string;
begin
  Result := FFontParams.Name;
end;

procedure TTextAttributes.SetFontName(const AValue: string);
begin
  if AValue <> FontName then
  begin
    FFontParams.Name := AValue;
    Changed;
  end;
end;

function TTextAttributes.GetNumberingStyle: TParaNumStyle;
begin
  Result := FParaNumbering.Style;
end;

procedure TTextAttributes.SetNumberingStyle(AValue: TParaNumStyle);
begin
  if AValue <> NumberingStyle then
  begin
    FParaNumbering.Style := AValue;
    Changed;
  end;
end;

function TTextAttributes.GetSize: Integer;
begin
  Result := FFontParams.Size;
end;

procedure TTextAttributes.SetSize(const AValue: Integer);
begin
  if AValue <> Size then
  begin
    FFontParams.Size := AValue;
    Changed;
  end;
end;

function TTextAttributes.GetUnderline: Boolean;
begin
  Result := fsUnderLine in FFontParams.Style;
end;

procedure TTextAttributes.SetUnderline(const AValue: Boolean);
begin
  if AValue <> Underline then
  begin
    if AValue then
      FFontParams.Style := FFontParams.Style + [fsUnderline]
    else
      FFontParams.Style := FFontParams.Style - [fsUnderline];
    Changed;
  end;
end;

function TTextAttributes.GetBkColor: TColor;
begin
  Result := FFontParams.BkColor;
end;

procedure TTextAttributes.SetBkColor(AValue: TColor);
begin
  if AValue <> BkColor then
  begin
    FFontParams.BkColor := AValue;
    Changed;
  end;
end;
{$endregion}

procedure TTextAttributes.Changed;
begin
  if FUpdateLock = 0 then
    DoUpdate;
end;

procedure TTextAttributes.DoUpdate;
begin
  FSelStart  := FEditor.SelStart;
  FSelLength := FEditor.SelLength;
  FEditor.SetTextAttributes(
    FSelStart,
    FSelLength,
    FFontParams
  );
  FEditor.SetParaAlignment(
    FSelStart,
    FSelLength,
    Alignment
  );
  Logger.Send('Start : %d, Length: %d', [FSelStart, FSelLength]);
  if Assigned(OnUpdate) then
    OnUpdate(Self);
end;

function TTextAttributes.GetAlignment: TParaAlignment;
begin
  Result := FParaAlignment;
end;

procedure TTextAttributes.SetAlignment(AValue: TParaAlignment);
begin
  if AValue <> Alignment then
  begin
    FParaAlignment := AValue;
    Changed;
  end;
end;

function TTextAttributes.PositionChanged: Boolean;
begin
  Result := (FEditor.SelStart <> FSelStart) or (FEditor.SelLength <> FSelLength);
end;

procedure TTextAttributes.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TTextAttributes.EndUpdate;
begin
  if FUpdateLock > 0 then
    Dec(FUpdateLock);
  if FUpdateLock = 0 then
    DoUpdate;
end;

procedure TTextAttributes.UpdateAttributes;
begin
  if PositionChanged then
  begin
    //if FEditor.SelLength = 0 then
    begin
      FSelStart  := FEditor.SelStart;
      FSelLength := FEditor.SelLength;
      FEditor.GetParaAlignment(FSelStart, FParaAlignment);
      FEditor.GetTextAttributes(FSelStart, FFontParams);
      FEditor.GetParaNumbering(FSelStart, FParaNumbering);
    end;
  end;
end;

end.

