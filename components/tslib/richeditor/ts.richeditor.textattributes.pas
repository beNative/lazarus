{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

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
  TTextAttributes = class(TPersistent)
  private
    FFontParams    : TFontParams;
    FParaMetric    : TParaMetric;
    FParaAlignment : TParaAlignment;

    FUpdateLock : Integer;
    FOnUpdate   : TNotifyEvent;
    FEditor     : TRichMemo;
    FSelStart   : Integer;
    FSelLength  : Integer;

    procedure Changed;
    procedure DoUpdate;

    function GetBold: Boolean;
    function GetColor: TColor;
    function GetItalic: Boolean;
    function GetName: string;
    function GetSize: Integer;
    function GetUnderline: Boolean;
    procedure SetBold(const AValue: Boolean);
    procedure SetColor(const AValue: TColor);
    procedure SetItalic(const AValue: Boolean);
    procedure SetName(const AValue: string);
    procedure SetSize(const AValue: Integer);
    procedure SetUnderline(const AValue: Boolean);

    function PositionChanged: Boolean;

  public
    constructor Create(AEditor: TRichMemo); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure UpdateAttributes;
    procedure BeginUpdate;
    procedure EndUpdate;


    property OnUpdate: TNotifyEvent
      read FOnUpdate write FOnUpdate;

    //property TextParams: RichBox.TTextAttributes
    //  read GetTextParams;

    property Name: string
      read GetName write SetName;

    property Color: TColor
      read GetColor write SetColor;

    property Size: Integer
      read GetSize write SetSize;

    property Bold: Boolean
      read GetBold write SetBold;

    property Italic: Boolean
      read GetItalic write SetItalic;

    property Underline: Boolean
      read GetUnderline write SetUnderline;
  end;

implementation

{$region 'construction and destruction' /fold}
constructor TTextAttributes.Create(AEditor: TRichMemo);
begin
  inherited Create;
  FEditor := AEditor;
end;

procedure TTextAttributes.AfterConstruction;
begin
  inherited AfterConstruction;
end;

procedure TTextAttributes.BeforeDestruction;
begin
  inherited BeforeDestruction;
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

function TTextAttributes.GetName: string;
begin
  Result := FFontParams.Name;
end;

procedure TTextAttributes.SetName(const AValue: string);
begin
  if AValue <> Name then
  begin
    FFontParams.Name := AValue;
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
{$endregion}







//function TTextAttributes.GetTextParams: RichBox.TTextAttributes;
//begin
//  UpdateAttributes;
//  Result := FTextParams;
//end;

procedure TTextAttributes.Changed;
begin
  if FUpdateLock = 0 then
    DoUpdate;
end;

procedure TTextAttributes.DoUpdate;
begin
  FEditor.SetTextAttributes(
    FSelStart,
    FSelLength,
    FFontParams
  );
  if Assigned(OnUpdate) then
    OnUpdate(Self);
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
    //begin
    //  FEditor.SetSelection(FEditor.SelStart, 1, True);
    //  FEditor.GetTextAttributes(FEditor.SelStart, FTextParams);
    //  FEditor.SetSelection(FEditor.SelStart, 0, True);
    //end
    //else
    //  FEditor.GetTextAttributes(FEditor.SelStart, FTextParams);
    FSelStart  := FEditor.SelStart;
    FSelLength := FEditor.SelLength;
  end;
end;

end.

