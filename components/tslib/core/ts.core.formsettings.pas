{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Core.FormSettings;

{$MODE DELPHI}

interface

uses
  Classes, Forms, Controls;

type
  TFormSettings = class(TPersistent)
  private
    FOnChanged   : TNotifyEvent;
    FWidth       : Integer;
    FHeight      : Integer;
    FLeft        : Integer;
    FTop         : Integer;
    FFormStyle   : TFormStyle;
    FWindowState : TWindowState;

    procedure SetFormStyle(AValue: TFormStyle);
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure SetWindowState(AValue: TWindowState);

  protected
    procedure Changed;

  public
    procedure AfterConstruction; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    property OnChanged: TNotifyEvent
      read FOnChanged write FOnChanged;

  published
    property Left: Integer
      read FLeft write SetLeft;

    property Top: Integer
      read FTop write SetTop;

    property Width: Integer
      read FWidth write SetWidth;

    property Height: Integer
      read FHeight write SetHeight;

    property FormStyle: TFormStyle
      read FFormStyle write SetFormStyle;

    property WindowState: TWindowState
      read FWindowState write SetWindowState;
  end;

implementation

{$REGION 'construction and destruction'}

procedure TFormSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FWidth  := 800;
  FHeight := 600;
end;

{$ENDREGION}

{$REGION 'property access mehods'}

procedure TFormSettings.SetWindowState(AValue: TWindowState);
begin
  if (AValue <> WindowState) and (AValue <> wsMinimized) then
  begin
    FWindowState := AValue;
    Changed;
  end;
end;

procedure TFormSettings.SetFormStyle(AValue: TFormStyle);
begin
  if FFormStyle = AValue then Exit;
  FFormStyle := AValue;
  Changed;
end;

procedure TFormSettings.SetHeight(AValue: Integer);
begin
  if FHeight = AValue then Exit;
  FHeight := AValue;
  Changed;
end;

procedure TFormSettings.SetLeft(AValue: Integer);
begin
  if FLeft = AValue then Exit;
  FLeft := AValue;
  Changed;
end;

procedure TFormSettings.SetTop(AValue: Integer);
begin
  if FTop = AValue then Exit;
  FTop := AValue;
  Changed;
end;

procedure TFormSettings.SetWidth(AValue: Integer);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;
  Changed;
end;

{$ENDREGION}

{$REGION 'protected methods'}

procedure TFormSettings.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{$ENDREGION}

{$REGION 'public methods'}

procedure TFormSettings.Assign(Source: TPersistent);
var
  F : TForm;
begin
  if Source is TForm then
  begin
    F           := TForm(Source);
    Left        := F.Left;
    Top         := F.Top;
    Width       := F.Width;
    Height      := F.Height;
    FormStyle   := F.FormStyle;
    WindowState := F.WindowState;
  end
  else
    inherited;
end;

procedure TFormSettings.AssignTo(Dest: TPersistent);
var
  F : TForm;
begin
  if Dest is TForm then
  begin
    F             := TForm(Dest);
    F.Left        := Left;
    F.Top         := Top;
    F.Width       := Width;
    F.Height      := Height;
    F.FormStyle   := FormStyle;
    F.WindowState := WindowState;
  end
  else
    inherited;
end;

{$ENDREGION}

end.
