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

unit ts_Core_FormSettings;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, Forms, Controls;

//=============================================================================

type
  TFormSettings = class(TPersistent)
  private
    FWidth       : Integer;
    FHeight      : Integer;
    FLeft        : Integer;
    FTop         : Integer;
    FFormStyle   : TFormStyle;
    FWindowState : TWindowState;
    procedure SetWindowState(AValue: TWindowState);

  public
    procedure AfterConstruction; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property Left: Integer
      read FLeft write FLeft;

    property Top: Integer
      read FTop write FTop;

    property Width: Integer
      read FWidth write FWidth;

    property Height: Integer
      read FHeight write FHeight;

    property FormStyle: TFormStyle
      read FFormStyle write FFormStyle;

    property WindowState: TWindowState
      read FWindowState write SetWindowState;
  end;

//*****************************************************************************

implementation

{$region 'construction and destruction' /fold}
//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TFormSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FWidth  := 800;
  FHeight := 600;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************
{$endregion}

{$region 'property access mehods' /fold}
//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

procedure TFormSettings.SetWindowState(AValue: TWindowState);
begin
  if (AValue <> WindowState) and (AValue <> wsMinimized) then
  begin
    FWindowState := AValue;
  end;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************
{$endregion}

{$region 'public methods' /fold}
//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

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

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************
{$endregion}

end.
