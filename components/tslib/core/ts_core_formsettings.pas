{
  Copyright (C) 2012 Tim Sinaeve tim.sinaeve@gmail.com

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
    FWidth     : Integer;
    FHeight    : Integer;
    FLeft      : Integer;
    FTop       : Integer;
    FFormStyle : TFormStyle;

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
  end;

//*****************************************************************************

implementation

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

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

procedure TFormSettings.Assign(Source: TPersistent);
var
  Form : TForm;
begin
  if Source is TForm then
  begin
    Form      := TForm(Source);
    Left      := Form.Left;
    Top       := Form.Top;
    Width     := Form.Width;
    Height    := Form.Height;
    FormStyle := Form.FormStyle;
  end
  else
    inherited;
end;

//-----------------------------------------------------------------------------

procedure TFormSettings.AssignTo(Dest: TPersistent);
var
  Form : TForm;
begin
  if Dest is TForm then
  begin
    Form           := TForm(Dest);
    Form.Left      := Left;
    Form.Top       := Top;
    Form.Width     := Width;
    Form.Height    := Height;
    Form.FormStyle := FormStyle;
  end
  else
    inherited;
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

initialization
  RegisterClass(TFormSettings);

end.
