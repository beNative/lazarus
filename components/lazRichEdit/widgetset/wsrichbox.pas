{
lzRichEdit

Copyright (C) 2010 Elson Junio elsonjunio@yahoo.com.br

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at your
option) any later version with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify
this library, you may extend this exception to your version of the library,
but you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

This library is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
for more details.

You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit WSRichBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, WSStdCtrls, WSRichBoxFactory, Graphics, RichBox;

type

  { TWSCustomRichBox }

  TWSCustomRichBox = class(TWSCustomMemo)
    class function Font_GetCharset(const AWinControl: TWinControl): TFontCharset; virtual;
    class function Font_GetColor(const AWinControl: TWinControl): TColor; virtual;
    class function Font_GetName(const AWinControl: TWinControl): TFontName; virtual;
    class function Font_GetPitch(const AWinControl: TWinControl): TFontPitch; virtual;
    class function Font_GetProtected(const AWinControl: TWinControl): Boolean; virtual;
    class function Font_GetSize(const AWinControl: TWinControl): Integer; virtual;
    class function Font_GetStyle(const AWinControl: TWinControl): TFontStyles; virtual;
    //
    class function Para_GetAlignment(const AWinControl: TWinControl): TAlignment; virtual;
    class function Para_GetFirstIndent(const AWinControl: TWinControl): Longint; virtual;
    class function Para_GetLeftIndent(const AWinControl: TWinControl): Longint; virtual;
    class function Para_GetRightIndent(const AWinControl: TWinControl): Longint; virtual;
    class function Para_GetNumbering(const AWinControl: TWinControl): TNumberingStyle; virtual;
    class function Para_GetTab(const AWinControl: TWinControl; Index: Byte): Longint; virtual;
    class function Para_GetTabCount(const AWinControl: TWinControl): Integer; virtual;
    //
    class procedure Font_SetCharset(const AWinControl: TWinControl; Value: TFontCharset); virtual;
    class procedure Font_SetColor(const AWinControl: TWinControl; Value: TColor); virtual;
    class procedure Font_SetName(const AWinControl: TWinControl; Value: TFontName); virtual;
    class procedure Font_SetPitch(const AWinControl: TWinControl; Value: TFontPitch); virtual;
    class procedure Font_SetProtected(const AWinControl: TWinControl; Value: Boolean); virtual;
    class procedure Font_SetSize(const AWinControl: TWinControl; Value: Integer);  virtual;
    class procedure Font_SetStyle(const AWinControl: TWinControl; Value: TFontStyles); virtual;
    //
    class procedure Para_SetAlignment(const AWinControl: TWinControl; Value: TAlignment); virtual;
    class procedure Para_SetFirstIndent(const AWinControl: TWinControl; Value: Longint); virtual;
    class procedure Para_SetLeftIndent(const AWinControl: TWinControl; Value: Longint); virtual;
    class procedure Para_SetRightIndent(const AWinControl: TWinControl; Value: Longint); virtual;
    class procedure Para_SetNumbering(const AWinControl: TWinControl; Value: TNumberingStyle); virtual;
    class procedure Para_SetTab(const AWinControl: TWinControl; Index: Byte; Value: Longint); virtual;
    class procedure Para_SetTabCount(const AWinControl: TWinControl; Value: Integer); virtual;
    //
    class procedure SaveToStream (const AWinControl: TWinControl; var Stream: TStream); virtual;
    class procedure LoadFromStream (const AWinControl: TWinControl; const Stream: TStream); virtual;
    //
    class function GetTextBuf (const AWinControl: TWinControl):String; virtual;
    class function GetTextSel (const AWinControl: TWinControl):String; virtual;

  end;
  TWSCustomRichBoxClass = class of TWSCustomRichBox;

{ WidgetSetRegistration }

procedure RegisterCustomRichBox;
implementation

procedure RegisterCustomRichBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  // TS outcomment for QT
  //WSRegisterCustomRichBox;
  Done := True;
end;

{ TWSCustomRichBox }

class function TWSCustomRichBox.Font_GetCharset(const AWinControl: TWinControl
  ): TFontCharset;
begin

end;

class function TWSCustomRichBox.Font_GetColor(const AWinControl: TWinControl
  ): TColor;
begin

end;

class function TWSCustomRichBox.Font_GetName(const AWinControl: TWinControl
  ): TFontName;
begin

end;

class function TWSCustomRichBox.Font_GetPitch(const AWinControl: TWinControl
  ): TFontPitch;
begin

end;

class function TWSCustomRichBox.Font_GetProtected(const AWinControl: TWinControl
  ): Boolean;
begin

end;

class function TWSCustomRichBox.Font_GetSize(const AWinControl: TWinControl
  ): Integer;
begin

end;

class function TWSCustomRichBox.Font_GetStyle(const AWinControl: TWinControl
  ): TFontStyles;
begin

end;

class function TWSCustomRichBox.Para_GetAlignment(const AWinControl: TWinControl
  ): TAlignment;
begin

end;

class function TWSCustomRichBox.Para_GetFirstIndent(
  const AWinControl: TWinControl): Longint;
begin

end;

class function TWSCustomRichBox.Para_GetLeftIndent(
  const AWinControl: TWinControl): Longint;
begin

end;

class function TWSCustomRichBox.Para_GetRightIndent(
  const AWinControl: TWinControl): Longint;
begin

end;

class function TWSCustomRichBox.Para_GetNumbering(const AWinControl: TWinControl
  ): TNumberingStyle;
begin

end;

class function TWSCustomRichBox.Para_GetTab(const AWinControl: TWinControl;
  Index: Byte): Longint;
begin

end;

class function TWSCustomRichBox.Para_GetTabCount(const AWinControl: TWinControl
  ): Integer;
begin

end;

class procedure TWSCustomRichBox.Font_SetCharset(
  const AWinControl: TWinControl; Value: TFontCharset);
begin

end;

class procedure TWSCustomRichBox.Font_SetColor(const AWinControl: TWinControl;
  Value: TColor);
begin

end;

class procedure TWSCustomRichBox.Font_SetName(const AWinControl: TWinControl;
  Value: TFontName);
begin

end;

class procedure TWSCustomRichBox.Font_SetPitch(const AWinControl: TWinControl;
  Value: TFontPitch);
begin

end;

class procedure TWSCustomRichBox.Font_SetProtected(
  const AWinControl: TWinControl; Value: Boolean);
begin

end;

class procedure TWSCustomRichBox.Font_SetSize(const AWinControl: TWinControl;
  Value: Integer);
begin

end;

class procedure TWSCustomRichBox.Font_SetStyle(const AWinControl: TWinControl;
  Value: TFontStyles);
begin

end;

class procedure TWSCustomRichBox.Para_SetAlignment(
  const AWinControl: TWinControl; Value: TAlignment);
begin

end;

class procedure TWSCustomRichBox.Para_SetFirstIndent(
  const AWinControl: TWinControl; Value: Longint);
begin

end;

class procedure TWSCustomRichBox.Para_SetLeftIndent(
  const AWinControl: TWinControl; Value: Longint);
begin

end;

class procedure TWSCustomRichBox.Para_SetRightIndent(
  const AWinControl: TWinControl; Value: Longint);
begin

end;

class procedure TWSCustomRichBox.Para_SetNumbering(
  const AWinControl: TWinControl; Value: TNumberingStyle);
begin

end;

class procedure TWSCustomRichBox.Para_SetTab(const AWinControl: TWinControl;
  Index: Byte; Value: Longint);
begin

end;

class procedure TWSCustomRichBox.Para_SetTabCount(
  const AWinControl: TWinControl; Value: Integer);
begin

end;

class procedure TWSCustomRichBox.SaveToStream(const AWinControl: TWinControl;
  var Stream: TStream);
begin

end;

class procedure TWSCustomRichBox.LoadFromStream(const AWinControl: TWinControl;
  const Stream: TStream);
begin

end;

class function TWSCustomRichBox.GetTextBuf(const AWinControl: TWinControl
  ): String;
begin

end;

class function TWSCustomRichBox.GetTextSel(const AWinControl: TWinControl
  ): String;
begin

end;



end.
