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

unit Gtk2WSRichBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, LCLProc, Clipbrd, StdCtrls, WSRichBox,
  gtk2, glib2, gdk2, pango, Gtk2Proc, Gtk2Def, gdk2pixbuf, Gtk2Globals,
  Gtk2WSControls, RichBox, Gtk2RTFTool;

const BulletCode=$2022;

type

  TWSFontAttributes=record
    Charset: TFontCharset;
    Color: TColor;
    Name: TFontName;
    Pitch: TFontPitch;
    fProtected: Boolean;
    Size: Integer;
    Style: TFontStyles;
  end;

  TWSParaAttributes=record
    Alignment: TAlignment;
    FirstIndent: Integer;
    LeftIndent: Integer;
    RightIndent: Integer;
    Numbering: TNumberingStyle;
    Tab: Integer;
    TabCount: Integer;
  end;

  { TGtk2WSCustomRichBox }

  TGtk2WSCustomRichBox = class(TWSCustomRichBox)
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    //Funções de Fonte
    class function Font_GetColor(const AWinControl: TWinControl): TColor; override;
    class function Font_GetName(const AWinControl: TWinControl): TFontName; override;
    class function Font_GetSize(const AWinControl: TWinControl): integer; override;
    class function Font_GetStyle(const AWinControl: TWinControl): TFontStyles; override;
    //
    //Funções de Paragrafos
    class function Para_GetAlignment(const AWinControl: TWinControl): TAlignment; override;
    class function Para_GetFirstIndent(const AWinControl: TWinControl): Longint; override;
    class function Para_GetLeftIndent(const AWinControl: TWinControl): Longint; override;
    class function Para_GetRightIndent(const AWinControl: TWinControl): Longint; override;
    class function Para_GetNumbering(const AWinControl: TWinControl): TNumberingStyle; override;
    //
    //Procedimentos de Fonte
    class procedure Font_SetColor(const AWinControl: TWinControl; Value: TColor); override;
    class procedure Font_SetName(const AWinControl: TWinControl; Value: TFontName); override;
    class procedure Font_SetSize(const AWinControl: TWinControl; Value: integer); override;
    class procedure Font_SetStyle(const AWinControl: TWinControl; Value: TFontStyles); override;
    //
    //Procedimentos de Paragrafos
    class procedure Para_SetAlignment(const AWinControl: TWinControl; Value: TAlignment); override;
    class procedure Para_SetFirstIndent(const AWinControl: TWinControl; Value: Longint); override;
    class procedure Para_SetLeftIndent(const AWinControl: TWinControl; Value: Longint); override;
    class procedure Para_SetRightIndent(const AWinControl: TWinControl; Value: Longint); override;
    class procedure Para_SetNumbering(const AWinControl: TWinControl; Value: TNumberingStyle); override;
    class procedure Paste(const ACustomEdit: TCustomEdit); override;
    //
    class procedure SaveToStream(const AWinControl: TWinControl; var Stream: TStream); override;
    class procedure LoadFromStream(const AWinControl: TWinControl; const Stream: TStream); override;
    //
    class function GetTextBuf (const AWinControl: TWinControl):String; override;
    class function GetTextSel (const AWinControl: TWinControl):String; override;
  end;

  {Exceptional}
  function WGGetBuffer(const AWinControl: TWinControl; var Buffer: PGtkTextBuffer): boolean;
  function WGGetRealTextBuf(const AWinControl: TWinControl; var S:String): Boolean;
  function WGGetFontAttributes(const AWinControl: TWinControl; Position:Integer; var FontAttributes: TWSFontAttributes): boolean;
  function WGGetParaAttributes(const AWinControl: TWinControl; Position:Integer; var ParaAttributes: TWSParaAttributes): boolean;
  function WGGetPos_FirstCharacter_Line(const AWinControl: TWinControl; Position: integer): integer;
  function WGGetPos_LastCharacter_Line(const AWinControl: TWinControl; Position: integer): integer;
  function WGGetNumbering(const AWinControl: TWinControl; Position: integer):TNumberingStyle;
  procedure WGSetFormat(const iSelStart, iSelLength: integer; Buffer: PGtkTextBuffer; Tag: Pointer);
  Procedure WGDelete(const AWinControl: TWinControl; iSelStart, iSelLength: integer);
  procedure WGInsertPos(const AWinControl: TWinControl; Position:Integer; const S:String);
  procedure WGInsertImage(const AWinControl: TWinControl; Position: integer; Image: TPicture; ResizeWidth, ResizeHeight: integer);
  {...}
implementation
function WGGetBuffer(const AWinControl: TWinControl; var Buffer: PGtkTextBuffer
  ): boolean;
var
  AWidget: PGtkWidget;
begin
  Result := False;
  //--
  AWidget := PGtkWidget(AWinControl.Handle);
  AWidget := GetWidgetInfo(AWidget, False)^.CoreWidget;
  if not (Assigned(AWidget)) then
    Exit;
  //--
  Buffer := gtk_text_view_get_buffer(PGtkTextView(AWidget));
  if not (Assigned(Buffer)) then
    Exit;
  //--
  Result := True;
end;

function WGGetRealTextBuf(const AWinControl: TWinControl; var S: String
  ): Boolean;
var
  iterStart, iterEnd: TGtkTextIter;
  Buffer: PGtkTextBuffer=nil;
begin
  Result:= False;
  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;

  gtk_text_buffer_get_start_iter(Buffer, @iterStart);
  gtk_text_buffer_get_end_iter(Buffer, @iterEnd);
  S := gtk_text_buffer_get_slice(Buffer, @iterStart, @iterEnd, gboolean(False));

  Result:= True;
end;

function WGGetFontAttributes(const AWinControl: TWinControl; Position: Integer;
  var FontAttributes: TWSFontAttributes): boolean;
var
  Buffer: PGtkTextBuffer = nil;
  Attributes: PGtkTextAttributes;
  iPosition: TGtkTextIter;
begin
  Result:= False;
  //--
  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;

  //--
  Attributes := gtk_text_attributes_new;
  if not Assigned(Attributes) then
    Exit;
  //--
  gtk_text_buffer_get_iter_at_offset(buffer, @iPosition, Position);

  //--
  if (gtk_text_iter_get_attributes(@iPosition, Attributes)) then
  begin
    FontAttributes.Name := pango_font_description_get_family(Attributes^.font);
    FontAttributes.Size := pango_font_description_get_size(Attributes^.font);
    if not (pango_font_description_get_size_is_absolute(Attributes^.font)) then
      FontAttributes.Size := Round(FontAttributes.Size / PANGO_SCALE);
    FontAttributes.Color := TGDKColorToTColor(Attributes^.appearance.fg_color);
    //--
    FontAttributes.Style := [];
    //--
    if (Strikethrough(Attributes^.appearance) > 0) then
      Include(FontAttributes.Style, fsStrikeOut);
    if (underline(Attributes^.appearance) > 0) then
      Include(FontAttributes.Style, fsUnderline);
    //--
    if (pango_font_description_get_weight(Attributes^.font) = PANGO_WEIGHT_BOLD) then
      Include(FontAttributes.Style,fsBold);
    if (pango_font_description_get_style(Attributes^.font) = PANGO_STYLE_ITALIC) then
      Include(FontAttributes.Style,fsItalic);
    //--
    Result:= True;
  end;
  gtk_text_attributes_unref(Attributes);
end;

function WGGetParaAttributes(const AWinControl: TWinControl; Position: Integer;
  var ParaAttributes: TWSParaAttributes): boolean;
var
  Buffer: PGtkTextBuffer = nil;
  Attributes: PGtkTextAttributes;
  iPosition: TGtkTextIter;
begin
  Result:= False;
  //--
  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;

  //--
  Attributes := gtk_text_attributes_new;
  if not Assigned(Attributes) then
    Exit;
  //--
  gtk_text_buffer_get_iter_at_offset(buffer, @iPosition, Position);

  //--
  if (gtk_text_iter_get_attributes(@iPosition, Attributes)) then
  begin
    case Attributes^.justification of
      GTK_JUSTIFY_LEFT: ParaAttributes.Alignment := taLeftJustify;
      GTK_JUSTIFY_RIGHT: ParaAttributes.Alignment := taRightJustify;
      GTK_JUSTIFY_CENTER: ParaAttributes.Alignment := taCenter;
    end;
    //--
    ParaAttributes.LeftIndent  := Attributes^.left_margin div 37;
    ParaAttributes.RightIndent := Attributes^.right_margin div 37;
    ParaAttributes.FirstIndent := Attributes^.indent div 37;
    //--

    Result:= True;
  end;
  gtk_text_attributes_unref(Attributes);
end;

function WGGetPos_FirstCharacter_Line(const AWinControl: TWinControl;
  Position: integer): integer;
var
  S: string='';
  CH: TUTF8Char;
  I, I2: integer;
begin
  Result:= 0;
  if not(WGGetRealTextBuf(AWinControl, S)) then Exit;

  for I := Position downto 0 do
  begin
    CH := UTF8Copy(S, I, 1);
    if (UTF8CharacterToUnicode(@CH[1], I2) = $A) or (I = 0) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := Position;
end;

function WGGetPos_LastCharacter_Line(const AWinControl: TWinControl;
  Position: integer): integer;
var
  S: string='';
  CH: TUTF8Char;
  I, I2: integer;
  Len: integer;
begin
  Result:= 0;
  if not(WGGetRealTextBuf(AWinControl, S)) then Exit;
  Len := UTF8Length(S);
  for I := Position to Len do
  begin
    CH := UTF8Copy(S, I, 1);
    if (UTF8CharacterToUnicode(@CH[1], I2) = $A) or (I = Len) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := Position;
end;

function WGGetNumbering(const AWinControl: TWinControl; Position: integer
  ): TNumberingStyle;
var
  I: integer;
  CH: TUTF8Char;
  RText: String='';
begin
  Result:= nsNone;
  I := WGGetPos_FirstCharacter_Line(AWinControl, Position);
  if not(WGGetRealTextBuf(AWinControl, RText)) then Exit;

  CH:= UTF8Copy(RText, I + 1, 1);

  if (UTF8CharacterToUnicode(@CH[1], I) = BulletCode) then Result:= nsBullets;
end;

procedure WGSetFormat(const iSelStart,
  iSelLength: integer; Buffer: PGtkTextBuffer; Tag: Pointer);
var
  iterStart, iterEnd: TGtkTextIter;
begin
  gtk_text_buffer_get_iter_at_offset(buffer, @iterStart, iSelStart);
  gtk_text_buffer_get_iter_at_offset(buffer, @iterEnd, iSelStart + iSelLength);
  gtk_text_buffer_apply_tag(buffer, tag, @iterStart, @iterEnd);
end;

procedure WGDelete(const AWinControl: TWinControl; iSelStart,
  iSelLength: integer);
var
  iterStart, iterEnd: TGtkTextIter;
  Buffer: PGtkTextBuffer;
begin
  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;
  gtk_text_buffer_get_iter_at_offset(buffer, @iterStart, iSelStart);
  gtk_text_buffer_get_iter_at_offset(buffer, @iterEnd, iSelStart + iSelLength);
  gtk_text_buffer_delete(buffer, @iterStart, @iterEnd);
end;

procedure WGInsertPos(const AWinControl: TWinControl; Position: Integer;
  const S: String);
var
  iterStart: TGtkTextIter;
  Buffer: PGtkTextBuffer = nil;
  Ch: String;
  //N2:Boolean=False;
begin
  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;
  if (Position < 0) then Exit;
  //--
  Ch := S;
  gtk_text_buffer_get_iter_at_offset(Buffer, @iterStart, Position);
  gtk_text_buffer_insert(Buffer, @iterStart, @Ch[1], Length(Ch));
end;

procedure WGInsertImage(const AWinControl: TWinControl; Position: integer;
  Image: TPicture; ResizeWidth, ResizeHeight: integer);
var
  Buffer: PGtkTextBuffer = nil;
  iPosition: TGtkTextIter;
  GDIObj: PGDIObject = nil;
  pixbuf: PGDKPixBuf = nil;
  scaled: PGDKPixBuf = nil;
  pixmap: PGdkDrawable = nil;
  bitmap: PGdkBitmap = nil;
  Width, Height: integer;
begin

  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;

  //--
  gtk_text_buffer_get_iter_at_offset(buffer, @iPosition, Position);

  //--
  GDIObj := PGDIObject(Image.Bitmap.Handle);

  //--
  case GDIObj^.GDIBitmapType of
    gbBitmap:
    begin
      bitmap := GDIObj^.GDIBitmapObject;
      gdk_drawable_get_size(bitmap, @Width, @Height);
      pixbuf := CreatePixbufFromDrawable(bitmap, nil, False,
        0, 0, 0, 0, Width, Height);
    end;
    gbPixmap:
    begin
      pixmap := GDIObj^.GDIPixmapObject.Image;
      if pixmap <> nil then
      begin
        gdk_drawable_get_size(pixmap, @Width, @Height);
        bitmap := CreateGdkMaskBitmap(Image.Bitmap.Handle, 0);
        pixbuf := CreatePixbufFromImageAndMask(pixmap, 0, 0,
          Width, Height, nil, Bitmap);
      end;
    end;
    gbPixbuf:
    begin
      pixbuf := gdk_pixbuf_copy(GDIObj^.GDIPixbufObject);
    end;
  end;

  if (ResizeWidth > 1) and (ResizeHeight > 1) then
  begin
    scaled := gdk_pixbuf_scale_simple(pixbuf, ResizeWidth,
      ResizeHeight, GDK_INTERP_HYPER);
    g_object_unref(pixbuf);
    pixbuf := scaled;
  end;

  if (pixbuf <> nil) then
    gtk_text_buffer_insert_pixbuf(buffer, @iPosition, pixbuf);
end;


{ TGtk2WSCustomRichBox }

class function TGtk2WSCustomRichBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  Widget, TempWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Widget := gtk_scrolled_window_new(nil, nil);
  Result := TLCLIntfHandle(PtrUInt(Widget));
  if Result = 0 then
    Exit;

  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);

  TempWidget := gtk_text_view_new();
  gtk_container_add(PGtkContainer(Widget), TempWidget);

  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(Widget)^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(Widget)^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(Widget),
    GTK_POLICY_AUTOMATIC,
    GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(Widget),
    BorderStyleShadowMap[TCustomControl(AWinControl).BorderStyle]);

  SetMainWidget(Widget, TempWidget);
  GetWidgetInfo(Widget, True)^.CoreWidget := TempWidget;

  gtk_text_view_set_editable(PGtkTextView(TempWidget), True);

  if TCustomRichBox(AWinControl).WordWrap then
    gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_WORD)
  else
    gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_NONE);

gtk_text_view_set_accepts_tab(PGtkTextView(TempWidget), TCustomRichBox(AWinControl).WantTabs);

  gtk_widget_show_all(Widget);

  Set_RC_Name(AWinControl, Widget);

  TGtk2WSWinControl.SetCallbacks(PGtkObject(Widget), TComponent(WidgetInfo^.LCLObject));
end;

class function TGtk2WSCustomRichBox.Font_GetColor(const AWinControl: TWinControl
  ): TColor;
var
  FontAttributes: TWSFontAttributes;
begin
  if WGGetFontAttributes(AWinControl, TRichBox(AWinControl).SelStart, FontAttributes) then
    Result:= FontAttributes.Color
  else
    Result:= clBlack;
end;

class function TGtk2WSCustomRichBox.Font_GetName(const AWinControl: TWinControl
  ): TFontName;
var
  FontAttributes: TWSFontAttributes;
begin
  if WGGetFontAttributes(AWinControl, TRichBox(AWinControl).SelStart, FontAttributes) then
    Result:= FontAttributes.Name
  else
    Result:= '';
end;

class function TGtk2WSCustomRichBox.Font_GetSize(const AWinControl: TWinControl
  ): integer;
var
  FontAttributes: TWSFontAttributes;
begin
  if WGGetFontAttributes(AWinControl, TRichBox(AWinControl).SelStart, FontAttributes) then
    Result:= FontAttributes.Size
  else
    Result:= 10;
end;

class function TGtk2WSCustomRichBox.Font_GetStyle(const AWinControl: TWinControl
  ): TFontStyles;
var
  FontAttributes: TWSFontAttributes;
begin
  if WGGetFontAttributes(AWinControl, TRichBox(AWinControl).SelStart, FontAttributes) then
    Result:= FontAttributes.Style
  else
    Result:= [];
end;

class function TGtk2WSCustomRichBox.Para_GetAlignment(
  const AWinControl: TWinControl): TAlignment;
var
  ParaAttributes: TWSParaAttributes;
begin
  if WGGetParaAttributes(AWinControl, TRichBox(AWinControl).SelStart, ParaAttributes) then
    Result:= ParaAttributes.Alignment
  else
    Result:= taLeftJustify;
end;

class function TGtk2WSCustomRichBox.Para_GetFirstIndent(
  const AWinControl: TWinControl): Longint;
var
  ParaAttributes: TWSParaAttributes;
begin
  if WGGetParaAttributes(AWinControl, TRichBox(AWinControl).SelStart, ParaAttributes) then
    Result:= ParaAttributes.FirstIndent
  else
    Result:= 0;
end;

class function TGtk2WSCustomRichBox.Para_GetLeftIndent(
  const AWinControl: TWinControl): Longint;
var
  ParaAttributes: TWSParaAttributes;
begin
  if WGGetParaAttributes(AWinControl, TRichBox(AWinControl).SelStart, ParaAttributes) then
    Result:= ParaAttributes.LeftIndent
  else
    Result:= 0;
end;

class function TGtk2WSCustomRichBox.Para_GetRightIndent(
  const AWinControl: TWinControl): Longint;
var
  ParaAttributes: TWSParaAttributes;
begin
  if WGGetParaAttributes(AWinControl, TRichBox(AWinControl).SelStart, ParaAttributes) then
    Result:= ParaAttributes.RightIndent
  else
    Result:= 0;
end;

class function TGtk2WSCustomRichBox.Para_GetNumbering(
  const AWinControl: TWinControl): TNumberingStyle;
begin

  Result:= WGGetNumbering(AWinControl, WGGetPos_FirstCharacter_Line(AWinControl, TRichBox(AWinControl).SelStart));

end;

class procedure TGtk2WSCustomRichBox.Font_SetColor(
  const AWinControl: TWinControl; Value: TColor);
var
  FontColor: TGDKColor;
  Buffer: PGtkTextBuffer = nil;
  Tag: Pointer = nil;
begin
  FontColor := TColortoTGDKColor(Value);
  //--
  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;
  //--
  Tag := gtk_text_buffer_create_tag(buffer, nil, 'foreground-gdk',[@FontColor,
    'foreground-set', gboolean(gTRUE), nil]);
  //--
  WGSetFormat(TRichBox(AWinControl).SelStart, TRichBox(AWinControl).SelLength, Buffer, Tag);
end;

class procedure TGtk2WSCustomRichBox.Font_SetName(
  const AWinControl: TWinControl; Value: TFontName);
var
  FontFamily: string;
  Buffer: PGtkTextBuffer = nil;
  Tag: Pointer = nil;
begin
  FontFamily := Value;
  //--
  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;
  //--
  Tag := gtk_text_buffer_create_tag(buffer, nil, 'family',
    [@FontFamily[1], 'family-set', gTRUE, nil]);
  //--
  WGSetFormat(TRichBox(AWinControl).SelStart, TRichBox(AWinControl).SelLength, Buffer, Tag);
end;

class procedure TGtk2WSCustomRichBox.Font_SetSize(
  const AWinControl: TWinControl; Value: integer);
var
  Buffer: PGtkTextBuffer = nil;
  Tag: Pointer = nil;
begin
  //--
  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;
  //--
  Tag := gtk_text_buffer_create_tag(buffer, nil, 'size-points',
  [double(Value), nil]);
  //--
  WGSetFormat(TRichBox(AWinControl).SelStart, TRichBox(AWinControl).SelLength, Buffer, Tag);
end;

class procedure TGtk2WSCustomRichBox.Font_SetStyle(
  const AWinControl: TWinControl; Value: TFontStyles);
var
  Tag: Pointer = nil;
  Buffer: PGtkTextBuffer = nil;
const
  PangoUnderline: array [boolean] of integer = (PANGO_UNDERLINE_NONE, PANGO_UNDERLINE_SINGLE);
  PangoBold: array [boolean] of integer = (PANGO_WEIGHT_NORMAL, PANGO_WEIGHT_BOLD);
  PangoItalic: array [boolean] of integer = (PANGO_STYLE_NORMAL, PANGO_STYLE_ITALIC);
begin
  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;

  Tag := gtk_text_buffer_create_tag(buffer, nil, 'underline',
   [PangoUnderline[fsUnderline in Value], 'underline-set',
    gboolean(gTRUE), 'weight', PangoBold[fsBold in Value],
    'weight-set', gboolean(gTRUE), 'style',
    PangoItalic[fsItalic in Value], 'style-set',
    gboolean(gTRUE), 'strikethrough', gboolean(fsStrikeOut in Value),
    'strikethrough-set', gboolean(gTRUE), nil]);

  WGSetFormat(TRichBox(AWinControl).SelStart, TRichBox(AWinControl).SelLength, Buffer, Tag);
end;

class procedure TGtk2WSCustomRichBox.Para_SetAlignment(
  const AWinControl: TWinControl; Value: TAlignment);
const
  GTKJustification: array [TAlignment] of integer = (GTK_JUSTIFY_LEFT, GTK_JUSTIFY_RIGHT, GTK_JUSTIFY_CENTER);
var
  Tag: Pointer = nil;
  Buffer: PGtkTextBuffer = nil;
  StartLine, EndLine:Integer;
begin
  //--
    StartLine:= WGGetPos_FirstCharacter_Line(AWinControl, TRichBox(AWinControl).SelStart);
    EndLine:= WGGetPos_LastCharacter_Line(AWinControl, TRichBox(AWinControl).SelStart);

  if (TRichBox(AWinControl).SelLength) > (EndLine - StartLine) then
    EndLine:= WGGetPos_LastCharacter_Line(AWinControl, (TRichBox(AWinControl).SelStart + TRichBox(AWinControl).SelLength));

  //--
  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;
  Tag := gtk_text_buffer_create_tag(buffer, nil,
    'justification', [GTKJustification[Value], 'justification-set',
    gboolean(gTRUE), nil]);
  WGSetFormat(StartLine, (EndLine - StartLine), Buffer, Tag);
end;

class procedure TGtk2WSCustomRichBox.Para_SetFirstIndent(
  const AWinControl: TWinControl; Value: Longint);
var
  Tag: Pointer = nil;
  Buffer: PGtkTextBuffer = nil;
  StartLine, EndLine:Integer;
begin
  //--
    StartLine:= WGGetPos_FirstCharacter_Line(AWinControl, TRichBox(AWinControl).SelStart);
    EndLine:= WGGetPos_LastCharacter_Line(AWinControl, TRichBox(AWinControl).SelStart);
  //--
  if (TRichBox(AWinControl).SelLength) > (EndLine - StartLine) then
    EndLine:= WGGetPos_LastCharacter_Line(AWinControl, (TRichBox(AWinControl).SelStart + TRichBox(AWinControl).SelLength));
  //--
  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;
  Tag := gtk_text_buffer_create_tag(buffer, nil, 'indent', [Value * 37,
    'indent-set', gboolean(gTRUE), nil]);
  WGSetFormat(StartLine, (EndLine - StartLine), Buffer, Tag);
end;

class procedure TGtk2WSCustomRichBox.Para_SetLeftIndent(
  const AWinControl: TWinControl; Value: Longint);
var
  Tag: Pointer = nil;
  Buffer: PGtkTextBuffer = nil;
  StartLine, EndLine:Integer;
begin
  //--
    StartLine:= WGGetPos_FirstCharacter_Line(AWinControl, TRichBox(AWinControl).SelStart);
    EndLine:= WGGetPos_LastCharacter_Line(AWinControl, TRichBox(AWinControl).SelStart);
  //--
  if (TRichBox(AWinControl).SelLength) > (EndLine - StartLine) then
    EndLine:= WGGetPos_LastCharacter_Line(AWinControl, (TRichBox(AWinControl).SelStart + TRichBox(AWinControl).SelLength));
  //--
  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;
  Tag := gtk_text_buffer_create_tag(buffer, nil, 'left_margin', [Value * 37,
    'left_margin-set', gboolean(gTRUE), nil]);
   WGSetFormat(StartLine, (EndLine - StartLine), Buffer, Tag);
end;

class procedure TGtk2WSCustomRichBox.Para_SetRightIndent(
  const AWinControl: TWinControl; Value: Longint);
var
  Tag: Pointer = nil;
  Buffer: PGtkTextBuffer = nil;
  StartLine, EndLine:Integer;
begin
  //--
    StartLine:= WGGetPos_FirstCharacter_Line(AWinControl, TRichBox(AWinControl).SelStart);
    EndLine:= WGGetPos_LastCharacter_Line(AWinControl, TRichBox(AWinControl).SelStart);
  //--
  if (TRichBox(AWinControl).SelLength) > (EndLine - StartLine) then
    EndLine:= WGGetPos_LastCharacter_Line(AWinControl, (TRichBox(AWinControl).SelStart + TRichBox(AWinControl).SelLength));
  //--
  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;
  Tag := gtk_text_buffer_create_tag(buffer, nil, 'right_margin', [Value *
    37, 'right_margin-set', gboolean(gTRUE), nil]);
  WGSetFormat(StartLine, (EndLine - StartLine), Buffer, Tag);
end;

class procedure TGtk2WSCustomRichBox.Para_SetNumbering(
  const AWinControl: TWinControl; Value: TNumberingStyle);
var
  I: integer;
  iterStart, iterEnd: TGtkTextIter;
  Buffer: PGtkTextBuffer = nil;
  Ch: TUTF8Char;
  N2: TNumberingStyle = nsNone;
begin
  I := WGGetPos_FirstCharacter_Line(AWinControl, TRichBox(AWinControl).SelStart);

  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;

  N2:= Para_GetNumbering(AWinControl);


  if (Value = nsNone) and (N2 = nsBullets) then
  begin
    gtk_text_buffer_get_iter_at_offset(Buffer, @iterStart, I);
    gtk_text_buffer_get_iter_at_offset(Buffer, @iterEnd, I + 1);
    gtk_text_buffer_Delete(Buffer, @iterStart, @iterEnd);
  end;

  if (Value = nsBullets) and (N2 = nsNone) then
  begin
    Ch := UnicodeToUTF8(BulletCode);
    gtk_text_buffer_get_iter_at_offset(Buffer, @iterStart, I);
    gtk_text_buffer_insert(Buffer, @iterStart, @Ch[1], Length(Ch));
    //--
  end;
end;

class procedure TGtk2WSCustomRichBox.Paste(const ACustomEdit: TCustomEdit);
var
   S:String;
   P:TPicture;
begin
    if Clipboard.HasFormat(CF_TEXT) then
      begin
        if ACustomEdit.SelLength > 0 then
          WGDelete(ACustomEdit, ACustomEdit.SelStart, ACustomEdit.SelLength);
          S := Clipboard.AsText;
          WGInsertPos(ACustomEdit, ACustomEdit.SelStart ,S);
      end;

    if Clipboard.HasFormat(CF_Bitmap) or Clipboard.HasFormat(CF_Picture) then
      begin
        if ACustomEdit.SelLength > 0 then
          WGDelete(ACustomEdit, ACustomEdit.SelStart, ACustomEdit.SelLength);

        P:=TPicture.Create;
        if Clipboard.HasFormat(CF_Bitmap) then
          P.Bitmap.Assign(Clipboard)
        else
          P.Assign(Clipboard);
        WGInsertImage(ACustomEdit, ACustomEdit.SelStart, P, 0, 0);
        P.Free;
      end;
end;

class procedure TGtk2WSCustomRichBox.SaveToStream(
  const AWinControl: TWinControl; var Stream: TStream);
var
RTFSave: TRTFSave;
begin
RTFSave:= TRTFSave.Create(AWinControl);
RTFSave.SaveToStream(Stream);
RTFSave.Free;
end;

class procedure TGtk2WSCustomRichBox.LoadFromStream(
  const AWinControl: TWinControl; const Stream: TStream);
var
  RTFRead:TRTFRead;
begin
RTFRead:=TRTFRead.Create(AWinControl);
RTFRead.LoadFromStream(Stream);
RTFRead.Free;
end;

class function TGtk2WSCustomRichBox.GetTextBuf(const AWinControl: TWinControl
  ): String;
var
  iterStart, iterEnd: TGtkTextIter;
  Buffer: PGtkTextBuffer=nil;
begin
  Result := '';
  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;
  //--
  gtk_text_buffer_get_start_iter(Buffer, @iterStart);
  gtk_text_buffer_get_end_iter(Buffer, @iterEnd);
  Result := gtk_text_buffer_get_slice(Buffer, @iterStart, @iterEnd, gboolean(False));
end;

class function TGtk2WSCustomRichBox.GetTextSel(const AWinControl: TWinControl
  ): String;
var
  SelStart, SelEnd:Integer;
  iterStart, iterEnd: TGtkTextIter;
  Buffer: PGtkTextBuffer = nil;
begin
  SelStart:= TRichBox(AWinControl).SelStart;
  SelEnd:= TRichBox(AWinControl).SelStart + TRichBox(AWinControl).SelLength;
  //--
  if not (WGGetBuffer(AWinControl, Buffer)) then
    Exit;
  //--
    gtk_text_buffer_get_iter_at_offset(Buffer, @iterStart, SelStart);
    gtk_text_buffer_get_iter_at_offset(Buffer, @iterEnd, SelEnd);
  //--
    Result := gtk_text_buffer_get_slice(Buffer, @iterStart, @iterEnd, gboolean(False));
end;


end.

