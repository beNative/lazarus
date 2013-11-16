{
 gtk2richmemo.pas

 Author: Dmitry 'skalogryz' Boyarintsev

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit Gtk2RichMemo;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  gtk2, glib2, gdk2, pango,
  // FCL
  Classes, SysUtils,
  // LCL
  LCLType, Controls, Graphics,
  // Gtk2 widget
  Gtk2Def,
  GTK2WinApiWindow, Gtk2Globals, Gtk2Proc, InterfaceBase,
  Gtk2WSControls,
  // RichMemo
  WSRichMemo;

  { TGtk2WSCustomRichMemo }
type
  TGtk2WSCustomRichMemo = class(TWSCustomRichMemo)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer; const Params: TIntFontParams); override;
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer; var Params: TIntFontParams): Boolean; override;
  end;

implementation


function gtktextattr_underline(const a : TGtkTextAppearance) : Boolean;
begin
  Result:=((a.flag0 and bm_TGtkTextAppearance_underline) shr bp_TGtkTextAppearance_underline) > 0;
end;

function gtktextattr_strikethrough(const a : TGtkTextAppearance) : Boolean;
begin
  Result:=((a.flag0 and bm_TGtkTextAppearance_strikethrough) shr bp_TGtkTextAppearance_strikethrough) > 0;
end;

function GtkTextAttrToFontParams(const textAttr: TGtkTextAttributes; var FontParams: TIntFontParams): Boolean;
var
  w   : integer;
  st  : TPangoStyle;
  pf  : PPangoFontDescription;
  pj  : LongInt; // Added by Massimo Nardello
  pi  : LongInt; // Added by Massimo Nardello
begin
  FontParams.Style := [];
  FontParams.Name := '';
  FontParams.Size := 0;
  FontParams.Color := 0;
  FontParams.BackColor := clWhite; // Added by Massimo Nardello
  FontParams.Alignment := []; // Added by Massimo Nardello
  FontParams.Indented := 0; // Added by Massimo Nardello
  FontParams.Changed := []; // Added by Massimo Nardello

  // Added by Massimo Nardello...
  pj := textAttr.justification;
  if pj = 0 then
    FontParams.Alignment := [trLeft]
  else if pj = 1 then
    FontParams.Alignment := [trRight]
  else if pj = 2 then
    FontParams.Alignment := [trCenter]
  else if pj = 3 then
    FontParams.Alignment := [trJustified];

  pi := textAttr.left_margin;
  FontParams.Indented := pi;
  // ... up to here

  pf := textAttr.font;
  Result := Assigned(pf);
  if not Result then Exit;

  if Assigned(pf) then begin
    FontParams.Name := pango_font_description_get_family(pf);
    FontParams.Size := pango_font_description_get_size(pf);
    if not pango_font_description_get_size_is_absolute(pf) then
      FontParams.Size := Round(FontParams.Size / PANGO_SCALE);

    w := pango_font_description_get_weight(pf);
    if w > PANGO_WEIGHT_NORMAL then Include(FontParams.Style, fsBold);

    st := pango_font_description_get_style(pf);
    if st and PANGO_STYLE_ITALIC > 0 then  Include(FontParams.Style, fsItalic);
  end;

  FontParams.Color := TGDKColorToTColor(textAttr.appearance.fg_color);
  FontParams.BackColor := TGDKColorToTColor(textAttr.appearance.bg_color); // Added by Massimo Nardello

  if gtktextattr_underline(textAttr.appearance) then  Include(FontParams.Style, fsUnderline);
  if gtktextattr_strikethrough(textAttr.appearance) then Include(FontParams.Style, fsStrikeOut);
end;


class procedure TGtk2WSCustomRichMemo.SetCallbacks(
  const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
   TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtk2WSCustomRichMemo.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget,
  TempWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Widget := gtk_scrolled_window_new(nil, nil);
  Result := TLCLIntfHandle(PtrUInt(Widget));
  if Result = 0 then Exit;
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget,dbgsName(AWinControl));
  {$ENDIF}

  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);

  TempWidget := gtk_text_view_new();
  gtk_container_add(PGtkContainer(Widget), TempWidget);

  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(Widget)^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(Widget)^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(Widget),
                                     GTK_POLICY_AUTOMATIC,
                                     GTK_POLICY_AUTOMATIC);
  // add border for memo
  gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(Widget),
    BorderStyleShadowMap[TCustomControl(AWinControl).BorderStyle]);

  SetMainWidget(Widget, TempWidget);
  GetWidgetInfo(Widget, True)^.CoreWidget := TempWidget;

  // gtk_text_buffer_set_text(gtk_text_view_get_buffer(PGtkTextView(TempWidget)), PChar(TCustomMemo(AWinControl).Text), -1);
  gtk_text_view_set_editable(PGtkTextView(TempWidget), True);
{  //gtk_text_view_set_editable(PGtkTextView(TempWidget), not TCustomMemo(AWinControl).ReadOnly);
  //gtk_text_view_set_justification(PGtkTextView(TempWidget), aGtkJustification[TCustomMemo(AWinControl).Alignment]);
  if TCustomMemo(AWinControl).WordWrap then
    gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_WORD)
  else
    gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_NONE);}
  gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_WORD);

  //gtk_text_view_set_accepts_tab(PGtkTextView(TempWidget), TCustomMemo(AWinControl).WantTabs);
  gtk_text_view_set_accepts_tab(PGtkTextView(TempWidget), True);

  gtk_widget_show_all(Widget);

  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

class procedure TGtk2WSCustomRichMemo.SetTextAttributes(const AWinControl: TWinControl;
  TextStart, TextLen: Integer; const Params: TIntFontParams);
var
  Widget, TextWidget: PGtkWidget;
  list    : PGList;
  buffer  : PGtkTextBuffer;
  tag     : Pointer;
  istart  : TGtkTextIter;
  iend    : TGtkTextIter;
  gcolor  : TGdkColor;
  bcolor  : TGdkColor;
  nm      : string;
const
  PangoUnderline : array [Boolean] of Integer = (PANGO_UNDERLINE_NONE, PANGO_UNDERLINE_SINGLE);
  PangoBold      : array [Boolean] of Integer = (PANGO_WEIGHT_NORMAL, PANGO_WEIGHT_BOLD);
  PangoItalic    : array [Boolean] of Integer = (PANGO_STYLE_NORMAL, PANGO_STYLE_ITALIC);
begin
  Widget := PGtkWidget(PtrUInt(AWinControl.Handle));

  list := gtk_container_get_children(PGtkContainer(Widget));
  if not Assigned(list) then Exit;

  TextWidget := PGtkWidget(list^.data);
  if not Assigned(TextWidget) then Exit;

  buffer := gtk_text_view_get_buffer (PGtkTextView(TextWidget));
  if not Assigned(buffer) then Exit;

  gcolor := TColortoTGDKColor(Params.Color);
  bcolor := TColortoTGDKColor(Params.BackColor);

  nm := Params.Name;
  if nm = '' then nm := #0;

  // Code modified by Massimo Nardello
  // Font name is modified
  gtk_text_buffer_get_iter_at_offset (buffer, @istart, TextStart);
  gtk_text_buffer_get_iter_at_offset (buffer, @iend, TextStart+TextLen);
  if fiName in Params.Changed then begin
    tag := gtk_text_buffer_create_tag (buffer, nil,
      'family-set',     [gboolean(gTRUE),
      'family',         @nm[1], nil]);
    gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
  end;
  // Font size is modified
  if fiSize in Params.Changed then begin
    tag := gtk_text_buffer_create_tag (buffer, nil,
      'size-set',       [gboolean(gTRUE),
      'size-points',    Double(Params.Size),  nil]);
    gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
  end;
  // Font color is modified
  if fiColor in Params.Changed then begin
    tag := gtk_text_buffer_create_tag (buffer, nil,
      'foreground-gdk', [@gcolor, nil]);
    gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
  end;
  // Font back color is modified
  if fiBackcolor in Params.Changed then begin
    tag := gtk_text_buffer_create_tag (buffer, nil,
      'background-gdk', [@bcolor, nil]);
    gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
  end;
  // Font style underline is activated or deactivated
  if fiUnderline in Params.Changed then begin
    tag := gtk_text_buffer_create_tag (buffer, nil,
      'underline-set',  [gboolean(gTRUE),
      'underline',      PangoUnderline[fsUnderline in Params.Style], nil]);
      gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
  end;
  // Font style bold is activated or deactivated
  if fiBold in Params.Changed then begin
    tag := gtk_text_buffer_create_tag (buffer, nil,
      'weight-set',     [gboolean(gTRUE),
      'weight',         PangoBold[fsBold in Params.Style],  nil]);
    gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
  end;
  // Font style italic is activated or deactivated
  if fiItalic in Params.Changed then begin
    tag := gtk_text_buffer_create_tag (buffer, nil,
      'style-set',      [gboolean(gTRUE),
      'style',          PangoItalic[fsItalic in Params.Style],  nil]);
    gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
  end;
  // Font style strike is activated or deactivated
  if fiStrike in Params.Changed then begin
    tag := gtk_text_buffer_create_tag (buffer, nil,
      'strikethrough-set', [gboolean(gTRUE),
      'strikethrough',    gboolean(fsStrikeOut in Params.Style),  nil]);
    gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
  end;
  // Alignment
  if fiAlignment in Params.Changed then begin
    // Remove possibile previous paragraph formatting
    gtk_text_buffer_remove_tag_by_name(buffer, 'center', @istart, @iend);
    gtk_text_buffer_remove_tag_by_name(buffer, 'left', @istart, @iend);
    gtk_text_buffer_remove_tag_by_name(buffer, 'right', @istart, @iend);
    gtk_text_buffer_remove_tag_by_name(buffer, 'justified', @istart, @iend);
    if Params.Alignment = [trCenter] then begin
      gtk_text_buffer_create_tag (buffer, 'center', 'justification',
        [GTK_JUSTIFY_CENTER, nil]);
      gtk_text_buffer_apply_tag_by_name(buffer, 'center', @istart, @iend);
    end
    else if Params.Alignment = [trLeft] then begin
      gtk_text_buffer_create_tag (buffer, 'left', 'justification',
        [GTK_JUSTIFY_LEFT, nil]);
      gtk_text_buffer_apply_tag_by_name(buffer, 'left', @istart, @iend);
    end
    else if Params.Alignment = [trRight] then begin
        gtk_text_buffer_create_tag (buffer, 'right', 'justification',
          [GTK_JUSTIFY_RIGHT, nil]);
        gtk_text_buffer_apply_tag_by_name(buffer, 'right', @istart, @iend);
    end
    else if Params.Alignment = [trJustified] then begin
        gtk_text_buffer_create_tag (buffer, 'justified', 'justification',
          [GTK_JUSTIFY_FILL, nil]);
        gtk_text_buffer_apply_tag_by_name(buffer, 'justified', @istart, @iend);
    end;
  end;
  if fiIndented in Params.Changed then begin
    //  Indent the text
    //  Alignment perfect with a Bullet and a tab in Sans 12
    if Params.Indented > 0 then begin
      gtk_text_buffer_create_tag (buffer, 'wide_margins',
        'left_margin', [50, 'indent', -Params.Indented, nil]);
      gtk_text_buffer_apply_tag_by_name(buffer, 'wide_margins', @istart, @iend);
    end
    else begin
      gtk_text_buffer_remove_tag_by_name(buffer, 'wide_margins', @istart, @iend);
    end;
  end;
end;


class function TGtk2WSCustomRichMemo.GetTextAttributes(const AWinControl: TWinControl;
  TextStart: Integer; var Params: TIntFontParams): Boolean;
var
  Widget     : PGtkWidget;
  TextWidget : PGtkWidget;
  list       : PGList;
  buffer     : PGtkTextBuffer;
  iter       : TGtkTextIter;
  attr       : PGtkTextAttributes;
begin
  Widget := PGtkWidget(PtrUInt(AWinControl.Handle));

  list := gtk_container_get_children(PGtkContainer(Widget));
  if not Assigned(list) then Exit;

  TextWidget := PGtkWidget(list^.data);
  if not Assigned(TextWidget) then Exit;

  buffer := gtk_text_view_get_buffer (PGtkTextView(TextWidget));
  if not Assigned(buffer) then Exit;

  attr := gtk_text_view_get_default_attributes(PGtkTextView(TextWidget));
  Result := Assigned(attr);
  if not Assigned(attr) then Exit;

  gtk_text_buffer_get_iter_at_offset(buffer, @iter, TextStart);
  Result := gtk_text_iter_get_attributes(@iter, attr);
  //if Result then
    {Result := }
  Result := true;
    GtkTextAttrToFontParams(attr^, Params);
  gtk_text_attributes_unref(attr);
end;

end.

