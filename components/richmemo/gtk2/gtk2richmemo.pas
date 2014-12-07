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
  // RTL/FCL
  Types, Classes, SysUtils,
  // LCL
  LCLType, Controls, Graphics, LazUTF8, StdCtrls,
  // Gtk2 widget
  Gtk2Def,
  GTK2WinApiWindow, Gtk2Globals, Gtk2Proc, InterfaceBase,
  gdk2pixbuf, Gtk2WSStdCtrls,
  // RichMemo
  RichMemo, WSRichMemo, RichMemoUtils;

  { TGtk2WSCustomRichMemo }
type
  TGtk2WSCustomRichMemo = class(TWSCustomRichMemo)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
    class procedure GetWidgetBuffer(const AWinControl: TWinControl; var TextWidget: PGtkWidget; var Buffer: PGtkTextBuffer);
    class function GetAttrAtPos(const AWinControl: TWinControl; TextStart: Integer; APara: Boolean = false): PGtkTextAttributes;
    class procedure ApplyTag(abuffer: PGtkTextBuffer; tag: PGtkTextTag; TextStart, TextLen: Integer; ToParagraphs: Boolean = False);

  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class function GetStyleRange(const AWinControl: TWinControl; TextStart: Integer;
      var RangeStart, RangeLen: Integer): Boolean; override;
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TIntFontParams): Boolean; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const Params: TIntFontParams); override;
    class function GetParaAlignment(const AWinControl: TWinControl; TextStart: Integer;
      var AAlign: TIntParaAlignment): Boolean; override;
    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: TIntParaAlignment); override;

    class function GetParaMetric(const AWinControl: TWinControl; TextStart: Integer;
      var AMetric: TIntParaMetric): Boolean; override;
    class procedure SetParaMetric(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AMetric: TIntParaMetric); override;

    class function GetParaRange(const AWinControl: TWinControl; TextStart: Integer; var rng: TParaRange): Boolean; override;
    class procedure InDelText(const AWinControl: TWinControl; const TextUTF8: String; DstStart, DstLen: Integer); override;

    class function Search(const AWinControl: TWinControl; const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer; override;

    class function ImageFromFile(const ARichMemo: TCustomRichMemo; APos: Integer;
         const FileNameUTF8: string;
         const AImgSize: TSize
      ): Boolean;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;

    class procedure SetZoomFactor(const AWinControl: TWinControl; AZoomFactor: Double); override;
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
  sz  : double;
const
  ScreenDPI = 96; // todo: might change, should be received dynamically
  PageDPI   = 72; // not expected to be changed
begin
  FontParams.Style := [];
  FontParams.Name := '';
  FontParams.Size := 0;
  FontParams.Color := 0;

  pf := textAttr.font;
  Result := Assigned(pf);
  if not Result then Exit;

  if Assigned(pf) then begin
    FontParams.Name := pango_font_description_get_family(pf);
    FontParams.Size := pango_font_description_get_size(pf);
    sz:=FontParams.Size / PANGO_SCALE;
    if pango_font_description_get_size_is_absolute(pf) then
      sz:=sz/(ScreenDPI/PageDPI);
    FontParams.Size:=round(sz);
    w := pango_font_description_get_weight(pf);
    if w > PANGO_WEIGHT_NORMAL then Include(FontParams.Style, fsBold);

    st := pango_font_description_get_style(pf);
    if st and PANGO_STYLE_ITALIC > 0 then  Include(FontParams.Style, fsItalic);
  end;

  FontParams.Color := TGDKColorToTColor(textAttr.appearance.fg_color);
  if gtktextattr_underline(textAttr.appearance) then  Include(FontParams.Style, fsUnderline);
  if gtktextattr_strikethrough(textAttr.appearance) then Include(FontParams.Style, fsStrikeOut);
end;

type
  TGtk2WSCustomMemoInt = class(TGtk2WSCustomMemo);
  TCustomRichMemoInt   = class(TCustomRichMemo);

procedure Gtk2WS_MemoSelChanged (Textbuffer: PGtkTextBuffer;
   StartIter: PGtkTextIter; mark: PGtkTextMark; WidgetInfo: PWidgetInfo); cdecl;
begin
  if TControl(WidgetInfo^.LCLObject) is TCustomRichMemo then
  begin
    TCustomRichMemoInt(WidgetInfo^.LCLObject).DoSelectionChange;
  end;
end;

procedure Gtk2WS_RichMemoInsert(Textbuffer: PGtkTextBuffer;
   StartIter: PGtkTextIter; text: PChar; len: gint; WidgetInfo: PWidgetInfo); cdecl;
var
  rm : TCustomRichMemo;
  iter : PGtkTextIter;
  tag  : PGtkTextTag;
  w    : PGtkWidget;
  b    : PGtkTextBuffer;
  attr : PGtkTextAttributes;
begin
  if TControl(WidgetInfo^.LCLObject) is TCustomRichMemo then
  begin
    rm := TCustomRichMemo(WidgetInfo^.LCLObject);
    // re-zooming any newly entered (pasted, manually inserted text)
    if (rm.ZoomFactor<>1) then begin
      TGtk2WSCustomRichMemo.GetWidgetBuffer(rm, w, b);
      iter:=gtk_text_iter_copy(StartIter);
      gtk_text_iter_backward_chars(iter, len);
      attr := gtk_text_view_get_default_attributes(PGtkTextView(w));
      gtk_text_iter_get_attributes(iter, attr);

      if attr^.font_scale<>rm.ZoomFactor then begin
        tag := gtk_text_buffer_create_tag(b, nil,
            'scale', [   gdouble(rm.ZoomFactor),
            'scale-set', gboolean(gTRUE),
            nil]);
        gtk_text_buffer_apply_tag(b, tag, iter, StartIter);
      end;
      gtk_text_attributes_unref(attr);
    end;
  end;
end;

class procedure TGtk2WSCustomRichMemo.SetCallbacks(
  const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
var
  TextBuf: PGtkTextBuffer;
begin
  TGtk2WSCustomMemoInt.SetCallbacks(AGtkWidget, AWidgetInfo);

  TextBuf := gtk_text_view_get_buffer(PGtkTextView(AWidgetInfo^.CoreWidget));
  SignalConnectAfter(PGtkWidget(TextBuf), 'mark-set', @Gtk2WS_MemoSelChanged, AWidgetInfo);
  SignalConnectAfter(PGtkWidget(TextBuf), 'insert-text', @Gtk2WS_RichMemoInsert, AWidgetInfo);
end;

class procedure TGtk2WSCustomRichMemo.GetWidgetBuffer(const AWinControl: TWinControl;
    var TextWidget: PGtkWidget; var Buffer: PGtkTextBuffer);
var
  Widget     : PGtkWidget;
  list       : PGList;
begin
  TextWidget:=nil;
  Buffer:=nil;
  // todo: cache values?
  Widget := PGtkWidget(PtrUInt(AWinControl.Handle));

  list := gtk_container_get_children(PGtkContainer(Widget));
  if not Assigned(list) then Exit;

  TextWidget := PGtkWidget(list^.data);
  if not Assigned(TextWidget) then Exit;

  buffer := gtk_text_view_get_buffer (PGtkTextView(TextWidget));
end;

class function TGtk2WSCustomRichMemo.GetAttrAtPos(
  const AWinControl: TWinControl; TextStart: Integer; APara: Boolean ): PGtkTextAttributes;
var
  TextWidget : PGtkWidget;
  buffer     : PGtkTextBuffer;
  iter       : TGtkTextIter;
  attr       : PGtkTextAttributes;
begin
  Result:=nil;
  GetWidgetBuffer(AWinControl, TextWidget, buffer);

  attr := gtk_text_view_get_default_attributes(PGtkTextView(TextWidget));
  if not Assigned(attr) then Exit;

  gtk_text_buffer_get_iter_at_offset(buffer, @iter, TextStart);
  if APara then gtk_text_iter_set_line_offset(@iter, 0);
  gtk_text_iter_get_attributes(@iter, attr);
  Result:=attr;
end;

class procedure TGtk2WSCustomRichMemo.ApplyTag(abuffer: PGtkTextBuffer;
  tag: PGtkTextTag; TextStart, TextLen: Integer; ToParagraphs: Boolean = False);
var
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
begin
  gtk_text_buffer_get_iter_at_offset (abuffer, @istart, TextStart);
  gtk_text_buffer_get_iter_at_offset (abuffer, @iend, TextStart+TextLen);
  if ToParagraphs then begin
    gtk_text_iter_set_line_offset(@istart, 0);
    gtk_text_iter_forward_to_line_end(@iend);
  end;
  gtk_text_buffer_apply_tag(abuffer, tag, @istart, @iend);
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

  gtk_text_view_set_editable(PGtkTextView(TempWidget), True);
  gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_WORD);

  gtk_text_view_set_accepts_tab(PGtkTextView(TempWidget), True);

  gtk_widget_show_all(Widget);

  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

class function TGtk2WSCustomRichMemo.GetStyleRange(
  const AWinControl: TWinControl; TextStart: Integer; var RangeStart,
  RangeLen: Integer): Boolean;
var
  w : PGtkWidget;
  b : PGtkTextBuffer;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then begin
    Result:=false;
    Exit;
  end;
  gtk_text_buffer_get_iter_at_offset (b, @istart, TextStart+1);
  if gtk_text_iter_get_offset(@istart)<>TextStart+1 then begin
    Result:=false; // TextStart is beyoned the end of text
    Exit;
  end;

  gtk_text_iter_backward_to_tag_toggle(@istart, nil);
  RangeStart:=gtk_text_iter_get_offset(@istart);

  gtk_text_buffer_get_iter_at_offset (b, @iend, TextStart);
  gtk_text_iter_forward_to_tag_toggle(@iend, nil);

  RangeLen:=gtk_text_iter_get_offset(@iend)-RangeStart;
  Result:=true;
end;

class procedure TGtk2WSCustomRichMemo.SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer; const Params: TIntFontParams);
var
  TextWidget: PGtkWidget;
  buffer  : PGtkTextBuffer;
  tag     : Pointer;
  gcolor  : TGdkColor;
  bgcolor : TGdkColor;
  nm      : string;
const
  pu: array [Boolean] of gint = (PANGO_UNDERLINE_NONE, PANGO_UNDERLINE_SINGLE);
  pb: array [Boolean] of gint = (PANGO_WEIGHT_NORMAL, PANGO_WEIGHT_BOLD);
  pi: array [Boolean] of gint = (PANGO_STYLE_NORMAL, PANGO_STYLE_ITALIC);
begin
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  if not Assigned(buffer) then Exit;

  gcolor := TColortoTGDKColor(Params.Color);
  bgcolor := TColortoTGDKColor(Params.BkColor);
  nm := Params.Name;
  if nm = '' then nm := #0;
  tag := gtk_text_buffer_create_tag (buffer, nil,
      'family-set',     [gboolean(gTRUE),
      'family',         @nm[1],
      'foreground-gdk', @gcolor,
      'foreground-set', gboolean(gTRUE),
      'background-gdk', @bgcolor,
      'background-set', gboolean(Params.HasBkClr),
      'size-set',       gboolean(gTRUE),
      'size-points',    gdouble(Params.Size),
      'underline-set',  gboolean(gTRUE),
      'underline',      gint(pu[fsUnderline in Params.Style]),
      'weight-set',     gboolean(gTRUE),
      'weight',         gint(pb[fsBold in Params.Style]),
      'style-set',      gboolean(gTRUE),
      'style',          gint(pi[fsItalic in Params.Style]),
      'strikethrough-set', gboolean(gTRUE),
      'strikethrough',    gboolean(fsStrikeOut in Params.Style),
      nil]);
  ApplyTag(buffer, tag, TextStart, TextLen);

end;

class function TGtk2WSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer; var AAlign: TIntParaAlignment
  ): Boolean;
var
  attr       : PGtkTextAttributes;
begin
  attr:=GetAttrAtPos(AWinControl, TextStart, true);
  Result := Assigned(attr);
  if Result then begin
    case attr^.justification of
      GTK_JUSTIFY_LEFT:   AAlign:=paLeft;
      GTK_JUSTIFY_RIGHT:  AAlign:=paRIGHT;
      GTK_JUSTIFY_CENTER: AAlign:=paCenter;
      GTK_JUSTIFY_FILL:   AAlign:=paJustify;
    else
      AAlign:=paLeft;
    end;
    gtk_text_attributes_unref(attr);
  end;
end;

class procedure TGtk2WSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AAlign: TIntParaAlignment);
var
  w      : PGtkWidget;
  buffer : PGtkTextBuffer;
  tag    : PGtkTextTag;
  val    : Integer;
begin
  case AAlign of
    paRight:   val:=GTK_JUSTIFY_RIGHT;
    paCenter:  val:=GTK_JUSTIFY_CENTER;
    paJustify: val:=GTK_JUSTIFY_FILL;
  else
    val := GTK_JUSTIFY_LEFT;
  end;
  GetWidgetBuffer(AWinControl, w, buffer);
  tag := gtk_text_buffer_create_tag (buffer, nil,
      'justification', [   gint(val),
      'justification-set', gboolean(gTRUE),
      nil]);
  ApplyTag(buffer, tag, TextStart, TextLen, true);
end;

class function TGtk2WSCustomRichMemo.GetParaMetric(
  const AWinControl: TWinControl; TextStart: Integer;
  var AMetric: TIntParaMetric): Boolean;
var
  attr : PGtkTextAttributes;
  fp   : TFontParams;
const
  ScreenDPI = 96; // todo: might change, should be received dynamically
  PageDPI   = 72; // not expected to be changed
  PixToPt   = PageDPI / ScreenDPI;
begin
  attr:=GetAttrAtPos(AWinControl, TextStart, true);
  GtkTextAttrToFontParams(attr^, fp);
  Result := Assigned(attr);
  if Result then begin
    if attr^.indent<0 then begin
      AMetric.FirstLine:=(attr^.left_margin)*PixToPt;
      AMetric.HeadIndent:=(-attr^.indent+attr^.left_margin)*PixToPt;
    end else begin
      AMetric.FirstLine:=(attr^.left_margin+attr^.indent)*PixToPt;
      AMetric.HeadIndent:=attr^.left_margin*PixToPt;
    end;
    AMetric.TailIndent:=attr^.right_margin*PixToPt;

    AMetric.SpaceAfter:=attr^.pixels_above_lines*PixToPt;
    AMetric.SpaceBefore:=attr^.pixels_below_lines*PixToPt;
    AMetric.LineSpacing:=(attr^.pixels_inside_wrap*PixToPt+fp.Size)/(fp.Size);
    gtk_text_attributes_unref(attr);
  end;
end;

class procedure TGtk2WSCustomRichMemo.SetParaMetric(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AMetric: TIntParaMetric);
var
  w      : PGtkWidget;
  buffer : PGtkTextBuffer;
  tag    : PGtkTextTag;
  h      : double;
  fl     : double;
  t      : double;
  attr   : PGtkTextAttributes;
  fp     : TFontParams;
const
  ScreenDPI = 96; // todo: might change, should be received dynamically
  PageDPI   = 72; // not expected to be changed
  DPIFactor = ScreenDPI / PageDPI;
begin
  h:=AMetric.HeadIndent;
  if h<0 then h:=0;
  fl:=AMetric.FirstLine;
  if fl<0 then fl:=0;

  if fl<h then begin
    t:=h;
    h:=fl;
    fl:=fl-t;
  end else
    fl:=fl-h;

  attr:=GetAttrAtPos(AWinControl, TextStart);
  GtkTextAttrToFontParams(attr^, fp);
  gtk_text_attributes_unref(attr);

  GetWidgetBuffer(AWinControl, w, buffer);
  tag := gtk_text_buffer_create_tag (buffer, nil,
      'pixels-above-lines',   [ gint(round(AMetric.SpaceBefore*DPIFactor)),
      'pixels-above-lines-set', gboolean(gTRUE),
      'pixels-below-lines',     gint(round(AMetric.SpaceAfter*DPIFactor)),
      'pixels-below-lines-set', gboolean(gTRUE),
      'left-margin',            gint(round(h*DPIFactor)),
      'left-margin-set',        gboolean(gTRUE),
      'right-margin',           gint(round(AMetric.TailIndent*DPIFactor)),
      'right-margin-set',       gboolean(gTRUE),
      'indent',                 gint(round(fl*DPIFactor)),
      'indent-set',             gboolean(gTRUE),
      'pixels-inside-wrap',     gint((round(fp.Size*(AMetric.LineSpacing-DefLineSpacing)*DPIFactor))),
      'pixels-inside_wrap-set', gboolean(gTRUE),
      nil]);
  ApplyTag(buffer, tag, TextStart, TextLen, true);
end;

class function TGtk2WSCustomRichMemo.GetParaRange(
  const AWinControl: TWinControl; TextStart: Integer; var rng: TParaRange
  ): Boolean;
var
  w : PGtkWidget;
  b : PGtkTextBuffer;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then begin
    Result:=false;
    Exit;
  end;
  gtk_text_buffer_get_iter_at_offset (b, @istart, TextStart);
  gtk_text_buffer_get_iter_at_offset (b, @iend, TextStart);
  gtk_text_iter_set_line_offset(@istart, 0);
  gtk_text_iter_forward_to_line_end(@iend);
  rng.start:=gtk_text_iter_get_offset(@istart);
  rng.lenghtNoBr:=gtk_text_iter_get_offset(@iend)-rng.start;

  // if there's a character to move, then it's end of line, if not then it won't change!
  gtk_text_iter_forward_char(@iend);
  rng.length:=gtk_text_iter_get_offset(@iend)-rng.start;
  Result:=true;
end;

class procedure TGtk2WSCustomRichMemo.InDelText(const AWinControl: TWinControl;
  const TextUTF8: String; DstStart, DstLen: Integer);
var
  w : PGtkWidget;
  b : PGtkTextBuffer;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then Exit;
  gtk_text_buffer_get_iter_at_offset (b, @istart, DstStart);
  gtk_text_buffer_get_iter_at_offset (b, @iend, DstStart+DstLen);
  gtk_text_buffer_delete(b, @istart, @iend);
  if length(TextUTF8)>0 then
    gtk_text_buffer_insert(b, @istart, @textUTF8[1], length(TextUTF8));
end;

procedure UTF8CharsToWideString(const p: Pchar; var w: WideString);
var
  slen : Integer;
  cnt: Integer;
  sz: SizeUInt;
begin
  if not Assigned(p) then begin
    w:='';
    Exit;
  end;
  slen:=strlen(p);
  if slen=0 then begin
    w:='';
    Exit;
  end;
  cnt:=UTF8Length(p, slen);
  SetLength(w, cnt);
  if cnt>0 then
   ConvertUTF8ToUTF16( @w[1], length(w), p, slen, [toInvalidCharToSymbol], sz);
  SetLength(w, sz);
end;

class function TGtk2WSCustomRichMemo.Search(const AWinControl: TWinControl;
  const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer;
var
  TextWidget   : PGtkWidget;
  buffer       : PGtkTextBuffer;
  istart       : TGtkTextIter;
  iend         : TGtkTextIter;
  start_match  : TGtkTextIter;
  end_match    : TGtkTextIter;
  Found        : Boolean;
  opt          : TGtkTextSearchFlags;
  gstr         : PChar;
  txt          : WideString;
  sub          : WIdeString;
const
  GTK_TEXT_SEARCH_VISIBLE_ONLY     = 1 shl 0;  (* values of TGtkTextSearchFlags *)
  {%H-}GTK_TEXT_SEARCH_TEXT_ONLY        = 1 shl 1;
  GTK_TEXT_SEARCH_CASE_INSENSITIVE = 1 shl 2;
begin
  Result := -1;
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  if not Assigned(buffer) then Exit;

  opt:=GTK_TEXT_SEARCH_VISIBLE_ONLY;
  if not (soMatchCase in SearchOpts.Options) then begin
    opt:=opt or GTK_TEXT_SEARCH_CASE_INSENSITIVE; // doesn't work anyway! it works in gtk3 only

    gtk_text_buffer_get_iter_at_offset (buffer, @istart, SearchOpts.start);
    gtk_text_buffer_get_iter_at_offset (buffer, @iend, SearchOpts.start+SearchOpts.len);

    gtk_text_buffer_get_text(buffer, @istart, @iend, false);
    gstr := gtk_text_buffer_get_text(Buffer, @istart, @iend, False);
    if Assigned(gstr) then begin
      UTF8CharsToWideString(gstr, txt);
      g_free(gstr);
      txt:=WideUpperCase(txt);
      sub:=WideUpperCase(UTF8Decode(ANiddle));
      Result:=Pos(sub,txt);
      if Result>0 then
        Result:=Result-1+SearchOpts.start
      else
        Result:=-1;
    end else
      Result:=-1;
  end else begin
    gtk_text_buffer_get_iter_at_offset(buffer, @istart, SearchOpts.start );
    if not (soBackward in SearchOpts.Options) then
    begin
      gtk_text_buffer_get_iter_at_offset(buffer, @iend, SearchOpts.start+SearchOpts.len );
      Found := gtk_text_iter_forward_search(@istart, PgChar(ANiddle), opt,
          @start_match, @end_match, @iend)
    end else begin
      gtk_text_buffer_get_iter_at_offset(buffer, @iend, SearchOpts.start-SearchOpts.len);
      Found := gtk_text_iter_backward_search(@istart, PgChar(ANiddle), opt,
          @start_match, @end_match, @iend)
    end;

    if Found
      then Result := gtk_text_iter_get_offset(@start_match)
      else Result := -1;
  end;
end;

class function TGtk2WSCustomRichMemo.ImageFromFile(
  const ARichMemo: TCustomRichMemo; APos: Integer; const FileNameUTF8: string;
  const AImgSize: TSize): Boolean;
var
  t: PGtkWidget;
  b: PGtkTextBuffer;
  istart: TGtkTextIter;
  pix: PGdkPixbuf;
  err: PGError;
const
  ScreenDPI = 96; // todo: might change, should be received dynamically
  PageDPI   = 72; // not expected to be changed
  DPIFactor = ScreenDPI / PageDPI;
begin
  Result:=false;
  GetWidgetBuffer(ARichMemo, t, b);
  if not Assigned(b) then Exit;

  err:=nil;

  if (AImgSize.cx=0) and (AImgSize.cy=0) then
    pix := gdk_pixbuf_new_from_file(PChar(FileNameUTF8), @err)
  else
    pix := gdk_pixbuf_new_from_file_at_size(PChar(FileNameUTF8),
      round(AImgSize.cx * DPIFactor),  round(AImgSize.cy * DPIFactor), @err);

  Result:=Assigned(pix);
  if Result then begin
    gtk_text_buffer_get_iter_at_offset(b, @istart, APos);
    gtk_text_buffer_insert_pixbuf(b, @istart, pix);
  end else
    writeln(err^.message);
end;

class procedure TGtk2WSCustomRichMemo.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
var
  TextMark: PGtkTextMark;
  CursorIter: TGtkTextIter;
  w      : PGtkWidget;
  b      : PGtkTextBuffer;
begin
  GetWidgetBuffer(ACustomEdit, w, b);
  if not Assigned(b) then Exit;

  if NewStart = -1 then
  begin
    // always scroll so the cursor is visible
    TextMark := gtk_text_buffer_get_insert(b);
    gtk_text_buffer_get_iter_at_mark(b, @CursorIter, TextMark);
  end
  else begin
    // SelStart was used and we should move to that location
    gtk_text_buffer_get_iter_at_offset(b, @CursorIter, NewStart);
    gtk_text_buffer_place_cursor(b, @CursorIter); // needed to move the cursor
    TextMark := gtk_text_buffer_get_insert(b);
  end;
  gtk_text_view_scroll_to_mark(PGtkTextView(w), TextMark, 0, True, 0, 1);
end;

class procedure TGtk2WSCustomRichMemo.SetSelLength(
  const ACustomEdit: TCustomEdit; NewLength: integer);
var
  TextMark: PGtkTextMark;
  StartIter,
  EndIter: TGtkTextIter;
  Offset: Integer;
  w      : PGtkWidget;
  b      : PGtkTextBuffer;
begin
  GetWidgetBuffer(ACustomEdit, w, b);
  if not Assigned(b) then Exit;

  TextMark := gtk_text_buffer_get_insert(b);
  gtk_text_buffer_get_iter_at_mark(b, @StartIter, TextMark);

  Offset := gtk_text_iter_get_offset(@StartIter);

  gtk_text_buffer_get_iter_at_offset(b, @EndIter, Offset+NewLength);

  gtk_text_buffer_select_range(b, @StartIter, @EndIter);
end;

class procedure TGtk2WSCustomRichMemo.SetZoomFactor(
  const AWinControl: TWinControl; AZoomFactor: Double);
var
  w      : PGtkWidget;
  b      : PGtkTextBuffer;
  tag    : PGtkTextTag;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
  p      : PGtkTextAttributes;
  sc     : gdouble;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then Exit;

  p:=GetAttrAtPos(AWinControl, 0);
  sc:=p^.font_scale;
  if sc=0 then sc:=1;
  gtk_text_attributes_unref(p);
  // restore the scale.
  // for whatever reason, scale is always assumed as a multiplier!
  // thus it is necessary to "unscale" the previous value as well
  sc:=1/sc*AZoomFactor;

  tag := gtk_text_buffer_create_tag(b, nil,
      'scale', [   gdouble(sc),
      'scale-set', gboolean(gTRUE),
      nil]);

  //gtk_text_buffer_get_start_iter(b, @istart);
  gtk_text_buffer_get_iter_at_offset(b, @istart, 0);
  gtk_text_buffer_get_end_iter(b, @iend);
  gtk_text_buffer_apply_tag(b, tag, @istart, @iend);

  //todo: set default font with scale
end;


class function TGtk2WSCustomRichMemo.GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer; var Params: TIntFontParams): Boolean;
var
  attr       : PGtkTextAttributes;
begin
  attr:=GetAttrAtPos(AWinControl, TextStart);
  Result := Assigned(attr);
  if Result then begin
    GtkTextAttrToFontParams(attr^, Params);
    gtk_text_attributes_unref(attr);
  end;
end;

function GtkInsertImageFromFile(const ARichMemo: TCustomRichMemo; APos: Integer;
     const FileNameUTF8: string;
     const AImgSize: TSize
  ): Boolean;
begin
  Result:=TGtk2WSCustomRichMemo.ImageFromFile(ARichMemo, APos, FileNameUTF8, AImgSize);
end;

initialization
  InsertImageFromFile := @GtkInsertImageFromFile;

end.

