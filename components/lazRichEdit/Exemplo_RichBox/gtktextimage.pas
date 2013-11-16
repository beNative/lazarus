unit GTKTextImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IntfGraphics, Controls, Graphics,
  GraphType, gtk2, glib2, gdk2, pango, Gtk2Proc, Gtk2Def, gdk2pixbuf, Gtk2Globals;


procedure InsertImage(TextView: TWinControl; Image:TPicture; Pos:Integer);
//procedure GetImage(TextView: TWinControl; BMP: TBitmap; Pos:Integer);
implementation

procedure InsertImage(TextView: TWinControl; Image:TPicture; Pos:Integer);
var
  AWidget: PGtkWidget;
  Buffer: PGtkTextBuffer;
  //--
  iPosition: TGtkTextIter;
  GDIObj: PGDIObject = nil;
  pixbuf: PGDKPixBuf = nil;
  scaled: PGDKPixBuf = nil;
  pixmap: PGdkDrawable = nil;
  bitmap: PGdkBitmap = nil;
  iWidth, iHeight: integer;
begin
  //--
  AWidget := PGtkWidget(TextView.Handle);
  AWidget := GetWidgetInfo(AWidget, False)^.CoreWidget;
  if not (Assigned(AWidget)) then
    Exit;
  //--
  Buffer := gtk_text_view_get_buffer(PGtkTextView(AWidget));
  if not (Assigned(Buffer)) then
    Exit;
  //--

    //--
  gtk_text_buffer_get_iter_at_offset(buffer, @iPosition, pos );
  //gtk_text_buffer_get_end_iter(buffer, @iPosition);
  //--
  GDIObj := PGDIObject(Image.Bitmap.Handle);

  //--
  case GDIObj^.GDIBitmapType of
    gbBitmap:
    begin
      bitmap := GDIObj^.GDIBitmapObject;
      gdk_drawable_get_size(bitmap, @iWidth, @iHeight);
      pixbuf := CreatePixbufFromDrawable(bitmap, nil, False,
        0, 0, 0, 0, iWidth, iHeight);
    end;
    gbPixmap:
    begin
      pixmap := GDIObj^.GDIPixmapObject.Image;
      if pixmap <> nil then
      begin
        gdk_drawable_get_size(pixmap, @iWidth, @iHeight);
        bitmap := CreateGdkMaskBitmap(Image.Bitmap.Handle, 0);
        pixbuf := CreatePixbufFromImageAndMask(pixmap, 0, 0,
          iWidth, iHeight, nil, Bitmap);
      end;
    end;
    gbPixbuf:
    begin
      pixbuf := gdk_pixbuf_copy(GDIObj^.GDIPixbufObject);
    end;
  end;

  {if (ResizeWidth > 1) and (ResizeHeight > 1) then
  begin
    scaled := gdk_pixbuf_scale_simple(pixbuf, ResizeWidth,
      ResizeHeight, GDK_INTERP_HYPER);
    g_object_unref(pixbuf);
    pixbuf := scaled;
  end;}

  if (pixbuf <> nil) then
    gtk_text_buffer_insert_pixbuf(buffer, @iPosition, pixbuf);

end;

procedure GetImage(TextView: TWinControl; BMP: TBitmap; Pos: Integer);
var
  AWidget: PGtkWidget;
  Buffer: PGtkTextBuffer;

  iPosition: TGtkTextIter;
  pixbuf: PGDKPixBuf = nil;
  BitmapData: TLazIntfImage = nil;
  Width, Height, rowstride, n_channels, i, j: integer;
  pixels, p: Pguchar;
  RawImageDescription: TRawImageDescription;
begin

  //--
  AWidget := PGtkWidget(TextView.Handle);
  AWidget := GetWidgetInfo(AWidget, False)^.CoreWidget;
  if not (Assigned(AWidget)) then
    Exit;
  //--
  Buffer := gtk_text_view_get_buffer(PGtkTextView(AWidget));
  if not (Assigned(Buffer)) then
    Exit;
  //--

    //--
  gtk_text_buffer_get_iter_at_offset(buffer, @iPosition, Pos);

  //--
  pixbuf := gtk_text_iter_get_pixbuf(@iPosition);

  //--
  if (pixbuf = nil) then
    Exit;

  n_channels := gdk_pixbuf_get_n_channels(pixbuf);
  Width := gdk_pixbuf_get_width(pixbuf);
  Height := gdk_pixbuf_get_height(pixbuf);
  rowstride := gdk_pixbuf_get_rowstride(pixbuf);
  pixels := gdk_pixbuf_get_pixels(pixbuf);

  BMP.Height := Height;
  BMP.Width := Width;
  BMP.PixelFormat := pf32bit;
  BMP.Transparent := True;
  //--
  BitmapData := BMP.CreateIntfImage;
  RawImageDescription := BitmapData.DataDescription;
  AddAlphaToDescription(RawImageDescription, 32);
  BitmapData.DataDescription := RawImageDescription;
  //--
  try
    for i := 0 to Width - 1 do
      for J := 0 to Height - 1 do
      begin
        p := pixels + j * rowstride + i * n_channels;
        PByteArray(BitmapData.PixelData)^[(((J * Width) + i) * 4)] := Ord((p + 2)^);
        PByteArray(BitmapData.PixelData)^[(((J * Width) + i) * 4) + 1] := Ord((p + 1)^);
        PByteArray(BitmapData.PixelData)^[(((J * Width) + i) * 4) + 2] := Ord((p + 0)^);
        PByteArray(BitmapData.PixelData)^[(((J * Width) + i) * 4) + 3] := Ord((P + 3)^);
      end;
    BMP.LoadFromIntfImage(BitmapData);
  finally
    BitmapData.Free;
  end;
  //--

end;

end.

