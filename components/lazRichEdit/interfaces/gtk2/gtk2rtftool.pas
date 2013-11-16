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

unit Gtk2RTFTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, LCLProc, IntfGraphics,
  GraphType, gtk2, glib2, gdk2, pango, Gtk2Proc, Gtk2Def, gdk2pixbuf, Gtk2Globals,
  {Gtk2WSControls,}
  RTFPars_lzRichEdit, FastImage;

const
  UnicodeBulletCode = $2022;



  //************Nome de Fontes *******
  type
    TFNameC=record
      CharPos:Int64;
      FontName:Integer;
    end;

    { TFontNameC }

    TFontNameC= class(TObject)
  private
    FFNameC: array of TFNameC;
  public
    function Count:Cardinal;
    procedure Add(FName:Integer; chPos:Int64);
    function Get(Index: Integer): TFNameC;
    constructor Create;
    destructor Destroy; override;
  end;
  //*********************************************
  //************Tamanho de Fontes *******
  type
    TFSizeC=record
      CharPos:Int64;
      FontSize:Integer;
    end;

    { TFontSizeC }

    TFontSizeC= class(TObject)
  private
    FFSizeC: array of TFSizeC;
  public
    function Count:Cardinal;
    procedure Add(FSize:Integer; chPos:Int64);
    function Get(Index: Integer): TFSizeC;
    constructor Create;
    destructor Destroy; override;
  end;
  //*********************************************
  //************Cor de Fontes *******
  type
    TFColorC=record
      CharPos:Int64;
      FontColor:TColor;
    end;



    { TFontColorC }

    TFontColorC= class(TObject)
  private
    FFColorC: array of TFColorC;
  public
    function Count:Cardinal;
    procedure Add(FColor:TColor; chPos:Int64);
    function Get(Index: Integer): TFColorC;
    constructor Create;
    destructor Destroy; override;
  end;
  //*********************************************

  //************Estilo de Fontes *******
  type
    TFStylesC=record
      CharPos:Int64;
      FontSTyles:TFontStyles;
    end;


    { TFontStylesC }

    TFontStylesC= class(TObject)
  private
    FFStylesC: array of TFStylesC;
  public
    function Count:Cardinal;
    procedure Add(FStyle:TFontStyles; chPos:Int64);
    function Get(Index: Integer): TFStylesC;
    constructor Create;
    destructor Destroy; override;
  end;
  //*********************************************

  //************Alinhamento de Parágrafos *******
  type
    TAlignC=record
      CharPos:Int64;
      Alignment:TAlignment;
    end;


    { TParAlignmentC }

    TParAlignmentC= class(TObject)
  private
    FAlignC: array of TAlignC;
  public
    function Count:Cardinal;
    procedure Add(Al:TAlignment; chPos:Int64);
    function Get(Index: Integer): TAlignC;
    constructor Create;
    destructor Destroy; override;
  end;
  //*********************************************
  //************Recuos de Parágrafos *******
  type
    TIndentC=record
      CharPos:Int64;
      LeftIndent:Integer;
      RightIndent: Integer;
      FirstIndent:Integer;
    end;



    { TParIndentC }

    TParIndentC= class(TObject)
  private
    FIndentC: array of TIndentC;
  public
    function Count:Cardinal;
    function Add(chPos:Int64):Integer;
    procedure AddLeftIdent(LeftIndent:Integer; chPos:Int64);
    procedure AddRightIndent(RightIndent:Integer; chPos:Int64);
    procedure AddFirstIndent(FirstIndent:Integer; chPos:Int64);

    function Get(Index: Integer): TIndentC;
    constructor Create;
    destructor Destroy; override;
  end;
  //*********************************************

type

  TRTFPict = record
    PictType,
    W,
    H,
    WG,
    HG: integer;
    HEX: string;
  end;

  TRSFontAttributes = record
    Charset: TFontCharset;
    Color: TColor;
    Name: TFontName;
    Pitch: TFontPitch;
    fProtected: boolean;
    Size: integer;
    Style: TFontStyles;
  end;

  TRSParaAttributes = record
    Alignment: TAlignment;
    FirstIndent: integer;
    LeftIndent: integer;
    RightIndent: integer;
    Tab: integer;
    TabCount: integer;
  end;

  { TRTFSave }

  TRTFSave = class(TObject)
  protected
    FTextView: TWinControl;
    FRTFout: TStringList;
    Ffonttbl: TStringList;
    Fcolortbl: TStringList;
    FFontAttributes: TRSFontAttributes;
  private
    function AddFont(FontName: string): integer;
    function AddColor(Color: TColor): integer;
    function GetGTKTextBuffer(var TextBuffer: PGtkTextBuffer): boolean;
    function GetText(var S: string): boolean;
    function GetFontAttributes(const Position: integer;
      var FontAttributes: TRSFontAttributes): boolean;
    function GetParaAttributes(const Position: integer;
      var ParaAttributes: TRSParaAttributes): boolean;
    function Start: boolean;
    function GetImage(Picture: TPicture; Position: integer): boolean;
  public
    procedure SaveToStream(Stream: TStream);
    constructor Create(TextView: TWinControl);
    destructor Destroy; override;
  end;

  { TRTFRead }

  TRTFRead = class
  private
    FRTFParser: TRTFParser;
    FTextView: TWinControl;
    RTFPict: TRTFPict;
    FIsPict: boolean;
    FGroups: integer;
    FSkipGroup: integer;
    //--
    FGFontAttributes: array of TRSFontAttributes;
    //--
    {FGUl:Integer;
    FGcf:Integer;
    FGB:Integer;
    FGI:Integer;}
    //--
    FParAlignmentC: TParAlignmentC;
    FParIndentC: TParIndentC;
    FFontNameC: TFontNameC;
    FFontSizeC: TFontSizeC;
    FFontColorC: TFontColorC;
    FFontStylesC: TFontStylesC;
    //--
    FFontStyles:TFontStyles;
    //--
    Buffer: PGtkTextBuffer;
    //--

  private
    function GetGTKTextBuffer(var TextBuffer: PGtkTextBuffer): boolean;
    function GetText(var S: string): boolean;
    function GetLenText: integer;
    procedure InsertPosLastChar(C: TUTF8Char);
    function GetRealTextBuf: string;
    procedure SetFormat(const iSelStart, iSelEnd: integer;
      TextBuffer: PGtkTextBuffer; Tag: Pointer);
    //--
    procedure SetFirstIndent(CharStart: integer; Value: Integer; CharEnd: integer);
    procedure SetLeftIndent(CharStart: integer; Value: Integer; CharEnd: integer);
    procedure SetRightIndent(CharStart: integer; Value: Integer; CharEnd: integer);
    procedure SetAlignment(CharStart: integer; Value: TAlignment; CharEnd: integer);
    procedure SetFontName(CharStart: integer; Value: TFontName; CharEnd: integer);
    procedure SetFontSize(CharStart: integer; Value: Integer; CharEnd: integer);
    procedure SetFontStyles(CharStart: integer; Value: TFontStyles; CharEnd: integer);
    procedure SetFontColor(CharStart: integer; Value: TColor; CharEnd: integer);
    //--
    procedure SetTextAttributes(Pos: integer; Value: TRSFontAttributes; LPos: integer = 1);
    procedure InsertImage(Image: TPicture);
  private
    procedure DoGroup;
    procedure DoWrite;
    procedure DoCtrl;
    //--
    procedure DoSpecialChar;
    procedure DoParAttr;
    procedure DoCharAttr;
    procedure DoPictAttr;
    procedure DoBeginPict;
    procedure DoEndPict;
  private
    procedure SetAlignmentC;
    procedure SetIndentC;
    procedure SetFontNameC;
    procedure SetFontSizeC;
    procedure SetStylesC;
    procedure SetColorC;
  public
    procedure LoadFromStream(Stream: TStream);
    constructor Create(TextView: TWinControl);
    destructor Destroy; override;
  end;


function PNGToRTF(PNG: TPortableNetworkGraphic): string;
function RTFToBMP(const S: string; DataType: integer; var Picture: TPicture;
  Width: integer = 0; Height: integer = 0): boolean;

implementation

function PNGToRTF(PNG: TPortableNetworkGraphic): string;
var
  MemoryStream: TMemoryStream;
  I:Int64=0;
  Len: integer=0;
  Buf: byte = $0;
begin
  Result := '{\pict\pngblip\picw' + IntToStr(PNG.Width * 15) + '\pich' +
    IntToStr(PNG.Height * 15) + '\picwgoal' + IntToStr(PNG.Width * 15) +
    '\pichgoal' + IntToStr(PNG.Height * 15) + UnicodeToUTF8($A);
  //--
  MemoryStream := TMemoryStream.Create;
  PNG.SaveToStream(MemoryStream);
  MemoryStream.Seek(0, soBeginning);
  Len := 0;
  //--
 { for I := 0 to MemoryStream.Size do
  begin
    if Len = 39 then
    begin
      Result := Result + UnicodeToUTF8($A);
      Len := 0;
    end;
    MemoryStream.Read(Buf, 1);
    Result := Result + LowerCase(IntToHex(Buf, 2));
    Len := Len + 1;
  end;}
  //--
  while I <= MemoryStream.Size do
  begin
    if Len = 39 then
    begin
      Result := Result + UnicodeToUTF8($A);
      Len := 0;
    end;
    MemoryStream.Read(Buf, 1);
    Result := Result + LowerCase(IntToHex(Buf, 2));
    Len := Len + 1;
    Inc(I);
  end;

  //--
  MemoryStream.Free;
  Result := Result + '}';
end;

function RTFToBMP(const S: string; DataType: integer; var Picture: TPicture;
  Width: integer; Height: integer): boolean;
var
  MStream: TMemoryStream;
  Pict: TPicture;
  I: integer = 1;
  B: byte = 0;
  L: integer;
  S2: string;
begin
  Result := False;
  MStream := TMemoryStream.Create;
  MStream.Seek(0, soBeginning);
  L := UTF8Length(S);
  while True do
  begin
    S2 := S[I] + S[I + 1];
    if (S2 <> '') then
      B := StrToInt('$' + trim(S2))
    else
      B := $0;
    MStream.Write(B, 1);
    I := I + 2;
    if (I > L) then
      Break;
  end;

  if DataType = 18 then
  begin
    MStream.Seek(0, soBeginning);
    if (Width = 0) and (Height = 0) then
      Picture.PNG.LoadFromStream(MStream)
    else
    begin
      Pict := TPicture.Create;
      Pict.PNG.LoadFromStream(MStream);
      //--
      Picture.PNG.Width := Width div 15;
      Picture.PNG.Height := Height div 15;
      Picture.PNG.Clear;
      Picture.PNG.Clear;
      //--
      Picture.PNG.Canvas.CopyRect(Rect(0, 0, Width div 15, Height div 15),
        Pict.PNG.Canvas,
        Rect(0, 0, Pict.PNG.Width, Pict.PNG.Height));
      Pict.Free;
    end;
    Result := True;
  end;

  MStream.Free;
end;

{ TFontColorC }

function TFontColorC.Count: Cardinal;
begin
  Result:= Length(FFColorC);
end;

procedure TFontColorC.Add(FColor: TColor; chPos: Int64);
var
  I:Integer=0;
begin
  {for I:= 0 to Count -1 do
    begin
      if FFColorC[I].CharPos = chPos then
        begin
          FFColorC[I].FontColor:= FColor;
          Exit;
        end;
    end;}
  while I < Count do
  begin
    if FFColorC[I].CharPos = chPos then
      begin
        FFColorC[I].FontColor:= FColor;
        Exit;
      end;
  Inc(I);
  end;

  SetLength(FFColorC, Count + 1);
  FFColorC[Count -1].FontColor:= FColor;
  FFColorC[Count -1].CharPos:= chPos;
end;

function TFontColorC.Get(Index: Integer): TFColorC;
begin
  Result.CharPos:= FFColorC[Index].CharPos;
  Result.FontColor:= FFColorC[Index].FontColor;
end;

constructor TFontColorC.Create;
begin
  SetLength(FFColorC, 0);
end;

destructor TFontColorC.Destroy;
begin
  SetLength(FFColorC, 0);
  inherited Destroy;
end;

{ TFontStylesC }

function TFontStylesC.Count: Cardinal;
begin
  Result:=Length(FFStylesC);
end;

procedure TFontStylesC.Add(FStyle: TFontStyles; chPos: Int64);
var
  I:Integer=0;
begin
  {for I:= 0 to Count -1 do
    begin
      if FFStylesC[I].CharPos = chPos then
        begin
          FFStylesC[I].FontSTyles:= FStyle;
          Exit;
        end;
    end;}
while I < Count do
  begin
      if FFStylesC[I].CharPos = chPos then
        begin
          FFStylesC[I].FontSTyles:= FStyle;
          Exit;
        end;
  Inc(I);
  end;

  SetLength(FFStylesC, Count + 1);
  FFStylesC[Count -1].FontSTyles:= FStyle;
  FFStylesC[Count -1].CharPos:= chPos;
end;

function TFontStylesC.Get(Index: Integer): TFStylesC;
begin
  Result.CharPos:= FFStylesC[Index].CharPos;
  Result.FontSTyles:= FFStylesC[Index].FontSTyles;
end;

constructor TFontStylesC.Create;
begin
  SetLength(FFStylesC, 0);
end;

destructor TFontStylesC.Destroy;
begin
  SetLength(FFStylesC, 0);
  inherited Destroy;
end;

{ TFontSizeC }

function TFontSizeC.Count: Cardinal;
begin
  Result:=Length(FFSizeC);
end;

procedure TFontSizeC.Add(FSize: Integer; chPos: Int64);
var
  I:Integer=0;
begin
  {for I:= 0 to Count -1 do
    begin
      if FFSizeC[I].CharPos = chPos then
        begin
          FFSizeC[I].FontSize:= FSize;
          Exit;
        end;
    end;}

  while I < Count do
  begin
      if FFSizeC[I].CharPos = chPos then
        begin
          FFSizeC[I].FontSize:= FSize;
          Exit;
        end;
  Inc(I);
  end;

  SetLength(FFSizeC, Count + 1);
  FFSizeC[Count -1].FontSize:= FSize;
  FFSizeC[Count -1].CharPos:= chPos;
end;

function TFontSizeC.Get(Index: Integer): TFSizeC;
begin
  Result.CharPos:= FFSizeC[Index].CharPos;
  Result.FontSize:= FFSizeC[Index].FontSize;
end;

constructor TFontSizeC.Create;
begin
  SetLength(FFSizeC, 0);
end;

destructor TFontSizeC.Destroy;
begin
  SetLength(FFSizeC, 0);
  inherited Destroy;
end;

{ TFontNameC }

function TFontNameC.Count: Cardinal;
begin
  Result:= Length(FFNameC);
end;

procedure TFontNameC.Add(FName: Integer; chPos: Int64);
var
  I:Integer=0;
begin
  {for I:= 0 to Count -1 do
    begin
      if FFNameC[I].CharPos = chPos then
        begin
          FFNameC[I].FontName:= FName;
          Exit;
        end;
    end;}
  while I < Count do
  begin
      if FFNameC[I].CharPos = chPos then
        begin
          FFNameC[I].FontName:= FName;
          Exit;
        end;
  Inc(I);
  end;
  SetLength(FFNameC, Count + 1);
  FFNameC[Count -1].FontName:= FName;
  FFNameC[Count -1].CharPos:= chPos;
end;

function TFontNameC.Get(Index: Integer): TFNameC;
begin
  Result.CharPos:= FFNameC[Index].CharPos;
  Result.FontName:= FFNameC[Index].FontName;
end;

constructor TFontNameC.Create;
begin
  SetLength(FFNameC, 0);
end;

destructor TFontNameC.Destroy;
begin
  SetLength(FFNameC, 0);
  inherited Destroy;
end;

{ TParIndentC }

function TParIndentC.Count: Cardinal;
begin
  Result:= Length(FIndentC);
end;

function TParIndentC.Add(chPos: Int64): Integer;
var
  I:Integer=0;
begin
  {for I:= 0 to Count -1 do
    if FIndentC[I].CharPos = chPos then
      begin
        Result:= I;
        Exit;
      end;}
  //--

  while I < Count do
  begin
    if FIndentC[I].CharPos = chPos then
      begin
        Result:= I;
        Exit;
      end;
  Inc(I);
  end;

  SetLength(FIndentC, Count + 1);
  FIndentC[Count -1].CharPos:= chPos;
  Result:= Count -1;
end;

procedure TParIndentC.AddLeftIdent(LeftIndent: Integer; chPos: Int64);
var
  Index: Integer;
begin
  Index:= Add(chPos);
  FIndentC[Index].LeftIndent:= LeftIndent;
end;

procedure TParIndentC.AddRightIndent(RightIndent: Integer; chPos: Int64);
var
  Index: Integer;
begin
  Index:= Add(chPos);
  FIndentC[Index].RightIndent:= RightIndent;
end;

procedure TParIndentC.AddFirstIndent(FirstIndent: Integer; chPos: Int64);
var
  Index: Integer;
begin
  Index:= Add(chPos);
  FIndentC[Index].FirstIndent:= FirstIndent;
end;

function TParIndentC.Get(Index: Integer): TIndentC;
begin
  Result.CharPos:= FIndentC[Index].CharPos;
  Result.LeftIndent:= FIndentC[Index].LeftIndent;
  Result.RightIndent:= FIndentC[Index].RightIndent;
  Result.FirstIndent:= FIndentC[Index].FirstIndent;

end;

constructor TParIndentC.Create;
begin
  SetLength(FIndentC, 0);
end;

destructor TParIndentC.Destroy;
begin
  SetLength(FIndentC, 0);
  inherited Destroy;
end;

{ TParAlignmentC }

function TParAlignmentC.Count: Cardinal;
begin
  Result:= Length(FAlignC);
end;

procedure TParAlignmentC.Add(Al: TAlignment; chPos: Int64);
begin
  SetLength(FAlignC, Count + 1);
  FAlignC[Count - 1].Alignment:= Al;
  FAlignC[Count - 1].CharPos:= chPos;
end;

function TParAlignmentC.Get(Index: Integer): TAlignC;
begin
  Result.CharPos:= FAlignC[Index].CharPos;
  Result.Alignment:= FAlignC[Index].Alignment;
end;

constructor TParAlignmentC.Create;
begin
  SetLength(FAlignC, 0);
end;

destructor TParAlignmentC.Destroy;
begin
  SetLength(FAlignC, 0);
  inherited Destroy;
end;

{ TRTFRead }

function TRTFRead.GetGTKTextBuffer(var TextBuffer: PGtkTextBuffer): boolean;
var
  AWidget: PGtkWidget;
begin
  Result := False;
  //--
  AWidget := PGtkWidget(FTextView.Handle);
  AWidget := GetWidgetInfo(AWidget, False)^.CoreWidget;
  if not (Assigned(AWidget)) then
    Exit;
  //--
  TextBuffer := gtk_text_view_get_buffer(PGtkTextView(AWidget));
  if not (Assigned(TextBuffer)) then
    Exit;
  //--
  Result := True;
end;

function TRTFRead.GetText(var S: string): boolean;
var
  iterStart, iterEnd: TGtkTextIter;
begin
  Result := False;

  gtk_text_buffer_get_start_iter(Buffer, @iterStart);
  gtk_text_buffer_get_end_iter(Buffer, @iterEnd);
  S := gtk_text_buffer_get_slice(Buffer, @iterStart, @iterEnd, gboolean(False));

  Result := True;
end;

function TRTFRead.GetLenText: integer;
begin
  Result := gtk_text_buffer_get_char_count(Buffer);
end;

procedure TRTFRead.InsertPosLastChar(C: TUTF8Char);
var
  iterStart: TGtkTextIter;
  Ch: TUTF8Char;
begin
  Ch := C;
  gtk_text_buffer_get_end_iter(Buffer, @iterStart);
  gtk_text_buffer_insert(Buffer, @iterStart, @Ch[1], Length(Ch));
end;

function TRTFRead.GetRealTextBuf: string;
begin
  Result := '';
  GetText(Result);
end;

procedure TRTFRead.SetFormat(const iSelStart, iSelEnd: integer;
  TextBuffer: PGtkTextBuffer; Tag: Pointer);
var
  iterStart, iterEnd: TGtkTextIter;
begin
  gtk_text_buffer_get_iter_at_offset(TextBuffer, @iterStart, iSelStart);
  gtk_text_buffer_get_iter_at_offset(TextBuffer, @iterEnd, iSelEnd);
  gtk_text_buffer_apply_tag(TextBuffer, tag, @iterStart, @iterEnd);
end;

procedure TRTFRead.SetFirstIndent(CharStart: integer; Value: Integer; CharEnd: integer);
var
  Tag: Pointer = nil;
begin
  //--
  Tag := gtk_text_buffer_create_tag(buffer, nil, 'indent',
    [Value, 'indent-set', gboolean(gTRUE), nil]);

  SetFormat(CharStart, CharEnd, Buffer, Tag);
end;

procedure TRTFRead.SetLeftIndent(CharStart: integer; Value: Integer; CharEnd: integer);
var
  Tag: Pointer = nil;
begin
  Tag := gtk_text_buffer_create_tag(buffer, nil, 'left_margin',
    [Value, 'left_margin-set', gboolean(gTRUE), nil]);
  SetFormat(CharStart, CharEnd, Buffer, Tag);
end;

procedure TRTFRead.SetRightIndent(CharStart: integer; Value: Integer; CharEnd: integer);
var
  Tag: Pointer = nil;
begin
  Tag := gtk_text_buffer_create_tag(buffer, nil, 'right_margin',
    [Value, 'right_margin-set', gboolean(gTRUE), nil]);
  SetFormat(CharStart, CharEnd, Buffer, Tag);
end;

procedure TRTFRead.SetAlignment(CharStart: integer; Value: TAlignment; CharEnd: integer);
const
  GTKJustification: array [TAlignment] of integer =
    (GTK_JUSTIFY_LEFT, GTK_JUSTIFY_RIGHT, GTK_JUSTIFY_CENTER);
var
  Tag: Pointer = nil;
begin
  Tag := gtk_text_buffer_create_tag(buffer, nil, 'justification',
    [GTKJustification[Value], 'justification-set', gboolean(gTRUE), nil]);
  SetFormat(CharStart, CharEnd, Buffer, Tag);
end;

procedure TRTFRead.SetFontName(CharStart: integer; Value: TFontName;
  CharEnd: integer);
var
  Tag: Pointer = nil;
begin
  Tag := gtk_text_buffer_create_tag(buffer, nil, 'family',
    [@Value[1], 'family-set', gTRUE, nil]);
  SetFormat(CharStart, CharEnd, Buffer, Tag);
end;

procedure TRTFRead.SetFontSize(CharStart: integer; Value: Integer;
  CharEnd: integer);
var
  Tag: Pointer = nil;
begin
  Tag := gtk_text_buffer_create_tag(buffer, nil, 'size-points',
    [double(Value), nil]);
  SetFormat(CharStart, CharEnd, Buffer, Tag);
end;

procedure TRTFRead.SetFontStyles(CharStart: integer; Value: TFontStyles;
  CharEnd: integer);
var
  Tag: Pointer = nil;
const
  PangoUnderline: array [boolean] of integer = (PANGO_UNDERLINE_NONE, PANGO_UNDERLINE_SINGLE);
  PangoBold: array [boolean] of integer = (PANGO_WEIGHT_NORMAL, PANGO_WEIGHT_BOLD);
  PangoItalic: array [boolean] of integer = (PANGO_STYLE_NORMAL, PANGO_STYLE_ITALIC);

begin
  //--
  Tag := gtk_text_buffer_create_tag(buffer, nil, 'underline',
    [PangoUnderline[fsUnderline in Value], 'underline-set',
    gboolean(gTRUE), 'weight', PangoBold[fsBold in Value],
    'weight-set', gboolean(gTRUE), 'style', PangoItalic[fsItalic in Value],
    'style-set', gboolean(gTRUE), 'strikethrough', gboolean(fsStrikeOut in Value),
    'strikethrough-set', gboolean(gTRUE), nil]);
  SetFormat(CharStart, CharEnd, Buffer, Tag);
end;

procedure TRTFRead.SetFontColor(CharStart: integer; Value: TColor;
  CharEnd: integer);
var
  FontColor: TGDKColor;
  Tag: Pointer = nil;
begin
  FontColor := TColortoTGDKColor(Value);

  Tag := gtk_text_buffer_create_tag(buffer, nil, 'foreground-gdk',
    [@FontColor, 'foreground-set', gboolean(gTRUE), nil]);
  SetFormat(CharStart, CharEnd, Buffer, Tag);
end;


procedure TRTFRead.SetTextAttributes(Pos: integer; Value: TRSFontAttributes;
  LPos: integer = 1);
var
  FontFamily: string;
  FontColor: TGDKColor;
  Tag: Pointer = nil;
const
  PangoUnderline: array [boolean] of integer =
    (PANGO_UNDERLINE_NONE, PANGO_UNDERLINE_SINGLE);
  PangoBold: array [boolean] of integer = (PANGO_WEIGHT_NORMAL, PANGO_WEIGHT_BOLD);
  PangoItalic: array [boolean] of integer = (PANGO_STYLE_NORMAL, PANGO_STYLE_ITALIC);
begin

  FontColor := TColortoTGDKColor(Value.Color);
  //--
  FontFamily := Value.Name;
  if (FontFamily = '') then
    FontFamily := #0;
  //--
  Tag := gtk_text_buffer_create_tag(buffer, nil, 'family',
    [@FontFamily[1], 'family-set', gTRUE, 'size-points', double(Value.Size),
    'foreground-gdk', @FontColor, 'foreground-set', gboolean(gTRUE),
    'underline', PangoUnderline[fsUnderline in Value.Style], 'underline-set',
    gboolean(gTRUE), 'weight', PangoBold[fsBold in Value.Style],
    'weight-set', gboolean(gTRUE), 'style', PangoItalic[fsItalic in Value.Style],
    'style-set', gboolean(gTRUE), 'strikethrough', gboolean(fsStrikeOut in Value.Style),
    'strikethrough-set', gboolean(gTRUE), nil]);

  SetFormat(Pos, LPos, Buffer, Tag);
end;

procedure TRTFRead.InsertImage(Image: TPicture);
var
  iPosition: TGtkTextIter;
  GDIObj: PGDIObject = nil;
  pixbuf: PGDKPixBuf = nil;
  pixmap: PGdkDrawable = nil;
  bitmap: PGdkBitmap = nil;
  iWidth, iHeight: integer;
begin

  //--
  gtk_text_buffer_get_end_iter(buffer, @iPosition);
  //--
  GDIObj := PGDIObject(Image.Bitmap.Handle);

  //--
  case GDIObj^.GDIBitmapType of
    gbBitmap:
    begin
      bitmap := GDIObj^.GDIBitmapObject;
      gdk_drawable_get_size(bitmap, @iWidth, @iHeight);
      pixbuf := CreatePixbufFromDrawable(bitmap, nil, False, 0,
        0, 0, 0, iWidth, iHeight);
    end;
    gbPixmap:
    begin
      pixmap := GDIObj^.GDIPixmapObject.Image;
      if pixmap <> nil then
      begin
        gdk_drawable_get_size(pixmap, @iWidth, @iHeight);
        bitmap := CreateGdkMaskBitmap(Image.Bitmap.Handle, 0);
        pixbuf := CreatePixbufFromImageAndMask(pixmap, 0, 0, iWidth,
          iHeight, nil, Bitmap);
      end;
    end;
    gbPixbuf:
    begin
      pixbuf := gdk_pixbuf_copy(GDIObj^.GDIPixbufObject);
    end;
  end;

  if (pixbuf <> nil) then
    gtk_text_buffer_insert_pixbuf(buffer, @iPosition, pixbuf);
end;

procedure TRTFRead.DoGroup;
begin
  if (FRTFParser.RTFMajor = rtfBeginGroup) then
  begin
    FGroups := FGroups + 1;
    SetLength(FGFontAttributes, FGroups);
    if (FGroups > 1) then
      begin
        FGFontAttributes[FGroups -1].Name := FGFontAttributes[FGroups -2].Name;
        FGFontAttributes[FGroups -1].Color := FGFontAttributes[FGroups -2].Color;
        FGFontAttributes[FGroups -1].Size := FGFontAttributes[FGroups -2].Size;
        FGFontAttributes[FGroups -1].Style := [];
        //--
        FFontStyles:= [];
      end;
    {FFontStylesC.Add(FGFontAttributes[FGroups -1].Style, GetLenText);
    FFontNameC.Add(StrToInt(FGFontAttributes[FGroups -1].Name), GetLenText);
    FFontSizeC.Add(FGFontAttributes[FGroups -1].Size, GetLenText);
    FFontColorC.Add(FGFontAttributes[FGroups -1].Color, GetLenText);}
  end
  else
  if (FRTFParser.RTFMajor = rtfEndGroup) then
    begin
    FGroups := FGroups - 1;
    SetLength(FGFontAttributes, FGroups);
    if (FGroups > 0) then
      begin
        FFontStylesC.Add(FGFontAttributes[FGroups -1].Style, GetLenText);
        FFontNameC.Add(StrToInt(FGFontAttributes[FGroups -1].Name), GetLenText);
        FFontSizeC.Add(FGFontAttributes[FGroups -1].Size, GetLenText);
        FFontColorC.Add(FGFontAttributes[FGroups -1].Color, GetLenText);
       end;

    end;

  if (FGroups < FSkipGroup) then
    FSkipGroup := -1;end;

procedure TRTFRead.DoWrite;
var
  C: TUTF8char;
begin
  C := UnicodeToUTF8(FRTFParser.RTFMajor);
  if FIsPict then
    RTFPict.HEX := RTFPict.HEX + C
  else
  begin
    if (FSkipGroup = -1) and (FRTFParser.RTFMajor = 183) then
    begin
      InsertPosLastChar(UnicodeToUTF8(UnicodeBulletCode));
      C := chr($0);
    end;

    if (FSkipGroup = -1) and (C <> chr($0)) then
    begin
      if C = #194 + #149 then
         C:= UnicodeToUTF8(UnicodeBulletCode);

      InsertPosLastChar(C);
    end;
  end;

end;

procedure TRTFRead.DoCtrl;
begin
  case FRTFParser.RTFMajor of
    rtfSpecialChar: DoSpecialChar;
    rtfParAttr: DoParAttr;
    rtfCharAttr: DoCharAttr;
    rtfPictAttr: DoPictAttr;
  end;
end;

procedure TRTFRead.DoSpecialChar;
begin
  case FRTFParser.rtfMinor of
    rtfPar:
    begin
      if (FSkipGroup = -1) then
        InsertPosLastChar(#10);
    end;
    rtfTab:
    begin
      if (FSkipGroup = -1) then
      begin
        InsertPosLastChar(#9);
      end;

    end;
    rtfOptDest:
    begin
      if (FSkipGroup = -1) then
        FSkipGroup := FGroups;
    end;
  end;

end;

procedure TRTFRead.DoParAttr;
begin
//--
case FRTFParser.rtfMinor of
  rtfParDef:begin
    FParIndentC.AddFirstIndent(0, GetLenText);
    FParIndentC.AddLeftIdent(0, GetLenText);
    FParIndentC.AddRightIndent(0, GetLenText);
    if (FGroups <= 1) then
      begin
      FParAlignmentC.Add(taLeftJustify, GetLenText);
      FFontColorC.Add(clWindowText, GetLenText);
      end;
  end;
  rtfQuadLeft: FParAlignmentC.Add(taLeftJustify, GetLenText);
  rtfQuadRight:FParAlignmentC.Add(taRightJustify, GetLenText);
  rtfQuadJust:FParAlignmentC.Add(taLeftJustify, GetLenText);
  rtfQuadCenter:FParAlignmentC.Add(taCenter, GetLenText);
  rtfFirstIndent:FParIndentC.AddFirstIndent(FRTFParser.rtfParam div 568 * 37, GetLenText);
  rtfLeftIndent:FParIndentC.AddLeftIdent(FRTFParser.rtfParam div 568 * 37, GetLenText);
  rtfRightIndent:FParIndentC.AddRightIndent(FRTFParser.rtfParam div 568 * 37, GetLenText);
end;
end;

procedure TRTFRead.DoCharAttr;
begin
  case FRTFParser.rtfMinor of
    rtfBold:
    begin
      if (FRTFParser.rtfParam <> rtfNoParam) then
        begin
          if (fsBold in FFontStyles) then
            FFontStyles := FFontStyles - [fsBold];
          //FGB:= -1;
        end
      else
        begin
          if not(fsBold in FFontStyles) then
            FFontStyles := FFontStyles + [fsBold];
          //FGB:= FGroups;
        end;
      FFontStylesC.Add(FFontStyles, GetLenText);
      FGFontAttributes[FGroups -1].Style:= FFontStyles;
    end;
    rtfItalic:
    begin
      if (FRTFParser.rtfParam <> rtfNoParam) then
        begin
          if (fsItalic in FFontStyles) then
            FFontStyles := FFontStyles - [fsItalic];
            //FGI:= -1;
        end
      else
        begin
          if not(fsItalic in FFontStyles) then
            FFontStyles := FFontStyles + [fsItalic];
            //FGI:= FGroups;
        end;
      FFontStylesC.Add(FFontStyles, GetLenText);
      FGFontAttributes[FGroups -1].Style:= FFontStyles;
    end;
    rtfStrikeThru:
    begin
      if (FRTFParser.rtfParam <> rtfNoParam) then
        begin
          if (fsStrikeOut in FFontStyles) then
            FFontStyles := FFontStyles - [fsStrikeOut];
        end
      else
        begin
          if not(fsStrikeOut in FFontStyles) then
            FFontStyles := FFontStyles + [fsStrikeOut];
        end;
      FFontStylesC.Add(FFontStyles, GetLenText);
      FGFontAttributes[FGroups -1].Style:= FFontStyles;
    end;
    rtfFontNum:
    begin
      FFontNameC.Add(FRTFParser.rtfParam, GetLenText);
      FGFontAttributes[FGroups -1].Name:= IntToStr(FRTFParser.rtfParam);
    end;
    rtfFontSize:
    begin
      FFontSizeC.Add(FRTFParser.rtfParam div 2, GetLenText);
      FGFontAttributes[FGroups -1].Size:= FRTFParser.rtfParam div 2;
    end;
    rtfUnderline:
    begin
      if not(fsUnderline in FFontStyles) then
        begin
          FFontStyles := FFontStyles + [fsUnderline];
          //FGUl:= FGroups;
        end;
      FFontStylesC.Add(FFontStyles, GetLenText);
      FGFontAttributes[FGroups -1].Style:= FFontStyles;
    end;
    rtfNoUnderline:
    begin
      if (fsUnderline in FFontStyles) then
        begin
          FFontStyles := FFontStyles - [fsUnderline];
          //FGUl:= -1;
        end;
      FFontStylesC.Add(FFontStyles, GetLenText);
      FGFontAttributes[FGroups -1].Style:= FFontStyles;
    end;
    rtfForeColor:
    begin
      //FGcf:= FGroups;
      if (FRTFParser.Colors[FRTFParser.rtfParam] = nil) or
      (FRTFParser.rtfParam = 0) then
        FFontColorC.Add(clWindowText, GetLenText)
      else
      FFontColorC.Add(RGBToColor(FRTFParser.Colors[FRTFParser.rtfParam]^.rtfCRed,
                                 FRTFParser.Colors[FRTFParser.rtfParam]^.rtfCGreen,
                                 FRTFParser.Colors[FRTFParser.rtfParam]^.rtfCBlue), GetLenText);
    FGFontAttributes[FGroups -1].Color:= FFontColorC.Get(FFontColorC.Count -1).FontColor;
    end;
  end;
end;

procedure TRTFRead.DoPictAttr;
begin
  if (FRTFParser.rtfMajor = rtfPictAttr) and (FRTFParser.rtfMinor in
    [rtfMacQD .. rtfpngblip]) then
    case FRTFParser.rtfMinor of
      rtfPicWid: RTFPict.W := FRTFParser.rtfParam;
      rtfPicHt: RTFPict.H := FRTFParser.rtfParam;
      rtfPicGoalWid: RTFPict.WG := FRTFParser.rtfParam;
      rtfPicGoalHt: RTFPict.HG := FRTFParser.rtfParam;
      rtfpngblip: RTFPict.PictType := rtfpngblip;
    end;
end;

procedure TRTFRead.DoBeginPict;
begin
  RTFPict.HEX := '';
  RTFPict.H := 0;
  RTFPict.HG := 0;
  RTFPict.W := 0;
  RTFPict.WG := 0;
  RTFPict.PictType := -1;
  FIsPict := True;
end;

procedure TRTFRead.DoEndPict;
var
  Picture: TPicture;
  R: boolean = False;
begin
  FIsPict := False;
  Picture := TPicture.Create;

  if (RTFPict.WG = 0) and (RTFPict.HG = 0) or (RTFPict.WG = RTFPict.W) and
    (RTFPict.HG = RTFPict.H) then
    R := RTFToBMP(RTFPict.HEX, RTFPict.PictType, Picture)
  else
    R := RTFToBMP(RTFPict.HEX, RTFPict.PictType, Picture, RTFPict.WG, RTFPict.HG);

  if R then
    InsertImage(Picture);

  Picture.Free;
end;

procedure TRTFRead.SetAlignmentC;
var
  I:Integer=0;
  ChStart:Integer = 0;
  ChEnd:Integer = 0;
begin
  {for I:=0 to FParAlignmentC.Count -1 do
    begin
      ChStart:= FParAlignmentC.Get(I).CharPos;
      if ((I + 1) < (FParAlignmentC.Count -1)) then
        ChEnd:= FParAlignmentC.Get(I + 1).CharPos
      else
        ChEnd:= GetLenText;
      //--
      SetAlignment(ChStart, FParAlignmentC.Get(I).Alignment, ChEnd);
    end;}
while I < FParAlignmentC.Count do
begin
  ChStart:= FParAlignmentC.Get(I).CharPos;
  if ((I + 1) < (FParAlignmentC.Count -1)) then
    ChEnd:= FParAlignmentC.Get(I + 1).CharPos
  else
    ChEnd:= GetLenText;
  //--
  SetAlignment(ChStart, FParAlignmentC.Get(I).Alignment, ChEnd);
 Inc(I)
end;
end;

procedure TRTFRead.SetIndentC;
var
  I:Integer=0;
  ChStart:Integer = 0;
  ChEnd:Integer = 0;
begin
  {for I:=0 to FParIndentC.Count -1 do
    begin
      ChStart:= FParIndentC.Get(I).CharPos;
      if ((I + 1) < (FParIndentC.Count -1)) then
        ChEnd:= FParIndentC.Get(I + 1).CharPos
      else
        ChEnd:= GetLenText;
      //--
      SetFirstIndent(ChStart, FParIndentC.Get(I).FirstIndent, ChEnd);
      SetLeftIndent(ChStart, FParIndentC.Get(I).LeftIndent, ChEnd);
      SetRightIndent(ChStart, FParIndentC.Get(I).RightIndent, ChEnd);
    end;}
  while I < FParIndentC.Count do
  begin
      ChStart:= FParIndentC.Get(I).CharPos;
      if ((I + 1) < (FParIndentC.Count -1)) then
        ChEnd:= FParIndentC.Get(I + 1).CharPos
      else
        ChEnd:= GetLenText;
      //--
      SetFirstIndent(ChStart, FParIndentC.Get(I).FirstIndent, ChEnd);
      SetLeftIndent(ChStart, FParIndentC.Get(I).LeftIndent, ChEnd);
      SetRightIndent(ChStart, FParIndentC.Get(I).RightIndent, ChEnd);
   Inc(I)
  end;
end;

procedure TRTFRead.SetFontNameC;
var
  I:Integer=0;
  ChStart:Integer = 0;
  ChEnd:Integer = 0;
begin
  {for I:=0 to FFontNameC.Count -1 do
    begin
      ChStart:= FFontNameC.Get(I).CharPos;
      if ((I + 1) < (FFontNameC.Count -1)) then
        ChEnd:= FFontNameC.Get(I + 1).CharPos
      else
        ChEnd:= GetLenText;
      //--
        SetFontName(ChStart, FRTFParser.Fonts[FFontNameC.Get(I).FontName]^.rtfFName, ChEnd);
    end;}
  while I < FFontNameC.Count do
  begin
      ChStart:= FFontNameC.Get(I).CharPos;
      if ((I + 1) < (FFontNameC.Count -1)) then
        ChEnd:= FFontNameC.Get(I + 1).CharPos
      else
        ChEnd:= GetLenText;
      //--
        SetFontName(ChStart, FRTFParser.Fonts[FFontNameC.Get(I).FontName]^.rtfFName, ChEnd);
   Inc(I)
  end;
end;

procedure TRTFRead.SetFontSizeC;
var
  I:Integer=0;
  ChStart:Integer = 0;
  ChEnd:Integer = 0;
begin
  {for I:=0 to FFontSizeC.Count -1 do
    begin
      ChStart:= FFontSizeC.Get(I).CharPos;
      if ((I + 1) < (FFontSizeC.Count -1)) then
        ChEnd:= FFontSizeC.Get(I + 1).CharPos
      else
        ChEnd:= GetLenText;
      //--
        SetFontSize(ChStart,FFontSizeC.Get(I).FontSize , ChEnd);
    end;}
  while I < FFontSizeC.Count do
  begin
      ChStart:= FFontSizeC.Get(I).CharPos;
      if ((I + 1) < (FFontSizeC.Count -1)) then
        ChEnd:= FFontSizeC.Get(I + 1).CharPos
      else
        ChEnd:= GetLenText;
      //--
        SetFontSize(ChStart,FFontSizeC.Get(I).FontSize , ChEnd);
   Inc(I)
  end;
end;

procedure TRTFRead.SetStylesC;
var
  I:Integer=0;
  ChStart:Integer = 0;
  ChEnd:Integer = 0;
begin
  {for I:=0 to FFontStylesC.Count -1 do
    begin
      ChStart:= FFontStylesC.Get(I).CharPos;
      if ((I + 1) < (FFontStylesC.Count -1)) then
        ChEnd:= FFontStylesC.Get(I + 1).CharPos
      else
        ChEnd:= GetLenText;
      //--
        SetFontStyles(ChStart,FFontStylesC.Get(I).FontSTyles , ChEnd);
    end;}
  while I < FFontStylesC.Count do
  begin
      ChStart:= FFontStylesC.Get(I).CharPos;
      if ((I + 1) < (FFontStylesC.Count -1)) then
        ChEnd:= FFontStylesC.Get(I + 1).CharPos
      else
        ChEnd:= GetLenText;
      //--
        SetFontStyles(ChStart,FFontStylesC.Get(I).FontSTyles , ChEnd);
   Inc(I)
  end;
end;

procedure TRTFRead.SetColorC;
var
  I:Integer=0;
  ChStart:Integer = 0;
  ChEnd:Integer = 0;
begin
  {for I:=0 to FFontColorC.Count -1 do
    begin
      ChStart:= FFontColorC.Get(I).CharPos;
      if ((I + 1) < (FFontColorC.Count -1)) then
        ChEnd:= FFontColorC.Get(I + 1).CharPos
      else
        ChEnd:= GetLenText;
      //--
      SetFontColor(ChStart, FFontColorC.Get(I).FontColor , ChEnd);
    end;}

  while I < FFontColorC.Count do
  begin
      ChStart:= FFontColorC.Get(I).CharPos;
      if ((I + 1) < (FFontColorC.Count -1)) then
        ChEnd:= FFontColorC.Get(I + 1).CharPos
      else
        ChEnd:= GetLenText;
      //--
      SetFontColor(ChStart, FFontColorC.Get(I).FontColor , ChEnd);
   Inc(I)
  end;
end;

procedure TRTFRead.LoadFromStream(Stream: TStream);
begin
  //--
  FGroups := 0;
  FSkipGroup := -1;
  FFontStyles:= [];
  //--
  SetLength(FGFontAttributes, 1);
  FGFontAttributes[0].Color:= clWindowText;
  FGFontAttributes[0].Size:= 10;
  FGFontAttributes[0].Name:='0';
  FGFontAttributes[0].Style:=[];
  //--
  //--
  FParAlignmentC:= TParAlignmentC.Create;
  FParIndentC:= TParIndentC.Create;
  FFontNameC:= TFontNameC.Create;
  FFontSizeC:= TFontSizeC.Create;
  FFontStylesC:= TFontStylesC.Create;
  FFontColorC:= TFontColorC.Create;
  //--
  FRTFParser := TRTFParser.Create(Stream);
  FRTFParser.classcallbacks[rtfText] := @DoWrite;
  FRTFParser.classcallbacks[rtfcontrol] := @DoCtrl;
  FRTFParser.classcallbacks[rtfGroup] := @DoGroup;
  FRTFParser.OnRTFBeginPict := @DoBeginPict;
  FRTFParser.OnRTFEndPict := @DoEndPict;
  FRTFParser.StartReading;

  //--
  SetAlignmentC;
  SetIndentC;
  SetFontNameC;
  SetFontSizeC;
  SetStylesC;
  SetColorC;

  //--
  FParAlignmentC.Free;
  FParIndentC.Free;
  FFontNameC.Free;
  FFontSizeC.Free;
  FFontStylesC.Free;
  FFontColorC.Free;
  //--
  FRTFParser.Free;
  SetLength(FGFontAttributes, 0);


end;

constructor TRTFRead.Create(TextView: TWinControl);
begin
  inherited Create;
  FTextView := TextView;
  FIsPict := False;
  GetGTKTextBuffer(Buffer);
end;

destructor TRTFRead.Destroy;
begin
  inherited Destroy;
end;

{ TRTFSave }

function TRTFSave.AddFont(FontName: string): integer;
var
I:Integer=0;
begin
  //if Ffonttbl.Find(FontName, Result) then
  //  Exit;
  While I < Ffonttbl.Count do
    begin
      if (UTF8LowerCase(Ffonttbl[I]) = UTF8LowerCase(FontName)) then
        begin
          Result:= I;
          Exit;
        end;
      Inc(I);
    end;
  Result := Ffonttbl.Add(FontName);
end;

function TRTFSave.AddColor(Color: TColor): integer;
var
  R, G, B: byte;
  Par: string;

  I: Integer=0;
begin
  R := Red(Color);
  G := Green(Color);
  B := Blue(Color);
  Par := '\red' + IntToStr(R) + '\green' + IntToStr(G) + '\blue' + IntToStr(B);
  While I < FColortbl.Count do
    begin
      if (UTF8LowerCase(FColortbl[I]) = UTF8LowerCase(Par)) then
        begin
          Result:= I;
          Exit;
        end;
      Inc(I);
    end;
  Result := FColortbl.Add(Par);
end;

function TRTFSave.GetGTKTextBuffer(var TextBuffer: PGtkTextBuffer): boolean;
var
  AWidget: PGtkWidget;
begin
  Result := False;
  //--
  AWidget := PGtkWidget(FTextView.Handle);
  AWidget := GetWidgetInfo(AWidget, False)^.CoreWidget;
  if not (Assigned(AWidget)) then
    Exit;
  //--
  TextBuffer := gtk_text_view_get_buffer(PGtkTextView(AWidget));
  if not (Assigned(TextBuffer)) then
    Exit;
  //--
  Result := True;
end;

function TRTFSave.GetText(var S: string): boolean;
var
  iterStart, iterEnd: TGtkTextIter;
  Buffer: PGtkTextBuffer = nil;
begin
  Result := False;
  if not (GetGTKTextBuffer(Buffer)) then
    Exit;

  gtk_text_buffer_get_start_iter(Buffer, @iterStart);
  gtk_text_buffer_get_end_iter(Buffer, @iterEnd);
  S := gtk_text_buffer_get_slice(Buffer, @iterStart, @iterEnd, gboolean(False));

  Result := True;
end;

function TRTFSave.GetFontAttributes(const Position: integer;
  var FontAttributes: TRSFontAttributes): boolean;
var
  Buffer: PGtkTextBuffer = nil;
  Attributes: PGtkTextAttributes;
  iPosition: TGtkTextIter;
begin
  Result := False;
  //--
  if not (GetGTKTextBuffer(Buffer)) then
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
      Include(FontAttributes.Style, fsBold);
    if (pango_font_description_get_style(Attributes^.font) = PANGO_STYLE_ITALIC) then
      Include(FontAttributes.Style, fsItalic);
    //--
    Result := True;
  end;
  gtk_text_attributes_unref(Attributes);

end;

function TRTFSave.GetParaAttributes(const Position: integer;
  var ParaAttributes: TRSParaAttributes): boolean;
var
  Buffer: PGtkTextBuffer = nil;
  Attributes: PGtkTextAttributes;
  iPosition: TGtkTextIter;
begin
  Result := False;
  //--
  if not (GetGTKTextBuffer(Buffer)) then
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
    ParaAttributes.LeftIndent := Attributes^.left_margin div 37;
    ParaAttributes.RightIndent := Attributes^.right_margin div 37;
    ParaAttributes.FirstIndent := Attributes^.indent div 37;
    //--

    Result := True;
  end;
  gtk_text_attributes_unref(Attributes);
end;

function TRTFSave.Start: boolean;
var
  S: String='';
  TextLen, I: Integer;
  IChar: Integer=1;
  CharLen: Integer;
  UChar: TUTF8Char;
  Line: String;
  LineCount: Integer=0;
  CodChar: Cardinal;
  TmpCodChar: Cardinal;
  TmpChar: TUTF8Char;
  NPara:Boolean=True;
  ParaAttributes: TRSParaAttributes;
  FontAttributes: TRSFontAttributes;
  Space: Boolean=False;
  Picture: TPicture;
begin
  Result:= False;
  //--Pega o texto no controle
  if not(GetText(S)) then Exit;
  //--
  FRTFout.Clear;
  Picture:= TPicture.Create;
  //--
  TextLen:= UTF8Length(S);
  //--
  FRTFout.Text:= '\pard';
  //--
//  for IChar:=1 to  TextLen do
  while IChar <= TextLen do
  begin
      UChar:= UTF8Copy(S, IChar, 1);
      CodChar:= UTF8CharacterToUnicode(@UChar[1], CharLen);
      if CodChar = $0 then
      begin
        UChar:= '';
      end;

      //--
      //Novo Parágrafo
      if (CodChar=$A) then
        begin
          //--
            if (fsBold in  FFontAttributes.Style) then
            FRTFout[FRTFout.Count -1]:= FRTFout[FRTFout.Count -1] + '\b0';
            if (fsItalic in  FFontAttributes.Style) then
            FRTFout[FRTFout.Count -1]:= FRTFout[FRTFout.Count -1] + '\i0';
            if (fsUnderline in  FFontAttributes.Style) then
            FRTFout[FRTFout.Count -1]:= FRTFout[FRTFout.Count -1] + '\ulnone';
            //--
            FFontAttributes.Style:= [];
          //--
          FRTFout[FRTFout.Count -1]:= FRTFout[FRTFout.Count -1] + '\par';
          FRTFout.Add('\pard');
          //--
          LineCount:= FRTFout.Count -1;
          //--
          NPara:= True;
        end;
      //---
      if (NPara) then
        begin
          //Resolve todas as questões do parágrafo
          //
          //-- Inicializa atributos de parágrafo
          ParaAttributes.LeftIndent:= 0;
          ParaAttributes.RightIndent:= 0;
          ParaAttributes.FirstIndent:= 0;
          ParaAttributes.Alignment:= taLeftJustify;

          //Pega as propriedades do parágrafo
          if (IChar > 1) then
            GetParaAttributes(IChar + 1, ParaAttributes)
          else
            GetParaAttributes(1, ParaAttributes);

          //Coloca o alinhamento
          case ParaAttributes.Alignment of
               taRightJustify: FRTFout[LineCount]:= FRTFout[LineCount] + '\qr';
               taCenter: FRTFout[LineCount]:= FRTFout[LineCount] + '\qc';
          end;


          //Verifica se é um marcador
          if (TextLen > IChar) and (UTF8Copy(S, IChar + 1, 1) = UnicodeToUTF8(UnicodeBulletCode)) or
             (IChar=1) and (UTF8Copy(S, 1, 1) = UnicodeToUTF8(UnicodeBulletCode)) then
            begin
               //Insere o código para marcador em RTF
              //FRTFout[LineCount]:= FRTFout[LineCount] + '{\pntext\f' + IntToStr(AddFont('Symbol')) + '\''B7\tab}{\*\pn\pnlvlblt\pnf' + IntToStr(AddFont('Symbol')) + '\pnindent0{\pntxtb\''B7}}';
              {if ParaAttributes.LeftIndent >0 then
                ParaAttributes.LeftIndent:= ParaAttributes.LeftIndent -1;}

              if (TextLen > IChar + 2) and (UTF8Copy(S, IChar + 2, 1) = UnicodeToUTF8($9))then
                begin
                  UTF8Delete(S, IChar + 2, 1);
                  UTF8Insert(UnicodeToUTF8($0), S, IChar + 2);
                end;

            end;

          //Parâmetros de identação
          if (ParaAttributes.LeftIndent > 0) then
          FRTFout[LineCount]:= FRTFout[LineCount] + '\li' + IntToStr(ParaAttributes.LeftIndent * 568);

          if (ParaAttributes.RightIndent > 0) then
          FRTFout[LineCount]:= FRTFout[LineCount] + '\ri' + IntToStr(ParaAttributes.RightIndent * 568);

          if (ParaAttributes.FirstIndent > 0) then
          FRTFout[LineCount]:= FRTFout[LineCount] + '\fi' + IntToStr(ParaAttributes.FirstIndent * 568);

          //Verifica se é um marcador
          if (TextLen > IChar) and (UTF8Copy(S, IChar + 1, 1) = UnicodeToUTF8(UnicodeBulletCode)) or
             (IChar=1) and (UTF8Copy(S, 1, 1) = UnicodeToUTF8(UnicodeBulletCode)) then
            begin
               //Insere o código para marcador em RTF
              FRTFout[LineCount]:= FRTFout[LineCount] + '{\pntext\f' + IntToStr(AddFont('Symbol')) + '\''B7\tab}{\*\pn\pnlvlblt\pnf' + IntToStr(AddFont('Symbol')) + '\pnindent0{\pntxtb\''B7}}';
            end;
           //--
          //NPara:= False;
        end;
      //--
      //Propriedades do Texto

      //--
      FontAttributes.Name:='Sans';
      FontAttributes.Size:=10;
      FontAttributes.Style:=[];
      FontAttributes.Color:=clWindowText;
      //--
      GetFontAttributes(IChar -1,  FontAttributes);
      //--
      if (FontAttributes.Name <> FFontAttributes.Name) or (NPara) then
        begin
          FRTFout[LineCount]:= FRTFout[LineCount] + '\f' + IntToStr(AddFont(FontAttributes.Name));
          FFontAttributes.Name := FontAttributes.Name;
          Space := True;
        end;
      if (FontAttributes.Size <> FFontAttributes.Size) or (NPara) then
        begin
          FRTFout[LineCount]:= FRTFout[LineCount] + '\fs' + IntToStr(FontAttributes.Size * 2);
          FFontAttributes.Size := FontAttributes.Size;
          Space := True;
        end;
       if (FontAttributes.Color <> FFontAttributes.Color) or (NPara) then
        begin
          FRTFout[LineCount]:= FRTFout[LineCount] + '\cf' + IntToStr(AddColor(FontAttributes.Color));
          FFontAttributes.Color := FontAttributes.Color;
          Space := True;
        end;
       //------
       if (fsBold in FontAttributes.Style) and not(fsBold in FFontAttributes.Style) then
         FRTFout[LineCount]:= FRTFout[LineCount] + '\b'
       else if (fsBold in FFontAttributes.Style) and not(fsBold in FontAttributes.Style) then
         FRTFout[LineCount]:= FRTFout[LineCount] + '\b0';
       //--
       if (fsItalic in FontAttributes.Style) and not(fsItalic in FFontAttributes.Style) then
         FRTFout[LineCount]:= FRTFout[LineCount] + '\i'
       else if (fsItalic in FFontAttributes.Style) and not(fsItalic in FontAttributes.Style) then
         FRTFout[LineCount]:= FRTFout[LineCount] + '\i0';
       //--
       if (fsUnderline in FontAttributes.Style) and not(fsUnderline in FFontAttributes.Style) then
         FRTFout[LineCount]:= FRTFout[LineCount] + '\ul'
       else if (fsUnderline in FFontAttributes.Style) and not(fsUnderline in FontAttributes.Style) then
         FRTFout[LineCount]:= FRTFout[LineCount] + '\ulnone';
       //--
       if FFontAttributes.Style <> FontAttributes.Style then Space:= true;
       FFontAttributes.Style := FontAttributes.Style;
       //-------
       if (CodChar= UnicodeBulletCode) or (CodChar= $A) then
        begin
          UChar:='';
          CodChar:=$0;
        end;
        //---
       //Imagens
       if CodChar = $FFFC then
        begin
          Picture.Clear;
          GetImage(Picture, IChar -1);
          FRTFout[LineCount]:= FRTFout[LineCount] + PNGToRTF(Picture.PNG);
          UChar:= '';
          CodChar:=$0;
        end;
       //--
       //Tratamento de caracteres
       //TAB
       if CodChar = $9 then
       begin
         FRTFout[LineCount]:= FRTFout[LineCount] + '\tab';
          UChar:= '';
          CodChar:=$0;
         Space := True;
       end;
       //--
       //--Caracteres com código maior que 160
       if (CodChar > $A0) then
       begin
         FRTFout[LineCount]:= FRTFout[LineCount] + '\''' + UTF8LowerCase(IntToHex(CodChar, 2));
          UChar:= '';
          CodChar:=$0;
       end;
       //Simbolos especiais
       if (UChar = '\') or (UChar = '{') or (UChar = '}') then
         FRTFout[LineCount]:= FRTFout[LineCount] + '\';
       //--
       if Space then
        begin
          Space:= False;
          FRTFout[LineCount]:= FRTFout[LineCount] + ' ';
        end;
       //--

       FRTFout[LineCount]:= FRTFout[LineCount] + UChar;
    NPara:= False;
    Inc(IChar);
    end;

  //Finaliza paragrafo
  FRTFout[LineCount]:= FRTFout[LineCount] + '\par';
//-------
 //Finaliza a montágem do arquivo
 Line := '{\rtf1\ansi\deff0\adeflang1025{\fonttbl ';

  {for I := 0 to (Ffonttbl.Count - 1) do
  begin
    Line := Line + '{\f' + IntToStr(I) + '\fswiss ' + Ffonttbl[I] + ';}';
  end;}
  I:=0;
  While I < Ffonttbl.Count do
  begin
  Line := Line + '{\f' + IntToStr(I) + '\fswiss ' + Ffonttbl[I] + ';}';
  Inc(I);
  end;

  Line := Line + '}';

  Line := Line + UnicodeToUTF8($A);

  if (Fcolortbl.Count > 1) then
  begin
    Line := Line + '{\colortbl ';
    {for I := 1 to (Fcolortbl.Count - 1) do
    begin
      Line := Line + ';' + Fcolortbl[I];
    end;}
    I:=1;
    While I < Fcolortbl.Count do
    begin
      Line := Line + ';' + Fcolortbl[I];
      Inc(I);
    end;
    Line := Line + ';}' + UnicodeToUTF8($A);
  end;

  Line := Line + '{\*\generator RTFTool 1.0;}\viewkind4';
  //--
  FRTFout.Insert(0, Line);
  FRTFout.Add('}');
  Picture.Free;
  Result:= True;
end;

function TRTFSave.GetImage(Picture: TPicture; Position: integer): boolean;
var
  Buffer: PGtkTextBuffer = nil;
  iPosition: TGtkTextIter;
  pixbuf: PGDKPixBuf = nil;
  Width, Height, rowstride, n_channels, i, j: integer;
  //Alpha:Boolean =  false;
  pixels: Pguchar = nil;
  p: Pguchar = nil;
  FastImage: TFastImage;
begin

  //--
  Result := False;
  //--
  if not (GetGTKTextBuffer(Buffer)) then
    Exit;

  //--
  gtk_text_buffer_get_iter_at_offset(buffer, @iPosition, Position);

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
  //Alpha :=   gdk_pixbuf_get_has_alpha(pixbuf);
  //--

  try

    FastImage := TFastImage.Create;
    FastImage.SetSize(Width, Height);

    {for i := 0 to Width - 1 do
      for J := 0 to Height - 1 do
      begin
        p := pixels + j * rowstride + i * n_channels;
        if n_channels = 4 then
          //FastImage.Color[i + 1, j + 1]:= RGBAToFastPixel(Ord((p + 2)^), Ord((p + 1)^), Ord((p + 0)^), Ord((P + 3)^))
          FastImage.Color[i + 1, j + 1] :=
            RGBAToFastPixel(Ord((p + 0)^), Ord((p + 1)^), Ord((p + 2)^), Ord((P + 3)^))


        else
          //FastImage.Color[i + 1, j + 1] := RGBAToFastPixel(Ord((p + 2)^), Ord((p + 1)^), Ord((p + 0)^), $FF);
        FastImage.Color[i + 1, j + 1] :=
          RGBAToFastPixel(Ord((p + 0)^), Ord((p + 1)^), Ord((p + 2)^), $FF);

      end;}

      I:=0;
      J:=0;

      While I < Width do
      begin
        While J < Height do
        //for J := 0 to Height - 1 do
        begin
          p := pixels + j * rowstride + i * n_channels;
          if n_channels = 4 then
            //FastImage.Color[i + 1, j + 1]:= RGBAToFastPixel(Ord((p + 2)^), Ord((p + 1)^), Ord((p + 0)^), Ord((P + 3)^))
            FastImage.Color[i + 1, j + 1] :=
              RGBAToFastPixel(Ord((p + 0)^), Ord((p + 1)^), Ord((p + 2)^), Ord((P + 3)^))
          else
            //FastImage.Color[i + 1, j + 1] := RGBAToFastPixel(Ord((p + 2)^), Ord((p + 1)^), Ord((p + 0)^), $FF);
          FastImage.Color[i + 1, j + 1] :=
            RGBAToFastPixel(Ord((p + 0)^), Ord((p + 1)^), Ord((p + 2)^), $FF);

          Inc(J);
        end;
        J:=0;
        Inc(I);
      end;

      FastImage.FastImageToPicture(Picture);
  finally
    FastImage.Free;

  end;

  Result := True;

end;

procedure TRTFSave.SaveToStream(Stream: TStream);
begin
  Start;
  FRTFout.SaveToStream(Stream);
end;

constructor TRTFSave.Create(TextView: TWinControl);
begin
  inherited Create;
  Ffonttbl := TStringList.Create;
  Fcolortbl := TStringList.Create;
  FRTFout := TStringList.Create;
  FTextView := TextView;
end;

destructor TRTFSave.Destroy;
begin
  FRTFout.Free;
  Ffonttbl.Free;
  Fcolortbl.Free;
  inherited Destroy;
end;

end.

