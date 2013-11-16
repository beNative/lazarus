unit RTF2HTML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RTFPars_lzRichEdit, LCLType, LCLProc, Dialogs, Graphics;

type

  TRTFPict = record
    PictType,
    W,
    H,
    WG,
    HG: integer;
    HEX: string;
  end;

  TRSFontAttributes=record
    Charset: TFontCharset;
    Color: TColor;
    Name: TFontName;
    Pitch: TFontPitch;
    fProtected: Boolean;
    Size: Integer;
    Style: TFontStyles;
  end;
{ TRtf2HTML }

TRtf2HTML = class(TObject)
private
FRTFParser: TRTFParser;
FRTFPict: TRTFPict;
FIsPict: boolean;
FGroups: integer;
FSkipGroup:Integer;
FAlign: TAlignment;
FFirstIndent: Integer;
FLeftIndent: Integer;
FRightIndent: Integer;
FFontParams: TRSFontAttributes;
FLastFontParams: TRSFontAttributes;
FBullet:Boolean;
//--
FSpan:String;
FCloseSPan:boolean;
FParZero:Boolean;
//--
FDirectory:String;
FFileNum:Integer;
private
procedure DoGroup;
procedure DoWrite;
procedure DoCtrl;
procedure DoSpecialChar;
procedure DoParAttr;
procedure DoCharAttr;
procedure DoPictAttr;
procedure DoBeginPict;
procedure DoEndPict;
public
  SunFontSize:Integer;
private
FHTML:String;
FPar: String;
public
function Convert(RTFStream: TStream; DirectoryHTML: String; HTMLFileName:String): Boolean;
end;

function SaveMedia(const S: string; DataType: integer; FileName:String): boolean;


implementation

function SaveMedia(const S: string; DataType: integer; FileName:String): boolean;
var
    MStream: TMemoryStream;
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


  MStream.SaveToFile(FileName);

    MStream.Free;

    Result:= true;
end;


{ TRtf2HTML }

procedure TRtf2HTML.DoGroup;
begin
  if (FRTFParser.RTFMajor = rtfBeginGroup) then
    FGroups := FGroups + 1
  else
    FGroups := FGroups - 1;
  if (FGroups < FSkipGroup) then
    FSkipGroup := -1;
end;

procedure TRtf2HTML.DoWrite;
var
  C: TUTF8char;
  L: integer;
begin
  C := UnicodeToUTF8(FRTFParser.RTFMajor);
  if FIsPict then
    FRTFPict.HEX := FRTFPict.HEX + C
  else
  begin
    if (FSkipGroup = -1) and (FRTFParser.RTFMajor = 183) then
    begin
    FBullet:= True;
    DoCharAttr;
    C := chr($0);
    end;
    if (FSkipGroup = -1) and (C <> chr($0)) then
    begin
      if (FSpan <> '') then
      begin
        if (FCloseSPan) then FPar:= FPar + '</SPAN>';
        FPar:= FPar + FSpan;
        FCloseSPan:= True;
        FSpan:= '';
      end;
      FPar:= FPar + C;
      FParZero:= false;
    end;
  end;

end;

procedure TRtf2HTML.DoCtrl;
begin
  case FRTFParser.RTFMajor of
    rtfSpecialChar: DoSpecialChar;
    rtfParAttr: DoParAttr;
    rtfCharAttr: DoCharAttr;
    rtfPictAttr: DoPictAttr;
  end;
end;

procedure TRtf2HTML.DoSpecialChar;
const
  PAling: array [TAlignment] of String = ('style="text-align:left', 'style="text-align:right', 'style="text-align:center');

  function GenSpan:String;
  var
    face:String='';
    color: String='';
    size: String='';
  begin
      Result:= '<SPAN style="';
      //--
      if (FFontParams.Name <> '') then
        face:= 'font-family:' + FFontParams.Name + '; ';
      if (FFontParams.Size <> 0) then
        size:= 'font-size:' + IntToStr(FFontParams.Size + SunFontSize) + 'px; ';
       color:= 'color:#' +  IntToHex(Red(FFontParams.Color), 2) +
          IntToHex(green(FFontParams.Color), 2) +
          IntToHex(blue(FFontParams.Color), 2);

       Result:= Result + face + size + color + '">';

  end;

var
  InitIdent:String='';
begin
  case FRTFParser.rtfMinor of
    rtfPar:
    begin
      if (FSkipGroup = -1) then
        begin
        if (fsBold in FFontParams.Style) then
            FPar:= FPar + '</b>';

        if (fsItalic in FFontParams.Style) then
            FPar:= FPar + '</i>';

        if (fsUnderline in FFontParams.Style) then
            FPar:= FPar + '</u>';

        if (FCloseSPan) then
          FPar:= FPar + '</SPAN>';


        if not(FParZero) then
        begin
        if (FFirstIndent <> 0) then
          begin
            InitIdent:= '; text-indent:'+ IntToStr(FFirstIndent div 20) +'px';
          end;
        if (FLeftIndent <> 0) then
          begin
          InitIdent:= InitIdent + '; padding-left:'+ IntToStr(FLeftIndent div 20) +'px';
          end;
        if (FRightIndent <> 0) then
          begin
          InitIdent:= InitIdent + '; padding-right:'+ IntToStr(FRightIndent div 20) +'px';
          end;

        if FBullet then
          begin
            FPar:= '<ul><li>' + FPar + '</li></ul>';
            FBullet:= False;
          end;

        FHTML:= FHTML + '<P ' + PAling[FAlign] + InitIdent + '">' + FPar + '</P>' + #10;
          FPar:= '';
        end
        else
          begin
          FHTML:= FHTML + '<BR>' + #10;
          FPar:= '';
          end;
        //---

        if (fsBold in FFontParams.Style) then
            FPar:= FPar + '<b>';

        if (fsItalic in FFontParams.Style) then
            FPar:= FPar + '<i>';

        if (fsUnderline in FFontParams.Style) then
            FPar:= FPar + '<u>';

//--
        FSpan:= GenSpan;
        FPar:= FPar + FSpan;
        FSpan:= '';
        FParZero:= True;

        end;
    end;
    rtfTab:
    begin
      if (FSkipGroup = -1) then
        FPar:= FPar + '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;';
    end;
    rtfOptDest:
    begin
      if (FSkipGroup = -1) then;
        FSkipGroup := FGroups;
    end;
  end;
end;

procedure TRtf2HTML.DoParAttr;
begin
  case FRTFParser.rtfMinor of
    rtfParDef:
    begin
      FAlign := taLeftJustify;
      FLeftIndent := 0;
      FRightIndent := 0;
      FFirstIndent:= 0;
    end;
    rtfQuadLeft:
    begin
      FAlign := taLeftJustify;
    end;
    rtfQuadRight:
    begin
      FAlign := taRightJustify;
    end;
    rtfQuadJust:
    begin
      FAlign := taLeftJustify;
    end;
    rtfQuadCenter:
    begin
      FAlign := taCenter;
    end;
    rtfFirstIndent:
    begin
      FFirstIndent:= FRTFParser.rtfParam;
    end;
    rtfLeftIndent:
    begin
      FLeftIndent := FRTFParser.rtfParam;
    end;
    rtfRightIndent:
    begin
      FRightIndent := FRTFParser.rtfParam;
    end;
  end;

end;

procedure TRtf2HTML.DoCharAttr;
  var
    c: PRTFColor;
    f: PRTFFont;

    function Styles(S: TFontStyle): boolean;
    begin
      if (s in FFontParams.Style) then
        FFontParams.Style := FFontParams.Style - [S]
      else
        FFontParams.Style := FFontParams.Style + [S];

      Result := (S in FFontParams.Style);
    end;

  function GenSpan:String;
  var
    face:String='';
    color: String='';
    size: String='';
  begin
      Result:= '<SPAN style="';
      //--
      if (FFontParams.Name <> '') then
        face:= 'font-family:' + FFontParams.Name + '; ';
      if (FFontParams.Size <> 0) then
        size:= 'font-size:' + IntToStr(FFontParams.Size + SunFontSize) + 'px; ';
       color:= 'color:#' +  IntToHex(Red(FFontParams.Color), 2) +
          IntToHex(green(FFontParams.Color), 2) +
          IntToHex(blue(FFontParams.Color), 2);

       Result:= Result + face + size + color + '">';
  end;
begin
    FLastFontParams.Name:= FFontParams.Name;
    FLastFontParams.Size:= FFontParams.Size;
    FLastFontParams.Color:= FFontParams.Color;
    FLastFontParams.Style:= FFontParams.Style;
    //--
    case FRTFParser.rtfMinor of
      rtfBold:
      begin
        Styles(fsBold);
      end;
      rtfItalic:
      begin
        Styles(fsItalic);
      end;
      rtfStrikeThru:
      begin
        Styles(fsStrikeOut);
      end;
      rtfFontNum:
      begin
        f := FRTFParser.Fonts[FRTFParser.rtfParam];
        if (f = nil) then
          FFontParams.Name := 'Sans'
        else
          FFontParams.Name := f^.rtfFName;
      end;
      rtfFontSize:
      begin
        FFontParams.Size := FRTFParser.rtfParam div 2;
      end;
      rtfUnderline:
      begin
        if not(fsUnderline in FFontParams.Style) then
          FFontParams.Style := FFontParams.Style + [fsUnderline]
      end;
      rtfNoUnderline:
      begin
        if (fsUnderline in FFontParams.Style) then
          FFontParams.Style := FFontParams.Style - [fsUnderline]
      end;
      rtfForeColor:
      begin
        C := FRTFParser.Colors[FRTFParser.rtfParam];
        if (C = nil) or (FRTFParser.rtfParam = 0) then
          FFontParams.Color := clWindowText
        else
          FFontParams.Color := RGBToColor(C^.rtfCRed, C^.rtfCGreen, C^.rtfCBlue);
      end;
    end;

    if (FLastFontParams.Style <> FFontParams.Style) then
      begin
        if (fsBold in FFontParams.Style) and
          not(fsBold in FLastFontParams.Style) then
            FPar:= FPar + '<b>'
        else if not(fsBold in FFontParams.Style) and
          (fsBold in FLastFontParams.Style) then
            FPar:= FPar + '</b>';

        if (fsItalic in FFontParams.Style) and
          not(fsItalic in FLastFontParams.Style) then
            FPar:= FPar + '<i>'
        else if not(fsItalic in FFontParams.Style) and
          (fsItalic in FLastFontParams.Style) then
            FPar:= FPar + '</i>';

        if (fsUnderline in FFontParams.Style) and
          not(fsUnderline in FLastFontParams.Style) then
            FPar:= FPar + '<u>'
        else if not(fsUnderline in FFontParams.Style) and
          (fsUnderline in FLastFontParams.Style) then
            FPar:= FPar + '</u>';
      end;

    FSpan:= GenSpan;
end;

procedure TRtf2HTML.DoPictAttr;
begin
  if (FRTFParser.rtfMajor = rtfPictAttr) and (FRTFParser.rtfMinor in
    [rtfMacQD .. rtfpngblip]) then
    case FRTFParser.rtfMinor of
      rtfPicWid: FRTFPict.W := FRTFParser.rtfParam;
      rtfPicHt: FRTFPict.H := FRTFParser.rtfParam;
      rtfPicGoalWid: FRTFPict.WG := FRTFParser.rtfParam;
      rtfPicGoalHt: FRTFPict.HG := FRTFParser.rtfParam;
      rtfpngblip: FRTFPict.PictType := rtfpngblip;
      rtfWinMetafile: FRTFPict.PictType := rtfWinMetafile;

    end;

end;

procedure TRtf2HTML.DoBeginPict;
begin
  FRTFPict.HEX := '';
  FRTFPict.H := 0;
  FRTFPict.HG := 0;
  FRTFPict.W := 0;
  FRTFPict.WG := 0;
  FRTFPict.PictType := -1;
  FIsPict := True;
end;

procedure TRtf2HTML.DoEndPict;
var
  R: boolean = False;
  L:Integer;
  FileName:String;
  PFileType: array [0..18] of String = ('', '.wmf', '.bmp', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '.png');

begin
  FIsPict := False;
  FileName:=  FDirectory + 'img' + IntToStr(FFileNum) + PFileType[FRTFPict.PictType];

  if (SaveMedia(FRTFPict.HEX, FRTFPict.PictType, FileName)) then
    Inc(FFileNum);

  if (FRTFPict.WG = 0) and (FRTFPict.HG = 0) or (FRTFPict.WG = FRTFPict.W) and
    (FRTFPict.HG = FRTFPict.H) then
      FPar:= FPar + '<img src="' + ExtractFileName(FileName) + '">'
  else
    FPar:= FPar + '<img src="' + ExtractFileName(FileName) + '" style=width:' + IntToStr(FRTFPict.WG) + 'px; height:' + IntToStr(FRTFPict.HG) + 'px">';

FParZero:= false;
end;

function TRtf2HTML.Convert(RTFStream: TStream; DirectoryHTML: String; HTMLFileName:String): Boolean;
var
  FileName:String;
  HTML: TStringList;
begin
  Result:= False;
  if not(DirectoryExists(DirectoryHTML)) then Exit;
  //--
  if (RightStr(DirectoryHTML, 1) <> DirectorySeparator) then
    begin
      FileName:= DirectoryHTML + DirectorySeparator + HTMLFileName;
      FDirectory:= DirectoryHTML + DirectorySeparator;
    end
  else
    begin
      FileName:= DirectoryHTML + HTMLFileName;
      FDirectory:= DirectoryHTML;
    end;
//
  FFontParams.Name:='';
  FFontParams.Size:=0;
  FFontParams.Color:=$000000;
  FFontParams.Style:=[];
//
  FPar:= '';
  FFileNum:= 0;
//--
  FCloseSPan:= False;
  FSpan:= '';
  FParZero:= true;
  FBullet:= False;
  //--
  FHTML:= '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"' + #10 +
          '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">' + #10 +
          '<html>' + #10 +
          '<head><meta http-equiv="Content-Type" content="text/html;charset=UTF-8" /></head>' + #10 +
          '<body>' + #10;
  //--

  HTML:= TStringList.Create;
  HTML.Clear;
  //--
  FGroups := 0;
  FSkipGroup := -1;
  //--
  FRTFParser := TRTFParser.Create(RTFStream);
  FRTFParser.classcallbacks[rtfText] := @DoWrite;
  FRTFParser.classcallbacks[rtfcontrol] := @DoCtrl;
  FRTFParser.classcallbacks[rtfGroup] := @DoGroup;
  FRTFParser.OnRTFBeginPict := @DoBeginPict;
  FRTFParser.OnRTFEndPict := @DoEndPict;
  FRTFParser.StartReading;
  FRTFParser.Free;
  //--
  FHTML := FHTML + #10 +'</body>' + #10 +
'</html>';
  //--
  HTML.Text:= FHTML;
  HTML.SaveToFile(FileName);
  HTML.Free;

  Result:= True;
end;

end.
