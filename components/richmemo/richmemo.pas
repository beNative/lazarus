{
 richmemo.pas
 
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

unit RichMemo; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, LazUTF8;

type
  TFontParams  = record
    Name      : String;
    Size      : Integer;
    Color     : TColor;
    Style     : TFontStyles;
    HasBkClr  : Boolean;
    BkColor   : TColor;
  end;

  TParaAlignment  = (paLeft, paRight, paCenter, paJustify);
  TParaMetric = record
    FirstLine   : Double; // in points
    TailIndent  : Double; // in points
    HeadIndent  : Double; // in points
    SpaceBefore : Double; // in points
    SpaceAfter  : Double; // in points
    LineSpacing : Double; // multiplier - matching CSS line-height by percentage/em
                          // note, that normal LineSpacing is 1.2, not 1.0
  end;

const
  DefLineSpacing     = 1.2;
  SingleLineSpacing  = DefLineSpacing;
  OneHalfLineSpacing = DefLineSpacing * 1.5;
  DoubleLineSpacing  = DefLineSpacing * 2.0;


type
  TParaNumStyle   = (pnNone, pnBullet, pnNumber, pnLowLetter
    , pnLowRoman, pnUpLetter, pnUpRoman, pnCustomChar);

  TParaNumbering  = record
    Numbering   : TParaNumStyle;
    NumCustom   : WideChar;
    NumIndent   : Double;
  end;

  TTextModifyMask  = set of (tmm_Color, tmm_Name, tmm_Size, tmm_Styles, tmm_BackColor);
  TParaModifyMask = set of (pmm_FirstLine, pmm_HeadIndent, pmm_TailIndent, pmm_SpaceBefore, pmm_SpaceAfter, pmm_LineSpacing);

  TSearchOption = (soMatchCase, soWholeWord, soBackward);
  TSearchOptions = set of TSearchOption;

  TParaRange = record
    start      : Integer; // the first character in the paragraph
    lenghtNoBr : Integer; // the length of the paragraph, excluding the line break character
    length     : Integer; // the length of the paragrpah, including the line break, if present
    // the last line in the control doesn't contain a line break character,
    // thus length = lengthNoBr
  end;


type
  TRichMemoObject = class(TObject);

  { TCustomRichMemo }

  TCustomRichMemo = class(TCustomMemo)
  private
    fHideSelection  : Boolean;
    fOnSelectionChange: TNotifyEvent;
    fZoomFactor : Double;
  protected
    procedure DoSelectionChange;
    class procedure WSRegisterClass; override;
    procedure CreateWnd; override;    
    procedure UpdateRichMemo; virtual;
    procedure SetHideSelection(AValue: Boolean);
    function GetContStyleLength(TextStart: Integer): Integer;
    
    procedure SetSelText(const SelTextUTF8: string); override;
    function GetZoomFactor: Double; virtual;
    procedure SetZoomFactor(AValue: Double); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CopyToClipboard; override;
    procedure CutToClipboard; override;
    procedure PasteFromClipboard; override;
    
    procedure SetTextAttributes(TextStart, TextLen: Integer; const TextParams: TFontParams); virtual;
    function GetTextAttributes(TextStart: Integer; var TextParams: TFontParams): Boolean; virtual;
    function GetStyleRange(CharOfs: Integer; var RangeStart, RangeLen: Integer): Boolean; virtual;

    function GetParaAlignment(TextStart: Integer; var AAlign: TParaAlignment): Boolean; overload; virtual;
    function GetParaAlignment(TextStart: Integer): TParaAlignment; overload;
    procedure SetParaAlignment(TextStart, TextLen: Integer; AAlign: TParaAlignment); virtual;
    function GetParaMetric(TextStart: Integer; var AMetric: TParaMetric): Boolean; virtual;
    procedure SetParaMetric(TextStart, TextLen: Integer; const AMetric: TParaMetric); virtual;
    function GetParaNumbering(TextStart: Integer; var ANumber: TParaNumbering): Boolean; virtual;
    procedure SetParaNumbering(TextStart, TextLen: Integer; const ANumber: TParaNumbering); virtual;
    function GetParaRange(CharOfs: Integer; var ParaRange: TParaRange): Boolean; virtual;
    function GetParaRange(CharOfs: Integer; var TextStart, TextLength: Integer): Boolean;

    procedure SetTextAttributes(TextStart, TextLen: Integer; AFont: TFont);
    procedure SetRangeColor(TextStart, TextLength: Integer; FontColor: TColor);
    procedure SetRangeParams(TextStart, TextLength: Integer; ModifyMask: TTextModifyMask;
      const FontName: String; FontSize: Integer; FontColor: TColor; AddFontStyle, RemoveFontStyle: TFontStyles); overload;
    procedure SetRangeParams(TextStart, TextLength: Integer; ModifyMask: TTextModifyMask;
      const fnt: TFontParams; AddFontStyle, RemoveFontStyle: TFontStyles); overload;
    procedure SetRangeParaParams(TextStart, TextLength: INteger; ModifyMask: TParaModifyMask;
      const ParaMetric: TParaMetric);

    function LoadRichText(Source: TStream): Boolean; virtual;
    function SaveRichText(Dest: TStream): Boolean; virtual;

    function InDelText(const UTF8Text: string; InsStartChar, ReplaceLength: Integer): Integer; virtual;

    procedure SetSelLengthFor(const aselstr: string);

    function Search(const ANiddle: string; Start, Len: Integer; const SearchOpt: TSearchOptions): Integer;

    property HideSelection : Boolean read fHideSelection write SetHideSelection;
    property OnSelectionChange: TNotifyEvent read fOnSelectionChange write fOnSelectionChange;
    property ZoomFactor: Double read GetZoomFactor write SetZoomFactor;
  end;
  
  { TRichMemo }

  TRichMemo = class(TCustomRichMemo)
  protected
    // this is "design-time" property
    fRtf: string; // initial RichText
    function GetRTF: string; virtual;
    procedure SetRTF(const AValue: string); virtual;
    procedure UpdateRichMemo; override;
  published
    property Align;
    property Alignment;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property Lines;
    property MaxLength;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnSelectionChange;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ParentShowHint;
    property ReadOnly;
    property Rtf: string read GetRTF write SetRTF;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;
    property ZoomFactor;
  end;

procedure InitFontParams(var p: TFontParams);
function GetFontParams(styles: TFontStyles): TFontParams; overload;
function GetFontParams(color: TColor; styles: TFontStyles): TFontParams; overload;
function GetFontParams(const Name: String; color: TColor; styles: TFontStyles): TFontParams; overload;
function GetFontParams(const Name: String; Size: Integer; color: TColor; styles: TFontStyles): TFontParams; overload;

procedure InitParaMetric(var m: TParaMetric);
procedure InitParaNumbering(var n: TParaNumbering);

var
  RTFLoadStream : function (AMemo: TCustomRichMemo; Source: TStream): Boolean = nil;
  RTFSaveStream : function (AMemo: TCustomRichMemo; Dest: TStream): Boolean = nil;

implementation

uses
  WSRichMemo;

procedure InitFontParams(var p: TFontParams);
begin
  FillChar(p, SizeOf(p), 0);
end;

function GetFontParams(styles: TFontStyles): TFontParams; overload;
begin 
  Result := GetFontParams('', 0, 0, styles);
end;

function GetFontParams(color: TColor; styles: TFontStyles): TFontParams; overload;
begin
  Result := GetFontParams('', 0, color, styles);
end;

function GetFontParams(const Name: String; color: TColor; styles: TFontStyles): TFontParams; overload;
begin
  Result := GetFontParams(Name, 0, color, styles);
end;

function GetFontParams(const Name: String; Size: Integer; color: TColor; styles: TFontStyles): TFontParams; overload;
begin
  InitFontParams(Result);
  Result.Name := Name;
  Result.Size := Size;
  Result.Color := color;
  Result.Style := styles;
end;

procedure InitParaMetric(var m: TParaMetric);
begin
  FillChar(m, sizeof(m), 0);
  m.LineSpacing:=DefLineSpacing;
end;

procedure InitParaNumbering(var n: TParaNumbering);
begin
  FillChar(n, sizeof(n), 0);
end;

{ TRichMemo }

function TRichMemo.GetRTF: string;
var
  st : TStringStream;
begin
  if (csDesigning in ComponentState) or not HandleAllocated then
    Result:=fRTF
  else begin
    try
      st := TStringStream.Create('');
      try
        SaveRichText(st);
        Result:=st.DataString;
      finally
        st.Free;
      end;
    except
      Result:='';
    end;
  end;
end;

procedure TRichMemo.SetRTF(const AValue: string);
var
  st : TStringStream;
begin
  if (csDesigning in ComponentState) or not HandleAllocated then
    fRTF:=AValue;

  if HandleAllocated then
    try
      st := TStringStream.Create(AValue);
      try
        LoadRichText(st);
      finally
        st.Free;
      end;
    except
    end;

  if ([csDesigning, csLoading] * ComponentState = []) and HandleAllocated then begin
    fRTF:=''; // reduce memory usage in run-time
  end;
end;

procedure TRichMemo.UpdateRichMemo;
begin
  inherited UpdateRichMemo;
  // if fRTF is blank, Text property would be used
  if fRTF<>'' then SetRTF(fRTF);
end;

{ TCustomRichMemo }

procedure TCustomRichMemo.SetHideSelection(AValue: Boolean);
begin
  if HandleAllocated then 
    TWSCustomRichMemoClass(WidgetSetClass).SetHideSelection(Self, AValue);
  fHideSelection := AValue;   
end;

function TCustomRichMemo.GetZoomFactor: Double;
begin
  Result:=fZoomFactor;
end;

procedure TCustomRichMemo.SetZoomFactor(AValue: Double);
begin
  if AValue=0 then AValue:=1.0;
  fZoomFactor:=AValue;
  if HandleAllocated then
    TWSCustomRichMemoClass(WidgetSetClass).SetZoomFactor(Self, AValue);
end;

procedure TCustomRichMemo.DoSelectionChange;
begin
  if Assigned(fOnSelectionChange) then fOnSelectionChange(Self);
end;

class procedure TCustomRichMemo.WSRegisterClass;  
begin
  inherited;
  WSRegisterCustomRichMemo;
end;

procedure TCustomRichMemo.CreateWnd;  
begin
  inherited CreateWnd;  
  UpdateRichMemo;
end;

procedure TCustomRichMemo.UpdateRichMemo; 
begin
  if not HandleAllocated then Exit;
  TWSCustomRichMemoClass(WidgetSetClass).SetHideSelection(Self, fHideSelection);
  TWSCustomRichMemoClass(WidgetSetClass).SetZoomFactor(Self, fZoomFactor);
end;

procedure TCustomRichMemo.SetTextAttributes(TextStart, TextLen: Integer;  
  AFont: TFont); 
var
  params  : TFontParams;
begin
  params.Name := AFont.Name;
  params.Color := AFont.Color;
  params.Size := AFont.Size;
  params.Style := AFont.Style;
  SetTextAttributes(TextStart, TextLen, {TextStyleAll,} params);
end;

procedure TCustomRichMemo.SetTextAttributes(TextStart, TextLen: Integer;  
  {SetMask: TTextStyleMask;} const TextParams: TFontParams);
begin
  if not HandleAllocated then HandleNeeded;
  if HandleAllocated then
    TWSCustomRichMemoClass(WidgetSetClass).SetTextAttributes(Self, TextStart, TextLen, {SetMask,} TextParams);
end;

function TCustomRichMemo.GetTextAttributes(TextStart: Integer; var TextParams: TFontParams): Boolean; 
begin
  if not HandleAllocated then HandleNeeded;
  if HandleAllocated then
    Result := TWSCustomRichMemoClass(WidgetSetClass).GetTextAttributes(Self, TextStart, TextParams)
  else
    Result := false;
end;

function TCustomRichMemo.GetStyleRange(CharOfs: Integer; var RangeStart,
  RangeLen: Integer): Boolean;
begin
  if HandleAllocated then begin
    Result := TWSCustomRichMemoClass(WidgetSetClass).GetStyleRange(Self, CharOfs, RangeStart, RangeLen);
    if Result and (RangeLen = 0) then RangeLen := 1;
  end else begin
    RangeStart := -1;
    RangeLen := -1;
    Result := false;
  end;
end;

function TCustomRichMemo.GetParaAlignment(TextStart: Integer;
  var AAlign: TParaAlignment): Boolean;
begin
  Result := HandleAllocated and
    TWSCustomRichMemoClass(WidgetSetClass).GetParaAlignment(Self, TextStart, AAlign);
end;

function TCustomRichMemo.GetParaAlignment(TextStart: Integer): TParaAlignment;
begin
  GetParaAlignment(TextStart, Result);
end;

procedure TCustomRichMemo.SetParaAlignment(TextStart, TextLen: Integer;
  AAlign: TParaAlignment);
begin
  if HandleAllocated then
    TWSCustomRichMemoClass(WidgetSetClass).SetParaAlignment(Self, TextStart, TextLen, AAlign);
end;

function TCustomRichMemo.GetParaMetric(TextStart: Integer;
  var AMetric: TParaMetric): Boolean;
begin
  if HandleAllocated then
    Result := TWSCustomRichMemoClass(WidgetSetClass).GetParaMetric(Self, TextStart, AMetric)
  else
    Result := false;
end;

procedure TCustomRichMemo.SetParaMetric(TextStart, TextLen: Integer;
  const AMetric: TParaMetric);
begin
  if HandleAllocated then
    TWSCustomRichMemoClass(WidgetSetClass).SetParaMetric(Self, TextStart, TextLen, AMetric);
end;

function TCustomRichMemo.GetParaNumbering(TextStart: Integer;
  var ANumber: TParaNumbering): Boolean;
begin
  if HandleAllocated then
    Result := TWSCustomRichMemoClass(WidgetSetClass).GetParaNumbering(Self, TextStart, ANumber)
  else
    Result := false;
end;

procedure TCustomRichMemo.SetParaNumbering(TextStart, TextLen: Integer;
  const ANumber: TParaNumbering);
begin
  if HandleAllocated then
    TWSCustomRichMemoClass(WidgetSetClass).SetParaNumbering(Self, TextStart, TextLen, ANumber);
end;

function TCustomRichMemo.GetParaRange(CharOfs: Integer;
  var ParaRange: TParaRange): Boolean;
begin
  Result:=false;
  if not HandleAllocated then HandleNeeded;
  if HandleAllocated then
    Result:=TWSCustomRichMemoClass(WidgetSetClass).GetParaRange(Self, CharOfs, ParaRange);
end;

function TCustomRichMemo.GetParaRange(CharOfs: Integer; var TextStart,
  TextLength: Integer): Boolean;
var
  p : TParaRange;
begin
  Result:=GetParaRange(CharOfs, p);
  TextStart:=p.start;
  TextLength:=p.length;
end;

function TCustomRichMemo.GetContStyleLength(TextStart: Integer): Integer;
var
  ofs, len  : Integer;
begin
  if GetStyleRange(TextStart, ofs, len)
    then Result := len - (TextStart-ofs)
    else Result := 0;
end;

procedure TCustomRichMemo.SetSelText(const SelTextUTF8: string);  
var
  st  : Integer;
begin
  if not HandleAllocated then HandleNeeded;
  Lines.BeginUpdate;
  try
    st := SelStart;
    if HandleAllocated then  
      TWSCustomRichMemoClass(WidgetSetClass).InDelText(Self, SelTextUTF8, SelStart, SelLength);
    SelStart := st;
    SelLength := length(UTF8Decode(SelTextUTF8));
  finally
    Lines.EndUpdate;
  end;
end;

constructor TCustomRichMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fZoomFactor:=1;
end;


procedure TCustomRichMemo.CopyToClipboard;  
begin
  if HandleAllocated then  
    TWSCustomRichMemoClass(WidgetSetClass).CopyToClipboard(Self);
end;

procedure TCustomRichMemo.CutToClipboard;  
begin
  if HandleAllocated then  
    TWSCustomRichMemoClass(WidgetSetClass).CutToClipboard(Self);
end;

procedure TCustomRichMemo.PasteFromClipboard;  
begin
  if HandleAllocated then  
    TWSCustomRichMemoClass(WidgetSetClass).PasteFromClipboard(Self);
end;

procedure TCustomRichMemo.SetRangeColor(TextStart, TextLength: Integer; FontColor: TColor);
begin
  SetRangeParams(TextStart, TextLength, [tmm_Color], '', 0, FontColor, [], []);
end;

procedure TCustomRichMemo.SetRangeParams(TextStart, TextLength: Integer; ModifyMask: TTextModifyMask;
      const FontName: String; FontSize: Integer; FontColor: TColor; AddFontStyle, RemoveFontStyle: TFontStyles);
var
  fnt : TFontParams;
begin
  InitFontParams(fnt);
  fnt.Name:=FontName;
  fnt.Size:=FontSize;
  fnt.Color:=FontColor;
  SetRangeParams(TextStart, TextLength, ModifyMask, fnt, AddFontStyle, RemoveFontStyle);
end;

procedure TCustomRichMemo.SetRangeParams(TextStart, TextLength: Integer;
  ModifyMask: TTextModifyMask; const fnt: TFontParams; AddFontStyle,
  RemoveFontStyle: TFontStyles);
var
  i : Integer;
  j : Integer;
  l : Integer;
  p : TFontParams;
begin
  if not HandleAllocated then HandleNeeded;

  if (ModifyMask = []) or (TextLength = 0) then Exit;

  i := TextStart;
  j := TextStart + TextLength;
  while i < j do begin
    GetTextAttributes(i, p);

    if tmm_Name in ModifyMask   then p.Name := fnt.Name;
    if tmm_Color in ModifyMask  then p.Color := fnt.Color;
    if tmm_Size in ModifyMask   then p.Size := fnt.Size;
    if tmm_Styles in ModifyMask then p.Style := p.Style + AddFontStyle - RemoveFontStyle;
    if tmm_BackColor in ModifyMask then begin
      p.HasBkClr:=fnt.HasBkClr;
      p.BkColor:=fnt.BkColor;
    end;
    l := GetContStyleLength(i);
    if i + l > j then l := j - i;
    if l = 0 then Break;
    SetTextAttributes(i, l, p);
    inc(i, l);
  end;
end;

procedure TCustomRichMemo.SetRangeParaParams(TextStart, TextLength: INteger;
  ModifyMask: TParaModifyMask; const ParaMetric: TParaMetric);
var
  ln: Integer;
  m : TParaMetric;
begin
  repeat
    if not GetParaRange(TextStart, TextStart, ln) then Break;
    if ln=0 then Break;
    GetParaMetric(TextStart, m);

    if pmm_FirstLine in ModifyMask   then m.FirstLine:=ParaMetric.FirstLine;
    if pmm_HeadIndent in ModifyMask  then m.HeadIndent:=ParaMetric.HeadIndent;
    if pmm_TailIndent in ModifyMask  then m.TailIndent:=ParaMetric.TailIndent;
    if pmm_SpaceBefore in ModifyMask then m.SpaceBefore:=ParaMetric.SpaceBefore;
    if pmm_SpaceAfter in ModifyMask  then m.SpaceAfter:=ParaMetric.SpaceAfter;
    if pmm_LineSpacing in ModifyMask then m.LineSpacing:=ParaMetric.LineSpacing;
    SetParaMetric(TextStart, 1, m);

    inc(TextStart, ln);
    dec(TextLength, ln);

  until TextLength<=0;
end;

function TCustomRichMemo.LoadRichText(Source: TStream): Boolean;
begin
  Result:=false;
  if not HandleAllocated then HandleNeeded;
  if Assigned(Source) and HandleAllocated then begin
    if Assigned(RTFLoadStream) then begin
      Self.Lines.BeginUpdate;
      try
        Self.Lines.Clear;
        Result:=RTFLoadStream(Self, Source);
      finally
        Self.Lines.EndUpdate;
      end;
    end else begin
      Result := TWSCustomRichMemoClass(WidgetSetClass).LoadRichText(Self, Source);
    end;
  end;
end;

function TCustomRichMemo.SaveRichText(Dest: TStream): Boolean;
begin
  if Assigned(Dest) and HandleAllocated then begin

    if Assigned(RTFSaveStream) then begin
      Result := RTFSaveStream(Self, Dest)
    end else
      Result := TWSCustomRichMemoClass(WidgetSetClass).SaveRichText(Self, Dest);
  end else
    Result := false;
end;

function TCustomRichMemo.InDelText(const UTF8Text: string; InsStartChar, ReplaceLength: Integer): Integer;
begin
  Result:=0;
  if not HandleAllocated then HandleNeeded;
  if HandleAllocated then begin
    TWSCustomRichMemoClass(WidgetSetClass).InDelText(Self, UTF8Text, InsStartChar, ReplaceLength);
    Result:=UTF8length(UTF8Text);
  end;
end;

procedure TCustomRichMemo.SetSelLengthFor(const aselstr: string);
begin
  SelLength:=UTF8Length(aselstr);
end;

function TCustomRichMemo.Search(const ANiddle: string; Start, Len: Integer;
  const SearchOpt: TSearchOptions): Integer;
var
  so : TIntSearchOpt;
begin
  if not HandleAllocated then HandleNeeded;
  if HandleAllocated then begin
    so.len:=Len;
    so.start:=Start;
    so.options:=SearchOpt;
    Result:=TWSCustomRichMemoClass(WidgetSetClass).Search(Self, ANiddle, so);
  end else
    Result:=-1;
end;

end.

