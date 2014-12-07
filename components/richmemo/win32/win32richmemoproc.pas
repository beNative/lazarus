{
 win32richmemoproc.pas 
 
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

unit Win32RichMemoProc; 

{$mode objfpc}{$H+}

interface

uses
  // windows units
  Windows,richedit, 
  // RTL units  
  Classes, SysUtils, 
  // LCL units
  Graphics,
  // RichMemoUnits
  RichMemo, WSRichMemo, RichMemoUtils,
  // Win32 widgetset units  
  win32proc, ActiveX, ComObj;

const
  IID_IRichEditOle: TGUID = '{00020D00-0000-0000-C000-000000000046}';
  IID_IRichEditOleCallback: TGUID = '{00020D03-0000-0000-C000-000000000046}';
  CLSID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';

const
  OLERENDER_NONE    = 0;
  OLERENDER_DRAW    = 1;
  OLERENDER_FORMAT  = 2;
  OLERENDER_ASIS    = 3;

const
  REO_GETOBJ_NO_INTERFACES	= 0;
  REO_GETOBJ_POLEOBJ	= 1;
  REO_GETOBJ_PSTG	 = 2;
  REO_GETOBJ_POLESITE	 = 4;
  REO_GETOBJ_ALL_INTERFACES	= 7;
  REO_CP_SELECTION  = -1;
  REO_IOB_SELECTION = -1;
  REO_IOB_USE_CP = -2;
  REO_NULL	= 0;
  REO_READWRITEMASK	= $3F;
  REO_DONTNEEDPALETTE	= 32;
  REO_BLANK	=  16;
  REO_DYNAMICSIZE		= 8;
  REO_INVERTEDSELECT	= 4;
  REO_BELOWBASELINE	= 2;
  REO_RESIZABLE	    = 1;
  REO_LINK	        = $80000000;
  REO_STATIC        =	$40000000;
  REO_SELECTED			= $08000000;
  REO_OPEN	        = $4000000;
  REO_INPLACEACTIVE	= $2000000;
  REO_HILITED	      = $1000000;
  REO_LINKAVAILABLE	= $800000;
  REO_GETMETAFILE	  = $400000;
  RECO_PASTE = 0;
  RECO_DROP	 = 1;
  RECO_COPY	 = 2;
  RECO_CUT	 = 3;
  RECO_DRAG	 = 4;



type
  TREOBJECT = packed record
    cbStruct  : DWORD;
    cp        : LONG;
    clsid     : CLSID;
    poleobj   : IOLEOBJECT;
    pstg      : ISTORAGE;
    polesite  : IOLECLIENTSITE;
    sizel     : SIZEL;
    dvaspect  : DWORD;
    dwFlags   : DWORD;
    dwUser    : DWORD;
  end;

type
  IRichEditOle = interface(IUnknown)
    ['{00020D00-0000-0000-C000-000000000046}']
    // *** IRichEditOle methods ***
    function GetClientSite(out clientSite: IOleClientSite): HRESULT; stdcall;
    function GetObjectCount: LongInt; stdcall;
    function GetLinkCount: LongInt; stdcall;
    function GetObject(iob: LongInt; out ReObject: TReObject;
      dwFlags: DWORD): HRESULT; stdcall;
    function InsertObject(var ReObject: TReObject): HRESULT; stdcall;
    function ConvertObject(iob: LongInt; const clsidNew: TCLSID;
      lpStrUserTypeNew: LPCSTR): HRESULT; stdcall;
    function ActivateAs(const clsid, clsidAs: TCLSID): HRESULT; stdcall;
    function SetHostNames(lpstrContainerApp: LPCSTR;
      lpstrContainerObj: LPCSTR): HRESULT; stdcall;
    function SetLinkAvailable(iob: LongInt; fAvailable: BOOL): HRESULT; stdcall;
    function SetDvaspect(iob: LongInt; dvaspect: DWORD): HRESULT; stdcall;
    function HandsOffStorage(iob: LongInt): HRESULT; stdcall;
    function SaveCompleted(iob: LongInt; const stg: IStorage): HRESULT; stdcall;
    function InPlaceDeactivate: HRESULT; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HRESULT; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
      out dataobj: IDataObject): HRESULT; stdcall;
    function ImportDataObject(const dataobj: IDataObject; cf: TClipFormat;
      hMetaPict: HGLOBAL): HRESULT; stdcall;
  end;

  IRichEditOleCallback = interface(IUnknown)
    ['{00020D03-0000-0000-C000-000000000046}']
    // *** IRichEditOleCallback methods ***
    function GetNewStorage(out stg: IStorage): HRESULT; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame;
      out Doc: IOleInPlaceUIWindow;
      lpFrameInfo: POleInPlaceFrameInfo): HRESULT; stdcall;
    function ShowContainerUI(fShow: BOOL): HRESULT; stdcall;
    function QueryInsertObject(const clsid: TCLSID; const stg: IStorage;
      cp: LongInt): HRESULT; stdcall;
    function DeleteObject(const oleobj: IOleObject): HRESULT; stdcall;
    function QueryAcceptData(const dataobj: IDataObject;
      var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL;
      hMetaPict: HGLOBAL): HRESULT; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HRESULT; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
      out dataobj: IDataObject): HRESULT; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
      var dwEffect: DWORD): HRESULT; stdcall;
    function GetContextMenu(seltype: Word; oleobj: IOleObject;
      const chrg: TCharRange; var menu: HMENU): HRESULT; stdcall;
  end;

type
  { TRichEditManager }

  TRichEditManager = class(TObject)
  public
    class function SetEventMask(RichEditWnd: Handle; eventmask: integer): Integer;

    class function GetTextLength(RichEditWnd: Handle): Integer;
    class function SetSelectedTextStyle(RichEditWnd: Handle; Params: TIntFontParams): Boolean; virtual;
    class function GetSelectedTextStyle(RichEditWnd: Handle; var Params: TIntFontParams): Boolean; virtual;
    class function GetStyleRange(RichEditWnd: Handle; TextStart: Integer; var RangeStart, RangeLen: Integer): Boolean; virtual; 
    class procedure GetSelection(RichEditWnd: Handle; var TextStart, TextLen: Integer); virtual;      
    class procedure SetSelection(RichEditWnd: Handle; TextStart, TextLen: Integer); virtual;      
    class procedure SetHideSelection(RichEditWnd: Handle; AValue: Boolean); virtual;
    class function LoadRichText(RichEditWnd: Handle; ASrc: TStream): Boolean; virtual;
    class function SaveRichText(RichEditWnd: Handle; ADst: TStream): Boolean; virtual;
    class procedure SetText(RichEditWnd: Handle; const Text: WideString; TextStart, ReplaceLength: Integer); virtual;
    class procedure GetPara2(RichEditWnd: Handle; TextStart: Integer; var para: PARAFORMAT2); virtual;
    class procedure SetPara2(RichEditWnd: Handle; TextStart, TextLen: Integer; const para: PARAFORMAT2); virtual;
    class function Find(RichEditWnd: THandle; const ANiddle: WideString; const ASearch: TIntSearchOpt): Integer; virtual;
    class procedure GetParaRange(RichEditWnd: Handle; TextStart: integer; var para: TParaRange); virtual;
  end;
  TRichManagerClass = class of TRichEditManager;
                     
var
  RichEditManager : TRichManagerClass = nil;

function InitRichEdit: Boolean;
function GetRichEditClass: AnsiString;
procedure CopyStringToCharArray(const s: String; var Chrs: array of Char; ChrsSize: integer);
function FontStylesToEffects(Styles: TFontStyles): LongWord;
function EffectsToFontStyles(Effects: LongWord): TFontStyles;

const
  CP_UNICODE = 1200;
  HardBreak  = #13;

const
  CFM_BACKCOLOR = $04000000;
  CFE_AUTOBACKCOLOR = CFM_BACKCOLOR;

implementation

const
  GlobalRichClass : AnsiString = '';
  
const
  TwipsInFontSize = 20; // see MSDN for CHARFORMAT Structure CFM_SIZE
  
function GetRichEditClass: AnsiString;
begin
  Result := GlobalRichClass;
end;  
 
function InitRichEdit: Boolean;
begin
  if GlobalRichClass = '' then begin
    if LoadLibrary('Msftedit.dll') <> 0 then begin
      GlobalRichClass := 'RichEdit50W';
    end else if LoadLibrary('RICHED20.DLL') <> 0 then begin
      if UnicodeEnabledOS then GlobalRichClass := 'RichEdit20W'
      else
      GlobalRichClass := 'RichEdit20A'
    end else if LoadLibrary('RICHED32.DLL') <> 0 then begin
      GlobalRichClass := 'RichEdit';
    end;
      
    if not Assigned(RichEditManager) then 
      RichEditManager := TRichEditManager;
      
    Result := GlobalRichClass <> '';
  end;
end;

procedure CopyStringToCharArray(const s: String; var Chrs: array of Char; ChrsSize: integer);
begin
  if length(s) < ChrsSize then ChrsSize := length(s);
  if length(s) > 0 then Move(s[1], Chrs[0], ChrsSize);
end;

function FontStylesToEffects(Styles: TFontStyles): LongWord;
begin
  Result := 0;
  if fsBold in Styles then Result := Result or CFE_BOLD;
  if fsItalic in Styles then Result := Result or CFE_ITALIC;
  if fsStrikeOut in Styles then Result := Result or CFE_STRIKEOUT;
  if fsUnderline in Styles then Result := Result or CFE_UNDERLINE;
end;

function EffectsToFontStyles(Effects: LongWord): TFontStyles;
begin
  Result := [];
  if Effects and CFE_BOLD > 0 then Include(Result, fsBold);
  if Effects and CFE_ITALIC > 0 then Include(Result, fsItalic);
  if Effects and CFE_STRIKEOUT > 0 then Include(Result, fsStrikeOut);
  if Effects and CFE_UNDERLINE > 0 then Include(Result, fsUnderline);
end;

         
procedure CharFormatToFontParams(const fmt: TCHARFORMAT; var Params: TIntFontParams);
begin
  Params.Name := fmt.szFaceName;
  Params.Size := Round(fmt.yHeight/TwipsInFontSize);
  Params.Color := fmt.crTextColor;
  Params.Style := EffectsToFontStyles(fmt.dwEffects);
end;

procedure CharFormatToFontParams(const fmt: TCHARFORMAT2; var Params: TIntFontParams);
begin
  Params.Name := fmt.szFaceName;
  Params.Size := Round(fmt.yHeight/TwipsInFontSize);
  Params.Color := fmt.crTextColor;
  Params.Style := EffectsToFontStyles(fmt.dwEffects);
  if fmt.cbSize > sizeof(CHARFORMAT) then begin
    Params.HasBkClr:=(fmt.dwEffects and CFE_AUTOBACKCOLOR) = 0;
    if Params.HasBkClr then Params.Color:=Params.Color;
  end;
end;


{ TRichEditManager }

class function TRichEditManager.SetEventMask(RichEditWnd: Handle; eventmask: integer): Integer;
begin
  Result := SendMessage(RichEditWnd, EM_GETEVENTMASK, 0, 0);
  SendMessage(RichEditWnd, EM_SETEVENTMASK, 0, eventmask);
end;

class function TRichEditManager.GetTextLength(RichEditWnd: Handle): Integer;
var
  textlen : TGETTEXTEX;
begin
  FillChar(textlen, sizeof(textlen), 0);
  textlen.flags := GTL_NUMCHARS or GTL_PRECISE;
  textlen.codepage := CP_UNICODE;
  Result := SendMessage(RichEditWnd, EM_GETTEXTLENGTHEX, WPARAM(@textlen), 0);
end;

class function TRichEditManager.SetSelectedTextStyle(RichEditWnd: Handle; 
  Params: TIntFontParams): Boolean;
var
  w : WPARAM;
  fmt : TCHARFORMAT2;
begin
  if RichEditWnd = 0 then begin
    Result := false;
    Exit;
  end;
  
  w := SCF_SELECTION;    
    
  FillChar(fmt, sizeof(fmt), 0);
  fmt.cbSize := sizeof(fmt);
  
  fmt.dwMask := fmt.dwMask or CFM_COLOR;
  fmt.crTextColor := Params.Color;

  fmt.dwMask := fmt.dwMask or CFM_FACE;
  // keep last char for Null-termination?
  CopyStringToCharArray(Params.Name, fmt.szFaceName, LF_FACESIZE-1); 
  
  fmt.dwMask := fmt.dwMask or CFM_SIZE;
  fmt.yHeight := Params.Size * TwipsInFontSize;
  
  fmt.dwMask := fmt.dwMask or CFM_EFFECTS;
  fmt.dwEffects := FontStylesToEffects(Params.Style);

  if Params.HasBkClr then begin
    fmt.dwMask := fmt.dwMask or CFM_BACKCOLOR;
    fmt.crBackColor := Params.BkColor;
  end else begin
    fmt.dwMask := fmt.dwMask or CFM_BACKCOLOR;
    fmt.dwEffects := fmt.dwEffects or CFE_AUTOBACKCOLOR;
  end;

  Result := SendMessage(RichEditWnd, EM_SETCHARFORMAT, w, PtrInt(@fmt))>0;
end;

class function TRichEditManager.GetSelectedTextStyle(RichEditWnd: Handle;  
  var Params: TIntFontParams): Boolean; 
var
  w     : WPARAM;
  fmt   : TCHARFORMAT2;
  
begin
  Result := false;
  if RichEditWnd = 0 then Exit;
  
  w := SCF_SELECTION;    
    
  FillChar(fmt, sizeof(fmt), 0);
  fmt.cbSize := sizeof(fmt);
  fmt.dwMask := CFM_COLOR or CFM_FACE or CFM_SIZE or CFM_EFFECTS or CFM_BACKCOLOR;
  
  SendMessage(RichEditWnd, EM_GETCHARFORMAT, w, PtrInt(@fmt));
  
  CharFormatToFontParams(fmt, Params);
  Result := true;  
end;

type
  richedit_gettextlengthex = packed record
    flags     : DWORD;
    codepage  : LongWord;
  end;
  Tgettextlengthex = richedit_gettextlengthex;

class function TRichEditManager.GetStyleRange(RichEditWnd: Handle; TextStart: Integer; 
  var RangeStart, RangeLen: Integer): Boolean; 
var
  len     : integer;
  fmt     : TCHARFORMAT;
  textlen : Tgettextlengthex;
  sel     : TCHARRANGE;
  d       : Integer;
  last    : Integer;

const
  ALL_MASK = CFM_BOLD or CFM_ITALIC or CFM_STRIKEOUT or CFM_UNDERLINE or 
             CFM_SIZE or CFM_COLOR or CFM_FACE;
begin
  Result := false;
  if (RichEditWnd = 0) then Exit;
  
  FillChar(textlen, sizeof(textlen), 0);
  textlen.flags := GTL_NUMCHARS or GTL_PRECISE;
  textlen.codepage := CP_UNICODE;
  len := SendMessage(RichEditWnd, EM_GETTEXTLENGTHEX, WPARAM(@textlen), 0);
  Result := TextStart < len;
  if not Result then Exit;
   
  FillChar(fmt, sizeof(fmt), 0);
  fmt.cbSize := sizeof(fmt);
  
  sel.cpMin := TextStart;
  sel.cpMax := len+1;
  SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
  SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
  if (fmt.dwMask and ALL_MASK) <> ALL_MASK then begin
    d := (len - sel.cpMin);
    while d > 1 do begin
      d := d div 2;
      if (fmt.dwMask and ALL_MASK) = ALL_MASK then
        sel.cpMax := sel.cpMax + d        
      else
        sel.cpMax := sel.cpMax - d;
      SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
      SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
    end;
    if (fmt.dwMask and ALL_MASK) = ALL_MASK then begin
      while (sel.cpMax <= len) and ((fmt.dwMask and ALL_MASK) = ALL_MASK) do begin
        inc(sel.cpMax);
        SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
        SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
      end;
    end else begin
      while (sel.cpMax > sel.cpMin) and ((fmt.dwMask and ALL_MASK) <> ALL_MASK) do begin
        dec(sel.cpMax);
        SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
        SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
      end;
      inc(sel.cpMax);
    end;
  end;
  last := sel.cpMax;  
  
  sel.cpMin := 0;
  sel.cpMax := TextStart+1;
  SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
  SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
  if (fmt.dwMask and ALL_MASK) <> ALL_MASK then begin
    d := TextStart;
    while d > 1 do begin
      d := d div 2;
      if (fmt.dwMask and ALL_MASK) = ALL_MASK then
        dec(sel.cpMin,d)
      else
        inc(sel.cpMin,d);
      SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
      SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
    end;
    if (fmt.dwMask and ALL_MASK) = ALL_MASK then begin
      while (sel.cpMin > 0) and ((fmt.dwMask and ALL_MASK) = ALL_MASK) do begin
        dec(sel.cpMin);
        SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
        SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
      end;
      if (fmt.dwMask and ALL_MASK) <> ALL_MASK then inc(sel.cpMin);
    end else begin
      while (sel.cpMin < TextStart) and ((fmt.dwMask and ALL_MASK) <> ALL_MASK) do begin
        inc(sel.cpMin);
        SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
        SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
      end;
    end;
  end;  
 
  RangeStart := sel.cpMin;
  RangeLen := last - sel.cpMin - 1;

  Result := true;  
end;

class procedure TRichEditManager.GetSelection(RichEditWnd: Handle; var TextStart, TextLen: Integer); 
var
  Range  : TCHARRANGE;
begin
  Range.cpMax := 0;
  Range.cpMin := 0;
  SendMessage(RichEditWnd, EM_EXGETSEL, 0, PtrInt(@Range));
  TextStart := Range.cpMin;
  TextLen := Range.cpMax-Range.cpMin;
end;

class procedure TRichEditManager.SetSelection(RichEditWnd: Handle; TextStart, TextLen: Integer); 
var
  Range  : TCHARRANGE;
begin
  Range.cpMin := TextStart;
  Range.cpMax := TextStart + TextLen;
  SendMessage(RichEditWnd, EM_EXSETSEL, 0, PtrInt(@Range));
end;

class procedure TRichEditManager.SetHideSelection(RichEditWnd: Handle; AValue: Boolean);
var
  style  : LResult;
begin
  // res-setting options might RichEdit style. Must restore it, after option is changed
  style := GetWindowLong(RichEditWnd, GWL_STYLE);
  if AValue then
    SendMessage(RichEditWnd, EM_SETOPTIONS, ECOOP_AND, not ECO_NOHIDESEL)
  else
    SendMessage(RichEditWnd, EM_SETOPTIONS, ECOOP_OR, ECO_NOHIDESEL);
  SetWindowLong(RichEditWnd, GWL_STYLE, style);
end;

type
  TEditStream_ = packed record
    dwCookie    : PDWORD;
    dwError     : DWORD;
    pfnCallback : EDITSTREAMCALLBACK;
  end;
  
function RTFLoadCallback(dwCookie:PDWORD; pbBuff:LPBYTE; cb:LONG; var pcb:LONG):DWORD; stdcall;
var
  s : TStream;  
begin
  try
    s := TStream(dwCookie);
    pcb := s.Read(pbBuff^, cb);
    Result := 0;
  except
    Result := 1;
  end;
end;

class function TRichEditManager.LoadRichText(RichEditWnd: Handle; ASrc: TStream): Boolean; 
var
  cbs : TEditStream_;
begin
  cbs.dwCookie := PDWORD(ASrc);
  cbs.dwError := 0;
  cbs.pfnCallback := @RTFLoadCallback;
  SendMessage(RichEditWnd, EM_STREAMIN, SF_RTF, LPARAM(@cbs) );
  Result := cbs.dwError = 0;
end;

function RTFSaveCallback(dwCookie:PDWORD; pbBuff:LPBYTE; cb:LONG; var pcb:LONG):DWORD; stdcall;
var
  s : TStream;  
begin
  try
    s := TStream(dwCookie);
    pcb := s.Write(pbBuff^, cb);
    Result := 0;
  except
    Result := 1;
  end;
end;

class function TRichEditManager.SaveRichText(RichEditWnd: Handle; ADst: TStream): Boolean; 
var
  cbs : TEditStream_;
begin
  cbs.dwCookie := PDWORD(ADst);
  cbs.dwError := 0;
  cbs.pfnCallback := @RTFSaveCallback;
  SendMessage(RichEditWnd, EM_STREAMOUT, SF_RTF, LPARAM(@cbs) );
  Result := cbs.dwError = 0;
end;

class procedure TRichEditManager.SetText(RichEditWnd:Handle;
  const Text: WideString; TextStart, ReplaceLength:Integer);
var
  AnsiText : AnsiString;
  txt      : PChar;
  s, l     : Integer;
begin
  GetSelection(RichEditWnd, s, l);
  SetSelection(RichEditWnd, TextStart, ReplaceLength);

  txt:=nil;
  if UnicodeEnabledOS then begin
    if Text<>'' then txt:=@Text[1];
    SendMessageW(RichEditWnd, EM_REPLACESEL, 0, LPARAM(txt));
  end else begin
    AnsiText:=Text;
    if AnsiText<>'' then txt:=@AnsiText[1];
    SendMessageA(RichEditWnd, EM_REPLACESEL, 0, LPARAM(txt));
  end;

  SetSelection(RichEditWnd, s, l);
end;

class procedure TRichEditManager.GetPara2(RichEditWnd: Handle; TextStart: Integer;
  var para: PARAFORMAT2);
var
  s, l     : Integer;
begin
  GetSelection(RichEditWnd, s, l);

  SetSelection(RichEditWnd, TextStart, 0);

  FillChar(para, sizeof(para), 0);
  para.cbSize:=sizeof(para);
  SendMessagea(RichEditWnd, EM_GETPARAFORMAT, 0, LPARAM(@para));

  SetSelection(RichEditWnd, s, l);
end;

class procedure TRichEditManager.SetPara2(RichEditWnd: Handle;
  TextStart, TextLen: Integer; const para: PARAFORMAT2);
var
  s, l     : Integer;
begin
  GetSelection(RichEditWnd, s, l);
  SetSelection(RichEditWnd, TextStart, TextLen);

  SendMessagea(RichEditWnd, EM_SETPARAFORMAT, 0, LPARAM(@para));

  SetSelection(RichEditWnd, s, l);
end;

class function TRichEditManager.Find(RichEditWnd: THandle;
  const ANiddle: WideString; const ASearch: TIntSearchOpt): Integer;
var
  fw: TFINDTEXTW;
  fa: TFINDTEXTA;
  opt: WParam;
  txt: string;
  mn, mx : Integer;
begin
  if ANiddle='' then begin
    Result:=-1;
    Exit;
  end;
  opt:=0;
  if not (soBackward in ASearch.Options) then opt:=FR_DOWN; // if not set, then search is backward
  if soMatchCase in ASearch.Options then opt := opt or FR_MATCHCASE;
  if soWholeWord in ASearch.Options then opt := opt or FR_WHOLEWORD;
  mn := ASearch.start;
  if soBackward in ASearch.Options then begin
    if ASearch.len<0 then mx := 0
    else begin
      mx := ASearch.start-ASearch.len;
      if mx < 0 then mx:=0;
    end;
  end else begin
    if ASearch.len<0 then fw.chrg.cpMax := -1
    else begin
      mx := ASearch.start+ASearch.len;
      if mx < 0 then mx:=-1;
    end;
  end;

  if UnicodeEnabledOS then begin
    fw.chrg.cpMin := mn;
    fw.chrg.cpMax := mx;
    fw.lpstrText := PWideChar(@ANiddle[1]);
    Result := SendMessage(RichEditWnd, EM_FINDTEXTW, opt, LParam(@fw));
  end else begin
    fa.chrg.cpMin := mn;
    fa.chrg.cpMax := mx;
    txt:=ANiddle;
    fa.lpstrText := PAnsiChar(@txt[1]);
    Result := SendMessage(RichEditWnd, EM_FINDTEXT, opt, LParam(@fa));
  end;
end;

class procedure TRichEditManager.GetParaRange(RichEditWnd: Handle; TextStart: integer;
  var para: TParaRange);
var
  line: Integer;
  txtlen: Integer;
  st: Integer;
  ln: Integer;
  toend: Integer;
  tost: Integer;
  buf : string[16];
  rng : TTEXTRANGEA;
  res : Integer;
begin
  txtlen:=GetTextLength(RichEditWnd);
  // lines are NOT paragraphs, but wordwrapped lines
  line:=SendMessage(RichEditWnd, EM_EXLINEFROMCHAR, 0, TextStart);
  st:=SendMessage(RichEditWnd, EM_LINEINDEX, line, 0);
  tost:=st;
  toend:=0;

  while tost>0 do begin
    rng.lpstrText:=@buf[1];
    rng.chrg.cpMin:=tost-1;
    rng.chrg.cpMax:=tost;
    buf[1]:=#0;
    res:=SendMessageA(RichEditWnd, EM_GETTEXTRANGE, 0, LPARAM(@rng));
    if (buf[1]=HardBreak) then
      Break // found the beggining of the paragraph
    else begin
      line:=SendMessage(RichEditWnd, EM_EXLINEFROMCHAR, 0, tost-2); // getting the line before the linebreak
      tost:=SendMessage(RichEditWnd, EM_LINEINDEX, line, 0);
      inc(toend, SendMessage(RichEditWnd, EM_LINELENGTH, line, 0));
    end;
  end;

  repeat
    ln:=SendMessage(RichEditWnd, EM_LINELENGTH, st, 0);
    inc(toend, ln);
    inc(st, ln);
    rng.lpstrText:=@buf[1];
    rng.chrg.cpMin:=st;
    rng.chrg.cpMax:=st+1;
    buf[1]:=#0;
    res:=SendMessage(RichEditWnd, EM_GETTEXTRANGE, 0, LPARAM(@rng));
  until (res=0) or (buf[1] = HardBreak);

  para.start:=tost;
  para.lenghtNoBr:=toend;
  if res>0 then inc(toend); // there's a line break character - add it to the range
  para.length:=toend;
end;

function WinInsertImageFromFile (const ARichMemo: TCustomRichMemo; APos: Integer;
   const FileNameUTF8: string;
   const AImgSize: TSize): Boolean;
var
  hnd : THandle;
  rch : IRichEditOle;
  Fmt : FORMATETC;
  FN : WideString;
  LockBytes: ILockBytes;
  ClientSite: IOleClientSite;
  Storage: IStorage;
  Image: IOleObject;
  Obj: TREOBJECT;

  sl, ss: Integer;
const
  PointSize     = 72.0;
  RtfSizeToInch = 2.54 * 1000.0;
  SizeFactor    = 1 / PointSize * RtfSizeToInch;
begin
  Result:=false;
  if not Assigned(ARichMemo) then Exit;
  if not ARichMemo.HandleAllocated then begin
    ARichMemo.HandleNeeded;
    if not ARichMemo.HandleAllocated then Exit;
  end;
  if (FileNameUTF8 ='') then Exit;

  ss:=ARichMemo.SelStart;
  sl:=ARichMemo.SelLength;
  try
    hnd:= THandle(ARichMemo.Handle);
    SendMessage(hnd, EM_GETOLEINTERFACE, 0, LPARAM(@rch));

    FillChar(Fmt, sizeoF(Fmt), 0);
    Fmt.dwAspect:=DVASPECT_CONTENT;
    Fmt.lindex:=-1;

    CreateILockBytesOnHGlobal(0, True, LockBytes);
    StgCreateDocfileOnILockBytes(LockBytes, STGM_SHARE_EXCLUSIVE or STGM_CREATE or STGM_READWRITE, 0, Storage);
    rch.GetClientSite(ClientSite);

    FN := UTF8Decode( FileNameUTF8 );
    OleCreateFromFile(CLSID_NULL, @FN[1], IOleObject
       , OLERENDER_DRAW, @Fmt, ClientSite, Storage, Image);
    OleSetContainedObject(Image, True);

    FillChar(Obj, sizeof(Obj),0);
    Obj.cbStruct := SizeOf(Obj);
    Obj.cp := REO_CP_SELECTION;
    Image.GetUserClassID(Obj.clsid);
    Obj.poleobj := Image;
    Obj.pstg := Storage;
    Obj.polesite := ClientSite;
    Obj.dvaspect := DVASPECT_CONTENT;
    if (AImgSize.cx<>0) or (AImgSize.cy<>0) then begin
      //http://msdn.microsoft.com/en-us/library/windows/desktop/bb787946%28v=vs.85%29.aspx
      //The size of the object. The unit of measure is 0.01 millimeters, which is a HIMETRIC measurement.
      Obj.sizel.cx:=round(AImgSize.cx * SizeFactor);
      Obj.sizel.cy:=round(AImgSize.cy * SizeFactor);
    end;

    ARichMemo.SelStart:=APos;
    ARichMemo.SelLength:=0;
    Result:= Succeeded(rch.InsertObject(obj));
  finally
    ARichMemo.SelStart:=ss;
    ARichMemo.SelLength:=sl;
  end;
end;

initialization
  InsertImageFromFile := @WinInsertImageFromFile;

end.                                            

