{
 win32richmemo.pas 
 
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

unit Win32RichMemo;

{$mode objfpc}{$H+}

interface

uses
  // Win32 headers  
  Windows, RichEdit, ActiveX,
  // RTL headers
  Classes, SysUtils, 
  // LCL headers
  LCLType, LCLIntf, LCLProc, WSLCLClasses,
  Graphics, Controls, StdCtrls, 
  // Win32WidgetSet
  Win32WSControls, Win32Int, Win32WSStdCtrls, win32proc,
  // RichMemo headers
  RichMemo, WSRichMemo, Win32RichMemoProc, Win32RichMemoOle;

type  

  { TWin32RichMemoStringsW }

  TWin32RichMemoStringsW = class(TWin32MemoStrings)
  protected
    fHandle : HWND;
    function GetTextStr: string; override;
  public
    constructor Create(AHandle: HWND; TheOwner: TWinControl);
  end;

  { TWin32RichMemoStringsA }

  TWin32RichMemoStringsA = class(TWin32MemoStrings)
  protected
    fHandle : HWND;
    function GetTextStr: string; override;
  public
    constructor Create(AHandle: HWND; TheOwner: TWinControl);
  end;

  { TWin32WSCustomRichMemo }

  TWin32WSCustomRichMemo = class(TWSCustomRichMemo)
  published
    class function GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;
    class procedure SetColor(const AWinControl: TWinControl); override;
  
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    
    class procedure CutToClipboard(const AWinControl: TWinControl); override;
    class procedure CopyToClipboard(const AWinControl: TWinControl); override;
    class procedure PasteFromClipboard(const AWinControl: TWinControl); override;

    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TIntFontParams): Boolean; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const Params: TIntFontParams); override;
    class procedure SetHideSelection(const ACustomEdit: TCustomEdit; AHideSelection: Boolean); override;      
    class function GetStyleRange(const AWinControl: TWinControl; TextStart: Integer; var RangeStart, RangeLen: Integer): Boolean; override;
    class function LoadRichText(const AWinControl: TWinControl; Source: TStream): Boolean; override;
    class function SaveRichText(const AWinControl: TWinControl; Dst: TStream): Boolean; override;

    class function GetParaAlignment(const AWinControl: TWinControl; TextStart: Integer;
      var AAlign: TParaAlignment): Boolean; override;
    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: TIntParaAlignment); override;

    class function GetParaMetric(const AWinControl: TWinControl; TextStart: Integer;
      var AMetrics: TIntParaMetric): Boolean; override;
    class procedure SetParaMetric(const AWinControl: TWinControl; TextStart, TextLength: Integer;
      const AMetrics: TIntParaMetric); override;
    class function GetParaRange(const AWinControl: TWinControl; TextStart: Integer;
      var ParaRange: TParaRange): Boolean; override;

    class function GetParaNumbering(const AWinControl: TWinControl; TextStart: Integer;
      var ANumber: TIntParaNumbering): Boolean; override;
    class procedure SetParaNumbering(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const ANumber: TIntParaNumbering); override;

    class procedure InDelText(const AWinControl: TWinControl; const TextUTF8: String; DstStart, DstLen: Integer); override;

    class function Search(const AWinControl: TWinControl; const ANiddle: string;
      const SearchOpts: TIntSearchOpt): Integer; override;

    class procedure SetZoomFactor(const AWinControl: TWinControl; AZoomFactor: Double); override;

    class function InlineInsert(const AWinControl: TWinControl; ATextStart, ATextLength: Integer;
      const ASize: TSize; AHandler: TRichMemoInline; var wsObj: TRichMemoInlineWSObject): Boolean; override;
    class procedure InlineInvalidate(const AWinControl: TWinControl;
       AHandler: TRichMemoInline; wsObj: TRichMemoInlineWSObject); override;
  end;

  { TWin32Inline }

  TWin32Inline = class(TCustomDataViewObject, IOleObject, IDataObject, IViewObject)
  public
    richMemo : TCustomRichMemo;
    canvas   : TCanvas;
    rminline : TRichMemoInline;
    isvis    : Boolean;
    function Draw(dwDrawAspect:DWord;LIndex:Long;pvaspect:pointer;ptd:PDVTARGETDEVICE;hdcTargetDev:HDC; hdcDraw:HDC;lprcBounds:PRECTL;lprcWBounds:PRECTL;pfncontinue:TContinueCallback;dwcontinue:ULONG_PTR):HResult; stdcall;
    function GetExtent(dwDrawAspect: DWORD; out size: TPoint): HResult;StdCall;
    function Close(dwSaveOption: DWORD): HResult;StdCall;
    destructor Destroy; override;
  end;

  
implementation

type
  TIntCustomRichMemo = class(TCustomRichMemo);

const
  AlignmentToEditFlags: array[TAlignment] of DWord =
  (
{ taLeftJustify  } ES_LEFT,
{ taRightJustify } ES_RIGHT,
{ taCenter       } ES_CENTER
  );

  
procedure LockRedraw(AHandle: HWND);
begin
  SendMessage(AHandle, WM_SETREDRAW, 0, 0);
end;

procedure UnlockRedraw(AHandle: HWND; NeedInvalidate: Boolean = true);
begin
  SendMessage(AHandle, WM_SETREDRAW, 1, 0);
  if NeedInvalidate then 
    Windows.InvalidateRect(AHandle, nil, true);
end;

function RichEditNotifyProc(const AWinControl: TWinControl; Window: HWnd;
      Msg: UInt; WParam: Windows.WParam; LParam: Windows.LParam;
      var MsgResult: Windows.LResult; var WinProcess: Boolean): Boolean;
var
  sch : PSELCHANGE;
begin
  Result:=false; // we need to catch just notifications,
    // any other message should be handled in a "Default" manner
    // So, default result is false;
  case Msg of
    WM_NOTIFY: begin
      sch:=PSELCHANGE(LPARAM);
      if sch^.nmhdr.code=EN_SELCHANGE then
      begin
        if Assigned(AWinControl) and (AWinControl is TCustomRichMemo) then
          TIntCustomRichMemo(AWinControl).DoSelectionChange;
        Result:=true;
      end;
    end;
  end;
end;

function RichEditProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
   LParam: Windows.LParam): LResult; stdcall;
begin 
  case Msg of
    WM_PAINT : begin
      //todo: LCL WM_PAINT handling prevents richedit from drawing correctly
      Result := CallDefaultWindowProc(Window, Msg, WParam, LParam)
      //Result := WindowProc(Window, Msg, WParam, LParam)
      end;
  else
    Result := WindowProc(Window, Msg, WParam, LParam);
  end;
end;

{ TWin32Inline }

function TWin32Inline.Draw(dwDrawAspect: DWord; LIndex: Long;
  pvaspect: pointer; ptd: PDVTARGETDEVICE; hdcTargetDev: HDC; hdcDraw: HDC;
  lprcBounds: PRECTL; lprcWBounds: PRECTL; pfncontinue: TContinueCallback;
  dwcontinue: ULONG_PTR): HResult; stdcall;
var
  rst : Boolean;
  pts : Windows.TPOINT;
  sz  : TSize;
begin
  if not isvis then begin
    isvis:=true;
    rminline.SetVisible(isvis);
  end;
  canvas.Handle:=hdcDraw;

  rst:= Assigned(lprcBounds);
  if rst then begin
    Windows.OffsetViewportOrgEx(hdcDraw, lprcBounds^.left, lprcBounds^.top, @pts);
    sz.cx:=lprcBounds^.right - lprcBounds^.left;
    sz.cy:=lprcBounds^.bottom - lprcBounds^.top;
  end else begin
    sz.cx:=0;
    sz.cy:=0;
  end;

  rminline.Draw(canvas, sz);
  if rst then Windows.OffsetViewportOrgEx(hdcDraw, pts.x, pts.y, nil);

  Result:=S_OK;
end;

function TWin32Inline.GetExtent(dwDrawAspect: DWORD; out size: TPoint
  ): HResult; StdCall;
begin
  if not isvis then begin
    rminline.SetVisible(true);
    isvis:=true;
  end;
  Result:=inherited GetExtent(dwDrawAspect, size);
end;

function TWin32Inline.Close(dwSaveOption: DWORD): HResult; StdCall;
begin
  if isvis then begin
    rminline.SetVisible(false);
    isvis:=false;
  end;
  Result:=inherited Close(dwSaveOption);
end;

destructor TWin32Inline.Destroy;
begin
  rminline.Free;
  inherited Destroy;
end;

{ TWin32RichMemoStringsW }

constructor TWin32RichMemoStringsW.Create(AHandle: HWND; TheOwner: TWinControl);
begin
  inherited Create(AHandle, TheOwner);
  fHandle:=AHandle;
end;

function TWin32RichMemoStringsW.GetTextStr: string;
var
  p   : GETTEXTLENGTHEX;
  t   : GETTEXTEX;
  res : Integer;
  w   : WideString;
begin
  fillchar(p, sizeof(p), 0);
  p.flags:=GTL_DEFAULT or GTL_PRECISE or GTL_NUMCHARS;
  res := SendMessageW(fHandle, EM_GETTEXTLENGTHEX, WPARAM(@P), CP_WINUNICODE);
  if res>0 then begin
    SetLength(w, res);
    FillChar(t, sizeof(t), 0);
    t.cb:=(length(w)+1)*sizeof(WideChar);
    t.flags:=GT_DEFAULT;
    t.codepage:=CP_WINUNICODE;
    res:=SendMessageW(fHandle, EM_GETTEXTEX, WPARAM(@t), LPARAM(@w[1]));
    Result:=UTF8Encode(w);
  end else
    Result:='';
end;


{ TWin32RichMemoStringsA }

constructor TWin32RichMemoStringsA.Create(AHandle: HWND; TheOwner: TWinControl);
begin
  inherited Create(AHandle, TheOwner);
  fHandle:=AHandle;
end;

function TWin32RichMemoStringsA.GetTextStr: string;
var
  p   : GETTEXTLENGTHEX;
  t   : GETTEXTEX;
  res : Integer;
  s   : WideString;
begin
  fillchar(p, sizeof(p), 0);
  p.flags:=GTL_DEFAULT or GTL_PRECISE or GTL_NUMBYTES;
  res := SendMessageW(fHandle, EM_GETTEXTLENGTHEX, WPARAM(@P), CP_ACP);
  if res>0 then begin
    SetLength(s, res);
    FillChar(t, sizeof(t), 0);
    t.cb:=length(s)+1;
    t.flags:=GT_DEFAULT;
    t.codepage:=CP_ACP;
    res:=SendMessageW(fHandle, EM_GETTEXTEX, WPARAM(@t), LPARAM(@s[1]));
    Result:=AnsiToUtf8(s);
  end else
    Result:='';
end;

{ TWin32WSCustomRichMemo }

class function TWin32WSCustomRichMemo.GetStrings(const ACustomMemo: TCustomMemo
  ): TStrings;
begin
  if UnicodeEnabledOS then
    Result := TWin32RichMemoStringsW.Create(ACustomMemo.Handle, ACustomMemo)
  else
    Result := TWin32RichMemoStringsA.Create(ACustomMemo.Handle, ACustomMemo);
end;

class procedure TWin32WSCustomRichMemo.SetColor(const AWinControl: TWinControl);  
begin
  // this methos is implemented, because Win32RichMemo doesn't use 
  // default LCL WM_PAINT message!
  SendMessage(AWinControl.Handle, EM_SETBKGNDCOLOR, 0, ColorToRGB(AWinControl.Color));
end;

class procedure TWin32WSCustomRichMemo.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);  
var
  range : Tcharrange;
begin
  range.cpMin := NewStart;
  range.cpMax := NewStart;
  SendMessage(ACustomEdit.Handle, EM_EXSETSEL, 0, LPARAM(@range));
  InvalidateRect(ACustomEdit.Handle, nil, false);
end;

class procedure TWin32WSCustomRichMemo.SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer);  
var
  range : Tcharrange;
begin
  SendMessage(ACustomEdit.Handle, EM_EXGETSEL, 0, LPARAM(@range));
  range.cpMax := range.cpMin + NewLength;
  SendMessage(ACustomEdit.Handle, EM_EXSETSEL, 0, LPARAM(@range));
  InvalidateRect(ACustomEdit.Handle, nil, false);
end;

class procedure TWin32WSCustomRichMemo.CutToClipboard(const AWinControl: TWinControl);  
begin
  SendMessage(AWinControl.Handle, WM_CUT, 0,0);  
end;

class procedure TWin32WSCustomRichMemo.CopyToClipboard(const AWinControl: TWinControl);  
begin
  SendMessage(AWinControl.Handle, WM_COPY, 0,0);  
end;

class procedure TWin32WSCustomRichMemo.PasteFromClipboard(const AWinControl: TWinControl);  
begin
  SendMessage(AWinControl.Handle, WM_PASTE, 0,0);
end;

class function TWin32WSCustomRichMemo.CreateHandle(const AWinControl: TWinControl;  
  const AParams: TCreateParams): HWND;  
var
  Params      : TCreateWindowExParams;
  RichClass   : AnsiString;
  ACustomMemo : TCustomMemo;
  eventmask   : LPARAM;
begin
  InitRichEdit;
  RichClass := GetRichEditClass;
  if RichClass = '' then begin
    Result := 0;
    Exit;
  end;

  // general initialization of Params

  // if you're using 0.9.28.2 lazarus, uncomment the line,
  // PrepareCreateWindow(AWinControl, Params);
  // and comment the following like (it's for 0.9.30 compatiblity):
  PrepareCreateWindow(AWinControl, AParams, Params);

  Params.SubClassWndProc := @RichEditProc;

  // customization of Params
  ACustomMemo := TCustomMemo(AWinControl);
  with Params do
  begin
    Flags := Flags or ES_AUTOVSCROLL or ES_MULTILINE or ES_WANTRETURN;

    if ACustomMemo.ReadOnly then
      Flags := Flags or ES_READONLY;
    Flags := Flags or AlignmentToEditFlags[ACustomMemo.Alignment];
    case ACustomMemo.ScrollBars of
      ssHorizontal, ssAutoHorizontal:
        Flags := Flags or WS_HSCROLL;
      ssVertical, ssAutoVertical:
        Flags := Flags or WS_VSCROLL;
      ssBoth, ssAutoBoth:
        Flags := Flags or WS_HSCROLL or WS_VSCROLL;
    end;
    if ACustomMemo.WordWrap then
      Flags := Flags and not WS_HSCROLL
    else
      Flags := Flags or ES_AUTOHSCROLL;
      
    if ACustomMemo.BorderStyle=bsSingle then
      FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
      
    pClassName := @RichClass[1];
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);

  eventmask := SendMessage(AWinControl.Handle, EM_GETEVENTMASK, 0, 0);
  eventmask := eventmask or ENM_SELCHANGE;
  SendMessage(AWinControl.Handle, EM_SETEVENTMASK, 0, eventmask);

  // memo is not a transparent control -> no need for parentpainting
  PArams.WindowInfo^.ParentMsgHandler := @RichEditNotifyProc;
  Params.WindowInfo^.needParentPaint := false;
  Result := Params.Window;
end;

class procedure TWin32WSCustomRichMemo.SetTextAttributes(const AWinControl: TWinControl; 
  TextStart, TextLen: Integer; const Params: TIntFontParams);  
var
  OrigStart : Integer;
  OrigLen   : Integer;
  NeedLock  : Boolean;
  eventmask : Integer;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;

  eventmask := RichEditManager.SetEventMask(AWinControl.Handle, 0);
  RichEditManager.GetSelection(AWinControl.Handle, OrigStart, OrigLen);
  
  NeedLock := (OrigStart <> TextStart) or (OrigLen <> TextLen);
  if NeedLock then begin
    LockRedraw(AWinControl.Handle);
    RichEditManager.SetSelection(AWinControl.Handle, TextStart, TextLen);
    RichEditManager.SetSelectedTextStyle(AWinControl.Handle, Params );
    RichEditManager.SetSelection(AWinControl.Handle, OrigStart, OrigLen);
    UnlockRedraw(AWinControl.Handle);
  end else 
    RichEditManager.SetSelectedTextStyle(AWinControl.Handle, Params);

  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
end;

class function TWin32WSCustomRichMemo.GetTextAttributes(const AWinControl: TWinControl; 
  TextStart: Integer; var Params: TIntFontParams): Boolean;  
var
  OrigStart : Integer;
  OrigLen   : Integer;
  NeedLock  : Boolean;  
  eventmask : LongWord;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then begin
    Result := false;
    Exit;
  end;

  eventmask := RichEditManager.SetEventMask(AWinControl.Handle, 0);
  
  RichEditManager.GetSelection(AWinControl.Handle, OrigStart, OrigLen);
  
  NeedLock := (OrigStart <> TextStart);
  if NeedLock then begin
    LockRedraw(AWinControl.Handle);
    RichEditManager.SetSelection(AWinControl.Handle, TextStart, 1);
    Result := RichEditManager.GetSelectedTextStyle(AWinControl.Handle, Params );
    RichEditManager.SetSelection(AWinControl.Handle, OrigStart, OrigLen);
    UnlockRedraw(AWinControl.Handle, false);
  end else begin
    LockRedraw(AWinControl.Handle);
    Result := RichEditManager.GetSelectedTextStyle(AWinControl.Handle, Params);
    UnlockRedraw(AWinControl.Handle, false);
  end;

  RichEditManager.SetEventMask(AWinControl.Handle,eventmask);
end;


class procedure TWin32WSCustomRichMemo.SetHideSelection(
  const ACustomEdit: TCustomEdit; AHideSelection: Boolean);  
begin
  if not Assigned(RichEditManager) or not Assigned(ACustomEdit) then Exit;
  RichEditManager.SetHideSelection(ACustomEdit.Handle, AHideSelection);
end;

procedure InitScrollInfo(var info: TScrollInfo);
begin
  FillChar(info, sizeof(info), 0);
  info.cbSize := sizeof(info);
  info.fMask := SIF_ALL;
end;

class function TWin32WSCustomRichMemo.GetStyleRange(
  const AWinControl: TWinControl; TextStart: Integer; var RangeStart, 
  RangeLen: Integer): Boolean;  
var
  OrigStart : Integer;
  OrigLen   : Integer;
  hInfo     : TScrollInfo;
  vInfo     : TScrollInfo;
  hVisible  : Boolean;
  vVisible  : Boolean;
  eventmask : longword;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then begin
    Result := false;
    Exit;
  end;

  eventmask := RichEditManager.SetEventMask(AWinControl.Handle, 0);

  RichEditManager.GetSelection(AWinControl.Handle, OrigStart, OrigLen);
  LockRedraw(AWinControl.Handle);
  InitScrollInfo(hInfo);
  InitScrollInfo(vInfo);  
  hVisible:=GetScrollbarVisible(AWinControl.Handle, SB_Horz);
  vVisible:=GetScrollbarVisible(AWinControl.Handle, SB_Vert);
  GetScrollInfo(AWinControl.Handle, SB_Horz, hInfo);
  GetScrollInfo(AWinControl.Handle, SB_Vert, vInfo);
  
  RichEditManager.SetSelection(AWinControl.Handle, TextStart, 1);
  try
    Result := RichEditManager.GetStyleRange(AWinControl.Handle, TextStart, RangeStart, RangeLen);
  except
  end;
  
  if hVisible then SetScrollInfo(AWinControl.Handle, SB_Horz, hInfo, false);
  if vVisible then SetScrollInfo(AWinControl.Handle, SB_Vert, vInfo, false);
  RichEditManager.SetSelection(AWinControl.Handle, OrigStart, OrigLen);
  UnlockRedraw(AWinControl.Handle, false);
  
  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
end;

class function TWin32WSCustomRichMemo.LoadRichText(
  const AWinControl: TWinControl; Source: TStream): Boolean;  
begin
  Result := False;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  Result := RichEditManager.LoadRichText(AWinControl.Handle, Source);
end;

class function TWin32WSCustomRichMemo.SaveRichText(
  const AWinControl: TWinControl; Dst: TStream): Boolean;  
begin
  Result := false;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  Result := RichEditManager.SaveRichText(AWinControl.Handle, Dst);
end;

class function TWin32WSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer; var AAlign: TParaAlignment
  ): Boolean;
var
  para : PARAFORMAT2;
  eventmask: Integer;
begin
  Result:=false;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;

  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);

  RichEditManager.GetPara2(AWinControl.Handle, TextStart, para);
  case para.wAlignment of
    PFA_CENTER:  AAlign:=paCenter;
    PFA_RIGHT:   AAlign:=paRight;
    PFA_JUSTIFY: AAlign:=paJustify;
  else
    AAlign:=paLeft;
  end;
  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);

  Result:=true;
end;

class procedure TWin32WSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer; const AAlign: TIntParaAlignment);
var
  para : PARAFORMAT2;
  eventmask: Integer;
const
  WinPara : array [TIntParaAlignment] of word = (PFA_LEFT, PFA_RIGHT, PFA_CENTER, PFA_JUSTIFY);
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);

  FillChar(para, sizeof(para), 0);
  para.cbSize:=sizeof(para);
  para.dwMask:=PFM_ALIGNMENT;
  para.wAlignment:=WinPara[AAlign];
  RichEditManager.SetPara2(AWinControl.Handle, TextStart, TextLen, para);

  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
end;

class function TWin32WSCustomRichMemo.GetParaMetric(
  const AWinControl: TWinControl; TextStart: Integer;
  var AMetrics: TIntParaMetric): Boolean;
var
  para : PARAFORMAT2;
  eventmask: Integer;
begin
  Result:=false;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;

  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);

  RichEditManager.GetPara2(AWinControl.Handle, TextStart, para);

  AMetrics.FirstLine:=para.dxStartIndent/20;
  AMetrics.TailIndent:=para.dxRightIndent/20;
  AMetrics.HeadIndent:=(para.dxStartIndent+para.dxOffset)/20;
  AMetrics.SpaceAfter:=para.dySpaceAfter/20;
  AMetrics.SpaceBefore:=para.dySpaceBefore/20;
  AMetrics.LineSpacing:=para.dyLineSpacing*DefLineSpacing/20;

  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
end;

class procedure TWin32WSCustomRichMemo.SetParaMetric(
  const AWinControl: TWinControl; TextStart, TextLength: Integer;
  const AMetrics: TIntParaMetric);
var
  para : PARAFORMAT2;
  eventmask: Integer;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  FillChar(para, SizeOf(para), 0);

  para.cbSize:=sizeof(para);
  para.dwMask:=
     PFM_STARTINDENT or PFM_RIGHTINDENT
     or PFM_OFFSET
     or PFM_SPACEAFTER or PFM_SPACEBEFORE
     or PFM_LINESPACING;
  para.dxStartIndent:=round(AMetrics.FirstLine*20);
  para.dxRightIndent:=round(AMetrics.TailIndent*20);
  para.dxOffset:=round((AMetrics.HeadIndent-AMetrics.FirstLine)*20);
    //round(AMetrics.HeadIndent*20);
  para.dySpaceAfter:=round(AMetrics.SpaceAfter*20);
  para.dySpaceBefore:=round(AMetrics.SpaceBefore*20);
  if AMetrics.LineSpacing > 0 then begin
    para.dyLineSpacing:=round(AMetrics.LineSpacing/DefLineSpacing*20);
    para.bLineSpacingRule:=5; // always line spacing?
  end;

  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);
  RichEditManager.SetPara2(AWinControl.Handle, TextStart, TextLength, para);
  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
end;

class function TWin32WSCustomRichMemo.GetParaRange(const AWinControl: TWinControl;
  TextStart: Integer; var ParaRange: TParaRange): Boolean;
begin
  if not Assigned(AWinControl) then
    Result:=False
  else begin
    RichEditManager.GetParaRange(AWinControl.Handle, TextStart, ParaRange);
    Result:=true;
  end;
end;

const
  PFN_ARABIC   = 2;
  PFN_LCLETTER = 3;
  PFN_LCROMAN  = 4;
  PFN_UCLETTER = 5;
  PFN_UCROMAN  = 6;
  PFN_CUSTOM   = 7;

class function TWin32WSCustomRichMemo.GetParaNumbering(
  const AWinControl: TWinControl; TextStart: Integer;
  var ANumber: TIntParaNumbering): Boolean;
var
  para : PARAFORMAT2;
  eventmask: INteger;
begin
  Result:=False;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;

  InitParaNumbering(ANumber);
  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);
  RichEditManager.GetPara2(AWinControl.Handle, TextStart, para);
  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);

  case para.wNumbering of
    PFN_BULLET:   ANumber.Style:=pnBullet;
    PFN_ARABIC:   ANumber.Style:=pnNumber;
    PFN_LCLETTER: ANumber.Style:=pnLowLetter;
    PFN_LCROMAN:  ANumber.Style:=pnLowRoman;
    PFN_UCLETTER: ANumber.Style:=pnUpLetter;
    PFN_UCROMAN:  ANumber.Style:=pnUpRoman;
    PFN_CUSTOM:   begin
      ANumber.Style:=pnCustomChar;
      ANumber.CustomChar:=WideChar(para.wNumberingStart);
    end;
  else
    ANumber.Style:=pnNone;
  end;
  if para.wNumberingStyle or PFNS_PLAIN > 0 then
    ANumber.SepChar:=SepNone
  else if para.wNumberingStyle or PFNS_PERIOD > 0 then
    ANumber.SepChar:=SepDot
  else if (ANumber.Style<>pnNone) and ((para.wNumberingStyle and PFNS_SOMESEPCHAR)= 0) then
    ANumber.SepChar:=SepPar;
  ANumber.Indent:=para.wNumberingTab/20;
  Result:=true;
end;

class procedure TWin32WSCustomRichMemo.SetParaNumbering(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const ANumber: TIntParaNumbering);
var
  para : PARAFORMAT2;
  eventmask: Integer;
  numbstyle: Integer;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  FillChar(para, SizeOf(para), 0);

  para.cbSize:=sizeof(para);
  para.dwMask:=
     PFM_NUMBERING or PFM_NUMBERINGTAB;

  numbstyle:=0;
  case ANumber.SepChar of
    SepPar: numbstyle:=numbstyle or PFNS_PAREN;
    SepDot: numbstyle:=numbstyle or PFNS_PERIOD;
    SepNone: numbstyle:=numbstyle or PFNS_PLAIN;
  end;
  case ANumber.Style of
    pnNone:       para.wNumbering:=0;
    pnBullet:     para.wNumbering:=PFN_BULLET;
    pnNumber: begin
      para.wNumbering:=PFN_ARABIC;
      para.dwMask:=para.dwMask or PFM_NUMBERINGSTART;
      para.wNumberingStart:=ANumber.NumberStart;
      if ANumber.ForceNewNum then numbstyle:=numbstyle or PFNS_NEWNUMBER;
    end;
    pnLowLetter:  para.wNumbering:=PFN_LCLETTER;
    pnLowRoman:   para.wNumbering:=PFN_LCROMAN;
    pnUpLetter:   para.wNumbering:=PFN_UCLETTER;
    pnUpRoman:    para.wNumbering:=PFN_UCROMAN;
    pnCustomChar: begin
      para.wNumbering:=PFN_CUSTOM;
      para.wNumberingStart:=Word(ANumber.CustomChar);
      para.dwMask:=para.dwMask or PFM_NUMBERINGSTART;
    end;
  end;
  if numbstyle<> 0 then begin
    para.dwMask:=para.dwMask or PFM_NUMBERINGSTYLE;
    para.wNumberingStyle:=numbstyle;
  end;

  para.wNumberingTab:=round(ANumber.Indent*20);
  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);
  RichEditManager.SetPara2(AWinControl.Handle, TextStart, TextLen, para);
  RichEditManager.SetEventMask(AWinControl.Handle, eventmask)
end;

class procedure TWin32WSCustomRichMemo.InDelText(const AWinControl:TWinControl;
  const TextUTF8:String;DstStart,DstLen:Integer);
var
  eventmask : Integer;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);
  RichEditManager.SetText(AWinControl.Handle,UTF8Decode(TextUTF8),DstStart,DstLen);
  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
end;

class function TWin32WSCustomRichMemo.Search(const AWinControl: TWinControl;
  const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  Result:=RichEditManager.Find(AWinControl.Handle, UTF8Decode(ANiddle), SearchOpts);
end;

class procedure TWin32WSCustomRichMemo.SetZoomFactor(
  const AWinControl: TWinControl; AZoomFactor: Double);
var
  DN : WParam;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  DN := 1000;
  SendMessage( AWinControl.Handle, EM_SETZOOM, round(AZoomFactor * DN), DN);
end;

class function TWin32WSCustomRichMemo.InlineInsert(
  const AWinControl: TWinControl; ATextStart, ATextLength: Integer;
  const ASize: TSize; AHandler: TRichMemoInline;
  var wsObj: TRichMemoInlineWSObject): Boolean;
var
  hnd : THandle;
  rch : IRichEditOle;
  Fmt : FORMATETC;
  LockBytes: ILockBytes;
  ClientSite: IOleClientSite;
  Storage: IStorage;
  Image: IOleObject;
  c: TWin32Inline;
  Obj: TREOBJECT;
  sl, ss: Integer;
  eventmask: Integer;
const
  PointSize     = 72.0;
  RtfSizeToInch = 2.54 * 1000.0;
  SizeFactor    = 1 / PointSize * RtfSizeToInch;
begin
  Result:=False;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;

  hnd:=(AWinControl.Handle);

  RichEditManager.GetSelection(hnd, ss, sl);
  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);
  try
    RichEditManager.SetSelection(hnd, ATextStart, ATextLength);
    SendMessage(hnd, EM_GETOLEINTERFACE, 0, LPARAM(@rch));

    FillChar(Fmt, sizeoF(Fmt), 0);
    Fmt.dwAspect:=DVASPECT_CONTENT;
    Fmt.lindex:=-1;

    CreateILockBytesOnHGlobal(0, True, LockBytes);
    StgCreateDocfileOnILockBytes(LockBytes, STGM_SHARE_EXCLUSIVE or STGM_CREATE or STGM_READWRITE, 0, Storage);
    rch.GetClientSite(ClientSite);

    c:=TWin32Inline.Create;
    c.richMemo:=TCustomRichMemo(AWinControl);
    c.canvas:=TCanvas.Create;
    c.rminline:=AHandler;

    Image:=c;
    OleSetContainedObject(Image, True);

    FillChar(Obj, sizeof(Obj),0);
    Obj.cbStruct := SizeOf(Obj);
    Obj.cp := REO_CP_SELECTION;
    Image.GetUserClassID(Obj.clsid);
    Obj.poleobj := Image;
    Obj.pstg := Storage;
    Obj.polesite := ClientSite;
    Obj.dvaspect := DVASPECT_CONTENT;
    Obj.dwFlags := REO_OWNERDRAWSELECT;

    Obj.sizel.cx:=round(ASize.cx * SizeFactor);
    Obj.sizel.cy:=round(ASize.cy * SizeFactor);

    Result:= Succeeded(rch.InsertObject(obj));
    if Result then wsObj:=c;
  finally
    RichEditManager.SetSelection(hnd, ss, sl);
    RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
  end;
end;

class procedure TWin32WSCustomRichMemo.InlineInvalidate(
  const AWinControl: TWinControl; AHandler: TRichMemoInline;
  wsObj: TRichMemoInlineWSObject);
begin
  //inherited InlineInvalidate(AWinControl, AHandler, wsObj);
  if not Assigned(AHandler) or not Assigned(wsObj) or (not (wsObj is TWin32Inline)) then Exit;
  if not Assigned(TWin32Inline(wsObj).fSink) then Exit;
  TWin32Inline(wsObj).fSink.OnViewChange(DVASPECT_CONTENT, -1);
end;
 
end.

