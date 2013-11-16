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

unit Win32WSRichBox;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Controls, LCLType, StdCtrls, Graphics,
  Win32Proc, Win32Int, Win32WSControls,
  WSRichBox, RichEdit, RichBox;

type

  { TWin32WSCustomRichBox }
  TWin32WSCustomRichBox = class(TWSCustomRichBox)
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): HWND; override;
    //--
    //Funções de Fonte
    class function Font_GetCharset(const AWinControl: TWinControl): TFontCharset; override;
    class function Font_GetColor(const AWinControl: TWinControl): TColor; override;
    class function Font_GetName(const AWinControl: TWinControl): TFontName; override;
    class function Font_GetPitch(const AWinControl: TWinControl): TFontPitch; override;
    class function Font_GetProtected(const AWinControl: TWinControl): boolean; override;
    class function Font_GetSize(const AWinControl: TWinControl): integer; override;
    class function Font_GetStyle(const AWinControl: TWinControl): TFontStyles; override;
    //
    //Funções de Paragrafos
    class function Para_GetAlignment(const AWinControl: TWinControl): TAlignment; override;
    class function Para_GetFirstIndent(const AWinControl: TWinControl): Longint; override;
    class function Para_GetLeftIndent(const AWinControl: TWinControl): Longint; override;
    class function Para_GetRightIndent(const AWinControl: TWinControl): Longint; override;
    class function Para_GetNumbering(const AWinControl: TWinControl): TNumberingStyle; override;
    class function Para_GetTab(const AWinControl: TWinControl; Index: Byte): Longint; override;
    class function Para_GetTabCount(const AWinControl: TWinControl): Integer; override;
    //
    //Procedimentos de Fonte
    class procedure Font_SetCharset(const AWinControl: TWinControl; Value: TFontCharset); override;
    class procedure Font_SetColor(const AWinControl: TWinControl; Value: TColor); override;
    class procedure Font_SetName(const AWinControl: TWinControl; Value: TFontName); override;
    class procedure Font_SetPitch(const AWinControl: TWinControl; Value: TFontPitch); override;
    class procedure Font_SetProtected(const AWinControl: TWinControl; Value: boolean); override;
    class procedure Font_SetSize(const AWinControl: TWinControl; Value: integer); override;
    class procedure Font_SetStyle(const AWinControl: TWinControl; Value: TFontStyles); override;
    //
    //Procedimentos de Paragrafos
    class procedure Para_SetAlignment(const AWinControl: TWinControl; Value: TAlignment); override;
    class procedure Para_SetFirstIndent(const AWinControl: TWinControl; Value: Longint); override;
    class procedure Para_SetLeftIndent(const AWinControl: TWinControl; Value: Longint); override;
    class procedure Para_SetRightIndent(const AWinControl: TWinControl; Value: Longint); override;
    class procedure Para_SetNumbering(const AWinControl: TWinControl; Value: TNumberingStyle); override;
    class procedure Para_SetTab(const AWinControl: TWinControl; Index: Byte; Value: Longint); override;
    class procedure Para_SetTabCount(const AWinControl: TWinControl; Value: Integer); override;
    //--
    class procedure SaveToStream(const AWinControl: TWinControl; var Stream: TStream); override;
    class procedure LoadFromStream(const AWinControl: TWinControl; const Stream: TStream); override;

  end;

{Exceptional}
procedure InitFMT(var FMT: TCHARFORMAT2);
procedure F_GetAttributes(const Window: HWND; var FMT: TCHARFORMAT2);
procedure F_SetAttributes(const Window: HWND; var FMT: TCHARFORMAT2);
//
procedure InitPARAFMT(var PARAFMT: TPARAFORMAT2);
procedure P_GetAttributes(const Window: HWND; var PARAFMT: TPARAFORMAT2);
procedure P_SetAttributes(const Window: HWND; var PARAFMT: TPARAFORMAT2);
//
function StreamSave(dwCookie: PDWORD; pbBuff: LPBYTE; cb: LONG; var pcb: LONG): DWORD; stdcall;
function StreamLoad(dwCookie: PDWORD; pbBuff: LPBYTE; cb: LONG; var pcb: LONG): DWORD; stdcall;
{...}
implementation

var
  RichEditLibrary_HWND: THandle;

const
  RichEditClass: ansistring = '';

function RichEditProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
  LParam: Windows.LParam): LResult; stdcall;
begin
  if Msg = WM_PAINT then
  begin
    Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
  end
  else
    Result := WindowProc(Window, Msg, WParam, LParam);
end;

{Exceptional}
procedure InitFMT(var FMT: TCHARFORMAT2);
begin
  FillChar(FMT, SizeOf(TCHARFORMAT2), 0);
  FMT.cbSize := SizeOf(TCHARFORMAT2);
end;

procedure F_GetAttributes(const Window: HWND; var FMT: TCHARFORMAT2);
begin
  InitFMT(FMT);
  SendMessage(Window, EM_GETCHARFORMAT, SCF_SELECTION, LPARAM(@FMT));
end;

procedure F_SetAttributes(const Window: HWND; var FMT: TCHARFORMAT2);
begin
  SendMessage(Window, EM_SETCHARFORMAT, SCF_SELECTION, LPARAM(@FMT));
end;
//
procedure InitPARAFMT(var PARAFMT: TPARAFORMAT2);
begin
  FillChar(PARAFMT, SizeOf(TPARAFORMAT2), 0);
  PARAFMT.cbSize := SizeOf(TPARAFORMAT2);
end;

procedure P_GetAttributes(const Window: HWND; var PARAFMT: TPARAFORMAT2);
begin
  InitPARAFMT(PARAFMT);
  SendMessage(Window, EM_GETPARAFORMAT, 0, LPARAM(@PARAFMT));
end;

procedure P_SetAttributes(const Window: HWND; var PARAFMT: TPARAFORMAT2);
begin
  SendMessage(Window, EM_SETPARAFORMAT, 0, LPARAM(@PARAFMT));
end;

function StreamSave(dwCookie: PDWORD; pbBuff: LPBYTE; cb: LONG; var pcb: LONG
  ): DWORD; stdcall;
var
  Stream: TStream;
begin
  try
    Stream := TStream(PtrInt(dwCookie^));
    pcb := Stream.Write(pbBuff^, cb);
    Result := 0;
  except
    Result := 1;
  end;
end;

function StreamLoad(dwCookie: PDWORD; pbBuff: LPBYTE; cb: LONG; var pcb: LONG
  ): DWORD; stdcall;
var
  s: TStream;
begin
  try
    s := TStream(PtrInt(dwCookie^));
    pcb := s.Read(pbBuff^, cb);
    Result := 0;
  except
    Result := 1;
  end;
end;

{ TWin32WSCustomRichBox }

class function TWin32WSCustomRichBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
const
  AlignmentToEditFlags: array[TAlignment] of DWord =
    (
    { taLeftJustify  } ES_LEFT,
    { taRightJustify } ES_RIGHT,
    { taCenter       } ES_CENTER
    );
var
  Params: TCreateWindowExParams;
begin
  //--
  if (RichEditLibrary_HWND = 0) then
  begin
    RichEditLibrary_HWND := LoadLibrary('Msftedit.dll');
    if (RichEditLibrary_HWND <> 0) and (RichEditLibrary_HWND <>
      HINSTANCE_ERROR) then
      RichEditClass := 'RichEdit50W'
    else
      RichEditLibrary_HWND := 0;
  end;
  //--
  if (RichEditLibrary_HWND = 0) then
  begin
    RichEditLibrary_HWND := LoadLibrary('RICHED20.DLL');
    if (RichEditLibrary_HWND <> 0) and (RichEditLibrary_HWND <>
      HINSTANCE_ERROR) then
    begin
      if UnicodeEnabledOS then
        RichEditClass := 'RichEdit20W'
      else
        RichEditClass := 'RichEdit20A';
    end
    else
      RichEditLibrary_HWND := 0;
  end;
  //--
  if (RichEditLibrary_HWND = 0) then
  begin
    RichEditLibrary_HWND := LoadLibrary('RICHED32.DLL');
    if (RichEditLibrary_HWND <> 0) and (RichEditLibrary_HWND <>
      HINSTANCE_ERROR) then
      RichEditClass := 'RICHEDIT'
    else
      RichEditLibrary_HWND := 0;
  end;
  //--
  if (RichEditLibrary_HWND = 0) then
  begin
    //'Aqui devo abortar a criação do componete!!!!'
  end;
  //--
  //--
  PrepareCreateWindow(AWinControl, AParams, Params);
  //--

  with Params do
  begin
    SubClassWndProc := @RichEditProc;

    Flags := Flags or ES_AUTOVSCROLL or ES_MULTILINE or ES_WANTRETURN;

    if TCustomRichBox(AWinControl).ReadOnly then
      Flags := Flags or ES_READONLY;
    Flags := Flags or AlignmentToEditFlags[TCustomRichBox(AWinControl).Alignment];
    case TCustomRichBox(AWinControl).ScrollBars of
      ssHorizontal, ssAutoHorizontal:
        Flags := Flags or WS_HSCROLL;
      ssVertical, ssAutoVertical:
        Flags := Flags or WS_VSCROLL;
      ssBoth, ssAutoBoth:
        Flags := Flags or WS_HSCROLL or WS_VSCROLL;
    end;
    if TCustomRichBox(AWinControl).WordWrap then
      Flags := Flags and not WS_HSCROLL
    else
      Flags := Flags or ES_AUTOHSCROLL;

    if TCustomRichBox(AWinControl).BorderStyle = bsSingle then
      FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;

    pClassName := @RichEditClass[1];
    WindowTitle := StrCaption;
  end;
  //--
  FinishCreateWindow(AWinControl, Params, False);

  Params.WindowInfo^.needParentPaint := False;
  Result := Params.Window;

end;

class function TWin32WSCustomRichBox.Font_GetCharset(
  const AWinControl: TWinControl): TFontCharset;
var
  FMT: TCHARFORMAT2;
begin
  F_GetAttributes(AWinControl.Handle, FMT);
  Result := FMT.bCharset;
end;

class function TWin32WSCustomRichBox.Font_GetColor(
  const AWinControl: TWinControl): TColor;
var
  FMT: TCHARFORMAT2;
begin
  F_GetAttributes(AWinControl.Handle, FMT);
  with FMT do
    if (dwEffects and CFE_AUTOCOLOR) <> 0 then
      Result := clWindowText
    else
      Result := crTextColor;
end;

class function TWin32WSCustomRichBox.Font_GetName(
  const AWinControl: TWinControl): TFontName;
var
  FMT: TCHARFORMAT2;
begin
  F_GetAttributes(AWinControl.Handle, FMT);
  Result := FMT.szFaceName;

end;

class function TWin32WSCustomRichBox.Font_GetPitch(
  const AWinControl: TWinControl): TFontPitch;
var
  FMT: TCHARFORMAT2;
begin
  F_GetAttributes(AWinControl.Handle, FMT);
  case (FMT.bPitchAndFamily and $03) of
    DEFAULT_PITCH: Result := fpDefault;
    VARIABLE_PITCH: Result := fpVariable;
    FIXED_PITCH: Result := fpFixed;
    else
      Result := fpDefault;
  end;
end;

class function TWin32WSCustomRichBox.Font_GetProtected(
  const AWinControl: TWinControl): boolean;
var
  FMT: TCHARFORMAT2;
begin
  F_GetAttributes(AWinControl.Handle, FMT);
  with FMT do
    if (dwEffects and CFE_PROTECTED) <> 0 then
      Result := True
    else
      Result := False;
end;

class function TWin32WSCustomRichBox.Font_GetSize(
  const AWinControl: TWinControl): integer;
var
  FMT: TCHARFORMAT2;
begin
  F_GetAttributes(AWinControl.Handle, FMT);
  Result := FMT.yHeight div 20;
end;

class function TWin32WSCustomRichBox.Font_GetStyle(
  const AWinControl: TWinControl): TFontStyles;
var
  FMT: TCHARFORMAT2;
begin
  Result := [];
  F_GetAttributes(AWinControl.Handle, FMT);
  with FMT do
  begin
    if (dwEffects and CFE_BOLD) <> 0 then
      Include(Result, fsBold);
    if (dwEffects and CFE_ITALIC) <> 0 then
      Include(Result, fsItalic);
    if (dwEffects and CFE_UNDERLINE) <> 0 then
      Include(Result, fsUnderline);
    if (dwEffects and CFE_STRIKEOUT) <> 0 then
      Include(Result, fsStrikeOut);
  end;
end;

class function TWin32WSCustomRichBox.Para_GetAlignment(
  const AWinControl: TWinControl): TAlignment;
var
  Paragraph: TPARAFORMAT2;
begin
  P_GetAttributes(AWinControl.Handle, Paragraph);
  Result := TAlignment(Paragraph.wAlignment - 1);
end;

class function TWin32WSCustomRichBox.Para_GetFirstIndent(
  const AWinControl: TWinControl): Longint;
var
  Paragraph: TPARAFORMAT2;
begin
  P_GetAttributes(AWinControl.Handle, Paragraph);
  Result := Paragraph.dxOffset div 20;
end;

class function TWin32WSCustomRichBox.Para_GetLeftIndent(
  const AWinControl: TWinControl): Longint;
var
  Paragraph: TPARAFORMAT2;
begin
  P_GetAttributes(AWinControl.Handle, Paragraph);
  Result := Paragraph.dxStartIndent div 20
end;

class function TWin32WSCustomRichBox.Para_GetRightIndent(
  const AWinControl: TWinControl): Longint;
var
  Paragraph: TPARAFORMAT2;
begin
  P_GetAttributes(AWinControl.Handle, Paragraph);
  Result := Paragraph.dxRightIndent div 20;
end;

class function TWin32WSCustomRichBox.Para_GetNumbering(
  const AWinControl: TWinControl): TNumberingStyle;
var
  Paragraph: TPARAFORMAT2;
begin
  P_GetAttributes(AWinControl.Handle, Paragraph);
  Result := TNumberingStyle(Paragraph.wNumbering);
end;

class function TWin32WSCustomRichBox.Para_GetTab(
  const AWinControl: TWinControl; Index: Byte): Longint;
var
  Paragraph: TPARAFORMAT2;
begin
  P_GetAttributes(AWinControl.Handle, Paragraph);
  Result := Paragraph.rgxTabs[Index] div 20;
end;

class function TWin32WSCustomRichBox.Para_GetTabCount(
  const AWinControl: TWinControl): Integer;
var
  Paragraph: TPARAFORMAT2;
begin
  P_GetAttributes(AWinControl.Handle, Paragraph);
  Result := Paragraph.cTabCount;
end;

class procedure TWin32WSCustomRichBox.Font_SetCharset(const AWinControl: TWinControl;
  Value: TFontCharset);
var
  FMT: TCHARFORMAT2;
begin
  InitFMT(FMT);
  with FMT do
  begin
    dwMask := CFM_CHARSET;
    bCharSet := Value;
  end;
  F_SetAttributes(AWinControl.Handle, FMT);
end;

class procedure TWin32WSCustomRichBox.Font_SetColor(const AWinControl: TWinControl;
  Value: TColor);
var
  FMT: TCHARFORMAT2;
begin
  InitFMT(FMT);
  with FMT do
  begin
    dwMask := CFM_COLOR;
    if Value = clWindowText then
      dwEffects := CFE_AUTOCOLOR
    else
      crTextColor := ColorToRGB(Value);
  end;
  F_SetAttributes(AWinControl.Handle, FMT);
end;

class procedure TWin32WSCustomRichBox.Font_SetName(const AWinControl: TWinControl;
  Value: TFontName);
var
  FMT: TCHARFORMAT2;
begin
  InitFMT(FMT);
  with FMT do
  begin
    dwMask := CFM_FACE;
    StrPLCopy(szFaceName, Value, SizeOf(szFaceName));
  end;
  F_SetAttributes(AWinControl.Handle, FMT);
end;

class procedure TWin32WSCustomRichBox.Font_SetPitch(const AWinControl: TWinControl;
  Value: TFontPitch);
var
  FMT: TCHARFORMAT2;
begin
  InitFMT(FMT);
  with FMT do
  begin
    case Value of
      fpVariable: FMT.bPitchAndFamily := VARIABLE_PITCH;
      fpFixed: FMT.bPitchAndFamily := FIXED_PITCH;
      else
        FMT.bPitchAndFamily := DEFAULT_PITCH;
    end;
  end;
  F_SetAttributes(AWinControl.Handle, FMT);
end;

class procedure TWin32WSCustomRichBox.Font_SetProtected(const AWinControl: TWinControl;
  Value: boolean);
var
  FMT: TCHARFORMAT2;
begin
  InitFMT(FMT);
  with FMT do
  begin
    dwMask := CFM_PROTECTED;
    if Value then
      dwEffects := CFE_PROTECTED;
  end;
  F_SetAttributes(AWinControl.Handle, FMT);
end;

class procedure TWin32WSCustomRichBox.Font_SetSize(const AWinControl: TWinControl;
  Value: integer);
var
  FMT: TCHARFORMAT2;
begin
  InitFMT(FMT);
  with FMT do
  begin
    dwMask := integer(CFM_SIZE);
    yHeight := Value * 20;
  end;
  F_SetAttributes(AWinControl.Handle, FMT);
end;

class procedure TWin32WSCustomRichBox.Font_SetStyle(const AWinControl: TWinControl;
  Value: TFontStyles);
var
  FMT: TCHARFORMAT2;
begin
  InitFMT(FMT);
  with FMT do
  begin
    dwMask := CFM_BOLD or CFM_ITALIC or CFM_UNDERLINE or CFM_STRIKEOUT;
    if fsBold in Value then
      dwEffects := dwEffects or CFE_BOLD;
    if fsItalic in Value then
      dwEffects := dwEffects or CFE_ITALIC;
    if fsUnderline in Value then
      dwEffects := dwEffects or CFE_UNDERLINE;
    if fsStrikeOut in Value then
      dwEffects := dwEffects or CFE_STRIKEOUT;
  end;
  F_SetAttributes(AWinControl.Handle, FMT);
end;

class procedure TWin32WSCustomRichBox.Para_SetAlignment(
  const AWinControl: TWinControl; Value: TAlignment);
var
  Paragraph: TPARAFORMAT2;
begin
  InitPARAFMT(Paragraph);
  with Paragraph do
  begin
    dwMask := PFM_ALIGNMENT;
    wAlignment := Ord(Value) + 1;
  end;
  P_SetAttributes(AWinControl.Handle, Paragraph);
end;

class procedure TWin32WSCustomRichBox.Para_SetFirstIndent(
  const AWinControl: TWinControl; Value: Longint);
var
  Paragraph: TPARAFORMAT2;
begin
  InitPARAFMT(Paragraph);
  with Paragraph do
  begin
    dwMask := PFM_OFFSET;
    dxOffset := Value * 20;
  end;
  P_SetAttributes(AWinControl.Handle, Paragraph);
end;

class procedure TWin32WSCustomRichBox.Para_SetLeftIndent(
  const AWinControl: TWinControl; Value: Longint);
var
  Paragraph: TPARAFORMAT2;
begin
  InitPARAFMT(Paragraph);
  with Paragraph do
  begin
    dwMask := PFM_STARTINDENT;
    dxStartIndent := Value * 20;
  end;
  P_SetAttributes(AWinControl.Handle, Paragraph);
end;

class procedure TWin32WSCustomRichBox.Para_SetRightIndent(
  const AWinControl: TWinControl; Value: Longint);
var
  Paragraph: TPARAFORMAT2;
begin
  InitPARAFMT(Paragraph);
  with Paragraph do
  begin
    dwMask := PFM_RIGHTINDENT;
    dxRightIndent := Value * 20;
  end;
  P_SetAttributes(AWinControl.Handle, Paragraph);
end;

class procedure TWin32WSCustomRichBox.Para_SetNumbering(
  const AWinControl: TWinControl; Value: TNumberingStyle);
var
  Paragraph: TPARAFORMAT2;
begin
  case Value of
    nsBullets: if TWin32WSCustomRichBox.Para_GetLeftIndent(AWinControl) < 10 then
                 TWin32WSCustomRichBox.Para_SetLeftIndent(AWinControl, 10);
    nsNone: TWin32WSCustomRichBox.Para_SetLeftIndent(AWinControl, 0);
  end;
  InitPARAFMT(Paragraph);
  with Paragraph do
  begin
    dwMask := PFM_NUMBERING;
    wNumbering := Ord(Value);
  end;
  P_SetAttributes(AWinControl.Handle, Paragraph);
end;

class procedure TWin32WSCustomRichBox.Para_SetTab(
  const AWinControl: TWinControl; Index: Byte; Value: Longint);
var
  Paragraph: TPARAFORMAT2;
begin
  InitPARAFMT(Paragraph);
  with Paragraph do
  begin
    rgxTabs[Index] := Value * 20;
    dwMask := PFM_TABSTOPS;
    if cTabCount < Index then cTabCount := Index;
    P_SetAttributes(AWinControl.Handle, Paragraph);
  end;
end;

class procedure TWin32WSCustomRichBox.Para_SetTabCount(
  const AWinControl: TWinControl; Value: Integer);
var
  Paragraph: TPARAFORMAT2;
begin
  P_GetAttributes(AWinControl.Handle, Paragraph);
  with Paragraph do
  begin
    dwMask := PFM_TABSTOPS;
    cTabCount := Value;
    P_SetAttributes(AWinControl.Handle, Paragraph);
  end;
end;

class procedure TWin32WSCustomRichBox.SaveToStream(
  const AWinControl: TWinControl; var Stream: TStream);
var
  EditStream_: TEditStream;
  StrType: integer;
begin
  EditStream_.dwCookie := longint(Pointer(@Stream));
  EditStream_.pfnCallback := @StreamSave;
  EditStream_.dwError := 0;

  if TCustomRichBox(AWinControl).PlainText then
    StrType := SF_TEXT
  else
    StrType := SF_RTF;

  SendMessage(AWinControl.Handle, EM_STREAMOUT, StrType, longint(@EditStream_));
end;

class procedure TWin32WSCustomRichBox.LoadFromStream(
  const AWinControl: TWinControl; const Stream: TStream);
var
  EditStream_: TEditStream;
  StrType: integer;
begin
  EditStream_.dwCookie := longint(Pointer(@Stream));
  EditStream_.pfnCallback := @StreamLoad;
  EditStream_.dwError := 0;

  if TCustomRichBox(AWinControl).PlainText then
    StrType := SF_TEXT
  else
    StrType := SF_RTF;

  SendMessage(AWinControl.Handle, EM_STREAMIN, StrType, LPARAM(@EditStream_));

end;
initialization



finalization

if (RichEditLibrary_HWND <> 0) then
    FreeLibrary(RichEditLibrary_HWND);
end.

