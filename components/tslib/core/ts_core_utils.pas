{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit ts_Core_Utils;

{$mode delphi}

//*****************************************************************************

{
  TODO:
    - Make a test application or test unit to test each routine.
}

interface

uses
  Graphics, SysUtils, Classes, Controls, ExtCtrls, Forms, Menus, TypInfo,
  StdCtrls, Character, Math,

  LCLType,
{$ifdef windows}
  Windows,
{$endif}
  DB;

//=============================================================================

type
  TVarRecArray = array of TVarRec;

const
  AnsiWhitespace = [' '];

type
  TShellLink = record
    Filename    : string;
    Description : string;
    ShortcutTo  : string;
    Parameters  : string;
    WorkingDir  : string;
    IconPath    : string;
    IconIndex   : Integer;
    ShowCommand : TWindowState;
    HotKey      : Integer;
  end;

//=============================================================================

{ Sets DoubleBuffered property for all TWinControl instances owned by the given
  component. }

procedure SetDoubleBuffered(AOwner: TComponent; AEnable: Boolean = True);

procedure CloneComponent(AFrom: TComponent; ATo: TComponent);

{$ifdef windows}
function VirtualKeyToChar(AKey : Word) : string;
{$endif}

function GetFullName(AComponent: TComponent) : string;

// string manipulation routines

procedure StrToStrings(const AString    : string;
                             AList      : TStrings;
                             ASeparator : Char);

function Unformat(const ASource,
                        APattern : string;
                  const AArgs    : array of const): Integer;

function Like(const ASource, ATemplate: string): Boolean;

{ original author: Vladimir Gaitanoff }
{ Returns a number of words delimited with AWordDelims }
function WordCount(const AString     : string;
                   const AWordDelims : TSysCharSet = AnsiWhiteSpace) : Integer;

{ Returns a position of word number AIndex in the string AString }
function WordPosition(const AIndex      : Integer;
                      const AString     : string;
                      const AWordDelims : TSysCharSet = AnsiWhiteSpace) : Integer;

{ Returns a word number AIndex in the string AString }
function ExtractWord(const AIndex      : Integer;
                     const AString     : string;
                     const AWordDelims : TSysCharSet = AnsiWhiteSpace): string;

function URLEncode(const AString: string): string;

function URLDecode(const AString: string): string;

// string formatting routines

function FormatElapsedTime(ASeconds: Extended): string;

function FormatByteText(ABytes: Integer): string;

// windows utilities
{$ifdef windows}
function CreateGUIDString: string;
function CreateUniqueID: string;
function GetLocalUserName: string;
function GetLocalComputerName: string;
{$endif}

function GetParentDir(sPath : string) : string;

{$ifdef windows}
procedure CreateShellLink(ShellLink: TShellLink);
function ExploreFile(const AFileName: string): Boolean;
{$endif}

// FCL utilities

procedure ChangeOwner(AComponent, ANewOwner : TComponent);

procedure EnableControls(AControlContainer : TWinControl;
                         AEnabled          : Boolean = True);

procedure DisableControls(AControlContainer : TWinControl);

function GetTextWidth(const AText : string;
                            AFont : TFont): Integer;

function GetTextHeight(const AText : string;
                             AFont : TFont): Integer;

procedure OptimizeWidth(APanel: TPanel);

function CloneMenuItem(SourceItem: TMenuItem): TMenuItem;


 // Variants and TVarRec conversions

procedure VariantToVarRec(    AVariant     : Variant;
                          var AVarRecArray : TVarRecArray);
procedure ClearVarRec(var AVarRecArray : TVarRecArray);

function VarRecToVariant(const AVarRec : TVarRec): Variant;
function VarRecToString(const AVarRec : TVarRec): string;
function VarRecToOleVariant(const AVarRec : TVarRec): OleVariant;

function VarArrayElemCount(const AVarArray: Variant): Integer;
function VarIsValue(const V: Variant): Boolean;
function VarAsTypeDef(const AValue    : Variant;
                            AVarType  : TVarType;
                      const ADefValue : Variant) : Variant;


procedure OleVarFromVariant(var   AOleVariant : OleVariant;
                            const AVariant    : Variant);

function GetVariantTypeName(const AVariant : Variant): string;

function VariantCompare(AVariant1, AVariant2 : Variant) : Boolean;

function VariantTypeForFieldType(const AFieldType : TFieldType): Integer;
function FieldTypeForVariant(const AVariant : Variant) : TFieldType;
function ConvertValueToFieldType(const AVariant : Variant;
                                 const AField   : TField): Variant;
function ValueNeedsConversion(const AVarType   : Integer;
                              const AFieldType : TFieldType): Boolean;

function StringToVariant(AString : string): Variant;

function MixColors(C1, C2: TColor; W1: Integer): TColor;

function StringReplaceMultiple(const Source: AnsiString;
  const OldPatterns, NewPatterns: array of AnsiString;
  CaseSensitive: Boolean = True): AnsiString;

// Interface utility routines

function GetPIMTOffset(const I : IInterface): Integer;

// UI windows utils

{$ifdef windows}
{ Displays a size grip on a window. This only works for WIN32/WIN64 widget set.}

procedure SetWindowSizeGrip(hWnd: HWND; Enable: Boolean);

function TaskBarHeight: Integer;

function GetCommonPath(ASL: TStrings): string;

function IsFormCovered(AForm: TForm): Boolean;
{$endif}

function DrawHTML(const ARect: TRect; const ACanvas: TCanvas; const Text: string): Integer;

procedure SetCheckedState(ACheckBox : TCheckBox; ACheck : Boolean);


function SetToString(
        ATypeInfo    : PTypeInfo;
  const AValue;
        AQuoteValues : Boolean = True;
        ABrackets    : Boolean = True;
        ATrimChars   : Integer = -1
): string;

//*****************************************************************************

implementation

uses
{$ifdef windows}
  ActiveX, ShlObj, Registry,
{$endif}
  Variants, ActnList;

//=============================================================================

resourcestring
  SNoCorrespondingFieldType = 'No corresponding fieldtype found for Variant ' +
                              'with value %s';

{$ifdef windows}
// code used by SetWindowSizeGrip

const
  SizeGripProp = 'SizeGrip';

type
  TWndProc = function(hWnd: hWnd; Msg: UINT; wParam: wParam; lParam: lParam)
    : LRESULT; stdcall;
  PGripInfo = ^TGripInfo;

  TGripInfo = record
    OldWndProc : TWndProc;
    Enabled    : Boolean;
    GripRect   : TRect;
  end;

{
  Code taken from SizeGripHWND.pas:
  Copyright (C) 2005, 2006 Volker Siebert <flocke@vssd.de>
  Alle Rechte vorbehalten.

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.
}
function SizeGripWndProc(hWnd: hWnd; Msg: UINT; wParam: wParam; lParam: lParam)
  : LRESULT; stdcall;
var
  Info : PGripInfo;
  dc   : HDC;
  pt   : TPoint;

  // Invalidate the current grip rectangle
  procedure InvalidateGrip;
  begin
    with Info^ do
      if (GripRect.Right > GripRect.Left)
        and (GripRect.Bottom > GripRect.Top) then
        InvalidateRect(hWnd, @GripRect, true);
  end;

  // Update (and invalidate) the current grip rectangle
  procedure UpdateGrip;
  begin
    with Info^ do
    begin
      GetClientRect(hWnd, GripRect);
      GripRect.Left := GripRect.Right - GetSystemMetrics(SM_CXHSCROLL);
      GripRect.Top := GripRect.Bottom - GetSystemMetrics(SM_CYVSCROLL);
    end;
    InvalidateGrip;
  end;

  function CallOld: LRESULT;
  begin
    Result := CallWindowProc(@Info^.OldWndProc, hWnd, Msg, wParam, lParam);
  end;

begin
  Info := PGripInfo(GetProp(hWnd, SizeGripProp));
  if Info = nil then
    Result := DefWindowProc(hWnd, Msg, wParam, lParam)
  else if not Info^.Enabled then
    Result := CallOld
  else
  begin
    case Msg of
      WM_NCDESTROY:
      begin
        Result := CallOld;
        SetWindowLong(hWnd, GWL_WNDPROC, LongInt(@Info^.OldWndProc));
        RemoveProp(hWnd, SizeGripProp);
        Dispose(Info);
      end;

      WM_PAINT:
      begin
        Result := CallOld;
        if wParam = 0 then
        begin
          dc := GetDC(hWnd);
          DrawFrameControl(dc, Info^.GripRect, DFC_SCROLL,
            DFCS_SCROLLSIZEGRIP);
          ReleaseDC(hWnd, dc);
        end;
      end;

      WM_NCHITTEST:
      begin
        pt.x := TSmallPoint(Integer(lParam)).x;
        pt.y := TSmallPoint(Integer(lParam)).y;
        ScreenToClient(hWnd, pt);
        if PtInRect(Info^.GripRect, pt) then
          Result := HTBOTTOMRIGHT
        else
          Result := CallOld;
      end;

      WM_SIZE:
      begin
        InvalidateGrip;
        Result := CallOld;
        UpdateGrip;
      end;

    else
      Result := CallOld;
    end;
  end;
end;

{ Note that SetWindowSizeGrip(..., false) does not really remove the hook -
  it just sets "Enabled" to false. The hook plus all data is removed when
  the window is destroyed.
}
procedure SetWindowSizeGrip(hWnd: hWnd; Enable: boolean);
var
  Info: PGripInfo;
begin
  Info := PGripInfo(GetProp(hWnd, SizeGripProp));
  if (Info = nil) and Enable then
  begin
    New(Info);
    FillChar(Info^, SizeOf(TGripInfo), 0);

    with Info^ do
    begin
      Info^.OldWndProc := TWndProc(Pointer(GetWindowLong(hWnd, GWL_WNDPROC)));

      GetClientRect(hWnd, GripRect);
      GripRect.Left := GripRect.Right - GetSystemMetrics(SM_CXHSCROLL);
      GripRect.Top := GripRect.Bottom - GetSystemMetrics(SM_CYVSCROLL);
    end;

    SetProp(hWnd, SizeGripProp, Cardinal(Info));
    SetWindowLong(hWnd, GWL_WNDPROC, LongInt(@SizeGripWndProc));
  end;

  if Info <> nil then
    if Enable <> Info^.Enabled then
      with Info^ do
      begin
        Enabled := Enable;
        if (GripRect.Right > GripRect.Left) and
          (GripRect.Bottom > GripRect.Top) then
          InvalidateRect(hWnd, @GripRect, true);
      end;
end;
{$endif}

function GetCommonPath(ASL: TStrings): string;
var
  I        : Integer;
  NextPath : string;
begin
  if not Assigned(ASL) or (ASL.Count = 0) then
    Exit;

  Result := ExtractFilePath(ASL[0]);
  I := 1;
  while I < ASL.Count do begin
    NextPath :=  ASL[I];
    while Copy(NextPath, 1, Length(Result)) <> Result do
      Result := ExtractFilePath(ExtractFileDir(Result));
    Inc(I);
  end;
end;

procedure SetDoubleBuffered(AOwner: TComponent; AEnable: Boolean);
var
  I : Integer;
begin
 for I := 0 to AOwner.ComponentCount - 1 do
    if AOwner.Components[I] is TWinControl then
      TWinControl(AOwner.Components[I]).DoubleBuffered := AEnable;
end;

{ Based on code of Xavier Pacheco. Fixed name assignment of ATo and removed
  assignment of Parent property.

   This procedure clones the published properties of AFrom and writes them to
   ATo.
   AFrom and ATo must be of the same type.  Use it for components that do
   not have an Assign method.

   AOwner is the owner of the cloned component. If AOwner is nil, the owner of
   the source component is used.
}

procedure CloneComponent(AFrom: TComponent; ATo: TComponent);
var
  MS : TMemoryStream;
  S1 : string;
  S2 : string;
begin
  if AFrom.ClassType <> ATo.ClassType then
    raise EComponentError.Create('Object types are incompatible');

  MS := TMemoryStream.Create; // Create the memory stream.
  with MS do try
    S1 := AFrom.Name;
    S2 := ATo.Name;
    AFrom.Name := '';
    WriteComponent(AFrom);        // Write C1 properties to stream
    AFrom.Name := S1;
    Seek(0, soFromBeginning);  // Position to beginning of stream.
    ReadComponent(ATo);         // read properties from stream into C2
    ATo.Name := S2;
  finally
    Free;                      // IAC, free stream.
  end;
end;

{ Returns fully qualified component instance name. The name is preceded by its
  owner name(s) (seperated by dots). }

function GetFullName(AComponent: TComponent) : string;
begin
  Result := AComponent.Name;
  while Assigned(AComponent.Owner) do
  begin
    if AComponent.Owner.Name <> '' then
      Result := AComponent.Owner.Name + '.' + Result;
    AComponent := AComponent.Owner;
  end;
end;

procedure ClearVarRec(var AVarRecArray: TVarRecArray);
var
  I : Integer;
begin
  for I := 0 to Length(AVarRecArray) - 1 do
    if AVarRecArray[I].VType in [vtExtended, vtString, vtVariant, vtInt64] then
      Dispose(AVarRecArray[I].VExtended);
  Finalize(AVarRecArray);
end;

function ConvertValueToFieldType(const AVariant: Variant;
  const AField: TField): Variant;
begin
  if (AField <> nil) and ValueNeedsConversion(VarType(AVariant), AField.DataType) then
    Result := VarAsType(AVariant, VariantTypeForFieldType(AField.DataType))
  else
   Result := AVariant;
end;

{
  Returns True if the Variant represents a value of a primitive type.

  Following variant values will return False:
    - Null
    - Unassigned
    - EmptyParam
    - varError
    - empty strings (because they are the same as Unassigned)
}

function VarIsValue(const V: Variant): Boolean;
begin
  Result := (not VarIsNull(V)) and (not VarIsEmpty(V)) and
            (not VarIsEmptyParam(V)) and (not VarIsError(V)) and
            not (VarIsStr(V) and (V = ''));
end;

function FieldTypeForVariant(const AVariant: Variant): TFieldType;
begin
  case VarType(AVariant) and varTypeMask of
   //varEmpty:    Result := ftVariant;
   varSmallint: Result := ftSmallint;
   varInteger:  Result := ftInteger;
   varSingle:   Result := ftFloat;
   varDouble:   Result := ftFloat;
   varCurrency: Result := ftCurrency;
   varDate:     Result := ftDateTime;
   varOleStr:   Result := ftString;
   varBoolean:  Result := ftBoolean;
   varString:   Result := ftString;
   else
     raise Exception.CreateFmt(SNoCorrespondingFieldType, [AVariant]);
  end;
end;

{$ifdef windows}
function GetLocalComputerName: string;
var
  Count : DWORD;
  S     : string;
begin
  Count := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(S, Count);
  if GetComputerName(PChar(S), Count) then
    SetLength(S, StrLen(PChar(S)))
  else
    S := '';
  Result := S;
end;

function GetLocalUserName: string;
var
  Count : DWORD;
  S     : string;
begin
  Count := 256 + 1; // UNLEN + 1
  // set buffer size to 256 + 2 characters
  SetLength(S, Count);
  if GetUserName(PChar(S), Count) then
    SetLength(S, StrLen(PChar(S)))
  else
    S := '';
  Result := S;
end;
{$endif}

function GetTextHeight(const AText: string; AFont: TFont): Integer;
var
  Bitmap: Graphics.TBitmap;
begin
  Bitmap := Graphics.TBitmap.Create;
  try
    Bitmap.Canvas.Font.Assign(AFont);
    Result := Bitmap.Canvas.TextExtent(AText).cy;
  finally
    Bitmap.Free;
  end;
end;

procedure OptimizeWidth(APanel: TPanel);
var
  S: string;
begin
  S := APanel.Caption;
  if Trim(S) <> '' then
    APanel.Width := GetTextWidth(APanel.Caption, APanel.Font) + 10
  else
    APanel.Width := 0;
end;

function CloneMenuItem(SourceItem: TMenuItem): TMenuItem;
var
  I: Integer;
Begin
  with SourceItem do
  Begin
    Result := NewItem(Caption, Shortcut, Checked, Enabled, OnClick, HelpContext, Name + 'Copy');
    for I := 0 To Count - 1 do
      Result.Add(CloneMenuItem(Items[I]));
  end;
end;

function GetTextWidth(const AText: string; AFont: TFont): Integer;
var
  Bitmap : Graphics.TBitmap;
begin
  Bitmap := Graphics.TBitmap.Create;
  try
    Bitmap.Canvas.Font.Assign(AFont);
    Result := Bitmap.Canvas.TextExtent(AText).cx;
  finally
    Bitmap.Free;
  end;
end;

function GetVariantTypeName(const AVariant: Variant): string;
begin
  case VarType(AVariant) and varTypeMask of
   varEmpty:    Result := 'varEmpty';
   varNull:     Result := 'varNull';
   varSmallint: Result := 'varSmallint';
   varInteger:  Result := 'varInteger';
   varSingle:   Result := 'varSingle';
   varDouble:   Result := 'varDouble';
   varCurrency: Result := 'varCurrency';
   varDate:     Result := 'varDate';
   varOleStr:   Result := 'varOleStr';
   varDispatch: Result := 'varDispatch';
   varError:    Result := 'varError';
   varBoolean:  Result := 'varBoolean';
   varVariant:  Result := 'varVariant';
   varUnknown:  Result := 'varUnknown';
   varByte:     Result := 'varByte';
   varString:   Result := 'varString';
   else
     Result := 'Unkown Variant Type';
  end;
  if VarType(AVariant) and varArray <> 0 then
    Result := Result + ' [Array]';
  if VarType(AVariant) and varByRef <> 0 then
    Result := Result + ' (By Reference)';
end;

procedure OleVarFromVariant(var AOleVariant: OleVariant; const AVariant: Variant);
begin
  if VarType(AVariant) = varString then
   AOleVariant := WideString(AVariant)
  else
   AOleVariant := AVariant;
end;

function StringToVariant(AString : string): Variant;
var
  I    : Int64;
  K    : Integer;
  iPos : Integer;
begin
  Result := Null;
  if AString <> '' then
    if AString[1] = '''' then
      Result := Copy(AString, 2, Length(AString) - 2)
    else
    begin
      Val(AString, I, K);
      if K = 0 then
        Result := Integer(I)
      else
      begin
        if FormatSettings.DecimalSeparator <> '.' then
        begin
          iPos := Pos('.', AString);
          if iPos > 0 then
            AString[iPos] := FormatSettings.DecimalSeparator;
        end;
        Result := StrToFloat(AString);
      end;
    end;
end;

{$ifdef windows}
function VirtualKeyToChar(AKey : Word) : string;
var
  KS : TKeyboardState;
  AR : Integer;
begin
  GetKeyboardState(KS);
  SetLength(Result, 2);
  AR := ToAscii(AKey, MapVirtualKey(AKey, 0), KS, @Result[1], 0);
  case AR of
    0 : Result := '';
    1 : SetLength(Result, 1);
    2 : ;
    else
      Result := '';
  end;
end;
{$endif}

procedure StrToStrings(const AString: string; AList: TStrings; ASeparator: Char);
var
  S : string;
  I : Integer;
begin
  if Assigned(AList) then
  try
    AList.BeginUpdate;
    AList.Clear;
    S := '';
    for I := 1 to Length(AString) do
      if AString[I] = ASeparator then
      begin
        AList.Add(Trim(S));
        S := '';
      end
      else if AString <> ' ' then
        S := S + AString[I];
    if S <> '' then
      AList.Add(Trim(S));
  finally
    AList.EndUpdate;
  end;
end;

function ValueNeedsConversion(const AVarType: Integer;
  const AFieldType: TFieldType): Boolean;
begin
   if AVarType and varByRef <> 0 then
    Result := (AFieldType <> ftReference) and
              ValueNeedsConversion(AVarType and not varByRef, AFieldType)
  else
  if AVarType and varArray <> 0 then
    Result := (AFieldType <> ftArray) and
              ValueNeedsConversion(AVarType and not varArray, AFieldType)
  else
  case AVarType and VarTypeMask of
   varSmallint, varInteger:
     Result := not (AFieldType in
                 [ftSmallint, ftInteger, ftWord, ftAutoInc, ftLargeint]);
   varSingle, varDouble:
     Result := not (AFieldType in [ftFloat]);
   varCurrency:
     Result := not (AFieldType in [ftCurrency, ftBCD]);
   varDate:
     Result := not (AFieldType in [ftDate, ftTime, ftDateTime]);
   varOleStr, varString:
     Result := not (AFieldType in [ftString, ftFixedChar, ftWideString]);
   varBoolean:
     Result := not (AFieldType in [ftBoolean]);
   varUnknown, varDispatch:
     Result := not (AFieldType in [ftDataSet, ftADT]);
   varByte:
     Result := not (AFieldType in
                 [ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
                  ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor]);
   //varEmpty, varAny, varError varVariant ???
   else
     Result := False;
  end;
end;

function VarArrayElemCount(const AVarArray: Variant): Integer;
begin
  if not VarIsArray(AVarArray) then
    Result := 1
  else
  begin
    Assert(VarArrayDimCount(AVarArray) = 1, 'VarArrayDimCount(Value) = 1');
    Result := VarArrayHighBound(AVarArray, 1) - VarArrayLowBound(AVarArray, 1) + 1;
  end;
end;

function VarAsTypeDef(const AValue: Variant; AVarType: TVarType;
  const ADefValue: Variant): Variant;
begin
  if VarIsType(AValue, AVarType) then
    Result := VarAsType(AValue, AVarType)
  else
    Result := ADefValue;
end;

procedure VariantToVarRec(AVariant: Variant; var AVarRecArray: TVarRecArray);
var
  I : Integer;
begin
  SetLength(AVarRecArray, VarArrayHighBound(AVariant, 1) + 1);

  for I := 0 to VarArrayHighBound(AVariant, 1) do
    case TVarData(AVariant[I]).VType of
      varSmallint, varInteger, varByte:
        begin
          AVarRecArray[I].VType := vtInteger;
          AVarRecArray[I].VInteger := AVariant[I];
        end;
      varInt64:
        begin
          AVarRecArray[I].VType := vtInt64;
          New(AVarRecArray[I].VInt64);
          AVarRecArray[I].VInt64^ := AVariant[I];
        end;
      varSingle, varDouble, varCurrency, varDate:
        begin
          AVarRecArray[I].VType := vtExtended;
          New(AVarRecArray[I].VExtended);
          AVarRecArray[I].VExtended^ := AVariant[I];
        end;
      varBoolean:
        begin
          AVarRecArray[I].VType := vtBoolean;
          AVarRecArray[I].VBoolean := AVariant[I];
        end;
      varOleStr, varString:
        begin
          AVarRecArray[I].VType := vtString;
          New(AVarRecArray[I].VString);
          AVarRecArray[I].VString^ := AVariant[I];
        end;
      varVariant:
        begin
          AVarRecArray[I].VType := vtVariant;
          New(AVarRecArray[I].VVariant);
          AVarRecArray[I].VVariant^ := AVariant[I];
        end;
    end;
end;

function VariantTypeForFieldType(const AFieldType: TFieldType): Integer;
begin
  case AFieldType of
   ftString:
     Result := varString;
   ftSmallint:
     Result := varSmallint;
   ftInteger, ftWord, ftAutoInc:
     Result := varInteger;
   ftBoolean:
     Result := varBoolean;
   ftFloat:
     Result := varDouble;
   ftCurrency, ftBCD:
     Result := varCurrency;
   ftDate, ftTime, ftDateTime:
     Result := varDate;
   ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle,
     ftDBaseOle, ftTypedBinary, ftCursor:
     Result := varByte or varArray;
   ftFixedChar:
     Result := varString;
   ftLargeint:
     Result := varInteger;
   ftWideString:
     Result := varOleStr;
   ftArray:
     Result := varVariant or varArray;
   ftReference:
     Result := varVariant or varByRef;
   ftDataSet, ftADT:
     Result := varUnknown;

   // ftUnknown
   else
     Result := varNull;
  end;
end;

function VarRecToOleVariant(const AVarRec: TVarRec): OleVariant;
begin
  case AVarRec.VType of
   vtInteger    : Result := AVarRec.VInteger;
   vtBoolean    : Result := AVarRec.VBoolean;
   vtChar       : Result := AVarRec.VChar;
   vtExtended   : Result := AVarRec.VExtended^;
   vtString     : Result := AVarRec.VString^;
   vtPChar      : Result := string(AVarRec.VPChar);
   vtWideChar   : Result := AVarRec.VWideChar;
   vtPWideChar  : Result := WideString(AVarRec.VPWideChar);
   vtAnsiString : Result := string(AVarRec.VAnsiString);
   vtCurrency   : Result := AVarRec.VCurrency^;
   vtVariant    : Result := AVarRec.VVariant^;
   vtWideString : Result := WideString(AVarRec.VWideString);
   vtPointer    : TVarData(Result).VPointer := AVarRec.VPointer;
   vtObject     : TVarData(Result).VPointer := AVarRec.VObject;
   vtClass      : TVarData(Result).VPointer := AVarRec.VClass;
   vtInterface  : TVarData(Result).VDispatch := AVarRec.VInterface;
  end;
end;

function VarRecToString(const AVarRec: TVarRec): string;
var
  S : string;
begin
  case AVarRec.VType of
    vtInteger    : S := Format('%d', [AVarRec.VInteger]);
    vtInt64      : S := Format('%d', [AVarRec.VInt64^]);
    vtString     : S := Format('%s', [AVarRec.VString^]);
    vtWideString : S := Format('%s', [WideString(AVarRec.VWideString)]);
    vtAnsiString : S := Format('%s', [AnsiString(AVarRec.VAnsiString)]);
    vtCurrency   : S := Format('%s', [CurrToStr(AVarRec.VCurrency^)]);
    vtExtended   : S := Format('%s', [FloatToStr(AVarRec.VExtended^)]);
    vtChar       : S := Format('%s', [AVarRec.VChar]);
    vtPChar      : S := Format('%s', [AVarRec.VPChar]);
    vtWideChar   : S := Format('%s', [AVarRec.VWideChar]);
    vtPWideChar  : S := Format('%s', [AVarRec.VPWideChar]);
    vtVariant    : S := Format('%s', [VarToStr(AVarRec.VVariant^)]);
  else
    S := 'Unassigned'
    // TODO: better: raise exception
  end;
  Result := S;
end;



function VarRecToVariant(const AVarRec: TVarRec): Variant;
begin
  with AVarRec do
    case VType of
      vtInteger,
      vtObject     : Result := VInteger;
      vtBoolean    : Result := VBoolean;
      vtExtended,
      vtCurrency   : Result := VExtended^;
      vtChar       : Result := VChar;
      vtString     : Result := VString^;
      vtAnsiString : Result := string(VAnsiString);
      vtVariant    : Result := VVariant^;
      else
        Result := Null;
    end;
end;



procedure ChangeOwner(AComponent, ANewOwner: TComponent);
begin
  if Assigned(AComponent) and Assigned(ANewOwner) then
  begin
    AComponent.Owner.RemoveComponent(AComponent);
    ANewOwner.InsertComponent(AComponent);
  end;
end;



function Unformat(const ASource, APattern: string; const AArgs: array of const)
  : Integer;

{ The opposite of Format, Unformat splits up a formatted source string into
  substrings and Integers.  It is an alternative to parsing when the format is
  known to be fixed. The pattern parameter contains the format string, which is
  a combination of plain characters and format specifiers.

  Note: only short strings (fixed length strings) can be passed as a parameter
        to this function.

  The following specifiers are supported:

  %s   indicates that a string value is required
  %d   indicates that an integer value is required
  %S   indicates that a string value should be ignored
  %D   indicates that an integer value should be ignored

  Unformat compares the source with the pattern, and plain characters
  that do not match will raise an EConvertError.  When a format specifier
  is encountered in the pattern, an argument is fetched and used to
  store the result that is obtained from the source.  Then the comparison
  continues.

  For each %s, the args list must contain a pointer to a string variable,
  followed by an integer specifying the maximum length of the string.
  For each %d, the args list must contain a pointer to an integer variable.

  When the end of the source string is reached, the function returns
  without modifying the remaining arguments, so you might wish to initialize
  your variables to "default" values before the function call.

  Unformat returns the number of values it has extracted.

  Examples:

  var
    s1, s2: string[31];
    i: Integer;

  Unformat('[abc]123(def)', '[%s]%d(%s)', [@s1, 31, @i, @s2, 31]);
    (* s1 = 'abc', i = 123, s2 = 'def' *)

  Unformat('Hello, Universe!!!', '%s, %s%d', [@s1, 31, @s2, 31, @i]);
    (* s1 = 'Hello', s2 = 'Universe!!!', i is untouched *)

  Unformat('How much wood could a woodchuck chuck...',
           '%S %S %s could a %S %s...', [@s1, 31, @s2, 31]);
    (* s1 = 'wood', s2 = 'chuck' *) }

const
  Digits = ['0'..'9'];

var
  I, J      : Integer;
  iArgIndex : Integer;
  iStart    : Integer;
  iEnd      : Integer;
  iMaxLen   : Integer;
  C         : Char;
begin
  Result := 0;
  iArgIndex := 0;
  I := 1;
  J := 1;
  while (I < Length(APattern)) and (J <= Length(ASource)) do
  begin
    if APattern[I] = '%' then
      case APattern[I+1] of
        'D': begin
               Inc(I, 2);
               while (J <= Length(ASource)) and
                 ((ASource[J] in Digits) or (ASource[J] = '-')) do Inc(J);
               Inc(Result);
             end;
        'S': begin
               Inc(I, 2);
               if I > Length(APattern) then break
               else
               begin
                 c := APattern[I];
                 while (J <= Length(ASource)) and (ASource[J] <> c) do
                   Inc(J);
               end;
               Inc(Result);
             end;
        'd': begin
               if iArgIndex > High(AArgs) then
                 raise EConvertError.Create('Not enough arguments');
               Inc(I, 2);
               iStart := J;
               while (J <= Length(ASource)) and
                 ((ASource[J] in Digits) or (ASource[J] = '-')) do Inc(J);
               iEnd := J;
               if iEnd > iStart then
                 PInteger(AArgs[iArgIndex].VPointer)^ :=
                   StrToInt(Copy(ASource, iStart, iEnd - iStart));
               Inc(iArgIndex);
               Inc(Result);
             end;
        's': begin
               if iArgIndex > High(AArgs) - 1 then
                 raise EConvertError.Create('Not enough arguments');
               if AArgs[iArgIndex + 1].VType <> vtInteger then
                 raise EConvertError.Create('No string size specified');
               iMaxLen := AArgs[iArgIndex+1].VInteger;
               Inc(I, 2);
               if I > Length(APattern) then
               begin
                 AArgs[iArgIndex].VString^ :=
                   Copy(ASource, J, Min(Length(ASource) + 1 - J, iMaxLen));
                 break;
               end
               else
               begin
                 c := APattern[I];
                 iStart := J;
                 while (J <= Length(ASource)) and (ASource[J] <> c) do
                   Inc(J);
                 iEnd := J;
                 AArgs[iArgIndex].VString^ :=
                   Copy(ASource, iStart, Min(iEnd - iStart, iMaxLen));
                 Inc(iArgIndex, 2);
               end;
               Inc(Result);
             end;
      else Inc(I);
      end
    else
      {if APattern[I] <> ASource[J] then
        raise EConvertError.Create('Pattern mismatch')
      else}
      begin
        Inc(I);
        Inc(J);
      end;
  end;
end;



function VariantCompare(AVariant1, AVariant2 : Variant) : Boolean;
begin
  Result := False;
  if (VarType(AVariant1) = VarType(AVariant2)) then
     Result := AVariant1 = AVariant2;
end;



{ 'Like' code is written by Wladimir Perepletchick }

function Like(const ASource, ATemplate: string): Boolean;
const
  SpecialChars: TSysCharSet = ['%', '*', '?', '_'];
var
 I, J, K, LTemplate, LSource: Integer;
begin
  Result := False;
  LTemplate := Length(ATemplate);
  LSource := Length(ASource);
  I := 1;
  J := 1;
  while (I <= LTemplate) and (J <= LSource) do
  begin
    case ATemplate[I] of
      '?', '_': ;
      '*', '%':
      begin
        while (ATemplate[I] in SpecialChars) and (I <= LTemplate) do
          Inc(I);
        if I > LTemplate then
          Result := True
        else
          while J <= LSource do
          begin
            while (ASource[J] <> ATemplate[I]) and (J <= LSource) do
              Inc(J);
            if J > LSource then Break;
            K := 0;
            while (ASource[J + K] = ATemplate[I + K]) and
                  (J + K <= LSource) and (I + K <= LTemplate) and
                  (not (ATemplate[I + K] in SpecialChars)) do
              Inc(K);
            if (ATemplate[I + K] in SpecialChars) or (I + K > LTemplate) then
            begin
              Inc(I, K - 1);
              Inc(J, K - 1);
              Break;
            end;
            Inc(J, K);
          end;
          if J > LSource then
            Break;
      end;
      else
        if (ASource[J] <> ATemplate[I]) then
          Break;
    end;
    Inc(I);
    Inc(J);
    if (J > LSource) then
    begin
      K := 0;
      while (ATemplate[I + K] in ['%', '*']) and (I + K <= LTemplate) do
        Inc(K);
      if (I + K > LTemplate) then
        Result := True;
    end;
  end;
end;

{$ifdef windows}
function CreateGUIDString: string;
var
  ClassID : TCLSID;
  P       : PWideChar;
begin
  CoCreateGuid(ClassID);
  StringFromCLSID(ClassID, P);
  Result := P;
  CoTaskMemFree(P);
end;

function CreateUniqueID: String;
var
  AGUID       : TGUID;
  AGUIDString : Widestring;
begin
  CoCreateGUID(AGUID);
  SetLength(AGUIDString, 39);
  StringFromGUID2(AGUID, PWideChar(AGUIDString), 39);
  Result := string(PWideChar(AGUIDString));
  Result := Copy(Result, 2, 36);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
end;
{$endif}

{ Formats the given time amount (in seconds) to the form:
  <Hours>:<Minutes>:<Seconds>.<Hundreds> }

function FormatElapsedTime(ASeconds: Extended): string;
var
  H  : Integer;
  M  : Integer;
  S  : Integer;
  HS : Integer;
begin
  HS := Round(Int(100 * Frac(ASeconds)));
  S  := Round(Int(ASeconds));
  H  := S div 3600;
  S  := S mod 3600;
  M  := S div 60;
  S  := S mod 60;

  Result := Format('%2.2d:%2.2d:%2.2d.%2.2d', [H, M, S, HS]);
end;



{ Author: Michael Haller }

function FormatByteText(ABytes: Integer): string;
var
  D : Double;
begin
  Result := 'n.a.';
  try
    D := ABytes / 1024;
    if ABytes = 0 then
      Result := '0 Byte'
    else
      if ABytes < 1048576 then
        Result := FloatToStrF(D, ffNumber, 18, 1)+' KB'
      else
        if ABytes < 1073741824 then begin
          D := D / 1024;
          ABytes := Round(D);
          if ABytes < 10 then
            Result := FloatToStrF(D, ffNumber, 18, 1)+' MB'
          else
            Result := IntToStr(ABytes)+' MB';
        end else begin
          ABytes := Round(D / 1024 / 1024);
          Result := IntToStr(ABytes)+' GB';
        end;
    if (Result[Length(Result)-3] = '0') and (Result[Length(Result)-4] = ',') then
      Delete(Result, Length(Result)-4, 2);
  except
    // ignore exceptions
  end;
end;

{ Mixes two colors for a given transparancy level  }

function MixColors(C1, C2: TColor; W1: Integer): TColor;
var
  W2: Cardinal;
begin
  Assert(W1 in [0..255]);
  W2 := W1 xor 255;
{$ifdef windows}
  if Integer(C1) < 0 then C1 := GetSysColor(C1 and $000000FF);
  if Integer(C2) < 0 then C2 := GetSysColor(C2 and $000000FF);
{$else}
  C1 := ColorToRGB(C1);
  C2 := ColorToRGB(C2);
{$endif}
  Result := Integer(
    ((Cardinal(C1) and $FF00FF) * Cardinal(W1) +
    (Cardinal(C2) and $FF00FF) * W2) and $FF00FF00 +
    ((Cardinal(C1) and $00FF00) * Cardinal(W1) +
    (Cardinal(C2) and $00FF00) * W2) and $00FF0000) shr 8;
end;

function StringReplaceMultiple(const Source: AnsiString;
  const OldPatterns, NewPatterns: array of AnsiString;
  CaseSensitive: Boolean = True): AnsiString;

type
  TFoundPos = record
    Position: Integer;
    PatternNum: Integer;
  end;

  TPattern = record
    Old: AnsiString;
    New: PAnsiChar;
    LengthOld: Integer;
    LengthNew: Integer;
    Diff: Integer;
  end;

var
  C: Integer;
  FoundCount: Integer;

  Positions: array of TFoundPos;
  PositionLength: Integer;

  Patterns: array of TPattern;
  PatternCount: Integer;
  PNum: Integer;

  SourcePosition: Integer;
  SourceLength: Integer;
  SearchSource: AnsiString;

  DeltaOld: Integer;
  Delta: Integer;

  PSource, PDest, PNew: PAnsiChar;
begin
  // Is there anything to do at all?
  if (Source = '') or (Length(OldPatterns) <> Length(NewPatterns)) then
  begin
    Result := Source;
    Exit;
  end;

  // Initialize the Pattern records
  PatternCount := Length(OldPatterns);

  FoundCount := 0;
  SetLength(Patterns, PatternCount);
  for C := 0 to PatternCount - 1 do
    if (OldPatterns[C] <> '') and (OldPatterns[C] <> NewPatterns[C]) then
    begin
      if CaseSensitive then
        Patterns[FoundCount].Old := OldPatterns[C]
      else
        Patterns[FoundCount].Old := AnsiLowerCase(OldPatterns[C]);
      Patterns[FoundCount].LengthOld := Length(OldPatterns[C]);
      Patterns[FoundCount].New := PAnsiChar(NewPatterns[C]);
      Patterns[FoundCount].LengthNew := Length(NewPatterns[C]);
      Patterns[FoundCount].Diff :=
        Patterns[FoundCount].LengthNew - Patterns[FoundCount].LengthOld;

      Inc(FoundCount);
    end;
  PatternCount := FoundCount;
  SetLength(Patterns, PatternCount);

  // Nothing to replace
  if PatternCount = 0 then
  begin
    Result := Source;
    Exit;
  end;

  if CaseSensitive then
    SearchSource := Source
  else
    SearchSource := AnsiLowerCase(Source);

  try
    // Initialize some variables
    SourceLength := Length(SearchSource);
    Delta := 0;

    DeltaOld := 0;
    for C := 0 to PatternCount - 1 do
      Inc(DeltaOld, Patterns[C].LengthOld);
    DeltaOld := Round(DeltaOld / PatternCount);

    FoundCount := 0;

    // ----------------------------------
    // Check the amount of replaces
    // ----------------------------------

    // We *should* range check here, but who has strings > 2GB ?
    PositionLength := SourceLength div DeltaOld + 1;
    SetLength(Positions, PositionLength);

    C := 1;
    while C <= SourceLength do
    begin
      for PNum := 0 to PatternCount - 1 do
      begin
        // Check first char before we waste a jump to CompareMem
        if (SearchSource[C]) = (Patterns[PNum].Old[1]) then
        begin
          if CompareMem(@SearchSource[C], @Patterns[PNum].Old[1], Patterns[PNum].LengthOld) then
          begin
            if FoundCount >= PositionLength then
            begin
              // Make room for more Positions
              Inc(PositionLength, 4);
              SetLength(Positions, PositionLength);
            end;

            Positions[FoundCount].Position := C; // Store the found position
            Positions[FoundCount].PatternNum := PNum;
            Inc(FoundCount);
            Inc(C, Patterns[PNum].LengthOld - 1); // Jump to after OldPattern
            Inc(Delta, Patterns[PNum].Diff);
            Break;
          end;
        end;
      end;
      Inc(C);
    end;

    // ----------------------------------
    // Actual replace
    // ----------------------------------

    if FoundCount > 0 then // Have we found anything?
    begin
      // We know the length of the result
      // Again, we *should* range check here...
      SetLength(Result, SourceLength + Delta);

      // Initialize some variables
      SourcePosition := 1;
      PSource := PAnsiChar(Source);
      PDest := PAnsiChar(Result);

      // Replace...

      for C := 0 to FoundCount - 1 do
      begin
        PNum := Positions[C].PatternNum;

        // Copy original and advance resultpos
        PNew := Patterns[PNum].New;

        Delta := Positions[C].Position - SourcePosition;
        Move(PSource^, PDest^, Delta);
        Inc(PDest, Delta);

        // Append NewPattern and advance resultpos
        Move(PNew^, PDest^, Patterns[PNum].LengthNew);
        Inc(PDest, Patterns[PNum].LengthNew);

        // Jump to after OldPattern
        Inc(PSource, Delta + Patterns[PNum].LengthOld);
        SourcePosition := Positions[C].Position + Patterns[PNum].LengthOld;
      end;

      // Append characters after last OldPattern
      Move(PSource^, PDest^, SourceLength - SourcePosition + 1);
    end else
      Result := Source; // Nothing to replace

  finally
    // Clean up
    Finalize(Positions);
    Finalize(Patterns);
  end;
end;

{ Returns the offset to the Pointer to the Interface Method Table.

  Author : Hallvard Vassbotn

  PIMT is short for pointer to interface method table. This is a special
  compiler generated "field" that is added to an object instance by the compiler
  when you declare that the class implements an interface. The "field" is a
  pointer to a kind of virtual method table for the methods declared on the
  interface. The function returns the offset of this field. Note that the
  compiler uses an ADD assembly instruction to adjust the Self parameter -
  but the value added is actually negative. That's why we return the negated
  value of the adjustment offset. }

function GetPIMTOffset(const I: IInterface): Integer;
// PIMT = Pointer to Interface Method Table
const
  AddByte = $04244483; // opcode for ADD DWORD PTR [ESP+4], Shortint
  AddLong = $04244481; // opcode for ADD DWORD PTR [ESP+4], Longint
type
  PAdjustSelfThunk = ^TAdjustSelfThunk;
  TAdjustSelfThunk = packed record
    case AddInstruction: longint of
      AddByte : (AdjustmentByte : ShortInt);
      AddLong : (AdjustmentLong : LongInt);
  end;
  PInterfaceMT = ^TInterfaceMT;
  TInterfaceMT = packed record
    QueryInterfaceThunk: PAdjustSelfThunk;
  end;
  TInterfaceRef = ^PInterfaceMT;
var
  QueryInterfaceThunk: PAdjustSelfThunk;
begin
  Result := -1;
  if Assigned(Pointer(I)) then
    try
      QueryInterfaceThunk := TInterfaceRef(I)^.QueryInterfaceThunk;
      case QueryInterfaceThunk.AddInstruction of
        AddByte: Result := -QueryInterfaceThunk.AdjustmentByte;
        AddLong: Result := -QueryInterfaceThunk.AdjustmentLong;
      end;
    except
      // Protect against non-Delphi or invalid interface references
    end;
end;

{$ifdef windows}
function ExploreFile(const AFileName: string): Boolean;
const
  PARAM = '/e,/select,"%s"';
var
  S : string;
begin
  if FileExists(AFileName) then
  begin
    S := Format(PARAM, [AFileName]);
    ShellExecute(
      Application.MainForm.Handle,
      'open',
      PChar('explorer.exe'),
      PChar(S),
      nil,
      SW_SHOWNORMAL
    );
    Result := True;
  end
  else
    Result := False;
end;
{$endif}

function GetParentDir(sPath : string) : string;
var
  I : Integer;
begin
  Result := '';
  sPath := ExcludeTrailingPathDelimiter(sPath);
  // Start from one character before last.
  for I := length(sPath) - 1 downto 1 do
    if sPath[I] = DirectorySeparator then
    begin
      Result := Copy(sPath, 1, I);
      Break;
    end;
end;

{$ifdef windows}
procedure CreateShellLink(ShellLink: TShellLink);
const
  IID_IPersistFile: TGUID = (D1:$0000010B;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
var
  SL: IShellLink;
  PF: IPersistFile;
  WideFilename: array[0..MAX_PATH-1] of WideChar;
begin
  try
    if CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IID_IShellLinkA, SL) <> S_OK then
      raise Exception.Create('CoCreateInstance failed');
    SL.SetPath(PChar(ShellLink.ShortcutTo));
    SL.SetArguments(PChar(ShellLink.Parameters));
    case ShellLink.ShowCommand of
      wsNormal: SL.SetShowCmd(SW_SHOW);
      wsMaximized: SL.SetShowCmd(SW_SHOWMAXIMIZED);
      wsMinimized: SL.SetShowCmd(SW_SHOWMINNOACTIVE);
    end;
    SL.SetIconLocation(PChar(ShellLink.IconPath), ShellLink.IconIndex);
    //SL.SetHotkey(ShellLink.HotKey);
    SL.SetWorkingDirectory(PChar(ShellLink.WorkingDir));
    SL.SetDescription(PChar(ShellLink.Description));
    if SL.QueryInterface(IID_IPersistFile, PF) <> S_OK then
      raise Exception.Create('IShellLink::QueryInterface failed');
    MultiByteToWideChar(CP_ACP, 0, PChar(ShellLink.Filename), -1, WideFilename, MAX_PATH);
    PF.Save(WideFilename, True);
  except
    // just ignore the creation
  end;
end;
{$endif}

function WordCount(const AString: string; const AWordDelims: TSysCharSet)
  : Integer;
var
  SLen, I: Cardinal;
begin
  Result := 0;
  I := 1;
  SLen := Length(AString);
  while I <= SLen do
    begin
      while (I <= SLen) and (AString[I] in AWordDelims) do
        Inc(I);
      if I <= SLen then
        Inc(Result);
      while (I <= SLen) and not (AString[I] in AWordDelims) do
        Inc(I);
    end;
end;

function WordPosition(const AIndex: Integer; const AString: string;
  const AWordDelims: TSysCharSet): Integer;
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while (I <= Length(AString)) and (Count <> AIndex) do
    begin
    { skip over delimiters }
      while (I <= Length(AString)) and (AString[I] in AWordDelims) do
        Inc(I);
    { if we're not beyond end of S, we're at the start of a word }
      if I <= Length(AString) then
        Inc(Count);
    { if not finished, find the end of the current word }
      if Count <> AIndex then
        while (I <= Length(AString)) and not (AString[I] in AWordDelims) do
          Inc(I)
      else
        Result := I;
    end;
end;

function ExtractWord(const AIndex: Integer; const AString: string;
  const AWordDelims: TSysCharSet): string;
var
  I   : Integer;
  Len : Integer;
begin
  Len := 0;
  I := WordPosition(AIndex, AString, AWordDelims);
  if I <> 0 then
    { find the end of the current word }
    while (I <= Length(AString)) and not (AString[I] in AWordDelims) do
      begin
      { add the I'th character to result }
        Inc(Len);
        SetLength(Result, Len);
        Result[Len] := AString[I];
        Inc(I);
      end;
  SetLength(Result, Len);
end;

procedure DisableControls(AControlContainer : TWinControl);
begin
  EnableControls(AControlContainer, False)
end;

{ Enables/disables all child controls of the AControlContainer control (
  eg. TPanel, TGroupBox, etc.). }

procedure EnableControls(AControlContainer : TWinControl; AEnabled : Boolean);
var
  I : Integer;
  C : TControl;
begin
  if not (csAcceptsControls in AControlContainer.ControlStyle) then
    raise Exception.Create('Invalid control container!');

  for I := 0 to AControlContainer.ControlCount - 1 do
  begin
    C := AControlContainer.Controls[I];
    if (csAcceptsControls in C.ControlStyle) then
      EnableControls(TWinControl(C), AEnabled);

    if C is TWinControl then
    begin
      C.Enabled := AEnabled;
      if not (csParentBackground in C.ControlStyle) and
         IsPublishedProp(C, 'Color') then
      begin
        if AEnabled then
          SetOrdProp(C, 'Color', clWhite)
        else
          SetOrdProp(C, 'Color', clBtnFace);
      end;
    end
    else if IsPublishedProp(C, 'Enabled') then
    begin
      if AEnabled then
        SetEnumProp(C, 'Enabled', 'True')
      else
        SetEnumProp(C, 'Enabled', 'False');
    end;
  end;
end;

function URLEncode(const AString: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AString) do
  begin
    case AString[I] of
      'A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.':
        Result := Result + AString[I];
      ' ':
        Result := Result + '%20';
      else
        Result := Result + '%' + SysUtils.IntToHex(Ord(AString[I]), 2);
    end;
  end;
end;

function URLDecode(const AString: string): string;
const
  HexChar = '0123456789ABCDEF';
var
  I, J: integer;
begin
  SetLength(Result, Length(AString));
  I := 1;
  J := 1;
  while I <= Length(AString) do
  begin
    if (AString[I] = '%') and (I + 2 < Length(AString)) then
    begin
      Result[J] := Chr(((pred(Pos(AString[I + 1], HexChar))) shl 4)
        or (Pred(Pos(AString[I + 2], HexChar))));
      Inc(I, 2);
    end
    else
      Result[J] := AString[I];
    Inc(I);
    Inc(J);
  end;
  SetLength(Result, pred(J));
end;

//  SetBit()      : Sets a single BIT in a string to true or false
procedure SetBit(var Str: string; BitNr: dword; Value: boolean);
var
  CharNr        : dword;
  CharBit       : byte;
  Original, Mask: byte;
begin
  CharNr := (BitNr DIV 8) + 1;
  CharBit := (BitNr MOD 8);
  Original := byte(Str[CharNr]);
  Mask := 1 shl CharBit;
  if Value = true then
    Original := (Original or Mask)
  else
    Original := (Original and not Mask);
  Str[CharNr] := char(Original);
end;

{
   GetBit()      : Returns the state of a single bit in a string }
function GetBit(Str: string; BitNr: dword): boolean;
var
  CharNr        : dword;
  CharBit       : byte;
  Original, Mask: byte;
begin
  CharNr := (BitNr DIV 8) + 1;
  CharBit := (BitNr MOD 8);
  Original := byte(Str[CharNr]);
  Mask := 1 shl CharBit;
  if (Original and Mask) = Mask then
    Result := true
  else
    Result := false;
end;

{  FindBest()    : Finds a substring in another string and returns position and
                  the number of characters upto where they are equal}

procedure FindBest(Main, Sub: string; var FoundLen, FoundPos: integer);
var
  P, T, FL, MaxLen: integer;
begin
  if Length(Sub) > Length(Main) then
    MaxLen := Length(Main)
  else
    MaxLen := Length(Sub);
  FoundLen := 0;
  FoundPos := 0;
  for P := 1 to Length(Main) - MaxLen do
  begin
    FL := 0;
    for T := 1 to MaxLen do
    begin
      if Main[P + T - 1] = Sub[T] then
        FL := T
      else
        Break;
    end;
    if FL > FoundLen then
    begin
      FoundLen := FL;
      FoundPos := P;
    end;
  end;
end;

function DrawHTML(const ARect: TRect; const ACanvas: TCanvas; const Text: String): Integer;
(*DrawHTML - Draws text on a canvas using tags based on a simple subset of HTML/CSS

  <B> - Bold e.g. <B>This is bold</B>
  <I> - Italic e.g. <I>This is italic</I>
  <U> - Underline e.g. <U>This is underlined</U>
  <font-color=x> Font colour e.g.
                <font-color=clRed>Delphi red</font-color>
                <font-color=#FFFFFF>Web white</font-color>
                <font-color=$000000>Hex black</font-color>
  <font-size=x> Font size e.g. <font-size=30>This is some big text</font-size>
  <font-family> Font family e.g. <font-family=Arial>This is arial</font-family>*)

  function CloseTag(const ATag: String): String;
  begin
    Result := concat('/', ATag);
  end;

  function GetTagValue(const ATag: String): String;
  var
    p: Integer;
  begin
    p := pos('=', ATag);

    if p = 0 then
      Result := ''
    else
      Result := copy(ATag, p + 1, MaxInt);
  end;

  function ColorCodeToColor(const Value: String): TColor;
  var
    HexValue: String;
  begin
    Result := 0;

    if Value <> '' then
    begin
      if (length(Value) >= 2) and (copy(Uppercase(Value), 1, 2) = 'CL') then
      begin
        // Delphi colour
        Result := StringToColor(Value);
      end else
      if Value[1] = '#' then
      begin
        // Web colour
        HexValue := copy(Value, 2, 6);

        Result := RGB(StrToInt('$'+Copy(HexValue, 1, 2)),
                      StrToInt('$'+Copy(HexValue, 3, 2)),
                      StrToInt('$'+Copy(HexValue, 5, 2)));
      end
      else
        // Hex or decimal colour
        Result := StrToIntDef(Value, 0);
    end;
  end;

const
  TagBold = 'B';
  TagItalic = 'I';
  TagUnderline = 'U';
  TagBreak = 'BR';
  TagFontSize = 'FONT-SIZE';
  TagFontFamily = 'FONT-FAMILY';
  TagFontColour = 'FONT-COLOR';
  TagColour = 'COLOUR';

var
  x, y, idx, CharWidth, MaxCharHeight: Integer;
  CurrChar: Char;
  Tag, TagValue: String;
  PreviousFontColour: TColor;
  PreviousFontFamily: String;
  PreviousFontSize: Integer;
  PreviousColour: TColor;

begin
  PreviousFontColour := ACanvas.Font.Color;
  PreviousFontFamily := ACanvas.Font.Name;
  PreviousFontSize := ACanvas.Font.Size;
  PreviousColour := ACanvas.Brush.Color;

  x := ARect.Left;
  y := ARect.Top + 1;
  idx := 1;

  MaxCharHeight := ACanvas.TextHeight('Ag');

  While idx <= length(Text) do
  begin
    CurrChar := Text[idx];

    // Is this a tag?
    if CurrChar = '<' then
    begin
      Tag := '';

      inc(idx);

      // Find the end of then tag
      while (Text[idx] <> '>')
            //and (idx <= length(Text)) do
            and (idx < length(Text)) do
      begin
        Tag := concat(Tag,  UpperCase(Text[idx]));

        inc(idx);
      end;

      ///////////////////////////////////////////////////
      // Simple tags
      ///////////////////////////////////////////////////
      if Tag = TagBold then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsBold] else

      if Tag = TagItalic then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsItalic] else

      if Tag = TagUnderline then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline] else

      if Tag = TagBreak then
      begin
        x := ARect.Left;

        inc(y, MaxCharHeight);
      end else

      ///////////////////////////////////////////////////
      // Closing tags
      ///////////////////////////////////////////////////
      if Tag = CloseTag(TagBold) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsBold] else

      if Tag = CloseTag(TagItalic) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsItalic] else

      if Tag = CloseTag(TagUnderline) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsUnderline] else

      if Tag = CloseTag(TagFontSize) then
        ACanvas.Font.Size := PreviousFontSize else

      if Tag = CloseTag(TagFontFamily) then
        ACanvas.Font.Name := PreviousFontFamily else

      if Tag = CloseTag(TagFontColour) then
        ACanvas.Font.Color := PreviousFontColour else

      if Tag = CloseTag(TagColour) then
        ACanvas.Brush.Color := PreviousColour else

      ///////////////////////////////////////////////////
      // Tags with values
      ///////////////////////////////////////////////////
      begin
        // Get the tag value (everything after '=')
        TagValue := GetTagValue(Tag);

        if TagValue <> '' then
        begin
          // Remove the value from the tag
          Tag := copy(Tag, 1, pos('=', Tag) - 1);

          if Tag = TagFontSize then
          begin
            PreviousFontSize := ACanvas.Font.Size;
            ACanvas.Font.Size := StrToIntDef(TagValue, ACanvas.Font.Size);
          end else

          if Tag = TagFontFamily then
          begin
            PreviousFontFamily := ACanvas.Font.Name;
            ACanvas.Font.Name := TagValue;
          end;

          if Tag = TagFontColour then
          begin
            PreviousFontColour := ACanvas.Font.Color;

            try
              ACanvas.Font.Color := ColorCodeToColor(TagValue);
            except
              //Just in case the canvas colour is invalid
            end;
          end else

          if Tag = TagColour then
          begin
            PreviousColour := ACanvas.Brush.Color;

            try
              ACanvas.Brush.Color := ColorCodeToColor(TagValue);
            except
              //Just in case the canvas colour is invalid
            end;
          end;
        end;
      end;
    end
    else
    // Draw the character if it's not a ctrl char
    //if CurrChar >= #32 then
    begin
      CharWidth := ACanvas.TextWidth(CurrChar);

      if x + CharWidth > ARect.Right then
      begin
        x := ARect.Left;

        inc(y, MaxCharHeight);
      end;

      if y + MaxCharHeight < ARect.Bottom then
      begin
        ACanvas.Brush.Style := bsClear;

        ACanvas.TextOut(x, y, CurrChar);
      end;

      x := x + CharWidth;
    end;

    inc(idx);
  end;

  Result := x;
end;

{$ifdef windows}
{ From stackoverflow:
  I was looking for a way to determine if a Form is actually visible (even only
  partially) to the user. In particular when it was supposed to be visible and
  Showing was True but the window was actually entirely behind another one.
}

function IsFormCovered(AForm: TForm): Boolean;
var
  R      : TRect;
  Rgn    : HRGN;
  TmpRgn : HRGN;
  RType  : Integer;
  H      : HWND;
begin
  R := AForm.BoundsRect;           // screen coordinates
  Rgn := CreateRectRgnIndirect(R); // AForm not overlapped region
  H := GetTopWindow(0);            // currently examined topwindow
  RType := SIMPLEREGION;           // Rgn type

// From topmost window downto AForm, build the not overlapped portion of AForm
  while (H <> 0) and (H <> AForm.handle) and (RType <> NULLREGION) do
  begin
    // nothing to do if hidden window
    if IsWindowVisible(H) then
    begin
      GetWindowRect(H, R);
      TmpRgn := CreateRectRgnIndirect(R);// currently examined window region
      RType := CombineRgn(Rgn, Rgn, TmpRgn, RGN_DIFF); // diff intersect
      DeleteObject( TmpRgn );
    end;
    if RType <> NULLREGION then // there's a remaining portion
      H := GetNextWindow(H, GW_HWNDNEXT);
  end;

  DeleteObject(Rgn);
  Result := RType = NULLREGION;
end;

{  Associates a given file extension with an application in the windows registry.
     example:  RegisterFileType('log','c:\MyLogViewer.exe') ;
}

procedure RegisterFileType(const AExtension: string; const AAppPath: string);
var
  R: TRegistry;
begin
  R := TRegistry.Create;
  try
    R.RootKey:= HKEY_CLASSES_ROOT;
    R.OpenKey('.' + AExtension, True);
    R.WriteString('', AExtension + 'file');
    R.CloseKey;
    R.CreateKey(AExtension + 'file') ;
    R.OpenKey(AExtension + 'file\DefaultIcon', True);
    R.WriteString('', AAppPath + ',0');
    R.CloseKey;
    R.OpenKey(AExtension + 'file\shell\open\command', True);
    R.WriteString('',AAppPath+' "%1"');
    R.CloseKey;
  finally
    R.Free;
  end;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;
{$endif}

function SetToString(ATypeInfo: PTypeInfo; const AValue;
  AQuoteValues: Boolean = True; ABrackets: Boolean = True;
  ATrimChars: Integer = -1): string;
var
  S    : TIntegerSet;
  I    : Integer;
  N    : Integer;
  Name : string;

  function GetOrdValue(Info: PTypeInfo; const SetParam): Integer;
  begin
    Result := 0;

    case GetTypeData(Info)^.OrdType of
      otSByte, otUByte:
        Result := Byte(SetParam);
      otSWord, otUWord:
        Result := Word(SetParam);
      otSLong, otULong:
        Result := Integer(SetParam);
    end;
  end;

  function GetPrefixLength(const AString: string): Integer;
  var
    C: Char;
    N: Integer;
  begin
    N := 0;
    if Length(AString) > 0 then
    begin
      C := AString[1];
      while (N < Length(AString)) and TCharacter.IsLower(C) do
      begin
        Inc(N);
        C := AString[N + 1];
      end;
    end;
    Result := N;
  end;

begin
  Result := '';
  Integer(S) := GetOrdValue(ATypeInfo, AValue);
  ATypeInfo := GetTypeData(ATypeInfo)^.CompType;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
  begin
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Name := GetEnumName(ATypeInfo, I);

      if ATrimChars >= 0 then
        N := ATrimChars
      else
        N := GetPrefixLength(Name);

      if N > 0 then
        Name := Copy(Name, N + 1, Length(Name) - N + 1);

      if AQuoteValues then
        Name := QuotedStr(Name);

      Result := Result + Name;
    end;
  end;
  if ABrackets and (Result <> '') then
    Result := '(' + Result + ')';
end;

{ Sets the Checked property of a TCheckbox without firing the OnClick event.
  If the checkbox is linked to an action, be sure to enable the AutoCheck
  property. }

 procedure SetCheckedState(ACheckBox : TCheckBox; ACheck : Boolean);
 var
   EH : TNotifyEvent;
   AH : TNotifyEvent;
 begin
   EH := ACheckBox.OnClick;

   if Assigned(ACheckBox.Action) then
   begin
     AH := ACheckBox.Action.OnExecute;
     ACheckBox.Action.OnExecute := nil;
   end;
   ACheckBox.OnClick := nil;
   ACheckBox.Checked := ACheck;
   if Assigned(ACheckBox.Action) then
   begin
     (ACheckBox.Action as TAction).Checked := ACheckBox.Checked;
     ACheckBox.Action.OnExecute := AH;
   end;
   ACheckBox.OnClick := EH;
 end;

 {$ifdef windows}
function TaskBarHeight: Integer;
var
  hTB    : HWND; // taskbar handle
  TBRect : TRect; // taskbar rectangle
begin
  hTB:= FindWindow('Shell_TrayWnd', '');
  if hTB = 0 then
    Result := 0
  else
  begin
    GetWindowRect(hTB, TBRect);
    Result := TBRect.Bottom - TBRect.Top;
  end;
end;
{$endif}

{

procedure AssociateFileExtension(const IconPath, ProgramName, Path, Extension: string);
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey(ProgramName, True);
    WriteString('', ProgramName);
    if IconPath <> '' then
    begin
      OpenKey(RC_DefaultIcon, True);
      WriteString('', IconPath);
    end;
    CloseKey;
    OpenKey(ProgramName, True);
    OpenKey('shell', True);
    OpenKey('open', True);
    OpenKey('command', True);
    WriteString('', '"' + Path + '" "%1"');
    Free;
  end;
  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey('.' + Extension, True);
    WriteString('', ProgramName);
    Free;
  end;
  RebuildIconCache;
end;

procedure AssociateExtension(const IconPath, ProgramName, Path, Extension: string);
begin
  AssociateFileExtension(IconPath, ProgramName, Path, Extension);
end;

}


//*****************************************************************************

end.
