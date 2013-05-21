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

unit ts_Editor_CharacterMapDialog;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Grids, ComCtrls,

  LCLProc, LCLUnicodeData, ExtCtrls,

  ts_Editor_Interfaces, ts_Editor_CustomToolView;

type
  TfrmCharacterMapDialog = class(TCustomEditorToolView, IEditorToolView)
    cbxUnicodeRange    : TComboBox;
    imgChar            : TImage;
    lblCharInfo        : TLabel;
    pcMain             : TPageControl;
    grdANSI            : TStringGrid;
    grdUnicode         : TStringGrid;
    pnlChar            : TPanel;
    tsANSI             : TTabSheet;
    tsUnicode          : TTabSheet;
    lblUnicodeCharInfo : TLabel;

    procedure cbxUnicodeRangeSelect(Sender: TObject);
    procedure EditorSettingsChanged(Sender: TObject);
    procedure grdANSIMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure grdUnicodeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure grdANSIMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure grdUnicodeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

  strict private
    procedure FillCharMap;

  public
    procedure AfterConstruction; override;

  end;

//*****************************************************************************

implementation

{$ifdef windows}
uses
  Windows, Graphics;
{$endif}

{$R *.lfm}

resourcestring
  SCharacterMap = 'Character Map';

function RoundUp(Value, Divi: Integer): Integer;
begin
  if Value mod Divi = 0 then
    Result := Value div Divi
  else
    Result := (Value div Divi) + 1;
end;

{$ifdef windows}
function GetUnicodeBitmap(const UnicodeFont: string; const w: WideChar;
  const BitmapSize: Integer; var Size: TSize): TBitmap;
var
  MaxLogPalette: TMaxLogPalette;
begin
  MaxLogPalette.palVersion := $300;
  MaxLogPalette.palNumEntries := 2;
  with MaxLogPalette.palPalEntry[0] do
  begin
    peRed   := 0;
    peGreen := 0;
    peBlue  := 0;
    peFlags := 0
  end;

  with MaxLogPalette.palPalEntry[1] do
  begin
    peRed   := 255;
    peGreen := 255;
    peBlue  := 255;
    peFlags := 0
  end;

  Result := TBitmap.Create;

  Result.Height := BitmapSize;
  Result.Width  := BitmapSize;

  Result.Palette := CreatePalette(pLogPalette(@MaxLogPalette)^);

  Result.Canvas.Brush.Color := clWhite;
  Result.Canvas.FillRect(Result.Canvas.ClipRect);

  Result.Canvas.Font.Name := UnicodeFont;
  Result.Canvas.Font.Height := BitmapSize;

  GetTextExtentPoint32W(RESULT.Canvas.Handle, @w, 1, Size);

  TextOutW(Result.Canvas.Handle, (Result.Width - size.cx) DIV 2, 0, @w, 1)
end;
{$endif}

{$region 'construction and destruction' /fold}
//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmCharacterMapDialog.AfterConstruction;
var
  I : Integer;
begin
  inherited AfterConstruction;
  Caption := SCharacterMap;
  lblCharInfo.Caption := '-';
  lblUnicodeCharInfo.Caption := '-';
  grdANSI.Font.Assign(Manager.Settings.EditorFont);
  grdUnicode.Font.Assign(Manager.Settings.EditorFont);
  FillCharMap;
  grdANSI.AutoSizeColumns;
  cbxUnicodeRange.Items.Clear;
  for I := 0 to MaxUnicodeBlocks do
  begin
    cbxUnicodeRange.Items.Add(UnicodeBlocks[I].PG);
  end;
  pcMain.ActivePageIndex := 0;
  cbxUnicodeRange.ItemIndex := 0;
  cbxUnicodeRangeSelect(nil);
  Manager.Settings.AddEditorSettingsChangedHandler(EditorSettingsChanged);
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************
{$endregion}

{$region 'event handlers' /fold}
//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

procedure TfrmCharacterMapDialog.cbxUnicodeRangeSelect(Sender: TObject);
var
  N : Integer;
  X : Integer;
  Y : Integer;
  S : Integer;
  E : Integer;
begin
  S := UnicodeBlocks[cbxUnicodeRange.ItemIndex].S;
  E := UnicodeBlocks[cbxUnicodeRange.ItemIndex].E;
  grdUnicode.Clear;
  grdUnicode.ColCount := 16;
  grdUnicode.RowCount := RoundUp(E - S, 16);
  N                  := 0;
  for Y := 0 to grdUnicode.RowCount - 1 do
    for X := 0 to grdUnicode.ColCount - 1 do
    begin
      if S + N <= E then
        grdUnicode.Cells[X, Y] := UnicodeToUTF8(S + N);
      Inc(N);
    end;
  grdUnicode.AutoSizeColumns;
end;

procedure TfrmCharacterMapDialog.EditorSettingsChanged(Sender: TObject);
begin
  grdANSI.Font.Assign(Manager.Settings.EditorFont);
  grdUnicode.Font.Assign(Manager.Settings.EditorFont);
end;

procedure TfrmCharacterMapDialog.grdANSIMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R : Integer;
  C : Integer;
begin
  R := 0;
  C := 0;
  if (Button = mbLeft) and (grdANSI.MouseToGridZone(X, Y) = gzNormal) then
  begin
    grdANSI.MouseToCell(X, Y, C, R);
    if grdANSI.Cells[C, R] <> '' then
      View.InsertTextAtCaret(grdANSI.Cells[C, R])
  end;
end;

procedure TfrmCharacterMapDialog.grdUnicodeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R : Integer;
  C : Integer;
begin
  R := 0;
  C := 0;
  if (Button = mbLeft) and (grdUnicode.MouseToGridZone(X, Y) = gzNormal) then
  begin
    grdUnicode.MouseToCell(X, Y, C, R);
    View.InsertTextAtCaret(grdUnicode.Cells[C, R])
  end;
end;

procedure TfrmCharacterMapDialog.grdANSIMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  B : Byte;
  R : Integer;
  C : Integer;
  WC: Char;
  S: TSize;
  Bmp: TBitmap;
begin
  R := 0;
  C := 0;
  if grdANSI.MouseToGridZone(X, Y) = gzNormal then
  begin
    grdANSI.MouseToCell(X, Y, C, R);
    if grdANSI.Cells[C, R] <> '' then
    begin
      WC := UTF8ToAnsi(grdANSI.Cells[C, R])[1];
      B  := Ord(WC);
      lblCharInfo.Caption := 'Decimal = ' + IntToStr(B) +
        ', Hex = $' + HexStr(B, 2);
      Bmp:= GetUnicodeBitmap(grdAnsi.Font.Name, WC, imgChar.Height, S);
        try
          imgChar.Picture.Graphic := Bmp
        finally
          Bmp.Free
        end;
    end
    else
      lblCharInfo.Caption := '-';
  end
  else
  begin
    lblCharInfo.Caption := '-';
  end;
end;

procedure TfrmCharacterMapDialog.grdUnicodeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  R  : Integer;
  C  : Integer;
  I  : Integer;
  S  : Cardinal;
  T1 : string;
  T2 : string;
  WC: WideChar;
  Size: TSize;
  Bmp: TBitmap;
begin
  R := 0;
  C := 0;
  if grdUnicode.MouseToGridZone(X, Y) = gzNormal then
  begin
    grdUnicode.MouseToCell(X, Y, C, R);
    S  := UnicodeBlocks[cbxUnicodeRange.ItemIndex].S + (C) + (R * 16);
    T1 := UnicodeToUTF8(S);
    T2 := '';
    for I := 1 to Length(T1) do
      T2 := T2 + '$' + IntToHex(Ord(T1[I]), 2);
    lblUnicodeCharInfo.Caption := 'U+' + IntToHex(S, 4) + ', UTF-8 = ' + T2;

    WC := WideChar(S);
    Bmp:= GetUnicodeBitmap(grdUnicode.Font.Name, WC, imgChar.Height, Size);
      try
        imgChar.Picture.Graphic := Bmp
      finally
        Bmp.Free
      end;
  end
  else
  begin
    lblCharInfo.Caption := '-';
  end;
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************
{$endregion}

{$region 'private methods' /fold}
//*****************************************************************************
// private methods                                                       BEGIN
//*****************************************************************************

procedure TfrmCharacterMapDialog.FillCharMap;
var
  R : Integer;
  C : Integer;
begin
  for R := 0 to Pred(grdANSI.RowCount) do
  begin
    if R <> 0 then
      grdANSI.Cells[0, R] := Format('%.3d +', [Succ(R) * 16]);
    for C := 1 to Pred(grdANSI.ColCount) do
    begin
      if R = 0 then
        grdANSI.Cells[C, R] := Format('%.2d', [Pred(C)])
      else
        grdANSI.Cells[C, R] := AnsiToUTF8(Chr(Succ(R) * 16 + Pred(C)));
    end;
  end;
end;

//*****************************************************************************
// private methods                                                         END
//*****************************************************************************
{$endregion}

end.

