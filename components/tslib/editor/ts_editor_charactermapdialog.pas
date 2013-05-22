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
  Classes, SysUtils, Forms, Controls, StdCtrls, Grids, ComCtrls, ExtCtrls,

  LCLProc, LCLUnicodeData,

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
    shpChar            : TShape;
    tsANSI             : TTabSheet;
    tsUnicode          : TTabSheet;
    lblUnicodeCharInfo : TLabel;

    procedure cbxUnicodeRangeSelect(Sender: TObject);
    procedure grdANSIKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure grdANSIMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure grdANSISelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure grdUnicodeKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure grdUnicodeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure grdANSIMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure grdUnicodeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure grdUnicodeSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);

  strict private
    procedure EditorSettingsChanged(Sender: TObject);

    procedure FillCharMap;
    procedure UpdateCharacterBitmap(const ACharacter: string);
    procedure UpdateUnicodeDisplay(ACol, ARow: Integer);
    procedure UpdateANSIDisplay(ACol, ARow: Integer);

  public
    procedure AfterConstruction; override;
  end;

//*****************************************************************************

implementation

uses
  Graphics, Dialogs,

  LCLType, LCLIntf;

{$R *.lfm}

resourcestring
  SCharacterMap = 'Character Map';

{$region 'non-interfaced routines' /fold}
//*****************************************************************************
// non-interfaced routines                                               BEGIN
//*****************************************************************************

function RoundUp(Value, Divi: Integer): Integer;
begin
  if Value mod Divi = 0 then
    Result := Value div Divi
  else
    Result := (Value div Divi) + 1;
end;

function CreateCharacterBitmap(
  const AFontName     : string;
  const ACharacter    : string;
  const ABitmapHeight : Integer
): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Height := ABitmapHeight;
  Result.Width  := ABitmapHeight;
  Result.Canvas.Brush.Color := clWhite;
  Result.Canvas.FillRect(Result.Canvas.ClipRect);
  Result.Canvas.Font.Name := AFontName;
  Result.Canvas.Font.Height := ABitmapHeight;
  Result.Canvas.TextOut(
    (Result.Width - Result.Canvas.GetTextWidth(ACharacter)) div 2,
    0,
    ACharacter
  );
end;

//*****************************************************************************
// non-interfaced routines                                                 END
//*****************************************************************************
{$endregion}

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
  N                   := 0;
  for Y := 0 to grdUnicode.RowCount - 1 do
    for X := 0 to grdUnicode.ColCount - 1 do
    begin
      if S + N <= E then
        grdUnicode.Cells[X, Y] := UnicodeToUTF8(S + N);
      Inc(N);
    end;
  grdUnicode.AutoSizeColumns;
end;

procedure TfrmCharacterMapDialog.grdANSIKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    View.InsertTextAtCaret(grdANSI.Cells[grdANSI.Col, grdANSI.Row]);
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

procedure TfrmCharacterMapDialog.grdANSISelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  UpdateANSIDisplay(aCol, aRow);
end;

procedure TfrmCharacterMapDialog.grdUnicodeKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    View.InsertTextAtCaret(grdUnicode.Cells[grdUnicode.Col, grdUnicode.Row]);
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
  B   : Byte;
  Row : Integer;
  Col : Integer;
begin
  Row := 0;
  Col := 0;
  if grdANSI.MouseToGridZone(X, Y) = gzNormal then
  begin
    grdANSI.MouseToCell(X, Y, Col, Row);
    UpdateANSIDisplay(Col, Row);
  end
  else
  begin
    lblCharInfo.Caption := '-';
  end;
end;

procedure TfrmCharacterMapDialog.grdUnicodeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Row   : Integer;
  Col   : Integer;
begin
  Row := 0;
  Col := 0;
  if grdUnicode.MouseToGridZone(X, Y) = gzNormal then
  begin
    grdUnicode.MouseToCell(X, Y, Col, Row);
    UpdateUnicodeDisplay(Col, Row);
  end
  else
  begin
    lblCharInfo.Caption := '-';
  end;
end;

procedure TfrmCharacterMapDialog.grdUnicodeSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  UpdateUnicodeDisplay(aCol, aRow);
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

procedure TfrmCharacterMapDialog.UpdateCharacterBitmap(const ACharacter: string);
var
  Bmp: TBitmap;
begin
  Bmp:= CreateCharacterBitmap(
    Manager.Settings.EditorFont.Name,
    ACharacter,
    imgChar.Height
  );
  try
    imgChar.Picture.Graphic := Bmp
  finally
    Bmp.Free;
  end;
end;

procedure TfrmCharacterMapDialog.UpdateUnicodeDisplay(ACol, ARow: Integer);
var
  I     : Integer;
  Start : Cardinal;
  T1    : string;
  T2    : string;
begin
  Start  := UnicodeBlocks[cbxUnicodeRange.ItemIndex].S + ACol + (ARow * 16);
  T1 := UnicodeToUTF8(Start);
  T2 := '';
  for I := 1 to Length(T1) do
    T2 := T2 + '$' + IntToHex(Ord(T1[I]), 2);
  lblUnicodeCharInfo.Caption := 'U+' + IntToHex(Start, 4) + ', UTF-8 = ' + T2;
  UpdateCharacterBitmap(grdUnicode.Cells[ACol, ARow]);
end;

procedure TfrmCharacterMapDialog.UpdateANSIDisplay(ACol, ARow: Integer);
var
  B: Byte;
begin
  if grdANSI.Cells[ACol, ARow] <> '' then
  begin
    B  := Ord(grdANSI.Cells[ACol, ARow][1]);
    lblCharInfo.Caption := 'Decimal = ' + IntToStr(B) +
      ', Hex = $' + HexStr(B, 2);
    UpdateCharacterBitmap(grdANSI.Cells[ACol, ARow]);
  end
  else
    lblCharInfo.Caption := '-';
end;

//*****************************************************************************
// private methods                                                         END
//*****************************************************************************
{$endregion}

end.

