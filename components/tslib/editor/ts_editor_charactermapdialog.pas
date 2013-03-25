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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, Grids, ButtonPanel, ComCtrls,

  LCLType, LCLProc, LCLUnicodeData,

  GraphType;

type
  TOnInsertCharacterEvent = procedure(const C: TUTF8Char) of object;

  { TCharacterMapDialog }

  TCharacterMapDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    CharInfoLabel: TLabel;
    lbxCategories: TListBox;
    PageControl1: TPageControl;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    UnicodeCharInfoLabel: TLabel;

    procedure lbxCategoriesSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure StringGrid2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

  private
    FOnInsertCharacter: TOnInsertCharacterEvent;
    procedure FillCharMap;

  public
    property OnInsertCharacter: TOnInsertCharacterEvent
      read FOnInsertCharacter write FOnInsertCharacter;
  end;

procedure ShowCharacterMap(AOnInsertChar: TOnInsertCharacterEvent);

implementation

{$R *.lfm}

resourcestring
  // character map
  lisCharacterMap = 'Character Map';

var
  CharacterMapDialog: TCharacterMapDialog;

procedure ShowCharacterMap(AOnInsertChar: TOnInsertCharacterEvent);
begin
  if CharacterMapDialog = nil then
    Application.CreateForm(TCharacterMapDialog, CharacterMapDialog);

  CharacterMapDialog.OnInsertCharacter := AOnInsertChar;
  CharacterMapDialog.Show;
end;

function RoundUp(Value, Divi: Integer): Integer;
begin
  if Value mod Divi = 0 then
    Result := Value div Divi
  else
    Result := (Value div Divi) + 1;
end;

{ TCharacterMapDialog }

procedure TCharacterMapDialog.FormCreate(Sender: TObject);
begin
  Caption               := lisCharacterMap;
  CharInfoLabel.Caption := '-';
  UnicodeCharInfoLabel.Caption := '-';
  FillCharMap;
end;

//function RoundUp(Value, Divi:integer):integer;
//begin
//  if Value mod Divi=0 then
//   Result:=Value div Divi else
//   Result:=(Value div Divi)+1;
//end;


procedure TCharacterMapDialog.lbxCategoriesSelect(Sender: TObject);
var
  cnt, x, y: Integer;
  S, E: Integer;
begin
  S := UnicodeBlocks[lbxCategories.ItemIndex].S;
  E := UnicodeBlocks[lbxCategories.ItemIndex].E;
  StringGrid2.Clear;
  StringGrid2.ColCount := 16;
  StringGrid2.RowCount := RoundUp(E - S, 16);
  cnt                  := 0;
  for y := 0 to StringGrid2.RowCount - 1 do
    for x := 0 to StringGrid2.ColCount - 1 do
    begin
      if S + Cnt <= E then
        StringGrid2.Cells[x, y] := UnicodeToUTF8(S + Cnt);
      Inc(cnt);
    end;
  StringGrid2.AutoSizeColumns;
end;

procedure TCharacterMapDialog.FormShow(Sender: TObject);
var
  i: Integer;
begin
  StringGrid1.Font.Size := 12;
  StringGrid2.Font.Size := 12;

  StringGrid1.AutoSizeColumns;
  lbxCategories.Items.Clear;
  for i := 0 to MaxUnicodeBlocks do
  begin
    lbxCategories.Items.Add(UnicodeBlocks[i].PG);
    lbxCategories.Items.Add(UnicodeBlocks[i].PG);
  end;
  lbxCategories.ItemIndex := 0;
  lbxCategoriesSelect(nil);
end;

procedure TCharacterMapDialog.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Row, Col: Integer;
begin
  Row := 0;
  Col := 0;
  if (Button = mbLeft) and (StringGrid1.MouseToGridZone(X, Y) = gzNormal) then
  begin
    StringGrid1.MouseToCell(X, Y, Col, Row);
    if (StringGrid1.Cells[Col, Row] <> '') and
      (Assigned(OnInsertCharacter)) then
      OnInsertCharacter(StringGrid1.Cells[Col, Row]);
  end;
end;

procedure TCharacterMapDialog.StringGrid1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  CharOrd: Byte;
  Row, Col: Integer;
begin
  Row := 0;
  Col := 0;
  if StringGrid1.MouseToGridZone(X, Y) = gzNormal then
  begin
    StringGrid1.MouseToCell(X, Y, Col, Row);

    if StringGrid1.Cells[Col, Row] <> '' then
    begin
      CharOrd               := Ord(UTF8ToAnsi(StringGrid1.Cells[Col, Row])[1]);
      CharInfoLabel.Caption := 'Decimal = ' + IntToStr(CharOrd) +
        ', Hex = $' + HexStr(CharOrd, 2);
    end
    else
      CharInfoLabel.Caption := '-';
  end
  else
  begin
    CharInfoLabel.Caption := '-';
  end;
end;

procedure TCharacterMapDialog.StringGrid2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Row, Col: Integer;
begin
  Row := 0;
  Col := 0;
  if (Button = mbLeft) and (StringGrid2.MouseToGridZone(X, Y) = gzNormal) then
  begin
    StringGrid2.MouseToCell(X, Y, Col, Row);
    if Assigned(OnInsertCharacter) then
      OnInsertCharacter(StringGrid2.Cells[Col, Row]);
  end;
end;

procedure TCharacterMapDialog.StringGrid2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Row, Col, i: Integer;
  S: Cardinal;
  tmp, tmp2: string;
begin
  Row := 0;
  Col := 0;
  if StringGrid2.MouseToGridZone(X, Y) = gzNormal then
  begin
    StringGrid2.MouseToCell(X, Y, Col, Row);
    S    := UnicodeBlocks[lbxCategories.ItemIndex].S + (Col) + (Row * 16);
    tmp  := UnicodeToUTF8(S);
    tmp2 := '';
    for i := 1 to Length(tmp) do
      tmp2 := tmp2 + '$' + IntToHex(Ord(tmp[i]), 2);
    UnicodeCharInfoLabel.Caption :=
      'U+' + inttohex(S, 4) + ', UTF-8 = ' + tmp2;
  end
  else
  begin
    CharInfoLabel.Caption := '-';
  end;
end;

procedure TCharacterMapDialog.FillCharMap;
var
  R, C: Integer;
begin
  for R := 0 to Pred(StringGrid1.RowCount) do
  begin
    if R <> 0 then
      StringGrid1.Cells[0, R] := Format('%.3d +', [Succ(R) * 16]);
    for C := 1 to Pred(StringGrid1.ColCount) do
    begin
      if R = 0 then
        StringGrid1.Cells[C, R] := Format('%.2d', [Pred(C)])
      else
        StringGrid1.Cells[C, R] := AnsiToUTF8(Chr(Succ(R) * 16 + Pred(C)));
    end;
  end;
end;

end.

