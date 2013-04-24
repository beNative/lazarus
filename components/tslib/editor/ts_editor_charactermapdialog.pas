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
  StdCtrls, Grids, ComCtrls,

  LCLType, LCLProc, LCLUnicodeData, ButtonPanel,

  GraphType,

  ts_Editor_Interfaces;

type
  TfrmCharacterMapDialog = class(TForm, IEditorToolView)
    cbxUnicodeRange    : TComboBox;
    pnlButtons         : TButtonPanel;
    lblCharInfo        : TLabel;
    pcMain             : TPageControl;
    grdANSI            : TStringGrid;
    grdUnicode         : TStringGrid;
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

  private
    procedure FillCharMap;
    function GetManager: IEditorManager;
    function GetView: IEditorView;

  protected
    function GetForm: TForm;
    function GetName:string;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    procedure UpdateView;

    property Visible: Boolean
      read GetVisible write SetVisible;

    property Name: string
      read GetName;

    property Form: TForm
      read GetForm;

    property Manager: IEditorManager
      read GetManager;

    property View: IEditorView
      read GetView;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

//*****************************************************************************

implementation

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

{$region 'construction and destruction' /fold}
//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmCharacterMapDialog.AfterConstruction;
var
  I : Integer;
begin
  inherited AfterConstruction;
  Caption               := SCharacterMap;
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
  cbxUnicodeRange.ItemIndex := 0;
  cbxUnicodeRangeSelect(nil);
  Manager.Settings.AddEditorSettingsChangedHandler(EditorSettingsChanged);
end;

procedure TfrmCharacterMapDialog.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************
{$endregion}

{$region 'property access mehods' /fold}
//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TfrmCharacterMapDialog.GetForm: TForm;
begin
  Result := Self;
end;

function TfrmCharacterMapDialog.GetName: string;
begin
  Result := inherited Name;
end;

function TfrmCharacterMapDialog.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

procedure TfrmCharacterMapDialog.SetVisible(AValue: Boolean);
begin
  inherited Visible := AValue;
end;

function TfrmCharacterMapDialog.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TfrmCharacterMapDialog.GetView: IEditorView;
begin
  Result := Owner as IEditorView;
end;

//*****************************************************************************
// property access methods                                                 END
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
begin
  R := 0;
  C := 0;
  if grdANSI.MouseToGridZone(X, Y) = gzNormal then
  begin
    grdANSI.MouseToCell(X, Y, C, R);
    if grdANSI.Cells[C, R] <> '' then
    begin
      B               := Ord(UTF8ToAnsi(grdANSI.Cells[C, R])[1]);
      lblCharInfo.Caption := 'Decimal = ' + IntToStr(B) +
        ', Hex = $' + HexStr(B, 2);
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
begin
  R := 0;
  C := 0;
  if grdUnicode.MouseToGridZone(X, Y) = gzNormal then
  begin
    grdUnicode.MouseToCell(X, Y, C, R);
    S    := UnicodeBlocks[cbxUnicodeRange.ItemIndex].S + (C) + (R * 16);
    T1  := UnicodeToUTF8(S);
    T2 := '';
    for I := 1 to Length(T1) do
      T2 := T2 + '$' + IntToHex(Ord(T1[I]), 2);
    lblUnicodeCharInfo.Caption := 'U+' + IntToHex(S, 4) + ', UTF-8 = ' + T2;
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


{$region 'protected methods' /fold}
//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TfrmCharacterMapDialog.UpdateView;
begin
//
end;

//function TfrmCharacterMapDialog.Focused: Boolean;
//begin
//  Result := Focused;
//end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************
{$endregion}

end.

