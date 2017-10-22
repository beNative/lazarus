unit TableTool.Helpers;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Grids;

procedure ClearSelection(AGrid: TStringGrid);

procedure DeleteSelectedRows(AGrid: TStringGrid);

procedure DeleteSelectedCols(AGrid: TStringGrid);

procedure SelectAll(AGrid: TStringGrid);

function SelectionToCommaText(
  AGrid       : TStringGrid;
  AQuoteItems : Boolean = True
): string;

function SelectionToDelimitedTable(
  AGrid           : TStringGrid;
  ADelimiter      : string;
  AFirstRowHeader : Boolean
): string;

function SelectionToTextTable(
  AGrid           : TStringGrid;
  AFirstRowHeader : Boolean = True
): string;

function SelectionToTextTable2(
  AGrid           : TStringGrid;
  AFirstRowHeader : Boolean = True;
  ACellMargin     : Integer = 1; // one char position
  AMinColWidth    : Integer = 1
): string;

function SelectionToWikiTable(
  AGrid           : TStringGrid;
  AFirstRowHeader : Boolean = True
): string;

implementation

uses
  StrUtils;

procedure ClearSelection(AGrid: TStringGrid);
begin
  AGrid.Clean(AGrid.Selection, [gzNormal]);
end;

procedure DeleteSelectedRows(AGrid: TStringGrid);
var
  I : Integer;
  N : Integer;
begin
  I := AGrid.Selection.Top;
  N := AGrid.Selection.Bottom - I;
  while N >= 0 do
  begin
    AGrid.DeleteRow(I);
    N -= 1;
  end;
end;

procedure DeleteSelectedCols(AGrid: TStringGrid);
var
  I : Integer;
  N : Integer;
begin
  I := AGrid.Selection.Left;
  N := AGrid.Selection.Right - I;
  while N >= 0 do
  begin
    AGrid.DeleteCol(I);
    N -= 1;
  end;
end;

procedure SelectAll(AGrid: TStringGrid);
var
  R: TGridRect;
begin
  R.Left   := 1;
  R.Top    := 1;
  R.Right  := AGrid.ColCount - 1;
  R.Bottom := AGrid.RowCount - 1;
  AGrid.Selection := R;
end;

{ Ensure that all values that form a selection are true. Negative values can
  occur on selections that are made from bottom to top and/or from right to
  left. }

procedure NormalizeRect(var ARect: TRect);
var
  T: Integer;
begin
  if ARect.Top > ARect.Bottom then
  begin
    T := ARect.Top;
    ARect.Top := ARect.Bottom;
    ARect.Bottom := T;
  end;
  if ARect.Left > ARect.Right then
  begin
    T := ARect.Left;
    ARect.Left := ARect.Right;
    ARect.Right := T;
  end;
end;

{ Returns the width (in characters) of the longest line in the string. }

function GetTextWidth(const AText: string): Integer;
var
  SL      : TStringList;
  I, W, R : Integer;
begin
  SL := TStringList.Create;
  try
    SL.Text := AText;
    R := 0;
    for I := 0 to SL.Count - 1 do
    begin
      W := Length(SL[I]);
      if W > R then
        R := W;
    end;
    Result := R;
  finally
    SL.Free;
  end;
end;

function GetMaxTextWidth(AStrings: TStrings): Integer;
var
  I : Integer;
  N : Integer;
begin
  Result := 0;
  if Assigned(AStrings) then
  begin
    for I := 0 to AStrings.Count - 1 do
    begin
      N := GetTextWidth(AStrings[I]);
      if N > Result then
        Result := N;
    end;
  end;
end;

function SelectionToCommaText(AGrid: TStringGrid; AQuoteItems: Boolean): string;
var
  X, Y   : Integer;
  S, T   : string;
  SR     : TRect;
  CCount : Integer;
begin
  S := '';
  SR := AGrid.Selection;
  NormalizeRect(SR);
  CCount := SR.Right - SR.Left;
  for Y := SR.Top to SR.Bottom do
  begin
    for X := SR.Left to SR.Right do
    begin
      T := AGrid.Cells[X, Y];
      if AQuoteItems then
        T := QuotedStr(T);
      S := S + T;
      if X < SR.Right then
        S := S + ', ';
    end;
    if (CCount = 1) and (Y < SR.Bottom) then
      S := S + ', '
    else if Y < SR.Bottom then
      S := S + #13#10
  end;
  Result := S;
end;

function SelectionToDelimitedTable(AGrid: TStringGrid; ADelimiter: string;
  AFirstRowHeader: Boolean): string;
var
  X, Y : Integer;
  S, T : string;
  SL   : TStringList;
  SR   : TRect;
begin
  SL := TStringList.Create;
  try
    S := '';
    SR := AGrid.Selection;
    NormalizeRect(SR);
    if AFirstRowHeader then
    begin
      for X := SR.Left to SR.Right do
      begin
        S := S + AGrid.Cells[X, 0];
        if X < SR.Right then
          S := S + ADelimiter;
      end;
      SL.Add(S);
    end;
    for Y := SR.Top to SR.Bottom do
    begin
      S := '';
      for X := SR.Left to SR.Right do
      begin
        T := AGrid.Cells[X, Y];
        S := S + T;
        if X < SR.Right then
          S := S + ADelimiter;
      end;
      SL.Add(S);
    end;
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

function SelectionToTextTable(AGrid: TStringGrid; AFirstRowHeader: Boolean
  ): string;
var
  X, Y   : Integer;
  S      : string;
  N      : Integer;
  I      : Integer;
  LTxt   : string;
  LLine  : string;
  LFmt   : string;
  LTitle : string;
  Widths : array of Integer;
  SL     : TStringList;
  SR     : TRect;
  LTop   : Integer;
begin
  LLine := '';
  LTxt  := '';
  SR    := AGrid.Selection;
  NormalizeRect(SR);
  SetLength(Widths, SR.Right - SR.Left + 1);
  try
    SL := TStringList.Create;
    try
      for X := SR.Left to SR.Right do
      begin
        I := X - SR.Left;
        SL.Clear;
        for Y := SR.Top to SR.Bottom do
        begin
          S := AGrid.Cells[X, Y];
          SL.Add(S);
        end;
        Widths[I] := GetMaxTextWidth(SL);
      end;
    finally
      FreeAndNil(SL);
    end;

    for X := SR.Left to SR.Right do
    begin
      I     := X - SR.Left;
      N     := Widths[I];
      LFmt  := '%-' + IntToStr(N) + 's';
      LLine := LLine + '+' + Format(LFmt, [DupeString('-', N)]);
      if AFirstRowHeader then
      begin
        if X < AGrid.ColCount then
          LTitle := AGrid.Cells[X, SR.Top]
        else
          LTitle := '';
        LTxt := LTxt + '|' + Format(LFmt, [LTitle]);
      end;
    end;
    LLine := LLine + '+';

    if AFirstRowHeader then
    begin
      LTop := SR.Top + 1;
      LTxt   := LTxt + '|';
      Result := LLine + #13#10 + LTxt + #13#10 + LLine;
    end
    else
    begin
      LTop := SR.Top;
      Result := LLine;
    end;

    for Y := LTop to SR.Bottom do
    begin
      LTxt := '';
      for X := SR.Left to SR.Right do
      begin
        S := AGrid.Cells[X, Y];
        I := X - SR.Left;
        N := Widths[I];
        LFmt := '%-' + IntToStr(N) + 's';
        LTxt := LTxt + '|' + Format(LFmt, [S]);
      end;
      LTxt := LTxt + '|';
      Result := Result + #13#10 + LTxt;
      LTxt := '';
    end;
    Result := Result + #13#10 + LLine;
  finally
    Finalize(Widths);
  end;
end;

function SelectionToTextTable2(AGrid: TStringGrid; AFirstRowHeader: Boolean;
  ACellMargin: Integer; AMinColWidth: Integer): string;
var
  X, Y   : Integer;
  S      : string;
  N      : Integer;
  I      : Integer;
  LTxt   : string;
  LLine  : string;
  LFmt   : string;
  LTitle : string;
  Widths : array of Integer;
  SL     : TStringList;
  SR     : TRect;
  LTop   : Integer;
begin
  LLine := '';
  LTxt  := '';
  SR    := AGrid.Selection;
  NormalizeRect(SR);
  SetLength(Widths, SR.Right - SR.Left + 1);
  try
    SL := TStringList.Create;
    try
      for X := SR.Left to SR.Right do
      begin
        I := X - SR.Left;
        SL.Clear;
        for Y := SR.Top to SR.Bottom do
        begin
          S := AGrid.Cells[X, Y];
          SL.Add(S);
        end;
        Widths[I] := GetMaxTextWidth(SL) - 2 * ACellMargin + 1;
        if Widths[I] < (AMinColWidth - 2 * ACellMargin) then
          Widths[I] := (AMinColWidth - 2 * ACellMargin);
      end;
    finally
      FreeAndNil(SL);
    end;

    for X := SR.Left to SR.Right do
    begin
      I     := X - SR.Left;
      N     := Widths[I];
      LFmt  := '%' + IntToStr(ACellMargin) + 's' +  '%-' + IntToStr(N) + 's' + '%' + IntToStr(ACellMargin) + 's';
      LLine := LLine + '+' + Format(LFmt, ['-', DupeString('-', N), '-']);
      if AFirstRowHeader then
      begin
        if X < AGrid.ColCount then
          LTitle := AGrid.Cells[X, SR.Top]
        else
          LTitle := '';
        LTxt := LTxt + '|' + Format(LFmt, [' ', LTitle, ' ']);
      end;
    end;
    LLine := LLine + '+';

    if AFirstRowHeader then
    begin
      LTop := SR.Top + 1;
      LTxt   := LTxt + '|';
      Result := LLine + #13#10 + LTxt + #13#10 + LLine;
    end
    else
    begin
      LTop := SR.Top;
      Result := LLine;
    end;

    for Y := LTop to SR.Bottom do
    begin
      LTxt := '';
      for X := SR.Left to SR.Right do
      begin
        S := AGrid.Cells[X, Y];
        I := X - SR.Left;
        N := Widths[I];
        LFmt := '%' + IntToStr(ACellMargin) + 's' + '%-' + IntToStr(N) + 's'+ '%' + IntToStr(ACellMargin) + 's';
        LTxt := LTxt + '|' + Format(LFmt, [' ', S, ' ']);
      end;
      LTxt := LTxt + '|';
      Result := Result + #13#10 + LTxt;
      LTxt := '';
    end;
    Result := Result + #13#10 + LLine;
  finally
    Finalize(Widths);
  end;
end;

function SelectionToWikiTable(AGrid: TStringGrid; AFirstRowHeader: Boolean
  ): string;
var
  X, Y   : Integer;
  S      : string;
  sTxt   : string;
  sFmt   : string;
  sTitle : string;
  SR     : TRect;
  LTop   : Integer;
begin
  sTxt := '';
  SR := AGrid.Selection;
  NormalizeRect(SR);
  if AFirstRowHeader then
  begin
    for X := SR.Left to SR.Right do
    begin
      if X = SR.Right then
        sFmt := '||%s||'
      else
        sFmt := '||%s';
      if X < AGrid.ColCount then
        sTitle := AGrid.Cells[X, SR.Top]
      else
        sTitle := '';
      sTxt := sTxt + Format(sFmt, [sTitle]);
    end;
    Result := sTxt;
    LTop := SR.Top + 1;
  end
  else
    LTop := SR.Top;
  for Y := LTop to SR.Bottom do
  begin
    sTxt := '';
    for X := SR.Left to SR.Right do
    begin
      S := AGrid.Cells[X, Y];
      if X = SR.Right then
        sFmt := '|%s|'
      else
        sFmt := '|%s';
      sTxt := sTxt + Format(sFmt, [S]);
    end;
    if Result <> '' then
      Result := Result + #13#10 + sTxt
    else
      Result := sTxt;
  end;
end;

end.

