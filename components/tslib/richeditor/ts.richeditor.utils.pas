{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit ts.RichEditor.Utils;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, TypInfo,

  KMemo, KGraphics;

// Block info routines
function BlockInfoString(ABlock: TKMemoBlock): string;
function TableInfoString(ABlock: TKMemoTable): string;
function TableRowInfoString(ABlock: TKMemoTableRow): string;
function TableCellInfoString(ABlock: TKMemoTableCell): string;
function ParaStyleInfoString(AParaStyle: TKMemoParaStyle): string;
function ImageInfoString(ABlock: TKMemoImageBlock): string;

implementation

function BlockInfoString(ABlock: TKMemoBlock): string;
var
  S : string;
begin
  S := '';
  if Assigned(ABlock.ParaStyle) then
    S := ParaStyleInfoString(ABlock.ParaStyle);
  if ABlock.SelLength > 0 then
  begin
    if not S.IsEmpty then
      S := S + sLineBreak;
    S := S + Format('SelStart = %d; SelEnd = %d; SelLength = %d', [
      ABlock.SelStart, ABlock.SelEnd, ABlock.SelLength]);
  end;
  Result := S;
end;

function TableInfoString(ABlock: TKMemoTable): string;
begin
  Result := Format('%d x %d', [ABlock.ColCount, ABlock.RowCount]);
end;

function TableRowInfoString(ABlock: TKMemoTableRow): string;
begin
  if ABlock.CellCount > 0 then
    Result := Format('%d', [ABlock.Cells[0].RowIndex])
  else
    Result := 'empty row';
end;

function TableCellInfoString(ABlock: TKMemoTableCell): string;
begin
  Result := Format('[%d(%d), %d(%d)]',
    [ABlock.ColIndex, ABlock.ColSpan, ABlock.RowIndex, ABlock.RowSpan]);
end;

function ParaStyleInfoString(AParaStyle: TKMemoParaStyle): string;
var
  LAlign : string;
begin
  case AParaStyle.HAlign of
    halLeft   : LAlign := 'Left';
    halRight  : LAlign := 'Right';
    halCenter : LAlign := 'Center';
  else
    LAlign := 'Justify';
  end;
  Result := Format('Alignment: %s', [LAlign]) + sLineBreak +
    Format('Margins: %d, %d, %d, %d', [
      AParaStyle.LeftMargin, AParaStyle.RightMargin,
      AParaStyle.TopMargin, AParaStyle.BottomMargin
    ]) + sLineBreak +
    Format('Numbering (List/ListLevel/StartAt): %d, %d, %d', [
      AParaStyle.NumberingList, AParaStyle.NumberingListLevel,
      AParaStyle.NumberStartAt]);
  Result := Result + Format('FirstIndent: %d', [AParaStyle.FirstIndent])
    + sLineBreak;
  Result := Result +
    Format('LineSpacingFactor: %f', [AParaStyle.LineSpacingFactor]) + sLineBreak;
  Result := Result +
    Format('LineSpacingMode: %s', [
      GetEnumName(TypeInfo(TKMemoLineSpacingMode),
      Integer(AParaStyle.LineSpacingMode))]) + sLineBreak;
  Result := Result + Format('LineSpacingValue: %d',
    [AParaStyle.LineSpacingValue]) + sLineBreak;
  Result := Result + Format('WordWrap: %s',
    [BoolToStr(AParaStyle.WordWrap, True)]);

end;

function ImageInfoString(ABlock: TKMemoImageBlock): string;
var
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add(Format('Block size: %d x %d',
      [ABlock.Height, ABlock.Width]));
    SL.Add(Format('Image size: %d x %d',
      [ABlock.ImageHeight, ABlock.ImageWidth]));
    SL.Add(Format('Explicit size: %d x %d',
      [ABlock.ExplicitHeight, ABlock.ExplicitWidth]));
    SL.Add(Format('Native/Explicit size: %d x %d',
      [ABlock.NativeOrExplicitHeight, ABlock.NativeOrExplicitWidth]));
    SL.Add(Format('Scale size: %d x %d',
      [ABlock.ScaleHeight, ABlock.ScaleWidth]));
    SL.Add(Format('LogScale X/Y: %d/%d', [ABlock.LogScaleX, ABlock.LogScaleY]));
    SL.Add(Format('Scale X/Y: %d/%d', [ABlock.ScaleX, ABlock.ScaleY]));
    SL.Add(Format('ImageDPI X/Y: %d/%d', [ABlock.ImageDPIX, ABlock.ImageDPIY]));
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

end.

