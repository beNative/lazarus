{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Components.SynMiniMap;

{$MODE DELPHI}

{$REGION 'comments'}
{
  Changes by Tim Sinaeve for Lazarus port:
  - Changed TBufferCoord to TPoint
  - removed registration procedure
  - replaced virtual functions in TSynMiniMapEditorPlugin

}
(*******************************************************************************
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/    Contributors(alphabetical order):    /
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Dorin Duminica - http://www.delphigeist.com

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Change Log:

 ~~~~~~~~~~~~~~~~
/    v 1.1     /
~~~~~~~~~~~~~~~

    new:
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -	editor scroll from MiniMap
    -	options class(to be extended)
    -	published a few more properties

    fixes:
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -	fixed flickering on scroll
    -	fixed line number calculation on click

    other:
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -	cleanups

    known issues:
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -	translation of "char" when clicking the map is
      not always correct needs rewrite

 ~~~~~~~~~~~~~~~
/    v 1.0    /
~~~~~~~~~~~~~~

    -	initial release

*******************************************************************************)
{$ENDREGION}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Classes, StdCtrls, Forms, Controls, Graphics, GraphMath,

  SynEdit, LazSynEditText;

type
  TBitmap = Graphics.TBitmap;

const
  SYNMINIMAP_DEFAULT_HEIGHT           = 4000;
  SYNMINIMAP_DEFAULT_WIDTH            = 200;
  SYNMINIMAP_DEFAULT_FONTFACTOR       = 5;
  SYNMINIMAP_FONTFACTOR_MIN           = 1; //2
  SYNMINIMAP_FONTFACTOR_MAX           = 10; //4
  SYNMINIMAP_DEFAULT_OPTIONS_TABWIDTH = 4;

type
  TSynMiniMapCanvasHelper = class helper for TCanvas
  public
    procedure StretchDrawHalftone(const AX, AY, AWidth, AHeight: Integer;
      const ASource: TCanvas;
      const ASrcX, ASrcY, ASrcWidth, ASrcHeight: Integer); overload;
    procedure StretchDrawHalftone(const APoint: TPoint;
      const AWidth, AHeight: Integer; const ASource: TCanvas;
      const ASrcPoint: TPoint; ASrcWidth, ASrcHeight: Integer); overload;
  end;

type
  ///
  ///  don't modify this, it will be extended to pass other information
  ///  in the future without breaking backwards compatibility
  ///
  PSynMiniMapEventData = ^TSynMiniMapEventData;
  TSynMiniMapEventData = record
    Coord: TPoint;
    Redraw: Boolean;
  end;

const
  szSynMiniMapEventData = SizeOf(TSynMiniMapEventData);

type
  ///
  ///  colors for mini map
  ///
  TSynMiniMapColors = class(TPersistent)
  private
    FBackground: TColor;
    FHighlight: TColor;
    FPreviousLine: TColor;
    FPreviousLineText: TColor;
    FText: TColor;
    FTextHighlight: TColor;
  public
    constructor Create(AOwner: TComponent);
  published
    property Background: TColor read FBackground write FBackground;
    property Highlight: TColor read FHighlight write FHighlight;
    property PreviousLine: TColor read FPreviousLine write FPreviousLine;
    property PreviousLineText: TColor read FPreviousLineText write FPreviousLineText;
    property Text: TColor read FText write FText;
    property TextHighlight: TColor read FTextHighlight write FTextHighlight;
  end;

  ///
  ///  various behavioral options
  ///
  TSynMinimapOptions = class(TPersistent)
  private
    FAllowScroll: Boolean;
    FReverseScroll: Boolean;
    FTabWidthOverride: Boolean;
    FTabWidth: Integer;
  public
    constructor Create(AOwner: TComponent);
  published
    ///
    ///  scrolling editor using the MiniMap is possible only if AllowScroll
    ///
    property AllowScroll: Boolean read FAllowScroll write FAllowScroll;
    property ReverseScroll: Boolean read FReverseScroll write FReverseScroll;
    property TabWidthOverride: Boolean read FTabWidthOverride write FTabWidthOverride;
    property TabWidth: Integer read FTabWidth write FTabWidth;
  end;

  ///
  ///  event fired under various conditions
  ///
  TSynMiniMapEvent = procedure (
    Sender : TObject;
    Data   : PSynMiniMapEventData
  ) of object;


  ///
  ///  forward declaration
  ///
  TSynMiniMap = class;

  ///
  ///  this plugin helps hook a few important events
  ///
  TSynMiniMapEditorPlugin = class(TLazSynEditPlugin)
  protected
    procedure LineCountChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
    procedure EditorDecPaintLock(Sender: TObject);

  private
    FSynMiniMap : TSynMiniMap;

  public
    constructor Create(ASynMiniMap: TSynMiniMap); reintroduce; virtual;

  end;

  ///
  ///  the minimap itself
  ///
  TSynMiniMap = class(TCustomControl)
  protected
    procedure Resize; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure DoClick(const AX, AY: Integer); virtual;

  private
    FFullSizeBitmap: TBitmap;
    FOffsetBitmap: TBitmap;
    FEditor: TSynEdit;

  private
    FEditorHeight: Integer;
    FEditorWidth: Integer;
    FEditorRealWidth: Integer;
    FFirstLine: Integer;
    FLastLine: Integer;
    FMaxCharsPerLine: Word;
    FLineHeightInPixels: Integer;
    FPreviousLineIndex: Integer;
    FFontFactor: Single;
    FCharWidth: Integer;
    FTabWidth: Integer;
    FOptions: TSynMinimapOptions;
    ///
    ///  mouse down & move => scroll
    ///  mouse down + up => click
    ///
    FMouseDownPoint: TPoint;
    FMouseUpPoint: TPoint;
    FScrolling: Boolean;
    FOnClick: TSynMiniMapEvent;
    FScrollBar: TScrollBar;
    FColors: TSynMiniMapColors;
    FMiniMapPlugin: TSynMiniMapEditorPlugin;

    function GetClickCoord: TPoint;
    procedure ResetInternals;
    procedure ClearEventData(var AEventData: TSynMiniMapEventData); inline;

    function GetPixelFormat: TPixelFormat;
    procedure SetPixelFormat(const Value: TPixelFormat);
    procedure SetEditor(const Value: TSynEdit);
    procedure SetFontFactor(const Value: Single);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;


    function TranslatePoint(const APoint: PPoint): TPoint;
    procedure Render; virtual;

    property PreviousLineIndex : Integer
      read FPreviousLineIndex;

    property Scrolling : Boolean
      read FScrolling;

  published
    property Colors : TSynMiniMapColors
      read FColors write FColors;

    property Editor : TSynEdit
      read FEditor write SetEditor;

    property FontFactor : Single
      read FFontFactor write SetFontFactor;

    property Options : TSynMinimapOptions
      read FOptions write FOptions;

    property PixelFormat : TPixelFormat
      read GetPixelFormat write SetPixelFormat;

    property OnClick : TSynMiniMapEvent
      read FOnClick write FOnClick;

    property Align;
    property Constraints;
    property Height;
    property ShowHint;
    property Width;
    property OnDblClick;
    property OnResize;
  end;

implementation

uses
  Math, Dialogs;

resourcestring
  SFontFactorMinMax = 'Font factor cannot be smaller than %.d or greater than %.d.';

{ TSynMiniMapCanvasHelper }

procedure TSynMiniMapCanvasHelper.StretchDrawHalftone(const AX, AY, AWidth,
  AHeight: Integer; const ASource: TCanvas;
  const ASrcX, ASrcY, ASrcWidth, ASrcHeight: Integer);
begin
  ///
  ///  helper function for stretch draw of full size bitmap on to
  ///  a smaller one
  ///

  {
  SetStretchBltMode(Self.Handle, HALFTONE);
  StretchBlt(
    Self.Handle,
    AX, AY,
    AWidth, AHeight,
    ASource.Handle,
    ASrcX, ASrcY,
    ASrcWidth, ASrcHeight,
    SRCCOPY
  );

  }
end;

procedure TSynMiniMapCanvasHelper.StretchDrawHalftone(const APoint: TPoint;
  const AWidth, AHeight: Integer; const ASource: TCanvas;
  const ASrcPoint: TPoint; ASrcWidth, ASrcHeight: Integer);
begin
  StretchDrawHalftone(
    APoint.X, APoint.Y,
    AWidth, AHeight,
    ASource,
    ASrcPoint.X, ASrcPoint.Y,
    ASrcWidth, ASrcHeight);
end;

{ TSynMiniMapColors }

constructor TSynMiniMapColors.Create(AOwner: TComponent);
begin
  inherited Create;
  ///
  ///  set default values
  ///
  Background := clWhite;
  //Highlight := $f4f4f4;
  Highlight := clSkyBlue;
  PreviousLine := clNone;
  PreviousLineText := clNone;
  Text := clGray;
  TextHighlight := clBlack;
end;

{ TSynMinimapOptions }

constructor TSynMinimapOptions.Create(AOwner: TComponent);
begin
  inherited Create;
  AllowScroll := False;
  ReverseScroll := False;
  TabWidthOverride := False;
  TabWidth := SYNMINIMAP_DEFAULT_OPTIONS_TABWIDTH;
end;

{ TSynMiniMapEditorPlugin }

procedure TSynMiniMapEditorPlugin.LineCountChanged(Sender: TSynEditStrings;
  AIndex, ACount: Integer);
//var
//  LLineIndex: Integer;
begin
  ///
  ///  check if we need to decrement the previous line index
  ///  if current line index is 10 and the user deleted a few lines
  ///  before that, we need to adjust FPreviousLineIndex
  ///
  //if FSynMiniMap.PreviousLineIndex >= LLineIndex then
  //begin
  //  Inc(FSynMiniMap.FPreviousLineIndex, ACount);
  //  FSynMiniMap.Render;
  //end;
end;

procedure TSynMiniMapEditorPlugin.EditorDecPaintLock(Sender: TObject);
begin
  FSynMiniMap.Render;
end;

constructor TSynMiniMapEditorPlugin.Create(ASynMiniMap: TSynMiniMap);
begin
  inherited Create(ASynMiniMap.Editor);
  FSynMiniMap := ASynMiniMap;
  ViewedTextBuffer.AddChangeHandler(senrLineCount, LineCountChanged);
  ViewedTextBuffer.AddNotifyHandler(senrHighlightChanged, EditorDecPaintLock);
end;

{ TSynMiniMap }

procedure TSynMiniMap.ClearEventData(var AEventData: TSynMiniMapEventData);
begin
  FillChar(AEventData, 0, szSynMiniMapEventData);
end;

constructor TSynMiniMap.Create(AOwner: TComponent);
begin
  inherited;
  FFullSizeBitmap := TBitmap.Create;
  FOffsetBitmap := TBitmap.Create;
  FColors := TSynMiniMapColors.Create(Self);
  PixelFormat := pf32bit;
  Self.Height := SYNMINIMAP_DEFAULT_HEIGHT;
  Self.Width := SYNMINIMAP_DEFAULT_WIDTH;
  FMaxCharsPerLine := 100;
  FOptions := TSynMinimapOptions.Create(Self);
  FPreviousLineIndex := -1;
  FFontFactor := SYNMINIMAP_DEFAULT_FONTFACTOR;
  FScrolling := False;
  FScrollBar := TScrollBar.Create(Self);
  FScrollBar.Parent := Self;
  FScrollBar.Kind := sbVertical;
  FScrollBar.Align := alRight;
  FScrollBar.Visible := False;
end;

destructor TSynMiniMap.Destroy;
begin
  FColors.Free;
  FFullSizeBitmap.Free;
  FOffsetBitmap.Free;
  FOptions.Free;
  inherited;
end;

procedure TSynMiniMap.DoClick(const AX, AY: Integer);
var
  LEventData : TSynMiniMapEventData;
  LPoint     : TPoint;
begin
  LEventData := Default(TSynMiniMapEventData);
  ///
  ///  OnClick has no value unless we have an editor assign
  ///
  if Assigned(FOnClick) and Assigned(FEditor) then begin
    ///
    ///  save previous line index for drawing in Render
    ///
    FPreviousLineIndex := FEditor.TopLine - 1;
    //FEditor.CaretY -1;
    ///
    ///  reset event data record
    ///
    ClearEventData(LEventData);
    ///
    ///  set the Line and Char coordonates
    ///
    LPoint.X := AX;
    LPoint.Y := AY;
    LEventData.Coord := TranslatePoint(@LPoint);
    ///
    ///  invoke assigned event
    ///
    FOnClick(Self, @LEventData);
    ///
    ///  check if we were asked to redraw
    ///
    if LEventData.Redraw then
      Render;
  end;
end;

function TSynMiniMap.GetClickCoord: TPoint;
//var
//  LPoint: TPoint;
begin
  ///
  ///  grab the cursor coordonates
  ///
  //Windows.GetCursorPos(LPoint);
  //LPoint := Self.ScreenToClient(LPoint);
  //Result := TranslatePoint(@LPoint);
  Result := Classes.Point(0, 0);
end;

function TSynMiniMap.GetPixelFormat: TPixelFormat;
begin
  ///
  ///  return the current pixel format
  ///
  Result := FFullSizeBitmap.PixelFormat;
end;

procedure TSynMiniMap.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X: Integer; Y: Integer);
begin
  inherited;
  FScrolling := (mbLeft = Button) and Options.AllowScroll;
  FMouseDownPoint.X := X;
  FMouseDownPoint.Y := Y;
end;

procedure TSynMiniMap.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
var
  LDelta: Integer;
  LScrollDown: Boolean;
begin
  inherited;
  if not Options.AllowScroll then
    Exit;
  if Scrolling and Assigned(Editor) then begin
    LDelta := FMouseDownPoint.Y - Y;
    LDelta := Trunc(LDelta /  FFontFactor);
    LDelta := Abs(LDelta);

    LScrollDown := (Y > FMouseDownPoint.Y);
    if Options.ReverseScroll then // swipe movements
      LScrollDown := not LScrollDown;

    if LScrollDown then
      Editor.TopLine := Editor.TopLine + LDelta
    else
      Editor.TopLine := Editor.TopLine - LDelta;
    Render;

    //if Y > FMouseDownPoint.Y then
    //  Editor.CaretY := Editor.CaretY + LDelta
    //else
    //  Editor.CaretY := Editor.CaretY - LDelta;
  end;
end;

procedure TSynMiniMap.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X: Integer; Y: Integer);
var
  LIsClick: Boolean;
begin
  inherited;
  FScrolling := False;
  FMouseUpPoint.X := X;
  FMouseUpPoint.Y := Y;
  LIsClick := ( not Options.AllowScroll )
    or (( FMouseDownPoint.X = FMouseUpPoint.X)
    and (FMouseDownPoint.Y = FMouseUpPoint.Y ));
  if LIsClick then
    DoClick(X, Y);
end;

procedure TSynMiniMap.Paint;
begin
  ///
  ///  draw the buffered bitmap
  ///
  Canvas.Draw(0, 0, FOffsetBitmap);
end;

procedure TSynMiniMap.Render;
var
  LLineHeight: Integer;
  LLineCount: Integer;
  LLineTop: Integer;
  LLineText: string;
  LMaxLineCount: Integer;
  LDrawHeight: Integer;
  LDrawWidth: Integer;
  LTopLineIndex: Integer;
  LFocusRect: TRect;
  LFocusTopIndex: Integer;
  LPreviousLine: Boolean;
  Index: Integer;

    function __get_line_xpos: Integer;
    ///
    ///  this function is counting leading spaces and tabs
    ///  could use improvements
    ///
    var
      cIndex: Integer;
    begin
      Result := 0;
      for cIndex := 1 to Length(LLineText) do
        case LLineText[cIndex] of
          #$9: Inc(Result, FTabWidth);
          ' ': Inc(Result, FCharWidth);
          else
            Break;
        end;
    end;

begin
  if (not Assigned(Editor)) or (csDestroying in ComponentState) then
    Exit;

  //FScrollBar.Min := 1;
  //FScrollBar.Max := Editor.Lines.Count - Editor.LinesInWindow;

  ///
  ///  this is where the magic happens
  ///  what it does:
  ///  -  the MiniMap control has a limited height, we need to translate
  ///   that height into a maximum number of lines we can display from
  ///   the synedit control
  ///  -  create a "full size bitmap"
  ///  -  paint various misc stuff(highlight, previous line, etc.)
  ///  -  paint each line starting from X lines before current visible lines
  ///   and Y lines after
  ///  -  scale down the "full size bitmap" and paint it on to the
  ///   "offset bitmap"
  ///   each time the "pain event" occures, the "offset bitmap" is used to
  ///   draw on the MiniMap control
  ///

  ///
  ///  grab the lines
  ///
  LLineCount := Editor.Lines.Count;
  ///
  /// store the first visible line index
  ///  IMPORTANT:
  ///  -  synedit refers to lines as "real index" + 1
  ///
  LTopLineIndex := Editor.TopLine -1;
  ///
  ///  grab the font size of full size bitmap
  ///
  LLineHeight := FFullSizeBitmap.Canvas.Font.Size;
  ///
  ///  add a 2 pixel line spacing
  ///
  Inc(LLineHeight, 2);
  ///
  ///  "shrink" the font to a much small dimension, each character will be
  ///  FontFactor times smaller than the "real thing"
  ///
  FLineHeightInPixels := Trunc( LLineHeight / FFontFactor);
  ///
  ///  calculate the maximum number of lines we can display
  ///  IMPORTANT:
  ///  -  FLineHeightInPixels includes the "line spacing"
  ///
  LMaxLineCount := Self.Height div FLineHeightInPixels;
  ///
  ///  calculate the smallest value of lines we will display
  ///  if the lines in editor are more than we can display
  ///  then we display the maximum possible, otherwise display as many
  ///  as there are in the editor
  ///
  LLineCount := Min(LLineCount, LMaxLineCount);
  ///
  ///  setup the full size bitmap dimensions
  ///
  FFullSizeBitmap.Height := LLineCount * LLineHeight;
  ///
  ///  setup the background color and fill it
  ///
  FFullSizeBitmap.Canvas.Brush.Color := Colors.Background;
  FFullSizeBitmap.Canvas.FillRect(Rect(0, 0, FFullSizeBitmap.Width, FFullSizeBitmap.Height));
  ///
  ///  calculate the first and last lines that we will grab from editor
  ///  and display in the MiniMap
  ///
  FFirstLine := LTopLineIndex - LMaxLineCount + Editor.LinesInWindow;
  //-( LMaxLineCount div 2 ) + (Editor.LinesInWindow div 2);
  FFirstLine := Max(FFirstLine, 0);
  FLastLine := Min(FFirstLine + LLineCount, Editor.Lines.Count -1);
  ///
  ///  setup brush and pen
  ///
  FFullSizeBitmap.Canvas.Brush.Style := bsSolid;
  FFullSizeBitmap.Canvas.Brush.Color := Colors.Highlight;
  FFullSizeBitmap.Canvas.Pen.Style := psClear;
  ///
  ///  highlight visible lines with provided color
  ///
  LFocusTopIndex := LTopLineIndex - FFirstLine;
  LFocusRect := Rect(
    0,
    LFocusTopIndex * LLineHeight,
    FFullSizeBitmap.Width,
    ( LFocusTopIndex + Editor.LinesInWindow ) * LLineHeight
  );
  FFullSizeBitmap.Canvas.Rectangle(LFocusRect);
  ///
  ///  check if we need to hightlight previous line
  ///  previous line is saved when the user clicks on the MiniMap
  ///  on MiniMap's OnClick event you can jump to the clicked line
  ///
  LPreviousLine := ( Colors.PreviousLine <> clNone ) and
    ( PreviousLineIndex >= FFirstLine ) and ( PreviousLineIndex <= FLastLine );
  if LPreviousLine then begin
    FFullSizeBitmap.Canvas.Brush.Color := Colors.PreviousLine;
    LFocusRect := Rect(
      0,
      ( PreviousLineIndex - FFirstLine ) * LLineHeight,
      FFullSizeBitmap.Width,
      ( PreviousLineIndex - FFirstLine  + 1) * LLineHeight);
    FFullSizeBitmap.Canvas.Rectangle(LFocusRect);
  end;
  ///
  ///  set the brush style to clear, otherwise we get uggly background color
  ///  for each line
  ///
  FFullSizeBitmap.Canvas.Brush.Style := bsClear;
  ///
  ///  LLineTop holds the Y pixel value of the line
  ///
  LLineTop := 0;
  ///
  ///  start drawing lines
  ///
  Index := FFirstLine;
  while Index <= FLastLine do begin
    ///
    ///  grab current line text
    ///
    LLineText := Editor.Lines[Index];
    if ( Index = PreviousLineIndex ) and (Colors.PreviousLineText <> clNone) then
      ///
      ///  color of the previous line if applies
      ///
      FFullSizeBitmap.Canvas.Font.Color := Colors.PreviousLineText
    else
    if ( Index >= LTopLineIndex ) and ( Index <= LTopLineIndex + Editor.LinesInWindow ) then
      ///
      ///  font color of lines visible in the editor
      ///
      FFullSizeBitmap.Canvas.Font.Color := Colors.TextHighlight
    else
      ///
      ///  normal text font color
      ///
      FFullSizeBitmap.Canvas.Font.Color := Colors.Text;
    ///
    ///  draw the text
    ///  at this point, the font size is the same as in the editor
    ///  just the line spacing is smaller
    ///
    FFullSizeBitmap.Canvas.TextOut(__get_line_xpos, LLineTop, LLineText);
    ///
    ///  increment the top pixel
    ///
    Inc(LLineTop, LLineHeight);
    ///
    ///  increment the line
    ///
    Inc(Index);
  end;
  ///
  ///  if the current number of lines in the editor is smaller than
  ///  the maximum we can display, we need to fill the canvas with
  ///  the provided background color
  ///
  FOffsetBitmap.Canvas.Brush.Color := Colors.Background;
  FOffsetBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
  ///
  ///  and adjust the size of the "scaled down" version of full size bitmap
  ///
  LDrawHeight := Height;
  if FOffsetBitmap.Height > LLineCount * FLineHeightInPixels then
    LDrawheight := LLineCount * FLineHeightInPixels;

  LDrawWidth := Trunc(FFullSizeBitmap.Width / FFontFactor);

  FOffsetBitmap.Canvas.StretchDrawHalftone(0, 0, LDrawWidth, LDrawheight,
    FFullSizeBitmap.Canvas, 0, 0, FFullSizeBitmap.Width, FFullSizeBitmap.Height);

  ///
  /// call paint to update the canvas
  ///
  Paint;
end;

procedure TSynMiniMap.ResetInternals;
begin
  if not Assigned(Editor) then
    Exit;
  FEditorHeight := Editor.Height;
  FEditorWidth := Editor.Width;
  FEditorRealWidth := FEditorWidth - Editor.Gutter.Width;
  FFullSizeBitmap.Canvas.Brush.Style := bsSolid;
  FFullSizeBitmap.Canvas.Brush.Color := Colors.Background;
  FFullSizeBitmap.Canvas.Font.Height := Editor.Font.Height;
  FFullSizeBitmap.Canvas.Font.Size := Editor.Font.Size;
  FFullSizeBitmap.Canvas.Font.Name := Editor.Font.Name;
  FCharWidth := FFullSizeBitmap.Canvas.TextWidth('X');
  if Options.TabWidthOverride then
    FTabWidth := Options.TabWidth
  else
    FTabWidth := FCharWidth * Editor.TabWidth;
  FMaxCharsPerLine := Trunc(Self.Width / (FCharWidth / FFontFactor));
  FFullSizeBitmap.Width := FMaxCharsPerLine * FCharWidth;
  Self.Color := Editor.Color;

  Render;
end;

procedure TSynMiniMap.Resize;
const
  CNO_EDITOR = '(no editor assigned)';
var
  LTextHeight: Integer;
  LTextWidth: Integer;
  LTextX: Integer;
  LTextY: Integer;
begin
  FOffsetBitmap.Height := Self.Height;
  FOffsetBitmap.Width := Self.Width;
  FOffsetBitmap.Canvas.Brush.Color := Colors.Background;
  FOffsetBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
  FOffsetBitmap.Canvas.Font.Color := Colors.Text;
  if csDesigning in ComponentState then
  begin
    LTextHeight := FOffsetBitmap.Canvas.TextHeight(CNO_EDITOR);
    LTextWidth := FOffsetBitmap.Canvas.TextWidth(CNO_EDITOR);
    LTextX := Width div 2 - LTextWidth div 2;
    LTextY := Height div 2 - LTextHeight div 2;
    FOffsetBitmap.Canvas.TextOut(LTextX, LTextY, CNO_EDITOR);
  end;
  ResetInternals;
  inherited Resize;
end;

procedure TSynMiniMap.SetEditor(const Value: TSynEdit);
begin
  FEditor := Value;
  ///
  ///  create a plugin if we don't have one
  ///
  if Value <> nil then
    FMiniMapPlugin := TSynMiniMapEditorPlugin.Create(Self);

  ResetInternals;
end;

procedure TSynMiniMap.SetFontFactor(const Value: Single);
begin
  if ( Value < SYNMINIMAP_FONTFACTOR_MIN ) or
      ( Value > SYNMINIMAP_FONTFACTOR_MAX ) then
  begin
    MessageDlg(
      Format(SFontFactorMinMax, [SYNMINIMAP_FONTFACTOR_MIN,
        SYNMINIMAP_FONTFACTOR_MAX]),
      mtError, [mbOK], 0);
    Exit;
  end;
  FFontFactor := Value;
  Render;
end;

procedure TSynMiniMap.SetPixelFormat(const Value: TPixelFormat);
begin
  ///
  ///  set the pixel format on both bitmaps
  ///
  FFullSizeBitmap.PixelFormat := Value;
  FOffsetBitmap.PixelFormat := Value;
end;

function TSynMiniMap.TranslatePoint(const APoint: PPoint): TPoint;
var
  LChar: Integer;
begin
  ///
  ///  this method translates X and Y from control's surface into
  ///  editor's Line and Char, mainly used in OnClick event
  ///
  if APoint.X < 1 then
    LChar := 1
  else
    LChar := Trunc(APoint.X / (FCharWidth / FFontFactor));
  if LChar > FMaxCharsPerLine then
    LChar := FMaxCharsPerLine;
  Result.X := LChar;
  Result.Y := FFirstLine + APoint.Y div FLineHeightInPixels +1;
end;

end.
