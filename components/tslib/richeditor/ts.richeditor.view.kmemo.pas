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

unit ts.RichEditor.View.KMemo;

{$MODE DELPHI}

{ A richtext editor view based on KMemo }

{
  TODO:
   - paste formatted text (HTML?)
   - proper support for tables
   - copy formatted text (WIKI, HTML?)
   - Undo/Redo is not yet supported by the KMemo component
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  ExtCtrls, Menus, Types,

  KControls, KMemo, KMemoDlgTextStyle, KMemoDlgHyperlink, KMemoDlgImage,
  KMemoDlgNumbering, KMemoDlgContainer, KMemoDlgParaStyle, KDialogs,

  DropComboTarget,

  ts.RichEditor.Interfaces;

type
  TRichEditorViewKMemo = class(TForm, IRichEditorView)
    dctMain       : TDropComboTarget;
    pnlRichEditor : TPanel;

    {$REGION 'event handlers'}
    procedure dctMainDrop(
      Sender     : TObject;
      ShiftState : TShiftState;
      APoint     : TPoint;
      var Effect : Longint
    );
    procedure FormDropFiles(
      Sender          : TObject;
      const FileNames : array of string
    );
    {$ENDREGION}

  private
    FEditor        : TKMemo;
    FTextStyle     : TKMemoTextStyle;
    FParaStyle     : TKMemoParaStyle;
    FUpdateLock    : Integer;
    FDefaultIndent : Integer;
    FOnChange      : TNotifyEvent;
    FOnDropFiles   : TDropFilesEvent;
    FOnSelectBlock : TNotifyEvent;
    FParaStyleForm : TKMemoParaStyleForm;
    FTextStyleForm : TKMemoTextStyleForm;
    FContainerForm : TKMemoContainerForm;
    FHyperlinkForm : TKMemoHyperlinkForm;
    FNumberingForm : TKMemoNumberingForm;
    FImageForm     : TKMemoImageForm;
    FPreviewDialog : TKPrintPreviewDialog;
    FFileName      : string;
    FIsFile        : Boolean;

    {$REGION 'event handlers'}
    // probably not needed
    function FEditorAlignInsertBefore(
      Sender             : TWinControl;
      Control1, Control2 : TControl
    ): Boolean;
    // probably not needed
    procedure FEditorAlignPosition(
      Sender                                   : TWinControl;
      Control                                  : TControl;
      var NewLeft, NewTop, NewWidth, NewHeight : Integer;
      var AlignRect                            : TRect;
      AlignInfo                                : TAlignInfo
    );
    procedure FEditorChange(Sender: TObject);
    procedure FEditorBlockClick(
      Sender     : TObject;
      ABlock     : TKMemoBlock;
      var Result : Boolean
    );
    procedure FEditorBlockEdit(
      Sender     : TObject;
      ABlock     : TKMemoBlock;
      var Result : Boolean
    );
    procedure FEditorEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FEditorExit(Sender: TObject);
    procedure FEditorListTableChanged(
      AList  : TKMemoList;
      ALevel : TKMemoListLevel
    );
    procedure FEditorMouseUp(
      Sender : TObject;
      Button : TMouseButton;
      Shift  : TShiftState;
      X, Y   : Integer
    );
    procedure FParaStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons);
    procedure FTextStyleChanged(Sender: TObject);
    function GetReadOnly: Boolean;
    {$ENDREGION}

    function SelectedBlock: TKMemoBlock;
    procedure SetReadOnly(AValue: Boolean);

    function TableRowCell(
      out ATable : TKMemoTable;
      out ARow   : TKMemoTableRow;
      out ACell  : TKMemoTableCell
    ): Boolean;

    function EditContainer(AItem: TKMemoContainer): Boolean;
    function EditImage(AItem: TKMemoImageBlock): Boolean;
    function EditHyperlink(AItem: TKMemoHyperlink): Boolean;

    function ResizeImageBlock(AItem: TKMemoImageBlock; AScale: Integer): Boolean;

  protected
    {$REGION 'property access mehods'}
    function GetActions: IRichEditorActions;
    function GetAlignCenter: Boolean;
    function GetAlignJustify: Boolean;
    function GetAlignLeft: Boolean;
    function GetAlignRight: Boolean;
    function GetBackgroundColor: TColor;
    function GetBullets: Boolean;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetContentSize: Int64;
    function GetEditor: TKMemo;
    function GetEvents: IRichEditorEvents;
    function GetFileName: string;
    function GetFont: TFont;
    function GetForm: TCustomForm;
    function GetIsEmpty: Boolean;
    function GetIsFile: Boolean;
    function GetIsInsideOfTable: Boolean;
    function GetKMemoNotifier: IKMemoNotifier;
    function GetModified: Boolean;
    function GetOnSelectBlock: TNotifyEvent;
    function GetOnChange: TNotifyEvent;
    function GetOnDropFiles: TDropFilesEvent;
    function GetPopupMenu: TPopupMenu; override;
    function GetRTFText: string;
    function GetSelAvail: Boolean;
    function GetSelEnd: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    function GetShowSpecialChars: Boolean;
    function GetText: string;
    function GetWordWrap: Boolean;
    procedure SetAlignCenter(AValue: Boolean);
    procedure SetAlignJustify(AValue: Boolean);
    procedure SetAlignLeft(AValue: Boolean);
    procedure SetAlignRight(AValue: Boolean);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetBullets(AValue: Boolean);
    procedure SetFileName(const AValue: string);
    procedure SetIsFile(AValue: Boolean);
    procedure SetModified(const AValue: Boolean);
    procedure SetOnSelectBlock(AValue: TNotifyEvent);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TDropFilesEvent);
    procedure SetPopupMenu(const AValue: TPopupMenu); reintroduce;
    procedure SetRTFText(AValue: string);
    procedure SetSelEnd(const AValue: Integer);
    procedure SetSelStart(const AValue: Integer);
    procedure SetSelText(const AValue: string);
    procedure SetShowSpecialChars(AValue: Boolean);
    procedure SetText(const AValue: string);
    procedure SetWordWrap(const AValue: Boolean);
    {$ENDREGION}

    procedure SelectAll;
    procedure EditSelectedItem;
    procedure DeleteSelectedItem;
    procedure EditParagraphStyle;
    procedure EditTextStyle;
    procedure LoadFromFile(const AFileName: string);
    procedure Load(const AStorageName: string = '');
    procedure LoadFromStream(AStream: TStream);
    procedure Save(const AStorageName: string = '');
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);

    procedure BeginUpdate;
    function IsUpdating: Boolean;
    function IsBlockSelected: Boolean;
    function IsImageSelected: Boolean;
    procedure EndUpdate;

    procedure InsertImageFile(const AFileName: string);
    procedure InsertImage(AImage: TPicture); overload;
    function InsertImage: Boolean; overload;
    procedure InsertHyperlink(
      const AText : string = '';
      const AURL  : string = ''
    );
    procedure InsertRowBefore;
    procedure InsertRowAfter;
    procedure InsertColumnBefore;
    procedure InsertColumnAfter;
    procedure DeleteColumn;
    procedure DeleteRow;
    procedure SelectTable;

    procedure ResizeImage(AScale: Integer);

    procedure CreateBulletList;
    procedure CreateNumberedList;
    procedure CreateTable(AColCount: Integer; ARowCount: Integer);
    procedure IncIndent;
    procedure DecIndent;

    procedure ShowPreview;
    procedure Clear;

    // clipboard commands
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;
    procedure Redo;

    procedure AddParagraph;

    // event dispatch methods
    procedure DoDropFiles(const AFileNames: array of string);
    procedure DoChange;
    procedure DoSelectBlock;

    property KMemoNotifier: IKMemoNotifier
      read GetKMemoNotifier;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure UpdateActions; override;
    function Focused: Boolean; override;

    property Actions: IRichEditorActions
      read GetActions;

    property Bullets: Boolean
      read GetBullets write SetBullets;

    property CanPaste: Boolean
      read GetCanPaste;

    property CanUndo: Boolean
      read GetCanUndo;

    property CanRedo: Boolean
      read GetCanRedo;

    property ContentSize: Int64
      read GetContentSize;

    property Events: IRichEditorEvents
      read GetEvents;

    property Editor: TKMemo
      read GetEditor;

    property FileName: string
      read GetFileName write SetFileName;

    property Font: TFont
      read GetFont;

    property ReadOnly: Boolean
      read GetReadOnly write SetReadOnly;

    property SelStart: Integer
      read GetSelStart write SetSelStart;

    property SelEnd: Integer
      read GetSelEnd write SetSelEnd;

    property SelAvail: Boolean
      read GetSelAvail;

    property SelText: string
      read GetSelText write SetSelText;

    property AlignLeft: Boolean
      read GetAlignLeft write SetAlignLeft;

    property AlignRight: Boolean
      read GetAlignRight write SetAlignRight;

    property AlignCenter: Boolean
      read GetAlignCenter write SetAlignCenter;

    property AlignJustify: Boolean
      read GetAlignJustify write SetAlignJustify;

    property BackgroundColor: TColor
      read GetBackgroundColor write SetBackgroundColor;

    property Modified: Boolean
      read GetModified write SetModified;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

    property Form: TCustomForm
      read GetForm;

    property IsEmpty: Boolean
      read GetIsEmpty;

    property IsFile: Boolean
      read GetIsFile write SetIsFile;

    property IsInsideOfTable: Boolean
      read GetIsInsideOfTable;

    property Text: string
      read GetText write SetText;

    property RTFText: string
      read GetRTFText write SetRTFText;

    property ShowSpecialChars: Boolean
      read GetShowSpecialChars write SetShowSpecialChars;

    property WordWrap: Boolean
      read GetWordWrap write SetWordWrap;

    property OnDropFiles: TDropFilesEvent
      read GetOnDropFiles write SetOnDropFiles;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;

    property OnSelectBlock: TNotifyEvent
      read GetOnSelectBlock write SetOnSelectBlock;
  end;

implementation

{$R *.lfm}

uses
  StdCtrls, Math, StrUtils, Printers,

  keditcommon, kgraphics,

  ts.Core.Logger,
  ts.RichEditor.Resources;

type
  TKAccessMemo = class(TKMemo)
  published
    property States;

  end;

{$REGION 'construction and destruction'}
procedure TRichEditorViewKMemo.AfterConstruction;
var
  LKey : TKEditKey;
begin
  inherited AfterConstruction;
  FEditor := TKMemo.Create(Self);
  FEditor.Parent              := pnlRichEditor;
  FEditor.BorderStyle         := bsNone;
  FEditor.ScrollBars          := ssBoth;
  FEditor.Align               := alClient;
  FEditor.DoubleBuffered      := True;
  FEditor.OnChange            := FEditorChange;
  FEditor.OnMouseUp           := FEditorMouseUp;
  FEditor.OnEndDrag           := FEditorEndDrag;
  FEditor.OnExit              := FEditorExit;
  FEditor.OnBlockEdit         := FEditorBlockEdit;
  FEditor.OnBlockClick        := FEditorBlockClick;
  FEditor.ListTable.OnChanged := FEditorListTableChanged;
  // needed?
  FEditor.OnAlignPosition     := FEditorAlignPosition;
  // needed?
  FEditor.OnAlignInsertBefore := FEditorAlignInsertBefore;
  FEditor.Options             := FEditor.Options + [eoWantTab];
  LKey.Key := 0;
  LKey.Shift := [];
  FEditor.KeyMapping.Key[ecScrollCenter] := LKey;

  dctMain.Target := FEditor;
  FDefaultIndent := 20;

  FTextStyle := TKMemoTextStyle.Create;
  FTextStyle.OnChanged := FTextStyleChanged;

  FParaStyle := TKMemoParaStyle.Create;
  FParaStyle.OnChanged := FParaStyleChanged;

  // TODO: these should be created by the manager and passed to the view
  FContainerForm := TKMemoContainerForm.Create(Self);
  FHyperlinkForm := TKMemoHyperlinkForm.Create(Self);
  FImageForm     := TKMemoImageForm.Create(Self);
  FNumberingForm := TKMemoNumberingForm.Create(Self);
  FTextStyleForm := TKMemoTextStyleForm.Create(Self);
  FParaStyleForm := TKMemoParaStyleForm.Create(Self);
  FPreviewDialog := TKPrintPreviewDialog.Create(Self);
end;

destructor TRichEditorViewKMemo.Destroy;
begin
  FTextStyle.Free;
  FParaStyle.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TRichEditorViewKMemo.GetActions: IRichEditorActions;
begin
  Result := Owner as IRichEditorActions;
end;

function TRichEditorViewKMemo.GetCanPaste: Boolean;
begin
  Result := FEditor.CommandEnabled(ecPaste);
end;

function TRichEditorViewKMemo.GetCanUndo: Boolean;
begin
  Result := FEditor.CommandEnabled(ecUndo);
end;

function TRichEditorViewKMemo.GetEditor: TKMemo;
begin
  Result := FEditor;
end;

function TRichEditorViewKMemo.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TRichEditorViewKMemo.SetFileName(const AValue: string);
begin
  if AValue <> FileName then
  begin
    FFileName := AValue;
  end;
end;

function TRichEditorViewKMemo.GetFont: TFont;
begin
  Result := FTextStyle.Font;
end;

function TRichEditorViewKMemo.GetForm: TCustomForm;
begin
  Result := Self;
end;

function TRichEditorViewKMemo.GetEvents: IRichEditorEvents;
begin
  Result := Owner as IRichEditorEvents;
end;

function TRichEditorViewKMemo.GetIsEmpty: Boolean;
begin
  Result := FEditor.Blocks.Count <= 1;
end;

function TRichEditorViewKMemo.GetIsFile: Boolean;
begin
  Result := FIsFile;
end;

procedure TRichEditorViewKMemo.SetIsFile(AValue: Boolean);
begin
  FIsFile := AValue;
end;

function TRichEditorViewKMemo.GetModified: Boolean;
begin
  Result := FEditor.Modified;
end;

procedure TRichEditorViewKMemo.SetModified(const AValue: Boolean);
begin
  FEditor.Modified := AValue;
end;

function TRichEditorViewKMemo.GetOnChange: TNotifyEvent;
begin
  Result := FOnChange;
end;

procedure TRichEditorViewKMemo.SetOnChange(const AValue: TNotifyEvent);
begin
  FOnChange := AValue;
end;

function TRichEditorViewKMemo.GetOnSelectBlock: TNotifyEvent;
begin
  Result := FOnSelectBlock;
end;

procedure TRichEditorViewKMemo.SetOnSelectBlock(AValue: TNotifyEvent);
begin
  FOnSelectBlock := AValue;
end;

function TRichEditorViewKMemo.GetOnDropFiles: TDropFilesEvent;
begin
  Result := FOnDropFiles;
end;

procedure TRichEditorViewKMemo.SetOnDropFiles(const AValue: TDropFilesEvent);
begin
  FOnDropFiles := AValue;
end;

function TRichEditorViewKMemo.GetSelAvail: Boolean;
begin
  Result := FEditor.SelAvail or FEditor.RelativeSelected;
end;

function TRichEditorViewKMemo.GetSelEnd: Integer;
begin
  Result := FEditor.SelEnd;
end;

procedure TRichEditorViewKMemo.SetSelEnd(const AValue: Integer);
begin
  FEditor.SelEnd := AValue;
end;

function TRichEditorViewKMemo.GetSelStart: Integer;
begin
  Result := FEditor.SelStart;
end;

procedure TRichEditorViewKMemo.SetSelStart(const AValue: Integer);
begin
  FEditor.SelStart :=  AValue;
end;

function TRichEditorViewKMemo.GetSelText: string;
begin
  Result := FEditor.SelText;
end;

procedure TRichEditorViewKMemo.SetSelText(const AValue: string);
begin
  FEditor.DeleteSelectedBlock;
  FEditor.InsertString(FEditor.SelStart, AValue);
end;

function TRichEditorViewKMemo.GetWordWrap: Boolean;
begin
  Result := FParaStyle.WordWrap;
end;

procedure TRichEditorViewKMemo.SetWordWrap(const AValue: Boolean);
begin
  FParaStyle.WordWrap := AValue;
end;

function TRichEditorViewKMemo.GetPopupMenu: TPopupMenu;
begin
  Result := FEditor.PopupMenu;
end;

procedure TRichEditorViewKMemo.SetPopupMenu(const AValue: TPopupMenu);
begin
  FEditor.PopupMenu := AValue;
end;

function TRichEditorViewKMemo.GetText: string;
begin
  Result := FEditor.Text;
end;

procedure TRichEditorViewKMemo.SetText(const AValue: string);
begin
  FEditor.Text := AValue;
end;

function TRichEditorViewKMemo.GetAlignCenter: Boolean;
begin
  Result := FParaStyle.HAlign = halCenter;
end;

procedure TRichEditorViewKMemo.SetAlignCenter(AValue: Boolean);
begin
  if AValue then
  begin
    FParaStyle.HAlign := halCenter;
    Modified := True;
  end;
end;

function TRichEditorViewKMemo.GetAlignJustify: Boolean;
begin
  Result := FParaStyle.HAlign = halJustify;
end;

procedure TRichEditorViewKMemo.SetAlignJustify(AValue: Boolean);
begin
  if AValue then
  begin
    FParaStyle.HAlign := halJustify;
    Modified := True;
  end;
end;

function TRichEditorViewKMemo.GetAlignLeft: Boolean;
begin
  Result := FParaStyle.HAlign = halLeft;
end;

procedure TRichEditorViewKMemo.SetAlignLeft(AValue: Boolean);
begin
  if AValue then
  begin
    FParaStyle.HAlign := halLeft;
    Modified := True;
  end;
end;

function TRichEditorViewKMemo.GetAlignRight: Boolean;
begin
  Result := FParaStyle.HAlign = halRight;
end;

procedure TRichEditorViewKMemo.SetAlignRight(AValue: Boolean);
begin
  if AValue then
  begin
    FParaStyle.HAlign := halRight;
  end;
end;

function TRichEditorViewKMemo.GetCanRedo: Boolean;
begin
   Result := FEditor.CommandEnabled(ecRedo);
end;

function TRichEditorViewKMemo.GetRTFText: string;
var
  SS : TStringStream;
begin
  SS := TStringStream.Create;
  try
    FEditor.SaveToRTFStream(SS, False, True);
    Result := SS.DataString;
  finally
    SS.Free;
  end;
end;

procedure TRichEditorViewKMemo.SetRTFText(AValue: string);
var
  SS : TStringStream;
begin
  SS := TStringStream.Create;
  try
    SS.WriteString(AValue);
    SS.Position := 0;
    FEditor.LoadFromRTFStream(SS);
  finally
    SS.Free;
  end;
end;

function TRichEditorViewKMemo.GetShowSpecialChars: Boolean;
begin
  Result := eoShowFormatting in FEditor.Options;
end;

procedure TRichEditorViewKMemo.SetShowSpecialChars(AValue: Boolean);
begin
  if AValue then
    FEditor.Options := FEditor.Options + [eoShowFormatting]
  else
    FEditor.Options := FEditor.Options - [eoShowFormatting];
end;

function TRichEditorViewKMemo.GetContentSize: Int64;
begin
  Result := Length(FEditor.RTF);
end;

function TRichEditorViewKMemo.GetKMemoNotifier: IKMemoNotifier;
begin
  Result := IKMemoNotifier(Editor);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TRichEditorViewKMemo.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
begin
  DoDropFiles(FileNames);
end;

procedure TRichEditorViewKMemo.dctMainDrop(Sender: TObject;
  ShiftState: TShiftState; APoint: TPoint; var Effect: Longint);
begin
  InsertHyperlink(dctMain.Title, dctMain.URL);
end;

{$REGION 'Editor'}
procedure TRichEditorViewKMemo.FEditorChange(Sender: TObject);
begin
  Modified := True;
  DoChange;
  Events.DoChange;
end;

function TRichEditorViewKMemo.FEditorAlignInsertBefore(Sender: TWinControl;
  Control1, Control2: TControl): Boolean;
begin
  //Modified := True;
  //DoChange;
  Result := True;
end;

procedure TRichEditorViewKMemo.FEditorAlignPosition(Sender: TWinControl;
  Control: TControl; var NewLeft, NewTop, NewWidth, NewHeight: Integer;
  var AlignRect: TRect; AlignInfo: TAlignInfo);
begin
  //Modified := True;
  //DoChange;
end;

procedure TRichEditorViewKMemo.FEditorBlockClick(Sender: TObject;
  ABlock: TKMemoBlock; var Result: Boolean);
begin
  Modified := True;
  DoSelectBlock;
end;

{ Gets called when executing IKMemoNotifier.EditBlock. }

procedure TRichEditorViewKMemo.FEditorBlockEdit(Sender: TObject;
  ABlock: TKMemoBlock; var Result: Boolean);
begin
  if ABlock is TKMemoTable then
  begin
    //Result := Edi
  end
  else if ABlock is TKMemoContainer then
  begin
    Result := EditContainer(ABlock as TKMemoContainer);
  end
  else if ABlock is TKMemoImageBlock then
  begin
    Result := EditImage(ABlock as TKMemoImageBlock);
  end
  else if ABlock is TKMemoHyperlink then
  begin
    Result := EditHyperlink(ABlock as TKMemoHyperlink);
  end
  else
  begin
    Result := False;
  end;
  if Result then
    DoChange;
end;

procedure TRichEditorViewKMemo.FEditorEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  Modified := True;
  DoChange;
end;

procedure TRichEditorViewKMemo.FEditorExit(Sender: TObject);
begin
  Modified := True;
  DoChange;
end;

procedure TRichEditorViewKMemo.FEditorListTableChanged(AList: TKMemoList;
  ALevel: TKMemoListLevel);
begin
  Logger.Info('ListTableChanged');
  Events.DoModified;
end;

procedure TRichEditorViewKMemo.FEditorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Modified := True;
  DoChange;
end;

procedure TRichEditorViewKMemo.FParaStyleChanged(Sender: TObject;
  AReasons: TKMemoUpdateReasons);
var
  LSelEnd     : TKMemoSelectionIndex;
  LStartIndex : TKMemoSelectionIndex;
  LEndIndex   : TKMemoSelectionIndex;
begin
    //muContent,
    //{ continue previous line info and extent calculation. }
    //muContentAddOnly,
    //{ recalculate extent. }
    //muExtent,
    //{ selection changed. }
    //muSelection,
    //{ selection changed and scroll operation is required to reflect the change. }
    //muSelectionScroll
  Logger.Info('FParaStyleChanged');
  // if there is no selection then simulate one word selection or set style for new text
  LSelEnd   := FEditor.SelEnd;
  if FEditor.SelAvail then
    FEditor.SelectionParaStyle := FParaStyle
  else if FEditor.GetNearestWordIndexes(LSelEnd, False, LStartIndex, LEndIndex)
    and (LStartIndex < LSelEnd) and (LSelEnd < LEndIndex) then
    // simulate MS Word behavior here, LSelEnd is caret position
    // do not select the word if we are at the beginning or end of the word
    // and allow set another text style for newly added text
    FEditor.SetRangeParaStyle(LStartIndex, LEndIndex, FParaStyle)
  else
    FEditor.SetRangeParaStyle(LStartIndex, LStartIndex, FParaStyle);
  Modified := True;
  DoChange;
end;

procedure TRichEditorViewKMemo.FTextStyleChanged(Sender: TObject);
var
  LSelAvail   : Boolean;
  LSelEnd     : TKMemoSelectionIndex;
  LStartIndex : TKMemoSelectionIndex;
  LEndIndex   : TKMemoSelectionIndex;
begin
  Logger.Info('FTextStyleChanged');
  // if there is no selection then simulate one word selection or set style for new text
  LSelAvail := FEditor.SelAvail;
  LSelEnd   := FEditor.SelEnd;
  if LSelAvail then
    FEditor.SelectionTextStyle := FTextStyle
  else if FEditor.GetNearestWordIndexes(LSelEnd, False, LStartIndex, LEndIndex)
    and (LStartIndex < LSelEnd) and (LSelEnd < LEndIndex) then
    // simulate MS Word behavior here, LSelEnd is caret position
    // do not select the word if we are at the beginning or end of the word
    // and allow set another text style for newly added text
    FEditor.SetRangeTextStyle(LStartIndex, LEndIndex, FTextStyle)
  else
    FEditor.NewTextStyle := FTextStyle;
  Modified := True;
  DoChange;
end;

function TRichEditorViewKMemo.GetReadOnly: Boolean;
begin
  Result := Editor.ReadOnly;
end;

procedure TRichEditorViewKMemo.SetReadOnly(AValue: Boolean);
begin
  Editor.ReadOnly := AValue;
end;

function TRichEditorViewKMemo.GetBullets: Boolean;
begin
  if FParaStyle.NumberingList < 0 then
    Result := False
  else
    Result :=
      KMemoNotifier.GetListTable.Items[FParaStyle.NumberingList]
                                .Levels[FParaStyle.NumberingListLevel]
                                .Numbering = pnuBullets;
end;

procedure TRichEditorViewKMemo.SetBullets(AValue: Boolean);
var
  BI : TKMemoBlockIndex;
  LI : TKMemoSelectionIndex;
  P  : TKMemoParagraph;
begin
  if AValue <> Bullets then
  begin
    FEditor.RealSelStart;
    BI := FEditor.ActiveInnerBlocks.IndexToBlockIndex(FEditor.RealSelStart, LI);
    P := FEditor.ActiveInnerBlocks.GetNearestParagraphBlock(BI);
    if not Assigned(P) then
      P := FEditor.NearestParagraph;
    if P.Numbering <> pnuBullets then
      P.Numbering := pnuBullets
    else
      P.Numbering := pnuNone;
    if SelAvail then
    begin
      FEditor.SelectionParaStyle := P.ParaStyle;
    end;
  end;
end;

function TRichEditorViewKMemo.GetIsInsideOfTable: Boolean;
begin
  Result := FEditor.ActiveInnerBlocks.InsideOfTable;
end;

function TRichEditorViewKMemo.GetBackgroundColor: TColor;
begin
  Result := FTextStyle.Brush.Color;
end;

procedure TRichEditorViewKMemo.SetBackgroundColor(AValue: TColor);
begin
  FTextStyle.Brush.Color := AValue;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TRichEditorViewKMemo.DoDropFiles(const AFileNames: array of string);
begin
  FEditor.Visible := False;
  if Assigned(FOnDropFiles) then
    FOnDropFiles(Self, AFileNames);
  FEditor.Visible := True;
end;

procedure TRichEditorViewKMemo.DoChange;
begin
  if Assigned(OnChange) and not IsUpdating then
  begin
    OnChange(Self);
    Events.DoChange;
  end;
end;

procedure TRichEditorViewKMemo.DoSelectBlock;
begin
  if Assigned(OnSelectBlock) then
    OnSelectBlock(Self);
  Events.DoSelectBlock;
end;
{$ENDREGION}

{$REGION 'private methods'}
function TRichEditorViewKMemo.SelectedBlock: TKMemoBlock;
begin
  Result := FEditor.SelectedBlock;
  if not Assigned(Result) then
    Result := FEditor.ActiveInnerBlock;
end;

{ Returns active Table/Row and Cell blocks if possible. }

function TRichEditorViewKMemo.TableRowCell(out ATable: TKMemoTable; out
  ARow: TKMemoTableRow; out ACell: TKMemoTableCell): Boolean;
begin
  Result := False;
  if FEditor.ActiveBlock is TKMemoTable then
  begin
    ATable := FEditor.ActiveBlock as TKMemoTable;
    if FEditor.ActiveInnerBlocks.Parent is TKMemoTableCell then
    begin
      ACell  := FEditor.ActiveInnerBlocks.Parent as TKMemoTableCell;
      ARow   := ACell.ParentRow;
      Result := True;
    end;
  end;
end;

function TRichEditorViewKMemo.EditContainer(AItem: TKMemoContainer): Boolean;
begin
  Result := False;
  FContainerForm.Load(Editor, AItem);
  if FContainerForm.ShowModal = mrOk then
  begin
    FContainerForm.Save(AItem);
    Editor.Modified := True;
    Events.DoModified;
    Result := True;
  end;
end;

function TRichEditorViewKMemo.EditImage(AItem: TKMemoImageBlock): Boolean;
begin
  Result := False;
  FImageForm.Load(Editor, AItem);
  if FImageForm.ShowModal = mrOk then
  begin
    FImageForm.Save(AItem);
    Editor.Modified := True;
    Events.DoModified;
    Result := True;
  end;
end;

function TRichEditorViewKMemo.EditHyperlink(AItem: TKMemoHyperlink): Boolean;
begin
  Result := False;
  FHyperlinkForm.Load(AItem);
  if FHyperlinkForm.ShowModal = mrOk then
  begin
    FHyperlinkForm.Save(AItem);
    Editor.Modified := True;
    Events.DoModified;
    Result := True;
  end;
end;

function TRichEditorViewKMemo.ResizeImageBlock(AItem: TKMemoImageBlock;
  AScale: Integer): Boolean;
begin
  if Assigned(AItem) then
  begin
    AItem.LogScaleX := AScale;
    AItem.LogScaleY := AScale;
    Result := True;
  end
  else
    Result := False;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TRichEditorViewKMemo.SelectAll;
begin
  FEditor.Select(0, FEditor.SelectableLength);
end;

procedure TRichEditorViewKMemo.EditSelectedItem;
var
  LBlock : TKMemoBlock;
begin
  LBlock := Editor.SelectedBlock;
  if LBlock = nil then
    LBlock := Editor.ActiveInnerBlock;
  if Assigned(LBlock) then
    KMemoNotifier.EditBlock(LBlock); // will invoke EditBlock event
end;

procedure TRichEditorViewKMemo.DeleteSelectedItem;
begin
  if SelAvail then
  begin
    FEditor.DeleteSelectedBlock;
  end;
end;

procedure TRichEditorViewKMemo.EditParagraphStyle;
begin
  FParaStyleForm.Load(FEditor, FParaStyle);
  if FParaStyleForm.ShowModal = mrOk then
  begin
    FParaStyleForm.Save(FParaStyle);
    Events.DoModified;
  end;
end;

procedure TRichEditorViewKMemo.EditTextStyle;
begin
  FTextStyleForm.Load(FTextStyle);
  if FTextStyleForm.ShowModal = mrOk then
  begin
    FTextStyleForm.Save(FTextStyle);
    Events.DoModified;
  end;
end;

procedure TRichEditorViewKMemo.LoadFromFile(const AFileName: string);
begin
  FEditor.LoadFromFile(AFileName);
end;

procedure TRichEditorViewKMemo.Load(const AStorageName: string);
begin
  Events.DoLoad(AStorageName);
  if IsFile then
  begin
    if (AStorageName <> '') and FileExists(AStorageName) then
      FileName := AStorageName;

    LoadFromFile(FFileName);
    Modified := False;
  end;
end;

procedure TRichEditorViewKMemo.SaveToFile(const AFileName: string);
begin
  FEditor.SaveToFile(AFileName);
  FEditor.ClearUndo;
end;

procedure TRichEditorViewKMemo.LoadFromStream(AStream: TStream);
begin
  BeginUpdate;
  try
    Clear;
    FEditor.LoadFromRTFStream(AStream);
    // TS: probably due to a bug an empty line gets inserted after loading
    // content using LoadFromRTFStream
    FEditor.DeleteLine(0);
  finally
    EndUpdate;
  end;
end;

procedure TRichEditorViewKMemo.Save(const AStorageName: string);
begin
  Logger.Enter(Self, 'Save');
  Events.DoBeforeSave(AStorageName);
  if IsFile then
  begin
    FileName := AStorageName;
    SaveToFile(AStorageName);
  end;
  Events.DoAfterSave(AStorageName);
  Modified := False;
  Logger.Leave(Self, 'Save');
end;

procedure TRichEditorViewKMemo.SaveToStream(AStream: TStream);
begin
  FEditor.SaveToRTFStream(AStream, False, True);
  FEditor.ClearUndo;
end;

procedure TRichEditorViewKMemo.BeginUpdate;
begin
  // do not call FEditor.LockUpdate as it causes problems if used in
  // combination with FEditor.LoadFromRTFStream.
  Inc(FUpdateLock);
end;

procedure TRichEditorViewKMemo.EndUpdate;
begin
  if FUpdateLock > 0 then
    Dec(FUpdateLock);
  // do not call FEditor.UnlLockUpdate as it causes problems if used in
  // combination with FEditor.LoadFromRTFStream.
end;

{ Inserts a new image block for the given image file at the current cursor
  position. }

procedure TRichEditorViewKMemo.InsertImageFile(const AFileName: string);
var
  P : TPicture;
begin
  P := TPicture.Create;
  try
    try
      P.LoadFromFile(AFileName);
      InsertImage(P);
    except
      MessageDlg(SWarning, SPictureFormatNotSupported, mtWarning, [mbOK], 0);
    end;
  finally
    P.Free;
  end;
end;

procedure TRichEditorViewKMemo.InsertImage(AImage: TPicture);
var
  LIndex      : TKMemoBlockIndex;
  LImageBlock : TKMemoImageBlock;
begin
  // TKMemo does only handle jpg and png well, so we convert any other
  // image types to jpeg first.
  try
    if not MatchStr(AImage.Graphic.MimeType, ['image/jpg', 'image/png']) then
    begin
      AImage.Assign(AImage.Jpeg);
    end;
    LIndex := -1;
    if Assigned(FEditor.ActiveBlocks) and Assigned(FEditor.ActiveBlock) then
      LIndex := FEditor.ActiveBlocks.BlockToIndex(FEditor.ActiveBlock);
    LImageBlock := FEditor.Blocks.AddImageBlock(AImage, LIndex);
    // Set logical scaling to 100%
    LImageBlock.LogScaleX := 100;
    LImageBlock.LogScaleY := 100;
  finally
    Events.DoModified;
  end;
end;

{ Creates new or updates an existing image block.  }

function TRichEditorViewKMemo.InsertImage: Boolean;
var
  LImage   : TKMemoImageBlock;
  LCreated : Boolean;
begin
  Result := False;
  LCreated := False;
  if SelectedBlock is TKMemoImageBlock then
    LImage := TKMemoImageBlock(SelectedBlock)
  else
  begin
    LImage := TKMemoImageBlock.Create;
    LCreated := True;
  end;
  FImageForm.Load(FEditor, LImage);
  if FImageForm.ShowModal = mrOk then
  begin
    FImageForm.Save(LImage);
    if LCreated then
    begin
      if FEditor.SelAvail then
        FEditor.ClearSelection;
      FEditor.ActiveInnerBlocks.AddAt(LImage, FEditor.SplitAt(FEditor.SelEnd));
    end;
    Events.DoModified;
    Modified := True;
    Result := True;
  end
  else if LCreated then
    LImage.Free;
end;

{ Creates a new or updates an existing hyperlink block. }

procedure TRichEditorViewKMemo.InsertHyperlink(const AText: string;
  const AURL: string);
var
  LBlock     : TKMemoBlock;
  LHyperlink : TKMemoHyperlink;
  LCreated   : Boolean;
begin
  LCreated := False;
  if FEditor.SelAvail then
  begin
    LHyperlink      := TKMemoHyperlink.Create;
    LHyperlink.Text := FEditor.SelText;
    LBlock          := FEditor.ActiveInnerBlock;
    if LBlock is TKMemoHyperlink then
      LHyperlink.URL := TKMemoHyperlink(LBlock).URL;
    LCreated := True;
  end
  else
  begin
    LBlock := FEditor.ActiveInnerBlock;
    if LBlock is TKMemoHyperlink then
      LHyperlink := TKMemoHyperlink(LBlock)
    else
    begin
      LHyperlink := TKMemoHyperlink.Create;
      LHyperlink.Text := AText;
      LHyperlink.URL  := AURL;
      LCreated   := True;
    end;
  end;
  FHyperlinkForm.Load(LHyperlink);
  if FHyperlinkForm.ShowModal = mrOk then
  begin
    FHyperlinkForm.Save(LHyperlink);
    if LCreated then
    begin
      if FEditor.SelAvail then
        FEditor.ClearSelection;
      FEditor.ActiveInnerBlocks.AddHyperlink(
        LHyperlink,
        FEditor.SplitAt(FEditor.SelEnd)
      );
    end;
    Modified := True;
    Events.DoModified;
  end
  else if LCreated then
    LHyperlink.Free;
end;

procedure TRichEditorViewKMemo.InsertRowBefore;
var
  LTable  : TKMemoTable;
  LRow    : TKMemoTableRow;
  LNewRow : TKMemoTableRow;
  LCell   : TKMemoTableCell;
  C       : TKMemoTableCell;
begin
  if TableRowCell(LTable, LRow, LCell) then
  begin
    LNewRow := TKMemoTableRow.Create;
    LNewRow.CellCount := LRow.CellCount;
    for C in LNewRow.Blocks do
    begin
      C.InsertParagraph(-1);
    end;
    LRow.ParentBlocks.AddAt(LNewRow, LCell.RowIndex);
    LTable.ApplyDefaultCellStyle;
    Events.DoModified;
  end;
end;

procedure TRichEditorViewKMemo.InsertRowAfter;
var
  LTable  : TKMemoTable;
  LRow    : TKMemoTableRow;
  LNewRow : TKMemoTableRow;
  LCell   : TKMemoTableCell;
  C       : TKMemoTableCell;
begin
  if TableRowCell(LTable, LRow, LCell) then
  begin
    LNewRow := TKMemoTableRow.Create;
    LNewRow.CellCount := LRow.CellCount;
    for C in LNewRow.Blocks do
    begin
      C.InsertParagraph(-1);
    end;
    // AddAt will append if index does not exist.
    LRow.ParentBlocks.AddAt(LNewRow, LCell.RowIndex + 1);
    LTable.ApplyDefaultCellStyle;
    Events.DoModified;
  end;
end;

procedure TRichEditorViewKMemo.InsertColumnBefore;
var
  LTable    : TKMemoTable;
  LRow      : TKMemoTableRow;
  LCell     : TKMemoTableCell;
  R         : TKMemoTableRow;
  C         : TKMemoTableCell;
  LColIndex : Integer;
begin
  if TableRowCell(LTable, LRow, LCell) then
  begin
    LColIndex :=  LCell.ColIndex;
    for R in LRow.ParentBlocks do
    begin
      C := TKMemoTableCell.Create;
      C.InsertParagraph(-1);
      R.Blocks.AddAt(C, LColIndex);
    end;
    LTable.ApplyDefaultCellStyle;
    Events.DoModified;
  end;
end;

procedure TRichEditorViewKMemo.InsertColumnAfter;
var
  LTable    : TKMemoTable;
  LRow      : TKMemoTableRow;
  LCell     : TKMemoTableCell;
  R         : TKMemoTableRow;
  C         : TKMemoTableCell;
  LColIndex : Integer;
begin
  if TableRowCell(LTable, LRow, LCell) then
  begin
    LColIndex :=  LCell.ColIndex;
    for R in LRow.ParentBlocks do
    begin
      C := TKMemoTableCell.Create;
      C.InsertParagraph(-1);
      R.Blocks.AddAt(C, LColIndex + 1);
    end;
    LTable.ApplyDefaultCellStyle;
    Events.DoModified;
  end;
end;

procedure TRichEditorViewKMemo.DeleteColumn;
var
  LTable    : TKMemoTable;
  LRow      : TKMemoTableRow;
  LCell     : TKMemoTableCell;
  R         : TKMemoTableRow;
  LColIndex : Integer;
begin
  if TableRowCell(LTable, LRow, LCell) then
  begin
    LColIndex :=  LCell.ColIndex;
    for R in LRow.ParentBlocks do
    begin
      Logger.Send('R.CellCount', R.CellCount);
      if LColIndex < R.CellCount then
        R.Blocks.Delete(LColIndex);
    end;
    Events.DoModified;
  end;
end;

procedure TRichEditorViewKMemo.DeleteRow;
var
  LTable    : TKMemoTable;
  LRow      : TKMemoTableRow;
  LCell     : TKMemoTableCell;
begin
  if TableRowCell(LTable, LRow, LCell) then
  begin
    LTable.Blocks.Delete(LCell.RowIndex);
  end;
  Events.DoModified;
end;

procedure TRichEditorViewKMemo.SelectTable;
begin
  if FEditor.ActiveBlock is TKMemoTable then
    KMemoNotifier.SelectBlock(FEditor.ActiveBlock, sgpNone);
end;

procedure TRichEditorViewKMemo.ResizeImage(AScale: Integer);
begin
  if IsImageSelected then
  begin
     ResizeImageBlock(SelectedBlock as TKMemoImageBlock, AScale);
  end;
end;

procedure TRichEditorViewKMemo.CreateBulletList;
var
  BI : TKMemoBlockIndex;
  LI : TKMemoSelectionIndex;
  P  : TKMemoParagraph;
begin
  FEditor.RealSelStart;
  BI := FEditor.ActiveInnerBlocks.IndexToBlockIndex(FEditor.RealSelStart, LI);
  P := FEditor.ActiveInnerBlocks.GetNearestParagraphBlock(BI);
  if not Assigned(P) then
    P := FEditor.NearestParagraph;
  if P.Numbering <> pnuBullets then
    P.Numbering := pnuBullets
  else
    P.Numbering := pnuNone;

  //FNumberingForm.Load(FEditor, FEditor.ListTable, P);
  //if FNumberingForm.ShowModal = mrOk then
  //  FNumberingForm.Save;

  if SelAvail then
  begin
    FEditor.SelectionParaStyle := P.ParaStyle;
  end;
  Events.DoModified;
end;

procedure TRichEditorViewKMemo.CreateNumberedList;
begin
  //
end;

procedure TRichEditorViewKMemo.CreateTable(AColCount: Integer; ARowCount: Integer);
var
  LTable : TKMemoTable;
  X      : Integer;
  Y      : Integer;
begin
  LTable := FEditor.ActiveInnerBlocks.AddTable;
  LTable.ColCount := AColCount;
  LTable.RowCount := ARowCount;
  for Y := 0 to LTable.RowCount - 1 do
  begin
    for X := 0 to LTable.ColCount - 1 do
    begin
      LTable.Cells[X, Y].Blocks.AddParagraph;
    end;
  end;
  LTable.CellStyle.BorderWidth := 2;
  LTable.ApplyDefaultCellStyle;
  Events.DoModified;
end;

procedure TRichEditorViewKMemo.IncIndent;
begin
  FParaStyle.LeftPadding := Min(
    FParaStyle.LeftPadding + FEditor.Pt2PxX(FDefaultIndent),
    FEditor.RequiredContentWidth
      - FParaStyle.RightPadding - FEditor.Pt2PxX(FDefaultIndent)
  );
end;

procedure TRichEditorViewKMemo.DecIndent;
begin
  FParaStyle.LeftPadding := Max(
    FParaStyle.LeftPadding - FEditor.Pt2PxX(FDefaultIndent), 0
  );
end;

procedure TRichEditorViewKMemo.ShowPreview;
begin
  FPreviewDialog.PrintPreviewForm.Preview.PageSetup.Orientation := poLandscape;
  FPreviewDialog.Control := FEditor;
  FPreviewDialog.Execute;
end;

function TRichEditorViewKMemo.IsUpdating: Boolean;
begin
  Result := FUpdateLock > 0;
end;

function TRichEditorViewKMemo.IsBlockSelected: Boolean;
begin
  Result := Assigned(SelectedBlock);
end;

function TRichEditorViewKMemo.IsImageSelected: Boolean;
begin
  Result := IsBlockSelected and (SelectedBlock is TKMemoImageBlock);
end;

procedure TRichEditorViewKMemo.Clear;
begin
  FEditor.Clear;
end;

procedure TRichEditorViewKMemo.Cut;
begin
  FEditor.ExecuteCommand(ecCut);
end;

procedure TRichEditorViewKMemo.Copy;
begin
  FEditor.ExecuteCommand(ecCopy);
end;

procedure TRichEditorViewKMemo.Paste;
begin
  FEditor.ExecuteCommand(ecPaste);
end;

procedure TRichEditorViewKMemo.Undo;
begin
  FEditor.ExecuteCommand(ecUndo); // not supported yet by TKMemo
end;

procedure TRichEditorViewKMemo.Redo;
begin
  FEditor.ExecuteCommand(ecRedo);  // not supported yet by TKMemo
end;

procedure TRichEditorViewKMemo.AddParagraph;
begin
  FEditor.ActiveInnerBlocks.AddParagraph;
  FEditor.ExecuteCommand(ecInsertNewLine);
  Events.DoModified;
  Logger.Info('AddParagraph');
end;
{$ENDREGION}

{$REGION 'public methods'}
function TRichEditorViewKMemo.Focused: Boolean;
begin
  Result := FEditor.Focused;
end;

procedure TRichEditorViewKMemo.UpdateActions;
begin
  if Assigned(FTextStyle) then
  begin
    FTextStyle.OnChanged := nil;
    try
      if FEditor.NewTextStyleValid then
        FTextStyle.Assign(FEditor.NewTextStyle)
      else
        FTextStyle.Assign(FEditor.SelectionTextStyle);
    finally
      FTextStyle.OnChanged := FTextStyleChanged;
    end;
  end;
  if Assigned(FParaStyle) then
  begin
    FParaStyle.OnChanged := nil;
    try
      FParaStyle.Assign(FEditor.SelectionParaStyle)
    finally
      FParaStyle.OnChanged := FParaStyleChanged;
    end;
  end;
  if Assigned(Actions) then
    Actions.UpdateActions;
  inherited UpdateActions;
end;
{$ENDREGION}

end.
