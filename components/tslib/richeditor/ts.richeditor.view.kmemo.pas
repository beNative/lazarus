{
  Copyright (C) 2013-2023 Tim Sinaeve tim.sinaeve@gmail.com

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
   - copy formatted text (WIKI, HTML?)
   - Undo/Redo is not yet supported by the KMemo component
   - drag and drop images in editor view
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  ExtCtrls, Menus, Types,

  KControls, KMemo, KMemoDlgTextStyle, KMemoDlgHyperlink, KMemoDlgImage,
  KMemoDlgNumbering, KMemoDlgContainer, KMemoDlgParaStyle, kdialogs,

  DropComboTarget, DropTarget,

  ts.RichEditor.Interfaces;

type

  { TRichEditorViewKMemo }

  TRichEditorViewKMemo = class(TForm, IRichEditorView)
    dctMain       : TDropComboTarget;
    pnlRichEditor : TPanel;

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

  private
    FEditor        : TKMemo;
    FTextStyle     : TKMemoTextStyle;
    FParaStyle     : TKMemoParaStyle;
    FUpdateLock    : Integer;
    FDefaultIndent : Integer;
    FOnChange      : TNotifyEvent;
    FOnDropFiles   : TDropFilesEvent;
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
    function FEditorAlignInsertBefore(Sender: TWinControl; Control1,
      Control2: TControl): Boolean;
    procedure FEditorAlignPosition(Sender: TWinControl; Control: TControl;
      var NewLeft, NewTop, NewWidth, NewHeight: Integer; var AlignRect: TRect;
      AlignInfo: TAlignInfo);
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
    procedure FEditorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FParaStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons);
    procedure FTextStyleChanged(Sender: TObject);
    function GetKMemoNotifier: IKMemoNotifier;
    {$ENDREGION}

    function SelectedBlock: TKMemoBlock;

    function EditContainer(AItem: TKMemoContainer): Boolean;
    function EditImage(AItem: TKMemoImageBlock): Boolean;
    function EditHyperlink(AItem: TKMemoHyperlink): Boolean;

  protected
    {$REGION 'property access mehods'}
    function GetActions: IRichEditorActions;
    function GetAlignCenter: Boolean;
    function GetAlignJustify: Boolean;
    function GetAlignLeft: Boolean;
    function GetAlignRight: Boolean;
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
    function GetModified: Boolean;
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
    procedure SetFileName(const AValue: string);
    procedure SetIsFile(AValue: Boolean);
    procedure SetModified(const AValue: Boolean);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TDropFilesEvent);
    procedure SetPopupMenu(const AValue: TPopupMenu); reintroduce;
    procedure SetSelEnd(const AValue: Integer);
    procedure SetSelStart(const AValue: Integer);
    procedure SetSelText(const AValue: string);
    procedure SetShowSpecialChars(AValue: Boolean);
    procedure SetText(const AValue: string);
    procedure SetWordWrap(const AValue: Boolean);
    {$ENDREGION}

    procedure SelectAll;
    procedure EditSelectedItem;
    procedure LoadFromFile(const AFileName: string);
    procedure Load(const AStorageName: string = '');
    procedure LoadFromStream(AStream: TStream);
    procedure Save(const AStorageName: string = '');
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);

    procedure BeginUpdate;
    function IsUpdating: Boolean;
    procedure EndUpdate;

    procedure InsertImageFile(const AFileName: string);
    procedure InsertImage(AImage: TPicture); overload;
    function InsertImage: Boolean; overload;
    procedure InsertHyperlink(
      const AText : string = '';
      const AURL  : string = ''
    );
    procedure InsertBulletList;
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

    property KMemoNotifier: IKMemoNotifier
      read GetKMemoNotifier;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure UpdateActions; override;
    function Focused: Boolean; override;

    property Actions: IRichEditorActions
      read GetActions;

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

    property Text: string
      read GetText write SetText;

    property RTFText: string
      read GetRTFText;

    property ShowSpecialChars: Boolean
      read GetShowSpecialChars write SetShowSpecialChars;

    property WordWrap: Boolean
      read GetWordWrap write SetWordWrap;

    property OnDropFiles: TDropFilesEvent
      read GetOnDropFiles write SetOnDropFiles;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;
  end;

implementation

{$R *.lfm}

uses
  StdCtrls, Math, StrUtils, Printers,

  keditcommon, kgraphics,

  ts.Core.Logger;

{$REGION 'construction and destruction'}
procedure TRichEditorViewKMemo.AfterConstruction;
var
  LKey : TKEditKey;
begin
  Logger.Enter(Self, 'AfterConstruction');
  inherited AfterConstruction;
  FEditor := TKMemo.Create(Self);
  FEditor.Parent         := pnlRichEditor;
  FEditor.BorderStyle    := bsNone;
  FEditor.ScrollBars     := ssBoth;
  FEditor.Align          := alClient;
  FEditor.DoubleBuffered := True;
  FEditor.OnChange       := FEditorChange;
  FEditor.OnMouseUp  := FEditorMouseUp;
  FEditor.OnEndDrag  := FEditorEndDrag;
  FEditor.OnExit  := FEditorExit;
  FEditor.OnBlockEdit    := FEditorBlockEdit;
  FEditor.OnBlockClick   := FEditorBlockClick;
  FEditor.OnAlignPosition  := FEditorAlignPosition;
  FEditor.OnAlignInsertBefore  := FEditorAlignInsertBefore;
  FEditor.Options        := FEditor.Options + [eoWantTab];
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

  Logger.Leave(Self, 'AfterConstruction');
end;

destructor TRichEditorViewKMemo.Destroy;
begin
  FTextStyle.Free;
  FParaStyle.Free;
  inherited Destroy;
  Logger.Info('TRichEditorViewKMemo.Destroy');
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
    if not Assigned(FEditor.NearestParagraph) then
    begin
      FEditor.ActiveBlocks.AddParagraph;
    end;
    FEditor.NearestParagraph.ParaStyle.HAlign := halCenter;
  end;
      //FParaStyle.HAlign := halCenter;
end;

function TRichEditorViewKMemo.GetAlignJustify: Boolean;
begin

  Result := FParaStyle.HAlign = halJustify;
end;

procedure TRichEditorViewKMemo.SetAlignJustify(AValue: Boolean);
begin
  if AValue then
  begin
    if not Assigned(FEditor.NearestParagraph) then
    begin
      FEditor.ActiveBlocks.AddParagraph;
    end;
    FEditor.NearestParagraph.ParaStyle.HAlign := halJustify;
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
    if not Assigned(FEditor.NearestParagraph) then
    begin
      FEditor.ActiveBlocks.AddParagraph;
    end;
    FEditor.NearestParagraph.ParaStyle.HAlign := halLeft;
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
    if not Assigned(FEditor.NearestParagraph) then
    begin
      FEditor.ActiveBlocks.AddParagraph;
    end;
    FEditor.NearestParagraph.ParaStyle.HAlign := halRight;
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
  Logger.Info('EditorChange');
  Modified := True;
  DoChange;
end;

function TRichEditorViewKMemo.FEditorAlignInsertBefore(Sender: TWinControl;
  Control1, Control2: TControl): Boolean;
begin
  Modified := True;
  DoChange;
end;

procedure TRichEditorViewKMemo.FEditorAlignPosition(Sender: TWinControl;
  Control: TControl; var NewLeft, NewTop, NewWidth, NewHeight: Integer;
  var AlignRect: TRect; AlignInfo: TAlignInfo);
begin
  Modified := True;
  DoChange;
end;

procedure TRichEditorViewKMemo.FEditorBlockClick(Sender: TObject;
  ABlock: TKMemoBlock; var Result: Boolean);
begin
  Modified := True;
  DoChange;
end;

{ Gets called when executing IKMemoNotifier.EditBlock. }

procedure TRichEditorViewKMemo.FEditorBlockEdit(Sender: TObject;
  ABlock: TKMemoBlock; var Result: Boolean);
begin
  if ABlock is TKMemoContainer then
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

procedure TRichEditorViewKMemo.FEditorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Modified := True;
  DoChange;
end;

procedure TRichEditorViewKMemo.FParaStyleChanged(Sender: TObject;
  AReasons: TKMemoUpdateReasons);
begin
  FEditor.SelectionParaStyle := FParaStyle;
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
{$ENDREGION}
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TRichEditorViewKMemo.DoDropFiles(const AFileNames: array of string);
begin
  if Assigned(FOnDropFiles) then
    FOnDropFiles(Self, AFileNames);
end;

procedure TRichEditorViewKMemo.DoChange;
begin
  if Assigned(OnChange) and not IsUpdating then
  begin
    OnChange(Self);
  end;
  Logger.Watch('ContentHeight', FEditor.ContentHeight);
  Logger.Watch('ContentWidth', FEditor.ContentWidth);
  Logger.Watch('ContentLeft', FEditor.ContentLeft);
  Logger.Watch('ContentTop', FEditor.ContentTop);
  Logger.Watch('SelText', FEditor.Blocks.SelText);
end;
{$ENDREGION}

{$REGION 'private methods'}
function TRichEditorViewKMemo.SelectedBlock: TKMemoBlock;
begin
  Result := FEditor.SelectedBlock;
  if not Assigned(Result) then
    Result := FEditor.ActiveInnerBlock;
end;

function TRichEditorViewKMemo.EditContainer(AItem: TKMemoContainer): Boolean;
begin
  Result := False;
  FContainerForm.Load(Editor, AItem);
  if FContainerForm.ShowModal = mrOk then
  begin
    FContainerForm.Save(AItem);
    Editor.Modified := True;
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
    Result := True;
  end;
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
    P.LoadFromFile(AFileName);
    InsertImage(P);
  finally
    P.Free;
  end;
end;

procedure TRichEditorViewKMemo.InsertImage(AImage: TPicture);
begin
  // TKMemo does only handle jpg and png well, so we convert any other
  // image types to jpeg first.
  if not MatchStr(AImage.Graphic.MimeType, ['image/jpg', 'image/png']) then
  begin
    AImage.Assign(AImage.Jpeg);
  end;
  FEditor.Blocks.AddImageBlock(AImage);
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
  end
  else if LCreated then
    LHyperlink.Free;
end;

procedure TRichEditorViewKMemo.InsertBulletList;
begin
  FNumberingForm.Load(FEditor, FEditor.ListTable, FEditor.NearestParagraph);
  if FNumberingForm.ShowModal = mrOk then
    FNumberingForm.Save;
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

//procedure TRichEditorViewKMemo.AdjustParagraphStyle;
//begin
//  FParaStyleForm.Load(FEditor, FParaStyle);
//  if FParaStyleForm.ShowModal = mrOk then
//    FParaStyleForm.Save(FParaStyle);
//end;

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
  FEditor.ActiveBlocks.AddParagraph;
  FEditor.ExecuteCommand(ecInsertNewLine);
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
