{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.RichEditor.View.KMemo;


{$MODE DELPHI}

{ A richtext editor view based on KMemo }

{
  TODO:
   - store images
   - drop files
   - paste formatted text (HTML?)
   - copy formatted text (WIKI, HTML?)
   - SetCaret__ methods don't work because readonly in RichMemo

   - Undo/Redo is not yet supported by the KMemo component
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  ExtCtrls, Menus,

  KControls, KMemo, KMemoDlgParaStyle, KMemoDlgTextStyle, KMemoDlgHyperlink,
  KMemoDlgImage, KMemoDlgNumbering, KMemoDlgContainer,

  ts.RichEditor.Interfaces;

type

  { TRichEditorViewKMemo }

  TRichEditorViewKMemo = class(TForm, IRichEditorView)
    pnlRichEditor : TPanel;
  private
    FEditor        : TKMemo;
    FTextStyle     : TKMemoTextStyle;
    FParaStyle     : TKMemoParaStyle;
    FUpdateLock    : Integer;
    FOnChange      : TNotifyEvent;
    FOnDropFiles   : TDropFilesEvent;
    //FParaStyleForm : TKMemoParaStyleForm;
    FTextStyleForm : TKMemoTextStyleForm;
    FContainerForm : TKMemoContainerForm;
    FHyperlinkForm : TKMemoHyperlinkForm;
    FNumberingForm : TKMemoNumberingForm;
    FImageForm     : TKMemoImageForm;
    FFileName      : string;
    FIsFile        : Boolean;

    {$REGION 'event handlers'}
    procedure EditorChange(Sender: TObject);
    procedure FEditorBlockClick(Sender: TObject; ABlock: TKMemoBlock;
      var Result: Boolean);
    procedure FEditorBlockEdit(Sender: TObject; ABlock: TKMemoBlock;
      var Result: Boolean);
    procedure FEditorDropFiles(Sender: TObject; X, Y: Integer; Files: TStrings);
    procedure FParaStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons);
    procedure FTextStyleChanged(Sender: TObject);
    function GetEvents: IRichEditorEvents;
    function GetIsFile: Boolean;
    {$ENDREGION}

    function SelectedBlock: TKMemoBlock;
    procedure SetIsFile(AValue: Boolean);

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
    function GetEditor: TComponent;
    function GetFileName: string;
    function GetFont: TFont;
    function GetForm: TCustomForm;
    function GetModified: Boolean;
    function GetOnChange: TNotifyEvent;
    function GetOnDropFiles: TDropFilesEvent;
    function GetPopupMenu: TPopupMenu; override;
    function GetSelAvail: Boolean;
    function GetSelEnd: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    function GetText: string;
    function GetWordWrap: Boolean;
    procedure SetAlignCenter(AValue: Boolean);
    procedure SetAlignJustify(AValue: Boolean);
    procedure SetAlignLeft(AValue: Boolean);
    procedure SetAlignRight(AValue: Boolean);
    procedure SetFileName(const AValue: string);
    procedure SetModified(const AValue: Boolean);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TDropFilesEvent);
    procedure SetPopupMenu(const AValue: TPopupMenu); reintroduce;
    procedure SetSelEnd(const AValue: Integer);
    procedure SetSelStart(const AValue: Integer);
    procedure SetSelText(const AValue: string);
    procedure SetText(const AValue: string);
    procedure SetWordWrap(const AValue: Boolean);
    {$ENDREGION}

    procedure SelectAll;
    procedure LoadFromFile(const AFileName: string);
    procedure Load(const AStorageName: string = '');
    procedure LoadFromStream(AStream: TStream);
    procedure Save(const AStorageName: string = '');
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure BeginUpdate;
    procedure EndUpdate;

    function InsertImage: Boolean;
    procedure InsertHyperlink;
    function IsUpdating: Boolean;
    function IsEmpty: Boolean;

    procedure Clear;

    // clipboard commands
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;
    procedure Redo;

    procedure DoDropFiles(const AFileNames: array of string);
    procedure DoChange;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure UpdateActions; override;

    property Actions: IRichEditorActions
      read GetActions;

    property CanPaste: Boolean
      read GetCanPaste;

    property CanUndo: Boolean
      read GetCanUndo;

    property CanRedo: Boolean
      read GetCanRedo;

    property Events: IRichEditorEvents
      read GetEvents;

    property Editor: TComponent
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

    property IsFile: Boolean
      read GetIsFile write SetIsFile;

    property Text: string
      read GetText write SetText;

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
  StdCtrls,

  keditcommon, kgraphics;

{$REGION 'construction and destruction'}
procedure TRichEditorViewKMemo.AfterConstruction;
begin
  inherited AfterConstruction;
  FEditor := TKMemo.Create(Self);
  FEditor.Parent         := pnlRichEditor;
  FEditor.BorderStyle    := bsNone;
  FEditor.ScrollBars     := ssBoth;
  FEditor.Align          := alClient;
  FEditor.DoubleBuffered := True;
  FEditor.OnDropFiles    := FEditorDropFiles;
  FEditor.OnChange       := EditorChange;
  FEditor.OnBlockEdit    := FEditorBlockEdit;
  FEditor.OnBlockClick   := FEditorBlockClick;

  FTextStyle := TKMemoTextStyle.Create;
  FTextStyle.OnChanged := FTextStyleChanged;

  FParaStyle := TKMemoParaStyle.Create;
  FParaStyle.OnChanged := FParaStyleChanged;

  FContainerForm := TKMemoContainerForm.Create(Self);
  FHyperlinkForm := TKMemoHyperlinkForm.Create(Self);
  FImageForm     := TKMemoImageForm.Create(Self);
  FNumberingForm := TKMemoNumberingForm.Create(Self);
  FTextStyleForm := TKMemoTextStyleForm.Create(Self);
end;

procedure TRichEditorViewKMemo.BeforeDestruction;
begin
  FTextStyle.Free;
  FParaStyle.Free;
  inherited BeforeDestruction;
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
//  Result := FEditor.CommandEnabled(ecUndo);
  Result := False; // not supported yet
end;

function TRichEditorViewKMemo.GetEditor: TComponent;
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
  FEditor.Modified := True;
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
  Result := FEditor.SelAvail;
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
    FParaStyle.HAlign := halCenter;
end;

function TRichEditorViewKMemo.GetAlignJustify: Boolean;
begin
  Result := FParaStyle.HAlign = halJustify;
end;

procedure TRichEditorViewKMemo.SetAlignJustify(AValue: Boolean);
begin
  if AValue then
    FParaStyle.HAlign := halJustify;
end;

function TRichEditorViewKMemo.GetAlignLeft: Boolean;
begin
  Result := FParaStyle.HAlign = halLeft;
end;

procedure TRichEditorViewKMemo.SetAlignLeft(AValue: Boolean);
begin
  if AValue then
    FParaStyle.HAlign := halLeft;
end;

function TRichEditorViewKMemo.GetAlignRight: Boolean;
begin
  Result := FParaStyle.HAlign = halRight;
end;

function TRichEditorViewKMemo.GetCanRedo: Boolean;
begin
  // Result := FEditor.CommandEnabled(ecRedo);
  Result := False;
end;

procedure TRichEditorViewKMemo.SetAlignRight(AValue: Boolean);
begin
  if AValue then
    FParaStyle.HAlign := halRight;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TRichEditorViewKMemo.EditorChange(Sender: TObject);
begin
  DoChange;
end;

procedure TRichEditorViewKMemo.FEditorBlockClick(Sender: TObject;
  ABlock: TKMemoBlock; var Result: Boolean);
begin
  FEditor.Modified :=  True;
  DoChange;
end;

procedure TRichEditorViewKMemo.FEditorBlockEdit(Sender: TObject;
  ABlock: TKMemoBlock; var Result: Boolean);
begin
  DoChange;
end;

procedure TRichEditorViewKMemo.FEditorDropFiles(Sender: TObject; X, Y: Integer;
  Files: TStrings);
var
  LFiles : array of string;
  I      : Integer;
begin
  SetLength(LFiles, Files.Count);
  try
    for I := 0 to Files.Count - 1 do
    begin
      LFiles[I] := Files[I];
    end;
    DoDropFiles(LFiles);
  finally
    LFiles := nil;
  end;
end;

procedure TRichEditorViewKMemo.FParaStyleChanged(Sender: TObject;
  AReasons: TKMemoUpdateReasons);
begin
  FEditor.SelectionParaStyle := FParaStyle;
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
  LSelEnd := FEditor.SelEnd;
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
  DoChange;
end;
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
end;
{$ENDREGION}

{$REGION 'private methods'}
function TRichEditorViewKMemo.SelectedBlock: TKMemoBlock;
begin
  Result := FEditor.SelectedBlock;
  if Result = nil then
    Result := FEditor.ActiveInnerBlock;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TRichEditorViewKMemo.SelectAll;
begin
  FEditor.Select(0, FEditor.SelectableLength);
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
  Events.DoBeforeSave(AStorageName);
  if IsFile then
  begin
    FileName := AStorageName;
    SaveToFile(AStorageName);
  end;
  Events.DoAfterSave(AStorageName);
  Modified := False;
end;

procedure TRichEditorViewKMemo.SaveToStream(AStream: TStream);
begin
  FEditor.SaveToRTFStream(AStream);
  FEditor.ClearUndo;
end;

procedure TRichEditorViewKMemo.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TRichEditorViewKMemo.EndUpdate;
begin
  if FUpdateLock > 0 then
    Dec(FUpdateLock);
end;

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
    FEditor.Modified := True;
    Result := True;
  end
  else if LCreated then
    LImage.Free;
end;

procedure TRichEditorViewKMemo.InsertHyperlink;
var
  LBlock     : TKMemoBlock;
  LHyperlink : TKMemoHyperlink;
  LCreated   : Boolean;
begin
  LCreated := False;
  if FEditor.SelAvail then
  begin
    LHyperlink := TKMemoHyperlink.Create;
    LHyperlink.Text := FEditor.SelText;
    LBlock := FEditor.ActiveInnerBlock;
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
      LCreated := True;
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
      FEditor.ActiveInnerBlocks.AddHyperlink(LHyperlink, FEditor.SplitAt(FEditor.SelEnd));
    end;
    FEditor.Modified := True;
  end
  else if LCreated then
    LHyperlink.Free;
end;

function TRichEditorViewKMemo.IsUpdating: Boolean;
begin
  Result := FUpdateLock > 0;
end;

function TRichEditorViewKMemo.IsEmpty: Boolean;
begin
  Result := FEditor.Empty;
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
  FEditor.ExecuteCommand(ecUndo);
end;

procedure TRichEditorViewKMemo.Redo;
begin
  FEditor.ExecuteCommand(ecRedo);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TRichEditorViewKMemo.UpdateActions;
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

  FParaStyle.OnChanged := nil;
  try
    FParaStyle.Assign(FEditor.SelectionParaStyle)
  finally
    FParaStyle.OnChanged := FParaStyleChanged;
  end;

  if Assigned(Actions) then
    Actions.UpdateActions;
  inherited UpdateActions;
end;
{$ENDREGION}

end.

