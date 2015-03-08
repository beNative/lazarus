{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts_RichEditor_View;

{$MODE Delphi}

{ A richtext editor view based on RichMemo.

- RichMemo is platform independent (maybe implement RichMemo version as well)

}

{
  TODO:
   - store images
   - drop files
   - paste formatted text (HTML?)
   - copy formatted text (WIKI, HTML?)
   - SetCaret__ methods don't work because readonly in RichMemo
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus,

  LCLType, LMessages,

  RichMemo,

  ts.RichEditor.Interfaces, ts.RichEditor.TextAttributes;

type

  { TRichEditorView }

  TRichEditorView = class(TForm, IRichEditorView)
    pnlRichEditor: TPanel;

    procedure EditorChange(Sender: TObject);
    procedure EditorEditingDone(Sender: TObject);
    procedure EditorOnClick(Sender: TObject);
    procedure EditorSelectionChange(Sender: TObject);

    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure TextAttributesUpdate(Sender: TObject);
    procedure UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char); reintroduce;

  private
    FEditor            : TRichMemo;
    FActions           : IRichEditorActions;
    FFileName          : string;
    FOnDropFiles       : TDropFilesEvent;
    FOnEditingDone     : TNotifyEvent;
    FOnChange          : TNotifyEvent;
    FOnSelectionChange : TNotifyEvent;
    FTextAttributes    : TTextAttributes;

  protected
    {$region 'property access methods' /fold}
    function GetActions: IRichEditorActions;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretX: Integer;
    function GetCaretXY: TPoint;
    function GetCaretY: Integer;
    function GetEditor: TRichMemo;
    function GetFileName: string;
    function GetForm: TCustomForm;
    function GetModified: Boolean;
    function GetOnChange: TNotifyEvent;
    function GetOnDropFiles: TDropFilesEvent;
    function GetOnEditingDone: TNotifyEvent;
    function GetSelAvail: Boolean;
    function GetSelEnd: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    function GetTextAttributes: TTextAttributes;
    function GetWordWrap: Boolean;
    procedure SetCaretX(const AValue: Integer);
    procedure SetCaretXY(const AValue: TPoint);
    procedure SetCaretY(const AValue: Integer);
    procedure SetFileName(const AValue: string);
    procedure SetModified(const AValue: Boolean);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TDropFilesEvent);
    procedure SetOnEditingDone(const AValue: TNotifyEvent);
    procedure SetPopupMenu(const AValue: TPopupMenu); reintroduce;
    procedure SetSelEnd(const AValue: Integer);
    procedure SetSelStart(const AValue: Integer);
    procedure SetSelText(const AValue: string);
    procedure SetWordWrap(const AValue: Boolean);
    function GetPopupMenu: TPopupMenu; override;
    function GetOnSelectionChange: TNotifyEvent;
    procedure SetOnSelectionChange(AValue: TNotifyEvent);
    {$endregion}

    procedure UpdateActions; override;

    procedure DoEditingDone; dynamic;
    procedure DoChange; dynamic;
    procedure DoSelectionChange; dynamic;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function Focused: Boolean; override;

    procedure SetFocus; override;
    procedure SelectAll;
    procedure Clear;
    procedure Copy;
    procedure Cut;
    procedure Paste;
    procedure Undo;
    procedure Redo;
//    function GetWordAtPosition(APosition: TPoint): string;

    //function GetWordFromCaret(const ACaretPos: TPoint): string;
    //procedure SelectWord;
    //procedure InsertTextAtCaret(const AText: string);
    //procedure SearchAndSelectLine(const ALine: string);
    //procedure SearchAndSelectText(const AText: string);
    //procedure Search;
    //procedure SelectBlockAroundCursor(const AStartTag, AEndTag: string;
    //  AIncludeStartTag, AIncludeEndTag: Boolean);
    //procedure UpperCaseSelection;
    //procedure LowerCaseSelection;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure BeginUpdate;
    procedure EndUpdate;

    property Actions: IRichEditorActions
      read GetActions;

    { current X-coordinate of the caret. }
    property CaretX: Integer
      read GetCaretX write SetCaretX;

    { current Y-coordinate of the caret. }
    property CaretY: Integer
      read GetCaretY write SetCaretY;

    property CaretXY: TPoint
      read GetCaretXY write SetCaretXY;

    property CanPaste: Boolean
      read GetCanPaste;

    property CanRedo: Boolean
      read GetCanRedo;

    property CanUndo: Boolean
      read GetCanUndo;

    property Editor: TRichMemo
      read GetEditor;

    property FileName: string
      read GetFileName write SetFileName;

    property SelStart: Integer
      read GetSelStart write SetSelStart;

    property SelEnd: Integer
      read GetSelEnd write SetSelEnd;

    property SelAvail: Boolean
      read GetSelAvail;

    property SelText: string
      read GetSelText write SetSelText;

    property Modified: Boolean
      read GetModified write SetModified;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

    property Form: TCustomForm
      read GetForm;

    property TextAttributes: TTextAttributes
      read GetTextAttributes;

    property WordWrap: Boolean
      read GetWordWrap write SetWordWrap;

    property OnDropFiles: TDropFilesEvent
      read GetOnDropFiles write SetOnDropFiles;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;

    property OnEditingDone: TNotifyEvent
      read GetOnEditingDone write SetOnEditingDone;

    property OnSelectionChange: TNotifyEvent
      read GetOnSelectionChange write SetOnSelectionChange;
  end; 

implementation

{$R *.lfm}

uses
  StdCtrls, Clipbrd;

{$region 'construction and destruction' /fold}
procedure TRichEditorView.AfterConstruction;
begin
  inherited AfterConstruction;
  FActions := Owner as IRichEditorActions;
  FEditor                := TRichMemo.Create(Self);
  FEditor.Parent         := pnlRichEditor;
  FEditor.BorderStyle    := bsNone;
  FEditor.ScrollBars     := ssAutoBoth;
  FEditor.Align          := alClient;
  FEditor.DoubleBuffered := True;
  FEditor.OnChange       := EditorChange;
  FEditor.OnEditingDone  := EditorEditingDone;
  FEditor.OnSelectionChange := EditorSelectionChange;
  FEditor.OnUTF8KeyPress := UTF8KeyPress;
  FEditor.OnClick        := EditorOnClick;

  FTextAttributes := TTextAttributes.Create(FEditor);
  FTextAttributes.OnUpdate  := TextAttributesUpdate;
end;

procedure TRichEditorView.BeforeDestruction;
begin
  FreeAndNil(FTextAttributes);
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'event handlers' /fold}
procedure TRichEditorView.TextAttributesUpdate(Sender: TObject);
begin
  //Editor.SetTextAttributes(SelStart, SelEnd - SelStart, TextAttributes.TextParams);
end;

procedure TRichEditorView.UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  FTextAttributes.UpdateAttributes;
end;

function TRichEditorView.GetOnSelectionChange: TNotifyEvent;
begin
  Result := FOnSelectionChange;
end;

procedure TRichEditorView.SetOnSelectionChange(AValue: TNotifyEvent);
begin
  FOnSelectionChange := AValue;
end;

procedure TRichEditorView.EditorChange(Sender: TObject);
begin
  FTextAttributes.UpdateAttributes;
  DoChange;
end;

procedure TRichEditorView.EditorEditingDone(Sender: TObject);
begin
  FTextAttributes.UpdateAttributes;
  if Assigned(OnEditingDone) then
    OnEditingDone(Self);
end;

procedure TRichEditorView.EditorOnClick(Sender: TObject);
begin
  FTextAttributes.UpdateAttributes;
end;

procedure TRichEditorView.EditorSelectionChange(Sender: TObject);
begin
  DoSelectionChange;
end;

{ Required to be able to handle shortcuts of the action list on the main form. }

procedure TRichEditorView.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
begin
  Handled := Actions.Actions.IsShortCut(Msg);
end;
{$endregion}

{$region 'property access mehods' /fold}
function TRichEditorView.GetEditor: TRichMemo;
begin
  Result := FEditor;
end;

function TRichEditorView.GetCanPaste: Boolean;
begin
  Result := not Editor.ReadOnly;
end;

function TRichEditorView.GetActions: IRichEditorActions;
begin
  Result := FActions;
end;

function TRichEditorView.GetCanRedo: Boolean;
begin
  Result := Editor.CanUndo;
end;

function TRichEditorView.GetCanUndo: Boolean;
begin
  Result := Editor.CanUndo;
end;

function TRichEditorView.GetCaretX: Integer;
begin
  Result := Editor.CaretPos.X;
end;

procedure TRichEditorView.SetCaretX(const AValue: Integer);
begin
  Editor.CaretPos := Point(AValue, CaretY);
end;

function TRichEditorView.GetCaretXY: TPoint;
begin
  Result := Editor.CaretPos;
end;

procedure TRichEditorView.SetCaretXY(const AValue: TPoint);
begin
  Editor.CaretPos := AValue;
end;

function TRichEditorView.GetCaretY: Integer;
begin
  Result := Editor.CaretPos.Y;
end;

procedure TRichEditorView.SetCaretY(const AValue: Integer);
begin
  Editor.CaretPos := Point(CaretX, AValue);
end;

function TRichEditorView.GetFileName: string;
begin
  Result := FFileName;
end;

function TRichEditorView.GetForm: TCustomForm;
begin
  Result := Self;
end;

procedure TRichEditorView.SetFileName(const AValue: string);
begin
  if AValue <> FileName then
  begin
    FFileName := AValue;
  end;
end;

function TRichEditorView.GetModified: Boolean;
begin
  Result := Editor.Modified;
end;

procedure TRichEditorView.SetModified(const AValue: Boolean);
begin
  Editor.Modified := AValue;
end;

function TRichEditorView.GetOnChange: TNotifyEvent;
begin
  Result := FOnChange;
end;

procedure TRichEditorView.SetOnChange(const AValue: TNotifyEvent);
begin
  FOnChange := AValue;
end;

function TRichEditorView.GetOnDropFiles: TDropFilesEvent;
begin
  Result := FOnDropFiles;
end;

procedure TRichEditorView.SetOnDropFiles(const AValue: TDropFilesEvent);
begin
  FOnDropFiles := AValue;
end;

function TRichEditorView.GetOnEditingDone: TNotifyEvent;
begin
  Result := FOnEditingDone;
end;

function TRichEditorView.GetPopupMenu: TPopupMenu;
begin
  Result := FEditor.PopupMenu;
end;

procedure TRichEditorView.SetPopupMenu(const AValue: TPopupMenu);
begin
  FEditor.PopupMenu := AValue;
end;

procedure TRichEditorView.SetOnEditingDone(const AValue: TNotifyEvent);
begin
  FOnEditingDone := AValue;
end;

function TRichEditorView.GetSelAvail: Boolean;
begin
  Result := Editor.SelLength > 0;
end;

function TRichEditorView.GetSelEnd: Integer;
begin
  Result := Editor.SelStart + Editor.SelLength;
end;

procedure TRichEditorView.SetSelEnd(const AValue: Integer);
begin
  if AValue <> SelEnd then
  begin
    Editor.SelLength := AValue - Editor.SelStart;
  end;
end;

function TRichEditorView.GetSelStart: Integer;
begin
  Result := Editor.SelStart;
end;

procedure TRichEditorView.SetSelStart(const AValue: Integer);
begin
  Editor.SelStart := AValue;
end;

function TRichEditorView.GetSelText: string;
begin
  Result := Editor.SelText;
end;

procedure TRichEditorView.SetSelText(const AValue: string);
begin
  Editor.SelText := AValue;
end;

function TRichEditorView.GetTextAttributes: TTextAttributes;
begin
  Result := FTextAttributes;
end;

function TRichEditorView.GetWordWrap: Boolean;
begin
  Result := Editor.WordWrap;
end;

procedure TRichEditorView.SetWordWrap(const AValue: Boolean);
begin
  Editor.WordWrap := AValue;
end;
{$endregion}

{$region 'event dispatch methods' /fold}
procedure TRichEditorView.DoEditingDone;
begin
  if Assigned(OnEditingDone) then
    OnEditingDone(Self);
end;

procedure TRichEditorView.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TRichEditorView.DoSelectionChange;
begin
  if Assigned(OnSelectionChange) then
    OnSelectionChange(Self);
end;
{$endregion}

{$region 'public methods' /fold}
procedure TRichEditorView.UpdateActions;
begin
  inherited UpdateActions;
  if Focused then
  begin
    Actions.ActiveView := Self as IRichEditorView;
  end;
  if Assigned(Actions) then
    Actions.UpdateActions;
end;

function TRichEditorView.Focused: Boolean;
begin
  Result := inherited Focused or Editor.Focused;
end;

procedure TRichEditorView.SetFocus;
begin
  if Editor.CanFocus then
  begin
    inherited;
    Editor.SetFocus;
  end;
end;

procedure TRichEditorView.SelectAll;
begin
  Editor.SelectAll;
end;

procedure TRichEditorView.Clear;
begin
  Editor.Clear;
  Editor.Invalidate;
end;

procedure TRichEditorView.Copy;
begin
  Editor.CopyToClipboard;
end;

procedure TRichEditorView.Cut;
begin
  Editor.CutToClipboard;
end;

procedure TRichEditorView.Paste;
var
  Image: TPicture;
begin
  Image := TPicture.Create;
  try
//    if Clipboard.HasPictureFormat then
//    begin
//
////      Image.Bitmap.SaveToClipboardFormat(2);
//    //lzRichEdit1.PasteFromClipboard;
//
//      //if Clipboard.HasFormat(CF_BITMAP) then
//      //  Image.Assign(Clipboard);
//  //    Image.Bitmap.Assign(Clipboard);
////      Editor.InsertImage(SelStart, Image );
//    end
//    else
      Editor.PasteFromClipboard;
  finally
    FreeAndNil(Image);
  end;
end;

procedure TRichEditorView.Undo;
begin
  Editor.Undo;
end;

procedure TRichEditorView.Redo;
begin
  ShowMessage('Not supported');
end;

procedure TRichEditorView.LoadFromFile(const AFileName: string);
var
  FS : TFileStream;
begin
  if FileExists(AFileName) then
  begin
    FileName := AFileName;
    FS := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
    try
      LoadFromStream(FS);
    finally
      FreeAndNil(FS);
    end;
    Modified := False;
  end;
end;

procedure TRichEditorView.LoadFromStream(AStream: TStream);
begin
  Editor.LoadRichText(AStream);
end;

procedure TRichEditorView.SaveToStream(AStream: TStream);
begin
  Editor.SaveRichText(AStream);
end;

procedure TRichEditorView.SaveToFile(const AFileName: string);
var
  FS : TFileStream;
  FN : string;
begin
  if AFileName <> '' then
  begin
    FN := Utf8ToAnsi(AFileName);
    FS := TFileStream.Create(FN, fmCreate);
    SaveToStream(FS);
    Modified := False;
    FreeAndNil(FS);
  end;
end;

procedure TRichEditorView.BeginUpdate;
begin
  Editor.BeginUpdateBounds;
  Editor.DisableAutoSizing;
end;

procedure TRichEditorView.EndUpdate;
begin
  Editor.EnableAutoSizing;
  Editor.EndUpdateBounds;
end;
{$endregion}




end.

