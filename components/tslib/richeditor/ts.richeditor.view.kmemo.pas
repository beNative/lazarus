{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,

  KFunctions, KControls, KMemo, ActnList, ExtCtrls,
  KDialogs, KMemoDlgParaStyle, KMemoDlgTextStyle, KMemoDlgHyperlink,
  KMemoDlgImage, KMemoDlgNumbering, KMemoDlgContainer,

  ts.RichEditor.Interfaces, ts.RichEditor.TextAttributes;

type

  { TRichEditorViewKMemo }

  TRichEditorViewKMemo = class(TForm, IRichEditorView)
    pnlRichEditor: TPanel;
  private
    FEditor            : TKMemo;
    FActions           : IRichEditorActions;

    {$REGION 'event handlers'}
    procedure EditorChange(Sender: TObject);
    procedure EditorOnClick(Sender: TObject);
    procedure UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    {$ENDREGION}

    {$REGION 'property access mehods'}
    function GetActions: IRichEditorActions;
    function GetCanPaste: Boolean;
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
    function GetOnSelectionChange: TNotifyEvent;
    function GetSelAvail: Boolean;
    function GetSelEnd: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    function GetTextAttributes: TTextAttributes;
    function GetWordWrap: Boolean;
    procedure SetCaretX(AValue: Integer);
    procedure SetCaretXY(AValue: TPoint);
    procedure SetCaretY(AValue: Integer);
    procedure SetFileName(AValue: string);
    procedure SetModified(AValue: Boolean);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetOnDropFiles(AValue: TDropFilesEvent);
    procedure SetOnEditingDone(AValue: TNotifyEvent);
    procedure SetOnSelectionChange(AValue: TNotifyEvent);
    procedure SetPopupMenu(AValue: TPopupMenu);
    procedure SetSelEnd(AValue: Integer);
    procedure SetSelStart(AValue: Integer);
    procedure SetSelText(AValue: string);
    procedure SetWordWrap(AValue: Boolean);
    {$ENDREGION}

  public
    procedure AfterConstruction; override;

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

{$REGION 'construction and destruction'}
procedure TRichEditorViewKMemo.AfterConstruction;
begin
  inherited AfterConstruction;
  FEditor := TKMemo.Create(Self);
  FActions := Owner as IRichEditorActions;
  FEditor.Parent         := pnlRichEditor;
  FEditor.BorderStyle    := bsNone;
  FEditor.ScrollBars     := ssAutoBoth;
  FEditor.Align          := alClient;
  FEditor.DoubleBuffered := True;

  FEditor.OnChange := EditorChange;
  FEditor.OnEditingDone  := EditorEditingDone;
  FEditor.OnSelectionChange := EditorSelectionChange;
  FEditor.OnUTF8KeyPress := UTF8KeyPress;
  FEditor.OnClick := EditorOnClick;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TRichEditorViewKMemo.GetActions: IRichEditorActions;
begin

end;


function TRichEditorViewKMemo.GetCanPaste: Boolean;
begin

end;

function TRichEditorViewKMemo.GetCanUndo: Boolean;
begin

end;

function TRichEditorViewKMemo.GetCaretX: Integer;
begin

end;

function TRichEditorViewKMemo.GetCaretXY: TPoint;
begin

end;

function TRichEditorViewKMemo.GetCaretY: Integer;
begin

end;

function TRichEditorViewKMemo.GetEditor: TRichMemo;
begin

end;

function TRichEditorViewKMemo.GetFileName: string;
begin

end;

function TRichEditorViewKMemo.GetForm: TCustomForm;
begin

end;

function TRichEditorViewKMemo.GetModified: Boolean;
begin

end;

function TRichEditorViewKMemo.GetOnChange: TNotifyEvent;
begin

end;

function TRichEditorViewKMemo.GetOnDropFiles: TDropFilesEvent;
begin

end;

function TRichEditorViewKMemo.GetOnEditingDone: TNotifyEvent;
begin

end;

function TRichEditorViewKMemo.GetOnSelectionChange: TNotifyEvent;
begin

end;

function TRichEditorViewKMemo.GetSelAvail: Boolean;
begin

end;

function TRichEditorViewKMemo.GetSelEnd: Integer;
begin

end;

function TRichEditorViewKMemo.GetSelStart: Integer;
begin

end;

function TRichEditorViewKMemo.GetSelText: string;
begin

end;

function TRichEditorViewKMemo.GetTextAttributes: TTextAttributes;
begin

end;

function TRichEditorViewKMemo.GetWordWrap: Boolean;
begin

end;

procedure TRichEditorViewKMemo.SetCaretX(AValue: Integer);
begin

end;

procedure TRichEditorViewKMemo.SetCaretXY(AValue: TPoint);
begin

end;

procedure TRichEditorViewKMemo.SetCaretY(AValue: Integer);
begin

end;

procedure TRichEditorViewKMemo.SetFileName(AValue: string);
begin

end;

procedure TRichEditorViewKMemo.SetModified(AValue: Boolean);
begin

end;

procedure TRichEditorViewKMemo.SetOnChange(AValue: TNotifyEvent);
begin

end;

procedure TRichEditorViewKMemo.SetOnDropFiles(AValue: TDropFilesEvent);
begin

end;

procedure TRichEditorViewKMemo.SetOnEditingDone(AValue: TNotifyEvent);
begin

end;

procedure TRichEditorViewKMemo.SetOnSelectionChange(AValue: TNotifyEvent);
begin

end;

procedure TRichEditorViewKMemo.SetPopupMenu(AValue: TPopupMenu);
begin

end;

procedure TRichEditorViewKMemo.SetSelEnd(AValue: Integer);
begin

end;

procedure TRichEditorViewKMemo.SetSelStart(AValue: Integer);
begin

end;

procedure TRichEditorViewKMemo.SetSelText(AValue: string);
begin

end;

procedure TRichEditorViewKMemo.SetWordWrap(AValue: Boolean);
begin

end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TRichEditorViewKMemo.EditorChange(Sender: TObject);
begin
  //
end;

procedure TRichEditorViewKMemo.EditorOnClick(Sender: TObject);
begin

end;

procedure TRichEditorViewKMemo.UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin

end;

{$ENDREGION}

end.

