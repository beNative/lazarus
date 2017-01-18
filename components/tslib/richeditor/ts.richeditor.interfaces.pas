{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.RichEditor.Interfaces;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Menus, ActnList,

  RichMemo,

  ts.RichEditor.TextAttributes;

 { All supported actions by the editor views, and holds a collection of all
    registered views. }
type
  IRichEditorActions = interface;

  { IRichEditorView }

  IRichEditorView = interface
  ['{9F85A3C6-584D-497F-9C5C-7300D7AEF92E}']
    {%region /fold}
    // property access methods
    function GetActions: IRichEditorActions;
    function GetCanPaste: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretX: Integer;
    function GetCaretXY: TPoint;
    function GetCaretY: Integer;
    //function GetBlockBegin: TPoint;
    //function GetBlockEnd: TPoint;
    //function GetCurrentWord: string;
    function GetEditor: TRichMemo;
    function GetFileName: string;
    //function GetFindHistory: TStrings;
    //function GetFindReplaceDialog: TFindReplaceDialog;
    function GetForm: TCustomForm;
    //function GetLines: TStrings;
    //function GetLinesInWindow: Integer;
    //function GetLineText: string;
//    function GetLogicalCaretXY: TPoint;
    function GetModified: Boolean;
    function GetOnChange: TNotifyEvent;
    function GetOnDropFiles: TDropFilesEvent;
    function GetOnEditingDone: TNotifyEvent;
    function GetOnSelectionChange: TNotifyEvent;
//    function GetOnEditingDone: TNotifyEvent;
    //function GetOnStatusChange: TStatusChangeEvent;
    function GetPopupMenu: TPopupMenu;
    //function GetReplaceHistory: TStrings;
    //function GetSearchText: string;
    function GetSelAvail: Boolean;
    function GetSelEnd: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    function GetTextAttributes: TTextAttributes;
    function GetWordWrap: Boolean;
    procedure SetCaretX(const AValue: Integer);
    procedure SetCaretXY(const AValue: TPoint);
    procedure SetCaretY(const AValue: Integer);
    //function GetSettings: IEditorSettings;
    //function GetText: string;
    //function GetTextSize: Integer;
    //function GetTopLine: Integer;
    //procedure SetBlockBegin(const AValue: TPoint);
    //procedure SetBlockEnd(const AValue: TPoint);
    procedure SetFileName(const AValue: string);
    //procedure SetLines(const AValue: TStrings);
    //procedure SetLineText(const AValue: string);
    //procedure SetLogicalCaretXY(const AValue: TPoint);
    procedure SetModified(const AValue: Boolean);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TDropFilesEvent);
    procedure SetOnEditingDone(const AValue: TNotifyEvent);
    procedure SetOnSelectionChange(AValue: TNotifyEvent);
    //procedure SetOnEditingDone(const AValue: TNotifyEvent);
    //procedure SetParent(const AValue: TWinControl);
    procedure SetParent(NewParent: TWinControl);
    procedure SetPopupMenu(const AValue: TPopupMenu);
    //procedure SetPopupMenu(const AValue: TPopupMenu);
    //procedure SetSearchText(const AValue: string);
    procedure SetSelEnd(const AValue: Integer);
    procedure SetSelStart(const AValue: Integer);
    procedure SetSelText(const AValue: string);
    //procedure SetText(const AValue: string);
    procedure SetWordWrap(const AValue: Boolean);
    {%endregion}

    // methods
    function Focused: Boolean;
    procedure SetFocus;
    procedure SelectAll;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Clear;

    // clipboard commands
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;

    // properties
    property Editor: TRichMemo
      read GetEditor;

    property Form: TCustomForm
      read GetForm;

    property Actions: IRichEditorActions
      read GetActions;

    property CanPaste: Boolean
      read GetCanPaste;

    property CanUndo: Boolean
      read GetCanUndo;

    //property Lines: TStrings
    //  read GetLines write SetLines;
    //
    //property Text: string
    //  read GetText write SetText;
    //
    property SelText: string
      read GetSelText write SetSelText;

    property SelStart: Integer
      read GetSelStart write SetSelStart;

    property SelEnd: Integer
      read GetSelEnd write SetSelEnd;

    //property CurrentWord: string
    //  read GetCurrentWord;
    //
    { current X-coordinate of the caret. }
    property CaretX: Integer
      read GetCaretX write SetCaretX;

    { current Y-coordinate of the caret. }
    property CaretY: Integer
      read GetCaretY write SetCaretY;

    property CaretXY: TPoint
      read GetCaretXY write SetCaretXY;

    property FileName: string
      read GetFileName write SetFileName;

    property SelAvail: Boolean
      read GetSelAvail;

    property Modified: Boolean
      read GetModified write SetModified;

    property OnDropFiles: TDropFilesEvent
      read GetOnDropFiles write SetOnDropFiles;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;

    property OnEditingDone: TNotifyEvent
      read GetOnEditingDone write SetOnEditingDone;

    property OnSelectionChange: TNotifyEvent
      read GetOnSelectionChange write SetOnSelectionChange;

    property TextAttributes: TTextAttributes
      read GetTextAttributes;

    property WordWrap: Boolean
      read GetWordWrap write SetWordWrap;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;
  end;

 { Events dispatched by the editor view. }

  IRichEditorEvents = interface
  ['{D078C92D-16DF-4727-A18F-4C76E07D37A2}']
    // property access methods
    function GetOnChange: TNotifyEvent;
    //function GetOnNewFile: TNewFileEvent;
    //function GetOnOpenFile: TFileEvent;
    //function GetOnSaveFile: TFileEvent;
    procedure SetOnChange(const AValue: TNotifyEvent);
    //procedure SetOnNewFile(const AValue: TNewFileEvent);
    //procedure SetOnOpenFile(const AValue: TFileEvent);
    //procedure SetOnSaveFile(const AValue: TFileEvent);

    // event dispatch methods
    procedure DoChange;
    //procedure DoModified;
    //procedure DoSaveFile;
    //procedure DoOpenFile;
    //procedure DoNewFile(const AFileName: string = ''; const AText: string = '');
    //function DoFindAndReplace: Integer;

    // event handlers
    { triggered when caret position changes }
    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;

    //property OnOpenFile: TFileEvent
    //  read GetOnOpenFile write SetOnOpenFile;
    //
    //property OnNewFile: TNewFileEvent
    //  read GetOnNewFile write SetOnNewFile;
    //
    //property OnSaveFile: TFileEvent
    //  read GetOnSaveFile write SetOnSaveFile;
  end;

  IRichEditorActions = interface
  ['{E60C0187-4F9E-4585-B776-5B710B5498F9}']
    {$region 'property access methods' /fold}
    function GetActions: TActionList;
    function GetEditorPopupMenu: TPopupMenu;
    function GetItem(AName: string): TContainedAction;
    function GetActiveView: IRichEditorView;
    function GetViewByName(AName: string): IRichEditorView;
    procedure SetActiveView(const AValue: IRichEditorView);
    function GetView(AIndex: Integer): IRichEditorView;
    function GetViewCount: Integer;
    {$endregion}

    procedure UpdateActions;
    procedure OpenFileAtCursor;

    function SaveFile: Boolean;
    procedure LoadFile;

    function AddView(
      const AName: string = '';
      const AFileName: string = ''
    ): IRichEditorView;
    function DeleteView(AIndex: Integer): Boolean;
    procedure ClearViews;

    property ActiveView: IRichEditorView
      read GetActiveView write SetActiveView;

    property ViewCount: Integer
      read GetViewCount;

    property Items[AName: string]: TContainedAction
      read GetItem; default;

    property Actions: TActionList
      read GetActions;

    property EditorPopupMenu: TPopupMenu
      read GetEditorPopupMenu;

    property Views[AIndex: Integer]: IRichEditorView
      read GetView;

    property ViewByName[AName: string]: IRichEditorView
      read GetViewByName;
  end;

implementation

end.

