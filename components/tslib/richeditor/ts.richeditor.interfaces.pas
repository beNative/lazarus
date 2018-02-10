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

unit ts.RichEditor.Interfaces;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Menus, ActnList, Graphics,

  ts.RichEditor.Types;

 { All supported actions by the editor views, and holds a collection of all
    registered views. }
type
  IRichEditorActions = interface;

  { IRichEditorView }

  IRichEditorView = interface
  ['{9F85A3C6-584D-497F-9C5C-7300D7AEF92E}']
    {$REGION 'property access methods'}
    function GetActions: IRichEditorActions;
    function GetAlignCenter: Boolean;
    function GetAlignJustify: Boolean;
    function GetAlignLeft: Boolean;
    function GetAlignRight: Boolean;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    //function GetCurrentWord: string;
    function GetEditor: TComponent;
    function GetFileName: string;
    function GetFont: TFont;
    function GetForm: TCustomForm;
    //function GetLinesInWindow: Integer;
    //function GetLineText: string;
    function GetModified: Boolean;
    function GetOnChange: TNotifyEvent;
    function GetOnDropFiles: TDropFilesEvent;
    function GetPopupMenu: TPopupMenu;
    function GetSelAvail: Boolean;
    function GetSelEnd: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    function GetWordWrap: Boolean;
    procedure SetAlignCenter(AValue: Boolean);
    procedure SetAlignJustify(AValue: Boolean);
    procedure SetAlignLeft(AValue: Boolean);
    procedure SetAlignRight(AValue: Boolean);
    //function GetSettings: IEditorSettings;
    function GetText: string;
    //function GetTextSize: Integer;
    //function GetTopLine: Integer;
    procedure SetFileName(const AValue: string);
    //procedure SetLineText(const AValue: string);

    procedure SetModified(const AValue: Boolean);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TDropFilesEvent);
    procedure SetParent(NewParent: TWinControl);
    procedure SetPopupMenu(const AValue: TPopupMenu);
    procedure SetSelEnd(const AValue: Integer);
    procedure SetSelStart(const AValue: Integer);
    procedure SetSelText(const AValue: string);
    procedure SetText(const AValue: string);
    procedure SetWordWrap(const AValue: Boolean);
    //function GetAlignment: TParaAlignment;
    //function GetBkColor: TColor;
    //function GetColor: TColor;
    //function GetHasBkColor: Boolean;
    //function GetNumberingStyle: TParaNumStyle;
    //procedure SetAlignment(AValue: TParaAlignment);
    //procedure SetBkColor(AValue: TColor);
    //procedure SetColor(const AValue: TColor);
    //procedure SetHasBkColor(AValue: Boolean);
    //procedure SetNumberingStyle(AValue: TParaNumStyle);
    {$ENDREGION}

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
    function IsUpdating: Boolean;
    function InsertImage: Boolean;
    procedure InsertHyperlink;
    procedure Clear;

    // clipboard commands
    procedure Cut;
    procedure Copy;
    procedure Paste;

    procedure Undo;
    procedure Redo;

    // properties
    property Editor: TComponent
      read GetEditor;

    property Form: TCustomForm
      read GetForm;

    property Actions: IRichEditorActions
      read GetActions;

    property CanPaste: Boolean
      read GetCanPaste;

    property CanUndo: Boolean
      read GetCanUndo;

    property CanRedo: Boolean
      read GetCanRedo;

    property Text: string
      read GetText write SetText;

    property SelText: string
      read GetSelText write SetSelText;

    property SelStart: Integer
      read GetSelStart write SetSelStart;

    property SelEnd: Integer
      read GetSelEnd write SetSelEnd;

    //property CurrentWord: string
    //  read GetCurrentWord;
    //
    property FileName: string
      read GetFileName write SetFileName;

    property Font: TFont
      read GetFont;

    property AlignLeft: Boolean
      read GetAlignLeft write SetAlignLeft;

    property AlignRight: Boolean
      read GetAlignRight write SetAlignRight;

    property AlignCenter: Boolean
      read GetAlignCenter write SetAlignCenter;

    property AlignJustify: Boolean
      read GetAlignJustify write SetAlignJustify;

    property SelAvail: Boolean
      read GetSelAvail;

    property Modified: Boolean
      read GetModified write SetModified;

    property OnDropFiles: TDropFilesEvent
      read GetOnDropFiles write SetOnDropFiles;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;

    property WordWrap: Boolean
      read GetWordWrap write SetWordWrap;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;
  end;

 { Events dispatched by the editor view. }

  { IRichEditorEvents }

  IRichEditorEvents = interface
  ['{D078C92D-16DF-4727-A18F-4C76E07D37A2}']
    {$REGION 'property access mehods'}
    function GetOnAfterSave: TStorageEvent;
    function GetOnBeforeSave: TStorageEvent;
    function GetOnChange: TNotifyEvent;
    function GetOnLoad: TStorageEvent;
    function GetOnNew: TNewEvent;
    function GetOnOpen: TStorageEvent;
    procedure SetOnAfterSave(AValue: TStorageEvent);
    procedure SetOnBeforeSave(AValue: TStorageEvent);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetOnLoad(AValue: TStorageEvent);
    procedure SetOnNew(AValue: TNewEvent);
    procedure SetOnOpen(AValue: TStorageEvent);
    {$ENDREGION}

    // event dispatch methods
    procedure DoChange;
    procedure DoOpen(const AName: string);
    procedure DoBeforeSave(const AName: string);
    procedure DoAfterSave(const AName: string);
    procedure DoLoad(const AName: string);
    procedure DoNew(
      const AName : string = '';
      const AText : string = ''
    );

    { triggered when caret position changes }
    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;

    property OnLoad: TStorageEvent
      read GetOnLoad write SetOnLoad;

    property OnNew: TNewEvent
      read GetOnNew write SetOnNew;

    property OnOpen: TStorageEvent
      read GetOnOpen write SetOnOpen;

    property OnBeforeSave: TStorageEvent
      read GetOnBeforeSave write SetOnBeforeSave;

    property OnAfterSave: TStorageEvent
      read GetOnAfterSave write SetOnAfterSave;
  end;

  IRichEditorActions = interface
  ['{E60C0187-4F9E-4585-B776-5B710B5498F9}']
    {$REGION 'property access methods'}
    function GetActions: TActionList;
    function GetItem(AName: string): TContainedAction;
    {$ENDREGION}

    procedure UpdateActions;

    property Items[AName: string]: TContainedAction
      read GetItem; default;

    property Actions: TActionList
      read GetActions;
  end;

  IRichEditorManager = interface
  ['{A1781DE6-B022-4DBA-9D06-327E4612F65A}']
    {$REGION 'property access methods'}
    function GetEditorPopupMenu: TPopupMenu;
    function GetActiveView: IRichEditorView;
    function GetViewByName(AName: string): IRichEditorView;
    procedure SetActiveView(const AValue: IRichEditorView);
    function GetView(AIndex: Integer): IRichEditorView;
    function GetViewCount: Integer;
    {$ENDREGION}

    function AddView(
      const AName     : string = '';
      const AFileName : string = ''
    ): IRichEditorView;
    function DeleteView(AIndex: Integer): Boolean;
    procedure ClearViews;

    property ActiveView: IRichEditorView
      read GetActiveView write SetActiveView;

    property EditorPopupMenu: TPopupMenu
      read GetEditorPopupMenu;

    property Views[AIndex: Integer]: IRichEditorView
      read GetView;

    property ViewByName[AName: string]: IRichEditorView
      read GetViewByName;

    property ViewCount: Integer
      read GetViewCount;
  end;

  IRichEditorToolbarsFactory = interface
  ['{0C183975-86DE-4013-8D05-70879A07E775}']
    function CreateMainToolbar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolbar;
  end;

  IRichEditorMenusFactory = interface
  ['{13671A4F-9330-4A0A-B277-B052356DFE12}']
    function CreateMainMenu(
      AOwner : TComponent
    ): TMainMenu;
  end;

implementation

end.

