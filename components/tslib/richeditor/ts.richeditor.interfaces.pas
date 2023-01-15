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

unit ts.RichEditor.Interfaces;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Menus, ActnList, Graphics,

  KMemo,

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
    function GetBackgroundColor: TColor;
    function GetBullets: Boolean;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetContentSize: Int64;
    function GetEditor: TKMemo;
    function GetFileName: string;
    function GetFont: TFont;
    function GetForm: TCustomForm;
    function GetIsEmpty: Boolean;
    function GetIsFile: Boolean;
    function GetIsInsideOfTable: Boolean;
    function GetModified: Boolean;
    function GetOnChange: TNotifyEvent;
    function GetOnDropFiles: TDropFilesEvent;
    function GetPopupMenu: TPopupMenu;
    function GetRTFText: string;
    function GetSelAvail: Boolean;
    function GetSelEnd: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    function GetShowSpecialChars: Boolean;
    function GetWordWrap: Boolean;
    procedure SetAlignCenter(AValue: Boolean);
    procedure SetAlignJustify(AValue: Boolean);
    procedure SetAlignLeft(AValue: Boolean);
    procedure SetAlignRight(AValue: Boolean);
    function GetText: string;
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetBullets(AValue: Boolean);
    procedure SetFileName(const AValue: string);
    procedure SetIsFile(AValue: Boolean);
    procedure SetModified(const AValue: Boolean);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TDropFilesEvent);
    procedure SetParent(NewParent: TWinControl);
    procedure SetPopupMenu(const AValue: TPopupMenu);
    procedure SetRTFText(AValue: string);
    procedure SetSelEnd(const AValue: Integer);
    procedure SetSelStart(const AValue: Integer);
    procedure SetSelText(const AValue: string);
    procedure SetShowSpecialChars(AValue: Boolean);
    procedure SetText(const AValue: string);
    procedure SetWordWrap(const AValue: Boolean);
    {$ENDREGION}

    function Focused: Boolean;
    procedure SetFocus;
    procedure SelectAll;

    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure Load(const AStorageName: string = '');
    procedure Save(const AStorageName: string = '');

    procedure ShowPreview;

    procedure BeginUpdate;
    procedure EndUpdate;

    function IsUpdating: Boolean;

    function InsertImage: Boolean; overload;
    procedure InsertImageFile(const AFileName: string);
    procedure InsertImage(AImage: TPicture); overload;
    procedure InsertHyperlink(
      const AText : string = '';
      const AURL  : string = ''
    );
    procedure EditSelectedItem;
    procedure EditParagraphStyle;
    procedure EditTextStyle;
    procedure CreateBulletList;
    procedure CreateNumberedList;
    procedure CreateTable(AColCount: Integer; ARowCount: Integer);
    procedure AddParagraph;

    procedure InsertRowBefore;
    procedure InsertRowAfter;
    procedure InsertColumnBefore;
    procedure InsertColumnAfter;
    procedure DeleteColumn;
    procedure DeleteRow;
    procedure SelectTable;

    procedure IncIndent;
    procedure DecIndent;

    procedure Clear;

    // clipboard commands
    procedure Cut;
    procedure Copy;
    procedure Paste;

    procedure Undo;
    procedure Redo;

    // properties
    property Bullets: Boolean
      read GetBullets write SetBullets;

    property Editor: TKMemo
      read GetEditor;

    property Form: TCustomForm
      read GetForm;

    property Actions: IRichEditorActions
      read GetActions;

    property BackgroundColor: TColor
      read GetBackgroundColor write SetBackgroundColor;

    property ContentSize: Int64
      read GetContentSize;

    property CanPaste: Boolean
      read GetCanPaste;

    property CanUndo: Boolean
      read GetCanUndo;

    property CanRedo: Boolean
      read GetCanRedo;

    { Returns True if the view doesn't contain any data. }
    property IsEmpty: Boolean
      read GetIsEmpty;

    property IsFile: Boolean
      read GetIsFile write SetIsFile;

    property IsInsideOfTable: Boolean
      read GetIsInsideOfTable;

    property RTFText: string
      read GetRTFText write SetRTFText;

    property Text: string
      read GetText write SetText;

    property SelText: string
      read GetSelText write SetSelText;

    property SelStart: Integer
      read GetSelStart write SetSelStart;

    property SelEnd: Integer
      read GetSelEnd write SetSelEnd;

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

    property WordWrap: Boolean
      read GetWordWrap write SetWordWrap;

    property ShowSpecialChars: Boolean
      read GetShowSpecialChars write SetShowSpecialChars;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

    property OnDropFiles: TDropFilesEvent
      read GetOnDropFiles write SetOnDropFiles;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;
  end;

  { Events dispatched by the editor view. }

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

