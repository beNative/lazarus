{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.Interfaces;

{$MODE Delphi}

interface

uses
  Classes, ActnList, Controls, Forms, Menus, Contnrs, Graphics,

  LCLType,

  SynEdit, SynEditTypes, SynMacroRecorder, SynEditHighlighter,
  SynEditMiscClasses, SynEditMarkupBracket,

  ts.Core.FormSettings,

  ts.Editor.Settings.AlignLines, ts.Editor.Settings.SearchEngine,
  ts.Editor.Settings.CodeShaper,

  ts.Editor.Highlighters, ts.Editor.HighlighterAttributes;

type
  // forward declarations
  IEditorView                   = interface;
  IEditorToolView               = interface;
  IEditorActions                = interface;
  IEditorSettings               = interface;
  IEditorSelection              = interface;
  TEditorViewListEnumerator     = class;
  TEditorToolViewListEnumerator = class;

  // event types
  TCaretPositionEvent = procedure(
    Sender : TObject;
    X, Y   : Integer
  ) of object;

  TStatusMessageEvent = procedure(
    Sender : TObject;
    Text   : string
  ) of object;

  TMacroStateChangeEvent = procedure(
    Sender: TObject;
    AState: TSynMacroState
  ) of object;

  TFileEvent = procedure(
        Sender    : TObject;
    var AFileName : string
  ) of object;

  TNewFileEvent = procedure(
          Sender    : TObject;
    var   AFileName : string;
    const AText     : string
  ) of object;

  TAddEditorViewEvent = procedure(
    Sender      : TObject;
    AEditorView : IEditorView
  ) of object;

  TEditorToolViewEvent = procedure(
    Sender          : TObject;
    AEditorToolView : IEditorToolView
  ) of object;

  TOpenOtherInstanceEvent = procedure(
          Sender  : TObject;
    const AParams : array of string
  ) of object;

// type aliases
  TEditorViewList     = TInterfaceList;
  //TEditorToolViewList = TInterfaceList;

  IControl = interface
  ['{303F3DE1-81F5-473B-812B-7DD4C306725B}']
    function GetName: string;
    function GetParent: TWinControl;
    function GetPopupMenu: TPopupMenu;
    procedure SetName(AValue: string);
    procedure SetParent(AValue: TWinControl);
    procedure SetPopupMenu(AValue: TPopupMenu);

    function Focused: Boolean;
    procedure SetFocus;

    property Parent: TWinControl
      read GetParent write SetParent;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

    property Name: string
      read GetName write SetName;
  end;

  { Handles display view of the editor. }

  IEditorView = interface(IControl)
  ['{94689213-B046-45F6-922B-FAE91C02A3FF}']
    {$region 'property access methods' /fold}
    function GetActions: IEditorActions;
    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretX: Integer;
    function GetCaretXY: TPoint;
    function GetCaretY: Integer;
    function GetCurrentWord: string;
    function GetEditor: TSynEdit;
    function GetEditorFont: TFont;
    function GetEncoding: string;
    function GetFileName: string;
    function GetFindHistory: TStrings;
    function GetFoldLevel: Integer;
    function GetFoldState: string;
    function GetForm: TCustomForm;
    function GetHighlighterItem: THighlighterItem;
    function GetInsertMode: Boolean;
    function GetLineBreakStyle: string;
    function GetLines: TStrings;
    function GetLinesInWindow: Integer;
    function GetLineText: string;
    function GetLogicalCaretXY: TPoint;
    function GetMasterView: IEditorView;
    function GetModified: Boolean;
    function GetMonitorChanges: Boolean;
    function GetOnChange: TNotifyEvent;
    function GetOnDropFiles: TDropFilesEvent;
    function GetOnStatusChange: TStatusChangeEvent;
    function GetPreviewText: string;
    function GetReplaceHistory: TStrings;
    function GetSearchOptions: TSynSearchOptions;
    function GetSearchText: string;
    function GetSelAvail: Boolean;
    function GetSelection: IEditorSelection;
    function GetSelectionMode: TSynSelectionMode;
    function GetSelEnd: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    function GetSettings: IEditorSettings;
    function GetShowSpecialChars: Boolean;
    function GetSlaveView: IEditorView;
    function GetSupportsFolding: Boolean;
    function GetText: string;
    function GetTextSize: Integer;
    function GetTopLine: Integer;
    procedure SetBlockBegin(const AValue: TPoint);
    procedure SetBlockEnd(const AValue: TPoint);
    procedure SetCaretX(const AValue: Integer);
    procedure SetCaretXY(const AValue: TPoint);
    procedure SetCaretY(const AValue: Integer);
    procedure SetEditorFont(AValue: TFont);
    procedure SetEncoding(const AValue: string);
    procedure SetFileName(const AValue: string);
    procedure SetFoldLevel(const AValue: Integer);
    procedure SetFoldState(const AValue: string);
    procedure SetHighlighterItem(const AValue: THighlighterItem);
    procedure SetInsertMode(AValue: Boolean);
    procedure SetLineBreakStyle(const AValue: string);
    procedure SetLines(const AValue: TStrings);
    procedure SetLineText(const AValue: string);
    procedure SetLogicalCaretXY(const AValue: TPoint);
    procedure SetMasterView(AValue: IEditorView);
    procedure SetModified(const AValue: Boolean);
    procedure SetMonitorChanges(const AValue: Boolean);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TDropFilesEvent);
    procedure SetOnStatusChange(const AValue: TStatusChangeEvent);
    procedure SetSearchOptions(AValue: TSynSearchOptions);
    procedure SetSearchText(const AValue: string);
    procedure SetSelectionMode(AValue: TSynSelectionMode);
    procedure SetSelEnd(const AValue: Integer);
    procedure SetSelStart(const AValue: Integer);
    procedure SetSelText(const AValue: string);
    procedure SetShowSpecialChars(const AValue: Boolean);
    procedure SetSlaveView(AValue: IEditorView);
    procedure SetText(const AValue: string);
    procedure SetTopLine(const AValue: Integer);
    {$endregion}

    // information retrieval
    function GetWordAtPosition(APosition: TPoint): string;
    function GetWordFromCaret(const ACaretPos: TPoint): string;
    function GetHighlighterAttriAtRowCol(
          APosition : TPoint;
      out AToken    : string;
      out AAttri    : TSynHighlighterAttributes
    ): Boolean;

    // lock updates
    procedure BeginUpdate;
    procedure EndUpdate;

    // make current view the active one if more than one view is managed.
    procedure Activate;

    // configuration
    procedure AssignHighlighter(const AHighlighter: string = 'TXT');

    procedure SetHighlightSearch(
      const ASearch  : string;
            AOptions : TSynSearchOptions
    );

    // commands   (IEditorCommands)
    procedure Clear;
    procedure AdjustFontSize(AOffset: Integer);
    procedure UpdateCommentSelection(ACommentOn, AToggle: Boolean);
    procedure ToggleBlockCommentSelection;
    procedure InsertTextAtCaret(const AText: string);
    procedure Close;
    procedure AlignSelection(
      const AToken                  : string;
            ACompressWS             : Boolean;
            AInsertSpaceBeforeToken : Boolean;
            AInsertSpaceAfterToken  : Boolean;
            AAlignInParagraphs      : Boolean
    );
    procedure StripMarkupFromSelection;
    procedure StripCharsFromSelection(
      AFirst : Boolean;
      ALast  : Boolean
    );
    procedure PascalStringFromSelection;
    procedure QuoteLinesInSelection(ADelimit : Boolean = False);
    procedure DequoteLinesInSelection;
    procedure Base64FromSelection(ADecode: Boolean = False);
    procedure ConvertTabsToSpacesInSelection;

    // search
    procedure SearchAndSelectLine(ALineIndex: Integer; const ALine: string);
    procedure SearchAndSelectText(const AText: string);
    procedure ClearHighlightSearch;

    // load and save
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);

    // selection
    procedure SelectAll;
    procedure SelectWord;
    procedure UpperCaseSelection;
    procedure LowerCaseSelection;
    procedure SmartSelect;
    function SelectBlockAroundCursor(const AStartTag, AEndTag: string;
      AIncludeStartTag, AIncludeEndTag: Boolean): Boolean;

    // clipboard commands
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;
    procedure Redo;

    // properties
    property FindHistory: TStrings
      read GetFindHistory;

    property ReplaceHistory: TStrings
      read GetReplaceHistory;

    property Editor: TSynEdit
      read GetEditor;

    property Form: TCustomForm
      read GetForm;

//-----------<TEMP>-------------------------------------------------------BEGIN
    { Master view from which the text buffer is shared. }
    property MasterView: IEditorView
      read GetMasterView write SetMasterView;

    { Slave view which shares the text buffer. }
    property SlaveView: IEditorView
      read GetSlaveView write SetSlaveView;
//-----------<TEMP>---------------------------------------------------------END

    { Settings shared by all edit instances managed by IEditorActions instance. }
    property Settings: IEditorSettings
      read GetSettings;

    { Reference to the main 'manager' instance }
    property Actions: IEditorActions
      read GetActions;

    // Selection properties
    property BlockBegin: TPoint
      read GetBlockBegin write SetBlockBegin;

    property BlockEnd: TPoint
      read GetBlockEnd write SetBlockEnd;

    property SelText: string
      read GetSelText write SetSelText;

    property SelStart: Integer
      read GetSelStart write SetSelStart;

    property SelEnd: Integer
      read GetSelEnd write SetSelEnd;

    property InsertMode: Boolean
      read GetInsertMode write SetInsertMode;

    property Selection: IEditorSelection
      read GetSelection;

    { Sets the current (default) selection mode.  }
    property SelectionMode: TSynSelectionMode
      read GetSelectionMode write SetSelectionMode;

    property SelAvail: Boolean
      read GetSelAvail;

    property SearchText: string
      read GetSearchText write SetSearchText;

    property SearchOptions: TSynSearchOptions
      read GetSearchOptions write SetSearchOptions;

    // Status information (readonly)
    property CanPaste: Boolean
      read GetCanPaste;

    property CanRedo: Boolean
      read GetCanRedo;

    property CanUndo: Boolean
      read GetCanUndo;

    property TextSize: Integer
      read GetTextSize;

    property LinesInWindow: Integer
      read GetLinesInWindow;

    property SupportsFolding: Boolean
      read GetSupportsFolding;

    property CurrentWord: string
      read GetCurrentWord;
    //------------------------------------------
    property Lines: TStrings
      read GetLines write SetLines;

    property Text: string
      read GetText write SetText;

    //---| Cursor properties |-------------------------------------------------
    { current X-coordinate of the caret. }
    property CaretX: Integer
      read GetCaretX write SetCaretX;

    { current Y-coordinate of the caret. }
    property CaretY: Integer
      read GetCaretY write SetCaretY;

    { TPoint representation of current caret position }
    property CaretXY: TPoint
      read GetCaretXY write SetCaretXY;

    property LogicalCaretXY: TPoint
      read GetLogicalCaretXY write SetLogicalCaretXY;

    //---| Folding support |---------------------------------------------------
    { String identifying the foldstate of current instance }
    property FoldState: string
      read GetFoldState write SetFoldState;

    property FoldLevel: Integer
      read GetFoldLevel write SetFoldLevel;

    property FileName: string
      read GetFileName write SetFileName;

    property LineText: string
      read GetLineText write SetLineText;

    //---| Properties |--------------------------------------------------------

    property PreviewText: string
      read GetPreviewText;

    property MonitorChanges: Boolean
      read GetMonitorChanges write SetMonitorChanges;

    property ShowSpecialChars: Boolean
      read GetShowSpecialChars write SetShowSpecialChars;

    property TopLine: Integer
      read GetTopLine write SetTopLine;

    property Modified: Boolean
      read GetModified write SetModified;

    property Encoding: string
      read GetEncoding write SetEncoding;

    property LineBreakStyle: string
      read GetLineBreakStyle write SetLineBreakStyle;

    property HighlighterItem: THighlighterItem
      read GetHighlighterItem write SetHighlighterItem;

    property EditorFont: TFont
      read GetEditorFont write SetEditorFont;

    // events
    property OnDropFiles: TDropFilesEvent
      read GetOnDropFiles write SetOnDropFiles;

    property OnStatusChange: TStatusChangeEvent
      read GetOnStatusChange write SetOnStatusChange;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;
  end;

  { IEditorSelection }

  IEditorSelection = interface
  ['{DEBBB1D5-A04A-4264-96E9-0693E20C2A0D}']
    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetCaretXY: TPoint;
    function GetLines: TStrings;
    function GetSelectionMode: TSynSelectionMode;
    function GetText: string;
    procedure SetBlockBegin(AValue: TPoint);
    procedure SetBlockEnd(AValue: TPoint);
    procedure SetCaretXY(AValue: TPoint);
    procedure SetSelectionMode(AValue: TSynSelectionMode);
    procedure SetText(AValue: string);

    procedure Clear;

    procedure Store(
      ALockUpdates           : Boolean = True;
      AAutoExcludeEmptyLines : Boolean = False
    );
    procedure Restore;
    procedure Ignore;

    property BlockBegin: TPoint
      read GetBlockBegin write SetBlockBegin;

    property BlockEnd: TPoint
      read GetBlockEnd write SetBlockEnd;

    property CaretXY: TPoint
      read GetCaretXY write SetCaretXY;

    property SelectionMode: TSynSelectionMode
      read GetSelectionMode write SetSelectionMode;

    property Lines: TStrings
      read GetLines;

    property Text: string
      read GetText write SetText;
  end;

  IEditorSearchEngine = interface
  ['{5403336C-3E81-4A1B-B2BB-170CF0EF0B84}']
    function GetCurrentIndex: Integer;
    function GetItemList: TObjectList;
    function GetOptions: TSynSearchOptions;
    function GetReplaceText: string;
    function GetSearchAllViews: Boolean;
    function GetSearchText: string;
    procedure SetCurrentIndex(AValue: Integer);
    procedure SetOptions(AValue: TSynSearchOptions);
    procedure SetReplaceText(AValue: string);
    procedure SetSearchAllViews(AValue: Boolean);
    procedure SetSearchText(AValue: string);
    procedure Execute;
    procedure Replace;
    procedure ReplaceAll;
    procedure FindNext;
    procedure FindPrevious;

    property CurrentIndex: Integer
      read GetCurrentIndex write SetCurrentIndex;

    property Options: TSynSearchOptions
      read GetOptions write SetOptions;

    property SearchText : string
      read GetSearchText write SetSearchText;

    property ReplaceText: string
      read GetReplaceText write SetReplaceText;

    property SearchAllViews: Boolean
      read GetSearchAllViews write SetSearchAllViews;

    property ItemList: TObjectList
      read GetItemList;
  end;

  IEditorEvents = interface
  ['{D078C92D-16DF-4727-A18F-4C76E07D37A2}']
    {$region 'property access methods' /fold}
    function GetOnAddEditorView: TAddEditorViewEvent;
    function GetOnHideEditorToolView: TEditorToolViewEvent;
    function GetOnOpenOtherInstance: TOpenOtherInstanceEvent;
    function GetOnShowEditorToolView: TEditorToolViewEvent;
    procedure SetOnAddEditorView(AValue: TAddEditorViewEvent);
    procedure SetOnHideEditorToolView(AValue: TEditorToolViewEvent);
    procedure SetOnMacroStateChange(const AValue: TMacroStateChangeEvent);
    function GetOnMacroStateChange: TMacroStateChangeEvent;
    function GetOnCaretPositionChange: TCaretPositionEvent;
    function GetOnNewFile: TNewFileEvent;
    function GetOnOpenFile: TFileEvent;
    function GetOnSaveFile: TFileEvent;
    function GetOnStatusChange: TStatusChangeEvent;
    procedure SetOnCaretPositionChange(const AValue: TCaretPositionEvent);
    procedure SetOnNewFile(const AValue: TNewFileEvent);
    procedure SetOnOpenFile(const AValue: TFileEvent);
    procedure SetOnOpenOtherInstance(AValue: TOpenOtherInstanceEvent);
    procedure SetOnSaveFile(const AValue: TFileEvent);
    procedure SetOnShowEditorToolView(AValue: TEditorToolViewEvent);
    procedure SetOnStatusChange(const AValue: TStatusChangeEvent);
    {$endregion}

    // event dispatch methods
    procedure DoCaretPositionChange;
    procedure DoActiveViewChange;
    procedure DoMacroStateChange(AState : TSynMacroState);
    procedure DoOpenOtherInstance(const AParams: array of string);
    procedure DoAddEditorView(AEditorView: IEditorView);
    procedure DoStatusMessage(AText: string);
    procedure DoStatusChange(AChanges: TSynStatusChanges);
    procedure DoShowToolView(AToolView: IEditorToolView);
    procedure DoHideToolView(AToolView: IEditorToolView);
    procedure DoChange;
    procedure DoModified;
    procedure DoSaveFile(const AFileName: string);
    procedure DoOpenFile(const AFileName: string);
    procedure DoNewFile(
      const AFileName : string = '';
      const AText     : string = ''
    );

    procedure AddOnChangeHandler(AEvent: TNotifyEvent);
    procedure AddOnModifiedHandler(AEvent: TNotifyEvent);
    procedure AddOnActiveViewChangeHandler(AEvent: TNotifyEvent);

    procedure RemoveOnChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveOnModifiedHandler(AEvent: TNotifyEvent);
    procedure RemoveOnActiveViewChangeHandler(AEvent: TNotifyEvent);

    // events
    //property OnActiveViewChange: TNotifyEvent
    //  read GetOnActiveViewChange write SetOnActiveViewChange;

    property OnAddEditorView: TAddEditorViewEvent
      read GetOnAddEditorView write SetOnAddEditorView;

    property OnShowEditorToolView: TEditorToolViewEvent
      read GetOnShowEditorToolView write SetOnShowEditorToolView;

    property OnHideEditorToolView: TEditorToolViewEvent
      read GetOnHideEditorToolView write SetOnHideEditorToolView;

    { triggered when caret position changes }
    property OnCaretPositionChange: TCaretPositionEvent
      read GetOnCaretPositionChange write SetOnCaretPositionChange;

    property OnStatusChange: TStatusChangeEvent
      read GetOnStatusChange write SetOnStatusChange;

    //property OnChange: TNotifyEvent
    //  read GetOnChange write SetOnChange;

    property OnMacroStateChange: TMacroStateChangeEvent
      read GetOnMacroStateChange write SetOnMacroStateChange;

    property OnOpenFile: TFileEvent
      read GetOnOpenFile write SetOnOpenFile;

    property OnNewFile: TNewFileEvent
      read GetOnNewFile write SetOnNewFile;

    property OnSaveFile: TFileEvent
      read GetOnSaveFile write SetOnSaveFile;

    property OnOpenOtherInstance: TOpenOtherInstanceEvent
      read GetOnOpenOtherInstance write SetOnOpenOtherInstance;
  end;

  { Settings we should store if we would like to restore the editor to its
    current state }

  IEditorSettings = interface
  ['{CDB18A45-54AA-49F2-82C7-15D68C952197}']
    {$region 'property access methods' /fold}
    function GetAlignLinesSettings: TAlignLinesSettings;
    function GetAutoFormatXML: Boolean;
    function GetAutoGuessHighlighterType: Boolean;
    function GetBlockIndent: Integer;
    function GetBlockTabIndent: Integer;
    function GetBracketHighlightStyle: TSynEditBracketHighlightStyle;
    function GetBracketMatchColor: TSynSelectedColor;
    function GetCloseWithESC: Boolean;
    function GetCodeShaperSettings: TCodeShaperSettings;
    function GetDebugMode: Boolean;
    function GetDimInactiveView: Boolean;
    function GetEditorFont: TFont;
    function GetExtraCharSpacing: Integer;
    function GetExtraLineSpacing: Integer;
    function GetFileName: string;
    function GetFoldedCodeColor: TSynSelectedColor;
    function GetFormSettings: TFormSettings;
    function GetHighlightAllColor: TSynSelectedColor;
    function GetHighlighterAttributes: THighlighterAttributes;
    function GetHighlighters: THighlighters;
    function GetHighlighterType: string;
    function GetIncrementColor: TSynSelectedColor;
    function GetLineHighlightColor: TSynSelectedColor;
    function GetMouseLinkColor: TSynSelectedColor;
    function GetPreviewVisible: Boolean;
    function GetReadOnly: Boolean;
    function GetRightEdge: Integer;
    function GetRightEdgeColor: TColor;
    function GetSearchEngineSettings: TSearchEngineSettings;
    function GetSelectedColor: TSynSelectedColor;
    function GetShowSpecialCharacters: Boolean;
    function GetSingleInstance: Boolean;
    function GetTabWidth: Integer;
    function GetWantTabs: Boolean;
    function GetXML: string;
    procedure SetAlignLinesSettings(AValue: TAlignLinesSettings);
    procedure SetAutoFormatXML(const AValue: Boolean);
    procedure SetAutoGuessHighlighterType(const AValue: Boolean);
    procedure SetBlockIndent(AValue: Integer);
    procedure SetBlockTabIndent(AValue: Integer);
    procedure SetBracketHighlightStyle(AValue: TSynEditBracketHighlightStyle);
    procedure SetBracketMatchColor(AValue: TSynSelectedColor);
    procedure SetCloseWithESC(const AValue: Boolean);
    procedure SetCodeShaperSettings(AValue: TCodeShaperSettings);
    procedure SetDebugMode(AValue: Boolean);
    procedure SetDimInactiveView(const AValue: Boolean);
    procedure SetEditorFont(AValue: TFont);
    procedure SetExtraCharSpacing(AValue: Integer);
    procedure SetExtraLineSpacing(AValue: Integer);
    procedure SetFileName(const AValue: string);
    procedure SetFoldedCodeColor(AValue: TSynSelectedColor);
    procedure SetFormSettings(const AValue: TFormSettings);
    procedure SetHighlightAllColor(AValue: TSynSelectedColor);
    procedure SetHighlighterAttributes(AValue: THighlighterAttributes);
    procedure SetHighlighterType(const AValue: string);
    procedure SetIncrementColor(AValue: TSynSelectedColor);
    procedure SetLineHighlightColor(AValue: TSynSelectedColor);
    procedure SetMouseLinkColor(AValue: TSynSelectedColor);
    procedure SetPreviewVisible(const AValue: Boolean);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetRightEdge(AValue: Integer);
    procedure SetRightEdgeColor(AValue: TColor);
    procedure SetSearchEngineSettings(AValue: TSearchEngineSettings);
    procedure SetSelectedColor(AValue: TSynSelectedColor);
    procedure SetShowSpecialCharacters(const AValue: Boolean);
    procedure SetSingleInstance(AValue: Boolean);
    procedure SetTabWidth(AValue: Integer);
    procedure SetWantTabs(AValue: Boolean);
    {$endregion}

    procedure Load;
    procedure Save;
    procedure Apply;

    procedure AddEditorSettingsChangedHandler(AEvent: TNotifyEvent);
    procedure RemoveEditorSettingsChangedHandler(AEvent: TNotifyEvent);

    property FileName: string
      read GetFileName write SetFileName;

    property EditorFont: TFont
      read GetEditorFont write SetEditorFont;

    property DimInactiveView: Boolean
      read GetDimInactiveView write SetDimInactiveView;

    property HighlighterType: string
      read GetHighlighterType write SetHighlighterType;

    property ReadOnly: Boolean
      read GetReadOnly write SetReadOnly;

    property PreviewVisible: Boolean
      read GetPreviewVisible write SetPreviewVisible;

    property ShowSpecialCharacters: Boolean
      read GetShowSpecialCharacters write SetShowSpecialCharacters;

    property AutoFormatXML: Boolean
      read GetAutoFormatXML write SetAutoFormatXML;

    property CloseWithESC: Boolean
      read GetCloseWithESC write SetCloseWithESC;

    property AutoGuessHighlighterType: Boolean
      read GetAutoGuessHighlighterType write SetAutoGuessHighlighterType;

    property Highlighters: THighlighters
      read GetHighlighters;

    property FormSettings: TFormSettings
      read GetFormSettings write SetFormSettings;

    property AlignLinesSettings: TAlignLinesSettings
      read GetAlignLinesSettings write SetAlignLinesSettings;

    property HighlighterAttributes: THighlighterAttributes
      read GetHighlighterAttributes write SetHighlighterAttributes;

    property SearchEngineSettings: TSearchEngineSettings
      read GetSearchEngineSettings write SetSearchEngineSettings;

    property CodeShaperSettings: TCodeShaperSettings
      read GetCodeShaperSettings write SetCodeShaperSettings;

    property DebugMode: Boolean
      read GetDebugMode write SetDebugMode;

    property SingleInstance: Boolean
      read GetSingleInstance write SetSingleInstance;

    property XML: string
      read GetXML;

    // Colors
    property IncrementColor: TSynSelectedColor
      read GetIncrementColor write SetIncrementColor;

    property HighlightAllColor: TSynSelectedColor
      read GetHighlightAllColor write SetHighlightAllColor;

    property BracketMatchColor: TSynSelectedColor
      read GetBracketMatchColor write SetBracketMatchColor;

    property MouseLinkColor: TSynSelectedColor
      read GetMouseLinkColor write SetMouseLinkColor;

    property LineHighlightColor: TSynSelectedColor
      read GetLineHighlightColor write SetLineHighlightColor;

    property FoldedCodeColor: TSynSelectedColor
      read GetFoldedCodeColor write SetFoldedCodeColor;

    property RightEdgeColor: TColor
      read GetRightEdgeColor write SetRightEdgeColor;

    property RightEdge: Integer
      read GetRightEdge write SetRightEdge;

    property BracketHighlightStyle: TSynEditBracketHighlightStyle
      read GetBracketHighlightStyle write SetBracketHighlightStyle;

    property SelectedColor: TSynSelectedColor
      read GetSelectedColor write SetSelectedColor;

    property TabWidth: Integer
      read GetTabWidth write SetTabWidth;

    property WantTabs: Boolean
      read GetWantTabs write SetWantTabs;

    property BlockIndent: Integer
      read GetBlockIndent write SetBlockIndent;

    property BlockTabIndent: Integer
      read GetBlockTabIndent write SetBlockTabIndent;

    property ExtraCharSpacing: Integer
      read GetExtraCharSpacing write SetExtraCharSpacing;

    property ExtraLineSpacing: Integer
      read GetExtraLineSpacing write SetExtraLineSpacing;
  end;

  IEditorViews = interface
  ['{FBFB8DC6-7663-4EA4-935D-5B9F3CD7C753}']
    function GetView(AIndex: Integer): IEditorView;
    function GetViewByFileName(AFileName: string): IEditorView;
    function GetViewByName(AName: string): IEditorView;
    function GetCount: Integer;
    function GetViewList: TEditorViewList;

    function Add(
      const AName        : string = '';
      const AFileName    : string = '';
      const AHighlighter : string = ''
    ): IEditorView;
    function AddSharedView(
            AEditorView : IEditorView;
      const AName       : string = ''
    ): IEditorView;

    function Delete(AIndex: Integer): Boolean; overload;
    function Delete(AView: IEditorView): Boolean; overload;
    function Delete(const AName: string): Boolean; overload;
    procedure Clear(AExceptActive: Boolean = False);

    function GetEnumerator: TEditorViewListEnumerator;

    property Views[AIndex: Integer]: IEditorView
      read GetView; default;

    property ViewByName[AName: string]: IEditorView
      read GetViewByName;

    property ViewByFileName[AFileName: string]: IEditorView
      read GetViewByFileName;

    property ViewList: TEditorViewList
      read GetViewList;

    property Count: Integer
      read GetCount;
  end;

  IEditorToolView = interface
  ['{F6BEE8F6-BA4D-4B38-8FB0-79088B615DF5}']
    function GetForm: TForm;
    function GetName: string;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    { Lets the view respond to changes. }
    procedure UpdateView;

    //procedure Refresh; TODO: refresh all items
    procedure SetFocus;
    function Focused: Boolean;

    property Visible: Boolean
      read GetVisible write SetVisible;

    property Name: string
      read GetName;

    property Form: TForm
      read GetForm;
  end;

  IEditorToolViews = interface
  ['{A5575878-A189-4F3C-9008-61899B739DA1}']
    function GetView(AIndex: Integer): IEditorToolView;
    function GetViewByName(AName: string): IEditorToolView;
    function GetCount: Integer;

    function GetEnumerator: TEditorToolViewListEnumerator;

    function Register(
            AFormClass : TComponentClass;
      const AName      : string = ''
    ): Boolean;
    procedure Hide;

    property Views[AIndex: Integer]: IEditorToolView
      read GetView;

    property ViewByName[AName: string]: IEditorToolView
      read GetViewByName; default;

    property Count: Integer
      read GetCount;
  end;

  IEditorCommands = interface
  ['{5CB77731-425D-44FD-93BA-2137875F76B5}']
    function SaveFile(
      const AFileName   : string = '';
            AShowDialog : Boolean = False
    ): Boolean;
    procedure LoadFile;
    procedure OpenFileAtCursor;
    procedure ToggleHighlighter;
    procedure InsertCharacter(const C: TUTF8Char);
    procedure AssignHighlighter(const AName: string);
    procedure CopyToClipboard;
  end;

  IClipboardCommands = interface
  ['{E8E71973-7048-4238-93DD-CB6307CD1BA8}']
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;
    procedure Redo;
  end;

  IEditorMenus = interface
  ['{4B6F6B6A-8A72-478B-B3AF-089E72E23CDF}']
    {$region 'property access methods' /fold}
    function GetClipboardPopupMenu: TPopupMenu;
    function GetEditorPopupMenu: TPopupMenu;
    function GetEncodingPopupMenu: TPopupMenu;
    function GetExportPopupMenu: TPopupMenu;
    function GetFilePopupMenu: TPopupMenu;
    function GetFoldPopupMenu: TPopupMenu;
    function GetHighlighterPopupMenu: TPopupMenu;
    function GetInsertPopupMenu: TPopupMenu;
    function GetLineBreakStylePopupMenu: TPopupMenu;
    function GetSearchPopupMenu: TPopupMenu;
    function GetSelectionModePopupMenu: TPopupMenu;
    function GetSelectionPopupMenu: TPopupMenu;
    function GetSelectPopupMenu: TPopupMenu;
    function GetSettingsPopupMenu: TPopupMenu;
    {$endregion}

    property ClipboardPopupMenu: TPopupMenu
      read GetClipboardPopupMenu;

    property EditorPopupMenu: TPopupMenu
      read GetEditorPopupMenu;

    property EncodingPopupMenu: TPopupMenu
      read GetEncodingPopupMenu;

    property ExportPopupMenu: TPopupMenu
      read GetExportPopupMenu;

    property FilePopupMenu: TPopupMenu
      read GetFilePopupMenu;

    property FoldPopupMenu: TPopupMenu
      read GetFoldPopupMenu;

    property HighlighterPopupMenu: TPopupMenu
      read GetHighlighterPopupMenu;

    property InsertPopupMenu: TPopupMenu
      read GetInsertPopupMenu;

    property LineBreakStylePopupMenu: TPopupMenu
      read GetLineBreakStylePopupMenu;

    property SearchPopupMenu: TPopupMenu
      read GetSearchPopupMenu;

    property SelectPopupMenu: TPopupMenu
      read GetSelectPopupMenu;

    property SelectionPopupMenu: TPopupMenu
      read GetSelectionPopupMenu;

    property SelectionModePopupMenu: TPopupMenu
      read GetSelectionModePopupMenu;

    property SettingsPopupMenu: TPopupMenu
      read GetSettingsPopupMenu;
  end;

  { All supported actions by the editor views. }

  IEditorActions = interface
  ['{E42EF2E3-A7A0-4847-B299-3C35699DC708}']
    function GetActionList: TActionList;
    function GetItem(AName: string): TCustomAction;

    procedure UpdateActions;
    procedure UpdateHighLighterActions;

    property Items[AName: string]: TCustomAction
      read GetItem; default;

    property ActionList: TActionList
      read GetActionList;

    { TODO -oTS : Declare all actions as properties }

  end;

  { IEditorManager }

  IEditorManager = interface
  ['{631A126F-1693-4E25-B691-CD2487BCB820}']
    {$region 'property access methods' /fold}
    function GetActions: IEditorActions;
    function GetCommands: IEditorCommands;
    function GetEvents: IEditorEvents;
    function GetMenus: IEditorMenus;
    function GetSearchEngine: IEditorSearchEngine;
    function GetSelection: IEditorSelection;
    function GetSettings: IEditorSettings;
    function GetToolViews: IEditorToolViews;
    function GetViews: IEditorViews;
    function GetPersistSettings: Boolean;
    procedure SetPersistSettings(const AValue: Boolean);
    function GetActiveView: IEditorView;
    procedure SetActiveView(AValue: IEditorView);
    function GetHighlighters: THighlighters;
    {$endregion}

    function ActivateView(const AName: string): Boolean;
    procedure ClearHighlightSearch;
    function OpenFile(const AFileName: string): IEditorView;
    function NewFile(
      const AFileName  : string;
      const AText      : string = ''
    ): IEditorView;

    property PersistSettings: Boolean
      read GetPersistSettings write SetPersistSettings;

    property Highlighters: THighlighters
      read GetHighlighters;

    property ActiveView: IEditorView
      read GetActiveView write SetActiveView;

    property Events: IEditorEvents
      read GetEvents;

    property Views: IEditorViews
      read GetViews;

    property ToolViews: IEditorToolViews
      read GetToolViews;

    property Menus: IEditorMenus
      read GetMenus;

    property Actions: IEditorActions
      read GetActions;

    property Commands: IEditorCommands
      read GetCommands;

    property Selection: IEditorSelection
      read GetSelection;

    property Settings: IEditorSettings
      read GetSettings;

    property SearchEngine: IEditorSearchEngine
      read GetSearchEngine;
  end;

//-----------------------------------------------------------------------------

  TEditorViewListEnumerator = class
  strict private
    FIndex : Integer;
    FList  : TEditorViewList;

  public
    constructor Create(AList: TEditorViewList);
    function GetCurrent: IEditorView;
    function MoveNext: Boolean;
    property Current: IEditorView
      read GetCurrent;
  end;

  { TEditorToolViewListEnumerator }

  TEditorToolViewListEnumerator = class
  strict private
    FIndex : Integer;
    FList  : IEditorToolViews;

  public
    constructor Create(AList: IEditorToolViews);
    procedure BeforeDestruction; override;
    function GetCurrent: IEditorToolView;
    function MoveNext: Boolean;
    property Current: IEditorToolView
      read GetCurrent;
  end;

implementation

{$region 'TEditorViewListEnumerator' /fold}
constructor TEditorViewListEnumerator.Create(AList: TEditorViewList);
begin
  FList := AList;
  FIndex := -1;
end;

function TEditorViewListEnumerator.GetCurrent: IEditorView;
begin
  Result := FList[FIndex] as IEditorView;
end;

function TEditorViewListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < (FList.Count - 1);
  if Result then
    Inc(FIndex);
end;
{$endregion}

{$region 'TEditorToolViewListEnumerator' /fold}
constructor TEditorToolViewListEnumerator.Create(AList: IEditorToolViews);
begin
  FList := AList;
  FIndex := -1;
end;

procedure TEditorToolViewListEnumerator.BeforeDestruction;
begin
  FList := nil;
  inherited BeforeDestruction;
end;

function TEditorToolViewListEnumerator.GetCurrent: IEditorToolView;
begin
  Result := FList.Views[FIndex] as IEditorToolView;
end;

function TEditorToolViewListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < (FList.Count - 1);
  if Result then
    Inc(FIndex);
end;
{$endregion}

end.

