{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.Interfaces;

{$MODE DELPHI}

{ This unit hosts all interfaces of the editor module.
  To make an editor instance you need to use this unit in the interface section
  and ts.Editor.Factories in the implementation part to let you create
  instances supporting one or more of these interfaces. }

interface

uses
  Classes, ActnList, Controls, Forms, Menus, Contnrs, Graphics, ComCtrls,

  LCLType,

  SynEdit, SynEditTypes, SynEditHighlighter, SynMacroRecorder,
  SynEditPointClasses,

  ts.Core.FormSettings,

  ts.Editor.AlignLines.Settings, ts.Editor.Search.Engine.Settings,
  ts.Editor.CodeShaper.Settings, ts.Editor.CodeFilter.Settings,
  ts.Editor.MiniMap.Settings, ts.Editor.HexEditor.Settings,
  ts.Editor.SortStrings.Settings, ts.Editor.Options.Settings,

  ts.Editor.Types, ts.Editor.Highlighters, ts.Editor.HighlighterAttributes,
  ts.Editor.Colors.Settings, ts.Editor.Tools.Settings;

type
  // forward declarations
  IEditorView                   = interface;
  IEditorToolView               = interface;
  IEditorActions                = interface;
  IEditorSettings               = interface;
  IEditorSelection              = interface;
  TEditorViewListEnumerator     = class;
  TEditorToolViewListEnumerator = class;

  TAddEditorViewEvent = procedure(
    Sender      : TObject;
    AEditorView : IEditorView
  ) of object;

  TEditorToolViewEvent = procedure(
    Sender          : TObject;
    AEditorToolView : IEditorToolView
  ) of object;

  IControl = interface
  ['{303F3DE1-81F5-473B-812B-7DD4C306725B}']
    {$REGION 'property access methods'}
    function GetName: string;
    function GetParent: TWinControl;
    function GetPopupMenu: TPopupMenu;
    procedure SetName(AValue: string);
    procedure SetParent(AValue: TWinControl);
    procedure SetPopupMenu(AValue: TPopupMenu);
    {$ENDREGION}

    function Focused: Boolean;
    procedure SetFocus;
    function Canfocus: Boolean;

    property Parent: TWinControl
      read GetParent write SetParent;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

    property Name: string
      read GetName write SetName;
  end;

  { Handles display view of the editor. }

  { IEditorView }

  IEditorView = interface(IControl)
  ['{94689213-B046-45F6-922B-FAE91C02A3FF}']
    {$REGION 'property access methods'}
    function GetActions: IEditorActions;
    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretX: Integer;
    function GetCaretXY: TPoint;
    function GetCaretY: Integer;
    function GetCurrentChar: WideChar;
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
    function GetHighlighterName: string;
    function GetInsertMode: Boolean;
    function GetIsEmpty: Boolean;
    function GetIsFile: Boolean;
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
    function GetSynSelection: TSynEditSelection;
    function GetText: string;
    function GetTextSize: Int64;
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
    procedure SetHighlighterName(AValue: string);
    procedure SetInsertMode(AValue: Boolean);
    procedure SetIsFile(AValue: Boolean);
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
    {$ENDREGION}

    // information retrieval
    function GetWordAtPosition(const APosition: TPoint): string;
    function GetWordFromCaret(const ACaretPos: TPoint): string;
    function GetHighlighterAttriAtRowCol(
      APosition  : TPoint;
      out AToken : string;
      out AAttri : TSynHighlighterAttributes
    ): Boolean;

    // lock updates
    procedure BeginUpdate;
    procedure EndUpdate;

    // make current view the active one if more than one view is managed.
    procedure Activate;

    // state change
    procedure SetHighlightSearch(
      const ASearch : string;
      AOptions      : TSynSearchOptions
    );

    procedure Clear;
    procedure Close;

    // search
    procedure SearchAndSelectLine(ALineIndex: Integer; const ALine: string);
    procedure SearchAndSelectText(const AText: string);
    procedure FindNextWordOccurrence(DirectionForward: Boolean);
    procedure ClearHighlightSearch;

    // load and save
    procedure Load(const AStorageName: string = '');
    procedure Save(const AStorageName: string = '');
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

     // selection
    procedure SelectAll;
    procedure SelectWord;

    procedure CopySelectionToClipboard;
    procedure CopyAllToClipboard;

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

    { Reference to the main 'Actions' instance .}
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

    property SynSelection: TSynEditSelection
      read GetSynSelection;

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

    property TextSize: Int64
      read GetTextSize;

    property LinesInWindow: Integer
      read GetLinesInWindow;

    property SupportsFolding: Boolean
      read GetSupportsFolding;

    property CurrentChar: WideChar
      read GetCurrentChar;

    property CurrentWord: string
      read GetCurrentWord;

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

    { If IsFile=True then this property holds the content's filename. In the
      other case it is a name that corresponds to content provided by the owning
      application. This name can eg. be associated with a database resource to
      load the content from. }
    property FileName: string
      read GetFileName write SetFileName;

    { Component name of the editor view }
    property Name: string
      read GetName write SetName;

    { Returns True if the view doesn't contain any data. }
    property IsEmpty: Boolean
      read GetIsEmpty;

    { Determines if the view content is stored in a file. If True the content
      will be laoded and saved to a file using the FileName property. If False
      this is delegated to the owning instance that can provide the content
      from another resource like eg. a database. }
    property IsFile: Boolean
      read GetIsFile write SetIsFile;

    property LineText: string
      read GetLineText write SetLineText;

    property PreviewText: string
      read GetPreviewText;

    property MonitorChanges: Boolean
      read GetMonitorChanges write SetMonitorChanges;

    property ShowSpecialChars: Boolean
      read GetShowSpecialChars write SetShowSpecialChars;

    property TopLine: Integer
      read GetTopLine write SetTopLine;

    { Set when content has changed since initial load or last save. }
    property Modified: Boolean
      read GetModified write SetModified;

    property Encoding: string
      read GetEncoding write SetEncoding;

    property LineBreakStyle: string
      read GetLineBreakStyle write SetLineBreakStyle;

    property HighlighterItem: THighlighterItem
      read GetHighlighterItem write SetHighlighterItem;

    property HighlighterName: string
      read GetHighlighterName write SetHighlighterName;

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

  IEditorSelection = interface
  ['{DEBBB1D5-A04A-4264-96E9-0693E20C2A0D}']
    {$REGION 'property access methods'}
    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetCaretXY: TPoint;
    function GetLines: TStrings;
    function GetSelectionMode: TSynSelectionMode;
    function GetText: string;
    function GetTextSize: Int64;
    procedure SetBlockBegin(AValue: TPoint);
    procedure SetBlockEnd(AValue: TPoint);
    procedure SetCaretXY(AValue: TPoint);
    procedure SetSelectionMode(AValue: TSynSelectionMode);
    procedure SetText(AValue: string);
    {$ENDREGION}

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

    property TextSize: Int64
      read GetTextSize;
  end;

  IEditorSearchEngine = interface
  ['{5403336C-3E81-4A1B-B2BB-170CF0EF0B84}']
    {$REGION 'property access methods'}
    function GetCurrentIndex: Integer;
    function GetItemGroups: TObjectList;
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
    {$ENDREGION}

    procedure Execute;
    procedure Replace;
    procedure ReplaceAll;
    procedure FindNext;
    procedure FindPrevious;

    procedure AddOnExecuteHandler(AEvent: TNotifyEvent);
    procedure RemoveOnExecuteHandler(AEvent: TNotifyEvent);
    procedure AddOnChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveOnChangeHandler(AEvent: TNotifyEvent);

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

    property ItemGroups: TObjectList
      read GetItemGroups;
  end;

  { TODO: make a seperation of the events and the dispatcher methods? }

  IEditorEvents = interface
  ['{D078C92D-16DF-4727-A18F-4C76E07D37A2}']
    {$REGION 'property access methods'}
    function GetOnAddEditorView: TAddEditorViewEvent;
    function GetOnAfterSave: TStorageEvent;
    function GetOnBeforeSave: TStorageEvent;
    function GetOnHideEditorToolView: TEditorToolViewEvent;
    function GetOnLoad: TStorageEvent;
    function GetOnOpen: TStorageEvent;
    function GetOnOpenOtherInstance: TOpenOtherInstanceEvent;
    function GetOnSave: TStorageEvent;
    function GetOnShowEditorToolView: TEditorToolViewEvent;
    procedure SetOnAddEditorView(AValue: TAddEditorViewEvent);
    procedure SetOnAfterSave(AValue: TStorageEvent);
    procedure SetOnBeforeSave(AValue: TStorageEvent);
    procedure SetOnHideEditorToolView(AValue: TEditorToolViewEvent);
    procedure SetOnLoad(const AValue: TStorageEvent);
    procedure SetOnMacroStateChange(const AValue: TMacroStateChangeEvent);
    function GetOnMacroStateChange: TMacroStateChangeEvent;
    function GetOnNew: TNewEvent;
    function GetOnStatusChange: TStatusChangeEvent;
    procedure SetOnNew(const AValue: TNewEvent);
    procedure SetOnOpen(AValue: TStorageEvent);
    procedure SetOnOpenOtherInstance(AValue: TOpenOtherInstanceEvent);
    procedure SetOnSave(const AValue: TStorageEvent);
    procedure SetOnShowEditorToolView(AValue: TEditorToolViewEvent);
    procedure SetOnStatusChange(const AValue: TStatusChangeEvent);
    {$ENDREGION}

    // event dispatch methods
    procedure DoCaretPositionChange;
    procedure DoActiveViewChange;
    procedure DoActionExecute(
          AAction  : TBasicAction;
      var AHandled : Boolean
    );
    procedure DoMacroStateChange(AState : TSynMacroState);
    procedure DoHighlighterChange;
    procedure DoOpenOtherInstance(const AParams: array of string);
    procedure DoAddEditorView(AEditorView: IEditorView);
    procedure DoStatusMessage(AText: string);
    procedure DoStatusChange(AChanges: TSynStatusChanges);
    procedure DoShowToolView(AToolView: IEditorToolView);
    procedure DoHideToolView(AToolView: IEditorToolView);
    procedure DoChange;
    procedure DoModified;
    procedure DoOpen(const AName: string);
    procedure DoBeforeSave(const AName: string);
    procedure DoAfterSave(const AName: string);
    procedure DoLoad(const AName: string);
    procedure DoNew(
      const AFileName : string = '';
      const AText     : string = ''
    );

    // multicast events
    procedure AddOnChangeHandler(AEvent: TNotifyEvent);
    procedure AddOnModifiedHandler(AEvent: TNotifyEvent);
    procedure AddOnActiveViewChangeHandler(AEvent: TNotifyEvent);
    procedure AddOnCaretPositionEvent(AEvent: TCaretPositionEvent);
    procedure AddOnActionExecuteEvent(AEvent: TActionExecuteEvent);
    procedure AddOnHighlighterChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveOnChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveOnModifiedHandler(AEvent: TNotifyEvent);
    procedure RemoveOnActiveViewChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveOnCaretPositionEvent(AEvent: TCaretPositionEvent);
    procedure RemoveOnActionExecuteEvent(AEvent: TActionExecuteEvent);
    procedure RemoveOnHighlighterChangeHandler(AEvent: TNotifyEvent);

    // events
    property OnAddEditorView: TAddEditorViewEvent
      read GetOnAddEditorView write SetOnAddEditorView;

    property OnShowEditorToolView: TEditorToolViewEvent
      read GetOnShowEditorToolView write SetOnShowEditorToolView;

    property OnHideEditorToolView: TEditorToolViewEvent
      read GetOnHideEditorToolView write SetOnHideEditorToolView;

    property OnStatusChange: TStatusChangeEvent
      read GetOnStatusChange write SetOnStatusChange;

    property OnMacroStateChange: TMacroStateChangeEvent
      read GetOnMacroStateChange write SetOnMacroStateChange;

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

    property OnOpenOtherInstance: TOpenOtherInstanceEvent
      read GetOnOpenOtherInstance write SetOnOpenOtherInstance;
  end;

  { Settings we should store if we would like to restore the editor to its
    current state }

  IEditorSettings = interface
  ['{CDB18A45-54AA-49F2-82C7-15D68C952197}']
    {$REGION 'property access methods'}
    function GetAutoFormatXML: Boolean;
    function GetAutoGuessHighlighterType: Boolean;
    function GetCloseWithESC: Boolean;
    function GetColors: TEditorColorSettings;
    function GetDebugMode: Boolean;
    function GetDimInactiveView: Boolean;
    function GetEditorFont: TFont;
    function GetEditorOptions: TEditorOptionsSettings;
    function GetFileName: string;
    function GetFormSettings: TFormSettings;
    function GetHighlighterAttributes: THighlighterAttributes;
    function GetHighlighters: THighlighters;
    function GetHighlighterType: string;
    function GetLanguageCode: string;
    function GetReadOnly: Boolean;
    function GetSingleInstance: Boolean;
    function GetToolSettings: TEditorToolSettings;
    function GetXML: string;
    procedure SetAutoFormatXML(const AValue: Boolean);
    procedure SetAutoGuessHighlighterType(const AValue: Boolean);
    procedure SetCloseWithESC(const AValue: Boolean);
    procedure SetColors(AValue: TEditorColorSettings);
    procedure SetDebugMode(AValue: Boolean);
    procedure SetDimInactiveView(const AValue: Boolean);
    procedure SetEditorFont(AValue: TFont);
    procedure SetEditorOptions(AValue: TEditorOptionsSettings);
    procedure SetFileName(const AValue: string);
    procedure SetFormSettings(const AValue: TFormSettings);
    procedure SetHighlighterAttributes(AValue: THighlighterAttributes);
    procedure SetHighlighterType(const AValue: string);
    procedure SetLanguageCode(AValue: string);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetSingleInstance(AValue: Boolean);
    procedure SetToolSettings(AValue: TEditorToolSettings);
    {$ENDREGION}

    procedure Load;
    procedure Save;
    procedure Apply;

    procedure AddEditorSettingsChangedHandler(AEvent: TNotifyEvent);
    procedure RemoveEditorSettingsChangedHandler(AEvent: TNotifyEvent);

    property Colors: TEditorColorSettings
      read GetColors write SetColors;

    property ToolSettings:  TEditorToolSettings
      read GetToolSettings write SetToolSettings;

    property FileName: string
      read GetFileName write SetFileName;

    property EditorFont: TFont
      read GetEditorFont write SetEditorFont;

    property DimInactiveView: Boolean
      read GetDimInactiveView write SetDimInactiveView;

    property HighlighterType: string
      read GetHighlighterType write SetHighlighterType;

    { Locale to be used by the application }
    property LanguageCode: string
      read GetLanguageCode write SetLanguageCode;

    property ReadOnly: Boolean
      read GetReadOnly write SetReadOnly;

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

    property HighlighterAttributes: THighlighterAttributes
      read GetHighlighterAttributes write SetHighlighterAttributes;

    property DebugMode: Boolean
      read GetDebugMode write SetDebugMode;

    property SingleInstance: Boolean
      read GetSingleInstance write SetSingleInstance;

    property XML: string
      read GetXML;

    // Editor options
    property EditorOptions: TEditorOptionsSettings
      read GetEditorOptions write SetEditorOptions;
  end;

  IEditorViews = interface
  ['{FBFB8DC6-7663-4EA4-935D-5B9F3CD7C753}']
    {$REGION 'property access methods'}
    function GetView(AIndex: Integer): IEditorView;
    function GetViewByFileName(AFileName: string): IEditorView;
    function GetViewByName(AName: string): IEditorView;
    function GetCount: Integer;
    function GetViewList: TEditorViewList;
    {$ENDREGION}

    function Add(
      const AName        : string = '';
      const AFileName    : string = '';
      const AHighlighter : string = ''
    ): IEditorView;
    function AddSharedView(
      AEditorView : IEditorView;
      const AName : string = ''
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
    {$REGION 'property access methods'}
    function GetForm: TForm;
    function GetName: string;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    {$ENDREGION}

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
    {$REGION 'property access methods'}
    function GetView(AIndex: Integer): IEditorToolView;
    function GetViewByName(AName: string): IEditorToolView;
    function GetCount: Integer;
    {$ENDREGION}

    function GetEnumerator: TEditorToolViewListEnumerator;

    function Register(
      AFormClass     : TComponentClass;
      ASettingsClass : TComponentClass;
      const AName    : string = ''
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
    procedure OpenFileAtCursor;
    procedure ToggleHighlighter;
    procedure AssignHighlighter(const AName: string);
    procedure CompressSpace;
    procedure CompressWhitespace;
    procedure CreateDesktopLink;
    procedure CopySelectionToClipboard;
    procedure CopyAllToClipboard;
    procedure UpperCaseSelection;
    procedure LowerCaseSelection;
    procedure PascalStringFromSelection;
    procedure QuoteLinesInSelection(ADelimit : Boolean = False);
    procedure DequoteLinesInSelection;
    procedure QuoteSelection;
    procedure DequoteSelection;
    procedure Base64FromSelection(ADecode: Boolean = False);
    procedure URLFromSelection(ADecode: Boolean = False);
    procedure XMLFromSelection(ADecode: Boolean = False);
    procedure ConvertTabsToSpacesInSelection;
    procedure SortStrings;
    procedure SyncEditSelection;
    procedure AlignSelection(
      const AToken            : string;
      ACompressWS             : Boolean;
      AInsertSpaceBeforeToken : Boolean;
      AInsertSpaceAfterToken  : Boolean;
      AAlignInParagraphs      : Boolean
    );
    procedure MergeBlankLinesInSelection;
    procedure StripCommentsFromSelection;
    procedure StripMarkupFromSelection;
    procedure StripCharsFromSelection(
      AFirst : Boolean;
      ALast  : Boolean
    );
    procedure Indent;
    procedure UnIndent;
    procedure UpdateCommentSelection(ACommentOn, AToggle: Boolean);
    procedure ToggleBlockCommentSelection;
    procedure InsertTextAtCaret(const AText: string);
    procedure FormatCode;
    procedure SmartSelect;
    procedure GuessHighlighterType;
    function SelectBlockAroundCursor(
      const AStartTag  : string;
      const AEndTag    : string;
      AIncludeStartTag : Boolean;
      AIncludeEndTag   : Boolean
    ): Boolean;
    procedure AdjustFontSize(AOffset: Integer);
    procedure Save;
    procedure SaveAll;

    procedure FindNext;
    procedure FindPrevious;
  end;

  IEditorMenus = interface
  ['{4B6F6B6A-8A72-478B-B3AF-089E72E23CDF}']
    {$REGION 'property access methods'}
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
    function GetSelectionDecodePopupMenu: TPopupMenu;
    function GetSelectionEncodePopupMenu: TPopupMenu;
    function GetSelectionModePopupMenu: TPopupMenu;
    function GetSelectionPopupMenu: TPopupMenu;
    function GetSelectPopupMenu: TPopupMenu;
    function GetSettingsPopupMenu: TPopupMenu;
    {$ENDREGION}

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

    property SelectionEncodePopupMenu: TPopupMenu
      read GetSelectionEncodePopupMenu;

    property SelectionDecodePopupMenu: TPopupMenu
      read GetSelectionDecodePopupMenu;

    property SelectionModePopupMenu: TPopupMenu
      read GetSelectionModePopupMenu;

    property SettingsPopupMenu: TPopupMenu
      read GetSettingsPopupMenu;
  end;

  { All supported actions by the editor views. }

  IEditorActions = interface
  ['{E42EF2E3-A7A0-4847-B299-3C35699DC708}']
    {$REGION 'property access methods'}
    function GetActionList: TActionList;
    function GetItem(AName: string): TCustomAction;
    {$ENDREGION}

    procedure UpdateActions;
    procedure UpdateHighLighterActions;

    property Items[AName: string]: TCustomAction
      read GetItem; default;

    property ActionList: TActionList
      read GetActionList;

    { TODO -oTS : Declare all actions as properties? }
  end;

  IEditorManager = interface
  ['{631A126F-1693-4E25-B691-CD2487BCB820}']
    {$REGION 'property access methods'}
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
    {$ENDREGION}

    procedure UpdateActions;
    function ActivateView(const AName: string): Boolean;
    procedure ClearHighlightSearch;
    function OpenFile(const AFileName: string): IEditorView;
    function NewFile(
      const AFileName : string;
      const AText     : string = ''
    ): IEditorView;
    function SaveFile(
      const AFileName : string = '';
      AShowDialog     : Boolean = False
    ): Boolean;

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

  IEditorManagerFactory = interface
  ['{BE85A08D-936E-4F76-BBE1-A1999DE882B9}']
    function CreateInstance(
      AOwner                  : TComponent = nil;
      APersistSettings        : Boolean = False;
      const ASettingsFileName : string = ''
    ): IEditorManager; overload;

    function CreateInstance(
      AOwner    : TComponent;
      ASettings : IEditorSettings
    ): IEditorManager; overload;
  end;

  IEditorViewFactory = interface
  ['{CAEF28D5-0E70-4D4E-AEC7-07BD6E743945}']
    function CreateInstance(
      AParent            : TWinControl;
      AManager           : IEditorManager;
      const AName        : string = '';
      const AFileName    : string = '';
      const AHighlighter : string = 'TXT'
    ): IEditorView;
  end;

  IEditorSettingsFactory = interface
  ['{6479785C-A7C0-40D9-9036-D39BEE780CA2}']
    function CreateInstance(
      AOwner          : TComponent = nil;
      const AFileName : string = ''
    ): IEditorSettings;
  end;

  IEditorMenusFactory = interface
  ['{99676D13-D72A-4C2F-B96B-84FC290DDE12}']
    function CreateMainMenu(
      AOwner: TComponent
    ): TMainMenu;
  end;

  IEditorToolbarsFactory = interface
  ['{0E1F34F3-E5AF-4A59-8B13-0F9B4D11D69D}']
    function CreateMainToolbar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolbar;

    function CreateSelectionToolbar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolbar;

    function CreateRightToolbar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolbar;
  end;

implementation

{$REGION 'TEditorViewListEnumerator'}
constructor TEditorViewListEnumerator.Create(AList: TEditorViewList);
begin
  FList  := AList;
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
{$ENDREGION}

{$REGION 'TEditorToolViewListEnumerator'}
constructor TEditorToolViewListEnumerator.Create(AList: IEditorToolViews);
begin
  FList  := AList;
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
{$ENDREGION}

end.

