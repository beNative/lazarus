{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts_Editor_View;

{$MODE Delphi}

{$region 'documentation' /fold}
{
Form holding a complete customizable text editor based on the open source
SynEdit components.
Features:
  - accepts dropped files
  - auto detect file encoding
  - dynamic editor creation
  - synchronized edit
  - highlight selected text
  - code folding
  - file monitor function to watch for external file changes.

TODO:
  - remove bookmark images
  - macrorecorder
  - template editor
  - configurable page setup and printing with preview
  - quickbuttons (like the Delphi version had)
  - URI opener, to open hyperlinks directly from the editor
  - customizable keystroke-function mappings
  - configurable code completion proposal
  - convert to another encoding (partially implemented)
  - find a way to fold particular sections (now only levels are supported)
  - send to mail action

  KNOWN ISSUES:
  - When created at runtime, the cleanup of the TSynEdit instance is magnitudes
    faster than using a design-time instance. Therefor we rely on a manually
    created instance.
    This way it is also easier to adapt to changes in the SynEdit component.

  DEPENDENCIES:
  - SynEdit
  - ts.Core.DirectoryWatch: do react on modifications.
}
{$endregion}

interface
uses
  Classes, Controls, Forms, Graphics, Menus, SysUtils, Dialogs, Types, StdCtrls,

  LMessages, LCLType,

  SynEdit, SynEditHighlighter, SynPluginSyncroEdit, SynPluginTemplateEdit,
  SynEditPointClasses, SynEditMarkupHighAll, SynEditTypes, SynBeautifier,
  SynEditHighlighterFoldBase, SynEditKeyCmds, SynEditMouseCmds, SynEditMarks,
  SynEditMiscClasses,

  ts.Core.DirectoryWatch,

  ts_Editor_Resources, ts.Editor.Highlighters, ts.Editor.Interfaces,
  ts.Editor.Selection,

  // logging
  ts.Core.SharedLogger;

type

  { TEditorView }

  TEditorView = class(TForm, IEditorView, IEditorSelection)
  published
    imlBookmarkImages: TImageList;

    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);

  strict private
    // SynEdit event handlers
    procedure EditorChangeUpdating(
      ASender    : TObject;
      AUpdating  : Boolean
    );
    procedure EditorSpecialLineMarkup(
          Sender  : TObject;
          Line    : Integer;
      var Special : Boolean;
          Markup  : TSynSelectedColor
    );
    procedure EditorClearBookmark(
          Sender : TObject;
      var Mark   : TSynEditMark
    );
    procedure EditorGutterClick(
      Sender     : TObject;
      X, Y, Line : Integer;
      Mark       : TSynEditMark
    );
    procedure EditorCutCopy(
          Sender       : TObject;
      var AText        : string;
      var AMode        : TSynSelectionMode;
          ALogStartPos : TPoint;
      var AAction      : TSynCopyPasteAction
    );
    procedure EditorMouseLink(
          Sender         : TObject;
          X, Y           : Integer;
      var AllowMouseLink : Boolean
    );
    procedure EditorSpecialLineColors(
          Sender  : TObject;
          Line    : Integer;
      var Special : Boolean;
      var FG, BG  : TColor
    );
    procedure EditorChange(Sender: TObject);
    procedure EditorClickLink(
      Sender : TObject;
      Button : TMouseButton;
      Shift  : TShiftState;
      X, Y   : Integer
    );
    procedure EditorPaste(
          Sender       : TObject;
      var AText        : string;
      var AMode        : TSynSelectionMode;
          ALogStartPos : TPoint;
      var AAction      : TSynCopyPasteAction
    );
    procedure EditorStatusChange(
      Sender  : TObject;
      Changes : TSynStatusChanges
    );
    procedure EditorProcessCommand(
          Sender  : TObject;
      var Command : TSynEditorCommand;
      var AChar   : TUTF8Char;
          Data    : Pointer
    );
    procedure EditorProcessUserCommand(
          Sender  : TObject;
      var Command : TSynEditorCommand;
      var AChar   : TUTF8Char;
          Data    : Pointer
    );
    procedure EditorCommandProcessed(
          Sender  : TObject;
      var Command : TSynEditorCommand;
      var AChar   : TUTF8Char;
          Data    : Pointer
    );
    procedure EditorReplaceText(
            Sender        : TObject;
      const ASearch       : string;
      const AReplace      : string;
            Line, Column  : Integer;
        var ReplaceAction : TSynReplaceAction
    );
    function EditorMouseActionExec(
          AnAction : TSynEditMouseAction;
      var AnInfo   : TSynEditMouseActionInfo
    ): Boolean;
    function EditorMouseActionSearch(
      var AnInfo          : TSynEditMouseActionInfo;
          HandleActionProc: TSynEditMouseActionHandler
    ): Boolean;
    procedure EditorKeyTranslation(
          Sender          : TObject;
          Code            : Word;
          SState          : TShiftState;
      var Data            : Pointer;
      var IsStartOfCombo  : Boolean;
      var Handled         : Boolean;
      var Command         : TSynEditorCommand;
          FinishComboOnly : Boolean;
      var ComboKeyStrokes : TSynEditKeyStrokes
    );

{$IFDEF Windows}
    procedure DirectoryWatchNotify(const Sender: TObject;
      const AAction: TWatchAction; const FileName: string);
{$ENDIF}

    function IsActive: Boolean;

    procedure UpdateSharedViews;
    procedure ApplySettings;

  private
    FUpdate          : Boolean;
{$IFDEF Windows}
    FDirectoryWatch  : TDirectoryWatch;
{$ENDIF}
    FEncoding        : string;
    FLineBreakStyle  : string;
    FEditor          : TSynEdit;
    FFindHistory     : TStringList;
    FHighlighter     : TSynCustomHighlighter;
    FReplaceHistory  : TStringList;
    FSyncronizedEdit : TSynPluginSyncroEdit;
    FTemplateEdit    : TSynPluginTemplateEdit;
    FHighlighterItem : THighlighterItem;
    FMHAC            : TSynEditMarkupHighlightAllCaret;
    FFileName        : string;
    FFoldLevel       : Integer;
    FBeautifier      : TSynBeautifier;
    FIsFile          : Boolean;
    FFontChanged     : Boolean;
    FSynSelection    : TSynEditSelection;

    FSelection       : IEditorSelection;
    // experimental
    FMasterView      : IEditorView;
    FSlaveView       : IEditorView;

    FOnDropFiles     : TDropFilesEvent;
    FOnStatusChange  : TStatusChangeEvent;
    FOnChange        : TNotifyEvent;

    { search settings }
    FSearchText     : string;
    FSearchOptions  : TSynSearchOptions;

    {$region 'property access methods' /fold}
    function GetActions: IEditorActions;
    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretX: Integer; virtual;
    function GetCaretXY: TPoint;
    function GetCaretY: Integer; virtual;
    function GetCommands: IEditorCommands;
    function GetCurrentChar: WideChar;
    function GetCurrentWord: string;
    function GetEditor: TSynEdit;
    function GetEditorFont: TFont;
    function GetHighlighterName: string;
    function GetInsertMode: Boolean;
    function GetIsFile: Boolean;
    function GetManager: IEditorManager;
    function GetEncoding: string;
    function GetEvents: IEditorEvents;
    function GetFileName: string;
    function GetFindHistory: TStrings;
    function GetFoldLevel: Integer;
    function GetFoldState: string;
    function GetForm: TCustomForm;
    function GetHighlighterItem: THighlighterItem;
    function GetLineBreakStyle: string;
    function GetLines: TStrings; virtual;
    function GetLinesInWindow: Integer;
    function GetLineText: string;
    function GetLogicalCaretXY: TPoint;
    function GetMasterView: IEditorView;
    function GetModified: Boolean;
    function GetMonitorChanges: Boolean;
    function GetName: string;
    function GetOnChange: TNotifyEvent;
    function GetOnDropFiles: TDropFilesEvent;
    function GetOnStatusChange: TStatusChangeEvent;
    function GetParent: TWinControl;
    function GetPopupMenu: TPopupMenu; reintroduce;
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
    function GetTextSize: Integer;
    function GetTopLine: Integer;
    procedure SetBlockBegin(const AValue: TPoint);
    procedure SetBlockEnd(const AValue: TPoint);
    procedure SetCaretX(const Value: Integer); virtual;
    procedure SetCaretXY(const AValue: TPoint);
    procedure SetCaretY(const Value: Integer); virtual;
    procedure SetEditorFont(AValue: TFont);
    procedure SetEncoding(const AValue: string);
    procedure SetFileName(const AValue: string);
    procedure SetFoldLevel(const AValue: Integer);
    procedure SetFoldState(const AValue: string);
    procedure SetHighlighter(const AValue: TSynCustomHighlighter);
    procedure SetHighlighterItem(const AValue: THighlighterItem);
    procedure SetHighlighterName(AValue: string);
    procedure SetInsertMode(AValue: Boolean);
    procedure SetIsFile(AValue: Boolean);
    procedure SetLineBreakStyle(const AValue: string);
    procedure SetLines(const Value: TStrings); virtual;
    procedure SetLineText(const AValue: string);
    procedure SetLogicalCaretXY(const AValue: TPoint);
    procedure SetMasterView(AValue: IEditorView);
    procedure SetModified(const AValue: Boolean);
    procedure SetMonitorChanges(const AValue: Boolean);
    procedure SetName(AValue: string); reintroduce;
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TDropFilesEvent);
    procedure SetOnStatusChange(const AValue: TStatusChangeEvent);
    procedure SetParent(const AValue: TWinControl); reintroduce;
    procedure SetPopupMenu(AValue: TPopupMenu);
    procedure SetSearchOptions(AValue: TSynSearchOptions);
    procedure SetSearchText(const Value: string); virtual;
    procedure SetSelectionMode(AValue: TSynSelectionMode);
    procedure SetSelEnd(const AValue: Integer);
    procedure SetSelStart(const AValue: Integer);
    procedure SetSelText(const AValue: string);
    procedure SetShowSpecialChars(const AValue: Boolean);
    procedure SetSlaveView(AValue: IEditorView);
    procedure SetText(const AValue: string);
    procedure SetTopLine(const AValue: Integer);
    {$endregion}

    procedure InitializeEditor(AEditor: TSynEdit);
    procedure EditorSettingsChanged(ASender: TObject);

  strict protected
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure CopyToClipboard;
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;
    procedure Redo;

    function EditorViewFocused: Boolean;
    function IEditorView.Focused = EditorViewFocused;

    procedure AssignHighlighterForFileType(const AFileExt: string);

    procedure ClearHighlightSearch;
    procedure SetHighlightSearch(
      const ASearch  : string;
            AOptions : TSynSearchOptions
    );
    procedure SearchAndSelectLine(
            ALineIndex : Integer;
      const ALine      : string
    );
    procedure SearchAndSelectText(const AText: string);
    procedure SelectWord;
    procedure FindNextWordOccurrence(ADirectionForward: Boolean);

    procedure Clear;
    procedure SelectAll;

    procedure DoChange; dynamic;

  protected
    // TCustomForm overrides
    procedure Activate; override;
    procedure UpdateActions; override;
    procedure DoClose(var CloseAction: TCloseAction); override;

  public
    // constructors and destructors
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    // public overridden methods
    function CloseQuery: Boolean; override;

    // public methods
    function GetWordAtPosition(const APosition: TPoint): string;
    function GetWordFromCaret(const ACaretPos: TPoint): string;
    function GetHighlighterAttriAtRowCol(
          APosition : TPoint;
      out AToken    : string;
      out AAttri    : TSynHighlighterAttributes
    ): Boolean;
    procedure Load(const AStorageName: string = '');
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure Save(const AStorageName: string = '');

    // public properties
    { Master view from which the text buffer is shared. }
    property MasterView: IEditorView
      read GetMasterView write SetMasterView;

    { Slave view which shares the text buffer. }
    property SlaveView: IEditorView
      read GetSlaveView write SetSlaveView;

    { Column and line of the start of the selected block. }
    property BlockBegin: TPoint
      read GetBlockBegin write SetBlockBegin;

    { Column and line of the end of the selected block. }
    property BlockEnd: TPoint
      read GetBlockEnd write SetBlockEnd;

    { Current position of the caret on the screen. Expanded TABs make this
      position different from LogicalCaretXY. }
    property CaretXY: TPoint
      read GetCaretXY write SetCaretXY;

    { Current position of the caret in the data buffer. }
    property LogicalCaretXY: TPoint
      read GetLogicalCaretXY write SetLogicalCaretXY;

  published // for the moment published only for easy debugging
    { current X-coordinate of the caret on the screen. }
    property CaretX: Integer
      read GetCaretX write SetCaretX;

    { current Y-coordinate of the caret on the screen. }
    property CaretY: Integer
      read GetCaretY write SetCaretY;

    property CurrentChar: WideChar
      read GetCurrentChar;

    property CurrentWord: string
      read GetCurrentWord;

    property TextSize: Integer
      read GetTextSize;

    property CanPaste: Boolean
      read GetCanPaste;

    property CanRedo: Boolean
      read GetCanRedo;

    property CanUndo: Boolean
      read GetCanUndo;

    property FoldLevel: Integer
      read GetFoldLevel write SetFoldLevel;

    property SelStart: Integer
      read GetSelStart write SetSelStart;

    property SelEnd: Integer
      read GetSelEnd write SetSelEnd;

    property InsertMode: Boolean
      read GetInsertMode write SetInsertMode;

    property SelectionMode: TSynSelectionMode
      read GetSelectionMode write SetSelectionMode;

    property ShowSpecialChars: Boolean
      read GetShowSpecialChars write SetShowSpecialChars;

    property Modified: Boolean
      read GetModified write SetModified;

    { Component name }
    property Name: string
      read GetName write SetName;

    property IsFile: Boolean
      read GetIsFile write SetIsFile;

    { Amount of visible lines (not including folds). }
    property LinesInWindow: Integer
      read GetLinesInWindow;

    property Editor: TSynEdit
      read GetEditor;

    property Form: TCustomForm
      read GetForm;

    property FoldState: string
      read GetFoldState write SetFoldState;

    property LineText: string
      read GetLineText write SetLineText;

    { Text to pass if the preview is shown (this is LineText when nothing is
      selected and the selected text otherwise). }
    property PreviewText: string
      read GetPreviewText;

    property SelAvail: Boolean
      read GetSelAvail;

    property Manager: IEditorManager
      read GetManager;

    { Reference to the IEditorActions singleton that manages one or more
      IEditView instances. }
    property Actions: IEditorActions
      read GetActions;

    property Commands: IEditorCommands
      read GetCommands;

    { A set of common events to dispatch to the application. }
    property Events: IEditorEvents
      read GetEvents;

    { Global settings shared by all EditorView instances }
    property Settings: IEditorSettings
      read GetSettings;

    { TODO: assign this to active search engine }
    property FindHistory: TStrings
      read GetFindHistory;

    { TODO: assign this to active search engine }
    property ReplaceHistory: TStrings
      read GetReplaceHistory;

    property Highlighter: TSynCustomHighlighter
      read FHighlighter write SetHighlighter;

    property HighlighterName: string
      read GetHighlighterName write SetHighlighterName;

    property HighlighterItem: THighlighterItem
      read GetHighlighterItem write SetHighlighterItem;

    property Selection: IEditorSelection
      read GetSelection implements IEditorSelection;

    property SynSelection: TSynEditSelection
      read GetSynSelection;

    { Shortcut to the text contained in the editor. }
    property Lines: TStrings
      read GetLines write SetLines;

    property MonitorChanges: Boolean
      read GetMonitorChanges write SetMonitorChanges;

    property Text: string
      read GetText write SetText;

    property TopLine: Integer
      read GetTopLine write SetTopLine;

    property SearchText: string
      read GetSearchText write SetSearchText;

    { These options are used for highlighting the active word. }
    property SearchOptions: TSynSearchOptions
      read GetSearchOptions write SetSearchOptions;

    { Currently selected text in the editor. }
    property SelText: string
      read GetSelText write SetSelText;

    property FileName: string
      read GetFileName write SetFileName;

    property Parent: TWinControl
      read GetParent write SetParent;

    { Current file encoding (Editor encoding is always UTF-8). }
    property Encoding: string
      read GetEncoding write SetEncoding;

    property EditorFont: TFont
      read GetEditorFont write SetEditorFont;

    property LineBreakStyle: string
      read GetLineBreakStyle write SetLineBreakStyle;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

    property OnStatusChange: TStatusChangeEvent
      read GetOnStatusChange write SetOnStatusChange;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;
  end;

implementation

{$R *.lfm}

uses
  GraphUtil, TypInfo, FileUtil, LazUTF8Classes,

  LConvEncoding, LCLProc,

  ts.Core.Utils,

  ts.Editor.Utils;

type

  { TSynEditAccess }

  TSynEditAccess = class(TSynEdit)
  private
    function GetCaret: TSynEditCaret;
  public
     // As viewed internally (with uncommited spaces / TODO: expanded tabs, folds). This may change, use with care
    property ViewedTextBuffer;
    // (TSynEditStringList) No uncommited (trailing/trimmable) spaces
    property TextBuffer;
    property WordBreaker; // TSynWordBreaker
    property Caret: TSynEditCaret
      read GetCaret;
  end;

{ TSynEditAccess }

{$region 'TSynEditAccess' /fold}
function TSynEditAccess.GetCaret: TSynEditCaret;
begin
  Result := GetCaretObj;
end;
{$endregion}

{$region 'construction and destruction' /fold}
procedure TEditorView.AfterConstruction;
var
  E : TSynEditAccess;
begin
  inherited AfterConstruction;
  FEditor             := TSynEditAccess.Create(Self);

  E                   := TSynEditAccess(FEditor);
  FSynSelection       := TSynEditSelection.Create(E.ViewedTextBuffer, True);
  FSynSelection.Caret := E.Caret;

  FSelection := TEditorSelection.Create(Self);

  FFindHistory            := TStringList.Create;
  FFindHistory.Sorted     := True;
  FFindHistory.Duplicates := dupIgnore;

  FReplaceHistory            := TStringList.Create;
  FReplaceHistory.Sorted     := True;
  FReplaceHistory.Duplicates := dupIgnore;

  FIsFile         := True;
  FEncoding       := EncodingUTF8;
  FLineBreakStyle := ALineBreakStyles[Lines.TextLineBreakStyle];

  InitializeEditor(FEditor);
{$IFDEF Windows}
  FDirectoryWatch          := TDirectoryWatch.Create;
  FDirectoryWatch.OnNotify := DirectoryWatchNotify;
  // TEST
  MonitorChanges := True;
{$ENDIF}
  Settings.AddEditorSettingsChangedHandler(EditorSettingsChanged);
  ApplySettings;
end;

procedure TEditorView.BeforeDestruction;
begin
  if Assigned(MasterView) then
  begin
    MasterView.SlaveView := nil;
  end;
  FMasterView  := nil;
  FHighlighter := nil;
  FSelection   := nil;
  if Assigned(Settings) then
    Settings.RemoveEditorSettingsChangedHandler(EditorSettingsChanged);
  DisableAutoSizing;
{$IFDEF Windows}
  FreeAndNil(FDirectoryWatch);
{$ENDIF}
  FreeAndNil(FSynSelection);
  FreeAndNil(FReplaceHistory);
  FreeAndNil(FFindHistory);
  FreeAndNil(FBeautifier);
  FreeAndNil(FSyncronizedEdit);
  FreeAndNil(FTemplateEdit);
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'event handlers' /fold}
procedure TEditorView.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
begin
  if Assigned(FOnDropFiles) then
    FOnDropFiles(Self, FileNames);
  Events.DoChange;
end;

procedure TEditorView.EditorProcessUserCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer);
begin
  Logger.Send(
    'EditorProcessUserCommand(Command = %s; AChar = %s; Data)',
    [EditorCommandToCodeString(Command), AChar]
  );
end;

procedure TEditorView.EditorReplaceText(Sender: TObject; const ASearch: string;
  const AReplace: string; Line, Column: Integer;
  var ReplaceAction: TSynReplaceAction);
begin
  Logger.Send('EditorReplaceText');
end;

function TEditorView.EditorMouseActionExec(AnAction: TSynEditMouseAction;
  var AnInfo: TSynEditMouseActionInfo): Boolean;
begin
  Result := False;
  Logger.Send(
    'EditorMouseActionExec(Action = %s)',
    [AnAction.DisplayName]
  );
  if AnAction.Command in [emcWheelZoomOut, emcWheelZoomIn, emcWheelZoomNorm] then
  begin
    // this is called before the font is actually changed
    FFontChanged := True;
  end;
end;

function TEditorView.EditorMouseActionSearch(
  var AnInfo: TSynEditMouseActionInfo;
  HandleActionProc: TSynEditMouseActionHandler): Boolean;
begin
  Result := False;
  Logger.Send(
    'EditorMouseActionSearch(Action = ?)'
  );
end;

procedure TEditorView.EditorKeyTranslation(Sender: TObject; Code: Word;
  SState: TShiftState; var Data: Pointer; var IsStartOfCombo: Boolean;
  var Handled: Boolean; var Command: TSynEditorCommand;
  FinishComboOnly: Boolean; var ComboKeyStrokes: TSynEditKeyStrokes);
begin
  //
end;

procedure TEditorView.EditorCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer);
begin
  Logger.Send(
    'EditorCommandProcessed(Command = %s; AChar = %s; Data)',
    [EditorCommandToCodeString(Command), AChar]
  );
end;

procedure TEditorView.EditorChangeUpdating(ASender: TObject;
  AUpdating: Boolean);
begin
  Logger.Send(
    'EditorChangeUpdating(AUpdating = %s)',
    [BoolToStr(AUpdating, 'True', 'False')]
  );
  if FFontChanged then
  begin
    Settings.EditorFont.Assign(EditorFont);
    Settings.Apply;
    FFontChanged := False;
  end;
end;

procedure TEditorView.EditorSpecialLineMarkup(Sender: TObject; Line: Integer;
  var Special: Boolean; Markup: TSynSelectedColor);
var
  S : string;
begin
  S := Markup.ToString;
  Logger.Send(
    'EditorSpecialLineMarkup(Line = %d; Special = %s; Markup = %s)',
    [Line, BoolToStr(Special, 'True', 'False'), S]
  );
end;

procedure TEditorView.EditorClearBookmark(Sender: TObject;
  var Mark: TSynEditMark);
var
  S : string;
begin
  S := GetEnumName(TypeInfo(TSynEditMark), Integer(Mark));
  Logger.Send('EditorClearBookmark(Mark = %s)', [S]);
end;

procedure TEditorView.EditorGutterClick(Sender: TObject; X, Y, Line: Integer;
  Mark: TSynEditMark);
//var
//  S : string;
begin
  //try
  //  S := GetEnumName(TypeInfo(TSynEditMark), Integer(Mark));
  //  Logger.Send(
  //    'EditorGutterClick(X = %d; Y = %d; Line = %d; Mark = %s)',
  //    [X, Y, Line, S]
  //  );
  //except
  //  Logger.SendWarning('Logging error on EditorGutterClick exception found');
  //end;
end;

procedure TEditorView.EditorCutCopy(Sender: TObject; var AText: string;
  var AMode: TSynSelectionMode; ALogStartPos: TPoint;
  var AAction: TSynCopyPasteAction);
var
  S : string;
  T : string;
begin
  S := GetEnumName(TypeInfo(TSynSelectionMode), Integer(AMode));
  T := GetEnumName(TypeInfo(TSynCopyPasteAction), Integer(AAction));
  Logger.Send(
    'EditorCutCopy(AText = %s; AMode = %s; ALogStartPos = %s; AAction = %s)',
    [AText, S, Logger.PointToStr(ALogStartPos), T]
  );
end;

procedure TEditorView.EditorMouseLink(Sender: TObject; X, Y: Integer;
  var AllowMouseLink: Boolean);
begin
  Logger.Send(
    'EditorMouseLink(X = %d; Y = %d; AllowMouseLink = %s)',
    [X, Y, BoolToStr(AllowMouseLink, 'True', 'False')]
  );
  AllowMouseLink := True;
end;

procedure TEditorView.EditorSpecialLineColors(Sender: TObject; Line: Integer;
  var Special: Boolean; var FG, BG: TColor);
begin
  // Workaround for SynEdit bug. Needs to be handled in order to let Editor.LineHighlightColor work.
  // 21/09/2013
end;

{ Makes actionlist shortcuts work on the form }

procedure TEditorView.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
begin
  Handled := Actions.ActionList.IsShortCut(Msg);
end;

{$IFDEF Windows}
{ Event triggered when MonitorChanges is True }

procedure TEditorView.DirectoryWatchNotify(const Sender: TObject;
  const AAction: TWatchAction; const FileName: string);
begin
  if SameText(FileName, ExtractFileName(Self.FileName)) and (AAction = waModified) then
  begin
    Load(Self.FileName);
    if CanFocus then
    begin
      Editor.CaretY := Editor.Lines.Count;
      Editor.EnsureCursorPosVisible;
    end;
  end;
end;
{$ENDIF}

procedure TEditorView.EditorClickLink(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Logger.Send('EditorClickLink');
  Commands.OpenFileAtCursor;
end;

procedure TEditorView.EditorPaste(Sender: TObject; var AText: string;
  var AMode: TSynSelectionMode; ALogStartPos: TPoint;
  var AAction: TSynCopyPasteAction);
var
  S : string;
  T : string;
begin
  S := GetEnumName(TypeInfo(TSynSelectionMode), Integer(AMode));
  T := GetEnumName(TypeInfo(TSynCopyPasteAction), Integer(AAction));
  Logger.Send(
    'EditorPaste(AMode = %s; ALogStartPos = %s; AAction = %s)',
    [S, Logger.PointToStr(ALogStartPos), T]
  );
  if (Lines.Count = 0) and Settings.AutoGuessHighlighterType then
    Commands.GuessHighlighterType;
  if (HighlighterName = HL_XML) and Settings.AutoFormatXML then
  begin
    AText := FormatXML(AText);
  end;
end;

procedure TEditorView.EditorChange(Sender: TObject);
begin
  Logger.Send('EditorChange');
  DoChange;
  Events.DoChange;
end;

procedure TEditorView.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if not (csDestroying in ComponentState) then
  begin
    Logger.Send(
      'EditorStatusChange(Changes = %s)',
      [SetToString(TypeInfo(TSynStatusChanges), Changes)]
    );

    // we use this event to ensure that the view is activated because the OnEnter
    // event is not triggered when the form is undocked!

    // but side-effects when it is docked !
    //if not IsActive then
		  //Activate;

    if Assigned(FOnStatusChange) then
      FOnStatusChange(Self, Changes);
    Events.DoStatusChange(Changes);
    if (scCaretX in Changes) or (scCaretY in Changes) then
    begin
      Events.DoCaretPositionChange;
    end;
    if scModified in Changes then
    begin
      Events.DoModified;
    end;
  end;
  //Logger.Send('FirstLineBytePos', SynSelection.FirstLineBytePos);
  //Logger.Send('EndLineBytePos', SynSelection.EndLineBytePos);
  //Logger.Send('StartLinePos', SynSelection.StartLinePos);
  //Logger.Send('EndLinePos', SynSelection.EndLinePos);
  //Logger.Send('StartBytePos', SynSelection.StartBytePos);
  //Logger.Send('EndBytePos', SynSelection.EndBytePos);
  //Logger.Send('LastLineBytePos', SynSelection.LastLineBytePos);

end;

procedure TEditorView.EditorProcessCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer);
begin
  // TODO: dispatch events to manager (cfr. bookmarks)
  Logger.Send(
    'EditorProcessCommand(Command = %s; AChar = %s; Data)',
    [EditorCommandToCodeString(Command), AChar]
  );
end;
{$endregion}

{$region 'event dispatch methods' /fold}
procedure TEditorView.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TEditorView.DoClose(var CloseAction: TCloseAction);
begin
  CloseAction :=  caFree;
  inherited DoClose(CloseAction);
end;
{$endregion}

{$region 'property access methods' /fold}
function TEditorView.GetSelText: string;
begin
  Result := Editor.SelText;
end;

procedure TEditorView.SetSelText(const AValue: string);
begin
  Editor.SelText := AValue;
end;

function TEditorView.GetFoldLevel: Integer;
begin
  Result := FFoldLevel;
end;

procedure TEditorView.SetFoldLevel(const AValue: Integer);
begin
  if AValue <> FoldLevel then
  begin
    FFoldLevel := AValue;
    Editor.FoldAll(FFoldLevel, True);
    Events.DoModified;
  end;
end;

function TEditorView.GetText: string;
begin
  Result := Lines.Text;
end;

procedure TEditorView.SetText(const AValue: string);
begin
  if Lines.Text <> AValue then
  begin
    Lines.Text := AValue;
  end;
end;

function TEditorView.GetCaretX: Integer;
begin
  Result := Editor.CaretX;
end;

procedure TEditorView.SetCaretX(const Value: Integer);
begin
  Editor.CaretX := Value;
end;

function TEditorView.GetCaretY: Integer;
begin
  Result := Editor.CaretY;
end;

procedure TEditorView.SetCaretY(const Value: Integer);
begin
  Editor.CaretY := Value;
end;

function TEditorView.GetEditorFont: TFont;
begin
  Result := Editor.Font;
end;

function TEditorView.GetHighlighterName: string;
begin
  if Assigned(FHighlighterItem) then
    Result := FHighlighterItem.Highlighter
  else
    Result := '';
end;

procedure TEditorView.SetHighlighterName(AValue: string);
begin
  if AValue <> HighlighterName then
  begin
    // HighlighterItem is always assigned
    HighlighterItem := Manager.Highlighters.ItemsByName[AValue];
  end;
  //if no Highlighters defined, we use the number0 Highlighters
  if trim(AValue)='' then
     if Manager.Highlighters.Count>0 then
         HighlighterItem := Manager.Highlighters.Items[0];
end;

function TEditorView.GetInsertMode: Boolean;
begin
  Result := Editor.InsertMode;
end;

procedure TEditorView.SetInsertMode(AValue: Boolean);
begin
  Editor.InsertMode := AValue;
end;

function TEditorView.GetIsFile: Boolean;
begin
  Result := FIsFile;
end;

procedure TEditorView.SetIsFile(AValue: Boolean);
begin
  FIsFile := AValue;
end;

procedure TEditorView.SetEditorFont(AValue: TFont);
begin
  if not Editor.Font.IsEqual(AValue) then
  begin
    Editor.Font.Assign(AValue);
  end;
end;

function TEditorView.GetLines: TStrings;
begin
  Result := Editor.Lines;
end;

procedure TEditorView.SetLines(const Value: TStrings);
begin
  Editor.Lines := Value;
end;


function TEditorView.GetSearchText: string;
begin
  Result := FSearchText;
end;

procedure TEditorView.SetSearchText(const Value: string);
begin
  if Value <> SearchText then
  begin
    FSearchText := Value;
    SetHighlightSearch(Value, SearchOptions);
  end;
end;

function TEditorView.GetTopLine: Integer;
begin
  Result := Editor.TopLine;
end;

procedure TEditorView.SetTopLine(const AValue: Integer);
begin
  Editor.TopLine := AValue;
end;

procedure TEditorView.SetHighlighter(const AValue: TSynCustomHighlighter);
begin
  if AValue <> Highlighter then
  begin
    FHighlighter := AValue;
    Editor.Highlighter := AValue;
  end;
end;

function TEditorView.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TEditorView.SetFileName(const AValue: string);
begin
  if AValue <> FileName then
  begin
    FFileName := AValue;
    Caption := ExtractFileName(AValue);
  end;
end;

function TEditorView.GetEncoding: string;
begin
  Result := FEncoding;
end;

procedure TEditorView.SetEncoding(const AValue: string);
begin
  if AValue <> Encoding then
  begin
    FEncoding := AValue;
    Modified  := True;
  end;
end;

function TEditorView.GetLineBreakStyle: string;
begin
  Result := FLineBreakStyle;
end;

procedure TEditorView.SetLineBreakStyle(const AValue: string);
begin
  if AValue <> LineBreakStyle then
  begin
    FLineBreakStyle := AValue;
    Modified        := True;
  end;
end;

function TEditorView.GetLineText: string;
begin
  Result := Editor.LineText;
end;

procedure TEditorView.SetLineText(const AValue: string);
begin
  Editor.LineText := AValue;
end;

function TEditorView.GetMonitorChanges: Boolean;
begin
{$IFDEF Windows}
  Result := FDirectoryWatch.Running;
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TEditorView.SetMonitorChanges(const AValue: Boolean);
begin
{$IFDEF Windows}
  if AValue <> MonitorChanges then
  begin
    if AValue then
    begin
      if FileExistsUTF8(FileName) then
      begin
        FDirectoryWatch.Directory := ExtractFileDir(FileName);
        FDirectoryWatch.Start;
      end;
    end
    else
      FDirectoryWatch.Stop;
  end;
{$ENDIF}
end;

function TEditorView.GetName: string;
begin
  Result := inherited Name;
end;

procedure TEditorView.SetName(AValue: string);
begin
  inherited Name := AValue;
end;

function TEditorView.GetOnChange: TNotifyEvent;
begin
  Result := FOnChange;
end;

procedure TEditorView.SetOnChange(const AValue: TNotifyEvent);
begin
  FOnChange := AValue;
end;

function TEditorView.GetOnDropFiles: TDropFilesEvent;
begin
  Result := FOnDropFiles;
end;

procedure TEditorView.SetOnDropFiles(const AValue: TDropFilesEvent);
begin
  FOnDropFiles := AValue;
end;

function TEditorView.GetOnStatusChange: TStatusChangeEvent;
begin
  Result := FOnStatusChange;
end;

procedure TEditorView.SetOnStatusChange(const AValue: TStatusChangeEvent);
begin
  FOnStatusChange := AValue;
end;

function TEditorView.GetParent: TWinControl;
begin
  Result := inherited Parent;
end;

procedure TEditorView.SetParent(const AValue: TWinControl);
begin
  inherited Parent := AValue;
  if Assigned(Parent) then
    Visible := True;
end;

function TEditorView.GetPopupMenu: TPopupMenu;
begin
  Result := Editor.PopupMenu;
end;

procedure TEditorView.SetPopupMenu(AValue: TPopupMenu);
begin
  Editor.PopupMenu := AValue;
end;

function TEditorView.GetFoldState: string;
begin
  Result := Editor.FoldState;
end;

procedure TEditorView.SetFoldState(const AValue: string);
begin
  Editor.FoldState := AValue;
end;

function TEditorView.GetLogicalCaretXY: TPoint;
begin
  Result := Editor.LogicalCaretXY;
end;

function TEditorView.GetMasterView: IEditorView;
begin
  Result := FMasterView;
end;

procedure TEditorView.SetMasterView(AValue: IEditorView);
begin
  if AValue <> MasterView then
  begin
    FMasterView := AValue;
    FEditor.ShareTextBufferFrom(FMasterView.Editor);
    FMasterView.SlaveView := Self;
  end;
end;

procedure TEditorView.SetLogicalCaretXY(const AValue: TPoint);
begin
  Editor.LogicalCaretXY := AValue;
end;

function TEditorView.GetModified: Boolean;
begin
  Result := Editor.Modified;
end;

procedure TEditorView.SetModified(const AValue: Boolean);
begin
  if AValue <> Modified then
  begin
    if not AValue then
      Editor.MarkTextAsSaved;
    Editor.Modified := AValue;
  end;
end;

function TEditorView.GetSearchOptions: TSynSearchOptions;
begin
  Result := FSearchOptions;
end;

procedure TEditorView.SetSearchOptions(AValue: TSynSearchOptions);
begin
  FSearchOptions := AValue;
end;

function TEditorView.GetSelEnd: Integer;
begin
  Result := Editor.SelEnd;
end;

procedure TEditorView.SetSelEnd(const AValue: Integer);
begin
  Editor.SelEnd := AValue;
end;

function TEditorView.GetSelStart: Integer;
begin
  Result := Editor.SelStart;
end;

procedure TEditorView.SetSelStart(const AValue: Integer);
begin
  Editor.SelStart := AValue;
end;

function TEditorView.GetSettings: IEditorSettings;
begin
  Result := Owner as IEditorSettings;
end;

function TEditorView.GetActions: IEditorActions;
begin
  Result := Owner as IEditorActions;
end;

function TEditorView.GetCaretXY: TPoint;
begin
  Result := Editor.CaretXY;
end;

procedure TEditorView.SetCaretXY(const AValue: TPoint);
begin
  Editor.CaretXY := AValue;
end;

function TEditorView.GetFindHistory: TStrings;
begin
  Result := FFindHistory;
end;

function TEditorView.GetHighlighterItem: THighlighterItem;
begin
  Result := FHighlighterItem;
end;

procedure TEditorView.SetHighlighterItem(const AValue: THighlighterItem);
begin
  if HighlighterItem <> AValue then
  begin
    FHighlighterItem := AValue;
    if Assigned(AValue) then
    begin
      AValue.Reload;
      Settings.HighlighterType := FHighlighterItem.Highlighter;
      Highlighter := AValue.SynHighlighter;
      Actions.UpdateHighLighterActions;
    end;
    Events.DoHighlighterChange;
  end;
end;

function TEditorView.GetShowSpecialChars: Boolean;
begin
  Result := eoShowSpecialChars in Editor.Options;
end;

function TEditorView.GetSlaveView: IEditorView;
begin
  Result := FSlaveView;
end;

procedure TEditorView.SetSlaveView(AValue: IEditorView);
begin
  if AValue <> SlaveView then
  begin
    FSlaveView := AValue;
  end;
end;

procedure TEditorView.SetShowSpecialChars(const AValue: Boolean);
begin
  if AValue then
    Editor.Options := Editor.Options + [eoShowSpecialChars]
  else
    Editor.Options := Editor.Options - [eoShowSpecialChars];
end;

function TEditorView.GetBlockBegin: TPoint;
begin
  Result := Editor.BlockBegin;
end;

procedure TEditorView.SetBlockBegin(const AValue: TPoint);
begin
  Editor.BlockBegin := AValue;
end;

function TEditorView.GetBlockEnd: TPoint;
begin
  Result := Editor.BlockEnd;
end;

procedure TEditorView.SetBlockEnd(const AValue: TPoint);
begin
  Editor.BlockEnd := AValue;
end;

function TEditorView.GetTextSize: Integer;
begin
  Result := Length(Text);
end;

function TEditorView.GetEvents: IEditorEvents;
begin
  Result := Owner as IEditorEvents;
end;

function TEditorView.GetSupportsFolding: Boolean;
begin
  Result := Assigned(HighlighterItem)
    and Assigned(HighlighterItem.SynHighlighter)
    and (HighlighterItem.SynHighlighter is TSynCustomFoldHighlighter);
end;

function TEditorView.GetSynSelection: TSynEditSelection;
begin
  Result := FSynSelection;
end;

function TEditorView.GetLinesInWindow: Integer;
begin
  Result := Editor.LinesInWindow;
end;

function TEditorView.GetReplaceHistory: TStrings;
begin
  Result := FReplaceHistory;
end;

function TEditorView.GetPreviewText: string;
begin
  if SelAvail then
    Result := SelText
  else
    Result := LineText;
end;

function TEditorView.GetSelAvail: Boolean;
begin
  Result := Editor.SelAvail;
end;

function TEditorView.GetSelection: IEditorSelection;
begin
  Result := FSelection;
end;

function TEditorView.GetSelectionMode: TSynSelectionMode;
begin
  Result := Editor.SelectionMode;
end;

procedure TEditorView.SetSelectionMode(AValue: TSynSelectionMode);
begin
  Editor.DefaultSelectionMode := AValue;
  Editor.SelectionMode := AValue;
end;

function TEditorView.GetCanPaste: Boolean;
begin
  Result := Editor.CanPaste;
end;

function TEditorView.GetCanRedo: Boolean;
begin
  Result := Editor.CanRedo;
end;

function TEditorView.GetCanUndo: Boolean;
begin
  Result := Editor.CanUndo;
end;

function TEditorView.GetForm: TCustomForm;
begin
  Result := Self;
end;

function TEditorView.GetCurrentWord: string;
var
  P: TPoint;
begin
  P := Editor.LogicalCaretXY;
  Result := Editor.GetWordAtRowCol(P);
end;

function TEditorView.GetCommands: IEditorCommands;
begin
  Result := Owner as IEditorCommands;
end;

function TEditorView.GetCurrentChar: WideChar;
begin
  if SelStart < Length(Text) then
    Result := Text[SelStart]
  else
    Result := #0;
end;

function TEditorView.GetEditor: TSynEdit;
begin
  Result := FEditor;
end;

function TEditorView.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;
{$endregion}

{$region 'private methods' /fold}
procedure TEditorView.AssignHighlighterForFileType(const AFileExt: string);
begin
  HighlighterItem := Manager.Highlighters.FindHighlighterForFileType(AFileExt);
  if not Assigned(HighlighterItem) then
  begin
    if Settings.AutoGuessHighlighterType then
    begin
      Commands.GuessHighlighterType;
    end;
    if not Assigned(HighlighterItem) then
      HighlighterName := HL_TXT;
  end
end;

function TEditorView.IsActive: Boolean;
begin
  Result := Manager.ActiveView = (Self as IEditorView);
end;

procedure TEditorView.UpdateSharedViews;
begin
  if Assigned(MasterView) then
  begin
    MasterView.TopLine := TopLine - LinesInWindow;
  end;
  if Assigned(SlaveView) then
  begin
    SlaveView.TopLine := TopLine + LinesInWindow;
  end;
end;

procedure TEditorView.ApplySettings;
begin
  EditorFont                   := Settings.EditorFont;
  ShowSpecialChars             := Settings.EditorOptions.ShowSpecialCharacters;
  Editor.ExtraLineSpacing      := Settings.EditorOptions.ExtraLineSpacing;
  Editor.ExtraCharSpacing      := Settings.EditorOptions.ExtraCharSpacing;
  Editor.BracketHighlightStyle := Settings.EditorOptions.BracketHighlightStyle;
  Editor.BlockTabIndent        := Settings.EditorOptions.BlockTabIndent;
  Editor.BlockIndent           := Settings.EditorOptions.BlockIndent;
  Editor.RightEdge             := Settings.EditorOptions.RightEdge;
  Editor.TabWidth              := Settings.EditorOptions.TabWidth;
  Editor.WantTabs              := Settings.EditorOptions.WantTabs;

  if Settings.EditorOptions.AutoIndent then
    Editor.Options := Editor.Options + [eoAutoIndent]
  else
    Editor.Options := Editor.Options - [eoAutoIndent];

  if Settings.EditorOptions.AutoIndentOnPaste then
    Editor.Options := Editor.Options + [eoAutoIndentOnPaste]
  else
    Editor.Options := Editor.Options - [eoAutoIndentOnPaste];

  if Settings.EditorOptions.SmartTabs then
    Editor.Options := Editor.Options + [eoSmartTabs]
  else
    Editor.Options := Editor.Options - [eoSmartTabs];

  if Settings.EditorOptions.EnhanceHomeKey then
    Editor.Options := Editor.Options + [eoEnhanceHomeKey]
  else
    Editor.Options := Editor.Options - [eoEnhanceHomeKey];

  if Settings.EditorOptions.TabIndent then
    Editor.Options := Editor.Options + [eoTabIndent]
  else
    Editor.Options := Editor.Options - [eoTabIndent];

  if Settings.EditorOptions.TabsToSpaces then
    Editor.Options := Editor.Options + [eoTabsToSpaces]
  else
    Editor.Options := Editor.Options - [eoTabsToSpaces];

  if Settings.EditorOptions.TrimTrailingSpaces then
    Editor.Options := Editor.Options + [eoTrimTrailingSpaces]
  else
    Editor.Options := Editor.Options - [eoTrimTrailingSpaces];

  if Settings.EditorOptions.DragDropEditing then
    Editor.Options := Editor.Options + [eoDragDropEditing]
  else
    Editor.Options := Editor.Options - [eoDragDropEditing];

  if Settings.EditorOptions.BracketHighlight then
    Editor.Options := Editor.Options + [eoBracketHighlight]
  else
    Editor.Options := Editor.Options - [eoBracketHighlight];

  if Settings.EditorOptions.ShowRightEdge then
    Editor.Options := Editor.Options - [eoHideRightMargin]
  else
    Editor.Options := Editor.Options + [eoHideRightMargin];

  if Settings.EditorOptions.EnhanceEndKey then
    Editor.Options2 := Editor.Options2 + [eoEnhanceEndKey]
  else
    Editor.Options2 := Editor.Options2 - [eoEnhanceEndKey];

  if Settings.EditorOptions.CaretSkipsSelection then
    Editor.Options2 := Editor.Options2 + [eoCaretSkipsSelection]
  else
    Editor.Options2 := Editor.Options2 - [eoCaretSkipsSelection];

  if Settings.EditorOptions.CaretSkipsTab then
    Editor.Options2 := Editor.Options2 + [eoCaretSkipTab]
  else
    Editor.Options2 := Editor.Options2 - [eoCaretSkipTab];

  if Settings.EditorOptions.AlwaysVisibleCaret then
    Editor.Options2 := Editor.Options2 + [eoAlwaysVisibleCaret]
  else
    Editor.Options2 := Editor.Options2 - [eoAlwaysVisibleCaret];

  if Settings.EditorOptions.FoldedCopyPaste then
    Editor.Options2 := Editor.Options2 + [eoFoldedCopyPaste]
  else
    Editor.Options2 := Editor.Options2 - [eoFoldedCopyPaste];

  if Settings.EditorOptions.PersistentBlock then
    Editor.Options2 := Editor.Options2 + [eoPersistentBlock]
  else
    Editor.Options2 := Editor.Options2 - [eoPersistentBlock];

  if Settings.EditorOptions.OverwriteBlock then
    Editor.Options2 := Editor.Options2 + [eoOverwriteBlock]
  else
    Editor.Options2 := Editor.Options2 - [eoOverwriteBlock];

  if Settings.EditorOptions.AutoHideCursor then
    Editor.Options2 := Editor.Options2 + [eoAutoHideCursor]
  else
    Editor.Options2 := Editor.Options2 - [eoAutoHideCursor];

  Editor.MouseLinkColor     := Settings.Colors.MouseLinkColor;
  Editor.BracketMatchColor  := Settings.Colors.BracketMatchColor;
  Editor.LineHighlightColor := Settings.Colors.LineHighlightColor;
  Editor.FoldedCodeColor    := Settings.Colors.FoldedCodeColor;
  Editor.HighlightAllColor  := Settings.Colors.HighlightAllColor;
  Editor.SelectedColor      := Settings.Colors.SelectedColor;
  Editor.IncrementColor     := Settings.Colors.IncrementColor;
  Editor.RightEdgeColor     := Settings.Colors.RightEdgeColor;

  Editor.Refresh; // will repaint using the actual highlighter settings

  // alternative block selection color?
  //Editor.UseIncrementalColor := False;
  //Editor.IncrementColor.Background := clLtGray;
  //Editor.IncrementColor.Foreground := clNone;

  // highlight all search matches after search operation
  //Editor.HighlightAllColor.Background := $0064B1FF;  // light orange
  //Editor.HighlightAllColor.FrameColor := $0064B1FF;
  ////Editor.HighlightAllColor.FrameColor := $004683FF;  // dark orange
  //Editor.HighlightAllColor.FrameStyle := slsSolid;
end;

procedure TEditorView.InitializeEditor(AEditor: TSynEdit);
var
  N: Integer;
begin
  AEditor.Parent := Self;
  AEditor.Align := alClient;
  AEditor.Font.Assign(Settings.EditorFont);
  AEditor.BorderStyle := bsNone;
  AEditor.DoubleBuffered := True;

  AEditor.BookMarkOptions.EnableKeys         := True;
  AEditor.BookMarkOptions.GlyphsVisible      := True;
  AEditor.BookMarkOptions.DrawBookmarksFirst := True;
  AEditor.BookMarkOptions.LeftMargin         := -1;
  AEditor.BookMarkOptions.BookmarkImages     := imlBookmarkImages;

  AEditor.Gutter.Color := clWhite;
  AEditor.Gutter.Width := 29;
  AEditor.Gutter.SeparatorPart.Visible := False;

  with AEditor.Gutter.LineNumberPart do
  begin
    Width := 15;
    MarkupInfo.Background := clNone;
    MarkupInfo.Foreground := clGray;
    MarkupInfo.StyleMask := [fsBold];
    DigitCount := 2;
    ShowOnlyLineNumbersMultiplesOf := 10;
    ZeroStart := False;
    LeadingZeros := False;
  end;
  with AEditor.Gutter.ChangesPart do
  begin
    Width := 4;
    ModifiedColor := 59900;
    SavedColor := clGreen;
  end;
  with AEditor.Gutter.CodeFoldPart do
  begin
    MarkupInfo.Background := clNone;
    MarkupInfo.Foreground := clMedGray;
  end;
  with AEditor.Gutter.MarksPart do
  begin
    Width := 1;
    Visible := True;
  end;

  AEditor.Options := [
    eoAltSetsColumnMode,
    eoAutoIndent,        // Will indent the caret on new lines with the same amount of leading white space as the preceding line
    eoAutoIndentOnPaste,
    eoEnhanceHomeKey,   // home key jumps to line start if nearer, similar to visual studio
    eoGroupUndo,       // When undoing/redoing actions, handle all continous changes of the same kind in one call instead undoing/redoing each command separately
    eoHalfPageScroll,   // When scrolling with page-up and page-down commands, only scroll a half page at a time
    eoSmartTabs,        // When tabbing, the cursor will go to the next non-white space character of the previous line
    eoTabIndent,        // When active <Tab> and <Shift><Tab> act as block indent, unindent when text is selected
    eoTabsToSpaces,    // Converts a tab character to a specified number of space characters
    eoTrimTrailingSpaces,  // Spaces at the end of lines will be trimmed and not saved
    eoBracketHighlight, // Highlight matching bracket
    eoShowCtrlMouseLinks,
    eoScrollPastEol,         // makes column selections easier
    eoDragDropEditing,
//    eoPersistentCaret,     // don't use! bug in TSynEdit
    eoShowScrollHint
  ];

  AEditor.Options2 := [
    eoEnhanceEndKey,
    eoFoldedCopyPaste,
    eoOverwriteBlock
  ];
  AEditor.MouseOptions := [
    emAltSetsColumnMode,
    emDragDropEditing,
    emCtrlWheelZoom,
    emShowCtrlMouseLinks
  ];
  AEditor.ScrollBars := ssAutoBoth;

  AEditor.OnChange             := EditorChange;
  AEditor.OnMouseLink          := EditorMouseLink;
  AEditor.OnClickLink          := EditorClickLink;
  AEditor.OnCutCopy            := EditorCutCopy;
  AEditor.OnPaste              := EditorPaste;
  AEditor.OnProcessCommand     := EditorProcessCommand;
  AEditor.OnProcessUserCommand := EditorProcessUserCommand;
  AEditor.OnGutterClick        := EditorGutterClick;
  AEditor.OnClearBookmark      := EditorClearBookmark;
  AEditor.OnSpecialLineMarkup  := EditorSpecialLineMarkup;
  AEditor.OnChangeUpdating     := EditorChangeUpdating;
  AEditor.OnCommandProcessed   := EditorCommandProcessed;
  AEditor.OnReplaceText        := EditorReplaceText;
  AEditor.RegisterMouseActionExecHandler(EditorMouseActionExec);
  AEditor.RegisterMouseActionSearchHandler(EditorMouseActionSearch);
  AEditor.RegisterStatusChangedHandler(
    EditorStatusChange,
    [scCaretX, scCaretY, scLeftChar, scTopLine, scLinesInWindow,
     scCharsInWindow, scInsertMode, scModified, scSelection, scReadOnly]
  );
  AEditor.RegisterKeyTranslationHandler(EditorKeyTranslation);

  // Workaround for SynEdit bug. Needs to be handled in order to let
  // Editor.LineHighlightColor work. 21/09/2013
  AEditor.OnSpecialLineColors := EditorSpecialLineColors;

  AEditor.Visible := True;

  FSyncronizedEdit := TSynPluginSyncroEdit.Create(nil);
  FSyncronizedEdit.Editor := Editor;
  FSyncronizedEdit.Active := False;

  FTemplateEdit := TSynPluginTemplateEdit.Create(nil);
  FTemplateEdit.Editor := Editor;
  FTemplateEdit.Active := False;

  FBeautifier := TSynBeautifier.Create(nil);
  FBeautifier.AutoIndent := True;
  AEditor.Beautifier := FBeautifier;

  // TEMP CODE TS
  N := AEditor.Keystrokes.FindShortcut(TextToShortCut('Shift+Ctrl+N'));
  AEditor.Keystrokes.Delete(N);
  N := AEditor.Keystrokes.FindShortcut(TextToShortCut('Ctrl+N'));
  AEditor.Keystrokes.Delete(N);

  // TEMP CODE FOR MACOS FIX
  {$IFDEF DARWIN}
  //N := AEditor.Keystrokes.FindShortcut(TextToShortCut('Ctrl+C'));
  //AEditor.Keystrokes.Delete(N);
  //N := AEditor.Keystrokes.FindShortcut(TextToShortCut('Ctrl+V'));
  //AEditor.Keystrokes.Delete(N);
  //N := AEditor.Keystrokes.FindShortcut(TextToShortCut('Ctrl+X'));
  //AEditor.Keystrokes.Delete(N);
  //with AEditor.Keystrokes.Add do
  //begin
  //    Key       := ord('C');
  //    Shift     := [ssMeta];
  //    ShiftMask := [];
  //    Command   := ecCopy;
  //end;
  //with AEditor.Keystrokes.Add do
  //begin
  //    Key       := ord('V');
  //    Shift     := [ssMeta];
  //    ShiftMask := [];
  //    Command   := ecPaste;
  //end;
  //with AEditor.Keystrokes.Add do
  //begin
  //    Key       := ord('X');
  //    Shift     := [ssMeta];
  //    ShiftMask := [];
  //    Command   := ecCut;
  //end;
  {$ENDIF}


  //N := AEditor.Keystrokes.FindShortcut(TextToShortCut('Ctrl+S'));
  //AEditor.Keystrokes.Delete(N);
  //N := AEditor.Keystrokes.FindShortcut(TextToShortCut('F1'));
  //AEditor.Keystrokes.Delete(N);

  //FCompletionProposal := TSynCompletionProposal.Create(Self);
  //FCompletionProposal.Editor := Editor;
  //FCompletionProposal.InsertList.Add('Insert');
  //FCompletionProposal.InsertList.Add('List');
  //FCompletionProposal.ItemList.Add('IOnsert');
  //FCompletionProposal.ItemList.Add('IOn');
  //FCompletionProposal.Columns.Add;
  //FCompletionProposal.Resizeable := True;
  //AEditor.MouseActions.FindCommand();
  // delete the quickpaste command with middle mouse button

  // highlights all words that are the same as the one surrounding the caret position
  FMHAC := Editor.MarkupByClass[TSynEditMarkupHighlightAllCaret]
    as TSynEditMarkupHighlightAllCaret;
  FMHAC.MarkupInfo.FrameColor := clSilver;
  FMHAC.MarkupInfo.FrameStyle := slsSolid;
  FMHAC.MarkupInfo.BackAlpha := 128;
  FMHAC.WaitTime := 200;
  FMHAC.MarkupInfo.Background := $00F0F0F0;
  FMHAC.IgnoreKeywords := True;
  FMHAC.SearchOptions := [ssoMatchCase, ssoWholeWord];
  FMHAC.FullWord := True;
  FMHAC.Enabled := True;

  // highlights all strings that are the same as the selected block
  //FMS :=  Editor.MarkupByClass[TSynEditMarkupSelection]
  //  as TSynEditMarkupSelection;
  //FMS.MarkupInfoSeletion.FrameStyle := slsWaved;
  //FMS.MarkupInfo.FrameStyle := slsWaved;
  //FMS.MarkupInfoIncr.FrameStyle := slsWaved;
  //FMS.Enabled := True;

  // highlights all strings that are the same as a particular word (SearchString)
  //FMHA := Editor.MarkupByClass[TSynEditMarkupHighlightAll]
  //  as TSynEditMarkupHighlightAll;
  //  FMHA.MarkupInfo.FrameColor := clBlue;
  //FMHA.MarkupInfo.FrameStyle := slsSolid;
  //FMHA.MarkupInfo.Background := clOlive;
  ////FMHA.SearchString := 'Style';
  //FMHA.SearchOptions := [ssoMatchCase, ssoWholeWord];
  //FMHA.Enabled := True;

          {
   FROM SYNEDIT SOURCES
    // needed before setting color
  fMarkupHighCaret := TSynEditMarkupHighlightAllCaret.Create(self);
  fMarkupHighCaret.Selection := FBlockSelection;
  fMarkupHighAll   := TSynEditMarkupHighlightAll.Create(self);
  fMarkupBracket   := TSynEditMarkupBracket.Create(self);
  fMarkupWordGroup := TSynEditMarkupWordGroup.Create(self);
  fMarkupCtrlMouse := TSynEditMarkupCtrlMouseLink.Create(self);
  fMarkupSpecialLine := TSynEditMarkupSpecialLine.Create(self);
  fMarkupSelection := TSynEditMarkupSelection.Create(self, FBlockSelection);
  fMarkupSpecialChar := TSynEditMarkupSpecialChar.Create(self);

  }
  ActiveControl := Editor;
end;

procedure TEditorView.EditorSettingsChanged(ASender: TObject);
begin
  FUpdate := True;
end;
{$endregion}

{$region 'protected methods' /fold}
{ TODO: store caret pos? }

procedure TEditorView.BeginUpdate;
begin
  //Logger.EnterMethod(Self, 'BeginUpdate');
  Editor.BeginUpdate;
  Editor.BeginUpdateBounds; // TODO investigate this
  Editor.BeginUndoBlock;
  //Logger.ExitMethod(Self, 'BeginUpdate');
end;

procedure TEditorView.EndUpdate;
begin
  //Logger.EnterMethod(Self, 'EndUpdate');
  Editor.EndUndoBlock;
  Editor.EndUpdateBounds; // TODO investigate this
  Editor.EndUpdate;
  //Logger.ExitMethod(Self, 'EndUpdate');
end;

procedure TEditorView.CopyToClipboard;
begin
  Editor.CopyToClipboard;
end;

procedure TEditorView.Cut;
begin
  if Editor.Focused then
  begin
    if not Editor.SelAvail then
      Editor.SelectWord;
    Editor.CutToClipboard;
  end
end;

procedure TEditorView.Copy;
begin
  if Editor.Focused then
  begin
    if not Editor.SelAvail then
      Editor.SelectWord;
    Editor.CopyToClipboard;
  end
end;

procedure TEditorView.Paste;
begin
  if Editor.Focused and CanPaste then;
  begin
    Editor.PasteFromClipboard;
  end;
end;

procedure TEditorView.Undo;
begin
  Editor.Undo;
end;

procedure TEditorView.Redo;
begin
  Editor.Redo;
end;

{ Make current editor instance the active one in the editor manager. This does
  not automatically make it focused as the current focus can be set to eg. a
  tool window. }

procedure TEditorView.Activate;
begin
  inherited;
  Manager.ActiveView := Self as IEditorView;
end;

function TEditorView.EditorViewFocused: Boolean;
begin
  Result := Focused or Editor.Focused;
end;

procedure TEditorView.SearchAndSelectLine(ALineIndex: Integer; const ALine: string);
begin
  try
    Editor.SearchReplaceEx(ALine, '', [ssoWholeWord], Point(0, ALineIndex));
  except
    // don't handle exceptions
  end;
end;

procedure TEditorView.SearchAndSelectText(const AText: string);
begin
  try
    Editor.SearchReplaceEx(AText, '', [], Point(0, 0));
  except
    // don't handle exceptions
  end;
end;

procedure TEditorView.SelectWord;
begin
  Editor.SelectWord;
end;

{ Clears all highlighted search matches of the last search operation. }

procedure TEditorView.ClearHighlightSearch;
begin
  SetHighlightSearch('', []);
end;

procedure TEditorView.Clear;
begin
  Editor.ClearAll;
end;

procedure TEditorView.SelectAll;
begin
  if Editor.Focused then
    Editor.SelectAll
end;

procedure TEditorView.FindNextWordOccurrence(ADirectionForward: Boolean);
var
  StartX, EndX: Integer;
  Flags: TSynSearchOptions;
  LogCaret: TPoint;
begin
  StartX := 0;
  EndX   := Editor.MaxLeftChar;
  LogCaret := LogicalCaretXY;
  Editor.GetWordBoundsAtRowCol(LogCaret, StartX, EndX);
  if EndX <= StartX then
    Exit;
  Flags := [ssoWholeWord];
  if ADirectionForward then
  begin
    LogCaret.X := EndX;
  end
  else
  begin
    LogCaret.X := StartX;
    Include(Flags, ssoBackwards);
  end;
  LogicalCaretXY := LogCaret;
  Editor.SearchReplace(Editor.GetWordAtRowCol(LogCaret), '', Flags);
end;

procedure TEditorView.SetHighlightSearch(const ASearch: string; AOptions: TSynSearchOptions);
begin
  try
    Editor.SetHighlightSearch(ASearch, AOptions);
  except
    // TO TEST
  end;
end;

procedure TEditorView.UpdateActions;
var
  B: Boolean;
begin
  inherited UpdateActions;
  B := Focused;
  if not B and Assigned(Parent) then
  begin
    if Parent.Focused then
      B := True;
  end;

  if B then
  begin
    Activate;
  end;

  if IsActive then
  begin
    Editor.Color := clWhite;
    UpdateSharedViews;
  end
  else
  begin
    if Settings.DimInactiveView then
      Editor.Color := GetHighLightColor(15329769, 10);
  end;

  if Assigned(Actions) then
    Actions.UpdateActions;

  if FUpdate then
  begin
    ApplySettings;
    FUpdate := False;
  end;
end;

{$endregion}

{$region 'public methods' /fold}
function TEditorView.CloseQuery: Boolean;
var
  MR: TModalResult;
  S : string;
  V : IEditorView;
begin
  V := nil;
  Result := inherited CloseQuery;
  if Modified then
  begin
    if Manager.ActiveView <> (Self as IEditorView) then
    begin
      V := Manager.ActiveView;
      Activate;
    end;
    S := Format(SAskSaveChanges, [FileName]);
    MR := MessageDlg(S, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if MR = mrYes then
    begin
      Result := Manager.SaveFile;
    end
    else if MR = mrNo then
    begin
      Result := True;
    end
    else
    begin
      // TODO: Why can't we prevent closing by setting Result to False?
      Result := False;
      Abort;
    end;
  end;
  if Assigned(V) then
  begin
    V.Activate;
  end;
end;

{  When IsFile is true this loads the given filenameinto the editor view. When
  IsFile is false, the given storagename is passed to an event which can be
  handled by the owning application to load the content from another resource
  like eg. a database table. }

procedure TEditorView.Load(const AStorageName: string);
var
  S  : string;
  FS : TFileStreamUTF8;
begin
  Events.DoLoad(AStorageName);
  if IsFile then
  begin
    if (AStorageName <> '') and FileExistsUTF8(AStorageName) then
      FileName := AStorageName;

    FS := TFileStreamUTF8.Create(FileName, fmOpenRead + fmShareDenyNone);
    try
      LoadFromStream(FS);
    finally
      FreeAndNil(FS);
    end;
    LineBreakStyle := ALineBreakStyles[GuessLineBreakStyle(Text)];
    S := ExtractFileExt(FileName);
    S := System.Copy(S, 2, Length(S));
    try
      if FileIsText(FileName) then
      begin
        AssignHighlighterForFileType(S);
      end;
    except
      { TODO -oTS : dirty: need to fix this }
      // for an unknown reason an EAbort is raised
    end;
    Modified := False;
  end;
end;

{ The loaded text always has to be converted to UTF8 because SynEdit only
  supports UTF8. When we save we convert back to the original file encoding. }

procedure TEditorView.LoadFromStream(AStream: TStream);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromStream(AStream);    ;
    FEncoding := GuessEncoding(SL.Text);
    if FEncoding <> EncodingUTF8 then
    begin
      Text := ConvertEncoding(SL.Text, FEncoding, EncodingUTF8);
    end
    else
      Text := SL.Text;
    FLineBreakStyle := ALineBreakStyles[GuessLineBreakStyle(Text)];
  finally
    FreeAndNil(SL);
  end;
end;

procedure TEditorView.SaveToStream(AStream: TStream);
var
  S  : string;
begin
  if LineBreakStyle <> ALineBreakStyles[GuessLineBreakStyle(Text)] then
  begin
    Text := ChangeLineBreakStyle(Text, StrToLineBreakStyle(LineBreakStyle));
  end;
  if Length(Text) > 0 then
  begin
    S := Text;
    S := ConvertEncoding(Text, EncodingUTF8, FEncoding);
    AStream.Write(S[1], Length(S));
  end;
end;

procedure TEditorView.Save(const AStorageName: string);
var
  FS : TFileStream;
begin
  Events.DoBeforeSave(AStorageName);
  if IsFile then
  begin
    FS := TFileStream.Create(AStorageName, fmCreate);
    try
      SaveToStream(FS);
    finally
      FreeAndNil(FS);
    end;
  end;
  Events.DoAfterSave(AStorageName);
  Modified := False;
end;

function TEditorView.GetWordAtPosition(const APosition: TPoint): string;
var
  CaretPos: TPoint;
begin
  Result := '';
  Caretpos := Editor.PhysicalToLogicalPos(APosition);
  Result := GetWordFromCaret(CaretPos);
end;

function TEditorView.GetWordFromCaret(const ACaretPos: TPoint): string;
begin
  Result := Editor.GetWordAtRowCol(ACaretPos);
end;

function TEditorView.GetHighlighterAttriAtRowCol(APosition: TPoint;
  out AToken: string; out AAttri: TSynHighlighterAttributes): Boolean;
begin
  Result := Editor.GetHighlighterAttriAtRowCol(APosition, AToken, AAttri);
end;
{$endregion}

end.

{$region 'Keyboard shortcuts' /fold}
(*//F1                      Topic Search
//Ctrl+F1                Topic Search
  ecNextEditor: SetResult(VK_F6,[]);
  ecPrevEditor: SetResult(VK_F6,[ssShift]);
  ecWordLeft:   SetResult(VK_A,[ssCtrlOS],VK_LEFT,[ssCtrlOS]);
  ecPageDown:   SetResult(VK_C,[ssCtrlOS],VK_NEXT,[]);
//Ctrl+D                 Moves the cursor right one column, accounting for the
//autoindent setting
//Ctrl+E                 Moves the cursor up one line
//Ctrl+F                 Moves one word right
//Ctrl+G                 Deletes the character to the right of the cursor
//Ctrl+H                 Deletes the character to the left of the cursor
//Ctrl+I                  Inserts a tab
//Ctrl+L                 Search|Search Again
//Ctrl+N                 Inserts a new line
//Ctrl+P                 Causes next character to be interpreted as an ASCII
//sequence
//Ctrl+R                 Moves up one screen
//Ctrl+S                 Moves the cursor left one column, accounting for the
//autoindent setting
//Ctrl+T                 Deletes a word
//Ctrl+V                 Turns insert mode on/off
//Ctrl+W                Moves down one screen
//Ctrl+X                 Moves the cursor down one line
//Ctrl+Y                 Deletes a line
//Ctrl+Z                 Moves the cursor up one line
//Ctrl+Shift+S          Performs an incremental search

//Block commands:
//---------------
//Ctrl+K+B      Marks the beginning of a block
//Ctrl+K+C      Copies a selected block
//Ctrl+K+H      Hides/shows a selected block
//Ctrl+K+I       Indents a block by the amount specified in the Block Indent
//combo box on the General page of the Editor Options dialog box.
//Ctrl+K+K      Marks the end of a block
//Ctrl+K+L       Marks the current line as a block
//Ctrl+K+N      Changes a block to uppercase
//Ctrl+K+O      Changes a block to lowercase
//Ctrl+K+P      Prints selected block
//Ctrl+K+R      Reads a block from a file
//Ctrl+K+T       Marks a word as a block
//Ctrl+K+U      Outdents a block by the amount specified in the Block Indent
//combo box on the General page of the Editor Options dialog box.
//Ctrl+K+V      Moves a selected block
//Ctrl+K+W      Writes a selected block to a file
//Ctrl+K+Y      Deletes a selected block
//Ctrl+O+C      Turns on column blocking
//Ctrl+O+I       Marks an inclusive block
//Ctrl+O+K      Turns off column blocking
//Ctrl+O+L      Marks a line as a block
//Shift+Alt+arrow Selects column-oriented blocks
//Click+Alt+mousemv Selects column-oriented blocks
//Ctrl+Q+B      Moves to the beginning of a block
//Ctrl+Q+K      Moves to the end of a block

//Miscellaneous commands:
//-----------------------
//Ctrl+K+D      Accesses the menu bar
//Ctrl+K+E       Changes a word to lowercase
//Ctrl+K+F       Changes a word to uppercase
//Ctrl+K+S      File|Save (Default and IDE Classic only)
//Ctrl+Q+A      Search|Replace
//Ctrl+Q+F      Search|Find
//Ctrl+Q+Y      Deletes to the end of a line
//Ctrl+Q+[       Finds the matching delimiter (forward)
//Ctrl+Q+Ctrl+[ Finds the matching delimiter (forward)
//Ctrl+Q+]       Finds the matching delimiter (backward)
//Ctrl+Q+Ctrl+] Finds the matching delimiter (backward)
//Ctrl+O+A      Open file at cursor
//Ctrl+O+B      Browse symbol at cursor (Delphi only)
//Alt+right arrow  For code browsing
//Alt +left arrow For code browsing
//Ctrl+O+G      Search|Go to line number
//Ctrl+O+O      Inserts compiler options and directives
//Ctrl+O+U      Toggles case
//Bookmark commands:
//------------------
//Shortcut       Action
//Ctrl+K+0       Sets bookmark 0
//Ctrl+K+1       Sets bookmark 1
//Ctrl+K+2       Sets bookmark 2
//Ctrl+K+3       Sets bookmark 3
//Ctrl+K+4       Sets bookmark 4
//Ctrl+K+5       Sets bookmark 5
//Ctrl+K+6       Sets bookmark 6
//Ctrl+K+7       Sets bookmark 7
//Ctrl+K+8       Sets bookmark 8
//Ctrl+K+9       Sets bookmark 9
//Ctrl+K+Ctrl+0 Sets bookmark 0
//Ctrl+K+Ctrl+1 Sets bookmark 1
//Ctrl+K+Ctrl+2 Sets bookmark 2
//Ctrl+K+Ctrl+3 Sets bookmark 3
//Ctrl+K+Ctrl+4 Sets bookmark 4
//Ctrl+K+Ctrl+5 Sets bookmark 5
//Ctrl+K+Ctrl+6 Sets bookmark 6
//Ctrl+K+Ctrl+7 Sets bookmark 7
//Ctrl+K+Ctrl+8 Sets bookmark 8
//Ctrl+K+Ctrl+9 Sets bookmark 9
//Ctrl+Q+0       Goes to bookmark 0
//Ctrl+Q+1       Goes to bookmark 1
//Ctrl+Q+2       Goes to bookmark 2
//Ctrl+Q+3       Goes to bookmark 3
//Ctrl+Q+4       Goes to bookmark 4
//Ctrl+Q+5       Goes to bookmark 5
//Ctrl+Q+6       Goes to bookmark 6
//Ctrl+Q+7       Goes to bookmark 7
//Ctrl+Q+8       Goes to bookmark 8
//Ctrl+Q+9       Goes to bookmark 9
//Ctrl+Q+Ctrl+0 Goes to bookmark 0
//Ctrl+Q+Ctrl+1 Goes to bookmark 1
//Ctrl+Q+Ctrl+2 Goes to bookmark 2
//Ctrl+Q+Ctrl+3 Goes to bookmark 3
//Ctrl+Q+Ctrl+4 Goes to bookmark 4
//Ctrl+Q+Ctrl+5 Goes to bookmark 5
//Ctrl+Q+Ctrl+6 Goes to bookmark 6
//Ctrl+Q+Ctrl+7 Goes to bookmark 7
//Ctrl+Q+Ctrl+8 Goes to bookmark 8
//Ctrl+Q+Ctrl+9 Goes to bookmark 9
//Cursor movement:
//----------------
//Ctrl+Q+B      Moves to the beginning of a block
//Ctrl+Q+C      Moves to end of a file
//Ctrl+Q+D      Moves to the end of a line
//Ctrl+Q+E      Moves the cursor to the top of the window
//Ctrl+Q+K      Moves to the end of a block
//Ctrl+Q+P      Moves to previous position
//Ctrl+Q+R      Moves to the beginning of a file
//Ctrl+Q+S      Moves to the beginning of a line
//Ctrl+Q+T      Moves the viewing editor so that the current line is placed at
//the top of the window
//Ctrl+Q+U      Moves the viewing editor so that the current line is placed at
//the bottom of the window, if possible
//Ctrl+Q+X      Moves the cursor to the bottom of the window
//System keys:
//------------

//F1              Displays context-sensitive Help
//F2              File|Save
//F3              File|Open
//F4              Run to Cursor
//F5              Zooms window
//F6              Displays the next page
//F7              Run|Trace Into
//F8              Run|Step Over
//F9              Run|Run
//F11             View|Object Inspector
//F12             View|Toggle Form/Unit
//Alt+0           View|Window List
//Alt+F2          View|CPU
//Alt+F3          File|Close
//Alt+F7          Displays previous error in Message view
//Alt+F8          Displays next error in Message view
//Alt+F11        File|Use Unit (Delphi)
//Alt+F11        File|Include Unit Hdr (C++)
//Alt+F12        Displays the Code editor
//Alt+X           File|Exit
//Alt+right arrow  For code browsing forward
//Alt +left arrow For code browsing backward
//Alt +up arrow  For code browsing Ctrl-click on identifier
//Alt+Page Down Goes to the next tab
//Alt+Page Up   Goes to the previous tab
//Ctrl+F1        Topic Search
//Ctrl+F2        Run|Program Reset
//Ctrl+F3        View|Call Stack
//Ctrl+F6        Open Source/Header file (C++)
//Ctrl+F7        Add Watch at Cursor
//Ctrl+F8        Toggle Breakpoint
//Ctrl+F9        Project|Compile project (Delphi)
//Ctrl+F9        Project|Make project (C++)
//Ctrl+F11       File|Open Project
//Ctrl+F12       View|Units
//Shift+F7       Run|Trace To Next Source Line
//Shift+F11      Project|Add To Project
//Shift+F12      View|Forms
//Ctrl+D         Descends item (replaces Inspector window)
//Ctrl+N         Opens a new Inspector window
//Ctrl+S          Incremental search
//Ctrl+T          Displays the Type Cast dialog
  else
    GetDefaultKeyForCommand(Command,TheKeyA,TheKeyB);
  end;
*)
{$endregion}
