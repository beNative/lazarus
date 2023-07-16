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

unit ts.HtmlEditor.Interfaces;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Menus, ActnList, Graphics,

  ts.Core.Types;

{ All supported actions by the editor views, and holds a collection of all
  registered views. }
type
  IHtmlEditorActions   = interface;
  IHtmlEditorToolView  = interface;
  IHtmlEditorToolViews = interface;

  THtmlEditorToolViewEvent = procedure(
    Sender              : TObject;
    AHtmlEditorToolView : IHtmlEditorToolView
  ) of object;

  IHtmlEditorView = interface
  ['{9F4CFE82-8E45-44FB-806E-015C70FF604F}']
    {$REGION 'property access methods'}
    function GetActions: IHtmlEditorActions;
    function GetAlignCenter: Boolean;
    function GetAlignJustify: Boolean;
    function GetAlignLeft: Boolean;
    function GetAlignRight: Boolean;
    function GetAllowHostObjects: Boolean;
    function GetBullets: Boolean;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetContentSize: Int64;
    function GetDefaultContextMenusEnabled: Boolean;
    function GetDefaultScriptDialogsEnabled: Boolean;
    function GetDevToolsEnabled: Boolean;
    function GetEditMode: Boolean;
    function GetFileName: string;
    function GetFont: TFont;
    function GetForm: TCustomForm;
    function GetHtmlText: string;
    function GetIsEmpty: Boolean;
    function GetIsFile: Boolean;
    function GetIsInitialized: Boolean;
    function GetIsNavigating: Boolean;
    function GetModified: Boolean;
    function GetOffline: Boolean;
    function GetOnAfterCreated: TNotifyEvent;
    function GetOnChange: TNotifyEvent;
    function GetOnDropFiles: TDropFilesEvent;
    function GetPopupMenu: TPopupMenu;
    function GetScriptEnabled: Boolean;
    function GetSelAvail: Boolean;
    function GetSelText: string;
    function GetSourceVisible: Boolean;
    function GetStatusBarEnabled: Boolean;
    function GetText: string;
    function GetSource: string;
    function GetWebMessageEnabled: Boolean;
    function GetZoomControlEnabled: Boolean;
    procedure SetAlignCenter(AValue: Boolean);
    procedure SetAlignJustify(AValue: Boolean);
    procedure SetAlignLeft(AValue: Boolean);
    procedure SetAlignRight(AValue: Boolean);
    procedure SetAllowHostObjects(AValue: Boolean);
    procedure SetBullets(AValue: Boolean);
    procedure SetDefaultContextMenusEnabled(AValue: Boolean);
    procedure SetDefaultScriptDialogsEnabled(AValue: Boolean);
    procedure SetDevToolsEnabled(AValue: Boolean);
    procedure SetEditMode(AValue: Boolean);
    procedure SetFileName(const AValue: string);
    procedure SetHtmlText(const AValue: string);
    procedure SetIsFile(AValue: Boolean);
    procedure SetModified(const AValue: Boolean);
    procedure SetOffline(AValue: Boolean);
    procedure SetOnAfterCreated(AValue: TNotifyEvent);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TDropFilesEvent);
    procedure SetParent(NewParent: TWinControl);
    procedure SetPopupMenu(const AValue: TPopupMenu);
    procedure SetScriptEnabled(AValue: Boolean);
    procedure SetSelText(const AValue: string);
    procedure SetSourceVisible(AValue: Boolean);
    procedure SetStatusBarEnabled(AValue: Boolean);
    procedure SetText(const AValue: string);
    procedure SetSource(AValue: string);
    procedure SetWebMessageEnabled(AValue: Boolean);
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

    procedure ShowDevTools;
    procedure ShowTaskManager;

    procedure BeginUpdate;
    procedure EndUpdate;

    function IsUpdating: Boolean;

    function ExecuteScript(const AScript: string): Boolean;
    function ClearCache: Boolean;
    function Refresh: Boolean;

    function InsertImage: Boolean; overload;
    procedure InsertImageFile(const AFileName: string);
    procedure InsertImage(AImage: TPicture); overload;
    procedure InsertHyperlink(
      const AText : string = '';
      const AURL  : string = ''
    );
    procedure CreateBulletList;
    procedure CreateNumberedList;
    procedure AddParagraph;

    procedure IncIndent;
    procedure DecIndent;

    procedure Clear;

    // clipboard commands
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure SetZoomControlEnabled(AValue: Boolean);

    procedure Undo;
    procedure Redo;

    {$REGION 'Webview properties'}
    property DefaultContextMenusEnabled: Boolean
      read GetDefaultContextMenusEnabled write SetDefaultContextMenusEnabled;

    property DefaultScriptDialogsEnabled: Boolean
      read GetDefaultScriptDialogsEnabled write SetDefaultScriptDialogsEnabled;

    property DevToolsEnabled: Boolean
      read GetDevToolsEnabled write SetDevToolsEnabled;

    property AllowHostObjects: Boolean
      read GetAllowHostObjects write SetAllowHostObjects;

    property Offline: Boolean
      read GetOffline write SetOffline;

    property ScriptEnabled: Boolean
      read GetScriptEnabled write SetScriptEnabled;

    property StatusBarEnabled: Boolean
      read GetStatusBarEnabled write SetStatusBarEnabled;

    property WebMessageEnabled: Boolean
      read GetWebMessageEnabled write SetWebMessageEnabled;

    property ZoomControlEnabled: Boolean
      read GetZoomControlEnabled write SetZoomControlEnabled;

    property IsNavigating: Boolean
      read GetIsNavigating;
  {$ENDREGION}

    // properties
    property ContentSize: Int64
      read GetContentSize;

    property SourceVisible: Boolean
      read GetSourceVisible write SetSourceVisible;

    property Bullets: Boolean
      read GetBullets write SetBullets;

    property EditMode: Boolean
      read GetEditMode write SetEditMode;

    property Form: TCustomForm
      read GetForm;

    property IsInitialized: Boolean
      read GetIsInitialized;

    property Actions: IHtmlEditorActions
      read GetActions;

    property CanPaste: Boolean
      read GetCanPaste;

    property CanUndo: Boolean
      read GetCanUndo;

    property CanRedo: Boolean
      read GetCanRedo;

    property FileName: string
      read GetFileName write SetFileName;

    { Returns True if the view doesn't contain any data. }
    property IsEmpty: Boolean
      read GetIsEmpty;

    property IsFile: Boolean
      read GetIsFile write SetIsFile;

    property HtmlText: string
      read GetHtmlText write SetHtmlText;

    property Source: string
      read GetSource write SetSource;

    property Text: string
      read GetText write SetText;

    property SelText: string
      read GetSelText write SetSelText;

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

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

    property OnDropFiles: TDropFilesEvent
      read GetOnDropFiles write SetOnDropFiles;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;

    property OnAfterCreated: TNotifyEvent
      read GetOnAfterCreated write SetOnAfterCreated;
  end;

  { Events dispatched by the editor view. }

  IHtmlEditorEvents = interface
  ['{ACDD3582-34F7-4634-B3E6-197CB25AF499}']
    {$REGION 'property access mehods'}
    function GetOnAfterSave: TStorageEvent;
    function GetOnBeforeSave: TStorageEvent;
    function GetOnHideToolView: THtmlEditorToolViewEvent;
    function GetOnLoad: TStorageEvent;
    function GetOnNew: TNewEvent;
    function GetOnOpen: TStorageEvent;
    function GetOnShowToolView: THtmlEditorToolViewEvent;
    procedure SetOnAfterSave(AValue: TStorageEvent);
    procedure SetOnBeforeSave(AValue: TStorageEvent);
    procedure SetOnHideToolView(AValue: THtmlEditorToolViewEvent);
    procedure SetOnShowToolView(AValue: THtmlEditorToolViewEvent);
    procedure SetOnLoad(AValue: TStorageEvent);
    procedure SetOnNew(AValue: TNewEvent);
    procedure SetOnOpen(AValue: TStorageEvent);
    {$ENDREGION}
    procedure AddOnChangeHandler(AEvent: TNotifyEvent);
    procedure AddOnModifiedHandler(AEvent: TNotifyEvent);
    procedure RemoveOnChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveOnModifiedHandler(AEvent: TNotifyEvent);
    procedure AddOnContentLoadedHandler(AEvent: TNotifyEvent);
    procedure RemoveOnContentLoadedHandler(AEvent: TNotifyEvent);
    procedure AddOnSourceChangedHandler(AEvent: TNotifyEvent);
    procedure RemoveOnSourceChangedHandler(AEvent: TNotifyEvent);

    // event dispatch methods
    procedure DoChange;
    procedure DoModified;
    procedure DoContentLoaded;
    procedure DoSourceChanged;
    procedure DoOpen(const AName: string);
    procedure DoBeforeSave(const AName: string);
    procedure DoAfterSave(const AName: string);
    procedure DoLoad(const AName: string);
    procedure DoNew(
      const AName : string = '';
      const AText : string = ''
    );
    procedure DoShowToolView(AToolView: IHtmlEditorToolView);
    procedure DoHideToolView(AToolView: IHtmlEditorToolView);

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

    property OnShowToolView: THtmlEditorToolViewEvent
      read GetOnShowToolView write SetOnShowToolView;

    property OnHideToolView : THtmlEditorToolViewEvent
      read GetOnHideToolView write SetOnHideToolView;
  end;

  IHtmlEditorActions = interface
  ['{101D9E15-2698-4346-9715-D35B490CAFE6}']
    {$REGION 'property access methods'}
    function GetActionList: TActionList;
    function GetItem(AName: string): TContainedAction;
    {$ENDREGION}

    procedure UpdateActions;

    property Items[AName: string]: TContainedAction
      read GetItem; default;

    property ActionList: TActionList
      read GetActionList;
  end;

  IHtmlEditorManager = interface
  ['{A1781DE6-B022-4DBA-9D06-327E4612F65A}']
    {$REGION 'property access methods'}
    function GetActionList: TActionList;
    function GetActions: IHtmlEditorActions;
    function GetActiveView: IHtmlEditorView;
    function GetEditorPopupMenu: TPopupMenu;
    function GetEvents: IHtmlEditorEvents;
    function GetToolViews: IHtmlEditorToolViews;
    function GetView(AIndex: Integer): IHtmlEditorView;
    function GetViewByName(AName: string): IHtmlEditorView;
    function GetViewCount: Integer;
    procedure SetActiveView(const AValue: IHtmlEditorView);
    {$ENDREGION}

    function AddView(
      const AName     : string = '';
      const AFileName : string = ''
    ): IHtmlEditorView;
    function DeleteView(AIndex: Integer): Boolean;
    procedure ClearViews;

    property ActionList: TActionList
      read GetActionList;

    property Actions: IHtmlEditorActions
      read GetActions;

    property ActiveView: IHtmlEditorView
      read GetActiveView write SetActiveView;

    property EditorPopupMenu: TPopupMenu
      read GetEditorPopupMenu;

    property Events: IHtmlEditorEvents
      read GetEvents;

    property ToolViews: IHtmlEditorToolViews
      read GetToolViews;

    property Views[AIndex: Integer]: IHtmlEditorView
      read GetView;

    property ViewByName[AName: string]: IHtmlEditorView
      read GetViewByName;

    property ViewCount: Integer
      read GetViewCount;
  end;

  IHtmlEditorToolbarsFactory = interface
  ['{447AEEC2-9A28-4E76-BF92-BEBA3185B01B}']
    function CreateMainToolbar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolbar;
  end;

  IHtmlEditorMenusFactory = interface
  ['{F8E628CE-DAB1-4D8E-944F-F6F69985E05C}']
    function CreateMainMenu(
      AOwner : TComponent
    ): TMainMenu;
  end;

  IHtmlEditorToolView = interface
  ['{5EB417D5-07AB-441F-8E8D-72F1E36F8736}']
    {$REGION 'property access methods'}
    function GetForm: TForm;
    function GetName: string;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    {$ENDREGION}
    { Lets the view respond to changes. }
    procedure UpdateView;

    procedure SetFocus;
    function Focused: Boolean;

    property Visible: Boolean
      read GetVisible write SetVisible;

    property Name: string
      read GetName;

    property Form: TForm
      read GetForm;
  end;

  THtmlEditorToolViewListEnumerator = class
  strict private
    FIndex : Integer;
    FList  : IHtmlEditorToolViews;

  public
    constructor Create(AList: IHtmlEditorToolViews);
    destructor Destroy; override;

    function GetCurrent: IHtmlEditorToolView;
    function MoveNext: Boolean;

    property Current: IHtmlEditorToolView
      read GetCurrent;
  end;

  IHtmlEditorToolViews = interface
  ['{C76482DF-8875-4D25-9FA6-CE6E538E954E}']
  {$REGION 'property access methods'}
  function GetView(AIndex: Integer): IHtmlEditorToolView;
  function GetViewByName(AName: string): IHtmlEditorToolView;
  function GetCount: Integer;
  {$ENDREGION}

  function GetEnumerator: THtmlEditorToolViewListEnumerator;

  function Register(
    AFormClass     : TComponentClass;
    ASettingsClass : TComponentClass;
    const AName    : string = ''
  ): Boolean;
  procedure Hide;

  property Views[AIndex: Integer]: IHtmlEditorToolView
    read GetView;

  property ViewByName[AName: string]: IHtmlEditorToolView
    read GetViewByName; default;

  property Count: Integer
    read GetCount;
  end;

implementation

{$REGION 'THtmlEditorToolViewListEnumerator'}
constructor THtmlEditorToolViewListEnumerator.Create(AList: IHtmlEditorToolViews
  );
begin
  FList  := AList;
  FIndex := -1;
end;

destructor THtmlEditorToolViewListEnumerator.Destroy;
begin
  FList := nil;
  inherited Destroy;
end;

function THtmlEditorToolViewListEnumerator.GetCurrent: IHtmlEditorToolView;
begin
  Result := FList.Views[FIndex] as IHtmlEditorToolView;
end;

function THtmlEditorToolViewListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < (FList.Count - 1);
  if Result then
    Inc(FIndex);
end;
{$ENDREGION}

end.

