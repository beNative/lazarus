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

unit ts.HtmlEditor.View;

{$MODE DELPHI}

interface

{
  REMARKS Delphi vs. FPC:
  In Delphi string = UnicodeString while in the RTL, the FCL and the LCL string
  = AnsiString(CP_ACP). So in Delphi string is UTF16 encoded and in FPC it
  is UTF8 encoded.
  Especially the RTL is not ready for string = UnicodeString. So your best
  bet is to use UTF8String or set the default code page to UTF8 (the LCL
  units do that by default.
}

uses
  LCLIntf, LCLType, LMessages, Messages, Classes, SysUtils, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, Menus, StdCtrls, ActiveX,

  uWVBrowser, uWVWindowParent, uWVLoader, uWVTypes, uWVEvents, uWVTypeLibrary,
  uWVCoreWebView2DownloadOperation,

  DropComboTarget,

  ts.HtmlEditor.Interfaces, Types, DropTarget, DragDropInternet, DragDropFile;

type
  THtmlEditorView = class(TForm, IHtmlEditorView)
    dctMain             : TDropComboTarget;
    edtSource           : TEdit;
    imgFavIcon          : TImage;
    pnlTop              : TPanel;
    pnlHtmlEditor       : TPanel;
    tmrCreateWebBrowser : TTimer;
    WVBrowser           : TWVBrowser;
    WVWindowParent      : TWVWindowParent;

    {$REGION 'event handlers'}
    procedure dctMainDrop(
      Sender     : TObject;
      ShiftState : TShiftState;
      APoint     : TPoint;
      var Effect : Longint
    );
    procedure edtSourceExit(Sender: TObject);
    procedure edtSourceMouseEnter(Sender: TObject);
    procedure edtSourceMouseLeave(Sender: TObject);
    procedure edtSourceEditingDone(Sender: TObject);
    procedure tmrCreateWebBrowserTimer(Sender: TObject);
    procedure WVBrowserAcceleratorKeyPressed(
      Sender            : TObject;
      const AController : ICoreWebView2Controller;
      const AArgs       : ICoreWebView2AcceleratorKeyPressedEventArgs
    );
    procedure WVBrowserAddScriptToExecuteOnDocumentCreatedCompleted(
      Sender     : TObject;
      AErrorCode : HRESULT;
      const AID  : wvstring
    );
    procedure WVBrowserAfterCreated(Sender: TObject);
    procedure WVBrowserBasicAuthenticationRequested(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2BasicAuthenticationRequestedEventArgs
    );
    procedure WVBrowserBrowserProcessExited(
      Sender             : TObject;
      const AEnvironment : ICoreWebView2Environment;
      const AArgs        : ICoreWebView2BrowserProcessExitedEventArgs
    );
    procedure WVBrowserBytesReceivedChanged(
      Sender                   : TObject;
      const ADownloadOperation : ICoreWebView2DownloadOperation;
      ADownloadID              : Integer
    );
    procedure WVBrowserCallDevToolsProtocolMethodCompleted(
      Sender                    : TObject;
      AErrorCode                : HRESULT;
      const AReturnObjectAsJson : wvstring;
      AExecutionID              : Integer
    );
    procedure WVBrowserClearBrowsingDataCompleted(
      Sender     : TObject;
      AErrorCode : HRESULT
    );
    procedure WVBrowserClearCacheCompleted(
      Sender  : TObject;
      AResult : Boolean
    );
    procedure WVBrowserClearDataForOriginCompleted(
      Sender  : TObject;
      AResult : Boolean
    );
    procedure WVBrowserContentLoading(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2ContentLoadingEventArgs
    );
    procedure WVBrowserContextMenuRequested(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2ContextMenuRequestedEventArgs
    );
    procedure WVBrowserCustomItemSelected(
      Sender          : TObject;
      const AMenuItem : ICoreWebView2ContextMenuItem
    );
    procedure WVBrowserDevToolsProtocolEventReceived(
      Sender           : TObject;
      const AWebView   : ICoreWebView2;
      const AArgs      : ICoreWebView2DevToolsProtocolEventReceivedEventArgs;
      const AEventName : wvstring;
      AEventID         : Integer
    );
    procedure WVBrowserDOMContentLoaded(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2DOMContentLoadedEventArgs
    );
    procedure WVBrowserDownloadStarting(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2DownloadStartingEventArgs
    );
    procedure WVBrowserDownloadStateChanged(
      Sender                   : TObject;
      const ADownloadOperation : ICoreWebView2DownloadOperation;
      ADownloadID              : Integer
    );
    procedure WVBrowserEstimatedEndTimeChanged(
      Sender                   : TObject;
      const ADownloadOperation : ICoreWebView2DownloadOperation;
      ADownloadID              : Integer
    );
    procedure WVBrowserExecuteScriptCompleted(
      Sender                    : TObject;
      AErrorCode                : HRESULT;
      const AResultObjectAsJson : wvstring;
      AExecutionID              : Integer
    );
    procedure WVBrowserFaviconChanged(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : IUnknown
    );
    procedure WVBrowserGetCookiesCompleted(
      Sender            : TObject;
      AResult           : HRESULT;
      const ACookieList : ICoreWebView2CookieList
    );
    procedure WVBrowserGetCustomSchemes(
      Sender             : TObject;
      var ACustomSchemes : TWVCustomSchemeInfoArray
    );
    procedure WVBrowserGetFaviconCompleted(
      Sender               : TObject;
      AErrorCode           : HRESULT;
      const AFaviconStream : IStream
    );
    procedure WVBrowserGetNonDefaultPermissionSettingsCompleted(
      Sender                : TObject;
      AErrorCode            : HRESULT;
      const aCollectionView : ICoreWebView2PermissionSettingCollectionView
    );
    procedure WVBrowserGotFocus(Sender: TObject);
    procedure WVBrowserHistoryChanged(Sender: TObject);
    procedure WVBrowserInitializationError(
      Sender              : TObject;
      AErrorCode          : HRESULT;
      const AErrorMessage : wvstring
    );
    procedure WVBrowserIsMutedChanged(
      Sender: TObject;
      const aWebView: ICoreWebView2
    );
    procedure WVBrowserLaunchingExternalUriScheme(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2LaunchingExternalUriSchemeEventArgs
    );
    procedure WVBrowserLostFocus(Sender: TObject);
    procedure WVBrowserMoveFocusRequested(
      Sender            : TObject;
      const AController : ICoreWebView2Controller;
      const AArgs       : ICoreWebView2MoveFocusRequestedEventArgs
    );
    procedure WVBrowserNavigationCompleted(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2NavigationCompletedEventArgs
    );
    procedure WVBrowserNavigationStarting(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2NavigationStartingEventArgs
    );
    procedure WVBrowserNewWindowRequested(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2NewWindowRequestedEventArgs
    );
    procedure WVBrowserOfflineCompleted(
      Sender  : TObject;
      AResult : Boolean
    );
    procedure WVBrowserPermissionRequested(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2PermissionRequestedEventArgs
    );
    procedure WVBrowserProcessFailed(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2ProcessFailedEventArgs
    );
    procedure WVBrowserProcessInfosChanged(
      Sender             : TObject;
      const AEnvironment : ICoreWebView2Environment
    );
    procedure WVBrowserRefreshIgnoreCacheCompleted(
      Sender                    : TObject;
      AErrorCode                : HRESULT;
      const AResultObjectAsJson : wvstring
    );
    procedure WVBrowserRenderCompMsg(
      Sender       : TObject;
      var AMessage : TMessage;
      var AHandled : Boolean
    );
    procedure WVBrowserRetrieveHTMLCompleted(
      Sender      : TObject;
      AResult     : Boolean;
      const AHTML : wvstring
    );
    procedure WVBrowserRetrieveTextCompleted(
      Sender      : TObject;
      AResult     : Boolean;
      const AText : wvstring
    );
    procedure WVBrowserScriptDialogOpening(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2ScriptDialogOpeningEventArgs
    );
    procedure WVBrowserServerCertificateErrorActionsCompleted(
      Sender     : TObject;
      AErrorCode : HRESULT
    );
    procedure WVBrowserServerCertificateErrorDetected(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2ServerCertificateErrorDetectedEventArgs
    );
    procedure WVBrowserSetPermissionStateCompleted(
      Sender     : TObject;
      AErrorCode : HRESULT
    );
    procedure WVBrowserSimulateKeyEventCompleted(
      Sender  : TObject;
      AResult : Boolean
    );
    procedure WVBrowserSourceChanged(Sender: TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2SourceChangedEventArgs
    );
    procedure WVBrowserStatusBarTextChanged(
      Sender         : TObject;
      const AWebView : ICoreWebView2
    );
    procedure WVBrowserTrySuspendCompleted(
      Sender        : TObject;
      AErrorCode    : HRESULT;
      AIsSuccessful : Boolean
    );
    procedure WVBrowserWebMessageReceived(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2WebMessageReceivedEventArgs
    );
    procedure WVBrowserWebResourceRequested(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2WebResourceRequestedEventArgs
    );
    procedure WVBrowserWebResourceResponseReceived(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2WebResourceResponseReceivedEventArgs
    );
    procedure WVBrowserWebResourceResponseViewGetContentCompleted(
      Sender          : TObject;
      AErrorCode      : HRESULT;
      const AContents : IStream;
      AResourceID     : Integer
    );
    procedure WVBrowserWindowCloseRequested(Sender: TObject);
    {$ENDREGION}

  private
    FFileName          : string;
    FEditMode          : Boolean;
    FIsFile            : Boolean;
    FOnDropFiles       : TDropFilesEvent;
    FOnAfterCreated    : TNotifyEvent;
    FOnChange          : TNotifyEvent;
    FGetHeaders        : Boolean;
    FHeaders           : TStringList;
    FDownloadOperation : TCoreWebView2DownloadOperation;
    FHtmlText          : string;
    FText              : string;
    FSource            : string;
    FUpdate            : Boolean;
    FModified          : Boolean;
    FEditorFont        : TFont;
    FFavIcon           : TPicture;

    FRequestingData : Boolean;  // Webbrowser -> Editor
    FDataReceived   : Boolean;  // Webbrowser -> Editor

    FSendingData    : Boolean;    // Editor -> Webbrowser
    FDataSent       : Boolean;    // Editor -> Webbrowser

    FOffline                     : Boolean;
    FDefaultContextMenusEnabled  : Boolean;
    FDefaultScriptDialogsEnabled : Boolean;
    FDevToolsEnabled             : Boolean;
    FAllowHostObjects            : Boolean;
    FWebMessageEnabled           : Boolean;
    FZoomControlEnabled          : Boolean;
    FStatusBarEnabled            : Boolean;
    FScriptEnabled               : Boolean;

    function GetIconBitmap: TBitmap;
    procedure WMMove(var AMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var AMessage : TMessage); message WM_MOVING;

  protected
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
    function GetDocumentTitle: string;
    function GetEditMode: Boolean;
    function GetEvents: IHtmlEditorEvents;
    function GetFileName: string;
    function GetFont: TFont;
    function GetForm: TCustomForm;
    function GetHtmlText: string;
    function GetIsEmpty: Boolean;
    function GetIsFile: Boolean;
    function GetIsInitialized: Boolean;
    function GetIsNavigating: Boolean;
    function GetIsSourceEmpty: Boolean;
    function GetModified: Boolean;
    function GetOffline: Boolean;
    function GetOnAfterCreated: TNotifyEvent;
    function GetOnChange: TNotifyEvent;
    function GetOnDropFiles: TDropFilesEvent;
    function GetScriptEnabled: Boolean;
    function GetSelAvail: Boolean;
    function GetSelText: string;
    function GetSource: string;
    function GetSourceVisible: Boolean;
    function GetStatusBarEnabled: Boolean;
    function GetText: string;
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
    procedure SetDocumentTitle(AValue: string);
    procedure SetEditMode(AValue: Boolean);
    procedure SetFileName(const AValue: string);
    procedure SetHtmlText(const AValue: string);
    procedure SetIsFile(AValue: Boolean);
    procedure SetModified(const AValue: Boolean);
    procedure SetOffline(AValue: Boolean);
    procedure SetOnAfterCreated(AValue: TNotifyEvent);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TDropFilesEvent);
    procedure SetPopupMenu(const AValue: TPopupMenu);
    procedure SetScriptEnabled(AValue: Boolean);
    procedure SetSelText(const AValue: string);
    procedure SetSource(AValue: string);
    procedure SetSourceVisible(AValue: Boolean);
    procedure SetStatusBarEnabled(AValue: Boolean);
    procedure SetText(const AValue: string);
    procedure SetWebMessageEnabled(AValue: Boolean);
    procedure SetZoomControlEnabled(AValue: Boolean);
    {$ENDREGION}

    procedure ApplySettings;

    procedure DoRequestData;
    procedure DoSendData;
    procedure DoChange;
    procedure DoAfterCreated;
    procedure DoDropFiles(const AFileNames: array of string);

    procedure UpdateActions; override;
    procedure UpdateWatches;

    procedure SelectAll;

    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure Load(const AStorageName: string = '');
    procedure Save(const AStorageName: string = '');

    procedure BeginUpdate;
    procedure EndUpdate;

    function IsUpdating: Boolean;

    // browser methods
    function ExecuteScript(const AScript: string): Boolean;
    function ExecuteEditingCommand(ACommand: TWV2EditingCommand): Boolean;
    function Navigate(const ASource: string): Boolean;
    function GoBack: Boolean;
    function GoForward: Boolean;
    function CanGoBack: Boolean;
    function CanGoForward: Boolean;
    function ClearCache: Boolean;
    function Refresh: Boolean;
    procedure ShowDevTools;
    procedure ShowTaskManager;

    // editor commands
    function InsertImage: Boolean; overload;
    procedure InsertImageFile(const AFileName: string);
    procedure InsertImage(AImage: TPicture); overload;
    procedure InsertHyperlink(
      const AText : string = '';
      const AUrl  : string = ''
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

    procedure Undo;
    procedure Redo;

    {$REGION 'Webview properties'}
    property DocumentTitle: string
      read GetDocumentTitle write SetDocumentTitle;

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

    property IsSourceEmpty: Boolean
      read GetIsSourceEmpty;

    property IconBitmap: TBitmap
      read GetIconBitmap;
  {$ENDREGION}

    // properties
    property ContentSize: Int64
      read GetContentSize;

    property Events: IHtmlEditorEvents
      read GetEvents;

    property Bullets: Boolean
      read GetBullets write SetBullets;

    property Form: TCustomForm
      read GetForm;

    property EditMode: Boolean
      read GetEditMode write SetEditMode;

    property SourceVisible: Boolean
      read GetSourceVisible write SetSourceVisible;

    property Actions: IHtmlEditorActions
      read GetActions;

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

    property IsInitialized: Boolean
      read GetIsInitialized;

    property HtmlText: string
      read GetHtmlText write SetHtmlText;

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

    property FileName: string
       read GetFileName write SetFileName;

    { Browser source in URI format. }
    property Source: string
      read GetSource write SetSource;

    property OnAfterCreated: TNotifyEvent
      read GetOnAfterCreated write SetOnAfterCreated;

    property OnDropFiles: TDropFilesEvent
      read GetOnDropFiles write SetOnDropFiles;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

uses
  StrUtils,

  uWVCoreWebView2Args, uWVCoreWebView2WebResourceResponseView,
  uWVCoreWebView2HttpResponseHeaders, uWVCoreWebView2HttpHeadersCollectionIterator,

  ts.Core.Utils, ts.Core.Logger, ts.Core.Helpers, ts.Core.Logger.Interfaces,
  ts.Core.ComponentInspector, ts.Core.Logger.Channel.IPC,

  ts.HtmlEditor.Resources;

{$REGION 'construction and destruction'}
procedure THtmlEditorView.AfterConstruction;
begin
  inherited AfterConstruction;
  FGetHeaders := True;
  FHeaders    := TStringList.Create;
  FDefaultScriptDialogsEnabled := True;
  FDefaultContextMenusEnabled  := True;
  FDevToolsEnabled             := True;
  FWebMessageEnabled           := True;
  FScriptEnabled               := True;
  FAllowHostObjects            := True;
  FZoomControlEnabled          := True;
  FStatusBarEnabled            := True;
  FOffline                     := False;
  FEditorFont                  := TFont.Create;
  FEditorFont.Name             := DEFAULT_EDITOR_FONT;
  FFavIcon                     := TPicture.Create;
  imgFavIcon.Picture := FFavIcon;
  tmrCreateWebBrowser.Enabled := True;
  Logger.SetLogLevel(10);
end;

destructor THtmlEditorView.Destroy;
begin
  FFavIcon.Free;
  FHeaders.Free;
  FEditorFont.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'message handlers'}
procedure THtmlEditorView.WMMove(var AMessage: TWMMove);
begin
  inherited;
  if Assigned(WVBrowser) then
    WVBrowser.NotifyParentWindowPositionChanged;
end;

function THtmlEditorView.GetIconBitmap: TBitmap;
begin
  Result := imgFavIcon.Picture.Bitmap;
end;

procedure THtmlEditorView.WMMoving(var AMessage: TMessage);
begin
  inherited;
  if Assigned(WVBrowser) then
    WVBrowser.NotifyParentWindowPositionChanged;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure THtmlEditorView.WVBrowserAfterCreated(Sender: TObject);
begin
  //Logger.Enter(Self, 'WVBrowserAfterCreated');
  WVWindowParent.UpdateSize;
  //Source := DEFAULT_SOURCE;

  //WVBrowser.SetVirtualHostNameToFolderMapping('customhost.test', '.', COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND_ALLOW);
  //WVBrowser.AddWebResourceRequestedFilter('*', COREWEBVIEW2_WEB_RESOURCE_CONTEXT_IMAGE);

  WVBrowser.AddScriptToExecuteOnDocumentCreated(
  UTF8Decode(JS_ADD_CONTENT_MODIFIED_EVENT));

  DoAfterCreated;
  ApplySettings;
  //Logger.Leave(Self, 'WVBrowserAfterCreated');
end;

procedure THtmlEditorView.WVBrowserBasicAuthenticationRequested
  (Sender: TObject; const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2BasicAuthenticationRequestedEventArgs);
begin
  //Logger.Info('WVBrowserBasicAuthenticationRequested');
end;

procedure THtmlEditorView.WVBrowserBrowserProcessExited(Sender: TObject;
  const AEnvironment: ICoreWebView2Environment;
  const AArgs: ICoreWebView2BrowserProcessExitedEventArgs);
begin
  //Logger.Info('WVBrowserBrowserProcessExited');
end;

procedure THtmlEditorView.WVBrowserBytesReceivedChanged(Sender: TObject;
  const ADownloadOperation: ICoreWebView2DownloadOperation; ADownloadID: Integer
  );
begin
  //Logger.Track(Self, 'WVBrowserBytesReceivedChanged');
end;

procedure THtmlEditorView.WVBrowserCallDevToolsProtocolMethodCompleted
  (Sender: TObject; AErrorCode: HRESULT; const AReturnObjectAsJson: wvstring;
  AExecutionID: Integer);
begin
  //Logger.Track(Self, 'WVBrowserCallDevToolsProtocolMethodCompleted');
end;

procedure THtmlEditorView.WVBrowserClearBrowsingDataCompleted(Sender: TObject;
  AErrorCode: HRESULT);
begin
//  Logger.Track(Self, 'WVBrowserClearBrowsingDataCompleted');
end;

procedure THtmlEditorView.WVBrowserClearCacheCompleted(Sender: TObject;
  AResult: Boolean);
begin
//  Logger.Track(Self, 'WVBrowserClearCacheCompleted');
end;

procedure THtmlEditorView.WVBrowserClearDataForOriginCompleted(Sender: TObject;
  AResult: Boolean);
begin
//  Logger.Track(Self, 'WVBrowserClearDataForOriginCompleted');
end;

procedure THtmlEditorView.WVBrowserContentLoading(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2ContentLoadingEventArgs);
begin
  //Logger.Info('WVBrowserContentLoading');
end;

procedure THtmlEditorView.WVBrowserContextMenuRequested(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2ContextMenuRequestedEventArgs);
begin
  //Logger.Track(Self, 'WVBrowserContextMenuRequested');
end;

procedure THtmlEditorView.WVBrowserCustomItemSelected(Sender: TObject;
  const AMenuItem: ICoreWebView2ContextMenuItem);
begin
  //Logger.Track(Self, 'WVBrowserCustomItemSelected');
end;

procedure THtmlEditorView.WVBrowserDevToolsProtocolEventReceived
  (Sender: TObject; const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2DevToolsProtocolEventReceivedEventArgs;
  const AEventName: wvstring; AEventID: Integer);
begin
  //Logger.Info('WVBrowserDevToolsProtocolEventReceived');
end;

{ The DOMContentLoaded event is fired when the initial HTML document has been
  completely loaded and parsed.  }

procedure THtmlEditorView.WVBrowserDOMContentLoaded(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2DOMContentLoadedEventArgs);
begin
  //Logger.Info('WVBrowserDOMContentLoaded');

    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Status
    //LStatus := LArgs.HttpStatusCode;
  if not FSendingData then
  begin
    DoRequestData;
  end
  else if FDataReceived then
  begin
    FSendingData := False;
    FDataSent    := True;
    FRequestingData := False;
  end;
  Events.DoContentLoaded;
end;

procedure THtmlEditorView.WVBrowserDownloadStarting(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2DownloadStartingEventArgs);
var
  LArgs : TCoreWebView2DownloadStartingEventArgs;
begin
  // This demo only allows 1 file download at the same time.
  // Create a list of TCoreWebView2DownloadOperation if you need to handle more
  // file downloads and check the DownloadID field.

  LArgs := TCoreWebView2DownloadStartingEventArgs.Create(AArgs);
  try

    if Assigned(LArgs.DownloadOperation) and not(assigned(FDownloadOperation)) then
      begin
        FDownloadOperation := TCoreWebView2DownloadOperation.Create(LArgs.DownloadOperation, 1);
        FDownloadOperation.AddAllBrowserEvents(WVBrowser);
        // We hide the download window
        LArgs.Handled := True;
      end;
  finally
    FreeAndNil(LArgs);
  end;
  //Logger.Info('WVBrowserDownloadStarting');
end;

procedure THtmlEditorView.WVBrowserDownloadStateChanged(Sender: TObject;
  const ADownloadOperation: ICoreWebView2DownloadOperation; ADownloadID: Integer
  );
begin
  //Logger.Track(Self, 'WVBrowserDownloadStateChanged');
end;

procedure THtmlEditorView.WVBrowserEstimatedEndTimeChanged(Sender: TObject;
  const ADownloadOperation: ICoreWebView2DownloadOperation; ADownloadID: Integer
  );
begin
  //Logger.Track(Self, 'WVBrowserEstimatedEndTimeChanged');
end;

procedure THtmlEditorView.WVBrowserExecuteScriptCompleted(Sender: TObject;
  AErrorCode: HRESULT; const AResultObjectAsJson: wvstring;
  AExecutionID: Integer);
begin
  //Logger.Enter(Self, 'WVBrowserExecuteScriptCompleted');
  //Logger.Send('AResultObjectAsJson', AResultObjectAsJson);
  //Logger.Send('AExecutionID', AExecutionID);
  //Logger.Leave(Self, 'WVBrowserExecuteScriptCompleted');
end;

procedure THtmlEditorView.WVBrowserFaviconChanged(Sender: TObject;
  const AWebView: ICoreWebView2; const AArgs: IUnknown);
begin
  //Logger.Track(Self, 'WVBrowserFaviconChanged');
end;

procedure THtmlEditorView.WVBrowserGetCookiesCompleted(Sender: TObject;
  AResult: HRESULT; const ACookieList: ICoreWebView2CookieList);
begin
  //Logger.Track(Self, 'WVBrowserGetCookiesCompleted');
end;

procedure THtmlEditorView.WVBrowserGetCustomSchemes(Sender: TObject;
  var ACustomSchemes: TWVCustomSchemeInfoArray);
begin
  //Logger.Track(Self, 'WVBrowserGetCustomSchemes');
end;

procedure THtmlEditorView.WVBrowserGetFaviconCompleted(Sender: TObject;
  AErrorCode: HRESULT; const AFaviconStream: IStream);
var
  LStream : TStream;
begin
  LStream := TMemoryStream.Create;
  try
    LoadIStreamIntoTStream(AFaviconStream, LStream);
    imgFavIcon.Picture.LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
  Events.DoNavigationCompleted;
end;

procedure THtmlEditorView.WVBrowserGetNonDefaultPermissionSettingsCompleted
  (Sender: TObject; AErrorCode: HRESULT;
  const aCollectionView: ICoreWebView2PermissionSettingCollectionView);
begin
  //Logger.Track(Self, 'WVBrowserGetNonDefaultPermissionSettingsCompleted');
end;

procedure THtmlEditorView.WVBrowserGotFocus(Sender: TObject);
begin
//  Logger.Info('WVBrowserGotFocus');
  FUpdate := True;
end;

procedure THtmlEditorView.WVBrowserHistoryChanged(Sender: TObject);
begin
//  Logger.Info('WVBrowserHistoryChanged');
end;

procedure THtmlEditorView.WVBrowserInitializationError(Sender: TObject;
  AErrorCode: HRESULT; const AErrorMessage: wvstring);
begin
  Logger.Error(UTF8Encode(AErrorMessage));
end;

procedure THtmlEditorView.WVBrowserIsMutedChanged(Sender: TObject;
  const aWebView: ICoreWebView2);
begin
  //Logger.Track(Self, 'WVBrowserIsMutedChanged');
end;

procedure THtmlEditorView.WVBrowserLaunchingExternalUriScheme(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2LaunchingExternalUriSchemeEventArgs);
begin
  //Logger.Track(Self, 'WVBrowserLaunchingExternalUriScheme');
end;

procedure THtmlEditorView.WVBrowserLostFocus(Sender: TObject);
begin
  DoRequestData;
end;

procedure THtmlEditorView.WVBrowserMoveFocusRequested(Sender: TObject;
  const AController: ICoreWebView2Controller;
  const AArgs: ICoreWebView2MoveFocusRequestedEventArgs);
begin
  //Logger.Info('WVBrowserMoveFocusRequested');
end;

procedure THtmlEditorView.WVBrowserNavigationCompleted(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2NavigationCompletedEventArgs);
var
  LArgs : TCoreWebView2NavigationCompletedEventArgs;
begin
  //Logger.Enter(Self, 'WVBrowserNavigationCompleted');
  LArgs := TCoreWebView2NavigationCompletedEventArgs.Create(AArgs);
  try
    LArgs.HttpStatusCode;
    WVBrowser.GetFavicon;
    //Logger.Send('HttpStatusCode', LArgs.HttpStatusCode);
    //Logger.Send('Initialized', LArgs.Initialized);
    //Logger.Send('IsSuccess', LArgs.IsSuccess);
    //Logger.Send('WebErrorStatus', LArgs.WebErrorStatus);
    //Logger.Send('WVBrowser.Source', WVBrowser.Source);
  finally
    LArgs.Free;
  end;
  //Events.DoNavigationCompleted;
  //Logger.Info(WVBrowser.DocumentTitle);
  //Logger.Leave(Self, 'WVBrowserNavigationCompleted');
end;

procedure THtmlEditorView.WVBrowserNavigationStarting(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2NavigationStartingEventArgs);
//var
//  LArgs : TCoreWebView2NavigationStartingEventArgs;
begin
  //Logger.Enter(Self, 'WVBrowserNavigationStarting');
  //LArgs := TCoreWebView2NavigationStartingEventArgs.Create(AArgs);
  //try
  //  FGetHeaders := True;
  //  Logger.Send('NavigationId', LArgs.NavigationID);
  //  Logger.Send('Uri', LArgs.URI);
  //  Logger.Send('IsUserInitiated', LArgs.IsUserInitiated);
  //  Logger.Send('IsRedirected', LArgs.IsRedirected);
  //
  //finally
  //  LArgs.Free;
  //end;
  //Logger.Leave(Self, 'WVBrowserNavigationStarting');
end;

procedure THtmlEditorView.WVBrowserNewWindowRequested(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2NewWindowRequestedEventArgs);
begin
  //Logger.Info('WVBrowserNewWindowRequested');
end;

procedure THtmlEditorView.WVBrowserOfflineCompleted(Sender: TObject;
  AResult: Boolean);
begin
  //Logger.Track(Self, 'WVBrowserOfflineCompleted');
  FOffline := AResult;
end;

procedure THtmlEditorView.WVBrowserPermissionRequested(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2PermissionRequestedEventArgs);
begin
  //Logger.Track(Self, 'WVBrowserPermissionRequested');
end;

procedure THtmlEditorView.WVBrowserProcessFailed(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2ProcessFailedEventArgs);
begin
  //Logger.Track(Self, 'WVBrowserProcessFailed');

//  COREWEBVIEW2_PROCESS_FAILED_KIND = TOleEnum;
//const
//  COREWEBVIEW2_PROCESS_FAILED_KIND_BROWSER_PROCESS_EXITED = $00000000;
//  COREWEBVIEW2_PROCESS_FAILED_KIND_RENDER_PROCESS_EXITED = $00000001;
//  COREWEBVIEW2_PROCESS_FAILED_KIND_RENDER_PROCESS_UNRESPONSIVE = $00000002;
//  COREWEBVIEW2_PROCESS_FAILED_KIND_FRAME_RENDER_PROCESS_EXITED = $00000003;
//  COREWEBVIEW2_PROCESS_FAILED_KIND_UTILITY_PROCESS_EXITED = $00000004;
//  COREWEBVIEW2_PROCESS_FAILED_KIND_SANDBOX_HELPER_PROCESS_EXITED = $00000005;
//  COREWEBVIEW2_PROCESS_FAILED_KIND_GPU_PROCESS_EXITED = $00000006;
//  COREWEBVIEW2_PROCESS_FAILED_KIND_PPAPI_PLUGIN_PROCESS_EXITED = $00000007;
//  COREWEBVIEW2_PROCESS_FAILED_KIND_PPAPI_BROKER_PROCESS_EXITED = $00000008;
//  COREWEBVIEW2_PROCESS_FAILED_KIND_UNKNOWN_PROCESS_EXITED = $00000009;
end;

procedure THtmlEditorView.WVBrowserProcessInfosChanged(Sender: TObject;
  const AEnvironment: ICoreWebView2Environment);
begin
  //Logger.Track(Self, 'WVBrowserProcessInfosChanged');
end;

procedure THtmlEditorView.WVBrowserRefreshIgnoreCacheCompleted(Sender: TObject;
  AErrorCode: HRESULT; const AResultObjectAsJson: wvstring);
begin
  //Logger.Track(Self, 'WVBrowserRefreshIgnoreCacheCompleted');
end;

procedure THtmlEditorView.WVBrowserRenderCompMsg(Sender: TObject;
  var AMessage: TMessage; var AHandled: Boolean);
begin
  //Logger.Track(Self, 'WVBrowserRenderCompMsg');
end;

procedure THtmlEditorView.WVBrowserRetrieveHTMLCompleted(Sender: TObject;
  AResult: Boolean; const AHTML: wvstring);
begin
  //Logger.Track(Self, 'WVBrowserRetrieveHTMLCompleted');
  if AResult then
  begin
    if FRequestingData and not FDataReceived then
    begin;
      HtmlText := UTF8Encode(AHTML);
    end;
    FRequestingData := False;
    FDataReceived   := True;
  end;
  //Logger.Send('HtmlText', HtmlText);
end;

procedure THtmlEditorView.WVBrowserRetrieveTextCompleted(Sender: TObject;
  AResult: Boolean; const AText: wvstring);
begin
  //Logger.Track(Self, 'WVBrowserRetrieveTextCompleted');
  if AResult then
    Text := UTF8Encode(AText);
end;

procedure THtmlEditorView.WVBrowserScriptDialogOpening(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2ScriptDialogOpeningEventArgs);
begin
  //Logger.Track(Self, 'WVBrowserScriptDialogOpening');
end;

procedure THtmlEditorView.WVBrowserServerCertificateErrorActionsCompleted
  (Sender: TObject; AErrorCode: HRESULT);
begin
  //Logger.Track(Self, 'WVBrowserServerCertificateErrorActionsCompleted');
end;

procedure THtmlEditorView.WVBrowserServerCertificateErrorDetected
  (Sender: TObject; const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2ServerCertificateErrorDetectedEventArgs);
begin
  //Logger.Track(Self, 'WVBrowserServerCertificateErrorDetected');
end;

procedure THtmlEditorView.WVBrowserSetPermissionStateCompleted(Sender: TObject;
  AErrorCode: HRESULT);
begin
  //Logger.Track(Self, 'WVBrowserSetPermissionStateCompleted');
end;

procedure THtmlEditorView.WVBrowserSimulateKeyEventCompleted(Sender: TObject;
  AResult: Boolean);
begin
  //Logger.Track(Self, 'WVBrowserSimulateKeyEventCompleted');
end;

procedure THtmlEditorView.WVBrowserSourceChanged(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2SourceChangedEventArgs);
var
  LArgs : TCoreWebView2SourceChangedEventArgs;
begin
  //Logger.Enter(Self, 'WVBrowserSourceChanged');
  LArgs := TCoreWebView2SourceChangedEventArgs.Create(AArgs);
  try
    //Logger.Send('Initialized', LArgs.Initialized);
    //Logger.Send('IsNewDocument', LArgs.IsNewDocument);
    Events.DoSourceChanged;
  finally
    LArgs.Free;
  end;
  //Logger.Leave(Self, 'WVBrowserSourceChanged');
end;

procedure THtmlEditorView.WVBrowserStatusBarTextChanged(Sender: TObject;
  const AWebView: ICoreWebView2);
begin
  //Logger.Track(Self, 'WVBrowserStatusBarTextChanged');
end;

procedure THtmlEditorView.WVBrowserTrySuspendCompleted(Sender: TObject;
  AErrorCode: HRESULT; AIsSuccessful: Boolean);
begin
  //Logger.Track(Self, 'WVBrowserTrySuspendCompleted');
end;

procedure THtmlEditorView.WVBrowserWebMessageReceived(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2WebMessageReceivedEventArgs);
var
  LArgs : TCoreWebView2WebMessageReceivedEventArgs;
begin
//  Logger.Enter(Self, 'WVBrowserWebMessageReceived');
  LArgs := TCoreWebView2WebMessageReceivedEventArgs.Create(AArgs);
  try
    //Logger.Send('Initialized', LArgs.Initialized);
    //Logger.Send('Source', LArgs.Source);
    //Logger.Send('WebMessageAsJson', LArgs.WebMessageAsJson);
    //Logger.Send('WebMessageAsString', LArgs.WebMessageAsString);
    //Logger.Send('AdditionalObjects', LArgs.AdditionalObjects);
    if LArgs.WebMessageAsString = 'contentModified' then
    begin
      DoChange;
    end;
  finally
    LArgs.Free;
  end;
//  Logger.Leave(Self, 'WVBrowserWebMessageReceived');
end;

procedure THtmlEditorView.WVBrowserWebResourceRequested(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2WebResourceRequestedEventArgs);
var
//  LArgs     : TCoreWebView2WebResourceRequestedEventArgs;
  LResponse : ICoreWebView2WebResourceResponse;
begin
//  Logger.Enter(Self, 'WVBrowserWebResourceRequested');
  LResponse := nil;
//  Logger.Leave(Self, 'WVBrowserWebResourceRequested');

   //WVBrowser.CoreWebView2Environment
  //if FBlockImages and
  //   WVBrowser1.CoreWebView2Environment.CreateWebResourceResponse(nil, 403, 'Blocked', 'Content-Type: image/jpeg', LResponse) then
  //  try
  //    LArgs          := TCoreWebView2WebResourceRequestedEventArgs.Create(AArgs);
  //    LArgs.Response := LResponse;
  //  finally
  //    FreeAndNil(LArgs);
  //    LResponse := nil;
  //  end;
end;

procedure THtmlEditorView.edtSourceMouseEnter(Sender: TObject);
var
  LEdit : TEdit absolute Sender;
begin
  LEdit.Color      := clWhite;
  LEdit.Font.Color := clBlack;
end;

procedure THtmlEditorView.dctMainDrop(Sender: TObject; ShiftState: TShiftState;
  APoint: TPoint; var Effect: Longint);
var
  LSource : string;
begin
  if dctMain.URL.IsEmpty then
  begin;
    LSource := 'file:///' + string(dctMain.Files.Text);
  end
  else
  begin
    LSource := dctMain.URL
  end;
  edtSource.Text := LSource;
  edtSource.Hint := LSource;
  Source         := LSource;
end;

procedure THtmlEditorView.edtSourceExit(Sender: TObject);
var
  LEdit : TEdit absolute Sender;
begin
  LEdit.Color      := clForm;
  LEdit.Font.Color := clDkGray;
  Logger.Info('Exited');
end;

procedure THtmlEditorView.edtSourceMouseLeave(Sender: TObject);
var
  LEdit : TEdit absolute Sender;
begin
  if not LEdit.Focused then
  begin
    LEdit.Color      := clForm;
    LEdit.Font.Color := clDkGray;
    WVBrowser.SetFocus;
  end;
end;

procedure THtmlEditorView.edtSourceEditingDone(Sender: TObject);
var
  LEdit : TEdit absolute Sender;
begin
  Source     := LEdit.Text;
  LEdit.Hint := LEdit.Text;
end;

procedure THtmlEditorView.tmrCreateWebBrowserTimer(Sender: TObject);
begin
  tmrCreateWebBrowser.Enabled := False;
  if GlobalWebView2Loader.Initialized then
    WVBrowser.CreateBrowser(WVWindowParent.Handle)
  else
    tmrCreateWebBrowser.Enabled := True;
end;

procedure THtmlEditorView.WVBrowserAcceleratorKeyPressed(Sender: TObject;
  const AController: ICoreWebView2Controller;
  const AArgs: ICoreWebView2AcceleratorKeyPressedEventArgs);
begin
  //Logger.Info('WVBrowserAcceleratorKeyPressed');
end;

procedure THtmlEditorView.WVBrowserAddScriptToExecuteOnDocumentCreatedCompleted
  (Sender: TObject; AErrorCode: HRESULT; const AID: wvstring);
begin
  //Logger.Track(Self, 'WVBrowserAddScriptToExecuteOnDocumentCreatedCompleted');
end;

procedure THtmlEditorView.WVBrowserWebResourceResponseReceived(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2WebResourceResponseReceivedEventArgs);
var
  LArgs     : TCoreWebView2WebResourceResponseReceivedEventArgs;
  LResponse : TCoreWebView2WebResourceResponseView;
  LHeaders  : TCoreWebView2HttpResponseHeaders;
  LIterator : TCoreWebView2HttpHeadersCollectionIterator;
  LName     : wvstring;
  LValue    : wvstring;
begin
  //Logger.Enter(Self, 'WVBrowserWebResourceResponseReceived');
  if FGetHeaders then
    try
      FHeaders.Clear;
      FGetHeaders  := False;
      LArgs     := TCoreWebView2WebResourceResponseReceivedEventArgs.Create(AArgs);
      LResponse := TCoreWebView2WebResourceResponseView.Create(LArgs.Response);
      LHeaders  := TCoreWebView2HttpResponseHeaders.Create(LResponse.Headers);
      LIterator := TCoreWebView2HttpHeadersCollectionIterator.Create(LHeaders.Iterator);

      while LIterator.HasCurrentHeader do
      begin
        if LIterator.GetCurrentHeader(LName, LValue) then
          FHeaders.Add(UTF8Encode(LName) + ': ' + UTF8Encode(LValue));
        LIterator.MoveNext;
      end;
    finally
      FreeAndNil(LIterator);
      FreeAndNil(LHeaders);
      FreeAndNil(LResponse);
      FreeAndNil(LArgs);
    end;
    //Logger.Leave(Self, 'WVBrowserWebResourceResponseReceived');
end;

procedure THtmlEditorView.WVBrowserWebResourceResponseViewGetContentCompleted
  (Sender: TObject; AErrorCode: HRESULT; const AContents: IStream;
  AResourceID: Integer);
begin
  ///Logger.Track(Self, 'WVBrowserWebResourceResponseViewGetContentCompleted');
end;

procedure THtmlEditorView.WVBrowserWindowCloseRequested(Sender: TObject);
begin
  //Logger.Info('WVBrowserWindowCloseRequested');
end;

{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure THtmlEditorView.DoChange;
begin
  //Logger.Info('DoChange');
  UpdateWatches;
  FUpdate   := True;
  FModified := True;
  if Assigned(OnChange) {and not IsUpdating} then
  begin
    OnChange(Self);
  end;
  Events.DoChange;
end;

procedure THtmlEditorView.DoAfterCreated;
begin
  UpdateWatches;
  if Assigned(FOnAfterCreated) then
    FOnAfterCreated(Self);
end;

procedure THtmlEditorView.DoDropFiles(const AFileNames: array of string);
begin
  if Assigned(FOnDropFiles) then
    FOnDropFiles(Self, AFileNames);
end;

procedure THtmlEditorView.DoRequestData;
begin
  //Logger.Info('DoRequestData');
  WVBrowser.RetrieveHTML;
  FRequestingData := True;
  FDataReceived   := False;
  UpdateWatches;
end;

procedure THtmlEditorView.DoSendData;
begin
  //Logger.Info('DoSendData');
  WVBrowser.NavigateToString(UTF8Decode(FHtmlText));
  FSendingData := True;
  FDataSent    := False;
  UpdateWatches;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function THtmlEditorView.GetDocumentTitle: string;
begin
  Result := UTF8Encode(WVBrowser.DocumentTitle);
end;

procedure THtmlEditorView.SetDocumentTitle(AValue: string);
begin
  // not supported yet
end;

function THtmlEditorView.GetSourceVisible: Boolean;
begin
  Result := edtSource.Visible;
end;

procedure THtmlEditorView.SetSourceVisible(AValue: Boolean);
begin
  if AValue <> edtSource.Visible then
  begin
    edtSource.Visible := AValue;
  end;
end;

function THtmlEditorView.GetActions: IHtmlEditorActions;
begin
  Result := Owner as IHtmlEditorActions;
end;

function THtmlEditorView.GetAlignCenter: Boolean;
begin
  Result := False;
end;

function THtmlEditorView.GetAlignJustify: Boolean;
begin
  Result := False;
end;

function THtmlEditorView.GetAlignLeft: Boolean;
begin
  Result := False;
end;

function THtmlEditorView.GetAlignRight: Boolean;
begin
  Result := False;
end;

function THtmlEditorView.GetBullets: Boolean;
begin
  Result := False;
end;

function THtmlEditorView.GetCanPaste: Boolean;
begin
  Result := True;
end;

function THtmlEditorView.GetCanRedo: Boolean;
begin
  Result := True;
end;

function THtmlEditorView.GetCanUndo: Boolean;
begin
  Result := True;
end;

function THtmlEditorView.GetContentSize: Int64;
begin
  if Source.IsEmpty then
    Result := Length(HtmlText)
  else
    Result := 0;
end;

function THtmlEditorView.GetEditMode: Boolean;
begin
  Result := FEditMode;
end;

function THtmlEditorView.GetAllowHostObjects: Boolean;
begin
  Result := WVBrowser.AreHostObjectsAllowed;
end;

function THtmlEditorView.GetDefaultContextMenusEnabled: Boolean;
begin
  Result := WVBrowser.DefaultContextMenusEnabled;
end;

function THtmlEditorView.GetDefaultScriptDialogsEnabled: Boolean;
begin
  Result := WVBrowser.DefaultScriptDialogsEnabled;
end;

function THtmlEditorView.GetDevToolsEnabled: Boolean;
begin
  Result := WVBrowser.DevToolsEnabled;
end;

function THtmlEditorView.GetIsInitialized: Boolean;
begin
  Result := WVBrowser.Initialized;
end;

function THtmlEditorView.GetIsNavigating: Boolean;
begin
  Result := WVBrowser.IsNavigating;
end;

function THtmlEditorView.GetEvents: IHtmlEditorEvents;
begin
  Result := Owner as IHtmlEditorEvents;
end;

function THtmlEditorView.GetOnAfterCreated: TNotifyEvent;
begin
  Result := FOnAfterCreated;
end;

procedure THtmlEditorView.SetOnAfterCreated(AValue: TNotifyEvent);
begin
  FOnAfterCreated := AValue;
end;

function THtmlEditorView.GetOffline: Boolean;
begin
  Result := WVBrowser.Offline;
end;

procedure THtmlEditorView.SetOffline(AValue: Boolean);
begin
  FOffline := AValue;
  WVBrowser.Offline := AValue;
end;

function THtmlEditorView.GetScriptEnabled: Boolean;
begin
  Result := WVBrowser.ScriptEnabled;
end;

function THtmlEditorView.GetStatusBarEnabled: Boolean;
begin
  Result := WVBrowser.StatusBarEnabled;
end;

function THtmlEditorView.GetIsSourceEmpty: Boolean;
begin
  Result := MatchText(Source, [EMPTY_PAGE_SOURCE, '']);
end;

function THtmlEditorView.GetWebMessageEnabled: Boolean;
begin
  Result := WVBrowser.WebMessageEnabled;
end;

function THtmlEditorView.GetZoomControlEnabled: Boolean;
begin
  Result := WVBrowser.ZoomControlEnabled;
end;

procedure THtmlEditorView.SetAllowHostObjects(AValue: Boolean);
begin
  WVBrowser.AreHostObjectsAllowed := AValue;
end;

procedure THtmlEditorView.SetDefaultContextMenusEnabled(AValue: Boolean);
begin
  FDefaultContextMenusEnabled := AValue;
  WVBrowser.DefaultContextMenusEnabled := AValue;
end;

procedure THtmlEditorView.SetDefaultScriptDialogsEnabled(AValue: Boolean);
begin
  FDefaultScriptDialogsEnabled := AValue;
  WVBrowser.DefaultScriptDialogsEnabled := AValue;
end;

procedure THtmlEditorView.SetDevToolsEnabled(AValue: Boolean);
begin
  FDevToolsEnabled := AValue;
  WVBrowser.DevToolsEnabled := AValue;
end;

procedure THtmlEditorView.SetEditMode(AValue: Boolean);
begin
  if AValue <> EditMode then
  begin
    FEditMode := AValue;
    if WVBrowser.Initialized then
      ApplySettings;
  end;
end;

procedure THtmlEditorView.SetScriptEnabled(AValue: Boolean);
begin
  FScriptEnabled := AValue;
  WVBrowser.ScriptEnabled := AValue;
end;

procedure THtmlEditorView.SetStatusBarEnabled(AValue: Boolean);
begin
  FStatusBarEnabled := AValue;
  WVBrowser.StatusBarEnabled := AValue;
end;

procedure THtmlEditorView.SetWebMessageEnabled(AValue: Boolean);
begin
  FWebMessageEnabled := AValue;
  WVBrowser.WebMessageEnabled := AValue;
end;

procedure THtmlEditorView.SetZoomControlEnabled(AValue: Boolean);
begin
  FZoomControlEnabled := AValue;
  WVBrowser.ZoomControlEnabled := AValue;
end;

function THtmlEditorView.GetFileName: string;
begin
  Result := FFileName;
end;

function THtmlEditorView.GetFont: TFont;
begin
  Result := FEditorFont;
end;

function THtmlEditorView.GetForm: TCustomForm;
begin
  Result := Self;
end;

function THtmlEditorView.GetHtmlText: string;
begin
  Result := FHtmlText;
end;

function THtmlEditorView.GetIsEmpty: Boolean;
var
  S : string;
begin
  S := Trim(HtmlText);
  if S.IsEmpty or SameText(S, EMPTY_PAGE_CONTENT) then
    Result := True
  else
    Result := False;
end;

function THtmlEditorView.GetIsFile: Boolean;
begin
  Result := FIsFile;
end;

function THtmlEditorView.GetModified: Boolean;
begin
  Result := FModified;
end;

function THtmlEditorView.GetOnChange: TNotifyEvent;
begin
  Result := FOnChange;
end;

function THtmlEditorView.GetOnDropFiles: TDropFilesEvent;
begin
  Result := FOnDropFiles;
end;

function THtmlEditorView.GetSelAvail: Boolean;
begin
  Result := False;
end;

function THtmlEditorView.GetSelText: string;
begin
  Result := '';
end;

function THtmlEditorView.GetText: string;
begin
  Result := FText;
end;

procedure THtmlEditorView.SetAlignCenter(AValue: Boolean);
begin
  ExecuteEditingCommand(ecJustifyCenter);
end;

procedure THtmlEditorView.SetAlignJustify(AValue: Boolean);
begin
  ExecuteEditingCommand(ecJustifyFull);
end;

procedure THtmlEditorView.SetAlignLeft(AValue: Boolean);
begin
  ExecuteEditingCommand(ecJustifyLeft);
end;

procedure THtmlEditorView.SetAlignRight(AValue: Boolean);
begin
  ExecuteEditingCommand(ecJustifyRight);
end;

procedure THtmlEditorView.SetBullets(AValue: Boolean);
begin
  imgFavIcon.Picture.Bitmap.Empty;
end;

procedure THtmlEditorView.SetFileName(const AValue: string);
begin
  FFileName := AValue;
end;

procedure THtmlEditorView.SetHtmlText(const AValue: string);
begin
  if AValue <> HtmlText then
  begin
    FHtmlText := AValue;
    if FRequestingData then
    begin
      DoChange;
    end
    else
    begin
      DoSendData;
    end;
  end;
end;

procedure THtmlEditorView.SetIsFile(AValue: Boolean);
begin
//
end;

procedure THtmlEditorView.SetModified(const AValue: Boolean);
begin
  if AValue <> Modified then
  begin
    FModified := AValue;
  end;
end;

procedure THtmlEditorView.SetOnChange(const AValue: TNotifyEvent);
begin
  FOnChange := AValue;
end;

procedure THtmlEditorView.SetOnDropFiles(const AValue: TDropFilesEvent);
begin
  FOnDropFiles := AValue;
end;

procedure THtmlEditorView.SetPopupMenu(const AValue: TPopupMenu);
begin
  WVWindowParent.PopupMenu := AValue;
  //edtSource.PopupMenu      := AValue;
end;

procedure THtmlEditorView.SetSelText(const AValue: string);
begin
// not supported
end;

procedure THtmlEditorView.SetText(const AValue: string);
begin
  if AValue <> Text then
  begin
    FText := AValue;
    DoChange;
  end;
end;

function THtmlEditorView.GetSource: string;
begin
  Result := FSource;
end;

procedure THtmlEditorView.SetSource(AValue: string);
begin
  if AValue <> Source then
  begin
    if AValue = EMPTY_PAGE_SOURCE then
      FSource := ''
    else
      FSource := AValue;
    if FSource <> '' then
    begin
      Navigate(FSource);
    end
  end;
  edtSource.Text := FSource;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure THtmlEditorView.ApplySettings;
begin
  if FEditMode then
    ExecuteScript('document.designMode = "on";')
  else
    ExecuteScript('document.designMode = "off";');

  ExecuteScript(
    Format('document.execCommand(''fontName'', false, ''%s'');',
           [FEditorFont.Name])
  );
  WVBrowser.DefaultContextMenusEnabled  := FDefaultContextMenusEnabled;
  WVBrowser.DefaultScriptDialogsEnabled := FDefaultScriptDialogsEnabled;
end;

procedure THtmlEditorView.ShowDevTools;
begin
  WVBrowser.OpenDevToolsWindow;
end;

procedure THtmlEditorView.ShowTaskManager;
begin
  WVBrowser.OpenTaskManagerWindow;
end;

procedure THtmlEditorView.UpdateActions;
begin
  inherited UpdateActions;
  if FUpdate and IsInitialized then
  begin
    DoRequestData;
    ApplySettings;
    if not edtSource.Focused then
    begin
      edtSource.Color      := clForm;
      edtSource.Font.Color := clDkGray;
    end;
    FUpdate := False;
    UpdateWatches;
  end;

  if Assigned(Actions) then
    Actions.UpdateActions;
end;

procedure THtmlEditorView.UpdateWatches;
begin
  //Logger.Watch('IsInitialized', IsInitialized);
  //Logger.Watch('Modified', Modified);
  //Logger.Watch('FUpdate', FUpdate);
  //Logger.Watch('IsNavigating', IsNavigating);
  //Logger.Watch('FDataReceived', FDataReceived);
  //Logger.Watch('FDataSent', FDataSent);
  //Logger.Watch('FRequestingData', FRequestingData);
  //Logger.Watch('FSendingData', FSendingData);
  //Logger.Watch('Source', Source);
end;

procedure THtmlEditorView.SelectAll;
begin
  ExecuteEditingCommand(ecSelectAll);
end;

function THtmlEditorView.Refresh: Boolean;
begin
  Result := WVBrowser.Refresh;
  ApplySettings;
end;

procedure THtmlEditorView.LoadFromFile(const AFileName: string);
var
  LSource : string;
begin
  if (Length(AFileName) > 0) and FileExists(AFileName) then
  begin
    LSource := UTF8Decode('file:///' + AFileName);
      // TODO: The Filename should be encoded
    Logger.Info(LSource);
    WVBrowser.Navigate(LSource);
    FSendingData := True;
    FDataSent    := False;
  end;
end;

procedure THtmlEditorView.LoadFromStream(AStream: TStream);
begin
  BeginUpdate;
  try
    Clear;
    if AStream is TStringStream then
    begin
      HtmlText := (AStream as TStringStream).DataString;
    end;
  finally
    EndUpdate;
  end;
end;

procedure THtmlEditorView.SaveToStream(AStream: TStream);
var
  SS : TStringStream;
begin
  SS := TStringStream.Create(HtmlText);
  try
    AStream.Position := 0;
    AStream.CopyFrom(SS, 0);
  finally
    FreeAndNil(SS);
  end;
end;

procedure THtmlEditorView.SaveToFile(const AFileName: string);
begin

end;

procedure THtmlEditorView.Load(const AStorageName: string);
begin

end;

procedure THtmlEditorView.Save(const AStorageName: string);
begin

end;

procedure THtmlEditorView.BeginUpdate;
begin

end;

procedure THtmlEditorView.EndUpdate;
begin

end;

function THtmlEditorView.IsUpdating: Boolean;
begin
//
end;

function THtmlEditorView.ExecuteScript(const AScript: string): Boolean;
begin
  Result := WVBrowser.ExecuteScript(UTF8Decode(AScript));
end;

function THtmlEditorView.ExecuteEditingCommand(ACommand: TWV2EditingCommand
  ): Boolean;
begin
  WVWindowParent.SetFocus;
  Result := WVBrowser.SimulateEditingCommand(ACommand);
  DoChange;
end;

function THtmlEditorView.Navigate(const ASource: string): Boolean;
begin
  WVBrowser.Navigate(UTF8Decode(FSource));
end;

function THtmlEditorView.GoBack: Boolean;
begin
  Result := WVBrowser.GoBack;
end;

function THtmlEditorView.GoForward: Boolean;
begin
  Result := WVBrowser.GoForward;
end;

function THtmlEditorView.CanGoBack: Boolean;
begin
  Result := WVBrowser.CanGoBack;
end;

function THtmlEditorView.CanGoForward: Boolean;
begin
  Result := WVBrowser.CanGoForward;
end;

function THtmlEditorView.ClearCache: Boolean;
begin
  Result := WVBrowser.ClearCache;
end;

function THtmlEditorView.InsertImage: Boolean;
begin
  ExecuteEditingCommand(ecInsertImage);
end;

procedure THtmlEditorView.InsertImageFile(const AFileName: string);
begin

end;

procedure THtmlEditorView.InsertImage(AImage: TPicture);
begin

end;

procedure THtmlEditorView.InsertHyperlink(const AText: string;
  const AUrl: string);
begin
  ExecuteEditingCommand(ecCreateLink);
end;

procedure THtmlEditorView.CreateBulletList;
begin
  ExecuteEditingCommand(ecInsertUnorderedList);
end;

procedure THtmlEditorView.CreateNumberedList;
begin
  ExecuteEditingCommand(ecInsertOrderedList);
end;

procedure THtmlEditorView.AddParagraph;
begin
  ExecuteEditingCommand(ecInsertParagraph);
end;

procedure THtmlEditorView.IncIndent;
begin
  ExecuteEditingCommand(ecIndent);
end;

procedure THtmlEditorView.DecIndent;
begin
  ExecuteEditingCommand(ecOutdent);
end;

procedure THtmlEditorView.Clear;
begin
  HtmlText := '';
end;

procedure THtmlEditorView.Cut;
begin
  ExecuteEditingCommand(ecCut);
end;

procedure THtmlEditorView.Copy;
begin
  ExecuteEditingCommand(ecCopy);
end;

procedure THtmlEditorView.Paste;
begin
  ExecuteEditingCommand(ecPaste);
end;

procedure THtmlEditorView.Undo;
begin
  ExecuteEditingCommand(ecUndo);
end;

procedure THtmlEditorView.Redo;
begin
  ExecuteEditingCommand(ecRedo);
end;
{$ENDREGION}

initialization
  GlobalWebView2Loader                := TWVLoader.Create(nil);
  GlobalWebView2Loader.UserDataFolder := UTF8Decode(
  ExtractFileDir(Application.ExeName) + '\CustomCache');
  GlobalWebView2Loader.StartWebView2;

end.
