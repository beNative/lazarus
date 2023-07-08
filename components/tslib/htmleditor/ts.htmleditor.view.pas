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
  LCLIntf, LCLType, LMessages, Messages,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  ActiveX,

  uWVBrowser, uWVWindowParent, uWVLoader, uWVTypes, uWVEvents, uWVTypeLibrary,
  uWVCoreWebView2DownloadOperation,

  DropComboTarget,

  ts.HtmlEditor.Interfaces;

type
  THtmlEditorView = class(TForm, IHtmlEditorView)
    dctMain        : TDropComboTarget;
    pnlHtmlEditor  : TPanel;
    Timer          : TTimer;
    WVBrowser      : TWVBrowser;
    WVWindowParent : TWVWindowParent;

    procedure TimerTimer(Sender: TObject);

    {$REGION 'event handlers'}
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
    procedure WVBrowserCapturePreviewCompleted(
      Sender     : TObject;
      AErrorCode : HRESULT
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
    procedure WVBrowserCompositionControllerCompleted(Sender: TObject);
    procedure WVBrowserContainsFullScreenElementChanged(Sender: TObject);
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
    procedure WVBrowserControllerCompleted(Sender: TObject);
    procedure WVBrowserCursorChanged(Sender: TObject);
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
    procedure WVBrowserEnvironmentCompleted(Sender: TObject);
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
    procedure WVBrowserFrameContentLoading(
      Sender       : TObject;
      const AFrame : ICoreWebView2Frame;
      const AArgs  : ICoreWebView2ContentLoadingEventArgs;
      AFrameID     : Integer
    );
    procedure WVBrowserFrameCreated(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2FrameCreatedEventArgs
    );
    procedure WVBrowserFrameNavigationCompleted(
      Sender         : TObject;
      const AWebView : ICoreWebView2;
      const AArgs    : ICoreWebView2NavigationCompletedEventArgs
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
    procedure WVBrowserRasterizationScaleChanged(Sender: TObject);
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
    procedure WVBrowserRetrieveMHTMLCompleted(
      Sender       : TObject;
      AResult      : Boolean;
      const AMHTML : wvstring
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
    procedure WVBrowserWidget0CompMsg(
      Sender       : TObject;
      var AMessage : TMessage;
      var AHandled : Boolean
    );
    procedure WVBrowserWidget1CompMsg(
      Sender       : TObject;
      var AMessage : TMessage;
      var AHandled : Boolean
    );
    procedure WVBrowserWindowCloseRequested(Sender: TObject);
    procedure WVBrowserZoomFactorChanged(Sender: TObject);
    {$ENDREGION}

  private
    FFileName          : string;
    FEditMode          : Boolean;
    FIsFile            : Boolean;
    FOnDropFiles       : TDropFilesEvent;
    FOnAfterCreated    : TNotifyEvent;
    FGetHeaders        : Boolean;
    FHeaders           : TStringList;
    FDownloadOperation : TCoreWebView2DownloadOperation;
    FHtmlText          : string;
    FMhtmlText         : string;
    FText              : string;
    FUpdate            : Boolean;
    FModified          : Boolean;

    FOffline                     : Boolean;
    FDefaultContextMenusEnabled  : Boolean;
    FDefaultScriptDialogsEnabled : Boolean;
    FDevToolsEnabled             : Boolean;
    FAllowHostObjects            : Boolean;
    FWebMessageEnabled           : Boolean;
    FZoomControlEnabled          : Boolean;
    FStatusBarEnabled            : Boolean;
    FScriptEnabled               : Boolean;

    function GetIsNavigating: Boolean;
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
    function GetMhtmlText: string;
    function GetModified: Boolean;
    function GetOffline: Boolean;
    function GetOnAfterCreated: TNotifyEvent;
    function GetOnChange: TNotifyEvent;
    function GetOnDropFiles: TDropFilesEvent;
    function GetScriptEnabled: Boolean;
    function GetSelAvail: Boolean;
    function GetSelEnd: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    function GetStatusBarEnabled: Boolean;
    function GetText: string;
    function GetUri: string;
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
    procedure SetMhtmlText(AValue: string);
    procedure SetModified(const AValue: Boolean);
    procedure SetOffline(AValue: Boolean);
    procedure SetOnAfterCreated(AValue: TNotifyEvent);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TDropFilesEvent);
    procedure SetPopupMenu(const AValue: TPopupMenu);
    procedure SetScriptEnabled(AValue: Boolean);
    procedure SetSelEnd(const AValue: Integer);
    procedure SetSelStart(const AValue: Integer);
    procedure SetSelText(const AValue: string);
    procedure SetStatusBarEnabled(AValue: Boolean);
    procedure SetText(const AValue: string);
    procedure SetUri(AValue: string);
    procedure SetWebMessageEnabled(AValue: Boolean);
    procedure SetZoomControlEnabled(AValue: Boolean);
    {$ENDREGION}

    procedure ApplySettings;
    procedure DoChange;
    procedure ShowDevTools;
    procedure ShowTaskManager;

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
    function ClearCache: Boolean;
    function Refresh: Boolean;

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
    property Bullets: Boolean
      read GetBullets write SetBullets;

    property Form: TCustomForm
      read GetForm;

    property EditMode: Boolean
      read GetEditMode write SetEditMode;

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

    property MhtmlText: string
      read GetMhtmlText write SetMhtmlText;

    property Text: string
      read GetText write SetText;

    property SelText: string
      read GetSelText write SetSelText;

    property SelStart: Integer
      read GetSelStart write SetSelStart;

    property SelEnd: Integer
      read GetSelEnd write SetSelEnd;

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

    property Uri: string
      read GetUri write SetUri;

    property OnAfterCreated: TNotifyEvent
      read GetOnAfterCreated write SetOnAfterCreated;

    property OnDropFiles: TDropFilesEvent
      read GetOnDropFiles write SetOnDropFiles;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property FileName: string
      read GetFileName write SetFileName;

  end;

implementation

{$R *.lfm}

uses
  uWVCoreWebView2Args, uWVCoreWebView2WebResourceResponseView,
  uWVCoreWebView2HttpResponseHeaders, uWVCoreWebView2HttpHeadersCollectionIterator,

  ts.Core.Utils, ts.Core.Logger, ts.Core.Helpers, ts.Core.Logger.Interfaces,
  ts.Core.ComponentInspector, ts.Core.Logger.Channel.IPC;

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

  FEditMode                    := True;

  if GlobalWebView2Loader.InitializationError then
    ShowMessage(UTF8Encode(GlobalWebView2Loader.ErrorMessage))
  else
    if GlobalWebView2Loader.Initialized then
      WVBrowser.CreateBrowser(WVWindowParent.Handle)
    else
      Timer.Enabled := True;

  BeginUpdate;
end;

destructor THtmlEditorView.Destroy;
begin
  FHeaders.Free;
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

function THtmlEditorView.GetIsNavigating: Boolean;
begin
  Result := WVBrowser.IsNavigating;
end;

procedure THtmlEditorView.WMMoving(var AMessage: TMessage);
begin
  inherited;
  if (WVBrowser <> nil) then
      WVBrowser.NotifyParentWindowPositionChanged;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure THtmlEditorView.WVBrowserAfterCreated(Sender: TObject);
begin
  Logger.Enter(Self, 'WVBrowserAfterCreated');
  WVWindowParent.UpdateSize;
  //WVBrowser.SetVirtualHostNameToFolderMapping('customhost.test', '.', COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND_ALLOW);

  // This code enables the editor mode when a web page is loaded
  //TempScript := 'document.designMode = "on";';
  //WVBrowser.AddScriptToExecuteOnDocumentCreated(TempScript);

  //WVBrowser.AddWebResourceRequestedFilter('*', COREWEBVIEW2_WEB_RESOURCE_CONTEXT_IMAGE);
  //EndUpdate;
  //ApplySettings;
  if Assigned(FOnAfterCreated) then
    FOnAfterCreated(Self);
  ApplySettings;
  Logger.Leave(Self, 'WVBrowserAfterCreated');
end;

procedure THtmlEditorView.WVBrowserBasicAuthenticationRequested
  (Sender: TObject; const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2BasicAuthenticationRequestedEventArgs);
begin
  Logger.Enter(Self, 'WVBrowserBasicAuthenticationRequested');
  Logger.Leave(Self, 'WVBrowserBasicAuthenticationRequested');
end;

procedure THtmlEditorView.WVBrowserBrowserProcessExited(Sender: TObject;
  const AEnvironment: ICoreWebView2Environment;
  const AArgs: ICoreWebView2BrowserProcessExitedEventArgs);
begin
  Logger.Enter(Self, 'WVBrowserBrowserProcessExited');
  Logger.Leave(Self, 'WVBrowserBrowserProcessExited');
end;

procedure THtmlEditorView.WVBrowserBytesReceivedChanged(Sender: TObject;
  const ADownloadOperation: ICoreWebView2DownloadOperation; ADownloadID: Integer
  );
begin
  Logger.Track(Self, 'WVBrowserBytesReceivedChanged');
end;

procedure THtmlEditorView.WVBrowserCallDevToolsProtocolMethodCompleted
  (Sender: TObject; AErrorCode: HRESULT; const AReturnObjectAsJson: wvstring;
  AExecutionID: Integer);
begin
  Logger.Track(Self, 'WVBrowserCallDevToolsProtocolMethodCompleted');
end;

procedure THtmlEditorView.WVBrowserCapturePreviewCompleted(Sender: TObject;
  AErrorCode: HRESULT);
begin
  //
end;

procedure THtmlEditorView.WVBrowserClearBrowsingDataCompleted(Sender: TObject;
  AErrorCode: HRESULT);
begin
  Logger.Track(Self, 'WVBrowserClearBrowsingDataCompleted');
end;

procedure THtmlEditorView.WVBrowserClearCacheCompleted(Sender: TObject;
  AResult: Boolean);
begin
  Logger.Track(Self, 'WVBrowserClearCacheCompleted');
end;

procedure THtmlEditorView.WVBrowserClearDataForOriginCompleted(Sender: TObject;
  AResult: Boolean);
begin
  Logger.Track(Self, 'WVBrowserClearDataForOriginCompleted');
end;

procedure THtmlEditorView.WVBrowserCompositionControllerCompleted
  (Sender: TObject);
begin
  Logger.Track(Self, 'WVBrowserCompositionControllerCompleted');
end;

procedure THtmlEditorView.WVBrowserContainsFullScreenElementChanged
  (Sender: TObject);
begin
  Logger.Track(Self, 'WVBrowserContainsFullScreenElementChanged');
end;

procedure THtmlEditorView.WVBrowserContentLoading(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2ContentLoadingEventArgs);
begin
  Logger.Info('WVBrowserContentLoading');
end;

procedure THtmlEditorView.WVBrowserContextMenuRequested(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2ContextMenuRequestedEventArgs);
begin
  Logger.Track(Self, 'WVBrowserContextMenuRequested');
end;

procedure THtmlEditorView.WVBrowserControllerCompleted(Sender: TObject);
begin
  Logger.Track(Self, 'WVBrowserControllerCompleted');
end;

procedure THtmlEditorView.WVBrowserCursorChanged(Sender: TObject);
begin
  Logger.Track(Self, 'WVBrowserCursorChanged');
end;

procedure THtmlEditorView.WVBrowserCustomItemSelected(Sender: TObject;
  const AMenuItem: ICoreWebView2ContextMenuItem);
begin
  Logger.Track(Self, 'WVBrowserCustomItemSelected');
end;

procedure THtmlEditorView.WVBrowserDevToolsProtocolEventReceived
  (Sender: TObject; const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2DevToolsProtocolEventReceivedEventArgs;
  const AEventName: wvstring; AEventID: Integer);
begin
  Logger.Track(Self, 'WVBrowserDevToolsProtocolEventReceived');
end;

procedure THtmlEditorView.WVBrowserDOMContentLoaded(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2DOMContentLoadedEventArgs);
begin
  Logger.Info('WVBrowserDOMContentLoaded');
  WVBrowser.RetrieveHTML;
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
  Logger.Info('WVBrowserDownloadStarting');
end;

procedure THtmlEditorView.WVBrowserDownloadStateChanged(Sender: TObject;
  const ADownloadOperation: ICoreWebView2DownloadOperation; ADownloadID: Integer
  );
begin
  Logger.Track(Self, 'WVBrowserDownloadStateChanged');
end;

procedure THtmlEditorView.WVBrowserEnvironmentCompleted(Sender: TObject);
begin
  Logger.Track(Self, 'WVBrowserEnvironmentCompleted');
end;

procedure THtmlEditorView.WVBrowserEstimatedEndTimeChanged(Sender: TObject;
  const ADownloadOperation: ICoreWebView2DownloadOperation; ADownloadID: Integer
  );
begin
  Logger.Track(Self, 'WVBrowserEstimatedEndTimeChanged');
end;

procedure THtmlEditorView.WVBrowserExecuteScriptCompleted(Sender: TObject;
  AErrorCode: HRESULT; const AResultObjectAsJson: wvstring;
  AExecutionID: Integer);
begin
  Logger.Enter(Self, 'WVBrowserExecuteScriptCompleted');
  Logger.Send('AResultObjectAsJson', AResultObjectAsJson);
  Logger.Send('AExecutionID', AExecutionID);
  Logger.Leave(Self, 'WVBrowserExecuteScriptCompleted');
end;

procedure THtmlEditorView.WVBrowserFaviconChanged(Sender: TObject;
  const AWebView: ICoreWebView2; const AArgs: IUnknown);
begin
  //Logger.Track(Self, 'WVBrowserFaviconChanged');
end;

procedure THtmlEditorView.WVBrowserFrameContentLoading(Sender: TObject;
  const AFrame: ICoreWebView2Frame;
  const AArgs: ICoreWebView2ContentLoadingEventArgs; AFrameID: Integer);
begin
  Logger.Track(Self, 'WVBrowserFrameContentLoading');
end;

procedure THtmlEditorView.WVBrowserFrameCreated(Sender: TObject;
  const AWebView: ICoreWebView2; const AArgs: ICoreWebView2FrameCreatedEventArgs
  );
begin
  Logger.Track(Self, 'WVBrowserFrameCreated');
end;

procedure THtmlEditorView.WVBrowserFrameNavigationCompleted(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2NavigationCompletedEventArgs);
var
  LNavigationId : QWord;
  LIsSuccess    : Integer;
begin
  Logger.Enter(Self, 'WVBrowserFrameNavigationCompleted');

  Logger.Leave(Self, 'WVBrowserFrameNavigationCompleted');
end;

procedure THtmlEditorView.WVBrowserGetCookiesCompleted(Sender: TObject;
  AResult: HRESULT; const ACookieList: ICoreWebView2CookieList);
begin
  Logger.Track(Self, 'WVBrowserGetCookiesCompleted');
end;

procedure THtmlEditorView.WVBrowserGetCustomSchemes(Sender: TObject;
  var ACustomSchemes: TWVCustomSchemeInfoArray);
begin
  Logger.Track(Self, 'WVBrowserGetCustomSchemes');
end;

procedure THtmlEditorView.WVBrowserGetFaviconCompleted(Sender: TObject;
  AErrorCode: HRESULT; const AFaviconStream: IStream);
begin
  Logger.Track(Self, 'WVBrowserGetFaviconCompleted');
end;

procedure THtmlEditorView.WVBrowserGetNonDefaultPermissionSettingsCompleted
  (Sender: TObject; AErrorCode: HRESULT;
  const aCollectionView: ICoreWebView2PermissionSettingCollectionView);
begin
  Logger.Track(Self, 'WVBrowserGetNonDefaultPermissionSettingsCompleted');
end;

procedure THtmlEditorView.WVBrowserGotFocus(Sender: TObject);
begin
  Logger.Track(Self, 'WVBrowserGotFocus');
end;

procedure THtmlEditorView.WVBrowserHistoryChanged(Sender: TObject);
begin
  Logger.Track(Self, 'WVBrowserHistoryChanged');
end;

procedure THtmlEditorView.WVBrowserInitializationError(Sender: TObject;
  AErrorCode: HRESULT; const AErrorMessage: wvstring);
begin
  Logger.Error(UTF8Encode(AErrorMessage));
end;

procedure THtmlEditorView.WVBrowserIsMutedChanged(Sender: TObject;
  const aWebView: ICoreWebView2);
begin
  Logger.Track(Self, 'WVBrowserIsMutedChanged');
end;

procedure THtmlEditorView.WVBrowserLaunchingExternalUriScheme(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2LaunchingExternalUriSchemeEventArgs);
begin
  Logger.Track(Self, 'WVBrowserLaunchingExternalUriScheme');
end;

procedure THtmlEditorView.WVBrowserLostFocus(Sender: TObject);
begin
  Logger.Track(Self, 'WVBrowserLostFocus');
end;

procedure THtmlEditorView.WVBrowserMoveFocusRequested(Sender: TObject;
  const AController: ICoreWebView2Controller;
  const AArgs: ICoreWebView2MoveFocusRequestedEventArgs);
begin
  Logger.Track(Self, 'WVBrowserMoveFocusRequested');
end;

{ stop loading }

procedure THtmlEditorView.WVBrowserNavigationCompleted(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2NavigationCompletedEventArgs);
var
  LArgs : TCoreWebView2NavigationCompletedEventArgs;
begin
  Logger.Enter(Self, 'WVBrowserNavigationCompleted');
  LArgs := TCoreWebView2NavigationCompletedEventArgs.Create(AArgs);
  try
    LArgs.HttpStatusCode;
    Logger.Send('HttpStatusCode', LArgs.HttpStatusCode);
    Logger.Send('Initialized', LArgs.Initialized);
    Logger.Send('IsSuccess', LArgs.IsSuccess);
    Logger.Send('WebErrorStatus', LArgs.WebErrorStatus);

    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Status
    //LStatus := LArgs.HttpStatusCode;
    WVBrowser.RetrieveHTML;
    WVBrowser.RetrieveText;
  finally
    LArgs.Free;
  end;
  Logger.Leave(Self, 'WVBrowserNavigationCompleted');
end;

{ start loading }

procedure THtmlEditorView.WVBrowserNavigationStarting(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2NavigationStartingEventArgs);
var
  LArgs : TCoreWebView2NavigationStartingEventArgs;
begin
  Logger.Enter(Self, 'WVBrowserNavigationStarting');
  LArgs := TCoreWebView2NavigationStartingEventArgs.Create(AArgs);
  try
    FGetHeaders := True;
    Logger.Send('NavigationId', LArgs.NavigationID);
    Logger.Send('Uri', LArgs.URI);
    Logger.Send('IsUserInitiated', LArgs.IsUserInitiated);
    Logger.Send('IsRedirected', LArgs.IsRedirected);

  finally
    LArgs.Free;
  end;
  Logger.Leave(Self, 'WVBrowserNavigationStarting');
end;

procedure THtmlEditorView.WVBrowserNewWindowRequested(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2NewWindowRequestedEventArgs);
begin
  Logger.Track(Self, 'WVBrowserNewWindowRequested');
end;

procedure THtmlEditorView.WVBrowserOfflineCompleted(Sender: TObject;
  AResult: Boolean);
begin
  Logger.Track(Self, 'WVBrowserOfflineCompleted');
  FOffline := AResult;
end;

procedure THtmlEditorView.WVBrowserPermissionRequested(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2PermissionRequestedEventArgs);
begin
  Logger.Track(Self, 'WVBrowserPermissionRequested');
end;

procedure THtmlEditorView.WVBrowserProcessFailed(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2ProcessFailedEventArgs);
begin
  Logger.Track(Self, 'WVBrowserProcessFailed');

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
  Logger.Track(Self, 'WVBrowserProcessInfosChanged');
end;

procedure THtmlEditorView.WVBrowserRasterizationScaleChanged(Sender: TObject);
begin
  Logger.Track(Self, 'WVBrowserRasterizationScaleChanged');
end;

procedure THtmlEditorView.WVBrowserRefreshIgnoreCacheCompleted(Sender: TObject;
  AErrorCode: HRESULT; const AResultObjectAsJson: wvstring);
begin
  Logger.Track(Self, 'WVBrowserRefreshIgnoreCacheCompleted');
end;

procedure THtmlEditorView.WVBrowserRenderCompMsg(Sender: TObject;
  var AMessage: TMessage; var AHandled: Boolean);
begin
  Logger.Track(Self, 'WVBrowserRenderCompMsg');
end;

procedure THtmlEditorView.WVBrowserRetrieveHTMLCompleted(Sender: TObject;
  AResult: Boolean; const AHTML: wvstring);
begin
  //Logger.Track(Self, 'WVBrowserRetrieveHTMLCompleted');
  if AResult then
    HtmlText := UTF8Encode(AHTML);
end;

procedure THtmlEditorView.WVBrowserRetrieveMHTMLCompleted(Sender: TObject;
  AResult: Boolean; const AMHTML: wvstring);
begin
  //Logger.Track(Self, 'WVBrowserRetrieveMHTMLCompleted');
  if AResult then
    MhtmlText := UTF8Encode(AMHTML);
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
  Logger.Track(Self, 'WVBrowserScriptDialogOpening');
end;

procedure THtmlEditorView.WVBrowserServerCertificateErrorActionsCompleted
  (Sender: TObject; AErrorCode: HRESULT);
begin
  Logger.Track(Self, 'WVBrowserServerCertificateErrorActionsCompleted');
end;

procedure THtmlEditorView.WVBrowserServerCertificateErrorDetected
  (Sender: TObject; const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2ServerCertificateErrorDetectedEventArgs);
begin
  Logger.Track(Self, 'WVBrowserServerCertificateErrorDetected');
end;

procedure THtmlEditorView.WVBrowserSetPermissionStateCompleted(Sender: TObject;
  AErrorCode: HRESULT);
begin
  Logger.Track(Self, 'WVBrowserSetPermissionStateCompleted');
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
  Logger.Enter(Self, 'WVBrowserSourceChanged');
  LArgs := TCoreWebView2SourceChangedEventArgs.Create(AArgs);
  try
    Logger.Send('Initialized', LArgs.Initialized);
    Logger.Send('IsNewDocument', LArgs.IsNewDocument);
  finally
    LArgs.Free;
  end;
  Logger.Leave(Self, 'WVBrowserSourceChanged');
end;

procedure THtmlEditorView.WVBrowserStatusBarTextChanged(Sender: TObject;
  const AWebView: ICoreWebView2);
begin
  //Logger.Track(Self, 'WVBrowserStatusBarTextChanged');
end;

procedure THtmlEditorView.WVBrowserTrySuspendCompleted(Sender: TObject;
  AErrorCode: HRESULT; AIsSuccessful: Boolean);
begin
  Logger.Track(Self, 'WVBrowserTrySuspendCompleted');
end;

procedure THtmlEditorView.WVBrowserWebMessageReceived(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2WebMessageReceivedEventArgs);
var
  LArgs : TCoreWebView2WebMessageReceivedEventArgs;
begin
  Logger.Enter(Self, 'WVBrowserWebMessageReceived');
  LArgs := TCoreWebView2WebMessageReceivedEventArgs.Create(AArgs);
  try
    Logger.Send('Initialized', LArgs.Initialized);
    Logger.Send('Source', LArgs.Source);
    Logger.Send('WebMessageAsJson', LArgs.WebMessageAsJson);
    Logger.Send('WebMessageAsString', LArgs.WebMessageAsString);
    //Logger.Send('AdditionalObjects', LArgs.AdditionalObjects);
  finally
    LArgs.Free;
  end;
  Logger.Leave(Self, 'WVBrowserWebMessageReceived');
end;

procedure THtmlEditorView.WVBrowserWebResourceRequested(Sender: TObject;
  const AWebView: ICoreWebView2;
  const AArgs: ICoreWebView2WebResourceRequestedEventArgs);
var
  LArgs     : TCoreWebView2WebResourceRequestedEventArgs;
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

procedure THtmlEditorView.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;

  if GlobalWebView2Loader.Initialized then
    WVBrowser.CreateBrowser(WVWindowParent.Handle)
  else
    Timer.Enabled := True;
end;

procedure THtmlEditorView.WVBrowserAcceleratorKeyPressed(Sender: TObject;
  const AController: ICoreWebView2Controller;
  const AArgs: ICoreWebView2AcceleratorKeyPressedEventArgs);
begin
  Logger.Info('WVBrowserAcceleratorKeyPressed');
end;

procedure THtmlEditorView.WVBrowserAddScriptToExecuteOnDocumentCreatedCompleted
  (Sender: TObject; AErrorCode: HRESULT; const AID: wvstring);
begin
  Logger.Track(Self, 'WVBrowserAddScriptToExecuteOnDocumentCreatedCompleted');
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
  Logger.Track(Self, 'WVBrowserWebResourceResponseViewGetContentCompleted');
end;

procedure THtmlEditorView.WVBrowserWidget0CompMsg(Sender: TObject;
  var AMessage: TMessage; var AHandled: Boolean);
begin
  Logger.Track(Self, 'WVBrowserWidget0CompMsg');
  //Logger.Send('AMessage', AMessage);
end;

procedure THtmlEditorView.WVBrowserWidget1CompMsg(Sender: TObject;
  var AMessage: TMessage; var AHandled: Boolean);
begin
Logger.Track(Self, 'WVBrowserWidget1CompMsg');
end;

procedure THtmlEditorView.WVBrowserWindowCloseRequested(Sender: TObject);
begin
  Logger.Track(Self, 'WVBrowserWindowCloseRequested');
end;

procedure THtmlEditorView.WVBrowserZoomFactorChanged(Sender: TObject);
begin
  Logger.Track(Self, 'WVBrowserZoomFactorChanged');
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure THtmlEditorView.DoChange;
begin
  FUpdate   := True;
  FModified := True;
end;
{$ENDREGION}

{$REGION 'property access methods'}
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

function THtmlEditorView.GetMhtmlText: string;
begin
  Result := FMhtmlText;
end;

procedure THtmlEditorView.SetMhtmlText(AValue: string);
begin
  if AValue <> MhtmlText then
  begin
    FMhtmlText := AValue;
//    WVBrowser.NavigateToString(UTF8Decode(AValue));
    DoChange;
  end;
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
begin

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
  //Result := FOnChange;
end;

function THtmlEditorView.GetOnDropFiles: TDropFilesEvent;
begin
  Result := FOnDropFiles;
end;

function THtmlEditorView.GetSelAvail: Boolean;
begin

end;

function THtmlEditorView.GetSelEnd: Integer;
begin

end;

function THtmlEditorView.GetSelStart: Integer;
begin

end;

function THtmlEditorView.GetSelText: string;
begin

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
    DoChange;
  end;
end;

procedure THtmlEditorView.SetIsFile(AValue: Boolean);
begin

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

end;

procedure THtmlEditorView.SetOnDropFiles(const AValue: TDropFilesEvent);
begin

end;

procedure THtmlEditorView.SetPopupMenu(const AValue: TPopupMenu);
begin
  WVWindowParent.PopupMenu := AValue;
end;

procedure THtmlEditorView.SetSelEnd(const AValue: Integer);
begin

end;

procedure THtmlEditorView.SetSelStart(const AValue: Integer);
begin

end;

procedure THtmlEditorView.SetSelText(const AValue: string);
begin

end;

procedure THtmlEditorView.SetText(const AValue: string);
begin
  if AValue <> Text then
  begin
    FText := AValue;
    DoChange;
  end;
end;

function THtmlEditorView.GetUri: string;
begin
  Result := UTF8Encode(WVBrowser.Source);
end;

procedure THtmlEditorView.SetUri(AValue: string);
begin
  WVBrowser.Navigate(UTF8Decode(AValue));
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure THtmlEditorView.ApplySettings;
begin
  if FEditMode then
    ExecuteScript('document.designMode = "on";')
  else
    ExecuteScript('document.designMode = "off";');
  WVBrowser.DefaultContextMenusEnabled  := FDefaultContextMenusEnabled;
  WVBrowser.DefaultScriptDialogsEnabled := FDefaultScriptDialogsEnabled;
  //WVBrowser.NavigateToString(UTF8Decode(FHtmlText));
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
  if FUpdate then
  begin
    ApplySettings;
    FUpdate := False;
  end;
  UpdateWatches;
end;

procedure THtmlEditorView.UpdateWatches;
begin
  Logger.Watch('IsInitialized', IsInitialized);
  Logger.Watch('Modified', Modified);
  Logger.Watch('FUpdate', FUpdate);
  Logger.Watch('IsNavigating', IsNavigating);
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
begin
  if (Length(AFileName) > 0) and FileExists(AFileName) then
  begin
      // TODO: The Filename should be encoded
    WVBrowser.Navigate(UTF8Decode('file:///' + AFileName));
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
  SelectAll;
  ExecuteEditingCommand(ecDelete);
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

