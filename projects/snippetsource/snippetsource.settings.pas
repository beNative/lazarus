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

unit SnippetSource.Settings;

{$MODE DELPHI}

interface

{ Application settings object for SnippetSource. }

uses
  Classes, SysUtils,
  LazMethodList,

  ts.Editor.Settings, ts.Editor.Interfaces,

  SnippetSource.Interfaces, SnippetSource.Resources;

type
  TSettings = class(TComponent, ISettings)
  private
    FFileName                     : string;
    FDatabase                     : string;
    FDebugMode                    : Boolean;
    FHtmlEditMode                 : Boolean;
    FHtmlSourceVisible            : Boolean;
    FAutoHideEditorToolBar        : Boolean;
    FAutoHideRichEditorToolBar    : Boolean;
    FChangeEvents                 : TMethodList;
    FLastFocusedId                : Integer;
    FDefaultRichEditorFontName    : string;
    FEmitLogMessages              : Boolean;
    FPythonVirtualEnvironmentName : string;
    FTextEditorSplitViewEnabled   : Boolean;
    FTextEditorSettings           : IEditorSettings;

  protected
    {$REGION 'property access methods'}
    function GetAutoHideEditorToolBar: Boolean;
    function GetAutoHideRichEditorToolBar: Boolean;
    function GetDatabase: string;
    function GetDebugMode: Boolean;
    function GetDefaultRichEditorFontName: string;
    function GetEmitLogMessages: Boolean;
    function GetFileName: string;
    function GetHtmlEditMode: Boolean;
    function GetHtmlSourceVisible: Boolean;
    function GetLastFocusedId: Integer;
    function GetPythonVirtualEnvironmentName: string;
    function GetTextEditorSettings: TEditorSettings;
    function GetTextEditorSplitViewEnabled: Boolean;
    procedure SetAutoHideEditorToolBar(AValue: Boolean);
    procedure SetAutoHideRichEditorToolBar(AValue: Boolean);
    procedure SetDatabase(const AValue: string);
    procedure SetDebugMode(AValue: Boolean);
    procedure SetDefaultRichEditorFontName(AValue: string);
    procedure SetEmitLogMessages(AValue: Boolean);
    procedure SetFileName(const AValue: string);
    procedure SetHtmlEditMode(AValue: Boolean);
    procedure SetHtmlSourceVisible(AValue: Boolean);
    procedure SetLastFocusedId(AValue: Integer);
    procedure SetPythonVirtualEnvironmentName(AValue: string);
    procedure SetTextEditorSettings(AValue: TEditorSettings);
    procedure SetTextEditorSplitViewEnabled(AValue: Boolean);
    {$ENDREGION}

    procedure Changed;
    procedure AddOnChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveOnChangeHandler(AEvent: TNotifyEvent);

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    function ToJson: string;

    procedure Load;
    procedure Save;

    { Settings file name }
    property FileName: string
      read GetFileName write SetFileName;

    property Name;
    property Tag;

  published
    { Database file name }
    property Database: string
      read GetDatabase write SetDatabase;

    property LastFocusedId: Integer
      read GetLastFocusedId write SetLastFocusedId;

    property AutoHideEditorToolBar: Boolean
      read GetAutoHideEditorToolBar write SetAutoHideEditorToolBar default True;

    property AutoHideRichEditorToolBar: Boolean
      read GetAutoHideRichEditorToolBar write SetAutoHideRichEditorToolBar;

    property DebugMode: Boolean
      read GetDebugMode write SetDebugMode;

    property DefaultRichEditorFontName: string
      read GetDefaultRichEditorFontName write SetDefaultRichEditorFontName;

    property HtmlEditMode: Boolean
      read GetHtmlEditMode write SetHtmlEditMode;

    property HtmlSourceVisible: Boolean
      read GetHtmlSourceVisible write SetHtmlSourceVisible;

    property PythonVirtualEnvironmentName: string
      read GetPythonVirtualEnvironmentName write SetPythonVirtualEnvironmentName;

    property EmitLogMessages: Boolean
      read GetEmitLogMessages write SetEmitLogMessages;

    property TextEditorSettings: TEditorSettings
      read GetTextEditorSettings write SetTextEditorSettings;

    property TextEditorSplitViewEnabled: Boolean
      read GetTextEditorSplitViewEnabled write SetTextEditorSplitViewEnabled;

  end;

implementation

uses
  Dialogs,

  ts.Core.Logger, ts.Editor.Factories,

  fpjsonrtti, FileUtil;

{$REGION 'construction and destruction'}
procedure TSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FTextEditorSettings    := TEditorFactories.CreateSettings(Self);
  FChangeEvents          := TMethodList.Create;
  Name                   := 'Settings';
  FFileName              := SETTINGS_FILE;
  Database               := DEFAULT_DATABASE_NAME;
  FAutoHideEditorToolBar := True;
end;

destructor TSettings.Destroy;
begin
  FChangeEvents.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TSettings.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TSettings.SetFileName(const AValue: string);
begin
  if AValue <> FileName then
  begin
    FFileName := AValue;
    Changed;
  end;
end;

function TSettings.GetTextEditorSplitViewEnabled: Boolean;
begin
  Result := FTextEditorSplitViewEnabled;
end;

procedure TSettings.SetTextEditorSplitViewEnabled(AValue: Boolean);
begin
  if AValue <> TextEditorSplitViewEnabled then
  begin
    FTextEditorSplitViewEnabled := AValue;
    Changed;
  end;
end;

function TSettings.GetTextEditorSettings: TEditorSettings;
begin
  Result := FTextEditorSettings as TEditorSettings;
end;

procedure TSettings.SetTextEditorSettings(AValue: TEditorSettings);
begin
  (FTextEditorSettings as TEditorSettings).Assign(AValue);
end;

function TSettings.GetAutoHideEditorToolBar: Boolean;
begin
  Result := FAutoHideEditorToolBar;
end;

procedure TSettings.SetAutoHideEditorToolBar(AValue: Boolean);
begin
  if AValue <> AutoHideEditorToolBar then
  begin
    FAutoHideEditorToolBar :=  AValue;
    Changed;
  end;
end;

function TSettings.GetAutoHideRichEditorToolBar: Boolean;
begin
  Result := FAutoHideRichEditorToolBar;
end;

procedure TSettings.SetAutoHideRichEditorToolBar(AValue: Boolean);
begin
  if AValue <> AutoHideRichEditorToolBar then
  begin
    FAutoHideRichEditorToolBar := AValue;
    Changed;
  end;
end;

function TSettings.GetLastFocusedId: Integer;
begin
  Result := FLastFocusedId;
end;

procedure TSettings.SetLastFocusedId(AValue: Integer);
begin
  if AValue <> LastFocusedId then
  begin
    FLastFocusedId := AValue;
  end;
end;

function TSettings.GetDefaultRichEditorFontName: string;
begin
  Result := FDefaultRichEditorFontName;
end;

procedure TSettings.SetDefaultRichEditorFontName(AValue: string);
begin
  if AValue <> DefaultRichEditorFontName then
  begin
    FDefaultRichEditorFontName := AValue;
    Changed;
  end;
end;

function TSettings.GetHtmlEditMode: Boolean;
begin
  Result := FHtmlEditMode;
end;

function TSettings.GetHtmlSourceVisible: Boolean;
begin
  Result := FHtmlSourceVisible;
end;

procedure TSettings.SetHtmlEditMode(AValue: Boolean);
begin
  if AValue <> HtmlEditMode then
  begin
    FHtmlEditMode := AValue;
    Changed;
  end;
end;

procedure TSettings.SetHtmlSourceVisible(AValue: Boolean);
begin
  if AValue <> HtmlSourceVisible then
  begin
    FHtmlSourceVisible := AValue;
    Changed;
  end;
end;

function TSettings.GetDebugMode: Boolean;
begin
  Result := FDebugMode;
end;

procedure TSettings.SetDebugMode(AValue: Boolean);
begin
  if AValue <> DebugMode then
  begin
    FDebugMode := AValue;
    Changed;
  end;
end;

function TSettings.GetPythonVirtualEnvironmentName: string;
begin
  Result := FPythonVirtualEnvironmentName;
end;

procedure TSettings.SetPythonVirtualEnvironmentName(AValue: string);
begin
  if AValue <> PythonVirtualEnvironmentName then
  begin
    FPythonVirtualEnvironmentName := AValue;
    Changed;
  end;
end;

function TSettings.GetEmitLogMessages: Boolean;
begin
  Result := FEmitLogMessages;
end;

procedure TSettings.SetEmitLogMessages(AValue: Boolean);
begin
  if AValue <> EmitLogMessages then
  begin
    FEmitLogMessages := AValue;
    Changed;
  end;
end;

function TSettings.GetDatabase: string;
begin
  Result := FDatabase;
end;

procedure TSettings.SetDatabase(const AValue: string);
begin
  if AValue <> Database then
  begin
    FDatabase := AValue;
    Changed;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TSettings.Changed;
begin
  FChangeEvents.CallNotifyEvents(Self);
end;

procedure TSettings.AddOnChangeHandler(AEvent: TNotifyEvent);
begin
  FChangeEvents.Add(TMethod(AEvent));
end;

procedure TSettings.RemoveOnChangeHandler(AEvent: TNotifyEvent);
begin
  FChangeEvents.Remove(TMethod(AEvent));
end;
{$ENDREGION}

{$REGION 'public methods'}
function TSettings.ToJson: string;
var
  LStreamer : TJSONStreamer;
begin
  LStreamer := TJSONStreamer.Create(nil);
  try
    LStreamer.Options :=  LStreamer.Options + [jsoComponentsInline];
    Result := LStreamer.ObjectToJSON(Self).FormatJSON;
  finally
    LStreamer.Free;
  end;
end;

procedure TSettings.Load;
var
  LDeStreamer : TJSONDeStreamer;
  S           : string;
begin
  if FileExists(FileName) then
  begin
    S := ReadFileToString(FileName);
    if not S.IsEmpty then
    begin;
      LDeStreamer := TJSONDeStreamer.Create(nil);
      try
        LDeStreamer.JSONToObject(S, Self);
        Logger.SendComponent('TextEditorSettings', TextEditorSettings);
      finally
        LDeStreamer.Free;
      end;
    end;
  end;
end;

procedure TSettings.Save;
var
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := ToJson;
    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;
{$ENDREGION}

end.

