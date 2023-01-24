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

  SnippetSource.Interfaces, SnippetSource.Resources;

type
  TSettings = class(TComponent, ISettings)
  private
    FFileName                  : string;
    FDatabase                  : string;
    FAutoHideRichEditor        : Boolean;
    FAutoHideEditor            : Boolean;
    FAutoHideEditorToolBar     : Boolean;
    FAutoHideRichEditorToolBar : Boolean;
    FChangeEvents              : TMethodList;
    FLastFocusedId             : Integer;
    FDefaultRichEditorFontName : string;
    FEmitLogMessages           : Boolean;
    FTrackHistory              : Boolean;

  protected
    {$REGION 'property access methods'}
    function GetEmitLogMessages: Boolean;
    procedure SetEmitLogMessages(AValue: Boolean);
    function GetAutoHidEditor: Boolean;
    procedure SetAutoHideEditor(AValue: Boolean);
    function GetAutoHideRichEditor: Boolean;
    procedure SetAutoHideRichEditor(AValue: Boolean);
    function GetAutoHideEditorToolBar: Boolean;
    procedure SetAutoHideEditorToolBar(AValue: Boolean);
    function GetAutoHideRichEditorToolBar: Boolean;
    procedure SetAutoHideRichEditorToolBar(AValue: Boolean);
    function GetDatabase: string;
    procedure SetDatabase(const AValue: string);
    function GetFileName: string;
    procedure SetFileName(const AValue: string);
    function GetLastFocusedId: Integer;
    procedure SetLastFocusedId(AValue: Integer);
    function GetDefaultRichEditorFontName: string;
    procedure SetDefaultRichEditorFontName(AValue: string);
    function GetTrackHistory: Boolean;
    procedure SetTrackHistory(AValue: Boolean);
    {$ENDREGION}

    procedure Changed;
    procedure AddOnChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveOnChangeHandler(AEvent: TNotifyEvent);

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

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

    property AutoHideEditor: Boolean
      read GetAutoHidEditor write SetAutoHideEditor;

    property AutoHideRichEditorToolBar: Boolean
      read GetAutoHideRichEditorToolBar write SetAutoHideRichEditorToolBar;

    property AutoHideRichEditor: Boolean
      read GetAutoHideRichEditor write SetAutoHideRichEditor;

    property DefaultRichEditorFontName: string
      read GetDefaultRichEditorFontName write SetDefaultRichEditorFontName;

    property EmitLogMessages: Boolean
      read GetEmitLogMessages write SetEmitLogMessages;

    property TrackHistory: Boolean
      read GetTrackHistory write SetTrackHistory;
  end;

implementation

uses
  Dialogs,

  fpjsonrtti;

{$REGION 'construction and destruction'}
procedure TSettings.AfterConstruction;
begin
  inherited AfterConstruction;
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

function TSettings.GetTrackHistory: Boolean;
begin
  Result := FTrackHistory;
end;

procedure TSettings.SetTrackHistory(AValue: Boolean);
begin
  if AValue <> TrackHistory then
  begin
    FTrackHistory := AValue;
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

function TSettings.GetAutoHidEditor: Boolean;
begin
  Result := FAutoHideEditor;
end;

procedure TSettings.SetAutoHideEditor(AValue: Boolean);
begin
  if AValue <> AutoHideEditor then
  begin
    FAutoHideEditor := AValue;
    Changed;
  end;
end;

function TSettings.GetAutoHideRichEditor: Boolean;
begin
  Result := FAutoHideRichEditor;
end;

procedure TSettings.SetAutoHideRichEditor(AValue: Boolean);
begin
  if AValue <> AutoHideRichEditor then
  begin
    FAutoHideRichEditor := AValue;
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
procedure TSettings.Load;
var
  LDeStreamer : TJSONDeStreamer;
  SL          : TStringList;
  S           : string;
begin
  if FileExists(FileName) then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(FileName);
      S := SL.Text;
    finally
      SL.Free;
    end;
    LDeStreamer := TJSONDeStreamer.Create(nil);
    try
      LDeStreamer.JSONToObject(S, Self);
    finally
      LDeStreamer.Free;
    end;
  end;
end;

procedure TSettings.Save;
var
  LStreamer : TJSONStreamer;
  S         : string;
  SL        : TStringList;
begin
  LStreamer := TJSONStreamer.Create(nil);
  try
    LStreamer.Options :=  LStreamer.Options + [jsoComponentsInline];
    S  := LStreamer.ObjectToJSON(Self).FormatJSON;
    SL := TStringList.Create;
    try
      SL.Text := S;
      SL.SaveToFile(FileName);
    finally
      SL.Free;
    end;
  finally
    LStreamer.Free;
  end;
end;
{$ENDREGION}

end.

