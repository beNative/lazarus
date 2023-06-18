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

unit ts.RichEditor.Events;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils,

  LazMethodList,

  ts.Core.Types, ts.RichEditor.Interfaces;

type
  TRichEditorEvents = class(TInterfacedObject, IRichEditorEvents)
  private
    FManager                  : IRichEditorManager;
    FChangeEvents             : TMethodList;
    FModifiedEvents           : TMethodList;
    FSelectBlockEvents        : TMethodList;
    FOnNew                    : TNewEvent;
    FOnLoad                   : TStorageEvent;
    FOnOpen                   : TStorageEvent;
    FOnBeforeSave             : TStorageEvent;
    FOnAfterSave              : TStorageEvent;
    FOnSave                   : TStorageEvent;
    FOnShowRichEditorToolView : TRichEditorToolViewEvent;
    FOnHideRichEditorToolView : TRichEditorToolViewEvent;

    {$REGION 'property access mehods'}
    function GetOnAfterSave: TStorageEvent;
    function GetOnBeforeSave: TStorageEvent;
    function GetOnHideRichEditorToolView: TRichEditorToolViewEvent;
    function GetOnLoad: TStorageEvent;
    function GetOnNew: TNewEvent;
    function GetOnOpen: TStorageEvent;
    function GetOnSave: TStorageEvent;
    function GetOnShowRichEditorToolView: TRichEditorToolViewEvent;
    function GetView: IRichEditorView;
    procedure SetOnAfterSave(AValue: TStorageEvent);
    procedure SetOnBeforeSave(AValue: TStorageEvent);
    procedure SetOnHideRichEditorToolView(AValue: TRichEditorToolViewEvent);
    procedure SetOnLoad(AValue: TStorageEvent);
    procedure SetOnNew(AValue: TNewEvent);
    procedure SetOnOpen(AValue: TStorageEvent);
    procedure SetOnSave(AValue: TStorageEvent);
    procedure SetOnShowRichEditorToolView(AValue: TRichEditorToolViewEvent);
    {$ENDREGION}

  protected
    // event dispatch methods
    procedure DoChange;
    procedure DoModified;
    procedure DoSelectBlock;
    procedure DoOpen(const AName: string);
    procedure DoBeforeSave(const AName: string);
    procedure DoAfterSave(const AName: string);
    procedure DoLoad(const AName: string);
    procedure DoNew(
      const AName : string = '';
      const AText : string = ''
    );
    procedure DoShowToolView(AToolView: IRichEditorToolView); virtual;
    procedure DoHideToolView(AToolView: IRichEditorToolView); virtual;

    procedure AddOnChangeHandler(AEvent: TNotifyEvent);
    procedure AddOnModifiedHandler(AEvent: TNotifyEvent);
    procedure RemoveOnChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveOnModifiedHandler(AEvent: TNotifyEvent);
    procedure AddOnSelectBlockHandler(AEvent: TNotifyEvent);
    procedure RemoveOnSelectBlockHandler(AEvent: TNotifyEvent);

    property View: IRichEditorView
      read GetView;

    property OnLoad: TStorageEvent
      read GetOnLoad write SetOnLoad;

    property OnSave: TStorageEvent
      read GetOnSave write SetOnSave;

    property OnNew: TNewEvent
      read GetOnNew write SetOnNew;

    property OnOpen: TStorageEvent
      read GetOnOpen write SetOnOpen;

    property OnBeforeSave: TStorageEvent
      read GetOnBeforeSave write SetOnBeforeSave;

    property OnAfterSave: TStorageEvent
      read GetOnAfterSave write SetOnAfterSave;

    property OnShowRichEditorToolView: TRichEditorToolViewEvent
      read GetOnShowRichEditorToolView write SetOnShowRichEditorToolView;

    property OnHideRichEditorToolView : TRichEditorToolViewEvent
      read GetOnHideRichEditorToolView write SetOnHideRichEditorToolView;

  public
    constructor Create(AManager: IRichEditorManager);
    procedure AfterConstruction; override;
    destructor Destroy; override;

  end;

implementation

{$REGION 'construction and destruction'}
constructor TRichEditorEvents.Create(AManager: IRichEditorManager);
begin
  inherited Create;
  FManager := AManager;
end;

procedure TRichEditorEvents.AfterConstruction;
begin
  inherited AfterConstruction;
  FChangeEvents      := TMethodList.Create;
  FModifiedEvents    := TMethodList.Create;
  FSelectBlockEvents := TMethodList.Create;
end;

destructor TRichEditorEvents.Destroy;
begin
  FManager := nil;
  FChangeEvents.Free;
  FModifiedEvents.Free;
  FSelectBlockEvents.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TRichEditorEvents.GetOnAfterSave: TStorageEvent;
begin
  Result := FOnAfterSave;
end;

function TRichEditorEvents.GetOnBeforeSave: TStorageEvent;
begin
  Result := FOnBeforeSave;
end;

function TRichEditorEvents.
  GetOnHideRichEditorToolView: TRichEditorToolViewEvent;
begin
  Result := FOnHideRichEditorToolView;
end;

function TRichEditorEvents.GetOnLoad: TStorageEvent;
begin
  Result := FOnLoad;
end;

function TRichEditorEvents.GetOnNew: TNewEvent;
begin
  Result := FOnNew;
end;

function TRichEditorEvents.GetOnOpen: TStorageEvent;
begin
  Result := FOnOpen;
end;

function TRichEditorEvents.GetOnSave: TStorageEvent;
begin
  Result := FOnSave;
end;

function TRichEditorEvents.
  GetOnShowRichEditorToolView: TRichEditorToolViewEvent;
begin
   Result := FOnShowRichEditorToolView;
end;

procedure TRichEditorEvents.SetOnSave(AValue: TStorageEvent);
begin
  FOnSave := AValue;
end;

procedure TRichEditorEvents.SetOnShowRichEditorToolView
  (AValue: TRichEditorToolViewEvent);
begin
  FOnShowRichEditorToolView := AValue;
end;

function TRichEditorEvents.GetView: IRichEditorView;
begin
  Result := FManager as IRichEditorView;
end;

procedure TRichEditorEvents.SetOnAfterSave(AValue: TStorageEvent);
begin
  FOnAfterSave := AValue;
end;

procedure TRichEditorEvents.SetOnBeforeSave(AValue: TStorageEvent);
begin
  FOnBeforeSave := AValue;
end;

procedure TRichEditorEvents.SetOnHideRichEditorToolView
  (AValue: TRichEditorToolViewEvent);
begin
  FOnHideRichEditorToolView := AValue;
end;

procedure TRichEditorEvents.SetOnLoad(AValue: TStorageEvent);
begin
  FOnLoad := AValue;
end;

procedure TRichEditorEvents.SetOnNew(AValue: TNewEvent);
begin
  FOnNew := AValue;
end;

procedure TRichEditorEvents.SetOnOpen(AValue: TStorageEvent);
begin
  FOnOpen := AValue;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TRichEditorEvents.DoChange;
begin
  FChangeEvents.CallNotifyEvents(Self);
end;

procedure TRichEditorEvents.DoModified;
begin
  FModifiedEvents.CallNotifyEvents(Self);
end;

procedure TRichEditorEvents.DoSelectBlock;
begin
  FSelectBlockEvents.CallNotifyEvents(Self);
end;

procedure TRichEditorEvents.DoOpen(const AName: string);
var
  S : string;
begin
  S  := AName;
  if Assigned(FOnOpen) then
    FOnOpen(Self, S);
end;

procedure TRichEditorEvents.DoBeforeSave(const AName: string);
var
  S : string;
begin
  if Assigned(FOnBeforeSave) then
  begin
    S := View.FileName;
    FOnBeforeSave(Self, S);
  end;
end;

procedure TRichEditorEvents.DoAfterSave(const AName: string);
var
  S : string;
begin
  S := AName;
  if Assigned(FOnAfterSave) then
  begin
    FOnAfterSave(Self, S);
  end;
end;

procedure TRichEditorEvents.DoLoad(const AName: string);
var
  S : string;
begin
  S  := AName;
  if Assigned(FOnLoad) then
    FOnLoad(Self, S);
end;

procedure TRichEditorEvents.DoNew(const AName: string; const AText: string);
var
  S : string;
begin
  S := AName;
  if Assigned(FOnNew) then
    FOnNew(Self, S, AText);
end;

procedure TRichEditorEvents.DoShowToolView(AToolView: IRichEditorToolView);
begin
  if Assigned(FOnShowRichEditorToolView) then
    FOnShowRichEditorToolView(Self, AToolView);
end;

procedure TRichEditorEvents.DoHideToolView(AToolView: IRichEditorToolView);
begin
  if Assigned(FOnHideRichEditorToolView) then
    FOnHideRichEditorToolView(Self, AToolView);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TRichEditorEvents.AddOnChangeHandler(AEvent: TNotifyEvent);
begin
  FChangeEvents.Add(TMethod(AEvent));
end;

procedure TRichEditorEvents.AddOnModifiedHandler(AEvent: TNotifyEvent);
begin
  FModifiedEvents.Add(TMethod(AEvent));
end;

procedure TRichEditorEvents.RemoveOnChangeHandler(AEvent: TNotifyEvent);
begin
  FChangeEvents.Remove(TMethod(AEvent));
end;

procedure TRichEditorEvents.RemoveOnModifiedHandler(AEvent: TNotifyEvent);
begin
   FModifiedEvents.Remove(TMethod(AEvent));
end;

procedure TRichEditorEvents.AddOnSelectBlockHandler(AEvent: TNotifyEvent);
begin
  FSelectBlockEvents.Add(TMethod(AEvent));
end;

procedure TRichEditorEvents.RemoveOnSelectBlockHandler(AEvent: TNotifyEvent);
begin
  FSelectBlockEvents.Remove(TMethod(AEvent));
end;
{$ENDREGION}

end.

