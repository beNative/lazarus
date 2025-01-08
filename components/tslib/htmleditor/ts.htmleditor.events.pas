{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.HtmlEditor.Events;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils,

  LazMethodList,

  ts.Core.Types, ts.HtmlEditor.Interfaces;

type
  THtmlEditorEvents = class(TInterfacedObject, IHtmlEditorEvents)
  private
    FManager                     : IHtmlEditorManager;
    FChangeEvents                : TMethodList;
    FModifiedEvents              : TMethodList;
    FOnContentLoadedEvents       : TMethodList;
    FOnSourceChangedEvents       : TMethodList;
    FOnNavigationCompletedEvents : TMethodList;

    FOnNew                 : TNewEvent;
    FOnLoad                : TStorageEvent;
    FOnOpen                : TStorageEvent;
    FOnBeforeSave          : TStorageEvent;
    FOnAfterSave           : TStorageEvent;
    FOnSave                : TStorageEvent;
    FOnShowToolView        : THtmlEditorToolViewEvent;
    FOnHideToolView        : THtmlEditorToolViewEvent;

    {$REGION 'property access mehods'}
    function GetOnAfterSave: TStorageEvent;
    function GetOnBeforeSave: TStorageEvent;
    function GetOnHideToolView: THtmlEditorToolViewEvent;
    function GetOnLoad: TStorageEvent;
    function GetOnNew: TNewEvent;
    function GetOnOpen: TStorageEvent;
    function GetOnSave: TStorageEvent;
    function GetOnShowToolView: THtmlEditorToolViewEvent;
    function GetView: IHtmlEditorView;
    procedure SetOnAfterSave(AValue: TStorageEvent);
    procedure SetOnBeforeSave(AValue: TStorageEvent);
    procedure SetOnHideToolView(AValue: THtmlEditorToolViewEvent);
    procedure SetOnLoad(AValue: TStorageEvent);
    procedure SetOnNew(AValue: TNewEvent);
    procedure SetOnOpen(AValue: TStorageEvent);
    procedure SetOnSave(AValue: TStorageEvent);
    procedure SetOnShowToolView(AValue: THtmlEditorToolViewEvent);
    {$ENDREGION}

  protected
    // event dispatch methods
    procedure DoChange;
    procedure DoModified;
    procedure DoContentLoaded;
    procedure DoSourceChanged;
    procedure DoNavigationCompleted;
    procedure DoOpen(const AName: string);
    procedure DoBeforeSave(const AName: string);
    procedure DoAfterSave(const AName: string);
    procedure DoLoad(const AName: string);
    procedure DoNew(
      const AName : string = '';
      const AText : string = ''
    );
    procedure DoShowToolView(AToolView: IHtmlEditorToolView); virtual;
    procedure DoHideToolView(AToolView: IHtmlEditorToolView); virtual;

    procedure AddOnChangeHandler(AEvent: TNotifyEvent);
    procedure AddOnModifiedHandler(AEvent: TNotifyEvent);
    procedure RemoveOnChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveOnModifiedHandler(AEvent: TNotifyEvent);
    procedure AddOnContentLoadedHandler(AEvent: TNotifyEvent);
    procedure RemoveOnContentLoadedHandler(AEvent: TNotifyEvent);
    procedure AddOnSourceChangedHandler(AEvent: TNotifyEvent);
    procedure RemoveOnSourceChangedHandler(AEvent: TNotifyEvent);
    procedure AddOnNavigationCompletedHandler(AEvent: TNotifyEvent);
    procedure RemoveOnNavigationCompletedHandler(AEvent: TNotifyEvent);

    property View: IHtmlEditorView
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

    property OnShowToolView: THtmlEditorToolViewEvent
      read GetOnShowToolView write SetOnShowToolView;

    property OnHideToolView : THtmlEditorToolViewEvent
      read GetOnHideToolView write SetOnHideToolView;

  public
    constructor Create(AManager: IHtmlEditorManager);
    procedure AfterConstruction; override;
    destructor Destroy; override;

  end;

implementation

{$REGION 'construction and destruction'}
procedure THtmlEditorEvents.AfterConstruction;
begin
  inherited AfterConstruction;
  FChangeEvents                := TMethodList.Create;
  FModifiedEvents              := TMethodList.Create;
  FOnContentLoadedEvents       := TMethodList.Create;
  FOnSourceChangedEvents       := TMethodList.Create;
  FOnNavigationCompletedEvents := TMethodList.Create;;
end;

constructor THtmlEditorEvents.Create(AManager: IHtmlEditorManager);
begin
  inherited Create;
  FManager := AManager;
end;

destructor THtmlEditorEvents.Destroy;
begin
  FManager := nil;
  FChangeEvents.Free;
  FModifiedEvents.Free;
  FOnContentLoadedEvents.Free;
  FOnSourceChangedEvents.Free;
  FOnNavigationCompletedEvents.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function THtmlEditorEvents.GetOnAfterSave: TStorageEvent;
begin
  Result := FOnAfterSave;
end;

function THtmlEditorEvents.GetOnBeforeSave: TStorageEvent;
begin
  Result := FOnBeforeSave;
end;

function THtmlEditorEvents.GetOnHideToolView: THtmlEditorToolViewEvent;
begin
  Result := FOnHideToolView;
end;

function THtmlEditorEvents.GetOnLoad: TStorageEvent;
begin
  Result := FOnLoad;
end;

function THtmlEditorEvents.GetOnNew: TNewEvent;
begin
  Result := FOnNew;
end;

function THtmlEditorEvents.GetOnOpen: TStorageEvent;
begin
  Result := FOnOpen;
end;

function THtmlEditorEvents.GetOnSave: TStorageEvent;
begin
  Result := FOnAfterSave;
end;

function THtmlEditorEvents.GetOnShowToolView: THtmlEditorToolViewEvent;
begin
  Result := FOnShowToolView;
end;

function THtmlEditorEvents.GetView: IHtmlEditorView;
begin
  Result := FManager as IHtmlEditorView;
end;

procedure THtmlEditorEvents.SetOnAfterSave(AValue: TStorageEvent);
begin
  FOnAfterSave := AValue;
end;

procedure THtmlEditorEvents.SetOnBeforeSave(AValue: TStorageEvent);
begin
  FOnBeforeSave := AValue;
end;

procedure THtmlEditorEvents.SetOnHideToolView(AValue: THtmlEditorToolViewEvent);
begin
  FOnHideToolView := AValue;
end;

procedure THtmlEditorEvents.SetOnLoad(AValue: TStorageEvent);
begin
  FOnLoad := AValue;
end;

procedure THtmlEditorEvents.SetOnNew(AValue: TNewEvent);
begin
  FOnNew := AValue;
end;

procedure THtmlEditorEvents.SetOnOpen(AValue: TStorageEvent);
begin
  FOnOpen := AValue;
end;

procedure THtmlEditorEvents.SetOnSave(AValue: TStorageEvent);
begin
  FOnSave := AValue;
end;

procedure THtmlEditorEvents.SetOnShowToolView(AValue: THtmlEditorToolViewEvent);
begin
  FOnShowToolView := AValue;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure THtmlEditorEvents.DoChange;
begin
  FChangeEvents.CallNotifyEvents(Self);
end;

procedure THtmlEditorEvents.DoModified;
begin
  FModifiedEvents.CallNotifyEvents(Self);
end;

procedure THtmlEditorEvents.DoContentLoaded;
begin
  FOnContentLoadedEvents.CallNotifyEvents(Self);
end;

procedure THtmlEditorEvents.DoSourceChanged;
begin
  FOnSourceChangedEvents.CallNotifyEvents(Self);
end;

procedure THtmlEditorEvents.DoNavigationCompleted;
begin
  FOnNavigationCompletedEvents.CallNotifyEvents(Self);
end;

procedure THtmlEditorEvents.DoOpen(const AName: string);
var
  S : string;
begin
  S  := AName;
  if Assigned(FOnOpen) then
    FOnOpen(Self, S);
end;

procedure THtmlEditorEvents.DoBeforeSave(const AName: string);
var
  S : string;
begin
  if Assigned(FOnBeforeSave) then
  begin
    S := View.FileName;
    FOnBeforeSave(Self, S);
  end;
end;

procedure THtmlEditorEvents.DoAfterSave(const AName: string);
var
  S : string;
begin
  S := AName;
  if Assigned(FOnAfterSave) then
  begin
    FOnAfterSave(Self, S);
  end;
end;

procedure THtmlEditorEvents.DoLoad(const AName: string);
var
  S : string;
begin
  S  := AName;
  if Assigned(FOnLoad) then
    FOnLoad(Self, S);
end;

procedure THtmlEditorEvents.DoNew(const AName: string; const AText: string);
var
  S : string;
begin
  S := AName;
  if Assigned(FOnNew) then
    FOnNew(Self, S, AText);
end;

procedure THtmlEditorEvents.DoShowToolView(AToolView: IHtmlEditorToolView);
begin
  if Assigned(FOnShowToolView) then
    FOnShowToolView(Self, AToolView);
end;

procedure THtmlEditorEvents.DoHideToolView(AToolView: IHtmlEditorToolView);
begin
  if Assigned(FOnHideToolView) then
    FOnHideToolView(Self, AToolView);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure THtmlEditorEvents.AddOnChangeHandler(AEvent: TNotifyEvent);
begin
  FChangeEvents.Add(TMethod(AEvent));
end;

procedure THtmlEditorEvents.AddOnModifiedHandler(AEvent: TNotifyEvent);
begin
  FModifiedEvents.Add(TMethod(AEvent));
end;

procedure THtmlEditorEvents.RemoveOnChangeHandler(AEvent: TNotifyEvent);
begin
  FChangeEvents.Remove(TMethod(AEvent));
end;

procedure THtmlEditorEvents.RemoveOnModifiedHandler(AEvent: TNotifyEvent);
begin
  FModifiedEvents.Remove(TMethod(AEvent));
end;

procedure THtmlEditorEvents.AddOnContentLoadedHandler(AEvent: TNotifyEvent);
begin
  FOnContentLoadedEvents.Add(TMethod(AEvent));
end;

procedure THtmlEditorEvents.RemoveOnContentLoadedHandler(AEvent: TNotifyEvent);
begin
  FOnContentLoadedEvents.Remove(TMethod(AEvent));
end;

procedure THtmlEditorEvents.AddOnSourceChangedHandler(AEvent: TNotifyEvent);
begin
  FOnSourceChangedEvents.Add(TMethod(AEvent));
end;

procedure THtmlEditorEvents.RemoveOnSourceChangedHandler(AEvent: TNotifyEvent);
begin
  FOnSourceChangedEvents.Remove(TMethod(AEvent));
end;

procedure THtmlEditorEvents.AddOnNavigationCompletedHandler(AEvent: TNotifyEvent
  );
begin
  FOnNavigationCompletedEvents.Add(TMethod(AEvent));
end;

procedure THtmlEditorEvents.RemoveOnNavigationCompletedHandler
  (AEvent: TNotifyEvent);
begin
  FOnNavigationCompletedEvents.Remove(TMethod(AEvent));
end;

{$ENDREGION}

end.

