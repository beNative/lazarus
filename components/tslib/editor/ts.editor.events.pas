unit ts.Editor.Events;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, LazMethodList,

  SynEditTypes, SynMacroRecorder,

  ts.Editor.Interfaces;

//TCaretPositionEvent = procedure(
//  Sender : TObject;
//  X, Y   : Integer
//) of object;

type

  { TCaretPositionEvents }

  TCaretPositionEvents = class(TMethodList)
    procedure CallEvents(Sender: TObject; X, Y: Integer);
  end;


type

  { TEditorEvents }

  TEditorEvents = class(TInterfacedObject, IEditorEvents)
  strict private
    FManager                : IEditorManager;
    FChangeEvents           : TMethodList;
    FModifiedEvents         : TMethodList;
    FActiveViewChangeEvents : TMethodList;
    FCaretPositionEvents    : TCaretPositionEvents;

    FOnAddEditorView       : TAddEditorViewEvent;
    FOnShowEditorToolView  : TEditorToolViewEvent;
    FOnHideEditorToolView  : TEditorToolViewEvent;
    FOnCaretPositionChange : TCaretPositionEvent;
    FOnMacroStateChange    : TMacroStateChangeEvent;
    FOnNewFile             : TNewFileEvent;
    FOnOpenFile            : TFileEvent;
    FOnOpenOtherInstance   : TOpenOtherInstanceEvent;
    FOnSaveFile            : TFileEvent;
    FOnStatusChange        : TStatusChangeEvent;
    FOnStatusMessage       : TStatusMessageEvent;

  strict protected
    function GetView: IEditorView;
    function GetOnAddEditorView: TAddEditorViewEvent;
    function GetOnHideEditorToolView: TEditorToolViewEvent;
    function GetOnMacroStateChange: TMacroStateChangeEvent;
    function GetOnNewFile: TNewFileEvent;
    function GetOnOpenFile: TFileEvent;
    function GetOnOpenOtherInstance: TOpenOtherInstanceEvent;
    function GetOnSaveFile: TFileEvent;
    function GetOnShowEditorToolView: TEditorToolViewEvent;
    function GetOnStatusChange: TStatusChangeEvent;
    procedure SetOnAddEditorView(AValue: TAddEditorViewEvent);
    procedure SetOnHideEditorToolView(AValue: TEditorToolViewEvent);
    procedure SetOnMacroStateChange(const AValue: TMacroStateChangeEvent);
    procedure SetOnNewFile(const AValue: TNewFileEvent);
    procedure SetOnOpenFile(const AValue: TFileEvent);
    procedure SetOnOpenOtherInstance(AValue: TOpenOtherInstanceEvent);
    procedure SetOnSaveFile(const AValue: TFileEvent);
    procedure SetOnShowEditorToolView(AValue: TEditorToolViewEvent);
    procedure SetOnStatusChange(const AValue: TStatusChangeEvent);

    { will get called by owner to trigger the events }
    procedure DoChange; virtual;
    procedure DoModified; virtual;
    procedure DoActiveViewChange; virtual;
    procedure DoAddEditorView(AEditorView: IEditorView); virtual;
    procedure DoShowToolView(AToolView: IEditorToolView); virtual;
    procedure DoHideToolView(AToolView: IEditorToolView); virtual;
    procedure DoCaretPositionChange; virtual;
    procedure DoMacroStateChange(AState : TSynMacroState); virtual;
    procedure DoOpenOtherInstance(const AParams: array of string); virtual;
    procedure DoStatusMessage(AText: string); virtual;
    procedure DoStatusChange(AChanges: TSynStatusChanges); virtual;
    procedure DoSaveFile(const AFileName: string);
    procedure DoOpenFile(const AFileName: string);
    procedure DoNewFile(
      const AFileName : string = '';
      const AText     : string = ''
    );

    procedure AddOnChangeHandler(AEvent: TNotifyEvent);
    procedure AddOnModifiedHandler(AEvent: TNotifyEvent);
    procedure AddOnActiveViewChangeHandler(AEvent: TNotifyEvent);
    procedure AddOnCaretPositionEvent(AEvent: TCaretPositionEvent);

    procedure RemoveOnChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveOnModifiedHandler(AEvent: TNotifyEvent);
    procedure RemoveOnActiveViewChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveOnCaretPositionEvent(AEvent: TCaretPositionEvent);

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

    property OnOpenFile: TFileEvent
      read GetOnOpenFile write SetOnOpenFile;

    property OnNewFile: TNewFileEvent
      read GetOnNewFile write SetOnNewFile;

    property OnSaveFile: TFileEvent
      read GetOnSaveFile write SetOnSaveFile;

    property OnOpenOtherInstance: TOpenOtherInstanceEvent
      read GetOnOpenOtherInstance write SetOnOpenOtherInstance;

    property View: IEditorView
      read GetView;

  public
    constructor Create(AManager: IEditorManager);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{ TCaretPositionEvents }

procedure TCaretPositionEvents.CallEvents(Sender: TObject; X, Y: Integer);
var
  I: Integer;
begin
  I := Count;
  while NextDownIndex(I) do
    TCaretPositionEvent(Items[I])(Sender, X, Y);
end;

{ TEditorEvents }

function TEditorEvents.GetView: IEditorView;
begin
  Result := FManager as IEditorView;
end;

function TEditorEvents.GetOnAddEditorView: TAddEditorViewEvent;
begin
  Result := FOnAddEditorView;
end;

function TEditorEvents.GetOnHideEditorToolView: TEditorToolViewEvent;
begin
  Result := FOnHideEditorToolView;
end;

function TEditorEvents.GetOnMacroStateChange: TMacroStateChangeEvent;
begin
  Result := FOnMacroStateChange;
end;

function TEditorEvents.GetOnNewFile: TNewFileEvent;
begin
  Result := FOnNewFile;
end;

function TEditorEvents.GetOnOpenFile: TFileEvent;
begin
  Result := FOnOpenFile;
end;

function TEditorEvents.GetOnOpenOtherInstance: TOpenOtherInstanceEvent;
begin
  Result := FOnOpenOtherInstance;
end;

function TEditorEvents.GetOnSaveFile: TFileEvent;
begin
  Result := FOnSaveFile;
end;

function TEditorEvents.GetOnShowEditorToolView: TEditorToolViewEvent;
begin
  Result := FOnShowEditorToolView;
end;

function TEditorEvents.GetOnStatusChange: TStatusChangeEvent;
begin
  Result := FOnStatusChange;
end;

procedure TEditorEvents.SetOnAddEditorView(AValue: TAddEditorViewEvent);
begin
  FOnAddEditorView := AValue;
end;

procedure TEditorEvents.SetOnHideEditorToolView(AValue: TEditorToolViewEvent);
begin
  FOnHideEditorToolView := AValue;
end;

procedure TEditorEvents.SetOnMacroStateChange(
  const AValue: TMacroStateChangeEvent);
begin
  FOnMacroStateChange := AValue;
end;

procedure TEditorEvents.SetOnNewFile(const AValue: TNewFileEvent);
begin
  FOnNewFile := AValue;
end;

procedure TEditorEvents.SetOnOpenFile(const AValue: TFileEvent);
begin
  FOnOpenFile := AValue;
end;

procedure TEditorEvents.SetOnOpenOtherInstance(AValue: TOpenOtherInstanceEvent);
begin
  FOnOpenOtherInstance := AValue;
end;

procedure TEditorEvents.SetOnSaveFile(const AValue: TFileEvent);
begin
  FOnSaveFile := AValue;
end;

procedure TEditorEvents.SetOnShowEditorToolView(AValue: TEditorToolViewEvent);
begin
  FOnShowEditorToolView := AValue;
end;

procedure TEditorEvents.SetOnStatusChange(const AValue: TStatusChangeEvent);
begin
  FOnStatusChange := AValue;
end;

procedure TEditorEvents.DoChange;
begin
  FChangeEvents.CallNotifyEvents(Self);
end;

procedure TEditorEvents.DoModified;
begin
  FModifiedEvents.CallNotifyEvents(Self);
end;

procedure TEditorEvents.DoActiveViewChange;
begin
  FActiveViewChangeEvents.CallNotifyEvents(Self);
end;

procedure TEditorEvents.DoAddEditorView(AEditorView: IEditorView);
begin
  if Assigned(FOnAddEditorView) then
    FOnAddEditorView(Self, AEditorView);
end;

procedure TEditorEvents.DoShowToolView(AToolView: IEditorToolView);
begin
  if Assigned(FOnShowEditorToolView) then
    FOnShowEditorToolView(Self, AToolView);
end;

procedure TEditorEvents.DoHideToolView(AToolView: IEditorToolView);
begin
  if Assigned(FOnHideEditorToolView) then
    FOnHideEditorToolView(Self, AToolView);
end;

procedure TEditorEvents.DoCaretPositionChange;
begin
  FCaretPositionEvents.CallEvents(Self, View.CaretX, View.CaretY);
end;

procedure TEditorEvents.DoMacroStateChange(AState: TSynMacroState);
begin
  if Assigned(FOnMacroStateChange) then
    FOnMacroStateChange(Self, AState);
end;

procedure TEditorEvents.DoOpenOtherInstance(const AParams: array of string);
begin
  if Assigned(FOnOpenOtherInstance) then
    FOnOpenOtherInstance(Self, AParams);
end;

procedure TEditorEvents.DoStatusMessage(AText: string);
begin
  if Assigned(FOnStatusMessage) then
    FOnStatusMessage(Self, AText);
end;

procedure TEditorEvents.DoStatusChange(AChanges: TSynStatusChanges);
begin
  if Assigned(FOnStatusChange) then
    FOnStatusChange(Self, AChanges);
end;

procedure TEditorEvents.DoSaveFile(const AFileName: string);
var
  S: string;
begin
  if Assigned(FOnSaveFile) then
  begin
    S := View.FileName;
    FOnSaveFile(Self, S);
    View.FileName := S;
  end;
end;

procedure TEditorEvents.DoOpenFile(const AFileName: string);
var
  S : string;
begin
  S  := AFileName;
  if Assigned(FOnOpenFile) then
    FOnOpenFile(Self, S);
end;

procedure TEditorEvents.DoNewFile(const AFileName: string; const AText: string);
var
  S : string;
begin
  S  := AFileName;
  if Assigned(FOnNewFile) then
    FOnNewFile(Self, S, AText);
end;

procedure TEditorEvents.AddOnChangeHandler(AEvent: TNotifyEvent);
begin
  FChangeEvents.Add(TMethod(AEvent));
end;

procedure TEditorEvents.AddOnModifiedHandler(AEvent: TNotifyEvent);
begin
  FModifiedEvents.Add(TMethod(AEvent));
end;

procedure TEditorEvents.AddOnActiveViewChangeHandler(AEvent: TNotifyEvent);
begin
  FActiveViewChangeEvents.Add(TMethod(AEvent));
end;

procedure TEditorEvents.AddOnCaretPositionEvent(AEvent: TCaretPositionEvent);
begin
  FCaretPositionEvents.Add(TMethod(AEvent));
end;

procedure TEditorEvents.RemoveOnChangeHandler(AEvent: TNotifyEvent);
begin
  FChangeEvents.Remove(TMethod(AEvent));
end;

procedure TEditorEvents.RemoveOnModifiedHandler(AEvent: TNotifyEvent);
begin
  FModifiedEvents.Remove(TMethod(AEvent));
end;

procedure TEditorEvents.RemoveOnActiveViewChangeHandler(AEvent: TNotifyEvent);
begin
  FActiveViewChangeEvents.Remove(TMethod(AEvent));
end;

procedure TEditorEvents.RemoveOnCaretPositionEvent(AEvent: TCaretPositionEvent);
begin
  FCaretPositionEvents.Remove(TMethod(AEvent));
end;

constructor TEditorEvents.Create(AManager: IEditorManager);
begin
  inherited Create;
  FManager := AManager;
end;

procedure TEditorEvents.AfterConstruction;
begin
  inherited AfterConstruction;
  FChangeEvents           := TMethodList.Create;
  FModifiedEvents         := TMethodList.Create;
  FActiveViewChangeEvents := TMethodList.Create;
  FCaretPositionEvents    := TCaretPositionEvents.Create;
end;

procedure TEditorEvents.BeforeDestruction;
begin
  FManager := nil;
  FChangeEvents.Free;
  FModifiedEvents.Free;
  FActiveViewChangeEvents.Free;
  FCaretPositionEvents.Free;
  inherited BeforeDestruction;
end;

end.

