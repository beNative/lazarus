{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.RichEditor.Events;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils,

  ts.RichEditor.Types, ts.RichEditor.Interfaces;

type

  { TRichEditorEvents }

  TRichEditorEvents = class(TInterfacedObject, IRichEditorEvents)
  private
    FManager      : IRichEditorManager;
    FOnNew        : TNewEvent;
    FOnLoad       : TStorageEvent;
    FOnOpen       : TStorageEvent;
    FOnBeforeSave : TStorageEvent;
    FOnAfterSave  : TStorageEvent;
    FOnSave       : TStorageEvent;
    FOnChange     : TNotifyEvent;

    {$REGION 'property access mehods'}
    function GetOnAfterSave: TStorageEvent;
    function GetOnBeforeSave: TStorageEvent;
    function GetOnChange: TNotifyEvent;
    function GetOnLoad: TStorageEvent;
    function GetOnNew: TNewEvent;
    function GetOnOpen: TStorageEvent;
    function GetOnSave: TStorageEvent;
    function GetView: IRichEditorView;
    procedure SetOnAfterSave(AValue: TStorageEvent);
    procedure SetOnBeforeSave(AValue: TStorageEvent);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetOnLoad(AValue: TStorageEvent);
    procedure SetOnNew(AValue: TNewEvent);
    procedure SetOnOpen(AValue: TStorageEvent);
    procedure SetOnSave(AValue: TStorageEvent);
    {$ENDREGION}

  protected
    // event dispatch methods
    procedure DoChange;
    procedure DoOpen(const AName: string);
    procedure DoBeforeSave(const AName: string);
    procedure DoAfterSave(const AName: string);
    procedure DoLoad(const AName: string);
    procedure DoNew(
      const AName : string = '';
      const AText : string = ''
    );

    property View: IRichEditorView
      read GetView;

  public
    constructor Create(AManager: IRichEditorManager);
    procedure BeforeDestruction; override;

    { triggered when caret position changes }
    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;

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

  end;

implementation

{$REGION 'construction and destruction'}
constructor TRichEditorEvents.Create(AManager: IRichEditorManager);
begin
  inherited Create;
  FManager := AManager;
end;

procedure TRichEditorEvents.BeforeDestruction;
begin
  FManager := nil;
  inherited BeforeDestruction;
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

function TRichEditorEvents.GetOnChange: TNotifyEvent;
begin
  Result := FOnChange;
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

procedure TRichEditorEvents.SetOnSave(AValue: TStorageEvent);
begin
  FOnSave := AValue;
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

procedure TRichEditorEvents.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
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
  if Assigned(FOnChange) then
    FOnChange(Self);
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
{$ENDREGION}

end.

