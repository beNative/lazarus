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

unit ts.RichEditor.ToolView.Base;

{ Base toolview form that can be used to create descendants that implement
  IRichEditorToolView.
  It reacts to changes in the common settings (IRichEditorSettings) which are
  associated with the owning manager (IRichEditorManager) instance.

  This base class provides properties which are shortcuts to the following
  instances that are used by the editor manager:
    - Manager  : IRichEditorManager   - The owning editor manager instance
    - Settings : IRichEditorSettings  - All persistable settings
    - View     : IRichEditorView      - The currently active editor view
    - Views    : IRichEditorViews     - The list of available editor views

  It provides virtual event handlers which act as callback methods to let us
  respond to certain changes in the active editor view or to changes in the
  settings. It acts as an observer to react on any relevant changes in the
  observed instances.
}

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,

  ts.RichEditor.Interfaces;

type
  TCustomRichEditorToolView = class(TForm, IRichEditorToolView)
  private
    FUpdate : Boolean;

  protected
    {$REGION 'property access methods'}
    function GetUpdate: Boolean;
    procedure SetUpdate(AValue: Boolean);
    function GetManager: IRichEditorManager;
    function GetView: IRichEditorView;
    function GetForm: TForm;
    function GetName: string;
    function GetVisible: Boolean;
    {$ENDREGION}

    procedure UpdateView; virtual;
    procedure Modified; virtual;

    property Manager: IRichEditorManager
      read GetManager;

    property View: IRichEditorView
      read GetView;

    property Update: Boolean
      read GetUpdate write SetUpdate;

  end;

implementation

{$R *.lfm}

{$REGION 'property access methods'}
function TCustomRichEditorToolView.GetUpdate: Boolean;
begin
  Result := FUpdate;
end;

procedure TCustomRichEditorToolView.SetUpdate(AValue: Boolean);
begin
  if AValue <> Update then
  begin
    FUpdate := AValue;
  end;
end;

function TCustomRichEditorToolView.GetManager: IRichEditorManager;
begin
  Result := Owner as IRichEditorManager;
end;

function TCustomRichEditorToolView.GetView: IRichEditorView;
begin
  Result := Manager.ActiveView;
end;

function TCustomRichEditorToolView.GetForm: TForm;
begin
  Result := Self;
end;

function TCustomRichEditorToolView.GetName: string;
begin
  Result := inherited Name;
end;

function TCustomRichEditorToolView.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TCustomRichEditorToolView.UpdateView;
begin
  //
end;

procedure TCustomRichEditorToolView.Modified;
begin
  FUpdate := True;
end;
{$ENDREGION}


end.

