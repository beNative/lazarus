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

program Notepas;

{$MODE DELPHI}

uses
  SysUtils,
{$IFDEF UNIX} // Required for Linux and BSD
  cthreads,
{$ENDIF}
  DefaultTranslator,
  Interfaces, // this includes the LCL widgetset
  Forms,

  RunTimeTypeInfoControls,
  pascalscript,
  kcontrols,
  ExceptionLogger,

  ts.Core.Logger.Channel.IPC, ts.Core.Logger.Interfaces, ts.Core.Logger,
  tslib_components, tslib_core, tslib_editor,
  ts.Editor.AboutDialog, ts.Editor.CodeFormatters, ts.Editor.CodeFormatters.SQL,
  ts.Editor.CodeTags, ts.Editor.Commands, ts.Editor.CommentStripper,
  ts.Editor.Events, ts.Editor.Factories.Manager, ts.Editor.Factories.Menus,
  ts.Editor.Factories, ts.Editor.Factories.Settings,
  ts.Editor.Factories.Toolbars, ts.Editor.Factories.Views,
  ts.Editor.HighlighterAttributes, ts.Editor.Highlighters, ts.Editor.Interfaces,
  ts.Editor.Manager, ts.Editor.Resources, ts.Editor.Selection,
  ts.Editor.ToolViews, ts.Editor.Types, ts.Editor.Utils, ts.Editor.View,
  ts.Editor.Colors.Settings, ts.Editor.Options.Settings, ts.Editor.Settings,

  Notepas.Main.Form;

{$R *.res}

begin
  Logger.Channels.Add(TIpcChannel.Create);
  Logger.Clear;
{$IF DECLARED(UseHeapTrace)}
   GlobalSkipIfNoLeaks := True; // supported as of debugger version 3.1.1
  if FileExists('Notepas.trc') then
    DeleteFile('Notepas.trc');
  SetHeapTraceOutput('Notepas.trc');
{$ENDIF}
  Application.Scaled := True;
  Application.Title := 'Notepas';

{$IFDEF WINDOWS}
{$WARNINGS OFF}
  Application.MainFormOnTaskbar := True;
{$WARNINGS ON}
{$ENDIF}
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

