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

program SnippetSource;

{$MODE DELPHI}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms,

  lazcontrols, luicontrols,
  virtualtreeview_package, virtualdbtreeexlaz,
  cmdbox, tslib_editor, tslib_richeditor, KControlsLaz,
  SQLDBLaz, RunTimeTypeInfoControls, SynEdit, LazUtils,
  python4lazarus_package,

  { you can add units after this }
  SnippetSource.Forms.Main, SnippetSource.Forms.Console,
  SnippetSource.Modules.Data, SnippetSource.Modules.Python,
  SnippetSource.Forms.Grid, SnippetSource.Settings,
  SnippetSource.Forms.Busy, SnippetSource.Modules.Terminal;

{$R *.res}

begin
{$IF DECLARED(UseHeapTrace)}
  if FileExists('trace.trc') then
    DeleteFile('trace.trc');
  GlobalSkipIfNoLeaks := True; // supported as of debugger version 3.1.1
  SetHeapTraceOutput('trace.trc');
{$ENDIF}
  Application.Title := 'SnippetSource';
  Application.Initialize;
  Application.CreateForm(TdmPython, dmPython);
  Application.CreateForm(TdmTerminal, dmTerminal);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
