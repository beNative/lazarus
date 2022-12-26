{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

program SnippetSource;

{$MODE DELPHI}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms,

  pascalscript, lazcontrols, luicontrols,
  virtualtreeview_package, virtualdbtreeexlaz,
  cmdbox,

  { you can add units after this }
  SnippetSource.Forms.Main, SnippetSource.Forms.Console,
  SnippetSource.Modules.Data, SnippetSource.Modules.Python, SnippetSource.Forms.Grid, SnippetSource.Settings,
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
