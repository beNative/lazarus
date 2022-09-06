{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

