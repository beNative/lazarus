program SnippetSource;

{$MODE DELPHI}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms, pascalscript, lazcontrols, luicontrols,
  virtualtreeview_package, cmdbox,
  virtualdbtreeexlaz,

  { you can add units after this }
  ts.Core.Logger.Channel.IPC, ts.Core.Logger.Interfaces, ts.Core.Logger,

  SnippetSource.Forms.Main, SnippetSource.Forms.Console,
  SnippetSource.Modules.Data, SnippetSource.Forms.Grid, SnippetSource.Settings;

{$R *.res}

begin
  Logger.Channels.Add(TIpcChannel.Create);
  Logger.Clear;
{$IF DECLARED(UseHeapTrace)}
  if FileExists('trace.trc') then
    DeleteFile('trace.trc');
  GlobalSkipIfNoLeaks := True; // supported as of debugger version 3.1.1
  SetHeapTraceOutput('trace.trc');
{$ENDIF}
  Application.Scaled := True;
  Application.Title := 'SnippetSource';
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
