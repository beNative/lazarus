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
  SnippetSource.Forms.Main, SnippetSource.Forms.Console,
  SnippetSource.Modules.Data, SnippetSource.Forms.Grid, SnippetSource.Settings,
  SnippetSource.Forms.Busy;

{$R *.res}

begin
{$IF DECLARED(UseHeapTrace)}
  //if FileExists('trace.trc') then
  //  DeleteFile('trace.trc');
  //GlobalSkipIfNoLeaks := True; // supported as of debugger version 3.1.1
  //SetHeapTraceOutput('trace.trc');
{$ENDIF}
  Application.Scaled := True;
  Application.Title := 'SnippetSource';
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
