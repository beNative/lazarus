program SnippetSource;

{$MODE DELPHI}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms, pascalscript, lazcontrols, richmemopackage,
  virtualtreeview_package, appexplore, AppExploreFrm, virtualdbtreeex_laz,
  luicontrols,

  { you can add units after this }

  SnippetSource.Forms.Main;

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
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.



