program SnippetSource;

{$MODE DELPHI}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms, pascalscript, richmemopackage,
  virtualtreeview_package, virtualdbtreeex_laz,
  luicontrols,

  { you can add units after this }

  SnippetSource.Forms.Main;

{$R *.res}

begin
  if FileExists('trace.trc') then
    DeleteFile('trace.trc');
  //SetHeapTraceOutput('trace.trc');
  Application.Title := 'SnippetSource';
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.



