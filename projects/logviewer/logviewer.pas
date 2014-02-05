program logviewer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  luicontrols,
  atbinhex_lcl,
  Interfaces, // this includes the LCL widgetset
  Forms,
  { add your units here }
  logviewer_forms_main,
  ts.Core.SharedLogger,
  runtimetypeinfocontrols;

{$R *.res}

begin
  Application.Title := 'Log viewer';
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

