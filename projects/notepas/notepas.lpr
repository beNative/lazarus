program Notepas;

{$mode objfpc}{$H+}

uses
  SysUtils,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, notepas_forms_main,
  { you can add units after this }
  DefaultTranslator,

  sharedlogger,
  {$ifdef windows}
  ipcchannel,
  {$endif}
  richmemopackage;

{$R *.res}

begin
  //{$ifdef windows}
  //  Logger.Channels.Add(TIPCChannel.Create);
  //{$endif}

  Application.Title := 'Notepas';
  //if FileExists('Notepas.trc') then
  //  DeleteFile('Notepas.trc');
  //SetHeapTraceOutput('Notepas.trc');
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

