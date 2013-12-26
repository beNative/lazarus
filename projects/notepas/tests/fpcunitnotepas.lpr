program fpcunitnotepas;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, Test.Editor.Settings,
  ts.Core.SharedLogger, ts.Core.Logger.Channel.IPC;

{$R *.res}

begin
  {$IFDEF Windows}
    Logger.Channels.Add(TIPCChannel.Create);
  {$ENDIF}


  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.


