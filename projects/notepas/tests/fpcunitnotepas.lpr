program fpcunitnotepas;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner,

  ts.Core.Logger,  ts.Core.Logger.Channel.IPC,
  Test.Editor.Settings,
  Test.ComponentStreaming, Test.ComponentStreaming.TestComponents;

{$R *.res}

begin
  {$IFDEF Windows}
    Logger.Channels.Add(TIPCChannel.Create);
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.


