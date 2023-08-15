{
  Copyright (C) 2013-2023 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

program fpcunitnotepas;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms,
  GuiTestRunner,

  ts.Core.Logger, ts.Core.Logger.Channel.IPC, ts.Editor.Colors.Settings,
  ts.Editor.Options.Settings, ts.Editor.Settings,

  Test.Editor.Settings,
  Test.ComponentStreaming, Test.ComponentStreaming.TestComponents;

{$R *.res}

begin
  {$IFDEF Windows}
    Logger.Channels.Add(TIPCChannel.Create);
  {$ENDIF}
  Logger.Clear;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.


