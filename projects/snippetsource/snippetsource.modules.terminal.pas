{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit SnippetSource.Modules.Terminal;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils,

  process,

  SnippetSource.Interfaces;

type
  TdmTerminal = class(TDataModule, ITerminal)
    prcTerminal : TProcess;

  private
    FSettings : ISettings;

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : ISettings
    ); reintroduce; virtual;

    procedure OpenWslTerminal(const ADistribution: string);
    procedure OpenCmdTerminal;
    procedure OpenPowerShellTerminal;

    procedure LaunchPythonVenv(const ACommand : string = '');
    function CreatePythonVenv(
      const APythonPath : string;
      const AVenvName   : string
    ): Boolean;
    procedure LaunchJupyterLab;

    procedure Execute(
      const ACommand          : string;
      const AWorkingDirectory : string = ''
    );
  end;

implementation

{$R *.lfm}

uses
  Dialogs, Forms,

  ts.Core.Utils, ts.Core.Logger,

  SnippetSource.Resources;

{$REGION 'construction and destruction'}
constructor TdmTerminal.Create(AOwner: TComponent; ASettings: ISettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TdmTerminal.OpenWslTerminal(const ADistribution: string);
begin
  prcTerminal.Parameters.Clear;
  prcTerminal.Executable := 'wsl.exe';
  prcTerminal.Parameters.Add('--distribution');
  prcTerminal.Parameters.Add(ADistribution);
  prcTerminal.Execute;
end;

procedure TdmTerminal.OpenCmdTerminal;
begin
  prcTerminal.Parameters.Clear;
  prcTerminal.Executable := 'cmd.exe';
  prcTerminal.Execute;
end;

procedure TdmTerminal.OpenPowerShellTerminal;
begin
  prcTerminal.Parameters.Clear;
  prcTerminal.Executable := 'powershell.exe';
  prcTerminal.Execute;
end;

procedure TdmTerminal.LaunchPythonVenv(const ACommand: string);
var
  LVenv : string;
begin
  LVenv := FSettings.PythonVirtualEnvironmentName;
  if ACommand = '' then
    Execute(Format(LOAD_PYTHON_VENV, [LVenv]))
  else
    Execute(Format(EXECUTE_PYTHON_VENV, [LVenv, ACommand]));
end;

function TdmTerminal.CreatePythonVenv(const APythonPath: string;
  const AVenvName: string): Boolean;
var
  LProcess : TProcess;
  LOutput  : TStringList;
begin
  LOutput := TStringList.Create;
  try
    LProcess := TProcess.Create(nil);
    try
      LProcess.Executable := APythonPath;
      LProcess.Parameters.Add('-m');
      LProcess.Parameters.Add('venv');
      LProcess.Parameters.Add(AVenvName);
      LProcess.Options := [poUsePipes, poNoConsole];
      LProcess.Execute;
      LOutput.LoadFromStream(LProcess.Output);
      Logger.SendText(LOutput.Text);
      Result := (LOutput.Count > 0) and (Pos('already exists', LOutput[0]) = 0);
    finally
      LProcess.Free;
    end;
  finally
    LOutput.Free;
  end;
end;

procedure TdmTerminal.LaunchJupyterLab;
begin
  LaunchPythonVenv(LAUNCH_JUPYTER_LAB);
end;

procedure TdmTerminal.Execute(const ACommand: string;
  const AWorkingDirectory: string);
begin
  prcTerminal.Parameters.Clear;
  prcTerminal.Executable := 'cmd.exe';
  prcTerminal.Parameters.Add('/K');
  prcTerminal.Parameters.Add(ACommand);
  if AWorkingDirectory <> '' then
    prcTerminal.CurrentDirectory := AWorkingDirectory
  else
    prcTerminal.CurrentDirectory := ExtractFilePath(ParamStr(0));
  prcTerminal.Execute;
end;
{$ENDREGION}

end.

