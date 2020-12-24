{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit SnippetSource.Forms.Console;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, process, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,

  PythonEngine, uCmdBox;

type

  { TfrmConsole }

  TfrmConsole = class(TForm)
    cmdMain           : TCmdBox;
    prcMain           : TProcess;
    PythonEngine      : TPythonEngine;
    PythonInputOutput : TPythonInputOutput;
    tmrMain           : TTimer;

    procedure cmdMainInput(ACmdBox: TCmdBox; Input: string);
    procedure PythonInputOutputReceiveData(Sender: TObject; var Data: AnsiString
      );
    procedure PythonInputOutputSendData(Sender: TObject; const Data: AnsiString
      );
    procedure tmrMainTimer(Sender: TObject);

  private
    procedure ProcessString(const AString: string);

  public
     procedure AfterConstruction; override;

     procedure Execute(const AFileName: string);
     procedure ExecutePy(const AStrings: TStrings);

  end;

implementation

uses
  ts.Core.Logger;

{$R *.lfm}

{$REGION 'construction and destruction'}
procedure TfrmConsole.AfterConstruction;
begin
  inherited AfterConstruction;
  prcMain.Active  := True;
  tmrMain.Enabled := True;
    //PythonEngine.LoadDll;
  cmdMain.StartRead(clSilver, clNavy, '', clYellow, clNavy);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmConsole.tmrMainTimer(Sender: TObject);
var
  LBuf : array[0..1024] of Char;
  S    : string;
begin
  LBuf := '';
  if prcMain.Output.NumBytesAvailable > 0 then
  begin
    while prcMain.Output.NumBytesAvailable > 0 do
    begin
      FillChar(LBuf, SizeOf(LBuf), #0);
      prcMain.Output.Read(LBuf, SizeOf(LBuf) - 1);
      S := LBuf;
    //  ProcessString(S);
      cmdMain.Write(S);
      cmdMain.StartRead(clSilver,clNavy,'',clYellow,clNavy);
    end;
  end;
end;

procedure TfrmConsole.cmdMainInput(ACmdBox: TCmdBox; Input: string);
var
  S : string;
begin
  S := Input + LineEnding;
  prcMain.Input.Write(S[1], Length(S));
  //ProcessString(S);
  //PythonEngine.ExecString(S);
end;

procedure TfrmConsole.PythonInputOutputReceiveData(Sender: TObject;
  var Data: AnsiString);
begin
  //ProcessString(Data);
  //PythonEngine.ExecString(Data);
end;

procedure TfrmConsole.PythonInputOutputSendData(Sender: TObject;
  const Data: AnsiString);
begin
//  ProcessString(Data);
  //      cmdMain.Writeln(Data);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmConsole.ProcessString(const AString: string);
var
  SL : TStringList;
  I  : Integer;
  S  : string;
begin
  Logger.Enter(Self, 'ProcessString');
  SL := TStringList.Create;
  try
    Logger.SendText(AString);
    SL.Text := AString;
    for I := 0 to SL.Count - 1 do
    begin
      S := SL[I];
      if I < SL.Count - 1 then
      begin
        cmdMain.Writeln(S);
      end
      else
        cmdMain.StartRead(clSilver, clBlack, S, clWhite, clBlack);
    end;
  finally
    SL.Free;
  end;
  Logger.Leave(Self, 'ProcessString');
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmConsole.Execute(const AFileName: string);
begin
  Logger.Enter(Self, 'Execute');
  prcMain.Executable := 'cmd';
  prcMain.Parameters.Add('/K call ' + AFileName);
  prcMain.Execute;
  cmdMain.SetFocus;
  Logger.Leave(Self, 'Execute');
end;

procedure TfrmConsole.ExecutePy(const AStrings: TStrings);
begin
  PythonEngine.ExecStrings(AStrings);
end;
{$ENDREGION}

end.
