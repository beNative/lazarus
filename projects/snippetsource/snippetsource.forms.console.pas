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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
  //cmdMain.StartRead(clSilver, clBlack, '', clWhite, clBlack);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmConsole.tmrMainTimer(Sender: TObject);
var
  LBuf : array[0..2048] of Char;
  S    : string;
begin
  LBuf := '';
  if prcMain.Output.NumBytesAvailable > 0 then
  begin
    while prcMain.Output.NumBytesAvailable > 0 do
    begin
      Logger.Watch('prcMain.Output.NumBytesAvailable', prcMain.Output.NumBytesAvailable);
      FillChar(LBuf, SizeOf(LBuf), #0);
      prcMain.Output.Read(LBuf, SizeOf(LBuf) - 1);
      //prcMain.Output.Read(LBuf, prcMain.Output.NumBytesAvailable);
      //prcMain.Output.Read(LBuf, prcMain.Output.NumBytesAvailable);
      S := LBuf;
      //Logger.Watch('LBuf', S);
      ProcessString(S);

      //cmdMain.Write(S);
    end;
    //cmdMain.StartRead(clSilver, clBlack, '', clWhite, clBlack);
  end;
end;

procedure TfrmConsole.cmdMainInput(ACmdBox: TCmdBox; Input: string);
var
  S : string;
begin
  Logger.Enter(Self, 'cmdMainInput');
  S := Input + LineEnding;
  Logger.SendText(S);
  prcMain.Input.Write(S[1], Length(S));
  //ProcessString(S);
  //PythonEngine.ExecString(S);
  Logger.Leave(Self, 'cmdMainInput');
end;

procedure TfrmConsole.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  cmdMain.Clear;
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
  SL := TStringList.Create;
  try
    SL.Text := AString;
    Logger.Send('CharCount', Length(AString));
    Logger.Send('LineCount', SL.Count);
    Logger.Send('AString', AString);
    if SL.Count > 0 then
    begin
      for I := 0 to SL.Count - 1 do
      begin
        S := SL[I];
        //Logger.Send('Char', S[Length(S)]);
        if (I = (SL.Count - 1)) and (S[Length(S)] = '>') then
        begin
          cmdMain.StartRead(clSilver, clBlack, S, clWhite, clBlack);
          Logger.Info(S);
        end
        else if I < (SL.Count - 1) then
        begin
          Logger.Send(IntToStr(I), S);
          cmdMain.Writeln(S);
        end
        else
        begin
          Logger.Send(IntToStr(I), S);
          cmdMain.Write(S);
        end;
      end;
    end
    else
      cmdMain.StartRead(clSilver, clBlack, AString, clWhite, clBlack);
  finally
    SL.Free;
  end;
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
