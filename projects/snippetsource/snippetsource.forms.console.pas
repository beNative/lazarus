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

{$mode delphi}

interface

uses
  Classes, SysUtils, process, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, uCmdBox;

type

  { TfrmConsole }

  TfrmConsole = class(TForm)
    cmdMain : TCmdBox;
    prcMain : TProcess;
    tmrMain : TTimer;
    procedure cmdMainInput(ACmdBox: TCmdBox; Input: string);
    procedure edtInputKeyPress(Sender: TObject; var Key: char);
    procedure tmrMainTimer(Sender: TObject);


  private
    procedure ProcessString(const AString: string);

  public
     procedure AfterConstruction; override;

  end;

implementation

{$R *.lfm}

procedure TfrmConsole.AfterConstruction;
begin
  inherited AfterConstruction;
  prcMain.Active  := True;
  tmrMain.Enabled := True;
  cmdMain.StartRead(clSilver,clNavy,'',clYellow,clNavy);
end;

procedure TfrmConsole.tmrMainTimer(Sender: TObject);
var
  //LBuf: array[0..65535] of Char;
  LBuf : array[0..1024] of Char;
  S : string;
begin
  if prcMain.Output.NumBytesAvailable > 0 then
  begin
    while prcMain.Output.NumBytesAvailable > 0 do
    begin
      FillChar(LBuf, SizeOf(LBuf), #0);
      prcMain.Output.Read(LBuf, SizeOf(LBuf) - 1);
      S := LBuf;
      ProcessString(S);
      //cmdMain.Write(S);
      //cmdMain.StartRead(clSilver,clNavy,'',clYellow,clNavy);
    end;
  end;
end;

procedure TfrmConsole.ProcessString(const AString: string);
var
  SL : TStringList;
  I  : Integer;
  S  : string;
begin
  SL := TStringList.Create;
  try
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

end;

procedure TfrmConsole.edtInputKeyPress(Sender: TObject; var Key: char);
//var
//  S : string;
begin
  //if Key = #13 then
  //begin
  //  Key:= #0;
  //  if not prcMain.Active then
  //    prcMain.Active := True;
  //  S := edtInput.Text + LineEnding;
  //  prcMain.Input.Write(S[1], Length(S));
  //end;
end;

procedure TfrmConsole.cmdMainInput(ACmdBox: TCmdBox; Input: string);
var
  S : string;
begin
  S := Input + LineEnding;
  prcMain.Input.Write(S[1], Length(S));
end;

end.

