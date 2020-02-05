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

unit ts.Core.Logger.Channel.IPC;

{ Copyright (C) 2006 Luiz Américo Pereira Câmara

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,

  simpleipc,

  ts.Core.Logger.Interfaces, ts.Core.Logger.Channel;

type

  { TIPCChannel }

  TIPCChannel = class (TCustomLogChannel)
  private
    FClient: TSimpleIPCClient;
    FBuffer: TMemoryStream;
  public
    constructor Create(AEnabled: Boolean = True); override;
    destructor Destroy; override;
    function Write(const AMsg: TLogMessage): Boolean; override;

  end;


implementation

const
  ZeroBuf: Integer = 0;

{ TIPCChannel }

constructor TIPCChannel.Create(AEnabled: Boolean);
begin
  FBuffer:=TMemoryStream.Create;
  FClient := TSimpleIPCClient.Create(nil);
  with FClient do
  begin
    ServerID:='ipc_log_server';
    if ServerRunning then
    begin
      Self.Enabled := True;
      Active := True;
    end
    else
      Active := False;
  end;
end;

destructor TIPCChannel.Destroy;
begin
  FClient.Destroy;
  FBuffer.Destroy;
end;

function TIPCChannel.Write(const AMsg: TLogMessage): Boolean;
var
  TextSize, DataSize: Integer;
begin
  with FBuffer do
  begin
    TextSize:=Length(AMsg.Text);
    Seek(0,soFromBeginning);
    WriteBuffer(AMsg.MsgType,SizeOf(Integer));
    WriteBuffer(AMsg.TimeStamp,SizeOf(TDateTime));
    WriteBuffer(TextSize,SizeOf(Integer));
    WriteBuffer(AMsg.Text[1],TextSize);
    if AMsg.Data <> nil then
    begin
      DataSize := AMsg.Data.Size;
      //WriteLn('[IPCChannel] Size Of Stream: ',DataSize);
      WriteBuffer(DataSize,SizeOf(Integer));
      AMsg.Data.Position := 0;
      CopyFrom(AMsg.Data,DataSize);
      //WriteLn('DataCopied: ',CopyFrom(AMsg.Data,DataSize));
    end
    else
      WriteBuffer(ZeroBuf,SizeOf(Integer));//necessary?
  end;
  FClient.SendMessage(mtUnknown,FBuffer);
  Result := True;
end;


end.

