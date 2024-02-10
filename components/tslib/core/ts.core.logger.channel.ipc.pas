{
  Copyright (C) 2013-2024 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Core.Logger.Channel.Ipc;

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
  Classes, SysUtils, Windows,

  simpleipc,

  ts.Core.Logger.Interfaces, ts.Core.Logger.Channel;

const
  IPC_SERVER_ID = 'ipc_log_server';

type
  { TIpcChannel }

  TIpcChannel = class (TCustomLogChannel)
  private
    FClient : TSimpleIPCClient;
    FBuffer : TBytesStream;

  protected
    function GetConnected: Boolean; override;

  public
    constructor Create(AEnabled: Boolean = True); override;
    destructor Destroy; override;

    function Connect: Boolean; override;
    function Disconnect: Boolean; override;

    function Write(const AMsg: TLogMessage): Boolean; override;

  end;

implementation

{$REGION 'construction and destruction'}
constructor TIpcChannel.Create(AEnabled: Boolean);
begin
  inherited Create;
  FBuffer := TBytesStream.Create;
  FClient := TSimpleIPCClient.Create(nil);
  with FClient do
  begin
    ServerID := IPC_SERVER_ID;
    if ServerRunning then
    begin
      Self.Enabled := True;
      Active := True;
    end
    else
      Active := False;
  end;
end;

destructor TIpcChannel.Destroy;
begin
  FClient.Free;
  FBuffer.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TIpcChannel.GetConnected: Boolean;
begin
  Result := FClient.Active;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TIpcChannel.Connect: Boolean;
begin
  FClient.Connect;
  Result := True;
end;

function TIpcChannel.Disconnect: Boolean;
begin
  FClient.Disconnect;
  Result := True;
end;
{
  Data is streamed in following order:
    - Message type:  4 bytes (Integer)
    - TimeStamp:     8 bytes (Double)
    - TextSize:      4 bytes (Integer)
    - Text:          TextSize bytes (UTF8 encoded, backwards compatible with Ansi)
    - DataSize:      4 bytes (Integer)
    - Data:          DataSize bytes
}

function TIpcChannel.Write(const AMsg: TLogMessage): Boolean;
const
  ZERO_BUF : Integer = 0;
var
  LTextSize : Integer;
  LDataSize : Integer;
begin
  Result := False;
  if Enabled then
  begin
    if not Connected and AutoConnect then
      Connect;
    if Connected then
    begin
      FBuffer.Clear;
      LTextSize := Length(AMsg.Text);
      FBuffer.Seek(0, soFromBeginning);
      FBuffer.WriteBuffer(AMsg.MsgType, SizeOf(TLogMessageType));
      FBuffer.WriteBuffer(AMsg.LogLevel, SizeOf(Byte));
      FBuffer.WriteBuffer(AMsg.Reserved1, SizeOf(Byte));
      FBuffer.WriteBuffer(AMsg.Reserved2, SizeOf(Byte));
      FBuffer.WriteBuffer(AMsg.TimeStamp, SizeOf(TDateTime));
      FBuffer.WriteBuffer(LTextSize, SizeOf(Integer));
      if LTextSize > 0 then
        FBuffer.WriteBuffer(AMsg.Text[1], LTextSize);
      if AMsg.Data <> nil then
      begin
        LDataSize := AMsg.Data.Size;
        FBuffer.WriteBuffer(LDataSize, SizeOf(Integer));
        AMsg.Data.Position := 0;
        FBuffer.CopyFrom(AMsg.Data, LDataSize);
      end
      else
        FBuffer.WriteBuffer(ZERO_BUF, SizeOf(Integer)); // indicates empty stream
      FClient.SendMessage(LongInt(GetCurrentProcessId), FBuffer);
      //FClient.SendStream(FBuffer);
      Result := True;
    end;
  end;
end;
{$ENDREGION}

end.

