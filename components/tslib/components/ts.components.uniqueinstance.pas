{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit ts.Components.UniqueInstance;

{
  UniqueInstance is a component to allow only a instance by program

  Copyright (C) 2006 Luiz Americo Pereira Camara
  pascalive@bol.com.br

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{ Modifications by Tim Sinaeve:
  - Enabled can be used at runtime. In the original version this setting had
    only effect when set at designtime.
  - It does not use SimpleIPCWrapper anymore as the bugs for which it was
    a workaround are resolved (bug 17248) and (bug 19136) in FPC 2.7.1
  - Added OnTerminateInstance notify event }

{$MODE objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils,

  SimpleIPC;

type
  TOnOtherInstance = procedure (
    Sender     : TObject;
    ParamCount : Integer;
    Parameters : array of string
  ) of object;

  { TUniqueInstance }

  TUniqueInstance = class(TComponent)
  private
    FIdentifier          : string;
    FIPCServer           : TSimpleIPCServer;
    FOnOtherInstance     : TOnOtherInstance;
    FOnTerminateInstance : TNotifyEvent;
    FUpdateInterval      : Cardinal;
    FEnabled             : Boolean;

    function GetServerId: String;
    procedure ReceiveMessage(Sender: TObject);
    procedure SetEnabled(AValue: Boolean);
    procedure TerminateApp(Sender: TObject; var Done: Boolean);
    {$IFDEF UNIX}
    procedure CheckMessage(Sender: TObject);
    {$ENDIF}
  protected
    procedure InitIPC;
  public
    procedure AfterConstruction; override;

  published
    property Enabled: Boolean
      read FEnabled write SetEnabled default False;

    property Identifier: String
      read FIdentifier write FIdentifier;

    property UpdateInterval: Cardinal
      read FUpdateInterval write FUpdateInterval default 1000;

    property OnOtherInstance: TOnOtherInstance
      read FOnOtherInstance write FOnOtherInstance;

    property OnTerminateInstance: TNotifyEvent
      read FOnTerminateInstance write FOnTerminateInstance;
  end;

implementation

uses
  ExtCtrls,

  StrUtils;

const
  BaseServerId = 'tuniqueinstance_';
  Separator = '|';

function GetFormattedParams: String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to ParamCount do
    Result := Result + ParamStr(i) + Separator;
end;

{ TUniqueInstance }

procedure TUniqueInstance.ReceiveMessage(Sender: TObject);
var
  TempArray: array of String;
  Count,i: Integer;

  procedure GetParams(const AStr: String);
  var
    pos1,pos2:Integer;
  begin
    SetLength(TempArray, Count);
    //fill params
    i := 0;
    pos1:=1;
    pos2:=pos(Separator, AStr);
    while pos1 < pos2 do
    begin
      TempArray[i] := Copy(AStr, pos1, pos2 - pos1);
      pos1 := pos2+1;
      pos2 := posex(Separator, AStr, pos1);
      inc(i);
    end;
  end;

begin
  if Assigned(FOnOtherInstance) then
  begin
    //MsgType stores ParamCount
    Count := FIPCServer.MsgType;
    GetParams(FIPCServer.StringMessage);
    FOnOtherInstance(Self, Count, TempArray);
    SetLength(TempArray, 0);
  end;
end;

procedure TUniqueInstance.SetEnabled(AValue: Boolean);
begin
  if AValue <> Enabled then
  begin
    FEnabled := AValue;
    InitIPC;
  end;
end;

{$IFDEF UNIX}
procedure TUniqueInstance.CheckMessage(Sender: TObject);
begin
  FIPCServer.PeekMessage(1, True);
end;
{$ENDIF}

procedure TUniqueInstance.TerminateApp(Sender: TObject; var Done: Boolean);
begin
  if Assigned(FOnTerminateInstance) then
    FOnTerminateInstance(Self);
  Application.Terminate;
  //necessary to avoid being a zombie
  Done := False;
end;

function TUniqueInstance.GetServerId: String;
begin
  if FIdentifier <> '' then
    Result := BaseServerId + FIdentifier
  else
    Result := BaseServerId + ExtractFileName(ParamStr(0));
end;

procedure TUniqueInstance.InitIPC;
var
  IPCClient: TSimpleIPCClient;
  {$IFDEF UNIX}
  Timer: TTimer;
  {$ENDIF}
begin
  if FEnabled then
  begin
    IPCClient := TSimpleIPCClient.Create(Self);
    try
      IPCClient.ServerId := GetServerId;
      if IPCClient.ServerRunning then
      begin
        //A instance is already running
        //Send a message and then exit
        if Assigned(FOnOtherInstance) then
        begin
          IPCClient.Active := True;
          IPCClient.SendStringMessage(ParamCount, GetFormattedParams);
        end;
        Application.ShowMainForm := False;
        Application.AddOnIdleHandler(@TerminateApp);
      end
      else
      begin
        //It's the first instance. Init the server
        if Assigned(FIPCServer) then
          FreeAndNil(FIPCServer);

        FIPCServer := TSimpleIPCServer.Create(Self);
        FIPCServer.ServerID := IPCClient.ServerId;
        FIPCServer.Global := True;
        FIPCServer.OnMessage := @ReceiveMessage;
        FIPCServer.StartServer;
        {$IFDEF UNIX}
        if Assigned(FOnOtherInstance) then
        begin
          Timer := TTimer.Create(Self);
          Timer.Interval := FUpdateInterval;
          Timer.OnTimer := @CheckMessage;
        end;
        {$ENDIF}
      end;
    finally
      IPCClient.Free;
    end;
  end
  else
    if Assigned(FIPCServer) then
      FreeAndNil(FIPCServer);
end;

procedure TUniqueInstance.AfterConstruction;
begin
  inherited AfterConstruction;
  FUpdateInterval := 1000;
end;

end.

