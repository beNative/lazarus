unit ts.Core.SystemInfo;

{$MODE Delphi}

interface

uses
  Classes, SysUtils;

{ Platform neutral singleton to retrieve system information. }

type

  { SystemInfo }

  SystemInfo = record
  private
    class function GetCurrentUserName: string; static;
    class function GetMACAddress: string; static;
  public

    class property CurrentUserName: string
      read GetCurrentUserName;

    class property MACAddress: string
      read GetMACAddress;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  DynLibs, Windows, JwaTlHelp32;
{$ENDIF}

const
  IP_ADDRESS_MASK  = '%d.%d.%d.%d';
  MAC_ADDRESS_MASK = '%2.2x-%2.2x-%2.2x-%2.2x-%2.2x-%2.2x';

{ SystemInfo }

class function SystemInfo.GetCurrentUserName: string; static;
begin
  Result := '';
  {$IFDEF UNIX}
    Result := {$I %USER%};
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    Result := {$I %USERNAME%};
  {$ENDIF}
end;

class function SystemInfo.GetMACAddress: string; static;
{$IFDEF MSWINDOWS}
type
  TCreateGUIDFunction = function(AGUID: PGUID): LongInt; stdcall;
{$ENDIF}
var
{$IFDEF UNIX}
  VPath, VDevice: string;
{$ENDIF}
{$IFDEF MSWINDOWS}
  VLibHandle: TLibHandle;
  VCreateGUIDFunction: TCreateGUIDFunction;
  VGUID1, VGUID2: TGUID;
{$ENDIF}
begin
  Result := '';
{$IFDEF UNIX}
  VDevice := 'eth0';
  VPath := Format('/sys/class/net/%s/address', [VDevice]);
  if FileExists(VPath) then
    Result := LSLoadFile(VPath)
  else
    Result := 'Could not find the device "' + VDevice + '".';
{$ENDIF}
{$IFDEF MSWINDOWS}
  VLibHandle := LoadLibrary('rpcrt4.dll');
  try
    if VLibHandle <> NilHandle then
    begin
      VCreateGUIDFunction := TCreateGUIDFunction(
        GetProcedureAddress(
          VLibHandle,
          'UuidCreateSequential'
        )
      );
      if Assigned(VCreateGUIDFunction) then
      begin
        if (VCreateGUIDFunction(@VGUID1) = 0)
          and (VCreateGUIDFunction(@VGUID2) = 0)
          and (VGUID1.D4[2] = VGUID2.D4[2])
          and (VGUID1.D4[3] = VGUID2.D4[3])
          and (VGUID1.D4[4] = VGUID2.D4[4])
          and (VGUID1.D4[5] = VGUID2.D4[5])
          and (VGUID1.D4[6] = VGUID2.D4[6])
          and (VGUID1.D4[7] = VGUID2.D4[7]) then
        begin
          Result := Format(
            MAC_ADDRESS_MASK, [
              VGUID1.D4[2],
              VGUID1.D4[3],
              VGUID1.D4[4],
              VGUID1.D4[5],
              VGUID1.D4[6],
              VGUID1.D4[7]
            ]
          );
        end;
      end;
    end;
  finally
    UnloadLibrary(VLibHandle);
  end;
{$ENDIF}
end;

end.

