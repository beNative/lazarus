{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Core.VersionInfo;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, FileInfo, Graphics;

type

  { TVersionInfo }

  TVersionInfo = class(TComponent)
  private
    FBuildDate       : string;
    FFPCVersion      : string;
    FLCLVersion      : string;
    FTargetCPU       : string;
    FTargetOS        : string;
    FFileVersionInfo : TFileVersionInfo;

    function GetCompanyName: string;
    function GetFileDescription: string;
    function GetFileName: string;
    function GetFileVersion: string;
    function GetIcon: TIcon;
    function GetInternalName: string;
    function GetLegalCopyright: string;
    function GetOriginalFilename: string;
    function GetProductName: string;
    function GetProductVersion: string;
    function GetURL: string;
    function GetWidgetSet: string;

  public
    procedure AfterConstruction; override;
    function AsString: string;

  published
    property FPCVersion: string
      read FFPCVersion;

    property LCLVersion: string
      read FLCLVersion;

    property LegalCopyright: string
      read GetLegalCopyright;

    property TargetOS: string
      read FTargetOS;

    property TargetCPU: string
      read FTargetCPU;

    property BuildDate: string
      read FBuildDate;

    property ProductVersion: string
      read GetProductVersion;

    property InternalName: string
      read GetInternalName;

    property OriginalFilename: string
      read GetOriginalFilename;

    property CompanyName: string
      read GetCompanyName;

    property FileVersion: string
      read GetFileVersion;

    property FileDescription: string
      read GetFileDescription;

    property FileName: string
      read GetFileName;

    property ProductName: string
      read GetProductName;

    property WidgetSet: string
      read GetWidgetSet;

    property URL: string
      read GetURL;

    property Icon: TIcon
      read GetIcon;
  end;

function VersionInfo: TVersionInfo;

implementation

uses
  Forms,

  LCLVersion, InterfaceBase;

const
  sGTKWidgetSet      = 'GTK';
  sGTK2WidgetSet     = 'GTK 2';
  sWin32_64WidgetSet = 'Win32/Win64';
  sWinCEWidgetSet    = 'WinCE';
  sCarbonWidgetSet   = 'Carbon';
  sQTWidgetSet       = 'QT';
  sFpGUIWidgetSet    = 'FpGUI';
  sOtherGUIWidgetSet = 'Other gui';

var
  FVersionInfo: TVersionInfo;


function VersionInfo: TVersionInfo;
begin
  if not Assigned(FVersionInfo) then
    FVersionInfo := TVersionInfo.Create(Application);
  Result := FVersionInfo;
end;



procedure TVersionInfo.AfterConstruction;
begin
  inherited AfterConstruction;
  FFPCVersion := {$I %FPCVERSION%};
  FTargetCPU := LowerCase({$I %FPCTARGETCPU%});
  FTargetOS := LowerCase({$I %FPCTARGETOS%});
  FBuildDate := {$I %DATE%};
  FFileVersionInfo := TFileVersionInfo.Create(Self);
  FFileVersionInfo.FileName := ParamStr(0);
  FFileVersionInfo.ReadFileInfo;
  FLCLVersion := lcl_version;
end;

function TVersionInfo.GetFileVersion: string;
begin
  Result := FFileVersionInfo.VersionStrings.Values['FileVersion'];
end;

function TVersionInfo.GetIcon: TIcon;
begin
  Result := Application.Icon;
end;

function TVersionInfo.GetInternalName: string;
begin
  Result := FFileVersionInfo.VersionStrings.Values['InternalName'];
end;

function TVersionInfo.GetLegalCopyright: string;
begin
  Result := FFileVersionInfo.VersionStrings.Values['LegalCopyright'];
end;

function TVersionInfo.GetOriginalFilename: string;
begin
  Result := FFileVersionInfo.VersionStrings.Values['OriginalFilename'];
end;

function TVersionInfo.GetProductName: string;
begin
  Result := FFileVersionInfo.VersionStrings.Values['ProductName'];
end;

function TVersionInfo.GetProductVersion: string;
begin
  Result := FFileVersionInfo.VersionStrings.Values['ProductVersion'];
end;

function TVersionInfo.GetURL: string;
begin
  Result := FFileVersionInfo.VersionStrings.Values['URL'];
end;

function TVersionInfo.GetFileName: string;
begin
Result := FFileVersionInfo.FileName;
end;

function TVersionInfo.GetFileDescription: string;
begin
  Result := FFileVersionInfo.VersionStrings.Values['FileDescription'];
end;

function TVersionInfo.GetCompanyName: string;
begin
  Result := FFileVersionInfo.VersionStrings.Values['CompanyName'];
end;

function TVersionInfo.GetWidgetSet: string;
begin
  case InterfaceBase.WidgetSet.LCLPlatform of
    lpGtk:
      Result := sGTKWidgetSet;
    lpGtk2:
      Result := sGTK2WidgetSet;
    lpWin32:
      Result := sWin32_64WidgetSet;
    lpWinCE:
      Result := sWinCEWidgetSet;
    lpCarbon:
      Result := sCarbonWidgetSet;
    lpQT:
      Result := sQTWidgetSet;
    lpfpGUI:
      Result := sFpGUIWidgetSet;
  else
    Result := sOtherGUIWidgetSet;
  end;
end;

function TVersionInfo.AsString: string;
const
  VERSION_INFO =
    '%s %s' + #13#10 +
    'Build date: %s' + #13#10 +
    'FPC version : %s' + #13#10 +
    'LCL version : %s' + #13#10 +
    'Target CPU: %s' + #13#10 +
    'Target OS: %s';
begin
  Result := Format(VERSION_INFO,
    [ApplicationName, FileVersion, BuildDate, FPCVersion, LCLVersion, TargetCPU,
    TargetOS]);
end;


end.

