unit ts_Core_VersionInfo;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, FileInfo, Graphics;

type
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

    property Icon: TIcon
      read GetIcon;
  end;

function VersionInfo: TVersionInfo;

//*****************************************************************************

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

//*****************************************************************************
// interfaced routines                                                   BEGIN
//*****************************************************************************

function VersionInfo: TVersionInfo;
begin
  if not Assigned(FVersionInfo) then
    FVersionInfo := TVersionInfo.Create(Application);
  Result := FVersionInfo;
end;

//*****************************************************************************
// interfaced routines                                                     END
//*****************************************************************************

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TVersionInfo.AfterConstruction;
begin
  inherited AfterConstruction;
  FFPCVersion := {$I %FPCVERSION%};
  FTargetCPU := LowerCase({$I %FPCTARGETCPU%});
  FTargetOS := LowerCase({$I %FPCTARGETOS%});
  FBuildDate := {$I %DATE%};
  FFileVersionInfo := TFileVersionInfo.Create(Self);
  FFileVersionInfo.FileName := ParamStr(0);
  FLCLVersion := lcl_version;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TVersionInfo.GetFileVersion: string;
begin
  Result := FFileVersionInfo.GetVersionSetting('FileVersion');
end;

function TVersionInfo.GetIcon: TIcon;
begin
  Result := Application.Icon;
end;

function TVersionInfo.GetInternalName: string;
begin
  Result := FFileVersionInfo.GetVersionSetting('InternalName');
end;

function TVersionInfo.GetLegalCopyright: string;
begin
  Result := FFileVersionInfo.GetVersionSetting('LegalCopyright');
end;

function TVersionInfo.GetOriginalFilename: string;
begin
  Result := FFileVersionInfo.GetVersionSetting('OriginalFilename');
end;

function TVersionInfo.GetProductName: string;
begin
  Result := FFileVersionInfo.GetVersionSetting('ProductName');
end;

function TVersionInfo.GetProductVersion: string;
begin
  Result := FFileVersionInfo.GetVersionSetting('ProductVersion');
end;

function TVersionInfo.GetFileName: string;
begin
  Result := FFileVersionInfo.FileName;
end;

function TVersionInfo.GetFileDescription: string;
begin
  Result := FFileVersionInfo.GetVersionSetting('FileDescription');
end;

function TVersionInfo.GetCompanyName: string;
begin
  Result := FFileVersionInfo.GetVersionSetting('CompanyName');
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

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

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

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

end.

