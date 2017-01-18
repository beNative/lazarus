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

{ File Association for all Windows versions.
  This includes Windows Vista/7/8 Default Programs feature.
  Author: Lainz. }

unit ts.Components.FileAssociation;

{$MODE Delphi}

interface

uses
{$IFDEF Windows}
  ShlObj,
{$ENDIF}
  Classes, SysUtils, Registry;

const
  HKCR = HKEY_CLASSES_ROOT;
  HKCU = HKEY_CURRENT_USER;
  HKLM = HKEY_LOCAL_MACHINE;
  HKU  = HKEY_USERS;
  HKCC = HKEY_CURRENT_CONFIG;

type
  TAppReg = record
    AppName,        // 'Lazarus IDE'
    AppDescription, // 'Open Source IDE for Free Pascal'
    AppNameNoSpaces,// 'LazarusIDE'
    ExtData,        // '.lpr'
    ExtIcon,        // 'C:\FPC\Lazarus\images\lazaruspackage.ico'
    ExtName,        // 'Lazarus package source file'
    ExtNameNoSpaces,// 'LazarusPackageSourceFile'
    CmdData,        // '"C:\FPC\Lazarus\lazarus.exe" "%1"'
    CmdIcon,        // 'C:\FPC\Lazarus\lazarus.exe,0'
    CmdName,        // 'Open Project'
    CmdNameNoSpaces // 'Open'
    : string;
  end;

  { TFileAssociation }

  TFileAssociation = class(TComponent)
  private
    FAppName                 : string;
    FAppDescription          : string;
    FAppNameNoSpaces         : string;
    FExtData                 : string;
    FExtIcon                 : string;
    FExtName                 : string;
    FExtNameNoSpaces         : string;
    FCmdData                 : string;
    FCmdIcon                 : string;
    FCmdName                 : string;
    FCmdNameNoSpaces         : string;
    FRegisterAssociation     : Boolean;
    FRegisterForAllUsers     : Boolean;
    FAddAppToDefaultPrograms : Boolean;
    FAddExtToDefaultPrograms : Boolean;

    procedure SetFAddAppToDefaultProgams(AValue: Boolean);
    procedure SetFAddExtToDefaultProgams(AValue: Boolean);
    procedure SetFAppDescription(AValue: string);
    procedure SetFAppName(AValue: string);
    procedure SetFAppNameNoSpaces(AValue: string);
    procedure SetFCmdData(AValue: string);
    procedure SetFCmdIcon(AValue: string);
    procedure SetFCmdName(AValue: string);
    procedure SetFCmdNameNoSpaces(AValue: string);
    procedure SetFExtData(AValue: string);
    procedure SetFExtIcon(AValue: string);
    procedure SetFExtName(AValue: string);
    procedure SetFExtNameNoSpaces(AValue: string);
    procedure SetFRegisterAssociation(AValue: Boolean);
    procedure SetFRegisterForAllUsers(AValue: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    procedure ClearIconCache;

  published
    property AppName: string
      read FAppName write SetFAppName;

    property AppDescription: string
      read FAppDescription write SetFAppDescription;

    property AppNameNoSpaces: string
      read FAppNameNoSpaces write SetFAppNameNoSpaces;

    property ExtData: string
      read FExtData write SetFExtData;

    property ExtIcon: string
      read FExtIcon write SetFExtIcon;

    property ExtName: string
      read FExtName write SetFExtName;

    property ExtNameNoSpaces: string
      read FExtNameNoSpaces write SetFExtNameNoSpaces;

    property CmdData: string
      read FCmdData write SetFCmdData;

    property CmdIcon: string
      read FCmdIcon write SetFCmdIcon;

    property CmdName: string
      read FCmdName write SetFCmdName;

    property CmdNameNoSpaces: string
      read FCmdNameNoSpaces write SetFCmdNameNoSpaces;

    property RegisterAssociation: Boolean
      read FRegisterAssociation write SetFRegisterAssociation;

    property RegisterForAllUsers: Boolean
      read FRegisterForAllUsers write SetFRegisterForAllUsers;

    property AddAppToDefaultPrograms: Boolean
      read FAddAppToDefaultPrograms write SetFAddAppToDefaultProgams;

    property AddExtToDefaultPrograms: Boolean
      read FAddExtToDefaultPrograms write SetFAddExtToDefaultProgams;
  end;

{ Write a string to Root\SubKey\ ValueName=ValueData }
function WriteString(Root: HKEY; SubKey: string; ValueName: string;
  ValueData: string): Boolean;

{ Registry 'Class' containing the icon }
function WriteFileAssociationClass(Root: HKEY;
  AppNameNoSpaces, ExtIcon, ExtName, ExtNameNoSpaces: string): Boolean; overload;
function WriteFileAssociationClass(Root: HKEY; app: TAppReg): Boolean; overload;

{ Add a command like 'Open', 'Edit', 'Print' or other }
function WriteFileAssociationClassCommand(Root: HKEY;
  AppNameNoSpaces, ExtNameNoSpaces, CmdName, CmdNameNoSpaces, CmdIcon,
  CmdData: string): Boolean; overload;
function WriteFileAssociationClassCommand(Root: HKEY; app: TAppReg): Boolean;
overload;

{ Associate the 'Class' with the file extension }
function WriteFileAssociation(Root: HKEY;
  AppNameNoSpaces, ExtNameNoSpaces, ExtData: string): Boolean; overload;
function WriteFileAssociation(Root: HKEY; app: TAppReg): Boolean; overload;

{ Add our application to Default Programs (Vista+) }
function WriteDefaultPrograms(AppNameNoSpaces, AppName, AppDescription: string): Boolean;
  overload;
function WriteDefaultPrograms(app: TAppReg): Boolean; overload;

{ Add extension to our application Default Programs page }
function WriteDefaultProgramsAddExt(AppNameNoSpaces, ExtData,
  ExtNameNoSpaces: string): Boolean; overload;
function WriteDefaultProgramsAddExt(app: TAppReg): Boolean; overload;

{ Create a File Association with all the previous functions together }
{ RegisterAssociation = True = this do the real association; = False = only the 'Class' will be created }
{ RegisterForAllUsers = True = this uses HKLM; = False uses HKCU and does not AddToDefaultPrograms}
{ AddAppToDefaultPrograms = True = add app to default programs in control panel, it only need to be called one time}
{ AddExtToDefaultPrograms = True = add ext to default programs in control panel, it needs AddAppToDefaultPrograms at least one time}
function CreateFileAssociation(
  App: TAppReg;
  RegisterAssociation,
  RegisterForAllUsers,
  AddAppToDefaultPrograms,
  AddExtToDefaultPrograms: Boolean
): Boolean;

{ Update icons }
procedure ClearIconCache;

implementation

function WriteString(Root: HKEY; SubKey: string; ValueName: string;
  ValueData: string): Boolean;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  reg.RootKey := Root;

  if reg.OpenKey(SubKey, True) then
  begin
    reg.WriteString(ValueName, ValueData);
    reg.CloseKey;
    reg.Free;
    Result := True;
  end
  else
    Result := False;
end;

function WriteFileAssociationClass(Root: HKEY;
  AppNameNoSpaces, ExtIcon, ExtName, ExtNameNoSpaces: string): Boolean;
var
  r1, r2: Boolean;
begin
  r1 := WriteString(Root, concat('Software\Classes\', AppNameNoSpaces,
    '.AssocFile.', ExtNameNoSpaces), '', ExtName);
  r2 := WriteString(Root, concat('Software\Classes\', AppNameNoSpaces,
    '.AssocFile.', ExtNameNoSpaces, '\DefaultIcon'), '', ExtIcon);

  if (r1 = False) or (r2 = False) then
    Result := False
  else
    Result := True;
end;

function WriteFileAssociationClass(Root: HKEY; app: TAppReg): Boolean;
begin
  Result := WriteFileAssociationClass(Root, app.AppNameNoSpaces,
    app.ExtIcon, app.ExtName, app.ExtNameNoSpaces);
end;

function WriteFileAssociationClassCommand(Root: HKEY;
  AppNameNoSpaces, ExtNameNoSpaces, CmdName, CmdNameNoSpaces, CmdIcon,
  CmdData: string): Boolean;
var
  r1, r2, r3: Boolean;
begin
  r1 := WriteString(Root, concat('Software\Classes\', AppNameNoSpaces,
    '.AssocFile.', ExtNameNoSpaces, '\Shell\', CmdNameNoSpaces), '', CmdName);
  r2 := WriteString(Root, concat('Software\Classes\', AppNameNoSpaces,
    '.AssocFile.', ExtNameNoSpaces, '\Shell\', CmdNameNoSpaces), 'Icon', CmdIcon);
  r3 := WriteString(Root, concat('Software\Classes\', AppNameNoSpaces,
    '.AssocFile.', ExtNameNoSpaces, '\Shell\', CmdNameNoSpaces, '\Command'),
    '', CmdData);

  if (r1 = False) or (r2 = False) or (r3 = False) then
    Result := False
  else
    Result := True;
end;

function WriteFileAssociationClassCommand(Root: HKEY; app: TAppReg): Boolean;
begin
  Result := WriteFileAssociationClassCommand(Root, app.AppNameNoSpaces,
    app.ExtNameNoSpaces, app.CmdName, app.CmdNameNoSpaces, app.CmdIcon, app.CmdData);
end;

function WriteFileAssociation(Root: HKEY;
  AppNameNoSpaces, ExtNameNoSpaces, ExtData: string): Boolean;
begin
  Result := WriteString(Root, concat('Software\Classes\', ExtData),
    '', concat(AppNameNoSpaces, '.AssocFile.', ExtNameNoSpaces));
end;

function WriteFileAssociation(Root: HKEY; app: TAppReg): Boolean;
begin
  Result := WriteFileAssociation(Root, app.AppNameNoSpaces, app.ExtNameNoSpaces,
    app.ExtData);
end;

function WriteDefaultPrograms(AppNameNoSpaces, AppName, AppDescription: string): Boolean;
var
  r1, r2, r3, r4: Boolean;
begin
  r1 := WriteString(HKLM, concat('Software\', AppNameNoSpaces,
    '\Capabilities'), '', '');
  r2 := WriteString(HKLM, concat('Software\', AppNameNoSpaces, '\Capabilities'),
    'ApplicationName', AppName);
  r3 := WriteString(HKLM, concat('Software\', AppNameNoSpaces, '\Capabilities'),
    'ApplicationDescription', AppDescription);
  r4 := WriteString(HKLM, 'Software\RegisteredApplications',
    AppNameNoSpaces, concat('Software\', AppNameNoSpaces, '\Capabilities'));
  if (r1 = False) or (r2 = False) or (r3 = False) or (r4 = False) then
    Result := False
  else
    Result := True;
end;

function WriteDefaultPrograms(app: TAppReg): Boolean;
begin
  Result := WriteDefaultPrograms(app.AppNameNoSpaces, app.AppName, app.AppDescription);
end;

function WriteDefaultProgramsAddExt(AppNameNoSpaces, ExtData,
  ExtNameNoSpaces: string): Boolean;
begin
  Result := WriteString(HKLM, concat('Software\', AppNameNoSpaces,
    '\Capabilities\FileAssociations'), ExtData,
    concat(AppNameNoSpaces, '.AssocFile.', ExtNameNoSpaces));
end;

function WriteDefaultProgramsAddExt(app: TAppReg): Boolean;
begin
  Result := WriteDefaultProgramsAddExt(app.AppNameNoSpaces, app.ExtData,
    app.ExtNameNoSpaces);
end;

function CreateFileAssociation(App: TAppReg;
  RegisterAssociation, RegisterForAllUsers, AddAppToDefaultPrograms,
  AddExtToDefaultPrograms: Boolean): Boolean;
var
  r1, r2, r3, r4, r5: Boolean;
  Root: HKEY;
begin
  if RegisterForAllUsers then
    Root := HKLM
  else
    Root := HKCU;

  r1 := WriteFileAssociationClass(Root, App);
  r2 := WriteFileAssociationClassCommand(Root, App);

  if RegisterAssociation then
    r3 := WriteFileAssociation(Root, App);

  if Root <> HKCU then
  begin
    if AddAppToDefaultPrograms then
      r4 := WriteDefaultPrograms(App);
    if AddExtToDefaultPrograms then
      r5 := WriteDefaultProgramsAddExt(App);
  end;

  if (r1 = False) or (r2 = False) or (r3 = False) or (r4 = False) or (r5 = False) then
    Result := False
  else
    Result := True;
end;

procedure ClearIconCache;
begin
{$IFDEF Windows}
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
{$ENDIF}
end;

{ TFileAssociation }

procedure TFileAssociation.SetFAppDescription(AValue: string);
begin
  if FAppDescription = AValue then
    Exit;
  FAppDescription := AValue;
end;

procedure TFileAssociation.SetFAddAppToDefaultProgams(AValue: Boolean);
begin
  if FAddAppToDefaultPrograms = AValue then
    Exit;
  FAddAppToDefaultPrograms := AValue;
end;

procedure TFileAssociation.SetFAddExtToDefaultProgams(AValue: Boolean);
begin
  if FAddExtToDefaultPrograms = AValue then
    Exit;
  FAddExtToDefaultPrograms := AValue;
end;

procedure TFileAssociation.SetFAppName(AValue: string);
begin
  if FAppName = AValue then
    Exit;
  FAppName := AValue;
end;

procedure TFileAssociation.SetFAppNameNoSpaces(AValue: string);
begin
  if FAppNameNoSpaces = AValue then
    Exit;
  FAppNameNoSpaces := AValue;
end;

procedure TFileAssociation.SetFCmdData(AValue: string);
begin
  if FCmdData = AValue then
    Exit;
  FCmdData := AValue;
end;

procedure TFileAssociation.SetFCmdIcon(AValue: string);
begin
  if FCmdIcon = AValue then
    Exit;
  FCmdIcon := AValue;
end;

procedure TFileAssociation.SetFCmdName(AValue: string);
begin
  if FCmdName = AValue then
    Exit;
  FCmdName := AValue;
end;

procedure TFileAssociation.SetFCmdNameNoSpaces(AValue: string);
begin
  if FCmdNameNoSpaces = AValue then
    Exit;
  FCmdNameNoSpaces := AValue;
end;

procedure TFileAssociation.SetFExtData(AValue: string);
begin
  if FExtData = AValue then
    Exit;
  FExtData := AValue;
end;

procedure TFileAssociation.SetFExtIcon(AValue: string);
begin
  if FExtIcon = AValue then
    Exit;
  FExtIcon := AValue;
end;

procedure TFileAssociation.SetFExtName(AValue: string);
begin
  if FExtName = AValue then
    Exit;
  FExtName := AValue;
end;

procedure TFileAssociation.SetFExtNameNoSpaces(AValue: string);
begin
  if FExtNameNoSpaces = AValue then
    Exit;
  FExtNameNoSpaces := AValue;
end;

procedure TFileAssociation.SetFRegisterAssociation(AValue: Boolean);
begin
  if FRegisterAssociation = AValue then
    Exit;
  FRegisterAssociation := AValue;
end;

procedure TFileAssociation.SetFRegisterForAllUsers(AValue: Boolean);
begin
  if FRegisterForAllUsers = AValue then
    Exit;
  FRegisterForAllUsers := AValue;
end;

constructor TFileAssociation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRegisterAssociation := True;
  FRegisterForAllUsers := True;
  FAddAppToDefaultPrograms := True;
  FAddExtToDefaultPrograms := True;
end;

destructor TFileAssociation.Destroy;
begin
  inherited Destroy;
end;

function TFileAssociation.Execute: Boolean;
var
  r1, r2, r3, r4, r5: Boolean;
  Root: HKEY;
begin
  if RegisterForAllUsers then
    Root := HKLM
  else
    Root := HKCU;

  r1 := WriteFileAssociationClass(Root, AppNameNoSpaces,
    ExtIcon, ExtName, ExtNameNoSpaces);
  r2 := WriteFileAssociationClassCommand(Root,
    AppNameNoSpaces, ExtNameNoSpaces, CmdName, CmdNameNoSpaces, CmdIcon, CmdData);

  if RegisterAssociation then
    r3 := WriteFileAssociation(Root, AppNameNoSpaces,
      ExtNameNoSpaces, ExtData);

  if Root <> HKCU then
  begin
    if AddAppToDefaultPrograms then
      r4 := WriteDefaultPrograms(AppNameNoSpaces,
        AppName, AppDescription);
    if AddExtToDefaultPrograms then
      r5 := WriteDefaultProgramsAddExt(AppNameNoSpaces,
        ExtData, ExtNameNoSpaces);
  end;

  if (r1 = False) or (r2 = False) or (r3 = False) or (r4 = False) or (r5 = False) then
    Result := False
  else
    Result := True;
end;

procedure TFileAssociation.ClearIconCache;
begin
  ts.Components.FileAssociation.ClearIconCache;
end;

end.
