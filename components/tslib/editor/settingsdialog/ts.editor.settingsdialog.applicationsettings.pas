{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.SettingsDialog.ApplicationSettings;

{$MODE DELPHI}

interface

uses
  RTTICtrls,

  ts.Editor.SettingsDialog.Base;

type
  TfrmApplicationSettings = class(TCustomSettingsDialog)
    chkDimInactiveView : TTICheckBox;
    chkSingleInstance  : TTICheckBox;
    chkCloseWithESC    : TTICheckBox;
    chkDebugMode       : TTICheckBox;

  public
    procedure AfterConstruction; override;
  end;

implementation

{$R *.lfm}

{$REGION 'construction and destruction'}
procedure TfrmApplicationSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  LinkProperty(Settings, chkSingleInstance.Link);
  LinkProperty(Settings, chkDebugMode.Link);
  LinkProperty(Settings, chkCloseWithESC.Link);
  LinkProperty(Settings, chkDimInactiveView.Link);
end;
{$ENDREGION}

end.

