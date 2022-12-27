{
  Copyright (C) 2013-2023 Tim Sinaeve tim.sinaeve@gmail.com

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

