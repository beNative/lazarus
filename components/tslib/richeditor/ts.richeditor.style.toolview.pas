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

unit ts.RichEditor.Style.ToolView;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,

  HexaColorPicker,

  ts.RichEditor.ToolView.Base, ts.RichEditor.Interfaces;

type
  TStyleToolView = class(TCustomRichEditorToolView)
    cpMain   : THexaColorPicker;
    tcColors : TTabControl;
  private

  public

  end;

implementation

{$R *.lfm}

end.

