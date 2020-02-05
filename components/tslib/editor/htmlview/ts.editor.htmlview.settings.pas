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

unit ts.Editor.HTMLView.Settings;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils;

const
  DEFAULT_WIDTH = 400;

type
  THTMLViewSettings = class(TComponent)
  private
    FWidth: Integer;
  public
    procedure AfterConstruction; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property Width: Integer
      read FWidth write FWidth default DEFAULT_WIDTH;
  end;

implementation

{$REGION 'construction and destruction'}
procedure THTMLViewSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FWidth := DEFAULT_WIDTH;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure THTMLViewSettings.AssignTo(Dest: TPersistent);
var
  S: THTMLViewSettings;
begin
  if Dest is THTMLViewSettings then
  begin
    S := THTMLViewSettings(Dest);
    S.Width := Width;
  end
  else
    inherited AssignTo(Dest);
end;

procedure THTMLViewSettings.Assign(Source: TPersistent);
var
  S: THTMLViewSettings;
begin
  if Source is THTMLViewSettings then
  begin
    S := THTMLViewSettings(Source);
    Width := S.Width;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

initialization
  RegisterClass(THTMLViewSettings);

end.


