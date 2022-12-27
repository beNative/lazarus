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

unit ts.Editor.Shortcuts.ToolView;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls,

  RichMemo;

type
  TfrmShortcuts = class(TForm)
    mmoMain : TRichMemo;
    pnlMain : TPanel;

    procedure FormShow(Sender: TObject);
    procedure mmoMainClick(Sender: TObject);
    procedure mmoMainEnterAbort(Sender: TObject);
    procedure mmoMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  private
    FText : string;

    function GetRichText: string;
    procedure SetRichText(const AValue: string);

  public
    procedure AfterConstruction; override;

    property RichText: string
      read GetRichText write SetRichText;
  end;

function ShortCuts: TForm;

implementation

{$R *.lfm}

var
  FShortcuts  : TfrmShortcuts;
  FForm       : TForm;

function ShortCuts: TForm;
begin
  if (not Assigned(FShortcuts)) and (not Assigned(FForm)) then
  begin
    FForm := TForm.Create(Application);
    FForm.Position := poScreenCenter;
    FForm.Width := 721;
    FForm.Height := 520;
    FShortcuts := TfrmShortcuts.Create(FForm);
    FShortcuts.BorderStyle := bsNone;
    FShortcuts.Parent := FForm;
    FShortcuts.Align := alClient;
    FShortcuts.Visible := True;
    FShortcuts.Enabled := False;
  end;
  Result := FForm;
end;

{$REGION 'construction and destruction'}
procedure TfrmShortcuts.AfterConstruction;
var
  SL: TStringList;
begin
  inherited AfterConstruction;
  SL := TStringList.Create ;
  try
    SL.LoadFromFile('ShortCuts.rtf');
    FText := SL.Text;
  finally
    FreeAndNil(SL);
  end;
  RichText := FText;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TfrmShortcuts.GetRichText: string;
var
  SS : TStringStream;
  S  : string;
begin
  S := '';
  SS := TStringStream.Create(S);
  try
    mmoMain.SaveRichText(SS);
    Result := S;
  finally
    FreeAndNil(SS);
  end;
end;

procedure TfrmShortcuts.SetRichText(const AValue: string);
var
  SS : TStringStream;
  S  : string;
begin
  S := '';
  SS := TStringStream.Create(S);
  try
    SS.WriteString(AValue);
    SS.Position := 0;
    mmoMain.LoadRichText(SS);
  finally
    FreeAndNil(SS);
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmShortcuts.FormShow(Sender: TObject);
begin
  RichText := FText;
end;
procedure TfrmShortcuts.mmoMainClick(Sender: TObject);
begin
  Abort;
end;

procedure TfrmShortcuts.mmoMainEnterAbort(Sender: TObject);
begin
   Abort;
end;

procedure TfrmShortcuts.mmoMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Abort;
end;
{$ENDREGION}

end.

