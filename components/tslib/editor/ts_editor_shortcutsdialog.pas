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

unit ts_Editor_ShortcutsDialog;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls,

  RichMemo;

type
  TfrmShortcutsDialog = class(TForm)
    mmoMain: TRichMemo;
    pnlMain: TPanel;
    procedure FormShow(Sender: TObject);
    procedure mmoMainClick(Sender: TObject);
    procedure mmoMainEnterAbort(Sender: TObject);
    procedure mmoMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FText : string;
    function GetRichText: string;
    procedure SetRichText(const AValue: string);
    { private declarations }
  public
    procedure AfterConstruction; override;

    property RichText: string
      read GetRichText write SetRichText;
  end;

function ShortCuts: TForm;

implementation

var
  FShortcuts: TfrmShortcutsDialog;
  FForm       : TForm;


function ShortCuts: TForm;
begin
  if (not Assigned(FShortcuts)) and (not Assigned(FForm)) then
  begin
    FForm := TForm.Create(Application);
    FForm.Position := poScreenCenter;
    FForm.Width := 721;
    FForm.Height := 520;
    FShortcuts := TfrmShortcutsDialog.Create(FForm);
    FShortcuts.BorderStyle := bsNone;
    FShortcuts.Parent := FForm;
    FShortcuts.Align := alClient;
    FShortcuts.Visible := True;
    FShortcuts.Enabled := False;
  end;
  Result := FForm;
end;

procedure TfrmShortcutsDialog.FormShow(Sender: TObject);
begin
  RichText := FText;;
end;
procedure TfrmShortcutsDialog.mmoMainClick(Sender: TObject);
begin
  Abort;
end;

procedure TfrmShortcutsDialog.mmoMainEnterAbort(Sender: TObject);
begin
   Abort;
end;

procedure TfrmShortcutsDialog.mmoMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Abort;
end;

function TfrmShortcutsDialog.GetRichText: string;
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

procedure TfrmShortcutsDialog.SetRichText(const AValue: string);
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

{ TfrmShortcutsDialog }

procedure TfrmShortcutsDialog.AfterConstruction;
var
  SL: TStringList;
begin
  inherited AfterConstruction;
  SL := TStringList.Create ;
  try
    SL.LoadFromFile('tsEdit ShortCuts.rtf');
    FText := SL.Text;
  finally
    FreeAndNil(SL);
  end;
  RichText := FText;
end;

initialization
  //{$I shortcutsdialog.lrs}

end.

