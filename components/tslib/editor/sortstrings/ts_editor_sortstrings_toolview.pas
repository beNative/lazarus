{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts_Editor_SortStrings_ToolView;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ActnList,

  ts.Editor.SortStrings.Settings,

  ts_Editor_ToolView_Base;

type

  { TfrmSortStrings }

  TfrmSortStrings = class(TCustomEditorToolView)
    aclMain          : TActionList;
    actExecute       : TAction;
    btnOK            : TButton;
    gbxOptions       : TCheckGroup;
    pnlBottom        : TPanel;
    rgpSortDirection : TRadioGroup;
    rgpSortScope     : TRadioGroup;

    procedure actExecuteExecute(Sender: TObject);

    procedure gbxOptionsItemClick(Sender: TObject; Index: Integer);
    procedure rgpSortScopeClick(Sender: TObject);
    procedure rgpSortDirectionClick(Sender: TObject);

  private
    function GetSettings: TSortStringsSettings;

  strict protected
    property Settings: TSortStringsSettings
      read GetSettings;

    procedure UpdateActions; override;
  end;

implementation

{$R *.lfm}

uses
  ts.Editor.Utils, ts.Editor.Types;

{$region 'property access mehods' /fold}
function TfrmSortStrings.GetSettings: TSortStringsSettings;
begin
  Result := inherited Settings
    .ToolSettings.ItemsByClass[TSortStringsSettings] as TSortStringsSettings;
end;
{$endregion}

{$region 'action handlers' /fold}
procedure TfrmSortStrings.actExecuteExecute(Sender: TObject);
begin
  Manager.Commands.SortStrings;
end;
{$endregion}

{$region 'event handlers' /fold}
procedure TfrmSortStrings.gbxOptionsItemClick(Sender: TObject; Index: Integer);
var
  B : Boolean;
begin
  B := (Sender as TCheckGroup).Checked[Index];
  case Index of
    0: Settings.CaseSensitive := B;
    1: Settings.IgnoreSpaces  := B;
  end;
end;

procedure TfrmSortStrings.rgpSortScopeClick(Sender: TObject);
begin
  Settings.SortScope := TSortScope((Sender as TRadioGroup).ItemIndex);
end;

procedure TfrmSortStrings.rgpSortDirectionClick(Sender: TObject);
begin
  Settings.SortDirection := TSortDirection((Sender as TRadioGroup).ItemIndex);
end;

procedure TfrmSortStrings.UpdateActions;
begin
  inherited UpdateActions;
  gbxOptions.Checked[0]      := Settings.CaseSensitive;
  gbxOptions.Checked[1]      := Settings.IgnoreSpaces;
  rgpSortDirection.ItemIndex := Integer(Settings.SortDirection);
  rgpSortScope.ItemIndex     := Integer(Settings.SortScope);
end;

{$endregion}

end.

