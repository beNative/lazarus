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

unit ts_Editor_ToolView_Filter;

{$MODE Delphi}

{
  This is an attempt to make a reusable filter view using a virtual treeview and
  a presenter.
}


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls,

  VirtualTrees,

  ts.Core.TreeviewPresenter, ts.Core.ColumnDefinitions,

  ts_Editor_ToolView_Base;

type

  { TfrmFilter }

  TfrmFilter = class(TCustomEditorToolView)
    edtFilter: TEdit;
    pnlView: TPanel;
    pnlHeader: TPanel;
    pnlMain: TPanel;
    sbrMain: TStatusBar;
    procedure edtFilterChange(Sender: TObject);
    procedure edtFilterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtFilterKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FVSTKeyPress(Sender: TObject; var Key: char);
  strict private
    FVST : TVirtualStringTree;
    FTVP : TTreeViewPresenter;

    FVKPressed : Boolean;
    FTextStyle : TTextStyle;

    function GetColumnDefinitions: TColumnDefinitions;
    function GetFilter: string;
    procedure SetFilter(AValue: string);

    function IsMatch(const AString : string): Boolean; overload; inline;
    function IsMatch(
      const AString : string;
        var AMatch  : string;
        var APos    : Integer
    ): Boolean; overload; inline;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Filter: string
      read GetFilter write SetFilter;

    property ColumnDefinitions: TColumnDefinitions
      read GetColumnDefinitions;
  end;

implementation

{$R *.lfm}

uses
  LMessages,

  Windows,

  ts.Editor.Utils, ts.Core.Helpers;

type
  TVKSet = set of Byte;

var
  VK_EDIT_KEYS : TVKSet = [
    VK_DELETE,
    VK_BACK,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END,
    VK_SHIFT,
    VK_CONTROL,
    VK_SPACE,
    VK_0..VK_Z,
    VK_OEM_1..VK_OEM_102,
    VK_MULTIPLY..VK_DIVIDE
  ];

  VK_CTRL_EDIT_KEYS : TVKSet = [
    VK_INSERT,
    VK_DELETE,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END,
    VK_C,
    VK_X,
    VK_V,
    VK_Z
  ];

  VK_SHIFT_EDIT_KEYS : TVKSet = [
    VK_INSERT,
    VK_DELETE,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END
  ];

{ TfrmFilter }

{$region 'construction and destruction' /fold}
procedure TfrmFilter.AfterConstruction;
begin
  inherited AfterConstruction;
  FVST := VST.Create(Self, pnlView);
  FVST.OnKeyPress := FVSTKeyPress;
  FTVP := CreateTVP(Self, FVST);

  FTextStyle.SingleLine := True;
  FTextStyle.Opaque     := False;
  FTextStyle.ExpandTabs := False;
  FTextStyle.Wordbreak  := False;
  FTextStyle.ShowPrefix := True;
  FTextStyle.Clipping   := False;
  FTextStyle.SystemFont := False;
  FTextStyle.Alignment  := taLeftJustify;
  FTextStyle.Layout     := tlCenter;
end;

procedure TfrmFilter.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'event handlers' /fold}
procedure TfrmFilter.edtFilterChange(Sender: TObject);
begin
  FTVP.ApplyFilter;
end;

procedure TfrmFilter.edtFilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  A : Boolean;
  B : Boolean;
  C : Boolean;
  D : Boolean;
begin
  A := (ssAlt in Shift) or (ssShift in Shift);
  B := (Key in VK_EDIT_KEYS) and (Shift = []);
  C := (Key in VK_CTRL_EDIT_KEYS) and (Shift = [ssCtrl]);
  D := (Key in VK_SHIFT_EDIT_KEYS) and (Shift = [ssShift]);
  if not (A or B or C or D) then
  begin
    FVKPressed := True;
    Key := 0;
  end;
end;

procedure TfrmFilter.edtFilterKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FVKPressed and FVST.Enabled then
  begin
{$IFDEF Windows}
    PostMessage(FVST.Handle, WM_KEYDOWN, Key, 0);
{$ENDIF}
    if Visible and FVST.CanFocus then
      FVST.SetFocus;
  end;
  FVKPressed := False;
end;

procedure TfrmFilter.FVSTKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = VK_RETURN then
  begin
    Close;
  end
  else if Ord(Key) = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Close;
  end
  else if not edtFilter.Focused then
  begin
    edtFilter.SetFocus;
    PostMessage(edtFilter.Handle, LM_CHAR, Ord(Key), 0);
    edtFilter.SelStart := Length(Filter);
    // required to prevent the invocation of accelerator keys!
    Key := #0;
  end;
end;

{$endregion}

{$region 'property access mehods' /fold}
function TfrmFilter.GetFilter: string;
begin
  Result := edtFilter.Text;
end;

procedure TfrmFilter.SetFilter(AValue: string);
begin
  if AValue <> Filter then
  begin
    edtFilter.Text := AValue;
  end;
end;

function TfrmFilter.GetColumnDefinitions: TColumnDefinitions;
begin
  Result := FTVP.ColumnDefinitions;
end;
{$endregion}

{$region 'private methods' /fold}
function TfrmFilter.IsMatch(const AString: string): Boolean;
begin
  if Filter = '' then
    Result := True
  else
    Result := StrPos(Filter, AString, False) > 0;
end;

function TfrmFilter.IsMatch(const AString: string; var AMatch: string;
  var APos: Integer): Boolean;
var
  S : string;
begin
  APos   := 0;
  AMatch := '';
  Result := False;
  if Filter <> '' then
  begin
    // remove accelerator token
    S := StringReplace(AString, '&', '', [rfReplaceAll]);
    APos   := StrPos(Filter, S, False);
    AMatch := System.Copy(S, APos, Length(Filter));
    Result := APos > 0;
  end;
end;
{$endregion}

end.

