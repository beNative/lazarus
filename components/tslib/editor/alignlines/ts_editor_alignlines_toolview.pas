{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts_Editor_AlignLines_ToolView;

{ Tool for aligning selected lines to a common specified token. }

{$MODE Delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ActnList, Contnrs,

  VirtualTrees,

  ts.Components.Multipanel, ts.Core.TreeViewPresenter,

  ts_Editor_ToolView_Base, ts.Editor.Interfaces, ts.Editor.Types,
  ts.Editor.AlignLines.Settings,

  ts.Core.SharedLogger;

const
  DEFAULT_TOKENS: array[0..28] of string = (
    ':=',
    '=',
    '//',
    '{',
    '(*',
    ':',
    ',',
    ';',
    '@',
    '*',
    '|',
    '--',
    '<<',
    '>>',
    '*)',
    '}',
    '-',
    '.',
    '%',
    '''',
    '"',
    '#',
    '+',
    'read',
    'write',
    'in',
    'as',
    'and',
    'or'
  );

type

  { TfrmAlignLines }

  TfrmAlignLines = class(TCustomEditorToolView, IEditorToolView)
    aclMain          : TActionList;
    actExecute       : TAction;
    btnOK            : TButton;
    gbxInsertSpace   : TCheckGroup;
    gbxOptions       : TCheckGroup;
    gbxTokenList     : TGroupBox;
    gbxTokensFound   : TGroupBox;
    mmoTokens        : TMemo;
    pnlTokens        : TOMultiPanel;
    pnlBottom        : TPanel;
    pnlVST           : TPanel;
    rgpAlignAt       : TRadioGroup;
    rgpSortDirection : TRadioGroup;
    sbrMain          : TScrollBox;
    splVertical      : TSplitter;

    procedure actExecuteExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure FTVPDoubleClick(Sender: TObject);
    procedure gbxInsertSpaceItemClick(Sender: TObject; Index: integer);
    procedure gbxOptionsItemClick(Sender: TObject; Index: integer);
    procedure mmoTokensChange(Sender: TObject);
    procedure pnlTokensResize(Sender: TObject);
    procedure rgpAlignAtClick(Sender: TObject);
    procedure rgpSortDirectionClick(Sender: TObject);

  strict private
    FTVP    : TTreeViewPresenter;
    FVST    : TVirtualStringTree;
    FTokens : TObjectList; // list of alignment tokens found in selection

  strict protected
    function GetSettings: TAlignLinesSettings;

    procedure AssignDefaultTokens;
    procedure UpdateTokenList;

    { Lets the view respond to changes. }
    procedure UpdateView; override;
    procedure Execute;

    property Settings: TAlignLinesSettings
      read GetSettings;

  protected
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.lfm}

uses
  ts.Core.Utils, ts.Core.Helpers,

  ts.Editor.Utils;

resourcestring
  SToken = 'Token';

{$region 'TToken' /fold}
type
  TToken = class(TPersistent)
  private
    FToken: string;

  public
    constructor Create(const AToken: string);

  published
    property Token: string
      read FToken write FToken;
  end;

constructor TToken.Create(const AToken: string);
begin
  inherited Create;
  FToken := AToken;
end;
{$endregion}

{$region 'construction and destruction' /fold}
procedure TfrmAlignLines.AfterConstruction;
begin
  inherited AfterConstruction;
  SetDoubleBuffered(Self);

  FTokens := TObjectList.Create;
  FVST := VST.Create(Self, pnlVST);
  FVST.Font.Name := Manager.Settings.EditorFont.Name;
  FTVP := TTreeViewPresenter.Create(Self);
  with FTVP.ColumnDefinitions.AddColumn(SToken) do
  begin
    Alignment := taCenter;
  end;
  if Settings.Tokens.Count = 0 then
    AssignDefaultTokens;
  FTVP.ItemsSource   := FTokens;
  FTVP.TreeView      := FVST;
  FTVP.ShowHeader    := False;
  FTVP.OnDoubleClick := FTVPDoubleClick;
  mmoTokens.Font.Name := Manager.Settings.EditorFont.Name;
  mmoTokens.Lines.Assign(Settings.Tokens);
  ActiveControl := FVST;
  Width := Settings.Width;
end;

procedure TfrmAlignLines.BeforeDestruction;
begin
  FTokens.Free;
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'action handlers' /fold}
procedure TfrmAlignLines.actExecuteExecute(Sender: TObject);
begin
  Execute;
end;
{$endregion}

{$region 'event handlers' /fold}
procedure TfrmAlignLines.FormShow(Sender: TObject);
begin
  mmoTokens.Lines.Assign(Settings.Tokens);
  UpdateTokenList;
  if View.SelAvail and (FTokens.Count > 0) then
  begin
    FVST.SetFocus;
    FTVP.SelectedItem := FTokens[0];
  end;
end;

procedure TfrmAlignLines.FTVPDoubleClick(Sender: TObject);
begin
  actExecute.Execute;
end;

procedure TfrmAlignLines.FormResize(Sender: TObject);
begin
  Settings.Width := Width;
end;

procedure TfrmAlignLines.gbxInsertSpaceItemClick(Sender: TObject; Index: integer);
var
  B : Boolean;
begin
  B := (Sender as TCheckGroup).Checked[Index];
  case Index of
    0: Settings.KeepSpaceBeforeToken := B;
    1: Settings.KeepSpaceAfterToken  := B;
  end;
end;

procedure TfrmAlignLines.gbxOptionsItemClick(Sender: TObject; Index: integer);
var
  B : Boolean;
begin
  B := (Sender as TCheckGroup).Checked[Index];
  case Index of
    0: Settings.RemoveWhiteSpace  := B;
    1: Settings.AlignInParagraphs := B;
    2: Settings.SortAfterAlign    := B;
  end;
end;

procedure TfrmAlignLines.mmoTokensChange(Sender: TObject);
begin
  Settings.Tokens := mmoTokens.Lines;
  UpdateView;
end;

procedure TfrmAlignLines.pnlTokensResize(Sender: TObject);
begin
  gbxTokenList.Width := (pnlTokens.ClientWidth + 4) div 2;
end;

procedure TfrmAlignLines.rgpAlignAtClick(Sender: TObject);
begin
  Settings.AlignToToken := TAlignToToken((Sender as TRadioGroup).ItemIndex);
end;

procedure TfrmAlignLines.rgpSortDirectionClick(Sender: TObject);
begin
  Settings.SortDirection := TSortDirection((Sender as TRadioGroup).ItemIndex);
end;
{$endregion}

{$region 'property access mehods' /fold}
function TfrmAlignLines.GetSettings: TAlignLinesSettings;
begin
  Result := inherited Settings.ToolSettings.ItemsByClass[TAlignLinesSettings] as TAlignLinesSettings;
end;
{$endregion}

{$region 'protected methods' /fold}
procedure TfrmAlignLines.UpdateTokenList;
var
  S  : string;
  O  : string;
  T  : TToken;
  ST : TToken; // selected token
begin
  FTVP.BeginUpdate;
  ST := nil;
  O  := '';
  T  := nil;
  if Assigned(FTVP.CurrentItem) then
  begin
    O := TToken(FTVP.CurrentItem).Token;
  end;
  FTokens.Clear;
  for S in Settings.Tokens do
  begin
    if StrContains(S, Manager.ActiveView.SelText) then
    begin
      T := TToken.Create(S);
      FTokens.Add(T);
      if S = O then
        ST := T;
    end;
  end;
  FTVP.EndUpdate;
  FTVP.Refresh;
  if Assigned(ST) then
    FTVP.CurrentItem := ST // restore focused item if possible
  else if FTokens.Count > 0 then
  begin
    FTVP.CurrentItem := FTokens[0];
  end;
end;

procedure TfrmAlignLines.AssignDefaultTokens;
var
  S  : string;
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    for S in DEFAULT_TOKENS do
      SL.Add(S);
    Settings.Tokens := SL;
  finally
    SL.Free;
  end;
end;

procedure TfrmAlignLines.UpdateView;
begin
  UpdateTokenList;
end;

procedure TfrmAlignLines.Execute;
var
  T : string;
begin
  if Assigned(FTVP.CurrentItem) then
  begin
    T := TToken(FTVP.CurrentItem).Token;
    Manager.Commands.AlignSelection(
      T,
      gbxOptions.Checked[0],          // Remove whitespace
      gbxInsertSpace.Checked[0],      // Before token
      gbxInsertSpace.Checked[1],      // After token
      gbxOptions.Checked[1]           // Align in paragraphs
    );
    if gbxOptions.Checked[2] then // TODO: sort after align
    begin
      // TODO
    end;
  end;
  View.Activate;
end;

procedure TfrmAlignLines.UpdateActions;
begin
  inherited UpdateActions;
  gbxOptions.Checked[0]      := Settings.RemoveWhiteSpace;
  gbxOptions.Checked[1]      := Settings.AlignInParagraphs;
  gbxOptions.Checked[2]      := Settings.SortAfterAlign;
  gbxInsertSpace.Checked[0]  := Settings.KeepSpaceBeforeToken;
  gbxInsertSpace.Checked[1]  := Settings.KeepSpaceAfterToken;
  rgpSortDirection.ItemIndex := Integer(Settings.SortDirection);
  rgpAlignAt.ItemIndex       := Integer(Settings.AlignToToken);
end;
{$endregion}

end.

