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

unit ts_Editor_AlignLinesForm;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, Grids, ActnList, Contnrs,

  VirtualTrees,

  ts_Editor_Interfaces, ts_Editor_Settings_AlignLines,

  ts_Core_TreeViewPresenter,

  sharedlogger;

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
  TfrmAlignLines = class(TForm, IEditorToolView)
    aclMain        : TActionList;
    actExecute     : TAction;
    btnCancel      : TButton;
    btnOK          : TButton;
    gbxOptions     : TCheckGroup;
    gbxInsertSpace : TCheckGroup;
    gbxTokenList   : TGroupBox;
    gbxTokensFound : TGroupBox;
    mmoTokens      : TMemo;
    pnlTokens      : TPanel;
    pnlVST         : TPanel;
    rgpAlignAt     : TRadioGroup;
    rgpSortDirection: TRadioGroup;
    splVertical    : TSplitter;

    procedure actExecuteExecute(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure gbxInsertSpaceItemClick(Sender: TObject; Index: integer);
    procedure gbxOptionsItemClick(Sender: TObject; Index: integer);
    procedure mmoTokensExit(Sender: TObject);
    procedure pnlTokensResize(Sender: TObject);
    procedure rgpSortDirectionClick(Sender: TObject);
  strict private
    FTVP    : TTreeViewPresenter;
    FVST    : TVirtualStringTree;
    FTokens : TObjectList; // list of alignment tokens found in selection

    procedure UpdateTokenList;

  protected
    function GetSettings: TAlignLinesSettings;
    function GetManager: IEditorManager;
    function GetForm: TForm;
    function GetName: string;
    function GetVisible: Boolean;

    { Lets the view respond to changes. }
    procedure UpdateView;
    procedure Execute;
    procedure UpdateActions; override;

    property Visible: Boolean
      read GetVisible write SetVisible;

    property Name: string
      read GetName;

    property Form: TForm
      read GetForm;

    property Manager: IEditorManager
      read GetManager;

    property Settings: TAlignLinesSettings
      read GetSettings;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

//*****************************************************************************

implementation

{$R *.lfm}

uses
  ts_Core_Utils, ts_Core_Helpers,

  ts_Editor_Utils;

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
//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmAlignLines.AfterConstruction;
var
  SL : TStringList;
  S  : string;
begin
  inherited AfterConstruction;
  SetDoubleBuffered(Self);
  FTokens := TObjectList.Create;
  FVST := CreateVST(Self, pnlVST);
  FVST.Font.Name := Manager.Settings.EditorFont.Name;
  FTVP := TTreeViewPresenter.Create(Self);
  with FTVP.ColumnDefinitions.AddColumn('Token') do
  begin
    Alignment := taCenter;
  end;
  SL := TStringList.Create;
  for S in DEFAULT_TOKENS do
    SL.Add(S);
  FTVP.ItemsSource := FTokens;
  FTVP.TreeView    := FVST;
  FTVP.ShowHeader  := False;
  //Settings.Tokens  := SL;
  mmoTokens.Font.Name := Manager.Settings.EditorFont.Name;
  mmoTokens.Lines.Assign(Settings.Tokens);
  SL.Free;
end;

procedure TfrmAlignLines.BeforeDestruction;
begin
  FTokens.Free;
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************
{$endregion}

{$region 'action handlers' /fold}
//*****************************************************************************
// action handlers                                                       BEGIN
//*****************************************************************************

procedure TfrmAlignLines.actExecuteExecute(Sender: TObject);
begin
  Execute;
end;

//*****************************************************************************
// action handlers                                                         END
//*****************************************************************************
{$endregion}

{$region 'event handlers' /fold}
//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

procedure TfrmAlignLines.FormShow(Sender: TObject);
begin
  mmoTokens.Lines.Assign(Settings.Tokens);
  UpdateTokenList;
  FVST.SetFocus;
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

procedure TfrmAlignLines.mmoTokensExit(Sender: TObject);
begin
  Settings.Tokens := mmoTokens.Lines;
end;

procedure TfrmAlignLines.pnlTokensResize(Sender: TObject);
begin
  gbxTokenList.Width := (pnlTokens.ClientWidth + 4) div 2;
end;

procedure TfrmAlignLines.rgpSortDirectionClick(Sender: TObject);
begin
  Settings.SortDirection := TSortDirection((Sender as TRadioGroup).ItemIndex);
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************
{$endregion}

{$region 'property access mehods' /fold}
//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TfrmAlignLines.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TfrmAlignLines.GetForm: TForm;
begin
  Result := Self;
end;

function TfrmAlignLines.GetName: string;
begin
  Result := inherited Name;
end;

function TfrmAlignLines.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

function TfrmAlignLines.GetSettings: TAlignLinesSettings;
begin
  Result := (Manager as IEditorSettings).AlignLinesSettings;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************
{$endregion}

{$region 'protected methods' /fold}
//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TfrmAlignLines.UpdateTokenList;
var
  S : string;
begin
  FTVP.BeginUpdate;
  FTokens.Clear;
  for S in Settings.Tokens do
  begin
    if StrContains(S, Manager.ActiveView.SelText) then
      FTokens.Add(TToken.Create(S));
  end;
  FTVP.EndUpdate;
  FTVP.Refresh;
end;

procedure TfrmAlignLines.UpdateView;
begin
  UpdateTokenList;
  if FTokens.Count > 0 then
    FTVP.CurrentItem := FTokens[0];
end;

procedure TfrmAlignLines.Execute;
var
  T : string;
begin
  if Assigned(FTVP.CurrentItem) then
  begin
    T := TToken(FTVP.CurrentItem).Token;
    Manager.ActiveView.AlignSelection(
      T,
      gbxOptions.Checked[0],          // Remove whitespace
      gbxInsertSpace.Checked[0],      // Before token
      gbxInsertSpace.Checked[1],      // After token
      gbxOptions.Checked[1]           // Align in paragraphs
    );
  end;
  CloseQuery;
end;

procedure TfrmAlignLines.UpdateActions;
begin
  inherited UpdateActions;
  gbxOptions.Checked[0]     := Settings.RemoveWhiteSpace;
  gbxOptions.Checked[1]     := Settings.AlignInParagraphs;
  gbxOptions.Checked[2]     := Settings.SortAfterAlign;
  gbxInsertSpace.Checked[0] := Settings.KeepSpaceBeforeToken;
  gbxInsertSpace.Checked[1] := Settings.KeepSpaceAfterToken;
  rgpSortDirection.ItemIndex := Integer(Settings.SortDirection);
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************
{$endregion}

end.

