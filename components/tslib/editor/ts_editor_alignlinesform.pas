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

  sharedloggerlcl;

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
    aclMain              : TActionList;
    actExecute           : TAction;
    btnCancel            : TButton;
    btnOK                : TButton;
    chkAlignInParagraphs : TCheckBox;
    chkSortAfterAlign: TCheckBox;
    chkBeforeToken       : TCheckBox;
    chkAfterToken        : TCheckBox;
    chkRemoveWhitespace  : TCheckBox;
    gbxInsertSpace       : TGroupBox;
    gbxOptions           : TGroupBox;
    lblRemoveWhiteSpace  : TLabel;
    lblTokens            : TLabel;
    pnlVST               : TPanel;
    rgpAlignAt           : TRadioGroup;

    procedure actExecuteExecute(Sender: TObject);
    procedure chkAfterTokenClick(Sender: TObject);
    procedure chkAlignInParagraphsClick(Sender: TObject);
    procedure chkRemoveWhitespaceClick(Sender: TObject);
    procedure chkBeforeTokenClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  strict private
    FTVP    : TTreeViewPresenter;
    FVST    : TVirtualStringTree;
    FTokens : TObjectList;

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
  I  : Integer;
  SL : TStringList;
  S  : string;
begin
  inherited AfterConstruction;
  SetDoubleBuffered(Self);

  FTokens := TObjectList.Create;
  FVST := CreateVST(Self, pnlVST);
  FTVP := TTreeViewPresenter.Create(Self);
  with FTVP.ColumnDefinitions.AddColumn('Token') do
  begin
    Alignment := taCenter;
  end;
  SL := TStringList.Create;
  for S in DEFAULT_TOKENS do
    SL.Add(S);
  FTVP.ItemsSource := FTokens;
  FTVP.TreeView := FVST;
  Settings.Tokens := SL;
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
  UpdateTokenList;
  FVST.SetFocus;
end;

procedure TfrmAlignLines.chkAfterTokenClick(Sender: TObject);
begin
  Settings.KeepSpaceAfterToken := (Sender as TCheckBox).Checked;
end;

procedure TfrmAlignLines.chkAlignInParagraphsClick(Sender: TObject);
begin
  Settings.AlignInParagraphs := (Sender as TCheckBox).Checked;
end;

procedure TfrmAlignLines.chkRemoveWhitespaceClick(Sender: TObject);
begin
  Settings.RemoveWhiteSpace := (Sender as TCheckBox).Checked;
end;

procedure TfrmAlignLines.chkBeforeTokenClick(Sender: TObject);
begin
  Settings.KeepSpaceBeforeToken := (Sender as TCheckBox).Checked;
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
  I: Integer;
  S: string;
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
      chkRemoveWhitespace.Checked,
      chkBeforeToken.Checked,
      chkAfterToken.Checked,
      chkAlignInParagraphs.Checked
    );
  end;
  CloseQuery;
end;

procedure TfrmAlignLines.UpdateActions;
begin
  inherited UpdateActions;
  chkAlignInParagraphs.Checked := Settings.AlignInParagraphs;
  chkAfterToken.Checked        := Settings.KeepSpaceAfterToken;
  chkBeforeToken.Checked       := Settings.KeepSpaceBeforeToken;
  chkRemoveWhitespace.Checked  := Settings.RemoveWhiteSpace;
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************
{$endregion}

end.

