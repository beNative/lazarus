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
  Classes, SysUtils, FileUtil, MenuButton, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Spin, Buttons, Grids, ActnList,

  ts_Editor_Interfaces, ts_Editor_Settings_AlignLines;

const
  DEFAULT_TOKENS: array[0..13] of string = (
    ':=',
    '=',
    '//',
    '{',
    '(*',
    '''',
    ':',
    '+',
    'read',
    'write',
    'in ''',
    'as',
    'and',
    'or'
  );

type
  TfrmAlignLines = class(TForm, IEditorToolView)
    aclMain: TActionList;
    actExecute: TAction;
    btnCancel: TButton;
    btnOK: TButton;
    chkAlignInParagraphs: TCheckBox;
    chkBeforeToken: TCheckBox;
    chkAfterToken: TCheckBox;
    chkRemoveWhitespace: TCheckBox;
    gbxInsertSpace: TGroupBox;
    gbxOptions: TGroupBox;
    lblRemoveWhiteSpace: TLabel;
    lblTokens: TLabel;
    lstTokens: TListBox;
    rgpAlignAt: TRadioGroup;
    procedure actExecuteExecute(Sender: TObject);
    procedure chkAfterTokenClick(Sender: TObject);
    procedure chkAlignInParagraphsClick(Sender: TObject);
    procedure chkRemoveWhitespaceClick(Sender: TObject);
    procedure chkBeforeTokenClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetSettings: TAlignLinesSettings;
  strict private
    FTokens: TStrings;

    function GetManager: IEditorManager;
    function GetForm: TForm;
    function GetName: string;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    { Lets the view respond to changes. }
    procedure UpdateView;

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

    procedure Execute;
  protected
    procedure UpdateActions; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

//*****************************************************************************

implementation

{$R *.lfm}

uses
  ts_Core_Utils,

  ts_Editor_Utils;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmAlignLines.AfterConstruction;
var
  I: Integer;
begin
  inherited AfterConstruction;
  SetDoubleBuffered(Self);
  FTokens := TStringList.Create;
  for I := Low(DEFAULT_TOKENS) to High(DEFAULT_TOKENS) do
    FTokens.Add(DEFAULT_TOKENS[I]);
end;

procedure TfrmAlignLines.BeforeDestruction;
begin
  FTokens.Free;
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// action handlers                                                       BEGIN
//*****************************************************************************

procedure TfrmAlignLines.actExecuteExecute(Sender: TObject);
begin
  Execute;
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
// action handlers                                                         END
//*****************************************************************************

//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

procedure TfrmAlignLines.FormShow(Sender: TObject);
begin
  AddStringsPresentInString(FTokens, lstTokens.Items, Manager.ActiveView.SelText);
  lstTokens.SetFocus;
end;

function TfrmAlignLines.GetSettings: TAlignLinesSettings;
begin
  Result := (Manager as IEditorSettings).AlignLinesSettings;
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************

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

procedure TfrmAlignLines.SetVisible(AValue: Boolean);
begin
  inherited SetVisible(AValue);
  if AValue then
    AddStringsPresentInString(FTokens, lstTokens.Items, Manager.ActiveView.SelText)
  else
    lstTokens.Clear;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// private methods                                                       BEGIN
//*****************************************************************************

procedure TfrmAlignLines.UpdateView;
begin
  AddStringsPresentInString(FTokens, lstTokens.Items, Manager.ActiveView.SelText);
  if lstTokens.Items.Count > 0 then
    lstTokens.ItemIndex := 0;
end;

procedure TfrmAlignLines.Execute;
var
  S : string;
  T : string;
begin
  if lstTokens.ItemIndex >= 0 then
  begin
    T := lstTokens.Items[lstTokens.ItemIndex];
    S := Manager.ActiveView.SelText;
    Manager.ActiveView.SelText := AlignLines(
      S,
      T,
      chkRemoveWhitespace.Checked,
      chkBeforeToken.Checked,
      chkAfterToken.Checked,
      chkAlignInParagraphs.Checked
    );
  end;
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
// private methods                                                         END
//*****************************************************************************

end.

