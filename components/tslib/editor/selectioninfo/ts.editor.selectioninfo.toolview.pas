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

unit ts.Editor.SelectionInfo.ToolView;

{$MODE DELPHI}

interface

uses
  SysUtils, Forms, StdCtrls,

  ts.Editor.Interfaces;

type
  TfrmSelectionInfo = class(TForm, IEditorToolView)
    {$REGION 'designer controls'}
    btnStore                         : TButton;
    btnRestore                       : TButton;
    chkLockUpdates                   : TCheckBox;
    chkExcludeEmptyLines             : TCheckBox;
    lblCaretXY                       : TLabel;
    lblLineCount                     : TLabel;
    lblLineCountValue                : TLabel;
    lblLogicalCaretXY                : TLabel;
    lblCaretXYValue                  : TLabel;
    lblLogicalCaretXYValue           : TLabel;
    lblStoredBlockBegin              : TLabel;
    lblBlockBegin                    : TLabel;
    lblStoredBlockBeginValue         : TLabel;
    lblBlockBeginValue               : TLabel;
    lblBlockEnd                      : TLabel;
    lblBlockEndValue                 : TLabel;
    lblStoredCaretXY                 : TLabel;
    lblStoredCaretXYValue            : TLabel;
    lblStoredBlockSelectionMode      : TLabel;
    lblStoredBlockEndValue           : TLabel;
    lblStoredBlockEnd                : TLabel;
    lblStoredBlockLines              : TLabel;
    lblStoredBlockSelectionModeValue : TLabel;
    mmoBlock                         : TMemo;
    {$ENDREGION}

    procedure btnRestoreClick(Sender: TObject);
    procedure btnStoreClick(Sender: TObject);
    procedure mmoBlockChange(Sender: TObject);

  private
    function GetView: IEditorView;

  protected
    function GetForm: TForm;
    function GetName: string;
    function GetVisible: Boolean;
    { Lets the view respond to changes. }
    procedure UpdateView;

    property View: IEditorView
      read GetView;

    procedure UpdateDisplay;
    //procedure Refresh; TODO: refresh all items
    procedure UpdateActions; override;

  end;

implementation

{$R *.lfm}

uses
  TypInfo,

  SynEditTypes;
{
  StoredBlockBegin X Y
  StoredBlockEnd   X Y
  StoredBlockSelectionMode
  StoredBlockLines
}

{$REGION 'property access mehods'}
function TfrmSelectionInfo.GetView: IEditorView;
begin
  Result := Owner as IEditorView;
end;

function TfrmSelectionInfo.GetForm: TForm;
begin
  Result := Self;
end;

function TfrmSelectionInfo.GetName: string;
begin
  Result := Name;
end;

function TfrmSelectionInfo.GetVisible: Boolean;
begin
  Result := Visible;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmSelectionInfo.btnStoreClick(Sender: TObject);
begin
  View.Selection.Store(chkLockUpdates.Checked, chkExcludeEmptyLines.Checked);
end;

procedure TfrmSelectionInfo.mmoBlockChange(Sender: TObject);
begin
  View.Selection.Text := mmoBlock.Text;
end;

procedure TfrmSelectionInfo.btnRestoreClick(Sender: TObject);
begin
  View.Selection.Restore;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmSelectionInfo.UpdateView;
begin
  UpdateDisplay;
end;

procedure TfrmSelectionInfo.UpdateDisplay;
var
  ES : IEditorSelection;
begin
  ES := View.Selection;
  lblStoredBlockBeginValue.Caption := Format(
    '(%d, %d)', [ES.BlockBegin.X, ES.BlockBegin.Y]
  );
  lblStoredBlockEndValue.Caption := Format(
    '(%d, %d)', [ES.BlockEnd.X, ES.BlockEnd.Y]
  );
  lblStoredCaretXYValue.Caption := Format(
    '(%d, %d)', [ES.CaretXY.X, ES.CaretXY.Y]
  );

  lblStoredBlockSelectionModeValue.Caption :=
    GetEnumName(TypeInfo(TSynSelectionMode), Ord(ES.SelectionMode));

    lblBlockBeginValue.Caption := Format(
      '(%d, %d)', [View.BlockBegin.X, View.BlockBegin.Y]
    );
    lblBlockEndValue.Caption := Format(
      '(%d, %d)', [View.BlockEnd.X, View.BlockEnd.Y]
    );

    lblCaretXYValue.Caption := Format(
      '(%d, %d)', [View.CaretX, View.CaretY]
    );
    lblLogicalCaretXYValue.Caption := Format(
      '(%d, %d)', [View.LogicalCaretXY.X, View.LogicalCaretXY.Y]
    );

    lblLineCountValue.Caption := IntToStr(ES.Lines.Count);
    lblStoredBlockLines.Caption := ES.Text;
    mmoBlock.Lines.Text         := ES.Text;
  end;

  procedure TfrmSelectionInfo.UpdateActions;
  begin
    inherited UpdateActions;
    UpdateDisplay;
  end;
  {$ENDREGION}

end.

