unit ts_Editor_SelectionInfoForm;

{$mode delphi}

//*****************************************************************************

interface

uses
  SysUtils, Forms, StdCtrls,

  ts_Editor_Interfaces, Classes;

type
  TfrmSelectionInfo = class(TForm, IEditorToolView)
    btnStoreBlock: TButton;
    btnRestoreBlock: TButton;
    chkLockUpdates: TCheckBox;
    chkExcludeEmptyLines: TCheckBox;
    lblCaretXY: TLabel;
    lblLineCount: TLabel;
    lblLineCountValue: TLabel;
    lblLogicalCaretXY: TLabel;
    lblCaretXYValue: TLabel;
    lblLogicalCaretXYValue: TLabel;
    lblStoredBlockBegin              : TLabel;
    lblBlockBegin: TLabel;
    lblStoredBlockBeginValue         : TLabel;
    lblBlockBeginValue: TLabel;
    lblBlockEnd: TLabel;
    lblBlockEndValue: TLabel;
    lblStoredCaretXY: TLabel;
    lblStoredCaretXYValue            : TLabel;
    lblStoredBlockSelectionMode      : TLabel;
    lblStoredBlockEndValue           : TLabel;
    lblStoredBlockEnd                : TLabel;
    lblStoredBlockLines              : TLabel;
    lblStoredBlockSelectionModeValue : TLabel;
    mmoBlock: TMemo;
    procedure btnRestoreBlockClick(Sender: TObject);
    procedure btnStoreBlockClick(Sender: TObject);
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

//*****************************************************************************

implementation

{$R *.lfm}

uses
  TypInfo,

  SynEditTypes,

  ts_Editor_SelectionInfo;
{
  StoredBlockBegin X Y
  StoredBlockEnd   X Y
  StoredBlockSelectionMode
  StoredBlockLines
}

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

procedure TfrmSelectionInfo.btnStoreBlockClick(Sender: TObject);
begin
  View.StoreBlock(chkLockUpdates.Checked, chkExcludeEmptyLines.Checked);
end;

procedure TfrmSelectionInfo.mmoBlockChange(Sender: TObject);
begin
  View.SelectionInfo.Text := mmoBlock.Text;
end;

procedure TfrmSelectionInfo.btnRestoreBlockClick(Sender: TObject);
begin
  View.RestoreBlock;
end;

procedure TfrmSelectionInfo.UpdateView;
begin
  UpdateDisplay;
end;

procedure TfrmSelectionInfo.UpdateDisplay;
var
  SI : TSelectionInfo;
begin
  SI := View.SelectionInfo;
  lblStoredBlockBeginValue.Caption := Format(
    '(%d, %d)', [SI.BlockBegin.X, SI.BlockBegin.Y]
  );
  lblStoredBlockEndValue.Caption := Format(
    '(%d, %d)', [SI.BlockEnd.X, SI.BlockEnd.Y]
  );
  lblStoredCaretXYValue.Caption := Format(
    '(%d, %d)', [SI.CaretXY.X, SI.CaretXY.Y]
  );

  lblStoredBlockSelectionModeValue.Caption :=
    GetEnumName(TypeInfo(TSynSelectionMode), Ord(SI.SelectionMode));

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

  lblLineCountValue.Caption := IntToStr(SI.Lines.Count);
  lblStoredBlockLines.Caption := SI.Text;
  mmoBlock.Lines.Text         := SI.Text;
end;

procedure TfrmSelectionInfo.UpdateActions;
begin
  inherited UpdateActions;
  UpdateDisplay;
end;





end.

