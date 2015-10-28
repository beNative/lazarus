unit TableTool_Forms_Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, ActnList, Menus, StdCtrls, Clipbrd,

  RTTICtrls, RTTIGrids,

  OMultiPanel,

  ts.Editor.Interfaces;

{
   enumeratecells
   enumerate cols
   enumerate cells in col/row

   multireplace
}

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actClearSelection: TAction;
    aclMain: TActionList;
    actAutoSizeColumns: TAction;
    actCopyAsWiki: TAction;
    actCopyAsTextTable: TAction;
    actCopyAsCSV: TAction;
    actCopyAsTabDelimited: TAction;
    actDeleteSelectedCols: TAction;
    actDeleteSelectedRows: TAction;
    actSelectAll: TAction;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    chkFirstRowAsHeader: TCheckBox;
    chkQuoteValues: TCheckBox;
    Edit1: TEdit;
    edtMargin: TLabeledEdit;
    edtMinColumnWidth: TLabeledEdit;
    grpLayoutOptions: TGroupBox;
    grpCharSpacing: TGroupBox;
    lblRows: TLabel;
    lblColumnCount: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mniClearGrid: TMenuItem;
    mniCopyAsWiki: TMenuItem;
    mniAutoSizeColumns: TMenuItem;
    mniCopyAsCSV: TMenuItem;
    mniCopyAsTabDelimited: TMenuItem;
    MenuItem6: TMenuItem;
    mniSelectAll: TMenuItem;
    OMultiPanel1: TOMultiPanel;
    pnlEditor: TPanel;
    pnlBottom: TPanel;
    pnlMain: TPanel;
    ppmMain: TPopupMenu;
    StringGrid: TStringGrid;
    edtRowCount: TTISpinEdit;
    edtColumnCount: TTISpinEdit;
    trbRows: TTITrackBar;
    trbFixedCols: TTITrackBar;
    procedure actAutoSizeColumnsExecute(Sender: TObject);
    procedure actCopyAsCSVExecute(Sender: TObject);
    procedure actCopyAsTabDelimitedExecute(Sender: TObject);
    procedure actCopyAsTextTableExecute(Sender: TObject);
    procedure actCopyAsWikiExecute(Sender: TObject);
    procedure actClearSelectionExecute(Sender: TObject);
    procedure actDeleteSelectedColsExecute(Sender: TObject);
    procedure actDeleteSelectedRowsExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);

  private
    FEditor : IEditorView;

  public
    procedure AfterConstruction; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  ts.Editor.Factories,

  TableTool.Helpers;

{ TfrmMain }

procedure TfrmMain.AfterConstruction;
begin
  inherited AfterConstruction;
  FEditor := TEditorFactories.CreateView(pnlEditor);
end;

procedure TfrmMain.actClearSelectionExecute(Sender: TObject);
begin
  ClearSelection(StringGrid);
end;

procedure TfrmMain.actDeleteSelectedColsExecute(Sender: TObject);
begin
  DeleteSelectedCols(StringGrid);
end;

procedure TfrmMain.actDeleteSelectedRowsExecute(Sender: TObject);
begin
  DeleteSelectedRows(StringGrid);
end;

procedure TfrmMain.actSelectAllExecute(Sender: TObject);
begin
  SelectAll(StringGrid);
end;

procedure TfrmMain.actAutoSizeColumnsExecute(Sender: TObject);
begin
  StringGrid.AutoSizeColumns;
end;

procedure TfrmMain.actCopyAsCSVExecute(Sender: TObject);
begin
  FEditor.Text := SelectionToDelimitedTable(StringGrid, ',', chkFirstRowAsHeader.Checked);
  Clipboard.AsText := FEditor.Text;
end;

procedure TfrmMain.actCopyAsTabDelimitedExecute(Sender: TObject);
begin
  FEditor.Text := SelectionToDelimitedTable(StringGrid, #9, chkFirstRowAsHeader.Checked);
  Clipboard.AsText := FEditor.Text;
end;

procedure TfrmMain.actCopyAsTextTableExecute(Sender: TObject);
begin
  FEditor.Text := SelectionToTextTable2(
    StringGrid,
    chkFirstRowAsHeader.Checked,
    StrToIntDef(edtMargin.Text, 0),
    StrToIntDef(edtMinColumnWidth.Text, 0)
  );
  Clipboard.AsText := FEditor.Text;
end;

procedure TfrmMain.actCopyAsWikiExecute(Sender: TObject);
begin
  FEditor.Text := SelectionToWikiTable(StringGrid, chkFirstRowAsHeader.Checked);
  Clipboard.AsText := FEditor.Text;
end;

end.

