unit ts_Editor_ToolView_ScriptEditor;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,

  ts_Editor_ToolView_Base, ts.Editor.Interfaces;

type

  { TfrmScriptEditor }

  TfrmScriptEditor = class(TCustomEditorToolView, IEditorToolView)
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pnlBottom: TPanel;
    pnlMain: TPanel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FEditor  : IEditorView;
    FManager : IEditorManager;
  public
    { public declarations }
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{$R *.lfm}

uses
  ts.Editor.Helpers;

{ TfrmScriptEditor }

{$region 'construction and destruction' /fold}
procedure TfrmScriptEditor.AfterConstruction;
begin
  inherited AfterConstruction;
  //FEditor := CreateEditorView(pnlLeft, 'ScriptEditor', '', 'PAS');
  FManager := CreateEditorManager(Self);
end;

procedure TfrmScriptEditor.BeforeDestruction;
begin
  FEditor := nil;
  FManager := nil;
  inherited BeforeDestruction;
end;
{$endregion}

procedure TfrmScriptEditor.FormShow(Sender: TObject);
begin
  if not Assigned(FEditor) then
    FEditor := CreateEditorView(pnlLeft, FManager, 'ScriptEditor', '', 'PAS');

  if FileExists('notepas.dws') then
    FEditor.LoadFromFile('notepas.dws');
end;

end.

