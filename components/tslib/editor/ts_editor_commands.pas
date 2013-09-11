unit ts_Editor_Commands;

{$mode delphi}

{ Commands that can be executed on the ActiveView }

//*****************************************************************************

interface

uses
  Classes, SysUtils,

  ts_Editor_Interfaces;

type
  TEditorCommands = class(TComponent)
  private
    function GetManager: IEditorManager;
  protected
    property Manager: IEditorManager
      read GetManager;
  public
    procedure AfterConstruction; override;

    { IEditorCommands }

  end;

//*****************************************************************************

implementation


procedure TEditorCommands.AfterConstruction;
begin
  inherited AfterConstruction;

end;



function TEditorCommands.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;




end.

