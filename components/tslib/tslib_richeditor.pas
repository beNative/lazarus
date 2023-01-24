{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit tslib_richeditor;

{$warn 5023 off : no warning about unused units}
interface

uses
  ts.RichEditor.Events, ts.RichEditor.Factories.Menus, 
  ts.RichEditor.Factories, ts.RichEditor.Factories.Toolbars, 
  ts.RichEditor.Interfaces, ts.RichEditor.Manager, ts.RichEditor.Settings, 
  ts.RichEditor.Types, ts.RichEditor.View.KMemo, ts.RichEditor.ToolView.Base, 
  ts.RichEditor.Test.Toolview, ts.richeditor.toolviews, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('tslib_richeditor', @Register);
end.
