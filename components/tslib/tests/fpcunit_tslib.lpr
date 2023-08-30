program fpcunit_tslib;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, Test.Core.Logger, tslib_core, Test.Data,
  Test.Utils;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

