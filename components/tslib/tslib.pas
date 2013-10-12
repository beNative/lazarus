{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit tslib;

interface

uses
  ts.Components.MultiPanel, tsLib.Registration, ts.Components.GridView, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('tsLib.Registration', @tsLib.Registration.Register);
end;

initialization
  RegisterPackage('tslib', @Register);
end.
