{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit VirtualDBTreeEx_laz; 

interface

uses
  VirtualDBTreeEx, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('VirtualDBTreeEx', @VirtualDBTreeEx.Register); 
end; 

initialization
  RegisterPackage('VirtualDBTreeEx_laz', @Register); 
end.
