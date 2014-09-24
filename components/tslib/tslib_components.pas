{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit tslib_components;

interface

uses
  ts.Components.DBGridView, ts.Components.Docking.OptionsDialog, 
  ts.Components.Docking, ts.Components.Docking.Resources, 
  ts.Components.Docking.Storage, ts.Components.ExportRTF, 
  ts.Components.FileAssociation, ts.Components.GridView, 
  ts.Components.Inspector, ts.Components.MultiPanel, ts.Components.SynMiniMap, 
  ts.Components.UNIHighlighter, ts.Components.UniqueInstance, 
  ts.Components.VirtualDBTreeEx, ts.Components.VirtualPages, 
  ts.Components.XMLTree.Editors, ts.Components.XMLTree.NodeAttributes, 
  ts.Components.XMLTree, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('tslib_components', @Register);
end.
