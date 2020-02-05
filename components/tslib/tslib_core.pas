{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit tslib_core;

{$warn 5023 off : no warning about unused units}
interface

uses
  ts.Core.Utils, ts.Core.FileAssociations, ts.Core.FormSettings, ts.Core.BRRE, 
  ts.Core.BRREUnicode, ts.Core.CodecUtilsWin32, ts.Core.ColumnDefinitions, 
  ts.Core.ColumnDefinitionsDataTemplate, ts.Core.DataTemplates, 
  ts.Core.DBUtils, ts.Core.DirectoryWatch, ts.Core.EncodingUtils, 
  ts.Core.Helpers, ts.Core.KeyValues, ts.Core.Logger.Channel.IPC, 
  ts.Core.Logger, ts.Core.NativeXml.Debug, ts.Core.NativeXml.ObjectStorage, 
  ts.Core.NativeXml, ts.Core.NativeXml.Streams, ts.Core.NativeXml.StringTable, 
  ts.Core.NativeXml.Win32Compat, ts.Core.SharedLogger, ts.Core.SQLParser, 
  ts.Core.SQLScanner, ts.Core.SQLTree, ts.Core.StringUtils, 
  ts.Core.SystemInfo, ts.Core.TreeViewPresenter, ts.Core.Value, 
  ts.Core.VersionInfo, ts.Core.XMLUtils, ts.Core.Collections, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('tslib_core', @Register);
end.
