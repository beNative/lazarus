{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit tslib;

interface

uses
  ts.Components.DBGridView, ts.Components.Docking.OptionsDialog, 
  ts.Components.Docking, ts.Components.Docking.Resources, 
  ts.Components.Docking.Storage, ts.Components.ExportRTF, 
  ts.Components.FileAssociation, ts.Components.GridView, 
  ts.Components.Inspector, ts.Components.MultiPanel, ts.Components.SynMiniMap, 
  ts.Components.UNIHighlighter, ts.Components.UniqueInstance, 
  ts.Components.VirtualDBTreeEx, ts.Components.XMLTree.Editors, 
  ts.Components.XMLTree.NodeAttributes, ts.Components.XMLTree, ts.Collections, 
  ts.Core.BRRE, ts.Core.BRREUnicode, ts.Core.CodecUtilsWin32, 
  ts.Core.Collections, ts.Core.ColumnDefinitions, 
  ts.Core.ColumnDefinitionsDataTemplate, ts.Core.DataTemplates, 
  ts.Core.DBUtils, ts.Core.DirectoryWatch, ts.Core.EncodingUtils, 
  ts.Core.Events, ts.Core.FileAssociations, ts.Core.FormSettings, 
  ts.Core.Generics.Collections, ts.Core.Generics.Defaults, 
  ts.Core.Generics.Hashes, ts.Core.Generics.Helpers, ts.Core.HashStrings, 
  ts.Core.Helpers, ts.Core.KeyValues, ts.Core.Logger.Channel.IPC, 
  ts.Core.Logger.Channel.LogFile, ts.Core.Logger, ts.Core.NativeXml.Debug, 
  ts.Core.NativeXml.ObjectStorage, ts.Core.NativeXml, 
  ts.Core.NativeXml.Streams, ts.Core.NativeXml.StringTable, 
  ts.Core.NativeXml.Win32Compat, ts.Core.SharedLogger, ts.Core.SQLParser, 
  ts.Core.SQLScanner, ts.Core.SQLTree, ts.Core.StringUtils, 
  ts.Core.TreeViewPresenter, ts.Core.Utils, ts.Core.Value, 
  ts.Core.VersionInfo, ts.Core.XMLUtils, ts_Core_ComponentInspector, 
  SetupFiltersDialog, ts.Editor.CodeFormatters, ts.Editor.CodeFormatters.SQL, 
  ts.Editor.CodeTags, ts.Editor.Commands, ts.Editor.CommentStripper, 
  ts.Editor.Events, ts.Editor.Helpers, ts.Editor.HighlighterAttributes, 
  ts.Editor.Highlighters, ts.Editor.Interfaces, ts.Editor.Selection, 
  ts.Editor.ToolView.Manager, ts.Editor.Types, ts.Editor.Utils, 
  ts_Editor_AboutDialog, ts_Editor_Manager, ts_Editor_Resources, 
  ts_Editor_View, ts.Editor.ActionList.Templates, 
  ts_Editor_ActionList_ToolView, ts.Editor.AlignLines.Settings, 
  ts_Editor_AlignLines_ToolView, ts_Editor_CharacterMap_Toolview, 
  ts.Editor.CodeFilter.Data, ts.Editor.CodeFilter.Settings, 
  ts_Editor_CodeFilter_Toolview, ts.Editor.CodeShaper.Settings, 
  ts_Editor_CodeShaper_ToolView, ts_Editor_Colors_ToolView, 
  ts_Editor_Filter_ToolView, ts.Editor.HexEditor.Settings, 
  ts_Editor_HexEditor_ToolView, ts.Editor.HTMLView.Settings, 
  ts_Editor_HTMLView_ToolView, ts.Editor.MiniMap.Settings, 
  ts_Editor_MiniMap_ToolView, ts_Editor_Preview_ToolView, 
  ts_Editor_ScriptEditor_ToolView, ts.Editor.Search.Data, 
  ts.Editor.Search.Engine, ts.Editor.Search.Engine.Settings, 
  ts.Editor.Search.Templates, ts_Editor_Search_Toolview, 
  ts_Editor_SelectionInfo_ToolView, ts.Editor.Settings, 
  ts_Editor_SettingsDialog, ts_Editor_SettingsDialog_ApplicationSettings, 
  ts_Editor_SettingsDialog_Base, ts_Editor_SettingsDialog_EditorSettings, 
  ts_Editor_SettingsDialog_Extensions, 
  ts_Editor_SettingsDialog_FileAssociations, 
  ts_Editor_SettingsDialog_FileTypes, ts_Editor_SettingsDialog_Highlighters, 
  ts_Editor_SettingsDialog_KeyMappings, ts_Editor_SettingsDialog_Old, 
  ts_Editor_Shortcuts_ToolView, ts_Editor_Structure_ToolView, 
  ts_Editor_Test_ToolView, ts_Editor_ToolView_Base, ts.Editor.ViewList.Data, 
  ts_Editor_ViewList_ToolView, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('tslib', @Register);
end.
