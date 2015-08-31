{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit tslib_editor;

interface

uses
  SetupFiltersDialog, ts.Editor.CodeFormatters, ts.Editor.CodeFormatters.SQL, 
  ts.Editor.CodeTags, ts.Editor.Commands, ts.Editor.CommentStripper, 
  ts.Editor.Events, ts.Editor.HighlighterAttributes, ts.Editor.Highlighters, 
  ts.Editor.Interfaces, ts.Editor.Selection, ts.Editor.ToolView.Manager, 
  ts.Editor.Types, ts.Editor.Utils, ts_Editor_AboutDialog, ts_Editor_Manager, 
  ts_Editor_Resources, ts_Editor_View, ts.Editor.ActionList.Templates, 
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
  ts_Editor_ViewList_ToolView, ts.Editor.Factories.Settings, 
  ts.Editor.SortStrings.Settings, ts.Editor.Factories.Manager, SynFacilBasic, 
  SynFacilHighlighter, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('tslib_editor', @Register);
end.
