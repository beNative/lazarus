program Notepas;

{$mode objfpc}{$H+}

uses
  SysUtils,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  notepas_forms_main,
  { you can add units after this }
  DefaultTranslator, pl_exsystem, pl_virtualtrees, FrameViewer09,
  lazrichedit,

  sharedlogger, ipcchannel, pl_luicontrols, runtimetypeinfocontrols,
  pl_kcontrols, pl_zeosdbocomp, pl_zmsql,

  richmemopackage,
ts_components_multipanel, ts_Components_DBGridView, ts_Components_Docking,
  ts_components_docking_optionsdialog, ts_components_docking_resources,
  ts_components_docking_storage, ts_Components_ExportRTF,
  ts_Components_FileAssociation, ts_Components_GridView,
  ts_Components_Inspector, ts_Components_SynMiniMap,
  ts_Components_UniHighlighter, ts_components_uniqueinstance,
  ts_Components_XMLTree, ts_Components_XMLTree_Editors,
  ts_Components_XMLTree_NodeAttributes, BRRE, BRREUnicode, sdDebug, sdStreams,
  sdStringTable, ts_core_codecutilswin32, ts_Core_Collections,
  ts_Core_ColumnDefinitions, ts_Core_ColumnDefinitionsDataTemplate,
  ts_Core_ComponentInspector, ts_Core_DataTemplates, ts_core_dbutils,
  ts_Core_DirectoryWatch, ts_Core_EncodingUtils, ts_Core_FormSettings,
  ts_core_hashstrings, ts_Core_Helpers, ts_core_keyvalues, ts_core_nativexml,
  ts_Core_NativeXmlObjectStorage, ts_Core_NativeXmlWin32Compat,
  ts_core_sqlparser, ts_Core_SQLScanner, ts_core_sqltree, ts_Core_StringUtils,
  ts_Core_TreeViewPresenter, ts_Core_Utils, ts_Core_Value, ts_Core_VersionInfo,
  ts_core_xmlutils, ts_Core_FileAssociations, ts_editor_aboutdialog,
  ts_Editor_ToolView_ActionList, ts_editor_toolview_alignlines,
  ts_Editor_ToolView_CharacterMap, ts_editor_toolview_codefilter,
  ts_Editor_CodeFormatters, ts_Editor_CodeFormatters_SQL,
  ts_editor_toolview_codeshaper, ts_editor_codetags, ts_editor_commands,
  ts_editor_commentstripper, ts_Editor_ToolView_Base, ts_Editor_Helpers,
  ts_editor_toolview_hexeditor, ts_Editor_HighlighterAttributes,
  ts_Editor_Highlighters, ts_editor_toolview_htmlview, ts_Editor_Interfaces,
  ts_Editor_Manager, ts_editor_toolview_minimap, ts_editor_toolview_preview,
  ts_Editor_Resources, ts_Editor_SearchEngine, ts_editor_toolview_search,
  ts_editor_selection, ts_Editor_ToolView_SelectionInfo, ts_Editor_Settings,
  ts_Editor_SettingsDialog_Old, ts_Editor_Settings_AlignLines,
  ts_editor_settings_codeshaper, ts_Editor_Settings_SearchEngine,
  ts_editor_toolview_shortcuts, ts_editor_toolview_test, ts_Editor_Utils,
  ts_Editor_View, ts_Editor_ToolView_ViewList, ts_Editor_ToolView_Structure,
  ts_Editor_SettingsDialog_FileAssociations, ts_Editor_SettingsDialog_FileTypes,
  SetupFiltersDialog, ts_RichEditor_Helpers, ts_RichEditor_Interfaces,
  ts_richeditor_manager, ts_RichEditor_TextAttributes, ts_RichEditor_View,
  ts_Editor_SettingsDialog, ts_Editor_SettingsDialog_KeyMappings,
  ts_editor_settingsdialog_highlighters, ts_Editor_ToolView_ScriptEditor;

{$R *.res}

begin
  Application.Title := 'Notepas';
  //if FileExists('Notepas.trc') then
  //  DeleteFile('Notepas.trc');
  //SetHeapTraceOutput('Notepas.trc');
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  //SetDefaultLang('nl');
  Application.Run;
end.

