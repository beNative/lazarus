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
  pl_kcontrols, pl_zeosdbocomp,

  richmemopackage, OMultiPanel, ts_Components_DBGridView, ts_Components_Docking,
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
  ts_core_xmlutils, ts_editor_aboutdialog, ts_Editor_ActionListViewForm,
  ts_Editor_AlignLinesForm, ts_Editor_CharacterMapDialog,
  ts_Editor_CodeFilterDialog, ts_Editor_CodeFormatters,
  ts_Editor_CodeFormatters_SQL, ts_Editor_CodeShaperForm, ts_editor_codetags,
  ts_editor_commands, ts_editor_commentstripper, ts_Editor_CustomToolView,
  ts_Editor_Helpers, ts_Editor_HexEditorForm, ts_Editor_HighlighterAttributes,
  ts_Editor_Highlighters, ts_Editor_HTMLViewForm, ts_Editor_Interfaces,
  ts_Editor_Manager, ts_Editor_Minimap, ts_Editor_PreviewForm,
  ts_Editor_Resources, ts_Editor_SearchEngine, ts_Editor_SearchForm,
  ts_Editor_SelectionInfo, ts_Editor_SelectionInfoForm, ts_Editor_Settings,
  ts_Editor_SettingsDialog, ts_Editor_Settings_AlignLines,
  ts_editor_settings_codeshaper, ts_Editor_Settings_SearchEngine,
  ts_editor_shortcutsdialog, ts_Editor_Testform, ts_Editor_Utils,
  ts_Editor_View, ts_Editor_ViewListForm, ts_Editor_XmlTreeForm,
  ts_richeditor_helpers, ts_richeditor_interfaces, ts_richeditor_manager,
  ts_richeditor_textattributes, ts_richeditor_view;

{$R *.res}

begin
  Application.Title := 'Notepas';
  //if FileExists('Notepas.trc') then
  //  DeleteFile('Notepas.trc');
  //SetHeapTraceOutput('Notepas.trc');
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  SetDefaultLang('nl');
  Application.Run;
end.

