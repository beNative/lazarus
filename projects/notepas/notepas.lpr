{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

program Notepas;

{$MODE Delphi}

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
  lazrichedit, richmemopackage,

  sharedlogger, ipcchannel, filechannel, fpccompat, logtreeview, multilog,
  multiloglcl, runtimetypeinfocontrols, pl_kcontrols, pl_zeosdbocomp, pl_zmsql,
  pl_richview, lazcontrols, pl_bgrabitmap, ts.Components.MultiPanel,
  ts.Components.DBGridView, ts.Components.Docking,
  ts.Components.Docking.OptionsDialog, ts.Components.Docking.Resources,
  ts.Components.Docking.Storage, ts.Components.ExportRTF,
  ts.Components.FileAssociation, ts.Components.GridView,
  ts.Components.Inspector, ts.Components.SynMiniMap,
  ts.Components.UNIHighlighter, ts.Components.UniqueInstance,
  ts.Components.XMLTree, ts.Components.XMLTree.Editors,
  ts.Components.XMLTree.NodeAttributes, ts_core_componentinspector,
  ts.Core.BRRE, ts.Core.BRREUnicode, ts.Core.NativeXml.Debug,
  ts.Core.NativeXml.Streams, ts.Core.NativeXml.StringTable,
  ts.Core.CodecUtilsWin32, ts.Core.Collections, ts.Core.ColumnDefinitions,
  ts.Core.ColumnDefinitionsDataTemplate, ts.Core.DataTemplates, ts.Core.DBUtils,
  ts.Core.DirectoryWatch, ts.Core.EncodingUtils, ts.Core.FormSettings,
  ts.Core.HashStrings, ts.Core.Helpers, ts.Core.KeyValues, ts.Core.NativeXml,
  ts.Core.NativeXml.ObjectStorage, ts.core.nativexml.win32compat,
  ts.Core.SQLParser, ts.Core.SQLScanner, ts.Core.SQLTree, ts.Core.StringUtils,
  ts.Core.TreeViewPresenter, ts.Core.Utils, ts.Core.Value, ts.Core.VersionInfo,
  ts.Core.XMLUtils, ts.Core.FileAssociations, ts.Editor.CodeFormatters,
  ts.Editor.CodeFormatters.SQL, ts.Editor.CodeTags, ts.Editor.Commands,
  ts.Editor.CommentStripper, ts.Editor.Helpers, ts.Editor.HighlighterAttributes,
  ts.Editor.Highlighters, ts.Editor.Interfaces, ts_Editor_Resources,
  ts.Editor.searchengine, ts.Editor.Selection, ts.editor.settings,
  ts.Editor.Settings.AlignLines, ts.Editor.Settings.CodeShaper,
  ts.editor.settings.searchengine, ts.Editor.Utils, SetupFiltersDialog,
  ts_Editor_toolview_actionlist, ts_editor_toolview_alignlines,
  ts_editor_toolview_base, ts_editor_toolview_charactermap,
  ts_editor_toolview_codefilter, ts_editor_toolview_codeshaper,
  ts_editor_toolview_hexeditor, ts_editor_toolview_htmlview,
  ts_editor_toolview_minimap, ts_editor_toolview_preview,
  ts_editor_toolview_scripteditor, ts_editor_toolview_search,
  ts_editor_toolview_selectioninfo, ts_editor_toolview_shortcuts,
  ts_editor_toolview_structure, ts_editor_toolview_test,
  ts_editor_toolview_viewlist, ts.RichEditor.Helpers, ts.RichEditor.Interfaces,
  ts.RichEditor.Manager, ts.RichEditor.TextAttributes, ts.RichEditor.View,

  ts.Editor.Toolview.Manager, ts_Editor_Manager, ts_Editor_AboutDialog,
  ts_Editor_View, ts_Editor_SettingsDialog_Old, ts_Editor_SettingsDialog,
  ts_Editor_SettingsDialog_FileAssociations, ts_editor_settingsdialog_filetypes,
  ts_Editor_SettingsDialog_Highlighters, ts_editor_settingsdialog_keymappings,
  ts_editor_settingsdialog_extensions, ts.Editor.Events, ts.Core.Events,
  ts_Editor_SettingsDialog_ApplicationSettings, ts_Editor_SettingsDialog_Base,
  ts_Editor_SettingsDialog_EditorSettings, ts.Editor.Settings.MiniMap,
  ts.Editor.Settings.HTMLView, ts.Editor.Settings.HexEditor,
  ts.Editor.Settings.CodeFilter, ts_Editor_ToolView_Filter, ts.Editor.Templates;

{$R *.res}

begin
  SetDefaultLang('en');
  Application.Title := 'Notepas';
  //if FileExists('Notepas.trc') then
  //  DeleteFile('Notepas.trc');
  //SetHeapTraceOutput('Notepas.trc');
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

