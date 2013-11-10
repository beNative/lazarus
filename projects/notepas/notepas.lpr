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

  {$DEFINE UseCThreads} // Required when running on Linux!!
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  notepas_forms_main,
  { you can add units after this }
  DefaultTranslator, pl_exsystem, pl_virtualtrees,
  {$IFDEF Windows}
  FrameViewer09,
  {$ENDIF}

  lazrichedit,
  richmemopackage,
  runtimetypeinfocontrols,
  pl_kcontrols,

  pl_zeosdbocomp,
  pl_zmsql,
  pl_richview,
  lazcontrols,

  ts.Components.MultiPanel,
  ts.Components.DBGridView,
  ts.Components.Docking,
  ts.Components.Docking.OptionsDialog,
  ts.Components.Docking.Resources,
  ts.Components.Docking.Storage,
  ts.Components.ExportRTF,
  ts.Components.FileAssociation,
  ts.Components.GridView,
  ts.Components.Inspector,
  ts.Components.SynMiniMap,
  ts.Components.UNIHighlighter,
  ts.Components.UniqueInstance,
  ts.Components.XMLTree,
  ts.Components.XMLTree.Editors,
  ts.Components.XMLTree.NodeAttributes,
  ts.Core.Logger.channel.logfile,
  ts.Core.Logger.Channel.IPC,
  ts.Core.Logger,
  ts.Core.SharedLogger,
  ts_core_componentinspector,
  ts.Core.BRRE,
  ts.Core.BRREUnicode,
  ts.Core.NativeXml.Debug,
  ts.Core.NativeXml.Streams,
  ts.Core.NativeXml.StringTable,
  ts.Core.CodecUtilsWin32,
  ts.Core.Collections,
  ts.Core.ColumnDefinitions,
  ts.Core.ColumnDefinitionsDataTemplate,
  ts.Core.DataTemplates,
  ts.Core.DBUtils,
  ts.Core.DirectoryWatch,
  ts.Core.EncodingUtils,
  ts.Core.FormSettings,
  ts.Core.HashStrings,
  ts.Core.Helpers,
  ts.Core.KeyValues,
  ts.Core.NativeXml,
  ts.Core.NativeXml.ObjectStorage,
  ts.Core.NativeXml.win32compat,
  ts.Core.SQLParser,
  ts.Core.SQLScanner,
  ts.Core.SQLTree,
  ts.Core.StringUtils,
  ts.Core.TreeViewPresenter,
  ts.Core.Utils,
  ts.Core.Value,
  ts.Core.VersionInfo,
  ts.Core.XMLUtils,
  ts.Core.Events,
  ts.Core.FileAssociations,

  ts.Editor.CodeFormatters,
  ts.Editor.CodeFormatters.SQL,
  ts.Editor.CodeTags,
  ts.Editor.Commands,
  ts.Editor.CommentStripper,
  ts.Editor.Helpers,
  ts.Editor.HighlighterAttributes,
  ts.Editor.Highlighters,
  ts.Editor.Interfaces,
  ts_Editor_Resources,
  ts.Editor.Search.Engine,
  ts.Editor.Selection,
  ts.Editor.Settings,
  ts.Editor.AlignLines.Settings,
  ts.Editor.codeshaper.Settings,
  ts.Editor.Search.Engine.Settings,
  ts.Editor.Utils,
  ts.Editor.Toolview.Manager,
  ts_Editor_Manager,
  ts_Editor_AboutDialog,
  ts_Editor_View,
  ts_Editor_SettingsDialog_Old,
  ts_Editor_SettingsDialog,
  ts_Editor_SettingsDialog_FileAssociations,
  ts_Editor_settingsdialog_filetypes,
  ts_Editor_SettingsDialog_Highlighters,
  ts_Editor_settingsdialog_keymappings,
  ts_Editor_settingsdialog_extensions,
  ts.Editor.Events,
  ts_Editor_Colors_ToolView,
  ts_Editor_SettingsDialog_ApplicationSettings,
  ts_Editor_SettingsDialog_Base,
  ts_Editor_SettingsDialog_EditorSettings,
  ts.Editor.minimap.Settings,
  ts.Editor.htmlview.Settings,
  ts.Editor.hexeditor.Settings,
  ts.Editor.codefilter.Settings,
  ts_Editor_filter_ToolView,
  ts_Editor_viewlist_ToolView,
  ts.Editor.ActionList.Templates,
  ts.Editor.Search.Data,
  ts.Editor.Search.Templates,
  ts_Editor_search_ToolView,
  ts_Editor_actionlist_ToolView,
  ts_Editor_alignlines_ToolView,
  ts_Editor_ToolView_base,
  ts_Editor_charactermap_ToolView,
  ts_Editor_codefilter_ToolView,
  ts_Editor_codeshaper_ToolView,
  ts_Editor_HexEditor_ToolView,
  ts_Editor_HTMLview_ToolView,
  ts_Editor_Minimap_ToolView,
  ts_Editor_preview_ToolView,
  ts_Editor_ScriptEditor_ToolView,
  ts_Editor_SelectionInfo_ToolView,
  ts_Editor_Shortcuts_ToolView,
  ts_Editor_Structure_ToolView,
  ts_Editor_Test_ToolView,
  ts.Editor.CodeFilter.Data,
  ts.Editor.ViewList.Data,

  ts.RichEditor.Helpers,
  ts.RichEditor.Interfaces,
  ts.RichEditor.Manager,
  ts.RichEditor.TextAttributes,
  ts.RichEditor.View,
  ts.collections,
  SetupFiltersDialog;

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

