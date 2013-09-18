{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for tslib 2.0

   This file was generated on 18/09/2013
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_tslib;

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPAckage('tslib');
    P.Version:='2.0';

{$ifdef ALLPACKAGES}
    // when this is part of a meta package, set here the sub directory
    // P.Directory:='put here the relative path';
{$endif ALLPACKAGES}

    P.Dependencies.Add('frameviewer09');
    P.Dependencies.Add('pl_exsystem');
    P.Dependencies.Add('pl_kcontrols');
    P.Dependencies.Add('turbopoweripro');
    P.Dependencies.Add('pl_exdesign');
    P.Dependencies.Add('pl_aggpasvs');
    P.Dependencies.Add('pl_fzcontrols');
    P.Dependencies.Add('pl_lazsolutions');
    P.Dependencies.Add('pl_luicontrols');
    P.Dependencies.Add('pl_virtualtrees');
    P.Dependencies.Add('pl_zeosdbocomp');
    P.Dependencies.Add('richmemopackage');
    P.Dependencies.Add('lazrichedit');
    P.Dependencies.Add('synedit');
    P.Dependencies.Add('runtimetypeinfocontrols');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-CX');
    P.Options.Add('-Cirot');
    P.Options.Add('-O2');
    P.Options.Add('-gs');
    P.Options.Add('-gl');
    P.Options.Add('-gh');
    P.Options.Add('-vewnhid');
    P.Options.Add('-l');
    P.Options.Add('-vm5024');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCL_PLATFORM)');
    P.IncludePath.Add('editor');
    P.Options.Add('-Fueditor');
    P.Options.Add('-Fucore');
    P.Options.Add('-Furicheditor');
    P.Options.Add('-Fucomponents');
    P.Options.Add('-Fu../../../typhon/components/pl_LuiControls/source/multilog');
    P.Options.Add('-Fu../../../typhon/packager/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../../typhon/components/lazutils/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../../typhon/components/pl_Synapse/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/lcl/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../../typhon/lcl/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/cairocanvas/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../thtmlviewer/package/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../../typhon/components/lazcontrols/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../lazRichEdit/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../../typhon/components/pl_AGGPas/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_lclextensions/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../richmemo/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../../typhon/components/synedit/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/ideintf/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_VirtualTrees/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/printers/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/tdbf/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_AGGPasVS/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_ExDesign/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_ExSystem/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_FZControls/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_LuiControls/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_ZeosDBO/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/rtticontrols/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/synedit/design/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/turbopower_ipro/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_KControls/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_LazSolutions/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu.');
    T:=P.Targets.AddUnit('tslib.pas');
    t.Dependencies.AddUnit('ts_editor_settingsdialog');
    t.Dependencies.AddUnit('ts_editor_aboutdialog');
    t.Dependencies.AddUnit('ts_editor_helpers');
    t.Dependencies.AddUnit('ts_editor_interfaces');
    t.Dependencies.AddUnit('ts_editor_settings');
    t.Dependencies.AddUnit('ts_editor_utils');
    t.Dependencies.AddUnit('ts_editor_view');
    t.Dependencies.AddUnit('ts_editor_highlighterattributes');
    t.Dependencies.AddUnit('ts_editor_highlighters');
    t.Dependencies.AddUnit('ts_core_datatemplates');
    t.Dependencies.AddUnit('ts_core_treeviewpresenter');
    t.Dependencies.AddUnit('ts_richeditor_manager');
    t.Dependencies.AddUnit('ts_richeditor_view');
    t.Dependencies.AddUnit('ts_components_docking');
    t.Dependencies.AddUnit('ts_components_docking_optionsdialog');
    t.Dependencies.AddUnit('ts_components_docking_storage');
    t.Dependencies.AddUnit('ts_components_docking_resources');
    t.Dependencies.AddUnit('ts_core_xmlutils');
    t.Dependencies.AddUnit('ts_core_columndefinitions');
    t.Dependencies.AddUnit('ts_core_columndefinitionsdatatemplate');
    t.Dependencies.AddUnit('ts_core_dbutils');
    t.Dependencies.AddUnit('ts_core_directorywatch');
    t.Dependencies.AddUnit('ts_core_formsettings');
    t.Dependencies.AddUnit('ts_core_keyvalues');
    t.Dependencies.AddUnit('ts_core_utils');
    t.Dependencies.AddUnit('ts_core_versioninfo');
    t.Dependencies.AddUnit('ts_richeditor_helpers');
    t.Dependencies.AddUnit('ts_richeditor_interfaces');
    t.Dependencies.AddUnit('ts_richeditor_textattributes');
    t.Dependencies.AddUnit('ts_editor_searchengine');
    t.Dependencies.AddUnit('ts_core_stringutils');
    t.Dependencies.AddUnit('ts_core_sqltree');
    t.Dependencies.AddUnit('ts_core_sqlparser');
    t.Dependencies.AddUnit('ts_core_sqlscanner');
    t.Dependencies.AddUnit('ts_core_componentinspector');
    t.Dependencies.AddUnit('ts_editor_codeformatters');
    t.Dependencies.AddUnit('ts_core_helpers');
    t.Dependencies.AddUnit('ts_editor_commentstripper');
    t.Dependencies.AddUnit('ts_editor_settings_codeshaper');
    t.Dependencies.AddUnit('ts_editor_settings_alignlines');
    t.Dependencies.AddUnit('ts_editor_settings_searchengine');
    t.Dependencies.AddUnit('ts_components_unihighlighter');
    t.Dependencies.AddUnit('ts_core_collections');
    t.Dependencies.AddUnit('ts_editor_codeformatters_sql');
    t.Dependencies.AddUnit('BRRE');
    t.Dependencies.AddUnit('BRREUnicode');
    t.Dependencies.AddUnit('ts_components_xmltree');
    t.Dependencies.AddUnit('ts_components_xmltree_nodeattributes');
    t.Dependencies.AddUnit('ts_components_xmltree_editors');
    t.Dependencies.AddUnit('ts_editor_resources');
    t.Dependencies.AddUnit('ts_components_exportrtf');
    t.Dependencies.AddUnit('ts_editor_selectioninfo');
    t.Dependencies.AddUnit('ts_components_uniqueinstance');
    t.Dependencies.AddUnit('ts_components_inspector');
    t.Dependencies.AddUnit('ts_components_dbgridview');
    t.Dependencies.AddUnit('ts_components_gridview');
    t.Dependencies.AddUnit('ts_components_fileassociation');
    t.Dependencies.AddUnit('ts_core_codecutilswin32');
    t.Dependencies.AddUnit('ts_core_encodingutils');
    t.Dependencies.AddUnit('ts_core_nativexml');
    t.Dependencies.AddUnit('ts_core_nativexmlobjectstorage');
    t.Dependencies.AddUnit('ts_core_nativexmlwin32compat');
    t.Dependencies.AddUnit('ts_editor_codetags');
    t.Dependencies.AddUnit('ts_core_value');
    t.Dependencies.AddUnit('ts_components_synminimap');
    t.Dependencies.AddUnit('ts_editor_commands');
    t.Dependencies.AddUnit('ts_editor_manager');
    t.Dependencies.AddUnit('ipcchannel');
    t.Dependencies.AddUnit('ts_editor_settingsdialog_fileassociations');
    t.Dependencies.AddUnit('ts_editor_settingsdialog_filetypes');
    t.Dependencies.AddUnit('ts_editor_settingsdialog_highlighters');
    t.Dependencies.AddUnit('ts_editor_settingsdialog_keymappings');
    t.Dependencies.AddUnit('ts_editor_settingsdialog_old');
    t.Dependencies.AddUnit('ts_editor_toolview_actionlist');
    t.Dependencies.AddUnit('ts_editor_toolview_alignlines');
    t.Dependencies.AddUnit('ts_editor_toolview_base');
    t.Dependencies.AddUnit('ts_editor_toolview_charactermap');
    t.Dependencies.AddUnit('ts_editor_toolview_codefilter');
    t.Dependencies.AddUnit('ts_editor_toolview_codeshaper');
    t.Dependencies.AddUnit('ts_editor_toolview_hexeditor');
    t.Dependencies.AddUnit('ts_editor_toolview_htmlview');
    t.Dependencies.AddUnit('ts_editor_toolview_minimap');
    t.Dependencies.AddUnit('ts_editor_toolview_preview');
    t.Dependencies.AddUnit('ts_editor_toolview_search');
    t.Dependencies.AddUnit('ts_editor_toolview_selectioninfo');
    t.Dependencies.AddUnit('ts_editor_toolview_shortcuts');
    t.Dependencies.AddUnit('ts_editor_toolview_structure');
    t.Dependencies.AddUnit('ts_editor_toolview_test');
    t.Dependencies.AddUnit('ts_editor_toolview_viewlist');

    T:=P.Targets.AddUnit('editor\ts_editor_settingsdialog.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_aboutdialog.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_helpers.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_interfaces.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_settings.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_utils.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_view.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_highlighterattributes.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_highlighters.pas');
    T:=P.Targets.AddUnit('core\ts_core_datatemplates.pas');
    T:=P.Targets.AddUnit('core\ts_core_treeviewpresenter.pas');
    T:=P.Targets.AddUnit('richeditor\ts_richeditor_manager.pas');
    T:=P.Targets.AddUnit('richeditor\ts_richeditor_view.pas');
    T:=P.Targets.AddUnit('components\ts_components_docking.pas');
    T:=P.Targets.AddUnit('components\ts_components_docking_optionsdialog.pas');
    T:=P.Targets.AddUnit('components\ts_components_docking_storage.pas');
    T:=P.Targets.AddUnit('components\ts_components_docking_resources.pas');
    T:=P.Targets.AddUnit('core\ts_core_xmlutils.pas');
    T:=P.Targets.AddUnit('core\ts_core_columndefinitions.pas');
    T:=P.Targets.AddUnit('core\ts_core_columndefinitionsdatatemplate.pas');
    T:=P.Targets.AddUnit('core\ts_core_dbutils.pas');
    T:=P.Targets.AddUnit('core\ts_core_directorywatch.pas');
    T:=P.Targets.AddUnit('core\ts_core_formsettings.pas');
    T:=P.Targets.AddUnit('core\ts_core_keyvalues.pas');
    T:=P.Targets.AddUnit('core\ts_core_utils.pas');
    T:=P.Targets.AddUnit('core\ts_core_versioninfo.pas');
    T:=P.Targets.AddUnit('richeditor\ts_richeditor_helpers.pas');
    T:=P.Targets.AddUnit('richeditor\ts_richeditor_interfaces.pas');
    T:=P.Targets.AddUnit('richeditor\ts_richeditor_textattributes.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_searchengine.pas');
    T:=P.Targets.AddUnit('core\ts_core_stringutils.pas');
    T:=P.Targets.AddUnit('core\ts_core_sqltree.pas');
    T:=P.Targets.AddUnit('core\ts_core_sqlparser.pas');
    T:=P.Targets.AddUnit('core\ts_core_sqlscanner.pas');
    T:=P.Targets.AddUnit('core\ts_core_componentinspector.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_codeformatters.pas');
    T:=P.Targets.AddUnit('core\ts_core_helpers.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_commentstripper.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_settings_codeshaper.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_settings_alignlines.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_settings_searchengine.pas');
    T:=P.Targets.AddUnit('components\ts_components_unihighlighter.pas');
    T:=P.Targets.AddUnit('core\ts_core_collections.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_codeformatters_sql.pas');
    T:=P.Targets.AddUnit('core\BRRE.pas');
    T:=P.Targets.AddUnit('core\BRREUnicode.pas');
    T:=P.Targets.AddUnit('components\ts_components_xmltree.pas');
    T:=P.Targets.AddUnit('components\ts_components_xmltree_nodeattributes.pas');
    T:=P.Targets.AddUnit('components\ts_components_xmltree_editors.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_resources.pas');
    T:=P.Targets.AddUnit('components\ts_components_exportrtf.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_selectioninfo.pas');
    T:=P.Targets.AddUnit('components\ts_components_uniqueinstance.pas');
    T:=P.Targets.AddUnit('components\ts_components_inspector.pas');
    T:=P.Targets.AddUnit('components\ts_components_dbgridview.pas');
    T:=P.Targets.AddUnit('components\ts_components_gridview.pas');
    T:=P.Targets.AddUnit('components\ts_components_fileassociation.pas');
    T:=P.Targets.AddUnit('core\ts_core_codecutilswin32.pas');
    T:=P.Targets.AddUnit('core\ts_core_encodingutils.pas');
    T:=P.Targets.AddUnit('core\ts_core_nativexml.pas');
    T:=P.Targets.AddUnit('core\ts_core_nativexmlobjectstorage.pas');
    T:=P.Targets.AddUnit('core\ts_core_nativexmlwin32compat.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_codetags.pas');
    T:=P.Targets.AddUnit('core\ts_core_value.pas');
    T:=P.Targets.AddUnit('components\ts_components_synminimap.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_commands.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_manager.pas');
    T:=P.Targets.AddUnit('..\..\..\typhon\components\pl_LuiControls\source\multilog\ipcchannel.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_settingsdialog_fileassociations.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_settingsdialog_filetypes.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_settingsdialog_highlighters.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_settingsdialog_keymappings.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_settingsdialog_old.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_toolview_actionlist.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_toolview_alignlines.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_toolview_base.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_toolview_charactermap.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_toolview_codefilter.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_toolview_codeshaper.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_toolview_hexeditor.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_toolview_htmlview.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_toolview_minimap.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_toolview_preview.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_toolview_search.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_toolview_selectioninfo.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_toolview_shortcuts.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_toolview_structure.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_toolview_test.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_toolview_viewlist.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('tslib.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_tslib;
  Installer.Run;
end.
{$endif ALLPACKAGES}
