{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for tslib 2.0

   This file was generated on 16/01/2013
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

    P.Dependencies.Add('zcomponent');
    P.Dependencies.Add('lazrichedit');
    P.Dependencies.Add('xmltreepkg');
    P.Dependencies.Add('luicontrols');
    P.Dependencies.Add('nativexml_laz');
    P.Dependencies.Add('virtualtreeview_package');
    P.Dependencies.Add('luicomponents');
    P.Dependencies.Add('multiloglaz');
    P.Dependencies.Add('lcl');
    P.Dependencies.Add('richmemopackage');
    P.Dependencies.Add('synedit');
    P.Dependencies.Add('runtimetypeinfocontrols');
    P.Dependencies.Add('fcl');
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
    P.IncludePath.Add('../NativeXml');
    P.IncludePath.Add('../NativeXml/nativexml');
    P.IncludePath.Add('dialogs');
    P.IncludePath.Add('core');
    P.IncludePath.Add('editor');
    P.Options.Add('-Fu../NativeXml');
    P.Options.Add('-Fu../NativeXml/nativexml');
    P.Options.Add('-Fu../NativeXml/general');
    P.Options.Add('-Fudialogs');
    P.Options.Add('-Fueditor');
    P.Options.Add('-Fucore');
    P.Options.Add('-Furicheditor');
    P.Options.Add('-Fudocking');
    P.Options.Add('-Fu../rtticontrols/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../synedit/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../richmemo/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../NativeXml/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../luipack/luicontrols/lib/$(CPU_TARGET)-$(OS_TARGET)-$(LCL_PLATFORM)');
    P.Options.Add('-Fu../luipack/luicomponents/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../virtualtreeview/XMLTree/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../virtualtreeview/lib/$(CPU_TARGET)-$(OS_TARGET)-$(LCL_PLATFORM)');
    P.Options.Add('-Fu../luipack/lclextensions/lib/$(CPU_TARGET)-$(OS_TARGET)-$(LCL_PLATFORM)');
    P.Options.Add('-Fu../multilog/lib/$(CPU_TARGET)-$(OS_TARGET)-$(LCL_PLATFORM)');
    P.Options.Add('-Fu../lzRichEdit/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../zeosdbo/packages/lazarus/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../ideintf/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../lazcontrols/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../lcl/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../lcl/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../lazutils/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../packager/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu.');
    T:=P.Targets.AddUnit('tslib.pas');
    t.Dependencies.AddUnit('ts_editor_actionlistviewform');
    t.Dependencies.AddUnit('ts_editor_charactermapdialog');
    t.Dependencies.AddUnit('ts_editor_codefilterdialog');
    t.Dependencies.AddUnit('ts_editor_settingsdialog');
    t.Dependencies.AddUnit('ts_editor_codeshaperform');
    t.Dependencies.AddUnit('ts_editor_aboutdialog');
    t.Dependencies.AddUnit('ts_editor_searchform');
    t.Dependencies.AddUnit('ts_editor_shortcutsdialog');
    t.Dependencies.AddUnit('ts_editor_viewlistform');
    t.Dependencies.AddUnit('ts_editor_manager');
    t.Dependencies.AddUnit('ts_editor_helpers');
    t.Dependencies.AddUnit('ts_editor_interfaces');
    t.Dependencies.AddUnit('ts_editor_resources');
    t.Dependencies.AddUnit('ts_editor_settings');
    t.Dependencies.AddUnit('ts_editor_utils');
    t.Dependencies.AddUnit('ts_editor_view');
    t.Dependencies.AddUnit('ts_editor_highlighterattributes');
    t.Dependencies.AddUnit('ts_editor_highlighters');
    t.Dependencies.AddUnit('ts_core_datatemplates');
    t.Dependencies.AddUnit('ts_core_treeviewpresenter');
    t.Dependencies.AddUnit('ts_richeditor_manager');
    t.Dependencies.AddUnit('ts_richeditor_view');
    t.Dependencies.AddUnit('ts_docking');
    t.Dependencies.AddUnit('ts_docking_optionsdialog');
    t.Dependencies.AddUnit('ts_docking_storage');
    t.Dependencies.AddUnit('ts_docking_resources');
    t.Dependencies.AddUnit('ts_editor_previewform');
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
    t.Dependencies.AddUnit('ts_editor_testform');
    t.Dependencies.AddUnit('ts_core_componentinspector');
    t.Dependencies.AddUnit('ts_editor_codeformatters');
    t.Dependencies.AddUnit('ts_editor_alignlinesform');
    t.Dependencies.AddUnit('ts_core_helpers');
    t.Dependencies.AddUnit('ts_editor_commentstripper');
    t.Dependencies.AddUnit('ts_editor_settings_codeshaper');
    t.Dependencies.AddUnit('ts_editor_settings_alignlines');
    t.Dependencies.AddUnit('ts_editor_settings_searchengine');
    t.Dependencies.AddUnit('ts_editor_types');
    t.Dependencies.AddUnit('ts_editor_unihighlighter');
    t.Dependencies.AddUnit('ts_editor_exportrtf');

    T:=P.Targets.AddUnit('editor\ts_editor_actionlistviewform.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_charactermapdialog.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_codefilterdialog.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_settingsdialog.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_codeshaperform.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_aboutdialog.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_searchform.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_shortcutsdialog.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_viewlistform.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_manager.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_helpers.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_interfaces.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_resources.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_settings.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_utils.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_view.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_highlighterattributes.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_highlighters.pas');
    T:=P.Targets.AddUnit('core\ts_core_datatemplates.pas');
    T:=P.Targets.AddUnit('core\ts_core_treeviewpresenter.pas');
    T:=P.Targets.AddUnit('richeditor\ts_richeditor_manager.pas');
    T:=P.Targets.AddUnit('richeditor\ts_richeditor_view.pas');
    T:=P.Targets.AddUnit('docking\ts_docking.pas');
    T:=P.Targets.AddUnit('docking\ts_docking_optionsdialog.pas');
    T:=P.Targets.AddUnit('docking\ts_docking_storage.pas');
    T:=P.Targets.AddUnit('docking\ts_docking_resources.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_previewform.pas');
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
    T:=P.Targets.AddUnit('editor\ts_editor_testform.pas');
    T:=P.Targets.AddUnit('core\ts_core_componentinspector.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_codeformatters.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_alignlinesform.pas');
    T:=P.Targets.AddUnit('core\ts_core_helpers.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_commentstripper.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_settings_codeshaper.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_settings_alignlines.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_settings_searchengine.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_types.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_unihighlighter.pas');
    T:=P.Targets.AddUnit('editor\ts_editor_exportrtf.pas');

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
