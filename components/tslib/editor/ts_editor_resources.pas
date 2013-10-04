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

unit ts_Editor_Resources;

{$MODE Delphi}

{ Common resources }

interface

uses
  Classes, SysUtils, FileUtil, Controls,

  SynHighlighterPas, SynHighlighterLFM, SynHighlighterXML, SynHighlighterHTML,
  SynHighlighterDiff, SynHighlighterPerl;

const
  HL_LOG  = 'LOG';
  HL_TXT  = 'TXT';
  HL_PAS  = 'PAS';
  HL_CPP  = 'CPP';
  HL_JAVA = 'JAVA';
  HL_SQL  = 'SQL';
  HL_XML  = 'XML';
  HL_LFM  = 'LFM';
  HL_INI  = 'INI';
  HL_BAT  = 'BAT';
  HL_RTF  = 'RTF';
  HL_RES  = 'RES';
  HL_PHP  = 'PHP';
  HL_PERL = 'PERL';
  HL_PY   = 'PY';
  HL_PO   = 'PO';
  HL_HTML = 'HTML';
  HL_CSS  = 'CSS';
  HL_JS   = 'JS';
  HL_CS   = 'CS';

  LAYOUT_INI = 'notepas.highlighter.ini.xml';
  LAYOUT_RTF = 'notepas.highlighter.rtf.xml';
  LAYOUT_RES = 'notepas.highlighter.res.xml';
  LAYOUT_LOG = 'notepas.highlighter.log.xml';
  LAYOUT_CS  = 'notepas.highlighter.cs.xml';

  // comma separated lists of supported file extensions (no spaces)
  FILE_EXTENSIONS_TXT  = 'txt,nfo,me';
  FILE_EXTENSIONS_PAS  = 'pas,dpr,pp,lpr,inc,dpk,lpk';
  FILE_EXTENSIONS_CPP  = 'cpp,h,hpp';
  FILE_EXTENSIONS_JAVA = 'java';
  FILE_EXTENSIONS_SQL  = 'sql';
  FILE_EXTENSIONS_XML  = 'xml,hgl,' +
                         'lpi,lps,' +            // Lazarus
                         'fpc,compiled' +        // FPC Make
                         'dproj,groupproj,' +    // Delphi
                         'template';             // Java templates
  FILE_EXTENSIONS_LFM  = 'dfm,lfm';
  FILE_EXTENSIONS_INI  = 'ini,' +
                         'fpd,' +                // FastReport definition
                         'reg,' +                // Windows registry file
                         'prop,properties';
  FILE_EXTENSIONS_BAT  = 'bat,cmd';
  FILE_EXTENSIONS_RTF  = 'rtf';
  FILE_EXTENSIONS_RES  = 'res';
  FILE_EXTENSIONS_PHP  = 'php';
  FILE_EXTENSIONS_PERL = 'pl';
  FILE_EXTENSIONS_PY   = 'py';
  FILE_EXTENSIONS_HTML = 'html,htm';
  FILE_EXTENSIONS_PO   = 'po';
  FILE_EXTENSIONS_JS   = 'js';
  FILE_EXTENSIONS_CSS  = 'css';
  FILE_EXTENSIONS_CS   = 'cs';

//-----------------------------------------------------------------------------

resourcestring
  STextNotFound = 'Text not found';
  SSearching    = 'Searching...';

  SINIDescription  = 'INI';
  SBATDescription  = 'Windows batch script';
  SPODescription   = 'Gettext translation strings';
  SRTFDescription  = 'Rich Text Format document';
  STXTDescription  = 'Text document';
  SXMLDescription  = 'XML document';
  SHTMLDescription = 'HTML document';
  SPASDescription  = 'Object Pascal';
  SSQLDescription  = 'SQL';
  SCPPDescription  = 'C++';
  SJavaDescription = 'Java';
  SLFMDescription  = 'Object Pascal form definition';
  SLOGDescription  = 'Log';
  SRESDescription  = 'Windows resources';
  SPHPDescription  = 'PHP';
  SPERLDescription = 'Perl';
  SPYDescription   = 'Python';
  SCSSDescription  = 'Cascading Style Sheet';
  SJSDescription   = 'JavaScript';
  SCSDescription   = 'C#';

  SAskSaveChanges = 'File %s is modified. Do you want to save changes?';

  SNewEditorViewFileName   = '<new>';
  SFileMenuCaption         = '&File';
  SSeLectionMenuCaption    = 'Se&lection';
  SSearchMenuCaption       = '&Search';
  SInsertMenuCaption       = '&Insert';
  SViewMenuCaption         = '&View';
  SToolsMenuCaption        = '&Tools';
  SSettingsMenuCaption     = '&Settings';
  SHighlightersMenuCaption = '&Highlighters';
  SHelpMenuCaption         = '&Help';
  SDebugMenuCaption        = '&Debug';
  SApplicationMenuCaption  = '&Application';

  // CodeFilterDialog
  SOneLineWithMatchFound = '1 line with match found.';
  SLinesWithMatchFound   = '%d lines with match found.';

const
  DEFAULT_BLEND_FACTOR = 128;

const
  ALineBreakStyles : array[TTextLineBreakStyle] of string = (
    'LF',
    'CRLF',
    'CR'
  );

type
  TEditorOptionsFoldInfo = record
    Name    : string;     // Name for display
    Xml     : string;     // Name for XML
    Index   : Integer;    // FHighlighter.FoldConf[index]
    Enabled : Boolean;
  end;

type
  TEditorOptionsDividerInfo = record
    Name     : string;   // Name for display
    Xml      : string;   // Name for XML
    BoolOpt  : Boolean;  // Checkbox only
    MaxLevel : Integer;
  end;

const
  dlgFoldPasBeginEnd        = 'Begin/End (nested)';
  dlgFoldPasProcBeginEnd    = 'Begin/End (procedure)';
  dlgFoldPasNestedComment   = 'Nested Comment';
  dlgFoldPasProcedure       = 'Procedure';
  dlgFoldPasUses            = 'Uses';
  dlgFoldPasVarType         = 'Var/Type (global)';
  dlgFoldLocalPasVarType    = 'Var/Type (local)';
  dlgFoldPasClass           = 'Class/Object';
  dlgFoldPasClassSection    = 'public/private';
  dlgFoldPasUnitSection     = 'Unit section';
  dlgFoldPasProgram         = 'Program';
  dlgFoldPasUnit            = 'Unit';
  dlgFoldPasRecord          = 'Record';
  dlgFoldPasTry             = 'Try';
  dlgFoldPasExcept          = 'Except/Finally';
  dlgFoldPasRepeat          = 'Repeat';
  dlgFoldPasCase            = 'Case';
  dlgFoldPasCaseElse        = 'Case/Else';
  dlgFoldPasAsm             = 'Asm';
  dlgFoldPasIfDef           = '{$IFDEF}';
  dlgFoldPasUserRegion      = '{%Region}';
  dlgFoldPasAnsiComment     = 'Comment (* *)';
  dlgFoldPasBorComment      = 'Comment { }';
  dlgFoldPasSlashComment    = 'Comment //';

  dlgFoldLfmObject      = 'Object (inherited, inline)';
  dlgFoldLfmList        = 'List <>';
  dlgFoldLfmItem        = 'Item';

  dlgFoldXmlNode        = 'Node';
  dlgFoldXmlComment     = 'Comment';
  dlgFoldXmlCData       = 'CData';
  dlgFoldXmlDocType     = 'DocType';
  dlgFoldXmlProcess     = 'Processing Instruction';

  // TSI: not supported yet
  dlgFoldHtmlNode        = 'Node';
  dlgFoldHtmlComment     = 'Comment';
  dlgFoldHtmlAsp         = 'ASP';

  // TSI: not supported yet
  dlgFoldDiffFile      = 'File';
  dlgFoldDiffChunk     = 'Chunk';
  dlgFoldDiffChunkSect = 'Chunk section';

  dlgDividerOnOff        = 'Draw divider';
  dlgDividerDrawDepth    = 'Draw divider level';
  dlgDividerTopColor     = 'Line color';
  dlgDividerColorDefault = 'Use right margin color';
  dlgDividerNestColor    = 'Nested line color';

  dlgDivPasUnitSectionName  = 'Unit sections';
  dlgDivPasUsesName         = 'Uses clause';
  dlgDivPasVarGlobalName    = 'Var/Type';
  dlgDivPasVarLocalName     = 'Var/Type (local)';
  dlgDivPasStructGlobalName = 'Class/Struct';
  dlgDivPasStructLocalName  = 'Class/Struct (local)';
  dlgDivPasProcedureName    = 'Procedure/Function';
  dlgDivPasBeginEndName     = 'Begin/End';
  dlgDivPasTryName          = 'Try/Except';

const
  EditorOptionsFoldInfoPas: array [0..23] of TEditorOptionsFoldInfo = (
    (Name:  dlgFoldPasProcedure;     Xml:     'Procedure';
     Index: Ord(cfbtProcedure);    Enabled: True),
    (Name:  dlgFoldLocalPasVarType;  Xml:     'LocalVarType';
     Index: Ord(cfbtLocalVarType); Enabled: False),
    (Name:  dlgFoldPasProcBeginEnd;  Xml:     'ProcBeginEnd';
     Index: Ord(cfbtTopBeginEnd);  Enabled: False),
    (Name:  dlgFoldPasBeginEnd;      Xml:     'BeginEnd';
     Index: Ord(cfbtBeginEnd);     Enabled: False),
    (Name:  dlgFoldPasRepeat;        Xml:     'Repeat';
     Index: Ord(cfbtRepeat);       Enabled: False),
    (Name:  dlgFoldPasCase;          Xml:     'Case';
     Index: Ord(cfbtCase);         Enabled: False),
    (Name:  dlgFoldPasCaseElse;      Xml:     'CaseElse';
     Index: Ord(cfbtCaseElse);      Enabled: False),
    (Name:  dlgFoldPasTry;           Xml:     'Try';
     Index: Ord(cfbtTry);          Enabled: False),
    (Name:  dlgFoldPasExcept;        Xml:     'Except';
     Index: Ord(cfbtExcept);       Enabled: False),
    (Name:  dlgFoldPasAsm;           Xml:     'Asm';
     Index: Ord(cfbtAsm);          Enabled: True),

    (Name:  dlgFoldPasProgram;       Xml:     'Program';
     Index: Ord(cfbtProgram);      Enabled: True),
    (Name:  dlgFoldPasUnit;          Xml:     'Unit';
     Index: Ord(cfbtUnit);         Enabled: True),
    (Name:  dlgFoldPasUnitSection;   Xml:     'UnitSection';
     Index: Ord(cfbtUnitSection);  Enabled: True),
    (Name:  dlgFoldPasUses;          Xml:     'Uses';
     Index: Ord(cfbtUses);         Enabled: True),

    (Name:  dlgFoldPasVarType;       Xml:     'VarType';
     Index: Ord(cfbtVarType);      Enabled: True),
    (Name:  dlgFoldPasClass;         Xml:     'Class';
     Index: Ord(cfbtClass);        Enabled: True),
    (Name:  dlgFoldPasClassSection;  Xml:     'ClassSection';
     Index: Ord(cfbtClassSection); Enabled: True),
    (Name:  dlgFoldPasRecord;        Xml:     'Record';
     Index: Ord(cfbtRecord);       Enabled: True),

    (Name:  dlgFoldPasIfDef;         Xml:     'IfDef';
     Index: Ord(cfbtIfDef)  ;        Enabled: True),
    (Name:  dlgFoldPasUserRegion;    Xml:     'UserRegion';
     Index: Ord(cfbtRegion);       Enabled: True),

    (Name:  dlgFoldPasAnsiComment;   Xml:     'AnsiComment';
     Index: Ord(cfbtAnsiComment);  Enabled: True),
    (Name:  dlgFoldPasBorComment;    Xml:     'BorComment';
     Index: Ord(cfbtBorCommand);   Enabled: True),
    (Name:  dlgFoldPasSlashComment;    Xml:     'SlashComment';
     Index: Ord(cfbtSlashComment); Enabled: True),

    (Name:  dlgFoldPasNestedComment; Xml:     'NestedComment';
     Index: Ord(cfbtNestedComment);Enabled: True)
  );

  EditorOptionsFoldInfoLFM: array [0..2] of TEditorOptionsFoldInfo = (
    ( Name:    dlgFoldLfmObject;
      Xml:    'Object';
      Index:   Ord(cfbtLfmObject);
      Enabled: True
    ),
    ( Name:    dlgFoldLfmList;
      Xml:     'List';
      Index:   Ord(cfbtLfmList);
      Enabled: True
    ),
    ( Name:    dlgFoldLfmItem;
      Xml:     'Item';
      Index:   Ord(cfbtLfmItem);
      Enabled: True
    )
  );

  EditorOptionsFoldInfoXML: array [0..4] of TEditorOptionsFoldInfo = (
    ( Name:    dlgFoldXmlNode;
      Xml:    'Node';
      Index:   Ord(cfbtXmlNode);
      Enabled: True
    ),
    ( Name:    dlgFoldXmlComment;
      Xml:    'Comment';
      Index:   Ord(cfbtXmlComment);
      Enabled: True
    ),
    ( Name:    dlgFoldXmlCData;
      Xml:    'CData';
      Index:   Ord(cfbtXmlCData);
      Enabled: True
    ),
    ( Name:    dlgFoldXmlDocType;
      Xml:    'DocType';
      Index:   Ord(cfbtXmlDocType);
      Enabled: True
    ),
    ( Name:    dlgFoldXmlProcess;
      Xml:    'ProcessInstr';
      Index:   Ord(cfbtXmlProcess);
      Enabled: True
    )
  );
  EditorOptionsFoldInfoHTML: array [0..2] of TEditorOptionsFoldInfo = (
      ( Name:    dlgFoldHtmlNode;
        Xml:    'Node';
        Index:   Ord(cfbtHtmlNode);
        Enabled: True
      ),
      ( Name:    dlgFoldHtmlComment;
        Xml:    'Comment';
        Index:   Ord(cfbtXmlComment);
        Enabled: True
      ),
      ( Name:    dlgFoldHtmlAsp;
        Xml:    'ASP';
        Index:   Ord(cfbtHtmlAsp);
        Enabled: True
      )
  );

  EditorOptionsFoldInfoDiff: array [0..2] of TEditorOptionsFoldInfo = (
      ( Name:    dlgFoldDiffFile;
        Xml:    'File';
        Index:   Ord(cfbtDiffFile);
        Enabled: True
      ),
      ( Name:    dlgFoldDiffChunk;
        Xml:    'Chunk';
        Index:   Ord(cfbtDiffChunk);
        Enabled: True
      ),
      ( Name:    dlgFoldDiffChunkSect;
        Xml:    'ChunkSect';
        Index:   Ord(cfbtDiffChunkSect);
        Enabled: True
      )
  );

type
  TEditorOptionsDividerInfoList = array [0..999] of TEditorOptionsDividerInfo;
  PEditorOptionsDividerInfoList = ^TEditorOptionsDividerInfoList;

  TEditorOptionsDividerRecord = record
    Count: Integer;
    Info: PEditorOptionsDividerInfoList;
  end;

var
  EditorOptionsDividerInfoPas: array [0..8] of TEditorOptionsDividerInfo  = (
    (Name: dlgDivPasUnitSectionName;  Xml: 'Sect';    BoolOpt: True;  MaxLevel: 1),
    (Name: dlgDivPasUsesName;         Xml: 'Uses';    BoolOpt: True;  MaxLevel: 0),
    (Name: dlgDivPasVarGlobalName;    Xml: 'GVar';    BoolOpt: True;  MaxLevel: 1),
    (Name: dlgDivPasVarLocalName;     Xml: 'LVar';    BoolOpt: False; MaxLevel: 0),
    (Name: dlgDivPasStructGlobalName; Xml: 'GStruct'; BoolOpt: False; MaxLevel: 1),
    (Name: dlgDivPasStructLocalName;  Xml: 'LStruct'; BoolOpt: False; MaxLevel: 0),
    (Name: dlgDivPasProcedureName;    Xml: 'Proc';    BoolOpt: False; MaxLevel: 1),
    (Name: dlgDivPasBeginEndName;     Xml: 'Begin';   BoolOpt: False; MaxLevel: 0),
    (Name: dlgDivPasTryName;          Xml: 'Try';     BoolOpt: False; MaxLevel: 0)
  );

type

  { TResourcesDataModule }

  TResourcesDataModule = class(TDataModule)
    imlFunctionKeys: TImageList;
    imlMain: TImageList;
  private
  public
  end;

implementation

{$R *.lfm}

end.

