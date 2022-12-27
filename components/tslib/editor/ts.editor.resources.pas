{
  Copyright (C) 2013-2023 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit ts.Editor.Resources;

{ Shared resources. }

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, FileUtil, Controls,

  SynHighlighterPas, SynHighlighterLFM, SynHighlighterXML, SynHighlighterHTML,
  SynHighlighterDiff, SynHighlighterPerl;

const
  HL_BAT  = 'BAT';
  HL_CPP  = 'CPP';
  HL_CS   = 'CS';
  HL_CSS  = 'CSS';
  HL_DIFF = 'DIFF';
  HL_HTML = 'HTML';
  HL_INI  = 'INI';
  HL_ISS  = 'ISS';
  HL_JAVA = 'JAVA';
  HL_JS   = 'JS';
  HL_LFM  = 'LFM';
  HL_LOG  = 'LOG';
  HL_LUA  = 'LUA';
  HL_PAS  = 'PAS';
  HL_PERL = 'PERL';
  HL_PHP  = 'PHP';
  HL_PIKE = 'PIKE';
  HL_PO   = 'PO';
  HL_PY   = 'PY';
  HL_RC   = 'RC';
  HL_RES  = 'RES';
  HL_RTF  = 'RTF';
  HL_RUBY = 'RUBY';
  HL_SH   = 'SH';
  HL_SQL  = 'SQL';
  HL_TEX  = 'TEX';
  HL_TXT  = 'TXT';
  HL_VB   = 'VB';
  HL_XML  = 'XML';

  LAYOUT_INI   = 'notepas.highlighter.ini.xml';
  LAYOUT_RTF   = 'notepas.highlighter.rtf.xml';
//  LAYOUT_RES   = 'notepas.highlighter.res.xml';
  LAYOUT_LOG   = 'notepas.highlighter.log.xml';
  //LAYOUT_CS    = 'notepas.highlighter.cs.xml';
  //LAYOUT_RUBY  = 'notepas.highlighter.ruby.xml';
  //LAYOUT_LUA   = 'notepas.highlighter.lua.xml';

  // comma separated lists of supported file extensions (no spaces)
  FILE_EXTENSIONS_TXT  = 'txt,nfo,me';
  FILE_EXTENSIONS_PAS  = 'pas,dpr,pp,lpr,inc,dpk';
  FILE_EXTENSIONS_CPP  = 'cpp,hpp,' +            // C++
                         'c,h,' +                // regular C
                         'ino,pde,' +            // Arduino sketch
                         'xpm';                  // X11 Pixmap Graphic
  FILE_EXTENSIONS_JAVA = 'java';                 // Java source file
  FILE_EXTENSIONS_SQL  = 'sql';                  // SQL script source file
  FILE_EXTENSIONS_XML  = 'xml,' +                // General XML file
                         'dtd,' +                // Document Type Definition
                         'xsd,' +                // XML Schema Definition
                         'xslt,' +               // XSL Transform file
                         'hgl,' +                // Highlighter (synunisyn)
                         'lpi,lps,lpk,' +        // Lazarus
                         'fpc,compiled,' +       // FPC Make
                         'dproj,groupproj,' +    // Delphi
                         'template,' +           // Java templates
                         'svg,' +                // Scalable Vector Graphics
                         'docx,' +               // Open XML document
                         'docm,' +               // Word Macro-Enabled Document
                         'dotx,' +               // Word Template
                         'dotm,' +               // Word Macro-Enabled Template
                         'xlsx,' +               // Open XML Spreadsheet
                         'xlsm,' +               // Excel Macro-Enabled Workbook
                         'xltx,' +               // Excel Template
                         'xltm,' +               // Excel Macro-Enabled Template
                         'xlam,' +               // Excel Add-In
                         'pptx,' +               // Open XML Presentation
                         'pptm,' +               // PowerPoint Macro-Enabled Presentation
                         'potx,'  +              // PowerPoint Template
                         'potm,'  +              // PowerPoint Macro-Enabled Template
                         'ppam,'  +              // PowerPoint Add-In
                         'ppsx,'  +              // PowerPoint Show
                         'ppsm,' +               // PowerPoint Macro-Enabled Show
                         'vsdx,' +               // Visio Drawing
                         'vsdm,' +               // Visio Macro-Enabled Drawing
                         'vssx,' +               // Visio Stencil
                         'vssm,' +               // Visio Macro-Enabled Stencil
                         'vstx,' +               // Visio Template
                         'vstm,' +               // Visio Macro-Enabled Template
                         'sgml,' +               // Standard Generalized Markup Language
                         'xmmap,' +              // Mindjet MindManager
                         'vcxproj,' +            // Visual C++ Project file
                         'vcxproj.filters,' +    // Visual C++ Project filters
                         'cpppjoj';              // C++ Project file
  FILE_EXTENSIONS_LFM  = 'dfm,' +                // Delphi VCL form
                         'lfm,' +                // Lazarus LCL form
                         'fmx';                  // Delphi Firemonkey form
  FILE_EXTENSIONS_INI  = 'ini,' +
                         'fpd,' +                // FastReport definition
                         'reg,' +                // Windows registry file
                         'lrt,' +
                         'msg,' +
                         'prop,properties,' +
                         'desktop';
  FILE_EXTENSIONS_BAT  = 'bat,cmd';
  FILE_EXTENSIONS_RTF  = 'rtf';
  FILE_EXTENSIONS_RES  = 'res';
  FILE_EXTENSIONS_PHP  = 'php';
  FILE_EXTENSIONS_PERL = 'pl';
  FILE_EXTENSIONS_PY   = 'py';
  FILE_EXTENSIONS_HTML = 'html,htm';
  FILE_EXTENSIONS_PO   = 'po';
  FILE_EXTENSIONS_JS   = 'js,' +
                         'json';               // Javascript Object Notation
  FILE_EXTENSIONS_CSS  = 'css';
  FILE_EXTENSIONS_CS   = 'cs';
  FILE_EXTENSIONS_DIFF = 'diff';
  FILE_EXTENSIONS_TEX  = 'tex';
  FILE_EXTENSIONS_SH   = 'sh';
  FILE_EXTENSIONS_RUBY = 'rb,rbw';
  FILE_EXTENSIONS_LUA  = 'lua';
  FILE_EXTENSIONS_PIKE = 'pike,pmod';
  FILE_EXTENSIONS_VB   = 'vb';
  FILE_EXTENSIONS_RC   = 'rc';
  FILE_EXTENSIONS_ISS  = 'iss';

resourcestring
  STextNotFound = 'Text not found';
  SSearching    = 'Searching...';

  SBATDescription  = 'Windows batch script';
  SCPPDescription  = 'C++';
  SCSDescription   = 'C#';
  SCSSDescription  = 'Cascading Style Sheet';
  SDIFFDescription = 'Diff';
  SHTMLDescription = 'HTML document';
  SINIDescription  = 'Settings file';
  SISSDescription  = 'Inno Setup script';
  SJavaDescription = 'Java';
  SJSDescription   = 'JavaScript';
  SLFMDescription  = 'Object Pascal form definition';
  SLOGDescription  = 'Log';
  SLUADescription  = 'Lua';
  SPASDescription  = 'Object Pascal';
  SPERLDescription = 'Perl';
  SPHPDescription  = 'PHP';
  SPikeDescription = 'Pike';
  SPODescription   = 'Gettext translation strings';
  SPYDescription   = 'Python';
  SRCDescription   = 'Windows resource file';
  SRTFDescription  = 'Rich Text Format document';
  SRUBYDescription = 'Ruby';
  SSHDescription   = 'Shell script';
  SSQLDescription  = 'SQL';
  STEXDescription  = 'TeX';
  STXTDescription  = 'Text document';
  SVBDescription   = 'Visual Basic';
  SXMLDescription  = 'XML document';


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
  SApplicationMenuCaption  = '&Application';

  // CodeFilterDialog
  SOneLineWithMatchFound = '1 line with match found.';
  SLinesWithMatchFound   = '%d lines with match found.';

  // IEditorManager
  SNotImplementedYet     = 'This feature is not implemented yet';

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
    (Name:    dlgFoldLfmObject;
     Xml:    'Object';
     Index:   Ord(cfbtLfmObject);
     Enabled: True
    ),
    (Name:    dlgFoldLfmList;
     Xml:     'List';
     Index:   Ord(cfbtLfmList);
     Enabled: True
    ),
    (Name:    dlgFoldLfmItem;
     Xml:     'Item';
     Index:   Ord(cfbtLfmItem);
     Enabled: True
    )
  );

  EditorOptionsFoldInfoXML: array [0..4] of TEditorOptionsFoldInfo = (
    (Name:    dlgFoldXmlNode;
     Xml:    'Node';
     Index:   Ord(cfbtXmlNode);
     Enabled: True
    ),
    (Name:    dlgFoldXmlComment;
     Xml:    'Comment';
     Index:   Ord(cfbtXmlComment);
     Enabled: True
    ),
    (Name:    dlgFoldXmlCData;
     Xml:    'CData';
     Index:   Ord(cfbtXmlCData);
     Enabled: True
    ),
    (Name:    dlgFoldXmlDocType;
     Xml:    'DocType';
     Index:   Ord(cfbtXmlDocType);
     Enabled: True
    ),
    (Name:    dlgFoldXmlProcess;
     Xml:    'ProcessInstr';
     Index:   Ord(cfbtXmlProcess);
     Enabled: True
    )
  );
  EditorOptionsFoldInfoHTML: array [0..2] of TEditorOptionsFoldInfo = (
    (Name:    dlgFoldHtmlNode;
     Xml:    'Node';
     Index:   Ord(cfbtHtmlNode);
     Enabled: True
    ),
    (Name:    dlgFoldHtmlComment;
     Xml:    'Comment';
     Index:   Ord(cfbtXmlComment);
     Enabled: True
    ),
    (Name:    dlgFoldHtmlAsp;
     Xml:    'ASP';
     Index:   Ord(cfbtHtmlAsp);
     Enabled: True
    )
  );

  EditorOptionsFoldInfoDiff: array [0..2] of TEditorOptionsFoldInfo = (
    (Name:    dlgFoldDiffFile;
     Xml:    'File';
     Index:   Ord(cfbtDiffFile);
     Enabled: True
    ),
    (Name:    dlgFoldDiffChunk;
     Xml:    'Chunk';
     Index:   Ord(cfbtDiffChunk);
     Enabled: True
    ),
    (Name:    dlgFoldDiffChunkSect;
     Xml:    'ChunkSect';
     Index:   Ord(cfbtDiffChunkSect);
     Enabled: True
    )
  );

type
  TEditorOptionsDividerInfoList = array [0..999] of TEditorOptionsDividerInfo;
  PEditorOptionsDividerInfoList = ^TEditorOptionsDividerInfoList;

  TEditorOptionsDividerRecord = record
    Count : Integer;
    Info  : PEditorOptionsDividerInfoList;
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
    imlFunctionKeys : TImageList;
  end;

implementation

{$R *.lfm}

end.

