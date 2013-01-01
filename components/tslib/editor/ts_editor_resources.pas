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

{$mode delphi}

{ Common resources }

//*****************************************************************************

interface

uses
  Classes, SysUtils;

const
  HL_BaltaLOG = 'BaltaLOG';
  HL_TXT      = 'TXT';
  HL_PAS      = 'PAS';
  HL_CPP      = 'CPP';
  HL_JAVA     = 'JAVA';
  HL_SQL      = 'SQL';
  HL_XML      = 'XML';
  HL_LFM      = 'LFM';
  HL_INI      = 'INI';
  HL_BAT      = 'BAT';
  HL_RTF      = 'RTF';
  HL_RES      = 'RES';
  HL_PHP      = 'PHP';
  HL_PERL     = 'PERL';
  HL_PY       = 'PY';
  HL_PO       = 'PO';
  HL_HTML     = 'HTML';

  LAYOUT_INI      = 'Ini.hgl';
  LAYOUT_RTF      = 'RTF.hgl';
  LAYOUT_RES      = 'Resource.hgl';
  LAYOUT_BALTALOG = 'Log.hgl';

  // comma seperated lists of supported file extensions (no spaces)
  FILE_EXTENSIONS_TXT  = 'txt,nfo,me';
  FILE_EXTENSIONS_PAS  = 'pas,dpr,pp,lpr,inc';
  FILE_EXTENSIONS_CPP  = 'cpp,h';
  FILE_EXTENSIONS_JAVA = 'java';
  FILE_EXTENSIONS_SQL  = 'sql';
  FILE_EXTENSIONS_XML  = 'xml,hgl,lpi,dproj,groupproj';
  FILE_EXTENSIONS_LFM  = 'dfm,lfm';
  FILE_EXTENSIONS_INI  = 'ini,fpd';
  FILE_EXTENSIONS_BAT  = 'bat,cmd';
  FILE_EXTENSIONS_RTF  = 'rtf';
  FILE_EXTENSIONS_RES  = 'res';
  FILE_EXTENSIONS_PHP  = 'php';
  FILE_EXTENSIONS_PERL = '';
  FILE_EXTENSIONS_PY   = 'py';
  FILE_EXTENSIONS_HTML = 'html,htm';
  FILE_EXTENSIONS_PO   = 'po';

//-----------------------------------------------------------------------------

resourcestring
  STextNotFound = 'Text not found';
  SSearching    = 'Searching...';

  SINIDescription      = 'INI';
  SBATDescription      = 'Windows batch script';
  SPODescription       = 'Gettext translation strings';
  SRTFDescription      = 'Rich Text Format document';
  STXTDescription      = 'Text document';
  SXMLDescription      = 'XML document';
  SHTMLDescription     = 'HTML document';
  SPASDescription      = 'Object Pascal';
  SSQLDescription      = 'SQL';
  SCPPDescription      = 'C++';
  SJavaDescription     = 'Java';
  SLFMDescription      = 'Object Pascal form definition';
  SBaltaLOGDescription = 'Balta log';
  SRESDescription      = 'Windows resources';
  SPHPDescription      = 'PHP';
  SPERLDescription     = 'Perl';
  SPYDescription       = 'Python';

  SAskSaveChanges = 'File is modified. Do you want to save changes?';

//*****************************************************************************

implementation

end.

