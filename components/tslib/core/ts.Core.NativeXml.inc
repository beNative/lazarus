{ ts.Core.NativeXml.inc

  Original name: simdesign.inc

  include file for many simdesign projects
  default path: \simlib\general

  Author: Nils Haeck M.Sc.
  Copyright (c) 2007 - 2011 Simdesign B.V.

}

// unicode avoid implicit string cast warning
{$IFDEF UNICODE}
{$WARN IMPLICIT_STRING_CAST OFF}
{$ENDIF UNICODE}

// Delphi and FPC versions

// Freepascal
{$IFDEF FPC}
  {$MODE Delphi}
  {$DEFINE D7UP}
{$ENDIF FPC}

// Delphi 5
{$IFDEF VER130}
  {$DEFINE D5UP}
{$ENDIF}

//Delphi 6
{$IFDEF VER140}
  {$DEFINE D5UP}
{$ENDIF}

//Delphi 7
{$IFDEF VER150}
  {$DEFINE D7UP}
{$ENDIF}

//Delphi 8
{$IFDEF VER160}
  {$DEFINE D7UP}
{$ENDIF}

// Delphi 2005 / 9
{$IFDEF VER170}
  {$DEFINE D7UP}
{$ENDIF}

// Delphi 2006 / 10
{$IFDEF VER180}
  {$DEFINE D7UP}
  // D10 publishes OnMouseEnter / OnMouseLeave
  {$DEFINE D10UP}
{$ENDIF}

// Delphi 2007 Rad studio / 11?
{$IFDEF VER185}
  {$DEFINE D7UP}
  {$DEFINE D10UP}
{$ENDIF}

// Delphi 2007 - NET / 11?
{$IFDEF VER190}
  {$DEFINE D7UP}
  {$DEFINE D10UP}
{$ENDIF}

// Delphi 2009 / 12
// first UNICODE version, so then directive UNICODE is defined, no need for directive D12UP
{$IFDEF VER200}
  {$DEFINE D7UP}
  {$DEFINE D10UP}
{$ENDIF}

// Delphi 2010 / 14?
{$IFDEF VER210}
  {$DEFINE D7UP}
  {$DEFINE D10UP}
{$ENDIF}

// Delphi XE / 15
{$IFDEF VER220}
  {$DEFINE D7UP}
  {$DEFINE D10UP}
  {$DEFINE D15UP}
{$ENDIF}

// Delphi XE2 / 16
{$IFDEF VER230}
  {$DEFINE D7UP}
  {$DEFINE D10UP}
  {$DEFINE D15UP}
{$ENDIF}

// Delphi XE3 / 17
{$IFDEF VER240}
  {$DEFINE D7UP}
  {$DEFINE D10UP}
  {$DEFINE D15UP}
{$ENDIF}


