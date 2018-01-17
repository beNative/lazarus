{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Core.BRRE;
(**********************************************************
** BRRE Regular Expression Library                        *
***********************************************************
**
** This file is part of the BRRE Regular Expression Library.
** Copyright (C) 2011-2012 by Benjamin Rosseaux
**
** See the file COPYING.BRRE, included in this distribution,
** for details about the copyright.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
**
*)
// Define {$DEFINE BRREStrictUTF8} if you do want strict valid UTF8 !
{$DEFINE BRREUseVirtualStack}
{$IFDEF FPC}
 {$MODE DELPHI}
 {$IFDEF cpui386}
  {$DEFINE cpu386}
 {$ENDIF}
 {$IFDEF cpu386}
  {$asmmode intel}
 {$ENDIF}
 {$IFDEF cpuamd64}
  {$asmmode intel}
 {$ENDIF}
 {$IFDEF FPC_LITTLE_ENDIAN}
  {$DEFINE LITTLE_ENDIAN}
 {$ELSE}
  {$IFDEF FPC_BIG_ENDIAN}
   {$DEFINE BIG_ENDIAN}
  {$ENDIF}
 {$ENDIF}
 {-$pic off}
 {$DEFINE caninline}
 {$IFDEF FPC_HAS_TYPE_EXTENDED}
  {$DEFINE HAS_TYPE_EXTENDED}
 {$ELSE}
  {$undef HAS_TYPE_EXTENDED}
 {$ENDIF}
 {$IFDEF FPC_HAS_TYPE_DOUBLE}
  {$DEFINE HAS_TYPE_DOUBLE}
 {$ELSE}
  {$undef HAS_TYPE_DOUBLE}
 {$ENDIF}
 {$IFDEF FPC_HAS_TYPE_SINGLE}
  {$DEFINE HAS_TYPE_SINGLE}
 {$ELSE}
  {$undef HAS_TYPE_SINGLE}
 {$ENDIF}
{$ELSE}
 {$realcompatibility off}
 {$localsymbols on}
 {$DEFINE LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$DEFINE cpu32}
 {$ENDIF}
 {$DEFINE HAS_TYPE_EXTENDED}
 {$DEFINE HAS_TYPE_DOUBLE}
 {$DEFINE HAS_TYPE_SINGLE}
{$ENDIF}
{$IFDEF win32}
 {$DEFINE windows}
{$ENDIF}
{$IFDEF win64}
 {$DEFINE windows}
{$ENDIF}
{$IFDEF wince}
 {$DEFINE windows}
{$ENDIF}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}

{$DEFINE BRRERegExpUseAssert}

interface

uses SysUtils,Classes,SyncObjs,

     ts.Core.BRREUnicode;

const BRREInitialized:boolean=false;

      BRREVersion=4;

      BRREVersionStr='1.00.2012.11.02.0022';
      BRREVersionString:ansistring=BRREVersionStr;

      brrefDELIMITERS=1 shl 0;
      brrefBACKTRACKING=1 shl 1;
      brrefFREESPACING=1 shl 2;
      brrefIGNORECASE=1 shl 3;
      brrefSINGLELINE=1 shl 4;
      brrefMULTILINE=1 shl 5;
      brrefLATIN1=1 shl 6;
      brrefUTF8=1 shl 7;
      brrefUTF8CODEUNITS=1 shl 8;
      brrefNAMED=1 shl 9;
      brrefCONTINUATION=1 shl 10;
      brrefAUTO=1 shl 11;
      brrefWHOLEONLY=1 shl 12;
      brrefCOMBINEOPTIMIZATION=1 shl 13;
      brrefUNGREEDY=1 shl 14;
      brrefLONGEST=1 shl 15;
      brrefNFA=1 shl 16;
      brrefDFA=1 shl 17;
      brrefNOBACKTRACKING=1 shl 18;
      brrefPREFILTER=1 shl 19;
      brrefPREFILTEROPT=1 shl 20;
      brrefAUTOCALLOUT=1 shl 21;
      brrefNOTEMPTY=1 shl 22;
      brrefRANGE=1 shl 23;

      brreMaxDFAStates=4096;

      brrefrNOINSTANCE=-$7fffffff;

      brreoskALT=0;
      brreoskPLUS=1;
      brreoskQUEST=2;
      brreoskSTAR=3;

      brreoNONE=0;
      brreoCHAR=1;
      brreoCHARSINGLE=2;
      brreoANY=3;
      brreoMATCH=4;
      brreoFAIL=5;
      brreoJMP=6;
      brreoSPLIT=7;
      brreoSPLITLONGEST=8;
      brreoBEGIN=9;
      brreoEND=10;
      brreoBOML=11;
      brreoEOML=12;
      brreoBOSL=13;
      brreoEOSL=14;
      brreoBOS=15;
      brreoEOS=16;
      brreoBRK=17;
      brreoNBRK=18;
      brreoEXACT=19;
      brreoGREEDY=20;
      brreoLAZY=21;
      brreoPOSSESSIVE=22;
      brreoSTOP=23;
      brreoBACKREF=24;
      brreoBACKREFI=25;
      brreoTESTBACKREF=26;
      brreoTEST=27;
      brreoCONTINUE=28;
      brreoLOOKAHEADFALSE=29;
      brreoLOOKAHEADTRUE=30;
      brreoLOOKBEHINDFALSE=31;
      brreoLOOKBEHINDTRUE=32;
      brreoPARALLEL=33;
      brreoONEPASS=34;
      brreoFIXEDSTRINGSEARCH=35;
      brreoPREFIX=36;
      brreoPREFIXCHAR=37;
      brreoCALLOUT=38;
      brreoRECURSIVE=39;

      brreoUNDEFINED=longint(-1);

      brrecINFINITY=-1;

      brreucrWORDS=0;
      brreucrDIGITS=1;
      brreucrWHITESPACES=2;
      brreucrLAST=brreucrWHITESPACES;

      brresuDONOTKNOW=-1;
      brresuNOUTF8=0;
      brresuPOSSIBLEUTF8=1;
      brresuISUTF8=2;

      brreucACCEPT=0;
      brreucERROR=16;

      brreqkGREEDY=0;
      brreqkLAZY=1;
      brreqkPOSSESSIVE=2;

      BRREHashMapItemsPerBucketsThreshold=5;
      BRREHashMapMaxSize=1 shl 28;

      brrefssmBRUTEFORCE=0;
      brrefssmSHIFTOR=1;
      brrefssmBOYERMOORE=2;

      brrepmBACKTRACKINGNFA=0;
      brrepmPARALLELNFA=1;
      brrepmONEPASSNFA=2;
      brrepmFIXEDSTRINGSEARCH=3;

      brresfEmptyBeginLine=1 shl 0;
      brresfEmptyEndLine=1 shl 1;
      brresfEmptyBeginText=1 shl 2;
      brresfEmptyEndText=1 shl 3;
      brresfEmptyWordBoundary=1 shl 4;
      brresfEmptyNonWordBoundary=1 shl 5;
      brresfEmptyAllFlags=(1 shl 6)-1;
      brresfIndexShift=16;
      brresfEmptyShift=6;
      brresfRealCapShift=brresfEmptyShift+1;
      brresfRealMaxCap=((brresfIndexShift-brresfRealCapShift) shr 1)*2;
      brresfCapShift=brresfRealCapShift;
      brresfMaxCap=brresfRealMaxCap;
      brresfMatchWins=1 shl brresfEmptyShift;
      brresfCapMask=((1 shl brresfRealMaxCap)-1) shl brresfRealCapShift;
      brresfImpossible=brresfEmptyWordBoundary or brresfEmptyNonWordBoundary;
      brresfDFAMatchWins=$1000;
      brresfDFALastWord=$2000;
      brresfDFAStart=$8000;
      brresfDFANeedShift=16;
      brresfDFAStartShifted=$80000000;

      brreDFAError=-1;
      brreDFAFail=0;
      brreDFAMatch=1;

      brredmNONE=0;
      brredmFAST=1;
      brredmFULL=2;

      brrecorFAIL=0;
      brrecorOK=1;

type EBRRESyntaxError=class(Exception);

     EBRRENFACollectError=class(Exception);

     EBRREMatchLimitOverflow=class(Exception);

     EBRREMatchLimitRecursionOverflow=class(Exception);

     EBRREMatchLimitStackRecursionOverflow=class(Exception);

     PBRREUnicodeCharRange=^TBRREUnicodeCharRange;
     TBRREUnicodeCharRange=array[0..1] of longword;

     PBRREUnicodeCharRanges=^TBRREUnicodeCharRanges;
     TBRREUnicodeCharRanges=array of TBRREUnicodeCharRange;

     PBRREUnicodeCharRangeClasses=^TBRREUnicodeCharRangeClasses;
     TBRREUnicodeCharRangeClasses=array[0..brreucrLAST] of TBRREUnicodeCharRanges;

     TBRREUTF8String=ansistring;

     TBRREUCS4Char=longint;

     TBRREUCS4CharSigned=longword;

     TBRREUCS4String=array of TBRREUCS4Char;

{$ifndef fpc}
{$if Declared(CompilerVersion) and (CompilerVersion>=23.0)}
     qword=uint64;

     ptruint=NativeUInt;
     ptrint=NativeInt;
{$ELSE}
     qword=int64;

{$IFDEF cpu64}
     ptruint=qword;
     ptrint=int64;
{$ELSE}
     ptruint=longword;
     ptrint=longint;
{$ENDIF}
{$ifend}
{$ENDIF}

     PBRRELongints=^TBRRELongints;
     TBRRELongints=array[0..65535] of longint;

     TBRRERegExp=class;

     PBRRECalloutBlock=^TBRRECalloutBlock;
     TBRRECalloutBlock=packed record
      Version:longword;
      Instance:TBRRERegExp;
      CalloutNumber:longint;
      CalloutData:pointer;
      Subject:pansichar;
      SubjectUnitsLength:longint;
      SubjectPointsLength:longint;
      Captures:pointer;
      CountCaptures:longint;
      CodeUnit:longint;
      CodePoint:longint;
     end;

     TBRRECalloutFunc=function(CalloutBlock:PBRRECalloutBlock):longint; cdecl;

     PBRREByteMap=^TBRREByteMap;
     TBRREByteMap=array[byte] of byte;

     TBRRERegExpCharClass=class;

     PBRREOnePassStateCharClassAction=^TBRREOnePassStateCharClassAction;
     TBRREOnePassStateCharClassAction=record // 32-bit: 16 bytes, 64-bit: 32 bytes
      AllNext,Next:PBRREOnePassStateCharClassAction;
      CharClass:TBRRERegExpCharClass;
      Condition:ptruint;
     end;

     PBRREOnePassState=^TBRREOnePassState;
     TBRREOnePassState=record
      MatchCondition,NoAction:longword;
      CharClassAction:PBRREOnePassStateCharClassAction;
      Action:array[0..0] of longword;
     end;

     PBRREStringHashMapEntity=^TBRREStringHashMapEntity;
     TBRREStringHashMapEntity=record
      Key:ansistring;
      Value:int64;
     end;

     TBRREStringHashMapEntities=array of TBRREStringHashMapEntity;

     TBRREStringHashMapEntityIndices=array of longint;

     TBRREStringHashMap=class
      private
       function FindCell(const Key:ansistring):longword;
       procedure Resize;
      protected
       function GetValue(const Key:ansistring):int64;
       procedure SetValue(const Key:ansistring;const Value:int64);
      public
       RealSize:longint;
       LogSize:longint;
       Size:longint;
       Entities:TBRREStringHashMapEntities;
       EntityToCellIndex:TBRREStringHashMapEntityIndices;
       CellToEntityIndex:TBRREStringHashMapEntityIndices;
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function Add(const Key:ansistring;Value:int64):PBRREStringHashMapEntity;
       function Get(const Key:ansistring;CreateIfNotExist:boolean=false):PBRREStringHashMapEntity;
       function Delete(const Key:ansistring):boolean;
       property Values[const Key:ansistring]:int64 read GetValue write SetValue; default;
     end;

     TBRREHashableObject=class
      public
       function HashCode:longword; virtual;
       function EqualsTo(OtherObject:TBRREHashableObject):boolean; virtual;
     end;

     PBRREHashMapEntity=^TBRREHashMapEntity;
     TBRREHashMapEntity=record
      Key:TBRREHashableObject;
      Value:int64;
     end;

     TBRREHashMapEntities=array of TBRREHashMapEntity;

     TBRREHashMapEntityIndices=array of longint;

     TBRREHashMap=class
      private
       function FindCell(const Key:TBRREHashableObject):longword;
       procedure Resize;
      protected
       function GetValue(const Key:TBRREHashableObject):int64;
       procedure SetValue(const Key:TBRREHashableObject;const Value:int64);
      public
       RealSize:longint;
       LogSize:longint;
       Size:longint;
       Entities:TBRREHashMapEntities;
       EntityToCellIndex:TBRREHashMapEntityIndices;
       CellToEntityIndex:TBRREHashMapEntityIndices;
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function Add(const Key:TBRREHashableObject;Value:int64):PBRREHashMapEntity;
       function Get(const Key:TBRREHashableObject;CreateIfNotExist:boolean=false):PBRREHashMapEntity;
       function Delete(const Key:TBRREHashableObject):boolean;
       property Values[const Key:TBRREHashableObject]:int64 read GetValue write SetValue; default;
     end;

     PBRREPointerArray=^TBRREPointerArray;
     TBRREPointerArray=array[0..(2147483647 div sizeof(pointer))-1] of pointer;

     TBRREPointerList=class
      private
       List:PBRREPointerArray;
       ListCount,ListCapacity:longint;
      protected
       function GetItem(Index:longint):pointer;
       procedure SetItem(Index:longint;Value:pointer);
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function Add(Item:pointer):longint;
       function Push(Item:pointer):longint;
       function Pop:pointer;
       procedure Insert(Index:longint;Item:pointer);
       function Find(Item:pointer):longint;
       function IndexOf(Item:pointer):longint;
       function Contains(Item:pointer):boolean;
       procedure Delete(Index:longint);
       function Remove(Item:pointer):longint;
       procedure Exchange(Index1,Index2:longint);
       procedure SetCapacity(NewCapacity:longint);
       procedure SetCount(NewCount:longint);
       property Count:longint read ListCount write SetCount;
       property Capacity:longint read ListCapacity write SetCapacity;
       property Items[Index:longint]:pointer read GetItem write SetItem; default;
     end;

     PBRREPtrValueSelfBalancedTreeValue=^TBRREPtrValueSelfBalancedTreeValue;
     TBRREPtrValueSelfBalancedTreeValue=ptrint;

     PBRREPtrValueSelfBalancedTreeNode=^TBRREPtrValueSelfBalancedTreeNode;
     TBRREPtrValueSelfBalancedTreeNode=record
      Parent,Left,Right,PreviousKey,NextKey:PBRREPtrValueSelfBalancedTreeNode;
      Level:int64;
      Key:ptruint;
      Value:TBRREPtrValueSelfBalancedTreeValue;
     end;

     TBRREPtrValueSelfBalancedTreeKeys=array of ptruint;

     TBRREPtrValueSelfBalancedTree=class
      protected
       procedure Skew(OldParent:PBRREPtrValueSelfBalancedTreeNode);
       function Split(OldParent:PBRREPtrValueSelfBalancedTreeNode):boolean;
       procedure RebalanceAfterLeafAdd(n:PBRREPtrValueSelfBalancedTreeNode);
       procedure DeleteNode(n:PBRREPtrValueSelfBalancedTreeNode); 
       function First(StartNode:PBRREPtrValueSelfBalancedTreeNode):PBRREPtrValueSelfBalancedTreeNode;
       function Next(n:PBRREPtrValueSelfBalancedTreeNode):PBRREPtrValueSelfBalancedTreeNode;
       function FindNode(const Key:ptruint):PBRREPtrValueSelfBalancedTreeNode;
       procedure ClearNode(var Node:PBRREPtrValueSelfBalancedTreeNode);
       procedure OptimizeNode(var Node:PBRREPtrValueSelfBalancedTreeNode;MoreOptimize:boolean);
       function GetValue(Key:ptruint):TBRREPtrValueSelfBalancedTreeValue;
       procedure SetValue(Key:ptruint;Value:TBRREPtrValueSelfBalancedTreeValue);
      public
       RootNode:PBRREPtrValueSelfBalancedTreeNode;
       FirstKey,LastKey:PBRREPtrValueSelfBalancedTreeNode;
       constructor Create;
       destructor Destroy; override;
       function Find(const Key:ptruint;var Value:TBRREPtrValueSelfBalancedTreeValue):boolean;
       function Insert(const Key:ptruint;Value:TBRREPtrValueSelfBalancedTreeValue):PBRREPtrValueSelfBalancedTreeNode;
       procedure Remove(const Key:ptruint);
       procedure Optimize;
       function Keys:TBRREPtrValueSelfBalancedTreeKeys;
       property Values[Key:ptruint]:TBRREPtrValueSelfBalancedTreeValue read GetValue write SetValue; default;
     end;

     TBRRERegExpPrefilterNode=class;

     TBRRERegExpPrefilterNodeList=class(TBRREPointerList)
      protected
       function GetNodeItem(Index:longint):TBRRERegExpPrefilterNode;
       procedure SetNodeItem(Index:longint;Value:TBRRERegExpPrefilterNode);
      public
       property Items[Index:longint]:TBRRERegExpPrefilterNode read GetNodeItem write SetNodeItem; default;
     end;

     TBRRERegExpPrefilterNodeOperation=(brrepfnoANY,brrepfnoATOM,brrepfnoAND,brrepfnoOR);

     TBRRERegExpPrefilterNode=class
      public
       Operation:TBRRERegExpPrefilterNodeOperation;
       Subs:TBRRERegExpPrefilterNodeList;
       Atom:ansistring;
       Exact:boolean;
       constructor Create;
       destructor Destroy; override;
       function Clone:TBRRERegExpPrefilterNode;
       function Expression:ansistring;
       function ShortExpression:ansistring;
       function SQLBooleanFullTextExpression:ansistring;
       function SQLExpression(const Field:ansistring):ansistring;
     end;

     TBRRERegExpOpcode=byte;

     TBRRERegExpFlags=longword;

     TBRRERegExpCharClassRange=class
      public
       CharClass:TBRRERegExpCharClass;
       Previous,Next,Left,Right:TBRRERegExpCharClassRange;
       Lo,Hi:longword;
       constructor Create(ACharClass:TBRRERegExpCharClass;ALo,AHi:longword);
       constructor CreateBefore(ACharClass:TBRRERegExpCharClass;ABefore:TBRRERegExpCharClassRange;ALo,AHi:longword);
       constructor CreateAfter(ACharClass:TBRRERegExpCharClass;AAfter:TBRRERegExpCharClassRange;ALo,AHi:longword);
       destructor Destroy; override;
     end;

     TBRRERegExpCharClassCharSet=set of ansichar;

     TBRRERegExpCharClassRanges=array of TBRRERegExpCharClassRange;

     TBRRERegExpCharClass=class(TBRREHashableObject)
      public
       RegExp:TBRRERegExp;
       Previous,Next:TBRRERegExpCharClass;
       First,Last,Root:TBRRERegExpCharClassRange;
       CharSet:TBRRERegExpCharClassCharSet;
       Inverted:longbool;
       Canonicalized:longbool;
       constructor Create(ARegExp:TBRRERegExp);
       destructor Destroy; override;
       procedure Clear;
       procedure Dump;
       procedure DebugDump;
       procedure Optimize;
       procedure AddRange(Lo,Hi:longword;IgnoreCase:boolean=false);
       procedure AddChar(c:longword;IgnoreCase:boolean=false);
       procedure AddUnicodeCategory(CategoryFlags:longword;IgnoreCase:boolean=false);
       procedure AddUnicodeScript(Script:longword;IgnoreCase:boolean=false);
       procedure AddUnicodeBlock(Block:longword;IgnoreCase:boolean=false);
       procedure Combine(From:TBRRERegExpCharClass);
       function Subtraction(From,SubtractWith:TBRRERegExpCharClass):boolean;
       function Intersection(From,SubtractWith:TBRRERegExpCharClass):boolean;
       procedure TakeoverCombine(From:TBRRERegExpCharClass);
       procedure TakeoverDirtyCombine(From:TBRRERegExpCharClass);
       procedure Assign(From:TBRRERegExpCharClass);
       procedure Append(From:TBRRERegExpCharClass);
       procedure Invert;
       procedure Canonicalize;
       procedure CompileBinarySearchTree;
       procedure CompileCharset;
       procedure Finalize;
       function Count:longword;
       function Contains(c:longword):boolean;
       function Intersects(Other:TBRRERegExpCharClass):boolean;
       function IsSingle:boolean;
       function HashCode:longword; override;
       function EqualsTo(OtherObject:TBRREHashableObject):boolean; override;
     end;

     TBRRERegExpCharClasses=class(TBRREPointerList)
      protected
       function GetNodeItem(Index:longint):TBRRERegExpCharClass;
       procedure SetNodeItem(Index:longint;Value:TBRRERegExpCharClass);
      public
       property Items[Index:longint]:TBRRERegExpCharClass read GetNodeItem write SetNodeItem; default;
     end;

     TBRRERegExpCapture=record
      StartCodePoint,EndCodePoint:longint;
      StartCodeUnit,EndCodeUnit:longint;
     end;

     TBRRERegExpCaptures=array of TBRRERegExpCapture;

     TBRRERegExpMultipleCaptures=array of TBRRERegExpCaptures;

     TBRRERegExpStrings=array of ansistring;

     TBRRERegExpMultipleStrings=array of TBRRERegExpStrings;

     TBRRERegExpCaptureStart=record
      StartCodePoint,StartCodeUnit:longint;
     end;

     TBRRERegExpCaptureStarts=array of TBRRERegExpCaptureStart;

     TBRRERegExpValue=longint;

     PBRRERegExpNFAInstruction=^TBRRERegExpNFAInstruction;

     TPBRRERegExpNFAInstructions=array of PBRRERegExpNFAInstruction;

     PPBRRERegExpNFAInstructionsStatic=^TPBRRERegExpNFAInstructionsStatic;
     TPBRRERegExpNFAInstructionsStatic=array[0..65535] of PBRRERegExpNFAInstruction;

     TBRRERegExpNFAInstruction=record // on 32-bit: 16 bytes, on 64-bit: 32 bytes
      IDandOpcode:ptruint; // first 8-bit = Opcode, remain bits = ID
      Value:ptrint; // Value can be also a PBRRERegExpNFAInstruction pointer at the brreoTEST opcode, so ptrint as datatype
      x,y:PBRRERegExpNFAInstruction;
     end;

     TBRRERegExpNFAInstructions=array of TBRRERegExpNFAInstruction;

     PBRRERegExpState=^TBRRERegExpState;
     TBRRERegExpState=record
      Captures:TBRRERegExpCaptures;
      ReferenceCount:longint;
      NextFree:PBRRERegExpState;
      NextUsed:PBRRERegExpState;
     end;

     PBRREParallelNFAThread=^TBRREParallelNFAThread;
     TBRREParallelNFAThread=record
      Instruction:PBRRERegExpNFAInstruction;
      State:PBRRERegExpState;
     end;

     PBRREParallelNFAThreadsStatic=^TBRREParallelNFAThreadsStatic;
     TBRREParallelNFAThreadsStatic=array[0..65535] of TBRREParallelNFAThread;

     TBRREParallelNFAThreads=array of TBRREParallelNFAThread;

     TBRRERegExpByteSet=set of byte;

     PBRREParallelNFAThreadList=^TBRREParallelNFAThreadList;
     TBRREParallelNFAThreadList=record
      Threads:TBRREParallelNFAThreads;
      Count:longint;
      GenerationID:ptruint;
     end;

     TBRRERegExpShiftOrPatternMaskBoyerMooreSkip=array[ansichar] of longword;

     TBRRERegExpBoyerMooreNext=array of longint;

     TBRRERegExpOnePassCaptures=array of longint;

     PBRREDFAState=^TBRREDFAState;

     TPBRREDFAStates=array of PBRREDFAState;

     PPBRREDFANextStatesByteBlock=^TPBRREDFANextStatesByteBlock;
     TPBRREDFANextStatesByteBlock=array[byte] of PBRREDFAState;

     PBRREDFARangePagedNextStates=^TBRREDFARangePagedNextStates;
     TBRREDFARangePagedNextStates=record
      Next:PBRREDFARangePagedNextStates;
      Page:longword;
      NextStates:TPBRREDFANextStatesByteBlock;
     end;

     TBRREDFA=class;

     PBRRERegExpProgramDataInstance=^TBRRERegExpProgramDataInstance;

     TBRREDFAState=record
      Flags:longword;
      Instructions:TPBRRERegExpNFAInstructions;
      CountInstructions:longint;
      RangePagedNextStates:PBRREDFARangePagedNextStates;
      Latin1NextStates:TPBRREDFANextStatesByteBlock;
     end;

     PBRREDFAStateHashMapEntity=^TBRREDFAStateHashMapEntity;
     TBRREDFAStateHashMapEntity=record
      Key:PBRREDFAState;
      Value:int64;
     end;

     TBRREDFAStateHashMapEntities=array of TBRREDFAStateHashMapEntity;

     TBRREDFAStateHashMapEntityIndices=array of longint;

     TBRREDFAStateHashMap=class
      private
       RealSize:longint;
       LogSize:longint;
       Size:longint;
       Entities:TBRREDFAStateHashMapEntities;
       EntityToCellIndex:TBRREDFAStateHashMapEntityIndices;
       CellToEntityIndex:TBRREDFAStateHashMapEntityIndices;
       function FindCell(const Key:PBRREDFAState):longword;
       procedure Resize;
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function Get(const Key:PBRREDFAState):PBRREDFAState;
       function Add(const Key:PBRREDFAState):PBRREDFAStateHashMapEntity;
       function Delete(const Key:PBRREDFAState):boolean;
       property Keys[const Key:PBRREDFAState]:PBRREDFAState read Get; default;
     end;

     TBRRERegExpProgram=class;

     TBRRERegExpDataInstance=class;

     TBRREFixedStringSearch=class
      public
       RegExp:TBRRERegExp;
       RegExpProgram:TBRRERegExpProgram;
       RegExpDataInstance:TBRRERegExpDataInstance;
       RegExpProgramData:PBRRERegExpProgramDataInstance;
       constructor Create(ARegExpProgram:TBRRERegExpProgram;ARegExpDataInstance:TBRRERegExpDataInstance;ARegExpProgramData:PBRRERegExpProgramDataInstance);
       destructor Destroy; override;
       function Search(const Input:ansistring;var CodePoint,CodeUnit:longint):boolean;
       function PtrSearch(const Input:pansichar;var CodePoint,CodeUnit:longint):boolean;
     end;

     PBRREDFAStatePool=^TBRREDFAStatePool;
     TBRREDFAStatePool=record
      Next:PBRREDFAStatePool;
      States:pointer;
      EndState:pointer;
      NextState:pointer;
     end;

     TBRREDFA=class
      private
       StackInstructions:TPBRRERegExpNFAInstructions;
       ByteMap:TBRREByteMap;
       ByteCharSetMap:TBRRERegExpCharClassCharSet;
       PrefixCharRangeLow,PrefixCharRangeHigh:longword;

       procedure ConnectState(State,NextState:PBRREDFAState;CurrentChar:longword;Fast:boolean); {$IFDEF caninline}inline;{$ENDIF}
       function CacheState(State:PBRREDFAState):PBRREDFAState; {$IFDEF caninline}inline;{$ENDIF}

       procedure FastAddInstructionThread(State:PBRREDFAState;Instruction:PBRRERegExpNFAInstruction);
       function FastProcessNextState(State:PBRREDFAState;CurrentChar:longword):PBRREDFAState;
       function SearchFastLatin1(const Input:ansistring):longint;
       function SearchFastLatin1FirstChar(const Input:ansistring):longint;
       function SearchFastUTF8(const Input:ansistring):longint;
       function SearchFastUTF8FirstChar(const Input:ansistring):longint;
       function PtrSearchFastLatin1(const Input:pansichar):longint;
       function PtrSearchFastLatin1FirstChar(const Input:pansichar):longint;
       function PtrSearchFastUTF8(const Input:pansichar):longint;
       function PtrSearchFastUTF8FirstChar(const Input:pansichar):longint;

       procedure FullAddInstructionThread(State:PBRREDFAState;Instruction:PBRRERegExpNFAInstruction;Flags:longword);
       function FullGetStartState(LastChar,CurrentChar:longword):PBRREDFAState;
       function FullProcessNextState(State:PBRREDFAState;CurrentChar:longword):PBRREDFAState;
       function SearchFullLatin1(const Input:ansistring):longint;
       function SearchFullLatin1FirstChar(const Input:ansistring):longint;
       function SearchFullUTF8(const Input:ansistring):longint;
       function SearchFullUTF8FirstChar(const Input:ansistring):longint;
       function PtrSearchFullLatin1(const Input:pansichar):longint;
       function PtrSearchFullLatin1FirstChar(const Input:pansichar):longint;
       function PtrSearchFullUTF8(const Input:pansichar):longint;
       function PtrSearchFullUTF8FirstChar(const Input:pansichar):longint;

      public
       RegExp:TBRRERegExp;
       RegExpProgram:TBRRERegExpProgram;
       StateCache:TBRREDFAStateHashMap;
       StartState:PBRREDFAState;
       TemporaryState,NewState:TBRREDFAState;
       StartStates:array[0..brresfEmptyAllFlags] of PBRREDFAState;
       CountStatesCached:longint;
       RegExpDataInstance:TBRRERegExpDataInstance;
       RegExpProgramData:PBRRERegExpProgramDataInstance;
       StartCodePoint,StartCodeUnit,EndCodePoint,EndCodeUnit:longint;
       NextStatesSize,DFAStateSize:ptrint;
       StatePoolUsed,StatePoolFree:PBRREDFAStatePool;
       StatePoolSize,StatePoolSizePowerOfTwo:ptruint;
       constructor Create(ARegExpProgram:TBRRERegExpProgram;ARegExpDataInstance:TBRRERegExpDataInstance;ARegExpProgramData:PBRRERegExpProgramDataInstance);
       destructor Destroy; override;
       procedure DestroyStatePool(StatePool:PBRREDFAStatePool);
       procedure FreeUsedStatePool;
       function AllocateNewStatePool:PBRREDFAStatePool;
       function GetState:PBRREDFAState;
       function TakeoverState(TakeoverFrom:PBRREDFAState):PBRREDFAState;
       procedure FreeState(State:PBRREDFAState);
       procedure Reset;
       function Search(const Input:ansistring;var AStartCodePoint,AStartCodeUnit,AEndCodePoint,AEndCodeUnit:longint):longint;
       function PtrSearch(const Input:pansichar;var AStartCodePoint,AStartCodeUnit,AEndCodePoint,AEndCodeUnit:longint):longint;
     end;

     TBRREOnePassNFA=class
      private
       StartCodePoint,EndCodePoint:longint;
       StartCodeUnit,EndCodeUnit:longint;
       WorkCaptures:TBRRERegExpOnePassCaptures;
       MatchCaptures:TBRRERegExpOnePassCaptures;
       procedure CopyWorkCapturesToMatchCaptures(Count:longint); {$IFDEF caninline}inline;{$ENDIF}
       function SearchLatin1(const Input:ansistring;var CodePoint,CodeUnit:longint):boolean;
       function SearchUTF8(const Input:ansistring;var CodePoint,CodeUnit:longint):boolean;
       function PtrSearchLatin1(const Input:pansichar;var CodePoint,CodeUnit:longint):boolean;
       function PtrSearchUTF8(const Input:pansichar;var CodePoint,CodeUnit:longint):boolean;
      public
       RegExp:TBRRERegExp;
       RegExpProgram:TBRRERegExpProgram;
       RegExpDataInstance:TBRRERegExpDataInstance;
       RegExpProgramData:PBRRERegExpProgramDataInstance;
       constructor Create(ARegExpProgram:TBRRERegExpProgram;ARegExpDataInstance:TBRRERegExpDataInstance;ARegExpProgramData:PBRRERegExpProgramDataInstance);
       destructor Destroy; override;
       function Search(const Input:ansistring;var CodePoint,CodeUnit:longint):boolean;
       function PtrSearch(const Input:pansichar;var CodePoint,CodeUnit:longint):boolean;
     end;

     PBRREBitstateNFAJob=^TBRREBitstateNFAJob;
     TBRREBitstateNFAJob=record
      Instruction:PBRRERegExpNFAInstruction;
      CodePoint,CodeUnit,Argument:longint;
     end;

     TBRREBitstateNFAJobs=array of TBRREBitstateNFAJob;

     TBRREBitstateNFAVisited=array of longword;

     TBRREBitstateNFA=class
      private
       function ShouldVisit(Instruction:PBRRERegExpNFAInstruction;CodeUnit:longint):boolean;
       procedure Push(Instruction:PBRRERegExpNFAInstruction;CodePoint,CodeUnit,Argument:longint);
      public
       RegExp:TBRRERegExp;
       RegExpProgram:TBRRERegExpProgram;
       RegExpDataInstance:TBRRERegExpDataInstance;
       RegExpProgramData:PBRRERegExpProgramDataInstance;
       Visited:TBRREBitstateNFAVisited;
       CountVisited:longint;
       Jobs:TBRREBitstateNFAJobs;
       CountJobs:longint;
       MaxJob:longint;
       Len:longint;
       BaseCodeUnit:longint;
       constructor Create(ARegExpProgram:TBRRERegExpProgram;ARegExpDataInstance:TBRRERegExpDataInstance);
       destructor Destroy; override;
       function Search(const Input:ansistring;var CodePoint,CodeUnit:longint;ALen,ABaseCodeUnit,EndCodeUnit:longint):boolean;
       function PtrSearch(const Input:pansichar;var CodePoint,CodeUnit:longint;ALen,ABaseCodeUnit,EndCodeUnit:longint):boolean;
     end;

     TBRREParallelNFA=class
      private
       StackThreads:TBRREParallelNFAThreads;
       States:TBRREPointerList;
       FreeState:PBRRERegExpState;
       UsedState:PBRRERegExpState;
       InstanceCurrentList:PBRREParallelNFAThreadList;
       InstanceNewList:PBRREParallelNFAThreadList;
       InputStr:ansistring;
       InputPtr:pansichar;
       CountOfStateCaptures,CaptureOffset:longint;
       function StateNew(Count:longint):PBRRERegExpState;
       procedure StateRelease(State:PBRRERegExpState);
       procedure ThreadAdd(List:PBRREParallelNFAThreadList;Thread:TBRREParallelNFAThread;CodePoint,CodeUnit:longint;CurrentWorkChar:longword);
       procedure PtrThreadAdd(List:PBRREParallelNFAThreadList;Thread:TBRREParallelNFAThread;CodePoint,CodeUnit:longint;CurrentWorkChar:longword);
      public
       RegExp:TBRRERegExp;
       RegExpProgram:TBRRERegExpProgram;
       RegExpDataInstance:TBRRERegExpDataInstance;
       RegExpProgramData:PBRRERegExpProgramDataInstance;
       constructor Create(ARegExpProgram:TBRRERegExpProgram;ARegExpDataInstance:TBRRERegExpDataInstance;ARegExpProgramData:PBRRERegExpProgramDataInstance);
       destructor Destroy; override;
       function Search(const AInput:ansistring;var CodePoint,CodeUnit:longint):boolean;
       function PtrSearch(const AInput:pansichar;var CodePoint,CodeUnit:longint):boolean;
     end;

     PBRRERegExpBacktrackingStackItem=^TBRRERegExpBacktrackingStackItem;
     TBRRERegExpBacktrackingStackItem=record
      CurrentWorkChar:longword;
      Instruction:PBRRERegExpNFAInstruction;
      CurrentCodePoint:longint;
      CurrentCodeUnit:longint;
      Arguments:array[0..3] of longword;
     end;

     PBRRERegExpBacktrackingStackItemsStatic=^TBRRERegExpBacktrackingStackItemsStatic;
     TBRRERegExpBacktrackingStackItemsStatic=array[0..65535] of TBRRERegExpBacktrackingStackItem;

     TBRRERegExpBacktrackingStackItems=array of TBRRERegExpBacktrackingStackItem;

     PBRREBacktrackingNFAMarkStackItem=^TBRREBacktrackingNFAMarkStackItem;
     TBRREBacktrackingNFAMarkStackItem=record
      CodePoint:longint;
      CodeUnit:longint;
      Char:longword;
      AlignDummy:longword;
     end;

     TBRRERegExpMarkStackItems=array of TBRREBacktrackingNFAMarkStackItem;

     TBRREBacktrackingNFA=class
      private
       InputStr:ansistring;
       InputPtr:pansichar;
       InputPointsLength:longint;
       InputUnitsLength:longint;
       InputIsUTF8:boolean;
       BacktrackingStack:TBRRERegExpBacktrackingStackItems;
       BacktrackingStackPointer:longint;
       MarkStack:TBRRERegExpMarkStackItems;
       MarkStackPointer:longint;
       MatchCounter:longint;
       AllFirstCharsClass:TBRRERegExpCharClass;
       MaxMatchLimit:longint;
       MaxMatchLimitRecursion:longint;
       MaxMatchLimitStackRecursion:longint;
       procedure MarkStackPush(CurrentCodePoint,CurrentCodeUnit:longint;CurrentChar:longword); {$IFDEF caninline}inline;{$ENDIF}
       function MarkStackPop(TopStack:longint;var MarkStackItem:TBRREBacktrackingNFAMarkStackItem):boolean; {$IFDEF caninline}inline;{$ENDIF}
       function GreedyRepeat(var CurrentWorkChar:longword;Instruction:PBRRERegExpNFAInstruction;var CurrentCodePoint,CurrentCodeUnit:longint;Count,Level:longint):boolean;
       function PtrGreedyRepeat(var CurrentWorkChar:longword;Instruction:PBRRERegExpNFAInstruction;var CurrentCodePoint,CurrentCodeUnit:longint;Count,Level:longint):boolean;
       function Push(Instruction:PBRRERegExpNFAInstruction;CodePoint,CodeUnit:longint;WorkChar:longword):PBRRERegExpBacktrackingStackItem; {$IFDEF caninline}inline;{$ENDIF}
       function ExecuteLevel(var CurrentWorkChar:longword;Instruction:PBRRERegExpNFAInstruction;CurrentCodePoint,CurrentCodeUnit:longint;var OutCodePoint,OutCodeUnit:longint;Level:longint):boolean;
       function PtrExecuteLevel(var CurrentWorkChar:longword;Instruction:PBRRERegExpNFAInstruction;CurrentCodePoint,CurrentCodeUnit:longint;var OutCodePoint,OutCodeUnit:longint;Level:longint):boolean;
      public
       RegExp:TBRRERegExp;
       RegExpProgram:TBRRERegExpProgram;
       RegExpDataInstance:TBRRERegExpDataInstance;
       RegExpProgramData:PBRRERegExpProgramDataInstance;
       constructor Create(ARegExpProgram:TBRRERegExpProgram;ARegExpDataInstance:TBRRERegExpDataInstance;ARegExpProgramData:PBRRERegExpProgramDataInstance);
       destructor Destroy; override;
       function Search(const Input:ansistring;var CodePoint,CodeUnit:longint;UseAnchoredStart:boolean):boolean;
       function PtrSearch(const Input:pansichar;var CodePoint,CodeUnit:longint;UseAnchoredStart:boolean):boolean;
     end;

     TBRRERegExpProgram=class
      private
      public
       RegExp:TBRRERegExp;
       ID:longint;
       Instructions:TBRRERegExpNFAInstructions;
       AnchoredInstructions:TBRRERegExpNFAInstructions;
       Start:PBRRERegExpNFAInstruction;
       AnchoredStart:PBRRERegExpNFAInstruction;
       AnchorStartInstruction:PBRRERegExpNFAInstruction;
       AnchorEndInstruction:PBRRERegExpNFAInstruction;
       Method:longint;
       Len,FirstCapture,CountOfCaptures:longint;
       HasRootCapture,IsGlobal,IsOnePass,CanBitstateNFA,CanDFA:boolean;
       ByteMap:TBRREByteMap;
       UnByteMap:TBRREByteMap;
       ByteCharSetMap:TBRRERegExpCharClassCharSet;
       ByteCount:longint;
       OnePassNodes:PBRREOnePassState;
       OnePassNodesCount:longint;
       OnePassStart:PBRREOnePassState;
       OnePassStateSize:longint;
       FixedString:ansistring;
       FixedStringShiftOrPatternMaskBoyerMooreSkip:TBRRERegExpShiftOrPatternMaskBoyerMooreSkip;
       FixedStringBoyerMooreNext:TBRRERegExpBoyerMooreNext;
       FixedStringSearchMethod:longint;
       PrefixIsWholeRegExp:boolean;
       FixedStringLength:longint;
       FixedStringUTF8Length:longint;
       FixedStringIsPrefix:boolean;
       AnchorBeginText:boolean;
       AnchorBeginLine:boolean;
       AnchorStart:boolean;
       AnchorEnd:boolean;
       SearchForFirstChars:boolean;
       DFAMode:longint;
       PrefilterRootNode:TBRRERegExpPrefilterNode;
       CharClassActions:PBRREOnePassStateCharClassAction;
       RangeLow:ansistring;
       RangeHigh:ansistring;
       HasRange:boolean;
       constructor Create(ARegExp:TBRRERegExp);
       destructor Destroy; override;
     end;

     TBRRERegExpDataInstanceVisited=array of longword;

     TBRRERegExpDataInstanceInstructionGenerationIDs=array of ptruint;

     PBRRERegExpDataInstanceInstructionGenerationIDsStatic=^TBRRERegExpDataInstanceInstructionGenerationIDsStatic;
     TBRRERegExpDataInstanceInstructionGenerationIDsStatic=array[0..65535] of ptruint;

     TBRRERegExpProgramDataInstance=record
      FixedStringSearch:TBRREFixedStringSearch;
      OnePassNFA:TBRREOnePassNFA;
      BitstateNFA:TBRREBitstateNFA;
      ParallelNFA:TBRREParallelNFA;
      BacktrackingNFA:TBRREBacktrackingNFA;
      InstructionGenerationIDs:TBRRERegExpDataInstanceInstructionGenerationIDs;
      GenerationID:ptruint;
     end;

     TBRRERegExpProgramDataInstances=array of TBRRERegExpProgramDataInstance;

     TBRRERegExpDataInstance=class
      public
       NextFree:TBRRERegExpDataInstance;
       RegExp:TBRRERegExp;
       Captures:TBRRERegExpCaptures;
       GlobalStartPositions:TBRRERegExpCaptureStarts;
       LastIndexCodeUnit:longint;
       LastIndexCodePoint:longint;
       WorkCaptures:TBRRERegExpCaptures;
       DFA:TBRREDFA;
       InputPointsLength:longint;
       InputUnitsLength:longint;
       InputIsUTF8:boolean;
       RegExpPrograms:TBRRERegExpProgramDataInstances;
       CalloutCaptures:PBRRELongints;
       constructor Create(ARegExp:TBRRERegExp);
       destructor Destroy; override;
       procedure Update;
       function Callout(Number:longint;Subject:pansichar;SubjectUnitsLength,SubjectPointsLength,CodeUnit,CodePoint,CodeUnitOffset:longint):longint;
     end;

     TBRRERegExpFixupableCapture=record
      CaptureIndex:longint;
      Identifier:ansistring;
     end;

     TBRRERegExpFixupableCaptures=array of TBRRERegExpFixupableCapture;

     TBRRERegExpCaptureSnapshot=record
      Count:longint;
      Values:array of longint;
      Identifiers:array of string;
      IdentifierValues:array of longint;
      Nodes:array of pointer;
      Instructions:array of pointer;
      FixupableCaptures:TBRRERegExpFixupableCaptures;
      CountFixupableCaptures:longint;
     end;

     TBRRERegExpASTNodeType=(ntNONE,
                             ntALT,
                             ntCAT,
                             ntSTAR,
                             ntQUEST,
                             ntPLUS,
                             ntPAREN,
                             ntCHAR,
                             ntDOT,
                             ntBOML,
                             ntEOML,
                             ntBOSL,
                             ntEOSL,
                             ntBOS,
                             ntEOS,
                             ntBRK,
                             ntNBRK,
                             ntEXACT,
                             ntREPEAT,
                             ntSTOP,
                             ntBACKREF,
                             ntBACKREFI,
                             ntTESTBACKREF,
                             ntTEST,
                             ntCONTINUE,
                             ntLOOKAHEADFALSE,
                             ntLOOKAHEADTRUE,
                             ntLOOKBEHINDFALSE,
                             ntLOOKBEHINDTRUE,
                             ntPARALLEL,
                             ntONEPASS,
                             ntFIXEDSTRINGSEARCH,
                             ntCALLOUT,
                             ntRECURSIVE);

     PPBRRERegExpASTNode=^PBRRERegExpASTNode;

     PBRRERegExpASTNode=^TBRRERegExpASTNode;

     TPBRRERegExpASTNodes=array of PBRRERegExpASTNode;

     TBRRERegExpASTNode=record
      NodeType:TBRRERegExpASTNodeType;
      Left,Right,Extra:PBRRERegExpASTNode;
      Value,Min,Max,Index:ptrint;
     end;

     TBRRERegExpCompiler=class
      private
       Nodes:TBRREPointerList;
       IsBacktracking:boolean;
       HasBacktrackingParent:boolean;
       RegExpProgram:TBRRERegExpProgram;
       Instruction:PBRRERegExpNFAInstruction;
       PrefixIsWholeRegExp:boolean;
       Prefix:ansistring;
       function GetFixupableCaptureIndex(const Identifier:ansistring):longint;
       function CreateCaptureSnapshot:TBRRERegExpCaptureSnapshot;
       procedure RestoreCaptureSnapshot(var CaptureSnapshot:TBRRERegExpCaptureSnapshot);
       procedure ReleaseCaptureSnapshot(var CaptureSnapshot:TBRRERegExpCaptureSnapshot);
       procedure ResetCaptures;
       function NextChar:longword;
       function PeekNextChar:longword;
       function SkipFreespacingWhiteSpace:longword;
       procedure SyntaxError;
       procedure Expect(c:longword);
       function ParseInteger:longint;
       function ParseHex:longword;
       function ParseHexValue:longword;
       function QuantifierIsNext:boolean;
       function TestClassPOSIXCharacterClass:boolean;
       function ParseClassPOSIXCharacterClass(var c:TBRRERegExpCharClass;CanBeAlreadyCanonicalized:boolean):boolean;
       function ParseClassEscapeUnicodeProperty(var c:TBRRERegExpCharClass;CanBeAlreadyCanonicalized:boolean):boolean;
       function ParseClassEscape(CanBeAlreadyCanonicalized:boolean):TBRRERegExpCharClass;
       function ParseClassAtom:TBRRERegExpCharClass;
       function ParseCharacterClass:TBRRERegExpCharClass;
       procedure CollectError;
       procedure FixUpFixupableCaptures;
       function NewNode(NodeType:TBRRERegExpASTNodeType;Left,Right,Extra:PBRRERegExpASTNode;Value:longint):PBRRERegExpASTNode;
       function CopyNode(FromNode:PBRRERegExpASTNode):PBRRERegExpASTNode;
       function AreNodesEqual(NodeA,NodeB:PBRRERegExpASTNode):boolean;
       function AreNodesEqualSafe(NodeA,NodeB:PBRRERegExpASTNode):boolean;
       function Concat(NodeLeft,NodeRight:PBRRERegExpASTNode):PBRRERegExpASTNode;
       function NewAlt(NodeLeft,NodeRight:PBRRERegExpASTNode):PBRRERegExpASTNode;
       function NewPlus(Node:PBRRERegExpASTNode;Kind:longint):PBRRERegExpASTNode;
       function NewStar(Node:PBRRERegExpASTNode;Kind:longint):PBRRERegExpASTNode;
       function NewQuest(Node:PBRRERegExpASTNode;Kind:longint):PBRRERegExpASTNode;
       function IsStarNullable(Node:PBRRERegExpASTNode):boolean;
       function StarDenull(Node:PBRRERegExpASTNode):PBRRERegExpASTNode;
       function NewRepeat(Node:PBRRERegExpASTNode;Kind,Min,Max:longint):PBRRERegExpASTNode;
       function ConvertUTF8RangeToLatin1Ranges(Lo,Hi:longword):PBRRERegExpASTNode;
       function ConvertUTF8CharClassNode(Node:PBRRERegExpASTNode):PBRRERegExpASTNode;
       function ParseAtom(var Node:PBRRERegExpASTNode):longint;
       function ParseTerm:PBRRERegExpASTNode;
       function ParseAlternative:PBRRERegExpASTNode;
       function ParseDisjunction(AllowOptimizations:boolean):PBRRERegExpASTNode;
       function CountInstructions(Node:PBRRERegExpASTNode):longint;
       procedure NumberizeInstructions(StartInstruction,EndInstruction:PBRRERegExpNFAInstruction);
       procedure OptimizeInstructions(StartInstruction,EndInstruction:PBRRERegExpNFAInstruction);
       procedure FinalizeInstructions(StartInstruction,EndInstruction:PBRRERegExpNFAInstruction);
       function OptimizeNode(NodeEx:PPBRRERegExpASTNode):boolean;
       function Dump(Node:PBRRERegExpASTNode):ansistring;
       procedure CompileRange(RegExpProgram:TBRRERegExpProgram;StartInstruction:PBRRERegExpNFAInstruction);
       procedure CompilePrefix(RootNode:PBRRERegExpASTNode);
       function CompileFixedStringSearch(RegExpProgram:TBRRERegExpProgram):boolean;
       function ConvertToFixedStringSearch(RegExpProgram:TBRRERegExpProgram):boolean;
       procedure CompileByteMap(RegExpProgram:TBRRERegExpProgram);
       function CompileOnePassNFA(StartInstruction,EndInstruction:PBRRERegExpNFAInstruction;RegExpProgram:TBRRERegExpProgram):boolean;
       function CompilePrefilterTree(RootNode:PBRRERegExpASTNode):TBRRERegExpPrefilterNode;
       procedure Emit(Node:PBRRERegExpASTNode);
       function Pass(AIsBacktracking:boolean;var ProgramIndex:longint;AHasBacktrackingParent:boolean):boolean;
       function PrefilterOptimize(Node:TBRRERegExpPrefilterNode):TBRRERegExpPrefilterNode;
       function SearchAllFirstCharsAndCheckIt(RegExpProgram:TBRRERegExpProgram):boolean;
      public
       RegExp:TBRRERegExp;
       AtEOF,IsFirstChar:boolean;
       CurrentChar:longword;
       SourceStr:ansistring;
       SourcePos:longint;
       HasRecursive:boolean;
       CaptureIdentifierHashMap:TBRREStringHashMap;
       CaptureIdentifiers:TStringList;
       CaptureNodes:TBRREPointerList;
       CaptureInstructions:TBRREPointerList;
       FixupableCaptureIdentifierHashMap:TBRREStringHashMap;
       FixupableCaptures:TBRRERegExpFixupableCaptures;
       CountFixupableCaptures:longint;
       Flags:longword;
       RegExpIsUTF8:longint;
       CharClassesHashMap:TBRREHashMap;
       CharClasses:TBRRERegExpCharClasses;
       AllFirstCharsClass:TBRRERegExpCharClass;
       AllCharsClass:TBRRERegExpCharClass;
       AnyCharClass:TBRRERegExpCharClass;
       constructor Create(ARegExp:TBRRERegExp);
       destructor Destroy; override;
       procedure Compile(const ARegExp:ansistring);
     end;

     TBRRERegExp=class
      private
       FirstCharClass:TBRRERegExpCharClass;
       LastCharClass:TBRRERegExpCharClass;
       RegExpDataInstanceList:TBRREPointerList;
       FreeRegExpDataInstance:TBRRERegExpDataInstance;
       CriticalSection:TCriticalSection;
       procedure RaiseMatchLimitOverflow;
       procedure RaiseMatchLimitRecursionOverflow;
       procedure RaiseMatchLimitStackRecursionOverflow;
       function AddCharClass(var CharClass:TBRRERegExpCharClass):longint;
       function IsWordChar(CharValue:longword):boolean; {$IFDEF caninline}inline;{$ENDIF}
       function AcquireRegExpDataInstance:TBRRERegExpDataInstance;
       procedure ReleaseRegExpDataInstance(Instance:TBRRERegExpDataInstance);
      public
       Source:ansistring;
       RegExpPrograms:TBRREPointerList;
       StartProgram:TBRRERegExpProgram;
       Flags:TBRRERegExpFlags;
       CountOfCaptures:longint;
       MaxRef:longint;
       CaptureIdentifierHashMap:TBRREStringHashMap;
       CaptureIdentifiers:TStringList;
       CharClassesHashMap:TBRREHashMap;
       CharClasses:TBRRERegExpCharClasses;
       AllFirstCharsClass:TBRRERegExpCharClass;
       AllCharsClass:TBRRERegExpCharClass;
       AnyCharClass:TBRRERegExpCharClass;
{$IFDEF BRRERegExpDebug}
       RegExpDebug:longint;
{$ENDIF}
       RegExpIsUTF8:longint;
       LastIndexCodePointValue:longint;
       LastIndexCodeUnitValue:longint;
       MaxMatchLimit:longint;
       MaxMatchLimitRecursion:longint;
       MaxMatchLimitStackRecursion:longint;
       PrefilterRootNode:TBRRERegExpPrefilterNode;
       CalloutFunc:TBRRECalloutFunc;
       CalloutData:pointer;
       constructor Create(const ARegExp:ansistring;AFlags:longword=brrefDELIMITERS);
       destructor Destroy; override;
       function PrefilterExpression:ansistring;
       function PrefilterShortExpression:ansistring;
       function PrefilterSQLBooleanFullTextExpression:ansistring;
       function PrefilterSQLExpression(Field:ansistring):ansistring;
       function GetRange(var RangeLow,RangeHigh:ansistring):longint;
       procedure PrepareInput(const Input:ansistring;var InputIsUTF8:longint;StartCodePoint:longint;var StartCodeUnit,InputCodeUnitLen,InputCodePointLen:longint;DoSearchStartCodeUnit:boolean=false);
       procedure PtrPrepareInput(const Input:pansichar;var InputIsUTF8:longint;StartCodePoint:longint;var StartCodeUnit,InputCodeUnitLen,InputCodePointLen:longint;DoSearchStartCodeUnit:boolean=false);
       function Match(const Input:ansistring;CodePoint,CodeUnit:longint;var OutputCaptures:TBRRERegExpCaptures;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):boolean;
       function PtrMatch(const Input:pansichar;CodePoint,CodeUnit:longint;var OutputCaptures:TBRRERegExpCaptures;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):boolean;
       function MatchAll(const Input:ansistring;var OutputMultipleCaptures:TBRRERegExpMultipleCaptures;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=1;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):longint;
       function PtrMatchAll(const Input:pansichar;var OutputMultipleCaptures:TBRRERegExpMultipleCaptures;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=0;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):longint;
       function ExtractAll(const Input:ansistring;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=1;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):TBRRERegExpMultipleStrings;
       function PtrExtractAll(const Input:pansichar;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=0;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):TBRRERegExpMultipleStrings;
       function Test(const Input:ansistring;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):boolean;
       function PtrTest(const Input:pansichar;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=0;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):boolean;
       function Find(const Input:ansistring;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):longint;
       function PtrFind(const Input:pansichar;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=0;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):longint;
       function Split(const Input:ansistring;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=1;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):TBRRERegExpStrings;
       function PtrSplit(const Input:pansichar;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=0;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):TBRRERegExpStrings;
       function Replace(const Input,Replacement:ansistring;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;ReplacementIsUTF8:longint=-1;ReplacementCodePointLen:longint=-1;ReplacementCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=1;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):ansistring;
       function PtrReplace(const Input,Replacement:pansichar;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;ReplacementIsUTF8:longint=-1;ReplacementCodePointLen:longint=-1;ReplacementCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=0;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):ansistring;
       function Benchmark(const Input:ansistring;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=1;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):longint;
       function PtrBenchmark(const Input:pansichar;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=0;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):longint;
     end;

function BRREUnicodeGetCategoryFromTable(c:longword):longword; {$IFDEF caninline}inline;{$ENDIF}
function BRREUnicodeGetScriptFromTable(c:longword):longword; {$IFDEF caninline}inline;{$ENDIF}
function BRREUnicodeGetUpperCaseDeltaFromTable(c:longword):longint; {$IFDEF caninline}inline;{$ENDIF}
function BRREUnicodeGetLowerCaseDeltaFromTable(c:longword):longint; {$IFDEF caninline}inline;{$ENDIF}
function BRREUnicodeGetTitleCaseDeltaFromTable(c:longword):longint; {$IFDEF caninline}inline;{$ENDIF}

function BRREUnicodeIsWord(c:longword):boolean; {$IFDEF caninline}inline;{$ENDIF}
function BRREUnicodeIsIDBegin(c:longword):boolean; {$IFDEF caninline}inline;{$ENDIF}
function BRREUnicodeIsIDPart(c:longword):boolean; {$IFDEF caninline}inline;{$ENDIF}
function BRREUnicodeIsWhiteSpace(c:longword):boolean; {$IFDEF caninline}inline;{$ENDIF}

function BRREUnicodeToUpper(c:longword):longword; {$IFDEF caninline}inline;{$ENDIF}
function BRREUnicodeToLower(c:longword):longword; {$IFDEF caninline}inline;{$ENDIF}
function BRREUnicodeToTitle(c:longword):longword; {$IFDEF caninline}inline;{$ENDIF}

function BRRERoundUpToPowerOfTwo(x:ptruint):ptruint; {$IFDEF caninline}inline;{$ENDIF}

function BRREHashString(const Str:ansistring):longword;

function BRREHashData(const Data:pointer;Len:longword):longword;

function BRREUTF8RangeToRegEx(Lo,Hi:longword):ansistring;

function BRREUTF32CharToUTF8(CharValue:longword):ansistring;
function BRREUTF32CharToUTF8Len(CharValue:longword):longint;
function BRREIsUTF8(const s:ansistring):boolean;
function BRREValidateUTF8(const s:ansistring):boolean;
function BRREGetUTF8(const s:ansistring):longint;
function BRREPtrGetUTF8(const s:pansichar;Len:longint):longint;
procedure BRREUTF8SafeInc(const s:ansistring;var CodeUnit:longint);
procedure BRREUTF8Inc(const s:ansistring;var CodeUnit:longint);
procedure BRREUTF8PtrInc(const s:pansichar;Len:longint;var CodeUnit:longint);
procedure BRREUTF8Dec(const s:ansistring;var CodeUnit:longint);
procedure BRREUTF8PtrDec(const s:pansichar;Len:longint;var CodeUnit:longint);
procedure BRREUTF8Delete(var s:ansistring;CodeUnit:longint);
function BRREUTF8Length(const s:ansistring):longint;
function BRREUTF8PtrLength(const s:ansistring;Len:longint):longint;
function BRREUTF8LengthEx(const s:ansistring):longint;
function BRREUTF8GetCodePoint(const s:ansistring;CodeUnit:longint):longint;
function BRREUTF8PtrGetCodePoint(const s:pansichar;Len,CodeUnit:longint):longint;
function BRREUTF8GetCodeUnit(const s:ansistring;CodePoint:longint):longint;
function BRREUTF8PtrGetCodeUnit(const s:ansistring;Len,CodePoint:longint):longint;
function BRREUTF8CodeUnitGetChar(const s:ansistring;CodeUnit:longint):longword;
function BRREUTF8PtrCodeUnitGetChar(const s:pansichar;Len,CodeUnit:longint):longword;
function BRREUTF8CodeUnitGetCharAndInc(const s:ansistring;var CodeUnit:longint):longword;
function BRREUTF8PtrCodeUnitGetCharAndInc(const s:pansichar;Len:longint;var CodeUnit:longint):longword;
function BRREUTF8CodeUnitGetCharFallback(const s:ansistring;CodeUnit:longint):longword;
function BRREUTF8CodeUnitGetCharAndIncFallback(const s:ansistring;var CodeUnit:longint):longword;
function BRREUTF8CodePointGetChar(const s:ansistring;CodePoint:longint;Fallback:boolean=false):longword;
function BRREUTF8GetCharLen(const s:ansistring;i:longint):longword;
function BRREUTF8Pos(const FindStr,InStr:ansistring):longint;
function BRREUTF8Copy(const Str:ansistring;Start,Len:longint):ansistring;
function BRREUTF8Uppercase(const Str:ansistring):ansistring;
function BRREUTF8Lowercase(const Str:ansistring):ansistring;
function BRREUTF8Trim(const Str:ansistring):ansistring;
function BRREUTF8Correct(const Str:ansistring):ansistring;
function BRREUTF8FromLatin1(const Str:ansistring):ansistring;
function BRREUTF8LevenshteinDistance(const s,t:ansistring):longint;
function BRREUTF8DamerauLevenshteinDistance(const s,t:ansistring):longint;
function BRREStringLength(const s:ansistring):longint;

function BRREEscape(s:ansistring):ansistring;

function BRREGetVersion:int64; cdecl;
function BRREGetVersionString:pansichar; cdecl;
function BRRECreate(RegExp:pansichar;RegExpLen:longint;Flags:longword;Error:ppansichar):TBRRERegExp; cdecl;
function BRRESetMaxMatchLimit(Instance:TBRRERegExp;MaxMatchLimit:longint):longint; cdecl;
function BRRESetMaxMatchLimitRecursion(Instance:TBRRERegExp;MaxMatchLimitRecursion:longint):longint; cdecl;
function BRRESetMaxMatchLimitStackRecursion(Instance:TBRRERegExp;MaxMatchLimitStackRecursion:longint):longint; cdecl;
function BRRESetCalloutFunc(Instance:TBRRERegExp;CalloutFunc:pointer):longint; cdecl;
function BRRESetCalloutData(Instance:TBRRERegExp;CalloutData:pointer):longint; cdecl;
function BRREGetCountCaptures(Instance:TBRRERegExp):longint; cdecl;
function BRREGetNamedGroupIndex(Instance:TBRRERegExp;GroupName:pansichar):longint; cdecl;
function BRREPrefilterExpression(Instance:TBRRERegExp;Expression,Error:ppansichar):longint; cdecl;
function BRREPrefilterShortExpression(Instance:TBRRERegExp;ShortExpression,Error:ppansichar):longint; cdecl;
function BRREPrefilterSQLBooleanFullTextExpression(Instance:TBRRERegExp;SQLBooleanFullTextExpression,Error:ppansichar):longint; cdecl;
function BRREPrefilterSQLExpression(Instance:TBRRERegExp;Field:pansichar;SQLExpression,Error:ppansichar):longint; cdecl;
function BRREGetRange(Instance:TBRRERegExp;RangeLow,RangeHigh,Error:ppansichar):longint; cdecl;
function BRREMatch(Instance:TBRRERegExp;Input:pansichar;InputIsUTF8,InputCodePointLen,InputCodeUnitLen:longint;Captures:ppointer;MaxCaptures,StartCodePoint,StartCodeUnit:longint;LastIndexCodePoint,LastIndexCodeUnit:pointer;Error:ppansichar):longint; cdecl;
function BRREMatchAll(Instance:TBRRERegExp;Input:pansichar;InputIsUTF8,InputCodePointLen,InputCodeUnitLen:longint;Matches:ppointer;StartCodePoint,StartCodeUnit,Limit:longint;LastIndexCodePoint,LastIndexCodeUnit:pointer;Error:ppansichar):longint; cdecl;
function BRREMatchRef(Instance:TBRRERegExp;Input:pansichar;InputIsUTF8,InputCodePointLen,InputCodeUnitLen:longint;Captures:ppointer;MaxCaptures,StartCodePoint,StartCodeUnit:longint;LastIndexCodePoint,LastIndexCodeUnit:pointer;Error:ppansichar):longint; cdecl;
function BRRETest(Instance:TBRRERegExp;Input:pansichar;InputIsUTF8,InputCodePointLen,InputCodeUnitLen:longint;StartCodePoint,StartCodeUnit:longint;LastIndexCodePoint,LastIndexCodeUnit:pointer;Error:ppansichar):longint; cdecl;
function BRREFind(Instance:TBRRERegExp;Input:pansichar;InputIsUTF8,InputCodePointLen,InputCodeUnitLen:longint;StartCodePoint,StartCodeUnit:longint;LastIndexCodePoint,LastIndexCodeUnit:pointer;Error:ppansichar):longint; cdecl;
function BRRESplit(Instance:TBRRERegExp;Input:pansichar;InputIsUTF8,InputCodePointLen,InputCodeUnitLen:longint;Output:ppointer;StartCodePoint,StartCodeUnit,Limit:longint;LastIndexCodePoint,LastIndexCodeUnit:pointer;Error:ppansichar):longint; cdecl;
function BRREReplace(Instance:TBRRERegExp;Input,Replacement:pansichar;InputIsUTF8,InputCodePointLen,InputCodeUnitLen,ReplacementIsUTF8,ReplacementCodePointLen,ReplacementCodeUnitLen,StartCodePoint,StartCodeUnit,Limit:longint;LastIndexCodePoint,LastIndexCodeUnit:pointer;ResultStr,Error:ppansichar):longint; cdecl;
function BRREBenchmark(Instance:TBRRERegExp;Input:pansichar;InputIsUTF8,InputCodePointLen,InputCodeUnitLen:longint;StartCodePoint,StartCodeUnit,Limit:longint;LastIndexCodePoint,LastIndexCodeUnit:pointer;Error:ppansichar):longint; cdecl;
procedure BRREFree(Instance:TBRRERegExp;p:pointer); cdecl;
procedure BRREDestroy(Instance:TBRRERegExp); cdecl;

procedure InitializeBRRE;
procedure FinalizeBRRE;

implementation

const CELL_EMPTY=-1;
      CELL_DELETED=-2;

      ENT_EMPTY=-1;
      ENT_DELETED=-2;

      MaxListSize=$07ffffff;

var BRREUnicodeCharRangeClasses:TBRREUnicodeCharRangeClasses;

    BRREUnicodeClassHashMap:TBRREStringHashMap;
    BRREUnicodeScriptHashMap:TBRREStringHashMap;
    BRREUnicodeBlockHashMap:TBRREStringHashMap;

    BRREWhichFlags:array[byte] of longword;

type TUTF8Chars=array[ansichar] of byte;

     TUTF8Bytes=array[byte] of byte;

{$IFDEF BRREStrictUTF8}
                              //0 1 2 3 4 5 6 7 8 9 a b c d e f
const UTF8CharSteps:TUTF8Chars=(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 1
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 2
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 3
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 4
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 5
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 6
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 7
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 8
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 9
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // a
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // b
                                1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // c
                                2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // d
                                3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,  // e
                                4,4,4,4,4,1,1,1,1,1,1,1,1,1,1,1); // f
                              //0 1 2 3 4 5 6 7 8 9 a b c d e f

{$ELSE}
                              //0 1 2 3 4 5 6 7 8 9 a b c d e f
const UTF8CharSteps:TUTF8Chars=(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 1
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 2
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 3
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 4
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 5
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 6
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 7
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 8
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 9
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // a
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // b
                                2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // c
                                2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // d
                                3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,  // e
                                4,4,4,4,4,4,4,4,5,5,5,5,6,6,1,1); // f
                              //0 1 2 3 4 5 6 7 8 9 a b c d e f

{$ENDIF}

var UTF8DFACharClasses:TUTF8Chars;
    UTF8DFATransitions:TUTF8Bytes;

procedure GetMemAligned(var p;Size:longint;Align:longint=16);
var Original,Aligned:pointer;
    Mask:ptruint;
begin
 if (Align and (Align-1))<>0 then begin
  Align:=BRRERoundUpToPowerOfTwo(Align);
 end;
 Mask:=Align-1;
 inc(Size,((Align shl 1)+sizeof(pointer)));
 GetMem(Original,Size);
 FillChar(Original^,Size,#0);
 Aligned:=pointer(ptruint(ptruint(Original)+sizeof(pointer)));
 if (Align>1) and ((ptruint(Aligned) and Mask)<>0) then begin
  inc(ptruint(Aligned),ptruint(ptruint(Align)-(ptruint(Aligned) and Mask)));
 end;
 pointer(pointer(ptruint(ptruint(Aligned)-sizeof(pointer)))^):=Original;
 pointer(pointer(@p)^):=Aligned;
end;

procedure FreeMemAligned(const p);
var pp:pointer;
begin
 pp:=pointer(pointer(@p)^);
 if assigned(pp) then begin
  pp:=pointer(pointer(ptruint(ptruint(pp)-sizeof(pointer)))^);
  FreeMem(pp);
 end;
end;

{$IFDEF cpuamd64}
function InterlockedCompareExchange128Ex(Target,NewValue,Comperand:pointer):boolean; assembler; register;
asm
 push rbx
{$IFDEF win64}
 push rdi
 push rsi
 mov rdi,rcx
 mov rsi,rdx
 mov rdx,qword ptr [r8+4]
 mov rax,qword ptr [r8+0]
{$ELSE}
 mov rax,qword ptr [rdx+0]
 mov rdx,qword ptr [rdx+4]
{$ENDIF}
 mov rcx,qword ptr [rsi+4]
 mov rbx,qword ptr [rsi+0]
 lock cmpxchg16b [rdi]
 setz al
{$IFDEF win64}
 pop rsi
 pop rdi
{$ENDIF}
 pop rbx
end;
{$ENDIF}

{$IFDEF cpu386}
{$ifndef ver130}
function InterlockedCompareExchange64Ex(Target,NewValue,Comperand:pointer):boolean; assembler; register;
asm
 push ebx
 push edi
 push esi
 mov edi,eax
 mov esi,edx
 mov edx,dword ptr [ecx+4]
 mov eax,dword ptr [ecx+0]
 mov ecx,dword ptr [esi+4]
 mov ebx,dword ptr [esi+0]
 lock cmpxchg8b [edi]
 setz al
 pop esi
 pop edi
 pop ebx
end;

function InterlockedCompareExchange64(var Target:int64;NewValue:int64;Comperand:int64):int64; assembler; register;
asm
 push ebx
 push edi
 mov edi,eax
 mov edx,dword ptr [Comperand+4]
 mov eax,dword ptr [Comperand+0]
 mov ecx,dword ptr [NewValue+4]
 mov ebx,dword ptr [NewValue+0]
 lock cmpxchg8b [edi]
 pop edi
 pop ebx
end;
{$ENDIF}
{$ENDIF}

{$ifndef fpc}
{$IFDEF cpu386}
function InterlockedDecrement(var Target:longint):longint; assembler; register;
asm
 mov edx,$ffffffff
 xchg eax,edx
 lock xadd dword ptr [edx],eax
 dec eax
end;

function InterlockedIncrement(var Target:longint):longint; assembler; register;
asm
 mov edx,1
 xchg eax,edx
 lock xadd dword ptr [edx],eax
 inc eax
end;

function InterlockedExchange(var Target:longint;Source:longint):longint; assembler; register;
asm
 lock xchg dword ptr [eax],edx
 mov eax,edx
end;

function InterlockedExchangeAdd(var Target:longint;Source:longint):longint; assembler; register;
asm
 xchg edx,eax
 lock xadd dword ptr [edx],eax
end;

function InterlockedCompareExchange(var Target:longint;NewValue,Comperand:longint):longint; assembler; register;
asm
 xchg ecx,eax
 lock cmpxchg dword ptr [ecx],edx
end;
{$ELSE}
function InterlockedDecrement(var Target:longint):longint; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=Windows.InterlockedDecrement(Target);
end;

function InterlockedIncrement(var Target:longint):longint; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=Windows.InterlockedIncrement(Target);
end;

function InterlockedExchange(var Target:longint;Source:longint):longint; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=Windows.InterlockedExchange(Target,Source);
end;

function InterlockedExchangeAdd(var Target:longint;Source:longint):longint; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=Windows.InterlockedExchangeAdd(Target,Source);
end;

function InterlockedCompareExchange(var Target:longint;NewValue,Comperand:longint):longint; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=Windows.InterlockedCompareExchange(Target,NewValue,Comperand);
end;
{$ENDIF}
{$ELSE}
function InterlockedDecrement(var Target:longint):longint; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=System.InterlockedDecrement(Target);
end;

function InterlockedIncrement(var Target:longint):longint; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=System.InterlockedIncrement(Target);
end;

function InterlockedExchange(var Target:longint;Source:longint):longint; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=System.InterlockedExchange(Target,Source);
end;

function InterlockedExchangeAdd(var Target:longint;Source:longint):longint; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=System.InterlockedExchangeAdd(Target,Source);
end;

function InterlockedCompareExchange(var Target:longint;NewValue,Comperand:longint):longint; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=System.InterlockedCompareExchange(Target,NewValue,Comperand);
end;
{$ENDIF}

function BRREUnicodeGetCategoryFromTable(c:longword):longword; {$IFDEF caninline}inline;{$ENDIF}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr BRREUnicodeCategoryArrayBlockBits;
  result:=BRREUnicodeCategoryArrayBlockData[BRREUnicodeCategoryArrayIndexBlockData[BRREUnicodeCategoryArrayIndexIndexData[Index shr BRREUnicodeCategoryArrayIndexBlockBits],Index and BRREUnicodeCategoryArrayIndexBlockMask],c and BRREUnicodeCategoryArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function BRREUnicodeGetScriptFromTable(c:longword):longword; {$IFDEF caninline}inline;{$ENDIF}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr BRREUnicodeScriptArrayBlockBits;
  result:=BRREUnicodeScriptArrayBlockData[BRREUnicodeScriptArrayIndexBlockData[BRREUnicodeScriptArrayIndexIndexData[Index shr BRREUnicodeScriptArrayIndexBlockBits],Index and BRREUnicodeScriptArrayIndexBlockMask],c and BRREUnicodeScriptArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function BRREUnicodeGetUpperCaseDeltaFromTable(c:longword):longint; {$IFDEF caninline}inline;{$ENDIF}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr BRREUnicodeUpperCaseDeltaArrayBlockBits;
  result:=BRREUnicodeUpperCaseDeltaArrayBlockData[BRREUnicodeUpperCaseDeltaArrayIndexBlockData[BRREUnicodeUpperCaseDeltaArrayIndexIndexData[Index shr BRREUnicodeUpperCaseDeltaArrayIndexBlockBits],Index and BRREUnicodeUpperCaseDeltaArrayIndexBlockMask],c and BRREUnicodeUpperCaseDeltaArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function BRREUnicodeGetLowerCaseDeltaFromTable(c:longword):longint; {$IFDEF caninline}inline;{$ENDIF}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr BRREUnicodeLowerCaseDeltaArrayBlockBits;
  result:=BRREUnicodeLowerCaseDeltaArrayBlockData[BRREUnicodeLowerCaseDeltaArrayIndexBlockData[BRREUnicodeLowerCaseDeltaArrayIndexIndexData[Index shr BRREUnicodeLowerCaseDeltaArrayIndexBlockBits],Index and BRREUnicodeLowerCaseDeltaArrayIndexBlockMask],c and BRREUnicodeLowerCaseDeltaArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function BRREUnicodeGetTitleCaseDeltaFromTable(c:longword):longint; {$IFDEF caninline}inline;{$ENDIF}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr BRREUnicodeTitleCaseDeltaArrayBlockBits;
  result:=BRREUnicodeTitleCaseDeltaArrayBlockData[BRREUnicodeTitleCaseDeltaArrayIndexBlockData[BRREUnicodeTitleCaseDeltaArrayIndexIndexData[Index shr BRREUnicodeTitleCaseDeltaArrayIndexBlockBits],Index and BRREUnicodeTitleCaseDeltaArrayIndexBlockMask],c and BRREUnicodeTitleCaseDeltaArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function BRREUnicodeIsWord(c:longword):boolean; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=(BRREUnicodeGetCategoryFromTable(c) in [BRREUnicodeCategoryLu,BRREUnicodeCategoryLl,BRREUnicodeCategoryLt,BRREUnicodeCategoryLm,BRREUnicodeCategoryLo,BRREUnicodeCategoryNd,BRREUnicodeCategoryNl,BRREUnicodeCategoryNo,BRREUnicodeCategoryPc]) or (c=ord('_'));
end;

function BRREUnicodeIsIDBegin(c:longword):boolean; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=(BRREUnicodeGetCategoryFromTable(c) in [BRREUnicodeCategoryLu,BRREUnicodeCategoryLl,BRREUnicodeCategoryLt,BRREUnicodeCategoryLm,BRREUnicodeCategoryLo,BRREUnicodeCategoryNl,BRREUnicodeCategoryNo,BRREUnicodeCategoryPc]) or (c=ord('_'));
end;

function BRREUnicodeIsIDPart(c:longword):boolean; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=(BRREUnicodeGetCategoryFromTable(c) in [BRREUnicodeCategoryLu,BRREUnicodeCategoryLl,BRREUnicodeCategoryLt,BRREUnicodeCategoryLm,BRREUnicodeCategoryLo,BRREUnicodeCategoryNd,BRREUnicodeCategoryNl,BRREUnicodeCategoryNo,BRREUnicodeCategoryPc]) or (c=ord('_'));
end;

function BRREUnicodeIsWhiteSpace(c:longword):boolean; {$IFDEF caninline}inline;{$ENDIF}
begin
//result:=BRREUnicodeGetCategoryFromTable(c) in [BRREUnicodeCategoryZs,BRREUnicodeCategoryZp,BRREUnicodeCategoryZl];
 result:=((c>=$0009) and (c<=$000d)) or (c=$0020) or (c=$00a0) or (c=$1680) or (c=$180e) or ((c>=$2000) and (c<=$200b)) or (c=$2028) or (c=$2029) or (c=$202f) or (c=$205f) or (c=$3000) or (c=$feff) or (c=$fffe);
end;

function BRREUnicodeToUpper(c:longword):longword; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=longword(longint(longint(c)+BRREUnicodeGetUpperCaseDeltaFromTable(c)));
end;

function BRREUnicodeToLower(c:longword):longword; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=longword(longint(longint(c)+BRREUnicodeGetLowerCaseDeltaFromTable(c)));
end;

function BRREUnicodeToTitle(c:longword):longword; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=longword(longint(longint(c)+BRREUnicodeGetTitleCaseDeltaFromTable(c)));
end;

function BRRERoundUpToPowerOfTwo(x:ptruint):ptruint; {$IFDEF caninline}inline;{$ENDIF}
begin
 dec(x);
 x:=x or (x shr 1);
 x:=x or (x shr 2);
 x:=x or (x shr 4);
 x:=x or (x shr 8);
 x:=x or (x shr 16);
{$IFDEF cpu64}
 x:=x or (x shr 32);
{$ENDIF}
 result:=x+1;
end;

function BRREHashString(const Str:ansistring):longword;
{$IFDEF cpuarm}
var b:pansichar;
    Len,h,i:longword;
begin
 result:=2166136261;
 Len:=length(Str);
 h:=Len;
 if Len>0 then begin
  b:=pansichar(Str);
  while Len>3 do begin
   i:=longword(pointer(b)^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
   inc(b,4);
   dec(Len,4);
  end;
  if Len>1 then begin
   i:=word(pointer(b)^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
   inc(b,2);
   dec(Len,2);
  end;
  if Len>0 then begin
   i:=byte(b^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
  end;
 end;
 result:=result xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;
{$ELSE}
const m=longword($57559429);
      n=longword($5052acdb);
var b:pansichar;
    h,k,Len:longword;
    p:{$IFDEF FPC}qword{$ELSE}int64{$ENDIF};
begin
 Len:=length(Str);
 h:=Len;
 k:=h+n+1;
 if Len>0 then begin
  b:=pansichar(Str);
  while Len>7 do begin
   begin
    p:=longword(pointer(b)^)*qword(n);
    h:=h xor longword(p and $ffffffff);
    k:=k xor longword(p shr 32);
    inc(b,4);
   end;
   begin
    p:=longword(pointer(b)^)*qword(m);
    k:=k xor longword(p and $ffffffff);
    h:=h xor longword(p shr 32);
    inc(b,4);
   end;
   dec(Len,8);
  end;
  if Len>3 then begin
   p:=longword(pointer(b)^)*qword(n);
   h:=h xor longword(p and $ffffffff);
   k:=k xor longword(p shr 32);
   inc(b,4);
   dec(Len,4);
  end;
  if Len>0 then begin
   if Len>1 then begin
    p:=word(pointer(b)^);
    inc(b,2);
    dec(Len,2);
   end else begin
    p:=0;
   end;
   if Len>0 then begin
    p:=p or (byte(b^) shl 16);
   end;
   p:=p*qword(m);
   k:=k xor longword(p and $ffffffff);
   h:=h xor longword(p shr 32);
  end;
 end;
 begin
  p:=(h xor (k+n))*qword(n);
  h:=h xor longword(p and $ffffffff);
  k:=k xor longword(p shr 32);
 end;
 result:=k xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;
{$ENDIF}

function BRREHashData(const Data:pointer;Len:longword):longword;
{$IFDEF cpuarm}
var b:pansichar;
    h,i:longword;
begin
 result:=$811c9dc5;
 h:=Len;
 if Len>0 then begin
  b:=Data;
  while Len>3 do begin
   i:=longword(pointer(b)^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
   inc(b,4);
   dec(Len,4);
  end;
  if Len>1 then begin
   i:=word(pointer(b)^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
   inc(b,2);
   dec(Len,2);
  end;
  if Len>0 then begin
   i:=byte(b^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
  end;
 end;
 result:=result xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;
{$ELSE}
const m=longword($57559429);
      n=longword($5052acdb);
var b:pansichar;
    h,k:longword;
    p:{$IFDEF FPC}qword{$ELSE}int64{$ENDIF};
begin
 h:=Len;
 k:=h+n+1;
 if Len>0 then begin
  b:=Data;
  while Len>7 do begin
   begin
    p:=longword(pointer(b)^)*qword(n);
    h:=h xor longword(p and $ffffffff);
    k:=k xor longword(p shr 32);
    inc(b,4);
   end;
   begin
    p:=longword(pointer(b)^)*qword(m);
    k:=k xor longword(p and $ffffffff);
    h:=h xor longword(p shr 32);
    inc(b,4);
   end;
   dec(Len,8);
  end;
  if Len>3 then begin
   p:=longword(pointer(b)^)*qword(n);
   h:=h xor longword(p and $ffffffff);
   k:=k xor longword(p shr 32);
   inc(b,4);
   dec(Len,4);
  end;
  if Len>0 then begin
   if Len>1 then begin
    p:=word(pointer(b)^);
    inc(b,2);
    dec(Len,2);
   end else begin
    p:=0;
   end;
   if Len>0 then begin
    p:=p or (byte(b^) shl 16);
   end;
   p:=p*qword(m);
   k:=k xor longword(p and $ffffffff);
   h:=h xor longword(p shr 32);
  end;
 end;
 begin
  p:=(h xor (k+n))*qword(n);
  h:=h xor longword(p and $ffffffff);
  k:=k xor longword(p shr 32);
 end;
 result:=k xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;
{$ENDIF}

function BRREUTF8RangeToRegEx(Lo,Hi:longword):ansistring;
type TString6Chars=string[6];
const Seq0010ffff:array[0..6,0..4,0..1] of longint=((($00,$7f),(-1,-1),(-1,-1),(-1,-1),(-1,-1)),        // 00-7F
                                                    (($c2,$df),($80,$bf),(-1,-1),(-1,-1),(-1,-1)),      // C2-DF 80-BF
                                                    (($e0,$e0),($a0,$bf),($80,$bf),(-1,-1),(-1,-1)),    // E0-E0 A0-BF 80-BF
                                                    (($e1,$ef),($80,$bf),($80,$bf),(-1,-1),(-1,-1)),    // E1-EF 80-BF 80-BF
                                                    (($f0,$f0),($80,$bf),($80,$bf),($80,$bf),(-1,-1)),  // F0-F0 90-BF 80-BF 80-BF
                                                    (($f1,$f3),($80,$bf),($80,$bf),($80,$bf),(-1,-1)),  // F1-F3 80-BF 80-BF 80-BF
                                                    (($f4,$f4),($80,$bf),($80,$bf),($80,$bf),(-1,-1))); // F4-F4 80-8F 80-BF 80-BF
      HexChars:array[$0..$f] of ansichar='0123456789ABCDEF';
var OutputCharSequence:ansistring;
 function ToString(CharValue:longword):TString6Chars;
 begin
  case CharValue of
   $00000000..$0000007f:begin
    SetLength(result,1);
    result[1]:=ansichar(byte(CharValue));
   end;
   $00000080..$000007ff:begin
    SetLength(result,2);
    result[1]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
    result[2]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
// {$IFDEF BRREStrictUTF8}$00000800..$0000d7ff,$0000e000..$0000ffff{$ELSE}$00000800..$0000ffff{$ENDIF}:begin
   $00000800..$0000ffff:begin
    SetLength(result,3);
    result[1]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    result[2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    result[3]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   $00010000..$0010ffff:begin
    SetLength(result,4);
    result[1]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
    result[2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    result[3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    result[4]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   $00200000..$03ffffff:begin
    SetLength(result,5);
    result[1]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
    result[2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    result[3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    result[4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    result[5]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   $04000000..$7fffffff:begin
    SetLength(result,6);
    result[1]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
    result[2]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
    result[3]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    result[4]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    result[5]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    result[6]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   else begin
    SetLength(result,3);
    result[1]:=#$ef;
    result[2]:=#$bf;
    result[3]:=#$bd;
   end;
  end;
 end;
 procedure AddRange(Lo,Hi:byte);
 var Data:array[0..11] of ansichar;
 begin
  Data:='[\x00-\x00]';
  Data[3]:=HexChars[(Lo shr 4) and $f];
  Data[4]:=HexChars[Lo and $f];
  Data[8]:=HexChars[(Hi shr 4) and $f];
  Data[9]:=HexChars[Hi and $f];
  OutputCharSequence:=OutputCharSequence+Data;
 end;
 procedure ProcessRange(Lo,Hi:longword);
 var i,m:longword;
     StrLo,StrHi:TString6Chars;
 begin
  if Hi>$0010ffff then begin
   Hi:=$0010ffff;
  end;
  if Lo<=Hi then begin
    if (Lo=$00000000) and (Hi=$0010ffff) then begin
    for m:=low(Seq0010ffff) to high(Seq0010ffff) do begin
     for i:=low(Seq0010ffff[m]) to high(Seq0010ffff[m]) do begin
      if Seq0010ffff[m,i,0]<0 then begin
       break;
      end;
      AddRange(byte(Seq0010ffff[m,i,0]),byte(Seq0010ffff[m,i,1]));
     end;
     OutputCharSequence:=OutputCharSequence+'|';
    end;
   end else if (Lo=$00000080) and (Hi=$0010ffff) then begin
    for m:=1 to high(Seq0010ffff) do begin
     for i:=low(Seq0010ffff[m]) to high(Seq0010ffff[m]) do begin
      if Seq0010ffff[m,i,0]<0 then begin
       break;
      end;
      AddRange(byte(Seq0010ffff[m,i,0]),byte(Seq0010ffff[m,i,1]));
     end;
     OutputCharSequence:=OutputCharSequence+'|';
    end;
   end else begin
    for i:=1 to 3 do begin
     if i=1 then begin
      m:=7;
     end else begin
      m:=(7-i)+(6*(i-1));
     end;
     m:=(1 shl m)-1;
     if (Lo<=m) and (m<Hi) then begin
      ProcessRange(Lo,m);
      ProcessRange(m+1,Hi);
      exit;
     end;
    end;
    if Hi<128 then begin
     AddRange(Lo,Hi);
     OutputCharSequence:=OutputCharSequence+'|';
    end else begin
     for i:=1 to 3 do begin
      m:=(1 shl (6*i))-1;
      if (Lo and not m)<>(Hi and not m) then begin
       if (Lo and m)<>0 then begin
        ProcessRange(Lo,Lo or m);
        ProcessRange((Lo or m)+1,Hi);
        exit;
       end else if (Hi and m)<>m then begin
        ProcessRange(Lo,(Hi and not m)-1);
        ProcessRange(Hi and not m,Hi);
        exit;
       end;
      end;
     end;
     StrLo:=ToString(Lo);
     StrHi:=ToString(Hi);
     if length(StrLo)=length(StrHi) then begin
      for i:=1 to length(StrLo) do begin
       AddRange(byte(StrLo[i]),byte(StrHi[i]));
      end;
      OutputCharSequence:=OutputCharSequence+'|';
     end;
    end;
   end;
  end;
 end;
begin
 OutputCharSequence:='';
 ProcessRange(Lo,Hi);
 result:=copy(OutputCharSequence,1,length(OutputCharSequence)-1);
end;

function BRREUTF32CharToUTF8(CharValue:longword):ansistring;
var Data:array[0..{$IFDEF BRREStrictUTF8}3{$ELSE}5{$ENDIF}] of ansichar;
    ResultLen:longint;
begin
 if CharValue=0 then begin
  result:=#0;
 end else begin
  if CharValue<=$7f then begin
   Data[0]:=ansichar(byte(CharValue));
   ResultLen:=1;
  end else if CharValue<=$7ff then begin
   Data[0]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
   Data[1]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=2;
{$IFDEF BRREStrictUTF8}
  end else if CharValue<=$d7ff then begin
   Data[0]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$dfff then begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
{$ENDIF}
  end else if CharValue<=$ffff then begin
   Data[0]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$1fffff then begin
   Data[0]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[3]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=4;
{$ifndef BRREStrictUTF8}
  end else if CharValue<=$3ffffff then begin
   Data[0]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[4]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=5;
  end else if CharValue<=$7fffffff then begin
   Data[0]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
   Data[3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[5]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=6;
{$ENDIF}
  end else begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
  end;
  SetString(result,pansichar(@Data[0]),ResultLen);
 end;
end;

function BRREUTF32CharToUTF8Len(CharValue:longword):longint;
begin
 if CharValue<=$7f then begin
  result:=1;
 end else if CharValue<=$7ff then begin
  result:=2;
 end else if CharValue<=$ffff then begin
  result:=3;
 end else if CharValue<=$1fffff then begin
  result:=4;
{$ifndef BRREStrictUTF8}
 end else if CharValue<=$3ffffff then begin
  result:=5;
 end else if CharValue<=$7fffffff then begin
  result:=6;
{$ENDIF}
 end else begin
  result:=3;
 end;
end;

function BRREIsUTF8(const s:ansistring):boolean;
var CodeUnit,CodePoints:longint;
    State:longword;
begin
 State:=brreucACCEPT;
 CodePoints:=0;
 for CodeUnit:=1 to length(s) do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  case State of
   brreucACCEPT:begin
    inc(CodePoints);
   end;
   brreucERROR:begin
    result:=false;
    exit;
   end;
  end;
 end;
 result:=(State=brreucACCEPT) and (length(s)<>CodePoints);
end;

function BRREValidateUTF8(const s:ansistring):boolean;
var CodeUnit:longint;
    State:longword;
begin
 State:=brreucACCEPT;
 for CodeUnit:=1 to length(s) do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  if State=brreucERROR then begin
   result:=false;
   exit;
  end;
 end;
 result:=State=brreucACCEPT;
end;

function BRREGetUTF8(const s:ansistring):longint;
var CodeUnit,CodePoints:longint;
    State:longword;
begin
 State:=brreucACCEPT;
 CodePoints:=0;
 for CodeUnit:=1 to length(s) do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  case State of
   brreucACCEPT:begin
    inc(CodePoints);
   end;
   brreucERROR:begin
    result:=brresuNOUTF8;
    exit;
   end;
  end;
 end;
 if State=brreucACCEPT then begin
  if length(s)<>CodePoints then begin
   result:=brresuISUTF8;
  end else begin
   result:=brresuPOSSIBLEUTF8;
  end;
 end else begin
  result:=brresuNOUTF8;
 end;
end;

function BRREPtrGetUTF8(const s:pansichar;Len:longint):longint;
var CodeUnit,CodePoints:longint;
    State:longword;
begin
 State:=brreucACCEPT;
 CodePoints:=0;
 for CodeUnit:=0 to Len-1 do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  case State of
   brreucACCEPT:begin
    inc(CodePoints);
   end;
   brreucERROR:begin
    result:=brresuNOUTF8;
    exit;
   end;
  end;
 end;
 if State=brreucACCEPT then begin
  if length(s)<>CodePoints then begin
   result:=brresuISUTF8;
  end else begin
   result:=brresuPOSSIBLEUTF8;
  end;
 end else begin
  result:=brresuNOUTF8;
 end;
end;

procedure BRREUTF8SafeInc(const s:ansistring;var CodeUnit:longint);
var Len:longint;
    StartCodeUnit,State:longword;
begin
 Len:=length(s);
 if CodeUnit>0 then begin
  StartCodeUnit:=CodeUnit;
  State:=brreucACCEPT;
  while CodeUnit<=Len do begin
   State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
   inc(CodeUnit);
   if State<=brreucERROR then begin
    break;
   end;
  end;
  if State<>brreucACCEPT then begin
   CodeUnit:=StartCodeUnit+1;
  end;
 end;
end;

procedure BRREUTF8Inc(const s:ansistring;var CodeUnit:longint);
begin
 if (CodeUnit>0) and (CodeUnit<=length(s)) then begin
  inc(CodeUnit,UTF8CharSteps[s[CodeUnit]]);
 end;
end;

procedure BRREUTF8PtrInc(const s:pansichar;Len:longint;var CodeUnit:longint);
begin
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  inc(CodeUnit,UTF8CharSteps[s[CodeUnit]]);
 end;
end;

procedure BRREUTF8Dec(const s:ansistring;var CodeUnit:longint);
begin
 if (CodeUnit>=1) and (CodeUnit<=(length(s)+1)) then begin
  dec(CodeUnit);
  while CodeUnit>0 do begin
   if s[CodeUnit] in [#$80..#$bf] then begin
    dec(CodeUnit);
   end else begin
    break;
   end;
  end;
 end;
end;

procedure BRREUTF8PtrDec(const s:pansichar;Len:longint;var CodeUnit:longint);
begin
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  dec(CodeUnit);
  while CodeUnit>=0 do begin
   if s[CodeUnit] in [#$80..#$bf] then begin
    dec(CodeUnit);
   end else begin
    break;
   end;
  end;
 end;
end;

procedure BRREUTF8Delete(var s:ansistring;CodeUnit:longint);
begin
 if (CodeUnit>=1) and (CodeUnit<=length(s)) then begin
  Delete(s,CodeUnit,1);
  while ((CodeUnit>=1) and (CodeUnit<=length(s))) and (s[CodeUnit] in [#$80..#$bf]) do begin
   Delete(s,CodeUnit,1);
  end;
 end;
end;

function BRREUTF8Length(const s:ansistring):longint;
{$IFDEF cpu386} assembler; register;
asm
 test eax,eax
 jz @End
  push esi
   cld
   mov esi,eax
   mov ecx,dword ptr [esi-4]
   xor edx,edx
   jecxz @LoopEnd
    @Loop:
      lodsb
      shl al,1
      js @IsASCIICharOrUTF8Begin
      jc @IsUTF8Part
      @IsASCIICharOrUTF8Begin:
       inc edx
      @IsUTF8Part:
     dec ecx
    jnz @Loop
   @LoopEnd:
   mov eax,edx
  pop esi
 @End:
end;
{$ELSE}
var CodeUnit:longint;
begin
 result:=0;
 for CodeUnit:=1 to length(s) do begin
  if (byte(s[CodeUnit]) and $c0)<>$80 then begin
   inc(result);
  end;
 end;
end;
{$ENDIF}

function BRREUTF8PtrLength(const s:ansistring;Len:longint):longint;
{$IFDEF cpu386} assembler; register;
asm
 test eax,eax
 jz @End
  push esi
   cld
   mov esi,eax
   mov ecx,edx
   xor edx,edx
   jecxz @LoopEnd
    @Loop:
      lodsb
      shl al,1
      js @IsASCIICharOrUTF8Begin
      jc @IsUTF8Part
      @IsASCIICharOrUTF8Begin:
       inc edx
      @IsUTF8Part:
     dec ecx
    jnz @Loop
   @LoopEnd:
   mov eax,edx
  pop esi
 @End:
end;
{$ELSE}
var CodeUnit:longint;
begin
 result:=0;
 for CodeUnit:=0 to Len-1 do begin
  if (byte(s[CodeUnit]) and $c0)<>$80 then begin
   inc(result);
  end;
 end;
end;
{$ENDIF}

function BRREUTF8LengthEx(const s:ansistring):longint;
var State:longword;
    CodeUnit:longint;
begin
 result:=0;
 State:=brreucACCEPT;
 for CodeUnit:=1 to length(s) do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  case State of
   brreucACCEPT:begin
    inc(result);
   end;
   brreucERROR:begin
    result:=0;
    exit;
   end;
  end;
 end;
 if State=brreucERROR then begin
  result:=0;
 end;
end;

function BRREUTF8GetCodePoint(const s:ansistring;CodeUnit:longint):longint;
var CurrentCodeUnit,Len:longint;
begin
 if CodeUnit<1 then begin
  result:=-1;
 end else begin
  result:=0;
  CurrentCodeUnit:=1;
  Len:=length(s);
  while (CurrentCodeUnit<=Len) and (CurrentCodeUnit<>CodeUnit) do begin
   inc(result);
   inc(CurrentCodeUnit,UTF8CharSteps[s[CurrentCodeUnit]]);
  end;
 end;
end;

function BRREUTF8PtrGetCodePoint(const s:pansichar;Len,CodeUnit:longint):longint;
var CurrentCodeUnit:longint;
begin
 result:=-1;
 if CodeUnit<0 then begin
  CurrentCodeUnit:=0;
  while (CurrentCodeUnit<Len) and (CurrentCodeUnit<>CodeUnit) do begin
   inc(result);
   inc(CurrentCodeUnit,UTF8CharSteps[s[CurrentCodeUnit]]);
  end;
 end;
end;

function BRREUTF8GetCodeUnit(const s:ansistring;CodePoint:longint):longint;
var CurrentCodePoint,Len:longint;
begin
 if CodePoint<0 then begin
  result:=0;
 end else begin
  result:=1;
  CurrentCodePoint:=0;
  Len:=length(s);
  while (result<=Len) and (CurrentCodePoint<>CodePoint) do begin
   inc(CurrentCodePoint);
   inc(result,UTF8CharSteps[s[result]]);
  end;
 end;
end;

function BRREUTF8PtrGetCodeUnit(const s:ansistring;Len,CodePoint:longint):longint;
var CurrentCodePoint:longint;
begin
 result:=-1;
 if CodePoint>=0 then begin
  result:=1;
  CurrentCodePoint:=0;
  Len:=length(s);
  while (result<Len) and (CurrentCodePoint<>CodePoint) do begin
   inc(CurrentCodePoint);
   inc(result,UTF8CharSteps[s[result]]);
  end;
 end;
end;

function BRREUTF8CodeUnitGetChar(const s:ansistring;CodeUnit:longint):longword;
var Value,CharClass,State:longword;
begin
 result:=0;
 if (CodeUnit>0) and (CodeUnit<=length(s)) then begin
  State:=brreucACCEPT;
  for CodeUnit:=CodeUnit to length(s) do begin
   Value:=byte(ansichar(s[CodeUnit]));
   CharClass:=UTF8DFACharClasses[ansichar(Value)];
   if State=brreucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=brreucERROR then begin
    break;
   end;
  end;
  if State<>brreucACCEPT then begin
   result:=$fffd;
  end;
 end;
end;

function BRREUTF8PtrCodeUnitGetChar(const s:pansichar;Len,CodeUnit:longint):longword;
var Value,CharClass,State:longword;
begin
 result:=0;
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  State:=brreucACCEPT;
  for CodeUnit:=CodeUnit to Len-1 do begin
   Value:=byte(ansichar(s[CodeUnit]));
   CharClass:=UTF8DFACharClasses[ansichar(Value)];
   if State=brreucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=brreucERROR then begin
    break;
   end;
  end;
  if State<>brreucACCEPT then begin
   result:=$fffd;
  end;
 end;
end;

function BRREUTF8CodeUnitGetCharAndInc(const s:ansistring;var CodeUnit:longint):longword;
var Len:longint;
    Value,CharClass,State:longword;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  State:=brreucACCEPT;
  while CodeUnit<=Len do begin
   Value:=byte(ansichar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=UTF8DFACharClasses[ansichar(Value)];
   if State=brreucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=brreucERROR then begin
    break;
   end;
  end;
  if State<>brreucACCEPT then begin
   result:=$fffd;
  end;
 end;
end;

function BRREUTF8PtrCodeUnitGetCharAndInc(const s:pansichar;Len:longint;var CodeUnit:longint):longword;
var Value,CharClass,State:longword;
begin
 result:=0;
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  State:=brreucACCEPT;
  while CodeUnit<Len do begin
   Value:=byte(ansichar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=UTF8DFACharClasses[ansichar(Value)];
   if State=brreucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=brreucERROR then begin
    break;
   end;
  end;
  if State<>brreucACCEPT then begin
   result:=$fffd;
  end;
 end;
end;

function BRREUTF8CodeUnitGetCharFallback(const s:ansistring;CodeUnit:longint):longword;
var Len:longint;
    StartCodeUnit,Value,CharClass,State:longword;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  StartCodeUnit:=CodeUnit;
  State:=brreucACCEPT;
  while CodeUnit<=Len do begin
   Value:=byte(ansichar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=UTF8DFACharClasses[ansichar(Value)];
   if State=brreucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=brreucERROR then begin
    break;
   end;
  end;
  if State<>brreucACCEPT then begin
   result:=byte(ansichar(s[StartCodeUnit]));
  end;
 end;
end;

function BRREUTF8CodeUnitGetCharAndIncFallback(const s:ansistring;var CodeUnit:longint):longword;
var Len:longint;
    StartCodeUnit,Value,CharClass,State:longword;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  StartCodeUnit:=CodeUnit;
  State:=brreucACCEPT;
  while CodeUnit<=Len do begin
   Value:=byte(ansichar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=UTF8DFACharClasses[ansichar(Value)];
   if State=brreucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=brreucERROR then begin
    break;
   end;
  end;
  if State<>brreucACCEPT then begin
   result:=byte(ansichar(s[StartCodeUnit]));
   CodeUnit:=StartCodeUnit+1;
  end;
 end;
end;

function BRREUTF8CodePointGetChar(const s:ansistring;CodePoint:longint;Fallback:boolean=false):longword;
begin
 result:=BRREUTF8CodeUnitGetChar(s,BRREUTF8GetCodeUnit(s,CodePoint));
end;

function BRREUTF8GetCharLen(const s:ansistring;i:longint):longword;
begin
 if (i>0) and (i<=length(s)) then begin
  result:=UTF8CharSteps[s[i]];
 end else begin
  result:=0;
 end;
end;

function BRREUTF8Pos(const FindStr,InStr:ansistring):longint;
var i,j,l:longint;
    ok:boolean;
begin
 result:=0;
 i:=1;
 while i<=length(InStr) do begin
  l:=i+length(FindStr)-1;
  if l>length(InStr) then begin
   exit;
  end;
  ok:=true;
  for j:=1 to length(FindStr) do begin
   if InStr[i+j-1]<>FindStr[j] then begin
    ok:=false;
    break;
   end;
  end;
  if ok then begin
   result:=i;
   exit;
  end;
  inc(i,UTF8CharSteps[InStr[i]]);
 end;
end;

function BRREUTF8Copy(const Str:ansistring;Start,Len:longint):ansistring;
var CodeUnit:longint;
begin
 result:='';
 CodeUnit:=1;
 while (CodeUnit<=length(Str)) and (Start>0) do begin
  inc(CodeUnit,UTF8CharSteps[Str[CodeUnit]]);
  dec(Start);
 end;
 if Start=0 then begin
  Start:=CodeUnit;
  while (CodeUnit<=length(Str)) and (Len>0) do begin
   inc(CodeUnit,UTF8CharSteps[Str[CodeUnit]]);
   dec(Len);
  end;
  if Start<CodeUnit then begin
   result:=copy(Str,Start,CodeUnit-Start);
  end;
 end;
end;

function BRREUTF8Uppercase(const Str:ansistring):ansistring;
var CodeUnit,Len,ResultLen:longint;
    StartCodeUnit,Value,CharClass,State,CharValue:longword;
    Data:pansichar;
begin
 result:='';
 CodeUnit:=1;
 Len:=length(Str);
 if Len>0 then begin
  SetLength(result,Len*{$IFDEF BRREStrictUTF8}4{$ELSE}6{$ENDIF});
  Data:=@result[1];
  ResultLen:=0;
  while CodeUnit<=Len do begin
   StartCodeUnit:=CodeUnit;
   State:=brreucACCEPT;
   CharValue:=0;
   while CodeUnit<=Len do begin
    Value:=byte(ansichar(Str[CodeUnit]));
    inc(CodeUnit);
    CharClass:=UTF8DFACharClasses[ansichar(Value)];
    if State=brreucACCEPT then begin
     CharValue:=Value and ($ff shr CharClass);
    end else begin
     CharValue:=(CharValue shl 6) or (Value and $3f);
    end;
    State:=UTF8DFATransitions[State+CharClass];
    if State<=brreucERROR then begin
     break;
    end;
   end;
   if State<>brreucACCEPT then begin
    CharValue:=byte(ansichar(Str[StartCodeUnit]));
    CodeUnit:=StartCodeUnit+1;
   end;
   if CharValue<=$10ffff then begin
    Value:=CharValue shr BRREUnicodeUpperCaseDeltaArrayBlockBits;
    CharValue:=longword(longint(longint(CharValue)+BRREUnicodeUpperCaseDeltaArrayBlockData[BRREUnicodeUpperCaseDeltaArrayIndexBlockData[BRREUnicodeUpperCaseDeltaArrayIndexIndexData[Value shr BRREUnicodeUpperCaseDeltaArrayIndexBlockBits],Value and BRREUnicodeUpperCaseDeltaArrayIndexBlockMask],CharValue and BRREUnicodeUpperCaseDeltaArrayBlockMask]));
   end;
   if CharValue<=$7f then begin
    Data[ResultLen]:=ansichar(byte(CharValue));
    inc(ResultLen);
   end else if CharValue<=$7ff then begin
    Data[ResultLen]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
    Data[ResultLen+1]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,2);
{$IFDEF BRREStrictUTF8}
   end else if CharValue<=$d7ff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$dfff then begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
{$ENDIF}
   end else if CharValue<=$ffff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$1fffff then begin
    Data[ResultLen]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,4);
{$ifndef BRREStrictUTF8}
   end else if CharValue<=$3ffffff then begin
    Data[ResultLen]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,5);
   end else if CharValue<=$7fffffff then begin
    Data[ResultLen]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+5]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,6);
{$ENDIF}
   end else begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
   end;
  end;
  SetLength(result,ResultLen);
 end;
end;

function BRREUTF8Lowercase(const Str:ansistring):ansistring;
var CodeUnit,Len,ResultLen:longint;
    StartCodeUnit,Value,CharClass,State,CharValue:longword;
    Data:pansichar;
begin
 result:='';
 CodeUnit:=1;
 Len:=length(Str);
 if Len>0 then begin
  SetLength(result,Len*{$IFDEF BRREStrictUTF8}4{$ELSE}6{$ENDIF});
  Data:=@result[1];
  ResultLen:=0;
  while CodeUnit<=Len do begin
   StartCodeUnit:=CodeUnit;
   State:=brreucACCEPT;
   CharValue:=0;
   while CodeUnit<=Len do begin
    Value:=byte(ansichar(Str[CodeUnit]));
    inc(CodeUnit);
    CharClass:=UTF8DFACharClasses[ansichar(Value)];
    if State=brreucACCEPT then begin
     CharValue:=Value and ($ff shr CharClass);
    end else begin
     CharValue:=(CharValue shl 6) or (Value and $3f);
    end;
    State:=UTF8DFATransitions[State+CharClass];
    if State<=brreucERROR then begin
     break;
    end;
   end;
   if State<>brreucACCEPT then begin
    CharValue:=byte(ansichar(Str[StartCodeUnit]));
    CodeUnit:=StartCodeUnit+1;
   end;
   if CharValue<=$10ffff then begin
    Value:=CharValue shr BRREUnicodeLowerCaseDeltaArrayBlockBits;
    CharValue:=longword(longint(longint(CharValue)+BRREUnicodeLowerCaseDeltaArrayBlockData[BRREUnicodeLowerCaseDeltaArrayIndexBlockData[BRREUnicodeLowerCaseDeltaArrayIndexIndexData[Value shr BRREUnicodeLowerCaseDeltaArrayIndexBlockBits],Value and BRREUnicodeLowerCaseDeltaArrayIndexBlockMask],CharValue and BRREUnicodeLowerCaseDeltaArrayBlockMask]));
   end;
   if CharValue<=$7f then begin
    Data[ResultLen]:=ansichar(byte(CharValue));
    inc(ResultLen);
   end else if CharValue<=$7ff then begin
    Data[ResultLen]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
    Data[ResultLen+1]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,2);
{$IFDEF BRREStrictUTF8}
   end else if CharValue<=$d7ff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$dfff then begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
{$ENDIF}
   end else if CharValue<=$ffff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$1fffff then begin
    Data[ResultLen]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,4);
{$ifndef BRREStrictUTF8}
   end else if CharValue<=$3ffffff then begin
    Data[ResultLen]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,5);
   end else if CharValue<=$7fffffff then begin
    Data[ResultLen]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+5]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,6);
{$ENDIF}
   end else begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
   end;
  end;
  SetLength(result,ResultLen);
 end;
end;

function BRREUTF8Trim(const Str:ansistring):ansistring;
var i,j:longint;
begin
 i:=1;
 while BRREUnicodeIsWhiteSpace(BRREUTF8CodeUnitGetChar(Str,i)) do begin
  inc(i,UTF8CharSteps[Str[i]]);
 end;
 j:=length(Str)+1;
 BRREUTF8Dec(Str,j);
 while BRREUnicodeIsWhiteSpace(BRREUTF8CodeUnitGetChar(Str,j)) do begin
  BRREUTF8Dec(Str,j);
 end;
 if (j<=length(Str)) and (Str[j]>=#80) then begin
  inc(j,longint(BRREUTF8GetCharLen(Str,j))-1);
 end;
 if i<=j then begin
  result:=copy(Str,i,(j-i)+1);
 end else begin
  result:='';
 end;
end;

function BRREUTF8Correct(const Str:ansistring):ansistring;
var CodeUnit,Len,ResultLen:longint;
    StartCodeUnit,Value,CharClass,State,CharValue:longword;
    Data:pansichar;
begin
 if (length(Str)=0) or BRREValidateUTF8(Str) then begin
  result:=Str;
 end else begin
  result:='';
  CodeUnit:=1;
  Len:=length(Str);
  SetLength(result,Len*{$IFDEF BRREStrictUTF8}4{$ELSE}6{$ENDIF});
  Data:=@result[1];
  ResultLen:=0;
  while CodeUnit<=Len do begin
   StartCodeUnit:=CodeUnit;
   State:=brreucACCEPT;
   CharValue:=0;
   while CodeUnit<=Len do begin
    Value:=byte(ansichar(Str[CodeUnit]));
    inc(CodeUnit);
    CharClass:=UTF8DFACharClasses[ansichar(Value)];
    if State=brreucACCEPT then begin
     CharValue:=Value and ($ff shr CharClass);
    end else begin
     CharValue:=(CharValue shl 6) or (Value and $3f);
    end;
    State:=UTF8DFATransitions[State+CharClass];
    if State<=brreucERROR then begin
     break;
    end;
   end;
   if State<>brreucACCEPT then begin
    CharValue:=byte(ansichar(Str[StartCodeUnit]));
    CodeUnit:=StartCodeUnit+1;
   end;
   if CharValue<=$7f then begin
    Data[ResultLen]:=ansichar(byte(CharValue));
    inc(ResultLen);
   end else if CharValue<=$7ff then begin
    Data[ResultLen]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
    Data[ResultLen+1]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,2);
{$IFDEF BRREStrictUTF8}
   end else if CharValue<=$d7ff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$dfff then begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
{$ENDIF}
   end else if CharValue<=$ffff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$1fffff then begin
    Data[ResultLen]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,4);
{$ifndef BRREStrictUTF8}
   end else if CharValue<=$3ffffff then begin
    Data[ResultLen]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,5);            
   end else if CharValue<=$7fffffff then begin
    Data[ResultLen]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+5]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,6);
{$ENDIF}
   end else begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
   end;
  end;
  SetLength(result,ResultLen);
 end;
end;

function BRREUTF8FromLatin1(const Str:ansistring):ansistring;
var CodeUnit:longint;
begin
 if BRREValidateUTF8(Str) then begin
  result:=Str;
 end else begin
  result:='';
  for CodeUnit:=1 to length(Str) do begin
   result:=result+BRREUTF32CharToUTF8(byte(ansichar(Str[CodeUnit])));
  end;
 end;
end;

function BRREUTF8LevenshteinDistance(const s,t:ansistring):longint;
var d:array of array of longint;
    n,m,i,j,ci,cj,oi,oj,Deletion,Insertion,Substitution:longint;
    si,tj:longword;
begin
 n:=BRREUTF8LengthEx(s);
 m:=BRREUTF8LengthEx(t);
 oi:=1;
 oj:=1;
 while ((n>0) and (m>0)) and (BRREUTF8CodeUnitGetChar(s,oi)=BRREUTF8CodeUnitGetChar(t,oj)) do begin
  if (oi>0) and (oi<=length(s)) then begin
   inc(oi,UTF8CharSteps[s[oi]]);
  end else begin
   break;
  end;
  if (oj>0) and (oj<=length(t)) then begin
   inc(oj,UTF8CharSteps[t[oj]]);
  end else begin
   break;
  end;
  dec(n);
  dec(m);
 end;
 if ((n>0) and (m>0)) and (s[length(s)]=t[length(t)]) then begin
  ci:=length(s)+1;
  cj:=length(t)+1;
  BRREUTF8Dec(s,ci);
  BRREUTF8Dec(t,cj);
  while ((n>0) and (m>0)) and (BRREUTF8CodeUnitGetChar(s,ci)=BRREUTF8CodeUnitGetChar(t,cj)) do begin
   BRREUTF8Dec(s,ci);
   BRREUTF8Dec(t,cj);
   dec(n);
   dec(m);
  end;
 end;
 if n=0 then begin
  result:=m;
 end else if m=0 then begin
  result:=n;
 end else begin
  d:=nil;
  SetLength(d,n+1,m+1);
  for i:=0 to n do begin
   d[i,0]:=i;
  end;
  for j:=0 to m do begin
   d[0,j]:=j;
  end;
  ci:=oi;
  for i:=1 to n do begin
   si:=BRREUTF8CodeUnitGetCharAndInc(s,ci);
   cj:=oj;
   for j:=1 to m do begin
    tj:=BRREUTF8CodeUnitGetCharAndInc(t,cj);
    if si<>tj then begin
     Deletion:=d[i-1,j]+1;
     Insertion:=d[i,j-1]+1;
     Substitution:=d[i-1,j-1]+1;
     if Deletion<Insertion then begin
      if Deletion<Substitution then begin
       d[i,j]:=Deletion;
      end else begin
       d[i,j]:=Substitution;
      end;
     end else begin
      if Insertion<Substitution then begin
       d[i,j]:=Insertion;
      end else begin
       d[i,j]:=Substitution;
      end;
     end;
    end else begin
     d[i,j]:=d[i-1,j-1];
    end;
   end;
  end;
  result:=d[n,m];
  SetLength(d,0);
 end;
end;

function BRREUTF8DamerauLevenshteinDistance(const s,t:ansistring):longint;
var d:array of array of longint;
    n,m,i,j,ci,cj,oi,oj,Cost,Deletion,Insertion,Substitution,Transposition,Value:longint;
    si,tj,lsi,ltj:longword;
begin
 n:=BRREUTF8LengthEx(s);
 m:=BRREUTF8LengthEx(t);
 oi:=1;
 oj:=1;
 while ((n>0) and (m>0)) and (BRREUTF8CodeUnitGetChar(s,oi)=BRREUTF8CodeUnitGetChar(t,oj)) do begin
  if (oi>0) and (oi<=length(s)) then begin
   inc(oi,UTF8CharSteps[s[oi]]);
  end else begin
   break;
  end;
  if (oj>0) and (oj<=length(t)) then begin
   inc(oj,UTF8CharSteps[t[oj]]);
  end else begin
   break;
  end;
  dec(n);
  dec(m);
 end;
 if ((n>0) and (m>0)) and (s[length(s)]=t[length(t)]) then begin
  ci:=length(s)+1;
  cj:=length(t)+1;
  BRREUTF8Dec(s,ci);
  BRREUTF8Dec(t,cj);
  while ((n>0) and (m>0)) and (BRREUTF8CodeUnitGetChar(s,ci)=BRREUTF8CodeUnitGetChar(t,cj)) do begin
   BRREUTF8Dec(s,ci);
   BRREUTF8Dec(t,cj);
   dec(n);
   dec(m);
  end;
 end;
 if n=0 then begin
  result:=m;
 end else if m=0 then begin
  result:=n;
 end else begin
  d:=nil;
  SetLength(d,n+1,m+1);
  for i:=0 to n do begin
   d[i,0]:=i;
  end;
  for j:=0 to m do begin
   d[0,j]:=j;
  end;
  ci:=oi;
  si:=0;
  for i:=1 to n do begin
   lsi:=si;
   si:=BRREUTF8CodeUnitGetCharAndInc(s,ci);
   cj:=oj;
   tj:=0;
   for j:=1 to m do begin
    ltj:=tj;
    tj:=BRREUTF8CodeUnitGetCharAndInc(t,cj);
    if si<>tj then begin
     Cost:=1;
    end else begin
     Cost:=0;
    end;
    Deletion:=d[i-1,j]+1;
    Insertion:=d[i,j-1]+1;
    Substitution:=d[i-1,j-1]+Cost;
    if Deletion<Insertion then begin
     if Deletion<Substitution then begin
      Value:=Deletion;
     end else begin
      Value:=Substitution;
     end;
    end else begin
     if Insertion<Substitution then begin
      Value:=Insertion;
     end else begin
      Value:=Substitution;
     end;
    end;
    if ((i>1) and (j>1)) and ((si=ltj) and (lsi=tj)) then begin
     Transposition:=d[i-2,j-2]+Cost;
     if Transposition<Value then begin
      Value:=Transposition;
     end;
    end;
    d[i,j]:=Value;
   end;
  end;
  result:=d[n,m];
  SetLength(d,0);
 end;
end;

function BRREStringLength(const s:ansistring):longint;
begin
 if BRREIsUTF8(s) then begin
  result:=BRREUTF8Length(s);
 end else begin
  result:=length(s);
 end;
end;

function PtrLength(p:pansichar):longint;
{$IFDEF cpu386}
asm
 push ebx
 push esi

 // if not assigned(p) then begin
 test eax,eax
 jnz @NotNull
  // result:=0;
  xor eax,eax
  // exit;
  jmp @Done
 @NotNull:

 // Start:=p;
 mov ecx,eax

 // Value:=((not ptruint(p))+1) and (sizeof(ptruint)-1);
 mov edx,eax
 not edx
 inc edx
 and edx,3

 // while Value>0 do begin
 test edx,edx
 jz @NonAlignedLoopDone
  @NonAlignedLoop:
   // if p^=#0 then begin
   cmp byte ptr [eax],0
   jnz @NonAlignedLoopNotNull
    // result:=ptruint(ptruint(p)-ptruint(Start));
    sub eax,ecx
    // exit;
    jmp @Done
   @NonAlignedLoopNotNull:
   inc eax
   dec edx
  jnz @NonAlignedLoop
 @NonAlignedLoopDone:

 @AlignedLoop:
   // Value:=ptruint(pointer(p)^);
   mov edx,dword ptr [eax]

   // if (((Value+MaskA) and not Value) and MaskB)<>0 then begin
   lea ebx,[edx-$01010101]
   mov esi,edx
   not esi
   and ebx,esi
   test ebx,$80808080
   // break;
   jnz @AlignedLoopDone
   // inc(p,sizeof(ptruint));
   add eax,4
  jmp @AlignedLoop

 // while (Value<>0) and ((Value and $ff)<>0) do begin
 @AfterAlignedLoop:

  // Value:=Value shr 8;
  shr edx,8

  // inc(p);
  inc eax

  @AlignedLoopDone:
  test edx,edx
  jz @AfterAlignedLoopDone
  test dl,$ff
  jnz @AfterAlignedLoop
 @AfterAlignedLoopDone:

 // result:=ptruint(ptruint(p)-ptruint(Start));
 sub eax,ecx

 @Done:

 pop esi
 pop ebx
end;
{$ELSE}
const MaskA=ptruint({$IFDEF cpu64}$fefefefefefefeff{$ELSE}$fefefeff{$ENDIF}); // it is: 0-$01010101 / 0-$0101010101010101
      MaskB=ptruint({$IFDEF cpu64}$8080808080808080{$ELSE}$80808080{$ENDIF});
var Start:pointer;
    Value:ptruint;
begin

 // If it is a null pointer, so return zero as length
 if not assigned(p) then begin
  result:=0;
  exit;
 end;

 Start:=p;

 // Check non-aligned first bytes
 Value:=((not ptruint(p))+1) and (sizeof(ptruint)-1);
 while Value>0 do begin
  if p^=#0 then begin
   result:=ptruint(ptruint(p)-ptruint(Start));
   exit;
  end;
  inc(p);
  dec(Value);
 end;

 // Then check chunkwise
 repeat
  Value:=ptruint(pointer(p)^);
  if (((Value+MaskA) and not Value) and MaskB)<>0 then begin
   break;
  end;
  inc(p,sizeof(ptruint));
 until false;

 // Thgen check last chunk bytewise
{$IFDEF BRRELENSAFECHECK}
 while p^<>#0 do begin
  inc(p);
 end;
{$ELSE}
{$IFDEF BIG_ENDIAN}
 inc(p,sizeof(ptruint)-1);
{$ENDIF}
 while (Value<>0) and ((Value and $ff)<>0) do begin
  Value:=Value shr 8;
  {$IFDEF BIG_ENDIAN}dec{$ELSE}inc{$ENDIF}(p);
 end;
{$ENDIF}

 // Return the length
 result:=ptruint(ptruint(p)-ptruint(Start));
end;
{$ENDIF}

function PtrPosChar(const Pattern:ansichar;const Text:pansichar;TextLength:longint;Offset:longint=0):longint;
type pptruint=^ptruint;
const MaskA=ptruint({$IFDEF cpu64}$fefefefefefefeff{$ELSE}$fefefeff{$ENDIF}); // it is: 0-$01010101 / 0-$0101010101010101
      MaskB=ptruint({$IFDEF cpu64}$8080808080808080{$ELSE}$80808080{$ENDIF});
var CurrentChar:pansichar;
    CurrentChunk:pptruint;
    XorMask,XoredChunk,Size:ptruint;
begin
 result:=-1;

 Size:=(TextLength-Offset)+1;
 if Size>0 then begin

  XorMask:=byte(Pattern);
  XorMask:=XorMask or (XorMask shl 8);
  XorMask:=XorMask or (XorMask shl 16);
{$IFDEF cpu64}
  XorMask:=XorMask or (XorMask shl 32);
{$ENDIF}

  CurrentChar:=@Text[Offset];

  if Size>(sizeof(ptruint)*2) then begin

   // Alignment initialization
   CurrentChunk:=pointer(ptruint(ptruint(CurrentChar) and not (sizeof(ptruint)-1)));

   // Try to get first chunk
   if ptruint(CurrentChunk)>=ptruint(Text) then begin
    // Yes, we can the get first chunk
    XoredChunk:=CurrentChunk^ xor XorMask;
   end else begin
    // No, so return dummy value to force to check the few first characters
    XoredChunk:=0;
   end;

   // Jump to next chunk
   inc(CurrentChunk);

   // Subtract the first chunk from size
   dec(ptruint(Size),ptruint(CurrentChunk)-ptruint(CurrentChar));

   // Scan first chunk
   if (((XoredChunk+MaskA) and not XoredChunk) and MaskB)<>0 then begin
    while ptruint(CurrentChar)<ptruint(CurrentChunk) do begin
     if CurrentChar^=Pattern then begin
      result:=ptruint(CurrentChar)-ptruint(Text);
      exit;
     end;
     inc(CurrentChar);
    end;
   end;

   // Scan until the last whole chunk
   while Size>=sizeof(ptruint) do begin
    XoredChunk:=CurrentChunk^ xor XorMask;
    if (((XoredChunk+MaskA) and not XoredChunk) and MaskB)<>0 then begin
{$IFDEF BRREPOSCHARSAFECHECK}
    CurrentChar:=pointer(CurrentChunk);
    if CurrentChar[0]=Pattern then begin
      result:=ptruint(pointer(@CurrentChar[0]))-ptruint(Text);
      exit;
     end;
     if CurrentChar[1]=Pattern then begin
      result:=ptruint(pointer(@CurrentChar[1]))-ptruint(Text);
      exit;
     end;
     if CurrentChar[2]=Pattern then begin
      result:=ptruint(pointer(@CurrentChar[2]))-ptruint(Text);
      exit;
     end;
     if CurrentChar[3]=Pattern then begin
      result:=ptruint(pointer(@CurrentChar[3]))-ptruint(Text);
      exit;
     end;
{$IFDEF cpu64}
     if CurrentChar[4]=Pattern then begin
      result:=ptruint(pointer(@CurrentChar[4]))-ptruint(Text);
      exit;
     end;
     if CurrentChar[5]=Pattern then begin
      result:=ptruint(pointer(@CurrentChar[5]))-ptruint(Text);
      exit;
     end;
     if CurrentChar[6]=Pattern then begin
      result:=ptruint(pointer(@CurrentChar[6]))-ptruint(Text);
      exit;
     end;
     if CurrentChar[7]=Pattern then begin
      result:=ptruint(pointer(@CurrentChar[7]))-ptruint(Text);
      exit;
     end;
{$ENDIF}
{$ELSE}
     CurrentChar:=pointer({$IFDEF BIG_ENDIAN}ptruint(ptruint(CurrentChunk)+ptruint(sizeof(ptruint)-1)){$ELSE}CurrentChunk{$ENDIF});
     XoredChunk:=XoredChunk xor XorMask;
     while (XoredChunk<>0) and ((XoredChunk and $ff)<>byte(Pattern)) do begin
      XoredChunk:=XoredChunk shr 8;
      {$IFDEF BIG_ENDIAN}dec{$ELSE}inc{$ENDIF}(CurrentChar);
     end;
     if XoredChunk<>0 then begin
      result:=ptruint(pointer(CurrentChar))-ptruint(Text);
      exit;
     end;
{$ENDIF}
    end;
    inc(CurrentChunk);
    dec(Size,sizeof(ptruint));
   end;

   // Set chunkwise to charwise pointer
   CurrentChar:=pointer(CurrentChunk);
  end;

  // Scan rest of the remained characters, if there are any
  while Size>0 do begin
   if CurrentChar^=Pattern then begin
    result:=ptruint(pointer(CurrentChar))-ptruint(Text);
    exit;
   end;
   inc(CurrentChar);
   dec(Size);
  end;

 end;
end;

function PosChar(const Pattern:ansichar;const Text:ansistring;Offset:longint=1):longint;
begin
 result:=length(Text);
 if result>0 then begin
  result:=PtrPosChar(Pattern,@Text[1],result,Offset-1)+1;
 end;
end;

function PosEx(const Pattern,Text:ansistring;Offset:longint=1):longint;
var Position,Found,LenPattern,Len:longint;
begin
 result:=0;
 LenPattern:=length(Pattern);
 Position:=Offset;
 Len:=(length(Text)-LenPattern)+1;
 while Position<=Len do begin
  if Text[Position]<>Pattern[1] then begin
   Position:=PosChar(Pattern[1],Text,Position);
   if Position=0 then begin
    break;
   end;
  end;
  Found:=1;
  while (Found<LenPattern) and (Text[Position+Found]=Pattern[Found+1]) do begin
   inc(Found);
  end;
  if Found=LenPattern then begin
   result:=Position;
   exit;
  end;
  inc(Position);
 end;
end;     

function PosCharUTF8(const Pattern:ansichar;const Text:ansistring;var CodePoint:longint;Offset:longint=1):longint;
var CodeUnit:longint;
    State:longword;
begin
 result:=0;
 State:=brreucACCEPT;
 for CodeUnit:=Offset to length(Text) do begin
  if Text[CodeUnit]=Pattern then begin
   result:=CodeUnit;
   exit;
  end;
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[Text[CodeUnit]]];
  if State in [brreucACCEPT,brreucERROR] then begin
   State:=brreucACCEPT;
   inc(CodePoint);
  end;
 end;
end;

function PosExUTF8(const Pattern,Text:ansistring;var CodePoint:longint;Offset:longint=1):longint;
var CodeUnit,x,LenPattern:longint;
    State:longword;
begin
 result:=0;
 LenPattern:=length(Pattern);
 State:=brreucACCEPT;
 for CodeUnit:=Offset to (length(Text)-LenPattern)+1 do begin
  if Text[CodeUnit]=Pattern[1] then begin
   x:=1;
   while (x<LenPattern) and (Text[CodeUnit+x]=Pattern[x+1]) do begin
    inc(x);
   end;
   if x=LenPattern then begin
    result:=CodeUnit;
    exit;
   end;
  end;
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[Text[CodeUnit]]];
  if State in [brreucACCEPT,brreucERROR] then begin
   State:=brreucACCEPT;
   inc(CodePoint);
  end;
 end;
end;

function PosShiftOr(const Pattern,Text:ansistring;const FixedStringShiftOrPatternMaskBoyerMooreSkip:TBRRERegExpShiftOrPatternMaskBoyerMooreSkip;Offset:longint=1):longint;
var i,m:longint;
    r:longword;
begin
 m:=length(Pattern);
 result:=0;
 if m<>0 then begin
  Offset:=PtrPosChar(Pattern[1],@Text[1],length(Text),Offset-1)+1;
  if Offset>0 then begin
   r:=$fffffffe;
   for i:=Offset to length(Text) do begin
    r:=(r or FixedStringShiftOrPatternMaskBoyerMooreSkip[Text[i]]) shl 1;
    if (r and (1 shl m))=0 then begin
     result:=(i-m)+1;
     exit;
    end;
   end;
  end;
 end;
end;

function PosShiftOrUTF8(const Pattern,Text:ansistring;const FixedStringShiftOrPatternMaskBoyerMooreSkip:TBRRERegExpShiftOrPatternMaskBoyerMooreSkip;var CodePoint:longint;Offset:longint=1):longint;
var CodeUnit,m:longint;
    r,State:longword;
    Value:ansichar;
begin
 result:=0;
 m:=length(Pattern);
 if m<>0 then begin
  r:=$fffffffe;
  State:=brreucACCEPT;
  for CodeUnit:=Offset to length(Text) do begin
   Value:=Text[CodeUnit];
   r:=(r or FixedStringShiftOrPatternMaskBoyerMooreSkip[Value]) shl 1;
   if (r and (1 shl m))=0 then begin
    result:=(CodeUnit-m)+1;
    exit;
   end;
   State:=UTF8DFATransitions[State+UTF8DFACharClasses[Value]];
   if State in [brreucACCEPT,brreucERROR] then begin
    State:=brreucACCEPT;
    inc(CodePoint);
   end;
  end;
 end;
end;

function PosBoyerMoore(const Pattern,Text:ansistring;const FixedStringShiftOrPatternMaskBoyerMooreSkip:TBRRERegExpShiftOrPatternMaskBoyerMooreSkip;const FixedStringBoyerMooreNext:TBRRERegExpBoyerMooreNext;Offset:longint=1):longint;
var i,j,k,h,m,n:longint;
begin
 m:=length(Pattern);
 result:=0;
 if m<>0 then begin
  n:=length(Text);
  Offset:=PtrPosChar(Pattern[1],@Text[1],n,Offset-1)+1;
  if Offset>0 then begin
   i:=(Offset+m)-1;
   while i<=n do begin
    j:=0;
    while (j<m) and (Text[i-j]=Pattern[m-j]) do begin
     inc(j);
    end;
    if j<>m then begin
     k:=FixedStringShiftOrPatternMaskBoyerMooreSkip[Text[i-j]];
     h:=FixedStringBoyerMooreNext[j];
     if k>h then begin
      inc(i,k-j);
     end else begin
      inc(i,h);
     end;
    end else begin
     result:=(i-m)+1;
     exit;
    end;
   end;
  end;
 end;
end;

function CompareEx(const Pattern,Text:ansistring;Offset:longint=1):boolean;
var i,x,LenPattern:longint;
begin
 result:=false;
 i:=Offset;
 LenPattern:=length(Pattern);
 if (i<=((Length(Text)-LenPattern)+1)) and (Text[i]=Pattern[1]) then begin
  x:=1;
  while (x<LenPattern) and (Text[i+x]=Pattern[x+1]) do begin
   inc(x);
  end;
  result:=x=LenPattern;
 end;
end;

function PtrPosCharRange(const First,Last:ansichar;const Text:pansichar;TextLength:longint;Offset:longint=0):longint;
var CodeUnit:longint;
    CurrentChar:ansichar;
begin
 result:=-1;
 for CodeUnit:=Offset to TextLength-1 do begin
  CurrentChar:=Text[CodeUnit];
  if (CurrentChar>=First) and (CurrentChar<=Last) then begin
   result:=CodeUnit;
   exit;
  end;
 end;
end;

function PtrPosEx(const Pattern:ansistring;const Text:pansichar;TextLength:longint;Offset:longint=0):longint;
var Position,Found,LenPattern,Len:longint;
begin
 result:=-1;
 LenPattern:=length(Pattern);
 Position:=Offset;
 Len:=(TextLength-LenPattern)+1;
 while Position<Len do begin
  if Text[Position]<>Pattern[1] then begin
   Position:=PtrPosChar(Pattern[1],Text,TextLength,Position);
   if Position<0 then begin
    break;
   end;
  end;
  Found:=1;
  while (Found<LenPattern) and (Text[Position+Found]=Pattern[Found+1]) do begin
   inc(Found);
  end;
  if Found=LenPattern then begin
   result:=Position;
   exit;
  end;
  inc(Position);
 end;
end;

function PtrPosCharUTF8(const Pattern:ansichar;const Text:pansichar;TextLength:longint;var CodePoint:longint;Offset:longint=0):longint;
var CodeUnit:longint;
    State:longword;
begin
 result:=-1;
 State:=brreucACCEPT;
 for CodeUnit:=Offset to TextLength-1 do begin
  if Text[CodeUnit]=Pattern then begin
   result:=CodeUnit;
   exit;
  end;
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[Text[CodeUnit]]];
  if State in [brreucACCEPT,brreucERROR] then begin
   State:=brreucACCEPT;
   inc(CodePoint);
  end;
 end;
end;

function PtrPosCharRangeUTF8(const First,Last:ansichar;const Text:pansichar;TextLength:longint;var CodePoint:longint;Offset:longint=0):longint;
var CodeUnit:longint;
    State:longword;
    CurrentChar:ansichar;
begin
 result:=-1;
 State:=brreucACCEPT;
 for CodeUnit:=Offset to TextLength-1 do begin
  CurrentChar:=Text[CodeUnit];
  if (CurrentChar>=First) and (CurrentChar<=Last) then begin
   result:=CodeUnit;
   exit;
  end;
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[Text[CodeUnit]]];
  if State in [brreucACCEPT,brreucERROR] then begin
   State:=brreucACCEPT;
   inc(CodePoint);
  end;
 end;
end;

function PtrPosExUTF8(const Pattern:ansistring;const Text:pansichar;TextLength:longint;var CodePoint:longint;Offset:longint=0):longint;
var CodeUnit,x,LenPattern:longint;
    State:longword;
begin
 result:=-1;
 LenPattern:=length(Pattern);
 State:=brreucACCEPT;
 for CodeUnit:=Offset to TextLength-LenPattern do begin
  if Text[CodeUnit]=Pattern[1] then begin
   x:=1;
   while (x<LenPattern) and (Text[CodeUnit+x]=Pattern[x+1]) do begin
    inc(x);
   end;
   if x=LenPattern then begin
    result:=CodeUnit;
    exit;
   end;
  end;
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[Text[CodeUnit]]];
  if State in [brreucACCEPT,brreucERROR] then begin
   State:=brreucACCEPT;
   inc(CodePoint);
  end;
 end;
end;

function PtrPosShiftOr(const Pattern:ansistring;const Text:pansichar;TextLength:longint;const FixedStringShiftOrPatternMaskBoyerMooreSkip:TBRRERegExpShiftOrPatternMaskBoyerMooreSkip;Offset:longint=0):longint;
var i,m:longint;
    r:longword;
begin
 m:=length(Pattern);
 result:=-1;
 if m<>0 then begin
  Offset:=PtrPosChar(Pattern[1],Text,TextLength,Offset);
  if Offset>=0 then begin
   r:=$fffffffe;
   for i:=Offset to TextLength-1 do begin
    r:=(r or FixedStringShiftOrPatternMaskBoyerMooreSkip[Text[i]]) shl 1;
    if (r and (1 shl m))=0 then begin
     result:=(i-m)+1;
     exit;
    end;
   end;
  end;
 end;
end;

function PtrPosShiftOrUTF8(const Pattern:ansistring;const Text:pansichar;TextLength:longint;const FixedStringShiftOrPatternMaskBoyerMooreSkip:TBRRERegExpShiftOrPatternMaskBoyerMooreSkip;var CodePoint:longint;Offset:longint=0):longint;
var CodeUnit,m:longint;
    r,State:longword;
    Value:ansichar;
begin
 m:=length(Pattern);
 result:=-1;
 if m<>0 then begin
  r:=$fffffffe;
  State:=brreucACCEPT;
  for CodeUnit:=Offset to TextLength-1 do begin
   Value:=Text[CodeUnit];
   r:=(r or FixedStringShiftOrPatternMaskBoyerMooreSkip[Value]) shl 1;
   if (r and (1 shl m))=0 then begin
    result:=(CodeUnit-m)+1;
    exit;
   end;
   State:=UTF8DFATransitions[State+UTF8DFACharClasses[Value]];
   if State in [brreucACCEPT,brreucERROR] then begin
    State:=brreucACCEPT;
    inc(CodePoint);
   end;
  end;
 end;
end;

function PtrPosBoyerMoore(const Pattern:ansistring;const Text:pansichar;TextLength:longint;const FixedStringShiftOrPatternMaskBoyerMooreSkip:TBRRERegExpShiftOrPatternMaskBoyerMooreSkip;const FixedStringBoyerMooreNext:TBRRERegExpBoyerMooreNext;Offset:longint=0):longint;
var i,j,k,h,m:longint;
begin
 m:=length(Pattern);
 result:=-1;
 if m<>0 then begin
  Offset:=PtrPosChar(Pattern[1],Text,TextLength,Offset);
  if Offset>=0 then begin
   i:=(Offset+m)-1;
   while i<TextLength do begin
    j:=0;
    while (j<m) and (Text[i-j]=Pattern[m-j]) do begin
     inc(j);
    end;
    if j<>m then begin
     k:=FixedStringShiftOrPatternMaskBoyerMooreSkip[Text[i-j]];
     h:=FixedStringBoyerMooreNext[j];
     if k>h then begin
      inc(i,k-j);
     end else begin
      inc(i,h);
     end;
    end else begin
     result:=(i-m)+1;
     exit;
    end;
   end;
  end;
 end;
end;

function PtrCompareEx(const Pattern:ansistring;const Text:pansichar;TextLength:longint;Offset:longint=0):boolean;
var i,x,LenPattern:longint;
begin
 result:=false;
 i:=Offset;
 LenPattern:=length(Pattern);
 if (i<=(TextLength-LenPattern)) and (Text[i]=Pattern[1]) then begin
  x:=1;
  while (x<LenPattern) and (Text[i+x]=Pattern[x+1]) do begin
   inc(x);
  end;
  result:=x=LenPattern;
 end;
end;

constructor TBRREStringHashMap.Create;
begin
 inherited Create;
 RealSize:=0;
 LogSize:=0;
 Size:=0;
 Entities:=nil;
 EntityToCellIndex:=nil;
 CellToEntityIndex:=nil;
 Resize;
end;

destructor TBRREStringHashMap.Destroy;
begin
 Clear;
 SetLength(Entities,0);
 inherited Destroy;
end;

procedure TBRREStringHashMap.Clear;
var Counter:longint;
begin
 for Counter:=0 to length(Entities)-1 do begin
  Entities[Counter].Key:='';
 end;
 RealSize:=0;
 LogSize:=0;
 Size:=0;
 SetLength(Entities,0);
 SetLength(EntityToCellIndex,0);
 SetLength(CellToEntityIndex,0);
 Resize;
end;

function TBRREStringHashMap.FindCell(const Key:ansistring):longword;
var HashCode,Mask,Step:longword;
    Entity:longint;
begin
 HashCode:=BRREHashString(Key);
 Mask:=(2 shl LogSize)-1;
 Step:=((HashCode shl 1)+1) and Mask;
 if LogSize<>0 then begin
  result:=HashCode shr (32-LogSize);
 end else begin
  result:=0;
 end;
 repeat
  Entity:=CellToEntityIndex[result];
  if (Entity=ENT_EMPTY) or ((Entity<>ENT_DELETED) and (Entities[Entity].Key=Key)) then begin
   exit;
  end;
  result:=(result+Step) and Mask;
 until false;
end;

procedure TBRREStringHashMap.Resize;
var NewLogSize,NewSize,Cell,Entity,Counter:longint;
    OldEntities:TBRREStringHashMapEntities;
    OldCellToEntityIndex:TBRREStringHashMapEntityIndices;
    OldEntityToCellIndex:TBRREStringHashMapEntityIndices;
begin
 NewLogSize:=0;
 NewSize:=RealSize;
 while NewSize<>0 do begin
  NewSize:=NewSize shr 1;
  inc(NewLogSize);
 end;
 if NewLogSize<1 then begin
  NewLogSize:=1;
 end;
 Size:=0;
 RealSize:=0;
 LogSize:=NewLogSize;
 OldEntities:=Entities;
 OldCellToEntityIndex:=CellToEntityIndex;
 OldEntityToCellIndex:=EntityToCellIndex;
 Entities:=nil;
 CellToEntityIndex:=nil;
 EntityToCellIndex:=nil;
 SetLength(Entities,2 shl LogSize);
 SetLength(CellToEntityIndex,2 shl LogSize);
 SetLength(EntityToCellIndex,2 shl LogSize);
 for Counter:=0 to length(CellToEntityIndex)-1 do begin
  CellToEntityIndex[Counter]:=ENT_EMPTY;
 end;
 for Counter:=0 to length(EntityToCellIndex)-1 do begin
  EntityToCellIndex[Counter]:=CELL_EMPTY;
 end;
 for Counter:=0 to length(OldEntityToCellIndex)-1 do begin
  Cell:=OldEntityToCellIndex[Counter];
  if Cell>=0 then begin
   Entity:=OldCellToEntityIndex[Cell];
   if Entity>=0 then begin
    Add(OldEntities[Counter].Key,OldEntities[Counter].Value);
   end;
  end;
 end;
end;

function TBRREStringHashMap.Add(const Key:ansistring;Value:int64):PBRREStringHashMapEntity;
var Entity:longint;
    Cell:longword;
begin
 result:=nil;
 while RealSize>=(1 shl LogSize) do begin
  Resize;
 end;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@Entities[Entity];
  result^.Key:=Key;
  result^.Value:=Value;
  exit;
 end;
 Entity:=Size;
 inc(Size);
 if Entity<(2 shl LogSize) then begin
  CellToEntityIndex[Cell]:=Entity;
  EntityToCellIndex[Entity]:=Cell;
  inc(RealSize);
  result:=@Entities[Entity];
  result^.Key:=Key;
  result^.Value:=Value;
 end;
end;

function TBRREStringHashMap.Get(const Key:ansistring;CreateIfNotExist:boolean=false):PBRREStringHashMapEntity;
var Entity:longint;
    Cell:longword;
begin
 result:=nil;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@Entities[Entity];
 end else if CreateIfNotExist then begin
  result:=Add(Key,0);
 end;
end;

function TBRREStringHashMap.Delete(const Key:ansistring):boolean;
var Entity:longint;
    Cell:longword;
begin
 result:=false;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  Entities[Entity].Key:='';
  Entities[Entity].Value:=0;
  EntityToCellIndex[Entity]:=CELL_DELETED;
  CellToEntityIndex[Cell]:=ENT_DELETED;
  dec(RealSize);
  result:=true;
 end;
end;

function TBRREStringHashMap.GetValue(const Key:ansistring):int64;
var Entity:longint;
    Cell:longword;
begin
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=Entities[Entity].Value;
 end else begin
  result:=-1;
 end;
end;

procedure TBRREStringHashMap.SetValue(const Key:ansistring;const Value:int64);
begin
 Add(Key,Value);
end;

constructor TBRREHashMap.Create;
begin
 inherited Create;
 RealSize:=0;
 LogSize:=0;
 Size:=0;
 Entities:=nil;
 EntityToCellIndex:=nil;
 CellToEntityIndex:=nil;
 Resize;
end;

destructor TBRREHashMap.Destroy;
begin
 Clear;
 SetLength(Entities,0);
 inherited Destroy;
end;

procedure TBRREHashMap.Clear;
var Counter:longint;
begin
 for Counter:=0 to length(Entities)-1 do begin
  Entities[Counter].Key:=nil;
 end;
 RealSize:=0;
 LogSize:=0;
 Size:=0;
 SetLength(Entities,0);
 SetLength(EntityToCellIndex,0);
 SetLength(CellToEntityIndex,0);
 Resize;
end;

function TBRREHashMap.FindCell(const Key:TBRREHashableObject):longword;
var HashCode,Mask,Step:longword;
    Entity:longint;
begin
 if assigned(Key) then begin
  HashCode:=Key.HashCode;
 end else begin
  HashCode:=$ffffffff;
 end;
 Mask:=(2 shl LogSize)-1;
 Step:=((HashCode shl 1)+1) and Mask;
 if LogSize<>0 then begin
  result:=HashCode shr (32-LogSize);
 end else begin
  result:=0;
 end;
 repeat
  Entity:=CellToEntityIndex[result];
  if (Entity=ENT_EMPTY) or ((Entity<>ENT_DELETED) and (assigned(Key) and (Entities[Entity].Key.EqualsTo(Key)))) then begin
   exit;
  end;
  result:=(result+Step) and Mask;
 until false;
end;

procedure TBRREHashMap.Resize;
var NewLogSize,NewSize,Cell,Entity,Counter:longint;
    OldEntities:TBRREHashMapEntities;
    OldCellToEntityIndex:TBRREHashMapEntityIndices;
    OldEntityToCellIndex:TBRREHashMapEntityIndices;
begin
 NewLogSize:=0;
 NewSize:=RealSize;
 while NewSize<>0 do begin
  NewSize:=NewSize shr 1;
  inc(NewLogSize);
 end;
 if NewLogSize<1 then begin
  NewLogSize:=1;
 end;
 Size:=0;
 RealSize:=0;
 LogSize:=NewLogSize;
 OldEntities:=Entities;
 OldCellToEntityIndex:=CellToEntityIndex;
 OldEntityToCellIndex:=EntityToCellIndex;
 Entities:=nil;
 CellToEntityIndex:=nil;
 EntityToCellIndex:=nil;
 SetLength(Entities,2 shl LogSize);
 SetLength(CellToEntityIndex,2 shl LogSize);
 SetLength(EntityToCellIndex,2 shl LogSize);
 for Counter:=0 to length(CellToEntityIndex)-1 do begin
  CellToEntityIndex[Counter]:=ENT_EMPTY;
 end;
 for Counter:=0 to length(EntityToCellIndex)-1 do begin
  EntityToCellIndex[Counter]:=CELL_EMPTY;
 end;
 for Counter:=0 to length(OldEntityToCellIndex)-1 do begin
  Cell:=OldEntityToCellIndex[Counter];
  if Cell>=0 then begin
   Entity:=OldCellToEntityIndex[Cell];
   if Entity>=0 then begin
    Add(OldEntities[Counter].Key,OldEntities[Counter].Value);
   end;
  end;
 end;
end;

function TBRREHashMap.Add(const Key:TBRREHashableObject;Value:int64):PBRREHashMapEntity;
var Entity:longint;
    Cell:longword;
begin
 result:=nil;
 while RealSize>=(1 shl LogSize) do begin
  Resize;
 end;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@Entities[Entity];
  result^.Key:=Key;
  result^.Value:=Value;
  exit;
 end;
 Entity:=Size;
 inc(Size);
 if Entity<(2 shl LogSize) then begin
  CellToEntityIndex[Cell]:=Entity;
  EntityToCellIndex[Entity]:=Cell;
  inc(RealSize);
  result:=@Entities[Entity];
  result^.Key:=Key;
  result^.Value:=Value;
 end;
end;

function TBRREHashMap.Get(const Key:TBRREHashableObject;CreateIfNotExist:boolean=false):PBRREHashMapEntity;
var Entity:longint;
    Cell:longword;
begin
 result:=nil;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@Entities[Entity];
 end else if CreateIfNotExist then begin
  result:=Add(Key,0);
 end;
end;

function TBRREHashMap.Delete(const Key:TBRREHashableObject):boolean;
var Entity:longint;
    Cell:longword;
begin
 result:=false;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  Entities[Entity].Key:=nil;
  Entities[Entity].Value:=0;
  EntityToCellIndex[Entity]:=CELL_DELETED;
  CellToEntityIndex[Cell]:=ENT_DELETED;
  dec(RealSize);
  result:=true;
 end;
end;

function TBRREHashMap.GetValue(const Key:TBRREHashableObject):int64;
var Entity:longint;
    Cell:longword;
begin
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=Entities[Entity].Value;
 end else begin
  result:=-1;
 end;
end;

procedure TBRREHashMap.SetValue(const Key:TBRREHashableObject;const Value:int64);
begin
 Add(Key,Value);
end;

function TBRREHashableObject.HashCode:longword;
begin
 result:=1;
end;

function TBRREHashableObject.EqualsTo(OtherObject:TBRREHashableObject):boolean;
begin
 result:=false;
end;

constructor TBRREPointerList.Create;
begin
 inherited Create;
 List:=nil;
 ListCount:=0;
 ListCapacity:=0;
 Clear;
end;

destructor TBRREPointerList.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TBRREPointerList.Clear;
begin
 ListCount:=0;
 ListCapacity:=0;
 if assigned(List) then begin
  FreeMem(List);
  List:=nil;
 end;
end;

procedure TBRREPointerList.SetCapacity(NewCapacity:longint);
begin
 if (NewCapacity>=0) and (NewCapacity<MaxListSize) then begin
  ReallocMem(List,NewCapacity*sizeof(pointer));
  ListCapacity:=NewCapacity;
  if ListCapacity<ListCount then begin
   ListCount:=ListCapacity;
  end;
  if ListCapacity<NewCapacity then begin
   FillChar(List^[ListCapacity],(NewCapacity-ListCapacity)*sizeof(pointer),#0);
  end;
 end;
end;

procedure TBRREPointerList.SetCount(NewCount:longint);
var NewCapacity:longint;
begin
 if NewCount<ListCount then begin
  if NewCount<1024 then begin
   NewCapacity:=1024;
  end else begin
   NewCapacity:=BRRERoundUpToPowerOfTwo(NewCount);
  end;
  if NewCapacity<>ListCapacity then begin
   ReallocMem(List,NewCapacity*sizeof(pointer));
   ListCapacity:=NewCapacity;
  end;
 end else if NewCount>ListCount then begin
  if NewCount>ListCapacity then begin
   if NewCount<1024 then begin
    NewCapacity:=1024;
   end else begin
    NewCapacity:=BRRERoundUpToPowerOfTwo(NewCount);
   end;
   ReallocMem(List,NewCapacity*sizeof(pointer));
   if ListCount<NewCapacity then begin
    FillChar(List^[ListCount],(NewCapacity-ListCount)*sizeof(pointer),#0);
   end else if ListCapacity<NewCapacity then begin
    FillChar(List^[ListCapacity],(NewCapacity-ListCapacity)*sizeof(pointer),#0);
   end;
   ListCapacity:=NewCapacity;
  end;
 end;
 ListCount:=NewCount;
end;

function TBRREPointerList.Add(Item:pointer):longint;
begin
 result:=ListCount;
 SetCount(result+1);
 List^[result]:=Item;
end;

function TBRREPointerList.Push(Item:pointer):longint;
begin
 result:=ListCount;
 SetCount(result+1);
 List^[result]:=Item;
end;

function TBRREPointerList.Pop:pointer;
begin
 if ListCount>0 then begin
  result:=List^[ListCount-1];
  List^[ListCount-1]:=nil;
  SetCount(ListCount-1);
 end else begin
  result:=nil;
 end;
end;

procedure TBRREPointerList.Insert(Index:longint;Item:pointer);
var i:longint;
begin
 if (Index>=0) and (Index<ListCount) then begin
  SetCount(ListCount+1);
  for i:=ListCount-1 downto Index do begin
   List^[i+1]:=List^[i];
  end;
  List^[Index]:=Item;
 end else if Index=ListCount then begin
  Add(Item);
 end else if Index>ListCount then begin
  SetCount(Index);
  Add(Item);
 end;
end;

function TBRREPointerList.Find(Item:pointer):longint;
var i:longint;
begin
 result:=-1;
 for i:=0 to ListCount-1 do begin
  if List^[i]=Item then begin
   result:=i;
   exit;
  end;
 end;
end;

function TBRREPointerList.IndexOf(Item:pointer):longint;
begin
 result:=Find(Item);
end;

function TBRREPointerList.Contains(Item:pointer):boolean;
var i:longint;
begin
 result:=false;
 for i:=0 to ListCount-1 do begin
  if List^[i]=Item then begin
   result:=true;
   exit;
  end;
 end;
end;

procedure TBRREPointerList.Delete(Index:longint);
var i,j,k:longint;
begin
 if (Index>=0) and (Index<ListCount) then begin
  k:=ListCount-1;
  j:=Index;
  List^[Index]:=nil;
  for i:=j to k-1 do begin
   List^[i]:=List^[i+1];
  end;
  SetCount(k);
 end;
end;

function TBRREPointerList.Remove(Item:pointer):longint;
begin
 result:=Find(Item);
 if result>=0 then begin
  Delete(result);
 end;
end;

procedure TBRREPointerList.Exchange(Index1,Index2:longint);
var TempPointer:pointer;
begin
 if (Index1>=0) and (Index1<ListCount) and (Index2>=0) and (Index2<ListCount) then begin
  TempPointer:=List^[Index1];
  List^[Index1]:=List^[Index2];
  List^[Index2]:=TempPointer;
 end;
end;

function TBRREPointerList.GetItem(Index:longint):pointer;
begin
 if (Index>=0) and (Index<ListCount) then begin
  result:=List^[Index];
 end else begin
  result:=nil;
 end;
end;

procedure TBRREPointerList.SetItem(Index:longint;Value:pointer);
begin
 if (Index>=0) and (Index<ListCount) then begin
  List^[Index]:=Value;
 end;
end;

constructor TBRREPtrValueSelfBalancedTree.Create;
begin
 inherited Create;
 new(RootNode);
 fillchar(RootNode^,sizeof(TBRREPtrValueSelfBalancedTreeNode),#0);
 RootNode^.Level:=$7fffffffffffffff;
 FirstKey:=nil;
 LastKey:=nil;
end;

destructor TBRREPtrValueSelfBalancedTree.Destroy;
begin
 ClearNode(RootNode^.Left);
 dispose(RootNode);
 inherited Destroy;
end;

function TBRREPtrValueSelfBalancedTree.First(StartNode:PBRREPtrValueSelfBalancedTreeNode):PBRREPtrValueSelfBalancedTreeNode;
begin
 try
  if not assigned(StartNode^.Left) then begin
   result:=nil;
   exit;
  end;
  result:=StartNode;
  while assigned(result^.Left) do begin
   result:=result^.Left;
  end;
 except
  result:=nil;
 end;
end;

function TBRREPtrValueSelfBalancedTree.Next(n:PBRREPtrValueSelfBalancedTreeNode):PBRREPtrValueSelfBalancedTreeNode;
begin
 try
  if assigned(n^.Right) then begin
   result:=n^.Right;
   while assigned(result^.Left) do begin
    result:=result^.Left;
   end;
  end else begin
   while assigned(n^.Parent) and (n^.Parent^.Right=n) do begin
    n:=n^.Parent;
   end;
   n:=n^.Parent;
   if not assigned(n) then begin
    result:=nil;
    exit;
   end;
   result:=n;
  end;
 except
  result:=nil;
 end;
end;

procedure TBRREPtrValueSelfBalancedTree.Skew(OldParent:PBRREPtrValueSelfBalancedTreeNode);
var NewParent:PBRREPtrValueSelfBalancedTreeNode;
begin
{$IFDEF UseAssert}
 Assert(assigned(OldParent));
{$ENDIF}
 NewParent:=OldParent^.Left;
{$IFDEF UseAssert}
 Assert(assigned(NewParent));
{$ENDIF}
 if OldParent^.Parent^.Left=OldParent then begin
  OldParent^.Parent^.Left:=NewParent;
 end else begin
  OldParent^.Parent^.Right:=NewParent;
 end;
 NewParent^.Parent:=OldParent^.Parent;
 OldParent^.Parent:=NewParent;

 OldParent^.Left:=NewParent^.Right;
 if assigned(OldParent^.Left) then begin
  OldParent^.Left^.Parent:=OldParent;
 end;
 NewParent^.Right:=OldParent;

 if assigned(OldParent^.Left) then begin
  OldParent^.level:=OldParent^.Left^.level+1;
 end else begin
  OldParent^.level:=1;
 end;
end;

function TBRREPtrValueSelfBalancedTree.Split(OldParent:PBRREPtrValueSelfBalancedTreeNode):boolean;
var NewParent:PBRREPtrValueSelfBalancedTreeNode;
begin
{$IFDEF UseAssert}
 Assert(assigned(OldParent));
{$ENDIF}
 NewParent:=OldParent^.Right;
 if assigned(NewParent) and assigned(NewParent^.Right) and (NewParent^.Right^.level=OldParent^.Level) then begin
  if OldParent^.Parent^.Left=OldParent then begin
   OldParent^.Parent^.Left:=NewParent;
  end else begin
   OldParent^.Parent^.Right:=NewParent;
  end;
  NewParent^.Parent:=OldParent^.Parent;
  OldParent^.Parent:=NewParent;

  OldParent^.Right:=NewParent^.Left;
  if assigned(OldParent^.Right) then begin
   OldParent^.Right^.Parent:=OldParent;
  end;
  NewParent^.Left:=OldParent;

  NewParent^.level:=OldParent^.level+1;

  result:=true;
 end else begin
  result:=false;
 end;
end;

procedure TBRREPtrValueSelfBalancedTree.RebalanceAfterLeafAdd(n:PBRREPtrValueSelfBalancedTreeNode);
begin
 // n is a node that has just been inserted and is now a Leaf node.
 n^.Level:=1;
 n^.Left:=nil;
 n^.Right:=nil;
 n:=n^.Parent;
 while n<>RootNode do begin
  if (assigned(n^.Left) and (n^.Level<>(n^.Left^.Level+1))) or ((not assigned(n^.Left)) and (n^.Level<>1)) then begin
   // this point the tree is correct, except (AA2) for n->Parent
   Skew(n);
   // We handle it (a Left add) by changing it into a Right add using Skew
   // If the original add was to the Left side of a node that is on the
   // Right side of a horizontal link, n now points to the rights side
   // of the second horizontal link, which is correct.
   // However if the original add was to the Left of node with a horizontal
   // link, we must get to the Right side of the second link.
   if (not assigned(n^.Right)) or (n^.Level<>n^.Right^.Level) then begin
    n:=n^.Parent;
   end;
  end;
  if not Split(n^.Parent) then begin
   break;
  end;
  n:=n^.Parent;
 end;
end;

function TBRREPtrValueSelfBalancedTree.FindNode(const Key:ptruint):PBRREPtrValueSelfBalancedTreeNode;
var n:PBRREPtrValueSelfBalancedTreeNode;
begin
 try
  result:=nil;
  n:=RootNode^.Left;
  while assigned(n) do begin
   if Key=n^.Key then begin
    result:=n;
    break;
   end else if ptruint(Key)<ptruint(n^.Key) then begin
    n:=n^.Left;
   end else begin
    n:=n^.Right;
   end;
  end;
 except
  result:=nil;
 end;
end;

function TBRREPtrValueSelfBalancedTree.Insert(const Key:ptruint;Value:TBRREPtrValueSelfBalancedTreeValue):PBRREPtrValueSelfBalancedTreeNode;
var n,s:PBRREPtrValueSelfBalancedTreeNode;
    LessThan:boolean;
begin
 result:=nil;
 try
  n:=nil;
  s:=RootNode^.Left;
  while assigned(s) do begin
   if Key=s^.Key then begin
    n:=s;
    break;
   end else if ptruint(Key)<ptruint(s^.Key) then begin
    s:=s^.Left;
   end else begin
    s:=s^.Right;
   end;
  end;
  if assigned(s) then begin
   n^.Value:=Value;
  end else begin
   new(n);
   fillchar(n^,sizeof(TBRREPtrValueSelfBalancedTreeNode),#0);
   n^.Key:=Key;
   n^.Value:=Value;
   if assigned(LastKey) then begin
    n^.PreviousKey:=LastKey;
    LastKey^.NextKey:=n;
    LastKey:=n;
   end else begin
    FirstKey:=n;
    LastKey:=n;
   end;
   s:=RootNode;
   LessThan:=true;
   while (LessThan and assigned(s^.Left)) or ((not LessThan) and assigned(s^.Right)) do begin
    if LessThan then begin
     s:=s^.Left;
    end else begin
     s:=s^.Right;
    end;
    LessThan:=ptruint(Key)<ptruint(s^.Key);
   end;
   if LessThan then begin
    s^.Left:=n;
   end else begin
    s^.Right:=n;
   end;
   n^.Parent:=s;
   RebalanceAfterLeafAdd(n);
   result:=n;
  end;
 except
  result:=nil;
 end;
end;

procedure TBRREPtrValueSelfBalancedTree.DeleteNode(n:PBRREPtrValueSelfBalancedTreeNode);
var Leaf,Temp:PBRREPtrValueSelfBalancedTreeNode;
begin
 try
  // If n is not a Leaf, we first swap it out with the Leaf node that just
  // precedes it.
  Leaf:=n;
  if assigned(n^.Left) then begin
   Leaf:=n^.Left;
   while assigned(Leaf^.Right) do begin
    Leaf:=Leaf^.Right;
   end;
  end else if assigned(n^.Right) then begin
   Leaf:=n^.Right;
  end;

  if Leaf^.Parent=n then begin
   Temp:=Leaf;
  end else begin
   Temp:=Leaf^.Parent;
  end;
  if Leaf^.Parent^.Left=Leaf then begin
   Leaf^.Parent^.Left:=nil;
  end else begin
   Leaf^.Parent^.Right:=nil;
  end;

  if n<>Leaf then begin
   if n^.Parent^.Left=n then begin
    n^.Parent^.Left:=Leaf;
   end else begin
    n^.Parent^.Right:=Leaf;
   end;
   Leaf^.Parent:=n^.Parent;
   if assigned(n^.Left) then begin
    n^.Left^.Parent:=Leaf;
   end;
   Leaf^.Left:=n^.Left;
   if assigned(n^.Right) then begin
    n^.Right^.Parent:=Leaf;
   end;
   Leaf^.Right:=n^.Right;
   Leaf^.level:=n^.level;
  end;
  if n<>RootNode then begin
   n^.Key:=0;
   if assigned(n^.PreviousKey) then begin
    n^.PreviousKey^.NextKey:=n^.NextKey;
   end else if FirstKey=n then begin
    FirstKey:=n^.NextKey;
   end;
   if assigned(n^.NextKey) then begin
    n^.NextKey^.PreviousKey:=n^.PreviousKey;
   end else if LastKey=n then begin
    LastKey:=n^.PreviousKey;
   end;
   dispose(n);
  end;

  while Temp<>RootNode do begin
   if (assigned(Temp^.Left) and (Temp^.level>(Temp^.Left^.level+1))) or ((not assigned(Temp^.Left)) and (Temp^.level>1)) then begin
    dec(Temp^.level);
    if Split(Temp) then begin
     if Split(Temp) then begin
      Skew(Temp^.Parent^.Parent);
     end;
     break;
    end;
    Temp:=Temp^.Parent;
   end else if (assigned(Temp^.Right) and (Temp^.level<=(Temp^.Right^.level+1))) or ((not assigned(Temp^.Right)) and (Temp^.level<=1)) then begin
    break;
   end else begin
    Skew(Temp);
 {  if assigned(Temp^.Right) then begin
     if assigned(Temp^.Right^.Left) then begin
      Temp^.Right^.level:=Temp^.Right^.level+1;
     end else begin
      Temp^.Right^.level:=1;
     end;
    end;}
    if Temp^.level>Temp^.Parent^.level then begin
     Skew(Temp);
     Split(Temp^.Parent^.Parent);
     break;
    end;
    Temp:=Temp^.Parent^.Parent;
   end;
  end;
 except
 end;
end;

procedure TBRREPtrValueSelfBalancedTree.Remove(const Key:ptruint);
var n:PBRREPtrValueSelfBalancedTreeNode;
begin
 try
  n:=RootNode^.Left;
  while assigned(n) do begin
   if Key=n^.Key then begin
    DeleteNode(n);
    break;
   end else if ptruint(Key)<ptruint(n^.Key) then begin
    n:=n^.Left;
   end else begin
    n:=n^.Right;
   end;
  end;
 except
 end;
end;

procedure TBRREPtrValueSelfBalancedTree.ClearNode(var Node:PBRREPtrValueSelfBalancedTreeNode);
begin
 if not assigned(Node) then begin
  exit;
 end;
 Node^.Key:=0;
 if assigned(Node^.PreviousKey) then begin
  Node^.PreviousKey^.NextKey:=Node^.NextKey;
 end else if FirstKey=Node then begin
  FirstKey:=Node^.NextKey;
 end;
 if assigned(Node^.NextKey) then begin
  Node^.NextKey^.PreviousKey:=Node^.PreviousKey;
 end else if LastKey=Node then begin
  LastKey:=Node^.PreviousKey;
 end;
 ClearNode(Node^.Left);
 ClearNode(Node^.Right);
 dispose(Node);
 Node:=nil;
end;

procedure TBRREPtrValueSelfBalancedTree.OptimizeNode(var Node:PBRREPtrValueSelfBalancedTreeNode;MoreOptimize:boolean);
var Nodes:array of TBRREPtrValueSelfBalancedTreeNode;
    NodeCount,NodeIndex:integer;
 procedure CountNodes(Node:PBRREPtrValueSelfBalancedTreeNode);
 begin
  if not assigned(Node) then begin
   exit;
  end;
  CountNodes(Node^.Left);
  CountNodes(Node^.Right);
  inc(NodeCount);
 end;
 procedure CollectNodes(Node:PBRREPtrValueSelfBalancedTreeNode);
 begin
  if not assigned(Node) then begin
   exit;
  end;
  CollectNodes(Node^.Left);
  if NodeIndex>=length(Nodes) then begin
   NodeCount:=NodeIndex+1;
   SetLength(Nodes,NodeCount);
  end;
  Nodes[NodeIndex]:=Node^;
  inc(NodeIndex);
  CollectNodes(Node^.Right);
 end;
 procedure FreeNodes(var Node:PBRREPtrValueSelfBalancedTreeNode);
 begin
  if not assigned(Node) then begin
   exit;
  end;
  Node^.Key:=0;
  if assigned(Node^.PreviousKey) then begin
   Node^.PreviousKey^.NextKey:=Node^.NextKey;
  end;
  if assigned(Node^.NextKey) then begin
   Node^.NextKey^.PreviousKey:=Node^.PreviousKey;
  end;
  if FirstKey=Node then begin
   FirstKey:=Node^.NextKey;
  end;
  if LastKey=Node then begin
   LastKey:=Node^.PreviousKey;
  end;
  CountNodes(Node^.Left);
  CountNodes(Node^.Right);
  dispose(Node);
  Node:=nil;
 end;
 procedure DoInsertNode(const Node:TBRREPtrValueSelfBalancedTreeNode);
 var n,s:PBRREPtrValueSelfBalancedTreeNode;
     LessThan:boolean;
 begin
  new(n);
  n^:=Node;
  n^.Parent:=nil;
  n^.Left:=nil;
  n^.Right:=nil;
  n^.Level:=0;
  s:=RootNode;
  LessThan:=true;
  while (LessThan and assigned(s^.Left)) or ((not LessThan) and assigned(s^.Right)) do begin
   if LessThan then begin
    s:=s^.Left;
   end else begin
    s:=s^.Right;
   end;
   LessThan:=ptruint(n^.Key)<ptruint(s^.Key);
  end;
  if LessThan then begin
   s^.Left:=n;
  end else begin
   s^.Right:=n;
  end;
  n^.Parent:=s;
  if assigned(LastKey) then begin
   n^.PreviousKey:=LastKey;
   LastKey^.NextKey:=n;
   LastKey:=n;
  end else begin
   FirstKey:=n;
   LastKey:=n;
  end;
  if not MoreOptimize then begin
   RebalanceAfterLeafAdd(n);
  end;
 end;
 procedure RepairNodes(var Node:PBRREPtrValueSelfBalancedTreeNode);
 begin
  if not assigned(Node) then begin
   exit;
  end;
  RepairNodes(Node^.Left);
  RepairNodes(Node^.Right);
  if assigned(Node^.Left) and assigned(Node^.Right) then begin
   Node^.Level:=Node^.Left^.Level+1;
  end else begin
   Node^.Level:=1;
  end;
 end;
 procedure ReinsertNodesForRepair(LowNode,HighNode:integer);
 var MiddleNode:integer;
 begin
  if HighNode<LowNode then begin
   exit;
  end;
  MiddleNode:=LowNode+((HighNode-LowNode) div 2);
  DoInsertNode(Nodes[MiddleNode]);
  ReinsertNodesForRepair(LowNode,MiddleNode-1);
  ReinsertNodesForRepair(MiddleNode+1,HighNode);
 end;
 procedure ReinsertNodes(LowNode,HighNode:integer);
 var i:integer;
 begin
  for i:=LowNode to HighNode do begin
   DoInsertNode(Nodes[i]);
  end;
 end;
begin
 if not assigned(Node) then begin
  exit;
 end;
 try
  Nodes:=nil;
  NodeCount:=0;
  CountNodes(Node);
  SetLength(Nodes,NodeCount);
  NodeIndex:=0;
  CollectNodes(Node);
  FreeNodes(Node);
  if MoreOptimize then begin
   ReinsertNodesForRepair(0,length(Nodes)-1);
   RepairNodes(RootNode^.Left);
  end else begin
   ReinsertNodes(0,length(Nodes)-1);
  end;
  SetLength(Nodes,0);
 except
 end;
end;

function TBRREPtrValueSelfBalancedTree.Find(const Key:ptruint;var Value:TBRREPtrValueSelfBalancedTreeValue):boolean;
var n:PBRREPtrValueSelfBalancedTreeNode;
begin
 n:=FindNode(Key);
 if assigned(n) then begin
  Value:=n^.Value;
  result:=true;
 end else begin
  fillchar(Value,sizeof(TBRREPtrValueSelfBalancedTreeValue),#0);
  result:=false;
 end;
end;

procedure TBRREPtrValueSelfBalancedTree.Optimize;
begin
 OptimizeNode(RootNode^.Left,true);
end;

function TBRREPtrValueSelfBalancedTree.Keys:TBRREPtrValueSelfBalancedTreeKeys;
var CurrentNode:PBRREPtrValueSelfBalancedTreeNode;
    Count:integer;
begin
 result:=nil;
 Count:=0;
 CurrentNode:=FirstKey;
 while assigned(CurrentNode) do begin
  inc(Count);
  CurrentNode:=CurrentNode^.NextKey;
 end;
 SetLength(result,Count);
 Count:=0;
 CurrentNode:=FirstKey;
 while assigned(CurrentNode) do begin
  result[Count]:=CurrentNode^.Key;
  inc(Count);
  CurrentNode:=CurrentNode^.NextKey;
 end;
end;

function TBRREPtrValueSelfBalancedTree.GetValue(Key:ptruint):TBRREPtrValueSelfBalancedTreeValue;
begin
 Find(Key,result);
end;

procedure TBRREPtrValueSelfBalancedTree.SetValue(Key:ptruint;Value:TBRREPtrValueSelfBalancedTreeValue);
begin
 Insert(Key,Value);
end;

function TBRRERegExpPrefilterNodeList.GetNodeItem(Index:longint):TBRRERegExpPrefilterNode;
begin
 result:=TBRRERegExpPrefilterNode(inherited GetItem(Index));
end;

procedure TBRRERegExpPrefilterNodeList.SetNodeItem(Index:longint;Value:TBRRERegExpPrefilterNode);
begin
 inherited SetItem(Index,Value);
end;

constructor TBRRERegExpPrefilterNode.Create;
begin
 inherited Create;
 Operation:=brrepfnoANY;
 Subs:=TBRRERegExpPrefilterNodeList.Create;
 Atom:='';
 Exact:=false;
end;

destructor TBRRERegExpPrefilterNode.Destroy;
var Counter:longint;
begin
 for Counter:=0 to Subs.Count-1 do begin
  Subs[Counter].Free;
 end;
 FreeAndNil(Subs);
 Atom:='';
 inherited Destroy;
end;

function TBRRERegExpPrefilterNode.Clone:TBRRERegExpPrefilterNode;
var Counter:longint;
begin
 result:=TBRRERegExpPrefilterNode.Create;
 result.Operation:=Operation;
 result.Atom:=Atom;
 result.Exact:=Exact;
 for Counter:=0 to Subs.Count-1 do begin
  result.Subs.Add(Subs[Counter].Clone);
 end;
end;

function TBRRERegExpPrefilterNode.Expression:ansistring;
var Counter:longint;
    s:ansistring;
begin
 result:='';
 case Operation of
  brrepfnoATOM:begin
   result:=Atom;
   Counter:=1;
   while Counter<=length(result) do begin
    case result[Counter] of
     '\','"':begin
      System.Insert('\',result,Counter);
      inc(Counter,2);
     end;
     #0:begin
      result[Counter]:='0';
      System.Insert('\',result,Counter);
      inc(Counter,2);
     end;
     else begin
      inc(Counter);
     end;
    end;
   end;
   result:='"'+result+'"';
  end;
  brrepfnoANY:begin
   result:='*';
  end;
  brrepfnoAND:begin
   case Subs.Count of
    0:begin
     result:='';
    end;
    1:begin
     result:=Subs[0].Expression;
    end;
    else begin
     for Counter:=0 to Subs.Count-1 do begin
      s:=Subs[Counter].Expression;
      if length(s)>0 then begin
       if length(result)>0 then begin
        result:=result+' AND ';
       end;
       result:=result+s;
      end;
     end;
     result:='('+result+')';
    end;
   end;
  end;
  brrepfnoOR:begin
   case Subs.Count of
    0:begin
     result:='';
    end;
    1:begin
     result:=Subs[0].Expression;
    end;
    else begin
     for Counter:=0 to Subs.Count-1 do begin
      s:=Subs[Counter].Expression;
      if length(s)>0 then begin
       if length(result)>0 then begin
        result:=result+' OR ';
       end;
       result:=result+s;
      end;
     end;
     result:='('+result+')';
    end;
   end;
  end;
 end;
end;

function TBRRERegExpPrefilterNode.ShortExpression:ansistring;
var Counter:longint;
    s:ansistring;
begin
 result:='';
 case Operation of
  brrepfnoATOM:begin
   result:=Atom;
   Counter:=1;
   while Counter<=length(result) do begin
    case result[Counter] of
     '\','(',')','|','*':begin
      System.Insert('\',result,Counter);
      inc(Counter,2);
     end;
     #0:begin
      result[Counter]:='0';
      System.Insert('\',result,Counter);
      inc(Counter,2);
     end;
     else begin
      inc(Counter);
     end;
    end;
   end;
  end;
  brrepfnoANY:begin
   result:='*';
  end;
  brrepfnoAND:begin
   case Subs.Count of
    0:begin
     result:='';
    end;
    1:begin
     result:=Subs[0].ShortExpression;
    end;
    else begin
     for Counter:=0 to Subs.Count-1 do begin
      result:=result+Subs[Counter].ShortExpression;
     end;
     result:='('+result+')';
    end;
   end;
  end;
  brrepfnoOR:begin
   case Subs.Count of
    0:begin
     result:='';
    end;
    1:begin
     result:=Subs[0].ShortExpression;
    end;
    else begin
     for Counter:=0 to Subs.Count-1 do begin
      s:=Subs[Counter].ShortExpression;
      if length(s)>0 then begin
       if length(result)>0 then begin
        result:=result+'|';
       end;
       result:=result+s;
      end;
     end;
     result:='('+result+')';
    end;
   end;
  end;
 end;
end;

function TBRRERegExpPrefilterNode.SQLBooleanFullTextExpression:ansistring;
var Counter:longint;
    s:ansistring;
begin
 result:='';
 case Operation of
  brrepfnoATOM:begin
   result:=Atom;
   Counter:=1;
   while Counter<=length(result) do begin
    case result[Counter] of
     '\','"','''','%':begin
      System.Insert('\',result,Counter);
      inc(Counter,2);
     end;
     #$1a:begin
      result[Counter]:='Z';
      System.Insert('\',result,Counter);
      inc(Counter,2);
     end;
     #$13:begin
      result[Counter]:='r';
      System.Insert('\',result,Counter);
      inc(Counter,2);
     end;
     #$10:begin
      result[Counter]:='n';
      System.Insert('\',result,Counter);
      inc(Counter,2);
     end;
     #$00:begin
      result[Counter]:='0';
      System.Insert('\',result,Counter);
      inc(Counter,2);
     end;
     else begin
      inc(Counter);
     end;
    end;
   end;
   result:='"'+result+'"';
  end;
  brrepfnoANY:begin
   result:='';
  end;
  brrepfnoAND:begin
   case Subs.Count of
    0:begin
     result:='';
    end;
    1:begin
     result:=Subs[0].SQLBooleanFullTextExpression;
    end;
    else begin
     for Counter:=0 to Subs.Count-1 do begin
      s:=Subs[Counter].SQLBooleanFullTextExpression;
      if length(s)>0 then begin
       if length(result)>0 then begin
        result:=result+' ';
       end;
       result:=result+'+'+s;
      end;
     end;
     result:='('+result+')';
    end;
   end;
  end;
  brrepfnoOR:begin
   case Subs.Count of
    0:begin
     result:='';
    end;
    1:begin
     result:=Subs[0].SQLBooleanFullTextExpression;
    end;
    else begin
     for Counter:=0 to Subs.Count-1 do begin
      s:=Subs[Counter].SQLBooleanFullTextExpression;
      if length(s)>0 then begin
       if length(result)>0 then begin
        result:=result+' ';
       end;
       result:=result+s;
      end;
     end;
     result:='('+result+')';
    end;
   end;
  end;
 end;
end;

function TBRRERegExpPrefilterNode.SQLExpression(const Field:ansistring):ansistring;
var Counter:longint;
    s:ansistring;
begin
 result:='';
 case Operation of
  brrepfnoATOM:begin
   result:=Atom;
   Counter:=1;
   while Counter<=length(result) do begin
    case result[Counter] of
     '\','"','''','%':begin
      System.Insert('\',result,Counter);
      inc(Counter,2);
     end;
     #$1a:begin
      result[Counter]:='Z';
      System.Insert('\',result,Counter);
      inc(Counter,2);
     end;
     #$13:begin
      result[Counter]:='r';
      System.Insert('\',result,Counter);
      inc(Counter,2);
     end;
     #$10:begin
      result[Counter]:='n';
      System.Insert('\',result,Counter);
      inc(Counter,2);
     end;
     #$00:begin
      result[Counter]:='0';
      System.Insert('\',result,Counter);
      inc(Counter,2);
     end;
     else begin
      inc(Counter);
     end;
    end;
   end;
   if length(result)>0 then begin
    result:='('+Field+' LIKE ''%'+result+'%'')';
   end;
  end;
  brrepfnoAND:begin
   case Subs.Count of
    0:begin
     result:='';
    end;
    1:begin
     result:=Subs[0].SQLExpression(Field);
    end;
    else begin
     for Counter:=0 to Subs.Count-1 do begin
      s:=Subs[Counter].SQLExpression(Field);
      if length(s)>0 then begin
       if length(result)>0 then begin
        result:=result+' AND ';
       end;
       result:=result+s;
      end;
     end;
     result:='('+result+')';
    end;
   end;
  end;
  brrepfnoOR:begin
   case Subs.Count of
    0:begin
     result:='';
    end;
    1:begin
     result:=Subs[0].SQLExpression(Field);
    end;
    else begin
     for Counter:=0 to Subs.Count-1 do begin
      s:=Subs[Counter].SQLExpression(Field);
      if length(s)>0 then begin
       if length(result)>0 then begin
        result:=result+' OR ';
       end;
       result:=result+s;
      end;
     end;
     result:='('+result+')';
    end;
   end;
  end;
 end;
end;

constructor TBRRERegExpCharClassRange.Create(ACharClass:TBRRERegExpCharClass;ALo,AHi:longword);
begin
 inherited Create;
 CharClass:=ACharClass;
 Lo:=ALo;
 Hi:=AHi;
 if assigned(CharClass.Last) then begin
  Previous:=CharClass.Last;
  CharClass.Last:=self;
  Previous.Next:=self;
  Next:=nil;
 end else begin
  CharClass.First:=self;
  CharClass.Last:=self;
  Previous:=nil;
  Next:=nil;
 end;
 Left:=nil;
 Right:=nil;
end;

constructor TBRRERegExpCharClassRange.CreateBefore(ACharClass:TBRRERegExpCharClass;ABefore:TBRRERegExpCharClassRange;ALo,AHi:longword);
begin
 inherited Create;
 CharClass:=ACharClass;
 Lo:=ALo;
 Hi:=AHi;
 Previous:=ABefore.Previous;
 Next:=ABefore;
 ABefore.Previous:=self;
 if assigned(Previous) then begin
  Previous.Next:=self;
 end else begin
  CharClass.First:=self;
 end;
end;

constructor TBRRERegExpCharClassRange.CreateAfter(ACharClass:TBRRERegExpCharClass;AAfter:TBRRERegExpCharClassRange;ALo,AHi:longword);
begin
 inherited Create;
 CharClass:=ACharClass;
 Lo:=ALo;
 Hi:=AHi;
 Previous:=AAfter;
 Next:=AAfter.Next;
 AAfter.Next:=self;
 if assigned(Next) then begin
  Next.Previous:=self;
 end else begin
  CharClass.Last:=self;
 end;
end;

destructor TBRRERegExpCharClassRange.Destroy;
begin
 if assigned(Previous) then begin
  Previous.Next:=Next;
 end else if CharClass.First=self then begin
  CharClass.First:=Next;
 end;
 if assigned(Next) then begin
  Next.Previous:=Previous;
 end else if CharClass.Last=self then begin
  CharClass.Last:=Previous;
 end;
 Previous:=nil;
 Next:=nil;
 inherited Destroy;
end;

constructor TBRRERegExpCharClass.Create(ARegExp:TBRRERegExp);
begin
 inherited Create;
 RegExp:=ARegExp;
 if assigned(RegExp.LastCharClass) then begin
  Previous:=RegExp.LastCharClass;
  RegExp.LastCharClass:=self;
  Previous.Next:=self;
  Next:=nil;
 end else begin
  RegExp.FirstCharClass:=self;
  RegExp.LastCharClass:=self;
  Previous:=nil;
  Next:=nil;
 end;
 First:=nil;
 Last:=nil;
 Root:=nil;
 Inverted:=false;
 Canonicalized:=false;
 CharSet:=[];
end;

destructor TBRRERegExpCharClass.Destroy;
begin
 while assigned(First) do begin
  First.Destroy;
 end;
 if assigned(Previous) then begin
  Previous.Next:=Next;
 end else if RegExp.FirstCharClass=self then begin
  RegExp.FirstCharClass:=Next;
 end;
 if assigned(Next) then begin
  Next.Previous:=Previous;
 end else if RegExp.LastCharClass=self then begin
  RegExp.LastCharClass:=Previous;
 end;
 Previous:=nil;
 Next:=nil;
 inherited Destroy;
end;

procedure TBRRERegExpCharClass.Clear;
begin
 while assigned(First) do begin
  First.Destroy;
 end;
 Inverted:=false;
 Canonicalized:=false;
 Root:=nil;
end;

procedure TBRRERegExpCharClass.Dump;
var Range:TBRRERegExpCharClassRange;
begin
 Range:=First;
 while assigned(Range) do begin
  writeln(Range.Lo:8,' ',Range.Hi:8);
  Range:=Range.Next;
 end;
end;

procedure TBRRERegExpCharClass.DebugDump;
var Range:TBRRERegExpCharClassRange;
begin
 Range:=First;
 while assigned(Range) do begin
  Range:=Range.Next;
 end;
end;

procedure TBRRERegExpCharClass.Optimize;
var Range:TBRRERegExpCharClassRange;
begin
 Range:=First;
 while assigned(Range) do begin
  if assigned(Range.Previous) and (((Range.Previous.Hi>=Range.Lo) or ((Range.Previous.Hi+1)=Range.Lo))) then begin
   if Range.Lo>Range.Previous.Lo then begin
    Range.Lo:=Range.Previous.Lo;
   end;
   if Range.Hi<Range.Previous.Hi then begin
    Range.Hi:=Range.Previous.Hi;
   end;
   Range.Previous.Destroy;
   if assigned(Range.Previous) then begin
    Range:=Range.Previous;
   end;
  end else if assigned(Range.Next) and (((Range.Hi>=Range.Next.Lo) or ((Range.Hi+1)=Range.Next.Lo))) then begin
   if Range.Lo>Range.Next.Lo then begin
    Range.Lo:=Range.Next.Lo;
   end;
   if Range.Hi<Range.Next.Hi then begin
    Range.Hi:=Range.Next.Hi;
   end;
   Range.Next.Destroy;
   if assigned(Range.Previous) then begin
    Range:=Range.Previous;
   end;
  end else begin
   Range:=Range.Next;
  end;
 end;
end;

procedure TBRRERegExpCharClass.AddRange(Lo,Hi:longword;IgnoreCase:boolean=false);
var Range:TBRRERegExpCharClassRange;
    c,cl,cu:longword;
    NeedToCanonicalize:boolean;
begin
 if IgnoreCase then begin
  NeedToCanonicalize:=false;
  for c:=Lo to Hi do begin
   cl:=BRREUnicodeToLower(c);
   cu:=BRREUnicodeToUpper(c);
   if (cl<>cu) or (cl<>c) or (cu<>c) then begin
    NeedToCanonicalize:=true;
    break;
   end;
  end;
  if NeedToCanonicalize then begin
   for c:=Lo to Hi do begin
    cl:=BRREUnicodeToLower(c);
    cu:=BRREUnicodeToUpper(c);
    if (cl=cu) and (cl=c) then begin
     AddRange(c,c,false);
    end else begin
     AddRange(cl,cl,false);
     AddRange(cu,cu,false);
     if (cl<>c) and (cu<>c) then begin
      AddRange(c,c,false);
     end;
    end;
   end;
  end else begin
   AddRange(Lo,Hi,false);
  end;
 end else begin
  Range:=First;
  while assigned(Range) do begin
   if (Lo>=Range.Lo) and (Hi<=Range.Hi) then begin
    exit;
   end else if (Lo<=Range.Lo) or ((Lo=Range.Lo) and (Hi<=Range.Hi)) then begin
    break;
   end;
   Range:=Range.Next;
  end;
  if assigned(Range) then begin
   TBRRERegExpCharClassRange.CreateBefore(self,Range,Lo,Hi);
  end else begin
   TBRRERegExpCharClassRange.Create(self,Lo,Hi);
  end;
  Optimize;
 end;
end;

procedure TBRRERegExpCharClass.AddChar(c:longword;IgnoreCase:boolean=false);
begin
 AddRange(c,c,IgnoreCase);
end;

procedure TBRRERegExpCharClass.AddUnicodeCategory(CategoryFlags:longword;IgnoreCase:boolean=false);
var Value,LowValue,HighValue,Index:longword;
begin
 LowValue:=$ffffffff;
 HighValue:=0;
 for Value:=0 to $10ffff do begin
  Index:=Value shr BRREUnicodeCategoryArrayBlockBits;
  if (CategoryFlags and (1 shl BRREUnicodeCategoryArrayBlockData[BRREUnicodeCategoryArrayIndexBlockData[BRREUnicodeCategoryArrayIndexIndexData[Index shr BRREUnicodeCategoryArrayIndexBlockBits],Index and BRREUnicodeCategoryArrayIndexBlockMask],Value and BRREUnicodeCategoryArrayBlockMask]))<>0 then begin
   if LowValue<=HighValue then begin
    if (HighValue+1)=Value then begin
     HighValue:=Value;
    end else begin
     AddRange(LowValue,HighValue,IgnoreCase);
     LowValue:=Value;
     HighValue:=Value;
    end;
   end else begin
    LowValue:=Value;
    HighValue:=Value;
   end;
  end;
 end;
 if LowValue<=HighValue then begin
  AddRange(LowValue,HighValue,IgnoreCase);
 end;
end;

procedure TBRRERegExpCharClass.AddUnicodeScript(Script:longword;IgnoreCase:boolean=false);
var Value,LowValue,HighValue,Index:longword;
begin
 LowValue:=$ffffffff;
 HighValue:=0;
 for Value:=0 to $10ffff do begin
  Index:=Value shr BRREUnicodeScriptArrayBlockBits;
  if Script=BRREUnicodeScriptArrayBlockData[BRREUnicodeScriptArrayIndexBlockData[BRREUnicodeScriptArrayIndexIndexData[Index shr BRREUnicodeScriptArrayIndexBlockBits],Index and BRREUnicodeScriptArrayIndexBlockMask],Value and BRREUnicodeScriptArrayBlockMask] then begin
   if LowValue<=HighValue then begin
    if (HighValue+1)=Value then begin
     HighValue:=Value;
    end else begin
     AddRange(LowValue,HighValue,IgnoreCase);
     LowValue:=Value;
     HighValue:=Value;
    end;
   end else begin
    LowValue:=Value;
    HighValue:=Value;
   end;
  end;
 end;
 if LowValue<=HighValue then begin
  AddRange(LowValue,HighValue,IgnoreCase);
 end;
end;

procedure TBRRERegExpCharClass.AddUnicodeBlock(Block:longword;IgnoreCase:boolean=false);
begin
 AddRange(ts.Core.BRREunicode.BRREUnicodeBlocks[Block].FromChar,ts.Core.BRREunicode.BRREUnicodeBlocks[Block].ToChar,IgnoreCase);
end;

procedure TBRRERegExpCharClass.Combine(From:TBRRERegExpCharClass);
var Range:TBRRERegExpCharClassRange;
begin
 if assigned(From) then begin
  if assigned(First) then begin
   Canonicalized:=Canonicalized and From.Canonicalized;
  end else begin
   Canonicalized:=From.Canonicalized;
  end;
  Range:=From.First;
  while assigned(Range) do begin
   AddRange(Range.Lo,Range.Hi,false);
   Range:=Range.Next;
  end;
 end;
end;
                                 
function TBRRERegExpCharClass.Subtraction(From,SubtractWith:TBRRERegExpCharClass):boolean;
var Range1,Range2:TBRRERegExpCharClassRange;
    First,Min,Max:longword;
begin
 result:=false;
 if ((assigned(From) and assigned(SubtractWith)) and (assigned(From.First) and assigned(SubtractWith.First))) and ((From.First.Lo<=SubtractWith.Last.Hi) and (SubtractWith.First.Lo<=From.Last.Hi)) then begin
  Canonicalized:=From.Canonicalized;
  Range1:=From.First;
  while assigned(Range1) do begin
   First:=Range1.Lo;
   Range2:=SubtractWith.First;
   while assigned(Range2) do begin
    if (First<=Range2.Hi) and (Range2.Lo<=Range1.Hi) then begin
     result:=true;
     if First>Range2.Lo then begin
      Min:=First;
     end else begin
      Min:=Range2.Lo;
     end;
     if Range1.Hi<Range2.Hi then begin
      Max:=Range1.Hi;
     end else begin
      Max:=Range2.Hi;
     end;
     if First<Min then begin
      AddRange(First,Min-1,false);
     end;
     First:=Max+1;
    end;
    Range2:=Range2.Next;
   end;
   if First<=Range1.Hi then begin
    AddRange(First,Range1.Hi,false);
   end;
   Range1:=Range1.Next;
  end;
 end;
end;

function TBRRERegExpCharClass.Intersection(From,SubtractWith:TBRRERegExpCharClass):boolean;
var Range1,Range2:TBRRERegExpCharClassRange;
    Min,Max:longword;
begin
 result:=false;
 if ((assigned(From) and assigned(SubtractWith)) and (assigned(From.First) and assigned(SubtractWith.First))) and ((From.First.Lo<=SubtractWith.Last.Hi) and (SubtractWith.First.Lo<=From.Last.Hi)) then begin
  Canonicalized:=From.Canonicalized;
  Range1:=From.First;
  while assigned(Range1) do begin
   Range2:=SubtractWith.First;
   while assigned(Range2) do begin
    if (Range1.Lo<=Range2.Hi) and (Range2.Lo<=Range1.Hi) then begin
     result:=true;
     if Range1.Lo>Range2.Lo then begin
      Min:=Range1.Lo;
     end else begin
      Min:=Range2.Lo;
     end;
     if Range1.Hi<Range2.Hi then begin
      Max:=Range1.Hi;
     end else begin
      Max:=Range2.Hi;
     end;
     if Min<=Max then begin
      AddRange(Min,Max,false);
     end;
    end;
    Range2:=Range2.Next;
   end;
   Range1:=Range1.Next;
  end;
 end;
end;

procedure TBRRERegExpCharClass.TakeoverCombine(From:TBRRERegExpCharClass);
var SrcRange,NextSrcRange,Range:TBRRERegExpCharClassRange;
begin
 if assigned(From) then begin
  if assigned(First) then begin
   Canonicalized:=Canonicalized and From.Canonicalized;
  end else begin
   Canonicalized:=From.Canonicalized;
  end;
  SrcRange:=From.First;
  while assigned(SrcRange) do begin
   Range:=First;
   while assigned(Range) do begin
    if (SrcRange.Lo>=Range.Lo) and (SrcRange.Hi<=Range.Hi) then begin
     exit;
    end else if (SrcRange.Lo<=Range.Lo) or ((SrcRange.Lo=Range.Lo) and (SrcRange.Hi<=Range.Hi)) then begin
     break;
    end;
    Range:=Range.Next;
   end;
   SrcRange.CharClass:=self;
   NextSrcRange:=SrcRange.Next;
   if assigned(SrcRange.Previous) then begin
    SrcRange.Previous.Next:=SrcRange.Next;
   end else if From.First=SrcRange then begin
    From.First:=SrcRange.Next;
   end;
   if assigned(SrcRange.Next) then begin
    SrcRange.Next.Previous:=SrcRange.Previous;
   end else if From.Last=SrcRange then begin
    From.Last:=SrcRange.Previous;
   end;
   SrcRange.Previous:=nil;
   SrcRange.Next:=nil;
   if assigned(Range) then begin
    SrcRange.Previous:=Range.Previous;
    SrcRange.Next:=Range;
    Range.Previous:=SrcRange;
    if assigned(SrcRange.Previous) then begin
     SrcRange.Previous.Next:=SrcRange;
    end else begin
     SrcRange.CharClass.First:=SrcRange;
    end;
   end else begin
    if assigned(SrcRange.CharClass.Last) then begin
     SrcRange.Previous:=SrcRange.CharClass.Last;
     SrcRange.CharClass.Last:=SrcRange;
     SrcRange.Previous.Next:=SrcRange;
     SrcRange.Next:=nil;
    end else begin
     SrcRange.CharClass.First:=SrcRange;
     SrcRange.CharClass.Last:=SrcRange;
     SrcRange.Previous:=nil;
     SrcRange.Next:=nil;
    end;
   end;
   Optimize;
   SrcRange:=NextSrcRange;
  end;
 end;
end;

procedure TBRRERegExpCharClass.TakeoverDirtyCombine(From:TBRRERegExpCharClass);
var SrcRange:TBRRERegExpCharClassRange;
begin
 if assigned(From) then begin
  if assigned(First) then begin
   Canonicalized:=Canonicalized and From.Canonicalized;
  end else begin
   Canonicalized:=From.Canonicalized;
  end;
  SrcRange:=From.First;
  while assigned(SrcRange) do begin
   SrcRange.CharClass:=self;
   SrcRange:=SrcRange.Next;
  end;
  if assigned(Last) then begin
   Last.Next:=From.First;
   From.First.Previous:=Last;
   Last:=From.Last;
  end else begin
   First:=From.First;
   Last:=From.Last;
  end;
  From.First:=nil;
  From.Last:=nil;
 end;
end;

procedure TBRRERegExpCharClass.Assign(From:TBRRERegExpCharClass);
var Range:TBRRERegExpCharClassRange;
begin
 if assigned(From) then begin
  while assigned(First) do begin
   First.Destroy;
  end;
  Inverted:=From.Inverted;
  Canonicalized:=From.Canonicalized;
  Range:=From.First;
  while assigned(Range) do begin
   Range.CharClass:=self;
   Range:=Range.Next;
  end;
  First:=From.First;
  Last:=From.Last;
  From.First:=nil;
  From.Last:=nil;
 end;
end;

procedure TBRRERegExpCharClass.Append(From:TBRRERegExpCharClass);
var Range:TBRRERegExpCharClassRange;
begin
 if assigned(From) then begin
  Range:=From.First;
  while assigned(Range) do begin
   TBRRERegExpCharClassRange.Create(self,Range.Lo,Range.Hi);
   Range:=Range.Next;
  end;
 end;
end;

procedure TBRRERegExpCharClass.Invert;
var NewList:TBRRERegExpCharClass;
    Range:TBRRERegExpCharClassRange;
    Lo,Hi:longword;
begin
 Optimize;
 Inverted:=not Inverted;
 if assigned(First) and (First=Last) and (First.Lo=0) and (First.Hi=$ffffffff) then begin
  First.Destroy;
 end else if not assigned(First) then begin
  TBRRERegExpCharClassRange.Create(self,0,$ffffffff);
 end else begin
  NewList:=TBRRERegExpCharClass.Create(RegExp);
  try
   Range:=First;
   if Range.Lo>0 then begin
    TBRRERegExpCharClassRange.Create(NewList,0,Range.Lo-1);
   end;
   Lo:=Range.Hi;
   Range:=Range.Next;
   while assigned(Range) do begin
    if (Lo+1)<Range.Lo then begin
     Hi:=Range.Lo;
     TBRRERegExpCharClassRange.Create(NewList,Lo+1,Hi-1);
    end;
    Lo:=Range.Hi;
    Range:=Range.Next;
   end;
   if Lo<>$ffffffff then begin
    TBRRERegExpCharClassRange.Create(NewList,Lo+1,$ffffffff);
   end;
   while assigned(First) do begin
    First.Destroy;
   end;
   Range:=NewList.First;
   while assigned(Range) do begin
    Range.CharClass:=self;
    Range:=Range.Next;
   end;
   First:=NewList.First;
   Last:=NewList.Last;
   NewList.First:=nil;
   NewList.Last:=nil;
   Range:=First;
   while assigned(Range) do begin
    Range.CharClass:=self;
    Range:=Range.Next;
   end;
  finally
   NewList.Destroy;
  end;
 end;
end;

procedure TBRRERegExpCharClass.Canonicalize;
var NewList:TBRRERegExpCharClass;
    Range:TBRRERegExpCharClassRange;
    OldInverted:boolean;
begin
 if not Canonicalized then begin
  NewList:=TBRRERegExpCharClass.Create(RegExp);
  try
   OldInverted:=Inverted;
   if Inverted then begin
    Invert;
   end;
   Range:=First;
   while assigned(Range) do begin
    NewList.AddRange(Range.Lo,Range.Hi,true);
    Range:=Range.Next;
   end;
   while assigned(First) do begin
    First.Destroy;
   end;
   First:=NewList.First;
   Last:=NewList.Last;
   NewList.First:=nil;
   NewList.Last:=nil;
   Range:=First;
   while assigned(Range) do begin
    Range.CharClass:=self;
    Range:=Range.Next;
   end;
   if OldInverted then begin
    Invert;
   end;
   Inverted:=OldInverted;
  finally
   NewList.Destroy;
  end;
  Canonicalized:=true;
 end;
end;

procedure TBRRERegExpCharClass.CompileBinarySearchTree;
type PTBRRERegExpCharClassRange=^TBRRERegExpCharClassRange;
var Ranges:TBRRERegExpCharClassRanges;
 procedure Process(Parent:PTBRRERegExpCharClassRange;LowIndex,HighIndex:longint);
 var Middle:longint;
 begin
  while LowIndex<=HighIndex do begin
   Middle:=(LowIndex+HighIndex) div 2;
   Parent^:=Ranges[Middle];
   case HighIndex-LowIndex of
    0:begin
     break;
    end;
    1:begin
     if Middle=LowIndex then begin
      Parent:=@Parent^.Right;
      LowIndex:=Middle+1;
     end else begin
      Parent:=@Parent^.Left;
      HighIndex:=Middle-1;
     end;
    end;
    else begin
     Process(@Parent^.Left,LowIndex,Middle-1);
     Parent:=@Parent^.Right;
     LowIndex:=Middle+1;
    end;
   end;
  end;
 end;
var Range:TBRRERegExpCharClassRange;
    Count:longint;
begin
 Root:=nil;
 Ranges:=nil;
 try
  Count:=0;
  Range:=First;
  while assigned(Range) do begin
   inc(Count);
   Range:=Range.Next;
  end;
  SetLength(Ranges,Count);
  Count:=0;
  Range:=First;
  while assigned(Range) do begin
   Ranges[Count]:=Range;
   inc(Count);
   Range:=Range.Next;
  end;
  if Count>0 then begin
   Process(@Root,0,Count-1);
  end;
 finally
  SetLength(Ranges,0);
 end;
end;

procedure TBRRERegExpCharClass.CompileCharset;
var Range:TBRRERegExpCharClassRange;
begin
 CharSet:=[];
 Range:=First;
 while assigned(Range) and (Range.Lo<256) do begin
  if Range.Lo=Range.Hi then begin
   System.Include(CharSet,ansichar(byte(Range.Lo)));
  end else begin
   if Range.Hi<256 then begin
    CharSet:=CharSet+[ansichar(byte(Range.Lo))..ansichar(byte(Range.Hi))];
   end else begin
    CharSet:=CharSet+[ansichar(byte(Range.Lo))..#$ff];
   end;
  end;
  Range:=Range.Next;
 end;
end;

procedure TBRRERegExpCharClass.Finalize;
begin
 CompileCharset;
 CompileBinarySearchTree;
end;

function TBRRERegExpCharClass.Count:longword;
var Range:TBRRERegExpCharClassRange;
begin
 result:=0;
 Range:=First;
 while assigned(Range) do begin
  inc(result,(Range.Hi-Range.Lo)+1);
  Range:=Range.Next;
 end;
end;

function TBRRERegExpCharClass.Contains(c:longword):boolean;
var Range:TBRRERegExpCharClassRange;
begin
 result:=false;
 Range:=Root;
 if assigned(Range) and ((c>=First.Lo) and (c<=Last.Hi)) then begin
  if First=Last then begin
   result:=true;
   exit;
  end else begin
   if c<256 then begin
    result:=ansichar(byte(c)) in CharSet;
   end else begin
    repeat
     if (c>=Range.Lo) and (c<=Range.Hi) then begin
      result:=true;
      break;
     end;
     if c<Range.Lo then begin
      Range:=Range.Left;
      continue;
     end;
     if Range.Hi<c then begin
      Range:=Range.Right;
      continue;
     end;
     result:=false;
     break;
    until not assigned(Range);
   end;
  end;
 end;
end;

function TBRRERegExpCharClass.Intersects(Other:TBRRERegExpCharClass):boolean;
var Range1,Range2:TBRRERegExpCharClassRange;
begin
 result:=false;
 if (assigned(Other) and (assigned(First) and assigned(Other.First))) and ((First.Lo<=Other.Last.Hi) and (Other.First.Lo<=Last.Hi)) then begin
  Range1:=First;
  while assigned(Range1) do begin
   Range2:=Other.First;
   while assigned(Range2) do begin
    if (Range1.Lo<=Range2.Hi) and (Range2.Lo<=Range1.Hi) then begin
     result:=true;
     exit;
    end;
    Range2:=Range2.Next;
   end;
   Range1:=Range1.Next;
  end;
 end;
end;

function TBRRERegExpCharClass.IsSingle:boolean;
begin
 result:=(First=Last) and ((assigned(First) and (First.Lo=First.Hi)) or not assigned(First));
end;

function TBRRERegExpCharClass.HashCode:longword;
var Range:TBRRERegExpCharClassRange;
    h,i:longword;
begin
 result:=$811c9dc5;
 Range:=First;
 h:=0;
 while assigned(Range) do begin
  inc(h);
  Range:=Range.Next;
 end;
 Range:=First;
 while assigned(Range) do begin
  i:=((Range.Hi and $ffff) shl 16) or (Range.Lo and $ffff);
  h:=(h xor i) xor $2e63823a;
  inc(h,(h shl 15) or (h shr (32-15)));
  dec(h,(h shl 9) or (h shr (32-9)));
  inc(h,(h shl 4) or (h shr (32-4)));
  dec(h,(h shl 1) or (h shr (32-1)));
  h:=h xor (h shl 2) or (h shr (32-2));
  result:=result xor i;
  inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
  i:=(Range.Hi and $ffff0000) or ((Range.Lo and $ffff0000) shr 16);
  h:=(h xor i) xor $2e63823a;
  inc(h,(h shl 15) or (h shr (32-15)));
  dec(h,(h shl 9) or (h shr (32-9)));
  inc(h,(h shl 4) or (h shr (32-4)));
  dec(h,(h shl 1) or (h shr (32-1)));
  h:=h xor (h shl 2) or (h shr (32-2));
  result:=result xor i;
  inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
  Range:=Range.Next;
 end;
 result:=result xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;

function CompareCharClasses(c1,c2:TBRRERegExpCharClass):longint;
var r1,r2:TBRRERegExpCharClassRange;
begin
 r1:=c1.First;
 r2:=c2.First;
 while assigned(r1) and assigned(r2) do begin
  if r1.Lo<>r2.Lo then begin
   result:=longint(r1.Lo)-longint(r2.Lo);
   exit;
  end;
  if r1.Hi<>r2.Hi then begin
   result:=longint(r1.Hi)-longint(r2.Hi);
   exit;
  end;
  r1:=r1.Next;
  r2:=r2.Next;
 end;
 if assigned(r1) then begin
  result:=1;
 end else if assigned(r2) then begin
  result:=-1;
 end else begin
  result:=0;
 end;
end;

function TBRRERegExpCharClass.EqualsTo(OtherObject:TBRREHashableObject):boolean;
begin
 result:=CompareCharClasses(self,TBRRERegExpCharClass(OtherObject))=0;
end;

function TBRRERegExpCharClasses.GetNodeItem(Index:longint):TBRRERegExpCharClass;
begin
 result:=TBRRERegExpCharClass(inherited GetItem(Index));
end;

procedure TBRRERegExpCharClasses.SetNodeItem(Index:longint;Value:TBRRERegExpCharClass);
begin
 inherited SetItem(Index,Value);
end;

function BRREHashDFAState(const Key:PBRREDFAState):longword;
begin
 if assigned(Key) and (Key.CountInstructions>0) then begin
  result:=((BRREHashData(@Key.Instructions[0],Key.CountInstructions*sizeof(PBRRERegExpNFAInstruction))) xor ((longword(Key.CountInstructions) shr 16) or (longword(Key.CountInstructions) shl 16))) xor ((Key.Flags shl 19) or (Key.Flags shr 13));
  if result=0 then begin
   result:=$ffffffff;
  end;
 end else begin
  result:=0;
 end;
end;

function BRRECompareDFAState(const a,b:PBRREDFAState):boolean;
var i:integer;
begin
 result:=a=b;
 if not result then begin
  if (assigned(a) and assigned(b)) and ((a.CountInstructions=b.CountInstructions) and (a.Flags=b.Flags)) then begin
   result:=true;
   for i:=0 to a.CountInstructions-1 do begin
    if a.Instructions[i]<>b.Instructions[i] then begin
     result:=false;
     exit;
    end;
   end;
  end;
 end;
end;

constructor TBRREDFAStateHashMap.Create;
begin
 inherited Create;
 RealSize:=0;
 LogSize:=0;
 Size:=0;
 Entities:=nil;
 EntityToCellIndex:=nil;
 CellToEntityIndex:=nil;
 Resize;
end;

destructor TBRREDFAStateHashMap.Destroy;
begin
 Clear;
 SetLength(Entities,0);
 inherited Destroy;
end;

procedure TBRREDFAStateHashMap.Clear;
begin
 RealSize:=0;
 LogSize:=0;
 Size:=0;
 SetLength(Entities,0);
 SetLength(EntityToCellIndex,0);
 SetLength(CellToEntityIndex,0);
 Resize;
end;

function TBRREDFAStateHashMap.FindCell(const Key:PBRREDFAState):longword;
var HashCode,Mask,Step:longword;
    Entity:longint;
begin
 HashCode:=BRREHashDFAState(Key);
 Mask:=(2 shl LogSize)-1;
 Step:=((HashCode shl 1)+1) and Mask;
 if LogSize<>0 then begin
  result:=HashCode shr (32-LogSize);
 end else begin
  result:=0;
 end;
 repeat
  Entity:=CellToEntityIndex[result];
  if (Entity=ENT_EMPTY) or ((Entity<>ENT_DELETED) and BRRECompareDFAState(Entities[Entity].Key,Key)) then begin
   exit;
  end;
  result:=(result+Step) and Mask;
 until false;
end;

function TBRREDFAStateHashMap.Add(const Key:PBRREDFAState):PBRREDFAStateHashMapEntity;
var Entity:longint;
    Cell:longword;
begin
 result:=nil;
 while RealSize>=(1 shl LogSize) do begin
  Resize;
 end;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@Entities[Entity];
  result^.Key:=Key;
  exit;
 end;
 Entity:=Size;
 inc(Size);
 if Entity<(2 shl LogSize) then begin
  CellToEntityIndex[Cell]:=Entity;
  EntityToCellIndex[Entity]:=Cell;
  inc(RealSize);
  result:=@Entities[Entity];
  result^.Key:=Key;
 end;
end;

procedure TBRREDFAStateHashMap.Resize;
var NewLogSize,NewSize,Cell,Entity,Counter:longint;
    OldEntities:TBRREDFAStateHashMapEntities;
    OldCellToEntityIndex:TBRREDFAStateHashMapEntityIndices;
    OldEntityToCellIndex:TBRREDFAStateHashMapEntityIndices;
begin
 NewLogSize:=0;
 NewSize:=RealSize;
 while NewSize<>0 do begin
  NewSize:=NewSize shr 1;
  inc(NewLogSize);
 end;
 if NewLogSize<1 then begin
  NewLogSize:=1;
 end;
 Size:=0;
 RealSize:=0;
 LogSize:=NewLogSize;
 OldEntities:=Entities;
 OldCellToEntityIndex:=CellToEntityIndex;
 OldEntityToCellIndex:=EntityToCellIndex;
 Entities:=nil;
 CellToEntityIndex:=nil;
 EntityToCellIndex:=nil;
 SetLength(Entities,2 shl LogSize);
 SetLength(CellToEntityIndex,2 shl LogSize);
 SetLength(EntityToCellIndex,2 shl LogSize);
 for Counter:=0 to length(CellToEntityIndex)-1 do begin
  CellToEntityIndex[Counter]:=ENT_EMPTY;
 end;
 for Counter:=0 to length(EntityToCellIndex)-1 do begin
  EntityToCellIndex[Counter]:=CELL_EMPTY;
 end;
 for Counter:=0 to length(OldEntityToCellIndex)-1 do begin
  Cell:=OldEntityToCellIndex[Counter];
  if Cell>=0 then begin
   Entity:=OldCellToEntityIndex[Cell];
   if Entity>=0 then begin
    Add(OldEntities[Counter].Key);
   end;
  end;
 end;
end;

function TBRREDFAStateHashMap.Get(const Key:PBRREDFAState):PBRREDFAState;
var Entity:longint;
    Cell:longword;
begin
 result:=nil;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=Entities[Entity].Key;
 end;
end;

function TBRREDFAStateHashMap.Delete(const Key:PBRREDFAState):boolean;
var Entity:longint;
    Cell:longword;
begin
 result:=false;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  Entities[Entity].Key:=nil;
  Entities[Entity].Value:=0;
  EntityToCellIndex[Entity]:=CELL_DELETED;
  CellToEntityIndex[Cell]:=ENT_DELETED;
  dec(RealSize);
  result:=true;
 end;
end;

constructor TBRREDFA.Create(ARegExpProgram:TBRRERegExpProgram;ARegExpDataInstance:TBRRERegExpDataInstance;ARegExpProgramData:PBRRERegExpProgramDataInstance);
var BRREDFAStateCreateTempDFAState:TBRREDFAState;
    Flags:longword;
begin
 inherited Create;

 RegExp:=ARegExpProgram.RegExp;
 RegExpProgram:=ARegExpProgram;
 RegExpDataInstance:=ARegExpDataInstance;
 RegExpProgramData:=ARegExpProgramData;

 StateCache:=TBRREDFAStateHashMap.Create;

 StartState:=nil;
 FillChar(StartStates,SizeOf(StartStates),#0);

 CountStatesCached:=0;

 StackInstructions:=nil;
 SetLength(StackInstructions,(length(RegExpProgram.AnchoredInstructions)+1) shl 1);

 ByteMap:=RegExpProgram.ByteMap;
 ByteCharSetMap:=RegExpProgram.ByteCharSetMap;

 StatePoolUsed:=nil;
 StatePoolFree:=nil;   

 NextStatesSize:=RegExpProgram.ByteCount*sizeof(PBRREDFAState);
 DFAStateSize:=ptrint(ptruint(pointer(@BRREDFAStateCreateTempDFAState.Latin1NextStates))-ptruint(pointer(@BRREDFAStateCreateTempDFAState)))+NextStatesSize;

 StatePoolSize:=(65536 div DFAStateSize)*DFAStateSize;
 if StatePoolSize=0 then begin
  StatePoolSize:=DFAStateSize*64;
 end;

 StatePoolSizePowerOfTwo:=BRRERoundUpToPowerOfTwo(StatePoolSize);

 AllocateNewStatePool;

 FillChar(TemporaryState,SizeOf(TBRREDFAState),AnsiChar(#0));
 FillChar(NewState,SizeOf(TBRREDFAState),AnsiChar(#0));

 FillChar(StartStates,SizeOf(StartStates),AnsiChar(#0));

 case RegExpProgram.DFAMode of
  brredmFAST:begin
   inc(RegExpProgramData^.GenerationID);
   GetMem(StartState,DFAStateSize);
   FillChar(StartState^,DFAStateSize,AnsiChar(#0));
   FastAddInstructionThread(StartState,RegExpProgram.AnchoredStart);
   StartState^.Flags:=StartState^.Flags or brresfDFAStart;
   StateCache.Add(StartState);
   inc(CountStatesCached);
   StartStates[brresfEmptyAllFlags]:=StartState;
  end;
  else {brredmFULL:}begin
   for Flags:=low(StartStates) to high(StartStates) do begin
    inc(RegExpProgramData^.GenerationID);
    GetMem(StartState,DFAStateSize);
    FillChar(StartState^,DFAStateSize,AnsiChar(#0));
    StartState^.Flags:=Flags;
    FullAddInstructionThread(StartState,RegExpProgram.AnchoredStart,Flags);
    StartState^.Flags:=StartState^.Flags or brresfDFAStart;
    StateCache.Add(StartState);
    inc(CountStatesCached);
    StartStates[Flags]:=StartState;
   end;
  end;
 end;
 
end;

destructor TBRREDFA.Destroy;
var Flags:longword;
begin
 DestroyStatePool(StatePoolUsed);
 DestroyStatePool(StatePoolFree);
 StatePoolUsed:=nil;
 StatePoolFree:=nil;
 FreeState(@TemporaryState);
 FreeState(@NewState);
 for Flags:=low(StartStates) to high(StartStates) do begin
  if assigned(StartStates[Flags]) then begin
   FreeState(StartStates[Flags]);
   FreeMem(StartStates[Flags]);
  end;
 end;
 FreeAndNil(StateCache);
 SetLength(StackInstructions,0);
 inherited Destroy;
end;

procedure TBRREDFA.DestroyStatePool(StatePool:PBRREDFAStatePool);
var Pool,NextPool:PBRREDFAStatePool;
    State:PBRREDFAState;
begin
 Pool:=StatePool;
 while assigned(Pool) do begin
  NextPool:=Pool^.Next;
  State:=Pool^.States;
  while assigned(State) and (State<>Pool^.EndState) do begin
   FreeState(State);
   inc(ptruint(State),DFAStateSize);
  end;
  FreeMem(Pool^.States);
  FreeMem(Pool);
  Pool:=NextPool;
 end;
end;

procedure TBRREDFA.FreeUsedStatePool;
var Pool,NextPool:PBRREDFAStatePool;
    State:PBRREDFAState;
begin
 Pool:=StatePoolUsed;
 StatePoolUsed:=nil;
 while assigned(Pool) do begin
  NextPool:=Pool^.Next;
  State:=Pool^.States;
  while assigned(State) and (State<>Pool^.EndState) do begin
   FreeState(State);
   inc(ptruint(State),DFAStateSize);
  end;
  FillChar(Pool^.States^,StatePoolSize,AnsiChar(#0));
  Pool^.Next:=StatePoolFree;
  StatePoolFree:=Pool;
  Pool:=NextPool;
 end;
end;

function TBRREDFA.AllocateNewStatePool:PBRREDFAStatePool;
begin
 if assigned(StatePoolFree) then begin
  result:=StatePoolFree;
  StatePoolFree:=result^.Next;
 end else begin
  GetMem(result,SizeOf(TBRREDFAStatePool));
  FillChar(result^,SizeOf(TBRREDFAStatePool),AnsiChar(#0));
  GetMem(result^.States,StatePoolSizePowerOfTwo);
  FillChar(result^.States^,StatePoolSize,AnsiChar(#0));
  result^.EndState:=pointer(ptruint(ptruint(result^.States)+ptruint(StatePoolSize)));
 end;
 result^.Next:=StatePoolUsed;
 StatePoolUsed:=result;
 result^.NextState:=result^.States;
end;

function TBRREDFA.GetState:PBRREDFAState;
begin
 if StatePoolUsed^.NextState=StatePoolUsed^.EndState then begin
  AllocateNewStatePool;
 end;
 result:=StatePoolUsed^.NextState;
 inc(ptruint(StatePoolUsed^.NextState),DFAStateSize);
end;

function TBRREDFA.TakeoverState(TakeoverFrom:PBRREDFAState):PBRREDFAState;
begin
 result:=GetState;
 result^.Instructions:=TakeoverFrom^.Instructions;
 result^.CountInstructions:=TakeoverFrom^.CountInstructions;
 result^.Flags:=TakeoverFrom^.Flags;
 TakeoverFrom^.Instructions:=nil;
 TakeoverFrom^.CountInstructions:=0;
 TakeoverFrom^.Flags:=0;
end;

procedure TBRREDFA.FreeState(State:PBRREDFAState);
var CurrentRangePagedNextStates,NextRangePagedNextStates:PBRREDFARangePagedNextStates;
begin
 if assigned(State) then begin
  SetLength(State^.Instructions,0);
  CurrentRangePagedNextStates:=State^.RangePagedNextStates;
  State^.RangePagedNextStates:=nil;
  while assigned(CurrentRangePagedNextStates) do begin
   NextRangePagedNextStates:=CurrentRangePagedNextStates^.Next;
   Dispose(CurrentRangePagedNextStates);
   CurrentRangePagedNextStates:=NextRangePagedNextStates;
  end;
  if assigned(StatePoolUsed) and
     ((ptruint(ptruint(State)-ptruint(StatePoolUsed^.States))<StatePoolSize) and
      (pointer(ptruint(ptruint(StatePoolUsed^.NextState)-ptruint(DFAStateSize)))=State)) then begin
   FillChar(State^,DFAStateSize,AnsiChar(#0));
   dec(ptruint(StatePoolUsed^.NextState),DFAStateSize);
  end;
 end;
end;

procedure TBRREDFA.Reset;
var State:PBRREDFAState;
    Flags:longword;
    RangePagedNextStates:PBRREDFARangePagedNextStates;
begin

 // Reset state pools
 FreeUsedStatePool;
 AllocateNewStatePool;

 // Reset state cache
 StateCache.Clear;
 CountStatesCached:=0;

 // Reset and recaching start states
 for Flags:=low(StartStates) to high(StartStates) do begin

  State:=StartStates[Flags];
  if assigned(State) then begin

   // Clear latin1 next states
   FillChar(State^.Latin1NextStates,NextStatesSize,AnsiChar(#0));

   // Clear non-latin1 next states
   RangePagedNextStates:=State^.RangePagedNextStates;
   while assigned(RangePagedNextStates) do begin
    FillChar(RangePagedNextStates^.NextStates,SizeOf(RangePagedNextStates^.NextStates),AnsiChar(#0));
    RangePagedNextStates:=RangePagedNextStates^.Next;
   end;

   // Recache state
   StateCache.Add(State);
   inc(CountStatesCached);
   
  end;

 end;

end;

procedure TBRREDFA.ConnectState(State,NextState:PBRREDFAState;CurrentChar:longword;Fast:boolean); {$IFDEF caninline}inline;{$ENDIF}
var RangePagedNextStates:PBRREDFARangePagedNextStates;
begin

 // Is the current char latin1 and optionally in the latin1 byte charset map of the current DFA instruction program stream?
 if (CurrentChar<256) and (Fast or (ansichar(byte(CurrentChar)) in ByteCharSetMap)) then begin

  // Yes, so store the next state to the associated latin1 next state slot of the previous state
  State.Latin1NextStates[ByteMap[CurrentChar]]:=NextState;

 end else begin

  // No, so store the next state to a extended range page next state block slot of the previous state

  // Search already existed RangePagedNextStates for a existent associated page
  RangePagedNextStates:=State^.RangePagedNextStates;
  while assigned(RangePagedNextStates) and (RangePagedNextStates^.Page<>(CurrentChar and $ffffff00)) do begin
   RangePagedNextStates:=RangePagedNextStates^.Next;
  end;

  // Check if a associated RangePagedNextState is existing
  if not assigned(RangePagedNextStates) then begin

   // No, so allocate it and link it to the previous RangePagedNextState

   New(RangePagedNextStates);
   FillChar(RangePagedNextStates^,sizeof(TBRREDFARangePagedNextStates),#0);
   RangePagedNextStates^.Next:=State.RangePagedNextStates;
   State.RangePagedNextStates:=RangePagedNextStates;
   RangePagedNextStates^.Page:=CurrentChar and $ffffff00;

  end;

  // Store the next state to the associated next state slot of this RangePagedNextState
  RangePagedNextStates^.NextStates[CurrentChar and $ff]:=NextState;

 end;
end;

function TBRREDFA.CacheState(State:PBRREDFAState):PBRREDFAState; {$IFDEF caninline}inline;{$ENDIF}
begin

 // Is it already cached?
 result:=StateCache[State];
 if not assigned(result) then begin
  // No, it is not already cached yet

  // Do we need reset the states?
  if CountStatesCached>=brreMaxDFAStates then begin
   // Yes, so do it
   Reset;
  end;

  // Move state in an own new memory instance
  result:=TakeoverState(State);

  // Add state to state cache hash map
  StateCache.Add(result);
  inc(CountStatesCached);

 end;

end;

procedure TBRREDFA.FastAddInstructionThread(State:PBRREDFAState;Instruction:PBRRERegExpNFAInstruction);
var StackPointer:longint;
    Stack:PPBRRERegExpNFAInstructionsStatic;
    InstructionGenerationIDs:PBRRERegExpDataInstanceInstructionGenerationIDsStatic;
begin
 Stack:=@StackInstructions[0];
 InstructionGenerationIDs:=@RegExpProgramData^.InstructionGenerationIDs[0];
 StackPointer:=0;
 Stack^[StackPointer]:=Instruction;
 inc(StackPointer);
 while StackPointer>0 do begin
  dec(StackPointer);
  Instruction:=Stack[StackPointer];
  while assigned(Instruction) and (InstructionGenerationIDs^[Instruction^.IDandOpcode shr 8]<>RegExpProgramData^.GenerationID) do begin
   InstructionGenerationIDs^[Instruction^.IDandOpcode shr 8]:=RegExpProgramData^.GenerationID;
   case Instruction^.IDandOpcode and $ff of
    brreoJMP,brreoBEGIN,brreoEND,brreoPREFIX,brreoPREFIXCHAR,brreoCALLOUT:begin
     Instruction:=Instruction^.x;
    end;
    brreoLOOKAHEADFALSE,brreoLOOKAHEADTRUE,brreoLOOKBEHINDFALSE,brreoLOOKBEHINDTRUE:begin
     Instruction:=Instruction^.y;
    end;
    brreoSPLIT,brreoSPLITLONGEST,brreoTESTBACKREF,brreoTEST:begin
     Stack^[StackPointer]:=Instruction^.y;
     inc(StackPointer);
     Instruction:=Instruction^.x;
    end;
    else begin
     if State.CountInstructions>=length(State.Instructions) then begin
      SetLength(State.Instructions,BRRERoundUpToPowerOfTwo(State.CountInstructions+1));
     end;
     State.Instructions[State.CountInstructions]:=Instruction;
     inc(State.CountInstructions);
     case Instruction^.IDandOpcode and $ff of
      brreoMATCH:begin
       State.Flags:=State.Flags or brresfDFAMatchWins;
      end;
      brreoBACKREF,brreoBACKREFI:begin
       Instruction:=Instruction^.x;
       continue;
      end;
      brreoTEST:begin
       Instruction:=Instruction^.y;
       continue;
      end;
     end;
     break;
    end;
   end;
  end;
 end;
end;

function TBRREDFA.FastProcessNextState(State:PBRREDFAState;CurrentChar:longword):PBRREDFAState;
var Counter:longint;
    Instruction:PBRRERegExpNFAInstruction;
begin

 // Process state instructions
 inc(RegExpProgramData^.GenerationID);
 NewState.CountInstructions:=0;
 NewState.Flags:=(State^.Flags and brresfDFAStart) shl brresfDFANeedShift;
 for Counter:=0 To State^.CountInstructions-1 do begin
  Instruction:=State^.Instructions[Counter];
  case Instruction^.IDandOpcode and $ff of
   brreoCHAR:begin
    if ((CurrentChar<256) and (ansichar(byte(CurrentChar)) in TBRRERegExpCharClass(pointer(Instruction^.Value)).Charset)) or
       ((CurrentChar>255) and TBRRERegExpCharClass(pointer(Instruction^.Value)).Contains(CurrentChar)) then begin
     FastAddInstructionThread(@NewState,Instruction^.x);
    end;
   end;
   brreoCHARSINGLE:begin
    if CurrentChar=ptruint(Instruction^.Value) then begin
     FastAddInstructionThread(@NewState,Instruction^.x);
    end;
   end;
   brreoMATCH:begin
    if (RegExp.Flags and brrefLONGEST)=0 then begin
     // We can stop processing the instruction list queue here since we found a match (for the PCRE leftmost first valid match behaviour)
     break;
    end;
   end;
   brreoBACKREF,brreoBACKREFI:begin
    if CurrentChar<>$ffffffff then begin
     FastAddInstructionThread(@NewState,Instruction^.x);
     FastAddInstructionThread(@NewState,Instruction);
    end;
   end;
   brreoRECURSIVE:begin
    if CurrentChar<>$ffffffff then begin
     FastAddInstructionThread(@NewState,Instruction^.y);
     FastAddInstructionThread(@NewState,Instruction);
    end;
   end;
   else begin
    // Oops?! Invalid byte code instruction for the DFA processing context! So abort and fallback to NFA...
    result:=nil;
    exit;
   end;
  end;
 end;

 // Dead state? If yes, ...
 if NewState.CountInstructions=0 then begin
  // ... drop it and take the start state as the next state
  result:=StartState;
 end else begin
  // .. otherwise try caching it
  result:=CacheState(@NewState);
 end;

 // Connect the last state to the new state with the current char
 ConnectState(State,result,CurrentChar,true);
end;

function TBRREDFA.SearchFastLatin1(const Input:ansistring):longint;
{$IFDEF cpu386}
var ResultValue:longint;
    StartChar:pointer;
asm
 push ebx
 push esi
 push edi

  mov dword ptr ResultValue,brreDFAFail

  mov esi,Input

  mov dword ptr StartChar,esi

  mov edx,self

  add esi,dword ptr [edx+StartCodeUnit]
  dec esi

  mov edi,dword ptr [edx+TBRREDFA.StartState]

  mov ecx,dword ptr [edx+TBRREDFA.RegExpDataInstance]
  mov ecx,dword ptr [ecx+TBRRERegExpDataInstance.InputUnitsLength]
  sub ecx,dword ptr [edx+StartCodeUnit]
  inc ecx
  cmp ecx,0
  jle @Done

  @Loop:

    movzx eax,byte ptr [esi]
    inc esi

    movzx ebx,byte ptr [edx+TBRREDFA.ByteMap+eax]
    mov ebx,dword ptr [edi+TBRREDFAState.Latin1NextStates+ebx*4]
    xchg edi,ebx
    test edi,edi
    jz @HaveNoNextState
    @HaveNextState:

    mov ebx,dword ptr [edi+TBRREDFAState.Flags]
    test ebx,brresfDFAStartShifted or brresfDFAMatchWins
    jnz @CheckFlags

   @BackToLoop:
   dec ecx
   jnz @Loop
   jmp @Done

   @CheckFlags:
    test ebx,brresfDFAStartShifted
    jz @IsNotStartShifted
     cmp dword ptr ResultValue,brreDFAMatch
     jz @Done
     mov eax,esi
     sub eax,dword ptr StartChar
     mov dword ptr [edx+StartCodeUnit],eax
    @IsNotStartShifted:
    test ebx,brresfDFAMatchWins
    jz @IsNotMatchWin
     mov eax,esi
     sub eax,dword ptr StartChar
     inc eax
     mov dword ptr [edx+EndCodeUnit],eax
     mov dword ptr ResultValue,brreDFAMatch
    @IsNotMatchWin:
    jmp @BackToLoop

   @HaveNoNextState:
    push ecx
    push edx
     mov ecx,eax
     mov eax,edx
     mov edx,ebx
     call FastProcessNextState
    pop edx
    pop ecx
    mov edi,eax
    test edi,edi
    jnz @HaveNextState

    mov dword ptr ResultValue,brreDFAError

  @Done:

  mov eax,dword ptr [edx+StartCodeUnit]
  dec eax
  mov dword ptr [edx+StartCodePoint],eax

  mov eax,dword ptr [edx+EndCodeUnit]
  dec eax
  mov dword ptr [edx+EndCodePoint],eax

  mov eax,dword ptr ResultValue

 pop edi
 pop esi
 pop ebx
end;
{$ELSE}
var CodeUnit,Len:longint;
    State,LastState:PBRREDFAState;
begin

 result:=brreDFAFail;

 CodeUnit:=StartCodeUnit;

 Len:=RegExpDataInstance.InputUnitsLength;

 State:=StartState;

 while CodeUnit<=Len do begin

  LastState:=State;

  State:=State.Latin1NextStates[ByteMap[byte(Input[CodeUnit])]];

  if not assigned(State) then begin
   State:=FastProcessNextState(LastState,byte(Input[CodeUnit]));
   if not assigned(State) then begin
    result:=brreDFAError;
    break;
   end;
  end;

  if (State.Flags and (brresfDFAStartShifted or brresfDFAMatchWins))<>0 then begin
   if (State.Flags and brresfDFAStartShifted)<>0 then begin
    if result=brreDFAMatch then begin
     // If we have already a valid match, we don't need to reset, but we do need stop the processing
     break;
    end;
    StartCodeUnit:=CodeUnit;
   end;
   if (State.Flags and brresfDFAMatchWins)<>0 then begin
    EndCodeUnit:=CodeUnit+1;
    result:=brreDFAMatch;
   end;
  end;

  inc(CodeUnit);

 end;

 StartCodePoint:=StartCodeUnit-1;
 EndCodePoint:=EndCodeUnit-1;

end;
{$ENDIF}

function TBRREDFA.SearchFastLatin1FirstChar(const Input:ansistring):longint;
var CodeUnit,Len:longint;
    State,LastState:PBRREDFAState;
begin

 result:=brreDFAFail;

 CodeUnit:=StartCodeUnit;

 Len:=RegExpDataInstance.InputUnitsLength;

 State:=StartState;

 while CodeUnit<=Len do begin

  LastState:=State;

  State:=State.Latin1NextStates[ByteMap[byte(Input[CodeUnit])]];

  if not assigned(State) then begin
   State:=FastProcessNextState(LastState,byte(Input[CodeUnit]));
   if not assigned(State) then begin
    result:=brreDFAError;
    break;
   end;
  end;

  if (State.Flags and (brresfDFAStart or brresfDFAStartShifted or brresfDFAMatchWins))<>0 then begin
   if (State.Flags and brresfDFAStartShifted)<>0 then begin
    if result=brreDFAMatch then begin
     // If we have already a valid match, we don't need to reset, but we do need stop the processing
     break;
    end;
    StartCodeUnit:=CodeUnit;
   end;
   if (State.Flags and brresfDFAMatchWins)<>0 then begin
    EndCodeUnit:=CodeUnit+1;
    result:=brreDFAMatch;
   end;
   if (State.Flags and brresfDFAStart)<>0 then begin
    if PrefixCharRangeLow=PrefixCharRangeHigh then begin
     CodeUnit:=PtrPosChar(ansichar(byte(ansichar(byte(PrefixCharRangeLow)))),@Input[1],Len,CodeUnit-1);
    end else begin
     CodeUnit:=PtrPosCharRange(ansichar(byte(PrefixCharRangeLow)),ansichar(byte(PrefixCharRangeHigh)),@Input[1],Len,CodeUnit-1);
    end;
    if CodeUnit<0 then begin
     break;
    end;
   end;
  end;

  inc(CodeUnit);

 end;

 StartCodePoint:=StartCodeUnit-1;
 EndCodePoint:=EndCodeUnit-1;

end;

function TBRREDFA.SearchFastUTF8(const Input:ansistring):longint;
var CodeUnit,NextCodeUnit,Len,CodePoint:longint;
    CurrentChar:longword;
    State,LastState:PBRREDFAState;
    RangePagedNextStates:PBRREDFARangePagedNextStates;
begin

 result:=brreDFAFail;

 CodeUnit:=StartCodeUnit;
 CodePoint:=StartCodePoint;

 NextCodeUnit:=StartCodeUnit;

 Len:=RegExpDataInstance.InputUnitsLength;

 State:=StartState;

 while CodeUnit<=Len do begin

  NextCodeUnit:=CodeUnit;
  CurrentChar:=BRREUTF8CodeUnitGetCharAndInc(Input,NextCodeUnit);

  LastState:=State;

  if CurrentChar<256 then begin
   State:=State.Latin1NextStates[ByteMap[CurrentChar]];
  end else begin
   State:=nil;
   RangePagedNextStates:=State.RangePagedNextStates;
   while assigned(RangePagedNextStates) do begin
    if RangePagedNextStates^.Page=(CurrentChar and $ffffff00) then begin
     State:=RangePagedNextStates^.NextStates[CurrentChar and $ff];
     break;
    end;
    RangePagedNextStates:=RangePagedNextStates^.Next;
   end;
  end;

  if not assigned(State) then begin
   State:=FastProcessNextState(LastState,CurrentChar);
   if not assigned(State) then begin
    result:=brreDFAError;
    break;
   end;
  end;

  if (State.Flags and (brresfDFAStartShifted or brresfDFAMatchWins))<>0 then begin
   if (State.Flags and brresfDFAStartShifted)<>0 then begin
    if result=brreDFAMatch then begin
     // If we have already a valid match, we don't need to reset, but we do need stop the processing
     break;
    end;
    StartCodeUnit:=CodeUnit;
    StartCodePoint:=CodePoint;
   end;
   if (State.Flags and brresfDFAMatchWins)<>0 then begin
    EndCodePoint:=CodePoint+1;
    EndCodeUnit:=NextCodeUnit;
    result:=brreDFAMatch;
   end;
  end;

  CodeUnit:=NextCodeUnit;
  inc(CodePoint);

 end;

end;

function TBRREDFA.SearchFastUTF8FirstChar(const Input:ansistring):longint;
var CodeUnit,NextCodeUnit,Len,CodePoint:longint;
    CurrentChar:longword;
    State,LastState:PBRREDFAState;
    RangePagedNextStates:PBRREDFARangePagedNextStates;
begin

 result:=brreDFAFail;

 CodeUnit:=StartCodeUnit;
 CodePoint:=StartCodePoint;

 NextCodeUnit:=StartCodeUnit;

 Len:=RegExpDataInstance.InputUnitsLength;

 State:=StartState;

 while CodeUnit<=Len do begin

  NextCodeUnit:=CodeUnit;
  CurrentChar:=BRREUTF8CodeUnitGetCharAndInc(Input,NextCodeUnit);

  LastState:=State;

  if CurrentChar<256 then begin
   State:=State.Latin1NextStates[ByteMap[CurrentChar]];
  end else begin
   State:=nil;
   RangePagedNextStates:=State.RangePagedNextStates;
   while assigned(RangePagedNextStates) do begin
    if RangePagedNextStates^.Page=(CurrentChar and $ffffff00) then begin
     State:=RangePagedNextStates^.NextStates[CurrentChar and $ff];
     break;
    end;
    RangePagedNextStates:=RangePagedNextStates^.Next;
   end;
  end;

  if not assigned(State) then begin
   State:=FastProcessNextState(LastState,CurrentChar);
   if not assigned(State) then begin
    result:=brreDFAError;
    break;
   end;
  end;

  if (State.Flags and (brresfDFAStart or brresfDFAStartShifted or brresfDFAMatchWins))<>0 then begin
   if (State.Flags and brresfDFAStartShifted)<>0 then begin
    if result=brreDFAMatch then begin
     // If we have already a valid match, we don't need to reset, but we do need stop the processing
     break;
    end;
    StartCodeUnit:=CodeUnit;
    StartCodePoint:=CodePoint;
   end;
   if (State.Flags and brresfDFAMatchWins)<>0 then begin
    EndCodePoint:=CodePoint+1;
    EndCodeUnit:=NextCodeUnit;
    result:=brreDFAMatch;
   end;
   if (State.Flags and brresfDFAStart)<>0 then begin
    inc(CodePoint);
    if PrefixCharRangeLow=PrefixCharRangeHigh then begin
     CodeUnit:=PtrPosCharUTF8(ansichar(byte(PrefixCharRangeLow)),@Input[1],Len,CodePoint,NextCodeUnit-1);
    end else begin
     CodeUnit:=PtrPosCharRangeUTF8(ansichar(byte(PrefixCharRangeLow)),ansichar(byte(PrefixCharRangeHigh)),@Input[1],Len,CodePoint,NextCodeUnit-1);
    end;
    if CodeUnit<0 then begin
     break;
    end;
    inc(CodeUnit);
    continue;
   end;
  end;

  CodeUnit:=NextCodeUnit;
  inc(CodePoint);

 end;

end;

function TBRREDFA.PtrSearchFastLatin1(const Input:pansichar):longint;
{$IFDEF cpu386}
var ResultValue:longint;
    StartChar:pointer;
asm
 push ebx
 push esi
 push edi

  mov dword ptr ResultValue,brreDFAFail

  mov esi,Input

  mov dword ptr StartChar,esi

  mov edx,self

  add esi,dword ptr [edx+StartCodeUnit]
  dec esi

  mov edi,dword ptr [edx+TBRREDFA.StartState]

  mov ecx,dword ptr [edx+TBRREDFA.RegExpDataInstance]
  mov ecx,dword ptr [ecx+TBRRERegExpDataInstance.InputUnitsLength]
  sub ecx,dword ptr [edx+StartCodeUnit]
  inc ecx
  cmp ecx,0
  jle @Done

  @Loop:

    movzx eax,byte ptr [esi]
    inc esi

    movzx ebx,byte ptr [edx+TBRREDFA.ByteMap+eax]
    mov ebx,dword ptr [edi+TBRREDFAState.Latin1NextStates+ebx*4]
    xchg edi,ebx
    test edi,edi
    jz @HaveNoNextState
    @HaveNextState:

    mov ebx,dword ptr [edi+TBRREDFAState.Flags]
    test ebx,brresfDFAStartShifted or brresfDFAMatchWins
    jnz @CheckFlags

   @BackToLoop:
   dec ecx
   jnz @Loop
   jmp @Done

   @CheckFlags:
    test ebx,brresfDFAStartShifted
    jz @IsNotStartShifted
     cmp dword ptr ResultValue,brreDFAMatch
     jz @Done
     mov eax,esi
     sub eax,dword ptr StartChar
     dec eax
     mov dword ptr [edx+StartCodeUnit],eax
    @IsNotStartShifted:
    test ebx,brresfDFAMatchWins
    jz @IsNotMatchWin
     mov eax,esi
     sub eax,dword ptr StartChar
     mov dword ptr [edx+EndCodeUnit],eax
     mov dword ptr ResultValue,brreDFAMatch
    @IsNotMatchWin:
    jmp @BackToLoop

   @HaveNoNextState:
    push ecx
    push edx
     mov ecx,eax
     mov eax,edx
     mov edx,ebx
     call FastProcessNextState
    pop edx
    pop ecx
    mov edi,eax
    test edi,edi
    jnz @HaveNextState

    mov dword ptr ResultValue,brreDFAError

  @Done:

  mov eax,dword ptr [edx+StartCodeUnit]
  mov dword ptr [edx+StartCodePoint],eax

  mov eax,dword ptr [edx+EndCodeUnit]
  mov dword ptr [edx+EndCodePoint],eax

  mov eax,dword ptr ResultValue

 pop edi
 pop esi
 pop ebx
end;
{$ELSE}
var CodeUnit,Len:longint;
    State,LastState:PBRREDFAState;
begin

 result:=brreDFAFail;

 CodeUnit:=StartCodeUnit;

 Len:=RegExpDataInstance.InputUnitsLength;

 State:=StartState;

 while CodeUnit<Len do begin

  LastState:=State;

  State:=State.Latin1NextStates[ByteMap[byte(Input[CodeUnit])]];

  if not assigned(State) then begin
   State:=FastProcessNextState(LastState,byte(Input[CodeUnit]));
   if not assigned(State) then begin
    result:=brreDFAError;
    break;
   end;
  end;

  if (State.Flags and (brresfDFAStartShifted or brresfDFAMatchWins))<>0 then begin
   if (State.Flags and brresfDFAStartShifted)<>0 then begin
    if result=brreDFAMatch then begin
     // If we have already a valid match, we don't need to reset, but we do need stop the processing
     break;
    end;
    StartCodeUnit:=CodeUnit;
   end;
   if (State.Flags and brresfDFAMatchWins)<>0 then begin
    EndCodeUnit:=CodeUnit+1;
    result:=brreDFAMatch;
   end;
  end;

  inc(CodeUnit);

 end;

 StartCodePoint:=StartCodeUnit;
 EndCodePoint:=EndCodeUnit;

end;
{$ENDIF}

function TBRREDFA.PtrSearchFastLatin1FirstChar(const Input:pansichar):longint;
var CodeUnit,Len:longint;
    State,LastState:PBRREDFAState;
begin

 result:=brreDFAFail;

 CodeUnit:=StartCodeUnit;

 Len:=RegExpDataInstance.InputUnitsLength;

 State:=StartState;

 while CodeUnit<Len do begin

  LastState:=State;

  State:=State.Latin1NextStates[ByteMap[byte(Input[CodeUnit])]];

  if not assigned(State) then begin
   State:=FastProcessNextState(LastState,byte(Input[CodeUnit]));
   if not assigned(State) then begin
    result:=brreDFAError;
    break;
   end;
  end;

  if (State.Flags and (brresfDFAStart or brresfDFAStartShifted or brresfDFAMatchWins))<>0 then begin
   if (State.Flags and brresfDFAStartShifted)<>0 then begin
    if result=brreDFAMatch then begin
     // If we have already a valid match, we don't need to reset, but we do need stop the processing
     break;
    end;
    StartCodeUnit:=CodeUnit;
   end;
   if (State.Flags and brresfDFAMatchWins)<>0 then begin
    EndCodeUnit:=CodeUnit+1;
    result:=brreDFAMatch;
   end;
   if (State.Flags and brresfDFAStart)<>0 then begin
    if PrefixCharRangeLow=PrefixCharRangeHigh then begin
     CodeUnit:=PtrPosChar(ansichar(byte(ansichar(byte(PrefixCharRangeLow)))),Input,Len,CodeUnit+1);
    end else begin
     CodeUnit:=PtrPosCharRange(ansichar(byte(PrefixCharRangeLow)),ansichar(byte(PrefixCharRangeHigh)),Input,Len,CodeUnit+1);
    end;
    if CodeUnit<0 then begin
     break;
    end;
    continue;
   end;
  end;

  inc(CodeUnit);

 end;

 StartCodePoint:=StartCodeUnit;
 EndCodePoint:=EndCodeUnit;

end;

function TBRREDFA.PtrSearchFastUTF8(const Input:pansichar):longint;
var CodeUnit,NextCodeUnit,Len,CodePoint:longint;
    CurrentChar:longword;
    State,LastState:PBRREDFAState;
    RangePagedNextStates:PBRREDFARangePagedNextStates;
begin

 result:=brreDFAFail;

 CodeUnit:=StartCodeUnit;
 CodePoint:=StartCodePoint;

 NextCodeUnit:=StartCodeUnit;

 Len:=RegExpDataInstance.InputUnitsLength;

 State:=StartState;

 while CodeUnit<Len do begin

  NextCodeUnit:=CodeUnit;
  CurrentChar:=BRREUTF8PtrCodeUnitGetCharAndInc(Input,Len,NextCodeUnit);

  LastState:=State;

  if CurrentChar<256 then begin
   State:=State.Latin1NextStates[ByteMap[CurrentChar]];
  end else begin
   State:=nil;
   RangePagedNextStates:=State.RangePagedNextStates;
   while assigned(RangePagedNextStates) do begin
    if RangePagedNextStates^.Page=(CurrentChar and $ffffff00) then begin
     State:=RangePagedNextStates^.NextStates[CurrentChar and $ff];
     break;
    end;
    RangePagedNextStates:=RangePagedNextStates^.Next;
   end;
  end;

  if not assigned(State) then begin
   State:=FastProcessNextState(LastState,CurrentChar);
   if not assigned(State) then begin
    result:=brreDFAError;
    break;
   end;
  end;

  if (State.Flags and (brresfDFAStartShifted or brresfDFAMatchWins))<>0 then begin
   if (State.Flags and brresfDFAStartShifted)<>0 then begin
    if result=brreDFAMatch then begin
     // If we have already a valid match, we don't need to reset, but we do need stop the processing
     break;
    end;
    StartCodeUnit:=CodeUnit;
    StartCodePoint:=CodePoint;
   end;
   if (State.Flags and brresfDFAMatchWins)<>0 then begin
    EndCodePoint:=CodePoint+1;
    EndCodeUnit:=NextCodeUnit;
    result:=brreDFAMatch;
   end;
  end;

  CodeUnit:=NextCodeUnit;
  inc(CodePoint);

 end;
end;

function TBRREDFA.PtrSearchFastUTF8FirstChar(const Input:pansichar):longint;
var CodeUnit,NextCodeUnit,Len,CodePoint:longint;
    CurrentChar:longword;
    State,LastState:PBRREDFAState;
    RangePagedNextStates:PBRREDFARangePagedNextStates;
begin

 result:=brreDFAFail;

 CodeUnit:=StartCodeUnit;
 CodePoint:=StartCodePoint;

 NextCodeUnit:=StartCodeUnit;

 Len:=RegExpDataInstance.InputUnitsLength;

 State:=StartState;

 while CodeUnit<Len do begin

  NextCodeUnit:=CodeUnit;
  CurrentChar:=BRREUTF8PtrCodeUnitGetCharAndInc(Input,Len,NextCodeUnit);

  LastState:=State;

  if CurrentChar<256 then begin
   State:=State.Latin1NextStates[ByteMap[CurrentChar]];
  end else begin
   State:=nil;
   RangePagedNextStates:=State.RangePagedNextStates;
   while assigned(RangePagedNextStates) do begin
    if RangePagedNextStates^.Page=(CurrentChar and $ffffff00) then begin
     State:=RangePagedNextStates^.NextStates[CurrentChar and $ff];
     break;
    end;
    RangePagedNextStates:=RangePagedNextStates^.Next;
   end;
  end;

  if not assigned(State) then begin
   State:=FastProcessNextState(LastState,CurrentChar);
   if not assigned(State) then begin
    result:=brreDFAError;
    break;
   end;
  end;

  if (State.Flags and (brresfDFAStart or brresfDFAStartShifted or brresfDFAMatchWins))<>0 then begin
   if (State.Flags and brresfDFAStartShifted)<>0 then begin
    if result=brreDFAMatch then begin
     // If we have already a valid match, we don't need to reset, but we do need stop the processing
     break;
    end;
    StartCodeUnit:=CodeUnit;
    StartCodePoint:=CodePoint;
   end;
   if (State.Flags and brresfDFAMatchWins)<>0 then begin
    EndCodePoint:=CodePoint+1;
    EndCodeUnit:=NextCodeUnit;
    result:=brreDFAMatch;
   end;
   if (State.Flags and brresfDFAStart)<>0 then begin
    inc(CodePoint);
    if PrefixCharRangeLow=PrefixCharRangeHigh then begin
     CodeUnit:=PtrPosCharUTF8(ansichar(byte(PrefixCharRangeLow)),Input,Len,CodePoint,NextCodeUnit);
    end else begin
     CodeUnit:=PtrPosCharRangeUTF8(ansichar(byte(PrefixCharRangeLow)),ansichar(byte(PrefixCharRangeHigh)),Input,Len,CodePoint,NextCodeUnit);
    end;
    if CodeUnit<0 then begin
     break;
    end;
    continue;
   end;
  end;

  CodeUnit:=NextCodeUnit;
  inc(CodePoint);

 end;
end;

procedure TBRREDFA.FullAddInstructionThread(State:PBRREDFAState;Instruction:PBRRERegExpNFAInstruction;Flags:longword);
var StackPointer:longint;
    Stack:PPBRRERegExpNFAInstructionsStatic;
    InstructionGenerationIDs:PBRRERegExpDataInstanceInstructionGenerationIDsStatic;
begin
 Stack:=@StackInstructions[0];
 InstructionGenerationIDs:=@RegExpProgramData^.InstructionGenerationIDs[0];
 StackPointer:=0;
 Stack^[StackPointer]:=Instruction;
 inc(StackPointer);
 while StackPointer>0 do begin
  dec(StackPointer);
  Instruction:=Stack^[StackPointer];
  while assigned(Instruction) and (InstructionGenerationIDs^[Instruction^.IDandOpcode shr 8]<>RegExpProgramData^.GenerationID) do begin
   InstructionGenerationIDs^[Instruction^.IDandOpcode shr 8]:=RegExpProgramData^.GenerationID;
   case Instruction^.IDandOpcode and $ff of
    brreoJMP,brreoBEGIN,brreoEND,brreoPREFIX,brreoPREFIXCHAR,brreoCALLOUT:begin
     Instruction:=Instruction^.x;
    end;
    brreoLOOKAHEADFALSE,brreoLOOKAHEADTRUE,brreoLOOKBEHINDFALSE,brreoLOOKBEHINDTRUE:begin
     Instruction:=Instruction^.y;
    end;
    brreoSPLIT,brreoSPLITLONGEST,brreoTESTBACKREF,brreoTEST:begin
     Stack^[StackPointer]:=Instruction^.y;
     inc(StackPointer);
     Instruction:=Instruction^.x;
    end;
    else begin
     if State.CountInstructions>=length(State.Instructions) then begin
      SetLength(State.Instructions,BRRERoundUpToPowerOfTwo(State.CountInstructions+1));
     end;
     State.Instructions[State.CountInstructions]:=Instruction;
     inc(State.CountInstructions);
     case Instruction^.IDandOpcode and $ff of
      brreoBOML,brreoEOML,brreoBOSL,brreoEOSL,brreoBOS,brreoEOS,brreoBRK,brreoNBRK:begin
       if (Flags and BRREWhichFlags[Instruction^.IDandOpcode and $ff])<>0 then begin
        Instruction:=Instruction^.x;
        continue;
       end;
      end;
      brreoBACKREF,brreoBACKREFI:begin
       Instruction:=Instruction^.x;
       continue;
      end;
      brreoRECURSIVE:begin
       Instruction:=Instruction^.y;
       continue;
      end;
     end;
     break;
    end;
   end;
  end;
 end;
end;

function TBRREDFA.FullGetStartState(LastChar,CurrentChar:longword):PBRREDFAState;
var Flags:longword;
begin
 Flags:=0;
 if (LastChar=$ffffffff) or (LastChar=$000a) or (LastChar=$000d) or (LastChar=$2028) or (LastChar=$2029) then begin
  Flags:=Flags or brresfEmptyBeginLine;
 end;
 if LastChar=$ffffffff then begin
  Flags:=Flags or brresfEmptyBeginText;
 end;
 if RegExp.IsWordChar(LastChar)<>RegExp.IsWordChar(CurrentChar) then begin
  Flags:=Flags or brresfEmptyWordBoundary;
 end else begin
  Flags:=Flags or brresfEmptyNonWordBoundary;
 end;
 if CurrentChar=$ffffffff then begin
  Flags:=Flags or (brresfEmptyEndLine or brresfEmptyEndText);
 end else if (CurrentChar=$000a) or (CurrentChar=$000d) or (CurrentChar=$2028) or (CurrentChar=$2029) then begin
  Flags:=Flags or brresfEmptyEndLine;
 end;
 result:=StartStates[Flags];
end;

function TBRREDFA.FullProcessNextState(State:PBRREDFAState;CurrentChar:longword):PBRREDFAState;
var Flags,NeedFlags,BeforeFlags,OldBeforeFlags,AfterFlags:longword;
    Counter:longint;
    Instruction:PBRRERegExpNFAInstruction;
    BaseState:PBRREDFAState;
    IsWordChar,IsMatch:boolean;
begin

 // Fetch flags
 NeedFlags:=State.Flags shr brresfDFANeedShift;
 BeforeFlags:=State.Flags and brresfEmptyAllFlags;
 OldBeforeFlags:=BeforeFlags;
 AfterFlags:=0;

 // Calculate flags
 if (CurrentChar=$000a) or (CurrentChar=$000d) or (CurrentChar=$2028) or (CurrentChar=$2029) then begin
  BeforeFlags:=BeforeFlags or brresfEmptyEndLine;
  AfterFlags:=AfterFlags or brresfEmptyBeginLine;
 end;
 if CurrentChar=$ffffffff then begin
  BeforeFlags:=BeforeFlags or (brresfEmptyEndLine or brresfEmptyEndText);
  AfterFlags:=AfterFlags or (brresfEmptyBeginLine or brresfEmptyBeginText);
 end;
 IsWordChar:=((CurrentChar<>$ffffffff) and RegExp.IsWordChar(CurrentChar));
 if ((State.Flags and brresfDFALastWord)<>0)=IsWordChar then begin
  BeforeFlags:=BeforeFlags or brresfEmptyNonWordBoundary;
 end else begin
  BeforeFlags:=BeforeFlags or brresfEmptyWordBoundary;
 end;

 // Process empty-string step
 if ((BeforeFlags and not OldBeforeFlags) and NeedFlags)<>0 then begin
  inc(RegExpProgramData^.GenerationID);
  TemporaryState.CountInstructions:=0;
  TemporaryState.Flags:=0;
  for Counter:=0 to State.CountInstructions-1 do begin
   FullAddInstructionThread(@TemporaryState,State.Instructions[Counter],BeforeFlags);
  end;
  BaseState:=@TemporaryState;
 end else begin
  BaseState:=State;
 end;

 // Process charpoint step
 Flags:=AfterFlags;
 IsMatch:=false;
 inc(RegExpProgramData^.GenerationID);
 NewState.CountInstructions:=0;
 NewState.Flags:=0;
 for Counter:=0 To BaseState.CountInstructions-1 do begin
  Instruction:=BaseState.Instructions[Counter];
  case Instruction^.IDandOpcode and $ff of
   brreoJMP,brreoEND,brreoSPLIT,brreoSPLITLONGEST,brreoBEGIN,brreoBOML,brreoEOML,brreoBOSL,brreoEOSL,brreoBOS,brreoEOS,brreoBRK,brreoNBRK,brreoPREFIX,brreoPREFIXCHAR:begin
    continue;
   end;
   brreoCHAR:begin
    if ((CurrentChar<256) and (ansichar(byte(CurrentChar)) in TBRRERegExpCharClass(pointer(Instruction^.Value)).Charset)) or
       ((CurrentChar>255) and TBRRERegExpCharClass(pointer(Instruction^.Value)).Contains(CurrentChar)) then begin
     FullAddInstructionThread(@NewState,Instruction^.x,Flags);
    end;
   end;
   brreoCHARSINGLE:begin
    if CurrentChar=ptruint(Instruction^.Value) then begin
     FullAddInstructionThread(@NewState,Instruction^.x,Flags);
    end;
   end;
   brreoMATCH:begin
    if not (RegExpProgram.AnchorEnd and (CurrentChar<>$ffffffff)) then begin
     IsMatch:=true;
     if (RegExp.Flags and brrefLONGEST)=0 then begin
      // We can stop processing the instruction list queue here since we found a match (for the PCRE leftmost first valid match behaviour)
      break;
     end;
    end;
   end;
   brreoBACKREF,brreoBACKREFI:begin
    if CurrentChar<>$ffffffff then begin
     FullAddInstructionThread(@NewState,Instruction^.x,Flags);
     FullAddInstructionThread(@NewState,Instruction,Flags);
    end;
   end;
   brreoRECURSIVE:begin
    if CurrentChar<>$ffffffff then begin
     FullAddInstructionThread(@NewState,Instruction^.y,Flags);
     FullAddInstructionThread(@NewState,Instruction,Flags);
    end;
   end;
   else begin
    result:=nil;
    exit;
   end;
  end;
 end;

 // Add missed flags
 if IsMatch then begin
  Flags:=Flags or brresfDFAMatchWins;
 end;
 if IsWordChar then begin
  Flags:=Flags or brresfDFALastWord;
 end;

 // Search needed flags for this next state
 NeedFlags:=0;
 for Counter:=0 To NewState.CountInstructions-1 do begin
  Instruction:=NewState.Instructions[Counter];
  case Instruction^.IDandOpcode and $ff of
   brreoBOML,brreoEOML,brreoBOSL,brreoEOSL,brreoBOS,brreoEOS,brreoBRK,brreoNBRK:begin
    NeedFlags:=NeedFlags or BRREWhichFlags[Instruction^.IDandOpcode and $ff];
   end;
  end;
 end;

 // Drop flags if no empty-string instructions are there, to save unneeded cached states
 if NeedFlags=0 then begin
  Flags:=Flags and brresfDFAMatchWins;
 end;

 // Store flags in the new state
 NewState.Flags:=Flags or ((NeedFlags or (State.Flags and brresfDFAStart)) shl brresfDFANeedShift);

 // Dead state? If yes, ...
 if (NewState.CountInstructions=0) and (Flags=0) then begin
  // ... drop it and take the start state as the next state
  result:=StartStates[brresfEmptyAllFlags];
 end else begin
  // .. otherwise try caching it
  result:=CacheState(@NewState);
 end;

 // Connect the last state to the new state with the current char
 ConnectState(State,result,CurrentChar,false);
end;

function TBRREDFA.SearchFullLatin1(const Input:ansistring):longint;
var CodeUnit,Len:longint;
    LastChar,CurrentChar:longword;
    State,LastState:PBRREDFAState;
    RangePagedNextStates:PBRREDFARangePagedNextStates;
begin
 result:=brreDFAFail;

 if RegExpProgram.AnchorBeginText and (StartCodeUnit<>1) then begin
  exit;
 end;

 LastChar:=$ffffffff;

 CodeUnit:=StartCodeUnit;

 Len:=RegExpDataInstance.InputUnitsLength;

 if (CodeUnit>1) and (CodeUnit<=(Len+1)) then begin
  LastChar:=byte(ansichar(Input[CodeUnit-1]));
 end;

 CurrentChar:=LastChar;

 State:=StartStates[brresfEmptyAllFlags];
 repeat

  LastChar:=CurrentChar;
  if CodeUnit<=Len then begin
   CurrentChar:=byte(Input[CodeUnit]);
  end else if CodeUnit=(Len+1) then begin
   CurrentChar:=$ffffffff;
  end else begin
   break;
  end;

  if (State.Flags and brresfDFAStart)<>0 then begin
   if RegExpProgram.AnchorBeginText and (CodeUnit<>1) then begin
    // If we are not on the begin of text with a begin-text-anchor, we don't need to reset, but we do need stop the processing
    break;
   end else begin
    // Otherwise reselect precached start state for the current position
    State:=FullGetStartState(LastChar,CurrentChar);
   end;
  end;

  LastState:=State;

  if (CurrentChar<256) and (ansichar(byte(CurrentChar)) in ByteCharSetMap) then begin
   State:=State.Latin1NextStates[ByteMap[CurrentChar]];
  end else begin
   RangePagedNextStates:=State.RangePagedNextStates;
   State:=nil;
   while assigned(RangePagedNextStates) do begin
    if RangePagedNextStates^.Page=(CurrentChar and $ffffff00) then begin
     State:=RangePagedNextStates^.NextStates[CurrentChar and $ff];
     break;
    end;
    RangePagedNextStates:=RangePagedNextStates^.Next;
   end;
  end;

  if not assigned(State) then begin
   State:=FullProcessNextState(LastState,CurrentChar);
   if not assigned(State) then begin
    result:=brreDFAError;
    break;
   end;
  end;

  if (State.Flags and (brresfDFAStartShifted or brresfDFAMatchWins))<>0 then begin
   if (State.Flags and brresfDFAStartShifted)<>0 then begin
    if result=brreDFAMatch then begin
     // If we have already a valid match, we don't need to reset, but we do need stop the processing
     break;
    end;
    StartCodeUnit:=CodeUnit;
   end;
   if (State.Flags and brresfDFAMatchWins)<>0 then begin
    EndCodeUnit:=CodeUnit;
    result:=brreDFAMatch;
   end;
  end;

  inc(CodeUnit);

 until false;

 StartCodePoint:=StartCodeUnit-1;
 EndCodePoint:=EndCodeUnit-1;

end;

function TBRREDFA.SearchFullLatin1FirstChar(const Input:ansistring):longint;
var CodeUnit,Len:longint;
    LastChar,CurrentChar:longword;
    State,LastState:PBRREDFAState;
    RangePagedNextStates:PBRREDFARangePagedNextStates;
begin
 result:=brreDFAFail;

 if RegExpProgram.AnchorBeginText and (StartCodeUnit<>1) then begin
  exit;
 end;

 LastChar:=$ffffffff;

 CodeUnit:=StartCodeUnit;

 Len:=RegExpDataInstance.InputUnitsLength;

 if (CodeUnit>1) and (CodeUnit<=(Len+1)) then begin
  LastChar:=byte(ansichar(Input[CodeUnit-1]));
 end;

 CurrentChar:=LastChar;

 State:=StartStates[brresfEmptyAllFlags];
 repeat

  LastChar:=CurrentChar;
  if CodeUnit<=Len then begin
   CurrentChar:=byte(Input[CodeUnit]);
  end else if CodeUnit=(Len+1) then begin
   CurrentChar:=$ffffffff;
  end else begin
   break;
  end;

  if (State.Flags and brresfDFAStart)<>0 then begin
   if RegExpProgram.AnchorBeginText and (CodeUnit<>1) then begin
    // If we are not on the begin of text with a begin-text-anchor, we don't need to reset, but we do need stop the processing
    break;
   end else begin
    // Otherwise reselect precached start state for the current position
    State:=FullGetStartState(LastChar,CurrentChar);
   end;
  end;

  LastState:=State;

  if (CurrentChar<256) and (ansichar(byte(CurrentChar)) in ByteCharSetMap) then begin
   State:=State.Latin1NextStates[ByteMap[CurrentChar]];
  end else begin
   RangePagedNextStates:=State.RangePagedNextStates;
   State:=nil;
   while assigned(RangePagedNextStates) do begin
    if RangePagedNextStates^.Page=(CurrentChar and $ffffff00) then begin
     State:=RangePagedNextStates^.NextStates[CurrentChar and $ff];
     break;
    end;
    RangePagedNextStates:=RangePagedNextStates^.Next;
   end;
  end;

  if not assigned(State) then begin
   State:=FullProcessNextState(LastState,CurrentChar);
   if not assigned(State) then begin
    result:=brreDFAError;
    break;
   end;
  end;

  if (State.Flags and (brresfDFAStart or brresfDFAStartShifted or brresfDFAMatchWins))<>0 then begin
   if (State.Flags and brresfDFAStartShifted)<>0 then begin
    if result=brreDFAMatch then begin
     // If we have already a valid match, we don't need to reset, but we do need stop the processing
     break;
    end;
    StartCodeUnit:=CodeUnit;
   end;
   if (State.Flags and brresfDFAMatchWins)<>0 then begin
    EndCodeUnit:=CodeUnit;
    result:=brreDFAMatch;
   end;
   if (State.Flags and brresfDFAStart)<>0 then begin
    if PrefixCharRangeLow=PrefixCharRangeHigh then begin
     CodeUnit:=PtrPosChar(ansichar(byte(ansichar(byte(PrefixCharRangeLow)))),@Input[1],Len,CodeUnit)+1;
    end else begin
     CodeUnit:=PtrPosCharRange(ansichar(byte(PrefixCharRangeLow)),ansichar(byte(PrefixCharRangeHigh)),@Input[1],Len,CodeUnit)+1;
    end;
    if CodeUnit=0 then begin
     break;
    end;
    if (CodeUnit>1) and (CodeUnit<=(Len+1)) then begin
     CurrentChar:=byte(ansichar(Input[CodeUnit-1]));
    end else begin
     CurrentChar:=$ffffffff;
    end;
    continue;
   end;
  end;

  inc(CodeUnit);

 until false;

 StartCodePoint:=StartCodeUnit-1;
 EndCodePoint:=EndCodeUnit-1;

end;

function TBRREDFA.SearchFullUTF8(const Input:ansistring):longint;
var CodeUnit,NextCodeUnit,NextCodePoint,Len,CodePoint:longint;
    LastChar,CurrentChar:longword;
    State,LastState:PBRREDFAState;
    RangePagedNextStates:PBRREDFARangePagedNextStates;
begin
 result:=brreDFAFail;

 if RegExpProgram.AnchorBeginText and (StartCodeUnit<>1) then begin
  exit;
 end;

 LastChar:=$ffffffff;

 CodeUnit:=StartCodeUnit;
 CodePoint:=StartCodePoint;

 NextCodeUnit:=StartCodeUnit;
 NextCodePoint:=StartCodePoint;

 Len:=RegExpDataInstance.InputUnitsLength;

 if (CodeUnit>1) and (CodeUnit<=(Len+1)) then begin
  NextCodeUnit:=CodeUnit;
  BRREUTF8Dec(Input,NextCodeUnit);
  if NextCodeUnit>0 then begin
   LastChar:=BRREUTF8CodeUnitGetChar(Input,NextCodeUnit);
  end;
 end;

 CurrentChar:=LastChar;

 State:=StartStates[brresfEmptyAllFlags];
 repeat

  LastChar:=CurrentChar;
  if CodeUnit<=Len then begin
   NextCodeUnit:=CodeUnit;
   CurrentChar:=BRREUTF8CodeUnitGetCharAndInc(Input,NextCodeUnit);
  end else if CodeUnit=(Len+1) then begin
   CurrentChar:=$ffffffff;
   NextCodeUnit:=CodeUnit+1;
  end else begin
   break;
  end;
  NextCodePoint:=CodePoint+1;

  if (State.Flags and brresfDFAStart)<>0 then begin
   if RegExpProgram.AnchorBeginText and (CodeUnit<>1) then begin
    // If we are not on the begin of text with a begin-text-anchor, we don't need to reset, but we do need stop the processing
    break;
   end else begin
    // Otherwise reselect precached start state for the current position
    State:=FullGetStartState(LastChar,CurrentChar);
   end;
  end;

  LastState:=State;

  if (CurrentChar<256) and (ansichar(byte(CurrentChar)) in ByteCharSetMap) then begin
   State:=State.Latin1NextStates[ByteMap[CurrentChar]];
  end else begin
   RangePagedNextStates:=State.RangePagedNextStates;
   State:=nil;
   while assigned(RangePagedNextStates) do begin
    if RangePagedNextStates^.Page=(CurrentChar and $ffffff00) then begin
     State:=RangePagedNextStates^.NextStates[CurrentChar and $ff];
     break;
    end;
    RangePagedNextStates:=RangePagedNextStates^.Next;
   end;
  end;

  if not assigned(State) then begin
   State:=FullProcessNextState(LastState,CurrentChar);
   if not assigned(State) then begin
    result:=brreDFAError;
    break;
   end;
  end;

  if (State.Flags and (brresfDFAStartShifted or brresfDFAMatchWins))<>0 then begin
   if (State.Flags and brresfDFAStartShifted)<>0 then begin
    if result=brreDFAMatch then begin
     // If we have already a valid match, we don't need to reset, but we do need stop the processing
     break;
    end;
    StartCodeUnit:=CodeUnit;
    StartCodePoint:=CodePoint;
   end;
   if (State.Flags and brresfDFAMatchWins)<>0 then begin
    EndCodeUnit:=CodeUnit;
    EndCodePoint:=CodePoint;
    result:=brreDFAMatch;
   end;
  end;

  CodeUnit:=NextCodeUnit;
  CodePoint:=NextCodePoint;

 until false;

end;

function TBRREDFA.SearchFullUTF8FirstChar(const Input:ansistring):longint;
var CodeUnit,NextCodeUnit,NextCodePoint,Len,CodePoint:longint;
    LastChar,CurrentChar:longword;
    State,LastState:PBRREDFAState;
    RangePagedNextStates:PBRREDFARangePagedNextStates;
begin
 result:=brreDFAFail;

 if RegExpProgram.AnchorBeginText and (StartCodeUnit<>1) then begin
  exit;
 end;

 LastChar:=$ffffffff;

 CodeUnit:=StartCodeUnit;
 CodePoint:=StartCodePoint;

 NextCodeUnit:=StartCodeUnit;
 NextCodePoint:=StartCodePoint;

 Len:=RegExpDataInstance.InputUnitsLength;

 if (CodeUnit>1) and (CodeUnit<=(Len+1)) then begin
  NextCodeUnit:=CodeUnit;
  BRREUTF8Dec(Input,NextCodeUnit);
  if NextCodeUnit>0 then begin
   LastChar:=BRREUTF8CodeUnitGetChar(Input,NextCodeUnit);
  end;
 end;

 CurrentChar:=LastChar;

 State:=StartStates[brresfEmptyAllFlags];
 repeat

  LastChar:=CurrentChar;
  if CodeUnit<=Len then begin
   NextCodeUnit:=CodeUnit;
   CurrentChar:=BRREUTF8CodeUnitGetCharAndInc(Input,NextCodeUnit);
  end else if CodeUnit=(Len+1) then begin
   CurrentChar:=$ffffffff;
   NextCodeUnit:=CodeUnit+1;
  end else begin
   break;
  end;
  NextCodePoint:=CodePoint+1;

  if (State.Flags and brresfDFAStart)<>0 then begin
   if RegExpProgram.AnchorBeginText and (CodeUnit<>1) then begin
    // If we are not on the begin of text with a begin-text-anchor, we don't need to reset, but we do need stop the processing
    break;
   end else begin
    // Otherwise reselect precached start state for the current position
    State:=FullGetStartState(LastChar,CurrentChar);
   end;
  end;

  LastState:=State;

  if (CurrentChar<256) and (ansichar(byte(CurrentChar)) in ByteCharSetMap) then begin
   State:=State.Latin1NextStates[ByteMap[CurrentChar]];
  end else begin
   RangePagedNextStates:=State.RangePagedNextStates;
   State:=nil;
   while assigned(RangePagedNextStates) do begin
    if RangePagedNextStates^.Page=(CurrentChar and $ffffff00) then begin
     State:=RangePagedNextStates^.NextStates[CurrentChar and $ff];
     break;
    end;
    RangePagedNextStates:=RangePagedNextStates^.Next;
   end;
  end;

  if not assigned(State) then begin
   State:=FullProcessNextState(LastState,CurrentChar);
   if not assigned(State) then begin
    result:=brreDFAError;
    break;
   end;
  end;

  if (State.Flags and (brresfDFAStart or brresfDFAStartShifted or brresfDFAMatchWins))<>0 then begin
   if (State.Flags and brresfDFAStartShifted)<>0 then begin
    if result=brreDFAMatch then begin
     // If we have already a valid match, we don't need to reset, but we do need stop the processing
     break;
    end;
    StartCodeUnit:=CodeUnit;
    StartCodePoint:=CodePoint;
   end;
   if (State.Flags and brresfDFAMatchWins)<>0 then begin
    EndCodeUnit:=CodeUnit;
    EndCodePoint:=CodePoint;
    result:=brreDFAMatch;
   end;
   if (State.Flags and brresfDFAStart)<>0 then begin
    CodePoint:=NextCodePoint;
    if PrefixCharRangeLow=PrefixCharRangeHigh then begin
     CodeUnit:=PtrPosCharUTF8(ansichar(byte(PrefixCharRangeLow)),@Input[1],Len,CodePoint,NextCodeUnit-1)+1;
    end else begin
     CodeUnit:=PtrPosCharRangeUTF8(ansichar(byte(PrefixCharRangeLow)),ansichar(byte(PrefixCharRangeHigh)),@Input[1],Len,CodePoint,NextCodeUnit-1)+1;
    end;
    if CodeUnit=0 then begin
     break;
    end;
    CurrentChar:=$ffffffff;
    if (CodeUnit>1) and (CodeUnit<=(Len+1)) then begin
     NextCodeUnit:=CodeUnit;
     BRREUTF8Dec(Input,NextCodeUnit);
     if NextCodeUnit>0 then begin
      CurrentChar:=BRREUTF8CodeUnitGetChar(Input,NextCodeUnit);
     end;
    end;
    continue;
   end;
  end;

  CodeUnit:=NextCodeUnit;
  CodePoint:=NextCodePoint;

 until false;

end;

function TBRREDFA.PtrSearchFullLatin1(const Input:pansichar):longint;
var CodeUnit,Len:longint;
    LastChar,CurrentChar:longword;
    State,LastState:PBRREDFAState;
    RangePagedNextStates:PBRREDFARangePagedNextStates;
begin
 result:=brreDFAFail;

 if RegExpProgram.AnchorBeginText and (StartCodeUnit<>0) then begin
  exit;
 end;

 LastChar:=$ffffffff;

 CodeUnit:=StartCodeUnit;

 Len:=RegExpDataInstance.InputUnitsLength;

 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  LastChar:=byte(ansichar(Input[CodeUnit-1]));
 end;

 CurrentChar:=LastChar;

 State:=StartStates[brresfEmptyAllFlags];
 repeat

  LastChar:=CurrentChar;
  if CodeUnit<Len then begin
   CurrentChar:=byte(Input[CodeUnit]);
  end else if CodeUnit=Len then begin
   CurrentChar:=$ffffffff;
  end else begin
   break;
  end;

  if (State.Flags and brresfDFAStart)<>0 then begin
   if RegExpProgram.AnchorBeginText and (CodeUnit<>0) then begin
    // If we are not on the begin of text with a begin-text-anchor, we don't need to reset, but we do need stop the processing
    break;
   end else begin
    // Otherwise reselect precached start state for the current position
    State:=FullGetStartState(LastChar,CurrentChar);
   end;
  end;

  LastState:=State;

  if (CurrentChar<256) and (ansichar(byte(CurrentChar)) in ByteCharSetMap) then begin
   State:=State.Latin1NextStates[ByteMap[CurrentChar]];
  end else begin
   RangePagedNextStates:=State.RangePagedNextStates;
   State:=nil;
   while assigned(RangePagedNextStates) do begin
    if RangePagedNextStates^.Page=(CurrentChar and $ffffff00) then begin
     State:=RangePagedNextStates^.NextStates[CurrentChar and $ff];
     break;
    end;
    RangePagedNextStates:=RangePagedNextStates^.Next;
   end;
  end;

  if not assigned(State) then begin
   State:=FullProcessNextState(LastState,CurrentChar);
   if not assigned(State) then begin
    result:=brreDFAError;
    break;
   end;
  end;

  if (State.Flags and (brresfDFAStartShifted or brresfDFAMatchWins))<>0 then begin
   if (State.Flags and brresfDFAStartShifted)<>0 then begin
    if result=brreDFAMatch then begin
     // If we have already a valid match, we don't need to reset, but we do need stop the processing
     break;
    end;
    StartCodeUnit:=CodeUnit;
   end;
   if (State.Flags and brresfDFAMatchWins)<>0 then begin
    EndCodeUnit:=CodeUnit;
    result:=brreDFAMatch;
   end;
  end;

  inc(CodeUnit);

 until false;

 StartCodePoint:=StartCodeUnit;
 EndCodePoint:=EndCodeUnit;

end;

function TBRREDFA.PtrSearchFullLatin1FirstChar(const Input:pansichar):longint;
var CodeUnit,Len:longint;
    LastChar,CurrentChar:longword;
    State,LastState:PBRREDFAState;
    RangePagedNextStates:PBRREDFARangePagedNextStates;
begin
 result:=brreDFAFail;

 if RegExpProgram.AnchorBeginText and (StartCodeUnit<>0) then begin
  exit;
 end;

 LastChar:=$ffffffff;

 CodeUnit:=StartCodeUnit;

 Len:=RegExpDataInstance.InputUnitsLength;

 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  LastChar:=byte(ansichar(Input[CodeUnit-1]));
 end;

 CurrentChar:=LastChar;

 State:=StartStates[brresfEmptyAllFlags];
 repeat

  LastChar:=CurrentChar;
  if CodeUnit<Len then begin
   CurrentChar:=byte(Input[CodeUnit]);
  end else if CodeUnit=Len then begin
   CurrentChar:=$ffffffff;
  end else begin
   break;
  end;

  if (State.Flags and brresfDFAStart)<>0 then begin
   if RegExpProgram.AnchorBeginText and (CodeUnit<>0) then begin
    // If we are not on the begin of text with a begin-text-anchor, we don't need to reset, but we do need stop the processing
    break;
   end else begin
    // Otherwise reselect precached start state for the current position
    State:=FullGetStartState(LastChar,CurrentChar);
   end;
  end;

  LastState:=State;

  if (CurrentChar<256) and (ansichar(byte(CurrentChar)) in ByteCharSetMap) then begin
   State:=State.Latin1NextStates[ByteMap[CurrentChar]];
  end else begin
   RangePagedNextStates:=State.RangePagedNextStates;
   State:=nil;
   while assigned(RangePagedNextStates) do begin
    if RangePagedNextStates^.Page=(CurrentChar and $ffffff00) then begin
     State:=RangePagedNextStates^.NextStates[CurrentChar and $ff];
     break;
    end;
    RangePagedNextStates:=RangePagedNextStates^.Next;
   end;
  end;

  if not assigned(State) then begin
   State:=FullProcessNextState(LastState,CurrentChar);
   if not assigned(State) then begin
    result:=brreDFAError;
    break;
   end;
  end;

  if (State.Flags and (brresfDFAStart or brresfDFAStartShifted or brresfDFAMatchWins))<>0 then begin
   if (State.Flags and brresfDFAStartShifted)<>0 then begin
    if result=brreDFAMatch then begin
     // If we have already a valid match, we don't need to reset, but we do need stop the processing
     break;
    end;
    StartCodeUnit:=CodeUnit;
   end;
   if (State.Flags and brresfDFAMatchWins)<>0 then begin
    EndCodeUnit:=CodeUnit;
    result:=brreDFAMatch;
   end;
   if (State.Flags and brresfDFAStart)<>0 then begin
    if PrefixCharRangeLow=PrefixCharRangeHigh then begin
     CodeUnit:=PtrPosChar(ansichar(byte(ansichar(byte(PrefixCharRangeLow)))),Input,Len,CodeUnit+1);
    end else begin
     CodeUnit:=PtrPosCharRange(ansichar(byte(PrefixCharRangeLow)),ansichar(byte(PrefixCharRangeHigh)),Input,Len,CodeUnit+1);
    end;
    if CodeUnit<0 then begin
     break;
    end;
    if (CodeUnit>0) and (CodeUnit<=Len) then begin
     CurrentChar:=byte(ansichar(Input[CodeUnit-1]));
    end else begin
     CurrentChar:=$ffffffff;
    end;
    continue;
   end;
  end;

  inc(CodeUnit);

 until false;

 StartCodePoint:=StartCodeUnit;
 EndCodePoint:=EndCodeUnit;

end;

function TBRREDFA.PtrSearchFullUTF8(const Input:pansichar):longint;
var CodeUnit,NextCodeUnit,NextCodePoint,Len,CodePoint:longint;
    LastChar,CurrentChar:longword;
    State,LastState:PBRREDFAState;
    RangePagedNextStates:PBRREDFARangePagedNextStates;
begin
 result:=brreDFAFail;

 if RegExpProgram.AnchorBeginText and (StartCodeUnit<>0) then begin
  exit;
 end;

 LastChar:=$ffffffff;

 CodeUnit:=StartCodeUnit;
 CodePoint:=StartCodePoint;

 NextCodeUnit:=StartCodeUnit;
 NextCodePoint:=StartCodePoint;

 Len:=RegExpDataInstance.InputUnitsLength;

 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  NextCodeUnit:=CodeUnit;
  BRREUTF8PtrDec(Input,Len,NextCodeUnit);
  if NextCodeUnit>=0 then begin
   LastChar:=BRREUTF8PtrCodeUnitGetChar(Input,Len,NextCodeUnit);
  end;
 end;

 CurrentChar:=LastChar;

 State:=StartStates[brresfEmptyAllFlags];
 repeat

  LastChar:=CurrentChar;
  if CodeUnit<Len then begin
   NextCodeUnit:=CodeUnit;
   CurrentChar:=BRREUTF8PtrCodeUnitGetCharAndInc(Input,Len,NextCodeUnit);
  end else if CodeUnit=Len then begin
   CurrentChar:=$ffffffff;
   NextCodeUnit:=CodeUnit+1;
  end else begin
   break;
  end;
  NextCodePoint:=CodePoint+1;

  if (State.Flags and brresfDFAStart)<>0 then begin
   if RegExpProgram.AnchorBeginText and (CodeUnit<>0) then begin
    // If we are not on the begin of text with a begin-text-anchor, we don't need to reset, but we do need stop the processing
    break;
   end else begin
    // Otherwise reselect precached start state for the current position
    State:=FullGetStartState(LastChar,CurrentChar);
   end;
  end;

  LastState:=State;

  if (CurrentChar<256) and (ansichar(byte(CurrentChar)) in ByteCharSetMap) then begin
   State:=State.Latin1NextStates[ByteMap[CurrentChar]];
  end else begin
   RangePagedNextStates:=State.RangePagedNextStates;
   State:=nil;
   while assigned(RangePagedNextStates) do begin
    if RangePagedNextStates^.Page=(CurrentChar and $ffffff00) then begin
     State:=RangePagedNextStates^.NextStates[CurrentChar and $ff];
     break;
    end;
    RangePagedNextStates:=RangePagedNextStates^.Next;
   end;
  end;

  if not assigned(State) then begin
   State:=FullProcessNextState(LastState,CurrentChar);
   if not assigned(State) then begin
    result:=brreDFAError;
    break;
   end;
  end;

  if (State.Flags and (brresfDFAStartShifted or brresfDFAMatchWins))<>0 then begin
   if (State.Flags and brresfDFAStartShifted)<>0 then begin
    if result=brreDFAMatch then begin
     // If we have already a valid match, we don't need to reset, but we do need stop the processing
     break;
    end;
    StartCodeUnit:=CodeUnit;
    StartCodePoint:=CodePoint;
   end;
   if (State.Flags and brresfDFAMatchWins)<>0 then begin
    EndCodeUnit:=CodeUnit;
    EndCodePoint:=CodePoint;
    result:=brreDFAMatch;
   end;
  end;

  CodeUnit:=NextCodeUnit;
  CodePoint:=NextCodePoint;

 until false;

end;

function TBRREDFA.PtrSearchFullUTF8FirstChar(const Input:pansichar):longint;
var CodeUnit,NextCodeUnit,NextCodePoint,Len,CodePoint:longint;
    LastChar,CurrentChar:longword;
    State,LastState:PBRREDFAState;
    RangePagedNextStates:PBRREDFARangePagedNextStates;
begin
 result:=brreDFAFail;

 if RegExpProgram.AnchorBeginText and (StartCodeUnit<>0) then begin
  exit;
 end;

 LastChar:=$ffffffff;

 CodeUnit:=StartCodeUnit;
 CodePoint:=StartCodePoint;

 NextCodeUnit:=StartCodeUnit;
 NextCodePoint:=StartCodePoint;

 Len:=RegExpDataInstance.InputUnitsLength;

 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  NextCodeUnit:=CodeUnit;
  BRREUTF8PtrDec(Input,Len,NextCodeUnit);
  if NextCodeUnit>=0 then begin
   LastChar:=BRREUTF8PtrCodeUnitGetChar(Input,Len,NextCodeUnit);
  end;
 end;

 CurrentChar:=LastChar;

 State:=StartStates[brresfEmptyAllFlags];
 repeat

  LastChar:=CurrentChar;
  if CodeUnit<Len then begin
   NextCodeUnit:=CodeUnit;
   CurrentChar:=BRREUTF8PtrCodeUnitGetCharAndInc(Input,Len,NextCodeUnit);
  end else if CodeUnit=Len then begin
   CurrentChar:=$ffffffff;
   NextCodeUnit:=CodeUnit+1;
  end else begin
   break;
  end;
  NextCodePoint:=CodePoint+1;

  if (State.Flags and brresfDFAStart)<>0 then begin
   if RegExpProgram.AnchorBeginText and (CodeUnit<>0) then begin
    // If we are not on the begin of text with a begin-text-anchor, we don't need to reset, but we do need stop the processing
    break;
   end else begin
    // Otherwise reselect precached start state for the current position
    State:=FullGetStartState(LastChar,CurrentChar);
   end;
  end;

  LastState:=State;

  if (CurrentChar<256) and (ansichar(byte(CurrentChar)) in ByteCharSetMap) then begin
   State:=State.Latin1NextStates[ByteMap[CurrentChar]];
  end else begin
   RangePagedNextStates:=State.RangePagedNextStates;
   State:=nil;
   while assigned(RangePagedNextStates) do begin
    if RangePagedNextStates^.Page=(CurrentChar and $ffffff00) then begin
     State:=RangePagedNextStates^.NextStates[CurrentChar and $ff];
     break;
    end;
    RangePagedNextStates:=RangePagedNextStates^.Next;
   end;
  end;

  if not assigned(State) then begin
   State:=FullProcessNextState(LastState,CurrentChar);
   if not assigned(State) then begin
    result:=brreDFAError;
    break;
   end;
  end;

  if (State.Flags and (brresfDFAStart or brresfDFAStartShifted or brresfDFAMatchWins))<>0 then begin
   if (State.Flags and brresfDFAStartShifted)<>0 then begin
    if result=brreDFAMatch then begin
     // If we have already a valid match, we don't need to reset, but we do need stop the processing
     break;
    end;
    StartCodeUnit:=CodeUnit;
    StartCodePoint:=CodePoint;
   end;
   if (State.Flags and brresfDFAMatchWins)<>0 then begin
    EndCodeUnit:=CodeUnit;
    EndCodePoint:=CodePoint;
    result:=brreDFAMatch;
   end;
   if (State.Flags and brresfDFAStart)<>0 then begin
    CodePoint:=NextCodePoint;
    if PrefixCharRangeLow=PrefixCharRangeHigh then begin
     CodeUnit:=PtrPosCharUTF8(ansichar(byte(PrefixCharRangeLow)),Input,Len,CodePoint,NextCodeUnit);
    end else begin
     CodeUnit:=PtrPosCharRangeUTF8(ansichar(byte(PrefixCharRangeLow)),ansichar(byte(PrefixCharRangeHigh)),Input,Len,CodePoint,NextCodeUnit);
    end;
    if CodeUnit<0 then begin
     break;
    end;
    CurrentChar:=$ffffffff;
    if (CodeUnit>0) and (CodeUnit<=Len) then begin
     NextCodeUnit:=CodeUnit;
     BRREUTF8PtrDec(Input,Len,NextCodeUnit);
     if NextCodeUnit>=0 then begin
      CurrentChar:=BRREUTF8PtrCodeUnitGetChar(Input,Len,NextCodeUnit);
     end;
    end;
    continue;
   end;
  end;

  CodeUnit:=NextCodeUnit;
  CodePoint:=NextCodePoint;

 until false;

end;

function TBRREDFA.Search(const Input:ansistring;var AStartCodePoint,AStartCodeUnit,AEndCodePoint,AEndCodeUnit:longint):longint;
var HasFirstChar:boolean;
begin
 StartCodePoint:=AStartCodePoint;
 StartCodeUnit:=AStartCodeUnit;
 EndCodePoint:=AEndCodePoint;
 EndCodeUnit:=AEndCodeUnit;

 HasFirstChar:=assigned(RegExp.AllFirstCharsClass.First) and
              (((RegExpDataInstance.InputIsUTF8 and (RegExp.AllFirstCharsClass.First.Lo<128)) or
               ((not RegExpDataInstance.InputIsUTF8) and (RegExp.AllFirstCharsClass.First.Lo<256))));
 if HasFirstChar then begin
  PrefixCharRangeLow:=RegExp.AllFirstCharsClass.First.Lo;
  PrefixCharRangeHigh:=RegExp.AllFirstCharsClass.Last.Hi;
  if RegExpDataInstance.InputIsUTF8 and (PrefixCharRangeHigh>=128) then begin
   PrefixCharRangeHigh:=255;
  end;
 end;

 case RegExpProgram.DFAMode of
  brredmFAST:begin
   if RegExpDataInstance.InputIsUTF8 then begin
    if HasFirstChar then begin
     result:=SearchFastUTF8FirstChar(Input);
    end else begin
     result:=SearchFastUTF8(Input);
    end;
   end else begin
    if HasFirstChar and (PrefixCharRangeLow=PrefixCharRangeHigh) then begin
     result:=SearchFastLatin1FirstChar(Input);
    end else begin
     result:=SearchFastLatin1(Input);
    end;
   end;
  end;
  brredmFULL:begin
   if RegExpDataInstance.InputIsUTF8 then begin
    if HasFirstChar then begin
     result:=SearchFullUTF8FirstChar(Input);
    end else begin
     result:=SearchFullUTF8(Input);
    end;
   end else begin
    if HasFirstChar then begin
     result:=SearchFullLatin1FirstChar(Input);
    end else begin
     result:=SearchFullLatin1(Input);
    end;
   end;
  end;
  else begin
   result:=brreDFAError;
  end;
 end;
 AStartCodePoint:=StartCodePoint;
 AStartCodeUnit:=StartCodeUnit;
 AEndCodePoint:=EndCodePoint;
 AEndCodeUnit:=EndCodeUnit;
end;

function TBRREDFA.PtrSearch(const Input:pansichar;var AStartCodePoint,AStartCodeUnit,AEndCodePoint,AEndCodeUnit:longint):longint;
var HasFirstChar:boolean;
begin
 StartCodePoint:=AStartCodePoint;
 StartCodeUnit:=AStartCodeUnit;
 EndCodePoint:=AEndCodePoint;
 EndCodeUnit:=AEndCodeUnit;

 HasFirstChar:=assigned(RegExp.AllFirstCharsClass.First) and
              (((RegExpDataInstance.InputIsUTF8 and (RegExp.AllFirstCharsClass.First.Lo<128)) or
               ((not RegExpDataInstance.InputIsUTF8) and (RegExp.AllFirstCharsClass.First.Lo<256))));
 if HasFirstChar then begin
  PrefixCharRangeLow:=RegExp.AllFirstCharsClass.First.Lo;
  PrefixCharRangeHigh:=RegExp.AllFirstCharsClass.Last.Hi;
  if RegExpDataInstance.InputIsUTF8 and (PrefixCharRangeHigh>=128) then begin
   PrefixCharRangeHigh:=255;
  end;
 end;

 case RegExpProgram.DFAMode of
  brredmFAST:begin
   if RegExpDataInstance.InputIsUTF8 then begin
    if HasFirstChar then begin
     result:=PtrSearchFastUTF8FirstChar(Input);
    end else begin
     result:=PtrSearchFastUTF8(Input);
    end;
   end else begin
    if HasFirstChar and (PrefixCharRangeLow=PrefixCharRangeHigh) then begin
     result:=PtrSearchFastLatin1FirstChar(Input);
    end else begin
     result:=PtrSearchFastLatin1(Input);
    end;
   end;
  end;
  brredmFULL:begin
   if RegExpDataInstance.InputIsUTF8 then begin
    if HasFirstChar then begin
     result:=PtrSearchFullUTF8FirstChar(Input);
    end else begin
     result:=PtrSearchFullUTF8(Input);
    end;
   end else begin
    if HasFirstChar then begin
     result:=PtrSearchFullLatin1FirstChar(Input);
    end else begin
     result:=PtrSearchFullLatin1(Input);
    end;
   end;
  end;
  else begin
   result:=brreDFAError;
  end;
 end;
 AStartCodePoint:=StartCodePoint;
 AStartCodeUnit:=StartCodeUnit;
 AEndCodePoint:=EndCodePoint;
 AEndCodeUnit:=EndCodeUnit;
end;

constructor TBRREFixedStringSearch.Create(ARegExpProgram:TBRRERegExpProgram;ARegExpDataInstance:TBRRERegExpDataInstance;ARegExpProgramData:PBRRERegExpProgramDataInstance);
begin
 inherited Create;
 RegExp:=ARegExpProgram.RegExp;
 RegExpProgram:=ARegExpProgram;
 RegExpDataInstance:=ARegExpDataInstance;
 RegExpProgramData:=ARegExpProgramData;
end;

destructor TBRREFixedStringSearch.Destroy;
begin
 inherited Destroy;
end;

function TBRREFixedStringSearch.Search(const Input:ansistring;var CodePoint,CodeUnit:longint):boolean;
begin
 if RegExpProgram.IsGlobal then begin
  if ((RegExp.Flags and (brrefUTF8 or brrefUTF8CODEUNITS))=brrefUTF8) and RegExpDataInstance.InputIsUTF8 then begin
   case RegExpProgram.FixedStringSearchMethod of
    brrefssmSHIFTOR:begin
     CodeUnit:=PosShiftOrUTF8(RegExpProgram.FixedString,Input,RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,CodePoint,CodeUnit);
    end;
{   brrefssmBOYERMOORE:begin
     CodeUnit:=PosBoyerMooreUTF8(RegExpProgram.FixedString,Input,RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,RegExpProgram.FixedStringBoyerMooreNext,CodePoint,CodeUnit);
    end;}
    else {brrefssmBRUTEFORCE:}begin
     if length(RegExpProgram.FixedString)=1 then begin
      CodeUnit:=PosCharUTF8(RegExpProgram.FixedString[1],Input,CodePoint,CodeUnit);
     end else begin
      CodeUnit:=PosExUTF8(RegExpProgram.FixedString,Input,CodePoint,CodeUnit);
     end;
    end;
   end;
   if CodeUnit>0 then begin
    RegExpDataInstance.Captures[0].StartCodeUnit:=CodeUnit;
    RegExpDataInstance.Captures[0].StartCodePoint:=CodePoint;
    RegExpDataInstance.Captures[0].EndCodeUnit:=RegExpDataInstance.Captures[0].StartCodeUnit+RegExpProgram.FixedStringLength;
    RegExpDataInstance.Captures[0].EndCodePoint:=RegExpDataInstance.Captures[0].StartCodePoint+RegExpProgram.FixedStringUTF8Length;
    result:=true;
    CodePoint:=RegExpDataInstance.Captures[0].EndCodePoint;
    CodeUnit:=RegExpDataInstance.Captures[0].EndCodeUnit;
   end else begin
    result:=false;
   end;
  end else begin
   case RegExpProgram.FixedStringSearchMethod of
    brrefssmSHIFTOR:begin
     CodeUnit:=PosShiftOr(RegExpProgram.FixedString,Input,RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,CodeUnit);
    end;
    brrefssmBOYERMOORE:begin
     CodeUnit:=PosBoyerMoore(RegExpProgram.FixedString,Input,RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,RegExpProgram.FixedStringBoyerMooreNext,CodeUnit);
    end;
    else {brrefssmBRUTEFORCE:}begin
     if length(RegExpProgram.FixedString)=1 then begin
      CodeUnit:=PosChar(RegExpProgram.FixedString[1],Input,CodeUnit);
     end else begin
      CodeUnit:=PosEx(RegExpProgram.FixedString,Input,CodeUnit);
     end;
    end;
   end;
   if CodeUnit>0 then begin
    RegExpDataInstance.Captures[0].StartCodeUnit:=CodeUnit;
    if RegExpDataInstance.InputIsUTF8 then begin
     RegExpDataInstance.Captures[0].StartCodePoint:=BRREUTF8GetCodePoint(Input,CodeUnit);
    end else begin
     RegExpDataInstance.Captures[0].StartCodePoint:=RegExpDataInstance.Captures[0].StartCodeUnit-1;
    end;
    RegExpDataInstance.Captures[0].EndCodeUnit:=RegExpDataInstance.Captures[0].StartCodeUnit+RegExpProgram.FixedStringLength;
    if RegExp.RegExpIsUTF8<>brresuNOUTF8 then begin
     RegExpDataInstance.Captures[0].EndCodePoint:=RegExpDataInstance.Captures[0].StartCodePoint+RegExpProgram.FixedStringUTF8Length;
    end else begin
     RegExpDataInstance.Captures[0].EndCodePoint:=RegExpDataInstance.Captures[0].EndCodeUnit-1;
    end;
    result:=true;
    CodePoint:=RegExpDataInstance.Captures[0].EndCodePoint;
    CodeUnit:=RegExpDataInstance.Captures[0].EndCodeUnit;
   end else begin
    result:=false;
    CodeUnit:=RegExpDataInstance.InputUnitsLength;
    if RegExp.RegExpIsUTF8<>brresuNOUTF8 then begin
     CodePoint:=RegExpDataInstance.InputPointsLength;
    end else begin
     CodePoint:=CodeUnit;
    end;
   end;
  end;
 end else begin
  if CompareEx(RegExpProgram.FixedString,Input,CodeUnit) then begin
   RegExpDataInstance.Captures[0].StartCodeUnit:=CodeUnit;
   if RegExpDataInstance.InputIsUTF8 then begin
    RegExpDataInstance.Captures[0].StartCodePoint:=BRREUTF8GetCodePoint(Input,CodeUnit);
   end else begin
    RegExpDataInstance.Captures[0].StartCodePoint:=RegExpDataInstance.Captures[0].StartCodeUnit-1;
   end;
   RegExpDataInstance.Captures[0].EndCodeUnit:=RegExpDataInstance.Captures[0].StartCodeUnit+length(RegExpProgram.FixedString);
   if RegExp.RegExpIsUTF8<>brresuNOUTF8 then begin
    RegExpDataInstance.Captures[0].EndCodePoint:=RegExpDataInstance.Captures[0].StartCodePoint+BRREUTF8Length(RegExpProgram.FixedString);
   end else begin
    RegExpDataInstance.Captures[0].EndCodePoint:=RegExpDataInstance.Captures[0].EndCodeUnit-1;
   end;
   result:=true;
  end else begin
   result:=false;
  end;
  if RegExp.RegExpIsUTF8<>brresuNOUTF8 then begin
   CodePoint:=CodeUnit-1;
  end;
 end;
end;

function TBRREFixedStringSearch.PtrSearch(const Input:pansichar;var CodePoint,CodeUnit:longint):boolean;
begin
 if RegExpProgram.IsGlobal then begin
  if ((RegExp.Flags and (brrefUTF8 or brrefUTF8CODEUNITS))=brrefUTF8) and RegExpDataInstance.InputIsUTF8 then begin
   case RegExpProgram.FixedStringSearchMethod of
    brrefssmSHIFTOR:begin
     CodeUnit:=PtrPosShiftOrUTF8(RegExpProgram.FixedString,Input,RegExpDataInstance.InputUnitsLength,RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,CodePoint,CodeUnit);
    end;
{   brrefssmBOYERMOORE:begin
     CodeUnit:=PtrPosBoyerMooreUTF8(RegExpProgram.FixedString,Input,RegExpDataInstance.InputUnitsLength,RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,RegExpProgram.FixedStringBoyerMooreNext,CodePoint,CodeUnit);
    end;}
    else {brrefssmBRUTEFORCE:}begin
     if length(RegExpProgram.FixedString)=1 then begin
      CodeUnit:=PtrPosCharUTF8(RegExpProgram.FixedString[1],Input,RegExpDataInstance.InputUnitsLength,CodePoint,CodeUnit);
     end else begin
      CodeUnit:=PtrPosExUTF8(RegExpProgram.FixedString,Input,RegExpDataInstance.InputUnitsLength,CodePoint,CodeUnit);
     end;
    end;
   end;
   if CodeUnit>=0 then begin
    RegExpDataInstance.Captures[0].StartCodeUnit:=CodeUnit;
    RegExpDataInstance.Captures[0].StartCodePoint:=CodePoint;
    RegExpDataInstance.Captures[0].EndCodeUnit:=RegExpDataInstance.Captures[0].StartCodeUnit+RegExpProgram.FixedStringLength;
    RegExpDataInstance.Captures[0].EndCodePoint:=RegExpDataInstance.Captures[0].StartCodePoint+RegExpProgram.FixedStringUTF8Length;
    result:=true;
    CodePoint:=RegExpDataInstance.Captures[0].EndCodePoint;
    CodeUnit:=RegExpDataInstance.Captures[0].EndCodeUnit;
   end else begin
    result:=false;
   end;
  end else begin
   case RegExpProgram.FixedStringSearchMethod of
    brrefssmSHIFTOR:begin
     CodeUnit:=PtrPosShiftOr(RegExpProgram.FixedString,Input,RegExpDataInstance.InputUnitsLength,RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,CodeUnit);
    end;
    brrefssmBOYERMOORE:begin
     CodeUnit:=PtrPosBoyerMoore(RegExpProgram.FixedString,Input,RegExpDataInstance.InputUnitsLength,RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,RegExpProgram.FixedStringBoyerMooreNext,CodeUnit);
    end;
    else {brrefssmBRUTEFORCE:}begin
     if length(RegExpProgram.FixedString)=1 then begin
      CodeUnit:=PtrPosChar(RegExpProgram.FixedString[1],Input,RegExpDataInstance.InputUnitsLength,CodeUnit);
     end else begin
      CodeUnit:=PtrPosEx(RegExpProgram.FixedString,Input,RegExpDataInstance.InputUnitsLength,CodeUnit);
     end;
    end;
   end;
   if CodeUnit>=0 then begin
    RegExpDataInstance.Captures[0].StartCodeUnit:=CodeUnit;
    if RegExpDataInstance.InputIsUTF8 then begin
     RegExpDataInstance.Captures[0].StartCodePoint:=BRREUTF8PtrGetCodePoint(Input,RegExpDataInstance.InputUnitsLength,CodeUnit);
    end else begin
     RegExpDataInstance.Captures[0].StartCodePoint:=RegExpDataInstance.Captures[0].StartCodeUnit;
    end;
    RegExpDataInstance.Captures[0].EndCodeUnit:=RegExpDataInstance.Captures[0].StartCodeUnit+RegExpProgram.FixedStringLength;
    if RegExp.RegExpIsUTF8<>brresuNOUTF8 then begin
     RegExpDataInstance.Captures[0].EndCodePoint:=RegExpDataInstance.Captures[0].StartCodePoint+RegExpProgram.FixedStringUTF8Length;
    end else begin
     RegExpDataInstance.Captures[0].EndCodePoint:=RegExpDataInstance.Captures[0].EndCodeUnit;
    end;
    result:=true;
    CodePoint:=RegExpDataInstance.Captures[0].EndCodePoint;
    CodeUnit:=RegExpDataInstance.Captures[0].EndCodeUnit;
   end else begin
    result:=false;
    CodeUnit:=RegExpDataInstance.InputUnitsLength;
    if RegExp.RegExpIsUTF8<>brresuNOUTF8 then begin
     CodePoint:=RegExpDataInstance.InputPointsLength;
    end else begin
     CodePoint:=CodeUnit;
    end;
   end;
  end;
 end else begin
  if CompareEx(RegExpProgram.FixedString,Input,CodeUnit) then begin
   RegExpDataInstance.Captures[0].StartCodeUnit:=CodeUnit;
   if RegExpDataInstance.InputIsUTF8 then begin
    RegExpDataInstance.Captures[0].StartCodePoint:=BRREUTF8PtrGetCodePoint(Input,RegExpDataInstance.InputUnitsLength,CodeUnit);
   end else begin
    RegExpDataInstance.Captures[0].StartCodePoint:=RegExpDataInstance.Captures[0].StartCodeUnit;
   end;
   RegExpDataInstance.Captures[0].EndCodeUnit:=RegExpDataInstance.Captures[0].StartCodeUnit+length(RegExpProgram.FixedString);
   if RegExp.RegExpIsUTF8<>brresuNOUTF8 then begin
    RegExpDataInstance.Captures[0].EndCodePoint:=RegExpDataInstance.Captures[0].StartCodePoint+BRREUTF8Length(RegExpProgram.FixedString);
   end else begin
    RegExpDataInstance.Captures[0].EndCodePoint:=RegExpDataInstance.Captures[0].EndCodeUnit;
   end;
   result:=true;
  end else begin
   result:=false;
  end;
  if RegExp.RegExpIsUTF8<>brresuNOUTF8 then begin
   CodePoint:=CodeUnit;
  end;
 end;
end;

constructor TBRREOnePassNFA.Create(ARegExpProgram:TBRRERegExpProgram;ARegExpDataInstance:TBRRERegExpDataInstance;ARegExpProgramData:PBRRERegExpProgramDataInstance);
begin
 inherited Create;
 RegExp:=ARegExpProgram.RegExp;
 RegExpProgram:=ARegExpProgram;
 RegExpDataInstance:=ARegExpDataInstance;
 RegExpProgramData:=ARegExpProgramData;
 WorkCaptures:=nil;
 MatchCaptures:=nil;
end;

destructor TBRREOnePassNFA.Destroy;
begin
 SetLength(WorkCaptures,0);
 SetLength(MatchCaptures,0);
 inherited Destroy;
end;
                          
procedure TBRREOnePassNFA.CopyWorkCapturesToMatchCaptures(Count:longint); {$IFDEF caninline}inline;{$ENDIF}
var Counter:longint;
begin
 for Counter:=0 to Count-1 do begin
  MatchCaptures[Counter]:=WorkCaptures[Counter];
 end;
end;

function TBRREOnePassNFA.SearchLatin1(const Input:ansistring;var CodePoint,CodeUnit:longint):boolean;
var State,Nodes:PBRREOnePassState;
    StateSize,Len,TwoCountOfCaptures:longint;
    ByteMap:PBRREByteMap;
    Done:boolean;
    NextMatchCondition,MatchCondition,Condition,NextIndex:longword;
 function Satisfy(Condition:longword):boolean;
 var Flags:longword;
 begin
  Flags:=0;
  if CodeUnit<=1 then begin
   Flags:=Flags or (brresfEmptyBeginText or brresfEmptyBeginLine);
  end else if Input[CodeUnit-1] in [#10,#13] then begin
   Flags:=Flags or brresfEmptyBeginLine;
  end;
  if CodeUnit>=Len then begin
   Flags:=Flags or (brresfEmptyEndText or brresfEmptyEndLine);
  end else if (CodeUnit<=Len) and (Input[CodeUnit] in [#10,#13]) then begin
   Flags:=Flags or brresfEmptyEndLine;
  end;
  if Len>0 then begin
   if CodeUnit=1 then begin
    if RegExp.IsWordChar(byte(Input[1])) then begin
     Flags:=Flags or brresfEmptyWordBoundary;
    end;
   end else if CodeUnit>Len then begin
    if RegExp.IsWordChar(byte(Input[CodeUnit-1])) then begin
     Flags:=Flags or brresfEmptyWordBoundary;
    end;
   end else if RegExp.IsWordChar(byte(Input[CodeUnit-1]))<>RegExp.IsWordChar(byte(Input[CodeUnit])) then begin
    Flags:=Flags or brresfEmptyWordBoundary;
   end;
  end;
  if (Flags and brresfEmptyWordBoundary)=0 then begin
   Flags:=Flags or brresfEmptyNonWordBoundary;
  end;
  result:=((Condition and brresfEmptyAllFlags) and not Flags)=0;
 end;
 procedure ApplyCaptures(Condition:longword;var DestCaptures:TBRRERegExpOnePassCaptures);
 var i:longint;
 begin
  for i:=0 to TwoCountOfCaptures-1 do begin
   if (Condition and ((1 shl brresfCapShift) shl i))<>0 then begin
    DestCaptures[i]:=CodeUnit;
   end;
  end;
 end;
 procedure CopyCaptures;
 var i,j:longint;
 begin
  j:=RegExpProgram.FirstCapture;
  for i:=0 to RegExpProgram.CountOfCaptures-1 do begin
   RegExpDataInstance.Captures[j].StartCodePoint:=MatchCaptures[i*2]-1;
   RegExpDataInstance.Captures[j].EndCodePoint:=MatchCaptures[(i*2)+1]-1;
   RegExpDataInstance.Captures[j].StartCodeUnit:=MatchCaptures[i*2];
   RegExpDataInstance.Captures[j].EndCodeUnit:=MatchCaptures[(i*2)+1];
   inc(j);
  end;
 end;
begin
 TwoCountOfCaptures:=RegExpProgram.CountOfCaptures*2;

 RegExpProgramData:=@RegExpDataInstance.RegExpPrograms[RegExpProgram.ID];

 if length(WorkCaptures)<TwoCountOfCaptures then begin
  SetLength(WorkCaptures,TwoCountOfCaptures);
 end;
 if length(MatchCaptures)<TwoCountOfCaptures then begin
  SetLength(MatchCaptures,TwoCountOfCaptures);
 end;

 State:=RegExpProgram.OnePassStart;
 Nodes:=RegExpProgram.OnePassNodes;
 StateSize:=RegExpProgram.OnePassStateSize;
 ByteMap:=@RegExpProgram.ByteMap;

 result:=false;
 Done:=false;

 if RegExpProgram.AnchorBeginText and (CodeUnit<>1) then begin
  exit;
 end;

 NextMatchCondition:=State^.MatchCondition;
 StartCodeUnit:=CodeUnit;
 EndCodeUnit:=CodeUnit;
 Condition:=0;
 Len:=RegExpDataInstance.InputUnitsLength;
 while CodeUnit<=Len do begin
  Condition:=State^.Action[ByteMap^[byte(ansichar(Input[CodeUnit]))]];
  MatchCondition:=NextMatchCondition;

  if ((Condition and brresfEmptyAllFlags)=0) or Satisfy(Condition) then begin
   NextIndex:=Condition shr brresfIndexShift;
   State:=pointer(@pansichar(Nodes)[StateSize*longint(NextIndex)]);
   NextMatchCondition:=State^.MatchCondition;
  end else begin
   State:=nil;
   NextMatchCondition:=brresfImpossible;
  end;

  if (MatchCondition<>brresfImpossible) and
     (((Condition and brresfMatchWins)<>0) or ((NextMatchCondition and brresfEmptyAllFlags)<>0)) and
     (((MatchCondition and brresfEmptyAllFlags)=0) or Satisfy(MatchCondition)) then begin
   CopyWorkCapturesToMatchCaptures(TwoCountOfCaptures);
   if (MatchCondition and brresfCapMask)<>0 then begin
    ApplyCaptures(Condition,MatchCaptures);
   end;
   EndCodeUnit:=CodeUnit;
   result:=true;
   if ((Condition and brresfMatchWins)<>0) and ((RegExp.Flags and brrefLONGEST)=0) then begin
    Done:=true;
    break;
   end;
  end;

  if not assigned(State) then begin
   Done:=true;
   break;
  end;

  if (Condition and brresfCapMask)<>0 then begin
   ApplyCaptures(Condition,WorkCaptures);
  end;

  inc(CodeUnit);
 end;
 CodePoint:=CodeUnit-1;
 if assigned(State) and not Done then begin
  MatchCondition:=State^.MatchCondition;
  if (MatchCondition<>brresfImpossible) and (((MatchCondition and brresfEmptyAllFlags)=0) or Satisfy(MatchCondition)) then begin
   if ((MatchCondition and brresfCapMask)<>0) and (TwoCountOfCaptures>0) then begin
    ApplyCaptures(MatchCondition,WorkCaptures);
   end; 
   CopyWorkCapturesToMatchCaptures(TwoCountOfCaptures);
   EndCodeUnit:=CodeUnit;
   result:=true;
  end;
 end;
 if result then begin
  CopyCaptures;
 end;
 if RegExpProgram.HasRootCapture then begin
  RegExpDataInstance.Captures[0].StartCodePoint:=StartCodeUnit-1;
  RegExpDataInstance.Captures[0].EndCodePoint:=EndCodeUnit-1;
  RegExpDataInstance.Captures[0].StartCodeUnit:=StartCodeUnit;
  RegExpDataInstance.Captures[0].EndCodeUnit:=EndCodeUnit;
 end;
end;

function TBRREOnePassNFA.SearchUTF8(const Input:ansistring;var CodePoint,CodeUnit:longint):boolean;
var State,Nodes:PBRREOnePassState;
    StateSize,Len,TwoCountOfCaptures,NextCodeUnit:longint;
    ByteMap:PBRREByteMap;
    Done:boolean;
    NextMatchCondition,MatchCondition,Condition,NextIndex,CurrentChar,LastChar:longword;
 function Satisfy(Condition:longword):boolean;
 var Flags:longword;
 begin
  Flags:=0;
  if CodeUnit<=1 then begin
   Flags:=Flags or (brresfEmptyBeginText or brresfEmptyBeginLine);
  end else begin
   case LastChar of
    $000a,$00d,$2028,$2029:begin
     Flags:=Flags or brresfEmptyBeginLine;
    end;
   end;
  end;
  if NextCodeUnit>=Len then begin
   Flags:=Flags or (brresfEmptyEndText or brresfEmptyEndLine);
  end else begin
   case CurrentChar of
    $000a,$00d,$2028,$2029:begin
     Flags:=Flags or brresfEmptyEndLine;
    end;
   end;
  end;
  if RegExp.IsWordChar(LastChar)<>RegExp.IsWordChar(CurrentChar) then begin
   Flags:=Flags or brresfEmptyWordBoundary;
  end else begin
   Flags:=Flags or brresfEmptyNonWordBoundary;
  end;
  result:=((Condition and brresfEmptyAllFlags) and not Flags)=0;
 end;
 procedure ApplyCaptures(Condition:longword;var DestCaptures:TBRRERegExpOnePassCaptures);
 var i:longint;
 begin
  for i:=0 to TwoCountOfCaptures-1 do begin
   if (Condition and ((1 shl brresfCapShift) shl i))<>0 then begin
    DestCaptures[i*2]:=CodeUnit;
    DestCaptures[(i*2)+1]:=CodePoint;
   end;
  end;
 end;
 procedure CopyCaptures;
 var i,j:longint;
 begin
  j:=RegExpProgram.FirstCapture;
  for i:=0 to RegExpProgram.CountOfCaptures-1 do begin
   RegExpDataInstance.Captures[j].StartCodeUnit:=MatchCaptures[(i*4)+0];
   RegExpDataInstance.Captures[j].StartCodePoint:=MatchCaptures[(i*4)+1];
   RegExpDataInstance.Captures[j].EndCodeUnit:=MatchCaptures[(i*4)+2];
   RegExpDataInstance.Captures[j].EndCodePoint:=MatchCaptures[(i*4)+3];
   inc(j);
  end;
 end;
var CharClassAction:PBRREOnePassStateCharClassAction;
begin
 TwoCountOfCaptures:=RegExpProgram.CountOfCaptures*2;

 RegExpProgramData:=@RegExpDataInstance.RegExpPrograms[RegExpProgram.ID];

 if length(WorkCaptures)<(TwoCountOfCaptures*2) then begin
  SetLength(WorkCaptures,TwoCountOfCaptures*2);
 end;
 if length(MatchCaptures)<(TwoCountOfCaptures*2) then begin
  SetLength(MatchCaptures,TwoCountOfCaptures*2);
 end;

 State:=RegExpProgram.OnePassStart;
 Nodes:=RegExpProgram.OnePassNodes;
 StateSize:=RegExpProgram.OnePassStateSize;
 ByteMap:=@RegExpProgram.ByteMap;

 result:=false;
 Done:=false;

 if RegExpProgram.AnchorBeginText and (CodeUnit<>1) then begin
  exit;
 end;

 CurrentChar:=$ffffffff;
 if (CodeUnit>1) and (CodeUnit<=RegExpDataInstance.InputUnitsLength) then begin
  if RegExpDataInstance.InputIsUTF8 then begin
   NextCodeUnit:=CodeUnit;
   BRREUTF8Dec(Input,NextCodeUnit);
   if NextCodeUnit>0 then begin
    CurrentChar:=BRREUTF8CodeUnitGetChar(Input,NextCodeUnit);
   end;
  end else begin
   CurrentChar:=byte(ansichar(Input[CodeUnit-1]));
  end;
 end;

 NextMatchCondition:=State^.MatchCondition;
 StartCodeUnit:=CodeUnit;
 EndCodeUnit:=CodeUnit;
 StartCodePoint:=CodePoint;
 EndCodePoint:=CodePoint;
 Condition:=0;
 Len:=RegExpDataInstance.InputUnitsLength;
 while CodeUnit<=Len do begin

  NextCodeUnit:=CodeUnit;

  LastChar:=CurrentChar;

  CurrentChar:=BRREUTF8CodeUnitGetCharAndInc(Input,NextCodeUnit);

  if CurrentChar<256 then begin
   Condition:=State^.Action[ByteMap^[CurrentChar]];
  end else begin
   Condition:=State^.NoAction;
   CharClassAction:=State^.CharClassAction;
   while assigned(CharClassAction) do begin
    if CharClassAction^.CharClass.Contains(CurrentChar) then begin
     Condition:=CharClassAction^.Condition;
     break;
    end;
    CharClassAction:=CharClassAction^.Next;
   end;
  end;

  MatchCondition:=NextMatchCondition;

  if ((Condition and brresfEmptyAllFlags)=0) or Satisfy(Condition) then begin
   NextIndex:=Condition shr brresfIndexShift;
   State:=pointer(@pansichar(Nodes)[StateSize*longint(NextIndex)]);
   NextMatchCondition:=State^.MatchCondition;
  end else begin
   State:=nil;
   NextMatchCondition:=brresfImpossible;
  end;

  if (MatchCondition<>brresfImpossible) and
     (((Condition and brresfMatchWins)<>0) or ((NextMatchCondition and brresfEmptyAllFlags)<>0)) and
     (((MatchCondition and brresfEmptyAllFlags)=0) or Satisfy(MatchCondition)) then begin
   CopyWorkCapturesToMatchCaptures(TwoCountOfCaptures*2);
   if (MatchCondition and brresfCapMask)<>0 then begin
    ApplyCaptures(Condition,MatchCaptures);
   end;
   EndCodeUnit:=CodeUnit;
   EndCodePoint:=CodePoint;
   result:=true;
   if ((Condition and brresfMatchWins)<>0) and ((RegExp.Flags and brrefLONGEST)=0) then begin
    Done:=true;
    break;
   end;
  end;

  if not assigned(State) then begin
   Done:=true;
   break;
  end;

  if (Condition and brresfCapMask)<>0 then begin
   ApplyCaptures(Condition,WorkCaptures);
  end;

  CodeUnit:=NextCodeUnit;
  inc(CodePoint);
 end;
 if assigned(State) and not Done then begin
  MatchCondition:=State^.MatchCondition;
  LastChar:=CurrentChar;
  CurrentChar:=$ffffffff;
  if (MatchCondition<>brresfImpossible) and (((MatchCondition and brresfEmptyAllFlags)=0) or Satisfy(MatchCondition)) then begin
   if ((MatchCondition and brresfCapMask)<>0) and (TwoCountOfCaptures>0) then begin
    ApplyCaptures(MatchCondition,WorkCaptures);
   end;
   CopyWorkCapturesToMatchCaptures(TwoCountOfCaptures*2);
   EndCodeUnit:=CodeUnit;
   EndCodePoint:=CodePoint;
   result:=true;
  end;
 end;
 if result then begin
  CopyCaptures;
 end;
 if RegExpProgram.HasRootCapture then begin
  RegExpDataInstance.Captures[0].StartCodePoint:=StartCodePoint;
  RegExpDataInstance.Captures[0].EndCodePoint:=EndCodePoint;
  RegExpDataInstance.Captures[0].StartCodeUnit:=StartCodeUnit;
  RegExpDataInstance.Captures[0].EndCodeUnit:=EndCodeUnit;
 end;
end;

function TBRREOnePassNFA.Search(const Input:ansistring;var CodePoint,CodeUnit:longint):boolean;
begin
 if RegExpDataInstance.InputIsUTF8 then begin
  result:=SearchUTF8(Input,CodePoint,CodeUnit);
 end else begin
  result:=SearchLatin1(Input,CodePoint,CodeUnit);
 end;
end;

function TBRREOnePassNFA.PtrSearchLatin1(const Input:pansichar;var CodePoint,CodeUnit:longint):boolean;
var State,Nodes:PBRREOnePassState;
    StateSize,Len,TwoCountOfCaptures:longint;
    ByteMap:PBRREByteMap;
    Done:boolean;
    NextMatchCondition,MatchCondition,Condition,NextIndex:longword;
 function Satisfy(Condition:longword):boolean;
 var Flags:longword;
 begin
  Flags:=0;
  if CodeUnit<=0 then begin
   Flags:=Flags or (brresfEmptyBeginText or brresfEmptyBeginLine);
  end else if Input[CodeUnit-1] in [#10,#13] then begin
   Flags:=Flags or brresfEmptyBeginLine;
  end;
  if CodeUnit>Len then begin
   Flags:=Flags or (brresfEmptyEndText or brresfEmptyEndLine);
  end else if (CodeUnit<Len) and (Input[CodeUnit] in [#10,#13]) then begin
   Flags:=Flags or brresfEmptyEndLine;
  end;
  if Len>0 then begin
   if CodeUnit=0 then begin
    if RegExp.IsWordChar(byte(Input[0])) then begin
     Flags:=Flags or brresfEmptyWordBoundary;
    end;
   end else if CodeUnit>=Len then begin
    if RegExp.IsWordChar(byte(Input[CodeUnit-1])) then begin
     Flags:=Flags or brresfEmptyWordBoundary;
    end;
   end else if RegExp.IsWordChar(byte(Input[CodeUnit-1]))<>RegExp.IsWordChar(byte(Input[CodeUnit])) then begin
    Flags:=Flags or brresfEmptyWordBoundary;
   end;
  end;
  if (Flags and brresfEmptyWordBoundary)=0 then begin
   Flags:=Flags or brresfEmptyNonWordBoundary;
  end;
  result:=((Condition and brresfEmptyAllFlags) and not Flags)=0;
 end;
 procedure ApplyCaptures(Condition:longword;var DestCaptures:TBRRERegExpOnePassCaptures);
 var i:longint;
 begin
  for i:=0 to TwoCountOfCaptures-1 do begin
   if (Condition and ((1 shl brresfCapShift) shl i))<>0 then begin
    DestCaptures[i]:=CodeUnit;
   end;
  end;
 end;
 procedure CopyCaptures;
 var i,j:longint;
 begin
  j:=RegExpProgram.FirstCapture;
  for i:=0 to RegExpProgram.CountOfCaptures-1 do begin
   RegExpDataInstance.Captures[j].StartCodePoint:=MatchCaptures[i*2];
   RegExpDataInstance.Captures[j].EndCodePoint:=MatchCaptures[(i*2)+1];
   RegExpDataInstance.Captures[j].StartCodeUnit:=MatchCaptures[i*2];
   RegExpDataInstance.Captures[j].EndCodeUnit:=MatchCaptures[(i*2)+1];
   inc(j);
  end;
 end;
begin
 TwoCountOfCaptures:=RegExpProgram.CountOfCaptures*2;

 RegExpProgramData:=@RegExpDataInstance.RegExpPrograms[RegExpProgram.ID];

 if length(WorkCaptures)<TwoCountOfCaptures then begin
  SetLength(WorkCaptures,TwoCountOfCaptures);
 end;
 if length(MatchCaptures)<TwoCountOfCaptures then begin
  SetLength(MatchCaptures,TwoCountOfCaptures);
 end;

 State:=RegExpProgram.OnePassStart;
 Nodes:=RegExpProgram.OnePassNodes;
 StateSize:=RegExpProgram.OnePassStateSize;
 ByteMap:=@RegExpProgram.ByteMap;

 result:=false;
 Done:=false;

 if RegExpProgram.AnchorBeginText and (CodeUnit<>0) then begin
  exit;
 end;

 NextMatchCondition:=State^.MatchCondition;
 StartCodeUnit:=CodeUnit;
 EndCodeUnit:=CodeUnit;
 Condition:=0;
 Len:=RegExpDataInstance.InputUnitsLength;
 while CodeUnit<Len do begin
  Condition:=State^.Action[ByteMap^[byte(ansichar(Input[CodeUnit]))]];
  MatchCondition:=NextMatchCondition;

  if ((Condition and brresfEmptyAllFlags)=0) or Satisfy(Condition) then begin
   NextIndex:=Condition shr brresfIndexShift;
   State:=pointer(@pansichar(Nodes)[StateSize*longint(NextIndex)]);
   NextMatchCondition:=State^.MatchCondition;
  end else begin
   State:=nil;
   NextMatchCondition:=brresfImpossible;
  end;

  if (MatchCondition<>brresfImpossible) and
     (((Condition and brresfMatchWins)<>0) or ((NextMatchCondition and brresfEmptyAllFlags)<>0)) and
     (((MatchCondition and brresfEmptyAllFlags)=0) or Satisfy(MatchCondition)) then begin
   CopyWorkCapturesToMatchCaptures(TwoCountOfCaptures);
   if (MatchCondition and brresfCapMask)<>0 then begin
    ApplyCaptures(Condition,MatchCaptures);
   end;
   EndCodeUnit:=CodeUnit;
   result:=true;
   if ((Condition and brresfMatchWins)<>0) and ((RegExp.Flags and brrefLONGEST)=0) then begin
    Done:=true;
    break;
   end;
  end;

  if not assigned(State) then begin
   Done:=true;
   break;
  end;

  if (Condition and brresfCapMask)<>0 then begin
   ApplyCaptures(Condition,WorkCaptures);
  end;

  inc(CodeUnit);
 end;
 CodePoint:=CodeUnit-1;
 if assigned(State) and not Done then begin
  MatchCondition:=State^.MatchCondition;
  if (MatchCondition<>brresfImpossible) and (((MatchCondition and brresfEmptyAllFlags)=0) or Satisfy(MatchCondition)) then begin
   if ((MatchCondition and brresfCapMask)<>0) and (TwoCountOfCaptures>0) then begin
    ApplyCaptures(MatchCondition,WorkCaptures);
   end;
   CopyWorkCapturesToMatchCaptures(TwoCountOfCaptures);
   EndCodeUnit:=CodeUnit;
   result:=true;
  end;
 end;
 if result then begin
  CopyCaptures;
 end;
 if RegExpProgram.HasRootCapture then begin
  RegExpDataInstance.Captures[0].StartCodePoint:=StartCodeUnit-1;
  RegExpDataInstance.Captures[0].EndCodePoint:=EndCodeUnit-1;
  RegExpDataInstance.Captures[0].StartCodeUnit:=StartCodeUnit;
  RegExpDataInstance.Captures[0].EndCodeUnit:=EndCodeUnit;
 end;
end;

function TBRREOnePassNFA.PtrSearchUTF8(const Input:pansichar;var CodePoint,CodeUnit:longint):boolean;
var State,Nodes:PBRREOnePassState;
    StateSize,Len,TwoCountOfCaptures,NextCodeUnit:longint;
    ByteMap:PBRREByteMap;
    Done:boolean;
    NextMatchCondition,MatchCondition,Condition,NextIndex,CurrentChar,LastChar:longword;
 function Satisfy(Condition:longword):boolean;
 var Flags:longword;
 begin
  Flags:=0;
  if CodeUnit<=0 then begin
   Flags:=Flags or (brresfEmptyBeginText or brresfEmptyBeginLine);
  end else begin
   case LastChar of
    $000a,$00d,$2028,$2029:begin
     Flags:=Flags or brresfEmptyBeginLine;
    end;
   end;
  end;
  if NextCodeUnit>Len then begin
   Flags:=Flags or (brresfEmptyEndText or brresfEmptyEndLine);
  end else begin
   case CurrentChar of
    $000a,$00d,$2028,$2029:begin
     Flags:=Flags or brresfEmptyEndLine;
    end;
   end;
  end;
  if RegExp.IsWordChar(LastChar)<>RegExp.IsWordChar(CurrentChar) then begin
   Flags:=Flags or brresfEmptyWordBoundary;
  end else begin
   Flags:=Flags or brresfEmptyNonWordBoundary;
  end;
  result:=((Condition and brresfEmptyAllFlags) and not Flags)=0;
 end;
 procedure ApplyCaptures(Condition:longword;var DestCaptures:TBRRERegExpOnePassCaptures);
 var i:longint;
 begin
  for i:=0 to TwoCountOfCaptures-1 do begin
   if (Condition and ((1 shl brresfCapShift) shl i))<>0 then begin
    DestCaptures[i*2]:=CodeUnit;
    DestCaptures[(i*2)+1]:=CodePoint;
   end;
  end;
 end;
 procedure CopyCaptures;
 var i,j:longint;
 begin
  j:=RegExpProgram.FirstCapture;
  for i:=0 to RegExpProgram.CountOfCaptures-1 do begin
   RegExpDataInstance.Captures[j].StartCodeUnit:=MatchCaptures[(i*4)+0];
   RegExpDataInstance.Captures[j].StartCodePoint:=MatchCaptures[(i*4)+1];
   RegExpDataInstance.Captures[j].EndCodeUnit:=MatchCaptures[(i*4)+2];
   RegExpDataInstance.Captures[j].EndCodePoint:=MatchCaptures[(i*4)+3];
   inc(j);
  end;
 end;
var CharClassAction:PBRREOnePassStateCharClassAction;
begin
 TwoCountOfCaptures:=RegExpProgram.CountOfCaptures*2;

 RegExpProgramData:=@RegExpDataInstance.RegExpPrograms[RegExpProgram.ID];

 if length(WorkCaptures)<(TwoCountOfCaptures*2) then begin
  SetLength(WorkCaptures,TwoCountOfCaptures*2);
 end;
 if length(MatchCaptures)<(TwoCountOfCaptures*2) then begin
  SetLength(MatchCaptures,TwoCountOfCaptures*2);
 end;

 State:=RegExpProgram.OnePassStart;
 Nodes:=RegExpProgram.OnePassNodes;
 StateSize:=RegExpProgram.OnePassStateSize;
 ByteMap:=@RegExpProgram.ByteMap;

 result:=false;
 Done:=false;

 if RegExpProgram.AnchorBeginText and (CodeUnit<>0) then begin
  exit;
 end;

 CurrentChar:=$ffffffff;
 if (CodeUnit>0) and (CodeUnit<RegExpDataInstance.InputUnitsLength) then begin
  if RegExpDataInstance.InputIsUTF8 then begin
   NextCodeUnit:=CodeUnit;
   BRREUTF8PtrDec(Input,RegExpDataInstance.InputUnitsLength,NextCodeUnit);
   if NextCodeUnit>=0 then begin
    CurrentChar:=BRREUTF8PtrCodeUnitGetChar(Input,RegExpDataInstance.InputUnitsLength,NextCodeUnit);
   end;
  end else begin
   CurrentChar:=byte(ansichar(Input[CodeUnit-1]));
  end;
 end;

 NextMatchCondition:=State^.MatchCondition;
 StartCodeUnit:=CodeUnit;
 EndCodeUnit:=CodeUnit;
 StartCodePoint:=CodePoint;
 EndCodePoint:=CodePoint;
 Condition:=0;
 Len:=RegExpDataInstance.InputUnitsLength;
 while CodeUnit<Len do begin

  NextCodeUnit:=CodeUnit;

  LastChar:=CurrentChar;

  CurrentChar:=BRREUTF8PtrCodeUnitGetCharAndInc(Input,RegExpDataInstance.InputUnitsLength,NextCodeUnit);

  if CurrentChar<256 then begin
   Condition:=State^.Action[ByteMap^[CurrentChar]];
  end else begin
   Condition:=State^.NoAction;
   CharClassAction:=State^.CharClassAction;
   while assigned(CharClassAction) do begin
    if CharClassAction^.CharClass.Contains(CurrentChar) then begin
     Condition:=CharClassAction^.Condition;
     break;
    end;
    CharClassAction:=CharClassAction^.Next;
   end;
  end;

  MatchCondition:=NextMatchCondition;

  if ((Condition and brresfEmptyAllFlags)=0) or Satisfy(Condition) then begin
   NextIndex:=Condition shr brresfIndexShift;
   State:=pointer(@pansichar(Nodes)[StateSize*longint(NextIndex)]);
   NextMatchCondition:=State^.MatchCondition;
  end else begin
   State:=nil;
   NextMatchCondition:=brresfImpossible;
  end;

  if (MatchCondition<>brresfImpossible) and
     (((Condition and brresfMatchWins)<>0) or ((NextMatchCondition and brresfEmptyAllFlags)<>0)) and
     (((MatchCondition and brresfEmptyAllFlags)=0) or Satisfy(MatchCondition)) then begin
   CopyWorkCapturesToMatchCaptures(TwoCountOfCaptures*2);
   if (MatchCondition and brresfCapMask)<>0 then begin
    ApplyCaptures(Condition,MatchCaptures);
   end;
   StartCodePoint:=CodePoint;
   EndCodePoint:=CodePoint;
   result:=true;
   if ((Condition and brresfMatchWins)<>0) and ((RegExp.Flags and brrefLONGEST)=0) then begin
    Done:=true;
    break;
   end;
  end;

  if not assigned(State) then begin
   Done:=true;
   break;
  end;

  if (Condition and brresfCapMask)<>0 then begin
   ApplyCaptures(Condition,WorkCaptures);
  end;

  CodeUnit:=NextCodeUnit;
  inc(CodePoint);
 end;
 if assigned(State) and not Done then begin
  MatchCondition:=State^.MatchCondition;
  LastChar:=CurrentChar;
  CurrentChar:=$ffffffff;
  if (MatchCondition<>brresfImpossible) and (((MatchCondition and brresfEmptyAllFlags)=0) or Satisfy(MatchCondition)) then begin
   if ((MatchCondition and brresfCapMask)<>0) and (TwoCountOfCaptures>0) then begin
    ApplyCaptures(MatchCondition,WorkCaptures);
   end;
   CopyWorkCapturesToMatchCaptures(TwoCountOfCaptures*2);
   StartCodePoint:=CodePoint;
   EndCodePoint:=CodePoint;
   result:=true;
  end;
 end;
 if result then begin
  CopyCaptures;
 end;
 if RegExpProgram.HasRootCapture then begin
  RegExpDataInstance.Captures[0].StartCodePoint:=StartCodePoint;
  RegExpDataInstance.Captures[0].EndCodePoint:=EndCodePoint;
  RegExpDataInstance.Captures[0].StartCodeUnit:=StartCodeUnit;
  RegExpDataInstance.Captures[0].EndCodeUnit:=EndCodeUnit;
 end;
end;

function TBRREOnePassNFA.PtrSearch(const Input:pansichar;var CodePoint,CodeUnit:longint):boolean;
begin
 if RegExpDataInstance.InputIsUTF8 then begin
  result:=PtrSearchUTF8(Input,CodePoint,CodeUnit);
 end else begin
  result:=PtrSearchLatin1(Input,CodePoint,CodeUnit);
 end;
end;

constructor TBRREBitstateNFA.Create(ARegExpProgram:TBRRERegExpProgram;ARegExpDataInstance:TBRRERegExpDataInstance);
begin
 inherited Create;
 RegExp:=ARegExpProgram.RegExp;
 RegExpProgram:=ARegExpProgram;
 RegExpDataInstance:=ARegExpDataInstance;
 RegExpProgramData:=@RegExpDataInstance.RegExpPrograms[RegExpProgram.ID];
 Visited:=nil;
 CountVisited:=0;
 Jobs:=nil;
 CountJobs:=0;
 MaxJob:=0;
 Len:=0;
 BaseCodeUnit:=0;
end;

destructor TBRREBitstateNFA.Destroy;
begin
 SetLength(Visited,0);
 SetLength(Jobs,0);
 inherited Destroy;
end;

function TBRREBitstateNFA.ShouldVisit(Instruction:PBRRERegExpNFAInstruction;CodeUnit:longint):boolean;
var i:longword;
begin
 i:=(ptruint(Instruction^.IDandOpcode shr 8)*longword(Len+1))+longword(longint(CodeUnit-BaseCodeUnit));
 result:=(Visited[i shr 5] and (1 shl (i and 31)))=0;
 if result then begin
  Visited[i shr 5]:=Visited[i shr 5] or (1 shl (i and 31));
 end;
end;

procedure TBRREBitstateNFA.Push(Instruction:PBRRERegExpNFAInstruction;CodePoint,CodeUnit,Argument:longint);
var Job:PBRREBitstateNFAJob;
begin
 if assigned(Instruction) and ((Instruction^.IDandOpcode and $ff)<>brreoFAIL) and not ((Argument=0) and not ShouldVisit(Instruction,CodeUnit)) then begin
  if CountJobs>=length(Jobs) then begin
   SetLength(Jobs,BRRERoundUpToPowerOfTwo(CountJobs+1));
  end;
  Job:=@Jobs[CountJobs];
  inc(CountJobs);
  Job^.Instruction:=Instruction;
  Job^.CodeUnit:=CodeUnit;
  Job^.CodePoint:=CodePoint;
  Job^.Argument:=Argument;
 end;
end;

function TBRREBitstateNFA.Search(const Input:ansistring;var CodePoint,CodeUnit:longint;ALen,ABaseCodeUnit,EndCodeUnit:longint):boolean;
 function TrySearch(StartInstruction:PBRRERegExpNFAInstruction;var CodePoint,CodeUnit:longint):boolean;
 var Job:PBRREBitstateNFAJob;
     Instruction:PBRRERegExpNFAInstruction;
     Argument,i,LastCodePoint:longint;
     CurrentChar,Flags:longword;
     InputIsUTF8:boolean;
  function Satisfy(NextFlags:longword;CodeUnit:longint):boolean;
  var Flags,LastChar,CurrentChar,NextChar:longword;
      Index,NextCodeUnit:longint;
  begin
   LastChar:=$ffffffff;
   CurrentChar:=$ffffffff;
   NextChar:=$ffffffff;
   if RegExpDataInstance.InputIsUTF8 then begin
    if CodeUnit>0 then begin
     Index:=CodeUnit;
     BRREUTF8Dec(Input,Index);
     if Index>0 then begin
      LastChar:=BRREUTF8CodeUnitGetChar(Input,Index);
     end;
     NextCodeUnit:=CodeUnit;
     CurrentChar:=BRREUTF8CodeUnitGetCharAndInc(Input,NextCodeUnit);
     if NextCodeUnit<=RegExpDataInstance.InputUnitsLength then begin
      NextChar:=BRREUTF8CodeUnitGetChar(Input,CodeUnit);
     end;
    end;
   end else begin
    if CodeUnit>0 then begin
     if CodeUnit>1 then begin
      LastChar:=byte(ansichar(Input[CodeUnit-1]));
     end;
     CurrentChar:=byte(ansichar(Input[CodeUnit]));
     NextCodeUnit:=CodeUnit+1;
     if NextCodeUnit<=RegExpDataInstance.InputUnitsLength then begin
      NextChar:=byte(ansichar(Input[NextCodeUnit]));
     end;
    end;
   end;
   Flags:=0;
   if CodeUnit<=1 then begin
    Flags:=Flags or (brresfEmptyBeginText or brresfEmptyBeginLine);
   end else begin
    case LastChar of
     $000a,$00d,$2028,$2029:begin
      Flags:=Flags or brresfEmptyBeginLine;
     end;
    end;
   end;
   if CodeUnit>RegExpDataInstance.InputUnitsLength then begin
    Flags:=Flags or (brresfEmptyEndText or brresfEmptyEndLine);
   end else begin
    case CurrentChar of
     $000a,$00d,$2028,$2029:begin
      Flags:=Flags or brresfEmptyEndLine;
     end;
    end;
   end;
   if RegExpDataInstance.InputUnitsLength>0 then begin
    if CodeUnit=1 then begin
     if RegExp.IsWordChar(CurrentChar) then begin
      Flags:=Flags or brresfEmptyWordBoundary;
     end;
    end else if CodeUnit>RegExpDataInstance.InputUnitsLength then begin
     if RegExp.IsWordChar(byte(LastChar)) then begin
      Flags:=Flags or brresfEmptyWordBoundary;
     end;
    end else if RegExp.IsWordChar(LastChar)<>RegExp.IsWordChar(CurrentChar) then begin
     Flags:=Flags or brresfEmptyWordBoundary;
    end;
   end;
   if (Flags and brresfEmptyWordBoundary)=0 then begin
    Flags:=Flags or brresfEmptyNonWordBoundary;
   end;
   if NextChar<>0 then begin
   end;
   result:=((NextFlags and brresfEmptyAllFlags) and not Flags)=0;
  end;
 begin
  result:=false;

  InputIsUTF8:=RegExpDataInstance.InputIsUTF8;

  LastCodePoint:=-1;

  CountJobs:=0;
  Push(StartInstruction,CodePoint,CodeUnit,0);

  while CountJobs>0 do begin
   dec(CountJobs);
   Job:=@Jobs[CountJobs];

   Instruction:=Job^.Instruction;
   CodePoint:=Job^.CodePoint;
   CodeUnit:=Job^.CodeUnit;
   Argument:=Job^.Argument;

   repeat

    case Instruction^.IDandOpcode and $ff of
     brreoFAIL:begin
      result:=false;
      exit;
     end;
     brreoSPLIT,brreoSPLITLONGEST:begin
      case Argument of
       0:begin
        Push(Instruction,CodePoint,CodeUnit,1);
        Instruction:=Instruction^.x;
        if ShouldVisit(Instruction,CodeUnit) then begin
         continue;
        end;
       end;
       1:begin
        Argument:=0;
        Instruction:=Instruction^.y;
        if ShouldVisit(Instruction,CodeUnit) then begin
         continue;
        end;
       end;
      end;
     end;
     brreoCHAR:begin
      if CodeUnit<=EndCodeUnit then begin
       if InputIsUTF8 then begin
        CurrentChar:=BRREUTF8CodeUnitGetCharAndInc(Input,CodeUnit);
       end else begin
        CurrentChar:=byte(ansichar(Input[CodeUnit]));
        inc(CodeUnit);
       end;
       inc(CodePoint);
      end else begin
       CurrentChar:=$ffffffff;
      end;
      if (CurrentChar<>$ffffffff) and
         (((CurrentChar<256) and (ansichar(byte(CurrentChar)) in TBRRERegExpCharClass(pointer(Instruction^.Value)).Charset)) or
          ((CurrentChar>255) and TBRRERegExpCharClass(pointer(Instruction^.Value)).Contains(CurrentChar))) then begin
       Instruction:=Instruction^.x;
       if ShouldVisit(Instruction,CodeUnit) then begin
        continue;
       end;
      end;
     end;
     brreoCHARSINGLE:begin
      if CodeUnit<=EndCodeUnit then begin
       if InputIsUTF8 then begin
        CurrentChar:=BRREUTF8CodeUnitGetCharAndInc(Input,CodeUnit);
       end else begin
        CurrentChar:=byte(ansichar(Input[CodeUnit]));
        inc(CodeUnit);
       end;
       inc(CodePoint);
      end else begin
       CurrentChar:=$ffffffff;
      end;
      if CurrentChar=ptruint(Instruction^.Value) then begin
       Instruction:=Instruction^.x;
       if ShouldVisit(Instruction,CodeUnit) then begin
        continue;
       end;
      end;
     end;
     brreoANY:begin
      if CodeUnit<=RegExpDataInstance.InputUnitsLength then begin
       if InputIsUTF8 then begin
        CurrentChar:=BRREUTF8CodeUnitGetCharAndInc(Input,CodeUnit);
       end else begin
        CurrentChar:=byte(ansichar(Input[CodeUnit]));
        inc(CodeUnit);
       end;
       inc(CodePoint);
       Instruction:=Instruction^.x;
       if ShouldVisit(Instruction,CodeUnit) then begin
        continue;
       end;
      end else begin
       CurrentChar:=$ffffffff;
      end;
     end;
     brreoMATCH:begin
      result:=true;
      if (RegExp.Flags and brrefLONGEST)<>0 then begin
       if LastCodePoint<CodePoint then begin
        LastCodePoint:=CodePoint;
        for i:=RegExpProgram.FirstCapture to RegExpProgram.FirstCapture+(RegExpProgram.CountOfCaptures-1) do begin
         RegExpDataInstance.Captures[i]:=RegExpDataInstance.WorkCaptures[i];
        end;
       end;
      end else begin
       for i:=RegExpProgram.FirstCapture to RegExpProgram.FirstCapture+(RegExpProgram.CountOfCaptures-1) do begin
        RegExpDataInstance.Captures[i]:=RegExpDataInstance.WorkCaptures[i];
       end;
       exit;
      end;
     end;
     brreoNONE,brreoJMP,brreoPREFIX,brreoPREFIXCHAR:begin
      Instruction:=Instruction^.x;
      if ShouldVisit(Instruction,CodeUnit) then begin
       continue;
      end;
     end;
     brreoBEGIN:begin
      case Argument of
       0:begin
        Push(Instruction,RegExpDataInstance.WorkCaptures[Instruction^.Value].StartCodePoint,RegExpDataInstance.WorkCaptures[Instruction^.Value].StartCodeUnit,1);
        RegExpDataInstance.WorkCaptures[Instruction^.Value].StartCodePoint:=CodePoint;
        RegExpDataInstance.WorkCaptures[Instruction^.Value].StartCodeUnit:=CodeUnit;
        Instruction:=Instruction^.x;
        if ShouldVisit(Instruction,CodeUnit) then begin
         continue;
        end;
       end;
       1:begin
        RegExpDataInstance.WorkCaptures[Instruction^.Value].StartCodePoint:=CodePoint;
        RegExpDataInstance.WorkCaptures[Instruction^.Value].StartCodeUnit:=CodeUnit;
       end;
      end;
     end;
     brreoEND:begin
      case Argument of
       0:begin
        Push(Instruction,RegExpDataInstance.WorkCaptures[Instruction^.Value].EndCodePoint,RegExpDataInstance.WorkCaptures[Instruction^.Value].EndCodeUnit,1);
        RegExpDataInstance.WorkCaptures[Instruction^.Value].EndCodePoint:=CodePoint;
        RegExpDataInstance.WorkCaptures[Instruction^.Value].EndCodeUnit:=CodeUnit;
        Instruction:=Instruction^.x;
        if ShouldVisit(Instruction,CodeUnit) then begin
         continue;
        end;
       end;
       1:begin
        RegExpDataInstance.WorkCaptures[Instruction^.Value].EndCodePoint:=CodePoint;
        RegExpDataInstance.WorkCaptures[Instruction^.Value].EndCodeUnit:=CodeUnit;
       end;
      end;
     end;
     brreoBOML,brreoEOML,brreoBOSL,brreoEOSL,brreoBOS,brreoEOS,brreoBRK,brreoNBRK:begin
      Flags:=BRREWhichFlags[Instruction^.IDandOpcode and $ff];
      if (Flags=0) or Satisfy(Flags,CodeUnit) then begin
       Instruction:=Instruction^.x;
       if ShouldVisit(Instruction,CodeUnit) then begin
        continue;
       end;
      end;
     end;
    end;

    break;
   until false;

  end;

 end;
var l,LenInput,LastCodeUnit,LastCodePoint:longint;
    State:longword;
begin
 result:=false;
 if ALen>0 then begin

  Len:=ALen;
  BaseCodeUnit:=ABaseCodeUnit;

  l:=(((Len+1)*length(RegExpProgram.AnchoredInstructions))+31) shr 5;
  if l>65536 then begin
   exit;
  end;
  if length(Visited)<l then begin
   SetLength(Visited,l);
  end;
  FillChar(Visited[0],l*sizeof(longint),#0);

  if length(RegExpDataInstance.WorkCaptures)<RegExp.CountOfCaptures then begin
   SetLength(RegExpDataInstance.WorkCaptures,RegExp.CountOfCaptures);
  end;
  if not RegExpProgram.HasRootCapture then begin
   Move(RegExpDataInstance.Captures[0],RegExpDataInstance.WorkCaptures[0],RegExp.CountOfCaptures*sizeof(TBRRERegExpCapture));
  end;

  State:=brreucACCEPT;
  l:=Len;
  LenInput:=RegExpDataInstance.InputUnitsLength;
  while (l>0) and (CodeUnit<=LenInput) do begin
   LastCodePoint:=CodePoint;
   LastCodeUnit:=CodeUnit;
   result:=TrySearch(RegExpProgram.AnchoredStart,CodePoint,CodeUnit);
   if result then begin
    break;
   end else begin
    dec(l);
    CodePoint:=LastCodePoint;
    CodeUnit:=LastCodeUnit;
    State:=UTF8DFATransitions[State+UTF8DFACharClasses[Input[CodeUnit]]];
    if State in [brreucACCEPT,brreucERROR] then begin
     State:=brreucACCEPT;
     inc(CodePoint);
    end;
    inc(CodeUnit);
   end;
  end;

 end;
end;

function TBRREBitstateNFA.PtrSearch(const Input:pansichar;var CodePoint,CodeUnit:longint;ALen,ABaseCodeUnit,EndCodeUnit:longint):boolean;
 function TrySearch(StartInstruction:PBRRERegExpNFAInstruction;var CodePoint,CodeUnit:longint):boolean;
 var Job:PBRREBitstateNFAJob;
     Instruction:PBRRERegExpNFAInstruction;
     Argument,i,LastCodePoint:longint;
     CurrentChar,Flags:longword;
     InputIsUTF8:boolean;
  function Satisfy(NextFlags:longword;CodeUnit:longint):boolean;
  var Flags,LastChar,CurrentChar,NextChar:longword;
      Index,NextCodeUnit:longint;
  begin
   LastChar:=$ffffffff;
   CurrentChar:=$ffffffff;
   NextChar:=$ffffffff;
   if RegExpDataInstance.InputIsUTF8 then begin
    if CodeUnit>=0 then begin
     Index:=CodeUnit;
     BRREUTF8PtrDec(Input,RegExpDataInstance.InputUnitsLength,Index);
     if Index>=0 then begin
      LastChar:=BRREUTF8PtrCodeUnitGetChar(Input,RegExpDataInstance.InputUnitsLength,Index);
     end;
     NextCodeUnit:=CodeUnit;
     CurrentChar:=BRREUTF8PtrCodeUnitGetCharAndInc(Input,RegExpDataInstance.InputUnitsLength,NextCodeUnit);
     if NextCodeUnit<RegExpDataInstance.InputUnitsLength then begin
      NextChar:=BRREUTF8PtrCodeUnitGetChar(Input,RegExpDataInstance.InputUnitsLength,CodeUnit);
     end;
    end;
   end else begin
    if CodeUnit>=0 then begin
     if CodeUnit>0 then begin
      LastChar:=byte(ansichar(Input[CodeUnit-1]));
     end;
     CurrentChar:=byte(ansichar(Input[CodeUnit]));
     NextCodeUnit:=CodeUnit+1;
     if NextCodeUnit<RegExpDataInstance.InputUnitsLength then begin
      NextChar:=byte(ansichar(Input[NextCodeUnit]));
     end;
    end;
   end;
   Flags:=0;
   if CodeUnit<=0 then begin
    Flags:=Flags or (brresfEmptyBeginText or brresfEmptyBeginLine);
   end else begin
    case LastChar of
     $000a,$00d,$2028,$2029:begin
      Flags:=Flags or brresfEmptyBeginLine;
     end;
    end;
   end;
   if CodeUnit>(RegExpDataInstance.InputUnitsLength-1) then begin
    Flags:=Flags or (brresfEmptyEndText or brresfEmptyEndLine);
   end else begin
    case CurrentChar of
     $000a,$00d,$2028,$2029:begin
      Flags:=Flags or brresfEmptyEndLine;
     end;
    end;
   end;
   if RegExpDataInstance.InputUnitsLength>0 then begin
    if CodeUnit=0 then begin
     if RegExp.IsWordChar(CurrentChar) then begin
      Flags:=Flags or brresfEmptyWordBoundary;
     end;
    end else if CodeUnit>=RegExpDataInstance.InputUnitsLength then begin
     if RegExp.IsWordChar(byte(LastChar)) then begin
      Flags:=Flags or brresfEmptyWordBoundary;
     end;
    end else if RegExp.IsWordChar(LastChar)<>RegExp.IsWordChar(CurrentChar) then begin
     Flags:=Flags or brresfEmptyWordBoundary;
    end;
   end;
   if (Flags and brresfEmptyWordBoundary)=0 then begin
    Flags:=Flags or brresfEmptyNonWordBoundary;
   end;
   if NextChar<>0 then begin
   end;
   result:=((NextFlags and brresfEmptyAllFlags) and not Flags)=0;
  end;
 begin
  result:=false;

  InputIsUTF8:=RegExpDataInstance.InputIsUTF8;

  LastCodePoint:=-1;

  CountJobs:=0;
  Push(StartInstruction,CodePoint,CodeUnit,0);

  while CountJobs>0 do begin
   dec(CountJobs);
   Job:=@Jobs[CountJobs];

   Instruction:=Job^.Instruction;
   CodePoint:=Job^.CodePoint;
   CodeUnit:=Job^.CodeUnit;
   Argument:=Job^.Argument;

   repeat

    case Instruction^.IDandOpcode and $ff of
     brreoFAIL:begin
      result:=false;
      exit;
     end;
     brreoSPLIT,brreoSPLITLONGEST:begin
      case Argument of
       0:begin
        Push(Instruction,CodePoint,CodeUnit,1);
        Instruction:=Instruction^.x;
        if ShouldVisit(Instruction,CodeUnit) then begin
         continue;
        end;
       end;
       1:begin
        Argument:=0;
        Instruction:=Instruction^.y;
        if ShouldVisit(Instruction,CodeUnit) then begin
         continue;
        end;
       end;
      end;
     end;
     brreoCHAR:begin
      if CodeUnit<EndCodeUnit then begin
       if InputIsUTF8 then begin
        CurrentChar:=BRREUTF8PtrCodeUnitGetCharAndInc(Input,RegExpDataInstance.InputUnitsLength,CodeUnit);
       end else begin
        CurrentChar:=byte(ansichar(Input[CodeUnit]));
        inc(CodeUnit);
       end;
       inc(CodePoint);
      end else begin
       CurrentChar:=$ffffffff;
      end;
      if (CurrentChar<>$ffffffff) and
         (((CurrentChar<256) and (ansichar(byte(CurrentChar)) in TBRRERegExpCharClass(pointer(Instruction^.Value)).Charset)) or
          ((CurrentChar>255) and TBRRERegExpCharClass(pointer(Instruction^.Value)).Contains(CurrentChar))) then begin
       Instruction:=Instruction^.x;
       if ShouldVisit(Instruction,CodeUnit) then begin
        continue;
       end;
      end;
     end;
     brreoCHARSINGLE:begin
      if CodeUnit<EndCodeUnit then begin
       if InputIsUTF8 then begin
        CurrentChar:=BRREUTF8PtrCodeUnitGetCharAndInc(Input,RegExpDataInstance.InputUnitsLength,CodeUnit);
       end else begin
        CurrentChar:=byte(ansichar(Input[CodeUnit]));
        inc(CodeUnit);
       end;
       inc(CodePoint);
      end else begin
       CurrentChar:=$ffffffff;
      end;
      if CurrentChar=ptruint(Instruction^.Value) then begin
       Instruction:=Instruction^.x;
       if ShouldVisit(Instruction,CodeUnit) then begin
        continue;
       end;
      end;
     end;
     brreoANY:begin
      if CodeUnit<RegExpDataInstance.InputUnitsLength then begin
       if InputIsUTF8 then begin
        CurrentChar:=BRREUTF8PtrCodeUnitGetCharAndInc(Input,RegExpDataInstance.InputUnitsLength,CodeUnit);
       end else begin
        CurrentChar:=byte(ansichar(Input[CodeUnit]));
        inc(CodeUnit);
       end;
       inc(CodePoint);
       Instruction:=Instruction^.x;
       if ShouldVisit(Instruction,CodeUnit) then begin
        continue;
       end;
      end else begin
       CurrentChar:=$ffffffff;
      end;
     end;
     brreoMATCH:begin
      if (RegExp.Flags and brrefLONGEST)<>0 then begin
       if LastCodePoint<CodePoint then begin
        LastCodePoint:=CodePoint;
        for i:=RegExpProgram.FirstCapture to RegExpProgram.FirstCapture+(RegExpProgram.CountOfCaptures-1) do begin
         RegExpDataInstance.Captures[i]:=RegExpDataInstance.WorkCaptures[i];
        end;
       end;
      end else begin
       for i:=RegExpProgram.FirstCapture to RegExpProgram.FirstCapture+(RegExpProgram.CountOfCaptures-1) do begin
        RegExpDataInstance.Captures[i]:=RegExpDataInstance.WorkCaptures[i];
       end;
       exit;
      end;
     end;
     brreoNONE,brreoJMP,brreoPREFIX,brreoPREFIXCHAR:begin
      Instruction:=Instruction^.x;
      if ShouldVisit(Instruction,CodeUnit) then begin
       continue;
      end;
     end;
     brreoBEGIN:begin
      case Argument of
       0:begin
        Push(Instruction,RegExpDataInstance.WorkCaptures[Instruction^.Value].StartCodePoint,RegExpDataInstance.WorkCaptures[Instruction^.Value].StartCodeUnit,1);
        RegExpDataInstance.WorkCaptures[Instruction^.Value].StartCodePoint:=CodePoint;
        RegExpDataInstance.WorkCaptures[Instruction^.Value].StartCodeUnit:=CodeUnit;
        Instruction:=Instruction^.x;
        if ShouldVisit(Instruction,CodeUnit) then begin
         continue;
        end;
       end;
       1:begin
        RegExpDataInstance.WorkCaptures[Instruction^.Value].StartCodePoint:=CodePoint;
        RegExpDataInstance.WorkCaptures[Instruction^.Value].StartCodeUnit:=CodeUnit;
       end;
      end;
     end;
     brreoEND:begin
      case Argument of
       0:begin
        Push(Instruction,RegExpDataInstance.WorkCaptures[Instruction^.Value].EndCodePoint,RegExpDataInstance.WorkCaptures[Instruction^.Value].EndCodeUnit,1);
        RegExpDataInstance.WorkCaptures[Instruction^.Value].EndCodePoint:=CodePoint;
        RegExpDataInstance.WorkCaptures[Instruction^.Value].EndCodeUnit:=CodeUnit;
        Instruction:=Instruction^.x;
        if ShouldVisit(Instruction,CodeUnit) then begin
         continue;
        end;
       end;
       1:begin
        RegExpDataInstance.WorkCaptures[Instruction^.Value].EndCodePoint:=CodePoint;
        RegExpDataInstance.WorkCaptures[Instruction^.Value].EndCodeUnit:=CodeUnit;
       end;
      end;
     end;
     brreoBOML,brreoEOML,brreoBOSL,brreoEOSL,brreoBOS,brreoEOS,brreoBRK,brreoNBRK:begin
      Flags:=BRREWhichFlags[Instruction^.IDandOpcode and $ff];
      if (Flags=0) or Satisfy(Flags,CodeUnit) then begin
       Instruction:=Instruction^.x;
       if ShouldVisit(Instruction,CodeUnit) then begin
        continue;
       end;
      end;
     end;
    end;

    break;
   until false;

  end;

 end;
var l,LenInput,LastCodePoint,LastCodeUnit:longint;
    State:longword;
begin
 result:=false;
 if ALen>0 then begin

  Len:=ALen;
  BaseCodeUnit:=ABaseCodeUnit;

  l:=(((Len+1)*length(RegExpProgram.AnchoredInstructions))+31) shr 5;
  if l>65536 then begin
   exit;
  end;
  if length(Visited)<l then begin
   SetLength(Visited,l);
  end;
  FillChar(Visited[0],l*sizeof(longint),#0);

  if length(RegExpDataInstance.WorkCaptures)<RegExp.CountOfCaptures then begin
   SetLength(RegExpDataInstance.WorkCaptures,RegExp.CountOfCaptures);
  end;
  if not RegExpProgram.HasRootCapture then begin
   Move(RegExpDataInstance.Captures[0],RegExpDataInstance.WorkCaptures[0],RegExp.CountOfCaptures*sizeof(TBRRERegExpCapture));
  end;

  State:=brreucACCEPT;
  l:=Len;
  LenInput:=RegExpDataInstance.InputUnitsLength;
  while (l>0) and (CodeUnit<=LenInput) do begin
   LastCodePoint:=CodePoint;
   LastCodeUnit:=CodeUnit;
   result:=TrySearch(RegExpProgram.AnchoredStart,CodePoint,CodeUnit);
   if result then begin
    break;
   end else begin
    dec(l);
    CodePoint:=LastCodePoint;
    CodeUnit:=LastCodeUnit;
    State:=UTF8DFATransitions[State+UTF8DFACharClasses[Input[CodeUnit]]];
    if State in [brreucACCEPT,brreucERROR] then begin
     State:=brreucACCEPT;
     inc(CodePoint);
    end;
    inc(CodeUnit);
   end;
  end;

 end;
end;

constructor TBRREParallelNFA.Create(ARegExpProgram:TBRRERegExpProgram;ARegExpDataInstance:TBRRERegExpDataInstance;ARegExpProgramData:PBRRERegExpProgramDataInstance);
begin
 inherited Create;
 RegExp:=ARegExpProgram.RegExp;
 RegExpProgram:=ARegExpProgram;
 RegExpDataInstance:=ARegExpDataInstance;
 RegExpProgramData:=ARegExpProgramData;
 States:=TBRREPointerList.Create;
 FreeState:=nil;
 UsedState:=nil;
 New(InstanceCurrentList);
 New(InstanceNewList);
 FillChar(InstanceCurrentList^,sizeof(TBRREParallelNFAThreadList),#0);
 FillChar(InstanceNewList^,sizeof(TBRREParallelNFAThreadList),#0);
 StackThreads:=nil;
 SetLength(StackThreads,length(RegExpProgram.Instructions)+1);
end;

destructor TBRREParallelNFA.Destroy;
var i:longint;
    State:PBRRERegExpState;
begin
 for i:=0 to States.Count-1 do begin
  State:=PBRRERegExpState(States[i]);
  if assigned(State) then begin
   SetLength(State^.Captures,0);
   Dispose(State);
  end;
 end;
 FreeAndNil(States);
 SetLength(InstanceCurrentList^.Threads,0);
 Dispose(InstanceCurrentList);
 SetLength(InstanceNewList^.Threads,0);
 Dispose(InstanceNewList);
 SetLength(StackThreads,0);
 inherited Destroy;
end;

function TBRREParallelNFA.StateNew(Count:longint):PBRRERegExpState;
begin
 result:=FreeState;
 if assigned(result) then begin
  FreeState:=result^.NextFree;
 end else begin
  New(result);
  result^.Captures:=nil;
  States.Add(result);
 end;
 if length(result^.Captures)<>Count then begin
  SetLength(result^.Captures,Count);
 end;
 result^.ReferenceCount:=1;
 result^.NextFree:=nil;
 result^.NextUsed:=UsedState;
 UsedState:=result;
end;

procedure TBRREParallelNFA.StateRelease(State:PBRRERegExpState);
begin
 dec(State^.ReferenceCount);
 if State^.ReferenceCount=0 then begin
  UsedState:=State^.NextUsed;
  State^.NextUsed:=nil;
  State^.NextFree:=FreeState;
  FreeState:=State;
 end;
end;

procedure TBRREParallelNFA.ThreadAdd(List:PBRREParallelNFAThreadList;Thread:TBRREParallelNFAThread;CodePoint,CodeUnit:longint;CurrentWorkChar:longword);
var NextChar:longword;
    Index,StackPointer:longint;
    State:PBRRERegExpState;
    Stack:PBRREParallelNFAThreadsStatic;
    InstructionGenerationIDs:PBRRERegExpDataInstanceInstructionGenerationIDsStatic;
begin
 Stack:=@StackThreads[0];
 InstructionGenerationIDs:=@RegExpProgramData^.InstructionGenerationIDs[0];
 StackPointer:=0;
 Stack^[StackPointer]:=Thread;
 inc(StackPointer);
 while StackPointer>0 do begin
  dec(StackPointer);
  Thread:=Stack^[StackPointer];
  repeat
   if InstructionGenerationIDs^[Thread.Instruction^.IDandOpcode shr 8]<>List^.GenerationID then begin
    InstructionGenerationIDs^[Thread.Instruction^.IDandOpcode shr 8]:=List^.GenerationID;
    case Thread.Instruction^.IDandOpcode and $ff of
     brreoJMP,brreoPREFIX,brreoPREFIXCHAR:begin
      Thread.Instruction:=Thread.Instruction^.x;
     end;
     brreoSPLIT,brreoSPLITLONGEST:begin
      Stack^[StackPointer+1].Instruction:=Thread.Instruction^.x;
      Stack^[StackPointer+1].State:=Thread.State;
      Stack^[StackPointer+0].Instruction:=Thread.Instruction^.y;
      Stack^[StackPointer+0].State:=Thread.State;
      inc(Thread.State^.ReferenceCount);
      inc(StackPointer,2);
      break;
     end;
     brreoBEGIN:begin
      if Thread.State^.ReferenceCount>1 then begin
       State:=Thread.State;
       Thread.State:=StateNew(CountOfStateCaptures);
       if CountOfStateCaptures>0 then begin
        Move(State^.Captures[0],Thread.State^.Captures[0],CountOfStateCaptures*sizeof(TBRRERegExpCapture));
       end;
       StateRelease(State);
      end;
      Index:=Thread.Instruction^.Value+CaptureOffset;
      Thread.Instruction:=Thread.Instruction^.x;
      Thread.State^.Captures[Index].StartCodePoint:=CodePoint;
      Thread.State^.Captures[Index].StartCodeUnit:=CodeUnit;
     end;
     brreoEND:begin
      if Thread.State^.ReferenceCount>1 then begin
       State:=Thread.State;
       Thread.State:=StateNew(CountOfStateCaptures);
       if CountOfStateCaptures>0 then begin
        Move(State^.Captures[0],Thread.State^.Captures[0],CountOfStateCaptures*sizeof(TBRRERegExpCapture));
       end;
       StateRelease(State);
      end;
      Index:=Thread.Instruction^.Value+CaptureOffset;
      Thread.Instruction:=Thread.Instruction^.x;
      Thread.State^.Captures[Index].EndCodePoint:=CodePoint;
      Thread.State^.Captures[Index].EndCodeUnit:=CodeUnit;
     end;
     brreoBOML:begin
      if CodePoint=0 then begin
       Thread.Instruction:=Thread.Instruction^.x;
      end else begin
       if CodeUnit<=RegExpDataInstance.InputUnitsLength then begin
        if RegExpDataInstance.InputIsUTF8 then begin
         NextChar:=BRREUTF8CodeUnitGetChar(InputStr,CodeUnit);
        end else begin
         NextChar:=byte(ansichar(InputStr[CodeUnit]));
        end;
       end else begin
        NextChar:=$ffffffff;
       end;
       case NextChar of
        $000a,$00d,$2028,$2029:begin
         Thread.Instruction:=Thread.Instruction^.x;
        end;
        else begin
         StateRelease(Thread.State);
         break;
        end;
       end;
      end;
     end;
     brreoEOML:begin
      if CodePoint>=RegExpDataInstance.InputPointsLength then begin
       Thread.Instruction:=Thread.Instruction^.x;
      end else begin
       if CodeUnit<=RegExpDataInstance.InputUnitsLength then begin
        if RegExpDataInstance.InputIsUTF8 then begin
         NextChar:=BRREUTF8CodeUnitGetChar(InputStr,CodeUnit);
        end else begin
         NextChar:=byte(ansichar(InputStr[CodeUnit]));
        end;
       end else begin
        NextChar:=$ffffffff;
       end;
       case NextChar of
        $000a,$00d,$2028,$2029:begin
         Thread.Instruction:=Thread.Instruction^.x;
        end;
        else begin
         StateRelease(Thread.State);
         break;
        end;
       end;
      end;
     end;
     brreoBOSL:begin
      if CodePoint=0 then begin
       Thread.Instruction:=Thread.Instruction^.x;
      end else begin
       StateRelease(Thread.State);
       break;
      end;
     end;
     brreoEOSL:begin
      if CodePoint>=RegExpDataInstance.InputPointsLength then begin
       Thread.Instruction:=Thread.Instruction^.x;
      end else begin
       StateRelease(Thread.State);
       break;
      end;
     end;
     brreoBOS:begin
      if CodePoint=0 then begin
       Thread.Instruction:=Thread.Instruction^.x;
      end else begin
       StateRelease(Thread.State);
       break;
      end;
     end;
     brreoEOS:begin
      if CodePoint>=RegExpDataInstance.InputPointsLength then begin
       Thread.Instruction:=Thread.Instruction^.x;
      end else begin
       StateRelease(Thread.State);
       break;
      end;
     end;
     brreoBRK:begin
      if CodeUnit<=RegExpDataInstance.InputUnitsLength then begin
       if RegExpDataInstance.InputIsUTF8 then begin
        NextChar:=BRREUTF8CodeUnitGetChar(InputStr,CodeUnit);
       end else begin
        NextChar:=byte(ansichar(InputStr[CodeUnit]));
       end;
      end else begin
       NextChar:=$ffffffff;
      end;
      if RegExp.IsWordChar(NextChar)<>RegExp.IsWordChar(CurrentWorkChar) then begin
       Thread.Instruction:=Thread.Instruction^.x;
      end else begin
       StateRelease(Thread.State);
       break;
      end;
     end;
     brreoNBRK:begin
      if CodeUnit<=RegExpDataInstance.InputUnitsLength then begin
       if RegExpDataInstance.InputIsUTF8 then begin
        NextChar:=BRREUTF8CodeUnitGetChar(InputStr,CodeUnit);
       end else begin
        NextChar:=byte(ansichar(InputStr[CodeUnit]));
       end;
      end else begin
       NextChar:=$ffffffff;
      end;
      if RegExp.IsWordChar(NextChar)=RegExp.IsWordChar(CurrentWorkChar) then begin
       Thread.Instruction:=Thread.Instruction^.x;
      end else begin
       StateRelease(Thread.State);
       break;
      end;
     end;
     else begin
      if List^.Count>=length(List^.Threads) then begin
       SetLength(List^.Threads,BRRERoundUpToPowerOfTwo(List^.Count+1));
      end;
      List^.Threads[List^.Count]:=Thread;
      inc(List^.Count);
      break;
     end;
    end;
    continue;
   end else begin
    break;
   end;
  until false;
 end;
end;

function TBRREParallelNFA.Search(const AInput:ansistring;var CodePoint,CodeUnit:longint):boolean;
var CurrentCodePoint,CurrentCodeUnit,Len,NextCodePoint,NextCodeUnit,Index,LastCodePoint:longint;
    CurrentWorkChar:longword;
    CurrentList,NewList,TempList:PBRREParallelNFAThreadList;
    Thread:TBRREParallelNFAThread;
    State,NextState,MatchState:PBRRERegExpState;
begin
 if RegExpProgram.AnchorBeginText and (CodeUnit<>1) then begin
  result:=false;
  exit;
 end;

 InputStr:=AInput;
 try

  CurrentCodePoint:=CodePoint;
  CurrentCodeUnit:=CodeUnit;

  if RegExpProgram.HasRootCapture then begin
   CountOfStateCaptures:=RegExp.CountOfCaptures;
   CaptureOffset:=0;
  end else begin
   CountOfStateCaptures:=RegExpProgram.CountOfCaptures;
   CaptureOffset:=-RegExpProgram.FirstCapture;
  end;

  State:=StateNew(CountOfStateCaptures);
  if RegExpProgram.HasRootCapture then begin
   Move(RegExpDataInstance.Captures[0],State.Captures[0],RegExp.CountOfCaptures*sizeof(TBRRERegExpCapture));
  end else if CountOfStateCaptures>0 then begin
   Move(RegExpDataInstance.Captures[RegExpProgram.FirstCapture],State.Captures[0],CountOfStateCaptures*sizeof(TBRRERegExpCapture));
  end;

  CurrentList:=InstanceCurrentList;
  NewList:=InstanceNewList;
  if length(CurrentList^.Threads)<RegExpProgram.Len then begin
   SetLength(CurrentList^.Threads,RegExpProgram.Len);
  end;
  if length(NewList^.Threads)<RegExpProgram.Len then begin
   SetLength(NewList^.Threads,RegExpProgram.Len);
  end;
  CurrentList^.Count:=0;
  NewList^.Count:=0;

  inc(RegExpProgramData^.GenerationID);
  CurrentList^.GenerationID:=RegExpProgramData^.GenerationID;

  inc(RegExpProgramData^.GenerationID);
  NewList^.GenerationID:=RegExpProgramData^.GenerationID;

  if (CurrentCodeUnit>1) and (CurrentCodeUnit<=RegExpDataInstance.InputUnitsLength) then begin
   if RegExpDataInstance.InputIsUTF8 then begin
    Index:=CurrentCodeUnit;
    BRREUTF8Dec(AInput,Index);
    if Index>0 then begin
     CurrentWorkChar:=BRREUTF8CodeUnitGetChar(AInput,Index);
    end else begin
     CurrentWorkChar:=$ffffffff;
    end;
   end else begin
    CurrentWorkChar:=byte(ansichar(AInput[CurrentCodeUnit-1]));
   end;
  end else begin
   CurrentWorkChar:=$ffffffff;
  end;

  Thread.Instruction:=RegExpProgram.Start;
  Thread.State:=State;
  ThreadAdd(CurrentList,Thread,CurrentCodePoint,CurrentCodeUnit,CurrentWorkChar);

  NextCodePoint:=CurrentCodePoint;
  NextCodeUnit:=CurrentCodeUnit;

  Len:=RegExpDataInstance.InputUnitsLength;

  MatchState:=nil;

  LastCodePoint:=-1;

  State:=nil;
  while CurrentList^.Count<>0 do begin
   CurrentCodePoint:=NextCodePoint;
   CurrentCodeUnit:=NextCodeUnit;

   if CurrentCodeUnit<=Len then begin
    if RegExpDataInstance.InputIsUTF8 then begin
     NextCodeUnit:=CurrentCodeUnit;
     CurrentWorkChar:=BRREUTF8CodeUnitGetCharAndInc(AInput,NextCodeUnit);
    end else begin
     CurrentWorkChar:=byte(ansichar(AInput[CurrentCodeUnit]));
     NextCodeUnit:=CurrentCodeUnit+1;
    end;
   end else begin
    CurrentWorkChar:=$ffffffff;
    NextCodeUnit:=CurrentCodeUnit+1;
   end;
   NextCodePoint:=CurrentCodePoint+1;

   Index:=0;
   while Index<CurrentList^.Count do begin
    Thread:=CurrentList^.Threads[Index];
    inc(Index);
    case Thread.Instruction^.IDandOpcode and $ff of
     brreoCHAR:begin
      if (CurrentWorkChar<>$ffffffff) and
         (((CurrentWorkChar<256) and (ansichar(byte(CurrentWorkChar)) in TBRRERegExpCharClass(pointer(Thread.Instruction^.Value)).Charset)) or
          ((CurrentWorkChar>255) and TBRRERegExpCharClass(pointer(Thread.Instruction^.Value)).Contains(CurrentWorkChar))) then begin
       Thread.Instruction:=Thread.Instruction^.x;
       ThreadAdd(NewList,Thread,NextCodePoint,NextCodeUnit,CurrentWorkChar);
      end else begin
       StateRelease(Thread.State);
      end;
     end;
     brreoCHARSINGLE:begin
      if CurrentWorkChar=ptruint(Thread.Instruction^.Value) then begin
       Thread.Instruction:=Thread.Instruction^.x;
       ThreadAdd(NewList,Thread,NextCodePoint,NextCodeUnit,CurrentWorkChar);
      end else begin
       StateRelease(Thread.State);
      end;
     end;
     brreoANY:begin
      if CurrentWorkChar=$ffffffff then begin
       StateRelease(Thread.State);
      end else begin
       Thread.Instruction:=Thread.Instruction^.x;
       ThreadAdd(NewList,Thread,NextCodePoint,NextCodeUnit,CurrentWorkChar);
      end;
     end;
     brreoMATCH:begin
      if (RegExp.Flags and brrefLONGEST)<>0 then begin
       if not assigned(MatchState) then begin
        MatchState:=StateNew(CountOfStateCaptures);
        LastCodePoint:=-1;
       end;
       if LastCodePoint<CurrentCodePoint then begin
        LastCodePoint:=CurrentCodePoint;
        if CountOfStateCaptures>0 then begin
         Move(Thread.State^.Captures[0],MatchState^.Captures[0],CountOfStateCaptures*sizeof(TBRRERegExpCapture));
        end;
       end;
      end else begin
       if assigned(State) then begin
        StateRelease(State);
       end;
       State:=Thread.State;
       while Index<CurrentList^.Count do begin
        StateRelease(CurrentList^.Threads[Index].State);
        inc(Index);
       end;
       break;
      end;
     end;
     brreoFAIL:begin
      if assigned(State) then begin
       StateRelease(State);
      end;
      State:=nil;
      while Index<CurrentList^.Count do begin
       StateRelease(CurrentList^.Threads[Index].State);
       inc(Index);
      end;
      break;
     end;
    end;
   end;

   TempList:=CurrentList;
   CurrentList:=NewList;
   NewList:=TempList;
   NewList^.Count:=0;
   inc(RegExpProgramData^.GenerationID);
   NewList^.GenerationID:=RegExpProgramData^.GenerationID;

  end;

  if assigned(MatchState) then begin
   if assigned(State) then begin
    StateRelease(State);
   end;
   State:=MatchState;
  end;

  if assigned(State) then begin
   for Index:=RegExpProgram.FirstCapture to RegExpProgram.FirstCapture+(RegExpProgram.CountOfCaptures-1) do begin
    RegExpDataInstance.Captures[Index]:=State^.Captures[Index+CaptureOffset];
   end;
   if RegExpProgram.HasRootCapture then begin
    RegExpDataInstance.Captures[0]:=State^.Captures[0];
   end;
   StateRelease(State);
   result:=true;
  end else begin
   result:=false;
  end;

  State:=UsedState;
  UsedState:=nil;
  while assigned(State) do begin
   NextState:=State^.NextUsed;
   State^.NextFree:=FreeState;
   FreeState:=State;
   State:=NextState;
  end;

  CodePoint:=CurrentCodePoint;
  CodeUnit:=CurrentCodeUnit;
 finally
  InputStr:='';
 end;
end;

procedure TBRREParallelNFA.PtrThreadAdd(List:PBRREParallelNFAThreadList;Thread:TBRREParallelNFAThread;CodePoint,CodeUnit:longint;CurrentWorkChar:longword);
var NextChar:longword;
    Index,StackPointer:longint;
    State:PBRRERegExpState;
    Stack:PBRREParallelNFAThreadsStatic;
    InstructionGenerationIDs:PBRRERegExpDataInstanceInstructionGenerationIDsStatic;
begin
 Stack:=@StackThreads[0];
 InstructionGenerationIDs:=@RegExpProgramData^.InstructionGenerationIDs[0];
 StackPointer:=0;
 Stack^[StackPointer]:=Thread;
 inc(StackPointer);
 while StackPointer>0 do begin
  dec(StackPointer);
  Thread:=Stack^[StackPointer];                                                           
  repeat
   if InstructionGenerationIDs^[Thread.Instruction^.IDandOpcode shr 8]<>List^.GenerationID then begin
    InstructionGenerationIDs^[Thread.Instruction^.IDandOpcode shr 8]:=List^.GenerationID;
    case Thread.Instruction^.IDandOpcode and $ff of
     brreoJMP,brreoPREFIX,brreoPREFIXCHAR:begin
      Thread.Instruction:=Thread.Instruction^.x;
     end;
     brreoSPLIT,brreoSPLITLONGEST:begin
      Stack^[StackPointer+1].Instruction:=Thread.Instruction^.x;
      Stack^[StackPointer+1].State:=Thread.State;
      Stack^[StackPointer+0].Instruction:=Thread.Instruction^.y;
      Stack^[StackPointer+0].State:=Thread.State;
      inc(Thread.State^.ReferenceCount);
      inc(StackPointer,2);
      break;
     end;
     brreoBEGIN:begin
      if Thread.State^.ReferenceCount>1 then begin
       State:=Thread.State;
       Thread.State:=StateNew(CountOfStateCaptures);
       if CountOfStateCaptures>0 then begin
        Move(State^.Captures[0],Thread.State^.Captures[0],CountOfStateCaptures*sizeof(TBRRERegExpCapture));
       end;
       StateRelease(State);
      end;
      Index:=Thread.Instruction^.Value+CaptureOffset;
      Thread.Instruction:=Thread.Instruction^.x;
      Thread.State^.Captures[Index].StartCodePoint:=CodePoint;
      Thread.State^.Captures[Index].StartCodeUnit:=CodeUnit;
     end;
     brreoEND:begin
      if Thread.State^.ReferenceCount>1 then begin
       State:=Thread.State;
       Thread.State:=StateNew(CountOfStateCaptures);
       if CountOfStateCaptures>0 then begin
        Move(State^.Captures[0],Thread.State^.Captures[0],CountOfStateCaptures*sizeof(TBRRERegExpCapture));
       end;
       StateRelease(State);
      end;
      Index:=Thread.Instruction^.Value+CaptureOffset;
      Thread.Instruction:=Thread.Instruction^.x;
      Thread.State^.Captures[Index].EndCodePoint:=CodePoint;
      Thread.State^.Captures[Index].EndCodeUnit:=CodeUnit;
     end;
     brreoBOML:begin
      if CodePoint=0 then begin
       Thread.Instruction:=Thread.Instruction^.x;
      end else begin
       if CodeUnit<RegExpDataInstance.InputUnitsLength then begin
        if RegExpDataInstance.InputIsUTF8 then begin
         NextChar:=BRREUTF8PtrCodeUnitGetChar(InputPtr,RegExpDataInstance.InputUnitsLength,CodeUnit);
        end else begin
         NextChar:=byte(ansichar(InputPtr[CodeUnit]));
        end;
       end else begin
        NextChar:=$ffffffff;
       end;
       case NextChar of
        $000a,$00d,$2028,$2029:begin
         Thread.Instruction:=Thread.Instruction^.x;
        end;
        else begin
         StateRelease(Thread.State);
         break;
        end;
       end;
      end;
     end;
     brreoEOML:begin
      if CodePoint>=RegExpDataInstance.InputPointsLength then begin
       Thread.Instruction:=Thread.Instruction^.x;
      end else begin
       if CodeUnit<RegExpDataInstance.InputUnitsLength then begin
        if RegExpDataInstance.InputIsUTF8 then begin
         NextChar:=BRREUTF8PtrCodeUnitGetChar(InputPtr,RegExpDataInstance.InputUnitsLength,CodeUnit);
        end else begin
         NextChar:=byte(ansichar(InputPtr[CodeUnit]));
        end;
       end else begin
        NextChar:=$ffffffff;
       end;
       case NextChar of
        $000a,$00d,$2028,$2029:begin
         Thread.Instruction:=Thread.Instruction^.x;
        end;
        else begin
         StateRelease(Thread.State);
         break;
        end;
       end;
      end;
     end;
     brreoBOSL:begin
      if CodePoint=0 then begin
       Thread.Instruction:=Thread.Instruction^.x;
      end else begin
       StateRelease(Thread.State);
       break;
      end;
     end;
     brreoEOSL:begin
      if CodePoint>=RegExpDataInstance.InputPointsLength then begin
       Thread.Instruction:=Thread.Instruction^.x;
      end else begin
       StateRelease(Thread.State);
       break;
      end;
     end;
     brreoBOS:begin
      if CodePoint=0 then begin
       Thread.Instruction:=Thread.Instruction^.x;
      end else begin
       StateRelease(Thread.State);
       break;
      end;
     end;
     brreoEOS:begin
      if CodePoint>=RegExpDataInstance.InputPointsLength then begin
       Thread.Instruction:=Thread.Instruction^.x;
      end else begin
       StateRelease(Thread.State);
       break;
      end;
     end;
     brreoBRK:begin
      if CodeUnit<RegExpDataInstance.InputUnitsLength then begin
       if RegExpDataInstance.InputIsUTF8 then begin
        NextChar:=BRREUTF8PtrCodeUnitGetChar(InputPtr,RegExpDataInstance.InputUnitsLength,CodeUnit);
       end else begin
        NextChar:=byte(ansichar(InputPtr[CodeUnit]));
       end;
      end else begin
       NextChar:=$ffffffff;
      end;
      if RegExp.IsWordChar(NextChar)<>RegExp.IsWordChar(CurrentWorkChar) then begin
       Thread.Instruction:=Thread.Instruction^.x;
      end else begin
       StateRelease(Thread.State);
       break;
      end;
     end;
     brreoNBRK:begin
      if CodeUnit<RegExpDataInstance.InputUnitsLength then begin
       if RegExpDataInstance.InputIsUTF8 then begin
        NextChar:=BRREUTF8PtrCodeUnitGetChar(InputPtr,RegExpDataInstance.InputUnitsLength,CodeUnit);
       end else begin
        NextChar:=byte(ansichar(InputPtr[CodeUnit]));
       end;
      end else begin
       NextChar:=$ffffffff;
      end;
      if RegExp.IsWordChar(NextChar)=RegExp.IsWordChar(CurrentWorkChar) then begin
       Thread.Instruction:=Thread.Instruction^.x;
      end else begin
       StateRelease(Thread.State);
       break;
      end;
     end;
     else begin
      if List^.Count>=length(List^.Threads) then begin
       SetLength(List^.Threads,BRRERoundUpToPowerOfTwo(List^.Count+1));
      end;
      List^.Threads[List^.Count]:=Thread;
      inc(List^.Count);
      break;
     end;
    end;
    continue;
   end else begin
    break;
   end;
  until false;
 end;
end;

function TBRREParallelNFA.PtrSearch(const AInput:pansichar;var CodePoint,CodeUnit:longint):boolean;
var CurrentCodePoint,CurrentCodeUnit,NextCodePoint,NextCodeUnit,Index,LastCodePoint:longint;
    CurrentWorkChar:longword;
    CurrentList,NewList,TempList:PBRREParallelNFAThreadList;
    Thread:TBRREParallelNFAThread;
    State,NextState,MatchState:PBRRERegExpState;
    Len:longint;
begin
 if RegExpProgram.AnchorBeginText and (CodeUnit<>0) then begin
  result:=false;
  exit;
 end;

 InputPtr:=AInput;
 try

  CurrentCodePoint:=CodePoint;
  CurrentCodeUnit:=CodeUnit;

  if RegExpProgram.HasRootCapture then begin
   CountOfStateCaptures:=RegExp.CountOfCaptures;
   CaptureOffset:=0;
  end else begin
   CountOfStateCaptures:=RegExpProgram.CountOfCaptures;
   CaptureOffset:=-RegExpProgram.FirstCapture;
  end;

  State:=StateNew(CountOfStateCaptures);
  if RegExpProgram.HasRootCapture then begin
   Move(RegExpDataInstance.Captures[0],State.Captures[0],RegExp.CountOfCaptures*sizeof(TBRRERegExpCapture));
  end else if CountOfStateCaptures>0 then begin
   Move(RegExpDataInstance.Captures[RegExpProgram.FirstCapture],State.Captures[0],CountOfStateCaptures*sizeof(TBRRERegExpCapture));
  end;

  CurrentList:=InstanceCurrentList;
  NewList:=InstanceNewList;
  if length(CurrentList^.Threads)<RegExpProgram.Len then begin
   SetLength(CurrentList^.Threads,RegExpProgram.Len);
  end;
  if length(NewList^.Threads)<RegExpProgram.Len then begin
   SetLength(NewList^.Threads,RegExpProgram.Len);
  end;
  CurrentList^.Count:=0;
  NewList^.Count:=0;

  inc(RegExpProgramData^.GenerationID);
  CurrentList^.GenerationID:=RegExpProgramData^.GenerationID;

  inc(RegExpProgramData^.GenerationID);
  NewList^.GenerationID:=RegExpProgramData^.GenerationID;

  if (CurrentCodeUnit>0) and (CurrentCodeUnit<RegExpDataInstance.InputUnitsLength) then begin
   if RegExpDataInstance.InputIsUTF8 then begin
    Index:=CurrentCodeUnit;
    BRREUTF8PtrDec(AInput,RegExpDataInstance.InputUnitsLength,Index);
    if Index>=0 then begin
     CurrentWorkChar:=BRREUTF8PtrCodeUnitGetChar(AInput,RegExpDataInstance.InputUnitsLength,Index);
    end else begin
     CurrentWorkChar:=$ffffffff;
    end;
   end else begin
    CurrentWorkChar:=byte(ansichar(AInput[CurrentCodeUnit-1]));
   end;
  end else begin
   CurrentWorkChar:=$ffffffff;
  end;

  Thread.Instruction:=RegExpProgram.Start;
  Thread.State:=State;
  PtrThreadAdd(CurrentList,Thread,CurrentCodePoint,CurrentCodeUnit,CurrentWorkChar);

  NextCodePoint:=CurrentCodePoint;
  NextCodeUnit:=CurrentCodeUnit;

  Len:=RegExpDataInstance.InputUnitsLength;

  MatchState:=nil;

  LastCodePoint:=-1;

  State:=nil;
  while CurrentList^.Count<>0 do begin
   CurrentCodePoint:=NextCodePoint;
   CurrentCodeUnit:=NextCodeUnit;

   if CurrentCodeUnit<Len then begin
    if RegExpDataInstance.InputIsUTF8 then begin
     NextCodeUnit:=CurrentCodeUnit;
     CurrentWorkChar:=BRREUTF8PtrCodeUnitGetCharAndInc(AInput,Len,NextCodeUnit);
    end else begin
     CurrentWorkChar:=byte(ansichar(AInput[CurrentCodeUnit]));
     NextCodeUnit:=CurrentCodeUnit+1;
    end;
   end else begin
    CurrentWorkChar:=$ffffffff;
    NextCodeUnit:=CurrentCodeUnit+1;
   end;
   NextCodePoint:=CurrentCodePoint+1;

   Index:=0;
   while Index<CurrentList^.Count do begin
    Thread:=CurrentList^.Threads[Index];
    inc(Index);
    case Thread.Instruction^.IDandOpcode and $ff of
     brreoCHAR:begin
      if (CurrentWorkChar<>$ffffffff) and
         (((CurrentWorkChar<256) and (ansichar(byte(CurrentWorkChar)) in TBRRERegExpCharClass(pointer(Thread.Instruction^.Value)).Charset)) or
          ((CurrentWorkChar>255) and TBRRERegExpCharClass(pointer(Thread.Instruction^.Value)).Contains(CurrentWorkChar))) then begin
       Thread.Instruction:=Thread.Instruction^.x;
       PtrThreadAdd(NewList,Thread,NextCodePoint,NextCodeUnit,CurrentWorkChar);
      end else begin
       StateRelease(Thread.State);
      end;
     end;
     brreoCHARSINGLE:begin
      if CurrentWorkChar=ptruint(Thread.Instruction^.Value) then begin
       Thread.Instruction:=Thread.Instruction^.x;
       PtrThreadAdd(NewList,Thread,NextCodePoint,NextCodeUnit,CurrentWorkChar);
      end else begin
       StateRelease(Thread.State);
      end;
     end;
     brreoANY:begin
      if CurrentWorkChar=$ffffffff then begin
       StateRelease(Thread.State);
      end else begin
       Thread.Instruction:=Thread.Instruction^.x;
       PtrThreadAdd(NewList,Thread,NextCodePoint,NextCodeUnit,CurrentWorkChar);
      end;
     end;
     brreoMATCH:begin
      if (RegExp.Flags and brrefLONGEST)<>0 then begin
       if not assigned(MatchState) then begin
        MatchState:=StateNew(CountOfStateCaptures);
        LastCodePoint:=-1;
       end;
       if LastCodePoint<CurrentCodePoint then begin
        LastCodePoint:=CurrentCodePoint;
        if CountOfStateCaptures>0 then begin
         Move(Thread.State^.Captures[0],MatchState^.Captures[0],CountOfStateCaptures*sizeof(TBRRERegExpCapture));
        end;
       end;
      end else begin
       if assigned(State) then begin
        StateRelease(State);
       end;
       State:=Thread.State;
       while Index<CurrentList^.Count do begin
        StateRelease(CurrentList^.Threads[Index].State);
        inc(Index);
       end;
       break;
      end;
     end;
     brreoFAIL:begin
      if assigned(State) then begin
       StateRelease(State);
      end;
      State:=nil;
      while Index<CurrentList^.Count do begin
       StateRelease(CurrentList^.Threads[Index].State);
       inc(Index);
      end;
      break;
     end;
    end;
   end;

   TempList:=CurrentList;
   CurrentList:=NewList;
   NewList:=TempList;
   NewList^.Count:=0;
   inc(RegExpProgramData^.GenerationID);
   NewList^.GenerationID:=RegExpProgramData^.GenerationID;
  end;

  if assigned(MatchState) then begin
   if assigned(State) then begin
    StateRelease(State);
   end;
   State:=MatchState;
  end;

  if assigned(State) then begin
   for Index:=RegExpProgram.FirstCapture to RegExpProgram.FirstCapture+(RegExpProgram.CountOfCaptures-1) do begin
    RegExpDataInstance.Captures[Index]:=State^.Captures[Index+CaptureOffset];
   end;
   if RegExpProgram.HasRootCapture then begin
    RegExpDataInstance.Captures[0]:=State^.Captures[0];
   end;
   StateRelease(State);
   result:=true;
  end else begin
   result:=false;
  end;

  State:=UsedState;
  UsedState:=nil;
  while assigned(State) do begin
   NextState:=State^.NextUsed;
   State^.NextFree:=FreeState;
   FreeState:=State;
   State:=NextState;
  end;

  CodePoint:=CurrentCodePoint;
  CodeUnit:=CurrentCodeUnit;
 finally
  InputPtr:=nil;
  RegExp.CriticalSection.Enter;
 end;
end;

constructor TBRREBacktrackingNFA.Create(ARegExpProgram:TBRRERegExpProgram;ARegExpDataInstance:TBRRERegExpDataInstance;ARegExpProgramData:PBRRERegExpProgramDataInstance);
begin
 inherited Create;
 RegExp:=ARegExpProgram.RegExp;
 RegExpProgram:=ARegExpProgram;
 RegExpDataInstance:=ARegExpDataInstance;
 RegExpProgramData:=ARegExpProgramData;
 InputStr:='';
 BacktrackingStack:=nil;
 SetLength(BacktrackingStack,1024);
 BacktrackingStackPointer:=0;
 SetLength(MarkStack,0);
 MarkStackPointer:=0;
 MatchCounter:=0;
end;

destructor TBRREBacktrackingNFA.Destroy;
begin
 SetLength(BacktrackingStack,0);
 SetLength(MarkStack,0);
 InputStr:='';
 inherited Destroy;
end;

procedure TBRREBacktrackingNFA.MarkStackPush(CurrentCodePoint,CurrentCodeUnit:longint;CurrentChar:longword); {$IFDEF caninline}inline;{$ENDIF}
var MarkStackItem:PBRREBacktrackingNFAMarkStackItem;
begin
 if MarkStackPointer>=length(MarkStack) then begin
  if length(MarkStack)=0 then begin
   SetLength(MarkStack,1024);
  end else begin
   SetLength(MarkStack,BRRERoundUpToPowerOfTwo(MarkStackPointer+1));
  end;
 end;
 MarkStackItem:=@MarkStack[MarkStackPointer];
 inc(MarkStackPointer);
 MarkStackItem^.CodePoint:=CurrentCodePoint;
 MarkStackItem^.CodeUnit:=CurrentCodeUnit;
 MarkStackItem^.Char:=CurrentChar;
end;

function TBRREBacktrackingNFA.MarkStackPop(TopStack:longint;var MarkStackItem:TBRREBacktrackingNFAMarkStackItem):boolean; {$IFDEF caninline}inline;{$ENDIF}
begin
 result:=MarkStackPointer>TopStack;
 if result then begin
  dec(MarkStackPointer);
  MarkStackItem:=MarkStack[MarkStackPointer];
 end;
end;

function TBRREBacktrackingNFA.GreedyRepeat(var CurrentWorkChar:longword;Instruction:PBRRERegExpNFAInstruction;var CurrentCodePoint,CurrentCodeUnit:longint;Count,Level:longint):boolean;
var CodePoint,CodeUnit,TopStack:longint;
    NextChar:longword;
    MarkStackItem:TBRREBacktrackingNFAMarkStackItem;
begin

 result:=false;

 TopStack:=MarkStackPointer;

 MarkStackPush(CurrentCodePoint,CurrentCodeUnit,CurrentWorkChar);

 while Count<>0 do begin
  NextChar:=CurrentWorkChar;
  if not ExecuteLevel(NextChar,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,CodePoint,CodeUnit,Level+1) then begin
   break;
  end;
  CurrentWorkChar:=NextChar;
  CurrentCodePoint:=CodePoint;
  CurrentCodeUnit:=CodeUnit;
  MarkStackPush(CurrentCodePoint,CurrentCodeUnit,CurrentWorkChar);
  if Count>0 then begin
   dec(Count);
  end;
 end;

 while MarkStackPop(TopStack,MarkStackItem) do begin
  if ExecuteLevel(MarkStackItem.Char,Instruction^.y,MarkStackItem.CodePoint,MarkStackItem.CodeUnit,CodePoint,CodeUnit,Level+1) then begin
   CurrentWorkChar:=MarkStackItem.Char;
   CurrentCodePoint:=CodePoint;
   CurrentCodeUnit:=CodeUnit;
   result:=true;
   break;
  end;
 end;

 MarkStackPointer:=TopStack;
end;

function TBRREBacktrackingNFA.PtrGreedyRepeat(var CurrentWorkChar:longword;Instruction:PBRRERegExpNFAInstruction;var CurrentCodePoint,CurrentCodeUnit:longint;Count,Level:longint):boolean;
var CodePoint,CodeUnit,TopStack:longint;
    NextChar:longword;
    MarkStackItem:TBRREBacktrackingNFAMarkStackItem;
begin

 result:=false;

 TopStack:=MarkStackPointer;

 MarkStackPush(CurrentCodePoint,CurrentCodeUnit,CurrentWorkChar);

 while Count<>0 do begin
  NextChar:=CurrentWorkChar;
  if not PtrExecuteLevel(NextChar,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,CodePoint,CodeUnit,Level+1) then begin
   break;
  end;
  CurrentWorkChar:=NextChar;
  CurrentCodePoint:=CodePoint;
  CurrentCodeUnit:=CodeUnit;
  MarkStackPush(CurrentCodePoint,CurrentCodeUnit,CurrentWorkChar);
  if Count>0 then begin
   dec(Count);
  end;
 end;

 while MarkStackPop(TopStack,MarkStackItem) do begin
  if PtrExecuteLevel(MarkStackItem.Char,Instruction^.y,MarkStackItem.CodePoint,MarkStackItem.CodeUnit,CodePoint,CodeUnit,Level+1) then begin
   CurrentWorkChar:=MarkStackItem.Char;
   CurrentCodePoint:=CodePoint;
   CurrentCodeUnit:=CodeUnit;
   result:=true;
   break;
  end;
 end;

 MarkStackPointer:=TopStack;
end;

function TBRREBacktrackingNFA.Push(Instruction:PBRRERegExpNFAInstruction;CodePoint,CodeUnit:longint;WorkChar:longword):PBRRERegExpBacktrackingStackItem; {$IFDEF caninline}inline;{$ENDIF}
begin
 if BacktrackingStackPointer>MaxMatchLimitRecursion then begin
  RegExp.RaiseMatchLimitRecursionOverflow;
 end;
 if BacktrackingStackPointer>=length(BacktrackingStack) then begin
  SetLength(BacktrackingStack,BRRERoundUpToPowerOfTwo(BacktrackingStackPointer+1));
 end;
 result:=@BacktrackingStack[BacktrackingStackPointer];
 inc(BacktrackingStackPointer);
 result^.CurrentWorkChar:=WorkChar;
 result^.Instruction:=Instruction;
 result^.CurrentCodePoint:=CodePoint;
 result^.CurrentCodeUnit:=CodeUnit;
 result^.Arguments[0]:=0;
end;

function TBRREBacktrackingNFA.ExecuteLevel(var CurrentWorkChar:longword;Instruction:PBRRERegExpNFAInstruction;CurrentCodePoint,CurrentCodeUnit:longint;var OutCodePoint,OutCodeUnit:longint;Level:longint):boolean;
var NextChar,c1,c2,Argument:longword;
    i,j,g,h,x,y,TopStack:longint;
    OK:boolean;
    StackItem:PBRRERegExpBacktrackingStackItem;
begin
 result:=false;
 if Level>MaxMatchLimitStackRecursion then begin
  RegExp.RaiseMatchLimitStackRecursionOverflow;
 end else begin
  i:=-1;
  j:=-1;
  g:=-1;
  h:=-1;
  TopStack:=BacktrackingStackPointer;
  Push(Instruction,CurrentCodePoint,CurrentCodeUnit,CurrentWorkChar);
  while BacktrackingStackPointer>TopStack do begin
   dec(BacktrackingStackPointer);
   StackItem:=@BacktrackingStack[BacktrackingStackPointer];
   CurrentWorkChar:=StackItem^.CurrentWorkChar;
   Instruction:=StackItem^.Instruction;
   CurrentCodePoint:=StackItem^.CurrentCodePoint;
   CurrentCodeUnit:=StackItem^.CurrentCodeUnit;
   Argument:=StackItem^.Arguments[0];
   if CurrentWorkChar=$ffffffff then begin
    if (CurrentCodeUnit>1) and (CurrentCodeUnit<=(InputUnitsLength+1)) then begin
     if InputIsUTF8 then begin
      i:=CurrentCodeUnit;
      BRREUTF8Dec(InputStr,i);
      if i>0 then begin
       CurrentWorkChar:=BRREUTF8CodeUnitGetChar(InputStr,i);
      end else begin
       CurrentWorkChar:=$ffffffff;
      end;
     end else begin
      CurrentWorkChar:=byte(ansichar(InputStr[CurrentCodeUnit-1]));
     end;
    end else begin
     CurrentWorkChar:=$ffffffff;
    end;
   end;
   while assigned(Instruction) do begin
    case Instruction^.IDandOpcode and $ff of
     brreoCHAR:begin
      inc(MatchCounter);
      if MatchCounter>MaxMatchLimit then begin
       RegExp.RaiseMatchLimitOverflow;
      end;
      if CurrentCodeUnit<=InputUnitsLength then begin
       if InputIsUTF8 then begin
        CurrentWorkChar:=BRREUTF8CodeUnitGetCharAndInc(InputStr,CurrentCodeUnit);
       end else begin
        CurrentWorkChar:=byte(ansichar(InputStr[CurrentCodeUnit]));
        inc(CurrentCodeUnit);
       end;
       inc(CurrentCodePoint);
      end else begin
       CurrentWorkChar:=$ffffffff;
      end;
      if (CurrentWorkChar=$ffffffff) or not
         (((CurrentWorkChar<256) and (ansichar(byte(CurrentWorkChar)) in TBRRERegExpCharClass(pointer(Instruction^.Value)).Charset)) or
          ((CurrentWorkChar>255) and TBRRERegExpCharClass(pointer(Instruction^.Value)).Contains(CurrentWorkChar))) then begin
       result:=false;
       break;
      end;
      Instruction:=Instruction^.x;
     end;
     brreoCHARSINGLE:begin
      inc(MatchCounter);
      if MatchCounter>MaxMatchLimit then begin
       RegExp.RaiseMatchLimitOverflow;
      end;
      if CurrentCodeUnit<=InputUnitsLength then begin
       if InputIsUTF8 then begin
        CurrentWorkChar:=BRREUTF8CodeUnitGetCharAndInc(InputStr,CurrentCodeUnit);
       end else begin
        CurrentWorkChar:=byte(ansichar(InputStr[CurrentCodeUnit]));
        inc(CurrentCodeUnit);
       end;
       inc(CurrentCodePoint);
      end else begin
       CurrentWorkChar:=$ffffffff;
      end;
      if CurrentWorkChar<>ptruint(Instruction^.Value) then begin
       result:=false;
       break;
      end;
      Instruction:=Instruction^.x;
     end;
     brreoANY:begin
      if CurrentCodeUnit<=InputUnitsLength then begin
       if InputIsUTF8 then begin
        CurrentWorkChar:=BRREUTF8CodeUnitGetCharAndInc(InputStr,CurrentCodeUnit);
       end else begin
        CurrentWorkChar:=byte(ansichar(InputStr[CurrentCodeUnit]));
        inc(CurrentCodeUnit);
       end;
       inc(CurrentCodePoint);
       Instruction:=Instruction^.x;
      end else begin
       CurrentWorkChar:=$ffffffff;
       result:=false;
       break;
      end;
     end;
     brreoMATCH:begin
      result:=true;
      break;
     end;
     brreoFAIL:begin
      result:=false;
      break;
     end;
     brreoJMP:begin
      Instruction:=Instruction^.x;
     end;
     brreoSPLIT:begin
      Push(Instruction^.y,CurrentCodePoint,CurrentCodeUnit,CurrentWorkChar);
      Instruction:=Instruction^.x;
     end;
     brreoSPLITLONGEST:begin
      i:=-1;
      j:=-1;
      c1:=CurrentWorkChar;
      if ExecuteLevel(c1,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,x,y,Level+1) then begin
       i:=x;
       j:=y;
      end;

      g:=-1;
      h:=-1;
      c2:=CurrentWorkChar;
      if ExecuteLevel(c2,Instruction^.y,CurrentCodePoint,CurrentCodeUnit,x,y,Level+1) then begin
       g:=x;
       h:=y;
      end;

      result:=(i>=0) or (g>=0);
      if (i>=0) or (g>=0) then begin
       if i<g then begin
        CurrentWorkChar:=c2;
        CurrentCodePoint:=g;
        CurrentCodeUnit:=h;
       end else begin
        CurrentWorkChar:=c1;
        CurrentCodePoint:=g;
        CurrentCodeUnit:=h;
       end;
      end;
      break;
     end;
     brreoBEGIN:begin
      i:=Instruction^.Value;
      case Argument of
       0:begin
        StackItem:=Push(Instruction,CurrentCodePoint,CurrentCodeUnit,CurrentWorkChar);
        StackItem^.Arguments[0]:=1;
        StackItem^.Arguments[1]:=RegExpDataInstance.Captures[i].StartCodePoint;
        StackItem^.Arguments[2]:=RegExpDataInstance.Captures[i].StartCodeUnit;
        RegExpDataInstance.GlobalStartPositions[i].StartCodePoint:=CurrentCodePoint;
        RegExpDataInstance.GlobalStartPositions[i].StartCodeUnit:=CurrentCodeUnit;
        Instruction:=Instruction^.x;
       end;
       1:begin
        RegExpDataInstance.Captures[i].StartCodePoint:=StackItem^.Arguments[1];
        RegExpDataInstance.Captures[i].StartCodeUnit:=StackItem^.Arguments[2];
        result:=false;
        break;
       end;
      end;
     end;
     brreoEND:begin
      i:=Instruction^.Value;
      case Argument of
       0:begin
        StackItem:=Push(Instruction,CurrentCodePoint,CurrentCodeUnit,CurrentWorkChar);
        StackItem^.Arguments[0]:=1;
        StackItem^.Arguments[1]:=RegExpDataInstance.Captures[i].EndCodePoint;
        StackItem^.Arguments[2]:=RegExpDataInstance.Captures[i].EndCodeUnit;
        RegExpDataInstance.Captures[i].StartCodePoint:=RegExpDataInstance.GlobalStartPositions[i].StartCodePoint;
        RegExpDataInstance.Captures[i].StartCodeUnit:=RegExpDataInstance.GlobalStartPositions[i].StartCodeUnit;
        RegExpDataInstance.Captures[i].EndCodePoint:=CurrentCodePoint;
        RegExpDataInstance.Captures[i].EndCodeUnit:=CurrentCodeUnit;
        Instruction:=Instruction^.x;
       end;
       1:begin
        RegExpDataInstance.Captures[i].EndCodePoint:=StackItem^.Arguments[1];
        RegExpDataInstance.Captures[i].EndCodeUnit:=StackItem^.Arguments[2];
        result:=false;
        break;
       end;
      end;
     end;
     brreoBOML:begin
      if CurrentCodePoint=0 then begin
       Instruction:=Instruction^.x;
      end else begin
       if CurrentCodeUnit<=InputUnitsLength then begin
        if InputIsUTF8 then begin
         NextChar:=BRREUTF8CodeUnitGetChar(InputStr,CurrentCodeUnit);
        end else begin
         NextChar:=byte(ansichar(InputStr[CurrentCodeUnit]));
        end;
       end else begin
        NextChar:=$ffffffff;
       end;
       case NextChar of
        $000a,$00d,$2028,$2029:begin
         Instruction:=Instruction^.x;
        end;
        else begin
         result:=false;
         break;
        end;
       end;
      end;
     end;
     brreoEOML:begin
      if CurrentCodePoint>=InputPointsLength then begin
       Instruction:=Instruction^.x;
      end else begin
       if CurrentCodeUnit<=InputUnitsLength then begin
        if InputIsUTF8 then begin
         NextChar:=BRREUTF8CodeUnitGetChar(InputStr,CurrentCodeUnit);
        end else begin
         NextChar:=byte(ansichar(InputStr[CurrentCodeUnit]));
        end;
       end else begin
        NextChar:=$ffffffff;
       end;
       case NextChar of
        $000a,$00d,$2028,$2029:begin
         Instruction:=Instruction^.x;
        end;
        else begin
         result:=false;
         break;
        end;
       end;
      end;
     end;
     brreoBOSL:begin
      if CurrentCodePoint=0 then begin
       Instruction:=Instruction^.x;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoEOSL:begin
      if CurrentCodePoint>=InputPointsLength then begin
       Instruction:=Instruction^.x;
      end else begin
       break;
      end;
     end;
     brreoBOS:begin
      if CurrentCodePoint=0 then begin
       Instruction:=Instruction^.x;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoEOS:begin
      if CurrentCodePoint>=InputPointsLength then begin
       Instruction:=Instruction^.x;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoBRK:begin
      if CurrentCodeUnit<=InputUnitsLength then begin
       if InputIsUTF8 then begin
        NextChar:=BRREUTF8CodeUnitGetChar(InputStr,CurrentCodeUnit);
       end else begin
        NextChar:=byte(ansichar(InputStr[CurrentCodeUnit]));
       end;
      end else begin
       NextChar:=$ffffffff;
      end;          
      if RegExp.IsWordChar(NextChar)<>RegExp.IsWordChar(CurrentWorkChar) then begin
       Instruction:=Instruction^.x;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoNBRK:begin
      if CurrentCodeUnit<=InputUnitsLength then begin
       if InputIsUTF8 then begin
        NextChar:=BRREUTF8CodeUnitGetChar(InputStr,CurrentCodeUnit);
       end else begin
        NextChar:=byte(ansichar(InputStr[CurrentCodeUnit]));
       end;
      end else begin
       NextChar:=$ffffffff;
      end;
      if RegExp.IsWordChar(NextChar)=RegExp.IsWordChar(CurrentWorkChar) then begin
       Instruction:=Instruction^.x;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoEXACT:begin
      OK:=true;
      for i:=1 to Instruction^.Value do begin
       NextChar:=CurrentWorkChar;
       if not ExecuteLevel(NextChar,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,g,h,Level+1) then begin
        result:=false;
        OK:=false;
        break;
       end;
       CurrentWorkChar:=NextChar;
       CurrentCodePoint:=g;
       CurrentCodeUnit:=h;
      end;
      if not OK then begin
       break;
      end;
      Instruction:=Instruction^.y;
     end;
     brreoGREEDY:begin
      if Instruction^.Value>0 then begin
       result:=GreedyRepeat(CurrentWorkChar,Instruction,CurrentCodePoint,CurrentCodeUnit,Instruction^.Value,Level);
       break;
      end else begin
       Instruction:=Instruction^.y;
      end;
     end;
     brreoLAZY:begin
      i:=Instruction^.Value;
      repeat
       NextChar:=CurrentWorkChar;
       if ExecuteLevel(NextChar,Instruction^.y,CurrentCodePoint,CurrentCodeUnit,g,h,Level+1) then begin
        CurrentWorkChar:=NextChar;
        CurrentCodePoint:=g;
        CurrentCodeUnit:=h;
        result:=true;
        break;
       end;
       if i=0 then begin
        result:=false;
        break;
       end;
       NextChar:=CurrentWorkChar;
       if not ExecuteLevel(NextChar,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,g,h,Level+1) then begin
        result:=false;
        break;
       end;
       CurrentWorkChar:=NextChar;
       CurrentCodePoint:=g;
       CurrentCodeUnit:=h;
       if i>0 then begin
        dec(i);
       end;
      until false;
      break;
     end;
     brreoPOSSESSIVE:begin
      i:=Instruction^.Value;
      while i<>0 do begin
       NextChar:=CurrentWorkChar;
       if not ExecuteLevel(NextChar,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,g,h,Level+1) then begin
        break;
       end;
       CurrentWorkChar:=NextChar;
       CurrentCodePoint:=g;
       CurrentCodeUnit:=h;
       if i>0 then begin
        dec(i);
       end;
      end;
      Instruction:=Instruction^.y;
     end;
     brreoSTOP:begin
      NextChar:=CurrentWorkChar;
      if ExecuteLevel(NextChar,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,i,j,Level+1) then begin
       CurrentWorkChar:=NextChar;
       CurrentCodePoint:=i;
       CurrentCodeUnit:=j;
       Instruction:=Instruction^.y;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoBACKREF:begin
      OK:=false;
      g:=0;
      if ((Instruction^.Value>=0) and (Instruction^.Value<length(RegExpDataInstance.Captures))) and
         ((RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit>0) and (RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit<RegExpDataInstance.Captures[Instruction^.Value].EndCodeUnit)) then begin
       g:=RegExpDataInstance.Captures[Instruction^.Value].EndCodeUnit-RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit;
       if ((CurrentCodeUnit+g)-1)<=InputUnitsLength then begin
        OK:=true;
        i:=CurrentCodeUnit;
        j:=RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit;
        while g>0 do begin
         if InputStr[i]<>InputStr[j] then begin
          OK:=false;
          break;
         end;
         inc(i);
         inc(j);
         dec(g);
        end;
        OK:=OK and (g=0);
       end;
      end;
      if OK then begin
       inc(CurrentCodePoint,RegExpDataInstance.Captures[Instruction^.Value].EndCodePoint-RegExpDataInstance.Captures[Instruction^.Value].StartCodePoint);
       inc(CurrentCodeUnit,RegExpDataInstance.Captures[Instruction^.Value].EndCodeUnit-RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit);
       Instruction:=Instruction^.x;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoBACKREFI:begin
      OK:=false;
      if InputIsUTF8 then begin
       if ((Instruction^.Value>=0) and (Instruction^.Value<length(RegExpDataInstance.Captures))) and
         ((RegExpDataInstance.Captures[Instruction^.Value].StartCodePoint>=0) and (RegExpDataInstance.Captures[Instruction^.Value].StartCodePoint<RegExpDataInstance.Captures[Instruction^.Value].EndCodePoint)) then begin
        i:=CurrentCodeUnit;
        j:=RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit;
        g:=RegExpDataInstance.Captures[Instruction^.Value].EndCodePoint-RegExpDataInstance.Captures[Instruction^.Value].StartCodePoint;
        if ((CurrentCodePoint+g)-1)<InputPointsLength then begin
         OK:=true;
         while g>0 do begin
          if BRREUnicodeToUpper(BRREUTF8CodeUnitGetCharAndInc(InputStr,i))<>BRREUnicodeToUpper(BRREUTF8CodeUnitGetCharAndInc(InputStr,j)) then begin
           OK:=false;
           break;
          end;
          dec(g);
         end;
         OK:=OK and (g=0);
        end;
       end;
      end else begin
       if ((Instruction^.Value>=0) and (Instruction^.Value<length(RegExpDataInstance.Captures))) and
          ((RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit>0) and (RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit<RegExpDataInstance.Captures[Instruction^.Value].EndCodeUnit)) then begin
        i:=CurrentCodeUnit;
        j:=RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit;
        g:=RegExpDataInstance.Captures[Instruction^.Value].EndCodeUnit-RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit;
        if ((CurrentCodeUnit+g)-1)<=InputUnitsLength then begin
         OK:=true;
         while g>0 do begin
          if BRREUnicodeToUpper(byte(ansichar(InputStr[i])))<>BRREUnicodeToUpper(byte(ansichar(InputStr[j]))) then begin
           OK:=false;
           break;
          end;
          inc(i);
          inc(j);
          dec(g);
         end;
         OK:=OK and (g=0);
        end;
       end;
      end;
      if OK then begin
       inc(CurrentCodePoint,RegExpDataInstance.Captures[Instruction^.Value].EndCodePoint-RegExpDataInstance.Captures[Instruction^.Value].StartCodePoint);
       inc(CurrentCodeUnit,RegExpDataInstance.Captures[Instruction^.Value].EndCodeUnit-RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit);
       Instruction:=Instruction^.x;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoTESTBACKREF:begin
      if ((Instruction^.Value>=0) and (Instruction^.Value<length(RegExpDataInstance.Captures))) and
         ((RegExpDataInstance.Captures[Instruction^.Value].StartCodePoint>=0) and (RegExpDataInstance.Captures[Instruction^.Value].StartCodePoint<RegExpDataInstance.Captures[Instruction^.Value].EndCodePoint)) then begin
       Instruction:=Instruction^.x;
      end else begin
       Instruction:=Instruction^.y;
      end;
     end;
     brreoTEST:begin
      NextChar:=CurrentWorkChar;
      if ExecuteLevel(NextChar,pointer(ptruint(Instruction^.Value)),CurrentCodePoint,CurrentCodeUnit,i,j,Level+1) then begin
       Instruction:=Instruction^.x;
      end else begin
       Instruction:=Instruction^.y;
      end;
     end;
     brreoCONTINUE:begin
      CurrentCodePoint:=RegExpDataInstance.LastIndexCodePoint;
      CurrentCodeUnit:=RegExpDataInstance.LastIndexCodeUnit;
      Instruction:=Instruction^.x;
     end;
     brreoLOOKAHEADFALSE:begin
      NextChar:=CurrentWorkChar;
      if not ExecuteLevel(NextChar,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,i,j,Level+1) then begin
       Instruction:=Instruction^.y;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoLOOKAHEADTRUE:begin
      NextChar:=CurrentWorkChar;
      if ExecuteLevel(NextChar,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,i,j,Level+1) then begin
       Instruction:=Instruction^.y;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoLOOKBEHINDFALSE:begin
      OK:=false;
      i:=CurrentCodePoint;
      j:=CurrentCodeUnit;
      if InputIsUTF8 then begin
       while i>0 do begin
        dec(i);
        BRREUTF8Dec(InputStr,j);
        NextChar:=$ffffffff;
        if ExecuteLevel(NextChar,Instruction^.x,i,j,g,h,Level+1) then begin
         if g=CurrentCodePoint then begin
          OK:=true;
          break;
         end;
        end;
       end;
      end else begin
       while i>0 do begin
        dec(i);
        dec(j);
        NextChar:=$ffffffff;
        if ExecuteLevel(NextChar,Instruction^.x,i,j,g,h,Level+1) then begin
         if g=CurrentCodePoint then begin
          OK:=true;
          break;
         end;
        end;
       end;
      end;
      if not OK then begin
       Instruction:=Instruction^.y;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoLOOKBEHINDTRUE:begin
      OK:=false;
      i:=CurrentCodePoint;
      j:=CurrentCodeUnit;
      if InputIsUTF8 then begin
       while i>0 do begin
        dec(i);
        BRREUTF8Dec(InputStr,j);
        NextChar:=$ffffffff;
        if ExecuteLevel(NextChar,Instruction^.x,i,j,g,h,Level+1) then begin
         if g=CurrentCodePoint then begin
          OK:=true;
          break;
         end;
        end;
       end;
      end else begin
       while i>0 do begin
        dec(i);
        dec(j);
        NextChar:=$ffffffff;
        if ExecuteLevel(NextChar,Instruction^.x,i,j,g,h,Level+1) then begin
         if g=CurrentCodePoint then begin
          OK:=true;
          break;
         end;
        end;
       end;
      end;
      if OK then begin
       Instruction:=Instruction^.y;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoPARALLEL:begin
      result:=false;
      if TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).CanBitstateNFA then begin
       result:=RegExpDataInstance.RegExpPrograms[TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).ID].BitstateNFA.Search(InputStr,CurrentCodePoint,CurrentCodePoint,InputPointsLength-CurrentCodePoint,CurrentCodeUnit,CurrentCodeUnit);
       if not result then begin
        result:=RegExpDataInstance.RegExpPrograms[TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).ID].ParallelNFA.Search(InputStr,CurrentCodePoint,CurrentCodeUnit);
       end;
      end else begin
       result:=RegExpDataInstance.RegExpPrograms[TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).ID].ParallelNFA.Search(InputStr,CurrentCodePoint,CurrentCodeUnit);
      end;
      if result then begin
       Instruction:=Instruction^.x;
       if (CurrentCodeUnit>1) and (CurrentCodeUnit<=InputUnitsLength) then begin
        if InputIsUTF8 then begin
         i:=CurrentCodeUnit;
         BRREUTF8Dec(InputStr,i);
         if i>0 then begin
          CurrentWorkChar:=BRREUTF8CodeUnitGetChar(InputStr,i);
         end else begin
          CurrentWorkChar:=$ffffffff;
         end;
        end else begin
         CurrentWorkChar:=byte(ansichar(InputStr[CurrentCodeUnit-1]));
        end;
       end else begin
        CurrentWorkChar:=$ffffffff;
       end;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoONEPASS:begin
      if RegExpDataInstance.RegExpPrograms[TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).ID].OnePassNFA.Search(InputStr,CurrentCodePoint,CurrentCodeUnit) then begin
       Instruction:=Instruction^.x;
       if (CurrentCodeUnit>1) and (CurrentCodeUnit<=InputUnitsLength) then begin
        if InputIsUTF8 then begin
         i:=CurrentCodeUnit;
         BRREUTF8Dec(InputStr,i);
         if i>0 then begin
          CurrentWorkChar:=BRREUTF8CodeUnitGetChar(InputStr,i);
         end else begin
          CurrentWorkChar:=$ffffffff;
         end;
        end else begin
         CurrentWorkChar:=byte(ansichar(InputStr[CurrentCodeUnit-1]));
        end;
       end else begin
        CurrentWorkChar:=$ffffffff;
       end;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoFIXEDSTRINGSEARCH:begin
      if RegExpDataInstance.RegExpPrograms[TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).ID].FixedStringSearch.Search(InputStr,CurrentCodePoint,CurrentCodeUnit) then begin
       Instruction:=Instruction^.x;
       if (CurrentCodeUnit>1) and (CurrentCodeUnit<=InputUnitsLength) then begin
        if InputIsUTF8 then begin
         i:=CurrentCodeUnit;
         BRREUTF8Dec(InputStr,i);
         if i>0 then begin
          CurrentWorkChar:=BRREUTF8CodeUnitGetChar(InputStr,i);
         end else begin
          CurrentWorkChar:=$ffffffff;
         end;
        end else begin
         CurrentWorkChar:=byte(ansichar(InputStr[CurrentCodeUnit-1]));
        end;
       end else begin
        CurrentWorkChar:=$ffffffff;
       end;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoPREFIX:begin
      if RegExpProgram.AnchorBeginText and (CurrentCodeUnit<>1) then begin
       result:=false;
       break;
      end;
      if InputIsUTF8 then begin
       case RegExpProgram.FixedStringSearchMethod of
        brrefssmSHIFTOR:begin
         CurrentCodeUnit:=PosShiftOrUTF8(RegExpProgram.FixedString,InputStr,RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,CurrentCodePoint,CurrentCodeUnit);
         if CurrentCodeUnit=0 then begin
          result:=false;
          break;
         end;
        end;
  {     brrefssmBOYERMOORE:begin
         CurrentCodeUnit:=PosBoyerMooreUTF8(RegExpProgram.FixedString,InputStr,RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,RegExpProgram.FixedStringBoyerMooreNext,CurrentCodeUnit);
         if CurrentCodeUnit=0 then begin
          result:=false;
          break;
         end;
        end;}
        else {brrefssmBRUTEFORCE:}begin
         if length(RegExpProgram.FixedString)=1 then begin
          CurrentCodeUnit:=PosCharUTF8(RegExpProgram.FixedString[1],InputStr,CurrentCodePoint,CurrentCodeUnit);
         end else begin
          CurrentCodeUnit:=PosExUTF8(RegExpProgram.FixedString,InputStr,CurrentCodePoint,CurrentCodeUnit);
         end;
         if CurrentCodeUnit=0 then begin
          result:=false;
          break;
         end;
        end;
       end;
      end else begin
       case RegExpProgram.FixedStringSearchMethod of
        brrefssmSHIFTOR:begin
         CurrentCodeUnit:=PosShiftOr(RegExpProgram.FixedString,InputStr,RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,CurrentCodeUnit);
        end;
        brrefssmBOYERMOORE:begin
         CurrentCodeUnit:=PosBoyerMoore(RegExpProgram.FixedString,InputStr,RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,RegExpProgram.FixedStringBoyerMooreNext,CurrentCodeUnit);
        end;
        else {brrefssmBRUTEFORCE:}begin
         if length(RegExpProgram.FixedString)=1 then begin
          CurrentCodeUnit:=PosChar(RegExpProgram.FixedString[1],InputStr,CurrentCodeUnit);
         end else begin
          CurrentCodeUnit:=PosEx(RegExpProgram.FixedString,InputStr,CurrentCodeUnit);
         end;
        end;
       end;
       if CurrentCodeUnit=0 then begin
        result:=false;
        break;
       end;
       CurrentCodePoint:=CurrentCodeUnit-1;
      end;
      if CurrentCodeUnit<=InputUnitsLength then begin
       if InputIsUTF8 then begin
        i:=CurrentCodeUnit;
        BRREUTF8Inc(InputStr,i);
        Push(Instruction,CurrentCodePoint+1,i,0);
       end else begin
        Push(Instruction,CurrentCodePoint+1,CurrentCodeUnit+1,0);
       end;
      end;
      CurrentWorkChar:=$ffffffff;
      if (CurrentCodeUnit>1) and (CurrentCodeUnit<=InputUnitsLength) then begin
       if InputIsUTF8 then begin
        i:=CurrentCodeUnit;
        BRREUTF8Dec(InputStr,i);
        if i>0 then begin
         CurrentWorkChar:=BRREUTF8CodeUnitGetChar(InputStr,i);
        end else begin
         CurrentWorkChar:=$ffffffff;
        end;
       end else begin
        CurrentWorkChar:=byte(ansichar(InputStr[CurrentCodeUnit-1]));
       end;
      end else begin
       CurrentWorkChar:=$ffffffff;
      end;
      Instruction:=Instruction^.x;
     end;
     brreoPREFIXCHAR:begin
      repeat
       if RegExpProgram.AnchorBeginText and (CurrentCodeUnit<>1) then begin
        result:=false;
        break;
       end;
       NextChar:=CurrentWorkChar;
       if CurrentCodeUnit<=InputUnitsLength then begin
        if InputIsUTF8 then begin
         h:=CurrentCodeUnit;
         CurrentWorkChar:=BRREUTF8CodeUnitGetCharAndInc(InputStr,h);
        end else begin
         CurrentWorkChar:=byte(ansichar(InputStr[CurrentCodeUnit]));
         h:=CurrentCodeUnit+1;
        end;
        g:=CurrentCodePoint+1;
        if ((CurrentWorkChar<256) and (ansichar(byte(CurrentWorkChar)) in AllFirstCharsClass.Charset)) or
           ((CurrentWorkChar>255) and AllFirstCharsClass.Contains(CurrentWorkChar)) then begin
         Push(Instruction,g,h,CurrentWorkChar);
         CurrentWorkChar:=NextChar;
         break;
        end;
        CurrentCodePoint:=g;
        CurrentCodeUnit:=h;
       end else begin
        CurrentWorkChar:=$ffffffff;
        break;
       end;
      until false;
      Instruction:=Instruction^.x;
     end;
     brreoCALLOUT:begin
      case RegExpDataInstance.Callout(Instruction^.Value,pointer(@InputStr[1]),InputUnitsLength,InputPointsLength,CurrentCodeUnit,CurrentCodePoint,-1) of
       brrecorFAIL:begin
        result:=false;
        break;
       end;
       else begin
        Instruction:=Instruction^.x;
       end;
      end;
     end;
     brreoRECURSIVE:begin
      if not (assigned(Instruction^.x) and ExecuteLevel(CurrentWorkChar,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,CurrentCodePoint,CurrentCodeUnit,Level+1)) then begin
       result:=false;
       break;
      end;
      Instruction:=Instruction^.y;
     end;
     else begin
      result:=false;
      break;
     end;
    end;
    Argument:=0;
   end;
   if result then begin
    break;
   end;
  end;
  OutCodePoint:=CurrentCodePoint;
  OutCodeUnit:=CurrentCodeUnit;
  BacktrackingStackPointer:=TopStack;
 end;
end;

function TBRREBacktrackingNFA.PtrExecuteLevel(var CurrentWorkChar:longword;Instruction:PBRRERegExpNFAInstruction;CurrentCodePoint,CurrentCodeUnit:longint;var OutCodePoint,OutCodeUnit:longint;Level:longint):boolean;
var NextChar,c1,c2,Argument:longword;
    i,j,g,h,x,y,TopStack:longint;
    OK:boolean;
    StackItem:PBRRERegExpBacktrackingStackItem;
begin
 result:=false;
 if Level>MaxMatchLimitStackRecursion then begin
  RegExp.RaiseMatchLimitStackRecursionOverflow;
 end else begin
  i:=-1;
  j:=-1;
  g:=-1;
  h:=-1;
  TopStack:=BacktrackingStackPointer;
  Push(Instruction,CurrentCodePoint,CurrentCodeUnit,CurrentWorkChar);
  while BacktrackingStackPointer>TopStack do begin
   dec(BacktrackingStackPointer);
   StackItem:=@BacktrackingStack[BacktrackingStackPointer];
   CurrentWorkChar:=StackItem^.CurrentWorkChar;
   Instruction:=StackItem^.Instruction;
   CurrentCodePoint:=StackItem^.CurrentCodePoint;
   CurrentCodeUnit:=StackItem^.CurrentCodeUnit;
   Argument:=StackItem^.Arguments[0];
   if CurrentWorkChar=$ffffffff then begin
    if (CurrentCodeUnit>0) and (CurrentCodeUnit<=InputUnitsLength) then begin
     if InputIsUTF8 then begin
      i:=CurrentCodeUnit;
      BRREUTF8PtrDec(InputPtr,InputUnitsLength,i);
      if i>=0 then begin
       CurrentWorkChar:=BRREUTF8PtrCodeUnitGetChar(InputPtr,InputUnitsLength,i);
      end else begin
       CurrentWorkChar:=$ffffffff;
      end;
     end else begin
      CurrentWorkChar:=byte(ansichar(InputStr[CurrentCodeUnit-1]));
     end;
    end else begin
     CurrentWorkChar:=$ffffffff;
    end;
   end;
   while assigned(Instruction) do begin
    case Instruction^.IDandOpcode and $ff of
     brreoCHAR:begin
      inc(MatchCounter);
      if MatchCounter>MaxMatchLimit then begin
       RegExp.RaiseMatchLimitOverflow;
      end;
      if CurrentCodeUnit<InputUnitsLength then begin
       if InputIsUTF8 then begin
        CurrentWorkChar:=BRREUTF8PtrCodeUnitGetCharAndInc(InputPtr,InputUnitsLength,CurrentCodeUnit);
       end else begin
        CurrentWorkChar:=byte(ansichar(InputPtr[CurrentCodeUnit]));
        inc(CurrentCodeUnit);
       end;
       inc(CurrentCodePoint);
      end else begin
       CurrentWorkChar:=$ffffffff;
      end;
      if (CurrentWorkChar=$ffffffff) or not
         (((CurrentWorkChar<256) and (ansichar(byte(CurrentWorkChar)) in TBRRERegExpCharClass(pointer(Instruction^.Value)).Charset)) or
          ((CurrentWorkChar>255) and TBRRERegExpCharClass(pointer(Instruction^.Value)).Contains(CurrentWorkChar))) then begin
       result:=false;
       break;
      end;
      Instruction:=Instruction^.x;
     end;
     brreoCHARSINGLE:begin
      inc(MatchCounter);
      if MatchCounter>MaxMatchLimit then begin
       RegExp.RaiseMatchLimitOverflow;
      end;
      if CurrentCodeUnit<InputUnitsLength then begin
       if InputIsUTF8 then begin
        CurrentWorkChar:=BRREUTF8PtrCodeUnitGetCharAndInc(InputPtr,InputUnitsLength,CurrentCodeUnit);
       end else begin
        CurrentWorkChar:=byte(ansichar(InputPtr[CurrentCodeUnit]));
        inc(CurrentCodeUnit);
       end;
       inc(CurrentCodePoint);
      end else begin
       CurrentWorkChar:=$ffffffff;
      end;
      if CurrentWorkChar<>ptruint(Instruction^.Value) then begin
       result:=false;
       break;
      end;
      Instruction:=Instruction^.x;
     end;
     brreoANY:begin
      if CurrentCodeUnit<InputUnitsLength then begin
       if InputIsUTF8 then begin
        CurrentWorkChar:=BRREUTF8PtrCodeUnitGetCharAndInc(InputPtr,InputUnitsLength,CurrentCodeUnit);
       end else begin
        CurrentWorkChar:=byte(ansichar(InputPtr[CurrentCodeUnit]));
        inc(CurrentCodeUnit);
       end;
       inc(CurrentCodePoint);
       Instruction:=Instruction^.x;
      end else begin
       CurrentWorkChar:=$ffffffff;
       result:=false;
       break;
      end;
     end;
     brreoMATCH:begin
      result:=true;
      break;
     end;
     brreoFAIL:begin
      result:=false;
      break;
     end;
     brreoJMP:begin
      Instruction:=Instruction^.x;
     end;
     brreoSPLIT:begin
      Push(Instruction^.y,CurrentCodePoint,CurrentCodeUnit,CurrentWorkChar);
      Instruction:=Instruction^.x;
     end;
     brreoSPLITLONGEST:begin
      i:=-1;
      j:=-1;
      c1:=CurrentWorkChar;
      if PtrExecuteLevel(c1,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,x,y,Level+1) then begin
       i:=x;
       j:=y;
      end;

      g:=-1;
      h:=-1;
      c2:=CurrentWorkChar;
      if PtrExecuteLevel(c2,Instruction^.y,CurrentCodePoint,CurrentCodeUnit,x,y,Level+1) then begin
       g:=x;
       h:=y;
      end;

      result:=(i>=0) or (g>=0);
      if (i>=0) or (g>=0) then begin
       if i<g then begin
        CurrentWorkChar:=c2;
        CurrentCodePoint:=g;
        CurrentCodeUnit:=h;
       end else begin
        CurrentWorkChar:=c1;
        CurrentCodePoint:=g;
        CurrentCodeUnit:=h;
       end;
      end;
      break;
     end;
     brreoBEGIN:begin
      i:=Instruction^.Value;
      case Argument of
       0:begin
        StackItem:=Push(Instruction,CurrentCodePoint,CurrentCodeUnit,CurrentWorkChar);
        StackItem^.Arguments[0]:=1;
        StackItem^.Arguments[1]:=RegExpDataInstance.Captures[i].StartCodePoint;
        StackItem^.Arguments[2]:=RegExpDataInstance.Captures[i].StartCodeUnit;
        RegExpDataInstance.GlobalStartPositions[i].StartCodePoint:=CurrentCodePoint;
        RegExpDataInstance.GlobalStartPositions[i].StartCodeUnit:=CurrentCodeUnit;
        Instruction:=Instruction^.x;
       end;
       1:begin
        RegExpDataInstance.Captures[i].StartCodePoint:=StackItem^.Arguments[1];
        RegExpDataInstance.Captures[i].StartCodeUnit:=StackItem^.Arguments[2];
        result:=false;
        break;
       end;
      end;
     end;
     brreoEND:begin
      i:=Instruction^.Value;
      case Argument of
       0:begin
        StackItem:=Push(Instruction,CurrentCodePoint,CurrentCodeUnit,CurrentWorkChar);
        StackItem^.Arguments[0]:=1;
        StackItem^.Arguments[1]:=RegExpDataInstance.Captures[i].EndCodePoint;
        StackItem^.Arguments[2]:=RegExpDataInstance.Captures[i].EndCodeUnit;
        RegExpDataInstance.Captures[i].StartCodePoint:=RegExpDataInstance.GlobalStartPositions[i].StartCodePoint;
        RegExpDataInstance.Captures[i].StartCodeUnit:=RegExpDataInstance.GlobalStartPositions[i].StartCodeUnit;
        RegExpDataInstance.Captures[i].EndCodePoint:=CurrentCodePoint;
        RegExpDataInstance.Captures[i].EndCodeUnit:=CurrentCodeUnit;
        Instruction:=Instruction^.x;
       end;
       1:begin
        RegExpDataInstance.Captures[i].EndCodePoint:=StackItem^.Arguments[1];
        RegExpDataInstance.Captures[i].EndCodeUnit:=StackItem^.Arguments[2];
        result:=false;
        break;
       end;
      end;
     end;
     brreoBOML:begin
      if CurrentCodePoint=0 then begin
       Instruction:=Instruction^.x;
      end else begin
       if CurrentCodeUnit<InputUnitsLength then begin
        if InputIsUTF8 then begin
         NextChar:=BRREUTF8PtrCodeUnitGetChar(InputPtr,InputUnitsLength,CurrentCodeUnit);
        end else begin
         NextChar:=byte(ansichar(InputPtr[CurrentCodeUnit]));
        end;
       end else begin
        NextChar:=$ffffffff;
       end;
       case NextChar of
        $000a,$00d,$2028,$2029:begin
         Instruction:=Instruction^.x;
        end;
        else begin
         result:=false;
         break;
        end;
       end;
      end;
     end;
     brreoEOML:begin
      if CurrentCodePoint>=InputPointsLength then begin
       Instruction:=Instruction^.x;
      end else begin
       if CurrentCodeUnit<InputUnitsLength then begin
        if InputIsUTF8 then begin
         NextChar:=BRREUTF8PtrCodeUnitGetChar(InputPtr,InputUnitsLength,CurrentCodeUnit);
        end else begin
         NextChar:=byte(ansichar(InputPtr[CurrentCodeUnit]));
        end;
       end else begin
        NextChar:=$ffffffff;
       end;
       case NextChar of
        $000a,$00d,$2028,$2029:begin
         Instruction:=Instruction^.x;
        end;
        else begin
         result:=false;
         break;
        end;
       end;
      end;
     end;
     brreoBOSL:begin
      if CurrentCodePoint=0 then begin
       Instruction:=Instruction^.x;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoEOSL:begin
      if CurrentCodePoint>=InputPointsLength then begin
       Instruction:=Instruction^.x;
      end else begin
       break;
      end;
     end;
     brreoBOS:begin
      if CurrentCodePoint=0 then begin
       Instruction:=Instruction^.x;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoEOS:begin
      if CurrentCodePoint>=InputPointsLength then begin
       Instruction:=Instruction^.x;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoBRK:begin
      if CurrentCodeUnit<InputUnitsLength then begin
       if InputIsUTF8 then begin
        NextChar:=BRREUTF8PtrCodeUnitGetChar(InputPtr,InputUnitsLength,CurrentCodeUnit);
       end else begin
        NextChar:=byte(ansichar(InputPtr[CurrentCodeUnit]));
       end;
      end else begin
       NextChar:=$ffffffff;
      end;
      if RegExp.IsWordChar(NextChar)<>RegExp.IsWordChar(CurrentWorkChar) then begin
       Instruction:=Instruction^.x;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoNBRK:begin
      if CurrentCodeUnit<InputUnitsLength then begin
       if InputIsUTF8 then begin
        NextChar:=BRREUTF8PtrCodeUnitGetChar(InputPtr,InputUnitsLength,CurrentCodeUnit);
       end else begin
        NextChar:=byte(ansichar(InputPtr[CurrentCodeUnit]));
       end;
      end else begin
       NextChar:=$ffffffff;
      end;
      if RegExp.IsWordChar(NextChar)=RegExp.IsWordChar(CurrentWorkChar) then begin
       Instruction:=Instruction^.x;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoEXACT:begin
      OK:=true;
      for i:=1 to Instruction^.Value do begin
       NextChar:=CurrentWorkChar;
       if not PtrExecuteLevel(NextChar,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,g,h,Level+1) then begin
        result:=false;
        OK:=false;
        break;
       end;
       CurrentWorkChar:=NextChar;
       CurrentCodePoint:=g;
       CurrentCodeUnit:=h;
      end;
      if not OK then begin
       break;
      end;
      Instruction:=Instruction^.y;
     end;
     brreoGREEDY:begin
      if Instruction^.Value>0 then begin
       result:=PtrGreedyRepeat(CurrentWorkChar,Instruction,CurrentCodePoint,CurrentCodeUnit,Instruction^.Value,Level);
       break;
      end else begin
       Instruction:=Instruction^.y;
      end;
     end;
     brreoLAZY:begin
      i:=Instruction^.Value;
      repeat
       NextChar:=CurrentWorkChar;
       if PtrExecuteLevel(NextChar,Instruction^.y,CurrentCodePoint,CurrentCodeUnit,g,h,Level+1) then begin
        CurrentWorkChar:=NextChar;
        CurrentCodePoint:=g;
        CurrentCodeUnit:=h;
        result:=true;
        break;
       end;
       if i=0 then begin
        result:=false;
        break;
       end;
       NextChar:=CurrentWorkChar;
       if not PtrExecuteLevel(NextChar,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,g,h,Level+1) then begin
        result:=false;
        break;
       end;
       CurrentWorkChar:=NextChar;
       CurrentCodePoint:=g;
       CurrentCodeUnit:=h;
       if i>0 then begin
        dec(i);
       end;
      until false;
      break;
     end;
     brreoPOSSESSIVE:begin
      i:=Instruction^.Value;
      while i<>0 do begin
       NextChar:=CurrentWorkChar;
       if not PtrExecuteLevel(NextChar,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,g,h,Level+1) then begin
        break;
       end;
       CurrentWorkChar:=NextChar;
       CurrentCodePoint:=g;
       CurrentCodeUnit:=h;
       if i>0 then begin
        dec(i);
       end;
      end;
      Instruction:=Instruction^.y;
     end;
     brreoSTOP:begin
      NextChar:=CurrentWorkChar;
      if PtrExecuteLevel(NextChar,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,i,j,Level+1) then begin
       CurrentWorkChar:=NextChar;
       CurrentCodePoint:=i;
       CurrentCodeUnit:=j;
       Instruction:=Instruction^.y;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoBACKREF:begin
      OK:=false;
      g:=0;
      if ((Instruction^.Value>=0) and (Instruction^.Value<length(RegExpDataInstance.Captures))) and
         ((RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit>0) and (RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit<RegExpDataInstance.Captures[Instruction^.Value].EndCodeUnit)) then begin
       g:=RegExpDataInstance.Captures[Instruction^.Value].EndCodeUnit-RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit;
       if ((CurrentCodeUnit+g)-1)<InputUnitsLength then begin
        OK:=true;
        i:=CurrentCodeUnit;
        j:=RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit;
        while g>0 do begin
         if InputPtr[i]<>InputPtr[j] then begin
          OK:=false;
          break;
         end;
         inc(i);
         inc(j);
         dec(g);
        end;
        OK:=OK and (g=0);
       end;
      end;
      if OK then begin
       inc(CurrentCodePoint,RegExpDataInstance.Captures[Instruction^.Value].EndCodePoint-RegExpDataInstance.Captures[Instruction^.Value].StartCodePoint);
       inc(CurrentCodeUnit,RegExpDataInstance.Captures[Instruction^.Value].EndCodeUnit-RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit);
       Instruction:=Instruction^.x;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoBACKREFI:begin
      OK:=false;
      if InputIsUTF8 then begin
       if ((Instruction^.Value>=0) and (Instruction^.Value<length(RegExpDataInstance.Captures))) and
         ((RegExpDataInstance.Captures[Instruction^.Value].StartCodePoint>=0) and (RegExpDataInstance.Captures[Instruction^.Value].StartCodePoint<RegExpDataInstance.Captures[Instruction^.Value].EndCodePoint)) then begin
        i:=CurrentCodeUnit;
        j:=RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit;
        g:=RegExpDataInstance.Captures[Instruction^.Value].EndCodePoint-RegExpDataInstance.Captures[Instruction^.Value].StartCodePoint;
        if ((CurrentCodePoint+g)-1)<InputPointsLength then begin
         OK:=true;
         while g>0 do begin
          if BRREUnicodeToUpper(BRREUTF8PtrCodeUnitGetCharAndInc(InputPtr,InputUnitsLength,i))<>BRREUnicodeToUpper(BRREUTF8PtrCodeUnitGetCharAndInc(InputPtr,InputUnitsLength,j)) then begin
           OK:=false;
           break;
          end;
          dec(g);
         end;
         OK:=OK and (g=0);
        end;
       end;
      end else begin
       if ((Instruction^.Value>=0) and (Instruction^.Value<length(RegExpDataInstance.Captures))) and
          ((RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit>0) and (RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit<RegExpDataInstance.Captures[Instruction^.Value].EndCodeUnit)) then begin
        i:=CurrentCodeUnit;
        j:=RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit;
        g:=RegExpDataInstance.Captures[Instruction^.Value].EndCodeUnit-RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit;
        if ((CurrentCodeUnit+g)-1)<InputUnitsLength then begin
         OK:=true;
         while g>0 do begin
          if BRREUnicodeToUpper(byte(ansichar(InputPtr[i])))<>BRREUnicodeToUpper(byte(ansichar(InputPtr[j]))) then begin
           OK:=false;
           break;
          end;
          inc(i);
          inc(j);
          dec(g);
         end;
         OK:=OK and (g=0);
        end;
       end;
      end;
      if OK then begin
       inc(CurrentCodePoint,RegExpDataInstance.Captures[Instruction^.Value].EndCodePoint-RegExpDataInstance.Captures[Instruction^.Value].StartCodePoint);
       inc(CurrentCodeUnit,RegExpDataInstance.Captures[Instruction^.Value].EndCodeUnit-RegExpDataInstance.Captures[Instruction^.Value].StartCodeUnit);
       Instruction:=Instruction^.x;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoTESTBACKREF:begin
      if ((Instruction^.Value>=0) and (Instruction^.Value<length(RegExpDataInstance.Captures))) and
         ((RegExpDataInstance.Captures[Instruction^.Value].StartCodePoint>=0) and (RegExpDataInstance.Captures[Instruction^.Value].StartCodePoint<RegExpDataInstance.Captures[Instruction^.Value].EndCodePoint)) then begin
       Instruction:=Instruction^.x;
      end else begin
       Instruction:=Instruction^.y;
      end;
     end;
     brreoTEST:begin
      NextChar:=CurrentWorkChar;
      if PtrExecuteLevel(NextChar,pointer(ptruint(Instruction^.Value)),CurrentCodePoint,CurrentCodeUnit,i,j,Level+1) then begin
       Instruction:=Instruction^.x;
      end else begin
       Instruction:=Instruction^.y;
      end;
     end;
     brreoCONTINUE:begin
      CurrentCodePoint:=RegExpDataInstance.LastIndexCodePoint;
      CurrentCodeUnit:=RegExpDataInstance.LastIndexCodeUnit;
      Instruction:=Instruction^.x;
     end;
     brreoLOOKAHEADFALSE:begin
      NextChar:=CurrentWorkChar;
      if not PtrExecuteLevel(NextChar,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,i,j,Level+1) then begin
       Instruction:=Instruction^.y;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoLOOKAHEADTRUE:begin
      NextChar:=CurrentWorkChar;
      if PtrExecuteLevel(NextChar,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,i,j,Level+1) then begin
       Instruction:=Instruction^.y;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoLOOKBEHINDFALSE:begin
      OK:=false;
      i:=CurrentCodePoint;
      j:=CurrentCodeUnit;
      if InputIsUTF8 then begin
       while i>0 do begin
        dec(i);
        BRREUTF8PtrDec(InputPtr,InputUnitsLength,j);
        NextChar:=$ffffffff;
        if PtrExecuteLevel(NextChar,Instruction^.x,i,j,g,h,Level+1) then begin
         if g=CurrentCodePoint then begin
          OK:=true;
          break;
         end;
        end;
       end;
      end else begin
       while i>0 do begin
        dec(i);
        dec(j);
        NextChar:=$ffffffff;
        if PtrExecuteLevel(NextChar,Instruction^.x,i,j,g,h,Level+1) then begin
         if g=CurrentCodePoint then begin
          OK:=true;
          break;
         end;
        end;
       end;
      end;
      if not OK then begin
       Instruction:=Instruction^.y;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoLOOKBEHINDTRUE:begin
      OK:=false;
      i:=CurrentCodePoint;
      j:=CurrentCodeUnit;
      if InputIsUTF8 then begin
       while i>0 do begin
        dec(i);
        BRREUTF8PtrDec(InputPtr,InputUnitsLength,j);
        NextChar:=$ffffffff;
        if PtrExecuteLevel(NextChar,Instruction^.x,i,j,g,h,Level+1) then begin
         if g=CurrentCodePoint then begin
          OK:=true;
          break;
         end;
        end;
       end;
      end else begin
       while i>0 do begin
        dec(i);
        dec(j);
        NextChar:=$ffffffff;
        if PtrExecuteLevel(NextChar,Instruction^.x,i,j,g,h,Level+1) then begin
         if g=CurrentCodePoint then begin
          OK:=true;
          break;
         end;
        end;
       end;
      end;
      if OK then begin
       Instruction:=Instruction^.y;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoPARALLEL:begin
      if TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).CanBitstateNFA then begin
       result:=RegExpDataInstance.RegExpPrograms[TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).ID].BitstateNFA.PtrSearch(InputPtr,CurrentCodePoint,CurrentCodePoint,InputPointsLength-CurrentCodePoint,CurrentCodeUnit,CurrentCodeUnit);
       if not result then begin
        result:=RegExpDataInstance.RegExpPrograms[TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).ID].ParallelNFA.PtrSearch(InputPtr,CurrentCodePoint,CurrentCodeUnit);
       end;
      end else begin
       result:=RegExpDataInstance.RegExpPrograms[TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).ID].ParallelNFA.PtrSearch(InputPtr,CurrentCodePoint,CurrentCodeUnit);
      end;
      if result then begin
       Instruction:=Instruction^.x;
       if (CurrentCodeUnit>0) and (CurrentCodeUnit<InputUnitsLength) then begin
        if InputIsUTF8 then begin
         i:=CurrentCodeUnit;
         BRREUTF8PtrDec(InputPtr,InputUnitsLength,i);
         if i>=0 then begin
          CurrentWorkChar:=BRREUTF8PtrCodeUnitGetChar(InputPtr,InputUnitsLength,i);
         end else begin
          CurrentWorkChar:=$ffffffff;
         end;
        end else begin
         CurrentWorkChar:=byte(ansichar(InputPtr[CurrentCodeUnit-1]));
        end;
       end else begin
        CurrentWorkChar:=$ffffffff;
       end;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoONEPASS:begin
      if RegExpDataInstance.RegExpPrograms[TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).ID].OnePassNFA.PtrSearch(InputPtr,CurrentCodePoint,CurrentCodeUnit) then begin
       Instruction:=Instruction^.x;
       if (CurrentCodeUnit>0) and (CurrentCodeUnit<InputUnitsLength) then begin
        if InputIsUTF8 then begin
         i:=CurrentCodeUnit;
         BRREUTF8PtrDec(InputPtr,InputUnitsLength,i);
         if i>=0 then begin
          CurrentWorkChar:=BRREUTF8PtrCodeUnitGetChar(InputPtr,InputUnitsLength,i);
         end else begin
          CurrentWorkChar:=$ffffffff;
         end;
        end else begin
         CurrentWorkChar:=byte(ansichar(InputPtr[CurrentCodeUnit-1]));
        end;
       end else begin
        CurrentWorkChar:=$ffffffff;
       end;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoFIXEDSTRINGSEARCH:begin
      if RegExpDataInstance.RegExpPrograms[TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).ID].FixedStringSearch.PtrSearch(InputPtr,CurrentCodePoint,CurrentCodeUnit) then begin
       Instruction:=Instruction^.x;
       if (CurrentCodeUnit>0) and (CurrentCodeUnit<InputUnitsLength) then begin
        if InputIsUTF8 then begin
         i:=CurrentCodeUnit;
         BRREUTF8PtrDec(InputPtr,InputUnitsLength,i);
         if i>=0 then begin
          CurrentWorkChar:=BRREUTF8PtrCodeUnitGetChar(InputPtr,InputUnitsLength,i);
         end else begin
          CurrentWorkChar:=$ffffffff;
         end;
        end else begin
         CurrentWorkChar:=byte(ansichar(InputPtr[CurrentCodeUnit-1]));
        end;
       end else begin
        CurrentWorkChar:=$ffffffff;
       end;
      end else begin
       result:=false;
       break;
      end;
     end;
     brreoPREFIX:begin
      if RegExpProgram.AnchorBeginText and (CurrentCodeUnit<>0) then begin
       result:=false;
       break;
      end;
      if InputIsUTF8 then begin
       case RegExpProgram.FixedStringSearchMethod of
        brrefssmSHIFTOR:begin
         CurrentCodeUnit:=PtrPosShiftOrUTF8(RegExpProgram.FixedString,InputPtr,InputUnitsLength,RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,CurrentCodePoint,CurrentCodeUnit);
         if CurrentCodeUnit<0 then begin
          result:=false;
          break;
         end;
        end;
  {     brrefssmBOYERMOORE:begin
         CurrentCodeUnit:=PtrPosBoyerMooreUTF8(RegExpProgram.FixedString,InputPtr,InputUnitsLength,RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,RegExpProgram.FixedStringBoyerMooreNext,CurrentCodeUnit);
         if CurrentCodeUnit<0 then begin
          result:=false;
          break;
         end;
        end;}
        else {brrefssmBRUTEFORCE:}begin
         if length(RegExpProgram.FixedString)=1 then begin
          CurrentCodeUnit:=PtrPosCharUTF8(RegExpProgram.FixedString[1],InputPtr,InputUnitsLength,CurrentCodePoint,CurrentCodeUnit);
         end else begin
          CurrentCodeUnit:=PtrPosExUTF8(RegExpProgram.FixedString,InputPtr,InputUnitsLength,CurrentCodePoint,CurrentCodeUnit);
         end;
         if CurrentCodeUnit<0 then begin
          result:=false;
          break;
         end;
        end;
       end;
      end else begin
       case RegExpProgram.FixedStringSearchMethod of
        brrefssmSHIFTOR:begin
         CurrentCodeUnit:=PtrPosShiftOr(RegExpProgram.FixedString,InputPtr,InputUnitsLength,RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,CurrentCodeUnit);
        end;
        brrefssmBOYERMOORE:begin
         CurrentCodeUnit:=PtrPosBoyerMoore(RegExpProgram.FixedString,InputPtr,InputUnitsLength,RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,RegExpProgram.FixedStringBoyerMooreNext,CurrentCodeUnit);
        end;
        else {brrefssmBRUTEFORCE:}begin
         if length(RegExpProgram.FixedString)=1 then begin
          CurrentCodeUnit:=PtrPosChar(RegExpProgram.FixedString[1],InputPtr,InputUnitsLength,CurrentCodeUnit);
         end else begin
          CurrentCodeUnit:=PtrPosEx(RegExpProgram.FixedString,InputPtr,InputUnitsLength,CurrentCodeUnit);
         end;
        end;
       end;
       if CurrentCodeUnit<0 then begin
        result:=false;
        break;
       end;
       CurrentCodePoint:=CurrentCodeUnit;
      end;
      if CurrentCodeUnit<InputUnitsLength then begin
       if InputIsUTF8 then begin
        i:=CurrentCodeUnit;
        BRREUTF8Inc(InputPtr,i);
        Push(Instruction,CurrentCodePoint+1,i,0)
       end else begin
        Push(Instruction,CurrentCodePoint+1,CurrentCodeUnit+1,0);
       end;
      end;
      if (CurrentCodeUnit>0) and (CurrentCodeUnit<InputUnitsLength) then begin
       if InputIsUTF8 then begin
        i:=CurrentCodeUnit;
        BRREUTF8PtrDec(InputPtr,InputUnitsLength,i);
        if i>=0 then begin
         CurrentWorkChar:=BRREUTF8PtrCodeUnitGetChar(InputPtr,InputUnitsLength,i);
        end else begin
         CurrentWorkChar:=$ffffffff;
        end;
       end else begin
        CurrentWorkChar:=byte(ansichar(InputPtr[CurrentCodeUnit-1]));
       end;
      end else begin
       CurrentWorkChar:=$ffffffff;
      end;
      Instruction:=Instruction^.x;
     end;
     brreoPREFIXCHAR:begin
      repeat
       if RegExpProgram.AnchorBeginText and (CurrentCodeUnit<>0) then begin
        result:=false;
        break;
       end;
       NextChar:=CurrentWorkChar;
       if CurrentCodeUnit<InputUnitsLength then begin
        if InputIsUTF8 then begin
         h:=CurrentCodeUnit;
         CurrentWorkChar:=BRREUTF8PtrCodeUnitGetCharAndInc(InputPtr,InputUnitsLength,h);
        end else begin
         CurrentWorkChar:=byte(ansichar(InputPtr[CurrentCodeUnit]));
         h:=CurrentCodeUnit+1;
        end;
        g:=CurrentCodePoint+1;
        if ((CurrentWorkChar<256) and (ansichar(byte(CurrentWorkChar)) in AllFirstCharsClass.Charset)) or
           ((CurrentWorkChar>255) and AllFirstCharsClass.Contains(CurrentWorkChar)) then begin
         Push(Instruction,g,h,CurrentWorkChar);
         CurrentWorkChar:=NextChar;
         break;
        end;
        CurrentCodePoint:=g;
        CurrentCodeUnit:=h;
       end else begin
        CurrentWorkChar:=$ffffffff;
        break;
       end;
      until false;
      Instruction:=Instruction^.x;
     end;
     brreoCALLOUT:begin
      case RegExpDataInstance.Callout(Instruction^.Value,pointer(@InputPtr[0]),InputUnitsLength,InputPointsLength,CurrentCodeUnit,CurrentCodePoint,0) of
       brrecorFAIL:begin
        result:=false;
        break;
       end;
       else begin
        Instruction:=Instruction^.x;
       end;
      end;
     end;
     brreoRECURSIVE:begin
      if not (assigned(Instruction^.x) and PtrExecuteLevel(CurrentWorkChar,Instruction^.x,CurrentCodePoint,CurrentCodeUnit,CurrentCodePoint,CurrentCodeUnit,Level+1)) then begin
       result:=false;
       break;
      end;
      Instruction:=Instruction^.y;
     end;
     else begin
      result:=false;
      break;
     end;
    end;
    Argument:=0;
   end;
   if result then begin
    break;
   end;
  end;
  OutCodePoint:=CurrentCodePoint;
  OutCodeUnit:=CurrentCodeUnit;
  BacktrackingStackPointer:=TopStack;
 end;
end;

function TBRREBacktrackingNFA.Search(const Input:ansistring;var CodePoint,CodeUnit:longint;UseAnchoredStart:boolean):boolean;
var TempIndex,TempPosition:longint;
    CurrentWorkChar:longword;
begin
 TempIndex:=-1;
 TempPosition:=-1;
 CurrentWorkChar:=$ffffffff;
 try
  InputStr:=Input;
  InputPointsLength:=RegExpDataInstance.InputPointsLength;
  InputUnitsLength:=RegExpDataInstance.InputUnitsLength;
  InputIsUTF8:=RegExpDataInstance.InputIsUTF8;
  BacktrackingStackPointer:=0;
  MarkStackPointer:=0;
  MatchCounter:=0;
  AllFirstCharsClass:=RegExp.AllFirstCharsClass;
  MaxMatchLimit:=RegExp.MaxMatchLimit;
  MaxMatchLimitRecursion:=RegExp.MaxMatchLimitRecursion;
  MaxMatchLimitStackRecursion:=RegExp.MaxMatchLimitStackRecursion;
  if UseAnchoredStart then begin
   result:=ExecuteLevel(CurrentWorkChar,RegExpProgram.AnchoredStart,CodePoint,CodeUnit,TempIndex,TempPosition,0);
  end else begin
   result:=ExecuteLevel(CurrentWorkChar,RegExpProgram.Start,CodePoint,CodeUnit,TempIndex,TempPosition,0);
  end;
 finally
  AllFirstCharsClass:=nil;
  InputStr:='';
 end;
end;

function TBRREBacktrackingNFA.PtrSearch(const Input:pansichar;var CodePoint,CodeUnit:longint;UseAnchoredStart:boolean):boolean;
var TempIndex,TempPosition:longint;
    CurrentWorkChar:longword;
begin
 TempIndex:=-1;
 TempPosition:=-1;
 CurrentWorkChar:=$ffffffff;
 try
  InputPtr:=Input;
  InputPointsLength:=RegExpDataInstance.InputPointsLength;
  InputUnitsLength:=RegExpDataInstance.InputUnitsLength;
  InputIsUTF8:=RegExpDataInstance.InputIsUTF8;
  BacktrackingStackPointer:=0;
  MarkStackPointer:=0;
  MatchCounter:=0;
  AllFirstCharsClass:=RegExp.AllFirstCharsClass;
  MaxMatchLimit:=RegExp.MaxMatchLimit;
  MaxMatchLimitRecursion:=RegExp.MaxMatchLimitRecursion;
  MaxMatchLimitStackRecursion:=RegExp.MaxMatchLimitStackRecursion;
  if UseAnchoredStart then begin
   result:=PtrExecuteLevel(CurrentWorkChar,RegExpProgram.AnchoredStart,CodePoint,CodeUnit,TempIndex,TempPosition,0);
  end else begin
   result:=PtrExecuteLevel(CurrentWorkChar,RegExpProgram.Start,CodePoint,CodeUnit,TempIndex,TempPosition,0);
  end;
 finally
  AllFirstCharsClass:=nil;
  InputStr:='';
 end;
end;

constructor TBRRERegExpProgram.Create(ARegExp:TBRRERegExp);
begin
 inherited Create;
 RegExp:=ARegExp;
 ID:=0;
 Instructions:=nil;
 AnchoredInstructions:=nil;
 Start:=nil;
 AnchoredStart:=nil;
 AnchorStartInstruction:=nil;
 AnchorEndInstruction:=nil;
 Method:=brrepmBACKTRACKINGNFA;
 HasRootCapture:=false;
 IsGlobal:=false;
 IsOnePass:=false;
 CanBitstateNFA:=false;
 CanDFA:=false;
 FillChar(ByteMap,sizeof(TBRREByteMap),#0);
 FillChar(UnByteMap,sizeof(TBRREByteMap),#0);
 ByteCharSetMap:=[];
 ByteCount:=0;
 OnePassNodes:=nil;
 OnePassNodesCount:=0;
 OnePassStart:=nil;
 OnePassStateSize:=0;
 FixedString:='';
 FixedStringBoyerMooreNext:=nil;
 FixedStringIsPrefix:=false;
 AnchorBeginText:=false;
 AnchorBeginLine:=false;
 AnchorStart:=false;
 AnchorEnd:=false;
 SearchForFirstChars:=false;
 DFAMode:=brredmNONE;
 PrefilterRootNode:=nil;
 CharClassActions:=nil;
 RangeLow:=#$00;
 RangeHigh:=#$ff;
 HasRange:=false;
end;

destructor TBRRERegExpProgram.Destroy;
var CurrentCharClassAction,NextCharClassAction:PBRREOnePassStateCharClassAction;
begin
 if assigned(PrefilterRootNode) then begin
  PrefilterRootNode.Destroy;
  PrefilterRootNode:=nil;
 end;
 SetLength(Instructions,0);
 SetLength(AnchoredInstructions,0);
 if assigned(OnePassNodes) then begin
  FreeMem(OnePassNodes);
  OnePassNodes:=nil;
 end;
 CurrentCharClassAction:=CharClassActions;
 CharClassActions:=nil;
 while assigned(CurrentCharClassAction) do begin
  NextCharClassAction:=CurrentCharClassAction^.AllNext;
  Dispose(CurrentCharClassAction);
  CurrentCharClassAction:=NextCharClassAction;
 end;
 FixedString:='';
 SetLength(FixedStringBoyerMooreNext,0);
 RangeLow:='';
 RangeHigh:='';
 inherited Destroy;
end;

constructor TBRRERegExpDataInstance.Create(ARegExp:TBRRERegExp);
var Counter,Count:integer;
    RegExpProgram:TBRRERegExpProgram;
    RegExpProgramData:PBRRERegExpProgramDataInstance;
begin
 inherited Create;
 NextFree:=nil;
 RegExp:=ARegExp;
 Captures:=nil;
 GlobalStartPositions:=nil;
 LastIndexCodePoint:=0;
 LastIndexCodeUnit:=0;
 WorkCaptures:=nil;
 DFA:=nil;
 RegExpPrograms:=nil;
 CalloutCaptures:=nil;
 SetLength(Captures,RegExp.CountOfCaptures);
 SetLength(GlobalStartPositions,RegExp.CountOfCaptures);
 SetLength(RegExpPrograms,RegExp.RegExpPrograms.Count);
 for Counter:=0 to RegExp.RegExpPrograms.Count-1 do begin
  RegExpProgramData:=@RegExpPrograms[Counter];
  FillChar(RegExpProgramData^,SizeOf(TBRRERegExpProgramDataInstance),AnsiChar(#0));
  RegExpProgram:=RegExp.RegExpPrograms[Counter];
  RegExpProgramData:=@RegExpPrograms[Counter];
  if assigned(RegExpProgram) then begin
   case RegExpProgram.Method of
    brrepmPARALLELNFA,brrepmONEPASSNFA:begin
     case RegExpProgram.Method of
      brrepmPARALLELNFA:begin
       if not assigned(RegExpProgramData^.ParallelNFA) then begin
        RegExpProgramData^.ParallelNFA:=TBRREParallelNFA.Create(RegExpProgram,self,RegExpProgramData);
       end;
      end;
      brrepmONEPASSNFA:begin
       if not assigned(RegExpProgramData^.OnePassNFA) then begin
        RegExpProgramData^.OnePassNFA:=TBRREOnePassNFA.Create(RegExpProgram,self,RegExpProgramData);
       end;
      end;
     end;
     if RegExpProgram.CanBitstateNFA and not assigned(RegExpProgramData^.BitstateNFA) then begin
      RegExpProgramData^.BitstateNFA:=TBRREBitstateNFA.Create(RegExpProgram,self);
     end;
     if length(RegExpProgram.AnchoredInstructions)<length(RegExpProgram.Instructions) then begin
      Count:=length(RegExpProgram.Instructions)+1;
     end else begin
      Count:=length(RegExpProgram.AnchoredInstructions)+1;
     end;
     SetLength(RegExpProgramData^.InstructionGenerationIDs,Count);
     FillChar(RegExpProgramData^.InstructionGenerationIDs[0],Count*SizeOf(LongInt),AnsiChar(#0));
     RegExpProgramData^.GenerationID:=0;
    end;
    brrepmBACKTRACKINGNFA:begin
     if not assigned(RegExpProgramData^.BacktrackingNFA) then begin
      RegExpProgramData^.BacktrackingNFA:=TBRREBacktrackingNFA.Create(RegExpProgram,self,RegExpProgramData);
      if RegExpProgram.CanDFA then begin
       if length(RegExpProgram.AnchoredInstructions)<length(RegExpProgram.Instructions) then begin
        Count:=length(RegExpProgram.Instructions)+1;
       end else begin
        Count:=length(RegExpProgram.AnchoredInstructions)+1;
       end;
       SetLength(RegExpProgramData^.InstructionGenerationIDs,Count);
       FillChar(RegExpProgramData^.InstructionGenerationIDs[0],Count*SizeOf(LongInt),AnsiChar(#0));
       RegExpProgramData^.GenerationID:=0;
      end;
     end;
    end;
    brrepmFIXEDSTRINGSEARCH:begin
     if not assigned(RegExpProgramData^.FixedStringSearch) then begin
      RegExpProgramData^.FixedStringSearch:=TBRREFixedStringSearch.Create(RegExpProgram,self,RegExpProgramData);
     end;
    end;
   end;
  end;
 end;
end;

destructor TBRRERegExpDataInstance.Destroy;
var Counter:longint;
    RegExpProgramData:PBRRERegExpProgramDataInstance;
begin
 SetLength(Captures,0);
 SetLength(GlobalStartPositions,0);
 SetLength(WorkCaptures,0);
 for Counter:=0 to length(RegExpPrograms)-1 do begin
  RegExpProgramData:=@RegExpPrograms[Counter];
  SetLength(RegExpProgramData^.InstructionGenerationIDs,0);
  if assigned(RegExpProgramData^.FixedStringSearch) then begin
   RegExpProgramData^.FixedStringSearch.Free;
   RegExpProgramData^.FixedStringSearch:=nil;
  end;
  if assigned(RegExpProgramData^.OnePassNFA) then begin
   RegExpProgramData^.OnePassNFA.Free;
   RegExpProgramData^.OnePassNFA:=nil;
  end;
  if assigned(RegExpProgramData^.BitstateNFA) then begin
   RegExpProgramData^.BitstateNFA.Free;
   RegExpProgramData^.BitstateNFA:=nil;
  end;
  if assigned(RegExpProgramData^.ParallelNFA) then begin
   RegExpProgramData^.ParallelNFA.Free;
   RegExpProgramData^.ParallelNFA:=nil;
  end;
  if assigned(RegExpProgramData^.BacktrackingNFA) then begin
   RegExpProgramData^.BacktrackingNFA.Free;
   RegExpProgramData^.BacktrackingNFA:=nil;
  end;
 end;
 SetLength(RegExpPrograms,0);
 if assigned(DFA) then begin
  DFA.Free;
 end;
 if assigned(CalloutCaptures) then begin
  FreeMem(CalloutCaptures);
 end;
 inherited Destroy;
end;

procedure TBRRERegExpDataInstance.Update;
var Counter:integer;
begin
 for Counter:=0 to length(Captures)-1 do begin
  Captures[Counter].StartCodePoint:=brreoUNDEFINED;
  Captures[Counter].EndCodePoint:=brreoUNDEFINED;
  Captures[Counter].StartCodeUnit:=brreoUNDEFINED;
  Captures[Counter].EndCodeUnit:=brreoUNDEFINED;
 end;
 if assigned(RegExp.CalloutFunc) and not assigned(CalloutCaptures) then begin
  GetMem(CalloutCaptures,SizeOf(longint)*(RegExp.CountOfCaptures*4));
 end;
end;

function TBRRERegExpDataInstance.Callout(Number:longint;Subject:pansichar;SubjectUnitsLength,SubjectPointsLength,CodeUnit,CodePoint,CodeUnitOffset:longint):longint;
var CalloutBlock:TBRRECalloutBlock;
    Counter:longint;
    p:plongint;
begin
 if assigned(RegExp.CalloutFunc) then begin
  p:=pointer(@CalloutCaptures[0]);
  for Counter:=0 to RegExp.CountOfCaptures-1 do begin
   if Captures[Counter].StartCodeUnit>=0 then begin
    p^:=Captures[Counter].StartCodeUnit-CodeUnitOffset;
   end else begin
    p^:=Captures[Counter].StartCodeUnit;
   end;
   inc(p);
   if Captures[Counter].EndCodeUnit>=0 then begin
    p^:=Captures[Counter].EndCodeUnit-CodeUnitOffset;
   end else begin
    p^:=Captures[Counter].EndCodeUnit;
   end;
   inc(p);
   p^:=Captures[Counter].StartCodePoint;
   inc(p);
   p^:=Captures[Counter].EndCodePoint;
   inc(p);
  end;
  CalloutBlock.Version:=0;
  CalloutBlock.Instance:=RegExp;
  CalloutBlock.CalloutNumber:=Number;
  CalloutBlock.CalloutData:=RegExp.CalloutData;
  CalloutBlock.Subject:=Subject;
  CalloutBlock.SubjectUnitsLength:=SubjectUnitsLength;
  CalloutBlock.SubjectPointsLength:=SubjectPointsLength;
  CalloutBlock.Captures:=CalloutCaptures;
  CalloutBlock.CountCaptures:=RegExp.CountOfCaptures;
  CalloutBlock.CodeUnit:=CodeUnit-CodeUnitOffset;
  CalloutBlock.CodePoint:=CodePoint;
  result:=RegExp.CalloutFunc(@CalloutBlock);
 end else begin
  result:=brrecorOK;
 end;
end;

constructor TBRRERegExpCompiler.Create(ARegExp:TBRRERegExp);
begin
 inherited Create;
 RegExp:=ARegExp;
 AtEOF:=false;
 IsFirstChar:=false;
 CurrentChar:=0;
 SourceStr:='';
 SourcePos:=0;
 HasRecursive:=false;
 CaptureIdentifierHashMap:=ARegExp.CaptureIdentifierHashMap;
 CaptureIdentifiers:=ARegexp.CaptureIdentifiers;
 CaptureNodes:=TBRREPointerList.Create;
 CaptureInstructions:=TBRREPointerList.Create;
 FixupableCaptureIdentifierHashMap:=TBRREStringHashMap.Create;
 FixupableCaptures:=nil;
 CountFixupableCaptures:=0;
 Flags:=RegExp.Flags;
 RegExpIsUTF8:=RegExp.RegExpIsUTF8;
 CharClassesHashMap:=RegExp.CharClassesHashMap;
 CharClasses:=RegExp.CharClasses;
 AllFirstCharsClass:=RegExp.AllFirstCharsClass;
 AllCharsClass:=RegExp.AllCharsClass;
 AnyCharClass:=RegExp.AnyCharClass;
 Nodes:=nil;
 IsBacktracking:=false;
 HasBacktrackingParent:=false;
 RegExpProgram:=nil;
 Instruction:=nil;
 PrefixIsWholeRegExp:=false;
 Prefix:='';
end;

destructor TBRRERegExpCompiler.Destroy;
begin
 SourceStr:='';
 CaptureNodes.Destroy;
 CaptureInstructions.Destroy;
 FixupableCaptureIdentifierHashMap.Destroy;
 SetLength(FixupableCaptures,0);
 Prefix:='';
 inherited Destroy;
end;

function TBRRERegExpCompiler.GetFixupableCaptureIndex(const Identifier:ansistring):longint;
begin
 result:=FixupableCaptureIdentifierHashMap.GetValue(Identifier);
 if result<0 then begin
  result:=CountFixupableCaptures;
  inc(CountFixupableCaptures);
  if CountFixupableCaptures>=length(FixupableCaptures) then begin
   SetLength(FixupableCaptures,BRRERoundUpToPowerOfTwo(CountFixupableCaptures+1));
  end;
  FixupableCaptureIdentifierHashMap.SetValue(Identifier,result);
  FixupableCaptures[result].CaptureIndex:=-1;
  FixupableCaptures[result].Identifier:=Identifier;
 end;
end;

function TBRRERegExpCompiler.CreateCaptureSnapshot:TBRRERegExpCaptureSnapshot;
var i:longint;
begin
 FillChar(result,SizeOf(TBRRERegExpCaptureSnapshot),#0);
 result.Count:=RegExp.CountOfCaptures;
 SetLength(result.Values,RegExp.CountOfCaptures);
 for i:=0 to RegExp.CountOfCaptures-1 do begin
  result.Values[i]:=CaptureIdentifierHashMap.GetValue(AnsiString(IntToStr(i)));
 end;
 SetLength(result.Identifiers,CaptureIdentifiers.Count);
 SetLength(result.IdentifierValues,CaptureIdentifiers.Count);
 for i:=0 to CaptureIdentifiers.Count-1 do begin
  result.Identifiers[i]:=CaptureIdentifiers[i];
  result.IdentifierValues[i]:=CaptureIdentifierHashMap.GetValue(AnsiString(CaptureIdentifiers[i]));
 end;
 SetLength(result.Nodes,CaptureNodes.Count);
 for i:=0 to CaptureNodes.Count-1 do begin
  result.Nodes[i]:=CaptureNodes[i];
 end;
 SetLength(result.Instructions,CaptureInstructions.Count);
 for i:=0 to CaptureInstructions.Count-1 do begin
  result.Instructions[i]:=CaptureInstructions[i];
 end;
 result.FixupableCaptures:=copy(FixupableCaptures);
 result.CountFixupableCaptures:=CountFixupableCaptures;
end;

procedure TBRRERegExpCompiler.RestoreCaptureSnapshot(var CaptureSnapshot:TBRRERegExpCaptureSnapshot);
var i:longint;
begin
 CaptureIdentifierHashMap.Clear;
 CaptureNodes.Clear;
 CaptureInstructions.Clear;
 FixupableCaptureIdentifierHashMap.Clear;
 RegExp.CountOfCaptures:=CaptureSnapshot.Count;
 for i:=0 to length(CaptureSnapshot.Values)-1 do begin
  CaptureIdentifierHashMap.SetValue(AnsiString(IntToStr(i)),CaptureSnapshot.Values[i]);
 end;
 for i:=0 to length(CaptureSnapshot.Identifiers)-1 do begin
  CaptureIdentifiers.Add(CaptureSnapshot.Identifiers[i]);
  CaptureIdentifierHashMap.SetValue(AnsiString(CaptureSnapshot.Identifiers[i]),CaptureSnapshot.IdentifierValues[i]);
 enD;
 for i:=0 to length(CaptureSnapshot.Nodes)-1 do begin
  CaptureNodes.Add(CaptureSnapshot.Nodes[i]);
 end;
 for i:=0 to length(CaptureSnapshot.Instructions)-1 do begin
  CaptureInstructions.Add(CaptureSnapshot.Instructions[i]);
 end;
 FixupableCaptures:=copy(CaptureSnapshot.FixupableCaptures);
 CountFixupableCaptures:=CaptureSnapshot.CountFixupableCaptures;
 for i:=0 to CountFixupableCaptures-1 do begin
  FixupableCaptureIdentifierHashMap.SetValue(FixupableCaptures[i].Identifier,i);
 end;
 SetLength(CaptureSnapshot.Values,0);
 SetLength(CaptureSnapshot.Identifiers,0);
 SetLength(CaptureSnapshot.IdentifierValues,0);
 SetLength(CaptureSnapshot.Nodes,0);
 SetLength(CaptureSnapshot.Instructions,0);
 SetLength(CaptureSnapshot.FixupableCaptures,0);
end;

procedure TBRRERegExpCompiler.ReleaseCaptureSnapshot(var CaptureSnapshot:TBRRERegExpCaptureSnapshot);
begin
 SetLength(CaptureSnapshot.Values,0);
 SetLength(CaptureSnapshot.Identifiers,0);
 SetLength(CaptureSnapshot.IdentifierValues,0);
 SetLength(CaptureSnapshot.Nodes,0);
 SetLength(CaptureSnapshot.Instructions,0);
end;

procedure TBRRERegExpCompiler.ResetCaptures;
begin
 RegExp.CountOfCaptures:=1;
 CaptureIdentifierHashMap.Clear;
 CaptureNodes.Clear;
 CaptureInstructions.Clear;
 FixupableCaptureIdentifierHashMap.Clear;
 SetLength(FixupableCaptures,0);
 CountFixupableCaptures:=0;
 CaptureIdentifierHashMap.SetValue('wholematch',0);
 CaptureIdentifiers.Add('wholematch');
end;

function TBRRERegExpCompiler.NextChar:longword;
begin
 IsFirstChar:=false;
 if SourcePos<=length(SourceStr) then begin
  if RegExp.RegExpIsUTF8=brresuISUTF8 then begin
   CurrentChar:=BRREUTF8CodeUnitGetCharAndInc(SourceStr,SourcePos);
  end else begin
   CurrentChar:=byte(ansichar(SourceStr[SourcePos]));
   inc(SourcePos);
  end;
 end else begin
  CurrentChar:=0;
  AtEOF:=true;
 end;
 result:=CurrentChar;
end;

function TBRRERegExpCompiler.PeekNextChar:longword;
begin
 if SourcePos<=length(SourceStr) then begin
  if RegExp.RegExpIsUTF8=brresuISUTF8 then begin
   result:=BRREUTF8CodeUnitGetChar(SourceStr,SourcePos);
  end else begin
   result:=byte(ansichar(SourceStr[SourcePos]));
  end;
 end else begin
  result:=0;
 end;
end;

function TBRRERegExpCompiler.SkipFreespacingWhiteSpace:longword;
begin
 if (Flags and brrefFREESPACING)<>0 then begin
  while not AtEOF do begin
   case CurrentChar of
    $0009..$000d,$0020,$00a0,$1680,$180e,$2000..$200b,$2028..$2029,$202f,
    $205f,$3000,$fffe,$feff:begin
     NextChar;
    end;
    ord('#'):begin
     NextChar;
     while not AtEOF do begin
      case CurrentChar of
       $000a,$000d,$2028,$2029:begin
        while not AtEOF do begin
         case CurrentChar of
          $000a,$000d,$2028,$2029:begin
           NextChar;
          end
          else begin
           break;
          end;
         end;
        end;
        break;
       end;
       else begin
        NextChar;
       end;
      end;
     end;
     break;
    end;
    else begin
     break;
    end;
   end;
  end;
 end;
 result:=CurrentChar;
end;

procedure TBRRERegExpCompiler.SyntaxError;
begin
 raise EBRRESyntaxError.Create('Invalid regular expression');
end;

procedure TBRRERegExpCompiler.Expect(c:longword);
begin
 if AtEOF or (CurrentChar<>c) then begin
  SyntaxError;
 end;
 NextChar;
end;

function TBRRERegExpCompiler.ParseInteger:longint;
var OK:boolean;
begin
 result:=0;
 OK:=false;
 while (not AtEOF) and (CurrentChar>=ord('0')) and (CurrentChar<=ord('9')) do begin
  result:=(result*10)+longint(CurrentChar-ord('0'));
  OK:=true;
  NextChar;
 end;
 if not OK then begin
  SyntaxError;
 end;
end;

function TBRRERegExpCompiler.ParseHex:longword;
begin
 result:=0;
 if AtEOF then begin
  SyntaxError;
 end else begin
  case CurrentChar of
   ord('0')..ord('9'):begin
    result:=CurrentChar-ord('0');
   end;
   ord('a')..ord('f'):begin
    result:=(CurrentChar-ord('a'))+$a;
   end;
   ord('A')..ord('F'):begin
    result:=(CurrentChar-ord('A'))+$a;
   end;
   else begin
    SyntaxError;
   end;
  end;
  NextChar;
 end;
end;

function TBRRERegExpCompiler.ParseHexValue:longword;
var OK:boolean;
begin
 result:=0;
 OK:=false;
 while not AtEOF do begin
  case CurrentChar of
   ord('0')..ord('9'):begin
    result:=(result shl 4) or (CurrentChar-ord('0'));
    OK:=true;
    NextChar;
   end;
   ord('a')..ord('f'):begin
    result:=(result shl 4) or ((CurrentChar-ord('a'))+$a);
    OK:=true;
    NextChar;
   end;
   ord('A')..ord('F'):begin
    result:=(result shl 4) or ((CurrentChar-ord('A'))+$a);
    OK:=true;
    NextChar;
   end;
   else begin
    break;
   end;
  end;
 end;
 if not OK then begin
  SyntaxError;
 end;
end;

function TBRRERegExpCompiler.QuantifierIsNext:boolean;
var OldSourcePos:longint;
    OldCurrentChar:longword;
    OldAtEOF,OldIsFirstChar,HasDigits:boolean;
begin
 OldSourcePos:=SourcePos;
 OldCurrentChar:=CurrentChar;
 OldAtEOF:=AtEOF;
 OldIsFirstChar:=IsFirstChar;
 try
  result:=false;
  if (not AtEOF) and (CurrentChar=ord('{')) then begin
   NextChar;
   HasDigits:=false;
   while (not AtEOF) and ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) do begin
    HasDigits:=true;
    NextChar;
   end;
   if (not AtEOF) and (CurrentChar=ord(',')) then begin
    NextChar;
    while (not AtEOF) and ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) do begin
     HasDigits:=true;
     NextChar;
    end;
   end;
   if (not AtEOF) and (CurrentChar=ord('}')) then begin
    result:=HasDigits;
   end;
  end;
 finally
  SourcePos:=OldSourcePos;
  CurrentChar:=OldCurrentChar;
  AtEOF:=OldAtEOF;
  IsFirstChar:=OldIsFirstChar;
 end;
end;

function TBRRERegExpCompiler.TestClassPOSIXCharacterClass:boolean;
var OldSourcePos,OldCurrentChar,i:longword;
    OldAtEOF,OldIsFirstChar:boolean;
begin
 result:=false;
 OldSourcePos:=SourcePos;
 OldCurrentChar:=CurrentChar;
 OldAtEOF:=AtEOF;
 OldIsFirstChar:=IsFirstChar;
 try
  if (not AtEOF) and (CurrentChar=ord('[')) then begin
   NextChar;
   if (not AtEOF) and (CurrentChar=ord(':')) then begin
    NextChar;
    i:=0;
    while (not AtEOF) and ((CurrentChar>=ord('a')) and (CurrentChar<=ord('z'))) do begin
     inc(i);
     NextChar;
    end;
    if (i>0) and ((not AtEOF) and (CurrentChar=ord(':'))) then begin
     NextChar;
     if (not AtEOF) and (CurrentChar=ord(']')) then begin
      NextChar;
      result:=true;
     end;
    end;
   end;
  end;
 finally
  SourcePos:=OldSourcePos;
  CurrentChar:=OldCurrentChar;
  AtEOF:=OldAtEOF;
  IsFirstChar:=OldIsFirstChar;
 end;
end;

function TBRRERegExpCompiler.ParseClassPOSIXCharacterClass(var c:TBRRERegExpCharClass;CanBeAlreadyCanonicalized:boolean):boolean;
var Identifier:ansistring;
    IgnoreCase:boolean;
    LastSourcePos,UntilSourcePos:longint;
begin
 result:=false;
 if (not AtEOF) and (CurrentChar=ord('[')) then begin
  NextChar;
  if (not AtEOF) and (CurrentChar=ord(':')) then begin
   LastSourcePos:=SourcePos;
   UntilSourcePos:=SourcePos;
   NextChar;
   IgnoreCase:=CanBeAlreadyCanonicalized and ((Flags and brrefIGNORECASE)<>0);
   while (not AtEOF) and (((CurrentChar>=ord('a')) and (CurrentChar<=ord('z'))) or ((CurrentChar>=ord('A')) and (CurrentChar<=ord('Z')))) do begin
    UntilSourcePos:=SourcePos;
    NextChar;
   end;
   if (LastSourcePos<UntilSourcePos) and ((not AtEOF) and (CurrentChar=ord(':'))) then begin
    Identifier:=AnsiString(LowerCase(String(copy(SourceStr,LastSourcePos,UntilSourcePos-LastSourcePos))));
    NextChar;
    if (not AtEOF) and (CurrentChar=ord(']')) then begin
     NextChar;
     case Identifier[1] of
      'a':begin
       if Identifier='alnum' then begin
        c.AddRange(ord('a'),ord('z'),IgnoreCase);
        c.AddRange(ord('A'),ord('Z'),IgnoreCase);
        c.AddRange(ord('0'),ord('9'),IgnoreCase);
       end else if Identifier='alpha' then begin
        c.AddRange(ord('a'),ord('z'),IgnoreCase);
        c.AddRange(ord('A'),ord('Z'),IgnoreCase);
       end else if Identifier='ascii' then begin
        c.AddRange(0,$7f,IgnoreCase);
       end;
      end;
      'b':begin
       if Identifier='blank' then begin
        c.AddChar(ord(' '),IgnoreCase);
        c.AddChar(9,IgnoreCase);
       end;
      end;
      'c':begin
       if Identifier='cntrl' then begin
        c.AddRange(0,$1f,IgnoreCase);
        c.AddChar($7f,IgnoreCase);
       end;
      end;
      'd':begin
       if Identifier='Digits' then begin
        c.AddRange(ord('0'),ord('9'),IgnoreCase);
       end;
      end;
      'g':begin
       if Identifier='graph' then begin
        c.AddRange($21,$7e,IgnoreCase);
       end;
      end;
      'l':begin
       if Identifier='lower' then begin
        c.AddRange(ord('a'),ord('z'),IgnoreCase);
       end;
      end;
      'p':begin
       if Identifier='print' then begin
        c.AddRange($20,$7e,IgnoreCase);
       end else if Identifier='punct' then begin
        c.AddChar(ord('!'),IgnoreCase);
        c.AddChar(ord('"'),IgnoreCase);
        c.AddChar(ord('#'),IgnoreCase);
        c.AddChar(ord('$'),IgnoreCase);
        c.AddChar(ord('%'),IgnoreCase);
        c.AddChar(ord('&'),IgnoreCase);
        c.AddChar(ord(''''),IgnoreCase);
        c.AddChar(ord('('),IgnoreCase);
        c.AddChar(ord('!'),IgnoreCase);
        c.AddChar(ord(')'),IgnoreCase);
        c.AddChar(ord('*'),IgnoreCase);
        c.AddChar(ord('+'),IgnoreCase);
        c.AddChar(ord(','),IgnoreCase);
        c.AddChar(ord('\'),IgnoreCase);
        c.AddChar(ord('-'),IgnoreCase);
        c.AddChar(ord('.'),IgnoreCase);
        c.AddChar(ord('/'),IgnoreCase);
        c.AddChar(ord(':'),IgnoreCase);
        c.AddChar(ord(';'),IgnoreCase);
        c.AddChar(ord('<'),IgnoreCase);
        c.AddChar(ord('='),IgnoreCase);
        c.AddChar(ord('>'),IgnoreCase);
        c.AddChar(ord('?'),IgnoreCase);
        c.AddChar(ord('@'),IgnoreCase);
        c.AddChar(ord('['),IgnoreCase);
        c.AddChar(ord('^'),IgnoreCase);
        c.AddChar(ord('_'),IgnoreCase);
        c.AddChar(ord('`'),IgnoreCase);
        c.AddChar(ord('{'),IgnoreCase);
        c.AddChar(ord('|'),IgnoreCase);
        c.AddChar(ord('}'),IgnoreCase);
        c.AddChar(ord('~'),IgnoreCase);
       end;
      end;
      's':begin
       if Identifier='space' then begin
        c.AddRange($09,$0d,IgnoreCase);
        c.AddChar($20,IgnoreCase);
       end;
      end;
      'u':begin
       if Identifier='upper' then begin
        c.AddRange(ord('A'),ord('Z'),IgnoreCase);
       end;
      end;
      'w':begin
       if Identifier='word' then begin
        c.AddRange(ord('a'),ord('z'),IgnoreCase);
        c.AddRange(ord('A'),ord('Z'),IgnoreCase);
        c.AddRange(ord('0'),ord('9'),IgnoreCase);
        c.AddChar(ord(':'),IgnoreCase);
       end;
      end;
      'x':begin
       if Identifier='xdigits' then begin
        c.AddRange(ord('a'),ord('f'),IgnoreCase);
        c.AddRange(ord('A'),ord('F'),IgnoreCase);
        c.AddRange(ord('0'),ord('9'),IgnoreCase);
       end;
      end;
     end;
     result:=assigned(c.First);
    end;
   end;
  end;
 end;
end;

function TBRRERegExpCompiler.ParseClassEscapeUnicodeProperty(var c:TBRRERegExpCharClass;CanBeAlreadyCanonicalized:boolean):boolean;
var Identifier:ansistring;
    i:longword;
    IgnoreCase,IsNegative:boolean;
    f,LastSourcePos,UntilSourcePos:longint;
begin
 result:=false;
 if not AtEOF then begin
  IgnoreCase:=CanBeAlreadyCanonicalized and ((Flags and brrefIGNORECASE)<>0);
  case CurrentChar of
   ord('a')..ord('z'),ord('A')..ord('Z'):begin
    f:=BRREUnicodeClassHashMap.GetValue(BRREUTF32CharToUTF8(BRREUnicodeToLower(CurrentChar)));
    if f>=0 then begin
     NextChar;
     c.AddUnicodeCategory(f,IgnoreCase);
     result:=true;
    end;
   end;
   ord('{'):begin
    LastSourcePos:=SourcePos;
    NextChar;
    if (not AtEOF) and (CurrentChar=ord('^')) then begin
     LastSourcePos:=SourcePos;
     NextChar;
     IsNegative:=true;
     IgnoreCase:=false;
    end else begin
     IsNegative:=false;
    end;
    UntilSourcePos:=LastSourcePos;
    while (not AtEOF) and (BRREUnicodeIsIDPart(CurrentChar) or ((CurrentChar=ord('-')) or (CurrentChar=ord('_')))) do begin
     UntilSourcePos:=SourcePos;
     NextChar;
    end;
    if (LastSourcePos<UntilSourcePos) and ((not AtEOF) and (CurrentChar=ord('}'))) then begin
     Identifier:=BRREUTF8Lowercase(copy(SourceStr,LastSourcePos,UntilSourcePos-LastSourcePos));
     NextChar;
     f:=BRREUnicodeClassHashMap.GetValue(Identifier);
     if f>0 then begin
      c.AddUnicodeCategory(f,IgnoreCase);
     end else begin
      f:=BRREUnicodeScriptHashMap.GetValue(Identifier);
      if f>0 then begin
       c.AddUnicodeScript(f,IgnoreCase);
      end else begin
       f:=BRREUnicodeBlockHashMap.GetValue(Identifier);
       if f>0 then begin
        c.AddUnicodeBlock(f,IgnoreCase);
       end else begin
        case Identifier[1] of
         'a':begin
          if Identifier='alnum' then begin
           c.AddUnicodeCategory((1 shl BRREUnicodeCategoryLu) or (1 shl BRREUnicodeCategoryLl) or (1 shl BRREUnicodeCategoryLt) or (1 shl BRREUnicodeCategoryNd),IgnoreCase);
          end else if Identifier='alpha' then begin
           c.AddUnicodeCategory((1 shl BRREUnicodeCategoryLu) or (1 shl BRREUnicodeCategoryLl) or (1 shl BRREUnicodeCategoryLt),IgnoreCase);
          end else if Identifier='ascii' then begin
           c.AddRange(0,$7f,IgnoreCase);
          end;
         end;
         'b':begin
          if Identifier='blank' then begin
           c.AddUnicodeCategory(1 shl BRREUnicodeCategoryZs,IgnoreCase);
           c.AddChar(9,IgnoreCase);
          end;
         end;
         'c':begin
          if Identifier='cntrl' then begin
           c.AddUnicodeCategory(1 shl BRREUnicodeCategoryCc,IgnoreCase);
          end;
         end;
         'd':begin
          if Identifier='digits' then begin
           c.AddUnicodeCategory(1 shl BRREUnicodeCategoryNd,IgnoreCase);
          end;
         end;
         'g':begin
          if Identifier='graph' then begin
           c.AddUnicodeCategory((1 shl BRREUnicodeCategoryZs) or (1 shl BRREUnicodeCategoryZl) or (1 shl BRREUnicodeCategoryZp) or (1 shl BRREUnicodeCategoryCc) or (1 shl BRREUnicodeCategoryCf) or (1 shl BRREUnicodeCategoryCo) or (1 shl BRREUnicodeCategoryCs) or (1 shl BRREUnicodeCategoryCn),IgnoreCase);
          end;
         end;
         'i':begin
          if Identifier='inbasiclatin' then begin
           c.AddRange(0,127,IgnoreCase);
          end else if ((Identifier='inno_block') or (Identifier='isno_block')) or ((Identifier='innoblock') or (Identifier='isnoblock')) then begin
           for i:=0 to ts.Core.BRREunicode.BRREUnicodeBlockCount-1 do begin
            c.AddUnicodeBlock(i,false);
           end;
           c.Invert;
           c.Inverted:=false;
           if IgnoreCase then begin
            c.Canonicalize;
           end;
          end;
         end;
         'l':begin
          if Identifier='lower' then begin
           c.AddUnicodeCategory(1 shl BRREUnicodeCategoryLl,IgnoreCase);
          end else if (Identifier='l&') or (Identifier='letter&') then begin
           c.AddUnicodeCategory((1 shl BRREUnicodeCategoryLu) or (1 shl BRREUnicodeCategoryLl) or (1 shl BRREUnicodeCategoryLt),IgnoreCase);
          end;
         end;
         'p':begin
          if Identifier='print' then begin
           c.AddUnicodeCategory((1 shl BRREUnicodeCategoryCc) or (1 shl BRREUnicodeCategoryCf) or (1 shl BRREUnicodeCategoryCo) or (1 shl BRREUnicodeCategoryCs) or (1 shl BRREUnicodeCategoryCn),IgnoreCase);
          end else if Identifier='punct' then begin
           c.AddUnicodeCategory((1 shl BRREUnicodeCategoryPd) or (1 shl BRREUnicodeCategoryPs) or (1 shl BRREUnicodeCategoryPe) or (1 shl BRREUnicodeCategoryPc) or (1 shl BRREUnicodeCategoryPo),IgnoreCase);
          end;
         end;
         's':begin
          if Identifier='space' then begin
           c.AddUnicodeCategory((1 shl BRREUnicodeCategoryZs) or (1 shl BRREUnicodeCategoryZl) or (1 shl BRREUnicodeCategoryZp),IgnoreCase);
           c.AddRange($0009,$000d,IgnoreCase);
          end;
         end;
         'u':begin
          if Identifier='upper' then begin
           c.AddUnicodeCategory(1 shl BRREUnicodeCategoryLu,IgnoreCase);
          end;
         end;
         'w':begin
          if Identifier='word' then begin
           c.AddUnicodeCategory((1 shl BRREUnicodeCategoryLu) or (1 shl BRREUnicodeCategoryLl) or (1 shl BRREUnicodeCategoryLt) or (1 shl BRREUnicodeCategoryLm) or (1 shl BRREUnicodeCategoryLo) or (1 shl BRREUnicodeCategoryNd) or (1 shl BRREUnicodeCategoryNl) or (1 shl BRREUnicodeCategoryNo) or (1 shl BRREUnicodeCategoryPc),IgnoreCase);
          end;
         end;
         'x':begin
          if Identifier='xdigits' then begin
           c.AddRange(ord('a'),ord('f'),IgnoreCase);
           c.AddRange(ord('A'),ord('F'),IgnoreCase);
           c.AddRange(ord('0'),ord('9'),IgnoreCase);
          end;
         end;
        end;
       end;
      end;
     end;
     if IsNegative and assigned(c.First) then begin
      c.Invert;
      c.Inverted:=false;
      if CanBeAlreadyCanonicalized and ((Flags and brrefIGNORECASE)<>0) then begin
       c.Canonicalize;
      end;
     end;
     result:=true;
    end;
   end;
  end;
 end;
end;

function TBRRERegExpCompiler.ParseClassEscape(CanBeAlreadyCanonicalized:boolean):TBRRERegExpCharClass;
var i:longint;
    ch:longword;
    IgnoreCase:boolean;
begin
 result:=nil;
 try
  IgnoreCase:=CanBeAlreadyCanonicalized and ((Flags and brrefIGNORECASE)<>0);
  result:=TBRRERegExpCharClass.Create(RegExp);
  if (CurrentChar>=ord('0')) and (CurrentChar<=ord('9')) then begin
   i:=0;
   while (not AtEOF) and (CurrentChar>=ord('0')) and (CurrentChar<=ord('9')) do begin
    i:=(i*10)+longint(CurrentChar-ord('0'));
    NextChar;
   end;
   if i<>0 then begin
    SyntaxError;
   end;
   result.AddChar(i,IgnoreCase);
   result.Canonicalized:=IgnoreCase;
   exit;
  end;
  ch:=CurrentChar;
  NextChar;
  case ch of
   ord('a'):begin
    result.AddChar($0007);
    result.Canonicalized:=true;
   end;
   ord('b'):begin
    result.AddChar($0008);
    result.Canonicalized:=true;
   end;
   ord('B'):begin
    result.AddChar(ord('\'));
    result.Canonicalized:=true;
   end;
   ord('t'):begin
    result.AddChar($0009);
    result.Canonicalized:=true;
   end;
   ord('n'):begin
    result.AddChar($000a);
    result.Canonicalized:=true;
   end;
   ord('v'):begin
    result.AddChar($000b);
    result.Canonicalized:=true;
   end;
   ord('f'):begin
    result.AddChar($000c);
    result.Canonicalized:=true;
   end;
   ord('r'):begin
    result.AddChar($000d);
    result.Canonicalized:=true;
   end;
   ord('e'):begin
    result.AddChar($001b);
    result.Canonicalized:=true;
   end;
   ord('d'):begin
    if RegExpIsUTF8<>brresuNOUTF8 then begin
     for i:=0 to length(BRREUnicodeCharRangeClasses[brreucrDIGITS])-1 do begin
      result.AddRange(BRREUnicodeCharRangeClasses[brreucrDIGITS,i,0],BRREUnicodeCharRangeClasses[brreucrDIGITS,i,1]);
     end;
    end else begin
     result.AddRange(ord('0'),ord('9'));
    end;
    result.Canonicalized:=true;
   end;
   ord('D'):begin
    if RegExpIsUTF8<>brresuNOUTF8 then begin
     for i:=0 to length(BRREUnicodeCharRangeClasses[brreucrDIGITS])-1 do begin
      result.AddRange(BRREUnicodeCharRangeClasses[brreucrDIGITS,i,0],BRREUnicodeCharRangeClasses[brreucrDIGITS,i,1]);
     end;
    end else begin
     result.AddRange(ord('0'),ord('9'));
    end;
    result.Invert;
    result.Inverted:=false;
    result.Canonicalized:=true;
   end;
   ord('w'):begin
    if RegExpIsUTF8<>brresuNOUTF8 then begin
     for i:=0 to length(BRREUnicodeCharRangeClasses[brreucrWORDS])-1 do begin
      result.AddRange(BRREUnicodeCharRangeClasses[brreucrWORDS,i,0],BRREUnicodeCharRangeClasses[brreucrWORDS,i,1]);
     end;
    end else begin
     result.AddRange(ord('a'),ord('z'));
     result.AddRange(ord('A'),ord('Z'));
     result.AddRange(ord('0'),ord('9'));
     result.AddChar(ord('_'));
    end;
    result.Canonicalized:=true;
   end;
   ord('W'):begin
    if RegExpIsUTF8<>brresuNOUTF8 then begin
     for i:=0 to length(BRREUnicodeCharRangeClasses[brreucrWORDS])-1 do begin
      result.AddRange(BRREUnicodeCharRangeClasses[brreucrWORDS,i,0],BRREUnicodeCharRangeClasses[brreucrWORDS,i,1]);
     end;
    end else begin
     result.AddRange(ord('a'),ord('z'));
     result.AddRange(ord('A'),ord('Z'));
     result.AddRange(ord('0'),ord('9'));
     result.AddChar(ord('_'));
    end;
    result.Invert;
    result.Inverted:=false;
    result.Canonicalized:=true;
   end;
   ord('s'):begin
    if RegExpIsUTF8<>brresuNOUTF8 then begin
     for i:=0 to length(BRREUnicodeCharRangeClasses[brreucrWHITESPACES])-1 do begin
      result.AddRange(BRREUnicodeCharRangeClasses[brreucrWHITESPACES,i,0],BRREUnicodeCharRangeClasses[brreucrWHITESPACES,i,1]);
     end;
    end else begin
     result.AddRange($0009,$000d);
     result.AddChar($0020);
    end;
    result.Canonicalized:=true;
   end;
   ord('S'):begin
    if RegExpIsUTF8<>brresuNOUTF8 then begin
     for i:=0 to length(BRREUnicodeCharRangeClasses[brreucrWHITESPACES])-1 do begin
      result.AddRange(BRREUnicodeCharRangeClasses[brreucrWHITESPACES,i,0],BRREUnicodeCharRangeClasses[brreucrWHITESPACES,i,1]);
     end;
    end else begin
     result.AddRange($0009,$000d);
     result.AddChar($0020);
    end;
    result.Invert;
    result.Inverted:=false;
    result.Canonicalized:=true;
   end;
   ord('p'):begin
    if not ParseClassEscapeUnicodeProperty(result,CanBeAlreadyCanonicalized) then begin
     SyntaxError;
    end;
    result.Canonicalized:=true;
   end;
   ord('P'):begin
    if not ParseClassEscapeUnicodeProperty(result,CanBeAlreadyCanonicalized) then begin
     SyntaxError;
    end;
    result.Invert;
    result.Inverted:=false;
    result.Canonicalized:=true;
   end;
   ord('c'):begin
    if AtEOF then begin
     SyntaxError;
    end;
    ch:=CurrentChar;
    NextChar;
    if ((ch>=ord('a')) and (ch<=ord('z'))) or ((ch>=ord('A')) and (ch<=ord('Z'))) then begin
     case ch of
      ord('a')..ord('z'):begin
       result.AddChar(ch-ord('a'),IgnoreCase);
      end;
      ord('A')..ord('Z'):begin
       result.AddChar(ch-ord('A'),IgnoreCase);
      end;
     end;
     result.Canonicalized:=IgnoreCase;
    end else begin
     SyntaxError;
    end;
   end;
   ord('x'):begin
    case CurrentChar of
     ord('{'):begin
      NextChar;
      ch:=ParseHexValue;
      Expect(ord('}'));
     end;
     else begin
      ch:=ParseHex;
      ch:=(ch shl 4) or ParseHex;
     end;
    end;
    result.AddChar(ch,IgnoreCase);
    result.Canonicalized:=IgnoreCase;
   end;
   ord('u'):begin
    ch:=ParseHex;
    ch:=(ch shl 4) or ParseHex;
    ch:=(ch shl 4) or ParseHex;
    ch:=(ch shl 4) or ParseHex;
    result.AddChar(ch,IgnoreCase);
    result.Canonicalized:=IgnoreCase;
   end;
   ord('U'):begin
    result.AddChar(ParseHexValue,IgnoreCase);
    result.Canonicalized:=IgnoreCase;
   end;
   ord('Q'):begin
    while not AtEOF do begin
     if CurrentChar=ord('\') then begin
      NextChar;
      if (not EOF) and (CurrentChar=ord('E')) then begin
       NextChar;
       break;
      end else begin
       result.AddChar(ord('\'),IgnoreCase);
      end;
     end else begin
      result.AddChar(CurrentChar,IgnoreCase);
      NextChar;
     end;
    end;
    result.Canonicalized:=IgnoreCase;
   end;
   else begin
    result.AddChar(ch,IgnoreCase);
    result.Canonicalized:=IgnoreCase;
   end;
  end;
 except
  FreeAndNil(result);
  raise;
 end;
end;

function TBRRERegExpCompiler.ParseClassAtom:TBRRERegExpCharClass;
begin
 if AtEOF then begin
  SyntaxError;
 end;
 result:=nil;
 try
  if CurrentChar=ord('\') then begin
   NextChar;
   result:=ParseClassEscape(false);
  end else if TestClassPOSIXCharacterClass then begin
   result:=TBRRERegExpCharClass.Create(RegExp);
   if not ParseClassPOSIXCharacterClass(result,false) then begin
    SyntaxError;
   end;
  end else begin
   result:=TBRRERegExpCharClass.Create(RegExp);
   result.AddChar(CurrentChar);
   NextChar;
  end;
 except
  FreeAndNil(result);
  raise;
 end;
end;

function TBRRERegExpCompiler.ParseCharacterClass:TBRRERegExpCharClass;
var InvertFlag:boolean;
    a,b,c:TBRRERegExpCharClass;
    CharNext:longword;
begin
 result:=nil;
 try
  a:=nil;
  b:=nil;
  try
   result:=TBRRERegExpCharClass.Create(RegExp);
   Expect(ord('['));
   InvertFlag:=(not AtEOF) and (CurrentChar=ord('^'));
   if InvertFlag then begin
    NextChar;
   end;
   while (not AtEOF) and (CurrentChar<>ord(']')) do begin
    CharNext:=PeekNextChar;
    if (not AtEOF) and ((CurrentChar=ord('-')) and ((CharNext=ord('[')) or (CharNext=ord(']')))) then begin
     NextChar;
     if CharNext=ord(']') then begin
      a:=TBRRERegExpCharClass.Create(Regexp);
      a.AddChar(ord('-'));
     end else begin
      a:=ParseCharacterClass;
      b:=TBRRERegExpCharClass.Create(RegExp);
      if b.Subtraction(result,a) then begin
       FreeAndNil(result);
       result:=b;
       b:=nil;
      end else begin
       FreeAndNil(b);
      end;
      FreeAndNil(a);
      if CurrentChar<>ord(']') then begin
       SyntaxError;
      end;
      continue;
     end;
    end else begin
     a:=ParseClassAtom;
    end;
    if (not AtEOF) and (CurrentChar=ord('-')) then begin
     NextChar;
     if (not AtEOF) and ((CurrentChar=ord('[')) or (CurrentChar=ord(']'))) then begin
      if CurrentChar=ord('[') then begin
       if assigned(a) then begin
        result.TakeoverCombine(a);
        FreeAndNil(a);
       end;
       a:=ParseCharacterClass;
       b:=TBRRERegExpCharClass.Create(RegExp);
       if b.Subtraction(result,a) then begin
        FreeAndNil(result);
        result:=b;
        b:=nil;
       end else begin
        FreeAndNil(b);
       end;
       FreeAndNil(a);
       if CurrentChar<>ord(']') then begin
        SyntaxError;
       end;
       continue;
      end else begin
       a.AddChar(ord('-'));
      end;
     end else if assigned(a) then begin
      if not a.IsSingle then begin
       if assigned(a.Last) and (a.Last.Lo<=a.Last.Hi) then begin
        c:=TBRRERegExpCharClass.Create(RegExp);
        c.AddChar(a.Last.Hi);
        c.Canonicalized:=a.Canonicalized;
        c.Inverted:=a.Inverted;
        result.TakeoverCombine(a);
        FreeAndNil(a);
        a:=c;
       end else begin
        FreeAndNil(a);
        SyntaxError;
       end;
      end;
      b:=ParseClassAtom;
      if (not b.IsSingle) and (assigned(b.First) and (a.First.Lo<=a.First.Hi)) then begin
       c:=TBRRERegExpCharClass.Create(RegExp);
       c.AddChar(b.First.Lo);
       c.Canonicalized:=b.Canonicalized;
       c.Inverted:=b.Inverted;
       result.TakeoverCombine(b);
       FreeAndNil(b);
       b:=c;
      end;
      if (not b.IsSingle) or ((assigned(a.Last) and assigned(b.Last)) and not (a.Last.Lo<=b.Last.Hi)) then begin
       FreeAndNil(a);
       FreeAndNil(b);
       SyntaxError;
      end;
      if assigned(a.Last) and assigned(b.Last) then begin
       a.Last.Hi:=b.Last.Hi;
      end else begin
       a.TakeoverCombine(b);
      end;
      FreeAndNil(b);
     end;
    end;
    if assigned(a) then begin
     result.TakeoverCombine(a);
     FreeAndNil(a);
    end;
   end;
   Expect(ord(']'));
   if ((Flags and brrefIGNORECASE)<>0) and not result.Canonicalized then begin
    result.Canonicalize;
   end;
   if InvertFlag then begin
    result.Invert;
   end;
  finally
   FreeAndNil(a);
   FreeAndNil(b);
  end;
 except
  FreeAndNil(result);
  raise;
 end;
end;

procedure TBRRERegExpCompiler.CollectError;
begin
 raise EBRRENFACollectError.Create('');
end;

procedure TBRRERegExpCompiler.FixUpFixupableCaptures;
var Node:PBRRERegExpASTNode;
    Counter,Index:longint;
begin
 for Counter:=0 to CountFixupableCaptures-1 do begin
  FixupableCaptures[Counter].CaptureIndex:=CaptureIdentifierHashMap.GetValue(FixupableCaptures[Counter].Identifier);
  if FixupableCaptures[Counter].CaptureIndex<0 then begin
   SyntaxError;
  end;
 end;
 for Counter:=0 to Nodes.Count-1 do begin
  Node:=Nodes[Counter];
  if (Node^.NodeType in [ntBACKREF,ntBACKREFI,ntRECURSIVE]) and (Node^.Value<0) then begin
   Index:=(-Node^.Value)-1;
   if ((Index>=0) and (Index<CountFixupableCaptures)) and (FixupableCaptures[Index].CaptureIndex>=0) then begin
    Node^.Value:=FixupableCaptures[Index].CaptureIndex;
   end else begin
    SyntaxError;
   end;
  end;
 end;
end;

function TBRRERegExpCompiler.NewNode(NodeType:TBRRERegExpASTNodeType;Left,Right,Extra:PBRRERegExpASTNode;Value:longint):PBRRERegExpASTNode;
begin
 New(result);
 FillChar(result^,sizeof(TBRRERegExpASTNode),#0);
 result^.NodeType:=NodeType;
 result^.Left:=Left;
 result^.Right:=Right;
 result^.Extra:=Extra;
 result^.Value:=Value;
 result^.Index:=Nodes.Add(result);
end;

function TBRRERegExpCompiler.CopyNode(FromNode:PBRRERegExpASTNode):PBRRERegExpASTNode;
begin
 if assigned(FromNode) then begin
  New(result);
  FillChar(result^,sizeof(TBRRERegExpASTNode),#0);
  result^.NodeType:=FromNode^.NodeType;
  result^.Left:=CopyNode(FromNode^.Left);
  result^.Right:=CopyNode(FromNode^.Right);
  result^.Value:=FromNode^.Value;
  result^.Index:=Nodes.Add(result);
 end else begin
  result:=nil;
 end;
end;

function TBRRERegExpCompiler.AreNodesEqual(NodeA,NodeB:PBRRERegExpASTNode):boolean;
begin
 result:=(NodeA=NodeB) or
         ((((assigned(NodeA) and assigned(NodeB))) and ((NodeA^.NodeType=NodeB^.NodeType) and ((NodeA^.Value=NodeB^.Value) and ((AreNodesEqual(NodeA^.Left,NodeB^.Left) and AreNodesEqual(NodeA^.Right,NodeB^.Right)) and AreNodesEqual(NodeA^.Extra,NodeB^.Extra))))) or
          not (assigned(NodeA) or assigned(NodeB)));
end;

function TBRRERegExpCompiler.AreNodesEqualSafe(NodeA,NodeB:PBRRERegExpASTNode):boolean;
begin
 result:=(NodeA=NodeB) or
         ((((assigned(NodeA) and assigned(NodeB))) and (((NodeA^.NodeType=NodeB^.NodeType) and not (NodeB^.NodeType in [ntPAREN])) and ((NodeA^.Value=NodeB^.Value) and ((AreNodesEqualSafe(NodeA^.Left,NodeB^.Left) and AreNodesEqualSafe(NodeA^.Right,NodeB^.Right)) and AreNodesEqualSafe(NodeA^.Extra,NodeB^.Extra))))) or
          not (assigned(NodeA) or assigned(NodeB)));
end;

function TBRRERegExpCompiler.Concat(NodeLeft,NodeRight:PBRRERegExpASTNode):PBRRERegExpASTNode;
begin
 if assigned(NodeLeft) and assigned(NodeRight) then begin
  if ((NodeLeft^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (NodeRight^.NodeType=ntPLUS)) and AreNodesEqualSafe(NodeLeft^.Left,NodeRight^.Left) and (NodeLeft^.Value=0) and (NodeRight^.Value=0) then begin
   result:=NodeRight;
  end else if ((NodeLeft^.NodeType in [ntSTAR,ntPLUS]) and (NodeRight^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(NodeLeft^.Left,NodeRight^.Left) and (NodeLeft^.Value=0) and (NodeRight^.Value=0) then begin
   result:=NodeLeft;
  end else if (NodeLeft^.NodeType=ntCAT) and assigned(NodeLeft^.Left) and assigned(NodeLeft^.Right) then begin
   if ((NodeLeft^.Right^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (NodeRight^.NodeType=ntPLUS)) and AreNodesEqualSafe(NodeLeft^.Right^.Left,NodeRight^.Left) and (NodeLeft^.Right^.Value=0) and (NodeRight^.Value=0) then begin
    NodeLeft^.Right:=NodeRight;
    result:=NodeLeft;
   end else if ((NodeLeft^.Right^.NodeType in [ntSTAR,ntPLUS]) and (NodeRight^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(NodeLeft^.Right^.Left,NodeRight^.Left) and (NodeLeft^.Right^.Value=0) and (NodeRight^.Value=0) then begin
    result:=NodeLeft;
   end else begin
    result:=NewNode(ntCAT,NodeLeft,NodeRight,nil,0);
   end;
  end else begin
   result:=NewNode(ntCAT,NodeLeft,NodeRight,nil,0);
  end;
 end else begin
  if assigned(NodeLeft) then begin
   result:=NodeLeft;
  end else if assigned(NodeRight) then begin
   result:=NodeRight;
  end else begin
   result:=nil;
  end;
 end;
 while (assigned(result) and (result^.NodeType=ntCAT)) and (assigned(result^.Left) and assigned(result^.Right)) do begin
  if (result^.Left^.NodeType=ntCAT) and (result^.Right^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and assigned(result^.Left^.Right) and (result^.Right^.Value=0) then begin
   if ((result^.Left^.Right^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Right^.NodeType=ntPLUS)) and AreNodesEqualSafe(result^.Left^.Right^.Left,result^.Right^.Left) then begin
    result^.Left^.Right:=result^.Right;
    result:=result^.Left;
    continue;
   end else if ((result^.Left^.Right^.NodeType in [ntSTAR,ntPLUS]) and (result^.Right^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(result^.Left^.Right^.Left,result^.Right^.Left) then begin
    result:=result^.Left;
    continue;
   end;
  end else if (result^.Left^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Right^.NodeType=ntCAT) and assigned(result^.Right^.Left) and (result^.Left^.Value=0) then begin
   if ((result^.Left^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Right^.Left^.NodeType=ntPLUS)) and AreNodesEqualSafe(result^.Left^.Left,result^.Right^.Left^.Left) and (result^.Right^.Left^.Value=0) then begin
    result:=result^.Right;
    continue;
   end else if ((result^.Left^.NodeType in [ntSTAR,ntPLUS]) and (result^.Right^.Left^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(result^.Left^.Left,result^.Right^.Left^.Left) and (result^.Right^.Left^.Value=0) then begin
    result^.Right^.Left:=result^.Left;
    result:=result^.Right;
    continue;
   end;
  end else if (result^.Left^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Right^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Left^.Value=0) and (result^.Right^.Value=0) then begin
   if ((result^.Left^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Right^.NodeType=ntPLUS)) and AreNodesEqualSafe(result^.Left^.Left,result^.Right^.Left) then begin
    result:=result^.Right;
    continue;
   end else if ((result^.Left^.NodeType in [ntSTAR,ntPLUS]) and (result^.Right^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(result^.Left^.Left,result^.Right^.Left) then begin
    result:=result^.Left;
    continue;
   end;
  end;
  break;
 end;
end;

function TBRRERegExpCompiler.NewAlt(NodeLeft,NodeRight:PBRRERegExpASTNode):PBRRERegExpASTNode;
var NodeEx,pl,pr:PPBRRERegExpASTNode;
    Node,l,r:PBRRERegExpASTNode;
    c:TBRRERegExpCharClass;
begin
 if assigned(NodeLeft) and assigned(NodeRight) then begin
  if (NodeLeft^.NodeType=ntCAT) and (NodeRight^.NodeType=ntCAT) then begin
   result:=NewNode(ntALT,NodeLeft,NodeRight,nil,0);
   NodeEx:=@result;
   while (((assigned(NodeEx) and assigned(NodeEx^)) and (NodeEx^^.NodeType=ntALT)) and (assigned(NodeEx^^.Left) and assigned(NodeEx^^.Right))) and ((NodeEx^^.Left^.NodeType=ntCAT) and (NodeEx^^.Right^.NodeType=ntCAT)) do begin
    Node:=NodeEx^;
    pl:=@Node^.Left;
    l:=Node^.Left;
    while (assigned(l) and assigned(l^.Left)) and (l^.Left^.NodeType=ntCAT) do begin
     pl:=@l^.Left;
     l:=l^.Left;
    end;
    pr:=@Node^.Right;
    r:=Node^.Right;
    while (assigned(r) and assigned(r^.Left)) and (r^.Left^.NodeType=ntCAT) do begin
     pr:=@r^.Left;
     r:=r^.Left;
    end;
    if ((assigned(l^.Left) and assigned(l^.Right)) and (assigned(r^.Left) and assigned(r^.Right))) and AreNodesEqualSafe(l^.Left,r^.Left) then begin
     NodeEx^:=l;
     pl^:=l^.Right;
     pr^:=r^.Right;
     l^.Right:=Node;
     NodeEx:=@l^.Right;
     continue;
    end;
    break;
   end;
  end else if AreNodesEqualSafe(NodeLeft,NodeRight) then begin
   result:=NodeLeft;
  end else if (NodeLeft^.NodeType=ntCHAR) and (NodeRight^.NodeType=ntCHAR) then begin
   c:=TBRRERegExpCharClass.Create(RegExp);
   c.Combine(CharClasses[NodeLeft^.Value]);
   c.Combine(CharClasses[NodeRight^.Value]);
   result:=ConvertUTF8CharClassNode(NewNode(ntCHAR,nil,nil,nil,RegExp.AddCharClass(c)));
  end else begin
   result:=NewNode(ntALT,NodeLeft,NodeRight,nil,0);
  end;
 end else begin
  if assigned(NodeLeft) then begin
   result:=NodeLeft;
  end else if assigned(NodeRight) then begin
   result:=NodeRight;
  end else begin
   result:=nil;
  end;
 end;
end;

{ * (a*)* equals a*
  * (a+)+ equals a+
  * (a?)? equals a?
  * (a*)+ equals (a+)* equals a*
  * (a*)? equals (a?)* equals a*
  * (a+)? equals (a?)+ equals a*
}
function TBRRERegExpCompiler.NewPlus(Node:PBRRERegExpASTNode;Kind:longint):PBRRERegExpASTNode;
begin
 if (Kind=brreqkPOSSESSIVE) and not IsBacktracking then begin
  // Possessive quantifiers are allowed only at the backtracking mode
  CollectError;
  result:=nil;
 end else if (Kind=brreqkGREEDY) and ((assigned(Node) and (Node^.NodeType=ntPAREN)) and ((assigned(Node^.Left) and (Node^.Left^.NodeType in [ntPLUS,ntSTAR])) and (Node^.Left^.Value=brreqkGREEDY))) then begin
  result:=Node;
 end else if (Kind=brreqkGREEDY) and ((assigned(Node) and (Node^.NodeType=ntPLUS)) and (Node^.Value=brreqkGREEDY)) then begin
  result:=Node;
 end else if (Kind=brreqkGREEDY) and ((assigned(Node) and (Node^.NodeType in [ntSTAR,ntQUEST])) and (Node^.Value=brreqkGREEDY)) then begin
  result:=Node;
  result^.NodeType:=ntSTAR;
 end else begin
  result:=NewNode(ntPLUS,Node,nil,nil,Kind);
 end;
end;

function TBRRERegExpCompiler.NewStar(Node:PBRRERegExpASTNode;Kind:longint):PBRRERegExpASTNode;
begin
 if (Kind=brreqkPOSSESSIVE) and not IsBacktracking then begin
  // Possessive quantifiers are allowed only at the backtracking mode
  CollectError;
  result:=nil;
 end else if (Kind=brreqkGREEDY) and ((assigned(Node) and (Node^.NodeType=ntPAREN)) and ((assigned(Node^.Left) and (Node^.Left^.NodeType in [ntSTAR,ntPLUS{,ntQUEST}])) and (Node^.Left^.Value=brreqkGREEDY))) then begin
  result:=Node;
  result^.Left^.NodeType:=ntSTAR;
 end else if (Kind=brreqkGREEDY) and ((assigned(Node) and (Node^.NodeType in [ntPLUS,ntQUEST,ntSTAR])) and (Node^.Value=brreqkGREEDY)) then begin
  result:=Node;
  result^.NodeType:=ntSTAR;
 end else begin
  result:=NewNode(ntSTAR,Node,nil,nil,Kind);
 end;
end;

function TBRRERegExpCompiler.NewQuest(Node:PBRRERegExpASTNode;Kind:longint):PBRRERegExpASTNode;
begin
 if Kind=brreqkPOSSESSIVE then begin
  // x?+ is just x
  result:=Node;
 end else if (Kind=brreqkGREEDY) and ((assigned(Node) and (Node^.NodeType=ntPAREN)) and ((assigned(Node^.Left) and (Node^.Left^.NodeType=ntQUEST)) and (Node^.Left^.Value=brreqkGREEDY))) then begin
  result:=Node;
 end else if (Kind=brreqkGREEDY) and ((assigned(Node) and (Node^.NodeType=ntPAREN)) and ((assigned(Node^.Left) and (Node^.Left^.NodeType in [ntSTAR,ntPLUS{,ntQUEST}])) and (Node^.Left^.Value=brreqkGREEDY))) then begin
  result:=Node;
  result^.Left^.NodeType:=ntSTAR;
 end else if (Kind=brreqkGREEDY) and ((assigned(Node) and (Node^.NodeType=ntQUEST)) and (Node^.Value=brreqkGREEDY)) then begin
  result:=Node;
 end else if (Kind=brreqkGREEDY) and ((assigned(Node) and (Node^.NodeType in [ntPLUS,ntSTAR])) and (Node^.Value=brreqkGREEDY)) then begin
  result:=Node;
  result^.NodeType:=ntSTAR;
 end else begin
  result:=NewNode(ntQUEST,Node,nil,nil,Kind);
 end;
end;

function TBRRERegExpCompiler.IsStarNullable(Node:PBRRERegExpASTNode):boolean;
begin
 if assigned(Node) then begin
  case Node^.NodeType of
   ntSTAR:begin
    result:=Node^.Value=brreqkGREEDY;
   end;
   ntALT:begin
    result:=(IsStarNullable(Node^.Left) or IsStarNullable(Node^.Right)) or not (assigned(Node^.Left) and assigned(Node^.Right));
   end;
   ntCAT:begin
    result:=IsStarNullable(Node^.Left) and IsStarNullable(Node^.Right);
   end;
   else begin
    result:=false;
   end;
  end;
 end else begin
  result:=false;
 end;
end;

function TBRRERegExpCompiler.StarDenull(Node:PBRRERegExpASTNode):PBRRERegExpASTNode;
begin
 result:=Node;
 if IsStarNullable(result) then begin
  case result^.NodeType of
   ntSTAR:begin
    result:=result^.Left;
   end;
   ntCAT:begin
    result:=NewAlt(StarDenull(result^.Left),StarDenull(result^.Right));
{     result^.NodeType:=ntALT;
    result^.Left:=StarDenull(result^.Left);
    result^.Right:=StarDenull(result^.Right);}
   end;
   ntALT:begin
    result:=NewAlt(StarDenull(result^.Left),StarDenull(result^.Right));
{     result^.Left:=StarDenull(result^.Left);
    result^.Right:=StarDenull(result^.Right);}
   end;
  end;
 end;
end;

function TBRRERegExpCompiler.NewRepeat(Node:PBRRERegExpASTNode;Kind,Min,Max:longint):PBRRERegExpASTNode;
var Counter:longint;
begin
 if (Min>=0) and (Max=brrecINFINITY) then begin
  case Min of
   0:begin
    // x{0,} is x*
    result:=NewStar(StarDenull(Node),Kind);
   end;
   1:begin
    // x{1,} is x+
    result:=NewPlus(Node,Kind);
   end;
   else begin
    if IsBacktracking and (((Flags and brrefNFA)<>0) or (Kind=brreqkPOSSESSIVE)) then begin
     result:=NewNode(ntREPEAT,Node,nil,nil,Kind);
     result^.Min:=Min;
     result^.Min:=Max;
    end else begin
     // x{2,} is xx+, x{3,} is xxx+, x{4,} is xxxx+ and so on...
     result:=nil;
     for Counter:=2 to Min do begin
      if assigned(result) then begin
       result:=Concat(result,CopyNode(Node));
      end else begin
       result:=CopyNode(Node);
      end;
     end;
     if assigned(result) then begin
      result:=Concat(result,NewPlus(Node,Kind));
     end else begin
      result:=Node;
     end;
    end;
   end;
  end;
 end else if (Min=0) and (Max=0) then begin
  // x{0,0} is nothing
  result:=nil;
 end else if (Min=0) and (Max=1) then begin
  // x{0,1} is x?
  result:=NewQuest(Node,Kind);
 end else if (Min=1) and (Max=1) then begin
  // x{1,1} is just x
  result:=Node;
 end else if (Min>0) and (Min=Max) then begin
  // x{n,m}, when where n equals to m, is n-multiple x
  if IsBacktracking and (((Flags and brrefNFA)<>0) or (Kind=brreqkPOSSESSIVE)) then begin
   result:=NewNode(ntEXACT,Node,nil,nil,Max);
  end else begin
   result:=nil;
   for Counter:=1 to Max-1 do begin
    if assigned(result) then begin
     result:=Concat(result,CopyNode(Node));
    end else begin
     result:=CopyNode(Node);
    end;
   end;
   if assigned(result) then begin
    result:=Concat(result,Node);
   end else begin
    result:=Node;
   end;
  end;
 end else if (Min>=0) and (Min<Max) then begin
  if IsBacktracking and (((Flags and brrefNFA)<>0) or (Kind=brreqkPOSSESSIVE)) then begin
   result:=NewNode(ntREPEAT,Node,nil,nil,Kind);
   result^.Min:=Min;
   result^.Min:=Max;
  end else begin
   // x{n,m} is n-multiple x + m-multiple x?
   result:=nil;
   if Min>0 then begin
    if IsBacktracking then begin
     result:=NewNode(ntEXACT,Node,nil,nil,Min);
    end else begin
     for Counter:=1 to Min do begin
      if assigned(result) then begin
       result:=Concat(result,CopyNode(Node));
      end else begin
       result:=CopyNode(Node);
      end;
     end;
    end;
   end;
   if Max>Min then begin
    for Counter:=Min+1 to Max-1 do begin
     if assigned(result) then begin
      result:=Concat(result,NewQuest(CopyNode(Node),Kind));
     end else begin
      result:=NewQuest(CopyNode(Node),Kind);
     end;
    end;
    if assigned(result) then begin
     result:=Concat(result,NewQuest(Node,Kind));
    end else begin
     result:=NewQuest(Node,Kind);
    end;
   end;
  end;
 end else begin
  // Oops, this should never happen, so cases like min > max, or min < max < 0, but the parser rejects such cases.
  result:=nil;
  SyntaxError;
 end;
end;

function TBRRERegExpCompiler.ConvertUTF8RangeToLatin1Ranges(Lo,Hi:longword):PBRRERegExpASTNode;
type TString6Chars=string[6];
const Seq0010ffff:array[0..6,0..4,0..1] of longint=((($00,$7f),(-1,-1),(-1,-1),(-1,-1),(-1,-1)),        // 00-7F
                                                    (($c2,$df),($80,$bf),(-1,-1),(-1,-1),(-1,-1)),      // C2-DF 80-BF
                                                    (($e0,$e0),($a0,$bf),($80,$bf),(-1,-1),(-1,-1)),    // E0-E0 A0-BF 80-BF
                                                    (($e1,$ef),($80,$bf),($80,$bf),(-1,-1),(-1,-1)),    // E1-EF 80-BF 80-BF
                                                    (($f0,$f0),($80,$bf),($80,$bf),($80,$bf),(-1,-1)),  // F0-F0 90-BF 80-BF 80-BF
                                                    (($f1,$f3),($80,$bf),($80,$bf),($80,$bf),(-1,-1)),  // F1-F3 80-BF 80-BF 80-BF
                                                    (($f4,$f4),($80,$bf),($80,$bf),($80,$bf),(-1,-1))); // F4-F4 80-8F 80-BF 80-BF
var OutputNode,LastNode:PBRRERegExpASTNode;
 function ToString(CharValue:longword):TString6Chars;
 begin
  case CharValue of
   $00000000..$0000007f:begin
    SetLength(result,1);
    result[1]:=ansichar(byte(CharValue));
   end;
   $00000080..$000007ff:begin
    SetLength(result,2);
    result[1]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
    result[2]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   $00000800..$0000ffff:begin
    SetLength(result,3);
    result[1]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    result[2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    result[3]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   $00010000..$0010ffff:begin
    SetLength(result,4);
    result[1]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
    result[2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    result[3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    result[4]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   $00200000..$03ffffff:begin
    SetLength(result,5);
    result[1]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
    result[2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    result[3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    result[4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    result[5]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   $04000000..$7fffffff:begin
    SetLength(result,6);
    result[1]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
    result[2]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
    result[3]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    result[4]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    result[5]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    result[6]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   else begin
    SetLength(result,3);
    result[1]:=#$ef;
    result[2]:=#$bf;
    result[3]:=#$bd;
   end;
  end;
 end;
 procedure AddRange(Lo,Hi:byte);
 var Node:PBRRERegExpASTNode;
     c:TBRRERegExpCharClass;
 begin
  c:=TBRRERegExpCharClass.Create(RegExp);
  c.AddRange(Lo,Hi,false);
  Node:=NewNode(ntCHAR,nil,nil,nil,RegExp.AddCharClass(c));
  if assigned(LastNode) then begin
   LastNode:=Concat(LastNode,Node);
  end else begin
   LastNode:=Node;
  end;
 end;
 procedure FlushRange;
 begin
  if assigned(LastNode) then begin
   if assigned(OutputNode) then begin
    OutputNode:=NewAlt(OutputNode,LastNode);
   end else begin
    OutputNode:=LastNode;
   end;
   LastNode:=nil;
  end;
 end;
 procedure ProcessRange(Lo,Hi:longword);
 var i,m:longword;
     StrLo,StrHi:TString6Chars;
 begin
{$IFDEF BRREStrictUTF8}
   if Hi>$0010ffff then begin
    Hi:=$0010ffff;
   end;
{$ELSE}
   if Hi>$7fffffff then begin
    Hi:=$7fffffff;
   end;
{$ENDIF}
  if Lo<=Hi then begin
    if (Lo=$00000000) and (Hi=$0010ffff) then begin
    for m:=low(Seq0010ffff) to high(Seq0010ffff) do begin
     for i:=low(Seq0010ffff[m]) to high(Seq0010ffff[m]) do begin
      if Seq0010ffff[m,i,0]<0 then begin
       break;
      end;
      AddRange(byte(Seq0010ffff[m,i,0]),byte(Seq0010ffff[m,i,1]));
     end;
     FlushRange;
    end;
   end else if (Lo=$00000080) and (Hi=$0010ffff) then begin
    for m:=1 to high(Seq0010ffff) do begin
     for i:=low(Seq0010ffff[m]) to high(Seq0010ffff[m]) do begin
      if Seq0010ffff[m,i,0]<0 then begin
       break;
      end;
      AddRange(byte(Seq0010ffff[m,i,0]),byte(Seq0010ffff[m,i,1]));
     end;
     FlushRange;
    end;
   end else begin
    for i:=1 to 3 do begin
     if i=1 then begin
      m:=7;
     end else begin
      m:=(7-i)+(6*(i-1));
     end;
     m:=(1 shl m)-1;
     if (Lo<=m) and (m<Hi) then begin
      ProcessRange(Lo,m);
      ProcessRange(m+1,Hi);
      exit;
     end;
    end;
    if Hi<128 then begin
     AddRange(Lo,Hi);
     FlushRange;
    end else begin
     for i:=1 to 3 do begin
      m:=(1 shl (6*i))-1;
      if (Lo and not m)<>(Hi and not m) then begin
       if (Lo and m)<>0 then begin
        ProcessRange(Lo,Lo or m);
        ProcessRange((Lo or m)+1,Hi);
        exit;
       end else if (Hi and m)<>m then begin
        ProcessRange(Lo,(Hi and not m)-1);
        ProcessRange(Hi and not m,Hi);
        exit;
       end;
      end;
     end;
     StrLo:=ToString(Lo);
     StrHi:=ToString(Hi);
     if length(StrLo)=length(StrHi) then begin
      for i:=1 to length(StrLo) do begin
       AddRange(byte(StrLo[i]),byte(StrHi[i]));
      end;
      FlushRange;
     end;
    end;
   end;
  end;
 end;
begin
 OutputNode:=nil;
 LastNode:=nil;
 ProcessRange(Lo,Hi);
 result:=OutputNode;
end;

function TBRRERegExpCompiler.ConvertUTF8CharClassNode(Node:PBRRERegExpASTNode):PBRRERegExpASTNode;
var Range:TBRRERegExpCharClassRange;
    NewNode:PBRRERegExpASTNode;
begin
 if ((Flags and brrefUTF8CODEUNITS)<>0) and (assigned(Node) and (Node^.NodeType=ntCHAR)) then begin
  if (not assigned(CharClasses[Node^.Value].First)) or (CharClasses[Node^.Value].Last.Hi<128) then begin
   result:=Node;
  end else begin
   result:=nil;
   Range:=CharClasses[Node^.Value].First;
   while assigned(Range) do begin
    NewNode:=ConvertUTF8RangeToLatin1Ranges(Range.Lo,Range.Hi);
    if assigned(result) then begin
     result:=NewAlt(result,NewNode);
    end else begin
     result:=NewNode;
    end;
    Range:=Range.Next;
   end;
   if not assigned(result) then begin
    result:=Node;
   end;
  end;
 end else begin
  result:=Node;
 end;
end;

function TBRRERegExpCompiler.ParseAtom(var Node:PBRRERegExpASTNode):longint;
var IsNegative,First,Num:boolean;
    i,OldCountOfCaptures,LastSourcePos,UntilSourcePos:longint;
    Identifier:ansistring;
    OldFlags:TBRRERegExpFlags;
    c:TBRRERegExpCharClass;
    Ch:longword;
    NodeA,NodeB,NodeC:PBRRERegExpASTNode;
begin
 c:=nil;
 result:=0;
 Identifier:='';
 try
  case CurrentChar of
   ord('^'):begin
    NextChar;
    if (Flags and brrefMULTILINE)<>0 then begin
     Node:=NewNode(ntBOML,nil,nil,nil,0);
    end else begin
     Node:=NewNode(ntBOSL,nil,nil,nil,0);
    end;
    result:=-1;
    exit;
   end;
   ord('$'):begin
    NextChar;
    if (Flags and brrefMULTILINE)<>0 then begin
     Node:=NewNode(ntEOML,nil,nil,nil,0);
    end else begin
     Node:=NewNode(ntEOSL,nil,nil,nil,0);
    end;
    result:=-1;
    exit;
   end;
   ord('*'),ord('+'),ord('?'),ord(')'),ord(']'),ord('{'),ord('}'),ord('|'):begin
    if IsBacktracking then begin
     SyntaxError;
    end else begin
     CollectError;
    end;
    result:=-1;
    exit;
   end;
   ord('('):begin
    NextChar;
    if (not AtEOF) and (CurrentChar=ord('?')) then begin
     NextChar;
     if not AtEOF then begin
      case CurrentChar of
       ord('#'):begin
        NextChar;
        while (not AtEOF) and (CurrentChar<>ord(')')) do begin
         NextChar;
        end;
        Expect(ord(')'));
        result:=1;
        exit;
       end;
       ord('+'),ord('-'),ord('a')..ord('z'):begin
        OldFlags:=Flags;
        IsNegative:=false;
        First:=true;
        Num:=false;
        while (not AtEOF) and (((CurrentChar=ord('-')) or (CurrentChar=ord('+'))) or (((CurrentChar>=ord('a')) and (CurrentChar<=ord('z'))) or ((CurrentChar>=ord('A')) and (CurrentChar<=ord('Z'))))) do begin
         case CurrentChar of
          ord('-'):begin
           IsNegative:=true;
           Num:=First;
          end;
          ord('+'):begin
           IsNegative:=false;
           Num:=First;
          end;
          ord('l'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefLATIN1;
           end else begin
            Flags:=Flags or brrefLATIN1;
           end;
          end;
          ord('i'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefIGNORECASE;
           end else begin
            Flags:=Flags or brrefIGNORECASE;
           end;
          end;
          ord('m'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefMULTILINE;
           end else begin
            Flags:=Flags or brrefMULTILINE;
           end;
          end;
          ord('s'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefSINGLELINE;
           end else begin
            Flags:=Flags or brrefSINGLELINE;
           end;
          end;
          ord('b'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefBACKTRACKING;
           end else begin
            Flags:=Flags or brrefBACKTRACKING;
           end;
          end;
          ord('B'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefNOBACKTRACKING;
           end else begin
            Flags:=Flags or brrefNOBACKTRACKING;
           end;
          end;
          ord('x'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefFREESPACING;
           end else begin
            Flags:=Flags or brrefFREESPACING;
           end;
          end;
          ord('u'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefUTF8;
           end else begin
            Flags:=Flags or brrefUTF8;
           end;
          end;
          ord('T'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefUTF8CODEUNITS;
           end else begin
            Flags:=Flags or brrefUTF8CODEUNITS;
           end;
          end;
          ord('n'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefNAMED;
           end else begin
            Flags:=Flags or brrefNAMED;
           end;
          end;
          ord('c'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefCONTINUATION;
           end else begin
            Flags:=Flags or brrefCONTINUATION;
           end;
          end;
          ord('a'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefAUTO;
           end else begin
            Flags:=Flags or brrefAUTO;
           end;
          end;
          ord('w'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefWHOLEONLY;
           end else begin
            Flags:=Flags or brrefWHOLEONLY;
           end;
          end;
          ord('o'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefCOMBINEOPTIMIZATION;
           end else begin
            Flags:=Flags or brrefCOMBINEOPTIMIZATION;
           end;
          end;
          ord('U'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefUNGREEDY;
           end else begin
            Flags:=Flags or brrefUNGREEDY;
           end;
          end;
          ord('p'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefLONGEST;
           end else begin
            Flags:=Flags or brrefLONGEST;
           end;
          end;
          ord('f'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefNFA;
           end else begin
            Flags:=Flags or brrefNFA;
           end;
          end;
          ord('F'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefDFA;
           end else begin
            Flags:=Flags or brrefDFA;
           end;
          end;
          ord('P'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefPREFILTER;
           end else begin
            Flags:=Flags or brrefPREFILTER;
           end;
          end;
          ord('O'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefPREFILTEROPT;
           end else begin
            Flags:=Flags or brrefPREFILTEROPT;
           end;
          end;
          ord('C'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefAUTOCALLOUT;
           end else begin
            Flags:=Flags or brrefAUTOCALLOUT;
           end;
          end;
          ord('E'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefNOTEMPTY;
           end else begin
            Flags:=Flags or brrefNOTEMPTY;
           end;
          end;
          ord('R'):begin
           if IsNegative then begin
            Flags:=Flags and not brrefRANGE;
           end else begin
            Flags:=Flags or brrefRANGE;
           end;
          end;
         end;
         First:=false;
         NextChar;
         if Num then begin
          case CurrentChar of
           ord('0')..ord('9'):begin
            i:=0;
            while (not AtEOF) and (CurrentChar>=ord('0')) and (CurrentChar<=ord('9')) do begin
             i:=(i*10)+longint(CurrentChar-ord('0'));
             NextChar;
            end;
            if IsNegative then begin
             i:=RegExp.CountOfCaptures-i;
            end else begin
             i:=RegExp.CountOfCaptures+i;
            end;
            if IsBackTracking and ((Flags and (brrefWHOLEONLY or brrefCOMBINEOPTIMIZATION))=0) then begin
             HasRecursive:=true;
             Node:=NewNode(ntRECURSIVE,nil,nil,nil,i);
            end else begin
             if IsBackTracking then begin
              SyntaxError;
             end else begin
              CollectError;
             end;
            end;
            Expect(ord(')'));
            exit;
           end;
          end;
         end;
        end;
        if (not AtEOF) and (CurrentChar=ord(':')) then begin
         NextChar;
         if IsBacktracking then begin
          Node:=ParseDisjunction(true);
          Expect(ord(')'));
         end else begin
          CollectError;
         end;
         Flags:=OldFlags;
        end else begin
         Expect(ord(')'));
         result:=1;
        end;
        exit;
       end;
       ord('{'):begin
        if IsBackTracking then begin
         NextChar;
         if (CurrentChar>=ord('0')) and (CurrentChar<=ord('9')) then begin
          i:=0;
          while (not AtEOF) and (CurrentChar>=ord('0')) and (CurrentChar<=ord('9')) do begin
           i:=(i*10)+longint(CurrentChar-ord('0'));
           NextChar;
          end;
          Node:=NewNode(ntCALLOUT,nil,nil,nil,i);
         end else begin
          SyntaxError;
         end;
        end else begin
         CollectError;
        end;
        Expect(ord('}'));
        Expect(ord(')'));
        result:=1;
        exit;
       end;
       ord('C'):begin
        if IsBackTracking then begin
         NextChar;
         if (CurrentChar>=ord('0')) and (CurrentChar<=ord('9')) then begin
          i:=0;
          while (not AtEOF) and (CurrentChar>=ord('0')) and (CurrentChar<=ord('9')) do begin
           i:=(i*10)+longint(CurrentChar-ord('0'));
           NextChar;
          end;
          Node:=NewNode(ntCALLOUT,nil,nil,nil,i);
         end else begin
          Node:=NewNode(ntCALLOUT,nil,nil,nil,-1);
         end;
        end else begin
         CollectError;
        end;
        Expect(ord(')'));
        result:=1;
        exit;
       end;
       ord('P'):begin
        NextChar;
        if not AtEOF then begin
         case CurrentChar of
          ord('<'):begin
           LastSourcePos:=SourcePos;
           UntilSourcePos:=SourcePos;
           NextChar;
           while (not AtEOF) and (BRREUnicodeIsIDPart(CurrentChar)) do begin
            UntilSourcePos:=SourcePos;
            NextChar;
           end;
           if (LastSourcePos<UntilSourcePos) and ((not AtEOF) and (CurrentChar=ord('>'))) then begin
            Identifier:=copy(SourceStr,LastSourcePos,UntilSourcePos-LastSourcePos);
            NextChar;
            i:=CaptureIdentifierHashMap.GetValue(Identifier);
            if i<0 then begin
             i:=RegExp.CountOfCaptures;
             inc(RegExp.CountOfCaptures);
             CaptureIdentifierHashMap.SetValue(AnsiString(IntToStr(i)),i);
             CaptureIdentifierHashMap.SetValue(Identifier,i);
             if i<CaptureIdentifiers.Count then begin
              CaptureIdentifiers[i]:=String(Identifier);
             end else if i=CaptureIdentifiers.Count then begin
              CaptureIdentifiers.Add(String(Identifier));
             end else begin
              while CaptureIdentifiers.Count<i do begin
               CaptureIdentifiers.Add('-');
              end;
              CaptureIdentifiers.Add(String(Identifier));
             end;
            end;
            Node:=ParseDisjunction(true);
            if i<CaptureNodes.Count then begin
             CaptureNodes[i]:=Node;
            end else if i=CaptureNodes.Count then begin
             CaptureNodes.Add(Node);
            end else begin
             while CaptureNodes.Count<i do begin
              CaptureNodes.Add(nil);
             end;
             CaptureNodes.Add(Node);
            end;
            Node:=NewNode(ntPAREN,Node,nil,nil,i);
           end else begin
            SyntaxError;
           end;
          end;
          ord('>'):begin
           if IsBackTracking and ((Flags and (brrefWHOLEONLY or brrefCOMBINEOPTIMIZATION))=0) then begin
            LastSourcePos:=SourcePos;
            UntilSourcePos:=SourcePos;
            NextChar;
            while (not AtEOF) and (BRREUnicodeIsIDPart(CurrentChar)) do begin
             UntilSourcePos:=SourcePos;
             NextChar;
            end;
            if (LastSourcePos<UntilSourcePos) and ((not AtEOF) and (CurrentChar=ord(')'))) then begin
             Identifier:=copy(SourceStr,LastSourcePos,UntilSourcePos-LastSourcePos);
             HasRecursive:=true;
             Node:=NewNode(ntRECURSIVE,nil,nil,nil,-$7fffffff);
             Node^.Value:=-(GetFixupableCaptureIndex(Identifier)+1);
            end else begin
             SyntaxError;
            end;
           end else begin
            if IsBackTracking then begin
             SyntaxError;
            end else begin
             CollectError;
            end;
           end;
          end;
          ord('='):begin
           if IsBackTracking and ((Flags and brrefWHOLEONLY)=0) then begin
            LastSourcePos:=SourcePos;
            UntilSourcePos:=SourcePos;
            NextChar;
            while (not AtEOF) and (BRREUnicodeIsIDPart(CurrentChar)) do begin
             UntilSourcePos:=SourcePos;
             NextChar;
            end;
            if (LastSourcePos<UntilSourcePos) and ((not AtEOF) and (CurrentChar=ord(')'))) then begin
             Identifier:=copy(SourceStr,LastSourcePos,UntilSourcePos-LastSourcePos);
             if (Flags and brrefIGNORECASE)<>0 then begin
              Node:=NewNode(ntBACKREFI,nil,nil,nil,-$7fffffff);
             end else begin
              Node:=NewNode(ntBACKREF,nil,nil,nil,-$7fffffff);
             end;
             Node^.Value:=-(GetFixupableCaptureIndex(Identifier)+1);
            end else begin
             SyntaxError;
            end;
           end else begin
            if IsBackTracking then begin
             SyntaxError;
            end else begin
             CollectError;
            end;
           end;
          end;
          else begin
           SyntaxError;
          end;
         end;
        end else begin
         SyntaxError;
        end;
       end;
       ord('0')..ord('9'):begin
        i:=0;
        while (not AtEOF) and (CurrentChar>=ord('0')) and (CurrentChar<=ord('9')) do begin
         i:=(i*10)+longint(CurrentChar-ord('0'));
         NextChar;
        end;
        if IsBackTracking and ((Flags and (brrefWHOLEONLY or brrefCOMBINEOPTIMIZATION))=0) then begin
         HasRecursive:=true;
         Node:=NewNode(ntRECURSIVE,nil,nil,nil,i);
        end else begin
         if IsBackTracking then begin
          SyntaxError;
         end else begin
          CollectError;
         end;
        end;
        Expect(ord(')'));
        exit;
       end;
       ord('R'):begin
        NextChar;
        if IsBackTracking and ((Flags and (brrefWHOLEONLY or brrefCOMBINEOPTIMIZATION))=0) then begin
         HasRecursive:=true;
         Node:=NewNode(ntRECURSIVE,nil,nil,nil,0);
        end else begin
         if IsBackTracking then begin
          SyntaxError;
         end else begin
          CollectError;
         end;
        end;
        Expect(ord(')'));
        exit;
       end;
       ord(':'):begin
        NextChar;
        Node:=ParseDisjunction(true);
        Expect(ord(')'));
        exit;
       end;
       ord('|'):begin
        NextChar;
        i:=RegExp.CountOfCaptures;
        NodeA:=ParseAlternative;
        OldCountOfCaptures:=RegExp.CountOfCaptures;
        Expect(ord('|'));
        RegExp.CountOfCaptures:=i;
        NodeB:=ParseAlternative;
        if RegExp.CountOfCaptures<OldCountOfCaptures then begin
         RegExp.CountOfCaptures:=OldCountOfCaptures;
        end;
        Node:=NewAlt(NodeA,NodeB);
        Expect(ord(')'));
        exit;
       end;
       ord('<'):begin
        LastSourcePos:=SourcePos;
        UntilSourcePos:=SourcePos;
        NextChar;
        if not AtEOF then begin
         case CurrentChar of
          ord('='),ord('!'):begin
           if IsBacktracking then begin
            IsNegative:=CurrentChar=ord('!');
            NextChar;
            if IsNegative then begin
             Node:=NewNode(ntLOOKBEHINDFALSE,ParseDisjunction(true),nil,nil,0);
            end else begin
             Node:=NewNode(ntLOOKBEHINDTRUE,ParseDisjunction(true),nil,nil,0);
            end;
           end else begin
            CollectError;
           end;
          end;
          else begin
           NextChar;
           while (not AtEOF) and (BRREUnicodeIsIDPart(CurrentChar)) do begin
            UntilSourcePos:=SourcePos;
            NextChar;
           end;
           if (LastSourcePos<UntilSourcePos) and ((not AtEOF) and (CurrentChar=ord('>'))) then begin
            Identifier:=copy(SourceStr,LastSourcePos,UntilSourcePos-LastSourcePos);
            NextChar;
            i:=CaptureIdentifierHashMap.GetValue(Identifier);
            if i<0 then begin
             i:=RegExp.CountOfCaptures;
             inc(RegExp.CountOfCaptures);
             CaptureIdentifierHashMap.SetValue(AnsiString(IntToStr(i)),i);
             CaptureIdentifierHashMap.SetValue(Identifier,i);
             if i<CaptureIdentifiers.Count then begin
              CaptureIdentifiers[i]:=String(Identifier);
             end else if i=CaptureIdentifiers.Count then begin
              CaptureIdentifiers.Add(String(Identifier));
             end else begin
              while CaptureIdentifiers.Count<i do begin
               CaptureIdentifiers.Add('-');
              end;
              CaptureIdentifiers.Add(String(Identifier));
             end;
            end;
            Node:=ParseDisjunction(true);
            if i<CaptureNodes.Count then begin
             CaptureNodes[i]:=Node;
            end else if i=CaptureNodes.Count then begin
             CaptureNodes.Add(Node);
            end else begin
             while CaptureNodes.Count<i do begin
              CaptureNodes.Add(nil);
             end;
             CaptureNodes.Add(Node);
            end;
            Node:=NewNode(ntPAREN,Node,nil,nil,i);
           end else begin
            SyntaxError;
           end;
          end;
         end;
        end else begin
         SyntaxError;
        end;
       end;
       ord('&'):begin
        if IsBackTracking and ((Flags and (brrefWHOLEONLY or brrefCOMBINEOPTIMIZATION))=0) then begin
         NextChar;
         LastSourcePos:=SourcePos;
         UntilSourcePos:=SourcePos;
         while (not AtEOF) and (BRREUnicodeIsIDPart(CurrentChar)) do begin
          UntilSourcePos:=SourcePos;
          NextChar;
         end;
         if (LastSourcePos<UntilSourcePos) and ((not AtEOF) and (CurrentChar=ord('>'))) then begin
          Identifier:=copy(SourceStr,LastSourcePos,UntilSourcePos-LastSourcePos);
          NextChar;
          HasRecursive:=true;
          Node:=NewNode(ntRECURSIVE,nil,nil,nil,-$7fffffff);
          Node^.Value:=-(GetFixupableCaptureIndex(Identifier)+1);
         end else begin
          SyntaxError;
         end;
         Expect(ord(')'));
        end else begin
         if IsBackTracking then begin
          SyntaxError;
         end else begin
          CollectError;
         end;
        end;
        exit;
       end;
       ord('>'):begin
        if IsBacktracking then begin
         NextChar;
         Node:=NewNode(ntSTOP,ParseDisjunction(true),nil,nil,0);
         Expect(ord(')'));
        end else begin
         CollectError;
        end;
        exit;
       end;
       ord('('):begin
        if IsBacktracking then begin
         NextChar;
         case CurrentChar of
          ord('1')..ord('9'):begin
           i:=0;
           while (not AtEOF) and (CurrentChar>=ord('0')) and (CurrentChar<=ord('9')) do begin
            i:=(i*10)+longint(CurrentChar-ord('0'));
            NextChar;
           end;
           if i=0 then begin
            SyntaxError;
           end;
           Expect(ord(')'));
           NodeA:=ParseAlternative;
           if (not AtEOF) and (CurrentChar=ord('|')) then begin
            Expect(ord('|'));
            NodeB:=ParseAlternative;
           end else begin
            NodeB:=nil;
           end;
           Node:=NewNode(ntTESTBACKREF,NodeA,NodeB,nil,0);
          end;
          ord('?'):begin
           NextChar;
           if (not AtEOF) and (CurrentChar=ord('=')) then begin
            NodeC:=ParseDisjunction(true);
            Expect(ord(')'));
            NodeA:=ParseAlternative;
            if (not AtEOF) and (CurrentChar=ord('|')) then begin
             Expect(ord('|'));
             NodeB:=ParseAlternative;
            end else begin
             NodeB:=nil;
            end;
            NodeB:=ParseAlternative;
            Node:=NewNode(ntTEST,NodeA,NodeB,NodeC,0);
           end else begin
            SyntaxError;
           end;
          end;
          else begin
           SyntaxError;
          end;
         end;
        end else begin
         CollectError;
         exit;
        end;
       end;
       ord('='),ord('!'):begin
        if IsBacktracking then begin
         IsNegative:=CurrentChar=ord('!');
         NextChar;
         if IsNegative then begin
          Node:=NewNode(ntLOOKAHEADFALSE,ParseDisjunction(true),nil,nil,0);
         end else begin
          Node:=NewNode(ntLOOKAHEADTRUE,ParseDisjunction(true),nil,nil,0);
         end;
        end else begin
         CollectError;
         exit;
        end;
       end;
       else begin
        if IsBacktracking then begin
         SyntaxError;
        end else begin
         CollectError;
        end;
        exit;
       end;
      end;
     end else begin
      if IsBacktracking then begin
       SyntaxError;
      end else begin
       CollectError;
      end;
      exit;
     end;
    end else begin
     if (Flags and brrefWHOLEONLY)<>0 then begin
      Node:=ParseDisjunction(true);
     end else begin
      i:=RegExp.CountOfCaptures;
      inc(RegExp.CountOfCaptures);
      Identifier:=AnsiString(IntToStr(i));
      CaptureIdentifierHashMap.SetValue(Identifier,i);
      if i<CaptureIdentifiers.Count then begin
       CaptureIdentifiers[i]:=String(Identifier);
      end else if i=CaptureIdentifiers.Count then begin
       CaptureIdentifiers.Add(String(Identifier));
      end else begin
       while CaptureIdentifiers.Count<i do begin
        CaptureIdentifiers.Add('-');
       end;
       CaptureIdentifiers.Add(String(Identifier));
      end;
      Node:=ParseDisjunction(true);
      if i<CaptureNodes.Count then begin
       CaptureNodes[i]:=Node;
      end else if i=CaptureNodes.Count then begin
       CaptureNodes.Add(Node);
      end else begin
       while CaptureNodes.Count<i do begin
        CaptureNodes.Add(nil);
       end;
       CaptureNodes.Add(Node);
      end;
      Node:=NewNode(ntPAREN,Node,nil,nil,i);
     end;
     Expect(ord(')'));
     result:=0;
     exit;
    end;
    Expect(ord(')'));
    Identifier:='';
    result:=0;
    exit;
   end;
   ord('\'):begin
    NextChar;
    if AtEOF then begin
     if IsBacktracking then begin
      SyntaxError;
     end else begin
      CollectError;
     end;
    end;
    case CurrentChar of
     ord('b'),ord('y'):begin
      NextChar;
      Node:=NewNode(ntBRK,nil,nil,nil,0);
      result:=1;
      exit;
     end;
     ord('B'),ord('Y'):begin
      NextChar;
      Node:=NewNode(ntNBRK,nil,nil,nil,0);
      result:=1;
      exit;
     end;
     ord('A'):begin
      NextChar;
      if (Flags and brrefMULTILINE)<>0 then begin
       Node:=NewNode(ntBOML,nil,nil,nil,0);
      end else begin
       Node:=NewNode(ntBOSL,nil,nil,nil,0);
      end;
      result:=1;
      exit;
     end;
     ord('Z'):begin
      NextChar;
      if (Flags and brrefMULTILINE)<>0 then begin
       Node:=NewNode(ntEOML,nil,nil,nil,0);
      end else begin
       Node:=NewNode(ntEOSL,nil,nil,nil,0);
      end;
      result:=1;
      exit;
     end;
     ord('k'):begin
      NextChar;
      Node:=NewNode(ntBOS,nil,nil,nil,0);
      result:=1;
      exit;
     end;
     ord('z'):begin
      NextChar;
      Node:=NewNode(ntEOS,nil,nil,nil,0);
      result:=1;
      exit;
     end;
     ord('G'):begin
      if IsBackTracking then begin
       NextChar;
       Node:=NewNode(ntCONTINUE,nil,nil,nil,0);
      end else begin
       CollectError;
      end;
      result:=1;
      exit;
     end;
     ord('g'):begin
      if IsBackTracking then begin
       LastSourcePos:=SourcePos;
       NextChar;
       if not AtEOF then begin
        case CurrentChar of
         ord('1')..ord('9'):begin
          i:=ParseInteger;
          if (Flags and brrefIGNORECASE)<>0 then begin
           Node:=NewNode(ntBACKREFI,nil,nil,nil,i);
          end else begin
           Node:=NewNode(ntBACKREF,nil,nil,nil,i);
          end;
          if RegExp.MaxRef<i then begin
           RegExp.MaxRef:=i;
          end;
          exit;
         end;
         ord('{'),ord('<'),ord(''''):begin
          ch:=CurrentChar;
          LastSourcePos:=SourcePos;
          NextChar;
          if (not AtEOF) and ((CurrentChar=ord('+')) or (CurrentChar=ord('-'))) then begin
           if CurrentChar=ord('+') then begin
            NextChar;
            i:=RegExp.CountOfCaptures+ParseInteger;
           end else begin
            NextChar;
            i:=RegExp.CountOfCaptures-ParseInteger;
            if i<0 then begin
             SyntaxError;
            end;
           end;
           if (ch=ord('<')) or (ch=ord('''')) then begin
            if (Flags and (brrefWHOLEONLY or brrefCOMBINEOPTIMIZATION))<>0 then begin
             SyntaxError;
            end;
            HasRecursive:=true;
            Node:=NewNode(ntRECURSIVE,nil,nil,nil,i);
           end else begin
            if (Flags and brrefIGNORECASE)<>0 then begin
             Node:=NewNode(ntBACKREFI,nil,nil,nil,i);
            end else begin
             Node:=NewNode(ntBACKREF,nil,nil,nil,i);
            end;
            if RegExp.MaxRef<i then begin
             RegExp.MaxRef:=i;
            end;
           end;
           if ((not AtEOF) and (((ch=ord('{')) and (CurrentChar=ord('}')) or ((ch=ord('<')) and (CurrentChar=ord('>'))) or ((ch=ord('''')) and (CurrentChar=ord('''')))))) then begin
            NextChar;
           end else begin
            SyntaxError;
           end;
           exit;
          end else begin
           UntilSourcePos:=LastSourcePos;
           while (not AtEOF) and (BRREUnicodeIsIDPart(CurrentChar)) do begin
            UntilSourcePos:=SourcePos;
            NextChar;
           end;
           if (LastSourcePos<UntilSourcePos) and ((not AtEOF) and (((ch=ord('{')) and (CurrentChar=ord('}')) or ((ch=ord('<')) and (CurrentChar=ord('>'))) or ((ch=ord('''')) and (CurrentChar=ord('''')))))) then begin
            Identifier:=copy(SourceStr,LastSourcePos,UntilSourcePos-LastSourcePos);
            NextChar;
            if (ch=ord('<')) or (ch=ord('''')) then begin
             if (Flags and (brrefWHOLEONLY or brrefCOMBINEOPTIMIZATION))<>0 then begin
              SyntaxError;
             end;
             HasRecursive:=true;
             Node:=NewNode(ntRECURSIVE,nil,nil,nil,-$7fffffff);
            end else begin
             if (Flags and brrefIGNORECASE)<>0 then begin
              Node:=NewNode(ntBACKREFI,nil,nil,nil,-$7fffffff);
             end else begin
              Node:=NewNode(ntBACKREF,nil,nil,nil,-$7fffffff);
             end;
            end;
            Node^.Value:=-(GetFixupableCaptureIndex(Identifier)+1);
            Identifier:='';
            exit;
           end else begin
            SyntaxError;
           end;
          end;
         end;
         ord('+'):begin
          NextChar;
          i:=RegExp.CountOfCaptures+ParseInteger;
          if (Flags and brrefIGNORECASE)<>0 then begin
           Node:=NewNode(ntBACKREFI,nil,nil,nil,i);
          end else begin
           Node:=NewNode(ntBACKREF,nil,nil,nil,i);
          end;
          if RegExp.MaxRef<i then begin
           RegExp.MaxRef:=i;
          end;
          exit;
         end;
         ord('-'):begin
          NextChar;
          i:=RegExp.CountOfCaptures-ParseInteger;
          if i<0 then begin
           SyntaxError;
          end;
          if (Flags and brrefIGNORECASE)<>0 then begin
           Node:=NewNode(ntBACKREFI,nil,nil,nil,i);
          end else begin
           Node:=NewNode(ntBACKREF,nil,nil,nil,i);
          end;
          if RegExp.MaxRef<i then begin
           RegExp.MaxRef:=i;
          end;
          exit;
         end;
         else begin
          SyntaxError;
         end;
        end;
       end else begin
        SyntaxError;
       end;
      end else begin
       CollectError;
       exit;
      end;
     end;
     ord('1')..ord('9'):begin
      if IsBackTracking then begin
       i:=ParseInteger;
       if (Flags and brrefIGNORECASE)<>0 then begin
        Node:=NewNode(ntBACKREFI,nil,nil,nil,i);
       end else begin
        Node:=NewNode(ntBACKREF,nil,nil,nil,i);
       end;
       if RegExp.MaxRef<i then begin
        RegExp.MaxRef:=i;
       end;
      end else begin
       CollectError;
      end;
      exit;
     end;
     ord('Q'):begin
      while not AtEOF do begin
       if CurrentChar=ord('\') then begin
        NextChar;
        if (not EOF) and (CurrentChar=ord('E')) then begin
         NextChar;
         break;
        end else begin
         c:=TBRRERegExpCharClass.Create(RegExp);
         c.AddChar(ord('\'),(Flags and brrefIGNORECASE)<>0);
         c.Canonicalized:=(Flags and brrefIGNORECASE)<>0;
         i:=RegExp.AddCharClass(c);
         if assigned(Node) then begin
          Node:=Concat(Node,ConvertUTF8CharClassNode(NewNode(ntCHAR,nil,nil,nil,i)));
         end else begin
          Node:=ConvertUTF8CharClassNode(NewNode(ntCHAR,nil,nil,nil,i));
         end;
        end;
       end else begin
        c:=TBRRERegExpCharClass.Create(RegExp);
        c.AddChar(CurrentChar,(Flags and brrefIGNORECASE)<>0);
        c.Canonicalized:=(Flags and brrefIGNORECASE)<>0;
        i:=RegExp.AddCharClass(c);
        if assigned(Node) then begin
         Node:=Concat(Node,ConvertUTF8CharClassNode(NewNode(ntCHAR,nil,nil,nil,i)));
        end else begin
         Node:=ConvertUTF8CharClassNode(NewNode(ntCHAR,nil,nil,nil,i));
        end;
        NextChar;
       end;
      end;
      exit;
     end;
     ord('p'):begin
      NextChar;
      c:=TBRRERegExpCharClass.Create(RegExp);
      if not ParseClassEscapeUnicodeProperty(c,true) then begin
       SyntaxError;
      end;
      c.Canonicalized:=true;
     end;
     ord('P'):begin
      NextChar;
      c:=TBRRERegExpCharClass.Create(RegExp);
      if not ParseClassEscapeUnicodeProperty(c,true) then begin
       SyntaxError;
      end;
      c.Invert;
      c.Inverted:=false;
      c.Canonicalized:=true;
     end;
     else begin
      c:=ParseClassEscape(true);
     end;
    end;
   end;
   ord('['):begin
    if TestClassPOSIXCharacterClass then begin
     c:=TBRRERegExpCharClass.Create(RegExp);
     if not ParseClassPOSIXCharacterClass(c,true) then begin
      SyntaxError;
     end;
    end else begin
     c:=ParseCharacterClass;
    end;
   end;
   ord('.'):begin
    NextChar;
    c:=TBRRERegExpCharClass.Create(RegExp);
    if (Flags and brrefSINGLELINE)<>0 then begin
     c.AddRange($0,$ffffffff);
    end else begin
     c.AddChar($000a);
     c.AddChar($000d);
     c.AddRange($2028,$2029);
     c.Invert;
    end;
    c.Inverted:=false;
    c.Canonicalized:=true;
   end;
   else begin
    c:=TBRRERegExpCharClass.Create(RegExp);
    c.AddChar(CurrentChar,(Flags and brrefIGNORECASE)<>0);
    c.Canonicalized:=(Flags and brrefIGNORECASE)<>0;
    NextChar;
   end;
  end;
  if assigned(c) then begin
   i:=RegExp.AddCharClass(c);
   Node:=ConvertUTF8CharClassNode(NewNode(ntCHAR,nil,nil,nil,i));
   Identifier:='';
  end else begin
   if IsBacktracking then begin
    SyntaxError;
   end else begin
    CollectError;
   end;
  end;
 except
  Identifier:='';
  if assigned(c) then begin
   FreeAndNil(c);
  end;
  raise;
 end;
end;

function TBRRERegExpCompiler.ParseTerm:PBRRERegExpASTNode;
var Min,Max:longint;
    Greedy,Possessive:boolean;
    Node,ResultNode:PBRRERegExpASTNode;
begin
 result:=nil;
 try
  while not AtEOF do begin
   Node:=nil;
   case ParseAtom(Node) of
    -1:begin
     if assigned(result) then begin
      result:=Concat(result,Node);
     end else begin
      result:=Node;
     end;
     break;
    end;
    1:begin
     if assigned(result) then begin
      result:=Concat(result,Node);
     end else begin
      result:=Node;
     end;
     SkipFreespacingWhiteSpace;
     continue;
    end
    else begin
    end;
   end;

   if AtEOF then begin
    Min:=1;
    Max:=1;
   end else if CurrentChar=ord('*') then begin
    NextChar;
    Min:=0;
    Max:=brrecINFINITY;
   end else if CurrentChar=ord('+') then begin
    NextChar;
    Min:=1;
    Max:=brrecINFINITY;
   end else if CurrentChar=ord('?') then begin
    NextChar;
    Min:=0;
    Max:=1;
   end else if QuantifierIsNext then begin
    NextChar;
    if (not AtEOF) and ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) then begin
     Min:=ParseInteger;
     if (Min<0) or (Min>1000) then begin
      SyntaxError;
     end;
    end else begin
     Min:=0;
    end;
    if (not AtEOF) and (CurrentChar=ord(',')) then begin
     Expect(ord(','));
     if (not AtEOF) and (CurrentChar=ord('}')) then begin
      Max:=brrecINFINITY;
     end else begin
      Max:=ParseInteger;
      if (Min>Max) or ((Max<=0) or (Max>1000)) then begin
       SyntaxError;
      end;
     end;
    end else begin
     Max:=Min;
    end;
    Expect(ord('}'));
   end else begin
    Min:=1;
    Max:=1;
   end;

   Greedy:=true;
   Possessive:=false;
   if not AtEOF then begin
    case CurrentChar of
     ord('?'):begin
      NextChar;
      Greedy:=false;
      if HasBacktrackingParent and not IsBacktracking then begin
       CollectError;
      end;
      if (Flags and brrefLONGEST)<>0 then begin
       SyntaxError;
      end;
     end;
     ord('+'):begin
      NextChar;
      Possessive:=true;
      if (Flags and brrefLONGEST)<>0 then begin
       SyntaxError;
      end;
     end;
    end;
   end;

   if ((Flags and (brrefUNGREEDY or brrefLONGEST))=brrefUNGREEDY) and (Greedy and not Possessive) then begin
    Greedy:=false;
   end;

   if (Min=Max) and not Greedy then begin
    Greedy:=true;
   end;

   if (Max<>brrecINFINITY) and (Min>Max) then begin
    if IsBacktracking then begin
     SyntaxError;
    end else begin
     CollectError;
    end;
   end;

   if (Min=1) and (Max=1) then begin
    if assigned(result) then begin
     result:=Concat(result,Node);
    end else begin
     result:=Node;
    end;
    exit;
   end;

   if Possessive then begin
    // Possessive
    ResultNode:=NewRepeat(Node,brreqkPOSSESSIVE,Min,Max);
   end else if Greedy then begin
    // Greedy
    ResultNode:=NewRepeat(Node,brreqkGREEDY,Min,Max);
   end else begin
    // Lazy
    ResultNode:=NewRepeat(Node,brreqkLAZY,Min,Max);
   end;

   if assigned(ResultNode) then begin
    if assigned(result) then begin
     result:=Concat(result,ResultNode);
    end else begin
     result:=ResultNode;
    end;
   end else begin
    if IsBacktracking then begin
     SyntaxError;
    end else begin
     CollectError;
    end;
   end;
   break;
  end;
 except
  raise;
 end;
end;

function TBRRERegExpCompiler.ParseAlternative:PBRRERegExpASTNode;
begin
 result:=nil;
 try
  SkipFreespacingWhiteSpace;
  while not AtEOF do begin
   case CurrentChar of
    ord(')'):begin
     break;
    end;
    ord('|'):begin
     break;
    end;
    else begin
     if assigned(result) then begin
      result:=Concat(result,ParseTerm);
     end else begin
      result:=ParseTerm;
     end;
     SkipFreespacingWhiteSpace;
    end;
   end;
  end;
 except
  raise;
 end;
end;

function TBRRERegExpCompiler.ParseDisjunction(AllowOptimizations:boolean):PBRRERegExpASTNode;
var OldSourcePos,OldCountOfCaptures,i:longint;
    OldCurrentChar:longword;
    OldAtEOF,OldIsFirstChar:boolean;
    OldFlags:TBRRERegExpFlags;
    TheRegExpProgram:TBRRERegExpProgram;
    CaptureSnapshot:TBRRERegExpCaptureSnapshot;
    DoTryIt:boolean;
begin
 OldSourcePos:=SourcePos;
 OldCurrentChar:=CurrentChar;
 OldCountOfCaptures:=RegExp.CountOfCaptures;
 OldAtEOF:=AtEOF;
 OldIsFirstChar:=IsFirstChar;
 OldFlags:=Flags;
 DoTryIt:=AllowOptimizations and (((Flags and brrefAUTOCALLOUT)=0) and (((IsFirstChar or ((Flags and brrefCOMBINEOPTIMIZATION)<>0)) and (IsBacktracking and (((Flags and brrefBACKTRACKING)=0)))))) and not HasRecursive;
 try
  if DoTryIt then begin
   CaptureSnapshot:=CreateCaptureSnapshot;
  end;
  i:=0;
  if DoTryIt and Pass(false,i,not IsFirstChar) then begin
   TheRegExpProgram:=TBRRERegExpProgram(RegExp.RegExpPrograms[i]);
   if (length(TheRegExpProgram.FixedString)>0) and not TheRegExpProgram.FixedStringIsPrefix then begin
    TheRegExpProgram.Method:=brrepmFIXEDSTRINGSEARCH;
    result:=NewNode(ntFIXEDSTRINGSEARCH,nil,nil,nil,i);
   end else begin
    if TheRegExpProgram.IsOnePass then begin
     TheRegExpProgram.Method:=brrepmONEPASSNFA;
     result:=NewNode(ntONEPASS,nil,nil,nil,i);
    end else begin
     TheRegExpProgram.Method:=brrepmPARALLELNFA;
     result:=NewNode(ntPARALLEL,nil,nil,nil,i);
    end;
   end;
  end else begin
   SourcePos:=OldSourcePos;
   CurrentChar:=OldCurrentChar;
   RegExp.CountOfCaptures:=OldCountOfCaptures;
   AtEOF:=OldAtEOF;
   IsFirstChar:=OldIsFirstChar;
   Flags:=OldFlags;
   if DoTryIt then begin
    RestoreCaptureSnapshot(CaptureSnapshot);
   end;
   result:=ParseAlternative;
   if (not AtEOF) and (CurrentChar=ord('|')) then begin
    NextChar;
    result:=NewAlt(result,ParseDisjunction(true));
   end;
  end;
 finally
  if DoTryIt then begin
   ReleaseCaptureSnapshot(CaptureSnapshot);
  end;
 end;
end;

function TBRRERegExpCompiler.CountInstructions(Node:PBRRERegExpASTNode):longint;
begin
 result:=0;
 if assigned(Node) then begin
  case Node^.NodeType of
   ntNONE:begin
   end;
   ntALT:begin
    result:=2+CountInstructions(Node^.Left)+CountInstructions(Node^.Right);
   end;
   ntCAT:begin
    result:=2+CountInstructions(Node^.Left)+CountInstructions(Node^.Right);
   end;
   ntSTAR:begin
    result:=4+CountInstructions(Node^.Left);
   end;
   ntQUEST:begin
    result:=4+CountInstructions(Node^.Left);
   end;
   ntPLUS:begin
    if IsBacktracking and (Node^.Value<>brreqkGREEDY) then begin
     result:=4+(2*CountInstructions(Node^.Left));
    end else begin
     result:=4+CountInstructions(Node^.Left);
    end;
   end;
   ntPAREN:begin
    result:=2+CountInstructions(Node^.Left);
   end;
   ntCHAR:begin
    if IsBacktracking and ((Flags and brrefAUTOCALLOUT)<>0) then begin
     result:=2;
    end else begin
     result:=1;
    end;
   end;
   ntDOT:begin
    result:=1;
   end;
   ntBOML:begin
    result:=1;
   end;
   ntEOML:begin
    result:=1;
   end;
   ntBOSL:begin
    result:=1;
   end;
   ntEOSL:begin
    result:=1;
   end;
   ntBOS:begin
    result:=1;
   end;
   ntEOS:begin
    result:=1;
   end;
   ntBRK:begin
    result:=1;
   end;
   ntNBRK:begin
    result:=1;
   end;
   ntEXACT:begin
    result:=3+CountInstructions(Node^.Left);
   end;
   ntREPEAT:begin
    result:=3+CountInstructions(Node^.Left);
   end;
   ntSTOP:begin
    result:=2+CountInstructions(Node^.Left);
   end;
   ntBACKREF:begin
    result:=1;
   end;
   ntBACKREFI:begin
    result:=1;
   end;
   ntTESTBACKREF:begin
    result:=2+CountInstructions(Node^.Left);
   end;
   ntTEST:begin
    result:=3+CountInstructions(Node^.Left);
   end;
   ntCONTINUE:begin
    result:=1;
   end;
   ntLOOKAHEADFALSE:begin
    result:=2+CountInstructions(Node^.Left);
   end;
   ntLOOKAHEADTRUE:begin
    result:=2+CountInstructions(Node^.Left);
   end;
   ntLOOKBEHINDFALSE:begin
    result:=2+CountInstructions(Node^.Left);
   end;
   ntLOOKBEHINDTRUE:begin
    result:=2+CountInstructions(Node^.Left);
   end;
   ntPARALLEL:begin
    result:=1;
   end;
   ntONEPASS:begin
    result:=1;
   end;
   ntFIXEDSTRINGSEARCH:begin
    result:=1;
   end;
   ntCALLOUT:begin
    result:=1;
   end;
   ntRECURSIVE:begin
    result:=1;
   end;
  end;
 end;
end;

procedure TBRRERegExpCompiler.NumberizeInstructions(StartInstruction,EndInstruction:PBRRERegExpNFAInstruction);
var Instruction:PBRRERegExpNFAInstruction;
    ID:longword;
begin
 Instruction:=StartInstruction;
 ID:=0;
 while assigned(Instruction) and (Instruction<>EndInstruction) do begin
  Instruction^.IDandOpcode:=ptruint(ptruint(Instruction^.IDandOpcode and $ff) or ptruint(ID shl 8));
  inc(ID);
  inc(Instruction);
 end;
end;

procedure TBRRERegExpCompiler.OptimizeInstructions(StartInstruction,EndInstruction:PBRRERegExpNFAInstruction); // Simple recursive-loop peephole optimizer
var Instruction:PBRRERegExpNFAInstruction;
    DoIt:boolean;
begin
 DoIt:=true;
 while DoIt do begin
  DoIt:=false;
  Instruction:=StartInstruction;
  while assigned(Instruction) and (Instruction<>EndInstruction) do begin
   if (Instruction^.IDandOpcode and $ff)=brreoTEST then begin
    while assigned(pointer(ptruint(Instruction^.Value))) and ((PBRRERegExpNFAInstruction(pointer(ptruint(Instruction^.Value)))^.IDandOpcode and $ff)=brreoJMP) do begin
     PBRRERegExpNFAInstruction(pointer(ptruint(Instruction^.Value))):=PBRRERegExpNFAInstruction(pointer(ptruint(Instruction^.Value)))^.x;
     DoIt:=true;
    end;
   end;
   while assigned(Instruction^.x) and ((Instruction^.x^.IDandOpcode and $ff)=brreoJMP) do begin
    Instruction^.x:=Instruction^.x^.x;
    DoIt:=true;
   end;
   while assigned(Instruction^.y) and ((Instruction^.y^.IDandOpcode and $ff)=brreoJMP) do begin
    Instruction^.y:=Instruction^.y^.x;
    DoIt:=true;
   end;
   inc(Instruction);
  end;
 end;
end;

procedure TBRRERegExpCompiler.FinalizeInstructions(StartInstruction,EndInstruction:PBRRERegExpNFAInstruction); // Simple recursive-loop peephole optimizer
var Instruction:PBRRERegExpNFAInstruction;
begin
 Instruction:=StartInstruction;
 while assigned(Instruction) and (Instruction<>EndInstruction) do begin
  case Instruction^.IDandOpcode and $ff of
   brreoRECURSIVE:begin
    if (Instruction^.Value>=0) and (Instruction^.Value<CaptureInstructions.Count) then begin
     Instruction^.x:=CaptureInstructions[Instruction^.Value];
    end;
   end;
  end;
  inc(Instruction);
 end;
end;

function TBRRERegExpCompiler.OptimizeNode(NodeEx:PPBRRERegExpASTNode):boolean;
 procedure ParseNodes(NodeList:TBRREPointerList;n:PBRRERegExpASTNode);
 begin
  while assigned(n) do begin
   if n^.NodeType=ntCAT then begin
    if assigned(n^.Right) then begin
     if n^.Right^.NodeType=ntCAT then begin
      ParseNodes(NodeList,n^.Right);
     end else begin
      NodeList.Add(n^.Right);
     end;
    end;
    if assigned(n^.Left) then begin
     if n^.Left^.NodeType=ntCAT then begin
      n:=n^.Left;
     end else begin
      NodeList.Add(n^.Left);
      break;
     end;
    end else begin
     break;
    end;
   end else begin
    NodeList.Add(n);
    break;
   end;
  end;
 end;
var Node,l,r:PBRRERegExpASTNode;
    pr,pl:PPBRRERegExpASTNode;
    DoContinue,Optimized:boolean;
    NodeList:TBRREPointerList;
    NodeIndex:longint;
    c:TBRRERegExpCharClass;
begin
 result:=false;
 DoContinue:=true;
 while DoContinue and (assigned(NodeEx) and assigned(NodeEx^)) do begin
  DoContinue:=false;
  Node:=NodeEx^;
  if assigned(Node) then begin
   case Node^.NodeType of
    ntNONE,ntCHAR,ntDOT,ntBOML,ntEOML,ntBOSL,ntEOSL,ntBOS,ntEOS,ntBRK,ntNBRK,ntBACKREF,ntBACKREFI,ntCONTINUE,ntPARALLEL,ntONEPASS,ntFIXEDSTRINGSEARCH,ntCALLOUT,ntRECURSIVE:begin
    end;
    ntPAREN,ntEXACT,ntREPEAT,ntSTOP:begin
     NodeEx:=@Node^.Left;
     DoContinue:=true;
    end;
    ntTESTBACKREF,ntLOOKAHEADFALSE,ntLOOKAHEADTRUE,ntLOOKBEHINDFALSE,ntLOOKBEHINDTRUE:begin
     if OptimizeNode(@Node^.Left) then begin
      result:=true;
     end;
     NodeEx:=@Node^.Right;
     DoContinue:=true;
    end;
    ntTEST:begin
     if OptimizeNode(@Node^.Left) then begin
      result:=true;
     end;
     if OptimizeNode(@Node^.Right) then begin
      result:=true;
     end;
     NodeEx:=@Node^.Extra;
     DoContinue:=true;
    end;
    ntPLUS:begin
     if (assigned(Node^.Left) and (Node^.Left^.NodeType=ntPAREN)) and (assigned(Node^.Left^.Left) and (Node^.Left^.Left^.NodeType in [ntPLUS,ntSTAR])) and ((Node^.Left^.Left^.Value=brreqkGREEDY) and (Node^.Value=brreqkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      DoContinue:=true;
      result:=true;
     end else if (assigned(Node^.Left) and (Node^.Left^.NodeType=ntPLUS)) and ((Node^.Left^.Value=brreqkGREEDY) and (Node^.Value=brreqkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      DoContinue:=true;
      result:=true;
     end else if (assigned(Node^.Left) and (Node^.Left^.NodeType in [ntSTAR,ntQUEST])) and ((Node^.Left^.Value=brreqkGREEDY) and (Node^.Value=brreqkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      NodeEx^^.NodeType:=ntSTAR;
      DoContinue:=true;
      result:=true;
     end else begin
      NodeEx:=@Node^.Left;
      DoContinue:=true;
     end;
    end;
    ntSTAR:begin
     if (assigned(Node^.Left) and (Node^.Left^.NodeType=ntPAREN)) and (assigned(Node^.Left^.Left) and (Node^.Left^.Left^.NodeType in [ntSTAR,ntPLUS{,ntQUEST}])) and ((Node^.Left^.Left^.Value=brreqkGREEDY) and (Node^.Value=brreqkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      NodeEx^^.Left^.NodeType:=ntSTAR;
      DoContinue:=true;
      result:=true;
     end else if (assigned(Node^.Left) and (Node^.Left^.NodeType in [ntPLUS,ntQUEST,ntSTAR])) and ((Node^.Left^.Value=brreqkGREEDY) and (Node^.Value=brreqkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      NodeEx^^.NodeType:=ntSTAR;
      DoContinue:=true;
      result:=true;
     end else begin
      if IsStarNullable(Node^.Left) then begin
       Node:=StarDenull(Node^.Left);
       result:=true;
      end;
      NodeEx:=@Node^.Left;
      DoContinue:=true;
     end;
    end;
    ntQUEST:begin
     if (assigned(Node^.Left) and (Node^.Left^.NodeType=ntPAREN)) and (assigned(Node^.Left^.Left) and (Node^.Left^.Left^.NodeType=ntQUEST)) and ((Node^.Left^.Left^.Value=brreqkGREEDY) and (Node^.Value=brreqkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      DoContinue:=true;
      result:=true;
     end else if (assigned(Node^.Left) and (Node^.Left^.NodeType=ntPAREN)) and (assigned(Node^.Left^.Left) and (Node^.Left^.Left^.NodeType in [ntSTAR,ntPLUS{,ntQUEST}])) and ((Node^.Left^.Left^.Value=brreqkGREEDY) and (Node^.Value=brreqkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      NodeEx^^.Left^.NodeType:=ntSTAR;
      DoContinue:=true;
      result:=true;
     end else if (assigned(Node^.Left) and (Node^.Left^.NodeType=ntQUEST)) and ((Node^.Left^.Value=brreqkGREEDY) and (Node^.Value=brreqkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      DoContinue:=true;
      result:=true;
     end else if (assigned(Node^.Left) and (Node^.Left^.NodeType in [ntPLUS,ntSTAR])) and ((Node^.Left^.Value=brreqkGREEDY) and (Node^.Value=brreqkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      NodeEx^^.NodeType:=ntSTAR;
      DoContinue:=true;
      result:=true;
     end else begin
      NodeEx:=@Node^.Left;
      DoContinue:=true;
     end;
    end;
    ntCAT:begin
     if assigned(Node^.Left) and assigned(Node^.Right) then begin
      if ((Node^.Left^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (Node^.Right^.NodeType=ntPLUS)) and AreNodesEqual(Node^.Left^.Left,Node^.Right^.Left) then begin
       NodeEx^:=Node^.Right;
       DoContinue:=true;
       result:=true;
      end else if ((Node^.Left^.NodeType in [ntSTAR,ntPLUS]) and (Node^.Right^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqual(Node^.Left^.Left,Node^.Right^.Left) then begin
       NodeEx^:=Node^.Left;
       DoContinue:=true;
       result:=true;
      end else begin
       NodeList:=TBRREPointerList.Create;
       try
        ParseNodes(NodeList,Node);
        Optimized:=false;
        DoContinue:=true;
        while DoContinue do begin
         DoContinue:=false;
         NodeIndex:=NodeList.Count-1;
         while NodeIndex>0 do begin
          l:=PBRRERegExpASTNode(NodeList[NodeIndex]);
          r:=PBRRERegExpASTNode(NodeList[NodeIndex-1]);
          if ((l^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (r^.NodeType=ntPLUS)) and AreNodesEqualSafe(l^.Left,r^.Left) then begin
           NodeList.Delete(NodeIndex);
           if NodeIndex>=NodeList.Count then begin
            NodeIndex:=NodeList.Count-1;
           end;
           DoContinue:=true;
           Optimized:=true;
          end else if ((l^.NodeType in [ntSTAR,ntPLUS]) and (r^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(l^.Left,r^.Left) then begin
           NodeList.Delete(NodeIndex-1);
           if NodeIndex>=NodeList.Count then begin
            NodeIndex:=NodeList.Count-1;
           end;
           DoContinue:=true;
           Optimized:=true;
          end else begin
           dec(NodeIndex);
          end;
         end;
        end;
        if Optimized and (NodeList.Count>0) then begin
         NodeEx^:=NodeList[NodeList.Count-1];
         for NodeIndex:=NodeList.Count-2 downto 0 do begin
          NodeEx^:=NewNode(ntCAT,NodeEx^,NodeList[NodeIndex],nil,0);
         end;
         DoContinue:=true;
         result:=true;
        end;
       finally
        FreeAndNil(NodeList);
       end;
       if not Optimized then begin
        if OptimizeNode(@Node^.Right) then begin
         result:=true;
        end;
        NodeEx:=@Node^.Left;
        DoContinue:=true;
       end;
      end;
     end else begin
      if assigned(Node^.Left) then begin
       NodeEx:=@Node^.Left;
      end else begin
       NodeEx:=@Node^.Right;
      end;
      DoContinue:=true;
     end;
    end;
    ntALT:begin
     if assigned(Node^.Left) and assigned(Node^.Right) then begin
      if (Node^.Left^.NodeType=ntCAT) and (Node^.Right^.NodeType=ntCAT) then begin
       pl:=@Node^.Left;
       l:=Node^.Left;
       while (assigned(l) and assigned(l^.Left)) and (l^.Left^.NodeType=ntCAT) do begin
        pl:=@l^.Left;
        l:=l^.Left;
       end;
       pr:=@Node^.Right;
       r:=Node^.Right;
       while (assigned(r) and assigned(r^.Left)) and (r^.Left^.NodeType=ntCAT) do begin
        pr:=@r^.Left;
        r:=r^.Left;
       end;
       if ((assigned(l^.Left) and assigned(l^.Right)) and (assigned(r^.Left) and assigned(r^.Right))) and AreNodesEqualSafe(l^.Left,r^.Left) then begin
        NodeEx^:=l;
        pl^:=l^.Right;
        pr^:=r^.Right;
        l^.Right:=Node;
        DoContinue:=true;
        result:=true;
       end;
      end else if AreNodesEqualSafe(Node^.Left,Node^.Right) then begin
       NodeEx^:=Node^.Left;
       DoContinue:=true;
       result:=true;
      end else if (Node^.Left^.NodeType=ntCHAR) and (Node^.Right^.NodeType=ntCHAR) then begin
       Node^.NodeType:=ntCHAR;
       c:=TBRRERegExpCharClass.Create(RegExp);
       c.Combine(CharClasses[Node^.Left^.Value]);
       c.Combine(CharClasses[Node^.Right^.Value]);
       Node^.Value:=RegExp.AddCharClass(c);
       Node^.Left:=nil;
       Node^.Right:=nil;
       DoContinue:=true;
       result:=true;
      end else begin
       if OptimizeNode(@Node^.Right) then begin
        result:=true;
       end;
       NodeEx:=@Node^.Left;
       DoContinue:=true;
      end;
     end else begin
      if assigned(Node^.Left) then begin
       NodeEx:=@Node^.Left;
      end else begin
       NodeEx:=@Node^.Right;
      end;
      DoContinue:=true;
     end;
    end;
   end;
  end;
 end;
end;

function TBRRERegExpCompiler.Dump(Node:PBRRERegExpASTNode):ansistring;
begin
 result:='';
 if assigned(Node) then begin
  case Node^.NodeType of
   ntCAT:begin
    result:=Dump(Node^.Left)+Dump(Node^.Right);
   end;
   ntALT:begin
    result:='{A:'+Dump(Node^.Left)+'|'+Dump(Node^.Right)+'}';
   end;
   ntPAREN:begin
    result:='('+Dump(Node^.Left)+')';
   end;
   ntCHAR:begin
    result:='['+AnsiString(IntToStr(Node^.Value))+']';
   end;
   ntSTAR:begin
    result:=Dump(Node^.Left)+'*';
    case Node^.Value of
     brreqkLAZY:begin
      result:=result+'?';
     end;
     brreqkPOSSESSIVE:begin
      result:=result+'+';
     end;
    end;
   end;
   ntPLUS:begin
    result:=Dump(Node^.Left)+'+';
    case Node^.Value of
     brreqkLAZY:begin
      result:=result+'?';
     end;
     brreqkPOSSESSIVE:begin
      result:=result+'+';
     end;
    end;
   end;
   ntQUEST:begin
    result:=Dump(Node^.Left)+'?';
    case Node^.Value of
     brreqkLAZY:begin
      result:=result+'?';
     end;
     brreqkPOSSESSIVE:begin
      result:=result+'+';
     end;
    end;
   end;
   ntDOT:begin
    result:='.';
   end;
   ntBOML:begin
    result:='^';
   end;
   ntBOSL:begin
    result:='^';
   end;
   ntEOML:begin
    result:='$';
   end;
   ntEOSL:begin
    result:='$';
   end;
   ntBOS:begin
    result:='\k';
   end;
   ntEOS:begin
    result:='\z';
   end;
   ntBRK:begin
    result:='\b';
   end;
   ntNBRK:begin
    result:='\B';
   end;
   ntREPEAT:begin
    result:=Dump(Node^.Left)+'{'+AnsiString(IntToStr(Node^.Min))+','+AnsiString(IntToStr(Node^.Max))+'}';
    case Node^.Value of
     brreqkLAZY:begin
      result:=result+'?';
     end;
     brreqkPOSSESSIVE:begin
      result:=result+'+';
     end;
    end;
   end;
   ntSTOP:begin
    result:='(?>'+Dump(Node^.Left)+')';
   end;
   ntBACKREF:begin
    result:='\g'+AnsiString(IntToStr(Node^.Value));
   end;
   ntBACKREFI:begin
    result:='\g'+AnsiString(IntToStr(Node^.Value));
   end;
   ntTESTBACKREF:begin
    result:='<?('+AnsiString(IntToStr(Node^.Value))+')'+Dump(Node^.Left)+'|'+Dump(Node^.Right)+')';
   end;
   ntTEST:begin
    result:='<?('+Dump(Node^.Extra)+')'+Dump(Node^.Left)+'|'+Dump(Node^.Right)+')';
   end;
   ntCONTINUE:begin
    result:='\G';
   end;
   ntLOOKAHEADFALSE:begin
    result:='(?!'+Dump(Node^.Left)+')';
   end;
   ntLOOKAHEADTRUE:begin
    result:='(?='+Dump(Node^.Left)+')';
   end;
  end;
 end;
end;

procedure TBRRERegExpCompiler.CompileRange(RegExpProgram:TBRRERegExpProgram;StartInstruction:PBRRERegExpNFAInstruction);
var LowRangeString,HighRangeString:ansistring;
    LastIndex,LastMatchIndex,RangeStringLength:longint;
 function AddChars(Index:longint;const Str:ansistring):longint;
 var Len,Counter,NewLen:longint;
 begin
  Len:=length(Str);
  result:=Index+Len;
  if RangeStringLength<result then begin
   Counter:=RangeStringLength+1;
   RangeStringLength:=result;
   NewLen:=BRRERoundUpToPowerOfTwo(result);
   if NewLen<16 then begin
    NewLen:=16;
   end;
   if length(LowRangeString)<NewLen then begin
    SetLength(LowRangeString,NewLen);
   end;
   if length(HighRangeString)<NewLen then begin
    SetLength(HighRangeString,NewLen);
   end;
   while Counter<=result do begin
    LowRangeString[Counter]:=Str[Counter-Index];
    HighRangeString[Counter]:=Str[Counter-Index];
    inc(Counter);
   end;
  end;
  for Counter:=1 to Len do begin
   if LowRangeString[Counter+Index]>Str[Counter] then begin
    LowRangeString[Counter+Index]:=Str[Counter];
   end;
   if HighRangeString[Counter+Index]<Str[Counter] then begin
    HighRangeString[Counter+Index]:=Str[Counter];
   end;
  end;
 end;
 procedure ThreadPass(Instruction:PBRRERegExpNFAInstruction;Index:longint);
 var CharClass:TBRRERegExpCharClass;
 begin
  while assigned(Instruction) do begin
   case Instruction^.IDandOpcode and $ff of
    brreoSPLIT,brreoSPLITLONGEST:begin
     if Instruction^.Value in [brreoskALT,brreoskQUEST] then begin
      ThreadPass(Instruction^.y,Index);
      Instruction:=Instruction^.x;
     end else begin
      if (LastIndex<0) or (Index<LastIndex) then begin
       LastIndex:=Index;
      end;
      break;
     end;
    end;
    brreoCHAR:begin
     CharClass:=TBRRERegExpCharClass(pointer(Instruction^.Value));
     if assigned(CharClass.First) then begin
      if CharClass.IsSingle then begin
       if RegExpIsUTF8<>brresuNOUTF8 then begin
        Index:=AddChars(Index,BRREUTF32CharToUTF8(CharClass.First.Lo));
       end else begin
        Index:=AddChars(Index,ansichar(byte(CharClass.First.Lo)));
       end;
      end else begin
       if RegExpIsUTF8<>brresuNOUTF8 then begin
        ThreadPass(Instruction^.x,AddChars(Index,BRREUTF32CharToUTF8(CharClass.Last.Hi)));
        Index:=AddChars(Index,BRREUTF32CharToUTF8(CharClass.First.Lo));
       end else begin
        AddChars(Index,ansichar(byte(CharClass.First.Lo)));
        Index:=AddChars(Index,ansichar(byte(CharClass.Last.Hi)));
       end;
      end;
     end;
     Instruction:=Instruction^.x;
    end;
    brreoCHARSINGLE:begin
     if RegExpIsUTF8<>brresuNOUTF8 then begin
      Index:=AddChars(Index,BRREUTF32CharToUTF8(Instruction^.Value));
     end else begin
      Index:=AddChars(Index,ansichar(byte(Instruction^.Value)));
     end;
     Instruction:=Instruction^.x;
    end;
    brreoJMP,brreoBEGIN,brreoEND,brreoBOML,brreoEOML,brreoBOSL,brreoEOSL,brreoBOS,brreoEOS,brreoBRK,brreoNBRK,brreoPREFIX,brreoPREFIXCHAR:begin
     Instruction:=Instruction^.x;
    end;
    brreoPARALLEL,brreoONEPASS:begin
     if TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).HasRange then begin
      ThreadPass(Instruction^.x,AddChars(Index,TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).RangeHigh));
      Index:=AddChars(Index,TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).RangeLow);
      Instruction:=Instruction^.x;
     end else begin
      if (LastIndex<0) or (Index<LastIndex) then begin
       LastIndex:=Index;
      end;
      break;
     end;
    end;
    brreoFIXEDSTRINGSEARCH:begin
     if length(TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).FixedString)>0 then begin
      Index:=AddChars(Index,TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).FixedString);
      Instruction:=Instruction^.x;
     end else begin
      if (LastIndex<0) or (Index<LastIndex) then begin
       LastIndex:=Index;
      end;
      break;
     end;
    end;
    brreoMATCH:begin
     if (LastMatchIndex<0) or (Index<LastMatchIndex) then begin
      LastMatchIndex:=Index;
     end;
     break;
    end;
    else begin
     if (LastIndex<0) or (Index<LastIndex) then begin
      LastIndex:=Index;
     end;
     break;
    end;
   end;
  end;
 end;
begin
 LowRangeString:='';
 HighRangeString:='';
 if not RegExpProgram.HasRange then begin
  RegExpProgram.RangeLow:=#$00;
  RegExpProgram.RangeHigh:=#$ff;
  RangeStringLength:=0;
  try
   LastIndex:=-1;
   LastMatchIndex:=-1;
   if assigned(StartInstruction) then begin
    if (((StartInstruction^.IDandOpcode and $ff)=brreoLAZY) and assigned(StartInstruction^.x) and assigned(StartInstruction^.y)) and (((StartInstruction^.x^.IDandOpcode and $ff)=brreoANY) and ((StartInstruction^.y^.IDandOpcode and $ff)=brreoBEGIN)) then begin
     StartInstruction:=StartInstruction^.y;
    end else if (((StartInstruction^.IDandOpcode and $ff) in [brreoSPLIT,brreoSPLITLONGEST]) and (StartInstruction^.Value=brreoskSTAR) and assigned(StartInstruction^.x) and assigned(StartInstruction^.y)) and ((((StartInstruction^.x^.IDandOpcode and $ff)=brreoBEGIN) and (StartInstruction^.x^.Value=0)) and ((StartInstruction^.y^.IDandOpcode and $ff)=brreoANY)) then begin
     StartInstruction:=StartInstruction^.x;
    end;
    ThreadPass(StartInstruction,0);
   end;
   if LastMatchIndex>=0 then begin
    if LastMatchIndex<RangeStringLength then begin
     RangeStringLength:=LastMatchIndex;
//   LastIndex:=LastMatchIndex;
    end;
    if LastMatchIndex<LastIndex then begin
     LastIndex:=LastMatchIndex;
    end;
   end;
   if LastIndex<0 then begin
    while (RangeStringLength>1) and ((LowRangeString[RangeStringLength]=#0) and (HighRangeString[RangeStringLength]=#255)) do begin
     dec(RangeStringLength);
     LastIndex:=RangeStringLength;
    end;
   end;
   if LastIndex=0 then begin
    LowRangeString:=#$00;
    HighRangeString:=#$ff;
    RangeStringLength:=1;
   end else if LastIndex>0 then begin
    while LastIndex>0 do begin
     while (LastIndex>1) and ((LowRangeString[LastIndex]=#0) and (HighRangeString[LastIndex]=#255)) do begin
      dec(LastIndex);
     end;
     if HighRangeString[LastIndex]<#$ff then begin
      inc(HighRangeString[LastIndex]);
     end;
     if (LastIndex<2) or ((LowRangeString[LastIndex]<>#0) or (HighRangeString[LastIndex]<>#255)) then begin
      break;
     end;
    end;
    RangeStringLength:=LastIndex;
   end;
   SetLength(LowRangeString,RangeStringLength);
   SetLength(HighRangeString,RangeStringLength);
   RegExpProgram.RangeLow:=LowRangeString;
   RegExpProgram.RangeHigh:=HighRangeString;
   RegExpProgram.HasRange:=true;
  finally
   LowRangeString:='';
   HighRangeString:='';
  end;
 end;
end;

procedure TBRRERegExpCompiler.CompilePrefix(RootNode:PBRRERegExpASTNode);
type TStackItem=record
      Node:PBRRERegExpASTNode;
      Argument:ptrint;
     end;
var Node:PBRRERegExpASTNode;
    Argument:ptrint;
    StackPointer,Counter:longint;
    NodeStrings:array of ansistring;
    Stack:array of TStackItem;
    Stop:boolean;
begin
 NodeStrings:=nil;
 Stack:=nil;
 Prefix:='';
 PrefixIsWholeRegExp:=false;
 try
  SetLength(NodeStrings,Nodes.Count);
  StackPointer:=Nodes.Count;
  for Counter:=0 to Nodes.Count-1 do begin
   Node:=Nodes[Counter];
   if assigned(Node) then begin
    case Node^.NodeType of
     ntCAT:begin
      inc(StackPointer,2);
     end;
    end;
   end;
  end;
  SetLength(Stack,StackPointer+1);
  for Counter:=0 to length(NodeStrings)-1 do begin
   NodeStrings[Counter]:='';
  end;
  StackPointer:=0;
  if assigned(RootNode) then begin
   Stack[StackPointer].Node:=RootNode;
   Stack[StackPointer].Argument:=0;
   inc(StackPointer);
   PrefixIsWholeRegExp:=true;
  end;
  Stop:=false;
  while StackPointer>0 do begin
   dec(StackPointer);
   Node:=Stack[StackPointer].Node;
   Argument:=Stack[StackPointer].Argument;
   while assigned(Node) do begin
    case Node^.NodeType of
     ntCAT:begin
      case Argument of
       2:begin
        if assigned(Node^.Left) and assigned(Node^.Right) then begin
         NodeStrings[Node^.Index]:=NodeStrings[Node^.Left^.Index]+NodeStrings[Node^.Right^.Index];
        end else if assigned(Node^.Left) then begin
         NodeStrings[Node^.Index]:=NodeStrings[Node^.Left^.Index];
        end else if assigned(Node^.Right) then begin
         NodeStrings[Node^.Index]:=NodeStrings[Node^.Right^.Index];
        end;
       end;
       1:begin
        if assigned(Node^.Right) and not Stop then begin
         Stack[StackPointer].Node:=Node;
         Stack[StackPointer].Argument:=2;
         inc(StackPointer);
         Node:=Node^.Right;
         Argument:=0;
        end else begin
         Argument:=2;
        end;
        continue;
       end;
       else {0:}begin
        NodeStrings[Node^.Index]:='';
        if assigned(Node^.Left) and not Stop then begin
         Stack[StackPointer].Node:=Node;
         Stack[StackPointer].Argument:=1;
         inc(StackPointer);
         Node:=Node^.Left;
         Argument:=0;
         continue;
        end else begin
         if assigned(Node^.Right) and not Stop then begin
          Stack[StackPointer].Node:=Node;
          Stack[StackPointer].Argument:=2;
          inc(StackPointer);
          Node:=Node^.Right;
          Argument:=0;
          continue;
         end;
        end;
       end;
      end;
     end;
     ntEXACT:begin
      case Argument of
       1:begin
        if assigned(Node^.Left) then begin
         for Counter:=1 to Node^.Value do begin
          NodeStrings[Node^.Index]:=NodeStrings[Node^.Index]+NodeStrings[Node^.Left^.Index];
         end;
        end;
       end;
       else {0:}begin
        NodeStrings[Node^.Index]:='';
        if assigned(Node^.Left) and not Stop then begin
         Stack[StackPointer].Node:=Node;
         Stack[StackPointer].Argument:=1;
         inc(StackPointer);
         Node:=Node^.Left;
         Argument:=0;
         continue;
        end;
       end;
      end;
     end;
     ntPLUS:begin
      case Argument of
       1:begin
        if assigned(Node^.Left) then begin
         NodeStrings[Node^.Index]:=NodeStrings[Node^.Left^.Index];
        end;
        PrefixIsWholeRegExp:=false;
        Stop:=true;
       end;
       else {0:}begin
        NodeStrings[Node^.Index]:='';
        if assigned(Node^.Left) and not Stop then begin
         Stack[StackPointer].Node:=Node;
         Stack[StackPointer].Argument:=1;
         inc(StackPointer);
         Node:=Node^.Left;
         Argument:=0;
         continue;
        end;
       end;
      end;
     end;
     ntCHAR:begin
      if assigned(CharClasses[Node^.Value].First) and CharClasses[Node^.Value].IsSingle then begin
       if RegExpIsUTF8<>brresuNOUTF8 then begin
        NodeStrings[Node^.Index]:=BRREUTF32CharToUTF8(CharClasses[Node^.Value].First.Lo);
       end else begin
        NodeStrings[Node^.Index]:=ansichar(byte(CharClasses[Node^.Value].First.Lo));
       end;
      end else begin
       NodeStrings[Node^.Index]:='';
       PrefixIsWholeRegExp:=false;
       Stop:=true;
      end;
     end;
     ntBOML,ntEOML,ntBOSL,ntEOSL,ntBOS,ntEOS,ntBRK,ntNBRK,ntTESTBACKREF,ntTEST,ntLOOKAHEADFALSE,ntLOOKAHEADTRUE,ntLOOKBEHINDFALSE,ntLOOKBEHINDTRUE,ntCALLOUT:begin
      // No-op instruction here, so don't stop but mark it as non-pure-string-literal regular expression  
      PrefixIsWholeRegExp:=false;
     end;
     else begin
      PrefixIsWholeRegExp:=false;
      Stop:=true;
     end;
    end;
    break;
   end;
  end;
  if assigned(RootNode) then begin
   Prefix:=NodeStrings[RootNode^.Index];
  end else begin
   Prefix:='';
  end;
  PrefixIsWholeRegExp:=PrefixIsWholeRegExp and (length(Prefix)>0);
 finally
  SetLength(NodeStrings,0);
  SetLength(Stack,0);
 end;
end;

function TBRRERegExpCompiler.CompileFixedStringSearch(RegExpProgram:TBRRERegExpProgram):boolean;
var c:ansichar;
    i,j,k:integer;
    HasMatch:boolean;
begin
 RegExpProgram.FixedStringLength:=length(RegExpProgram.FixedString);
 RegExpProgram.FixedStringUTF8Length:=BRREUTF8Length(RegExpProgram.FixedString);
 result:=RegExpProgram.FixedStringLength>0;
 RegExpProgram.FixedStringSearchMethod:=brrefssmBRUTEFORCE;
 if result and (length(RegExpProgram.FixedString)>1) then begin
  if ((Flags and (brrefUTF8 or brrefUTF8CODEUNITS))=brrefUTF8) and (length(RegExpProgram.FixedString)<32) then begin
   for c:=low(ansichar) to high(ansichar) do begin
    RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip[c]:=$ffffffff;
   end;
   for i:=1 to RegExpProgram.FixedStringLength do begin
    RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip[RegExpProgram.FixedString[i]]:=RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip[RegExpProgram.FixedString[i]] and not (1 shl (i-1));
   end;
   RegExpProgram.FixedStringSearchMethod:=brrefssmSHIFTOR;
  end else begin
   for c:=low(ansichar) to high(ansichar) do begin
    RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip[c]:=RegExpProgram.FixedStringLength;
   end;
   for i:=1 to RegExpProgram.FixedStringLength do begin
    RegExpProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip[RegExpProgram.FixedString[i]]:=((RegExpProgram.FixedStringLength-(i-1))-1);
   end;
   SetLength(RegExpProgram.FixedStringBoyerMooreNext,RegExpProgram.FixedStringLength+1);
   for j:=0 to RegExpProgram.FixedStringLength do begin
    i:=RegExpProgram.FixedStringLength-1;
    while i>=1 do begin
     HasMatch:=true;
     for k:=1 to j do begin
      if (i-k)<0 then begin
       break;
      end;
      if RegExpProgram.FixedString[(RegExpProgram.FixedStringLength-k)+1]<>RegExpProgram.FixedString[(i-k)+1] then begin
       HasMatch:=false;
       break;
      end;
     end;
     if HasMatch then begin
      break;
     end;
     dec(i);
    end;
    RegExpProgram.FixedStringBoyerMooreNext[j]:=RegExpProgram.FixedStringLength-i;
   end;
   RegExpProgram.FixedStringSearchMethod:=brrefssmBOYERMOORE;
  end;
 end;
end;

function TBRRERegExpCompiler.ConvertToFixedStringSearch(RegExpProgram:TBRRERegExpProgram):boolean;
begin
 RegExpProgram.FixedString:=Prefix;
 RegExpProgram.FixedStringLength:=length(RegExpProgram.FixedString);
 RegExpProgram.FixedStringUTF8Length:=BRREUTF8Length(RegExpProgram.FixedString);
 result:=PrefixIsWholeRegExp and (RegExpProgram.FixedStringLength>0);
 RegExpProgram.FixedStringIsPrefix:=not result;
 RegExpProgram.FixedStringSearchMethod:=brrefssmBRUTEFORCE;
 RegExpProgram.PrefixIsWholeRegExp:=PrefixIsWholeRegExp;
 CompileFixedStringSearch(RegExpProgram);
end;

procedure TBRRERegExpCompiler.CompileByteMap(RegExpProgram:TBRRERegExpProgram);
var Node:PBRRERegExpASTNode;
    i,j,ByteCount:integer;
    CharSetMap:TBRRERegExpCharClassCharSet;
    CharClass:TBRRERegExpCharClass;
    c:ansichar;
begin
 FillChar(RegExpProgram.ByteMap,sizeof(TBRREByteMap),#0);
 FillChar(RegExpProgram.UnByteMap,sizeof(TBRREByteMap),#0);
 ByteCount:=0;
 CharSetMap:=[];
 for i:=0 to Nodes.Count-1 do begin
  Node:=Nodes[i];
  if assigned(Node) then begin
   case Node^.NodeType of
    ntCHAR:begin
     CharClass:=CharClasses[Node^.Value];
     if CharClass.CharSet=[] then begin
      CharClass.CompileCharset;
     end;
     if CharClass.CharSet<>[#0..#255] then begin
      for j:=0 to 255 do begin
       c:=ansichar(byte(j));
       if (c in CharClass.CharSet) and not (c in CharSetMap) then begin
        System.Include(CharSetMap,c);
        RegExpProgram.ByteMap[byte(c)]:=ByteCount;
        RegExpProgram.UnByteMap[ByteCount]:=byte(c);
        inc(ByteCount);
       end;
      end;
     end;
    end;
   end;
  end;
 end;
 if ByteCount<256 then begin
  for i:=0 to 255 do begin
   if ansichar(byte(i)) in CharSetMap then begin
    inc(RegExpProgram.ByteMap[i]);
   end;
  end;
  inc(ByteCount);
 end;
 RegExpProgram.ByteCharSetMap:=CharSetMap;
 RegExpProgram.ByteCount:=ByteCount;
end;

function TBRRERegExpCompiler.CompileOnePassNFA(StartInstruction,EndInstruction:PBRRERegExpNFAInstruction;RegExpProgram:TBRRERegExpProgram):boolean;
type TStackItem=record
      Instruction:PBRRERegExpNFAInstruction;
      Condition:longword;
     end;
     TStack=array of TStackItem;
var Instruction:PBRRERegExpNFAInstruction;
    ToVisit,WorkQueue:TBRREPointerList;
    Stack:TStack;
    StackPointer,Len,MaxNodes,StateSize,i,NextIndex,NodesCount,b,ToVisitIndex,IndexValue:longint;
    Condition,ID,Action,NewAction:longword;
    ByteMap:PBRREByteMap;
    ByteCount:integer;
    NodeById:array of longint;
    Nodes:PBRREOnePassState;
    Node:PBRREOnePassState;
    Matched,HasMatch:boolean;
    CharClass:TBRRERegExpCharClass;
    CharClassAction:PBRREOnePassStateCharClassAction;
    DestCharClassAction:PBRREOnePassStateCharClassAction;
    SingleCharClassesTree:TBRREPtrValueSelfBalancedTree;
    Index:TBRREPtrValueSelfBalancedTreeValue;
begin
 result:=true;
 Node:=nil;
 ByteMap:=@RegExpProgram.ByteMap;
 ByteCount:=RegExpProgram.ByteCount;
 Instruction:=StartInstruction;
 ID:=0;
 Len:=0;
 while assigned(Instruction) and (Instruction<>EndInstruction) do begin
  Instruction^.IDandOpcode:=ptruint(ptruint(Instruction^.IDandOpcode and $ff) or ptruint(ID shl 8));
  inc(ID);
  inc(Len);
  inc(Instruction);
 end;
 MaxNodes:=Len+2;
 StateSize:=sizeof(TBRREOnePassState)+((ByteCount+1)*sizeof(longword));
 HasMatch:=false;
 SingleCharClassesTree:=TBRREPtrValueSelfBalancedTree.Create;
 try
  for i:=0 to CharClasses.Count-1 do begin
   if CharClasses[i].IsSingle then begin
    SingleCharClassesTree[CharClasses[i].First.Lo]:=i;
   end;
  end;
  Stack:=nil;
  try
   SetLength(Stack,Len*4);
   NodeById:=nil;
   try
    SetLength(NodeById,Len+2);
    FillChar(NodeById[0],length(NodeById)*sizeof(longint),#$ff);
    Nodes:=nil;
    try
     GetMem(Nodes,StateSize*MaxNodes);
     FillChar(Nodes^,StateSize*MaxNodes,#$00);
     NodeByID[StartInstruction^.IDandOpcode shr 8]:=0;
     NodesCount:=1;
     Condition:=0;
     ToVisit:=TBRREPointerList.Create;
     try
      WorkQueue:=TBRREPointerList.Create;
      try
       ToVisit.Push(StartInstruction);
       ToVisitIndex:=0;
       while result and (ToVisitIndex<ToVisit.Count) do begin
        Instruction:=ToVisit[ToVisitIndex];
        inc(ToVisitIndex);
        if assigned(Instruction) then begin
         Node:=pointer(@pansichar(Nodes)[StateSize*longint(NodeByID[Instruction^.IDandOpcode shr 8])]);
         for i:=0 to ByteCount-1 do begin
          Node^.Action[i]:=brresfImpossible;
         end;
         Node^.MatchCondition:=brresfImpossible;
         Node^.NoAction:=brresfImpossible;
         Matched:=false;
         WorkQueue.Clear;
         Stack[0].Instruction:=Instruction;
         Stack[0].Condition:=0;
         StackPointer:=1;
         while result and (StackPointer>0) do begin
          dec(StackPointer);
          Instruction:=Stack[StackPointer].Instruction;
          Condition:=Stack[StackPointer].Condition;
          case Instruction^.IDandOpcode and $ff of
           brreoNONE,brreoJMP,brreoPREFIX,brreoPREFIXCHAR:begin
            if WorkQueue.Contains(Instruction^.x) then begin
             result:=false;
             break;
            end;
            WorkQueue.Add(Instruction^.x);
            Stack[StackPointer].Instruction:=Instruction.x;
            Stack[StackPointer].Condition:=Condition;
            inc(StackPointer);
           end;
           brreoCHAR,brreoCHARSINGLE,brreoANY:begin
            NextIndex:=NodeByID[Instruction^.x^.IDandOpcode shr 8];
            if NextIndex<0 then begin
             if NodesCount>=MaxNodes then begin
              result:=false;
              break;
             end;
             NextIndex:=NodesCount;
             NodeByID[Instruction^.x^.IDandOpcode shr 8]:=NextIndex;
             inc(NodesCount);
             ToVisit.Push(Instruction^.x);
            end;
            if Matched then begin
             Condition:=Condition or brresfMatchWins;
            end;
            NewAction:=longword(NextIndex shl brresfIndexShift) or Condition;
            begin
             case Instruction^.IDandOpcode and $ff of
              brreoCHAR:begin
               CharClass:=TBRRERegExpCharClass(pointer(Instruction^.Value));
              end;
              brreoCHARSINGLE:begin
               CharClass:=nil;
               if SingleCharClassesTree.Find(Instruction^.Value,Index) then begin
                CharClass:=CharClasses[Index];
               end;
               if not assigned(CharClass) then begin
                result:=false;
                break;
               end;
              end;
              else {brreoANY:}begin
               CharClass:=AnyCharClass;
              end;
             end;
             DestCharClassAction:=nil;
             CharClassAction:=Node^.CharClassAction;
             while assigned(CharClassAction) do begin
              if CharClassAction.CharClass.Intersects(CharClass) then begin
               DestCharClassAction:=CharClassAction;
               break;
              end;
              CharClassAction:=CharClassAction^.Next;
             end;
             if assigned(DestCharClassAction) then begin
              if DestCharClassAction^.Condition<>NewAction then begin
               result:=false;
               break;
              end;
             end else begin
              New(DestCharClassAction);
              FillChar(DestCharClassAction^,sizeof(TBRREOnePassStateCharClassAction),#0);
              DestCharClassAction^.AllNext:=RegExpProgram.CharClassActions;
              RegExpProgram.CharClassActions:=DestCharClassAction;
              DestCharClassAction^.Next:=Node^.CharClassAction;
              Node^.CharClassAction:=DestCharClassAction;
              DestCharClassAction^.CharClass:=CharClass;
              DestCharClassAction^.Condition:=NewAction;
             end;
            end;
            begin
             case Instruction^.IDandOpcode and $ff of
              brreoCHAR:begin
               if TBRRERegExpCharClass(pointer(Instruction^.Value)).CharSet=[#0..#255] then begin
                for i:=0 to ByteCount-1 do begin
                 Action:=Node^.Action[i];
                 if (Action and brresfImpossible)=brresfImpossible then begin
                  Node^.Action[i]:=NewAction;
                 end else if Action<>NewAction then begin
                  result:=false;
                  break;
                 end;
                end;
               end else begin
                for i:=0 to 255 do begin
                 if ansichar(byte(i)) in TBRRERegExpCharClass(pointer(Instruction^.Value)).CharSet then begin
                  b:=ByteMap^[i];
                  Action:=Node^.Action[b];
                  if (Action and brresfImpossible)=brresfImpossible then begin
                   Node^.Action[b]:=NewAction;
                  end else if Action<>NewAction then begin
                   result:=false;
                   break;
                  end;
                 end;
                end;
               end;
               if not result then begin
                break;
               end;
              end;
              brreoCHARSINGLE:begin
               if Instruction^.Value in [$00..$ff] then begin
                b:=ByteMap^[Instruction^.Value];
                Action:=Node^.Action[b];
                if (Action and brresfImpossible)=brresfImpossible then begin
                 Node^.Action[b]:=NewAction;
                end else if Action<>NewAction then begin
                 result:=false;
                 break;
                end;
               end;
              end;
              else {brreoANY:}begin
               for i:=0 to ByteCount-1 do begin
                Action:=Node^.Action[i];
                if (Action and brresfImpossible)=brresfImpossible then begin
                 Node^.Action[i]:=NewAction;
                end else if Action<>NewAction then begin
                 result:=false;
                 break;
                end;
               end;
               if not result then begin
                break;
               end;
              end;
             end;
            end;
           end;
           brreoMATCH:begin
            if Matched then begin
             result:=false;
             break;
            end;
            Matched:=true;
            HasMatch:=true;
            Node^.MatchCondition:=Condition;
            if NodesCount>=MaxNodes then begin
             result:=false;
             break;
            end else begin
             NextIndex:=0;
             NewAction:=longword(NextIndex shl brresfIndexShift) or (Condition or brresfMatchWins);
             if (Node^.NoAction shr brresfIndexShift)=0 then begin
              if (Node^.NoAction and brresfImpossible)=brresfImpossible then begin
               Node^.NoAction:=NewAction;
              end else if Node^.NoAction<>NewAction then begin
               result:=false;
               break;
              end;
             end;
             for i:=0 to ByteCount-1 do begin
              Action:=Node^.Action[i];
              if (Action shr brresfIndexShift)=0 then begin
               if (Action and brresfImpossible)=brresfImpossible then begin
                Node^.Action[i]:=NewAction;
               end else if Action<>NewAction then begin
                result:=false;
                break;
               end;
              end;
             end;
            end;
           end;
           brreoFAIL:begin
           end;
           brreoSPLIT,brreoSPLITLONGEST:begin
            if WorkQueue.Contains(Instruction^.x) or WorkQueue.Contains(Instruction^.y) then begin
             result:=false;
             break;
            end;
            WorkQueue.Add(Instruction^.x);
            WorkQueue.Add(Instruction^.y);
            Stack[StackPointer].Instruction:=Instruction^.y;
            Stack[StackPointer].Condition:=Condition;
            inc(StackPointer);
            Stack[StackPointer].Instruction:=Instruction^.x;
            Stack[StackPointer].Condition:=Condition;
            inc(StackPointer);
           end;
           brreoBEGIN,brreoEND:begin
            if Instruction^.Value>=RegExpProgram.FirstCapture then begin
             IndexValue:=((Instruction^.Value-RegExpProgram.FirstCapture) shl 1) or (byte(bytebool((Instruction^.IDandOpcode and $ff)=brreoEND)) and 1);
             if IndexValue>=brresfMaxCap then begin
              result:=false;
              break;
             end;
             Condition:=Condition or ((1 shl brresfCapShift) shl IndexValue);
            end;
            if WorkQueue.Contains(Instruction^.x) then begin
             result:=false;
             break;
            end;
            WorkQueue.Add(Instruction^.x);
            Stack[StackPointer].Instruction:=Instruction.x;
            Stack[StackPointer].Condition:=Condition;
            inc(StackPointer);
           end;
           brreoBOML,brreoEOML,brreoBOSL,brreoEOSL,brreoBOS,brreoEOS,brreoBRK,brreoNBRK:begin
            case Instruction^.IDandOpcode and $ff of
             brreoBOML:begin
              Condition:=Condition or brresfEmptyBeginLine;
             end;
             brreoEOML:begin
              Condition:=Condition or brresfEmptyEndLine;
             end;
             brreoBOSL,brreoBOS:begin
              Condition:=Condition or brresfEmptyBeginText;
             end;
             brreoEOSL,brreoEOS:begin
              Condition:=Condition or brresfEmptyEndText;
             end;
             brreoBRK:begin
              Condition:=Condition or brresfEmptyWordBoundary;
             end;
             brreoNBRK:begin
              Condition:=Condition or brresfEmptyNonWordBoundary;
             end;
            end;
            if WorkQueue.Contains(Instruction^.x) then begin
             result:=false;
             break;
            end;
            WorkQueue.Add(Instruction^.x);
            Stack[StackPointer].Instruction:=Instruction.x;
            Stack[StackPointer].Condition:=Condition;
            inc(StackPointer);
           end;
           else {brreoQUESTG,brreoQUESTP,brreoQUESTL,brreoSTARG,brreoSTARP,brreoSTARL,brreoPLUSG,brreoPLUSP,brreoPLUSL,brreoEXACT,
           brreoREPEATG,brreoREPEATP,brreoREPEATL,brreoSTOP,brreoBACKREF,brreoBACKREFI,brreoTESTBACKREF,brreoTEST,
           brreoCONTINUE,brreoLOOKAHEADFALSE,brreoLOOKAHEADTRUE.brreoLOOKBEHINDFALSE,brreoLOOKBEHINDTRUE,brreoPARALLEL,
           brreoFIXEDSTRINGSEARCH:}begin
            result:=false;
            break;
           end;
          end;
         end;
        end else begin
         break;
        end;
       end;
       if not HasMatch then begin
        result:=false;
       end;
       if result then begin
        if NodesCount>=MaxNodes then begin
         result:=false;
        end else begin
         NextIndex:=0;
         NewAction:=longword(NextIndex shl brresfIndexShift) or (Condition or brresfMatchWins);
         if (Node^.NoAction shr brresfIndexShift)=0 then begin
          if (Node^.NoAction and brresfImpossible)=brresfImpossible then begin
           Node^.NoAction:=NewAction;
          end else if Node^.NoAction<>NewAction then begin
           result:=false;
          end;
         end;
         if result then begin
          for i:=0 to ByteCount-1 do begin
           Action:=Node^.Action[i];
           if (Action shr brresfIndexShift)=0 then begin
            if (Action and brresfImpossible)=brresfImpossible then begin
             Node^.Action[i]:=NewAction;
            end else if Action<>NewAction then begin
             result:=false;
             break;
            end;
           end;
          end;
         end;
        end;
       end;
       if result then begin
        GetMem(RegExpProgram.OnePassNodes,NodesCount*StateSize);
        RegExpProgram.OnePassNodesCount:=NodesCount;
        Move(Nodes^,RegExpProgram.OnePassNodes^,NodesCount*StateSize);
        RegExpProgram.OnePassStart:=pointer(@pansichar(RegExpProgram.OnePassNodes)[StateSize*longint(NodeByID[StartInstruction^.IDandOpcode shr 8])]);
        RegExpProgram.OnePassStateSize:=StateSize;
       end;
      finally
       WorkQueue.Free;
      end;
     finally
      ToVisit.Free;
     end;
    finally
     if assigned(Nodes) then begin
      FreeMem(Nodes);
     end;
    end;
   finally
    SetLength(NodeByID,0);
   end;
  finally
   SetLength(Stack,0);
  end;
 finally
  SingleCharClassesTree.Free;
 end;
end;

function TBRRERegExpCompiler.CompilePrefilterTree(RootNode:PBRRERegExpASTNode):TBRRERegExpPrefilterNode;
 function Process(Node:PBRRERegExpASTNode):TBRRERegExpPrefilterNode;
 var Left,Right,Temp,OtherTemp:TBRRERegExpPrefilterNode;
     Counter,SubCounter,IndexCounter:longint;
     CharClassRange:TBRRERegExpCharClassRange;
     CharValue:longword;
     OK,ParentLoop:boolean;
 begin
  result:=nil;
  if assigned(Node) then begin

   // Convertion and generation pass ===============================================================================

   case Node^.NodeType of
    ntCAT:begin
     Left:=Process(Node^.Left);
     Right:=Process(Node^.Right);
     if assigned(Left) and assigned(Right) then begin
      if ((Left.Operation=brrepfnoATOM) and (Right.Operation=brrepfnoATOM)) and (Left.Exact and Right.Exact) then begin
       result:=Left;
       result.Atom:=result.Atom+Right.Atom;
       FreeAndNil(Right);
      end else if (Left.Operation=brrepfnoAND) and (Right.Operation=brrepfnoAND) then begin
       result:=Left;
       for Counter:=0 to Right.Subs.Count-1 do begin
        result.Subs.Add(Right.Subs[Counter]);
        result.Exact:=result.Exact and Right.Subs[Counter].Exact;
       end;
       Right.Subs.Clear;
       FreeAndNil(Right);
      end else if Left.Operation=brrepfnoAND then begin
       result:=Left;
       result.Subs.Add(Right);
       result.Exact:=result.Exact and Right.Exact;
      end else if Right.Operation=brrepfnoAND then begin
       result:=Right;
       result.Subs.Insert(0,Left);
       result.Exact:=result.Exact and Left.Exact;
      end else begin
       result:=TBRRERegExpPrefilterNode.Create;
       result.Operation:=brrepfnoAND;
       result.Exact:=Left.Exact and Right.Exact;
       result.Subs.Add(Left);
       result.Subs.Add(Right);
      end;
     end else if assigned(Left) then begin
      result:=Left;
     end else if assigned(Right) then begin
      result:=Right;
     end;
    end;
    ntALT:begin
     Left:=Process(Node^.Left);
     Right:=Process(Node^.Right);
     if assigned(Left) and assigned(Right) then begin
      if (Left.Operation=brrepfnoOR) and (Right.Operation=brrepfnoOR) then begin
       result:=Left;
       for Counter:=0 to Right.Subs.Count-1 do begin
        result.Subs.Add(Right.Subs[Counter]);
        result.Exact:=result.Exact and Right.Subs[Counter].Exact;
       end;
       Right.Subs.Clear;
       FreeAndNil(Right);
      end else if Left.Operation=brrepfnoOR then begin
       result:=Left;
       result.Subs.Add(Right);
       result.Exact:=result.Exact and Right.Exact;
      end else if Right.Operation=brrepfnoOR then begin
       result:=Right;
       result.Subs.Insert(0,Left);
       result.Exact:=result.Exact and Left.Exact;
      end else begin
       result:=TBRRERegExpPrefilterNode.Create;
       result.Operation:=brrepfnoOR;
       result.Exact:=Left.Exact and Right.Exact;
       result.Subs.Add(Left);
       result.Subs.Add(Right);
      end;
     end else if assigned(Left) then begin
      result:=Left;
     end else if assigned(Right) then begin
      result:=Right;
     end;
    end;
    ntPAREN:begin
     result:=Process(Node^.Left);
    end;
    ntCHAR:begin
     if assigned(CharClasses[Node^.Value].First) and CharClasses[Node^.Value].IsSingle then begin
      result:=TBRRERegExpPrefilterNode.Create;
      result.Operation:=brrepfnoATOM;
      if RegExpIsUTF8<>brresuNOUTF8 then begin
       result.Atom:=BRREUTF32CharToUTF8(CharClasses[Node^.Value].First.Lo);
      end else begin
       result.Atom:=ansichar(byte(CharClasses[Node^.Value].First.Lo));
      end;
      result.Exact:=true;
     end else begin
      CharClassRange:=CharClasses[Node^.Value].First;
      if assigned(CharClassRange) and (CharClasses[Node^.Value].Count<=10) then begin
       result:=TBRRERegExpPrefilterNode.Create;
       result.Operation:=brrepfnoOR;
       result.Exact:=true;
       while assigned(CharClassRange) do begin
        for CharValue:=CharClassRange.Lo to CharClassRange.Hi do begin
         if (RegExpIsUTF8=brresuNOUTF8) and (CharValue>255) then begin
          break;
         end;
         Left:=TBRRERegExpPrefilterNode.Create;
         Left.Operation:=brrepfnoATOM;
         if RegExpIsUTF8<>brresuNOUTF8 then begin
          Left.Atom:=BRREUTF32CharToUTF8(CharValue);
         end else begin
          Left.Atom:=ansichar(byte(CharValue));
         end;
         Left.Exact:=true;
         result.Subs.Add(Left);
        end;
        CharClassRange:=CharClassRange.Next;
       end;
      end else begin
       result:=TBRRERegExpPrefilterNode.Create;
       result.Operation:=brrepfnoANY;
       result.Exact:=false;
      end;
     end;
    end;
    ntQUEST,ntSTAR,ntDOT,ntBOML,ntEOML,ntBOSL,ntEOSL,ntBOS,ntEOS,ntBRK,ntNBRK,ntEXACT,ntREPEAT,ntSTOP,ntBACKREF,ntBACKREFI,ntTESTBACKREF,ntTEST,ntCONTINUE,ntLOOKAHEADFALSE,ntLOOKAHEADTRUE,ntLOOKBEHINDFALSE,ntLOOKBEHINDTRUE,ntCALLOUT,ntRECURSIVE:begin
     result:=TBRRERegExpPrefilterNode.Create;
     result.Operation:=brrepfnoANY;
     result.Exact:=false;
    end;
    ntPLUS:begin
     Left:=Process(Node^.Left);
     Right:=TBRRERegExpPrefilterNode.Create;
     Right.Operation:=brrepfnoANY;
     Right.Exact:=false;
     result:=TBRRERegExpPrefilterNode.Create;
     result.Operation:=brrepfnoAND;
     result.Subs.Add(Left);
     result.Subs.Add(Right);
     result.Exact:=false;
    end;
    ntPARALLEL,ntONEPASS,ntFIXEDSTRINGSEARCH:begin
     if assigned(TBRRERegExpProgram(RegExp.RegExpPrograms[Node^.Value]).PrefilterRootNode) then begin
      result:=TBRRERegExpProgram(RegExp.RegExpPrograms[Node^.Value]).PrefilterRootNode;
      TBRRERegExpProgram(RegExp.RegExpPrograms[Node^.Value]).PrefilterRootNode:=nil;
     end else begin
      result:=TBRRERegExpPrefilterNode.Create;
      result.Operation:=brrepfnoANY;
      result.Exact:=false;
     end;
    end;
    else begin
     result:=nil;
    end;
   end;

   // Optimization pass ============================================================================================

   while assigned(result) do begin

    case result.Operation of

     brrepfnoAND:begin

      for Counter:=0 to result.Subs.Count-1 do begin
       result.Exact:=result.Exact and result.Subs[Counter].Exact;
      end;

      OK:=false;
      Counter:=0;
      while Counter<result.Subs.Count do begin
       Left:=result.Subs[Counter];
       if not assigned(Left) then begin
        result.Subs.Delete(Counter);
        OK:=true;
       end else begin
        inc(Counter);
       end;
      end;
      if OK and (result.Subs.Count>1) then begin
       continue;
      end;

      case result.Subs.Count of
       0:begin
        result.Destroy;
        result:=nil;
       end;
       1:begin
        Left:=result.Subs[0];
        Left.Exact:=Left.Exact and result.Exact;
        result.Subs.Clear;
        result.Destroy;
        result:=Left;
       end;
       else begin
        ParentLoop:=false;
        IndexCounter:=0;
        while IndexCounter<(result.Subs.Count-1) do begin
         Left:=result.Subs[IndexCounter];
         Right:=result.Subs[IndexCounter+1];
         if ((Left.Operation=brrepfnoATOM) and (Right.Operation=brrepfnoATOM)) and (Left.Exact and Right.Exact) then begin
          Left.Atom:=Left.Atom+Right.Atom;
          FreeAndNil(Right);
          result.Subs.Delete(IndexCounter+1);
          ParentLoop:=true;
          continue;
         end else if (Left.Operation=brrepfnoANY) and (Right.Operation=brrepfnoANY) then begin
          FreeAndNil(Right);
          result.Subs.Delete(IndexCounter+1);
          ParentLoop:=true;
          continue;
         end else if ((Left.Operation=brrepfnoATOM) and (Right.Operation=brrepfnoOR)) and (Left.Exact and Right.Exact) then begin
          OK:=true;
          for Counter:=0 to Right.Subs.Count-1 do begin
           if (Right.Subs[Counter].Operation<>brrepfnoATOM) or not Right.Subs[Counter].Exact then begin
            OK:=false;
            break;
           end;
          end;
          if OK then begin
           for Counter:=0 to Right.Subs.Count-1 do begin
            Right.Subs[Counter].Atom:=Left.Atom+Right.Subs[Counter].Atom;
           end;
           FreeAndNil(Left);
           result.Subs.Delete(IndexCounter);
           ParentLoop:=false;
           continue;
          end;
         end else if ((Left.Operation=brrepfnoOR) and (Right.Operation=brrepfnoATOM)) and (Left.Exact and Right.Exact) then begin
          OK:=true;
          for Counter:=0 to Left.Subs.Count-1 do begin
           if (Left.Subs[Counter].Operation<>brrepfnoATOM) or not Left.Subs[Counter].Exact then begin
            OK:=false;
            break;
           end;
          end;
          if OK then begin
           for Counter:=0 to Left.Subs.Count-1 do begin
            Left.Subs[Counter].Atom:=Left.Subs[Counter].Atom+Right.Atom;
           end;
           FreeAndNil(Right);
           result.Subs.Delete(IndexCounter+1);
           ParentLoop:=false;
           continue;
          end;
         end else if (Left.Operation=brrepfnoOR) and (Right.Operation=brrepfnoOR) and (Left.Exact and Right.Exact) then begin
          OK:=true;
          for Counter:=0 to Left.Subs.Count-1 do begin
           if (Left.Subs[Counter].Operation<>brrepfnoATOM) or not Left.Subs[Counter].Exact then begin
            OK:=false;
            break;
           end;
          end;
          if OK then begin
           for Counter:=0 to Right.Subs.Count-1 do begin
            if (Right.Subs[Counter].Operation<>brrepfnoATOM) or not Right.Subs[Counter].Exact then begin
             OK:=false;
             break;
            end;
           end;
           if OK then begin
            OtherTemp:=TBRRERegExpPrefilterNode.Create;
            OtherTemp.Operation:=brrepfnoOR;
            OtherTemp.Exact:=true;
            for Counter:=0 to Left.Subs.Count-1 do begin
             for SubCounter:=0 to Right.Subs.Count-1 do begin
              Temp:=TBRRERegExpPrefilterNode.Create;
              Temp.Operation:=brrepfnoATOM;
              Temp.Atom:=Left.Subs[Counter].Atom+Right.Subs[SubCounter].Atom;
              Temp.Exact:=true;
              OtherTemp.Subs.Add(Temp);
             end;
            end;
            FreeAndNil(Left);
            FreeAndNil(Right);
            result.Subs[IndexCounter]:=OtherTemp;
            result.Subs.Delete(IndexCounter+1);
            continue;
           end;
          end;
         end;
         inc(IndexCounter);
        end;
        if ParentLoop then begin
         continue;
        end;
       end;
      end;

     end;

     brrepfnoOR:begin

      OK:=true;
      for Counter:=0 to result.Subs.Count-1 do begin
       Left:=result.Subs[Counter];
       if assigned(Left) and ((Left.Operation=brrepfnoANY) or not Left.Exact) then begin
        OK:=false;
        break;
       end;
      end;
      if not OK then begin
       result.Destroy;
       result:=TBRRERegExpPrefilterNode.Create;
       result.Operation:=brrepfnoANY;
       result.Exact:=false;
       continue;
      end;

      OK:=false;
      Counter:=0;
      while Counter<result.Subs.Count do begin
       Left:=result.Subs[Counter];
       if not assigned(Left) then begin
        result.Subs.Delete(Counter);
        OK:=true;
       end else begin
        inc(Counter);
       end;
      end;
      if OK and (result.Subs.Count>1) then begin
       continue;
      end;

      case result.Subs.Count of
       0:begin
        result.Destroy;
        result:=nil;
       end;
       1:begin
        Left:=result.Subs[0];
        Left.Exact:=Left.Exact and result.Exact;
        result.Subs.Clear;
        result.Destroy;
        result:=Left;
       end;
      end;

     end;
    end;

    break;
   end;

   // End ==========================================================================================================

  end;
 end;
begin
 result:=Process(RootNode);
end;

procedure TBRRERegExpCompiler.Emit(Node:PBRRERegExpASTNode);
var p1,p2,t:PBRRERegExpNFAInstruction;
    v:longint;
begin
 if assigned(Node) then begin
  case Node^.NodeType of
   ntNONE:begin
   end;
   ntALT:begin
    Instruction^.IDandOpcode:=brreoSPLIT;
    Instruction^.Value:=brreoskALT;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
    Emit(Node^.Left);
    p2:=Instruction;
    Instruction^.IDandOpcode:=brreoJMP;
    p2:=Instruction;
    inc(Instruction);
    p1^.y:=Instruction;
    Emit(Node^.Right);
    p2^.x:=Instruction;
   end;
   ntCAT:begin
    Emit(Node^.Left);
    Emit(Node^.Right);
   end;
   ntSTAR:begin
    if IsBacktracking and (((Flags and brrefNFA)<>0) or (Node^.Value=brreqkPOSSESSIVE)) then begin
     case Node^.Value of
      brreqkGREEDY:begin
       Instruction^.IDandOpcode:=brreoGREEDY;
      end;
      brreqkLAZY:begin
       Instruction^.IDandOpcode:=brreoLAZY;
      end;
      else {brreqkPOSSESSIVE:}begin
       Instruction^.IDandOpcode:=brreoPOSSESSIVE;
      end;
     end;
     p1:=Instruction;
     inc(Instruction);
     p1^.x:=Instruction;
     p1^.Value:=-1;
     Emit(Node^.Left);
     Instruction^.IDandOpcode:=brreoMATCH;
     inc(Instruction);
     p1^.y:=Instruction;
    end else begin
     Instruction^.IDandOpcode:=brreoSPLIT;
     Instruction^.Value:=brreoskSTAR;
     p1:=Instruction;
     inc(Instruction);
     p1^.x:=Instruction;
     Emit(Node^.Left);
     p2:=Instruction;
     Instruction^.IDandOpcode:=brreoJMP;
     Instruction^.x:=p1;
     inc(Instruction);
     p1^.y:=Instruction;
     if Node^.Value<>0 then begin
      t:=p1^.x;
      p1^.x:=p1^.y;
      p1^.y:=t;
     end;
    end;
   end;
   ntQUEST:begin
    case Node^.Value of
     brreqkGREEDY:begin
      // Greedy quest
      Instruction^.IDandOpcode:=brreoSPLIT;
      Instruction^.Value:=brreoskQUEST;
      p1:=Instruction;
      inc(Instruction);
      p1^.x:=Instruction;
      Emit(Node^.Left);
      p1^.y:=Instruction;
     end;
     brreqkLAZY:begin
      // Lazy quest
      Instruction^.IDandOpcode:=brreoSPLIT;
      Instruction^.Value:=brreoskQUEST;
      p1:=Instruction;
      inc(Instruction);
      p1^.y:=Instruction;
      Emit(Node^.Left);
      p1^.x:=Instruction;
     end;
     else {brreqkPOSSESSIVE:}begin
      // Possessive quest
      Emit(Node^.Left);
     end;
    end;
   end;
   ntPLUS:begin
    if IsBacktracking and (((Flags and brrefNFA)<>0) or (Node^.Value=brreqkPOSSESSIVE)) then begin
     Emit(Node^.Left);
     case Node^.Value of
      brreqkGREEDY:begin
       Instruction^.IDandOpcode:=brreoGREEDY;
      end;
      brreqkLAZY:begin
       Instruction^.IDandOpcode:=brreoLAZY;
      end;
      else {brreqkPOSSESSIVE:}begin
       Instruction^.IDandOpcode:=brreoPOSSESSIVE;
      end;
     end;
     p1:=Instruction;
     inc(Instruction);
     p1^.x:=Instruction;
     p1^.Value:=-1;
     Emit(Node^.Left);
     Instruction^.IDandOpcode:=brreoMATCH;
     inc(Instruction);
     p1^.y:=Instruction;
    end else{}begin
     p1:=Instruction;
     Emit(Node^.Left);
     Instruction^.IDandOpcode:=brreoSPLIT;
     Instruction^.Value:=brreoskPLUS;
     Instruction^.x:=p1;
     p2:=Instruction;
     inc(Instruction);
     p2^.y:=Instruction;
     if Node^.Value<>0 then begin
      t:=p2^.x;
      p2^.x:=p2^.y;
      p2^.y:=t;
     end;
    end;
   end;
   ntPAREN:begin
    v:=Node^.Value;
    Instruction^.IDandOpcode:=brreoBEGIN;
    Instruction^.Value:=v;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
    Emit(Node^.Left);
    Instruction^.IDandOpcode:=brreoEND;
    Instruction^.Value:=v;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntCHAR:begin
    if IsBacktracking and ((Flags and brrefAUTOCALLOUT)<>0) then begin
     Instruction^.IDandOpcode:=brreoCALLOUT;
     Instruction^.Value:=255;
     p1:=Instruction;
     inc(Instruction);
     p1^.x:=Instruction;
    end;
    AllCharsClass.Combine(CharClasses[Node^.Value]);
    if assigned(CharClasses[Node^.Value].First) and CharClasses[Node^.Value].IsSingle then begin
     Instruction^.IDandOpcode:=brreoCHARSINGLE;
     Instruction^.Value:=CharClasses[Node^.Value].First.Lo;
    end else begin
     Instruction^.IDandOpcode:=brreoCHAR;
     Instruction^.Value:=ptrint(pointer(CharClasses[Node^.Value]));
    end;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntDOT:begin
    Instruction^.IDandOpcode:=brreoANY;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntBOML:begin
    Instruction^.IDandOpcode:=brreoBOML;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntEOML:begin
    Instruction^.IDandOpcode:=brreoEOML;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntBOSL:begin
    Instruction^.IDandOpcode:=brreoBOSL;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntEOSL:begin
    Instruction^.IDandOpcode:=brreoEOSL;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntBOS:begin
    Instruction^.IDandOpcode:=brreoBOS;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntEOS:begin
    Instruction^.IDandOpcode:=brreoEOS;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntBRK:begin
    Instruction^.IDandOpcode:=brreoBRK;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntNBRK:begin
    Instruction^.IDandOpcode:=brreoNBRK;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntEXACT:begin
    if Node^.Value=1 then begin
     Emit(Node^.Left);
    end else if Node^.Value>0 then begin
     Instruction^.IDandOpcode:=brreoEXACT;
     p1:=Instruction;
     inc(Instruction);
     p1^.x:=Instruction;
     p1^.Value:=Node^.Value;
     Emit(Node^.Left);
     Instruction^.IDandOpcode:=brreoMATCH;
     inc(Instruction);
     p1^.y:=Instruction;
    end;
   end;
   ntREPEAT:begin
    if (Node^.Min>0) and (Node^.Min=Node^.Max) then begin
     if Node^.Max=1 then begin
      Emit(Node^.Left);
     end else begin
      Instruction^.IDandOpcode:=brreoEXACT;
      p1:=Instruction;
      inc(Instruction);
      p1^.x:=Instruction;
      p1^.Value:=Node^.Max;
      Emit(Node^.Left);
      Instruction^.IDandOpcode:=brreoMATCH;
      inc(Instruction);
      p1^.y:=Instruction;
     end;
    end else if Node^.Min<=0 then begin
     if (Node.Max<>0) and ((Node^.Max<0) or (Node^.Min<Node^.Max)) then begin
      case Node^.Value of
       brreqkGREEDY:begin
        Instruction^.IDandOpcode:=brreoGREEDY;
       end;
       brreqkLAZY:begin
        Instruction^.IDandOpcode:=brreoLAZY;
       end;
       else {brreqkPOSSESSIVE:}begin
        Instruction^.IDandOpcode:=brreoPOSSESSIVE;
       end;
      end;
      p1:=Instruction;
      inc(Instruction);
      p1^.x:=Instruction;
      p1^.Value:=Node^.Max;
      Emit(Node^.Left);
      Instruction^.IDandOpcode:=brreoMATCH;
      inc(Instruction);
      p1^.y:=Instruction;
     end;
    end else begin
     Instruction^.IDandOpcode:=brreoEXACT;
     p1:=Instruction;
     inc(Instruction);
     p1^.Value:=Node^.Min;
     if (Node^.Max<>0) and (((Node^.Max<0) or (Node^.Min<Node^.Max)) and ((Node^.Max-Node^.Min)<>0)) then begin
      case Node^.Value of
       brreqkGREEDY:begin
        Instruction^.IDandOpcode:=brreoGREEDY;
       end;
       brreqkLAZY:begin
        Instruction^.IDandOpcode:=brreoLAZY;
       end;
       else {brreqkPOSSESSIVE:}begin
        Instruction^.IDandOpcode:=brreoPOSSESSIVE;
       end;
      end;
      p1^.y:=Instruction;
      p2:=Instruction;
      inc(Instruction);
      p1^.x:=Instruction;
      p2^.x:=Instruction;
      if Node^.Max<0 then begin
       p2^.Value:=-1;
      end else begin
       p2^.Value:=Node^.Max-Node^.Min;
      end;
      Emit(Node^.Left);
      Instruction^.IDandOpcode:=brreoMATCH;
      inc(Instruction);
      p2^.y:=Instruction;
     end else begin
      Emit(Node^.Left);
      Instruction^.IDandOpcode:=brreoMATCH;
      inc(Instruction);
      p1^.y:=Instruction;
     end;
    end;
   end;
   ntSTOP:begin
    Instruction^.IDandOpcode:=brreoSTOP;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
    Emit(Node^.Left);
    Instruction^.IDandOpcode:=brreoMATCH;
    inc(Instruction);
    p1^.y:=Instruction;
   end;
   ntBACKREF:begin
    Instruction^.IDandOpcode:=brreoBACKREF;
    Instruction^.Value:=Node^.Value;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntBACKREFI:begin
    Instruction^.IDandOpcode:=brreoBACKREFI;
    Instruction^.Value:=Node^.Value;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntTESTBACKREF:begin
    Instruction^.IDandOpcode:=brreoTESTBACKREF;
    Instruction^.Value:=Node^.Value;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
    Emit(Node^.Left);
    Instruction^.IDandOpcode:=brreoJMP;
    p2:=Instruction;
    inc(Instruction);
    p1^.y:=Instruction;
    if assigned(Node^.Right) then begin
     Emit(Node^.Right);
    end;
    p2^.x:=Instruction;
   end;
   ntTEST:begin
    Instruction^.IDandOpcode:=brreoTEST;
    p1:=Instruction;
    Emit(Node^.Extra);
    Instruction^.IDandOpcode:=brreoMATCH;
    p1^.Value:=ptrint(pointer(Instruction));
    inc(Instruction);
    p1^.x:=Instruction;
    Emit(Node^.Left);
    Instruction^.IDandOpcode:=brreoJMP;
    p2:=Instruction;
    inc(Instruction);
    p1^.y:=Instruction;
    if assigned(Node^.Right) then begin
     Emit(Node^.Right);
    end;
    p2^.x:=Instruction;
   end;
   ntCONTINUE:begin
    Instruction^.IDandOpcode:=brreoCONTINUE;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntLOOKAHEADFALSE:begin
    Instruction^.IDandOpcode:=brreoLOOKAHEADFALSE;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
    Emit(Node^.Left);
    Instruction^.IDandOpcode:=brreoMATCH;
    inc(Instruction);
    p1^.y:=Instruction;
   end;
   ntLOOKAHEADTRUE:begin
    Instruction^.IDandOpcode:=brreoLOOKAHEADTRUE;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
    Emit(Node^.Left);
    Instruction^.IDandOpcode:=brreoMATCH;
    inc(Instruction);
    p1^.y:=Instruction;
   end;
   ntLOOKBEHINDFALSE:begin
    Instruction^.IDandOpcode:=brreoLOOKBEHINDFALSE;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
    Emit(Node^.Left);
    Instruction^.IDandOpcode:=brreoMATCH;
    inc(Instruction);
    p1^.y:=Instruction;
   end;
   ntLOOKBEHINDTRUE:begin
    Instruction^.IDandOpcode:=brreoLOOKBEHINDTRUE;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
    Emit(Node^.Left);
    Instruction^.IDandOpcode:=brreoMATCH;
    inc(Instruction);
    p1^.y:=Instruction;
   end;
   ntPARALLEL:begin
    Instruction^.IDandOpcode:=brreoPARALLEL;
    Instruction^.Value:=Node^.Value;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntONEPASS:begin
    Instruction^.IDandOpcode:=brreoONEPASS;
    Instruction^.Value:=Node^.Value;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntFIXEDSTRINGSEARCH:begin
    Instruction^.IDandOpcode:=brreoFIXEDSTRINGSEARCH;
    Instruction^.Value:=Node^.Value;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntCALLOUT:begin
    Instruction^.IDandOpcode:=brreoCALLOUT;
    Instruction^.Value:=Node^.Value;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=Instruction;
   end;
   ntRECURSIVE:begin
    Instruction^.IDandOpcode:=brreoRECURSIVE;
    Instruction^.Value:=Node^.Value;
    p1:=Instruction;
    inc(Instruction);
    p1^.x:=nil;
    p1^.y:=Instruction;
   end;
  end;
 end;
end;

function TBRRERegExpCompiler.Pass(aIsBacktracking:boolean;var ProgramIndex:longint;AHasBacktrackingParent:boolean):boolean;
var Node,TempNode:PBRRERegExpASTNode;
    i,j,CountOfChars,AnchorStart,AnchorEnd,NonWorkBreak,WorkBreak:longint;
    OldNodes:TBRREPointerList;
    OldIsBacktracking:boolean;
    OldHasBacktrackingParent:boolean;
    OldRegExpProgram:TBRRERegExpProgram;
    OldInstruction:PBRRERegExpNFAInstruction;
begin
 Prefix:='';
 result:=false;
 OldNodes:=Nodes;
 OldIsBacktracking:=IsBacktracking;
 OldHasBacktrackingParent:=HasBacktrackingParent;
 OldRegExpProgram:=RegExpProgram;
 OldInstruction:=Instruction;
 try
  IsBacktracking:=AIsBacktracking;
  HasBacktrackingParent:=AHasBacktrackingParent;
  try
   Nodes:=TBRREPointerList.Create;
   try
    RegExpProgram:=TBRRERegExpProgram.Create(RegExp);
    try
     RegExpProgram.Method:=brrepmBACKTRACKINGNFA;
     RegExpProgram.IsGlobal:=IsFirstChar;
     RegExpProgram.FirstCapture:=RegExp.CountOfCaptures;
     HasRecursive:=false;
     Node:=ParseDisjunction(true);
     while OptimizeNode(@Node) do begin
     end;
     FixUpFixupableCaptures;
//   writeln(Dump(Node));
     if (Flags and brrefPREFILTER)<>0 then begin
      RegExpProgram.PrefilterRootNode:=CompilePrefilterTree(Node);
     end else begin
      RegExpProgram.PrefilterRootNode:=nil;
     end;
     CompilePrefix(Node);
     if IsBacktracking or not ConvertToFixedStringSearch(RegExpProgram) then begin

      begin
       RegExpProgram.FixedString:='';

       RegExpProgram.AnchorBeginText:=false;
       RegExpProgram.AnchorBeginLine:=false;
       RegExpProgram.AnchorStart:=false;
       RegExpProgram.AnchorEnd:=false;

       TempNode:=Node;
       while assigned(TempNode) do begin
        case TempNode^.NodeType of
         ntCAT:begin
          if assigned(TempNode^.Left) then begin
           case TempNode^.Left^.NodeType of
            ntCAT,ntBOSL,ntBOS,ntBOML:begin
             TempNode:=TempNode^.Left;
            end;
            ntNBRK,ntBRK:begin
             TempNode:=TempNode^.Right;
            end;
            else begin
             break;
            end;
           end;
          end else begin
           break;
          end;
         end;
         ntBOSL,ntBOS:begin
          RegExpProgram.AnchorBeginText:=true;
          RegExpProgram.AnchorStart:=true;
          break;
         end;
         ntBOML:begin
          RegExpProgram.AnchorBeginLine:=true;
          RegExpProgram.AnchorStart:=true;
          break;
         end;
         ntPARALLEL,ntONEPASS,ntFIXEDSTRINGSEARCH:begin
          RegExpProgram.AnchorBeginText:=RegExpProgram.AnchorBeginText or TBRRERegExpProgram(RegExp.RegExpPrograms[Node^.Value]).AnchorBeginText;
          RegExpProgram.AnchorBeginLine:=RegExpProgram.AnchorBeginLine or TBRRERegExpProgram(RegExp.RegExpPrograms[Node^.Value]).AnchorBeginLine;
          RegExpProgram.AnchorStart:=RegExpProgram.AnchorStart or TBRRERegExpProgram(RegExp.RegExpPrograms[Node^.Value]).AnchorStart;
          break;
         end;
         else begin
          break;
         end;
        end;
       end;

       TempNode:=Node;
       while assigned(TempNode) do begin
        case TempNode^.NodeType of
         ntCAT:begin
          if assigned(TempNode^.Right) then begin
           case TempNode^.Right^.NodeType of
            ntCAT,ntEOSL,ntEOS,ntEOML:begin
             TempNode:=TempNode^.Right;
            end;
            ntNBRK,ntBRK:begin
             TempNode:=TempNode^.Left;
            end;
            else begin
             break;
            end;
           end;
          end else begin
           break;
          end;
         end;
         ntEOSL,ntEOS,ntEOML:begin
          RegExpProgram.AnchorEnd:=true;
          break;
         end;
         ntPARALLEL,ntONEPASS,ntFIXEDSTRINGSEARCH:begin
          RegExpProgram.AnchorEnd:=RegExpProgram.AnchorEnd or TBRRERegExpProgram(RegExp.RegExpPrograms[Node^.Value]).AnchorEnd;
          break;
         end;
         else begin
          break;
         end;
        end;
       end;
      end;

      if assigned(Node) and not (RegExpProgram.IsGlobal and (Node^.NodeType in [ntPARALLEL,ntONEPASS,ntFIXEDSTRINGSEARCH])) then begin
       CompileByteMap(RegExpProgram);
       if RegExpProgram.IsGlobal then begin

        if 0<CaptureNodes.Count then begin
         CaptureNodes[0]:=Node;
        end else if 0=CaptureNodes.Count then begin
         CaptureNodes.Add(Node);
        end else begin
         while CaptureNodes.Count<0 do begin
          CaptureNodes.Add(nil);
         end;
         CaptureNodes.Add(Node);
        end;
        Node:=NewNode(ntPAREN,Node,nil,nil,0);

        RegExpProgram.HasRootCapture:=true;
        begin
         i:=CountInstructions(Node)+1;
         if IsBacktracking and ((Flags and brrefAUTOCALLOUT)<>0) then begin
          inc(i);
         end;
         if HasRecursive then begin
          for j:=0 to CaptureNodes.Count-1 do begin
           if assigned(CaptureNodes[j]) then begin
            inc(i,CountInstructions(CaptureNodes[j])+1);
            if IsBacktracking and ((Flags and brrefAUTOCALLOUT)<>0) then begin
             inc(i);
            end;
           end;
          end;
         end;
         SetLength(RegExpProgram.Instructions,i+1);
         FillChar(RegExpProgram.Instructions[0],(i+1)*sizeof(TBRRERegExpNFAInstruction),#0);
         RegExpProgram.Start:=@RegExpProgram.Instructions[1];
         Instruction:=RegExpProgram.Start;
         AnchorStart:=0;
         AnchorEnd:=0;
         WorkBreak:=0;
         NonWorkBreak:=0;
         for i:=0 to Nodes.Count-1 do begin
          Node:=Nodes[i];
          case Node^.NodeType of
           ntBOS,ntBOSL,ntBOML:begin
            inc(AnchorStart);
           end;
           ntEOS,ntEOSL,ntEOML:begin
            inc(AnchorENd);
           end;
           ntBRK:begin
            inc(WorkBreak);
           end;
           ntNBRK:begin
            inc(NonWorkBreak);
           end;
          end;
         end;
//       Prefix:='';
         Emit(Node);
         RegExpProgram.CanDFA:=(Flags and brrefNFA)=0;
         if RegExpProgram.CanDFA and IsBacktracking then begin
          if (Flags and brrefCOMBINEOPTIMIZATION)<>0 then begin
           RegExpProgram.CanDFA:=false;
          end else begin
           for i:=0 to length(RegExpProgram.Instructions)-1 do begin
            if (RegExpProgram.Instructions[i].IDandOpcode and $ff) in [brreoCONTINUE,brreoEXACT,brreoGREEDY,brreoLAZY,brreoPOSSESSIVE,brreoSTOP,brreoPARALLEL,brreoONEPASS,brreoFIXEDSTRINGSEARCH] then begin
             RegExpProgram.CanDFA:=false;
             break;
            end;
           end;
          end;
         end;
         if RegExpProgram.CanDFA then begin
          if (WorkBreak or NonWorkBreak or AnchorStart or AnchorEnd)=0 then begin
           RegExpProgram.DFAMode:=brredmFAST;
          end else begin
           RegExpProgram.DFAMode:=brredmFULL;
          end;
         end else begin
          RegExpProgram.DFAMode:=brredmNONE;
         end;
         RegExpProgram.AnchorStartInstruction:=nil;
         RegExpProgram.AnchorEndInstruction:=nil;
         if AnchorStart>0 then begin
          for i:=0 to length(RegExpProgram.Instructions)-1 do begin
           if (RegExpProgram.Instructions[i].IDandOpcode and $ff) in [brreoBOML,brreoBOSL,brreoBOS] then begin
            RegExpProgram.AnchorStartInstruction:=@RegExpProgram.Instructions[i];
            break;
           end;
          end;
         end;
         if AnchorEnd>0 then begin
          for i:=length(RegExpProgram.Instructions)-1 downto 0 do begin
           if (RegExpProgram.Instructions[i].IDandOpcode and $ff) in [brreoEOML,brreoEOSL,brreoEOS] then begin
            RegExpProgram.AnchorEndInstruction:=@RegExpProgram.Instructions[i];
            break;
           end;
          end;
         end;
         if length(Prefix)>1 then begin
          RegExpProgram.FixedString:=Prefix;
          RegExpProgram.FixedStringIsPrefix:=CompileFixedStringSearch(RegExpProgram);
         end;
         if IsBacktracking and ((Flags and brrefAUTOCALLOUT)<>0) then begin
          Instruction^.IDandOpcode:=brreoCALLOUT;
          Instruction^.Value:=255;
          Instruction^.x:=Instruction;
          inc(Instruction^.x);
          inc(Instruction);
         end;
         Instruction^.IDandOpcode:=brreoMATCH;
         inc(Instruction);
         if HasRecursive then begin
          CaptureInstructions.Clear;
          for j:=0 to CaptureNodes.Count-1 do begin
           if assigned(CaptureNodes[j]) then begin
            CaptureInstructions.Add(Instruction);
            Emit(CaptureNodes[j]);
            if IsBacktracking and ((Flags and brrefAUTOCALLOUT)<>0) then begin
             Instruction^.IDandOpcode:=brreoCALLOUT;
             Instruction^.Value:=255;
             Instruction^.x:=Instruction;
             inc(Instruction^.x);
             inc(Instruction);
            end;
            Instruction^.IDandOpcode:=brreoMATCH;
            inc(Instruction);
           end else begin
            CaptureInstructions.Add(nil);
           end;
          end;
         end;
         RegExpProgram.Len:=((ptruint(Instruction)-ptruint(RegExpProgram.Start)) div sizeof(TBRRERegExpNFAInstruction))+1;
         NumberizeInstructions(@RegExpProgram.Instructions[1],Instruction);
         OptimizeInstructions(@RegExpProgram.Instructions[1],Instruction);
         FinalizeInstructions(@RegExpProgram.Instructions[1],Instruction);
         if (Flags and brrefRANGE)<>0 then begin
          CompileRange(RegExpProgram,RegExpProgram.Start);
         end;
        end;
        if not IsBacktracking then begin
         RegExpProgram.IsOnePass:=CompileOnePassNFA(@RegExpProgram.Instructions[1],Instruction,RegExpProgram);
         RegExpProgram.CanBitstateNFA:=RegExpProgram.Len<512;
        end;
        if RegExpProgram.CanDFA or not IsBacktracking then begin
         RegExpProgram.AnchoredInstructions:=RegExpProgram.Instructions;
         RegExpProgram.AnchoredStart:=RegExpProgram.Start;
         RegExpProgram.Instructions:=nil;
         RegExpProgram.Start:=nil;
        end;
        Node:=NewNode(ntCAT,NewStar(NewNode(ntDOT,nil,nil,nil,0),brreqkLAZY),Node,nil,0);
       end else begin
        RegExpProgram.HasRootCapture:=false;
       end;
      end;
      begin
       i:=CountInstructions(Node)+1;
       if IsBacktracking and ((Flags and brrefAUTOCALLOUT)<>0) then begin
        inc(i);
       end;
       if HasRecursive then begin
        for j:=0 to CaptureNodes.Count-1 do begin
         if assigned(CaptureNodes[j]) then begin
          inc(i,CountInstructions(CaptureNodes[j])+1);
          if IsBacktracking and ((Flags and brrefAUTOCALLOUT)<>0) then begin
           inc(i);
          end;
         end;
        end;
       end;
       SetLength(RegExpProgram.Instructions,i+1);
       FillChar(RegExpProgram.Instructions[0],(i+1)*sizeof(TBRRERegExpNFAInstruction),#0);
       RegExpProgram.Start:=@RegExpProgram.Instructions[1];
       Instruction:=RegExpProgram.Start;
       Emit(Node);
       if IsBacktracking and ((Flags and brrefAUTOCALLOUT)<>0) then begin
        Instruction^.IDandOpcode:=brreoCALLOUT;
        Instruction^.Value:=255;
        Instruction^.x:=Instruction;
        inc(Instruction^.x);
        inc(Instruction);
       end;
       Instruction^.IDandOpcode:=brreoMATCH;
       inc(Instruction);
       if HasRecursive then begin
        CaptureInstructions.Clear;
        for j:=0 to CaptureNodes.Count-1 do begin
         if assigned(CaptureNodes[j]) then begin
          CaptureInstructions.Add(Instruction);
          Emit(CaptureNodes[j]);
          if IsBacktracking and ((Flags and brrefAUTOCALLOUT)<>0) then begin
           Instruction^.IDandOpcode:=brreoCALLOUT;
           Instruction^.Value:=255;
           Instruction^.x:=Instruction;
           inc(Instruction^.x);
           inc(Instruction);
          end;
          Instruction^.IDandOpcode:=brreoMATCH;
          inc(Instruction);
         end else begin
          CaptureInstructions.Add(nil);
         end;
        end;
       end;
       RegExpProgram.Len:=((ptruint(Instruction)-ptruint(RegExpProgram.Start)) div sizeof(TBRRERegExpNFAInstruction))+1;
       NumberizeInstructions(@RegExpProgram.Instructions[1],Instruction);
       OptimizeInstructions(@RegExpProgram.Instructions[1],Instruction);
       FinalizeInstructions(@RegExpProgram.Instructions[1],Instruction);
       if (Flags and brrefRANGE)<>0 then begin
        CompileRange(RegExpProgram,RegExpProgram.Start);
       end;
      end;
     end;
     RegExpProgram.CountOfCaptures:=RegExp.CountOfCaptures-RegExpProgram.FirstCapture;
     CountOfChars:=0;
     for i:=0 to Nodes.Count-1 do begin
      Node:=Nodes[i];
      if Node^.NodeType in [ntCHAR,ntDOT] then begin
       inc(CountOfChars);
      end;
     end;
     if (not IsBacktracking) and (CountOfChars=0) then begin
      result:=false;
      FreeAndNil(RegExpProgram);
     end else begin
      ProgramIndex:=RegExp.RegExpPrograms.Add(RegExpProgram);
      if IsBacktracking and not assigned(RegExp.StartProgram) then begin
       RegExp.StartProgram:=RegExpProgram;
      end;
      result:=true;
     end;
    except
     result:=false;
     FreeAndNil(RegExpProgram);
     raise;
    end;
   finally
    for i:=0 to Nodes.Count-1 do begin
     Node:=Nodes[i];
     Dispose(Node);
    end;
    FreeAndNil(Nodes);
    Prefix:='';
   end;
  except
   result:=false;
   if IsBacktracking then begin
    raise;
   end;
  end;
 finally
  Nodes:=OldNodes;
  IsBacktracking:=OldIsBacktracking;
  HasBacktrackingParent:=OldHasBacktrackingParent;
  RegExpProgram:=OldRegExpProgram;
  Instruction:=OldInstruction;
 end;
end;

function TBRRERegExpCompiler.PrefilterOptimize(Node:TBRRERegExpPrefilterNode):TBRRERegExpPrefilterNode;
var OK:boolean;
    Counter:longint;
    Temp:TBRRERegExpPrefilterNode;
begin
 result:=Node;
 while assigned(result) do begin
  for Counter:=0 to result.Subs.Count do begin
   result.Subs[Counter]:=PrefilterOptimize(result.Subs[Counter]);
  end;
  case result.Operation of
   brrepfnoAND,brrepfnoOR:begin
    OK:=false;
    Counter:=0;
    while Counter<result.Subs.Count do begin
     Temp:=result.Subs[Counter];
     if not assigned(Temp) then begin
      result.Subs.Delete(Counter);
      OK:=true;
     end else if Temp.Operation=brrepfnoANY then begin
      Temp.Destroy;
      result.Subs.Delete(Counter);
      result.Exact:=false;
      OK:=true;
     end else begin
      inc(Counter);
     end;
    end;
    case result.Subs.Count of
     0:begin
      result.Destroy;
      result:=nil;
     end;
     1:begin
      Temp:=result.Subs[0];
      Temp.Exact:=Temp.Exact and result.Exact;
      result.Subs.Clear;
      result.Destroy;
      result:=Temp;
     end;
     else begin
      if OK then begin
       continue;
      end;
     end;
    end;
   end;
  end;
  break;
 end;
end;

function TBRRERegExpCompiler.SearchAllFirstCharsAndCheckIt(RegExpProgram:TBRRERegExpProgram):boolean;
var Visited:array of boolean;
 function Process(Instruction:PBRRERegExpNFAInstruction):boolean;
 var Ignore:boolean;
 begin
  result:=false;
  Ignore:=false;
  while assigned(Instruction) and not Visited[Instruction^.IDandOpcode shr 8] do begin
   Visited[Instruction^.IDandOpcode shr 8]:=true;
   case Instruction^.IDandOpcode and $ff of
    brreoCHAR:begin
     AllFirstCharsClass.Combine(TBRRERegExpCharClass(pointer(Instruction^.Value)));
     result:=result or not Ignore;
     break;
    end;
    brreoCHARSINGLE:begin
     AllFirstCharsClass.AddChar(Instruction^.Value,false);
     result:=result or not Ignore;
     break;
    end;
    brreoJMP,brreoEND,brreoBEGIN,brreoBOML,brreoEOML,brreoBOSL,brreoEOSL,brreoBOS,brreoEOS,brreoBRK,brreoNBRK,brreoPREFIX,brreoPREFIXCHAR,brreoANY,brreoBACKREF,brreoBACKREFI,brreoCONTINUE:begin
     Instruction:=Instruction^.x;
     continue;
    end;
    brreoSPLIT,brreoSPLITLONGEST,brreoGREEDY,brreoLAZY,brreoPOSSESSIVE,brreoTESTBACKREF,brreoTEST:begin
     Ignore:=true;
     Process(Instruction^.y);
     Instruction:=Instruction^.x;
     continue;
    end;
    brreoLOOKAHEADFALSE,brreoLOOKAHEADTRUE,brreoLOOKBEHINDFALSE,brreoLOOKBEHINDTRUE:begin
     Instruction:=Instruction^.y;
     continue;
    end;
    brreoPARALLEL,brreoONEPASS:begin
     if SearchAllFirstCharsAndCheckIt(RegExp.RegExpPrograms[Instruction^.Value]) then begin
      result:=result or not Ignore;
     end;
     break;
    end;
    brreoFIXEDSTRINGSEARCH:begin
     if length(TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).FixedString)>0 then begin
      AllFirstCharsClass.AddChar(byte(ansichar(TBRRERegExpProgram(RegExp.RegExpPrograms[Instruction^.Value]).FixedString[1])),false);
      result:=result or not Ignore;
     end;
     break;
    end;
    else begin
     break;
    end;
   end;
  end;
 end;
begin
 Visited:=nil;
 SetLength(Visited,length(RegExpProgram.Instructions)+2);
 try
  FillChar(Visited[0],length(Visited)*sizeof(boolean),#0);
  result:=Process(RegExpProgram.Start);
 finally
  SetLength(Visited,0);
 end;
end;

procedure TBRRERegExpCompiler.Compile(const ARegExp:ansistring);
var FlagsStr:ansistring;
    i,j,IsUTF8:longint;
    StartDelimiter,EndDelimiter:ansichar;
    Instruction:PBRRERegExpNFAInstruction;
begin
 ResetCaptures;
 try
  FlagsStr:='';
  RegExp.Source:=ARegExp;
  SourceStr:=AnsiString(trim(String(ARegExp)));
  if length(SourceStr)=0 then begin
   raise EBRRESyntaxError.Create('Invalid regular expression');
  end;
  if (Flags and brrefDELIMITERS)<>0 then begin
   StartDelimiter:=SourceStr[1];
   case StartDelimiter of
    '(':begin
     EndDelimiter:=')';
    end;
    '[':begin
     EndDelimiter:=']';
    end;
    '{':begin
     EndDelimiter:='}';
    end;
    else begin
     EndDelimiter:=StartDelimiter;
    end;
   end;
   i:=0;
   for j:=length(SourceStr) downto 2 do begin
    if SourceStr[j]=EndDelimiter then begin
     i:=j;
     break;
    end;
   end;
   if i=0 then begin
    raise EBRRESyntaxError.Create('Invalid regular expression');
   end;
   FlagsStr:=copy(SourceStr,i+1,(length(SourceStr)-i)+1);
   SourceStr:=copy(SourceStr,2,i-2);
   for i:=1 to length(FlagsStr) do begin
    case FlagsStr[i] of
     'b':begin
      if (Flags and brrefBACKTRACKING)<>0 then begin
       raise EBRRESyntaxError.Create('Too many backtracking regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefBACKTRACKING;
      end;
     end;
     'B':begin
      if (Flags and brrefNOBACKTRACKING)<>0 then begin
       raise EBRRESyntaxError.Create('Too many no backtracking regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefNOBACKTRACKING;
      end;
     end;
     'l':begin
      if (Flags and brrefLATIN1)<>0 then begin
       raise EBRRESyntaxError.Create('Too many LATIN1 regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefLATIN1;
      end;
     end;
     'x':begin
      if (Flags and brrefFREESPACING)<>0 then begin
       raise EBRRESyntaxError.Create('Too many free-spacing regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefFREESPACING;
      end;
     end;
     'i':begin
      if (Flags and brrefIGNORECASE)<>0 then begin
       raise EBRRESyntaxError.Create('Too many ignore-case regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefIGNORECASE;
      end;
     end;
     'n':begin
      if (Flags and brrefNAMED)<>0 then begin
       raise EBRRESyntaxError.Create('Too many named regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefNAMED;
      end;
     end;
     's':begin
      if (Flags and brrefSINGLELINE)<>0 then begin
       raise EBRRESyntaxError.Create('Too many single-line regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefSINGLELINE;
      end;
     end;
     'm':begin
      if (Flags and brrefMULTILINE)<>0 then begin
       raise EBRRESyntaxError.Create('Too many multi-line regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefMULTILINE;
      end;
     end;
     'u':begin
      if (Flags and brrefUTF8)<>0 then begin
       raise EBRRESyntaxError.Create('Too many UTF8 regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefUTF8;
      end;
     end;
     'T':begin
      if (Flags and brrefUTF8CODEUNITS)<>0 then begin
       raise EBRRESyntaxError.Create('Too many UTF8 codeunits regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefUTF8CODEUNITS;
      end;
     end;
     'c':begin
      if (Flags and brrefCONTINUATION)<>0 then begin
       raise EBRRESyntaxError.Create('Too many continuation regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefCONTINUATION;
      end;
     end;
     'a':begin
      if (Flags and brrefAUTO)<>0 then begin
       raise EBRRESyntaxError.Create('Too many automatic LATIN1/UTF8 regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefAUTO;
      end;
     end;
     'w':begin
      if (Flags and brrefWHOLEONLY)<>0 then begin
       raise EBRRESyntaxError.Create('Too many whole only regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefWHOLEONLY;
      end;
     end;
     'o':begin
      if (Flags and brrefCOMBINEOPTIMIZATION)<>0 then begin
       raise EBRRESyntaxError.Create('Too many combine optimization regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefCOMBINEOPTIMIZATION;
      end;
     end;
     'U':begin
      if (Flags and brrefUNGREEDY)<>0 then begin
       raise EBRRESyntaxError.Create('Too many ungreedy regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefUNGREEDY;
      end;
     end;
     'p':begin
      if (Flags and brrefLONGEST)<>0 then begin
       raise EBRRESyntaxError.Create('Too many ungreedy regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefLONGEST;
      end;
     end;
     'f':begin
      if (Flags and brrefNFA)<>0 then begin
       raise EBRRESyntaxError.Create('Too many NFA regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefNFA;
      end;
     end;
     'F':begin
      if (Flags and brrefDFA)<>0 then begin
       raise EBRRESyntaxError.Create('Too many DFA regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefDFA;
      end;
     end;
     'P':begin
      if (Flags and brrefPREFILTER)<>0 then begin
       raise EBRRESyntaxError.Create('Too many prefilter regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefPREFILTER;
      end;
     end;
     'O':begin
      if (Flags and brrefPREFILTEROPT)<>0 then begin
       raise EBRRESyntaxError.Create('Too many prefilter optimization regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefPREFILTEROPT;
      end;
     end;
     'C':begin
      if (Flags and brrefAUTOCALLOUT)<>0 then begin
       raise EBRRESyntaxError.Create('Too many auto callout regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefAUTOCALLOUT;
      end;
     end;
     'E':begin
      if (Flags and brrefNOTEMPTY)<>0 then begin
       raise EBRRESyntaxError.Create('Too many not empty regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefNOTEMPTY;
      end;
     end;
     'R':begin
      if (Flags and brrefRANGE)<>0 then begin
       raise EBRRESyntaxError.Create('Too many range regular expression modifier flags');
      end else begin
       Flags:=Flags or brrefRANGE;
      end;
     end;
     else begin
      raise EBRRESyntaxError.Create('Unknown regular expression modifier flag');
     end;
    end;
   end;
  end;
  if (Flags and brrefLATIN1)<>0 then begin
   IsUTF8:=brresuNOUTF8;
   if (Flags and (brrefUTF8 or brrefAUTO))<>0 then begin
    raise EBRRESyntaxError.Create('Invalid regular expression modifier flag combination');
   end;
  end else if (Flags and brrefUTF8)<>0 then begin
   IsUTF8:=brresuISUTF8;
   if (Flags and (brrefLATIN1 or brrefAUTO))<>0 then begin
    raise EBRRESyntaxError.Create('Invalid regular expression modifier flag combination');
   end;
  end else if (Flags and brrefAUTO)<>0 then begin
   IsUTF8:=BRREGetUTF8(SourceStr);
   if IsUTF8<>brresuNOUTF8 then begin
    SourceStr:=BRREUTF8Trim(SourceStr);
   end;
  end else begin
   IsUTF8:=brresuNOUTF8;
   Flags:=Flags or brrefLATIN1;
  end;
  RegExp.MaxRef:=0;
  AtEOF:=false;
  if (Flags and brrefUTF8)<>0 then begin
   if IsUTF8=brresuNOUTF8 then begin
    SourceStr:=BRREUTF8Correct(SourceStr);
    RegExpIsUTF8:=brresuISUTF8;
   end else begin
    RegExpIsUTF8:=IsUTF8;
   end;
  end else if (Flags and brrefLATIN1)<>0 then begin
   RegExpIsUTF8:=brresuNOUTF8;
  end else begin
   RegExpIsUTF8:=IsUTF8;
  end;
  SourcePos:=1;
  NextChar;
  IsFirstChar:=true;
  i:=-1;
  Pass(true,i,false);
  if assigned(RegExp.StartProgram) and assigned(RegExp.StartProgram.Start) then begin
   if (Flags and brrefPREFILTEROPT)<>0 then begin
    RegExp.PrefilterRootNode:=PrefilterOptimize(RegExp.StartProgram.PrefilterRootNode);
   end else begin
    RegExp.PrefilterRootNode:=RegExp.StartProgram.PrefilterRootNode;
   end;
   RegExp.StartProgram.PrefilterRootNode:=nil;
   RegExp.StartProgram.SearchForFirstChars:=SearchAllFirstCharsAndCheckIt(RegExp.StartProgram);
   if ((((RegExp.StartProgram.Start^.IDandOpcode and $ff) in [brreoPARALLEL,brreoONEPASS,brreoFIXEDSTRINGSEARCH])) and assigned(RegExp.StartProgram.Start^.x)) and ((RegExp.StartProgram.Start^.x^.IDandOpcode and $ff)=brreoMATCH) then begin
    RegExp.StartProgram:=TBRRERegExpProgram(RegExp.RegExpPrograms[RegExp.StartProgram.Start^.Value]);
   end else if RegExp.StartProgram.SearchForFirstChars and ((((RegExp.StartProgram.Start^.IDandOpcode and $ff)=brreoLAZY) and assigned(RegExp.StartProgram.Start^.x) and assigned(RegExp.StartProgram.Start^.y)) and (((RegExp.StartProgram.Start^.x^.IDandOpcode and $ff)=brreoANY) and ((RegExp.StartProgram.Start^.y^.IDandOpcode and $ff)=brreoBEGIN))) then begin
    if assigned(RegExp.StartProgram.Start^.y) then begin
     Instruction:=RegExp.StartProgram.Start^.y;
     if RegExp.StartProgram.FixedStringIsPrefix and (length(RegExp.StartProgram.FixedString)>0) then begin
      RegExp.StartProgram.Start^.IDandOpcode:=(RegExp.StartProgram.Start^.IDandOpcode and not $ff) or brreoPREFIX;
      RegExp.StartProgram.Start^.x:=RegExp.StartProgram.Start^.y;
     end else begin
      RegExp.StartProgram.Start^.IDandOpcode:=(RegExp.StartProgram.Start^.IDandOpcode and not $ff) or brreoPREFIXCHAR;
      RegExp.StartProgram.Start^.Value:=Instruction^.Value;
      RegExp.StartProgram.Start^.x:=RegExp.StartProgram.Start^.y;
     end;
    end;
   end;
   if RegExp.StartProgram.Method=brrepmBACKTRACKINGNFA then begin
    if (Flags and (brrefNOBACKTRACKING or brrefDFA))<>0 then begin
     SyntaxError;
    end else if (Flags and brrefLONGEST)<>0 then begin
     for i:=0 to length(RegExp.StartProgram.Instructions)-1 do begin
      Instruction:=@RegExp.StartProgram.Instructions[i];
      case Instruction^.IDandOpcode and $ff of
       brreoSPLIT:begin
        Instruction^.IDandOpcode:=(Instruction^.IDandOpcode and not $ff) or brreoSPLITLONGEST;
       end;
      end;
     end;
    end;
   end else if (RegExp.StartProgram.Method<>brrepmFIXEDSTRINGSEARCH) and (((Flags and brrefDFA)<>0) and (RegExp.StartProgram.DFAMode=brredmNONE)) then begin
    SyntaxError;
   end;
  end;
  if RegExp.MaxRef>=RegExp.CountOfCaptures then begin
   SyntaxError;
  end;
  for i:=0 to RegExp.RegExpPrograms.Count-1 do begin
   if assigned(RegExp.RegExpPrograms[i]) then begin
    if assigned(TBRRERegExpProgram(RegExp.RegExpPrograms[i]).PrefilterRootNode) then begin
     TBRRERegExpProgram(RegExp.RegExpPrograms[i]).PrefilterRootNode.Destroy;
     TBRRERegExpProgram(RegExp.RegExpPrograms[i]).PrefilterRootNode:=nil;
    end;
    TBRRERegExpProgram(RegExp.RegExpPrograms[i]).ID:=i;
   end;
  end;
  for i:=0 to CharClasses.Count-1 do begin
   CharClasses[i].Finalize;
  end;
  AllFirstCharsClass.Finalize;
  AllCharsClass.Finalize;
  RegExp.Flags:=Flags;
  RegExp.RegExpIsUTF8:=RegExpIsUTF8;
 finally
  CharClasses.SetCapacity(CharClasses.Count);
  SourceStr:='';
  FlagsStr:='';
 end;
end;

constructor TBRRERegExp.Create(const ARegExp:ansistring;AFlags:longword=brrefDELIMITERS);
var Compiler:TBRRERegExpCompiler;
begin
 inherited Create;
 FirstCharClass:=nil;
 LastCharClass:=nil;
 Source:='';
 RegExpPrograms:=TBRREPointerList.Create;
 StartProgram:=nil;
 RegExpDataInstanceList:=TBRREPointerList.Create;
 FreeRegExpDataInstance:=nil;
 CriticalSection:=TCriticalSection.Create;
 Flags:=AFlags;
 CountOfCaptures:=0;
 MaxRef:=0;
 CaptureIdentifierHashMap:=TBRREStringHashMap.Create;
 CaptureIdentifiers:=TStringList.Create;
 CharClassesHashMap:=TBRREHashMap.Create;
 CharClasses:=TBRRERegExpCharClasses.Create;
 AllFirstCharsClass:=TBRRERegExpCharClass.Create(self);
 AllCharsClass:=TBRRERegExpCharClass.Create(self);
 AnyCharClass:=TBRRERegExpCharClass.Create(self);
 AnyCharClass.AddRange($00000000,$ffffffff);
 AddCharClass(AnyCharClass);
 LastIndexCodePointValue:=0;
 LastIndexCodeUnitValue:=0;
 MaxMatchLimit:=$7ffffffe;
 MaxMatchLimitRecursion:=1000000;
 MaxMatchLimitStackRecursion:=4096;
 PrefilterRootNode:=nil;
 CalloutFunc:=nil;
 CalloutData:=nil;
{$IFDEF BRRERegExpDebug}
 RegExpDebug:=0;
{$ENDIF}
 RegExpIsUTF8:=brresuNOUTF8;

 Compiler:=TBRRERegExpCompiler.Create(self);
 try
  Compiler.Compile(ARegExp);
 finally
  Compiler.Destroy;
 end;
 
end;

destructor TBRRERegExp.Destroy;
var i:longint;
    RegExpProgram:TBRRERegExpProgram;
    RegExpDataInstance:TBRRERegExpDataInstance;
begin
 AllFirstCharsClass.Destroy;
 AllCharsClass.Destroy;
 if assigned(PrefilterRootNode) then begin
  PrefilterRootNode.Destroy;
  PrefilterRootNode:=nil;
 end;
 CharClasses.Destroy;
 CharClassesHashMap.Destroy;
 while assigned(FirstCharClass) do begin
  FirstCharClass.Destroy;
 end;
 CaptureIdentifierHashMap.Destroy;
 CaptureIdentifiers.Destroy;
 Source:='';
 for i:=0 to RegExpDataInstanceList.Count-1 do begin
  RegExpDataInstance:=TBRRERegExpDataInstance(RegExpDataInstanceList[i]);
  if assigned(RegExpDataInstance) then begin
   RegExpDataInstance.Destroy;
  end;
 end;
 RegExpDataInstanceList.Destroy;
 FreeRegExpDataInstance:=nil;
 for i:=0 to RegExpPrograms.Count-1 do begin
  RegExpProgram:=TBRRERegExpProgram(RegExpPrograms[i]);
  if assigned(RegExpProgram) then begin
   RegExpProgram.Destroy;
  end;
 end;
 RegExpPrograms.Destroy;
 CriticalSection.Destroy;
 inherited Destroy;
end;

function TBRRERegExp.AddCharClass(var CharClass:TBRRERegExpCharClass):longint;
begin
 result:=CharClassesHashMap[CharClass];
 if result>=0 then begin
  FreeAndNil(CharClass);
 end else begin
  result:=CharClasses.Add(CharClass);
  CharClassesHashMap[CharClass]:=result;
 end;
end;

procedure TBRRERegExp.RaiseMatchLimitOverflow;
begin
 raise EBRREMatchLimitOverflow.Create('Match limit overflow');
end;

procedure TBRRERegExp.RaiseMatchLimitRecursionOverflow;
begin
 raise EBRREMatchLimitRecursionOverflow.Create('Match limit recursion overflow');
end;

procedure TBRRERegExp.RaiseMatchLimitStackRecursionOverflow;
begin
 raise EBRREMatchLimitRecursionOverflow.Create('Match limit stack recursion overflow');
end;

function TBRRERegExp.IsWordChar(CharValue:longword):boolean; {$IFDEF caninline}inline;{$ENDIF}
begin
 if CharValue=$ffffffff then begin
  result:=false;
 end else begin
  if RegExpIsUTF8<>brresuNOUTF8 then begin
   result:=BRREUnicodeIsWord(CharValue);
  end else begin
   case CharValue of
    ord('a')..ord('z'),ord('A')..ord('Z'),ord('0')..ord('9'),ord('_'):begin
     result:=true;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
 end;
end;

function TBRRERegExp.AcquireRegExpDataInstance:TBRRERegExpDataInstance;
begin
 CriticalSection.Enter;
 try
  result:=FreeRegExpDataInstance;
  if assigned(result) then begin
   FreeRegExpDataInstance:=result.NextFree;
  end else begin
   result:=TBRRERegExpDataInstance.Create(self);
   RegExpDataInstanceList.Add(result);
  end;
 finally
  CriticalSection.Leave;
 end;
end;

procedure TBRRERegExp.ReleaseRegExpDataInstance(Instance:TBRRERegExpDataInstance);
begin
 CriticalSection.Enter;
 try
  Instance.NextFree:=FreeRegExpDataInstance;
  FreeRegExpDataInstance:=Instance;
 finally
  CriticalSection.Leave;
 end;
end;

function TBRRERegExp.PrefilterExpression:ansistring;
begin
 result:='';
 if assigned(PrefilterRootNode) then begin
  result:=PrefilterRootNode.Expression;
  if (length(result)>0) and (result[1]='(') then begin
   result:=copy(result,2,length(result)-2);
  end;
 end;
end;

function TBRRERegExp.PrefilterShortExpression:ansistring;
begin
 result:='';
 if assigned(PrefilterRootNode) then begin
  result:=PrefilterRootNode.ShortExpression;
  if (length(result)>0) and (result[1]='(') then begin
   result:=copy(result,2,length(result)-2);
  end;
 end;
 if length(result)=0 then begin
  result:='*';
 end;
end;

function TBRRERegExp.PrefilterSQLBooleanFullTextExpression:ansistring;
begin
 result:='';
 if assigned(PrefilterRootNode) then begin
  result:=PrefilterRootNode.SQLBooleanFullTextExpression;
  if (length(result)>0) and (result[1]='(') then begin
   result:=copy(result,2,length(result)-2);
  end;
 end;
end;

function TBRRERegExp.PrefilterSQLExpression(Field:ansistring):ansistring;
begin
 result:='';
 if assigned(PrefilterRootNode) then begin
  if length(trim(String(Field)))=0 then begin
   Field:='textfield';
  end;
  result:=PrefilterRootNode.SQLExpression(Field);
 end;
 if length(result)=0 then begin
  result:='('+Field+' LIKE "%")';
 end;
end;

function TBRRERegExp.GetRange(var RangeLow,RangeHigh:ansistring):longint;
begin
 if assigned(StartProgram) then begin
  result:=length(StartProgram.RangeLow);
  RangeLow:=StartProgram.RangeLow;
  RangeHigh:=StartProgram.RangeHigh;
 end else begin
  result:=1;
  RangeLow:=#$00;
  RangeHigh:=#$ff;
 end;
end;

procedure TBRRERegExp.PrepareInput(const Input:ansistring;var InputIsUTF8:longint;StartCodePoint:longint;var StartCodeUnit,InputCodeUnitLen,InputCodePointLen:longint;DoSearchStartCodeUnit:boolean=false);
begin
 if InputIsUTF8<0 then begin
  if (Flags and brrefAUTO)<>0 then begin
   if (Flags and brrefUTF8CODEUNITS)<>0 then begin
    InputIsUTF8:=brresuNOUTF8;
   end else begin
    InputIsUTF8:=BRREGetUTF8(Input);
   end;
  end else if (Flags and (brrefUTF8 or brrefUTF8CODEUNITS))=brrefUTF8 then begin
   InputIsUTF8:=brresuISUTF8;
  end else {if brrefLATIN1 in Flags then} begin
   InputIsUTF8:=brresuNOUTF8;
  end;
 end;
 if InputCodeUnitLen<0 then begin
  InputCodeUnitLen:=length(Input);
 end;
 if InputIsUTF8=brresuISUTF8 then begin
  if InputCodePointLen<0 then begin
   InputCodePointLen:=BRREUTF8Length(Input);
  end;
  if DoSearchStartCodeUnit then begin
   StartCodeUnit:=BRREUTF8GetCodeUnit(Input,StartCodePoint);
  end;
 end else begin
  if InputCodePointLen<0 then begin
   InputCodePointLen:=InputCodeUnitLen;
  end;
  if DoSearchStartCodeUnit then begin
   StartCodeUnit:=StartCodePoint+1;
  end;
 end;
end;

procedure TBRRERegExp.PtrPrepareInput(const Input:pansichar;var InputIsUTF8:longint;StartCodePoint:longint;var StartCodeUnit,InputCodeUnitLen,InputCodePointLen:longint;DoSearchStartCodeUnit:boolean=false);
begin
 if InputIsUTF8<0 then begin
  if (Flags and brrefAUTO)<>0 then begin
   if (Flags and brrefUTF8CODEUNITS)<>0 then begin
    InputIsUTF8:=brresuNOUTF8;
   end else begin
    InputIsUTF8:=BRREGetUTF8(Input);
   end;
  end else if (Flags and (brrefUTF8 or brrefUTF8CODEUNITS))=brrefUTF8 then begin
   InputIsUTF8:=brresuISUTF8;
  end else {if brrefLATIN1 in Flags then} begin
   InputIsUTF8:=brresuNOUTF8;
  end;
 end;
 if InputCodeUnitLen<0 then begin
  InputCodeUnitLen:=PtrLength(Input);
 end;
 if InputIsUTF8=brresuISUTF8 then begin
  if InputCodePointLen<0 then begin
   InputCodePointLen:=BRREUTF8PtrLength(Input,InputCodeUnitLen);
  end;
  if DoSearchStartCodeUnit then begin
   StartCodeUnit:=BRREUTF8PtrGetCodeUnit(Input,InputCodeUnitLen,StartCodePoint);
  end;
 end else begin
  if InputCodePointLen<0 then begin
   InputCodePointLen:=InputCodeUnitLen;
  end;
  if DoSearchStartCodeUnit then begin
   StartCodeUnit:=StartCodePoint;
  end;
 end;
end;

function TBRRERegExp.Match(const Input:ansistring;CodePoint,CodeUnit:longint;var OutputCaptures:TBRRERegExpCaptures;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):boolean;
var RegExpDataInstance:TBRRERegExpDataInstance;
    StartCodePoint,StartCodeUnit,EndCodePoint,EndCodeUnit,NextCodeUnit:longint;
    CurrentChar:longword;
    RegExpProgramDataInstance:PBRRERegExpProgramDataInstance;
begin
 result:=false;
 RegExpDataInstance:=AcquireRegExpDataInstance;
 try
  RegExpDataInstance.Update;
  if InputIsUTF8<0 then begin
   if (Flags and brrefAUTO)<>0 then begin
    if (Flags and brrefUTF8CODEUNITS)<>0 then begin
     InputIsUTF8:=brresuNOUTF8;
    end else begin
     InputIsUTF8:=BRREGetUTF8(Input);
    end;
   end else if (Flags and (brrefUTF8 or brrefUTF8CODEUNITS))=brrefUTF8 then begin
    InputIsUTF8:=brresuISUTF8;
   end else {if brrefLATIN1 in Flags then} begin
    InputIsUTF8:=brresuNOUTF8;
   end;
  end;
  if InputCodeUnitLen<0 then begin
   InputCodeUnitLen:=length(Input);
  end;
  if InputCodePointLen<0 then begin
   if InputIsUTF8=brresuISUTF8 then begin
    InputCodePointLen:=BRREUTF8Length(Input);
   end else begin
    InputCodePointLen:=InputCodeUnitLen;
   end;
  end;
  if assigned(LastIndexCodePoint) then begin
   RegExpDataInstance.LastIndexCodePoint:=LastIndexCodePoint^;
  end else begin
   RegExpDataInstance.LastIndexCodePoint:=0;
  end;
  if assigned(LastIndexCodeUnit) then begin
   RegExpDataInstance.LastIndexCodeUnit:=LastIndexCodeUnit^;
  end else begin
   if (InputIsUTF8=brresuISUTF8) and (assigned(LastIndexCodePoint) and not assigned(LastIndexCodeUnit)) then begin
    RegExpDataInstance.LastIndexCodeUnit:=BRREUTF8GetCodeUnit(Input,LastIndexCodePoint^);
   end else begin
    RegExpDataInstance.LastIndexCodeUnit:=1;
   end;
  end;
  RegExpDataInstance.Captures[0].StartCodePoint:=CodePoint;
  RegExpDataInstance.Captures[0].EndCodePoint:=CodePoint;
  RegExpDataInstance.Captures[0].StartCodeUnit:=CodeUnit;
  RegExpDataInstance.Captures[0].EndCodeUnit:=CodeUnit;
  RegExpDataInstance.InputIsUTF8:=InputIsUTF8=brresuISUTF8;
  RegExpDataInstance.InputUnitsLength:=InputCodeUnitLen;
  RegExpDataInstance.InputPointsLength:=InputCodePointLen;
  if assigned(StartProgram) then begin
   RegExpProgramDataInstance:=@RegExpDataInstance.RegExpPrograms[StartProgram.ID];
   if StartProgram.FixedStringIsPrefix and (length(StartProgram.FixedString)>0) and not (StartProgram.Method in [brrepmFIXEDSTRINGSEARCH,brrepmBACKTRACKINGNFA]) then begin
    StartCodePoint:=CodePoint;
    StartCodeUnit:=CodeUnit;
    if (((Flags and (brrefUTF8 or brrefUTF8CODEUNITS))=brrefUTF8)) and (InputIsUTF8<>brresuNOUTF8) then begin
     case StartProgram.FixedStringSearchMethod of
      brrefssmSHIFTOR:begin
       EndCodePoint:=CodePoint;
       EndCodeUnit:=PosShiftOrUTF8(StartProgram.FixedString,Input,StartProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,EndCodePoint,StartCodeUnit);
       if EndCodeUnit>0 then begin
        result:=true;
        CodeUnit:=EndCodeUnit;
        CodePoint:=EndCodePoint;
       end;
      end;
{     brrefssmBOYERMOORE:begin
       EndCodeUnit:=PosBoyerMooreUTF8(StartProgram.FixedString,Input,StartProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,StartProgram.FixedStringBoyerMooreNext,StartCodeUnit);
      end;}
      else {brrefssmBRUTEFORCE:}begin
       EndCodePoint:=CodePoint;
       if length(StartProgram.FixedString)=1 then begin
        EndCodeUnit:=PosCharUTF8(StartProgram.FixedString[1],Input,EndCodePoint,StartCodeUnit);
       end else begin
        EndCodeUnit:=PosExUTF8(StartProgram.FixedString,Input,EndCodePoint,StartCodeUnit);
       end;
       if EndCodeUnit>0 then begin
        result:=true;
        CodeUnit:=EndCodeUnit;
        CodePoint:=EndCodePoint;
       end;
      end;
     end;
    end else if ((Flags and brrefLATIN1)<>0) and (InputIsUTF8=brresuNOUTF8) then begin
     case StartProgram.FixedStringSearchMethod of
      brrefssmSHIFTOR:begin
       EndCodeUnit:=PosShiftOr(StartProgram.FixedString,Input,StartProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,StartCodeUnit);
      end;
      brrefssmBOYERMOORE:begin
       EndCodeUnit:=PosBoyerMoore(StartProgram.FixedString,Input,StartProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,StartProgram.FixedStringBoyerMooreNext,StartCodeUnit);
      end;
      else {brrefssmBRUTEFORCE:}begin
       if length(StartProgram.FixedString)=1 then begin
        EndCodeUnit:=PosChar(StartProgram.FixedString[1],Input,StartCodeUnit);
       end else begin
        EndCodeUnit:=PosEx(StartProgram.FixedString,Input,StartCodeUnit);
       end;
      end;
     end;
     if EndCodeUnit>0 then begin
      result:=true;
      CodeUnit:=EndCodeUnit;
      CodePoint:=EndCodeUnit-1;
     end;
    end;
   end else begin
    result:=true;
   end;
   if result then begin
    StartCodePoint:=CodePoint;
    StartCodeUnit:=CodeUnit;
    EndCodeUnit:=StartCodeUnit;
    EndCodePoint:=StartCodePoint;
    repeat
     case StartProgram.Method of
      brrepmPARALLELNFA,brrepmONEPASSNFA:begin
       if not assigned(RegExpDataInstance.DFA) then begin
        RegExpDataInstance.DFA:=TBRREDFA.Create(StartProgram,RegExpDataInstance,RegExpProgramDataInstance);
       end;
       if StartProgram.SearchForFirstChars and not StartProgram.AnchorBeginText then begin
        if RegExpDataInstance.InputIsUTF8 then begin
         while StartCodeUnit<=RegExpDataInstance.InputUnitsLength do begin
          NextCodeUnit:=StartCodeUnit;
          CurrentChar:=BRREUTF8CodeUnitGetCharAndInc(Input,NextCodeUnit);
          if ((CurrentChar<256) and (ansichar(byte(CurrentChar)) in AllFirstCharsClass.Charset)) or
             ((CurrentChar>255) and AllFirstCharsClass.Contains(CurrentChar)) then begin
           break;
          end;
          StartCodeUnit:=NextCodeUnit;
          inc(StartCodePoint);
         end;
        end else begin
         while StartCodeUnit<=RegExpDataInstance.InputUnitsLength do begin
          CurrentChar:=byte(Input[StartCodeUnit]);
          if ansichar(byte(CurrentChar)) in AllFirstCharsClass.Charset then begin
           break;
          end;
          inc(StartCodeUnit);
          inc(StartCodePoint);
         end;
        end;
       end;
       case RegExpDataInstance.DFA.Search(Input,StartCodePoint,StartCodeUnit,EndCodePoint,EndCodeUnit) of
        1:begin
         if ((Flags and brrefNOTEMPTY)<>0) and (StartCodePoint=EndCodePoint) then begin
          // Empty match? Skip it!
          StartCodeUnit:=EndCodeUnit;
          StartCodePoint:=EndCodePoint+1;
          if RegExpDataInstance.InputIsUTF8 then begin
           BRREUTF8Inc(Input,StartCodeUnit);
          end else begin
           inc(StartCodeUnit);
          end;
          if StartCodeUnit>RegExpDataInstance.InputUnitsLength then begin
           RegExpDataInstance.Captures[0].StartCodePoint:=StartCodePoint;
           RegExpDataInstance.Captures[0].EndCodePoint:=EndCodePoint;
           RegExpDataInstance.Captures[0].StartCodeUnit:=StartCodeUnit;
           RegExpDataInstance.Captures[0].EndCodeUnit:=EndCodeUnit;
           result:=false;
           break;
          end else begin
           continue;
          end;
         end;
         // Yay! We've a full match!
         RegExpDataInstance.Captures[0].StartCodePoint:=StartCodePoint;
         RegExpDataInstance.Captures[0].EndCodePoint:=EndCodePoint;
         RegExpDataInstance.Captures[0].StartCodeUnit:=StartCodeUnit;
         RegExpDataInstance.Captures[0].EndCodeUnit:=EndCodeUnit;
         if CountOfCaptures>1 then begin
          // And now search for sub matches, when there are some
          case StartProgram.Method of
           brrepmPARALLELNFA:begin
            if StartProgram.CanBitstateNFA then begin
             result:=RegExpProgramDataInstance^.BitstateNFA.Search(Input,StartCodePoint,StartCodeUnit,(EndCodeUnit-StartCodeUnit)+1,StartCodeUnit,EndCodeUnit);
             if not result then begin
              result:=RegExpProgramDataInstance^.ParallelNFA.Search(Input,StartCodePoint,StartCodeUnit);
             end;
            end else begin
             result:=RegExpProgramDataInstance^.ParallelNFA.Search(Input,StartCodePoint,StartCodeUnit);
            end;
            EndCodeUnit:=StartCodeUnit;
            EndCodePoint:=StartCodePoint;
           end;
           else{brrepmONEPASSNFA:}begin
            result:=RegExpProgramDataInstance^.OnePassNFA.Search(Input,StartCodePoint,StartCodeUnit);
           end;
          end;
         end else begin
          // We are done, when they are no sub matches!
          result:=true;
         end;
         CodePoint:=EndCodePoint;
         CodeUnit:=EndCodeUnit;
        end;
        -1:begin
         // Fallback to NFA
         case StartProgram.Method of
          brrepmPARALLELNFA:begin
           if StartProgram.CanBitstateNFA then begin
            RegExpDataInstance.Captures[0].StartCodePoint:=StartCodePoint;
            RegExpDataInstance.Captures[0].StartCodeUnit:=StartCodeUnit;
            result:=RegExpProgramDataInstance^.BitstateNFA.Search(Input,StartCodePoint,StartCodeUnit,RegExpDataInstance.InputUnitsLength,StartCodeUnit,RegExpDataInstance.InputUnitsLength);
            RegExpDataInstance.Captures[0].EndCodePoint:=StartCodePoint;
            RegExpDataInstance.Captures[0].EndCodeUnit:=StartCodeUnit;
            if not result then begin
             result:=RegExpProgramDataInstance^.ParallelNFA.Search(Input,StartCodePoint,StartCodeUnit);
            end;
           end else begin
            result:=RegExpProgramDataInstance^.ParallelNFA.Search(Input,StartCodePoint,StartCodeUnit);
           end;
           EndCodeUnit:=StartCodeUnit;
           EndCodePoint:=StartCodePoint;
          end;
          else{brrepmONEPASSNFA:}begin
           result:=false;
           while StartCodeUnit<=RegExpDataInstance.InputUnitsLength do begin
            if RegExpDataInstance.InputIsUTF8 then begin
             while StartCodeUnit<=RegExpDataInstance.InputUnitsLength do begin
              NextCodeUnit:=StartCodeUnit;
              CurrentChar:=BRREUTF8CodeUnitGetCharAndInc(Input,NextCodeUnit);
              if ((CurrentChar<256) and (ansichar(byte(CurrentChar)) in AllFirstCharsClass.Charset)) or
                 ((CurrentChar>255) and AllFirstCharsClass.Contains(CurrentChar)) then begin
               break;
              end;
              StartCodeUnit:=NextCodeUnit;
              inc(StartCodePoint);
             end;
            end else begin
             while StartCodeUnit<=RegExpDataInstance.InputUnitsLength do begin
              CurrentChar:=byte(Input[StartCodeUnit]);
              if ansichar(byte(CurrentChar)) in AllFirstCharsClass.Charset then begin
               break;
              end;
              inc(StartCodeUnit);
              inc(StartCodePoint);
             end;
            end;
            EndCodeUnit:=StartCodeUnit;
            EndCodePoint:=StartCodePoint;
            RegExpDataInstance.Captures[0].StartCodeUnit:=StartCodeUnit;
            RegExpDataInstance.Captures[0].StartCodePoint:=StartCodePoint;
            result:=RegExpProgramDataInstance^.OnePassNFA.Search(Input,StartCodePoint,StartCodeUnit);
            if result then begin
             break;
            end else begin
             if RegExpDataInstance.InputIsUTF8 then begin
              StartCodeUnit:=EndCodeUnit;
              BRREUTF8Inc(Input,StartCodeUnit);
             end else begin
              StartCodeUnit:=EndCodeUnit+1;
             end;
             StartCodePoint:=EndCodePoint+1;
            end;
           end;
           RegExpDataInstance.Captures[0].EndCodeUnit:=StartCodeUnit;
           RegExpDataInstance.Captures[0].EndCodePoint:=StartCodePoint;
           EndCodeUnit:=StartCodeUnit;
           EndCodePoint:=StartCodePoint;
          end;
         end;
        end;
        else begin
         RegExpDataInstance.Captures[0].StartCodePoint:=StartCodePoint;
         RegExpDataInstance.Captures[0].EndCodePoint:=EndCodePoint;
         RegExpDataInstance.Captures[0].StartCodeUnit:=StartCodeUnit;
         RegExpDataInstance.Captures[0].EndCodeUnit:=EndCodeUnit;
         result:=false;
        end;
       end;
      end;
      brrepmFIXEDSTRINGSEARCH:begin
       result:=RegExpProgramDataInstance^.FixedStringSearch.Search(Input,StartCodePoint,StartCodeUnit);
       EndCodeUnit:=StartCodeUnit;
       EndCodePoint:=StartCodePoint;
      end;
      else {brrepmBACKTRACKINGNFA:}begin
       if StartProgram.CanDFA then begin
        if not assigned(RegExpDataInstance.DFA) then begin
         RegExpDataInstance.DFA:=TBRREDFA.Create(StartProgram,RegExpDataInstance,RegExpProgramDataInstance);
        end;
        repeat
         EndCodeUnit:=StartCodeUnit;
         EndCodePoint:=StartCodePoint;
         case RegExpDataInstance.DFA.Search(Input,StartCodePoint,StartCodeUnit,EndCodePoint,EndCodeUnit) of
          1:begin
           EndCodeUnit:=StartCodeUnit;
           EndCodePoint:=StartCodePoint;
           result:=RegExpProgramDataInstance^.BacktrackingNFA.Search(Input,StartCodePoint,StartCodeUnit,true);
           if result then begin
            EndCodeUnit:=StartCodeUnit;
            EndCodePoint:=StartCodePoint;
           end else begin
            StartCodeUnit:=EndCodeUnit;
            StartCodePoint:=EndCodePoint+1;
            if RegExpDataInstance.InputIsUTF8 then begin
             BRREUTF8Inc(Input,StartCodeUnit);
            end else begin
             inc(StartCodeUnit);
            end;
            if StartCodeUnit<=RegExpDataInstance.InputUnitsLength then begin
             continue;
            end;
           end;
          end;
          -1:begin
           result:=RegExpProgramDataInstance^.BacktrackingNFA.Search(Input,StartCodePoint,StartCodeUnit,false);
           EndCodeUnit:=StartCodeUnit;
           EndCodePoint:=StartCodePoint;
          end;
          else begin
           result:=false;
          end;
         end;
         break;
        until false;
       end else begin
        result:=RegExpProgramDataInstance^.BacktrackingNFA.Search(Input,StartCodePoint,StartCodeUnit,false);
        EndCodeUnit:=StartCodeUnit;
        EndCodePoint:=StartCodePoint;
       end;
      end;
     end;
     CodePoint:=EndCodePoint;
     CodeUnit:=EndCodeUnit;
     if ((Flags and brrefNOTEMPTY)<>0) and (result and (RegExpDataInstance.Captures[0].StartCodePoint>=RegExpDataInstance.Captures[0].EndCodePoint)) then begin
      // Empty or invalid match? Skip it!
      StartCodeUnit:=RegExpDataInstance.Captures[0].EndCodeUnit;
      StartCodePoint:=RegExpDataInstance.Captures[0].EndCodePoint+1;
      if RegExpDataInstance.InputIsUTF8 then begin
       BRREUTF8Inc(Input,StartCodeUnit);
      end else begin
       inc(StartCodeUnit);
      end;
      if StartCodeUnit>RegExpDataInstance.InputUnitsLength then begin
       result:=false;
       break;
      end;
      continue;
     end else begin
      break;
     end;
    until false;
   end;
  end;
  if result then begin
   RegExpDataInstance.LastIndexCodePoint:=RegExpDataInstance.Captures[0].EndCodePoint;
   RegExpDataInstance.LastIndexCodeUnit:=RegExpDataInstance.Captures[0].EndCodeUnit;
   if length(OutputCaptures)<>CountOfCaptures then begin
    SetLength(OutputCaptures,CountOfCaptures);
   end;
   if CountOfCaptures>0 then begin
    Move(RegExpDataInstance.Captures[0],OutputCaptures[0],CountOfCaptures*sizeof(TBRRERegExpCapture));
   end;
  end else begin
   if (Flags and brrefCONTINUATION)=0 then begin
    RegExpDataInstance.LastIndexCodePoint:=0;
    RegExpDataInstance.LastIndexCodeUnit:=0;
   end;
  end;
  if assigned(LastIndexCodePoint) then begin
   LastIndexCodePoint^:=RegExpDataInstance.LastIndexCodePoint;
  end;
  if assigned(LastIndexCodeUnit) then begin
   LastIndexCodeUnit^:=RegExpDataInstance.LastIndexCodeUnit;
  end;
 finally
  ReleaseRegExpDataInstance(RegExpDataInstance);
 end;
end;

function TBRRERegExp.PtrMatch(const Input:pansichar;CodePoint,CodeUnit:longint;var OutputCaptures:TBRRERegExpCaptures;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):boolean;
var RegExpDataInstance:TBRRERegExpDataInstance;
    StartCodePoint,StartCodeUnit,EndCodePoint,EndCodeUnit,NextCodeUnit:longint;
    CurrentChar:longword;
    RegExpProgramDataInstance:PBRRERegExpProgramDataInstance;
begin
 result:=false;
 RegExpDataInstance:=AcquireRegExpDataInstance;
 try
  RegExpDataInstance.Update;
  if InputCodeUnitLen<0 then begin
   InputCodeUnitLen:=PtrLength(Input);
  end;
  if InputIsUTF8<0 then begin
   if (Flags and brrefAUTO)<>0 then begin
    if (Flags and brrefUTF8CODEUNITS)<>0 then begin
     InputIsUTF8:=brresuNOUTF8;
    end else begin
     InputIsUTF8:=BRREPtrGetUTF8(Input,InputCodeUnitLen);
    end;
   end else if (Flags and (brrefUTF8 or brrefUTF8CODEUNITS))=brrefUTF8 then begin
    InputIsUTF8:=brresuISUTF8;
   end else {if brrefLATIN1 in Flags then} begin
    InputIsUTF8:=brresuNOUTF8;
   end;
  end;
  if InputCodePointLen<0 then begin
   if InputIsUTF8=brresuISUTF8 then begin
    InputCodePointLen:=BRREUTF8PtrLength(Input,InputCodeUnitLen);
   end else begin
    InputCodePointLen:=InputCodeUnitLen;
   end;
  end;
  if assigned(LastIndexCodePoint) then begin
   RegExpDataInstance.LastIndexCodePoint:=LastIndexCodePoint^;
  end else begin
   RegExpDataInstance.LastIndexCodePoint:=0;
  end;
  if assigned(LastIndexCodeUnit) then begin
   RegExpDataInstance.LastIndexCodeUnit:=LastIndexCodeUnit^;
  end else begin
   if (InputIsUTF8=brresuISUTF8) and (assigned(LastIndexCodePoint) and not assigned(LastIndexCodeUnit)) then begin
    RegExpDataInstance.LastIndexCodeUnit:=BRREUTF8PtrGetCodeUnit(Input,InputCodeUnitLen,LastIndexCodePoint^);
   end else begin
    RegExpDataInstance.LastIndexCodeUnit:=0;
   end;
  end;
  RegExpDataInstance.Captures[0].StartCodePoint:=CodePoint;
  RegExpDataInstance.Captures[0].EndCodePoint:=CodePoint;
  RegExpDataInstance.Captures[0].StartCodeUnit:=CodeUnit;
  RegExpDataInstance.Captures[0].EndCodeUnit:=CodeUnit;
  RegExpDataInstance.InputIsUTF8:=InputIsUTF8=brresuISUTF8;
  RegExpDataInstance.InputUnitsLength:=InputCodeUnitLen;
  RegExpDataInstance.InputUnitsLength:=InputCodePointLen;
  if assigned(StartProgram) then begin
   RegExpProgramDataInstance:=@RegExpDataInstance.RegExpPrograms[StartProgram.ID];
   if StartProgram.FixedStringIsPrefix and (length(StartProgram.FixedString)>0) and not (StartProgram.Method in [brrepmFIXEDSTRINGSEARCH,brrepmBACKTRACKINGNFA]) then begin
    StartCodePoint:=CodePoint;
    StartCodeUnit:=CodeUnit;
    if (((Flags and (brrefUTF8 or brrefUTF8CODEUNITS))=brrefUTF8)) and (InputIsUTF8<>brresuNOUTF8) then begin
     case StartProgram.FixedStringSearchMethod of
      brrefssmSHIFTOR:begin
       EndCodePoint:=CodePoint;
       EndCodeUnit:=PtrPosShiftOrUTF8(StartProgram.FixedString,Input,InputCodeUnitLen,StartProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,EndCodePoint,StartCodeUnit);
       if EndCodeUnit>0 then begin
        result:=true;
        CodeUnit:=EndCodeUnit;
        CodePoint:=EndCodePoint;
       end;
      end;
{     brrefssmBOYERMOORE:begin
       EndCodeUnit:=PtrPosBoyerMooreUTF8(StartProgram.FixedString,Input,InputCodeUnitLen,StartProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,StartProgram.FixedStringBoyerMooreNext,StartCodeUnit);
      end;}
      else {brrefssmBRUTEFORCE:}begin
       EndCodePoint:=CodePoint;
       if length(StartProgram.FixedString)=1 then begin
        EndCodeUnit:=PtrPosCharUTF8(StartProgram.FixedString[1],Input,InputCodeUnitLen,EndCodePoint,StartCodeUnit);
       end else begin
        EndCodeUnit:=PtrPosExUTF8(StartProgram.FixedString,Input,InputCodeUnitLen,EndCodePoint,StartCodeUnit);
       end;
       if EndCodeUnit>0 then begin
        result:=true;
        CodeUnit:=EndCodeUnit;
        CodePoint:=EndCodePoint;
       end;
      end;
     end;
    end else if ((Flags and brrefLATIN1)<>0) and (InputIsUTF8=brresuNOUTF8) then begin
     case StartProgram.FixedStringSearchMethod of
      brrefssmSHIFTOR:begin
       EndCodeUnit:=PtrPosShiftOr(StartProgram.FixedString,Input,InputCodeUnitLen,StartProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,StartCodeUnit);
      end;
      brrefssmBOYERMOORE:begin
       EndCodeUnit:=PtrPosBoyerMoore(StartProgram.FixedString,Input,InputCodeUnitLen,StartProgram.FixedStringShiftOrPatternMaskBoyerMooreSkip,StartProgram.FixedStringBoyerMooreNext,StartCodeUnit);
      end;
      else {brrefssmBRUTEFORCE:}begin
       if length(StartProgram.FixedString)=1 then begin
        EndCodeUnit:=PtrPosChar(StartProgram.FixedString[1],Input,InputCodeUnitLen,StartCodeUnit);
       end else begin
        EndCodeUnit:=PtrPosEx(StartProgram.FixedString,Input,InputCodeUnitLen,StartCodeUnit);
       end;
      end;
     end;
     if EndCodeUnit>0 then begin
      result:=true;
      CodeUnit:=EndCodeUnit;
      CodePoint:=EndCodeUnit;
     end;
    end;
   end else begin
    result:=true;
   end;
   if result then begin
    StartCodePoint:=CodePoint;
    StartCodeUnit:=CodeUnit;
    EndCodeUnit:=StartCodeUnit;
    EndCodePoint:=StartCodePoint;
    repeat
     case StartProgram.Method of
      brrepmPARALLELNFA,brrepmONEPASSNFA:begin
       if not assigned(RegExpDataInstance.DFA) then begin
        RegExpDataInstance.DFA:=TBRREDFA.Create(StartProgram,RegExpDataInstance,RegExpProgramDataInstance);
       end;
       StartCodePoint:=CodePoint;
       StartCodeUnit:=CodeUnit;
       if StartProgram.SearchForFirstChars and not StartProgram.AnchorBeginText then begin
        if RegExpDataInstance.InputIsUTF8 then begin
         while StartCodeUnit<RegExpDataInstance.InputUnitsLength do begin
          NextCodeUnit:=StartCodeUnit;
          CurrentChar:=BRREUTF8PtrCodeUnitGetCharAndInc(Input,RegExpDataInstance.InputUnitsLength,NextCodeUnit);
          if ((CurrentChar<256) and (ansichar(byte(CurrentChar)) in AllFirstCharsClass.Charset)) or
             ((CurrentChar>255) and AllFirstCharsClass.Contains(CurrentChar)) then begin
           break;
          end;
          StartCodeUnit:=NextCodeUnit;
          inc(StartCodePoint);
         end;
        end else begin
         while StartCodeUnit<RegExpDataInstance.InputUnitsLength do begin
          CurrentChar:=byte(Input[StartCodeUnit]);
          if ansichar(byte(CurrentChar)) in AllFirstCharsClass.Charset then begin
           break;
          end;
          inc(StartCodeUnit);
          inc(StartCodePoint);
         end;
        end;
       end;
       case RegExpDataInstance.DFA.PtrSearch(Input,StartCodePoint,StartCodeUnit,EndCodePoint,EndCodeUnit) of
        1:begin
         if ((Flags and brrefNOTEMPTY)<>0) and (StartCodePoint=EndCodePoint) then begin
          // Empty match? Skip it!
          StartCodeUnit:=EndCodeUnit;
          StartCodePoint:=EndCodePoint+1;
          if RegExpDataInstance.InputIsUTF8 then begin
           BRREUTF8PtrInc(Input,RegExpDataInstance.InputUnitsLength,StartCodeUnit);
          end else begin
           inc(StartCodeUnit);
          end;
          if StartCodeUnit>=RegExpDataInstance.InputUnitsLength then begin
           RegExpDataInstance.Captures[0].StartCodePoint:=StartCodePoint;
           RegExpDataInstance.Captures[0].EndCodePoint:=EndCodePoint;
           RegExpDataInstance.Captures[0].StartCodeUnit:=StartCodeUnit;
           RegExpDataInstance.Captures[0].EndCodeUnit:=EndCodeUnit;
           result:=false;
           break;
          end else begin
           continue;
          end;
         end;
         // Yay! We've a full match!
         RegExpDataInstance.Captures[0].StartCodePoint:=StartCodePoint;
         RegExpDataInstance.Captures[0].EndCodePoint:=EndCodePoint;
         RegExpDataInstance.Captures[0].StartCodeUnit:=StartCodeUnit;
         RegExpDataInstance.Captures[0].EndCodeUnit:=EndCodeUnit;
         if CountOfCaptures>1 then begin
          // And now search for sub matches, when there are some
          case StartProgram.Method of
           brrepmPARALLELNFA:begin
            if StartProgram.CanBitstateNFA then begin
             result:=RegExpProgramDataInstance^.BitstateNFA.PtrSearch(Input,StartCodePoint,StartCodeUnit,(EndCodeUnit-StartCodeUnit)+1,StartCodeUnit,EndCodeUnit);
             if not result then begin
              result:=RegExpProgramDataInstance^.ParallelNFA.PtrSearch(Input,StartCodePoint,StartCodeUnit);
             end;
            end else begin
             result:=RegExpProgramDataInstance^.ParallelNFA.PtrSearch(Input,StartCodePoint,StartCodeUnit);
            end;
            EndCodeUnit:=StartCodeUnit;
            EndCodePoint:=StartCodePoint;
           end;
           else{brrepmONEPASSNFA:}begin
            result:=RegExpProgramDataInstance^.OnePassNFA.PtrSearch(Input,StartCodePoint,StartCodeUnit);
           end;
          end;
         end else begin
          // We are done, when they are no sub matches!
          result:=true;
         end;
         CodePoint:=EndCodePoint;
         CodeUnit:=EndCodeUnit;
        end;
        -1:begin
         // Fallback to NFA
         case StartProgram.Method of
          brrepmPARALLELNFA:begin
           if StartProgram.CanBitstateNFA then begin
            RegExpDataInstance.Captures[0].StartCodePoint:=StartCodePoint;
            RegExpDataInstance.Captures[0].StartCodeUnit:=StartCodeUnit;
            result:=RegExpProgramDataInstance^.BitstateNFA.PtrSearch(Input,StartCodePoint,StartCodeUnit,RegExpDataInstance.InputUnitsLength,StartCodeUnit,RegExpDataInstance.InputUnitsLength);
            RegExpDataInstance.Captures[0].EndCodePoint:=StartCodePoint;
            RegExpDataInstance.Captures[0].EndCodeUnit:=StartCodeUnit;
            if not result then begin
             result:=RegExpProgramDataInstance^.ParallelNFA.PtrSearch(Input,StartCodePoint,StartCodeUnit);
            end;
           end else begin
            result:=RegExpProgramDataInstance^.ParallelNFA.PtrSearch(Input,StartCodePoint,StartCodeUnit);
           end;
           EndCodeUnit:=StartCodeUnit;
           EndCodePoint:=StartCodePoint;
          end;
          else{brrepmONEPASSNFA:}begin
           result:=false;
           while StartCodeUnit<=RegExpDataInstance.InputUnitsLength do begin
            if RegExpDataInstance.InputIsUTF8 then begin
             while StartCodeUnit<RegExpDataInstance.InputUnitsLength do begin
              NextCodeUnit:=StartCodeUnit;
              CurrentChar:=BRREUTF8PtrCodeUnitGetCharAndInc(Input,RegExpDataInstance.InputUnitsLength,NextCodeUnit);
              if ((CurrentChar<256) and (ansichar(byte(CurrentChar)) in AllFirstCharsClass.Charset)) or
                 ((CurrentChar>255) and AllFirstCharsClass.Contains(CurrentChar)) then begin
               break;
              end;
              StartCodeUnit:=NextCodeUnit;
              inc(StartCodePoint);
             end;
            end else begin
             while StartCodeUnit<RegExpDataInstance.InputUnitsLength do begin
              CurrentChar:=byte(Input[StartCodeUnit]);
              if ansichar(byte(CurrentChar)) in AllFirstCharsClass.Charset then begin
               break;
              end;
              inc(StartCodeUnit);
              inc(StartCodePoint);
             end;
            end;
            EndCodeUnit:=StartCodeUnit;
            EndCodePoint:=StartCodePoint;
            RegExpDataInstance.Captures[0].StartCodeUnit:=StartCodeUnit;
            RegExpDataInstance.Captures[0].StartCodePoint:=StartCodePoint;
            result:=RegExpProgramDataInstance^.OnePassNFA.PtrSearch(Input,StartCodePoint,StartCodeUnit);
            if result then begin
             break;
            end else begin
             if RegExpDataInstance.InputIsUTF8 then begin
              StartCodeUnit:=EndCodeUnit;
              BRREUTF8PtrInc(Input,RegExpDataInstance.InputUnitsLength,StartCodeUnit);
             end else begin
              StartCodeUnit:=EndCodeUnit+1;
             end;
             StartCodePoint:=EndCodePoint+1;
            end;
           end;
           RegExpDataInstance.Captures[0].EndCodeUnit:=StartCodeUnit;
           RegExpDataInstance.Captures[0].EndCodePoint:=StartCodePoint;
           EndCodeUnit:=StartCodeUnit;
           EndCodePoint:=StartCodePoint;
          end;
         end;
        end;
        else begin
         RegExpDataInstance.Captures[0].StartCodePoint:=StartCodePoint;
         RegExpDataInstance.Captures[0].EndCodePoint:=EndCodePoint;
         RegExpDataInstance.Captures[0].StartCodeUnit:=StartCodeUnit;
         RegExpDataInstance.Captures[0].EndCodeUnit:=EndCodeUnit;
         result:=false;
        end;
       end;
      end;
      brrepmFIXEDSTRINGSEARCH:begin
       result:=RegExpProgramDataInstance^.FixedStringSearch.PtrSearch(Input,StartCodePoint,StartCodeUnit);
       EndCodeUnit:=StartCodeUnit;
       EndCodePoint:=StartCodePoint;
      end;
      else {brrepmBACKTRACKINGNFA:}begin
       if StartProgram.CanDFA then begin
        if not assigned(RegExpDataInstance.DFA) then begin
         RegExpDataInstance.DFA:=TBRREDFA.Create(StartProgram,RegExpDataInstance,RegExpProgramDataInstance);
        end;
        repeat
         EndCodeUnit:=StartCodeUnit;
         EndCodePoint:=StartCodePoint;
         case RegExpDataInstance.DFA.PtrSearch(Input,StartCodePoint,StartCodeUnit,EndCodePoint,EndCodeUnit) of
          1:begin
           EndCodeUnit:=StartCodeUnit;
           EndCodePoint:=StartCodePoint;
           result:=RegExpProgramDataInstance^.BacktrackingNFA.PtrSearch(Input,StartCodePoint,StartCodeUnit,true);
           if result then begin
            EndCodeUnit:=StartCodeUnit;
            EndCodePoint:=StartCodePoint;
           end else begin
            StartCodeUnit:=EndCodeUnit;
            StartCodePoint:=EndCodePoint+1;
            if RegExpDataInstance.InputIsUTF8 then begin
             BRREUTF8PtrInc(Input,RegExpDataInstance.InputUnitsLength,StartCodeUnit);
            end else begin
             inc(StartCodeUnit);
            end;
            if StartCodeUnit<RegExpDataInstance.InputUnitsLength then begin
             continue;
            end;
           end;
          end;
          -1:begin
           result:=RegExpProgramDataInstance^.BacktrackingNFA.PtrSearch(Input,StartCodePoint,StartCodeUnit,false);
           EndCodeUnit:=StartCodeUnit;
           EndCodePoint:=StartCodePoint;
          end;
          else begin
           result:=false;
          end;
         end;
         break;
        until false;
       end else begin
        result:=RegExpProgramDataInstance^.BacktrackingNFA.PtrSearch(Input,StartCodePoint,StartCodeUnit,false);
        EndCodeUnit:=StartCodeUnit;
        EndCodePoint:=StartCodePoint;
       end;
      end;
     end;
     CodePoint:=EndCodePoint;
     CodeUnit:=EndCodeUnit;
     if ((Flags and brrefNOTEMPTY)<>0) and (result and (RegExpDataInstance.Captures[0].StartCodePoint>=RegExpDataInstance.Captures[0].EndCodePoint)) then begin
      // Empty or invalid match? Skip it!
      StartCodeUnit:=RegExpDataInstance.Captures[0].EndCodeUnit;
      StartCodePoint:=RegExpDataInstance.Captures[0].EndCodePoint+1;
      if RegExpDataInstance.InputIsUTF8 then begin
       BRREUTF8PtrInc(Input,RegExpDataInstance.InputUnitsLength,StartCodeUnit);
      end else begin
       inc(StartCodeUnit);
      end;
      if StartCodeUnit>=RegExpDataInstance.InputUnitsLength then begin
       result:=false;
       break;
      end;
      continue;
     end else begin
      break;
     end;
    until false;
   end;
  end;
  if result then begin
   RegExpDataInstance.LastIndexCodePoint:=RegExpDataInstance.Captures[0].EndCodePoint;
   RegExpDataInstance.LastIndexCodeUnit:=RegExpDataInstance.Captures[0].EndCodeUnit;
   if length(OutputCaptures)<>CountOfCaptures then begin
    SetLength(OutputCaptures,CountOfCaptures);
   end;
   if CountOfCaptures>0 then begin
    Move(RegExpDataInstance.Captures[0],OutputCaptures[0],CountOfCaptures*sizeof(TBRRERegExpCapture));
   end;
  end else begin
   if (Flags and brrefCONTINUATION)=0 then begin
    RegExpDataInstance.LastIndexCodePoint:=0;
    RegExpDataInstance.LastIndexCodeUnit:=0;
   end;
  end;
  if assigned(LastIndexCodePoint) then begin
   LastIndexCodePoint^:=RegExpDataInstance.LastIndexCodePoint;
  end;
  if assigned(LastIndexCodeUnit) then begin
   LastIndexCodeUnit^:=RegExpDataInstance.LastIndexCodeUnit;
  end;
 finally
  ReleaseRegExpDataInstance(RegExpDataInstance);
 end;
end;

function TBRRERegExp.MatchAll(const Input:ansistring;var OutputMultipleCaptures:TBRRERegExpMultipleCaptures;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=1;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):longint;
var NextCount,TempLastIndexCodePoint,TempLastIndexCodeUnit:longint;
    WholeCapture:TBRRERegExpCapture;
begin
 result:=0;
 TempLastIndexCodePoint:=0;
 TempLastIndexCodeUnit:=0;
 if not assigned(LastIndexCodePoint) then begin
  LastIndexCodePoint:=@TempLastIndexCodePoint;
 end;
 if not assigned(LastIndexCodeUnit) then begin
  LastIndexCodeUnit:=@TempLastIndexCodeUnit;
 end;
 PrepareInput(Input,InputIsUTF8,StartCodePoint,StartCodeUnit,InputCodePointLen,InputCodeUnitLen,StartCodeUnit<=0);
 if (InputIsUTF8=brresuISUTF8) and ((LastIndexCodePoint<>@TempLastIndexCodePoint) and (LastIndexCodeUnit=@TempLastIndexCodeUnit)) then begin
  TempLastIndexCodeUnit:=BRREUTF8GetCodeUnit(Input,LastIndexCodePoint^);
 end;
 while (Limit<>0) and (StartCodePoint<=InputCodePointLen) do begin
  NextCount:=result+1;
  if NextCount>=length(OutputMultipleCaptures) then begin
   SetLength(OutputMultipleCaptures,BRRERoundUpToPowerOfTwo(NextCount+1));
  end;
  if Match(Input,StartCodePoint,StartCodeUnit,OutputMultipleCaptures[result],InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit) then begin
   WholeCapture:=OutputMultipleCaptures[result,0];
   result:=NextCount;
   if Limit>0 then begin
    dec(Limit);
   end;
   StartCodeUnit:=WholeCapture.StartCodeUnit;
   StartCodePoint:=WholeCapture.StartCodePoint;
   if StartCodePoint=WholeCapture.EndCodePoint then begin
    inc(StartCodePoint);
    if InputIsUTF8=brresuISUTF8 then begin
     if (StartCodeUnit>0) and (StartCodeUnit<=InputCodeUnitLen) then begin
      inc(StartCodeUnit,UTF8CharSteps[Input[StartCodeUnit]]);
     end else begin
      break;
     end;
    end else begin
     inc(StartCodeUnit);
    end;
   end else begin
    StartCodePoint:=WholeCapture.EndCodePoint;
    StartCodeUnit:=WholeCapture.EndCodeUnit;
   end;
   continue;
  end;
  break;
 end;
end;

function TBRRERegExp.PtrMatchAll(const Input:pansichar;var OutputMultipleCaptures:TBRRERegExpMultipleCaptures;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=0;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):longint;
var NextCount,TempLastIndexCodePoint,TempLastIndexCodeUnit:longint;
    WholeCapture:TBRRERegExpCapture;
begin
 result:=0;
 TempLastIndexCodePoint:=0;
 TempLastIndexCodeUnit:=0;
 if not assigned(LastIndexCodePoint) then begin
  LastIndexCodePoint:=@TempLastIndexCodePoint;
 end;
 if not assigned(LastIndexCodeUnit) then begin
  LastIndexCodeUnit:=@TempLastIndexCodeUnit;
 end;
 PtrPrepareInput(Input,InputIsUTF8,StartCodePoint,StartCodeUnit,InputCodePointLen,InputCodeUnitLen,StartCodeUnit<0);
 if (InputIsUTF8=brresuISUTF8) and ((LastIndexCodePoint<>@TempLastIndexCodePoint) and (LastIndexCodeUnit=@TempLastIndexCodeUnit)) then begin
  TempLastIndexCodeUnit:=BRREUTF8PtrGetCodeUnit(Input,InputCodePointLen,LastIndexCodePoint^);
 end;
 while (Limit<>0) and (StartCodePoint<=InputCodePointLen) do begin
  NextCount:=result+1;
  if NextCount>=length(OutputMultipleCaptures) then begin
   SetLength(OutputMultipleCaptures,BRRERoundUpToPowerOfTwo(NextCount+1));
  end;
  if PtrMatch(Input,StartCodePoint,StartCodeUnit,OutputMultipleCaptures[result],InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit) then begin
   WholeCapture:=OutputMultipleCaptures[result,0];
   result:=NextCount;
   if Limit>0 then begin
    dec(Limit);
   end;
   StartCodeUnit:=WholeCapture.StartCodeUnit;
   StartCodePoint:=WholeCapture.StartCodePoint;
   if StartCodePoint=WholeCapture.EndCodePoint then begin
    inc(StartCodePoint);
    if InputIsUTF8=brresuISUTF8 then begin
     if (StartCodeUnit>=0) and (StartCodeUnit<InputCodeUnitLen) then begin
      inc(StartCodeUnit,UTF8CharSteps[Input[StartCodeUnit]]);
     end else begin
      break;
     end;
    end else begin
     inc(StartCodeUnit);
    end;
   end else begin
    StartCodePoint:=WholeCapture.EndCodePoint;
    StartCodeUnit:=WholeCapture.EndCodeUnit;
   end;
   continue;
  end;
  break;
 end;
end;

function TBRRERegExp.ExtractAll(const Input:ansistring;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=1;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):TBRRERegExpMultipleStrings;
var Captures:TBRRERegExpCaptures;
    i,Count,TempLastIndexCodePoint,TempLastIndexCodeUnit:longint;
begin
 result:=nil;
 Captures:=nil;
 Count:=0;
 try
  TempLastIndexCodePoint:=0;
  TempLastIndexCodeUnit:=0;
  if not assigned(LastIndexCodePoint) then begin
   LastIndexCodePoint:=@TempLastIndexCodePoint;
  end;
  if not assigned(LastIndexCodeUnit) then begin
   LastIndexCodeUnit:=@TempLastIndexCodeUnit;
  end;
  PrepareInput(Input,InputIsUTF8,StartCodePoint,StartCodeUnit,InputCodePointLen,InputCodeUnitLen,StartCodeUnit<=0);
  if (InputIsUTF8=brresuISUTF8) and ((LastIndexCodePoint<>@TempLastIndexCodePoint) and (LastIndexCodeUnit=@TempLastIndexCodeUnit)) then begin
   TempLastIndexCodeUnit:=BRREUTF8GetCodeUnit(Input,LastIndexCodePoint^);
  end;
  while (Limit<>0) and (StartCodePoint<=InputCodePointLen) do begin
   if Match(Input,StartCodePoint,StartCodeUnit,Captures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit) then begin
    if length(Captures)>0 then begin
     if Count>=length(result) then begin
      SetLength(result,(Count+1)*2);
     end;
     SetLength(result[Count],length(Captures));
     for i:=0 to length(Captures)-1 do begin
      result[Count,i]:=copy(Input,Captures[i].StartCodeUnit,Captures[i].EndCodeUnit-Captures[i].StartCodeUnit);
     end;
     inc(Count);
     if Limit>0 then begin
      dec(Limit);
     end;
     StartCodeUnit:=Captures[0].StartCodeUnit;
     StartCodePoint:=Captures[0].StartCodePoint;
     if StartCodePoint=Captures[0].EndCodePoint then begin
      inc(StartCodePoint);
      if InputIsUTF8=brresuISUTF8 then begin
       if (StartCodeUnit>0) and (StartCodeUnit<=InputCodeUnitLen) then begin
        inc(StartCodeUnit,UTF8CharSteps[Input[StartCodeUnit]]);
       end else begin
        break;
       end;
      end else begin
       inc(StartCodeUnit);
      end;
     end else begin
      StartCodePoint:=Captures[0].EndCodePoint;
      StartCodeUnit:=Captures[0].EndCodeUnit;
     end;
     continue;
    end;
   end;
   break;
  end;
 finally
  SetLength(result,Count);
  SetLength(Captures,0);
 end;
end;

function TBRRERegExp.PtrExtractAll(const Input:pansichar;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=0;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):TBRRERegExpMultipleStrings;
var Captures:TBRRERegExpCaptures;
    i,Count,TempLastIndexCodePoint,TempLastIndexCodeUnit:longint;
begin
 result:=nil;
 Captures:=nil;
 Count:=0;
 try
  TempLastIndexCodePoint:=0;
  TempLastIndexCodeUnit:=0;
  if not assigned(LastIndexCodePoint) then begin
   LastIndexCodePoint:=@TempLastIndexCodePoint;
  end;
  if not assigned(LastIndexCodeUnit) then begin
   LastIndexCodeUnit:=@TempLastIndexCodeUnit;
  end;
  PtrPrepareInput(Input,InputIsUTF8,StartCodePoint,StartCodeUnit,InputCodePointLen,InputCodeUnitLen,StartCodeUnit<0);
  if (InputIsUTF8=brresuISUTF8) and ((LastIndexCodePoint<>@TempLastIndexCodePoint) and (LastIndexCodeUnit=@TempLastIndexCodeUnit)) then begin
   TempLastIndexCodeUnit:=BRREUTF8PtrGetCodeUnit(Input,InputCodeUnitLen,LastIndexCodePoint^);
  end;
  while (Limit<>0) and (StartCodePoint<=InputCodePointLen) do begin
   if PtrMatch(Input,StartCodePoint,StartCodeUnit,Captures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit) then begin
    if length(Captures)>0 then begin
     if Count>=length(result) then begin
      SetLength(result,(Count+1)*2);
     end;
     SetLength(result[Count],length(Captures));
     for i:=0 to length(Captures)-1 do begin
      result[Count,i]:=copy(Input,Captures[i].StartCodeUnit,Captures[i].EndCodeUnit-Captures[i].StartCodeUnit);
     end;
     inc(Count);
     if Limit>0 then begin
      dec(Limit);
     end;
     StartCodeUnit:=Captures[0].StartCodeUnit;
     StartCodePoint:=Captures[0].StartCodePoint;
     if StartCodePoint=Captures[0].EndCodePoint then begin
      inc(StartCodePoint);
      if InputIsUTF8=brresuISUTF8 then begin
       if (StartCodeUnit>=0) and (StartCodeUnit<InputCodeUnitLen) then begin
        inc(StartCodeUnit,UTF8CharSteps[Input[StartCodeUnit]]);
       end else begin
        break;
       end;
      end else begin
       inc(StartCodeUnit);
      end;
     end else begin
      StartCodePoint:=Captures[0].EndCodePoint;
      StartCodeUnit:=Captures[0].EndCodeUnit;
     end;
     continue;
    end;
   end;
   break;
  end;
 finally
  SetLength(result,Count);
  SetLength(Captures,0);
 end;
end;

function TBRRERegExp.Test(const Input:ansistring;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):boolean;
var Captures:TBRRERegExpCaptures;
    TempLastIndexCodePoint,TempLastIndexCodeUnit:longint;
begin
 Captures:=nil;
 try
  TempLastIndexCodePoint:=0;
  TempLastIndexCodeUnit:=0;
  if not assigned(LastIndexCodePoint) then begin
   LastIndexCodePoint:=@TempLastIndexCodePoint;
  end;
  if not assigned(LastIndexCodeUnit) then begin
   LastIndexCodeUnit:=@TempLastIndexCodeUnit;
  end;
  PrepareInput(Input,InputIsUTF8,StartCodePoint,StartCodeUnit,InputCodePointLen,InputCodeUnitLen,StartCodeUnit<=0);
  if (InputIsUTF8=brresuISUTF8) and ((LastIndexCodePoint<>@TempLastIndexCodePoint) and (LastIndexCodeUnit=@TempLastIndexCodeUnit)) then begin
   TempLastIndexCodeUnit:=BRREUTF8GetCodeUnit(Input,LastIndexCodePoint^);
  end;
  result:=Match(Input,StartCodePoint,StartCodeUnit,Captures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit);
 finally
  SetLength(Captures,0);
 end;
end;

function TBRRERegExp.PtrTest(const Input:pansichar;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=0;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):boolean;
var Captures:TBRRERegExpCaptures;
    TempLastIndexCodePoint,TempLastIndexCodeUnit:longint;
begin
 Captures:=nil;
 try
  TempLastIndexCodePoint:=0;
  TempLastIndexCodeUnit:=0;
  if not assigned(LastIndexCodePoint) then begin
   LastIndexCodePoint:=@TempLastIndexCodePoint;
  end;
  if not assigned(LastIndexCodeUnit) then begin
   LastIndexCodeUnit:=@TempLastIndexCodeUnit;
  end;
  PtrPrepareInput(Input,InputIsUTF8,StartCodePoint,StartCodeUnit,InputCodePointLen,InputCodeUnitLen,StartCodeUnit<0);
  if (InputIsUTF8=brresuISUTF8) and ((LastIndexCodePoint<>@TempLastIndexCodePoint) and (LastIndexCodeUnit=@TempLastIndexCodeUnit)) then begin
   TempLastIndexCodeUnit:=BRREUTF8PtrGetCodeUnit(Input,InputCodeUnitLen,LastIndexCodePoint^);
  end;
  result:=PtrMatch(Input,StartCodePoint,StartCodeUnit,Captures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit);
 finally
  SetLength(Captures,0);
 end;
end;

function TBRRERegExp.Find(const Input:ansistring;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):longint;
var Captures:TBRRERegExpCaptures;
    TempLastIndexCodePoint,TempLastIndexCodeUnit:longint;
begin
 result:=-1;
 Captures:=nil;
 try
  TempLastIndexCodePoint:=0;
  TempLastIndexCodeUnit:=0;
  if not assigned(LastIndexCodePoint) then begin
   LastIndexCodePoint:=@TempLastIndexCodePoint;
  end;
  if not assigned(LastIndexCodeUnit) then begin
   LastIndexCodeUnit:=@TempLastIndexCodeUnit;
  end;
  PrepareInput(Input,InputIsUTF8,StartCodePoint,StartCodeUnit,InputCodePointLen,InputCodeUnitLen,StartCodeUnit<=0);
  if (InputIsUTF8=brresuISUTF8) and ((LastIndexCodePoint<>@TempLastIndexCodePoint) and (LastIndexCodeUnit=@TempLastIndexCodeUnit)) then begin
   TempLastIndexCodeUnit:=BRREUTF8GetCodeUnit(Input,LastIndexCodePoint^);
  end;
  if Match(Input,StartCodePoint,StartCodeUnit,Captures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit) then begin
   if length(Captures)>0 then begin
    result:=Captures[0].StartCodePoint;
   end;
  end;
 finally
  SetLength(Captures,0);
 end;
end;

function TBRRERegExp.PtrFind(const Input:pansichar;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=0;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):longint;
var Captures:TBRRERegExpCaptures;
    TempLastIndexCodePoint,TempLastIndexCodeUnit:longint;
begin
 result:=-1;
 Captures:=nil;
 try
  TempLastIndexCodePoint:=0;
  TempLastIndexCodeUnit:=0;
  if not assigned(LastIndexCodePoint) then begin
   LastIndexCodePoint:=@TempLastIndexCodePoint;
  end;
  if not assigned(LastIndexCodeUnit) then begin
   LastIndexCodeUnit:=@TempLastIndexCodeUnit;
  end;
  PtrPrepareInput(Input,InputIsUTF8,StartCodePoint,StartCodeUnit,InputCodePointLen,InputCodeUnitLen,StartCodeUnit<0);
  if (InputIsUTF8=brresuISUTF8) and ((LastIndexCodePoint<>@TempLastIndexCodePoint) and (LastIndexCodeUnit=@TempLastIndexCodeUnit)) then begin
   TempLastIndexCodeUnit:=BRREUTF8GetCodeUnit(Input,LastIndexCodePoint^);
  end;
  if PtrMatch(Input,StartCodePoint,StartCodeUnit,Captures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit) then begin
   if length(Captures)>0 then begin
    result:=Captures[0].StartCodePoint;
   end;
  end;
 finally
  SetLength(Captures,0);
 end;
end;

function TBRRERegExp.Split(const Input:ansistring;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=1;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):TBRRERegExpStrings;
var DoStop:boolean;
    Captures:TBRRERegExpCaptures;
    IndexCodePoint,IndexCodeUnit,i,Count,TempLastIndexCodePoint,TempLastIndexCodeUnit:longint;
begin
 result:=nil;
 Captures:=nil;
 Count:=0;
 try
  TempLastIndexCodePoint:=0;
  TempLastIndexCodeUnit:=0;
  if not assigned(LastIndexCodePoint) then begin
   LastIndexCodePoint:=@TempLastIndexCodePoint;
  end;
  if not assigned(LastIndexCodeUnit) then begin
   LastIndexCodeUnit:=@TempLastIndexCodeUnit;
  end;
  PrepareInput(Input,InputIsUTF8,StartCodePoint,StartCodeUnit,InputCodePointLen,InputCodeUnitLen,StartCodeUnit<=0);
  if (InputIsUTF8=brresuISUTF8) and ((LastIndexCodePoint<>@TempLastIndexCodePoint) and (LastIndexCodeUnit=@TempLastIndexCodeUnit)) then begin
   TempLastIndexCodeUnit:=BRREUTF8GetCodeUnit(Input,LastIndexCodePoint^);
  end;
  if Limit<>0 then begin
   if InputCodeUnitLen=0 then begin
    if not Match(Input,StartCodePoint,StartCodeUnit,Captures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit) then begin
     if Count>=length(result) then begin
      SetLength(result,(Count+1)*2);
     end;
     result[Count]:=Input;
     inc(Count);
    end;
   end else begin
    IndexCodePoint:=StartCodePoint;
    IndexCodeUnit:=StartCodeUnit;
    while (IndexCodePoint<InputCodePointLen) and ((not (assigned(StartProgram) and StartProgram.AnchorBeginText)) or (IndexCodePoint=0)) do begin
     SetLength(Captures,0);
     if Match(Input,IndexCodePoint,IndexCodeUnit,Captures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit) then begin
      if length(Captures)>0 then begin
       if Captures[0].EndCodePoint=StartCodePoint then begin
        inc(IndexCodePoint);
        if InputIsUTF8=brresuISUTF8 then begin
         if (IndexCodeUnit>0) and (IndexCodeUnit<=InputCodeUnitLen) then begin
          inc(IndexCodeUnit,UTF8CharSteps[Input[IndexCodeUnit]]);
         end else begin
          break;
         end;
        end else begin
         inc(IndexCodeUnit);
        end;
        continue;
       end else begin
        if StartCodePoint<longint(Captures[0].StartCodePoint) then begin
         if Count>=length(result) then begin
          SetLength(result,(Count+1)*2);
         end;
         result[Count]:=copy(Input,StartCodeUnit,Captures[0].StartCodeUnit-StartCodeUnit);
         inc(Count);
        end;
        if (Limit>=0) and (length(result)>=Limit) then begin
         break;
        end else begin
         StartCodePoint:=Captures[0].EndCodePoint;
         StartCodeUnit:=Captures[0].EndCodeUnit;
         i:=0;
         DoStop:=false;
         while true do begin
          if i=(length(Captures)-1) then begin
           IndexCodePoint:=StartCodePoint;
           IndexCodeUnit:=StartCodeUnit;
           DoStop:=false;
           break;
          end else begin
           inc(i);
           if Count>=length(result) then begin
            SetLength(result,(Count+1)*2);
           end;
           if Captures[i].EndCodePoint=brreoUNDEFINED then begin
            result[Count]:='';
           end else begin
            result[Count]:=copy(Input,Captures[i].StartCodeUnit,Captures[i].EndCodeUnit-Captures[i].StartCodeUnit);
           end;
           inc(Count);
           if (Limit>=0) and (length(result)>=Limit) then begin
            DoStop:=true;
            break;
           end;
          end;
         end;
         if DoStop then begin
          break;
         end else begin
          continue;
         end;
        end;
       end;
      end;
     end;
     break;
    end;
    if not ((Limit>=0) and (length(result)>=Limit)) then begin
     if StartCodePoint<InputCodePointLen then begin
      if (InputCodePointLen-StartCodePoint)>0 then begin
       if Count>=length(result) then begin
        SetLength(result,(Count+1)*2);
       end;
       result[Count]:=copy(Input,StartCodeUnit,(InputCodeUnitLen-StartCodeUnit)+1);
       inc(Count);
      end;
     end;
    end;
   end;
  end;
 finally
  SetLength(result,Count);
  SetLength(Captures,0);
 end;
end;

function TBRRERegExp.PtrSplit(const Input:pansichar;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=0;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):TBRRERegExpStrings;
var DoStop:boolean;
    Captures:TBRRERegExpCaptures;
    IndexCodePoint,IndexCodeUnit,i,Count,TempLastIndexCodePoint,TempLastIndexCodeUnit:longint;
begin
 result:=nil;
 Captures:=nil;
 Count:=0;
 try
  TempLastIndexCodePoint:=0;
  TempLastIndexCodeUnit:=0;
  if not assigned(LastIndexCodePoint) then begin
   LastIndexCodePoint:=@TempLastIndexCodePoint;
  end;
  if not assigned(LastIndexCodeUnit) then begin
   LastIndexCodeUnit:=@TempLastIndexCodeUnit;
  end;
  PtrPrepareInput(Input,InputIsUTF8,StartCodePoint,StartCodeUnit,InputCodePointLen,InputCodeUnitLen,StartCodeUnit<0);
  if (InputIsUTF8=brresuISUTF8) and ((LastIndexCodePoint<>@TempLastIndexCodePoint) and (LastIndexCodeUnit=@TempLastIndexCodeUnit)) then begin
   TempLastIndexCodeUnit:=BRREUTF8PtrGetCodeUnit(Input,InputCodePointLen,LastIndexCodePoint^);
  end;
  if Limit<>0 then begin
   if InputCodeUnitLen=0 then begin
    if not PtrMatch(Input,StartCodePoint,StartCodeUnit,Captures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit) then begin
     if Count>=length(result) then begin
      SetLength(result,(Count+1)*2);
     end;
     result[Count]:=Input;
     inc(Count);
    end;
   end else begin
    IndexCodePoint:=StartCodePoint;
    IndexCodeUnit:=StartCodeUnit;
    while (IndexCodePoint<InputCodePointLen) and ((not (assigned(StartProgram) and StartProgram.AnchorBeginText)) or (IndexCodePoint=0)) do begin
     SetLength(Captures,0);
     if PtrMatch(Input,IndexCodePoint,IndexCodeUnit,Captures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit) then begin
      if length(Captures)>0 then begin
       if Captures[0].EndCodePoint=StartCodePoint then begin
        inc(IndexCodePoint);
        if InputIsUTF8=brresuISUTF8 then begin
         if (IndexCodeUnit>=0) and (IndexCodeUnit<InputCodeUnitLen) then begin
          inc(IndexCodeUnit,UTF8CharSteps[Input[IndexCodeUnit]]);
         end else begin
          break;
         end;
        end else begin
         inc(IndexCodeUnit);
        end;
        continue;
       end else begin
        if StartCodePoint<longint(Captures[0].StartCodePoint) then begin
         if Count>=length(result) then begin
          SetLength(result,(Count+1)*2);
         end;
         result[Count]:=copy(Input,StartCodeUnit,Captures[0].StartCodeUnit-StartCodeUnit);
         inc(Count);
        end;
        if (Limit>=0) and (length(result)>=Limit) then begin
         break;
        end else begin
         StartCodePoint:=Captures[0].EndCodePoint;
         StartCodeUnit:=Captures[0].EndCodeUnit;
         i:=0;
         DoStop:=false;
         while true do begin
          if i=(length(Captures)-1) then begin
           IndexCodePoint:=StartCodePoint;
           IndexCodeUnit:=StartCodeUnit;
           DoStop:=false;
           break;
          end else begin
           inc(i);
           if Count>=length(result) then begin
            SetLength(result,(Count+1)*2);
           end;
           if Captures[i].EndCodePoint=brreoUNDEFINED then begin
            result[Count]:='';
           end else begin
            result[Count]:=copy(Input,Captures[i].StartCodeUnit,Captures[i].EndCodeUnit-Captures[i].StartCodeUnit);
           end;
           inc(Count);
           if (Limit>=0) and (length(result)>=Limit) then begin
            DoStop:=true;
            break;
           end;
          end;
         end;
         if DoStop then begin
          break;
         end else begin
          continue;
         end;
        end;
       end;
      end;
     end;
     break;
    end;
    if not ((Limit>=0) and (length(result)>=Limit)) then begin
     if StartCodePoint<InputCodePointLen then begin
      if (InputCodePointLen-StartCodePoint)>0 then begin
       if Count>=length(result) then begin
        SetLength(result,(Count+1)*2);
       end;
       result[Count]:=copy(Input,StartCodeUnit,(InputCodeUnitLen-StartCodeUnit)+1);
       inc(Count);
      end;
     end;
    end;
   end;
  end;
 finally
  SetLength(result,Count);
  SetLength(Captures,0);
 end;
end;

function TBRRERegExp.Replace(const Input,Replacement:ansistring;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;ReplacementIsUTF8:longint=-1;ReplacementCodePointLen:longint=-1;ReplacementCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=1;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):ansistring;
var SimpleReplacement:boolean;
    Captures:TBRRERegExpCaptures;
    IndexCodePoint,IndexCodeUnit,i,j,l,e,TempLastIndexCodePoint,TempLastIndexCodeUnit:longint;
    OutputString:ansistring;
    cc,c:ansichar;
begin
 result:='';
 Captures:=nil;
 try
  TempLastIndexCodePoint:=0;
  TempLastIndexCodeUnit:=0;
  if not assigned(LastIndexCodePoint) then begin
   LastIndexCodePoint:=@TempLastIndexCodePoint;
  end;
  if not assigned(LastIndexCodeUnit) then begin
   LastIndexCodeUnit:=@TempLastIndexCodeUnit;
  end;
  PrepareInput(Input,InputIsUTF8,StartCodePoint,StartCodeUnit,InputCodePointLen,InputCodeUnitLen,StartCodeUnit<=0);
  if (InputIsUTF8=brresuISUTF8) and ((LastIndexCodePoint<>@TempLastIndexCodePoint) and (LastIndexCodeUnit=@TempLastIndexCodeUnit)) then begin
   TempLastIndexCodeUnit:=BRREUTF8GetCodeUnit(Input,LastIndexCodePoint^);
  end;
  if ReplacementIsUTF8<0 then begin
   if (Flags and brrefAUTO)<>0 then begin
    if (Flags and brrefUTF8CODEUNITS)<>0 then begin
     ReplacementIsUTF8:=brresuNOUTF8;
    end else begin
     ReplacementIsUTF8:=BRREGetUTF8(Replacement);
    end;
   end else if (Flags and (brrefUTF8 or brrefUTF8CODEUNITS))=brrefUTF8 then begin
    ReplacementIsUTF8:=brresuISUTF8;
   end else {if (Flags and brrefLATIN1)<>0 then} begin
    ReplacementIsUTF8:=brresuNOUTF8;
   end;
  end;
  if ReplacementCodeUnitLen<0 then begin
   ReplacementCodeUnitLen:=length(Replacement);
  end;
  if InputCodeUnitLen>0 then begin
   SimpleReplacement:=(pos('$',String(Replacement))=0) and (pos('\',String(Replacement))=0);
   OutputString:='';
   IndexCodePoint:=StartCodePoint;
   IndexCodeUnit:=StartCodeUnit;
   while (Limit<>0) and (IndexCodePoint<InputCodePointLen) and ((not (assigned(StartProgram) and StartProgram.AnchorBeginText)) or (IndexCodePoint=0)) do begin
    SetLength(Captures,0);
    if Match(Input,IndexCodePoint,IndexCodeUnit,Captures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit) then begin
     if length(Captures)>0 then begin
      if Captures[0].EndCodePoint=StartCodePoint then begin
       inc(IndexCodePoint);
       if InputIsUTF8=brresuISUTF8 then begin
        if (IndexCodeUnit>0) and (IndexCodeUnit<=InputCodeUnitLen) then begin
         inc(IndexCodeUnit,UTF8CharSteps[Input[IndexCodeUnit]]);
        end else begin
         break;
        end;
       end else begin
        inc(IndexCodeUnit);
       end;
      end else begin
       if StartCodePoint<Captures[0].StartCodePoint then begin
        OutputString:=OutputString+copy(Input,StartCodeUnit,Captures[0].StartCodeUnit-StartCodeUnit);
       end;
       StartCodePoint:=Captures[0].EndCodePoint;
       StartCodeUnit:=Captures[0].EndCodeUnit;
       IndexCodePoint:=Captures[0].EndCodePoint;
       IndexCodeUnit:=Captures[0].EndCodeUnit;
       if SimpleReplacement then begin
        OutputString:=OutputString+Replacement;
       end else begin
        i:=1;
        while i<=ReplacementCodeUnitLen do begin
         c:=Replacement[i];
         case c of
          '$','\':begin
           cc:=c;
           inc(i);
           if i<=ReplacementCodeUnitLen then begin
            c:=Replacement[i];
            case c of
             '$':begin
              if cc='$' then begin
               OutputString:=OutputString+c;
               inc(i);
              end else begin
               OutputString:=OutputString+'\$';
               inc(i);
              end;
             end;
             '\':begin
              if cc='\' then begin
               OutputString:=OutputString+c;
               inc(i);
              end else begin
               OutputString:=OutputString+'$\';
               inc(i);
              end;
             end;
             '&':begin
              OutputString:=OutputString+copy(Input,Captures[0].StartCodeUnit,Captures[0].EndCodeUnit-Captures[0].StartCodeUnit);
              inc(i);
             end;
             '`':begin
              OutputString:=OutputString+copy(Input,1,Captures[0].StartCodeUnit-1);
              inc(i);
             end;
             '''':begin
              OutputString:=OutputString+copy(Input,Captures[0].EndCodeUnit,(InputCodeUnitLen-Captures[0].EndCodeUnit)+1);
              inc(i);
             end;
             '_':begin
              OutputString:=OutputString+Input;
              inc(i);
             end;
             '-':begin
              if length(Captures)>1 then begin
               OutputString:=OutputString+copy(Input,Captures[1].StartCodeUnit,Captures[1].EndCodeUnit-Captures[1].StartCodeUnit);
              end;
              inc(i);
             end;
             '+':begin
              if length(Captures)>1 then begin
               e:=length(Captures)-1;
               OutputString:=OutputString+copy(Input,Captures[e].StartCodeUnit,Captures[e].EndCodeUnit-Captures[e].StartCodeUnit);
              end;
              inc(i);
             end;
             'g':begin
              if cc='\' then begin
               e:=-1;
               inc(i);
               j:=i;
               while i<=ReplacementCodeUnitLen do begin
                if ReplacementIsUTF8=brresuISUTF8 then begin
                 l:=i;
                 if not BRREUnicodeIsIDPart(BRREUTF8CodeUnitGetCharAndInc(Replacement,i)) then begin
                  i:=l;
                  break;
                 end;
                end else begin
                 if Replacement[i] in ['a'..'z','A'..'Z','_','0'..'9'] then begin
                  inc(i);
                 end else begin
                  break;
                 end;
                end;
               end;
               if j<i then begin
                e:=CaptureIdentifierHashMap.GetValue(copy(Replacement,j,i-j));
               end;
               if e<0 then begin
                OutputString:=OutputString+cc+'g';
                i:=j;
               end else begin
                if (e>=0) and (e<length(Captures)) then begin
                 OutputString:=OutputString+copy(Input,Captures[e].StartCodeUnit,Captures[e].EndCodeUnit-Captures[e].StartCodeUnit);
                end;
               end;
              end else begin
               OutputString:=OutputString+cc+'g';
               inc(i);
              end;
             end;
             '{':begin
              e:=-1;
              inc(i);
              j:=i;
              if i<=ReplacementCodeUnitLen then begin
               case Replacement[i] of
                '0'..'9':begin
                 e:=0;
                 while i<=ReplacementCodeUnitLen do begin
                  c:=Replacement[i];
                  case c of
                   '0'..'9':begin
                    e:=(e*10)+(ord(c)-ord('0'));
                    inc(i);
                   end;
                   else begin
                    break;
                   end;
                  end;
                 end;
                 if (i<=ReplacementCodeUnitLen) and (Replacement[i]='}') then begin
                  inc(i);
                 end else begin
                  e:=-1;
                 end;
                end;
                else begin
                 while i<=ReplacementCodeUnitLen do begin
                  if ReplacementIsUTF8=brresuISUTF8 then begin
                   l:=i;
                   if not BRREUnicodeIsIDPart(BRREUTF8CodeUnitGetCharAndInc(Replacement,i)) then begin
                    i:=l;
                    break;
                   end;
                  end else begin
                   if Replacement[i] in ['a'..'z','A'..'Z','_','0'..'9'] then begin
                    inc(i);
                   end else begin
                    break;
                   end;
                  end;
                 end;
                 if (j<i) and (Replacement[i]='}') then begin
                  e:=CaptureIdentifierHashMap.GetValue(copy(Replacement,j,i-j));
                  inc(i);
                 end else begin
                  e:=-1;
                 end;
                end;
               end;
              end;
              if e<0 then begin
               OutputString:=OutputString+cc+'{';
               i:=j;
              end else begin
               if (e>=0) and (e<length(Captures)) then begin
                OutputString:=OutputString+copy(Input,Captures[e].StartCodeUnit,Captures[e].EndCodeUnit-Captures[e].StartCodeUnit);
               end;
              end;
             end;
             '0'..'9':begin
              if length(Captures)<10 then begin
               e:=ord(c)-ord('0');
               inc(i);
              end else begin
               e:=0;
               while i<=ReplacementCodeUnitLen do begin
                c:=Replacement[i];
                case c of
                 '0'..'9':begin
                  e:=(e*10)+(ord(c)-ord('0'));
                  inc(i);
                 end;
                 else begin
                  break;
                 end;
                end;
               end;
              end;
              if (e>=0) and (e<length(Captures)) then begin
               OutputString:=OutputString+copy(Input,Captures[e].StartCodeUnit,Captures[e].EndCodeUnit-Captures[e].StartCodeUnit);
              end;
             end;
             else begin
              OutputString:=OutputString+c;
              inc(i);
             end;
            end;
           end;
          end;
          else begin
           OutputString:=OutputString+c;
           inc(i);
          end;
         end;
        end;
       end;
       if Limit>0 then begin
        dec(Limit);
       end;
      end;
      continue;
     end;
    end;
    break;
   end;
   if StartCodePoint<InputCodePointLen then begin
    OutputString:=OutputString+copy(Input,StartCodeUnit,(InputCodeUnitLen-StartCodeUnit)+1);
   end;
  end;
  result:=OutputString;
 finally
  SetLength(Captures,0);
  OutputString:='';
 end;
end;

function TBRRERegExp.PtrReplace(const Input,Replacement:pansichar;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;ReplacementIsUTF8:longint=-1;ReplacementCodePointLen:longint=-1;ReplacementCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=0;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):ansistring;
var SimpleReplacement:boolean;
    Captures:TBRRERegExpCaptures;
    IndexCodePoint,IndexCodeUnit,i,j,l,e,TempLastIndexCodePoint,TempLastIndexCodeUnit:longint;
    OutputString:ansistring;
    cc,c:ansichar;
begin
 result:='';
 Captures:=nil;
 try
  TempLastIndexCodePoint:=0;
  TempLastIndexCodeUnit:=0;
  if not assigned(LastIndexCodePoint) then begin
   LastIndexCodePoint:=@TempLastIndexCodePoint;
  end;
  if not assigned(LastIndexCodeUnit) then begin
   LastIndexCodeUnit:=@TempLastIndexCodeUnit;
  end;
  PtrPrepareInput(Input,InputIsUTF8,StartCodePoint,StartCodeUnit,InputCodePointLen,InputCodeUnitLen,StartCodeUnit<0);
  if (InputIsUTF8=brresuISUTF8) and ((LastIndexCodePoint<>@TempLastIndexCodePoint) and (LastIndexCodeUnit=@TempLastIndexCodeUnit)) then begin
   TempLastIndexCodeUnit:=BRREUTF8PtrGetCodeUnit(Input,InputCodePointLen,LastIndexCodePoint^);
  end;
  if ReplacementCodeUnitLen<0 then begin
   ReplacementCodeUnitLen:=PtrLength(Replacement);
  end;
  if ReplacementIsUTF8<0 then begin
   if (Flags and brrefAUTO)<>0 then begin
    if (Flags and brrefUTF8CODEUNITS)<>0 then begin
     ReplacementIsUTF8:=brresuNOUTF8;
    end else begin
     ReplacementIsUTF8:=BRREPtrGetUTF8(Replacement,ReplacementCodeUnitLen);
    end;
   end else if (Flags and (brrefUTF8 or brrefUTF8CODEUNITS))=brrefUTF8 then begin
    ReplacementIsUTF8:=brresuISUTF8;
   end else {if (Flags and brrefLATIN1)<>0 then} begin
    ReplacementIsUTF8:=brresuNOUTF8;
   end;
  end;
  if InputCodeUnitLen>0 then begin
   SimpleReplacement:=(pos('$',String(Replacement))=0) and (pos('\',String(Replacement))=0);
   OutputString:='';
   IndexCodePoint:=StartCodePoint;
   IndexCodeUnit:=StartCodeUnit;
   while (Limit<>0) and (IndexCodePoint<InputCodePointLen) and ((not (assigned(StartProgram) and StartProgram.AnchorBeginText)) or (IndexCodePoint=0)) do begin
    SetLength(Captures,0);
    if PtrMatch(Input,IndexCodePoint,IndexCodeUnit,Captures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit) then begin
     if length(Captures)>0 then begin
      if Captures[0].EndCodePoint=StartCodePoint then begin
       inc(IndexCodePoint);
       if InputIsUTF8=brresuISUTF8 then begin
        if (IndexCodeUnit>=0) and (IndexCodeUnit<InputCodeUnitLen) then begin
         inc(IndexCodeUnit,UTF8CharSteps[Input[IndexCodeUnit]]);
        end else begin
         break;
        end;
       end else begin
        inc(IndexCodeUnit);
       end;
      end else begin
       if StartCodePoint<Captures[0].StartCodePoint then begin
        OutputString:=OutputString+copy(Input,StartCodeUnit,Captures[0].StartCodeUnit-StartCodeUnit);
       end;
       StartCodePoint:=Captures[0].EndCodePoint;
       StartCodeUnit:=Captures[0].EndCodeUnit;
       IndexCodePoint:=Captures[0].EndCodePoint;
       IndexCodeUnit:=Captures[0].EndCodeUnit;
       if SimpleReplacement then begin
        OutputString:=OutputString+Replacement;
       end else begin
        i:=0;
        while i<ReplacementCodeUnitLen do begin
         c:=Replacement[i];
         case c of
          '$','\':begin
           cc:=c;
           inc(i);
           if i<ReplacementCodeUnitLen then begin
            c:=Replacement[i];
            case c of
             '$':begin
              if cc='$' then begin
               OutputString:=OutputString+c;
               inc(i);
              end else begin
               OutputString:=OutputString+'\$';
               inc(i);
              end;
             end;
             '\':begin
              if cc='\' then begin
               OutputString:=OutputString+c;
               inc(i);
              end else begin
               OutputString:=OutputString+'$\';
               inc(i);
              end;
             end;
             '&':begin
              OutputString:=OutputString+copy(Input,Captures[0].StartCodeUnit,Captures[0].EndCodeUnit-Captures[0].StartCodeUnit);
              inc(i);
             end;
             '`':begin
              OutputString:=OutputString+copy(Input,1,Captures[0].StartCodeUnit-1);
              inc(i);
             end;
             '''':begin
              OutputString:=OutputString+copy(Input,Captures[0].EndCodeUnit,(InputCodeUnitLen-Captures[0].EndCodeUnit)+1);
              inc(i);
             end;
             '_':begin
              OutputString:=OutputString+Input;
              inc(i);
             end;
             '-':begin
              if length(Captures)>1 then begin
               OutputString:=OutputString+copy(Input,Captures[1].StartCodeUnit,Captures[1].EndCodeUnit-Captures[1].StartCodeUnit);
              end;
              inc(i);
             end;
             '+':begin
              if length(Captures)>1 then begin
               e:=length(Captures)-1;
               OutputString:=OutputString+copy(Input,Captures[e].StartCodeUnit,Captures[e].EndCodeUnit-Captures[e].StartCodeUnit);
              end;
              inc(i);
             end;
             'g':begin
              if cc='\' then begin
               e:=-1;
               inc(i);
               j:=i;
               while i<ReplacementCodeUnitLen do begin
                if ReplacementIsUTF8=brresuISUTF8 then begin
                 l:=i;
                 if not BRREUnicodeIsIDPart(BRREUTF8CodeUnitGetCharAndInc(Replacement,i)) then begin
                  i:=l;
                  break;
                 end;
                end else begin
                 if Replacement[i] in ['a'..'z','A'..'Z','_','0'..'9'] then begin
                  inc(i);
                 end else begin
                  break;
                 end;
                end;
               end;
               if j<i then begin
                e:=CaptureIdentifierHashMap.GetValue(copy(Replacement,j,i-j));
               end;
               if e<0 then begin
                OutputString:=OutputString+cc+'g';
                i:=j;
               end else begin
                if (e>=0) and (e<length(Captures)) then begin
                 OutputString:=OutputString+copy(Input,Captures[e].StartCodeUnit,Captures[e].EndCodeUnit-Captures[e].StartCodeUnit);
                end;
               end;
              end else begin
               OutputString:=OutputString+cc+'g';
               inc(i);
              end;
             end;
             '{':begin
              e:=-1;
              inc(i);
              j:=i;
              if i<ReplacementCodeUnitLen then begin
               case Replacement[i] of
                '0'..'9':begin
                 e:=0;
                 while i<ReplacementCodeUnitLen do begin
                  c:=Replacement[i];
                  case c of
                   '0'..'9':begin
                    e:=(e*10)+(ord(c)-ord('0'));
                    inc(i);
                   end;
                   else begin
                    break;
                   end;
                  end;
                 end;
                 if (i<ReplacementCodeUnitLen) and (Replacement[i]='}') then begin
                  inc(i);
                 end else begin
                  e:=-1;
                 end;
                end;
                else begin
                 while i<ReplacementCodeUnitLen do begin
                  if ReplacementIsUTF8=brresuISUTF8 then begin
                   l:=i;
                   if not BRREUnicodeIsIDPart(BRREUTF8CodeUnitGetCharAndInc(Replacement,i)) then begin
                    i:=l;
                    break;
                   end;
                  end else begin
                   if Replacement[i] in ['a'..'z','A'..'Z','_','0'..'9'] then begin
                    inc(i);
                   end else begin
                    break;
                   end;
                  end;
                 end;
                 if (j<i) and (Replacement[i]='}') then begin
                  e:=CaptureIdentifierHashMap.GetValue(copy(Replacement,j,i-j));
                  inc(i);
                 end else begin
                  e:=-1;
                 end;
                end;
               end;
              end;
              if e<0 then begin
               OutputString:=OutputString+cc+'{';
               i:=j;
              end else begin
               if (e>=0) and (e<length(Captures)) then begin
                OutputString:=OutputString+copy(Input,Captures[e].StartCodeUnit,Captures[e].EndCodeUnit-Captures[e].StartCodeUnit);
               end;
              end;
             end;
             '0'..'9':begin
              if length(Captures)<10 then begin
               e:=ord(c)-ord('0');
               inc(i);
              end else begin
               e:=0;
               while i<ReplacementCodeUnitLen do begin
                c:=Replacement[i];
                case c of
                 '0'..'9':begin
                  e:=(e*10)+(ord(c)-ord('0'));
                  inc(i);
                 end;
                 else begin
                  break;
                 end;
                end;
               end;
              end;
              if (e>=0) and (e<length(Captures)) then begin
               OutputString:=OutputString+copy(Input,Captures[e].StartCodeUnit,Captures[e].EndCodeUnit-Captures[e].StartCodeUnit);
              end;
             end;
             else begin
              OutputString:=OutputString+c;
              inc(i);
             end;
            end;
           end;
          end;
          else begin
           OutputString:=OutputString+c;
           inc(i);
          end;
         end;
        end;
       end;
       if Limit>0 then begin
        dec(Limit);
       end;
      end;
      continue;
     end;
    end;
    break;
   end;
   if StartCodePoint<InputCodePointLen then begin
    OutputString:=OutputString+copy(Input,StartCodeUnit,(InputCodeUnitLen-StartCodeUnit)+1);
   end;
  end;
  result:=OutputString;
 finally
  SetLength(Captures,0);
  OutputString:='';
 end;
end;

function TBRRERegExp.Benchmark(const Input:ansistring;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=1;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):longint;
var Captures:TBRRERegExpCaptures;
    TempLastIndexCodePoint,TempLastIndexCodeUnit:longint;
begin
 result:=0;
 Captures:=nil;
 try
  TempLastIndexCodePoint:=0;
  TempLastIndexCodeUnit:=0;
  if not assigned(LastIndexCodePoint) then begin
   LastIndexCodePoint:=@TempLastIndexCodePoint;
  end;
  if not assigned(LastIndexCodeUnit) then begin
   LastIndexCodeUnit:=@TempLastIndexCodeUnit;
  end;
  PrepareInput(Input,InputIsUTF8,StartCodePoint,StartCodeUnit,InputCodePointLen,InputCodeUnitLen,StartCodeUnit<=0);
  if (InputIsUTF8=brresuISUTF8) and ((LastIndexCodePoint<>@TempLastIndexCodePoint) and (LastIndexCodeUnit=@TempLastIndexCodeUnit)) then begin
   TempLastIndexCodeUnit:=BRREUTF8GetCodeUnit(Input,LastIndexCodePoint^);
  end;
  while (Limit<>0) and (StartCodePoint<=InputCodePointLen) and ((not (assigned(StartProgram) and StartProgram.AnchorBeginText)) or (StartCodePoint=0)) do begin
   if Match(Input,StartCodePoint,StartCodeUnit,Captures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit) then begin
    if length(Captures)>0 then begin
     inc(result);
     if Limit>0 then begin
      dec(Limit);
     end;
     StartCodeUnit:=Captures[0].StartCodeUnit;
     StartCodePoint:=Captures[0].StartCodePoint;
     if StartCodePoint=Captures[0].EndCodePoint then begin
      if InputIsUTF8=brresuISUTF8 then begin
       if (StartCodeUnit>0) and (StartCodeUnit<=length(Input)) then begin
        inc(StartCodeUnit,UTF8CharSteps[Input[StartCodeUnit]]);
       end else begin
        break;
       end;
      end else begin
       inc(StartCodeUnit);
      end;
     end else begin
      StartCodePoint:=Captures[0].EndCodePoint;
      StartCodeUnit:=Captures[0].EndCodeUnit;
     end;
     continue;
    end;
   end;
   break;
  end;
 finally
  SetLength(Captures,0);
 end;
end;

function TBRRERegExp.PtrBenchmark(const Input:pansichar;InputIsUTF8:longint=-1;InputCodePointLen:longint=-1;InputCodeUnitLen:longint=-1;StartCodePoint:longint=0;StartCodeUnit:longint=0;Limit:longint=-1;LastIndexCodePoint:plongint=nil;LastIndexCodeUnit:plongint=nil):longint;
var Captures:TBRRERegExpCaptures;
    TempLastIndexCodePoint,TempLastIndexCodeUnit:longint;
begin
 result:=0;
 Captures:=nil;
 try
  TempLastIndexCodePoint:=0;
  TempLastIndexCodeUnit:=0;
  if not assigned(LastIndexCodePoint) then begin
   LastIndexCodePoint:=@TempLastIndexCodePoint;
  end;
  if not assigned(LastIndexCodeUnit) then begin
   LastIndexCodeUnit:=@TempLastIndexCodeUnit;
  end;
  PtrPrepareInput(Input,InputIsUTF8,StartCodePoint,StartCodeUnit,InputCodePointLen,InputCodeUnitLen,StartCodeUnit<0);
  if (InputIsUTF8=brresuISUTF8) and ((LastIndexCodePoint<>@TempLastIndexCodePoint) and (LastIndexCodeUnit=@TempLastIndexCodeUnit)) then begin
   TempLastIndexCodeUnit:=BRREUTF8PtrGetCodeUnit(Input,InputCodeUnitLen,LastIndexCodePoint^);
  end;
  while (Limit<>0) and (StartCodePoint<=InputCodePointLen) and ((not (assigned(StartProgram) and StartProgram.AnchorBeginText)) or (StartCodePoint=0)) do begin
   if PtrMatch(Input,StartCodePoint,StartCodeUnit,Captures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit) then begin
    if length(Captures)>0 then begin
     inc(result);
     if Limit>0 then begin
      dec(Limit);
     end;
     StartCodeUnit:=Captures[0].StartCodeUnit;
     StartCodePoint:=Captures[0].StartCodePoint;
     if StartCodePoint=Captures[0].EndCodePoint then begin
      if InputIsUTF8=brresuISUTF8 then begin
       if (StartCodeUnit>=0) and (StartCodeUnit<InputCodeUnitLen) then begin
        inc(StartCodeUnit,UTF8CharSteps[Input[StartCodeUnit]]);
       end else begin
        break;
       end;
      end else begin
       inc(StartCodeUnit);
      end;
     end else begin
      StartCodePoint:=Captures[0].EndCodePoint;
      StartCodeUnit:=Captures[0].EndCodeUnit;
     end;
     continue;
    end;
   end;
   break;
  end;
 finally
  SetLength(Captures,0);
 end;
end;

function BRREEscape(s:ansistring):ansistring;
var i:longint;
    c:ansichar;
begin
 result:='';
 for i:=1 to length(s) do begin
  c:=s[i];
  if c in ['(','|','.','*','?','^','$','-','[','{','}',']',')','\'] then begin
   result:=result+'\';
  end;
  result:=result+c;
 end;
end;

function BRREGetVersion:int64; cdecl;
begin
 result:=BRREVersion;
end;

function BRREGetVersionString:pansichar; cdecl;
begin
 result:=pansichar(@BRREVersionString[1]);
end;

function BRRECreate(RegExp:pansichar;RegExpLen:longint;Flags:longword;Error:ppansichar):TBRRERegExp; cdecl;
var s:ansistring;
    Len:longint;
begin
 result:=nil;
 if assigned(Error) and assigned(Error^) then begin
  FreeMem(Error^);
  Error^:=nil;
 end;
 try
  if RegExpLen<0 then begin
   result:=TBRRERegExp.Create(AnsiString(RegExp),Flags);
  end else begin
   result:=TBRRERegExp.Create(copy(AnsiString(RegExp)+AnsiString(''),1,RegExpLen),Flags);
  end;
 except
  on e:Exception do begin
   if assigned(Error) then begin
    s:=AnsiString(e.Message);
    Len:=length(s);
    if Len>0 then begin
     GetMem(Error^,Len+1);
     FillChar(Error^^,Len+1,ansichar(#0));
     Move(s[1],Error^^,Len);
    end else begin
     Error^:=nil;
    end;
   end;
   result:=nil;
  end;
 end;
end;

function BRRESetMaxMatchLimit(Instance:TBRRERegExp;MaxMatchLimit:longint):longint; cdecl;
begin
 if assigned(Instance) then begin
  Instance.MaxMatchLimit:=MaxMatchLimit;
  result:=1;
 end else begin
  result:=0;
 end;
end;

function BRRESetMaxMatchLimitRecursion(Instance:TBRRERegExp;MaxMatchLimitRecursion:longint):longint; cdecl;
begin
 if assigned(Instance) then begin
  Instance.MaxMatchLimitRecursion:=MaxMatchLimitRecursion;
  result:=1;
 end else begin
  result:=0;
 end;
end;

function BRRESetMaxMatchLimitStackRecursion(Instance:TBRRERegExp;MaxMatchLimitStackRecursion:longint):longint; cdecl;
begin
 if assigned(Instance) then begin
  Instance.MaxMatchLimitStackRecursion:=MaxMatchLimitStackRecursion;
  result:=1;
 end else begin
  result:=0;
 end;
end;

function BRRESetCalloutFunc(Instance:TBRRERegExp;CalloutFunc:pointer):longint; cdecl;
begin
 if assigned(Instance) then begin
  Instance.CalloutFunc:=CalloutFunc;
  result:=1;
 end else begin
  result:=0;
 end;
end;

function BRRESetCalloutData(Instance:TBRRERegExp;CalloutData:pointer):longint; cdecl;
begin
 if assigned(Instance) then begin
  Instance.CalloutData:=CalloutData;
  result:=1;
 end else begin
  result:=0;
 end;
end;

function BRREGetCountCaptures(Instance:TBRRERegExp):longint; cdecl;
begin
 if assigned(Instance) then begin
  result:=Instance.CountOfCaptures;
 end else begin
  result:=0;
 end;
end;

function BRREGetNamedGroupIndex(Instance:TBRRERegExp;GroupName:pansichar):longint; cdecl;
begin
 if assigned(Instance) then begin
  result:=Instance.CaptureIdentifierHashMap.GetValue(GroupName);
 end else begin
  result:=-1;
 end;
end;

function BRREPrefilterExpression(Instance:TBRRERegExp;Expression,Error:ppansichar):longint; cdecl;
var s:ansistring;
    Len:longint;
begin
 result:=0;
 if assigned(Expression) and assigned(Expression^) then begin
  FreeMem(Expression^);
  Expression^:=nil;
 end;
 if assigned(Instance) then begin
  try
   if assigned(Expression) then begin
    s:=Instance.PrefilterExpression;
    Len:=length(s);
    if Len>0 then begin
     GetMem(Expression^,Len+1);
     FillChar(Expression^^,Len+1,ansichar(#0));
     Move(s[1],Expression^^,Len);
    end else begin
     Expression^:=nil;
    end;
    s:='';
   end;
   result:=1;
  except
   on e:Exception do begin
    if assigned(Error) then begin
     s:=AnsiString(e.Message);
     Len:=length(s);
     if Len>0 then begin
      GetMem(Error^,Len+1);
      FillChar(Error^^,Len+1,ansichar(#0));
      Move(s[1],Error^^,Len);
     end else begin
      Error^:=nil;
     end;
     s:='';
    end;
    result:=0;
   end;
  end;
 end;
end;

function BRREPrefilterShortExpression(Instance:TBRRERegExp;ShortExpression,Error:ppansichar):longint; cdecl;
var s:ansistring;
    Len:longint;
begin
 result:=0;
 if assigned(ShortExpression) and assigned(ShortExpression^) then begin
  FreeMem(ShortExpression^);
  ShortExpression^:=nil;
 end;
 if assigned(Instance) then begin
  try
   if assigned(ShortExpression) then begin
    s:=Instance.PrefilterShortExpression;
    Len:=length(s);
    if Len>0 then begin
     GetMem(ShortExpression^,Len+1);
     FillChar(ShortExpression^^,Len+1,ansichar(#0));
     Move(s[1],ShortExpression^^,Len);
    end else begin
     ShortExpression^:=nil;
    end;
    s:='';
   end;
   result:=1;
  except
   on e:Exception do begin
    if assigned(Error) then begin
     s:=AnsiString(e.Message);
     Len:=length(s);
     if Len>0 then begin
      GetMem(Error^,Len+1);
      FillChar(Error^^,Len+1,ansichar(#0));
      Move(s[1],Error^^,Len);
     end else begin
      Error^:=nil;
     end;
     s:='';
    end;
    result:=0;
   end;
  end;
 end;
end;

function BRREPrefilterSQLBooleanFullTextExpression(Instance:TBRRERegExp;SQLBooleanFullTextExpression,Error:ppansichar):longint; cdecl;
var s:ansistring;
    Len:longint;
begin
 result:=0;
 if assigned(SQLBooleanFullTextExpression) and assigned(SQLBooleanFullTextExpression^) then begin
  FreeMem(SQLBooleanFullTextExpression^);
  SQLBooleanFullTextExpression^:=nil;
 end;
 if assigned(Instance) then begin
  try
   if assigned(SQLBooleanFullTextExpression) then begin
    s:=Instance.PrefilterSQLBooleanFullTextExpression;
    Len:=length(s);
    if Len>0 then begin
     GetMem(SQLBooleanFullTextExpression^,Len+1);
     FillChar(SQLBooleanFullTextExpression^^,Len+1,ansichar(#0));
     Move(s[1],SQLBooleanFullTextExpression^^,Len);
    end else begin
     SQLBooleanFullTextExpression^:=nil;
    end;
    s:='';
   end;
   result:=1;
  except
   on e:Exception do begin
    if assigned(Error) then begin
     s:=AnsiString(e.Message);
     Len:=length(s);
     if Len>0 then begin
      GetMem(Error^,Len+1);
      FillChar(Error^^,Len+1,ansichar(#0));
      Move(s[1],Error^^,Len);
     end else begin
      Error^:=nil;
     end;
     s:='';
    end;
    result:=0;
   end;
  end;
 end;
end;

function BRREPrefilterSQLExpression(Instance:TBRRERegExp;Field:pansichar;SQLExpression,Error:ppansichar):longint; cdecl;
var s:ansistring;
    Len:longint;
begin
 result:=0;
 if assigned(SQLExpression) and assigned(SQLExpression^) then begin
  FreeMem(SQLExpression^);
  SQLExpression^:=nil;
 end;
 if assigned(Instance) then begin
  try
   if assigned(SQLExpression) then begin
    s:=Instance.PrefilterSQLExpression(Field);
    Len:=length(s);
    if Len>0 then begin
     GetMem(SQLExpression^,Len+1);
     FillChar(SQLExpression^^,Len+1,ansichar(#0));
     Move(s[1],SQLExpression^^,Len);
    end else begin
     SQLExpression^:=nil;
    end;
    s:='';
   end;
   result:=1;
  except
   on e:Exception do begin
    if assigned(Error) then begin
     s:=AnsiString(e.Message);
     Len:=length(s);
     if Len>0 then begin
      GetMem(Error^,Len+1);
      FillChar(Error^^,Len+1,ansichar(#0));
      Move(s[1],Error^^,Len);
     end else begin
      Error^:=nil;
     end;
     s:='';
    end;
    result:=0;
   end;
  end;
 end;
end;

function BRREGetRange(Instance:TBRRERegExp;RangeLow,RangeHigh,Error:ppansichar):longint; cdecl;
var s,RangeLowString,RangeHighString:ansistring;
    Len:longint;
begin
 result:=0;
 RangeLowString:='';
 RangeHighString:='';
 if assigned(RangeLow) and assigned(RangeLow^) then begin
  FreeMem(RangeLow^);
  RangeLow^:=nil;
 end;
 if assigned(RangeHigh) and assigned(RangeHigh^) then begin
  FreeMem(RangeHigh^);
  RangeHigh^:=nil;
 end;
 if assigned(Instance) then begin
  try
   result:=Instance.GetRange(RangeLowString,RangeHighString);
   if assigned(RangeLow) then begin
    Len:=length(RangeLowString);
    if Len>0 then begin
     GetMem(RangeLow^,Len+1);
     FillChar(RangeLow^^,Len+1,ansichar(#0));
     Move(RangeLowString[1],RangeLow^^,Len);
    end else begin
     RangeLow^:=nil;
    end;
    RangeLowString:='';
   end;
   if assigned(RangeHigh) then begin
    Len:=length(RangeHighString);
    if Len>0 then begin
     GetMem(RangeHigh^,Len+1);
     FillChar(RangeHigh^^,Len+1,ansichar(#0));
     Move(RangeHighString[1],RangeHigh^^,Len);
    end else begin
     RangeHigh^:=nil;
    end;
    RangeHighString:='';
   end;
  except
   on e:Exception do begin
    if assigned(Error) then begin
     s:=AnsiString(e.Message);
     Len:=length(s);
     if Len>0 then begin
      GetMem(Error^,Len+1);
      FillChar(Error^^,Len+1,ansichar(#0));
      Move(s[1],Error^^,Len);
     end else begin
      Error^:=nil;
     end;
     s:='';
    end;
    result:=0;
   end;
  end;
 end;
 RangeLowString:='';
 RangeHighString:='';
end;

function BRREMatch(Instance:TBRRERegExp;Input:pansichar;InputIsUTF8,InputCodePointLen,InputCodeUnitLen:longint;Captures:ppointer;MaxCaptures,StartCodePoint,StartCodeUnit:longint;LastIndexCodePoint,LastIndexCodeUnit:pointer;Error:ppansichar):longint; cdecl;
var i,Len:longint;
    OutputCaptures:TBRRERegExpCaptures;
    p:plongint;
    s:ansistring;
begin
 result:=0;
 if assigned(Error) and assigned(Error^) then begin
  FreeMem(Error^);
  Error^:=nil;
 end;
 if assigned(Instance) then begin
  OutputCaptures:=nil;
  try
   try
    if Instance.PtrMatch(Input,StartCodePoint,StartCodeUnit,OutputCaptures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit) then begin
     result:=length(OutputCaptures);
     if (MaxCaptures<>0) and assigned(Captures) then begin
      if MaxCaptures<0 then begin
       GetMem(Captures^,result*(sizeof(longint)*4));
       pointer(Captures):=pointer(Captures^);
      end else if result>MaxCaptures then begin
       result:=MaxCaptures;
      end;
      p:=pointer(Captures);
      for i:=0 to result-1 do begin
       p^:=OutputCaptures[i].StartCodePoint;
       inc(p);
       p^:=OutputCaptures[i].EndCodePoint;
       inc(p);
       p^:=OutputCaptures[i].StartCodeUnit;
       inc(p);
       p^:=OutputCaptures[i].EndCodeUnit;
       inc(p);
      end;
     end;
    end else begin
     result:=0;
    end;
   except
    on e:Exception do begin
     if assigned(Error) then begin
      s:=AnsiString(e.Message);
      Len:=length(s);
      if Len>0 then begin
       GetMem(Error^,Len+1);
       FillChar(Error^^,Len+1,ansichar(#0));
       Move(s[1],Error^^,Len);
      end else begin
       Error^:=nil;
      end;
      s:='';
     end;
     result:=-1;
    end;
   end;
  finally
   SetLength(OutputCaptures,0);
  end;
 end;
end;

function BRREMatchAll(Instance:TBRRERegExp;Input:pansichar;InputIsUTF8,InputCodePointLen,InputCodeUnitLen:longint;Matches:ppointer;StartCodePoint,StartCodeUnit,Limit:longint;LastIndexCodePoint,LastIndexCodeUnit:pointer;Error:ppansichar):longint; cdecl;
var i,j,Len:longint;
    OutputMultipleCaptures:TBRRERegExpMultipleCaptures;
    p:plongint;
    s:ansistring;
begin
 result:=0;
 if assigned(Error) and assigned(Error^) then begin
  FreeMem(Error^);
  Error^:=nil;
 end;
 if assigned(Instance) then begin
  OutputMultipleCaptures:=nil;
  try
   try
    result:=Instance.PtrMatchAll(Input,OutputMultipleCaptures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,StartCodePoint,StartCodeUnit,Limit,LastIndexCodePoint,LastIndexCodeUnit);
    if result>0 then begin
     if assigned(Matches^) then begin
      FreeMem(Matches^);
      Matches^:=nil;
     end;
     GetMem(Matches^,(result*Instance.CountOfCaptures*(sizeof(longint)*4))+(2*sizeof(longint)));
     pointer(Matches):=pointer(Matches^);
     p:=pointer(Matches);
     p^:=result;
     inc(p);
     p^:=Instance.CountOfCaptures;
     inc(p);
     for i:=0 to result-1 do begin
      for j:=0 to Instance.CountOfCaptures-1 do begin
       p^:=OutputMultipleCaptures[i,j].StartCodePoint;
       inc(p);
       p^:=OutputMultipleCaptures[i,j].EndCodePoint;
       inc(p);
       p^:=OutputMultipleCaptures[i,j].StartCodeUnit;
       inc(p);
       p^:=OutputMultipleCaptures[i,j].EndCodeUnit;
       inc(p);
      end;
     end;
    end;
   except
    on e:Exception do begin
     if assigned(Error) then begin
      s:=AnsiString(e.Message);
      Len:=length(s);
      if Len>0 then begin
       GetMem(Error^,Len+1);
       FillChar(Error^^,Len+1,ansichar(#0));
       Move(s[1],Error^^,Len);
      end else begin
       Error^:=nil;
      end;
      s:='';
     end;
     result:=-1;
    end;
   end;
  finally
   SetLength(OutputMultipleCaptures,0);
  end;
 end;
end;

function BRREMatchRef(Instance:TBRRERegExp;Input:pansichar;InputIsUTF8,InputCodePointLen,InputCodeUnitLen:longint;Captures:ppointer;MaxCaptures,StartCodePoint,StartCodeUnit:longint;LastIndexCodePoint,LastIndexCodeUnit:pointer;Error:ppansichar):longint; cdecl;
var i,Len:longint;
    OutputCaptures:TBRRERegExpCaptures;
    p:plongint;
    s:ansistring;
begin
 result:=0;
 if assigned(Error) and assigned(Error^) then begin
  FreeMem(Error^);
  Error^:=nil;
 end;
 if assigned(Instance) then begin
  OutputCaptures:=nil;
  try
   try
    if assigned(LastIndexCodeUnit) and (longint(LastIndexCodeUnit^)>=0) then begin
     inc(longint(LastIndexCodeUnit^));
    end;
    if Instance.Match(copy(AnsiString(Input)+AnsiString(''),0,InputCodeUnitLen),StartCodePoint,StartCodeUnit,OutputCaptures,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,LastIndexCodePoint,LastIndexCodeUnit) then begin
     result:=length(OutputCaptures);
     if (MaxCaptures<>0) and assigned(Captures) then begin
      if MaxCaptures<0 then begin
       GetMem(Captures^,result*(sizeof(longint)*4));
       pointer(Captures):=pointer(Captures^);
      end else if result>MaxCaptures then begin
       result:=MaxCaptures;
      end;
      p:=pointer(Captures);
      for i:=0 to result-1 do begin
       p^:=OutputCaptures[i].StartCodePoint;
       inc(p);
       p^:=OutputCaptures[i].EndCodePoint;
       inc(p);
       if OutputCaptures[i].StartCodeUnit>0 then begin
        p^:=OutputCaptures[i].StartCodeUnit-1;
       end else begin
        p^:=OutputCaptures[i].StartCodeUnit;
       end;
       inc(p);
       if OutputCaptures[i].EndCodeUnit>0 then begin
        p^:=OutputCaptures[i].EndCodeUnit-1;
       end else begin
        p^:=OutputCaptures[i].EndCodeUnit;
       end;
       inc(p);
      end;
     end;
    end else begin
     result:=0;
    end;
    if assigned(LastIndexCodeUnit) and (longint(LastIndexCodeUnit^)>0) then begin
     dec(longint(LastIndexCodeUnit^));
    end;
   except
    on e:Exception do begin
     if assigned(Error) then begin
      s:=AnsiString(e.Message);
      Len:=length(s);
      if Len>0 then begin
       GetMem(Error^,Len+1);
       FillChar(Error^^,Len+1,ansichar(#0));
       Move(s[1],Error^^,Len);
      end else begin
       Error^:=nil;
      end;
      s:='';
     end;
     result:=-1;
    end;
   end;
  finally
   SetLength(OutputCaptures,0);
  end;
 end;
end;

function BRRETest(Instance:TBRRERegExp;Input:pansichar;InputIsUTF8,InputCodePointLen,InputCodeUnitLen:longint;StartCodePoint,StartCodeUnit:longint;LastIndexCodePoint,LastIndexCodeUnit:pointer;Error:ppansichar):longint; cdecl;
var Len:longint;
    s:ansistring;
begin
 result:=0;
 if assigned(Error) and assigned(Error^) then begin
  FreeMem(Error^);
  Error^:=nil;
 end;
 if assigned(Instance) then begin
  try
   if Instance.PtrTest(Input,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,StartCodePoint,StartCodeUnit,LastIndexCodePoint,LastIndexCodeUnit) then begin
    result:=1;
   end else begin
    result:=0;
   end;
  except
   on e:Exception do begin
    if assigned(Error) then begin
     s:=AnsiString(e.Message);
     Len:=length(s);
     if Len>0 then begin
      GetMem(Error^,Len+1);
      FillChar(Error^^,Len+1,ansichar(#0));
      Move(s[1],Error^^,Len);
     end else begin
      Error^:=nil;
     end;
     s:='';
    end;
    result:=-1;
   end;
  end;
 end;
end;

function BRREFind(Instance:TBRRERegExp;Input:pansichar;InputIsUTF8,InputCodePointLen,InputCodeUnitLen:longint;StartCodePoint,StartCodeUnit:longint;LastIndexCodePoint,LastIndexCodeUnit:pointer;Error:ppansichar):longint; cdecl;
var Len:longint;
    s:ansistring;
begin
 result:=0;
 if assigned(Error) and assigned(Error^) then begin
  FreeMem(Error^);
  Error^:=nil;
 end;
 if assigned(Instance) then begin
  try
   result:=Instance.PtrFind(Input,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,StartCodePoint,StartCodeUnit,LastIndexCodePoint,LastIndexCodeUnit);
  except
   on e:Exception do begin
    if assigned(Error) then begin
     s:=AnsiString(e.Message);
     Len:=length(s);
     if Len>0 then begin
      GetMem(Error^,Len+1);
      FillChar(Error^^,Len+1,ansichar(#0));
      Move(s[1],Error^^,Len);
     end else begin
      Error^:=nil;
     end;
     s:='';
    end;
    result:=-2;
   end;
  end;
 end;
end;

function BRRESplit(Instance:TBRRERegExp;Input:pansichar;InputIsUTF8,InputCodePointLen,InputCodeUnitLen:longint;Output:ppointer;StartCodePoint,StartCodeUnit,Limit:longint;LastIndexCodePoint,LastIndexCodeUnit:pointer;Error:ppansichar):longint; cdecl;
var i,j,Len:longint;
    OutputStrings:TBRRERegExpStrings;
    p:pansichar;
    s:ansistring;
begin
 result:=0;
 if assigned(Error) and assigned(Error^) then begin
  FreeMem(Error^);
  Error^:=nil;
 end;
 if assigned(Instance) then begin
  OutputStrings:=nil;
  try
   try
    OutputStrings:=Instance.PtrSplit(Input,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,StartCodePoint,StartCodeUnit,Limit,LastIndexCodePoint,LastIndexCodeUnit);
    result:=length(OutputStrings);
    if result>0 then begin
     if assigned(Output^) then begin
      FreeMem(Output^);
      Output^:=nil;
     end;
     j:=2;
     for i:=0 to result-1 do begin
      inc(j,length(OutputStrings[i])+1);
     end;
     GetMem(Output^,j);
     FillChar(Output^^,j,AnsiChar(#0));
     pointer(Output):=pointer(Output^);
     p:=pointer(Output);
     for i:=0 to result-1 do begin
      if length(OutputStrings[i])>0 then begin
       Move(OutputStrings[i][1],p^,length(OutputStrings[i]));
      end;
      inc(p,length(OutputStrings[i])+1);
     end;
    end;
   except
    on e:Exception do begin
     if assigned(Error) then begin
      s:=AnsiString(e.Message);
      Len:=length(s);
      if Len>0 then begin
       GetMem(Error^,Len+1);
       FillChar(Error^^,Len+1,ansichar(#0));
       Move(s[1],Error^^,Len);
      end else begin
       Error^:=nil;
      end;
      s:='';
     end;
     result:=-1;
    end;
   end;
  finally
   SetLength(OutputStrings,0);
  end;
 end;
end;

function BRREReplace(Instance:TBRRERegExp;Input,Replacement:pansichar;InputIsUTF8,InputCodePointLen,InputCodeUnitLen,ReplacementIsUTF8,ReplacementCodePointLen,ReplacementCodeUnitLen,StartCodePoint,StartCodeUnit,Limit:longint;LastIndexCodePoint,LastIndexCodeUnit:pointer;ResultStr,Error:ppansichar):longint; cdecl;
var Len:longint;
    s:ansistring;
begin
 result:=0;
 if assigned(Error) and assigned(Error^) then begin
  FreeMem(Error^);
  Error^:=nil;
 end;
 if assigned(ResultStr) and assigned(ResultStr^) then begin
  FreeMem(ResultStr^);
  ResultStr^:=nil;
 end;
 if assigned(Instance) then begin
  try
   if assigned(LastIndexCodeUnit) and (longint(LastIndexCodeUnit^)>=0) then begin
    inc(longint(LastIndexCodeUnit^));
   end;
   if StartCodeUnit>=0 then begin
    inc(StartCodeUnit);
   end;
   s:=Instance.PtrReplace(Input,Replacement,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,ReplacementIsUTF8,ReplacementCodePointLen,ReplacementCodeUnitLen,StartCodePoint,StartCodeUnit,Limit,LastIndexCodePoint,LastIndexCodeUnit);
   Len:=length(s);
   result:=Len;
   if assigned(ResultStr) then begin
    if Len>0 then begin
     GetMem(ResultStr^,Len+1);
     Move(s[1],ResultStr^^,Len);
     ResultStr^[Len]:=#0;
    end else begin
     Error^:=nil;
    end;
    s:='';
   end;
   if assigned(LastIndexCodeUnit) and (longint(LastIndexCodeUnit^)>0) then begin
    dec(longint(LastIndexCodeUnit^));
   end;
  except
   on e:Exception do begin
    if assigned(Error) then begin
     s:=AnsiString(e.Message);
     Len:=length(s);
     if Len>0 then begin
      GetMem(Error^,Len+1);
      FillChar(Error^^,Len+1,ansichar(#0));
      Move(s[1],Error^^,Len);
     end else begin
      Error^:=nil;
     end;
     s:='';
    end;
    result:=-1;
   end;
  end;
 end;
end;

function BRREBenchmark(Instance:TBRRERegExp;Input:pansichar;InputIsUTF8,InputCodePointLen,InputCodeUnitLen:longint;StartCodePoint,StartCodeUnit,Limit:longint;LastIndexCodePoint,LastIndexCodeUnit:pointer;Error:ppansichar):longint; cdecl;
var Len:longint;
    s:ansistring;
begin
 result:=0;
 if assigned(Error) and assigned(Error^) then begin
  FreeMem(Error^);
  Error^:=nil;
 end;
 if assigned(Instance) then begin
  try
   Instance.PtrBenchmark(Input,InputIsUTF8,InputCodePointLen,InputCodeUnitLen,StartCodePoint,StartCodeUnit,Limit,LastIndexCodePoint,LastIndexCodeUnit);
   result:=1;
  except
   on e:Exception do begin
    if assigned(Error) then begin
     s:=AnsiString(e.Message);
     Len:=length(s);
     if Len>0 then begin
      GetMem(Error^,Len+1);
      FillChar(Error^^,Len+1,ansichar(#0));
      Move(s[1],Error^^,Len);
     end else begin
      Error^:=nil;
     end;
     s:='';
    end;
    result:=-1;
   end;
  end;
 end;
end;

procedure BRREFree(Instance:TBRRERegExp;p:pointer); cdecl;
begin
 if assigned(p) then begin
  FreeMem(p);
 end;
end;

procedure BRREDestroy(Instance:TBRRERegExp); cdecl;
begin
 if assigned(Instance) then begin
  Instance.Destroy;
 end;
end;

procedure InitializeBRRE;
const BRRESignature:ansistring=' BRRE - yet another efficient, principled regular expression library - Version '+BRREVersionStr+' - Copyright (C) 2011, Benjamin ''BeRo'' Rosseaux - benjamin@rosseaux.com - http://www.rosseaux.com ';
 procedure InitializeUTF8DFA;
 type TAnsicharset=set of ansichar;
{$IFDEF BRREStrictUTF8}
{ c0  8 11000000   | d0  2 11(010000) | e0 10 11100000   | f0 11 11110000
  c1  8 11000001   | d1  2 11(010001) | e1  3 111(00001) | f1  6 111100(01)
  c2  2 11(000010) | d2  2 11(010010) | e2  3 111(00010) | f2  6 111100(10)
  c3  2 11(000011) | d3  2 11(010011) | e3  3 111(00011) | f3  6 111100(11)
  c4  2 11(000100) | d4  2 11(010100) | e4  3 111(00100) | f4  5 11110(100)
  c5  2 11(000101) | d5  2 11(010101) | e5  3 111(00101) | f5  8 11110101
  c6  2 11(000110) | d6  2 11(010110) | e6  3 111(00110) | f6  8 11110110
  c7  2 11(000111) | d7  2 11(010111) | e7  3 111(00111) | f7  8 11110111
  c8  2 11(001000) | d8  2 11(011000) | e8  3 111(01000) | f8  8 11111000
  c9  2 11(001001) | d9  2 11(011001) | e9  3 111(01001) | f9  8 11111001
  ca  2 11(001010) | da  2 11(011010) | ea  3 111(01010) | fa  8 11111010
  cb  2 11(001011) | db  2 11(011011) | eb  3 111(01011) | fb  8 11111011
  cc  2 11(001100) | dc  2 11(011100) | ec  3 111(01100) | fc  8 11111100
  cd  2 11(001101) | dd  2 11(011101) | ed  4 1110(1101) | fd  8 11111101
  ce  2 11(001110) | de  2 11(011110) | ee  3 111(01110) | fe  8 11111110
  cf  2 11(001111) | df  2 11(011111) | ef  3 111(01111) | ff  8 11111111  }
 const cc007F=$0;
       cc808F=$1;
       ccC2DF=$2;
       ccE1ECEEEF=$3;
       ccED=$4;
       ccF4=$5;
       ccF1F3=$6;
       ccA0BF=$7;
       ccC0C1F5FF=$8;
       cc909F=$9;
       ccE0=$a;
       ccF0=$b;
       tsBEGIN=0;
       tsERROR=1;
       tsSINGLETAIL=2;
       tsDOUBLETAIL=3;
       tsDOUBLETAILwithA0BFonly=4;
       tsDOUBLETAILwith809FFonly=5;
       tsTRIPLETAILwith90BFonly=6;
       tsTRIPLETAIL=7;
       tsTRIPLETAILwith808Fonly=8;
{$ELSE}
 const cc007F=$0;
       cc80BF=$1; // Tail
       ccC0DF=$3; // ($ff shr $03)=$1f
       ccE0EF=$4; // ($ff shr $04)=$0f
       ccF0F7=$5; // ($ff shr $05)=$07
       ccF8FB=$6; // ($ff shr $06)=$03
       ccFCFD=$7; // ($ff shr $07)=$01
       ccFEFF=$8; // ($ff shr $08)=$00
       tsBEGIN=0;
       tsERROR=1;
       tsSINGLETAIL=2;
       tsDOUBLETAIL=3;
       tsTRIPLETAIL=4;
       tsQUADTAIL=5;
       tsQUINTAIL=6;
{$ENDIF}
       tsMUL=16;
  procedure AssignCharsetToCharClass(const Charset:TAnsicharset;CharClass:byte);
  var c:ansichar;
  begin
   for c:=low(ansichar) to high(ansichar) do begin
    if c in Charset then begin
     UTF8DFACharClasses[c]:=CharClass;
    end;
   end;
  end;
  procedure AddTranslation(FromState,AtCharClass,ToState:byte);
  begin
   UTF8DFATransitions[(FromState*tsMUL)+AtCharClass]:=ToState*tsMUL;
  end;
 var i:longint;
 begin
  FillChar(UTF8DFACharClasses,sizeof(TUTF8Chars),#0);
  FillChar(UTF8DFATransitions,sizeof(TUTF8Bytes),#0);
  begin
{$IFDEF BRREStrictUTF8}
   AssignCharsetToCharClass([#$00..#$7f],cc007F);
   AssignCharsetToCharClass([#$80..#$8f],cc808F);
   AssignCharsetToCharClass([#$90..#$9f],cc909F);
   AssignCharsetToCharClass([#$a0..#$bf],ccA0BF);
   AssignCharsetToCharClass([#$c0..#$c1],ccC0C1F5FF);
   AssignCharsetToCharClass([#$c2..#$df],ccC2DF);
   AssignCharsetToCharClass([#$e0],ccE0);
   AssignCharsetToCharClass([#$e1..#$ec,#$ee..#$ef],ccE1ECEEEF);
   AssignCharsetToCharClass([#$ed],ccED);
   AssignCharsetToCharClass([#$f0],ccF0);
   AssignCharsetToCharClass([#$f1..#$f3],ccF1F3);
   AssignCharsetToCharClass([#$f4],ccF4);
   AssignCharsetToCharClass([#$f5..#$ff],ccC0C1F5FF);
{$ELSE}
   AssignCharsetToCharClass([#$00..#$7f],cc007F);
   AssignCharsetToCharClass([#$80..#$bf],cc80BF);
   AssignCharsetToCharClass([#$c0..#$df],ccC0DF);
   AssignCharsetToCharClass([#$e0..#$ef],ccE0EF);
   AssignCharsetToCharClass([#$f0..#$f7],ccF0F7);
   AssignCharsetToCharClass([#$f8..#$fb],ccF8FB);
   AssignCharsetToCharClass([#$fc..#$fd],ccFCFD);
   AssignCharsetToCharClass([#$fe..#$ff],ccFEFF);
{$ENDIF}
  end;
  begin
   for i:=low(TUTF8Bytes) to high(TUTF8Bytes) do begin
    UTF8DFATransitions[i]:=tsERROR*tsMUL;
   end;
{$IFDEF BRREStrictUTF8}
   begin
    AddTranslation(tsBEGIN,cc007F,tsBEGIN);
    AddTranslation(tsBEGIN,cc808F,tsERROR);
    AddTranslation(tsBEGIN,cc909F,tsERROR);
    AddTranslation(tsBEGIN,ccA0BF,tsERROR);
    AddTranslation(tsBEGIN,ccC2DF,tsSINGLETAIL);
    AddTranslation(tsBEGIN,ccE0,tsDOUBLETAILwithA0BFonly);
    AddTranslation(tsBEGIN,ccE1ECEEEF,tsDOUBLETAIL);
    AddTranslation(tsBEGIN,ccED,tsDOUBLETAILwith809FFonly);
    AddTranslation(tsBEGIN,ccF0,tsTRIPLETAILwith90BFonly);
    AddTranslation(tsBEGIN,ccF1F3,tsTRIPLETAIL);
    AddTranslation(tsBEGIN,ccF4,tsTRIPLETAILwith808Fonly);
    AddTranslation(tsBEGIN,ccC0C1F5FF,tsERROR);
   end;
   begin
    AddTranslation(tsERROR,cc007F,tsERROR);
    AddTranslation(tsERROR,cc808F,tsERROR);
    AddTranslation(tsERROR,cc909F,tsERROR);
    AddTranslation(tsERROR,ccA0BF,tsERROR);
    AddTranslation(tsERROR,ccC2DF,tsERROR);
    AddTranslation(tsERROR,ccE0,tsERROR);
    AddTranslation(tsERROR,ccE1ECEEEF,tsERROR);
    AddTranslation(tsERROR,ccED,tsERROR);
    AddTranslation(tsERROR,ccF0,tsERROR);
    AddTranslation(tsERROR,ccF1F3,tsERROR);
    AddTranslation(tsERROR,ccF4,tsERROR);
    AddTranslation(tsERROR,ccC0C1F5FF,tsERROR);
   end;
   begin
    AddTranslation(tsSINGLETAIL,cc007F,tsERROR);
    AddTranslation(tsSINGLETAIL,cc808F,tsBEGIN);
    AddTranslation(tsSINGLETAIL,cc909F,tsBEGIN);
    AddTranslation(tsSINGLETAIL,ccA0BF,tsBEGIN);
    AddTranslation(tsSINGLETAIL,ccC2DF,tsERROR);
    AddTranslation(tsSINGLETAIL,ccE0,tsERROR);
    AddTranslation(tsSINGLETAIL,ccE1ECEEEF,tsERROR);
    AddTranslation(tsSINGLETAIL,ccED,tsERROR);
    AddTranslation(tsSINGLETAIL,ccF0,tsERROR);
    AddTranslation(tsSINGLETAIL,ccF1F3,tsERROR);
    AddTranslation(tsSINGLETAIL,ccF4,tsERROR);
    AddTranslation(tsSINGLETAIL,ccC0C1F5FF,tsERROR);
   end;
   begin
    AddTranslation(tsDOUBLETAIL,cc007F,tsERROR);
    AddTranslation(tsDOUBLETAIL,cc808F,tsSINGLETAIL);
    AddTranslation(tsDOUBLETAIL,cc909F,tsSINGLETAIL);
    AddTranslation(tsDOUBLETAIL,ccA0BF,tsSINGLETAIL);
    AddTranslation(tsDOUBLETAIL,ccC2DF,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccE0,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccE1ECEEEF,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccED,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccF0,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccF1F3,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccF4,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccC0C1F5FF,tsERROR);
   end;
   begin
    AddTranslation(tsDOUBLETAILwithA0BFonly,cc007F,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,cc808F,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,cc909F,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccA0BF,tsSINGLETAIL);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccC2DF,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccE0,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccE1ECEEEF,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccED,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccF0,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccF1F3,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccF4,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccC0C1F5FF,tsERROR);
   end;
   begin
    AddTranslation(tsDOUBLETAILwith809FFonly,cc007F,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,cc808F,tsSINGLETAIL);
    AddTranslation(tsDOUBLETAILwith809FFonly,cc909F,tsSINGLETAIL);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccA0BF,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccC2DF,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccE0,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccE1ECEEEF,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccED,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccF0,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccF1F3,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccF4,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccC0C1F5FF,tsERROR);
   end;
   begin
    AddTranslation(tsTRIPLETAILwith90BFonly,cc007F,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,cc808F,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,cc909F,tsDOUBLETAIL);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccA0BF,tsDOUBLETAIL);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccC2DF,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccE0,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccE1ECEEEF,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccED,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccF0,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccF1F3,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccF4,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccC0C1F5FF,tsERROR);
   end;
   begin
    AddTranslation(tsTRIPLETAIL,cc007F,tsERROR);
    AddTranslation(tsTRIPLETAIL,cc808F,tsDOUBLETAIL);
    AddTranslation(tsTRIPLETAIL,cc909F,tsDOUBLETAIL);
    AddTranslation(tsTRIPLETAIL,ccA0BF,tsDOUBLETAIL);
    AddTranslation(tsTRIPLETAIL,ccC2DF,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccE0,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccE1ECEEEF,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccED,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccF0,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccF1F3,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccF4,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccC0C1F5FF,tsERROR);
   end;
   begin
    AddTranslation(tsTRIPLETAILwith808Fonly,cc007F,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,cc808F,tsDOUBLETAIL);
    AddTranslation(tsTRIPLETAILwith808Fonly,cc909F,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccA0BF,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccC2DF,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccE0,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccE1ECEEEF,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccED,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccF0,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccF1F3,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccF4,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccC0C1F5FF,tsERROR);
   end;
  end;
{$ELSE}
   begin
    AddTranslation(tsBEGIN,cc007F,tsBEGIN);
    AddTranslation(tsBEGIN,cc80BF,tsERROR);
    AddTranslation(tsBEGIN,ccC0DF,tsSINGLETAIL);
    AddTranslation(tsBEGIN,ccE0EF,tsDOUBLETAIL);
    AddTranslation(tsBEGIN,ccF0F7,tsTRIPLETAIL);
    AddTranslation(tsBEGIN,ccF8FB,tsQUADTAIL);
    AddTranslation(tsBEGIN,ccFCFD,tsQUINTAIL);
    AddTranslation(tsBEGIN,ccFEFF,tsERROR);
   end;
   begin
    AddTranslation(tsERROR,cc007F,tsERROR);
    AddTranslation(tsERROR,cc80BF,tsERROR);
    AddTranslation(tsERROR,ccC0DF,tsERROR);
    AddTranslation(tsERROR,ccE0EF,tsERROR);
    AddTranslation(tsERROR,ccF0F7,tsERROR);
    AddTranslation(tsERROR,ccF8FB,tsERROR);
    AddTranslation(tsERROR,ccFCFD,tsERROR);
    AddTranslation(tsERROR,ccFEFF,tsERROR);
   end;
   begin
    AddTranslation(tsSINGLETAIL,cc007F,tsERROR);
    AddTranslation(tsSINGLETAIL,cc80BF,tsBEGIN);
    AddTranslation(tsSINGLETAIL,ccC0DF,tsERROR);
    AddTranslation(tsSINGLETAIL,ccE0EF,tsERROR);
    AddTranslation(tsSINGLETAIL,ccF0F7,tsERROR);
    AddTranslation(tsSINGLETAIL,ccF8FB,tsERROR);
    AddTranslation(tsSINGLETAIL,ccFCFD,tsERROR);
    AddTranslation(tsSINGLETAIL,ccFEFF,tsERROR);
   end;
   begin
    AddTranslation(tsDOUBLETAIL,cc007F,tsERROR);
    AddTranslation(tsDOUBLETAIL,cc80BF,tsSINGLETAIL);
    AddTranslation(tsDOUBLETAIL,ccC0DF,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccE0EF,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccF0F7,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccF8FB,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccFCFD,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccFEFF,tsERROR);
   end;
   begin
    AddTranslation(tsTRIPLETAIL,cc007F,tsERROR);
    AddTranslation(tsTRIPLETAIL,cc80BF,tsDOUBLETAIL);
    AddTranslation(tsTRIPLETAIL,ccC0DF,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccE0EF,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccF0F7,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccF8FB,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccFCFD,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccFEFF,tsERROR);
   end;
   begin
    AddTranslation(tsQUADTAIL,cc007F,tsERROR);
    AddTranslation(tsQUADTAIL,cc80BF,tsTRIPLETAIL);
    AddTranslation(tsQUADTAIL,ccC0DF,tsERROR);
    AddTranslation(tsQUADTAIL,ccE0EF,tsERROR);
    AddTranslation(tsQUADTAIL,ccF0F7,tsERROR);
    AddTranslation(tsQUADTAIL,ccF8FB,tsERROR);
    AddTranslation(tsQUADTAIL,ccFCFD,tsERROR);
    AddTranslation(tsQUADTAIL,ccFEFF,tsERROR);
   end;
   begin
    AddTranslation(tsQUINTAIL,cc007F,tsERROR);
    AddTranslation(tsQUINTAIL,cc80BF,tsQUADTAIL);
    AddTranslation(tsQUINTAIL,ccC0DF,tsERROR);
    AddTranslation(tsQUINTAIL,ccE0EF,tsERROR);
    AddTranslation(tsQUINTAIL,ccF0F7,tsERROR);
    AddTranslation(tsQUINTAIL,ccF8FB,tsERROR);
    AddTranslation(tsQUINTAIL,ccFCFD,tsERROR);
    AddTranslation(tsQUINTAIL,ccFEFF,tsERROR);
   end;
  end;
{$ENDIF}
 end;
 procedure InitializeUnicode;
 var i,l,h:longword;
     Count:longint;
     s:ansistring;
  procedure AddRange(Table,FirstChar,LastChar:longword);
  begin
   if (Count+1)>length(BRREUnicodeCharRangeClasses[Table]) then begin
    SetLength(BRREUnicodeCharRangeClasses[Table],(Count+4097) and not 4095);
   end;
   BRREUnicodeCharRangeClasses[Table,Count,0]:=FirstChar;
   BRREUnicodeCharRangeClasses[Table,Count,1]:=LastChar;
   inc(Count);
  end;
  procedure AddChar(Table,TheChar:longword);
  begin
   AddRange(Table,TheChar,TheChar);
  end;
 begin
  FillChar(BRREUnicodeCharRangeClasses,SizeOf(TBRREUnicodeCharRangeClasses),#0);
  begin
   Count:=0;
   l:=$ffffffff;
   h:=0;
   for i:=0 to $10ffff do begin
    if (BRREUnicodeGetCategoryFromTable(i) in [BRREUnicodeCategoryLu,BRREUnicodeCategoryLl,BRREUnicodeCategoryLt,BRREUnicodeCategoryLm,BRREUnicodeCategoryLo,BRREUnicodeCategoryNd,BRREUnicodeCategoryNl,BRREUnicodeCategoryNo,BRREUnicodeCategoryPc]) or (i=ord('_')) then begin
     if l<=h then begin
      if (h+1)=i then begin
       h:=i;
      end else begin
       AddRange(brreucrWORDS,l,h);
       l:=i;
       h:=i;
      end;
     end else begin
      l:=i;
      h:=i;
     end;
    end;
   end;
   if l<=h then begin
    AddRange(brreucrWORDS,l,h);
   end;
   SetLength(BRREUnicodeCharRangeClasses[brreucrWORDS],Count);
  end;
  begin
   Count:=0;
   l:=$ffffffff;
   h:=0;
   for i:=0 to $10ffff do begin
    if BRREUnicodeGetCategoryFromTable(i) in [BRREUnicodeCategoryNd] then begin
     if l<=h then begin
      if (h+1)=i then begin
       h:=i;
      end else begin
       AddRange(brreucrDIGITS,l,h);
       l:=i;
       h:=i;
      end;
     end else begin
      l:=i;
      h:=i;
     end;
    end;
   end;
   if l<=h then begin
    AddRange(brreucrDIGITS,l,h);
   end;
   SetLength(BRREUnicodeCharRangeClasses[brreucrDIGITS],Count);
  end;
  begin
   Count:=0;
   AddRange(brreucrWHITESPACES,$0009,$000d);
   AddChar(brreucrWHITESPACES,$0020);
   AddChar(brreucrWHITESPACES,$00a0);
   AddChar(brreucrWHITESPACES,$1680);
   AddChar(brreucrWHITESPACES,$180e);
   AddRange(brreucrWHITESPACES,$2000,$200b);
   AddRange(brreucrWHITESPACES,$2028,$2029);
   AddChar(brreucrWHITESPACES,$202f);
   AddChar(brreucrWHITESPACES,$205f);
   AddChar(brreucrWHITESPACES,$3000);
   AddChar(brreucrWHITESPACES,$fffe);
   AddChar(brreucrWHITESPACES,$feff);
{  l:=$ffffffff;
   h:=0;
   for i:=0 to $10ffff do begin
    if BRREUnicodeIsWhiteSpace(i) then begin
     if l<=h then begin
      if (h+1)=i then begin
       h:=i;
      end else begin
       AddRange(brreucrWHITESPACES,l,h);
       l:=i;
       h:=i;
      end;
     end else begin
      l:=i;
      h:=i;
     end;
    end;
   end;
   if l<=h then begin
    AddRange(brreucrWHITESPACES,l,h);
   end;}
   SetLength(BRREUnicodeCharRangeClasses[brreucrWHITESPACES],Count);
  end;
  begin                                           
   BRREUnicodeClassHashMap:=TBRREStringHashMap.Create;
   for i:=0 to BRREUnicodeCategoryCount-1 do begin
    s:=BRREUnicodeCategoryIDs[i];
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase(s),1 shl i);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Is'+s),1 shl i);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('In'+s),1 shl i);
   end;
   begin
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Lu'),1 shl BRRE_CT_UPPERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Uppercase_Letter'),1 shl BRRE_CT_UPPERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('UppercaseLetter'),1 shl BRRE_CT_UPPERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Ll'),1 shl BRRE_CT_LOWERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Lowercase_Letter'),1 shl BRRE_CT_LOWERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('LowercaseLetter'),1 shl BRRE_CT_LOWERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Lt'),1 shl BRRE_CT_TITLECASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Titlecase_Letter'),1 shl BRRE_CT_TITLECASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('TitlecaseLetter'),1 shl BRRE_CT_TITLECASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Lm'),1 shl BRRE_CT_MODIFIER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Modifier_Letter'),1 shl BRRE_CT_MODIFIER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('ModifierLetter'),1 shl BRRE_CT_MODIFIER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Lo'),1 shl BRRE_CT_OTHER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Other_Letter'),1 shl BRRE_CT_OTHER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('OtherLetter'),1 shl BRRE_CT_OTHER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('L'),(1 shl BRRE_CT_UPPERCASE_LETTER) or (1 shl BRRE_CT_LOWERCASE_LETTER) or (1 shl BRRE_CT_TITLECASE_LETTER) or (1 shl BRRE_CT_MODIFIER_LETTER) or (1 shl BRRE_CT_OTHER_LETTER));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Mn'),1 shl BRRE_CT_NON_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Non_Spacing_Mark'),1 shl BRRE_CT_NON_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('NonSpacingMark'),1 shl BRRE_CT_NON_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Me'),1 shl BRRE_CT_ENCLOSING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Enclosing_Mark'),1 shl BRRE_CT_ENCLOSING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('EnclosingMark'),1 shl BRRE_CT_ENCLOSING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Mc'),1 shl BRRE_CT_COMBINING_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Spacing_Combining_Mark'),1 shl BRRE_CT_COMBINING_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('SpacingCombiningMark'),1 shl BRRE_CT_COMBINING_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('M'),(1 shl BRRE_CT_NON_SPACING_MARK) or (1 shl BRRE_CT_ENCLOSING_MARK) or (1 shl BRRE_CT_COMBINING_SPACING_MARK));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Mark'),(1 shl BRRE_CT_NON_SPACING_MARK) or (1 shl BRRE_CT_ENCLOSING_MARK) or (1 shl BRRE_CT_COMBINING_SPACING_MARK));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Nd'),1 shl BRRE_CT_DECIMAL_DIGIT_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Decimal_Digit_Number'),1 shl BRRE_CT_DECIMAL_DIGIT_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('DecimalDigitNumber'),1 shl BRRE_CT_DECIMAL_DIGIT_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Nl'),1 shl BRRE_CT_LETTER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Letter_Number'),1 shl BRRE_CT_LETTER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('LetterNumber'),1 shl BRRE_CT_LETTER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('No'),1 shl BRRE_CT_OTHER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Other_Number'),1 shl BRRE_CT_OTHER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('OtherNumber'),1 shl BRRE_CT_OTHER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('N'),(1 shl BRRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl BRRE_CT_LETTER_NUMBER) or (1 shl BRRE_CT_OTHER_NUMBER));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Number'),(1 shl BRRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl BRRE_CT_LETTER_NUMBER) or (1 shl BRRE_CT_OTHER_NUMBER));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Zs'),1 shl BRRE_CT_SPACE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Space_Separator'),1 shl BRRE_CT_SPACE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('SpaceSeparator'),1 shl BRRE_CT_SPACE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Zl'),1 shl BRRE_CT_LINE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Line_Separator'),1 shl BRRE_CT_LINE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('LineSeparator'),1 shl BRRE_CT_LINE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Zp'),1 shl BRRE_CT_PARAGRAPH_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Paragraph_Separator'),1 shl BRRE_CT_PARAGRAPH_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('ParagraphSeparator'),1 shl BRRE_CT_PARAGRAPH_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Z'),(1 shl BRRE_CT_SPACE_SEPARATOR) or (1 shl BRRE_CT_LINE_SEPARATOR) or (1 shl BRRE_CT_PARAGRAPH_SEPARATOR));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Seperator'),(1 shl BRRE_CT_SPACE_SEPARATOR) or (1 shl BRRE_CT_LINE_SEPARATOR) or (1 shl BRRE_CT_PARAGRAPH_SEPARATOR));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Cc'),1 shl BRRE_CT_CONTROL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Control'),1 shl BRRE_CT_CONTROL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Cf'),1 shl BRRE_CT_FORMAT);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Format'),1 shl BRRE_CT_CONTROL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Co'),1 shl BRRE_CT_PRIVATE_USE);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Private_Use'),1 shl BRRE_CT_PRIVATE_USE);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('PrivateUse'),1 shl BRRE_CT_PRIVATE_USE);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Cs'),1 shl BRRE_CT_SURROGATE);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Surrogate'),1 shl BRRE_CT_SURROGATE);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Cn'),1 shl BRRE_CT_UNASSIGNED);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Unassigned'),1 shl BRRE_CT_UNASSIGNED);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('C'),(1 shl BRRE_CT_CONTROL) or (1 shl BRRE_CT_FORMAT) or (1 shl BRRE_CT_PRIVATE_USE) or (1 shl BRRE_CT_SURROGATE) or (1 shl BRRE_CT_UNASSIGNED));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Other'),(1 shl BRRE_CT_CONTROL) or (1 shl BRRE_CT_FORMAT) or (1 shl BRRE_CT_PRIVATE_USE) or (1 shl BRRE_CT_SURROGATE) or (1 shl BRRE_CT_UNASSIGNED));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Pd'),1 shl BRRE_CT_DASH_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Dash_Punctuation'),1 shl BRRE_CT_DASH_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('DashPunctuation'),1 shl BRRE_CT_DASH_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Ps'),1 shl BRRE_CT_START_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Open_Punctuation'),1 shl BRRE_CT_START_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('OpenPunctuation'),1 shl BRRE_CT_START_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Pe'),1 shl BRRE_CT_END_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Close_Punctuation'),1 shl BRRE_CT_END_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('ClosePunctuation'),1 shl BRRE_CT_END_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Pi'),1 shl BRRE_CT_INITIAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Initial_Punctuation'),1 shl BRRE_CT_INITIAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InitialPunctuation'),1 shl BRRE_CT_INITIAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Pf'),1 shl BRRE_CT_FINAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Final_Punctuation'),1 shl BRRE_CT_FINAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('FinalPunctuation'),1 shl BRRE_CT_FINAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Pc'),1 shl BRRE_CT_CONNECTOR_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Connector_Punctuation'),1 shl BRRE_CT_CONNECTOR_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('ConnectorPunctuation'),1 shl BRRE_CT_CONNECTOR_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Po'),1 shl BRRE_CT_OTHER_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Other_Punctuation'),1 shl BRRE_CT_OTHER_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('OtherPunctuation'),1 shl BRRE_CT_OTHER_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('P'),(1 shl BRRE_CT_DASH_PUNCTUATION) or (1 shl BRRE_CT_START_PUNCTUATION) or (1 shl BRRE_CT_END_PUNCTUATION) or (1 shl BRRE_CT_INITIAL_PUNCTUATION) or (1 shl BRRE_CT_FINAL_PUNCTUATION) or (1 shl BRRE_CT_CONNECTOR_PUNCTUATION) or (1 shl BRRE_CT_OTHER_PUNCTUATION));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Punctuation'),(1 shl BRRE_CT_DASH_PUNCTUATION) or (1 shl BRRE_CT_START_PUNCTUATION) or (1 shl BRRE_CT_END_PUNCTUATION) or (1 shl BRRE_CT_INITIAL_PUNCTUATION) or (1 shl BRRE_CT_FINAL_PUNCTUATION) or (1 shl BRRE_CT_CONNECTOR_PUNCTUATION) or (1 shl BRRE_CT_OTHER_PUNCTUATION));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Sm'),1 shl BRRE_CT_MATH_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Math_Symbol'),1 shl BRRE_CT_MATH_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('MathSymbol'),1 shl BRRE_CT_MATH_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Sc'),1 shl BRRE_CT_CURRENCY_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Currency_Symbol'),1 shl BRRE_CT_CURRENCY_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('CurrencySymbol'),1 shl BRRE_CT_CURRENCY_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Sk'),1 shl BRRE_CT_MODIFIER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Modifier_Symbol'),1 shl BRRE_CT_MODIFIER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('ModifierSymbol'),1 shl BRRE_CT_MODIFIER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('So'),1 shl BRRE_CT_OTHER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Other_Symbol'),1 shl BRRE_CT_OTHER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('OtherSymbol'),1 shl BRRE_CT_OTHER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('S'),(1 shl BRRE_CT_MATH_SYMBOL) or (1 shl BRRE_CT_CURRENCY_SYMBOL) or (1 shl BRRE_CT_MODIFIER_SYMBOL) or (1 shl BRRE_CT_OTHER_SYMBOL));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('Symbol'),(1 shl BRRE_CT_MATH_SYMBOL) or (1 shl BRRE_CT_CURRENCY_SYMBOL) or (1 shl BRRE_CT_MODIFIER_SYMBOL) or (1 shl BRRE_CT_OTHER_SYMBOL));
   end;
   begin
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsLu'),1 shl BRRE_CT_UPPERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsUppercase_Letter'),1 shl BRRE_CT_UPPERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsUppercaseLetter'),1 shl BRRE_CT_UPPERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsLl'),1 shl BRRE_CT_LOWERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsLowercase_Letter'),1 shl BRRE_CT_LOWERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsLowercaseLetter'),1 shl BRRE_CT_LOWERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsLt'),1 shl BRRE_CT_TITLECASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsTitlecase_Letter'),1 shl BRRE_CT_TITLECASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsTitlecaseLetter'),1 shl BRRE_CT_TITLECASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsLm'),1 shl BRRE_CT_MODIFIER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsModifier_Letter'),1 shl BRRE_CT_MODIFIER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsModifierLetter'),1 shl BRRE_CT_MODIFIER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsLo'),1 shl BRRE_CT_OTHER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsOther_Letter'),1 shl BRRE_CT_OTHER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsOtherLetter'),1 shl BRRE_CT_OTHER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsL'),(1 shl BRRE_CT_UPPERCASE_LETTER) or (1 shl BRRE_CT_LOWERCASE_LETTER) or (1 shl BRRE_CT_TITLECASE_LETTER) or (1 shl BRRE_CT_MODIFIER_LETTER) or (1 shl BRRE_CT_OTHER_LETTER));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsMn'),1 shl BRRE_CT_NON_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsNon_Spacing_Mark'),1 shl BRRE_CT_NON_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsNonSpacingMark'),1 shl BRRE_CT_NON_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsMe'),1 shl BRRE_CT_ENCLOSING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsEnclosing_Mark'),1 shl BRRE_CT_ENCLOSING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsEnclosingMark'),1 shl BRRE_CT_ENCLOSING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsMc'),1 shl BRRE_CT_COMBINING_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsSpacing_Combining_Mark'),1 shl BRRE_CT_COMBINING_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsSpacingCombiningMark'),1 shl BRRE_CT_COMBINING_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsM'),(1 shl BRRE_CT_NON_SPACING_MARK) or (1 shl BRRE_CT_ENCLOSING_MARK) or (1 shl BRRE_CT_COMBINING_SPACING_MARK));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsMark'),(1 shl BRRE_CT_NON_SPACING_MARK) or (1 shl BRRE_CT_ENCLOSING_MARK) or (1 shl BRRE_CT_COMBINING_SPACING_MARK));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsNd'),1 shl BRRE_CT_DECIMAL_DIGIT_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsDecimal_Digit_Number'),1 shl BRRE_CT_DECIMAL_DIGIT_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsDecimalDigitNumber'),1 shl BRRE_CT_DECIMAL_DIGIT_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsNl'),1 shl BRRE_CT_LETTER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsLetter_Number'),1 shl BRRE_CT_LETTER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsLetterNumber'),1 shl BRRE_CT_LETTER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsNo'),1 shl BRRE_CT_OTHER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsOther_Number'),1 shl BRRE_CT_OTHER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsOtherNumber'),1 shl BRRE_CT_OTHER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsN'),(1 shl BRRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl BRRE_CT_LETTER_NUMBER) or (1 shl BRRE_CT_OTHER_NUMBER));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsNumber'),(1 shl BRRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl BRRE_CT_LETTER_NUMBER) or (1 shl BRRE_CT_OTHER_NUMBER));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsZs'),1 shl BRRE_CT_SPACE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsSpace_Separator'),1 shl BRRE_CT_SPACE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsSpaceSeparator'),1 shl BRRE_CT_SPACE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsZl'),1 shl BRRE_CT_LINE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsLine_Separator'),1 shl BRRE_CT_LINE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsLineSeparator'),1 shl BRRE_CT_LINE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsZp'),1 shl BRRE_CT_PARAGRAPH_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsParagraph_Separator'),1 shl BRRE_CT_PARAGRAPH_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsParagraphSeparator'),1 shl BRRE_CT_PARAGRAPH_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsZ'),(1 shl BRRE_CT_SPACE_SEPARATOR) or (1 shl BRRE_CT_LINE_SEPARATOR) or (1 shl BRRE_CT_PARAGRAPH_SEPARATOR));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsSeperator'),(1 shl BRRE_CT_SPACE_SEPARATOR) or (1 shl BRRE_CT_LINE_SEPARATOR) or (1 shl BRRE_CT_PARAGRAPH_SEPARATOR));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsCc'),1 shl BRRE_CT_CONTROL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsControl'),1 shl BRRE_CT_CONTROL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsCf'),1 shl BRRE_CT_FORMAT);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsFormat'),1 shl BRRE_CT_CONTROL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsCo'),1 shl BRRE_CT_PRIVATE_USE);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsPrivate_Use'),1 shl BRRE_CT_PRIVATE_USE);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsPrivateUse'),1 shl BRRE_CT_PRIVATE_USE);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsCs'),1 shl BRRE_CT_SURROGATE);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsSurrogate'),1 shl BRRE_CT_SURROGATE);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsCn'),1 shl BRRE_CT_UNASSIGNED);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsUnassigned'),1 shl BRRE_CT_UNASSIGNED);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsC'),(1 shl BRRE_CT_CONTROL) or (1 shl BRRE_CT_FORMAT) or (1 shl BRRE_CT_PRIVATE_USE) or (1 shl BRRE_CT_SURROGATE) or (1 shl BRRE_CT_UNASSIGNED));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsOther'),(1 shl BRRE_CT_CONTROL) or (1 shl BRRE_CT_FORMAT) or (1 shl BRRE_CT_PRIVATE_USE) or (1 shl BRRE_CT_SURROGATE) or (1 shl BRRE_CT_UNASSIGNED));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsPd'),1 shl BRRE_CT_DASH_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsDash_Punctuation'),1 shl BRRE_CT_DASH_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsDashPunctuation'),1 shl BRRE_CT_DASH_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsPs'),1 shl BRRE_CT_START_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsOpen_Punctuation'),1 shl BRRE_CT_START_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsOpenPunctuation'),1 shl BRRE_CT_START_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsPe'),1 shl BRRE_CT_END_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsClose_Punctuation'),1 shl BRRE_CT_END_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsClosePunctuation'),1 shl BRRE_CT_END_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsPi'),1 shl BRRE_CT_INITIAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsInitial_Punctuation'),1 shl BRRE_CT_INITIAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsInitialPunctuation'),1 shl BRRE_CT_INITIAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsPf'),1 shl BRRE_CT_FINAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsFinal_Punctuation'),1 shl BRRE_CT_FINAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsFinalPunctuation'),1 shl BRRE_CT_FINAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsPc'),1 shl BRRE_CT_CONNECTOR_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsConnector_Punctuation'),1 shl BRRE_CT_CONNECTOR_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsConnectorPunctuation'),1 shl BRRE_CT_CONNECTOR_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsPo'),1 shl BRRE_CT_OTHER_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsOther_Punctuation'),1 shl BRRE_CT_OTHER_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsOtherPunctuation'),1 shl BRRE_CT_OTHER_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsP'),(1 shl BRRE_CT_DASH_PUNCTUATION) or (1 shl BRRE_CT_START_PUNCTUATION) or (1 shl BRRE_CT_END_PUNCTUATION) or (1 shl BRRE_CT_INITIAL_PUNCTUATION) or (1 shl BRRE_CT_FINAL_PUNCTUATION) or (1 shl BRRE_CT_CONNECTOR_PUNCTUATION) or (1 shl BRRE_CT_OTHER_PUNCTUATION));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsPunctuation'),(1 shl BRRE_CT_DASH_PUNCTUATION) or (1 shl BRRE_CT_START_PUNCTUATION) or (1 shl BRRE_CT_END_PUNCTUATION) or (1 shl BRRE_CT_INITIAL_PUNCTUATION) or (1 shl BRRE_CT_FINAL_PUNCTUATION) or (1 shl BRRE_CT_CONNECTOR_PUNCTUATION) or (1 shl BRRE_CT_OTHER_PUNCTUATION));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsSm'),1 shl BRRE_CT_MATH_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsMath_Symbol'),1 shl BRRE_CT_MATH_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsMathSymbol'),1 shl BRRE_CT_MATH_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsSc'),1 shl BRRE_CT_CURRENCY_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsCurrency_Symbol'),1 shl BRRE_CT_CURRENCY_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsCurrencySymbol'),1 shl BRRE_CT_CURRENCY_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsSk'),1 shl BRRE_CT_MODIFIER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsModifier_Symbol'),1 shl BRRE_CT_MODIFIER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsModifierSymbol'),1 shl BRRE_CT_MODIFIER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsSo'),1 shl BRRE_CT_OTHER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsOther_Symbol'),1 shl BRRE_CT_OTHER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsOtherSymbol'),1 shl BRRE_CT_OTHER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsS'),(1 shl BRRE_CT_MATH_SYMBOL) or (1 shl BRRE_CT_CURRENCY_SYMBOL) or (1 shl BRRE_CT_MODIFIER_SYMBOL) or (1 shl BRRE_CT_OTHER_SYMBOL));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('IsSymbol'),(1 shl BRRE_CT_MATH_SYMBOL) or (1 shl BRRE_CT_CURRENCY_SYMBOL) or (1 shl BRRE_CT_MODIFIER_SYMBOL) or (1 shl BRRE_CT_OTHER_SYMBOL));
   end;
   begin
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InLu'),1 shl BRRE_CT_UPPERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InUppercase_Letter'),1 shl BRRE_CT_UPPERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InUppercaseLetter'),1 shl BRRE_CT_UPPERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InLl'),1 shl BRRE_CT_LOWERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InLowercase_Letter'),1 shl BRRE_CT_LOWERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InLowercaseLetter'),1 shl BRRE_CT_LOWERCASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InLt'),1 shl BRRE_CT_TITLECASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InTitlecase_Letter'),1 shl BRRE_CT_TITLECASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InTitlecaseLetter'),1 shl BRRE_CT_TITLECASE_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InLm'),1 shl BRRE_CT_MODIFIER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InModifier_Letter'),1 shl BRRE_CT_MODIFIER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InModifierLetter'),1 shl BRRE_CT_MODIFIER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InLo'),1 shl BRRE_CT_OTHER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InOther_Letter'),1 shl BRRE_CT_OTHER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InOtherLetter'),1 shl BRRE_CT_OTHER_LETTER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InL'),(1 shl BRRE_CT_UPPERCASE_LETTER) or (1 shl BRRE_CT_LOWERCASE_LETTER) or (1 shl BRRE_CT_TITLECASE_LETTER) or (1 shl BRRE_CT_MODIFIER_LETTER) or (1 shl BRRE_CT_OTHER_LETTER));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InMn'),1 shl BRRE_CT_NON_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InNon_Spacing_Mark'),1 shl BRRE_CT_NON_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InNonSpacingMark'),1 shl BRRE_CT_NON_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InMe'),1 shl BRRE_CT_ENCLOSING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InEnclosing_Mark'),1 shl BRRE_CT_ENCLOSING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InEnclosingMark'),1 shl BRRE_CT_ENCLOSING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InMc'),1 shl BRRE_CT_COMBINING_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InSpacing_Combining_Mark'),1 shl BRRE_CT_COMBINING_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InSpacingCombiningMark'),1 shl BRRE_CT_COMBINING_SPACING_MARK);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InM'),(1 shl BRRE_CT_NON_SPACING_MARK) or (1 shl BRRE_CT_ENCLOSING_MARK) or (1 shl BRRE_CT_COMBINING_SPACING_MARK));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InMark'),(1 shl BRRE_CT_NON_SPACING_MARK) or (1 shl BRRE_CT_ENCLOSING_MARK) or (1 shl BRRE_CT_COMBINING_SPACING_MARK));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InNd'),1 shl BRRE_CT_DECIMAL_DIGIT_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InDecimal_Digit_Number'),1 shl BRRE_CT_DECIMAL_DIGIT_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InDecimalDigitNumber'),1 shl BRRE_CT_DECIMAL_DIGIT_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InNl'),1 shl BRRE_CT_LETTER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InLetter_Number'),1 shl BRRE_CT_LETTER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InLetterNumber'),1 shl BRRE_CT_LETTER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InNo'),1 shl BRRE_CT_OTHER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InOther_Number'),1 shl BRRE_CT_OTHER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InOtherNumber'),1 shl BRRE_CT_OTHER_NUMBER);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InN'),(1 shl BRRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl BRRE_CT_LETTER_NUMBER) or (1 shl BRRE_CT_OTHER_NUMBER));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InNumber'),(1 shl BRRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl BRRE_CT_LETTER_NUMBER) or (1 shl BRRE_CT_OTHER_NUMBER));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InZs'),1 shl BRRE_CT_SPACE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InSpace_Separator'),1 shl BRRE_CT_SPACE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InSpaceSeparator'),1 shl BRRE_CT_SPACE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InZl'),1 shl BRRE_CT_LINE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InLine_Separator'),1 shl BRRE_CT_LINE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InLineSeparator'),1 shl BRRE_CT_LINE_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InZp'),1 shl BRRE_CT_PARAGRAPH_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InParagraph_Separator'),1 shl BRRE_CT_PARAGRAPH_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InParagraphSeparator'),1 shl BRRE_CT_PARAGRAPH_SEPARATOR);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InZ'),(1 shl BRRE_CT_SPACE_SEPARATOR) or (1 shl BRRE_CT_LINE_SEPARATOR) or (1 shl BRRE_CT_PARAGRAPH_SEPARATOR));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InSeperator'),(1 shl BRRE_CT_SPACE_SEPARATOR) or (1 shl BRRE_CT_LINE_SEPARATOR) or (1 shl BRRE_CT_PARAGRAPH_SEPARATOR));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InCc'),1 shl BRRE_CT_CONTROL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InControl'),1 shl BRRE_CT_CONTROL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InCf'),1 shl BRRE_CT_FORMAT);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InFormat'),1 shl BRRE_CT_CONTROL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InCo'),1 shl BRRE_CT_PRIVATE_USE);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InPrivate_Use'),1 shl BRRE_CT_PRIVATE_USE);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InPrivateUse'),1 shl BRRE_CT_PRIVATE_USE);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InCs'),1 shl BRRE_CT_SURROGATE);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InSurrogate'),1 shl BRRE_CT_SURROGATE);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InCn'),1 shl BRRE_CT_UNASSIGNED);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InUnassigned'),1 shl BRRE_CT_UNASSIGNED);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InC'),(1 shl BRRE_CT_CONTROL) or (1 shl BRRE_CT_FORMAT) or (1 shl BRRE_CT_PRIVATE_USE) or (1 shl BRRE_CT_SURROGATE) or (1 shl BRRE_CT_UNASSIGNED));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InOther'),(1 shl BRRE_CT_CONTROL) or (1 shl BRRE_CT_FORMAT) or (1 shl BRRE_CT_PRIVATE_USE) or (1 shl BRRE_CT_SURROGATE) or (1 shl BRRE_CT_UNASSIGNED));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InPd'),1 shl BRRE_CT_DASH_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InDash_Punctuation'),1 shl BRRE_CT_DASH_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InDashPunctuation'),1 shl BRRE_CT_DASH_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InPs'),1 shl BRRE_CT_START_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InOpen_Punctuation'),1 shl BRRE_CT_START_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InOpenPunctuation'),1 shl BRRE_CT_START_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InPe'),1 shl BRRE_CT_END_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InClose_Punctuation'),1 shl BRRE_CT_END_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InClosePunctuation'),1 shl BRRE_CT_END_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InPi'),1 shl BRRE_CT_INITIAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InInitial_Punctuation'),1 shl BRRE_CT_INITIAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InInitialPunctuation'),1 shl BRRE_CT_INITIAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InPf'),1 shl BRRE_CT_FINAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InFinal_Punctuation'),1 shl BRRE_CT_FINAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InFinalPunctuation'),1 shl BRRE_CT_FINAL_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InPc'),1 shl BRRE_CT_CONNECTOR_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InConnector_Punctuation'),1 shl BRRE_CT_CONNECTOR_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InConnectorPunctuation'),1 shl BRRE_CT_CONNECTOR_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InPo'),1 shl BRRE_CT_OTHER_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InOther_Punctuation'),1 shl BRRE_CT_OTHER_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InOtherPunctuation'),1 shl BRRE_CT_OTHER_PUNCTUATION);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InP'),(1 shl BRRE_CT_DASH_PUNCTUATION) or (1 shl BRRE_CT_START_PUNCTUATION) or (1 shl BRRE_CT_END_PUNCTUATION) or (1 shl BRRE_CT_INITIAL_PUNCTUATION) or (1 shl BRRE_CT_FINAL_PUNCTUATION) or (1 shl BRRE_CT_CONNECTOR_PUNCTUATION) or (1 shl BRRE_CT_OTHER_PUNCTUATION));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InPunctuation'),(1 shl BRRE_CT_DASH_PUNCTUATION) or (1 shl BRRE_CT_START_PUNCTUATION) or (1 shl BRRE_CT_END_PUNCTUATION) or (1 shl BRRE_CT_INITIAL_PUNCTUATION) or (1 shl BRRE_CT_FINAL_PUNCTUATION) or (1 shl BRRE_CT_CONNECTOR_PUNCTUATION) or (1 shl BRRE_CT_OTHER_PUNCTUATION));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InSm'),1 shl BRRE_CT_MATH_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InMath_Symbol'),1 shl BRRE_CT_MATH_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InMathSymbol'),1 shl BRRE_CT_MATH_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InSc'),1 shl BRRE_CT_CURRENCY_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InCurrency_Symbol'),1 shl BRRE_CT_CURRENCY_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InCurrencySymbol'),1 shl BRRE_CT_CURRENCY_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InSk'),1 shl BRRE_CT_MODIFIER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InModifier_Symbol'),1 shl BRRE_CT_MODIFIER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InModifierSymbol'),1 shl BRRE_CT_MODIFIER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InSo'),1 shl BRRE_CT_OTHER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InOther_Symbol'),1 shl BRRE_CT_OTHER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InOtherSymbol'),1 shl BRRE_CT_OTHER_SYMBOL);
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InS'),(1 shl BRRE_CT_MATH_SYMBOL) or (1 shl BRRE_CT_CURRENCY_SYMBOL) or (1 shl BRRE_CT_MODIFIER_SYMBOL) or (1 shl BRRE_CT_OTHER_SYMBOL));
    BRREUnicodeClassHashMap.SetValue(BRREUTF8Lowercase('InSymbol'),(1 shl BRRE_CT_MATH_SYMBOL) or (1 shl BRRE_CT_CURRENCY_SYMBOL) or (1 shl BRRE_CT_MODIFIER_SYMBOL) or (1 shl BRRE_CT_OTHER_SYMBOL));
   end;
  end;
  begin
   BRREUnicodeScriptHashMap:=TBRREStringHashMap.Create;
   for i:=BRREUnicodeScriptCommon to BRREUnicodeScriptCount-1 do begin
    s:=BRREUnicodeScriptIDs[i];
    BRREUnicodeScriptHashMap.SetValue(BRREUTF8Lowercase(s),i);
    BRREUnicodeScriptHashMap.SetValue(BRREUTF8Lowercase('In'+s),i);
    BRREUnicodeScriptHashMap.SetValue(BRREUTF8Lowercase('Is'+s),i);
    s:=AnsiString(StringReplace(String(s),'_','',[rfREPLACEALL]));
    BRREUnicodeScriptHashMap.SetValue(BRREUTF8Lowercase(s),i);
    BRREUnicodeScriptHashMap.SetValue(BRREUTF8Lowercase('In'+s),i);
    BRREUnicodeScriptHashMap.SetValue(BRREUTF8Lowercase('Is'+s),i);
   end;
  end;
  begin
   BRREUnicodeBlockHashMap:=TBRREStringHashMap.Create;
   for i:=BRREUnicodeScriptCommon to BRREUnicodeBlockCount-1 do begin
    s:=AnsiString(StringReplace(String(BRREUnicodeBlocks[i].Name),' ','_',[rfREPLACEALL]));
    BRREUnicodeBlockHashMap.SetValue(BRREUTF8Lowercase(s),i);
    BRREUnicodeBlockHashMap.SetValue(BRREUTF8Lowercase('In'+s),i);
    BRREUnicodeBlockHashMap.SetValue(BRREUTF8Lowercase('Is'+s),i);
    s:=AnsiString(StringReplace(String(s),'_','',[rfREPLACEALL]));
    BRREUnicodeBlockHashMap.SetValue(BRREUTF8Lowercase(s),i);
    BRREUnicodeBlockHashMap.SetValue(BRREUTF8Lowercase('In'+s),i);
    BRREUnicodeBlockHashMap.SetValue(BRREUTF8Lowercase('Is'+s),i);
   end;
  end;
 end;
 procedure InitializeFlags;
 begin
  FillChar(BRREWhichFlags,SizeOf(BRREWhichFlags),AnsiChaR(#0));
  BRREWhichFlags[brreoBOML]:=brresfEmptyBeginLine;
  BRREWhichFlags[brreoEOML]:=brresfEmptyEndLine;
  BRREWhichFlags[brreoBOSL]:=brresfEmptyBeginText;
  BRREWhichFlags[brreoEOSL]:=brresfEmptyEndText;
  BRREWhichFlags[brreoBOS]:=brresfEmptyBeginText;
  BRREWhichFlags[brreoEOS]:=brresfEmptyEndText;
  BRREWhichFlags[brreoBRK]:=brresfEmptyWordBoundary;
  BRREWhichFlags[brreoNBRK]:=brresfEmptyNonWordBoundary;
 end;
begin
 if (not BRREInitialized) and (length(BRRESignature)>0) then begin
  BRREInitialized:=true;
{$IFDEF HasJIT}
{$IFDEF unix}
{$IFDEF darwin}
  fpmprotect:=dlsym(dlopen('libc.dylib',RTLD_NOW),'mprotect');
{$ELSE}
  fpmprotect:=dlsym(dlopen('libc.so',RTLD_NOW),'mprotect');
{$ENDIF}
  if not assigned(fpmprotect) then begin
   raise Exception.Create('Importing of mprotect from libc.so failed!');
  end;
{$ENDIF}
{$ENDIF}
  InitializeUTF8DFA;
  InitializeUnicode;
  InitializeFlags;
 end;
end;

procedure FinalizeBRRE;
var i:longint;
begin
 if BRREInitialized then begin
  for i:=low(TBRREUnicodeCharRangeClasses) to high(TBRREUnicodeCharRangeClasses) do begin
   SetLength(BRREUnicodeCharRangeClasses[i],0);
  end;
  FreeAndNil(BRREUnicodeClassHashMap);
  FreeAndNil(BRREUnicodeScriptHashMap);
  FreeAndNil(BRREUnicodeBlockHashMap);
  BRREInitialized:=false;
 end;
end;

initialization
 InitializeBRRE;
finalization
 FinalizeBRRE;
end.
