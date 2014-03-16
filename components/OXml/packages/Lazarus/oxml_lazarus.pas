{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit oxml_lazarus;

interface

uses
  OBufferedStreams, ODictionary, OEncoding, OHashedStrings, OTextReadWrite, 
  OWideSupp, OXmlCDOM, OXmlDOMVendor, OXmlLng, OXmlPDOM, OXmlReadWrite, 
  OXmlSAX, OXmlSeq, OXmlUtils, OXmlXPath, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('oxml_lazarus', @Register);
end.
