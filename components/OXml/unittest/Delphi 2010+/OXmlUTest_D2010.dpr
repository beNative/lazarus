program OXmlUTest_D2010;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Classes,
  OXmlUnitTests in '..\OXmlUnitTests.pas',
  OBufferedStreams in '..\..\units\OBufferedStreams.pas',
  ODictionary in '..\..\units\ODictionary.pas',
  OEncoding in '..\..\units\OEncoding.pas',
  OHashedStrings in '..\..\units\OHashedStrings.pas',
  OTextReadWrite in '..\..\units\OTextReadWrite.pas',
  OXmlLng in '..\..\units\OXmlLng.pas',
  OXmlReadWrite in '..\..\units\OXmlReadWrite.pas',
  OXmlSAX in '..\..\units\OXmlSAX.pas',
  OXmlSeq in '..\..\units\OXmlSeq.pas',
  OXmlUtils in '..\..\units\OXmlUtils.pas',
  OXmlXPath in '..\..\units\OXmlXPath.pas',
  OXmlPDOM in '..\..\units\OXmlPDOM.pas',
  OXmlCDOM in '..\..\units\OXmlCDOM.pas',
  OXmlRTTISerialize in '..\..\units\OXmlRTTISerialize.pas',
  OXmlSerialize in '..\..\units\OXmlSerialize.pas',
  OWideSupp in '..\..\units\OWideSupp.pas',
  OXmlDOMVendor in '..\..\units\OXmlDOMVendor.pas';

var
  xTest: TOXmlUnitTest;
  xStrL: TStringList;
  I: Integer;
begin
  ReportMemoryLeaksOnShutdown := True;

  xTest := TOXmlUnitTest.Create;
  xStrL := TStringList.Create;
  try
    xTest.OXmlTestAll(xStrL);
    for I := 0 to xStrL.Count-1 do
      Writeln(xStrL[I]);
  finally
    xStrL.Free;
    xTest.Free;
  end;

  Writeln;
  Write('Press enter to close.');
  Readln;
end.
