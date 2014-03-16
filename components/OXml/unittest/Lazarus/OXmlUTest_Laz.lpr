program OXmlUTest_Laz;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, OXmlUnitTests, OXmlXPath, OBufferedStreams, ODictionary,
  OEncoding, OHashedStrings, OTextReadWrite, OWideSupp, OXmlLng,
  OXmlReadWrite, OXmlSAX, OXmlSeq, OXmlUtils, OXmlPDOM;

var
  xTest: TOXmlUnitTest;
  xStrL: TStringList;
  I: Integer;
begin
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
