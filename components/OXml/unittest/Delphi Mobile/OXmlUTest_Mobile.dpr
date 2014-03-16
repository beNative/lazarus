program OXmlUTest_Mobile;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFormTest in 'uFormTest.pas' {FormTest},
  OXmlUnitTests in '..\OXmlUnitTests.pas',
  OXmlSAX in '..\..\units\OXmlSAX.pas',
  OXmlPDOM in '..\..\units\OXmlPDOM.pas',
  OXmlReadWrite in '..\..\units\OXmlReadWrite.pas',
  OXmlSeq in '..\..\units\OXmlSeq.pas',
  OXmlLng in '..\..\units\OXmlLng.pas',
  OHashedStrings in '..\..\units\OHashedStrings.pas',
  OXmlUtils in '..\..\units\OXmlUtils.pas',
  OWideSupp in '..\..\units\OWideSupp.pas',
  OTextReadWrite in '..\..\units\OTextReadWrite.pas',
  OXmlXPath in '..\..\units\OXmlXPath.pas',
  OEncoding in '..\..\units\OEncoding.pas',
  OBufferedStreams in '..\..\units\OBufferedStreams.pas',
  ODictionary in '..\..\units\ODictionary.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTest, FormTest);
  Application.Run;
end.
