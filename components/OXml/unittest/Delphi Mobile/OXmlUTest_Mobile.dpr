program OXmlUTest_Mobile;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFormTest in 'uFormTest.pas' {Form1},
  OXmlUnitTests in '..\OXmlUnitTests.pas',
  OWideSupp in '..\..\units\OWideSupp.pas',
  OTextReadWrite in '..\..\units\OTextReadWrite.pas',
  OEncoding in '..\..\units\OEncoding.pas',
  OBufferedStreams in '..\..\units\OBufferedStreams.pas',
  ODictionary in '..\..\units\ODictionary.pas',
  OXmlXPath in '..\..\units\OXmlXPath.pas',
  OXmlCDOM in '..\..\units\OXmlCDOM.pas',
  OXmlPDOM in '..\..\units\OXmlPDOM.pas',
  OXmlReadWrite in '..\..\units\OXmlReadWrite.pas',
  OXmlDOMVendor in '..\..\units\OXmlDOMVendor.pas',
  OXmlUtils in '..\..\units\OXmlUtils.pas',
  OXmlLng in '..\..\units\OXmlLng.pas',
  OXmlSeq in '..\..\units\OXmlSeq.pas',
  OHashedStrings in '..\..\units\OHashedStrings.pas',
  OXmlSAX in '..\..\units\OXmlSAX.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
