program FrameworkTests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, Generics.Collections, generics.defaults,
  generics.hashes, generics.helpers, generics.memoryexpanders, generics.strings,
  UCommon.Collections, UCompareTests, UFilterTests, UDisposablesTest,
  UAutoPtrTests, UBoxTests, UVariantToolTests, UMiscTests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

