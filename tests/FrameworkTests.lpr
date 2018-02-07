program FrameworkTests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, UCacheTests, Generics.Collections,
  generics.defaults, generics.hashes, generics.helpers,
  generics.memoryexpanders, generics.strings;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

