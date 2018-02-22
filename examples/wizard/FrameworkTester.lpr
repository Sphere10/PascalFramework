program PascalWizard;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UFRMMain, UFRMWorkArea, UFRMNotifyManyTest, Generics.Defaults,
  UWIZName_Step1A, UWIZName_Step1B, unit1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFRMMain, MainForm);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

