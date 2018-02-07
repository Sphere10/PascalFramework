program PascalWizard;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UFRMMain, UFRMWorkArea, UFRMNotifyManyTest, Generics.Defaults, UCache,
  UWIZName_Step1A, UWIZName_Step1B;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFRMMain, MainForm);
  Application.Run;
end.

