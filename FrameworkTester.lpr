program PascalWizard;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UFRMMain, UWizard, UFRMWorkArea, UPagedGrid;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFRMMain, MainForm);
  Application.CreateForm(TFRMWorkArea, WorkAreaForm);
  Application.Run;
end.

