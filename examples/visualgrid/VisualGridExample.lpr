program VisualGridExample;

{$mode objfpc}{$H+}
{.$DEFINE REPORT_MEMORY_LEAKS}
{.$APPTYPE CONSOLE}

uses
  {$IFDEF REPORT_MEMORY_LEAKS}
  heaptrc,
  {$ENDIF}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, fMain;

{$R *.res}

begin
  // report memory leaks
  {$IFDEF REPORT_MEMORY_LEAKS}
  SetHeapTraceOutput('heaptrc.log');
  {$ENDIF}
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

