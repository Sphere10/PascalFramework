unit fMain;

{$I pf.inc}

interface

uses
  SynSQLite3Static, SysUtils, Classes, Forms, Controls, Graphics, Dialogs,
  PF.VisualGrid, SynCommons, mORMot, StdCtrls;

type
  TForm1 = class(TForm)
    VisualGrid1: TVisualGrid;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

end.
