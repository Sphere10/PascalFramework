unit fMain;

{$I pf.inc}

interface

uses
  SysUtils, Classes, Forms, Controls, Graphics, Dialogs, Math,
  PF.VisualGrid, StdCtrls, Menus, SynCommons;

type

  { TForm1 }

  TForm1 = class(TForm, IDataSource)
    VisualGrid1: TVisualGrid;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    function FetchPage(APageIndex, APageSize : Integer; out APageCount : Integer; AFilter: TFilterCriteria): TDataTable;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

function TForm1.FetchPage(APageIndex, APageSize: Integer;
  out APageCount: Integer; AFilter: TFilterCriteria): TDataTable;
var
  i, delta: integer;
begin
  APageCount:=100;

  // APageIndex = -1 is acceptable as initial value
  if APageIndex < -1 then
    raise Exception.CreateFmt('Wrong value for APageNumber (%d)', [APageIndex]);
  APageIndex := ifthen(APageIndex=-1,0,APageIndex);

  // test data
  Result.Columns := TArray<utf8string>.Create('ID', 'Name', 'Foo');

  SetLength(Result.Rows, APageSize);
  delta := APageSize * APageIndex;
  for i := 0 to APageSize-1 do
    Result.Rows[i] := _JsonFastFmt('{ID:%,Name:?,Foo:%}',[i + delta, i + delta],['name'+inttostr(i + delta)]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  VisualGrid1.DataSource := Self;
end;

end.
