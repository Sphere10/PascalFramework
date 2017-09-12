unit fMain;

{$I pf.inc}

interface

uses
  SysUtils, Classes, Forms, Controls, Graphics, Dialogs, Math,
  PF.VisualGrid, StdCtrls, Menus, SynCommons, Types, Grids;

type

  { TForm1 }

  TForm1 = class(TForm, IDataSource)
    VisualGrid1: TVisualGrid;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function FetchPage(constref AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

function TForm1.FetchPage(constref AParams: TPageFetchParams;
  var ADataTable: TDataTable): TPageFetchResult;
var
  i, delta: Integer;
  LCount: Integer;
begin
  Result.TotalDataCount:=10001;
  Result.PageCount:=Result.TotalDataCount div AParams.PageSize;
  if Result.TotalDataCount mod AParams.PageSize <> 0 then
    Inc(Result.PageCount);

  // APageIndex = -1 is acceptable as initial value
  if AParams.PageIndex < -1 then
    raise Exception.CreateFmt('Wrong value for AParams.PageIndex (%d)', [AParams.PageIndex]);

  // page index parametr may be wrong for new value of page size
  Result.PageIndex := ifthen(AParams.PageIndex=-1,0,AParams.PageIndex);
  if AParams.PageIndex > Result.PageCount-1 then
    Result.PageIndex := Result.PageCount-1;

  // for last page
  delta := AParams.PageSize * Result.PageIndex;
  if (delta + AParams.PageSize) > Result.TotalDataCount then
    LCount := Result.TotalDataCount - delta
  else
    LCount := AParams.PageSize;

  // test data
  ADataTable.Columns := TArray<utf8string>.Create('ID', 'Name', 'Foo');
  SetLength(ADataTable.Rows, LCount);

  for i := 0 to LCount - 1 do
    ADataTable.Rows[i] := _JsonFastFmt('{ID:%,Name:?,Foo:%}',
      [i + delta + 1, i + delta],
      ['name'+inttostr(i + delta)]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  VisualGrid1.DataSource := Self;
end;

end.
