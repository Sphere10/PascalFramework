unit fMain;

{$I pf.inc}

interface

uses
  SynSQLite3Static, SysUtils, Classes, Forms, Controls, Graphics, Dialogs, Math,
  PF.VisualGrid, SynCommons, mORMot, StdCtrls, mORMotSQLite3, SynDBSQLite3, SynDB;

type
  TSampleData = class(TSQLRecord)
  protected
    fName: RawUTF8;
    fFoo: Integer;
  published
    property name: RawUTF8 read fName write fName;
    property foo: Integer read fFoo write fFoo;
  end;

  { TForm1 }

  TForm1 = class(TForm, IDataSource)
    VisualGrid1: TVisualGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FModel: TSQLModel;
    FServer: TSQLRestServerDB;
    FProp: TSQLDBConnectionProperties;

    function FetchPage(APageIndex, APageSize : Integer; out APageCount : Integer; AFilter: TFilterCriteria): TDataTable;
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

function TForm1.FetchPage(APageIndex, APageSize: Integer;
  out APageCount: Integer; AFilter: TFilterCriteria): TDataTable;
var
  LColumnFilter: TColumnFilter;
  LOrderBy: utf8string;
  LRecordsCount, LModulo: integer;
  i: integer;
begin
  if Length(AFilter) > 0 then
  begin
    LOrderBy := ' order by';
    for LColumnFilter in AFilter do
    begin
      LOrderBy := LOrderBy + ' ' + LColumnFilter.ColumnName;
      case LColumnFilter.Sort of
        sdNone: ;
        sdAscending: LOrderBy := LOrderBy + ' asc';
        sdDescending: LOrderBy := LOrderBy + ' desc';
      end;
      LOrderBy := LOrderBy + ',';
    end;
    SetLength(LOrderBy, Length(LOrderBy)-1);
  end;

  // APageIndex = -1 is acceptable as initial value
  if APageIndex < -1 then
    raise Exception.CreateFmt('Wrong value for APageNumber (%d)', [APageIndex]);
  APageIndex := ifthen(APageIndex=-1,0,APageIndex);

  // rather bad approach, but for demo purposes is ok
  // https://stackoverflow.com/questions/14468586/efficient-paging-in-sqlite-with-millions-of-records
  LRecordsCount := FServer.TableRowCount(TSampleData);
  APageCount := LRecordsCount div APageSize;
  LModulo := LRecordsCount mod APageSize;
  if LModulo > 0 then
    Inc(APageCount);

  with FProp.Execute('select * from SampleData' + LOrderBy +
    ' LIMIT ? OFFSET ?', [APageSize, APageSize*APageIndex]) do
  begin
    SetLength(Result.Columns, ColumnCount);
    for i := 0 to ColumnCount - 1 do
      Result.Columns[i] := ColumnName(i);

    if (Succ(APageIndex)=APageCount) and (LModulo<>0) then
      SetLength(Result.Rows, LModulo)
    else
      SetLength(Result.Rows, APageSize);
    i := 0;
    while Step do
    begin
      RowDocVariant(Result.Rows[i]);
      Inc(i);
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
  sd: TSampleData;
begin
  FModel := TSQLModel.Create([TSampleData]);
  FServer := TSQLRestServerDB.Create(FModel, ':memory:');
  FServer.CreateMissingTables;
  FProp := TSQLDBSQLite3ConnectionProperties.Create(FServer.DB);

  with TAutoFree.One(sd, TSampleData.Create) do
  for i := 0 to 10000 do
  begin
    sd.Name := 'name' + inttostr(i);
    sd.Foo := i;
    FServer.Add(sd, true);
  end;

  VisualGrid1.DataSource := Self;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FProp.Free;
  FServer.Free;
  FModel.Free;
end;

end.
