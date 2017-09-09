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

  TForm1 = class(TForm, IDataSource)
    VisualGrid1: TVisualGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FModel: TSQLModel;
    FServer: TSQLRestServerDB;
    FProp: TSQLDBConnectionProperties;

    function FetchPage(APageNumber, APageSize : Integer; out APageCount : Integer; AFilter: TFilterCriteria): TDataTable;
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

function TForm1.FetchPage(APageNumber, APageSize: Integer;
  out APageCount: Integer; AFilter: TFilterCriteria): TDataTable;
var
  LColumnFilter: TColumnFilter;
  LOrderBy: utf8string;
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

  // rather bad approach, but for demo purposes is ok
  // https://stackoverflow.com/questions/14468586/efficient-paging-in-sqlite-with-millions-of-records
  APageCount := FServer.TableRowCount(TSampleData) div APageSize;
  APageCount := ifthen(APageCount = 0, 1, APageCount);

  with FProp.Execute('select * from SampleData' + LOrderBy +
    ' LIMIT ?, ?', [APageSize, APageSize*APageNumber]) do
  begin
    SetLength(Result.Columns, ColumnCount);
    for i := 0 to ColumnCount - 1 do
      Result.Columns[i] := ColumnName(i);

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
