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
    procedure VisualGrid1DrawVisualCell(Sender: TObject; ACol, ARow: Longint;
      Rect: TRect; State: TGridDrawState; const RowData: Variant;
      var Handled: boolean);
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

procedure TForm1.VisualGrid1DrawVisualCell(Sender: TObject; ACol,
  ARow: Longint; Rect: TRect; State: TGridDrawState; const RowData: Variant;
  var Handled: boolean);
var
  LTextStyle: TTextStyle;
begin
  Handled := (ACol = 2) and (ARow > 0);
  if Handled then
  begin
    VisualGrid1.Canvas.Font.Color:=clRed;
    VisualGrid1.Canvas.Font.Style:=[fsBold];
    LTextStyle := VisualGrid1.Canvas.TextStyle;
    LTextStyle.Alignment:=taCenter;
    VisualGrid1.Canvas.TextStyle:=LTextStyle;
    VisualGrid1.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, RowData, LTextStyle);
  end;
end;

end.