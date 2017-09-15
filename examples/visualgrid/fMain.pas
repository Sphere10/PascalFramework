unit fMain;

{$MODE DELPHI}

interface

uses
  SysUtils, Classes, Forms, Controls, Graphics, Dialogs, Math,
  UVisualGrid, StdCtrls, Menus, SynCommons, Types, Grids, ExtCtrls, UCommon;

type

  { TForm1 }

  TForm1 = class(TForm, IDataSource)
    CanPageCheckBox: TCheckBox;
    CanSearchCheckBox: TCheckBox;
    AlignCheckBox: TCheckBox;
    CanSelectCheckBox: TCheckBox;
    AddDelayCheckBox: TCheckBox;
    GridPanel: TPanel;
    UserPageSizeRadioButton: TRadioButton;
    AutoPageSizeRadioButton: TRadioButton;
    FullPageSizeRadioButton: TRadioButton;
    CellSelectRadioButton: TRadioButton;
    SingleSelectRadioButton: TRadioButton;
    MultiRowSelectRadioButton: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure VisualGrid1DrawVisualCell(Sender: TObject; ACol, ARow: Longint;
      Canvas : TCanvas; Rect: TRect; State: TGridDrawState; const RowData: Variant;
      var Handled: boolean);
  private
    FVisualGrid: TVisualGrid;
    { Private declarations }
    function GetCapabilities : TArray<TSearchCapability>;
    property Capability : TArray<TSearchCapability> read GetCapabilities;
    function FetchPage(constref AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

function TForm1.GetCapabilities : TArray<TSearchCapability>;
begin
  SetLength(Result, 2);
  Result[0] := TSearchCapability.From('ID', SORTABLE_NUMERIC_FILTER);
  Result[1] := TSearchCapability.From('ID', SORTABLE_TEXT_FILTER);
end;

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
  begin
    ADataTable.Rows[i] := TDocVariant.New([dvoReturnNullForUnknownProperty,dvoValueCopiedByReference]);
    ADataTable.Rows[i].ID := i + delta;
    ADataTable.Rows[i].Name := 'name'+inttostr(i + delta);
    ADataTable.Rows[i].Foo := i + delta + 1;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FVisualGrid := TVisualGrid.Create(Self);
  FVisualGrid.DataSource := Self;
  GridPanel.AddDockCenter(FVisualGrid);
end;

procedure TForm1.VisualGrid1DrawVisualCell(Sender: TObject; ACol,
  ARow: Longint; Canvas : TCanvas; Rect: TRect; State: TGridDrawState; const RowData: Variant;
  var Handled: boolean);
var
  LTextStyle: TTextStyle;
begin
  Handled := (ACol = 2) and (ARow > 0);
  if Handled then
  begin
    Canvas.Font.Color:=clRed;
    Canvas.Font.Style:=[fsBold];
    LTextStyle := Canvas.TextStyle;
    LTextStyle.Alignment:=taCenter;
    Canvas.TextStyle:=LTextStyle;
    Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, RowData, LTextStyle);
  end;
end;


end.
