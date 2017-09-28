unit fMain;

{$MODE DELPHI}

interface

uses
  SysUtils, Classes, Forms, Controls, Graphics, Dialogs, Math, PropEdits,
  LCLType, UVisualGrid, StdCtrls, Menus, Types, Grids, ExtCtrls, CheckLst,
  RTTIGrids, UCommon;

type

  { TForm1 }

  TForm1 = class(TForm, IDataSource)
    AddDelayCheckBox: TCheckBox;
    ColumnsAutoFillCheckBox: TCheckBox;
    FirstColumnStretchedCheckBox: TCheckBox;
    bRefresh: TButton;
    GridPanel: TPanel;
    lSelection: TLabel;
    miUpdateCell: TMenuItem;
    miUpdateRow: TMenuItem;
    MenuItem3: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    TIPropertyGrid1: TTIPropertyGrid;
    procedure ColumnsAutoFillCheckBoxChange(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
    procedure FirstColumnStretchedCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miUpdateCellClick(Sender: TObject);
    procedure miUpdateRowClick(Sender: TObject);
  private
    FColumns: TTableColumns;
    FVisualGrid: TVisualGrid;
    { Private declarations }
  private { IDataSource implementation }
    function GetSearchCapabilities: TSearchCapabilities;
    function FetchPage(constref AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
  private
    procedure PreparePopupMenu(Sender: TObject; constref ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
    procedure DrawVisualCell(Sender: TObject; ACol, ARow: Longint;
      Canvas : TCanvas; Rect: TRect; State: TGridDrawState; const RowData: Variant;
      var Handled: boolean);
    procedure Selection(Sender: TObject; constref ASelection: TVisualGridSelection);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

function TForm1.GetSearchCapabilities: TSearchCapabilities;
begin
  Result := TSearchCapabilities.Create(
    TSearchCapability.From('ID', SORTABLE_NUMERIC_FILTER),
    TSearchCapability.From('Name', SORTABLE_TEXT_FILTER));
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
  ADataTable.Columns := FColumns;
  SetLength(ADataTable.Rows, LCount);

  for i := 0 to LCount - 1 do
  begin
    ADataTable.Rows[i] := TTableRow.New(@FColumns);
    ADataTable.Rows[i].ID := i + delta;
    ADataTable.Rows[i].Name := 'name'+inttostr(i + delta);
    ADataTable.Rows[i].Foo := i + delta + 1;
  end;

  if AddDelayCheckBox.Checked then
    Sleep(3000);
end;

procedure TForm1.PreparePopupMenu(Sender: TObject; constref
  ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
begin
  APopupMenu := PopupMenu1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FColumns := TTableColumns.Create('ID', 'Name', 'Foo');
  FVisualGrid := TVisualGrid.Create(Self);
  FVisualGrid.DataSource := Self;
  FVisualGrid.OnDrawVisualCell:=DrawVisualCell;
  FVisualGrid.OnPreparePopupMenu:=PreparePopupMenu;
  FVisualGrid.OnSelection:=Selection;

  GridPanel.AddDockCenter(FVisualGrid);
  TIPropertyGrid1.TIObject := FVisualGrid;
end;

procedure TForm1.miUpdateCellClick(Sender: TObject);
begin
  with FVisualGrid.Selection do
    FVisualGrid.Cells[Col, Row] := 'Test';
end;

procedure TForm1.miUpdateRowClick(Sender: TObject);
var
  i, j: Integer;
  r: variant;
  rd: TTableRowData absolute r;
begin
  with FVisualGrid.Selection do
    for i := 0 to RowCount - 1 do
    begin
      r := FVisualGrid.Rows[i + Row];
      for j := 0 to High(rd.vvalues) do
        rd.vvalues[j] := 'Test';
      FVisualGrid.Rows[i + Row] := r;
    end;
end;

procedure TForm1.bRefreshClick(Sender: TObject);
begin
  TIPropertyGrid1.RefreshPropertyValues;
end;

procedure TForm1.FirstColumnStretchedCheckBoxChange(Sender: TObject);
begin
  FVisualGrid.Columns[0].StretchedToFill:=FirstColumnStretchedCheckBox.Checked;
end;

procedure TForm1.ColumnsAutoFillCheckBoxChange(Sender: TObject);
begin
  if ColumnsAutoFillCheckBox.Checked then
    FVisualGrid.Options := FVisualGrid.Options + [vgoColAutoFill]
  else
    FVisualGrid.Options := FVisualGrid.Options - [vgoColAutoFill];

  TIPropertyGrid1.RefreshPropertyValues;
end;

procedure TForm1.DrawVisualCell(Sender: TObject; ACol,
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

procedure TForm1.Selection(Sender: TObject; constref
  ASelection: TVisualGridSelection);
begin
  lSelection.Caption := Format('Col = %d Row = %d ColCount = %d RowCount = %d ',
    [ASelection.Col, ASelection.Row, ASelection.ColCount, ASelection.RowCount]);
end;

begin
  RegisterPropertyEditor(TypeInfo(TAlign), TVisualGrid, 'Align', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCursor), TVisualGrid, 'Cursor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpContext), TVisualGrid, 'HelpContext', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TVisualGrid, 'HelpKeyword', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpType), TVisualGrid, 'HelpType', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTranslateString), TVisualGrid, 'Hint', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TComponentName), TVisualGrid, 'Name', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(PtrInt), TVisualGrid, 'Tag', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TVisualGrid, 'Left', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TVisualGrid, 'Top', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TVisualGrid, 'Width', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TVisualGrid, 'Height', THiddenPropertyEditor);
end.
