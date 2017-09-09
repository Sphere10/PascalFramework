unit PF.VisualGrid;

{$I pf.inc}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Controls, Grids, ComCtrls, Types
  {$IFNDEF FPC}
  ,Tabs
  {$ENDIF};

type
  TSortDirection = (sdNone, sdAscending, sdDescending);
  TFilterType = (ftMatchTextExact, ftMatchTextBeginning, ftMatchTextEnd,
    ftMatchTextAnywhere, ftNumericEQ, ftNumericLT, ftNumericLTE, ftNumericGT,
    ftNumericGTE, ftNumericBetweenInclusive, ftNumericBetweenExclusive);

  TColumnFilter = record
    ColumnName: utf8string;
    Sort: TSortDirection;
    Filter: TFilterType;
    FilterText: utf8string;
    FilterNumeric1: Int64;
    FilterNumeric2: Int64;
  end;

  TFilterCriteria = TArray<TColumnFilter>;

  TDataTable = record
  public
    //Name : utf8string;
    Columns: TArray<utf8string>;
    Rows : TArray<Variant>;
  end;

  IDataSource = interface
    function FetchPage(APageNumber, APageSize : Integer; out APageCount : Integer; AFilter: TFilterCriteria): TDataTable;
  end;

  TDrawVisualCellEvent = procedure(Sender: TObject; ACol, ARow: Longint;
    Rect: TRect; State: TGridDrawState; const RowData: Variant; var Handled: boolean) of object;

  TCustomVisualGrid = class(TWinControl)
  protected { component interface part }
    FSearchLabel: TLabel;
    FSearchEdit: TEdit;
    FTopPanel: TPanel;
    FTopPanelRight: TPanel;
    FBottomPanel: TPanel;
    FBottomCenterPanel: TPanel;
    FBottomRightPanel: TPanel;

    FButtonFirst: TButton;
    FButtonLast: TButton;
    FButtonNext: TButton;
    FButtonPrevious: TButton;

    FCurrentPageEdit: TEdit;
    FAllPagesLabel: TLabel;

    FPageSizeEdit: TEdit;
    FPageSizeLabel: TLabel;
    FAllRecordsCountLabel: TLabel;

    FDrawGrid: TDrawGrid;
  private
    procedure ClickTest(Sender: TObject);
  protected { TComponent }
    procedure Loaded; override;
  protected
    FDataTable: TDataTable;
    FDataSource: IDataSource;

    FColumns: TStrings;
    FOnDrawVisualCell: TDrawVisualCellEvent;

    procedure SetColumns(const Value: TStrings);
    procedure RefreshGrid;
    procedure ReloadColumns;
    procedure SetDataSource(ADataSource: IDataSource);
    procedure StandardDrawCell(Sender: TObject; ACol, ARow: Longint;
    Rect: TRect; State: TGridDrawState);
    procedure DoDrawCell(Sender: TObject; ACol, ARow: Longint;
      Rect: TRect; State: TGridDrawState; const RowData: Variant);

    property OnDrawVisualCell: TDrawVisualCellEvent read FOnDrawVisualCell write FOnDrawVisualCell;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property Columns: TStrings read FColumns write SetColumns;
    property DataSource: IDataSource read FDataSource write SetDataSource;
  end;

  TVisualGrid = class(TCustomVisualGrid)
  published
    property Align;
    property Columns;
    property OnDrawVisualCell;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Framework', [TVisualGrid]);
end;

{ TCustomVisualGrid }

procedure TCustomVisualGrid.ClickTest(Sender: TOBject);
begin
  TButton(Sender).Caption := Format('%dx%d', [FSearchEdit.Left,FSearchEdit.Top]);
end;

constructor TCustomVisualGrid.Create(Owner: TComponent);
begin
  inherited;
  FColumns := TStringList.Create;
  ControlStyle := ControlStyle - [csAcceptsControls];

  FTopPanel := TPanel.Create(Self);
  FTopPanel.ControlStyle := FTopPanel.ControlStyle - [csAcceptsControls];
  FTopPanel.Parent := Self;
  with FTopPanel do
  begin
    Align := alTop;
    BevelOuter := bvNone;
    Height := 40;

    FTopPanelRight := TPanel.Create(Self);
    FTopPanelRight.ControlStyle := FTopPanelRight.ControlStyle - [csAcceptsControls];
    FTopPanelRight.Parent := FTopPanel;
    with FTopPanelRight do
    begin
      BevelOuter := bvNone;
      Align := alRight;
      Height := 40;
      Width := 177;

      FSearchLabel := TLabel.Create(Self);
      FSearchLabel.Parent := FTopPanelRight;
      with FSearchLabel do
      begin
        Left := 7;
        Top := 13;
        Width := 37;
        Height := 13;
        Caption := 'Search:';
      end;

      FSearchEdit := TEdit.Create(Self);
      FSearchEdit.Parent := FTopPanelRight;
      with FSearchEdit do
      begin
        Left := 50;
        Top := 10;
        Width := 121;
        Height := 21;
      end;
    end;
  end;

  FBottomPanel := TPanel.Create(Self);
  FBottomPanel.ControlStyle := FBottomPanel.ControlStyle - [csAcceptsControls];
  FBottomPanel.Parent := Self;
  with FBottomPanel do
  begin
    Align := alBottom;
    BevelOuter := bvNone;
    Height := 40;

    FBottomRightPanel := TPanel.Create(Self);
    FBottomRightPanel.ControlStyle := FBottomRightPanel.ControlStyle - [csAcceptsControls];
    FBottomRightPanel.Parent := FBottomPanel;
    with FBottomRightPanel do
    begin
      Width := 217;
      Height := 40;
      Align := alRight;
      BevelOuter := bvNone;
      FCurrentPageEdit := TEdit.Create(Self);
      FCurrentPageEdit.Parent := FBottomRightPanel;
      with FCurrentPageEdit do
      begin
        Left := 61;
        Top := 10;
        Width := 56;
        Height := 21;
      end;
      FAllPagesLabel := TLabel.Create(Self);
      FAllPagesLabel.Parent := FBottomRightPanel;
      with FAllPagesLabel do
      begin
        Left := 118;
        Top := 13;
        Width := 36;
        Height := 13;
        Caption := '/';
      end;
      FButtonFirst := TButton.Create(Self);
      FButtonFirst.Parent := FBottomRightPanel;
      with FButtonFirst do
      begin
        Left := 8;
        Top := 8;
        Width := 25;
        Height := 25;
        Caption := #9198;
      end;
      FButtonPrevious := TButton.Create(Self);
      FButtonPrevious.Parent := FBottomRightPanel;
      with FButtonPrevious do
      begin
        Left := 32;
        Top := 8;
        Width := 25;
        Height := 25;
        Caption := #9204;
      end;
      FButtonNext := TButton.Create(Self);
      FButtonNext.Parent := FBottomRightPanel;
      with FButtonNext do
      begin
        Left := 160;
        Top := 8;
        Width := 25;
        Height := 25;
        Caption := #9205;
      end;
      FButtonLast := TButton.Create(Self);
      FButtonLast.Parent := FBottomRightPanel;
      with FButtonLast do
      begin
        Left := 184;
        Top := 8;
        Width := 25;
        Height := 25;
        Caption := #9197;
      end;
    end;

    FBottomCenterPanel := TPanel.Create(Self);
    FBottomCenterPanel.ControlStyle := FBottomCenterPanel.ControlStyle - [csAcceptsControls];
    FBottomCenterPanel.Parent := FBottomPanel;
    with FBottomCenterPanel do
    begin
      Align := alClient;
      BevelOuter := bvNone;
      FAllRecordsCountLabel := TLabel.Create(Self);
      FAllRecordsCountLabel.Parent := FBottomCenterPanel;
      with FAllRecordsCountLabel do
      begin
        Left := 7;
        Top := 13;
        Width := 31;
        Height := 13;
        Caption := 'Total: ';
      end;
      FPageSizeLabel := TLabel.Create(Self);
      FPageSizeLabel.Parent := FBottomCenterPanel;
      with FPageSizeLabel do
      begin
        Left := 126;
        Top := 13;
        Width := 31;
        Height := 13;
        Caption := 'Page size:'
      end;
      FPageSizeEdit := TEdit.Create(Self);
      FPageSizeEdit.Parent := FBottomCenterPanel;
      with FPageSizeEdit do
      begin
        Left := 181;
        Top := 10;
        Width := 52;
        Height := 21;
      end;
    end;
  end;

  FDrawGrid := TDrawGrid.Create(Self);
  FDrawGrid.Parent := Self;
  FDrawGrid.Align := alClient;
  FDrawGrid.OnDrawCell := StandardDrawCell;

  with TButton.Create(Self) do
  begin
    Left := 0;
    Top := 0;
    Parent := Self;
    OnClick := ClickTest;
    Caption := 'Test';
  end;
end;

destructor TCustomVisualGrid.Destroy;
begin
  FColumns.Free;
  inherited;
end;

procedure TCustomVisualGrid.DoDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState; const RowData: Variant);
begin
  if ARow = 0 then
    FDrawGrid.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, FColumns[ACol])
  else
    FDrawGrid.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, RowData);

end;

procedure TCustomVisualGrid.Loaded;
begin
  inherited;
  ReloadColumns;
end;

procedure TCustomVisualGrid.RefreshGrid;
begin

end;

procedure TCustomVisualGrid.ReloadColumns;
var
  I: Integer;
begin
  FDrawGrid.ColCount := FColumns.Count;
end;

procedure TCustomVisualGrid.SetColumns(const Value: TStrings);
begin
  FColumns.Assign(Value);
  ReloadColumns;
end;

procedure TCustomVisualGrid.SetDataSource(ADataSource: IDataSource);
var
  LPages: Integer;
  LFilter: TFilterCriteria;
  i: Integer;
begin
  if FDataSource = ADataSource then
    Exit;

  FDataSource := ADataSource;
  FDataTable := FDataSource.FetchPage(1, 100, LPages, LFilter);

  FColumns.Clear;
  for i := 0 to High(FDataTable.Columns) do
    FColumns.Add(FDataTable.Columns[i]);
  ReloadColumns;

  FDrawGrid.RowCount := 100;
end;

procedure TCustomVisualGrid.StandardDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  LHandled: boolean;
  LCellData: Variant;
begin
  LHandled := False;
  if (ARow > 0) and Assigned(FDataSource) then
    LCellData := FDataTable.Rows[ARow]._(ACol);

  if Assigned(FOnDrawVisualCell) then
    FOnDrawVisualCell(Self, ACol, ARow, Rect, State, LCellData, LHandled);
  if not LHandled then
    DoDrawCell(Self, ACol, ARow, Rect, State, LCellData);
end;

end.

