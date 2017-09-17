unit UVisualGrid;

{$MODE DELPHI}
{.$DEFINE VISUALGRID_DEBUG}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Controls, Grids, Types, Graphics,
  UCommon;



type
  TSortDirection = (sdNone, sdAscending, sdDescending);
  TVisualGridFilter = (vgfMatchTextExact, vgfMatchTextBeginning, vgfMatchTextEnd,
    vgfMatchTextAnywhere, vgfNumericEQ, vgfNumericLT, vgfNumericLTE, vgfNumericGT,
    vgfNumericGTE, vgfNumericBetweenInclusive, vgfNumericBetweenExclusive, vgfSortable);
  TVisualGridFilters = set of TVisualGridFilter;

const
  TEXT_FILTER = [vgfMatchTextExact, vgfMatchTextBeginning, vgfMatchTextEnd, vgfMatchTextAnywhere];
  NUMERIC_FILTER = [vgfNumericEQ, vgfNumericLT, vgfNumericLTE, vgfNumericGT, vgfNumericGTE, vgfNumericBetweenInclusive, vgfNumericBetweenExclusive];
  SORTABLE_TEXT_FILTER = [vgfMatchTextExact, vgfMatchTextBeginning, vgfMatchTextEnd, vgfMatchTextAnywhere, vgfSortable];
  SORTABLE_NUMERIC_FILTER = [vgfNumericEQ, vgfNumericLT, vgfNumericLTE, vgfNumericGT, vgfNumericGTE, vgfNumericBetweenInclusive, vgfNumericBetweenExclusive, vgfSortable];

type
  TColumnFilter = record
    ColumnName: utf8string;
    Sort: TSortDirection;
    Filter: TVisualGridFilters;
    FilterText: utf8string;
    FilterNumeric1: Int64;
    FilterNumeric2: Int64;
  end;

  TFilterCriteria = TArray<TColumnFilter>;

  TDataTable = record
  public
    Columns: TTableColumns;
    Rows : TArray<Variant>;
  end;

  { TPageFetchParams }

  TPageFetchParams = record
    PageIndex: Integer;
    PageSize: Integer;
    Filter: TFilterCriteria;
    constructor Create(AIndex: Integer; ASize: Integer; AFilter: TFilterCriteria);
  end;

  { TPageFetchResult }

  TPageFetchResult = record
    PageIndex: Integer;
    PageCount: Integer;
    TotalDataCount: Integer;
  end;

  { TSearchCapability }

  TSearchCapability = record
    ColumnName : utf8string;
    SupportedFilters : TVisualGridFilters;
    class function From(const AName : utf8string; AFilterType : TVisualGridFilters) : TSearchCapability; static;
  end;

  { IDataSource }

  IDataSource = interface
    function FetchPage(constref AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
    property Capability : TArray<TSearchCapability>;
  end;

  TDrawVisualCellEvent = procedure(Sender: TObject; ACol, ARow: Longint;
    Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const RowData: Variant; var Handled: boolean) of object;

  EVisualGridError = class(Exception);

  { TCustomVisualGrid }

  TCustomVisualGrid = class(TWinControl)
  protected type
    TUpdateOfVisualGridGUI = set of (updPageIndex, updPageSize);
  protected const
    PAGE_NAVIGATION_FIRST    = 1;
    PAGE_NAVIGATION_PREVIOUS = 2;
    PAGE_NAVIGATION_NEXT     = 3;
    PAGE_NAVIGATION_LAST     = 4;
  protected { component interface part }
    FSearchLabel: TLabel;
    FSearchEdit: TEdit;
    FTopPanel: TPanel;
    FTopPanelRight: TPanel;
    //FClientPanel: TPanel;
    FBottomPanel: TPanel;
    FBottomCenterPanel: TPanel;
    FBottomRightPanel: TPanel;

    FButtonFirst: TButton;
    FButtonLast: TButton;
    FButtonNext: TButton;
    FButtonPrevious: TButton;

    FPageIndexEdit: TEdit;
    FPageCountLabel: TLabel;

    FPageSizeEdit: TEdit;
    FPageSizeLabel: TLabel;
    FAllRecordsCountLabel: TLabel;

    FDrawGrid: TDrawGrid;
  protected { events for UI }
    procedure StandardDrawCell(Sender: TObject; ACol, ARow: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure PageIndexEditChange(Sender: TObject);
    procedure PageSizeEditChange(Sender: TObject);
    procedure PageNavigationClick(Sender: TObject);
  private
    FCanPage: boolean;
    function GetCanvas: TCanvas;
    procedure SetCanPage(AValue: boolean);
{$IFDEF VISUALGRID_DEBUG}
    procedure ClickTest(Sender: TObject);
{$ENDIF}
    procedure SetPageIndex(const Value: Integer);
    procedure SetPageSize(const Value: Integer);
  protected { TComponent }
    procedure Loaded; override;
  protected
    FGUIUpdates: TUpdateOfVisualGridGUI;
    FDataTable: TDataTable;
    FDataSource: IDataSource;
    FPageSize: Integer;
    FPageIndex: Integer;
    FPageCount: Integer;

    FOnDrawVisualCell: TDrawVisualCellEvent;

    procedure RefreshGrid;
    procedure ReloadColumns;
    procedure SetDataSource(ADataSource: IDataSource);
    procedure DoDrawCell(Sender: TObject; ACol, ARow: Longint;
      Rect: TRect; State: TGridDrawState; const RowData: Variant);
    procedure RefreshPageIndexInterface;
    procedure RefreshPageIndexData(ARefreshColumns: boolean);
    procedure SetPageIndexEditText(const AStr: utf8string);
    procedure SetPageSizeEditText(const AStr: utf8string);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property DataSource: IDataSource read FDataSource write SetDataSource;
    property PageSize: Integer read FPageSize write SetPageSize default 100;
    property PageIndex: Integer read FPageIndex write SetPageIndex default -1;
    property CanPage: boolean read FCanPage write SetCanPage default true;
    property Canvas: TCanvas read GetCanvas;

    property OnDrawVisualCell: TDrawVisualCellEvent read FOnDrawVisualCell write FOnDrawVisualCell;
  end;

  TVisualGrid = class(TCustomVisualGrid)
  published
    property Align;
    property PageSize;
    property CanPage;

    property OnDrawVisualCell;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Framework', [TVisualGrid]);
end;

{ TPageFetchParams }

constructor TPageFetchParams.Create(AIndex: Integer; ASize: Integer;
  AFilter: TFilterCriteria);
begin
  PageIndex:= AIndex;
  PageSize:=ASize;
  Filter:=AFilter;
end;

{ TSearchCapability }

class function TSearchCapability.From(const AName : utf8string; AFilterType : TVisualGridFilters) : TSearchCapability;
begin
  Result.ColumnName := AName;
  Result.SupportedFilters := AFilterType;
end;

{ TCustomVisualGrid }

{$IFDEF VISUALGRID_DEBUG}
procedure TCustomVisualGrid.ClickTest(Sender: TOBject);
begin
  TButton(Sender).Caption := Format('%dx%d', [FSearchEdit.Left,FSearchEdit.Top]);
end;
{$ENDIF}

constructor TCustomVisualGrid.Create(Owner: TComponent);
begin
  inherited;

  { component layout }

  ControlStyle := ControlStyle - [csAcceptsControls] + [csOwnedChildrenNotSelectable];

  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  with FTopPanel do
  begin
    Align := alTop;
    BevelOuter := bvNone;
    Height := 40;

    FTopPanelRight := TPanel.Create(Self);
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
  FBottomPanel.Parent := Self;
  with FBottomPanel do
  begin
    Align := alBottom;
    BevelOuter := bvNone;
    Height := 40;

    FBottomRightPanel := TPanel.Create(Self);
    FBottomRightPanel.Parent := FBottomPanel;
    with FBottomRightPanel do
    begin
      Width := 217;
      Height := 40;
      Align := alRight;
      BevelOuter := bvNone;
      FPageIndexEdit := TEdit.Create(Self);
      FPageIndexEdit.Parent := FBottomRightPanel;
      with FPageIndexEdit do
      begin
        Left := 61;
        Top := 10;
        Width := 56;
        Height := 21;
        OnChange := PageIndexEditChange;
      end;
      FPageCountLabel := TLabel.Create(Self);
      FPageCountLabel.Parent := FBottomRightPanel;
      with FPageCountLabel do
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
        Caption := '|<';
        OnClick := PageNavigationClick;
        Tag := PAGE_NAVIGATION_FIRST;
      end;
      FButtonPrevious := TButton.Create(Self);
      FButtonPrevious.Parent := FBottomRightPanel;
      with FButtonPrevious do
      begin
        Left := 32;
        Top := 8;
        Width := 25;
        Height := 25;
        Caption := '<';
        OnClick := PageNavigationClick;
        Tag := PAGE_NAVIGATION_PREVIOUS;
      end;
      FButtonNext := TButton.Create(Self);
      FButtonNext.Parent := FBottomRightPanel;
      with FButtonNext do
      begin
        Left := 160;
        Top := 8;
        Width := 25;
        Height := 25;
        Caption := '>';
        OnClick := PageNavigationClick;
        Tag := PAGE_NAVIGATION_NEXT;
      end;
      FButtonLast := TButton.Create(Self);
      FButtonLast.Parent := FBottomRightPanel;
      with FButtonLast do
      begin
        Left := 184;
        Top := 8;
        Width := 25;
        Height := 25;
        Caption := '>|';
        OnClick := PageNavigationClick;
        Tag := PAGE_NAVIGATION_LAST;
      end;
    end;

    FBottomCenterPanel := TPanel.Create(Self);
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
        Left := 116;
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
        OnChange:=PageSizeEditChange;
      end;
    end;
  end;

  {FClientPanel := TPanel.Create(Self);
  FClientPanel.Parent := Self;
  with FClientPanel do
  begin
    Align := alClient;
    BevelOuter := bvNone;
    Color:=clWhite;}

    FDrawGrid := TDrawGrid.Create(Self);
    FDrawGrid.Parent := Self;
    with FDrawGrid do
    begin
      Align := alClient;
      OnDrawCell := StandardDrawCell;
    end;
  //end;

  { default values for properties }

  PageSize := 100;
  PageIndex := -1;
  FCanPage := true;

  {$IFDEF VISUALGRID_DEBUG}
  with TButton.Create(Self) do
  begin
    Left := 0;
    Top := 0;
    Parent := Self;
    OnClick := ClickTest;
    Caption := 'Test';
  end;
  {$ENDIF}
end;

destructor TCustomVisualGrid.Destroy;
begin
  inherited;
end;

procedure TCustomVisualGrid.DoDrawCell(Sender: TObject; ACol, ARow: Longint;
  Rect: TRect; State: TGridDrawState; const RowData: Variant);
var
  LText: utf8string;
begin
  if ARow = 0 then
  begin
    if ACol < Length(FDataTable.Columns) then
      LText := FDataTable.Columns[ACol]
    else
      raise EVisualGridError.CreateFmt('Improper column index. Max expected is %d but %d found.', [Length(FDataTable.Columns),ACol]);
    FDrawGrid.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, LText)
  end
  else
    FDrawGrid.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, RowData);
end;

procedure TCustomVisualGrid.Loaded;
begin
  inherited;
  ReloadColumns;
end;

procedure TCustomVisualGrid.PageIndexEditChange(Sender: TObject);
var
  LPageIndex: Integer;
begin
  if updPageIndex in FGUIUpdates then
    Exit;
  // value in edit has increased value by 1 (more readable for end user)
  LPageIndex := Pred(StrToIntDef(FPageIndexEdit.Text, Succ(FPageIndex)));
  if (LPageIndex < 0) then
  begin
    LPageIndex := 0;
    SetPageIndexEditText('1');
  end;
  if (LPageIndex > FPageCount-1) then
  begin
    LPageIndex := FPageCount-1;
    SetPageIndexEditText(IntToStr(FPageCount));
  end;

  PageIndex := LPageIndex;
end;

procedure TCustomVisualGrid.PageSizeEditChange(Sender: TObject);
var
  LPageSize: Integer;
begin
  if updPageSize in FGUIUpdates then
    Exit;
  LPageSize:=StrToIntDef(FPageSizeEdit.Text, FPageSize);
  if LPageSize <= 0 then
  begin
    LPageSize:=FPageSize;
    SetPageSizeEditText(IntToStr(FPageSize));
  end
  else if LPageSize > 1000000 then
  begin
    LPageSize:=1000000;
    SetPageSizeEditText('1000000');
  end;
  PageSize:=LPageSize;
end;

procedure TCustomVisualGrid.PageNavigationClick(Sender: TObject);
begin
  if FPageCount = 0 then
    Exit;
  case TButton(Sender).Tag of
    PAGE_NAVIGATION_FIRST: PageIndex := 0;
    PAGE_NAVIGATION_PREVIOUS: PageIndex := PageIndex - 1;
    PAGE_NAVIGATION_NEXT: PageIndex := PageIndex + 1;
    PAGE_NAVIGATION_LAST: PageIndex := FPageCount - 1;
  end;
end;

function TCustomVisualGrid.GetCanvas: TCanvas;
begin
  Result := FDrawGrid.Canvas;
end;

procedure TCustomVisualGrid.SetCanPage(AValue: boolean);
begin
  if FCanPage=AValue then
    Exit;

  FCanPage:=AValue;
  FBottomPanel.Visible:=FCanPage;
  if csDesigning in ComponentState then
    if FBottomPanel.Visible then
      FBottomPanel.ControlStyle := FBottomPanel.ControlStyle - [csNoDesignVisible]
    else
      FBottomPanel.ControlStyle := FBottomPanel.ControlStyle + [csNoDesignVisible];
end;

procedure TCustomVisualGrid.RefreshGrid;
begin

end;

procedure TCustomVisualGrid.RefreshPageIndexData(ARefreshColumns: boolean);
var
  i: Integer;
  LResult: TPageFetchResult;
begin
  if Assigned(FDataSource) then
  begin
    if FPageIndex >= FPageCount then
      FPageIndex := FPageCount - 1;

    LResult := FDataSource.FetchPage(
      TPageFetchParams.Create(FPageIndex, FPageSize, nil), FDataTable);

    FPageCount:=LResult.PageCount;

    FAllRecordsCountLabel.Visible := LResult.TotalDataCount >= 0;
    FAllRecordsCountLabel.Caption:=Format('Total: %d', [LResult.TotalDataCount]);

    FPageIndex := LResult.PageIndex;

    if ARefreshColumns then
      ReloadColumns;
    FDrawGrid.RowCount := Length(FDataTable.Rows) + 1;
  end;
  RefreshPageIndexInterface;
  FDrawGrid.Refresh;
end;

procedure TCustomVisualGrid.RefreshPageIndexInterface;
begin
  SetPageIndexEditText(IntToStr(Succ(FPageIndex)));
  FPageCountLabel.Caption := Format('/%d',[FPageCount]);
end;

procedure TCustomVisualGrid.ReloadColumns;
begin
  FDrawGrid.ColCount := Length(FDataTable.Columns);
end;

procedure TCustomVisualGrid.SetDataSource(ADataSource: IDataSource);
begin
  if FDataSource = ADataSource then
    Exit;

  FDataSource := ADataSource;

  RefreshPageIndexData(true);
end;

procedure TCustomVisualGrid.SetPageIndex(const Value: Integer);
begin
  if FPageIndex = Value then
    Exit;


  FPageIndex := Value;
  RefreshPageIndexData(false)
end;

procedure TCustomVisualGrid.SetPageIndexEditText(const AStr: utf8string);
begin
  Include(FGUIUpdates, updPageIndex);
  FPageIndexEdit.Text := AStr;
  Exclude(FGUIUpdates, updPageIndex);
end;

procedure TCustomVisualGrid.SetPageSizeEditText(const AStr: utf8string);
begin
  Include(FGUIUpdates, updPageSize);
  FPageSizeEdit.Text := AStr;
  Exclude(FGUIUpdates, updPageSize);
end;

procedure TCustomVisualGrid.SetPageSize(const Value: Integer);
begin
  if FPageSize = Value then
    Exit;

  FPageSize := Value;
  SetPageSizeEditText(IntToStr(FPageSize));
  RefreshPageIndexData(false);
end;

procedure TCustomVisualGrid.StandardDrawCell(Sender: TObject; ACol,
  ARow: Longint; Rect: TRect; State: TGridDrawState);
var
  LHandled: boolean;
  LCellData: Variant;
begin
  LHandled := False;
  if (ARow > 0) and Assigned(FDataSource) then
    LCellData := FDataTable.Rows[ARow-1]._(ACol);

  if Assigned(FOnDrawVisualCell) then
    FOnDrawVisualCell(Self, ACol, ARow, Canvas, Rect, State, LCellData, LHandled);
  if not LHandled then
    DoDrawCell(Self, ACol, ARow, Rect, State, LCellData);
end;

end.

