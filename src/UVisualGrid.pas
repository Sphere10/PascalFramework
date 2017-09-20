unit UVisualGrid;

{$MODE DELPHI}
{.$DEFINE VISUALGRID_DEBUG}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Controls, Grids, Types, Graphics,
  UCommon, Generics.Collections;



type
  TSelectionType = (stNone, stCell, stRow, stMultiRow);
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

  TSearchCapabilities = array of TSearchCapability;

  { IDataSource }

  IDataSource = interface
    function FetchPage(constref AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
    property Capability : TSearchCapabilities;
  end;

  TDrawVisualCellEvent = procedure(Sender: TObject; ACol, ARow: Longint;
    Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const RowData: Variant; var Handled: boolean) of object;

  EVisualGridError = class(Exception);

  { TCustomVisualGrid }

  TCustomVisualGrid = class(TCustomControl)
  protected type
    TUpdateOfVisualGridGUI = set of (updPageIndex, updPageSize);

    { TSearchEdit }

    TSearchEdit = class
    private
      FPanel: TPanel;
      FEdit: TEdit;
      FButton: TButton;

      function GetEditVisible: boolean;
      function GetVisible: boolean;
      procedure SetEditVisible(AValue: boolean);
      procedure SetVisible(AValue: boolean);
    public
      constructor Create(AParent: TWinControl);
      destructor Destroy; override;

      property EditVisible: boolean read GetEditVisible write SetEditVisible;
      property Visible: boolean read GetVisible write SetVisible;
    end;
  protected const
    PAGE_NAVIGATION_FIRST    = 1;
    PAGE_NAVIGATION_PREVIOUS = 2;
    PAGE_NAVIGATION_NEXT     = 3;
    PAGE_NAVIGATION_LAST     = 4;
  protected { component interface part }
    FSearchLabel: TLabel;
    FSearchEdit: TEdit;
    FSearchButton: TButton;
    FMultiSearchToggleBox: TToggleBox;
    FMultiSearchButton: TButton;
    FTopPanel: TPanel;
    FTopPanelMultiSearch: TPanel;
    FTopPanelMultiSearchFixed: TPanel;
    FTopPanelMultiSearchClient: TPanel;
    FTopPanelMultiSearchRight: TPanel;
    FTopPanelRight: TPanel;
    FClientPanel: TPanel;
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
    FDelayedBoundsChangeTimer: TTimer;

    FMultiSearchEdits: TObjectList<TSearchEdit>;
  protected { events for UI }
    procedure StandardDrawCell(Sender: TObject; ACol, ARow: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure PageIndexEditChange(Sender: TObject);
    procedure PageSizeEditChange(Sender: TObject);
    procedure PageNavigationClick(Sender: TObject);
    procedure MultiSearchToggleBoxChange(Sender: TObject);
  private
    FShowAllData: boolean;
    FAutoPageSize: boolean;
    FCanPage: boolean;
    FCanSearch: boolean;
    FSelectionType: TSelectionType;
    function GetCanvas: TCanvas;
    function GetMultiSearch: boolean;
    function GetMultiSearchToggleBox: boolean;
    function GetSingleSearch: boolean;
    procedure SetMultiSearchToggleBox(AValue: boolean);
    procedure SetMultiSearch(AValue: boolean);
    procedure SetShowAllData(AValue: boolean);
    procedure SetAutoPageSize(AValue: boolean);
    procedure SetCanPage(AValue: boolean);
    procedure SetCanSearch(AValue: boolean);
{$IFDEF VISUALGRID_DEBUG}
    procedure ClickTest(Sender: TObject);
{$ENDIF}
    procedure SetPageIndex(const Value: Integer);
    procedure SetPageSize(Value: Integer);
    procedure SetSelectionType(AValue: TSelectionType);
    procedure SetSingleSearch(AValue: boolean);
  protected { TComponent }
    procedure Loaded; override;
  protected { TControl }
    procedure BoundsChanged; override;
    procedure DelayedBoundsChange(Sender: TObject);
  protected
    FGUIUpdates: TUpdateOfVisualGridGUI;
    FDataTable: TDataTable;
    FDataSource: IDataSource;
    FPageSize: Integer;
    FPageIndex: Integer;
    FPageCount: Integer;
    FDefaultDrawGridOptions: TGridOptions;
    FTotalDataCount: Integer;

    FOnDrawVisualCell: TDrawVisualCellEvent;

    procedure RefreshGrid;
    procedure ReloadColumns;
    procedure LayoutChanged;
    function ClientRowCount: Integer;
    procedure HidePageSizeControls(AVisible: boolean);
    procedure HidePageNavigationControls(AVisible: boolean);
    // return true if range is correct
    function CheckRangeForPageSize(var APageSize: Integer): boolean;
    procedure SetDataSource(ADataSource: IDataSource);
    procedure DoDrawCell(Sender: TObject; ACol, ARow: Longint;
      Rect: TRect; State: TGridDrawState; const RowData: Variant);
    procedure RefreshPageIndexInterface;
    procedure RefreshPageIndexData(ARefreshColumns: boolean);
    procedure ResizeSearchEdit(ACol: Integer);
    procedure SetPageIndexEditText(const AStr: utf8string);
    procedure SetPageSizeEditText(const AStr: utf8string);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property DataSource: IDataSource read FDataSource write SetDataSource;
    property PageSize: Integer read FPageSize write SetPageSize default 100;
    property PageIndex: Integer read FPageIndex write SetPageIndex default -1;
    property AutoPageSize: boolean read FAutoPageSize write SetAutoPageSize default false;
    property ShowAllData: boolean read FShowAllData write SetShowAllData default false;

    property CanPage: boolean read FCanPage write SetCanPage default true;
    property CanSearch: boolean read FCanSearch write SetCanSearch default true;
    property MultiSearchToggleBox: boolean read GetMultiSearchToggleBox write SetMultiSearchToggleBox default true;
    property MultiSearch: boolean read GetMultiSearch write SetMultiSearch default true;
    property SingleSearch: boolean read GetSingleSearch write SetSingleSearch default true;
    property Canvas: TCanvas read GetCanvas;
    property SelectionType: TSelectionType read FSelectionType write SetSelectionType;

    property OnDrawVisualCell: TDrawVisualCellEvent read FOnDrawVisualCell write FOnDrawVisualCell;
  end;

  TVisualGrid = class(TCustomVisualGrid)
  published
    property Align;
    property PageSize;
    property AutoPageSize;
    property ShowAllData;
    property CanPage;
    property CanSearch;
    property MultiSearchToggleBox;
    property MultiSearch;
    property SingleSearch;
    property SelectionType;

    property OnDrawVisualCell;
  end;

procedure Register;

implementation

type
  TDrawGridAccess = class(TDrawGrid);
  TScrollBarAccess = class(TScrollBar);

procedure Register;
begin
  RegisterComponents('Pascal Framework', [TVisualGrid]);
end;

{ TCustomVisualGrid.TSearchEdit }

function TCustomVisualGrid.TSearchEdit.GetEditVisible: boolean;
begin
  Result := FEdit.Visible;
end;

function TCustomVisualGrid.TSearchEdit.GetVisible: boolean;
begin
  Result := FPanel.Visible;
end;

procedure TCustomVisualGrid.TSearchEdit.SetEditVisible(AValue: boolean);
begin
  FEdit.Visible:=AValue;
  FButton.Visible:=AValue;
end;

procedure TCustomVisualGrid.TSearchEdit.SetVisible(AValue: boolean);
begin
  FPanel.Visible := AValue;
end;

constructor TCustomVisualGrid.TSearchEdit.Create(AParent: TWinControl);
begin
  FPanel := TPanel.Create(nil);
  FPanel.Parent := AParent;
  FPanel.BevelOuter := bvNone;
  FEdit := TEdit.Create(FPanel);
  FPanel.Height:=FEdit.Height;
  FEdit.Parent := FPanel;
  FEdit.Align:=alClient;
  {FButton := TButton.Create(FPanel);
  FButton.Width:=25;
  FButton.Parent := FPanel;
  FButton.Align:=alRight;}
end;

destructor TCustomVisualGrid.TSearchEdit.Destroy;
begin
  FPanel.Free;
  inherited Destroy;
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

  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  with FTopPanel do
  begin
    Align := alTop;
    BevelOuter := bvNone;
    Height := 40;

    FMultiSearchToggleBox := TToggleBox.Create(Self);
    FMultiSearchToggleBox.Parent := FTopPanel;
    with FMultiSearchToggleBox do
    begin
      Caption := 'Multi-Search';
      State := cbChecked;
      Left := 4;
      Top := 6;
      Height:=28;
      Width := 120;
      OnChange:=MultiSearchToggleBoxChange;
    end;

    FMultiSearchButton := TButton.Create(Self);
    FMultiSearchButton.Parent := FTopPanel;
    with FMultiSearchButton do
    begin
      Left := 126;
      Top := 6;
      Width := 28;
      Height := 28;
      Caption := 'OK';
    end;


    FTopPanelRight := TPanel.Create(Self);
    FTopPanelRight.Parent := FTopPanel;
    with FTopPanelRight do
    begin
      BevelOuter := bvNone;
      Align := alRight;
      Height := 40;
      Width := 200;

      {FSearchLabel := TLabel.Create(Self);
      FSearchLabel.Parent := FTopPanelRight;
      with FSearchLabel do
      begin
        Left := 7;
        Top := 13;
        Width := 37;
        Height := 13;
        Caption := 'Search:';
      end;}

      FSearchEdit := TEdit.Create(Self);
      FSearchEdit.Parent := FTopPanelRight;
      with FSearchEdit do
      begin
        Left := 50;
        Top := 10;
        Width := 121;
        Height := 21;
        TextHint := 'Search';
      end;

      FSearchButton := TButton.Create(Self);
      FSearchButton.Parent := FTopPanelRight;
      with FSearchButton do
      begin
        Left := 173;
        Top := 10;
        Width := 24;
        Height := 23;
        Caption := 'OK';
      end;
    end;
  end;

  FClientPanel := TPanel.Create(Self);
  FClientPanel.Parent := Self;
  with FClientPanel do
  begin
    Align := alClient;
    BevelOuter := bvNone;

    FTopPanelMultiSearch := TPanel.Create(Self);
    FTopPanelMultiSearch.Parent := FClientPanel;
    with FTopPanelMultiSearch do
    begin
      Align:=alTop;
      BevelOuter := bvNone;
      Height := 40;

      FTopPanelMultiSearchFixed := TPanel.Create(Self);
      FTopPanelMultiSearchFixed.Parent := FTopPanelMultiSearch;
      with FTopPanelMultiSearchFixed do
      begin
        Align:=alLeft;
        BevelOuter := bvNone;
        Height := 40;
        Width := 0; // may be usefull for fixed columns
      end;

      FTopPanelMultiSearchClient := TPanel.Create(Self);
      FTopPanelMultiSearchClient.Parent := FTopPanelMultiSearch;
      with FTopPanelMultiSearchClient do
      begin
        Align:=alClient;
        BevelOuter := bvNone;
        Height := 40;
      end;

      {FTopPanelMultiSearchRight := TPanel.Create(Self);
      FTopPanelMultiSearchRight.Parent := FTopPanelMultiSearch;
      with FTopPanelMultiSearchRight do
      begin
        Align:=alRight;
        BevelOuter := bvNone;
        Height := 40;
        Width:=TScrollBarAccess.GetControlClassDefaultSize.cy;
      end;}
    end;


    FDrawGrid := TDrawGrid.Create(Self);
    FDrawGrid.Parent := FClientPanel;
    with FDrawGrid do
    begin
      Align := alClient;
      BorderStyle := bsNone;
      OnDrawCell := StandardDrawCell;
      Options := (Options - [goRangeSelect]) + [goColSizing];
      FixedCols := 0;
    end;
    FDefaultDrawGridOptions := FDrawGrid.Options;
  end;

  FDelayedBoundsChangeTimer := TTimer.Create(Self);
  with FDelayedBoundsChangeTimer do
  begin
    Enabled:=false;
    Interval:=10;
    OnTimer:=DelayedBoundsChange;
  end;

  { default values for properties }
  PageSize := 100;
  PageIndex := -1;
  FCanPage := true;
  FCanSearch := true;
  FTotalDataCount := -1;

  FMultiSearchEdits := TObjectList<TSearchEdit>.Create;

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
  FMultiSearchEdits.Free;
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

procedure TCustomVisualGrid.BoundsChanged;
begin
  inherited BoundsChanged;
  // fix maximize form problem for AutoPageSize (new size of grid is not yet fully propagated)
  FDelayedBoundsChangeTimer.Enabled:=true;
end;

procedure TCustomVisualGrid.DelayedBoundsChange(Sender: TObject);
begin
  FDelayedBoundsChangeTimer.Enabled:=false;
  if AutoPageSize then
    PageSize := ClientRowCount;
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
  if not CheckRangeForPageSize(LPageSize) then
    SetPageSizeEditText(IntToStr(LPageSize));
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

procedure TCustomVisualGrid.MultiSearchToggleBoxChange(Sender: TObject);
begin
   FTopPanelMultiSearch.Visible := FMultiSearchToggleBox.State=cbChecked;
   FMultiSearchButton.Visible := FMultiSearchToggleBox.Visible and (FMultiSearchToggleBox.State=cbChecked);
   LayoutChanged;
end;

function TCustomVisualGrid.GetCanvas: TCanvas;
begin
  Result := FDrawGrid.Canvas;
end;

function TCustomVisualGrid.GetMultiSearch: boolean;
begin
  Result := FMultiSearchToggleBox.State=cbChecked;
end;

function TCustomVisualGrid.GetMultiSearchToggleBox: boolean;
begin
  Result := FMultiSearchToggleBox.Visible;
end;

function TCustomVisualGrid.GetSingleSearch: boolean;
begin
  Result := FTopPanelRight.Visible;
end;

procedure TCustomVisualGrid.SetMultiSearchToggleBox(AValue: boolean);
begin
  if FMultiSearchToggleBox.Visible=AValue then
    Exit;
  FMultiSearchToggleBox.Visible:=AValue;
  FMultiSearchButton.Visible:=AValue;

  CanSearch:=AValue or SingleSearch;
end;

procedure TCustomVisualGrid.SetMultiSearch(AValue: boolean);
begin
  if (AValue and (FMultiSearchToggleBox.State=cbChecked)) or
     (not AValue and (FMultiSearchToggleBox.State=cbUnchecked)) then
    Exit;

  if AValue then
    FMultiSearchToggleBox.State:=cbChecked
  else
    FMultiSearchToggleBox.State:=cbUnchecked;

  MultiSearchToggleBoxChange(Self);
end;

procedure TCustomVisualGrid.SetShowAllData(AValue: boolean);
begin
  if FShowAllData=AValue then
    Exit;

  FShowAllData:=AValue;
  if FShowAllData then
    AutoPageSize:=false;

  HidePageSizeControls(not AValue);
  HidePageNavigationControls(not AValue);
  PageSize:=FTotalDataCount;
end;

procedure TCustomVisualGrid.SetAutoPageSize(AValue: boolean);

begin
  if FAutoPageSize=AValue then
    Exit;

  FAutoPageSize:=AValue;
  if FAutoPageSize then
    ShowAllData:=false;

  HidePageSizeControls(not FAutoPageSize);

  if FAutoPageSize then
    FDrawGrid.ScrollBars:=ssNone
  else
    FDrawGrid.ScrollBars:=ssAutoBoth;

  PageSize := ClientRowCount;
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

  LayoutChanged;
end;

procedure TCustomVisualGrid.SetCanSearch(AValue: boolean);
begin
  if FCanSearch=AValue then
    Exit;

  FCanSearch:=AValue;
  FTopPanel.Visible:=FCanSearch;
  if csDesigning in ComponentState then
    if FTopPanel.Visible then
      FTopPanel.ControlStyle := FTopPanel.ControlStyle - [csNoDesignVisible]
    else
      FTopPanel.ControlStyle := FTopPanel.ControlStyle + [csNoDesignVisible];

  LayoutChanged;
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

    if LResult.TotalDataCount >= 0 then
      FTotalDataCount := LResult.TotalDataCount
    else
      FTotalDataCount := -1;

    FAllRecordsCountLabel.Visible := FTotalDataCount<>-1;
    FAllRecordsCountLabel.Caption:=Format('Total: %d', [FTotalDataCount]);

    FPageIndex := LResult.PageIndex;

    if ARefreshColumns then
      ReloadColumns;
    FDrawGrid.RowCount := Length(FDataTable.Rows) + 1;
  end;
  RefreshPageIndexInterface;
  FDrawGrid.Refresh;
end;

procedure TCustomVisualGrid.ResizeSearchEdit(ACol: Integer);
var
  LEdit: TSearchEdit;
  LEditOnLeft, LEditOnRight: TSearchEdit;
  //LFixedRect: TRect;
  LRect: TRect;
begin
  LEdit := FMultiSearchEdits[ACol];
  LEdit.Visible := FDrawGrid.IsFixedCellVisible(aCol, 0);
  if ACol > 0 then
    LEditOnLeft := FMultiSearchEdits[ACol-1]
  else
    LEditOnLeft := nil;

  if ACol < FMultiSearchEdits.Count-1 then
    LEditOnRight := FMultiSearchEdits[ACol+1]
  else
    LEditOnRight := nil;

  if ACol = TDrawGridAccess(FDrawGrid).GCache.VisibleGrid.Right then
    if Assigned(LEditOnRight) then
      LEditOnRight.Visible:=false;
  if ACol = TDrawGridAccess(FDrawGrid).GCache.VisibleGrid.Left then
    begin
      if Assigned(LEditOnLeft) then
        if (ACol > FDrawGrid.FixedCols) or (not FDrawGrid.IsFixedCellVisible(ACol-1,0)) then
          LEditOnLeft.Visible:=false
    end;
  // TODO : next column after fixed column
  {if (ACol > 0) and (ACol = FDrawGrid.FixedCols) then
  begin
    fr := FDrawGrid.CellRect(aCol-1, aRow);

    e.SetBounds(FDrawGrid.Left + fr.Right + 2, FDrawGrid.Top - e.Height, (aRect.Right - (fr.Right)) -1, e.Height);
  end
  else}
  LRect := FDrawGrid.CellRect(ACol,0);
  LEdit.FPanel.SetBounds(LRect.Left + 1, 0, LRect.Width - 2, LEdit.FEdit.Height);
end;


procedure TCustomVisualGrid.RefreshPageIndexInterface;
begin
  SetPageIndexEditText(IntToStr(Succ(FPageIndex)));
  FPageCountLabel.Caption := Format('/%d',[FPageCount]);
end;

procedure TCustomVisualGrid.ReloadColumns;
var
  i: Integer;
begin
  FDrawGrid.ColCount := Length(FDataTable.Columns);
  // TODO: may be optimized
  FMultiSearchEdits.Clear;
  for i := 0 to FDrawGrid.ColCount - 1 do
  begin
    FMultiSearchEdits.Add(TSearchEdit.Create(FTopPanelMultiSearchClient));
    ResizeSearchEdit(i);
  end;
  if FDrawGrid.ColCount > 0 then
    FTopPanelMultiSearch.Height:=FMultiSearchEdits.Last.FPanel.Height;
end;

procedure TCustomVisualGrid.LayoutChanged;
begin
  // layout has changed, maybe more space is available
  if AutoPageSize then
    PageSize := ClientRowCount;
end;

function TCustomVisualGrid.ClientRowCount: Integer;
begin
  Result := ((FDrawGrid.ClientHeight - FDrawGrid.GridLineWidth) div FDrawGrid.DefaultRowHeight) - FDrawGrid.FixedRows;
  if Result = 0 then
    Result := 1;
  FDrawGrid.VisibleRowCount;
end;

procedure TCustomVisualGrid.HidePageSizeControls(AVisible: boolean);
begin
  FPageSizeEdit.Visible:=AVisible;
  FPageSizeLabel.Visible:=AVisible;
end;

procedure TCustomVisualGrid.HidePageNavigationControls(AVisible: boolean);
begin
  FBottomRightPanel.Visible:=AVisible;
end;

function TCustomVisualGrid.CheckRangeForPageSize(var APageSize: Integer
  ): boolean;
begin
  if APageSize <= 0 then
  begin
    APageSize:=FPageSize;
    Exit(False);
  end
  else if APageSize > 1000000 then
  begin
    APageSize:=1000000;
    Exit(False);
  end;
  Result := True;
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

procedure TCustomVisualGrid.SetPageSize(Value: Integer);
begin
  if FPageSize = Value then
    Exit;

  CheckRangeForPageSize(Value);
  FPageSize := Value;
  SetPageSizeEditText(IntToStr(FPageSize));
  RefreshPageIndexData(false);
end;

procedure TCustomVisualGrid.SetSelectionType(AValue: TSelectionType);
begin
  if FSelectionType=AValue then
    Exit;

  FSelectionType:=AValue;
  case FSelectionType of
    stNone: FDrawGrid.Options:=FDefaultDrawGridOptions;
    stCell: FDrawGrid.Options:=FDefaultDrawGridOptions+[goDrawFocusSelected];
    stRow: FDrawGrid.Options:=FDefaultDrawGridOptions+[goRowSelect];
    stMultiRow: FDrawGrid.Options:=FDefaultDrawGridOptions+[goRowSelect,goRangeSelect];
  end;
end;

procedure TCustomVisualGrid.SetSingleSearch(AValue: boolean);
begin
  if AValue = FTopPanelRight.Visible then
    exit;
  FTopPanelRight.Visible := AValue;
  CanSearch:=AValue or MultiSearchToggleBox;
end;

procedure TCustomVisualGrid.StandardDrawCell(Sender: TObject; ACol,
  ARow: Longint; Rect: TRect; State: TGridDrawState);
var
  LHandled: boolean;
  LCellData: Variant;
begin
  LHandled := False;
  if ARow = 0 then
    ResizeSearchEdit(ACol);
  if (ARow > 0) and Assigned(FDataSource) then
    LCellData := FDataTable.Rows[ARow-1]._(ACol);

  if Assigned(FOnDrawVisualCell) then
    FOnDrawVisualCell(Self, ACol, ARow, Canvas, Rect, State, LCellData, LHandled);
  if not LHandled then
    DoDrawCell(Self, ACol, ARow, Rect, State, LCellData);
end;

end.

