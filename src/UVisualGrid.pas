unit UVisualGrid;

{$MODE DELPHI}
{.$DEFINE VISUALGRID_DEBUG}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Controls, Grids, Types, Graphics,
  UCommon, Generics.Collections, Menus, ComboEx, Buttons, Math;



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

  PDataTable = ^TDataTable;
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

  PSearchCapability = ^TSearchCapability;
  TSearchCapability = record
    ColumnName : utf8string;
    SupportedFilters : TVisualGridFilters;
    class function From(const AName : utf8string; AFilterType : TVisualGridFilters) : TSearchCapability; static;
  end;

  TSearchCapabilities = array of TSearchCapability;

  { IDataSource }

  IDataSource = interface
    function FetchPage(constref AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
    function GetSearchCapabilities: TSearchCapabilities;
    property SearchCapabilities : TSearchCapabilities read GetSearchCapabilities;
  end;

  { TVisualGridSelection }

  TVisualGridSelection = record
  private
    function GetCol: longint;
    function GetColCount: longint;
    function GetRow: longint;
    function GetRowCount: longint;
  public
    Selections: array of TRect;
    property Col: longint read GetCol;
    property Row: longint read GetRow;
    property RowCount: longint read GetRowCount;
    property ColCount: longint read GetColCount;
  end;

  { TColumnOptions }

  TVisualColumn = class
  private
    function GetStretchedToFill: boolean; inline;
    procedure SetStretchedToFill(AValue: boolean); inline;
  protected
    FColumn: TGridColumn;
  public
    constructor Create(AColumn: TGridColumn);
    property StretchedToFill: boolean read GetStretchedToFill write SetStretchedToFill;
  end;

  TPreparePopupMenuEvent = procedure(Sender: TObject; constref ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu) of object;
  TSelectionEvent = procedure(Sender: TObject; constref ASelection: TVisualGridSelection) of object;
  TDrawVisualCellEvent = procedure(Sender: TObject; ACol, ARow: Longint;
    Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const RowData: Variant; var Handled: boolean) of object;

  EVisualGridError = class(Exception);

  TVisualGridOptions = set of (vgoColAutoFill, vgoColSizing);

  { TCustomVisualGrid }

  TCustomVisualGrid = class(TCustomControl)
  protected type
    TUpdateOfVisualGridGUI = set of (updPageIndex, updPageSize);

    TLastFetchDataResult = record
      FromThread: boolean;
      RefreshColumns: boolean;
      FetchResult: TPageFetchResult;
    end;

    { TSearchEdit }

    TSearchEdit = class
    private
      FGrid: TCustomVisualGrid;
      FPanel: TPanel;
      FEdit: TEdit;
      FButton: TButton;

      function GetEditVisible: boolean;
      function GetVisible: boolean;
      procedure SetEditVisible(AValue: boolean);
      procedure SetVisible(AValue: boolean);
    public
      SearchCapability: PSearchCapability;
      constructor Create(AParent: TWinControl; AGrid: TCustomVisualGrid);
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
    FMainPanel: TPanel;
    FCaptionLabel: TLabel;
    FSearchLabel: TLabel;
    FSearchEdit: TEdit;
    FSearchButton: TSpeedButton;
    FMultiSearchCheckComboBox: TCheckComboBox;
    FTopPanel: TPanel;
    FTopPanelMultiSearch: TPanel;
    FTopPanelMultiSearchFixed: TPanel;
    FTopPanelMultiSearchClient: TPanel;
    FTopPanelMultiSearchRight: TPanel;
    FTopPanelRight: TPanel;
    FClientPanel: TPanel;
    FLoadDataPanel: TPanel;
    FLoadDataLabel: TLabel;
    FLoadDataProgressLabel: TLabel;
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
    FFetchDataThreadTimer: TTimer;
    FEditingDoneTimer: TTimer;
    FSearchKindPopupMenu: TPopupMenu;
    FSingleSearchMenuItem: TMenuItem;
    FMultiSearchMenuItem: TMenuItem;

    FMultiSearchEdits: TObjectList<TSearchEdit>;
    FColumns: TObjectList<TVisualColumn>;
  protected { events for UI }
    procedure StandardDrawCell(Sender: TObject; ACol, ARow: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SearchKindPopupMenuClick(Sender: TObject);
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure PageIndexEditingDone(Sender: TObject);
    procedure PageSizeEditingDone(Sender: TObject);
    procedure PageNavigationClick(Sender: TObject);
    procedure MultiSearchCheckComboBoxChange(Sender: TObject; AIndex: Integer);
    procedure DelayedBoundsChange(Sender: TObject);
    procedure FetchDataThreadProgress(Sender: TObject);
  private
    FFetchDataInThread: boolean;
    FOnPreparePopupMenu: TPreparePopupMenuEvent;
    FOnSelection: TSelectionEvent;
    FOptions: TVisualGridOptions;
    FShowAllData: boolean;
    FAutoPageSize: boolean;
    FCanPage: boolean;
    FCanSearch: boolean;
    FSelectionType: TSelectionType;
    function GetCaption: TCaption;
    function GetCaptionAlignment: TAlignment;
    function GetCaptionFont: TFont;
    function GetCells(ACol, ARow: Integer): Variant;
    function GetColCount: Integer; inline;
    function GetColumns(Index: Integer): TVisualColumn;
    function GetRowCount: Integer; inline;
    function GetRows(ARow: Integer): Variant;
    function GetSelection: TVisualGridSelection;
    function GetCaptionVisible: boolean;
    procedure ControlsEnable(AEnable: boolean);
    function GetCanvas: TCanvas;
    function GetSingleSearch: boolean;
    procedure SetCaption(AValue: TCaption);
    procedure SetCaptionAlignment(AValue: TAlignment);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetCaptionVisible(AValue: boolean);
    procedure SetCells(ACol, ARow: Integer; AValue: Variant);
    procedure SetFetchDataInThread(AValue: boolean);
    procedure SetOptions(AValue: TVisualGridOptions);
    procedure SetRows(ARow: Integer; AValue: Variant);
    procedure SetShowAllData(AValue: boolean);
    procedure SetAutoPageSize(AValue: boolean);
    procedure SetCanPage(AValue: boolean);
    procedure SetCanSearch(AValue: boolean);
{$IFDEF VISUALGRID_DEBUG}
    procedure ClickTest(Sender: TObject);
{$ENDIF}
    procedure SetPageIndex(Value: Integer);
    procedure SetPageSize(Value: Integer);
    procedure SetSelectionType(AValue: TSelectionType);
  protected { TComponent }
    procedure Loaded; override;
  protected { TControl }
    procedure BoundsChanged; override;
  protected
    FGUIUpdates: TUpdateOfVisualGridGUI;
    FDataTable: TDataTable;
    FCachedDataTable: PDataTable;
    FDataSource: IDataSource;
    FSearchCapabilities: TSearchCapabilities;
    FPageSize: Integer;
    FPageIndex: Integer;
    FPageCount: Integer;
    FDefaultDrawGridOptions: TGridOptions;
    FTotalDataCount: Integer;
    FLastFetchDataResult: TLastFetchDataResult;

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
    procedure RefreshPageIndexAndGridInterface;
    procedure RefreshPageIndexData(ARefreshColumns: boolean);
    procedure ResizeSearchEdit(ACol: Integer);
    procedure SetPageIndexEditText(const AStr: utf8string);
    procedure SetPageSizeEditText(const AStr: utf8string);
    procedure BeforeFetchPage;
    procedure FetchPage(out AResult: TPageFetchResult);
    procedure AfterFetchPage;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property DataSource: IDataSource read FDataSource write SetDataSource;
    property PageSize: Integer read FPageSize write SetPageSize default 100;
    property PageIndex: Integer read FPageIndex write SetPageIndex default -1;
    property AutoPageSize: boolean read FAutoPageSize write SetAutoPageSize default false;
    property ShowAllData: boolean read FShowAllData write SetShowAllData default false;
    property FetchDataInThread: boolean read FFetchDataInThread write SetFetchDataInThread;

    property CanPage: boolean read FCanPage write SetCanPage default true;
    property CanSearch: boolean read FCanSearch write SetCanSearch default true;
    property Options: TVisualGridOptions read FOptions write SetOptions;
    property Canvas: TCanvas read GetCanvas;
    property SelectionType: TSelectionType read FSelectionType write SetSelectionType;
    property Selection: TVisualGridSelection read GetSelection;

    property Caption: TCaption read GetCaption write SetCaption;
    property CaptionVisible: boolean read GetCaptionVisible write SetCaptionVisible;
    property CaptionFont: TFont read GetCaptionFont write SetCaptionFont;
    property CaptionAlignment: TAlignment read GetCaptionAlignment write SetCaptionAlignment default taLeftJustify;

    property ColCount: Integer read GetColCount;
    property Columns[Index: Integer]: TVisualColumn read GetColumns;
    property Cells[ACol, ARow: Integer]: Variant read GetCells write SetCells;
    property RowCount: Integer read GetRowCount;
    property Rows[ARow: Integer]: Variant read GetRows write SetRows;

    property OnDrawVisualCell: TDrawVisualCellEvent read FOnDrawVisualCell write FOnDrawVisualCell;
    property OnSelection: TSelectionEvent read FOnSelection write FOnSelection;
    property OnPreparePopupMenu: TPreparePopupMenuEvent read FOnPreparePopupMenu write FOnPreparePopupMenu;
  end;

  TVisualGrid = class(TCustomVisualGrid)
  published
    property Caption;
    property CaptionVisible;
    property CaptionFont;
    property CaptionAlignment;

    property Align;
    property PageSize;
    property AutoPageSize;
    property ShowAllData;
    property CanPage;
    property CanSearch;
    property Options;
    property SelectionType;
    property FetchDataInThread;

    property OnDrawVisualCell;
    property OnSelection;
    property OnPreparePopupMenu;
  end;

procedure Register;

implementation

{$R *.rc}

type
  TDrawGridAccess = class(TDrawGrid);
  //TScrollBarAccess = class(TScrollBar);

  { TFetchDataThread }

  TFetchDataThread = class(TThread)
  protected
    FGrid: TCustomVisualGrid;
    FLastFetchDataResult: TCustomVisualGrid.TLastFetchDataResult;
    procedure Execute; override;
  public
    constructor Create(AGrid: TCustomVisualGrid; ARefreshColumns: boolean);
    destructor Destroy; override;
  end;

procedure Register;
begin
  RegisterComponents('Pascal Framework', [TVisualGrid]);
end;

{ TVisualColumn }

function TVisualColumn.GetStretchedToFill: boolean;
begin
  Result := FColumn.SizePriority > 0;
end;

procedure TVisualColumn.SetStretchedToFill(AValue: boolean);
begin
  FColumn.SizePriority := ifthen(AValue, 1);
end;

constructor TVisualColumn.Create(AColumn: TGridColumn);
begin
  FColumn := AColumn;
end;

{ TVisualGridSelection }

function TVisualGridSelection.GetCol: longint;
begin
  if Length(Selections) = 0 then
    Exit(-1);
  Result := Selections[0].Left;
end;

function TVisualGridSelection.GetColCount: longint;
begin
  if Length(Selections) = 0 then
    Exit(0);
  Result := Selections[0].Width + 1;
end;

function TVisualGridSelection.GetRow: longint;
begin
  if Length(Selections) = 0 then
    Exit(-1);
  Result := Selections[0].Top;
end;

function TVisualGridSelection.GetRowCount: longint;
begin
  if Length(Selections) = 0 then
    Exit(0);
  Result := Selections[0].Height + 1;
end;

{ TFetchDataThread }

procedure TFetchDataThread.Execute;
begin
  FGrid.FetchPage(FLastFetchDataResult.FetchResult);
end;

constructor TFetchDataThread.Create(AGrid: TCustomVisualGrid;
  ARefreshColumns: boolean);
begin
  FGrid := AGrid;
  FGrid.ControlsEnable(false);
  FGrid.FFetchDataThreadTimer.Enabled:=true;
  FGrid.FLoadDataPanel.Visible:=True;
  FGrid.FLoadDataPanel.BringToFront;
  FGrid.BeforeFetchPage;
  FreeOnTerminate:=true;
  FLastFetchDataResult.RefreshColumns:=ARefreshColumns;
  // fast copy of data (we need to draw old data for a while)
  New(FGrid.FCachedDataTable);
  Move(FGrid.FDataTable, FGrid.FCachedDataTable^, SizeOf(TDataTable));
  FillChar(FGrid.FDataTable, SizeOf(TDataTable), #0);
  inherited Create(false);
end;

destructor TFetchDataThread.Destroy;
begin
  FLastFetchDataResult.FromThread:=true;
  FGrid.FLastFetchDataResult := FLastFetchDataResult;
  Synchronize(FGrid.AfterFetchPage);
  inherited Destroy;
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
  //FButton.Visible:=AValue;
end;

procedure TCustomVisualGrid.TSearchEdit.SetVisible(AValue: boolean);
begin
  FPanel.Visible := AValue;
end;

constructor TCustomVisualGrid.TSearchEdit.Create(AParent: TWinControl;
  AGrid: TCustomVisualGrid);
begin
  FGrid := AGrid;
  FPanel := TPanel.Create(nil);
  FPanel.Parent := AParent;
  FPanel.BevelOuter := bvNone;
  FEdit := TEdit.Create(FPanel);
  FPanel.Height:=FEdit.Height;
  FEdit.Parent := FPanel;
  FEdit.PopupMenu := FGrid.FSearchKindPopupMenu;
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

  FMultiSearchEdits := TObjectList<TSearchEdit>.Create;
  FColumns := TObjectList<TVisualColumn>.Create;

  { component layout }

  ControlStyle := ControlStyle - [csAcceptsControls] + [csOwnedChildrenNotSelectable];

  FSearchKindPopupMenu := TPopupMenu.Create(Self);
  FSingleSearchMenuItem := TMenuItem.Create(Self);
  FSingleSearchMenuItem.RadioItem:=True;
  FSingleSearchMenuItem.Caption:='Standard Search';
  FSingleSearchMenuItem.OnClick:=SearchKindPopupMenuClick;
  FMultiSearchMenuItem := TMenuItem.Create(Self);
  FMultiSearchMenuItem.RadioItem:=True;
  FMultiSearchMenuItem.Caption:='Multi-Column Search';
  FMultiSearchMenuItem.OnClick:=SearchKindPopupMenuClick;

  FSearchKindPopupMenu.Items.Add([FSingleSearchMenuItem, FMultiSearchMenuItem]);

  FMainPanel := TPanel.Create(Self);
  FMainPanel.Parent := Self;
  with FMainPanel do
  begin
    Align:=alClient;
    BevelOuter := bvNone;
  end;

  FBottomPanel := TPanel.Create(Self);
  FBottomPanel.Parent := FMainPanel;
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
        OnEditingDone := PageIndexEditingDone;
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
        OnEditingDone:=PageSizeEditingDone;
      end;
    end;
  end;

  FCaptionLabel := TLabel.Create(Self);
  FCaptionLabel.Parent := Self;
  with FCaptionLabel do
  begin
    Align := alTop;
    Visible := false;
  end;

  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := FMainPanel;
  with FTopPanel do
  begin
    Align := alTop;
    BevelOuter := bvNone;
    Height := 36;

    FMultiSearchCheckComboBox := TCheckComboBox.Create(Self);
    FMultiSearchCheckComboBox.Parent := FTopPanel;
    with FMultiSearchCheckComboBox do
    begin
      Left := 4;
      Top := 6;
      Height:=28;
      Width := 120;
      OnItemChange:=MultiSearchCheckComboBoxChange;
      PopupMenu:=FSearchKindPopupMenu;
    end;

    FSearchEdit := TEdit.Create(Self);
    FSearchEdit.Parent := FTopPanel;
    with FSearchEdit do
    begin
      AnchorSideLeft.Control := FMultiSearchCheckComboBox;
      AnchorSideLeft.Side := asrBottom;
      AnchorSideTop.Control := FMultiSearchCheckComboBox;
      AnchorSideBottom.Control := FMultiSearchCheckComboBox;
      AnchorSideBottom.Side := asrBottom;
      Anchors := [akTop, akLeft, akBottom];
      BorderSpacing.Left := 2;
      Left := 50;
      Top := 6;
      Width := 121;
      Height := 21;
      TextHint := 'Search expression';
      PopupMenu:=FSearchKindPopupMenu;
    end;

    FSearchButton := TSpeedButton.Create(Self);
    FSearchButton.Parent := FTopPanel;
    FSearchButton.LoadGlyphFromResourceName(HINSTANCE, 'VISUALGRID_SEARCH');
    with FSearchButton do
    begin
      AnchorSideLeft.Control := FSearchEdit;
      AnchorSideLeft.Side := asrBottom;
      AnchorSideTop.Control := FMultiSearchCheckComboBox;
      AnchorSideBottom.Control := FMultiSearchCheckComboBox;
      AnchorSideBottom.Side := asrBottom;
      Anchors := [akTop, akLeft, akBottom];
      BorderSpacing.Left := 2;
      Left := 173;
      Top := 6;
      Width := 23;
      Height := 23;
      PopupMenu:=FSearchKindPopupMenu;
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
    end;
  end;

  FClientPanel := TPanel.Create(Self);
  FClientPanel.Parent := FMainPanel;
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
      OnMouseUp := GridMouseUp;
      OnSelection := GridSelection;
      Options := (Options - [goRangeSelect]);
      FixedCols := 0;
      ColCount:=0;
      RowCount:=0;
    end;
    FDefaultDrawGridOptions := FDrawGrid.Options;
  end;

  FLoadDataPanel := TPanel.Create(Self);
  FLoadDataPanel.Parent := Self;
  with FLoadDataPanel do
  begin
    //BevelOuter := bvNone;
    Width := 300;
    Height := 150;
    //Align:=alClient;
    Visible:=false;
    AnchorSideLeft.Control := Self;
    AnchorSideLeft.Side := asrCenter;
    AnchorSideTop.Control := Self;
    AnchorSideTop.Side := asrCenter;

    FLoadDataLabel := TLabel.Create(Self);
    FLoadDataLabel.Parent := FLoadDataPanel;
    with FLoadDataLabel do
    begin
      AnchorSideLeft.Control := FLoadDataPanel;
      AnchorSideLeft.Side := asrCenter;
      AnchorSideTop.Control := FLoadDataPanel;
      AnchorSideTop.Side := asrCenter;
      Caption := 'DATA LOADING';
    end;
    FLoadDataProgressLabel := TLabel.Create(Self);
    FLoadDataProgressLabel.Parent := FLoadDataPanel;
    with FLoadDataProgressLabel do
    begin
      AnchorSideLeft.Control := FLoadDataLabel;
      AnchorSideLeft.Side := asrCenter;
      AnchorSideTop.Control := FLoadDataLabel;
      AnchorSideTop.Side := asrBottom;
      Caption := '-';
    end
  end;

  FDelayedBoundsChangeTimer := TTimer.Create(Self);
  with FDelayedBoundsChangeTimer do
  begin
    Enabled:=false;
    Interval:=20;
    OnTimer:=DelayedBoundsChange;
  end;

  FFetchDataThreadTimer := TTimer.Create(Self);
  with FFetchDataThreadTimer do
  begin
    Enabled:=false;
    Interval:=250;
    OnTimer:=FetchDataThreadProgress;
  end;

  FEditingDoneTimer := TTimer.Create(Self);
  with FEditingDoneTimer do
  begin
    Enabled:=false;
    Interval:=2000;
  end;

  { default values for properties }
  PageSize := 100;
  PageIndex := -1;
  FCanPage := true;
  FCanSearch := true;
  FTotalDataCount := -1;

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
  FColumns.Free;
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

procedure TCustomVisualGrid.FetchDataThreadProgress(Sender: TObject);
{$J+}
const
  PROGRESS: Integer = 0;
{$J-}
  PROGRESS_CHARS: array[0..3] of string = ('-', '\', '|', '/');
begin
  FLoadDataProgressLabel.Caption := PROGRESS_CHARS[PROGRESS];
  Inc(PROGRESS);
  if PROGRESS > High(PROGRESS_CHARS) then
    PROGRESS := 0;
end;

procedure TCustomVisualGrid.ControlsEnable(AEnable: boolean);
var
  e: TSearchEdit;
  LReadOnly: boolean;
begin
  LReadOnly:=not AEnable;
  FMultiSearchCheckComboBox.Enabled:=AEnable;
  FSearchEdit.ReadOnly:=LReadOnly;
  FPageSizeEdit.ReadOnly:=LReadOnly;
  FPageIndexEdit.ReadOnly:=LReadOnly;
  FDrawGrid.Enabled:=AEnable;
  for e in FMultiSearchEdits do
    e.FEdit.ReadOnly:=LReadOnly;
end;

function TCustomVisualGrid.GetCells(ACol, ARow: Integer): Variant;
begin
  Result := FDataTable.Rows[ARow]._(ACol);
end;

function TCustomVisualGrid.GetColCount: Integer;
begin
  Result := FColumns.Count;
end;

function TCustomVisualGrid.GetColumns(Index: Integer): TVisualColumn;
begin
  Result := FColumns[Index];
end;

function TCustomVisualGrid.GetRowCount: Integer;
begin
  Result := Length(FDataTable.Rows);
end;

function TCustomVisualGrid.GetCaption: TCaption;
begin
  Result := FCaptionLabel.Caption;
end;

function TCustomVisualGrid.GetCaptionAlignment: TAlignment;
begin
  Result := FCaptionLabel.Alignment;
end;

function TCustomVisualGrid.GetCaptionFont: TFont;
begin
  Result := FCaptionLabel.Font;
end;

function TCustomVisualGrid.GetRows(ARow: Integer): Variant;
begin
  Result := FDataTable.Rows[ARow];
end;

function TCustomVisualGrid.GetSelection: TVisualGridSelection;
var
  i: Integer;
begin
  SetLength(Result.Selections, FDrawGrid.SelectedRangeCount);
  for i := 0 to High(Result.Selections) do
  begin
    Result.Selections[i] := FDrawGrid.SelectedRange[i];
    Result.Selections[i].Top:=Result.Selections[i].Top-1; // - fixed row
    Result.Selections[i].Bottom:=Result.Selections[i].Bottom-1; // - fixed row
  end;
end;

function TCustomVisualGrid.GetCaptionVisible: boolean;
begin
  Result := FCaptionLabel.Visible;
end;

procedure TCustomVisualGrid.PageIndexEditingDone(Sender: TObject);
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

procedure TCustomVisualGrid.PageSizeEditingDone(Sender: TObject);
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

procedure TCustomVisualGrid.MultiSearchCheckComboBoxChange(Sender: TObject; AIndex: Integer);
var
  LState: PTCheckComboItemState;
  LOldHasEdit, LNewHasEdit: boolean;

  function HasOneOrMoreEdit: boolean;
  var
    LEdit: TSearchEdit;
  begin
    for LEdit in FMultiSearchEdits do
      if LEdit.EditVisible then
        exit(true);
    Result := false
  end;

begin
  LOldHasEdit := HasOneOrMoreEdit;
  LState := PTCheckComboItemState(FMultiSearchCheckComboBox.Items.Objects[AIndex]);
  if Assigned(LState^.Data) then
    TSearchEdit(LState^.Data).EditVisible:=LState^.State=cbChecked
  else
    FSearchEdit.Visible:=LState^.State=cbChecked;

  LNewHasEdit := HasOneOrMoreEdit;
  FTopPanelMultiSearch.Visible := LNewHasEdit;

  FSearchButton.Visible:=FSearchEdit.Visible or LNewHasEdit;

  if LOldHasEdit <> LNewHasEdit then
    LayoutChanged;
end;

function TCustomVisualGrid.GetCanvas: TCanvas;
begin
  Result := FDrawGrid.Canvas;
end;

function TCustomVisualGrid.GetSingleSearch: boolean;
begin
  Result := FTopPanelRight.Visible;
end;

procedure TCustomVisualGrid.SetCaption(AValue: TCaption);
begin
  FCaptionLabel.Caption:=AValue;
  LayoutChanged;
end;

procedure TCustomVisualGrid.SetCaptionAlignment(AValue: TAlignment);
begin
  FCaptionLabel.Alignment:=AValue;
end;

procedure TCustomVisualGrid.SetCaptionFont(AValue: TFont);
begin
  FCaptionLabel.Font:=AValue;
  LayoutChanged;
end;

procedure TCustomVisualGrid.SetCaptionVisible(AValue: boolean);
begin
  FCaptionLabel.Visible:=AValue;
  LayoutChanged;
end;

procedure TCustomVisualGrid.SetCells(ACol, ARow: Integer; AValue: Variant);
begin
  TTableRowData(FDataTable.Rows[ARow]).vvalues[ACol] := AValue;
  FDrawGrid.InvalidateCell(ACol, ARow);
end;

procedure TCustomVisualGrid.SetFetchDataInThread(AValue: boolean);
begin
  if FFetchDataInThread=AValue then Exit;
  FFetchDataInThread:=AValue;
end;

procedure TCustomVisualGrid.SetOptions(AValue: TVisualGridOptions);
begin
  if FOptions=AValue then Exit;

  FOptions:=AValue;
  if vgoColSizing in FOptions then
    FDrawGrid.Options := FDrawGrid.Options + [goColSizing]
  else
    FDrawGrid.Options := FDrawGrid.Options - [goColSizing];

  FDrawGrid.AutoFillColumns:=vgoColAutoFill in FOptions;
end;

procedure TCustomVisualGrid.SetRows(ARow: Integer; AValue: Variant);
begin
  FDataTable.Rows[ARow] := AValue;
  FDrawGrid.InvalidateRow(ARow + 1); // + fixed row
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
begin
  if Assigned(FDataSource) and FetchDataInThread then
    TFetchDataThread.Create(Self, ARefreshColumns)
  else
  begin
    BeforeFetchPage;
    FLastFetchDataResult.FromThread:=false;
    FLastFetchDataResult.RefreshColumns:=ARefreshColumns;
    FetchPage(FLastFetchDataResult.FetchResult);
    AfterFetchPage;
  end;
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


procedure TCustomVisualGrid.RefreshPageIndexAndGridInterface;
begin
  SetPageIndexEditText(IntToStr(Succ(FPageIndex)));
  FPageCountLabel.Caption := Format('/%d',[FPageCount]);
  FDrawGrid.Refresh;
end;

procedure TCustomVisualGrid.ReloadColumns;
var
  i: Integer;
  LEdit: TSearchEdit;
  p: PSearchCapability;
  LColumn: TVisualColumn;

  function SearchCapability: PSearchCapability;
  var
    j: Integer;
  begin
    for j := 0 to High(FSearchCapabilities) do
      if FSearchCapabilities[j].ColumnName = FDataTable.Columns[i] then
        Exit(@FSearchCapabilities[j]);
    Result := nil;
  end;

begin
  FDrawGrid.Columns.Clear;
  FDrawGrid.Columns.BeginUpdate;
  FColumns.Clear;
  for i := 0 to High(FDataTable.Columns) do
  begin
    LColumn := TVisualColumn.Create(FDrawGrid.Columns.Add);
    FColumns.Add(LColumn);
    LColumn.StretchedToFill:=False;
    LColumn.FColumn.Title.Caption:=''; //FDataTable.Columns[i]; already painted in default drawing event
  end;
  FDrawGrid.Columns.EndUpdate;
  // TODO: may be optimized
  FMultiSearchEdits.Clear;
  FSearchButton.Visible := true;
  FMultiSearchCheckComboBox.Clear;
  FSearchCapabilities := Copy(FDataSource.SearchCapabilities);
  for i := 0 to High(FDataTable.Columns) do
  begin
    LEdit := TSearchEdit.Create(FTopPanelMultiSearchClient, Self);
    FMultiSearchEdits.Add(LEdit);
    p := SearchCapability;
    LEdit.EditVisible:=Assigned(p);
    LEdit.SearchCapability := p;
    ResizeSearchEdit(i);
    if Assigned(p) then
    begin
      FMultiSearchCheckComboBox.AddItem(p^.ColumnName, cbChecked);
      FMultiSearchCheckComboBox.Objects[FMultiSearchCheckComboBox.Items.Count-1] := LEdit;
    end;
  end;
  if FDrawGrid.Columns.Count > 0 then
    FTopPanelMultiSearch.Height:=FMultiSearchEdits.Last.FPanel.Height;

  // last item doesn't need to store object
  FMultiSearchCheckComboBox.AddItem('Expression', cbChecked);
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

procedure TCustomVisualGrid.SetPageIndex(Value: Integer);
begin
  if Value >= FPageCount then
    Value := FPageCount - 1;
  if Value < 0 then
    Value := 0;

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

procedure TCustomVisualGrid.BeforeFetchPage;
begin
  if Assigned(FDataSource) then
    if FPageIndex >= FPageCount then
      FPageIndex := FPageCount - 1;
end;

procedure TCustomVisualGrid.FetchPage(out AResult: TPageFetchResult);
begin
  if Assigned(FDataSource) then
    AResult := FDataSource.FetchPage(
      TPageFetchParams.Create(FPageIndex, FPageSize, nil), FDataTable)
  else
    FillChar(AResult, SizeOf(AResult), #0);
end;

procedure TCustomVisualGrid.AfterFetchPage;
begin
  with FLastFetchDataResult do
  if Assigned(FDataSource) then
  begin
    if FromThread then
    begin
      Dispose(FCachedDataTable);
      FCachedDataTable := nil;
      FFetchDataThreadTimer.Enabled:=false;
      FLoadDataPanel.Visible:=False;
      ControlsEnable(true);
    end;

    FPageCount:=FetchResult.PageCount;

    if FetchResult.TotalDataCount >= 0 then
      FTotalDataCount := FetchResult.TotalDataCount
    else
      FTotalDataCount := -1;

    FAllRecordsCountLabel.Visible := FTotalDataCount<>-1;
    FAllRecordsCountLabel.Caption:=Format('Total: %d', [FTotalDataCount]);

    FPageIndex := FetchResult.PageIndex;

    if RefreshColumns then
      ReloadColumns;
    FDrawGrid.RowCount := Length(FDataTable.Rows) + 1;
  end;
  RefreshPageIndexAndGridInterface;
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

procedure TCustomVisualGrid.StandardDrawCell(Sender: TObject; ACol,
  ARow: Longint; Rect: TRect; State: TGridDrawState);
var
  LHandled: boolean;
  LSource: PDataTable;
  LCellData: Variant;
begin
  LHandled := False;

  if ARow = 0 then
    ResizeSearchEdit(ACol);
  if (ARow > 0) and Assigned(FDataSource) then
  begin
    if FFetchDataThreadTimer.Enabled then
      LSource := FCachedDataTable
    else
      LSource := @FDataTable;

    LCellData := LSource^.Rows[ARow-1]._(ACol);
  end;

  if Assigned(FOnDrawVisualCell) then
    FOnDrawVisualCell(Self, ACol, ARow, Canvas, Rect, State, LCellData, LHandled);
  if not LHandled then
    DoDrawCell(Self, ACol, ARow, Rect, State, LCellData);
end;

procedure TCustomVisualGrid.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LPopup: TPopupMenu;
  LSelection: TVisualGridSelection;
  i: integer;
  LCol, LRow: longint;
  LContains: boolean = false;
begin
  if Button = mbRight then
    if (SelectionType <> stNone) and Assigned(FOnPreparePopupMenu) and
       (FDrawGrid.MouseToGridZone(X, Y) = gzNormal) then
    begin
      FDrawGrid.MouseToCell(X, Y, LCol, LRow);
      // fixed rows
      Dec(LRow);
      LSelection := Selection;
      for i := 0 to High(LSelection.Selections) do
        if not LContains then
        begin
          with LSelection.Selections[i] do
            LContains := (LCol >= Left) and (LRow >= Top) and (LCol <= Right) and (LRow <= Bottom);
          Break;
        end;
      if not LContains then
        Exit;
      FOnPreparePopupMenu(Self, LSelection, LPopup);
      if Assigned(LPopup) then
        with FDrawGrid.ClientToScreen(TPoint.Create(X, Y)) do
          LPopup.PopUp(X, Y);
    end;
end;

procedure TCustomVisualGrid.SearchKindPopupMenuClick(Sender: TObject);
var
  i: Integer;
  LIsMultiSearch: boolean;
begin
  TMenuItem(Sender).Checked:=true;
  LIsMultiSearch := Sender = FMultiSearchMenuItem;

  // LIsMultiSearch
  //   true: check almost all (except expression)
  //   false: check only expression
  for i := 0 to FMultiSearchCheckComboBox.Items.Count - 2 do
    FMultiSearchCheckComboBox.Checked[i] := LIsMultiSearch;
  FMultiSearchCheckComboBox.Checked[FMultiSearchCheckComboBox.Items.Count-1] := not LIsMultiSearch;
end;

procedure TCustomVisualGrid.GridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  if (SelectionType <> stNone) and Assigned(FOnSelection) then
    FOnSelection(Self, Selection);
end;

end.

