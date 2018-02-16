unit fMain;

{$MODE DELPHI}

interface

uses
  SysUtils, Classes, Forms, Controls, Graphics, Dialogs, Math, PropEdits,
  LCLType, UVisualGrid, StdCtrls, Menus, Types, Grids, ExtCtrls, CheckLst,
  RTTIGrids, UCommon, UCommon.Data, Generics.Collections, TypInfo;

type

  { TEntity }

  TEntity = record
    ID : Integer;
    Name : AnsiString;
    Foo: Integer;
    Bar : AnsiString;
  end;

  { TEntityDataSource }

  TEntityDataSource = class(TCustomDataSource<TEntity>)
    protected
      function GetItemDisposePolicy : TItemDisposePolicy; override;
      function GetColumns : TTableColumns;  override;
      function GetSearchCapabilities: TSearchCapabilities; override;
      procedure OnBeforeFetchAll(constref AParams: TPageFetchParams); override;
      procedure FetchAll(const AContainer : TList<TEntity> ); override;
      procedure OnAfterFetchAll(constref AParams: TPageFetchParams); override;
      function GetItemField(constref AItem: TEntity; const AColumnName : utf8string) : Variant; override;
      procedure DehydrateItem(constref AItem: TEntity; var ATableRow: Variant); override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    AddDelayCheckBox: TCheckBox;
    bSetWidth: TButton;
    bSearchParser: TButton;
    cbExpectedKind: TComboBox;
    ColumnsAutoFillCheckBox: TCheckBox;
    cbSearchParser: TComboBox;
    eCol: TEdit;
    eWidth: TEdit;
    FirstColumnStretchedCheckBox: TCheckBox;
    bRefresh: TButton;
    GridPanel: TPanel;
    Label1: TLabel;
    lExpectedKind: TLabel;
    lbSearchCriteria: TListBox;
    lWidth: TLabel;
    lSelection: TLabel;
    miUpdateCell: TMenuItem;
    miUpdateRow: TMenuItem;
    MenuItem3: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PopupMenu1: TPopupMenu;
    TIPropertyGrid1: TTIPropertyGrid;
    procedure bSetWidthClick(Sender: TObject);
    procedure bSearchParserClick(Sender: TObject);
    procedure ColumnsAutoFillCheckBoxChange(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
    procedure FirstColumnStretchedCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miUpdateCellClick(Sender: TObject);
    procedure miUpdateRowClick(Sender: TObject);
  private
    FColumns: TTableColumns;
    FDataSource : TEntityDataSource;
    FVisualGrid: TVisualGrid;
    { Private declarations }
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

uses
  UCommon.UI;

{$R *.lfm}

var
  GData : TList<TEntity>;
  GSearchCriteria: TListBox;
  GAddDelayCheckBox : TCheckBox;
  i : integer;
  entity : TEntity;

  { TEntityDataSource }

  function TEntityDataSource.GetItemDisposePolicy : TItemDisposePolicy;
  begin
    Result := idpNone;
  end;

  function TEntityDataSource.GetColumns : TTableColumns;
  begin
    Result := TTableColumns.Create('ID', 'Name', 'Foo', 'Bar');
  end;

  function TEntityDataSource.GetSearchCapabilities: TSearchCapabilities;
  begin
    Result := TSearchCapabilities.Create(
      TSearchCapability.From('ID', SORTABLE_NUMERIC_FILTER),
      TSearchCapability.From('Name', SORTABLE_TEXT_FILTER),
      TSearchCapability.From('Bar', SORTABLE_TEXT_FILTER)
    );
  end;

  procedure TEntityDataSource.OnBeforeFetchAll(constref AParams: TPageFetchParams);
  var
    i : Integer;
    LFilter: TColumnFilter;

    function FilterToStr(const AFilter: TColumnFilter): string;
    var
      LValues: utf8string;
      LValue: Variant;
    begin
      for LValue in AFilter.Values do
        LValues := LValues + QuotedStr(LValue) + ',';
      SetLength(LValues, Length(LValues) - 1);
      Result := Format('Col: ''%s'' :: Kind: %s :: Sort: %s :: Values(%s)', [
        AFilter.ColumnName,
        GetEnumName(TypeInfo(TVisualGridFilter), Ord(AFilter.Filter)),
        GetEnumName(TypeInfo(UVisualGrid.TSortDirection), Ord(AFilter.Sort)),
        LValues]);
    end;
  begin
    // show in GUI search criteria
    GSearchCriteria.Clear;
    for LFilter in AParams.Filter do
      GSearchCriteria.AddItem(FilterToStr(LFilter), nil);
  end;

  procedure TEntityDataSource.FetchAll(const AContainer : TList<TEntity>);
  var
    i : integer;
  begin
    for i := 0 to GData.Count - 1 do begin
      AContainer.Add(GData[i]);
    end;
  end;

  procedure TEntityDataSource.OnAfterFetchAll(constref AParams: TPageFetchParams);
  begin
    if GAddDelayCheckBox.Checked then
      Sleep(3000);
  end;

  function TEntityDataSource.GetItemField(constref AItem: TEntity; const AColumnName : utf8string) : Variant;
  begin
     if AColumnName = 'ID' then
       Result := AItem.ID
     else if AColumnName = 'Name' then
       Result := AItem.Name
     else if AColumnName = 'Foo' then
       Result := AItem.Foo
     else if AColumnName = 'Bar' then
       Result := AItem.Bar
     else raise Exception.Create(Format('Field not found [%s]', [AColumnName]));
  end;

  procedure TEntityDataSource.DehydrateItem(constref AItem: TEntity; var ATableRow: Variant);
  begin
    ATableRow.ID := AItem.ID;
    ATableRow.Name := AItem.Name;
    ATableRow.Foo := AItem.Foo;
    ATableRow.Bar := AItem.Bar;
  end;

  { TForm1 }

  procedure TForm1.PreparePopupMenu(Sender: TObject; constref
    ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
  begin
    APopupMenu := PopupMenu1;
  end;

  procedure TForm1.FormCreate(Sender: TObject);
  begin
    GAddDelayCheckBox := AddDelayCheckBox;
    GSearchCriteria := lbSearchCriteria;

    FDataSource := TEntityDataSource.Create(Self);
    FVisualGrid := TVisualGrid.Create(Self);
    FVisualGrid.DataSource := FDataSource;
    FVisualGrid.OnDrawVisualCell:=DrawVisualCell;
    FVisualGrid.OnPreparePopupMenu:=PreparePopupMenu;
    FVisualGrid.OnSelection:=Selection;

    GridPanel.AddControlDockCenter(FVisualGrid);
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

  procedure TForm1.bSetWidthClick(Sender: TObject);
  var
    LCol: Integer;
  begin
    LCol := StrToIntDef(eCol.Text, 0);
    if LCol >= FVisualGrid.ColCount then
    begin
      LCol := FVisualGrid.ColCount - 1;
      eCol.Text := IntToStr(LCol);
    end;

    FVisualGrid.Columns[LCol].Width := StrToIntDef(eWidth.Text, 150);
  end;

  procedure TForm1.bSearchParserClick(Sender: TObject);
  var
    er: TExpressionRecord;
    i: Integer;
    LExpressionKind, LSubKind, LValues: string;
    LMsg: string;
    LExpectedKind: TExpressionKind;
  begin
    LExpectedKind := TExpressionKind(cbExpectedKind.ItemIndex);
    TSearchExpressionService.Parse(cbSearchParser.Text, LExpectedKind, er);

    LExpressionKind := GetEnumName(TypeInfo(TExpressionKind), Ord(er.Kind));
    case er.Kind of
      ekNum: LSubKind := GetEnumName(TypeInfo(TNumericComparisionKind), Ord(er.NumericComparisionKind));
      ekText: LSubKind := GetEnumName(TypeInfo(TTextMatchKind), Ord(er.TextMatchKind));
      ekSet: LSubKind := GetEnumName(TypeInfo(TSetKind), Ord(er.SetKind));
    end;

    for i := 0 to High(er.Values) do
      LValues:=LValues + (er.Values[i]) + sLineBreak;
    SetLength(LValues, Length(LValues) - Length(sLineBreak));

    LMsg := Format('Kind : %s'+sLineBreak+'SubKind : %s'+sLineBreak+'Values:'+sLineBreak+'%s',
      [LExpressionKind, LSubKind, LValues]);
    if IsConsole then
      WriteLn(LMsg, sLineBreak)
    else
      ShowMessage(LMsg);
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


initialization
  GData := TList<TEntity>.Create;

  for i := 0 to 999 do begin
    entity.ID := i;
    entity.Name := Format('Name %d', [i + 1]);
    entity.Foo := i + 1;
    entity.Bar := Format('String with %d', [(i * i) mod ((i div 5) + 5)]);
    GData.Add(entity);
  end;

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

finalization
  FreeAndNil(GData);
end.


