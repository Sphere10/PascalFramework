unit fMain;

{$MODE DELPHI}

interface

uses
  SysUtils, Classes, Forms, Controls, Graphics, Dialogs, Math, PropEdits,
  LCLType, UVisualGrid, StdCtrls, Menus, Types, Grids, ExtCtrls, CheckLst,
  RTTIGrids, UMemory, UCommon, UCommon.Data, Generics.Collections, TypInfo;

type

  { TEntity }

  TEntity = record
    ID : Integer;
    Name : AnsiString;
    Foo: Integer;
    &Boolean : boolean;
    &Char : char;
    &UInt16 : UInt16;
    &Real : Real;
    Bar : utf8String;
  end;

  { TEntityDataSource }

  TEntityDataSource = class(TCustomDataSource<TEntity>)
    protected
      function GetItemDisposePolicy : TDisposePolicy; override;
      function GetColumns : TDataColumns;  override;
      procedure OnBeforeFetchAll(constref AParams: TPageFetchParams); override;
      procedure FetchAll(const AContainer : TList<TEntity> ); override;
      procedure OnAfterFetchAll(constref AParams: TPageFetchParams); override;
      function GetItemField(constref AItem: TEntity; const AColumnName : AnsiString) : Variant; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    AddDelayCheckBox: TCheckBox;
    bRegenerate: TButton;
    bDeselect: TButton;
    bSetWidth: TButton;
    bSearchParser: TButton;
    Button1: TButton;
    cbExpectedKind: TComboBox;
    ColumnsAutoFillCheckBox: TCheckBox;
    cbSearchParser: TComboBox;
    ComboBox1: TComboBox;
    eCol: TEdit;
    Edit1: TEdit;
    eWidth: TEdit;
    eNumEntities: TEdit;
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
    pWidgets: TPanel;
    PopupMenu1: TPopupMenu;
    TIPropertyGrid1: TTIPropertyGrid;
    procedure bDeselectClick(Sender: TObject);
    procedure bSetWidthClick(Sender: TObject);
    procedure bSearchParserClick(Sender: TObject);
    procedure bRegenerateClick(Sender: TObject);
    procedure ColumnsAutoFillCheckBoxChange(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
    procedure eNumEntitiesChange(Sender: TObject);
    procedure FirstColumnStretchedCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miUpdateCellClick(Sender: TObject);
    procedure miUpdateRowClick(Sender: TObject);
  private
    FDataSource : TEntityDataSource;
    FVisualGrid: TVisualGrid;
    { Private declarations }
  private
    procedure PreparePopupMenu(Sender: TObject; constref ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
    procedure DrawVisualCell(Sender: TObject; ACol, ARow: Longint; Canvas : TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
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
  GNumEntities : Integer;

  { Globals }
  procedure GenerateEntities;
  begin
    if NOT Assigned(GData) then
      GData := TList<TEntity>.Create;

    GData.Clear;
    for i := 0 to GNumEntities - 1 do begin
      entity.ID := i;
      entity.Name := Format('Name %d', [i + 1]);
      entity.Foo := i div 5;
      entity.&Boolean := IIF(i mod 2 = 0, True, False);
      entity.&Char := Char(65 + (i mod 3));   // A, B or C
      entity.&UInt16 := i + 1;
      entity.&Real:= i / 2.0;
      entity.Bar := TGuid.NewGuid.ToString(True);
      GData.Add(entity);
    end;
  end;

  { TEntityDataSource }

  function TEntityDataSource.GetItemDisposePolicy : TDisposePolicy;
  begin
    Result := idpNone;
  end;

  function TEntityDataSource.GetColumns : TDataColumns;
  begin
    Result := TDataColumns.Create(
      TDataColumn.From('ID'),
      TDataColumn.From('Name'),
      TDataColumn.From('Foo'),
      TDataColumn.From('Boolean'),
      TDataColumn.From('Char'),
      TDataColumn.From('UInt16'),
      TDataColumn.From('Real'),
      TDataColumn.From('Bar')
    );
  end;

  procedure TEntityDataSource.OnBeforeFetchAll(constref AParams: TPageFetchParams);
  var
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
        GetEnumName(TypeInfo(TDataFilter), Ord(AFilter.Filter)),
        GetEnumName(TypeInfo(UCommon.Data.TSortDirection), Ord(AFilter.Sort)),
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

  function TEntityDataSource.GetItemField(constref AItem: TEntity; const AColumnName : AnsiString) : Variant;
  begin
     if AColumnName = 'ID' then
       Result := AItem.ID
     else if AColumnName = 'Name' then
       Result := AItem.Name
     else if AColumnName = 'Foo' then
       Result := AItem.Foo
     else if AColumnName = 'Boolean' then
       Result := AItem.&Boolean
     else if AColumnName = 'Char' then
       Result := AItem.&Char
     else if AColumnName = 'UInt16' then
       Result := AItem.&UInt16
     else if AColumnName = 'Real' then
       Result := AItem.&Real
     else if AColumnName = 'Bar' then
       Result := AItem.&Bar
     else raise Exception.Create(Format('Field not found [%s]', [AColumnName]));
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
    with FVisualGrid.AddColumn('1: ID') do begin
      Binding := 'ID';
      Filters:=SORTABLE_NUMERIC_FILTER;
    end;
    with FVisualGrid.AddColumn('Name') do begin
      Filters:=SORTABLE_TEXT_FILTER;
    end;
    with FVisualGrid.AddColumn('Foo') do begin
      Filters:=SORTABLE_TEXT_FILTER;
    end;
    with FVisualGrid.AddColumn('Boolean') do begin
      Filters:=SORTABLE_NUMERIC_FILTER;
    end;
    with FVisualGrid.AddColumn('Char') do begin
      Filters:=SORTABLE_TEXT_FILTER;
    end;
    with FVisualGrid.AddColumn('UInt16') do begin
      Filters:=SORTABLE_NUMERIC_FILTER;
    end;
    with FVisualGrid.AddColumn('Real') do begin
      Filters:=SORTABLE_NUMERIC_FILTER;
      Renderer:=TVisualCellRenderers.DollarValue;
    end;
    with FVisualGrid.AddColumn('Bar') do begin
      Filters:=SORTABLE_TEXT_FILTER;
    end;

    GridPanel.AddControlDockCenter(FVisualGrid);
    TIPropertyGrid1.TIObject := FVisualGrid;
    TIPropertyGrid1.PropertyEditorHook.LookupRoot := Self;
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
    rd: TDataRowData absolute r;
  begin
{    with FVisualGrid.Selection do
      for i := 0 to RowCount - 1 do
      begin
        r := FVisualGrid.Rows[i + Row];
        for j := 0 to High(rd.vvalues) do
          rd.vvalues[j] := 'Test';
        FVisualGrid.Rows[i + Row] := r;
      end;}
  end;

  procedure TForm1.bRefreshClick(Sender: TObject);
  begin
    TIPropertyGrid1.RefreshPropertyValues;
  end;

  procedure TForm1.eNumEntitiesChange(Sender: TObject);
  begin
     GNumEntities := StrToInt(eNumEntities.Text);
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

  procedure TForm1.bDeselectClick(Sender: TObject);
  begin
    FVisualGrid.ClearSelection;
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

  procedure TForm1.DrawVisualCell(Sender: TObject; ACol, ARow: Longint; Canvas : TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
  var
    LTextStyle: TTextStyle;
  begin
    Handled := (ACol = 2) and (ARow > 0);
    if Handled then
    begin
      Canvas.Font.Color:=clRed;
      Canvas.Font.Style:=[fsBold];
      LTextStyle := Canvas.TextStyle;
//      LTextStyle.Alignment:=taCenter;
      LTextStyle.Alignment:=taRightJustify;
      Canvas.TextStyle:=LTextStyle;
      Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, CellData, LTextStyle);
    end;
  end;

  procedure TForm1.Selection(Sender: TObject; constref ASelection: TVisualGridSelection);
  begin
    lSelection.Caption := Format('C=%d R=%d ColCount=%d RowCount=%d P=%d',
      [ASelection.Col, ASelection.Row, ASelection.ColCount, ASelection.RowCount, ASelection.Page]);
  end;

  procedure TForm1.bRegenerateClick(Sender: TObject);
  begin
    GenerateEntities;
    FVisualGrid.RefreshGrid;
  end;

type

  { TWidgetsPropertyEditor }

  TWidgetsPropertyEditor = class(TComponentOneFormPropertyEditor)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TWidgetsPropertyEditor }

procedure TWidgetsPropertyEditor.GetValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := 0 to Form1.pWidgets.ControlCount - 1 do
    Proc(Form1.pWidgets.Controls[i].Name);
end;


initialization
  GNumEntities := 10000;
  GenerateEntities;

{ID : Integer;
Name : AnsiString;
Foo: Integer;
&Boolean : boolean;
&Char : char;
&UInt16 : UInt16;
&Real : Real;
Bar : utf8String;
}

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

  RegisterPropertyEditor(TypeInfo(TControl), TVisualGrid, 'WidgetControl', TWidgetsPropertyEditor);

finalization
  FreeAndNil(GData);
end.


