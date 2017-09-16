{
  Copyright (c) 2017 Sphere 10 Software

  Author: Herman Schoenfeld <herman@sphere10.com>

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  Additional Credits:
    Maciej Izak (hnb)
}

unit UCommon;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Controls, FGL, Generics.Collections, Generics.Defaults,
  Variants;

{ GLOBAL FUNCTIONS }

{ Converts a string to hexidecimal format }
function String2Hex(const Buffer: AnsiString): AnsiString;

{ Binary-safe StrComp replacement. StrComp will return 0 for when str1 and str2 both start with NUL character. }
function BinStrComp(const Str1, Str2 : AnsiString): Integer;

{ Ternary operator equivalent of predicate ? (true-val) : (false-value) }
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Cardinal): Cardinal; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Integer): Integer; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Int64): Int64; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: UInt64): UInt64; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Double): Double; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: string): string; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: TObject): TObject; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: variant): variant; overload;

{ DateTime functions }
function TimeStamp : AnsiString;
function UtcTimeStamp : AnsiString;

type
  { TBox - a generic wrapper class for wrappying any type, mainly strings and primtives }
  TBox<T> = class(TObject)
    type
      TDestroyItemDelegate = procedure (constref val : T) of object;
    strict private
      FValue: T;
      FDestroyFunc : TDestroyItemDelegate;
      class procedure NoOpDestroyItem(constref val : T);
    public
      constructor Create(Value: T); overload;
      constructor Create(Value: T; destroyItemFunc: TDestroyItemDelegate); overload;
      destructor Destroy; override;
      property Value: T read FValue;
  end;

  { A TObject-wrapped string }
  TStringObject = TBox<AnsiString>;

  { TArrayTool }
  TArrayTool<T> = class
    public
      class function Contains(const Values: TArray<T>; const Item: T; const Comparer: IEqualityComparer<T>; out ItemIndex: SizeInt): Boolean; overload; static;
      class function Contains(const Values: TArray<T>; const Item: T; out ItemIndex: SizeInt): Boolean; overload; static;
      class function Contains(const Values: TArray<T>; const Item: T) : Boolean; overload; static;
      class function IndexOf(const Values: TArray<T>; const Item: T; const Comparer: IEqualityComparer<T>): SizeInt; overload; static;
      class function IndexOf(const Values: TArray<T>; const Item: T): SizeInt; overload; static;
      class procedure Add(var Values: TArray<T>; const AValue : T); static;
      class procedure Remove(var Values : TArray<T>; const Item : T; const Comparer : IEqualityComparer<T>); overload; static;
      class procedure Remove(var Values : TArray<T>; const Item : T); overload; static;
      class procedure RemoveAt(var Values : TArray<T>; ItemIndex : SizeInt); static;
      class function Concat(const Arrays: array of TArray<T>): TArray<T>; static;
      class function Create(const a : T; const b : T) : TArray<T>; static;
      class function ToArray(Enumerable: TEnumerable<T>; Count: SizeInt): TArray<T>; static;
    end;

  { TNotifyManyEvent - support for multiple listeners }
  TNotifyManyEvent = TArray<TNotifyEvent>;

  { Helper for TNotifyManyEvent }
  TNotifyManyEventHelper = record helper for TNotifyManyEvent
    procedure Add(listener : TNotifyEvent);
    procedure Remove(listener : TNotifyEvent);
    procedure Invoke(sender : TObject);
  end;

  { Controls Helpers }
  TWinControlHelper = class helper for TWinControl
    procedure RemoveAllControls(destroy : boolean);
    procedure AddDockCenter(constref AControl: TWinControl);
  end;

  TTableColumns = TArray<utf8string>;
  PTableColumns = ^TTableColumns;
  ETableRow = class(Exception);

  { TTableRow }

  TTableRow = class(TInvokeableVariantType)
  private
    class constructor Create;
    class destructor Destroy;
  protected type
    TColumnMapToIndex = TDictionary<utf8string, Integer>;
    TColumnsDictionary = TObjectDictionary<PTableColumns, TColumnMapToIndex>;
  protected class var
    FColumns: TColumnsDictionary;
  protected
    class function MapColumns(AColumns: PTableColumns): TColumnMapToIndex;
  public
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(var V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;

    class function New(AColumns: PTableColumns): Variant;
  end;

  TTableRowData = packed record
  public
    vtype: tvartype;
  private
    vfiller1 : word;
    vfiller2: int32;
  public
    vcolumnmap: TTableRow.TColumnMapToIndex;
    vvalues: TArray<Variant>;
  end;

implementation

var
  TableRowType: TTableRow = nil;

{%region Global functions %}

function String2Hex(const Buffer: AnsiString): AnsiString;
var
  n: Integer;
begin
  Result := '';
  for n := 1 to Length(Buffer) do
    Result := LowerCase(Result + IntToHex(Ord(Buffer[n]), 2));
end;


function BinStrComp(const Str1, Str2: AnsiString): integer;
var Str1Len, Str2Len, i : Integer;
begin
   Str1Len := Length(Str1);
   Str2Len := Length(Str2);
   if (Str1Len < Str2Len) then
     Result := -1
   else if (Str1Len > Str2Len) then
     Result := 1
   else begin
     Result := 0;
     For i:= 1 to Str1Len do begin
       if Str1[i] < Str2[i] then begin
         Result := -1;
         break;
       end else if Str1[i] > Str2[i] then begin
         Result := 1;
         break;
       end
     end;
   end;
End;

{%endregion}

{%region Language-level tools }
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Cardinal): Cardinal;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Integer): Integer;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Int64): Int64;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: UInt64): UInt64;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Double): Double;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: string): string;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: TObject): TObject;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: variant): variant;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

{ DateTime functions }
function TimeStamp : AnsiString;
begin
  Result := FormatDateTime('yyy-mm-dd hh:nn:ss', Now);
end;

function UtcTimeStamp : AnsiString;
begin
  raise Exception.Create('Not implemented');
end;

{%endregion}

{%region TBox }

constructor TBox<T>.Create(Value: T);
begin
  Create(Value, NoOpDestroyItem);
end;

constructor TBox<T>.Create(Value: T; destroyItemFunc: TDestroyItemDelegate);
begin
  inherited Create;
  FValue := Value;
  FDestroyFunc := destroyItemFunc;
end;

destructor TBox<T>.Destroy;
begin
  FDestroyFunc(FValue);
  inherited;
end;

class procedure TBox<T>.NoOpDestroyItem(constref val : T);
begin
  // No op
end;

{%endregion}

{%region TArrayTool }

class function TArrayTool<T>.Contains(const Values: TArray<T>; const Item: T; const Comparer: IEqualityComparer<T>; out ItemIndex: SizeInt): Boolean;
var
  Index: SizeInt;
begin
  for Index := 0 to high(Values) do begin
    if Comparer.Equals(Values[Index], Item) then begin
      ItemIndex := Index;
      Result := True;
      exit;
    end;
  end;
  ItemIndex := -1;
  Result := False;
end;

class function TArrayTool<T>.Contains(const Values: TArray<T>; const Item: T; out ItemIndex: SizeInt): Boolean;
begin
  Result := TArrayTool<T>.Contains(Values, Item, TEqualityComparer<T>.Default, ItemIndex);
end;

class function TArrayTool<T>.Contains(const Values: TArray<T>; const Item: T): Boolean;
var
  ItemIndex: SizeInt;
begin
  Result := TArrayTool<T>.Contains(Values, Item, ItemIndex);
end;

class function TArrayTool<T>.IndexOf(const Values: TArray<T>; const Item: T; const Comparer: IEqualityComparer<T>): SizeInt;
begin
  TArrayTool<T>.Contains(Values, Item, Comparer, Result);
end;

class function TArrayTool<T>.IndexOf(const Values: TArray<T>; const Item: T): SizeInt;
begin
  TArrayTool<T>.Contains(Values, Item, Result);
end;

class procedure TArrayTool<T>.Add(var Values: TArray<T>; const AValue : T);
begin
  SetLength(Values, SizeInt(Length(Values)) + 1);
  Values[High(Values)] := AValue;
end;

class procedure TArrayTool<T>.Remove(var Values : TArray<T>; const Item : T; const Comparer : IEqualityComparer<T>);
var index : SizeInt;
begin
  while TArrayTool<T>.Contains(Values, item, Comparer, index) do begin
    TArrayTool<T>.RemoveAt(Values, index);
  end;
end;

class procedure TArrayTool<T>.Remove(var Values : TArray<T>; const Item : T);
begin
  TArrayTool<T>.Remove(Values, Item, TEqualityComparer<T>.Default);
end;

class procedure TArrayTool<T>.RemoveAt(var Values : TArray<T>; ItemIndex : SizeInt);
var i : Integer;
begin
  for i := ItemIndex + 1 to High(Values) do
    Values[i - 1] := Values[i];
  SetLength(Values, Length(Values) - 1);
end;

class function TArrayTool<T>.Concat(const Arrays: array of TArray<T>): TArray<T>;
var
  i, k, LIndex, LLength: Integer;
begin
  LLength := 0;
  for i := 0 to High(Arrays) do
    Inc(LLength, Length(Arrays[i]));
  SetLength(Result, LLength);
  LIndex := 0;
  for i := 0 to High(Arrays) do
  begin
    for k := 0 to High(Arrays[i]) do
    begin
      Result[LIndex] := Arrays[i][k];
      Inc(LIndex);
    end;
  end;
end;

class function TArrayTool<T>.Create(const a, b: T): TArray<T>;
begin
  SetLength(result,2);
  result[0] := a;
  result[1] := b;
end;

class function TArrayTool<T>.ToArray(Enumerable: TEnumerable<T>; Count: SizeInt): TArray<T>;
var
  LItem: T;
begin
  SetLength(Result, Count);
  Count := 0;
  for LItem in Enumerable do
  begin
    Result[Count] := LItem;
    Inc(Count);
  end;
end;

{%endregion}

{ TTableRow }

class constructor TTableRow.Create;
begin
  FColumns := TColumnsDictionary.Create([doOwnsValues]);
end;

class destructor TTableRow.Destroy;
begin
  FColumns.Free;
end;

class function TTableRow.MapColumns(AColumns: PTableColumns): TColumnMapToIndex;
var
  i: Integer;
begin
  Result := TColumnMapToIndex.Create;
  for i := 0 to High(AColumns^) do
    Result.Add(AColumns^[i], i);
  FColumns.Add(AColumns, Result);
end;

function TTableRow.GetProperty(var Dest: TVarData;
  const V: TVarData; const Name: string): Boolean;
var
  LRow: TTableRowData absolute V;
begin
  Variant(Dest) := LRow.vvalues[LRow.vcolumnmap[Name]];
  Result := true;
end;

function TTableRow.SetProperty(var V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
var
  LRow: TTableRowData absolute V;
begin
  LRow.vvalues[LRow.vcolumnmap[Name]] := Variant(Value);
  Result := true;
end;

procedure TTableRow.Clear(var V: TVarData);
begin
  Finalize(TTableRowData(V));
  FillChar(V, SizeOf(V), #0);
end;

procedure TTableRow.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
var
  LDestRow: TTableRowData absolute Dest;
  LSourceRow: TTableRowData absolute Source;
begin
  if Indirect then
    SimplisticCopy(Dest,Source,true)
  else
  begin
    VarClear(variant(Dest));
    LDestRow.vtype := LSourceRow.vtype;
    LDestRow.vcolumnmap := LSourceRow.vcolumnmap;
    LDestRow.vvalues := system.copy(TTableRowData(LSourceRow).vvalues);
  end;
end;

function TTableRow.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
var
  LRow: TTableRowData absolute V;
begin
  Result := (Name = '_') and (Length(Arguments)=1);
  if Result then
    Variant(Dest) := LRow.vvalues[Variant(Arguments[0])];
end;

class function TTableRow.New(AColumns: PTableColumns): Variant;
var
  LColumnMap: TColumnMapToIndex;
begin
  if not Assigned(AColumns) then
    raise ETableRow.Create('AColumns can''t be nil!');

  VarClear(Result);
  FillChar(Result, SizeOf(Result), #0);
  TTableRowData(Result).vtype:=TableRowType.VarType;

  if not FColumns.TryGetValue(AColumns, LColumnMap) then
    LColumnMap := MapColumns(AColumns);

  TTableRowData(Result).vcolumnmap:=LColumnMap;
  SetLength(TTableRowData(Result).vvalues, Length(AColumns^));
end;

{%region TNotifyManyEventHelper}

procedure TNotifyManyEventHelper.Add(listener : TNotifyEvent);
begin
  if TArrayTool<TNotifyEvent>.IndexOf(self, listener) = -1 then begin
    TArrayTool<TNotifyEvent>.Add(self, listener);
  end;
end;

procedure TNotifyManyEventHelper.Remove(listener : TNotifyEvent);
begin
  TArrayTool<TNotifyEvent>.Remove(self, listener);
end;

procedure TNotifyManyEventHelper.Invoke(sender : TObject);
var i : Integer;
begin
  for i := 0 to high(self) do
    self[i](sender);
end;

{%endregion}

{%region TWinControlHelper }

procedure TWinControlHelper.RemoveAllControls(destroy : boolean);
var
  control : TControl;
begin
  while self.ControlCount > 0 do begin
    control := self.Controls[0];
    self.RemoveControl(control);
    if destroy then control.Destroy;
  end;
end;


procedure TWinControlHelper.AddDockCenter(constref AControl: TWinControl);
begin
  with AControl do begin
     Align := alClient;
     Parent := self;
  end;
end;

{%endregion}

initialization
  TableRowType := TTableRow.Create;
finalization
  TableRowType.Free;
end.

