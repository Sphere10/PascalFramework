{
  Copyright (c) 2017 Sphere 10 Software

  Common unit usable across all tiers.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
    Herman Schoenfeld
    Maciej Izak (hnb)
}

unit UCommon;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults,
  Variants, LazUTF8, math, typinfo;

{ CONSTANTS }

const
  MillisPerSecond = 1000;
  MillisPerMinute = 60 * MillisPerSecond;
  MillisPerHour = 60 * MillisPerMinute;
  MillisPerDay = 24 * MillisPerHour;
  MaxMilliseconds = High(Int64);
  MinMilliseconds = Low(Int64);
  MaxSeconds = MaxMilliseconds div 60;
  MinSeconds = MinMilliseconds div 60;


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

function GetSetName(const aSet:PTypeInfo; Value: Integer):string;
function GetSetValue(const aSet:PTypeInfo; Name: String): Integer;



{ Clip Value }
function ClipValue( AValue, MinValue, MaxValue: Integer) : Integer;

{ DateTime functions }
function TimeStamp : AnsiString;
function UtcTimeStamp : AnsiString;

type

  { TTimeSpan }

  TTimeSpan = record
    // Naive implementation - only accurate to millisecond scale, needs updating to tick scale.
    private
      FMillis: Int64;
    strict private
      class constructor Create;
      function GetDays: Integer;
      function GetHours: Integer;
      function GetMinutes: Integer;
      function GetSeconds: Integer;
      function GetMilliseconds: Integer;
      function GetTotalDays: Double;
      function GetTotalHours: Double;
      function GetTotalMinutes: Double;
      function GetTotalSeconds: Double;
      function GetTotalMilliseconds: Double;
      class function Normalize(const ADateTime : TDateTime) : Int64; inline; static;
    public
      constructor Create(Hours, Minutes, Seconds: Integer); overload;
      constructor Create(Days, Hours, Minutes, Seconds: Integer); overload;
      constructor Create(Days, Hours, Minutes, Seconds, Milliseconds: Integer); overload;
      function Add(const TS: TTimeSpan): TTimeSpan; overload;
      function Duration: TTimeSpan;
      function Negate: TTimeSpan;
      function Subtract(const TS: TTimeSpan): TTimeSpan; overload;
      function ToString: string;
      class function FromDays(Value: Double): TTimeSpan; static;
      class function FromHours(Value: Double): TTimeSpan; static;
      class function FromMinutes(Value: Double): TTimeSpan; static;
      class function FromSeconds(Value: Double): TTimeSpan; static;
      class function FromMilliseconds(Value: Double): TTimeSpan; static;
      class function Subtract(const D1, D2: TDateTime): TTimeSpan; overload; static;
      class function Parse(const S: string): TTimeSpan; static;
      class function TryParse(const S: string; out Value: TTimeSpan): Boolean; static;
      class operator +(const Left, Right: TTimeSpan) ATimeSpan : TTimeSpan;
      class operator +(const Left: TTimeSpan; Right: TDateTime) ADateTime: TDateTime;
      class operator +(const Left: TDateTime; Right: TTimeSpan) ADateTime: TDateTime;
      class operator -(const Left, Right: TTimeSpan) ATimeSpan : TTimeSpan;
      class operator -(const Left: TDateTime; Right: TTimeSpan) ADateTime: TDateTime;
      class operator =(const Left, Right: TTimeSpan) ABool : Boolean;
      class operator <>(const Left, Right: TTimeSpan) ABool : Boolean;
      class operator >(const Left, Right: TTimeSpan) ABool : Boolean;
      class operator >=(const Left, Right: TTimeSpan) ABool : Boolean;
      class operator <(const Left, Right: TTimeSpan) ABool : Boolean;
      class operator <=(const Left, Right: TTimeSpan) ABool : Boolean;
      class operator Negative(const Value: TTimeSpan) ATimeSpan: TTimeSpan;
      class operator Positive(const Value: TTimeSpan) ATimeSpan : TTimeSpan;
      class operator Implicit(const Value: TTimeSpan) AString : string;
      class operator Explicit(const Value: TTimeSpan) AString : string;
      property Days: Integer read GetDays;
      property Hours: Integer read GetHours;
      property Minutes: Integer read GetMinutes;
      property Seconds: Integer read GetSeconds;
      property Milliseconds: Integer read GetMilliseconds;
      property TotalDays: Double read GetTotalDays;
      property TotalHours: Double read GetTotalHours;
      property TotalMinutes: Double read GetTotalMinutes;
      property TotalSeconds: Double read GetTotalSeconds;
      property TotalMilliseconds: Double read GetTotalMilliseconds;
    end;

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

  { TDateTimeHelper }

  TDateTimeHelper = record helper for TDateTime
   private
     function GetDay: Word; inline;
     function GetDate: TDateTime; inline;
     function GetDayOfWeek: Word; inline;
     function GetDayOfYear: Word; inline;
     function GetHour: Word; inline;
     function GetMillisecond: Word; inline;
     function GetMinute: Word; inline;
     function GetMonth: Word; inline;
     function GetSecond: Word; inline;
     function GetTime: TDateTime; inline;
     function GetYear: Word; inline;
     class function GetNow: TDateTime; static; inline;
     class function GetToday: TDateTime; static; inline;
     class function GetTomorrow: TDateTime; static; inline;
     class function GetYesterDay: TDateTime; static; inline;
   public
     class function Create(const aYear, aMonth, aDay: Word): TDateTime; overload; static; inline;
     class function Create(const aYear, aMonth, aDay, aHour, aMinute, aSecond, aMillisecond: Word): TDateTime; overload; static; inline;

     class property Now: TDateTime read GetNow;
     class property Today: TDateTime read GetToday;
     class property Yesterday: TDateTime read GetYesterDay;
     class property Tomorrow: TDateTime read GetTomorrow;

     property Date: TDateTime read GetDate;
     property Time: TDateTime read GetTime;

     property DayOfWeek: Word read GetDayOfWeek;
     property DayOfYear: Word read GetDayOfYear;

     property Year: Word read GetYear;
     property Month: Word read GetMonth;
     property Day: Word read GetDay;
     property Hour: Word read GetHour;
     property Minute: Word read GetMinute;
     property Second: Word read GetSecond;
     property Millisecond: Word read GetMillisecond;

     function ToString(const aFormatStr: string = ''): string; inline;

     function StartOfYear: TDateTime; inline;
     function EndOfYear: TDateTime; inline;
     function StartOfMonth: TDateTime; inline;
     function EndOfMonth: TDateTime; inline;
     function StartOfWeek: TDateTime; inline;
     function EndOfWeek: TDateTime; inline;
     function StartOfDay: TDateTime; inline;
     function EndOfDay: TDateTime; inline;

     function AddYears(const aNumberOfYears: Integer = 1): TDateTime; inline;
     function AddMonths(const aNumberOfMonths: Integer = 1): TDateTime; inline;
     function AddDays(const aNumberOfDays: Integer = 1): TDateTime; inline;
     function AddHours(const aNumberOfHours: Int64 = 1): TDateTime; inline;
     function AddMinutes(const aNumberOfMinutes: Int64 = 1): TDateTime; inline;
     function AddSeconds(const aNumberOfSeconds: Int64 = 1): TDateTime; inline;
     function AddMilliseconds(const aNumberOfMilliseconds: Int64 = 1): TDateTime; inline;

     function CompareTo(const aDateTime: TDateTime): TValueRelationship; inline;
     function Equals(const aDateTime: TDateTime): Boolean; inline;
     function IsSameDay(const aDateTime: TDateTime): Boolean; inline;
     function InRange(const aStartDateTime, aEndDateTime: TDateTime; const aInclusive: Boolean = True): Boolean; inline;
     function IsInLeapYear: Boolean; inline;
     function IsToday: Boolean; inline;
     function IsAM: Boolean; inline;
     function IsPM: Boolean; inline;

     function YearsBetween(const aDateTime: TDateTime): Integer; inline;
     function MonthsBetween(const aDateTime: TDateTime): Integer; inline;
     function WeeksBetween(const aDateTime: TDateTime): Integer; inline;
     function DaysBetween(const aDateTime: TDateTime): Integer; inline;
     function HoursBetween(const aDateTime: TDateTime): Int64; inline;
     function MinutesBetween(const aDateTime: TDateTime): Int64; inline;
     function SecondsBetween(const aDateTime: TDateTime): Int64; inline;
     function MilliSecondsBetween(const aDateTime: TDateTime): Int64; inline;

     function WithinYears(const aDateTime: TDateTime; const aYears: Integer): Boolean; inline;
     function WithinMonths(const aDateTime: TDateTime; const aMonths: Integer): Boolean; inline;
     function WithinWeeks(const aDateTime: TDateTime; const aWeeks: Integer): Boolean; inline;
     function WithinDays(const aDateTime: TDateTime; const aDays: Integer): Boolean; inline;
     function WithinHours(const aDateTime: TDateTime; const aHours: Int64): Boolean; inline;
     function WithinMinutes(const aDateTime: TDateTime; const aMinutes: Int64): Boolean; inline;
     function WithinSeconds(const aDateTime: TDateTime; const aSeconds: Int64): Boolean; inline;
     function WithinMilliseconds(const aDateTime: TDateTime; const AMilliseconds: Int64): Boolean; inline;
   end;

  { Collection Support }

  TArrayTool<T> = class
    public
      class function Contains(const Values: TArray<T>; const Item: T; const Comparer: IEqualityComparer<T>; out ItemIndex: SizeInt): Boolean; overload; static;
      class function Contains(const Values: TArray<T>; const Item: T; out ItemIndex: SizeInt): Boolean; overload; static;
      class function Contains(const Values: TArray<T>; const Item: T) : Boolean; overload; static;
      class function IndexOf(const Values: TArray<T>; const Item: T; const Comparer: IEqualityComparer<T>): SizeInt; overload; static;
      class function IndexOf(const Values: TArray<T>; const Item: T): SizeInt; overload; static;
      class function Copy(const AArray: array of T): TArray<T>; overload;
      class function Copy(const AArray: array of T; FromIndex, Count: SizeInt ): TArray<T>; overload;
      class procedure Add(var Values: TArray<T>; const AValue : T); static;
      class procedure Remove(var Values : TArray<T>; const Item : T; const Comparer : IEqualityComparer<T>); overload; static;
      class procedure Remove(var Values : TArray<T>; const Item : T); overload; static;
      class procedure RemoveAt(var Values : TArray<T>; ItemIndex : SizeInt); static;
      class procedure Append(var Arr: TArray<T>; Value: T);
      class procedure Prepend(var Arr: TArray<T>; Value: T);
      class procedure InsertAt(var Values : TArray<T>; ItemIndex : SizeInt; const Item : T);
      class procedure Swap(var Values : array of T; Item1Index, Item2Index : SizeInt);
      class procedure MoveItem(var Values : array of T; FromIndex, ToIndex : SizeInt);
      class function Concat(const Arrays: array of TArray<T>): TArray<T>; static;
      class function Create(const item0 : T) : TArray<T>; overload; static;
      class function Create(const item0 : T; const item1 : T) : TArray<T>; overload; static;
      class function Create(const item0 : T; const item1 : T; const item2 : T) : TArray<T>; overload; static;
      class function Create(const item0 : T; const item1 : T; const item2 : T; const item3 : T) : TArray<T>; overload; static;
      class function Create(const item0 : T; const item1 : T; const item2 : T; const item3 : T; const item4 : T) : TArray<T>; overload; static;
      class function Create(const item0 : T; const item1 : T; const item2 : T; const item3 : T; const item4 : T; const item5 : T) : TArray<T>; overload; static;
      class function ToArray(Enumerable: TEnumerable<T>; Count: SizeInt): TArray<T>; static;
    end;

  TItemDisposePolicy = (idpNone, idpNil, idpFreeAndNil);
  TListTool<T> = class
    public type
      TPredicate = function (constref Item: T) : boolean of object;
    class function Filter(const AList: TList<T>; const APredicate: TPredicate) : SizeInt; overload;
    class function Filter(const AList: TList<T>; const APredicate: TPredicate; AItemDisposePolicy : TItemDisposePolicy) : SizeInt; overload;
  end;


  { Event Support}

  TNotifyEventEx = procedure (sender : TObject; const args: array of Pointer) of object;
  TNotifyManyEvent = TArray<TNotifyEvent>;
  TNotifyManyEventEx = TArray<TNotifyEventEx>;
  TNotifyManyEventHelper = record helper for TNotifyManyEvent
    procedure Add(listener : TNotifyEvent);
    procedure Remove(listener : TNotifyEvent);
    procedure Invoke(sender : TObject);
  end;
  TNotifyManyEventExHelper = record helper for TNotifyManyEventEx
    procedure Add(listener : TNotifyEventEx);
    procedure Remove(listener : TNotifyEventEx);
    procedure Invoke(sender : TObject; const args: array of Pointer);
  end;

  { Comparer Support }

  TNestedComparison<T> = function (constref Left, Right: T): Integer is nested;

  TNestedComparer<T> = class(TComparer<T>)
    private
      FComparer: TNestedComparison<T>;
    public
      constructor Create(const comparer: TNestedComparison<T>);
      function Compare(constref Left, Right: T): Integer; override;
  end;

  TManyComparer<T> = class(TInterfacedObject, IComparer<T>)
    private type
      TComparer_T = TComparer<T>;
      IComparer_T = IComparer<T>;
      TNestedComparison_T = TNestedComparison<T>;
      TOnComparison_T = TOnComparison<T>;
      TComparison_T = TComparisonFunc<T>;
    private
      FComparers : TArray<IComparer_T>;
    public
      constructor Create(const comparers: TArray<IComparer_T>); overload;
      destructor Destroy; override;
      function Compare(constref Left, Right: T): Integer;
      class function Construct(const comparers: array of TNestedComparison<T>) : IComparer<T>; overload;
      class function Construct(const comparers: array of TOnComparison<T>) : IComparer<T>; overload;
      class function Construct(const comparers: array of TComparisonFunc<T>) : IComparer<T>; overload;
      class function Construct(const comparers: array of TComparer<T>) : IComparer<T>; overload;
  end;

  { TTable types }

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
    function GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean; override;
    function SetProperty(var V: TVarData; const Name: string; const Value: TVarData): Boolean; override;
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    function DoFunction(var Dest: TVarData; const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean; override;
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

  TExpressionKind = (ekUnknown, ekText, ekNum, ekSet);
  TTextMatchKind = (tmkUnknown, tmkMatchTextExact, tmkMatchTextBeginning, tmkMatchTextEnd, tmkMatchTextAnywhere);
  TNumericComparisionKind = (nckUnknown, nckNumericEQ, nckNumericLT, nckNumericLTE, nckNumericGT, nckNumericGTE);
  TSetKind = (skUnknown, skNumericBetweenInclusive, skNumericBetweenExclusive);

  TExpressionRecord = record
    Values: array of utf8string;
  case Kind: TExpressionKind of
    ekUnknown: ();
    ekText: (TextMatchKind: TTextMatchKind);
    ekNum: (NumericComparisionKind: TNumericComparisionKind);
    ekSet: (SetKind: TSetKind);
  end;

  ESearchExpressionParserException = class(Exception);

  { TSearchExpressionService }

  TSearchExpressionService = class
  public
    class procedure Parse(const AExpression: utf8string; const AExpressionKind: TExpressionKind; out AExpressionRecord: TExpressionRecord); overload;
    class function Parse(const AExpression: utf8string): TExpressionRecord; overload;
  end;

{ COMPLEX CONSTANTS }

const
    MinTimeSpan : TTimeSpan = (FMillis: Low(Int64));
    MaxTimeSpan: TTimeSpan = (FMillis: High(Int64));
    ZeroTimeSpan: TTimeSpan = (FMillis: 0);

{ RESOURCES }
resourcestring
  sNotImplemented = 'Not implemented';
  sInvalidParameter_OutOfBounds = 'Invalid Parameter: %s out of bounds';
  sAColumnsCantBeNil = 'AColumns can''t be nil!';
  sTooManyValues = 'Too many values';
  sInvalidUTF8String = 'Invalid UTF8 string';
  sBadNumericExpression = 'Bad numeric expression';
  sUnexpectedNumberFormat = 'Unexpected number format';
  sBadSyntaxForEscapeCharacter = 'Bad syntax for escape character "\"';
  sUnexpectedCharInExpression = 'Unexpected char in expression';
  sInvaildExpression_CharDetectedAfterClosingBracket = 'Invaild expression (char detected after closing bracket)';
  sUnexpectedTokenFound = 'Unexpected token found : "%s"';
  sUnexpectedStringLiteralInExpression = 'Unexpected string literal in expression';
  sBadlyClosedBetweenExpression = 'Badly closed "between" expression';
  sMissingNumberInExpression = 'Missing number in expression';
  sUnexpectedOccurrenceOf_Found = 'Unexpected occurrence of "%s" found';
  sBadBetweenExpression_UnexpectedToken = 'Bad "between" expression. Unexpected "%s"';
  sExpressionError_NoValue = 'Expression error (no value)';

implementation

uses dateutils;

{ CONSTANTS }
const
  IntlDateTimeFormat : TFormatSettings = (
    DateSeparator : '-';
    TimeSeparator : ':';
    ShortDateFormat : 'yyyy/mm/dd';
    LongDateFormat : ' yyyy/mm/dd';
    ShortTimeFormat : 'hh:nn:zzz';
    LongTimeFormat : 'hh:nn:zzz'
  );

{ VARIABLES }

var
  TableRowType: TTableRow = nil;
  MinTimeStampDateTime : TDateTime = 0;

{%region Global functions }

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
     for i:= Low(Str1) to High(Str1) do begin
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

{%region TTimeSpan }

class constructor TTimeSpan.Create;
begin
end;

function TTimeSpan.GetDays: Integer;
begin
  Result :=  FMillis div MillisPerDay;
end;

function TTimeSpan.GetHours: Integer;
begin
  Result := (FMillis div MillisPerHour) mod HoursPerDay;
end;

function TTimeSpan.GetMinutes: Integer;
begin
  Result := (FMillis div MillisPerMinute) mod MinsPerHour;
end;

function TTimeSpan.GetSeconds: Integer;
begin
  Result := (FMillis div MillisPerSecond) mod SecsPerMin;
end;

function TTimeSpan.GetMilliseconds: Integer;
begin
  Result := FMillis mod MillisPerSecond;
end;

function TTimeSpan.GetTotalDays: Double;
begin
  Result := Double(FMillis) / Double(MillisPerDay);
end;

function TTimeSpan.GetTotalHours: Double;
begin
  Result := Double(FMillis) / Double(MillisPerHour);
end;

function TTimeSpan.GetTotalMinutes: Double;
begin
  Result := Double(FMillis) / Double(MillisPerMinute);
end;

function TTimeSpan.GetTotalSeconds: Double;
begin
  Result := Double(FMillis) / Double(MillisPerSecond);
end;

function TTimeSpan.GetTotalMilliseconds: Double;
begin
  Result := Double(FMillis);
end;

class function TTimeSpan.Normalize(const ADateTime : TDateTime) : Int64; static;
begin
  Result := MilliSecondsBetween(ADateTime, MinDateTime);
end;

constructor TTimeSpan.Create(Hours, Minutes, Seconds: Integer);
begin
  Self.FMillis := (Hours*MillisPerHour) + (Minutes*MillisPerMinute) + (Seconds*MillisPerSecond);
end;

constructor TTimeSpan.Create(Days, Hours, Minutes, Seconds: Integer); overload;
begin
  Self.FMillis := Days*MillisPerDay + Hours*MillisPerHour + Minutes*MillisPerMinute + Seconds*MillisPerSecond;
end;

constructor TTimeSpan.Create(Days, Hours, Minutes, Seconds, Milliseconds: Integer); overload;
begin
  Self.FMillis := Days*MillisPerDay + Hours*MillisPerHour + Minutes*MillisPerMinute + Seconds*MillisPerSecond + Milliseconds;
end;

function TTimeSpan.Add(const TS: TTimeSpan): TTimeSpan;
begin
  Result.FMillis := FMillis + TS.FMillis;
end;

function TTimeSpan.Duration: TTimeSpan;
begin
  Result.FMillis := FMillis;
end;

function TTimeSpan.Negate: TTimeSpan;
begin
  Result.FMillis := -FMillis;
end;

function TTimeSpan.Subtract(const TS: TTimeSpan): TTimeSpan;
begin
  Result.FMillis := FMillis - TS.FMillis;
end;

function TTimeSpan.ToString: string;
begin
  Result := Format('[%d]:[%.2d]:[%.2d]:[%.2d].[%.4d]', [GetDays, GetHours, GetMinutes, GetSeconds, GetMilliseconds]);
end;

class function TTimeSpan.FromDays(Value: Double): TTimeSpan;
begin
  Result.FMillis := Round(Value * Double(MillisPerDay));
end;

class function TTimeSpan.FromHours(Value: Double): TTimeSpan; static;
begin
  Result.FMillis := Round(Value * Double(MillisPerHour));
end;

class function TTimeSpan.FromMinutes(Value: Double): TTimeSpan; static;
begin
  Result.FMillis := Round(Value * Double(MillisPerMinute));
end;

class function TTimeSpan.FromSeconds(Value: Double): TTimeSpan; static;
begin
  Result.FMillis := Round(Value * Double(MillisPerSecond));
end;

class function TTimeSpan.FromMilliseconds(Value: Double): TTimeSpan; static;
begin
  Result.FMillis := Round(Value);
end;

class function TTimeSpan.Subtract(const D1, D2: TDateTime): TTimeSpan; static;
begin
  Result.FMillis := Normalize(D1) - Normalize(D2);
end;

class function TTimeSpan.Parse(const S: string): TTimeSpan; static;
begin
  raise ENotImplemented.Create('');
end;

class function TTimeSpan.TryParse(const S: string; out Value: TTimeSpan): Boolean; static;
begin
  raise ENotImplemented.Create('');
end;

class operator TTimeSpan.+(const Left, Right: TTimeSpan) ATimeSpan : TTimeSpan;
begin
  ATimeSpan.FMillis := Left.FMillis + Right.FMillis;
end;

class operator TTimeSpan.+(const Left: TTimeSpan; Right: TDateTime) ADateTime: TDateTime;
begin
  ADateTime := IncMilliSecond(Right, Left.FMillis);
end;

class operator TTimeSpan.+(const Left: TDateTime; Right: TTimeSpan) ADateTime: TDateTime;
begin
  ADateTime := IncMilliSecond(Left, Right.FMillis);
end;

class operator TTimeSpan.-(const Left, Right: TTimeSpan) ATimeSpan : TTimeSpan;
begin
  ATimeSpan.FMillis := Left.FMillis - Right.FMillis;
end;

class operator TTimeSpan.-(const Left: TDateTime; Right: TTimeSpan) ADateTime: TDateTime;
begin
  ADateTime := IncMilliSecond(Left, -Right.FMillis);
end;

class operator TTimeSpan.=(const Left, Right: TTimeSpan) ABool : Boolean;
begin
  ABool := Left.FMillis = Right.FMillis;
end;

class operator TTimeSpan.<>(const Left, Right: TTimeSpan) ABool : Boolean;
begin
  ABool := Left.FMillis <> Right.FMillis;
end;

class operator TTimeSpan.>(const Left, Right: TTimeSpan) ABool : Boolean;
begin
  ABool := Left.FMillis > Right.FMillis;
end;

class operator TTimeSpan.>=(const Left, Right: TTimeSpan) ABool : Boolean;
begin
  ABool := Left.FMillis >= Right.FMillis;
end;

class operator TTimeSpan.<(const Left, Right: TTimeSpan) ABool : Boolean;
begin
  ABool := Left.FMillis < Right.FMillis;
end;

class operator TTimeSpan.<=(const Left, Right: TTimeSpan) ABool : Boolean;
begin
  ABool := Left.FMillis <= Right.FMillis;
end;

class operator TTimeSpan.Negative(const Value: TTimeSpan) ATimeSpan: TTimeSpan;
begin
  ATimeSpan := Value.Negate;
end;

class operator TTimeSpan.Positive(const Value: TTimeSpan) ATimeSpan : TTimeSpan;
begin
  ATimeSpan := Value;
end;

class operator TTimeSpan.Implicit(const Value: TTimeSpan) AString : string;
begin
  AString := Value.ToString;
end;

class operator TTimeSpan.Explicit(const Value: TTimeSpan) AString : string;
begin
  AString := Value.ToString;
end;

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

{ Enums }

function GetSetName(const aSet:PTypeInfo; Value: Integer):string;
var
  vData1 : PTypeData;
  vData2 : PTypeData;
  vCntr  : Integer;
  v: Integer;
begin
  Result := '';
  if aSet^.Kind = tkSet then begin
    vData1 := GetTypeData(aSet);
    vData2 := GetTypeData(vData1^.CompType);
    for vCntr := vData2^.MinValue to vData2^.MaxValue do
      if (Value shr vCntr) and 1 <> 0 then
        Result := Result+ GetEnumName(vData1^.CompType,vCntr)+',';
    if Result <> '' then Delete(Result, Length(Result), 1);
  end;
end;

function GetSetValue(const aSet:PTypeInfo; Name: String): Integer;
var
  vData1 : PTypeData;
  vData2 : PTypeData;
  vCntr  : Integer;
  p      : Integer;
begin
  Result := 0;
  if aSet^.Kind = tkSet then begin
    vData1 := GetTypeData(aSet);
    vData2 := GetTypeData(vData1^.CompType);
    for vCntr := vData2^.MinValue to vData2^.MaxValue do begin
      p := pos(GetEnumName(vData1^.CompType, vCntr), Name);
      if p = 0 then
        Continue;
      if (p = 1) or (Name[p-1] = ',') then
        Result := Result or (1 shl vCntr);
    end;
  end;
end;

{ Clip Value }

function ClipValue( AValue, MinValue, MaxValue: Integer) : Integer;
begin
  if AValue < MinValue then
    Result := MinValue
  else if AValue > MaxValue then
    Result := MaxValue
  else
    Result := AValue
end;

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

{%region Collection Support }

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

class function TArrayTool<T>.Copy(const AArray: array of T): TArray<T>;
begin
  Copy(AArray, 0, Length(AArray));
end;

class function TArrayTool<T>.Copy(const AArray: array of T; FromIndex, Count: SizeInt ): TArray<T>;
var
  i : SizeInt;
begin
  if Count < 0 then raise EArgumentOutOfRangeException.Create('Count was less than 0');
  if (FromIndex + Count) > High(AArray) then raise EArgumentOutOfRangeException.Create('FromIndex + Count was greater than High(AArray)');
  SetLength(Result, Count);
  for i:= FromIndex to FromIndex + Count do
    Result[i - FromIndex] := AArray[i];
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

class procedure TArrayTool<T>.Append(var Arr: TArray<T>; Value: T);
begin
  SetLength(Arr, Length(Arr)+1);
  Arr[High(Arr)] := Value;
end;

class procedure TArrayTool<T>.Prepend(var Arr: TArray<T>; Value: T);
var i : Integer;
begin
  SetLength(Arr, Length(Arr)+1);
  for i := High(Arr)-1 downto Low(Arr) do
    Arr[i+1] := Arr[i];
  Arr[Low(Arr)] := Value;
end;

class procedure TArrayTool<T>.InsertAt(var Values : TArray<T>; ItemIndex : SizeInt; const Item : T);
var i : Integer;
begin
  if (ItemIndex < Low(Values)) OR (ItemIndex > High(Values)) then Raise Exception.CreateFmt(sInvalidParameter_OutOfBounds, ['ItemIndex']);
  SetLength(Values, Length(Values)+1);
  for i := High(Values)-1 downto ItemIndex do
    Values[i+1] := Values[i];
  Values[ItemIndex] := Item;
end;

class procedure TArrayTool<T>.Swap(var Values : array of T; Item1Index, Item2Index : SizeInt);
var temp : T; len, recSize : SizeInt; itemSize : SizeInt;
begin
  len := Length(Values);
  recSize := SizeOf(T);
  if (Item1Index < 0) OR (Item1Index > len) then Raise Exception.CreateFmt(sInvalidParameter_OutOfBounds, ['Item1Index']);
  if (Item2Index < 0) OR (Item2Index > len) then Raise Exception.CreateFmt(sInvalidParameter_OutOfBounds, ['Item2Index']);
  temp := Values[Item1Index];
  Values[Item1Index] := Values[Item2Index];
  Values[Item2Index] := temp;
end;

class procedure TArrayTool<T>.MoveItem(var Values : array of T; FromIndex, ToIndex : SizeInt);
var i : Integer; item : T;
begin
  if (FromIndex < Low(Values)) OR (FromIndex > High(Values)) then Raise Exception.CreateFmt(sInvalidParameter_OutOfBounds, ['FromIndex']);
  if (ToIndex < Low(Values)) OR (ToIndex > High(Values)) then Raise Exception.CreateFmt(sInvalidParameter_OutOfBounds, ['ToIndex']);

  item := Values[FromIndex];
  if FromIndex < ToIndex then begin
    for i := FromIndex + 1 to ToIndex do
      Values[i - 1] := Values[i];
    Values[ToIndex] := item;
  end else if FromIndex > ToIndex then begin
    for i := FromIndex - 1 downto ToIndex do
      Values[i + 1] := Values[i];
    Values[ToIndex] := item;
  end;
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

class function TArrayTool<T>.Create(const item0 : T) : TArray<T>;
begin
  SetLength(result,1);
  result[0] := item0;
end;

class function TArrayTool<T>.Create(const item0 : T; const item1 : T) : TArray<T>;
begin
  SetLength(result,2);
  result[0] := item0;
  result[1] := item1;
end;

class function TArrayTool<T>.Create(const item0 : T; const item1 : T; const item2 : T) : TArray<T>;
begin
  SetLength(result,3);
  result[0] := item0;
  result[1] := item1;
  result[2] := item2;
end;

class function TArrayTool<T>.Create(const item0 : T; const item1 : T; const item2 : T; const item3 : T) : TArray<T>;
begin
  SetLength(result,4);
  result[0] := item0;
  result[1] := item1;
  result[2] := item2;
  result[3] := item3;
end;

class function TArrayTool<T>.Create(const item0 : T; const item1 : T; const item2 : T; const item3 : T; const item4 : T) : TArray<T>;begin
  SetLength(result,5);
  result[0] := item0;
  result[1] := item1;
  result[2] := item2;
  result[3] := item3;
  result[4] := item4;
end;

class function TArrayTool<T>.Create(const item0 : T; const item1 : T; const item2 : T; const item3 : T; const item4 : T; const item5 : T) : TArray<T>;
begin
  SetLength(result,6);
  result[0] := item0;
  result[1] := item1;
  result[2] := item2;
  result[3] := item3;
  result[4] := item4;
  result[5] := item5;
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

class function TListTool<T>.Filter(const AList: TList<T>; const APredicate: TPredicate) : SizeInt;
begin
  Result := Filter(AList, APredicate, idpNone);
end;

class function TListTool<T>.Filter(const AList: TList<T>; const APredicate: TPredicate; AItemDisposePolicy : TItemDisposePolicy) : SizeInt;
var
  i : SizeInt;
  item : T;
begin
  Result := 0;
  i := 0;
  while i <= AList.Count do begin
    if APredicate(AList[i]) then begin
      case AItemDisposePolicy of
          idpNone: ;
          idpNil: AList[i] := nil;
          idpFreeAndNil: begin
            item := AList[i];
            FreeAndNil(item);
            AList[i] := nil;
          end
          else raise ENotSupportedException(Format('TItemDisposePolicy: [%d]', [Ord(AItemDisposePolicy)]));
      end;
      AList.Delete(i);
      inc(Result);
    end else Inc(i);
  end;
end;

{%endregion}


{%region Date/Time Support }

function TimeStamp : AnsiString;
begin
  Result := FormatDateTime('yyy-mm-dd hh:nn:ss', Now);
end;

function UtcTimeStamp : AnsiString;
begin
  raise Exception.Create(sNotImplemented);
end;

function TDateTimeHelper.AddDays(const aNumberOfDays: Integer): TDateTime;
begin
  Result := IncDay(Self, aNumberOfDays);
end;

function TDateTimeHelper.AddHours(const aNumberOfHours: Int64): TDateTime;
begin
  Result := IncHour(Self, aNumberOfHours);
end;

function TDateTimeHelper.AddMilliseconds(const aNumberOfMilliseconds: Int64): TDateTime;
begin
  Result := IncMilliSecond(Self, aNumberOfMilliseconds);
end;

function TDateTimeHelper.AddMinutes(const aNumberOfMinutes: Int64): TDateTime;
begin
  Result := IncMinute(Self, aNumberOfMinutes);
end;

function TDateTimeHelper.AddMonths(const aNumberOfMonths: Integer): TDateTime;
begin
  Result := IncMonth(Self, aNumberOfMonths);
end;

function TDateTimeHelper.AddSeconds(const aNumberOfSeconds: Int64): TDateTime;
begin
  Result := IncSecond(Self, aNumberOfSeconds);
end;

function TDateTimeHelper.AddYears(const aNumberOfYears: Integer): TDateTime;
begin
  Result := IncYear(Self, aNumberOfYears);
end;

function TDateTimeHelper.CompareTo(const aDateTime: TDateTime): TValueRelationship;
begin
  Result := CompareDateTime(Self, aDateTime);
end;

class function TDateTimeHelper.Create(const aYear, aMonth,
  aDay: Word): TDateTime;
begin
  Result := EncodeDate(aYear, aMonth, aDay);
end;

class function TDateTimeHelper.Create(const aYear, aMonth, aDay, aHour, aMinute,
  aSecond, aMillisecond: Word): TDateTime;
begin
  Result := EncodeDateTime(aYear, aMonth, aDay, aHour, aMinute, aSecond, aMillisecond);
end;

function TDateTimeHelper.DaysBetween(const aDateTime: TDateTime): Integer;
begin
  Result := dateutils.DaysBetween(Self, aDateTime);
end;

function TDateTimeHelper.EndOfDay: TDateTime;
begin
  Result := EndOfTheDay(Self);
end;

function TDateTimeHelper.EndOfMonth: TDateTime;
begin
  Result := EndOfTheMonth(Self);
end;

function TDateTimeHelper.EndOfWeek: TDateTime;
begin
  Result := EndOfTheWeek(Self);
end;

function TDateTimeHelper.EndOfYear: TDateTime;
begin
  Result := EndOfTheYear(Self);
end;

function TDateTimeHelper.Equals(const aDateTime: TDateTime): Boolean;
begin
  Result := SameDateTime(Self, aDateTime);
end;

function TDateTimeHelper.GetDate: TDateTime;
begin
  Result := DateOf(Self);
end;

function TDateTimeHelper.GetDay: Word;
begin
  Result := DayOf(Self);
end;

function TDateTimeHelper.GetDayOfWeek: Word;
begin
  Result := DayOfTheWeek(Self);
end;

function TDateTimeHelper.GetDayOfYear: Word;
begin
  Result := DayOfTheYear(Self);
end;

function TDateTimeHelper.GetHour: Word;
begin
  Result := HourOf(Self);
end;

function TDateTimeHelper.GetMillisecond: Word;
begin
  Result := MilliSecondOf(Self);
end;

function TDateTimeHelper.GetMinute: Word;
begin
  Result := MinuteOf(Self);
end;

function TDateTimeHelper.GetMonth: Word;
begin
  Result := MonthOf(Self);
end;

class function TDateTimeHelper.GetNow: TDateTime;
begin
  Result := SysUtils.Now;
end;

function TDateTimeHelper.GetSecond: Word;
begin
  Result := SecondOf(Self);
end;

function TDateTimeHelper.GetTime: TDateTime;
begin
  Result := TimeOf(Self);
end;

class function TDateTimeHelper.GetToday: TDateTime;
begin
  Result := SysUtils.Date;
end;

class function TDateTimeHelper.GetTomorrow: TDateTime;
begin
  Result := SysUtils.Date + 1;
end;

function TDateTimeHelper.GetYear: Word;
begin
  Result := YearOf(Self);
end;

class function TDateTimeHelper.GetYesterDay: TDateTime;
begin
  Result := SysUtils.Date - 1;
end;

function TDateTimeHelper.HoursBetween(const aDateTime: TDateTime): Int64;
begin
  Result := DateUtils.HoursBetween(Self, aDateTime);
end;

function TDateTimeHelper.InRange(const aStartDateTime, aEndDateTime: TDateTime; const aInclusive: Boolean): Boolean;
begin
  if aInclusive then
    Result := (aStartDateTime <= self) AND (self <= aEndDateTime)
  else
    Result := (aStartDateTime < self) AND (self < aEndDateTime);
end;

function TDateTimeHelper.IsAM: Boolean;
begin
  Result := NOT dateutils.IsPM(self);
end;

function TDateTimeHelper.IsInLeapYear: Boolean;
begin
  Result := dateutils.IsInLeapYear(Self);
end;

function TDateTimeHelper.IsPM: Boolean;
begin
  Result := dateutils.IsPM(Self);
end;

function TDateTimeHelper.IsSameDay(const aDateTime: TDateTime): Boolean;
begin
  Result := DateUtils.IsSameDay(Self, aDateTime);
end;

function TDateTimeHelper.IsToday: Boolean;
begin
  Result := DateUtils.IsToday(Self);
end;

function TDateTimeHelper.MilliSecondsBetween(const aDateTime: TDateTime): Int64;
begin
  Result := DateUtils.MilliSecondsBetween(Self, aDateTime);
end;

function TDateTimeHelper.MinutesBetween(const aDateTime: TDateTime): Int64;
begin
  Result := DateUtils.MinutesBetween(Self, aDateTime);
end;

function TDateTimeHelper.MonthsBetween(const aDateTime: TDateTime): Integer;
begin
  Result := DateUtils.MonthsBetween(Self, aDateTime);
end;

function TDateTimeHelper.SecondsBetween(const aDateTime: TDateTime): Int64;
begin
  Result := DateUtils.SecondsBetween(Self, aDateTime);
end;

function TDateTimeHelper.StartOfDay: TDateTime;
begin
  Result := StartOfTheDay(Self);
end;

function TDateTimeHelper.StartOfMonth: TDateTime;
begin
  Result := StartOfTheMonth(Self);
end;

function TDateTimeHelper.StartOfWeek: TDateTime;
begin
  Result := StartOfTheWeek(Self);
end;

function TDateTimeHelper.StartOfYear: TDateTime;
begin
  Result := StartOfTheYear(Self);
end;

function TDateTimeHelper.ToString(const aFormatStr: string): string;
begin
  if aFormatStr = '' then
    Result := DateToStr(Self)
  else
    Result := FormatDateTime(aFormatStr, Self);
end;

function TDateTimeHelper.WeeksBetween(const aDateTime: TDateTime): Integer;
begin
  Result := DateUtils.WeeksBetween(Self, aDateTime);
end;

function TDateTimeHelper.WithinDays(const aDateTime: TDateTime;
  const aDays: Integer): Boolean;
begin
  Result := DateUtils.WithinPastDays(Self, aDateTime, aDays);
end;

function TDateTimeHelper.WithinHours(const aDateTime: TDateTime;
  const aHours: Int64): Boolean;
begin
  Result := DateUtils.WithinPastHours(Self, aDateTime, aHours);
end;

function TDateTimeHelper.WithinMilliseconds(const aDateTime: TDateTime;
  const AMilliseconds: Int64): Boolean;
begin
  Result := DateUtils.WithinPastMilliSeconds(Self, aDateTime, AMilliseconds);
end;

function TDateTimeHelper.WithinMinutes(const aDateTime: TDateTime;
  const aMinutes: Int64): Boolean;
begin
  Result := DateUtils.WithinPastMinutes(Self, aDateTime, aMinutes);
end;

function TDateTimeHelper.WithinMonths(const aDateTime: TDateTime;
  const aMonths: Integer): Boolean;
begin
  Result := DateUtils.WithinPastMonths(Self, aDateTime, aMonths);
end;

function TDateTimeHelper.WithinSeconds(const aDateTime: TDateTime;
  const aSeconds: Int64): Boolean;
begin
  Result := DateUtils.WithinPastSeconds(Self, aDateTime, aSeconds);
end;

function TDateTimeHelper.WithinWeeks(const aDateTime: TDateTime;
  const aWeeks: Integer): Boolean;
begin
  Result := DateUtils.WithinPastWeeks(Self, aDateTime, aWeeks);
end;

function TDateTimeHelper.WithinYears(const aDateTime: TDateTime;
  const aYears: Integer): Boolean;
begin
  Result := DateUtils.WithinPastYears(Self, aDateTime, aYears);
end;

function TDateTimeHelper.YearsBetween(const aDateTime: TDateTime): Integer;
begin
  Result := DateUtils.YearsBetween(Self, aDateTime);
end;

{%endregion}

{% Comparer Support }

constructor TNestedComparer<T>.Create(const comparer: TNestedComparison<T>);
begin
  FComparer := comparer;
end;

function TNestedComparer<T>.Compare(constref Left, Right: T): Integer;
begin
  Result := FComparer(Left, Right);
end;

constructor TManyComparer<T>.Create(const comparers: TArray<IComparer_T>);
begin
  FComparers := comparers;
end;

destructor TManyComparer<T>.Destroy;
begin
  FComparers := nil;
  inherited;
end;

function TManyComparer<T>.Compare(constref Left, Right: T): Integer;
var
  i : Integer;
begin
  if Length(FComparers) = 0 then
    raise Exception.Create('No comparers defined');
  for i := Low(FComparers) to High(FComparers) do begin
    Result := FComparers[i].Compare(Left, Right);
    if (Result <> 0) or (i = High(FComparers)) then exit;
  end;
end;

class function TManyComparer<T>.Construct(const comparers: array of TNestedComparison<T>) : IComparer<T>;
var
  i : Integer;
  internalComparers : TArray<IComparer_T>;
begin
  SetLength(internalComparers, Length(comparers));
  for i := 0 to High(comparers) do
    internalComparers[i] := TNestedComparer<T>.Create(comparers[i]);
  Create(internalComparers);
end;

class function TManyComparer<T>.Construct(const comparers: array of TOnComparison<T>) : IComparer<T>;
var
  i : Integer;
  internalComparers : TArray<IComparer_T>;
begin
  SetLength(internalComparers, Length(comparers));
  for i := 0 to High(comparers) do
    internalComparers[i] := TComparer<T>.Construct(comparers[i]);
  Create(internalComparers);
end;

class function TManyComparer<T>.Construct(const comparers: array of TComparisonFunc<T>) : IComparer<T>;
var
  i : Integer;
  internalComparers : TArray<IComparer_T>;
begin
  SetLength(internalComparers, Length(comparers));
  for i := 0 to High(comparers) do
    internalComparers[i] := TComparer<T>.Construct(comparers[i]);
  Create(internalComparers);
end;

class function TManyComparer<T>.Construct(const comparers: array of TComparer<T>) : IComparer<T>;
var
  i : Integer;
  internalComparers : TArray<IComparer_T>;
begin
  SetLength(internalComparers, Length(comparers));
  for i := 0 to High(comparers) do
    internalComparers[i] := IComparer_T(comparers[i]);
  Create(internalComparers);
end;

{%endegion }


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
    FillChar(LDestRow, SizeOf(LDestRow), #0);
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
    raise ETableRow.Create(sAColumnsCantBeNil);

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
  for i := low(self) to high(self) do
    self[i](sender);
end;

{%endregion}

{%region TNotifyManyEventHelperEx}

procedure TNotifyManyEventExHelper.Add(listener : TNotifyEventEx);
begin
  if TArrayTool<TNotifyEventEx>.IndexOf(self, listener) = -1 then begin
    TArrayTool<TNotifyEventEx>.Add(self, listener);
  end;
end;

procedure TNotifyManyEventExHelper.Remove(listener : TNotifyEventEx);
begin
  TArrayTool<TNotifyEventEx>.Remove(self, listener);
end;

procedure TNotifyManyEventExHelper.Invoke(sender : TObject; const args: array of Pointer);
var i : Integer;
begin
  for i := Low(Self) to high(Self) do
    self[i](sender, args);
end;

{%endregion}

{ TSearchExpressionService }

class procedure TSearchExpressionService.Parse(const AExpression: utf8string;
  const AExpressionKind: TExpressionKind; out
  AExpressionRecord: TExpressionRecord);
const
  MAX_VALUES = 2;
type
  TToken = (tkNone, tkPercent, tkLess, tkGreater, tkEqual, tkLessOrEqual,
    tkGreaterOrEqual, tkOpeningParenthesis, tkClosingParenthesis,
    tkOpeningBracket, tkClosingBracket,  tkText, tkNum, tkComma);

  TUTF8Char = record
    Length: byte;
    Char: array [0..3] of AnsiChar;
  end;

const
  CONVERTABLE_TOKENS_TO_STR = [tkLess..tkClosingBracket, tkComma];
  NUM_OPERATORS = [tkLess..tkGreaterOrEqual];

  procedure GetChar(APos: PAnsiChar; out AChar: TUTF8Char);
  begin
    AChar.Length := UTF8CharacterLength(APos);

    if AChar.Length >= 1 then AChar.Char[0] := APos[0];
    if AChar.Length >= 2 then AChar.Char[1] := APos[1];
    if AChar.Length >= 3 then AChar.Char[2] := APos[2];
    if AChar.Length = 4 then AChar.Char[3] := APos[3];
  end;

  function TokenToStr(AToken: TToken): utf8string;
  const
    CONVERTER: array[TToken] of utf8string = (
      'NONE', '%', '<', '>', '=', '<=', '>=', '(', ')', '[', ']', 'TEXT',
      'NUMBER', ','
    );
  begin
    Result := CONVERTER[AToken];
  end;

var
  c, nc: PAnsiChar;
  i: Integer;
  LDot: boolean = false;
  LChar: TUTF8Char;
  LValueIdx: Integer = -1;
  LValues: array[0..MAX_VALUES-1] of utf8string; // for now only 2 values for set "between"
  LValue: PUTF8String;
  LToken: TToken = tkNone;
  LPrevToken: TToken = tkNone;
  LExpression: utf8string;
  LLastPercent: boolean = false;
  LExpressionKind: TExpressionKind;

  procedure NextValue;
  begin
    Inc(LValueIdx);
    if LValueIdx > MAX_VALUES - 1 then
      raise ESearchExpressionParserException.Create(sTooManyValues);
    LValue := @LValues[LValueIdx];
  end;

  procedure EscapeSequence(AChar: Char);
  begin
    LToken := tkText;
    LValue^ := LValue^ + AChar;
    Inc(c);
  end;

begin
  AExpressionRecord := Default(TExpressionRecord);
  if AExpression = '' then
    Exit;

  LExpressionKind := AExpressionKind;
  // more simple parsing loop
  if AExpressionKind in [ekSet, ekNum] then
    LExpression:=Trim(AExpression)
  else
    LExpression:=AExpression;

  c := @LExpression[1];
  if FindInvalidUTF8Character(c, Length(LExpression)) <> -1 then
    raise ESearchExpressionParserException.Create(sInvalidUTF8String);

  NextValue;
  repeat
    // simple scanner
    GetChar(c, LChar);
    if LChar.Length = 1 then
      case LChar.Char[0] of
        #0: Break;
        #1..#32:
          case LExpressionKind of
            ekSet:
              begin
                while c^ in [#1..#32] do Inc(c);
                Continue;
              end;
            ekText, ekUnknown:
              begin
                LValue^:=LValue^+LChar.Char[0];
                LToken:=tkText;
              end;
            ekNum:
              if not (LPrevToken in NUM_OPERATORS) then
                raise ESearchExpressionParserException.Create(sBadNumericExpression)
              else
              begin
                while c^ in [#1..#32] do Inc(c);
                continue;
              end;
          end;
        '0'..'9':
          begin
            repeat
              if c^ = '.' then
                if LDot then
                  raise ESearchExpressionParserException.Create(sUnexpectedNumberFormat)
                else
                  LDot:=true;
              LValue^:=LValue^+c^;
              Inc(c);
            until not (c^ in ['0'..'9', '.']);
            Dec(c);
            case LExpressionKind of
              ekUnknown, ekSet, ekNum: LToken:=tkNum;
              ekText: LToken:=tkText;
            end;
          end;
        '%':
          begin
            if not (LExpressionKind in [ekText,ekUnknown]) then
              ESearchExpressionParserException.Create(sBadNumericExpression);

            LToken := tkPercent;
          end;
        '\':
          begin
            if not (LExpressionKind in [ekText,ekUnknown]) then
              ESearchExpressionParserException.Create(sBadNumericExpression);
            case (c+1)^ of
              '%': EscapeSequence('%');
              '\': EscapeSequence('\');
              '[': EscapeSequence('[');
              '(': EscapeSequence('(');
              ']': EscapeSequence(']');
              ')': EscapeSequence(')');
              '<': EscapeSequence('<');
              '>': EscapeSequence('>');
              '=': EscapeSequence('=');
            else
              raise ESearchExpressionParserException.Create(sBadSyntaxForEscapeCharacter);
            end
          end;
        '<':
          if (c+1)^ = '=' then
          begin
            LToken := tkLessOrEqual;
            Inc(c);
          end
          else
            LToken := tkLess;
        '>':
          if (c+1)^ = '=' then
          begin
            LToken := tkGreaterOrEqual;
            Inc(c);
          end
          else
            LToken := tkGreater;
        '=': LToken := tkEqual;
        '(': LToken := tkOpeningParenthesis;
        ')': LToken := tkClosingParenthesis;
        '[': LToken := tkOpeningBracket;
        ']': LToken := tkClosingBracket;
        ',': LToken := tkComma;
      else
        LValue^ := LValue^ + LChar.Char[0];
        LToken:=tkText;
      end
    else
    begin
      if not (LExpressionKind in [ekUnknown, ekText]) then
        raise ESearchExpressionParserException.Create(sUnexpectedCharInExpression);
      SetLength(LValue^, Length(LValue^) + LChar.Length);
      Move(LChar.Char[0], LValue^[Succ(Length(LValue^) - LChar.Length)], LChar.Length);
      LToken:=tkText;
    end;

    // parser is able to deduce expression kind (if needed)
    if LExpressionKind = ekUnknown then
    case LToken of
      tkPercent, tkText, tkComma: LExpressionKind:=ekText;
      tkOpeningBracket, tkOpeningParenthesis: LExpressionKind:=ekSet;
      tkLess..tkGreaterOrEqual, tkNum: LExpressionKind:=ekNum;
    else
      raise ESearchExpressionParserException.Create(sUnexpectedCharInExpression);
    end;

    // text mode has precedence (parsing the expressions like: 123abs)
    if (LExpressionKind = ekNum) and (AExpressionKind = ekUnknown)
      and (LToken in [tkText, tkPercent]) and (AExpressionRecord.NumericComparisionKind = nckUnknown) then
    begin
      LExpressionKind := ekText;
    end;

    // text mode is special so part of tokens are used as normal characters
    if (LExpressionKind = ekText) and (LToken in CONVERTABLE_TOKENS_TO_STR) then
      LValue^:=LValue^+TokenToStr(LToken);

    if LPrevToken in [tkClosingBracket, tkClosingParenthesis] then
      raise ESearchExpressionParserException.Create(sInvaildExpression_CharDetectedAfterClosingBracket);

    // rules
    case LToken of
      tkNum:
        if LExpressionKind = ekSet then
          if not (LPrevToken in [tkOpeningBracket, tkOpeningParenthesis, tkComma]) then
            raise ESearchExpressionParserException.CreateFmt(sUnexpectedTokenFound, [TokenToStr(LToken)]);
      tkText:
        if LExpressionKind in [ekSet, ekNum] then
          raise ESearchExpressionParserException.Create(sUnexpectedStringLiteralInExpression);
      tkClosingBracket:
        if (LExpressionKind = ekSet) then
          if (AExpressionRecord.SetKind<>skNumericBetweenInclusive) then
            raise ESearchExpressionParserException.Create(sBadlyClosedBetweenExpression)
          else if LPrevToken <> tkNum then
            raise ESearchExpressionParserException.Create(sMissingNumberInExpression);
      tkClosingParenthesis:
        if (LExpressionKind = ekSet) then
          if (AExpressionRecord.SetKind<>skNumericBetweenExclusive) then
            raise ESearchExpressionParserException.Create(sBadlyClosedBetweenExpression)
          else if LPrevToken <> tkNum then
            raise ESearchExpressionParserException.Create(sMissingNumberInExpression);
      tkComma:
        if LExpressionKind = ekSet then
          if not (LPrevToken = tkNum) then
            raise ESearchExpressionParserException.CreateFmt(sUnexpectedOccurrenceOf_Found, [','])
          else
            NextValue;
      tkPercent:
        if LExpressionKind = ekText then
        begin
          if LLastPercent then
            raise ESearchExpressionParserException.CreateFmt(sUnexpectedOccurrenceOf_Found, ['%']);
          case LPrevToken of
            tkText, tkNum: // tkNum is here because is possible to parse: 123%
              begin
                if (AExpressionRecord.TextMatchKind = tmkUnknown) then
                  AExpressionRecord.TextMatchKind:=tmkMatchTextEnd
                else
                  AExpressionRecord.TextMatchKind:=tmkMatchTextAnywhere;
                LLastPercent:=true;
              end;
            tkNone:
              AExpressionRecord.TextMatchKind:=tmkMatchTextBeginning;
            tkPercent:
              raise ESearchExpressionParserException.CreateFmt(sUnexpectedOccurrenceOf_Found, ['%']);
          end;
        end
        else
          raise ESearchExpressionParserException.CreateFmt(sUnexpectedOccurrenceOf_Found, ['%']);
      tkLess..tkGreaterOrEqual:
        case LExpressionKind of
          ekNum:
            if LPrevToken <> tkNone then
              raise ESearchExpressionParserException.Create(sBadNumericExpression)
            else
              with AExpressionRecord do
              case LToken of
                tkLess: NumericComparisionKind:=nckNumericLT;
                tkGreater: NumericComparisionKind:=nckNumericGT;
                tkEqual: NumericComparisionKind:=nckNumericEQ;
                tkLessOrEqual: NumericComparisionKind:=nckNumericLTE;
                tkGreaterOrEqual: NumericComparisionKind:=nckNumericGTE;
              end;
          ekSet:
            raise ESearchExpressionParserException.CreateFmt(sUnexpectedTokenFound, [TokenToStr(LToken)]);
        end;
      tkOpeningParenthesis, tkOpeningBracket:
        if LExpressionKind = ekSet then
          if LPrevToken <> tkNone then
            raise ESearchExpressionParserException.CreateFmt(sBadBetweenExpression_UnexpectedToken, [TokenToStr(LToken)])
          else
          with AExpressionRecord do
          case LToken of
            tkOpeningParenthesis: SetKind:=skNumericBetweenExclusive;
            tkOpeningBracket: SetKind:=skNumericBetweenInclusive;
          end;
    end;
    LPrevToken := LToken;
    Inc(c, LChar.Length);
  until (LChar.Length=0) or (c^ = #0);

  case LExpressionKind of
    ekText:
      if AExpressionRecord.TextMatchKind = tmkUnknown then
        AExpressionRecord.TextMatchKind:=tmkMatchTextExact;
    ekSet:
      case AExpressionRecord.SetKind of
        skNumericBetweenInclusive:
          if LPrevToken <> tkClosingBracket then
            raise ESearchExpressionParserException.Create(sBadlyClosedBetweenExpression);
        skNumericBetweenExclusive:
          if LPrevToken <> tkClosingParenthesis then
            raise ESearchExpressionParserException.Create(sBadlyClosedBetweenExpression);
      end;
  end;

  if (LValueIdx = 0) and (LValue^='') then
    raise ESearchExpressionParserException.Create(sExpressionError_NoValue);

  SetLength(AExpressionRecord.Values, LValueIdx + 1);
  for i := 0 to LValueIdx do
    AExpressionRecord.Values[i] := LValues[i];

  AExpressionRecord.Kind := LExpressionKind;
end;

class function TSearchExpressionService.Parse(const AExpression: utf8string
  ): TExpressionRecord;
begin
  Result.Kind := ekUnknown;
  Parse(AExpression, Result.Kind, Result);
end;


initialization
  TableRowType := TTableRow.Create;
  MinTimeStampDateTime:= StrToDateTime('1980-01-01 00:00:000', IntlDateTimeFormat);

finalization
  TableRowType.Free;
end.

