{
  Copyright (c) 2017 Sphere 10 Software

  Author: Herman Schoenfeld <herman@sphere10.com>

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  Additional Credits:
    <contributors add yourselves here>
}

unit UCommon;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Controls, FGL;

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

  { TDictionary }
  TDictionary<TKey, TData> = class(specialize TFPGMap<TKey, TData>)
    property At[AKey : TKey] : TData read GetKeyData write PutKeyData;
    procedure AddOrSetValue(const AKey : TKey; AData : TData);
    function TryGetValue(const AKey : TKey; var AValue : TData) : Boolean;
    function GetOrDefault(const AKey : TKey) : TData;
    function ContainsKey(const AKey : TKey) : Boolean;
    function ContainsValue(AValue : TData) : Boolean;
  end;

  { Tool for manipulating arrays }
  TArrayTool<T> = class
  public
    class procedure Append(var arr : TArray<T>; item : T); static;
    class function Remove(var arr : TArray<T>; item : T) : Integer; static;
    class procedure RemoveAt(var arr: TArray<T>; const index: Cardinal); static;
    class function Concat(const arrays: array of TArray<T>) : TArray<T>; static;
    class function Create(const a : T; const b : T) : TArray<T>; static;
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
  end;

implementation

{% Global functions %}

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

{%endregion

{%region TDictionary }

procedure TDictionary<TKey, TData>.AddOrSetValue(const AKey : TKey; AData : TData);
begin
    if ContainsKey(AKey) then
        KeyData[AKey] := AData
    else
        Add(AKey, AData);
end;

function TDictionary<TKey, TData>.TryGetValue(const AKey : TKey; var AValue : TData) : Boolean;
begin
    Result := False;
    if ContainsKey(AKey) then
    begin
        AValue := KeyData[AKey];
        Result := True;
    end;
end;

function TDictionary<TKey, TData>.GetOrDefault(const AKey : TKey) : TData;
begin
  Result := Default(TData);
  if self.ContainsKey(AKey) then
    Result := self.At[AKey]
end;

function TDictionary<TKey, TData>.ContainsKey(const AKey : TKey) : Boolean;
begin
    Result := IndexOf(AKey) <> -1;
end;

function TDictionary<TKey, TData>.ContainsValue(AValue : TData) : Boolean;
begin
    Result := IndexOfData(AValue) <> - 1;
end;

{%endregion}

{%region ArrayTool }

class procedure TArrayTool<T>.Append(var arr : TArray<T>; item : T);
begin
  if arr = nil then exit;
  SetLength(arr, Length(arr) + 1);
  arr[High(arr)] := item;
end;


class function TArrayTool<T>.Remove(var arr : TArray<T>; item : T) : Integer;
var index : Integer;
begin
end;

class procedure TArrayTool<T>.RemoveAt(var arr: TArray<T>; const Index: Cardinal);
var
  ALength: Cardinal;
  TailElements: Cardinal;
begin
  ALength := Length(arr);
  Assert(ALength > 0);
  Assert(Index < ALength);
  Finalize(arr[Index]);
  TailElements := ALength - Index;
  if TailElements > 0 then
    Move(arr[Index + 1], arr[Index], SizeOf(T) * TailElements);
  Initialize(arr[ALength - 1]);
  SetLength(arr, ALength - 1);
end;

class function TArrayTool<T>.Concat(const arrays: array of TArray<T>): TArray<T>;
var
  i, k, LIndex, LLength: Integer;
begin
  LLength := 0;
  for i := 0 to High(arrays) do
    Inc(LLength, Length(arrays[i]));
  SetLength(Result, LLength);
  LIndex := 0;
  for i := 0 to High(arrays) do
  begin
    for k := 0 to High(arrays[i]) do
    begin
      Result[LIndex] := arrays[i][k];
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

{%endregion}

{%region TNotifyManyEventHelper }

procedure TNotifyManyEventHelper.Add(listener : TNotifyEvent);
var x : TArray<AnsiString>;
begin
 x.
 { if self = nil then
    SetLength(self, 1);
  self[0] := listener;}
end;

procedure TNotifyManyEventHelper.Remove(listener : TNotifyEvent);
begin

end;

procedure TNotifyManyEventHelper.Invoke(sender : TObject);
begin
  if self = nil then exit;
end;

{%endregion}


end.

