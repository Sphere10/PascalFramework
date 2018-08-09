unit UMiscTests;

{$mode delphi}
{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TMiscTests = class(TTestCase)
    published
      procedure SortedHashSet_Large;
      procedure SortedHashSet_Clear;
      procedure SortedHashSet_AddRange;
      procedure Bytes2Hex_1;
      procedure Bytes2Hex_2;
      procedure Bytes2Hex_3;
      procedure Bytes2Hex_4;
      procedure Hex2Bytes_1;
      procedure Hex2Bytes_2;
      procedure Hex2Bytes_3;
      procedure Hex2Bytes_4;
  end;

implementation

uses Generics.Defaults, Generics.Collections, UCommon, UCommon.Collections, UMemory;

procedure TMiscTests.SortedHashSet_Large;
var
  i : integer;
  c : Cardinal;
  LSet : TSortedHashSet<Cardinal>;
  GC : TDisposables;
begin
  LSet := GC.AddObject( TSortedHashSet<Cardinal>.Create ) as TSortedHashSet<Cardinal>;

  // Add 1 to 1000000 in non-ordered manner
  for i := 1 to 1000000 do begin
    if i mod 2 = 0 then LSet.Add(i);
  end;

  for i := 1000000 downto 1 do begin
    if i mod 2 = 1 then LSet.Add(i);
  end;

  // Add 1 to 1000000 again, should not do anything (since set)
  for i := 1 to 1000000 do begin
    LSet.Add(i);
  end;

  // Ensure enumerates ordered manner
  i := 1;
  for c in LSet do begin
    AssertEquals(i, c);
    Inc(i);
  end;
end;

procedure TMiscTests.SortedHashSet_Clear;
var
  i : integer;
  c : Cardinal;
  LSet : TSortedHashSet<Cardinal>;
  GC : TDisposables;
begin
  LSet := GC.AddObject( TSortedHashSet<Cardinal>.Create ) as TSortedHashSet<Cardinal>;

  for i := 1 to 1000 do  LSet.Add(i);
  LSet.Clear;
  for i := 1001 to 2000 do  LSet.Add(i);

  // Ensure enumerates ordered manner
  i := 1001;
  for c in LSet do begin
    AssertEquals(i, c);
    Inc(i);
  end;
end;

procedure TMiscTests.SortedHashSet_AddRange;
var
  i, j : integer;
  c : Cardinal;
  LVals : TArray<Cardinal>;
  LSet : TSortedHashSet<Cardinal>;
  GC : TDisposables;
begin
  LSet := GC.AddObject( TSortedHashSet<Cardinal>.Create ) as TSortedHashSet<Cardinal>;

  // Prepare addrange input
  SetLength(LVals, 1000);
  for i := 0 to 999 do
    LVals[i] := i;
  // shuffle randomly
  for i := 0 to 999 do begin
    TArrayTool<Cardinal>.Swap(LVals, i, Random(999));
  end;

  LSet.AddRange(LVals);
  LSet.Clear;
  LSet.AddRange(LVals);

  // Ensure enumerates ordered manner
  i := 0;
  for c in LSet do begin
    AssertEquals(i, c);
    Inc(i);
  end;
end;

procedure TMiscTests.Bytes2Hex_1;
var B : TBytes;
begin
  SetLength(B, 0);
  AssertEquals('', Bytes2Hex(B));
end;

procedure TMiscTests.Bytes2Hex_2;
begin
  AssertEquals('00FF', Bytes2Hex(TBytes.Create(0, 255)));
end;

procedure TMiscTests.Bytes2Hex_3;
begin
  AssertEquals('00FFFE', Bytes2Hex(TBytes.Create(0, 255, 254)));
end;

procedure TMiscTests.Bytes2Hex_4;
begin
  AssertEquals('0x00FFFE', Bytes2Hex(TBytes.Create(0, 255, 254), True));
end;

procedure TMiscTests.Hex2Bytes_1;
var B : TBytes;
begin
  SetLength(B, 0);
  AssertEquals(0, BytesCompare (B, Hex2Bytes('')));
end;

procedure TMiscTests.Hex2Bytes_2;
begin
  AssertEquals(0, BytesCompare (TBytes.Create(0, 255), Hex2Bytes('00FF')));
end;

procedure TMiscTests.Hex2Bytes_3;
begin
  AssertEquals(0, BytesCompare( TBytes.Create(0, 255, 254), Hex2Bytes('00FFFE')));
end;

procedure TMiscTests.Hex2Bytes_4;
begin
  AssertEquals(0, BytesCompare( Hex2Bytes('0x00FFFE'), Hex2Bytes('00FFFE')));
end;

initialization
  RegisterTest(TMiscTests);
end.

