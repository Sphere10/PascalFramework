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

initialization
  RegisterTest(TMiscTests);
end.

