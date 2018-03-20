unit UFilterTests;

{$mode delphi}
{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TFilterTests = class(TTestCase)
    private
      function ObjectFilterFunc(constref AItem : Integer ) : boolean;
    protected
      procedure SetUp; override;
      procedure TearDown; override;
    published
      procedure NestedFilter;
      procedure ObjectFilter;
      procedure GlobalFilter;
      procedure AndManyFilter;
      procedure OrManyFilter;
  end;

implementation

uses Generics.Defaults, Generics.Collections, UCommon, UCommon.Collections, UMemory, LazLogger;

procedure TFilterTests.NestedFilter;
var
  list : TList<Integer>;
  pred : IPredicate<Integer>;
  res : Integer;
  GC : TDisposables;
  i : integer;

  function NestedFilterFunc(constref AItem : Integer ) : boolean;
  begin
    Result := (AItem mod 2) = 0;
  end;

begin
  list := GC.AddObject( TList<Integer>.Create ) as TList<Integer>;
  list.AddRange([1,2,3,4,5,6,7,8,9,10]);
  pred := TPredicateTool<Integer>.FromFunc( NestedFilterFunc );
  TListTool<Integer>.FilterBy(list, pred);
  self.AssertEquals(5, list.Count);
  self.AssertEquals(2, list[0]);
  self.AssertEquals(4, list[1]);
  self.AssertEquals(6, list[2]);
  self.AssertEquals(8, list[3]);
  self.AssertEquals(10, list[4]);
end;

function TFilterTests.ObjectFilterFunc(constref AItem : Integer ) : boolean;
begin
  Result := (AItem mod 2) = 0;
end;

procedure TFilterTests.ObjectFilter;
var
  list : TList<Integer>;
  pred : IPredicate<Integer>;
  res : Integer;
  GC : TDisposables;
begin
  list := GC.AddObject( TList<Integer>.Create ) as TList<Integer>;
  list.AddRange([1,2,3,4,5,6,7,8,9,10]);
  pred := TPredicateTool<Integer>.FromFunc( ObjectFilterFunc );
  TListTool<Integer>.FilterBy(list, pred);
  self.AssertEquals(5, list.Count);
  self.AssertEquals(2, list[0]);
  self.AssertEquals(4, list[1]);
  self.AssertEquals(6, list[2]);
  self.AssertEquals(8, list[3]);
  self.AssertEquals(10, list[4]);
end;

function GlobalCompareFunc(constref AItem : Integer ) : boolean;
begin
  Result := (AItem mod 2) = 0;
end;

procedure TFilterTests.GlobalFilter;
var
  list : TList<Integer>;
  pred : IPredicate<Integer>;
  res : Integer;
  GC : TDisposables;
begin
  list := GC.AddObject( TList<Integer>.Create ) as TList<Integer>;
  list.AddRange([1,2,3,4,5,6,7,8,9,10]);
  pred := TPredicateTool<Integer>.FromFunc( GlobalCompareFunc );
  TListTool<Integer>.FilterBy(list, pred);
  self.AssertEquals(5, list.Count);
  self.AssertEquals(2, list[0]);
  self.AssertEquals(4, list[1]);
  self.AssertEquals(6, list[2]);
  self.AssertEquals(8, list[3]);
  self.AssertEquals(10, list[4]);
end;


procedure TFilterTests.AndManyFilter;
var
  list : TList<Integer>;
  pred : IPredicate<Integer>;
  res : Integer;
  GC : TDisposables;

  function NestedFilterOddFunc(constref AItem : Integer ) : boolean;
  begin
    Result := (AItem mod 2) = 0;
  end;

    function NestedFilterEvenFunc(constref AItem : Integer ) : boolean;
  begin
    Result := (AItem mod 2) <> 0;
  end;

begin
  list := GC.AddObject( TList<Integer>.Create ) as TList<Integer>;
  list.AddRange([1,2,3,4,5,6,7,8,9,10]);
  pred := TPredicateTool<Integer>.AndMany( [NestedFilterOddFunc, NestedFilterEvenFunc] );
  TListTool<Integer>.FilterBy(list, pred);
  self.AssertEquals(0, list.Count);
end;


procedure TFilterTests.OrManyFilter;
var
  list : TList<Integer>;
  pred : IPredicate<Integer>;
  res : Integer;
  GC : TDisposables;

  function NestedFilterOddFunc(constref AItem : Integer ) : boolean;
  begin
    Result := (AItem mod 2) = 0;
  end;

    function NestedFilterEvenFunc(constref AItem : Integer ) : boolean;
  begin
    Result := (AItem mod 2) <> 0;
  end;

begin
  list := GC.AddObject( TList<Integer>.Create ) as TList<Integer>;
  list.AddRange([1,2,3,4,5,6,7,8,9,10]);
  pred := TPredicateTool<Integer>.OrMany( [NestedFilterOddFunc, NestedFilterEvenFunc] );
  TListTool<Integer>.FilterBy(list, pred);
  self.AssertEquals(10, list.Count);
end;


procedure TFilterTests.SetUp;
begin

end;

procedure TFilterTests.TearDown;
begin

end;

initialization

  RegisterTest(TFilterTests);
end.

