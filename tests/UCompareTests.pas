unit UCompareTests;

{$mode delphi}
{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TCompareTests = class(TTestCase)
    private
      function ObjectCompareFunc(constref Left, Right : Integer ) : Integer;
    protected
      procedure SetUp; override;
      procedure TearDown; override;
    published
      procedure NestedCompare;
      procedure ObjectCompare;
      procedure GlobalCompare;
      procedure ManyTest_LE;
      procedure ManyTest_EQ;
      procedure ManyTest_GE;
      procedure MemTest;
  end;

implementation

uses Generics.Defaults, Generics.Collections, UCommon, UCommon.Collections;

procedure TCompareTests.NestedCompare;
var
  cmp : IComparer<Integer>;
  res : Integer;

  function NestedCompareFunc(constref Left, Right : Integer ) : Integer;
  begin
    Result := TCompare.Integer(Left, Right);
  end;

begin
  cmp := TComparerTool<Integer>.FromFunc( NestedCompareFunc );
  res := cmp.Compare(2,3);
  self.AssertEquals(-1, res);
end;

function TCompareTests.ObjectCompareFunc(constref Left, Right : Integer ) : Integer;
begin
  Result := TCompare.Integer(Left, Right);
end;

procedure TCompareTests.ObjectCompare;
var
  cmp : IComparer<Integer>;
  res : Integer;

begin
  cmp := TComparerTool<Integer>.FromFunc( ObjectCompareFunc );
  res := cmp.Compare(2,3);
  self.AssertEquals(-1, res);
end;

function GlobalCompareFunc(constref Left, Right : Integer ) : Integer;
begin
  Result := TCompare.Integer(Left, Right);
end;

procedure TCompareTests.GlobalCompare;
var
  cmp : IComparer<Integer>;
  res : Integer;

begin
  cmp := TComparerTool<Integer>.FromFunc( GlobalCompareFunc );
  res := cmp.Compare(2,3);
  self.AssertEquals(-1, res);
end;

procedure TCompareTests.MemTest;

  // Manually debug and confirm destructor called on comparers
  procedure Test1;
  var
    cmp : IComparer<Integer>;
  begin
       // 1 globalcomparer destroy
      cmp := TComparerTool<Integer>.FromFunc(GlobalCompareFunc);
  end;

  procedure Test2;
  var
    cmp : IComparer<Integer>;
  begin
      // 1 andmanycomparer, 2 globalcomparers detroys
      cmp := TComparerTool<Integer>.Many([GlobalCompareFunc, GlobalCompareFunc]);
  end;

begin
   Test1;
   Test2;
end;

procedure TCompareTests.ManyTest_LE;
var
  cmp : IComparer<Integer>;
  count,res : integer;

  function NestedCompareFunc(constref Left, Right : Integer ) : Integer;
  begin
    Inc(count);
    Result := TCompare.Integer(Left, Right);
  end;

begin
  count := 0;
  cmp := TComparerTool<Integer>.Many([NestedCompareFunc, NestedCompareFunc]);
  res := cmp.Compare(1,2);
  AssertEquals(1, count);
  AssertEquals(-1, res);
end;

procedure TCompareTests.ManyTest_EQ;
var
  cmp : IComparer<Integer>;
  count,res : integer;

  function NestedCompareFunc(constref Left, Right : Integer ) : Integer;
  begin
    Inc(count);
    Result := TCompare.Integer(Left, Right);
  end;

begin
  count := 0;
  cmp := TComparerTool<Integer>.Many([NestedCompareFunc, NestedCompareFunc]);
  res := cmp.Compare(1,1);
  AssertEquals(2, count);
  AssertEquals(0, res);
end;

procedure TCompareTests.ManyTest_GE;
var
  cmp : IComparer<Integer>;
  count,res : integer;

  function NestedCompareFunc(constref Left, Right : Integer ) : Integer;
  begin
    Inc(count);
    Result := TCompare.Integer(Left, Right);
  end;

begin
  count := 0;
  cmp := TComparerTool<Integer>.Many([NestedCompareFunc, NestedCompareFunc]);
  res := cmp.Compare(2,1);
  AssertEquals(1, count);
  AssertEquals(1, res);
end;



procedure TCompareTests.SetUp;
begin

end;

procedure TCompareTests.TearDown;
begin

end;

initialization

  RegisterTest(TCompareTests);
end.

