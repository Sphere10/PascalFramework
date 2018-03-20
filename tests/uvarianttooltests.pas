unit UVariantToolTests;

{$mode delphi}
{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TVariantToolTests = class(TTestCase)
    published
      procedure NumericEQ;
      procedure NumericLT;

      procedure TVariantCompare_1;
      procedure TVariantCompare_2;
      procedure TVariantCompare_3;
      procedure TVariantCompare_4;
  end;

implementation

uses Generics.Defaults, Generics.Collections, UCommon, UCommon.Collections, UMemory, LazLogger;


procedure TVariantToolTests.NumericEQ;
var
  lbyte : byte;
  luint16 : uint16;
  lint : integer;
  lqword : qword;
  left, right : variant;
  cmp : boolean;
begin
  lbyte := 111;
  luint16 := 111;
  lint := 111;
  lqword := 111;

  left := Variant(lbyte);
  right := Variant(luint16);
  cmp := TVariantTool.NumericEq(left, right);
  self.AssertEquals(true, cmp);

  left := Variant(lbyte);
  right := Variant(lint);
  cmp := TVariantTool.NumericEq(left, right);
  self.AssertEquals(true, cmp);

  left := Variant(lbyte);
  right := Variant(lqword);
  cmp := TVariantTool.NumericEq(left, right);
  self.AssertEquals(true, cmp);

  left := Variant(lint);
  right := Variant(lqword);
  cmp := TVariantTool.NumericEq(left, right);
  self.AssertEquals(true, cmp);

end;

procedure TVariantToolTests.NumericLT;
var
  lbyte : byte;
  lint : integer;
  lqword : qword;
  left, right : variant;
  cmp : boolean;
begin
  lbyte := 111;
  lint := 11111;
  lqword := 1111111111111111111;

  left := Variant(lbyte);
  right := Variant(lint);
  cmp := TVariantTool.NumericLT(left, right);
  self.AssertEquals(true, cmp);

  left := Variant(lbyte);
  right := Variant(lqword);
  cmp := TVariantTool.NumericLT(left, right);
  self.AssertEquals(true, cmp);

  left := Variant(lint);
  right := Variant(lqword);
  cmp := TVariantTool.NumericLT(left, right);
  self.AssertEquals(true, cmp);

end;


procedure TVariantToolTests.TVariantCompare_1;
var
  lval : byte;
  rval : integer;
  left, right : variant;
  cmp : integer;
begin
  lval := 111;
  rval := 111;
  left := lval;
  right := rval;
  cmp := TCompare.Variant(@left, @right);
  self.AssertEquals(0, cmp);
  cmp := TCompare.Variant(@Right, @Left);
  self.AssertEquals(0, cmp);
end;

procedure TVariantToolTests.TVariantCompare_2;
var
  lval : integer;
  rval : qword;
  left, right : variant;
  cmp : integer;
begin
  lval := 111;
  rval := 111;
  left := lval;
  right := rval;
  cmp := TCompare.Variant(@left, @right);
  self.AssertEquals(0, cmp);
  cmp := TCompare.Variant(@Right, @Left);
  self.AssertEquals(0, cmp);
end;

procedure TVariantToolTests.TVariantCompare_3;
var
  lval : real;
  rval : qword;
  left, right : variant;
  cmp : integer;
begin
  lval := 111.0;
  rval := 111;
  left := lval;
  right := rval;
  cmp := TCompare.Variant(@left, @right);
  self.AssertEquals(0, cmp);
  cmp := TCompare.Variant(@Right, @Left);
  self.AssertEquals(0, cmp);
end;

procedure TVariantToolTests.TVariantCompare_4;
var
  lval : uint16;
  rval : qword;
  left, right : variant;
  cmp : integer;
begin
  lval := 10001;
  rval := 10001;
  left := lval;
  right := rval;
  cmp := TCompare.Variant(@left, @right);
  self.AssertEquals(0, cmp);
  cmp := TCompare.Variant(@Right, @Left);
  self.AssertEquals(0, cmp);
end;

initialization

  RegisterTest(TVariantToolTests);
end.


