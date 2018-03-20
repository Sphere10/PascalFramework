unit UAutoPtrTests;

{$mode delphi}
{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  UCommon;

type
  TAutoPtrTest = class(TTestCase)
    published
      procedure TestReAssign;
      procedure TestNestedScope;
      procedure TestRecordScope;
  end;

implementation

uses Generics.Defaults, Generics.Collections, UCommon.Collections, UMemory, LazLogger;

type
  TTestObject = class(TObject)
    public
      Instances: Integer; static;
      Val : AnsiString;
      class constructor Create;
      constructor Create; overload;
      destructor Destroy; override;
  end;

  TTestRecord = record
    public
      FDummy : TAutoPtr<TTestObject>;
  end;


procedure TAutoPtrTest.TestReAssign;
var auto : TAutoPtr<TTestObject>;
begin
  auto.Value := TTestObject.Create;
  Self.AssertEquals(1, TTestObject.Instances);
  auto.Value := TTestObject.Create;
  Self.AssertEquals(1, TTestObject.Instances);
end;

procedure TAutoPtrTest.TestNestedScope;
   procedure RunTest;
   var auto : TAutoPtr<TTestObject>;
   begin
     auto.Value := TTestObject.Create;
     Self.AssertEquals('Premature collection', 1, TTestObject.Instances);
     auto.Value.Val:= 'XXX';
     Self.AssertEquals('XXX', auto.Value.Val);
   end;
begin
  RunTest;
  Self.AssertEquals('Memory leak', 0, TTestObject.Instances);
end;

procedure TAutoPtrTest.TestRecordScope;
   procedure RunTest;
   var autoRec : TTestRecord; dummy : TTestObject;

   begin
     autoRec.FDummy.Value := TTestObject.Create;
     Self.AssertEquals('Premature collection', 1, TTestObject.Instances);
     dummy := autoRec.FDummy.Value;
     dummy.Val:= 'XXX';
     Self.AssertEquals('XXX', autoRec.FDummy.Value.Val);
   end;

begin
  RunTest;
  Self.AssertEquals('Memory leak', 0, TTestObject.Instances);
end;

class constructor TTestObject.Create;
begin
  Instances := 0;
end;

constructor TTestObject.Create;
begin
  Inherited;
  Inc(Instances);
end;

destructor TTestObject.Destroy;
begin
  Inherited;
  Dec(Instances);
end;

initialization
  RegisterTest(TAutoPtrTest);
end.


