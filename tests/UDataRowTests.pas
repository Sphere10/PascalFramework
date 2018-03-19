unit UDataRowTests;

{$mode delphi}
{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  UCommon;

type

  TDummyObject = class(TObject)
    public
      Instances: Integer; static;
      Val : AnsiString;
      class constructor Create;
      constructor Create; overload;
      destructor Destroy; override;
  end;

  TDummyRecord = record
    public
      FDummy : TAuto<TDummyObject>;
  end;

  TAutoTest = class(TTestCase)
    published
      procedure TestNestedScope;
      procedure TestRecordScope;
  end;


implementation

uses Generics.Defaults, Generics.Collections, UCommon.Collections, UAutoScope, LazLogger;


class constructor TDummyObject.Create;
begin
  Instances := 0;
end;

constructor TDummyObject.Create;
begin
  Inherited;
  Inc(Instances);
end;

destructor TDummyObject.Destroy;
begin
  Inherited;
  Dec(Instances);
end;

procedure TAutoTest.TestNestedScope;
   procedure RunTest;
   var auto : TAuto<TDummyObject>;
   begin
     auto.Item := TDummyObject.Create;
     Self.AssertEquals('Premature collection', 1, TDummyObject.Instances);
     auto.Item.Val:= 'XXX';
     Self.AssertEquals('XXX', auto.Item.Val);
   end;
begin
  RunTest;
  Self.AssertEquals('Memory leak', 0, TDummyObject.Instances);
end;


procedure TAutoTest.TestRecordScope;
   procedure RunTest;
   var autoRec : TDummyRecord; dummy : TDummyObject;

   begin
     autoRec.FDummy.Item := TDummyObject.Create;
     Self.AssertEquals('Premature collection', 1, TDummyObject.Instances);
     dummy := autoRec.FDummy.Item;
     dummy.Val:= 'XXX';
     Self.AssertEquals('XXX', autoRec.FDummy.Item.Val);
   end;

begin
  RunTest;
  Self.AssertEquals('Memory leak', 0, TDummyObject.Instances);
end;

initialization
  RegisterTest(TAutoTest);
end.


