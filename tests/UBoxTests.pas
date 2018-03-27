unit UBoxTests;

{$mode delphi}
{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  UCommon;

type
  TBoxTests = class(TTestCase)
    published
      procedure TestStringList;
      procedure TestStringListReleaseOnly;
  end;

implementation

uses Generics.Defaults, Generics.Collections, UCommon.Collections, UMemory, LazLogger;

type
  TTestRecord = record
    AInt : Integer;
    AString : String;
  end;


procedure TBoxTests.TestStringList;
var rec : TTestRecord; box : TBox<TTestRecord>; strList : TStringList;
begin
  strList := TStringList.Create;
  strList.OwnsObjects:=true;
  rec.AInt := 11;
  rec.AString := 'ABC';
  box := TBox<TTestRecord>.Create(rec);
  strList.AddObject('first', box);
  Self.AssertEquals(1, TBox<TTestRecord>.Instances);
  strList.Clear;
  Self.AssertEquals(0, TBox<TTestRecord>.Instances);
  strList.Free;
end;

procedure TBoxTests.TestStringListReleaseOnly;
var rec : TTestRecord; box : TBox<TTestRecord>;  ref : TBox<TTestRecord>; strList : TStringList;
begin
  strList := TStringList.Create;
  strList.OwnsObjects:=true;
  rec.AInt := 11;
  rec.AString := 'ABC';
  box := TBox<TTestRecord>.Create(rec);
  strList.AddObject('first', box);
  Self.AssertEquals(1, TBox<TTestRecord>.Instances);
  ref := TBox<TTestRecord>( strList.Objects[0] );
  strList.Clear;
  Self.AssertEquals(0, TBox<TTestRecord>.Instances);  // owned object collected anyway
  ref := nil;
  Self.AssertEquals(0, TBox<TTestRecord>.Instances);
  strList.Free;
end;

initialization
  RegisterTest(TBoxTests);
end.
