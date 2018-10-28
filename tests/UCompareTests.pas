unit UCompareTests;

{$mode delphi}
{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type


implementation

uses Generics.Defaults, Generics.Collections, UCommon, UCommon.Collections;




procedure TCompareTests.SetUp;
begin

end;

procedure TCompareTests.TearDown;
begin

end;

initialization

  RegisterTest(TCompareTests);
end.

