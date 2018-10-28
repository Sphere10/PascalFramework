unit UResultTests;

{$mode delphi}
{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

implementation

uses
  variants, Generics.Defaults, Generics.Collections, UCommon, UMemory,
  UCommon.Collections;



initialization
  RegisterTest(TResultTests);
end.


