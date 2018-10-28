unit UVariantToolTests;

{$mode delphi}
{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

implementation

uses Generics.Defaults, Generics.Collections, UCommon, UCommon.Collections, UMemory, LazLogger;



initialization
  RegisterTest(TVariantToolTests);
end.


