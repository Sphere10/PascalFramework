unit UCacheTests;

{$mode delphi}
{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TCacheTests= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ExpirationTest_Simple_1;
  end;

implementation

uses UCache;

const
  AStringValue : string = 'FIRST';

procedure ExpirationTest_Simple_1;
var
  cache : TActionCache<Integer, AnsiString>;
begin
  cache = TActionCache<int, string>.Create(
  (x) => AStringValue,
  reapStrategy: CacheReapPolicy.LeastUsed,
  expirationStrategy: ExpirationPolicy.SinceFetchedTime,
  expirationDuration: TimeSpan.FromMilliseconds(100)
  );
  Assert.AreEqual("first", cache[1]);
  val = "second";
  Assert.AreEqual("first", cache[1]);
  Assert.AreEqual("first", cache[1]);
  Thread.Sleep(111);
  Assert.AreEqual("second", cache[1]);
  Assert.AreEqual("second", cache[1]);
  Assert.AreEqual("second", cache[1]);
  }
end;



procedure TCacheTests.SetUp;
begin

end;

procedure TCacheTests.TearDown;
begin

end;

initialization

  RegisterTest(TCacheTests);
end.

