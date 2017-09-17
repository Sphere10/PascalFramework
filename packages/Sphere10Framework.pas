{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Sphere10Framework;

interface

uses
  UVisualGrid, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UVisualGrid', @UVisualGrid.Register);
end;

initialization
  RegisterPackage('Sphere10Framework', @Register);
end.
