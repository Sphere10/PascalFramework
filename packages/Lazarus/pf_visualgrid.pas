{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PF_VisualGrid;

{$warn 5023 off : no warning about unused units}
interface

uses
  PF.VisualGrid, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PF.VisualGrid', @PF.VisualGrid.Register);
end;

initialization
  RegisterPackage('PF_VisualGrid', @Register);
end.
