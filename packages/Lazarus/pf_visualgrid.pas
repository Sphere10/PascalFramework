{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PF_VisualGrid;

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
