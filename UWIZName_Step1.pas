unit UWIZName_Step1;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, UWizard, UCommon;

type
  { TWIZName_Step1 }
  TWIZName_Step1 = class(TWizardForm<TGenericWizardBag>)
    FSomeButton: TBitBtn;
    FMustBeCheckedCheckBox: TCheckBox;
    FNameEdit: TEdit;
    FDimensionLabel: TLabel;
    Label1: TLabel;
    StaticText1: TStaticText;
    procedure FNameEditEditingDone(Sender: TObject);
    procedure UpdateDimensionLabel(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure Initialize; override;
    procedure OnPresent; override;
    procedure OnPrevious; override;
    procedure OnNext; override;
    function Validate(out message : AnsiString) : boolean; override;
  end;


implementation
{$R *.lfm}
{%region TWIZName_Step1}

procedure TWIZName_Step1.FNameEditEditingDone(Sender: TObject);
begin
  self.Bag['Name'].Destroy; // free existing value
  self.Bag['Name'] := TStringObject.Create(FNameEdit.Text);
end;



procedure TWIZName_Step1.UpdateDimensionLabel(Sender: TObject);
begin
  FDimensionLabel.Caption := Width.ToString + ' x ' + Height.ToString;
end;


procedure TWIZName_Step1.Initialize;
begin
  // form level initialise
end;

procedure TWIZName_Step1.OnPresent;
begin
  // Display value of property bag
  FNameEdit.Text :=  TStringObject(Bag['Name']).Value;
end;

procedure TWIZName_Step1.OnPrevious;
begin

end;

procedure TWIZName_Step1.OnNext;
begin

end;

function TWIZName_Step1.Validate(out message : AnsiString) : boolean;
begin
  if FMustBeCheckedCheckBox.Checked then
  begin
    Validate := true;
    message := '';
  end else begin
    Validate := false;
    message := 'Please check checkbox';
  end;
end;

{%endregion}

end.

