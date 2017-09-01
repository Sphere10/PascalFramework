unit UFRMMain;
{$R *.lfm}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, UWizard, UWIZName_Step1, UWIZName_Step2, UFRMWorkArea, UCommon;


type

  { TFRMMain }
  TFRMMain = class(TForm)
    _wizardTestButton: TButton;
    FWorkAreaButton: TButton;
    procedure FWorkAreaButtonClick(Sender: TObject);
    procedure _wizardTestButtonClick(Sender: TObject);
  private
    { private declarations }
    function CancelCallback( screenIndex:integer; constref bag : TGenericWizardBag; out message : AnsiString ) : boolean;
    function FinishCallback(constref bag  : TGenericWizardBag; out message : AnsiString) : boolean;
  public
    { public declarations }
  end;

var
  MainForm: TFRMMain;

implementation

{ TFRMMain }

procedure TFRMMain._wizardTestButtonClick(Sender: TObject);

type
  TTestWizard = TActionWizard<TGenericWizardBag>;

var
  propertyBag : TGenericWizardBag;
begin
  propertyBag := TGenericWizardBag.Create;
  try
    // Pre-set property bag values when using TGenericWizardBag
    propertyBag['Name'] := TStringObject.Create('');
    TTestWizard.Show(
      self,
      'My Test Action Wizard',
      'Finish Test',
      propertyBag,
      [ TWIZName_Step1.Create(nil), TWIZName_Step2.Create(nil)],
      CancelCallback,
      FinishCallback
    );
  finally
    propertyBag['Name'].Destroy;
    propertyBag.Destroy;
  end;
end;

procedure TFRMMain.FWorkAreaButtonClick(Sender: TObject);
var
  frm : TFRMWorkArea;
begin
  try
    frm := TFRMWorkArea.Create(self);
    frm.ShowModal;
  finally
    frm.Destroy;
  end;
end;

function TFRMMain.CancelCallback( screenIndex:integer; constref bag : TGenericWizardBag; out message : AnsiString ) : boolean;
begin
  if screenIndex <> 0 then begin
    Result := false;
    message := 'Can only cancel on first screen';
  end else begin
    Result := true;
  end;
end;

function TFRMMain.FinishCallback(constref bag  : TGenericWizardBag; out message : AnsiString) : boolean;
begin
  if PChar(bag['Name']) = 'fail' then begin
    message := 'Name cannot be ''fail''';
    Result := false;
  end else begin
    ShowMessage('Finish: ' + TStringObject(bag['Name']).Value);
    Result := true;
  end;
end;

end.

