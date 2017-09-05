unit PF.VisualGrid;

{$I pf.inc}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Controls, Grids, ComCtrls;

type
  TCustomVisualGrid = class(TCustomPanel)
  protected
    FSearchEdit: TCustomEdit;
    FTopPanel: TCustomPanel;
    FDrawGrid: TCustomDrawGrid;
    FTabs: TCustomTabControl;

    procedure ClickTest(Sender: TOBject);
  public
    constructor Create(Owner: TComponent); override;
  end;

  TVisualGrid = class(TCustomVisualGrid)
  public

  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Framework', [TVisualGrid]);
end;

{ TCustomVisualGrid }

procedure TCustomVisualGrid.ClickTest(Sender: TOBject);
begin
  TButton(Sender).Caption := Format('%dx%d', [FSearchEdit.Left,FSearchEdit.Top]);
end;

constructor TCustomVisualGrid.Create(Owner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle - [csAcceptsControls];

  FTopPanel := TCustomPanel.Create(Self);
  FTopPanel.ControlStyle := FTopPanel.ControlStyle - [csAcceptsControls];
  FTopPanel.Parent := Self;
  FTopPanel.Align := alTop;
  FTopPanel.Height := 100;

  FTabs := TCustomTabControl.Create(Self);
  FTabs.ControlStyle := FTabs.ControlStyle - [csAcceptsControls];
  FTabs.Parent := Self;
  FTabs.Align := alClient;

  FSearchEdit := TCustomEdit.Create(Self);
  FSearchEdit.Parent := FTopPanel;
  FSearchEdit.Top := 4;
  FSearchEdit.Left := 4;
  FSearchEdit.Anchors := [akTop, akRight];

  FDrawGrid := TCustomDrawGrid.Create(Self);
  FDrawGrid.Parent := FTabs;
  FDrawGrid.Align := alClient;



  with TButton.Create(Self) do
  begin
    Left := 0;
    Top := 0;
    Parent := Self;
    OnClick := ClickTest
  end;

end;

end.

