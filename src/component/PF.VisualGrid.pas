unit PF.VisualGrid;

{$I pf.inc}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Controls, Grids, ComCtrls
  {$IFNDEF FPC}
  ,Tabs
  {$ENDIF};

type
  TCustomVisualGrid = class(TWinControl)
  protected
    FSearchLabel: TCustomLabel;
    FSearchEdit: TCustomEdit;
    FTopPanel: TCustomPanel;
    FBottomPanel: TCustomPanel;
    FDrawGrid: TCustomDrawGrid;
    FTabs: {$IFDEF FPC}TCustomTabControl{$ELSE}TTabSet{$ENDIF};

    procedure ClickTest(Sender: TOBject);
  public
    constructor Create(Owner: TComponent); override;
  end;

  TVisualGrid = class(TCustomVisualGrid)
  published
    property Align;
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
  FTopPanel.Height := 50;

  FBottomPanel := TCustomPanel.Create(Self);
  FBottomPanel.ControlStyle := FBottomPanel.ControlStyle - [csAcceptsControls];
  FBottomPanel.Parent := Self;
  FBottomPanel.Align := alBottom;
  FBottomPanel.Height := 50;

  {$IFDEF FPC}
  FTabs := TCustomTabControl.Create(Self);
  {$ELSE}
  FTabs := TTabSet.Create(Self);
  {$ENDIF}
  FTabs.Parent := FBottomPanel;
  FTabs.Align := alClient;


  FSearchLabel := TCustomLabel.Create(Self);
  FSearchLabel.Parent := FTopPanel;

  FSearchEdit := TCustomEdit.Create(Self);
  FSearchEdit.Parent := FTopPanel;
  FSearchEdit.Top := 4;
  FSearchEdit.Left := 4;
  FSearchEdit.Anchors := [akTop, akRight];

  FDrawGrid := TCustomDrawGrid.Create(Self);
  FDrawGrid.Parent := Self;
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

