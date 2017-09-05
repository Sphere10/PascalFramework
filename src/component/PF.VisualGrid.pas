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
    FSearchLabel: TLabel;
    FSearchEdit: TEdit;
    FTopPanel: TPanel;
    FBottomPanel: TPanel;
    FBottomCenterPanel: TPanel;
    FBottomRightPanel: TPanel;

    FButtonFirst: TButton;
    FButtonLast: TButton;
    FButtonNext: TButton;
    FButtonPrevious: TButton;

    FCurrentPageEdit: TEdit;
    FAllPagesLabel: TLabel;


    FDrawGrid: TDrawGrid;
//    FTabs: {$IFDEF FPC}TCustomTabControl{$ELSE}TTabSet{$ENDIF};

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

  FTopPanel := TPanel.Create(Self);
  FTopPanel.ControlStyle := FTopPanel.ControlStyle - [csAcceptsControls];
  FTopPanel.Parent := Self;
  FTopPanel.Align := alTop;
  FTopPanel.Height := 50;

  FBottomPanel := TPanel.Create(Self);
  FBottomPanel.ControlStyle := FBottomPanel.ControlStyle - [csAcceptsControls];
  FBottomPanel.Parent := Self;
  FBottomPanel.Align := alBottom;
  FBottomPanel.BevelOuter := bvNone;
  FBottomPanel.Height := 40;

  FBottomRightPanel := TPanel.Create(Self);
  FBottomRightPanel.Parent := FBottomPanel;
  with FBottomRightPanel do
  begin
    Width := 217;
    Height := 40;
    Align := alRight;
    BevelOuter := bvNone;
    FCurrentPageEdit := TEdit.Create(Self);
    FCurrentPageEdit.Parent := FBottomRightPanel;
    with FCurrentPageEdit do
    begin
      Left := 61;
      Top := 10;
      Width := 56;
      Height := 21;
    end;
    FAllPagesLabel := TLabel.Create(Self);
    FAllPagesLabel.Parent := FBottomRightPanel;
    with FAllPagesLabel do
    begin
      Left := 118;
      Top := 13;
      Width := 36;
      Height := 13;
      Caption := '/';
    end;
    FButtonFirst := TButton.Create(Self);
    FButtonFirst.Parent := FBottomRightPanel;
    with FButtonFirst do
    begin
      Left := 8;
      Top := 8;
      Width := 25;
      Height := 25;
      Caption := #9198;
    end;
    FButtonPrevious := TButton.Create(Self);
    FButtonPrevious.Parent := FBottomRightPanel;
    with FButtonPrevious do
    begin
      Left := 32;
      Top := 8;
      Width := 25;
      Height := 25;
      Caption := #9204;
    end;
    FButtonNext := TButton.Create(Self);
    FButtonNext.Parent := FBottomRightPanel;
    with FButtonNext do
    begin
      Left := 160;
      Top := 8;
      Width := 25;
      Height := 25;
      Caption := #9205;
    end;
    FButtonLast := TButton.Create(Self);
    FButtonLast.Parent := FBottomRightPanel;
    with FButtonLast do
    begin
      Left := 184;
      Top := 8;
      Width := 25;
      Height := 25;
      Caption := #9197;
    end;
  end;

  FBottomCenterPanel := TPanel.Create(Self);
  FBottomCenterPanel.Parent := FBottomPanel;
  FBottomCenterPanel.Align := alClient;

//  {$IFDEF FPC}
//  FTabs := TCustomTabControl.Create(Self);
//  {$ELSE}
//  FTabs := TTabSet.Create(Self);
//  {$ENDIF}
//  FTabs.Parent := FBottomPanel;
//  FTabs.Align := alClient;


  FSearchLabel := TLabel.Create(Self);
  FSearchLabel.Parent := FTopPanel;

  FSearchEdit := TEdit.Create(Self);
  FSearchEdit.Parent := FTopPanel;
  FSearchEdit.Top := 4;
  FSearchEdit.Left := 4;
  FSearchEdit.Anchors := [akTop, akRight];

  FDrawGrid := TDrawGrid.Create(Self);
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

