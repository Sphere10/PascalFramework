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
    FTopPanelRight: TPanel;
    FBottomPanel: TPanel;
    FBottomCenterPanel: TPanel;
    FBottomRightPanel: TPanel;

    FButtonFirst: TButton;
    FButtonLast: TButton;
    FButtonNext: TButton;
    FButtonPrevious: TButton;

    FCurrentPageEdit: TEdit;
    FAllPagesLabel: TLabel;

    FPageSizeEdit: TEdit;
    FPageSizeLabel: TLabel;
    FAllRecordsCountLabel: TLabel;

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
  with FTopPanel do
  begin
    Align := alTop;
    BevelOuter := bvNone;
    Height := 40;

    FTopPanelRight := TPanel.Create(Self);
    FTopPanelRight.ControlStyle := FTopPanelRight.ControlStyle - [csAcceptsControls];
    FTopPanelRight.Parent := FTopPanel;
    with FTopPanelRight do
    begin
      BevelOuter := bvNone;
      Align := alRight;
      Height := 40;
      Width := 177;

      FSearchLabel := TLabel.Create(Self);
      FSearchLabel.Parent := FTopPanelRight;
      with FSearchLabel do
      begin
        Left := 7;
        Top := 13;
        Width := 37;
        Height := 13;
        Caption := 'Search:';
      end;

      FSearchEdit := TEdit.Create(Self);
      FSearchEdit.Parent := FTopPanelRight;
      with FSearchEdit do
      begin
        Left := 50;
        Top := 10;
        Width := 121;
        Height := 21;
      end;
    end;
  end;

  FBottomPanel := TPanel.Create(Self);
  FBottomPanel.ControlStyle := FBottomPanel.ControlStyle - [csAcceptsControls];
  FBottomPanel.Parent := Self;
  with FBottomPanel do
  begin
    Align := alBottom;
    BevelOuter := bvNone;
    Height := 40;

    FBottomRightPanel := TPanel.Create(Self);
    FBottomRightPanel.ControlStyle := FBottomRightPanel.ControlStyle - [csAcceptsControls];
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
    FBottomCenterPanel.ControlStyle := FBottomCenterPanel.ControlStyle - [csAcceptsControls];
    FBottomCenterPanel.Parent := FBottomPanel;
    with FBottomCenterPanel do
    begin
      Align := alClient;
      BevelOuter := bvNone;
      FAllRecordsCountLabel := TLabel.Create(Self);
      FAllRecordsCountLabel.Parent := FBottomCenterPanel;
      with FAllRecordsCountLabel do
      begin
        Left := 7;
        Top := 13;
        Width := 31;
        Height := 13;
        Caption := 'Total: ';
      end;
      FPageSizeLabel := TLabel.Create(Self);
      FPageSizeLabel.Parent := FBottomCenterPanel;
      with FPageSizeLabel do
      begin
        Left := 126;
        Top := 13;
        Width := 31;
        Height := 13;
        Caption := 'Page size:'
      end;
      FPageSizeEdit := TEdit.Create(Self);
      FPageSizeEdit.Parent := FBottomCenterPanel;
      with FPageSizeEdit do
      begin
        Left := 181;
        Top := 10;
        Width := 52;
        Height := 21;
      end;
    end;
  end;

//  {$IFDEF FPC}
//  FTabs := TCustomTabControl.Create(Self);
//  {$ELSE}
//  FTabs := TTabSet.Create(Self);
//  {$ENDIF}
//  FTabs.Parent := FBottomPanel;
//  FTabs.Align := alClient;

  FDrawGrid := TDrawGrid.Create(Self);
  FDrawGrid.Parent := Self;
  FDrawGrid.Align := alClient;

  with TButton.Create(Self) do
  begin
    Left := 0;
    Top := 0;
    Parent := Self;
    OnClick := ClickTest;
    Caption := 'Test';
  end;
end;

end.

