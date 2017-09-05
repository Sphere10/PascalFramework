unit UFRMNotifyManyTest;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, UCommon;

type

  { TFRMNotifyManyTest }

  TFRMNotifyManyTest = class(TForm)
    Button1: TButton;
    cbListener1: TCheckBox;
    cbListener2: TCheckBox;
    cbListener3: TCheckBox;
    txtLog: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure cbListener1Change(Sender: TObject);
    procedure cbListener2Change(Sender: TObject);
    procedure cbListener3Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
    FMouseMoveMulticastEvent : TNotifyManyEvent;
    procedure Listener1Handler(Sender : TObject);
    procedure Listener2Handler(Sender : TObject);
    procedure Listener3Handler(Sender : TObject);
  public
    { public declarations }
  end;

var
  FRMNotifyManyTest: TFRMNotifyManyTest;

implementation

{$R *.lfm}

{ TFRMNotifyManyTest }

procedure TFRMNotifyManyTest.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  // Invoke the multi-cast event, no need to initialise it!
  FMouseMoveMulticastEvent.Invoke(Sender);
end;

procedure TFRMNotifyManyTest.Listener1Handler(Sender : TObject);
begin
  txtLog.Lines.Add(TimeStamp + 'Listener 1 called');
end;

procedure TFRMNotifyManyTest.Listener2Handler(Sender : TObject);
begin
   txtLog.Lines.Add(TimeStamp + 'Listener 2 called');
end;

procedure TFRMNotifyManyTest.Listener3Handler(Sender : TObject);
begin
   txtLog.Lines.Add(TimeStamp + 'Listener 3 called');
end;


procedure TFRMNotifyManyTest.cbListener1Change(Sender: TObject);
begin
   if cbListener1.Checked then
     FMouseMoveMulticastEvent.Add(Listener1Handler)
   else
     FMouseMoveMulticastEvent.Remove(Listener1Handler);
end;

procedure TFRMNotifyManyTest.cbListener2Change(Sender: TObject);
begin
   if cbListener2.Checked then
     FMouseMoveMulticastEvent.Add(Listener2Handler)
   else
     FMouseMoveMulticastEvent.Remove(Listener2Handler);
end;

procedure TFRMNotifyManyTest.cbListener3Change(Sender: TObject);
begin
   if cbListener3.Checked then
     FMouseMoveMulticastEvent.Add(Listener3Handler)
   else
     FMouseMoveMulticastEvent.Remove(Listener3Handler);
end;

procedure TFRMNotifyManyTest.FormCreate(Sender: TObject);
begin
   if cbListener3.Checked then
     FMouseMoveMulticastEvent.Add(Listener1Handler)
   else
     FMouseMoveMulticastEvent.Remove(Listener1Handler);
end;

procedure TFRMNotifyManyTest.Button1Click(Sender: TObject);
begin
  txtLog.Lines.Clear;
end;

end.

