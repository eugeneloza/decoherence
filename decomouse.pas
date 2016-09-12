unit decomouse;

{$mode objfpc}{$H+}

interface

uses classes, fgl,
  CastleLog,
  castleFilesUtils, CastleKeysMouse,
  global_var, decoimages;

type DTouch = class (TObject)
  FingerIndex:integer;
  x0,y0:integer;     //to handle sweeps, drags and cancels
  constructor create(const xx,yy:single; const finger:integer);
end;

type DTouchList = specialize TFPGObjectList<DTouch>;

{-------------------------------- vars --------------------------------------}

var TouchArray:DTouchList;

{------------------------------- procs --------------------------------------}

procedure doMousePress(const Event: TInputPressRelease);

{============================================================================}

implementation

constructor DTouch.create(const xx,yy:single; const finger:integer);
begin
  x0:=round(xx);
  y0:=round(yy);
  fingerindex:=finger;
end;

procedure doMousePress(const Event: TInputPressRelease);
var NewEventTouch:DTouch;
    i:integer;
begin
  NewEventTouch:=DTouch.create(event.Position[0],event.Position[1],event.FingerIndex);
  TouchArray.Add(NewEventTouch);
end;

end.

