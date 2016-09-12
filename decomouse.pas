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
procedure doMouseRelease(const Event: TInputPressRelease);

{============================================================================}

implementation

constructor DTouch.create(const xx,yy:single; const finger:integer);
begin
  x0:=round(xx);
  y0:=round(yy);
  fingerindex:=finger;
end;

{-----------------------------------------------------------------------------}

procedure doMouseRelease(const Event: TInputPressRelease);
var i,fingerindex:integer;
    found:boolean;
begin
  if event.MouseButton=mbleft then
    fingerindex:=event.FingerIndex
  else if event.MouseButton=mbright then
    fingerindex:=100
  else if event.MouseButton=mbmiddle then
    fingerindex:=200;
  i:=0;
  found:=false;
  Repeat
    if touchArray[i].FingerIndex=fingerindex then found:=true else inc(i);
  until (i>TouchArray.Count-1) or found;
  if found then
    TouchArray.Remove(touchArray[i])
  else
    WritelnLog('doMouseRelease','Touch event not found!');
end;

{-----------------------------------------------------------------------------}

procedure doMousePress(const Event: TInputPressRelease);
var NewEventTouch:DTouch;
begin
  if event.MouseButton=mbleft then
    NewEventTouch:=DTouch.create(event.Position[0],event.Position[1],event.FingerIndex)
  else if event.MouseButton=mbright then
    NewEventTouch:=DTouch.create(event.Position[0],event.Position[1],100)
  else if event.MouseButton=mbmiddle then
    NewEventTouch:=DTouch.create(event.Position[0],event.Position[1],200);
  TouchArray.Add(NewEventTouch);
end;

end.

