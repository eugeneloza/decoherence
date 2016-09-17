{Copyright (C) 2012-2016 Yevhen Loza

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.}
unit decomouse;

{$mode objfpc}{$H+}

interface

uses classes, fgl, sysUtils,
  CastleLog,
  castleFilesUtils, CastleKeysMouse,
  decoglobal, decoimages;

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

function GetFingerIndex(const Event: TInputPressRelease):integer;
begin
  if event.MouseButton=mbleft then
    result:=event.FingerIndex
  else if event.MouseButton=mbright then
    result:=100
  else if event.MouseButton=mbmiddle then
    result:=200;
end;

procedure doMouseRelease(const Event: TInputPressRelease);
var i,fingerindex:integer;
    found:boolean;
begin
 if TouchArray.Count>0 then begin
    fingerindex:=GetFingerIndex(Event);
    i:=0;
    found:=false;
    Repeat
      if touchArray[i].FingerIndex=fingerindex then found:=true else inc(i);
    until (i>TouchArray.Count-1) or found;
    WritelnLog('doMouseRelease','Caught mouse release finger='+inttostr(fingerindex)+'n='+inttostr(i));
    if found then
      TouchArray.Remove(touchArray[i])
    else
      WritelnLog('doMouseRelease','ERROR: Touch event not found!');
 end else
   WritelnLog('doMouseRelease','ERROR: Touch event list is empty!');

end;

{-----------------------------------------------------------------------------}

procedure doMousePress(const Event: TInputPressRelease);
var NewEventTouch:DTouch;
    fingerindex:integer;
begin
  fingerindex:=GetFingerIndex(Event);
  NewEventTouch:=DTouch.create(event.Position[0],event.Position[1],fingerindex);
  TouchArray.Add(NewEventTouch);
  WritelnLog('doMouseRelease','Caught mouse press finger='+inttostr(FingerIndex));
end;

end.

