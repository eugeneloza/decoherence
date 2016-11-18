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

{---------------------------------------------------------------------------}

{ Handles mouse behaviour }
unit decomouse;

{$INCLUDE compilerconfig.inc}

interface

uses classes, fgl, sysUtils,
  CastleLog,
  castleFilesUtils, CastleKeysMouse,
  decointerface, decogui,
  decoglobal;

type DTouch = class (TObject)
  FingerIndex:integer;
  x0,y0:integer;     //to handle sweeps, drags and cancels
  click_element: DAbstractInterfaceElement;
  constructor create(const xx,yy:single; const finger:integer);
  procedure update(Event: TInputMotion);
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
  x0 := round(xx);
  y0 := round(yy);
  fingerindex := finger;
end;

procedure DTouch.update(Event: TInputMotion);
begin
  x0 := round(event.Position[0]);
  y0 := round(event.Position[1]);
end;

{-----------------------------------------------------------------------------}

function GetFingerIndex(const Event: TInputPressRelease):integer;
begin
  if event.MouseButton=mbleft then
    result := event.FingerIndex
  else if event.MouseButton=mbright then
    result := 100
  else if event.MouseButton=mbmiddle then
    result := 200;
end;

procedure doMouseRelease(const Event: TInputPressRelease);
var i,fingerindex:integer;
    found:boolean;
begin
 if TouchArray.Count>0 then begin
    fingerindex := GetFingerIndex(Event);
    i := 0;
    found := false;
    Repeat
      if touchArray[i].FingerIndex=fingerindex then found := true else inc(i);
    until (i>TouchArray.Count-1) or found;
    WritelnLog('doMouseRelease','Caught mouse release finger='+inttostr(fingerindex)+' n='+inttostr(i));
    if found then begin
      if (touchArray[i].click_element<>nil) then begin
        if assigned(touchArray[i].click_element.OnMouseRelease) then
          touchArray[i].click_element.OnMouseRelease(touchArray[i].click_element,touchArray[i].x0,touchArray[i].y0);
        if assigned(touchArray[i].click_element.OnDrop) then
          touchArray[i].click_element.OnDrop(touchArray[i].click_element,touchArray[i].x0,touchArray[i].y0);
      end;
      TouchArray.Remove(touchArray[i])
    end else
      WritelnLog('doMouseRelease','ERROR: Touch event not found!');
 end else
   WritelnLog('doMouseRelease','ERROR: Touch event list is empty!');

end;

{-----------------------------------------------------------------------------}

procedure doMousePress(const Event: TInputPressRelease);
var NewEventTouch:DTouch;
    fingerindex:integer;
    tmpLink: DAbstractElement;
begin
  fingerindex := GetFingerIndex(Event);
  NewEventTouch := DTouch.create(event.Position[0],event.Position[1],fingerindex);

  //catch the element which has been pressed
  tmpLink := GUI.IfMouseOver(round(event.Position[0]),round(event.Position[1]),true);
  if (tmpLink is DAbstractInterfaceElement) then begin
    NewEventTouch.click_element := tmpLink as DAbstractInterfaceElement;
    if assigned(NewEventTouch.click_element.OnMousePress) then
      NewEventTouch.click_element.OnMousePress(tmpLink,round(event.Position[0]),round(event.Position[1]));
    if NewEventTouch.click_element.CanDrag then NewEventTouch.click_element.startDrag(round(event.Position[0]),round(event.Position[1]));
  end;

  TouchArray.Add(NewEventTouch);
  WritelnLog('doMousePress','Caught mouse press finger='+inttostr(FingerIndex));
end;

end.

