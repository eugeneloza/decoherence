{Copyright (C) 2012-2017 Yevhen Loza

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
unit DecoMouse;

{$INCLUDE compilerconfig.inc}

interface

uses Classes, fgl, SysUtils,
  CastleLog,
  CastleFilesUtils, CastleKeysMouse,
  DecoInterface, DecoGui;

type DTouch = class (TObject)
  FingerIndex: cardinal;
  x0,y0: integer;     //to handle sweeps, drags and cancels
  ClickElement: DSingleInterfaceElement;
  constructor Create(const xx,yy: single; const Finger: integer);
  procedure Update(const Event: TInputMotion);
end;

type DTouchList = specialize TFPGObjectList<DTouch>;

{-------------------------------- vars --------------------------------------}

var TouchArray: DTouchList;

{------------------------------- procs --------------------------------------}

procedure doMousePress(const Event: TInputPressRelease);
procedure doMouseRelease(const Event: TInputPressRelease);
function doMouseLook(const Event: TInputMotion): boolean;
function doMouseDrag(const Event: TInputMotion): boolean;
procedure CenterMouseCursor;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses CastleVectors,
  DecoNavigation,
  DecoGlobal, DecoPlayerCharacter;

constructor DTouch.Create(const xx,yy: single; const Finger: integer);
begin
  x0 := Round(xx);
  y0 := Round(yy);
  FingerIndex := Finger;
end;

procedure DTouch.Update(const Event: TInputMotion);
begin
  x0 := Round(Event.Position[0]);
  y0 := Round(Event.Position[1]);
end;

{-----------------------------------------------------------------------------}

function GetFingerIndex(const Event: TInputPressRelease): integer;
begin
  if Event.MouseButton = mbLeft then
    Result := Event.FingerIndex
  else if Event.MouseButton = mbRight then
    Result := 100
  else if Event.MouseButton = mbMiddle then
    Result := 200
  else raise Exception.Create('Unknown event.MouseButton in decomouse.GetFingerIndex!');
end;

procedure doMouseRelease(const Event: TInputPressRelease);
var i,FingerIndex: integer;
    Found: boolean;
begin
 if TouchArray.Count>0 then begin
    fingerindex := GetFingerIndex(Event);
    i := 0;
    Found := false;
    Repeat
      if TouchArray[i].FingerIndex = FingerIndex then Found := true else inc(i);
    until (i>TouchArray.Count-1) or Found;
    WritelnLog('doMouseRelease','Caught mouse release finger='+IntToStr(FingerIndex)+' n='+IntToStr(i));
    if Found then begin
      if (TouchArray[i].ClickElement <> nil) then begin
        if Assigned(touchArray[i].ClickElement.OnMouseRelease) then
          TouchArray[i].ClickElement.OnMouseRelease(TouchArray[i].ClickElement,TouchArray[i].x0,TouchArray[i].y0);
        if Assigned(TouchArray[i].ClickElement.OnDrop) then
          TouchArray[i].ClickElement.OnDrop(TouchArray[i].ClickElement,TouchArray[i].x0,TouchArray[i].y0);
      end;
      TouchArray.Remove(TouchArray[i]);
    end else
      WriteLnLog('doMouseRelease','ERROR: Touch event not found!');
 end else
   WriteLnLog('doMouseRelease','ERROR: Touch event list is empty!');

end;

{-----------------------------------------------------------------------------}

procedure doMousePress(const Event: TInputPressRelease);
var NewEventTouch: DTouch;
    FingerIndex: integer;
    tmpLink: DAbstractElement;
begin
  FingerIndex := GetFingerIndex(Event);
  NewEventTouch := DTouch.Create(Event.Position[0],Event.Position[1],FingerIndex);

  //catch the element which has been pressed
  tmpLink := GUI.IfMouseOver(Round(Event.Position[0]),Round(Event.Position[1]),true,true);
  if (tmpLink is DSingleInterfaceElement) then begin
    NewEventTouch.ClickElement := DSingleInterfaceElement(tmpLink);
    if Assigned(NewEventTouch.ClickElement.OnMousePress) then
      NewEventTouch.ClickElement.OnMousePress(tmpLink,Round(Event.Position[0]),Round(Event.Position[1]));
    if NewEventTouch.ClickElement.CanDrag then NewEventTouch.ClickElement.StartDrag(Round(Event.Position[0]),Round(Event.Position[1]));
  end;

  TouchArray.Add(NewEventTouch);
  WriteLnLog('doMousePress','Caught mouse press finger='+IntToStr(FingerIndex));
end;

{----------------------------------------------------------------------------}

var WindowCenter: TVector2;   {pull it in GUI at rescale}

var CameraWarning: boolean = true;
function doMouseLook(const Event: TInputMotion): boolean;
begin
  {if FMouseLook then
    Cursor := mcForceNone else
    Cursor := mcDefault;}
  if Camera = nil then begin
    if CameraWarning then begin
      WriteLnLog('DecoMouse>doMouseLook','Warning: Camera is not initialized for MouseLook');
      CameraWarning := false;
    end;
    Exit;
  end;
  Camera.Cursor := mcForceNone; {do it only once}
  WindowCenter := Vector2(Window.Width div 2, Window.Height div 2);
  if not TVector2.PerfectlyEquals(Event.Position,WindowCenter) then begin
    CurrentParty.InputMouse(Event.Position - WindowCenter);
    doMouseLook := false;
    Window.MousePosition := WindowCenter;
  end else
    doMouseLook := true; {prevent onMotion call-back}
end;

procedure CenterMouseCursor;
begin
  WindowCenter := Vector2(Window.Width div 2, Window.Height div 2);
  Window.MousePosition := WindowCenter;
end;

{----------------------------------------------------------------------------}

function doMouseDrag(const Event: TInputMotion): boolean;
var i: integer;
begin
  {check for drag-n-drops}
  Result := false;
  {if Event.EventType = itMouseButton then }begin
    if TouchArray.Count > 0 then begin
     i:=0;
     repeat
       if TouchArray[i].FingerIndex = Event.FingerIndex then begin
         TouchArray[i].Update(Event);
         if (TouchArray[i].ClickElement<>nil) and (TouchArray[i].ClickElement.CanDrag) then begin
           TouchArray[i].ClickElement.Drag(Round(Event.Position[0]),Round(Event.Position[1]));
           Result := true;
         end;
         Break;
       end;
       inc(i);
     until (i >= TouchArray.Count);
    end;

  end;
end;


Initialization
  TouchArray := DTouchList.Create;

Finalization
  FreeAndNil(TouchArray);

end.

