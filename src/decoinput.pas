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
unit DecoInput;

{$INCLUDE compilerconfig.inc}

interface

uses Classes, Generics.Collections, SysUtils,
  CastleFilesUtils, CastleKeysMouse,
  DecoInterfaceCore, DecoGui,
  DecoGlobal;

type
  DTouch = class(DObject)
    FingerIndex: cardinal;
    x0, y0: integer;     //to handle sweeps, drags and cancels
    ClickElement: DSingleInterfaceElement;
    constructor Create(const xx, yy: single; const Finger: integer);
    procedure Update(const Event: TInputMotion);
  end;

type
  DTouchList = specialize TObjectList<DTouch>;

{-------------------------------- vars --------------------------------------}

var
  TouchArray: DTouchList;

{------------------------------- procs --------------------------------------}

procedure doMouseMotion(const Event: TInputMotion);
procedure doKeyboardPress(const aKey: TKey);
procedure doKeyboardRelease(const aKey: TKey);

procedure doMousePress(const Event: TInputPressRelease);
procedure doMouseRelease(const Event: TInputPressRelease);
function doMouseLook(const Event: TInputMotion): boolean;
function doMouseDrag(const Event: TInputMotion): boolean;
procedure CenterMouseCursor;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses CastleVectors,
  DecoNavigation, DecoPlayerCharacter,
  DecoGameMode, DecoLog, Profiler;

var
  RecordKeys: boolean = False;
  RecordedKeys: string;

procedure KeyRecorder(const aKey: TKey);
const
  test1 = 'DIIQI';
const
  test2 = 'DIKFA';

  function AddKey: boolean;
  begin
    Result := True;
    case aKey of
      k_A: RecordedKeys += 'A';
      k_F: RecordedKeys += 'F';
      k_K: RecordedKeys += 'K';
      k_Q: RecordedKeys += 'Q';
      {messing with letter a bit :)}
      k_D: RecordedKeys += 'I';
      k_I: RecordedKeys += 'D';
      else
        Result := False;
    end;
    //dLog(LogVerbose,nil,_CurrentRoutine,RecordedKeys);
  end;

  function TestRecord: boolean;

    function ifCorresponds(a: string): boolean;
    begin
      Result := (RecordedKeys = copy(a, 1, length(RecordedKeys))) and
        (Length(RecordedKeys) <= Length(a));
    end;

  begin
    if ifCorresponds(test1) or ifCorresponds(test2) then
      Result := True
    else
      Result := False;
  end;

begin
  if CurrentGameMode = gmTravel then
  begin
    if RecordKeys then
    begin
      if AddKey then
      begin
        if TestRecord then
        begin
          if (RecordedKeys = test1) or (RecordedKeys = test2) then
          begin
            Log(LogVerbose, _CurrentRoutine, 'No! This is a different game!');
            RecordKeys := False;
          end;
        end
        else
          RecordKeys := False;
      end
      else
        RecordKeys := False;
    end
    else
    if (aKey = k_I) then
    begin
      RecordedKeys := '';
      RecordKeys := True;
      AddKey;
    end;

  end;
end;

{-----------------------------------------------------------------------------}

procedure doKeyboardRelease(const aKey: TKey);
begin
  StartProfiler;

  case aKey of
    k_W: Player.InputRelease(mdForward);
    k_S: Player.InputRelease(mdBack);
    k_A: Player.InputRelease(mdLeft);
    k_D: Player.InputRelease(mdRight);
  end;

  StopProfiler;
end;

{-----------------------------------------------------------------------------}

procedure doKeyboardPress(const aKey: TKey);
begin
  StartProfiler;

  case aKey of
    k_W: Player.InputMove(mdForward);
    k_S: Player.InputMove(mdBack);
    k_A: Player.InputMove(mdLeft);
    k_D: Player.InputMove(mdRight);
  end;
  KeyRecorder(aKey);

  StopProfiler;
end;

{-----------------------------------------------------------------------------}

procedure doMouseMotion(const Event: TInputMotion);
var
  tmpLink: DAbstractElement;
  Dragging: boolean;
begin
  StartProfiler;

  if doMouseLook(Event) then
    Exit;

  Dragging := doMouseDrag(Event);

  {mouse over / if no drag-n-drop}
  //this is not needed at the moment, we'll turn here a bit later when implementing drag-n-drop
  //no mouseover is detected if no ifmouseover is run, so should still be here
  if not Dragging then
  begin
    tmpLink := GUI.IfMouseOver(Round(Event.Position[0]), Round(
      Event.Position[1]), True, True);
    if tmpLink <> nil then
      Log(logVerbose, _CurrentRoutine, 'Motion caught ' + tmpLink.ClassName);
  end;

  StopProfiler;
end;

{================================= TOUCH ====================================}

constructor DTouch.Create(const xx, yy: single; const Finger: integer);
begin
  StartProfiler;

  x0 := Round(xx);
  y0 := Round(yy);
  FingerIndex := Finger;

  StopProfiler;
end;

{-----------------------------------------------------------------------------}

procedure DTouch.Update(const Event: TInputMotion);
begin
  StartProfiler;

  x0 := Round(Event.Position[0]);
  y0 := Round(Event.Position[1]);

  StopProfiler;
end;

{-----------------------------------------------------------------------------}

function GetFingerIndex(const Event: TInputPressRelease): integer;
begin
  StartProfiler;

  if Event.MouseButton = mbLeft then
    Result := Event.FingerIndex
  else if Event.MouseButton = mbRight then
    Result := 100
  else if Event.MouseButton = mbMiddle then
    Result := 200
  else
    raise Exception.Create('Unknown event.MouseButton in decomouse.GetFingerIndex!');

  StopProfiler;
end;

{-----------------------------------------------------------------------------}

var
  { used to detect if mouse is in dragg-look mode }
  DragMouseLook: boolean = False;

procedure doMouseRelease(const Event: TInputPressRelease);
var
  i, FingerIndex: integer;
  Found: boolean;
begin
  StartProfiler;

  if TouchArray.Count > 0 then
  begin
    fingerindex := GetFingerIndex(Event);
    i := 0;
    Found := False;
    repeat
      if TouchArray[i].FingerIndex = FingerIndex then
        Found := True
      else
        Inc(i);
    until (i > TouchArray.Count - 1) or Found;

    //stop dragging
    if i = 0 then
      DragMouseLook := False;

    Log(LogMouseInfo, _CurrentRoutine, 'Caught mouse release finger=' +
      IntToStr(FingerIndex) + ' n=' + IntToStr(i));
    if Found then
    begin
      if (TouchArray[i].ClickElement <> nil) then
      begin
        if Assigned(touchArray[i].ClickElement.OnMouseRelease) then
          TouchArray[i].ClickElement.OnMouseRelease(
            TouchArray[i].ClickElement, TouchArray[i].x0, TouchArray[i].y0);
        if Assigned(TouchArray[i].ClickElement.OnDrop) then
          TouchArray[i].ClickElement.OnDrop(TouchArray[i].ClickElement,
            TouchArray[i].x0, TouchArray[i].y0);
      end;
      TouchArray.Remove(TouchArray[i]);
    end
    else
      Log(LogMouseError, _CurrentRoutine, 'ERROR: Touch event not found!');
  end
  else
    Log(LogMouseError, _CurrentRoutine, 'ERROR: Touch event list is empty!');

  StopProfiler;
end;

{-----------------------------------------------------------------------------}

procedure doMousePress(const Event: TInputPressRelease);
var
  NewEventTouch: DTouch;
  FingerIndex: integer;
  tmpLink: DAbstractElement;
  InterfaceCaughtEvent: boolean;
  i: integer;
begin
  StartProfiler;

  InterfaceCaughtEvent := False;

  FingerIndex := GetFingerIndex(Event);
  NewEventTouch := DTouch.Create(Event.Position[0], Event.Position[1], FingerIndex);

  //catch the element which has been pressed
  tmpLink := GUI.IfMouseOver(Round(Event.Position[0]), Round(
    Event.Position[1]), True, True);
  if (tmpLink is DSingleInterfaceElement) then
  begin
    NewEventTouch.ClickElement := DSingleInterfaceElement(tmpLink);
    if Assigned(NewEventTouch.ClickElement.OnMousePress) then
    begin
      NewEventTouch.ClickElement.OnMousePress(
        tmpLink, Round(Event.Position[0]), Round(Event.Position[1]));
      InterfaceCaughtEvent := True;
    end;
    if NewEventTouch.ClickElement.CanDrag then
      NewEventTouch.ClickElement.StartDrag(Round(Event.Position[0]), Round(Event.Position[1]));
  end;

  i := TouchArray.Add(NewEventTouch);

  {todo: if interface didn't catch the click then}
  if (CurrentGameMode = gmTravel) and (not InterfaceCaughtEvent) then
  begin
    //switch control mode
    if Event.MouseButton = mbRight then
      Camera.MouseLook := not Camera.MouseLook;
    //start dragging mouse look
    if i = 0 then
      DragMouseLook := True;
  end;

  Log(LogMouseInfo, _CurrentRoutine, 'Caught mouse press finger=' + IntToStr(FingerIndex));

  StopProfiler;
end;

{----------------------------------------------------------------------------}

var
  CameraWarning: boolean = True;

function doMouseLook(const Event: TInputMotion): boolean;
begin
  StartProfiler;

  if Camera = nil then
  begin
    if CameraWarning then
    begin
      Log(LogMouseSoftError, _CurrentRoutine,
        'Warning: Camera is not initialized for MouseLook');
      CameraWarning := False;
    end;
    Exit;
  end;

  if Camera.MouseLook then
  begin

    Camera.Cursor := mcForceNone; {do it only once}
    if not TVector2.PerfectlyEquals(Event.Position, GUI.Center) then
    begin
      Player.InputMouse(Event.Position - GUI.Center);
      doMouseLook := False;
      Window.MousePosition := GUI.Center; //=CenterMouseCursor inlined
    end
    else
      doMouseLook := True; {prevent onMotion call-back}

  end
  else
  if DragMouseLook then
  begin
    //DragMouseLook doesn't change cursor.position
    {however, it's a good idea to catch DragMouseLook not to go outside window
     - scroll it like Blender does}
    {$HINT Why Event.OldPosition rotation style is MUCH slower than MouseLook style rotation???}
    Player.InputMouse(Event.OldPosition - Event.Position);
    doMouseLook := False;
  end;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure CenterMouseCursor;
begin
  Window.MousePosition := GUI.Center;
end;

{----------------------------------------------------------------------------}

function doMouseDrag(const Event: TInputMotion): boolean;
var
  i: integer;
begin
  StartProfiler;

  {check for drag-n-drops}
  Result := False;
  {if Event.EventType = itMouseButton then }begin
    if TouchArray.Count > 0 then
    begin
      i := 0;
      repeat
        if TouchArray[i].FingerIndex = Event.FingerIndex then
        begin
          TouchArray[i].Update(Event);
          if (TouchArray[i].ClickElement <> nil) and
            (TouchArray[i].ClickElement.CanDrag) then
          begin
            TouchArray[i].ClickElement.Drag(Round(Event.Position[0]),
              Round(Event.Position[1]));
            Result := True;
          end;
          Break;
        end;
        Inc(i);
      until (i >= TouchArray.Count);
    end;

  end;

  StopProfiler;
end;


initialization
  TouchArray := DTouchList.Create;

finalization
  FreeAndNil(TouchArray);

end.
