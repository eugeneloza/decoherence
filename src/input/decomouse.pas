{ Copyright (C) 2012-2018 Yevhen Loza

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>. }

{---------------------------------------------------------------------------}

(* Handles mouse behaviour *)

unit DecoMouse;

{$INCLUDE compilerconfig.inc}

interface

uses
  Generics.Collections,
  CastleKeysMouse, CastleVectors,
  DecoPointerDeviceInput, DecoInterfaceCore,
  DecoGlobal, DecoTime;

type
  DMouseInput = class(DPointerDeviceInput)
  strict private
  type
    { Class for a single touch / mouse click }
    DTouch = class(DObject)
      TouchStart: DTime;
      FingerIndex: cardinal;
      OldPos: TVector2;     //to handle sweeps, drags and cancels
      ClickElement: DSingleInterfaceElement;
      constructor Create(const Pos: TVector2; const Finger: integer);
      procedure Update(const Event: TInputMotion);
    end;
    DTouchList = specialize TObjectList<DTouch>;
  strict private
    { All currently active touches/clicks }
    TouchArray: DTouchList;
    { Implements MouseLook (mouse only) }
    function doMouseLook(const Event: TInputMotion): boolean;
    { Implements MouseDrag (mouse/touch) }
    function doMouseDrag(const Event: TInputMotion): boolean;
  public
    { If mouse has been moved }
    procedure doMouseMotion(const Event: TInputMotion); override;
    { If mouse button / touch has been pressed }
    procedure doMousePress(const Event: TInputPressRelease); override;
    { If mouse button / touch has been released }
    procedure doMouseRelease(const Event: TInputPressRelease); override;
    { Centers the mouse cursor coordinates, without causing MouseLook }
    procedure CenterMouseCursor;
  public
    constructor Create; //override;
    destructor Destroy; override;
  end;


{............................................................................}
implementation
uses
  SysUtils,
  DecoPlayer, DecoGUI, DecoGUIScale, DecoGameMode, DecoWindow,
  DecoLog;

constructor DMouseInput.DTouch.Create(const Pos: TVector2; const Finger: integer);
begin
  //inherited Create <------- nothing to inherit
  OldPos := Pos;
  FingerIndex := Finger;
  TouchStart := DecoNow;
end;

{-----------------------------------------------------------------------------}

procedure DMouseInput.DTouch.Update(const Event: TInputMotion);
begin
  OldPos := Event.Position;
end;

{===========================================================================}

procedure DMouseInput.doMouseMotion(const Event: TInputMotion);
var
  Dragging: boolean;
begin
  if Player.MouseLook then
    if doMouseLook(Event) then
      Exit;

  Dragging := doMouseDrag(Event);

  GUI.UpdateCursor(Event.Position[0], Event.Position[1], TouchArray.Count > 0);
end;

{-----------------------------------------------------------------------------}

var
  { used to detect if mouse is in dragg-look mode }
  DragMouseLook: boolean = false;

procedure DMouseInput.doMouseRelease(const Event: TInputPressRelease);
var
  i: integer;
  Found: boolean;
begin
  if TouchArray.Count > 0 then
  begin
    i := 0;
    Found := false;
    repeat
      if TouchArray[i].FingerIndex = Event.FingerIndex then
        Found := true
      else
        inc(i);
    until (i >= TouchArray.Count) or Found;

    //stop dragging
    if i = 0 then
      DragMouseLook := false;

    Log(LogMouseInfo, CurrentRoutine, 'Caught mouse release finger=' +
      IntToStr(Event.FingerIndex) + ' n=' + IntToStr(i));
    if Found then
    begin
      if (TouchArray[i].ClickElement <> nil) then
      begin
        if Assigned(touchArray[i].ClickElement.OnMouseRelease) then
          TouchArray[i].ClickElement.OnMouseRelease(
            TouchArray[i].ClickElement, Round(TouchArray[i].OldPos[0]),
            Round(TouchArray[i].OldPos[1]));
        TouchArray[i].ClickElement.Drop;
{        if Assigned(TouchArray[i].ClickElement.OnDrop) then
          TouchArray[i].ClickElement.OnDrop(TouchArray[i].ClickElement,
            Round(TouchArray[i].OldPos[0]), Round(TouchArray[i].OldPos[1]));}
      end;
      TouchArray.Remove(TouchArray[i]);
    end
    else
      Log(LogMouseError, CurrentRoutine, 'ERROR: Touch event not found!');
  end
  else
    Log(LogMouseError, CurrentRoutine, 'ERROR: Touch event list is empty!');

  GUI.UpdateCursor(Event.Position[0], Event.Position[1], TouchArray.Count > 0);
end;

{-----------------------------------------------------------------------------}

procedure DMouseInput.doMousePress(const Event: TInputPressRelease);
var
  NewEventTouch: DTouch;
  tmpLink: DAbstractElement;
  InterfaceCaughtEvent: boolean;
  i: integer;
begin
  InterfaceCaughtEvent := false;

  NewEventTouch := DTouch.Create(Event.Position, Event.FingerIndex);

  //catch the element which has been pressed
  tmpLink := GUI.IfMouseOver(Round(Event.Position[0]),
    Round(Event.Position[1]), true, true);
  if (tmpLink is DSingleInterfaceElement) then
  begin
    NewEventTouch.ClickElement := DSingleInterfaceElement(tmpLink);
    if Assigned(NewEventTouch.ClickElement.OnMousePress) then
    begin
      NewEventTouch.ClickElement.OnMousePress(
        tmpLink, Round(Event.Position[0]), Round(Event.Position[1]));
      InterfaceCaughtEvent := true;
    end;
    if NewEventTouch.ClickElement.CanDrag then
      NewEventTouch.ClickElement.StartDrag(Round(Event.Position[0]), Round(Event.Position[1]));
  end;

  i := TouchArray.Add(NewEventTouch);

  if GameModeMouseLook then
    if (not InterfaceCaughtEvent) then
    begin
      if Event.MouseButton = mbRight then
          Player.ToggleMouseLook
      else
      //start dragging mouse look
      if i = 0 then
        DragMouseLook := true;
    end;

  GUI.UpdateCursor(Event.Position[0], Event.Position[1], TouchArray.Count > 0);

  Log(LogMouseInfo, CurrentRoutine, 'Caught mouse press finger=' + IntToStr(Event.FingerIndex));
end;

{----------------------------------------------------------------------------}

function DMouseInput.doMouseLook(const Event: TInputMotion): boolean;
begin
  //if gamemode ... then Exit;
  if Player.MouseLook then
  begin
    if not TVector2.PerfectlyEquals(Event.Position, GUICenter) then
    begin
      //Player.InputMouse(Event.Position - GUICenter);
      Result := false;
      Window.MousePosition := GUICenter; //=CenterMouseCursor inlined
    end
    else
      Result := true; {prevent onMotion call-back}

  end
  else
  if DragMouseLook then
  begin
    //DragMouseLook doesn't change cursor.position
    {however, it's a good idea to catch DragMouseLook not to go outside window
     - scroll it like Blender does}
    //Player.InputMouse(Event.OldPosition - Event.Position);
    Result := false;
  end;
end;

{----------------------------------------------------------------------------}

procedure DMouseInput.CenterMouseCursor;
begin
  Window.MousePosition := GUICenter;
end;

{----------------------------------------------------------------------------}

function DMouseInput.doMouseDrag(const Event: TInputMotion): boolean;
var
  i: integer;
begin
  {check for drag-n-drops}
  Result := false;

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
          Result := true;
        end;
        Break;
      end;
      inc(i);
    until (i >= TouchArray.Count);
  end;
end;

{----------------------------------------------------------------------------}

constructor DMouseInput.Create;
begin
  //inherited <---------- nothing to inherit
  TouchArray := DTouchList.Create;
  // init mouse cursor so that it always starts in a defined location, instead of (-1,-1)
  CenterMouseCursor;
end;

{----------------------------------------------------------------------------}

destructor DMouseInput.Destroy;
begin
  TouchArray.Free;
  inherited Destroy;
end;


end.

