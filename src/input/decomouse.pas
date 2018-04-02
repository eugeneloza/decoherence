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
  CastleKeysMouse, CastleVectors,
  DecoPointerDeviceInput, DecoInterfaceCore;

type
  DMouseInput = class(DPointerDeviceInput)
  strict private
    type DMousePressEvent = record
      { If this mouse button is pressed}
      isPressed: boolean;
      { Coordinates where the press occured }
      Position: TVector2;
      { Caches if this event is not a dragging event }
      isDragging: boolean;
    end;
  strict private
    MouseButton: array [TMouseButton] of DMousePressEvent;
    { Implements MouseLook (mouse only) }
    function doMouseLook(const Event: TInputMotion): boolean;
    procedure TryClick(const Event: TInputPressRelease);
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

var
  { used to detect if mouse is in dragg-look mode }
  DragMouseLook: boolean = false;


procedure DMouseInput.doMousePress(const Event: TInputPressRelease);
var
  tmpLink: DAbstractElement;
  InterfaceCaughtEvent: boolean;
  ClickElement, DragElement: DSingleInterfaceElement;
begin
  InterfaceCaughtEvent := false;

  // record start of the click
  with MouseButton[Event.MouseButton] do
  begin
    isPressed := true;
    Position := Event.Position;
    isDragging := false;
  end;

  //UGLY BUGFIX
  if GUI.Cursor.DragElement <> nil then
    GUI.Cursor.DragElement.Drop;

  DragElement := nil;
  //catch the element which has been pressed
  tmpLink := GUI.IfMouseOver(Round(Event.Position[0]), Round(Event.Position[1]),
    true, true);

  //process click event
  if (tmpLink is DSingleInterfaceElement) then
  begin
    ClickElement := DSingleInterfaceElement(tmpLink);

    if Event.MouseButton = mbLeft then
    begin
      if ClickElement.CanDrag then
      begin
        DragElement := ClickElement;
        DragElement.StartDrag(Round(Event.Position[0]), Round(Event.Position[1]));
        InterfaceCaughtEvent := true;
      end;
    end;

    if Assigned(ClickElement.OnMouseLeftButton) or
      Assigned(ClickElement.OnMouseRightButton) then
    begin
      GUI.Cursor.ClickElement := ClickElement;
      InterfaceCaughtEvent := true;
    end;
  end;

  if (not InterfaceCaughtEvent) and GameModeMouseLook then
  begin
    // if this is a right-click, change control mode
    if Event.MouseButton = mbRight then
    begin
      CenterMouseCursor;
      Player.ToggleMouseLook
    end;

    //start dragging mouse look
    if (Event.MouseButton = mbMiddle) then
      DragMouseLook := true;
  end;

  GUI.UpdateCursor(Event.Position[0], Event.Position[1], DragElement);

  Log(LogMouseInfo, CurrentRoutine, 'Caught mouse press finger=' + IntToStr(Event.FingerIndex));
end;

{-----------------------------------------------------------------------------}

procedure DMouseInput.doMouseMotion(const Event: TInputMotion);
var
  mb: TMouseButton;
begin
  { this will check all conditions properly and handle both
    mouse look and mouse drag depending on the current mode }
  if doMouseLook(Event) then
    Exit;

  { if mouse cursor shifted too far away (1 interface item away)
    then it's a drag-move, not a click }
  for mb in Event.Pressed do
    if (MouseButton[mb].Position[0] - Event.Position[0] > GUIUnit / 2) or
       (MouseButton[mb].Position[1] - Event.Position[1] > GUIUnit / 2) then
      MouseButton[mb].isDragging := false;

  // if we have a interface element dragging - update it.
  //!!! TODO: move it to GUI.UpdateCursor !!!
  if (GUI.Cursor.DragElement <> nil) then
    GUI.Cursor.DragElement.Drag(Round(Event.Position[0]), Round(Event.Position[1]));

  GUI.UpdateCursor(Event.Position[0], Event.Position[1], GUI.Cursor.DragElement);
end;

{-----------------------------------------------------------------------------}

procedure DMouseInput.TryClick(const Event: TInputPressRelease);
begin
  if GUI.Cursor.ClickElement = nil then Exit;
  { There might be (and quiet possible) a bug here - when the "shift" of the
    cursor is not too large to cancel the click, but the element the click was
    made is different. }

  if (Event.MouseButton = mbLeft) and
    Assigned(GUI.Cursor.ClickElement.OnMouseLeftButton) then
    GUI.Cursor.ClickElement.OnMouseLeftButton(
      GUI.Cursor.ClickElement, Round(Event.Position[0]), Round(Event.Position[1]))
  else
  if (Event.MouseButton = mbRight) and
    Assigned(GUI.Cursor.ClickElement.OnMouseRightButton) then
    GUI.Cursor.ClickElement.OnMouseRightButton(
      GUI.Cursor.ClickElement, Round(Event.Position[0]), Round(Event.Position[1]));

  GUI.Cursor.ClickElement := nil;
end;

{-----------------------------------------------------------------------------}

procedure DMouseInput.doMouseRelease(const Event: TInputPressRelease);
begin
  // stop the click
  MouseButton[Event.MouseButton].isPressed := false;

  //stop drag mouse look
  if Event.MouseButton = mbMiddle then
    DragMouseLook := false;

  if Event.MouseButton = mbLeft then
  begin
    //stop dragging interface element
    if GUI.Cursor.DragElement <> nil then
      GUI.Cursor.DragElement.Drop;

    //try clicking it if mouse didn't move too far away
    if MouseButton[Event.MouseButton].isDragging = false then
      TryClick(Event);
  end;

  GUI.UpdateCursor(Event.Position[0], Event.Position[1]);
end;

{----------------------------------------------------------------------------}

function DMouseInput.doMouseLook(const Event: TInputMotion): boolean;
begin
  Result := false;

  //if gamemode ... then Exit;
  if Player.MouseLook then
  begin
    if not TVector2.PerfectlyEquals(Event.Position, GUICenter) then
    begin
      Player.doLook(Event.Position - GUICenter);
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
    Player.doLook(Event.OldPosition - Event.Position);
    Result := false;
  end;
end;

{----------------------------------------------------------------------------}

procedure DMouseInput.CenterMouseCursor;
begin
  Window.MousePosition := GUICenter;
end;

{----------------------------------------------------------------------------}

constructor DMouseInput.Create;
begin
  //inherited <---------- nothing to inherit

  // init mouse cursor so that it always starts in a defined location, instead of (-1,-1)
  CenterMouseCursor;
end;

{----------------------------------------------------------------------------}

destructor DMouseInput.Destroy;
begin
  inherited Destroy;
end;


end.

