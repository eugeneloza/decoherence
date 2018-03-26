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

(* Handles mouse and touch behaviour *)

unit DecoTouch;

{$INCLUDE compilerconfig.inc}

interface

uses
  Generics.Collections,
  CastleKeysMouse, CastleVectors,
  DecoInterfaceCore,
  DecoGlobal, DecoTime;

type
  { Currently assigned control keys }
  DTouchOptions = record
    LongTouch: single;
  end;

type
  DTouchInput = class(DObject)
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
    function GetFingerIndex(const Event: TInputPressRelease): integer;
    { Implements MouseLook (mouse only) }
    function doMouseLook(const Event: TInputMotion): boolean;
    { Implements MouseDrag (mouse/touch) }
    function doMouseDrag(const Event: TInputMotion): boolean;
  public
    TouchOptions: DTouchOptions;
    { If mouse has been moved }
    procedure doMouseMotion(const Event: TInputMotion);
    { If mouse button / touch has been pressed }
    procedure doMousePress(const Event: TInputPressRelease);
    { If mouse button / touch has been released }
    procedure doMouseRelease(const Event: TInputPressRelease);
    { Centers the mouse cursor coordinates, without causing MouseLook }
    procedure CenterMouseCursor;

    procedure LoadTouchConfig;
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

const
  RightButtonFingerIndex = 100;
  MiddleButtonFingerIndex = 200;

{================================= TOUCH ====================================}

constructor DTouchInput.DTouch.Create(const Pos: TVector2; const Finger: integer);
begin
  //inherited Create <------- nothing to inherit
  OldPos := Pos;
  FingerIndex := Finger;
  TouchStart := DecoNow;
end;

{-----------------------------------------------------------------------------}

procedure DTouchInput.DTouch.Update(const Event: TInputMotion);
begin
  OldPos := Event.Position;
end;

{===========================================================================}

procedure DTouchInput.doMouseMotion(const Event: TInputMotion);
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

function DTouchInput.GetFingerIndex(const Event: TInputPressRelease): integer;
begin
  if Event.MouseButton = mbLeft then
    Result := Event.FingerIndex
  else if Event.MouseButton = mbRight then
    Result := RightButtonFingerIndex
  else if Event.MouseButton = mbMiddle then
    Result := MiddleButtonFingerIndex
  else
    Log(LogMouseError, CurrentRoutine, 'Unknown Event.FingerIndex');
end;

{-----------------------------------------------------------------------------}

var
  { used to detect if mouse is in dragg-look mode }
  DragMouseLook: boolean = false;

procedure DTouchInput.doMouseRelease(const Event: TInputPressRelease);
var
  i, FingerIndex: integer;
  Found: boolean;
begin
  if TouchArray.Count > 0 then
  begin
    FingerIndex := GetFingerIndex(Event);
    i := 0;
    Found := false;
    repeat
      if TouchArray[i].FingerIndex = FingerIndex then
        Found := true
      else
        inc(i);
    until (i >= TouchArray.Count) or Found;

    //stop dragging
    if i = 0 then
      DragMouseLook := false;

    Log(LogMouseInfo, CurrentRoutine, 'Caught mouse release finger=' +
      IntToStr(FingerIndex) + ' n=' + IntToStr(i));
    if Found then
    begin
      if (DecoNow - TouchArray[i].TouchStart > TouchOptions.LongTouch) then
      begin
        Log(LogMouseInfo, CurrentRoutine, 'Long-touch caught!');
      end;
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

procedure DTouchInput.doMousePress(const Event: TInputPressRelease);
var
  NewEventTouch: DTouch;
  FingerIndex: integer;
  tmpLink: DAbstractElement;
  InterfaceCaughtEvent: boolean;
  i: integer;
begin
  InterfaceCaughtEvent := false;

  FingerIndex := GetFingerIndex(Event);
  NewEventTouch := DTouch.Create(Event.Position, FingerIndex);

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

  Log(LogMouseInfo, CurrentRoutine, 'Caught mouse press finger=' + IntToStr(FingerIndex));
end;

{----------------------------------------------------------------------------}

function DTouchInput.doMouseLook(const Event: TInputMotion): boolean;
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

procedure DTouchInput.CenterMouseCursor;
begin
  Window.MousePosition := GUICenter;
end;

{----------------------------------------------------------------------------}

function DTouchInput.doMouseDrag(const Event: TInputMotion): boolean;
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

procedure DTouchInput.LoadTouchConfig;
begin
  with TouchOptions do
  begin
    LongTouch := 0.5; { in seconds // 500ms is standard for Android }
  end;
end;

{----------------------------------------------------------------------------}

constructor DTouchInput.Create;
begin
  //inherited <---------- nothing to inherit
  TouchArray := DTouchList.Create;
  LoadTouchConfig;
end;

{----------------------------------------------------------------------------}

destructor DTouchInput.Destroy;
begin
  TouchArray.Free;
  inherited Destroy;
end;


end.

