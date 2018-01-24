{Copyright (C) 2012-2018 Yevhen Loza

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

(* Handles keyboard, mouse, gamepad, touch behaviour *)

unit DecoInput;

{$INCLUDE compilerconfig.inc}

interface

uses Classes, SysUtils, Generics.Collections,
  CastleVectors, CastleFilesUtils, CastleKeysMouse,
  DecoTime, DecoGlobal;


type
  { Currently assigned control keys }
  DInputOptions = record
    MoveForwardKey, MoveBackwardKey, StrafeLeftKey, StrafeRightKey: TKey;
    ScreenShotKey: TKey;
    LongTouch: single;
  end;

type
  DInputProcessor = class(DObject)
  strict private
  type
    { Class for a single touch / mouse click }
    DTouch = class(DObject)
      TouchStart: DTime;
      FingerIndex: cardinal;
      OldPos: TVector2;     //to handle sweeps, drags and cancels
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
    { Some cheat-codes related fun }
    procedure KeyRecorder(const aKey: TKey);
  public
    { Currently assigned keyboard keys }
    InputOptions: DInputOptions;
    { If mouse has been moved }
    procedure doMouseMotion(const Event: TInputMotion);
    { If mouse button / touch has been pressed }
    procedure doMousePress(const Event: TInputPressRelease);
    { If mouse button / touch has been released }
    procedure doMouseRelease(const Event: TInputPressRelease);
    { If keyboard button has been pressed }
    procedure doKeyboardPress(const aKey: TKey);
    { If keyboard button has been released }
    procedure doKeyboardRelease(const aKey: TKey);
    { Centers the mouse cursor coordinates, without causing MouseLook }
    procedure CenterMouseCursor;
  public
    constructor Create;
    destructor Destroy; override;
    { Loads input configuration (key bindings, etc) }
    procedure LoadInputConfig;
  end;


var
  { Handles all possible ways of input }
  InputProcessor: DInputProcessor;

{ Initializes Input events and loads key bindings
  Input must be initialized AFTER window is open }
procedure InitInput;
{ Releases Input events and InputProcessor }
procedure FreeInput;
{............................................................................}
implementation

uses CastleWindow,
  DecoPlayer,
  DecoGuiScale,
  DecoWindow, DecoLog;

{================================= TOUCH ====================================}

constructor DInputProcessor.DTouch.Create(const Pos: TVector2; const Finger: integer);
begin
  OldPos := Pos;
  FingerIndex := Finger;
  TouchStart := DecoNow;
end;

{-----------------------------------------------------------------------------}

procedure DInputProcessor.DTouch.Update(const Event: TInputMotion);
begin
  OldPos := Event.Position;
end;

{============================= INPUT PROCESSOR ============================}
var
  RecordKeys: boolean = False;
  RecordedKeys: string;

procedure DInputProcessor.KeyRecorder(const aKey: TKey);
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
  if {CurrentGameMode = gmTravel} true then
  begin
    if RecordKeys then
    begin
      if AddKey then
      begin
        if TestRecord then
        begin
          if (RecordedKeys = test1) or (RecordedKeys = test2) then
          begin
            Log(true, CurrentRoutine, 'No! This is a different game!');
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

procedure DInputProcessor.doKeyboardRelease(const aKey: TKey);
begin
  {if context is 3D then }
  if aKey = InputOptions.MoveForwardKey then
    Player.MoveKeyRelease(KeyboardForward)
  else
  if aKey = InputOptions.MoveBackwardKey then
    Player.MoveKeyRelease(KeyboardBackward)
  else
  if aKey = InputOptions.StrafeLeftKey then
    Player.MoveKeyRelease(KeyboardStrafeLeft)
  else
  if aKey = InputOptions.StrafeRightKey then
    Player.MoveKeyRelease(KeyboardStrafeRight);
end;

{-----------------------------------------------------------------------------}

procedure DInputProcessor.doKeyboardPress(const aKey: TKey);
begin
  {if context is 3D then }
  if aKey = InputOptions.MoveForwardKey then
    Player.MoveKeyPress(KeyboardForward)
  else
  if aKey = InputOptions.MoveBackwardKey then
    Player.MoveKeyPress(KeyboardBackward)
  else
  if aKey = InputOptions.StrafeLeftKey then
    Player.MoveKeyPress(KeyboardStrafeLeft)
  else
  if aKey = InputOptions.StrafeRightKey then
    Player.MoveKeyPress(KeyboardStrafeRight);

  KeyRecorder(aKey);
end;

{-----------------------------------------------------------------------------}

procedure DInputProcessor.doMouseMotion(const Event: TInputMotion);
var
//  tmpLink: DAbstractElement;
  Dragging: boolean;
begin
  if Player.MouseLook then
    if doMouseLook(Event) then
      Exit;

  Dragging := doMouseDrag(Event);

  {mouse over / if no drag-n-drop}
  //this is not needed at the moment, we'll turn here a bit later when implementing drag-n-drop
  //no mouseover is detected if no ifmouseover is run, so should still be here
  if not Dragging then
  begin
    {tmpLink := GUI.IfMouseOver(Round(Event.Position[0]), Round(
      Event.Position[1]), True, True);
    if tmpLink <> nil then
      Log(logVerbose, _CurrentRoutine, 'Motion caught ' + tmpLink.ClassName);}
  end;
end;

{-----------------------------------------------------------------------------}

function DInputProcessor.GetFingerIndex(const Event: TInputPressRelease): integer;
begin
  if Event.MouseButton = mbLeft then
    Result := Event.FingerIndex
  else if Event.MouseButton = mbRight then
    Result := 100
  else if Event.MouseButton = mbMiddle then
    Result := 200
  else
    Log(LogMouseError, CurrentRoutine, 'Unknown Event.FingerIndex');
end;

{-----------------------------------------------------------------------------}

var
  { used to detect if mouse is in dragg-look mode }
  DragMouseLook: boolean = False;

procedure DInputProcessor.doMouseRelease(const Event: TInputPressRelease);
var
  i, FingerIndex: integer;
  Found: boolean;
begin
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

    Log(LogMouseInfo, CurrentRoutine, 'Caught mouse release finger=' +
      IntToStr(FingerIndex) + ' n=' + IntToStr(i));
    if Found then
    begin
      if (DecoNow - TouchArray[i].TouchStart > InputOptions.LongTouch) then begin
        Log(LogMouseInfo, CurrentRoutine, 'Long-touch caught!');
      end;
      {if (TouchArray[i].ClickElement <> nil) then
      begin
        if Assigned(touchArray[i].ClickElement.OnMouseRelease) then
          TouchArray[i].ClickElement.OnMouseRelease(
            TouchArray[i].ClickElement, TouchArray[i].x0, TouchArray[i].y0);
        if Assigned(TouchArray[i].ClickElement.OnDrop) then
          TouchArray[i].ClickElement.OnDrop(TouchArray[i].ClickElement,
            TouchArray[i].x0, TouchArray[i].y0);
      end;}
      TouchArray.Remove(TouchArray[i]);
    end
    else
      Log(LogMouseError, CurrentRoutine, 'ERROR: Touch event not found!');
  end
  else
    Log(LogMouseError, CurrentRoutine, 'ERROR: Touch event list is empty!');
end;

{-----------------------------------------------------------------------------}

procedure DInputProcessor.doMousePress(const Event: TInputPressRelease);
var
  NewEventTouch: DTouch;
  FingerIndex: integer;
  //tmpLink: DAbstractElement;
  InterfaceCaughtEvent: boolean;
  i: integer;
begin
  InterfaceCaughtEvent := False;

  FingerIndex := GetFingerIndex(Event);
  NewEventTouch := DTouch.Create(Event.Position, FingerIndex);

  //catch the element which has been pressed
{  tmpLink := GUI.IfMouseOver(Round(Event.Position[0]), Round(
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
  end; }

  i := TouchArray.Add(NewEventTouch);

  {todo: if interface didn't catch the click then}
 { if (CurrentGameMode = gmTravel) and (not InterfaceCaughtEvent) then
  begin
    //start dragging mouse look
    if i = 0 then
      DragMouseLook := True;
  end; }

  //switch control mode
  if Event.MouseButton = mbRight then
    Player.ToggleMouseLook;

  Log(LogMouseInfo, CurrentRoutine, 'Caught mouse press finger=' + IntToStr(FingerIndex));
end;

{----------------------------------------------------------------------------}

function DInputProcessor.doMouseLook(const Event: TInputMotion): boolean;
begin
{  if Camera = nil then
  begin
    Log(LogMouseSoftError, _CurrentRoutine,
      'Warning: Camera is not initialized for MouseLook');
    Exit;
  end;

  if Camera.MouseLook then
  begin

    Camera.Cursor := mcForceNone; {do it only once}  }
    if not TVector2.PerfectlyEquals(Event.Position, GUICenter) then
    begin
      //Player.InputMouse(Event.Position - GUICenter);
      Result := False;
      Window.MousePosition := GUICenter; //=CenterMouseCursor inlined
    end
    else
      Result := True; {prevent onMotion call-back}

 { end
  else
  if DragMouseLook then
  begin
    //DragMouseLook doesn't change cursor.position
    {however, it's a good idea to catch DragMouseLook not to go outside window
     - scroll it like Blender does}
    {$HINT Why Event.OldPosition rotation style is MUCH slower than MouseLook style rotation???}
    Player.InputMouse(Event.OldPosition - Event.Position);
    Result := False;
  end;}
end;

{----------------------------------------------------------------------------}

procedure DInputProcessor.CenterMouseCursor;
begin
  Window.MousePosition := GUICenter;
end;

{----------------------------------------------------------------------------}

function DInputProcessor.doMouseDrag(const Event: TInputMotion): boolean;
var
  i: integer;
begin
  {check for drag-n-drops}
  Result := False;

  if TouchArray.Count > 0 then
  begin
    i := 0;
    repeat
      if TouchArray[i].FingerIndex = Event.FingerIndex then
      begin
        TouchArray[i].Update(Event);
        {if (TouchArray[i].ClickElement <> nil) and
          (TouchArray[i].ClickElement.CanDrag) then
        begin
          TouchArray[i].ClickElement.Drag(Round(Event.Position[0]),
            Round(Event.Position[1]));
          Result := True;
        end;  }
        Break;
      end;
      Inc(i);
    until (i >= TouchArray.Count);
  end;
end;

{----------------------------------------------------------------------------}

constructor DInputProcessor.Create;
begin
  //inherited Create <------ nothing to inherit
  TouchArray := DTouchList.Create;
end;

{----------------------------------------------------------------------------}

destructor DInputProcessor.Destroy;
begin
  TouchArray.Free;
  inherited Destroy;
end;

{----------------------------------------------------------------------------}

procedure DInputProcessor.LoadInputConfig;
begin
  with InputOptions do
  begin
    MoveForwardKey := K_W;
    MoveBackwardKey := K_S;
    StrafeLeftKey := K_A;
    StrafeRightKey := K_D;
    ScreenShotKey := K_P;
    LongTouch := 0.5; { in seconds // this is standart for Android }
  end;
end;

{======================== EVENTS =================================}

{$PUSH}{$WARN 5024 off : Parameter "$1" not used}
procedure doPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  // todo Joystick
  if Event.EventType = itMouseButton then
    InputProcessor.doMousePress(Event)
  else
  if Event.EventType = itKey then
  begin
    {some generic buttons here}
    if Event.Key = InputProcessor.InputOptions.ScreenShotKey then
      MakeScreenShot;
    //hardcoded keys
    case Event.Key of
      K_PrintScreen: //k_printscreen doesn't work in x-window system if assigned to some external program like scrot
        MakeScreenShot;
{      K_r: Player.CurrentParty.Rest;
      k_i: if AmbientIntensity.Ambient = 0 then
          AmbientIntensity.SetAmbientIntensity(3)
        else
          AmbientIntensity.SetAmbientIntensity(0);}

    end;

{    if (CurrentGameMode = gmTravel) and (Player <> nil) then }
      InputProcessor.doKeyboardPress(Event.Key);
  end;
end;

{--------------------------------------------------------------------------}

procedure doRelease(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itMouseButton then
    InputProcessor.doMouseRelease(Event)
  else
  if Event.EventType = itKey then
    InputProcessor.doKeyboardRelease(Event.Key);
end;

{--------------------------------------------------------------------------}

procedure doMotion(Container: TUIContainer; const Event: TInputMotion);
begin
  InputProcessor.doMouseMotion(Event);
end;
{$POP}

{............................................................................}
procedure InitInput;
begin
  InputProcessor := DInputProcessor.Create;
  InputProcessor.LoadInputConfig;
  Window.OnPress := @doPress;
  Window.OnRelease := @doRelease;
  Window.OnMotion := @doMotion;
end;

procedure FreeInput;
begin
  InputProcessor.Free;
  Window.OnPress := nil; //to be on the safe side so that already-freed Player won't get input
  Window.OnRelease := nil;
  Window.OnMotion := nil;
end;

end.
