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

(* Basic unit for handling input handlers:
   Mouse(touch) and keyboard at the moment*)

unit DecoInput;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoKeyboard, DecoPointerDeviceInput;

{ Resets mouse cursor to the central position
  does anything only in case Mouse is used as a pointer device}
procedure ResetMouseCursor;

{ Initializes Input events and loads key bindings
  Input must be initialized AFTER window is created }
procedure InitInput;
{ Releases Input events and InputProcessor }
procedure FreeInput;
{............................................................................}
implementation
uses CastleWindow, CastleKeysMouse,
  DecoMouse,
  DecoWindow;

var
  { Handles mouse or touch input }
  PointerInput: DPointerDeviceInput;
  { Handles keyboard input }
  KeyboardInput: DKeyboardInput;
  { Handles gamepad/joystick input }
  //GamepadInput: ...


procedure ResetMouseCursor;
begin
  if PointerInput is DMouseInput then
    DMouseInput(PointerInput).CenterMouseCursor;
end;

{======================== EVENTS =================================}

{$PUSH}{$WARN 5024 off : Parameter "$1" not used}
procedure doPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itMouseButton then
    PointerInput.doMousePress(Event)
  else
  if Event.EventType = itKey then
  begin
    {some generic buttons here}
    if Event.Key = KeyboardInput.KeyboardOptions.ScreenShotKey then
      MakeScreenShot;
    //hardcoded keys
    case Event.Key of
      KeyPrintScreen: //KeyPrintScreen doesn't work in x-window system if assigned to some external program like scrot
        MakeScreenShot;
    end;

    KeyboardInput.doKeyboardPress(Event.Key);
  end;
end;

{--------------------------------------------------------------------------}

procedure doRelease(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itMouseButton then
    PointerInput.doMouseRelease(Event)
  else
  if Event.EventType = itKey then
    KeyboardInput.doKeyboardRelease(Event.Key);
end;

{--------------------------------------------------------------------------}

procedure doMotion(Container: TUIContainer; const Event: TInputMotion);
begin
  PointerInput.doMouseMotion(Event);
end;
{$POP}

{............................................................................}
procedure InitInput;
begin
  PointerInput := DMouseInput.Create;
  KeyboardInput := DKeyboardInput.Create;

  Window.OnPress := @doPress;
  Window.OnRelease := @doRelease;
  Window.OnMotion := @doMotion;
end;

{--------------------------------------------------------------------------}

procedure FreeInput;
begin
  //to be on the safe side so that already-freed Player won't accidentally get input
  Window.OnPress := nil;
  Window.OnRelease := nil;
  Window.OnMotion := nil;

  PointerInput.Free;
  KeyboardInput.Free;
end;

end.
