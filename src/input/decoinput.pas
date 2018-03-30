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
  DecoKeyboard, DecoMouse;

var
  TouchInput: DTouchInput;
  KeyboardInput: DKeyboardInput;
  //GamepadInput: ...

{ Initializes Input events and loads key bindings
  Input must be initialized AFTER window is created }
procedure InitInput;
{ Releases Input events and InputProcessor }
procedure FreeInput;
{............................................................................}
implementation
uses CastleWindow, CastleKeysMouse,
  DecoWindow;

{======================== EVENTS =================================}

{$PUSH}{$WARN 5024 off : Parameter "$1" not used}
procedure doPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itMouseButton then
    TouchInput.doMousePress(Event)
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
    TouchInput.doMouseRelease(Event)
  else
  if Event.EventType = itKey then
    KeyboardInput.doKeyboardRelease(Event.Key);
end;

{--------------------------------------------------------------------------}

procedure doMotion(Container: TUIContainer; const Event: TInputMotion);
begin
  TouchInput.doMouseMotion(Event);
end;
{$POP}

{............................................................................}
procedure InitInput;
begin
  TouchInput := DTouchInput.Create;
  KeyboardInput := DKeyboardInput.Create;
  Window.OnPress := @doPress;
  Window.OnRelease := @doRelease;
  Window.OnMotion := @doMotion;
  // init mouse cursor so that it always starts in a defined location, instead of (-1,-1)
  TouchInput.CenterMouseCursor;
end;

procedure FreeInput;
begin
  Window.OnPress := nil; //to be on the safe side so that already-freed Player won't accidentally get input
  Window.OnRelease := nil;
  Window.OnMotion := nil;
  TouchInput.Free;
  KeyboardInput.Free;
end;

end.
