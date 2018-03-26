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

(* Handles keyboard, mouse, gamepad, touch behaviour *)

unit DecoInput;

{$INCLUDE compilerconfig.inc}

interface

uses Classes, SysUtils,
  CastleVectors, CastleFilesUtils, CastleKeysMouse,
  DecoInterfaceCore,
  DecoKeyboard, DecoTouch,
  DecoTime, DecoGlobal;

type
  DInputProcessor = class(DObject)
  public
    TouchInput: DTouchInput;
    KeyboardInput: DKeyboardInput;
    //GamepadInput: ...
  public
    constructor Create; //override;
    destructor Destroy; override;
  end;


var
  { Handles all possible ways of input }
  InputProcessor: DInputProcessor;

{ Initializes Input events and loads key bindings
  Input must be initialized AFTER window is created }
procedure InitInput;
{ Releases Input events and InputProcessor }
procedure FreeInput;
{............................................................................}
implementation

uses CastleWindow,
  DecoWindow,
  DecoLog;

constructor DInputProcessor.Create;
begin
  //inherited Create <------ nothing to inherit
  TouchInput := DTouchInput.Create;
  KeyboardInput := DKeyboardInput.Create;
end;

{----------------------------------------------------------------------------}

destructor DInputProcessor.Destroy;
begin
  TouchInput.Free;
  KeyboardInput.Free;
  inherited Destroy;
end;

{----------------------------------------------------------------------------}


{======================== EVENTS =================================}

{$PUSH}{$WARN 5024 off : Parameter "$1" not used}
procedure doPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itMouseButton then
    InputProcessor.TouchInput.doMousePress(Event)
  else
  if Event.EventType = itKey then
  begin
    {some generic buttons here}
    if Event.Key = InputProcessor.KeyboardInput.KeyboardOptions.ScreenShotKey then
      MakeScreenShot;
    //hardcoded keys
    case Event.Key of
      KeyPrintScreen: //KeyPrintScreen doesn't work in x-window system if assigned to some external program like scrot
        MakeScreenShot;
    end;

    InputProcessor.KeyboardInput.doKeyboardPress(Event.Key);
  end;
end;

{--------------------------------------------------------------------------}

procedure doRelease(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itMouseButton then
    InputProcessor.TouchInput.doMouseRelease(Event)
  else
  if Event.EventType = itKey then
    InputProcessor.KeyboardInput.doKeyboardRelease(Event.Key);
end;

{--------------------------------------------------------------------------}

procedure doMotion(Container: TUIContainer; const Event: TInputMotion);
begin
  InputProcessor.TouchInput.doMouseMotion(Event);
end;
{$POP}

{............................................................................}
procedure InitInput;
begin
  InputProcessor := DInputProcessor.Create;
  Window.OnPress := @doPress;
  Window.OnRelease := @doRelease;
  Window.OnMotion := @doMotion;
  InputProcessor.TouchInput.CenterMouseCursor;
end;

procedure FreeInput;
begin
  InputProcessor.Free;
  Window.OnPress := nil; //to be on the safe side so that already-freed Player won't accidentally get input
  Window.OnRelease := nil;
  Window.OnMotion := nil;
end;

end.
