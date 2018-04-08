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

(* Handles keyboard behaviour *)

{$INCLUDE compilerconfig.inc}

unit DecoKeyboard;

interface

uses
  CastleKeysMouse,
  DecoKeyboardRecorder,
  DecoGlobal;

type
  { Currently assigned control keys }
  DKeyboardOptions = record
    MoveForwardKey, MoveBackwardKey, StrafeLeftKey, StrafeRightKey: TKey;
    ScreenShotKey: TKey;
  end;

type
  { Internal names for movement keys }
  DMoveKey = (KeyboardForward, KeyboardBackward, KeyboardStrafeLeft, KeyboardStrafeRight);


type
  {}
  DKeyboardInput = class(DObject)
  private
    { Records keys sequences }
    KeyboardRecorder: DKeyboardRecorder;
    { Keys are source of discrete movement unlike Proplusion and Gamepad axes }
    MoveKeys: array[DMoveKey] of boolean;
    { Keyboard movement }
    procedure MoveKeyPress(const aKey: DMoveKey);
    procedure MoveKeyRelease(const aKey: DMoveKey);
    { Reset all input controls to initial values }
    procedure ReleaseControls;
  public
    { Currently assigned keyboard keys }
    KeyboardOptions: DKeyboardOptions;
  public
    { If keyboard button has been pressed }
    procedure doKeyboardPress(const aKey: TKey);
    { If keyboard button has been released }
    procedure doKeyboardRelease(const aKey: TKey);
    { Loads input configuration (key bindings, etc) }
    procedure LoadKeyboardConfig;
  public
    constructor Create; //override;
    destructor Destroy; override;
  end;

{............................................................................}
implementation
uses
  DecoPlayer;

{----------------------------------------------------------------------------}

procedure DKeyboardInput.MoveKeyPress(const aKey: DMoveKey);
begin
  MoveKeys[aKey] := true;
  case aKey of
    KeyboardForward: Player.doAccelerateForward(+1.0);
    KeyboardBackward: Player.DoAccelerateForward(-1.0);
    KeyboardStrafeLeft: Player.doAccelerateStrafe(+1.0);
    KeyboardStrafeRight: Player.doAccelerateStrafe(-1.0);
  end;
end;

{----------------------------------------------------------------------------}

procedure DKeyboardInput.MoveKeyRelease(const aKey: DMoveKey);
begin
  MoveKeys[aKey] := false;
  {not sure if this is the right behaviour, but let it be for now}
  case aKey of
    KeyboardForward: Player.doAccelerateForward(0);
    KeyboardBackward: Player.DoAccelerateForward(0);
    KeyboardStrafeLeft: Player.doAccelerateStrafe(0);
    KeyboardStrafeRight: Player.doAccelerateStrafe(0);
  end;
end;

{----------------------------------------------------------------------------}

procedure DKeyboardInput.doKeyboardRelease(const aKey: TKey);
begin
  {if context is 3D then }
  if aKey = KeyboardOptions.MoveForwardKey then
    MoveKeyRelease(KeyboardForward)
  else
  if aKey = KeyboardOptions.MoveBackwardKey then
    MoveKeyRelease(KeyboardBackward)
  else
  if aKey = KeyboardOptions.StrafeLeftKey then
    MoveKeyRelease(KeyboardStrafeLeft)
  else
  if aKey = KeyboardOptions.StrafeRightKey then
    MoveKeyRelease(KeyboardStrafeRight);
end;

{-----------------------------------------------------------------------------}

procedure DKeyboardInput.doKeyboardPress(const aKey: TKey);
begin
  {if context is 3D then }
  if aKey = KeyboardOptions.MoveForwardKey then
    MoveKeyPress(KeyboardForward)
  else
  if aKey = KeyboardOptions.MoveBackwardKey then
    MoveKeyPress(KeyboardBackward)
  else
  if aKey = KeyboardOptions.StrafeLeftKey then
    MoveKeyPress(KeyboardStrafeLeft)
  else
  if aKey = KeyboardOptions.StrafeRightKey then
    MoveKeyPress(KeyboardStrafeRight);

  KeyboardRecorder.KeyRecorder(aKey);
end;

{-----------------------------------------------------------------------------}

procedure DKeyboardInput.LoadKeyboardConfig;
begin
  with KeyboardOptions do
  begin
    MoveForwardKey := KeyW;
    MoveBackwardKey := KeyS;
    StrafeLeftKey := KeyA;
    StrafeRightKey := KeyD;
    ScreenShotKey := KeyP;
  end;
end;

{-----------------------------------------------------------------------------}

procedure DKeyboardInput.ReleaseControls;
var
  k: DMoveKey;
begin
  for k in DMoveKey do
    MoveKeys[k] := false;
end;

{-----------------------------------------------------------------------------}

constructor DKeyboardInput.Create;
begin
  //inherited <-------- nothing to inherit
  KeyboardRecorder := DKeyboardRecorder.Create;
  LoadKeyboardConfig;
  ReleaseControls;
end;

{-----------------------------------------------------------------------------}

destructor DKeyboardInput.Destroy;
begin
  KeyboardRecorder.Free;
  inherited Destroy;
end;


end.

