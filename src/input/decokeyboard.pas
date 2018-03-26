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

unit DecoKeyboard;

{$INCLUDE compilerconfig.inc}

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
  {}
  DKeyboardInput = class(DObject)
  private
    {}
    KeyboardRecorder: DKeyboardRecorder;
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

procedure DKeyboardInput.doKeyboardRelease(const aKey: TKey);
begin
  {if context is 3D then }
  if aKey = KeyboardOptions.MoveForwardKey then
    Player.MoveKeyRelease(KeyboardForward)
  else
  if aKey = KeyboardOptions.MoveBackwardKey then
    Player.MoveKeyRelease(KeyboardBackward)
  else
  if aKey = KeyboardOptions.StrafeLeftKey then
    Player.MoveKeyRelease(KeyboardStrafeLeft)
  else
  if aKey = KeyboardOptions.StrafeRightKey then
    Player.MoveKeyRelease(KeyboardStrafeRight);
end;

{-----------------------------------------------------------------------------}

procedure DKeyboardInput.doKeyboardPress(const aKey: TKey);
begin
  {if context is 3D then }
  if aKey = KeyboardOptions.MoveForwardKey then
    Player.MoveKeyPress(KeyboardForward)
  else
  if aKey = KeyboardOptions.MoveBackwardKey then
    Player.MoveKeyPress(KeyboardBackward)
  else
  if aKey = KeyboardOptions.StrafeLeftKey then
    Player.MoveKeyPress(KeyboardStrafeLeft)
  else
  if aKey = KeyboardOptions.StrafeRightKey then
    Player.MoveKeyPress(KeyboardStrafeRight);

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

constructor DKeyboardInput.Create;
begin
  //inherited <-------- nothing to inherit
  KeyboardRecorder := DKeyboardRecorder.Create;
  LoadKeyboardConfig;
end;

{-----------------------------------------------------------------------------}

destructor DKeyboardInput.Destroy;
begin
  KeyboardRecorder.Free;
  inherited Destroy;
end;


end.

