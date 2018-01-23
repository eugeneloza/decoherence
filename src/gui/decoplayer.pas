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

(* Handling camera and movement of player-controlled party *)

unit DecoPlayer;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoGlobal;

type
  DMoveKey = (KForward, KBackward, KStrafeLeft, KStrafeRight);

type
  { Handling of player movement }
  DPlayer = class(TObject)
  private
    MoveKeys: array[DMoveKey] of boolean;
  public
    //CurrentParty
    //CameraMman
    procedure MoveKeyPress(const aKey: DMoveKey);
    procedure MoveKeyRelease(const aKey: DMoveKey);
  public
    procedure Manage;
  end;

var
  Player: DPlayer;

procedure InitPlayer;
procedure FreePlayer;
implementation
uses
  SysUtils;

procedure DPlayer.MoveKeyPress(const aKey: DMoveKey);
begin
  MoveKeys[aKey] := true;
end;

{----------------------------------------------------------------------------}

procedure DPlayer.MoveKeyRelease(const aKey: DMoveKey);
begin
  MoveKeys[aKey] := false;
end;

{----------------------------------------------------------------------------}

procedure DPlayer.Manage;
begin

end;

{----------------------------------------------------------------------------}

procedure InitPlayer;
begin
  Player := DPlayer.Create;
end;

{----------------------------------------------------------------------------}

procedure FreePlayer;
begin
  FreeAndNil(Player);
end;

end.

