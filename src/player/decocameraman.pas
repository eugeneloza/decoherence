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

(* CameraMan is a "virtual" character, holding the camera during the
   player party travel through the game world or cutscenes *)

unit DecoCameraMan;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoGlobal, CastleVectors;

type
  {}
  DCameraMan = class(DObject)
  private
    { Sets Up to GravityUp }
    procedure ResetUp;
  public
    { CameraMan reqruies different handling of "up" vector }
    Up: TVector3;
    { Rotations of the CameraMan, required for smoother camera rotations }
    Theta, Phi: DFloat;
    { Resets Theta and Phi to match CameraMan.Direction
          Also zeroes mouse coordinate shift }
    procedure ResetAngles;
  public
    constructor Create; //override;
  end;

{.......................................................................}
implementation
uses
  Math,
  DecoMath;

constructor DCameraMan.Create;
begin
  //inherited Create;
  ResetUp;
  Theta := 0;
  phi := 0;
end;

{----------------------------------------------------------------------------}

procedure DCameraMan.ResetUp;
begin
  //Up := CurrentWorld.GetGravity(Position);
end;

{----------------------------------------------------------------------------}

procedure DCameraMan.ResetAngles;
begin
  Theta := 0;//ArcSin(Direction[2] / Direction.Length);
  Phi := 0;//Sign(Direction[1]) * ArcCos(Direction[0] / (sqr(Direction[0]) + sqr(Direction[1])));
  //CenterMouseCursor;
end;

end.

