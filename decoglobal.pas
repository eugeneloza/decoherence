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

{ Defines some generic variables }

unit decoglobal;

{$INCLUDE compilerconfig.inc}

interface

uses {todo: temporary}SysUtils,
  Classes, CastleWindowTouch,
  CastleRandom, CastleTimeUtils;

{ for easy changing into double in case needed }
type Float = single;
     pFloat = ^float;
     DTime = TFloatTime;

{ folders constants relative to ApplicationData path
  should be "/", not pathdelim, because those are URLs }
const InterfaceFolder    = 'interface/';
      FramesFolder       = InterfaceFolder+'frames/';
      LoadScreenFolder   = InterfaceFolder+'loadscreen/';
      ProgressBarFolder  = InterfaceFolder+'progressbar/';
      BackgroundsFolder  = InterfaceFolder+'background/';
      WindFolder         = InterfaceFolder+'wind/';
      DecorationsFolder  = InterfaceFolder+'decorations/';
      PortraitFolder     = InterfaceFolder+'portrait/';
      PerksFolder        = InterfaceFolder+'perks/';
      DamageFolder       = InterfaceFolder+'damage/';

      //TODO: Android incompatible!!!
      Models_folder       = 'data'+pathdelim+'models'+pathdelim;
      Tiles_folder        = Models_folder + 'tiles'+pathdelim;
      Placeholders_folder = Models_folder + 'placeholders'+pathdelim;

const anisotropic_smoothing = 4;
      Shadow_maps_enabled = false;
      Shadow_volumes_enabled = false;

var Window : TCastleWindowTouch;
    { random generator used for all interface random events }
    rnd: TCastleRandom;
    {$IFNDEF Android}
    LogStream : TFileStream;
    {$ENDIF}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
procedure InitGlobal;
procedure DestroyGlobal;
implementation
//uses SysUtils;

// no implementation here needed. Maybe merge with GameMode?
procedure InitGlobal;
begin
  rnd := TCastleRandom.Create;
end;

{----------------------------------------------------------------------------}

procedure DestroyGlobal;
begin
  freeandnil(rnd);
end;

end.


