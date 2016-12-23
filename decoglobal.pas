{Copyright (C) 2012-2016 Yevhen Loza

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

uses
  Classes, CastleRandom, CastleWindowTouch;

{ for easy changing into double in case needed }
type Float = single;
     pFloat = ^float;

{ folders constants relative to ApplicationData path }
const InterfaceFolder     = 'interface/';
      FramesFolder       = InterfaceFolder+'frames/';
      LoadScreenFolder   = InterfaceFolder+'loadscreen/';
      ProgressBarFolder  = InterfaceFolder+'progressbar/';
      BackgroundsFolder  = InterfaceFolder+'background/';
      WindFolder         = InterfaceFolder+'wind/';
      DecorationsFolder  = InterfaceFolder+'decorations/';
      PortraitFolder     = InterfaceFolder+'portrait/';
      PerksFolder        = InterfaceFolder+'perks/';

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
uses SysUtils;

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


