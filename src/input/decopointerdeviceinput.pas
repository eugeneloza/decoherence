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

(* Handles abstract pointer device behaviour
   abstract routines common for both mouse and touch *)

{$INCLUDE compilerconfig.inc}

unit DecoPointerDeviceInput;

interface

uses
  CastleKeysMouse,
  DecoGlobal;

type
  DPointerDeviceInput = class abstract(DObject)
  public
    { If mouse has been moved }
    procedure doMouseMotion(const Event: TInputMotion); virtual; abstract;
    { If mouse button / touch has been pressed }
    procedure doMousePress(const Event: TInputPressRelease); virtual; abstract;
    { If mouse button / touch has been released }
    procedure doMouseRelease(const Event: TInputPressRelease); virtual; abstract;
  end;

{............................................................................}
implementation

end.

