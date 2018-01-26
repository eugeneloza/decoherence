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

(* Container of interface elements
   Responsible for scale and animation of the interface elements *)

unit DecoInterfaceContainer;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoGlobal;

type
  { Determines coordinates and size of a interface element }
  DInterfaceContainer = class(DObject)
  public
    x, y, w, h: integer;
    a: DFloat;
    { Copy current container's xywha to aDest }
    procedure AssignTo(const aDest: DInterfaceContainer); TryInline
    { Copy current container's xywha from aSource }
    procedure AssignFrom(const aSource: DInterfaceContainer); TryInline
    { Mix this container's xywha from aLast and aNext with aPhase as a weight }
    procedure AssignMix(const aLast, aNext: DInterfaceContainer; const aPhase: DFloat); TryInline

    { Initialize current container with float coordinates }
    procedure SetFloatSize(const ax, ay, aw, ah: DFloat; const aAlpha: DFloat = 1);
    procedure SetFloatCoord(const ax1, ay1, ax2, ay2: DFloat; const aAlpha: DFloat = 1);
  end;

{............................................................................}
implementation
uses
  DecoGUIScale,
  DecoLog;

procedure DInterfaceContainer.AssignTo(const aDest: DInterfaceContainer); TryInline
begin
  aDest.x := Self.x;
  aDest.y := Self.y;
  aDest.w := Self.w;
  aDest.h := Self.h;
  aDest.a := Self.a;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.AssignFrom(const aSource: DInterfaceContainer); TryInline
begin
  Self.x := aSource.x;
  Self.y := aSource.y;
  Self.w := aSource.w;
  Self.h := aSource.h;
  Self.a := aSource.a;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.AssignMix(const aLast, aNext: DInterfaceContainer; const aPhase: DFloat); TryInline
begin
  Self.x := Round(aLast.x + (aNext.x - aLast.x) * aPhase);
  Self.y := Round(aLast.y + (aNext.y - aLast.y) * aPhase);
  Self.w := Round(aLast.w + (aNext.w - aLast.w) * aPhase);
  Self.h := Round(aLast.h + (aNext.h - aLast.h) * aPhase);
  Self.a :=      (aLast.a + (aNext.a - aLast.a) * aPhase);
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.SetFloatSize(const ax, ay, aw, ah: DFloat; const aAlpha: DFloat = 1);
begin
  x := Round(ax * GUIHeight);
  y := Round(ay * GUIHeight);
  w := Round(aw * GUIHeight);
  h := Round(ah * GUIHeight);
  if ax < 0 then
    x := x + GUIWidth - w;
  a := aAlpha;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.SetFloatCoord(const ax1, ay1, ax2, ay2: DFloat; const aAlpha: DFloat = 1);
begin
  SetFloatSize(ax1, ay1, ax2 - ax1, ay2 - ay1, aAlpha);
end;


end.

