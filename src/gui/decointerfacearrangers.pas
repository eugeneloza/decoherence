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

(* Meta-elements arranging children *)

unit DecoInterfaceArrangers;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoInterfaceCore,
  DecoTime;

type
  { calls ManageChildren in Update and resets their animation state }
  DAbstractArranger = class(DInterfaceElement)
  strict protected
    {}
    procedure ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime); virtual; abstract;
  public
    procedure SizeChanged(const Animate: TAnimationStyle; const Duration: DTime); override;
  end;

type
  { arranges children relative to its center without scaling them }
  DCenterArranger = class(DAbstractArranger)
  strict protected
    procedure ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime); override;
  end;

{............................................................................}
implementation
{uses
  DecoLog;}

procedure DAbstractArranger.SizeChanged(const Animate: TAnimationStyle; const Duration: DTime);
begin
  //inherited SizeChanged(Animate, Duration);
  ArrangeChildren(Animate, Duration);
end;

{======================  DCenterArranger =====================================}

procedure DCenterArranger.ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime);
var
  c: DSingleInterfaceElement;
begin
  //inherited ArrangeChildren(Animate, Duration); <------- parent is abstract
  Self.GetAnimationState;

  for c in Children do
  begin
    c.GetAnimationState;
    c.Next.x := Self.Current.x + (Self.Current.w - c.Next.w) div 2;
    c.Next.y := Self.Current.y + (Self.Current.h - c.Next.h) div 2;
    c.ResetAnimation;

    c.SetSize(Self.Next.x + (Self.Next.w - c.Next.w) div 2,
      Self.Next.y + (Self.Next.h - c.Next.h) div 2,
      c.Next.w,
      c.Next.h,
      c.Next.a, Animate, Duration);
  end;
end;

end.

