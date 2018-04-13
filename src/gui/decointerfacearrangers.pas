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

{$INCLUDE compilerconfig.inc}

unit DecoInterfaceArrangers;

interface

uses
  DecoInterfaceCore, DecoInterfaceContainer,
  DecoTime;

type
  {}
  TVerticalAlign = (vaTop, vaCenter, vaBottom);
  THorizontalAlign = (haLeft, haCenter, haRight);

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
  { arranges children relative to its coordinates without scaling them
    default is: center alignment }
  DAlignedArranger = class(DAbstractArranger)
  strict protected
    procedure ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime); override;
  public
    {}
    VAlign: TVerticalAlign;
    HAlign: THorizontalAlign;
  public
    constructor Create; override;
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

procedure DAlignedArranger.ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime);
var
  c: DSingleInterfaceElement;
  FromState, ToState: DInterfaceContainer;
begin
  //inherited ArrangeChildren(Animate, Duration); <------- parent is abstract
  Self.GetAnimationState;

  FromState.AssignFrom(Self.Current);

  for c in Children do
  begin

    ToState.AssignFrom(c.Next);
    case HAlign of
      haCenter: ToState.x := Self.Next.x + (Self.Next.w - c.Next.w) div 2;
      haLeft: ToState.x := Self.Next.x;
      haRight: ToState.x := Self.Next.x2 - c.Next.w;
    end;
    case VAlign of
      vaCenter: ToState.y := Self.Next.y + (Self.Next.h - c.Next.h) div 2;
      vaBottom: ToState.y := Self.Next.y;
      vaTop: ToState.y := Self.Next.y2 - c.Next.h;
    end;

    c.ForceSize(FromState);
    c.SetSize(ToState, Animate, Duration);
  end;
end;

{-----------------------------------------------------------------------------}

constructor DAlignedArranger.Create;
begin
  inherited Create;
  VAlign := vaCenter;
  HAlign := haCenter;
end;

end.

