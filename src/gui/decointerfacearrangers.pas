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
  { Calls ArrangeChildren in SizeChanged }
  DAbstractArranger = class abstract(DInterfaceElement)
  strict protected
    { Arranges Children according to the Arranger type }
    procedure ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime); virtual; abstract;
  public
    procedure SizeChanged(const Animate: TAnimationStyle; const Duration: DTime); override;
  end;

type
  { Alignment of the content within the Aligned Arranger }
  TVerticalAlign = (vaTop, vaCenter, vaBottom);
  THorizontalAlign = (haLeft, haCenter, haRight);

type
  { arranges children relative to its coordinates without scaling them
    default is: center alignment }
  DAlignedArranger = class(DAbstractArranger)
  strict protected
    procedure ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime); override;
  public
    { Alignment of the content }
    VAlign: TVerticalAlign;
    HAlign: THorizontalAlign;
  public
    constructor Create; override;
  end;

{............................................................................}
implementation
uses
  Profiler;//DecoLog;

procedure DAbstractArranger.SizeChanged(const Animate: TAnimationStyle; const Duration: DTime);
begin
  {StartProfiler}

  //inherited SizeChanged(Animate, Duration);
  Self.GetAnimationState;
  ArrangeChildren(Animate, Duration);

  {StopProfiler}
end;

{======================  DCenterArranger =====================================}

procedure DAlignedArranger.ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime);
var
  c: DSingleInterfaceElement;
  FromState, ToState: DInterfaceContainer;
begin
  {StartProfiler}

  //inherited ArrangeChildren(Animate, Duration); <------- parent is abstract

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

    c.FromToAnimate(FromState, ToState, Animate, Duration);
  end;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

constructor DAlignedArranger.Create;
begin
  inherited Create;
  VAlign := vaCenter;
  HAlign := haCenter;
end;

end.

