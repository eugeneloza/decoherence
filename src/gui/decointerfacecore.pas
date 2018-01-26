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

(* Contains most of interface basics and grouping *)

unit DecoInterfaceCore;

{$INCLUDE compilerconfig.inc}

interface

uses
  Generics.Collections,
  DecoInterfaceTimer, DecoInterfaceContainer,
  DecoGlobal, DecoTime;

const
  { Default duration of all animations (in seconds),
    unless otherwise specified }
  DefaultAnimationDuration = 0.3;

type
  { Style of the animation.
    asLinear is just linear interpolation,
    asSquare is slower in the beginning and end, and faster in the middle}
  TAnimationCurve = (acLinear, acSquare);

type
  { Animation style of the object. asDefault means "animate from previous state",
    presuming that there was some "previous state"}
  TAnimationStyle = (asNone, asDefault,
    asFadeIn, asFadeOut,// asFadeOutSuicide,
    asZoomIn, asZoomOut,// asZoomOutSuicide,
    asFlyInRandom, asFlyOutRandom,// asFlyOutRandomSuicide,
    asFlyInTop, asFlyOutTop,
    asFlyInBottom, asFlyOutBottom,
    asFlyInLeft, asFlyOutLeft,
    asFlyInRight, asFlyOutRight);

type
  { Most abstract container for interface elements
    Defines size, scaling and animation state }
  DAbstractElement = class abstract(DObject)
  strict private
    AnimationStart: DTime;
    AnimationDuration: DTime;
    AnimationCurve: TAnimationCurve;
    { Location and size of this element / not sure about visibility level }
    Last, Next, Current: DInterfaceContainer;
    procedure GetAnimationState; TryInline
  strict protected
    KillMePlease: Boolean;
    { updates the data of the class with current external data,
      here it just gets the current animation state }
    procedure Update; virtual;
  public
    { Draw the element / as abstract as it might be :) }
    procedure Draw; virtual; abstract;
    { Set tint of the element }
    procedure SetTint; virtual; abstract;

    procedure SetSize(const ax, ay, aw, ah: integer; const aAlpha: DFloat = 1.0; const Animate: TAnimationStyle = asNone);
  public
    //...
  public
    constructor Create; virtual; //override;
    destructor Destroy; override;
  end;

type
  { Fully-featured Interface Element with Mouse/Touch support
    It lacks only "Children" or specific "Draw" to be used }
  DSingleInterfaceElement = class abstract(DAbstractElement)
  strict protected
    //...
    { A simple timer to fire some event on time-out }
    Timer: DTimer;
    procedure Update; override;
  public
    { Activate and initialize timer }
    procedure SetTimeOut(const Seconds: DTime);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

type
  { List of DSingleInterfaceElement instances }
  DInterfaceElementsList = specialize TObjectList<DSingleInterfaceElement>;
type
  { An interface element, that can contain "Children" }
  DInterfaceElement = class(DSingleInterfaceElement)
  strict protected
    { List of the children of this interface element }
    Children: DInterfaceElementsList;
    procedure Update; override;
  public
    procedure SetTint; override;
    procedure Draw; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

{............................................................................}
implementation
uses
  SysUtils,
  DecoLog;

{============================================================================}
{======================== D ABSTRACT ELEMENT ================================}
{============================================================================}

constructor DAbstractElement.Create;
begin
  //inherited <------- nothing to inherit
  KillMePlease := false;
  Last := DInterfaceContainer.Create;
  Next := DInterfaceContainer.Create;
  Current := DInterfaceContainer.Create;
  AnimationCurve := acSquare;
end;

{-----------------------------------------------------------------------------}

destructor DAbstractElement.Destroy;
begin
  Current.Free;
  Next.Free;
  Last.Free;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.GetAnimationState; TryInline
var
  Phase: DFloat;
begin
  //if this is start of the animation - init time
  if AnimationStart < 0 then
    AnimationStart := DecoNow;

  if (DecoNow - AnimationStart < AnimationDuration) then
  begin
    //determine the animation time passed relative to AnimationDuration
    Phase := (DecoNow - AnimationStart) / AnimationDuration;
    //determine the animation phase
    case AnimationCurve of
      //acLinear: ; //<- change nothing.
      acSquare: if Phase < 0.5 then
          Phase := Sqr(2 * Phase) / 2
        else
          Phase := 1 - Sqr(2 * (1 - Phase)) / 2;
    end;
    Current.AssignMix(Last, Next, Phase);
  end
  else
  begin
    Current.AssignFrom(Next);
  end;
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.Update;
begin
  GetAnimationState;
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.SetSize(const ax, ay, aw, ah: integer; const aAlpha: DFloat = 1.0; const Animate: TAnimationStyle = asNone);
begin
  Next.SetIntSize(ax, ay, aw, ah, aAlpha);
  //AnimateTo(Next);
end;


{============================================================================}
{===================== D SINGLE INTERFACE ELEMENT ===========================}
{============================================================================}

constructor DSingleInterfaceElement.Create;
begin
  inherited Create;
  //...
end;

{-----------------------------------------------------------------------------}

destructor DSingleInterfaceElement.Destroy;
begin
  FreeAndNil(Timer);
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.Update;
begin
  inherited Update;
  if (Timer <> nil) and (Timer.Enabled) then
    Timer.Update;
end;

{-----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.SetTimeOut(const Seconds: DTime);
begin
  if Timer = nil then
    Timer := DTimer.Create;
  Timer.SetTimeOut(Seconds);
end;

{============================================================================}
{======================= D INTERFACE ELEMENT ================================}
{============================================================================}

constructor DInterfaceElement.Create;
begin
  inherited Create;
  Children := DInterfaceElementsList.Create(True);
end;

{----------------------------------------------------------------------------}

destructor DInterfaceElement.Destroy;
begin
  //this should fire as recoursive because children owns elements, which in turn will fire their destructors onfree
  Children.Free;
  inherited Destroy;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Update;
var
  i: integer;
  c: DSingleInterfaceElement;
begin
  i := 0;
  repeat
    if Children[i].KillMePlease then
    begin
      c := Children[i];
      Children.Remove(c);
      c.Free;
    end
    else
      Inc(i);
  until i = Children.Count;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.SetTint;
var
  i: integer;
begin
  //inherited SetTint; <---------- parent is abstract
  for i := 0 to Children.Count - 1 do
    Children[i].SetTint;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Draw;
var
  i: integer;
begin
  //inherited Draw; <---------- parent is abstract
  {if isVisible then
  begin
    Update; }
    for i := 0 to Children.Count - 1 do
      Children[i].Draw;
  {end;}
end;

end.

