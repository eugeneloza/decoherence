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
    asFadeIn, asFadeOut,
    asZoomIn, asZoomOut);

type
  { Most abstract container for interface elements
    Defines size, scaling and animation state }
  DAbstractElement = class abstract(DObject)
  strict private
    AnimationStart: DTime;
    AnimationDuration: DTime;
    AnimationCurve: TAnimationCurve;
    AnimationSuicide: boolean;
    { Updates/caches Current container }
    procedure GetAnimationState; TryInline
    function GetAlpha: DFloat;
  strict protected
    { Parent interface element }
    Parent: DAbstractElement;
    { Container of this inhterface element determined based on
      Last and Next state of the animation}
    Current: DInterfaceContainer;
    { Last and Next state of the container during the animation
      in case the animation is over, Next container is used }
    Last, Next: DInterfaceContainer;
    { Init the animation of this element }
    procedure AnimateTo(const Animate: TAnimationStyle;
      const Duration: DTime = DefaultAnimationDuration);
    { Stops any animation, and sets Last = Next }
    procedure ResetAnimation;
  strict protected
    { Enables suicide of this element in case it has vanished }
    KillMePlease: Boolean;
    { updates the class each frame,
      (e.g. gets the current animation state) }
    procedure Update; virtual;
  public
    property Alpha: Single read GetAlpha;
    { Draw the element / as abstract as it might be :) }
    procedure Draw; virtual; abstract;
    { Set tint of the element }
    procedure SetTint; virtual;
    { Initialize this element with specific coordinates/size/alpha
      optionally animation may be specified }
    procedure SetSize(const ax, ay, aw, ah: integer; const aAlpha: DFloat = 1.0;
      const Animate: TAnimationStyle = asDefault; const Duration: DTime = DefaultAnimationDuration);
    { Scale this element to full screen (no animation) }
    procedure FullScreen(const aAlpha: Single = 1);
  public
    constructor Create; virtual; //override;
    destructor Destroy; override;
  end;

type
  { A simple procedure which reports Sender and clicked x,y }
  TXYProcedure = procedure(const Sender: DAbstractElement; const ax, ay: integer) of object;

type
  { Fully-featured Interface Element with Mouse/Touch support
    It lacks only "Children" or specific "Draw" to be used }
  DSingleInterfaceElement = class abstract(DAbstractElement)
  strict protected
    { A simple timer to fire some event on time-out }
    Timer: DTimer;
    procedure Update; override;
  public
    { Activate and initialize timer }
    procedure SetTimeOut(const Seconds: DTime);

    (* Mouse routines *)
  public
    { If this element is active (clickable) }
    CanMouseOver: boolean;
    { Can this element be dragged? }
    CanDrag: boolean;
    { Are these coordinates in this element's box? }
    function IAmHere(const xx, yy: integer): boolean;
    { Returns self if IAmHere and runs all possible events }
    function ifMouseOver(const xx, yy: integer; const RaiseEvents: boolean;
      const AllTree: boolean): DAbstractElement; virtual;
  strict private
    { If mouse is over this element }
    isMouseOver: boolean;
    DragX, DragY: integer;
  public
    procedure Drag(const xx, yy: integer);
    procedure StartDrag(const xx, yy: integer);
  public
    { Mouse/touch Events }
    OnMouseEnter: TXYProcedure;
    OnMouseLeave: TXYProcedure;
    OnMouseOver: TXYProcedure;
    OnMousePress: TXYProcedure;
    OnMouseRelease: TXYProcedure;
    { Dragg-n-drop routines }
    OnDrop: TXYProcedure;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;
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
    { Assign given element as a child }
    procedure Grab(const aChild: DSingleInterfaceElement);
    procedure Clear;

    (* Mouse routines *)
  public
    { Returns last (cached) call to MouseOverTree result, may be invalid! }
    isMouseOverTree: boolean;
    { Returns Self if IAmHere and runs all possible events + scans all children }
    function ifMouseOver(const xx, yy: integer; const RaiseEvents: boolean;
      const AllTree: boolean): DAbstractElement; override;
    { Returns true if mouse is over any "canmouseover" child of this element }
    function MouseOverTree(const xx, yy: integer): boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

{............................................................................}
implementation
uses
  SysUtils,
  DecoGUIScale, DecoLog;

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
  AnimationSuicide := false;
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

  if not Next.isInitialized then
    Log(LogInterfaceError, CurrentRoutine, 'Warning: NEXT is not initialized!');

  if (DecoNow - AnimationStart < AnimationDuration) and (Last.isInitialized) then
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
    {if this was a suicide animation then kill this element}
    if AnimationSuicide then
      Self.KillMePlease := true;
  end;
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.SetTint;
begin
  //just does nothing
end;

{-----------------------------------------------------------------------------}

function DAbstractElement.GetAlpha: DFloat;
begin
  if Parent <> nil then
    Result := Current.a * Parent.Alpha
  else
    Result := Current.a;
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.AnimateTo(const Animate: TAnimationStyle;
  const Duration: DTime = DefaultAnimationDuration);
begin
  { Next must be set before the AnimateTo! }
  GetAnimationState;
  Last.AssignFrom(Current);   //to current animation state

  AnimationStart := -1;
  AnimationDuration := Duration;
  AnimationSuicide := false;

  case Animate of
    asNone: AnimationDuration := -1; //no animation will just assign Current = Next on next frame
    //asDefault: <------ will simply animate from "Last = Current (= Next if uninitialized)" to "Next"
    {fades in/out element}
    asFadeIn: Last.a := 0;
    asFadeOut: Next.a := 0;
    {zooms in/out element}
    asZoomIn: Last.SetIntWidthHeight(1, 1, 0);
    asZoomOut: Next.SetIntWidthHeight(1, 1 , 0);
  end;
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.ResetAnimation;
begin
  Last.AssignFrom(Next);
  AnimationDuration := -1;
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.Update;
begin
  GetAnimationState;
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.SetSize(const ax, ay, aw, ah: integer; const aAlpha: DFloat = 1.0;
  const Animate: TAnimationStyle = asDefault; const Duration: DTime = DefaultAnimationDuration);
begin
  Next.SetIntSize(ax, ay, aw, ah, aAlpha);
  if not Last.isInitialized then Last.AssignFrom(Next);
  AnimateTo(Animate, Duration);
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.FullScreen(const aAlpha: Single = 1);
begin
  Next.SetIntSize(0, 0, GUIWidth, GUIHeight, aAlpha);
  ResetAnimation;
end;

{============================================================================}
{===================== D SINGLE INTERFACE ELEMENT ===========================}
{============================================================================}

constructor DSingleInterfaceElement.Create;
begin
  inherited Create;
  isMouseOver := false;
  CanMouseOver := false;
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

{==============================  Mouse handling =============================}

function DSingleInterfaceElement.IAmHere(const xx, yy: integer): boolean; TryInline
const
  CutTransparency = 0.3;
begin
  if (xx >= Next.x) and (xx <= Next.x2) and
    (yy >= Next.y) and (yy <= Next.y2) and
    (Self.Alpha > CutTransparency) then
    Result := true
  else
    Result := false;
end;

{-----------------------------------------------------------------------------}

{$PUSH}
{$WARN 5024 off} //AllTree is not used here and it's ok. It's used in DInterfaceElement later
function DSingleInterfaceElement.ifMouseOver(const xx, yy: integer;
  const RaiseEvents: boolean; const AllTree: boolean): DAbstractElement;
begin
  Result := nil;
  if IAmHere(xx, yy) then
  begin
    if RaiseEvents then
    begin
      if isMouseOver = false then
      begin
        if Assigned(onMouseEnter) then
          onMouseEnter(Self, xx, yy);
        isMouseOver := true;
      end;
      if Assigned(onMouseOver) then
        onMouseOver(Self, xx, yy);
    end;
    if CanMouseOver then  //todo
      Result := Self;
  end
  else
  begin
    if isMouseOver and RaiseEvents then
    begin
      if Assigned(onMouseLeave) then
        onMouseLeave(Self, xx, yy);
      isMouseOver := false;
    end;
  end;
end;
{$POP}

{-----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.StartDrag(const xx, yy: integer);
begin
  ResetAnimation;
  DragX := Next.x - xx;
  DragY := Next.y - yy;
end;

{-----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.Drag(const xx, yy: integer);
begin
  Next.x := DragX + xx;
  Next.y := DragY + yy;
  //this is ugly!
  ResetAnimation;
end;

{============================================================================}
{======================= D INTERFACE ELEMENT ================================}
{============================================================================}

constructor DInterfaceElement.Create;
begin
  inherited Create;
  Children := DInterfaceElementsList.Create(true);
end;

{----------------------------------------------------------------------------}

destructor DInterfaceElement.Destroy;
begin
  //this should fire as recoursive because children owns elements, which in turn will fire their destructors onfree
  Children.Free;
  inherited Destroy;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Clear;
begin
  Children.Clear;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Update;
var
  i: integer;
begin
  { does not call update on children as they will call their own update on draw }
  if Children.Count > 0 then
  begin
    i := 0;
    repeat
      if Children[i].KillMePlease then
        Children.Delete(i)
      else
        inc(i);
    until i >= Children.Count;
  end;

  inherited Update;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.SetTint;
var
  c: DSingleInterfaceElement;
begin
  //inherited SetTint; <---------- parent is an "empty" virtual procedure
  for c in Children do
    c.SetTint;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Draw;
var
  c: DSingleInterfaceElement;
begin
  //inherited Draw; <---------- parent is abstract
  Update;
  for c in Children do
    c.Draw;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Grab(const aChild: DSingleInterfaceElement);
begin
  Children.Add(aChild);
  aChild.Parent := Self;
end;

{----------------------------------------------------------------------------}

function DInterfaceElement.ifMouseOver(const xx, yy: integer;
  const RaiseEvents: boolean; const AllTree: boolean): DAbstractElement;
var
  i: integer;
  tmpLink: DAbstractElement;
begin
  Result := inherited ifMouseOver(xx, yy, RaiseEvents, AllTree);
  //if rsult<>nil ... *or drag-n-drop should get the lowest child?

  // recoursively scan all children
  for i := 0 to Pred(Children.Count) do
  begin
    tmpLink := Children[i].ifMouseOver(xx, yy, RaiseEvents, AllTree);
    if tmpLink <> nil then
    begin
      Result := tmpLink;
      if not AllTree then
        Break; // if drag-n-drop one is enough
    end;
  end;
end;

{-----------------------------------------------------------------------------}

function DInterfaceElement.MouseOverTree(const xx, yy: integer): boolean;
var
  tmp: DAbstractElement;
begin
  // maybe rewrite it using isMouseOver - the idea is still a little different
  tmp := Self.ifMouseOver(xx, yy, false, false);
  if (tmp <> nil) and (tmp is DSingleInterfaceElement) and
    (DSingleInterfaceElement(tmp).CanMouseOver) then
    isMouseOverTree := true
  else
    isMouseOverTree := false;

  Result := isMouseOverTree;
end;

end.

