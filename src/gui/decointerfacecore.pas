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

{$INCLUDE compilerconfig.inc}

unit DecoInterfaceCore;

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
    asSquare is faster in the beginning and slower in the end}
  TAnimationCurve = (acLinear, acSquare);

type
  { Animation style of the object. asDefault means "animate from previous state",
    presuming that there was some "previous state"}
  TAnimationStyle = (asNone, asDefault,
    asFadeIn, asFadeOut,
    asZoomIn, asZoomOut,
    asFlyInRadial, asFlyOutRadial,
    asFlyInRandom, asFlyOutRandom);

type
  { Most abstract container for interface elements
    Defines size, scaling and animation state }
  DAbstractElement = class abstract(DObject)
  strict private
    AnimationStart: DTime;
    AnimationDuration: DTime;
    AnimationCurve: TAnimationCurve;
    CurrentAnimation: TAnimationStyle;
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
    { If this element is full-screen? }
    isFullScreen: boolean;
    { Enables suicide of this element in case it has vanished }
    KillMePlease: Boolean;
    { updates the class each frame,
      (e.g. gets the current animation state) }
    procedure Update; virtual;
    { Updates/caches Current container }
    procedure GetAnimationState; TryInline
    { If CurrentAnimation is a suicide animation }
    function AnimationSuicide: boolean;
    { If CurrentAnimatino is suitable for full-screen elements? }
    function AnimationFullScreen: boolean;
  public
    { Transparency of current element, stacking all parents' transparation }
    property Alpha: Single read GetAlpha;
    { Draw the element / as abstract as it might be :) }
    procedure Draw; virtual; abstract;
    { Set tint of the element }
    procedure SetTint; virtual;
    { Initialize this element with specific coordinates/size/alpha
      optionally animation may be specified }
    procedure SetSize(const ax, ay, aw, ah: integer; const aAlpha: DFloat = 1.0;
      const Animate: TAnimationStyle = asDefault; const Duration: DTime = DefaultAnimationDuration);
    procedure SetSize(const aContainer: DInterfaceContainer;
      const Animate: TAnimationStyle = asDefault; const Duration: DTime = DefaultAnimationDuration);
    { Forces the element size to a specific value and resets the animation
      Careful, it doesn't trigger SetSize events sequence,
      e.g. doesn't call SizeChanged }
    procedure ForceSize(const ax, ay, aw, ah: integer; const aAlpha: DFloat = 1.0);
    procedure ForceSize(const aContainer: DInterfaceContainer);
    {}
    procedure FromToAnimate(const FromContainer, ToContainer: DInterfaceContainer;
      const Animate: TAnimationStyle = asDefault; const Duration: DTime = DefaultAnimationDuration);
    { Scale this element to full screen (no animation) }
    procedure FullScreen(const aAlpha: Single = 1);
    { This procedure alerts Parent that this element has changed its size }
    procedure SizeChanged(const Animate: TAnimationStyle; const Duration: DTime); virtual;
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
    { To support for Drag-n-drop return item to its origin in case it
      cannot be dropped here }
    SavedContainerState: DInterfaceContainer;
  public
    { Dragg-n-drop routines }
    procedure Drag(const xx, yy: integer);
    procedure StartDrag(const xx, yy: integer);
    procedure Drop;
  public
    { Mouse/touch Events }
    OnMouseEnter: TXYProcedure;
    OnMouseLeave: TXYProcedure;
    OnMouseOver: TXYProcedure;
    OnMouseLeftButton: TXYProcedure;
    OnMouseRightButton: TXYProcedure;
    OnMouseRelease: TXYProcedure;
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
    procedure Clear(const Animate: TAnimationStyle = asNone;
      const Duration: DTime = DefaultAnimationDuration);

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
  DecoGUIScale, DecoLog, Profiler;

{============================================================================}
{======================== D ABSTRACT ELEMENT ================================}
{============================================================================}

constructor DAbstractElement.Create;
begin
  //inherited <------- nothing to inherit
  KillMePlease := false;
  Last.Create;
  Next.Create;
  Current.Create;
  AnimationCurve := acSquare;
  CurrentAnimation := asNone;
  isFullScreen := false;
end;

{-----------------------------------------------------------------------------}

destructor DAbstractElement.Destroy;
begin
  //...
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

function DAbstractElement.AnimationSuicide: boolean;
begin
  if (CurrentAnimation in [asFadeOut, asZoomOut, asFlyOutRadial, asFlyOutRandom]) then
    Result := true
  else
    Result := false;
end;

{-----------------------------------------------------------------------------}

function DAbstractElement.AnimationFullScreen: boolean;
begin
  if (CurrentAnimation in [asNone, asDefault, asFadeIn, asFadeOut]) then
    Result := true
  else
    Result := false;
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.GetAnimationState; TryInline
var
  Phase: DFloat;
begin
  {StartProfiler}

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
      acSquare: Phase := Sqrt(Phase);
    end;
    Current.AssignMix(Last, Next, Phase);
  end else
  begin
    Current.AssignFrom(Next);
    {if this was a suicide animation then kill this element}
    if AnimationSuicide then
      Self.KillMePlease := true;
  end;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.SizeChanged(const Animate: TAnimationStyle; const Duration: DTime);
begin
  //...
  {if Parent <> nil then
    Parent.SizeChanged(Animate, Duration);}
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.SetTint;
begin
  //just does nothing
end;

{-----------------------------------------------------------------------------}

function DAbstractElement.GetAlpha: DFloat;
begin
  {StartProfiler}

  if Parent <> nil then
    Result := Current.a * Parent.Alpha
  else
    Result := Current.a;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.AnimateTo(const Animate: TAnimationStyle;
  const Duration: DTime = DefaultAnimationDuration);
var
  mx, my: integer;
  dx, dy, ddx, ddy: DFloat;
begin
  {StartProfiler}

  { Next must be set before the AnimateTo! }
  GetAnimationState;
  Last.AssignFrom(Current);   //to current animation state

  AnimationStart := -1;
  AnimationDuration := Duration;
  CurrentAnimation := Animate;

  if isFullScreen and not AnimationFullScreen then
  begin
    if AnimationSuicide then
      CurrentAnimation := asFadeOut
    else
      CurrentAnimation := asFadeIn;
  end;

  case Animate of
    asNone: AnimationDuration := -1; //no animation will just assign Current = Next on next frame
    //asDefault: <------ will simply animate from "Last = Current (= Next if uninitialized)" to "Next"
    {fades in/out element}
    asFadeIn: Last.a := 0;
    asFadeOut: Next.a := 0;
    {zooms in/out element}
    asZoomIn: Last.SetIntWidthHeight(1, 1, 0);
    asZoomOut: Next.SetIntWidthHeight(1, 1, 0);
    {fly in/out}
    asFlyInRadial,asFlyOutRadial,asFlyInRandom,asFlyOutRandom:
      begin
        if Animate in [asFlyInRadial, asFlyOutRadial] then
        begin
          dx := Current.x - GUICenter[0];
          dy := Current.y - GUICenter[1];
          ddy := dy / GUIHeight;
          ddx := dx / GUIWidth;
          if abs(ddy) > abs(ddx) then
          begin
            if dy < 0 then
              my := 0
            else
              my := GUIHeight;
            mx := Round(GUIWidth * (1 + ddx / abs(ddy)) / 2);
          end else
          begin
            if dx < 0 then
              mx := 0
            else
              mx := GUIWidth;
            my := Round(GUIHeight * (1 + ddy / abs(ddx)) / 2);
          end;
        end else
          case DRND.Random(4) of
            0: begin
                 mx := 0;
                 my := DRND.Random(GUIHeight);
               end;
            1: begin
                 mx := GUIWidth;
                 my := DRND.Random(GUIHeight);
               end;
            2: begin
                 mx := DRND.Random(GUIWidth);
                 my := 0;
               end;
            3: begin
                 mx := DRND.Random(GUIWidth);
                 my := GUIHeight;
               end;
          end;
        case Animate of
          asFlyInRadial, asFlyInRandom: Last.SetIntSize(mx, my, 1, 1, 0);
          asFlyOutRadial, asFlyOutRandom: Next.SetIntSize(mx, my, 1, 1, 0);
        end;
      end;
  end;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.ResetAnimation;
begin
  {StartProfiler}

  Last.AssignFrom(Next);
  AnimationDuration := -1;
  GetAnimationState;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.Update;
begin
  {StartProfiler}

  GetAnimationState;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.SetSize(const ax, ay, aw, ah: integer; const aAlpha: DFloat = 1.0;
  const Animate: TAnimationStyle = asDefault; const Duration: DTime = DefaultAnimationDuration);
begin
  {StartProfiler}

  Next.SetIntSize(ax, ay, aw, ah, aAlpha);
  if not Last.isInitialized then Last.AssignFrom(Next);
  AnimateTo(Animate, Duration);
  SizeChanged(Animate, Duration);

  {StopProfiler}
end;

procedure DAbstractElement.SetSize(const aContainer: DInterfaceContainer;
  const Animate: TAnimationStyle = asDefault; const Duration: DTime = DefaultAnimationDuration);
begin
  {StartProfiler}

  SetSize(aContainer.x, aContainer.y, aContainer.w, aContainer.h, aContainer.a,
    Animate, Duration);

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.ForceSize(const ax, ay, aw, ah: integer; const aAlpha: DFloat = 1.0);
begin
  {StartProfiler}

  Next.SetIntSize(ax, ay, aw, ah, aAlpha);
  ResetAnimation;

  {StopProfiler}
end;

procedure DAbstractElement.ForceSize(const aContainer: DInterfaceContainer);
begin
  {StartProfiler}

  Next.AssignFrom(aContainer);
  ResetAnimation;

  {StopProfiler}
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.FromToAnimate(const FromContainer, ToContainer: DInterfaceContainer;
  const Animate: TAnimationStyle = asDefault; const Duration: DTime = DefaultAnimationDuration);
begin
  {StartProfiler}

  ForceSize(FromContainer);
  SetSize(ToContainer, Animate, Duration);

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DAbstractElement.FullScreen(const aAlpha: Single = 1);
begin
  {StartProfiler}

  Next.SetIntSize(0, 0, GUIWidth, GUIHeight, aAlpha);
  ResetAnimation;
  isFullScreen := true;
  //SizeChanged <----- we don't call it, as it's very unlikely that a FullScreen element is a Child of some Arranger

  {StopProfiler}
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
  {StartProfiler}

  inherited Update;
  if (Timer <> nil) and (Timer.Enabled) then
    Timer.Update;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.SetTimeOut(const Seconds: DTime);
begin
  {StartProfiler}

  if Timer = nil then
    Timer := DTimer.Create;
  Timer.SetTimeOut(Seconds);

  {StopProfiler}
end;

{==============================  Mouse handling =============================}

function DSingleInterfaceElement.IAmHere(const xx, yy: integer): boolean; TryInline
const
  CutTransparency = 0.3;
begin
  {StartProfiler}

  if (xx >= Next.x) and (xx <= Next.x2) and
    (yy >= Next.y) and (yy <= Next.y2) and
    (Self.Alpha > CutTransparency) then
    Result := true
  else
    Result := false;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

{$PUSH}
{$WARN 5024 off} //AllTree is not used here and it's ok. It's used in DInterfaceElement later
function DSingleInterfaceElement.ifMouseOver(const xx, yy: integer;
  const RaiseEvents: boolean; const AllTree: boolean): DAbstractElement;
begin
  {StartProfiler}

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
    if (CanMouseOver) or (CanDrag) then
      Result := Self;
  end else
  begin
    if isMouseOver and RaiseEvents then
    begin
      if Assigned(onMouseLeave) then
        onMouseLeave(Self, xx, yy);
      isMouseOver := false;
    end;
  end;

  {StopProfiler}
end;
{$POP}

{-----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.StartDrag(const xx, yy: integer);
begin
  {StartProfiler}

  ResetAnimation;
  SavedContainerState.AssignFrom(Next);
  DragX := Next.x - xx;
  DragY := Next.y - yy;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.Drag(const xx, yy: integer);
begin
  {StartProfiler}

  Next.x := DragX + xx;
  Next.y := DragY + yy;
  //this is ugly!
  ResetAnimation;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.Drop;
begin
  {StartProfiler}

  //if CanDropHere
  AnimateTo(asDefault);
  Next.AssignFrom(SavedContainerState);
  AnimateTo(asDefault);

  {StopProfiler}
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

procedure DInterfaceElement.Clear(const Animate: TAnimationStyle = asNone;
      const Duration: DTime = DefaultAnimationDuration);
var
  c: DSingleInterfaceElement;
begin
  {StartProfiler}

  if Animate = asNone then
    Children.Clear
  else
    for c in Children do
    begin
      c.AnimateTo(Animate, Duration);
      if c is DInterfaceElement then
        DInterfaceElement(c).Clear(Animate, Duration);
    end;

  {StopProfiler}
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Update;
var
  i: integer;
begin
  {StartProfiler}

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

  {StopProfiler}
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.SetTint;
var
  c: DSingleInterfaceElement;
begin
  {StartProfiler}

  //inherited SetTint; <---------- parent is an "empty" virtual procedure
  for c in Children do
    c.SetTint;

  {StopProfiler}
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Draw;
var
  c: DSingleInterfaceElement;
begin
  {StartProfiler}

  //inherited Draw; <---------- parent is abstract
  Update;
  for c in Children do
    c.Draw;

  {StopProfiler}
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Grab(const aChild: DSingleInterfaceElement);
begin
  {StartProfiler}

  Children.Add(aChild);
  aChild.Parent := Self;

  {StopProfiler}
end;

{----------------------------------------------------------------------------}

function DInterfaceElement.ifMouseOver(const xx, yy: integer;
  const RaiseEvents: boolean; const AllTree: boolean): DAbstractElement;
var
  i: integer;
  tmpLink: DAbstractElement;
begin
  {StartProfiler}

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

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

function DInterfaceElement.MouseOverTree(const xx, yy: integer): boolean;
var
  tmp: DAbstractElement;
begin
  {StartProfiler}

  // maybe rewrite it using isMouseOver - the idea is still a little different
  tmp := Self.ifMouseOver(xx, yy, false, false);
  if (tmp <> nil) and (tmp is DSingleInterfaceElement) and
    ((DSingleInterfaceElement(tmp).CanMouseOver) or (DSingleInterfaceElement(tmp).CanDrag)) then
    isMouseOverTree := true
  else
    isMouseOverTree := false;

  Result := isMouseOverTree;

  {StopProfiler}
end;

end.

