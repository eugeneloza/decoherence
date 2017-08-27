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

{ Contains most of interface basics and grouping }
unit DecoInterface;

{$INCLUDE compilerconfig.inc}

interface

uses
  fgl,
  DecoGlobal, DecoTime, DecoThread;

const DefaultAnimationDuration = 0.3; {in seconds}

{animation style of the object. asDefault means "animate from previous state",
 presuming that there was some "previous state"}
Type TAnimationStyle = (asNone, asDefault,
                        asFadeIn, asFadeOut,// asFadeOutSuicide,
                        asZoomIn, asZoomOut,// asZoomOutSuicide,
                        asFlyInRandom, asFlyOutRandom,// asFlyOutRandomSuicide,
                        asFlyInTop,asFlyOutTop,
                        asFlyInBottom,asFlyOutBottom,
                        asFlyInLeft,asFlyOutLeft,
                        asFlyInRight,asFlyOutRight);

type TAnchorSide = (asLeft,asRight,asTop,asBottom);
type TAnchorAlign = (noAlign, haLeft, haRight, haCenter, vaTop, vaBottom, vaMiddle);
type TProportionalScale = (psNone, psWidth, psHeight);

type
  { This is an container with coordinates and size
    Capable of rescaling / copying itself
    As abstract as it seems, it's also used as animation state,
    So, constructing it as standalone should be possible }
  DAbstractContainer = class(TObject)
  strict private
    fInitialized: boolean;
    { Parent container size (cached) }
    cx1,cx2,cy1,cy2,cw,ch: integer;
    { Determine and cache parent container size }
    procedure GetAnchors;
    { Converts integer size to float. }
    procedure IntegerToFloat;
  public
    { Converts float size to integer.
      should be public only for animation states. }
    procedure FloatToInteger;
  public
    type
      DAnchor = record
        { Anchor to which element }
        Anchor: DAbstractContainer;
        { To which side of the Anchor should it align? }
        AlignTo: TAnchorAlign;
        { Additional gap between this element and Anchor in px }
        Gap: integer;
      end;
  public
    { Anchors of this container }
    Anchor: array[TAnchorSide] of DAnchor;
    { Float size of the Container }
    fx1,fy1,fx2,fy2{,fw,fh}: float;
    { Real size of the Container }
    x1,y1,x2,y2,w,h: integer;
    { Opacity of the container }
    Opacity: float;
    { Keep proportions of the container }
    RealWidth, RealHeight: integer;
    ProportionalScale: TProportionalScale;
    { Is this Container scaled agains Anchors or Window?
      Should be True only at top-level Container (i.e. GUI Container)
      However, maybe, reintroduction or manual scaling would be prefferable? }
    ScaleToWindow: boolean;
    { If this Container ready to be used? }
    property isInitialized: boolean read fInitialized;
    constructor Create; //virtual;
    //destructor Destroy; override;
    { Copy parameters from the Source }
    procedure Assign(const Source: DAbstractContainer);
    procedure AssignTo(const Dest: DAbstractContainer);
    { Set container position and size }
    procedure SetFloatCoord(const afx1,afy1,afx2,afy2: float);
    procedure SetFloatSize(const afx1,afy1,afWidth,afHeight: float);
    procedure SetIntCoord(const ax1,ay1,ax2,ay2: integer);
    procedure SetIntSize(const ax1,ay1,aWidth,aHeight: integer);
    { Sets int width/height for scaling animations }
    procedure SetIntWidthHeight(const aWidth,aHeight: integer);
    procedure SetRealSize(const aWidth,aHeight: integer);
    { Anchors this Container to aParent }
    procedure AnchorTo(const aParent: DAbstractContainer; const Gap: integer = 0);
  end;

type
  { Style of the animation.
    asLinear is just linear interpolation,
    asSquare is slower in the beginning and end, and faster in the middle}
  TAnimationCurve = (acsLinear, acSquare);

Type
  { most abstract container for interface elements
    Defines size, scaling and animation state }
  DAbstractElement = class abstract(DThreadedObject)
  strict protected
    { Caches current animation state, recalculated by GetAnimationState at every render}
    procedure GetAnimationState;
  public
    { changes the scale of the element relative to current window size }
    procedure Rescale; virtual;
    { draw the element / as abstract as it might be :) }
    procedure Draw; virtual;
    { updates the data of the class with current external data,
      here it just gets the current animation state }
    procedure Update; virtual;
  strict private
    { Last and Next animation states. }
    Last, Next: DAbstractContainer;
    fVisible: boolean;
    {$HINT should it set visible to all children? }
    procedure SetVisible(const Value: boolean);
  public
    { these values are "strict" and unaffected by animations. Usually determines
      the basic stage and implies image rescale and init GL. }
    AnimationStart: DTime;
    AnimationDuration: DTime;
    AnimationCurve: TAnimationCurve;
    {base state of the element. contains its coordinates and width/height}
    Current, Base: DAbstractContainer;
    {source width/height of the element. Used to preserve proportions while scaling}

    procedure SetBaseSize(const NewX,NewY,NewW,NewH,NewO: float; const Animate: TAnimationStyle = asNone); virtual;
    //procedure SetIntSize(const x1,y1,x2,y2:integer; Animate: TAnimationStyle); virtual;
    { If the element is visible, if false then draw will not be called.
      PAY ATTENTION: if assigned to a single interface element then the animations
      and initGL will occur as they would normally. BUT if assigned to a
      composite parent element, its children WILL NOT do anything like this and
      will be frozen until visible=true. Maybe I'll fix this some day. }
    property isVisible: boolean read fVisible write SetVisible;
    { animates the interface element from current state to base state,
      Important: GetAnimationState must be called before setting basesize
      of the element as AnimateTo uses currentAnimationState}
    procedure AnimateTo(const Animate: TAnimationStyle; const Duration: float = DefaultAnimationDuration);
    constructor Create; virtual; //override;
    destructor Destroy; override;
  end;

{ Simple procedures for (mouse and time) events }
//type TSimpleProcedure = procedure(sender: DAbstractElement) of Object;
type TSimpleProcedure = procedure of Object;
type TXYProcedure = procedure(const Sender: DAbstractElement; const x,y: integer) of Object;

Type
  { A simple time-out mechanisms to preform some timed events on interface
    elements }
  DTimer = class(TObject)
    private
      { Set automatically, date of the timer count start }
      StartTime: DTime;
    public
      { If the timer is running }
      Enabled: boolean;
      { How long (in seconds) will it take the timer to fire }
      Interval: DTime;
      { Action to preform }
      onTimer: TSimpleProcedure;
      constructor Create;
      { A simple way to set and run timer }
      procedure SetTimeOut(const Seconds: DTime);
      { Check if the timer finished and run onTimer if true }
      procedure Update;
  end;

Type
  { Fully-featured Interface Element with Mouse/Touch support
    It lacks only "Children" or specific "Draw" to be used }
  DSingleInterfaceElement = class abstract(DAbstractElement)
  public
    { Higher-level element. Yet unused, maybe never. }
    Parent: DSingleInterfaceElement;
    { A simple timer to fire some event on time-out }
    Timer: DTimer;
    procedure SetTimeOut(const Seconds: DTime);
    procedure Update; override;
    //also resizes content and frame
 {   procedure SetBaseSize(const NewX,NewY,NewW,NewH,NewO: float; Animate: TAnimationStyle); override;
    procedure SetIntSize(const x1,y1,x2,y2:integer; Animate: TAnimationStyle); override;
}
  {* Mouse routines *}
  public
    {if this element is active (clickable)}
    CanMouseOver: boolean;
    CanDrag: boolean;
    {are these coordinates in this element's box?}
    function IAmHere(const xx,yy: integer): boolean;
    {returns self if IAmHere and runs all possible events}
    function ifMouseOver(const xx,yy: integer; const  RaiseEvents: boolean; const AllTree: boolean): DAbstractElement; virtual;
  private
    {if mouse is over this element}
    isMouseOver: boolean;
  public
    {events}
    OnMouseEnter: TXYProcedure;
    OnMouseLeave: TXYProcedure;
    OnMouseOver: TXYProcedure;
    OnMousePress: TXYProcedure;
    OnMouseRelease: TXYProcedure;
    {dragg-n-drop routines}
    OnDrop: TXYProcedure;
    DragX, DragY: integer;
    procedure Drag(const xx,yy: integer);
    procedure StartDrag(const xx,yy: integer);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

type DInterfaceElementsList = specialize TFPGObjectList<DSingleInterfaceElement>;

Type
  {An interface element, that can contain "children"}
  DInterfaceElement = class(DSingleInterfaceElement)
  public

    {list of the children of this interface element}
    Children: DInterfaceElementsList;
    procedure Draw; override;
    procedure Rescale; override;
  public
    {assign given element as a child and sets its parent to self}
    procedure Grab(const Child: DSingleInterfaceElement);
    {}
    {procedure RescaleToChildren(animate: TAnimationStyle);  }
  public
    {returns last call to MouseOverTree result, may be buggy!}
    isMouseOverTree: boolean;
    {returns Self if IAmHere and runs all possible events + scans all children}
    function ifMouseOver(const xx,yy: integer; const RaiseEvents: boolean; const AllTree: boolean): DAbstractElement; override;
    {returns true if mouse is over any "canmouseover" child of this element}
    function MouseOverTree(const xx,yy: integer): boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

{contains global list of all interface elements // not needed yet}
//var InterfaceList: DInterfaceElementsList;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, CastleLog;

{=============================================================================}
{========================== Abstract Container ===============================}
{=============================================================================}

constructor DAbstractContainer.Create;
var aa: TAnchorSide;
begin
  //inherited;
  ScaleToWindow := false;
  fInitialized := false;
  {this is redundant}
  for aa in TAnchorSide do with Anchor[aa] do begin
    Anchor := nil;
    AlignTo := noalign;
    Gap := 0;
  end;
  Opacity := 1;
  ProportionalScale := psNone;
end;

{destructor DAbstractContainer.Destroy;
begin
  inherited;
end;}

{----------------------------------------------------------------------------}

procedure DAbstractContainer.GetAnchors;
begin
  if ScaleToWindow then begin
    cx1 := 0;
    cy1 := 0;
    cx2 := Window.Width;
    cy2 := Window.Height;
  end else begin
    if (Anchor[asLeft].Anchor = nil) or
       (Anchor[asTop].Anchor = nil) or
       (Anchor[asRight].Anchor = nil) or
       (Anchor[asBottom].Anchor = nil) then begin
         WriteLnLog('DAbstractContainer.GetAnchors','Anchor is Nil!');
         Exit;
       end;

    case Anchor[asLeft].AlignTo of
      haLeft:   cx1 := Anchor[asLeft].Anchor.x1;
      haRight:  cx1 := Anchor[asLeft].Anchor.x2;
      haCenter: cx1 := (Anchor[asLeft].Anchor.x1 + Anchor[asLeft].Anchor.x2) div 2;
      else WriteLnLog('DAbstractContainer.GetAnchors','Invalid Anchor align!')
    end;
    case Anchor[asRight].AlignTo of
      haLeft:   cx2 := Anchor[asRight].Anchor.x1;
      haRight:  cx2 := Anchor[asRight].Anchor.x2;
      haCenter: cx2 := (Anchor[asRight].Anchor.x1 + Anchor[asRight].Anchor.x2) div 2;
      else WriteLnLog('DAbstractContainer.GetAnchors','Invalid Anchor align!')
    end;
    case Anchor[asTop].AlignTo of
      vaTop:    cy1 := Anchor[asTop].Anchor.y1;
      vaBottom: cy1 := Anchor[asTop].Anchor.y2;
      vaMiddle: cy1 := (Anchor[asTop].Anchor.y1 + Anchor[asTop].Anchor.y2) div 2;
      else WriteLnLog('DAbstractContainer.GetAnchors','Invalid Anchor align!')
    end;
    case Anchor[asBottom].AlignTo of
      vaTop:    cy2 := Anchor[asBottom].Anchor.y1;
      vaBottom: cy2 := Anchor[asBottom].Anchor.y2;
      vaMiddle: cy2 := (Anchor[asBottom].Anchor.y1 + Anchor[asBottom].Anchor.y2) div 2;
      else WriteLnLog('DAbstractContainer.GetAnchors','Invalid Anchor align!')
    end;
  end;
  cw := cx2-cx1;
  ch := cy2-cy1;
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.IntegerToFloat;
begin
  GetAnchors;

  fx1 := (x1 - cx1 - Anchor[asLeft  ].Gap)/cw;
  fx2 := (x2 - cx2 + Anchor[asRight ].Gap)/cw;
  fy1 := (y1 - cy1 - Anchor[asTop   ].Gap)/ch;
  fy2 := (y2 - cy2 + Anchor[asBottom].Gap)/ch;

  fInitialized := true;
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.FloatToInteger;
var Ratio: float;
begin
  GetAnchors;

  x1 := cx1 + Round(cw * fx1) + Anchor[asLeft].Gap;
  x2 := cx2 + Round(cw * fx2) - Anchor[asRight].Gap;
  y1 := cy1 + Round(ch * fy1) + Anchor[asTop].Gap;
  y2 := cy2 + Round(ch * fy2) - Anchor[asBottom].Gap;

  w := x2 - x1;
  h := y2 - y1;

  {inefficient}
  case ProportionalScale of
    psWidth:  begin
                Ratio := RealWidth/RealHeight;
                w := Round(h*Ratio);
                x2 := x1 + w;
              end;
    psHeight: begin
                Ratio := RealHeight/RealWidth;
                h := Round(w*Ratio);
                y2 := y1 + h;
              end;
  end;

  fInitialized := true;
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.SetFloatCoord(const afx1,afy1,afx2,afy2: float);
begin
  fx1 := afx1;
  fy1 := afy1;
  fx2 := afx2;
  fy2 := afy2;

  FloatToInteger;
end;
procedure DAbstractContainer.SetFloatSize(const afx1,afy1,afWidth,afHeight: float);
begin
  fx1 := afx1;
  fy1 := afy1;
  fx2 := - (1 - afWidth - afx1);
  fy2 := - (1 - afHeight - afy1);

  FloatToInteger;
end;
procedure DAbstractContainer.SetIntCoord(const ax1,ay1,ax2,ay2: integer);
begin
  x1 := ax1;
  y1 := ay1;
  x2 := ax2;
  y2 := ay2;

  IntegerToFloat;
end;
procedure DAbstractContainer.SetIntSize(const ax1,ay1,aWidth,aHeight: integer);
begin
  x1 := ax1;
  y1 := ay1;
  x2 := ax1 + aWidth;
  y2 := ay1 + aHeight;

  IntegerToFloat;
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.SetIntWidthHeight(const aWidth,aHeight: integer);
begin
  x1 := x1 + (w-aWidth) div 2;
  x2 := x2 - (w-aWidth) div 2;
  y1 := y1 + (h-aHeight) div 2;
  y2 := y2 - (h-aHeight) div 2;

  w := aWidth;
  h := aHeight;

  IntegerToFloat; //not needed here?
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.SetRealSize(const aWidth,aHeight: integer);
begin
  RealWidth := aWidth;
  RealHeight := aHeight;
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.AnchorTo(const aParent: DAbstractContainer; const Gap: integer = 0);
begin
  Anchor[asLeft  ].Anchor := aParent;
  Anchor[asLeft  ].Gap := Gap;
  Anchor[asLeft  ].AlignTo := haLeft;
  Anchor[asRight ].Anchor := aParent;
  Anchor[asRight ].Gap := Gap;
  Anchor[asRight ].AlignTo := haRight;
  Anchor[asTop   ].Anchor := aParent;
  Anchor[asTop   ].Gap := Gap;
  Anchor[asTop   ].AlignTo := vaTop;
  Anchor[asBottom].Anchor := aParent;
  Anchor[asBottom].Gap := Gap;
  Anchor[asBottom].AlignTo := vaBottom;
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.Assign(const Source: DAbstractContainer);
var aa: TAnchorSide;
begin
  Self.fx1 := Source.fx1;
  Self.fy1 := Source.fy1;
  Self.fx2 := Source.fx2;
  Self.fy2 := Source.fy2;
  {Self.fw := Source.fw;
  Self.fh := Source.fh;}
  Self.x1 := Source.x1;
  Self.y1 := Source.y1;
  Self.x2 := Source.x2;
  Self.y2 := Source.y2;
  Self.w := Source.w;
  Self.h := Source.h;
  Self.RealWidth := Source.RealWidth;
  Self.RealHeight := Source.RealHeight;
  Self.Opacity := Source.Opacity;
  Self.ProportionalScale := Source.ProportionalScale;
  Self.fInitialized := Source.isInitialized;
  for aa in TAnchorSide do
    Self.Anchor[aa] := Source.Anchor[aa];
end;
procedure DAbstractContainer.AssignTo(const Dest: DAbstractContainer);
var aa: TAnchorSide;
begin
  Dest.fx1 := Self.fx1;
  Dest.fy1 := Self.fy1;
  Dest.fx2 := Self.fx2;
  Dest.fy2 := Self.fy2;
  {Dest.fw := Self.fw;
  Dest.fh := Self.fh;}
  Dest.x1 := Self.x1;
  Dest.y1 := Self.y1;
  Dest.x2 := Self.x2;
  Dest.y2 := Self.y2;
  Dest.w := Self.w;
  Dest.h := Self.h;
  Dest.RealWidth := Self.RealWidth;
  Dest.RealHeight := Self.RealHeight;
  Dest.Opacity := Self.Opacity;
  Dest.ProportionalScale := Self.ProportionalScale;
  Dest.fInitialized := Self.isInitialized;
  for aa in TAnchorSide do
    Dest.Anchor[aa] := Self.Anchor[aa];
end;

{============================================================================}
{======================== ABSTRACT ELEMENT ==================================}
{============================================================================}

procedure DAbstractElement.Rescale;
begin
  {set animation states to changed container size}
  Base.FloatToInteger;
  Last.FloatToInteger;
  Next.FloatToInteger;
  //not setting Current here as it is automatically determined in GetCurrentAnimationState without any scaling
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.SetVisible(const Value: boolean);
begin
  fVisible := Value;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.AnimateTo(const Animate: TAnimationStyle; const Duration: float = DefaultAnimationDuration);
var mx,my: float;
begin
  if Animate = asNone then Exit else begin
    AnimationStart := -1;
    AnimationDuration := Duration;

    GetAnimationState;
    Last.Assign(Current);   //to current animation state
    Next.Assign(Base);
    case Animate of
      {just grabs some previous locations and animates the item from there to base
       requires that CurrentAnimationState is initialized... TODO}
      //asDefault: Last.Assign(Self);
      {fades in/out element}
      asFadeIn:  Last.Opacity := 0;
      asFadeOut: Next.Opacity := 0;
      {asFadeOutSuicide}
      {zooms in/out element}
      asZoomIn:  begin
                   Last.SetIntWidthHeight(1,1);
                   Last.Opacity := 0;
                 end;
      asZoomOut: begin
                   Next.SetIntWidthHeight(1,1);
                   Next.Opacity := 0;
                 end;

      {asZoomOutSuicide}
      asFlyInRandom,asFlyOutRandom: begin
                       mx := drnd.Random;
                       my := drnd.Random;
                       case drnd.Random(4) of
                         0: mx :=  0.0001;
                         1: mx := -0.0001;
                         2: my :=  0.0001;
                         3: my := -0.0001;
                       end;
                       if Animate=asFlyInRandom then begin
                         Last.fx1 := mx;
                         Last.fy1 := my;
                         Last.ScaleToWindow := true;
                       end else begin
                         Next.fx2 := mx;
                         Next.fy2 := my;
                         Next.ScaleToWindow := true;
                       end;
                     end;
      asFlyInTop,asFlyOutTop,asFlyInBottom,asFlyOutBottom,asFlyInLeft,asFlyOutLeft,asFlyInRight,asFlyOutRight: begin
                       mx := drnd.Random*Window.Width/Window.Height;
                       my := drnd.Random;
                       case animate of
                         asFlyInLeft,asFlyOutLeft: mx :=  0.0001;
                         asFlyInRight,asFlyOutRight: mx := -0.0001;
                         asFlyInBottom,asFlyOutBottom: my := 0.0001;
                         asFlyInTop,asFlyOutTop: my := -0.0001;
                       end;
                       if (Animate=asFlyInLeft) or (Animate=asFlyInRight) or (Animate=asFlyInTop) or (Animate=asFlyInBottom) then begin
                         Last.fx1 := mx;
                         Last.fy1 := my;
                         Last.ScaleToWindow := true;
                       end else begin
                         Next.fx2 := mx;
                         Next.fy2 := my;
                         Next.ScaleToWindow := true;
                       end;
                     end;
      {asFlyOutRandomSuicide}
    end;
  end;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.SetBaseSize(const NewX,NewY,NewW,NewH,NewO: float; const Animate: TAnimationStyle = asNone);
begin
  Base.SetFloatSize(NewX,NewY,NewW,NewH);
  Base.Opacity := NewO;
  AnimateTo(Animate);
end;

{----------------------------------------------------------------------------}

{procedure DAbstractElement.SetIntSize(const x1,y1,x2,y2:integer; const Animate: TAnimationStyle);
begin
  Base.BackwardSetXYWH(x1,y1,x2-x1,y2-y1);
  AnimateTo(Animate);
end;}

{----------------------------------------------------------------------------}

{procedure DSingleInterfaceElement.SetBaseSize(const NewX,NewY,NewW,NewH,NewO: float; const Animate: TAnimationStyle);
begin
  inherited SetBaseSize(NewX,NewY,NewW,NewH,NewO,Animate);
  ResetContentSize(Animate);
end;}

{----------------------------------------------------------------------------}

{procedure DSingleInterfaceElement.SetIntsize(const x1,y1,x2,y2:integer; const Animate: TAnimationStyle);
begin
  inherited SetIntsize(x1,y1,x2,y2,Animate);
  ResetContentSize(Animate);
end;}

{----------------------------------------------------------------------------}

procedure DAbstractElement.GetAnimationState;
var Phase: float;
begin
  if Base.isInitialized then begin
    if (Last.isInitialized) and (Next.isInitialized) and
      ((Animationstart<0) or (DecoNow-AnimationStart < AnimationDuration)) then begin
      //if this is start of the animation - init time
      if AnimationStart<0 then AnimationStart := DecoNow;
      //determine animation time passed
      Phase := (DecoNow-AnimationStart)/AnimationDuration;
      //determine animation phase
      case AnimationCurve of
        //acLinear: ; //<- change nothing.
        acSquare: if Phase<0.5 then Phase := Sqr(2*Phase)/2 else Phase := 1 - Sqr(2*(1-Phase))/2;
      end;
      Current.x1 := Last.x1+Round((Next.x1-Last.x1)*Phase);
      Current.y1 := Last.y1+Round((Next.y1-Last.y1)*Phase);
      Current.x2 := Last.x2+Round((Next.x2-Last.x2)*Phase);
      Current.y2 := Last.y2+Round((Next.y2-Last.y2)*Phase);
      Current.Opacity := Last.Opacity+((Next.opacity-Last.Opacity)*Phase);
      //we don't need scale back to float, as it's not a basic animation state
    end else begin
      {should be "next" here}
      Current.x1 := Base.x1;
      Current.x2 := Base.x2;
      Current.y1 := Base.y1;
      Current.y2 := Base.y2;
      Current.Opacity := Base.Opacity;
    end;
    Current.w := Current.x2 - Current.x1;
    Current.h := Current.y2 - Current.y1;
    //Current.fInitialized := true;
 end else
   //Current.fInitialized := false;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.Update;
begin
  GetAnimationState;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.Draw;
begin
  Update;
  if not isVisible then Exit;
end;

{----------------------------------------------------------------------------}

constructor DAbstractElement.Create;
begin
  inherited;
  fVisible := true;
  AnimationCurve := acSquare;
  Base := DAbstractContainer.Create;
  Last := DAbstractContainer.Create;
  Next := DAbstractContainer.Create;
  Current := DAbstractContainer.Create;
end;

{----------------------------------------------------------------------------}

destructor DAbstractElement.Destroy;
begin
  FreeAndNil(Base);
  FreeAndNil(Last);
  FreeAndNil(Next);
  FreeAndNIl(Current);
  inherited;
end;

{=============================================================================}
{=============== Single interface element & Timer ============================}
{=============================================================================}

constructor DTimer.Create;
begin
  inherited;
  Enabled := false;
  StartTime := -1;
end;

{-----------------------------------------------------------------------------}

procedure DTimer.Update;
begin
  if StartTime<0 then StartTime := DecoNow else
  if (DecoNow-StartTime) >= Interval then begin
    Enabled := false;
    if Assigned(onTimer) then onTimer;
  end;
end;

{-----------------------------------------------------------------------------}

procedure DTimer.SetTimeout(const Seconds: DTime);
begin
  StartTime := -1;
  Enabled := true;
  Interval := Seconds;
end;

{============================================================================}

constructor DSingleInterfaceElement.Create;
begin
  inherited;
  isMouseOver := false;
  CanMouseOver := false;
  CanDrag := false;
end;

{-----------------------------------------------------------------------------}

destructor DSingleInterfaceElement.Destroy;
begin
  FreeAndNil(Timer);
  inherited;
end;

{----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.SetTimeOut(const Seconds: DTime);
begin
  if Timer = nil then Timer := DTimer.Create;
  Timer.SetTimeOut(Seconds);
end;

{----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.Update;
begin
  inherited;
  if (Timer<>nil) and (Timer.Enabled) then Timer.Update;
end;

{==============================  Mouse handling =============================}

function DSingleInterfaceElement.IAmHere(const xx,yy: integer): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if (xx >= Base.x1) and (xx <= Base.x2) and
     (yy >= Base.y1) and (yy <= Base.y2)
  then
    Result := true
  else
    Result := false;
end;

{-----------------------------------------------------------------------------}

function DSingleInterfaceElement.ifMouseOver(const xx,yy: integer; const RaiseEvents: boolean; const AllTree: boolean): DAbstractElement;
begin
  Result := nil;
  if IAmHere(xx,yy) then begin
    if RaiseEvents then begin
      if isMouseOver = false then begin
        if Assigned(onMouseEnter) then onMouseEnter(self,xx,yy);
        isMouseOver := true;
      end;
      if Assigned(onMouseOver) then onMouseOver(self,xx,yy);
    end;
    if CanMouseOver then  //todo
      Result := Self
  end else begin
    if isMouseOver and RaiseEvents then begin
      if Assigned(onMouseLeave) then onMouseLeave(Self,xx,yy);
      isMouseOver := false;
    end;
  end;
end;

function DInterfaceElement.ifMouseOver(const xx,yy: integer; const RaiseEvents: boolean; const AllTree: boolean): DAbstractElement;
var i: integer;
    tmpLink: DAbstractElement;
begin
  Result := inherited ifMouseOver(xx,yy,RaiseEvents,AllTree);
  //if rsult<>nil ... *or drag-n-drop should get the lowest child?

  // recoursively scan all children
  for i := 0 to Children.Count-1 do begin
    tmpLink := Children[i].ifMouseOver(xx,yy,RaiseEvents,AllTree);
    if tmpLink <> nil then begin
      Result := tmpLink;
      if not AllTree then Break; // if drag-n-drop one is enough
    end;
  end;
end;

{-----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.StartDrag(const xx,yy: integer);
begin
  DragX := Base.x1 - xx;
  DragY := Base.y1 - yy;
end;
procedure DSingleInterfaceElement.Drag(const xx,yy: integer);
begin
  Base.x1 := DragX + xx;
  Base.y1 := DragY + yy;
end;

{=============================================================================}
{=========================== interface element ===============================}
{=============================================================================}

constructor DInterfaceElement.Create;
begin
  inherited;
  Children := DInterfaceElementsList.Create(true);
end;

{----------------------------------------------------------------------------}

destructor DInterfaceElement.Destroy;
begin
  //this should fire as recoursive because children owns elements, which in turn will fire their destructors onfree
  FreeAndNil(Children);
  inherited;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Rescale;
var i: integer;
begin
  inherited;
  for i:=0 to Children.Count-1 do Children[i].Rescale;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceElement.Draw;
var i: integer;
begin
  inherited;
  for i := 0 to Children.Count-1 do Children[i].Draw;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Grab(const Child: DSingleInterfaceElement);
begin
  Children.Add(Child);
  if (Child is DSingleInterfaceElement) then DSingleInterfaceElement(Child).Parent := Self;
  //{Child.ID := }InterfaceList.Add(Child); //global ID of the element
end;

{----------------------------------------------------------------------------}

{procedure DInterfaceElement.RescaleToChildren(const Animate: TAnimationStyle);
var i: integer;
    x1,y1,x2,y2: integer;
begin
  if Children.Count>0 then begin
    x1 := Window.Width;
    y1 := Window.Height;
    x2 := 0;
    y2 := 0;
    for i := 1 to Children.Count-1 do begin
      if x1>Children[i].Base.x1 then x1 := Children[i].Base.x1;
      if y1>Children[i].Base.y1 then y1 := Children[i].Base.y1;
      if x2>Children[i].Base.x2 then x2 := Children[i].Base.x2;
      if y2>Children[i].Base.y2 then y2 := Children[i].Base.y2;
    end;
    Self.SetIntSize(x1,y1,x2,y2,Animate);
  end
  else WriteLnLog('DInterfaceElement.RescaleToChildren','No children for resale to');
end;}

{-----------------------------------------------------------------------}

function DInterfaceElement.MouseOverTree(const xx,yy: integer): boolean;
var tmp: DAbstractElement;
begin
  // maybe rewrite it using isMouseOver - the idea is still a little different
  tmp := Self.ifMouseOver(xx,yy,false,false);
  if (tmp <> nil) and (tmp is DSingleInterfaceElement) and (DSingleInterfaceElement(tmp).CanMouseOver){ and (tmp.base.opacity>0)} then
    isMouseOverTree := true
  else
    isMouseOverTree := false;
  //base.opacity breaks the algorithm, if transparent item is above (i.e. below) the opaque element
  Result := isMouseOverTree;
end;

end.

