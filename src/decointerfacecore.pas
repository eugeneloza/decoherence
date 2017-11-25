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
unit DecoInterfaceCore;

{$INCLUDE compilerconfig.inc}

interface

uses Generics.Collections,
  DecoGlobal, DecoTime;

const DefaultAnimationDuration = 0.3; {in seconds}
const UninitializedIntCoord = -999;

{ Animation style of the object. asDefault means "animate from previous state",
  presuming that there was some "previous state"}
Type TAnimationStyle = (asNone, asDefault,
                        asFadeIn, asFadeOut,// asFadeOutSuicide,
                        asZoomIn, asZoomOut,// asZoomOutSuicide,
                        asFlyInRandom, asFlyOutRandom,// asFlyOutRandomSuicide,
                        asFlyInTop,asFlyOutTop,
                        asFlyInBottom,asFlyOutBottom,
                        asFlyInLeft,asFlyOutLeft,
                        asFlyInRight,asFlyOutRight);

{
 PAY ATTENTION!
   x1 = Left
   x2 = Right
   y1 = Bottom
   y2 = Top

   x2>x1 ; Width = x2 - x1;
   y2>y1 ; Height = y2 - y1;

   Left/right/bottom/top are how they are SEEN AT SCREEN
   x1/x2/y1/y2 are how they are RENDERED by OPENGL

   Anchor to Bottom means that Element is anchored to Bottom coordinate!
}

{ Type of this Anchor }
type TAnchorSide = (asLeft,asRight,asTop,asBottom);
{ Which part of Parent does this Anchor align to? }
type TAnchorAlign = (noAlign, haLeft, haRight, haCenter, vaTop, vaBottom, vaMiddle);
{ Which part of the image is fitted to maintain proportions }
type TProportionalScale = (psNone, psWidth, psHeight);

type
  { }
  Txy = record
  x1,y1,x2,y2: integer;
end;


type
  { }
  DAnchoredObject = class abstract (DObject)
  public
    { }
    procedure AddAnchor(aAnchor: DAnchoredObject); virtual; abstract;
    { }
    procedure Rescale; virtual; abstract;
  end;

type TAnchorList = specialize TObjectList<DAnchoredObject>;

type
  { This is an container with coordinates and size
    Capable of rescaling / copying itself
    As abstract as it seems, it's also used as animation state,
    So, constructing it as standalone should be possible }
  DIntContainer = class(DObject)
  strict private
    function GetWidth: integer;
    function GetHeight: integer;
  strict protected
    { Owner of this Container (for displaying debug info) }
    Owner: DAnchoredObject;
  public
    { Real size of the Container }
    x1,y1,x2,y2: integer;
    Opacity: float;


    //procedure SetWidth(const aWidth: integer);
    //procedure SetHeight(const aHeight: integer);

    property w: integer read GetWidth {write SetWidth};
    property h: integer read GetHeight {write SetHeight};

    procedure Assign(const Source: DIntContainer);
    procedure AssignTo(const Dest: DIntContainer);

    procedure SetIntCoord(const ax1,ay1,ax2,ay2: integer);
    procedure SetIntFull(const ax1,ay1,ax2,ay2: integer; const aOpacity: float);
    procedure SetIntFull(const ax1,ay1,ax2,ay2: float; const aOpacity: float);
    procedure SetIntSize(const ax1,ay1,aWidth,aHeight: integer);

    constructor Create(aOwner: DAnchoredObject); virtual;
    //destructor Destroy; override;
  end;

type
  { }
  DFloatContainer = class(DIntContainer)
  strict protected
    fInitialized: boolean;
    { Parent container size (cached) }
    cx1,cx2,cy1,cy2,cw,ch: integer;
    cValid: boolean;
    { Parent container opacity }
    co: float;
    { Determine and cache parent container size }
    procedure GetAnchors;
    { Anchor this Container to Window }
    procedure GetWindowAnchor;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    { Converts integer size to float. }
    procedure IntegerToFloat;
    { Resets x2,y2,w,h to fit RealWidth,RealHeight This suggest use of only Left anchor! }
    procedure AdjustToRealSize;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF} deprecated;

    function GetInitialized: boolean;
  public
    { Converts float size to integer.
      should be public only for animation states. }
    procedure FloatToInteger;
  public
    type
      { Specifies Anchor object, Anchor type, and additional gap }
      DAnchor = record
        { Anchor to which element }
        Anchor: DFloatContainer;
        { To which side of the Anchor should it align? }
        AlignTo: TAnchorAlign;
        { Additional gap between this element and Anchor in px }
        Gap: integer;
      end;
  public
    { Anchors of this container }
    Anchor: array[TAnchorSide] of DAnchor;
    { Parent's opacity is multiplied by this Container opacity,
      May be nil }
    OpacityAnchor: DFloatContainer;
    { Is this Container scaled agains Anchors or Window?
      Should be True only at top-level Container (i.e. GUI Container)
      However, maybe, reintroduction or manual scaling would be prefferable? }
    AnchorToWindow: boolean;
    { Float size of the Container }
    fx1,fy1,fx2,fy2{,fw,fh}: float;
    { Base value of the opacity of the container }
    BaseOpacity: float; //deprecated;
    { Keep proportions of the container }
    RealWidth, RealHeight: integer; //deprecated;
    { Can this item be scaled? Otherwise its w, h is always RealWidth, RealHeight}
    ScaleItem: boolean; //deprecated;
    { Should this Container scale proportionaly? }
    ProportionalScale: TProportionalScale; //deprecated;
    { If this Container ready to be used? }
    property isInitialized: boolean read GetInitialized;
    { Copy parameters from the Source }
    procedure Assign(const Source: DFloatContainer);
    procedure AssignTo(const Dest: DFloatContainer);
    { Set container position and size }
    procedure SetFloatCoord(const afx1,afy1,afx2,afy2: float);
    procedure SetFloatFull(const afx1,afy1,afx2,afy2,aOpacity: float);
    procedure SetFloatSize(const afx1,afy1,afWidth,afHeight: float);
    procedure SetFloatSizeFull(const afx1,afy1,afWidth,afHeight,aOpacity: float);

    { Sets int width/height for scaling animations }
    procedure SetIntWidthHeight(const aWidth,aHeight: integer); deprecated;
    { Resets width and height to their "real" values, e.g. for elements that are not scaled }
    procedure ResetToReal; deprecated;
    procedure SetRealSize(const aWidth,aHeight: integer); deprecated;
    { Anchors this Container to aParent }
    procedure AnchorTo(const aParent: DFloatContainer; const Gap: integer = 0);
    { Anchors aChild to this Container  }
    procedure AnchorChild(const aChild: DFloatContainer; const Gap: integer = 0);
    { }
    procedure AnchorSide(const aSide: TAnchorSide; const aParent: DFloatContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
    procedure AnchorTop(const aParent: DFloatContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
    procedure AnchorBottom(const aParent: DFloatContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
    procedure AnchorLeft(const aParent: DFloatContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
    procedure AnchorRight(const aParent: DFloatContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
    { do these two Containers have equal anchors? }
    function AnchorsEqual(const aCompare: DFloatContainer): boolean;
  public
    constructor Create(aOwner: DAnchoredObject); override;
  end;

type
  { Style of the animation.
    asLinear is just linear interpolation,
    asSquare is slower in the beginning and end, and faster in the middle}
  TAnimationCurve = (acsLinear, acSquare);


type
  { most abstract container for interface elements
    Defines size, scaling and animation state }
  DAbstractElement = class abstract(DAnchoredObject)
  strict protected
    { Caches current animation state, recalculated by GetAnimationState at every render}
    procedure GetAnimationState;
    { updates the data of the class with current external data,
      here it just gets the current animation state }
    procedure Update; virtual;
    { Sets this interface element size to full screen and aligns it to Window }
    procedure SetFullScreen;
    { Returns true size of this interface element }
    function GetSize: Txy; virtual; deprecated;
  strict protected
    { }
    NotifyAnchors: TAnchorList;
    { }
    procedure NotifyRescale;
  public
    { }
    procedure AddAnchor(aAnchor: DAnchoredObject); override;
    { }
    //function ProcessRescaleResult(var r1: TRescaleResult; const r2: TRescaleResult): TRescaleResult;
    { changes the scale of the element relative to current window size }
    procedure Rescale; override;
    { }
    procedure RescaleRecoursive; virtual;
    { }
    function RescaleContainer: boolean;
    { }
    function RescaleContent: boolean; virtual;
    { draw the element / as abstract as it might be :) }
    procedure Draw; virtual; abstract;
  strict private
    { Last and Next animation states. }
    Last: DIntContainer;
    Next: DFloatContainer;
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
    Current: DIntContainer;
    Base: DFloatContainer;
    {source width/height of the element. Used to preserve proportions while scaling}

    procedure SetBaseSize(const NewX,NewY,NewW,NewH: float;NewO: float=1; const Animate: TAnimationStyle = asNone); virtual;
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
    procedure AnchorTo(const aElement: DAbstractElement);
  public
    constructor Create; virtual;//override;
    destructor Destroy; override;
  end;

{ Simple procedures for (mouse and time) events }
//type TSimpleProcedure = procedure(sender: DAbstractElement) of Object;
type TSimpleProcedure = procedure of Object;
type TXYProcedure = procedure(const Sender: DAbstractElement; const x,y: integer) of Object;

Type
  { A simple time-out mechanisms to preform some timed events on interface
    elements }
  DTimer = class(DObject)
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
    { Activate and initialize timer }
    procedure SetTimeOut(const Seconds: DTime);
    procedure Update; override;
    //also resizes content and frame
 {   procedure SetBaseSize(const NewX,NewY,NewW,NewH,NewO: float; Animate: TAnimationStyle); override;
    procedure SetIntSize(const x1,y1,x2,y2:integer; Animate: TAnimationStyle); override;
}
  {* Mouse routines *}
  public
    { If this element is active (clickable) }
    CanMouseOver: boolean;
    CanDrag: boolean;
    { Are these coordinates in this element's box? }
    function IAmHere(const xx,yy: integer): boolean;
    { Returns self if IAmHere and runs all possible events }
    function ifMouseOver(const xx,yy: integer; const  RaiseEvents: boolean; const AllTree: boolean): DAbstractElement; virtual;
  private
    { If mouse is over this element }
    isMouseOver: boolean;
  public
    { Mouse/touch Events }
    OnMouseEnter: TXYProcedure;
    OnMouseLeave: TXYProcedure;
    OnMouseOver: TXYProcedure;
    OnMousePress: TXYProcedure;
    OnMouseRelease: TXYProcedure;
    { Dragg-n-drop routines }
    OnDrop: TXYProcedure;
    DragX, DragY: integer;
    procedure Drag(const xx,yy: integer);
    procedure StartDrag(const xx,yy: integer);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

type DInterfaceElementsList = specialize TObjectList<DSingleInterfaceElement>;

Type
  { An interface element, that can contain "Children" }
  DInterfaceElement = class(DSingleInterfaceElement)
  strict protected
    { Returns true size of this interface element and all of its children }
    function GetSize: Txy; override; deprecated;
  public
    { List of the children of this interface element }
    Children: DInterfaceElementsList;
    procedure Draw; override;
    { }
    procedure RescaleRecoursive; override;
    { Rescales this Interface element to fit children sizes
      (should be used only in case children may have fixed sizes (like text labels)
       or force-change their size, e.g. if they don't allow size smaller
       than frame gaps)}
    procedure RescaleToChildren; deprecated;
  public
    { Assign given element as a child and sets its parent to Self }
    procedure Grab(const Child: DSingleInterfaceElement);
    { Removes and frees all children }
    procedure Clear;
  public
    { Returns last (cached) call to MouseOverTree result, may be invalid! }
    isMouseOverTree: boolean;
    { Returns Self if IAmHere and runs all possible events + scans all children }
    function ifMouseOver(const xx,yy: integer; const RaiseEvents: boolean; const AllTree: boolean): DAbstractElement; override;
    { Returns true if mouse is over any "canmouseover" child of this element }
    function MouseOverTree(const xx,yy: integer): boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

{contains global list of all interface elements // not needed yet}
//var InterfaceList: DInterfaceElementsList;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils,
  DecoLog, Profiler;

{=============================================================================}
{=========================== Integer Container ===============================}
{=============================================================================}

constructor DIntContainer.Create(aOwner: DAnchoredObject);
begin
  StartProfiler;

  //inherited Create;
  Owner := aOwner;

  x1 := UninitializedIntCoord; //used for debugging
  x2 := UninitializedIntCoord;
  y1 := UninitializedIntCoord;
  y2 := UninitializedIntCoord;
  //Opacity := 1;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DIntContainer.SetIntCoord(const ax1,ay1,ax2,ay2: integer);
begin
  StartProfiler;

  x1 := ax1;
  y1 := ay1;
  {if ScaleItem then begin
    x2 := ax2;
    y2 := ay2;
  end else AdjustToRealSize;

  IntegerToFloat;}
  {$WARNING Integers are not converted to float}

  StopProfiler;
end;
procedure DIntContainer.SetIntFull(const ax1,ay1,ax2,ay2: integer; const aOpacity: float);
begin
  StartProfiler;

  {$WARNING Base opacity is not set!}
  Opacity := aOpacity;
  SetIntCoord(ax1,ay1,ax2,ay2);

  StopProfiler;
end;
procedure DIntContainer.SetIntFull(const ax1,ay1,ax2,ay2: float; const aOpacity: float);
begin
  StartProfiler;

  {$WARNING Base opacity is not set!}
  Opacity := aOpacity;
  SetIntCoord(Round(ax1),Round(ay1),Round(ax2),Round(ay2));

  StopProfiler;
end;
procedure DIntContainer.SetIntSize(const ax1,ay1,aWidth,aHeight: integer);
begin
  StartProfiler;

  SetIntCoord(ax1,ay1,ax1 + aWidth,ay1 + aHeight);

  StopProfiler;
end;

{procedure DIntegerContainer.SetWidth(const aWidth: integer);
begin
  StartProfiler;

  {unoptimal and might be wrong}
  //SetIntCoord(x1,y1,x1+aWidth,y2);

  StopProfiler;
end;
procedure DIntegerContainer.SetHeight(const aHeight: integer);
begin
  StartProfiler;

  {unoptimal and might be wrong}
  //SetIntCoord(x1,y1,x2,y1+aHeight);

  StopProfiler;
end;}

{----------------------------------------------------------------------------}

function DIntContainer.GetWidth: integer;
begin
  StartProfiler;

  Result := x2-x1;

  StopProfiler;
end;
function DIntContainer.GetHeight: integer;
begin
  StartProfiler;

  Result := y2-y1;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DIntContainer.Assign(const Source: DIntContainer);
var aa: TAnchorSide;
begin
  StartProfiler;

  Self.x1 := Source.x1;
  Self.y1 := Source.y1;
  Self.x2 := Source.x2;
  Self.y2 := Source.y2;
  Self.Opacity := Source.Opacity;
  if Self.Owner <> Source.Owner then begin
    Log(LogInterfaceScaleError, _CurrentRoutine, 'WARNING: NotifyAnchor should be copied, do it!');
  end;

  StopProfiler;
end;
procedure DIntContainer.AssignTo(const Dest: DIntContainer);
var aa: TAnchorSide;
begin
  StartProfiler;

  Dest.x1 := Self.x1;
  Dest.y1 := Self.y1;
  Dest.x2 := Self.x2;
  Dest.y2 := Self.y2;
  Dest.Opacity := Self.Opacity;
  if Dest.Owner <> Self.Owner then begin
    Log(LogInterfaceScaleError, _CurrentRoutine,'WARNING: NotifyAnchor should be copied, do it!');
  end;

  StopProfiler;
end;

{=============================================================================}
{============================ Float Container ================================}
{=============================================================================}

constructor DFloatContainer.Create(aOwner: DAnchoredObject);
var aa: TAnchorSide;
begin
  StartProfiler;

  inherited Create(aOwner);
  AnchorToWindow := false;
  fInitialized := false;
  {this is redundant}
  for aa in TAnchorSide do with Anchor[aa] do begin
    Anchor := nil;
    AlignTo := noalign;
    Gap := 0;
  end;
  OpacityAnchor := nil;
  BaseOpacity := 1;
  ProportionalScale := psNone;
  ScaleItem := true;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DFloatContainer.GetWindowAnchor;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  StartProfiler;

  cx1 := 0;
  cy1 := 0;
  cx2 := Window.Width;
  cy2 := Window.Height;

  StopProfiler;
end;

procedure DFloatContainer.GetAnchors;
begin
  StartProfiler;

  if AnchorToWindow then
    GetWindowAnchor
  else begin
    if (Anchor[asLeft].Anchor = nil) or
       (Anchor[asTop].Anchor = nil) or
       (Anchor[asRight].Anchor = nil) or
       (Anchor[asBottom].Anchor = nil) then begin
         Log(LogInterfaceError,Owner.ClassName+'>'+_CurrentRoutine,'Anchor is Nil!');
         GetWindowAnchor;
    end else begin
      case Anchor[asLeft].AlignTo of
        haLeft:   cx1 := Anchor[asLeft].Anchor.x1;
        haRight:  cx1 := Anchor[asLeft].Anchor.x2;
        haCenter: cx1 := (Anchor[asLeft].Anchor.x1 + Anchor[asLeft].Anchor.x2) div 2;
        else Log(LogInterfaceError,Owner.ClassName+'>'+_CurrentRoutine,'Invalid Anchor align!')
      end;
      case Anchor[asRight].AlignTo of
        haLeft:   cx2 := Anchor[asRight].Anchor.x1;
        haRight:  cx2 := Anchor[asRight].Anchor.x2;
        haCenter: cx2 := (Anchor[asRight].Anchor.x1 + Anchor[asRight].Anchor.x2) div 2;
        else Log(LogInterfaceError,Owner.ClassName+'>'+_CurrentRoutine,'Invalid Anchor align!')
      end;
      case Anchor[asBottom].AlignTo of                {Pay attention, this is INVERT due to OpenGL}
        vaBottom: cy1 := Anchor[asBottom].Anchor.y1;
        vaTop:    cy1 := Anchor[asBottom].Anchor.y2;
        vaMiddle: cy1 := (Anchor[asBottom].Anchor.y1 + Anchor[asBottom].Anchor.y2) div 2;
        else Log(LogInterfaceError,Owner.ClassName+'>'+_CurrentRoutine,'Invalid Anchor align!')
      end;
      case Anchor[asTop].AlignTo of             {Pay attention, this is INVERT due to OpenGL}
        vaBottom: cy2 := Anchor[asTop].Anchor.y1;
        vaTop:    cy2 := Anchor[asTop].Anchor.y2;
        vaMiddle: cy2 := (Anchor[asTop].Anchor.y1 + Anchor[asTop].Anchor.y2) div 2;
        else Log(LogInterfaceError,Owner.ClassName+'>'+_CurrentRoutine,'Invalid Anchor align!')
      end;
    end;  
  end;
  if (OpacityAnchor <> nil) then
    co := OpacityAnchor.Opacity
  else
    co := 1;

  cw := cx2-cx1;
  ch := cy2-cy1;
  if (cw > 0) and (ch > 0) then cValid := true else cValid := false;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DFloatContainer.IntegerToFloat;
begin
  StartProfiler;

  GetAnchors;

  if cValid then begin
    Opacity := BaseOpacity * co;

    fx1 := (x1 - cx1 - Anchor[asLeft  ].Gap)/cw;
    fx2 := (x2 - cx2 + Anchor[asRight ].Gap)/cw;
    fy1 := (y1 - cy1 - Anchor[asBottom].Gap)/ch;
    fy2 := (y2 - cy2 + Anchor[asTop   ].Gap)/ch;

    fInitialized := true
  end
  else
    fInitialized := false;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DFloatContainer.AdjustToRealSize;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  StartProfiler;

  x2 := x1 + RealWidth;
  y2 := y1 + RealHeight;
  fInitialized := true;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DFloatContainer.FloatToInteger;
var Ratio: float;
begin
  StartProfiler;

  GetAnchors;

  if cValid then begin
    Opacity := BaseOpacity * co;

    x1 := cx1 + Round(cw * fx1) + Anchor[asLeft].Gap;
    y1 := cy1 + Round(ch * fy1) + Anchor[asBottom].Gap;
    if ScaleItem then begin
      x2 := cx1 + Round(cw * fx2) - Anchor[asRight].Gap;
      y2 := cy1 + Round(ch * fy2) - Anchor[asTop].Gap;
      {inefficient}
      case ProportionalScale of
        psWidth:  begin
                    if RealHeight = 0 then Exit;
                    Ratio := RealWidth/RealHeight;
                    x2 := x1 + Round(h*Ratio);
                  end;
        psHeight: begin
                    if RealWidth = 0 then Exit;
                    Ratio := RealHeight/RealWidth;
                    y2 := y1 + Round(w*Ratio);
                  end;
      end;
    end
    else AdjustToRealSize; {sets x2,y2,w,h}
    fInitialized := true
  end
  else
    fInitialized := false;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DFloatContainer.SetFloatCoord(const afx1,afy1,afx2,afy2: float);
begin
  StartProfiler;

  fx1 := afx1;
  fy1 := afy1;
  fx2 := afx2;
  fy2 := afy2;

  FloatToInteger;

  StopProfiler;
end;
procedure DFloatContainer.SetFloatFull(const  afx1,afy1,afx2,afy2,aOpacity: float);
begin
  StartProfiler;

  BaseOpacity := aOpacity;
  SetFloatCoord(afx1,afy1,afx2,afy2);

  StopProfiler;
end;
procedure DFloatContainer.SetFloatSize(const afx1,afy1,afWidth,afHeight: float);
begin
  StartProfiler;

  fx1 := afx1;
  fy1 := afy1;
  fx2 := afx1+afWidth;//- (1 - afWidth - afx1);
  fy2 := afy1+afHeight;//- (1 - afHeight - afy1);

  FloatToInteger;

  StopProfiler;
end;
procedure DFloatContainer.SetFloatSizeFull(const afx1,afy1,afWidth,afHeight,aOpacity: float);
begin
  StartProfiler;

  BaseOpacity := aOpacity;
  SetFloatSize(afx1,afy1,afWidth,afHeight);

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DFloatContainer.SetIntWidthHeight(const aWidth,aHeight: integer);
begin
  StartProfiler;

  {not sure about this}
  SetIntSize(x1,y1,aWidth,aHeight);

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DFloatContainer.ResetToReal;
begin
  StartProfiler;

  SetIntWidthHeight(RealWidth,RealHeight);

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DFloatContainer.SetRealSize(const aWidth,aHeight: integer);
begin
  StartProfiler;

  RealWidth := aWidth;
  RealHeight := aHeight;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DFloatContainer.AnchorTo(const aParent: DFloatContainer; const Gap: integer = 0);
begin
  StartProfiler;

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
  OpacityAnchor := aParent;
  aParent.Owner.AddAnchor(Self.Owner);

  StopProfiler;
end;
procedure DFloatContainer.AnchorChild(const aChild: DFloatContainer; const Gap: integer = 0);
begin
  StartProfiler;

  aChild.Anchor[asLeft  ].Anchor := Self;
  aChild.Anchor[asLeft  ].Gap := Gap;
  aChild.Anchor[asLeft  ].AlignTo := haLeft;
  aChild.Anchor[asRight ].Anchor := Self;
  aChild.Anchor[asRight ].Gap := Gap;
  aChild.Anchor[asRight ].AlignTo := haRight;
  aChild.Anchor[asTop   ].Anchor := Self;
  aChild.Anchor[asTop   ].Gap := Gap;
  aChild.Anchor[asTop   ].AlignTo := vaTop;
  aChild.Anchor[asBottom].Anchor := Self;
  aChild.Anchor[asBottom].Gap := Gap;
  aChild.Anchor[asBottom].AlignTo := vaBottom;
  aChild.OpacityAnchor := Self;
  Self.Owner.AddAnchor(aChild.Owner);

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DFloatContainer.AnchorSide(const aSide: TAnchorSide; const aParent: DFloatContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
begin
  StartProfiler;

  Anchor[aSide].Anchor := aParent;
  Anchor[aSide].AlignTo := aAlign;
  Anchor[aSide].Gap := Gap;
  aParent.Owner.AddAnchor(Self.Owner);

  StopProfiler;
end;

procedure DFloatContainer.AnchorTop(const aParent: DFloatContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
begin
  StartProfiler;

  AnchorSide(asTop,aParent,aAlign,Gap);

  StopProfiler;
end;
procedure DFloatContainer.AnchorBottom(const aParent: DFloatContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
begin
  StartProfiler;

  AnchorSide(asBottom,aParent,aAlign,Gap);

  StopProfiler;
end;
procedure DFloatContainer.AnchorLeft(const aParent: DFloatContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
begin
  StartProfiler;

  AnchorSide(asLeft,aParent,aAlign,Gap);

  StopProfiler;
end;
procedure DFloatContainer.AnchorRight(const aParent: DFloatContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
begin
  StartProfiler;

  AnchorSide(asRight,aParent,aAlign,Gap);

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DFloatContainer.Assign(const Source: DFloatContainer);
var aa: TAnchorSide;
begin
  StartProfiler;

  Self.x1 := Source.x1;
  Self.y1 := Source.y1;
  Self.x2 := Source.x2;
  Self.y2 := Source.y2;
  Self.Opacity := Source.Opacity;

  Self.fx1 := Source.fx1;
  Self.fy1 := Source.fy1;
  Self.fx2 := Source.fx2;
  Self.fy2 := Source.fy2;
  Self.ScaleItem := Source.ScaleItem;
  Self.RealWidth := Source.RealWidth;
  Self.RealHeight := Source.RealHeight;
  Self.BaseOpacity := Source.BaseOpacity;
  Self.ProportionalScale := Source.ProportionalScale;
  Self.fInitialized := Source.isInitialized;
  Self.AnchorToWindow := Source.AnchorToWindow;
  for aa in TAnchorSide do
    Self.Anchor[aa] := Source.Anchor[aa];
  Self.OpacityAnchor := Source.OpacityAnchor;
  if Self.Owner <> Source.Owner then begin
    Log(LogInterfaceScaleError, _CurrentRoutine, 'WARNING: NotifyAnchor should be copied, do it!');
  end;

  StopProfiler;
end;
procedure DFloatContainer.AssignTo(const Dest: DFloatContainer);
var aa: TAnchorSide;
begin
  StartProfiler;

  Dest.x1 := Self.x1;
  Dest.y1 := Self.y1;
  Dest.x2 := Self.x2;
  Dest.y2 := Self.y2;
  Dest.Opacity := Self.Opacity;

  Dest.fx1 := Self.fx1;
  Dest.fy1 := Self.fy1;
  Dest.fx2 := Self.fx2;
  Dest.fy2 := Self.fy2;
  Dest.ScaleItem := Self.ScaleItem;
  Dest.RealWidth := Self.RealWidth;
  Dest.RealHeight := Self.RealHeight;
  Dest.BaseOpacity := Self.BaseOpacity;
  Dest.ProportionalScale := Self.ProportionalScale;
  Dest.fInitialized := Self.isInitialized;
  Dest.AnchorToWindow := Self.AnchorToWindow;
  for aa in TAnchorSide do
    Dest.Anchor[aa] := Self.Anchor[aa];
  Dest.OpacityAnchor := Self.OpacityAnchor;
  if Dest.Owner <> Self.Owner then begin
    Log(LogInterfaceScaleError, _CurrentRoutine,'WARNING: NotifyAnchor should be copied, do it!');
  end;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

function DFloatContainer.AnchorsEqual(const aCompare: DFloatContainer): boolean;
begin
  StartProfiler;

  { we don't care to which part of Parent we Anchor?
    Or should we? }
  if (AnchorToWindow          = aCompare.AnchorToWindow) and
     (Anchor[asLeft  ].Anchor = aCompare.Anchor[asLeft  ].Anchor) and
     (Anchor[asRight ].Anchor = aCompare.Anchor[asRight ].Anchor) and
     (Anchor[asTop   ].Anchor = aCompare.Anchor[asTop   ].Anchor) and
     (Anchor[asBottom].Anchor = aCompare.Anchor[asBottom].Anchor) and
     (OpacityAnchor           = aCompare.OpacityAnchor) then
    Result := true
  else
    Result := false;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

function DFloatContainer.GetInitialized: boolean;
begin
  StartProfiler;

  if fInitialized then begin
    {check if the x1,x2,y1,y2 are initialized / the wrong place to }
    if (x1 = UninitializedIntCoord) or
       (y1 = UninitializedIntCoord) or
       (x2 = UninitializedIntCoord) or
       (y2 = UninitializedIntCoord) then fInitialized := false;
  end;
  Result := fInitialized;

  StopProfiler;
end;

{============================================================================}
{======================== ABSTRACT ELEMENT ==================================}
{============================================================================}

{function DAbstractElement.ProcessRescaleResult(var r1: TRescaleResult; const r2: TRescaleResult): TRescaleResult;
begin
  StartProfiler;

  if (r1 = rrInvalid) or (r2 = rrInvalid) then Result := rrInvalid else
  if (r1 = rrDirty  ) or (r2 = rrDirty  ) then Result := rrDirty   else
                                               Result := rrOk;

  StopProfiler;
end;}

{----------------------------------------------------------------------------}

function DAbstractElement.RescaleContainer: boolean;
var TmpSize: Txy;
begin
  StartProfiler;

  {ugly/temporary fix to check if container size has changed}
  TmpSize.x1 := Base.x1;
  TmpSize.x2 := Base.x2;
  TmpSize.y1 := Base.y1;
  TmpSize.y2 := Base.y2;

  {set animation states to changed container size}
  Base.FloatToInteger;

  if not Base.isInitialized then
    Log(LogInterfaceInfo,_CurrentRoutine,'Base is uninitialized in Rescale');

  {check if container size has been actually changed}
  if (Base.isInitialized) then
    Result := (TmpSize.x1 <> Base.x1) or (TmpSize.x2 <> Base.x2) or
              (TmpSize.y1 <> Base.y1) or (TmpSize.y2 <> Base.y2)
  else
    Result := false;
  {don't run anything related to container size changed, even if it did change
   Beware of bugs here! As the container size might remain the same, just
   Base became initialized.
   Actually this can't happen in a normal situation. But did anything ever went as planned?}

   {Rescale Last and Next if they're initialized or just copy Base to avoid bugs}
{   if Last.isInitialized then
     Last.FloatToInteger
   else
     Last.Assign(Base);}

   if Next.isInitialized then
     Next.FloatToInteger
   else
     Next.Assign(Base);

   {$WARNING All angors are attached to current, not to base!!!! Should notify all chidren of current has changed???}
   GetAnimationState; //Get Self.Current (required to scale Anchored elements accordingly!)

   StopProfiler;
end;

{----------------------------------------------------------------------------}

function DAbstractElement.RescaleContent: boolean;
begin
  StartProfiler;

  {at this abstract level it does nothing}
  Result := false;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.RescaleRecoursive;
begin
  StartProfiler;

  Rescale; //just rescale self and do nothing more

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.Rescale;
var ContainerSizeChanged: boolean;
begin
  StartProfiler;

  ContainerSizeChanged := RescaleContainer;
  ContainerSizeChanged := RescaleContent or ContainerSizeChanged;

  //notify anchored elements to this one, that this one has rescaled
  //if something has changed to avoid cyclic
  if (ContainerSizeChanged) then
    NotifyRescale;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.SetVisible(const Value: boolean);
begin
  StartProfiler;

  fVisible := Value;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.AnimateTo(const Animate: TAnimationStyle; const Duration: float = DefaultAnimationDuration);
var mx,my: float;
begin
  StartProfiler;

  begin
    GetAnimationState;
    Last.Assign(Current);   //to current animation state
    {$WARNING Base may be "last" animation state, not "next"!}
    Next.Assign(Base);

    if Animate = asNone then Exit;

    AnimationStart := -1;
    AnimationDuration := Duration;

    case Animate of
      {just grabs some previous locations and animates the item from there to base
       requires that CurrentAnimationState is initialized... TODO}
      //asDefault: Last.Assign(Self);
      {fades in/out element}
      asFadeIn:  Last.Opacity := 0;
      asFadeOut: Next.BaseOpacity := 0;
      {asFadeOutSuicide}
      {zooms in/out element}
      asZoomIn:  begin
                   {$WARNING not working}
                   //Last.SetIntWidthHeight(1,1);
                   Last.Opacity := 0;
                 end;
      asZoomOut: begin
                   Next.SetIntWidthHeight(1,1);
                   Next.BaseOpacity := 0;
                 end;

      {asZoomOutSuicide}
      asFlyInRandom,asFlyOutRandom: begin
                       mx := DRND.Random;
                       my := DRND.Random;
                       case DRND.Random(4) of
                         0: mx :=  0.0001;
                         1: mx := -0.0001;
                         2: my :=  0.0001;
                         3: my := -0.0001;
                       end;
                       if Animate=asFlyInRandom then begin
                         {$HINT Test needed}
                         Last.x1 := Round(mx*Window.Width);
                         Last.y1 := Round(my*Window.Width);
                         //Last.AnchorToWindow := true;
                       end else begin
                         Next.fx1 := mx;
                         Next.fy1 := my;
                         Next.AnchorToWindow := true;
                       end;
                     end;
      asFlyInTop,asFlyOutTop,asFlyInBottom,asFlyOutBottom,asFlyInLeft,asFlyOutLeft,asFlyInRight,asFlyOutRight: begin
                       mx := DRND.Random*Window.Width/Window.Height;
                       my := DRND.Random;
                       case animate of
                         asFlyInLeft,asFlyOutLeft: mx :=  0.0001;
                         asFlyInRight,asFlyOutRight: mx := -0.0001;
                         asFlyInBottom,asFlyOutBottom: my := 0.0001;
                         asFlyInTop,asFlyOutTop: my := -0.0001;
                       end;
                       if (Animate=asFlyInLeft) or (Animate=asFlyInRight) or (Animate=asFlyInTop) or (Animate=asFlyInBottom) then begin
                         {$HINT Test needed}
                         Last.x1 := Round(mx*Window.Width);
                         Last.y1 := Round(my*Window.Width);
                         //Last.AnchorToWindow := true;
                       end else begin
                         Next.fx1 := mx;
                         Next.fy1 := my;
                         Next.AnchorToWindow := true;
                       end;
                     end;
      {asFlyOutRandomSuicide}
    end;
  end;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.SetBaseSize(const NewX,NewY,NewW,NewH: float;NewO: float=1; const Animate: TAnimationStyle = asNone);
begin
  StartProfiler;

  Base.SetFloatSizeFull(NewX,NewY,NewW,NewH,NewO);
  AnimateTo(Animate);
  Rescale;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.AnchorTo(const aElement: DAbstractElement);
begin
  StartProfiler;

  {$WARNING not working}
  Base.AnchorTo(aElement.Base);

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.GetAnimationState;
var Phase: float;
begin
  StartProfiler;

  if Base.isInitialized then begin
    if AnimationStart<0 then AnimationStart := DecoNow;
    if {(Last.isInitialized) and} (Next.isInitialized) and
      ((DecoNow-AnimationStart < AnimationDuration)) then begin
      //if this is start of the animation - init time
      //determine animation time passed
      Phase := (DecoNow-AnimationStart)/AnimationDuration;
      //determine animation phase
      case AnimationCurve of
        //acLinear: ; //<- change nothing.
        acSquare: if Phase<0.5 then Phase := Sqr(2*Phase)/2 else Phase := 1 - Sqr(2*(1-Phase))/2;
      end;
      {$WARNING Not working}
      Current.Assign(Next);
      Current.SetIntFull(Last.x1+(Next.x1-Last.x1)*Phase,
                         Last.y1+(Next.y1-Last.y1)*Phase,
                         Last.x2+(Next.x2-Last.x2)*Phase,
                         Last.y2+(Next.y2-Last.y2)*Phase,
                         Last.Opacity+((Next.Opacity-Last.Opacity)*Phase));
      {during the animation, the Element is always scaled}
      //Current.ScaleItem := true;
      //we don't need scale back to float, as it's not a basic animation state
    end else begin
      Current.Assign(Base);
      Current.SetIntFull( Base.x1,
                          Base.y1,
                          Base.x2,
                          Base.y2,
                          Base.BaseOpacity);
    end;
  end else begin
    Current.Assign(Base); {just fall back to an uninitialized copy}
    Log(LogInterfaceInfo,_CurrentRoutine,'Base is uninitialized, falling back to Current=Base');
  end;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.Update;
begin
  StartProfiler;

  GetAnimationState;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.SetFullScreen;
begin
  StartProfiler;

  Base.AnchorToWindow := true;
  SetBaseSize(0,0,1,1);

  StopProfiler;
end;

{----------------------------------------------------------------------------}

function DAbstractElement.GetSize: Txy;
begin
  StartProfiler;

  Result.x1 := Base.x1;
  Result.x2 := Base.x2;
  Result.y1 := Base.y1;
  Result.y2 := Base.y2;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.AddAnchor(aAnchor: DAnchoredObject);
var a: DAnchoredObject;
begin
  StartProfiler;

  //check if the Anchor isn't already available in NotifyAnchors
  for a in NotifyAnchors do if a = aAnchor then Exit;
  NotifyAnchors.Add(aAnchor);

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.NotifyRescale;
var a: DAnchoredObject;
begin
  StartProfiler;

  for a in NotifyAnchors do a.Rescale;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

constructor DAbstractElement.Create;
begin
  StartProfiler;

  inherited Create;
  fVisible := true;
  AnimationCurve := acSquare;
  Base := DFloatContainer.Create(Self);
  Last := DFloatContainer.Create(Self);
  Next := DFloatContainer.Create(Self);
  Current := DFloatContainer.Create(Self);

  NotifyAnchors := TAnchorList.Create(false);

  StopProfiler;
end;

{----------------------------------------------------------------------------}

destructor DAbstractElement.Destroy;
begin
  StartProfiler;

  FreeAndNil(Base);
  FreeAndNil(Last);
  FreeAndNil(Next);
  FreeAndNil(Current);

  FreeAndNil(NotifyAnchors);
  inherited Destroy;

  StopProfiler;
end;

{=============================================================================}
{=============== Single interface element & Timer ============================}
{=============================================================================}

constructor DTimer.Create;
begin
  StartProfiler;

  inherited Create;
  Enabled := false;
  StartTime := -1;

  StopProfiler;
end;

{-----------------------------------------------------------------------------}

procedure DTimer.Update;
begin
  StartProfiler;

  if StartTime<0 then StartTime := DecoNow else
  if (DecoNow-StartTime) >= Interval then begin
    Enabled := false;
    if Assigned(onTimer) then onTimer;
  end;

  StopProfiler;
end;

{-----------------------------------------------------------------------------}

procedure DTimer.SetTimeout(const Seconds: DTime);
begin
  StartProfiler;

  StartTime := -1;
  Enabled := true;
  Interval := Seconds;

  StopProfiler;
end;

{============================================================================}

constructor DSingleInterfaceElement.Create;
begin
  StartProfiler;

  inherited Create;
  isMouseOver := false;
  CanMouseOver := false;
  CanDrag := false;

  StopProfiler;
end;

{-----------------------------------------------------------------------------}

destructor DSingleInterfaceElement.Destroy;
begin
  StartProfiler;

  FreeAndNil(Timer);
  inherited Destroy;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.SetTimeOut(const Seconds: DTime);
begin
  StartProfiler;

  if Timer = nil then Timer := DTimer.Create;
  Timer.SetTimeOut(Seconds);

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.Update;
begin
  StartProfiler;

  inherited Update;
  if (Timer<>nil) and (Timer.Enabled) then Timer.Update;

  StopProfiler;
end;

{==============================  Mouse handling =============================}

function DSingleInterfaceElement.IAmHere(const xx,yy: integer): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  StartProfiler;

  if (xx >= Base.x1) and (xx <= Base.x2) and
     (yy >= Base.y1) and (yy <= Base.y2)
  then
    Result := true
  else
    Result := false;

  StopProfiler;
end;

{-----------------------------------------------------------------------------}

function DSingleInterfaceElement.ifMouseOver(const xx,yy: integer; const RaiseEvents: boolean; const AllTree: boolean): DAbstractElement;
begin
  StartProfiler;

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

  StopProfiler;
end;

function DInterfaceElement.ifMouseOver(const xx,yy: integer; const RaiseEvents: boolean; const AllTree: boolean): DAbstractElement;
var i: integer;
    tmpLink: DAbstractElement;
begin
  StartProfiler;

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

  StopProfiler;
end;

{-----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.StartDrag(const xx,yy: integer);
begin
  StartProfiler;

  DragX := Base.x1 - xx;
  DragY := Base.y1 - yy;

  StopProfiler;
end;
procedure DSingleInterfaceElement.Drag(const xx,yy: integer);
begin
  StartProfiler;

  Base.x1 := DragX + xx;
  Base.y1 := DragY + yy;

  StopProfiler;
end;

{=============================================================================}
{=========================== interface element ===============================}
{=============================================================================}

constructor DInterfaceElement.Create;
begin
  StartProfiler;

  inherited Create;
  Children := DInterfaceElementsList.Create(true);

  StopProfiler;
end;

{----------------------------------------------------------------------------}

destructor DInterfaceElement.Destroy;
begin
  StartProfiler;

  //this should fire as recoursive because children owns elements, which in turn will fire their destructors onfree
  FreeAndNil(Children);
  inherited Destroy;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.RescaleRecoursive;
var i: integer;
begin
  StartProfiler;

  inherited RescaleRecoursive;
  for i := 0 to Children.Count-1 do Children[i].RescaleRecoursive;

  StopProfiler;
end;

{-----------------------------------------------------------------------------}

function DInterfaceElement.GetSize: Txy;
var tx1,tx2,ty1,ty2: integer;
    tmp: Txy;
    c: DAbstractElement;
begin
  StartProfiler;

  //inherited <------- no inheritance, this procedure works a bit differently
  tx1 := Base.x1;
  tx2 := Base.x2;
  ty1 := Base.y1;
  ty2 := Base.y2;

  //scan children
  for c in Self.Children do begin
    tmp := c.GetSize;
    if tx1 > tmp.x1 then tx1 := tmp.x1;
    if tx2 < tmp.x2 then tx2 := tmp.x2;
    if ty1 > tmp.y1 then ty1 := tmp.y1;
    if ty2 < tmp.y2 then ty2 := tmp.y2;
  end;

  Result.x1 := tx1;
  Result.x2 := tx2;
  Result.y1 := ty1;
  Result.y2 := ty2;

  StopProfiler;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceElement.RescaleToChildren;
var TrueSize: Txy;
begin
  StartProfiler;

  {This procedure is basic, however, there's no need to move it "up" by inheritance level
   as it's called only for elements containing children}
  TrueSize := Self.GetSize;
  log(LogInterfaceScaleHint,'x1=',IntToStr(TrueSize.x1)+'/'+IntToStr(Base.x1));
  log(LogInterfaceScaleHint,'x2=',IntToStr(TrueSize.x2)+'/'+IntToStr(Base.x2));
  log(LogInterfaceScaleHint,'y1=',IntToStr(TrueSize.y1)+'/'+IntToStr(Base.y1));
  log(LogInterfaceScaleHint,'y2=',IntToStr(TrueSize.y2)+'/'+IntToStr(Base.y2));

  //call rescale only in case something has changed and if container size is "valid" (non-zero)
  if  (TrueSize.x2 <> TrueSize.x1) and (TrueSize.y2 <> TrueSize.y1) and
     ((TrueSize.x1 < Self.Base.x1) or (TrueSize.x2 > Self.Base.x2) or
      (TrueSize.y1 < Self.Base.y1) or (TrueSize.y2 > Self.Base.y2)) then
  begin
    Base.SetIntCoord(TrueSize.x1,TrueSize.x2,TrueSize.y1,TrueSize.y2);
    Log(LogInterfaceScaleHint,_CurrentRoutine,'Backward-rescaling to Children.');
    //if Base.isInitialized then Rescale;
  end;

  StopProfiler;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceElement.Draw;
var i: integer;
begin
  StartProfiler;

  //inherited Draw; <---------- parent is abstract
  if isVisible then begin
    Update;
    for i := 0 to Children.Count-1 do Children[i].Draw;
  end;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Grab(const Child: DSingleInterfaceElement);
begin
  StartProfiler;

  Children.Add(Child);
  if (Child is DSingleInterfaceElement) then DSingleInterfaceElement(Child).Parent := Self;
  //{Child.ID := }InterfaceList.Add(Child); //global ID of the element

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Clear;
begin
  StartProfiler;

  Children.Clear;

  StopProfiler;
end;

{-----------------------------------------------------------------------}

function DInterfaceElement.MouseOverTree(const xx,yy: integer): boolean;
var tmp: DAbstractElement;
begin
  StartProfiler;

  // maybe rewrite it using isMouseOver - the idea is still a little different
  tmp := Self.ifMouseOver(xx,yy,false,false);
  if (tmp <> nil) and (tmp is DSingleInterfaceElement) and (DSingleInterfaceElement(tmp).CanMouseOver){ and (tmp.base.opacity>0)} then
    isMouseOverTree := true
  else
    isMouseOverTree := false;
  //base.opacity breaks the algorithm, if transparent item is above (i.e. below) the opaque element
  Result := isMouseOverTree;

  StopProfiler;
end;

end.

