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

type Txy = record
  x1,y1,x2,y2: integer;
end;

type
  { This is an container with coordinates and size
    Capable of rescaling / copying itself
    As abstract as it seems, it's also used as animation state,
    So, constructing it as standalone should be possible }
  DAbstractContainer = class(DObject)
  strict private
    { Owner of this Container (for displaying debug info) }
    Owner: DObject;
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
    procedure AdjustToRealSize;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

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
        Anchor: DAbstractContainer;
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
    OpacityAnchor: DAbstractContainer;
    { Is this Container scaled agains Anchors or Window?
      Should be True only at top-level Container (i.e. GUI Container)
      However, maybe, reintroduction or manual scaling would be prefferable? }
    AnchorToWindow: boolean;
    { Float size of the Container }
    fx1,fy1,fx2,fy2{,fw,fh}: float;
    { Real size of the Container }
    x1,y1,x2,y2: integer;
    { Base value of the opacity of the container }
    BaseOpacity: float;
    { Opacity of the container multiplied by Anchor's opacity }
    CurrentOpacity: float;
    { Keep proportions of the container }
    RealWidth, RealHeight: integer;
    { Can this item be scaled? Otherwise its w, h is always RealWidth, RealHeight}
    ScaleItem: boolean;
    { Should this Container scale proportionaly? }
    ProportionalScale: TProportionalScale;

    procedure SetWidth(const aWidth: integer);
    procedure SetHeight(const aHeight: integer);
    function GetWidth: integer;
    function GetHeight: integer;
    property w: integer read GetWidth write SetWidth;
    property h: integer read GetHeight write SetHeight;
    { If this Container ready to be used? }
    property isInitialized: boolean read GetInitialized;
    constructor Create(aOwner: DObject); //virtual;
    //destructor Destroy; override;
    { Copy parameters from the Source }
    procedure Assign(const Source: DAbstractContainer);
    procedure AssignTo(const Dest: DAbstractContainer);
    { Set container position and size }
    procedure SetFloatCoord(const afx1,afy1,afx2,afy2: float);
    procedure SetFloatFull(const afx1,afy1,afx2,afy2,aOpacity: float);
    procedure SetFloatSize(const afx1,afy1,afWidth,afHeight: float);
    procedure SetFloatSizeFull(const afx1,afy1,afWidth,afHeight,aOpacity: float);
    procedure SetIntCoord(const ax1,ay1,ax2,ay2: integer);
    procedure SetIntFull(const ax1,ay1,ax2,ay2: integer; const aOpacity: float);
    procedure SetIntSize(const ax1,ay1,aWidth,aHeight: integer);

    { Sets int width/height for scaling animations }
    procedure SetIntWidthHeight(const aWidth,aHeight: integer);
    { Resets width and height to their "real" values, e.g. for elements that are not scaled }
    procedure ResetToReal;
    procedure SetRealSize(const aWidth,aHeight: integer);
    { Anchors this Container to aParent }
    procedure AnchorTo(const aParent: DAbstractContainer; const Gap: integer = 0);
    { Anchors aChild to this Container  }
    procedure AnchorChild(const aChild: DAbstractContainer; const Gap: integer = 0);
    {}
    procedure AnchorSide(const aSide: TAnchorSide; const aParent: DAbstractContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
    procedure AnchorTop(const aParent: DAbstractContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
    procedure AnchorBottom(const aParent: DAbstractContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
    procedure AnchorLeft(const aParent: DAbstractContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
    procedure AnchorRight(const aParent: DAbstractContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
    { do these two Containers have equal anchors? }
    function AnchorsEqual(const aCompare: DAbstractContainer): boolean;
  end;

type
  { Style of the animation.
    asLinear is just linear interpolation,
    asSquare is slower in the beginning and end, and faster in the middle}
  TAnimationCurve = (acsLinear, acSquare);

type
  {}
  TRescaleResult = (rrOk, rrDirty, rrInvalid);

Type
  { most abstract container for interface elements
    Defines size, scaling and animation state }
  DAbstractElement = class abstract(DThreadedObject)
  strict protected
    { Caches current animation state, recalculated by GetAnimationState at every render}
    procedure GetAnimationState;
    { updates the data of the class with current external data,
      here it just gets the current animation state }
    procedure Update; virtual;
    { Sets this interface element size to full screen and aligns it to Window }
    procedure SetFullScreen;
    { Returns true size of this interface element }
    function GetSize: Txy; virtual;
  public
    {}
    function ProcessRescaleResult(var r1: TRescaleResult; const r2: TRescaleResult): TRescaleResult;
    { changes the scale of the element relative to current window size }
    procedure Rescale; virtual;
    { draw the element / as abstract as it might be :) }
    procedure Draw; virtual; abstract;
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
    constructor Create; override;
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

type DInterfaceElementsList = specialize TFPGObjectList<DSingleInterfaceElement>;

Type
  { An interface element, that can contain "Children" }
  DInterfaceElement = class(DSingleInterfaceElement)
  strict protected
    { Returns true size of this interface element and all of its children }
    function GetSize: Txy; override;
  public
    { List of the children of this interface element }
    Children: DInterfaceElementsList;
    procedure Draw; override;
    procedure Rescale; override;
    { Rescales this Interface element to fit children sizes
      (should be used only in case children may have fixed sizes (like text labels)
       or force-change their size, e.g. if they don't allow size smaller
       than frame gaps)}
    procedure RescaleToChildren;
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
  DecoLog;

{=============================================================================}
{========================== Abstract Container ===============================}
{=============================================================================}

constructor DAbstractContainer.Create(aOwner: DObject);
var aa: TAnchorSide;
begin
  //inherited Create;
  Owner := aOwner;

  x1 := UninitializedIntCoord;
  x2 := UninitializedIntCoord;
  y1 := UninitializedIntCoord;
  y2 := UninitializedIntCoord;

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
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.GetWindowAnchor;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  cx1 := 0;
  cy1 := 0;
  cx2 := Window.Width;
  cy2 := Window.Height;
end;

procedure DAbstractContainer.GetAnchors;
begin
  if AnchorToWindow then
    GetWindowAnchor
  else begin
    if (Anchor[asLeft].Anchor = nil) or
       (Anchor[asTop].Anchor = nil) or
       (Anchor[asRight].Anchor = nil) or
       (Anchor[asBottom].Anchor = nil) then begin
         fLog(LogInterfaceError,Owner.ClassName+'>'+{$I %CURRENTROUTINE%},'Anchor is Nil!');
         GetWindowAnchor;
    end else begin
      case Anchor[asLeft].AlignTo of
        haLeft:   cx1 := Anchor[asLeft].Anchor.x1;
        haRight:  cx1 := Anchor[asLeft].Anchor.x2;
        haCenter: cx1 := (Anchor[asLeft].Anchor.x1 + Anchor[asLeft].Anchor.x2) div 2;
        else fLog(LogInterfaceError,Owner.ClassName+'>'+{$I %CURRENTROUTINE%},'Invalid Anchor align!')
      end;
      case Anchor[asRight].AlignTo of
        haLeft:   cx2 := Anchor[asRight].Anchor.x1;
        haRight:  cx2 := Anchor[asRight].Anchor.x2;
        haCenter: cx2 := (Anchor[asRight].Anchor.x1 + Anchor[asRight].Anchor.x2) div 2;
        else fLog(LogInterfaceError,Owner.ClassName+'>'+{$I %CURRENTROUTINE%},'Invalid Anchor align!')
      end;
      case Anchor[asBottom].AlignTo of                {Pay attention, this is INVERT due to OpenGL}
        vaBottom: cy1 := Anchor[asBottom].Anchor.y1;
        vaTop:    cy1 := Anchor[asBottom].Anchor.y2;
        vaMiddle: cy1 := (Anchor[asBottom].Anchor.y1 + Anchor[asBottom].Anchor.y2) div 2;
        else fLog(LogInterfaceError,Owner.ClassName+'>'+{$I %CURRENTROUTINE%},'Invalid Anchor align!')
      end;
      case Anchor[asTop].AlignTo of             {Pay attention, this is INVERT due to OpenGL}
        vaBottom: cy2 := Anchor[asTop].Anchor.y1;
        vaTop:    cy2 := Anchor[asTop].Anchor.y2;
        vaMiddle: cy2 := (Anchor[asTop].Anchor.y1 + Anchor[asTop].Anchor.y2) div 2;
        else fLog(LogInterfaceError,Owner.ClassName+'>'+{$I %CURRENTROUTINE%},'Invalid Anchor align!')
      end;
    end;  
  end;
  if (OpacityAnchor <> nil) then
    co := OpacityAnchor.CurrentOpacity
  else
    co := 1;

  cw := cx2-cx1;
  ch := cy2-cy1;
  if (cw > 0) and (ch > 0) then cValid := true else cValid := false;
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.IntegerToFloat;
begin
  GetAnchors;

  if cValid then begin
    CurrentOpacity := BaseOpacity * co;

    fx1 := (x1 - cx1 - Anchor[asLeft  ].Gap)/cw;
    fx2 := (x2 - cx2 + Anchor[asRight ].Gap)/cw;
    fy1 := (y1 - cy1 - Anchor[asBottom].Gap)/ch;
    fy2 := (y2 - cy2 + Anchor[asTop   ].Gap)/ch;

    fInitialized := true
  end
  else
    fInitialized := false
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.AdjustToRealSize;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  x2 := x1 + RealWidth;
  y2 := y1 + RealHeight;
  fInitialized := true;
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.FloatToInteger;
var Ratio: float;
begin
  GetAnchors;

  if cValid then begin
    CurrentOpacity := BaseOpacity * co;

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
    fInitialized := false
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
procedure DAbstractContainer.SetFloatFull(const  afx1,afy1,afx2,afy2,aOpacity: float);
begin
  BaseOpacity := aOpacity;
  SetFloatCoord(afx1,afy1,afx2,afy2);
end;
procedure DAbstractContainer.SetFloatSize(const afx1,afy1,afWidth,afHeight: float);
begin
  fx1 := afx1;
  fy1 := afy1;
  fx2 := afx1+afWidth;//- (1 - afWidth - afx1);
  fy2 := afy1+afHeight;//- (1 - afHeight - afy1);

  FloatToInteger;
end;
procedure DAbstractContainer.SetFloatSizeFull(const afx1,afy1,afWidth,afHeight,aOpacity: float);
begin
  BaseOpacity := aOpacity;
  SetFloatSize(afx1,afy1,afWidth,afHeight);
end;
procedure DAbstractContainer.SetIntCoord(const ax1,ay1,ax2,ay2: integer);
begin
  x1 := ax1;
  y1 := ay1;
  if ScaleItem then begin
    x2 := ax2;
    y2 := ay2;
  end else AdjustToRealSize;

  IntegerToFloat;
end;
procedure DAbstractContainer.SetIntFull(const ax1,ay1,ax2,ay2: integer; const aOpacity: float);
begin
  BaseOpacity := aOpacity;
  SetIntCoord(ax1,ay1,ax2,ay2);
end;
procedure DAbstractContainer.SetIntSize(const ax1,ay1,aWidth,aHeight: integer);
begin
  SetIntCoord(ax1,ay1,ax1 + aWidth,ay1 + aHeight);
end;

procedure DAbstractContainer.SetWidth(const aWidth: integer);
begin
  {unoptimal and might be wrong}
  SetIntCoord(x1,y1,x1+aWidth,y2);
end;
procedure DAbstractContainer.SetHeight(const aHeight: integer);
begin
  {unoptimal and might be wrong}
  SetIntCoord(x1,y1,x2,y1+aHeight);
end;

{----------------------------------------------------------------------------}

function DAbstractContainer.GetWidth: integer;
begin
  Result := x2-x1;
end;
function DAbstractContainer.GetHeight: integer;
begin
  Result := y2-y1;
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.SetIntWidthHeight(const aWidth,aHeight: integer);
begin
  {not sure about this}
  SetIntSize(x1,y1,aWidth,aHeight);
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.ResetToReal;
begin
  SetIntWidthHeight(RealWidth,RealHeight);
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
  OpacityAnchor := aParent;
end;
procedure DAbstractContainer.AnchorChild(const aChild: DAbstractContainer; const Gap: integer = 0);
begin
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
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.AnchorSide(const aSide: TAnchorSide; const aParent: DAbstractContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
begin
  Anchor[aSide].Anchor := aParent;
  Anchor[aSide].AlignTo := aAlign;
  Anchor[aSide].Gap := Gap;
end;

procedure DAbstractContainer.AnchorTop(const aParent: DAbstractContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
begin
  AnchorSide(asTop,aParent,aAlign,Gap);
end;
procedure DAbstractContainer.AnchorBottom(const aParent: DAbstractContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
begin
  AnchorSide(asBottom,aParent,aAlign,Gap);
end;
procedure DAbstractContainer.AnchorLeft(const aParent: DAbstractContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
begin
  AnchorSide(asLeft,aParent,aAlign,Gap);
end;
procedure DAbstractContainer.AnchorRight(const aParent: DAbstractContainer; const aAlign: TAnchorAlign; const Gap: integer = 0);
begin
  AnchorSide(asRight,aParent,aAlign,Gap);
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.Assign(const Source: DAbstractContainer);
var aa: TAnchorSide;
begin
  Self.fx1 := Source.fx1;
  Self.fy1 := Source.fy1;
  Self.fx2 := Source.fx2;
  Self.fy2 := Source.fy2;
  Self.x1 := Source.x1;
  Self.y1 := Source.y1;
  Self.x2 := Source.x2;
  Self.y2 := Source.y2;
  Self.ScaleItem := Source.ScaleItem;
  Self.RealWidth := Source.RealWidth;
  Self.RealHeight := Source.RealHeight;
  Self.BaseOpacity := Source.BaseOpacity;
  Self.CurrentOpacity := Source.CurrentOpacity;
  Self.ProportionalScale := Source.ProportionalScale;
  Self.fInitialized := Source.isInitialized;
  Self.AnchorToWindow := Source.AnchorToWindow;
  for aa in TAnchorSide do
    Self.Anchor[aa] := Source.Anchor[aa];
  Self.OpacityAnchor := Source.OpacityAnchor;
end;
procedure DAbstractContainer.AssignTo(const Dest: DAbstractContainer);
var aa: TAnchorSide;
begin
  Dest.fx1 := Self.fx1;
  Dest.fy1 := Self.fy1;
  Dest.fx2 := Self.fx2;
  Dest.fy2 := Self.fy2;
  Dest.x1 := Self.x1;
  Dest.y1 := Self.y1;
  Dest.x2 := Self.x2;
  Dest.y2 := Self.y2;
  Dest.ScaleItem := Self.ScaleItem;
  Dest.RealWidth := Self.RealWidth;
  Dest.RealHeight := Self.RealHeight;
  Dest.BaseOpacity := Self.BaseOpacity;
  Dest.CurrentOpacity := Self.CurrentOpacity;
  Dest.ProportionalScale := Self.ProportionalScale;
  Dest.fInitialized := Self.isInitialized;
  Dest.AnchorToWindow := Self.AnchorToWindow;
  for aa in TAnchorSide do
    Dest.Anchor[aa] := Self.Anchor[aa];
  Dest.OpacityAnchor := Self.OpacityAnchor;
end;

{----------------------------------------------------------------------------}

function DAbstractContainer.AnchorsEqual(const aCompare: DAbstractContainer): boolean;
begin
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
end;

{----------------------------------------------------------------------------}

function DAbstractContainer.GetInitialized: boolean;
begin
  if fInitialized then begin
    {check if the x1,x2,y1,y2 are initialized / the wrong place to }
    if (x1 = UninitializedIntCoord) or
       (y1 = UninitializedIntCoord) or
       (x2 = UninitializedIntCoord) or
       (y2 = UninitializedIntCoord) then fInitialized := false;
  end;
  Result := fInitialized;
end;

{============================================================================}
{======================== ABSTRACT ELEMENT ==================================}
{============================================================================}

function DAbstractElement.ProcessRescaleResult(var r1: TRescaleResult; const r2: TRescaleResult): TRescaleResult;
begin
  if (r1 = rrInvalid) or (r2 = rrInvalid) then Result := rrInvalid else
  if (r1 = rrDirty  ) or (r2 = rrDirty  ) then Result := rrDirty   else
                                               Result := rrOk;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.Rescale;
begin
  {set animation states to changed container size}
  Base.FloatToInteger;

  if not Base.isInitialized then
    Log(LogInterfaceInfo,{$I %CURRENTROUTINE%},'Base is uninitialized in Rescale');

  {Rescale Last and Next if they're initialized or just copy Base to avoid bugs}
  if Last.isInitialized then
    Last.FloatToInteger
  else
    Last.Assign(Base);

  if Next.isInitialized then
    Next.FloatToInteger
  else
    Next.Assign(Base);

  GetAnimationState; //Get Self.Current (required to scale Anchored elements accordingly!)
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
      asFadeIn:  Last.BaseOpacity := 0;
      asFadeOut: Next.BaseOpacity := 0;
      {asFadeOutSuicide}
      {zooms in/out element}
      asZoomIn:  begin
                   Last.SetIntWidthHeight(1,1);
                   Last.BaseOpacity := 0;
                 end;
      asZoomOut: begin
                   Next.SetIntWidthHeight(1,1);
                   Next.BaseOpacity := 0;
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
                         Last.AnchorToWindow := true;
                       end else begin
                         Next.fx2 := mx;
                         Next.fy2 := my;
                         Next.AnchorToWindow := true;
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
                         Last.AnchorToWindow := true;
                       end else begin
                         Next.fx2 := mx;
                         Next.fy2 := my;
                         Next.AnchorToWindow := true;
                       end;
                     end;
      {asFlyOutRandomSuicide}
    end;
  end;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.SetBaseSize(const NewX,NewY,NewW,NewH: float;NewO: float=1; const Animate: TAnimationStyle = asNone);
begin
  Base.SetFloatSizeFull(NewX,NewY,NewW,NewH,NewO);
  AnimateTo(Animate);
  Rescale;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.AnchorTo(const aElement: DAbstractElement);
begin
  Base.AnchorTo(aElement.Current);
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.GetAnimationState;
var Phase: float;
begin
  if Base.isInitialized then begin
    if AnimationStart<0 then AnimationStart := DecoNow;
    if (Last.isInitialized) and (Next.isInitialized) and
      ((DecoNow-AnimationStart < AnimationDuration)) then begin
      //if this is start of the animation - init time
      //determine animation time passed
      Phase := (DecoNow-AnimationStart)/AnimationDuration;
      //determine animation phase
      case AnimationCurve of
        //acLinear: ; //<- change nothing.
        acSquare: if Phase<0.5 then Phase := Sqr(2*Phase)/2 else Phase := 1 - Sqr(2*(1-Phase))/2;
      end;
      Current.Assign(Next);
      Current.SetFloatFull( Last.fx1+(Next.fx1-Last.fx1)*Phase,
                            Last.fy1+(Next.fy1-Last.fy1)*Phase,
                            Last.fx2+(Next.fx2-Last.fx2)*Phase,
                            Last.fy2+(Next.fy2-Last.fy2)*Phase,
                            Last.BaseOpacity+((Next.BaseOpacity-Last.BaseOpacity)*Phase));
      {during the animation, the Element is always scaled}
      Current.ScaleItem := true;
      //we don't need scale back to float, as it's not a basic animation state
    end else begin
      Current.Assign(Base);
      Current.SetFloatFull( Base.fx1,
                            Base.fy1,
                            Base.fx2,
                            Base.fy2,
                            Base.BaseOpacity);
    end;
 end else begin
   Current.Assign(Base); {just fall back to an uninitialized copy}
   Log(LogInterfaceInfo,{$I %CURRENTROUTINE%},'Base is uninitialized, falling back to Current=Base');
 end;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.Update;
begin
  GetAnimationState;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.SetFullScreen;
begin
  Base.AnchorToWindow := true;
  SetBaseSize(0,0,1,1);
end;

{----------------------------------------------------------------------------}

function DAbstractElement.GetSize: Txy;
begin
  Result.x1 := Base.x1;
  Result.x2 := Base.x2;
  Result.y1 := Base.y1;
  Result.y2 := Base.y2;
end;

{----------------------------------------------------------------------------}

constructor DAbstractElement.Create;
begin
  inherited Create;
  fVisible := true;
  AnimationCurve := acSquare;
  Base := DAbstractContainer.Create(Self);
  Last := DAbstractContainer.Create(Self);
  Next := DAbstractContainer.Create(Self);
  Current := DAbstractContainer.Create(Self);
end;

{----------------------------------------------------------------------------}

destructor DAbstractElement.Destroy;
begin
  FreeAndNil(Base);
  FreeAndNil(Last);
  FreeAndNil(Next);
  FreeAndNil(Current);
  inherited Destroy;
end;

{=============================================================================}
{=============== Single interface element & Timer ============================}
{=============================================================================}

constructor DTimer.Create;
begin
  inherited Create;
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
  inherited Create;
  isMouseOver := false;
  CanMouseOver := false;
  CanDrag := false;
end;

{-----------------------------------------------------------------------------}

destructor DSingleInterfaceElement.Destroy;
begin
  FreeAndNil(Timer);
  inherited Destroy;
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
  inherited Update;
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
  inherited Create;
  Children := DInterfaceElementsList.Create(true);
end;

{----------------------------------------------------------------------------}

destructor DInterfaceElement.Destroy;
begin
  //this should fire as recoursive because children owns elements, which in turn will fire their destructors onfree
  FreeAndNil(Children);
  inherited Destroy;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Rescale;
var i: integer;
begin
  inherited Rescale;
  for i := 0 to Children.Count-1 do Children[i].Rescale;
  //if this container is "fine" then get children's content and try to rescale
  if Base.isInitialized then RescaleToChildren;
end;

{-----------------------------------------------------------------------------}

function DInterfaceElement.GetSize: Txy;
var tx1,tx2,ty1,ty2: integer;
    tmp: Txy;
    c: DAbstractElement;
begin
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
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceElement.RescaleToChildren;
var TrueSize: Txy;
begin
  {This procedure is basic, however, there's no need to move it "up" by inheritance level
   as it's called only for elements containing children}
  TrueSize := Self.GetSize;
  log(LogInterfaceScaleHint,'x1=',IntToStr(TrueSize.x1)+'/'+IntToStr(Base.x1));
  log(LogInterfaceScaleHint,'x2=',IntToStr(TrueSize.x2)+'/'+IntToStr(Base.x2));
  log(LogInterfaceScaleHint,'y1=',IntToStr(TrueSize.y1)+'/'+IntToStr(Base.y1));
  log(LogInterfaceScaleHint,'y2=',IntToStr(TrueSize.y2)+'/'+IntToStr(Base.y2));

  //call rescale only in case something has changed and if container size is "valid" (non-zero)
  if  (TrueSize.x2 <> TrueSize.x1) and (TrueSize.y2 <> TrueSize.y1) and
     ((TrueSize.x1 <> Self.Base.x1) or (TrueSize.x2 <> Self.Base.x2) or
      (TrueSize.y1 <> Self.Base.y1) or (TrueSize.y2 <> Self.Base.y2)) then
  begin
    Base.SetIntCoord(TrueSize.x1,TrueSize.x2,TrueSize.y1,TrueSize.y2);
    Log(LogInterfaceScaleHint,{$I %CURRENTROUTINE%},'Backward-rescaling to Children.');
    //if Base.isInitialized then Rescale;
  end;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceElement.Draw;
var i: integer;
begin
  //inherited Draw; <---------- parent is abstract
  if isVisible then begin
    Update;
    for i := 0 to Children.Count-1 do Children[i].Draw;
  end;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Grab(const Child: DSingleInterfaceElement);
begin
  Children.Add(Child);
  if (Child is DSingleInterfaceElement) then DSingleInterfaceElement(Child).Parent := Self;
  //{Child.ID := }InterfaceList.Add(Child); //global ID of the element
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Clear;
begin
  Children.Clear;
end;

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

