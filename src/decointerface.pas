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
  Classes, fgl,
  CastleVectors, CastleImages, CastleGLImages,
  DecoGlobal, DecoTime;

const InterfaceScalingMethod: TResizeInterpolation = riBilinear;  //to quickly change it. Maybe will be a variable some day to support older PCs.

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
type TAnchorAlign = (noalign, haLeft, haRight, haCenter, vaTop, vaBottom, vaMiddle);

type
  { This is an abstract container with coordinates and size
    Capable of rescaling / copying itself }
  DAbstractContainer = class(TObject)
  strict private
    fInitialized: boolean;
    { Parent container size (cached) }
    cx1,cx2,cy1,cy2,cw,ch: integer;
    //aScaleX,aScaleY: float;
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
    KeepProportions: boolean;
    { Is this Container scaled agains Anchors or Window?
      Should be True only at top-level Container (i.e. GUI Container)
      However, maybe, reintroduction or manual scaling would be prefferable? }
    ScaleToWindow: boolean;
    { If this Container ready to be used? }
    property isInitialized: boolean read fInitialized;
    constructor Create;
    //destructor Destroy; override;
    { Copy parameters from the Source }
    procedure Assign(Source: DAbstractContainer);
    procedure AssignTo(Dest: DAbstractContainer);
    { Set container position and size }
    procedure SetFloatCoord(afx1,afy1,afx2,afy2: float);
    procedure SetFloatSize(afx1,afy1,afWidth,afHeight: float);
    procedure SetIntCoord(ax1,ay1,ax2,ay2: integer);
    procedure SetIntSize(ax1,ay1,aWidth,aHeight: integer);
    { Sets int width/height for scaling animations }
    procedure SetIntWidthHeight(aWidth,aHeight: integer);
  end;

type
  { Style of the animation.
    asLinear is just linear interpolation,
    asSquare is slower in the beginning and end, and faster in the middle}
  TAnimationCurve = (acsLinear, acSquare);

Type
  { most abstract container suitable for images, labels and interface elements
    Just defines the box and rescaling }
  DAbstractElement = class abstract(DAbstractContainer)
  strict private
    { Caches current animation state, recalculated by GetAnimationState at every render}
    procedure GetAnimationState;
  public
    { changes the scale of the element relative to current window size }
    procedure Rescale; virtual;
    { draw the element / as abstract as it might be :) }
    procedure Draw; virtual; abstract;
    { updates the data of the class with current external data,
      here it just gets the current animation state }
    procedure Update; virtual;
  strict private
    { Last and Next animation states. }
    Last, Next: DAbstractContainer;
    fVisible: boolean;
    {$HINT should it set visible to all children? }
    procedure SetVisible(Value: boolean);
  public
    { these values are "strict" and unaffected by animations. Usually determines
      the basic stage and implies image rescale and init GL. }
    AnimationStart: DTime;
    AnimationDuration: DTime;
    AnimationCurve: TAnimationCurve;
    {base state of the element. contains its coordinates and width/height}
    Base: DAbstractContainer;
    {source width/height of the element. Used to preserve proportions while scaling}
  {  RealWidth, RealHeight: integer;
    procedure SetBaseSize(const NewX,NewY,NewW,NewH,NewO: float; Animate: TAnimationStyle); virtual;
    procedure SetIntSize(const x1,y1,x2,y2:integer; Animate: TAnimationStyle); virtual;
    }
    { If the element is visible, if false then draw will not be called.
      PAY ATTENTION: if assigned to a single interface element then the animations
      and initGL will occur as they would normally. BUT if assigned to a
      composite parent element, its children WILL NOT do anything like this and
      will be frozen until visible=true. Maybe I'll fix this some day. }
    property Visible: boolean read fVisible write SetVisible;
    { animates the interface element from current state to base state,
      Important: GetAnimationState must be called before setting basesize
      of the element as AnimateTo uses currentAnimationState}
    procedure AnimateTo(Animate: TAnimationStyle; Duration: float = DefaultAnimationDuration);
   constructor Create; //override;
   destructor Destroy; override;
  end;

{ Simple procedures for (mouse) events }
//type TSimpleProcedure = procedure(sender: DAbstractElement) of Object;
type TSimpleProcedure = procedure of Object;
type TXYProcedure = procedure(Sender: DAbstractElement; x,y: integer) of Object;

Type
  { Fully-featured Interface Element with Mouse/Touch support }
  DSingleInterfaceElement = class(DAbstractElement)
  public
    {Higher-level element. Seldomly used in specific cases}
 {   Parent: DAbstractElement;
    procedure Draw; override;
    procedure Rescale; override;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    //also resizes content and frame
    procedure SetBaseSize(const NewX,NewY,NewW,NewH,NewO: float; Animate: TAnimationStyle); override;
    procedure SetIntSize(const x1,y1,x2,y2:integer; Animate: TAnimationStyle); override;
  private
    {GL image of the frame}
    GLFrame: TGLImage;
    {scaled frame image}
    FrameImage: TRGBAlphaImage;
    {duplicates AbstractImage properties, but is private, so sohuldn't conflict}
    InitGLPending: boolean;
    FrameReady: boolean;
    {Resizes the element's frame to fit base size}
    procedure FrameResize3x3;
    { initialize GL image. NOT THREAD SAFE! / Almost a copy of AbstractImage.initGl}
    procedure InitGL;{ override;}
    { resets parent and content size after rescale if needed}
    procedure ResetContentSize(animate: TAnimationStyle);

    procedure DrawFrame;
    procedure DrawContent;
  public
    {if this element is active (clickable)}
    CanMouseOver: boolean;
    CanDrag: boolean;
    {are these coordinates in this element's box?}
    function IAmHere(xx,yy: integer): boolean;
    {returns self if IAmHere and runs all possible events}
    function ifMouseOver(xx,yy: integer; RaiseEvents: boolean; AllTree: boolean): DAbstractElement; virtual;
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
    procedure Drag(x,y: integer);
    procedure StartDrag(x,y: integer); }
  end;

type DInterfaceElementsList = specialize TFPGObjectList<DSingleInterfaceElement>;

Type
  { a simple time-out mechanisms to preform some timed events on interface
    elements /
    maybe should be just a few additional routines at the parent class}
  DTimer = class(TObject)
    private
    {  {set automatically, date of the timer count start}
      StartTime: DTime;
    public
      {if the timer is running}
      Enabled: boolean;
      {how long (in seconds) will it take the timer to fire}
      Interval: DTime;
      {action to preform}
      onTimer: TSimpleProcedure;
      constructor Create;
      {a simple way to set and run timer}
      procedure SetTimeOut(Seconds: DTime);
      {check if the timer finished and run onTimer if true}
      procedure Update;}
  end;

Type
  {An interface element, that can contain "children"}
  DInterfaceElement = class(DSingleInterfaceElement)
  public
    {if the interface element rescales each time children rescale}
 {   ScaleToChildren: boolean;
    {list of the children of this interface element}
    Children: DInterfaceElementsList;
    {a simple timer to fire some event on time-out}
    Timer: DTimer;
    procedure Draw; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Rescale; override;
    procedure Update; override;
  public
    {assign given element as a child and sets its parent to self}
    procedure Grab(Child: DSingleInterfaceElement);
    {}
    procedure RescaleToChildren(animate: TAnimationStyle);
  public
    {returns last call to MouseOverTree result, may be buggy!}
    isMouseOverTree: boolean;
    {returns self if IAmHere and runs all possible events + scans all children}
    function ifMouseOver(xx,yy: integer; RaiseEvents: boolean; AllTree: boolean): DAbstractElement; override;
    {returns true if mouse is over any "canmouseover" child of this element}
    function MouseOverTree(xx,yy: integer): boolean; }
  end;

{Var {simple outline around black box}
    //SimpleFrame,
    {a frame with 19px header}
    //CaptionFrame,
    {Just black background with no frame}
    BlackFrame: DFrame;    }

    {contains global list of all interface elements // not needed yet}
    //InterfaceList: DInterfaceElementsList;

    {this is a thrash can for interface elements to dispose them off safely
     after "suicide animation" has finished}
    //GarbageThrash: DInterfaceElementsList;

{initializes "burner" image to bake some pattern over the interface images,
 mostly used for frames}
//procedure Init_burner_image;
{reads some interface-related data, like loading frames images}
procedure InitInterface;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog, castleFilesUtils,
  DecoGui {this is horrible},
  DecoInterfaCecomposite, DecoInputOutput;

{-------------------- BURNER IMAGE --------------------------------------------}

(*var BURNER_IMAGE_UNSCALED,BURNER_IMAGE:TCastleImage;  //todo: not freed automatically!!!!
procedure Init_burner_image;
begin
  {$IFNDEF AllowRescale}if BURNER_IMAGE<>nil then exit;{$ENDIF}
  WriteLnLog('Init_burner_image','started');
  if BURNER_IMAGE_UNSCALED = nil then
    BURNER_IMAGE_UNSCALED := LoadImageSafe(ApplicationData(InterfaceFolder+'burner/burner_Pattern_203_CC0_by_Nobiax_diffuse.png'), [TRGBImage]) as TRGBImage;
  if (BURNER_IMAGE=nil) or (BURNER_IMAGE.height <> window.height) or (BURNER_IMAGE.width <> window.width) then begin
    FreeAndNil(BURNER_IMAGE);
    BURNER_IMAGE := BURNER_IMAGE_UNSCALED.MakeCopy;
    BURNER_IMAGE.Resize(window.width, window.height, riBilinear);
  end;
  {$IFNDEF AllowRescale}FreeAndNil(BURNER_IMAGE_UNSCALED);{$ENDIF}

  WriteLnLog('Init_burner_image','finished');
end; *)

{-------------------- INIT INTERFACE ------------------------------------------}

procedure InitInterface;
begin
  WriteLnLog('InitInterface','started');
  //Init_burner_image;

{  SimpleFrame := DFrame.create(Window);
  with SimpleFrame do begin
    SourceImage := LoadImageSafe(ApplicationData(FramesFolder+'frame.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 1; CornerBottom := 1; cornerLeft := 1; CornerRight := 1;
  end;

  CaptionFrame := DFrame.create(Window);
  with CaptionFrame do begin
    SourceImage := LoadImageSafe(ApplicationData(FramesFolder+'frame_caption.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 19; CornerBottom := 1; cornerLeft := 1; CornerRight := 1;            //todo: variable top line!
  end;    }

  {BlackFrame := DFrame.Create(Window);
  with BlackFrame do begin
    SourceImage := LoadImageSafe(ApplicationData(FramesFolder+'blackframe.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    CornerTop := 0; CornerBottom := 0; CornerLeft := 0; CornerRight := 0;
  end;}

  InitCompositeInterface;

  //InterfaceList := DInterfaceElementsList.create(false);

  WriteLnLog('InitInterface','finished');
end;

{=============================================================================}

{constructor DFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Rectagonal := true;
end;}

{destructor DFrame.Destroy;
begin
  FreeAndNil(SourceImage);
  inherited;
end;}

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
    gap := 0;
  end;
  Opacity := 1;
  KeepProportions := false;
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
  {aScaleX := 1/aw;
  aScaleY := 1/ah;}
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
begin
  GetAnchors;

  x1 := cx1 + Round(cw * fx1) + Anchor[asLeft].Gap;
  x2 := cx2 + Round(cw * fx2) - Anchor[asRight].Gap;
  y1 := cy1 + Round(ch * fy1) + Anchor[asTop].Gap;
  y2 := cy2 + Round(ch * fy2) - Anchor[asBottom].Gap;

  w := x2 - x1;
  h := y2 - y1;

  fInitialized := true;
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.SetFloatCoord(afx1,afy1,afx2,afy2: float);
begin
  fx1 := afx1;
  fy1 := afy1;
  fx2 := afx2;
  fy2 := afy2;

  FloatToInteger;
end;
procedure DAbstractContainer.SetFloatSize(afx1,afy1,afWidth,afHeight: float);
begin
  fx1 := afx1;
  fy1 := afy1;
  fx2 := - (1 - afWidth - afx1);
  fy2 := - (1 - afHeight - afy1);

  FloatToInteger;
end;
procedure DAbstractContainer.SetIntCoord(ax1,ay1,ax2,ay2: integer);
begin
  x1 := ax1;
  y1 := ay1;
  x2 := ax2;
  y2 := ay2;

  IntegerToFloat;
end;
procedure DAbstractContainer.SetIntSize(ax1,ay1,aWidth,aHeight: integer);
begin
  x1 := ax1;
  y1 := ay1;
  x2 := ax1 + aWidth;
  y2 := ay1 + aHeight;

  IntegerToFloat;
end;

{----------------------------------------------------------------------------}

procedure DAbstractContainer.SetIntWidthHeight(aWidth,aHeight: integer);
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

procedure DAbstractContainer.Assign(Source: DAbstractContainer);
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
  Self.Opacity := Source.Opacity;
  Self.KeepProportions := Source.KeepProportions;
  Self.fInitialized := Source.isInitialized;
  for aa in TAnchorSide do
    Self.Anchor[aa] := Source.Anchor[aa];
end;
procedure DAbstractContainer.AssignTo(Dest: DAbstractContainer);
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
  Dest.Opacity := Self.Opacity;
  Dest.KeepProportions := Self.KeepProportions;
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
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.SetVisible(Value: boolean);
begin
  fVisible := Value;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.AnimateTo(Animate: TAnimationStyle; Duration: float = DefaultAnimationDuration);
var mx,my: float;
begin
  if Animate = asNone then Exit else begin
    AnimationStart := -1;
    AnimationDuration := Duration;

    GetAnimationState;
    Last.Assign(Self);   //to current animation state
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

{procedure DAbstractElement.SetBaseSize(const NewX,NewY,NewW,NewH,NewO: float; Animate: TAnimationStyle);
begin
  GetAnimationState;
  Base.setsize(NewX,NewY,NewW,NewH);
  Base.opacity := NewO;
  AnimateTo(Animate);
  //rescale; //????
end;}

{----------------------------------------------------------------------------}

{procedure DAbstractElement.SetIntSize(const x1,y1,x2,y2:integer; Animate: TAnimationStyle);
begin
  GetAnimationState;
  Base.BackwardSetXYWH(x1,y1,x2-x1,y2-y1);
  AnimateTo(Animate);
  //rescale; //????
end;}

{----------------------------------------------------------------------------}

{procedure DSingleInterfaceElement.SetBaseSize(const NewX,NewY,NewW,NewH,NewO: float; Animate: TAnimationStyle);
begin
  inherited SetBaseSize(NewX,NewY,NewW,NewH,NewO,Animate);
  ResetContentSize(Animate);
end;}

{----------------------------------------------------------------------------}

{procedure DSingleInterfaceElement.SetIntsize(const x1,y1,x2,y2:integer; Animate: TAnimationStyle);
begin
  inherited SetIntsize(x1,y1,x2,y2,Animate);
  ResetContentSize(Animate);
end;}

{----------------------------------------------------------------------------}

{procedure DSingleInterfaceElement.ResetContentSize(Animate: TAnimationStyle);
begin
  if (Parent<>nil) and (Parent is DInterfaceElement) and (DInterfaceElement(Parent).ScaleToChildren) then
    DInterfaceElement(Parent).RescaleToChildren(Animate);
  //frame should be automatically resized during "rescale" and animated during draw...
  if Content <> nil then begin
    Content.Base.CopyXYWH(Self.Base);
    Content.Base.SubstractFrame(Frame,0); //adjust to frame borders, no problem that frame may be unrescaled yet, because frame borders always the same
    Content.CurrentAnimationState.CopyXYWH(Self.CurrentAnimationState);
    Content.CurrentAnimationState.SubstractFrame(Frame,0);
    Content.AnimateTo(Animate);
  end;

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
      Self.x1 := Last.x1+Round((Next.x1-Last.x1)*Phase);
      Self.y1 := Last.y1+Round((Next.y1-Last.y1)*Phase);
      Self.x2 := Last.x2+Round((Next.x2-Last.x2)*Phase);
      Self.y2 := Last.y2+Round((Next.y2-Last.y2)*Phase);
      Self.Opacity := Last.Opacity+((Next.opacity-Last.Opacity)*Phase);
      //we don't need scale back to float, as it's not a basic animation state
    end else begin
      {should be "next" here}
      Self.x1 := Base.x1;
      Self.x2 := Base.x2;
      Self.y1 := Base.y1;
      Self.y2 := Base.y2;
      Self.Opacity := Base.Opacity;
    end;
    Self.w := Self.x2 - Self.x1;
    Self.h := Self.y2 - Self.y1;
    //Self.fInitialized := true;
 end else
   //Self.fInitialized := false;
end;

procedure DAbstractElement.Update;
begin
  GetAnimationState;
end;

{----------------------------------------------------------------------------}


constructor DAbstractElement.Create;
begin
  inherited Create;
  fVisible := true;
  AnimationCurve := acSquare;
  Base := DAbstractContainer.Create;
  Last := DAbstractContainer.Create;
  Next := DAbstractContainer.Create;
end;

{----------------------------------------------------------------------------}

destructor DAbstractElement.Destroy;
begin
  freeandnil(Base);
  freeandnil(Last);
  freeandnil(Next);
  inherited;
end;

{=============================================================================}
{=================== Single interface element ===============================}
{=============================================================================}

{procedure DSingleInterfaceElement.Rescale;
begin
  {$WARNING Memory Leak here}
  if Frame <> nil then FrameResize3x3;
  if Content <> nil then begin
    //content.base.copyxywh(base);
    //content.base.SubstractFrame(frame,2);
    Content.Rescale;   //todo
  end;
end;}

{----------------------------------------------------------------------------}

{constructor DSingleInterfaceElement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FrameOnTop := false;
  //ID := -1;
  //OwnsContent := false;
  FrameOpacity := 0.8;
  isMouseOver := false;
  CanMouseOver := false;
  CanDrag := false;
end;}

{-----------------------------------------------------------------------------}

{destructor DSingleInterfaceElement.Destroy;
begin
  //InterfaceList.Remove(self);
  FreeAndNil(GLFrame);
  //scaledImage is automatically freed by GlImage
  {$HINT BUG: why Scaled image is not freed automatically?????}
  {BUG: I still need to free ScaledImage - while it's owned by GLImage
   DAMN IT. The link may be obsolete after freeandnil(GLImage)!
   Looks like I always set ScaledImage := nil after sucessfuly assigning it,
   but should keep an eye on it!}
  FreeAndNil(FrameImage);
  //if owns content destroy it here;
  //if OwnsContent then FreeAndNil(content);  //content usually has AOwner = self
  inherited;
end;}

{----------------------------------------------------------------------------}

{procedure DSingleInterfaceElement.FrameResize3x3;
var ScaledImageParts: array [0..2,0..2] of TCastleImage;
    ix,iy: integer;
    UnscaledWidth, UnscaledHeight:integer;
    SourceXs, SourceYs, DestXs, DestYs: TVector4Integer;
begin
  FrameReady := false;
  if Base.Initialized = false then begin
    WriteLnLog('DAbstractInterfaceElement.FrameResize3x3','ERROR: Base is not initialized!');
  end;

  if FrameImage<>nil then begin
    FreeAndNil(FrameImage);
    WriteLnLog('DSingleInterfaceElement.FrameResize3x3','ERROR: FrameImage is not nil! (memory leak)');
  end;
  FrameImage := Frame.SourceImage.CreateCopy as TRGBAlphaImage;

  UnscaledWidth := FrameImage.Width;
  UnscaledHeight := FrameImage.Height;

  {check if minimal frame size is larger than the requested frame size}
  if Frame.CornerLeft+Frame.CornerRight+1 > Base.w then begin
    WriteLnLog('DAbstractInterfaceElement.FrameResize3x3','Reset backwards base.w = '+IntToStr(Base.w)+' / cornerLeft+cornerRight = '+IntToStr(Frame.CornerLeft+Frame.CornerRight));
    Base.w := Frame.CornerLeft+Frame.CornerRight+1;
    Base.BackwardSetSize(Base.w,-1);
  end;
  if Frame.CornerTop+Frame.CornerBottom+1 > Base.h then begin
    WriteLnLog('DAbstractInterfaceElement.FrameResize3x3','Reset backwards base.h = '+inttostr(base.h)+' / cornerTop+cornerBottom = '+inttostr(frame.cornerTop+frame.cornerBottom));
    Base.h := Frame.CornerTop+Frame.CornerBottom+1;
    Base.BackwardSetSize(-1,Base.h);
  end;

  SourceXs[0] := 0;
  SourceXs[1] := Frame.CornerLeft;
  SourceXs[2] := UnscaledWidth-Frame.CornerRight;
  SourceXs[3] := UnscaledWidth;
  SourceYs[0] := 0;
  SourceYs[1] := Frame.cornerBottom;
  SourceYs[2] := UnscaledHeight-Frame.CornerTop;
  SourceYs[3] := UnscaledHeight;
  DestXs[0] := 0;
  DestXs[1] := Frame.CornerLeft;
  DestXs[2] := Base.w-Frame.CornerRight;
  DestXs[3] := Base.w;
  DestYs[0] := 0;
  DestYs[1] := Frame.CornerBottom;
  DestYs[2] := Base.h-Frame.CornerTop;
  DestYs[3] := Base.h;

  for ix := 0 to 2 do
   for iy := 0 to 2 do begin
     ScaledImageParts[ix,iy] := TRGBAlphaImage.Create;
     ScaledImageParts[ix,iy].SetSize(SourceXs[ix+1]-SourceXs[ix],SourceYs[iy+1]-SourceYs[iy]);
     ScaledImageParts[ix,iy].Clear(Vector4Byte(0,0,0,0));
     ScaledImageParts[ix,iy].DrawFrom(FrameImage,0,0,SourceXs[ix],SourceYs[iy],SourceXs[ix+1]-SourceXs[ix],SourceYs[iy+1]-SourceYs[iy],dmBlendSmart);
     ScaledImageParts[ix,iy].Resize(DestXs[ix+1]-DestXs[ix],DestYs[iy+1]-DestYs[iy],riNearest);
   end;

  FrameImage.SetSize(Base.w,Base.h,1);
  FrameImage.Clear(Vector4byte(0,0,0,0));
  for ix := 0 to 2 do
    for iy := 0 to 2 do FrameImage.DrawFrom(ScaledImageParts[ix,iy],DestXs[ix],DestYs[iy],0,0,DestXs[ix+1]-DestXs[ix],DestYs[iy+1]-DestYs[iy],dmBlendSmart);

  for ix := 0 to 2 do
    for iy := 0 to 2 do FreeAndNil(ScaledImageParts[ix,iy]);

  //and burn the burner
  //FrameImage.DrawFrom(BURNER_IMAGE,0,0,base.x1,base.y1,base.w,base.h,dmMultiply);

  InitGLPending := true;
end;}

{----------------------------------------------------------------------------}

{procedure DSingleInterfaceElement.InitGL;
begin
  //content makes his own initGL on first draw
  if InitGLPending then begin
    InitGLPending := false;
    if FrameImage<>nil then begin
      FreeAndNil(GLFrame);
      GLFrame := TGLImage.Create(FrameImage,true,true); //todo: maybe, it's inefficient
      FrameImage := nil;
      FrameReady := true;
    end;
  end;
end;}

{----------------------------------------------------------------------------}

{procedure DSingleInterfaceElement.DrawFrame; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if Frame <> nil then begin
    if FrameReady then begin
      GLFrame.Color := Vector4(1,1,1,CurrentAnimationState.Opacity * FrameOpacity);     //todo
      GLFrame.Draw(CurrentAnimationState.x1,CurrentAnimationState.y1,CurrentAnimationState.w,CurrentAnimationState.h);
    end else begin
      if InitGLPending then InitGL;
    end;
  end;
end;}

{procedure DSingleInterfaceElement.DrawContent; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  //todo
  if Content <> nil then begin
    //Content.base.copyxywh(currentAnimationState);
    Content.Draw;
  end;
end;}

{procedure DSingleInterfaceElement.Draw;
begin
  Update;
  if not Visible then Exit;

  if FrameOnTop then begin
    DrawContent;
    DrawFrame;
  end else begin
    DrawFrame;
    DrawContent;
  end;

end;}

{========== Abstract intarface element : Mouse handling =====================}

{function DSingleInterfaceElement.IAmHere(xx,yy: integer): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  //get current element location... maybe, use not current animation, but "base"? Or completely ignore items being animated?
  GetAnimationState;
  if (xx >= CurrentAnimationState.x1) and (xx <= CurrentAnimationState.x2) and
     (yy >= CurrentAnimationState.y1) and (yy <= CurrentAnimationState.y2)
  then
    Result := true
  else
    Result := false;
end;}

{-----------------------------------------------------------------------------}

{function DSingleInterfaceElement.ifMouseOver(xx,yy: integer; RaiseEvents: boolean; AllTree: boolean): DAbstractElement;
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
end;}

{function DInterfaceElement.ifMouseOver(xx,yy: integer; RaiseEvents: boolean; AllTree: boolean): DAbstractElement;
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
end;}

{-----------------------------------------------------------------------------}

{procedure DSingleInterfaceElement.StartDrag(x,y: integer);
begin
  DragX := Base.x1 - x;
  DragY := Base.y1 - y;
end;}


{procedure DSingleInterfaceElement.drag(x,y: integer);
begin
  Base.x1 := DragX + x;
  Base.y1 := DragY + y;
end;}

{=============================================================================}
{=========================== interface element ===============================}
{=============================================================================}

{constructor DTimer.Create;
begin
  inherited;
  Enabled := false;
  StartTime := -1;
end;}

{-----------------------------------------------------------------------------}

{procedure DTimer.Update;
begin
  if StartTime<0 then StartTime := DecoNow else
  if (DecoNow-StartTime) >= Interval then begin
    Enabled := false;
    if Assigned(onTimer) then onTimer;
  end;
end; }

{-----------------------------------------------------------------------------}

{procedure DTimer.SetTimeout(Seconds: DTime);
begin
  StartTime := -1;
  Enabled := true;
  Interval := Seconds;
end;}

{-----------------------------------------------------------------------------}

{procedure DInterfaceElement.Rescale;
var i: integer;
begin
  inherited;
  //{$WARNING Memory Leak here}
  for i:=0 to Children.Count-1 do Children[i].Rescale;
end;}

{-----------------------------------------------------------------------------}

{procedure DInterfaceElement.Update;
begin
  inherited;
  if Timer.Enabled then Timer.Update;
end; }

{-----------------------------------------------------------------------------}

{constructor DInterfaceElement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is DSingleInterfaceElement then Parent := DSingleInterfaceElement(AOwner);
  ScaleToChildren := false;
  Children := DInterfaceElementsList.Create(true);
  Timer := DTimer.Create; //maybe create timer on-demand?
end;}

{----------------------------------------------------------------------------}

{destructor DInterfaceElement.Destroy;
begin
  FreeAndNil(Children);   //this should fire as recoursive because children owns elements, which in turn will fire their destructors onfree
  FreeAndNil(Timer);
  inherited;
end;}

{----------------------------------------------------------------------------}

{procedure DInterfaceElement.Draw;
var i: integer;
begin
  inherited;
  for i := 0 to Children.Count-1 do Children[i].Draw;
end;}

{----------------------------------------------------------------------------}

{procedure DInterfaceElement.Grab(Child: DSingleInterfaceElement);
begin
  Children.Add(Child);
  if (Child is DSingleInterfaceElement) then DSingleInterfaceElement(Child).Parent := Self; //not sure about this line
  //{Child.ID := }InterfaceList.Add(Child); //global ID of the element
end;}

{----------------------------------------------------------------------------}

{procedure DInterfaceElement.RescaleToChildren(Animate: TAnimationStyle);
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

{function DInterfaceElement.MouseOverTree(xx,yy: integer): boolean;
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
end;}

end.

