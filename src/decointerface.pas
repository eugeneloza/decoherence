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
unit decointerface;

{$INCLUDE compilerconfig.inc}

interface

uses
  Classes, fgl,
  castleVectors, castleImages, CastleGLImages,
  decoglobal;

const InterfaceScalingMethod: TResizeInterpolation = riBilinear;  //to quickly change it. Maybe will be a variable some day to support older PCs.

const defaultanimationduration = 0.3; {in seconds}

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

Type
 { Several types of frames, including with captions }
 DFrame = class(TComponent)
 public
   SourceImage: TRGBAlphaImage;
   {frame borders}
   cornerTop, cornerbottom, cornerLeft, cornerRight: integer;
   Rectagonal: boolean;
   constructor create(AOwner: TComponent); override;
   destructor destroy; override;
end;

 { constants for special scaling cases }
const fullwidth = -1;
      fullheight = -2;
      proportionalscale = -3;
type
  { Yes, that looks stupid for now. But I'll simplify it later. Maybe.
   Contains redunant data on animation with possible rescaling in mind.}
  Txywh = class(TComponent)
  public
    //Property x1: integer get x1 set setx;  Setx -> set fx recalculate ffx
    { integer "box" }
    x1,y1,x2,y2,w,h: integer;
    { float }
    fx,fy,fw,fh: float;
    opacity: float;
    initialized: boolean;
    { assign float and convert to Integer }
    procedure setsize(const newx,newy,neww,newh: float);
    { get integer and convert to float / for fixed-size objects }
    procedure backwardsetsize(const neww,newh: integer);
    procedure backwardsetxywh(const newx,newy,neww,newh: integer);
    {substract frame width from the base}
    Procedure SubstractFrame(frame: DFrame; AdditionalGap: integer = 0);
    { transform floats to integer }
    procedure recalculate;
    constructor create(AOwner: TComponent); override;
    { provides for proportional width/height scaling for some images }
    procedure FixProportions(ww,hh: integer);
  end;

type
  { to make copies }
  Txywha = class(Txywh)
  public
    //function makecopy:Txywh;
    procedure copyxywh(source: Txywh);
  end;

Type
  { most abstract container suitable for images, labels and interface elements
    Just defines the box and rescaling }
  DAbstractElement = class(TComponent)
  public
    {stores current animation state, recalculated by GetAnimationState at every render}
    CurrentAnimationState: Txywha;
    procedure GetAnimationState; virtual;

    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    { changes the scale of the element relative to current window size }
    procedure rescale; virtual;
    { draw the element / as abstract as it might be :) }
    procedure draw; virtual; abstract;
    //procedure InitGL; virtual; abstract; //no need for an abstract method?
    { updates the data of the class with current external data, most often
      just gets the current animation state }
    procedure update; virtual;
  private
    { Last and Next animation states. }
    last, next: Txywha;
//    Free_on_end: boolean;
    fvisible: boolean;
    procedure setvisible(value: boolean);
  public
    { these values are "strict" and unaffected by animations. Usually determines
      the basic stage and implies image rescale and init GL. }
    animationstart: TDateTime;
    animationduration: single;
    {base state of the element. contains its coordinates and width/height}
    base: Txywha;
    {source width/height of the element. Used to preserve proportions while scaling}
    RealWidth, RealHeight: integer;
    procedure setbasesize(const newx,newy,neww,newh,newo: float; animate: TAnimationStyle); virtual;
    procedure setIntSize(const x1,y1,x2,y2:integer; animate: TAnimationStyle); virtual;
    {if the element is visible, if false then draw will not be called.
     PAY ATTENTION: if assigned to a single interface element then the animations
     and initGL will occur as they would normally. BUT if assigned to a
     composite parent element, its children WILL NOT do anything like this and
     will be frozen until visible=true. Maybe I'll fix this.}
    property visible: boolean read fvisible write setvisible;
    {animates the interface element from current state to base state,
     Important: GetAnimationState must be called before setting basesize
     of the element as AnimateTo uses currentAnimationState}
    procedure AnimateTo(animate: TAnimationStyle; duration: single = defaultanimationduration);
  end;

{Definition of simple procedures for (mouse) events}
//type TSimpleProcedure = procedure(sender: DAbstractElement) of Object;
type TSimpleProcedure = procedure of Object;
type TXYProcedure = procedure(sender: DAbstractElement; x,y: integer) of Object;

Type
  {Element with a frame and content}
  DSingleInterfaceElement = class(DAbstractElement)
  public
    {Higher-level element. Seldomly used in specific cases}
    parent: DAbstractElement;
    {whether Interface element owns its contents? If true they'll bee freed
     on destroy // using TComponent Inheritance for now}
    //OwnsContent: boolean;
    { content of the Interface element: label or image }
    Content: DAbstractElement;    {actually DAbstractImage, but cyclic references are not allowed}
    { multiplier to opacity <1, e.g. overall opacity=0.8 FrameOpacity=0.8, frame final Opacity=0.8*0.8 }
    FrameOpacity: float;
    { frame behind the Interface element }
    frame: DFrame;
    {specifices if the frame draws over content or vice versa}
    FrameOnTop: boolean;
    procedure draw; override;
    procedure rescale; override;
    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
    //also resizes content and frame
    procedure setbasesize(const newx,newy,neww,newh,newo: float; animate: TAnimationStyle); override;
    procedure setIntSize(const x1,y1,x2,y2:integer; animate: TAnimationStyle); override;
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

    procedure drawFrame;
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
    dragx, dragy: integer;
    procedure drag(x,y: integer);
    procedure startdrag(x,y: integer);
  end;

type DInterfaceElementsList = specialize TFPGObjectList<DSingleInterfaceElement>;

Type
  { a simple time-out mechanisms to preform some timed events on interface
    elements /
    maybe should be just a few additional routines at the parent class}
  DTimer = class(TObject)
    private
      {set automatically, date of the timer count start}
      StartTime: DTime;
    public
      {if the timer is running}
      enabled: boolean;
      {how long (in seconds) will it take the timer to fire}
      Interval: DTime;
      {action to preform}
      onTimer: TSimpleProcedure;
      constructor create;
      {a simple way to set and run timer}
      procedure settimeout(seconds: DTime);
      {check if the timer finished and run onTimer if true}
      procedure update;
  end;

Type
  {An interface element, that can contain "children"}
  DInterfaceElement = class(DSingleInterfaceElement)
  public
    {if the interface element rescales each time children rescale}
    ScaleToChildren: boolean;
    {list of the children of this interface element}
    children: DInterfaceElementsList;
    {a simple timer to fire some event on time-out}
    Timer: DTimer;
    procedure draw; override;
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
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
    function MouseOverTree(xx,yy: integer): boolean;
  end;

Var {simple outline around black box}
    //SimpleFrame,
    {a frame with 19px header}
    //CaptionFrame,
    {Just black background with no frame}
    BlackFrame: DFrame;

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

uses sysutils, CastleLog, castleFilesUtils,
  decointerfacecomposite, decoinputoutput;

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

  BlackFrame := DFrame.create(Window);
  with BlackFrame do begin
    SourceImage := LoadImageSafe(ApplicationData(FramesFolder+'blackframe.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 0; CornerBottom := 0; cornerLeft := 0; CornerRight := 0;
  end;

  InitCompositeInterface;

  //InterfaceList := DInterfaceElementsList.create(false);

  WriteLnLog('InitInterface','finished');
end;

{=============================================================================}

constructor DFrame.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  rectagonal := true;
end;

destructor DFrame.destroy;
begin
  freeandnil(SourceImage);
  inherited;
end;

{=============================================================================}
{=========================== Abstract element ================================}
{=============================================================================}


constructor Txywh.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  initialized := false;
end;

{----------------------------------------------------------------------------}

procedure Txywh.setsize(const newx,newy,neww,newh:float);
begin
  if (abs(newx) > 1) or (abs(newy) > 1) or
     (((neww<0) or (neww>1)) and (not FloatsEqual(neww,proportionalscale) and not FloatsEqual(neww,fullwidth) and not FloatsEqual(neww,fullheight))) or
     (((newh<0) or (newh>1)) and (not FloatsEqual(neww,proportionalscale) and not FloatsEqual(newh,fullheight))) then
  begin
    writeLnLog('Txywh.setsize','ERROR: Incorrect newx,newy,neww,newh!');
    exit;
  end;

  { stop if nothing was changed }
  if FloatsEqual(fx,newx) and FloatsEqual(fy,newy) and FloatsEqual(fw,neww) and FloatsEqual(fh,newh) then exit;

  fx := newx;
  fy := newy;
  fw := neww;
  fh := newh;

  recalculate;
end;

{----------------------------------------------------------------------------}

procedure Txywh.recalculate;
begin
  { convert float to integer }

  if fx >= 0 then
    x1 := round(Window.height*fx)
  else
    x1 := Window.width + round(Window.height*fx);

  if fy>=0 then
    y1 := round(Window.height*fy)     // turn over y-axis?
  else
    y1 := Window.height + round(Window.height*fy);

  if FloatsEqual(fw,fullwidth) then begin
    w := Window.width;
    x1 := 0
  end
  else
  if FloatsEqual(fw,fullheight) then
    w := Window.height
  else
    w := round(Window.height*fw);

  if FloatsEqual(fh,fullheight) then begin
    h := Window.height;
    y1 := 0
  end else
    h := round(Window.height*fh);

  x2 := x1+w;
  y2 := y1+h;

  initialized := true;
end;

{----------------------------------------------------------------------------}


procedure Txywh.backwardsetsize(const neww,newh: integer);
begin
  if neww>0 then begin
    w := neww;
    fw := neww/window.height;
    if x1+w > Window.width then begin
      x1 := Window.width - w;
      fx := x1/Window.height;
    end;
    x2 := x1+w;
  end;
  if newh>0 then begin
    h := newh;
    fh := newh/window.height;
    if y1+h > Window.height then begin
      y1 := Window.height - h;
      fy := y1/Window.height;
    end;
    y2 := y1+h;
  end;
end;

procedure Txywh.backwardsetxywh(const newx,newy,neww,newh: integer);
begin
  //todo check for consistency
  x1 := newx;
  y1 := newy;
  w := neww;
  h := newh;
  if newx<window.width div 2 then fx := newx/window.height else fx := (newx-Window.Width)/window.height;
  fy := newy/window.height;
  fw := neww/window.height;
  fh := newh/window.height;
  initialized := true;
end;

{----------------------------------------------------------------------------}

Procedure Txywh.SubstractFrame(frame: DFrame; AdditionalGap: integer = 0);
Begin
  If (frame<>nil) and (frame.rectagonal) then begin
    X1 := x1 + frame.cornerleft+AdditionalGap;
    X2 := x2 - frame.cornerright-AdditionalGap;
    W := w - frame.cornerleft - frame.cornerright -2*AdditionalGap;
    Y1 := y1 + frame.cornertop+AdditionalGap;
    Y2 := y2 - frame.cornerbottom-AdditionalGap;
    H := h - frame.cornertop - frame.cornerbottom -2*AdditionalGap;
    backwardsetxywh(x1,y1,w,h);
  end else if AdditionalGap<>0 then begin
    X1 := x1 + AdditionalGap;
    X2 := x2 - AdditionalGap;
    W := w - 2*AdditionalGap;
    Y1 := y1 + AdditionalGap;
    Y2 := y2 - AdditionalGap;
    H := h - 2*AdditionalGap;
    backwardsetxywh(x1,y1,w,h);
  end;
End;

{----------------------------------------------------------------------------}

procedure Txywh.FixProportions(ww,hh: integer);
begin
  if FloatsEqual(fw,proportionalscale) then
    w := round(h*ww/hh)
  else                                 //they can't be proportional both
  if FloatsEqual(fh,proportionalscale) then
    h := round(w*hh/ww);
end;

{----------------------------------------------------------------------------}

procedure Txywha.copyxywh(source: Txywh);
begin
  x1 := source.x1;
  y1 := source.y1;
  x2 := source.x2;
  y2 := source.y2;
  w := source.w;
  h := source.h;
  fx := source.fx;
  fy := source.fy;
  fw := source.fw;
  fh := source.fh;
  opacity := source.opacity;
  initialized := source.initialized;
end;

{============================================================================}

procedure DAbstractElement.rescale;
begin
  base.recalculate;
  last.recalculate;
  next.recalculate;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.setvisible(value: boolean);
begin
  fvisible := value;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.AnimateTo(animate: TAnimationStyle; duration: single = defaultanimationduration);
var mx,my: float;
begin
  if animate = asNone then exit else begin
    animationstart := -1;
    animationduration := duration;
    last.copyxywh(base); //todo: CurrentAnimationState
    next.copyxywh(base);
    case animate of
      {just grabs some previous locations and animates the item from there to base
       requires that CurrentAnimationState is initialized... TODO}
      asDefault: last.copyxywh(CurrentAnimationState);
      {fades in/out element}
      asFadeIn:  last.opacity := 0;
      asFadeOut: next.opacity := 0;
      {asFadeOutSuicide}
      {zooms in/out element}
      asZoomIn:  begin
                   Last.backwardsetsize(1,1);
                   Last.opacity := 0;
                 end;
      asZoomOut: begin
                   Next.backwardsetsize(1,1);
                   Next.opacity := 0;
                 end;
      {asZoomOutSuicide}
      asFlyInRandom,asFlyOutRandom: begin
                       mx := drnd.random*window.width/window.height;  //mult by aspect ratio
                       my := drnd.random;
                       case drnd.Random(4) of
                         0: mx :=  0.0001;
                         1: mx := -0.0001;
                         2: my :=  0.0001;
                         3: my := -0.0001;
                       end;
                       if animate=asFlyInRandom then begin
                         Last.fx := mx;
                         Last.fy := my;
                       end else begin
                         Next.fx := mx;
                         Next.fy := my;
                       end;
                     end;
      asFlyInTop,asFlyOutTop,asFlyInBottom,asFlyOutBottom,asFlyInLeft,asFlyOutLeft,asFlyInRight,asFlyOutRight: begin
                       mx := drnd.random*window.width/window.height;
                       my := drnd.random;
                       case animate of
                         asFlyInLeft,asFlyOutLeft: mx :=  0.0001;
                         asFlyInRight,asFlyOutRight: mx := -0.0001;
                         asFlyInBottom,asFlyOutBottom: my := 0.0001;
                         asFlyInTop,asFlyOutTop: my := -0.0001;
                       end;
                       if (animate=asFlyInLeft) or (animate=asFlyInRight) or (animate=asFlyInTop) or (animate=asFlyInBottom) then begin
                         Last.fx := mx;
                         Last.fy := my;
                       end else begin
                         Next.fx := mx;
                         Next.fy := my;
                       end;
                     end;
      {asFlyOutRandomSuicide}
    end;
  end;
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.setbasesize(const newx,newy,neww,newh,newo: float; animate: TAnimationStyle);
begin
  GetAnimationState;
  base.setsize(newx,newy,neww,newh);
  base.opacity := newo;
  animateTo(animate);
  //rescale; //????
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.setIntSize(const x1,y1,x2,y2:integer; animate: TAnimationStyle);
begin
  GetAnimationState;
  base.backwardsetxywh(x1,y1,x2-x1,y2-y1);
  animateTo(animate);
  //rescale; //????
end;

{----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.setbasesize(const newx,newy,neww,newh,newo: float; animate: TAnimationStyle);
begin
  inherited setBaseSize(newx,newy,neww,newh,newo,animate);
  ResetContentSize(animate);
end;

{----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.setIntsize(const x1,y1,x2,y2:integer; animate: TAnimationStyle);
begin
  inherited setIntsize(x1,y1,x2,y2,animate);
  ResetContentSize(animate);
end;

{----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.resetContentSize(animate: TAnimationStyle);
begin
  if (parent<>nil) and (parent is DInterfaceElement) and (DInterfaceElement(parent).ScaleToChildren) then
    DInterfaceElement(parent).RescaleToChildren(animate);
  //frame should be automatically resized during "rescale" and animated during draw...
  if content <> nil then begin
    content.base.copyxywh(self.base);
    content.base.SubstractFrame(frame,0); //adjust to frame borders, no problem that frame may be unrescaled yet, because frame borders always the same
    content.CurrentAnimationState.copyxywh(self.currentAnimationState);
    content.CurrentAnimationState.SubstractFrame(frame,0);
    content.animateTo(animate);
  end;

end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.GetAnimationState;
var phase: single;
begin
  if true then begin //todo!!!!!!!!!!!!!!!!!!!!!!!
    if (last.initialized) and (next.initialized) and
      ((animationstart<0) or (decoNow-animationstart < animationduration)) then begin
      if animationstart<0 then animationstart := decoNow;
      phase := (decoNow-animationstart)/animationduration; //animationtime
      //make curve slower at ends and sharper at middle
      if phase<0.5 then phase := sqr(2*phase)/2 else phase := 1 - sqr(2*(1-phase))/2;
      CurrentAnimationState.x1 := last.x1+round((next.x1-last.x1)*phase);
      CurrentAnimationState.x2 := last.x2+round((next.x2-last.x2)*phase);
      CurrentAnimationState.y1 := last.y1+round((next.y1-last.y1)*phase);
      CurrentAnimationState.y2 := last.y2+round((next.y2-last.y2)*phase);
      CurrentAnimationState.h := last.h+round((next.h-last.h)*phase);
      CurrentAnimationState.w := last.w+round((next.w-last.w)*phase);
      CurrentAnimationState.opacity := last.opacity+((next.opacity-last.opacity)*phase);
      CurrentAnimationState.initialized := true;
    end else begin
      {should be "next" here}
      CurrentAnimationState.x1 := base.x1;
      CurrentAnimationState.x2 := base.x2;
      CurrentAnimationState.y1 := base.y1;
      CurrentAnimationState.y2 := base.y2;
      CurrentAnimationState.h := base.h;
      CurrentAnimationState.w := base.w;
      CurrentAnimationState.opacity := base.Opacity;
      CurrentAnimationState.initialized := true;
    end;
  end;
end;

procedure DAbstractElement.update;
begin
  GetAnimationState;
end;

{----------------------------------------------------------------------------}


constructor DAbstractElement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  visible := true;
  base := Txywha.Create(self);
  last := Txywha.Create(self);
  next := Txywha.Create(self);
  CurrentAnimationState := Txywha.Create(self);
end;

{----------------------------------------------------------------------------}

destructor DAbstractElement.destroy;
begin
  //actulally this is not needed as they are owned by the class
{  freeandnil(base);
  freeandnil(last);
  freeandnil(next);}

  inherited;
end;

{=============================================================================}
{=================== Single interface element ===============================}
{=============================================================================}

procedure DSingleInterfaceElement.rescale;
begin
  {$WARNING Memory Leak here}
  if frame <> nil then FrameResize3x3;
  if content <> nil then begin
    //content.base.copyxywh(base);
    //content.base.SubstractFrame(frame,2);
    content.rescale;   //todo
  end;
end;

{----------------------------------------------------------------------------}

constructor DSingleInterfaceElement.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FrameOnTop := false;
  //ID := -1;
  //OwnsContent := false;
  FrameOpacity := 0.8;
  isMouseOver := false;
  CanMouseOver := false;
  CanDrag := false;
end;

{-----------------------------------------------------------------------------}

destructor DSingleInterfaceElement.destroy;
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
end;

{----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.FrameResize3x3;
var ScaledImageParts: array [0..2,0..2] of TCastleImage;
    ix,iy: integer;
    UnscaledWidth, UnscaledHeight:integer;
    SourceXs, SourceYs, DestXs, DestYs: TVector4Integer;
begin
  FrameReady := false;
  if base.initialized = false then begin
    writeLnLog('DAbstractInterfaceElement.FrameResize3x3','ERROR: Base is not initialized!');
  end;

  //FreeAndNil(FrameImage);
  if FrameImage<>nil then WriteLnLog('DSingleInterfaceElement.FrameResize3x3','ERROR: FrameImage is not nil! (memory leak)');
  FrameImage := frame.SourceImage.CreateCopy as TRGBAlphaImage;

  UnscaledWidth := FrameImage.width;
  UnscaledHeight := FrameImage.height;

  {check if minimal frame size is larger than the requested frame size}
  if frame.cornerLeft+frame.cornerRight+1 > base.w then begin
    writeLnLog('DAbstractInterfaceElement.FrameResize3x3','Reset backwards base.w = '+inttostr(base.w)+' / cornerLeft+cornerRight = '+inttostr(frame.cornerLeft+frame.cornerRight));
    base.w := frame.cornerLeft+frame.cornerRight+1;
    base.backwardsetsize(base.w,-1);
  end;
  if frame.cornerTop+frame.cornerBottom+1>base.h then begin
    writeLnLog('DAbstractInterfaceElement.FrameResize3x3','Reset backwards base.h = '+inttostr(base.h)+' / cornerTop+cornerBottom = '+inttostr(frame.cornerTop+frame.cornerBottom));
    base.h := frame.cornerTop+frame.cornerBottom+1;
    base.backwardsetsize(-1,base.h);
  end;

  SourceXs[0] := 0;
  SourceXs[1] := frame.cornerLeft;
  SourceXs[2] := UnscaledWidth-frame.cornerRight;
  SourceXs[3] := UnscaledWidth;
  SourceYs[0] := 0;
  SourceYs[1] := frame.cornerBottom;
  SourceYs[2] := UnscaledHeight-frame.cornerTop;
  SourceYs[3] := UnscaledHeight;
  DestXs[0] := 0;
  DestXs[1] := frame.cornerLeft;
  DestXs[2] := base.w-frame.cornerRight;
  DestXs[3] := base.w;
  DestYs[0] := 0;
  DestYs[1] := frame.cornerBottom;
  DestYs[2] := base.h-frame.cornerTop;
  DestYs[3] := base.h;

  for ix := 0 to 2 do
   for iy := 0 to 2 do begin
     ScaledImageParts[ix,iy] := TRGBAlphaImage.create;
     ScaledImageParts[ix,iy].SetSize(SourceXs[ix+1]-SourceXs[ix],SourceYs[iy+1]-SourceYs[iy]);
     ScaledImageParts[ix,iy].Clear(Vector4Byte(0,0,0,0));
     ScaledImageParts[ix,iy].DrawFrom(FrameImage,0,0,SourceXs[ix],SourceYs[iy],SourceXs[ix+1]-SourceXs[ix],SourceYs[iy+1]-SourceYs[iy],dmBlendSmart);
     ScaledImageParts[ix,iy].Resize(DestXs[ix+1]-DestXs[ix],DestYs[iy+1]-DestYs[iy],riNearest);
   end;

  FrameImage.SetSize(base.w,base.h,1);
  FrameImage.Clear(Vector4byte(0,0,0,0));
  for ix := 0 to 2 do
    for iy := 0 to 2 do FrameImage.DrawFrom(ScaledImageParts[ix,iy],DestXs[ix],DestYs[iy],0,0,DestXs[ix+1]-DestXs[ix],DestYs[iy+1]-DestYs[iy],dmBlendSmart);

  for ix := 0 to 2 do
    for iy := 0 to 2 do freeAndNil(ScaledImageParts[ix,iy]);

  //and burn the burner
  //FrameImage.DrawFrom(BURNER_IMAGE,0,0,base.x1,base.y1,base.w,base.h,dmMultiply);

  initGLPending := true;
end;

{----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.InitGL;
begin
  //content makes his own initGL on first draw
  if InitGLPending then begin
    InitGLPending := false;
    if FrameImage<>nil then begin
      freeandnil(GLFrame);
      GLFrame := TGLImage.create(FrameImage,true,true);
      FrameImage := nil;
      FrameReady := true;
    end;
  end;
end;

{----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.DrawFrame; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if frame <> nil then begin
    if FrameReady then begin
      GLFrame.color := vector4single(1,1,1,currentAnimationState.Opacity * FrameOpacity);     //todo
      GLFrame.Draw(currentAnimationState.x1,currentAnimationState.y1,currentAnimationState.w,currentAnimationState.h);
    end else begin
      if InitGLPending then InitGL;
    end;
  end;
end;

procedure DSingleInterfaceElement.DrawContent; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  //todo
  if content <> nil then begin
    //Content.base.copyxywh(currentAnimationState);
    Content.draw;
  end;
end;

procedure DSingleInterfaceElement.draw;
begin
  update;
  if not visible then exit;

  if FrameOnTop then begin
    DrawContent;
    DrawFrame;
  end else begin
    DrawFrame;
    DrawContent;
  end;

end;

{========== Abstract intarface element : Mouse handling =====================}

function DSingleInterfaceElement.IAmHere(xx,yy: integer): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  //get current element location... maybe, use not current animation, but "base"? Or completely ignore items being animated?
  GetAnimationState;
  if (xx >= CurrentAnimationState.x1) and (xx <= CurrentAnimationState.x2) and
     (yy >= CurrentAnimationState.y1) and (yy <= CurrentAnimationState.y2)
  then
    result := true
  else
    result := false;
end;

{-----------------------------------------------------------------------------}

function DSingleInterfaceElement.ifMouseOver(xx,yy: integer; RaiseEvents: boolean; AllTree: boolean): DAbstractElement;
begin
  result := nil;
  if IAmHere(xx,yy) then begin
    if RaiseEvents then begin
      if isMouseOver = false then begin
        if Assigned(onMouseEnter) then onMouseEnter(self,xx,yy);
        isMouseOver := true;
      end;
      if Assigned(onMouseOver) then onMouseOver(self,xx,yy);
    end;
    if CanMouseOver then  //todo
      result := self
  end else begin
    if isMouseOver and RaiseEvents then begin
      if Assigned(onMouseLeave) then onMouseLeave(self,xx,yy);
      isMouseOver := false;
    end;
  end;
end;

function DInterfaceElement.ifMouseOver(xx,yy: integer; RaiseEvents: boolean; AllTree: boolean): DAbstractElement;
var i: integer;
    tmplink: DAbstractElement;
begin
  result := inherited ifMouseOver(xx,yy,RaiseEvents,AllTree);
  //if rsult<>nil ... *or drag-n-drop should get the lowest child?

  // recoursively scan all children
  for i := 0 to children.count-1 do begin
    tmpLink := children[i].ifMouseOver(xx,yy,RaiseEvents,AllTree);
    if tmpLink <> nil then begin
      result := tmpLink;
      if not AllTree then break; // if drag-n-drop one is enough
    end;
  end;
end;

{-----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.startdrag(x,y: integer);
begin
  dragx := base.x1 - x;
  dragy := base.y1 - y;
end;


procedure DSingleInterfaceElement.drag(x,y: integer);
begin
  base.x1 := dragx + x;
  base.y1 := dragy + y;
end;

{=============================================================================}
{=========================== interface element ===============================}
{=============================================================================}

 constructor DTImer.create;
 begin
   inherited;
   enabled := false;
   StartTime := -1;
 end;

{-----------------------------------------------------------------------------}

procedure DTimer.update;
begin
  if StartTime<0 then StartTime := DecoNow else
  if (DecoNow-StartTime) >= Interval then begin
    Enabled := false;
    if Assigned(onTimer) then onTimer;
  end;
end;

{-----------------------------------------------------------------------------}

procedure DTimer.settimeout(Seconds: DTime);
begin
  StartTime := -1;
  Enabled := true;
  Interval := Seconds;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceElement.rescale;
var i: integer;
begin
  inherited;
  //{$WARNING Memory Leak here}
  for i:=0 to children.Count-1 do children[i].rescale;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceElement.update;
begin
  inherited;
  if timer.enabled then timer.update;
end;

{-----------------------------------------------------------------------------}

constructor DInterfaceElement.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  if AOwner is DSingleInterfaceElement then parent := DSingleInterfaceElement(AOwner);
  ScaleToChildren := false;
  children := DInterfaceElementsList.Create(true);
  timer := DTimer.create;
end;

{----------------------------------------------------------------------------}

destructor DInterfaceElement.destroy;
begin
  freeandnil(children);   //this should fire as recoursive because children owns elements, which in turn will fire their destructors onfree
  freeandnil(timer);
  inherited;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.draw;
var i: integer;
begin
  inherited;
  for i := 0 to children.Count-1 do children[i].draw;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Grab(Child: DSingleInterfaceElement);
begin
  children.Add(Child);
  if (Child is DSingleInterfaceElement) then DSingleInterfaceElement(Child).Parent := self; //not sure about this line
  //{Child.ID := }InterfaceList.Add(Child); //global ID of the element
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.RescaleToChildren(animate: TAnimationStyle);
var i: integer;
    x1,y1,x2,y2: integer;
begin
  if children.count>0 then begin
    x1 := window.width;
    y1 := window.height;
    x2 := 0;
    y2 := 0;
    for i := 1 to children.count-1 do begin
      if x1>children[i].base.x1 then x1 := children[i].base.x1;
      if y1>children[i].base.y1 then y1 := children[i].base.y1;
      if x2>children[i].base.x2 then x2 := children[i].base.x2;
      if y2>children[i].base.y2 then y2 := children[i].base.y2;
    end;
    self.setIntSize(x1,y1,x2,y2,animate);
  end
  else WriteLnLog('DInterfaceElement.RescaleToChildren','No children for resale to');
end;

{-----------------------------------------------------------------------}

function DInterfaceElement.MouseOverTree(xx,yy: integer): boolean;
var tmp: DAbstractElement;
begin
  // maybe rewrite it using isMouseOver - the idea is still a little different
  tmp := self.ifMouseOver(xx,yy,false,false);
  if (tmp <> nil) and (tmp is DSingleInterfaceElement) and (DSingleInterfaceElement(tmp).CanMouseOver){ and (tmp.base.opacity>0)} then
    isMouseOverTree := true
  else
    isMouseOverTree := false;
  //base.opacity breaks the algorithm, if transparent item is above (i.e. below) the opaque element
  result := isMouseOverTree;
end;

end.

