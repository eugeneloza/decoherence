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

Type
 { Several types of frames, including with captions }
 DFrame = class(TComponent)
 public
   SourceImage: TRGBAlphaImage;
   {frame borders}
   CornerTop, CornerBottom, CornerLeft, CornerRight: integer;
   Rectagonal: boolean;
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
end;

 { constants for special scaling cases }
const FullWidth = -1;
      FullHeight = -2;
      ProportionalScale = -3;
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
    Opacity: float;
    Initialized: boolean;
    { assign float and convert to Integer }
    procedure SetSize(const NewX,NewY,NewW,NewH: float);
    { get integer and convert to float / for fixed-size objects }
    procedure BackwardSetSize(const NewW,NewH: integer);
    procedure BackwardSetXYWH(const NewX,NewY,NewW,NewH: integer);
    {substract frame width from the base}
    procedure SubstractFrame(Frame: DFrame; AdditionalGap: integer = 0);
    { transform floats to integer }
    procedure Recalculate;
    constructor Create(AOwner: TComponent); override;
    { provides for proportional width/height scaling for some images }
    procedure FixProportions(ww,hh: integer);
  end;

type
  { to make copies }
  Txywha = class(Txywh)
  public
    //function makecopy:Txywh;
    procedure CopyXYWH(Source: Txywh);
  end;

Type
  { most abstract container suitable for images, labels and interface elements
    Just defines the box and rescaling }
  DAbstractElement = class(TComponent)
  public
    {stores current animation state, recalculated by GetAnimationState at every render}
    CurrentAnimationState: Txywha;
    procedure GetAnimationState; virtual;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { changes the scale of the element relative to current window size }
    procedure Rescale; virtual;
    { draw the element / as abstract as it might be :) }
    procedure Draw; virtual; abstract;
    //procedure InitGL; virtual; abstract; //no need for an abstract method?
    { updates the data of the class with current external data, most often
      just gets the current animation state }
    procedure Update; virtual;
  private
    { Last and Next animation states. }
    Last, Next: Txywha;
//    Free_on_end: boolean;
    fVisible: boolean;
    procedure SetVisible(Value: boolean);
  public
    { these values are "strict" and unaffected by animations. Usually determines
      the basic stage and implies image rescale and init GL. }
    AnimationStart: TDateTime;
    AnimationDuration: float;
    {base state of the element. contains its coordinates and width/height}
    Base: Txywha;
    {source width/height of the element. Used to preserve proportions while scaling}
    RealWidth, RealHeight: integer;
    procedure SetBaseSize(const NewX,NewY,NewW,NewH,NewO: float; Animate: TAnimationStyle); virtual;
    procedure SetIntSize(const x1,y1,x2,y2:integer; Animate: TAnimationStyle); virtual;
    {if the element is visible, if false then draw will not be called.
     PAY ATTENTION: if assigned to a single interface element then the animations
     and initGL will occur as they would normally. BUT if assigned to a
     composite parent element, its children WILL NOT do anything like this and
     will be frozen until visible=true. Maybe I'll fix this.}
    property Visible: boolean read fVisible write SetVisible;
    {animates the interface element from current state to base state,
     Important: GetAnimationState must be called before setting basesize
     of the element as AnimateTo uses currentAnimationState}
    procedure AnimateTo(Animate: TAnimationStyle; Duration: float = DefaultAnimationDuration);
  end;

{Definition of simple procedures for (mouse) events}
//type TSimpleProcedure = procedure(sender: DAbstractElement) of Object;
type TSimpleProcedure = procedure of Object;
type TXYProcedure = procedure(Sender: DAbstractElement; x,y: integer) of Object;

Type
  {Element with a frame and content}
  DSingleInterfaceElement = class(DAbstractElement)
  public
    {Higher-level element. Seldomly used in specific cases}
    Parent: DAbstractElement;
    {whether Interface element owns its contents? If true they'll bee freed
     on destroy // using TComponent Inheritance for now}
    //OwnsContent: boolean;
    { content of the Interface element: label or image }
    Content: DAbstractElement;    {actually DAbstractImage, but cyclic references are not allowed}
    { multiplier to opacity <1, e.g. overall opacity=0.8 FrameOpacity=0.8, frame final Opacity=0.8*0.8 }
    FrameOpacity: float;
    { frame behind the Interface element }
    Frame: DFrame;
    {specifices if the frame draws over content or vice versa}
    FrameOnTop: boolean;
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
    procedure StartDrag(x,y: integer);
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
      Enabled: boolean;
      {how long (in seconds) will it take the timer to fire}
      Interval: DTime;
      {action to preform}
      onTimer: TSimpleProcedure;
      constructor Create;
      {a simple way to set and run timer}
      procedure SetTimeOut(Seconds: DTime);
      {check if the timer finished and run onTimer if true}
      procedure Update;
  end;

Type
  {An interface element, that can contain "children"}
  DInterfaceElement = class(DSingleInterfaceElement)
  public
    {if the interface element rescales each time children rescale}
    ScaleToChildren: boolean;
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

uses SysUtils, CastleLog, castleFilesUtils,
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

  BlackFrame := DFrame.Create(Window);
  with BlackFrame do begin
    SourceImage := LoadImageSafe(ApplicationData(FramesFolder+'blackframe.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    CornerTop := 0; CornerBottom := 0; CornerLeft := 0; CornerRight := 0;
  end;

  InitCompositeInterface;

  //InterfaceList := DInterfaceElementsList.create(false);

  WriteLnLog('InitInterface','finished');
end;

{=============================================================================}

constructor DFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Rectagonal := true;
end;

destructor DFrame.Destroy;
begin
  FreeAndNil(SourceImage);
  inherited;
end;

{=============================================================================}
{=========================== Abstract element ================================}
{=============================================================================}


constructor Txywh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Initialized := false;
end;

{----------------------------------------------------------------------------}

procedure Txywh.SetSize(const NewX,NewY,NewW,NewH: float);
begin
  if (Abs(NewX) > 1) or (Abs(NewY) > 1) or
     (((NewW<0) or (NewW>1)) and (not FloatsEqual(NewW,ProportionalScale) and not FloatsEqual(NewW,FullWidth) and not FloatsEqual(NewW,FullHeight))) or
     (((NewH<0) or (NewH>1)) and (not FloatsEqual(NewW,ProportionalScale) and not FloatsEqual(NewW,FullHeight))) then
  begin
    WriteLnLog('Txywh.setsize','ERROR: Incorrect newx,newy,neww,newh!');
    Exit;
  end;

  { stop if nothing was changed }
  if FloatsEqual(fx,NewX) and FloatsEqual(fy,NewY) and FloatsEqual(fw,NewW) and FloatsEqual(fh,NewH) then begin
    initialized := true; //to avoid bugs
    Exit;
  end;

  fx := NewX;
  fy := NewY;
  fw := NewW;
  fh := NewH;

  Recalculate;
end;

{----------------------------------------------------------------------------}

procedure Txywh.Recalculate;
begin
  { convert float to integer }

  if fx >= 0 then
    x1 := Round(Window.Height*fx)
  else
    x1 := Window.Width + Round(Window.Height*fx);

  if fy>=0 then
    y1 := Round(Window.Height*fy)     // turn over y-axis?
  else
    y1 := Window.Height + Round(Window.Height*fy);

  if FloatsEqual(fw,FullWidth) then begin
    w := Window.Width;
    x1 := 0
  end
  else
  if FloatsEqual(fw,FullHeight) then
    w := Window.Height
  else
    w := Round(Window.Height*fw);

  if FloatsEqual(fh,FullHeight) then begin
    h := Window.Height;
    y1 := 0
  end else
    h := Round(Window.Height*fh);

  x2 := x1+w;
  y2 := y1+h;

  Initialized := true;
end;

{----------------------------------------------------------------------------}


procedure Txywh.BackwardSetSize(const NewW,NewH: integer);
begin
  if NewW>0 then begin
    w := NewW;
    fw := NewW/Window.Height;
    if x1+w > Window.Width then begin
      x1 := Window.Width - w;
      fx := x1/Window.Height;
    end;
    x2 := x1+w;
  end;
  if NewH>0 then begin
    h := NewH;
    fh := NewH/Window.Height;
    if y1+h > Window.Height then begin
      y1 := Window.Height - h;
      fy := y1/Window.Height;
    end;
    y2 := y1+h;
  end;
end;

procedure Txywh.BackwardSetXYWH(const NewX,NewY,NewW,NewH: integer);
begin
  //todo check for consistency
  x1 := NewX;
  y1 := NewY;
  w := NewW;
  h := NewH;
  if NewX<Window.Width div 2 then fx := NewX/Window.Height else fx := (NewX-Window.Width)/Window.Height;
  fy := NewY/Window.Height;
  fw := NewW/Window.Height;
  fh := NewH/Window.Height;
  Initialized := true;
end;

{----------------------------------------------------------------------------}

Procedure Txywh.SubstractFrame(Frame: DFrame; AdditionalGap: integer = 0);
Begin
  If (Frame<>nil) and (Frame.Rectagonal) then begin
    x1 := x1 + Frame.CornerLeft+AdditionalGap;
    x2 := x2 - Frame.CornerRight-AdditionalGap;
    w := w - Frame.CornerLeft - Frame.CorneRright -2*AdditionalGap;
    y1 := y1 + Frame.CornerTop+AdditionalGap;
    y2 := y2 - Frame.CornerBottom-AdditionalGap;
    h := h - Frame.CornerTop - Frame.CornerBottom -2*AdditionalGap;
    BackwardSetXYWH(x1,y1,w,h);
  end else if AdditionalGap<>0 then begin
    x1 := x1 + AdditionalGap;
    x2 := x2 - AdditionalGap;
    w := w - 2*AdditionalGap;
    y1 := y1 + AdditionalGap;
    y2 := y2 - AdditionalGap;
    h := h - 2*AdditionalGap;
    BackwardSetXYWH(x1,y1,w,h);
  end;
End;

{----------------------------------------------------------------------------}

procedure Txywh.FixProportions(ww,hh: integer);
begin
  if FloatsEqual(fw,ProportionalScale) then
    w := Round(h*ww/hh)
  else                                 //they can't be proportional both
  if FloatsEqual(fh,ProportionalScale) then
    h := Round(w*hh/ww);
end;

{----------------------------------------------------------------------------}

procedure Txywha.copyXYWH(Source: Txywh);
begin
  x1 := Source.x1;
  y1 := Source.y1;
  x2 := Source.x2;
  y2 := Source.y2;
  w := Source.w;
  h := Source.h;
  fx := Source.fx;
  fy := Source.fy;
  fw := Source.fw;
  fh := Source.fh;
  Opacity := Source.Opacity;
  Initialized := Source.Initialized;
end;

{============================================================================}

procedure DAbstractElement.Rescale;
begin
  Base.Recalculate;
  Last.Recalculate;
  Next.Recalculate;
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
    Last.CopyXYWH(Base); //todo: CurrentAnimationState
    Next.CopyXYWH(Base);
    case Animate of
      {just grabs some previous locations and animates the item from there to base
       requires that CurrentAnimationState is initialized... TODO}
      asDefault: Last.CopyXYWH(CurrentAnimationState);
      {fades in/out element}
      asFadeIn:  Last.Opacity := 0;
      asFadeOut: Next.Opacity := 0;
      {asFadeOutSuicide}
      {zooms in/out element}
      asZoomIn:  begin
                   Last.BackwardSetSize(1,1);
                   Last.Opacity := 0;
                 end;
      asZoomOut: begin
                   Next.BackwardSetSize(1,1);
                   Next.Opacity := 0;
                 end;
      {asZoomOutSuicide}
      asFlyInRandom,asFlyOutRandom: begin
                       mx := drnd.Random*Window.Width/Window.Height;  //mult by aspect ratio
                       my := drnd.Random;
                       case drnd.Random(4) of
                         0: mx :=  0.0001;
                         1: mx := -0.0001;
                         2: my :=  0.0001;
                         3: my := -0.0001;
                       end;
                       if Animate=asFlyInRandom then begin
                         Last.fx := mx;
                         Last.fy := my;
                       end else begin
                         Next.fx := mx;
                         Next.fy := my;
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

procedure DAbstractElement.SetBaseSize(const NewX,NewY,NewW,NewH,NewO: float; Animate: TAnimationStyle);
begin
  GetAnimationState;
  Base.setsize(NewX,NewY,NewW,NewH);
  Base.opacity := NewO;
  AnimateTo(Animate);
  //rescale; //????
end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.SetIntSize(const x1,y1,x2,y2:integer; Animate: TAnimationStyle);
begin
  GetAnimationState;
  Base.BackwardSetXYWH(x1,y1,x2-x1,y2-y1);
  AnimateTo(Animate);
  //rescale; //????
end;

{----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.SetBaseSize(const NewX,NewY,NewW,NewH,NewO: float; Animate: TAnimationStyle);
begin
  inherited SetBaseSize(NewX,NewY,NewW,NewH,NewO,Animate);
  ResetContentSize(Animate);
end;

{----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.SetIntsize(const x1,y1,x2,y2:integer; Animate: TAnimationStyle);
begin
  inherited SetIntsize(x1,y1,x2,y2,Animate);
  ResetContentSize(Animate);
end;

{----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.ResetContentSize(Animate: TAnimationStyle);
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

end;

{----------------------------------------------------------------------------}

procedure DAbstractElement.GetAnimationState;
var Phase: float;
begin
  if true then begin //todo!!!!!!!!!!!!!!!!!!!!!!!
    if (Last.Initialized) and (Next.Initialized) and
      ((Animationstart<0) or (DecoNow-AnimationStart < AnimationDuration)) then begin
      if AnimationStart<0 then AnimationStart := DecoNow;
      Phase := (DecoNow-AnimationStart)/AnimationDuration; //animationtime
      //make curve slower at ends and sharper at middle
      if Phase<0.5 then Phase := Sqr(2*Phase)/2 else Phase := 1 - Sqr(2*(1-Phase))/2;
      CurrentAnimationState.x1 := Last.x1+Round((Next.x1-Last.x1)*Phase);
      CurrentAnimationState.x2 := Last.x2+Round((Next.x2-Last.x2)*Phase);
      CurrentAnimationState.y1 := Last.y1+Round((Next.y1-Last.y1)*Phase);
      CurrentAnimationState.y2 := Last.y2+Round((Next.y2-Last.y2)*Phase);
      CurrentAnimationState.h := Last.h+Round((Next.h-Last.h)*Phase);
      CurrentAnimationState.w := Last.w+Round((Next.w-Last.w)*Phase);
      CurrentAnimationState.Opacity := Last.Opacity+((Next.opacity-Last.Opacity)*Phase);
      CurrentAnimationState.Initialized := true;
    end else begin
      {should be "next" here}
      CurrentAnimationState.x1 := Base.x1;
      CurrentAnimationState.x2 := Base.x2;
      CurrentAnimationState.y1 := Base.y1;
      CurrentAnimationState.y2 := Base.y2;
      CurrentAnimationState.h := Base.h;
      CurrentAnimationState.w := Base.w;
      CurrentAnimationState.Opacity := Base.Opacity;
      CurrentAnimationState.Initialized := true;
    end;
  end;
end;

procedure DAbstractElement.Update;
begin
  GetAnimationState;
end;

{----------------------------------------------------------------------------}


constructor DAbstractElement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Visible := true;
  Base := Txywha.Create(Self);
  Last := Txywha.Create(Self);
  Next := Txywha.Create(Self);
  CurrentAnimationState := Txywha.Create(Self);
end;

{----------------------------------------------------------------------------}

destructor DAbstractElement.Destroy;
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

procedure DSingleInterfaceElement.Rescale;
begin
  {$WARNING Memory Leak here}
  if Frame <> nil then FrameResize3x3;
  if Content <> nil then begin
    //content.base.copyxywh(base);
    //content.base.SubstractFrame(frame,2);
    Content.Rescale;   //todo
  end;
end;

{----------------------------------------------------------------------------}

constructor DSingleInterfaceElement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FrameOnTop := false;
  //ID := -1;
  //OwnsContent := false;
  FrameOpacity := 0.8;
  isMouseOver := false;
  CanMouseOver := false;
  CanDrag := false;
end;

{-----------------------------------------------------------------------------}

destructor DSingleInterfaceElement.Destroy;
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

