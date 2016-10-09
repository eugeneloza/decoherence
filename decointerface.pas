{Copyright (C) 2012-2016 Yevhen Loza

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
unit decointerface;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, sysutils,
  CastleLog, castleFilesUtils,
  castleVectors, CastleGLImages, CastleImages,
  decoglobal;

const Frames_Folder='interface/frames/';

{1/17 of window.height is a "unit" in GUI scale.
Basically calculated as 4 characters vertical space allocation
3 lines of buttons for each
and add one line at the bottom for menu button and other stuff
i.e. 3*4+1 units in window.height
Most often equal scale is used for width - in fractions of height to maintain squares etc.}

const GUI_scale_unit_float = 1/(4*3+1);

{ animation takes place from initial TAnimationState to Final TAnimationState}
type TAnimationState = record
  x, y, h, w : integer;
  Opacity : single;
end;

const ZeroAnimation : TAnimationState = (
  x : 0;
  y : 0;
  h : 0;
  w : 0;
  Opacity : 1
  );

const defaultAnimationLength = 1000; {in ms}

Type DAbstractElement = class(TComponent)
 public
  x, y, h, w : integer;
  Opacity : single;
  color : TVector4Single;
  procedure drawMe; virtual; abstract;
  procedure InitGL; virtual; abstract;
  procedure DestroyMe; virtual; abstract;
end;

Type DFrame = class(TComponent)
 public
   Image : TRGBAlphaImage;
   cornerTop, cornerbottom, cornerLeft, cornerRight : integer;
   constructor Create(AOwner : TComponent); override;
 //image
end;

Type DAbstractInterfaceElement = class(DAbstractElement)
 public
  { content of the Interface element: label or image }
  content : DAbstractElement;
  { multiplier to opacity <1, e.g. overall opacity=1 FrameOpacity=0.8, frame final Opacity=1*0.8 }
  FrameOpacity : float;
  { frame around the Interface element }
  frame : DFrame;
  //Parent:TComponent;
  { initializes GL content of the interface element }
  procedure InitGL; override;
  { looks like destructor... but I couldn't get destructor to work }
  procedure DestroyMe; override;
  { creates the instance }
  constructor Create(AOwner : TComponent); override;
  { changes current values based on current animation }
  procedure AnimateMe; virtual; abstract;
 private
   FrameImage : TRGBAlphaImage;
   FrameGL : TGLImage;
   procedure FrameResize3x3;
   procedure ResizeChildren; virtual; Abstract;
 private
  doAnimation : boolean;
  procedure AskParentForAnimation; virtual; abstract;
end;

type DInterfaceChildrenList = specialize TFPGObjectList<DAbstractInterfaceElement>;

Type DInterfaceElement = class(DAbstractInterfaceElement)
 public
  children : DInterfaceChildrenList;
  parent : DAbstractInterfaceElement;
  constructor Create(AOwner : TComponent); override;
  procedure DrawMe; override;
  procedure DestroyMe; override;
  { saves the current state as TAnimationState }
  Function GetAnimationState : TAnimationState;
  { animates the element from current to some TAnimationState }
  Procedure AnimateTo(NewAnimationState : TAnimationState;
                    NewAnimationLength : integer = defaultAnimationLength);
  { changes current values based on current animation }
  procedure AnimateMe; override;
 private
  procedure ResizeChildren; override;
 private
  LastAnimationState,NextAnimationState,CurrentAnimationState : TAnimationState;
  AnimationStartTime,AnimationLength : TDateTime;
  procedure AskParentForAnimation; override;
end;

Type DInterfaceContainer = class(DInterfaceElement)
 public
  {integer scale of GUI_scale_unit. I'm not very sure if it's correct to use it,
   because it will make a 'hole' of window.height mod 17 size at the bottom...
   but let it be for now, I can always remove it later}
  GUI_scale : integer;
  {This is the major workspace container size for action buttons, perks, text,
  action bars and etc. Most of the interface is scaled and organized relative
  to it. I might make it as a separate GUI container, but not sure if it's needed.}
  mid_h, mid_w, mid_startx, mid_starty : integer;
end;

var frames : array[0..1] of DFrame;
    GUI : DInterfaceContainer;

procedure MakeInterface1;

procedure InitInterface;
procedure ResizeInterface;
procedure DrawInterface;

implementation

{----------------------------------------------------------------}
{----------------------------------------------------------------}
{----------------------------------------------------------------}

var BURNER_IMAGE_UNSCALED,BURNER_IMAGE:TCastleImage;
procedure Init_burner_image;
begin
  WriteLnLog('Init_burner_image','started');
  if BURNER_IMAGE_UNSCALED = nil then
    BURNER_IMAGE_UNSCALED := LoadImage(ApplicationData(Interface_Foler+'burner_Pattern_203_CC0_by_Nobiax_diffuse.png'), [TRGBImage]) as TRGBImage;
  if BURNER_IMAGE=nil then begin
    BURNER_IMAGE := BURNER_IMAGE_UNSCALED.MakeCopy;
    BURNER_IMAGE.Resize(window.width, window.height, riBilinear);
  end;
  if (BURNER_IMAGE.height <> window.height) or (BURNER_IMAGE.width <> window.width) then begin
    FreeAndNil(BURNER_IMAGE);
    BURNER_IMAGE := BURNER_IMAGE_UNSCALED.MakeCopy;
    BURNER_IMAGE.Resize(window.width, window.height, riBilinear);
  end;
  WriteLnLog('Init_burner_image','finished');
end;

{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}

procedure MakeInterface1;
var newElement:DInterfaceElement;
begin
  NewElement := DInterfaceElement.create(GUI);
  NewElement.x := GUI.mid_startx;
  NewElement.y := GUI.mid_starty+GUI.mid_h-GUI.GUI_scale;
  NewElement.w := GUI.GUI_scale;
  NewElement.h := GUI.GUI_scale;
  NewElement.AnimateTo(NewElement.GetAnimationState);
  NewElement.frame := frames[0];
  NewElement.InitGL;
  GUI.children.add(NewElement);

  NewElement := DInterfaceElement.create(GUI);
  NewElement.x := GUI.mid_startx+GUI.GUI_scale;
  NewElement.y := GUI.mid_starty+GUI.mid_h-GUI.GUI_scale;
  NewElement.w := GUI.GUI_scale;
  NewElement.h := GUI.GUI_scale;
  NewElement.AnimateTo(NewElement.GetAnimationState);
  NewElement.frame := frames[0];
  NewElement.InitGL;
  GUI.children.add(NewElement);
end;

{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}

procedure DrawInterface;
begin
  //GUI draw children recoursive
  GUI.drawMe;
end;

{---------------------------------------------------------------------------}

procedure InitInterface;
begin
  WriteLnLog('InitInterface','started');
  Init_burner_image;
  GUI := DInterfaceContainer.create(Window);
  frames[0] := DFrame.create(Window);
  with frames[0] do begin
    image := LoadImage(ApplicationData(Frames_Folder+'frame.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 1;CornerBottom := 1;cornerLeft := 1;CornerRight := 1;
  end;
  frames[1] := DFrame.create(Window);
  with frames[1] do begin
    image := LoadImage(ApplicationData(Frames_Folder+'frame_caption.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 19;CornerBottom := 1;cornerLeft := 1;CornerRight := 1;            //todo: variable top line!
  end;

//  GUI.frame := Dframe.create(frames[0],GUI);
  ResizeInterface;
  WriteLnLog('InitInterface','finished');
end;

{---------------------------------------------------------------------------}

procedure ResizeInterface;
begin
  WriteLnLog('ResizeInterface','started');
  //Rescale Burner image if necessary
  Init_burner_image;
  {GUI always takes the whole screen and every child is scaled against it}
  GUI.x := 0;
  GUI.y := 0;
  GUI.w := window.width;
  GUI.h := window.height;
  GUI.GUI_scale := round(GUI_scale_unit_float*GUI.h);
  GUI.mid_h := round((1-1/17)*GUI.h);
  GUI.mid_w := round((1-8/17)*GUI.h);
  GUI.mid_startx := round(4/17*GUI.h);
  GUI.mid_starty := 0;
  GUI.frame := nil;
  GUI.content := nil;
  //GUI resize children recoursive
  WriteLnLog('ResizeInterface','finished');
end;

{============================================================================}

constructor Dframe.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
end;

{============================================================================}

procedure DInterfaceElement.DrawMe;
var i:integer;
begin
  if frameGL<>nil then begin
    //frameGL.Draw3x3(x,y,w,h,frame.cornerTop,frame.CornerRight,frame.CornerBottom,Frame.CornerLeft);
    FrameGL.Draw(x,y,w,h);
    color[3] := Opacity*FrameOpacity;
    FrameGL.color := Color;
  end;
  if content<>nil then begin
    content.opacity := Opacity;   //todo: different opacity for content
    content.drawMe;
  end;
  //draw children recoursive
  for i := 0 to Children.count-1 do Children[i].DrawMe;
end;

{============================================================================}

procedure DInterfaceElement.ResizeChildren;
var i:integer;
begin
 for i := 0 to children.count-1 do begin
   //todo
   children[i].resizeChildren;
 end;
end;

constructor DInterfaceElement.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
  children := DInterfaceChildrenList.create(true);
  LastAnimationState := ZeroAnimation;
end;

procedure DInterfaceElement.DestroyMe;
begin
 freeandnil(children);
 inherited;
end;

{-----------------------------------------------------------------------------}


procedure DAbstractInterfaceElement.FrameResize3x3;
var ScaledImageParts: array [0..2,0..2] of TCastleImage;
    ix,iy: integer;
    UnscaledWidth,UnscaledHeight:integer;
    SourceXs,SourceYs,DestXs,DestYs: TVector4Integer;
    CornersVector:TVector4Integer;
begin
  FrameImage := frame.Image.CreateCopy as TRGBAlphaImage;
  CornersVector := Vector4Integer(frame.cornerTop,frame.cornerLeft,frame.cornerBottom,frame.cornerRight);

  UnscaledWidth := FrameImage.width;
  UnscaledHeight := FrameImage.height;

  SourceXs[0] := 0;
  SourceXs[1] := CornersVector[3];
  SourceXs[2] := UnscaledWidth-CornersVector[1];
  SourceXs[3] := UnscaledWidth;
  SourceYs[0] := 0;
  SourceYs[1] := CornersVector[2];
  SourceYs[2] := UnscaledHeight-CornersVector[0];
  SourceYs[3] := UnscaledHeight;
  DestXs[0] := 0;
  DestXs[1] := CornersVector[3];
  DestXs[2] := w-CornersVector[1];
  DestXs[3] := w;
  DestYs[0] := 0;
  DestYs[1] := CornersVector[2];
  DestYs[2] := h-CornersVector[0];
  DestYs[3] := h;

  for ix := 0 to 2 do
   for iy := 0 to 2 do begin
     ScaledImageParts[ix,iy] := TRGBAlphaImage.create;
     ScaledImageParts[ix,iy].SetSize(SourceXs[ix+1]-SourceXs[ix],SourceYs[iy+1]-SourceYs[iy]);
     ScaledImageParts[ix,iy].Clear(Vector4Byte(0,0,0,255));
     ScaledImageParts[ix,iy].DrawFrom(FrameImage,0,0,SourceXs[ix],SourceYs[iy],SourceXs[ix+1]-SourceXs[ix],SourceYs[iy+1]-SourceYs[iy],dmBlend);
     ScaledImageParts[ix,iy].Resize(DestXs[ix+1]-DestXs[ix],DestYs[iy+1]-DestYs[iy],riNearest);
   end;

  FrameImage.SetSize(w,h,1);
  FrameImage.Clear(Vector4byte(0,0,0,255));
  for ix := 0 to 2 do
    for iy := 0 to 2 do FrameImage.DrawFrom(ScaledImageParts[ix,iy],DestXs[ix],DestYs[iy],0,0,DestXs[ix+1]-DestXs[ix],DestYs[iy+1]-DestYs[iy],dmBlend);

  for ix := 0 to 2 do
    for iy := 0 to 2 do freeAndNil(ScaledImageParts[ix,iy]);

  //and burn the burner
  FrameImage.DrawFrom(BURNER_IMAGE,0,0,x,y,w,h,dmMultiply);
end;

{------------------------------------------------------------------------}

procedure DAbstractInterfaceElement.InitGl;
begin
  if frame<> nil then begin
    FrameResize3x3;
    //FrameImage.Resize3x3(w,h,CornersVector,riNearest);
    freeandnil(FrameGL);
    FrameGL := TGLImage.create(FrameImage,true,true);
    //FrameImage := nil;
  end;
  if Content<>nil then content.InitGl;
end;

procedure DAbstractInterfaceElement.DestroyMe;
begin
 freeandnil(frameGL);
 //Freeandnil(FrameImage);
end;

constructor DAbstractInterfaceElement.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
  Opacity := 1;
  FrameOpacity := 0.8;
  color := vector4Single(1,1,1,1);
end;

{------------------------------------------------------------------------}


procedure DInterfaceElement.AnimateTo(NewAnimationState : TAnimationState;
                    NewAnimationLength : Integer);
begin
  LastAnimationState := CurrenAnimationState;
  NextAnimationState := NewAnimationState;
  AnimationStartTime := now;
  AnimationLength    := (NewAnimationLength+random*200-100) /24/60/60/1000;
  doAnimation        := true;
end;

Function DInterfaceElement.GetAnimationState: TAnimationState;
begin
  result.h := h;
  result.w := w;
  result.x := x;
  result.y := y;
  result.Opacity := Opacity;
end;

procedure DInterfaceElement.AnimateMe;
var t : single;
    i : integer;
begin
 if DoAnimation then begin
  if now - AnimationStartTime <= AnimationLength then begin
    t := sin(now - AnimationStartTime / AnimationLength);
    currentAnimationState.x := round(LastAnimationState.x+(NextAnimationState.x-LastAnimationState.x)*t);
    currentAnimationState.y := round(LastAnimationState.y+(NextAnimationState.y-LastAnimationState.y)*t);
    currentAnimationState.h := round(LastAnimationState.h+(NextAnimationState.h-LastAnimationState.h)*t);
    currentAnimationState.w := round(LastAnimationState.w+(NextAnimationState.w-LastAnimationState.w)*t);
    currentAnimationState.Opacity := LastAnimationState.Opacity+(NextAnimationState.Opacity-LastAnimationState.Opacity)*t;
  end else DoAnimation:=false;
  { if children's animations aren't finished then they'll ask parent
    to continue animation }
  for i := 0 to children.count-1 do begin
    //todo
    children[i].AnimateMe;
  end;
 end;
end;

procedure DInterfaceElement.AskParentForAnimation;
begin
  DoAnimation := true;
  if parent <> nil then
    parent.AskParentForAnimation
  else
    writeLog('DAbstractInterfaceElement.AskParentForAnimation','ERROR: Parent is nil!');
end;

{------------------------------------------------------------------------}

end.

