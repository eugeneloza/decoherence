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

Type DAbstractElement=class(TComponent)
 public
  x,y,h,w:integer;
  procedure drawMe; virtual; abstract;
  procedure InitGL; virtual; abstract;
  procedure DestroyMe; virtual; abstract;
end;

Type DFrame=class(TComponent)
 public
   Image:TRGBAlphaImage;
   cornerTop,cornerbottom,cornerLeft,cornerRight:integer;
   constructor Create(AOwner:TComponent); override;
 //image
end;

Type DAbstractInterfaceElement=class(DAbstractElement)
 public
  content:DAbstractElement;
  frame:DFrame;
  Parent:TComponent;
  procedure InitGL; override;
  procedure DestroyMe; override;
 private
   FrameImage:TRGBAlphaImage;
   FrameGL:TGLImage;
   procedure FrameResize3x3;
   procedure ResizeChildren; virtual; Abstract;
end;

type DInterfaceChildrenList=specialize TFPGObjectList<DAbstractInterfaceElement>;

Type DInterfaceElement=class(DAbstractInterfaceElement)
 public
  children:DInterfaceChildrenList;
  constructor Create(AOwner:TComponent); override;
  procedure DrawMe; override;
  procedure DestroyMe; override;
 private
  procedure ResizeChildren; override;
end;

Type DInterfaceContainer=class(DInterfaceElement)
end;

var frames:array[0..1] of DFrame;
    GUI:DInterfaceContainer;

procedure InitInterface;
procedure MakeInterface1;
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
  if BURNER_IMAGE_UNSCALED=nil then BURNER_IMAGE_UNSCALED:=LoadImage(ApplicationData(Interface_Foler+'burner_Pattern_203_CC0_by_Nobiax_diffuse.png'), [TRGBImage]) as TRGBImage;
  if BURNER_IMAGE=nil then begin
    BURNER_IMAGE:=BURNER_IMAGE_UNSCALED.MakeCopy;
    BURNER_IMAGE.Resize(window.width,window.height,riBilinear);
  end;
  if (BURNER_IMAGE.height<>window.height) or (BURNER_IMAGE.width<>window.width) then begin
    FreeAndNil(BURNER_IMAGE);
    BURNER_IMAGE:=BURNER_IMAGE_UNSCALED.MakeCopy;
    BURNER_IMAGE.Resize(window.width,window.height,riBilinear);
  end;
  WriteLnLog('Init_burner_image','finished');
end;

{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}

procedure MakeInterface1;
var newElement:DInterfaceElement;
begin
  NewElement:=DInterfaceElement.create(GUI);
  NewElement.frame:=frames[1];
  NewElement.x:=100;
  NewElement.y:=100;
  NewElement.w:=100;
  NewElement.h:=100;
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
  GUI:=DInterfaceContainer.create(Window);
  frames[0]:=DFrame.create(Window);
  with frames[0] do begin
    image:=LoadImage(ApplicationData(Frames_Folder+'frame.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop:=1;CornerBottom:=1;cornerLeft:=1;CornerRight:=1;
  end;
  frames[1]:=DFrame.create(Window);
  with frames[1] do begin
    image:=LoadImage(ApplicationData(Frames_Folder+'frame_caption.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop:=19;CornerBottom:=1;cornerLeft:=1;CornerRight:=1;
  end;

//  GUI.frame:=Dframe.create(frames[0],GUI);
  ResizeInterface;
  WriteLnLog('InitInterface','finished');
end;

{---------------------------------------------------------------------------}

procedure ResizeInterface;
begin
  WriteLnLog('ResizeInterface','started');
  {GUI always takes the whole screen and every child is scaled against it}
  GUI.x:=0;
  GUI.y:=0;
  GUI.w:=window.width;
  GUI.h:=window.height;
  GUI.frame:=nil;
  GUI.content:=nil;
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
  if frameGL<>nil then
    frameGL.Draw3x3(x,y,w,h,frame.cornerTop,frame.CornerRight,frame.CornerBottom,Frame.CornerLeft);
  if content<>nil then
    content.drawMe;
  //draw children recoursive
  for i:=0 to Children.count-1 do Children[i].DrawMe;
end;

{============================================================================}

procedure DInterfaceElement.ResizeChildren;
var i:integer;
begin
 for i:=0 to children.count-1 do begin
   //todo
   children[i].resizeChildren;
 end;
end;

constructor DInterfaceElement.Create(AOwner:TComponent);
begin
  inherited;
  children:=DInterfaceChildrenList.create(true);
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
  FrameImage:=frame.Image.CreateCopy as TRGBAlphaImage;
  CornersVector:=Vector4Integer(frame.cornerTop,frame.cornerLeft,frame.cornerBottom,frame.cornerRight);

  UnscaledWidth:=FrameImage.width;
  UnscaledHeight:=FrameImage.height;

  SourceXs[0]:=0;
  SourceXs[1]:=CornersVector[3];
  SourceXs[2]:=UnscaledWidth-CornersVector[1];
  SourceXs[3]:=UnscaledWidth;
  SourceYs[0]:=0;
  SourceYs[1]:=CornersVector[2];
  SourceYs[2]:=UnscaledHeight-CornersVector[0];
  SourceYs[3]:=UnscaledHeight;
  DestXs[0]:=0;
  DestXs[1]:=CornersVector[3];
  DestXs[2]:=w-CornersVector[1];
  DestXs[3]:=w;
  DestYs[0]:=0;
  DestYs[1]:=CornersVector[2];
  DestYs[2]:=h-CornersVector[0];
  DestYs[3]:=h;

  for ix:=0 to 2 do
   for iy:=0 to 2 do begin
     ScaledImageParts[ix,iy]:=TRGBAlphaImage.create;
     ScaledImageParts[ix,iy].SetSize(SourceXs[ix+1]-SourceXs[ix],SourceYs[iy+1]-SourceYs[iy]);
     ScaledImageParts[ix,iy].Clear(Vector4Byte(0,0,0,255));
     ScaledImageParts[ix,iy].DrawFrom(FrameImage,0,0,SourceXs[ix],SourceYs[iy],SourceXs[ix+1]-SourceXs[ix],SourceYs[iy+1]-SourceYs[iy],dmBlend);
     ScaledImageParts[ix,iy].Resize(DestXs[ix+1]-DestXs[ix],DestYs[iy+1]-DestYs[iy],riNearest);
   end;

  FrameImage.SetSize(w,h,1);
  FrameImage.Clear(Vector4byte(0,0,0,255));
  for ix:=0 to 2 do
    for iy:=0 to 2 do FrameImage.DrawFrom(ScaledImageParts[ix,iy],DestXs[ix],DestYs[iy],0,0,DestXs[ix+1]-DestXs[ix],DestYs[iy+1]-DestYs[iy],dmBlend);

  for ix:=0 to 2 do
    for iy:=0 to 2 do freeAndNil(ScaledImageParts[ix,iy]);

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
    FrameGL:=TGLImage.create(FrameImage,true,true);
    FrameImage:=nil;
  end;
  if Content<>nil then content.InitGl;
end;

procedure DAbstractInterfaceElement.DestroyMe;
begin
 freeandnil(frameGL);
 Freeandnil(FrameImage);
end;

end.

