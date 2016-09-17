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
end;

Type DFrame=class(TComponent)
 public
   Image:TGLImage;
   cornerTop,cornerbottom,cornerLeft,cornerRight:integer;
   constructor Create(AOwner:TComponent); override;
 //image
end;

Type DAbstractInterfaceElement=class(DAbstractElement)
 public
  content:DAbstractElement;
  frame:DFrame;
  Parent:TComponent;
  procedure DrawFrame;
end;

type DInterfaceChildrenList=specialize TFPGObjectList<DAbstractInterfaceElement>;

Type DInterfaceElement=class(DAbstractInterfaceElement)
 public
  children:DInterfaceChildrenList;
  procedure ResizeChildren;
end;

Type DInterfaceContainer=class(DInterfaceElement)
end;

var frames:array[0..1] of DFrame;
    GUI:DInterfaceContainer;

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

procedure DrawInterface;
begin
  //GUI draw children recoursive

end;

{---------------------------------------------------------------------------}

procedure InitInterface;
begin
  WriteLnLog('InitInterface','started');
  Init_burner_image;
  GUI:=DInterfaceContainer.create(Window);
  frames[0]:=DFrame.create(Window);
  with frames[0] do begin
    image:=TGLImage.create(ApplicationData(Frames_Folder+'frame.png'));
    cornerTop:=1;CornerBottom:=1;cornerLeft:=1;CornerRight:=1;
  end;
  frames[1]:=DFrame.create(Window);
  with frames[1] do begin
    image:=TGLImage.create(ApplicationData(Frames_Folder+'frame_caption.png'));
    cornerTop:=18;CornerBottom:=1;cornerLeft:=1;CornerRight:=1;
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
  //GUI resize children recoursive
  WriteLnLog('ResizeInterface','finished');
end;

{============================================================================}

constructor Dframe.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
end;

{============================================================================}

procedure DAbstractInterfaceElement.DrawFrame;
begin
  frame.image.Draw3x3(x,y,w,h,frame.cornerTop,frame.CornerRight,frame.CornerBottom,Frame.CornerLeft); //frame.x...
end;

{============================================================================}

procedure DInterfaceElement.ResizeChildren;
var i:integer;
begin
 for i:=0 to children.count-1 do {*******};
end;

end.

