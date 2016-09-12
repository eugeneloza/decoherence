unit decointerface;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, sysutils,
  CastleLog, {CastleWindow,} castleFilesUtils,
  castleVectors, CastleGLImages, CastleImages,
  {decoimages, decoLabel,}
  decoglobal{, DecoFont};

const Frames_Folder='interface/frames/';

Type DAbstractElement=class(TComponent)
 public
  x,y,h,w:integer;
  procedure drawMe; virtual; abstract;
end;

Type DFrame=class(DAbstractElement)
 public
   Image:TGLImage;
   constructor Create(img:TGLImage;AOwner:TComponent); virtual;
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

var frames:array[0..0] of TGlImage;
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

end;

{---------------------------------------------------------------------------}

procedure InitInterface;
begin
  WriteLnLog('InitInterface','started');
  Init_burner_image;
  GUI:=DInterfaceContainer.create(Window);
  frames[0]:=TGLImage.create(ApplicationData(Frames_Folder+'frame.png'));
//  GUI.frame:=Dframe.create(frames[0],GUI);
  ResizeInterface;
  WriteLnLog('InitInterface','finished');
end;

{---------------------------------------------------------------------------}

procedure ResizeInterface;
begin
  WriteLnLog('ResizeInterface','started');
  GUI.x:=0;
  GUI.y:=0;
  GUI.w:=window.width;
  GUI.h:=window.height;
  WriteLnLog('ResizeInterface','finished');
end;

{============================================================================}

constructor Dframe.Create(img:TGLImage;AOwner:TComponent);
begin
  inherited create(AOwner);
  //will need to fix this for animated frames
  Image:=img;
end;

{============================================================================}

procedure DAbstractInterfaceElement.DrawFrame;
begin
  frame.image.Draw3x3(x,y,w,h,4,4,4,4); //frame.x...
end;

{============================================================================}

procedure DInterfaceElement.ResizeChildren;
var i:integer;
begin
 for i:=0 to children.count-1 do {*******};
end;

end.

