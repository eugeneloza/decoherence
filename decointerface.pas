unit decointerface;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, sysutils,
  CastleLog, {CastleWindow,} castleFilesUtils,
  castleVectors, CastleGLImages,
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
   constructor Create(URL:String;AOwner:TComponent);
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
  procedure ResizeChildren;
  children:DInterfaceChildrenList;
end;

Type DInterfaceContainer=class(DInterfaceElement)
end;

var frames:array[0..0] of DFrame;
    GUI:DInterfaceContainer;

procedure InitInterface;
procedure ResizeInterface;
procedure DrawInterface;

implementation

{---------------------------------------------------------------------------}

procedure DrawInterface;
begin

end;

{---------------------------------------------------------------------------}

procedure InitInterface;
begin
  GUI:=DInterfaceContainer.create(Window);
  frames[0]:=DFrame.create('frame.png',GUI);
  GUI.frame:=frames[0];
  ResizeInterface;
end;

{---------------------------------------------------------------------------}

procedure ResizeInterface;
begin
  GUI.x:=0;
  GUI.y:=0;
  GUI.w:=window.width;
  GUI.h:=window.height;
end;

{============================================================================}

constructor Dframe.Create(URL:string;AOwner:TComponent);
begin
  inherited create(AOwner);
  Image:=TGLImage.Create(ApplicationData(Frames_Folder+URL));
end;

{============================================================================}

procedure DAbstractInterfaceElement.DrawFrame;
begin
  frame.image.Draw3x3(x,y,w,h,4,4,4,4);
end;

{============================================================================}

procedure DInterfaceElement.ResizeChildren;
var i:integer;
begin
 for i:=low(children) to high(children) do {*******};
end;

end.

