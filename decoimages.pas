unit decoimages;

{$mode objfpc}{$H+}

interface

uses classes, SysUtils, {fgl,}
  CastleLog, castleFilesUtils,
  {CastleControls,} CastleImages, castleVectors,
  CastleGLImages,
  decoglobal;

type DAbstractImage=class(TPersistent)
 public
  w,h:integer;
  constructor Create;
  procedure LoadMe(filename:string); virtual; abstract;
  procedure drawMe; virtual; abstract;
  procedure ScaleMe(const new_w:integer=0;const new_h:integer=0;const doInit:boolean=false); virtual; abstract;
 private
  ImageReady:boolean;
  ImageLoaded:boolean;
end;

type DStaticImage=class(DAbstractImage)
 public
  x,y:integer;
  Image:TGLImage;
  SourceImage:TCastleImage;
  {used temporarily to scale the image //thread-safe}
  TmpImage:TCastleImage;
  procedure LoadMe(filename:string); override;
  procedure DrawMe; override;
  procedure ScaleMe(const new_w:integer=0;const new_h:integer=0;const doInit:boolean=false); override;
  {Initialize GL Image // thread-unsafe!!!}
  procedure InitGL;
end;

type DWindImage=class(DStaticImage)
  public
  phase:single;
  alpha:single;
  procedure DrawMe; override;
end;

implementation

{----------------------------------------------------------------}

Constructor DAbstractImage.Create;
begin
  inherited;
  ImageReady:=false;
  ImageLoaded:=false;
end;

{----------------------------------------------------------------}

procedure DStaticImage.LoadMe(filename:string);
begin
  WritelnLog('DSimpleImage.LoadMe',filename);
  freeandnil(SourceImage);
  freeandnil(Image);
  SourceImage:=LoadImage(ApplicationData(filename));
  w:=SourceImage.width;
  h:=SourceImage.Height;
  //don't load TGLImage until scaleMe!!!
  ImageLoaded:=true;
end;

{----------------------------------------------------------------}

procedure DStaticImage.ScaleMe(const new_w:integer=0;const new_h:integer=0;const doInit:boolean=false);
begin
 ImageReady:=false;
 if ImageLoaded then begin
   if (new_h>0) and (new_w>0) then begin
     h:=new_h;
     w:=new_w;
   end;
   if new_w=-1 then begin
     if new_h=-1 then begin
       h:=window.height;
       w:=window.width;
     end else begin
       h:=round(window.Height);
       w:=round(h/SourceImage.Height*sourceImage.width);
     end;
   end;
   WritelnLog('DStaticImage.ScaleMe',inttostr(w)+'x'+inttostr(h));
   TmpImage:=SourceImage.CreateCopy as TCastleImage;
   if (h>0) and (w>0) then
     TmpImage.Resize(w,h,riBilinear);
   if doInit then initGL;
 end else WritelnLog('DStaticImage.ScaleMe','ERROR: Image not loaded!');
end;

{----------------------------------------------------------------}


procedure DStaticImage.InitGl;
//var OldImage:TGLImage;
begin
 if TmpImage=nil then ScaleMe(0,0,false);
 //OldImage:=Image;
 // freeandnil(Image);         !!! MEMORY LEAKS HERE... but causes SIGSEGV if initGL is called twice...
 Image:=TGLImage.create(TmpImage,true,true);
 {if oldimage<>nil then begin
   OldImage.Destroy; //freeandnil(oldimage);
 end;}
 //if oldimage<>nil then OldImage.free;
 freeandnil(tmpImage);
 ImageReady:=true;
end;

{----------------------------------------------------------------}

procedure DStaticImage.DrawMe;
begin
  if ImageReady then
    Image.Draw(x,y,w,h)
  //else WritelnLog('DStaticImage.DrawMe','ERROR: Cannot Draw');
end;

{----------------------------------------------------------------}

procedure DWindImage.DrawMe;
var phase_scaled:integer;
begin
  if ImageReady then begin
    image.Color:=Vector4Single(1,1,1,alpha+alpha/4*sin(2*Pi*3*phase));
    phase_scaled:=round(Phase*w);

    //image.draw(0,0);
    //draw first part of the image
    Image.Draw(phase_scaled,0,
               w-phase_scaled,h,
               0,0,
               w-phase_scaled,h);
    //draw second part of the image
    Image.Draw(0,0,
               phase_scaled,h,
               w-phase_scaled,0,
               phase_scaled,h);
  end;
end;

{----------------------------------------------------------------}
{----------------------------------------------------------------}
{----------------------------------------------------------------}

var BURNER_IMAGE_UNSCALED,BURNER_IMAGE:TCastleImage;
procedure Init_burner_image;
begin
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
end;

end.

