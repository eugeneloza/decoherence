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
unit decoimages;

{$mode objfpc}{$H+}

interface

uses classes, SysUtils, {fgl,}
  CastleLog, castleFilesUtils,
  {CastleControls,} CastleImages, castleVectors,
  CastleGLImages,
  decointerface,
  decoglobal;

type DAbstractImage=class(DAbstractElement)
 public
  constructor Create(AOwner:TComponent); override;
  procedure LoadMe(filename:string); virtual; abstract;
  procedure ScaleMe(const new_w:integer=0;const new_h:integer=0); virtual; abstract;
 private
  ImageReady:boolean;
  ImageLoaded:boolean;
  doRescale:boolean;
end;

type DStaticImage=class(DAbstractImage)
 public
  Image:TGLImage;
  SourceImage:TCastleImage;
  {used temporarily to scale the image //thread-safe}
  TmpImage:TCastleImage;
  procedure LoadMe(filename:string); override;
  {destructor} procedure DestroyMe; override;
  procedure DrawMe; override;
  procedure ScaleMe(const new_w:integer=0;const new_h:integer=0); override;
  {Initialize GL Image // thread-unsafe!!!}
  procedure InitGL; override;
end;

type DWindImage=class(DStaticImage)
  public
  phase:single;
  alpha:single;
  procedure DrawMe; override;
end;

implementation

{----------------------------------------------------------------}

Constructor DAbstractImage.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
  color:=vector4Single(1,1,1,1);
  ImageReady:=false;
  ImageLoaded:=false;
end;

{----------------------------------------------------------------}

{Destructor}Procedure DStaticImage.DestroyMe;
begin
 freeandnil(SourceImage);
 freeandnil(Image);
 if tmpimage<>nil then WritelnLog('DStaticImage.DestroyMe','ERROR: tmpimage is not nil!');
 //freeandnil(TmpImage);
// inherited
end;

procedure DStaticImage.LoadMe(filename:string);
begin
  WritelnLog('DSimpleImage.LoadMe',filename);
  SourceImage:=LoadImage(ApplicationData(filename));
  w:=-1;//SourceImage.width;
  h:=-1;//SourceImage.Height;
  //don't load TGLImage until scaleMe!!!
  ImageLoaded:=true;
end;

{----------------------------------------------------------------}

procedure DStaticImage.ScaleMe(const new_w:integer=0;const new_h:integer=0);
var hh,ww:integer;
begin
 if ImageReady then writeLnLog('DStaticImage.ScaleMe','ERROR: DoubleLoading image!!!');
 if not doRescale then
 if ImageLoaded then begin
   doRescale:=true;
   ImageReady:=false;
   if (new_h>0) and (new_w>0) then begin
     hh:=new_h;
     ww:=new_w;
   end;
   if new_w=-1 then begin
     if new_h=-1 then begin
       hh:=window.height;
       ww:=window.width;
     end else begin
       hh:=window.Height;
       ww:=round(hh/SourceImage.Height*sourceImage.width);
     end;
   end;
   if (h<>hh) or (w<>ww) or (Image=nil) then begin
     h:=hh;
     w:=ww;
     WritelnLog('DStaticImage.ScaleMe',inttostr(w)+'x'+inttostr(h));
     TmpImage:=SourceImage.CreateCopy as TCastleImage;
     if (h>0) and (w>0) then
       TmpImage.Resize(w,h,riBilinear);
   end else begin
     ImageReady:=true;
     doRescale:=false;
     WritelnLog('DStaticImage.ScaleMe','No need to rescale, skipping...');
   end;
 end else WritelnLog('DStaticImage.ScaleMe','ERROR: Image not loaded!');
end;

{----------------------------------------------------------------}

procedure DStaticImage.InitGl;
begin
 if doRescale then begin
   if (TmpImage<>nil) then begin
     WritelnLog('DStaticImage.InitGl','GL initialize');
     freeandnil(Image);
     Image:=TGLImage.create(TmpImage,true,true);
     //tmpImage:=nil;        //Looks like the issue has been fixed. Still I'll have to keep an eye at this line
     ImageReady:=true;
     doRescale:=false;
   end else WritelnLog('DStaticImage.InitGl','ERROR: TmpImage is nil!');
 end else WritelnLog('DStaticImage.InitGl','Image not changed, skipping...');
end;

{----------------------------------------------------------------}

procedure DStaticImage.DrawMe;
begin
  if ImageReady then begin
    color[3]:=Opacity;
    Image.color:=Color;
    Image.Draw(x,y,w,h)
  end else WritelnLog('DStaticImage.DrawMe','ERROR: Static Image not ready to draw!');
end;

{----------------------------------------------------------------}

procedure DWindImage.DrawMe;
var phase_scaled:integer;
begin
  if ImageReady then begin
    color[3]:=alpha+alpha/4*sin(2*Pi*3*phase);
    image.Color:=color;
    phase_scaled:=round(Phase*w);

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
  end else WriteLnLog('DWindImage.DrawMe','ERROR: Wind image not ready to draw!');
end;


end.

