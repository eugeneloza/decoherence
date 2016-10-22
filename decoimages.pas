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

uses Classes,
  castleVectors, CastleGLImages, CastleImages,
  decointerface,
  decoglobal;

type
  DAbstractImage = class(DAbstractElement)
  { General routines shared by images and labels }
  public
    { Thread-safe part of rescaling the image }
    procedure RescaleImage;
    procedure rescale; override;
    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
    procedure draw; override;
    procedure Load(const filename:string);
  private
    SourceImage: TCastleImage;  //todo scale Source Image for max screen resolution ? //todo never store on Android.
    ScaledImage: TCastleImage;
    { keeps from accidentally re-initializing GL }
    InitGLPending: boolean;
    ImageReady: boolean;
    GLImage: TGLImage;
    { initialize GL image. NOT THREAD SAFE! }
    procedure InitGL;
  end;

type
  { most simple image type }
  DStaticImage = class(DAbstractImage)
  public
{    Opacity: float;
    Function GetAnimationState: Txywha; override;}
  end;

type
  { abstract "phased image" that moves and morphs with "phase" }
  DPhasedImage = class(DAbstractImage)
  public
    color: TVector4Single; //todo
    phasespeed: float;   {1/seconds to scroll the full screen}
    Opacity: float;
    constructor create(AOwner: TComponent); override;
  private
    phase, opacityphase: float;
    lasttime: TDateTime;
  end;

type
  { Wind and smoke effects used in different situations }
  //todo might be descendant of DStaticImage
  DWindImage = class (DPhasedImage)
  public
    { completely overrides the default drawing procedure }
    procedure draw; override;
  private
    procedure cyclephase;
  end;

type
  { A floating image for LoadScreens }
  DFloatImage = class(DPhasedImage)
  public
    procedure Draw; override;
  private
    procedure CyclePhase;
  end;


{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog, CastleFilesUtils,
     decogui;

{-----------------------------------------------------------------------------}

procedure DAbstractImage.Load(const filename: string);
begin
  WritelnLog('DAbstractImage.LoadImage',filename);
  SourceImage := LoadImage(ApplicationData(filename));
{  w := -1;//SourceImage.width;
  h := -1;//SourceImage.Height;
  //don't load TGLImage until scaleMe!!!}
//  ImageLoaded := true;
end;

{----------------------------------------------------------------------------}

procedure DAbstractImage.InitGL;
begin
  if InitGLPending then begin
    InitGLPending:=false;
    if ScaledImage<>nil then begin
      FreeAndNil(GLImage);
      GLImage := TGLImage.create(ScaledImage,true,true);
      ImageReady := true;
    end else WriteLnLog('DAbstractElement.InitGL','ERROR: Scaled Image is nil!');
  end;
end;

constructor DAbstractImage.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  InitGLPending := false;
  imageReady := false;
end;

destructor DAbstractImage.destroy;
begin
  FreeAndNil(GLImage);
  //scaledImage is automatically freed by GlImage
  {freeandnil(ScaledImage);}
  FreeAndNil(SourceImage);
  inherited;
end;

procedure DAbstractImage.rescale;
begin
  inherited;
  RescaleImage;
  //todo!!!!!!!!!!!!!!! Not thread safe
  InitGL;
end;

procedure DAbstractImage.RescaleImage;
begin
 if base.initialized then
  if (scaledImage = nil) or (ScaledImage.Width <> base.w) or (ScaledImage.height <> base.h) then begin
    scaledImage := SourceImage.CreateCopy as TCastleImage;
    scaledImage.Resize(base.w,base.h,InterfaceScalingMethod);
    InitGLPending := true;
  end
 else
   writeLnLog('DAbstractImage.RescaleImage','ERROR: base.initialized = false');
end;

procedure DAbstractImage.draw;
var currentAnimationState:Txywha;
begin
  if ImageReady then begin
    //animate
    currentAnimationState:=GetAnimationState;
    GLImage.color:=vector4single(1,1,1,currentAnimationState.Opacity); //todo
    GLIMage.Draw(currentAnimationState.x1,currentAnimationState.y1,currentAnimationState.w,currentAnimationState.h); //todo
  end;
end;

{=============================================================================}
{========================= static image ========================================}
{=============================================================================}

{Function DStaticImage.GetAnimationState: Txywha;
begin
  result := Txywha.create(nil);
  result.x1 := base.x1;
  result.x2 := base.x2;
  result.opacity := Opacity;
end;     }


{=============================================================================}
{======================== phased image =======================================}
{=============================================================================}

constructor DPhasedImage.create(AOwner: TComponent);
begin
  inherited;
  lasttime := -1;
  color:=vector4Single(1,1,1,1);
end;


{=============================================================================}
{========================= wind image ========================================}
{=============================================================================}

procedure DWindImage.CyclePhase;
var phaseshift: float;
begin
  if lasttime=-1 then lasttime:=now;
  phaseshift:=(now-lasttime)*24*60*60*phaseSpeed;
  if phaseshift<0.5 then begin
    phase -= phaseshift*(1+0.1*GUI.rnd.Random);
    if phase<0 then phase += 1;
    opacityphase -= phaseshift/2*(1+0.2*GUI.rnd.Random);
    if opacityphase<0 then opacityphase += 1;
  end else begin
    //if pause was too long reinitialize with random phases.
    phase := GUI.rnd.Random;
    opacityphase := GUI.rnd.Random;
  end;
  lasttime:=now;
end;


procedure DWindImage.Draw;
var phase_scaled:integer;
begin
  if ImageReady then begin
    CyclePhase;
    color[3] := Opacity + Opacity/4 * sin(2*Pi*opacityphase);
    GLImage.Color := color;
    phase_scaled := round(Phase*Window.width);

    //draw first part of the image
    GLImage.Draw(phase_scaled,0,
                 Window.width-phase_scaled,Window.height,
                 0,0,
                 Window.width-phase_scaled,Window.height);
    //draw second part of the image
    GLImage.Draw(0,0,
                 phase_scaled,Window.height,
                 Window.width-phase_scaled,0,
                 phase_scaled,Window.height);
  end else WriteLnLog('DWindImage.Draw','ERROR: Wind image not ready to draw!');
end;

{=============================================================================}
{========================= float image =======================================}
{=============================================================================}

procedure DFloatImage.CyclePhase;
var phaseshift: float;
begin
  if lasttime=-1 then lasttime:=now;
  phaseshift:=(now-lasttime)*24*60*60*phaseSpeed;
  phase -= phaseshift*(1+0.1*GUI.rnd.Random);
  if phase>1 then begin
    phase:=1;
    //reloadimage;
  end;
  lasttime:=now;
end;

procedure DFloatImage.draw;
var x:integer;
begin
  if ImageReady then begin
    cyclePhase;
    color[3] := Opacity*sin(phase);
    GLImage.color := Color;
    x:=round((window.width-base.w)*phase);
    GLImage.Draw(x,0);
  end else WritelnLog('DStaticImage.DrawMe','ERROR: Static Image not ready to draw!');
end;

end.

