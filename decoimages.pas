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
{$INCLUDE compilerconfig.inc}

interface

uses Classes,
  castleVectors, CastleGLImages, CastleImages,
  decointerface, decoactor,
  decoglobal;

type
  DAbstractImage = class(DAbstractElement)
  { General routines shared by images and labels }
  public
    color: TVector4Single; //todo
    { very simple draw procedure }
    procedure draw; override;
    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
    { initialize GL image. NOT THREAD SAFE! }
    procedure InitGL;{ override;}
    { frees an image without freeing the whole instance }
    procedure FreeImage;
  private
    GLImage: TGLImage;
  public
    ImageReady, ImageLoaded: boolean;
    { keeps from accidentally re-initializing GL }
    InitGLPending: boolean;
    SourceImage: TCastleImage;  //todo scale Source Image for max screen resolution ? //todo never store on Android.
    ScaledImage: TCastleImage;
  end;

type
  { most simple image type }
  DStaticImage = class(DAbstractImage)
  public

    { if thread is running }
    ThreadWorking: boolean;
    { loads image in realtime }
    procedure Load(const filename:string); virtual;
    { loads image in a thread }
    procedure LoadThread(const filename:string);
    { Thread-safe part of rescaling the image }
    procedure rescale; override;
  private
    procedure RescaleImage;
  end;

type
  { abstract "phased image" that moves and morphs with "phase" }
  DPhasedImage = class(DStaticImage)
  public
    phasespeed: float;   {1/seconds to scroll the full screen}
    Opacity: float;
    constructor create(AOwner: TComponent); override;
    procedure Load(const filename:string); override;
  private
    lasttime: TDateTime;
  public
    phase, opacityphase: float;
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

Type TBarStyle = (bsVertical,bsHorizontal);

Type
  { Generic bar used for progress bars and health bars }
  DBarImage = class(DStaticImage)
  public
    { minimmum, maximum, current maximum and current position
      minimum usually is zero and automatically set in constructor }
    min, max, currentMax, position: single;
    { vertical or horizontal style of the bar }
    kind: TBarStyle;
    procedure draw; override;
    constructor create(AOwner: TComponent); override;
  end;

Type TStatBarStyle = (sbHealth, sbStamina, sbConcentration, sbMetaphysics);

Type
  { bar to display health for mobs and primary 4 stats for player characters }
  DStatBarImage = class(DBarImage)
  public
    { Points to the actor for who the health is displayed}
    Target: DActor;
    { determines which value to display: Health, Stamina, Concentration or Metaphysics}
    Style: TStatBarStyle;
    procedure update; override;
    procedure draw; override;
  end;


var LoadNewFloaterImage: boolean;


{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog, CastleFilesUtils,
     decogui;

type TLoadImageThread = class(TThread)
  private
  protected
    Target: DStaticImage;
    Source: String;
    procedure Execute; override;
end;

{----------------------------------------------------------------------------}

procedure TLoadImageThread.execute;
begin
  target.ThreadWorking:=true;
  WritelnLog('TLoadImageThread.execute','Image thread started.');
  Target.Load(Source);
  if Target=nil then begin
    WritelnLog('TLoadImageThread.execute','Image was destroyed before it was loaded!');
    exit; //fix bug if the image didn't load completely but was destroyed
  end;
  Target.rescale;
  WritelnLog('TLoadImageThread.execute','Image thread finished.');
  target.ThreadWorking:=false;
end;

{-----------------------------------------------------------------------------}

procedure DStaticImage.LoadThread(const filename: string);
var LoadImageThread: TLoadImageThread;
begin
 if not ThreadWorking then begin
   LoadImageThread := TLoadImageThread.Create(true);
   LoadImageThread.Target := self;
   LoadImageThread.Source := filename;
   LoadImageThread.FreeOnTerminate := true;
   LoadImageThread.Priority := tpLower;
   LoadImageThread.Start;
 end;
end;

{----------------------------------------------------------------------------}

procedure DStaticImage.Load(const filename: string);
begin
  WritelnLog('DAbstractImage.LoadImage',filename);
  SourceImage := LoadImage(ApplicationData(filename));
  RealWidth := SourceImage.Width;
  RealHeight := SourceImage.Height;
  ImageLoaded := true;
end;

{----------------------------------------------------------------------------}

procedure DAbstractImage.FreeImage;
begin
  freeandnil(GLImage);
  freeandnil(SourceImage);
  ImageReady := false;
  ImageLoaded := false;
  InitGLPending:=false;
end;

{----------------------------------------------------------------------------}

procedure DAbstractImage.InitGL;
begin
  if InitGLPending then begin
    InitGLPending:=false;
    if ScaledImage<>nil then begin
      FreeAndNil(GLImage);
      GLImage := TGLImage.create(ScaledImage,true,true);
      ScaledImage := nil;
      ImageReady := true;
    end else WriteLnLog('DAbstractElement.InitGL','ERROR: Scaled Image is nil!');
  end;
end;

{----------------------------------------------------------------------------}

constructor DAbstractImage.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  color:=vector4Single(1,1,1,1);
  InitGLPending := false;
  imageReady := false;
  imageLoaded := false;
end;

{----------------------------------------------------------------------------}

destructor DAbstractImage.destroy;
begin
  FreeAndNil(GLImage);
  //scaledImage is automatically freed by GlImage
  {freeandnil(ScaledImage);}
  FreeAndNil(SourceImage);
  inherited;
end;


{----------------------------------------------------------------------------}

procedure DAbstractImage.draw;
begin
  if ImageReady then begin
    //animate
    update;
    GLImage.color := vector4single(1,1,1,currentAnimationState.Opacity); //todo
    GLIMage.Draw(currentAnimationState.x1,currentAnimationState.y1,currentAnimationState.w,currentAnimationState.h); //todo
  end else begin
    if InitGLPending then InitGL;
  end;
end;

{=============================================================================}
{========================= static image ========================================}
{=============================================================================}

{----------------------------------------------------------------------------}

procedure DStaticImage.rescale;
begin
  inherited;
  base.fixProportions(RealWidth,Realheight);
{  last.fixProportions(sourceImage.Width,sourceImage.height);
  next.fixProportions(sourceImage.Width,sourceImage.height);}
  RescaleImage;
end;

{----------------------------------------------------------------------------}

procedure DStaticImage.RescaleImage;
begin
 {$IFNDEF AllowRescale}If sourceImage=nil then exit;{$ENDIF}
 if ImageLoaded then begin
   if base.initialized then
    if (scaledImage = nil){ or (ScaledImage.Width <> base.w) or (ScaledImage.height <> base.h)} then begin
      ImageReady:=false;
      FreeAndNil(GLImage);
      scaledImage := SourceImage.CreateCopy as TCastleImage;
      {$IFNDEF AllowRescale}freeandnil(sourceImage);{$ENDIF}
      scaledImage.Resize(base.w,base.h,InterfaceScalingMethod);
      InitGLPending := true;
    end
   else
     writeLnLog('DStaticImage.RescaleImage','ERROR: base.initialized = false');
 end;
end;

{=============================================================================}
{======================== phased image =======================================}
{=============================================================================}

constructor DPhasedImage.create(AOwner: TComponent);
begin
  inherited;
end;

{----------------------------------------------------------------------------}

Procedure DPhasedImage.Load(const filename:string);
begin
  inherited Load(filename);
  lasttime := -1;
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

{----------------------------------------------------------------------------}

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
  end else begin
    if InitGLPending then InitGL;
//    WriteLnLog('DWindImage.Draw','ERROR: Wind image not ready to draw!');
  end;
end;

{=============================================================================}
{========================= float image =======================================}
{=============================================================================}

procedure DFloatImage.CyclePhase;
var phaseshift: float;
begin
  if lasttime = -1 then begin
    lasttime := now;
    phase := 0;
  end;
  phaseshift := (now-lasttime)*24*60*60*phaseSpeed;
  phase += phaseshift*(1+0.1*GUI.rnd.Random);
  if phase>1 then begin
    phase:=1;
    LoadNewFloaterImage:=true;
    ImageLoaded:=false;
  end;
  lasttime:=now;
end;

{----------------------------------------------------------------------------}

procedure DFloatImage.draw;
var x:integer;
begin
  if ImageReady then begin
    cyclePhase;
    color[3] := Opacity*sin(Pi*phase);
    GLImage.color := Color;
    x:=round((window.width-base.w)*phase);
    GLImage.Draw(x,0);
  end else begin
    if InitGLPending then InitGL;
  //   WritelnLog('DStaticImage.DrawMe','ERROR: Static Image not ready to draw!');
  end;
end;

{=============================================================================}
{=========================== bar image =======================================}
{=============================================================================}

procedure DBarImage.draw;
begin
  if imageReady then begin
    //...
  end else
    if InitGLPending then InitGL;
end;

constructor DBarImage.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  min := 0;
  max := 1;
  position := 0;
  Kind := bsHorizontal;
end;

procedure DStatBarImage.update;
begin
  inherited;
  min := 0;
  //maybe pointers will be better? Still, it doesn't look inefficient;
  case Style of
    sbHealth: begin
                max := Target.MaxMaxHp;
                CurrentMax := Target.MaxHp;
                Position := Target.Hp;
              end;
    sbStamina: begin
                max := Target.MaxMaxSta;
                CurrentMax := Target.MaxSta;
                Position := Target.Sta;
              end;
    sbConcentration: begin
                max := Target.MaxMaxConc;
                CurrentMax := Target.MaxConc;
                Position := Target.Conc;
              end;
    sbMetaphysics: begin
                max := Target.MaxMaxMph;
                CurrentMax := Target.MaxMph;
                Position := Target.Mph;
              end;
  end;
end;

procedure DStatBarImage.draw;
begin
  update;
  inherited draw;
end;

end.

