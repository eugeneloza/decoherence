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

{ Works with different types of images }
unit decoimages;

{$INCLUDE compilerconfig.inc}
interface

uses Classes,
  castleVectors, CastleGLImages, CastleImages,
  decointerface, DecoActor,
  decoglobal;

type
  { General routines shared by images and labels }
  DAbstractImage = class(DAbstractElement)
  public
    color: TVector4; //todo
    { very simple draw procedure }
    procedure Draw; override;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    { initialize GL image. NOT THREAD SAFE! }
    procedure InitGL;{ override;}
    { frees an image without freeing the whole instance }
    procedure FreeImage;
//  private
  public
    GLImage: TGLImage;
    ImageReady, ImageLoaded: boolean;
    { keeps from accidentally re-initializing GL }
    InitGLPending: boolean;
    SourceImage: TCastleImage;  //todo scale Source Image for max screen resolution ?
    ScaledImage: TCastleImage;
    {due to a little bug I have to define these separately}
    ScaledWidth, ScaledHeight: integer;
    { Thread-safe part of rescaling the image }
    procedure Rescale; override;
    procedure RescaleImage; virtual;
  end;


type TLoadImageThread = class(TThread)
  public
    Target: DAbstractImage;
    filename: String;
  protected
    procedure Execute; override;
  end;

type
  { most simple image type }
  DStaticImage = class(DAbstractImage)
  private
    LoadImageThread: TLoadImageThread;
    { if thread is running }
    ThreadWorking: boolean;
  public
    { loads image in realtime }
    procedure Load(const filename:string); virtual;
    procedure Load(const CopyImage: TCastleImage);
    procedure afterload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    { loads image in a thread }
    procedure LoadThread(const filename:string);
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
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

Type TBarStyle = (bsVertical, bsHorizontal);

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
    constructor create(AOwner: TComponent); override;
  end;

var LoadNewFloaterImage: boolean;


{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog, CastleFilesUtils,
  DecoInputOutput, DecoTime;

procedure TLoadImageThread.execute;
var TargetImage: DSTaticImage;
begin
  TargetImage := Target as DStaticImage;

  WritelnLog('TLoadImageThread.execute','Image thread started.');

  TargetImage.Load(filename);
  TargetImage.rescale;

  WritelnLog('TLoadImageThread.execute','Image thread finished.');
  TargetImage.ThreadWorking := false;
end;

{-----------------------------------------------------------------------------}

constructor DStaticImage.create(AOwner: TComponent);
begin
  ThreadWorking := false;
  Inherited create(AOwner);
end;

{----------------------------------------------------------------------------}

destructor DStaticImage.destroy;
begin
  if ThreadWorking then begin
    Try
      LoadImageThread.Terminate;
    finally
      FreeAndNil(LoadImageThread); //redundant, as freeonterminate=true?
    end;
  end;
  inherited;
end;

{----------------------------------------------------------------------------}

procedure DStaticImage.LoadThread(const filename: string);
begin
 if not ThreadWorking then begin
   LoadImageThread := TLoadImageThread.Create(true);
   LoadImageThread.Target := self;
   LoadImageThread.filename := filename;
   LoadImageThread.FreeOnTerminate := true;
   LoadImageThread.Priority := tpLower;
   ThreadWorking := true;
   LoadImageThread.Start;
 end
 else
   writeLnLog('DStaticImage.LoadThread','Thread already working...');
end;

{----------------------------------------------------------------------------}

procedure DStaticImage.Load(const filename: string);
begin
  WritelnLog('DStaticImage.LoadImage',filename);
  SourceImage := LoadImageSafe(ApplicationData(filename));
  afterload;
end;
procedure DStaticImage.Load(const CopyImage: TCastleImage);
begin
  WritelnLog('DStaticImage.LoadImage','Copying image from '+CopyImage.ClassName);
  SourceImage := CopyImage.MakeCopy;
  afterload;
end;
procedure DStaticImage.afterload;
begin
  RealWidth := SourceImage.Width;
  RealHeight := SourceImage.Height;
  ScaledWidth := -1;
  ScaledHeight := -1;
  ImageLoaded := true;
end;

{----------------------------------------------------------------------------}

procedure DAbstractImage.FreeImage;
begin
  FreeAndNil(GLImage);
  FreeAndNil(SourceImage);

  //scaledImage is automatically freed by GlImage
  {$WARNING BUG: why Scaled image is not freed automatically?????}
  {BUG: I still need to free ScaledImage - while it's owned by GLImage
   DAMN IT. The link may be obsolete after FreeAndNil(GLImage)!
   Looks like I always set ScaledImage := nil after sucessfuly assigning it,
   but should keep an eye on it!}
  FreeAndNil(ScaledImage);

  ImageReady := false;
  ImageLoaded := false;
  InitGLPending := false;
end;

{----------------------------------------------------------------------------}

procedure DAbstractImage.InitGL;
begin
  if InitGLPending then begin
    InitGLPending := false;
    {if ScaledImage<>nil then} begin
      FreeAndNil(GLImage);
      ScaledWidth := ScaledImage.Width;
      ScaledHeight := ScaledImage.Height;
      GLImage := TGLImage.create(ScaledImage,true,true);
      ScaledImage := nil;
      ImageReady := true;
    end {else WriteLnLog('DAbstractElement.InitGL','ERROR: Scaled Image is nil!');}
  end;
end;

{----------------------------------------------------------------------------}

constructor DAbstractImage.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  color := vector4Single(1,1,1,1);
  InitGLPending := false;
  imageReady := false;
  imageLoaded := false;
end;

{----------------------------------------------------------------------------}

destructor DAbstractImage.destroy;
begin
  FreeImage;
  inherited;
end;

{----------------------------------------------------------------------------}

procedure DAbstractImage.rescale;
begin
  inherited;
  base.fixProportions(RealWidth,Realheight);
{  last.fixProportions(sourceImage.Width,sourceImage.height);
  next.fixProportions(sourceImage.Width,sourceImage.height);}
  RescaleImage;
end;

{----------------------------------------------------------------------------}

procedure DAbstractImage.RescaleImage;
begin
 {$IFNDEF AllowRescale}If SourceImage = nil then exit;{$ENDIF}
 if ImageLoaded then begin
   if Base.Initialized then
    if (ScaledWidth <> base.w) or (ScaledHeight <> base.h) then begin
      ImageReady := false;
      FreeAndNil(GLImage);
      ScaledImage := SourceImage.CreateCopy as TCastleImage;
      {$IFNDEF AllowRescale}FreeAndNil(SourceImage);{$ENDIF}
      ScaledImage.Resize(Base.w,Base.h,InterfaceScalingMethod);
      InitGLPending := true;
    end
   else
     WriteLnLog('DStaticImage.RescaleImage','ERROR: base.initialized = false');
 end;
end;

{----------------------------------------------------------------------------}

procedure DAbstractImage.Draw;
begin
  if ImageReady then begin
    //animate
    Update;

    if not Visible then Exit;

    GLImage.Color := Vector4(1,1,1,CurrentAnimationState.Opacity); //todo
    GLIMage.Draw(CurrentAnimationState.x1,CurrentAnimationState.y1,CurrentAnimationState.w,CurrentAnimationState.h); //todo
  end else begin
    if InitGLPending then InitGL;
  end;
end;

{=============================================================================}
{========================= static image ========================================}
{=============================================================================}


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
var PhaseShift: float;
begin
  if Lasttime = -1 then LastTime := DecoNow;
  PhaseShift := (DecoNow-LastTime)*phaseSpeed;
  if PhaseShift < 0.5 then begin
    Phase -= PhaseShift*(1+0.1*drnd.Random);
    if Phase<0 then Phase += 1;
    OpacityPhase -= PhaseShift/2*(1+0.2*drnd.Random);
    if OpacityPhase<0 then OpacityPhase += 1;
  end else begin
    //if pause was too long reinitialize with random phases.
    Phase := drnd.Random;
    OpacityPhase := drnd.Random;
  end;
  LastTime := DecoNow;
end;

{----------------------------------------------------------------------------}

procedure DWindImage.Draw;
var phase_scaled:integer;
begin
  if ImageReady then begin
    CyclePhase;

    if not visible then exit;

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
  if LastTime = -1 then begin
    LastTime := DecoNow;
    phase := 0;
  end;
  phaseshift := (DecoNow-LastTime)*PhaseSpeed;
  phase += phaseshift*(1+0.1*drnd.Random);
  if phase > 1 then begin
    phase := 1;
    LoadNewFloaterImage := true;
    ImageLoaded := false;
  end;
  LastTime := DecoNow;
end;

{----------------------------------------------------------------------------}

procedure DFloatImage.draw;
var x: integer;
begin
  if ImageReady then begin
    cyclePhase;

    if not visible then exit;

    color[3] := Opacity*sin(Pi*phase);
    GLImage.color := Color;
    x := round((window.width-base.w)*phase);
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
var x: integer;
begin
  if imageReady then begin
    update;

    if not visible then exit;
      GLImage.color := color;
      if max = min then begin
        writeLnLog('DBarImage.draw','ERROR: Division by zero!');
        exit;
      end;
      if Kind = bsVertical then begin
        x := round(base.h * position/(max-min));
        GLImage.draw(base.x1,base.y1,base.w,x);
      end else begin
        x := round(base.w * position/(max-min));
        GLImage.draw(base.x1,base.y1,x,base.h);
      end;
  end else
    if InitGLPending then InitGL;
end;

{---------------------------------------------------------------------------}

constructor DBarImage.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  min := 0;
  max := 1;
  position := 0;
  Kind := bsHorizontal;   //default for most progressbars and enemy health
end;

{================ Stat Bar image =========================================}

procedure DStatBarImage.update;
begin
  if target=nil then exit; //don't waste time if target isn't present

    inherited update;

  min := 0;
  //maybe pointers will be better? Still, it doesn't look inefficient;
  case Style of
    sbHealth: begin
                max := Target.MaxMaxHp;
                CurrentMax := Target.MaxHp;
                Position := Target.Hp;
                //color := Vector4Single(1,0,0,1);  //red  ? maybe green->red?
              end;
    sbStamina: begin
                max := Target.MaxMaxSta;
                CurrentMax := Target.MaxSta;
                Position := Target.Sta;
                //color := Vector4Single(1,1,0,1);  //yellow
              end;
    sbConcentration: begin
                max := Target.MaxMaxCNC;
                CurrentMax := Target.MaxCNC;
                Position := Target.CNC;
                //color := Vector4Single(0,1,1,1);  //cyan
              end;
    sbMetaphysics: begin
                max := Target.MaxMaxMph;
                CurrentMax := Target.MaxMph;
                Position := Target.Mph;
                //color := Vector4Single(1,0.5,1,1);  //purple
              end;
  end;
end;

{---------------------------------------------------------------------------}

procedure DStatBarImage.draw;
begin
  if target = nil then exit; //don't draw if target is absent
  //update; //already present in parent call
  inherited draw;
end;

{---------------------------------------------------------------------------}

constructor DStatBarImage.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Kind := bsVertical;   //default for player bars display
end;

end.

