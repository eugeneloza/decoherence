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
unit DecoImages;

{$INCLUDE compilerconfig.inc}
interface

uses Classes,
  CastleVectors, CastleGLImages, CastleImages,
  DecoInterface, DecoActor,
  DecoGlobal, DecoTime;

type
  { General routines shared by images and labels }
  DAbstractImage = class(DAbstractElement)
  public
 {   color: TVector4; //todo
    { very simple draw procedure }
    procedure Draw; override;
    constructor Create(AOwner: TComponent); override;
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
    procedure RescaleImage; virtual; }
  end;

  {Type
   { Several types of frames, including with captions }
   DFrame = class(TComponent)
   public
     SourceImage: TRGBAlphaImage;
     {frame borders}
     CornerTop, CornerBottom, CornerLeft, CornerRight: integer;
     Rectagonal: boolean;
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
  end;     }


type TLoadImageThread = class(TThread)
  public
    Target: DAbstractImage;
    FileName: String;
  protected
    procedure Execute; override;
  end;

type
  { most simple image type }
  DStaticImage = class(DAbstractImage)
  private
  {  LoadImageThread: TLoadImageThread;
    { if thread is running }
    ThreadWorking: boolean;
  public
    { loads image in realtime }
    procedure Load(const FileName: string); virtual;
    procedure Load(const CopyImage: TCastleImage);
    procedure Afterload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    { loads image in a thread }
    procedure LoadThread(const FileName:string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;     }
  end;

type
  { abstract "phased image" that moves and morphs with "phase" }
  DPhasedImage = class(DStaticImage)
  public
 {   PhaseSpeed: float;   {1/seconds to scroll the full screen}
    Opacity: float;
    //constructor Create(AOwner: TComponent); override;
    procedure Load(const FileName:string); override;
  private
    LastTime: DTime;
  public
    Phase, OpacityPhase: float;    }
  end;

type
  { Wind and smoke effects used in different situations }
  //todo might be descendant of DStaticImage
  DWindImage = class(DPhasedImage)
  public
    { completely overrides the default drawing procedure }
{    procedure Draw; override;
  private
    procedure CyclePhase;  }
  end;

type
  { A floating image for LoadScreens }
  DFloatImage = class(DPhasedImage)
  public
 {   procedure Draw; override;
  private
    procedure CyclePhase;  }
  end;

Type TBarStyle = (bsVertical, bsHorizontal);

Type
  { Generic bar used for progress bars and health bars }
  DBarImage = class(DStaticImage)
  public
    { minimmum, maximum, current maximum and current position
      minimum usually is zero and automatically set in constructor }
 {   Min, Max, CurrentMax, Position: float;
    { vertical or horizontal style of the bar }
    Kind: TBarStyle;
    procedure Draw; override;
    constructor Create(AOwner: TComponent); override;    }
  end;

Type TStatBarStyle = (sbHealth, sbStamina, sbConcentration, sbMetaphysics);

Type
  { bar to display health for mobs and primary 4 stats for player characters }
  DStatBarImage = class(DBarImage)
  public
    { Points to the actor for who the health is displayed}
  {  Target: DActor;
    { determines which value to display: Health, Stamina, Concentration or Metaphysics}
    Style: TStatBarStyle;
    procedure Update; override;
    procedure Draw; override;
    constructor Create(AOwner: TComponent); override;    }
  end;

var LoadNewFloaterImage: boolean;


{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog, CastleFilesUtils,
  DecoInputOutput;

procedure TLoadImageThread.execute;
var TargetImage: DSTaticImage;
begin
  TargetImage := Target as DStaticImage;

  WritelnLog('TLoadImageThread.execute','Image thread started.');

  //****TargetImage.Load(FileName);
  //****TargetImage.Rescale;

  WritelnLog('TLoadImageThread.execute','Image thread finished.');
  //****TargetImage.ThreadWorking := false;
end;

{-----------------------------------------------------------------------------}

{constructor DStaticImage.Create(AOwner: TComponent);
begin
  ThreadWorking := false;
  Inherited Create(AOwner);
end;}

{----------------------------------------------------------------------------}

{destructor DStaticImage.Destroy;
begin
  if ThreadWorking then begin
    Try
      LoadImageThread.Terminate;
    finally
      FreeAndNil(LoadImageThread); //redundant, as freeonterminate=true?
    end;
  end;
  inherited;
end; }

{----------------------------------------------------------------------------}

{procedure DStaticImage.LoadThread(const FileName: string);
begin
 if not ThreadWorking then begin
   LoadImageThread := TLoadImageThread.Create(true);
   LoadImageThread.Target := Self;
   LoadImageThread.FileName := FileName;
   LoadImageThread.FreeOnTerminate := true;
   LoadImageThread.Priority := tpLower;
   ThreadWorking := true;
   LoadImageThread.Start;
 end
 else
   writeLnLog('DStaticImage.LoadThread','Thread already working...');
end; }

{----------------------------------------------------------------------------}

{procedure DStaticImage.Load(const FileName: string);
begin
  WritelnLog('DStaticImage.LoadImage',FileName);
  SourceImage := LoadImageSafe(ApplicationData(FileName));
  AfterLoad;
end;
procedure DStaticImage.Load(const CopyImage: TCastleImage);
begin
  WritelnLog('DStaticImage.LoadImage','Copying image from '+CopyImage.ClassName);
  SourceImage := CopyImage.MakeCopy;
  AfterLoad;
end;
procedure DStaticImage.AfterLoad;
begin
  RealWidth := SourceImage.Width;
  RealHeight := SourceImage.Height;
  ScaledWidth := -1;
  ScaledHeight := -1;
  ImageLoaded := true;
end; }

{----------------------------------------------------------------------------}

{procedure DAbstractImage.FreeImage;
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
end;  }

{----------------------------------------------------------------------------}

{procedure DAbstractImage.InitGL;
begin
  if InitGLPending then begin
    InitGLPending := false;
    {if ScaledImage<>nil then} begin
      FreeAndNil(GLImage);
      ScaledWidth := ScaledImage.Width;
      ScaledHeight := ScaledImage.Height;
      GLImage := TGLImage.Create(ScaledImage,true,true);
      ScaledImage := nil;
      ImageReady := true;
    end {else WriteLnLog('DAbstractElement.InitGL','ERROR: Scaled Image is nil!');}
  end;
end;}

{----------------------------------------------------------------------------}

{constructor DAbstractImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := Vector4(1,1,1,1);
  InitGLPending := false;
  ImageReady := false;
  ImageLoaded := false;
end; }

{----------------------------------------------------------------------------}

{destructor DAbstractImage.Destroy;
begin
  FreeImage;
  inherited;
end; }

{----------------------------------------------------------------------------}

{procedure DAbstractImage.Rescale;
begin
  inherited;
  Base.FixProportions(RealWidth,Realheight);
{  last.fixProportions(sourceImage.Width,sourceImage.height);
  next.fixProportions(sourceImage.Width,sourceImage.height);}
  RescaleImage;
end; }

{----------------------------------------------------------------------------}

{procedure DAbstractImage.RescaleImage;
begin
 {$IFNDEF AllowRescale}If SourceImage = nil then Exit;{$ENDIF}
 if ImageLoaded then begin
   if Base.Initialized then
   {this is not optimal, however, required in case content has been changed}
    {if (ScaledWidth <> base.w) or (ScaledHeight <> base.h) then} begin
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
end; }

{----------------------------------------------------------------------------}

{procedure DAbstractImage.Draw;
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
end; }

{=============================================================================}
{========================= static image ========================================}
{=============================================================================}


{=============================================================================}
{======================== phased image =======================================}
{=============================================================================}
{
constructor DPhasedImage.Create(AOwner: TComponent);
begin
  inherited;
end;
}
{----------------------------------------------------------------------------}

{Procedure DPhasedImage.Load(const FileName:string);
begin
  inherited Load(filename);
  LastTime := -1;
end; }

{=============================================================================}
{========================= wind image ========================================}
{=============================================================================}

{procedure DWindImage.CyclePhase;
var PhaseShift: float;
begin
  if Lasttime = -1 then LastTime := DecoNow;
  PhaseShift := (DecoNow-LastTime)*PhaseSpeed;
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
end; }

{----------------------------------------------------------------------------}

{procedure DWindImage.Draw;
var PhaseScaled:integer;
begin
  if ImageReady then begin
    CyclePhase;

    if not Visible then Exit;

    Color[3] := Opacity + Opacity/4 * Sin(2*Pi*OpacityPhase);
    GLImage.Color := Color;
    PhaseScaled := Round(Phase*Window.Width);

    //draw first part of the image
    GLImage.Draw(PhaseScaled,0,
                 Window.Width-PhaseScaled,Window.Height,
                 0,0,
                 Window.Width-PhaseScaled,Window.Height);
    //draw second part of the image
    GLImage.Draw(0,0,
                 PhaseScaled,Window.Height,
                 Window.Width-PhaseScaled,0,
                 PhaseScaled,Window.Height);
  end else begin
    if InitGLPending then InitGL;
//    WriteLnLog('DWindImage.Draw','ERROR: Wind image not ready to draw!');
  end;
end; }

{=============================================================================}
{========================= float image =======================================}
{=============================================================================}

{procedure DFloatImage.CyclePhase;
var PhaseShift: float;
begin
  if LastTime = -1 then begin
    LastTime := DecoNow;
    Phase := 0;
  end;
  PhaseShift := (DecoNow-LastTime)*PhaseSpeed;
  Phase += PhaseShift*(1+0.1*drnd.Random);
  if Phase > 1 then begin
    Phase := 1;
    LoadNewFloaterImage := true;
    ImageLoaded := false;
  end;
  LastTime := DecoNow;
end;  }

{----------------------------------------------------------------------------}

{procedure DFloatImage.Draw;
var x: integer;
begin
  if ImageReady then begin
    CyclePhase;

    if not Visible then Exit;

    Color[3] := Opacity*Sin(Pi*Phase);
    GLImage.Color := Color;
    x := Round((Window.Width-Base.w)*Phase);
    GLImage.Draw(x,0);
  end else begin
    if InitGLPending then InitGL;
  //   WritelnLog('DStaticImage.DrawMe','ERROR: Static Image not ready to draw!');
  end;
end; }

{=============================================================================}
{=========================== bar image =======================================}
{=============================================================================}

{procedure DBarImage.draw;
var x: integer;
begin
  if ImageReady then begin
    Update;

    if not Visible then Exit;
      GLImage.Color := Color;
      if Max = Min then begin
        writeLnLog('DBarImage.draw','ERROR: Division by zero!');
        Exit;
      end;
      if Kind = bsVertical then begin
        x := Round(Base.h * Position/(Max-Min));
        GLImage.Draw(Base.x1,Base.y1,Base.w,x);
      end else begin
        x := Round(Base.w * Position/(Max-Min));
        GLImage.Draw(Base.x1,Base.y1,x,Base.h);
      end;
  end else
    if InitGLPending then InitGL;
end;   }

{---------------------------------------------------------------------------}

{constructor DBarImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Min := 0;
  Max := 1;
  Position := 0;
  Kind := bsHorizontal;   //default for most progressbars and enemy health
end;  }

{================ Stat Bar image =========================================}

{procedure DStatBarImage.Update;
  function AboveZero(const a: float): float; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  begin
    if a > 0 then Result := a else Result := 0;
  end;
begin
  if Target = nil then Exit; //don't waste time if target isn't present

  inherited Update;

  Min := 0;
  //maybe pointers will be better? Still, it doesn't look inefficient;
  case Style of
    sbHealth: begin
                Max := Target.MaxMaxHp;
                CurrentMax := AboveZero(Target.MaxHp);
                Position := AboveZero(Target.Hp);
                //color := Vector4Single(1,0,0,1);  //red  ? maybe green->red?
              end;
    sbStamina: begin
                Max := Target.MaxMaxSta;
                CurrentMax := AboveZero(Target.MaxSta);
                Position := AboveZero(Target.Sta);
                //color := Vector4Single(1,1,0,1);  //yellow
              end;
    sbConcentration: begin
                Max := Target.MaxMaxCNC;
                CurrentMax := AboveZero(Target.MaxCNC);
                Position := AboveZero(Target.CNC);
                //color := Vector4Single(0,1,1,1);  //cyan
              end;
    sbMetaphysics: begin
                Max := Target.MaxMaxMph;
                CurrentMax := AboveZero(Target.MaxMph);
                Position := AboveZero(Target.Mph);
                //color := Vector4Single(1,0.5,1,1);  //purple
              end;
  end;
end;  }

{---------------------------------------------------------------------------}

{procedure DStatBarImage.Draw;
begin
  if Target = nil then Exit; //don't draw if target is absent
  //update; //already present in parent call
  inherited Draw;
end;  }

{---------------------------------------------------------------------------}

{constructor DStatBarImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Kind := bsVertical;   //default for player bars display
end;  }

end.

