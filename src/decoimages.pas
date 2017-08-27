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
  DecoInputOutput, DecoGlobal, DecoTime;

const InterfaceScalingMethod: TResizeInterpolation = riBilinear;  //to quickly change it. Maybe will be a variable some day to support older PCs.

type
  { General routines shared by images, frames and labels }
  DAbstractImage = class(DSingleInterfaceElement)
  protected
    GLImage: TGLImage;
    ImageReady, ImageLoaded: boolean;
    { keeps from accidentally re-initializing GL }
    InitGLPending: boolean;
    SourceImage: TCastleImage;  //todo scale Source Image for max screen resolution ?
    ScaledImage: TCastleImage;
  public
    Color: TVector4; //todo
    { initialize GL image.}
    procedure InitGL;
    { frees an image without freeing the whole instance }
    procedure FreeImage;
  public
    {due to a little bug I have to define these separately}
    //ScaledWidth, ScaledHeight: integer;
    { Thread-safe part of rescaling the image }
    procedure Rescale; override;
    { Scales the image to Base.size }
    procedure RescaleImage; virtual; abstract;
    procedure Draw; override;

    constructor Create; override;
    destructor Destroy; override;
  end;

type
  { 3x3 scaled image (i.e. frames) }
  DFrameImage = class(DAbstractImage)
  public
    CornerTop, CornerBottom, CornerLeft, CornerRight: integer;
    procedure RescaleImage; override;
    {todo: maybe not scale the image, but draw3x3 by OpenGl features?}
  end;

type
  { Simple image rescalied by stretch }
  DSimpleImage = class abstract(DAbstractImage)
  public
    { very simple draw procedure }
    procedure RescaleImage; override;
  end;

type
  { most simple image type }
  DStaticImage = class(DAbstractImage, ILoadObject)
  private
  {  LoadImageThread: TLoadImageThread;
    { if thread is running }}
    fThreadWorking: boolean;
  public
    { loads image in realtime }
    procedure Load(const URL: string); virtual;
    procedure Load(const CopyImage: TCastleImage);
    function ThreadLocked: boolean;
    procedure LockThread;
    procedure UnlockThread;
    procedure Afterload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    { loads image in a thread }
{    procedure LoadThread(const FileName:string);}
    constructor Create; override;
    destructor Destroy; override;
  end;

type
  { abstract "phased image" that moves and morphs with "phase" }
  DPhasedImage = class(DStaticImage)
  private
    LastTime: DTime;
  protected
    Phase, OpacityPhase: float;
  public
    PhaseSpeed: float;   {1/seconds to scroll the full screen}
 {   Opacity: float;
    procedure Load(const FileName:string); override; }
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

//var LoadNewFloaterImage: boolean;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog, CastleFilesUtils;

{=============================================================================}
{============================= Abstract Image ================================}
{=============================================================================}

procedure DAbstractImage.FreeImage;
begin
  FreeAndNil(GLImage);
  FreeAndNil(SourceImage);

  {BUG: I still need to free ScaledImage - while it's owned by GLImage
   DAMN IT. The link may be obsolete after FreeAndNil(GLImage)!
   Looks like I always set ScaledImage := nil after sucessfuly assigning it,
   but should keep an eye on it!}
  FreeAndNil(ScaledImage); //scaledImage is automatically freed by GlImage

  ImageReady := false;
  ImageLoaded := false;
  InitGLPending := false;
end;

{----------------------------------------------------------------------------}

procedure DAbstractImage.InitGL;
begin
  if InitGLPending then begin
    InitGLPending := false;
    if ScaledImage<>nil then begin
      FreeAndNil(GLImage);
      {ScaledWidth := ScaledImage.Width;      //not yet needed, maybe set RealWidth...
      ScaledHeight := ScaledImage.Height;}
      GLImage := TGLImage.Create(ScaledImage,true,true);
      ScaledImage := nil;
      ImageReady := true;
    end else WriteLnLog('DAbstractElement.InitGL','ERROR: Scaled Image is nil!');
  end;
end;

{----------------------------------------------------------------------------}

constructor DAbstractImage.Create;
begin
  inherited;
  Color := Vector4(1,1,1,1);
  InitGLPending := false;
  ImageReady := false;
  ImageLoaded := false;
end;

{----------------------------------------------------------------------------}

destructor DAbstractImage.Destroy;
begin
  FreeImage;
  inherited;
end;

{----------------------------------------------------------------------------}

procedure DAbstractImage.Rescale;
begin
  inherited;
  //Base.FixProportions(RealWidth,Realheight); //should be done by proportional scale?
  RescaleImage;
end;

{----------------------------------------------------------------------------}

procedure DAbstractImage.Draw;
begin
  if ImageReady then begin
    inherited; //calls update and checks if the image is visible
    GLImage.Color := Vector4(1,1,1,Current.Opacity); //todo
    GLIMage.Draw(Current.x1,Current.y1,Current.w,Current.h); //todo
  end else begin
    if InitGLPending then InitGL;
  end;
end;

{=============================================================================}
{============================== Frame Image ==================================}
{=============================================================================}

procedure DFrameImage.RescaleImage;
var ScaledImageParts: array [0..2,0..2] of TCastleImage;
    ix, iy: integer;
    UnscaledWidth, UnscaledHeight: integer;
    SourceXs, SourceYs, DestXs, DestYs: TVector4Integer;
begin
  ImageReady := false;
  FreeAndNil(GLImage);
  FreeAndNil(ScaledImage); //redundant, but let it be here

  ScaledImage := SourceImage.CreateCopy as TRGBAlphaImage;

  UnscaledWidth := ScaledImage.Width;
  UnscaledHeight := ScaledImage.Height;

  {check if minimal frame size is larger than the requested frame size}
  if CornerLeft+CornerRight+1 > Base.w then begin
    WriteLnLog('DAbstractInterfaceElement.FrameResize3x3','Reset backwards base.w = '+IntToStr(Base.w)+' / cornerLeft+cornerRight = '+IntToStr(CornerLeft+CornerRight));
    Base.w := CornerLeft+CornerRight+1;
    //Base.BackwardSetSize(Base.w,-1);
  end;
  if CornerTop+CornerBottom+1 > Base.h then begin
    WriteLnLog('DAbstractInterfaceElement.FrameResize3x3','Reset backwards base.h = '+inttostr(base.h)+' / cornerTop+cornerBottom = '+inttostr(cornerTop+cornerBottom));
    Base.h := CornerTop+CornerBottom+1;
    //Base.BackwardSetSize(-1,Base.h);
  end;

  SourceXs[0] := 0;
  SourceXs[1] := CornerLeft;
  SourceXs[2] := UnscaledWidth-CornerRight;
  SourceXs[3] := UnscaledWidth;
  SourceYs[0] := 0;
  SourceYs[1] := cornerBottom;
  SourceYs[2] := UnscaledHeight-CornerTop;
  SourceYs[3] := UnscaledHeight;
  DestXs[0] := 0;
  DestXs[1] := CornerLeft;
  DestXs[2] := Base.w-CornerRight;
  DestXs[3] := Base.w;
  DestYs[0] := 0;
  DestYs[1] := CornerBottom;
  DestYs[2] := Base.h-CornerTop;
  DestYs[3] := Base.h;

  for ix := 0 to 2 do
   for iy := 0 to 2 do begin
     ScaledImageParts[ix,iy] := TRGBAlphaImage.Create;
     ScaledImageParts[ix,iy].SetSize(SourceXs[ix+1]-SourceXs[ix],SourceYs[iy+1]-SourceYs[iy]);
     ScaledImageParts[ix,iy].Clear(Vector4Byte(0,0,0,0));
     ScaledImageParts[ix,iy].DrawFrom(SourceImage,0,0,SourceXs[ix],SourceYs[iy],SourceXs[ix+1]-SourceXs[ix],SourceYs[iy+1]-SourceYs[iy],dmBlendSmart);
     ScaledImageParts[ix,iy].Resize(DestXs[ix+1]-DestXs[ix],DestYs[iy+1]-DestYs[iy],riNearest);
   end;

  ScaledImage.SetSize(Base.w,Base.h,1);
  ScaledImage.Clear(Vector4byte(0,0,0,0));
  for ix := 0 to 2 do
    for iy := 0 to 2 do ScaledImage.DrawFrom(ScaledImageParts[ix,iy],DestXs[ix],DestYs[iy],0,0,DestXs[ix+1]-DestXs[ix],DestYs[iy+1]-DestYs[iy],dmBlendSmart);

  for ix := 0 to 2 do
    for iy := 0 to 2 do FreeAndNil(ScaledImageParts[ix,iy]);

  //and burn the burner
  //FrameImage.DrawFrom(BURNER_IMAGE,0,0,base.x1,base.y1,base.w,base.h,dmMultiply);

  InitGLPending := true;
end;

{=============================================================================}
{======================== Simple image =======================================}
{=============================================================================}

procedure DSimpleImage.RescaleImage;
begin
 {$IFNDEF AllowRescale}If SourceImage = nil then Exit;{$ENDIF}
 if ImageLoaded then begin
   if Base.isInitialized then begin
     ImageReady := false;
     FreeAndNil(GLImage);
     FreeAndNil(ScaledImage); //redundant, but let it be here
     ScaledImage := SourceImage.CreateCopy as TCastleImage;
     {$IFNDEF AllowRescale}FreeAndNil(SourceImage);{$ENDIF}
     ScaledImage.Resize(Base.w,Base.h,InterfaceScalingMethod);
     InitGLPending := true;
   end
   else
     WriteLnLog('DSimpleImage.RescaleImage','ERROR: Base.Initialized = false');
 end;
end;


{=============================================================================}
{======================== static image =======================================}
{=============================================================================}

function DStaticImage.ThreadLocked: boolean;
begin
 Result := fThreadWorking;
end;
procedure DStaticImage.LockThread;
begin
  fThreadWorking := true;
end;
procedure DStaticImage.UnlockThread;
begin
  fThreadWorking := true;
end;


{procedure TLoadImageThread.execute;
var TargetImage: DSTaticImage;
begin
  TargetImage := Target as DStaticImage;

  WritelnLog('TLoadImageThread.execute','Image thread started.');

  //****TargetImage.Load(FileName);
  //****TargetImage.Rescale;

  WritelnLog('TLoadImageThread.execute','Image thread finished.');
  //****TargetImage.ThreadWorking := false;
end;}

{-----------------------------------------------------------------------------}

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

procedure DStaticImage.Load(const URL: string);
begin
  WritelnLog('DStaticImage.LoadImage',URL);
  SourceImage := LoadImageSafe({ApplicationData(URL)}URL);
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
  Base.RealWidth := SourceImage.Width;
  Base.RealHeight := SourceImage.Height;
  {ScaledWidth := -1;
  ScaledHeight := -1;}
  ImageLoaded := true;
end;

{----------------------------------------------------------------------------}

constructor DStaticImage.Create;
begin
  inherited;
  fThreadWorking := false;
end;

{----------------------------------------------------------------------------}

destructor DStaticImage.Destroy;
begin
  {if ThreadWorking then begin
    Try
      LoadImageThread.Terminate;
    finally
      FreeAndNil(LoadImageThread); //redundant, as freeonterminate=true?
    end;
  end;}
  inherited;
end;

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

