unit decoimages;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, CastleLog,
  castleVectors, CastleGLImages, CastleImages,
  decointerface,
  decoglobal;

type
  DAbstractImage = class(DAbstractElement)
  { General routines shared by images and labels }
  public
    { Thread-safe part of rescaling the image }
    procedure RescaleImage;
    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
    procedure draw; override;
  private
    SourceImage: TCastleImage;  //todo scale Source Image for max screen resolution ? //todo never store on Android.
    ScaledImage: TCastleImage;
    { keeps from accidentally re-initing GL }
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
    Opacity: float;
    Function GetAnimationState: Txywha; override;
  end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses decogui; //todo

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

{----------------------------------------------------------------------------}

Function DStaticImage.GetAnimationState: Txywha;
begin
  result := Txywha.create(nil);
  result.x1 := base.x1;
  result.x2 := base.x2;
  result.opacity := Opacity;
end;


end.

