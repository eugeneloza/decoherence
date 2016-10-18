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


implementation

procedure DAbstractImage.InitGL;
begin
  if InitGLPending then begin
    InitGLPending:=false;
    if ScaledImage<>nil then begin
      WriteLnLog('DAbstractElement.InitGL','Initializing...');
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

  end;
end;

{----------------------------------------------------------------------------}

end.

