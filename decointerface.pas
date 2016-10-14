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
unit decointerface;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, sysutils,
  CastleLog, castleFilesUtils, CastleRandom,
  castleVectors, CastleGLImages, CastleImages,
  decoglobal;

const Frames_Folder = 'interface/frames/';

const fullwidth = -1;
      fullheight = -2;


{1/17 of window.height is a "unit" in GUI scale.
Basically calculated as 4 characters vertical space allocation
3 lines of buttons for each
and add one line at the bottom for menu button and other stuff
i.e. 3*4+1 units in window.height
Most often equal scale is used for width - in fractions of height to maintain squares etc.}

const
  GUI_grid = (4*3+1);
  GUI_scale_unit_float = 1/GUI_grid;

Type
  { most abstract container suitable for images, labels and interface elements
    Just defines the box and rescaling }
  DAbstractElement = class(TComponent)
  public
    procedure InitGL;
    procedure setsize(const newx,newy,neww,newh:float);
    procedure Rescale;
    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
  private
    { these values are "strict" they are determined by rescale and unaffected
      by animations }
    x1,y1,x2,y2,w,h: integer;
    fx,fy,fw,fh: float;

    { keeps from accidentally re-initing GL }
    InitGLPending: boolean;
    ImageReady: boolean;
    SourceImage: TCastleImage;  //todo scale Source Image for max screen resolution ? //todo never store on Android.
    ScaledImage: TCastleImage;
    GLImage: TGLImage;
    procedure RescaleImage;
  end;

Type
  DAbstractInterfaceElement = class(DAbstractElement)
  end;

type DInterfaceChildrenList = specialize TFPGObjectList<DAbstractInterfaceElement>;

Type
  DInterfaceElement = class(DAbstractInterfaceElement)
  public
    children: DInterfaceChildrenList;
  end;

Type
  DInterfaceContainer = class(DInterfaceElement)
  public
    { random generator used for all interface random events }
    rnd: TCastleRandom;
    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
end;

var GUI: DInterfaceContainer;

implementation

{=============================================================================}
{=========================== Abstract element ================================}
{=============================================================================}


procedure DAbstractElement.InitGL;
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

procedure DAbstractElement.setsize(const newx,newy,neww,newh:float);
begin
  if (abs(newx)>GUI_grid) or (abs(newy)>GUI_grid) or
     (((neww<0) or (neww>GUI_grid)) and ((neww<>fullwidth) and (neww<>fullheight))) or
     (((newh<0) or (newh>GUI_grid)) and (newh<>fullheight)) then
  begin
    writeLnLog('DAbstractElement.setsize','ERROR: Incorrect newx,newy,neww,newh!');
    exit;
  end;

  { stop if nothing was changed }
  if (fx=newx) and (fy=newy) and (fw=neww) and (fh=newh) then exit;

  fx:=newx;
  fy:=newy;
  fw:=neww;
  fh:=newh;

  self.Rescale;

end;

procedure DAbstractElement.Rescale;
begin
  { convert float to integer }
  if fx>0 then
    x1 := round(GUI.h*fx*GUI_scale_unit_float)
  else
    x1 := GUI.w - round(GUI.h*fx*GUI_scale_unit_float);

  if fy>0 then
    y1 := round(GUI.h*fy*GUI_scale_unit_float)     // turn over y-axis?
  else
    y1 := GUI.h - round(GUI.h*fy*GUI_scale_unit_float);

  if fw = fullwidth then begin
    w := GUI.w;
    x1 := 0
  end
  else
  if fw = fullheight then
    w := GUI.h
  else
    w := round(GUI.h*fw*GUI_scale_unit_float);

  if fh = fullheight then begin
    h := GUI.h;
    y1 := 0
  end else
    h := round(GUI.h*fh*GUI_scale_unit_float);

  x2:=x1+w;
  y2:=y1+h;
end;

procedure DAbstractElement.RescaleImage;
begin

end;

{----------------------------------------------------------------------------}

constructor DAbstractElement.Create(AOwner: TComponent);
begin
  InitGLPending := false;
  imageReady := false;
end;

destructor DAbstractElement.destroy;
begin
  FreeAndNil(GLImage);
  //scaledImage is automatically freed by GlImage
  FreeAndNil(SourceImage);
  inherited;
end;

{=============================================================================}
{========================== interface container ==============================}
{=============================================================================}

constructor DInterfaceContainer.create(AOwner: TComponent);
begin
  writeLnLog('DInterfaceContainer.create','Creating interface.');
  inherited create(AOwner);
  rnd := TCastleRandom.Create;
end;

destructor DInterfaceContainer.destroy;
begin
  writeLnLog('DInterfaceContainer.destroy','Game over...');
  freeandnil(rnd);
  inherited;
end;

end.

