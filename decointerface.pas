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
  Classes, fgl,
  castleVectors, castleImages,
  decoglobal;

const Frames_Folder = 'interface/frames/';
      LoadScreen_folder = 'interface/loadscreen/';

const fullwidth = -1;
      fullheight = -2;

const InterfaceScalingMethod: TResizeInterpolation = riBilinear;


{1/17 of window.height is a "unit" in GUI scale.
Basically calculated as 4 characters vertical space allocation
3 lines of buttons for each
and add one line at the bottom for menu button and other stuff
i.e. 3*4+1 units in window.height
Most often equal scale is used for width - in fractions of height to maintain squares etc.}

const
  GUI_grid = (4*3+1);
  GUI_scale_unit_float = 1/GUI_grid;

type
  { Yes, that looks stupid for now. But I'll simplify it later }
  Txywh = class(TComponent)
  public
    { integer "box" }
    x1,y1,x2,y2,w,h:integer;
    { float }
    fx,fy,fw,fh: float;
    initialized: boolean;
    { assign float and convert to Integer }
    procedure setsize(const newx,newy,neww,newh:float);
    procedure recalculate;
    constructor create(AOwner: TComponent); override;
  end;

type
  { extended Txywh with opacity for animations }
  Txywha = class(Txywh)
  public
    opacity: float;
  end;

Type
  { most abstract container suitable for images, labels and interface elements
    Just defines the box and rescaling }
  DAbstractElement = class(TComponent)
  public
    { these values are "strict" and unaffected by animations. Usually determines
      the basic stage and implies image rescale and init GL. }
    base: Txywh;
    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
    procedure rescale; virtual;
    procedure draw; virtual; abstract;
    function GetAnimationState: Txywha; virtual;
  private
    { Last and Next animation states. }
    last, next: Txywha;
//    animation_start, animation_end: TDateTime;
//    Free_on_end: boolean;
  end;

Type
  DAbstractInterfaceElement = class(DAbstractElement)
  end;

type DInterfaceChildrenList = specialize TFPGObjectList<DAbstractInterfaceElement>;

Type
  DInterfaceElement = class(DAbstractInterfaceElement)
  public
    parent: DAbstractInterfaceElement;
    children: DInterfaceChildrenList;
    procedure draw; override;
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure Rescale; override;
  end;


{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses sysutils, CastleLog, castleFilesUtils,
  decogui;

{=============================================================================}
{=========================== Abstract element ================================}
{=============================================================================}


constructor Txywh.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  initialized:=false;

end;

procedure Txywh.setsize(const newx,newy,neww,newh:float);
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

  recalculate;
end;

procedure Txywh.recalculate;
begin
  { convert float to integer }

  if fx>0 then
    x1 := round(Window.height*fx*GUI_scale_unit_float)
  else
    x1 := Window.width - round(Window.height*fx*GUI_scale_unit_float);

  if fy>0 then
    y1 := round(Window.height*fy*GUI_scale_unit_float)     // turn over y-axis?
  else
    y1 := Window.height - round(Window.height*fy*GUI_scale_unit_float);

  if fw = fullwidth then begin
    w := Window.width;
    x1 := 0
  end
  else
  if fw = fullheight then
    w := Window.height
  else
    w := round(Window.height*fw*GUI_scale_unit_float);

  if fh = fullheight then begin
    h := Window.height;
    y1 := 0
  end else
    h := round(Window.height*fh*GUI_scale_unit_float);

  x2:=x1+w;
  y2:=y1+h;

  initialized:=true;
end;

{============================================================================}


procedure DAbstractElement.rescale;
begin
  base.recalculate;
  last.recalculate;
  next.recalculate;
end;

{----------------------------------------------------------------------------}

Function DAbstractElement.GetAnimationState: Txywha;
begin
  if true then begin //todo!!!!!!!!!!!!!!!!!!!!!!!
    result := Txywha.create(nil);
    result.x1 := base.x1;
    result.x2 := base.x2;
    result.y1 := base.y1;
    result.y2 := base.y2;
    result.h := base.h;
    result.w := base.w;
    result.opacity := 1;
  end;
end;

{----------------------------------------------------------------------------}


constructor DAbstractElement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  base := Txywh.Create(self);
  last := Txywha.Create(self);
  next := Txywha.Create(self);
end;

destructor DAbstractElement.destroy;
begin
  //actulally this is not needed as they are owned by the class
{  freeandnil(base);
  freeandnil(last);
  freeandnil(next);}

  inherited;
end;

{=============================================================================}
{=========================== interface element ===============================}
{=============================================================================}

procedure DInterfaceElement.rescale;
var i:integer;
begin
  inherited;
  for i:=0 to children.Count-1 do children[i].rescale;
end;

{-----------------------------------------------------------------------------}

constructor DInterfaceElement.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  if AOwner is DAbstractInterfaceElement then parent:=AOwner as DAbstractInterfaceElement;
  children := DInterfaceChildrenList.Create(true);
end;

destructor DInterfaceElement.destroy;
begin
  freeandnil(children);   //this should fire as recoursive because children owns elements, which in turn will fire their destructors onfree
  inherited;
end;

procedure DInterfaceElement.draw;
var i:integer;
begin
  //todo
  //frame.draw;
  //content.draw;
  for i:=0 to children.Count-1 do children[i].draw;
end;


end.

