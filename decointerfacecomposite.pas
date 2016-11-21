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

{---------------------------------------------------------------------------}

{ Contains groups of items containing several specific interface elements
  used in different game situations }
unit decointerfacecomposite;

{$INCLUDE compilerconfig.inc}

interface

uses classes,
  castleImages,
  decointerface, decoactor, decoplayercharacter,
  decoglobal, decoimages;


type
  { wrapper for composite Interface elements with ArrangeChildren procedure
    each child will define its own arrange method }
  DAbstractCompositeInterfaceElement = class(DInterfaceElement)
  public
    { arranges children within base.w, base.h }
    procedure ArrangeChildren(animate: boolean); virtual; abstract;
    procedure setbasesize(const newx,newy,neww,newh,newo: float; animate: boolean); override;
end;

type
  {HP,STA,CNC and MPH vertical bars for a player character}
  DPlayerBars = class(DAbstractCompositeInterfaceElement)
  private
    ftarget: DActor;
    procedure settarget(value: DActor);
  public
    {these are links for easier access to the children}
    HP_bar, STA_bar, CNC_bar, MPH_bar: DSingleInterfaceElement;
    {the character being monitored}
    property Target: DActor read ftarget write settarget;
    constructor create(AOwner: TComponent); override;
    procedure ArrangeChildren(animate: boolean); override;
end;

type
  { character portrait. Some day it might be replaced for TUIContainer of
    the 3d character face :) Only 2D inanimated image for now... }
  DPortrait = class(DSingleInterfaceElement)
  private
    fTarget: DPlayerCharacter;
    procedure settarget(value: DPlayerCharacter);
  public
    property Target: DPlayerCharacter read ftarget write settarget;
    constructor create(AOwner: TComponent); override;
  end;


var HealthBarImage: TCastleImage; //todo not freed automatically!!!
    PortraitIMG: TCastleImage; //todo!!!

procedure InitCompositeInterface;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

implementation
uses CastleLog, CastleFilesUtils, castleVectors,
  decogui,
  decolabel;

procedure DAbstractCompositeInterfaceElement.setbasesize(const newx,newy,neww,newh,newo: float; animate: boolean);
begin
  inherited setbasesize(newx,newy,neww,newh,newo,animate);
  ArrangeChildren(animate);
end;

{===========================================================================}

procedure InitCompositeInterface;
begin
  HealthBarImage := LoadImage(ApplicationData(ProgressBar_folder+'verticalbar.png'));
  PortraitIMG := LoadImage(ApplicationData(PortraitFolder+'AF.jpg'));
end;

{===========================================================================}
{=================== player bars ===========================================}
{===========================================================================}

constructor DPlayerBars.create(AOwner: TComponent);
var tmp_bar: DStatBarImage;
    tmp_element: DSingleInterfaceElement;
begin
  inherited create(AOwner);
  frame := BlackFrame;

  //or make parent nil? as they are freed by freeing children? Keep an eye out for troubles...
  tmp_element := DSingleInterfaceElement.create(self);
  tmp_bar := DStatBarImage.create(tmp_element);
  tmp_bar.Load(HealthBarImage);
  tmp_bar.Style := sbHealth;
  tmp_bar.Kind := bsVertical;
  tmp_element.Content := tmp_bar;
  tmp_element.frame := SimpleFrame;
  HP_bar := tmp_element;
  self.children.add(tmp_element);

  tmp_element := DSingleInterfaceElement.create(self);
  tmp_bar := DStatBarImage.create(tmp_element);
  tmp_bar.Load(HealthBarImage);
  tmp_bar.Style := sbStamina;
  tmp_bar.Kind := bsVertical;
  tmp_element.Content := tmp_bar;
  tmp_element.frame := SimpleFrame;
  STA_bar := tmp_element;
  self.children.add(tmp_element);

  tmp_element := DSingleInterfaceElement.create(self);
  tmp_bar := DStatBarImage.create(tmp_element);
  tmp_bar.Load(HealthBarImage);
  tmp_bar.Style := sbConcentration;
  tmp_bar.Kind := bsVertical;
  tmp_element.Content := tmp_bar;
  tmp_element.frame := SimpleFrame;
  CNC_bar := tmp_element;
  self.children.add(tmp_element);

  tmp_element := DSingleInterfaceElement.create(self);
  tmp_bar := DStatBarImage.create(tmp_element);
  tmp_bar.Load(HealthBarImage);
  tmp_bar.Style := sbMetaphysics;
  tmp_bar.Kind := bsVertical;
  tmp_element.Content := tmp_bar;
  tmp_element.frame := SimpleFrame;
  MPH_bar := tmp_element;
  self.children.add(tmp_element);
end;

{-----------------------------------------------------------------------------}

procedure DPlayerBars.settarget(value: DActor);
begin
  if ftarget <> value then begin
    ftarget := value;
    //and copy the target to all children
    (HP_bar.content as DStatBarImage).Target := ftarget;
    (STA_bar.content as DStatBarImage).Target := ftarget;
    (CNC_bar.content as DStatBarImage).Target := ftarget;
    (MPH_bar.content as DStatBarImage).Target := ftarget;
  end;
end;

{-----------------------------------------------------------------------------}

procedure DPlayerBars.ArrangeChildren(animate: boolean);
var scalex: float;
begin
  if not base.initialized then begin
    writeLnLog('DPlayerBars.ArrangeChildren','ERROR: Base is not initialized!');
    exit;
  end;
  {metaphysics bar is displayed only if the character is known to posess metaphysic skills}
  if ftarget.maxmaxmph > 0 then scalex := base.fw/4 else scalex := base.fw/3;
  HP_bar. setbasesize(base.fx         ,base.fy,scalex,base.fh,base.opacity,animate);
  STA_bar.setbasesize(base.fx+  scalex,base.fy,scalex,base.fh,base.opacity,animate);
  CNC_bar.setbasesize(base.fx+2*scalex,base.fy,scalex,base.fh,base.opacity,animate);
  if ftarget.maxmaxmph > 0 then
  MPH_bar.setbasesize(base.fx+3*scalex,base.fy,scalex,base.fh,base.opacity,animate);
end;

{========================== Character portrait ===============================}

procedure DPortrait.settarget(value: DPlayerCharacter);
begin
  ftarget := value;
  (content as DStaticImage).freeImage;
  WriteLnLog('DPortrait.settarget','Load from portrait');
  (content as DStaticImage).Load(PortraitFolder+'AF.jpg'{PortraitIMG});  //todo
end;

constructor DPortrait.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  content := DStaticImage.create(self);
  frame := BlackFrame;
end;

{=============================================================================}

end.

