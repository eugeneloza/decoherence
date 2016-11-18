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
  DAbstractCompositeInterfaceElement = class(DInterfaceElement)
  public
    procedure ArrangeChildren; virtual; abstract;
end;

type
  DPlayerBars = class(DAbstractCompositeInterfaceElement)
  private
    ftarget: DActor;
    procedure settarget(value: DActor);
  public
    {these are links for easier access to the children}
    HP_bar,STA_bar,CNC_bar,MPH_bar: integer;
    property Target: DActor read ftarget write settarget;
    constructor create(AOwner: TComponent); override;
    procedure ArrangeChildren; override;
end;

var HealthBarImage: TCastleImage; //not freed automatically!!!

procedure InitCompositeInterface;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

implementation
uses CastleLog, CastleFilesUtils, castleVectors,
  decogui,
  decolabel;

procedure InitCompositeInterface;
begin
  HealthBarImage := LoadImage(ApplicationData(ProgressBar_folder+'verticalbar.png'));
end;

{---------------------------------------------------------------------------}

constructor DPlayerBars.create(AOwner: TComponent);
var tmp_bar: DStatBarImage;
    tmp_element: DInterfaceElement;
begin
  inherited create(AOwner);
  frame := BlackFrame;

  //or make parent nil? as they are freed by freeing children?
  tmp_element := DInterfaceElement.create(self);
  tmp_bar := DStatBarImage.create(tmp_element);
  tmp_bar.Load(HealthBarImage);
  tmp_bar.Style := sbHealth;
  tmp_bar.color := Vector4Single(1,0,0,1);
  tmp_element.Content := tmp_bar;
  tmp_element.frame := SimpleFrame;
  HP_bar := self.children.add(tmp_element);

  tmp_element := DInterfaceElement.create(self);
  tmp_bar := DStatBarImage.create(tmp_element);
  tmp_bar.Load(HealthBarImage);
  tmp_bar.Style := sbStamina;
  tmp_bar.color := Vector4Single(1,1,0,1);
  tmp_element.Content := tmp_bar;
  tmp_element.frame := SimpleFrame;
  STA_bar := self.children.add(tmp_element);

  tmp_element := DInterfaceElement.create(self);
  tmp_bar := DStatBarImage.create(tmp_element);
  tmp_bar.Load(HealthBarImage);
  tmp_bar.Style := sbConcentration;
  tmp_bar.color := Vector4Single(0,1,1,1);
  tmp_element.Content := tmp_bar;
  tmp_element.frame := SimpleFrame;
  CNC_bar := self.children.add(tmp_element);

  tmp_element := DInterfaceElement.create(self);
  tmp_bar := DStatBarImage.create(tmp_element);
  tmp_bar.Load(HealthBarImage);
  tmp_bar.Style := sbMetaphysics;
  tmp_bar.color := Vector4Single(1,0,1,1);
  tmp_element.Content := tmp_bar;
  tmp_element.frame := SimpleFrame;
  MPH_bar := self.children.add(tmp_element);
end;

{-----------------------------------------------------------------------------}

procedure DPlayerBars.settarget(value: DActor);
begin
  if ftarget<>value then begin
    ftarget := value;
    //and drop the target to all children
    (children[HP_bar].Content as DStatBarImage).Target := ftarget;
    (children[STA_bar].Content as DStatBarImage).Target := ftarget;
    (children[CNC_bar].Content as DStatBarImage).Target := ftarget;
    (children[MPH_bar].Content as DStatBarImage).Target := ftarget;
  end;
end;

{-----------------------------------------------------------------------------}

procedure DPlayerBars.ArrangeChildren;
begin
  if not base.initialized then begin
    writeLnLog('DPlayerBars.ArrangeChildren','ERROR: Base is not initialized!');
    exit;
  end;
  //...
  if ftarget.maxmaxmph>0 then begin
  //...
  end else begin
  //...
  end;
end;

end.

