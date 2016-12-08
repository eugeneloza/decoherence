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
  decointerface, decoactor, decoplayercharacter, decoperks,
  decoglobal;


type
  { wrapper for composite Interface elements with ArrangeChildren procedure
    each child will define its own arrange method }
  DAbstractCompositeInterfaceElement = class(DInterfaceElement)
  public
    { arranges children within base.w, base.h of this container }
    procedure ArrangeChildren(animate: boolean); virtual; abstract;
    { additionally calls ArrangeChildren }
    procedure setbasesize(const newx,newy,neww,newh,newo: float; animate: boolean); override;
end;

type
  {HP,STA,CNC and MPH vertical bars for a player character}
  DPlayerBars = class(DAbstractCompositeInterfaceElement)
  private
    {these are links for easier access to the children}
    HP_bar, STA_bar, CNC_bar, MPH_bar: DSingleInterfaceElement;
    {target character}
    ftarget: DActor;
    procedure settarget(value: DActor);
  public
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
    {Player character which portrait is displayed}
    property Target: DPlayerCharacter read ftarget write settarget;
    constructor create(AOwner: TComponent); override;
  end;


//todo: float label is identical except pinteger -> pfloat
type
  { a simple editor for an integer variable featuring plus and minus
    buttons }
  DIntegerEdit = class(DAbstractCompositeInterfaceElement)
  private
    {sub-elements nicknames for easy access}
    iLabel: DSingleInterfaceElement;
    PlusButton, MinusButton: DSingleInterfaceElement;

    fTarget: PInteger;
    procedure settarget(value: PInteger);
  public
    {integer to change}
    property Target: Pinteger read ftarget write settarget;
    constructor create(AOwner: TComponent); override;
    procedure ArrangeChildren(animate: boolean); override;
  end;

  {integer with "bonus" edit}

type DPerksContainer = class(DAbstractCompositeInterfaceElement)
  {container for buffs-debuffs, perks and actions}
  private
    fTarget: DPlayerCharacter;
    procedure settarget(value: DPlayerCharacter);
  public
    property Target: DPlayerCharacter read ftarget write settarget;
    procedure MakePerksList;
    procedure ArrangeChildren(animate: boolean); override;
    //procedure UpdatePerksList;
  end;




var HealthBarImage: TCastleImage; //todo not freed automatically!!!
    PortraitIMG: TCastleImage; //todo!!!

    decorationframe1_left,decorationframe1_right,
    decorationframe2_left,decorationframe2_right,
    decorationframe2_bottomleft,decorationframe2_bottomright: DFrame;


{reads some interface-related data, like loading health bars images}
procedure InitCompositeInterface;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

implementation
uses CastleLog, CastleFilesUtils, castleVectors,
  decogui, decoimages, decolabels;


procedure InitCompositeInterface;
begin
  HealthBarImage := LoadImage(ApplicationData(ProgressBar_folder+'verticalbar_by_Alexey.png'));
  PortraitIMG := LoadImage(ApplicationData(PortraitFolder+'portrait_tmp.jpg'));

  decorationframe1_left := DFrame.create(Window);
  with decorationframe1_left do begin
    SourceImage := LoadImage(ApplicationData(DecorationsFolder+'frame_1_left_by_Alexey.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 3; CornerBottom := 23; cornerLeft := 2; CornerRight := 6;
  end;
  decorationframe1_right := DFrame.create(Window);
  with decorationframe1_right do begin
    SourceImage := LoadImage(ApplicationData(DecorationsFolder+'frame_1_right_by_Alexey.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 3; CornerBottom := 23; cornerLeft := 6; CornerRight := 2;
  end;
  decorationframe2_left := DFrame.create(Window);
  with decorationframe2_left do begin
    SourceImage := LoadImage(ApplicationData(DecorationsFolder+'frame_2_left_by_Alexey.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 6; CornerBottom := 9; cornerLeft := 2; CornerRight := 6;
    //cornerTop := 0; CornerBottom := 0; cornerLeft := 0; CornerRight := 0;
  end;
  decorationframe2_right := DFrame.create(Window);
  with decorationframe2_right do begin
    SourceImage := LoadImage(ApplicationData(DecorationsFolder+'frame_2_right_by_Alexey.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 6; CornerBottom := 9; cornerLeft := 6; CornerRight := 2;
  end;
  decorationframe2_bottomleft := DFrame.create(Window);
  with decorationframe2_bottomleft do begin
    SourceImage := LoadImage(ApplicationData(DecorationsFolder+'frame_2_bottomleft_by_Alexey.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 6; CornerBottom := 2; cornerLeft := 0; CornerRight := 9;
  end;
  decorationframe2_bottomright := DFrame.create(Window);
  with decorationframe2_bottomright do begin
    SourceImage := LoadImage(ApplicationData(DecorationsFolder+'frame_2_bottomright_by_Alexey.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 6; CornerBottom := 2; cornerLeft := 9; CornerRight := 0;
  end;

end;

{===========================================================================}

procedure DAbstractCompositeInterfaceElement.setbasesize(const newx,newy,neww,newh,newo: float; animate: boolean);
begin
  inherited setbasesize(newx,newy,neww,newh,newo,animate);
  ArrangeChildren(animate);
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
  grab(tmp_element);

  tmp_element := DSingleInterfaceElement.create(self);
  tmp_bar := DStatBarImage.create(tmp_element);
  tmp_bar.Load(HealthBarImage);
  tmp_bar.Style := sbStamina;
  tmp_bar.Kind := bsVertical;
  tmp_element.Content := tmp_bar;
  tmp_element.frame := SimpleFrame;
  STA_bar := tmp_element;
  grab(tmp_element);

  tmp_element := DSingleInterfaceElement.create(self);
  tmp_bar := DStatBarImage.create(tmp_element);
  tmp_bar.Load(HealthBarImage);
  tmp_bar.Style := sbConcentration;
  tmp_bar.Kind := bsVertical;
  tmp_element.Content := tmp_bar;
  tmp_element.frame := SimpleFrame;
  CNC_bar := tmp_element;
  grab(tmp_element);

  tmp_element := DSingleInterfaceElement.create(self);
  tmp_bar := DStatBarImage.create(tmp_element);
  tmp_bar.Load(HealthBarImage);
  tmp_bar.Style := sbMetaphysics;
  tmp_bar.Kind := bsVertical;
  tmp_element.Content := tmp_bar;
  tmp_element.frame := SimpleFrame;
  MPH_bar := tmp_element;
  grab(tmp_element);
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
  MPH_bar.setbasesize(base.fx+3*scalex,base.fy,scalex,base.fh,base.opacity,animate);
  if ftarget.maxmaxmph > 0 then
    MPH_bar.visible := true
  else
    MPH_bar.visible := false
end;

{=============================================================================}
{========================== Character portrait ===============================}
{=============================================================================}

procedure DPortrait.settarget(value: DPlayerCharacter);
begin
  if ftarget <> value then begin
    ftarget := value;
    (content as DStaticImage).freeImage;
    WriteLnLog('DPortrait.settarget','Load from portrait');
    (content as DStaticImage).Load(PortraitIMG);  //todo
  end;
end;

constructor DPortrait.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  content := DStaticImage.create(self);
  frame := BlackFrame;
end;

{=============================================================================}
{============================== Integer edit =================================}
{=============================================================================}

procedure incTarget(Sender: DAbstractElement; x,y: integer);
begin
  {hmm... looks complex...}
  if sender is DSingleInterfaceElement then  //fool's check
    if (sender as DSingleInterfaceElement).parent is DIntegerEdit then    //another fool's check :)
      inc(((sender as DSingleInterfaceElement).parent as DIntegerEdit).Target^); //todo!!!!!!!!!
end;

constructor DIntegerEdit.create(AOwner: TComponent);
var tmp: DIntegerLabel;
    tmpimg: DStaticImage;
begin
  inherited create(AOwner);
  ilabel := DSingleInterfaceElement.create(self);
  tmp := DIntegerLabel.create(ilabel);
  ilabel.content := tmp;
  ilabel.frame := simpleframe;
  grab(ilabel);

  PlusButton := DSingleInterfaceElement.create(self);
  //PlusButton.parent
  PlusButton.CanMouseOver := true;
  PlusButton.OnMousePress := @IncTarget;
  tmpImg := DStaticImage.create(PlusButton);
  //tmpImg.LoadThread('');
  PlusButton.content := tmpImg;
  grab(PlusButton);

  MinusButton := DSingleInterfaceElement.create(self);
  tmpImg := DStaticImage.create(MinusButton);
  //tmpImg.LoadThread('');
  MinusButton.content := tmpImg;
  grab(MinusButton);
end;

procedure DIntegerEdit.settarget(value: pinteger);
begin
  if ftarget <> value then begin
    ftarget := value;
    (ilabel.content as DIntegerLabel).value := value;
    //reset button activity
  end;
end;

procedure DIntegerEdit.ArrangeChildren(animate: boolean);
begin
  //todo ***
end;

{=============================================================================}
{=========================== Perks container =================================}
{=============================================================================}

procedure DPerksContainer.settarget(value: DPlayerCharacter);
begin
  if ftarget <> value then begin
    ftarget := value;
    MakePerksList;
  end;
end;

procedure DPerksContainer.MakePerksList;
begin
  //todo ***
  ArrangeChildren(true);
end;

procedure DPerksContainer.ArrangeChildren(animate: boolean);
begin
  //todo ***
end;


end.

