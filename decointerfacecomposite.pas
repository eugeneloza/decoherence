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
  decointerface,
  decoactor, decoplayercharacter, decoperks,
  decoglobal;


type
  { wrapper for composite Interface elements with ArrangeChildren procedure
    each child will define its own arrange method }
  DAbstractCompositeInterfaceElement = class(DInterfaceElement)
  public
    { arranges children within base.w, base.h of this container
      basically at this level it is abstract, it just substracts the frame
      from base.w and base.h and does nothing more}
    var cnt_x,cnt_y,cnt_fw,cnt_fh: float;
    procedure ArrangeChildren(animate: TAnimationStyle); virtual;
    { additionally calls ArrangeChildren }
    procedure setbasesize(const newx,newy,neww,newh,newo: float; animate: TAnimationStyle); override;
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
    procedure ArrangeChildren(animate: TAnimationStyle); override;
end;

type
  {Nickname + PlayerBars + NumericHP}
  DPlayerBarsFull = class(DAbstractCompositeInterfaceElement) //not a child of DPlayerBars!
  private
    {these are links for easier access to the children}
    PartyBars: DPlayerBars;
    NumHealth: DSingleInterfaceElement;
    NickName: DSingleInterfaceElement;
    {target character}
    ftarget: DActor;
    procedure settarget(value: DActor);
  public
    {the character being monitored}
    property Target: DActor read ftarget write settarget;
    constructor create(AOwner: TComponent); override;
    procedure ArrangeChildren(animate: TAnimationStyle); override;
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
    procedure ArrangeChildren(animate: TAnimationStyle); override;
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
    procedure ArrangeChildren(animate: TAnimationStyle); override;
    //procedure UpdatePerksList;
  end;




var HealthBarImage: TCastleImage; //todo not freed automatically!!!
    PortraitIMG: TCastleImage; //todo!!!

    characterbar_top, characterbar_mid, characterbar_bottom,
    portraitframe_left, portraitframe_right,
    decorationframe1_left,decorationframe1_right,
    decorationframe2_left,decorationframe2_right,
    decorationframe2_bottomleft,decorationframe2_bottomright,
    decorationframe3_bottom
                                                           : DFrame;


{reads some interface-related data, like loading health bars images}
procedure InitCompositeInterface;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

implementation
uses CastleLog, CastleFilesUtils, castleVectors,
  decogui, decoimages, decolabels;


procedure InitCompositeInterface;
begin
  HealthBarImage := LoadImage(ApplicationData(ProgressBarFolder+'verticalbar_CC-BY-SA_by_Saito00.png'));
  PortraitIMG := LoadImage(ApplicationData(PortraitFolder+'portrait_tmp.jpg'));

  {load artwork by Saito00}

  portraitframe_left := DFrame.create(Window);
  with portraitframe_left do begin
    SourceImage := LoadImage(ApplicationData(FramesFolder+'frameborder_left_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 4; CornerBottom := 4; cornerLeft := 3; CornerRight := 4;
  end;
  portraitframe_right := DFrame.create(Window);
  with portraitframe_right do begin
    SourceImage := LoadImage(ApplicationData(FramesFolder+'frameborder_right_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 4; CornerBottom := 4; cornerLeft := 4; CornerRight := 3;
  end;

  decorationframe1_left := DFrame.create(Window);
  with decorationframe1_left do begin
    SourceImage := LoadImage(ApplicationData(DecorationsFolder+'frame_1_left_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 3; CornerBottom := 23; cornerLeft := 2; CornerRight := 6;
  end;
  decorationframe1_right := DFrame.create(Window);
  with decorationframe1_right do begin
    SourceImage := LoadImage(ApplicationData(DecorationsFolder+'frame_1_right_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 3; CornerBottom := 23; cornerLeft := 6; CornerRight := 2;
  end;
  decorationframe2_left := DFrame.create(Window);
  with decorationframe2_left do begin
    SourceImage := LoadImage(ApplicationData(DecorationsFolder+'frame_2_left_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 6; CornerBottom := 9; cornerLeft := 2; CornerRight := 6;
  end;
  decorationframe2_right := DFrame.create(Window);
  with decorationframe2_right do begin
    SourceImage := LoadImage(ApplicationData(DecorationsFolder+'frame_2_right_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 6; CornerBottom := 9; cornerLeft := 6; CornerRight := 2;
  end;
  decorationframe2_bottomleft := DFrame.create(Window);
  with decorationframe2_bottomleft do begin
    SourceImage := LoadImage(ApplicationData(DecorationsFolder+'frame_2_bottomleft_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 6; CornerBottom := 2; cornerLeft := 0; CornerRight := 9;
  end;
  decorationframe2_bottomright := DFrame.create(Window);
  with decorationframe2_bottomright do begin
    SourceImage := LoadImage(ApplicationData(DecorationsFolder+'frame_2_bottomright_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 6; CornerBottom := 2; cornerLeft := 9; CornerRight := 0;
  end;
  decorationframe3_bottom := DFrame.create(Window);
  with decorationframe3_bottom do begin
    SourceImage := LoadImage(ApplicationData(DecorationsFolder+'frame_3_bottom_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 10; CornerBottom := 9; cornerLeft := 23; CornerRight := 23;
  end;

  characterbar_top := DFrame.create(Window);
  with characterbar_top do begin
    SourceImage := LoadImage(ApplicationData(FramesFolder+'character_bar_top_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 5; CornerBottom := 5; cornerLeft := 4; CornerRight := 4;
  end;
  characterbar_mid := DFrame.create(Window);
  with characterbar_mid do begin
    SourceImage := LoadImage(ApplicationData(FramesFolder+'character_bar_mid_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 0; CornerBottom := 0; cornerLeft := 4; CornerRight := 4;
  end;
  characterbar_bottom := DFrame.create(Window);
  with characterbar_bottom do begin
    SourceImage := LoadImage(ApplicationData(FramesFolder+'character_bar_bottom_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 5; CornerBottom := 5; cornerLeft := 4; CornerRight := 4;
  end;

end;

{===========================================================================}

procedure DAbstractCompositeInterfaceElement.setbasesize(const newx,newy,neww,newh,newo: float; animate: TAnimationStyle);
begin
  inherited setbasesize(newx,newy,neww,newh,newo,animate);
  ArrangeChildren(animate);
end;

procedure DAbstractCompositeInterfaceElement.ArrangeChildren(animate: TAnimationStyle);
begin
  cnt_x := base.fx;
  cnt_y := base.fy;
  cnt_fw := base.fw;
  cnt_fh := base.fh;
  if (frame <> nil) and (frame.Rectagonal) then begin
    cnt_x += frame.cornerLeft/Window.Height;
    cnt_y += frame.cornerBottom/Window.height;
    cnt_fw -= (frame.cornerLeft+frame.cornerRight)/Window.Height;
    cnt_fh -= (frame.cornerTop+frame.cornerBottom)/Window.Height;
  end;



end;

{===========================================================================}
{=================== player bars ===========================================}
{===========================================================================}

constructor DPlayerBars.create(AOwner: TComponent);
var tmp_bar: DStatBarImage;
    //tmp_element: DSingleInterfaceElement;
begin
  inherited create(AOwner);
  frame := BlackFrame;

  //or make parent nil? as they are freed by freeing children? Keep an eye out for troubles...
  HP_bar := DSingleInterfaceElement.create(self);
  tmp_bar := DStatBarImage.create(HP_bar);
  tmp_bar.Load(HealthBarImage);
  tmp_bar.Style := sbHealth;
  tmp_bar.Kind := bsVertical;
  HP_bar.Content := tmp_bar;
  //HP_bar.frame := SimpleFrame;
  grab(HP_bar);

  STA_bar := DSingleInterfaceElement.create(self);
  tmp_bar := DStatBarImage.create(STA_bar);
  tmp_bar.Load(HealthBarImage);
  tmp_bar.Style := sbStamina;
  tmp_bar.Kind := bsVertical;
  STA_bar.Content := tmp_bar;
  //STA_bar.frame := SimpleFrame;
  grab(STA_bar);

  CNC_bar := DSingleInterfaceElement.create(self);
  tmp_bar := DStatBarImage.create(CNC_bar);
  tmp_bar.Load(HealthBarImage);
  tmp_bar.Style := sbConcentration;
  tmp_bar.Kind := bsVertical;
  CNC_bar.Content := tmp_bar;
  //STA_bar.frame := SimpleFrame;
  grab(CNC_bar);

  MPH_bar := DSingleInterfaceElement.create(self);
  tmp_bar := DStatBarImage.create(MPH_bar);
  tmp_bar.Load(HealthBarImage);
  tmp_bar.Style := sbMetaphysics;
  tmp_bar.Kind := bsVertical;
  MPH_bar.Content := tmp_bar;
  //MPH_bar.frame := SimpleFrame;
  grab(MPH_bar);
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

{---------------------------------------------------------------------------}

procedure DPlayerBars.ArrangeChildren(animate: TAnimationStyle);
var scalex: float;
begin
  inherited ArrangeChildren(animate);

  if not base.initialized then begin
    writeLnLog('DPlayerBars.ArrangeChildren','ERROR: Base is not initialized!');
    exit;
  end;

  {metaphysics bar is displayed only if the character is known to posess metaphysic skills}
  if ftarget.maxmaxmph > 0 then scalex := cnt_fw/4 else scalex := cnt_fw/3;
  HP_bar. setbasesize(cnt_x         , cnt_y, scalex, cnt_fh, base.opacity, animate);
  STA_bar.setbasesize(cnt_x+  scalex, cnt_y, scalex, cnt_fh, base.opacity, animate);
  CNC_bar.setbasesize(cnt_x+2*scalex, cnt_y, scalex, cnt_fh, base.opacity, animate);
  MPH_bar.setbasesize(cnt_x+3*scalex, cnt_y, scalex, cnt_fh, base.opacity, animate);
  if ftarget.maxmaxmph > 0 then
    MPH_bar.visible := true
  else
    MPH_bar.visible := false
end;

{=============================================================================}
{================ Player bars with nickname and profession ===================}
{=============================================================================}

procedure DPlayerBarsFull.settarget(value: DActor);
begin
  if ftarget <> value then begin
    ftarget := value;
    //and copy the target to all children
    PartyBars.Target := value;
    (NumHealth.content as DFloatLabel).Value := @value.HP;
    (NickName.content as DStringLabel).Value := @value.nickname;
  end;
end;

constructor DPlayerBarsFull.create(AOwner:TComponent);
var tmp_flt: DFloatLabel;
    tmp_str: DStringLabel;
begin
  inherited create(AOwner);
  PartyBars := DPlayerBars.create(self);
  NumHealth := DSingleInterfaceElement.create(self);
  NickName  := DSingleInterfaceElement.create(self);

  PartyBars.frame := characterbar_mid;
  NickName.frame  := characterbar_top;
  NumHealth.frame := characterbar_bottom;

  tmp_flt := DFloatLabel.create(NumHealth);
  tmp_flt.Digits := 0;
  tmp_flt.ScaleLabel := true;
  NumHealth.Content := tmp_flt;
  tmp_str := DStringLabel.create(NickName);
  tmp_str.ScaleLabel := true;
  NickName.content := tmp_str;

  grab(PartyBars);
  grab(NumHealth);
  grab(NickName);
end;

Procedure DPlayerBarsFull.ArrangeChildren(animate: TAnimationStyle);
var labelspace: float;
begin
  inherited ArrangeChildren(animate);
  {********** INTERFACE DESIGN BY Saito00 ******************}

  labelspace := 23/800;
  NickName. setbasesize(cnt_x, cnt_y+cnt_fh-labelspace, cnt_fw, labelspace, base.opacity, animate);
  PartyBars.setbasesize(cnt_x, cnt_y+labelspace       , cnt_fw, cnt_fh-2*labelspace, base.opacity, animate);
  NumHealth.setbasesize(cnt_x, cnt_y                  , cnt_fw, labelspace, base.opacity, animate);
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

procedure DIntegerEdit.ArrangeChildren(animate: TAnimationStyle);
begin
  inherited ArrangeChildren(animate);
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
  ArrangeChildren(asNone);
end;

procedure DPerksContainer.ArrangeChildren(animate: TAnimationStyle);
begin
  inherited ArrangeChildren(animate);
  //todo ***
end;


end.

