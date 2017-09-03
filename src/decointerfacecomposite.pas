{Copyright (C) 2012-2017 Yevhen Loza, Saito00

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

{ Describes groups of items containing several specific interface elements
  used in different game situations. These interface elements usually preform
  some specific function, i.e. describe specific data management elements
  in cotrast to more abstract "interface elements" which describe how elements
  are organized and displayed}
unit DecoInterfaceComposite;

{$INCLUDE compilerconfig.inc}

interface

uses
  CastleImages,
  DecoInterface, DecoImages, DecoLabels,
  DecoActor, DecoPlayerCharacter, DecoPerks,
  DecoGlobal;

const PortraitTimeOut = 1; {seconds}

type
  {}
  DWindElement = class(DInterfaceElement)
  private
    {}
    Wind1, Wind2: DWindImage;
  public
    constructor Create; override;
  end;

type
  {}
  DCompositeElement = class(DInterfaceElement)
  public
    {}
    procedure ArrangeChildren; virtual; abstract;
  end;

type
  { An element or an element group surrounded by a rectagonal frame
    Warning: changing frame.frame won't anchor content to the frame!
    Should Anchor it manually }
  DFramedElement = class(DCompositeElement)
  public
    {}
    Frame: DFrameImage;
    procedure ArrangeChildren; override;
    constructor Create; override;
    constructor Create(const aFrame: DRectagonalFrame);
  end;

type
  { ... }
  DFramedImage = class(DFramedElement)
  public
    {}
    Image: DStaticImage;
    procedure ArrangeChildren; override;
    constructor Create; override;
    constructor Create(const aImage: TCastleImage; const aFrame: DRectagonalFrame);
  end;


type
  {}
  DFramedBar = class(DFramedElement)
  private
    fTarget: DBasicActor;
    fStyle: TStatBarStyle;
    procedure SetTarget(Value: DBasicActor);
    procedure SetStyle(Value: TStatBarStyle);
  public
    {}
    Bar: DStatBarImage;
    {}
    property Target: DBasicActor read fTarget write SetTarget;
    {}
    property Style: TStatBarStyle read fStyle write SetStyle;
    procedure ArrangeChildren; override;
    constructor Create; override;
  end;

type
  { HP,STA,CNC and MPH vertical bars for a player character }
  DStatBars = class(DFramedElement)
  private
    {these are links for easier access to the children}
    HP_bar, STA_bar, CNC_bar, MPH_bar: DFramedBar;
    {target character}
    fTarget: DBasicActor; //we don't need anything "higher" than this
    procedure SetTarget(Value: DBasicActor);
  public
    {the character being monitored}
    procedure ArrangeChildren; override;
    property Target: DBasicActor read fTarget write SetTarget;
    constructor Create; override;
end;

type
  {Nickname + PlayerBars + NumericHP}
  DPlayerBarsFull = class(DInterfaceElement) //not a child of DPlayerBars!
  private
    {these are links for easier access to the children}
 {   PartyBars: DPlayerBars;
    NumHealth: DSingleInterfaceElement;
    NickName: DSingleInterfaceElement;
    {target character}
    fTarget: DActor;
    procedure SetTarget(Value: DActor);
  public
    {the character being monitored}
    property Target: DActor read fTarget write SetTarget;
    constructor Create(AOwner: TComponent); override;
    procedure ArrangeChildren(Animate: TAnimationStyle); override; }
  end;

type
  { character portrait. Some day it might be replaced for TUIContainer of
    the 3d character face :) Only 2D inanimated image for now... }
  DPortrait = class(DFramedElement)
  public
    {this is a BUG, but I won't fix it now, it requires remaking ALL THE INTERFACE}
  {  DamageOverlay: DSingleInterfaceElement;
    DamageLabel: DSingleInterfaceElement;
  private
    fTarget: DPlayerCharacter;
    procedure SetTarget(value: DPlayerCharacter);
  public
    {Player character which portrait is displayed}
    property Target: DPlayerCharacter read ftarget write settarget;
    constructor Create(AOwner: TComponent); override;
    procedure doHit(Dam: float; DamType: TDamageType);}
  end;

{
//todo: float label is identical except pointeger -> pfloat
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
    procedure incTarget(Sender: DAbstractElement; x,y: integer);
    procedure decTarget(Sender: DAbstractElement; x,y: integer);
  end;
}
//  {integer with "bonus" edit}

type
  {A displayer for a single perk}
  DPerkInterfaceItem =  class (DFramedElement)
  private
 {   PerkImage: DSingleInterfaceElement; {animated image}
    fTarget: DPerk;
    //fCharacter: DPlayerCharacter;
    procedure settarget(value: DPerk);
    //procedure setcharacter(value: DPlayerCharacter);
  public
    property Target: DPerk read ftarget write settarget;
    //property Character: DPlayerCharacter read fCharacter write setcharacter;
    {proedure getSelectedStatus -----> update}
    constructor create(AOwner: TComponent); override; }
end;

type
 {sorts in n rows and m lines the list of interface elements within self.base. Without specific data management and sorting parameters it is abstract and should parent DPerkSorter and DItemSorter}
 DAbstractSorter = class(DCompositeElement)
   private
  {
   public
     lines{,rows}: integer;
     procedure ArrangeChildren(animate: TAnimationStyle); override;
     constructor create(AOwner: TComponent); override; }
   end;

//type TPerkContainerStyle = (pcActions,pcActive,pcGlobal);

type DPerksContainer = class(DAbstractSorter)
  {container for buffs-debuffs, perks and actions}
  private
   { fTarget: DPlayerCharacter;
    procedure settarget(value: DPlayerCharacter);
  public
    //ContainerStyle: TPerkContainerStyle;
    property Target: DPlayerCharacter read ftarget write settarget;
    procedure MakePerksList(animate: TAnimationStyle);
    //procedure UpdatePerksList;  }
  end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

implementation
uses SysUtils, CastleLog, {castleVectors,}
   //DecoFont,
   //DecoInterfaceBlocks,
   DecoInputOutput, DecoInterfaceLoader;

{===========================================================================}
{====================== D Wind Element =====================================}
{===========================================================================}

constructor DWindElement.Create;
begin
  inherited Create;
  Base.AnchorToWindow := true;
  SetBaseSize(0,0,1,1);

  Wind1 := DWindImage.Create;
  Wind2 := DWindImage.Create;
  Grab(Wind1);
  Grab(Wind2);

  Wind1.Load(WindImage1);
  Wind2.Load(WindImage2);
  Wind1.PhaseSpeed := 1/(15+drnd.Random);
  Wind2.PhaseSpeed := 1/(10+drnd.Random);

  Rescale;
end;

{===========================================================================}
{======================== D Framed Element =================================}
{===========================================================================}

constructor DFramedElement.Create;
begin
  inherited Create;
  Frame := DFrameImage.Create;
  Grab(Frame);
  ArrangeChildren;
end;
constructor DFramedElement.Create(const aFrame: DRectagonalFrame);
begin
  Create;
  Frame.Frame := aFrame;
end;

{-----------------------------------------------------------------------------}

procedure DFramedElement.ArrangeChildren;
begin
  Frame.Base.AnchorTo(Self.Current);
  Frame.SetBaseSize(0,0,1,1);
end;

{-----------------------------------------------------------------------------}

constructor DFramedImage.Create;
begin
  inherited Create;
  Image := DStaticImage.Create;
  Grab(Image);
  ArrangeChildren;
end;
constructor DFramedImage.Create(const aImage: TCastleImage; const aFrame: DRectagonalFrame);
begin
  inherited Create
  Frame.Frame := aFrame;
  Image := DStaticImage.Create;
  Image.Load(aImage);
  Grab(Image);
  ArrangeChildren;
end;

{-----------------------------------------------------------------------------}

procedure DFramedElement.ArrangeChildren;
begin
  inherited ArrangeChildren;
  Image.Base.AnchorToFrame(Frame);
  Image.SetBaseSize(0,0,1,1);
end;

{===========================================================================}
{===================== Framed bar ==========================================}
{===========================================================================}

procedure DFramedBar.SetTarget(Value: DBasicActor);
begin
  if fTarget<>Value then begin
    fTarget := Value;
    Bar.Target := Value;
  end;
end;

{-----------------------------------------------------------------------------}

procedure DFramedBar.SetStyle(Value: TStatBarStyle);
begin
  if fStyle<>Value then begin
    fStyle := Value;
    Bar.Style := Value;
  end;
end;

{-----------------------------------------------------------------------------}

constructor DFramedBar.Create;
begin
  inherited Create;
  Frame.Frame := StatBarsFrame;
  Bar := DStatBarImage.Create;
  Grab(Bar);
  ArrangeChildren;
end;

{-----------------------------------------------------------------------------}

procedure DFramedBar.ArrangeChildren;
begin
  inherited ArrangeChildren;
  Bar.Base.AnchorToFrame(Frame);
  Bar.SetBaseSize(0,0,1,1);
end;

{===========================================================================}
{===================== Stat bars ===========================================}
{===========================================================================}

constructor DStatBars.Create;
begin
  inherited Create;

  Frame.Frame := BlackFrame;

  HP_bar := DFramedBar.Create;
  HP_bar.Bar.Load(HpBarImage);
  HP_bar.Style := sbHealth;
  HP_bar.Bar.Kind := bsVertical;
  Grab(HP_bar);

  STA_bar := DFramedBar.Create;
  STA_bar.Bar.Load(StaBarImage);
  STA_bar.Style := sbStamina;
  STA_bar.Bar.Kind := bsVertical;
  Grab(STA_bar);

  CNC_bar := DFramedBar.Create;
  CNC_bar.Bar.Load(CncBarImage);
  CNC_bar.Style := sbConcentration;
  CNC_bar.Bar.Kind := bsVertical;
  Grab(CNC_bar);

  MPH_bar := DFramedBar.Create;
  MPH_bar.Bar.Load(MphBarImage);
  MPH_bar.Style := sbMetaphysics;
  MPH_bar.Bar.Kind := bsVertical;
  Grab(MPH_bar);

  ArrangeChildren;

  Rescale;
end;

{-----------------------------------------------------------------------------}

procedure DStatBars.ArrangeChildren;
begin
  HP_bar.Base.AnchorToFrame(Frame);
  STA_bar.Base.AnchorToFrame(Frame);
  CNC_bar.Base.AnchorToFrame(Frame);
  MPH_bar.Base.AnchorToFrame(Frame);

end;

{-----------------------------------------------------------------------------}

procedure DStatBars.Settarget(Value: DBasicActor);
begin
  if fTarget <> Value then begin
    fTarget := Value;
    //and copy the target to all children
    HP_bar.Target := fTarget;
    STA_bar.Target := fTarget;
    CNC_bar.Target := fTarget;
    MPH_bar.Target := fTarget;
  end;
end;

{---------------------------------------------------------------------------}

{procedure DPlayerBars.ArrangeChildren(animate: TAnimationStyle);
var scalex: float;
begin
  inherited ArrangeChildren(animate);

  if not base.initialized then begin
    writeLnLog('DPlayerBars.ArrangeChildren','ERROR: Base is not initialized!');
    exit;
  end;

  {metaphysics bar is displayed only if the character is known to posess metaphysic skills}
  if ftarget.maxmaxmph > 0 then scalex := cnt_w/4 else scalex := cnt_w/3;
  HP_bar. setbasesize(cnt_x         , cnt_y, scalex, cnt_h, base.opacity, animate);
  STA_bar.setbasesize(cnt_x+  scalex, cnt_y, scalex, cnt_h, base.opacity, animate);
  CNC_bar.setbasesize(cnt_x+2*scalex, cnt_y, scalex, cnt_h, base.opacity, animate);
  MPH_bar.setbasesize(cnt_x+3*scalex, cnt_y, scalex, cnt_h, base.opacity, animate);
  if ftarget.maxmaxmph > 0 then
    MPH_bar.visible := true
  else
    MPH_bar.visible := false
end;}

{=============================================================================}
{=============== Player bars with nickname and health label ==================}
{=============================================================================}

{procedure DPlayerBarsFull.settarget(value: DActor);
begin
  if fTarget <> Value then begin
    fTarget := Value;
    //and copy the target to all children
    PartyBars.Target := Value;
    DFloatLabel(NumHealth.Content).Value := @Value.HP;
    DStringLabel(NickName.Content).Value := @Value.nickname;
  end;
end;}

{---------------------------------------------------------------------------}

{constructor DPlayerBarsFull.Create(AOwner:TComponent);
var tmp_flt: DFloatLabel;
    tmp_str: DStringLabel;
begin
  inherited create(AOwner);
  PartyBars := DPlayerBars.Create(Self);
  NumHealth := DSingleInterfaceElement.Create(Self);
  NickName  := DSingleInterfaceElement.Create(Self);

  PartyBars.frame := Characterbar_mid;
  NickName.frame  := Characterbar_top;
  NumHealth.frame := Characterbar_bottom;

  tmp_flt := DFloatLabel.create(NumHealth);
  tmp_flt.Digits := 0;
  tmp_flt.ScaleLabel := false;
  tmp_flt.Font := CharHealthFont;
  NumHealth.Content := tmp_flt;
  tmp_str := DStringLabel.create(NickName);
  tmp_str.ScaleLabel := true;
  tmp_str.Font := CharNickNameFont;
  NickName.content := tmp_str;

  grab(PartyBars);
  grab(NumHealth);
  grab(NickName);
end;}

{---------------------------------------------------------------------------}

{Procedure DPlayerBarsFull.ArrangeChildren(animate: TAnimationStyle);
var labelspace: float;
begin
  inherited ArrangeChildren(animate);
  {********** INTERFACE DESIGN BY Saito00 ******************}

  labelspace := 23/800;
  NickName. setbasesize(cnt_x, cnt_y+cnt_h-labelspace , cnt_w, labelspace, base.opacity, animate);
  PartyBars.setbasesize(cnt_x, cnt_y+labelspace       , cnt_w, cnt_h-2*labelspace, base.opacity, animate);
  NumHealth.setbasesize(cnt_x, cnt_y                  , cnt_w, labelspace, base.opacity, animate);
end; }

{=============================================================================}
{========================== Character portrait ===============================}
{=============================================================================}

{procedure DPortrait.settarget(value: DPlayerCharacter);
begin
  if ftarget <> value then begin
    ftarget := value;
    DStaticImage(content).freeImage;
    WriteLnLog('DPortrait.settarget','Load from portrait');
    DStaticImage(content).Load(portrait_img[drnd.random(length(portrait_img))]);  //todo
    fTarget.onHit := @self.doHit;
  end;
end;}

{---------------------------------------------------------------------------}

{constructor DPortrait.create(AOwner: TComponent);
var tmp_staticimage: DStaticImage;
    tmp_label: DLabel;
begin
  inherited create(AOwner);
  content := DStaticImage.create(self);

  DamageOverlay := DSingleInterfaceElement.create(self);
  tmp_staticimage := DStaticImage.create(self);
  DamageOverlay.content := tmp_staticimage;
  Grab(DamageOverlay);
//  DamageOverlay.ScaleToParent := true;

  DamageLabel := DSingleInterfaceElement.create(self);
  tmp_label := DLabel.create(self);
  tmp_label.ScaleLabel := false;
  tmp_label.Font := PlayerDamageFont;
  DamageLabel.content := tmp_label;
  Grab(damageLabel);
//  DamageLabel.ScaleToParent := true;
end;}

{---------------------------------------------------------------------------}

{procedure DPortrait.doHit(dam: float; damtype: TDamageType);
begin
  DCharacterSpace(parent).doSlideIn;
  DCharacterSpace(parent).timer.settimeout(PortraitTimeOut);
  (damageOverlay.content as DStaticImage).FreeImage;
  case damtype of
    dtHealth: (damageOverlay.content as DStaticImage).Load(damageOverlay_img);
  end;
  DamageOverlay.Base.copyxywh(self.base);
  //damageOverlay.resetContentSize?
  DamageOverlay.Content.base.copyxywh(self.base);
  DamageOverlay.Content.AnimateTo(asFadeIn);
  DamageOverlay.Rescale;
  DamageOverlay.AnimateTo(asFadeIn);
  if Dam>0 then begin
    DamageLabel.Base.CopyXYWH(self.base);
    DamageLabel.Content.Base.CopyXYWH(self.base);
    DamageLabel.AnimateTo(asFadeIn);
    DamageLabel.Content.AnimateTo(asFadeIn);
    DLabel(damageLabel.content).Text := IntToStr(Round(Dam));
    DamageLabel.Rescale;
  end;
end;}

{=============================================================================}
{============================== Integer edit =================================}
{=============================================================================}

//range checking should be done externally by enabling/disabling the buttons.

{procedure DIntegerEdit.incTarget(Sender: DAbstractElement; x,y: integer);
begin
  //remake to self.ftarget! make min/max
  {hmm... looks complex...}
  if Sender is DSingleInterfaceElement then  //fool's check
    if DSingleInterfaceElement(Sender).Parent is DIntegerEdit then    //another fool's check :)
      Inc(DIntegerEdit(DSingleInterfaceElement(Sender).Parent).Target^); //todo!!!!!!!!!
end;}

{---------------------------------------------------------------------------}

{procedure DIntegerEdit.decTarget(Sender: DAbstractElement; x,y: integer);
begin
  //remake to self.ftarget! make min/max
  {hmm... looks complex...}
  if Sender is DSingleInterfaceElement then  //fool's check
    if DSingleInterfaceElement(Sender).Parent is DIntegerEdit then    //another fool's check :)
      Dec(DIntegerEdit(DSingleInterfaceElement(Sender).Parent).Target^); //todo!!!!!!!!!
end;}

{---------------------------------------------------------------------------}

{constructor DIntegerEdit.create(AOwner: TComponent);
var tmp: DIntegerLabel;
    tmpImg: DStaticImage;
begin
  inherited create(AOwner);
  iLabel := DSingleInterfaceElement.create(Self);
  tmp := DIntegerLabel.create(ilabel);
  iLabel.content := tmp;
  //ilabel.frame := simpleframe;
  Grab(iLabel);

  PlusButton := DSingleInterfaceElement.Create(Self);
  //PlusButton.parent
  PlusButton.CanMouseOver := true;
  PlusButton.OnMousePress := @IncTarget;
  tmpImg := DStaticImage.create(PlusButton);
  //tmpImg.LoadThread('');
  PlusButton.content := tmpImg;
  grab(PlusButton);

  MinusButton := DSingleInterfaceElement.create(Self);
  MinusButton.CanMouseOver := true;
  MinusButton.OnMousePress := @decTarget;
  tmpImg := DStaticImage.create(MinusButton);
  //tmpImg.LoadThread('');
  MinusButton.Content := tmpImg;
  Grab(MinusButton);
end;}

{---------------------------------------------------------------------------}

{procedure DIntegerEdit.SetTarget(Value: pInteger);
begin
  if fTarget <> Value then begin
    fTarget := Value;
    DIntegerLabel(iLabel.Content).Value := Value;
    //reset button activity
  end;
end;}

{---------------------------------------------------------------------------}

{procedure DIntegerEdit.ArrangeChildren(Animate: TAnimationStyle);
begin
  inherited ArrangeChildren(Animate);
  //todo ***
  PlusButton.SetBaseSize(cnt_x,cnt_y,cnt_h,cnt_h,1,Animate);
  MinusButton.SetBaseSize(cnt_x+cnt_w-cnt_h,cnt_y,cnt_h,cnt_h,1,Animate);
  iLabel.SetBaseSize(cnt_x+cnt_h,cnt_y,cnt_w-2*cnt_h,cnt_h,1,Animate);
end;}

{=============================================================================}
{=========================== Perks container =================================}
{=============================================================================}

{procedure DPerkInterfaceItem.settarget(value: DPerk);
begin
  if Value <> fTarget then begin
    fTarget := Value;
    (PerkImage.Content as DStaticImage).FreeImage;
    {$Warning todo}
    //(PerkImage.content as DStaticImage).load(fTarget.Image.SourceImage);
    //add events?
  end;
end;}

{---------------------------------------------------------------------------}

{constructor DPerkInterfaceItem.create(AOwner: TComponent);
var tmp: DStaticImage;
begin
  inherited create(AOwner);
  PerkImage := DSingleInterfaceElement.create(self);
  tmp := DStaticImage.create(self);
  PerkImage.Content := tmp;
  Grab(PerkImage);
end;}

{=============================================================================}
{=========================== Abstract sorter =================================}
{=============================================================================}

{procedure DAbstractSorter.ArrangeChildren(animate: TAnimationStyle);
var scale: float;
    i: integer;
begin
  inherited ArrangeChildren(animate);
  scale := cnt_h/lines;
  for i := 0 to children.Count-1 do children[i].setbasesize(cnt_x+i*scale,cnt_y,scale,scale,1,animate);
  //todo : auto scrollers
end;}

{---------------------------------------------------------------------}

{constructor DAbstractSorter.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Lines := 1;
end;}

{=============================================================================}
{=========================== Perks container =================================}
{=============================================================================}

{procedure DPerksContainer.SetTarget(Value: DPlayerCharacter);
begin
  if fTarget <> Value then begin
    fTarget := Value;
    MakePerksList(asNone);
  end;
end;}

{---------------------------------------------------------------------}

{procedure DPerksContainer.MakePerksList(animate: TAnimationStyle);
var tmp: DSingleInterfaceElement;
    i: integer;
begin
  if fTarget<>nil then begin
    if (fTarget.Actions<>nil) and (fTarget.Actions.count>0) then begin
      children.Clear;
      for i:= 0 to fTarget.Actions.count-1 do begin
        tmp := DSingleInterfaceElement.create(self);
        tmp.Content := DStaticImage.create(tmp);
        {$warning todo}
        //(tmp.Content as DSTaticImage).Load(fTarget.Actions[0].Image.SourceImage);
        grab(tmp);
      end;
      ArrangeChildren(animate);
    end else
      WriteLnLog('DPerksContainer.MakePerksList','ERROR: Target.Actions is empty!');
  end else
    WriteLnLog('DPerksContainer.MakePerksList','ERROR: Target is nil!');
end;}

{---------------------------------------------------------------------}

end.

