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
    procedure SpawnChildren; virtual; abstract;
    constructor Create; override;
  end;

type
  { An element or an element group surrounded by a rectagonal frame
    Warning: changing frame.frame won't anchor content to the frame!
    Should Anchor it manually }
  DFramedElement = class(DCompositeElement)
  private
    fFrame: DFrameImage;
    procedure SetFrame(const Value: DFrameImage);
  public
    {}
    property Frame: DFrameImage read fFrame write SetFrame;
    procedure ArrangeChildren; override;
    procedure SpawnChildren; override;
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
    procedure SpawnChildren; override;
    constructor Create; override;
    constructor Create(const aImage: TCastleImage; const aFrame: DRectagonalFrame);
  end;

type
  { ... }
  DHealthLabel = class(DFramedElement)
  private
    fTarget: DBasicActor; //we don't need anything "higher" than this
    procedure SetTarget(const Value: DBasicActor);
  public
    {}
    Lab: DFloatLabel;
    {the character being monitored}
    property Target: DBasicActor read fTarget write SetTarget;
    procedure ArrangeChildren; override;
    procedure SpawnChildren; override;
  end;

type
  { ... }
  DNameLabel = class(DFramedElement)
  private
    fTarget: DBasicActor; //we don't need anything "higher" than this
    procedure SetTarget(const Value: DBasicActor);
  public
    {}
    Lab: DStringLabel;
    {the character being monitored}
    property Target: DBasicActor read fTarget write SetTarget;
    procedure ArrangeChildren; override;
    procedure SpawnChildren; override;
  end;


type
  {}
  DFramedBar = class(DFramedElement)
  private
    fTarget: PStatValue;
    procedure SetTarget(const Value: PStatValue);
  public
    {}
    Bar: DStatBarImage;
    {}
    property Target: PStatValue read fTarget write SetTarget;
    procedure ArrangeChildren; override;
    procedure SpawnChildren; override;
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
    procedure SetTarget(const Value: DBasicActor);
  public
    procedure SpawnChildren; override;
    procedure ArrangeChildren; override;
    {the character being monitored}
    property Target: DBasicActor read fTarget write SetTarget;
    constructor Create; override;
end;

type
  {Nickname + PlayerBars + NumericHP}
  DPlayerBarsFull = class(DCompositeElement) //not a child of DPlayerBars!
  private
    {these are links for easier access to the children}
    PlayerBars: DStatBars;
    Health: DHealthLabel;
    NickName: DNameLabel;
    {target character}
    fTarget: DBasicActor;
    procedure SetTarget(const Value: DBasicActor);
  public
    {the character being monitored}
    property Target: DBasicActor read fTarget write SetTarget;
    procedure SpawnChildren; override;
    procedure ArrangeChildren; override;
  end;

type
  { character portrait. Some day it might be replaced for TUIContainer of
    the 3d character face :) Only 2D inanimated image for now... }
  DPortrait = class(DFramedElement)
  private
    {}
    Portrait: DPlayerPortrait;
    {}
    DamageOverlay: DStaticImage; //static at this moment
    {}
    DamageLabel: DDamageLabel;
  private
    fTarget: DPlayerCharacter;
    procedure SetTarget(const Value: DPlayerCharacter);
  public
    {Player character which portrait is displayed}
    property Target: DPlayerCharacter read fTarget write SetTarget;
    procedure doHit(const Dam: float; const DamType: TDamageType);
    procedure SpawnChildren; override;
    procedure ArrangeChildren; override;
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
   DecoFont,
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
{======================== CompositeElement =================================}
{===========================================================================}

constructor DCompositeElement.Create;
begin
  inherited Create;
  SpawnChildren;
  ArrangeChildren;
end;

{===========================================================================}
{======================== D Framed Element =================================}
{===========================================================================}

constructor DFramedElement.Create;
begin
  {emm... why can't I just omit Constructor?}
  inherited Create;
end;

constructor DFramedElement.Create(const aFrame: DRectagonalFrame);
begin
  Create;
  Frame.Frame := aFrame;
end;

{-----------------------------------------------------------------------------}

procedure DFramedElement.SetFrame(const Value: DFrameImage);
begin
  if fFrame<>Value then begin
    fFrame := Value;
    ArrangeChildren;
  end
end;

{-----------------------------------------------------------------------------}

procedure DFramedElement.SpawnChildren;
begin
  Frame := DFrameImage.Create;
  Grab(Frame);
end;

{-----------------------------------------------------------------------------}

procedure DFramedElement.ArrangeChildren;
begin
  Frame.Base.AnchorTo(Self.Current);
  Frame.SetBaseSize(0,0,1,1);
end;

{=============================================================================}

constructor DFramedImage.Create;
begin
  {emm... why can't I just omit Constructor?}
  inherited Create;
end;

constructor DFramedImage.Create(const aImage: TCastleImage; const aFrame: DRectagonalFrame);
begin
  inherited Create;
  Frame.Frame := aFrame;
  Image.Load(aImage);
end;

{-----------------------------------------------------------------------------}

procedure DFramedImage.SpawnChildren;
begin
  inherited SpawnChildren;
  Image := DStaticImage.Create;
  Grab(Image);
end;

{-----------------------------------------------------------------------------}

procedure DFramedImage.ArrangeChildren;
begin
  inherited ArrangeChildren;
  Image.Base.AnchorToFrame(Frame);
  Image.SetBaseSize(0,0,1,1);
end;

{=============================================================================}

procedure DHealthLabel.SpawnChildren;
begin
  inherited SpawnChildren;
  Lab := DFloatLabel.Create; //scale=false
  Lab.Digits := 0;
  Lab.Font := PlayerHealthFont;
  Grab(Lab);
  Frame.Frame := Characterbar_Bottom;
end;

{-----------------------------------------------------------------------------}

procedure DHealthLabel.ArrangeChildren;
begin
  inherited ArrangeChildren;
  Lab.Base.AnchorToFrame(Frame);
  Lab.SetBaseSize(0,0,1,1);
end;

{-----------------------------------------------------------------------------}

procedure DHealthLabel.SetTarget(const Value: DBasicActor);
begin
  if fTarget <> Value then begin
    fTarget := Value;
    Lab.Target := @Value.HP;
  end;
end;

{=============================================================================}

procedure DNameLabel.SpawnChildren;
begin
  inherited SpawnChildren;
  Lab := DStringLabel.Create;  //scale=false
  Lab.Font := PlayerNameFont;
  Grab(Lab);
  Frame.Frame := Characterbar_Top;
end;

{-----------------------------------------------------------------------------}

procedure DNameLabel.ArrangeChildren;
begin
  inherited ArrangeChildren;
  Lab.Base.AnchorToFrame(Frame);
  Lab.SetBaseSize(0,0,1,1);
end;

{-----------------------------------------------------------------------------}

procedure DNameLabel.SetTarget(const Value: DBasicActor);
begin
  if fTarget <> Value then begin
    fTarget := Value;
    Lab.Target := @Value.Nickname;
  end;
end;

{===========================================================================}
{===================== Framed bar ==========================================}
{===========================================================================}

procedure DFramedBar.SetTarget(const Value: PStatValue);
begin
  if fTarget <> Value then begin
    fTarget := Value;
    Bar.Target := Value;
  end;
end;

{-----------------------------------------------------------------------------}

constructor DFramedBar.Create;
begin
  inherited Create;
  Frame.Frame := StatBarsFrame;
end;

{-----------------------------------------------------------------------------}

procedure DFramedBar.SpawnChildren;
begin
  inherited SpawnChildren;
  Bar := DStatBarImage.Create;
  Grab(Bar);
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
  Frame.Frame := Characterbar_mid;
  Rescale; //<---------- UGLY
end;

{-----------------------------------------------------------------------------}

procedure DStatBars.SpawnChildren;
begin
  HP_bar := DFramedBar.Create;
  HP_bar.Bar.Load(HpBarImage);
  HP_bar.Bar.Kind := bsVertical;
  Grab(HP_bar);

  STA_bar := DFramedBar.Create;
  STA_bar.Bar.Load(StaBarImage);
  STA_bar.Bar.Kind := bsVertical;
  Grab(STA_bar);

  CNC_bar := DFramedBar.Create;
  CNC_bar.Bar.Load(CncBarImage);
  CNC_bar.Bar.Kind := bsVertical;
  Grab(CNC_bar);

  MPH_bar := DFramedBar.Create;
  MPH_bar.Bar.Load(MphBarImage);
  MPH_bar.Bar.Kind := bsVertical;
  Grab(MPH_bar);
end;

{-----------------------------------------------------------------------------}

procedure DStatBars.ArrangeChildren;
var ScaleX: float;
begin
  inherited ArrangeChildren;
  HP_bar. Base.AnchorToFrame(Frame);
  STA_bar.Base.AnchorToFrame(Frame);
  CNC_bar.Base.AnchorToFrame(Frame);
  MPH_bar.Base.AnchorToFrame(Frame);

  if fTarget.MaxMaxMPH > 0 then begin
    ScaleX := 1/4;
    MPH_bar.isVisible := true;
  end else begin
    ScaleX := 1/3;
    MPH_bar.isVisible := false;
  end;
  HP_bar. SetBaseSize(       0,0,ScaleX,1);
  STA_bar.SetBaseSize(  ScaleX,0,ScaleX,1);
  CNC_bar.SetBaseSize(2*ScaleX,0,ScaleX,1);
  MPH_bar.SetBaseSize(3*ScaleX,0,ScaleX,1);
end;

{-----------------------------------------------------------------------------}

procedure DStatBars.SetTarget(const Value: DBasicActor);
begin
  if fTarget <> Value then begin
    fTarget := Value;
    //and copy the target to all children
    HP_bar.Target := fTarget.HPRef;
    STA_bar.Target := fTarget.STARef;
    CNC_bar.Target := fTarget.CNCRef;
    MPH_bar.Target := fTarget.MPHRef;
    ArrangeChildren; //in case fTarget has different set of stats
  end;
end;

{=============================================================================}
{=============== Player bars with nickname and health label ==================}
{=============================================================================}

procedure DPlayerBarsFull.SetTarget(const Value: DBasicActor);
begin
  if fTarget <> Value then begin
    fTarget := Value;
    //and copy the target to all children
    PlayerBars.Target := Value;
    Health.Target  := Value;
    NickName.Target   := Value;
  end;
end;

{---------------------------------------------------------------------------}

procedure DPlayerBarsFull.SpawnChildren;
begin
  //inherited SpawnChildren;  <----- nothing to inherit
  PlayerBars := DStatBars.Create;
  Health := DHealthLabel.Create;
  NickName  := DNameLabel.Create;

  Grab(PlayerBars);
  Grab(Health);
  Grab(NickName);
end;

{---------------------------------------------------------------------------}

procedure DPlayerBarsFull.ArrangeChildren;
const LabelSpace = 23/800;
begin
  //inherited ArrangeChildren;  <------- nothing to inherit

  NickName.Base.AnchorTo(Self.Base); //AnchorToFrame(Frame);
  NickName.Base.Anchor[asBottom].AlignTo := noAlign;
  Health.Base.AnchorTo(Self.Base); //AnchorToFrame(Frame);
  Health.Base.Anchor[asTop].AlignTo := noAlign;
  PlayerBars.Base.AnchorTo(Self.Base); //AnchorToFrame(Frame);
  PlayerBars.Base.Anchor[asTop].Anchor := NickName.Base;
  PlayerBars.Base.Anchor[asBottom].Anchor := Health.Base;

  {********** INTERFACE DESIGN BY Saito00 ******************}

  {NickName.  setbasesize(0, cnt_y+cnt_h-labelspace , 1, labelspace);
  PlayerBars.setbasesize(0, cnt_y+labelspace       , 1, cnt_h-2*labelspace);
  NumHealth. setbasesize(0, cnt_y                  , 1, labelspace);
  }
end;

{=============================================================================}
{========================== Character portrait ===============================}
{=============================================================================}

procedure DPortrait.SetTarget(const Value: DPlayerCharacter);
begin
  if fTarget <> Value then begin
    fTarget := value;
    //DStaticImage(content).FreeImage;
    WriteLnLog('DPortrait.SetTarget','Load from portrait');
    //DStaticImage(content).Load(Portrait_img[drnd.Random(Length(Portrait_img))]);  //todo
    fTarget.onHit := @Self.doHit;
    ArrangeChildren;
  end;
end;

{---------------------------------------------------------------------------}

procedure DPortrait.SpawnChildren;
begin
  inherited SpawnChildren;
  Portrait:= DPlayerPortrait.Create;
  Grab(Portrait);
  DamageOverlay:= DStaticImage.Create;
  Grab(DamageOverlay);
  DamageLabel:= DDamageLabel.Create;
  Grab(DamageLabel);
end;

{---------------------------------------------------------------------------}

procedure DPortrait.ArrangeChildren;
begin
  inherited ArrangeChildren;
  //todo
end;

{---------------------------------------------------------------------------}

procedure DPortrait.doHit(const dam: float; const damtype: TDamageType);
begin
  {DCharacterSpace(parent).doSlideIn;
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
  end;}
end;

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

