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
  DecoInterfaceCore, DecoImages, DecoLabels,
  DecoActor, DecoPlayerCharacter, DecoPerks,
  DecoGlobal;

const PortraitTimeOut = 1; {seconds}

type
  { Draws two cycling wind images overlaying each other, moving with different speed }
  DWindElement = class(DInterfaceElement)
  private
    { images to render }
    Wind1, Wind2: DWindImage;
  public
    constructor Create; override;
  end;

type
  { Element with "ArrangeChildren" and "SpawnChildren" methods
    Usually it is just a container for bottom-level elements
    and doesn't draw anything by itself}
  DCompositeElement = class(DInterfaceElement)
  strict private
    isSpawned: boolean;
  strict protected
    { Arranges children elements within this container }
    procedure ArrangeChildren; virtual; abstract;
    { Creates (spawns) elements in this container }
    procedure SpawnChildren; virtual; abstract;
  public
    {}
    procedure RearrangeChildren;
    constructor Create; override;

    //procedure Rescale; override;
  end;

type
  { An element or an element group surrounded by a rectagonal frame
    Warning: changing frame.frame won't anchor content to the frame!
    Should Anchor it manually }
  DFramedElement = class(DCompositeElement)
  private
    { Frame image }
    fFrame: DFrameImage;
    procedure SetFrame(const Value: DRectagonalFrame);
  strict protected
    procedure ArrangeChildren; override;
    procedure SpawnChildren; override;
  public
    { Frame image around the element }
    property Frame: DRectagonalFrame read fFrame.Frame write SetFrame;
  end;

type
  { Image within a frame }
  DFramedImage = class(DFramedElement)
  strict protected
    procedure ArrangeChildren; override;
    procedure SpawnChildren; override;
  public
    { The image interface element }
    Image: DStaticImage;
  end;

type
  { A clickable button with automatic highlighting by mouseover
    Click should be managed in children elements
    Should receive a link to a loaded image, not load it from HDD. }
  DButton = class abstract(DFramedElement)
  private
    fisEnabled: boolean;
    { Link to the button image
      used only to check if the image=Value}
    OldImage: TCastleImage;
    { Two images for "mouse out" and "mouse over" states }
    Image_out,Image_over: DStaticImage;
    procedure SetImage(Value: TCastleImage);
    { mouse over and mouse out events }
    procedure MouseOver(const Sender: DAbstractElement; const x,y: integer);
    procedure MouseLeave(const Sender: DAbstractElement; const x,y: integer);
    procedure SetEnabled(const Value: boolean);
  strict protected
    procedure ArrangeChildren; override;
    procedure SpawnChildren; override;
  public
    { button image }
    property Image: TCastleImage write SetImage;
    property isEnabled: boolean read fisEnabled write SetEnabled;
  end;

type
  { A button with some text over it
    As a child of DButton it can also contain a bg image }
  DTextButton = class(DButton)
  private
    {Label displaying caption of the button}
    fLabel: DLabel;
    procedure SetText(Value: string);
    function GetText: string;
  strict protected
    procedure SpawnChildren; override;
    procedure ArrangeChildren; override;
  public
    { Displayed caption at the Button }
    property Caption: string read GetText write SetText;
  end;

type
  { A text label in a frame, displaying player's health }
  DHealthLabel = class(DFramedElement)
  private
    { Label that would display the health number }
    fLabel: DFloatLabel;
    { target Actor }
    fTarget: DBasicActor; //we don't need anything "higher" than this
    procedure SetTarget(const Value: DBasicActor);
  strict protected
    procedure ArrangeChildren; override;
    procedure SpawnChildren; override;
  public
    { the character being monitored }
    property Target: DBasicActor read fTarget write SetTarget;
  end;

type
  { Label, containing character's nickname, framed }
  DNameLabel = class(DFramedElement)
  private
    { Label that displays character's nickname }
    fLabel: DStringLabel;
    { target Actor }
    fTarget: DBasicActor; //we don't need anything "higher" than this
    procedure SetTarget(const Value: DBasicActor);
  strict protected
    procedure ArrangeChildren; override;
    procedure SpawnChildren; override;
  public
    { the character being monitored }
    property Target: DBasicActor read fTarget write SetTarget;
  end;

type
  { A statbar within a frame }
  DFramedBar = class(DFramedElement)
  private
    { Displayed statbar image }
    fBar: DStatBarImage;
    { target Actor }
    fTarget: PStatValue;
    procedure SetTarget(const Value: PStatValue);
  strict protected
    procedure ArrangeChildren; override;
    procedure SpawnChildren; override;
  public
    { Character's stat to be monitored }
    property Target: PStatValue read fTarget write SetTarget;
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
  strict protected
    procedure ArrangeChildren; override;
    procedure SpawnChildren; override;
  public
    {the character being monitored}
    property Target: DBasicActor read fTarget write SetTarget;
end;

type
  { Nickname + PlayerBars + NumericHP }
  DPlayerBarsFull = class(DCompositeElement) //not a child of DPlayerBars!
  private
    {these are links for easier access to the children}
    PlayerBars: DStatBars;
    Health: DHealthLabel;
    NickName: DNameLabel;
    {target character}
    fTarget: DBasicActor;
    procedure SetTarget(const Value: DBasicActor);
  strict protected
    procedure ArrangeChildren; override;
    procedure SpawnChildren; override;
  public
    {the character being monitored}
    property Target: DBasicActor read fTarget write SetTarget;
  end;

{type
  { Decorations around travel screen
    Are drawn "in background" with player's info overlay }
  DPartyDecorations = class(DCompositeElement)
  private
    Frame1Left,Frame1Right,
    Frame2Left,Frame2Right,
    Frame2BottomLeft,Frame2BottomRight,
    Frame3Bottom : DFrameImage;
  strict protected
    procedure ArrangeChildren; override;
    procedure SpawnChildren; override;
  end;}

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
    { Player character }
    fTarget: DPlayerCharacter;
    procedure SetTarget(const Value: DPlayerCharacter);
  strict protected
    procedure ArrangeChildren; override;
    procedure SpawnChildren; override;
  public
    { Player character which portrait is displayed }
    property Target: DPlayerCharacter read fTarget write SetTarget;
    { Call-back for character being hit }
    procedure doHit(const Dam: float; const DamType: TDamageType);
  end;

{
//todo: float label is identical except pointeger -> pfloat
type
  { a simple editor for an integer variable featuring plus and minus
    buttons
    How to set max/min? Maybe, this element is not needed, but specific
    elements should be used for every integer-edit case, like stat edit }
  DIntegerEdit = class(DFramedElement)
  private
    {sub-elements nicknames for easy access}
    iLabel: DIntegerLabel;
    PlusButton, MinusButton: DButton;
    { target value }
    fTarget: PInteger;
    procedure SetTarget(value: PInteger);
  public
    {integer to change}
    property Target: Pinteger read ftarget write settarget;
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
  strict protected
    procedure ArrangeChildren; override;
    procedure SpawnChildren; override;
  public
    property Target: DPerk read ftarget write settarget;
    //property Character: DPlayerCharacter read fCharacter write setcharacter;
    {proedure getSelectedStatus -----> update}}
end;

type
 {sorts in n rows and m lines the list of interface elements within self.base. Without specific data management and sorting parameters it is abstract and should parent DPerkSorter and DItemSorter}
 DAbstractSorter = class(DCompositeElement)
   private
  {
   public
     lines{,rows}: integer;
   strict protected
     procedure ArrangeChildren; override;
     procedure SpawnChildren; override;
     }
   end;

//type TPerkContainerStyle = (pcActions,pcActive,pcGlobal);

type DPerksContainer = class(DAbstractSorter)
  {container for buffs-debuffs, perks and actions}
  private
   { fTarget: DPlayerCharacter;
    procedure settarget(value: DPlayerCharacter);
  strict protected
    procedure ArrangeChildren; override;
    procedure SpawnChildren; override;
  public
    //ContainerStyle: TPerkContainerStyle;
    property Target: DPlayerCharacter read ftarget write settarget;
    procedure MakePerksList(animate: TAnimationStyle);
    //procedure UpdatePerksList;  }
  end;

type
  { Abstract dialogue element containing "content" and "buttons" }
  DAbstractDialogue = class(DFramedElement)
  private
    Content, fHeader: DFramedElement; {DFramedLabel}
    //ButtonSpace: DCompositeElement; {or button sorter!}
    //procedure SetText(Value: string);
    //function GetText: string;
  strict protected
    //procedure SpawnChildren; override;
    //procedure ArrangeChildren; override;
  public
    { Caption of the Dialogue window }
    //property Caption read GetText write SetText;
  end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

implementation
uses SysUtils,
   DecoFont, DecoImageProcess,
   //DecoInterfaceBlocks,
   DecoInterfaceLoader, DecoLog;

{===========================================================================}
{====================== D Wind Element =====================================}
{===========================================================================}

constructor DWindElement.Create;
begin
  inherited Create;
  SetFullScreen;

  //create wind image containers
  Wind1 := DWindImage.Create;
  Wind2 := DWindImage.Create;
  Grab(Wind1);
  Grab(Wind2);

  //load (copy) wind images
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
  isSpawned := false;
  inherited Create;
  SpawnChildren;
  isSpawned := true;
  ArrangeChildren;
  //Rescale;
end;

{-----------------------------------------------------------------------------}

procedure DCompositeElement.RearrangeChildren;
begin
  if isSpawned then begin
    ArrangeChildren;
    //Rescale;
  end;
end;

{-----------------------------------------------------------------------------}

{procedure DCompositeElement.Rescale;
begin
  //inherited Rescale;
  RearrangeChildren;
  inherited Rescale;
end;}

{===========================================================================}
{========================== Framed Element =================================}
{===========================================================================}

procedure DFramedElement.SetFrame(const Value: DRectagonalFrame);
begin
  if fFrame.Frame <> Value then begin
    fFrame.Frame := Value;
    //fFrame.Rescale;
    RearrangeChildren;
  end
end;

{-----------------------------------------------------------------------------}

procedure DFramedElement.SpawnChildren;
begin
  fFrame := DFrameImage.Create;
  Grab(fFrame);
end;

{-----------------------------------------------------------------------------}

procedure DFramedElement.ArrangeChildren;
begin
  //inherited ArrangeChildren;
  fFrame.AnchorTo(Self);
  fFrame.SetBaseSize(0,0,1,1);
end;

{===========================================================================}
{=========================== Framed Image =================================}
{===========================================================================}

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
  Image.AnchorToFrame(fFrame);
  Image.SetBaseSize(0,0,1,1);
end;

{===========================================================================}
{=========================== Framed Button =================================}
{===========================================================================}

procedure DButton.SetImage(Value: TCastleImage);
begin
  if OldImage<>Value then
  begin
    OldImage := Value;
    Image_out.Load(Value);
    Image_over.Load(Brighter(Value,1.2));
  end;
end;

{-----------------------------------------------------------------------------}

procedure DButton.SpawnChildren;
begin
  inherited SpawnChildren;
  OldImage := nil;
  Image_out := DStaticImage.Create;
  Image_over := DStaticImage.Create;
  Grab(Image_out);
  Grab(Image_over);
  //assign button mouse over/out events
  Self.OnMouseOver := @Self.MouseOver;
  Self.OnMouseLeave := @Self.MouseLeave;
  SetEnabled(true);
end;

{-----------------------------------------------------------------------------}

procedure DButton.MouseOver(const Sender: DAbstractElement; const x,y: integer);
begin
  if fisEnabled then begin
    Image_out.AnimateTo(asFadeOut);
    Image_over.AnimateTo(asFadeIn);
  end;  
end;

procedure DButton.MouseLeave(const Sender: DAbstractElement; const x,y: integer);
begin
  Image_out.AnimateTo(asFadeIn);
  Image_over.AnimateTo(asFadeOut);
end;

{-----------------------------------------------------------------------------}

procedure DButton.SetEnabled(const Value: boolean);
begin
  {emm... Value isn't used here? Rename method to "toggleEnabled" or use Value?}
  if Value = fisEnabled then Exit; //let's fix it this way for now.

  if fisEnabled then begin
    fisEnabled := false;
    Self.MouseLeave(nil,0,0); {and fade out in case enabled is set to false} //should go to "disabled" brightness
  end else 
    fisEnabled := true;
  CanMouseOver := fisEnabled;
end;

{-----------------------------------------------------------------------------}

procedure DButton.ArrangeChildren;
begin
  inherited ArrangeChildren;
  Image_out.AnchorToFrame(fFrame);
  Image_out.SetBaseSize(0,0,1,1);
  Image_over.AnchorToFrame(fFrame);
  Image_over.SetBaseSize(0,0,1,1);
end;

{===========================================================================}

procedure DTextButton.SpawnChildren;
begin
  inherited SpawnChildren;
  fLabel := DLabel.Create;
  Grab(fLabel);
end;

{-----------------------------------------------------------------------------}

procedure DTextButton.ArrangeChildren;
begin
  inherited ArrangeChildren;
  fLabel.AnchorToFrame(fFrame);
  fLabel.SetBaseSize(0,0,1,1);
  //fLabel.Base.Anchor[...] := acCenter;
end;

{-----------------------------------------------------------------------------}

procedure DTextButton.SetText(Value: string);
begin
  fLabel.Text := Value;
end;

{-----------------------------------------------------------------------------}

function DTextButton.GetText: string;
begin
  Result := fLabel.Text;
end;

{===========================================================================}
{=========================== Health Label ==================================}
{===========================================================================}


procedure DHealthLabel.SpawnChildren;
begin
  inherited SpawnChildren;
  Frame := Characterbar_Bottom;
  fLabel := DFloatLabel.Create; //scale=false
  fLabel.Digits := 0;
  fLabel.Font := PlayerHealthFont;
  Grab(fLabel);
end;

{-----------------------------------------------------------------------------}

procedure DHealthLabel.ArrangeChildren;
begin
  inherited ArrangeChildren;
  fLabel.AnchorToFrame(fFrame);
  fLabel.SetBaseSize(0,0,1,1);
end;

{-----------------------------------------------------------------------------}

procedure DHealthLabel.SetTarget(const Value: DBasicActor);
begin
  if fTarget <> Value then begin
    fTarget := Value;
    fLabel.Target := @Value.HP;
  end;
end;

{===========================================================================}
{============================ Name Label ===================================}
{===========================================================================}


procedure DNameLabel.SpawnChildren;
begin
  inherited SpawnChildren;
  Frame := Characterbar_Top;
  fLabel := DStringLabel.Create;  //scale=false
  fLabel.Font := PlayerNameFont;
  Grab(fLabel);
end;

{-----------------------------------------------------------------------------}

procedure DNameLabel.ArrangeChildren;
begin
  inherited ArrangeChildren;
  fLabel.AnchorToFrame(fFrame);
  fLabel.SetBaseSize(0,0,1,1);
end;

{-----------------------------------------------------------------------------}

procedure DNameLabel.SetTarget(const Value: DBasicActor);
begin
  if fTarget <> Value then begin
    fTarget := Value;
    fLabel.Target := @Value.Nickname;
  end;
end;

{===========================================================================}
{===================== Framed bar ==========================================}
{===========================================================================}

procedure DFramedBar.SetTarget(const Value: PStatValue);
begin
  if fTarget <> Value then begin
    fTarget := Value;
    fBar.Target := Value;
  end;
end;

{-----------------------------------------------------------------------------}

procedure DFramedBar.SpawnChildren;
begin
  inherited SpawnChildren;
  Frame := StatBarsFrame;
  fBar := DStatBarImage.Create;
  Grab(fBar);
end;

{-----------------------------------------------------------------------------}

procedure DFramedBar.ArrangeChildren;
begin
  inherited ArrangeChildren;
  fBar.AnchorToFrame(fFrame);
  fBar.SetBaseSize(0,0,1,1);
end;

{===========================================================================}
{===================== Stat bars ===========================================}
{===========================================================================}

procedure DStatBars.SpawnChildren;
begin
  inherited SpawnChildren;
  Frame := Characterbar_mid;

  {$Warning accessing private fields: redo it!}
  HP_bar := DFramedBar.Create;
  HP_bar.fBar.Load(HpBarImage);
  HP_bar.fBar.Kind := bsVertical;
  Grab(HP_bar);

  STA_bar := DFramedBar.Create;
  STA_bar.fBar.Load(StaBarImage);
  STA_bar.fBar.Kind := bsVertical;
  Grab(STA_bar);

  CNC_bar := DFramedBar.Create;
  CNC_bar.fBar.Load(CncBarImage);
  CNC_bar.fBar.Kind := bsVertical;
  Grab(CNC_bar);

  MPH_bar := DFramedBar.Create;
  MPH_bar.fBar.Load(MphBarImage);
  MPH_bar.fBar.Kind := bsVertical;
  Grab(MPH_bar);
end;

{-----------------------------------------------------------------------------}

procedure DStatBars.ArrangeChildren;
var ScaleX: float;
begin
  inherited ArrangeChildren;

  HP_bar. AnchorToFrame(fFrame);
  STA_bar.AnchorToFrame(fFrame);
  CNC_bar.AnchorToFrame(fFrame);
  MPH_bar.AnchorToFrame(fFrame);

  if (fTarget <> nil) and (fTarget.MaxMaxMPH > 0) then begin
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
    RearrangeChildren; //in case fTarget has different set of stats
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

  Grab(Health);
  Grab(NickName);
  Grab(PlayerBars);
end;

{---------------------------------------------------------------------------}

procedure DPlayerBarsFull.ArrangeChildren;
begin
  //inherited ArrangeChildren;

  NickName.AnchorTo(Self); //AnchorToFrame(fFrame);
  NickName.SetBaseSize(0,0.9,1,0.1);
  //NickName.Base.Anchor[asBottom].AlignTo := noAlign;
  Health.AnchorTo(Self);
  Health.SetBaseSize(0,0,1,0.1);
  //Health.Base.Anchor[asTop].AlignTo := noAlign;
  PlayerBars.Base.AnchorTo(Self.Current);
  PlayerBars.SetBaseSize(0,0.1,1,0.8);
  PlayerBars.Base.AnchorTop(NickName.Current,vaBottom);
  PlayerBars.Base.AnchorBottom(Health.Current,vaTop);

  {********** INTERFACE DESIGN BY Saito00 ******************}

  {const LabelSpace = 23/800;}
end;

{=============================================================================}
{========================== PARTY DECORATIONS ================================}
{=============================================================================}

{procedure DPartyDecorations.SpawnChildren;
begin
  //inherited SpawnChildren; <---- nothing to inherit
  SetFullScreen;

  Frame1Left := DFrameImage.Create;
  Frame1Left.Frame := DecorationFrame1_Left;
  Grab(Frame1Left);
  Frame1Right := DFrameImage.Create;
  Frame1Right.Frame := DecorationFrame1_Right;
  Grab(Frame1Right);
  Frame2Left := DFrameImage.Create;
  Frame2Left.Frame := DecorationFrame2_Left;
  Grab(Frame2Left);
  Frame2Right := DFrameImage.Create;
  Frame2Right.Frame := DecorationFrame2_Right;
  Grab(Frame2Right);
  Frame2BottomLeft := DFrameImage.Create;
  Frame2BottomLeft.Frame := DecorationFrame2_BottomLeft;
  Grab(Frame2BottomLeft);
  Frame2BottomRight := DFrameImage.Create;
  Frame2BottomRight.Frame := DecorationFrame2_BottomRight;
  Grab(Frame2BottomRight);
  Frame3Bottom := DFrameImage.Create;
  Frame3Bottom.Frame := DecorationFrame3_Bottom;
  Grab(Frame3Bottom);
end;                  }

{---------------------------------------------------------------------------}

{procedure DPartyDecorations.ArrangeChildren;
const yscale = 1/800;
      xscale = 1/1200;
var yy1, yy2: float;
begin
  inherited ArrangeChildren;

  {********** INTERFACE DESIGN BY Saito00 ******************}
  yy1 := (20+45+180*(MaxParty div 2+1)-22)*yscale;
  yy2 := (20+45+180*(MaxParty div 2+1)-22-27)*yscale;
  Frame1Left.       SetBaseSize(       0, 1-yy1,  50*xscale,   yy1);
  Frame2Left.       SetBaseSize(       0,    0,   9*xscale, 1-yy2);

  yy1 := (20+45+180*(MaxParty div 2)-22)*yscale;
  yy2 := (20+45+180*(MaxParty div 2)-22-27)*yscale;
  Frame1Right.      SetBaseSize( 1-50*xscale, 1-yy1,  50*xscale,   yy1);
  Frame2Right.      SetBaseSize( 1 -9*xscale,    0,   9*xscale, 1-yy2);

  Frame2BottomLeft. SetBaseSize(   9*xscale,    0, 300*xscale,  9*yscale);
  Frame2BottomRight.SetBaseSize(1-309*xscale,    0, 297*xscale,  9*yscale);   //???? SCALING ?????
  //todo: make frame3 scaled by content // maybe put it into a separate block?
  Frame3Bottom     .SetBaseSize( 280*xscale,    0, 300*xscale, 62*yscale);
  //Frame3Bottom     .base.backwardsetsize(frame2bottomright.base.x1-frame2bottomleft.base.x2+22*2,-1);
  //Frame3Bottom.AnimateTo(appear_animation);
  Rescale; //BUG?
end;}

{=============================================================================}
{========================== Character portrait ===============================}
{=============================================================================}

procedure DPortrait.SetTarget(const Value: DPlayerCharacter);
begin
  if fTarget <> Value then begin
    fTarget := value;
    //DStaticImage(content).FreeImage;
    Log(LogInitInterface,{$I %CURRENTROUTINE%},'Load from portrait');
    //DStaticImage(content).Load(Portrait_img[drnd.Random(Length(Portrait_img))]);  //todo
    fTarget.onHit := @Self.doHit;
    RearrangeChildren;
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
      dLog(LogInterfaceError,Self,{$I %CURRENTROUTINE%},'ERROR: Target.Actions is empty!');
  end else
    dLog(LogInterfaceError,Self,{$I %CURRENTROUTINE%},'ERROR: Target is nil!');
end;}

{---------------------------------------------------------------------}

end.

