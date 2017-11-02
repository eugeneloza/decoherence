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

{ Creates large interface blocks for different game situations containing
  large amounts of interface elements and composite interface elements }
unit DecoInterfaceBlocks;

{$INCLUDE compilerconfig.inc}

interface

uses Classes,
  CastleControls,
  DecoInterfaceCore, DecoInterfaCecomposite,
  DecoImages, DecoLabels,
  DecoPlayerCharacter,
  DecoGlobal;

const appear_animation = asFadeIn;

type
  DLoadScreen = class(DCompositeElement)
  private
    Wind: DWindElement;
    Floater: DFloatImage;
    FloaterLabel: DPhasedLabel;
    MainLabel: DLabel;
  strict protected
    procedure SpawnChildren; override;
    procedure ArrangeChildren; override;
  public
    procedure ReloadFact;
  end;

type
  {stores and displays every interface element related to current character}
  DCharacterSpace = class(DInterfaceElement)
  private
   { fTarget: DPlayerCharacter;
    procedure settarget(value: DPlayerCharacter);
  public
    ID: integer;
    Slided: boolean;
    StatBars: DPlayerBarsFull;
    Portrait: DPortrait;
    property Target: DPlayerCharacter read fTarget write settarget;
    procedure ArrangeChildren(animate: TAnimationStyle); override;
    constructor Create(AOwner: TComponent); override;
  public
    procedure SlideIn(Sender: DAbstractElement; x,y: integer);
    procedure SlideOut(Sender: DAbstractElement; x,y: integer);
    procedure doSlideIn;
    procedure doSlideOut;
    procedure doTimeout; }
  end;

type
  {presents 7 characters with health bars, portraits and characters control space }
  DPartyView = class (DInterfaceElement)
  public
    {CharacterSpace: array [0..maxparty] of DCharacterSpace;
    procedure ArrangeChildren(animate: TAnimationStyle); override;
    constructor Create(AOwner: TComponent); override; }
  end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses CastleFilesUtils, DecoFont,
  DecoLoadScreen,
  DecoLog;

procedure DLoadScreen.SpawnChildren;
begin
  //inherited SpawnChildren; <------ nothing to inherit
  SetFullScreen;

  Floater := DFloatImage.Create;
  Grab(Floater);

  Wind := DWindElement.Create;
  Grab(Wind);

  FloaterLabel := DPhasedLabel.Create;
  Grab(FloaterLabel);

  MainLabel := DLabel.Create;
  Grab(MainLabel);
end;

{---------------------------------------------------------------------------}

procedure DLoadScreen.ArrangeChildren;
begin
  //inherited ArrangeChildren; <------ nothing to inherit
  Floater.SetBaseSize(0,0,1,1);
  Floater.onCycleFinish := @ReloadFact;

  FloaterLabel.Font := LoadScreenFont;

  MainLabel.Base.AnchorToWindow := true;
  MainLabel.Font := LoadScreenFont;
  MainLabel.ShadowIntensity := 1;

  ReloadFact;
end;

{---------------------------------------------------------------------------}

procedure DLoadScreen.ReloadFact;
begin
  MainLabel.SetBaseSize(0.03,0.8,0.4,0.1);
  MainLabel.text := LoadScreenMainText;

  FloaterLabel.SetBaseSize(0,2/3,0.3,0.1); //need to reset it each time
  FloaterLabel.Text := GetRandomFact;
  FloaterLabel.Phase := 0;
  Floater.Load(ApplicationData(LoadScreenFolder+GetRandomFactImage));
end;

{=============================================================================}
{=========================== Character Space =================================}
{=============================================================================}

{constructor DCharacterSpace.Create;
begin
  inherited Create;

  Timer.onTimer := @Self.doTimeout;

  Self.OnMouseEnter := @SlideIn;
  Self.OnMouseLeave := @SlideOut;
  //self.canMouseOver := true;

  //create portraits
  Portrait := DPortrait.create(self);
  Portrait.CanMouseOver := true;
  Portrait.OnMouseLeave := @SlideOut;
  Grab(Portrait);

  //create stat bars
  StatBars := DPlayerBarsFull.create(self);
  StatBars.CanMouseOver := true;
  StatBars.OnMouseEnter := @SlideIn;
  StatBars.OnMouseLeave := @SlideOut;
  Grab(StatBars);

end;}

{---------------------------------------------------------------------------}

{procedure DCharacterSpace.ArrangeChildren;
begin
  //inherited ArrangeChildren(animate); //not needed here as this element doesn't have a frame
  {********** INTERFACE DESIGN BY Saito00 ******************}
  if Odd(self.ID) then Portrait.Frame := portraitframe_right else
                       Portrait.Frame := portraitframe_left;

  if not Odd(self.ID) then
    StatBars.setbasesize(10/800,-(40+155+180*(self.ID div 2))/800,35/800,155/800,1,animate)
  else
    StatBars.setbasesize(-45/800,-(40+155+180*(self.ID div 2))/800,35/800,155/800,1,animate);
  StatBars.rescale;

  if not odd(self.ID) then begin
    Portrait.setbasesize(47/800,-(40+155+180*(self.ID div 2))/800,135/800,155/800,1,animate);
    {bug}
    Portrait.DamageLabel.setbasesize(47/800,-(40+155+180*(self.ID div 2))/800,135/800,155/800,1,animate);
    Portrait.DamageOverlay.setbasesize(47/800,-(40+155+180*(self.ID div 2))/800,135/800,155/800,1,animate);
  end else begin
    Portrait.setbasesize(-(46+135)/800,-(40+155+180*(self.ID div 2))/800,135/800,155/800,1,animate);
    {bug}
    Portrait.DamageLabel.setbasesize(-(46+135)/800,-(40+155+180*(self.ID div 2))/800,135/800,155/800,1,animate);
    Portrait.DamageOverlay.setbasesize(-(46+135)/800,-(40+155+180*(self.ID div 2))/800,135/800,155/800,1,animate);
  end;
  //Portrait.rescale;
  Slided := true;
  doSlideOut;

end; }

{---------------------------------------------------------------------------}

{procedure DCharacterSpace.SetTarget(value: DPlayerCharacter);
begin
  if fTarget <> Value then begin
    Self.fTarget := Value;
    StatBars.Target := Self.fTarget;
    Portrait.Target := Self.fTarget;
  end;
end;}

{---------------------------------------------------------------------------}

{procedure DCharacterSpace.DoSlideIn;
var myx: float;
begin
  Slided := true;
  if not Odd(self.ID) then
    myx := 47/800
  else
    myx := -(46)/800-Portrait.Base.fw;

  {bug}
  Portrait.DamageLabel.setbasesize(myx,Portrait.base.fy,Portrait.base.fw,Portrait.base.fh,1,asDefault);
  Portrait.DamageLabel.rescale;
  Portrait.DamageOverlay.setbasesize(myx,Portrait.base.fy,Portrait.base.fw,Portrait.base.fh,1,asDefault);
  Portrait.DamageOverlay.rescale;

  Portrait.setbasesize(myx,Portrait.base.fy,Portrait.base.fw,Portrait.base.fh,1,asDefault);
  Portrait.rescale;
end;}

{---------------------------------------------------------------------------}

{procedure DCharacterSpace.DoSlideOut;
var myx: float;
begin
  slided := false;
  if not odd(self.ID) then
    myx := 0/800
  else
    myx := -135{portrait.base.fw}/800;
  //{$WARNING Memory Leak here}

  {bug}
  Portrait.DamageLabel.setbasesize(myx,Portrait.base.fy,Portrait.base.fw,Portrait.base.fh,0,asDefault);
  Portrait.DamageLabel.rescale;
  Portrait.DamageOverlay.setbasesize(myx,Portrait.base.fy,Portrait.base.fw,Portrait.base.fh,0,asDefault);
  Portrait.DamageOverlay.rescale;

  Portrait.setbasesize(myx,Portrait.base.fy,Portrait.base.fw,Portrait.base.fh,0,asDefault);
  Portrait.rescale;
end;}

{---------------------------------------------------------------------------}

{procedure DCharacterSpace.SlideIn(Sender: DAbstractElement; x,y: integer);
begin
  if not slided then
    if MouseOverTree(x,y) then doSlideIn;
end;}

{---------------------------------------------------------------------------}

{procedure DCharacterSpace.slideOut(Sender: DAbstractElement; x,y: integer);
begin
  if slided then
    if not MouseOverTree(x,y) then timer.settimeout(PortraitTimeOut){doSlideOut};
  //if selected then timeout 10-30 seconds, else just doslideout
end;}

{---------------------------------------------------------------------------}

{procedure DCharacterSpace.doTimeout;
begin
  if slided then begin
    if not IsMouseOverTree then doSlideOut;
    dLog('timer out');
  end;
end;}

{=============================================================================}
{============================= Party  view ===================================}
{=============================================================================}

{procedure DPartyView.ArrangeChildren(animate: TAnimationStyle);
var i: integer;
begin
  //inherited ArrangeChildren(animate); //not needed here as this element doesn't have a frame
  {********** INTERFACE DESIGN BY Saito00 ******************}

  {rescale party}
  //{$WARNING Memory Leak here}
  for i := 0 to maxparty do begin
    if not odd(i) then
      CharacterSpace[i].setBaseSize(0,-(40+155+180*(i div 2))/800,35/800,155/800,1,animate)
    else
      CharacterSpace[i].setBaseSize(-35/800,-(40+155+180*(i div 2))/800,35/800,155/800,1,animate);
//      CharacterSpace[i].setBaseSize(0,0,fullwidth,fullheight,1,animate);
  end;
end;}

{-----------------------------------------------------------------------------}

{constructor DPartyView.Create(AOwner: TComponent);
var i: integer;
begin
  inherited Create(AOwner);
  for i := 0 to CurrentParty.Character.Count-1 do begin
    CharacterSpace[i] := DCharacterSpace.create(self);
    CharacterSpace[i].ID := i;
    {$warning todo}
    CharacterSpace[i].Target := CurrentParty.Character[i];
    CharacterSpace[i].ScaleToChildren := true;
    grab(CharacterSpace[i]);
    //at grab we set parent as DPartyView;
  end;
  SetBaseSize(0,0,FullWidth,FullHeight,1,Appear_Animation);
//  ArrangeChildren(false); //automatically arranged on TCompositeElement.setbasesize
end;}

end.

