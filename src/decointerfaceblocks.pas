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
  DecoInterface, DecoInterfaCecomposite,
  DecoPlayerCharacter,
  DecoGlobal;

const appear_animation = asFadeIn;

type
  {stores and displays every interface element related to current character}
  DCharacterSpace = class(DAbstractCompositeInterfaceElement)
  private
    fTarget: DPlayerCharacter;
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
    procedure doTimeout;
  end;

type
  {presents 7 characters with health bars, portraits and characters control space }
  DPartyView = class (DAbstractCompositeInterfaceElement)
  public
    CharacterSpace: array [0..maxparty] of DCharacterSpace;
    procedure ArrangeChildren(animate: TAnimationStyle); override;
    constructor Create(AOwner: TComponent); override;
  end;

type
  {decorations around travel screen}
  DDecorations = class(DAbstractCompositeInterfaceElement)
  private
    frame1left,frame1right,
    frame2left,frame2right,
    frame2bottomleft,frame2bottomright,
    frame3bottom : DSingleInterfaceElement;
  public
    procedure ArrangeChildren(Animate: TAnimationStyle); override;
    constructor Create(AOwner: TComponent); override;
  end;


{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses CastleLog;

{=============================================================================}
{=========================== Character Space =================================}
{=============================================================================}

constructor DCharacterSpace.create(AOwner: TComponent);
begin
  inherited Create(AOwner);

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

end;

{---------------------------------------------------------------------------}

procedure DCharacterSpace.ArrangeChildren(animate: TAnimationStyle);
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

  if not odd(self.ID) then
    Portrait.setbasesize(47/800,-(40+155+180*(self.ID div 2))/800,135/800,155/800,1,animate)
  else
    Portrait.setbasesize(-(46+135)/800,-(40+155+180*(self.ID div 2))/800,135/800,155/800,1,animate);
  //Portrait.rescale;
  Slided := true;
  doSlideOut;

end;

{---------------------------------------------------------------------------}

procedure DCharacterSpace.SetTarget(value: DPlayerCharacter);
begin
  if fTarget <> Value then begin
    Self.fTarget := Value;
    StatBars.Target := Self.fTarget;
    Portrait.Target := Self.fTarget;
  end;
end;

{---------------------------------------------------------------------------}

procedure DCharacterSpace.DoSlideIn;
var myx: float;
begin
  Slided := true;
  if not Odd(self.ID) then
    myx := 47/800
  else
    myx := -(46)/800-Portrait.Base.fw;
  Portrait.setbasesize(myx,Portrait.base.fy,Portrait.base.fw,Portrait.base.fh,1,asDefault);
  Portrait.rescale;
end;

{---------------------------------------------------------------------------}

procedure DCharacterSpace.DoSlideOut;
var myx: float;
begin
  slided := false;
  if not odd(self.ID) then
    myx := 0/800
  else
    myx := -135{portrait.base.fw}/800;
  //{$WARNING Memory Leak here}
  Portrait.setbasesize(myx,Portrait.base.fy,Portrait.base.fw,Portrait.base.fh,0,asDefault);
  Portrait.rescale;
end;

{---------------------------------------------------------------------------}

procedure DCharacterSpace.SlideIn(Sender: DAbstractElement; x,y: integer);
begin
  if not slided then
    if MouseOverTree(x,y) then doSlideIn;
end;

{---------------------------------------------------------------------------}

procedure DCharacterSpace.slideOut(Sender: DAbstractElement; x,y: integer);
begin
  if slided then
    if not MouseOverTree(x,y) then timer.settimeout(PortraitTimeOut){doSlideOut};
  //if selected then timeout 10-30 seconds, else just doslideout
end;

{---------------------------------------------------------------------------}

procedure DCharacterSpace.doTimeout;
begin
  if slided then begin
    if not IsMouseOverTree then doSlideOut;
    writelnlog('timer out');
  end;
end;

{=============================================================================}
{============================= Party  view ===================================}
{=============================================================================}

procedure DPartyView.ArrangeChildren(animate: TAnimationStyle);
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
end;

{-----------------------------------------------------------------------------}

constructor DPartyView.Create(AOwner: TComponent);
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
end;

{=============================================================================}
{============================= Decorations ===================================}
{=============================================================================}

procedure DDecorations.ArrangeChildren(animate: TAnimationStyle);
var yy1,yy2: float;
begin
  // inherited ArrangeChildren(animate); //not needed here
  {********** INTERFACE DESIGN BY Saito00 ******************}
  yy1 := (20+45+180*(maxparty div 2+1)-22)/800;
  yy2 := (20+45+180*(maxparty div 2+1)-22-27)/800;
  frame1left.       setbasesize(       0, -yy1,  50/800,   yy1, 1, appear_animation);
  frame2left.       setbasesize(       0,    0,   9/800, 1-yy2, 1, appear_animation);

  yy1 := (20+45+180*(maxparty div 2)-22)/800;
  yy2 := (20+45+180*(maxparty div 2)-22-27)/800;
  frame1right.      setbasesize( -50/800, -yy1,  50/800,   yy1, 1, appear_animation);
  frame2right.      setbasesize(  -9/800,    0,   9/800, 1-yy2, 1, appear_animation);

  frame2bottomleft. setbasesize(   9/800,    0, 300/800,  9/800, 1, appear_animation);
  frame2bottomright.setbasesize(-309/800,    0, 297/800,  9/800, 1, appear_animation);   //???? SCALING ?????
  //todo: make frame3 scaled by content // maybe put it into a separate block?
  frame3bottom     .setbasesize( 280/800,    0, 300/800, 62/800, 1, appear_animation);
  frame3bottom     .base.backwardsetsize(frame2bottomright.base.x1-frame2bottomleft.base.x2+22*2,-1);
  frame3bottom.AnimateTo(appear_animation);
  rescale;
end;

constructor DDecorations.create(AOwner: TComponent);
begin
  inherited create(AOwner);

  frame1left := DSingleInterfaceElement.create(self);
  frame1left.frame := decorationframe1_left;
  grab(frame1left);
  frame1right := DSingleInterfaceElement.create(self);
  frame1right.frame := decorationframe1_right;
  grab(frame1right);
  frame2left := DSingleInterfaceElement.create(self);
  frame2left.frame := decorationframe2_left;
  grab(frame2left);
  frame2right := DSingleInterfaceElement.create(self);
  frame2right.frame := decorationframe2_right;
  grab(frame2right);
  frame2bottomleft := DSingleInterfaceElement.create(self);
  frame2bottomleft.frame := decorationframe2_bottomleft;
  grab(frame2bottomleft);
  frame2bottomright := DSingleInterfaceElement.create(self);
  frame2bottomright.frame := decorationframe2_bottomright;
  grab(frame2bottomright);
  frame3bottom := DSingleInterfaceElement.create(self);
  frame3bottom.frame := decorationframe3_bottom;
  grab(frame3bottom);
  setbasesize(0,0,fullwidth,fullheight,1,appear_animation);
end;

end.

