{Copyright (C) 2012-2017 Yevhen Loza

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

{ An animated 3D body for Actors }
unit DecoBody;

{$INCLUDE compilerconfig.inc}

interface

uses Classes,
  CastleResources, CastleScene, Castle3D,
  CastleCreatures,
  DecoGlobal;

{ at this moment it works fine. So no need to "upgrade" it }
type DBodyResource = TCreatureResource;

{type
  { this class contains references to animations of DBodyResource,
    maybe it's redundant. }
  TBodyKind = class (TObject)}

{not sure about it, it would cover basic enemies animations, but
 some Actors, player characters and npcs first of all, may use advanced set
 of animations, therefore just calling the animation by name might be better
 at least for now.}
type TAnimationType = (atIdle, atWalk, atAttack, atHurt, atDie);
{type TAnimationType = string;
const atIdle = 'idle';
      atWalk = 'walk';
      atAttack = 'attack';
      atHurt = 'hurt';
      atDie = 'die';}

type
  { Body is a 3D manifestation of an Actor in the 3D world.
    First of all it manages animations of the Actor.
    T3DOrient has been chosen because it can translate/rotate the animated scene,
    however, the exact ancestor choise is not clear yet.
    Each Actor gets a unique body, which references to a specific body resource }
  DBody = class(T3DOrient)
  { animation display routines }
  private

    CurrentAnimation: T3DResourceAnimation;
    { Current "time" for the scene to determine the animation frame to display.
      As animations are managed internally in Castle Game Engine,
      we follow their specification of "single" time management for now.
      At the moment it is absolutely identical to our implementation of time }
    Time: Single;
    procedure AdvanceTime(SecondsPassed: single);{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  public
    { Name of current and next animation }
    CurrentAnimationName, NextAnimationName: string;

    { reference to the spawning body resource }
    Resource: DBodyResource;
    { Coefficient to slow the time. E.g. 0.9 will slow time by 10% for this
      actor's body animations }
    SlowTimeRate: float;
    { resets the current playing animation }
    procedure ResetAnimation;


    { Returns current frame of the animation
      Engine-specific }
    function GetChild: T3D; override;
    { Advances time and recalculates the geometry
      Engine-specific }
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    constructor Create(AOwner: TComponent); override;
  end;

var tmpKnightCreature: DBodyResource;


{what to do after animation has ended?
 aeLoop - just repeat it forever (e.g. Idle or Walk)
 aeStop - stop the animation an fix the Actor in last position (i.e. dead)
 aeIdle - switch to idle (after any action finished)
 mind that this determines automatic management of animations,
 "Walk" animation loops forever and is managed by Actor AI}
type TAnimationEnd = (aeLoop, aeStop, aeIdle);
function AnimationEnd(at: TAnimationType): TAnimationEnd;
function AnimationEnd(at: string): TAnimationEnd;
function AnimationToString(at: TAnimationType): string;

procedure tmpLoadKnightCreature;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses CastleLog, CastleFilesUtils,
  DecoInputOutput;

function AnimationEnd(at: TAnimationType): TAnimationEnd;
begin
  case at of
    atIdle:   Result := aeLoop;
    atWalk:   Result := aeLoop;
    atAttack: Result := aeIdle;
    atHurt:   Result := aeIdle;
    atDie:    Result := aeStop;
    else WriteLnLog('DecoBody.AnimationEnd','ERROR: Unknown animation '+AnimationToString(at));
  end;
end;
function AnimationEnd(at: string): TAnimationEnd;
begin
  case at of
    'idle','walk': Result := aeLoop;
    'die': Result := aeStop;
    else Result := aeIdle;
  end;
end;

{---------------------------------------------------------------------------}

function AnimationToString(at: TAnimationType): string;
begin
  case at of
    atIdle:   Result := 'idle';
    atWalk:   Result := 'walk';
    atAttack: Result := 'attack';
    atHurt:   Result := 'hurt';
    atDie:    Result := 'die';
    else WriteLnLog('DecoBody.AnimationEnd','ERROR: Unknown animation ');
  end;
end;

{================================= D BODY =================================}

function DBody.GetChild: T3D;
begin
  if not (GetExists and Resource.Prepared) then Exit;
  if CurrentAnimation = nil then ResetAnimation;
  Result := CurrentAnimation.Scene(Time, true);
end;

{---------------------------------------------------------------------------}

procedure DBody.ResetAnimation;
begin
  //if not (GetExists and Resource.Prepared) then Exit;
  CurrentAnimation := Resource.Animations.FindName(CurrentAnimationName);
  time := 0;
end;

{---------------------------------------------------------------------------}

procedure DBody.AdvanceTime(SecondsPassed: single);{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  Time += SecondsPassed * SlowTimeRate;
end;
procedure DBody.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  AdvanceTime(SecondsPassed);
  VisibleChangeHere([vcVisibleGeometry]);
end;

{---------------------------------------------------------------------------}

constructor DBody.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  time := 0; //redundant
  SlowTimeRate := 1.0;
  case drnd.Random(5) of
    0: CurrentAnimationName := 'walk';
    1: CurrentAnimationName := 'hurt';
    2: CurrentAnimationName := 'attack';
    3: CurrentAnimationName := 'die';
    else CurrentAnimationName := 'idle';
  end;
  //ResetAnimation;
end;

{****************************************************************************}

procedure tmpLoadKnightCreature;
begin
  Resources.LoadSafe(ApplicationData('models/creatures/knight_creature/'));
  {$hint todo}
  {  Animation := TMyAnimation.Create(Res,'idle');
  Animation.URL := ApplicationData('knight_multiple_castle_anim_frames/idle.castle-anim-frames');
  Animation.Prepare1;
  Res.Animations.Add(Animation);
  Res.Prepare(nil);}

  tmpKnightCreature := Resources.FindName('Knight') as DBodyResource;
  tmpKnightCreature.Prepare(nil);
end;

end.

