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
unit DecoActorBody;

{$INCLUDE compilerconfig.inc}

interface

uses Classes,
  CastleResources, CastleScene, Castle3D,
  CastleCreatures,
  DecoGlobal;

{ at this moment it works fine. So no need to "upgrade" it }
type DBodyResource = T3DResource;

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

const AnimationExtension = '.castle-anim-frames';

type
  { Body is a 3D manifestation of an Actor in the 3D world.
    First of all it manages animations of the Actor.
    T3DOrient has been chosen because it can translate/rotate the animated scene,
    however, the exact ancestor choise is not clear yet.
    Each Actor gets a unique body, which references to a specific body resource }
  DBody = class(T3DOrient)
  { animation display routines }
  private
    CurrentChild: T3D;

    CurrentAnimation: T3DResourceAnimation;
    { Current "time" for the scene to determine the animation frame to display.
      As animations are managed internally in Castle Game Engine,
      we follow their specification of "single" time management for now.
      At the moment it is absolutely identical to our implementation of time }
    Time: Single;
    { Move Actor's time forward }
    procedure AdvanceTime(const SecondsPassed: single);{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  public
    { Name of current and next animation }
    CurrentAnimationName, NextAnimationName: string;

    { Reference to the spawning body resource }
    Resource: DBodyResource;
    { Coefficient to slow the time. E.g. 0.9 will slow time by 10% for this
      actor's body animations }
    SlowTimeRate: float;
    { Resets the current playing animation }
    procedure ResetAnimation;


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
function AnimationEnd(const at: TAnimationType): TAnimationEnd;
function AnimationEnd(const at: string): TAnimationEnd;
function AnimationToString(const at: TAnimationType): string;

procedure tmpLoadKnightCreature;
procedure FreeCreatures;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, CastleFilesUtils,
  {DecoHDD,} DecoLog, Profiler;

function AnimationEnd(const at: TAnimationType): TAnimationEnd;
begin
  StartProfiler;

  case at of
    atIdle:   Result := aeLoop;
    atWalk:   Result := aeLoop;
    atAttack: Result := aeIdle;
    atHurt:   Result := aeIdle;
    atDie:    Result := aeStop;
    else Log(LogAnimationError,_CurrentRoutine,'ERROR: Unknown animation '+AnimationToString(at));
  end;

  StopProfiler;
end;
function AnimationEnd(const at: string): TAnimationEnd;
begin
  StartProfiler;

  case at of
    'idle','walk': Result := aeLoop;
    'die': Result := aeStop;
    else Result := aeIdle;
  end;

  StopProfiler;
end;

{---------------------------------------------------------------------------}

function AnimationToString(const at: TAnimationType): string;
begin
  StartProfiler;

  case at of
    atIdle:   Result := 'idle';
    atWalk:   Result := 'walk';
    atAttack: Result := 'attack';
    atHurt:   Result := 'hurt';
    atDie:    Result := 'die';
    else Log(LogAnimationError,_CurrentRoutine,'ERROR: Unknown animation '+AnimationToString(at));
  end;

  StopProfiler;
end;

{================================= D BODY =================================}

{---------------------------------------------------------------------------}

procedure DBody.ResetAnimation;
begin
  StartProfiler;

  //if not (GetExists and Resource.Prepared) then Exit;
  if Resource = nil then Exit; //if the actor has no body, just hang up
  CurrentAnimation := (Resource.Animations.FindName(CurrentAnimationName)) as T3DResourceAnimation{DAnimation};
  Time := 0;

  StopProfiler;
end;

{---------------------------------------------------------------------------}

procedure DBody.AdvanceTime(const SecondsPassed: single);{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  StartProfiler;

  Time += SecondsPassed * SlowTimeRate;

  // manage end of the animation
  if (CurrentAnimation<>nil) and (Time>CurrentAnimation.Duration) then begin
    if NextAnimationName<>'' then begin
      CurrentAnimationName := NextAnimationName;
      ResetAnimation;
      NextAnimationName := '';
    end else
      case AnimationEnd(CurrentAnimationName) of
        aeLoop: Time -= CurrentAnimation.Duration;
        aeIdle: begin
                  CurrentAnimationName := 'idle';
                  ResetAnimation;
                end;
        aeStop: Time := CurrentAnimation.Duration-0.01; //this is ugly, fix it soon
      end;
  end;

  StopProfiler;
end;
procedure DBody.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);

  { Returns current frame of the animation
    Engine-specific }
  function GetChild: T3D;
  begin
    if not (GetExists and Resource.Prepared) then Exit;
    if CurrentAnimation = nil then ResetAnimation;
    Result := CurrentAnimation.Scene(Time, true);
  end;

  procedure UpdateChild;
  var
    NewChild: T3D;
  begin
    NewChild := GetChild;
    if CurrentChild <> NewChild then
    begin
      if CurrentChild <> nil then
        Remove(CurrentChild);
      CurrentChild := NewChild;
      if CurrentChild <> nil then
        Add(CurrentChild);
    end;
  end;

begin
  StartProfiler;

  AdvanceTime(SecondsPassed);
  VisibleChangeHere([vcVisibleGeometry]);
  UpdateChild;

  StopProfiler;
end;

{---------------------------------------------------------------------------}

constructor DBody.Create(AOwner: TComponent);
begin
  StartProfiler;

  inherited Create(AOwner);
  Time := 0; //redundant
  SlowTimeRate := 1.0;
  case DRND.Random(5) of
    0: CurrentAnimationName := 'walk';
    1: CurrentAnimationName := 'hurt';
    2: CurrentAnimationName := 'attack';
    3: CurrentAnimationName := 'die';
    else CurrentAnimationName := 'idle';
  end;
  //ResetAnimation;
  StopProfiler;
end;

{============================================================================}

{****************************************************************************}

procedure tmpLoadKnightCreature;
var CreatureName: string;
begin
  StartProfiler;

  CreatureName := 'knight';

  Resources.LoadFromFiles(ApplicationData(CreaturesFolder+CreatureName));
  tmpKnightCreature := Resources.FindName('Knight') as DBodyResource;
  tmpKnightCreature.Prepare(nil);

  StopProfiler;
end;

{---------------------------------------------------------------------------}

procedure FreeCreatures;
begin
  StartProfiler;
  Log(LogInitData,_CurrentRoutine,'Freeing creature resources...');
  //nothing here
  StopProfiler;
end;



end.

