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

{Contains description of perks and actions}
unit DecoPerks;

{$INCLUDE compilerconfig.inc}

interface

uses Classes, fgl,
  //decoimages,
  DecoStats,
  DecoGlobal;

{Type
  TPerkKind = (pkAction, pkActive, pkPassive);
Type
  TPerkTarget = (ptSelf, ptNone, ptEnemy, ptAlly, ptArea, ptAllEnemies, ptAllAllies);

Type TPerkEffect = (peDamage, peHeal, peStat);   }

type
  {Simplest perk is a passive perk that influences the character in some way}
  DPerk = class(TObject)
  public
    //PerkKind: TPerkKind;
    //PerkType: TPerkType;
    //Effect: TPerkEffect;
    Strength: Float;
    PerkName: string;
    //Efficiency  modifier
    //Requirements
    //Requirements for research
    constructor Create; virtual;//override;
    destructor Destroy; override;
end;
(*
Type
  {Active perk must be chosen to take effect and it's availability depends
   on other chosen perks, actions first of all}
  DActivePerk = class(DPassivePerk)
  Public

  end;

Type
  {No direct action of its own, but gives access to actions}
  DChapterPerk = class(DActivePerk) //todo
  Public

  end;


Type
  {Selectable general action, containing an optional subset
   of Active and Passive perks}
  DActionPerk = class(DChapterPerk)
  Public

  end;            *)



{list of perks}
type DPerksList = specialize TFPGObjectList<DPerk>;

type DMultiPerk = class(DPerk)
  Children: DPerksList;
  constructor Create; override;
  destructor Destroy; override;
end;

    {lists all perks possible ingame}
var Perks: DPerksList;

{loads perks data and images}
procedure InitPerks;
procedure FreePerks;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
Implementation
uses SysUtils, DecoLog;

constructor DPerk.Create;
begin
  inherited Create;
  //Image := DStaticImage.create(self);
end;

{---------------------------------------------------------------------------}

destructor DPerk.Destroy;
begin
  inherited Destroy;

end;

{---------------------------------------------------------------------------}

constructor DMultiPerk.Create;
begin
  inherited Create;
  Children := DPerksList.Create;
end;

{---------------------------------------------------------------------------}

destructor DMultiPerk.Destroy;
begin
  FreeAndNil(Children);
  inherited Destroy;
end;

{============================================================================}

procedure InitPerks;
var TmpPerk: DPerk;
begin
  dLog(LogInitCharacters,nil,{$I %CURRENTROUTINE%},'Loading perks...');
  Perks := DPerksList.create(true);
  TmpPerk := DPerk.create;
  //TmpPerk.image.LoadThread(PerksFolder+'crossed-swords.png');
  Perks.add(TmpPerk);
end;

{---------------------------------------------------------------------------}

procedure FreePerks;
begin
  dLog(LogInitCharacters,nil,{$I %CURRENTROUTINE%},'Freeing perks...');
  FreeAndNil(Perks);
end;

end.

