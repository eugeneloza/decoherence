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
unit decoperks;

{$INCLUDE compilerconfig.inc}

interface

uses classes, fgl,
  decoimages,
  decostats,
  decoglobal;

{Type
  TPerkKind = (pkAction, pkActive, pkPassive);
Type
  TPerkTarget = (ptSelf, ptNone, ptEnemy, ptAlly, ptArea, ptAllEnemies, ptAllAllies);

Type TPerkEffect = (peDamage, peHeal, peStat);   }

type
  {Simplest perk is a passive perk that influences the character in some way}
  DPerk = class(TComponent)
  public
    {Stores the perk's image. Generally we don't use perks directly as static images,
     but static images provide a convenient routine to load the image in a thread
     so let it be this way for now. Theoretically, it's better to make a separate
     object that handles perks and items images and allows just rescaling them
     correctly - to save memory. But it is not the issue for now.}
    Image: DStaticImage;
    //PerkKind: TPerkKind;
    //PerkType: TPerkType;
    //Effect: TPerkEffect;
    Strength: Float;
    PerkName: string;
    //Efficiency  modifier
    //Requirements
    //Requirements for research
    Constructor create(AOwner: TComponent); override;
    Destructor destroy; override;
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
Type DPerksList = specialize TFPGObjectList<DPerk>;

    {lists all perks possible ingame}
Var perks: DPerksList;

{loads perks data and images}
Procedure InitPerks;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
Implementation
uses CastleWindow;

Procedure InitPerks;
Var TmpPerk: DPerk;
Begin
  perks := DPerksList.create(true);
  TmpPerk := DPerk.create(Application);
  TmpPerk.image.LoadThread(PerksFolder+'crossed-swords.png');
  Perks.add(TmpPerk);
End;

{---------------------------------------------------------------------------}

Constructor DPerk.create(AOwner: TComponent);
Begin
  Inherited create(AOwner);
  Image := DStaticImage.create(self);
End;

{---------------------------------------------------------------------------}

Destructor DPerk.destroy;
Begin
  Inherited;

End;

end.

