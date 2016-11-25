unit decoperks;

{$INCLUDE compilerconfig.inc}

interface

uses classes, fgl,
  decoimages,
  decoglobal;

{Type
  TPerkKind = (pkAction, pkActive, pkPassive);
Type
  TPerkTarget = (ptSelf, ptNone, ptEnemy, ptAlly, ptArea, ptAllEnemies, ptAllAllies);

Type TPerkEffect = (peDamage, peHeal, peStat);   }

type
  {Simplest perk is a passive perk that influences the character in some way}
  DPassivePerk = class(TComponent)
  public
    {Stores the perk's image}
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

  end;



{list of perks}
Type DPerksList = specialize TFPGObjectList<DPassivePerk>;

    {lists all perks possible ingame}
Var perks: DPerksList;

{loads perks data and images}
Procedure InitPerks;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
Implementation
uses CastleWindow;

Procedure InitPerks;
Var TmpPerk: DActivePerk;
Begin
  perks := DPerksList.create(true);
  TmpPerk := DActivePerk.create(Application);
  //TmpPerk.image.LoadThread('..');
  Perks.add(TmpPerk);
End;

{---------------------------------------------------------------------------}

Constructor DPassivePerk.create(AOwner: TComponent);
Begin
  Inherited create(AOwner);
  Image := DStaticImage.create(self);
End;

{---------------------------------------------------------------------------}

Destructor DPassivePerk.destroy;
Begin
  Inherited;
End;

end.

