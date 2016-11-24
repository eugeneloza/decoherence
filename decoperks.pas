unit decoperks;

{$INCLUDE compilerconfig.inc}

interface

uses classes, fgl,
  decoimages,
  decoglobal;

Type
  TPerkKind = (pkAction, pkActive, pkPassive);
Type
  TPerkTarget = (ptSelf, ptNone, ptEnemy, ptAlly, ptArea, ptAllEnemies, ptAllAllies);

Type TPerkEffect = (peDamage, peHeal, peStat);
Type
  {A basic class representing a perk with its action and other routines}
  DPerk = class(TComponent)
  Public
    {Stores the perk's image}
    Image: DStaticImage;
    //PerkKind: TPerkKind;
    //PerkType: TPerkType;
    Effect: TPerkEffect;
    Strength: Float;
    PerkName: string;
    //Efficiency  modifier
    //Requirements
    //Requirements for research
    Constructor create(AOwner: TComponent); override;
    Destructor destroy; override;
  end;

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
  //TmpPerk.image.LoadThread('..');
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

