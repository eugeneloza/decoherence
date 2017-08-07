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

{ Definitions and routines for player character and recruitable characters }
unit decoplayercharacter;

{$INCLUDE compilerconfig.inc}

interface

uses Classes, fgl,
  DecoActor, DecoRaceProfession, DecoPerks,
  DecoGlobal;

const MaxParty = 6; {0..6 = 7 characters}

{
  {Stores the perk's image. Generally we don't use perks directly as static images,
   but static images provide a convenient routine to load the image in a thread
   so let it be this way for now. Theoretically, it's better to make a separate
   object that handles perks and items images and allows just rescaling them
   correctly - to save memory. But it is not the issue for now.}
  Image: DStaticImage;
}

Type
  {player character - the most complex actor available :)}
  DPlayerCharacter = class(DActor)
  public
    Procedure Die; override;
    constructor Create; override;
    destructor Destroy; override;
end;

{list of the party characters}
type DCharList = specialize TFPGObjectList<DPlayerCharacter>;

type
  { Physical manifestation of Player in the world
    including camera
    party characters }
  DParty = class (TComponent)
  public
    CameraMan: DCoordActor;
    Char: DCharList;
    procedure tmpParty;
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  end;

var Party: DParty;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, CastleLog;

constructor DParty.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Char := DCharList.create(true);
end;

{----------------------------------------------------------------------------}

Procedure DParty.tmpParty;
var i: integer;
    NewMember: DPlayerCharacter;
begin
  for i := 0 to MaxParty do begin
    NewMember := DPlayerCharacter.Create;
    NewMember.MaxMaxMPH := 0;
    NewMember.Hit(DRND.Random(80),1);
    NewMember.DrainCNC(DRND.Random(80),1);
    NewMember.DrainMPH(DRND.Random(80),1);
    NewMember.DrainSTA(DRND.Random(80),1);

    if (Perks=nil) or (Perks.Count=0) then WriteLnLog('CreateTestParty','FATAL ERROR: Perks is empty!');
    NewMember.Actions.Add(Perks[0]);

    Char.Add(NewMember);
  end;
end;

{----------------------------------------------------------------------------}

Destructor DParty.Destroy;
var i: integer;
begin
  FreeAndNil(Char); //it will auto free all its components
end;

{======================== DPlayerCharacter ==================================}

constructor DPlayerCharacter.Create;
begin
  inherited;
end;

{----------------------------------------------------------------------------}

Procedure DPlayerCharacter.Die;
begin
  WriteLnLog('DPlayerCharacter.die','Character has entered clinical death state');
end;

{----------------------------------------------------------------------------}

destructor DPlayerCharacter.Destroy;
begin
  Inherited;
end;

end.

