{Copyright (C) 2012-2016 Yevhen Loza

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
unit decointerfaceblocks;

{$INCLUDE compilerconfig.inc}

interface

uses classes,
  decointerface, decointerfacecomposite,
  decoplayercharacter,
  decoglobal;


type
  {presents 7 characters with health bars, portraits and characters control space }
  DPartyView = class (DAbstractCompositeInterfaceElement)
  public
    PartyBars: array [0..maxparty] of DPlayerBars;
    Portraits: array [0..maxparty] of DPortrait;
    procedure ArrangeChildren(animate: boolean); override;
    constructor create(AOwner: TComponent); override;
  end;


{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
//uses ;

procedure DPartyView.ArrangeChildren(animate: boolean);
var i: integer;
begin
  {rescale party stat bars}
  for i := 0 to maxparty do begin
    if not odd(i) then
      PartyBars[i].setbasesize(0,1+(maxparty-i+1) div 2*3,0.5,3,1,animate)
    else
      PartyBars[i].setbasesize(-0.5,1+(maxparty-i+1) div 2*3,0.5,3,1,animate);
    PartyBars[i].rescale;
  end;

  {rescale party portraits}
  for i := 0 to maxparty do begin
    if not odd(i) then
      portraits[i].setbasesize(0.5,1+(maxparty-i+1) div 2*3,2.5,3,1,animate)
    else
      portraits[i].setbasesize(-3,1+(maxparty-i+1) div 2*3,2.5,3,1,animate);
    portraits[i].rescale;
  end;
end;

constructor DPartyView.create(AOwner: TComponent);
var i: integer;
begin
  inherited Create(AOwner);
  for i := 0 to maxparty do begin
    //create stat bars
    PartyBars[i] := DPlayerBars.create(self);
    PartyBars[i].target := party[i];
    self.children.Add(PartyBars[i]);

    //create portraits
    Portraits[i] := DPortrait.create(self);
    Portraits[i].target := party[i];
    self.children.Add(Portraits[i]);
  end;
  setbasesize(0,0,fullwidth,fullheight,1,false);
//  ArrangeChildren(false); //automatically arranged on TCompositeElement.setbasesize
end;


end.

