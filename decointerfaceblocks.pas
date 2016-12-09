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

type
  {decorations around travel screen}
  DDecorations = class(DAbstractCompositeInterfaceElement)
  private
    frame1left,frame1right,
    frame2left,frame2right,
    frame2bottomleft,frame2bottomright: DSingleInterfaceElement;
  public
    procedure ArrangeChildren(animate: boolean); override;
    constructor create(AOwner: TComponent); override;
  end;


{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
//uses ;

procedure DPartyView.ArrangeChildren(animate: boolean);
var i: integer;
begin
  {********** INTERFACE DESIGN BY ALEXEY ******************}

  {rescale party stat bars}
  for i := 0 to maxparty do begin
    if not odd(i) then
      PartyBars[i].setbasesize(10/800,-(40+155+180*(i div 2))/800,35/800,155/800,1,animate)
    else
      PartyBars[i].setbasesize(-45/800,-(40+155+180*(i div 2))/800,35/800,155/800,1,animate);
    PartyBars[i].rescale;
  end;

  {rescale party portraits}
  for i := 0 to maxparty do begin
    if not odd(i) then
      portraits[i].setbasesize(46/800,-(40+155+180*(i div 2))/800,135/800,155/800,1,animate)
    else
      portraits[i].setbasesize(-(46+135)/800,-(40+155+180*(i div 2))/800,135/800,155/800,1,animate);
    portraits[i].rescale;
  end;
end;

{-----------------------------------------------------------------------------}

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

{=============================================================================}
{============================= Decorations ===================================}
{=============================================================================}

procedure DDecorations.ArrangeChildren(animate: boolean);
var yy1,yy2: float;
begin
  {********** INTERFACE DESIGN BY ALEXEY ******************}
  yy1 := (20+45+180*4-22)/800; {todo}
  //yy2 := yy1;//
  yy2 := (20+45+180*4-22-27)/800;
  frame1left.       setbasesize(       0, -yy1,  50/800,   yy1, 1, false);
  frame1right.      setbasesize( -50/800, -yy1,  50/800,   yy1, 1, false);
  frame2left.       setbasesize(       0,    0,   9/800, 1-yy2, 1, false);
  frame2right.      setbasesize(  -9/800,    0,   9/800, 1-yy2, 1, false);
  frame2bottomleft. setbasesize(   9/800,    0, 300/800, 9/800, 1, false);
  frame2bottomright.setbasesize(-309/800,    0, 300/800, 9/800, 1, false);
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
  setbasesize(0,0,fullwidth,fullheight,1,false);
end;

end.

