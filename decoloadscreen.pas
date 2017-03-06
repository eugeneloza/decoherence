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

{ Temporary(?) unit "loads" facts and loadscreen images }
unit decoloadscreen;

{$INCLUDE compilerconfig.inc}
interface

uses fgl,
  decotranslation;

type DFact = class
  value: string;
  frequency: integer;
  //compatibility
  //times displayed?
end;

Type TFactList = specialize TFPGObjectList<DFact>;

var Facts: TFactList;
    //facts_text: array of string;
    image_text: array of string;

var //N_facts: integer;
    N_images: integer;

procedure LoadFacts;
procedure LoadFacts(FileName: string);

procedure DestroyFacts;
function GetRandomFact: string;
function GetRandomFactImage: string;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog, CastleFilesUtils,
  DOM, {CastleDownload, }CastleXMLUtils,
  decoglobal;

var LastFact: integer = -1;


function GetRandomFact: string;
var newFact: integer;
begin
  repeat
    NewFact:=rnd.random(facts.Count);
  until (NewFact <> LastFact) and (rnd.random < 1/facts[newFact].frequency);
  inc(facts[newFact].frequency,7);      //todo balance facts frequency, now chance is 1,1/8,1/15,1/22...
  result := facts[newFact].value;
  LastFact := newFact;
end;

{-----------------------------------------------------------------------------}

var LoadImageOld: integer=-1;
function GetRandomFactImage: string;
var LoadImageNew: integer;
begin
  repeat
    LoadImageNew := rnd.random(N_Images);
  until LoadImageOld <> LoadImageNew;
  LoadImageOld := LoadImageNew;
  result := image_text[loadImageNew];
end;

{---------------------------------------------------------------------------------}

procedure LoadFacts(FileName: string);
var FactsDoc: TXMLDocument;
    BaseElement: TDOMElement;
    ValueNode: TDOMElement;
    Iterator: TXMLElementIterator;
    F: DFact;
begin
  if facts<>nil then begin
    freeandnil(Facts);
    WriteLnLog('LoadFacts','Facts is not nil. Freeing...');
  end;
  Facts := TFactList.create(true);

  WriteLnLog('LoadFacts','Reading file '+FileName);

  FactsDoc := URLReadXML(FileName);
  BaseElement := FactsDoc.DocumentElement;
  Iterator := BaseElement.ChildrenIterator;
  try
    while Iterator.GetNext do
    begin
      F := DFact.create;
      ValueNode := Iterator.current.ChildElement('Value', true);
      //{$PUSH}{$WARN 4105 OFF} // string conversion is ok here
      F.value := UTF8encode(ValueNode.TextData);
      //{$POP}
      F.frequency := 1;
      Facts.add(F);
    end;
  finally
    FreeAndNil(Iterator);
  end;
  freeandnil(FactsDoc);
  WriteLnLog('LoadFacts','Reading file finished.');
end;

procedure LoadFacts;
begin

  LoadFacts(ApplicationData(Scenario_Folder+LanguageDir(CurrentLanguage)+'facts'+xml_extension));

  {---}

  N_images := 43+1;

  setLength(image_text,N_images);
  image_text[00] := 'colour-of-nature-fractal_CC0_by_Sharon_Apted_[colorize].jpg';
  image_text[01] := 'lovely-image_CC0_by_Sharon_Apted_[GMIC].jpg';
  image_text[02] := 'pink-fractal-13086661465Zb_CC0_by_Sharon_Apted_[crop].jpg';
  image_text[03] := 'alien-worm_CC0_by_Piotr_Siedlecki_[invert,colorize].jpg';
  image_text[04] := 'angry-mantis_CC0_by_Sharon_Apted_[GMIC].jpg';
  image_text[05] := 'beige-fractal_CC0_by_Sharon_Apted_[GMIC,colorize].jpg';
  image_text[06] := 'blue-fractal-1307520582C9u_CC0_by_Sharon_Apted_[colorize,glow].jpg';
  image_text[07] := 'colour-of-nature_CC0_by_Sharon_Apted_[GMIC,colorize].jpg';
  image_text[08] := 'feathery-fractal-1308665767ois_CC0_by_Sharon_Apted_[Gimp,colorize,glow].jpg';
  image_text[09] := 'fractal-1305618518XpG_CC0_by_Sharon_Apted_[glow].jpg';
  image_text[10] := 'fractal-plate-like-image_CC0_by_Sharon_Apted_[glow,colorize].jpg';
  image_text[11] := 'fractal-ball-3_CC0_by_Piotr_Siedlecki_[glow,colorize].jpg';
  image_text[12] := 'fractal-spirals-1441742946ko2_CC0_by_Piotr_Siedlecki_[gmic,glow].jpg';
  image_text[13] := 'fractal-splash-1441596688e0H_CC0_by_Piotr_Siedlecki_[colorize].jpg';
  image_text[14] := 'pink-fantasy_CC0_by_Sharon_Apted_[gimp].jpg';
  image_text[15] := 'pretty-fractal-1307075682Q9V_CC0_by_Sharon_Apted_[colozize].jpg';
  image_text[16] := 'simple-fractal-13356910404Li_CC0_by_Sharon_Apted_[gimp].jpg';
  image_text[17] := 'simply-green_CC0_by_Sharon_Apted_[crop].jpg';
  image_text[18] := 'white-fractal-flower_CC0_by_Piotr_Siedlecki_[crop].jpg';
  image_text[19] := 'hintergrund-tapete-1456752859roe_CC0_by_kai_Stachowiak.jpg';
  image_text[20] := 'a-flame-fractal_CC0_by_Sharon_Apted_[gmic].jpg';
  image_text[21] := 'beautiful-fractal-1309767713KZe_CC0_by_Sharon_Apted_[crop,gmic].jpg';
  image_text[22] := 'colourful-fractal-1357555007a2W_CC0_by_Sharon_Apted_[gmic].jpg';
  image_text[23] := 'colourful-star_CC0_by_Sharon_Apted_[gmic].jpg';
  image_text[24] := 'cosmic-adventure_CC0_by_Sharon_Apted_[glow,color].jpg';
  image_text[25] := 'feather-duster_CC0_by_Sharon_Apted_[glow].jpg';
  image_text[26] := 'fractal-2014-1_CC0_by_Claudette_Gallant_[crop].jpg';
  image_text[27] := 'fractal-1309767060SE3_CC0_by_Sharon_Apted_[GIMP,GMIC].jpg';
  image_text[28] := 'fractal-13075203504gu_CC0_by_Sharon_Apted_[glow,gmic].jpg';
  image_text[29] := 'fractal-disks_CC0_by_Piotr_Siedlecki_[colorize].jpg';
  image_text[30] := 'fractal-shield_CC0_by_Piotr_Siedlecki_[gmic,glow,crop].jpg';
  image_text[31] := 'fractal-spiral-1401066127NAn_CC0_by_Piotr_Siedlecki_[crop,glow].jpg';
  image_text[32] := 'rose-window-1397763406a2Y_CC0_by_Piotr_Siedlecki_[colorize].jpg';
  image_text[33] := 'turquoise-fractal_CC0_by_Sharon_Apted_[gmic,crop,colorize].jpg';
  image_text[34] := 'golden-spiral_by_Piotr_Siedlecki_[crop].jpg';
  image_text[35] := 'red-orange-and-yellow-kaleidoscope_CC0_by_Michelle_Arena_[crop,softglow].jpg';
  image_text[36] := 'red-swirl-in-the-dark_CC0_by_Lynn_Greyling_[crop,glow].jpg';
  image_text[37] := 'white-rotation-on-black_CC0_by_Lynn_Greyling_[gimp,gmic,crop].jpg';
  image_text[38] := 'violet-tunnel_by_Piotr_Siedlecki_[gimp,crop].jpg';
  image_text[39] := 'yellow-spiral_by_Piotr_Siedlecki_[gimp,crop].jpg';
  image_text[40] := 'SunFlare_CC0_by-GIMP.jpg';
  image_text[41] := 'LensFlare_CC0_by-GIMP.jpg';
  image_text[42] := 'Milky_Way_2005_CC0_by_NASA_[glow,crop].jpg';
  image_text[43] := 'Ocean_planet1_CC0_by_Merikanto_[gimp,gmic].jpg';
end;

{---------------------------------------------------------------------------------}

procedure DestroyFacts;
begin
  FreeAndNil(Facts);
  //
end;

end.

