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

{ This unit operates LoadScreens: loads and manages facts and loadscreen images }
unit decoloadscreen;

{$INCLUDE compilerconfig.inc}
interface

uses fgl,
  decotranslation;

type
  {link to image file}
  DLoadImage = class
    {file name without path}
    value: string;
end;

{actually this one looks like a TStringList for now, but may be extended in future}
TLoadImageList = specialize TFPGObjectList<DLoadImage>;

type
  {a fact with control of displayed frequency and a list of compatible images}
  DFact = class
    {fact text}
    value: string;
    {how many times the fact has been displayed}
    {$HINT TODO: not saved/read ATM (should overridden by a savegame?)}
    frequency: integer;
    {list of compatible loadscreen images. It's inefficient to store them
     "many copies" here... but that's for some optimization I might do later}
    compatibility: TLoadImageList;
    (* TODO: REQUIREMENTS: some facts may appear only after the fact has been
       discovered in-game. Not done yet. *)
    destructor destroy; override;
end;

Type TFactList = specialize TFPGObjectList<DFact>;

var
    {a list of all loaded facts and links to compatible loadscreen images}
    Facts: TFactList;

{initialize LoadScreens}
procedure InitLoadScreen;
{load facts from a filename. Used both by game and constructor.}
procedure LoadFacts(FileName: string);

{free fact list memory}
procedure FreeLoadScreen;
{get a random fact}
function GetRandomFact: string;
{get a random fact image compatible with the last fact
 MUST BE CALLED AFTER GetRandomFact}
function GetRandomFactImage: string;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog, CastleFilesUtils,
  DOM, CastleXMLUtils,
  decoglobal, decoinputoutput;

var LastFact: integer = -1;
    CurrentFact: DFact = nil;      //looks ugly! Maybe I should remake it?
function GetRandomFact: string;
var newFact: integer;
begin
  repeat
    NewFact := drnd.random(facts.Count);
  until (NewFact <> LastFact) and (drnd.random < 1/facts[newFact].frequency);
  inc(facts[newFact].frequency,7);      //todo balance facts frequency, now chance is 1,1/8,1/15,1/22...
  result := facts[newFact].value;
  CurrentFact := Facts[newFact];
  LastFact := newFact;
end;

{-----------------------------------------------------------------------------}

var LoadImageOld: string = '';
function GetRandomFactImage: string;
var LoadImageNew: string;
begin
  if currentFact = nil then
    raise Exception.create('GetRandomFactImage ERROR: Get Fact before getting the image!');
  if currentFact.compatibility.Count>0 then
    repeat
      //as ugly as it might ever be...
      LoadImageNew := currentFact.compatibility[drnd.random(currentFact.compatibility.Count)].value;
    until (LoadImageOld <> LoadImageNew) or (currentFact.compatibility.Count=1)
  else
    raise Exception.create('GetRandomFactImage ERROR: No images to load!');
  LoadImageOld := LoadImageNew;
  result := LoadImageNew;
  CurrentFact := nil;
end;

{---------------------------------------------------------------------------------}

procedure LoadFacts(FileName: string);
var FactsDoc: TXMLDocument;
    BaseElement: TDOMElement;
    ValueNode: TDOMElement;
    Iterator,Iterator2: TXMLElementIterator;
    F: DFact;
    LI: DLoadImage;
begin
  if facts<>nil then begin
    freeandnil(Facts);
    WriteLnLog('LoadFacts','Facts is not nil. Freeing...');
  end;
  Facts := TFactList.create(true);

  WriteLnLog('LoadFacts','Reading file '+FileName);

  FactsDoc := URLReadXMLSafe(FileName);
  BaseElement := FactsDoc.DocumentElement;
  Iterator := BaseElement.ChildrenIterator;
  try
    while Iterator.GetNext do
    begin
      F := DFact.create;
      F.frequency := 1;
      F.compatibility := TLoadImageList.create(true);
      ValueNode := Iterator.current.ChildElement('Value', true);  //todo: required=false and catch nil value
      F.value := {$IFDEF UTF8Encode}UTF8encode{$ENDIF}(ValueNode.TextData);
      try
        Iterator2 := Iterator.current.ChildElement('ImageList', true).ChildrenIterator;
        while Iterator2.GetNext do
        begin
          LI := DLoadImage.create;
          LI.value := {$IFDEF UTF8Encode}UTF8encode{$ENDIF}(Iterator2.current.TextData);
          F.compatibility.Add(LI);
        end;
      finally
        freeAndNil(Iterator2);
      end;

      Facts.add(F);
    end;
  finally
    FreeAndNil(Iterator);
  end;
  freeandnil(FactsDoc);
  WriteLnLog('LoadFacts','Reading file finished.');
end;

{---------------------------------------------------------------------------}

destructor DFact.destroy;
begin
  FreeAndNil(compatibility);
  inherited;
end;

{---------------------------------------------------------------------------}

procedure InitLoadScreen;
begin
  //LoadFacts does everything, jsut providing it a correct filename.
  LoadFacts(ApplicationData(LanguageDir(CurrentLanguage)+'facts.xml'+gz_ext));
end;

{---------------------------------------------------------------------------------}

procedure FreeLoadScreen;
begin
  FreeAndNil(Facts);
end;

end.

