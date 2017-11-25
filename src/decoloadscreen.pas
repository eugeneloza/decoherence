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
unit DecoLoadScreen;

{$INCLUDE compilerconfig.inc}
interface

uses Generics.Collections,
  DecoTranslation,
  DecoGlobal;

type
  {link to image file}
  DLoadImage = class(DObject)
    {file name without path}
    Value: string;
  end;

{ Actually this one looks like a TStringList for now, but may be extended in future }
TLoadImageList = specialize TObjectList<DLoadImage>;

type
  { A fact with control of displayed frequency and a list of compatible images }
  DFact = class
    { Fact text }
    Value: string;
    {how many times the fact has been displayed}
    {$HINT TODO: not saved/read ATM (should overridden by a savegame?)}
    Frequency: integer;
    {list of compatible loadscreen images. It's inefficient to store them
     "many copies" here... but that's for some optimization I might do later}
    Compatibility: TLoadImageList;
    (* TODO: REQUIREMENTS: some facts may appear only after the fact has been
       discovered in-game. Not done yet. *)
    destructor Destroy; override;
end;

Type TFactList = specialize TObjectList<DFact>;

var
    {a list of all loaded facts and links to compatible loadscreen images}
    Facts: TFactList;

{initialize LoadScreens}
procedure InitLoadScreen;
{load facts from a filename. Used both by game and constructor.}
procedure LoadFacts(const FileName: string);

{free fact list memory}
procedure FreeLoadScreen;
{get a random fact}
function GetRandomFact: string;
{get a random fact image compatible with the last fact
 MUST BE CALLED AFTER GetRandomFact}
function GetRandomFactImage: string;

function LoadScreenMainText: string;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleFilesUtils,
  DOM, CastleXMLUtils,
  DecoFont,
  DecoLog, Profiler;

var LastFact: integer = -1;
    CurrentFact: DFact = nil;      //looks ugly! Maybe I should remake it?
function GetRandomFact: string;
var NewFact: integer;
begin
  StartProfiler;

  repeat
    NewFact := DRND.Random(Facts.Count);
  until (NewFact <> LastFact) and (DRND.Random < 1/Facts[NewFact].Frequency);
  inc(Facts[NewFact].Frequency,7);      //todo balance facts frequency, now chance is 1,1/8,1/15,1/22...
  Result := Facts[NewFact].Value;
  CurrentFact := Facts[NewFact];
  LastFact := NewFact;

  StopProfiler;
end;

{-----------------------------------------------------------------------------}

var LoadImageOld: string = '';
function GetRandomFactImage: string;
var LoadImageNew: string;
begin
  StartProfiler;

  if CurrentFact = nil then
    raise Exception.Create('GetRandomFactImage ERROR: Get Fact before getting the image!');
  if CurrentFact.Compatibility.Count>0 then
    repeat
      //as ugly as it might ever be...
      LoadImageNew := CurrentFact.Compatibility[DRND.Random(CurrentFact.Compatibility.Count)].Value;
    until (LoadImageOld <> LoadImageNew) or (CurrentFact.Compatibility.Count=1)
  else
    raise Exception.Create('GetRandomFactImage ERROR: No images to load!');
  LoadImageOld := LoadImageNew;
  Result := LoadImageNew;
  CurrentFact := nil;

  StopProfiler;
end;

{---------------------------------------------------------------------------------}

procedure LoadFacts(const FileName: string);
var FactsDoc: TXMLDocument;
    BaseElement: TDOMElement;
    ValueNode: TDOMElement;
    Iterator,Iterator2: TXMLElementIterator;
    F: DFact;
    LI: DLoadImage;
begin
  StartProfiler;

  if Facts <> nil then begin
    Log(LogInitError,Facts.ClassName+'>'+_CurrentRoutine,'Facts is not nil. Freeing...');
    FreeAndNil(Facts);
  end;
  Facts := TFactList.Create(true);

  Log(LogInitInterface,Facts.ClassName+'>'+_CurrentRoutine,'Reading file '+FileName);

  FactsDoc := URLReadXML(FileName);
  BaseElement := FactsDoc.DocumentElement;
  Iterator := BaseElement.ChildrenIterator;
  try
    while Iterator.GetNext do
    begin
      F := DFact.Create;
      F.Frequency := 1;
      F.Compatibility := TLoadImageList.Create(true);
      ValueNode := Iterator.Current.ChildElement('Value', true);  //todo: required=false and catch nil value
      F.Value := {$IFDEF UTF8Encode}UTF8encode{$ENDIF}(ValueNode.TextData);
      try
        Iterator2 := Iterator.Current.ChildElement('ImageList', true).ChildrenIterator;
        while Iterator2.GetNext do
        begin
          LI := DLoadImage.Create;
          LI.Value := {$IFDEF UTF8Encode}UTF8encode{$ENDIF}(Iterator2.Current.TextData);
          F.Compatibility.Add(LI);
        end;
      finally
        FreeAndNil(Iterator2);
      end;

      Facts.Add(F);
    end;
  finally
    FreeAndNil(Iterator);
  end;
  FreeAndNil(FactsDoc);
  Log(LogInitInterface,Facts.ClassName+'>'+_CurrentRoutine,'Reading file finished.');

  StopProfiler;
end;

{---------------------------------------------------------------------------}

destructor DFact.Destroy;
begin
  StartProfiler;

  FreeAndNil(Compatibility);
  inherited Destroy;

  StopProfiler;
end;

{---------------------------------------------------------------------------}

procedure InitLoadScreen;
begin
  StartProfiler;

  //LoadFacts does everything, jsut providing it a correct filename.
  LoadFacts(ApplicationData(LanguageDir(CurrentLanguage)+'facts.xml'+gz_ext));

  StopProfiler;
end;

{---------------------------------------------------------------------------------}

procedure FreeLoadScreen;
begin
  StartProfiler;

  FreeAndNil(Facts);

  StopProfiler;
end;

{--------------------------------------------------------------------------}

function LoadScreenMainText: string;
begin
  StartProfiler;

  {remake it some day into something useful}
  case CurrentLanguage of
    Language_English: Result := 'Welcome to Decoherence :)'+dlinebreak+
                                'Loading completed,'+dlinebreak+
                                'just press any key...';
    Language_Russian: Result := 'Добро пожаловать в Decoherence :)'+dlinebreak+
                                'Загрузка завершена,'+dlinebreak+
                                'просто нажмите любую клавишу...';
    else Result := 'Language unavailable'
  end;

  StopProfiler;
end;

end.

