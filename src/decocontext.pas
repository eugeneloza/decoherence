{Copyright (C) 2012-2017 Yevhen Loza

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a Copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.}

{---------------------------------------------------------------------------}

{ Context is a tool to pick a phrase/event
  from a list based on a set of conditions
  Each condition is a DContextElement
  The set of conditions is DContext
  The Result is determined as "convolution" of 2 DContext instances
  
  Amont the tasks there are: selecting a phrase variant based on character's
  parameters, such as gender, character, mood, health, availability of voice acting etc.,
  time of the day, location, etc.
  
  At this point the unit is temporary, because it will be required by
  scenario processor later in the game development }
unit DecoContext;

{$INCLUDE compilerconfig.inc}

interface

uses Generics.Collections, DecoGlobal;

type TContextRecord = string;   //todo: integer; read from xml list of possible Context elements

type TContextTarget = (ctSpeaker, ctListener, ctTopic);

type
  { A single record that represents a single Context element }
  DContextElement = class(DObject)
  public
    { Name (identifier) of this context element }
    Name: TContextRecord;
    { "target" is oazis-specific
      (and, maybe, wrong - should be included in name(identifier), e.g. "speaker-male") }
    Target: TContextTarget;
    { max and min values of the context element, e.g. that might go for
      disposition between 0.3 and 0.6 or for time between 3pm and 6pm }
    Max, Min: float;
    { importance of this context element
      1 is critically important (default)
      0 is not important at all }
    Importance: float;
    //constructor Create;
    //function Compare;
  end;

type TContextList = specialize TObjectList<DContextElement>;

//todo: some Context elements may and should be dynamically generated only when requested!!!!!!!!
type
  { A basic containter for Context

    Example: "Good morning!"
      Demand = 'MORNING', 'GREETING'
      Allow = 'MALE','FEMALE'
      DENY = 'HOSTILE'
    means that "Good morning!" would be spoken as a *greeting* and only in the *morning*
    the character may be either male or female
    and this phrase may not be spoken by a hostile character }
  DContext = class(DObject)
  public
    { if one of these fails, the whole Context fails }
    Demand: TContextList;
    { What Context is possible(Allowed) in this Context
      Demand searches for pairs in "Demand" and "Allow" lists }
    Allow: TContextList;
    { Which Context is disAllowed in this Context
      if one of this is true, the whole Context fails }
    Deny: TContextList;

    constructor Create; //override;
    destructor Destroy; override;
  
    { find a Context element by Name (identifier)
      * Maybe, should be split into FindAllowDemand and FindDeny as they're very different in nature }
    function FindByName(FindTarget: TContextTarget; FindRecord: TContextRecord): DContextElement;
end;

type
  { Not sure if this one is needed at this point, as this class is Oazis-specific
    This represents the "global" Context of the dialogue,
    i.e. each phrase would be compared to this Context
    and based on Result it will be "available" or "unavailable" }
  DDialogueContext = class(DObject)
  public
    {}
    Context: DContext;

    //speaker, listener, about

    { copies all Context records of the input }
    procedure CopyContext(const NewContext: DContext);
    { the same as a Copy, but FreeAndNils the source afterwards
      just to grab and free the function Result }
    procedure Extract(const NewContext: DContext);
    { if the given context is compatible with this dialogue? }
    function Compare(const CheckContext: DContext): float;

    constructor Create; //override;
    destructor Destroy; override;
  end;

{ make Context element }
function MCE(NewTarget: TContextTarget; NewContextElement: TContextRecord; NewImportance: float = 1; NewMin: float = 0; NewMax: float = 1): DContextElement;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, DecoLog, CastleVectors;

function MCE(NewTarget: TContextTarget; NewContextElement: TContextRecord; NewImportance: float = 1; NewMin: float = 0; NewMax: float = 1): DContextElement;
begin
  Result := DContextElement.Create;
  Result.Target := NewTarget;
  Result.Name := NewContextElement;
  Result.Importance := NewImportance;
  Result.Min := NewMin;
  Result.Max := NewMax;
end;

{-----------------------------------------------------------------------------}

function Maximum(a, b: float): float;
begin
  if a > b then Result := a else Result := b;
end;

function CompareElements(e1, e2: DContextElement): float; {boolean}
var Dist: float;
begin
  //fatal: comparing incompatible elements
  if e1.Name <> e2.Name then begin
    { actually such stuff should never happen }
    Result := 0;
    fLog(LogContextError,{$I %CURRENTROUTINE%},'Error: comparing incompatible elements '+e1.Name+' vs '+e2.Name);
    Exit;
  end;

  Dist := 0;
  if e1.Min > e2.Max then Dist := e1.Min - e2.Max else
  if e1.Max < e2.Min then Dist := e2.Min - e1.Max;

  if Zero(Dist) then
    Result := 1
  else
    Result := 1 - Maximum(e1.Importance, e2.Importance); //minimum(1-self.Importance,1-cmp.Importance); // include dist here?
end;

{==========================  Context  ===============================}

constructor DContext.Create;
begin
  //inherited Create; <------- nothing to inherit
  Demand := TContextList.Create(true);
  Allow := TContextList.Create(true);
  Deny := TContextList.Create(true);
end;

{--------------------------------------------------------------------------}

destructor DContext.Destroy;
begin
  FreeAndNil(Demand);
  FreeAndNil(Allow);
  FreeAndNil(Deny);
  inherited Destroy;
end;

{--------------------------------------------------------------------------}

function DContext.FindByName(FindTarget: TContextTarget; FindRecord: TContextRecord): DContextElement;
var i: integer;
begin
  Result := nil;
  //find only the first matching element
  for i := 0 to Demand.Count-1 do
    if (Demand[i].Target = FindTarget) and (Demand[i].Name = FindRecord) then begin
      Result := Demand[i];
      Exit;
    end;
  for i := 0 to Allow.Count-1 do
    if (Allow[i].Target = FindTarget) and (Allow[i].Name = FindRecord) then begin
      Result := Allow[i];
      Exit;
    end;
end;

{============================ DIALOGUE Context ===============================}

constructor DDialogueContext.Create;
begin
  //inherited Create; <------- nothing to inherit
  Context := DContext.Create;
end;

{--------------------------------------------------------------------------}

destructor DDialogueContext.Destroy;
begin
  FreeAndNil(Context);
  inherited Destroy;
end;

{--------------------------------------------------------------------------}

procedure DDialogueContext.CopyContext(const NewContext: DContext);
var i: integer;
begin
  for i := 0 to NewContext.Demand.Count-1 do
    Context.Demand.Add(NewContext.Demand[i]);
  for i := 0 to NewContext.Allow.Count-1 do
    Context.Allow.Add(NewContext.Allow[i]);
end;

{--------------------------------------------------------------------------}

procedure DDialogueContext.Extract(const NewContext: DContext);
var temp: DContext;
begin
  temp := NewContext;
  Self.CopyContext(temp);
  FreeAndNil(temp); //mmm? What did I mean by this?
end;

{--------------------------------------------------------------------------}

function DDialogueContext.Compare(const CheckContext: DContext): float;
var i: integer;
    tmp: DContextElement;
begin
  Result := 1;
  {$HINT process deny first, as it's most "severe"}
  for i := 0 to Context.Demand.Count-1 do begin
    tmp := CheckContext.FindByName(Context.Demand[i].Target,Context.Demand[i].Name);
    if tmp = nil then Result := 0 else
                      Result *= CompareElements(tmp, Context.Demand[i]);
    if Zero(Result) then Exit;
  end;
  for i := 0 to CheckContext.Demand.Count-1 do begin
    tmp := Context.FindByName(CheckContext.Demand[i].Target, CheckContext.Demand[i].Name);
    if tmp = nil then Result := 0 else
                      Result *= CompareElements(tmp, CheckContext.Demand[i]);
    if Zero(Result) then Exit;
  end;
  {search across demand/allow not made? Fix it relevant to the logic.}
end;

{--------------------------------------------------------------------------}

end.

