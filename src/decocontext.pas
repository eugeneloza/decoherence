unit DecoContext;

{$mode objfpc}{$H+}

interface

uses fgl;

type TContextRecord = string;   //todo: integer; read from xml list of possible context elements

type TContextTarget = (ctSpeaker, ctListener, ctAbout);

type
  {a single record that represents a single context element}
  OContextElement = class(TObject)
  public
    name: TContextRecord;
    target: TContextTarget;
    max,min: float;
    importance: float;
    //constructor create;
    //function compare;
  end;

type TContextList = specialize TFPGObjectList<OContextElement>;

//todo: some context elements may and should be dynamically generated only when requested!!!!!!!!
type
  {a containter for context}
  OContext = class(TObject)
  public
    {if one of these fails, the whole context fails}
    demand: TContextList;
    {What context is possible(allowed) in this context
     demand searches for pairs in "demand" and "allow" lists}
    allow: TContextList;

    constructor Create;
    destructor Destroy; override;
    function FindByName(find_target: TContextTarget; find_record: TContextRecord): OContextElement;
end;

type

  ODialogueContext = class(TObject)
  public
    context: OContext;

    //speaker, listener, about

    {copies all context records of the input}
    procedure copycontext(const newcontext: OContext);
    {the same as a copy, but freeandnils the source afterwards
     just to grab and free the function result}
    procedure extract(const newcontext: OContext);
    function Compare(CheckContext: OContext): float;

    constructor Create;
    destructor Destroy; override;
  end;

{make context element}
function MCE(NewTarget: TContextTarget; NewContextElement: TContextRecord; newimportance: float = 1; newmin: float = 0; newmax: float = 1): OContextElement;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses sysUtils, castleLog, castlevectors;

function MCE(NewTarget: TContextTarget; NewContextElement: TContextRecord; newimportance: float = 1; newmin: float = 0; newmax: float = 1): OContextElement;
begin
  result := OContextElement.create;
  result.target := newTarget;
  result.name := NewContextElement;
  result.importance := newimportance;
  result.min := newmin;
  result.max := newmax;
end;

{-----------------------------------------------------------------------------}

function max(a,b: float): float;
begin
  if a>b then result := a else result := b;
end;

function CompareElements(e1,e2: OContextElement): float; {boolean}
var dist: float;
begin
  //fatal: comparing incompatible elements
  if e1.name<>e2.name then begin
    result := 0;
    WriteLnLog('CompareElements','Error: comparing incompatible elements '+e1.name+' vs '+e2.name);
    exit;
  end;

  dist := 0;
  if e1.min > e2.max then dist := e1.min-e2.max else
  if e1.max < e2.min then dist := e2.min-e1.max;

  if zero(dist) then
    result := 1
  else
    result := 1-max(e1.importance,e2.importance); //minimum(1-self.importance,1-cmp.importance); // include dist here?
end;

{==========================  CONTEXT  ===============================}

constructor OContext.create;
begin
  inherited;
  demand := TContextList.create(true);
  allow := TContextList.create(true);
end;

{--------------------------------------------------------------------------}

destructor OContext.destroy;
begin
  freeandnil(demand);
  freeandnil(allow);
  inherited;
end;

{--------------------------------------------------------------------------}

function OContext.FindByName(find_target: TContextTarget; find_record: TContextRecord): OContextElement;
var i: integer;
begin
  result := nil;
  //find only the first matching element
  for i := 0 to demand.count-1 do
    if (demand[i].target = find_target) and (demand[i].name = find_record) then begin
      result := demand[i];
      exit;
    end;
  for i := 0 to allow.count-1 do
    if (allow[i].target = find_target) and (allow[i].name = find_record) then begin
      result := allow[i];
      exit;
    end;
end;

{============================ DIALOGUE CONTEXT ===============================}

constructor ODialogueContext.create;
begin
  inherited;
  context := OContext.create;
end;

{--------------------------------------------------------------------------}

destructor ODialogueContext.destroy;
begin
  freeandnil(context);
  inherited;
end;

{--------------------------------------------------------------------------}

procedure ODialogueContext.copycontext(const newcontext: OContext);
var i: integer;
begin
  for i := 0 to newcontext.demand.Count-1 do
    context.demand.Add(newcontext.demand[i]);
  for i := 0 to newcontext.allow.Count-1 do
    context.allow.Add(newcontext.allow[i]);
end;

{--------------------------------------------------------------------------}

procedure ODialogueContext.extract(const newcontext: OContext);
var temp: OContext;
begin
  temp := newcontext;
  self.copycontext(temp);
  freeandnil(temp);
end;

{--------------------------------------------------------------------------}

function ODialogueContext.Compare(CheckContext: OContext): float;
var i: integer;
    tmp: OContextElement;
begin
  result := 1;
  for i := 0 to context.demand.count-1 do begin
    tmp := CheckContext.FindByName(context.demand[i].target,context.demand[i].name);
    if tmp = nil then result := 0 else
                      result *= CompareElements(tmp,context.demand[i]);
    if Zero(result) then exit;
  end;
  for i := 0 to CheckContext.demand.count-1 do begin
    tmp := context.FindByName(CheckContext.demand[i].target,CheckContext.demand[i].name);
    if tmp = nil then result := 0 else
                      result *= CompareElements(tmp,CheckContext.demand[i]);
    if Zero(result) then exit;
  end;

end;

{--------------------------------------------------------------------------}

end.

