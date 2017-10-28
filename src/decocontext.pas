unit DecoContext;

{$mode objfpc}{$H+}

interface

uses fgl, DecoGlobal;

type TContextRecord = string;   //todo: integer; read from xml list of possible context elements

type TContextTarget = (ctSpeaker, ctListener, ctAbout);

type
  {a single record that represents a single context element}
  DContextElement = class(DObject)
  public
    name: TContextRecord;
    target: TContextTarget;
    max,min: float;
    importance: float;
    //constructor create;
    //function compare;
  end;

type TContextList = specialize TFPGObjectList<DContextElement>;

//todo: some context elements may and should be dynamically generated only when requested!!!!!!!!
type
  {a containter for context}
  DContext = class(DObject)
  public
    {if one of these fails, the whole context fails}
    demand: TContextList;
    {What context is possible(allowed) in this context
     demand searches for pairs in "demand" and "allow" lists}
    allow: TContextList;

    constructor Create;
    destructor Destroy; override;
    function FindByName(find_target: TContextTarget; find_record: TContextRecord): DContextElement;
end;

type

  DDialogueContext = class(DObject)
  public
    context: DContext;

    //speaker, listener, about

    {copies all context records of the input}
    procedure copycontext(const newcontext: DContext);
    {the same as a copy, but freeandnils the source afterwards
     just to grab and free the function result}
    procedure extract(const newcontext: DContext);
    function Compare(CheckContext: DContext): float;

    constructor Create;
    destructor Destroy; override;
  end;

{make context element}
function MCE(NewTarget: TContextTarget; NewContextElement: TContextRecord; newimportance: float = 1; newmin: float = 0; newmax: float = 1): DContextElement;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses sysUtils, castleLog, castlevectors;

function MCE(NewTarget: TContextTarget; NewContextElement: TContextRecord; newimportance: float = 1; newmin: float = 0; newmax: float = 1): DContextElement;
begin
  result := DContextElement.create;
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

function CompareElements(e1,e2: DContextElement): float; {boolean}
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

  if Zero(dist) then
    result := 1
  else
    result := 1-max(e1.importance,e2.importance); //minimum(1-self.importance,1-cmp.importance); // include dist here?
end;

{==========================  CONTEXT  ===============================}

constructor DContext.create;
begin
  inherited;
  demand := TContextList.create(true);
  allow := TContextList.create(true);
end;

{--------------------------------------------------------------------------}

destructor DContext.destroy;
begin
  freeandnil(demand);
  freeandnil(allow);
  inherited;
end;

{--------------------------------------------------------------------------}

function DContext.FindByName(find_target: TContextTarget; find_record: TContextRecord): DContextElement;
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

constructor DDialogueContext.create;
begin
  inherited;
  context := DContext.create;
end;

{--------------------------------------------------------------------------}

destructor DDialogueContext.destroy;
begin
  freeandnil(context);
  inherited;
end;

{--------------------------------------------------------------------------}

procedure DDialogueContext.copycontext(const newcontext: DContext);
var i: integer;
begin
  for i := 0 to newcontext.demand.Count-1 do
    context.demand.Add(newcontext.demand[i]);
  for i := 0 to newcontext.allow.Count-1 do
    context.allow.Add(newcontext.allow[i]);
end;

{--------------------------------------------------------------------------}

procedure DDialogueContext.extract(const newcontext: DContext);
var temp: DContext;
begin
  temp := newcontext;
  self.copycontext(temp);
  freeandnil(temp);
end;

{--------------------------------------------------------------------------}

function DDialogueContext.Compare(CheckContext: DContext): float;
var i: integer;
    tmp: DContextElement;
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

