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

{ contains definitions for 3d World entity }
unit DecoNodeParser;

{$INCLUDE compilerconfig.inc}
{$HINT IsPlaceholder is not profiled}

interface

uses X3DNodes,
  DecoGlobal;

type
  {information on the node name parsing}
  DNodeInfo = record
    {does this node collide with the actors? (default=false)}
    Collision: boolean;
    {is this node visible? (default=true)}
    Visible: boolean;
    {random chance to be placed (default=1.0)}
    Rand: float;
    {symmetry group / autoassigned; -1 is no symmetry}
    Symmetry: integer;
    {possible deviation angle in radians (default = 0.0)}
    Deviationangle: float;
    {placeholder marker}
    Placeholder: string;
    {name marker}
    Name: string;
    {trigeger marker name}
    Trigger: string;
  end;

{temporary?
 if this node is a placeholder}
function IsPlaceholder(const Node: TX3DNode): boolean;

function ParseNode(const Node: TX3DNode): DNodeInfo;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses StrUtils, DecoLog, Profiler;

{eeeem? there should have been this procedure in System unit?}
function StrToInt(const v: string): integer;
var
  e: integer;
begin
  {StartProfiler}

  if v = '' then
    Result := 0
  else
  begin
    Val(v, Result, e);
    if e <> 0 then
      Log(LogParserError, _CurrentRoutine, 'Invalid integer: ' + v);
  end;

  {StopProfiler}
end;


{============================= NODE NAMING RULES =============================}
{ "*" and "@" must go in front of the node name!
  "(" must be either the first or the second symbol
  Everything between "(" and ")" is considered a placeholder name
  parameters starting with "/" may appear anywhere}

{=========================== NODE NAMING EXAMPLES ============================}
{ *Cube.001 -------- is a simple visible collision node
  @Cube.002 -------- is an invisible collision node
  Cube.001 --------- is a visible non-collision node

  (table) ---------- is a placeholder node (no collisions)
  *(chair) --------- is a placeholder node (with collisions)
  (column)/s=3 ----- is a placeholder node of 3rd symmetry group
  (painting)/r=75 -- is a placeholder node with 75% chance to be displayed
  (chest)/a=15 ----- is a placeholder node with 15 deg. random rotation around z-axis

  /////// EXPERIMENTAL

  button/trigger=button1; is a trigger node, launching trigger "button1" when clicked
  (step-over triggers are attached to tiles, not to nodes)
  lamp/name=lamp1; -- adds this node to namespace as "lamp1" to allow events/scripts to change its state, e.g. by lamp1.turnOn/lamp1.turnOff

  todo: tags
  some tags are "generic" (like light nodes)
  unlike name - tags can be identical for many nodes
}

const
  CollisionSymbol = '*';
  //visible=true, collision=true (default is visible=true, collision=false)
  InvisibleCollision = '@'; //visible=false, collision=true
  PlaceHolderMarker = '(';
  PlaceHolderEndMarker = ')';
  ParameterMarker = '/';  //used to detect if a parameter is present in the string
  SymmetryMarker = '/s=';  //adds this node to the symmetry group
  RandomMarker = '/r=';  //chance that node will not be replaced by "empty"
  AngleDeviationMarker = '/a='; //small deviations of the initial rotation along z-axis
  NameMarker = '/name=';  //adds this node to the node names space
  TriggerMarker = '/trigger=';  //attaches a trigger to the node
  SeparatorMarker = ';'; //marks the end of a trigger/name string value

function ParseNode(const Node: TX3DNode): DNodeInfo;
var
  NameLength: integer; //for optimization

  function GetMarker(const Marker: string): string;
  var
    i: integer;
    s: string;
    Finish: boolean;
  begin
    i := AnsiIndexText(Node.X3DName, marker);
    Result := '';
    if i >= 0 then
    begin
      i += Length(Marker) - 1; //skip the marker
      repeat
        Inc(i);
        s := Copy(Node.X3DName, i, 1); //copy a char
        Finish := (s = ParameterMarker) or (s = SeparatorMarker);
        if not Finish then
          Result := Result + s;
      until Finish or (i >= NameLength);
    end;
  end;

  function GetPlaceholder: string;
  var
    j: integer;
    s: string;
    Finish: boolean;
  begin
    j := -1;
    if (Copy(Node.X3DName, 1, 1) = PlaceHolderMarker) then
      j := 1; //start of the placeholder signature
    if (Copy(Node.X3DName, 2, 1) = PlaceHolderMarker) then
      j := 2;
    Result := '';
    if j > 0 then
    begin
      repeat
        Inc(j);
        s := Copy(Node.X3DName, j, 1); //copy a char
        Finish := (s = PlaceHolderEndMarker);
        if not Finish then
          Result := Result + s;
      until Finish or (j >= NameLength);
      if s <> PlaceHolderEndMarker then
      begin
        Log(LogParserError, Node.ClassName + '>' + _CurrentRoutine,
          'ERROR: Could not find end of the placeholder: ' + Node.X3DName);
        //result := '';
      end;
    end;
  end;

begin
  {StartProfiler}

  //parse visible/collision state
  Result.Visible := True;
  Result.Collision := False;
  if (Copy(Node.X3DName, 1, 1) = CollisionSymbol) then
    Result.Collision := True;
  if (Copy(Node.X3DName, 1, 1) = InvisibleCollision) then
  begin
    Result.Visible := False;
    Result.Collision := True;
  end;
  //parse placeholder name
  NameLength := Length(Node.X3DName);
  Result.Placeholder := GetPlaceholder;
  //parse parameters
  if AnsiContainsText(Node.X3DName, ParameterMarker) then
  begin
    if GetMarker(RandomMarker) <> '' then
      Result.Rand := StrToInt(GetMarker(RandomMarker)) / 100
    else
      Result.Rand := 1;
    Result.Symmetry := StrToInt(GetMarker(SymmetryMarker));
    Result.DeviationAngle := StrToInt(GetMarker(AngleDeviationMarker)) / 180 * Pi;
    Result.Name := GetMarker(NameMarker);
    Result.Trigger := GetMarker(TriggerMarker);
  end;

  {StopProfiler}
end;

{------------------------------------------------------------------------------}

function IsPlaceholder(const Node: TX3DNode): boolean;
begin
  Result := False;
  //at this moment only TransformNodes can be placeholders!
  if not (Node is TTransformNode) then
    exit;

  if (Copy(Node.X3DName, 1, 1) = PlaceHolderMarker) or
    (Copy(Node.X3DName, 2, 1) = PlaceHolderMarker) then
    Result := True;
end;



end.
