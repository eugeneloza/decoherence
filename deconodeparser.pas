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
unit deconodeparser;

{$INCLUDE compilerconfig.inc}
interface

uses X3DNodes,
  decoglobal;

type
  {information on the node name parsing}
  DNodeInfo = record
    {does this node collide with the actors? (default=false)}
    collision: boolean;
    {is this node visible? (default=true)}
    visible: boolean;
    {random chance to be placed (default=1.0)}
    rand: float;
    {symmetry group / autoassigned; -1 is no symmetry}
    symmetry: integer;
    {possible deviation angle in radians (default = 0.0)}
    deviationangle: float;
    {placeholder marker}
    placeholder: string;
    {name marker}
    name: string;
    {trigeger marker name}
    trigger: string;
  end;

{temporary?
 if this node is a placeholder}
function IsPlaceholder(node: TX3DNode): boolean;

function ParseNode(node: TX3DNode): DNodeInfo;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses strutils, CastleLog;

{eeeem? there should have been this procedure in System unit?}
function StrToInt(v: string): integer;
var e: integer;
begin
  if v='' then
    Result :=0
  else begin
    val(v,Result,e);
    if e<>0 then WriteLnLog('deconodeparser.StrToInt','Invalid integer: '+v);
  end;
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

const CollisionSymbol = '*';  //visible=true, collision=true (default is visible=true, collision=false)
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

function ParseNode(node: TX3DNode): DNodeInfo;
var NameLength: integer; //for optimization
  function GetMarker(marker: string): string;
  var i: integer;
      s: string;
      finish: boolean;
  begin
    i := AnsiIndexText(node.X3DName,marker);
    result := '';
    if i>=0 then begin
      i += length(marker)-1; //skip the marker
      repeat
        inc(i);
        s := copy(node.X3DName,i,1); //copy a char
        finish := (s=ParameterMarker) or (s=SeparatorMarker);
        if not finish then
          result := result + s;
      until finish or (i>=NameLength);
    end;
  end;
  function GetPlaceholder: string;
  var j: integer;
      s: string;
      finish: boolean;
  begin
    j := -1;
    if (copy(node.X3DName,1,1) = PlaceHolderMarker) then j := 1; //start of the placeholder signature
    if (copy(node.X3DName,2,1) = PlaceHolderMarker) then j := 2;
    result := '';
    if j>0 then begin
      repeat
        inc(j);
        s := copy(node.X3DName,j,1); //copy a char
        finish := (s=PlaceHolderEndMarker);
        if not finish then
          result := result + s;
      until finish or (j>=NameLength);
      if s<>PlaceHolderEndMarker then begin
        WriteLnLog('ParseNode.GetPlaceholder','ERROR: Could not find end of the placeholder: '+node.X3DName);
        //result := '';
      end;
    end;
  end;

begin
  with Result do begin
    //parse visible/collision state
    visible := true;
    collision := false;
    if (copy(node.X3DName,1,1) = CollisionSymbol) then collision := true;
    if (copy(node.X3DName,1,1) = InvisibleCollision) then begin
      visible := false;
      collision := true;
    end;
    //parse placeholder name
    NameLength := length(node.X3DName);
    placeholder := getPlaceholder;
    //parse parameters
    if AnsiContainsText(node.X3DName,ParameterMarker) then begin
      if GetMarker(RandomMarker)<>'' then
        rand := StrToInt(GetMarker(RandomMarker))/100;
      else
        rand := 1;
      Symmetry := StrToInt(GetMarker(SymmetryMarker));
      deviationangle := StrToInt(GetMarker(AngleDeviationMarker))/180*Pi;
      name := GetMarker(NameMarker);
      trigger := GetMarker(TriggerMarker);
    end;
  end;
end;

{------------------------------------------------------------------------------}

function IsPlaceholder(node: TX3DNode): boolean;
begin
  Result := false;
  //at this moment only TransformNodes can be placeholders!
  if not (node is TTransformNode) then exit;

  if (copy(node.X3DName,1,1) = PlaceHolderMarker) or
     (copy(node.X3DName,2,1) = PlaceHolderMarker) then
    result := true;
end;



end.

