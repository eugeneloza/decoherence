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

{ x3d file loading and basic processing routines.
  The unit name is not good, should change it to something more informative}
unit deco3dload;

{$INCLUDE compilerconfig.inc}

interface

uses X3DNodes,
  decoglobal;

{ extension of Castle Game Engine Load3D, automatically clears garbage
  of blender x3d exporter and adds requested anisortopic filtering}
function LoadBlenderX3D(URL: string): TX3DRootNode;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, StrUtils,
  x3dload, castleLog, castlevectors;

var TextureProperties: TTexturePropertiesNode;

procedure MakeDefaultTextureProperties;
begin
  {$PUSH}{$WARN 6018 OFF} //hide "unreachable code" warning, it's ok here
  {freeandnil?}
  if anisotropic_smoothing > 0 then begin
    textureProperties := TTexturePropertiesNode.Create;
    TextureProperties.AnisotropicDegree := anisotropic_smoothing;
    TextureProperties.FdMagnificationFilter.Value := 'DEFAULT';
    TextureProperties.FdMinificationFilter.Value := 'DEFAULT';
  end else TextureProperties := nil;
  {$POP}
end;

{-----------------------------------------------------------------------------}

{maybe, a better name would be nice.
 attaches texture properties (anisotropic smoothing) to the texture of the object.
 TODO: Normal map still doesn't work. I should fix it one day...}
procedure AddMaterial(Root: TX3DRootNode);
  procedure ScanNodesRecoursive(source: TAbstractX3DGroupingNode);
  var i: integer;
  begin
    for i := 0 to source.FdChildren.Count-1 do
    if source.FdChildren[i] is TAbstractX3DGroupingNode then
      ScanNodesRecoursive(TAbstractX3DGroupingNode(source.FdChildren[i]))
    else
      if (source.FdChildren[i] is TShapeNode) then
        try
          // assign TextureProperties (anisotropic smoothing) for the imagetexture
          (TShapeNode(source.FdChildren[i]).fdAppearance.Value.FindNode(TImageTextureNode,false) as TImageTextureNode).FdTextureProperties.Value := TextureProperties;
          // set material ambient intensity to zero for complete darkness :)
          // maybe, make a list of links to implement night vision
          (TShapeNode(source.FdChildren[i]).FdAppearance.Value.FindNode(TMaterialNode,false) as TMaterialNode).AmbientIntensity := 0;
        except
          writeLnLog('ScanRootRecoursive','try..except fired');
        end;
  end;
begin
  ScanNodesRecoursive(Root);
end;

{---------------------------------------------------------------------------}

{ Cleans up blender exporter garbage
  Use CleanWorld = true to delete camera, navigation and background
  Use CleanUnitTransform = true to delete unit TransformNodes
  (use the last option it only if you know what you're doing!)
  total gain after cleaning up is ~ 0.5% less RAM consumption
  + the same order speed gain

  Warning: this is a destructive operation! It will overwrite the Root node.

  can be used both as a function and as a procedure:
  Root := CleanUp(Load3D(filename));
  or
  Root := Load3D(filename); CleanUp(Root);}
function CleanUp(Root: TX3DRootNode; CleanWorld: boolean = false; CleanUnitTransform: boolean = false): TX3DRootNode;
  //recoursively clean the given node
  function CleanRecoursive(parent: TAbstractX3DGroupingNode; child: TX3DNode): boolean;
  var i: integer;
      RemoveNodeOnly, RemoveAll: boolean;

    //if given vector is zero translation
    function ZeroVector(a: TVector3Single): boolean;
    begin
      result := (Abs(a[0]  ) < SingleEqualityEpsilon) and
                (Abs(a[1]  ) < SingleEqualityEpsilon) and
                (Abs(a[2]  ) < SingleEqualityEpsilon)
    end;
    //if given vector is unit scale
    function UnitVector(a: TVector3Single): boolean;
    begin
      result := (Abs(a[0]-1) < SingleEqualityEpsilon) and
                (Abs(a[1]-1) < SingleEqualityEpsilon) and
                (Abs(a[2]-1) < SingleEqualityEpsilon)
    end;
    //if given vector is zero rotation
    function NoRotation(a: TVector4Single): boolean;
    begin
      //zero rotation angle is absolutely enough.
      result := Abs(a[3]) < SingleEqualityEpsilon
    end;
  begin
    result := false;

    RemoveNodeOnly := false;
    RemoveAll := false;

    //clean Background, Navigation and Camera if needed.
    if CleanWorld then
      if (child is TBackgroundNode) or
         (child is TNavigationInfoNode) or
         ((child is TTransformNode) and (AnsiContainsText(child.x3dName,'Camera_'))) then RemoveAll := true;

    if RemoveAll then begin
      //remove the node completely
      if parent<>nil then begin
        parent.FdChildren.remove(child);
        result := true; //changed the fdChildren.count of parent
        //freeandnil(child); //????
      end;
    end
    else
      if child is TAbstractX3DGroupingNode then begin

        {blender exporter garbage these two nodes are absolutely useless}
        if AnsiContainsText(child.x3dName,'group_ME_') or
           AnsiContainsText(child.x3dName,'_ifs_TRANSFORM') then
             RemoveNodeOnly := true else RemoveNodeOnly := false;

        { There is a problem when removing unit TransformNode.
          The node itself might be a placeholder and not its transform
          but its name carries information. However, we can't
          automatically tell that. So this is a "risky" option. }
        if (CleanUnitTransform) and (child is TTransformNode) then
          if ZeroVector(TTransformNode(child).Translation) and
             UnitVector(TTransformNode(child).Scale) and
             NoRotation(TTransformNode(child).rotation) then RemoveNodeOnly := true;

        //repeat...until because fdChildren.count can change during the runtime!
        i := 0;
        if TAbstractX3DGroupingNode(child).FdChildren.count>0 then
        repeat
          if not CleanRecoursive(TAbstractX3DGroupingNode(child),TAbstractX3DGroupingNode(child).FdChildren[i])
           then inc(i);
          //if result was true then TAbstractX3DGroupingNode(child).FdChildren[i] has just been removed
        until i >= TAbstractX3DGroupingNode(child).FdChildren.count;

        if parent<> nil then
          if RemoveNodeOnly then begin
            //move this node's children up one level
            for i := 0 to TAbstractX3DGroupingNode(child).FdChildren.count-1 do
              parent.FdChildren.add(TAbstractX3DGroupingNode(child).FdChildren[i]);
            //and delete the node;
            parent.FdChildren.Remove(child);
            result := true; //changed the fdChildren.count of parent
            //freeandnil(child); //????
          end

      end;

  end;
begin
  // recoursively scan the node and remove garbage
  CleanRecoursive(nil,root);
  Result := root;
end;

{---------------------------------------------------------------------------}

function LoadBlenderX3D(URL: string): TX3DRootNode;
begin
  result := CleanUp(load3D(URL),true,true);
  AddMaterial(result);
end;


initialization
  MakeDefaultTextureProperties;


end.

