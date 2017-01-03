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
  x3dload, castleLog;

var TextureProperties: TTexturePropertiesNode;

procedure MakeDefaultTextureProperties;
begin
  if anisotropic_smoothing>0 then begin
    textureProperties:=TTexturePropertiesNode.Create;
    TextureProperties.AnisotropicDegree:=anisotropic_smoothing;
    TextureProperties.FdMagnificationFilter.Value:='DEFAULT';
    TextureProperties.FdMinificationFilter.Value:='DEFAULT';
  end else TextureProperties:=nil;
end;

{-----------------------------------------------------------------------------}

{ First of all this procedure fixes bugs (fetatures) of blender X3D exporter
by removing 2 garbage nodes from each object in the file
and attaches texture properties (anisotropic smoothing) to the texture of the object.
TODO: Normal map still doesn't work. I should fix it one day...}
Procedure AddChildRecoursive(target,source:TAbstractX3DGroupingNode);
var i:integer;
    tmpTransform:TTransformNode;
    tmpGroup:TGroupNode;
begin
  {Scan loaded model for it's descendants
  and add any valuable data to Tile3D
  dropping all blender exporter garbage on the way
  at this moment it's:
  "*_ifs_TRANSFORM" which is a useless unit transform
  "group_ME_*" which is a useless group transform}
  for i:=0 to source.FdChildren.Count-1 do begin
    //copy TTransformNode
    if (source.FdChildren[i] is TTransformNode) then begin
      if not AnsiContainsText(source.FdChildren[i].Name,'_ifs_TRANSFORM') then begin
        //TODO: not copy unit transforms!
        tmpTransform:=TTransformNode.Create(source.FdChildren[i].Name,'');
        tmpTransform.Translation:=(source.FdChildren[i] as TTransformNode).Translation;
        tmpTransform.Rotation:=(source.FdChildren[i] as TTransformNode).Rotation;
        tmpTransform.scale:=(source.FdChildren[i] as TTransformNode).scale;
        AddChildRecoursive(TmpTransform,source.FdChildren[i] as TTransformNode);
        target.FdChildren.add(TmpTransform);
      end else AddChildRecoursive(target,source.FdChildren[i] as TTransformNode); // drop junk exporter node
    end else
    //copy TGroupNode... Is that needed? well...let's leave it here for now
    if (source.FdChildren[i] is TGroupNode) then begin
      if not AnsiContainsText(source.FdChildren[i].Name,'group_ME_') then begin
         tmpGroup:=TGroupNode.create(source.FdChildren[i].Name,'');
         AddChildRecoursive(tmpGroup,source.FdChildren[i] as TGroupNode);
         target.FdChildren.add(tmpGroup);
      end else AddChildRecoursive(target,source.FdChildren[i] as TGroupNode); // drop junk exporter node
    end else
    //copy TShapeNode, no recoursion, just add it
    if (source.FdChildren[i] is TShapeNode) then begin
      try
        // assign TextureProperties (anisotropic smoothing) for the imagetexture
        ((source.FdChildren[i] as TShapeNode).fdAppearance.Value.FindNode(TImageTextureNode,false) as TImageTextureNode).FdTextureProperties.Value := TextureProperties;
        // set material ambient intensity to zero for complete darkness :)
        // maybe, make a list of links to implement night vision
        ((source.FdChildren[i] as TShapeNode).FdAppearance.Value.FindNode(TMaterialNode,false) as TMaterialNode).AmbientIntensity := 0;
      except
        writeLnLog('ScanRootRecoursive','try..except fired');
      end;
      target.FdChildren.add(source.FdChildren[i]);
    end else
    //copy TAbstractLightNode, no recoursion, just add it
    if (source.FdChildren[i] is TAbstractLightNode) then begin
      //TODO: add to a global list of lights
      //(source.FdChildren[i] as TAbstractLightNode).FdOn.value := true;
      target.FdChildren.add(source.FdChildren[i]);
    end;
  end;
end;

{---------------------------------------------------------------------------}

function LoadBlenderX3D(URL: string): TX3DRootNode;
var TmpRootNode:TX3DRootNode;
begin
  MakeDefaultTextureProperties;

  TmpRootNode := load3D(URL);
  result := TX3DRootNode.Create;

  { scan the X3DRootNode and copy only nodes, we need
    we also scan temporary X3DRootNode for TImageTexture derivatives
    and update them as necessary (to include anisotropic smoothing)}
  AddChildRecoursive(result,TmpRootNode);

  { hmm... that one does seem to memory leak?
    because I don't copy (link to) all the fdChildren nodes
    will they be freed automatically? }
  FreeAndNil(TmpRootNode);
end;


end.

