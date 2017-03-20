unit blendercleaner;

{$mode objfpc}{$H+}

interface

uses
  X3DNodes;

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

implementation

uses CastleVectors, StrUtils;

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

end.

