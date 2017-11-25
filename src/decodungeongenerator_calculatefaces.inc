var ix,iy,iz: integer;
    a: TAngle;
    f: TTileFace;
    tmpDock: DDockPoint;
begin
  StartProfiler;

  if Dock = nil then Dock := TDockPointList.Create else Dock.Clear;
  fFreeFaces := 0;
  fVolume := 0;
  {$IFNDEF CompleteGen}fMaxDepth := 0;{$ENDIF}
  {$IFDEF CompleteGen}fHasStairsDown := false;{$ENDIF}
  //FacesList := [];
  for ix := 0 to sizex-1 do
    for iy := 0 to sizey-1 do
      for iz := 0 to sizez-1 do begin
        if isPassable(Map[ix,iy,iz].Base) then begin
          inc(fVolume);
          {$IFNDEF CompleteGen}if fMaxDepth<iz then fMaxDepth := iz;{$ENDIF}
        end;
        {$IFDEF CompleteGen}if Map[ix,iy,iz].Base = tkDown then fHasStairsDown := true;{$ENDIF}
        {$Warning floor/ceiling not working!}
        for a in THorizontalAngle do
          if isPassable(Map[ix,iy,iz].Faces[a]) then begin
            f := MapSafeFace(ix+a_dx(a),iy+a_dy(a),iz+a_dz(a),InvertAngle(a));
            if (f = tfNone) or (f = tfInacceptible) then begin
              tmpDock.face := a;
              tmpDock.facetype := Map[ix,iy,iz].Faces[a];
              tmpDock.x := ix;
              tmpDock.y := iy;
              tmpDock.z := iz;
              Dock.Add(tmpDock);
              //{$IFDEF CompleteGen}
              //Include(FacesList,Map[ix,iy,iz].faces[a]);
              //{$ENDIF}
              inc(fFreeFaces);
            end;
          end;
      end;
  {$IFDEF CompleteGen}
  //check if it's a blocker tile
  if (FreeFaces = 1) and (SizeX+SizeY+SizeZ = 3) and (Map[0,0,0].Base = tkNone) then
    Blocker := true
  else
    Blocker := false;
  {$ENDIF}
  Result := FreeFaces;

  StopProfiler;
end;
