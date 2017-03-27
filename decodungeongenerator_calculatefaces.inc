var ix,iy,iz: integer;
    a: TAngle;
    f: TTileFace;
    tmpDock: DDockPoint;
begin
  if Dock = nil then Dock := TDockPointList.Create else Dock.Clear;
  fFreeFaces := 0;
  fVolume := 0;
  {$IFDEF CompleteGen}fHasStairsDown := false;{$ENDIF}
  //FacesList := [];
  for ix := 0 to sizex-1 do
    for iy := 0 to sizey-1 do
      for iz := 0 to sizez-1 do begin
        if isPassable(Map[ix,iy,iz].base) then inc(fVolume);
        {$IFDEF CompleteGen}if Map[ix,iy,iz].base = tkDown then fHasStairsDown := true;{$ENDIF}
        {$Warning floor/ceiling not working!}
        for a in THorizontalAngle do
          if isPassable(Map[ix,iy,iz].faces[a]) then begin
            f := MapSafeFace(ix+a_dx(a),iy+a_dy(a),iz+a_dz(a),invertAngle(a));
            if (f = tfNone) or (f = tfInacceptible) then begin
              tmpDock.face := a;
              tmpDock.facetype := Map[ix,iy,iz].faces[a];
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
  if (FreeFaces = 1) and (sizex+sizey+sizez = 3) and (Map[0,0,0].base = tkNone) then
    blocker := true
  else
    blocker := false;
  {$ENDIF}
  Result := FreeFaces;
end;
