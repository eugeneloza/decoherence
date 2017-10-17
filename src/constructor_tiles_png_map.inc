{this is a write-and-forget procedure. So moved it to an external file.
 Do not change it if its working. Not worth it.
 TODO: BUT IT ISN'T WORKING AS EXPECTED}

procedure TDungeonTilesEditor.MakePNGMap;
var PNG_Tileset, PNG_Up, PNG_Down, PNG_UpDown, PNG_Map: TRGBAlphaImage;
    ix,iy,iz: integer;
    tx,ty: integer;    // tile position in a PNG file
    Corner_tl,Corner_tr,Corner_bl,Corner_br: boolean; // are the Corners filled? TL = top left, BR = bottom right etc,
    ArrowUp,ArrowDown: boolean;

    function CheckCorner(Cornerx,Cornery,Cornerz: integer; FaceA,FaceB: TAngle): boolean;
      function GetCornerFace(cox,coy,coz: integer; MyFace: TAngle): boolean;
        begin
         GetCornerFace := false;
         if TileM.IsSafe(cox,coy) then begin
           if TileM.MapSafeFace(cox,coy,coz,MyFace) = tfWall
           then
             GetCornerFace := true
           else
           if TileM.MapSafeFace(cox+a_dx(MyFace),coy+a_dy(MyFace),coz,invertAngle(MyFace)) = tfWall
           then
             GetCornerFace := true;
         end
         else
             GetCornerFace := true;  //not sure about this line
       end;
    begin
     Result := GetCornerFace(Cornerx+a_dx(FaceA),Cornery+a_dy(FaceA),Cornerz, FaceB) or
               GetCornerFace(Cornerx+a_dx(FaceB),Cornery+a_dy(FaceB),Cornerz, FaceA);
    end;

begin
 if (not isTileLoaded) or (TileM = nil) then begin
   dLog(LogConstructorError,Self,'TDungeonTilesEditor.MakePNGMap','No tile loaded. Aborting');
   Exit;
 end;

 if not TileM.Blocker then with TileM do begin
   //I don't care about efficiency or memory use here. It's just to make the tiles once.
   PNG_Tileset := LoadImage(ConstructorData(LocalFolder+'template_CC0_by_caeles.png',false),        [TRGBAlphaImage]) as TRGBAlphaImage;
   PNG_Up      := LoadImage(ConstructorData(LocalFolder+'template_CC0_by_caeles_up.png',false),     [TRGBAlphaImage]) as TRGBAlphaImage;
   PNG_Down    := LoadImage(ConstructorData(LocalFolder+'template_CC0_by_caeles_down.png',false),   [TRGBAlphaImage]) as TRGBAlphaImage;
   PNG_UpDown  := LoadImage(ConstructorData(LocalFolder+'template_CC0_by_caeles_updown.png',false), [TRGBAlphaImage]) as TRGBAlphaImage;
   //create map image.
   for iz:=0 to Sizez-1 do begin
    PNG_Map := TRGBAlphaImage.Create;
    PNG_Map.setsize((SizeX)*16,(SizeY)*16,1);
    PNG_Map.Clear(Vector4Byte(0,0,0,0));
    for ix:=0 to SizeX-1 do
     for iy:=0 to SizeY-1 do with Map[ix,iy,iz] do begin
       if Base <> tkNone then begin
       //No-Corner tiles
       if (Faces[aTop] = tfWall) and (Faces[aBottom] = tfWall) then begin
         if (Faces[aLeft] = tfWall) then begin
           // |=
           tx:=2;ty:=1;
           if (Faces[aRight] = tfWall) then begin
             //all blocked
             tx:=1;ty:=1;
           end;
         end else if (Faces[aRight] = tfWall) then begin
           // =|
           tx:=5;ty:=7;
         end else begin
           // =
           tx:=5;ty:=6;
         end;
       end else if (Faces[aLeft] = tfWall) and (Faces[aRight] = tfWall) then begin
         if (Faces[aTop] = tfWall) then begin
           // П
           tx:=1;ty:=2;
         end else if (Faces[aBottom] = tfWall) then begin
           // |_|
           tx:=7;ty:=5;
         end else begin
           // ||
           tx:=6;ty:=5;
         end;
       end else begin
         //Corner_tiles;
         //I calculate all Corners just for 'less code' reason
         Corner_TL := CheckCorner(ix,iy,iz,aTop,aLeft);
         Corner_TR := CheckCorner(ix,iy,iz,aTop,aRight);
         Corner_BL := CheckCorner(ix,iy,iz,aBottom,aLeft);
         Corner_BR := CheckCorner(ix,iy,iz,aBottom,aRight);

         if Faces[aLeft] = tfWall then begin
           if Faces[aTop] = tfWall then begin
             //Г
             if Corner_BR then begin
               tx :=4 ; ty := 5;
             end else begin
               tx := 2; ty := 2;
             end;
           end else if Faces[aBottom] = tfWall then begin
             //|_
             if Corner_TR then begin
               tx := 1; ty := 7;
             end else begin
               tx := 6; ty := 7;
             end;
           end else begin
             // |:
             if Corner_TR and Corner_BR then begin
               tx := 1; ty := 3;
             end else if Corner_TR then begin
               tx := 1; ty := 4;
             end else if Corner_BR then begin
               tx := 1; ty := 6;
             end else begin
               tx := 1; ty := 5;
             end;
           end;
         end else if Faces[aRight] = tfWall then begin
           if Faces[aTop] = tfWall then begin
             //7
             if Corner_BL then begin
               tx := 7; ty := 1;
             end else begin
               tx := 7; ty := 6;
             end;
           end else if Faces[aBottom] = tfWall then begin
             //_|
             if Corner_TL then begin
               tx := 5; ty := 5;
             end else begin
               tx := 7; ty := 7;
             end;
           end else begin
             // :|
             if Corner_TL and Corner_BL then begin
               tx := 7; ty := 4;
             end else if Corner_TL then begin
               tx := 7; ty := 2;
             end else if Corner_BL then begin
               tx := 7; ty := 3;
             end else begin
               tx := 3; ty := 5;
             end;
           end;
         end else begin
           if Faces[aTop] = tfWall then begin
             //--
             //..
             if Corner_BL and Corner_BR then begin
               tx := 3; ty := 1;
             end else if Corner_BL then begin
               tx := 4; ty := 1;
             end else if Corner_BR then begin
               tx := 6; ty := 1;
             end else begin
               tx := 5; ty := 1;
             end;
           end else if Faces[aBottom] = tfWall then begin
             //..
             //__
             if Corner_TL and Corner_TR then begin
               tx := 4; ty := 7;
             end else if Corner_TL then begin
               tx := 2; ty := 7;
             end else if Corner_TR then begin
               tx := 3; ty := 7;
             end else begin
               tx := 4; ty := 4;
             end;
           end else begin
             //all empty
             if Corner_TL and Corner_TR then begin
               if Corner_BL then begin
                 //three Corners TL TR BL
                 tx := 6; ty := 6;
                 if Corner_BR then begin
                   //all Corners
                   tx := 4; ty := 6;
                 end;
               end else if Corner_BR then begin
                 //three Corners TL TR BR
                 tx := 3; ty := 2;
               end else begin
                 //only upper Corners
                 tx := 2; ty := 4;
               end;
             end else if Corner_BL and Corner_BR then begin
               if Corner_TL then begin
                 //three Corners BL BR TL
                 tx := 2; ty := 3;
               end else if Corner_TR then begin
                 //three Corners BL BR TR
                 tx := 6; ty := 4;
               end else begin
                 //only lower Corners
                 tx := 5; ty := 4;
               end;
             end else if Corner_TL then begin
               if Corner_BR then begin
                 // "\"
                 tx := 3; ty := 4;
               end else if Corner_BL then begin
                 // ": "
                 tx :=4 ; ty := 2;
               end else begin
                 //only single Corner
                 tx := 4; ty := 3;
               end;
             end else if Corner_TR then begin
               if Corner_BL then begin
                 // "/"
                 tx := 3; ty := 3;
               end else if Corner_BR then begin
                 // " :"
                 tx := 3; ty := 6;
               end else begin
                 //only single Corner
                 tx := 6; ty := 2;
               end;
             end else if Corner_BL then begin
               //just a single variant left
               tx := 2; ty := 6;
             end else if Corner_BR then begin
               //just a single variant left
               tx := 6; ty := 3;
             end else begin
               //all empty, no Corners
               tx := 5; ty := 2;
             end;
           end;
         end;
       end;
     end else begin
       //n/a tile (transparent)
       tx := 7; ty := 5;
     end;

     //now, check if there are stairs up/down nearby?
     ArrowUp := false;
     ArrowDown := false;
     if Base = tkUp then ArrowUp:=true;
     if Base = tkDown then ArrowDown:=true;

     if isPassable(Map[ix,iy,iz].Faces[aLeft]) then begin
       if MapSafeBase(ix-1,iy,iz) = tkUp then ArrowUp := true;
       if MapSafeBase(ix-1,iy,iz) = tkDown then ArrowDown := true;
     end;
     if isPassable(Map[ix,iy,iz].Faces[aRight]) then begin
       if MapSafeBase(ix+1,iy,iz) = tkUp then ArrowUp := true;
       if MapSafeBase(ix+1,iy,iz) = tkDown then ArrowDown := true;
     end;
     if isPassable(Map[ix,iy,iz].Faces[aTop]) then begin
       if MapSafeBase(ix,iy-1,iz) = tkUp then ArrowUp := true;
       if MapSafeBase(ix,iy-1,iz) = tkDown then ArrowDown := true;
     end;
     if isPassable(Map[ix,iy,iz].Faces[aBottom]) then begin
       if MapSafeBase(ix,iy+1,iz) = tkUp then ArrowUp := true;
       if MapSafeBase(ix,iy+1,iz) = tkDown then ArrowDown := true;
     end;

     //place the tile at tx,ty to the resulting image;
      if ArrowUp and ArrowDown then
        PNG_Map.DrawFrom(PNG_UpDown,(ix)*16,PNG_Map.Height-(iy+1)*16,(tx-1)*16,PNG_UpDown.Height-(ty)*16,16,16,dmBlendSmart)
      else if ArrowUp then
        PNG_Map.DrawFrom(PNG_Up,(ix)*16,PNG_Map.Height-(iy+1)*16,(tx-1)*16,PNG_Up.Height-(ty)*16,16,16,dmBlendSmart)
      else if ArrowDown then
        PNG_Map.DrawFrom(PNG_Down,(ix)*16,PNG_Map.Height-(iy+1)*16,(tx-1)*16,PNG_Down.Height-(ty)*16,16,16,dmBlendSmart)
      else
        PNG_Map.DrawFrom(PNG_Tileset,(ix)*16,PNG_Map.Height-(iy+1)*16,(tx-1)*16,PNG_Tileset.Height-(ty)*16,16,16,dmBlendSmart);

    end;

    SaveImage(PNG_Map,ConstructorData(TilesFolder+TileName+'_'+IntToStr(iz)+'.png',false));
    FreeAndNil(PNG_Map);
   end;

   FreeAndNil(PNG_Tileset);
   FreeAndNil(PNG_Up);
   FreeAndNil(PNG_Down);
   FreeAndNil(PNG_UpDown);

 end else with TileM.Map[0,0,0] do begin
   if IsPassable(Faces[aTop]) then
     PNG_Map := LoadImage(ConstructorData(LocalFolder+'blocker_bottom.png',false), [TRGBAlphaImage]) as TRGBAlphaImage
   else
   if IsPassable(Faces[aBottom]) then
     PNG_Map := LoadImage(ConstructorData(LocalFolder+'blocker_top.png',false), [TRGBAlphaImage]) as TRGBAlphaImage
   else
   if IsPassable(Faces[aLeft]) then
     PNG_Map := LoadImage(ConstructorData(LocalFolder+'blocker_right.png',false), [TRGBAlphaImage]) as TRGBAlphaImage
   else
   if IsPassable(Faces[aRight]) then
     PNG_Map := LoadImage(ConstructorData(LocalFolder+'blocker_left.png',false), [TRGBAlphaImage]) as TRGBAlphaImage;

   if PNG_Map <> nil then begin
     SaveImage(PNG_Map,ConstructorData(TilesFolder+TileName+'_'+IntToStr(0)+'.png',false));
     FreeAndNil(PNG_Map);
   end
   else raise Exception.Create('TDungeonTilesEditor.MakePNGMap blocker png is nil!');
 end;
end;
