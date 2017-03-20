{this is a write-and-forget procedure. So moved it to an external file.
 Do not change it if its working. Not worth it.
 TODO: BUT IT ISN'T WORKING AS EXPECTED}

procedure TDungeonTilesEditor.MakePNGMap;
var PNG_tileset, PNG_up, PNG_down, PNG_updown, PNG_map: TRGBAlphaImage;
    ix,iy,iz: integer;
    tx,ty: integer;    // tile position in a PNG file
    corner_tl,corner_tr,corner_bl,corner_br: boolean; // are the corners filled? TL = top left, BR = bottom right etc,
    arrowup,arrowdown: boolean;

    function CheckCorner(cornerx,cornery,cornerz: integer; faceA,faceB: TAngle): boolean;
      function GetCornerFace(cox,coy,coz: integer; MyFace: TAngle): boolean;
        begin
         GetCornerFace := false;
         if TileM.IsSafe(cox,coy) then begin
           if TileM.TileMapSafeFace(cox,coy,coz,MyFace) = tfWall
           then GetCornerFace := true else
           if TileM.TileMapSafeFace(cox+a_dx(MyFace),coy+a_dy(MyFace),coz,invertAngle(MyFace)) = tfWall
           then GetCornerFace := true;
         end
         else   GetCornerFace := true;  //not sure about this line
       end;
    begin
     CheckCorner := GetCornerFace(cornerx+a_dx(faceA),cornery+a_dy(faceA),cornerz,faceA) or
                    GetCornerFace(cornerx+a_dx(faceB),cornery+a_dy(faceB),cornerz,faceB);
    end;

begin
 if (not isTileLoaded) or (TileM=nil) then begin
   WriteLnLog('TDungeonTilesEditor.MakePNGMap','No tile loaded. Aborting');
   exit;
 end;

 if not TileM.Blocker then with TileM do begin
   //I don't care about efficiency or memory use here. It's just to make the tiles once.
   PNG_Tileset := LoadImage(ConstructorData(LocalFolder+'template_CC0_by_caeles.png',false),        [TRGBAlphaImage]) as TRGBAlphaImage;
   PNG_UP      := LoadImage(ConstructorData(LocalFolder+'template_CC0_by_caeles_up.png',false),     [TRGBAlphaImage]) as TRGBAlphaImage;
   PNG_Down    := LoadImage(ConstructorData(LocalFolder+'template_CC0_by_caeles_down.png',false),   [TRGBAlphaImage]) as TRGBAlphaImage;
   PNG_UpDown  := LoadImage(ConstructorData(LocalFolder+'template_CC0_by_caeles_updown.png',false), [TRGBAlphaImage]) as TRGBAlphaImage;
   //create map image.
   for iz:=0 to TileSizez-1 do begin
    PNG_map:=LoadImage(ConstructorData(LocalFolder+'0.png',false), [TRGBAlphaImage]) as TRGBAlphaImage;
    PNG_map.setsize((TileSizeX)*16,(TileSizeY)*16,1);
    PNG_map.Clear(Vector4Byte(0,0,0,0));
    for ix:=0 to TileSizeX-1 do
     for iy:=0 to TileSizeY-1 do with TileMap[ix,iy,iz] do begin
       if base <> tkNone then begin
       //No-corner tiles
       if (faces[aTop] = tfWall) and (faces[aBottom] = tfWall) then begin
         if (faces[aLeft] = tfWall) then begin
           // |=
           tx:=2;ty:=1;
           if (faces[aRight] = tfWall) then begin
             //all blocked
             tx:=1;ty:=1;
           end;
         end else if (faces[aRight] = tfWall) then begin
           // =|
           tx:=5;ty:=7;
         end else begin
           // =
           tx:=5;ty:=6;
         end;
       end else if (faces[aLeft] = tfWall) and (faces[aRight] = tfWall) then begin
         if (faces[aTop] = tfWall) then begin
           // П
           tx:=1;ty:=2;
         end else if (faces[aBottom] = tfWall) then begin
           // |_|
           tx:=7;ty:=5;
         end else begin
           // ||
           tx:=6;ty:=5;
         end;
       end else begin
         //corner_tiles;
         //I calculate all corners just for 'less code' reason
         Corner_TL := CheckCorner(ix,iy,iz,aTop,aLeft);
         Corner_TR := CheckCorner(ix,iy,iz,aTop,aRight);
         Corner_BL := checkCorner(ix,iy,iz,aBottom,aLeft);
         Corner_BR := CheckCorner(ix,iy,iz,aBottom,aRight);
         if faces[aLeft] = tfWall then begin
           if faces[aTop] = tfWall then begin
             //Г
             if corner_BR then begin
               tx :=4 ; ty := 5;
             end else begin
               tx := 2; ty := 2;
             end;
           end else if faces[aBottom] = tfWall then begin
             //|_
             if corner_TR then begin
               tx := 1; ty := 7;
             end else begin
               tx := 6; ty := 7;
             end;
           end else begin
             // |:
             if corner_TR and corner_BR then begin
               tx := 1; ty := 3;
             end else if corner_TR then begin
               tx := 1; ty := 4;
             end else if corner_BR then begin
               tx := 1; ty := 6;
             end else begin
               tx := 1; ty := 5;
             end;
           end;
         end else if faces[aRight] = tfWall then begin
           if faces[aTop] = tfWall then begin
             //7
             if corner_BL then begin
               tx := 7; ty := 1;
             end else begin
               tx := 7; ty := 6;
             end;
           end else if faces[aBottom] = tfWall then begin
             //_|
             if corner_TL then begin
               tx := 5; ty := 5;
             end else begin
               tx := 7; ty := 7;
             end;
           end else begin
             // :|
             if corner_TL and corner_BL then begin
               tx := 7; ty := 4;
             end else if corner_TL then begin
               tx := 7; ty := 2;
             end else if corner_BL then begin
               tx := 7; ty := 3;
             end else begin
               tx := 3; ty := 5;
             end;
           end;
         end else begin
           if faces[aTop] = tfWall then begin
             //--
             //..
             if corner_BL and corner_BR then begin
               tx := 3; ty := 1;
             end else if corner_BL then begin
               tx := 4; ty := 1;
             end else if corner_BR then begin
               tx := 6; ty := 1;
             end else begin
               tx := 5; ty := 1;
             end;
           end else if faces[aBottom] = tfWall then begin
             //..
             //__
             if corner_TL and corner_TR then begin
               tx := 4; ty := 7;
             end else if corner_TL then begin
               tx := 2; ty := 7;
             end else if corner_TR then begin
               tx := 3; ty := 7;
             end else begin
               tx := 4; ty := 4;
             end;
           end else begin
             //all empty
             if corner_TL and corner_TR then begin
               if corner_BL then begin
                 //three corners TL TR BL
                 tx := 6; ty := 6;
                 if corner_BR then begin
                   //all corners
                   tx := 4; ty := 6;
                 end;
               end else if corner_BR then begin
                 //three corners TL TR BR
                 tx := 3; ty := 2;
               end else begin
                 //only upper corners
                 tx := 2; ty := 4;
               end;
             end else if corner_BL and corner_BR then begin
               if corner_TL then begin
                 //three corners BL BR TL
                 tx := 2; ty := 3;
               end else if corner_TR then begin
                 //three corners BL BR TR
                 tx := 6; ty := 4;
               end else begin
                 //only lower corners
                 tx := 5; ty := 4;
               end;
             end else if corner_TL then begin
               if corner_BR then begin
                 // "\"
                 tx := 3; ty := 4;
               end else if corner_BL then begin
                 // ": "
                 tx :=4 ; ty := 2;
               end else begin
                 //only single corner
                 tx := 4; ty := 3;
               end;
             end else if corner_TR then begin
               if corner_BL then begin
                 // "/"
                 tx := 3; ty := 3;
               end else if corner_BR then begin
                 // " :"
                 tx := 3; ty := 6;
               end else begin
                 //only single corner
                 tx := 6; ty := 2;
               end;
             end else if corner_BL then begin
               //just a single variant left
               tx := 2; ty := 6;
             end else if corner_BR then begin
               //just a single variant left
               tx := 6; ty := 3;
             end else begin
               //all empty, no corners
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
     arrowup := false;
     arrowdown := false;
     if base = tkUp then arrowup:=true;
     if base = tkDown then arrowdown:=true;

     if isPassable(TileMap[ix,iy,iz].faces[aLeft]) then begin
       if TileMapSafeBase(ix-1,iy,iz) = tkUp then arrowup := true;
       if TileMapSafeBase(ix-1,iy,iz) = tkDown then arrowdown := true;
     end;
     if isPassable(TileMap[ix,iy,iz].faces[aRight]) then begin
       if TileMapSafeBase(ix+1,iy,iz) = tkUp then arrowup := true;
       if TileMapSafeBase(ix+1,iy,iz) = tkDown then arrowdown := true;
     end;
     if isPassable(TileMap[ix,iy,iz].faces[aTop]) then begin
       if TileMapSafeBase(ix,iy-1,iz) = tkUp then arrowup := true;
       if TileMapSafeBase(ix,iy-1,iz) = tkDown then arrowdown := true;
     end;
     if isPassable(TileMap[ix,iy,iz].faces[aBottom]) then begin
       if TileMapSafeBase(ix,iy+1,iz) = tkUp then arrowup := true;
       if TileMapSafeBase(ix,iy+1,iz) = tkDown then arrowdown := true;
     end;

     //place the tile at tx,ty to the resulting image;
     //PNG_map.DrawFrom(PNG_Tileset,(ix-1)*16,(iy-1)*16,(tx-1)*16,(ty-1)*16,16,16,dmBlend);
     if arrowUp and arrowDown then
       PNG_map.DrawFrom(PNG_updown,(ix)*16,PNG_map.Height-(iy+1)*16,(tx-1)*16,PNG_updown.Height-(ty)*16,16,16,dmBlendSmart)
     else if arrowUp then
       PNG_map.DrawFrom(PNG_up,(ix)*16,PNG_map.Height-(iy+1)*16,(tx-1)*16,PNG_up.Height-(ty)*16,16,16,dmBlendSmart)
     else if arrowDown then
       PNG_map.DrawFrom(PNG_down,(ix)*16,PNG_map.Height-(iy+1)*16,(tx-1)*16,PNG_down.Height-(ty)*16,16,16,dmBlendSmart)
     else
       PNG_map.DrawFrom(PNG_Tileset,(ix)*16,PNG_map.Height-(iy+1)*16,(tx-1)*16,PNG_tileset.Height-(ty)*16,16,16,dmBlendSmart);
    end;
    SaveImage(PNG_map,TileName+'_'+inttostr(iz)+'.png');
    FreeAndNil(PNG_map);
   end;

   FreeAndNil(PNG_tileset);
   FreeAndNil(PNG_up);
   FreeAndNil(PNG_down);
   FreeAndNil(PNG_updown);

 end else with TileM.TileMap[0,0,0] do begin
   if IsPassable(faces[aTop]) then
     PNG_map:=LoadImage(LocalFolder+'blocker_bottom.png', [TRGBAlphaImage]) as TRGBAlphaImage
   else
   if IsPassable(faces[aBottom]) then
     PNG_map:=LoadImage(LocalFolder+'blocker_top.png', [TRGBAlphaImage]) as TRGBAlphaImage
   else
   if IsPassable(faces[aLeft]) then
     PNG_map:=LoadImage(LocalFolder+'blocker_right.png', [TRGBAlphaImage]) as TRGBAlphaImage
   else
   if IsPassable(faces[aRight]) then
     PNG_map:=LoadImage(LocalFolder+'blocker_left.png', [TRGBAlphaImage]) as TRGBAlphaImage;

   if png_map<>nil then begin
     SaveImage(PNG_map,TileName+'_'+inttostr(0)+'.png');
     FreeAndNil(PNG_map);
   end;
 end;
end;
