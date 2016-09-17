{Copyright (C) 2012-2016 Yevhen Loza

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
unit decoloadscreen;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils,
  CastleLog, CastleWindow, castleFilesUtils,
  castleVectors,
  decoimages, decoLabel,
  decoloadfacts,
  decoglobal, DecoFont;

procedure MakeLoadScreen;
Procedure DestroyLoadScreen;
//procedure updateprogress;

implementation

const LoadScreenFolder='interface/loadscreen/';
var loadscreen_img:DStaticImage;
    Loadscreen_wind1,Loadscreen_wind2: DWindImage;
    Loadscreen_label,Loadscreen_facts: DLabel;
    LoadScreen_ready:boolean=false; //safeguard against accidental errors


type TLoadImageThread = class(TThread)
  private
  protected
    procedure Execute; override;
end;
var LoadImageThreadReady:boolean=false;
    LoadImageReady:boolean=false;
    LoadImageThread:TLoadImageThread;
    LoadImageString:string;
    LoadImageOld:integer=-1;

{----------------------------------------------------------------}

Procedure OnLoadScreenResize(Container: TUIContainer);
begin
  WritelnLog('LoadScreen','LoadScreenResize');
  if LoadScreen_img<> nil then begin LoadScreen_img.scaleMe(-1); loadScreen_img.InitGL end;
  if LoadScreen_wind1<> nil then begin LoadScreen_wind1.scaleMe(-1,-1); LoadScreen_wind1.initGL end;
  if LoadScreen_wind2<> nil then begin LoadScreen_wind2.scaleMe(-1,-1); LoadScreen_wind2.initGL end;
  if loadscreen_label <>nil then begin
    loadscreen_label.w:=round(window.width/3);
    loadscreen_label.x:=32;
    loadscreen_label.y:=window.Height - loadscreen_label.h -32;
  end;
  if loadscreen_facts <>nil then begin
    loadscreen_facts.w:=round(window.width/3);
    loadscreen_facts.x:=window.width - loadscreen_facts.w - 32;
    loadscreen_facts.y:=32 {- loadscreen_facts.h};
  end;
  //if LoadImageReady then LoadScreen_img.ScaleMe(-1) else WritelnLog('OnLoadScreenResize','LoadScreen is not ready!');

end;

{--------------------------------------------------------------------------------}

procedure TLoadImageThread.execute;
begin
  WritelnLog('TLoadImageThread.execute','Image thread started.');
  LoadScreen_Img.LoadMe(LoadImageString);
  LoadScreen_Img.ScaleMe(-1);
  WritelnLog('TLoadImageThread.execute','Image thread finished.');
  LoadImageThreadReady:=true;
end;

{----------------------------------------------------------------------}

procedure NewLoadScreenImage;
const N_LoadScrenImages=42;
var s:string;
    LoadImageNew:integer;
begin
  LoadImageThreadReady:=false;
  WritelnLog('NewLoadScreenImage','Resetting image.');
  if loadscreen_img<>nil then loadscreen_img.DestroyMe;
  freeandnil(loadscreen_img);
  LoadScreen_Img:=DStaticImage.Create(Window);

  repeat
    LoadImageNew:= random(N_LoadScrenImages)+1;
  until LoadImageOld<>LoadImageNew;
  case LoadImageNew of
    1:s:='colour-of-nature-fractal_CC0_by_Sharon_Apted_[colorize].jpg';
    2:s:='lovely-image_CC0_by_Sharon_Apted_[GMIC].jpg';
    3:s:='pink-fractal-13086661465Zb_CC0_by_Sharon_Apted_[crop].jpg';
    4:s:='alien-worm_CC0_by_Piotr_Siedlecki_[invert,colorize].jpg';
    5:s:='angry-mantis_CC0_by_Sharon_Apted_[GMIC].jpg';
    6:s:='beige-fractal_CC0_by_Sharon_Apted_[GMIC,colorize].jpg';
    7:s:='blue-fractal-1307520582C9u_CC0_by_Sharon_Apted_[colorize,glow].jpg';
    8:s:='colour-of-nature_CC0_by_Sharon_Apted_[GMIC,colorize].jpg';
    9:s:='feathery-fractal-1308665767ois_CC0_by_Sharon_Apted_[Gimp,colorize,glow].jpg';
    10:s:='fractal-1305618518XpG_CC0_by_Sharon_Apted_[glow].jpg';
    11:s:='fractal-plate-like-image_CC0_by_Sharon_Apted_[glow,colorize].jpg';
    12:s:='fractal-ball-3_CC0_by_Piotr_Siedlecki_[glow,colorize].jpg';
    13:s:='fractal-spirals-1441742946ko2_CC0_by_Piotr_Siedlecki_[gmic,glow].jpg';
    14:s:='fractal-splash-1441596688e0H_CC0_by_Piotr_Siedlecki_[colorize].jpg';
    15:s:='pink-fantasy_CC0_by_Sharon_Apted_[gimp].jpg';
    16:s:='pretty-fractal-1307075682Q9V_CC0_by_Sharon_Apted_[colozize].jpg';
    17:s:='simple-fractal-13356910404Li_CC0_by_Sharon_Apted_[gimp].jpg';
    18:s:='simply-green_CC0_by_Sharon_Apted_[crop].jpg';
    19:s:='white-fractal-flower_CC0_by_Piotr_Siedlecki_[crop].jpg';
    20:s:='hintergrund-tapete-1456752859roe_CC0_by_kai Stachowiak.jpg';
    21:s:='a-flame-fractal_CC0_by_Sharon_Apted_[gmic].jpg';
    22:s:='beautiful-fractal-1309767713KZe_CC0_by_Sharon Apted_[crop,gmic].jpg';
    23:s:='colourful-fractal-1357555007a2W_CC0_by_Sharon_Apted_[gmic].jpg';
    24:s:='colourful-star_CC0_by_Sharon_Apted_[gmic].jpg';
    25:s:='cosmic-adventure_CC0_by_Sharon_Apted_[glow,color].jpg';
    26:s:='feather-duster_CC0_by_Sharon_Apted_[glow].jpg';
    27:s:='fractal-2014-1_CC0_by_Claudette Gallant_[crop].jpg';
    28:s:='fractal-1309767060SE3_CC0_by_Sharon_Apted_[GIMP,GMIC].jpg';
    29:s:='fractal-13075203504gu_CC0_by_Sharon_Apted_[glow,gmic].jpg';
    30:s:='fractal-disks_CC0_by_Piotr_Siedlecki_[colorize].jpg';
    31:s:='fractal-shield_CC0_by_Piotr_Siedlecki_[gmic,glow,crop].jpg';
    32:s:='fractal-spiral-1401066127NAn_CC0_by_Piotr_Siedlecki_[crop,glow].jpg';
    33:s:='rose-window-1397763406a2Y_CC0_by_Piotr_Siedlecki_[colorize].jpg';
    34:s:='turquoise-fractal_CC0_by_Sharon_Apted_[gmic,crop,colorize].jpg';
    35:s:='golden-spiral_by_Piotr_Siedlecki_[crop].jpg';
    36:s:='red-orange-and-yellow-kaleidoscope_CC0_by_Michelle_Arena_[crop,softglow].jpg';
    37:s:='red-swirl-in-the-dark_CC0_by_Lynn_Greyling_[crop,glow].jpg';
    38:s:='white-rotation-on-black_CC0_by_Lynn_Greyling_[gimp,gmic,crop].jpg';
    39:s:='violet-tunnel_by_Piotr_Siedlecki_[gimp,crop].jpg';
    40:s:='yellow-spiral_by_Piotr_Siedlecki_[gimp,crop].jpg';
    41:s:='SunFlare_CC0_by-GIMP.jpg';
    42:s:='LensFlare_CC0_by-GIMP.jpg';
    else WritelnLog('NewLoadScreenImage','ERROR. out of N_LoadScrenImages');
  end;
  WritelnLog('NewLoadScreenImage','Selected '+s+'.');
  LoadImageOld:= LoadImageNew;
  LoadImageString:=LoadScreenFolder+s;

  LoadImageThread:=TLoadImageThread.Create(true);
  LoadImageThread.FreeOnTerminate:=true;
  LoadImageThread.Priority:=tpLower;
  LoadImageThread.Start;

  loadscreen_facts.text:=GetRandomFact;
  loadscreen_facts.w:=round(window.width/3);//a quick fix for 'first fact bug' parsed with w=0;
  loadScreen_facts.InitGL;
end;

{-----------------------------------------------------------------------------}

var RenderReady:boolean=false;
    renderTime:float;
procedure OnLoadScreenTimer;
var phase:float;
    RenderStart:TDateTime;
begin
 if RenderReady then begin
  RenderReady:=false;
  RenderStart:=now;
  If LoadScreen_ready then begin
    if LoadImageThreadReady then begin
      LoadScreen_img.initGL;
      LoadScreen_img.Image.Color:=vector4Single(1,1,1,0);
      LoadImageThreadReady:=false;
      LoadImageReady:=true;
    end;
    if (LoadImageReady) and (loadScreen_img.image<>nil) then begin
      LoadScreen_img.x+=1{*2};
      phase:=abs(sin(Pi*LoadScreen_img.x/(window.width-LoadScreen_img.w)));
      //if random<sqrt(cos(Pi*phase)) then LoadScreen_img.x+=1;
      LoadScreen_img.image.Color:=vector4Single(1,1,1,phase*0.8);
      LoadScreen_facts.Color:=vector4Single(1,1,1,phase);
      LoadScreen_facts.y:=32+{LoadScreen_facts.h+}LoadScreen_img.x div 3;
      if LoadScreen_img.x+LoadScreen_img.w>=window.width then NewLoadScreenImage;
    end;

//    LoadScreen_wind1.phase:=0*random;
    LoadScreen_wind1.phase-=0.001*(1-random/10)*2;
    if LoadScreen_wind1.phase<0 then LoadScreen_wind1.phase+=1;
    LoadScreen_wind2.phase-=0.001*(2+random/10)*2;
    if LoadScreen_wind2.phase<0 then LoadScreen_wind2.phase+=1;

  end else WritelnLog('OnLoadScreenTimer','Error: LoadScreen is not ready.');
  RenderTime:=(RenderTime*99+(now-RenderStart)*24*60*60)/100; {render time in ms}
  RenderReady:=true;
 end;
end;

{-----------------------------------------------------------------------------}

Procedure DestroyLoadScreen;
begin
  RenderReady:=false;
  WritelnLog('DestroyLoadScreen','Freeing all...');
  if LoadScreen_wind1<> nil then Loadscreen_wind1.DestroyMe;
  freeandnil(Loadscreen_wind1);
  if Loadscreen_wind2<> nil then Loadscreen_wind2.DestroyMe;
  freeandnil(Loadscreen_wind2);
  if Loadscreen_img<> nil then Loadscreen_img.DestroyMe;
  freeandnil(Loadscreen_img);
  if loadscreen_label<> nil then loadscreen_label.DestroyMe;
  freeandnil(loadscreen_label);
  if loadscreen_facts<> nil then loadscreen_facts.DestroyMe;
  freeandnil(loadscreen_facts);
  DestroyFacts;
  LoadScreen_ready:=false;
  application.OnTimer:=nil;
  Window.OnResize:=nil;
  WritelnLog('DestroyLoadScreen','Done.');
end;

{*****************************************************************************}

var Render_finished:boolean=true;
procedure LoadScreenRender(Container: TUIContainer);
begin
 if Render_finished then begin
   render_finished:=false;
   if LoadScreen_img<>nil then LoadScreen_img.DrawMe;
   if LoadScreen_wind1<>nil then LoadScreen_Wind1.DrawMe;
   if LoadScreen_wind2<>nil then LoadScreen_wind2.DrawMe;
   if LoadScreen_label<>nil then LoadScreen_label.DrawMe;
   if loadScreen_facts<>nil then loadScreen_facts.DrawMe;
   render_finished:=true;
 end;
end;

{*****************************************************************************}

procedure MakeLoadScreen;
begin
  WritelnLog('MakeLoadScreen','Initialize...');
  RenderReady:=false;
  RenderTime:=0;

  WritelnLog('MakeLoadScreen','Reading "Wind" image.');
 // if Loadscreen_wind1<>nil then WritelnLog('NewLoadScreenImage','Error: wind image already exists.');
  Loadscreen_wind1:=DWindImage.create(Window);
  Loadscreen_wind1.LoadMe(LoadScreenFolder+'WindClouds1_GIMP.jpg');
  Loadscreen_wind1.ScaleMe(-1,-1);
  LoadScreen_wind1.initGL;
  LoadScreen_wind1.alpha:=0.1;
  LoadScreen_wind1.phase:=random;
  Loadscreen_wind2:=DWindImage.create(Window);
  Loadscreen_wind2.LoadMe(LoadScreenFolder+'WindClouds2_GIMP.jpg');
  Loadscreen_wind2.ScaleMe(-1,-1);
  LoadScreen_wind2.initGL;
  LoadScreen_wind2.alpha:=0.1;
  LoadScreen_wind1.phase:=random;

  WritelnLog('MakeLoadScreen','Making labels.');
  loadscreen_label:=DLabel.create(Window);
  loadscreen_label.text:='Добро пожаловать в Decoherence :)'+dlinebreak+'Идёт загрузка, подождите...'+dlinebreak+'П.С. пока "почти нечего грузить" :)'+dlinebreak+'Просто нажмите любую клавишу...';
  loadscreen_label.color:=vector4Single(1,1,1,1);
  LoadScreen_label.shadow:=1;
  loadscreen_label.Font:=RegularFont16;
  LoadScreen_label.w:=round(window.width/3);//a quick fix for 'first fact bug' parsed with w=0;
  LoadScreen_label.InitGL;

  loadscreen_facts:=DLabel.create(Window);
  loadscreen_facts.color:=vector4Single(1,1,1,0);
  loadscreen_facts.shadow:=1;
  LoadScreen_facts.Font:=RegularFont16;

  LoadFacts;

  NewLoadScreenImage;

  WritelnLog('MakeLoadScreen','Tune the events.');
  application.TimerMilisec:=1000 div 30; //60 fps
  application.OnTimer:=@OnLoadScreenTimer;
  Window.OnResize:=@OnLoadScreenResize;
  window.OnRender:=@LoadScreenRender;
  //OnLoadScreenResize(nil);
  LoadScreen_ready:=true;
  RenderReady:=true;
  Render_Finished:=true;
end;

end.

