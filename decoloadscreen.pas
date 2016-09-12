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
    LoadScreen_ready:boolean=false; //safeguard agains accidental errors


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
  if LoadScreen_img<> nil then LoadScreen_img.scaleMe(-1,0,true);
  if LoadScreen_wind2<> nil then LoadScreen_wind2.scaleMe(-1,-1,true);
  if LoadScreen_wind1<> nil then LoadScreen_wind1.scaleMe(-1,-1,true);
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
const N_LoadScrenImages=34;
var s:string;
    LoadImageNew:integer;
begin
  LoadImageThreadReady:=false;
  WritelnLog('NewLoadScreenImage','Resetting image.');
  freeandnil(loadscreen_img);
  LoadScreen_Img:=DStaticImage.Create;

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
      LoadScreen_img.x+=1*2;
      phase:=abs(sin(Pi*LoadScreen_img.x/(window.width-LoadScreen_img.w)));
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
  //Window.controls.Clear;
  freeandnil(Loadscreen_wind1);
  freeandnil(Loadscreen_wind2);
  freeandnil(Loadscreen_img);
  freeandnil(loadscreen_label);
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
  Loadscreen_wind1:=DWindImage.create;
  Loadscreen_wind1.LoadMe(LoadScreenFolder+'WindClouds1_GIMP.jpg');
  Loadscreen_wind1.ScaleMe(-1,-1,true);
  LoadScreen_wind1.alpha:=0.1;
  LoadScreen_wind1.phase:=random;
  Loadscreen_wind2:=DWindImage.create;
  Loadscreen_wind2.LoadMe(LoadScreenFolder+'WindClouds2_GIMP.jpg');
  Loadscreen_wind2.ScaleMe(-1,-1,true);
  LoadScreen_wind2.alpha:=0.1;
  LoadScreen_wind1.phase:=random;

  WritelnLog('MakeLoadScreen','Making labels.');
  loadscreen_label:=DLabel.create;
  loadscreen_label.text:='Добро пожаловать в Decoherence :)'+decolinebreak+'Идёт загрузка, подождите...'+decolinebreak+'П.С. пока "нечего грузить" :)'+decolinebreak+'Просто нажмите любую клавишу...';
  loadscreen_label.color:=vector4Single(1,1,1,1);
  LoadScreen_label.shadow:=1;
  loadscreen_label.Font:=RegularFont16;
  LoadScreen_label.w:=round(window.width/3);//a quick fix for 'first fact bug' parsed with w=0;
  LoadScreen_label.InitGL;

  loadscreen_facts:=DLabel.create;
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

