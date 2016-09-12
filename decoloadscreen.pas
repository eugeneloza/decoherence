unit decoloadscreen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleLog, CastleControls, CastleImages, CastleWindow, castleFilesUtils,
  castleVectors,
  global_var, DecoFont;

const LoadScreenFolder='loadscreen'+pathdelim;

var loadscreen_img, Loadscreen_wind1,Loadscreen_wind2: TCastleImageControl;
    Loadscreen_label,Loadscreen_facts: TCastleLabel;
    LoadScreen_ready:boolean=false; //safeguard agains accidental errors


procedure MakeLoadScreen;
Procedure DestroyLoadScreen;
//procedure updateprogress;

implementation

Procedure OnLoadScreenResize(Container: TUIContainer);
begin
  WritelnLog('LoadScreen','LoadScreenResize');
  WritelnLog('LoadScreen','wind1');
  Loadscreen_wind1.Image.Resize(window.width*2, window.Height,  riBilinear);
  WritelnLog('LoadScreen','wind2');
  Loadscreen_wind2.Image.Resize(window.width*2, window.Height,  riBilinear);

  WritelnLog('LoadScreen','img1');
  if Loadscreen_img.Image<>nil then
    Loadscreen_img.Image.Resize(round(loadscreen_img.image.Width/loadscreen_img.image.height*window.Height), window.Height,  riBilinear);
  WritelnLog('LoadScreen','next');
  Loadscreen_img.left:=0;
  Loadscreen_img.bottom:=0;

  loadscreen_label.Left:=32;
  loadscreen_label.bottom:=window.Height-loadscreen_label.calculatedheight-32;

  loadscreen_facts.Left:=window.width - loadscreen_facts.calculatedWidth - 32;
  loadscreen_facts.bottom:=32;

end;


{--------------------------------------------------------------------------------}

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
    LastFact:integer=-1;

procedure TLoadImageThread.execute;
begin
  LoadImageThreadReady:=false;
  WritelnLog('TLoadImageThread.execute','Image thread started.');
  loadscreen_img.image:=LoadImage(ApplicationData(LoadScreenFolder+LoadImageString), [TRGBAlphaImage]) as TRGBAlphaImage;
  Loadscreen_img.Image.Resize(round(loadscreen_img.image.Width/loadscreen_img.image.height*window.Height), window.Height,  riBilinear);
  WritelnLog('TLoadImageThread.execute','Image thread finished.');
  LoadImageThreadReady:=true;
end;

{----------------------------------------------------------------------}

procedure NewLoadScreenFact;
const N_facts=40;
var newFact:integer;
    Fact_line1,Fact_line2:string;
begin
  repeat
    NewFact:=random(N_facts)+1;
  until NewFact<>LastFact;
  case NewFact of
    1: begin
         fact_line1:='В алфавите Кэрф около 2 тысяч согласных без единой гласной.';
         fact_line2:='Количество же слов повседневного обихода достигает 9 миллиардов.';
       end;
    2: begin
         fact_line1:='Тэсс имеют на пальцах вибросы, которые позволяют им';
         fact_line2:='производить операции даже вслепую.';
       end;
    3: begin
         fact_line1:='Асэк является первой расой в Галактике, разработавшей';
         fact_line2:='технологию передвижения со скоростями выше скорости света.';
       end;
    4: begin
         fact_line1:='Вегетос не нуждаются в комфорте и';
         fact_line2:='могут регенерировать при дневном свете.';
       end;
    5: begin
         fact_line1:='Басс могут осознано фиксировать суставы в конечностях';
         fact_line2:='и таким образом они становятся свободны от тремора.';
       end;
    6: begin
         fact_line1:='В то время, как колющее оружие лучше пробивает доспехи,';
         fact_line2:='рубящее оружие наносит больший урон.';
       end;
    7: begin
         fact_line1:='Пикирующих пртивников невозможно атаковать врукопашную, необходимо';
         fact_line2:='использовать либо дистанционное оруже, либо ждать шанса для контратаки.';
       end;
    8: begin
         fact_line1:='Парализованные персонажи и персонажи в нок-ауте';
         fact_line2:='являются лёгкой целью для противника.';
       end;
    9: begin
         fact_line1:='Персонаж может активно сопротивляться внешнему воздействию,';
         fact_line2:='что в свою очередь будет требовать концентрации его внимания и сил.';
       end;
    10: begin
         fact_line1:='Глобальные модификаторы влияют на персонажа в целом,';
         fact_line2:='Модификаторы действия влияют лишь на одно конкретное действие.';
       end;
    11: begin
         fact_line1:='Кэрф могут позиционировать свои руки с субмикронной точностью,';
         fact_line2:='что даёт им возможность производить сложный ремонт без дополнительных инструментов.';
       end;
    12: begin
         fact_line1:='Велокс не теряют сознание';
         fact_line2:='при остановке сердца.';
       end;
    13: begin
         fact_line1:='Если здоровье персонажа падает ниже нуля - это клиническая смерть,';
         fact_line2:='необходимы немедленные реанимационные мероприятия.';
       end;
    14: begin
         fact_line1:='При смене текущего действия или цели';
         fact_line2:='готовность персонажа несколько уменьшается.';
       end;
    15: begin
         fact_line1:='Полученный урон, активные негативные эффекты';
         fact_line2:='мешают персонажу выполнять свои действия.';
       end;
    16: begin
         fact_line1:='Чем больше активных перков добавлено к действию, тем оно сильнее,';
         fact_line2:='однако это существенно выматывает персонажа.';
       end;
    17: begin
         fact_line1:='Проводите научные и практические исследования на отдыхе,';
         fact_line2:='чтобы продвигаться по сюжету и изучать новые перки.';
       end;
    18: begin
         fact_line1:='Персонажи могут обучать друг друга новым перкам,';
         fact_line2:='что ускоряет процесс обучения и позитивно влияет на отношения в команде.';
       end;
    19: begin
         fact_line1:='Зрение - основной, но не единственный орган чувств,';
         fact_line2:='в критической ситуации на помощь придут обоняние и слух.';
       end;
    20: begin
         fact_line1:='Боевая нагрузка уменьшает эффективность действий персонажа.';
         fact_line2:='Походная нагрузка не мешает ведению боя, но утомляет в пути.';
       end;
    21: begin
         fact_line1:='Личные вещи персонажа (экипированные и распиханные по карманам)';
         fact_line2:='составляют боевую нагрузку. Персонаж может использовать их в бою.';
       end;
    22: begin
         fact_line1:='Плохая погода, низкая температура, опасность нападения -';
         fact_line2:='всё это негативно влияет на комфортность отдыха.';
       end;
    23: begin
         fact_line1:='Большинство квестов из разных возможных путей';
         fact_line2:='прохождения игры не исключают друг друга.';
       end;
    24: begin
         fact_line1:='Кэрф имеют твёрдый хитиновый панцирь,';
         fact_line2:='обеспечивающий дополнительную защиту в бою.';
       end;
    25: begin
         fact_line1:='Тэсс привычны к боли, а также';
         fact_line2:='хорошо сопротивляются ядам.';
       end;
    26: begin
         fact_line1:='Асэк имеют бонус к получаемому опыту,';
         fact_line2:='а также бонус к научным исследованиям.';
       end;
    27: begin
         fact_line1:='Басс являются прирождёнными лидерами. Наличие сильного лидера';
         fact_line2:='в команде позитивно влияет на эффективность её членов.';
       end;
    28: begin
         fact_line1:='Инициатор научных исследований задаёт перспективу, в то время, как';
         fact_line2:='вся команда продвигает текущий прогресс, который не может её превысить.';
       end;
    29: begin
         fact_line1:='Сильный воин со слабым оружием не способен нанести существенного урона,';
         fact_line2:='также и слабый воин с сильным оружием неэффективен.';
       end;
    30: begin
         fact_line1:='Если что-то кажется слишком легко,';
         fact_line2:='ищите подвох.';
       end;
    31: begin
         fact_line1:='Сила влияет на наносимый персонажем';
         fact_line2:='урон в рукопашном бою.';
       end;
    32: begin
         fact_line1:='Выносливость влияет на здоровье и запас сил персонажа,';
         fact_line2:='а также на допустимую боевую нагрузку.';
       end;
    33: begin
         fact_line1:='Ловкость определяет шанс попасть по цели в рукопашном бою,';
         fact_line2:='а также поразить её уязвимые точки, если они известны.';
       end;
    34: begin
         fact_line1:='Изворотливость позволяет персонажу';
         fact_line2:='уворачиваться от ударов и выстрелов противника.';
       end;
    35: begin
         fact_line1:='Скорость несколько влияет на скорость';
         fact_line2:='всех действий персонажа.';
       end;
    36: begin
         fact_line1:='Интеллект определяет в первую очередь понимание физико-математических';
         fact_line2:='наук, а также количество опыта, которое получает персонаж.';
       end;
    37: begin
         fact_line1:='Пространственное мышление позволяет персонажу составлять сложные химические';
         fact_line2:='вещества, а также атаковать и избегать множественных целей в бою одновременно.';
       end;
    38: begin
         fact_line1:='Интуиция определяет возможность персонажа понимать сложные системы, такие как';
         fact_line2:='биологоические объекты, заодно повышая его сопротивление урону.';
       end;
    39: begin
         fact_line1:='Метафизика даёт персонажу сверхъестественные понимание и контроль над внешним миром,';
         fact_line2:='иногда до такой степени, которая может встретиться лишь в сказке...';
       end;
    40: begin
         fact_line1:='Количество опыта получаемого за уничтожение одинаковых противников уменьшается,';
         fact_line2:='однако при этом увеличивается шанс нанесения им критических ударов.';
       end;
    else WritelnLog('NewLoadScreenFact','ERROR. out of N_facts');
  end;
  LastFact:=newFact;
  loadscreen_facts.text.text:=Fact_line1;
  loadscreen_facts.text.Add(Fact_line2);
end;

{---------------------------------------------------------------------------------}

procedure NewLoadScreenImage;
const N_LoadScrenImages=19;
var s:string;
    LoadImageNew:integer;
begin
  LoadImageThreadReady:=false;
  LoadImageReady:=false;
  WritelnLog('NewLoadScreenImage','Resetting image.');
  //if loadscreen_img<>nil then WritelnLog('NewLoadScreenImage','Error: image already exists.');
  freeandnil(loadscreen_img);
  loadscreen_img:=TCastleImageControl.create(Window);
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
    else WritelnLog('NewLoadScreenImage','ERROR. out of N_LoadScrenImages');
  end;
  WritelnLog('NewLoadScreenImage','Selected '+s+'.');
  LoadImageOld:= LoadImageNew;
  LoadImageString:=s;

  LoadImageThread:=TLoadImageThread.Create(true);
  LoadImageThread.FreeOnTerminate:=true;
  LoadImageThread.Priority:=tpLower;
  LoadImageThread.Start;

  NewLoadScreenFact;
end;

{-----------------------------------------------------------------------------}

procedure OnLoadScreenTimer;
var phase:single;
begin
  If LoadScreen_ready then begin
    if LoadImageThreadReady then begin
      Window.Controls.InsertBack(loadscreen_img);
      OnLoadScreenResize(nil);
      LoadImageThreadReady:=false;
      LoadImageReady:=true;
    end;
    if (LoadImageReady) and (loadScreen_img.image<>nil) then begin
      LoadScreen_img.left:=LoadScreen_img.left+1;
      phase:=abs(sin(Pi*LoadScreen_img.Left/(window.width-LoadScreen_img.image.Width)));
      (LoadScreen_img.Image as TRGBAlphaImage).ClearAlpha(round(255*phase));
      LoadScreen_facts.Color:=vector4Single(1,1,1,phase);
      LoadScreen_facts.bottom:=32+LoadScreen_img.Left div 3;
      if LoadScreen_img.Left+LoadScreen_img.image.Width>=window.width then NewLoadScreenImage;
    end;

    LoadScreen_wind1.left:=LoadScreen_wind1.left-1;
    if -LoadScreen_wind1.Left>=LoadScreen_wind1.image.Width div 2 then LoadScreen_wind1.left:=0;
    LoadScreen_wind2.left:=LoadScreen_wind2.left-2;
    if -LoadScreen_wind2.Left>=LoadScreen_wind2.image.Width div 2 then LoadScreen_wind2.left:=0;

    phase:=sqr(sin(5*2*Pi*LoadScreen_wind1.Left/(LoadScreen_wind1.image.Width)));
    (LoadScreen_wind1.Image as TRGBAlphaImage).ClearAlpha(50+round(11*Phase));
    (LoadScreen_wind2.Image as TRGBAlphaImage).ClearAlpha(50-round(11*Phase));

    loadscreen_wind1.imageChanged;
    loadscreen_wind2.imageChanged;
    loadscreen_IMG.ImageChanged;
  end else WritelnLog('OnLoadScreenTimer','Error: LoadScreen is not ready.');

end;

{-----------------------------------------------------------------------------}


Procedure DestroyLoadScreen;
begin
  WritelnLog('DestroyLoadScreen','Freeing all...');
  //Window.controls.Clear;
  freeandnil(Loadscreen_wind1);
  freeandnil(Loadscreen_wind2);
  freeandnil(Loadscreen_img);
  freeandnil(loadscreen_label);
  freeandnil(loadscreen_facts);
  LoadScreen_ready:=false;
  application.OnTimer:=nil;
  Window.OnResize:=nil;
  WritelnLog('DestroyLoadScreen','Done.');
end;

{-----------------------------------------------------------------------------}

procedure MakeLoadScreen;
begin
  WritelnLog('MakeLoadScreen','Initialize...');
  WritelnLog('MakeLoadScreen','Reading "Wind" image.');
  if Loadscreen_wind1<>nil then WritelnLog('NewLoadScreenImage','Error: wind image already exists.');
  Loadscreen_wind1:=TCastleImageControl.create(Window);
  Loadscreen_wind1.image:=LoadImage(ApplicationData(LoadScreenFolder+'WindClouds1_GIMP.jpg'), [TRGBAlphaImage]) as TRGBAlphaImage;
  (LoadScreen_wind1.Image as TRGBAlphaImage).ClearAlpha(50);
  if Loadscreen_wind2<>nil then WritelnLog('NewLoadScreenImage','Error: wind image already exists.');
  Loadscreen_wind2:=TCastleImageControl.create(Window);
  Loadscreen_wind2.image:=LoadImage(ApplicationData(LoadScreenFolder+'WindClouds2_GIMP.jpg'), [TRGBAlphaImage]) as TRGBAlphaImage;
  (LoadScreen_wind2.Image as TRGBAlphaImage).ClearAlpha(50);
  Loadscreen_wind1.left:=0;
  Loadscreen_wind1.bottom:=0;
  Loadscreen_wind2.left:=0;
  Loadscreen_wind2.bottom:=0;

  WritelnLog('MakeLoadScreen','Making labels.');
  loadscreen_label:=TCastleLabel.create(Window);
  loadscreen_label.text.text:='Добро пожаловать в Decoherence :)';
  loadscreen_label.text.Add('Идёт загрузка, подождите...');
  loadscreen_label.CustomFont:=RegularFont16;

  loadscreen_facts:=TCastleLabel.create(Window);
  LoadScreen_facts.CustomFont:=RegularFont16;

  NewLoadScreenImage;

WritelnLog('MakeLoadScreen','Adding controls.');
  Window.Controls.InsertFront(loadscreen_wind1);
  Window.Controls.InsertFront(loadscreen_wind2);
  Window.Controls.InsertFront(loadscreen_label);
  Window.Controls.InsertFront(loadscreen_facts);

  WritelnLog('MakeLoadScreen','Tune the events.');
  LoadScreen_ready:=true;
  application.TimerMilisec:=1000 div 60; //60 fps
  application.OnTimer:=@OnLoadScreenTimer;
  Window.OnResize:=@OnLoadScreenResize;
  OnLoadScreenResize(nil);
end;

end.

