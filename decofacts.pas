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

{ Temporary(?) unit "loads" facts and loadscreen images }
unit decofacts;

{$INCLUDE compilerconfig.inc}
interface

uses fgl,
  decotranslation;

type DFact = class
  value: string;
end;

Type TFactList = specialize TFPGObjectList<DFact>;

var facts_text: array of string;
    image_text: array of string;

var N_facts: integer;
    N_images: integer;
    LastFact: integer = -1;
    FactsFrequency: array of integer;

procedure LoadFacts;
procedure DestroyFacts;
function GetRandomFact: string;
function GetRandomFactImage: string;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses decoglobal;

function GetRandomFact: string;
var newFact: integer;
begin
  repeat
    NewFact:=rnd.random(N_facts-1);
  until (NewFact <> LastFact) and (rnd.random < 1/factsFrequency[newFact]);
  inc(FactsFrequency[newFact],7);                                     //todo balance facts frequency, now chance is 1,1/8,1/15,1/22...
  result := Facts_text[newFact];
  LastFact := newFact;
end;

{-----------------------------------------------------------------------------}

var LoadImageOld: integer=-1;
function GetRandomFactImage: string;
var LoadImageNew: integer;
begin
  repeat
    LoadImageNew := rnd.random(N_Images);
  until LoadImageOld <> LoadImageNew;
  LoadImageOld := LoadImageNew;
  result := image_text[loadImageNew];
end;

{---------------------------------------------------------------------------------}

procedure LoadFacts;
var i: integer;
begin
  N_facts := 56+1;

  setLength(facts_text,N_facts);
  Facts_text[00] := 'В алфавите Кэрф около 2 тысяч согласных без единой гласной. Количество же слов повседневного обихода достигает 9 миллиардов.';
  Facts_text[01] := 'Тэсс имеют на пальцах вибросы, которые позволяют им производить операции даже вслепую.';
  Facts_text[02] := 'Асэк является первой расой в Галактике Диадема, разработавшей технологию передвижения со скоростями выше скорости света.';
  Facts_text[03] := 'Вегетос не нуждаются в комфорте и могут регенерировать при дневном свете.';
  Facts_text[04] := 'Басс могут осознано фиксировать суставы в конечностях и таким образом они становятся свободны от тремора.';
  Facts_text[05] := 'В то время, как колющее оружие лучше пробивает доспехи, рубящее оружие наносит больший урон.';
  Facts_text[06] := 'Пикирующих пртивников невозможно атаковать врукопашную, необходимо либо использовать дистанционное оруже, либо ждать шанса для контратаки.';
  Facts_text[07] := 'Парализованные персонажи и персонажи в нок-ауте являются лёгкой целью для противника.';
  Facts_text[08] := 'Персонаж может активно сопротивляться внешнему воздействию, что в свою очередь будет требовать концентрации его внимания и сил.';
  Facts_text[09] := 'Глобальные модификаторы влияют на персонажа в целом, модификаторы действия влияют лишь на одно конкретное действие.';
  Facts_text[10] := 'Кэрф могут позиционировать свои руки с субмикронной точностью, что даёт им возможность производить сложный ремонт без дополнительных инструментов.';
  Facts_text[11] := 'Велокс не теряют сознание при остановке сердца или других критических травмах.';
  Facts_text[12] := 'Если здоровье персонажа падает ниже нуля - это клиническая смерть, необходимы немедленные реанимационные мероприятия.';
  Facts_text[13] := 'При смене текущего действия или цели готовность персонажа несколько уменьшается.';
  Facts_text[14] := 'Полученный урон, активные негативные эффекты мешают персонажу выполнять свои действия.';
  Facts_text[15] := 'Чем больше активных перков добавлено к действию, тем оно сильнее, однако это существенно выматывает персонажа.';
  Facts_text[16] := 'Персонажи могут проводить научные и практические исследования на отдыхе, чтобы продвигаться по сюжету и изучать новые перки.';
  Facts_text[17] := 'Персонажи могут обучать друг друга новым перкам, что ускоряет процесс обучения и позитивно влияет на отношения в команде.';
  Facts_text[18] := 'Зрение - основной, но не единственный орган чувств, в критической ситуации на помощь придут обоняние и слух.';
  Facts_text[19] := 'Боевая нагрузка уменьшает эффективность действий персонажа. Походная нагрузка не мешает ведению боя, но утомляет в пути.';
  Facts_text[20] := 'Личные вещи персонажа (экипированные и распиханные по карманам) составляют боевую нагрузку. Персонаж может использовать их в бою.';
  Facts_text[21] := 'Плохая погода, низкая температура, опасность нападения - всё это негативно влияет на комфортность отдыха.';
  Facts_text[22] := 'Большинство квестов из разных возможных путей прохождения игры не исключают друг друга.';
  Facts_text[23] := 'Кэрф имеют твёрдый хитиновый панцирь, обеспечивающий дополнительную защиту в бою.';
  Facts_text[24] := 'Тэсс привычны к боли, а также хорошо сопротивляются ядам.';
  Facts_text[25] := 'Асэк имеют бонус к получаемому опыту, а также бонус к научным исследованиям.';
  Facts_text[26] := 'Басс являются прирождёнными лидерами. Наличие сильного лидера в команде позитивно влияет на эффективность её членов.';
  Facts_text[27] := 'Инициатор научных исследований задаёт перспективу, в то время, как вся команда продвигает текущий прогресс, который не может её превысить.';
  Facts_text[28] := 'Сильный воин со слабым оружием не способен нанести существенного урона, также и слабый воин с сильным оружием неэффективен.';
  Facts_text[29] := 'Если что-то кажется слишком легко, ищите подвох.';
  Facts_text[30] := 'Характеристика "сила" влияет на наносимый персонажем урон в рукопашном бою.';
  Facts_text[31] := 'Характеристика "выносливость" влияет на здоровье и запас сил персонажа, а также на допустимую боевую нагрузку.';
  Facts_text[32] := 'Характеристика "ловкость" определяет шанс попасть по цели в рукопашном бою, а также поразить её уязвимые точки, если они известны.';
  Facts_text[33] := 'Характеристика "изворотливость" позволяет персонажу уворачиваться от ударов и выстрелов противника.';
  Facts_text[34] := 'Характеристика "скорость" несколько влияет на скорость всех действий персонажа.';
  Facts_text[35] := 'Характеристика "интеллект" определяет в первую очередь понимание физико-математических наук, а также количество опыта, которое получает персонаж.';
  Facts_text[36] := 'Характеристика "пространственное мышление" позволяет персонажу составлять сложные химические вещества, а также атаковать и избегать множественных целей в бою одновременно.';
  Facts_text[37] := 'Характеристика "интуиция" определяет возможность персонажа понимать сложные системы, такие как биологоические объекты, заодно повышая его сопротивление урону.';
  Facts_text[38] := 'Метафизика даёт персонажу сверхъестественные понимание и контроль над внешним миром, иногда до такой степени, которая может встретиться лишь в сказке...';
  Facts_text[39] := 'Количество опыта, получаемого за уничтожение одинаковых противников, уменьшается, однако при этом увеличивается шанс нанесения им критических ударов.';
  Facts_text[40] := 'Характеристика "меткость" определяет точность и скорость выстрелов, которые может сделать персонаж.';
  Facts_text[41] := 'Тэсс могут использовать рентгеновское зрение, что при всех его преимуществах, пагубно влияет на здоровье персонажа.';
  Facts_text[42] := 'Вы можете настроить систему жизнеобеспечения тэсс через соответствующее меню "тех.обслуживание".';
  Facts_text[43] := 'Велокс является единственной расой Галактики, которая имеет существенное преимущество в условиях низкой освещённости.';
  Facts_text[44] := 'В Галактике Диадема есть 6 цивилизаций, а также недавно была обнаружена одна вымершая раса - падда.';
  Facts_text[45] := 'Родной мир цивилизации вегетос был уничтожен в Галактической Войне, что вынудило Древо Вегетос прекратить военные действия.';
  Facts_text[46] := 'Способности к звукоподражанию кэрф даже меньше, чем у вегетос. Как и у вегетос, некоторая часть родного языка кэрф имеет химическую природу.';
  Facts_text[47] := 'Родной мир цивилизации асэк пережил несколько глобальных экологических катастроф. В некоторые времена даже были побаивания, что планета станет не способной поддерживать жизнь.';
  Facts_text[48] := 'При получении уровня персонаж автоматически получает 5 бонусных очков к характеристикам, определяемым его профессией и расой. Ещё 5 очков можно распределить вручную.';
  Facts_text[49] := 'В то время как раса и пол персонажа определяют начальное значение и скорость развития характеристик, его профессия определяет их возможный максимум.';
  Facts_text[50] := 'Межгалактический полёт длился чуть менее 700 лет на средней скорости почти в 5000 раз превосходящей скорость света. На корабле за это время прошло 9 лет.';
  Facts_text[51] := 'Нанимаемые персонажи всегда соответствуют среднему уровню команды.';
  Facts_text[52] := 'Цивилизация басс меньше других пострадала в Галактической Войне, что в большей степени определило их лидирующее место в Содружестве.';
  Facts_text[53] := 'Разные перки имеют различные требования к характеристикам персонажа, не удовлетворение которых снижает эффективность действия.';
  Facts_text[54] := 'Гиперсветовой тоннель является областью с разрежённым вакуумом, что позволяет физическим объектам перемещаться в нём существенно быстрее скорости света в обычном вакууме.';
  Facts_text[55] := 'Гиперсветовые тоннели формируются путём резонансного взрыва звёздных ядер.';
  Facts_text[56] := 'Подготовка к межгалактическому полёту проходила боеле 100 лет.';

  setLength(FactsFrequency, N_Facts);
  for i := low(FactsFrequency) to high(FactsFrequency) do FactsFrequency[i] := 1;

  {---}

  N_images := 43+1;

  setLength(image_text,N_images);
  image_text[00] := 'colour-of-nature-fractal_CC0_by_Sharon_Apted_[colorize].jpg';
  image_text[01] := 'lovely-image_CC0_by_Sharon_Apted_[GMIC].jpg';
  image_text[02] := 'pink-fractal-13086661465Zb_CC0_by_Sharon_Apted_[crop].jpg';
  image_text[03] := 'alien-worm_CC0_by_Piotr_Siedlecki_[invert,colorize].jpg';
  image_text[04] := 'angry-mantis_CC0_by_Sharon_Apted_[GMIC].jpg';
  image_text[05] := 'beige-fractal_CC0_by_Sharon_Apted_[GMIC,colorize].jpg';
  image_text[06] := 'blue-fractal-1307520582C9u_CC0_by_Sharon_Apted_[colorize,glow].jpg';
  image_text[07] := 'colour-of-nature_CC0_by_Sharon_Apted_[GMIC,colorize].jpg';
  image_text[08] := 'feathery-fractal-1308665767ois_CC0_by_Sharon_Apted_[Gimp,colorize,glow].jpg';
  image_text[09] := 'fractal-1305618518XpG_CC0_by_Sharon_Apted_[glow].jpg';
  image_text[10] := 'fractal-plate-like-image_CC0_by_Sharon_Apted_[glow,colorize].jpg';
  image_text[11] := 'fractal-ball-3_CC0_by_Piotr_Siedlecki_[glow,colorize].jpg';
  image_text[12] := 'fractal-spirals-1441742946ko2_CC0_by_Piotr_Siedlecki_[gmic,glow].jpg';
  image_text[13] := 'fractal-splash-1441596688e0H_CC0_by_Piotr_Siedlecki_[colorize].jpg';
  image_text[14] := 'pink-fantasy_CC0_by_Sharon_Apted_[gimp].jpg';
  image_text[15] := 'pretty-fractal-1307075682Q9V_CC0_by_Sharon_Apted_[colozize].jpg';
  image_text[16] := 'simple-fractal-13356910404Li_CC0_by_Sharon_Apted_[gimp].jpg';
  image_text[17] := 'simply-green_CC0_by_Sharon_Apted_[crop].jpg';
  image_text[18] := 'white-fractal-flower_CC0_by_Piotr_Siedlecki_[crop].jpg';
  image_text[19] := 'hintergrund-tapete-1456752859roe_CC0_by_kai_Stachowiak.jpg';
  image_text[20] := 'a-flame-fractal_CC0_by_Sharon_Apted_[gmic].jpg';
  image_text[21] := 'beautiful-fractal-1309767713KZe_CC0_by_Sharon_Apted_[crop,gmic].jpg';
  image_text[22] := 'colourful-fractal-1357555007a2W_CC0_by_Sharon_Apted_[gmic].jpg';
  image_text[23] := 'colourful-star_CC0_by_Sharon_Apted_[gmic].jpg';
  image_text[24] := 'cosmic-adventure_CC0_by_Sharon_Apted_[glow,color].jpg';
  image_text[25] := 'feather-duster_CC0_by_Sharon_Apted_[glow].jpg';
  image_text[26] := 'fractal-2014-1_CC0_by_Claudette_Gallant_[crop].jpg';
  image_text[27] := 'fractal-1309767060SE3_CC0_by_Sharon_Apted_[GIMP,GMIC].jpg';
  image_text[28] := 'fractal-13075203504gu_CC0_by_Sharon_Apted_[glow,gmic].jpg';
  image_text[29] := 'fractal-disks_CC0_by_Piotr_Siedlecki_[colorize].jpg';
  image_text[30] := 'fractal-shield_CC0_by_Piotr_Siedlecki_[gmic,glow,crop].jpg';
  image_text[31] := 'fractal-spiral-1401066127NAn_CC0_by_Piotr_Siedlecki_[crop,glow].jpg';
  image_text[32] := 'rose-window-1397763406a2Y_CC0_by_Piotr_Siedlecki_[colorize].jpg';
  image_text[33] := 'turquoise-fractal_CC0_by_Sharon_Apted_[gmic,crop,colorize].jpg';
  image_text[34] := 'golden-spiral_by_Piotr_Siedlecki_[crop].jpg';
  image_text[35] := 'red-orange-and-yellow-kaleidoscope_CC0_by_Michelle_Arena_[crop,softglow].jpg';
  image_text[36] := 'red-swirl-in-the-dark_CC0_by_Lynn_Greyling_[crop,glow].jpg';
  image_text[37] := 'white-rotation-on-black_CC0_by_Lynn_Greyling_[gimp,gmic,crop].jpg';
  image_text[38] := 'violet-tunnel_by_Piotr_Siedlecki_[gimp,crop].jpg';
  image_text[39] := 'yellow-spiral_by_Piotr_Siedlecki_[gimp,crop].jpg';
  image_text[40] := 'SunFlare_CC0_by-GIMP.jpg';
  image_text[41] := 'LensFlare_CC0_by-GIMP.jpg';
  image_text[42] := 'Milky_Way_2005_CC0_by_NASA_[glow,crop].jpg';
  image_text[43] := 'Ocean_planet1_CC0_by_Merikanto_[gimp,gmic].jpg';
end;

{---------------------------------------------------------------------------------}

procedure DestroyFacts;
begin
  setLength(facts_text,0);
  setLength(FactsFrequency,0);
end;

end.

