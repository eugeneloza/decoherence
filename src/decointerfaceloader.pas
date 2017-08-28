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

{ Load some content for the Interface }

unit DecoInterfaceLoader;

{$INCLUDE compilerconfig.inc}

interface
uses CastleImages, CastleVectors,
  DecoImages;


var HpBarImage, StaBarImage, CncBarImage, MphBarImage: TCastleImage; //todo not freed automatically!!!
    //StatBarsFrame: DFrame;
{  {simple outline around black box}
    //SimpleFrame,
    {a frame with 19px header}
    //CaptionFrame,
    {Just black background with no frame}
    BlackFrame: DFrame;    }
  {characterbar_top, characterbar_mid, characterbar_bottom,
  portraitframe_left, portraitframe_right,
  decorationframe1_left,decorationframe1_right,
  decorationframe2_left,decorationframe2_right,
  decorationframe2_bottomleft,decorationframe2_bottomright,
  decorationframe3_bottom
                                                         : DFrame;}
  Portrait_img: array of TCastleImage; //todo!!!
  damageOverlay_img: TCastleImage;
  { a GL shade color imposed on all interface elements }
  InterfaceColor: TVector4;



{reads some interface-related data, like loading frames images}
procedure InitInterface;
{reads some interface-related data, like loading health bars images and decoration frames}
procedure InitCompositeInterface;
procedure DestroyCompositeInterface;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses Classes, SysUtils, CastleLog, CastleFilesUtils,
  DecoInputOutput, DecoGlobal;

{-------------------- INIT INTERFACE ------------------------------------------}

procedure InitInterface;
begin
  WriteLnLog('InitInterface','started');

  InterfaceColor := Vector4(1,1,1,1);

  {BlackFrame := DFrame.Create(Window);
  with BlackFrame do begin
    SourceImage := LoadImageSafe(ApplicationData(FramesFolder+'blackframe.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    CornerTop := 0; CornerBottom := 0; CornerLeft := 0; CornerRight := 0;
  end;}

  InitCompositeInterface;

  WriteLnLog('InitInterface','finished');
end;

procedure InitCompositeInterface;
var i: integer;
    s: string;
    fname: string;
begin
  HpBarImage := LoadImageSafe(ApplicationData(ProgressBarFolder+'hp_bar_CC-BY-SA_by_Saito00.png'));
  StaBarImage := LoadImageSafe(ApplicationData(ProgressBarFolder+'en_bar_CC-BY-SA_by_Saito00.png'));
  CncBarImage := LoadImageSafe(ApplicationData(ProgressBarFolder+'m_bar_CC-BY-SA_by_Saito00.png'));
  MphBarImage := LoadImageSafe(ApplicationData(ProgressBarFolder+'mph_bar_CC-BY-SA_by_Saito00.png'));

  DamageOverlay_img := LoadImageSafe(ApplicationData(DamageFolder+'damageOverlay_CC0_by_EugeneLoza[GIMP].png'));

  {StatBarsFrame := DFrame.create(Window);
  with StatBarsFrame do begin
    SourceImage := LoadImageSafe(ApplicationData(FramesFolder+'blackframe.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    CornerTop := 0; CornerBottom := 0; cornerLeft := 0; CornerRight := 1;
  end;}

  Setlength(Portrait_img,20);
  for i := 0 to length(portrait_img)-1 do begin
    s := IntToStr(i+1);
    if i+1<10 then s := '0'+s;
    fName := PortraitFolder+'UNKNOWN_p'+s+'.jpg';
    try
      Portrait_img[i] := LoadImageSafe(ApplicationData(fName));
    except
      { If the file does not exist, load the placeholder portrait.
        This is signalled by EFOpenError now, although in the future LoadImage may re-raise
        it as some EImageLoadError descendant. }
      on EFOpenError do begin
        Portrait_img[i] := LoadImageSafe(ApplicationData(PortraitFolder+'placeholder.png'));
        WriteLnLog('DecoInterfaceLoader>InitCompositeInterface','ERROR loading portrait '+fName);
      end;
      on EImageLoadError do begin
        Portrait_img[i] := LoadImageSafe(ApplicationData(PortraitFolder+'placeholder.png'));
        WriteLnLog('DecoInterfaceLoader>InitCompositeInterface','ERROR loading portrait '+fName);
      end;
    end;
  end;
  {load artwork by Saito00}

  {portraitframe_left := DFrame.create(Window);
  with portraitframe_left do begin
    SourceImage := LoadImageSafe(ApplicationData(FramesFolder+'frameborder_left_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 4; CornerBottom := 4; cornerLeft := 3; CornerRight := 4;
  end;
  portraitframe_right := DFrame.create(Window);
  with portraitframe_right do begin
    SourceImage := LoadImageSafe(ApplicationData(FramesFolder+'frameborder_right_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 4; CornerBottom := 4; cornerLeft := 4; CornerRight := 3;
  end;

  decorationframe1_left := DFrame.create(Window);
  with decorationframe1_left do begin
    SourceImage := LoadImageSafe(ApplicationData(DecorationsFolder+'frame_1_left_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 3; CornerBottom := 23; cornerLeft := 2; CornerRight := 6;
  end;
  decorationframe1_right := DFrame.create(Window);
  with decorationframe1_right do begin
    SourceImage := LoadImageSafe(ApplicationData(DecorationsFolder+'frame_1_right_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 3; CornerBottom := 23; cornerLeft := 6; CornerRight := 2;
  end;
  decorationframe2_left := DFrame.create(Window);
  with decorationframe2_left do begin
    SourceImage := LoadImageSafe(ApplicationData(DecorationsFolder+'frame_2_left_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 6; CornerBottom := 9; cornerLeft := 2; CornerRight := 6;
  end;
  decorationframe2_right := DFrame.create(Window);
  with decorationframe2_right do begin
    SourceImage := LoadImageSafe(ApplicationData(DecorationsFolder+'frame_2_right_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 6; CornerBottom := 9; cornerLeft := 6; CornerRight := 2;
  end;
  decorationframe2_bottomleft := DFrame.create(Window);
  with decorationframe2_bottomleft do begin
    SourceImage := LoadImageSafe(ApplicationData(DecorationsFolder+'frame_2_bottomleft_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 6; CornerBottom := 2; cornerLeft := 0; CornerRight := 9;
  end;
  decorationframe2_bottomright := DFrame.create(Window);
  with decorationframe2_bottomright do begin
    SourceImage := LoadImageSafe(ApplicationData(DecorationsFolder+'frame_2_bottomright_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 6; CornerBottom := 2; cornerLeft := 9; CornerRight := 0;
  end;
  decorationframe3_bottom := DFrame.create(Window);
  with decorationframe3_bottom do begin
    SourceImage := LoadImageSafe(ApplicationData(DecorationsFolder+'frame_3_bottom_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 10; CornerBottom := 9; cornerLeft := 23; CornerRight := 23;
  end;

  characterbar_top := DFrame.create(Window);
  with characterbar_top do begin
    SourceImage := LoadImageSafe(ApplicationData(FramesFolder+'character_bar_top_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 5; CornerBottom := 5; cornerLeft := 4; CornerRight := 4;
  end;
  characterbar_mid := DFrame.create(Window);
  with characterbar_mid do begin
    SourceImage := LoadImageSafe(ApplicationData(FramesFolder+'character_bar_mid_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 0; CornerBottom := 0; cornerLeft := 4; CornerRight := 4;
  end;
  characterbar_bottom := DFrame.create(Window);
  with characterbar_bottom do begin
    SourceImage := LoadImageSafe(ApplicationData(FramesFolder+'character_bar_bottom_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;
    cornerTop := 5; CornerBottom := 5; cornerLeft := 4; CornerRight := 4;
  end;

  ActionFrame := DFrame.create(Window);
  ActionFrame.Rectagonal := false;
  ActionFrame.SourceImage := LoadImageSafe(ApplicationData(FramesFolder+'action_frame_CC-BY-SA_by_Saito00.png'),[TRGBAlphaImage]) as TRGBAlphaImage;}
end;

procedure DestroyCompositeInterface;
var i: integer;
begin
  writelnLog('DestroyCompositeInterface','(todo)');
  FreeAndNil(HpBarImage);
  FreeAndNil(StaBarImage);
  FreeAndNil(CncBarImage);
  FreeAndNil(MphBarImage);
  FreeAndNil(damageOverlay_img);
  for i := 0 to Length(Portrait_img)-1 do
    FreeAndNil(Portrait_img[i]);
  Portrait_img := nil;
end;


end.

