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
  { Just black background with no frame }
  BlackFrame: DRectagonalFrame;

  StatBarsFrame: DRectagonalFrame;

  CharacterBar_Top, CharacterBar_Mid, CharacterBar_Bottom,
  PortraitFrame_Left, PortraitFrame_Right,
  DecorationFrame1_Left, DecorationFrame1_Right,
  DecorationFrame2_Left, DecorationFrame2_Right,
  DecorationFrame2_BottomLeft, DecorationFrame2_BottomRight,
  DecorationFrame3_Bottom
                                                         : DRectagonalFrame;
  Portrait_img: array of TCastleImage; //todo!!!
  DamageOverlay_Img: TCastleImage;
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

  BlackFrame := DRectagonalFrame.Create('blackframe.png',0,0,0,1);

  InitCompositeInterface;

  WriteLnLog('InitInterface','finished');
end;

procedure InitCompositeInterface;
var i: integer;
    s: string;
    fName: string;
begin
  HpBarImage := LoadImageSafe(ApplicationData(ProgressBarFolder+'hp_bar_CC-BY-SA_by_Saito00.png'));
  StaBarImage := LoadImageSafe(ApplicationData(ProgressBarFolder+'en_bar_CC-BY-SA_by_Saito00.png'));
  CncBarImage := LoadImageSafe(ApplicationData(ProgressBarFolder+'m_bar_CC-BY-SA_by_Saito00.png'));
  MphBarImage := LoadImageSafe(ApplicationData(ProgressBarFolder+'mph_bar_CC-BY-SA_by_Saito00.png'));

  DamageOverlay_Img := LoadImageSafe(ApplicationData(DamageFolder+'damageOverlay_CC0_by_EugeneLoza[GIMP].png'));

  Setlength(Portrait_Img,20);
  for i := 0 to Length(Portrait_Img)-1 do begin
    s := IntToStr(i+1);
    if i+1<10 then s := '0'+s;
    fName := PortraitFolder+'UNKNOWN_p'+s+'.jpg';
    try
      Portrait_Img[i] := LoadImageSafe(ApplicationData(fName));
    except
      { If the file does not exist, load the placeholder portrait.
        This is signalled by EFOpenError now, although in the future LoadImage may re-raise
        it as some EImageLoadError descendant. }
      on EFOpenError do begin
        Portrait_Img[i] := LoadImageSafe(ApplicationData(PortraitFolder+'placeholder.png'));
        WriteLnLog('DecoInterfaceLoader>InitCompositeInterface','ERROR loading portrait '+fName);
      end;
      on EImageLoadError do begin
        Portrait_img[i] := LoadImageSafe(ApplicationData(PortraitFolder+'placeholder.png'));
        WriteLnLog('DecoInterfaceLoader>InitCompositeInterface','ERROR loading portrait '+fName);
      end;
    end;
  end;

  StatBarsFrame := DRectagonalFrame.Create('blackframe.png',0,0,0,1);

  {load artwork by Saito00}
  PortraitFrame_Left := DRectagonalFrame.Create('frameborder_left_CC-BY-SA_by_Saito00.png',4,4,3,4);
  PortraitFrame_Right := DRectagonalFrame.Create('frameborder_right_CC-BY-SA_by_Saito00.png',4,4,4,3);
  DecorationFrame1_Left := DRectagonalFrame.Create('frame_1_left_CC-BY-SA_by_Saito00.png',3,23,2,6);
  DecorationFrame1_Right := DRectagonalFrame.Create('frame_1_right_CC-BY-SA_by_Saito00.png',3,23,6,2);
  DecorationFrame2_Left := DRectagonalFrame.Create('frame_2_left_CC-BY-SA_by_Saito00.png',6,9,2,6);
  DecorationFrame2_Right := DRectagonalFrame.Create('frame_2_right_CC-BY-SA_by_Saito00.png',6,9,6,2);
  DecorationFrame2_Bottomleft := DRectagonalFrame.Create('frame_2_bottomleft_CC-BY-SA_by_Saito00.png',6,2,0,9);
  DecorationFrame2_Bottomright := DRectagonalFrame.Create('frame_2_bottomright_CC-BY-SA_by_Saito00.png',6,2,9,0);
  DecorationFrame3_Bottom := DRectagonalFrame.Create('frame_3_bottom_CC-BY-SA_by_Saito00.png',10,9,23,23);

  Characterbar_Top := DRectagonalFrame.Create('character_bar_top_CC-BY-SA_by_Saito00.png',5,5,4,4);
  Characterbar_Mid := DRectagonalFrame.Create('character_bar_mid_CC-BY-SA_by_Saito00.png',0,0,4,4);
  Characterbar_Bottom := DRectagonalFrame.Create('character_bar_bottom_CC-BY-SA_by_Saito00.png',5,5,4,4);
end;

procedure DestroyCompositeInterface;
var i: integer;
begin
  WritelnLog('DestroyCompositeInterface','(todo)');
  FreeAndNil(HpBarImage);
  FreeAndNil(StaBarImage);
  FreeAndNil(CncBarImage);
  FreeAndNil(MphBarImage);
  FreeAndNil(DamageOverlay_Img);
  for i := 0 to Length(Portrait_Img)-1 do
    FreeAndNil(Portrait_Img[i]);
  Portrait_Img := nil;
end;


end.

