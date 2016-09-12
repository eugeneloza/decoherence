unit DecoFont;

{$mode objfpc}{$H+}

interface

uses
  CastleFonts, CastleUnicode, CastleStringUtils, castleFilesUtils;

const NormalFontFile='fonts/LinBiolinum_R_G.ttf';

var MyCharSet:TUnicodeCharList;
  RegularFont16:TTextureFont;


implementation


initialization
 if MyCharSet=nil then begin
    MyCharSet:=TUnicodeCharList.Create;
    MyCharSet.add(SimpleAsciiCharacters);
    MyCharSet.add('śЁЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮёйцукенгшщзхъфывапролджэячсмитьбюІЇЄіїє');
 end;
 RegularFont16:=TTextureFont.Create(ApplicationData(NormalFontFile),16,true,MyCharSet)
end.

