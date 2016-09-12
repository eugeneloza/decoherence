unit DecoFont;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef Android}
  castletexturefont_linbiolinumrg_16
  {$else}
  CastleFonts, CastleUnicode,
  {$endif}
  CastleStringUtils,
  CastleLog, castleFilesUtils;

const NormalFontFile='fonts/LinBiolinum_R_G.ttf';

var MyCharSet:TUnicodeCharList;
  RegularFont16:TTextureFont;

procedure InitializeFonts;

implementation

procedure InitializeFonts;
begin
   {$IfDef Android}
   RegularFont16:=TTextureFont.Create(TextureFont_LinBiolinumRG_16);
   {$else}
   if MyCharSet=nil then begin
      MyCharSet:=TUnicodeCharList.Create;
      MyCharSet.add(SimpleAsciiCharacters);
      MyCharSet.add('śЁЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮёйцукенгшщзхъфывапролджэячсмитьбюІЇЄіїє');
   end;
   RegularFont16:=TTextureFont.Create(ApplicationData(NormalFontFile),16,true,MyCharSet);
   {$endif}
   WritelnLog('DecoFont:initialization','Fonts loaded successfully.');
end;


initialization
  InitializeFonts;
end.

