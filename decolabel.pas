unit DecoLabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleFonts, CastleVectors, CastleFontFamily,
  DecoFont,
  CastleLog, castleFilesUtils,
  CastleImages, CastleGLImages,
  decointerface,
  decoglobal;

{HTML disabled at the moment}
Type DLabel=class(DAbstractElement)
 public
  text: string;            //todo read ftext write ftext
  color:TVector4Single;
  Font:DFont;
  Shadow:Float;
  constructor Create(AOwner:TComponent); override;
  procedure DrawMe; override;
  procedure InitGL;
 private
  BrokenString:DStringList;
  //R_Text: TRichText;
  GImage:TGLImage;
end;


implementation

constructor DLabel.create(AOwner:TComponent);
begin
  inherited create(AOwner);
  Color:=Vector4Single(1,1,1,1);
  Shadow:=0;
end;

procedure DLabel.InitGL;
var DummyImage,ShadowImage,resultImage:TGrayscaleAlphaImage;
    i,maxh{,maxw},iteration:integer;
    P: PVector2Byte;
begin
  //if GImage<>nil then GImage.Free;
  FreeAndNil(BrokenString);
  brokenString:=DStringList.create;
  BrokenString:=font.Break_String(text,w);
  maxh:=0;
  //maxw:=0;
  for i:=0 to BrokenString.count-1 do begin
    if maxh<BrokenString[i].height then maxh:=BrokenString[i].height;
    //if maxw<s[i].width then maxw:=s[i].width;
  end;
  //  h:=maxh*BrokenString.Count;

  if shadow=0 then
    DummyImage:=font.broken_string_to_image(BrokenString)
  else
    DummyImage:=font.broken_string_to_image_with_shadow(BrokenString,shadow,3);

  h:=DummyImage.height;

  GImage:=TGLImage.create(DummyImage,true,true);
  freeAndNil(DummyImage);

{  freeandnil(R_text);
  R_Text := TRichText.Create(font, text, true); ///html capable
  try
    R_Text.Wrap(w);
    h:=font.RowHeight*(R_Text.Count-1);
  finally {FreeAndNil(R_Text);} end;
  writelnLog('DLabel.CalculateHeight','height ='+inttostr(h)); }
end;

procedure DLabel.DrawMe;
var i:integer;
    t:TDAteTime;
begin
  if (GImage<>nil){ and (R_Text<>nil)} then begin
 {   if shadow>0 then begin
      R_text.print(x+1,y-1,Vector4Single(0,0,0,shadow*color[3]),0);
      R_text.print(x+2,y-2,Vector4Single(0,0,0,shadow/2*color[3]),0);
      R_text.print(x-1,y+1,Vector4Single(0,0,0,shadow/3*color[3]),0);
    end;
    R_text.print(x,y,Color,0); ///html capable }
{    t:=now;
    for i:=1 to 10000 do begin}
      GImage.color:=color;
      GImage.Draw(x,y);
{    end;
    writelnLog('','gl = '+floattostr((now-t)*24*60*60*1000)+' ms');
    t:=now;
    for i:=1 to 10000 do begin
      Font.PrintBrokenString(x,y,Color,text,w,true,0,true);
    end;
    writelnLog('','brks = '+floattostr((now-t)*24*60*60*1000)+' ms');}

  end else writelnLog('DLabel.DrawMe','ERROR: no font');
end;

end.

