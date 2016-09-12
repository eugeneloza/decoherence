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
var DummyImage:TGrayscaleAlphaImage;
    i,maxh,iteration:integer;
begin
  //if GImage<>nil then GImage.Free;   //todo: memory leaks here!
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
end;

procedure DLabel.DrawMe;
begin
  if (GImage<>nil) then begin
    GImage.color:=color;
    GImage.Draw(x,y);
  end else writelnLog('DLabel.DrawMe','ERROR: no font');
end;

end.

