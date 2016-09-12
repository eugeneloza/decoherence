unit DecoLabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  DecoFont, CastleFonts, CastleVectors, CastleFontFamily,
  CastleLog, castleFilesUtils,
  CastleImages, CastleGLImages,
  decoglobal;

{HTML disabled at the moment}
Type DLabel=class(TObject)
 public
  x,y,w,h:integer;
  text: string;            //todo read ftext write ftext
  color:TVector4Single;
  Font:DFont;
  Shadow:Float;
  constructor Create;
  procedure DrawMe;
  procedure calculateHeight;
 private
  BrokenString:DStringList;
  R_Text: TRichText;
  GImage:TGLImage;
end;


implementation

constructor DLabel.create;
begin
  inherited;
  Color:=Vector4Single(1,1,1,1);
  Shadow:=0;
end;

procedure DLabel.CalculateHeight;
var DummyImage:TGrayscaleAlphaImage;
begin
  //if GImage<>nil then GImage.Free;
  FreeAndNil(BrokenString);
  brokenString:=DStringList.create;
  BrokenString:=font.Break_String(text,w);

  DummyImage:=font.broken_string_to_image(BrokenString);
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
begin
  if (GImage<>nil){ and (R_Text<>nil)} then begin
 {   if shadow>0 then begin
      R_text.print(x+1,y-1,Vector4Single(0,0,0,shadow*color[3]),0);
      R_text.print(x+2,y-2,Vector4Single(0,0,0,shadow/2*color[3]),0);
      R_text.print(x-1,y+1,Vector4Single(0,0,0,shadow/3*color[3]),0);
    end;
    R_text.print(x,y,Color,0); ///html capable }
    GImage.color:=color;
    GImage.Draw(x,y);
  end else writelnLog('DLabel.DrawMe','ERROR: no font');
end;

end.

