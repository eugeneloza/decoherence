unit DecoLabel;

{$mode objfpc}{$H+}

interface

uses
  {Classes,} SysUtils,
  DecoFont, CastleFonts, CastleVectors, CastleFontFamily,
  CastleLog, castleFilesUtils;

{HTML disabled at the moment}
Type DLabel=class(TObject)
 public
  x,y,w,h:integer;
  text: string;            //todo read ftext write ftext
  color:TVector4Single;
  Font:TTextureFont;
  constructor Create;
  procedure DrawMe;
  procedure calculateHeight;
end;


implementation

constructor DLabel.create;
begin
  inherited;
  Color:=Vector4Single(1,1,1,1);
end;

procedure DLabel.CalculateHeight;
var R_Text: TRichText;
begin
  R_Text := TRichText.Create(font, text, true); ///html capable
  try
    R_Text.Wrap(w);
    h:=font.RowHeight*(R_Text.Count-1);
  finally FreeAndNil(R_Text); end;
  writelnLog('DLabel.CalculateHeight','height ='+inttostr(h));
end;

procedure DLabel.DrawMe;
begin
  if font<>nil then begin
    Font.PrintBrokenString(x,y,Color,text,w,true,0,true); ///html capable
  end else writelnLog('DLabel.DrawMe','ERROR: no font');
end;

end.

