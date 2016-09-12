unit DecoLabel;

{$mode objfpc}{$H+}

interface

uses
  {Classes, SysUtils}
  DecoFont, CastleFonts, CastleVectors,
  CastleLog, castleFilesUtils;

{HTML disabled at the moment}
Type DLabel=class(TObject)
  x,y,w,h:integer;
  text:String;
  color:TVector4Single;
  Font:TTextureFont;
  constructor Create;
  procedure DrawMe;
end;


implementation

constructor DLabel.create;
begin
  inherited;
  Color:=Vector4Single(1,1,1,1);
end;

procedure DLabel.DrawMe;
begin
  if font<>nil then begin
    //Font.PrintBrokenString(text,w,x,y,true,0);
    Font.PrintBrokenString(x,y,Color,text,w,true,1,false);
{    function PrintBrokenString(X0, Y0: Integer; const Color: TCastleColor;
      const S: string; const MaxLineWidth: Integer;
      const PositionsFirst: boolean;
      const LineSpacing: Integer;
      const Html: boolean = false): Integer;}
  end else writelnLog('DLabel.DrawMe','ERROR: no font');
end;

end.

