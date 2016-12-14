{Copyright (C) 2012-2016 Yevhen Loza

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

{ Works with different types of labels }
unit decolabels;

{$INCLUDE compilerconfig.inc}

interface

uses classes,
  decoimages, decofont,
  decoglobal;

Type
  { a powerful text label, converted to GLImage to be extremely fast }
  DLabel = class(DAbstractImage)
  public
    { font to print the label }
    Font: DFont;
    { shadow intensity. Shadow=0 is no shadow }
    Shadow: Float;
    { whether the label final image is scaled or remains 1:1 for clear text}
    ScaleLabel: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Rescale; override;
    procedure RescaleImage; override;
    //procedure draw; override;
  private
    procedure PrepareTextImage;
    procedure settext(const value: string);
    function gettext: string;
  public
    { text at the label }
    property text: string read gettext write settext;
  private
    ftext:  string;
    BrokenString: DStringList;
  end;

Type
  {provides a simple integer output into a label}
  DIntegerLabel = class (DLabel)
  public
    { pointer to the value it monitors }
    value: Pinteger;
    procedure draw; override;
  end;

Type
  {provides a simple string output into a label}
  DStringLabel = class (DLabel)
  public
    { pointer to the value it monitors }
    value: Pstring;
    procedure draw; override;
  end;

Type
  {provides a simple float output into a label}
  DFloatLabel = class (DLabel)
  public
    { pointer to the value it monitors }
    value: PFloat;
    { how many digits after point are displayed?
      0 - float is rounded to integer (1.6423 -> 2)
      1 - one digit like 1.2
      2 - two digits like 1.03
      no more needed at the moment }
    Digits: integer;
    constructor Create(AOwner: TComponent); override;
    procedure draw; override;
  end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

implementation

uses sysutils, CastleImages, CastleLog,
  decointerface;

{----------------------------------------------------------------------------}

constructor DLabel.create(AOwner : TComponent);
begin
  inherited create(AOwner);
  ScaleLabel := false;
  Shadow := 0;
end;

{----------------------------------------------------------------------------}

destructor DLabel.Destroy;
begin
  if BrokenString <> nil then BrokenString.Clear;
  FreeAndNil(BrokenString);
  inherited
end;

{----------------------------------------------------------------------------}

procedure DLabel.settext(const value : string);
begin
  if ftext <> value then begin
    ftext := value;
    PrepareTextImage;
  end;
end;

{----------------------------------------------------------------------------}

function DLabel.gettext : string;
begin
  result := ftext;
end;

{----------------------------------------------------------------------------}

procedure DLabel.Rescale;
begin
  inherited;
  {if not ScaleLabel then
    base.backwardsetsize(RealWidth,RealHeight) }
end;

{----------------------------------------------------------------------------}

procedure DLabel.PrepareTextImage;
begin
  if BrokenString<> nil then BrokenString.Clear;
  FreeAndNil(BrokenString);
  BrokenString := font.break_stings(text,base.w);
  FreeImage;

  // for i := 0 to brokenString.count-1 do writeLnLog('',inttostr(brokenstring[i].height));

//  SourceImage := nil; // let it be as a safeguard here. I don't want to freeannil GImage before it is instantly created to avoid sigsegvs

  if shadow = 0 then
    SourceImage := font.broken_string_to_image(BrokenString)
  else
    SourceImage := font.broken_string_to_image_with_shadow(BrokenString,shadow,3);

  RealHeight := SourceImage.height;
  RealWidth := sourceImage.width;

  ImageLoaded := true;     //not good...
  RescaleImage;
end;

{----------------------------------------------------------------------------}

procedure DLabel.RescaleImage;
begin
  {$IFNDEF AllowRescale}If sourceImage=nil then exit;{$ENDIF}
  If self.ScaleLabel then
    inherited //rescale this label as a simple image to fit "base size"
  else begin
    //don't rescale this label to provide sharp font
    if ImageLoaded then
       if base.initialized then begin
          ScaledImage := SourceImage.MakeCopy;
          base.backwardsetsize(RealWidth,RealHeight);
          InitGLPending := true;
          {$IFNDEF AllowRescale}freeandnil(sourceImage);{$ENDIF}
        end
       else
         writeLnLog('DLabel.RescaleImage/no scale label','ERROR: base.initialized = false');
  end;
end;


{=============================================================================}
{========================= Integer label =====================================}
{=============================================================================}

procedure DIntegerLabel.draw;
begin
  Text := inttostr(value^);
  inherited;
end;

{=============================================================================}
{========================== String label =====================================}
{=============================================================================}

procedure DStringLabel.draw;
begin
  Text := value^;
  inherited;
end;

{=============================================================================}
{=========================== Float label =====================================}
{=============================================================================}

Constructor DFloatLabel.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Digits := 0;
end;

procedure DFloatLabel.draw;
begin
  case Digits of
    1: Text := inttostr(trunc(value^))+'.'+inttostr(round(frac(value^)*10));
    2: Text := inttostr(trunc(value^))+'.'+inttostr(round(frac(value^)*100));
    else Text := inttostr(round(value^));
  end;
  inherited;
end;

end.

