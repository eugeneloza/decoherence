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

{ Works with different types of labels }
unit DecoLabels;

{$INCLUDE compilerconfig.inc}

interface

uses Classes,
  DecoImages, DecoFont,
  DecoGlobal;

type
  { a powerful text label, converted to GLImage to be extremely fast }
  DLabel = class(DAbstractImage)
  public
{    { font to print the label }
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
    procedure SetText(const value: string);
    function GetText: string;
  public
    { text at the label }
    property Text: string read GetText write SetText;
  private
    fText:  string;
    BrokenString: DStringList;  }
  end;

Type
  {provides a simple integer output into a label}
  DIntegerLabel = class (DLabel)
  public
 {   { pointer to the value it monitors }
    Value: Pinteger;
    procedure Draw; override; }
  end;

Type
  {provides a simple string output into a label}
  DStringLabel = class (DLabel)
  public
    { pointer to the value it monitors }
 {   value: Pstring;
    procedure Draw; override; }
  end;

Type
  {provides a simple float output into a label}
  DFloatLabel = class (DLabel)
  public
    { pointer to the value it monitors }
 {   Value: PFloat;
    { how many digits after point are displayed?
      0 - float is rounded to integer (1.6423 -> 2)
      1 - one digit like 1.2
      2 - two digits like 1.03
      no more needed at the moment }
    Digits: integer;
    constructor Create(AOwner: TComponent); override;
    procedure Draw; override;  }
  end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

implementation

uses SysUtils, CastleImages, CastleLog,
  DecoInterface;

{----------------------------------------------------------------------------}

{constructor DLabel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ScaleLabel := false;
  Shadow := 0;
end; }

{----------------------------------------------------------------------------}

{destructor DLabel.Destroy;
begin
  if BrokenString <> nil then BrokenString.Clear;
  FreeAndNil(BrokenString);
  inherited
end; }

{----------------------------------------------------------------------------}

{procedure DLabel.SetText(const Value: string);
begin
  if fText <> Value then begin
    fText := Value;
    PrepareTextImage;
  end;
end;  }

{----------------------------------------------------------------------------}

{function DLabel.GetText: string;
begin
  Result := fText;
end;  }

{----------------------------------------------------------------------------}

{procedure DLabel.Rescale;
begin
  inherited;
  {if not ScaleLabel then
    base.backwardsetsize(RealWidth,RealHeight) }
end; }

{----------------------------------------------------------------------------}

{procedure DLabel.PrepareTextImage;
begin
  if BrokenString<> nil then BrokenString.Clear;
  FreeAndNil(BrokenString);
  BrokenString := Font.BreakStings(Text,Base.w);
  FreeImage;

  // for i := 0 to brokenString.count-1 do writeLnLog('',inttostr(brokenstring[i].height));

//  SourceImage := nil; // let it be as a safeguard here. I don't want to freeannil GImage before it is instantly created to avoid sigsegvs

  if Shadow = 0 then
    SourceImage := Font.BrokenStringToImage(BrokenString)
  else
    SourceImage := Font.BrokenStringToImageWithShadow(BrokenString,Shadow,3);

  RealHeight := SourceImage.Height;
  RealWidth := SourceImage.Width;

  ImageLoaded := true;     //not good...
  RescaleImage;
end;  }

{----------------------------------------------------------------------------}

{procedure DLabel.RescaleImage;
begin
  {$IFNDEF AllowRescale}If SourceImage = nil then Exit;{$ENDIF}
  If Self.ScaleLabel then
    inherited //rescale this label as a simple image to fit "base size"
  else begin
    //don't rescale this label to provide sharp font
    if ImageLoaded then
       if Base.Initialized then begin
          ScaledImage := SourceImage.MakeCopy;
          Base.BackwardSetSize(RealWidth,RealHeight);
          InitGLPending := true;
          {$IFNDEF AllowRescale}FreeAndNil(SourceImage);{$ENDIF}
        end
       else
         WriteLnLog('DLabel.RescaleImage/no scale label','ERROR: base.initialized = false');
  end;
end; }


{=============================================================================}
{========================= Integer label =====================================}
{=============================================================================}

{procedure DIntegerLabel.Draw;
begin
  Text := IntToStr(value^);
  inherited;
end; }

{=============================================================================}
{========================== String label =====================================}
{=============================================================================}

{procedure DStringLabel.Draw;
begin
  Text := Value^;
  inherited;
end; }

{=============================================================================}
{=========================== Float label =====================================}
{=============================================================================}

{Constructor DFloatLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Digits := 0;
end;  }

{---------------------------------------------------------------------------}

{procedure DFloatLabel.Draw;
begin
  case Digits of
    1: Text := IntToStr(Trunc(Value^))+'.'+IntToStr(Round(Frac(Value^)*10));
    2: Text := IntToStr(Trunc(Value^))+'.'+IntToStr(Round(Frac(Value^)*100));
    else Text := IntToStr(Round(Value^));
  end;
  inherited;
end; }

end.

