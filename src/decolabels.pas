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
  DecoGlobal, DecoTime;

type
  { a powerful text label, converted to GLImage to be extremely fast }
  DLabel = class(DSimpleImage)
  strict private
    fText: string;
    { a set of strings each no longer than "x" }
    BrokenString: DStringList;
  private
    { converts string (Text) into an image }
    procedure PrepareTextImage;
    { change the current Text and call Prepare Text Image if needed }
    procedure SetText(const Value: string);
  public
    { font to print the label }
    Font: DFont;
    { shadow intensity. Shadow=0 is no shadow }
    Shadow: Float;
    constructor Create; override;
    destructor Destroy; override;
    //procedure Rescale; override;
    procedure RescaleImage; override;
  public
    { text at the label }
    property Text: string read fText write SetText;
  end;

type
  {provides a simple integer output into a label}
  DIntegerLabel = class (DLabel)
  public
    { pointer to the value it monitors }
    Value: Pinteger;
    procedure Update; override;
  end;

type
  {provides a simple string output into a label}
  DStringLabel = class (DLabel)
  public
    { pointer to the value it monitors }
    value: Pstring;
    procedure Update; override;
  end;

type
  { Provides a simple float output into a label }
  DFloatLabel = class (DLabel)
  public
    { pointer to the value it monitors }
    Value: PFloat;
    { how many digits after point are displayed?
      0 - float is rounded to integer (1.6423 -> 2)
      1 - one digit like 1.2
      2 - two digits like 1.03
      no more needed at the moment }
    Digits: integer;
    procedure Update; override;
    constructor Create; override;
  end;

type
  { Debug label that counts FPS
      Practically it just increases FPS by 1 each CountFPS call
      and changes displayed value approx once per second }
  DFPSLabel = class(DLabel)
  strict private
    FPScount: Integer;
    LastRenderTime: DTime;
  public
    { Call this each frame instead of Draw,
         Draw is automatically called here }
    procedure CountFPS;
    constructor Create; override;
  end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

implementation

uses SysUtils, CastleImages, CastleLog,
  DecoInterface;

{----------------------------------------------------------------------------}

constructor DLabel.Create;
begin
  inherited Create;
  Base.ScaleItem := false;
  Shadow := 0;
  Font := DefaultFont;
  //fText := ''; //autoinitialized
end;

{----------------------------------------------------------------------------}

destructor DLabel.Destroy;
begin
  FreeAndNil(BrokenString);
  inherited Destroy;
end;

{----------------------------------------------------------------------------}

procedure DLabel.SetText(const Value: string);
begin
  if fText <> Value then begin
    fText := Value;
    PrepareTextImage;
  end;
end;

{----------------------------------------------------------------------------}

procedure DLabel.PrepareTextImage;
begin
  ImageReady := false;
  ImageLoaded := false;

  FreeAndNil(BrokenString);
  BrokenString := Font.BreakStings(fText, Base.w);
  FreeImage;

  if Shadow = 0 then
    SourceImage := Font.BrokenStringToImage(BrokenString)
  else
    SourceImage := Font.BrokenStringToImageWithShadow(BrokenString,Shadow,3);

  Base.SetRealSize(SourceImage.Width,SourceImage.Height);

  ImageLoaded := true;

  Rescale;
end;

{----------------------------------------------------------------------------}

procedure DLabel.RescaleImage;
begin
  {$IFNDEF AllowRescale}If SourceImage = nil then Exit;{$ENDIF}
  If Base.ScaleItem then
    inherited RescaleImage //rescale this label as a simple image to fit "base size"
  else begin
    //don't rescale this label to provide sharp font
    if ImageLoaded then
       if Base.isInitialized then begin
          {$IFDEF AllowRescale}
          ScaledImage := SourceImage.MakeCopy;
          {$ELSE}
          ScaledImage := SourceImage;
          SourceImage := nil;
          {$ENDIF}
          InitGLPending := true;
        end
       else
         WriteLnLog('DLabel.RescaleImage/no scale label','ERROR: base.initialized = false');
  end;
end;


{=============================================================================}
{========================= Integer label =====================================}
{=============================================================================}

procedure DIntegerLabel.Update;
begin
  inherited Update;
  Text := IntToStr(value^);
end;

{=============================================================================}
{========================== String label =====================================}
{=============================================================================}

procedure DStringLabel.Update;
begin
  inherited Update;
  Text := Value^;
end;

{=============================================================================}
{=========================== Float label =====================================}
{=============================================================================}

Constructor DFloatLabel.Create;
begin
  inherited Create;
  Digits := 0;
end;

{---------------------------------------------------------------------------}

procedure DFloatLabel.Update;
begin
  inherited Update;
  case Digits of
    1: Text := IntToStr(Trunc(Value^))+'.'+IntToStr(Round(Frac(Value^)*10));
    2: Text := IntToStr(Trunc(Value^))+'.'+IntToStr(Round(Frac(Value^)*100));
    else Text := IntToStr(Round(Value^));
  end;
end;

{=============================================================================}
{============================ FPS label ======================================}
{=============================================================================}

constructor DFPSLabel.Create;
begin
  inherited Create;
  FPSCount := 0;
  LastRenderTime := -1;

  Base.AnchorToWindow := true;
  Base.ScaleItem := false;
  Base.SetRealSize(100,100);
  SetBaseSize(0,0,0.05,0.05,1.0);

  Shadow := 0;
  Font := DebugFont;
  //Text := '';
end;

{---------------------------------------------------------------------------}

procedure DFPSLabel.CountFPS;
begin
  if LastRenderTime < 0 then LastRenderTime := DecoNow;

  if (DecoNow - LastRenderTime >= 1) then begin
    Text := IntToStr(FPSCount){+' '+inttostr(round(Window.Fps.RealTime))};
    FPSCount := 0;
    LastRenderTime := DecoNow;
  end else inc(FPSCount);
  Draw;
end;

end.

