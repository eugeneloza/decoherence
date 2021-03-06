{ Copyright (C) 2012-2018 Yevhen Loza

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>. }

{---------------------------------------------------------------------------}

(* A highets-level container for the interface and its routines *)

{$INCLUDE compilerconfig.inc}

unit DecoGUI;

interface

uses
  DecoInterfaceCore, DecoMouseCursor,
  {<temporary included for debugging>}
  DecoInterfaceImages,
  DecoWind,
  DecoFrames,
  DecoInterfaceBars,
  DecoInterfaceArrangers,
  DecoImageLoader,
  DecoFont,
  DecoFramedElement,
  DecoBaseActor,
  {</temporary>}
  DecoLabels,
  DecoGlobal, DecoGUIScale;

type
  { GUI container, manages all other GUI elements }
  DGUI = class(DInterfaceElement)
  strict private
    FPSLabel: DFPSLabel;
  strict private
    FFirstRender: boolean;
    { Some post-initialization routines, that require graphics context fully available }
    procedure FirstRender;
  public
    { A pop-up window, showing a message }
    procedure ShowMessage(const aMessage: string);
  public
    { (Re)sets the tint of the whole interface }
    procedure SetTint; override;
    { Draw the GUI and all its child elements }
    procedure Draw; override;
  public
    { Mouse cursor and corresponding operations 
      Also used to render dragged image }
    Cursor: DCursor;
    { Updates cursor position and image }
    procedure UpdateCursor(const CursorX, CursorY: single;
      const DragElement: DSingleInterfaceElement = nil);
  public
    (* Specific interface cases *)
    procedure TestInterface;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  GUI: DGUI;

{ Initialize GUI instance }
procedure InitGUI;
{ Free GUI instance }
procedure FreeGUI;
{............................................................................}
implementation
uses
  SysUtils, CastleColors, CastleGLUtils,
  DecoPlayer, DecoGameMode, DecoMouse,
  DecoLog, Profiler;

procedure DGUI.ShowMessage(const aMessage: string);
begin
  Log(true, CurrentRoutine, aMessage); //temporary
  { will create a message window with text }
end;

{-----------------------------------------------------------------------------}

procedure DGUI.Draw;
begin
  {StartProfiler}


  if FFirstRender then FirstRender;

  { clear the screen depending on the game mode
    in case SceneManager doesn't clear it }
  if GameModeNeedsClearingScreen then
    RenderContext.Clear([cbColor], Black);

  { draw children elements }
  inherited Draw;

  { draw special elements }
  FPSLabel.CountFPS;
  Cursor.Draw;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DGUI.UpdateCursor(const CursorX, CursorY: single;
  const DragElement: DSingleInterfaceElement = nil);
begin
  {StartProfiler}

  Cursor.DragElement := DragElement;

  if Cursor.DragElement = nil then
  begin
    if MouseLook then
      Cursor.CurrentCursor := ctMouseLook
    else
      Cursor.CurrentCursor := ctDefault;

    if MouseLook then
    begin
      { it's important to avoid mouse shimmer during MouseLook }
      Cursor.x := GUICenter[0];
      Cursor.y := GUICenter[1];
    end else
    begin
      Cursor.x := CursorX;
      Cursor.y := CursorY;
    end;
  end;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DGUI.SetTint;
begin
  {StartProfiler}

  inherited SetTint;
  Cursor.SetTint;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DGUI.FirstRender;
begin
  {StartProfiler}

  FFirstRender := false;
  Cursor.HideOSCursor;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

constructor DGUI.Create;
begin
  {StartProfiler}

  inherited Create;
  FullScreen;

  GUITint := White;

  FFirstRender := true;
  Cursor := DCursor.Create;

  FPSLabel := DFPSLabel.Create;

  SetTint;

  TestInterface;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

destructor DGUI.Destroy;
begin
  {StartProfiler}

  FreeAndNil(Cursor);
  FreeAndNil(FPSLabel);
  inherited Destroy;

  {StopProfiler}
end;

{===================== SPECIFIC INTERFACE KINDS ============================}

procedure DGUI.TestInterface;
var
  Arr: DRectagonalFramedElement;
  Img: DFullScreenImage;
  Frame: DRectagonalFrame;
  Bar: DPlayerBars;
  Lab: DLabelImage;
begin
  Clear;

  {Img := DFullScreenImage.Create;
  Img.Load(LoadFullScreenImage('GUI/Burner/abstract-background-1523717636ybk_cc0_by_Linnaea_Mallette_[burner].jpg'));
  Grab(Img);}

  Grab(DWind.Create);

  Frame := DRectagonalFrame.Create;
  Frame.SetSize(100, 100, 300, 300, 0.9, asFlyInRadial, 2.0);
  Frame.Load(GetFrameByName('RegularFrame'));
  Grab(Frame);

  Arr := DRectagonalFramedElement.Create;
  Arr.LoadFrame(GetFrameByName('RegularFrame'));

  Lab := DLabelImage.Create;
  Lab.ShadowIntensity := 1;
  Lab.SetSize(330, 330, 50, 50);
  Lab.Font := GetFontByName('Default');
  Lab.Text := 'DragMe';
  Lab.CanDrag := true;
  Lab.ResetToRealSize;
  Arr.Grab(Lab);

  Arr.SetSize(600, 100, 200, 200, 1.0, asFlyInRadial, 0.6);
  Grab(Arr);

  {Arr := DRectagonalFramedElement.Create;
  Arr.LoadFrame(GetFrameByName('RegularFrame'));
  Arr.SetSize(100, 100, 400, 400, 1.0, asFlyInRadial, 0.6);
  Grab(Arr);   }

  Bar := DPlayerBars.Create;
  Bar.SetSize(600, 200, 26, 165);
  //Bar.SetSize(100, 100, 400, 400);
  Grab(Bar);

  Lab := DLabelImage.Create;
  Lab.SetSize(120, 160, 150, 50, 1.0, asFlyInRadial);
  Lab.Font := GetFontByName('Default');
  Lab.LabelType := ltJustify;
  Lab.Text := 'This is a very long text to test label "justify" method, which should align the text evenly along width with only the last line not aligned. Should keep an eye for correct spacebar handling.';
  Lab.ResetToRealSize;
  Grab(Lab);

  Lab := DLabelImage.Create;
  Lab.SetSize(230, 300, 150, 50, 1.0, asFlyInRadial);
  Lab.Font := GetFontByName('Default');
  Lab.LabelType := ltSimple;
  Lab.Text := 'This is a very long text to test simple label method, which just output the text trying not to exceed the with. Should keep an eye for correct line start and end.';
  Lab.ResetToRealSize;
  Grab(Lab);

  Lab := DLabelImage.Create;
  Lab.SetSize(140, 310, 10, 50, 1.0, asFlyInRadial);
  Lab.Font := GetFontByName('Default');
  Lab.LabelType := ltSimple;
  Lab.Text := 'This label is too small yet correct.';
  Lab.ResetToRealSize;
  Grab(Lab);
end;

{............................................................................}

procedure InitGUI;
begin
  {StartProfiler}

  GUI := DGUI.Create;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure FreeGUI;
begin
  {StartProfiler}

  GUI.Free;

  {StopProfiler}
end;

end.

