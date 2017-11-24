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

{ Manages player camera and other effects such as lights and screen shaders }
unit DecoNavigation;

{$INCLUDE compilerconfig.inc}
interface

uses Classes, SysUtils,
  CastleCameras, {castlePlayer,} CastleSceneCore, CastleScene,
  CastleVectors,  X3DNodes,
  decoglobal;

const PlayerHeight = 0.5;

var Camera: TWalkCamera;
    Navigation: TCastleScene;

procedure InitNavigation;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses  CastleFilesUtils, x3dLoad,
  DecoLog;

var Nav: TKambiNavigationInfoNode;
    NavRoot: TX3DRootNode;
    NavLight: TPointLightNode; {$HINT Check this}
    ScreenEffect: TX3DRootNode;


procedure InitNavigation;
begin
  Log(LogInitPlayer,_CurrentRoutine,'Initialize navigation');
  Camera := TWalkCamera.Create(Window);
  {z-up orientation}
  Camera.Gravity := false;
  Camera.PreferredHeight := PlayerHeight;
  Camera.MouseDragMode := mdRotate;

  Camera.Input := [];  //-----  completely disable camera

  NavRoot := TX3DRootNode.create;

  //create light that follows the player
  {$Hint todo}
  NavLight:= TPointLightNode.Create;
  NavLight.FdColor.Value := Vector3(1,0.3,0.1);
  NavLight.FdAttenuation.Value := Vector3(1,0,1);
  {$Warning light distance}
  NavLight.FdRadius.Value := 10*3;
  NavLight.FdIntensity.value := 20*3;
  NavLight.FdOn.Value := true;
  NavLight.FdShadows.Value := false;

  //and create a respective navigation node
  Nav := TKambiNavigationInfoNode.Create;
  Nav.FdHeadLightNode.Value := NavLight;
  Nav.FdHeadlight.Value := true;

  NavRoot.FdChildren.Add(Nav);

  {$Hint todo}

  {this is a temporary "addition" of a screen shader,
   should be replaced for something more useful some time later}
  {Shaders := TSwitchNode.create;}
  ScreenEffect := Load3D(ApplicationData('shaders/empty.x3dv'));
  {Shaders.fdChildren.add(screenEffect.FdChildren[0]);
  ScreenEffect := load3D(ApplicationData('shaders/edgedetect.x3dv'));
  Shaders.fdChildren.add(screenEffect.FdChildren[0]);
  ScreenEffect := load3D(ApplicationData('shaders/blur.x3dv'));
  Shaders.fdChildren.add(screenEffect.FdChildren[0]);}

  NavRoot.FdChildren.add(ScreenEffect);
  //Scene.Attributes.EnableTextures := false;

  Navigation := TCastleScene.Create(Window);
  Navigation.Spatial := [{ssRendering, ssDynamicCollisions}];
  Navigation.ProcessEvents := true;
  Navigation.ShadowMaps := ShadowMapsEnabled;
  Navigation.Load(NavRoot,true);

end;

end.

