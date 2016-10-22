unit decogui;

{$mode objfpc}{$H+}

interface

uses classes, CastleRandom,
  decointerface, decoimages,
  decoglobal;

Type
  DInterfaceContainer = class(DInterfaceElement)
  public
    { just = window.height, wihdow.width. Maybe I'll deprecate it later }
    width,height:integer;
    { random generator used for all interface random events }
    rnd: TCastleRandom;
    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
    procedure Rescale; override;
end;

var GUI: DInterfaceContainer;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog;


{=============================================================================}
{========================== interface container ==============================}
{=============================================================================}

constructor DInterfaceContainer.create(AOwner: TComponent);
begin
  writeLnLog('DInterfaceContainer.create','Creating interface.');
  inherited create(AOwner);
  rnd := TCastleRandom.Create;

  width := -1;
  height := -1;
end;

destructor DInterfaceContainer.destroy;
begin
  writeLnLog('DInterfaceContainer.destroy','Game over...');
  freeandnil(rnd);
  inherited;
end;

procedure DInterfaceContainer.rescale;
begin
  GUI.width := window.Width;
  GUI.height := window.Height;
  inherited;
end;

end.

