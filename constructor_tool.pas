program constructor_tool;

{$INCLUDE compilerconfig.inc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, castle_base, castle_window, castle_components, constructor_mainunit,
  constructor_facts, constructor_global, decotranslation,
  constructor_dungeontiles, constructor_map;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFactsEditor, FactsEditor);
  Application.CreateForm(TDungeonTilesEditor, DungeonTilesEditor);
  Application.CreateForm(TMapEditor, MapEditor);
  Application.Run;
end.

