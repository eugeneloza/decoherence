unit decostats;

{$INCLUDE compilerconfig.inc}

interface
uses
  decoglobal;

Const
  {stats for actors}
  St_str = 0;
  St_dx = 1;
  St_ag = 2;
  St_acc = 3;
  Maxbasestats = 3; {+1}
  {stats for player characters}
  St_end = 1;
  St_spd = 5;
  St_chem = 6;
  St_phys = 7;
  St_bio = 8;
  St_mph = 9;
  Maxstats = 9; {+1}

Type
  {A record representing the base stats array}
  DStats = class(TObject)
    Value: array of float;
    Count: integer;
    Constructor create(setfullstats: boolean);
  End;

implementation

Constructor DStats.create(setfullstats: boolean);
Begin
  If setfullstats then
    count := Maxstats+1
  Else
    count := Maxbasestats+1;
  setlength(Value,Count);

End;

end.

