# CONTRIBUTION GUIDELINES AND CODE CONVENTIONS

(document in process of creation)

## Submitting a bug report

Please, use https://github.com/eugeneloza/decoherence/issues to create a bug report. Also stick to the rule "one report - one issue".

## Submitting an idea

Please, also use GitHub issues as above.

## Contribute code

Please, follow Castle Game Engine contribution guidelines:
https://github.com/castle-engine/castle-engine/blob/master/doc/miscellaneous_notes/coding_conventions.txt

There are some minor differences in code style:

* Always use CAPITALS for compiler directives and CamelCase for $DEFINE values. E.g. `{$IFDEF Desktop}...{$ELSE}...{$ENDIF}`

* Use extended names for compiler directives. E.g. not `{$I somefile.inc}` but `{$INCLUDE somefile.inc}`.

* Always use lowercase for single-letter variables like `i`, `j`, `k`.
First letter is lowercase in words "tmp" and "temp", e.g. `tmpVariable: integer`, `tempFile: File`.

* Use `DClassName` instead of `TClassName` for game-specific objects/classes/records/enums
Not sure if this is wise, but this is made to make a difference with regular FPC or CGE related classes.

* Leave `{}` if comment is missing for some type/variable/routine definition.
This makes it easier to find such missing comments automatically

* Use a dot-spacer before the implementation part and also before the initialization part (and every routine related to initalization). Like this:
```
var
  x: integer;

procedure IncX;
procedure DecX;
procedure InitX;
{............................................................................}
implementation

procedure IncX;
begin
  inc(x);
end;

{-----------------------------------------------------------------------------}

procedure IncX;
begin
  inc(x);
end;

{............................................................................}

procedure IncX;
begin
  x := 0;
end;

end.
```

* Never put anything into `initialization`..`finalization` parts. Create a InitXxx procedure and reference it in DecoInit. This allows to control the rigit initialization flow.

* Always put {$INCLUDE compilerconfig.inc} before the unit name. Even if it doesn't use any compiler options.

* Use TryInline macro instead of Inline. Note that TryInclude is not followed by a semicolon: `procedure Foo; TryInline`.

