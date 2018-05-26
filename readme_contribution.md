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

* Always use CAPITALS for compiler directives and CamelCase for `$DEFINE` values. E.g. `{$IFDEF DesktopVairable}...{$ELSE}...{$ENDIF}`

* "inc", "dec", "true", "false" keywords are exception and are written in lowercase.

* Use extended names for compiler directives. E.g. not `{$I somefile.inc}` but `{$INCLUDE somefile.inc}`. Otherwise add a comment on what the compiler directive does (exception is `{$R+}{$Q+}`).

* Always use lowercase for single-letter variables like `i`, `j`, `k`.
First letter is lowercase in words "tmp" and "temp", e.g. `tmpVariable: integer`, `tempFile: File`.

* Use `DClassName` instead of `TClassName` for game-specific objects/classes/records/enums
Not sure if this is wise, but this is made to make a difference with regular FPC or CGE related classes.

* Prefer longer and explanatory names, better if unique, for entities unless it is a local/nested variable/routine. E.g. `SourceData` instead of `Src`.
E.g. you can easily find `SourceStringDictionary` in the code, wherever it is used, but looking for a specific `Source` variable over hundreds of units is not something easily done.

* Always comment every API element, even if its meaning is obvious. A bonus is a comment even for `private` fields.
Virtual procedures usually should receive comment only once - during the first definition, unless `override` significantly changes its behavior.
Leave `{}` if comment is missing for some type/variable/routine definition. This makes it easier to find such missing comments automatically

* Use a dot-spacer before the `implementation` part and also before the initialization part (and every routine related to initalization). Like this:
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

procedure DecX;
begin
  inc(x);
end;

{............................................................................}

procedure InitX;
begin
  x := 0;
end;

end.
```

* Never put anything into `initialization`..`finalization` parts. Create a `InitXxx` procedure and reference it in `DecoInit`. This allows to control the correct initialization flow as some routines may expect others already initialized (e.g. to output log correctly).

* Always put `{$INCLUDE compilerconfig.inc}` before the unit name. Even if it doesn't use any compiler options.

* Use `TryInline` macro instead of `inline`. Note that `TryInline` is not followed by a semicolon: `procedure Foo; TryInline`.

* Always reference explicit `inherited` routine reference, e.g. `inherited Create;`, not just `inherited;`.

* Prefer American English. E.g. "behavior", not "behaviour".

* Always use `Result` variable to assign the `function` result.

* Put all sources into `src` folder. Choose or create an appropriate sub-folder.

* Do not change `version.inc` file, it's the version of master branch.

* Prefer explanatory names for commits and add a valid reference to issues (if required). E.g. not "commit 8", but "make Update global, fixes https://github.com/eugeneloza/decoherence/issues/18570"
