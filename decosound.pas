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

{ Sound and music routines
  Adaptive music and playlists }

unit decosound;

{$INCLUDE compilerconfig.inc}
interface
//uses

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SyncObjs, Classes, SysUtils, CastleLog, castleFilesUtils,
  CastleSoundEngine, CastleTimeUtils,
  decoinputoutput;
 {{$IFDEF UNIX}cthreads,{$ENDIF}
  CastleOpenAL, , , CastleVectors,}

var
  {a lock to ensure no simultaneous HDD access}
  MusicLock: TCriticalSection;

type
  {thread that actually loads the music}
  TMusicLoadThread = class(TThread)
  public
    music: TSoundBuffer;
    music_duration: TFloatTime;
  protected
    procedure Execute; override;
end;

procedure TMusicLoadThread.Execute;
begin
  if not MusicLock.TryEnter then begin
    WriteLnLog('TMusicLoadThread.Execute','Another thread is already loading music. Exiting.');
    {$HINT Something more usefull should happen here... The game situation has already changed, so, maybe, we just need to wait for this thread to finish and load the next one at once?}
    exit;
  end;
  //MusicLock.Acquire;

  Music := LoadBufferSafe(ApplicationData(''{MusFolder+music_name}),music_duration);

  MusicLock.Release;
end;


initialization
MusicLock := TCriticalSection.create;

finalization
freeAndNil(MusicLock);

end.

