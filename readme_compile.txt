#General

You can compile the source simply by opening *.lpi in Lazarus IDE (version 1.9 and above) and pushing F9 to compile&run.

#Requirements

This version requires at least 6.3 Castle Game Engine version to compile (no earlier than GIT 21.12.2017). You can download it here https://github.com/castle-engine/castle-engine and follow installation instructions here http://castle-engine.sourceforge.net/tutorial_install.php
FreePascal compiler at least version 3.1.1 is required. I recommend using FPC+Lazarus trunk (SVN) version (as of 16.12.2017) due to better Generics.Collections support in recent CodeTools patches.
You can install latest Lazarus+FPC+cross-compilation tools with fpcupdeluxe. See https://github.com/castle-engine/castle-engine/wiki/fpcupdeluxe for more info.

##Linux

You need both normal and development versions of the libraries required for Castle Game Engine to run properly. These are: (Debian/Ubuntu package reference):
libopenal1
libopenal-dev
libpng
libpng-dev
zlib1g
zlib1g-dev
libvorbis
libvorbis-dev
libfreetype6
libfreetype6-dev
libgtkglext1
libgtkglext1-dev
You will also need dev version of OpenGL drivers for your video card. In general case it is libgl1-mesa-dev.
Linux version of the Constructor tool requires GTK+2 (Thanks Akien for the information).

##Windows

Specific DLL libraries (32 bit / 64 bit) are required in case of Windows. These may be downloaded here: http://castle-engine.sourceforge.net/engine.php.
The DLLs must be placed in the exe folder.

#Command-line compilation

You can compile the game without Lazarus IDE using Castle Game Engine build-tool https://github.com/castle-engine/castle-engine/wiki/Build-Tool.
