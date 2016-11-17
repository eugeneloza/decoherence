You can compile the source simply by opening *.lpi in Lazarus IDE (version 1.6.0 and above) and pushing F9 to complie&run.

This version requires Castle Game Engine to compile (at least GIT version 13.10.2016). You can download it here https://github.com/castle-engine/castle-engine and follow installation instructions here http://castle-engine.sourceforge.net/tutorial_install.php
FreePascal at least version 3.0.0 is required.

Linux version requires 32bit GTK+2 (Thanks Akien for the information).
(Debian/Ubuntu package reference):
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
You will also need dev version of OpenGL drivers for your videocard. In general case it is libgl1-mesa-dev.

Or Castle Game Engine DLLs (32 bit / 64 bit) in case of Windows. These may be downloaded here: http://castle-engine.sourceforge.net/engine.php
The DLLs must be placed in the exe folder.

However, if you prefer command-line compilation, you may try lazbuild. See instructions at http://wiki.lazarus.freepascal.org/lazbuild (Thanks to Akien for the information)