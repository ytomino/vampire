debug: i686-pc-mingw32-debug
freebsd: i686-pc-freebsd6-release
users: users-i686-pc-mingw32-debug
users-freebsd4: users-i686-pc-freebsd6-release
unlock: unlock-i686-pc-mingw32-debug
unlock-freebsd4: unlock-i686-pc-freebsd6-release
shuffle: shuffle-i686-pc-mingw32-debug
shuffle-freebsd4: shuffle-i686-pc-freebsd6-release
archive: archiving

i686-pc-mingw32-debug:
	gnatmake -P vampire
i686-pc-freebsd6-release:
	i686-pc-freebsd6-gnatmake -P vampire -XTARGET=i686-pc-freebsd6 -XBUILD=release
bind:
	cmd /c "cd i686-pc-freebsd6 && i686-pc-freebsd6-gnatbind vampire"
link:
	cmd /c "cd i686-pc-freebsd6 && i686-pc-freebsd6-gnatlink -o ..\site\vampire.cgi vampire -Xlinker --gc-sections -Xlinker --print-gc-sections -Xlinker -s"

users-i686-pc-mingw32-debug:
	gnatmake -P users
users-i686-pc-freebsd6-release:
	i686-pc-freebsd6-gnatmake -P users -XTARGET=i686-pc-freebsd6 -XBUILD=release

unlock-i686-pc-mingw32-debug:
	gnatmake -P unlock
unlock-i686-pc-freebsd6-release:
	i686-pc-freebsd6-gnatmake -P unlock -XTARGET=i686-pc-freebsd6 -XBUILD=release

shuffle-i686-pc-mingw32-debug:
	gnatmake -P shuffle
shuffle-i686-pc-freebsd6-release:
	i686-pc-freebsd6-gnatmake -P shuffle -XTARGET=i686-pc-freebsd6 -XBUILD=release

archiving:
	copy site\cast .\archive
	copy site\*.html .\archive
	copy site\*.css .\archive
	copy *.ad? .\archive\source
	copy *.gpr .\archive\source
	copy makefile .\archive\source
	copy D:\site\sakura\alang\ase.7z .\archive\source
	copy D:\site\sakura\alang\dyayaml.7z .\archive\source
	copy D:\site\sakura\alang\iconv-ada.7z .\archive\source
	cmd "/c cd archive && 7za a -t7z ..\vampire.7z *"
	cmd /c move vampire.7z D:\Site\sakura\vampire
