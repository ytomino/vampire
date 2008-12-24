ifneq ($(ComSpec),)
CGISUFFIX=.exe
else
CGISUFFIX=.cgi
endif

ifneq ($(wildcard lib),)
export ADA_PROJECT_PATH=lib
else
endif

BUILDTYPE=debug
BUILDDIR=build

.PHONY: all get-lib get-ase get-interfaces get-iconv get-dyayaml

all: site/vampire$(CGISUFFIX)

site/vampire$(CGISUFFIX): $(BUILDDIR)
	gnatmake -P source/vampire.gpr -XBUILDTYPE=$(BUILDTYPE) -XBUILDDIR=../$(BUILDDIR) -XCGISUFFIX=$(CGISUFFIX)

site/unlock$(CGISUFFIX): $(BUILDDIR)
	gnatmake -P source/unlock.gpr -XBUILDTYPE=$(BUILDTYPE) -XBUILDDIR=../$(BUILDDIR) -XCGISUFFIX=$(CGISUFFIX)

get-lib: get-ase get-interfaces get-iconv get-dyayaml

get-ase:
	svn export --force http://panathenaia.googlecode.com/svn/trunk/ase/source lib

get-interfaces:
	svn export --force http://panathenaia.googlecode.com/svn/trunk/yt-gnat/source lib

get-iconv:
	svn export --force http://panathenaia.googlecode.com/svn/trunk/iconv-gnat/source lib

get-dyayaml:
	svn export --force http://panathenaia.googlecode.com/svn/trunk/dyayaml/gnat lib

$(BUILDDIR):
	mkdir $(BUILDDIR)

###################

debug: i686-pc-mingw32-debug
freebsd: i686-pc-freebsd6-release
users: users-i686-pc-mingw32-debug
users-freebsd4: users-i686-pc-freebsd6-release
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

shuffle-i686-pc-mingw32-debug:
	gnatmake -P shuffle
shuffle-i686-pc-freebsd6-release:
	i686-pc-freebsd6-gnatmake -P shuffle -XTARGET=i686-pc-freebsd6 -XBUILD=release
