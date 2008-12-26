ifneq ($(ComSpec),)
export CGISUFFIX=.exe
HOST=i686-pc-mingw32
PATHLISTSEP=;
else
export CGISUFFIX=.cgi
HOST=i686-pc-freebsd6
PATHLISTSEP=:
endif

export TARGET=$(HOST)

ifneq ($(TARGET),$(HOST))
GNATMAKE=$(TARGET)-gnatmake
else
GNATMAKE=gnatmake
endif

LIB_PROJECTS=$(wildcard lib/*/*.gpr)
ifneq ($(LIB_PROJECTS),)
empty :=
space :=$(empty) $(empty)
export ADA_PROJECT_PATH=$(subst $(space),$(PATHLISTSEP),$(dir $(LIB_PROJECTS)))
else
endif

export BUILDTYPE=debug
BUILDDIR=build
ABS_BUILDDIR=$(abspath $(BUILDDIR))

REPOSITORY=https://panathenaia.googlecode.com/svn

.PHONY: all clean get-lib get-ase get-interfaces get-iconv get-dyayaml

all: site/vampire$(CGISUFFIX)

clean:
	gnatclean -P source/vampire.gpr -XBUILDDIR=$(ABS_BUILDDIR)

site/vampire$(CGISUFFIX): $(BUILDDIR)
	$(GNATMAKE) -P source/vampire.gpr -XBUILDDIR=$(ABS_BUILDDIR)

site/unlock$(CGISUFFIX): $(BUILDDIR)
	$(GNATMAKE) -P source/unlock.gpr -XBUILDDIR=$(ABS_BUILDDIR)

site/users$(CGISUFFIX): $(BUILDDIR)
	$(GNATMAKE) -P source/users.gpr -XBUILDDIR=$(ABS_BUILDDIR)

site/shuffle$(CGISUFFIX): $(BUILDDIR)
	$(GNATMAKE) -P source/shuffle.gpr -XBUILDDIR=$(ABS_BUILDDIR)

get-lib: get-ase get-interfaces get-iconv get-dyayaml

get-ase:
	svn checkout $(REPOSITORY)/trunk/ase/source lib/ase

get-interfaces:
	svn checkout $(REPOSITORY)/trunk/yt-gnat/interfaces lib/interfaces

get-iconv:
	svn checkout $(REPOSITORY)/trunk/iconv-gnat/source lib/iconv

get-dyayaml:
	svn checkout $(REPOSITORY)/trunk/dyayaml/gnat lib/dyayaml

$(BUILDDIR):
	mkdir $(BUILDDIR)
