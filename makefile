empty :=
space :=$(empty) $(empty)

ifneq ($(ComSpec),)
HOST=i686-pc-mingw32
DIRSEP=\$(empty)
PATHLISTSEP=;
else
HOST=i686-pc-freebsd6
DIRSEP=/
PATHLISTSEP=:
endif

export TARGET=$(HOST)

ifeq ($(TARGET),i686-pc-mingw32)
export CGISUFFIX=.exe
else
export CGISUFFIX=.cgi
endif

ifneq ($(TARGET),$(HOST))
GNATMAKE=$(TARGET)-gnatmake
export BUILDTYPE=release
export BUILDDIR:=build-$(TARGET)
else
GNATMAKE=gnatmake
export BUILDTYPE=debug
export BUILDDIR:=build
endif

override BUILDDIR:=$(abspath $(BUILDDIR))

LIB_PROJECTS=$(wildcard lib/*/*.gpr)
ifneq ($(LIB_PROJECTS),)
export ADA_PROJECT_PATH=$(subst $(space),$(PATHLISTSEP),$(dir $(LIB_PROJECTS)))
export ADA_INCLUDE_PATH=
export ADA_OBJECTS_PATH=
else
endif

LIB_MAKEFILES=$(wildcard lib/*/makefile)
LIB_MAKE=$(addsuffix .mk,$(LIB_MAKEFILES))

.PHONY: all clean test-vampire test-users archive

all: site/vampire$(CGISUFFIX)

clean:
	gnatclean -P source/vampire.gpr

site/vampire$(CGISUFFIX): $(BUILDDIR) $(LIB_MAKE)
	$(GNATMAKE) -P source/vampire.gpr

site/unlock$(CGISUFFIX): $(BUILDDIR) $(LIB_MAKE)
	$(GNATMAKE) -P source/unlock.gpr

site/users$(CGISUFFIX): $(BUILDDIR) $(LIB_MAKE)
	$(GNATMAKE) -P source/users.gpr

site/shuffle$(CGISUFFIX): $(BUILDDIR) $(LIB_MAKE)
	$(GNATMAKE) -P source/shuffle.gpr

$(BUILDDIR):
	mkdir $(subst /,$(DIRSEP),$(BUILDDIR))

%.mk : %
	make -C $(dir $@) BUILDDIR=$(BUILDDIR)

test-vampire:
	cd site && $(DEBUGGER) .$(DIRSEP)vampire$(CGISUFFIX)

test-users:
	cd site && .$(DIRSEP)users$(CGISUFFIX)

archive:
	-rm site/vampire.7z
	mkdir archive
	svn export lib archive/lib
	svn export site archive/site
	svn export source archive/source
	svn export makefile archive/makefile
	svn export readme.txt archive/readme.txt
	cd archive && 7za a -t7z ../site/vampire.7z *
	rm -rf archive
