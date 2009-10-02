empty :=
space :=$(empty) $(empty)

ifneq ($(ComSpec),)
HOST=i686-pc-mingw32
DIRSEP=\$(empty)
PATHLISTSEP=;
else
ifneq ($(Apple_PubSub_Socket_Render),)
HOST=i686-apple-darwin9
DIRSEP=/
PATHLISTSEP=:
else
HOST=i686-pc-freebsd7
DIRSEP=/
PATHLISTSEP=:
endif
endif

export TARGET=$(HOST)

ifeq ($(TARGET),i686-pc-mingw32)
export CGISUFFIX=.exe
else
export CGISUFFIX=.cgi
endif

ifneq ($(TARGET),$(HOST))
GNATMAKE=$(TARGET)-gnatmake
export BUILD=release
export BUILDDIR:=build-$(TARGET)
else
GNATMAKE=gnatmake
export BUILD=debug
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

.PHONY: all clean test-vampire test-shuffle test-users archive \
	site/vampire$(CGISUFFIX) \
	site/unlock$(CGISUFFIX) \
	site/users$(CGISUFFIX) \
	site/shuffle$(CGISUFFIX)

all: site/vampire$(CGISUFFIX)

clean:
	-rm -rf build*

site/vampire$(CGISUFFIX):
	$(GNATMAKE) -a -p -P source/vampire.gpr

site/unlock$(CGISUFFIX):
	$(GNATMAKE) -a -p -P source/unlock.gpr

site/users$(CGISUFFIX):
	$(GNATMAKE) -a -p -P source/users.gpr

site/shuffle$(CGISUFFIX):
	$(GNATMAKE) -a -p -P source/shuffle.gpr

test-vampire:
	cd site && $(DEBUGGER) .$(DIRSEP)vampire$(CGISUFFIX)

test-users:
	cd site && .$(DIRSEP)users$(CGISUFFIX)

test-shuffle:
	cd site && .$(DIRSEP)shuffle$(CGISUFFIX)

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
