empty :=
space :=$(empty) $(empty)

HOST=$(shell gcc -dumpmachine)
export TARGET=$(HOST)

ifeq ($(TARGET),i686-pc-mingw32)
DIRSEP=\$(empty)
PATHLISTSEP=;
CGISUFFIX=.exe
else
DIRSEP=/
PATHLISTSEP=:
CGISUFFIX=.cgi
endif

ifneq ($(TARGET),$(HOST))
GNATMAKE=$(TARGET)-gnatmake
BUILD=release
BUILDDIR:=$(abspath build-$(TARGET))
else
GNATMAKE=gnatmake
BUILD=debug
BUILDDIR:=$(abspath build)
endif

ifeq ($(BUILD),release)
CARGS=-O2 -momit-leaf-frame-pointer -fdata-sections -gnatn -gnatwaI
BARGS=
LARGS=-s -Xlinker --gc-sections
else
CARGS=-Os -momit-leaf-frame-pointer -g -gnata -gnatn -gnatwaI
BARGS=-E
LARGS=-g
endif

MARGS=-a -cargs $(CARGS) -bargs $(BARGS) -largs $(LARGS)

export ADA_PROJECT_PATH=
export ADA_INCLUDE_PATH=$(subst $(space),$(PATHLISTSEP),$(abspath $(wildcard lib/*) $(wildcard lib/*/$(TARGET))))
export ADA_OBJECTS_PATH=

CARGS:=$(CARGS) -gnatwFK.R
LARGS:=$(LARGS) -lcrypto -liconv -ldyayaml

.PHONY: all clean test-vampire test-shuffle test-users archive \
	site/vampire$(CGISUFFIX) \
	site/unlock$(CGISUFFIX) \
	site/users$(CGISUFFIX) \
	site/shuffle$(CGISUFFIX)

all: site/vampire$(CGISUFFIX)

clean:
	-rm -rf build*

site/vampire$(CGISUFFIX): source/vampire.adb $(BUILDDIR)
	cd $(BUILDDIR) && $(GNATMAKE) -o ../$@ ../$< $(MARGS)

site/unlock$(CGISUFFIX): source/unlock.adb $(BUILDDIR)
	cd $(BUILDDIR) && $(GNATMAKE) -o ../$@ ../$< $(MARGS)

site/users$(CGISUFFIX): source/users.adb $(BUILDDIR)
	cd $(BUILDDIR) && $(GNATMAKE) -o ../$@ ../$< $(MARGS)

site/shuffle$(CGISUFFIX): source/shuffle.adb $(BUILDDIR)
	cd $(BUILDDIR) && $(GNATMAKE) -o ../$@ ../$< $(MARGS)

test-vampire:
	cd site && $(DEBUGGER) .$(DIRSEP)vampire$(CGISUFFIX)

test-users:
	cd site && .$(DIRSEP)users$(CGISUFFIX)

test-shuffle:
	cd site && .$(DIRSEP)shuffle$(CGISUFFIX)

$(BUILDDIR):
	mkdir $@

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
