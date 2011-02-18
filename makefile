empty :=
space :=$(empty) $(empty)

HOST=$(shell gcc -dumpmachine)
export TARGET=$(HOST)

ifeq ($(TARGET),i686-pc-mingw32)
DIRSEP=\$(empty)
PATHLISTSEP=;
EXESUFFIX=.exe
CGISUFFIX=.exe
else
DIRSEP=/
PATHLISTSEP=:
EXESUFFIX=
CGISUFFIX=.cgi
endif

ifneq ($(TARGET),$(HOST))
GNATMAKE=$(TARGET)-gnatmake
BUILD=release
BUILDDIR=$(abspath build-$(TARGET))
IMPORTDIR:=$(abspath import-$(TARGET))
else
GNATMAKE=gnatmake
BUILD=debug
BUILDDIR=$(abspath build)
IMPORTDIR:=$(abspath import)
endif

# I'm using SandTrip :-)
# http://kirika.la.coocan.jp/proj/sandtrip/
TESTDIR=~/Sites/SandTrip/vampire

ifeq ($(BUILD),release)
CARGS=-Os -momit-leaf-frame-pointer -fdata-sections -gnatn -gnatwaIFK.R
BARGS=
LARGS=-s -Xlinker --gc-sections
else
CARGS=-Os -momit-leaf-frame-pointer -g -gnata -gnatn -gnatwaIFK.R
BARGS=-E
LARGS=-g
endif

MARGS:=-cargs $(CARGS) -bargs $(BARGS) -largs $(LARGS)

ifneq ($(DRAKE_RTSROOT),)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)
endif
ifneq ($(DRAKE_RTSDIR),)
IMPORTDIR:=
MARGS:=--RTS=$(abspath $(DRAKE_RTSDIR)) $(MARGS) -lc-gnat
endif

export ADA_PROJECT_PATH=
export ADA_INCLUDE_PATH=$(subst $(space),$(PATHLISTSEP),$(IMPORTDIR) $(abspath $(wildcard lib/*) $(wildcard lib/*/$(TARGET))))
export ADA_OBJECTS_PATH=

.PHONY: all clean test-vampire archive \
	site/vampire$(CGISUFFIX) \
	site/unlock$(CGISUFFIX)

all: site/vampire$(CGISUFFIX)

site/vampire$(CGISUFFIX): source/tabula-vampires-main.adb $(BUILDDIR) $(IMPORTDIR)
	cd $(BUILDDIR) && $(GNATMAKE) -o ../$@ ../$< $(MARGS)

site/unlock$(CGISUFFIX): source/tabula-unlock.adb $(BUILDDIR)
	cd $(BUILDDIR) && $(GNATMAKE) -o ../$@ ../$< $(MARGS)

analyze$(EXESUFFIX): source/analyze/analyze.adb $(BUILDDIR)
	cd $(BUILDDIR) && $(GNATMAKE) -o ../$@ ../$< $(MARGS)

exclude$(EXESUFFIX): source/analyze/exclude.adb $(BUILDDIR)
	cd $(BUILDDIR) && $(GNATMAKE) -o ../$@ ../$< $(MARGS)

export QUERY_STRING=

$(TESTDIR)/%: site/%
	install $< $(TESTDIR)

install-test: \
	$(TESTDIR)/vampire$(CGISUFFIX) \
	$(TESTDIR)/cast \
	$(TESTDIR)/style.css \
	$(addprefix $(TESTDIR)/,$(notdir $(wildcard site/*.html)))

test-vampire: install-test
	cd $(TESTDIR) && gdb .$(DIRSEP)vampire$(CGISUFFIX)

$(BUILDDIR):
	mkdir $@

ifneq ($(IMPORTDIR),)
$(IMPORTDIR): lib/import.h
	headmaster --gcc $(TARGET)-gcc --to ada -p -D $(IMPORTDIR) $+
endif

clean:
	-rm -rf build*
	-rm -rf import*

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
