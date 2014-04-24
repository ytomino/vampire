empty:=
space:=$(empty) $(empty)

HOST=$(shell gcc -dumpmachine)
export TARGET=$(HOST)

VERSION=$(shell gcc -dumpversion)

ifneq (,$(findstring mingw,$(TARGET)))
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

BUILDDIR=$(abspath $(TARGET).build)

ifneq ($(TARGET),$(HOST))
GNATMAKE=$(TARGET)-gnatmake
BUILD=release
else
GNATMAKE=gnatmake
BUILD=debug
LARGS:=$(shell pkg-config --libs-only-L yaml-0.1)
endif

LINK=gc

TESTDIR=~/Sites/cgi/vampire

CARGS:=-gnat2012 -gnatwaIFK.R
BARGS:=
LARGS:=$(LARGS) -lm
ifneq ($(findstring darwin,$(TARGET)),)
LARGS:=$(LARGS) -licucore
endif
ifneq ($(findstring freebsd,$(TARGET)),)
LARGS:=$(LARGS) -liconv
endif

ifeq ($(LINK),gc)
ifneq ($(findstring darwin,$(TARGET)),)
LARGS:=$(LARGS) -dead_strip
ifneq ($(WHYLIVE),)
LARGS:=$(LARGS) -Xlinker -why_live -Xlinker $(WHYLIVE)
endif
else
CARGS:=$(CARGS) -ffunction-sections -fdata-sections
LARGS:=$(LARGS) -Xlinker --gc-sections -s
endif
endif
ifeq ($(LINK),lto)
CARGS:=$(CARGS) -flto -fwhole-program
LARGS:=$(LARGS) -flto -fwhole-program
endif

ifeq ($(BUILD),debug)
CARGS:=$(CARGS) -Og -g -gnata
BARGS:=$(BARGS) -E
LARGS:=$(LARGS) -g
else
CARGS:=$(CARGS) -Os -momit-leaf-frame-pointer -gnatn
endif

ifneq ($(findstring freebsd,$(TARGET)),)
LARGS:=$(LARGS) -lgcc_eh -lpthread
endif

MARGS:=-cargs $(CARGS) -bargs $(BARGS) -largs $(LARGS)

ifneq ($(DRAKE_RTSROOT),)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
endif
ifneq ($(DRAKE_RTSDIR),)
MARGS:=--RTS=$(abspath $(DRAKE_RTSDIR)) $(MARGS)
else
IMPORTDIR=$(BUILDDIR)/import
endif

export ADA_PROJECT_PATH=
export ADA_INCLUDE_PATH=$(subst $(space),$(PATHLISTSEP),$(IMPORTDIR) $(abspath $(wildcard lib/*/source) $(wildcard lib/*/source/$(TARGET)) lib/iconv-ada/source/libiconv))
export ADA_OBJECTS_PATH=

.PHONY: all clean test-vampire install-test find xref archive

all: site/vampire$(CGISUFFIX)

site/vampire$(CGISUFFIX): source/vampire-main.adb $(wildcard source/*.ad?) $(BUILDDIR) $(IMPORTDIR)
	cd $(BUILDDIR) && $(GNATMAKE) -o ../$@ ../$< $(MARGS)

site/unlock$(CGISUFFIX): source/vampire-unlock.adb $(wildcard source/*.ad?) $(BUILDDIR)
	cd $(BUILDDIR) && $(GNATMAKE) -o ../$@ ../$< $(MARGS)

site/dump-users-log$(EXESUFFIX): source/vampire-dump_users_log.adb $(BUILDDIR)
	cd $(BUILDDIR) && $(GNATMAKE) -o ../$@ ../$< $(MARGS)

DEBUGGER=gdb
export QUERY_STRING=

$(TESTDIR)/%: site/%
	install $< $(dir $@)

install-test: \
	$(TESTDIR)/vampire$(CGISUFFIX) \
	$(TESTDIR)/unlock$(CGISUFFIX) \
	$(TESTDIR)/cast \
	$(TESTDIR)/style.css \
	$(addprefix $(TESTDIR)/,$(notdir $(wildcard site/*.html))) \
	$(addprefix $(TESTDIR)/image/,$(notdir $(wildcard site/image/*.png)))

test-vampire: install-test
	cd $(TESTDIR) && $(DEBUGGER) .$(DIRSEP)vampire$(CGISUFFIX)

$(BUILDDIR):
	mkdir $@

ifneq ($(IMPORTDIR),)
$(IMPORTDIR): lib/import.h
	headmaster --gcc $(TARGET)-gcc --to ada -p -D $(IMPORTDIR) $+
endif

clean:
	-rm -rf *.build

find:
	gnatfind -f --RTS=$(DRAKE_RTSDIR) -aIsource -aO$(BUILDDIR) $(X)

xref:
	gnatfind -f -r --RTS=$(DRAKE_RTSDIR) -aIsource -aO$(BUILDDIR) $(X)

archive:
	git archive --format=tar HEAD | bzip2 > site/vampire.tar.bz2
