empty:=
space:=$(empty) $(empty)

HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

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

BUILDDIR=$(TARGET).build

ifneq ($(TARGET),$(HOST))
GNATPREFIX=$(TARGET)-
BUILD=release
else
GNATPREFIX=
BUILD=debug
endif

LINK=gc

TESTDIR=~/Sites/cgi/vampire

GARGS:=
MARGS:=-D $(BUILDDIR)
CARGS:=-gnat2012 -gnatwaIFK.R -gnatef
BARGS:=-x
LARGS:=

ifeq ($(TARGET),$(HOST))
LARGS:=$(LARGS) $(shell pkg-config --libs-only-L yaml-0.1)
endif
ifneq ($(findstring darwin,$(TARGET)),)
LARGS:=$(LARGS) -licucore
endif
ifneq ($(findstring freebsd,$(TARGET)),)
LARGS:=$(LARGS) -liconv -lm -lgcc_eh -lpthread
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

ifneq ($(DRAKE_RTSROOT),)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
endif
ifneq ($(DRAKE_RTSDIR),)
GARGS:=$(GARGS) --RTS=$(DRAKE_RTSDIR)
else
endif

export ADA_PROJECT_PATH=
export ADA_INCLUDE_PATH=$(subst $(space),$(PATHLISTSEP),$(wildcard lib/*/source) $(wildcard lib/*/source/$(TARGET)))
export ADA_OBJECTS_PATH=

.PHONY: all clean test-vampire install-test find xref archive

all: site/vampire$(CGISUFFIX)

site/vampire$(CGISUFFIX): source/vampire-main.adb $(wildcard source/*.ad?) $(BUILDDIR) $(IMPORTDIR)
	$(GNATPREFIX)gnatmake -c $< $(MARGS) $(GARGS) -cargs $(CARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatbind $(basename $(notdir $<)).ali $(GARGS) $(BARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatlink -o ../$@ $(basename $(notdir $<)).ali $(GARGS) $(LARGS)

site/unlock$(CGISUFFIX): source/vampire-unlock.adb $(wildcard source/*.ad?) $(BUILDDIR)
	$(GNATPREFIX)gnatmake -c $< $(MARGS) $(GARGS) -cargs $(CARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatbind $(basename $(notdir $<)).ali $(GARGS) $(BARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatlink -o ../$@ $(basename $(notdir $<)).ali $(GARGS) $(LARGS)

site/dump-users-log$(EXESUFFIX): source/vampire-dump_users_log.adb $(BUILDDIR)
	$(GNATPREFIX)gnatmake -c $< $(MARGS) $(GARGS) -cargs $(CARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatbind $(basename $(notdir $<)).ali $(GARGS) $(BARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatlink -o ../$@ $(basename $(notdir $<)).ali $(GARGS) $(LARGS)

$(BUILDDIR):
	mkdir $@

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

clean:
	-rm -rf *.build

find:
	gnatfind -f -aIsource -aO$(BUILDDIR) $(X) $(GARGS) | sed 's/^$(subst /,\/,$(PWD))\///'

xref:
	gnatfind -f -r -aIsource -aO$(BUILDDIR) $(X) $(GARGS) | sed 's/^$(subst /,\/,$(PWD))\///'

archive:
	git archive --format=tar HEAD | bzip2 > site/vampire.tar.bz2
