empty :=
space :=$(empty) $(empty)

HOST=$(shell gcc -dumpmachine)
export TARGET=$(HOST)

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

CARGS:=-Os -momit-leaf-frame-pointer -gnatn -gnat2012 -gnatwaIFK.R
BARGS:=
LARGS:=-lm
ifeq ($(BUILD),release)
ifneq ($(findstring apple-darwin,$(TARGET)),)
LARGS:=$(LARGS) -dead_strip
ifneq ($(WHYLIVE),)
LARGS:=$(LARGS) -Xlinker -why_live -Xlinker $(WHYLIVE)
endif
else
CARGS:=$(CARGS) -ffunction-sections -fdata-sections
LARGS:=$(LARGS) -Xlinker --gc-sections -s
endif
else
CARGS:=$(CARGS) -g -gnata
BARGS:=$(BARGS) -E
LARGS:=$(LARGS) -g
endif
ifneq ($(TARGET),$(HOST))
LARGS:=$(LARGS) -lgcc_eh
endif
ifneq ($(LTO),)
CARGS:=$(CARGS) -flto -fwhole-program
LARGS:=$(LARGS) -flto -fwhole-program
endif

MARGS:=-cargs $(CARGS) -bargs $(BARGS) -largs $(LARGS)

ifneq ($(DRAKE_RTSROOT),)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)
endif
ifneq ($(DRAKE_RTSDIR),)
IMPORTDIR:=
MARGS:=--RTS=$(abspath $(DRAKE_RTSDIR)) $(MARGS) -lm
endif

export ADA_PROJECT_PATH=
export ADA_INCLUDE_PATH=$(subst $(space),$(PATHLISTSEP),$(IMPORTDIR) $(abspath $(wildcard lib/*/source) $(wildcard lib/*/source/$(TARGET))))
export ADA_OBJECTS_PATH=

.PHONY: all clean test-vampire install-test find xref archive

all: site/vampire$(CGISUFFIX)

site/vampire$(CGISUFFIX): source/vampire-main.adb $(wildcard source/*.ad?) $(BUILDDIR) $(IMPORTDIR)
	cd $(BUILDDIR) && $(GNATMAKE) -o ../$@ ../$< $(MARGS)

site/unlock$(CGISUFFIX): source/vampire-unlock.adb $(wildcard source/*.ad?) $(BUILDDIR)
	cd $(BUILDDIR) && $(GNATMAKE) -o ../$@ ../$< $(MARGS)

site/dump-users-log$(EXESUFFIX): source/vampire-dump_users_log.adb $(BUILDDIR)
	cd $(BUILDDIR) && $(GNATMAKE) -o ../$@ ../$< $(MARGS)

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
	cd $(TESTDIR) && gdb .$(DIRSEP)vampire$(CGISUFFIX)

$(BUILDDIR):
	mkdir $@

ifneq ($(IMPORTDIR),)
$(IMPORTDIR): lib/import.h
	headmaster --gcc $(TARGET)-gcc --to ada -p -D $(IMPORTDIR) $+
endif

clean:
	-rm -rf build* import*

find:
	gnatfind -f --RTS=$(DRAKE_RTSDIR) -aIsource -aO$(BUILDDIR) $(X)

xref:
	gnatfind -f -r --RTS=$(DRAKE_RTSDIR) -aIsource -aO$(BUILDDIR) $(X)

archive:
	git archive-all site/vampire.tar.bz2
