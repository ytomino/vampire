HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

VERSION=$(shell gcc -dumpversion)

ifneq (,$(findstring mingw,$(TARGET)))
EXESUFFIX=.exe
CGISUFFIX=.exe
else
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

ifeq ($(BUILD),debug)
LINK=
else
LINK=gc
endif

TESTDIR=~/Sites/cgi/vampire

GARGS:=
MARGS:=-C -D $(BUILDDIR) $(addprefix -I,$(wildcard lib/*/source) $(wildcard lib/*/source/$(TARGET))) -gnatA
CARGS:=-pipe -gnatef -gnatwaIFK.R $(addprefix -gnatec=,$(abspath $(wildcard *.adc)))
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
LARGS:=$(LARGS) -Xlinker --gc-sections
endif
endif
ifeq ($(LINK),lto)
CARGS:=$(CARGS) -flto
LARGS:=$(LARGS) -flto -Xlinker --gc-sections
ifneq ($(filter -lgcc_eh,$(LARGS)),)
LARGS:=$(filter-out -lgcc_eh,$(LARGS)) -static-libgcc
endif
endif

ifeq ($(BUILD),debug)
CARGS:=$(CARGS) -ggdb -Og -fno-guess-branch-probability -gnata
BARGS:=$(BARGS) -E
LARGS:=$(LARGS) -ggdb -Og
else
CARGS:=$(CARGS) -ggdb1 -Os -gnatB -gnatVn -gnatn2
BARGS:=$(BARGS) -E
LARGS:=$(LARGS) -ggdb1 -Os
endif

ifneq ($(DRAKE_RTSROOT),)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
endif
ifneq ($(DRAKE_RTSDIR),)
GARGS:=$(GARGS) --RTS=$(DRAKE_RTSDIR)
else
endif

.PHONY: all clean test-vampire install-test xfind xfindall archive

all: site/vampire$(CGISUFFIX)
	$(foreach I,$(filter-out $(BUILDDIR)/.stamp,$(wildcard *.build/.stamp)),rm $(I))

site/vampire$(CGISUFFIX): source/vampire-main.adb $(wildcard source/*.ad?) $(BUILDDIR)/.stamp
	$(GNATPREFIX)gnatmake -c $< $(MARGS) $(GARGS) -cargs $(CARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatbind $(basename $(notdir $<)).ali $(GARGS) $(BARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatlink -o ../$@ $(basename $(notdir $<)).ali $(GARGS) $(LARGS)

site/unlock$(CGISUFFIX): source/vampire-unlock.adb $(wildcard source/*.ad?) $(BUILDDIR)/.stamp
	$(GNATPREFIX)gnatmake -c $< $(MARGS) $(GARGS) -cargs $(CARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatbind $(basename $(notdir $<)).ali $(GARGS) $(BARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatlink -o ../$@ $(basename $(notdir $<)).ali $(GARGS) $(LARGS)

site/dump-users-log$(EXESUFFIX): source/vampire-dump_users_log.adb $(wildcard source/*.ad?) $(BUILDDIR)/.stamp
	$(GNATPREFIX)gnatmake -c $< $(MARGS) $(GARGS) -cargs $(CARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatbind $(basename $(notdir $<)).ali $(GARGS) $(BARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatlink -o ../$@ $(basename $(notdir $<)).ali $(GARGS) $(LARGS)

$(BUILDDIR)/.stamp: | $(BUILDDIR)
	touch $@

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
	cd $(TESTDIR) && $(DEBUGGER) ./vampire$(CGISUFFIX)

clean:
	-rm -rf *.build

xfind:
	gnatfind -f -aIsource -aO$(BUILDDIR) $(X) $(GARGS) $(FARGS) | sed 's|^$(PWD)/||'

xfindall: FARGS+=-r
xfindall: xfind

archive:
	git archive --format=tar HEAD | bzip2 > site/vampire.tar.bz2
