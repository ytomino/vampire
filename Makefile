HOST:=$(shell gcc -dumpmachine)
TARGET=$(HOST)

ifneq (,$(findstring mingw,$(TARGET)))
 EXESUFFIX=.exe
 CGISUFFIX=.exe
else
 EXESUFFIX=
 CGISUFFIX=.cgi
endif

ifneq ($(TARGET),$(HOST))
 GNATPREFIX=$(TARGET)-
else
 GNATPREFIX=
endif

BUILD=release

ifeq ($(BUILD),debug)
 BUILDDIR=$(TARGET).build/debug
 LINK=
else
 BUILDDIR=$(TARGET).build
 LINK=gc
endif

GARGS:=
MARGS:=-C -D $(BUILDDIR) $(addprefix -I,$(wildcard lib/*/source) $(wildcard lib/*/source/$(TARGET))) -gnatA
CARGS:=-pipe -gnatef -gnatwaIFK $(addprefix -gnatec=,$(abspath $(wildcard *.adc)))
BARGS:=-x
LARGS:=

ifeq ($(TARGET),$(HOST))
 LARGS:=$(LARGS) $(shell pkg-config --libs-only-L yaml-0.1)
endif
ifneq ($(findstring darwin,$(TARGET)),)
 LARGS:=$(LARGS) -licucore
else ifneq ($(findstring freebsd,$(TARGET)),)
 LARGS:=$(LARGS) -liconv -lm -lgcc_eh -lpthread
endif

ifeq ($(LINK),gc)
 ifneq ($(findstring darwin,$(TARGET)),)
  LARGS:=$(LARGS) -dead_strip
  ifneq ($(WHYLIVE),)
   LARGS:=$(LARGS) -Wl,-why_live,$(WHYLIVE)
  endif
 else
  CARGS:=$(CARGS) -ffunction-sections -fdata-sections
  LARGS:=$(LARGS) -Wl,--gc-sections
 endif
else ifeq ($(LINK),lto)
 CARGS:=$(CARGS) -flto
 LARGS:=$(LARGS) -flto -Wl,--gc-sections
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
 ifneq ($(findstring freebsd,$(TARGET))$(findstring linux-gnu,$(TARGET)),)
  LARGS:=$(LARGS) -Wl,--compress-debug-sections=zlib
 endif
endif

ifneq ($(DRAKE_RTSROOT),)
 VERSION:=$(shell gcc -dumpversion)
 ifneq ($(and $(filter debug,$(BUILD)),$(wildcard $(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)/debug)),)
  DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)/debug
 else
  DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
 endif
endif
ifneq ($(DRAKE_RTSDIR),)
 GARGS:=$(GARGS) --RTS=$(DRAKE_RTSDIR)
endif

TESTDIR?=$(HOME)/Documents/Sites/local/vampire

.PHONY: all clean test-vampire install-test xfind xfindall

all: site/vampire$(CGISUFFIX)
	$(foreach I, \
		$(filter-out $(BUILDDIR)/.stamp, \
			$(wildcard *.build/.stamp) $(wildcard *.build/debug/.stamp)), \
		rm $(I);)

site/vampire$(CGISUFFIX): source/vampire-main.adb $(wildcard source/*.ad?) $(BUILDDIR)/.stamp
	$(GNATPREFIX)gnatmake -c $< $(MARGS) $(GARGS) -cargs $(CARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatbind $(basename $(notdir $<)).ali $(GARGS) $(BARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatlink -o "$(abspath $@)" \
		$(basename $(notdir $<)).ali $(GARGS) $(LARGS)

site/unlock$(CGISUFFIX): source/vampire-unlock.adb $(wildcard source/*.ad?) $(BUILDDIR)/.stamp
	$(GNATPREFIX)gnatmake -c $< $(MARGS) $(GARGS) -cargs $(CARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatbind $(basename $(notdir $<)).ali $(GARGS) $(BARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatlink -o "$(abspath $@)" \
		$(basename $(notdir $<)).ali $(GARGS) $(LARGS)

site/dump-users-log$(EXESUFFIX): source/vampire-dump_users_log.adb $(wildcard source/*.ad?) $(BUILDDIR)/.stamp
	$(GNATPREFIX)gnatmake -c $< $(MARGS) $(GARGS) -cargs $(CARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatbind $(basename $(notdir $<)).ali $(GARGS) $(BARGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatlink -o "$(abspath $@)" \
		$(basename $(notdir $<)).ali $(GARGS) $(LARGS)

$(BUILDDIR)/.stamp: | $(BUILDDIR)
	touch $@

$(BUILDDIR):
	mkdir -p $@

clean:
	-rm -r $(BUILDDIR)

DEBUGGER=gdb
export QUERY_STRING=

$(TESTDIR)/image:
	mkdir -p $@

$(TESTDIR)/users:
	mkdir -p $@

$(TESTDIR)/villages:
	mkdir -p $@

$(TESTDIR)/_data:
	mkdir -p $@

$(TESTDIR)/%: site/%
	install $< $(dir $@)

install-test: \
	$(TESTDIR)/vampire$(CGISUFFIX) \
	$(TESTDIR)/unlock$(CGISUFFIX) \
	$(TESTDIR)/cast \
	$(TESTDIR)/style.css \
	$(TESTDIR)/image \
	$(TESTDIR)/villages \
	$(TESTDIR)/_data \
	$(addprefix $(TESTDIR)/,$(notdir $(wildcard site/*.html))) \
	$(addprefix $(TESTDIR)/image/,$(notdir $(wildcard site/image/*.png)))

test-vampire: install-test
	cd $(TESTDIR) && $(DEBUGGER) ./vampire$(CGISUFFIX)

xfind:
	gnatfind -f -aIsource -aO$(BUILDDIR) $(X) $(GARGS) $(FARGS) | sed 's|^$(PWD)/||'

xfindall: FARGS+=-r
xfindall: xfind
