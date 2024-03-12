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
 BUILDDIR=$(TARGET).noindex/debug
 LINK=
else
 BUILDDIR=$(TARGET).noindex
 LINK=gc
endif

CFLAGS=-pipe
CFLAGS_ADA=-gnatef -gnatwaIFK
LDFLAGS=

ifeq ($(TARGET),$(HOST))
 YAML_LDFLAGS:=$(shell pkg-config --libs-only-L yaml-0.1)
 LDFLAGS+=$(YAML_LDFLAGS)
endif
ifneq ($(findstring darwin,$(TARGET)),)
 LDFLAGS+=-licucore
else ifneq ($(findstring freebsd,$(TARGET)),)
 LDFLAGS+=-liconv -lm -lgcc_eh -lpthread
endif

ifeq ($(LINK),gc)
 ifneq ($(findstring darwin,$(TARGET)),)
  LDFLAGS+=-dead_strip
  ifneq ($(WHYLIVE),)
   LDFLAGS+=-Wl,-why_live,$(WHYLIVE)
  endif
 else
  CFLAGS+=-ffunction-sections -fdata-sections
  LDFLAGS+=-Wl,--gc-sections
 endif
else ifeq ($(LINK),lto)
 CFLAGS+=-flto
 LDFLAGS+=-flto -Wl,--gc-sections
 ifneq ($(filter -lgcc_eh,$(LDFLAGS)),)
  LDFLAGS:=$(filter-out -lgcc_eh,$(LDFLAGS)) -static-libgcc
 endif
endif

ifeq ($(BUILD),debug)
 CFLAGS+=-ggdb -Og -fno-guess-branch-probability
 CFLAGS_ADA+=-gnata
 LDFLAGS+=-ggdb -Og
else
 CFLAGS+=-ggdb1 -Os
 CFLAGS_ADA+=-gnatB -gnatn2 -gnatVn
 LDFLAGS+=-ggdb1 -Os
 ifneq ($(findstring freebsd,$(TARGET))$(findstring linux-gnu,$(TARGET)),)
  LDFLAGS+=-Wl,--compress-debug-sections=zlib
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

GARGS=$(addprefix --RTS=,$(DRAKE_RTSDIR))
MARGS=-C -D $(BUILDDIR) -gnatA $(addprefix -gnatec=,$(wildcard *.adc)) \
      $(addprefix -I, \
        $(wildcard lib/*/source) $(wildcard lib/*/source/$(TARGET)))
CARGS=$(CFLAGS) $(CFLAGS_ADA)
BARGS=-E -x
LARGS=$(LDFLAGS)
FARGS=

TESTDIR?=$(HOME)/Documents/Sites/local/vampire

.PHONY: all clean test-vampire install-test xfind xfindall

all: site/vampire$(CGISUFFIX)
	$(foreach I, \
		$(filter-out $(BUILDDIR)/.stamp, \
			$(wildcard *.noindex/.stamp) $(wildcard *.noindex/debug/.stamp)), \
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

$(TESTDIR)/%$(CGISUFFIX): site/%$(CGISUFFIX)
	install $< $(dir $@)

$(TESTDIR)/%: site/%
	install -m 644 $< $(dir $@)

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
