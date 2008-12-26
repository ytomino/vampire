empty :=
space :=$(empty) $(empty)

ifneq ($(ComSpec),)
export CGISUFFIX=.exe
HOST=i686-pc-mingw32
DIRSEP=\$(empty)
PATHLISTSEP=;
else
export CGISUFFIX=.cgi
HOST=i686-pc-freebsd6
DIRSEP=/
PATHLISTSEP=:
endif

export TARGET=$(HOST)

ifneq ($(TARGET),$(HOST))
GNATMAKE=$(TARGET)-gnatmake
else
GNATMAKE=gnatmake
endif

export BUILDTYPE=debug

export BUILDDIR:=build
override BUILDDIR:=$(abspath $(BUILDDIR))

LIB_PROJECTS=$(wildcard lib/*/*.gpr)
ifneq ($(LIB_PROJECTS),)
export ADA_PROJECT_PATH=$(subst $(space),$(PATHLISTSEP),$(dir $(LIB_PROJECTS)))
else
endif

LIB_MAKEFILES=$(wildcard lib/*/makefile)
LIB_MAKE=$(addsuffix .mk,$(LIB_MAKEFILES))

.PHONY: all clean

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
