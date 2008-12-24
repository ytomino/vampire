ifneq ($(ComSpec),)
export CGISUFFIX=.exe
else
export CGISUFFIX=.cgi
endif

ifneq ($(TARGET),)
GNATMAKE=$(TARGET)-gnatmake
else
GNATMAKE=gnatmake
endif

ifneq ($(wildcard lib/*.gpr),)
export ADA_PROJECT_PATH=lib
else
endif

export BUILDTYPE=debug
BUILDDIR=build

.PHONY: all get-lib get-ase get-interfaces get-iconv get-dyayaml

all: site/vampire$(CGISUFFIX)

site/vampire$(CGISUFFIX): $(BUILDDIR)
	$(GNATMAKE) -P source/vampire.gpr -XBUILDDIR=../$(BUILDDIR)

site/unlock$(CGISUFFIX): $(BUILDDIR)
	$(GNATMAKE) -P source/unlock.gpr -XBUILDDIR=../$(BUILDDIR)

site/users$(CGISUFFIX): $(BUILDDIR)
	$(GNATMAKE) -P source/users.gpr -XBUILDDIR=../$(BUILDDIR)

site/shuffle$(CGISUFFIX): $(BUILDDIR)
	$(GNATMAKE) -P source/shuffle.gpr -XBUILDDIR=../$(BUILDDIR)

get-lib: get-ase get-interfaces get-iconv get-dyayaml

get-ase:
	svn export --force http://panathenaia.googlecode.com/svn/trunk/ase/source lib

get-interfaces:
	svn export --force http://panathenaia.googlecode.com/svn/trunk/yt-gnat/source lib

get-iconv:
	svn export --force http://panathenaia.googlecode.com/svn/trunk/iconv-gnat/source lib

get-dyayaml:
	svn export --force http://panathenaia.googlecode.com/svn/trunk/dyayaml/gnat lib

$(BUILDDIR):
	mkdir $(BUILDDIR)
