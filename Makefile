
# default is profile build
build    = profile
BUILDDIR = build/$(build)
BINDIR   = bin/$(build)

HC              = ghc
HC_OPTS         = -Wall -i$(BUILDDIR) -odir $(BUILDDIR) -hidir $(BUILDDIR) $(EXTRA_HC_OPTS)
HC_OPTS_RELEASE = -O2
HC_OPTS_DEBUG   =
HC_OPTS_PROFILE =  -prof -auto-all -caf-all -O2
PACKAGES        = -package mtl

# key on build= argument
ifeq "$(build)" "profile"
HC_OPTS += $(HC_OPTS_PROFILE)
else
ifeq "$(build)" "debug"
HC_OPTS += $(HC_OPTS_DEBUG)
else
ifeq "$(build)" "release"
HC_OPTS += $(HC_OPTS_RELEASE)
endif
endif
endif


HP2PS_OPTS = -e8in $(EXTRA_HP2PS_OPTS)

# Unix regulars
MV    = mv -f
AWK   = awk
SORT  = sort
PR    = pr
FIND  = find
MKDIR = mkdir -p

findSrcs  = $(shell $(FIND) . -name '_darcs' -prune -o -name $(1) -print)
findClean = if [ -d $(1) ]; then $(FIND) $(1) -name '_darcs' -prune -o -name $(2) -exec $(RM) {} \; ; fi


PROG = OptionCalculator
SRCS := $(call findSrcs,"*.hs")
_OBJS = $(SRCS:.%.hs=$(BUILDDIR)%.o)
# Nasty fudge, ghdefault is profile build
build    = profile
BUILDDIR = build/$(build)
BINDIR   = bin/$(build)

HC              = ghc
HC_OPTS         = -Wall -i$(BUILDDIR) -odir $(BUILDDIR) -hidir $(BUILDDIR) $(EXTRA_HC_OPTS)
HC_OPTS_RELEASE = -O2
HC_OPTS_DEBUG   =
HC_OPTS_PROFILE =  -prof -auto-all -caf-all -O2
PACKAGES        = -package mtl

# key on build= argument
ifeq "$(build)" "profile"
HC_OPTS += $(HC_OPTS_PROFILE)
else
ifeq "$(build)" "debug"
HC_OPTS += $(HC_OPTS_DEBUG)
else
ifeq "$(build)" "release"
HC_OPTS += $(HC_OPTS_RELEASE)
endif
endif
endif


HP2PS_OPTS = -e8in $(EXTRA_HP2PS_OPTS)
HASKTAGS = hasktags -e
ETAGS = etags 
TAGFILE = TAGS

# Unix regulars
MV    = mv -f
AWK   = awk
SORT  = sort
PR    = pr
FIND  = find
MKDIR = mkdir -p

findSrcs  = $(shell $(FIND) . -name '_darcs' -prune -o -name $(1) -print)
findClean = if [ -d $(1) ]; then $(FIND) $(1) -name '_darcs' -prune -o -name $(2) -exec $(RM) {} \; ; fi


PROG = OptionCalculator
SRCS := $(call findSrcs,"*.hs")
_OBJS = $(SRCS:.%.hs=$(BUILDDIR)%.o)
# Nasty fudge, ghc renames our
# object file with our Main in
# it.  Providing you stick to
# using the name of the final
# binary this will work.
OBJS = $(_OBJS:$(PROG).o=Main.o)
DEPEND = $(BUILDDIR)/depend

# None file targets
.PHONY : clean_build clean_results clean_emacs clean_depend clean all graph help emacs

all : $(PROG) 

graph : $(PROG).jpg
	$(MV) $(PROG).prof $(PROG).prof.txt
	$(RM) $(PROG).aux

clean_build : clean_depend
	$(call findClean,./$(BUILDDIR),"*.hi")
	$(call findClean,./$(BUILDDIR),"*.o")  
	$(RM) $(BINDIR)/$(PROG)  

clean_results : 
	$(RM) $(PROG).jpg $(PROG).prof $(PROG).prof.txt $(PROG).sstderr.txt $(PROG).hp $(PROG).aux

clean_emacs :
	$(call findClean,./,"*~")
	$(call findClean,./,"#*#")
	$(RM) $(TAGFILE)

clean_depend :
	$(RM) $(DEPEND)

clean : clean_build clean_results clean_emacs 

help :
	$(MAKE) -p --question |                                                    \
	$(AWK) '/^[^.%][-A-Za-z0-9_]*:/ { print substr($$1, 1, length($$1)-1) }' | \
	$(SORT) | $(PR) -w80 -4 -l20

# Load up the project and the haskell tags
emacs : $(TAGFILE)
	emacs -f visit-tags-table Makefile $(SRCS) &

$(TAGFILE) : Makefile $(SRCS)
	$(HASKTAGS) $(SRCS)
	$(ETAGS) -a Makefile

$(PROG) : $(OBJS)
	$(RM) $(BINDIR)/$@
	$(MKDIR) $(BINDIR)
	$(HC) -o $(BINDIR)/$@ $(PACKAGES) $(HC_OPTS) $(OBJS)


# .o files are in our separate
# directory.  This maps them
# to their corresponding .hs file.
$(BUILDDIR)/%.o : %.hs
	$(HC) -c $< $(HC_OPTS)


# Special case as ghc dependency creator
# renames our Main object file if we
# use odir!
$(BUILDDIR)/Main.o : $(PROG).hs
	$(HC) -c $< $(HC_OPTS)

# This is of use here because
# to get from any .o->.hi we don't actually
# have to do anything! That's what @: says. 
%.hi : %.o 
	@:

# Get dependencies, '-' kills warning
-include $(DEPEND)

# If they don't exist create them!
# Note - this is automatic the first
# time but needs cleaning for any
# subsequent dependency change
# Should do better here!
$(DEPEND) :
	$(MKDIR) $(@D) 
	$(HC) -M -dep-makefile $@ $(HC_OPTS) $(PROG).hs


# Create postscript file from profile run
%.hp : %
	$(BINDIR)/$< +RTS -hc -p -K100M -sstderr > $<.sstderr.txt 2>&1

%.ps : %.hp
	hp2ps -c $< $(HP2PS_OPTS)

# PDF conversion to rotate 90deg!
%.pdf : %.ps
	ps2pdf $<

%.jpg : %.pdf
	gs -sDEVICE=jpeg -sOutputFile=$@ - < $<

