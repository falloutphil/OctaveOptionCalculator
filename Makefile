HC         = ghc
BUILDDIR   = build/$(build)
BINDIR     = bin/$(build)

HC_OPTS    = -Wall -i$(BUILDDIR) -odir $(BUILDDIR) -hidir $(BUILDDIR) $(EXTRA_HC_OPTS)
HC_OPTS_RELEASE = -O2
HC_OPTS_DEBUG   =
HC_OPTS_PROFILE =  -prof -auto-all -caf-all -O2

HP2PS_OPTS = -e8in $(EXTRA_HP2PS_OPTS)

PACKAGES  = -package mtl

findSrcs  = $(shell find . -name '_darcs' -prune -o -name $(1) -print)
findClean = if [ -d $(1) ]; then find $(1) -name '_darcs' -prune -o -name $(2) -exec rm -f {} \; ; fi


PROG = OptionCalculator
SRCS := $(call findSrcs,"*.hs")
_OBJS = $(SRCS:.%.hs=$(BUILDDIR)%.o)
# Nasty fudge, ghc renames our
# object file with our Main in
# it.  Providing you stick to
# using the name of the final
# binary this will work.
OBJS = $(_OBJS:$(PROG).o=Main.o)

# default
build = release
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

all : $(PROG) 


$(PROG) : $(OBJS)
	rm -f $(BINDIR)/$@
	mkdir -p $(BINDIR)
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


# Create postscript file from profile run
%.hp : %
	$(BINDIR)/$< +RTS -hc -p -K100M


%.ps : %.hp
	hp2ps -c $< $(HP2PS_OPTS)

# PDF conversion to rotate 90deg!
%.pdf : %.ps
	ps2pdf $<

%.jpg : %.pdf
	gs -sDEVICE=jpeg -sOutputFile=$@ - < $<




# Standard suffix rules

# Clear suffixes
.SUFFIXES :
# Define ours
.SUFFIXES : .o .hi 

# This is a bit of a cheat.  Suffixes
# allow us to say any.o to any_other.hi
# freeing us from specific forms used
# above.  This is of use here because
# to get from any .o->.hi we don't actually
# have to do anything! That's what @: says. 
.o.hi:
	@:


clean :
	$(call findClean,./$(BUILDDIR),"*.hi")
	$(call findClean,./$(BUILDDIR),"*.o") 
	$(call findClean,./,"*~")  
	rm -f $(BINDIR)/$(PROG) $(PROG).aux $(PROG).hp Makefile.bak


depend : 
	$(HC) -M $(HC_OPTS) $(SRCS)


# DO NOT DELETE: Beginning of Haskell dependencies
build/profile/Random/Framework.o : Random/Framework.hs
build/profile/Random/Ranq1.o : Random/Ranq1.hs
build/profile/Random/Ranq1.o : build/profile/Random/Framework.hi
build/profile/Normal/Framework.o : Normal/Framework.hs
build/profile/Normal/Framework.o : build/profile/Random/Framework.hi
build/profile/Normal/BoxMuller.o : Normal/BoxMuller.hs
build/profile/Normal/BoxMuller.o : build/profile/Normal/Framework.hi
build/profile/Normal/BoxMuller.o : build/profile/Random/Framework.hi
build/profile/Normal/Acklam.o : Normal/Acklam.hs
build/profile/Normal/Acklam.o : build/profile/Normal/Framework.hi
build/profile/Normal/Acklam.o : build/profile/Random/Framework.hi
build/profile/Normal/Interface.o : Normal/Interface.hs
build/profile/Normal/Interface.o : build/profile/Normal/Acklam.hi
build/profile/Normal/Interface.o : build/profile/Normal/BoxMuller.hi
build/profile/MonteCarlo/DataStructures.o : MonteCarlo/DataStructures.hs
build/profile/MonteCarlo/Framework.o : MonteCarlo/Framework.hs
build/profile/MonteCarlo/Framework.o : build/profile/MonteCarlo/DataStructures.hi
build/profile/MonteCarlo/Framework.o : build/profile/Random/Framework.hi
build/profile/MonteCarlo/Framework.o : build/profile/Normal/Framework.hi
build/profile/MonteCarlo/European.o : MonteCarlo/European.hs
build/profile/MonteCarlo/European.o : build/profile/Normal/Framework.hi
build/profile/MonteCarlo/European.o : build/profile/MonteCarlo/Framework.hi
build/profile/MonteCarlo/Lookback.o : MonteCarlo/Lookback.hs
build/profile/MonteCarlo/Lookback.o : build/profile/Normal/Framework.hi
build/profile/MonteCarlo/Lookback.o : build/profile/MonteCarlo/DataStructures.hi
build/profile/MonteCarlo/Lookback.o : build/profile/MonteCarlo/Framework.hi
build/profile/MonteCarlo/Interface.o : MonteCarlo/Interface.hs
build/profile/MonteCarlo/Interface.o : build/profile/MonteCarlo/Lookback.hi
build/profile/MonteCarlo/Interface.o : build/profile/MonteCarlo/European.hi
build/profile/Misc/Debug.o : Misc/Debug.hs
build/profile/Maths/Prime.o : Maths/Prime.hs
build/profile/Random/Halton.o : Random/Halton.hs
build/profile/Random/Halton.o : build/profile/Maths/Prime.hi
build/profile/Random/Halton.o : build/profile/Random/Framework.hi
build/profile/Random/Interface.o : Random/Interface.hs
build/profile/Random/Interface.o : build/profile/Random/Ranq1.hi
build/profile/Random/Interface.o : build/profile/Random/Halton.hi
build/profile/FrameworkInterface.o : FrameworkInterface.hs
build/profile/FrameworkInterface.o : build/profile/MonteCarlo/Framework.hi
build/profile/FrameworkInterface.o : build/profile/Normal/Framework.hi
build/profile/FrameworkInterface.o : build/profile/MonteCarlo/DataStructures.hi
build/profile/FrameworkInterface.o : build/profile/MonteCarlo/Interface.hi
build/profile/FrameworkInterface.o : build/profile/Normal/Interface.hi
build/profile/FrameworkInterface.o : build/profile/Random/Interface.hi
build/profile/Main.o : OptionCalculator.hs
build/profile/Main.o : build/profile/FrameworkInterface.hi
build/profile/Main.o : build/profile/MonteCarlo/DataStructures.hi
build/profile/Main.o : build/profile/MonteCarlo/Interface.hi
build/profile/Main.o : build/profile/Normal/Interface.hi
build/profile/Main.o : build/profile/Random/Interface.hi
# DO NOT DELETE: End of Haskell dependencies
