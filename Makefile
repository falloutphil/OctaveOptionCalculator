HC         = ghc
BUILDDIR   = build/$(build)
BINDIR     = bin/$(build)
HC_OPTS    = -Wall -i$(BUILDDIR) -odir $(BUILDDIR) -hidir $(BUILDDIR) $(EXTRA_HC_OPTS)
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

# Key compile params on $(build)
release = -O2
debug   =
profile =  -prof -auto-all -caf-all -O2
 

profile : HC_OPTS += -prof -auto-all -caf-all -O2
profile : $(PROG)

debug : $(PROG)

release : HC_OPTS += -O2
release : $(PROG)

all : release	 

graph : $(BINDIR)/$(PROG).ps

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

%.hp : profile
	./$* +RTS -hc -p -K100M
	mv *.hp $(BINDIR)
	mv *.prof $(BINDIR)

%.ps : %.hp
	hp2ps -c $< $(HP2PS_OPTS)
	mv *.aux $(BINDIR)
	mv *.ps $(BINDIR)


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
	rm -f $(BINDIR)/$(PROG) $(BINDIR)/$(PROG).aux $(BINDIR)/$(PROG).hp $(BINDIR)/$(PROG).prof $(BINDIR)/$(PROG).ps Makefile.bak


depend : 
	$(HC) -M $(HC_OPTS) $($(build)) $(SRCS)


# DO NOT DELETE: Beginning of Haskell dependencies
build/release/Misc/Debug.o : Misc/Debug.hs
build/release/Random/Framework.o : Random/Framework.hs
build/release/Random/Ranq1.o : Random/Ranq1.hs
build/release/Random/Ranq1.o : build/release/Random/Framework.hi
build/release/MonteCarlo/DataStructures.o : MonteCarlo/DataStructures.hs
build/release/Normal/Framework.o : Normal/Framework.hs
build/release/Normal/Framework.o : build/release/Random/Framework.hi
build/release/MonteCarlo/Framework.o : MonteCarlo/Framework.hs
build/release/MonteCarlo/Framework.o : build/release/MonteCarlo/DataStructures.hi
build/release/MonteCarlo/Framework.o : build/release/Random/Framework.hi
build/release/MonteCarlo/Framework.o : build/release/Normal/Framework.hi
build/release/MonteCarlo/European.o : MonteCarlo/European.hs
build/release/MonteCarlo/European.o : build/release/Normal/Framework.hi
build/release/MonteCarlo/European.o : build/release/MonteCarlo/Framework.hi
build/release/MonteCarlo/Lookback.o : MonteCarlo/Lookback.hs
build/release/MonteCarlo/Lookback.o : build/release/Normal/Framework.hi
build/release/MonteCarlo/Lookback.o : build/release/MonteCarlo/DataStructures.hi
build/release/MonteCarlo/Lookback.o : build/release/MonteCarlo/Framework.hi
build/release/MonteCarlo/Interface.o : MonteCarlo/Interface.hs
build/release/MonteCarlo/Interface.o : build/release/MonteCarlo/Lookback.hi
build/release/MonteCarlo/Interface.o : build/release/MonteCarlo/European.hi
build/release/Normal/Acklam.o : Normal/Acklam.hs
build/release/Normal/Acklam.o : build/release/Normal/Framework.hi
build/release/Normal/Acklam.o : build/release/Random/Framework.hi
build/release/Normal/BoxMuller.o : Normal/BoxMuller.hs
build/release/Normal/BoxMuller.o : build/release/Normal/Framework.hi
build/release/Normal/BoxMuller.o : build/release/Random/Framework.hi
build/release/Normal/Interface.o : Normal/Interface.hs
build/release/Normal/Interface.o : build/release/Normal/Acklam.hi
build/release/Normal/Interface.o : build/release/Normal/BoxMuller.hi
build/release/Maths/Prime.o : Maths/Prime.hs
build/release/Random/Halton.o : Random/Halton.hs
build/release/Random/Halton.o : build/release/Maths/Prime.hi
build/release/Random/Halton.o : build/release/Random/Framework.hi
build/release/Random/Interface.o : Random/Interface.hs
build/release/Random/Interface.o : build/release/Random/Ranq1.hi
build/release/Random/Interface.o : build/release/Random/Halton.hi
build/release/FrameworkInterface.o : FrameworkInterface.hs
build/release/FrameworkInterface.o : build/release/MonteCarlo/Framework.hi
build/release/FrameworkInterface.o : build/release/Normal/Framework.hi
build/release/FrameworkInterface.o : build/release/MonteCarlo/DataStructures.hi
build/release/FrameworkInterface.o : build/release/MonteCarlo/Interface.hi
build/release/FrameworkInterface.o : build/release/Normal/Interface.hi
build/release/FrameworkInterface.o : build/release/Random/Interface.hi
build/release/Main.o : OptionCalculator.hs
build/release/Main.o : build/release/FrameworkInterface.hi
build/release/Main.o : build/release/MonteCarlo/DataStructures.hi
build/release/Main.o : build/release/MonteCarlo/Interface.hi
build/release/Main.o : build/release/Normal/Interface.hi
build/release/Main.o : build/release/Random/Interface.hi
# DO NOT DELETE: End of Haskell dependencies
