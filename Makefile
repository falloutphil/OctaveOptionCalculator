HC       = ghc
BUILDDIR   = build
HC_OPTS  = -Wall -i$(BUILDDIR) -odir $(BUILDDIR) -hidir $(BUILDDIR) $(EXTRA_HC_OPTS)
PACKAGES = -package mtl

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

all : release

profile : HC_OPTS += -prof -auto-all -caf-all -fforce-recomp
profile : release

graph : profile
	./$(PROG) +RTS -hc -p -K100M
	hp2ps -e8in -c $(PROG).hp

release : HC_OPTS += -O2
release : $(PROG)

debug : $(PROG)

$(PROG) : $(OBJS)
	  rm -f $@
	  $(HC) -o $@ $(PACKAGES) $(HC_OPTS) $(OBJS)

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
	rm -f $(PROG) $(PROG).aux $(PROG).hp $(PROG).prof $(PROG).ps Makefile.bak

depend :
	ghc -M $(HC_OPTS) $(SRCS)


# DO NOT DELETE: Beginning of Haskell dependencies
build/Misc/Debug.o : Misc/Debug.hs
build/Random/Framework.o : Random/Framework.hs
build/Random/Ranq1.o : Random/Ranq1.hs
build/Random/Ranq1.o : build/Random/Framework.hi
build/MonteCarlo/DataStructures.o : MonteCarlo/DataStructures.hs
build/Normal/Framework.o : Normal/Framework.hs
build/Normal/Framework.o : build/Random/Framework.hi
build/MonteCarlo/Framework.o : MonteCarlo/Framework.hs
build/MonteCarlo/Framework.o : build/MonteCarlo/DataStructures.hi
build/MonteCarlo/Framework.o : build/Random/Framework.hi
build/MonteCarlo/Framework.o : build/Normal/Framework.hi
build/MonteCarlo/European.o : MonteCarlo/European.hs
build/MonteCarlo/European.o : build/Normal/Framework.hi
build/MonteCarlo/European.o : build/MonteCarlo/Framework.hi
build/MonteCarlo/Lookback.o : MonteCarlo/Lookback.hs
build/MonteCarlo/Lookback.o : build/Normal/Framework.hi
build/MonteCarlo/Lookback.o : build/MonteCarlo/DataStructures.hi
build/MonteCarlo/Lookback.o : build/MonteCarlo/Framework.hi
build/MonteCarlo/Interface.o : MonteCarlo/Interface.hs
build/MonteCarlo/Interface.o : build/MonteCarlo/Lookback.hi
build/MonteCarlo/Interface.o : build/MonteCarlo/European.hi
build/Normal/Acklam.o : Normal/Acklam.hs
build/Normal/Acklam.o : build/Normal/Framework.hi
build/Normal/Acklam.o : build/Random/Framework.hi
build/Normal/BoxMuller.o : Normal/BoxMuller.hs
build/Normal/BoxMuller.o : build/Normal/Framework.hi
build/Normal/BoxMuller.o : build/Random/Framework.hi
build/Normal/Interface.o : Normal/Interface.hs
build/Normal/Interface.o : build/Normal/Acklam.hi
build/Normal/Interface.o : build/Normal/BoxMuller.hi
build/Maths/Prime.o : Maths/Prime.hs
build/Random/Halton.o : Random/Halton.hs
build/Random/Halton.o : build/Maths/Prime.hi
build/Random/Halton.o : build/Random/Framework.hi
build/Random/Interface.o : Random/Interface.hs
build/Random/Interface.o : build/Random/Ranq1.hi
build/Random/Interface.o : build/Random/Halton.hi
build/FrameworkInterface.o : FrameworkInterface.hs
build/FrameworkInterface.o : build/MonteCarlo/Framework.hi
build/FrameworkInterface.o : build/Normal/Framework.hi
build/FrameworkInterface.o : build/MonteCarlo/DataStructures.hi
build/FrameworkInterface.o : build/MonteCarlo/Interface.hi
build/FrameworkInterface.o : build/Normal/Interface.hi
build/FrameworkInterface.o : build/Random/Interface.hi
build/Main.o : OptionCalculator.hs
build/Main.o : build/FrameworkInterface.hi
build/Main.o : build/MonteCarlo/DataStructures.hi
build/Main.o : build/MonteCarlo/Interface.hi
build/Main.o : build/Normal/Interface.hi
build/Main.o : build/Random/Interface.hi
# DO NOT DELETE: End of Haskell dependencies
