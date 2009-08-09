HC       = ghc
HC_OPTS  = -Wall $(EXTRA_HC_OPTS)
PACKAGES = -package mtl

findSrcs = $(shell find -name '_darcs' -prune -o -name $(1) -print)
findClean = find -name '_darcs' -prune -o -name $(1) -exec rm -f {} \;

SRCS := $(call findSrcs,"*.hs") 
OBJS = $(SRCS:.hs=.o)
PROG = OptionCalculator

.SUFFIXES : .o .hs .hi .lhs .hc .s

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

# Standard suffix rules
.o.hi:
	@:

.lhs.o:
	$(HC) -c $< $(HC_OPTS)

.hs.o:
	$(HC) -c $< $(HC_OPTS)

.o-boot.hi-boot:
	@:

.lhs-boot.o-boot:
	$(HC) -c $< $(HC_OPTS)

.hs-boot.o-boot:
	$(HC) -c $< $(HC_OPTS)

clean :
	$(call findClean,"*.hi")
	$(call findClean,"*.o") 
	$(call findClean,"*~")  
	rm -f $(PROG) $(PROG).aux $(PROG).hp $(PROG).prof $(PROG).ps Makefile.bak

depend :
	ghc -M $(HC_OPTS) $(SRCS)


# DO NOT DELETE: Beginning of Haskell dependencies
Misc/Debug.o : Misc/Debug.hs
Random/Framework.o : Random/Framework.hs
Random/Ranq1.o : Random/Ranq1.hs
Random/Ranq1.o : Random/Framework.hi
MonteCarlo/DataStructures.o : MonteCarlo/DataStructures.hs
Normal/Framework.o : Normal/Framework.hs
Normal/Framework.o : Random/Framework.hi
MonteCarlo/Framework.o : MonteCarlo/Framework.hs
MonteCarlo/Framework.o : MonteCarlo/DataStructures.hi
MonteCarlo/Framework.o : Random/Framework.hi
MonteCarlo/Framework.o : Normal/Framework.hi
MonteCarlo/European.o : MonteCarlo/European.hs
MonteCarlo/European.o : Normal/Framework.hi
MonteCarlo/European.o : MonteCarlo/Framework.hi
MonteCarlo/Lookback.o : MonteCarlo/Lookback.hs
MonteCarlo/Lookback.o : Normal/Framework.hi
MonteCarlo/Lookback.o : MonteCarlo/DataStructures.hi
MonteCarlo/Lookback.o : MonteCarlo/Framework.hi
MonteCarlo/Interface.o : MonteCarlo/Interface.hs
MonteCarlo/Interface.o : MonteCarlo/Lookback.hi
MonteCarlo/Interface.o : MonteCarlo/European.hi
Normal/Acklam.o : Normal/Acklam.hs
Normal/Acklam.o : Normal/Framework.hi
Normal/Acklam.o : Random/Framework.hi
Normal/BoxMuller.o : Normal/BoxMuller.hs
Normal/BoxMuller.o : Normal/Framework.hi
Normal/BoxMuller.o : Random/Framework.hi
Normal/Interface.o : Normal/Interface.hs
Normal/Interface.o : Normal/Acklam.hi
Normal/Interface.o : Normal/BoxMuller.hi
Maths/Prime.o : Maths/Prime.hs
Random/Halton.o : Random/Halton.hs
Random/Halton.o : Maths/Prime.hi
Random/Halton.o : Random/Framework.hi
Random/Interface.o : Random/Interface.hs
Random/Interface.o : Random/Ranq1.hi
Random/Interface.o : Random/Halton.hi
FrameworkInterface.o : FrameworkInterface.hs
FrameworkInterface.o : MonteCarlo/Framework.hi
FrameworkInterface.o : Normal/Framework.hi
FrameworkInterface.o : MonteCarlo/DataStructures.hi
FrameworkInterface.o : MonteCarlo/Interface.hi
FrameworkInterface.o : Normal/Interface.hi
FrameworkInterface.o : Random/Interface.hi
OptionCalculator.o : OptionCalculator.hs
OptionCalculator.o : FrameworkInterface.hi
OptionCalculator.o : MonteCarlo/DataStructures.hi
OptionCalculator.o : MonteCarlo/Interface.hi
OptionCalculator.o : Normal/Interface.hi
OptionCalculator.o : Random/Interface.hi
# DO NOT DELETE: End of Haskell dependencies
