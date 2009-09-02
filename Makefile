

# Default build type (profile if not specified on command line)
build    := profile
BUILDDIR := build/$(build)
BINDIR   := bin/$(build)
LIBDIR   := lib/$(build)

# Compiler, etc defaults
HC              := ghc
HC_OPTS         := -Wall -i$(BUILDDIR) -odir $(BUILDDIR) -hidir $(BUILDDIR) $(EXTRA_HC_OPTS)
HC_OPTS_RELEASE := -O2
HC_OPTS_DEBUG   :=
HC_OPTS_PROFILE := -prof -auto-all -caf-all -O2
HC_OPTS_FFI     := -O0 
PACKAGES        := -package mtl
OC              := mkoctfile
OC_OPTS         := -v -lCInterface -L$(LIBDIR)

# Options for heap graph
HP2PS_OPTS := -e8in $(EXTRA_HP2PS_OPTS)

# Emacs source definitions
HASKTAGS   := hasktags -e
ETAGS      := etags 
TAGFILE    := TAGS

# key on build= argument
ifeq "$(build)" "profile"
HC_OPTS += $(HC_OPTS_PROFILE)
else
ifeq "$(build)" "debug"
HC_OPTS += $(HC_OPTS_DEBUG)
else
ifeq "$(build)" "release"
HC_OPTS += $(HC_OPTS_RELEASE)
else
ifeq "$(build)" "ffi"
HC_OPTS += $(HC_OPTS_FFI)
endif
endif
endif
endif

# Unix regulars
MV    := mv -f
AWK   := awk
SORT  := sort
PR    := pr
FIND  := find
MKDIR := mkdir -p
RM    := rm -rf

# Functions
findSrcs  = $(shell $(FIND) $(1) -name '_darcs' -prune -o -name $(2) -print)
findClean = if [ -d $(1) ]; then $(FIND) $(1) -name '_darcs' -prune -o -name $(2) -exec $(RM) {} \; ; fi

# Program and Library names
PROG    := OptionCalculator
FFI_LIB := libCInterface.so

# Sources and Objects for various targets
COMMON_HS_SRCS := $(call findSrcs, "Maths", "*.hs")
COMMON_HS_SRCS += $(call findSrcs, "Misc", "*.hs")
COMMON_HS_SRCS += $(call findSrcs, "MonteCarlo", "*.hs")
COMMON_HS_SRCS += $(call findSrcs, "Normal", "*.hs")
COMMON_HS_SRCS += $(call findSrcs, "Random", "*.hs")
COMMON_HS_SRCS += FrameworkInterface.hs
EXE_HS_SRCS    := $(COMMON_HS_SRCS) OptionCalculator.hs
OCTAVE_HS_SRCS := $(COMMON_HS_SRCS) $(call findSrcs, "FFI/Octave", "*.hs") 
_EXE_HS_OBJS   :=  $(EXE_HS_SRCS:%.hs=$(BUILDDIR)/%.o)
# Nasty fudge, ghc renames our
# object file with our Main in
# it.  Providing you stick to
# using the name of the final
# binary this will work.
EXE_HS_OBJS    := $(_EXE_HS_OBJS:$(PROG).o=Main.o)
OCTAVE_HS_OBJS := $(OCTAVE_HS_SRCS:%.hs=$(BUILDDIR)/%.o)

# File for dependancies
DEPEND := $(BUILDDIR)/depend.mk

# Complete list of sources 
# only used for tags and emacs
# Haskell
HSRCS  := $(call findSrcs,".", "*.hs")
# C/C++
CSRCS  := $(call findSrcs,".", "*.h")
CSRCS  += $(call findSrcs,".", "*.c")
CSRCS  += $(call findSrcs,".", "*.cpp")
# Octave/Matlab
MSRCS  := $(call findSrcs,".", "*.m")

# None-file targets
.PHONY : clean_build clean_results clean_emacs clean_depend clean all graph help emacs octave

all : $(PROG) octave

graph : $(PROG).jpg
	$(MV) $(PROG).prof $(PROG).prof.txt
	$(RM) $(PROG).aux

clean_build : clean_depend
	$(call findClean,./$(BUILDDIR),"*.hi")
	$(call findClean,./$(BUILDDIR),"*.o")
	$(call findClean,./FFI,"*.o")
	$(RM) $(BINDIR)/*
	$(RM) $(LIBDIR)/*  

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

# Load up the project and the function definitions
emacs : $(TAGFILE)
	emacs -f visit-tags-table Makefile $(HSRCS) $(CSRCS) $(MSRCS) &

# Octave binaries
octave : hs_init.oct hs_exit.oct price_option.oct

# Create function definitions
# Octave example from
# http://www.gnu.org/software/emacs/manual/html_node/emacs/Etags-Regexps.html
$(TAGFILE) : Makefile $(HSRCS)
	$(HASKTAGS) $(HSRCS)
	$(ETAGS) -a Makefile
	$(ETAGS) -a $(CSRCS)
	$(ETAGS) -a --language=none \
                    --regex='/[ \t]*function.*=[ \t]*\([^ \t]*\)[ \t]*(/\1/' \
                    --regex='/###key \(.*\)/\1/' \
                    --regex='/[ \t]*global[ \t].*/' \
                    $(MSRCS)

# Build the actual program!
$(PROG) : $(EXE_HS_OBJS)
	$(RM) $(BINDIR)/$@
	$(MKDIR) $(BINDIR)
	$(HC) -o $(BINDIR)/$@ $(PACKAGES) $(HC_OPTS) $^

# Create FFI Interface library
$(FFI_LIB) : FFI/Octave/CInterface.c $(BUILDDIR)/FFI/Octave/OptionInterface_stub.o $(OCTAVE_HS_OBJS) 
	$(RM) $(LIBDIR)/$@
	$(MKDIR) $(LIBDIR)
	$(HC) -o $(LIBDIR)/$@ $(PACKAGES) -package array $(HC_OPTS) -no-hs-main -optl '-shared' $^

# Create Octave binary
%.oct : FFI/Octave/%.cpp $(FFI_LIB)
	$(MKDIR) $(BINDIR)
	$(OC) $(OC_OPTS) $< -o $(BINDIR)/$@

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

# FFI stub generation is automatic
# when the FFI haskell file is compiled
$(BUILDDIR)/%_stub.o : $(BUILDDIR)/%.o
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
	$(HC) -M -dep-makefile $@ $(HC_OPTS) $(PROG).hs FFI/Octave/OptionInterface.hs 


# Create postscript file from profile run
%.hp : %
	$(BINDIR)/$< +RTS -hc -p -K100M -sstderr > $<.sstderr.txt 2>&1

%.ps : %.hp
	hp2ps -c $< $(HP2PS_OPTS)

# PDF conversion to rotate 90deg!
%.pdf : %.ps
	ps2pdf $<

# JPEG output for website
%.jpg : %.pdf
	gs -sDEVICE=jpeg -sOutputFile=$@ - < $<

