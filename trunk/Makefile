
#############################################################################

# Configure compiler and compilation flags for platform
include Makeinclude.config

#############################################################################

SHELL = /bin/sh
WD := $(shell pwd)

#$(shell cd $(WD)/config/; ./hpcplatform)
OpenAnalysis = $(OPENANALYSISBASE)/$(PLATFORM)

#CXXFLAGS += -g -O0 -DOA_IRHANDLETYPE_UL \

CXXFLAGS += -pg -O0 -DOA_IRHANDLETYPE_UL \
            -I. -I$(OpenAnalysis)/include $(USER_CXXFLAGS) $(OPEN64_IFLAGS)
#LDFLAGS  += -g -I$(OpenAnalysis)/include -DOA_IRHANDLETYPE_UL -I$(OMEGALIBDIR)/include -I$(OMEGADIR)/basic/include

LDFLAGS  += $(CXXFLAGS)

#VPATH = Open64IRInterface

EXES = $(EXE1) $(EXE2) #$(EXE1a) $(EXE2a)

# The Open64 test drivers
EXE1 = test-open64
EXE2 = FORTRANtoOA

# The original mem-ref and alias test driver
EXE2a = testMemRefExpr-bogus
EXE2b = testAlias-bogus

LIBS1 = $(OPEN64TARG)/be/be.so $(WHIRL2OA)/lib/Whirl2OA.a -lOAul
LIBS2a = -lOAul #-laliasstart
LIBS2b = -lOAul #-laliasstart

VPATH += $(OpenAnalysis)/lib

EXE1_OBJS = \
	test-open64.o \
	CmdLineParser.o \
	Args.o

EXE2_OBJS = \
	FORTRANtoOA.o \
	CmdLineParser.o \
	Args.o

#EXE1_OBJS = \
#	test-open64.o \
#	Open64IRInterface.o \
#	WhirlGlobalStateUtils.o \
#	WhirlIO.o \
#	wn_attr.o \
#	stab_attr.o \
#	SymTab.o \
#	diagnostics.o \
#	CmdLineParser.o \
#	Args.o
#
#EXE2_OBJS = \
#	FORTRANtoOA.o \
#	Open64IRInterface.o \
#	WhirlGlobalStateUtils.o \
#	WhirlIO.o \
#	wn_attr.o \
#	stab_attr.o \
#	SymTab.o \
#	diagnostics.o \
#	CmdLineParser.o \
#	Args.o


EXE2a_OBJS = testMemRefExpr.o #bogus2OA.o
EXE2b_OBJS = testAlias.o      #bogus2OA.o


#############################################################################

all: $(EXES)
	@echo "NewOA Path is $<"
	@echo "$(OpenAnalysis) "

$(EXE1): $(EXE1_OBJS) $(LIBS1) 
	$(CXX) -o $@ $(LDFLAGS) $^

$(EXE2): $(EXE2_OBJS) $(LIBS1) 
	$(CXX) -o $@ $(LDFLAGS) $^

$(EXE2a): $(EXE2a_OBJS) $(LIBS2a)
	$(CXX) -o $@ $(LDFLAGS) $^

$(EXE2b): $(EXE2b_OBJS) $(LIBS2b)
	$(CXX) -o $@ $(LDFLAGS) $^

%.o : %.cpp
	$(CXX) -c -o $@ $(CXXFLAGS) $<

clean:
	@rm -f *.o $(EXES) 

