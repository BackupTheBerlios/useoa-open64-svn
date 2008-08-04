# Makefile
#############################################################################

# Configure compiler and compilation flags for platform
include Makeinclude.config

#############################################################################

WD := $(shell pwd)


#CXXFLAGS += -g -O0 -DOA_IRHANDLETYPE_UL \

CXXFLAGS += -O3 -DNDEBUG -DOA_IRHANDLETYPE_UL \
            -I. -I$(WHIRL2OA) $(OA_INC) \
			$(USER_CXXFLAGS) $(OPEN64_IFLAGS)

LDFLAGS  += $(CXXFLAGS)


EXES = $(EXE1) $(EXE2) extractDOT

# The Open64 test drivers
EXE1 = test-open64
#EXE2 = FORTRANtoOA


LIBS1 = $(OPEN64TARG)/be/be.so $(WHIRL2OA)/lib/Whirl2OA.a $(OA_LIB)

dep = $(WHIRL2OA)/lib/Whirl2OA.a


EXE1_OBJS = \
	test-open64.o \
	CmdLineParser.o \
	timer.o \
	Args.o 



EXE2_OBJS = \
#	FORTRANtoOA.o \
	CmdLineParser.o \
	Args.o

#############################################################################

all: build_log $(EXES) mark_compile_successful extractDOT
	@echo "OpenAnalysis Path is $<"
	@echo "$(OpenAnalysis) "

$(EXE1): $(EXE1_OBJS) $(LIBS1) 
	$(CXX) -o $@ $(LDFLAGS) $^

$(EXE2): $(EXE2_OBJS) $(LIBS1) 
	$(CXX) -o $@ $(LDFLAGS) $^

extractDOT: extractDOT.cpp
	$(CXX) -o extractDOT extractDOT.cpp

%.o : %.cpp
	$(CXX) -c -o $@ $(CXXFLAGS) $<

$(dep):
	make -C $(WHIRL2OA)

clean:
	@rm -f *.o $(EXES) 
	make clean -C $(WHIRL2OA)

build_log:
	@echo -e "\033[32mOutputing flags to build.log\033[0m"
	@date +"Compiling OAOpen64 on %D at %r %Z" > build.log
	@echo "Compile started by" `whoami` >> build.log
	@echo "OADIR:='$(OADIR)'"  >> build.log
	@echo "OPEN64TARG:='$(OPEN64TARG)'"  >> build.log
	@echo "CXXFLAGS:='$(CXXFLAGS)'"  >> build.log
	@echo "CCFLAGS:='$(CCFLAGS)'"  >> build.log
	@echo "PLATFORM:='$(PLATFORM)'"  >> build.log
	@echo "CXX:='$(CXX)'"  >> build.log 
	@echo "CC:='$(CC)'"  >> build.log 
	@echo "CXXLD:='$(CXXLD)'"  >> build.log 
	@echo "CCLD:='$(CCLD)'"  >> build.log

mark_compile_successful: 
	@echo "COMPILE_SUCCESFUL='yes'" >> build.log 
	@date +"Compilation finished on %D at %r %Z" >> build.log

clean_log: 
	@echo "Project cleaned" > build.log 

