#        ADIFOR script prefix
#
AD_SCRIPT = rosenbrock
AD_OUTPUT_DIR = GeneratedFiles

LAPACK= Source/dlange.o Source/dgesv.o Source/lsame.o Source/dlassq.o \
	Source/xerbla.o Source/dgetrf.o Source/dgetrs.o Source/ilaenv.o \
	Source/dgetf2.o Source/dlaswp.o 

BLAS=   Source/dtrsm.o Source/dgemm.o Source/idamax.o \
	Source/dswap.o Source/dscal.o Source/dger.o


# Needed only when using the old Sun Fortran compiler.  Use AD_ARCH instead
# under normal circumstances.
AD_COMP = sun4-SC1.0
# AD_COMP = $(AD_ARCH)

adnewton: Source/adnewton.o $(AD_OUTPUT_DIR)/g_func.o Source/func.o \
	  $(LAPACK) $(BLAS)
	f77 -o adnewton Source/adnewton.o $(AD_OUTPUT_DIR)/g_func.o \
		Source/func.o $(LAPACK) $(BLAS) \
		$(AD_LIB)/lib/ReqADIntrinsics-$(AD_COMP).o \
		$(AD_LIB)/lib/libADIntrinsics-$(AD_COMP).a

newton: Source/newton.o Source/func.o Source/fprime.o $(LAPACK) $(BLAS)
	f77 -o newton Source/newton.o Source/func.o Source/fprime.o \
		$(LAPACK) $(BLAS)

Source/%.o: Source/%.f
	f77 -o $@ -c $(FFLAGS) Source/$*.f

$(AD_OUTPUT_DIR)/%.o: $(AD_OUTPUT_DIR)/%.f
	f77 -o $@ -c $(FFLAGS) $(AD_OUTPUT_DIR)/$*.f

$(AD_OUTPUT_DIR)/g_func.f: $(AD_SCRIPT).adf
	Adifor2.0 AD_SCRIPT=$(AD_SCRIPT).adf

clean:
	/bin/rm -rf AD_cache $(AD_OUTPUT_DIR) 
	/bin/rm -f Source/*.o

