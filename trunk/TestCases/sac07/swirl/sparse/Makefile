TIMER = libTimer-$(AD_ARCH).a

AD_COMP = sun4-SC1.0

swirl_sparse: swirl_sparse_driver.o random.o g_dsfdfj.o dsfdfj.o
	f77 -o swirl_sparse swirl_sparse_driver.o color.o random.o g_dsfdfj.o dsfdfj.o $(AD_LIB)/lib/ReqADIntrinsics-$(AD_COMP).o $(AD_LIB)/lib/libADIntrinsics-$(AD_COMP).a $(TIMER) $(AD_LIB)/lib/libSparsLinC-$(AD_COMP).a

g_dsfdfj.o: g_dsfdfj.f
	$(FC) $(FFLAGS) -c g_dsfdfj.f

g_dsfdfj.f: ../swirl_sparse.adf ../main.f ../dsfdfj.f
	cd ..; Adifor2.0 AD_SCRIPT=swirl_sparse.adf

clean:
	rm -f *.A *.aux *~ *.o
