# AD_COMP = sun4-SC1.0
AD_COMP = $(AD_OS)
TIMER = ../timer/libTimer-$(AD_OS).a

msa_ps_sparse: s_dmsasep.o driver_ps_sparse.o dmsasep.o dmsabc.o dmsafg.o dmsamain.o $(TIMER)
	f77 $(FFLAGS) -o msa_ps_sparse s_dmsasep.o driver_ps_sparse.o \
		dmsasep.o dmsabc.o dmsafg.o dmsamain.o $(TIMER) \
		$(AD_LIB)/lib/ReqADIntrinsics-$(AD_COMP).o \
		$(AD_LIB)/lib/libADIntrinsics-$(AD_COMP).a \
		$(AD_LIB)/lib/libSparsLinC-$(AD_COMP).a

s_%.o: s_%.f
	f77 -c $(FFLAGS) s_$*.f

s_%.f: ../source/%.f
	cd ..;Adifor2.0 AD_SCRIPT=msa_ps_sparse.adf

%.o: ../source/%.f
	f77 -o $@ -c $(FFLAGS) ../source/$*.f

$(TIMER):
	cd ../timer; make timer

clean:
	rm -f *.o s_*.f *.aux *.A *~
	cd ../timer; make clean

reallyclean: clean
	rm -f *.out msa_ps_sparse
