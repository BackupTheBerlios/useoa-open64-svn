AD_COMP = $(AD_OS)
TIMER = ../timer/libTimer-$(AD_OS).a

msa_nps_sparse: s_dmsafg.o s_dmsamain.o driver_nps_sparse.o dmsabc.o dmsafg.o dmsamain.o $(TIMER)
	$(FC) -o msa_nps_sparse s_dmsafg.o s_dmsamain.o driver_nps_sparse.o \
		dmsabc.o dmsafg.o dmsamain.o $(TIMER) \
		./libSparsLinC-$(AD_COMP).a
#                $(AD_LIB)/lib/libSparsLinC-$(AD_COMP).a
#                $(AD_LIB)/lib/ReqADIntrinsics-$(AD_COMP).o \
#                $(AD_LIB)/lib/libADIntrinsics-$(AD_COMP).a \


s_%.o: s_%.f
	$(FC) -c $(FFLAGS) s_$*.f

s_%.f: ../source/%.f
	cd ..;Adifor2.0 AD_SCRIPT=msa_nps_sparse.adf

%.o: ../source/%.f
	$(FC) -o $@ -c $(FFLAGS) ../source/$*.f

$(TIMER):
	cd ../timer; make timer

clean:
	rm -f *.o s_*.f *.aux *.A *~
	cd ../timer; make clean

reallyclean: clean
	rm -f *.out msa_nps_sparse
