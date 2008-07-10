# AD_COMP = sun4-SC1.0
AD_COMP = $(AD_OS)
TIMER = ../timer/libTimer-$(AD_OS).a

msa_ps_comp: g_dmsasep.o driver_ps_comp.o dmsasep.o dmsabc.o dmsafg.o dmsamain.o dmsasparsity.o dsm.o fd.o $(TIMER) 
	$(FC) -o msa_ps_comp g_dmsasep.o driver_ps_comp.o dmsasparsity.o dsm.o fd.o \
		dmsasep.o dmsabc.o dmsafg.o dmsamain.o $(TIMER) \
		$(AD_LIB)/lib/ReqADIntrinsics-$(AD_COMP).o \
		$(AD_LIB)/lib/libADIntrinsics-$(AD_COMP).a

g_%.o: g_%.f
	$(FC) -c $(FFLAGS) g_$*.f

g_%.f: ../source/%.f
	cd ..;Adifor2.0 AD_SCRIPT=msa_ps_comp.adf

%.o: ../source/%.f
	$(FC) -o $@ -c $(FFLAGS) ../source/$*.f

$(TIMER):
	cd ../timer; make timer

clean:
	rm -f *.o g_*.f *.aux *.A *~
	cd ../timer; make clean

reallyclean: clean
	rm -f *.out msa_ps_comp
