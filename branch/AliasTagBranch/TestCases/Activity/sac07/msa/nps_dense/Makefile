FFLAGS = -O3 -axKW
# FC = pgf77
FC = ifc

AD_COMP = $(AD_OS)
TIMER = ../timer/libTimer-$(AD_OS).a

msa_nps_dense: g_dmsafg.o g_dmsamain.o driver_nps_dense.o dmsabc.o dmsafg.o dmsamain.o $(TIMER)
	$(FC) -o msa_nps_dense g_dmsafg.o g_dmsamain.o driver_nps_dense.o \
		dmsabc.o dmsafg.o dmsamain.o $(TIMER)

g_%.o: g_%.f
	$(FC) -c $(FFLAGS) g_$*.f

g_%.f: ../source/%.f
	cd ..;Adifor2.0 AD_SCRIPT=msa_nps_dense.adf

%.o: ../source/%.f
	$(FC) -o $@ -c $(FFLAGS) ../source/$*.f

$(TIMER):
	cd ../timer; make timer

clean:
	rm -f *.o g_*.f *.aux *.A *~ *.trace
	cd ../timer; make clean

reallyclean: clean
	rm -f *.out msa_nps_dense
