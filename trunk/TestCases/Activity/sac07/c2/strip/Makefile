AD_COMP = sun4-SC1.0

strip: admain.o g_fcn.o g_fcn2.o
	f77 -o strip admain.o g_fcn.o g_fcn2.o $(AD_LIB)/lib/ReqADIntrinsics-$(AD_COMP).o $(AD_LIB)/lib/libADIntrinsics-$(AD_COMP).a

process: ../c2strip.adf ../fcn.f ../fcn2.f
	cd ..; Adifor2.0 AD_SCRIPT=c2strip.adf

clean:
	rm -f *.A *.aux *.o *~
