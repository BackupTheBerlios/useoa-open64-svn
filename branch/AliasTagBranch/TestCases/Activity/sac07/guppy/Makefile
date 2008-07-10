AD_COMP = sun4-SC1.0

g_newbl: driver.o g_newbl.o
	f77 -o g_newbl g_newbl.o driver.o $(AD_LIB)/lib/ReqADIntrinsics-$(AD_COMP).o $(AD_LIB)/lib/libADIntrinsics-$(AD_COMP).a

process: newbl.f newbl.adf newbl.cmp
	Adifor2.0 AD_SCRIPT=newbl.adf
clean:
	rm -f *.o *.aux *.A *~
	rm -rf AD_cache
